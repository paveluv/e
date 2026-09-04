;; tty.e -- terminal input decoding: the library (tty), v2 core
;; dissolution (docs/DESIGN2.md).  Pure infrastructure with no init!;
;; raw-mode control (termios) lives in the platform layer, (sys).
;;
;; One entry point: (tty:read-event port) decodes the next keyboard,
;; mouse, or host event from a terminal input port into plain data --
;;
;;   a key        "a", "C-x", "M-f", "UP", "C-M-S-F5", "KP-ENTER", ...
;;   end of input the eof object
;;   mouse        (mouse c b x y)      SGR: press/release/drag/wheel
;;   paste        (paste . text)       bracketed, closer stripped
;;   host report  (host-color-scheme dark|light)   DSR 997
;;
;; The decoder is a pure function of the byte stream: side effects
;; happen at consumption, on whatever thread pumps the events (the
;; core's reader thread posts them to the main mailbox).  Unknown
;; escape sequences are swallowed whole so their payloads can never
;; leak into a buffer as typed text.

(library (tty)
  (export read-event character-event key-event-character
          mouse-reporting! paste-lines)
  (import (rnrs)
          (only (chezscheme) format char-ready?)
          (only (sys) terminal-output-port)
          (prefix (strings) strings:))

  ;;; Input-side negotiation ------------------------------------------------------

  (define (mouse-reporting! on?)
    ;; SGR mouse tracking with button-event reports (1002;1006): on
    ;; asks the terminal to send the (mouse ...) events read-event
    ;; decodes; off restores the terminal's native selection.
    (let ([port (terminal-output-port)])
      (display (if on? "\x1b;[?1002;1006h" "\x1b;[?1002;1006l") port)
      (flush-output-port port)))

  ;;; Key naming ---------------------------------------------------------------

  (define (character-event c)
    (let ([n (char->integer c)])
      (cond [(= n 0) "C-@"]
            [(= n 9) "TAB"]
            [(or (= n 10) (= n 13)) "RET"]
            [(= n 27) "ESC"]
            [(= n 28) "C-\\"]
            [(= n 29) "C-]"]
            [(= n 30) "C-^"]
            [(= n 31) "C-_"]
            [(and (> n 0) (< n 27))
             (format "C-~c" (integer->char (+ n 96)))]
            [(= n 127) "BACKSPACE"]
            [else (string c)])))

  (define (key-event-character event)
    ;; the plain character a key event types, or #f
    (and (string? event) (= (string-length event) 1)
         (let ([c (string-ref event 0)])
           (and (>= (char->integer c) 32) c))))

  (define (csi-numbers text)
    (let loop ([characters (string->list text)] [digits '()] [out '()])
      (cond [(null? characters)
             (reverse
               (if (null? digits) out
                   (cons (string->number (list->string (reverse digits)))
                         out)))]
            [(char=? (car characters) #\;)
             (loop (cdr characters) '()
                   (cons (and (pair? digits)
                              (string->number
                                (list->string (reverse digits))))
                         out))]
            [else (loop (cdr characters) (cons (car characters) digits)
                        out)])))

  (define (xterm-modified-name name modifier)
    (string-append
      (case modifier [(2) "S-"] [(3) "M-"] [(4) "M-S-"]
        [(5) "C-"] [(6) "C-S-"] [(7) "C-M-"] [(8) "C-M-S-"]
        [else ""])
      name))

  (define (xterm-function-name base modifier)
    (let ([offset (case modifier [(2) 12] [(5) 24] [(6) 36]
                    [(3) 48] [(4) 60] [else 0])])
      (if (and (= modifier 4) (> base 3))
          (xterm-modified-name (format "F~a" base) modifier)
          (format "F~a" (+ base offset)))))

  (define (xterm-function-base code)
    (case code [(15) 5] [(17) 6] [(18) 7] [(19) 8]
      [(20) 9] [(21) 10] [(23) 11] [(24) 12] [else #f]))

  ;;; Sequence decoding ----------------------------------------------------------

  (define (mouse-event port)
    ;; The rest of an ESC [ < sequence: b ; x ; y then M (press) or
    ;; m (release), as data.
    (let drain ([c (read-char port)] [ps '()])
      (if (and (char? c) (or (char<=? #\0 c #\9) (char=? c #\;)))
          (drain (read-char port) (cons c ps))
          (let ([nums (let split ([chars (reverse ps)] [cur 0] [acc '()])
                        (cond [(null? chars) (reverse (cons cur acc))]
                              [(char=? (car chars) #\;)
                               (split (cdr chars) 0 (cons cur acc))]
                              [else
                               (split (cdr chars)
                                      (+ (* cur 10)
                                         (- (char->integer (car chars))
                                            48))
                                      acc)]))])
            (and (char? c) (= (length nums) 3)
                 (list 'mouse c (car nums) (cadr nums) (caddr nums)))))))

  (define (paste-body port)
    ;; Everything up to ESC [ 2 0 1 ~.  Hold only a prefix of the
    ;; closer while matching it one character at a time.  On a
    ;; mismatch, emit that prefix as payload and reconsider a
    ;; mismatching ESC as the start of the real closer.
    (define closer "\x1b;[201~")
    (define (emit-prefix acc matched)
      (let loop ([i 0] [acc acc])
        (if (= i matched)
            acc
            (loop (+ i 1) (cons (string-ref closer i) acc)))))
    (let loop ([acc '()] [matched 0])
      (let ([c (read-char port)])
        (cond
          [(eof-object? c)
           (list->string (reverse (emit-prefix acc matched)))]
          [(char=? c (string-ref closer matched))
           (let ([matched (+ matched 1)])
             (if (= matched (string-length closer))
                 (list->string (reverse acc))
                 (loop acc matched)))]
          [else
           (let ([acc (emit-prefix acc matched)])
             (if (char=? c #\esc)
                 (loop acc 1)
                 (loop (cons c acc) 0)))]))))

  (define (csi-event port)
    (let ([first (read-char port)])
      (cond
        [(and (char? first) (char=? first #\<))
         (or (mouse-event port) "MOUSE-HANDLED")]
        [(and (char? first) (char=? first #\?))
         ;; A private report from the host, not a key. The color-scheme
         ;; report (DSR 997) is acted on; any other is swallowed so its
         ;; payload cannot leak into the buffer as typed text.
         (let drain ([b (read-char port)] [params '()])
           (if (and (char? b)
                    (or (char<=? #\0 b #\9) (char=? b #\;)))
               (drain (read-char port) (cons b params))
               (let ([numbers (csi-numbers
                                (list->string (reverse params)))])
                 (if (and (char? b) (char=? b #\n)
                          (pair? numbers) (eqv? (car numbers) 997))
                     (list 'host-color-scheme
                           (if (eqv? (and (pair? (cdr numbers))
                                          (cadr numbers))
                                     2)
                               'light 'dark))
                     #f))))]
        [else
         (let drain ([b first] [params '()])
           (if (and (char? b)
                    (or (char<=? #\0 b #\9) (char=? b #\;)))
               (drain (read-char port) (cons b params))
               (let ([p (list->string (reverse params))])
                 (define numbers (csi-numbers p))
                 (define modifier
                   ;; the second parameter; for letter finals a lone
                   ;; 2/3/4 is a legacy modifier spelling, but a ~
                   ;; final's first parameter is the keycode itself
                   ;; (ESC [ 3 ~ is Delete, never M-)
                   (cond [(and (pair? numbers) (pair? (cdr numbers)))
                          (or (cadr numbers) 1)]
                         [(and (not (char=? b #\~))
                               (pair? numbers)
                               (memv (car numbers) '(2 3 4)))
                          (car numbers)]
                         [else 1]))
                 (define (named name) (xterm-modified-name name modifier))
                 (case b
                   [(#\A) (named "UP")] [(#\B) (named "DOWN")]
                   [(#\C) (named "RIGHT")] [(#\D) (named "LEFT")]
                   [(#\H) (named "HOME")] [(#\F) (named "END")]
                   [(#\P #\Q #\R #\S)
                    (xterm-function-name
                      (+ 1 (- (char->integer b) (char->integer #\P)))
                      modifier)]
                   [(#\Z) "S-TAB"]
                   [(#\~)
                    (let ([code (and (pair? numbers) (car numbers))])
                      (cond [(eqv? code 200)
                             (cons 'paste (paste-body port))]
                            [(memv code '(1 7)) (named "HOME")]
                            [(memv code '(4 8)) (named "END")]
                            [(eqv? code 2) (named "INSERT")]
                            [(eqv? code 3) (named "DELETE")]
                            [(eqv? code 5) (named "PAGEUP")]
                            [(eqv? code 6) (named "PAGEDOWN")]
                            [(xterm-function-base code)
                             => (lambda (base)
                                  (xterm-function-name base modifier))]
                            [else #f]))]
                   [else #f]))))])))

  (define (read-event port)
    ;; Decode the terminal once, into data.  Blocks until a whole
    ;; event is available; a lone ESC is the ESC key only when no more
    ;; input is pending.
    (let again ()
      (let ([c (read-char port)])
        (cond
          [(eof-object? c) c]
          [(not (char=? c #\esc)) (character-event c)]
          [(not (char-ready? port)) "ESC"]
          [else
           (let ([a (read-char port)])
             (cond
               [(eof-object? a) "ESC"]
               [(char=? a #\[)
                (or (csi-event port) (again))]
               [(char=? a #\O)
                (case (read-char port)
                  [(#\P) "F1"] [(#\Q) "F2"]
                  [(#\R) "F3"] [(#\S) "F4"]
                  [(#\A) "UP"] [(#\B) "DOWN"]
                  [(#\C) "RIGHT"] [(#\D) "LEFT"]
                  [(#\H) "HOME"] [(#\F) "END"]
                  [(#\E) "BEGIN"]
                  [(#\p) "KP-0"] [(#\q) "KP-1"]
                  [(#\r) "KP-2"] [(#\s) "KP-3"]
                  [(#\t) "KP-4"] [(#\u) "KP-5"]
                  [(#\v) "KP-6"] [(#\w) "KP-7"]
                  [(#\x) "KP-8"] [(#\y) "KP-9"]
                  [(#\n) "KP-DECIMAL"] [(#\o) "KP-DIVIDE"]
                  [(#\j) "KP-MULTIPLY"] [(#\m) "KP-SUBTRACT"]
                  [(#\k) "KP-ADD"] [(#\l) "KP-COMMA"]
                  [(#\X) "KP-EQUAL"] [(#\M) "KP-ENTER"]
                  [else (again)])]
               [else
                (let ([plain (character-event a)])
                  (if (strings:prefix? "C-" plain)
                      (string-append "C-M-" (strings:tail plain 2))
                      (string-append "M-"
                                     (if (string=? plain " ")
                                         "SPC"
                                         plain))))]))]))))
  ;;; Pasted text -------------------------------------------------------------------

  (define (paste-lines s)
    ;; Pasted text split at newlines, whichever convention the terminal
    ;; delivered: \n, \r\n, or bare \r.
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond [(= i n) (reverse (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\return)
               (let ([next (if (and (< (+ i 1) n)
                                    (char=? (string-ref s (+ i 1)) #\newline))
                               (+ i 2) (+ i 1))])
                 (loop next next (cons (substring s start i) acc)))]
              [else (loop (+ i 1) start acc)]))))
)
