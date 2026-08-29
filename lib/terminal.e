;; terminal.e -- PTY-backed terminal emulator app.

(library (terminal)
  (export init! terminal!! terminal-send! terminal-close! terminal-scrollback)
  (import (chezscheme) (core) (sys)
          (only (describe) register-descriptions!))

  (define-record-type terminal-state
    (fields buffer process display lock
            (mutable rows) (mutable cols) (mutable screen)
            (mutable row) (mutable col)
            (mutable saved-row) (mutable saved-col)
            (mutable scroll-top) (mutable scroll-bottom)
            (mutable parser) (mutable parameters)
            (mutable osc-escape) (mutable charset) (mutable shift)
            (mutable dirty) (mutable alive) (mutable prefix)
            (mutable mouse) (mutable bracketed) (mutable main-screen)
            (mutable history)
            (mutable styles) (mutable main-styles) (mutable history-styles)
            (mutable sgr) (mutable style)))

  (define terminals '())
  (define serial 0)
  (define style-serial 0)
  (define style-cache (make-hashtable string-hash string=?))
  (define style-lock (make-mutex))
  (define terminal-scrollback
    (make-parameter 10000
      (lambda (lines)
        (unless (and (integer? lines) (exact? lines) (>= lines 0))
          (error 'terminal-scrollback "must be a nonnegative integer" lines))
        lines)))

  (define (terminal-of buffer)
    (find (lambda (state) (eq? (terminal-state-buffer state) buffer))
          terminals))

  (define (blank-line cols) (make-string cols #\space))
  (define (blank-styles cols style) (make-vector cols style))

  (define (copy-vector-range! source source-start target target-start count)
    (if (<= target-start source-start)
        (do ([index 0 (+ index 1)]) ((= index count))
          (vector-set! target (+ target-start index)
                       (vector-ref source (+ source-start index))))
        (do ([index (- count 1) (- index 1)]) ((< index 0))
          (vector-set! target (+ target-start index)
                       (vector-ref source (+ source-start index))))))

  (define (make-screen rows cols)
    (let ([screen (make-vector rows)])
      (do ([row 0 (+ row 1)]) ((= row rows) screen)
        (vector-set! screen row (blank-line cols)))))

  (define (make-style-screen rows cols style)
    (let ([screen (make-vector rows)])
      (do ([row 0 (+ row 1)]) ((= row rows) screen)
        (vector-set! screen row (blank-styles cols style)))))

  (define (resized-screen old old-rows old-cols rows cols)
    (let ([new (make-screen rows cols)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (string-copy! (vector-ref old row) 0
                      (vector-ref new row) 0 copy-cols))))

  (define (resized-styles old old-rows old-cols rows cols style)
    (let ([new (make-style-screen rows cols style)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (copy-vector-range! (vector-ref old row) 0
                            (vector-ref new row) 0 copy-cols))))

  (define (clamp value low high) (min high (max low value)))

  (define (resize-screen! state rows cols)
    (unless (and (= rows (terminal-state-rows state))
                 (= cols (terminal-state-cols state)))
      (let* ([old-rows (terminal-state-rows state)]
             [old-cols (terminal-state-cols state)]
             [new (resized-screen (terminal-state-screen state)
                                  old-rows old-cols rows cols)]
             [new-styles (resized-styles (terminal-state-styles state)
                                         old-rows old-cols rows cols
                                         (terminal-state-style state))])
        (when (terminal-state-main-screen state)
          (terminal-state-main-screen-set!
            state (resized-screen (terminal-state-main-screen state)
                                  old-rows old-cols rows cols)))
        (when (terminal-state-main-styles state)
          (terminal-state-main-styles-set!
            state (resized-styles (terminal-state-main-styles state)
                                  old-rows old-cols rows cols
                                  (terminal-state-style state))))
        (terminal-state-screen-set! state new)
        (terminal-state-styles-set! state new-styles)
        (terminal-state-rows-set! state rows)
        (terminal-state-cols-set! state cols)
        (terminal-state-row-set! state
                                 (clamp (terminal-state-row state) 0 (- rows 1)))
        (terminal-state-col-set! state
                                 (clamp (terminal-state-col state) 0 (- cols 1)))
        (terminal-state-scroll-top-set! state 0)
        (terminal-state-scroll-bottom-set! state (- rows 1))
        (terminal-state-dirty-set! state #t)
        (resize-terminal-process! (terminal-state-process state) rows cols))))

  (define (scroll-up! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (when (and (= top 0) (= bottom (- (terminal-state-rows state) 1))
                   (not (terminal-state-main-screen state))
                   (> (terminal-scrollback) 0))
          (let ([history (append (terminal-state-history state)
                                 (list (string-copy (vector-ref screen top))))])
            (terminal-state-history-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history))))
          (let ([history (append (terminal-state-history-styles state)
                                 (list (vector-copy (vector-ref styles top))))])
            (terminal-state-history-styles-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history)))))
        (do ([row top (+ row 1)]) ((= row bottom))
          (vector-set! screen row (vector-ref screen (+ row 1)))
          (vector-set! styles row (vector-ref styles (+ row 1))))
        (vector-set! screen bottom (blank-line cols))
        (vector-set! styles bottom
                     (blank-styles cols (terminal-state-style state))))))

  (define (scroll-down! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (do ([row bottom (- row 1)]) ((= row top))
          (vector-set! screen row (vector-ref screen (- row 1)))
          (vector-set! styles row (vector-ref styles (- row 1))))
        (vector-set! screen top (blank-line cols))
        (vector-set! styles top
                     (blank-styles cols (terminal-state-style state))))))

  (define (line-feed! state)
    (if (= (terminal-state-row state) (terminal-state-scroll-bottom state))
        (scroll-up! state 1)
        (terminal-state-row-set!
          state (min (- (terminal-state-rows state) 1)
                     (+ (terminal-state-row state) 1)))))

  (define (put-character! state character)
    (when (>= (terminal-state-col state) (terminal-state-cols state))
      (terminal-state-col-set! state 0)
      (line-feed! state))
    (string-set! (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))
                 (terminal-state-col state) character)
    (vector-set! (vector-ref (terminal-state-styles state)
                             (terminal-state-row state))
                 (terminal-state-col state) (terminal-state-style state))
    (terminal-state-col-set! state (+ (terminal-state-col state) 1)))

  (define (erase-line! state start end)
    (let ([line (vector-ref (terminal-state-screen state)
                            (terminal-state-row state))]
          [styles (vector-ref (terminal-state-styles state)
                              (terminal-state-row state))])
      (do ([col (max 0 start) (+ col 1)])
          ((>= col (min end (terminal-state-cols state))))
        (string-set! line col #\space)
        (vector-set! styles col (terminal-state-style state)))))

  (define (clear-rows! state start end)
    (do ([row (max 0 start) (+ row 1)])
        ((>= row (min end (terminal-state-rows state))))
      (vector-set! (terminal-state-screen state) row
                   (blank-line (terminal-state-cols state)))
      (vector-set! (terminal-state-styles state) row
                   (blank-styles (terminal-state-cols state)
                                 (terminal-state-style state)))))

  (define (parameter-list text)
    (let* ([plain (if (and (> (string-length text) 0)
                           (memv (string-ref text 0) '(#\? #\> #\!)))
                      (substring text 1 (string-length text)) text)]
           [parts (split-lines
                    (list->string
                      (map (lambda (c) (if (char=? c #\;) #\newline c))
                           (string->list plain))))])
      (map (lambda (part) (or (string->number part) 0)) parts)))

  (define (param parameters index default)
    (let ([value (and (< index (length parameters))
                      (list-ref parameters index))])
      (if (or (not value) (= value 0)) default value)))

  (define (delete-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [cols (terminal-state-cols state)]
           [count (min count (- cols col))])
      (string-copy! line (+ col count) line col (- cols col count))
      (copy-vector-range! styles (+ col count) styles col (- cols col count))
      (do ([i (- cols count) (+ i 1)]) ((= i cols))
        (string-set! line i #\space)
        (vector-set! styles i (terminal-state-style state)))))

  (define (insert-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [cols (terminal-state-cols state)]
           [count (min count (- cols col))])
      (do ([i (- cols 1) (- i 1)]) ((< i (+ col count)))
        (string-set! line i (string-ref line (- i count)))
        (vector-set! styles i (vector-ref styles (- i count))))
      (do ([i col (+ i 1)]) ((= i (+ col count)))
        (string-set! line i #\space)
        (vector-set! styles i (terminal-state-style state)))))

  (define (sgr-style sequence)
    (if (string=? sequence "")
        'plain
        (with-mutex style-lock
          (or (hashtable-ref style-cache sequence #f)
              (let ([name (string->symbol
                            (format "terminal-sgr-~a" style-serial))])
                (set! style-serial (+ style-serial 1))
                ;; set-style! accepts SGR parameters, not a complete escape
                ;; sequence. Passing CSI here produced CSI CSI ... m m; the
                ;; second trailing `m` was painted as text by the host
                ;; terminal, most visibly during top's frequent SGR changes.
                (set-style! name (format "0;~a" sequence))
                (hashtable-set! style-cache sequence name)
                name)))))

  (define (set-sgr! state text)
    ;; Keeping the sequence since the last reset preserves cumulative SGR
    ;; semantics (31m followed by 1m means bold red). Later codes naturally
    ;; override earlier ones when the renderer emits the combined sequence.
    (let* ([codes (parameter-list text)]
           [last-reset
            (let loop ([xs codes] [index 0] [found #f])
              (if (null? xs) found
                  (loop (cdr xs) (+ index 1)
                        (if (= (car xs) 0) index found))))]
           [kept (if last-reset (list-tail codes (+ last-reset 1)) codes)]
           [addition (string-join (map number->string kept) ";")]
           [sequence
            (cond [last-reset addition]
                  [(string=? addition "") (terminal-state-sgr state)]
                  [(string=? (terminal-state-sgr state) "") addition]
                  [else (string-append (terminal-state-sgr state)
                                       ";" addition)])])
      (terminal-state-sgr-set! state sequence)
      (terminal-state-style-set! state (sgr-style sequence))))

  (define (dispatch-csi! state final text)
    (let* ([parameters (parameter-list text)]
           [n (param parameters 0 1)]
           [row (terminal-state-row state)]
           [col (terminal-state-col state)]
           [rows (terminal-state-rows state)]
           [cols (terminal-state-cols state)])
      (if (and (> (string-length text) 0)
               (char=? (string-ref text 0) #\?)
               (memv final '(#\h #\l)))
          (let ([on? (char=? final #\h)])
            (for-each
              (lambda (mode)
                (case mode
                  [(1000 1002 1003 1006) (terminal-state-mouse-set! state on?)]
                  [(2004) (terminal-state-bracketed-set! state on?)]
                  [(1049)
                   (if on?
                       (unless (terminal-state-main-screen state)
                         (terminal-state-main-screen-set!
                           state (terminal-state-screen state))
                         (terminal-state-main-styles-set!
                           state (terminal-state-styles state))
                         (terminal-state-screen-set!
                           state (make-screen rows cols))
                         (terminal-state-styles-set!
                           state (make-style-screen rows cols
                                                    (terminal-state-style state)))
                         (terminal-state-row-set! state 0)
                         (terminal-state-col-set! state 0))
                       (when (terminal-state-main-screen state)
                         (terminal-state-screen-set!
                           state (terminal-state-main-screen state))
                         (terminal-state-styles-set!
                           state (terminal-state-main-styles state))
                         (terminal-state-main-screen-set! state #f)
                         (terminal-state-main-styles-set! state #f)
                         (terminal-state-row-set! state 0)
                         (terminal-state-col-set! state 0)))]
                  [else (void)]))
              parameters))
          (case final
            [(#\A) (terminal-state-row-set! state (max 0 (- row n)))]
            [(#\B #\e) (terminal-state-row-set! state (min (- rows 1) (+ row n)))]
            [(#\C #\a) (terminal-state-col-set! state (min (- cols 1) (+ col n)))]
            [(#\D) (terminal-state-col-set! state (max 0 (- col n)))]
            [(#\E) (terminal-state-row-set! state (min (- rows 1) (+ row n)))
             (terminal-state-col-set! state 0)]
            [(#\F) (terminal-state-row-set! state (max 0 (- row n)))
             (terminal-state-col-set! state 0)]
            [(#\G #\`) (terminal-state-col-set! state
                                                (clamp (- n 1) 0 (- cols 1)))]
            [(#\d) (terminal-state-row-set! state
                                            (clamp (- n 1) 0 (- rows 1)))]
            [(#\H #\f)
             (terminal-state-row-set! state
                                      (clamp (- (param parameters 0 1) 1)
                                        0 (- rows 1)))
             (terminal-state-col-set! state
                                      (clamp (- (param parameters 1 1) 1)
                                        0 (- cols 1)))]
            [(#\J)
             (case (param parameters 0 0)
               [(0) (erase-line! state col cols)
                (clear-rows! state (+ row 1) rows)]
               [(1) (clear-rows! state 0 row)
                (erase-line! state 0 (+ col 1))]
               [(2 3) (clear-rows! state 0 rows)])]
            [(#\K)
             (case (param parameters 0 0)
               [(0) (erase-line! state col cols)]
               [(1) (erase-line! state 0 (+ col 1))]
               [(2) (erase-line! state 0 cols)])]
            [(#\S) (scroll-up! state n)]
            [(#\T) (scroll-down! state n)]
            [(#\P) (delete-characters! state n)]
            [(#\@) (insert-characters! state n)]
            [(#\X) (erase-line! state col (+ col n))]
            [(#\L) (let ([old (terminal-state-scroll-top state)])
                     (terminal-state-scroll-top-set! state row)
                     (scroll-down! state n)
                     (terminal-state-scroll-top-set! state old))]
            [(#\M) (let ([old (terminal-state-scroll-top state)])
                     (terminal-state-scroll-top-set! state row)
                     (scroll-up! state n)
                     (terminal-state-scroll-top-set! state old))]
            [(#\r)
             (terminal-state-scroll-top-set!
               state (clamp (- (param parameters 0 1) 1) 0 (- rows 1)))
             (terminal-state-scroll-bottom-set!
               state (clamp (- (param parameters 1 rows) 1) 0 (- rows 1)))
             (terminal-state-row-set! state 0)
             (terminal-state-col-set! state 0)]
            [(#\s) (terminal-state-saved-row-set! state row)
             (terminal-state-saved-col-set! state col)]
            [(#\u) (terminal-state-row-set! state (terminal-state-saved-row state))
             (terminal-state-col-set! state (terminal-state-saved-col state))]
            [(#\m) (set-sgr! state text)]
            [else (void)]))))

  (define line-drawing
    '((#\j . #\x2518) (#\k . #\x2510) (#\l . #\x250c) (#\m . #\x2514)
      (#\n . #\x253c) (#\q . #\x2500) (#\t . #\x251c) (#\u . #\x2524)
      (#\v . #\x2534) (#\w . #\x252c) (#\x . #\x2502)))

  (define (mapped-character state character)
    (if (eq? (terminal-state-charset state) 'line)
        (cond [(assv character line-drawing) => cdr] [else character])
        character))

  (define (feed-character! state character)
    (case (terminal-state-parser state)
      [(normal)
       (case (char->integer character)
         [(7) (void)]
         [(8) (terminal-state-col-set! state
                                       (max 0 (- (terminal-state-col state) 1)))]
         [(9) (terminal-state-col-set!
                state (min (- (terminal-state-cols state) 1)
                           (* 8 (+ 1 (quotient (terminal-state-col state) 8)))))]
         [(10 11 12) (line-feed! state)]
         [(13) (terminal-state-col-set! state 0)]
         [(14) (terminal-state-shift-set! state 1)]
         [(15) (terminal-state-shift-set! state 0)]
         [(27) (terminal-state-parser-set! state 'escape)]
         [else (when (>= (char->integer character) 32)
                 (put-character! state (mapped-character state character)))])]
      [(escape)
       (case character
         [(#\[) (terminal-state-parser-set! state 'csi)
          (terminal-state-parameters-set! state "")]
         [(#\]) (terminal-state-parser-set! state 'osc)
          (terminal-state-osc-escape-set! state #f)]
         [(#\7) (terminal-state-saved-row-set! state (terminal-state-row state))
          (terminal-state-saved-col-set! state (terminal-state-col state))
          (terminal-state-parser-set! state 'normal)]
         [(#\8) (terminal-state-row-set! state (terminal-state-saved-row state))
          (terminal-state-col-set! state (terminal-state-saved-col state))
          (terminal-state-parser-set! state 'normal)]
         [(#\D) (line-feed! state) (terminal-state-parser-set! state 'normal)]
         [(#\M) (if (= (terminal-state-row state)
                       (terminal-state-scroll-top state))
                    (scroll-down! state 1)
                    (terminal-state-row-set!
                      state (max 0 (- (terminal-state-row state) 1))))
          (terminal-state-parser-set! state 'normal)]
         [(#\c) (clear-rows! state 0 (terminal-state-rows state))
          (terminal-state-row-set! state 0)
          (terminal-state-col-set! state 0)
          (terminal-state-parser-set! state 'normal)]
         [(#\( #\)) (terminal-state-parser-set! state 'charset)]
         [else (terminal-state-parser-set! state 'normal)])]
      [(charset)
       (terminal-state-charset-set! state (if (char=? character #\0)
                                              'line 'ascii))
       (terminal-state-parser-set! state 'normal)]
      [(csi)
       (if (char<=? #\@ character #\~)
           (begin
             (dispatch-csi! state character (terminal-state-parameters state))
             (terminal-state-parser-set! state 'normal))
           (terminal-state-parameters-set!
             state (string-append (terminal-state-parameters state)
                                  (string character))))]
      [(osc)
       (cond [(= (char->integer character) 7)
              (terminal-state-parser-set! state 'normal)]
             [(and (terminal-state-osc-escape state) (char=? character #\\))
              (terminal-state-parser-set! state 'normal)
              (terminal-state-osc-escape-set! state #f)]
             [else
              (terminal-state-osc-escape-set! state (char=? character #\esc))])]))

  (define (refresh-terminal! state)
    (let ([size (buffer-window-size (terminal-state-buffer state))])
      (when size
        (with-mutex (terminal-state-lock state)
          (resize-screen! state (max 1 (car size)) (max 1 (cdr size)))
          (when (terminal-state-dirty state)
            (view-replace!
              (terminal-state-buffer state)
              ;; The emulator mutates its cell grid in place.  The generic
              ;; view and paint caches need immutable snapshots; sharing these
              ;; strings made character echo invisible until a later scroll
              ;; happened to replace an entire row.
              (map string-copy
                   (append (if (terminal-state-main-screen state)
                               '() (terminal-state-history state))
                           (vector->list (terminal-state-screen state)))))
            (terminal-state-dirty-set! state #f))
          (when (eq? (current-buffer) (terminal-state-buffer state))
            (goto-point!
              (cons (+ (if (terminal-state-main-screen state)
                           0 (length (terminal-state-history state)))
                       (terminal-state-row state))
                    (min (- (terminal-state-cols state) 1)
                         (terminal-state-col state)))))))))

  (define (terminal-row-styles buffer row line)
    (let ([state (terminal-of buffer)])
      (and state
           (if (terminal-state-main-screen state)
               (and (< row (terminal-state-rows state))
                    (vector-copy (vector-ref (terminal-state-styles state)
                                             row)))
               (let ([history (terminal-state-history-styles state)])
                 (if (< row (length history))
                     (vector-copy (list-ref history row))
                     (let ([screen-row (- row (length history))])
                       (and (< screen-row (terminal-state-rows state))
                            (vector-copy
                              (vector-ref (terminal-state-styles state)
                                          screen-row))))))))))

  (define (reader-loop state)
    (define (display-redraw!)
      (parameterize ([terminal-output-port (terminal-state-display state)])
        (redraw!)))
    (define (finished!)
      (terminal-state-alive-set! state #f)
      (guard (ex [else (void)])
        (set-app-capture! (terminal-state-buffer state) #f))
      ;; Publish the dead state before waiting for the session leader.  A
      ;; platform-specific wait must never make the editor appear frozen.
      (let ([notice
             "Terminal process exited; editor keys are active (C-] q closes it)"])
        (log! 'terminal notice)
        (parameterize ([message-source #f]) (set-message! notice)))
      (guard (ex [else (void)]) (display-redraw!))
      (guard (ex [else (void)])
        (reap-terminal-process! (terminal-state-process state)))
      (guard (ex [else (void)])
        (close-port (terminal-state-display state))))
    (guard (ex [else (finished!)])
      (let ([input (transcoded-port
                     (terminal-process-input (terminal-state-process state))
                     (make-transcoder (utf-8-codec) 'none 'replace))])
        (let loop ()
          (let ([character (get-char input)])
            (if (eof-object? character)
                (finished!)
                (begin
                  (with-mutex (terminal-state-lock state)
                    (feed-character! state character)
                    (let drain ([remaining 4095])
                      (when (and (> remaining 0) (char-ready? input))
                        (let ([next (get-char input)])
                          (unless (eof-object? next)
                            (feed-character! state next)
                            (drain (- remaining 1))))))
                    (terminal-state-dirty-set! state #t))
                  ;; Output must become visible while the main thread is
                  ;; blocked reading the editor's keyboard.
                  (display-redraw!)
                  (loop))))))))

  (define (write-bytes! state bytes)
    (when (terminal-state-alive state)
      (let ([output (terminal-process-output (terminal-state-process state))])
        (put-bytevector output bytes)
        (flush-output-port output))))

  (define (terminal-send! text)
    (let ([state (terminal-of (current-buffer))])
      (unless state (error 'terminal-send! "current buffer is not a terminal"))
      (write-bytes! state (string->utf8 text))))

  (define (control-byte letter)
    (bytevector (- (char->integer (char-upcase letter)) 64)))

  (define (bytes-append left right)
    (let ([result (make-bytevector (+ (bytevector-length left)
                                      (bytevector-length right)))])
      (bytevector-copy! left 0 result 0 (bytevector-length left))
      (bytevector-copy! right 0 result (bytevector-length left)
                        (bytevector-length right))
      result))

  (define (event-bytes event)
    (cond
      [(= (string-length event) 1) (string->utf8 event)]
      [(string-prefix? "C-M-" event)
       (bytes-append (bytevector 27)
                     (control-byte (string-ref event 4)))]
      [(string-prefix? "M-" event)
       (bytes-append (bytevector 27)
                     (string->utf8 (substring event 2
                                              (string-length event))))]
      [(and (string-prefix? "C-" event) (= (string-length event) 3))
       (control-byte (string-ref event 2))]
      [else
       (cond [(assoc event
                     '(("RET" . "\r") ("TAB" . "\t")
                       ("BACKSPACE" . "\x7f;") ("ESC" . "\x1b;")
                       ("UP" . "\x1b;[A") ("DOWN" . "\x1b;[B")
                       ("RIGHT" . "\x1b;[C") ("LEFT" . "\x1b;[D")
                       ("HOME" . "\x1b;[H") ("END" . "\x1b;[F")
                       ("DELETE" . "\x1b;[3~")
                       ("PAGEUP" . "\x1b;[5~")
                       ("PAGEDOWN" . "\x1b;[6~")
                       ("S-TAB" . "\x1b;[Z")
                       ("F1" . "\x1b;OP") ("F2" . "\x1b;OQ")
                       ("F3" . "\x1b;OR") ("F4" . "\x1b;OS")
                       ("F5" . "\x1b;[15~") ("F6" . "\x1b;[17~")
                       ("F7" . "\x1b;[18~") ("F8" . "\x1b;[19~")
                       ("F9" . "\x1b;[20~") ("F10" . "\x1b;[21~")
                       ("F11" . "\x1b;[23~") ("F12" . "\x1b;[24~")))
              => (lambda (entry) (string->utf8 (cdr entry)))]
             [else #f])]))

  (define (terminal-close! . buffer*)
    (let* ([buffer (if (pair? buffer*) (car buffer*) (current-buffer))]
           [state (terminal-of buffer)])
      (when state
        (terminal-state-alive-set! state #f)
        (close-terminal-process! (terminal-state-process state))
        (set! terminals (remq state terminals)))))

  (define (handle-terminal-event! state event)
    (cond
      ;; Once the PTY has closed, this is an ordinary read-only app again.
      ;; Let global chords such as C-x b, C-x k, and C-x o escape naturally
      ;; instead of silently sending them into a dead descriptor.
      [(not (terminal-state-alive state)) #f]
      [(string=? event "C-]")
       (escape-app-capture!
         "C-]" (lambda () (write-bytes! state (bytevector 29))))
       (set-message! "Terminal capture suspended for one e command")
       #t]
      [(string=? event "PASTE")
       (let ([text (read-paste)])
         (write-bytes!
           state
           (string->utf8
             (if (terminal-state-bracketed state)
                 (string-append "\x1b;[200~" text "\x1b;[201~") text))))
       #t]
      [(string=? event "MOUSE-CLICK")
       (if (terminal-state-mouse state)
           (let ([x (+ (cdr (point)) 1)] [y (+ (car (point)) 1)])
             (terminal-send! (format "\x1b;[<0;~a;~aM" x y))
             #t)
           #f)]
      [(string=? event "MOUSE-DRAG")
       (if (terminal-state-mouse state)
           (let ([x (+ (cdr (point)) 1)] [y (+ (car (point)) 1)])
             (terminal-send! (format "\x1b;[<32;~a;~aM" x y))
             #t)
           #f)]
      [(string=? event "MOUSE-RELEASE")
       (if (terminal-state-mouse state)
           (let ([x (+ (cdr (point)) 1)] [y (+ (car (point)) 1)])
             (terminal-send! (format "\x1b;[<0;~a;~am" x y))
             #t)
           #f)]
      [(member event '("WHEEL-UP" "WHEEL-DOWN"))
       (if (terminal-state-mouse state)
           (let ([x (+ (cdr (point)) 1)] [y (+ (car (point)) 1)])
             (terminal-send!
               (format "\x1b;[<~a;~a;~aM"
                       (if (string=? event "WHEEL-UP") 64 65) x y))
             #t)
           #f)]
      [(string=? event "MOUSE") #f]
      [(event-bytes event) => (lambda (bytes) (write-bytes! state bytes) #t)]
      [else #t]))

  (define (shell-command)
    ;; A PTY on standard input is enough for shells to enter interactive mode.
    ;; Do not assume a shared spelling for "login shell" options: several
    ;; perfectly usable shells reject -l.
    "exec \"${SHELL:-/bin/sh}\"")

  (define (terminal!! . command*)
    (let* ([command (if (pair? command*) (car command*) (shell-command))]
           [directory (or (and (buffer-file (current-buffer))
                               (let ([path (buffer-file (current-buffer))])
                                 (let loop ([index (- (string-length path) 1)])
                                   (cond [(< index 0) (current-directory)]
                                         [(char=? (string-ref path index) #\/)
                                          (substring path 0 (max 1 index))]
                                         [else (loop (- index 1))]))))
                          (current-directory))]
          )
      (set! serial (+ serial 1))
      (let* ([name (if (= serial 1) "*terminal*"
                       (format "*terminal*<~a>" serial))]
             [buffer #f]
             [state #f])
        (set! buffer
          (register-app!
            name
            (lambda () (when state (refresh-terminal! state)))
            (lambda (event) (and state (handle-terminal-event! state event)))))
        (set-app-presentation! buffer 0 #f #f 'block)
        (set-app-capture! buffer #t)
        (set-buffer-mode! buffer "terminal")
        (show-buffer! buffer)
        (let* ([size (or (buffer-window-size buffer) '(24 . 80))]
               [rows (max 1 (car size))]
               [cols (max 1 (cdr size))]
               [display (duplicate-output-port (terminal-output-port))]
               [process (spawn-terminal-process command directory rows cols)])
          (set! state
            (make-terminal-state buffer process display (make-mutex)
                                 rows cols (make-screen rows cols)
                                 0 0 0 0 0 (- rows 1)
                                 'normal "" #f 'ascii 0 #t #t #f
                                 #f #f #f '()
                                 (make-style-screen rows cols 'plain)
                                 #f '() "" 'plain))
          (set! terminals (cons state terminals))
          (fork-thread (lambda () (reader-loop state)))
          (void)))))

  (define (init!)
    (register-mode! "terminal" '() '() (lambda (line) #f)
                    #f terminal-row-styles)
    (bind-key! "C-c t" terminal!!)
    (add-buffer-kill-hook! terminal-close!)
    (add-status-hint!
      (lambda ()
        (let ([state (terminal-of (current-buffer))])
          (and state
               (if (terminal-state-alive state)
                   "C-] terminal commands"
                   "process exited")))))
    (register-descriptions!
      '(((terminal!!)
         (("procedure" . "(terminal!! [command])")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Open a PTY-backed terminal app running `$SHELL`, or `command` when supplied. It captures keyboard, paste, and mouse input. C-] suspends capture for one complete global e command; C-] C-] sends the character literally.")
        ((terminal-send!)
         (("procedure" . "(terminal-send! text)")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Send text to the process in the current terminal buffer.")
        ((terminal-close!)
         (("procedure" . "(terminal-close! [buffer])")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Terminate and detach the process owned by a terminal buffer."))))

) ;; library (terminal)
