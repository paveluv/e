#!/usr/bin/env scheme-script

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (vt) vt:)
             (prefix (head) head:) (prefix (paint) paint:) (prefix (render) render:)
             (prefix (actor) actor:) (prefix (store) store:) (prefix (surface) surface:)
             (prefix (kernel) kernel:) (prefix (log) log:) (prefix (text) text:)
             (prefix (test) test:))

     (define (check label true?)
       (test:check label true? #t))

     (define (contains? text part)
       (let ([n (string-length text)] [m (string-length part)])
         (let loop ([at 0])
           (and (<= (+ at m) n)
                (or (string=? (substring text at (+ at m)) part)
                    (loop (+ at 1)))))))

     (define (read-process process)
       (let ([input (transcoded-port
                      (sys:terminal-process-input process)
                      (make-transcoder (utf-8-codec) 'none 'replace))])
         (let loop ([characters '()])
           (guard (ex [(i/o-read-error? ex)
                       (list->string (reverse characters))]
                      [else (raise ex)])
             (let ([character (get-char input)])
               (if (eof-object? character)
                   (list->string (reverse characters))
                   (loop (cons character characters))))))))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "printf 'pid=%s tty=' $$; if test -t 0; then printf yes; else printf no; fi; printf ' size='; stty size; printf ' term=%s' \"$TERM\""
               (current-directory) 13 47)]
            [pid (sys:terminal-process-pid process)]
            [output (read-process process)])
       (sys:reap-terminal-process! process)
       (check 'direct-exec-pid
              (contains? output (format "pid=~a" pid)))
       (check 'controlling-terminal (contains? output "tty=yes"))
       (check 'initial-window-size (contains? output "size=13 47"))
       (check 'advertised-terminal (contains? output "term=xterm-256color")))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "read value; printf 'input=<%s>\\n' \"$value\"; printf 'stderr-line\\n' >&2"
               (current-directory) 5 20)]
            [output (sys:terminal-process-output process)])
       (put-bytevector output (string->utf8 "hello terminal\n"))
       (flush-output-port output)
       (let ([text (read-process process)])
         (sys:reap-terminal-process! process)
         (check 'interactive-input (contains? text "input=<hello terminal>"))
         (check 'stderr-shares-pty (contains? text "stderr-line"))))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "trap 'printf resized=; stty size; exit 0' WINCH; echo ready; while :; do sleep 1; done"
               (current-directory) 5 20)]
            [input (transcoded-port
                     (sys:terminal-process-input process)
                     (make-transcoder (utf-8-codec) 'none 'replace))])
       (check 'resize-child-ready (string=? (get-line input) "ready\r"))
       (sys:resize-terminal-process! process 9 37)
       (let loop ([characters '()])
         (guard (ex [(i/o-read-error? ex)
                     (let ([output (list->string (reverse characters))])
                       (check 'resized-window-size
                              (contains? output "resized=9 37")))]
                    [else (raise ex)])
           (let ([character (get-char input)])
             (unless (eof-object? character)
               (loop (cons character characters))))))
       (sys:reap-terminal-process! process))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "trap '' TERM; echo ready; while :; do sleep 1; done"
               (current-directory) 5 20)]
            [start (current-time 'time-monotonic)])
       ;; Let the shell install its ignored-SIGTERM disposition.
       (sleep (make-time 'time-duration 50000000 0))
       (sys:close-terminal-process! process)
       (let* ([elapsed (time-difference (current-time 'time-monotonic) start)]
              [milliseconds (+ (* (time-second elapsed) 1000)
                               (quotient (time-nanosecond elapsed) 1000000))])
         (check 'bounded-stubborn-child (< milliseconds 1000))))

     ;; One child exercises the shared producer without relying on the head
     ;; pump. File handshakes delimit input phases; DSR proves a held frame
     ;; has been parsed before we inspect its still-unpublished state.
     (let* ([child (format "/tmp/e-terminal-frame-~a.ss" (get-process-id))]
            [marker (string-append child ".phase")]
            [first '(head "first")] [second '(head "second")]
            [previous (head:window-buffer (head:current))]
            [id #f] [owner #f] [buffer #f] [subscription #f]
            ;; Keep this caller's exports before reload rebinds M-x's prefix.
            [send! vt:send!] [close! vt:close!]
            [phase 'initial] [coherent? #f] [complete? #f] [interfered? #f]
            [events (test:recorder)] [retired (test:recorder)] [audit (test:recorder)])
       (define (transcript) (let-values ([(text revision) (store:snapshot id)]) text))
       (define (has? part)
         (exists (lambda (line) (contains? line part)) (vector->list (transcript))))
       (define (published? part)
         (and (has? part)
              (let ([frame (surface:snapshot id)]) (and frame (= (cadr frame) (store:revision id))))))
       (define (stage)
         (guard (ex [else #f]) (call-with-input-file marker read)))
       (define (wait-stage value) (test:await value (lambda () (equal? (stage) value))))
       (define (send from text size) (send! from id text size #f))
       (define (offer from size) (actor:send! owner (list 'request from id 'resize size)))
       (dynamic-wind
         (lambda ()
           (call-with-output-file child
             (lambda (port)
               (for-each (lambda (form) (write form port) (newline port))
                 `((import (chezscheme))
                   (system "stty raw -echo")
                   (define (note value)
                     (call-with-output-file ,marker
                       (lambda (port) (write value port)) 'replace))
                   (define (reply-through end)
                     (let loop ([out '()])
                       (let ([ch (get-char (current-input-port))])
                         (if (char=? ch end) (list->string (reverse (cons ch out)))
                             (loop (cons ch out))))))
                   (display "\x1b;[?1004hhistory0\r\nhistory1\r\nlive0\r\nlive1")
                   (flush-output-port)
                   (let loop ()
                     (let ([command (get-line (current-input-port))])
                       (case (string->symbol command)
                         [(alt)
                          (display "\x1b;[?1049h\x1b;[?25l\x1b;[6 q\x1b;[32m\x1b;]8;id=live;https://frame.example\x1b;\\界q\x301;NEW\x1b;]8;;\x1b;\\\x1b;[?1002h\x1b;[?1006h\x1b;]52;c;c2hhcmVk\x7;\x1b;]0;fixture\x7;")]
                         [(mouse)
                          (note 'mouse)
                          (note (list 'mouse (reply-through #\M)))]
                         [(hold)
                          (display "\x1b;[?2026h\x1b;[2J\x1b;[HHOLD\x1b;[6n")
                          (flush-output-port)
                          (reply-through #\R)
                          (note 'held)]
                         [(release) (display "\x1b;[?2026l")]
                         [(main) (display "\x1b;[?1049l\x1b;[?1002l\x1b;[?1006l")]
                         [(size)
                          (note 'size)
                          (do ([i 0 (+ i 1)]) ((= i 3)) (get-char (current-input-port)))
                          (system "stty size")]
                         [(finish)
                          (display "\x1b;[?2026h\x1b;[2J\x1b;[H\x1b;[35m界q\x301;FINAL")
                          (flush-output-port)
                          (exit)])
                       (flush-output-port)
                       (loop)))))) 'replace)
           (kernel:load-module! "vt")
           (kernel:load-module! "terminal")
           (head:set-frame-hook! (lambda () (void)))
           (head:before-frame!)
           (log:set-presenter!
             (lambda (entry show?)
               (when (and (eq? (cadr entry) 'store)
                          (contains? (caddr entry) "(app terminal")) (audit show?))))
           (set! id (vt:open! first (format "exec scheme-script ~a" child) (current-directory) 3 24))
           (set! owner (store:property id 'app))
           (set! subscription
             (store:subscribe! id
               (lambda (event)
                 (events event)
                 ;; A receipt is its commit, not the state after callouts.
                 ;; Race one final frame with a forced edit through the seam.
                 (when (and (not interfered?) (eq? (car event) 'edit) (has? "FINAL"))
                   (set! interfered? 'pending)
                   (let* ([text (transcript)]
                          [row (find (lambda (i) (contains? (vector-ref text i) "FINAL"))
                                     (iota (vector-length text)))])
                     (let-values ([(status revision)
                                   (store:edit! '(agent "reentry") id (store:revision id)
                                     (text:make-span row 0 row (string-length (vector-ref text row)))
                                     '("intervening write"))])
                       (set! interfered? status))))
                 (when (and (eq? (car event) 'property) (eq? (caddr event) 'alive)
                            (not (store:property id 'alive)))
                   (retired (list (store:property id 'capture) (surface:snapshot id))))))))
         (lambda ()
           (test:await 'shared-output (lambda () (published? "live1")))
           (test:check 'base-transcript-has-text-history-and-no-head-dependency
             (list (has? "history0") (substring (store:buffer-name id) 0 1)
                   (map (lambda (name) (kernel:module-requires? "vt" name))
                        '("head" "edit" "paint" "mode" "log")))
             '(#t "*" (#f #f #f #f #f)))
           (set! buffer (head:adopt-store-buffer! id))
           (head:buffer-line-numbers-setting-set! buffer #f)
           (head:window-size-set! (head:current) 3)
           (head:window-width-set! (head:current) 24)
           (head:set-repaint-hook!
             (lambda ()
               (when (eq? phase 'initial)
                 (set! phase 'waiting)
                 (let ([header (render:header (head:buffer-rendition buffer))])
                   (set! coherent? (and header (= (cadr header) (head:buffer-store-rev buffer)))))
                 (send first "alt\n" '(3 24))
                 (test:await 'producer-during-repaint (lambda () (published? "NEW")))
                 (head:before-frame!)
                 (set! complete? #t))))
           (head:set-window-buffer! (head:current) buffer)
           (test:await 'shared-title (lambda () (string=? (store:buffer-name id) "*fixture*")))
           (test:check 'reentrant-adoption-uses-coherent-shared-rendition
             (list coherent? complete? (head:app-of buffer)
                   (substring (store:line id 1) 0 6) (has? "history0")
                   (head:app-cursor-style buffer) (head:app-cursor-visible-in? (head:current))
                   (paint:buffer-line-hyperlinks buffer 1)
                   (store:property id 'clipboard) (store:buffer-name id))
             `(#t #t #f "界q\x301;NEW" #t bar #f ((0 6 "https://frame.example" "live"))
               (1 ,first "shared") "*fixture*"))
           (check 'background-app-audit-keeps-records-without-echo
             (and (pair? (audit)) (for-all not (audit))))
           (head:set-repaint-hook! paint:invalidate-screen-cache!)
           (send first "mouse\n" '(3 24))
           (wait-stage 'mouse)
           (let* ([frame (surface:snapshot id)] [generation (car frame)] [revision (cadr frame)])
             (for-each
               (lambda (entry)
                 (actor:send! owner
                   (list 'input first id "MOUSE-CLICK"
                     `((size 3 24) (revision . ,(car entry)) (generation . ,(cadr entry))
                       (cell . ,(caddr entry)) (button . 0)))))
               (list (list revision (- generation 1) '(1 . 4))
                     (list (- revision 1) generation '(1 . 5))
                     (list revision generation '(0 . 2))
                     (list revision generation '(1 . 24))
                     (list revision generation '(1 . 2)))))
           (test:await 'mouse-reply (lambda () (pair? (stage))))
           (test:check 'only-current-in-grid-pointer-reaches-the-pty (stage) '(mouse "\x1b;[<0;3;1M"))
           (let ([before (transcript)] [frame (surface:snapshot id)] [count (length (events))])
             (send first "hold\n" '(3 24))
             (wait-stage 'held)
             (sleep (make-time 'time-duration 50000000 0))
             (test:check 'mode-2026-holds-text-and-surface
               (list (transcript) (surface:snapshot id)) (list before frame))
             (test:await 'bounded-synchronized-publication (lambda () (published? "HOLD")))
             (test:check 'one-held-frame-is-one-attributed-edit-without-redundant-facts
               (map (lambda (event) (list (car event) (list-ref event 3)))
                    (list-tail (events) count)) (list (list 'edit owner))))
           (send first "release\nmain\n" '(3 24))
           (test:await 'main-screen-restored (lambda () (published? "live1")))
           (check 'alternate-output-preserves-main-history-and-releases-mouse-capture
             (and (has? "history0") (member "MOUSE-CLICK" (cdr (store:property id 'capture))) #t))
           (send second "size\n" '(5 30))
           (wait-stage 'size)
           (offer first '(3 24))
           (actor:send! owner (list 'input first id "FOCUS" '((size 3 24))))
           (test:await 'latest-typist-size (lambda () (published? "5 30")))
           (offer second '(4 26))
           (test:await 'controller-size-offer (lambda () (equal? (store:property id 'size) '(4 26))))
           (test:check 'latest-typist-owns-resize-and-focus-does-not-steal-it
             (store:property id 'size) '(4 26))
           ;; Reloading the engine also reloads its facade. Durable state and
           ;; endpoints must still reach the worker created by the old code.
           (kernel:reload-module! "vt")
           (test:check 'engine-and-facade-reload-retain-the-base-actor (store:property id 'app) owner)
           (head:set-window-buffer! (head:current) previous)
           (send second "finish\n" '(4 26))
           (test:await 'offscreen-exit (lambda () (not (store:property id 'alive))))
           (test:await 'actor-retired (lambda () (not (actor:send! owner (list 'request second id 'resize '(4 26))))))
           (test:check 'exit-publishes-final-held-text-before-retiring-capture-and-rendition
             (list (has? "界q\x301;FINAL") interfered? (retired)
                   (exists (lambda (event) (eq? (car event) 'reset)) (events)))
             '(#t applied ((#f #f)) #f)))
         (lambda ()
           (log:set-presenter! #f)
           (head:set-repaint-hook! paint:invalidate-screen-cache!)
           (head:set-frame-hook! paint:redraw!)
           (when subscription (store:unsubscribe! subscription))
           (when id
             (close! id)
             (when (store:exists? id) (store:delete! first id)))
           (when buffer (head:forget-buffer! buffer))
           (for-each (lambda (file) (when (file-exists? file) (delete-file file))) (list child marker)))))

     ;; Store lifetime owns the process even if no head ever adopts it.
     (let* ([open! (eval '(begin (import (prefix (vt) refreshed:)) refreshed:open!))]
            [id (open! '(agent "fixture") "printf ready; read value" (current-directory) 2 12)]
            [owner (store:property id 'app)])
       (test:await 'deletion-child-ready (lambda () (contains? (store:line id 0) "ready")))
       (store:delete! '(agent "fixture") id)
       (test:await 'deletion-closes-actor
         (lambda () (not (actor:send! owner (list 'request '(agent "fixture") id 'resize '(2 12))))))
       (test:check 'deletion-retires-the-surface (surface:snapshot id) #f))

     (test:finish! 'terminal-process)))
