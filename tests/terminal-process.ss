#!/usr/bin/env scheme-script

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (vt) vt:) (prefix (git) git:)
             (prefix (head) head:) (prefix (paint) paint:) (prefix (render) render:)
             (prefix (actor) actor:) (prefix (store) store:) (prefix (surface) surface:)
             (prefix (kernel) kernel:) (prefix (text) text:)
             (prefix (test) test:))

     (define (check label true?)
       (test:check label true? #t))

     (define (contains? text part)
       (let ([n (string-length text)] [m (string-length part)])
         (let loop ([at 0])
           (and (<= (+ at m) n)
                (or (string=? (substring text at (+ at m)) part)
                    (loop (+ at 1)))))))

     (define (with-command arguments bytes use)
       (let ([process #f])
         (dynamic-wind #t
           (lambda ()
             (when process (error 'command-test "scope has ended"))
             (set! process (sys:open-process arguments)))
           (lambda () (sys:write-process! process bytes) (use process))
           (lambda () (sys:close-process! process)))))

     ;; One unrelated command stays alive throughout the table and Git calls.
     ;; Its final reply/status prove cleanup never killed or reaped its PID.
     (let* ([before (list (test:child-pids) (test:fd-count))]
            [other (sys:open-process '("/bin/sh" "-c" "printf ready; read value; printf %s \"$value\"; exit 23"))])
       (dynamic-wind void
         (lambda ()
           (check 'other-command-ready
             (equal? (get-bytevector-n (sys:process-input other) 5) (string->utf8 "ready")))
           (let ([resources (list (test:child-pids) (test:fd-count))]
                 [quoted "spaces ' and ; $() `literal`"]
                 [zeros (make-bytevector 131072 0)]
                 [body (make-bytevector 524288 120)])
             (check 'command-completion
               (for-all
                 (lambda (row)
                   (with-command (car row) (cadr row)
                     (lambda (process)
                       ;; Read EOF before asking for status; argument evaluation
                       ;; order must not decide the resource protocol.
                       (let ([output (get-bytevector-all (sys:process-input process))])
                         (equal? (cons output (call-with-values (lambda () (sys:process-result process)) list))
                                 (cddr row))))))
                 (list
                   (list (list "/bin/sh" "-c" "printf %s \"$1\"; printf %s \"$2\" >&2; exit 7" "fixture" quoted quoted)
                         #f (string->utf8 quoted) 7 quoted)
                   (list '("/bin/sh" "-c" "head -c 131072 /dev/zero; head -c 131072 /dev/zero >&2; exec cat")
                         body (let ([expected (make-bytevector (+ (bytevector-length zeros) (bytevector-length body)) 120)])
                                (bytevector-copy! zeros 0 expected 0 (bytevector-length zeros)) expected)
                         0 (make-string 131072 #\nul))
                   (list '("/bin/sh" "-c" "kill -TERM $$") #f (eof-object) -15 ""))))
             (check 'command-scope-exits
               (for-all
                 (lambda (exit)
                   (let* ([held #f] [expired #f] [start (current-time 'time-monotonic)]
                          [outcome
                           (guard (ex [(eq? ex 'command-error) 'raised])
                             ((make-engine
                                (lambda ()
                                  (with-command '("/bin/sh" "-c" "trap '' TERM; printf ready; exec sleep 20") #f
                                    (lambda (process)
                                      (set! held process)
                                      (get-bytevector-n (sys:process-input process) 5)
                                      (case exit
                                        [(close) (close-port (sys:process-input process)) 'closed]
                                        [(raise) (raise 'command-error)]
                                        [(fuel) (engine-block)])))))
                              1000000 (lambda (ticks value) value)
                              (lambda (engine) (set! expired engine) 'expired)))])
                     (and (eq? outcome (case exit [(close) 'closed] [(raise) 'raised] [else 'expired]))
                          (port-closed? (sys:process-input held))
                          (= (time-second (time-difference (current-time 'time-monotonic) start)) 0)
                          (equal? (call-with-values (lambda () (sys:process-result held)) list) '(-9 ""))
                          (or (not expired)
                              (test:raises?
                                (lambda () (expired 1000000 (lambda args (void)) (lambda args (void))))
                                (lambda (ex) (and (who-condition? ex) (eq? (condition-who ex) 'command-test))))))))
                 '(close raise fuel)))
             (do ([i 0 (+ i 1)]) ((= i 8))
               (let ([repository (git:open ".")]) (git:current-branch repository) (git:status repository)))
             (check 'git-error-keeps-real-status
               (test:raises?
                 (lambda () (git:open "/no/such/e-repository __E_GIT_STATUS__=1 '/missing"))
                 (lambda (ex)
                   (and (git:error? ex) (= (git:error-code ex) 128)
                        (contains? (git:error-stderr ex) "__E_GIT_STATUS__=1 '")))))
             (check 'commands-release-only-owned-resources
               (equal? (list (test:child-pids) (test:fd-count)) resources)))
           (sys:write-process! other (string->utf8 "still here\n"))
           (let ([output (get-bytevector-all (sys:process-input other))])
             (check 'other-command-retains-completion
               (equal? (cons (utf8->string output) (call-with-values (lambda () (sys:process-result other)) list))
                       '("still here" 23 "")))))
         (lambda () (sys:close-process! other)))
       (check 'command-table-releases-resources
         (equal? (list (test:child-pids) (test:fd-count)) before)))

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
            [gap-entered (test:gate)] [gap-release (test:gate)] [gap-result #f]
            [events (test:recorder)] [retired (test:recorder)])
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
       (define (head-view)
         (let ([frame (head:buffer-rendition buffer)] [w (head:current)])
           (list (head:buffer-lines buffer) (head:buffer-store-rev buffer)
                 (render:header frame) (render:row frame (head:window-top w))
                 (head:window-prow w) (head:window-pcol w) (head:window-top w))))
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
                         [(gap) (display "\x1b;[H\x1b;[31mGAP\x1b;[0m")]
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
           (set! id (vt:open! first (format "exec scheme-script ~a" child) (current-directory) 3 24))
           (set! owner (store:property id 'app))
           (set! subscription
             (store:subscribe! id
               (lambda (event)
                 (events event)
                 (when (and (not (gap-entered)) (eq? (car event) 'edit) (has? "GAP"))
                   (gap-entered #t)
                   (test:await 'release-frame gap-release))
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
                 (let ([before (head-view)])
                   (send first "gap\n" '(3 24))
                   (test:await 'text-before-rendition gap-entered)
                   (head:before-frame!)
                   (set! gap-result (list (> (store:revision id) (cadr before))
                                          (equal? before (head-view))))
                   (gap-release #t)
                   (test:await 'rendition-after-text (lambda () (published? "GAP")))
                   ;; Paint may refresh after a surface overtakes its source.
                   ;; Only the next adoption can install that complete pair.
                   (head:refresh-renditions!)
                   (set! gap-result (append gap-result (list (equal? before (head-view)))))
                   (head:before-frame!)
                   (let ([after (head-view)])
                     (set! gap-result
                       (append gap-result
                         (list (and (= (cadr after) (store:revision id))
                                    (equal? (car after) (transcript))
                                    (equal? (caddr after) (surface:snapshot id))))))))
                 (send first "alt\n" '(3 24))
                 (test:await 'producer-during-repaint (lambda () (published? "NEW")))
                 (head:before-frame!)
                 (set! complete? #t))))
           (head:set-window-buffer! (head:current) buffer)
           (test:await 'shared-title (lambda () (string=? (store:buffer-name id) "*fixture*")))
           (test:check 'reentrant-adoption-uses-coherent-shared-rendition
             (list coherent? complete? gap-result (head:app-of buffer)
                   (substring (store:line id 1) 0 6) (has? "history0")
                   (head:app-cursor-style buffer) (head:app-cursor-visible-in? (head:current))
                   (paint:buffer-line-hyperlinks buffer 1)
                   (store:property id 'clipboard) (store:buffer-name id))
             `(#t #t (#t #t #t #t) #f "界q\x301;NEW" #t bar #f ((0 6 "https://frame.example" "live"))
               (1 ,first "shared") "*fixture*"))
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
           (gap-release #t)
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
