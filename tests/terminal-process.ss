#!/usr/bin/env scheme-script

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (sys sys) sys:) (prefix (service vt) vt:) (prefix (service git) git:)
             (prefix (state actor) actor:) (prefix (state store) store:) (prefix (state surface) surface:) (prefix (state view) view:)
             (prefix (core kernel) kernel:) (prefix (foundation text) text:)
             (prefix (sys activity) activity:)
             (prefix (test) test:) (prefix (fixture) fixture:))

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
           ;; On Linux, wait until this owned child is a zombie before GC:
           ;; its unread stdout must not let Chez discard its exit status.
           ;; Other hosts retain the same completion check after collection.
           (when (test:child-pids)
             (let ([pid (car (remp (lambda (pid) (memv pid (car before))) (test:child-pids)))])
               (test:await 'other-command-is-waitable
                 (lambda ()
                   (guard (ex [(i/o-file-does-not-exist-error? ex) #f])
                     (call-with-input-file (format "/proc/~a/stat" pid)
                       (lambda (port) (read port) (read port) (eq? (read port) 'Z))))))))
           (collect (collect-maximum-generation))
           (let ([output (get-bytevector-all (sys:process-input other))])
             (check 'other-command-retains-completion-through-collection
               (equal? (cons (utf8->string output) (call-with-values (lambda () (sys:process-result other)) list))
                       '("still here" 23 "")))))
         (lambda () (sys:close-process! other)))
       (check 'command-table-releases-resources
         (equal? (list (test:child-pids) (test:fd-count)) before)))

     (define (process-reader process)
       (transcoded-port (sys:terminal-process-input process) (make-transcoder (utf-8-codec) 'none 'replace)))
     (define (read-until input done?)
       ;; Collect the child's output until the slave closes or done? accepts
       ;; the text so far. A child that produces neither within ten seconds
       ;; fails with what it did print instead of blocking the suite.
       (let ([deadline (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 10))])
         (let loop ([characters '()])
           (let ([text (list->string (reverse characters))])
             (cond
               [(done? text) text]
               [(time>=? (current-time 'time-monotonic) deadline)
                (error 'terminal-process-test "the child produced no further output" text)]
               [(guard (ex [(i/o-read-error? ex) 'closed]) (char-ready? input))
                => (lambda (ready)
                     (if (eq? ready 'closed) text
                         (let ([character (guard (ex [(i/o-read-error? ex) (eof-object)]) (get-char input))])
                           (if (eof-object? character) text (loop (cons character characters))))))]
               [else (sleep (make-time 'time-duration 5000000 0)) (loop characters)])))))
     (define (read-process process) (read-until (process-reader process) (lambda (text) #f)))

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
       (let* ([text ""]
              [drain (fixture:terminal-reader process
                       (lambda (chunk) (set! text (string-append text chunk))))])
         (test:parallel 2 (lambda (index) (test:await 'concurrent-pty-drain drain)))
         (sys:reap-terminal-process! process)
         (check 'interactive-input (contains? text "input=<hello terminal>"))
         (check 'stderr-shares-pty (contains? text "stderr-line"))))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "trap 'printf resized=; stty size; exit 0' WINCH; echo ready; while :; do sleep 0.05; done"
               (current-directory) 5 20)]
            [input (process-reader process)])
       (check 'resize-child-ready
              (contains? (read-until input (lambda (text) (contains? text "ready\r\n"))) "ready\r"))
       (sys:resize-terminal-process! process 9 37)
       (check 'resized-window-size (contains? (read-until input (lambda (text) #f)) "resized=9 37"))
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
            [id #f] [owner #f] [subscription #f]
            ;; Keep this caller's exports before reload rebinds M-x's prefix.
            [close! vt:close!] [views '()]
            [interfered? #f]
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
       (define (lease from)
         (let ([v (cdr (assoc from views))]) (list v (view:generation (view:snapshot v)))))
       (define (send from text size)
         (actor:send! owner (list 'input from id "TEXT" (list (cons 'text text) (cons 'view (lease from)) (cons 'size size)))))
       (define (offer from size)
         (actor:send! owner (list 'request from id 'resize (list (cons 'view (lease from)) (cons 'size size)))))
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
                          (note 'finish-ready)
                          ;; The parent's replace deletes and recreates the
                          ;; marker, so a poll can find it missing for a moment.
                          (let wait ()
                            (unless (equal? (guard (ex [else #f]) (call-with-input-file ,marker read))
                                            'finish-release)
                              (sleep (make-time 'time-duration 5000000 0)) (wait)))
                          (display "\x1b;[?2026h\x1b;[2J\x1b;[H\x1b;[35m界q\x301;FINAL\x1b;[6n")
                          (flush-output-port)
                          (reply-through #\R)
                          (note 'finish-parsed)
                          (exit)])
                       (flush-output-port)
                       (loop)))))) 'replace)
           (kernel:load-module! "vt")
           (set! id (vt:open! first (format "exec scheme-script ~a" child) (current-directory) 3 24))
           (set! owner (store:property id 'app))
           (set! views (map (lambda (who)
                              (let ([v (view:create! who id 'terminal 1 '() '(partial #t))])
                                (view:claim! who v) (cons who v))) (list first second)))
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
           (send first "alt\n" '(3 24))
           (test:await 'alternate-output (lambda () (published? "NEW")))
           (test:await 'shared-title (lambda () (string=? (store:buffer-name id) "*fixture*")))
           (test:check 'alternate-frame-retains-history-and-addresses-notices-to-controller
             (list (substring (store:line id 1) 0 6) (has? "history0")
                   (store:property id 'cursor-style) (caddr (caddr (surface:snapshot id)))
                   (store:property id 'clipboard))
             `("界q\x301;NEW" #t bar #f (1 ,first "shared")))
           (send first "mouse\n" '(3 24))
           (wait-stage 'mouse)
           (let* ([frame (surface:snapshot id)] [generation (car frame)] [revision (cadr frame)])
             (for-each
               (lambda (entry)
                 (actor:send! owner
                   (list 'input first id "MOUSE-CLICK"
                     `((view . ,(lease first)) (size 3 24) (revision . ,(car entry)) (generation . ,(cadr entry))
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
           ;; One PTY, two views in one head and another head. The latest
           ;; admitted input owns geometry; ownership generations fence input
           ;; queued by a released mount even after the same actor reclaims it.
           (let* ([a (view:create! first id 'terminal 1 '() '())]
                  [b (view:create! first id 'terminal 1 '() '())]
                  [c (view:create! second id 'terminal 1 '() '())]
                  [sizes (test:recorder)] [token #f])
             (define (witness v) (list v (view:generation (view:snapshot v))))
             (define (input who witness size)
               (actor:send! owner (list 'input who id "TEXT"
                                    (list (cons 'text "") (cons 'view witness) (cons 'size size)))))
             (define (resize who witness size)
               (actor:send! owner (list 'request who id 'resize
                                    (list (cons 'view witness) (cons 'size size)))))
             (define (sized size)
               (test:await 'view-controller-size (lambda () (equal? (store:property id 'size) size))))
             (for-each (lambda (v) (view:claim! (if (equal? v c) second first) v)) (list a b c))
             (set! token (store:subscribe! id
                           (lambda (event)
                             (when (and (eq? (car event) 'property) (eq? (caddr event) 'size))
                               (sizes (store:property id 'size))))))
             (input first (witness a) '(7 31)) (sized '(7 31))
             (actor:send! owner (list 'input first id "UNKNOWN-KEY"
                                  (list (cons 'view (witness b)) '(size 2 12))))
             (resize first (witness b) '(2 12))
             (offer first '(2 12))
             (resize first (witness a) '(6 30)) (sized '(6 30))
             (input first (witness b) '(8 32)) (sized '(8 32))
             (let ([old (witness b)])
               (view:release! first b (cadr old))
               (view:claim! first b)
               (input first old '(2 12))
               (resize first (witness b) '(2 12))
               (input second (witness c) '(9 33)) (sized '(9 33)))
             (test:check 'view-generation-fences-input-and-same-head-resize
               (sizes) '((7 31) (6 30) (8 32) (9 33)))
             (for-each (lambda (v) (view:release! (if (equal? v c) second first) v
                                     (view:generation (view:snapshot v)))) (list a b c))
             (store:unsubscribe! token)
             (test:check 'release-keeps-the-process-and-last-grid
               (list (store:property id 'alive) (store:property id 'size)) '(#t (9 33))))
           (send second "size\n" '(5 30))
           (wait-stage 'size)
           (offer first '(3 24))
           (actor:send! owner (list 'input first id "FOCUS" (list (cons 'view (lease first)) '(size 3 24))))
           (test:await 'latest-typist-size (lambda () (published? "5 30")))
           (offer second '(4 26))
           (test:await 'controller-size-offer (lambda () (equal? (store:property id 'size) '(4 26))))
           (test:check 'latest-typist-owns-resize-and-focus-does-not-steal-it
             (store:property id 'size) '(4 26))
           ;; Reloading the engine also reloads its facade. Durable state and
           ;; endpoints must still reach the worker created by the old code.
           (kernel:reload-module! "vt")
           (test:check 'engine-and-facade-reload-retain-the-base-actor (store:property id 'app) owner)
           (send second "finish\n" '(4 26))
           (wait-stage 'finish-ready)
           (actor:register! '(head "pause checkpoint") void)
           (actor:checkpoint! '(head "pause checkpoint") 'before)
           (activity:pause! (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 2)))
           (let* ([before (transcript)] [frame (surface:snapshot id)]
                  [started (test:recorder)] [completed (test:recorder)]
                  [workers
                   (map (lambda (operation)
                          (test:worker
                            (lambda ()
                              (started operation)
                              (case operation
                                [(checkpoint) (actor:checkpoint! '(head "pause checkpoint") 'after)]
                                [(input) (offer second '(3 24))]
                                [(process)
                                 (with-command '("/bin/true") #f
                                   (lambda (p)
                                     (get-bytevector-all (sys:process-input p))
                                     (test:check 'resumed-child-exits
                                       (call-with-values (lambda () (sys:process-result p)) list) '(0 ""))))])
                              (completed operation)))) '(checkpoint input process))])
             (dynamic-wind void
               (lambda ()
                 (test:await 'paused-callers (lambda () (= (length (started)) 3)))
                 (call-with-output-file marker (lambda (p) (write 'finish-release p)) 'replace)
                 (wait-stage 'finish-parsed)
                 (test:await 'natural-exit-during-pause (lambda () (not (member owner (vt:running)))))
                 (test:check 'pause-keeps-parsing-without-publishing-or-admitting-side-effects
                   (list (transcript) (surface:snapshot id) (completed)
                         (actor:checkpoint '(head "pause checkpoint")))
                   (list before frame '() 'before)))
               activity:resume!)
             (for-each (lambda (finish) (finish)) workers)
             (test:check 'resume-admits-checkpoint-input-and-process
               (list (length (completed)) (actor:checkpoint '(head "pause checkpoint"))) '(3 after)))
           (actor:detach! '(head "pause checkpoint"))
           (test:await 'offscreen-exit (lambda () (not (store:property id 'alive))))
           (test:await 'actor-retired (lambda () (not (offer second '(4 26)))))
           (test:check 'exit-publishes-final-held-text-before-retiring-capture-and-rendition
             (list (has? "界q\x301;FINAL") interfered? (retired)
                   (exists (lambda (event) (eq? (car event) 'reset)) (events)))
             '(#t applied ((#f #f)) #f)))
         (lambda ()
           (when subscription (store:unsubscribe! subscription))
           (when id
             (close! id)
             (when (store:exists? id) (store:delete! first id)))
           (for-each (lambda (file) (when (file-exists? file) (delete-file file))) (list child marker)))))

     ;; Store lifetime owns the process even if no head ever adopts it.
     (let* ([open! (eval '(begin (import (prefix (service vt) refreshed:)) refreshed:open!))]
            [id (open! '(agent "fixture") "printf ready; read value" (current-directory) 2 12)]
            [owner (store:property id 'app)])
       (test:await 'deletion-child-ready (lambda () (contains? (store:line id 0) "ready")))
       (store:delete! '(agent "fixture") id)
       (test:await 'deletion-closes-actor
         (lambda () (not (actor:send! owner (list 'request '(agent "fixture") id 'resize '(2 12))))))
       (test:check 'deletion-retires-the-surface (surface:snapshot id) #f))

     (test:finish! 'terminal-process)))
