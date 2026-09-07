#!/usr/bin/env scheme-script

;; One protocol fixture: framing, a real headless bootstrap, concurrent
;; connections and cleanup. The daemon gets an isolated installation/config.
(import (chezscheme))
(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (wire) wire:) (prefix (sys) sys:) (prefix (test) test:)
             (prefix (string) string:) (prefix (kernel) kernel:) (prefix (text) text:))

     (define (encoded value)
       (let-values ([(port result) (open-bytevector-output-port)])
         (wire:send! port value) (result)))
     (define (raw text)
       (let* ([bytes (string->utf8 text)] [out (make-bytevector (+ 4 (bytevector-length bytes)))])
         (bytevector-u32-set! out 0 (bytevector-length bytes) (endianness big))
         (bytevector-copy! bytes 0 out 4 (bytevector-length bytes)) out))
     (let* ([datum '(#f () (λ . "two\nlines") #(1 #\λ #vu8(0 255)))]
            [port (open-bytevector-input-port (encoded datum))])
       (test:check 'plain-data-and-clean-eof
         (list (wire:receive port) (eof-object? (wire:receive port))) (list datum #t)))
     (test:check 'bad-frames-refuse-before-use
       (map (lambda (bytes) (test:raises? (lambda () (wire:receive (open-bytevector-input-port bytes)))))
         (append '(#vu8(0 0) #vu8(0 0 0 0) #vu8(1 0 0 1) #vu8(0 0 0 3 40 41))
                 (map raw '("" "a b" "(" "#0=(a . #0#)")) ))
       (make-list 8 #t))
     (test:check 'runtime-objects-cannot-be-sent
       (map (lambda (value) (test:raises? (lambda () (encoded value)))) (list (box 1) void)) '(#t #t))

     (define root (format "/tmp/e-wire-~a-~a" (get-process-id) (random 1000000000)))
     (define sources (string-append root "/lib"))
     (define objects (string-append root "/eo"))
     (define socket (string-append root "/socket λ"))
     (define trigger (string-append root "/continue"))
     (define terminal-pid-file (string-append root "/terminal-pid"))
     (define inventory-file (string-append root "/sessions"))
     (define edit-held (string-append root "/edit-held"))
     (define edit-release (string-append root "/edit-release"))
     (define (quote-shell text)
       (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))
     (define (write-text path text)
       (call-with-output-file path (lambda (port) (display text port)) 'replace))
     (define (copy-text source target) (write-text target (call-with-input-file source get-string-all)))
     (define (write-forms path forms)
       (call-with-output-file path (lambda (port) (for-each (lambda (form) (pretty-print form port)) forms)) 'replace))
     (for-each (lambda (path) (mkdir path #o700)) (list root sources objects))
     (copy-text "e" (string-append root "/e"))
     (for-each (lambda (name) (copy-text (string-append "lib/" name) (string-append sources "/" name)))
       (filter (lambda (name) (string:suffix? ".e" name)) (directory-list "lib")))
     (write-text (string-append root "/config.e") "(error 'head-config \"daemon loaded head config\")\n")
     (write-forms (string-append root "/base-config.e")
       `((define footprint
           (list (filter (lambda (name) (member name '("head" "main" "edit" "paint" "terminal" "describe")))
                         (kernel:loaded-modules))
                 (kernel:module-requires? "base" "head") (actor:current) (store:buffer-list)))
         (define held (policy:mint! '(agent "authority probe") (policy:reader) '(head "desk λ")))
         (define old-eval policy:session-eval!)
         (define old-edit policy:session-edit!)
         (define old-undo policy:session-undo!)
         (define old-ask policy:session-ask!)
         (define refused-core
           (map (lambda (name)
                  (guard (ex [else (and (message-condition? ex)
                                        (string:search (condition-message ex) "restart e"
                                          0 (string-length (condition-message ex))) #t)])
                    (kernel:reload-module! name) #f))
                '("policy" "vt" "reference" "store" "base")))
         (define retained (old-eval held "(+ 1 2)"))
         (policy:revoke! held)
         (define revoked
           (list (old-eval held "(+ 1 2)")
                 (call-with-values (lambda () (old-edit held 999 0 (text:make-span 0 0 0 0) '("x"))) list)
                 (call-with-values (lambda () (old-undo held 999)) list)
                 (old-ask held "still active?" '() void) (policy:sessions)))
         (define notes (store:create! '(base e) "notes λ" '("hello λ")
                         (list (cons 'bootstrap footprint)
                               (cons 'process-id (get-process-id))
                               (cons 'authority (list refused-core retained revoked)))))
         (store:create! '(base e) "private" '("a local audience") '((audience (head "desk λ"))))
         (define default-policy (base:connection-policy))
         (base:connection-policy
           (lambda (actor)
             (if (member actor '((agent "first") (agent "second")))
                 (policy:make '() 10000 '("notes λ") 0)
                 (default-policy actor))))
         (define held-edit? #f)
         (log:subscribe!
           (lambda (record presentation)
             (when (eq? (log:component record) 'policy)
               (let ([event (log:datum record)])
                 (when (and (eq? (car event) 'edit) (equal? (cadr event) '(agent "first")) (not held-edit?))
                   (set! held-edit? #t)
                   (call-with-output-file ,edit-held (lambda (out) (display "committed" out)))
                   (let wait ()
                     (unless (file-exists? ,edit-release) (sleep (make-time 'time-duration 5000000 0)) (wait))))
                 (when (memq (car event) '(mint revoke))
                   (let ([inventory (policy:sessions)])
                     (store:set-property! '(base e) notes 'sessions inventory)
                     (call-with-output-file ,inventory-file (lambda (out) (write inventory out)) 'replace)))))))
         (actor:subscribe!
           (lambda (batch)
             (for-each (lambda (event)
                         (when (and (eq? (car event) 'attached) (memq (caadr event) '(head agent)))
                           (actor:send! (cadr event) '(from-base "welcome")))) batch)))
         (vt:shell "/bin/sh")
         (vt:open! '(base e)
           ,(format "echo $$ > ~a; printf 'still here'; read answer" (quote-shell terminal-pid-file))
           ,root 4 30)
         (fork-thread
           (lambda ()
             (let wait ()
               (unless (file-exists? ,trigger) (sleep (make-time 'time-duration 5000000 0)) (wait)))
             (collect)
             (store:reset! '(agent "background") notes '("agent work while detached"))))))

     (define command (format "exec scheme-script ~a --daemon --socket ~a"
                             (quote-shell (string-append root "/e")) (quote-shell socket)))
     (test:check 'noninteractive-head-refuses-before-base-config-starts-work
       (list (zero? (system (format "TERM=dumb scheme-script ~a > ~a 2>&1"
                                    (quote-shell (string-append root "/e"))
                                    (quote-shell (string-append root "/early-output")))))
             (file-exists? terminal-pid-file)) '(#f #f))
     (define ready (test:gate))
     (define output (test:recorder))
     (define clients '())
     (define stopped? #f)
     (define last-request #f)
     (define (receive connection)
       (guard (ex [else (error 'wire-test "receive failed" last-request (kernel:condition-text ex))])
         ((test:worker (lambda () (wire:receive (sys:connection-input connection)))))))
     (define (exchange connection message)
       (set! last-request message)
       (wire:send! (sys:connection-output connection) message)
       (receive connection))
     (define (connect)
       (let ([connection (sys:connect-local socket)])
         (set! clients (cons connection clients)) connection))
     (define (hello connection actor)
       (exchange connection (list 'hello wire:version actor)))
     (define (reply-value reply)
       (unless (and (list? reply) (= (length reply) 4) (equal? (list-head reply 3) '(reply 7 ok)))
         (error 'wire-test "request failed" last-request reply))
       (cadddr reply))
     (define (rpc connection operation . args)
       (reply-value (exchange connection (append (list 'request 7 operation) args))))
     (define (inventory connection)
       (cdr (assq 'sessions (caddr (rpc connection 'snapshot 1)))))

     (let-values ([(input from errors pid) (open-process-ports command 'block (native-transcoder))])
       (define out-done
         (test:worker
           (lambda ()
             (let loop ()
               (let ([line (get-line from)])
                 (unless (eof-object? line)
                   (output line)
                   (when (string:prefix? "e: listening on " line) (ready #t))
                   (loop)))))))
       (define err-done (test:worker (lambda () (let ([text (get-string-all errors)]) (if (eof-object? text) "" text)))))
       (define (signal! signal)
         (system (format "kill -~a ~a 2>/dev/null" signal pid)))
       (define (stop!)
         (unless stopped?
           (set! stopped? #t)
           (signal! "TERM")
           (guard (ex [else (signal! "KILL")
                            (error 'wire-test "daemon failed to stop" last-request (kernel:condition-text ex) (err-done))])
             (out-done)
             (let ([text (err-done)])
               (unless (string=? text "") (error 'wire-test "daemon stderr" text))))))
       (dynamic-wind void
         (lambda ()
           (guard (ex [else (error 'wire-test "daemon did not start" (output) (err-done))])
             (test:await 'daemon ready))
           (let* ([head (connect)] [identity '(head "desk λ")])
             (test:check 'claim-precedes-welcome-and-queued-mail
               (list (hello head identity) (receive head))
               (list (list 'hello 1 identity '(read edit undo)) '(event (from-base "welcome"))))
             (let* ([ids (rpc head 'buffers)] [snapshot (rpc head 'snapshot (car ids))]
                    [facts (caddr snapshot)])
               (test:check 'base-only-config-and-owned-snapshot
                 (list (map (lambda (id) (rpc head 'name id)) ids) (car snapshot)
                       (cdr (assq 'bootstrap facts)) (cdr (assq 'process-id facts)))
                 (list '("notes λ" "private" "*terminal*") '#("hello λ") '(() #f (base e) ()) pid))
               (test:check 'core-reload-refuses-and-explicit-revoke-reaches-retained-entries
                 (cdr (assq 'authority facts))
                 '((#t #t #t #t #t) (ok . "=> 3")
                   ((refused . "the session is revoked") (refused revoked) (refused revoked) #f ())))
               (string-set! (vector-ref (car snapshot) 0) 0 #\X)
               (test:check 'client-mutation-cannot-change-the-base
                 (car (rpc head 'snapshot (car ids))) '#("hello λ")))
             (test:check 'bad-version-and-duplicate-name-preserve-the-owner
               (map (lambda (version)
                      (let ([duplicate (connect)])
                        (list (car (exchange duplicate (list 'hello version identity)))
                              (eof-object? (receive duplicate))
                              (and (member identity (map car (rpc head 'actors))) #t)
                              (inventory head))))
                    '(0 1))
               (make-list 2 (list 'error #t #t (list (list identity identity)))))
             (test:check 'request-errors-preserve-the-connection
               (map (lambda (message) (list-head (exchange head message) 3))
                 '((request 1 edit) (request 2 snapshot 999) (request 3 buffers extra)
                   (request 4 edit 1 0 (0 0 -1 0) ("x")) (request 5 edit 1 0.5 (0 0 0 0) ("x"))
                   (request 6 undo 1 everyone) (request 7 actors)))
               '((reply 1 error) (reply 2 error) (reply 3 error) (reply 4 error)
                 (reply 5 error) (reply 6 error) (reply 7 ok)))
             (test:check 'existing-endpoint-is-never-unlinked
               (list (test:raises? (lambda () (sys:listen-local socket))) (rpc head 'name 1))
               '(#t "notes λ"))
             (sys:close-connection! head))
           ;; Leave one connection stalled before hello and another after it.
           ;; The background actor must still collect and publish with no head.
           (let ([idle (connect)] [agent (connect)] [identity '(agent "reader")])
             (test:check 'agent-uses-the-same-read-connection
               (list (hello agent identity) (receive agent) (rpc agent 'buffers)
                     (car (rpc agent 'snapshot 2)))
               (list (list 'hello 1 identity '(read)) '(event (from-base "welcome")) '(1 2 3)
                     '#("a local audience")))
             (test:await 'head-detached
               (lambda () (not (exists (lambda (entry) (eq? (caar entry) 'head)) (rpc agent 'actors)))))
             (let* ([first (connect)] [second (connect)]
                    [writers (list first second)] [actors '((agent "first") (agent "second"))])
               (test:check 'configured-agents-use-server-selected-permissions
                 (map (lambda (connection actor) (list (hello connection actor) (receive connection))) writers actors)
                 (map (lambda (actor) (list (list 'hello 1 actor '(read edit undo)) '(event (from-base "welcome")))) actors))
               ;; Hold the first policy audit callback after commit. A second
               ;; actor commits before the first reply: its receipt must still
               ;; describe exactly its own accepted revision and anchor chain.
               (wire:send! (sys:connection-output first) '(request 7 edit 1 0 (0 0 0 5) ("HELLO")))
               (test:await 'first-edit-committed (lambda () (file-exists? edit-held)))
               (let ([second-result (rpc second 'edit 1 0 '(0 7 0 7) '("!"))])
                 (write-text edit-release "continue")
                 (let ([results (list (reply-value (receive first)) second-result)])
                   (test:check 'authoritative-receipts-rebase-and-attribute-both-writers
                     (map
                       (lambda (result actor)
                         (let* ([receipt (cadr result)] [changes (caddr receipt)]
                                [reconstructed
                                 (fold-left
                                   (lambda (lines change)
                                     (let ([delta (text:datum->delta (caddr change))])
                                       (unless (equal? (text:extract lines (text:delta-span delta)) (text:delta-removed delta))
                                         (error 'wire-test "wrong removed text" change))
                                       (let-values ([(next actual) (text:apply-edit lines (text:delta-span delta) (text:delta-inserted delta))]) next)))
                                   '#("hello λ") changes)])
                           (list (car result) (car receipt) (cadr receipt)
                                 (equal? reconstructed (cadr receipt))
                                 (equal? (cadr (car (reverse changes))) actor)
                                 (map car changes))))
                       results actors)
                     '((applied 1 #("HELLO λ") #t #t (1)) (applied 2 #("HELLO λ!") #t #t (1 2))))))
               (test:check 'stale-and-permission-refusals-preserve-text
                 (list (rpc second 'edit 1 0 '(0 1 0 3) '("bad"))
                       (rpc agent 'edit 1 2 '(0 0 0 0) '("bad"))
                       (rpc first 'edit 2 0 '(0 0 0 0) '("bad"))
                       (rpc agent 'undo 1 'all) (rpc first 'undo 2)
                       (car (rpc agent 'snapshot 1)) (car (rpc agent 'snapshot 2)))
                 '((stale overlap) (refused buffer) (refused buffer) (refused buffer) (refused buffer)
                   #("HELLO λ!") #("a local audience")))
               (let* ([mine (rpc first 'undo 1)] [after-mine (car (rpc agent 'snapshot 1))]
                      [again (rpc first 'undo 1)] [other (rpc first 'undo 1 '(actor (agent "second")))])
                 (rpc second 'edit 1 4 '(0 0 0 0) '("B"))
                 (test:check 'undo-defaults-to-mine-with-explicit-actor-and-all-scopes
                   (list mine after-mine again other (rpc first 'undo 1 'all) (car (rpc agent 'snapshot 1)))
                   '((applied 3) #("hello λ!") (nothing #f) (applied 4) (applied 6) #("hello λ"))))
               (for-each sys:close-connection! writers)
               (test:await 'connection-sessions-revoked
                 (lambda () (equal? (inventory agent) (list (list identity #f))))))
             (write-text trigger "continue")
             (test:await 'background-agent
               (lambda () (equal? (car (rpc agent 'snapshot 1)) '#("agent work while detached"))))
             (test:check 'terminal-outlives-head-disconnect
               (cdr (assq 'alive (caddr (rpc agent 'snapshot 3)))) #t)
             (signal! "HUP")
             (test:check 'hup-keeps-the-base-and-current-revision
               (car (rpc agent 'snapshot 1)) '#("agent work while detached"))
             (let ([head (connect)])
               (hello head '(head "desk λ")) (receive head)
               (test:check 'released-name-can-read-current-state
                 (car (rpc head 'snapshot 1)) '#("agent work while detached")))
             (stop!)
             (test:check 'stop-closes-idle-clients-and-releases-the-path
               (list (eof-object? (receive idle)) (eof-object? (receive agent)) (file-exists? socket))
               '(#t #t #f)))
           (let ([terminal-pid (call-with-input-file terminal-pid-file read)])
             (test:check 'base-stop-reaps-its-terminal-and-revokes-every-session
               (list (zero? (system (format "kill -0 ~a 2>/dev/null" terminal-pid)))
                     (call-with-input-file inventory-file read)) '(#f ())))
           (let ([listener (sys:listen-local socket)]) (sys:close-local-listener! listener))
           (write-text socket "ordinary file")
           (test:check 'ordinary-file-at-socket-path-is-preserved
             (list (test:raises? (lambda () (sys:listen-local socket)))
                   (call-with-input-file socket get-string-all)) '(#t "ordinary file")))
         (lambda ()
           (write-text edit-release "continue")
           (guard (ex [else (void)]) (stop!))
           (for-each sys:close-connection! clients)
           (for-each close-port (list input from errors))
           (for-each
             (lambda (directory)
               (for-each (lambda (name) (delete-file (string-append directory "/" name))) (directory-list directory))
               (delete-directory directory)) (list objects sources))
           (for-each (lambda (name) (delete-file (string-append root "/" name))) (directory-list root))
           (delete-directory root))))
     (test:finish! 'wire)))
