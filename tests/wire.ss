#!/usr/bin/env scheme-script

;; One protocol fixture: framing, a real headless bootstrap, concurrent
;; connections and cleanup. The daemon gets an isolated installation/config.
;; Scenario groups run in order, or only those named on the command line:
;; protocol attached resume overload bootstrap cold automatic shutdown
;; final-shutdown session recovery restart restart-accept restart-file force help.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

;; Heads answer this suite's evaluation mail; the bases and heads it starts
;; inherit the switch.
(putenv "E_TEST_EVAL" "1")

(eval
  '(begin
     (import (prefix (foundation wire) wire:) (prefix (sys sys) sys:) (prefix (test) test:)
             (prefix (fixture) fixture:)
             (prefix (foundation string) string:) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:) (prefix (foundation text) text:) (prefix (service vt) vt:))

     (define encoded wire:encode)
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
     (define base-directory (string-append root "/base λ"))
     (define socket (string-append base-directory "/socket"))
     (define test-base #f)
     (define trigger (string-append root "/continue"))
     (define producer-result (string-append root "/producer-result"))
     (define terminal-pid-file (string-append root "/terminal-pid"))
     (define inventory-file (string-append root "/sessions"))
     (define audit-file (string-append root "/audit"))
     (define edit-held (string-append root "/edit-held"))
     (define edit-release (string-append root "/edit-release"))
     (define open-held (string-append root "/open-held"))
     (define open-release (string-append root "/open-release"))
     (define automatic-control (string-append root "/automatic-control"))
     (define sync-failure (string-append root "/fail-session-sync"))
     (define sync-held (string-append sync-failure ".held"))
     ;; Every process of this installation models its disk through the control
     ;; file, including a base restoring a session before its configuration.
     (putenv "E_TEST_SYNC_CONTROL" sync-failure)
     (define lost-closing (string-append root "/lose-closing"))
     (define (quote-shell text)
       (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))
     (define (write-text path text)
       (call-with-output-file path (lambda (port) (display text port)) 'replace))
     (define (write-control! text)
       (let ([pending (string-append automatic-control ".next")])
         (write-text pending text) (rename-file pending automatic-control)))
     (define (copy-text source target) (write-text target (call-with-input-file source get-string-all)))
     (define (copy-libraries source target)
       (for-each
         (lambda (name)
           (let ([from (string-append source "/" name)] [to (string-append target "/" name)])
             (cond [(file-directory? from) (mkdir to) (copy-libraries from to)]
                   [(string:suffix? ".sls" name) (copy-text from to)])))
         (directory-list source)))
     (define (write-forms path forms)
       (call-with-output-file path (lambda (port) (for-each (lambda (form) (pretty-print form port)) forms)) 'replace))
     (define (remove-tree! path)
       (if (file-directory? path #f)
           (begin
             (for-each (lambda (name) (remove-tree! (string-append path "/" name))) (directory-list path))
             (delete-directory path))
           (delete-file path)))
     (define (loader-exit arguments)
       (let ([process (sys:open-process (append (list "env" "TERM=xterm-256color" "scheme-script"
                                                  (string-append root "/e")) arguments))])
         (dynamic-wind void
           (lambda ()
             (sys:write-process! process #f)
             (guard (ex [else
                         ;; A command that keeps running is the finding: kill it
                         ;; and report what it printed instead of a bare timeout.
                         (sys:signal-process! process 9)
                         (let ([output (get-bytevector-all (sys:process-input process))])
                           (let-values ([(code errors) (sys:process-result process)])
                             (error 'loader-exit "the command did not exit" arguments
                               (if (eof-object? output) "" (utf8->string output)) errors)))])
               (test:await 'base-cli-exits (lambda () (sys:process-status process))))
             (let ([output (get-bytevector-all (sys:process-input process))])
               (let-values ([(code errors) (sys:process-result process)])
                 (list code errors (if (eof-object? output) "" (utf8->string output))))))
           (lambda () (sys:close-process! process)))))
     (define (base-exit directory) (loader-exit (list "--base" "--base-working-dir" directory)))
     (define (fresh-session!)
       ;; Only between unrelated, fully stopped fixture scenarios.
       (let ([path (string-append base-directory "/session")])
         (when (file-exists? path) (delete-file path))))
     ;; The copy keeps modification times, and starts with this installation's
     ;; compiled objects: a base or head of the fixture recompiles only what a
     ;; scenario changes. The runner warms both runtimes first.
     (define (seed-objects! runtime)
       (when (file-directory? (string-append "eo/" runtime))
         (unless (zero? (system (format "cp -a ~a ~a" (quote-shell (string-append "eo/" runtime)) (quote-shell objects))))
           (error 'wire-test "cannot seed compiled objects" runtime))))
     (for-each (lambda (path) (mkdir path #o700)) (list root objects))
     (unless (zero? (system (format "cp -a lib ~a && cp -p e ~a" (quote-shell sources) (quote-shell root))))
       (error 'wire-test "cannot copy the installation"))
     (mkdir (string-append root "/tools"))
     (copy-text "tools/environment-worker.sps" (string-append root "/tools/environment-worker.sps"))
     (copy-text "start.e" (string-append root "/start.e"))
     (for-each seed-objects! '("base" "client"))
     (define operation-extension (string-append root "/operation-extension"))
     (for-each (lambda (part) (mkdir (string-append operation-extension part)))
       '("" "/lib" "/lib/base"))
     (copy-text "tests/operation-probe.sps" (string-append operation-extension "/lib/operation-probe.sls"))
     (write-forms (string-append operation-extension "/lib/base/operation-support.sls")
       '((library (operation-support) (export generation) (import (chezscheme)) (define generation 1))))
     ;; Every base of this installation loses closing notices through the
     ;; product's hook while the control file exists; the ordinary path runs
     ;; otherwise. The disk fault is environmental, see E_TEST_SYNC_CONTROL.
     (define fault-forms
       `((base:closing-hook
           (lambda (connection reason send!)
             (if (file-exists? ,lost-closing) (sys:close-connection! connection) (send!))))))
     (define (base-config! forms)
       (write-forms (string-append root "/base-config.e") (append fault-forms forms)))
     (write-text (string-append root "/config.e") "(error 'head-config \"daemon loaded head config\")\n")
     (base-config!
       `((define footprint
           (list (filter (lambda (name) (member name '("head" "main" "edit" "paint" "terminal" "describe")))
                         (kernel:loaded-modules))
                 (kernel:module-requires? "base" "head") (actor:current) (store:buffer-list)))
         (extension:load! ,operation-extension "operation-probe")
         (define held (policy:mint! '(agent "authority probe") (policy:reader) '(head "desk λ")))
         (define model-checks 0)
         (model:register-kind! 'wire-value 1
           (lambda (value)
             (set! model-checks (+ model-checks 1))
             (call-with-output-file ,(string-append root "/model-checks")
               (lambda (out) (write model-checks out)) 'replace)
             (string? value)))
         (define old-eval policy:session-eval!)
         (define old-edit policy:session-edit!)
         (define old-undo policy:session-undo!)
         (define old-redo policy:session-redo!)
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
                 (call-with-values (lambda () (old-redo held 999)) list)
                 (old-ask held "still active?" '() void) (policy:sessions)))
         ;; Simulate retained daemon history before any screen connects.
         (log:retention 5000)
         ,@(if (or (null? (command-line-arguments)) (member "protocol" (command-line-arguments)))
               '((do ([i 0 (+ i 1)]) ((= i 5002)) (log:add! 'wire-retained i #f))) '())
         (define notes (store:create! '(base e) "notes λ" '("hello λ")
                         (list (cons 'bootstrap footprint)
                               (cons 'process-id (get-process-id))
                               (cons 'authority (list refused-core retained revoked)))))
         (store:create! '(base e) "private" '("a local audience") '((audience (head "desk λ"))))
         (actor:register! '(agent "background")
           (lambda (message)
             (when (and (pair? message) (eq? (car message) 'ask))
               (actor:answer! (cadr message) #f))))
         (define default-policy (base:connection-policy))
         (base:connection-policy
           (lambda (actor)
             (cond [(member actor '((agent "first") (agent "second")))
                    (policy:make '(+ quote begin display let lambda make-vector) 10000 '("notes λ" "attached text") 16)]
                   [(equal? actor '(head "read only")) (policy:reader)]
                   [else (default-policy actor)])))
         (define default-owner (base:connection-owner))
         (base:connection-owner
           (lambda (actor)
             (if (member actor '((agent "first") (agent "second")))
                 '(head "screen A") (default-owner actor))))
         (define held-edit? #f)
         ;; File publication invokes subscribers before the reply crosses the
         ;; socket. Observe creation, or retarget an accepted save.
         (store:subscribe! #f
           (lambda (event)
             (when (and (eq? (car event) 'create) (string:prefix? "shared-visit-" (caddr event)))
               (let ([id (cadr event)] [who '(agent "file opening")])
                 (store:edit! who id 0 (text:make-span 0 0 0 0) '("callback "))
                 (store:set-properties! who id '((mode . "scheme") (mode-auto . #f)))
                 (call-with-output-file ,open-held (lambda (out) (display "ready" out)) 'replace)
                 (let wait ([left 1000])
                   (unless (file-exists? ,open-release)
                     (when (zero? left) (error 'wire-test "file opening barrier timed out"))
                     (sleep (make-time 'time-duration 5000000 0)) (wait (- left 1))))))
             (when (and (eq? (car event) 'create) (string:prefix? "wire-open-" (caddr event)))
               (let ([id (cadr event)] [who '(agent "opening")])
                 (store:set-properties! who id
                   (list (cons 'open-observation
                           (call-with-values (lambda () (store:snapshot-state id)) list))
                         '(mode . "scheme") '(mode-auto . #f)))
                 (store:edit! who id 0 (text:make-span 0 0 0 0) '("agent "))))
             (when (and (eq? (car event) 'property) (eq? (caddr event) 'file))
               (let* ([id (cadr event)] [target (store:property id 'save-retarget #f)])
                 (when target
                   (let ([seen (cons (store:buffer-name id)
                                 (map (lambda (key) (store:property id key))
                                   '(file base mode mode-auto read-only disposable modified)))])
                     (store:set-properties! '(base e) id
                       `((save-retarget . #f) (save-observation . ,seen)
                         (file . ,target) (base . "new baseline\n") (mode . "scheme") (mode-auto . #f)))
                     (store:rename! '(base e) id "retargeted.ss")))))))
         (define operation-audits '())
         (define app-presentations '())
         (log:subscribe!
           (lambda (record presentation)
             (when (eq? (log:component record) 'base:audit-store-event!)
               (let* ([event (log:datum record)] [app? (eq? (car (log:actor record)) 'app)]
                      [operation? (and (pair? event) (memq (car event) '(edit reset)) (equal? (cadr event) notes))])
                 (when app? (set! app-presentations (cons presentation app-presentations)))
                 (when operation? (set! operation-audits (cons (list (log:actor record) event) operation-audits)))
                 (when (or app? operation?)
                   (call-with-output-file ,audit-file
                     (lambda (out) (write (list (reverse operation-audits) app-presentations) out)) 'replace))))
             (when (eq? (log:component record) 'policy:audit!)
               (let ([event (log:datum record)])
                 (when (and (eq? (car event) 'edit) (equal? (cadr event) '(agent "first")) (not held-edit?))
                   (set! held-edit? #t)
                   (call-with-output-file ,edit-held (lambda (out) (display "committed" out)))
                   (let wait ()
                     (unless (file-exists? ,edit-release) (sleep (make-time 'time-duration 5000000 0)) (wait))))
                 (when (memq (car event) '(mint revoke))
                   (let ([inventory (policy:sessions)])
                     (call-with-output-file ,inventory-file (lambda (out) (write inventory out)) 'replace)
                     ;; Committed stop closes canonical writes before retiring
                     ;; sessions. Observe retirement even when this probe's
                     ;; optional store projection is consequently refused.
                     (store:set-property! '(base e) notes 'sessions inventory)))))))
         (actor:subscribe!
           (lambda (batch)
             (for-each (lambda (event)
                         (when (and (eq? (car event) 'attached) (memq (caadr event) '(head agent)))
                           (actor:send! (cadr event) '(from-base "welcome"))
                           (let ([name (cadadr event)])
                             (when (member name '("stalled count" "stalled bytes"))
                               (store:set-property! '(base e) '(buffer 2) 'padding (make-string (if (string=? name "stalled count") 8192 2097152) #\x)))
                             ;; Publication may itself cause mail before the
                             ;; writer starts. Refusal must reach the sender.
                             (when (equal? name "stalled mail")
                               (let send ([remaining 512] [message (make-string 8192 #\x)])
                                 (cond [(zero? remaining) (store:set-property! '(base e) notes 'mail-refused #f)]
                                   [(actor:send! (cadr event) message) (send (- remaining 1) message)]
                                   [else (store:set-property! '(base e) notes 'mail-refused #t)])))))) batch)))
         (vt:shell "/bin/sh")
         (vt:open! '(base e)
           ,(format "echo $$ > ~a; printf 'still here'; read answer" (quote-shell terminal-pid-file))
           ,root 4 30)
         (fork-thread
           (lambda ()
             (let wait ()
               (unless (file-exists? ,trigger) (sleep (make-time 'time-duration 5000000 0)) (wait)))
             (guard (ex [else (call-with-output-file ,producer-result
                                (lambda (out) (write (list 'failed (kernel:condition-text ex)) out)) 'replace)])
               (call-with-output-file ,producer-result (lambda (out) (write 'publishing out)) 'replace)
               (store:reset! '(agent "background") notes '("agent work while detached"))
               (call-with-output-file ,producer-result (lambda (out) (write 'finished out)) 'replace))))))

     (test:check 'noninteractive-head-refuses-before-base-config-starts-work
       (list (zero? (system (format "TERM=dumb scheme-script ~a > ~a 2>&1"
                                    (quote-shell (string-append root "/e"))
                                    (quote-shell (string-append root "/early-output")))))
             (file-exists? terminal-pid-file)) '(#f #f))
     (define clients '())
     (define heads '())
     (define killed-heads '())
     (define stopped? #f)
     (define last-request #f)
     (define notices (test:recorder))
     (define (receive connection)
       (guard (ex [else (error 'wire-test "receive failed" last-request (kernel:condition-text ex))])
         ;; A protocol reply can depend on keyboard input in a real head.
         ;; Keep draining every PTY while waiting, or a full terminal pipe
         ;; blocks its renderer before it can process the rest of that input.
         (let* ([done (test:gate)]
                [finish (test:worker (lambda ()
                                       (dynamic-wind void
                                         (lambda () (wire:receive (sys:connection-input connection)))
                                         (lambda () (done #t)))))])
           (test:await 'wire-receive (lambda () (for-each pump-head! heads) (done)))
           (finish))))
     (define (exchange connection message)
       (set! last-request message)
       (wire:send! (sys:connection-output connection) message)
       (receive-reply connection))
     (define (receive-reply connection)
       (let ([message (receive connection)])
         (cond [(and (pair? message) (eq? (car message) 'changed))
                (notices (cons connection (cadr message))) (receive-reply connection)]
           [(equal? message '(event (pending))) (receive-reply connection)]
           [(and (pair? message) (eq? (car message) 'models)) (receive-reply connection)]
           [else message])))
     (define (connect)
       (let ([connection (sys:connect-local socket)])
         (set! clients (cons connection clients)) connection))
     (define (hello connection actor)
       (exchange connection (list 'hello wire:version actor (fingerprint))))
     (define (fingerprint)
       (parameterize ([kernel:installation-directory root]) (kernel:fingerprint)))
     (define (reply-value reply . id)
       (unless (and (list? reply) (= (length reply) 4)
                    (equal? (list-head reply 3) (list 'reply (if (pair? id) (car id) 7) 'ok)))
         (error 'wire-test "request failed" last-request reply))
       (cadddr reply))
     (define (rpc connection operation . args)
       (reply-value (exchange connection (append (list 'request 7 operation) args))))
     (define (reject connection operation . args)
       (let ([reply (exchange connection (append (list 'request 7 operation) args))])
         (and (eq? (caddr reply) 'error) (cadddr reply))))
     (define (cancel connection review) (rpc connection 'cancel-review (cadr review)))
     (define (terminal connection)
       (rpc connection 'vt-open "printf ready; while read value; do printf '<%s>' \"$value\"; done"
         root 3 32 'dark))
     (define (inventory connection)
       (cdr (assq 'sessions (caddr (rpc connection 'snapshot '(buffer 1))))))

     ;; Drive the real editor in the daemon's installation. All heads use
     ;; the same object cache, which also exercises repeated client loading.
     (define (start-command args columns)
       (let* ([process (sys:spawn-terminal-process "/bin/sh"
                         (apply fixture:command test-base args)
                         root 24 columns)]
              [head (vector process #f (vt:make-emulator 24 columns) ""
                      (let ([named (member "--name" args)]) (and named (pair? (cdr named)) (cadr named))))])
         (vector-set! head 1
           (fixture:terminal-reader process
             (lambda (chunk)
               (vt:emulator-feed! (vector-ref head 2) chunk)
               (let ([text (string-append (vector-ref head 3) chunk)])
                 (vector-set! head 3
                   (if (> (string-length text) 32768)
                       (string:tail text (- (string-length text) 16384)) text))))))
         (set! heads (cons head heads)) head))
     (define (start-head name . width)
       (let ([head (start-command (list "--name" name) (if (pair? width) (car width) 80))])
         (vector-set! head 4 name) head))
     (define (pump-head! head)
       ((vector-ref head 1)))
     (define (head-sees? head text)
       (pump-head! head)
       (exists (lambda (line) (string:search line text 0 (string-length line)))
         (vector->list (vt:emulator-screen (vector-ref head 2)))))
     (define (head-send! head text)
       (put-bytevector (sys:terminal-process-output (vector-ref head 0)) (string->utf8 text))
       (flush-output-port (sys:terminal-process-output (vector-ref head 0))))
     (define (head-wait label head predicate . seconds)
       (guard (ex [else (error 'wire-head (format "~a" label)
                          (kernel:condition-text ex)
                          (vector->list (vt:emulator-screen (vector-ref head 2)))
                          (let ([text (vector-ref head 3)])
                            (string:tail text (max 0 (- (string-length text) 1200)))))])
         ;; Every live PTY needs a reader, even while another head is active.
         (apply test:await label (lambda () (for-each pump-head! heads) (predicate)) seconds)))
     (define evaluator #f)
     (define (evaluator-connection)
       ;; One agent carries evaluation mail for the group. It is opened while
       ;; the base admits current sources and kept until an exchange fails:
       ;; a restarted base leaves a dead one behind, and a deliberately
       ;; stale base would refuse a fresh hello.
       (unless evaluator
         (set! evaluator (fixture:evaluator root base-directory '(agent "evaluator")))
         (set! clients (cons evaluator clients)))
       evaluator)
     (define (evaluator-close!)
       ;; A live agent session is a review risk; drop it before scenarios
       ;; that expect none.
       (when evaluator
         (guard (ex [else (void)]) (sys:close-connection! evaluator))
         (set! evaluator #f)))
     (define (head-read head expression)
       ;; Runs on the head's main thread. Keys sent earlier are not ordered
       ;; before it: callers observe their effect on screen first. A base that
       ;; stopped or restarted since the last read leaves a dead connection
       ;; behind; retry once through a fresh one.
       (let ([who (list 'head (vector-ref head 4))] [first-failure #f])
         (guard (ex [else (error 'wire-head "evaluation failed" (kernel:condition-text ex)
                            (and first-failure (kernel:condition-text first-failure)) who expression
                            (vector->list (vt:emulator-screen (vector-ref head 2))))])
           (guard (ex [(or (i/o-error? ex)
                           (and (message-condition? ex)
                                (string=? (condition-message ex) "the evaluator connection closed")))
                       (set! first-failure ex)
                       (set! evaluator #f)
                       (fixture:evaluate (evaluator-connection) who (fixture:composed expression))])
             (pump-head! head)
             (fixture:evaluate (evaluator-connection) who (fixture:composed expression))))))
     (define (head-blame head)
       (head-read head
         '(let* ([b (test-document)] [id b]
                 [frame (widget:prepared (widget:descendant (test-window) (quote document)))]
                 [styles (widget:frame-cell-styles frame 0)])
            (list
              (let loop ([i 0] [start #f] [out '()])
                (let ([ink? (and (< i (vector-length styles))
                                 (memq (vector-ref styles i) '(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6)))])
                  (cond [(= i (vector-length styles)) (reverse (if start (cons (list 0 start i) out) out))]
                    [ink? (loop (+ i 1) (or start i) out)]
                    [else (loop (+ i 1) #f (if start (cons (list 0 start i) out) out))])))
              (cadar (store:blame id 1))))))
     (define (screen-state head)
       (head-read head
         '(list (let shape ([node (test-manager)])
                  (let ([d (interaction:snapshot node)])
                    (if (eq? (view:kind d) 'window) (field (view:options d) 'number)
                      (list (view:kind d) (view:state d)
                        (map (lambda (c) (shape (cadr c))) (view:children d))))))
                (test-window) (edit:copy-text)
                (map (lambda (w)
                       (let* ([b (window:document (test-manager) w)] [root (widget:descendant w 'document)]
                              [d (interaction:snapshot root)]
                              [body (and (eq? (view:kind d) 'markdown-page) (widget:descendant root 'text))]
                              [state (view:state (if body (interaction:snapshot body) d))]
                              [query (and body (view:source (interaction:snapshot body)))]
                              [source (and query (field (field (field (collection:summary query) 'value) 'details) 'document))]
                              [wrap (assq 'wrap (view:options (interaction:snapshot w)))])
                         (if source
                           (list source (store:line source (cadar state)) (cadddr (car state))
                             (store:line source (cadr (caddr state))) wrap)
                           (list b (map (lambda (p) (cons (store:line b (car p)) (cdr p))) (list-head state 3))
                             (cadddr state) wrap)))) (window:list (test-manager))))))
     (define (occurrences text part)
       (let loop ([from 0] [count 0])
         (cond [(string:search text part from (string-length text))
                => (lambda (at) (loop (+ at (string-length part)) (+ count 1)))]
           [else count])))
     (define (apply-changes lines changes)
       (fold-left
         (lambda (lines change)
           (let ([delta (text:datum->delta (caddr change))])
             (unless (equal? (text:extract lines (text:delta-span delta)) (text:delta-removed delta))
               (error 'wire-test "wrong removed text" change))
             (let-values ([(next actual) (text:apply-edit lines (text:delta-span delta) (text:delta-inserted delta))]) next)))
         lines changes))

     (define (shutdown-scenarios!)
       (fresh-session!)
       (let ([held (string-append root "/pause-held")]
             [release (string-append root "/pause-release")]
             [session (string-append base-directory "/session")])
         (write-text (string-append root "/config.e") "(void)\n")
         (base-config!
           `((base:connection-policy
               (lambda (who)
                 (if (and (eq? (car who) 'head) (not (equal? who '(head "restricted"))))
                     (policy:make 'all 100000000 'any 8000) (policy:reader))))
             (define replacement #f)
             (actor:register! '(base lifecycle)
               (lambda (message)
                 (case message
                   [(replace)
                    (when replacement (policy:revoke! replacement))
                    (set! replacement (policy:mint! '(agent "replacement") (policy:reader)))]
                   [(hold)
                    (fork-thread
                      (lambda ()
                        (activity:call-with
                          (lambda ()
                            (call-with-output-file ,held (lambda (p) (write #t p)))
                            (let wait ()
                              (unless (file-exists? ,release)
                                (sleep (make-time 'time-duration 5000000 0)) (wait)))))))])))))
         (fixture:call-with-base root base-directory
           (lambda (base)
             (set! test-base base)
             (let ([a (connect)] [b (connect)] [restricted (connect)] [agent (connect)])
               (define (phase) (cdr (assq 'phase (rpc agent 'status))))
               (for-each (lambda (connection who) (hello connection who))
                 (list a b restricted agent)
                 '((head "shutdown A") (head "shutdown B") (head "restricted") (agent "observer")))
               (let* ([clean (rpc a 'create "shared work" '("on disk"))]
                      [review (rpc a 'prepare-close)] [token (cadr review)])
                 (test:check 'review-owns-admission-but-leaves-status-and-existing-work-live
                   (list (phase)
                         (map (lambda (who)
                                (let ([new (connect)])
                                  (let ([reply (hello new who)]) (sys:close-connection! new) reply)))
                           '((head "busy head") (agent "busy agent")))
                         (and (reject b 'prepare-close) (reject b 'shutdown token)
                              (reject restricted 'prepare-close) (reject agent 'prepare-close)
                              (reject agent 'leaving #t) #t))
                   '(reviewing ((error #f (busy reviewing)) (error #f (busy reviewing))) #t))
                 (let ([next (rpc a 'prepare-close)])
                   (test:check 'replacement-tokens-cannot-be-used-to-stop-or-cancel
                     (list (not (= token (cadr next))) (and (reject a 'shutdown token) #t)
                           (and (reject a 'cancel-review token) #t) (phase)) '(#t #t #t reviewing))
                   (cancel a next))
                 (test:check 'restricted-head-detaches-even-with-shutdown-preference
                   (eq? (car (rpc restricted 'leaving #t)) 'last) #f)
                 (sys:close-connection! restricted)
                 (rpc a 'edit clean 0 '(0 0 0 0) '("new "))
                 (let ([abandoned (connect)])
                   (hello abandoned '(head "abandoned review"))
                   (rpc abandoned 'prepare-close)
                   (sys:close-connection! abandoned)
                   (test:await 'disconnect-releases-review (lambda () (eq? (phase) 'running))))
                 ;; A bounded pause failure must release ownership as well as
                 ;; reopen writes. The fixture holds an admitted callback.
                 (rpc a 'send '(base lifecycle) 'hold)
                 (test:await 'held-producer (lambda () (file-exists? held)))
                 (dynamic-wind void
                   (lambda ()
                     (let* ([review (rpc a 'prepare-close)]
                            [error (reject a 'shutdown (cadr review))])
                       (test:check 'quiescence-timeout-reopens-the-base
                         (list (and error (> (occurrences error "timed out") 0)) (phase)) '(#t running)))
                     (let ([abandoned (connect)])
                       (hello abandoned '(head "disconnect while pausing"))
                       (let ([review (rpc abandoned 'prepare-close)])
                         (wire:send! (sys:connection-output abandoned) (list 'request 7 'shutdown (cadr review))))
                       (test:await 'acceptance-is-pausing (lambda () (eq? (phase) 'paused)))
                       (test:check 'competing-review-refuses-during-pause
                         (and (reject b 'prepare-close) #t) #t)
                       (sys:close-connection! abandoned)
                       (write-text release "continue")
                       (test:await 'lost-requester-cancels-before-acceptance (lambda () (eq? (phase) 'running)))
                       (test:check 'disconnect-before-acceptance-keeps-work (car (rpc a 'snapshot clean)) '#("new on disk"))))
                   (lambda () (write-text release "continue")))
                 (let ([term (terminal a)])
                   (rpc a 'send '(base lifecycle) 'replace)
                   (for-each
                     (lambda (kind)
                       (let* ([review (rpc a 'prepare-close)]
                              [next
                               (begin
                                 (case kind
                                   [(terminal)
                                    (rpc a 'vt-close term)
                                    (test:await 'old-terminal-ended
                                      (lambda () (not (cdr (assq 'alive (caddr (rpc a 'snapshot term)))))))
                                    (set! term (terminal a))]
                                   [(agent) (rpc a 'send '(base lifecycle) 'replace)])
                                 (rpc a 'shutdown (cadr review)))])
                         (test:check (list 'same-count-replacement-needs-review kind)
                           (list (car next) (cdr (assq (if (eq? kind 'terminal) 'terminals 'agents) (cadddr next))))
                           (list 'review (cdr (assq (if (eq? kind 'terminal) 'terminals 'agents) (cadddr review)))))
                         (cancel a next))) '(terminal agent))
                   (let* ([leavers (list a b)]
                          [answers (test:parallel 2 (lambda (i) (rpc (list-ref leavers i) 'leaving #t)))]
                          [last (if (eq? (caar answers) 'last) 0 1)])
                     (test:check 'simultaneous-shutdown-quitters-have-exactly-one-last-head
                       (list (length (filter (lambda (answer) (eq? (car answer) 'last)) answers))
                             (cdr (assq 'heads (rpc agent 'status)))) '(1 1))
                     (set! a (list-ref leavers last))
                     (sys:close-connection! (list-ref leavers (- 1 last)))
                     (cancel a (rpc a 'prepare-close))
                     (test:check 'cancelled-last-head-remains-present
                       (list (phase) (cdr (assq 'heads (rpc agent 'status)))) '(running 1)))
                   (let ([ui (start-head "shutdown UI")])
                     (head-wait 'shutdown-ui-ready ui (lambda () (head-sees? ui "*scratch*")))
                     (head-read ui
                       '(begin (test-private! "<local shutdown work>" "local draft") #t))
                     (for-each
                       (lambda (key)
                         (head-send! ui "\x1b;xlifecycle:shutdown!\r")
                         (head-wait 'shutdown-review-question ui (lambda () (head-sees? ui "Stop the base?")))
                         (head-send! ui key)
                         (test:await 'ui-cancel-releases-review (lambda () (eq? (phase) 'running)))
                         (test:check (list 'shutdown-cancellation key)
                           (head-read ui '(list (head:quitting?)
                                            (store:line (store:find-named "<local shutdown work>") 0)))
                           '(#f "local draft"))) '("n" "v" "\x1b;" "\x07;"))
                     (head-read ui '(begin (lifecycle:shutdown-on-exit #t)
                                           (void) #t))
                     (rpc a 'leaving #f) (sys:close-connection! a)
                     (head-send! ui "\x18;\x03;")
                     (head-wait 'last-head-reviews-before-exit ui (lambda () (head-sees? ui "Stop the base?")))
                     (head-send! ui "n")
                     (test:await 'cancelled-last-head-keeps-editing (lambda () (eq? (phase) 'running)))
                     (test:check 'last-head-cancellation-retains-head-and-preference
                       (head-read ui '(list (head:quitting?) (lifecycle:shutdown-on-exit))) '(#f #t))
                     (head-send! ui "\x18;\x03;")
                     (head-wait 'last-head-acceptance-question ui (lambda () (head-sees? ui "Stop the base?")))
                     (head-send! ui "y")
                     (head-wait 'reviewed-shutdown-announced ui (lambda () (head-sees? ui "e: the base shut down")))
                     (test:await 'accepted-base-exits (lambda () (sys:process-status (fixture:process base))))
                     (test:check 'graceful-shutdown-saves-session-and-restores-terminal
                       (list (file-exists? session) (receive agent)
                             (map (lambda (key) (cdr (assq key (vt:emulator-state (vector-ref ui 2)))))
                               '(mouse-tracking sgr-mouse))) '(#t (closing shutdown) (#f #f)))))))))))

     (define (final-shutdown-scenarios!)
       ;; A plain base: the clean mode's review must find only shared text.
       (base-config! '())
       (for-each
         (lambda (mode)
           (fresh-session!)
           (fixture:call-with-base root base-directory
             (lambda (base)
               (set! test-base base)
               (if (eq? mode 'clean)
                   (let ([ui (start-head "clean shutdown")])
                     (head-wait 'clean-head-ready ui (lambda () (head-sees? ui "*scratch*")))
                     (head-read ui '(begin (edit:insert! (test-editor) "shared unsaved work") #t))
                     (evaluator-close!)
                     (head-send! ui "\x1b;xlifecycle:shutdown!\r")
                     (head-wait 'clean-shutdown-needs-no-question ui (lambda () (head-sees? ui "e: the base shut down")))
                     (test:check 'shared-unsaved-work-is-saved-without-a-question
                       (list (occurrences (vector-ref ui 3) "Stop the base?")
                             (map (lambda (state) (list-ref state 3))
                               (cdr (list-ref (call-with-input-file (string-append base-directory "/session") read) 4))))
                       '(0 (#("shared unsaved work")))))
                   (let ([owner (connect)] [observer (connect)]
                         [session (string-append base-directory "/session")])
                     (hello owner '(head "durable owner"))
                     (hello observer '(agent "durable observer"))
                     (write-text session "old session") (chmod session #o600)
                     (when (file-exists? sync-held) (delete-file sync-held))
                     (write-text sync-failure (if (eq? mode 'accepted) "hold" "hold-fail"))
                     (dynamic-wind void
                       (lambda ()
                         (let ([review (rpc owner 'prepare-close)])
                           (wire:send! (sys:connection-output owner) (list 'request 7 'shutdown (cadr review))))
                         (test:await 'durable-step-entered (lambda () (file-exists? sync-held)))
                         (let ([late (connect)])
                           (test:check (list 'acceptance-pauses-before-stop mode)
                             (list (hello late '(head "during durable step"))
                                   (cdr (assq 'phase (rpc observer 'status)))
                                   (file-exists? session) (sys:process-status (fixture:process base)))
                             '((error #f (busy paused)) paused #t #f))
                           (sys:close-connection! late))
                         (sys:close-connection! owner))
                       (lambda () (when (file-exists? sync-failure) (delete-file sync-failure))))
                     (if (eq? mode 'accepted)
                         (begin
                           (test:await 'disconnected-acceptance-completes (lambda () (sys:process-status (fixture:process base))))
                           (test:check 'base-completes-after-requester-disconnects (receive observer) '(closing shutdown)))
                         (begin
                           (test:await 'disconnected-durable-failure-resumes
                             (lambda () (eq? (cdr (assq 'phase (rpc observer 'status))) 'running)))
                           (let ([next (connect)])
                             (test:check 'failed-durable-operation-reopens-admission-without-owner
                               (car (hello next '(head "after failed durable step"))) 'hello)
                             (sys:close-connection! next)))))))))
         '(clean accepted failed)))

     (define (session-scenarios!)
       (fresh-session!)
       (let ([path (string-append base-directory "/session")]
             [temporary (string-append base-directory "/session.tmp")]
             [disk (string-append root "/persistent-file")]
             [initialized (string-append root "/restored-before-config")]
             [model-state (string-append root "/restored-models")]
             [expected-models #f]
             [saved #f] [note #f] [file #f] [term #f] [ended #f] [omitted #f] [gap #f]
             [checkpoint #f] [expected #f] [expected-history '()])
         (write-text (string-append root "/config.e") "(void)\n")
         (base-config!
           `((vt:shell "/bin/sh")
             ;; First startup defines both schemas. Later startups retain
             ;; one unknown schema and a known definition that cannot adopt
             ;; its payload; neither may block maintenance or the next save.
             (if (null? (model:ids))
                 (begin
                   (model:register-kind! 'session-model-fixture 1 string?)
                   (model:register-kind! 'session-model-fixture 2 string?)
                   (model:create! '(base fixture) 'session-model-fixture 1 '(head "kept desk") 'persistent '((buffer 999)) "kept")
                   (model:create! '(base fixture) 'session-model-fixture 2 'session 'persistent '((model 1)) "future")
                   (model:create! '(base fixture) 'session-model-fixture 2 'session 'transient '() "ephemeral"))
                 (model:register-kind! 'session-model-fixture 1 (lambda (payload) #f)))
             (call-with-output-file ,model-state
               (lambda (port)
                 (write (list (call-with-values model:export list) (map model:available? (model:ids))) port)) 'replace)
             (define default-policy (base:connection-policy))
             (base:connection-policy
               (lambda (who) (if (equal? who '(head "restricted")) (policy:reader) (default-policy who))))
             (define replacement #f)
             (actor:register! '(base session-fixture)
               (lambda (message)
                 (when replacement (policy:revoke! replacement))
                 (set! replacement (policy:mint! '(agent "replacement") (policy:reader)))))
             (call-with-output-file ,initialized
               (lambda (port)
                 (write (list (store:buffer-list)
                          (store:create! '(base e) "configured" '("") '((disposable . #t)))) port)) 'replace)))
         (fixture:call-with-base root base-directory
           (lambda (base)
             (set! test-base base)
             (let ([head (connect)] [control (connect)] [agent (connect)])
               (hello head '(head "kept desk"))
               (hello agent '(agent "pending actor"))
               (test:check 'maintenance-neither-claims-names-nor-adds-participants
                 (let ([hello (exchange control '(maintenance 1 (head "kept desk")))])
                   (list (list-head hello 2) (cdr (assq 'heads (caddr hello)))
                         (length (rpc head 'actors)) (length (rpc head 'sessions))))
                 '((maintenance 1) 1 3 2))
               (test:check 'maintenance-permission-and-operation-boundaries
                 (list (map (lambda (identity)
                              (let ([connection (connect)])
                                (car (exchange connection (list 'maintenance 1 identity)))))
                         '((agent "refused") (head "restricted")))
                       (and (reject control 'create "forbidden" '("")) #t)
                       (and (reject head 'prepare-restart) #t))
                 '((error error) #t #t))
               (set! note (rpc head 'create "persistent notes" '("initial")))
               (write-text disk "disk baseline\n")
               (set! file (rpc head 'create "persistent file" '("disk baseline")
                            `((file . ,disk) (base . "disk baseline\n") (stamp 10 . 9) (trailing . #t) (mode . "scheme"))))
               (set! omitted (rpc head 'create "generated result" '("temporary") '((disposable . #t))))
               (set! term (terminal head))
               (set! ended (terminal head))
               (rpc head 'vt-close ended)
               (test:await 'ended-source-ready
                 (lambda () (not (cdr (assq 'alive (caddr (rpc head 'snapshot ended)))))))
               (set! gap (rpc head 'create "deleted before save" '("")))
               (rpc head 'delete gap)
               (let ([review (rpc control 'prepare-restart)] [extra (connect)])
                 (test:check 'restart-review-refuses-new-screen-admission
                   (hello extra '(head "new participant")) '(error #f (busy reviewing)))
                 (cancel control review))
               (rpc head 'send '(base session-fixture) 'replace)
               (for-each
                 (lambda (kind)
                   (let ([review (rpc control 'prepare-restart)])
                     (case kind
                       [(terminal) (rpc head 'vt-close term) (set! term (terminal head))]
                       [(agent) (rpc head 'send '(base session-fixture) 'replace)]
                       [(pending) (rpc agent 'ask '(head "kept desk") "Omitted on restart?" '()) (receive head)])
                     (let ([next (rpc control 'restart (cadr review))])
                       (test:check (list 'restart-reviews-new-incarnations kind)
                         (list (car next) (not (= (cadr next) (cadr review)))
                               (and (reject control 'cancel-review (cadr review)) #t)
                               (cdr (assq 'phase (rpc control 'status)))) '(review #t #t reviewing))
                       (cancel control next)))) '(terminal agent pending))
               (let ([review (rpc control 'prepare-restart)])
                 (sys:close-connection! control)
                 (test:await 'maintenance-disconnect-releases-review
                   (lambda () (eq? (cdr (assq 'phase (rpc head 'status))) 'running)))
                 (set! control (connect)) (exchange control '(maintenance 1 (head "kept desk"))))
               ;; Both reviewed entry points use the same save. Check both
               ;; sides of rename while keeping the same live PTY.
               (write-forms path '((session 1 7 1 (buffers) (checkpoints)))) (chmod path #o600)
               (for-each
                 (lambda (scenario)
                   (let* ([operation (car scenario)] [failure (cadr scenario)]
                          [connection (if (eq? operation 'restart) control head)]
                          [previous (call-with-input-file path get-string-all)])
                     (if (eq? failure 'temporary) (mkdir temporary #o700) (write-text sync-failure "fail"))
                     (dynamic-wind void
                       (lambda ()
                         (let* ([review (rpc connection (if (eq? operation 'restart) 'prepare-restart 'prepare-close))]
                                [error (reject connection operation (cadr review))]
                                [status (rpc control 'status)])
                           (test:check (cons 'failed-save-preserves-service scenario)
                             (list (and error #t) (cdr (assq 'phase status))
                                   (equal? previous (call-with-input-file path get-string-all))
                                   (cdr (assq 'session-uncertain? status))
                                   (cdr (assq 'alive (caddr (rpc head 'snapshot term))))
                                   (if (eq? failure 'sync) (> (occurrences error "new session is installed") 0) #t))
                             (list #t 'running (eq? failure 'temporary) (eq? failure 'sync) #t #t))))
                       (lambda ()
                         (if (eq? failure 'temporary) (delete-directory temporary) (delete-file sync-failure))))))
                 '((restart temporary) (shutdown sync)))
               ;; A signal steals a pending review; later signals in the held
               ;; save coalesce even when that save ultimately fails.
               (let ([review (rpc control 'prepare-restart)])
                 (when (file-exists? sync-held) (delete-file sync-held))
                 (write-text sync-failure "hold-fail-log")
                 (dynamic-wind void
                   (lambda ()
                     (sys:signal-process! (fixture:process base) 15)
                     (test:await 'signal-save-paused (lambda () (file-exists? sync-held)))
                     (let ([observer (connect)])
                       (test:check 'maintenance-status-stays-readable-during-save
                         (cdr (assq 'phase (caddr (exchange observer '(maintenance 1 (head "save status")))))) 'paused))
                     (for-each (lambda (signal) (sys:signal-process! (fixture:process base) signal)) '(2 15 2))
                     (sleep (make-time 'time-duration 30000000 0)))
                   (lambda () (delete-file sync-failure)))
                 (test:await 'signal-save-failure-resumes
                   (lambda () (eq? (cdr (assq 'phase (rpc control 'status))) 'running)))
                 (test:check 'signal-failure-releases-review-and-reports-durability
                   (list (and (reject control 'restart (cadr review)) #t)
                         (> (occurrences (fixture:diagnostics base) "durability is uncertain") 0)
                         (sys:process-status (fixture:process base))) '(#t #t #f)))
               (let* ([view (rpc head 'view-create term 'terminal 1 '() '(partial #t))]
                      [generation (begin (rpc head 'view-claim view) (cdr (assq 'generation (rpc head 'view-read view))))])
                 (rpc head 'send (cdr (assq 'app (caddr (rpc head 'snapshot term))))
                   (list 'input '(head "kept desk") term "TEXT"
                     (list (cons 'view (list view generation)) '(size 3 32) '(text . "recovered\n"))))
                 (test:await 'terminal-resumes-after-save-failures
                   (lambda () (exists (lambda (line) (> (occurrences line "<recovered>") 0))
                                (vector->list (car (rpc head 'snapshot term))))))
                 (rpc head 'view-release view generation)
                 (let* ([binding (cadr (rpc head 'invoke 'composition:acquire! '((string) (list list)) '("session")))]
                        [binding-id (cdr (assq 'id binding))]
                        [admitted (rpc head 'invoke 'composition:admit!
                                    '((list (or model #f) list (or model #f (one-of retire))) (symbol datum list))
                                    (list binding view (rpc head 'view-tree view) #f))]
                        [lease (cdr (assq 'generation (cdar (list-ref admitted 3))))])
                   (rpc head 'view-release view lease)
                   (let ([initial (call-with-input-file model-state (lambda (port) (car (read port))))]
                         [packet (rpc head 'model-read (list view binding-id))])
                     (set! expected-models (list (+ (cadr binding-id) 1)
                                             (append (cadr initial) (map caddr (cadr packet))))))))
               (let ([review (rpc control 'prepare-restart)])
                 ;; Shared edits and checkpoint updates during review are
                 ;; saved at acceptance and do not require another question.
                 (do ([n 0 (+ n 1)]) ((= n 3))
                   (rpc head 'edit note (cadr (rpc head 'snapshot note)) '(0 0 0 0) '("latest ")))
                 (set! checkpoint `(future-view (,note 1) (,gap missing) (,omitted unsupported)))
                 (rpc head 'checkpoint checkpoint)
                 (set! expected (rpc head 'snapshot note))
                 (set! expected-history (rpc head 'history note))
                 (test:check 'restart-saves-latest-shared-edits-without-reprompt
                   (exchange control (list 'request 7 'restart (cadr review))) '(closing restart)))
               (test:await 'saved-base-exits (lambda () (sys:process-status (fixture:process base))))
               (set! saved (call-with-input-file path read))
               (test:check 'snapshot-has-private-mode-and-no-temporary-file
                 (list (get-mode path) (file-exists? temporary) (list-head saved 2)) '(#o600 #f (session 3)))
               (test:check 'session-saves-persistent-models-and-transient-allocation-gaps
                 (list (list-ref saved 6) (car expected-models) (length (cadr expected-models)))
                 (list (cons* 'models (car expected-models) (cadr expected-models)) 6 4)))))
         (write-text disk "changed while stopped\n")
         (let ([base (fixture:start! root base-directory)])
           (dynamic-wind void
             (lambda ()
               (set! test-base base)
               (let* ([head (connect)] [states (cdr (list-ref saved 4))]
                      [before (call-with-input-file initialized read)])
                 (hello head '(head "kept desk"))
                 (test:check 'session-restores-opaque-models-before-config-and-disables-unsupported-actions
                   (call-with-input-file model-state read) (list expected-models '(#f #f #t #t)))
                 (let* ([acquired (rpc head 'invoke 'composition:acquire! '((string) (list list)) '("session"))]
                        [root (cdr (assq 'root (cdr (assq 'value (cadr acquired)))))]
                        [saved-view (find (lambda (r) (eq? (cdr (assq 'kind r)) 'widget-view)) (cadr expected-models))])
                   (test:check 'composition-reclaims-saved-root-after-process-recovery
                     (list root (cdr (assq 'owner (cdar (caddr acquired))))
                       (cdr (assq 'generation (cdar (caddr acquired)))))
                     (list (cdr (assq 'id saved-view)) '(head "kept desk")
                       (+ 1 (cdr (assq 'generation (cdr (assq 'value saved-view))))))))
                 (test:check 'restore-precedes-configuration-and-preserves-allocator-gaps
                   (list (list-sort (lambda (a b) (< (cadr a) (cadr b))) (car before)) (cadr before)
                     (assoc omitted states) (assoc gap states) (rpc head 'checkpoint))
                   (list (map car states) (list 'buffer (cadddr saved)) #f #f checkpoint))
                 (test:check 'restore-preserves-text-revisions-baselines-and-edit-time
                   (list (rpc head 'snapshot note)
                     (let ([facts (caddr (rpc head 'snapshot file))])
                       (map (lambda (key) (cdr (assq key facts))) '(base stamp modified)))
                     (rpc head 'history note) (rpc head 'read-marks note))
                   (list expected '("disk baseline\n" (10 . 9) #f) expected-history '()))
                 ;; a transcript's rows are as wide as the terminal was: its wrap fact,
                 ;; #f, is kept, or a head would soft-wrap every full row by a character;
                 ;; its mode stays too, as when a process ends in a session, while the
                 ;; app, alive and disposable facts go
                 (test:check 'live-and-ended-terminals-restore-as-read-only-unwrapped-transcripts-in-terminal-mode
                   (map (lambda (id)
                          (let ([facts (caddr (rpc head 'snapshot id))])
                            (list (car (rpc head 'snapshot id)) (cdr (assq 'read-only facts)) (assq 'wrap facts)
                              (filter (lambda (entry) (memq (car entry) '(app mode alive disposable))) facts))))
                     (list term ended))
                   (map (lambda (id) (list (list-ref (assoc id states) 3) #t '(wrap . #f) '((mode . "terminal")))) (list term ended)))
                 (let* ([revision (cadr expected)] [notice (rpc head 'startup-notice)])
                   (rpc head 'edit note revision '(0 0 0 0) '("new base "))
                   ;; The restored log keeps the deltas: a basis from before the
                   ;; stop still rebases, through the saved edit and the new one.
                   (test:check 'restored-revisions-keep-their-deltas
                     (list (length (cadddr (rpc head 'snapshot note (- revision 1))))
                       (length (cadddr (rpc head 'snapshot note revision)))
                       (> (occurrences notice "restored a session saved") 0)
                       (rpc head 'startup-notice) (cdr (assq 'saved-at (rpc head 'status)))
                       (equal? saved (call-with-input-file path read)))
                     (list 2 1 #t #f (caddr saved) #t)))
                 (test:check 'restore-does-not-overwrite-externally-changed-file
                   (call-with-input-file disk get-string-all) "changed while stopped\n")
                 ;; A crash after import leaves the previous snapshot intact.
                 (sys:signal-process! (fixture:process base) 9)
                 (test:await 'crash-after-import (lambda () (sys:process-status (fixture:process base))))
                 (test:check 'crash-keeps-last-recovery-snapshot (call-with-input-file path read) saved)))
             (lambda () (sys:close-process! (fixture:process base)))))
         ;; Recover the pre-crash state, then alternate ordinary shutdown
         ;; and OS signals through the same workspace. Every cycle adds
         ;; new text and a view, and consumes another disposable buffer id.
         (for-each
           (lambda (mode cycle)
             (fixture:call-with-base root base-directory
               (lambda (base)
                 (set! test-base base)
                 (let ([head (connect)] [before (call-with-input-file initialized read)])
                   (hello head '(head "kept desk"))
                   (test:check (list 'repeated-stops-restore-latest-state mode cycle)
                     (list (rpc head 'snapshot note) (rpc head 'checkpoint)
                           (list-sort (lambda (a b) (< (cadr a) (cadr b))) (car before)) (cadr before)
                           (rpc head 'history note)
                           (map (lambda (id) (car (rpc head 'snapshot id))) (list term ended)))
                     (list expected checkpoint (map car (cdr (list-ref saved 4))) (list 'buffer (cadddr saved)) expected-history
                           (map (lambda (id) (list-ref (assoc id (cdr (list-ref saved 4))) 3)) (list term ended))))
                   (let ([review (and (eq? mode 'shutdown) (rpc head 'prepare-close))])
                     ;; Shared edits during a shutdown review need no new
                     ;; consent, just as with the maintenance restart above.
                     (rpc head 'edit note (cadr expected) '(0 0 0 0) (list (format "cycle ~a " cycle)))
                     (set! checkpoint `(future-view (,note ,cycle) (,gap missing) (,omitted unsupported)))
                     (rpc head 'checkpoint checkpoint)
                     (set! expected (rpc head 'snapshot note))
                     (set! expected-history (rpc head 'history note))
                     (test:check (list 'repeated-stops-save-without-reprompt mode cycle)
                       (if review
                           (exchange head (list 'request 7 'shutdown (cadr review)))
                           (begin (sys:signal-process! (fixture:process base) mode) (receive head)))
                       (list 'closing (if review 'shutdown 'signal))))
                   (test:await 'repeated-save-exits (lambda () (sys:process-status (fixture:process base))))
                   (set! saved (call-with-input-file path read))
                   (test:check (list 'repeated-stops-replace-session mode cycle)
                     (list (list-ref (assoc note (cdr (list-ref saved 4))) 3)
                           (cadr (assoc "kept desk" (cdr (list-ref saved 5))))
                           (list-ref saved 6)
                           (call-with-input-file model-state read)
                           (get-mode path) (file-exists? temporary))
                     (list (car expected) checkpoint
                           (cons* 'models (car expected-models) (cadr expected-models))
                           (list expected-models '(#f #f #t #t)) #o600 #f))))))
           '(shutdown 15 shutdown 2 shutdown 15) (iota 6))))

     (define (recovery-scenarios!)
       (fresh-session!)
       (let* ([path (string-append base-directory "/session")]
              [initialized (string-append root "/recovery-initialized")]
              [bad '("" "(" "#0=(a . #0#)" "(session 99 0 1 (buffers) (checkpoints))"
                     "(session 1 0 1 (buffers) (checkpoints)) extra"
                     "(session 2 0 2 (buffers (1 0 \"valid prefix\" #(\"text\") ())) (checkpoints) (models 0))"
                     "(session 1 0 3 (buffers (1 7 \"valid first\" #(\"text\") ()) (1 0 \"duplicate id\" #(\"\") ())) (checkpoints))"
                     "(session 1 0 1 (buffers) (checkpoints (\"desk\" opaque) (\"desk\" another)))")]
              [retained '()])
         (define (archive-paths)
           (map (lambda (name) (string-append base-directory "/" name))
             (list-sort string<? (filter (lambda (name) (string:prefix? "session.incompatible" name))
                                   (directory-list base-directory)))))
         (define (stop-reviewed head)
           (let ([review (rpc head 'prepare-close)])
             (exchange head (list 'request 7 'shutdown (cadr review)))))
         (base-config!
           `((call-with-output-file ,initialized (lambda (port) (write (store:buffer-list) port)) 'replace)))
         (for-each
           (lambda (text)
             (write-text path text) (chmod path #o600)
             (fixture:call-with-base root base-directory
               (lambda (base)
                 (set! test-base base)
                 (let* ([head (connect)] [paths (archive-paths)])
                   (hello head '(head "recovery"))
                   (set! retained (append retained (list (if (string=? text "") (eof-object) text))))
                   (test:check (list 'invalid-snapshot-is-preserved-before-empty-start (length retained))
                     (list (rpc head 'buffers) (call-with-input-file initialized read)
                           (map (lambda (path) (call-with-input-file path get-string-all)) paths)
                           (cdr (assq 'recovery-archives (rpc head 'status)))
                           (let ([notice (rpc head 'startup-notice)])
                             (list (occurrences notice "could not be restored")
                                   (> (occurrences notice (car (last-pair paths))) 0)
                                   (occurrences notice "older recovery archive")))
                           (file-exists? path))
                     (list '() '() retained paths (list 1 #t (if (> (length retained) 1) 1 0)) #f))
                   (stop-reviewed head))))) bad)
         ;; Successfully read bad data may be archived. An unreadable file,
         ;; unsafe permissions or later configuration failure must stay put.
         (write-forms path '((session 1 0 1 (buffers) (checkpoints))))
         (let ([original (call-with-input-file path get-string-all)] [paths (archive-paths)])
           (test:check 'read-and-startup-errors-preserve-session-evidence
             (map (lambda (mode)
                    (when (file-exists? initialized) (delete-file initialized))
                    (chmod path mode)
                    (let ([code (car (base-exit base-directory))])
                      (chmod path #o600)
                      (list code (call-with-input-file path get-string-all)
                            (archive-paths) (file-exists? initialized)))) '(0 #o644))
             (make-list 2 (list 1 original paths #f)))
           (base-config! '((error 'fixture "failed after import")))
           (test:check 'failed-start-after-import-keeps-the-snapshot
             (list (car (base-exit base-directory)) (call-with-input-file path get-string-all) (archive-paths))
             (list 1 original paths)))
         (base-config!
           `((call-with-output-file ,initialized (lambda (port) (write (store:buffer-list) port)) 'replace)))
         (when (file-exists? initialized) (delete-file initialized))
         (write-text path "(malformed saved session)")
         (chmod path #o600)
         (write-text sync-failure "fail")
         (dynamic-wind void
           (lambda ()
             (test:check 'uncertain-archive-fails-startup-with-evidence-in-place
               (list (car (base-exit base-directory)) (file-exists? path) (file-exists? initialized)
                     (call-with-input-file (car (last-pair (archive-paths))) get-string-all))
               '(1 #f #f "(malformed saved session)")))
           (lambda () (delete-file sync-failure)))
         (fixture:call-with-base root base-directory
           (lambda (base)
             (set! test-base base)
             (let ([head (connect)])
               (hello head '(head "after failed start"))
               (let ([control (connect)])
                 (exchange control '(maintenance 1 (head "after failed start")))
                 (write-text sync-failure "fail")
                 (dynamic-wind void
                   (lambda ()
                     (let ([review (rpc control 'prepare-restart)]) (reject control 'restart (cadr review))))
                   (lambda () (delete-file sync-failure)))
                 (let ([status (rpc control 'status)] [notice (rpc head 'startup-notice)])
                   (test:check 'a-later-save-does-not-invent-a-restore-notice
                     (list (number? (cdr (assq 'saved-at status))) (cdr (assq 'restored-at status))
                           (occurrences notice "restored a session saved")
                           (occurrences notice "could not be restored")
                           (occurrences notice (format "~a older recovery archives" (length (archive-paths)))))
                     '(#t #f 0 0 1))))
               (test:check 'retained-archives-are-rediscovered-after-failed-startup
                 (list (cdr (assq 'recovery-archives (rpc head 'status)))
                       (rpc head 'startup-notice)
                       (stop-reviewed head) (length (archive-paths)))
                 (list (archive-paths) #f '(closing shutdown) (+ 1 (length bad)))))))
         ;; Reuse the final restore process for both binding-schema migration
         ;; and interrupted composition/window output disposal. Buffers 3 and
         ;; 5 were already deleted before the interrupted cleanup completed.
         (write-forms path
           (list
             `(session 3 0 6 (buffers ((buffer 1) 0 "owned output" #("discard") ())
                                      ((buffer 2) 0 "borrowed text" #("keep") ())
                                      ((buffer 4) 0 "window output" #("discard") ())) (checkpoints)
                (models 5
                  ,(map cons '(id kind schema scope persistence revision actor references value)
                     '((model 1) composition-binding 1 (head "restored with archives") persistent 4 (base composition) ()
                       ((profile . "legacy") (attachment . #f) (initialized? . #t) (root . #f))))
                  ,(map cons '(id kind schema scope persistence revision actor references value)
                     '((model 2) composition-binding 2 (head "restored with archives") persistent 4 (base composition) ((buffer 1) (buffer 3))
                       ((profile . "pending") (attachment . #f) (initialized? . #t) (root . #f) (cleanup (buffer 1) (buffer 3)))))
                  ,(map cons '(id kind schema scope persistence revision actor references value)
                     (list '(model 3) 'widget-view 3 'session 'persistent 1 '(head "restored with archives")
                       '((model 4) (buffer 4) (buffer 5))
                       (descriptor:with
                         (descriptor:make #f 'window-manager 2
                           '((head head "restored with archives") (selected model 4) (links) (cleanup (buffer 4) (buffer 5))) '())
                         '((children (layout (model 4) (grow 1)))))))
                  ,(map cons '(id kind schema scope persistence revision actor references value)
                     (list '(model 4) 'widget-view 3 '(model 3) 'persistent 0 '(head "restored with archives") '()
                       (descriptor:with (descriptor:make #f 'window 1 '((number . 1)) '()) '((parent model 3)))))))))
         (chmod path #o600)
         (fixture:call-with-base root base-directory
           (lambda (base)
             (set! test-base base)
             (let ([head (connect)])
               (hello head '(head "restored with archives"))
               (let* ([packet (rpc head 'model-read '((model 1) (model 2) (model 3)))]
                      [rows (map caddr (cadr packet))])
                 (test:check 'recovery-upgrades-bindings-and-finishes-composition-and-window-disposal
                   (list (rpc head 'buffers) (car (rpc head 'snapshot '(buffer 2)))
                     (map (lambda (r) (cdr (assq 'schema r))) rows)
                     (map (lambda (r) (cdr (assq 'cleanup (cdr (assq 'value r))))) (list-head rows 2))
                     (descriptor:cleanup (cdr (assq 'value (caddr rows))))
                     (map (lambda (r) (cdr (assq 'references r))) rows))
                   '(((buffer 2)) #("keep") (2 2 3) (() ()) () (() () ((model 4))))))
               (let ([notice (rpc head 'startup-notice)])
                 (test:check 'successful-restore-distinguishes-retained-archives-from-a-new-failure
                   (list (occurrences notice "restored a session saved")
                         (occurrences notice "could not be restored")
                         (occurrences notice (format "~a older recovery archives remain in ~a"
                                               (+ 1 (length bad)) base-directory))
                         (rpc head 'startup-notice)
                         (cdr (assq 'recovery-archives (rpc head 'status))))
                   (list 1 0 1 #f (archive-paths))))
               (stop-reviewed head))))))

     (define (help-scenarios!)
       (let* ([missing (string-append root "/help-unused")]
              [result (loader-exit (list "--help" "--restart" "--force" "--base-working-dir" missing))])
         (test:check 'help-without-a-base-does-not-start-or-create-one
           (list (car result) (occurrences (caddr result) (format "Base: no base is listening.\nBase directory: ~s\n" missing))
                 (file-exists? missing)) '(0 1 #f)))
       (fresh-session!)
       (base-config! '())
       (let* ([seat:default-directory (string-append root "/.base")]
              [path (string-append sources "/head/head.sls")]
              [source (call-with-input-file path get-string-all)]
              [base (fixture:start! root base-directory)]
              [head (connect)] [detached (connect)])
         (set! test-base base)
         (dynamic-wind void
           (lambda ()
             (hello head '(head "help inspector"))
             (hello detached '(head "saved α's desk"))
             (rpc detached 'checkpoint '(opaque "keep my screen"))
             (rpc detached 'leaving #f)
             (sys:close-connection! detached)
             (let ([id (rpc head 'create "help work" '("keep this text") '())])
               ;; Help must reach this mismatched base without head imports,
               ;; even with restart flags and an already attached --name.
               (write-text path "this is deliberately not a library\n")
               (test:check 'help-reads-presence-without-claiming-a-head-or-changing-the-review
                 (map
                   (lambda (row)
                     ;; The default may itself be a symlink. Both implicit
                     ;; selection and an explicit alias need short commands.
                     (when (eq? (cadr row) 'default)
                       (unless (zero? ((foreign-procedure "symlink" (string string) int) base-directory seat:default-directory))
                         (error 'fixture "cannot link the default base directory")))
                     (let* ([phase (car row)]
                            [review (and (eq? phase 'reviewing) (rpc head 'prepare-close))]
                            [before (rpc head 'status)]
                            [result (loader-exit (append '("--help" "--restart" "--force" "--name" "help inspector")
                                                   (if (eq? (cadr row) 'default) '() (list "--base-working-dir" base-directory))))]
                            [output (caddr result)]
                            [after (rpc head 'status)])
                       (when review (cancel head review))
                       (list (car result)
                             (occurrences output
                               (format "Base: alive (pid ~a; wire ~a; source ~a); ~a\nBase directory: ~s\nStop the base: M-x (lifecycle:shutdown!) or kill -TERM ~a\nThe base holds ~a.\n"
                                 (car (cdr (assq 'instance before))) wire:version (cdr (assq 'fingerprint before)) phase base-directory
                                 (car (cdr (assq 'instance before)))
                                 "1 buffer (1 modified), 1 attached head, 0 running terminals and 0 agents"))
                             (occurrences output "A tiny, fully customizable, self-aware, Emacs-like editor.")
                             (occurrences output "attached \"help inspector\"")
                             (occurrences output "detached \"saved α's desk\"")
                             (occurrences output (string-append "Resume: " (quote-shell (string-append root "/e"))
                                                   " --name " (quote-shell "saved α's desk")
                                                   (if (eq? (cadr row) 'custom)
                                                       (string-append " --base-working-dir " (quote-shell base-directory)) "") "\n"))
                             (occurrences output "Attach:")
                             (occurrences output "Describe: M-x (describe:this lifecycle:shutdown!)")
                             (occurrences output "\x1b;") (equal? before after) (car (rpc head 'snapshot id)))))
                   '((running custom) (reviewing default) (running alias)))
                 (make-list 3 '(0 1 1 1 1 1 0 0 0 #t #("keep this text"))))))
           (lambda ()
             (when (file-exists? seat:default-directory #f) (delete-file seat:default-directory))
             (write-text path source)
             (sys:close-connection! head)
             (sys:close-connection! detached)
             (fixture:stop! base)))))

     ;; The restart groups share one fixture: a base from the installation's
     ;; own sources with a maintenance connection, and a head holding shared
     ;; text and a local draft. Sources a body changes are restored afterwards.
     (define (call-with-restart-fixture body)
       (define restoring (string-append root "/replacement-restoring"))
       (define hold (string-append root "/replacement-hold"))
       (fresh-session!)
       (when (file-exists? automatic-control) (delete-file automatic-control))
       (write-text (string-append root "/config.e") "(void)\n")
       ;; The replacement is launched by the real CLI. This fixture-owned
       ;; config also gives failure cleanup a cooperative stop for that child.
       (base-config!
         `((fork-thread
             (lambda ()
               (let wait ()
                 (unless (file-exists? ,automatic-control)
                   (sleep (make-time 'time-duration 5000000 0)) (wait)))
               (system (format "kill -TERM ~a" (get-process-id)))))
           ;; Hold only the replacement before listening. A concurrent plain
           ;; launcher must wait for this owner instead of starting another.
           (when (file-exists? ,hold)
             (call-with-output-file ,restoring (lambda (p) (write #t p)) 'replace)
             (let wait ([left 4000])
               (when (file-exists? ,hold)
                 (when (zero? left) (error 'fixture "replacement restore hold timed out"))
                 (sleep (make-time 'time-duration 5000000 0)) (wait (- left 1)))))))
       (let* ([source-path (string-append sources "/base/state/store.sls")]
              [source (call-with-input-file source-path get-string-all)]
              [wire-path (string-append sources "/foundation/wire.sls")]
              [wire-source (call-with-input-file wire-path get-string-all)]
              [original-fingerprint (fingerprint)]
              [base (fixture:start! root base-directory)]
              [pid-path (string-append base-directory "/pid")]
              [original (call-with-input-file pid-path read)]
              [path (string-append base-directory "/session")]
              [temporary (string-append base-directory "/session.tmp")]
              [file (string-append root "/restart-argument")])
         (set! test-base base)
         (dynamic-wind void
           (lambda ()
             (let ([head (start-head "restart desk")] [control (connect)])
               (exchange control '(maintenance 1 (head "restart desk")))
               (head-wait 'restart-source-ready head (lambda () (head-sees? head "*scratch*")))
               (head-read head
                 '(begin
                    (edit:insert! (test-editor) "kept after restart")
                    (test-go! '(0 . 4))
                    (test-private! "<local draft kept>" "draft")
                    #t))
               (body head control base original original-fingerprint
                     source-path source wire-path wire-source pid-path path temporary file hold restoring)))
           (lambda ()
             (when (file-exists? hold) (delete-file hold))
             (write-text source-path source)
             (write-text wire-path wire-source)
             (write-control! "stop")
             (test:await 'restart-fixture-releases-ownership
               (lambda () (let ([lock (sys:acquire-file-lock (string-append base-directory "/lock"))])
                            (and lock (begin (sys:release-file-lock! lock) #t)))))
             (sys:close-process! (fixture:process base))))))

     (define (restart-scenarios!)
       ;; Refusals, reviews and cancellations against the original base.
       (call-with-restart-fixture
         (lambda (head control base original original-fingerprint
                   source-path source wire-path wire-source pid-path path temporary file hold restoring)
           ;; Renderer edits do not stale the resident base. Admission still
           ;; refuses real base changes and incompatible wire versions before
           ;; importing head code; maintenance works across both differences.
           (let ([before (rpc control 'status)]
                 [policy-before (head-read head '(length (log:entries 'policy 100)))])
             (let* ([renderer-path (string-append sources "/head/head.sls")]
                    [renderer (call-with-input-file renderer-path get-string-all)]
                    [connection (connect)])
               (dynamic-wind
                 (lambda () (write-text renderer-path (string-append renderer "\n; renderer-only change\n")))
                 (lambda ()
                   (test:check 'renderer-change-admits-a-head-without-restarting-the-base
                     (list (equal? original-fingerprint (fingerprint))
                           (list-head (exchange connection (list 'hello wire:version '(head "new renderer") (fingerprint))) 3)
                           (equal? original (call-with-input-file pid-path read)))
                     (list #t (list 'hello wire:version '(head "new renderer")) #t)))
                 (lambda () (write-text renderer-path renderer) (sys:close-connection! connection)))
               (test:await 'renderer-check-head-detaches
                 (lambda () (= (cdr (assq 'heads (rpc control 'status))) 1))))
             ;; Admission preserves a named head checkpoint in the directory.
             (set! before (rpc control 'status))
             (write-text source-path (string-append source "\n; resident base change\n"))
             (test:check 'source-and-version-refusals-keep-the-owner-and-startup-fingerprint
               (list
                 (map (lambda (message)
                        (let ([connection (connect)])
                          (list (exchange connection message) (eof-object? (receive connection)))))
                      (append (map (lambda (who) (list 'hello wire:version who (fingerprint)))
                                '((head "restart desk") (agent "mismatched")))
                        (list (list 'hello (- wire:version 1) '(head "restart desk") original-fingerprint))))
                 (equal? original-fingerprint (cdr (assq 'fingerprint before)))
                 (not (equal? original-fingerprint (fingerprint))))
               (list (make-list 3 (list (list 'error #f (list 'stale-base before)) #t)) #t #t))
             (let* ([needle (format "(define version ~a)" wire:version)]
                    [at (string:search wire-source needle 0 (string-length wire-source))])
               (unless at (error 'fixture "wire version declaration not found"))
               (test:check 'stale-launcher-refuses-before-head-import-and-screen
                 (map (lambda (version)
                        (when version
                          (write-text wire-path
                            (string-append (substring wire-source 0 at) (format "(define version ~a)" version)
                              (substring wire-source (+ at (string-length needle)) (string-length wire-source)))))
                        (let* ([result (loader-exit (list "--name" "restart desk" "--base-working-dir" base-directory))]
                               [errors (cadr result)])
                          (list (car result)
                                (occurrences errors (if version (format "wire version ~a; this head uses ~a" wire:version version)
                                                        "the running base was built from other sources than this head"))
                                (occurrences errors (string-append (quote-shell (string-append root "/e")) " --restart --name "
                                                      (quote-shell "restart desk") " --base-working-dir " (quote-shell base-directory)))
                                (occurrences errors (format "~a modified" (cdr (assq 'modified before))))
                                (occurrences (caddr result) "\x1b;")
                                (equal? before (rpc control 'status))
                                (head-read head '(list (store:line (test-document) 0) (length (log:entries 'policy 100)))))))
                      (list #f (+ wire:version 1)))
                 (make-list 2 (list 1 1 1 1 0 #t (list "kept after restart" policy-before)))))
             (write-text source-path (string-append source "\n; changed source for maintenance restart\n")))
           (for-each
             (lambda (answer)
               (let ([launcher (start-command '("--restart" "--name" "restart desk") 100)])
                 (head-wait 'restart-question-before-screen launcher
                   (lambda () (> (occurrences (vector-ref launcher 3) "Restart anyway?") 0)))
                 (test:check (list 'restart-review-before-raw-mode answer)
                   (list (occurrences (vector-ref launcher 3) "\x1b;[?1049h")
                         (cdr (assq 'heads (rpc control 'status)))) '(0 1))
                 (head-send! launcher answer)
                 (test:await 'cancelled-restart-keeps-original-base
                   (lambda () (eq? (cdr (assq 'phase (rpc control 'status))) 'running)))
                 (sys:reap-terminal-process! (vector-ref launcher 0))
                 (test:check 'restart-cancellation-is-inert
                   (list (equal? original (call-with-input-file pid-path read))
                         (head-read head '(store:line (test-document) 0))) '(#t "kept after restart"))))
             '("n\n" "\x04;"))
           (mkdir temporary #o700)
           (dynamic-wind void
             (lambda ()
               (let ([launcher (start-command '("--restart" "--force" "--name" "restart desk") 100)])
                 (head-wait 'force-still-reviews-responsive-base launcher
                   (lambda () (> (occurrences (vector-ref launcher 3) "Restart anyway?") 0)))
                 (head-send! launcher "y\n")
                 (head-wait 'explicit-save-error-is-not-forced launcher
                   (lambda () (> (occurrences (vector-ref launcher 3) "base refused the operation") 0)))
                 (test:check 'force-never-turns-a-save-error-into-a-kill
                   (list (occurrences (vector-ref launcher 3) "sending SIG")
                         (sys:process-status (fixture:process base))
                         (cdr (assq 'phase (rpc control 'status)))) '(0 #f running))))
             (lambda () (delete-directory temporary)))
         )))
     (define (restart-accept-scenarios!)
       ;; Accepted maintenance restarts across changed sources and a changed
       ;; wire version, as the review group leaves them behind.
       (call-with-restart-fixture
         (lambda (head control base original original-fingerprint
                   source-path source wire-path wire-source pid-path path temporary file hold restoring)
           (write-text source-path (string-append source "\n; changed source for maintenance restart\n"))
           (let* ([needle (format "(define version ~a)" wire:version)]
                  [at (string:search wire-source needle 0 (string-length wire-source))])
             (unless at (error 'fixture "wire version declaration not found"))
             (write-text wire-path
               (string-append (substring wire-source 0 at) (format "(define version ~a)" (+ wire:version 1))
                 (substring wire-source (+ at (string-length needle)) (string-length wire-source)))))
           (let* ([expected (head-read head '(list (store:line (test-document) 0) (test-point)))]
                  [saved-query (head-read head
                                 '(let* ([source (collection:create-source! head:ui-actor '((name "Name" string))
                                                   '#(("persisted" ((name . "persisted")) ())) 'persistent)])
                                    (collection:create! head:ui-actor source "" '() 'persistent)))]
                  [saved-widget (head-read head
                                  '(let ([id (view:create! head:ui-actor '(model 999999) 'text 1 '((catalogue . #t) (name . "<unavailable>")) '(4 2) (test-window))]
                                         [was (test-document)])
                                     (window-control:open-document! (test-window) id)
                                     (test-show! was) (interaction:flush!) id))]
                  [launcher (start-command '("--restart" "--name" "restart desk") 100)])
             (head-wait 'accepted-restart-question launcher
               (lambda () (> (occurrences (vector-ref launcher 3) "Restart anyway?") 0)))
             (write-text hold "wait for the concurrent launcher")
             (head-send! launcher "yes\n")
             (test:await 'replacement-restores-before-listening (lambda () (file-exists? restoring)))
             (let* ([replacement (call-with-input-file pid-path read)]
                    [joining (start-head "joining during restart")]
                    [heads (list launcher joining)])
               (dynamic-wind void
                 (lambda ()
                   (head-wait 'plain-start-waits-for-restoring-owner joining
                     (lambda () (> (occurrences (vector-ref joining 3) "starting the base") 0)))
                   (test:check 'start-during-restart-waits-without-replacing-the-owner
                     (list (equal? replacement (call-with-input-file pid-path read))
                           (file-exists? socket) (occurrences (vector-ref joining 3) "\x1b;[?1049h"))
                     '(#t #f 0)))
                 (lambda () (delete-file hold)))
               (for-each
                 (lambda (screen)
                   (head-wait 'restart-resumes-shared-work screen
                     (lambda () (head-sees? screen "kept after restart")))) heads)
               (test:check 'restart-keeps-unobserved-collection-idle
                 (head-read launcher `(cdr (assq 'status (cdr (assq 'value (collection:summary ',saved-query)))))) 'pending)
               (head-read launcher `(begin (model:subscribe! (list ',saved-query) (lambda (notice) (void))) #t))
               (test:await 'restored-collection-index
                 (lambda ()
                   (head-read launcher
                     `(let* ([r (collection:summary ',saved-query)] [v (cdr (assq 'value r))])
                        (and (eq? (cdr (assq 'status v)) 'ready)
                          (eq? (car (collection:range ',saved-query (cdr (assq 'generation v)) 0 1 '(name))) 'ready))))))
               (test:check 'restart-rebuilds-collection-recipe
                 (head-read launcher
                   `(let* ([v (cdr (assq 'value (collection:summary ',saved-query)))]
                           [r (collection:range ',saved-query (cdr (assq 'generation v)) 0 1 '(name))])
                      (cadar (list-ref r 4)))) "persisted")
               (test:check 'restart-restores-named-view-after-pre-screen-notice
                 (list
                   (length
                     (filter values
                             (map (lambda (head)
                                    (let* ([output (vector-ref head 3)]
                                           [notice (string:search output "restored a session saved" 0 (string-length output))]
                                           [screen (string:search output "\x1b;[?1049h" 0 (string-length output))])
                                      (and notice screen (< notice screen)))) heads)))
                   (not (equal? original replacement))
                   (head-read launcher '(list (store:line (test-document) 0) (test-point)))
                   (head-read launcher '(and (store:find-named "<local draft kept>") #t))
                   (map (lambda (head)
                          (head-read head '(let ([status (client:request 'status)])
                                             (map (lambda (key) (cdr (assq key status))) '(fingerprint wire-version instance)))))
                        heads))
                 (list 1 #t expected #t (make-list 2 (list (fingerprint) (+ wire:version 1) (cdr replacement)))))
               (head-read launcher `(begin (window-control:open-document! (test-window) ',saved-widget) #t))
               (head-wait 'restarted-widget-placeholder launcher (lambda () (head-sees? launcher "[Unavailable widget")))
               (test:check 'restart-reclaims-view-generation-without-losing-unavailable-data-state
                 (head-read launcher
                   `(let ([descriptor (interaction:snapshot ',saved-widget)])
                      (list (> (view:generation descriptor) 1) (view:state descriptor) (widget:actions ',saved-widget))))
                 '(#t (4 2) ()))
               (head-wait 'old-screen-gets-restart-farewell head
                 (lambda () (> (occurrences (vector-ref head 3) "base is restarting") 0)))
               (for-each
                 (lambda (head)
                   (head-send! head "\x18;\x03;")
                   (head-wait 'restart-screen-detaches head (lambda () (head-sees? head "e: detached")))
                   (sys:reap-terminal-process! (vector-ref head 0))) heads)))
         )))
     (define (restart-file-scenarios!)
       ;; With no live work a restart needs no review: the file argument opens
       ;; in the replacement, and a lost restart reply is neither replayed nor
       ;; forced. The fixture's head detaches first and takes its draft along.
       (call-with-restart-fixture
         (lambda (head control base original original-fingerprint
                   source-path source wire-path wire-source pid-path path temporary file hold restoring)
           ;; A hidden Finder keeps its own cursor while another view edits
           ;; their shared filter. Restart must rebase that saved cursor.
           (head-read head
             `(let* ([was (test-document)] [app (finder:open! (test-window) (unquote root))]
                     [entry (widget:descendant app 'table 'filter 'entry)])
                (entry:insert! entry "old")
                (test-show! was)
                (let ([other (view:fork! head:ui-actor entry)])
                  (widget:mount! other 'filter-writer)
                  (entry:set-text! other ,(string-append root "/A"))
                  (widget:unmount! other)
                  (view:retire! head:ui-actor other (cdr (assq 'revision (caddar (cadr (model:snapshots (list other)))))))) #t))
           ;; quitting asks nothing: the draft goes with the head, named in the exit notice
           (head-send! head "\x18;\x03;")
           (head-wait 'fixture-head-detaches head (lambda () (head-sees? head "e: detached")))
           (sys:reap-terminal-process! (vector-ref head 0))
           ;; No live work must include no evaluation agent.
           (evaluator-close!)
           (write-text file "file argument after restart\n")
           (let ([launcher (start-command (list "--restart" "--name" "restart desk" file) 100)])
             (head-wait 'no-live-work-restarts-without-question launcher
               (lambda () (head-sees? launcher "file argument after restart")))
             (test:check 'no-live-restart-keeps-omission-notice-and-opens-file-argument
               (list (occurrences (vector-ref launcher 3) "Restart anyway?")
                     (> (occurrences (vector-ref launcher 3) "restart keeps shared text with undo/redo history") 0)
                     (head-read launcher '(store:property (test-document) 'file #f))) (list 0 #t file))
             (test:check 'restarted-hidden-finder-edits-the-restored-filter
               (head-read launcher
                 '(let* ([app (finder:open! (test-window))] [entry (widget:descendant app 'table 'filter 'entry)])
                    (entry:delete! entry 'backward)
                    (let-values ([(source d inputs) (widget:context entry 'current)])
                      (vector-ref (cdr (assq 'value source)) 0))))
               (string-append root "/"))
             (head-send! launcher "\x18;\x03;")
             (head-wait 'detach-before-lost-reply launcher (lambda () (head-sees? launcher "e: detached")))
             (sys:reap-terminal-process! (vector-ref launcher 0)))
           (evaluator-close!)
           (write-text lost-closing "lose this response")
           (dynamic-wind void
             (lambda ()
               (let ([launcher (start-command '("--restart" "--force" "--name" "restart desk") 100)])
                 (head-wait 'unknown-restart-outcome-is-reported launcher
                   (lambda () (> (occurrences (vector-ref launcher 3) "restart outcome is unknown") 0)))
                 (test:await 'unacknowledged-restart-exits (lambda () (not (file-exists? pid-path))))
                 (test:check 'unknown-restart-is-neither-replayed-nor-forced
                   (list (file-exists? path) (occurrences (vector-ref launcher 3) "sending SIG")
                         (occurrences (vector-ref launcher 3) "\x1b;[?1049h")) '(#t 0 0))))
             (lambda () (delete-file lost-closing)))))
     )
     (define (force-scenarios!)
       ;; Linux supplies the actual flock holder and pidfd reference. Other
       ;; OSes intentionally refuse force until equivalent proof is available.
       (when (file-exists? "/proc/sys/kernel/random/boot_id")
         (fresh-session!)
         (when (file-exists? automatic-control) (delete-file automatic-control))
         (base-config! '())
         (set! test-base (fixture:start! root base-directory))
         (fixture:stop! test-base)
         (let ([held (string-append root "/force-held")]
               [pid-path (string-append base-directory "/pid")])
           ;; The first base gets stuck in configuration, after publishing its
           ;; identity but before serving. The replacement starts normally.
           (base-config!
             `((unless (file-exists? ,held)
                 (call-with-output-file ,held (lambda (port) (write #t port)))
                 (let stuck () (sleep (make-time 'time-duration 50000000 0)) (stuck)))
               (fork-thread
                 (lambda ()
                   (let wait ()
                     (unless (file-exists? ,automatic-control)
                       (sleep (make-time 'time-duration 5000000 0)) (wait)))
                   (system (format "kill -TERM ~a" (get-process-id)))))))
           (let ([process (sys:open-process (list "scheme-script" (string-append root "/e")
                                                  "--base" "--base-working-dir" base-directory))])
             (dynamic-wind void
               (lambda ()
                 (sys:write-process! process #f)
                 (test:await 'owned-base-is-unresponsive (lambda () (file-exists? held)))
                 (let ([original (call-with-input-file pid-path read)])
                   (fixture:call-with-base root (string-append root "/unrelated-base")
                     (lambda (other)
                       (let ([unrelated (call-with-input-file (string-append (fixture:directory other) "/pid") read)])
                         (test:check 'force-refuses-incomplete-stale-and-wrong-lock-identities
                           (map (lambda (record)
                                  (write-forms pid-path (list record)) (chmod pid-path #o600)
                                  (let ([entered? #f] [before (test:fd-count)])
                                    (let ([refused? (test:raises?
                                                      (lambda ()
                                                        (sys:call-with-verified-base base-directory
                                                          (lambda (signal! wait!) (set! entered? #t)))))])
                                      (list refused? entered? (sys:process-status process)
                                            (sys:process-status (fixture:process other)) (= before (test:fd-count))))))
                             (list (list 'base (cadr original) #f)
                                   (list 'base (cadr original) '(linux "wrong generation" 0)) unrelated))
                           (make-list 3 '(#t #f #f #f #t)))
                         (let ([launcher (start-command '("--restart" "--force") 100)])
                           (head-wait 'force-cli-refuses-unverified-instance launcher
                             (lambda () (> (occurrences (vector-ref launcher 3) "manual recovery is required") 0)))
                           (test:check 'unverified-force-does-not-signal-or-open-a-screen
                             (list (sys:process-status process) (sys:process-status (fixture:process other))
                                   (occurrences (vector-ref launcher 3) "sending SIG")
                                   (occurrences (vector-ref launcher 3) "\x1b;[?1049h")) '(#f #f 0 0))))))
                   (write-forms pid-path (list original)) (chmod pid-path #o600)
                   (sys:call-with-verified-base base-directory
                     (lambda (signal! wait!)
                       (test:check 'verified-reference-waits-for-the-live-instance
                         (list (sys:process-exited? (cdr original)) (wait! (current-time 'time-monotonic))) '(#f #f))
                       ;; `--restart --force` escalates through the product's ten-second
                       ;; SIGTERM grace; the verified reference kills the stuck instance
                       ;; here, and the fixture starts the replacement itself.
                       (signal! 9)
                       (test:await 'stuck-base-reaped (lambda () (sys:process-status process)))
                       (test:check 'verified-reference-signals-its-own-instance
                         (list (sys:process-status process) (wait! (current-time 'time-monotonic))
                               (equal? original (call-with-input-file pid-path read)))
                         '(-9 #t #t))
                       ;; Even after a replacement has taken ownership, the
                       ;; retained old reference cannot signal its new pid.
                       (let ([replacement (fixture:start! root base-directory)])
                         (signal! 9)
                         (let ([head (connect)])
                           (hello head '(head "force inspector"))
                           (test:check 'old-process-reference-cannot-retarget-the-replacement
                             (list (cdr (assq 'phase (rpc head 'status)))
                                   (not (equal? original (call-with-input-file pid-path read))))
                             '(running #t))
                           (sys:close-connection! head))
                         (fixture:stop! replacement))))))
               (lambda ()
                 (write-control! "stop")
                 (sys:close-process! process)
                 (test:await 'force-fixture-releases-ownership
                   (lambda () (let ([lock (sys:acquire-file-lock (string-append base-directory "/lock"))])
                                (and lock (begin (sys:release-file-lock! lock) #t)))))))))))

     ;; The protocol groups share one fixture: a base of this installation, its
     ;; stop and signal procedures, and the cleanup that releases everything.
     (define (call-with-protocol-base body)
       (let* ([base (fixture:start! root base-directory)]
              [pid (sys:process-pid (fixture:process base))])
         (define (signal! signal)
           (sys:signal-process! (fixture:process base)
             (cdr (assoc signal '(("TERM" . 15) ("KILL" . 9) ("HUP" . 1))))))
         (define (stop!)
           (unless stopped?
             (set! stopped? #t)
             (fixture:stop! base)))
         (set! test-base base)
         (dynamic-wind void
           (lambda () (body base pid signal! stop!))
           (lambda ()
             (write-text edit-release "continue")
             (write-control! "stop")
             (guard (ex [else (void)]) (stop!))
             (for-each sys:close-connection! clients)
             (for-each (lambda (head) (sys:close-terminal-process! (vector-ref head 0))) heads)
             (test:await 'automatic-fixture-releases-ownership
               (lambda ()
                 (let ([lock (sys:acquire-file-lock (string-append base-directory "/lock"))])
                   (and lock (begin (sys:release-file-lock! lock) #t)))))
             (void)))))

     ;; The bootstrap groups need only the installation: a stopped base record
     ;; names it for the heads' launch commands.
     (define (call-with-bootstrap-installation body)
       (set! test-base (fixture:start! root base-directory))
       (fixture:stop! test-base)
       (dynamic-wind void body
         (lambda ()
           (write-control! "stop")
           (for-each sys:close-connection! clients)
           (for-each (lambda (head) (sys:close-terminal-process! (vector-ref head 0))) heads)
           (test:await 'automatic-fixture-releases-ownership
             (lambda ()
               (let ([lock (sys:acquire-file-lock (string-append base-directory "/lock"))])
                 (and lock (begin (sys:release-file-lock! lock) #t))))))))

     (define (protocol-scenarios!)
       (call-with-protocol-base
         (lambda (base pid signal! stop!)
           (test:check 'contending-base-exits-three-without-changing-the-owner
             (list (car (base-exit base-directory))
               (cadr (call-with-input-file (string-append base-directory "/pid") read)))
             (list 3 pid))
           (let ([unsafe (string-append root "/unsafe")])
             (mkdir unsafe #o755)
             (test:check 'nonprivate-base-directory-is-not-repaired-or-used
               (list (car (base-exit unsafe)) (get-mode unsafe) (directory-list unsafe))
               '(1 #o755 ()))
             (delete-directory unsafe))
           (let* ([head (connect)] [identity '(head "desk λ")])
             (test:check 'claim-precedes-welcome-and-queued-mail
               (list (hello head identity) (receive head))
               (list (list 'hello wire:version identity '(read edit undo redo)) '(event (from-base "welcome"))))
             (test:check 'registered-operation-uses-generic-envelope-and-void-acknowledgement
               (list (rpc head 'invoke 'operation-probe:remember! '((datum) #f) '("wire"))
                     (rpc head 'invoke 'operation-probe:snapshot '(() (actor integer datum)) '())
                     (and (reject head 'invoke 'kernel:load-module! '((string) #f) '("never-load")) #t))
               '((completed) (values (head "desk λ") 1 "wire") #t))
             (let* ([binding (cadr (rpc head 'invoke 'composition:acquire! '((string) (list list)) '("blank")))]
                    [id (cdr (assq 'id binding))] [value (cdr (assq 'value binding))])
               (test:check 'composition-bindings-refuse-generic-wire-writes
                 (map (lambda (attempt) (and (apply reject head attempt) #t))
                   (list (list 'model-create 'composition-binding 1 identity 'persistent '() value)
                     (list 'model-commit (list (list id (cdr (assq 'revision binding)) '() value)))
                     (list 'model-retire id (cdr (assq 'revision binding))))) '(#t #t #t)))
             (let* ([inspection (rpc head 'inspection-create 'subject '(keys))]
                    [id (car inspection)] [part (cdar (cadr inspection))])
               (test:check 'inspection-authority-cannot-be-bypassed-by-generic-model-operations
                 (map (lambda (ref)
                        (let* ([r (caddr (caadr (rpc head 'model-read (list ref))))]
                               [kind (cdr (assq 'kind r))] [revision (cdr (assq 'revision r))]
                               [value (cdr (assq 'value r))] [references (cdr (assq 'references r))])
                          (map (lambda (attempt) (and (apply reject head attempt) #t))
                            (list (list 'model-create kind 1 identity 'transient references value)
                              (list 'model-commit (list (list ref revision references value)))
                              (list 'model-retire ref revision)))))
                   (list id part)) '((#t #t #t) (#t #t #t)))
               (rpc head 'inspection-close id))
             (let* ([draft (rpc head 'conflict-review-create '())]
                    [preview (rpc head 'review-preview-create draft)]
                    [id (car preview)] [document (cadr preview)]
                    [r (caddr (caadr (rpc head 'model-read (list id))))])
               (test:check 'preview-retirement-must-dispose-owned-output-through-its-service
                 (list (and (reject head 'model-retire id (cdr (assq 'revision r))) #t)
                   (begin (rpc head 'review-preview-close id)
                          (list (caddr (caadr (rpc head 'model-read (list id))))
                            (member document (rpc head 'buffers))))) '(#t (#f #f)))
               (rpc head 'conflict-review-close draft 0))
             (let* ([ids (rpc head 'buffers)] [snapshot (rpc head 'snapshot (car ids))]
                    [facts (caddr snapshot)])
               (test:check 'base-only-config-and-owned-snapshot
                 (list (map (lambda (id) (rpc head 'name id)) ids) (car snapshot)
                   (cdr (assq 'bootstrap facts)) (cdr (assq 'process-id facts)))
                 (list '("notes λ" "private" "*terminal*") '#("hello λ") '(() #f (base e) ()) pid))
               (test:check 'core-reload-refuses-and-explicit-revoke-reaches-retained-entries
                 (cdr (assq 'authority facts))
                 '((#t #t #t #t #t) (ok . "=> 3")
                   ((refused . "the session is revoked") (refused revoked) (refused revoked) (refused revoked) #f ())))
               (string-set! (vector-ref (car snapshot) 0) 0 #\X)
               (test:check 'client-mutation-cannot-change-the-base
                 (car (rpc head 'snapshot (car ids))) '#("hello λ")))
             (let ([temporary (connect)])
               (hello temporary '(head "prompt owner")) (receive temporary)
               (let* ([id (rpc temporary 'prompt-create #f #f "input" '((origin . explicit)) '(head-symbols 1))]
                      [record (caddr (caadr (rpc head 'model-read (list id))))]
                      [draft (cdr (assq 'draft (cdr (assq 'value record))))]
                      [view (rpc temporary 'view-create draft 'entry 1 '() '((0 . 0) (0 . 0)) id)]
                      [removed (rpc temporary 'view-create #f 'label 1 '() '() id)])
                 (test:check 'prompt-wire-attribution-and-service-ownership
                   (list (rpc head 'prompt-accept id 0 0)
                     (rpc temporary 'prompt-accept id 0 0)
                     (list-head (exchange temporary `(request 7 model-retire ,id 1)) 3)
                     (list-head (exchange temporary `(request 7 model-retire ,removed 0)) 3)
                     (rpc temporary 'view-retire removed 0))
                   '(unavailable applied (reply 7 error) (reply 7 error) (applied #f)))
                 (sys:close-connection! temporary)
                 (test:await 'prompt-wire-departure-releases-owned-state
                   (lambda () (and (not (caddr (caadr (rpc head 'model-read (list id)))))
                                (not (caddr (caadr (rpc head 'model-read (list view)))))
                                (not (member draft (rpc head 'buffers))))))))
             (let* ([temporary (connect)] [document (car (rpc head 'buffers))])
               (hello temporary '(head "search owner")) (receive temporary)
               (let* ([target (rpc temporary 'view-create document 'editor 1 '() '((0 . 0) (0 . 0) (0 . 0) #f))]
                      [request (map cons '(target document basis sequence start needle fold? visible direction summary? overlap?)
                                 (list target document 0 0 '(0 . 0) "hello" #f '(0 0 0 7) 'next #f #t))]
                      [id (rpc temporary 'search-create request #t)])
                 (test:check 'search-wire-owner-and-generation-fences
                   (list (rpc head 'search-configure id 0 request)
                     (rpc temporary 'search-configure id 0 request)
                     (rpc temporary 'search-configure id 0 request)
                     (list-head (exchange temporary `(request 7 model-retire ,id 1)) 3))
                   '(#f 1 #f (reply 7 error)))
                 (sys:close-connection! temporary)
                 (test:await 'search-wire-departure-releases-request
                   (lambda () (not (caddr (caadr (rpc head 'model-read (list id)))))))
                 (rpc head 'view-retire target 0)))
             (let* ([temporary (connect)] [gate (string-append root "/environment-continue")])
               (hello temporary '(head "environment owner")) (receive temporary)
               (let* ([environment (rpc temporary 'environment-create
                                     (list (cons 'directory root) '(roots) '(imports (chezscheme))) 'transient)]
                      [job (rpc temporary 'environment-evaluate environment 1
                             (format "(display \"ready\") (let wait () (unless (file-exists? ~s) (sleep (make-time 'time-duration 5000000 0)) (wait))) 42" gate) #f)])
                 (define (job-value)
                   (cdr (assq 'value (caddr (caadr (rpc head 'model-read (list job)))))))
                 (let ([output (cdr (assq 'output (job-value)))])
                   (test:await 'environment-worker-streaming
                     (lambda () (equal? (car (rpc head 'snapshot output)) '#("ready"))))
                   (sys:close-connection! temporary)
                   (write-text gate "continue")
                   (test:await 'environment-finishes-after-detach
                     (lambda () (eq? (cdr (assq 'status (job-value))) 'ok)))
                   (test:check 'environment-wire-preserves-jobs-and-protects-service-models
                     (list (cdr (assq 'result (job-value)))
                       (list-head (exchange head `(request 7 model-retire ,environment 0)) 3)
                       (rpc head 'environment-release job) (rpc head 'environment-close environment 1))
                     '((value (42) "(42)") (reply 7 error) #t #t)))))
             (test:check 'bad-hello-and-duplicate-name-preserve-the-owner
               (map (lambda (message)
                      (let ([duplicate (connect)])
                        (list (car (exchange duplicate message))
                          (eof-object? (receive duplicate))
                          (and (member identity (map car (rpc head 'actors))) #t)
                          (inventory head))))
                    (append (map (lambda (version) (list 'hello version identity (fingerprint)))
                              (list 0 (- wire:version 1) wire:version))
                      (list (list 'hello wire:version identity) (list 'hello wire:version identity #f)
                            (list 'hello wire:version identity "mismatched"))))
               (make-list 6 (list 'error #t #t (list (list identity identity)))))
             (test:check 'request-errors-preserve-the-connection
               (map (lambda (message) (list-head (exchange head message) 3))
                    '((request 1 edit) (request 2 snapshot (buffer 999)) (request 3 buffers extra)
                      (request 4 edit (buffer 1) 0 (0 0 -1 0) ("x")) (request 5 edit (buffer 1) 0.5 (0 0 0 0) ("x"))
                      (request 6 undo (buffer 1) everyone)
                      (request 7 edit (buffer 1) 0 (0 0 0 0) ("x") (g "invalid" ((trailing . #t)) ((trailing . #f))))
                      (request 8 edit (buffer 1) 0 (0 0 0 0) ("x") #f #f #f)
                      (request 8 edit (buffer 1) 0 (0 0 0 0) ("x") #f delta)
                      (request 9 redo (buffer 1) all) (request 10 snapshot (buffer 1) #f)
                      (request 11 snapshot (buffer 1) -1) (request 12 watch extra)
                      (request 13 checkpoint (head "another") stolen)
                      (request 14 eval) (request 15 eval #f (agent "forged"))
                      (request 16 revoke (head "desk λ")) (request 17 revoke #f)
                      (request 18 log-snapshot 0 -1) (request 19 log-snapshot 0 1.0)
                      (request 20 log-snapshot 0 1 "component") (request 21 log-snapshot)
                      (request 22 log-snapshot 0 1 #f extra)
                      (request 23 log-retention 0) (request 24 log-retention -1)
                      (request 25 log-retention 1.0) (request 26 log-retention 4 5)
                      (request 27 actors)
                      (request 28 find-file #f) (request 29 visit "file" ("seed") ())
                      (request 30 visit "file" ("seed") ((file . #f)))))
               '((reply 1 error) (reply 2 error) (reply 3 error) (reply 4 error)
                 (reply 5 error) (reply 6 error) (reply 7 error) (reply 8 error) (reply 8 error)
                 (reply 9 error) (reply 10 error) (reply 11 error) (reply 12 error) (reply 13 error)
                 (reply 14 error) (reply 15 error) (reply 16 error) (reply 17 error)
                 (reply 18 error) (reply 19 error) (reply 20 error) (reply 21 error)
                 (reply 22 error) (reply 23 error) (reply 24 error) (reply 25 error)
                 (reply 26 error) (reply 27 ok) (reply 28 error) (reply 29 error) (reply 30 error)))
             (test:check 'existing-endpoint-is-never-unlinked
               (list (test:raises? (lambda () (sys:listen-local socket))) (rpc head 'name '(buffer 1)))
               '(#t "notes λ"))
             (sys:close-connection! head))
           ;; Leave one connection stalled before hello and another after it.
           ;; The background actor must still collect and publish with no head.
           (let ([idle (connect)] [agent (connect)] [identity '(agent "reader")])
             (test:check 'agent-uses-the-same-read-connection
               (list (hello agent identity) (receive agent) (rpc agent 'buffers)
                 (car (rpc agent 'snapshot '(buffer 2)))
                 (rpc agent 'eval "(buffer-text-line \"notes λ\" 0)") (rpc agent 'eval #f)
                 (rpc agent 'eval "'#0=#(#0#)")
                 (rpc agent 'eval "(") (car (rpc agent 'eval "(delete-file \"unused\")")))
               (list (list 'hello wire:version identity '(read)) '(event (from-base "welcome")) '((buffer 1) (buffer 2) (buffer 3))
                 '#("a local audience") '(ok . "=> \"hello λ\"") '(ok . "=> #f")
                 '(ok . "=> #0=#(#0#)")
                 '(error . "unreadable expression") 'unbound))
             (let* ([tail (rpc agent 'log-snapshot 0 2 'wire-retained)]
                    [bounds (rpc agent 'log-snapshot 0 0)])
               (rpc agent 'log-add 'wire-retained 'after-bookmark #f)
               (test:check 'bounded-log-reads-report-expiry-and-resume-from-absolute-bookmarks
                 (list (map cadddr (car tail)) (> (caddr tail) 0) (- (cadr tail) (caddr tail))
                   (car bounds)
                   (map cadddr (car (rpc agent 'log-snapshot (cadr bounds) #f 'wire-retained)))
                   (map cadddr (car (rpc agent 'log-snapshot 0 2 'wire-retained identity)))
                   (car (rpc agent 'log-snapshot 0 10 'missing-component)))
                 '((5001 5000) #t 5000 () (after-bookmark) (after-bookmark) ())))
             (let ([limited (connect)])
               (hello limited '(head "read only")) (receive limited)
               (test:check 'registered-operations-reuse-admission-and-authenticated-context
                 (map (lambda (connection)
                        (list (rpc connection 'invoke 'operation-probe:snapshot '(() (actor integer datum)) '())
                              (and (reject connection 'invoke 'operation-probe:remember! '((datum) #f) '("refused")) #t)))
                   (list agent limited))
                 '(((values (agent "reader") 1 "wire") #t) ((values (head "read only") 1 "wire") #t)))
               (test:check 'session-control-requires-an-all-buffer-human-head
                 (map (lambda (connection)
                        (map (lambda (message) (list-head (exchange connection message) 3))
                             '((request 1 sessions) (request 2 revoke (agent "reader"))
                               (request 3 log-retention 100) (request 4 log-retention))))
                      (list agent limited))
                 '(((reply 1 error) (reply 2 error) (reply 3 error) (reply 4 ok))
                   ((reply 1 error) (reply 2 error) (reply 3 error) (reply 4 ok))))
               (sys:close-connection! limited))
             (test:await 'head-detached
               (lambda () (not (exists (lambda (entry) (eq? (caar entry) 'head)) (rpc agent 'actors)))))
             (test:check 'agent-requests-check-authority-and-question-data
               (map (lambda (message) (list-head (exchange agent message) 3))
                    '((request 1 checkpoint) (request 2 checkpoint stolen)
                      (request 3 send (head "desk λ") raw-control)
                      (request 4 ask (head "desk λ") #t ())
                      (request 5 ask (head "desk λ") "Choices?" (1))
                      (request 6 ask (agent "forged") (head "desk λ") "Source?" ())))
               '((reply 1 error) (reply 2 error) (reply 3 error)
                 (reply 4 error) (reply 5 error) (reply 6 error)))
             (wire:send! (sys:connection-output agent)
               '(request 70 ask (agent "background") "Immediate?" ()))
             (test:check 'an-immediate-answer-precedes-its-ticket-reply-and-keeps-false-values
               (list (receive-reply agent) (number? (reply-value (receive-reply agent) 70))
                 (rpc agent 'owner) (rpc agent 'ask "No owner?" '()))
               '((event (answer 70 #f)) #t #f #f))
             (let* ([first (connect)] [second (connect)]
                    [writers (list first second)] [actors '((agent "first") (agent "second"))])
               (test:check 'configured-agents-use-server-selected-permissions
                 (map (lambda (connection actor)
                        (list (hello connection actor) (receive connection)
                          (rpc connection 'watch) (rpc connection 'watch))) writers actors)
                 (map (lambda (actor) (list (list 'hello wire:version actor '(read edit undo redo))
                                        '(event (from-base "welcome")) '((buffer 1) (buffer 2) (buffer 3)) '((buffer 1) (buffer 2) (buffer 3)))) actors))
               (test:check 'wire-evaluation-uses-the-configured-grants-fuel-and-preview-cap
                 (list (rpc first 'eval "(+ 1 2)")
                   (rpc first 'eval "(display \"hi\") (+ 1 2)")
                   (rpc first 'eval "(quote \"abcdefghijklmnopqrstuvwxyz\")")
                   (car (rpc first 'eval "(buffer-names)"))
                   (car (rpc first 'eval "(let loop () (loop))"))
                   (car (rpc first 'eval "(make-vector 100000 #f)"))
                   (car (rpc second 'snapshot '(buffer 1))))
                 '((ok . "=> 3") (ok . "=> 3\noutput:\nhi") (ok . "=> \"abcdefghijkl ...")
                   unbound fuel fuel #("hello λ")))
               ;; Hold the first policy audit callback after commit. A second
               ;; actor commits before the first reply: its receipt must still
               ;; describe exactly its own accepted revision and anchor chain.
               (wire:send! (sys:connection-output first)
                 '(request 7 edit (buffer 1) 0 (0 0 0 5) ("HELLO")
                    ((batch 1)
                     "replace and prefix"
                     (undo (trailing . #f))
                     (commit (saved-stamp . "observed")))
                    #t))
               (test:await 'first-edit-committed (lambda () (file-exists? edit-held)))
               (let ([second-result (rpc second 'edit '(buffer 1) 0 '(0 7 0 7) '("!") '((batch 1) "other actor"))])
                 (write-text edit-release "continue")
                 (let ([results (list (reply-value (receive-reply first)) second-result)])
                   (test:check 'full-and-delta-receipts-keep-each-writers-own-commit-facts
                     (list
                       (map
                         (lambda (result actor)
                           (let* ([receipt (cadr result)] [changes (caddr receipt)])
                             (list (car result) (car receipt) (cadr receipt)
                               (apply-changes '#("hello λ") changes)
                               (equal? (cadr (car (reverse changes))) actor)
                               (map car changes) (length receipt) (map car (cadddr receipt)))))
                         results actors)
                       (< (cdr (assq 'modified-at (cadddr (cadar results))))
                          (cdr (assq 'modified-at (cadddr (cadadr results))))))
                     '(((applied 1 #f #("HELLO λ") #t (1) 4 (modified modified-at conflicts))
                        (applied 2 #("HELLO λ!") #("HELLO λ!") #t (1 2) 4 (modified modified-at conflicts))) #t))))
               (test:check 'watchers-adopt-one-text-facts-and-anchor-snapshot
                 (map
                   (lambda (connection)
                     (let* ([state (rpc connection 'snapshot '(buffer 1) 0)] [chain (cadddr state)])
                       (list (car state) (cadr state) (assq 'trailing (caddr state))
                         (assq 'saved-stamp (caddr state))
                         (apply-changes '#("hello λ") chain) (map cadr chain)
                         (and (exists (lambda (notice)
                                        (and (eq? (car notice) connection)
                                             (or (not (cdr notice)) (assoc '(buffer 1) (cdr notice))))) (notices)) #t))))
                   writers)
                 (make-list 2 '(#("HELLO λ!") 2 (trailing . #f) (saved-stamp . "observed")
                                #("HELLO λ!") ((agent "first") (agent "second")) #t)))
               (test:check 'wire-delta-replies-omit-text-only-with-a-complete-chain
                 (let ([delta (rpc (car writers) 'state '(buffer 1) 0 #t)]
                       [plain (rpc (car writers) 'state '(buffer 1) 0)]
                       [future (rpc (car writers) 'state '(buffer 1) 5 #t)])
                   (list (cadr delta) (apply-changes '#("hello λ") (list-ref delta 4)) (caddr delta)
                     (cadr plain) (cadr future) (list-ref future 4)
                     (map car (cadddr (rpc (car writers) 'state '(buffer 1) 2 'facts)))
                     (and (assq 'trailing (cadddr delta)) #t)))
                 '(#f #("HELLO λ!") 2 #("HELLO λ!") #("HELLO λ!") #f (modified modified-at conflicts) #t))
               (test:check 'stale-and-permission-refusals-preserve-text
                 (list (rpc second 'edit '(buffer 1) 0 '(0 1 0 3) '("bad"))
                   (rpc agent 'edit '(buffer 1) 2 '(0 0 0 0) '("bad"))
                   (rpc first 'edit '(buffer 2) 0 '(0 0 0 0) '("bad"))
                   (rpc agent 'undo '(buffer 1) 'all) (rpc first 'undo '(buffer 2)) (rpc agent 'redo '(buffer 1)) (rpc first 'redo '(buffer 2))
                   (car (rpc agent 'snapshot '(buffer 1))) (car (rpc agent 'snapshot '(buffer 2))))
                 '((stale overlap) (refused buffer) (refused buffer) (refused buffer) (refused buffer) (refused buffer) (refused buffer)
                   #("HELLO λ!") #("a local audience")))
               ;; Repeating an actor/buffer-local key joins its two commits,
               ;; across the other writer's same-key edit. Undo facts reverse
               ;; with the text; observed external facts survive undo/redo.
               (rpc first 'edit '(buffer 1) 2 '(0 0 0 0) '("A") '((batch 1) "replace and prefix"))
               (let* ([mine (rpc first 'undo '(buffer 1))] [after-mine (rpc agent 'snapshot '(buffer 1))]
                      [again (rpc first 'undo '(buffer 1))] [other (rpc first 'undo '(buffer 1) '(actor (agent "second")))])
                 (test:check 'grouped-undo-defaults-to-mine-and-keeps-commit-facts
                   (list mine (car after-mine) (assq 'trailing (caddr after-mine))
                     (assq 'saved-stamp (caddr after-mine)) again other)
                   '((applied 5) #("hello λ!") #f (saved-stamp . "observed") (nothing #f) (applied 6))))
               (let* ([original-author (rpc second 'redo '(buffer 1))] [other (rpc first 'redo '(buffer 1))]
                      [mine (rpc first 'redo '(buffer 1))] [after (rpc agent 'snapshot '(buffer 1))]
                      [undo-group (rpc first 'undo '(buffer 1) 'all)] [undo-other (rpc first 'undo '(buffer 1) 'all)])
                 (test:check 'redo-belongs-to-the-undo-requester-and-restores-whole-groups
                   (list original-author other mine (car after)
                     (assq 'trailing (caddr after)) (assq 'saved-stamp (caddr after))
                     undo-group undo-other (car (rpc agent 'snapshot '(buffer 1))))
                   '((nothing #f) (applied 7) (applied 9) #("AHELLO λ!")
                     (trailing . #f) (saved-stamp . "observed") (applied 11) (applied 12) #("hello λ"))))
               (rpc first 'edit '(buffer 1) 12 '(0 0 0 0) '("") '(protect "protect" (undo (read-only . #t))))
               (let ([before (rpc agent 'snapshot '(buffer 1))])
                 (test:check 'wire-clients-cannot-bypass-read-only-with-facts-or-history
                   (list (rpc first 'edit '(buffer 1) 13 '(0 0 0 0) '("bad") '(escape "escape" (undo (read-only . #f)) (expected read-only)))
                     (rpc second 'edit '(buffer 1) 13 '(0 0 0 0) '("bad"))
                     (rpc first 'undo '(buffer 1) 'all) (rpc first 'redo '(buffer 1))
                     (equal? before (rpc agent 'snapshot '(buffer 1))))
                   '((refused read-only) (refused read-only) (refused read-only) (refused read-only) #t)))
               (for-each sys:close-connection! writers)
               (test:await 'connection-sessions-revoked
                 (lambda () (equal? (inventory agent) (list (list identity #f))))))
             ;; Exercise both outbox bounds with a peer that sends requests
             ;; but never reads replies. Other clients and the PTY stay live;
             ;; overload must wake the blocked writer and revoke its session.
             (test:check 'slow-readers-disconnect-without-blocking-other-clients
               (map
                 (lambda (scenario)
                   (let* ([slow (connect)] [who (list 'agent (car scenario))])
                     (hello slow who) (receive slow)
                     (let ([sent (test:worker
                                   (lambda ()
                                     (guard (ex [else (void)])
                                       (do ([i 0 (+ i 1)]) ((= i (cdr scenario)))
                                           (wire:send! (sys:connection-output slow) (list 'request i 'snapshot '(buffer 2)))))))])
                       (test:await 'overloaded-reader-detached
                         (lambda () (not (assoc who (rpc agent 'actors)))))
                       (sent))
                     (sys:close-connection! slow)
                     (list (not (assoc who (inventory agent)))
                       (cdr (assq 'alive (caddr (rpc agent 'snapshot '(buffer 3))))))))
                 '(("stalled count" . 512) ("stalled bytes" . 32)))
               '((#t #t) (#t #t)))
             (let ([slow (connect)] [who '(agent "stalled mail")])
               (wire:send! (sys:connection-output slow) (list 'hello wire:version who (fingerprint)))
               (test:await 'publication-mail-refused
                 (lambda () (assq 'mail-refused (caddr (rpc agent 'snapshot '(buffer 1))))))
               (test:await 'publication-overload-detached
                 (lambda () (not (assoc who (rpc agent 'actors)))))
               (test:check 'publication-overload-refuses-delivery-and-cleans-up-before-writer-start
                 (list (eof-object? (receive slow)) (not (assoc who (inventory agent)))
                   (assq 'mail-refused (caddr (rpc agent 'snapshot '(buffer 1)))))
                 '(#t #t (mail-refused . #t))))
             (write-text trigger "continue")
             (guard (ex [else (error 'wire-test "background producer did not converge"
                                (kernel:condition-text ex)
                                (and (file-exists? producer-result) (call-with-input-file producer-result get-string-all))
                                (fixture:diagnostics base))])
               (test:await 'background-agent
                 (lambda () (equal? (car (rpc agent 'snapshot '(buffer 1))) '#("agent work while detached")))))
             (test:check 'terminal-outlives-head-disconnect
               (cdr (assq 'alive (caddr (rpc agent 'snapshot '(buffer 3))))) #t)
             (signal! "HUP")
             (test:check 'hup-keeps-the-base-and-current-revision
               (car (rpc agent 'snapshot '(buffer 1))) '#("agent work while detached"))
             ;; a basis before the reset at 14 gets a chain, the reset bridged by
             ;; a line diff; a future basis gets none
             (test:check 'wire-catchup-distinguishes-current-bridged-and-missing-history
               (map (lambda (basis)
                      (let ([state (rpc agent 'snapshot '(buffer 1) basis)])
                        (list (car state) (cadr state) (assq 'read-only (caddr state))
                              (let ([changes (cadddr state)]) (if (pair? changes) 'chain changes)))))
                    '(14 13 15))
               '((#("agent work while detached") 14 (read-only . #t) ())
                 (#("agent work while detached") 14 (read-only . #t) chain)
                 (#("agent work while detached") 14 (read-only . #t) #f)))
             (let ([recorded #f])
               ;; Text commits before callbacks complete. Wait for the audit
               ;; prefix instead of racing a partially rewritten fixture file.
               (test:await 'audit-through-background-reset
                 (lambda ()
                   (guard (ex [else #f])
                     (set! recorded (call-with-input-file audit-file read))
                     (= (length (car recorded)) 14))))
               (let ([audits (car recorded)] [app (cadr recorded)])
                 (test:check 'base-audits-once-under-the-author-and-keeps-app-output-quiet
                   (list (map (lambda (entry) (caddr (cadr entry))) audits)
                     (map car audits) (cadar audits) (cadr (car (reverse audits)))
                     (and (pair? app) (for-all not app)))
                   (list (map add1 (iota 14))
                     (append '((agent "first") (agent "second")) (make-list 11 '(agent "first"))
                       '((agent "background")))
                     '(edit (buffer 1) 1 (0 0 0 5)) '(reset (buffer 1) 14) #t))))
             (let ([head (connect)])
               (hello head '(head "desk λ")) (receive head)
               (test:check 'released-name-reads-producer-work-and-respects-read-only
                 (list (car (rpc head 'snapshot '(buffer 1)))
                   (rpc head 'edit '(buffer 1) 14 '(0 0 0 0) '("bad")) (rpc head 'undo '(buffer 1)) (rpc head 'redo '(buffer 1)))
                 '(#("agent work while detached") (refused read-only) (refused read-only) (refused read-only)))
               ;; Exercise connection contention and framing here; store.ss
               ;; owns the larger atomic name/facts stress. Bounded bursts
               ;; keep writes contending without filling the outbox.
               (let* ([writer (connect)]
                      [target (rpc head 'create "epoch-0" '("text") '((epoch . 0)))])
                 (hello writer '(head "state writer")) (receive writer)
                 (let ([results
                        (test:parallel 2
                          (lambda (index)
                            (let ([connection (if (zero? index) writer head)] [mismatch #f]
                                  [width (if (zero? index) 16 1)] [batches (if (zero? index) 8 128)])
                              (do ([batch 0 (+ batch 1)]) ((= batch batches))
                                  (do ([offset 0 (+ offset 1)]) ((= offset width))
                                    (let ([n (+ (* batch width) offset 1)])
                                      (wire:send! (sys:connection-output connection)
                                        (if (zero? index)
                                          `(request 7 properties ,target ((epoch . ,n)) #f ,(format "epoch-~a" n))
                                          `(request 7 state ,target #f)))))
                                  (do ([offset 0 (+ offset 1)]) ((= offset width))
                                    (let ([result (reply-value (wire:receive (sys:connection-input connection)))])
                                      (unless (if (zero? index) (eq? result #t)
                                                (or (not result)
                                                    (equal? (car result)
                                                      (format "epoch-~a" (cdr (assq 'epoch (cadddr result)))))))
                                        (unless mismatch (set! mismatch result))))))
                              (when (zero? index) (rpc writer 'delete target))
                              mismatch)))])
                   (test:check 'wire-state-keeps-name-and-facts-coherent-through-deletion
                     (list results (rpc head 'state target #f)) '((#f #f) #f)))
                 (sys:close-connection! writer))
             )
             (stop!)
             (for-each (lambda (head)
                         (head-wait 'head-restores-terminal-after-disconnect head
                           (lambda ()
                             (and (> (occurrences (vector-ref head 3) "\x1b;[?1049l") 0)
                                  (let ([state (vt:emulator-state (vector-ref head 2))])
                                    (not (or (cdr (assq 'mouse-tracking state))
                                           (cdr (assq 'sgr-mouse state)))))))))
               ;; SIGKILL cannot run terminal cleanup; all cooperative exits can.
               (filter (lambda (head) (not (memq head killed-heads))) heads))
             (test:check 'stop-closes-idle-clients-and-releases-the-path
               (list (eof-object? (receive idle)) (receive agent)
                 (eof-object? (receive agent)) (file-exists? socket))
               '(#t (closing signal) #t #f)))
           (let ([terminal-pid (call-with-input-file terminal-pid-file read)])
             (test:check 'base-stop-reaps-its-terminal-and-revokes-every-session
               (list (zero? (system (format "kill -0 ~a 2>/dev/null" terminal-pid)))
                 (call-with-input-file inventory-file read)) '(#f ())))
           (void))))
     (define (attached-scenarios!)
       (call-with-protocol-base
         (lambda (base pid signal! stop!)
           (let ([head (connect)] [agent (connect)])
             (hello head '(head "desk λ")) (receive head)
             (hello agent '(agent "reader")) (receive agent)
             (let ([id (rpc head 'create "attached text" '("shared text") '((trailing . #t)))])
               (write-forms (string-append root "/config.e")
                 `((void)
                   (extension:load! ,operation-extension "operation-probe")
                   (kernel:load-module! "composition")
                   (head:run-on-main! (lambda () (head:run-on-main!
                                                   (lambda () (screen:open-document! (cdr (assq 'root (cdr (assq 'value (root:current))))) ',id)))))))
               (let* ([a (start-head "screen A")]
                      [ready-a (head-wait 'first-real-head a (lambda () (head-sees? a "shared text")))]
                      [b (start-head "screen B")])
                 (head-wait 'second-real-head b (lambda () (head-sees? b "shared text")))
                 (write-forms (string-append root "/config.e")
                   `((extension:load! ,operation-extension "operation-probe")
                     (kernel:load-module! "composition")))
                 (test:check 'shared-operation-generates-client-proxy-with-exact-results
                   (head-read a
                     '(list
                        (eq? (operation-probe:remember! "from proxy") (void))
                        (operation-probe:attached?)
                        (call-with-values (lambda () (operation-probe:broken 'zero)) list)
                        (actor:call-as '(agent "forged")
                          (lambda () (call-with-values operation-probe:snapshot list)))
                        (guard (ex [else #t]) (operation-probe:broken 'throw) #f)))
                   '(#t #t () ((head "screen A") 1 "from proxy") #t))
                 (test:check 'composition-empty-admission-crosses-generated-client-seam
                   (head-read a
                     '(let-values ([(binding rows) (composition:acquire! "blank")])
                        (let-values ([(status current rows) (composition:admit! binding #f '() #f)])
                          (list status (cdr (assq 'initialized? (cdr (assq 'value current)))) rows))))
                   '(applied #t ()))
                 (test:check 'rewrite-preview-crosses-the-client-seam
                   (head-read a `(call-with-values (lambda () (store:rewrite-preview ',id '())) list)) '(#("shared text") () () 0))
                 (test:check 'rewrite-draft-lifecycle-crosses-the-client-seam
                   (head-read a `(let* ([draft (rewrite:create! head:ui-actor ',id)] [p (rewrite:preview draft)])
                                   (let ([preview (review-preview:create! head:ui-actor draft)])
                                     (review-preview:close! head:ui-actor (car preview))
                                     (when (store:exists? (cadr preview)) (error 'preview "output survived retirement")))
                                   (rewrite:close! head:ui-actor draft 0)
                                   (list (list-ref p 3) (list-ref p 6)))) '(#("shared text") 0))
                 (test:check 'conflict-review-lifecycle-crosses-the-client-seam
                   (head-read a `(let* ([draft (conflict-review:create! head:ui-actor (list ',id))]
                                        [p (conflict-review:preview draft ',id)]
                                        [results (conflict-review:settle! head:ui-actor draft 0 (list ',id))])
                                   (conflict-review:close! head:ui-actor draft 0)
                                   (list (list-ref p 3) (map cadr results)))) '(#("shared text") (applied)))
                 (let* ([draft (rpc head 'conflict-review-create (list id))]
                        [query (rpc head 'collection-create draft "" '() 'persistent)]
                        [git (rpc head 'git-source ".")] [metadata #f])
                   (rpc head 'model-watch (list query))
                   (test:await 'empty-review-rows
                     (lambda ()
                       (set! metadata (cdr (assq 'value (rpc head 'collection-summary query))))
                       (eq? (cdr (assq 'status metadata)) 'ready)))
                   (test:check 'no-result-controls-return-portable-acknowledgements
                     (list (rpc head 'conflict-source-choose-all query (cdr (assq 'generation metadata))
                             (cdr (assq 'basis metadata)) 'disk) (rpc head 'git-refresh git)) '(#t #t))
                   (rpc head 'model-retire query (cdr (assq 'revision (rpc head 'collection-summary query))))
                   (rpc head 'model-retire git 0)
                   (rpc head 'conflict-review-close draft 0))
                 (for-each (lambda (ui)
                             (head-read ui `(begin (log-view:open! (test-window)) (window-control:keep! (test-window))
                                                   (test-show! ',id) #t))) (list a b))
                 (let ([model (rpc head 'model-create 'wire-value 1 'session 'transient '() "first")])
                   (define (checks) (call-with-input-file (string-append root "/model-checks") read))
                   (let ([view (rpc head 'view-create model 'value 1 '() 0)])
                     (for-each (lambda (ui) (head-read ui '(begin (kernel:load-module! "interaction") #t))) (list a b))
                     (test:check 'view-single-mount-owner
                       (map (lambda (ui) (head-read ui `(car (call-with-values (lambda () (interaction:claim! head:ui-actor ',view)) list))))
                         (list a b)) '(applied owned))
                     (test:check 'view-provisional-interaction-is-immediate
                       (head-read a `(begin
                                       (do ([n 1 (+ n 1)]) ((= n 101)) (interaction:set-state! head:ui-actor ',view 0 n))
                                       (list (let ([d (interaction:snapshot ',view)]) (list (view:sequence d) (view:basis d) (view:state d)))
                                             (view:state (view:snapshot ',view))))) '((100 0 100) 0))
                     (test:check 'view-publication-ack-does-not-roll-back-owner
                       (head-read a `(begin (interaction:flush!) (let ([d (interaction:snapshot ',view)]) (list (view:sequence d) (view:basis d) (view:state d))))) '(100 0 100))
                     (head-read a `(begin (interaction:release! head:ui-actor ',view 1) #t))
                     (test:check 'view-new-owner-restores-saved-state
                       (head-read b `(begin (interaction:claim! head:ui-actor ',view) (interaction:publish!)
                                            (let ([d (interaction:snapshot ',view)]) (list (view:sequence d) (view:basis d) (view:state d))))) '(0 0 100))
                     (head-read b `(begin (interaction:release! head:ui-actor ',view 2) #t)))
                   (for-each (lambda (ui)
                               (head-read ui
                                 `(begin (define model-events '())
                                         (define model-reader (model:subscribe! '(,model)
                                                                (lambda (event) (set! model-events (cons event model-events)))))
                                         (model:available? ',model)))) (list a b))
                   (let ([before (checks)])
                     (test:check 'model-shared-mirrors-are-owned-and-warm-reads-are-local
                       (head-read a
                         `(begin
                            (define second-reader (model:subscribe! '(,model) void))
                            (guard (ex [else (void)])
                              (kernel:call-with-registration-update
                                (lambda () (model:subscribe! '((model 999999)) void) (error 'rollback "rollback"))))
                            (do ([i 0 (+ i 1)]) ((= i 3)) (model:snapshot ',model) (model:available? ',model))
                            (let ([copy (model:snapshot ',model)])
                              (string-set! (cdr (assq 'value copy)) 0 #\X))
                            (model:unsubscribe! second-reader)
                            (list (cdr (assq 'value (model:snapshot ',model)))
                                  (guard (ex [else #t]) (model:snapshot '(model 999999)) #f)))) '("first" #t))
                     (test:check 'second-subscription-and-warm-reads-send-no-model-requests (checks) before))
                   (rpc head 'model-commit (list (list model 0 '() "remote")))
                   (for-each (lambda (ui)
                               (head-wait 'model-background-mirror ui
                                 (lambda () (equal? (head-read ui `(cdr (assq 'value (model:snapshot ',model)))) "remote")))) (list a b))
                   (rpc head 'model-retire model 1)
                   (for-each (lambda (ui)
                               (head-wait 'model-retirement ui (lambda () (not (head-read ui `(model:snapshot ',model)))))
                               (test:check 'model-last-subscriber-releases-mirror
                                 (head-read ui `(begin (model:unsubscribe! model-reader)
                                                       (guard (ex [else #t]) (model:snapshot ',model) #f))) #t)) (list a b)))
                 (include "tests/widgets-wire.sps")
                 (let* ([data (rpc head 'model-create 'wire-value 1 'session 'persistent '() "alpha\nbeta\ngamma")]
                        [root-view (head-read a '(view:create! head:ui-actor #f 'scroll 1 '((catalogue . #t) (name . "<wire tree>")) #f (test-window)))]
                        [first (head-read a `(view:create! head:ui-actor ',data 'text 2 '() 0 ',root-view))]
                        [second (head-read b `(view:create! head:ui-actor ',data 'text 2 '((catalogue . #t) (name . "<wire text>")) 0 (test-window)))]
                        [side (head-read a '(window-control:split! (test-window) 'right))]
                        [missing (head-read a `(view:create! head:ui-actor ',data 'not-installed 1 '((catalogue . #t) (name . "<missing>")) '(0 0) ',side))]
                        [composition
                         (head-read a
                           `(let* ([who head:ui-actor] [source (store:create! who "entry across heads" '("seed"))]
                                   [left (view:create! who source 'entry 1 '() '((0 . 0) (0 . 0)) ',root-view)]
                                   [right (view:create! who source 'entry 1 '() '((0 . 0) (0 . 0)) ',root-view)]
                                   [row (view:create! who #f 'row 1 '() '() ',root-view)]
                                   [column (view:create! who #f 'column 1 '() '() ',root-view)]
                                   [overlay (view:create! who #f 'overlay 1 '() '() ',root-view)]
                                   [root ',root-view])
                              (view:arrange! who
                                (list (list row 0 (list (list 'left left '(grow 1)) (list 'right right '(grow 1))) '())
                                      (list column 0 (list (list 'value ',first 'fit) (list 'fields row 'fit)) '())
                                      (list overlay 0 (list (list 'body column '(grow 1))) '())
                                      (list root 0 (list (list 'body overlay '(grow 1))) '((catalogue . #t) (name . "<wire tree>")))) '())
                              (list root left right source)))]
                        [left (cadr composition)] [right (caddr composition)] [source (cadddr composition)])
                   (head-read a `(begin (window-control:open-document! (test-window) ',root-view)
                                        (window-control:open-document! ',side ',missing) #t))
                   (head-read b `(begin (window-control:open-document! (test-window) ',second) #t))
                   (for-each (lambda (ui) (head-wait 'widget-mounted ui (lambda () (head-sees? ui "> alpha")))) (list a b))
                   (head-send! a "\x1b;[B")
                   (head-wait 'widget-keyboard-selection a (lambda () (head-sees? a "> beta")))
                   (test:check 'widget-selections-are-independent-across-heads
                     (list (head-read a `(view:state (interaction:snapshot ',first)))
                           (head-read b `(view:state (interaction:snapshot ',second)))) '(1 0))
                   (rpc head 'model-commit (list (list data 0 '() "alpha\nREMOTE beta\ngamma")))
                   (for-each (lambda (ui) (head-wait 'widget-remote-update ui (lambda () (head-sees? ui "REMOTE beta")))) (list a b))
                   (test:check 'widget-activation-carries-current-target-and-basis
                     (head-read a `(widget:act! ',first 'choose)) (list data 1 1 "REMOTE beta"))
                   (test:check 'widget-wheel-does-not-select-and-pointer-uses-shown-row
                     (head-read b
                       `(let* ([p (car (widget:shown))] [x (cadr p)] [y (caddr p)])
                          (widget:pointer! '(scroll 0 3 cells) x y)
                          (let ([before (view:state (interaction:snapshot ',second))])
                            (widget:pointer! '(pointer press primary ()) x (+ y 2))
                            (list before (view:state (interaction:snapshot ',second)))))) '(0 2))
                   (head-read a `(begin (widget:focus! ',root-view ',left) #t))
                   (head-send! a "\x1b;[200~local \x1b;[201~")
                   (head-wait 'nested-entry-paste a (lambda () (head-sees? a "local seed")))
                   (head-read b `(begin (store:edit! head:ui-actor (quote (unquote source)) (store:revision (quote (unquote source))) (text:make-span 0 0 0 0) (quote ("remote "))) #t))
                   (head-wait 'nested-entry-foreign-text a (lambda () (head-sees? a "remote local seed")))
                   (head-read a `(begin (entry:undo! ',left) (entry:select! ',right 3 1) (widget:focus! ',root-view ',right) #t))
                   (head-wait 'nested-entry-shared-undo a (lambda () (head-sees? a "remote seed")))
                   (test:check 'nested-entry-keeps-shared-text-and-independent-selection
                     (head-read a `(list (text-source:lines (text-source:lookup ',source))
                                         (view:state (interaction:snapshot ',right))
                                         (equal? (view:state (interaction:snapshot ',left)) (view:state (interaction:snapshot ',right)))))
                     '(#("remote seed") ((0 . 3) (0 . 1)) #f))
                   (let ([before (call-with-input-file (string-append root "/model-checks") read)])
                     (head-read a `(begin
                                     (do ([i 0 (+ i 1)]) ((= i 100)) (widget:prepare! ',root-view (+ 10 (mod i 7)) (+ 4 (mod i 3)))) #t))
                     (test:check 'warm-composition-resize-does-not-refetch-its-model
                       (call-with-input-file (string-append root "/model-checks") read) before))
                   (head-send! a "\x18;\x03;")
                   (head-wait 'widget-head-detached a (lambda () (pump-head! a)))
                   (test:await 'widget-owner-released (lambda () (not (cdr (assq 'owner (rpc head 'view-read first))))))
                   (rpc head 'edit source (cadr (rpc head 'snapshot source)) '(0 0 0 0) '("off "))
                   (set! a (start-head "screen A"))
                   (head-wait 'widget-resumed a
                     (lambda () (and (head-sees? a "> REMOTE beta") (head-sees? a "[Unavailable widget"))))
                   (test:check 'widget-resume-preserves-state-and-missing-renderer
                     (head-read a `(list (view:state (interaction:snapshot ',first)) (widget:actions ',missing))) '(1 ()))
                   (test:check 'nested-resume-restores-focus-and-advances-logical-selection
                     (head-read a `(list (widget:focused ',root-view) (view:state (interaction:snapshot ',right))
                                         (text-source:lines (text-source:lookup ',source))))
                     (list right '((0 . 7) (0 . 5)) '#("off remote seed")))
                   (test:check 'resumed-entry-edits-its-rebased-selection-after-detached-edits
                     (head-read a `(begin (entry:insert! ',right "X") (vector-ref (text-source:lines (text-source:lookup ',source)) 0)))
                     "off rXote seed")
                   (head-read a `(begin (window-control:keep! (test-window)) (test-show! ',id) (test-retire! ',root-view) #t))
                   (head-read
                     b
                     `(begin
                        (test-show! ',id) (test-retire! ',second)
                        #t)))
                 (test:check 'two-real-heads-use-client-services-and-local-tools
                   (map (lambda (client)
                          (head-read client
                            '(list head:ui-actor (actor:current)
                                   (let* ([app (cadr (window:find-app (test-manager) (test-window) "log"))]
                                          [d (view:snapshot app)])
                                     (list (cdr (assq 'name (view:options d))) (view:kind d)))
                                   (kernel:module-source "store")
                                   (kernel:module-requires? "main" "base")
                                   (guard (ex [else #t]) (kernel:reload-module! "store") #f)
                                   (guard (ex [else #t]) (actor:register! head:ui-actor (lambda (message) #f)) #f)))) (list a b))
                   (map (lambda (name)
                          (list (list 'head name) (list 'head name) '("<log>" log)
                                (string-append sources "/client/state/store.sls") #f #t #t)) '("screen A" "screen B")))
                 (test:check 'client-preparation-failures-preserve-connection-and-concurrent-replies
                   (head-read a
                     `(begin
                        (interaction:flush!)
                        (let ([saved (client:request 'checkpoint)] [cycle (list 'cycle)])
                          (set-cdr! cycle cycle)
                          (let* ([rejected (map (lambda (value)
                                                  (guard (ex [(client:ended? ex) (raise ex)] [else #t])
                                                    (client:request 'checkpoint value) #f)) (list void cycle))]
                                 [calls (list '(checkpoint) '(name ,id) (list 'checkpoint saved) '(name ,id))]
                                 [results (make-vector (length calls))]
                                 [workers (map (lambda (call i)
                                                 (fork-thread
                                                   (lambda ()
                                                     (vector-set! results i
                                                       (guard (ex [else (kernel:condition-text ex)])
                                                         (apply client:request call)))))) calls (iota (length calls)))])
                            (for-each thread-join workers)
                            (list rejected (equal? (vector->list results) (list saved "attached text" #t "attached text")))))))
                   '((#t #t) #t))
                 (let ([conflicted (rpc head 'create "coherent conflict" '("alpha tail")
                                        '((base . "alpha tail") (trailing . #f)))])
                   (define (review)
                     (head-read a
                       `(let-values ([(text revision conflicts) (store:conflict-state ',conflicted)])
                          (list text (map (lambda (c) (list-ref c 5)) conflicts)))))
                   (rpc head 'edit conflicted 0 '(0 0 0 5) '("mine"))
                   (rpc head 'reload conflicted '("disk tail") '((base . "disk tail") (trailing . #f)))
                   (head-read a `(begin (store:snapshot ',conflicted) #t))
                   (let ([cached (review)] [picked (rpc head 'conflicts conflicted)]
                         [omitted (car (rpc head 'conflict-state conflicted (cadr (rpc head 'snapshot conflicted))))])
                     (rpc head 'reload conflicted '("newdisk tail") '((base . "newdisk tail") (trailing . #f)))
                     (test:check 'client-conflict-snapshots-work-with-cached-and-newer-text
                       (list cached omitted (review))
                       '((#("disk tail") (("disk"))) #f (#("newdisk tail") (("newdisk")))))
                     (test:check 'attached-settlement-checks-the-reviewed-state-at-the-base
                       (head-read a
                         `(let ([stale (call-with-values (lambda () (store:resolve-picks! head:ui-actor ',conflicted ',picked '(1))) list)]
                                [fresh (store:conflicts ',conflicted)])
                            (let-values ([(status detail) (store:resolve-picks! head:ui-actor ',conflicted fresh '(1))])
                              (list stale status (store:line ',conflicted 0)))))
                       '((refused conflict-changed) applied "mine tail")))
                   (rpc head 'delete conflicted))
                 ;; Exercise the installed save hook, including first load,
                 ;; reload, inactive roots and pinned code.
                 (let* ([probe (string-append sources "/apps/layout-probe.sls")]
                        [ignored (list (cons (string-append sources "/base/state/layout-inactive.sls") '(state layout-inactive))
                                       (cons (string-append root "/layout-outside.sls") '(layout-outside)))])
                   (define (publish path library version)
                     ;; a module declares the library its place names, (kind name)
                     ;; under a root's kind directory, (name) flat
                     (write-forms path
                       `((library ,library (export init! value) (import (rnrs))
                           (define (value) ,version) (define (init!) (value))))))
                   (publish probe '(apps layout-probe) 1)
                   (for-each (lambda (entry) (publish (car entry) (cdr entry) 0)) ignored)
                   (let ([first (head-read a `(begin (file:run-post-save-hooks! ,probe #f)
                                                     (eval '(layout-probe:value))))])
                     (publish probe '(apps layout-probe) 2)
                     (test:check 'save-hook-follows-active-sls-roots-and-pinned-lifetimes
                       (list first
                             (head-read a
                               `(let ([before (top-level-value 'store:exists?)])
                                  (for-each (lambda (path) (file:run-post-save-hooks! path #f))
                                    (append ',(cons probe (map car ignored))
                                            (list (kernel:module-source "store"))))
                                  (list (eval '(layout-probe:value))
                                    (filter (lambda (name)
                                              (member name '("layout-probe" "layout-inactive" "layout-outside")))
                                            (kernel:loaded-modules))
                                    (eq? before (top-level-value 'store:exists?))))))
                       '(1 (2 ("layout-probe") #t))))
                   ;; Existing heads may reload; later heads compare the
                   ;; whole source tree. Retire this reload-only fixture.
                   (for-each delete-file (cons probe (map car ignored))))
                 (test:check 'client-log-delivery-stays-on-main-through-workers-reentry-and-retraction
                   (head-read a
                     '(let ([seen '()] [main-thread (get-thread-id)])
                        (parameterize ([kernel:registering-module 'wire-log-observer])
                          (log:subscribe!
                            (lambda (entry presentation)
                              (when (and (eq? (log:component entry) 'wire-log) (eq? (log:datum entry) 'first))
                                (log:add! 'wire-log 'second #f)
                                (error 'observer "test failure"))))
                          (log:subscribe!
                            (lambda (entry presentation)
                              (when (eq? (log:component entry) 'wire-log)
                                (set! seen (cons (list (log:datum entry) (actor:current) presentation
                                                       (= main-thread (get-thread-id))) seen))))))
                        (thread-join (fork-thread (lambda () (log:add! 'wire-log 'worker #f))))
                        (log:add! 'wire-log 'first #f)
                        (kernel:retract-module! 'wire-log-observer)
                        (log:add! 'wire-log 'after-retraction #f)
                        (list (reverse seen) (map log:datum (log:entries 'wire-log 2))
                              (let ([old (log:retention)])
                                (log:retention 5001)
                                (let ([new (log:retention)]) (log:retention old) (list old new)))
                              (let-values ([(entries end first) (log:snapshot 0 1 'wire-log)])
                                (list (map log:datum entries) (<= (- end first) (log:retention)))))))
                   '(((worker (head "screen A") #f #t)
                      (first (head "screen A") #f #t) (second (head "screen A") #f #t))
                     (after-retraction second) (5000 5001) ((after-retraction) #t)))
                 (head-send! a "A")
                 (head-wait 'foreign-paint-without-a-key b (lambda () (head-sees? b "Ashared text")))
                 (head-read b '(begin (test-go! (quote (0 . 12))) #t))
                 (head-send! b "\x1b;[200~ B\x1b;[201~")
                 (head-wait 'second-writer a (lambda () (head-sees? a "Ashared text B")))
                 (head-read a '(begin (edit:undo! (test-editor)) #t))
                 (head-wait 'undo-mine b (lambda () (head-sees? b "shared text B")))
                 (test:check 'attached-history-keeps-other-actors-and-rich-receipts
                   (list (car (rpc head 'snapshot id))
                         (head-read a '(test-point))
                         (map cadr (rpc head 'history id 3))
                         (let ([stamp (cdr (assq 'modified-at (caddr (rpc head 'snapshot id))))])
                           (map (lambda (screen)
                                  (= stamp (head-read screen '(store:property (test-document) (quote modified-at) #f)))) (list a b))))
                   '(#("shared text B") (0 . 0) ((head "screen A") (head "screen B") (head "screen A")) (#t #t)))
                 (head-read a '(parameterize ([edit:undo-scope 'all]) (edit:undo! (test-editor)) #t))
                 (test:check 'attached-explicit-other-actor-undo-and-requester-redo
                   (list (car (rpc head 'snapshot id))
                         (begin (head-read a '(begin (edit:redo! (test-editor)) #t)) (car (rpc head 'snapshot id))))
                   '(#("shared text") #("shared text B")))
                 (let ([ink (rpc head 'create "tint overlap" '("base"))])
                   (for-each
                     (lambda (screen)
                       (head-read screen `(begin (test-show! (quote (unquote ink))) #t)))
                     (list a b))
                   (head-send! b "\x1b;[200~FOREIGN\x1b;[201~")
                   (head-wait 'foreign-ink a (lambda () (head-sees? a "FOREIGNbase")))
                   (let ([before (map head-blame (list a b))])
                     (head-read a '(begin (test-go! (quote (0 . 3))) #t))
                     (head-send! a "X")
                     (head-wait 'own-ink-inside-foreign-range b (lambda () (head-sees? b "FORXEIGNbase")))
                     (test:check 'attached-tints-follow-the-author-after-overlap
                       (list before (map head-blame (list a b)) (car (rpc head 'snapshot ink)))
                       '(((((0 0 7)) (head "screen B")) (() (head "screen B")))
                         ((() (head "screen A")) (((0 3 4)) (head "screen A")))
                         #("FORXEIGNbase"))))
                   (for-each
                     (lambda (screen)
                       (head-read screen `(begin (test-show! ',id) #t)))
                     (list a b))
                   (rpc head 'delete ink))
                 (let ([ticket (reply-value (exchange agent
                                              '(request 71 ask (head "screen A") "Ready to continue?" ("yes" "no"))) 71)])
                   (head-send! a "\x03;a")
                   (head-wait 'base-question a (lambda () (head-sees? a "λ (edit:answer! ")))
                   (head-send! a "\"yes\"\r")
                   (test:check 'attached-questions-route-to-the-requesting-client-once
                     (list (receive-reply agent) (rpc agent 'cancel ticket)
                           (head-read a '(actor:pending head:ui-actor)))
                     '((event (answer 71 "yes")) #f ())))

                 ;; Questions refresh their own indicator as the pending
                 ;; table changes. Withdrawal must also leave another echo
                 ;; message or an active answer prompt in control.
                 (test:check 'question-withdrawal-refreshes-only-its-idle-indicator
                   (map
                     (lambda (state)
                       (head-read a '(begin (head:report! "") #t))
                       (let* ([first (rpc agent 'ask '(head "screen A") "First question?" '())]
                              [shown (head-wait 'first-question-indicator a
                                       (lambda () (head-sees? a "First question?")))]
                              [second (rpc agent 'ask '(head "screen A") "Second question?" '())])
                         (head-wait 'question-count-refreshes a
                           (lambda () (and (head-sees? a "First question?") (head-sees? a "(2 waiting)"))))
                         (rpc agent 'cancel first)
                         (head-wait 'oldest-question-refreshes a
                           (lambda () (and (head-sees? a "Second question?") (not (head-sees? a "(2 waiting)")))))
                         (case state
                           [(message)
                            (head-read a '(begin (head:report! "Keep this message") #t))
                            (head-wait 'unrelated-echo a (lambda () (head-sees? a "Keep this message")))]
                           [(prompt)
                            (head-send! a "\x03;a\"draft\"")
                            (head-wait 'answer-being-written a (lambda () (head-sees? a "(edit:answer! \"draft\"")))])
                         (pump-head! a)
                         (vector-set! a 3 "")
                         (rpc agent 'cancel second)
                         (head-wait (list 'withdrawal-frame state) a
                           (lambda ()
                             (and (> (occurrences (vector-ref a 3) "\x1b;[?2026l") 0)
                                  (case state
                                    [(idle) (not (head-sees? a "Second question?"))]
                                    [(message) (head-sees? a "Keep this message")]
                                    [(prompt) (head-sees? a "(edit:answer! \"draft\"")]))))
                         (when (eq? state 'prompt)
                           (head-send! a "\r")
                           ;; the answer arrives after the withdrawal: no question is pending any more
                           (head-wait 'withdrawn-answer a (lambda () (head-sees? a "Nothing to answer"))))
                         (head-read a '(actor:pending head:ui-actor))))
                     '(idle message prompt))
                   '(() () ()))

                 ;; Exercise the actual head/client adapters as well as the
                 ;; request envelope: a refused fact batch must stay false.
                 (let ([target (rpc head 'create "guarded facts" '("keep")
                                    '((base . "keep\n") (trailing . #t)))])
                   (head-read a `(begin (test-show! (quote (unquote target))) #t))
                   (rpc head 'properties target '((base . "other\n")))
                   (let ([before (rpc head 'snapshot target)])
                     (test:check 'attached-fact-and-merge-refusals-preserve-the-source
                       (list
                         (head-read a
                           '(let ([b (test-document)])
                              (list (store:set-properties! head:ui-actor b (quote ((base . "lost"))) (quote ((base . "keep\n"))) "lost name")
                                    (call-with-values
                                      (lambda () (store:edit! head:ui-actor b (store:revision b) (text:make-span 0 0 0 4) '("lost")
                                                   '(merge "merge" (undo) (commit (base . "lost")) (expected (base . "keep\n"))))) list))))
                         (rpc head 'edit target 0 '(0 0 0 4) '("lost")
                              '(merge "merge" (undo . ()) (commit . ((base . "lost"))) (expected . ((base . "keep\n")))))
                         (equal? before (rpc head 'snapshot target)) (rpc head 'history target)
                         (rpc head 'name target))
                       '((#f (stale property-changed)) (stale property-changed) #t () "guarded facts")))
                   (test:check 'attached-fresh-guards-commit-and-undo-keeps-the-baseline
                     (head-read a
                       '(let* ([b (test-document)]
                               [accepted (store:set-properties! head:ui-actor b (quote ((stamp . #f))) (quote ((base . "other\n") stamp)) "accepted facts")])
                          (store:edit! head:ui-actor b (store:revision b) (text:make-span 0 0 0 4) (quote ("disk")) (quote (merge "merge" (undo (trailing . #f)) (commit (base . "disk")) (expected (base . "other\n") (trailing . #t)))))
                          (let ([clean? (not (store:property b (quote modified) #f))])
                            (edit:undo! (test-editor))
                            (list accepted clean? (car (call-with-values (lambda () (store:snapshot b)) list)) (store:property b (quote base) #f)
                                  (store:property b (quote trailing) #f) (store:property b (quote modified) #f) (store:buffer-name b)))))
                     '(#t #t #("keep") "disk" #t #t "accepted facts"))
                   (head-read a `(begin (test-show! ',id) #t))
                   (rpc head 'delete target))

                 (for-each
                   (lambda (existing?)
                     (let ([path (string-append root (if existing? "/wire-open-existing.txt" "/wire-open-missing.txt"))])
                       (when existing? (write-text path "disk\n"))
                       (let ([target (head-read a
                                       `(begin (screen:open-file! (test-root) (unquote path)) (test-document)))])
                         (test:check (list existing? 'attached-file-opening-keeps-create-callback-work)
                           (let* ([screens
                                   (map (lambda (screen)
                                          (head-read screen
                                            `(begin (head:before-frame!)
                                                    (let* ([b (quote (unquote target))]
                                                           [seen (store:property b (quote open-observation) #f)])
                                                      (list (car seen) (cadr seen)
                                                        (map (lambda (key) (or (assq key (caddr seen)) key))
                                                          '(file base trailing mode mode-auto wrap modified))
                                                        (car (call-with-values (lambda () (store:snapshot b)) list)) (mode:name-of b) (store:property b (quote mode-auto) #f)
                                                        (store:property b (quote base) #f) (store:property b (quote modified) #f)))))) (list a b))]
                                  [authors (map cadr (rpc head 'history target))]
                                  [disk (and (file-exists? path) (call-with-input-file path get-string-all))])
                             (rpc head 'undo target 'all)
                             (let ([state (rpc head 'snapshot target)])
                               (list screens authors disk
                                     (list (car state) (cdr (assq 'modified (caddr state)))
                                           (cdr (assq 'mode-auto (caddr state)))))))
                           (list
                             (make-list 2
                               (list (if existing? '#("disk") '#("")) 0
                                     (list (cons 'file path) (cons 'base (if existing? "disk\n" ""))
                                           (cons 'trailing existing?) 'mode 'mode-auto 'wrap '(modified . #f))
                                     (if existing? '#("agent disk") '#("agent ")) "scheme" #f
                                     (if existing? "disk\n" "") #t))
                             '((agent "opening")) (if existing? "disk\n" (eof-object))
                             (list (if existing? '#("disk") '#("")) #f #f)))
                         (head-read a `(begin (test-show! ',id) #t))
                         (rpc head 'delete target))))
                   '(#t #f))

                 ;; Hold A's create reply while B visits the same canonical
                 ;; path and edits it. A stale candidate also crosses the wire.
                 (for-each
                   (lambda (existing?)
                     (let* ([name (if existing? "shared-visit-existing.txt" "shared-visit-missing.txt")]
                            [path (string-append root "/" name)])
                       (when existing? (write-text path "disk\n"))
                       (head-send! a (format "\x1b;xscreen:open-file! (test-root) ~s\r" path))
                       (head-wait 'first-visit-published a (lambda () (file-exists? open-held)))
                       (let* ([target (rpc head 'find-file path)]
                              [second (head-read b
                                        `(begin (screen:open-file! (test-root) (unquote (string-append root "/./" name)))
                                                (edit:insert! (test-editor) "B ") (test-document)))]
                              [before (rpc head 'snapshot target)] [history (rpc head 'history target)]
                              [reused (rpc head 'visit "stale candidate" '("lost")
                                           (list (cons 'file path) '(base . "lost\n") '(mode . #f)))]
                              [kept? (and (equal? reused (list target #f))
                                          (equal? before (rpc head 'snapshot target))
                                          (equal? history (rpc head 'history target)))])
                         (write-text open-release "continue")
                         (test:check (list existing? 'overlapping-head-visits-share-current-work)
                           (list (equal? target second) kept?
                                 (map (lambda (screen)
                                        (head-read screen
                                          '(begin (head:before-frame!)
                                             (let ([b (test-document)])
                                               (list b (car (call-with-values (lambda () (store:snapshot b)) list))
                                                     (store:property b (quote base) #f) (mode:name-of b)
                                                     (store:property b (quote mode-auto) #f)
                                                     (length (store:history b))))))) (list a b))
                                 (and (file-exists? path) (call-with-input-file path get-string-all)))
                           (list #t #t
                                 (make-list 2 (list target (if existing? '#("B callback disk") '#("B callback "))
                                                    (if existing? "disk\n" "") "scheme" #f 2))
                                 (if existing? "disk\n" (eof-object))))
                         (for-each (lambda (screen) (head-read screen `(begin (test-show! ',id) #t)))
                           (list a b))
                         (rpc head 'delete target))
                       (delete-file path)
                       (delete-file open-held) (delete-file open-release)))
                   '(#t #f))

                 (let* ([path (string-append root "/adoption.txt")]
                        [retarget (string-append root "/retargeted.ss")]
                        [target (rpc head 'create "before adoption" '("written")
                                     `((save-retarget . ,retarget) (read-only . #t) (disposable . #t)))])
                   (head-read a `(begin (test-show! (quote (unquote target))) #t))
                   (test:check 'attached-save-publishes-adoption-together-and-keeps-newer-callback-choices
                     (list (head-read a `(edit:save-file! (test-editor) (unquote path))) (call-with-input-file path get-string-all)
                           (map (lambda (screen)
                                  (head-read screen
                                    `(begin (head:before-frame!)
                                       (let ([b (quote (unquote target))])
                                         (list (store:buffer-name b) (store:property b (quote file) #f) (store:property b (quote base) #f)
                                               (mode:name-of b) (store:property b (quote mode-auto) #f)
                                               (car (call-with-values (lambda () (store:snapshot b)) list)) (store:property b (quote modified) #f)))))) (list a b))
                           (cdr (assq 'save-observation (caddr (rpc head 'snapshot target))))
                           (file-exists? retarget))
                     (list #t "written\n"
                           (make-list 2 (list "retargeted.ss" retarget "new baseline\n" "scheme" #f '#("written") #t))
                           (list "adoption.txt" path "written\n" #f #t #f #f #f) #f))
                   (head-read a `(begin (test-show! ',id) #t))
                   (rpc head 'delete target))

                 (let ([path (string-append root "/saved.txt")])
                   (write-text path "shared text B\n")
                   (head-read a
                     `(begin
                        (store:set-properties! head:ui-actor (test-document) (quote ((file unquote path) (base . "shared text B\n"))))
                        (parameterize ([kernel:registering-module 'wire-save-hook])
                          (file:add-pre-save-hook!
                            (lambda (target editor)
                              (when (string=? target ,path) (file:write! target '#("from hook") #t))))) #t))
                   (head-read a `(edit:save-file! (test-editor) ,path))
                   ;; the hook's write is what the disk holds at save time: the save
                   ;; reloads it, the buffer clean against its baseline, then writes
                   (head-wait 'a-pre-save-hooks-disk-write-is-reloaded a
                     (lambda () (equal? (car (rpc head 'snapshot id)) '#("from hook"))))
                   (test:check 'the-hooks-write-is-reloaded-then-written
                     (list (head-read a '(begin (kernel:retract-module! 'wire-save-hook)
                                                (store:set-properties! head:ui-actor (test-document) (quote ((file . #f) (base . #f)))) #t))
                           (call-with-input-file path get-string-all) (car (rpc head 'snapshot id)))
                     '(#t "from hook\n" #("from hook")))
                   ;; Nothing asks about a file changed on disk: a visit merges the
                   ;; disk's changes, or rereads the disk where the log does not reach
                   ;; the baseline, undoably; a save reloads first and, where it cannot
                   ;; merge, rereads instead and refuses, the buffer's text kept in the
                   ;; log for undo to bring back
                   (for-each
                     (lambda (command)
                       (write-text path "disk\n")
                       (let ([target (rpc head 'create "file review" '("keep")
                                          `((file . ,path) (base . "base\n") (trailing . #t)))])
                         (head-read a
                           `(begin (test-show! (quote (unquote target)))
                                   (edit:insert! (test-editor) "mine ") #t))
                         (let ([accepted
                                (head-read a
                                  `(guard (ex [(kernel:refusal? ex)
                                               (and (eq? ',command 'edit:save-file!)
                                                 (string:search (kernel:condition-text ex) "was reread" 0
                                                   (string-length (kernel:condition-text ex))) #t)])
                                     (,command (,(if (eq? command 'edit:save-file!) 'test-editor 'test-root)) ,path)))])
                           (test:check (list command 'a-changed-file-past-the-log-is-reread-undoably)
                             (list accepted (head-read a '(let ([b (test-document)])
                                                            (list (car (call-with-values (lambda () (store:snapshot b)) list)) (store:property b (quote modified) #f) (store:property b (quote conflicted) #f))))
                               (call-with-input-file path get-string-all)
                               (head-read a '(begin (edit:undo! (test-editor)) (car (call-with-values (lambda () (store:snapshot (test-document))) list)))))
                             '(#t (#("disk") #f #f) "disk\n" #("mine keep"))))
                         (head-read a `(begin (test-show! ',id) #t))
                         (rpc head 'delete target)))
                     '(screen:open-file! edit:save-file!))
                   ;; saving under a name a file holds asks nothing: what the file held
                   ;; is kept first as a backup, a trashed buffer named after it with
                   ;; .bak, and restore! brings it back
                   (write-text path "disk\n")
                   (let ([target (rpc head 'create "file review" '("keep") '((trailing . #t)))])
                     (head-read a `(begin (test-show! (quote (unquote target))) (edit:insert! (test-editor) "mine ") #t))
                     (head-read a `(edit:save-file! (test-editor) ,path))
                     (test:check 'a-save-as-over-a-file-keeps-what-it-held-as-a-backup
                       (list (call-with-input-file path get-string-all)
                             ;; the newest backup of the path, its name saved.txt.bak or a suffixed one
                             (head-read a `(let ([kept (find (lambda (b) (equal? (cadr b) ,path)) (edit:backups))])
                                             (and kept (string? (list-ref kept 4))
                                                  (let* ([row (find (lambda (row) (equal? (cdr (assq 'name (cadr row))) (car kept)))
                                                                (cadr (store:metadata)))]
                                                         [id (car row)])
                                                    (store:archive! head:ui-actor id (cdr (assq 'version (cadr row))) 'restore)
                                                    (test-show! id) (vector->list (car (call-with-values (lambda () (store:snapshot id)) list))))))))
                       '("mine keep\n" ("disk")))
                     (head-read a `(begin (test-show! ',id) #t))
                     (rpc head 'delete target)))

                 ;; A recovered visiting buffer may have no disk baseline.
                 ;; Saving then rereads the disk undoably and refuses; undo
                 ;; brings the text back to save. New visits now create a baseline.
                 (let* ([path (string-append root "/missing-save.txt")]
                        [target (rpc head 'create "missing-save.txt" '("") (list (cons 'file path) '(trailing . #t)))])
                   (head-read a `(begin (test-show! (quote (unquote target))) (edit:insert! (test-editor) "mine") #t))
                   (write-text path "disk\n")
                   ;; Inspect the refusal itself: its echo may wrap anywhere
                   ;; as paths and qualified source names vary in length.
                   (test:check 'no-ancestor-rereads
                     (head-read a `(guard (ex [else
                                               (and (string:search (kernel:condition-text ex) "was reread" 0
                                                      (string-length (kernel:condition-text ex))) #t)])
                                     (edit:save-file! (test-editor) (unquote path)) #f)) #t)
                   (test:check 'a-save-without-a-baseline-rereads-and-undo-restores-the-text-to-save
                     (list (head-read a '(car (call-with-values (lambda () (store:snapshot (test-document))) list)))
                           (call-with-input-file path get-string-all)
                           (head-read a `(begin (edit:undo! (test-editor)) (edit:save-file! (test-editor) (unquote path)) (car (call-with-values (lambda () (store:snapshot (test-document))) list))))
                           (call-with-input-file path get-string-all))
                     '(#("disk") "disk\n" #("mine") "mine\n"))
                   (head-read a `(begin (test-show! ',id) #t))
                   (rpc head 'delete target)
                   (delete-file path))

                 ;; C-x k kills at once: a document with unsaved work goes to the
                 ;; trash, hidden from every head with its text and history kept,
                 ;; and restore! brings it back under its name.
                 (let ([doomed (rpc head 'create "kill review" '("work"))])
                   (rpc head 'edit doomed 0 '(0 4 0 4) '("!"))
                   (head-read a `(begin (test-show! (quote (unquote doomed))) #t))
                   (head-send! a "\x18;k")
                   (head-wait 'kill-trashes-unsaved-work a (lambda () (head-sees? a "its unsaved work is in the trash")))
                   (test:check 'a-trashed-buffer-is-hidden-but-kept
                     (head-read a `(list (store:exists? ',doomed) (store:visible? head:ui-actor ',doomed)
                                         (map car (edit:trash))))
                     '(#t #f ("kill review")))
                   (test:check 'restore-brings-the-buffer-back-with-its-text
                     (head-read a '(let ([b (edit:restore! "kill review")])
                                     (list (store:buffer-name b) (vector->list (car (call-with-values (lambda () (store:snapshot b)) list))) (edit:trash))))
                     '("kill review" ("work!") ()))
                   (rpc head 'delete doomed))

                 ;; Reuse this head and base for the root protocol. The launcher
                 ;; still starts the default host; installation takes the same
                 ;; pump and terminal output path over at a command boundary.
                 (test:check 'window-service-uses-the-shared-registered-operation-contract
                   (head-read b
                     '(let* ([manager (window:create-manager! #f)] [first (window:current manager)]
                             [second (window:split! manager first 'right)]
                             [text (store:create! head:ui-actor "wire placement" '("borrowed text"))]
                             [app (view:create! head:ui-actor text 'label 1 '((name . "<wire app>") (catalogue . #t)) '() second)])
                        (window:select! manager second)
                        (window:link! manager first second 'follow)
                        (window:open-document! manager second text)
                        (window:open-document! manager second app)
                        (let ([before (list (equal? (window:list manager) (list first second))
                                        (equal? (window:document manager second) app)
                                        (equal? (window:documents manager second) (list app text))
                                        (equal? (window:current manager) second)
                                        (equal? (window:numbered manager 2) second)
                                        (equal? (window:links manager) (list (list first second 'follow))))])
                          (let ([after (list (begin (window:return! manager second app)
                                               (equal? (window:document manager second) text))
                                         (window:close! manager second)
                                         (null? (window:links manager))
                                         (equal? (window:current manager) first)
                                         (and (not (view:snapshot app)) (string=? (store:line text 0) "borrowed text")))])
                            (store:delete! head:ui-actor text)
                            (append before after)))))
                   '(#t #t #t #t #t #t #t #t #t #t #t))
                 (head-read b '(begin (kernel:load-module! "root") (kernel:load-module! "window-control") #t))
                 (let* ([pair
                         (head-read b
                           '(begin
                              (let* ([source (store:create! head:ui-actor "root text" '("Root editor"))]
                                     [view (window:create-manager! #f)])
                                (root:install! (root:current) view 'retire)
                                (list view source))))]
                        [view (car pair)] [source (cadr pair)])
                   (head-wait 'root-client-admits-empty-window b
                     (lambda () (head-read b `(equal? (map (lambda (shown) (widget:frame-id (car shown))) (widget:shown)) (list ',view)))))
                   ;; Adding a document after mounting must acquire its subtree
                   ;; and source from the base without a head-side arrange.
                   (head-read b `(begin (window-control:open-document! (window:current ',view) ',source) #t))
                   (head-wait 'root-client-adopts-registered-operation b (lambda () (head-sees? b "Root editor")))
                   (head-send! b "Z")
                   (head-wait 'root-input-uses-recursive-route b (lambda () (head-sees? b "ZRoot editor")))
                   (test:check 'root-client-input-and-canonical-binding-agree
                     (list (car (rpc head 'snapshot source))
                       (head-read b '(cdr (assq 'root (cdr (assq 'value (root:current)))))))
                     (list '#("ZRoot editor") view))
                   (rpc head 'edit source (cadr (rpc head 'snapshot source)) '(0 0 0 0) '("REMOTE "))
                   (head-wait 'root-client-borrowed-text-updates-without-window-sync b
                     (lambda () (head-sees? b "REMOTE ZRoot editor")))
                   (head-read b '(begin (root:install! (root:current) #f 'retire) #t))
                   (head-wait 'root-client-empty-frame b (lambda () (head-read b '(null? (widget:shown)))))
                   (test:check 'root-client-retirement-keeps-borrowed-text
                     (list (rpc head 'view-read view) (car (rpc head 'snapshot source))) '(#f #("REMOTE ZRoot editor"))))

                 (stop!)
                 (for-each (lambda (head)
                             (head-wait 'head-restores-terminal-after-disconnect head
                               (lambda ()
                                 (and (> (occurrences (vector-ref head 3) "\x1b;[?1049l") 0)
                                   (let ([state (vt:emulator-state (vector-ref head 2))])
                                     (not (or (cdr (assq 'mouse-tracking state))
                                            (cdr (assq 'sgr-mouse state)))))))))
                   ;; SIGKILL cannot run terminal cleanup; all cooperative exits can.
                   (filter (lambda (head) (not (memq head killed-heads))) heads))
                 (void)))))))
     (define (resume-scenarios!)
       ;; The protocol configuration holds (agent "first")'s first edit until
       ;; this file exists; that barrier belongs to the protocol group.
       (write-text edit-release "continue")
       (call-with-protocol-base
         (lambda (base pid signal! stop!)
           (let ([head (connect)] [agent (connect)])
             (hello head '(head "desk λ")) (receive head)
             (hello agent '(agent "reader")) (receive agent)
             (let ([id (rpc head 'create "attached text" '("shared text B") '((trailing . #t)))])
               (write-forms (string-append root "/config.e")
                 `((void) (client:inbox-limits (cons 48 262144))
                   (head:run-on-main! (lambda () (head:run-on-main!
                                                   (lambda () (screen:open-document! (cdr (assq 'root (cdr (assq 'value (root:current))))) ',id)))))))
               (let* ([a (start-head "screen A")]
                      [ready-a (head-wait 'first-real-head a (lambda () (head-sees? a "shared text")))]
                      [b (start-head "screen B")])
                 (head-wait 'second-real-head b (lambda () (head-sees? b "shared text")))
                 (write-forms (string-append root "/config.e") '((client:inbox-limits (cons 48 262144))))
                 (let ([scroll (rpc head 'create "scrolling"
                                    (map (lambda (n) (format "row ~3,'0d" n)) (iota 80)))])
                   (define (frame-complete?)
                     (let ([frames (vector-ref a 3)])
                       (and (> (occurrences frames "\x1b;[?2026h") 0)
                            (= (occurrences frames "\x1b;[?2026h")
                               (occurrences frames "\x1b;[?2026l")))))
                   (head-read a `(begin (test-show! (quote (unquote scroll)))
                                        (window-control:split! (test-window) (quote right)) #t))
                   ;; A PTY read may end between row text and the frame's
                   ;; closing marker. Both capture boundaries need a frame.
                   (head-wait 'split-before-scroll a (lambda () (and (head-sees? a "scrolling") (frame-complete?))))
                   (vector-set! a 3 "")
                   (head-send! a (apply string-append (make-list 30 "\x1b;[B")))
                   (head-wait 'held-down-scroll a
                     (lambda () (and (head-sees? a "row 030") (head-sees? a "L31 C1") (frame-complete?))))
                   (let* ([frames (vector-ref a 3)] [opened (occurrences frames "\x1b;[?2026h")]
                          [closed (occurrences frames "\x1b;[?2026l")])
                     (test:check 'attached-split-scrolling-uses-balanced-2026
                       (list (> opened 0) (= opened closed) (head-read a '(test-point))) '(#t #t (30 . 0))))
                   ;; The attached head also services resize while idle.
                   ;; Observe geometry and the pending question's new width
                   ;; before any more input; the other head keeps its size.
                   (head-read a '(begin (head:report! "") #t))
                   (let* ([question "This pending question expands after resize, without another key."]
                          [ticket (rpc agent 'ask '(head "screen A") question '())])
                     (head-wait 'attached-resize-question-starts-elided a
                       (lambda () (and (head-sees? a "This pending que") (not (head-sees? a question)))))
                     (vector-set! a 3 "")
                     (vt:emulator-resize! (vector-ref a 2) 18 120)
                     (sys:resize-terminal-process! (vector-ref a 0) 18 120)
                     ;; The message widget uses its allocation, without a
                     ;; fixed-width echo box or another input loop.
                     (head-wait 'attached-idle-resize-refits-question a
                       (lambda ()
                         (and (head-sees? a question)
                              (let ([frames (vector-ref a 3)])
                                (and (> (occurrences frames "\x1b;[?2026h") 0)
                                     (= (occurrences frames "\x1b;[?2026h")
                                        (occurrences frames "\x1b;[?2026l")))))))
                     (rpc agent 'cancel ticket))
                   (test:check 'attached-resize-keeps-the-other-head-size
                     (head-read b '(list (tui:screen-rows) (tui:screen-cols))) '(24 80))
                   (vt:emulator-resize! (vector-ref a 2) 24 80)
                   (sys:resize-terminal-process! (vector-ref a 0) 24 80)
                   (head-read a `(begin (window-control:keep! (test-window))
                                        (test-show! ',id) #t))
                   (rpc head 'delete scroll))
                 (test:check 'attached-private-doc-source-and-local-rendering
                   (head-read a
                     '(let* ([names (list 'attached-document)] [body (string-copy "Original head documentation")]
                             [data (list names '(("procedure" . "(attached-document)"))
                                         #f '() 'fixture "Attached" #f body)])
                        (parameterize ([kernel:registering-module 'attached-document])
                          (doc:register! (list data)))
                        (set-car! names 'changed-document)
                        (string-set! body 0 #\X)
                        (let* ([receiver (describe:show! 'attached-document)]
                               [page (reference:page head:ui-actor receiver)]
                               [source (car page)]
                               [entry (car (reference:lookup 'attached-document))])
                          (string-set! (doc:description entry) 0 #\Y)
                          (list (store:property source (quote audience) #f)
                                (store:property source (quote read-only) #f)
                                (doc:description entry)
                                (and (member "Original head documentation" (vector->list (car (call-with-values (lambda () (store:snapshot source)) list)))) #t)))))
                   '(((head "screen A")) #t "Original head documentation" #t))
                 (test:check 'attached-document-scope-and-retraction
                   (list (head-read b '(reference:lookup 'attached-document))
                         (head-read a
                           '(begin (kernel:retract-module! 'attached-document)
                              (describe:show! 'describe:show!)
                              (reference:lookup 'attached-document))))
                   '(() ()))
                 (head-read a '(begin (terminal:open! (test-window) "printf 'attached terminal'; read answer; printf '\\n%s' \"$answer\"; read done") #t))
                 (head-wait 'attached-terminal a (lambda () (head-sees? a "attached terminal")))
                 (let ([terminal-id (head-read a '(test-document))])
                   (head-read b `(begin (test-show! (quote (unquote terminal-id))) #t))
                   (head-wait 'shared-terminal-surface b (lambda () (head-sees? b "attached terminal")))
                   (head-read a '(begin (terminal:send! (widget:descendant (test-window) (quote document)) "through base\n") #t))
                   (head-wait 'shared-terminal-input b (lambda () (head-sees? b "through base")))
                   (test:check 'terminal-output-keeps-authorship-without-tints
                     (map head-blame (list a b))
                     (make-list 2 (list '() (cdr (assq 'app (caddr (rpc head 'snapshot terminal-id)))))))
                   (head-read a '(begin (window-control:keep! (test-window)) (edit:copy-text! "screen A kill") #t))
                   (head-read b '(begin (edit:copy-text! "screen B kill")
                                        (terminal:toggle-capture! (widget:descendant (test-window) (quote document))) #t))
                   (test:check 'shared-terminal-capture-is-local-to-each-head
                     (list (head-read a '(eq? 'full (car (view:state (view:snapshot (widget:descendant (test-window) (quote document)))))))
                           (and (head-sees? a "▶ ◐") #t) (and (head-sees? b "▶ ●") #t)) '(#f #t #t))
                   (head-send! a "\x18;\x03;")
                   (head-wait 'real-head-detaches a
                     (lambda () (not (member '(head "screen A") (map car (rpc head 'actors))))))
                   (test:check 'detach-preserves-dirty-text-and-live-base-terminal
                     (list (car (rpc head 'snapshot id))
                           (cdr (assq 'alive (caddr (rpc head 'snapshot terminal-id))))
                           (and (member '(head "screen B") (map car (rpc head 'actors))) #t))
                     '(#("shared text B") #t #t))
                   ;; Both real heads and the control head leave. Two
                   ;; independent clients coordinate edits and questions;
                   ;; only their connections own their outstanding requests.
                   (let ([first (connect)] [second (connect)])
                     (for-each (lambda (connection who) (hello connection who) (receive connection))
                       (list first second) '((agent "first") (agent "second")))
                     ;; Lose SSH with full capture. A clean keyboard detach
                     ;; would first switch to partial capture and save that choice.
                     (unless (zero? (system (format "kill -KILL ~a" (sys:terminal-process-pid (vector-ref b 0)))))
                       (error 'wire-head "could not terminate fixture head"))
                     (set! killed-heads (cons b killed-heads))
                     (sys:close-connection! head)
                     (test:await 'every-human-head-is-absent
                       (lambda () (not (exists (lambda (entry) (eq? (caar entry) 'head)) (rpc agent 'actors)))))
                     (let* ([basis (cadr (rpc first 'snapshot id))]
                            [edited (rpc first 'edit id basis '(0 0 0 0) '("agent "))]
                            [sent? (rpc first 'mail '(agent "second") (list 'review id (car (cadr edited))))]
                            [message (receive-reply second)]
                            [task (caddr (cadr message))]
                            [reviewed (rpc second 'edit (cadr task) (caddr task) '(0 0 0 0) '("reviewed "))])
                       (test:check 'scripted-agents-cooperate-with-every-head-absent
                         (list (map (lambda (connection) (rpc connection 'owner)) (list first second))
                               (car edited) sent? (list-head (cadr message) 2) (car task)
                               (car reviewed) (car (rpc agent 'snapshot id))
                               (map cadr (rpc agent 'history id 2))
                               (cdr (assq 'alive (caddr (rpc agent 'snapshot terminal-id)))))
                         '(((head "screen A") (head "screen A")) applied #t (message (agent "first")) review
                           applied #("reviewed agent shared text B") ((agent "second") (agent "first")) #t)))
                     (let ([ticket (reply-value (exchange second
                                                  '(request 72 ask (agent "first") "Review complete?" ("yes"))) 72)])
                       (test:check 'peer-questions-bind-the-answerer-and-the-cancelling-session
                         (list (receive-reply first) (rpc second 'answer ticket "wrong")
                               (rpc first 'cancel ticket) (rpc first 'answer ticket "yes")
                               (receive-reply second) (rpc first 'answer ticket "again"))
                         (list (list 'event (list 'ask ticket '(agent "second") "Review complete?" '("yes")))
                               #f #f #t '(event (answer 72 "yes")) #f)))
                     ;; Restore the shared text through each agent's own
                     ;; history, leaving the existing screen-resume checks
                     ;; on their original text while retaining attribution.
                     (rpc second 'undo id)
                     (rpc first 'undo id)
                     (let ([question (reply-value (exchange second '(request 73 ask "Continue agent work?" ("yes" "no"))) 73)]
                           [abandoned (rpc first 'ask "Withdraw when I disconnect?" '())])
                       (test:check 'offline-owner-questions-are-retained-without-queueing-ordinary-mail
                         (list (number? question) (number? abandoned)
                               (rpc first 'cancel question)
                               (rpc first 'mail '(head "screen A") 'unreachable)
                               (rpc first 'ask '(head "never attached") "Unknown?" '()))
                         '(#t #t #f #f #f))
                       (sys:close-connection! first)
                       (test:await 'departing-agent-revoked-and-detached
                         (lambda () (not (assoc '(agent "first") (rpc second 'actors)))))
                       (let ([replacement (connect)])
                         (hello replacement '(agent "first")) (receive replacement)
                         (test:check 'agent-name-reuse-cannot-revive-or-consume-outstanding-requests
                           (list (rpc replacement 'cancel abandoned) (rpc replacement 'cancel question)
                                 (rpc replacement 'answer question "forged")) '(#f #f #f))
                         (sys:close-connection! replacement))
                       (set! head (connect))
                       (hello head '(head "desk λ")) (receive head)
                       (set! b (start-head "screen B"))
                       (head-wait 'named-head-restores-full-capture b
                         (lambda () (and (head-sees? b "through base") (head-sees? b "▶ ●"))))
                       (head-send! b "\x1d;")
                       (head-wait 'restored-capture-remains-toggleable b (lambda () (head-sees? b "▶ ◐")))
                       (set! a (start-head "screen A"))
                       (head-wait 'owner-returns-to-offline-question a (lambda () (head-sees? a "through base")))
                       (test:check 'returning-owner-sees-only-the-live-session-question
                         (head-read a '(actor:pending head:ui-actor))
                         (list (list question '(agent "second") "Continue agent work?" '("yes" "no"))))
                       (head-send! a "\x1b;xedit:answer! \"yes\"\r")
                       (test:check 'reattached-human-answers-the-surviving-agent-once
                         (list (receive-reply second) (rpc second 'cancel question)
                               (rpc head 'answer question "wrong head")
                               (head-read a '(actor:pending head:ui-actor)))
                         '((event (answer 73 "yes")) #f #f ())))
                     ;; A real human head controls the existing session
                     ;; inventory, including agents routed to another head.
                     ;; Revocation wakes an idle reader and removes watches
                     ;; through the ordinary connection cleanup.
                     (head-read a '(begin (head:report! "") #t))
                     (let* ([question (rpc second 'ask "Withdraw on revoke?" '())]
                            [watched (rpc second 'watch)]
                            [shown (head-wait 'question-before-revocation a
                                     (lambda () (head-sees? a "Withdraw on revoke?")))]
                            [selected
                             (head-read b
                               '(list (assoc '(agent "second") (client:request 'sessions))
                                      (client:request 'revoke '(agent "second"))))])
                       (test:await 'human-revocation-retracts-the-endpoint
                         (lambda () (not (assoc '(agent "second") (rpc agent 'actors)))))
                       (head-wait 'revoked-question-disappears-while-idle a
                         (lambda () (not (head-sees? a "Withdraw on revoke?"))))
                       (test:check 'human-revocation-closes-the-agent-and-preserves-other-clients
                         (list selected (eof-object? (receive-reply second))
                               (head-read a `(list (actor:pending head:ui-actor)
                                                   (actor:answer! ,question "too late")))
                               (head-read b '(client:request 'revoke '(agent "second")))
                               (rpc agent 'eval "(+ 1 2)") (car (rpc head 'snapshot id))
                               (cdr (assq 'alive (caddr (rpc head 'snapshot terminal-id)))))
                         '((((agent "second") (head "screen A")) 1) #t (() #f) 0
                           (ok . "=> 3") #("shared text B") #t))
                       (let ([replacement (connect)])
                         (hello replacement '(agent "second")) (receive replacement)
                         (test:check 'explicit-revocation-does-not-change-future-admission
                           (list (rpc replacement 'eval "(+ 1 2)")
                                 (rpc replacement 'owner) (rpc replacement 'cancel question))
                           '((ok . "=> 3") (head "screen A") #f))
                         (sys:close-connection! replacement))))
                   (let ([again a])
                     (head-wait 'real-head-reattaches again (lambda () (head-sees? again "through base")))
                     (test:check 'clean-reattach-restores-the-terminal-and-reuses-scratch
                       (list (head-read again '(list (test-document)
                                                     (length (window:list (test-manager))) (edit:copy-text)))
                             (filter (lambda (name) (string:prefix? "*scratch*" name))
                               (map (lambda (id) (rpc head 'name id)) (rpc head 'buffers))))
                       (list (list terminal-id 1 "screen A kill") '("*scratch*")))
                     (let ([plain (rpc head 'create "resume lines"
                                       (map (lambda (n) (format "resume ~3,'0d" n)) (iota 80)))]
                           [source (rpc head 'create "resume.md"
                                        '("|alpha beta gamma delta epsilon|x|" "|-|-|" "|long entry|y|"
                                          "" "# After table" "" "# Tail"))])
                       (head-read again
                         `(begin
                            (test-show! (quote (unquote plain)))
                            (window-control:split! (test-window) (quote right)) (window-control:navigate! (test-window) (quote next))
                            (let ([source (quote (unquote source))])
                              (mode:choose! "markdown" source)
                              (test-show! source) (test-go! (quote (4 . 0)))
                              (markdown:view! (test-window)))
                            (window-control:navigate! (test-window) (quote next)) (window:set-display! (test-manager) (test-window) (list (cons (quote wrap) #f))) (window-control:split! (test-window) (quote below))
                            (let* ([manager (test-manager)] [rest (cadar (view:children (interaction:snapshot manager)))]
                                   [children (view:children (interaction:snapshot rest))]
                                   [inner (cadar children)])
                              (window:resize! manager rest (map cadr children) '(2 3))
                              (window:resize! manager inner (map cadr (view:children (interaction:snapshot inner))) '(2 1)))
                            (widget:pump!)
                            (widget:prepare! (test-root) (tui:screen-cols) (tui:screen-rows))
                            (edit:select! (test-editor) '(25 . 3) '(26 . 4))
                            (let ([other (widget:descendant (window:numbered (test-manager) 3) 'document)])
                              (edit:select! other '(50 . 4) '(50 . 4)))
                            (interaction:flush!) #t))
                       (head-wait 'markdown-before-loss again (lambda () (head-sees? again "After table")))
                       (let ([before (screen-state again)]
                             [markdown (head-read again '(window:document (test-manager) (window:numbered (test-manager) 2)))])
                         ;; Completion opens the pop-up window. A wake
                         ;; in that modal loop must not checkpoint its chrome.
                         ;; The first Tab only counts matches; the second lists them.
                         (head-send! again "\x1b;xwindow:\t\t")
                         ;; Observe the completion status, not a particular
                         ;; candidate: public API/docs change the page breaks.
                         (head-wait 'completions-before-loss again
                           (lambda () (head-sees? again "matches of symbol")))
                         (vector-set! again 3 "")
                         (rpc head 'properties plain '((fixture-wake . #t)))
                         (head-wait 'wake-inside-prompt again
                           (lambda () (> (occurrences (vector-ref again 3) "\x1b;[?2026l") 0)))
                         (unless (zero? (system (format "kill -KILL ~a" (sys:terminal-process-pid (vector-ref again 0)))))
                           (error 'wire-head "could not terminate fixture head"))
                         (set! killed-heads (cons again killed-heads))
                         (test:await 'abrupt-head-loss
                           (lambda () (not (member '(head "screen A") (map car (rpc head 'actors))))))
                         (rpc head 'edit plain 0 '(0 0 0 0) '("offline" ""))
                         (rpc head 'edit source (cadr (rpc head 'snapshot source)) '(0 0 0 0) '("# Offline" "" ""))
                         (rpc head 'rename source "renamed resume.md")
                         (let ([truth (map (lambda (id) (rpc head 'snapshot id)) (list plain source))]
                               [resumed (start-head "screen A" 52)])
                           (head-wait 'screen-resumes-at-new-width resumed (lambda () (head-sees? resumed "resume 025")))
                           (head-wait 'markdown-after-loss resumed (lambda () (head-sees? resumed "After table")))
                           (test:check 'abrupt-reattach-rebuilds-layout-and-source-anchors
                             (list (screen-state resumed)
                                   (map (lambda (id) (rpc head 'snapshot id)) (list plain source)))
                             (list before truth))
                           (void)
                           (head-send! resumed "\x18;\x03;")
                           (head-wait 'detach-before-input-removal resumed
                             (lambda () (not (member '(head "screen A") (map car (rpc head 'actors))))))
                           (rpc head 'properties plain '((audience)))
                           (rpc head 'delete source)
                           (let ([fallback (start-head "screen A")])
                             (head-wait 'missing-input-fallback fallback (lambda () (head-sees? fallback "Markdown source unavailable")))
                             (test:check 'missing-documents-fall-back-and-unavailable-widget-keeps-its-identity
                               (list (head-read fallback '(map (lambda (w) (window:document (test-manager) w))
                                                               (window:list (test-manager))))
                                     (head-read b '(list (length (window:list (test-manager)))
                                                         (test-document) (edit:copy-text))))
                               (list (list terminal-id #f markdown) (list 1 terminal-id "screen B kill")))))))))
               )))
           (stop!)
           (for-each (lambda (head)
                       (head-wait 'head-restores-terminal-after-disconnect head
                         (lambda ()
                           (and (> (occurrences (vector-ref head 3) "\x1b;[?1049l") 0)
                                (let ([state (vt:emulator-state (vector-ref head 2))])
                                  (not (or (cdr (assq 'mouse-tracking state))
                                           (cdr (assq 'sgr-mouse state)))))))))
             ;; SIGKILL cannot run terminal cleanup; all cooperative exits can.
             (filter (lambda (head) (not (memq head killed-heads))) heads))
           (void))))
     (define (overload-scenarios!)
       ;; A paused head's inbox fills: both budgets close only that head.
       (call-with-protocol-base
         (lambda (base pid signal! stop!)
           (let ([head (connect)])
             (hello head '(head "desk λ")) (receive head)
             (let ([id (rpc head 'create "attached text" '("shared text B") '((trailing . #t)))])
               (write-forms (string-append root "/config.e")
                 `((void) (client:inbox-limits (cons 48 262144))
                   (head:run-on-main! (lambda () (head:run-on-main!
                                                   (lambda () (screen:open-document! (cdr (assq 'root (cdr (assq 'value (root:current))))) ',id)))))))
               (let ([b (start-head "screen B")])
                 (head-wait 'second-real-head b (lambda () (head-sees? b "shared text B")))
                 ;; Pause only a head's UI while its socket reader keeps
                 ;; running. Both budgets must close that connection and
                 ;; leave the other screen and the daemon's PTYs usable.
                 (for-each
                   (lambda (case)
                     (let* ([name (symbol->string (car case))] [who (list 'head name)]
                            [slow (start-head name)] [held (string-append root "/head-held")]
                            [release (string-append root "/head-release")])
                       (head-wait 'pressure-head slow (lambda () (head-sees? slow "shared text B")))
                       (for-each (lambda (path) (when (file-exists? path) (delete-file path))) (list held release))
                       (head-send! slow
                         (format "\x1b;x\x1b;[200~~begin (call-with-output-file ~s (lambda (p) (write #t p))) (let wait () (unless (file-exists? ~s) (sleep (make-time (quote time-duration) 5000000 0)) (wait)))\x1b;[201~~\r"
                                 held release))
                       (head-wait 'ui-paused slow (lambda () (file-exists? held)))
                       (dynamic-wind void
                         (lambda ()
                           (let send ([remaining (cadr case)] [payload (make-string (caddr case) #\x)])
                             (when (and (> remaining 0) (rpc head 'send who payload)) (send (- remaining 1) payload)))
                           (test:await 'overloaded-head-detached
                             (lambda () (not (member who (map car (rpc head 'actors)))))))
                         (lambda () (write-text release "continue")))
                       (head-wait 'client-reports-inbox-overload slow
                         (lambda () (> (occurrences (vector-ref slow 3) "pending input limit reached") 0)))
                       (test:check (list 'attached-inbox-overload (car case))
                         (list (and (member '(head "screen B") (map car (rpc head 'actors))) #t)
                               (cdr (assq 'alive (caddr (rpc head 'snapshot '(buffer 3)))))) '(#t #t))))
                   '((inbox-count 96 1024) (inbox-bytes 3 131072)))
               )))
           (stop!)
           (for-each (lambda (head)
                       (head-wait 'head-restores-terminal-after-disconnect head
                         (lambda ()
                           (and (> (occurrences (vector-ref head 3) "\x1b;[?1049l") 0)
                                (let ([state (vt:emulator-state (vector-ref head 2))])
                                  (not (or (cdr (assq 'mouse-tracking state))
                                           (cdr (assq 'sgr-mouse state)))))))))
             ;; SIGKILL cannot run terminal cleanup; all cooperative exits can.
             (filter (lambda (head) (not (memq head killed-heads))) heads))
           (void))))
     (define (bootstrap-scenarios!)
       (call-with-bootstrap-installation
         (lambda ()
           (let ([listener (sys:listen-local socket)]) (sys:close-local-listener! listener))
           (write-text socket "ordinary file")
           (test:check 'ordinary-file-at-socket-path-is-preserved
             (list (test:raises? (lambda () (sys:listen-local socket)))
                   (test:raises? (lambda () (sys:connect-local socket)))
                   (let ([result (loader-exit (list "--help" "--base-working-dir" base-directory))])
                     (list (car result) (occurrences (caddr result) "Base status unavailable")))
                   (call-with-input-file socket get-string-all)) '(#t #t (0 1) "ordinary file"))
           (delete-file socket)

           ;; Bound whole hello exchanges, including partial frames and a
           ;; blocked write. This also exercises watchdog cleanup on failure.
           (let ([listener (sys:listen-local socket)])
             (dynamic-wind void
               (lambda ()
                 (test:check 'hello-deadline-covers-header-payload-and-write
                   (map
                     (lambda (prefix)
                       (let* ([client (sys:connect-local socket)] [server (sys:accept-local listener)])
                         (dynamic-wind void
                           (lambda ()
                             (when prefix
                               (put-bytevector (sys:connection-output server) prefix)
                               (flush-output-port (sys:connection-output server)))
                             (test:raises?
                               (lambda ()
                                 (sys:call-with-connection-deadline client
                                   (add-duration (current-time 'time-monotonic) (make-time 'time-duration 50000000 0))
                                   (lambda ()
                                     (if prefix (wire:receive (sys:connection-input client))
                                         (wire:send! (sys:connection-output client) (make-string #x100000 #\x))))))
                               sys:unresponsive?))
                           (lambda () (sys:close-connection! client) (sys:close-connection! server)))))
                     '(#vu8(0) #vu8(0 0 0 4 40) #f)) '(#t #t #t))
                 (let* ([launcher (test:worker
                                    (lambda () (loader-exit (list "--help" "--name" "status probe"
                                                                  "--base-working-dir" base-directory))))]
                        [connection (sys:accept-local listener)])
                   (dynamic-wind void
                     (lambda ()
                       (test:check 'help-times-out-without-retrying-or-changing-the-endpoint
                         (list (wire:receive (sys:connection-input connection))
                               (let ([result (launcher)])
                                 (list (car result) (occurrences (caddr result) "unresponsive")
                                       (occurrences (caddr result) "\x1b;") (file-exists? socket))))
                         '((maintenance 1 (head "status probe")) (0 1 0 #t))))
                     (lambda () (sys:close-connection! connection))))
                 (let ([pending '()] [timed-out? #f])
                   (dynamic-wind void
                     (lambda ()
                       ;; Fill a listening socket without accepting. The
                       ;; kernel's queue, not a guessed sleep, creates pressure.
                       (let fill ([left 128])
                         (unless (or timed-out? (zero? left))
                           (guard (ex [else (set! timed-out?
                                              (and (string:search (kernel:condition-text ex) "connection timed out" 0
                                                     (string-length (kernel:condition-text ex))) #t))])
                             (set! pending
                               (cons (sys:connect-local socket
                                       (add-duration (current-time 'time-monotonic) (make-time 'time-duration 100000000 0)))
                                 pending)))
                           (fill (- left 1))))
                       (test:check 'full-listener-backlog-respects-connect-deadline timed-out? #t))
                     (lambda () (for-each sys:close-connection! pending)))))
               (lambda () (sys:close-local-listener! listener))))

           ;; Stop immediately after the readiness probe, when an accepting
           ;; worker can receive a signal just before blocking in I/O. The
           ;; command also proves the base's signal mask stops at exec.
           (base-config!
             '((let ([child (sys:open-process '("/bin/sh" "-c" "kill -TERM $$; exit 9"))])
                 (dynamic-wind void
                   (lambda ()
                     (sys:write-process! child #f)
                     (get-bytevector-all (sys:process-input child))
                     (let-values ([(code errors) (sys:process-result child)])
                       (unless (= code -15) (error 'fixture "child inherited blocked signals" code errors))))
                   (lambda () (sys:close-process! child))))))
           (test:check 'fresh-base-stops-on-either-signal-and-keeps-child-signals-normal
             (map (lambda (signal)
                    (let ([fresh (fixture:start! root (format "~a/immediate-~a" root signal))])
                      (fixture:stop! fresh signal) #t))
               '(15 2)) '(#t #t))

           (void))))
     (include "tests/repl-wire.sps")
     (define (cold-scenarios!)
       (call-with-bootstrap-installation
         (lambda ()
           ;; Reuse this installation for the actual automatic bootstrap.
           ;; The fixture's own config can stop or crash its own base; tests
           ;; never signal a pid read from a possibly stale process record.
           (fresh-session!)
           (base-config!
             `((store:create! '(base e) "bootstrap" '("ready")
                 (list (cons 'process-id (get-process-id)) (cons 'directory (current-directory))))
               (for-each
                 (lambda (name)
                   (let ([who (list 'head name)])
                     (actor:register! who void)
                     (actor:checkpoint! who
                       '(screen 6 1 (window 1 0 0 0 default default ())
                          (((local "*scratch*" 4 ((trailing . #f)) ("restored")) #f ())
                           ((local "<hidden recovery>" 2 () ("hidden authored text")) #f ()))))
                     (actor:detach! who))) '("auto α's desk" "auto B"))
               (fork-thread
                 (lambda ()
                   (let wait ()
                     (unless (file-exists? ,automatic-control)
                       (sleep (make-time 'time-duration 5000000 0)) (wait)))
                   (system (format "kill -~a ~a"
                             (if (eq? (call-with-input-file ,automatic-control read) 'crash) "KILL" "TERM")
                             (get-process-id)))))))
           (write-forms (string-append root "/config.e") '((void)))
           (write-text (string-append base-directory "/log/2000-01-01.log") "expired")
           (write-text (string-append base-directory "/log/keep.txt") "keep")
           ;; Exercise concurrent cold compilation once. The later heads race
           ;; to exec a base and contend on its lifetime flock; recompiling the
           ;; unrelated head adapter repeats the same cache-lock mechanism.
           (test:check 'concurrent-cold-compilers-share-a-consistent-cache
             (map (lambda (round)
                    (remove-tree! objects)
                    (test:parallel 3 (lambda (index) (list-head (loader-exit '("--help")) 2)))) '(1))
             (make-list 1 (make-list 3 '(0 ""))))
           (seed-objects! "base")
           (seed-objects! "client")
           (write-forms (string-append root "/blank.e")
             '((list (cons 'profile "blank") (cons 'entry (lambda (context) #f)))))
           (let* ([a (start-command '("--name" "auto α's desk" "--start" "start.e") 80)]
                  [b (start-command '("--name" "auto B" "--start" "blank.e" "unhandled.txt") 80)])
             (head-wait 'automatic-head a (lambda () (head-sees? a "*scratch*")))
             (head-wait 'blank-head-uses-ordinary-pump b
               (lambda () (pump-head! b) (> (occurrences (vector-ref b 3) "does not accept file requests") 0)))
             (test:check 'startup-recipe-imports-supported-text-only-for-its-editor-profile
               (list (head-read a
                       '(let* ([screen (cdr (assq 'root (cdr (assq 'value (root:current)))))]
                               [manager (widget:descendant screen 'content 'windows)]
                               [document (window:document manager (window:current manager))])
                          (list (store:line document 0) (and (store:property document 'import-origin #f) #t)
                            (store:property document 'audience) (client:request 'checkpoint)
                            (store:line (store:find-named "<hidden recovery>") 0))))
                 (head-read b '(let () (import (prefix (state actor) actor:) (prefix (head head) head:))
                                 (and (client:request 'checkpoint) #t))))
               '(("restored" #t ((head "auto α's desk")) #f "hidden authored text") #t))
             (head-send! b "\x18;\x03;")
             (vt:emulator-resize! (vector-ref b 2) 18 100)
             (sys:resize-terminal-process! (vector-ref b 0) 18 100)
             (head-wait 'blank-head-resizes-without-default-windows b
               (lambda () (head-read b '(let () (import (prefix (head tui) tui:)) (= (tui:screen-cols) 100)))))
             (test:check 'blank-recipe-has-no-editor-windows-or-implicit-keymap
               (list (head-read b
                       '(let () (import (prefix (head head) head:)
                                        (prefix (head root) root:) (prefix (head widget) widget:))
                          (list (head:quitting?) (widget:shown)
                            (cdr (assq 'root (cdr (assq 'value (root:current))))))))
                 (file-exists? (string-append root "/unhandled.txt")))
               '((#f () #f) #f))
             (let* ([pid-path (string-append base-directory "/pid")]
                    [record (call-with-input-file pid-path read)]
                    [boot-pid '(let () (import (prefix (state store) store:)) (store:property (store:find-named "bootstrap") 'process-id))])
               (test:check 'cold-start-race-shares-one-base-and-preserves-head-directory
                 (list (head-read a boot-pid) (head-read b boot-pid)
                       (head-read a '(current-directory))
                       (head-read b '(let () (import (prefix (state store) store:)) (store:property (store:find-named "bootstrap") 'directory)))
                       (map get-mode (list base-directory socket pid-path (string-append base-directory "/lock")))
                       (file-exists? (string-append base-directory "/log/2000-01-01.log"))
                       (file-exists? (string-append base-directory "/log/keep.txt")))
                 (list (cadr record) (cadr record) root base-directory '(#o700 #o600 #o600 #o600) #f #t))
               (head-read a '(begin (widget:input! (cdr (assq 'root (cdr (assq 'value (root:current))))) '(text "retained" keyboard)) #t))
               (test:check 'attached-composition-opens-and-reuses-unmounted-apps
                 (head-read a
                   '(let* ([screen (cdr (assq 'root (cdr (assq 'value (root:current)))))]
                           [manager (widget:descendant screen 'content 'windows)]
                           [window (window:current manager)]
                           [finder (finder:open! window)])
                      (window-control:return! window)
                      (let ([buffet (buffet:open! window)]
                            [cleaned (let ([before (list (model:ids) (map car (store:buffer-list)))])
                                       (guard (ex [else (equal? before (list (model:ids) (map car (store:buffer-list))))])
                                         (window-control:open-app! window "Rejected Buffet"
                                           (lambda (owner commands)
                                             (let* ([app (buffet:create! owner commands)]
                                                    [r (caddar (cadr (model:snapshots (list app))))]
                                                    [d (cdr (assq 'value r))])
                                               (view:arrange! head:ui-actor
                                                 (list (list app (cdr (assq 'revision r)) (view:children d)
                                                         (cons '(catalogue . #f) (view:options d)))) '()) app))) #f))])
                        (window-control:return! window)
                        (let ([same (equal? buffet (buffet:open! window))])
                          (window-control:return! window)
                          (let ([other (window-control:split! window 'right)])
                            (when (equal? finder (window-control:open-document! other finder))
                              (error 'fixture "another window must fork the hidden app"))
                            (window-control:close! other))
                          (let* ([aux (screen:auxiliary! screen)] [inspector (bindings:open! aux screen)])
                            (screen:hide-auxiliary! screen)
                            (list same cleaned (map (lambda (id) (view:kind (view:snapshot id)))
                                                 (list finder buffet inspector))))))))
                 '(#t #t (finder buffet bindings)))
               (head-send! a "\x13;retained")
               (head-wait 'attached-composition-search-entry a (lambda () (head-sees? a "I-search:")))
               (test:check 'attached-search-is-parented-to-the-document-window
                 (head-read a
                   '(let* ([root (cdr (assq 'root (cdr (assq 'value (root:current)))))]
                           [manager (widget:descendant root 'content 'windows)] [window (window:current manager)]
                           [panel (widget:descendant window 'search)]
                           [entry (widget:descendant panel 'search 'entry)])
                      (list (equal? window (view:parent (view:snapshot panel)))
                        (equal? entry (widget:focused root))))) '(#t #t))
               (head-send! a "\x07;")
               (head-wait 'attached-search-returns-to-editor a (lambda () (not (head-sees? a "I-search:"))))
               (head-send! a "\x18;\x03;")
               (repl-scenarios! b)
               (for-each (lambda (head)
                           (head-wait 'automatic-detach head (lambda () (head-sees? head "e: detached")))
                           (sys:reap-terminal-process! (vector-ref head 0))) (list a b))
               (void))))))
     (define (automatic-scenarios!)
       (call-with-bootstrap-installation
         (lambda ()
           ;; Reuse this installation for the actual automatic bootstrap.
           ;; The fixture's own config can stop or crash its own base; tests
           ;; never signal a pid read from a possibly stale process record.
           (fresh-session!)
           (base-config!
             `((store:create! '(base e) "bootstrap" '("ready")
                 (list (cons 'process-id (get-process-id)) (cons 'directory (current-directory))))
               (fork-thread
                 (lambda ()
                   (let wait ()
                     (unless (file-exists? ,automatic-control)
                       (sleep (make-time 'time-duration 5000000 0)) (wait)))
                   (system (format "kill -~a ~a"
                             (if (eq? (call-with-input-file ,automatic-control read) 'crash) "KILL" "TERM")
                             (get-process-id)))))))
           (write-forms (string-append root "/config.e") '((void)))
           (write-text (string-append base-directory "/log/2000-01-01.log") "expired")
           (write-text (string-append base-directory "/log/keep.txt") "keep")
           (let* ([a (start-head "auto α's desk")] [b (start-head "auto B")])
             (for-each (lambda (head) (head-wait 'automatic-head head (lambda () (head-sees? head "*scratch*"))))
               (list a b))
             (let* ([pid-path (string-append base-directory "/pid")]
                    [record (call-with-input-file pid-path read)]
                    [boot-pid '(store:property (store:find-named "bootstrap") 'process-id)])
               (head-read a '(begin (edit:insert! (test-editor) "retained")
                                    (head:add-shutdown-hook! (lambda () (edit:select! (widget:focused) '(0 . 3) '(0 . 3)))) #t))
               (for-each (lambda (head) (head-send! head "\x18;\x03;")) (list a b))
               (for-each (lambda (head)
                           (head-wait 'automatic-detach head (lambda () (head-sees? head "e: detached")))
                           (sys:reap-terminal-process! (vector-ref head 0))) (list a b))
               (let* ([again (start-head "auto α's desk")] [inspector (connect)]
                      [leavers (list (connect) (connect))])
                 (head-wait 'automatic-resume again (lambda () (head-sees? again "retained")))
                 (test:check 'quit-keeps-shared-edits-and-print-only-one-farewell-line
                   (list (head-read again '(list (store:line (test-document) 0) (test-point)))
                         (equal? record (call-with-input-file pid-path read))
                         (occurrences (vector-ref again 3) "e: started base")
                         (map (lambda (head)
                                (let* ([output (vector-ref head 3)]
                                       [at (string:search output "e: detached;" 0 (string-length output))]
                                       [farewell (substring output at (string-length output))])
                                  (list (occurrences farewell "\n") (occurrences farewell "Resume:")
                                        (occurrences farewell "Stop the base:")))) (list a b)))
                   '(("retained" (0 . 3)) #t 0 ((1 0 0) (1 0 0))))
                 (hello inspector '(head "inspector"))
                 (for-each (lambda (connection name) (hello connection (list 'head name))) leavers '("leave A" "leave B"))
                 (let* ([before (cdr (assq 'heads (rpc inspector 'status)))]
                        [departures (test:parallel 2 (lambda (index) (rpc (list-ref leavers index) 'leaving #f)))])
                   (test:check 'concurrent-departures-commit-before-their-replies
                     (list (list-sort < (map (lambda (status) (cdr (assq 'heads status))) departures))
                           (cdr (assq 'heads (rpc inspector 'status))))
                     (list (list (- before 2) (- before 1)) (- before 2))))
                 (for-each sys:close-connection! (cons inspector leavers))
                 (write-control! "crash")
                 (head-wait 'unexpected-base-death again (lambda () (head-sees? again "e: the base is gone")))
                 (sys:reap-terminal-process! (vector-ref again 0))
                 (test:check 'crash-leaves-recoverable-endpoints
                   (map file-exists? (list socket pid-path (string-append base-directory "/lock"))) '(#t #t #t))
                 (delete-file automatic-control)
                 (let ([fresh (start-head "after crash")])
                   (head-wait 'stale-endpoints-recovered fresh (lambda () (head-sees? fresh "*scratch*")))
                   (test:check 'stale-cleanup-starts-a-fresh-in-memory-session
                     (list (not (equal? record (call-with-input-file pid-path read)))
                           (head-read fresh '(store:line (test-document) 0))) '(#t ""))
                   (write-control! "stop")
                   (head-wait 'announced-base-stop fresh (lambda () (head-sees? fresh "e: the base stopped (signal)")))
                   (sys:reap-terminal-process! (vector-ref fresh 0))
                   (test:await 'automatic-base-cleans-up (lambda () (not (file-exists? pid-path))))
                   (test:check 'all-automatic-exits-restore-the-terminal
                     (map (lambda (head)
                            (let ([state (vt:emulator-state (vector-ref head 2))])
                              (list (> (occurrences (vector-ref head 3) "\x1b;[?1049l") 0)
                                    (cdr (assq 'mouse-tracking state)) (cdr (assq 'sgr-mouse state)))))
                       (list a b again fresh)) (make-list 4 '(#t #f #f)))))))
           (void))))
     (define (run-group! name)
       (case (string->symbol name)
         [(protocol) (protocol-scenarios!)]
         [(attached) (attached-scenarios!)]
         [(resume) (resume-scenarios!)]
         [(bootstrap) (bootstrap-scenarios!)]
         [(cold) (cold-scenarios!)]
         [(automatic) (automatic-scenarios!)]
         [(shutdown) (shutdown-scenarios!)]
         [(final-shutdown) (final-shutdown-scenarios!)]
         [(session) (session-scenarios!)]
         [(recovery) (recovery-scenarios!)]
         [(restart) (restart-scenarios!)]
         [(restart-accept) (restart-accept-scenarios!)]
         [(restart-file) (restart-file-scenarios!)]
         [(overload) (overload-scenarios!)]
         [(force) (force-scenarios!)]
         [(help) (help-scenarios!)]
         [else (error 'wire-test "unknown scenario group" name)]))
     ;; Groups that ran after the protocol fixture found its base directory;
     ;; standalone they need the private directory as well.
     (sys:ensure-private-directory! base-directory)
     (dynamic-wind void
       (lambda ()
         (for-each run-group!
           (if (null? (command-line-arguments))
               '("protocol" "attached" "resume" "overload" "bootstrap" "cold" "automatic"
                 "shutdown" "final-shutdown" "session" "recovery" "restart" "restart-accept" "restart-file" "force" "help")
               (command-line-arguments))))
       (lambda ()
         (for-each (lambda (head) (sys:close-terminal-process! (vector-ref head 0))) heads)
         (remove-tree! root)))
     (test:finish! 'wire)))
