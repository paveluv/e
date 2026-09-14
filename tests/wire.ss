#!/usr/bin/env scheme-script

;; One protocol fixture: framing, a real headless bootstrap, concurrent
;; connections and cleanup. The daemon gets an isolated installation/config.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (wire) wire:) (prefix (sys) sys:) (prefix (test) test:)
             (prefix (fixture) fixture:)
             (prefix (string) string:) (prefix (kernel) kernel:) (prefix (text) text:) (prefix (vt) vt:))

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
     (define terminal-pid-file (string-append root "/terminal-pid"))
     (define inventory-file (string-append root "/sessions"))
     (define audit-file (string-append root "/audit"))
     (define edit-held (string-append root "/edit-held"))
     (define edit-release (string-append root "/edit-release"))
     (define open-held (string-append root "/open-held"))
     (define open-release (string-append root "/open-release"))
     (define automatic-control (string-append root "/automatic-control"))
     (define sync-failure (string-append root "/fail-session-sync"))
     (define sync-held (string-append root "/session-sync-held"))
     (define (quote-shell text)
       (string-append "'" (apply string-append
                            (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))
     (define (write-text path text)
       (call-with-output-file path (lambda (port) (display text port)) 'replace))
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
       (let ([process (sys:open-process (append (list "scheme-script" (string-append root "/e")) arguments))])
         (dynamic-wind void
           (lambda ()
             (sys:write-process! process #f)
             (test:await 'base-cli-exits (lambda () (sys:process-status process)))
             (get-bytevector-all (sys:process-input process))
             (call-with-values (lambda () (sys:process-result process)) list))
           (lambda () (sys:close-process! process)))))
     (define (base-exit directory) (loader-exit (list "--base" "--base-working-dir" directory)))
     (for-each (lambda (path) (mkdir path #o700)) (list root sources objects))
     (copy-text "e" (string-append root "/e"))
     (chmod (string-append root "/e") (get-mode "e"))
     (copy-libraries "lib" sources)
     ;; Fault the OS sync in this owned installation only. Production has
     ;; no testing option or alternate lifecycle path; the ordinary syscall
     ;; still runs except during the one post-unlink failure scenario.
     (let* ([path (string-append sources "/sys/sys.sls")]
            [text (call-with-input-file path get-string-all)]
            [call "((foreign-procedure __collect_safe \"fsync\" (int) int) fd)"]
            [at (string:search text call 0 (string-length text))])
       (unless at (error 'wire-test "session sync syscall not found"))
       (write-text path
         (string-append (substring text 0 at)
           (format "(let ([sync (lambda () ~a)])
                      (if (not (file-exists? ~s)) (sync)
                          (let ([mode (call-with-input-file ~s get-string-all)])
                            (unless (string=? mode \"fail\")
                              (call-with-output-file ~s (lambda (p) (write #t p)) 'replace)
                              (let wait ([left 1000])
                                (when (file-exists? ~s)
                                  (when (zero? left) (error 'fixture \"sync hold timed out\"))
                                  (sleep (make-time 'time-duration 5000000 0)) (wait (- left 1)))))
                            (if (string=? mode \"hold\") (sync) -1))))"
             call sync-failure sync-failure sync-held sync-failure)
           (substring text (+ at (string-length call)) (string-length text)))))
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
         (do ([i 0 (+ i 1)]) ((= i 5002)) (log:add! 'wire-retained i #f))
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
             (when (eq? (log:component record) 'store)
               (let* ([event (log:datum record)] [app? (eq? (car (log:actor record)) 'app)]
                      [operation? (and (pair? event) (memq (car event) '(edit reset)) (= (cadr event) notes))])
                 (when app? (set! app-presentations (cons presentation app-presentations)))
                 (when operation? (set! operation-audits (cons (list (log:actor record) event) operation-audits)))
                 (when (or app? operation?)
                   (call-with-output-file ,audit-file
                     (lambda (out) (write (list (reverse operation-audits) app-presentations) out)) 'replace))))
             (when (eq? (log:component record) 'policy)
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
                               (store:set-property! '(base e) 2 'padding
                                 (make-string (if (string=? name "stalled count") 8192 2097152) #\x)))
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
             (collect)
             (store:reset! '(agent "background") notes '("agent work while detached"))))))

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
         ((test:worker (lambda () (wire:receive (sys:connection-input connection)))))))
     (define (exchange connection message)
       (set! last-request message)
       (wire:send! (sys:connection-output connection) message)
       (receive-reply connection))
     (define (receive-reply connection)
       (let ([message (receive connection)])
         (cond [(and (pair? message) (eq? (car message) 'changed))
                (notices (cons connection (cadr message))) (receive-reply connection)]
           [(equal? message '(event (pending))) (receive-reply connection)]
           [else message])))
     (define (connect)
       (let ([connection (sys:connect-local socket)])
         (set! clients (cons connection clients)) connection))
     (define (hello connection actor)
       (exchange connection (list 'hello wire:version actor)))
     (define (reply-value reply . id)
       (unless (and (list? reply) (= (length reply) 4)
                    (equal? (list-head reply 3) (list 'reply (if (pair? id) (car id) 7) 'ok)))
         (error 'wire-test "request failed" last-request reply))
       (cadddr reply))
     (define (rpc connection operation . args)
       (reply-value (exchange connection (append (list 'request 7 operation) args))))
     (define (inventory connection)
       (cdr (assq 'sessions (caddr (rpc connection 'snapshot 1)))))

     ;; Drive the real editor in the daemon's installation. All heads use
     ;; the same object cache, which also exercises repeated client loading.
     (define probe (string-append root "/head-result"))
     (define (start-head name . width)
       (let* ([columns (if (pair? width) (car width) 80)]
              [process (sys:spawn-terminal-process "/bin/sh"
                         (fixture:command test-base "--name" name)
                         root 24 columns)]
              [head (vector process
                      (transcoded-port (sys:terminal-process-input process) (make-transcoder (utf-8-codec) 'none 'replace))
                      (vt:make-emulator 24 columns) "")])
         (set! heads (cons head heads)) head))
     (define (pump-head! head)
       (let drain ()
         (when (guard (ex [else #f]) (char-ready? (vector-ref head 1)))
           (let ([c (guard (ex [else (eof-object)]) (get-char (vector-ref head 1)))])
             (unless (eof-object? c)
               (vt:emulator-feed! (vector-ref head 2) (string c))
               (let ([text (string-append (vector-ref head 3) (string c))])
                 (vector-set! head 3 (if (> (string-length text) 32768) (string:tail text 16384) text)))
               (drain))))))
     (define (head-sees? head text)
       (pump-head! head)
       (exists (lambda (line) (string:search line text 0 (string-length line)))
         (vector->list (vt:emulator-screen (vector-ref head 2)))))
     (define (head-send! head text)
       (put-bytevector (sys:terminal-process-output (vector-ref head 0)) (string->utf8 text))
       (flush-output-port (sys:terminal-process-output (vector-ref head 0))))
     (define (head-wait label head predicate)
       (guard (ex [else (error 'wire-head (format "~a" label)
                          (kernel:condition-text ex)
                          (vector->list (vt:emulator-screen (vector-ref head 2)))
                          (let ([text (vector-ref head 3)])
                            (string:tail text (max 0 (- (string-length text) 1200)))))])
         ;; Every live PTY needs a reader, even while another head is active.
         (test:await label (lambda () (for-each pump-head! heads) (predicate)))))
     (define (head-read head expression . prefix)
       (when (file-exists? probe) (delete-file probe))
       (head-send! head (format "~a\x1b;x\x1b;[200~~call-with-output-file ~s (lambda (p) (write ~s p)) (quote replace)\x1b;[201~~\r"
                          (if (pair? prefix) (car prefix) "") probe expression))
       (let ([result (eof-object)])
         (head-wait 'head-evaluation head
           (lambda ()
             (guard (ex [else #f])
               (and (file-exists? probe)
                    (begin (set! result (call-with-input-file probe read)) (not (eof-object? result)))))))
         result))
     (define (head-blame head)
       (head-read head
         '(let* ([b (current-buffer)] [id (head:buffer-store-id b)])
            (list
              (map (lambda (range) (list (cadr range) (caddr range) (cadddr range)))
                (filter (lambda (range)
                          (and (= (length range) 5) (eq? (car range) b)
                               (memq (list-ref range 4)
                                 '(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))))
                  (paint:highlight-ranges)))
              (cadar (store:blame id 1))))))
     (define (screen-state head)
       (head-read head
         '(list (let shape ([node (head:root)])
                  (if (head:window? node) (head:window-index node)
                      (list (head:layout-split-orientation node)
                            (head:layout-split-first-weight node) (head:layout-split-second-weight node)
                            (shape (head:layout-split-first node)) (shape (head:layout-split-second node)))))
                (head:window-index (head:current)) (head:kill-ring)
                (map (lambda (w)
                       (let ([b (head:window-buffer w)])
                         (list (or (head:buffer-store-id b) (head:buffer-name b))
                               (buffer-line b (head:window-prow w)) (head:window-pcol w)
                               (and (head:buffer-store-id b) (buffer-line b (head:window-top w)))
                               (head:window-wrap w)))) (head:windows)))))
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
       (let ([held (string-append root "/pause-held")]
             [release (string-append root "/pause-release")]
             [session (string-append base-directory "/session")]
             [disk (string-append root "/reviewed-file")])
         (write-text (string-append root "/config.e") "(main:set-startup-page! #f)\n")
         (write-forms (string-append root "/base-config.e")
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
               (define (reject connection operation . args)
                 (let ([reply (exchange connection (append (list 'request 7 operation) args))])
                   (and (eq? (caddr reply) 'error) (cadddr reply))))
               (define (cancel connection review) (rpc connection 'cancel-review (cadr review)))
               (define (terminal connection)
                 (rpc connection 'vt-open "printf ready; while read value; do printf '<%s>' \"$value\"; done"
                   root 3 32 'dark))
               (for-each (lambda (connection who) (hello connection who))
                 (list a b restricted agent)
                 '((head "shutdown A") (head "shutdown B") (head "restricted") (agent "observer")))
               (write-text disk "on disk\n")
               (let* ([clean (rpc a 'create "matches disk" '("on disk") `((file . ,disk) (trailing . #t)))]
                      [empty (rpc a 'create "empty" '(""))]
                      [output (rpc a 'create "disposable" '("output") '((disposable . #t)))]
                      [large (map (lambda (i) (rpc a 'create (format "large ~a" i)
                                                (list (make-string (* 6 1024 1024) #\x)))) '(1 2 3))]
                      [review (rpc a 'prepare-close)] [token (cadr review)])
                 (test:check 'compact-review-retains-large-text-and-exact-cleanliness-at-the-base
                   (list (< (bytevector-length (encoded review)) 4096)
                         (map (lambda (id) (caddr (assv id (caddr review)))) (cons clean (cons empty large)))
                         (assv output (caddr review))) '(#t (#f #f #t #t #t) #f))
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
                 (for-each (lambda (id) (rpc a 'delete id)) large)
                 ;; Disk changes can turn unchanged store text into unsaved work.
                 (let* ([review (rpc a 'prepare-close)]
                        [next (begin (write-text disk "changed on disk\n")
                                     (rpc a 'shutdown (cadr review)))])
                   (test:check 'disk-change-needs-new-consent
                     (list (car next) (caddr (assv clean (caddr next)))) '(review #t))
                   (cancel a next))
                 (for-each
                   (lambda (change)
                     (let* ([review (rpc a 'prepare-close)]
                            [next (begin
                                    (case change
                                      [(edit) (rpc a 'edit clean 0 '(0 0 0 0) '("new "))]
                                      [(facts) (rpc a 'properties clean '((read-only . #t)))]
                                      [(new) (rpc a 'create "new hidden work" '("kept") '((audience)))])
                                    (rpc a 'shutdown (cadr review)))])
                       (test:check (list 'stale-shutdown change)
                         (list (car next) (not (= (cadr review) (cadr next))) (phase)) '(review #t reviewing))
                       (cancel a next))) '(edit facts new))
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
                   ;; Deletions and disposable output need no fresh consent.
                   ;; An invalid session path then fails before unlink, while
                   ;; the injected sync failure is after unlink: both resume.
                   (for-each
                     (lambda (failure)
                       (if (eq? failure 'unlink) (mkdir session #o700)
                           (begin (write-text session "saved session") (chmod session #o600)
                                  (write-text sync-failure "fail")))
                       (let* ([review (rpc a 'prepare-close)]
                              [error (begin
                                       (when (eq? failure 'unlink)
                                         (rpc a 'delete empty)
                                         (rpc a 'reset output '("new disposable output")))
                                       (reject a 'shutdown (cadr review)))])
                         (test:check (list 'durable-failure-resumes-before-ending-processes failure)
                           (list (and error #t) (phase) (file-exists? session)
                                 (cdr (assq 'alive (caddr (rpc a 'snapshot term))))
                                 (if (eq? failure 'sync) (> (occurrences error "uncertain") 0) #t))
                           (list #t 'running (eq? failure 'unlink) #t #t)))
                       (if (eq? failure 'unlink) (delete-directory session) (delete-file sync-failure)))
                     '(unlink sync))
                   (rpc a 'vt-send term "recovered\n" '(3 32) #f #f)
                   (test:await 'terminal-works-after-failed-stop
                     (lambda () (exists (lambda (line) (> (occurrences line "<recovered>") 0))
                                  (vector->list (car (rpc a 'snapshot term))))))
                   (write-text session "saved session") (chmod session #o600)
                   (let ([ui (start-head "shutdown UI")])
                     (head-wait 'shutdown-ui-ready ui (lambda () (head-sees? ui "*scratch*")))
                     (head-read ui
                       '(let ([b (head:new-local-buffer "local shutdown work")])
                          (head:add-buffer! b) (head:store-reset! b '("local draft"))
                          (head:buffer-modified-set! b #t) #t))
                     (for-each
                       (lambda (key)
                         (head-send! ui "\x1b;xmain:shutdown!!\r")
                         (head-wait 'shutdown-review-question ui (lambda () (head-sees? ui "Stop the base?")))
                         (head-send! ui key)
                         (test:await 'ui-cancel-releases-review (lambda () (eq? (phase) 'running)))
                         (test:check (list 'shutdown-cancellation key)
                           (head-read ui '(list (head:quitting?)
                                            (buffer-line (head:buffer-named "<local shutdown work>") 0)))
                           '(#f "local draft"))) '("n" "v" "\x1b;" "\x07;"))
                     (head-read ui '(begin (main:shutdown-on-exit #t)
                                           (head:buffer-fact-set! (head:buffer-named "<local shutdown work>") 'disposable #t) #t))
                     (rpc a 'leaving #f) (sys:close-connection! a)
                     (head-send! ui "\x18;\x03;")
                     (head-wait 'last-head-reviews-before-exit ui (lambda () (head-sees? ui "Stop the base?")))
                     (head-send! ui "n")
                     (test:await 'cancelled-last-head-keeps-editing (lambda () (eq? (phase) 'running)))
                     (test:check 'last-head-cancellation-retains-head-and-preference
                       (head-read ui '(list (head:quitting?) (main:shutdown-on-exit))) '(#f #t))
                     (head-send! ui "\x18;\x03;")
                     (head-wait 'last-head-acceptance-question ui (lambda () (head-sees? ui "Stop the base?")))
                     (head-send! ui "y")
                     (head-wait 'reviewed-shutdown-announced ui (lambda () (head-sees? ui "e: the base shut down")))
                     (test:await 'accepted-base-exits (lambda () (sys:process-status (fixture:process base))))
                     (test:check 'durable-shutdown-removes-session-and-restores-terminal
                       (list (file-exists? session) (receive agent)
                             (map (lambda (key) (cdr (assq key (vt:emulator-state (vector-ref ui 2)))))
                               '(mouse-tracking sgr-mouse))) '(#f (closing shutdown) (#f #f)))))))))))

     (define (final-shutdown-scenarios!)
       (for-each
         (lambda (mode)
           (fixture:call-with-base root base-directory
             (lambda (base)
               (set! test-base base)
               (if (eq? mode 'clean)
                   (let ([ui (start-head "clean shutdown")])
                     (head-wait 'clean-head-ready ui (lambda () (head-sees? ui "*scratch*")))
                     (head-send! ui "\x1b;xmain:shutdown!!\r")
                     (head-wait 'clean-shutdown-needs-no-question ui (lambda () (head-sees? ui "e: the base shut down")))
                     (test:check 'clean-base-stops-without-a-question
                       (occurrences (vector-ref ui 3) "Stop the base?") 0))
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
                             '((error #f (busy paused)) paused #f #f))
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
         (lambda ()
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
             (test:check 'bad-version-and-duplicate-name-preserve-the-owner
               (map (lambda (version)
                      (let ([duplicate (connect)])
                        (list (car (exchange duplicate (list 'hello version identity)))
                              (eof-object? (receive duplicate))
                              (and (member identity (map car (rpc head 'actors))) #t)
                              (inventory head))))
                    (list 0 (- wire:version 1) wire:version))
               (make-list 3 (list 'error #t #t (list (list identity identity)))))
             (test:check 'request-errors-preserve-the-connection
               (map (lambda (message) (list-head (exchange head message) 3))
                 '((request 1 edit) (request 2 snapshot 999) (request 3 buffers extra)
                   (request 4 edit 1 0 (0 0 -1 0) ("x")) (request 5 edit 1 0.5 (0 0 0 0) ("x"))
                   (request 6 undo 1 everyone)
                   (request 7 edit 1 0 (0 0 0 0) ("x") (g "invalid" ((trailing . #t)) ((trailing . #f))))
                   (request 8 edit 1 0 (0 0 0 0) ("x") #f #f #f)
                   (request 8 edit 1 0 (0 0 0 0) ("x") #f delta)
                   (request 9 redo 1 all) (request 10 snapshot 1 #f)
                   (request 11 snapshot 1 -1) (request 12 watch extra)
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
               (list (test:raises? (lambda () (sys:listen-local socket))) (rpc head 'name 1))
               '(#t "notes λ"))
             (sys:close-connection! head))
           ;; Leave one connection stalled before hello and another after it.
           ;; The background actor must still collect and publish with no head.
           (let ([idle (connect)] [agent (connect)] [identity '(agent "reader")])
             (test:check 'agent-uses-the-same-read-connection
               (list (hello agent identity) (receive agent) (rpc agent 'buffers)
                     (car (rpc agent 'snapshot 2))
                     (rpc agent 'eval "(buffer-text-line \"notes λ\" 0)") (rpc agent 'eval #f)
                     (rpc agent 'eval "'#0=#(#0#)")
                     (rpc agent 'eval "(") (car (rpc agent 'eval "(delete-file \"unused\")")))
               (list (list 'hello wire:version identity '(read)) '(event (from-base "welcome")) '(1 2 3)
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
                                            '(event (from-base "welcome")) '(1 2 3) '(1 2 3))) actors))
               (test:check 'wire-evaluation-uses-the-configured-grants-fuel-and-preview-cap
                 (list (rpc first 'eval "(+ 1 2)")
                       (rpc first 'eval "(display \"hi\") (+ 1 2)")
                       (rpc first 'eval "(quote \"abcdefghijklmnopqrstuvwxyz\")")
                       (car (rpc first 'eval "(buffer-names)"))
                       (car (rpc first 'eval "(let loop () (loop))"))
                       (car (rpc first 'eval "(make-vector 100000 #f)"))
                       (car (rpc second 'snapshot 1)))
                 '((ok . "=> 3") (ok . "=> 3\noutput:\nhi") (ok . "=> \"abcdefghijkl ...")
                   unbound fuel fuel #("hello λ")))
               ;; Hold the first policy audit callback after commit. A second
               ;; actor commits before the first reply: its receipt must still
               ;; describe exactly its own accepted revision and anchor chain.
               (wire:send! (sys:connection-output first)
                 '(request 7 edit 1 0 (0 0 0 5) ("HELLO")
                    ((batch 1) "replace and prefix" ((trailing . #f)) ((saved-stamp . "observed"))) #t))
               (test:await 'first-edit-committed (lambda () (file-exists? edit-held)))
               (let ([second-result (rpc second 'edit 1 0 '(0 7 0 7) '("!") '((batch 1) "other actor"))])
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
                     '(((applied 1 #f #("HELLO λ") #t (1) 4 (modified modified-at))
                        (applied 2 #("HELLO λ!") #("HELLO λ!") #t (1 2) 4 (modified modified-at))) #t))))
               (test:check 'watchers-adopt-one-text-facts-and-anchor-snapshot
                 (map
                   (lambda (connection)
                     (let* ([state (rpc connection 'snapshot 1 0)] [chain (cadddr state)])
                       (list (car state) (cadr state) (assq 'trailing (caddr state))
                             (assq 'saved-stamp (caddr state))
                             (apply-changes '#("hello λ") chain) (map cadr chain)
                             (and (exists (lambda (notice)
                                            (and (eq? (car notice) connection)
                                                 (or (not (cdr notice)) (assv 1 (cdr notice))))) (notices)) #t))))
                   writers)
                 (make-list 2 '(#("HELLO λ!") 2 (trailing . #f) (saved-stamp . "observed")
                                #("HELLO λ!") ((agent "first") (agent "second")) #t)))
               (test:check 'wire-delta-replies-omit-text-only-with-a-complete-chain
                 (let ([delta (rpc (car writers) 'state 1 0 #t)]
                       [plain (rpc (car writers) 'state 1 0)]
                       [future (rpc (car writers) 'state 1 5 #t)])
                   (list (cadr delta) (apply-changes '#("hello λ") (list-ref delta 4)) (caddr delta)
                         (cadr plain) (cadr future) (list-ref future 4)
                         (map car (cadddr (rpc (car writers) 'state 1 2 'facts)))
                         (and (assq 'trailing (cadddr delta)) #t)))
                 '(#f #("HELLO λ!") 2 #("HELLO λ!") #("HELLO λ!") #f (modified modified-at) #t))
               (test:check 'stale-and-permission-refusals-preserve-text
                 (list (rpc second 'edit 1 0 '(0 1 0 3) '("bad"))
                       (rpc agent 'edit 1 2 '(0 0 0 0) '("bad"))
                       (rpc first 'edit 2 0 '(0 0 0 0) '("bad"))
                       (rpc agent 'undo 1 'all) (rpc first 'undo 2) (rpc agent 'redo 1) (rpc first 'redo 2)
                       (car (rpc agent 'snapshot 1)) (car (rpc agent 'snapshot 2)))
                 '((stale overlap) (refused buffer) (refused buffer) (refused buffer) (refused buffer) (refused buffer) (refused buffer)
                   #("HELLO λ!") #("a local audience")))
               ;; Repeating an actor/buffer-local key joins its two commits,
               ;; across the other writer's same-key edit. Undo facts reverse
               ;; with the text; observed external facts survive undo/redo.
               (rpc first 'edit 1 2 '(0 0 0 0) '("A") '((batch 1) "replace and prefix"))
               (let* ([mine (rpc first 'undo 1)] [after-mine (rpc agent 'snapshot 1)]
                      [again (rpc first 'undo 1)] [other (rpc first 'undo 1 '(actor (agent "second")))])
                 (test:check 'grouped-undo-defaults-to-mine-and-keeps-commit-facts
                   (list mine (car after-mine) (assq 'trailing (caddr after-mine))
                         (assq 'saved-stamp (caddr after-mine)) again other)
                   '((applied 5) #("hello λ!") #f (saved-stamp . "observed") (nothing #f) (applied 6))))
               (let* ([original-author (rpc second 'redo 1)] [other (rpc first 'redo 1)]
                      [mine (rpc first 'redo 1)] [after (rpc agent 'snapshot 1)]
                      [undo-group (rpc first 'undo 1 'all)] [undo-other (rpc first 'undo 1 'all)])
                 (test:check 'redo-belongs-to-the-undo-requester-and-restores-whole-groups
                   (list original-author other mine (car after)
                         (assq 'trailing (caddr after)) (assq 'saved-stamp (caddr after))
                         undo-group undo-other (car (rpc agent 'snapshot 1)))
                   '((nothing #f) (applied 7) (applied 9) #("AHELLO λ!")
                     (trailing . #f) (saved-stamp . "observed") (applied 11) (applied 12) #("hello λ"))))
               (rpc first 'edit 1 12 '(0 0 0 0) '("") '(protect "protect" ((read-only . #t))))
               (let ([before (rpc agent 'snapshot 1)])
                 (test:check 'wire-clients-cannot-bypass-read-only-with-facts-or-history
                   (list (rpc first 'edit 1 13 '(0 0 0 0) '("bad")
                           '(escape "escape" ((read-only . #f)) () (read-only)))
                         (rpc second 'edit 1 13 '(0 0 0 0) '("bad"))
                         (rpc first 'undo 1 'all) (rpc first 'redo 1)
                         (equal? before (rpc agent 'snapshot 1)))
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
                                         (wire:send! (sys:connection-output slow) (list 'request i 'snapshot 2))))))])
                       (test:await 'overloaded-reader-detached
                         (lambda () (not (assoc who (rpc agent 'actors)))))
                       (sent))
                     (sys:close-connection! slow)
                     (list (not (assoc who (inventory agent)))
                           (cdr (assq 'alive (caddr (rpc agent 'snapshot 3)))))))
                 '(("stalled count" . 512) ("stalled bytes" . 32)))
               '((#t #t) (#t #t)))
             (let ([slow (connect)] [who '(agent "stalled mail")])
               (wire:send! (sys:connection-output slow) (list 'hello wire:version who))
               (test:await 'publication-mail-refused
                 (lambda () (assq 'mail-refused (caddr (rpc agent 'snapshot 1)))))
               (test:await 'publication-overload-detached
                 (lambda () (not (assoc who (rpc agent 'actors)))))
               (test:check 'publication-overload-refuses-delivery-and-cleans-up-before-writer-start
                 (list (eof-object? (receive slow)) (not (assoc who (inventory agent)))
                       (assq 'mail-refused (caddr (rpc agent 'snapshot 1))))
                 '(#t #t (mail-refused . #t))))
             (write-text trigger "continue")
             (test:await 'background-agent
               (lambda () (equal? (car (rpc agent 'snapshot 1)) '#("agent work while detached"))))
             (test:check 'terminal-outlives-head-disconnect
               (cdr (assq 'alive (caddr (rpc agent 'snapshot 3)))) #t)
             (signal! "HUP")
             (test:check 'hup-keeps-the-base-and-current-revision
               (car (rpc agent 'snapshot 1)) '#("agent work while detached"))
             (test:check 'wire-catchup-distinguishes-current-and-missing-history
               (map (lambda (basis)
                      (let ([state (rpc agent 'snapshot 1 basis)])
                        (list (car state) (cadr state) (assq 'read-only (caddr state)) (cadddr state))))
                 '(14 13 15))
               '((#("agent work while detached") 14 (read-only . #t) ())
                 (#("agent work while detached") 14 (read-only . #t) #f)
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
                         '(edit 1 1 (0 0 0 5)) '(reset 1 14) #t))))
             (let ([head (connect)])
               (hello head '(head "desk λ")) (receive head)
               (test:check 'released-name-reads-producer-work-and-respects-read-only
                 (list (car (rpc head 'snapshot 1))
                       (rpc head 'edit 1 14 '(0 0 0 0) '("bad")) (rpc head 'undo 1) (rpc head 'redo 1))
                 '(#("agent work while detached") (refused read-only) (refused read-only) (refused read-only)))
               ;; Separate connection threads race named fact batches against
               ;; state reads. Bounded bursts keep writes contending without
               ;; filling the outbox. Each whole worker has one timeout.
               (let* ([writer (connect)]
                      [target (rpc head 'create "epoch-0" '("text") '((epoch . 0)))])
                 (hello writer '(head "state writer")) (receive writer)
                 (let ([results
                        (test:parallel 2
                          (lambda (index)
                            (let ([connection (if (zero? index) writer head)] [mismatch #f]
                                  [width (if (zero? index) 32 1)] [batches (if (zero? index) 64 2000)])
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
               (let ([id (rpc head 'create "attached text" '("shared text") '((trailing . #t)))])
                 (write-forms (string-append root "/config.e")
                   `((main:set-startup-page! #f)
                     (show-buffer! (head:adopt-store-buffer! ,id))))
                 (let* ([a (start-head "screen A")]
                        [ready-a (head-wait 'first-real-head a (lambda () (head-sees? a "shared text")))]
                        [b (start-head "screen B")])
                   (head-wait 'second-real-head b (lambda () (head-sees? b "shared text")))
                   (test:check 'two-real-heads-use-client-services-and-local-tools
                     (map (lambda (client)
                            (head-read client
                              '(list head:ui-actor (actor:current)
                                     (head:buffer-name (head:find-tool-buffer "*log*"))
                                     (vector-length (head:buffer-lines (head:find-tool-buffer "*log*")))
                                     (kernel:module-source "store")
                                     (kernel:module-requires? "main" "base")
                                     (guard (ex [else #t]) (kernel:reload-module! "store") #f)))) (list a b))
                     (map (lambda (name)
                            (list (list 'head name) (list 'head name) "<log>" 4096
                                  (string-append sources "/client/state/store.sls") #f #t)) '("screen A" "screen B")))
                   ;; Exercise the installed save hook, including first load,
                   ;; reload, inactive roots, old extensions and pinned code.
                   (let* ([probe (string-append sources "/apps/layout-probe.sls")]
                          [ignored (list (cons (string-append sources "/apps/layout-legacy.e") "layout-legacy")
                                         (cons (string-append sources "/base/state/layout-inactive.sls") "layout-inactive")
                                         (cons (string-append root "/layout-outside.sls") "layout-outside"))])
                     (define (publish path name version)
                       (write-forms path
                         `((library (,(string->symbol name)) (export init! value) (import (rnrs))
                             (define (value) ,version) (define (init!) (value))))))
                     (publish probe "layout-probe" 1)
                     (for-each (lambda (entry) (publish (car entry) (cdr entry) 0)) ignored)
                     (let ([first (head-read a `(begin (file:run-post-save-hooks! ,probe)
                                                       (eval '(layout-probe:value))))])
                       (publish probe "layout-probe" 2)
                       (test:check 'save-hook-follows-active-sls-roots-and-pinned-lifetimes
                         (list first
                           (head-read a
                             `(let ([before (top-level-value 'store:exists?)])
                                (for-each file:run-post-save-hooks!
                                  (append ',(cons probe (map car ignored))
                                          (list (kernel:module-source "store"))))
                                (list (eval '(layout-probe:value))
                                      (filter (lambda (name)
                                                (member name '("layout-probe" "layout-legacy"
                                                               "layout-inactive" "layout-outside")))
                                              (kernel:loaded-modules))
                                      (eq? before (top-level-value 'store:exists?))))))
                         '(1 (2 ("layout-probe") #t)))))
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
                                  (list (map log:datum entries) (- end first))))))
                     '(((worker (head "screen A") #f #t)
                        (first (head "screen A") #f #t) (second (head "screen A") #f #t))
                       (after-retraction second) (5000 5001) ((after-retraction) 5000)))
                   (head-send! a "A")
                   (head-wait 'foreign-paint-without-a-key b (lambda () (head-sees? b "Ashared text")))
                   (head-read b '(begin (goto-point! '(0 . 12)) #t))
                   (head-send! b "\x1b;[200~ B\x1b;[201~")
                   (head-wait 'second-writer a (lambda () (head-sees? a "Ashared text B")))
                   (head-read a '(undo!))
                   (head-wait 'undo-mine b (lambda () (head-sees? b "shared text B")))
                   (test:check 'attached-history-keeps-other-actors-and-rich-receipts
                     (list (car (rpc head 'snapshot id))
                           (head-read a '(point))
                           (map cadr (rpc head 'history id 3))
                           (let ([stamp (cdr (assq 'modified-at (caddr (rpc head 'snapshot id))))])
                             (map (lambda (screen)
                                    (= stamp (head-read screen '(head:buffer-modified-at (current-buffer))))) (list a b))))
                     '(#("shared text B") (0 . 0) ((head "screen A") (head "screen B") (head "screen A")) (#t #t)))
                   (head-read a '(undo! 'all))
                   (test:check 'attached-explicit-other-actor-undo-and-requester-redo
                     (list (car (rpc head 'snapshot id))
                           (begin (head-read a '(redo!)) (car (rpc head 'snapshot id))))
                     '(#("shared text") #("shared text B")))
                   (let ([ink (rpc head 'create "tint overlap" '("base"))])
                     (for-each
                       (lambda (screen)
                         (head-read screen `(begin (show-buffer! (head:adopt-store-buffer! ,ink)) #t)))
                       (list a b))
                     (head-send! b "\x1b;[200~FOREIGN\x1b;[201~")
                     (head-wait 'foreign-ink a (lambda () (head-sees? a "FOREIGNbase")))
                     (let ([before (map head-blame (list a b))])
                       (head-read a '(begin (goto-point! '(0 . 3)) #t))
                       (head-send! a "X")
                       (head-wait 'own-ink-inside-foreign-range b (lambda () (head-sees? b "FORXEIGNbase")))
                       (test:check 'attached-tints-follow-the-author-after-overlap
                         (list before (map head-blame (list a b)) (car (rpc head 'snapshot ink)))
                         '(((((0 0 7)) (head "screen B")) (() (head "screen B")))
                           ((() (head "screen A")) (((0 3 4)) (head "screen A")))
                           #("FORXEIGNbase"))))
                     (for-each
                       (lambda (screen)
                         (head-read screen `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t)))
                       (list a b))
                     (rpc head 'delete ink))
                   (let ([ticket (reply-value (exchange agent
                                                '(request 71 ask (head "screen A") "Ready to continue?" ("yes" "no"))) 71)])
                     (head-send! a "\x03;a")
                     (head-wait 'base-question a (lambda () (head-sees? a "Ready to continue?")))
                     (head-send! a "yes\r")
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
                         (head-read a '(begin (echo:settle!) #t))
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
                              (head-read a '(begin (echo:set-text! "Keep this message") #t))
                              (head-wait 'unrelated-echo a (lambda () (head-sees? a "Keep this message")))]
                             [(prompt)
                              (head-send! a "\x03;adraft")
                              (head-wait 'answer-being-written a (lambda () (head-sees? a "[...] draft")))])
                           (pump-head! a)
                           (vector-set! a 3 "")
                           (rpc agent 'cancel second)
                           (head-wait (list 'withdrawal-frame state) a
                             (lambda ()
                               (and (> (occurrences (vector-ref a 3) "\x1b;[?2026l") 0)
                                    (case state
                                      [(idle) (not (head-sees? a "Second question?"))]
                                      [(message) (head-sees? a "Keep this message")]
                                      [(prompt) (head-sees? a "[...] draft")]))))
                           (when (eq? state 'prompt)
                             (head-send! a "\r")
                             (head-wait 'withdrawn-answer a (lambda () (head-sees? a "That question was withdrawn"))))
                           (head-read a '(actor:pending head:ui-actor))))
                       '(idle message prompt))
                     '(() () ()))

                   ;; Exercise the actual head/client adapters as well as the
                   ;; request envelope: a refused fact batch must stay false.
                   (let ([target (rpc head 'create "guarded facts" '("keep")
                                   '((base . "keep\n") (trailing . #t)))])
                     (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,target)) #t))
                     (rpc head 'properties target '((base . "other\n")))
                     (let ([before (rpc head 'snapshot target)])
                       (test:check 'attached-fact-and-merge-refusals-preserve-the-source
                         (list
                           (head-read a
                             '(let ([b (current-buffer)])
                                (list (head:buffer-facts-set! b '((base . "lost")) '((base . "keep\n")) "lost name")
                                      (guard (ex [else (kernel:refusal? ex)])
                                        (head:store-edit! b (text:make-span 0 0 0 4) '("lost")
                                          '(merge "merge" () ((base . "lost")) ((base . "keep\n"))))))))
                           (rpc head 'edit target 0 '(0 0 0 4) '("lost")
                             '(merge "merge" () ((base . "lost")) ((base . "keep\n"))))
                           (equal? before (rpc head 'snapshot target)) (rpc head 'history target)
                           (rpc head 'name target))
                         '((#f #t) (stale property-changed) #t () "guarded facts")))
                     (test:check 'attached-fresh-guards-commit-and-undo-keeps-the-baseline
                       (head-read a
                         '(let* ([b (current-buffer)]
                                 [accepted (head:buffer-facts-set! b '((stamp . #f)) '((base . "other\n") stamp) "accepted facts")])
                            (head:store-edit! b (text:make-span 0 0 0 4) '("disk")
                              '(merge "merge" ((trailing . #f)) ((base . "disk")) ((base . "other\n") (trailing . #t))))
                            (let ([clean? (not (head:buffer-modified b))])
                              (undo!)
                              (list accepted clean? (head:buffer-lines b) (head:buffer-base b)
                                    (head:buffer-trailing b) (head:buffer-modified b) (head:buffer-name b)))))
                       '(#t #t #("keep") "disk" #t #t "accepted facts"))
                     (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t))
                     (rpc head 'delete target))

                   (for-each
                     (lambda (existing?)
                       (let ([path (string-append root (if existing? "/wire-open-existing.txt" "/wire-open-missing.txt"))])
                         (when existing? (write-text path "disk\n"))
                         (let ([target (head-read a
                                         `(begin (visit-file! ,path) (head:buffer-store-id (current-buffer))))])
                           (test:check (list existing? 'attached-file-opening-keeps-create-callback-work)
                             (let* ([screens
                                     (map (lambda (screen)
                                            (head-read screen
                                              `(begin (head:before-frame!)
                                                 (let* ([b (head:buffer-of-store-id ,target)]
                                                        [seen (head:buffer-fact b 'open-observation #f)])
                                                   (list (car seen) (cadr seen)
                                                     (map (lambda (key) (or (assq key (caddr seen)) key))
                                                       '(file base trailing mode mode-auto wrap modified))
                                                     (head:buffer-lines b) (mode:name-of b) (head:buffer-mode-auto b)
                                                     (head:buffer-base b) (head:buffer-modified b)))))) (list a b))]
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
                                       (list (cons 'file path) (if existing? '(base . "disk\n") 'base)
                                             '(trailing . #t) '(mode . #f) '(mode-auto . #t) '(wrap . default) '(modified . #f))
                                       (if existing? '#("agent disk") '#("agent ")) "scheme" #f
                                       (and existing? "disk\n") #t))
                               '((agent "opening")) (and existing? "disk\n")
                               (list (if existing? '#("disk") '#("")) #f #f)))
                           (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t))
                           (rpc head 'delete target))))
                     '(#t #f))

                   ;; Hold A's create reply while B visits the same canonical
                   ;; path and edits it. A stale candidate also crosses the wire.
                   (for-each
                     (lambda (existing?)
                       (let* ([name (if existing? "shared-visit-existing.txt" "shared-visit-missing.txt")]
                              [path (string-append root "/" name)])
                         (when existing? (write-text path "disk\n"))
                         (head-send! a (format "\x1b;xvisit-file! ~s\r" path))
                         (head-wait 'first-visit-published a (lambda () (file-exists? open-held)))
                         (let* ([target (rpc head 'find-file path)]
                                [second (head-read b
                                          `(begin (visit-file! ,(string-append root "/./" name))
                                             (insert-text! "B ") (head:buffer-store-id (current-buffer))))]
                                [before (rpc head 'snapshot target)] [history (rpc head 'history target)]
                                [reused (rpc head 'visit "stale candidate" '("lost")
                                          (list (cons 'file path) '(base . "lost\n") '(mode . #f)))]
                                [kept? (and (equal? reused (list target #f))
                                            (equal? before (rpc head 'snapshot target))
                                            (equal? history (rpc head 'history target)))])
                           (write-text open-release "continue")
                           (test:check (list existing? 'overlapping-head-visits-share-current-work)
                             (list (= target second) kept?
                                   (map (lambda (screen)
                                          (head-read screen
                                            '(begin (head:before-frame!)
                                               (let ([b (current-buffer)])
                                                 (list (head:buffer-store-id b) (head:buffer-lines b)
                                                       (head:buffer-base b) (mode:name-of b)
                                                       (head:buffer-mode-auto b)
                                                       (length (store:history (head:buffer-store-id b)))))))) (list a b))
                                   (and (file-exists? path) (call-with-input-file path get-string-all)))
                             (list #t #t
                                   (make-list 2 (list target (if existing? '#("B callback disk") '#("B callback "))
                                                      (and existing? "disk\n") "scheme" #f 2))
                                   (and existing? "disk\n")))
                           (for-each (lambda (screen) (head-read screen `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t)))
                             (list a b))
                           (rpc head 'delete target))
                         (when existing? (delete-file path))
                         (delete-file open-held) (delete-file open-release)))
                     '(#t #f))

                   (let* ([path (string-append root "/adoption.txt")]
                          [retarget (string-append root "/retargeted.ss")]
                          [target (rpc head 'create "before adoption" '("written")
                                    `((save-retarget . ,retarget) (read-only . #t) (disposable . #t)))])
                     (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,target)) #t))
                     (test:check 'attached-save-publishes-adoption-together-and-keeps-newer-callback-choices
                       (list (head-read a `(save-file! ,path)) (call-with-input-file path get-string-all)
                             (map (lambda (screen)
                                    (head-read screen
                                      `(begin (head:before-frame!)
                                         (let ([b (head:buffer-of-store-id ,target)])
                                           (list (head:buffer-name b) (head:buffer-file b) (head:buffer-base b)
                                                 (mode:name-of b) (head:buffer-mode-auto b)
                                                 (head:buffer-lines b) (head:buffer-modified b)))))) (list a b))
                             (cdr (assq 'save-observation (caddr (rpc head 'snapshot target))))
                             (file-exists? retarget))
                       (list #t "written\n"
                             (make-list 2 (list "retargeted.ss" retarget "new baseline\n" "scheme" #f '#("written") #t))
                             (list "adoption.txt" path "written\n" #f #t #f #f #f) #f))
                     (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t))
                     (rpc head 'delete target))

                   (let ([path (string-append root "/saved.txt")])
                     (write-text path "shared text B\n")
                     (head-read a
                       `(begin
                          (head:buffer-facts-set! (current-buffer) '((file . ,path) (base . "shared text B\n")))
                          (parameterize ([kernel:registering-module 'wire-save-hook])
                            (file:add-pre-save-hook!
                              (lambda (target)
                                (when (string=? target ,path) (file:write! target '#("from hook") #t))))) #t))
                     (head-send! a (format "\x1b;xsave-file! ~s\r" path))
                     (head-wait 'pre-save-disk-write-is-reviewed a (lambda () (head-sees? a "changed on disk")))
                     (head-send! a "c")
                     (test:check 'cancel-keeps-a-pre-save-hooks-disk-write
                       (list (head-read a '(begin (kernel:retract-module! 'wire-save-hook)
                                             (head:buffer-facts-set! (current-buffer) '((file . #f) (base . #f))) #t))
                             (call-with-input-file path get-string-all) (car (rpc head 'snapshot id)))
                       '(#t "from hook\n" #("shared text B")))
                     ;; Each destructive choice reviews both the disk and its
                     ;; source. Keep the real prompt open through the change;
                     ;; rejected choices preserve text, facts and head history.
                     (for-each
                       (lambda (scenario)
                         (let ([command (car scenario)] [answer (cadr scenario)] [change (caddr scenario)])
                           (write-text path "disk\n")
                           (let ([target (rpc head 'create "file review" '("keep")
                                           `((file . ,(and (not (eq? command 'save-as)) path))
                                             (base . "base\n") (trailing . #t)))])
                             (head-read a
                               `(begin (show-buffer! (head:adopt-store-buffer! ,target))
                                       (insert-text! "mine ")
                                       (head:buffer-marked-set! (current-buffer) #t) #t))
                             (head-send! a (format "\x1b;x~a ~s\r"
                                             (if (eq? command 'save-as) 'save-file! command) path))
                             (head-wait 'file-review a
                               (lambda () (head-sees? a (if (eq? command 'save-as) "exists; overwrite?" "changed on disk"))))
                             (case change
                               [(text) (rpc head 'edit target (cadr (rpc head 'snapshot target)) '(0 0 0 0) '("new "))]
                               [(facts) (rpc head 'properties target '((trailing . #f)))]
                               [(file) (rpc head 'properties target `((file . ,(string-append path ".other"))))]
                               [(base) (rpc head 'properties target '((base . "new baseline\n")))]
                               [(disk) (write-text path "later\n")]
                               [(deleted) (delete-file path)]
                               [(unreadable) (delete-file path) (mkdir path)])
                             (let ([before (rpc head 'snapshot target)] [history (rpc head 'history target)])
                               (head-send! a answer)
                               (head-wait 'file-choice-cancelled a
                                 (lambda () (head-sees? a (if (eq? change 'unreadable) "Cannot verify" "cancelled"))))
                               (test:check (list scenario 'file-review-preserves-newer-work)
                                 (list (equal? before (rpc head 'snapshot target))
                                   (equal? history (rpc head 'history target))
                                   (head-read a '(let ([b (current-buffer)])
                                                   (list (head:buffer-marked b)
                                                         (pair? (vector-ref (head:buffer-history b) 0)))))
                                   (cond [(file-directory? path) 'directory]
                                         [(file-exists? path) (call-with-input-file path get-string-all)]
                                         [else #f]))
                                 (list #t #t '(#t #t)
                                   (case change [(disk) "later\n"] [(deleted) #f] [(unreadable) 'directory]
                                     [else "disk\n"]))))
                             (when (eq? change 'unreadable) (delete-directory path))
                             (when (and (eq? change 'disk) (member answer '("o" "y")))
                               (head-send! a (format "\x1b;xsave-file! ~s\r" path))
                               (head-wait 'new-overwrite-review a
                                 (lambda () (head-sees? a (if (eq? command 'save-as) "exists; overwrite?" "changed on disk"))))
                               ;; Identical bytes with a new stamp still have consent.
                               (write-text path "later\n")
                               (head-send! a answer)
                               (test:check 'fresh-overwrite-accepts-unchanged-content-and-invalidates-the-stamp
                                 (list (head-read a
                                         '(let ([b (current-buffer)])
                                            (list (head:buffer-base b) (head:buffer-modified b) (head:buffer-stamp b))))
                                       (call-with-input-file path get-string-all))
                                 '(("mine keep\n" #f #f) "mine keep\n")))
                             (when (memq change '(text facts))
                               (head-send! a (format "\x1b;xvisit-file! ~s\r" path))
                               (head-wait 'reread-reviewed-again a (lambda () (head-sees? a "changed on disk")))
                               (head-send! a "r")
                               (test:check 'fresh-reread-adopts-the-disk-and-clears-only-accepted-history
                                 (head-read a
                                   '(let ([b (current-buffer)])
                                      (list (head:buffer-lines b) (head:buffer-modified b)
                                        (head:buffer-marked b) (head:buffer-history b)
                                        (store:history (head:buffer-store-id b)))))
                                 '(#("disk") #f #f #(() ()) ())))
                             (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t))
                             (rpc head 'delete target))))
                       '((visit-file! "r" text) (visit-file! "r" facts)
                         (visit-file! "r" disk) (visit-file! "m" disk)
                         (save-file! "o" disk) (save-file! "o" deleted)
                         (save-file! "o" unreadable) (save-file! "o" file)
                         (save-file! "m" base) (save-file! "m" disk)
                         (save-as "y" disk) (save-as "y" file))))

                   ;; No common ancestor exists when a newly visited path
                   ;; appears on disk before first save. Keep both valid exits.
                   (for-each
                     (lambda (baseline)
                       (let* ([path (string-append root "/missing-save.txt")]
                              [target (head-read a
                                        `(begin (visit-file! ,path) (insert-text! "mine")
                                           (when (not ',baseline) (head:buffer-base-set! (current-buffer) #f))
                                           (head:buffer-store-id (current-buffer))))])
                         (write-text path "disk\n")
                         (head-send! a (format "\x1b;xsave-file! ~s\r" path))
                         (head-wait 'no-merge-ancestor a (lambda () (head-sees? a "no saved baseline")))
                         (let* ([choices? (not (head-sees? a "merge"))]
                                [before (rpc head 'snapshot target)] [history (rpc head 'history target)]
                                [cancelled
                                 (begin
                                   (head-read a '#t "mc")
                                   (list (equal? before (rpc head 'snapshot target))
                                         (equal? history (rpc head 'history target))
                                         (call-with-input-file path get-string-all)))])
                           (head-send! a (format "\x1b;xsave-file! ~s\r" path))
                           (head-wait 'review-no-ancestor-again a (lambda () (head-sees? a "no saved baseline")))
                           (head-send! a "o")
                           (test:check (list baseline 'missing-ancestor-keeps-cancel-and-overwrite)
                             (list choices? cancelled
                                   (head-read a '(let ([b (current-buffer)])
                                                   (list (head:buffer-lines b) (head:buffer-base b)
                                                         (head:buffer-modified b))))
                                   (equal? history (rpc head 'history target))
                                   (call-with-input-file path get-string-all))
                             '(#t (#t #t "disk\n") (#("mine") "mine\n" #f) #t "mine\n")))
                         (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t))
                         (rpc head 'delete target)
                         (delete-file path)))
                     '(absent #f))

                   (let ([doomed (rpc head 'create "kill review" '("work"))])
                     (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,doomed)) #t))
                     (head-send! a "\x18;k\r")
                     (head-wait 'kill-review a (lambda () (head-sees? a "modified; kill anyway")))
                     (rpc head 'edit doomed 0 '(0 4 0 4) '("!"))
                     (rpc head 'rename doomed "kill review later")
                     (head-wait 'kill-review-advanced a (lambda () (head-sees? a "work!")))
                     (head-send! a "y")
                     (head-wait 'kill-reviews-the-new-state a
                       (lambda () (head-sees? a "Buffer kill review later modified")))
                     (head-send! a "n")
                     (test:check 'kill-confirmation-cannot-discard-a-racing-edit
                       (list (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,id)) #t))
                             (car (rpc head 'snapshot doomed))) '(#t #("work!")))
                     (rpc head 'delete doomed))

                   (let ([scroll (rpc head 'create "scrolling"
                                   (map (lambda (n) (format "row ~3,'0d" n)) (iota 80)))])
                     (head-read a `(begin (show-buffer! (head:adopt-store-buffer! ,scroll))
                                     (split-window-right!) #t))
                     (head-wait 'split-before-scroll a (lambda () (head-sees? a "scrolling")))
                     (vector-set! a 3 "")
                     (head-send! a (apply string-append (make-list 30 "\x1b;[B")))
                     (head-wait 'held-down-scroll a (lambda () (head-sees? a "row 030")))
                     (let* ([frames (vector-ref a 3)] [opened (occurrences frames "\x1b;[?2026h")]
                            [closed (occurrences frames "\x1b;[?2026l")])
                       (test:check 'attached-split-scrolling-uses-balanced-2026
                         (list (> opened 0) (= opened closed) (head-read a '(point))) '(#t #t (30 . 0))))
                     ;; The attached head also services resize while idle.
                     ;; Observe geometry and the pending question's new width
                     ;; before any more input; the other head keeps its size.
                     (head-read a
                       '(let ([owner (get-thread-id)])
                          (parameterize ([kernel:registering-module 'wire-resize])
                            (paint:add-status-hint!
                              (lambda ()
                                (format "~ax~a/~a" (paint:screen-rows) (paint:screen-cols)
                                  (= owner (get-thread-id))))))
                          (echo:settle!) #t))
                     (let* ([question "This pending question expands to the new terminal width before another key."]
                            [ticket (rpc agent 'ask '(head "screen A") question '())])
                       (head-wait 'attached-resize-question-starts-elided a
                         (lambda () (and (head-sees? a "This pending que") (not (head-sees? a question)))))
                       (vector-set! a 3 "")
                       (vt:emulator-resize! (vector-ref a 2) 18 120)
                       (sys:resize-terminal-process! (vector-ref a 0) 18 120)
                       (head-wait 'attached-idle-resize-refits-question a
                         (lambda ()
                           (and (head-sees? a "18x120/#t") (head-sees? a question)
                                (let ([frames (vector-ref a 3)])
                                  (and (> (occurrences frames "\x1b;[?2026h") 0)
                                       (= (occurrences frames "\x1b;[?2026h")
                                          (occurrences frames "\x1b;[?2026l")))))))
                       (rpc agent 'cancel ticket))
                     (test:check 'attached-resize-keeps-the-other-head-size
                       (head-read b '(list (paint:screen-rows) (paint:screen-cols))) '(24 80))
                     (head-read a '(begin (kernel:retract-module! 'wire-resize) #t))
                     (vt:emulator-resize! (vector-ref a 2) 24 80)
                     (sys:resize-terminal-process! (vector-ref a 0) 24 80)
                     (head-read a `(begin (delete-other-windows!)
                                     (show-buffer! (head:adopt-store-buffer! ,id)) #t))
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
                          (describe:show! 'attached-document)
                          (let* ([page (reference:page head:ui-actor)]
                                 [source (head:buffer-of-store-id (car page))]
                                 [entry (car (reference:lookup 'attached-document))])
                            (string-set! (doc:description entry) 0 #\Y)
                            (list (head:buffer-fact source 'audience #f)
                                  (head:buffer-read-only source)
                                  (not (head:buffer-store-id (markdown:companion source)))
                                  (doc:description entry)
                                  (and (member "Original head documentation" (vector->list (head:buffer-lines source))) #t)))))
                     '(((head "screen A")) #t #t "Original head documentation" #t))
                   (test:check 'attached-document-scope-and-retraction
                     (list (head-read b '(reference:lookup 'attached-document))
                           (head-read a
                             '(begin (kernel:retract-module! 'attached-document)
                                (describe:show! 'describe:show!)
                                (reference:lookup 'attached-document))))
                     '(() ()))
                   (head-read a '(begin (terminal:open!! "printf 'attached terminal'; read answer; printf '\\n%s' \"$answer\"; read done") #t))
                   (head-wait 'attached-terminal a (lambda () (head-sees? a "attached terminal")))
                   (let ([terminal-id (head-read a '(head:buffer-store-id (current-buffer)))])
                     (head-read b `(begin (show-buffer! (head:adopt-store-buffer! ,terminal-id)) #t))
                     (head-wait 'shared-terminal-surface b (lambda () (head-sees? b "attached terminal")))
                     (head-read a '(begin (terminal:send! "through base\n") #t))
                     (head-wait 'shared-terminal-input b (lambda () (head-sees? b "through base")))
                     (test:check 'terminal-output-keeps-authorship-without-tints
                       (map head-blame (list a b))
                       (make-list 2 (list '() (cdr (assq 'app (caddr (rpc head 'snapshot terminal-id)))))))
                     (head-read a '(begin (delete-other-windows!) (head:set-kill-ring! "screen A kill") #t))
                     (head-read b '(begin (head:set-kill-ring! "screen B kill")
                                          (terminal:toggle-capture!) #t))
                     (test:check 'shared-terminal-capture-is-local-to-each-head
                       (list (head-read a '(head:full-capture? (selected-window)))
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
                         (head-send! a "\x1b;xanswer!!\r")
                         (head-wait 'offline-question-prompt a (lambda () (head-sees? a "Continue agent work?")))
                         (head-send! a "yes\r")
                         (test:check 'reattached-human-answers-the-surviving-agent-once
                           (list (receive-reply second) (rpc second 'cancel question)
                                 (rpc head 'answer question "wrong head")
                                 (head-read a '(actor:pending head:ui-actor)))
                           '((event (answer 73 "yes")) #f #f ())))
                       ;; A real human head controls the existing session
                       ;; inventory, including agents routed to another head.
                       ;; Revocation wakes an idle reader and removes watches
                       ;; through the ordinary connection cleanup.
                       (head-read a '(begin (echo:settle!) #t))
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
                         (list (head-read again '(list (head:buffer-store-id (current-buffer))
                                                       (length (head:windows)) (head:kill-ring)))
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
                              (show-buffer! (head:adopt-store-buffer! ,plain))
                              (split-window-right!) (other-window!)
                              (let ([source (head:adopt-store-buffer! ,source)])
                                (mode:choose! source "markdown")
                                (show-buffer! (markdown:companion! source "<resume view>")))
                              (goto-point! (cons (let find ([row 0])
                                                   (if (string=? (buffer-line (current-buffer) row) "After table")
                                                     row (find (+ row 1)))) 2))
                              (other-window!) (wrap! #f) (split-window!)
                              (head:layout-split-first-weight-set! (head:root) 2)
                              (head:layout-split-second-weight-set! (head:root) 3)
                              (head:layout-split-first-weight-set! (head:layout-split-first (head:root)) 2)
                              (head:layout-split-second-weight-set! (head:layout-split-first (head:root)) 1)
                              (goto-point! '(25 . 3)) (head:window-top-set! (head:current) 20)
                              (head:buffer-mark-row-set! (current-buffer) 26)
                              (head:buffer-mark-col-set! (current-buffer) 4)
                              (head:buffer-marked-set! (current-buffer) #t)
                              (let ([other (head:window-numbered 2)])
                                (head:window-prow-set! other 50) (head:window-pcol-set! other 4)
                                (head:window-top-set! other 45)) #t))
                         (let ([before (screen-state again)])
                           ;; Completion borrows the selected window. A wake
                           ;; in that modal loop must not checkpoint its chrome.
                           (head-send! again "\x1b;xhead:window-\t")
                           ;; Narrow status lines prioritize the page count;
                           ;; observe a candidate rather than the view label.
                           (head-wait 'completions-before-loss again
                             (lambda () (head-sees? again "head:window-buffer")))
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
                             (test:check 'abrupt-reattach-rebuilds-layout-and-local-source-anchors
                               (list (screen-state resumed)
                                     (map (lambda (id) (rpc head 'snapshot id)) (list plain source)))
                               (list before truth))
                             (test:check 'resumed-marks-replace-old-window-and-region-names
                               (head-read resumed
                                 `(let* ([marks (store:marks head:ui-actor ,plain)] [region (cdr (assq 'region marks))])
                                    (list (length marks)
                                          (text:span-start region) (text:span-end region))))
                               '(5 (26 . 3) (27 . 4)))
                             (head-send! resumed "\x18;\x03;")
                             (head-wait 'detach-before-input-removal resumed
                               (lambda () (not (member '(head "screen A") (map car (rpc head 'actors))))))
                             (rpc head 'properties plain '((audience)))
                             (rpc head 'delete source)
                             (let ([fallback (start-head "screen A")])
                               (head-wait 'missing-input-fallback fallback (lambda () (head-sees? fallback "shared text B")))
                               (test:check 'missing-or-hidden-inputs-fall-back-without-disturbing-another-screen
                                 (list (head-read fallback '(map (lambda (w) (head:buffer-store-id (head:window-buffer w)))
                                                              (head:windows)))
                                       (head-read b '(list (length (head:windows))
                                                           (head:buffer-store-id (current-buffer)) (head:kill-ring))))
                                 (list (make-list 3 id) (list 1 terminal-id "screen B kill")))))))))
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
                                 (cdr (assq 'alive (caddr (rpc head 'snapshot 3))))) '(#t #t))))
                     '((inbox-count 512 8192) (inbox-bytes 24 2097152))))))
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
           (let ([listener (sys:listen-local socket)]) (sys:close-local-listener! listener))
           (write-text socket "ordinary file")
           (test:check 'ordinary-file-at-socket-path-is-preserved
             (list (test:raises? (lambda () (sys:listen-local socket)))
                   (test:raises? (lambda () (sys:connect-local socket)))
                   (call-with-input-file socket get-string-all)) '(#t #t "ordinary file"))
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
                               (lambda (ex) (and (string:search (kernel:condition-text ex) "hello timed out" 0
                                                   (string-length (kernel:condition-text ex))) #t))))
                           (lambda () (sys:close-connection! client) (sys:close-connection! server)))))
                     '(#vu8(0) #vu8(0 0 0 4 40) #f)) '(#t #t #t))
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
           (write-forms (string-append root "/base-config.e")
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

           ;; Reuse this installation for the actual automatic bootstrap.
           ;; The fixture's own config can stop or crash its own base; tests
           ;; never signal a pid read from a possibly stale process record.
           (write-forms (string-append root "/base-config.e")
             `((store:create! '(base e) "bootstrap" '("ready")
                 (list (cons 'process-id (get-process-id)) (cons 'directory (current-directory))))
               (fork-thread
                 (lambda ()
                   (let wait ()
                     (unless (file-exists? ,automatic-control)
                       (sleep (make-time 'time-duration 5000000 0)) (wait)))
                   (if (eq? (call-with-input-file ,automatic-control read) 'crash)
                       (system (format "kill -KILL ~a" (get-process-id)))
                       (kernel:mailbox-post! daemon:control 'signal))))))
           (write-forms (string-append root "/config.e") '((main:set-startup-page! #f)))
           (write-text (string-append base-directory "/log/2000-01-01.log") "expired")
           (write-text (string-append base-directory "/log/keep.txt") "keep")
           ;; A cold cache exercises concurrent compilation before both heads
           ;; race to exec a base and contend on the same lifetime flock.
           (test:check 'concurrent-cold-compilers-share-a-consistent-cache
             (map (lambda (round)
                    (remove-tree! objects)
                    (test:parallel 3 (lambda (index) (loader-exit '("--help"))))) '(1 2 3 4))
             (make-list 4 (make-list 3 '(0 ""))))
           (remove-tree! objects)
           (let* ([a (start-head "auto α's desk")] [b (start-head "auto B")])
             (for-each (lambda (head) (head-wait 'automatic-head head (lambda () (head-sees? head "*scratch*"))))
               (list a b))
             (let* ([pid-path (string-append base-directory "/pid")]
                    [record (call-with-input-file pid-path read)]
                    [boot-pid '(store:property (store:find-named "bootstrap") 'process-id)])
               (test:check 'cold-start-race-shares-one-base-and-preserves-head-directory
                 (list (head-read a boot-pid) (head-read b boot-pid)
                       (head-read a '(current-directory))
                       (head-read b '(store:property (store:find-named "bootstrap") 'directory))
                       (map get-mode (list base-directory socket pid-path (string-append base-directory "/lock")))
                       (file-exists? (string-append base-directory "/log/2000-01-01.log"))
                       (file-exists? (string-append base-directory "/log/keep.txt")))
                 (list (cadr record) (cadr record) root base-directory '(#o700 #o600 #o600 #o600) #f #t))
               (head-read a '(begin (insert-text! "retained")
                                    (head:add-shutdown-hook! (lambda () (goto-point! '(0 . 3)))) #t))
               (for-each (lambda (head) (head-send! head "\x18;\x03;")) (list a b))
               (for-each (lambda (head)
                           (head-wait 'automatic-detach head (lambda () (head-sees? head "e: detached")))
                           (sys:reap-terminal-process! (vector-ref head 0))) (list a b))
               (let* ([again (start-head "auto α's desk")] [inspector (connect)]
                      [leavers (list (connect) (connect))])
                 (head-wait 'automatic-resume again (lambda () (head-sees? again "retained")))
                 (test:check 'quit-keeps-shared-edits-checkpoint-and-shell-quoted-resume
                   (list (head-read again '(list (buffer-line (current-buffer) 0) (point)))
                         (equal? record (call-with-input-file pid-path read))
                         (> (occurrences (vector-ref a 3) "--name 'auto α'\\''s desk'") 0))
                   '(("retained" (0 . 3)) #t #t))
                 (hello inspector '(head "inspector"))
                 (for-each (lambda (connection name) (hello connection (list 'head name))) leavers '("leave A" "leave B"))
                 (let* ([before (cdr (assq 'heads (rpc inspector 'status)))]
                        [departures (test:parallel 2 (lambda (index) (rpc (list-ref leavers index) 'leaving #f)))])
                   (test:check 'concurrent-departures-commit-before-their-replies
                     (list (list-sort < (map (lambda (status) (cdr (assq 'heads status))) departures))
                           (cdr (assq 'heads (rpc inspector 'status))))
                     (list (list (- before 2) (- before 1)) (- before 2))))
                 (for-each sys:close-connection! (cons inspector leavers))
                 (write-text automatic-control "crash")
                 (head-wait 'unexpected-base-death again (lambda () (head-sees? again "e: the base is gone")))
                 (sys:reap-terminal-process! (vector-ref again 0))
                 (test:check 'crash-leaves-recoverable-endpoints
                   (map file-exists? (list socket pid-path (string-append base-directory "/lock"))) '(#t #t #t))
                 (delete-file automatic-control)
                 (let ([fresh (start-head "after crash")])
                   (head-wait 'stale-endpoints-recovered fresh (lambda () (head-sees? fresh "*scratch*")))
                   (test:check 'stale-cleanup-starts-a-fresh-in-memory-session
                     (list (not (equal? record (call-with-input-file pid-path read)))
                           (head-read fresh '(buffer-line (current-buffer) 0))) '(#t ""))
                   (write-text automatic-control "stop")
                   (head-wait 'announced-base-stop fresh (lambda () (head-sees? fresh "e: the base stopped (signal)")))
                   (sys:reap-terminal-process! (vector-ref fresh 0))
                   (test:await 'automatic-base-cleans-up (lambda () (not (file-exists? pid-path))))
                   (test:check 'all-automatic-exits-restore-the-terminal
                     (map (lambda (head)
                            (let ([state (vt:emulator-state (vector-ref head 2))])
                              (list (> (occurrences (vector-ref head 3) "\x1b;[?1049l") 0)
                                    (cdr (assq 'mouse-tracking state)) (cdr (assq 'sgr-mouse state)))))
                       (list a b again fresh)) (make-list 4 '(#t #f #f)))))))
           (shutdown-scenarios!)
           (final-shutdown-scenarios!))
         (lambda ()
           (write-text edit-release "continue")
           (write-text automatic-control "stop")
           (guard (ex [else (void)]) (stop!))
           (for-each sys:close-connection! clients)
           (for-each (lambda (head) (sys:close-terminal-process! (vector-ref head 0))) heads)
           (test:await 'automatic-fixture-releases-ownership
             (lambda ()
               (let ([lock (sys:acquire-file-lock (string-append base-directory "/lock"))])
                 (and lock (begin (sys:release-file-lock! lock) #t)))))
           (remove-tree! root))))
     (test:finish! 'wire)))
