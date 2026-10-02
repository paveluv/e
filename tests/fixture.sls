;; Shared process fixtures: private bases, PTY readers and editor evaluation.
(library (fixture)
  (export start! stop! call-with-base command directory process diagnostics quote-shell
          terminal-reader evaluator evaluate composed)
  (import (except (chezscheme) process) (prefix (sys sys) sys:) (prefix (foundation string) string:)
          (prefix (foundation wire) wire:) (prefix (core kernel) kernel:))

  (define (evaluator installation base-directory who)
    ;; A wire client that only carries evaluation mail. Its hello must match
    ;; the base's wire version and fingerprint: normally the installation's
    ;; own, or -- when a scenario made the sources stale or restarted a base
    ;; from modified ones -- what the refusal itself advertises, since this
    ;; client loads no editor code.
    (define (attempt version fingerprint retry?)
      (let* ([deadline (sys:after 10)]
             [connection (sys:connect-local (string-append base-directory "/socket") deadline)]
             [reply
              (guard (ex [else (sys:close-connection! connection) (raise ex)])
                (sys:call-with-connection-deadline connection deadline
                  (lambda ()
                    (wire:send! (sys:connection-output connection) (list 'hello version who fingerprint))
                    (wire:receive (sys:connection-input connection)))))])
        (cond
          [(and (pair? reply) (eq? (car reply) 'hello)) connection]
          [(and retry? (list? reply) (= (length reply) 3) (eq? (car reply) 'error)
                (pair? (caddr reply)) (eq? (car (caddr reply)) 'stale-base) (pair? (cdr (caddr reply)))
                (list? (cadr (caddr reply)))
                (assq 'fingerprint (cadr (caddr reply))) (assq 'wire-version (cadr (caddr reply))))
           (let ([status (cadr (caddr reply))])
             (sys:close-connection! connection)
             (attempt (cdr (assq 'wire-version status)) (cdr (assq 'fingerprint status)) #f))]
          [else
           (sys:close-connection! connection)
           (error 'evaluator "the base refused the evaluator" who reply)])))
    (attempt wire:version
      (parameterize ([kernel:installation-directory installation]) (kernel:fingerprint)) #t))

  (define evaluation-token 0)
  ;; Explicit selectors shared by process-boundary fixtures. The product has
  ;; no implicit current-window API; these address the shipped test screen.
  (define (composed expression)
    `(begin
       (unless (top-level-bound? 'test-root)
         (kernel:load-module! "tui")
         (eval '(begin
                  (define (field record key) (cdr (assq key record)))
                  (define (test-root) (field (field (root:current) 'value) 'root))
                  (define (test-manager) (widget:descendant (test-root) 'content 'windows))
                  (define (test-window) (window:current (test-manager)))
                  (define (test-editor) (widget:descendant (test-window) 'document))
                  (define (test-document) (window:document (test-manager) (test-window)))
                  (define (test-point) (car (view:state (interaction:snapshot (test-editor)))))
                  (define (test-frame id)
                    (define (find-frame frame)
                      (if (equal? id (widget:frame-id frame)) frame
                        (exists find-frame (widget:frame-children frame))))
                    (exists (lambda (p) (find-frame (car p))) (widget:shown)))
                  (define (test-divider)
                    (let* ([id (cadar (view:children (interaction:snapshot (test-manager))))]
                           [f (test-frame id)] [d (widget:frame-descriptor f)]
                           [r (widget:frame-rect (car (widget:frame-children f)))]
                           [x? (eq? (field (view:options d) 'axis) 'x)])
                      (list (if x? 'right 'below) (+ (car r) (if x? (caddr r) 0))
                        (+ (cadr r) (if x? 0 (- (cadddr r) 1))))))
                  (define (test-show! document) (window-control:open-document! (test-window) document))
                  (define (test-go! point) (edit:select! (test-editor) point point))
                  (define (test-retire! id)
                    (interaction:flush!)
                    (let ([r (caddar (cadr (model:snapshots (list id))))])
                      (when r (view:retire! head:ui-actor id (field r 'revision)))))
                  (define (test-private! name text)
                    (store:create! head:ui-actor name (list text) (list (list 'audience head:ui-actor))))
                )))
       (kernel:evaluate! ',expression (interaction-environment))))
  (define (evaluated-payload message token)
    ;; The head answers with actor:send!, which the base delivers raw to a
    ;; wire client: (event (evaluated token ...)). Accept the mail envelope too.
    (and (pair? message) (eq? (car message) 'event) (pair? (cdr message))
         (let* ([event (cadr message)]
                [payload (cond [(and (pair? event) (eq? (car event) 'evaluated)) event]
                               [(and (list? event) (= (length event) 3) (eq? (car event) 'message)) (caddr event)]
                               [else #f])])
           (and (list? payload) (>= (length payload) 3) (eq? (car payload) 'evaluated)
                (eqv? (cadr payload) token) payload))))
  (define (evaluate connection who expression . timeout)
    ;; Ask the head `who` to evaluate a datum on its main thread, through the
    ;; E_TEST_EVAL mail it accepts, and read the printed result back. Replies
    ;; and unrelated events may interleave on the connection; a head that
    ;; never answers fails the caller instead of hanging the suite.
    (unless (and (<= (length timeout) 1)
                 (or (null? timeout) (and (real? (car timeout)) (< 0 (car timeout) +inf.0))))
      (error 'evaluate "expected an optional positive timeout in seconds" timeout))
    (set! evaluation-token (+ evaluation-token 1))
    (let ([token evaluation-token]
          [deadline (add-duration (current-time 'time-monotonic)
                      (let ([ns (exact (ceiling (* 1000000000 (if (null? timeout) 30 (car timeout)))))])
                        (make-time 'time-duration (mod ns 1000000000) (div ns 1000000000))))])
      (guard (ex [(sys:unresponsive? ex)
                  (error 'evaluate "no evaluation reply within the deadline" who expression)])
        (sys:call-with-connection-deadline connection deadline
          (lambda ()
            (wire:send! (sys:connection-output connection)
              (list 'request 1 'mail who (list 'evaluate token (format "~s" expression))))
            (let wait ()
              (let ([message (wire:receive (sys:connection-input connection))])
                (cond
                  [(eof-object? message) (error 'evaluate "the evaluator connection closed" who expression)]
                  [(and (pair? message) (eq? (car message) 'reply))
                   (unless (and (= (length message) 4) (eq? (caddr message) 'ok) (cadddr message))
                     (error 'evaluate "evaluation mail was not delivered" who expression message))
                   (wait)]
                  [(evaluated-payload message token)
                   => (lambda (payload)
                        (if (eq? (caddr payload) 'error)
                          (error 'evaluate "evaluation failed" expression (cadddr payload))
                          (read (open-input-string (caddr payload)))))]
                  [else (wait)]))))))))

  (define-record-type base (fields installation directory process))
  (define directory base-directory)
  (define process base-process)

  (define (quote-shell text)
    (string-append "'" (apply string-append
                         (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))

  (define (terminal-reader process consume!)
    ;; Feed available output as a chunk. Per-character emulator calls and
    ;; transcript concatenation made the PTY drivers unnecessarily expensive.
    ;; Remember EOF so callers can await exit without a separate blocking read.
    (let ([input (transcoded-port (sys:terminal-process-input process)
                   (make-transcoder (utf-8-codec) 'none 'replace))]
          [ended? #f])
      (lambda ()
        (let ([text
               (call-with-string-output-port
                 (lambda (output)
                   (let drain ()
                     (when (and (not ended?)
                                (guard (ex [else (set! ended? #t) #f]) (char-ready? input)))
                       (let ([c (guard (ex [else (eof-object)]) (get-char input))])
                         (if (eof-object? c) (set! ended? #t)
                             (begin (put-char output c) (drain))))))))])
          (unless (string=? text "") (consume! text))
          ended?))))

  (define (command base . args)
    (string-append "exec "
      (string:join
        (map quote-shell (append (list "scheme-script" (string-append (base-installation base) "/e")
                                   "--base-working-dir" (directory base)) args)) " ")))

  (define (diagnostics base)
    (let ([dir (string-append (directory base) "/log")])
      (if (not (file-directory? dir)) ""
          (string:join
            (map (lambda (name) (call-with-input-file (string-append dir "/" name) get-string-all))
              (list-sort string<? (directory-list dir))) "\n"))))

  (define (start! installation directory)
    (let* ([path (or directory (format "/tmp/e-base-~a-~a" (get-process-id) (random 1000000000)))]
           [process (sys:open-process (list "scheme-script" (string-append installation "/e")
                                        "--base" "--base-working-dir" path))]
           [base (make-base installation path process)]
           [deadline (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 120))])
      (guard (ex [else (sys:close-process! process) (raise ex)])
        (sys:write-process! process #f)
        (let wait ()
          (cond [(sys:process-status process)
                 (get-bytevector-all (sys:process-input process))
                 (let-values ([(code errors) (sys:process-result process)])
                   (error 'fixture "base failed to start" code errors (diagnostics base)))]
                [(sys:try-connect-local (string-append path "/socket") deadline)
                 => (lambda (connection)
                      (sys:close-connection! connection)
                      (let ([record (call-with-input-file (string-append path "/pid") read)])
                        (unless (= (cadr record) (sys:process-pid process))
                          (error 'fixture "another process owns the requested base directory" path))))]
                [else (sleep (make-time 'time-duration 50000000 0)) (wait)]))
        base)))

  (define stop!
    (case-lambda
      [(base) (stop! base 15)]
      [(base signal)
       (let* ([process (process base)]
              [deadline (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 5))])
         (dynamic-wind void
           (lambda ()
             (sys:signal-process! process signal)
             (let wait ()
               (unless (sys:process-status process)
                 (when (time>=? (current-time 'time-monotonic) deadline)
                   (error 'fixture "base did not stop" (diagnostics base)))
                 (sleep (make-time 'time-duration 10000000 0)) (wait)))
             (unless (= (sys:process-status process) 0)
               (error 'fixture "base exited unsuccessfully" (sys:process-status process) (diagnostics base)))
             (when (exists (lambda (name) (file-exists? (string-append (directory base) "/" name))) '("socket" "pid"))
               (error 'fixture "base left a socket or process record" (directory base)))
             (unless (file-exists? (string-append (directory base) "/lock"))
               (error 'fixture "base removed its lock inode" (directory base)))
             (let ([lock (sys:acquire-file-lock (string-append (directory base) "/lock"))])
               (unless lock (error 'fixture "base left a locked directory" (directory base)))
               (sys:release-file-lock! lock)))
           (lambda () (sys:close-process! process))))]))

  (define (call-with-base installation directory thunk)
    (let ([base (start! installation directory)])
      (dynamic-wind void (lambda () (thunk base)) (lambda () (stop! base)))))
)
