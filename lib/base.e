;; base.e -- process lifetime and the local daemon. No head imports.
(library (base)
  (export call-with-runtime run connection-policy)
  (import (chezscheme)
          (prefix (kernel) kernel:) (prefix (startup) startup:)
          (prefix (sys) sys:) (prefix (wire) wire:)
          (prefix (store) store:) (prefix (actor) actor:)
          (prefix (policy) policy:) (prefix (text) text:) (prefix (datum) datum:)
          (prefix (log) log:)
          (prefix (file) file:) (prefix (vt) vt:))

  (define modules
    '("actor" "datum" "diff" "doc" "file" "git" "https" "log" "policy"
      "reference" "sandbox" "startup" "store" "string" "surface" "sys" "text" "vt" "wire"))

  ;; Base configuration selects permissions from the admitted local identity.
  ;; The hello supplies no grants. Agent write access must be selected here.
  (define connection-policy
    (make-parameter
      (lambda (actor)
        (if (eq? (car actor) 'head) (policy:make 'all 100000000 'any 8000)
            (policy:reader)))))

  (define (call-with-runtime thunk)
    ;; Pin before config can start active work. Plain e owns this same base
    ;; lifetime; ending a head connection never enters this cleanup.
    (kernel:pin-modules! (cons "base" modules))
    (let ([audit #f])
      (dynamic-wind void
        (lambda ()
          ;; One producer for every head and for work while all heads are
          ;; absent. Log small operation facts, never retained text/deltas.
          (set! audit (store:subscribe! #f audit-store-event!))
          (actor:call-as '(base e)
            (lambda ()
              (let ([failures (kernel:load-modules! modules)])
                (unless (null? failures) (raise (cdar failures))))
              (let ([result (kernel:load-config! 'base)])
                (when (condition? result) (raise result)))))
          (thunk))
        (lambda ()
          (dynamic-wind void vt:close-all!
            (lambda () (when audit (store:unsubscribe! audit))))))))

  (define (audit-store-event! event)
    (let* ([kind (car event)] [id (cadr event)]
           [actor (if (eq? kind 'delete) (caddr event) (cadddr event))]
           [detail
            (case kind
              [(edit)
               (append (list 'edit id (caddr event)
                         (text:span->datum (text:delta-span (list-ref event 4))))
                 (list-tail event 5))]
              [(delete) (list 'delete id)]
              [else (list kind id (caddr event))])])
      (actor:call-as actor (lambda () (log:add! 'store detail #f)))))

  (define (request session operation args)
    (define (arity n)
      (unless (= (length args) n) (error 'wire "wrong request arity" operation)))
    (case operation
      [(buffers actors)
       (arity 0)
       (if (eq? operation 'actors) (actor:attached)
           (sort < (store:buffer-list)))]
      [(name)
       (arity 1)
       (store:buffer-name (car args))]
      [(snapshot)
       (unless (<= 1 (length args) 2) (error 'wire "expected buffer and optional basis"))
       (when (pair? (cdr args))
         (unless (and (integer? (cadr args)) (exact? (cadr args)) (>= (cadr args) 0))
           (error 'wire "expected a nonnegative basis revision")))
       ;; Audience is view routing, not a read ACL. Optional changes end at
       ;; exactly this text/facts snapshot and use the edit receipt's codec.
       (datum:copy (call-with-values (lambda () (apply store:snapshot-state args)) list)
         text:delta->datum)]
      [(edit)
       (unless (<= 4 (length args) 5) (error 'wire "expected buffer, basis, span, lines and optional context"))
       (unless (and (integer? (cadr args)) (exact? (cadr args)) (>= (cadr args) 0))
         (error 'wire "expected a nonnegative basis revision"))
       (call-with-values
         (lambda () (apply policy:session-edit! session (car args) (cadr args)
                      (text:datum->span (caddr args)) (cadddr args) (list-tail args 4))) list)]
      [(undo)
       (unless (<= 1 (length args) 2) (error 'wire "expected buffer and optional undo scope"))
       (call-with-values (lambda () (apply policy:session-undo! session args)) list)]
      [(redo)
       (arity 1)
       (call-with-values (lambda () (policy:session-redo! session (car args))) list)]
      [else (error 'wire "unknown request" operation)]))

  (define (serve-connection connection)
    (let ([owner (list 'connection connection)] [out (kernel:make-mailbox)]
          [out-lock (make-mutex)] [queued-bytes 0] [queued-count 0] [closed? #f]
          [writer #f] [session #f] [changes #f])
      (define (close!)
        (when (with-mutex out-lock
                (and (not closed?) (begin (set! closed? #t) #t)))
          (sys:close-connection! connection)
          (kernel:mailbox-post! out #f)))
      (define (post! message)
        (guard (ex [else (close!) (raise ex)])
          (when (with-mutex out-lock closed?) (error 'wire "connection is closed"))
          ;; #t is one coalesced watch wakeup; all other work is owned bytes.
          ;; Count includes an in-flight write. A stalled peer cannot retain
          ;; unlimited store versions, tiny mail envelopes or encoded replies.
          (let* ([frame (if (eq? message #t) #t (wire:encode message))]
                 [size (if (bytevector? frame) (bytevector-length frame) 0)])
            (unless (with-mutex out-lock
                      (and (not closed?) (< queued-count 256)
                           (<= (+ queued-bytes size) #x2000000)
                           (begin
                             (set! queued-count (+ queued-count 1))
                             (set! queued-bytes (+ queued-bytes size))
                             (kernel:mailbox-post! out frame) #t)))
              ;; Overload is a disconnect, never a silently dropped reply or
              ;; actor message. Only invalidations may coalesce.
              (error 'wire "pending output limit reached")))))
      (define (watch!)
        (unless changes
          ;; Publish the take procedure before the writer can consume a wake.
          ;; Store callbacks run outside its lock; post! takes only out-lock.
          (with-mutex out-lock
            (parameterize ([kernel:registering-module owner])
              (let-values ([(token take!) (store:watch! (lambda () (post! #t)))])
                (set! changes take!)))))
        ;; Subscribe before inventory so a racing commit is in one or both.
        (sort < (store:buffer-list)))
      (dynamic-wind void
        (lambda ()
          (guard (ex [else
                      (unless writer
                        (guard (ignored [else (void)])
                          (wire:send! (sys:connection-output connection)
                            (list 'error #f (kernel:condition-text ex)))))])
            (let* ([hello (wire:receive (sys:connection-input connection))]
                   [actor (and (list? hello) (= (length hello) 3)
                               (eq? (car hello) 'hello) (equal? (cadr hello) wire:version)
                               (caddr hello))])
              (unless (and (actor:identity? actor) (= (length actor) 2)
                           (memq (car actor) '(head agent)) (string? (cadr actor)))
                (error 'wire "expected (hello 1 (head-or-agent name))"))
              (let* ([p ((connection-policy) (datum:copy actor))]
                     [capabilities (if (null? (policy:buffers p)) '(read) '(read edit undo redo))])
                (set! session (policy:mint! actor p (and (eq? (car actor) 'head) actor)))
                ;; Queue hello before publishing; name refusal still revokes
                ;; this connection's session without touching the old owner.
                (post! (list 'hello wire:version actor capabilities))
                (parameterize ([kernel:registering-module owner])
                  (actor:register! actor (lambda (message) (post! (list 'event message))) capabilities)))
              (set! writer
                (fork-thread
                  (lambda ()
                    (guard (ex [else (close!)])
                      (let loop ()
                        (let ([item (kernel:mailbox-receive! out)])
                          (when item
                            (let ([frame
                                   (if (bytevector? item) item
                                       (wire:encode (list 'changed ((with-mutex out-lock changes)))))])
                              (put-bytevector (sys:connection-output connection) frame)
                              (flush-output-port (sys:connection-output connection)))
                            (with-mutex out-lock
                              (set! queued-count (- queued-count 1))
                              (when (bytevector? item)
                                (set! queued-bytes (- queued-bytes (bytevector-length item)))))
                            (loop))))))))
              (actor:call-as actor
                (lambda ()
                  (let loop ()
                    (let ([message (wire:receive (sys:connection-input connection))])
                      (unless (eof-object? message)
                        (unless (and (list? message) (>= (length message) 3)
                                     (eq? (car message) 'request)
                                     (integer? (cadr message)) (exact? (cadr message)) (>= (cadr message) 0)
                                     (symbol? (caddr message)))
                          (error 'wire "expected (request id operation argument ...)"))
                        (post!
                          (guard (ex [else (list 'reply (cadr message) 'error (kernel:condition-text ex))])
                            (list 'reply (cadr message) 'ok
                              (if (eq? (caddr message) 'watch)
                                  (if (= (length message) 3) (watch!) (error 'wire "watch takes no arguments"))
                                  (request session (caddr message) (cdddr message))))))
                        (loop)))))))))
        (lambda ()
          (close!)
          (when session (policy:revoke! session))
          (kernel:retract-module! owner)
          (when writer (thread-join writer))))))

  (define (run)
    ;; Foreground for a supervisor or shell background job; SIGHUP leaves the
    ;; base alive. A process stop is distinct from a connection disconnect.
    (let* ([path (file:canonical (file:expand (startup:socket)))]
           [directory (file:directory-part path)]
           [control (kernel:make-mailbox)] [lock (make-mutex)] [finished (make-condition)]
           [connections '()] [stopping? #f] [acceptor #f])
      (unless (file-exists? directory) (mkdir directory #o700))
      (let ([listener (sys:listen-local path)])
        (dynamic-wind void
          (lambda ()
            (sys:watch-daemon-signals! (lambda () (kernel:mailbox-post! control 'stop)))
            (set! acceptor
              (fork-thread
                (lambda ()
                  (guard (ex [else (kernel:mailbox-post! control ex)])
                    (let loop ()
                      (let ([connection (sys:accept-local listener)])
                        (when connection
                          (with-mutex lock
                            (if stopping? (sys:close-connection! connection)
                                (begin
                                  (set! connections (cons connection connections))
                                  (fork-thread
                                    (lambda ()
                                      (dynamic-wind void
                                        (lambda () (serve-connection connection))
                                        (lambda ()
                                          (with-mutex lock
                                            (set! connections (remq connection connections))
                                            (condition-broadcast finished)))))))))
                          (loop))))))))
            (format #t "e: listening on ~a\n" path)
            (flush-output-port (current-output-port))
            ;; This pump does not evaluate code. Force an interrupt check on
            ;; each wake: idle Scheme can otherwise take many timed waits to
            ;; spend Chez's ordinary call counter and deliver an OS signal.
            (parameterize ([timer-interrupt-handler void])
              (let wait ()
                (set-timer 1)
                (let* ([now (current-time 'time-monotonic)]
                       [deadline (make-time 'time-monotonic (time-nanosecond now) (+ 1 (time-second now)))]
                       [message (kernel:mailbox-receive! control deadline)])
                  (cond [(not message) (wait)]
                    [(condition? message) (raise message)])))))
          (lambda ()
            (let ([active (with-mutex lock (set! stopping? #t) connections)])
              (sys:close-local-listener! listener)
              (for-each sys:close-connection! active)
              (when acceptor (thread-join acceptor))
              (with-mutex lock
                (let wait ()
                  (unless (null? connections)
                    ;; Client owners finish their endpoint and writer cleanup.
                    (condition-wait finished lock)
                    (wait))))))))))
)
