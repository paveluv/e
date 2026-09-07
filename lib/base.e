;; base.e -- process lifetime and the local daemon. No head imports.
(library (base)
  (export call-with-runtime run connection-policy)
  (import (chezscheme)
          (prefix (kernel) kernel:) (prefix (startup) startup:)
          (prefix (sys) sys:) (prefix (wire) wire:)
          (prefix (store) store:) (prefix (actor) actor:)
          (prefix (policy) policy:) (prefix (text) text:) (prefix (datum) datum:)
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
    (dynamic-wind void
      (lambda ()
        (actor:call-as '(base e)
          (lambda ()
            (let ([failures (kernel:load-modules! modules)])
              (unless (null? failures) (raise (cdar failures))))
            (let ([result (kernel:load-config! 'base)])
              (when (condition? result) (raise result)))))
        (thunk))
      vt:close-all!))

  (define (request session operation args)
    (define (arity n)
      (unless (= (length args) n) (error 'wire "wrong request arity" operation)))
    (case operation
      [(buffers actors)
       (arity 0)
       (if (eq? operation 'actors) (actor:attached)
           (sort < (store:buffer-list)))]
      [(name snapshot)
       (arity 1)
       (let ([id (car args)])
         ;; Audience is view routing, as in the store; it is not a read ACL.
         (if (eq? operation 'name) (store:buffer-name id)
             (let-values ([(lines revision facts) (store:snapshot-state id)])
               (list lines revision facts))))]
      [(edit)
       (arity 4)
       (unless (and (integer? (cadr args)) (exact? (cadr args)) (>= (cadr args) 0))
         (error 'wire "expected a nonnegative basis revision"))
       (call-with-values
         (lambda () (policy:session-edit! session (car args) (cadr args)
                      (text:datum->span (caddr args)) (cadddr args))) list)]
      [(undo)
       (unless (<= 1 (length args) 2) (error 'wire "expected buffer and optional undo scope"))
       (call-with-values (lambda () (apply policy:session-undo! session args)) list)]
      [else (error 'wire "unknown request" operation)]))

  (define (serve-connection connection)
    (let ([owner (list 'connection connection)] [out (kernel:make-mailbox)] [writer #f] [session #f])
      (define (post! message) (kernel:mailbox-post! out message))
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
                     [capabilities (if (null? (policy:buffers p)) '(read) '(read edit undo))])
                (set! session (policy:mint! actor p (and (eq? (car actor) 'head) actor)))
                ;; Queue hello before publishing; name refusal still revokes
                ;; this connection's session without touching the old owner.
                (post! (list 'hello wire:version actor capabilities))
                (parameterize ([kernel:registering-module owner])
                  (actor:register! actor (lambda (message) (post! (list 'event message))) capabilities)))
              (set! writer
                (fork-thread
                  (lambda ()
                    (guard (ex [else (sys:close-connection! connection)])
                      (let loop ()
                        (let ([message (kernel:mailbox-receive! out)])
                          (when message
                            (wire:send! (sys:connection-output connection) message)
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
                              (request session (caddr message) (cdddr message)))))
                        (loop)))))))))
        (lambda ()
          (when session (policy:revoke! session))
          (kernel:retract-module! owner)
          (sys:close-connection! connection)
          (post! #f)
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
