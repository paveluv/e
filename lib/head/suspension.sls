;; Linear (prompts) callers park at a command boundary, never in an input loop.
(import (only (foundation edoc) elibrary))
(elibrary (head suspension)
  (export call! cancel! close! drain! resolve! wait!)
  (import (chezscheme) (prefix (core identity) identity:)
          (prefix (head text-source) text-source:))

  (define-record-type task (fields actor notify (mutable escape) (mutable waiting)))
  (define-record-type ticket
    (fields task continuation (mutable state) (mutable value) (mutable cleanup)))
  (define current (make-parameter #f))
  (define pending '())
  (define ready '())

  (define (clean! ticket)
    (let ([proc (ticket-cleanup ticket)])
      (ticket-cleanup-set! ticket #f)
      (when proc (proc))))
  (define (forget! ticket)
    (set! pending (remq ticket pending))
    (task-waiting-set! (ticket-task ticket) #f))
  (define (drive! task thunk)
    ;; A resumed continuation still contains its original drive call. Its
    ;; mutable escape always returns to the *current* pump invocation.
    (let ([reply
           (call/1cc
             (lambda (escape)
               (task-escape-set! task escape)
               (parameterize ([current task])
                 (guard (ex [else
                             (let ([ticket (task-waiting task)])
                               (when ticket
                                 (ticket-state-set! ticket 'closed) (forget! ticket)
                                 (guard (ignored [else (void)]) (clean! ticket))))
                             ((task-escape task) (cons 'raised ex))])
                   (call-with-values thunk
                     (lambda ignored ((task-escape task) '(done))))))))])
      (task-escape-set! task #f)
      (when (eq? (car reply) 'raised) (raise (cdr reply)))))

  (edoc "Run a head command with a suspension boundary. An input request can park its continuation and return to the caller; only drain! resumes it. Command return values are discarded."
        (actor actor "head identity") (notify thunk "wake the ordinary pump") (thunk thunk "command"))
  (define (call! actor notify thunk)
    (unless (and (identity:valid? actor) (eq? (car actor) 'head) (procedure? notify) (procedure? thunk))
      (error 'call! "expected a head identity, wakeup and command"))
    (if (current) (thunk)
      (drive! (make-task actor notify #f #f) thunk)))

  (edoc "Park a linear prompt caller. Register receives a one-shot ticket and returns a cleanup thunk for its transient request. Resolution is delivered later by drain!. No edit group may span suspension."
        (register procedure "ticket -> cleanup thunk") (returns any) (prompts))
  (define (wait! register)
    (let ([task (current)])
      (unless (and task (task-escape task)) (error 'wait! "input requires a command boundary"))
      (when (text-source:current-batch (task-actor task))
        (error 'wait! "an edit group cannot span a prompt"))
      (unless (procedure? register) (error 'wait! "expected a request registration procedure"))
      (call/1cc
        (lambda (resume)
          (let ([ticket (make-ticket task resume 'waiting #f #f)])
            (task-waiting-set! task ticket)
            (set! pending (cons ticket pending))
            (let ([cleanup (register ticket)])
              (unless (procedure? cleanup) (error 'wait! "request registration must return a cleanup thunk"))
              (ticket-cleanup-set! ticket cleanup))
            ((task-escape task) '(waiting)))))))

  (edoc "Resolve a waiting continuation once. Queue the value and wake its pump; never execute the caller from a notification, renderer or accept handler. False denotes a duplicate or abandoned ticket. Call on the owning head thread."
        (ticket any "opaque ticket from wait!") (value any "request outcome") (returns boolean))
  (define (resolve! ticket value)
    (unless (ticket? ticket) (error 'resolve! "expected an input ticket"))
    (and (eq? (ticket-state ticket) 'waiting)
      (begin
        (ticket-state-set! ticket 'ready) (ticket-value-set! ticket value)
        (set! ready (cons ticket ready))
        ((task-notify (ticket-task ticket))) #t)))

  (edoc "Cancel a waiting linear prompt with false. Its request cleanup and caller run on the next pump turn."
        (ticket any "opaque ticket") (returns boolean))
  (define (cancel! ticket) (resolve! ticket #f))

  (edoc "Deliver the current batch of resolved prompt outcomes from the top-level pump. Each caller has its own command boundary; newly resolved requests wait for a later batch."
        (report procedure "condition -> report; a failing caller does not lose other ready outcomes"))
  (define (drain! report)
    (when (current) (error 'drain! "cannot resume from inside another command"))
    (let ([batch (reverse ready)])
      (set! ready '())
      (for-each
        (lambda (ticket)
          (when (eq? (ticket-state ticket) 'ready)
            (ticket-state-set! ticket 'delivered) (forget! ticket)
            (guard (ex [else (report ex)])
              (clean! ticket)
              (drive! (ticket-task ticket)
                (lambda () ((ticket-continuation ticket) (ticket-value ticket))))))) batch)))

  (edoc "Abandon this head's waiting and ready continuations on departure. Cleanup runs once, but no suspended caller or old accept target resumes."
        (actor actor "departing head"))
  (define (close! actor)
    (let ([owned (filter (lambda (ticket) (equal? actor (task-actor (ticket-task ticket)))) pending)])
      ;; Fence the whole set before cleanup can resolve another request.
      (for-each (lambda (ticket) (ticket-state-set! ticket 'closed) (forget! ticket)) owned)
      (set! ready (remp (lambda (ticket) (eq? (ticket-state ticket) 'closed)) ready))
      (for-each (lambda (ticket) (guard (ex [else (void)]) (clean! ticket))) owned))))
