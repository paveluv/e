;; actor.e -- actor identity and the interaction protocol: the
;; library (actor).
;;
;; An actor is an identity (plain data: (head "desk"), (agent claude 3))
;; plus a registered delivery procedure.  Any actor may pose a
;; question to another -- (actor:ask! from to question choices
;; reply!) -- delivered through the target's registration: a human's
;; head shows it in the echo area and answers at leisure; an agent's
;; delivery posts to its mailbox.  Answers route back through the
;; ticket, asynchronously; nobody's keyboard is stolen.
;;
;; Directory metadata and delivery share one owned registration. The
;; pending-ask table has its own lifetime: open questions survive reloads
;; and detach until explicitly answered or cancelled.

(library (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send!
          ask! answer! cancel! pending)
  (import (rnrs)
          (only (chezscheme) box unbox set-box! void make-mutex with-mutex
                current-time time-second make-thread-parameter parameterize)
          (prefix (kernel) kernel:)
          (prefix (datum) datum:))

  ;;; Registration ----------------------------------------------------------

  (define-record-type registration
    (fields identity attached-at capabilities delivery))

  (define registrations (kernel:make-registry registration-identity))

  (define (identity? actor)
    (and (list? actor) (>= (length actor) 2) (symbol? (car actor))
         (or (symbol? (cadr actor))
             (and (string? (cadr actor)) (> (string-length (cadr actor)) 0)))))

  (define (audience? audience)
    (or (eq? audience 'all) (and (list? audience) (for-all identity? audience))))

  (define (in-audience? actor audience)
    (or (eq? audience 'all) (and (member actor audience) #t)))

  ;; Attribution context, not a capability. Callbacks may run on another
  ;; actor's thread; their identity follows the work, not that thread's head.
  (define current-actor (make-thread-parameter #f))

  (define (current) (datum:copy (current-actor)))

  (define (call-as actor thunk)
    (parameterize ([current-actor (datum:copy actor)]) (thunk)))

  (define register!
    (case-lambda
      [(actor deliver!) (register! actor deliver! #f)]
      [(actor deliver! capabilities)
       ;; Identity is (kind name ...). Legacy symbol names remain valid;
       ;; named heads use strings. Capabilities describe policy, never grant it.
       (unless (identity? actor)
         (error 'register! "expected (kind name ...)" actor))
       (unless (procedure? deliver!)
         (error 'register! "expected a delivery procedure" deliver!))
       (let ([identity (datum:copy actor)] [capabilities (datum:copy capabilities)])
         (kernel:registry-add! registrations
           (make-registration identity (time-second (current-time 'time-utc)) capabilities deliver!))
         (datum:copy identity))]))

  (define (registration-of actor)
    (kernel:registry-find registrations
      (lambda (entry) (equal? (registration-identity entry) actor))))

  (define (directory-entry entry)
    (let* ([actor (registration-identity entry)] [name (cadr actor)])
      (datum:copy (list actor (car actor) (if (string? name) name (symbol->string name))
                        (registration-attached-at entry) (registration-capabilities entry)))))

  (define (attached)
    ;; Oldest registration first; like kernel reads, initializers can
    ;; inspect their staged view. Other threads only see committed actors.
    (map directory-entry (reverse (kernel:registry-items registrations))))

  (define (describe actor)
    (let ([entry (registration-of actor)]) (and entry (directory-entry entry))))

  (define (registered? actor) (and (registration-of actor) #t))

  (define (detach! actor)
    ;; Removes the captured endpoint only, never a concurrent replacement.
    ;; Already selected deliveries may finish. Tickets remain independent.
    (kernel:registry-remove! registrations
      (lambda (entry) (equal? (registration-identity entry) actor))))

  (define (subscribe! proc)
    ;; One batch of (detached actor)/(attached actor) per commit; an
    ;; atomic replacement reports both in the same batch, without a gap.
    (unless (procedure? proc) (error 'subscribe! "expected a procedure" proc))
    (kernel:registry-observe! registrations
      (lambda (removed added)
        (proc (append
                (map (lambda (entry) (list 'detached (datum:copy (registration-identity entry))))
                     (reverse removed))
                (map (lambda (entry) (list 'attached (datum:copy (registration-identity entry))))
                     (reverse added)))))))

  (define (unsubscribe! token) (kernel:registry-unobserve! token))

  (define (send! to message)
    ;; Deliver an owned plain message; #f if unreachable or delivery fails.
    ;; Invalid payloads raise before calling a reachable endpoint.
    (kernel:call-with-runtime-registrations
      (lambda ()
        (cond [(registration-of to)
               => (lambda (entry)
                    (let ([message (datum:copy message)])
                      (guard (ex [else #f])
                        (call-as (registration-identity entry)
                          (lambda () ((registration-delivery entry) message)))
                        #t)))]
              [else #f]))))

  ;;; Ask and reply -----------------------------------------------------------

  ;; A pending ask: #(ticket from to question choices reply!), held
  ;; until answered or cancelled.  reply! is the asker's continuation;
  ;; it runs on the answering actor's thread.

  (define pending-asks (kernel:persistent-cell 'actors-pending
                                               (lambda () '())))
  (define ticket-counter (kernel:persistent-cell 'actors-tickets
                                                 (lambda () 0)))
  (define protocol-lock
    ;; Keep the lock across reload alongside the existing cells, so old
    ;; reply closures and a new module instance serialize the same state.
    (unbox (kernel:persistent-cell 'actors-protocol-lock make-mutex)))

  (define (ask! from to question choices reply!)
    ;; Pose a question; -> the ticket, or #f when the target actor is
    ;; unreachable.  choices is a list of strings offered to the
    ;; answerer (empty for free-form); reply! receives the answer.
    (unless (procedure? reply!)
      (error 'ask! "expected a reply procedure" reply!))
    (let* ([from (datum:copy from)] [to (datum:copy to)]
           [question (datum:copy question)] [choices (datum:copy choices)]
           [ticket
            (with-mutex protocol-lock
              (let ([ticket (+ (unbox ticket-counter) 1)])
                (set-box! ticket-counter ticket)
                (set-box! pending-asks
                          (append (unbox pending-asks)
                                  (list (vector ticket from to question choices reply!))))
                ticket))])
      ;; Delivery may answer synchronously or ask again. Never call out
      ;; while holding the protocol lock; a failed delivery only cancels
      ;; its own ticket if it is still pending.
      (if (send! to (list 'ask ticket from question choices))
          ticket
          (begin (cancel! ticket) #f))))

  (define (pending to)
    ;; the questions awaiting an actor, oldest first:
    ;; ((ticket from question choices) ...)
    (fold-right (lambda (entry acc)
                  (if (equal? (vector-ref entry 2) to)
                      (cons (datum:copy (list (vector-ref entry 0)
                                              (vector-ref entry 1)
                                              (vector-ref entry 3)
                                              (vector-ref entry 4)))
                            acc)
                      acc))
                '()
                (with-mutex protocol-lock (unbox pending-asks))))

  (define (take-ticket! ticket)
    ;; Answer and cancellation compete for one atomic consumption. The
    ;; winner receives the callback after releasing the lock.
    (with-mutex protocol-lock
      (let ([entry (find (lambda (entry)
                           (eqv? (vector-ref entry 0) ticket))
                         (unbox pending-asks))])
        (when entry
          (set-box! pending-asks (remq entry (unbox pending-asks))))
        entry)))

  (define (answer! ticket answer)
    ;; Resolve an ask: the answer routes to the asker's reply
    ;; procedure (on this thread).  -> #t, or #f for a stale ticket.
    ;; Validate/copy before consuming the ticket: a malformed answer must
    ;; not discard a question, and the callback owns its mutable payload.
    (let ([answer (datum:copy answer)])
      (cond [(take-ticket! ticket)
             => (lambda (entry)
                  (guard (ex [else (void)])
                    (kernel:call-with-runtime-registrations
                      (lambda ()
                        (call-as (vector-ref entry 1)
                          (lambda () ((vector-ref entry 5) answer))))))
                  #t)]
        [else #f])))

  (define (cancel! ticket)
    ;; Withdraw a question nobody answered.
    (and (take-ticket! ticket) #t)))
