;; actor.e -- actor identity and the interaction protocol: the
;; library (actor).
;;
;; An actor is an identity (plain data: (head main), (agent claude 3))
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
          deliver send!
          ask! answer! cancel! pending)
  (import (rnrs)
          (only (chezscheme) box unbox set-box! void make-mutex with-mutex
                current-time time-second)
          (prefix (kernel) kernel:))

  ;;; Registration ----------------------------------------------------------

  (define-record-type registration
    (fields identity attached-at capabilities delivery))

  (define registrations (kernel:make-registry registration-identity))

  (define (copy-data datum)
    ;; Own admitted names/metadata, and never expose their mutable parts
    ;; through directory snapshots, return values, or presence messages.
    (let copy ([datum datum] [path '()])
      (when (memq datum path) (error 'actor "cyclic directory data"))
      (cond
        [(pair? datum)
         (let ([path (cons datum path)])
           (cons (copy (car datum) path) (copy (cdr datum) path)))]
        [(vector? datum)
         (list->vector (map (lambda (item) (copy item (cons datum path))) (vector->list datum)))]
        [(string? datum) (string-copy datum)]
        [(bytevector? datum) (bytevector-copy datum)]
        [(or (null? datum) (symbol? datum) (number? datum) (boolean? datum) (char? datum)) datum]
        [else (error 'actor "expected plain directory data" datum)])))

  (define register!
    (case-lambda
      [(actor deliver!) (register! actor deliver! #f)]
      [(actor deliver! capabilities)
       ;; Identity is (kind name ...). Legacy symbol names remain valid;
       ;; named heads use strings. Capabilities describe policy, never grant it.
       (unless (and (list? actor) (>= (length actor) 2) (symbol? (car actor))
                    (or (symbol? (cadr actor))
                        (and (string? (cadr actor)) (> (string-length (cadr actor)) 0))))
         (error 'register! "expected (kind name ...)" actor))
       (unless (procedure? deliver!)
         (error 'register! "expected a delivery procedure" deliver!))
       (let ([identity (copy-data actor)] [capabilities (copy-data capabilities)])
         (kernel:registry-add! registrations
           (make-registration identity (time-second (current-time 'time-utc)) capabilities deliver!))
         (copy-data identity))]))

  (define (registration-of actor)
    (kernel:registry-find registrations
      (lambda (entry) (equal? (registration-identity entry) actor))))

  (define (directory-entry entry)
    (let* ([actor (registration-identity entry)] [name (cadr actor)])
      (copy-data (list actor (car actor) (if (string? name) name (symbol->string name))
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
                (map (lambda (entry) (list 'detached (copy-data (registration-identity entry))))
                     (reverse removed))
                (map (lambda (entry) (list 'attached (copy-data (registration-identity entry))))
                     (reverse added)))))))

  (define (unsubscribe! token) (kernel:registry-unobserve! token))

  (define (deliver actor)
    ;; Delivery procedures may run on any thread: post/wake, never block.
    (let ([entry (registration-of actor)]) (and entry (registration-delivery entry))))

  (define (send! to message)
    ;; deliver a protocol message; #t when the actor was reachable
    (kernel:call-with-runtime-registrations
      (lambda ()
        (cond [(deliver to)
               => (lambda (deliver!)
                    (guard (ex [else #f]) (deliver! message) #t))]
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
    (let ([ticket
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
                      (cons (list (vector-ref entry 0)
                                  (vector-ref entry 1)
                                  (vector-ref entry 3)
                                  (vector-ref entry 4))
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
    (cond [(take-ticket! ticket)
           => (lambda (entry)
                (guard (ex [else (void)])
                  (kernel:call-with-runtime-registrations
                    (lambda () ((vector-ref entry 5) answer))))
                #t)]
          [else #f]))

  (define (cancel! ticket)
    ;; Withdraw a question nobody answered.
    (and (take-ticket! ticket) #t)))
