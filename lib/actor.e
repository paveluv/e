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
;; pending-ask table has its own lifetime: known heads can receive questions
;; while detached. Session-owned questions end with their asking session.

(library (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send!
          ask! answer! cancel! cancel-owned! pending checkpoint checkpoint!)
  (import (rnrs)
          (only (chezscheme) void make-mutex with-mutex
                current-time time-second parameterize)
          (prefix (kernel) kernel:)
          (prefix (datum) datum:) (prefix (identity) identity:))

  ;;; Registration ----------------------------------------------------------

  (define-record-type registration
    (fields identity attached-at capabilities delivery))

  (define registrations (kernel:make-registry registration-identity))

  ;; A named head's identity and screen outlive its endpoint. Admit both
  ;; registries in one update: failed/staged claims never create known heads.
  (define-record-type head-state
    (fields identity (mutable checkpoint)))
  (define known-heads (kernel:make-registry head-state-identity))

  (define (known-head actor)
    (kernel:registry-find known-heads
      (lambda (entry) (equal? (head-state-identity entry) actor))))

  (define identity? identity:valid?)
  (define audience? identity:audience?)
  (define in-audience? identity:in-audience?)
  (define current identity:current)
  (define call-as identity:call-as)

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
         (kernel:call-with-registration-update
           (lambda ()
             (kernel:registry-add! registrations
               (make-registration identity (time-second (current-time 'time-utc)) capabilities deliver!))
             (when (and (eq? (car identity) 'head) (= (length identity) 2)
                        (string? (cadr identity)) (not (known-head identity)))
               (parameterize ([kernel:registering-module #f])
                 (kernel:registry-add! known-heads (make-head-state identity #f))))))
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

  ;; A pending ask: #(ticket from to question choices reply! owner), held
  ;; until answered or cancelled.  reply! is the asker's continuation;
  ;; it runs on the answering actor's thread. The optional opaque owner is
  ;; a session key, not the recipient identity and never wire data.
  ;; Actor is a pinned process root; all protocol state has its lifetime.
  (define pending-asks '())
  (define ticket-counter 0)
  (define protocol-lock (make-mutex))

  (define (checkpoint actor)
    (let ([entry (known-head actor)])
      (and entry (datum:copy (with-mutex protocol-lock (head-state-checkpoint entry))))))

  (define (checkpoint! actor state)
    ;; A screen checkpoint whose kill slot is the symbol kept keeps the
    ;; kill text of the retained checkpoint: heads send that text only
    ;; when it changes.
    (let ([entry (known-head actor)] [state (datum:copy state)])
      (unless (and entry (registered? actor))
        (error 'checkpoint! "expected an attached named head" actor))
      (with-mutex protocol-lock
        (head-state-checkpoint-set! entry
          (if (and (list? state) (>= (length state) 3) (eq? (caddr state) 'kept))
              (let ([previous (head-state-checkpoint entry)])
                (cons* (car state) (cadr state)
                       (if (and (list? previous) (>= (length previous) 3) (string? (caddr previous)))
                           (caddr previous) "")
                       (cdddr state)))
              state)))))

  (define ask!
    (case-lambda
      [(from to question choices reply!) (ask! from to question choices reply! #f)]
      [(from to question choices reply! owner)
       ;; Unknown/unspecified targets refuse; a known named head retains
       ;; the ticket even when delivery fails. Delivery is only its wakeup.
       (let ([from (datum:copy from)] [to (datum:copy to)]
             [question (datum:copy question)] [choices (datum:copy choices)])
         (unless (and (identity? from) (or (not to) (identity? to))
                      (string? question) (list? choices) (for-all string? choices)
                      (procedure? reply!))
           (error 'ask! "expected identities, question text, string choices and reply procedure"))
         (and to
           (let ([ticket
                  (with-mutex protocol-lock
                    (set! ticket-counter (+ ticket-counter 1))
                    (set! pending-asks
                      (append pending-asks (list (vector ticket-counter from to question choices reply! owner))))
                    ticket-counter)])
             ;; Delivery may answer synchronously or ask again. Never call out
             ;; while holding the lock. Observe committed head identities only.
             (if (or (send! to (list 'ask ticket from question choices))
                   (kernel:call-with-runtime-registrations (lambda () (known-head to))))
               ticket
               (begin (cancel! ticket) #f)))))]))

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
                (with-mutex protocol-lock pending-asks)))

  (define (notify-pending! entries)
    ;; One invalidation per recipient after an atomic removal. The pending
    ;; table is authoritative even if another mutation notifies first.
    (for-each (lambda (to) (send! to '(pending)))
      (fold-left (lambda (recipients entry)
                   (let ([to (vector-ref entry 2)])
                     (if (member to recipients) recipients (cons to recipients))))
        '() entries)))

  (define (take-ticket! ticket accepts?)
    ;; Answer and cancellation compete for one atomic consumption. The
    ;; winner receives the callback after releasing the lock.
    (let ([entry
           (with-mutex protocol-lock
             (let ([entry (find (lambda (entry)
                                  (and (eqv? (vector-ref entry 0) ticket) (accepts? entry)))
                                pending-asks)])
               (when entry
                 (set! pending-asks (remq entry pending-asks)))
               entry))])
      (when entry (notify-pending! (list entry)))
      entry))

  (define answer!
    ;; Resolve an ask: the answer routes to the asker's reply
    ;; procedure (on this thread).  -> #t, or #f for a stale ticket.
    ;; Validate/copy before consuming the ticket: a malformed answer must
    ;; not discard a question, and the callback owns its mutable payload.
    (case-lambda
      [(ticket answer) (answer! ticket answer #f)]
      [(ticket answer to)
       (let ([answer (datum:copy answer)] [to (datum:copy to)])
         (cond [(take-ticket! ticket (lambda (entry) (or (not to) (equal? (vector-ref entry 2) to))))
                => (lambda (entry)
                     (guard (ex [else (void)])
                       (kernel:call-with-runtime-registrations
                         (lambda ()
                           (call-as (vector-ref entry 1)
                             (lambda () ((vector-ref entry 5) answer))))))
                     #t)]
           [else #f]))]))

  (define cancel!
    ;; A session may withdraw only its own question; the trusted one-argument
    ;; form keeps the in-process ticket API. Checks and consumption are atomic.
    (case-lambda
      [(ticket) (cancel! ticket #f)]
      [(ticket owner)
       (and (take-ticket! ticket (lambda (entry) (or (not owner) (eq? (vector-ref entry 6) owner)))) #t)]))

  (define (cancel-owned! owner)
    (unless owner (error 'cancel-owned! "expected a question owner"))
    (notify-pending!
      (with-mutex protocol-lock
        (let-values ([(removed kept) (partition (lambda (entry) (eq? (vector-ref entry 6) owner)) pending-asks)])
          (set! pending-asks kept)
          removed)))))
