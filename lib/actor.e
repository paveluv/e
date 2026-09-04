;; actor.e -- actor identity and the interaction protocol: the
;; library (actor), v2 stage 3 (dev/DESIGN2.md).
;;
;; An actor is an identity (plain data: (head main), (agent claude 3))
;; plus a registered delivery procedure.  Any actor may pose a
;; question to another -- (actor:ask! from to question choices
;; reply!) -- delivered through the target's registration: a human's
;; head shows it in the echo area and answers at leisure; an agent's
;; delivery posts to its mailbox.  Answers route back through the
;; ticket, asynchronously; nobody's keyboard is stolen.
;;
;; Registrations ride kernel registries, so a reloaded module's stale
;; delivery procedures retract with it; the pending-ask table rides a
;; persistent cell, so open questions survive reloads.

(library (actor)
  (export register! unregister? deliver send!
          ask! answer! cancel! pending)
  (import (rnrs)
          (only (chezscheme) box unbox set-box! format void)
          (prefix (kernel) kernel:))

  ;;; Registration ----------------------------------------------------------

  (define registrations (kernel:make-registry))

  (define (register! actor deliver!)
    ;; deliver! receives protocol messages -- for an ask:
    ;; (ask ticket from question choices).  It may run on any thread;
    ;; it must only do thread-safe work (post to a mailbox, wake a
    ;; loop) and never block.
    (unless (procedure? deliver!)
      (error 'register! "expected a delivery procedure" deliver!))
    (kernel:registry-add! registrations (cons actor deliver!))
    actor)

  (define (deliver actor)
    ;; the newest registered delivery for the actor, or #f
    (cond [(kernel:registry-find
             registrations
             (lambda (entry) (equal? (car entry) actor)))
           => cdr]
          [else #f]))

  (define (unregister? actor) (and (deliver actor) #t))

  (define (send! to message)
    ;; deliver a protocol message; #t when the actor was reachable
    (cond [(deliver to)
           => (lambda (deliver!)
                (guard (ex [else #f]) (deliver! message) #t))]
          [else #f]))

  ;;; Ask and reply -----------------------------------------------------------

  ;; A pending ask: #(ticket from to question choices reply!), held
  ;; until answered or cancelled.  reply! is the asker's continuation;
  ;; it runs on the answering actor's thread.

  (define pending-asks (kernel:persistent-cell 'actors-pending
                                               (lambda () '())))
  (define ticket-counter (kernel:persistent-cell 'actors-tickets
                                                 (lambda () 0)))

  (define (ask! from to question choices reply!)
    ;; Pose a question; -> the ticket, or #f when the target actor is
    ;; unreachable.  choices is a list of strings offered to the
    ;; answerer (empty for free-form); reply! receives the answer.
    (unless (procedure? reply!)
      (error 'ask! "expected a reply procedure" reply!))
    (let ([ticket (+ (unbox ticket-counter) 1)])
      (set-box! ticket-counter ticket)
      (set-box! pending-asks
                (append (unbox pending-asks)
                        (list (vector ticket from to question choices
                                      reply!))))
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
                (unbox pending-asks)))

  (define (take-ticket! ticket)
    (let ([entry (find (lambda (entry)
                         (eqv? (vector-ref entry 0) ticket))
                       (unbox pending-asks))])
      (when entry
        (set-box! pending-asks (remq entry (unbox pending-asks))))
      entry))

  (define (answer! ticket answer)
    ;; Resolve an ask: the answer routes to the asker's reply
    ;; procedure (on this thread).  -> #t, or #f for a stale ticket.
    (cond [(take-ticket! ticket)
           => (lambda (entry)
                (guard (ex [else (void)])
                  ((vector-ref entry 5) answer))
                #t)]
          [else #f]))

  (define (cancel! ticket)
    ;; Withdraw a question nobody answered.
    (and (take-ticket! ticket) #t)))
