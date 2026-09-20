;; actor.sls -- actor identity and the interaction protocol: the
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

(import (only (edoc) elibrary))
(elibrary (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send!
          ask! answer! cancel! cancel-owned! pending pending-tickets checkpoint checkpoint!
          head-names export import! valid-import?)
  (import (rnrs)
          (only (chezscheme) void make-mutex with-mutex
                current-time time-second parameterize)
          (prefix (kernel) kernel:)
          (prefix (activity) activity:)
          (prefix (datum) datum:)
          (prefix (identity) identity:))

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

  (edoc "Register an actor with its delivery procedure and optional capabilities; a head's registration replaces an earlier one atomically."
        (actor actor "the identity")
        (deliver! procedure "(deliver! message)")
        (capabilities any "policy description, optional"))
  (define register! (activity:wrap
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
                           (datum:copy identity))])))

  (define (registration-of actor)
    (kernel:registry-find registrations
      (lambda (entry) (equal? (registration-identity entry) actor))))

  (define (directory-entry entry)
    (let* ([actor (registration-identity entry)] [name (cadr actor)])
      (datum:copy (list actor (car actor) (if (string? name) name (symbol->string name))
                        (registration-attached-at entry) (registration-capabilities entry)))))

  (edoc "The registered actors, oldest first, as directory entries."
        (returns list))
  (define (attached)
    ;; Oldest registration first; like kernel reads, initializers can
    ;; inspect their staged view. Other threads only see committed actors.
    (map directory-entry (reverse (kernel:registry-items registrations))))

  (edoc "An actor's directory entry, or #f."
        (actor actor "the actor identity")
        (returns (or list #f)))
  (define (describe actor)
    (let ([entry (registration-of actor)]) (and entry (directory-entry entry))))

  (edoc "Whether an actor is registered."
        (actor actor "the actor identity")
        (returns boolean))
  (define (registered? actor)
    (and (registration-of actor) #t))

  (edoc "Remove an actor's registration; deliveries already selected may finish."
        (actor actor "the actor identity"))
  (define (detach! actor)
    (activity:call-with-retirement
      (lambda ()
        ;; Removes the captured endpoint only, never a concurrent replacement.
        ;; Already selected deliveries may finish. Tickets remain independent.
        (kernel:registry-remove! registrations
                                 (lambda (entry) (equal? (registration-identity entry) actor))))))

  (edoc "Watch the directory: (proc batch) with (detached actor) and (attached actor) entries per commit; the token unsubscribes."
        (proc procedure "the observer")
        (returns any))
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

  (edoc "Stop watching the directory, by token."
        (token any "the token"))
  (define (unsubscribe! token)
    (kernel:registry-unobserve! token))

  (edoc "Deliver a plain message to an actor; #f when unreachable or delivery fails."
        (to actor "the recipient identity")
        (message datum "the message")
        (returns boolean))
  (define (send! to message)
    (activity:call-with
      (lambda ()
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
                  [else #f]))))))

  ;;; Ask and reply -----------------------------------------------------------

  ;; A pending ask: #(ticket from to question choices reply! owner), held
  ;; until answered or cancelled.  reply! is the asker's continuation;
  ;; it runs on the answering actor's thread. The optional opaque owner is
  ;; a session key, not the recipient identity and never wire data.
  ;; Actor is a pinned process root; all protocol state has its lifetime.
  (define pending-asks '())
  (define ticket-counter 0)
  (define protocol-lock (make-mutex))

  (edoc "A copy of a head's last screen checkpoint, or #f."
        (actor actor "the actor identity")
        (returns any))
  (define (checkpoint actor)
    (let ([entry (known-head actor)])
      (and entry (datum:copy (with-mutex protocol-lock (head-state-checkpoint entry))))))

  (edoc "The names of the known heads."
        (returns (list-of string)))
  (define (head-names)
    ;; Status needs identities, not copies of opaque screen/kill contents.
    (map (lambda (entry) (string-copy (cadr (head-state-identity entry))))
      (kernel:registry-items known-heads)))

  (edoc "The known heads' checkpoints for the session file, (name checkpoint) each."
        (returns list))
  (define (export)
    ;; The lifecycle barrier stabilizes the directory before this read.
    ;; Checkpoint bodies stay opaque: missing targets and old views are valid.
    (let ([heads (kernel:registry-items known-heads)])
      (with-mutex protocol-lock
        (map (lambda (entry) (datum:copy (list (cadr (head-state-identity entry)) (head-state-checkpoint entry)))) heads))))

  (edoc "Whether a value is a saved checkpoint directory."
        (entries any "the value")
        (returns boolean))
  (define (valid-import? entries)
    (and (list? entries)
         (let ([names (make-hashtable string-hash string=?)])
           (for-all
             (lambda (entry)
               (and (list? entry) (= (length entry) 2)
                    (string? (car entry)) (> (string-length (car entry)) 0)
                    (not (hashtable-contains? names (car entry)))
                    (guard (ex [(datum:invalid? ex) #f] [else (raise ex)]) (datum:copy (cadr entry)) #t)
                    (begin (hashtable-set! names (car entry) #t) #t))) entries))))

  (edoc "Restore the known heads' checkpoints from the session file."
        (entries list "the saved directory"))
  (define (import! entries)
    (unless (valid-import? entries) (error 'import! "invalid checkpoint directory"))
    (let ([entries (datum:copy entries)])
      (activity:call-with
        (lambda ()
          (parameterize ([kernel:registering-module #f])
            (kernel:call-with-registration-update
              (lambda ()
                (unless (null? (kernel:registry-items known-heads))
                  (error 'import! "restore requires an empty checkpoint directory"))
                (for-each (lambda (entry)
                            (kernel:registry-add! known-heads (make-head-state (list 'head (car entry)) (cadr entry))))
                  entries))))))))

  (edoc "The tickets of every unanswered question."
        (returns list))
  (define (pending-tickets)
    (with-mutex protocol-lock (map (lambda (entry) (vector-ref entry 0)) pending-asks)))

  (edoc "Record a head's screen checkpoint; a kept kill slot keeps the previous kill text."
        (actor actor "the actor identity")
        (state datum "the checkpoint"))
  (define (checkpoint! actor state)
    (activity:call-with
      (lambda ()
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
                                          state)))))))

  (edoc "Ask an actor a question through the interaction protocol; the reply procedure receives the answer, and an owner may withdraw it. The ticket."
        (from actor "the asker")
        (to actor "the asked actor")
        (question string "the question")
        (choices (list-of string) "the offered answers")
        (reply! procedure "(reply! answer)")
        (owner any "the session owning it, optional")
        (returns any))
  (define ask! (activity:wrap
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
                            (begin (cancel! ticket) #f)))))])))

  (edoc "The questions awaiting an actor, oldest first: (ticket from question choices) each."
        (to actor "the asked actor")
        (returns list))
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

  (edoc "Answer a question by its ticket; #t, or #f for a stale ticket."
        (ticket any "the ticket")
        (answer any "the answer")
        (to (or actor #f) "who answers, optional")
        (returns boolean))
  (define answer! (activity:wrap
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
                           [else #f]))])))

  (edoc "Withdraw a question by its ticket; a session may withdraw only its own."
        (ticket any "the ticket")
        (owner any "the session, optional"))
  (define cancel! (activity:wrap
                    ;; A session may withdraw only its own question; the trusted one-argument
                    ;; form keeps the in-process ticket API. Checks and consumption are atomic.
                    (case-lambda
                      [(ticket) (cancel! ticket #f)]
                      [(ticket owner)
                       (and (take-ticket! ticket (lambda (entry) (or (not owner) (eq? (vector-ref entry 6) owner)))) #t)])))

  (edoc "Withdraw every question a session owner asked."
        (owner any "the owner"))
  (define (cancel-owned! owner)
    (activity:call-with-retirement
      (lambda ()
        (unless owner (error 'cancel-owned! "expected a question owner"))
        (notify-pending!
          (with-mutex protocol-lock
            (let-values ([(removed kept) (partition (lambda (entry) (eq? (vector-ref entry 6) owner)) pending-asks)])
              (set! pending-asks kept)
              removed)))))))
