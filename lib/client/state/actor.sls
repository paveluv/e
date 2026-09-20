;; Client endpoint and directory. Attribution values stay process-local;
;; registration, delivery and open questions belong to the daemon.
(import (only (edoc) elibrary))
(elibrary (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send! pending answer! checkpoint checkpoint!)
  (import (chezscheme)
          (prefix (client) client:)
          (prefix (identity) identity:)
          (prefix (startup) startup:)
          (prefix (datum) datum:))
  (define current identity:current)
  (define call-as identity:call-as)
  (define identity? identity:valid?)
  (define audience? identity:audience?)
  (define in-audience? identity:in-audience?)
  (define bound? #f)
  ;; The head's open questions, read once per change: every delivered event
  ;; (a question, a pending notice, an answer) may have changed them.
  (define pending-known? #f)
  (define pending-questions '())
  (define (forget-pending!) (set! pending-known? #f))
  (edoc "Bind this head's delivery procedure, and its capabilities when given, to its claimed identity."
        (actor actor "the actor identity")
        (deliver! procedure "(deliver! message)")
        (capabilities any "policy description, or #f"))
  (define register!
    (case-lambda
      [(actor deliver!)
       (register! actor deliver! #f)]
      [(actor deliver! capabilities)
       ;; The runtime claims before importing a head. Binding its callback
       ;; adopts any negotiated default-name suffix without a second hello.
       (let ([identity (client:identity)])
         (unless (and identity (not bound?) (equal? actor (list 'head (or (startup:name) (startup:default-name)))))
           (error 'register! "a head binds its negotiated actor once" actor))
         (client:subscribe! 'event
           (lambda (message)
             (forget-pending!)
             (call-as identity (lambda () (deliver! message)))))
         (set! bound? #t)
         identity)]))
  (edoc "The registered actors, from the base."
        (returns list))
  (define (attached)
    (client:request 'actors))
  (edoc "An actor's directory entry, or #f."
        (actor actor "the actor identity")
        (returns (or list #f)))
  (define (describe actor)
    (find (lambda (entry) (equal? (car entry) actor)) (attached)))
  (edoc "Whether an actor is registered."
        (actor actor "the actor identity")
        (returns boolean))
  (define (registered? actor)
    (and (describe actor) #t))
  (edoc "Detach this head by closing its connection; a head detaches only itself."
        (actor actor "the actor identity"))
  (define (detach! actor)
    (unless (equal? actor (client:identity)) (error 'detach! "a head detaches itself"))
    (client:close!))
  (edoc "Watch the directory: (procedure batch) with (detached actor) and (attached actor) entries; the token unsubscribes."
        (procedure procedure "the observer")
        (returns any))
  (define (subscribe! procedure)
    (let ([old (map car (attached))])
      (client:subscribe! 'presence
        (lambda ()
          (let* ([next (map car (attached))]
                 [batch (append
                          (map (lambda (actor) (list 'detached actor))
                            (filter (lambda (actor) (not (member actor next))) old))
                          (map (lambda (actor) (list 'attached actor))
                            (filter (lambda (actor) (not (member actor old))) next)))])
            (set! old next)
            ;; A same-name replacement can coalesce to an empty diff. It
            ;; must still invalidate the head's cached app-size offer.
            (procedure batch))))))
  (define unsubscribe! client:unsubscribe!)
  (edoc "Deliver a plain message to an actor through the base."
        (to actor "the recipient identity")
        (message datum "the message")
        (returns any))
  (define (send! to message)
    (client:request 'send to message))
  (edoc "The questions awaiting this head, oldest first."
        (actor actor "the actor identity")
        (returns list))
  (define (pending actor)
    (unless (equal? actor (client:identity)) (error 'pending "a head reads its own questions"))
    (unless pending-known?
      (set! pending-questions (client:request 'pending))
      (set! pending-known? #t))
    (datum:copy pending-questions))
  (edoc "Answer a question by its ticket."
        (ticket any "the ticket")
        (answer any "the answer")
        (returns any))
  (define (answer! ticket answer)
    (forget-pending!)
    (client:request 'answer ticket answer))
  (define (own-checkpoint actor . state)
    (unless (equal? actor (client:identity)) (error 'checkpoint "a head owns its own checkpoint"))
    (apply client:request 'checkpoint state))
  (edoc "This head's last screen checkpoint, or #f."
        (actor actor "the actor identity")
        (returns any))
  (define (checkpoint actor)
    (own-checkpoint actor))
  (edoc "Record this head's screen checkpoint in the base."
        (actor actor "the actor identity")
        (state datum "the checkpoint"))
  (define (checkpoint! actor state)
    (own-checkpoint actor state) (void))
)
