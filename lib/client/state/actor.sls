;; Client endpoint and directory. Attribution values stay process-local;
;; registration, delivery and open questions belong to the daemon.
(library (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send! pending answer! checkpoint checkpoint!)
  (import (only (edoc) edefine edoc) (chezscheme)
          (prefix (client) client:) (prefix (identity) identity:)
          (prefix (startup) startup:) (prefix (datum) datum:))
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
  (edefine register!
    (case-lambda
      [(actor deliver!)
       (edoc "Bind this head's delivery procedure to its claimed identity."
             (actor any "the actor identity")
             (deliver! procedure "(deliver! message)"))
       (register! actor deliver! #f)]
      [(actor deliver! capabilities)
       (edoc "Bind this head's delivery procedure and capabilities to its claimed identity."
             (actor any "the actor identity")
             (deliver! procedure "(deliver! message)")
             (capabilities any "policy description, or #f"))
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
  (edefine (attached)
    (edoc "The registered actors, from the base."
          (returns list))
    (client:request 'actors))
  (edefine (describe actor)
    (edoc "An actor's directory entry, or #f."
          (actor any "the actor identity")
          (returns (or list #f)))
    (find (lambda (entry) (equal? (car entry) actor)) (attached)))
  (edefine (registered? actor)
    (edoc "Whether an actor is registered."
          (actor any "the actor identity")
          (returns boolean))
    (and (describe actor) #t))
  (edefine (detach! actor)
    (edoc "Detach this head by closing its connection; a head detaches only itself."
          (actor any "the actor identity"))
    (unless (equal? actor (client:identity)) (error 'detach! "a head detaches itself"))
    (client:close!))
  (edefine (subscribe! procedure)
    (edoc "Watch the directory: (procedure batch) with (detached actor) and (attached actor) entries; the token unsubscribes."
          (procedure procedure "the observer")
          (returns any))
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
  (edefine (send! to message)
    (edoc "Deliver a plain message to an actor through the base."
          (to any "the recipient identity")
          (message datum "the message")
          (returns any))
    (client:request 'send to message))
  (edefine (pending actor)
    (edoc "The questions awaiting this head, oldest first."
          (actor any "the actor identity")
          (returns list))
    (unless (equal? actor (client:identity)) (error 'pending "a head reads its own questions"))
    (unless pending-known?
      (set! pending-questions (client:request 'pending))
      (set! pending-known? #t))
    (datum:copy pending-questions))
  (edefine (answer! ticket answer)
    (edoc "Answer a question by its ticket."
          (ticket any "the ticket")
          (answer any "the answer")
          (returns any))
    (forget-pending!)
    (client:request 'answer ticket answer))
  (define (own-checkpoint actor . state)
    (unless (equal? actor (client:identity)) (error 'checkpoint "a head owns its own checkpoint"))
    (apply client:request 'checkpoint state))
  (edefine (checkpoint actor)
    (edoc "This head's last screen checkpoint, or #f."
          (actor any "the actor identity")
          (returns any))
    (own-checkpoint actor))
  (edefine (checkpoint! actor state)
    (edoc "Record this head's screen checkpoint in the base."
          (actor any "the actor identity")
          (state datum "the checkpoint"))
    (own-checkpoint actor state) (void))
)
