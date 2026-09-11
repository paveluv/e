;; Client endpoint and directory. Attribution values stay process-local;
;; registration, delivery and open questions belong to the daemon.
(library (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send! pending answer! checkpoint checkpoint!)
  (import (chezscheme)
          (prefix (client) client:) (prefix (identity) identity:)
          (prefix (path) path:) (prefix (startup) startup:) (prefix (datum) datum:))
  (define current identity:current)
  (define call-as identity:call-as)
  (define identity? identity:valid?)
  (define audience? identity:audience?)
  (define in-audience? identity:in-audience?)
  ;; The head's open questions, read once per change: every delivered event
  ;; (a question, a pending notice, an answer) may have changed them.
  (define pending-known? #f)
  (define pending-questions '())
  (define (forget-pending!) (set! pending-known? #f))
  (define register!
    (case-lambda
      [(actor deliver!) (register! actor deliver! #f)]
      [(actor deliver! capabilities)
       (let ([identity (client:claim! actor (path:canonical (path:expand (startup:socket))))])
         (client:subscribe! 'event
           (lambda (message)
             (forget-pending!)
             (call-as identity (lambda () (deliver! message)))))
         identity)]))
  (define (attached) (client:request 'actors))
  (define (describe actor) (find (lambda (entry) (equal? (car entry) actor)) (attached)))
  (define (registered? actor) (and (describe actor) #t))
  (define (detach! actor)
    (unless (equal? actor (client:identity)) (error 'detach! "a head detaches itself"))
    (client:close!))
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
  (define (send! to message) (client:request 'send to message))
  (define (pending actor)
    (unless (equal? actor (client:identity)) (error 'pending "a head reads its own questions"))
    (unless pending-known?
      (set! pending-questions (client:request 'pending))
      (set! pending-known? #t))
    (datum:copy pending-questions))
  (define (answer! ticket answer)
    (forget-pending!)
    (client:request 'answer ticket answer))
  (define (own-checkpoint actor . state)
    (unless (equal? actor (client:identity)) (error 'checkpoint "a head owns its own checkpoint"))
    (apply client:request 'checkpoint state))
  (define (checkpoint actor) (own-checkpoint actor))
  (define (checkpoint! actor state) (own-checkpoint actor state) (void))
)
