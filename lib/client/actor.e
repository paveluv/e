;; Client endpoint and directory. Attribution values stay process-local;
;; registration, delivery and open questions belong to the daemon.
(library (actor)
  (export register! registered? detach! attached describe subscribe! unsubscribe!
          current call-as identity? audience? in-audience? send! pending answer!)
  (import (chezscheme)
          (prefix (client) client:) (prefix (identity) identity:)
          (prefix (file) file:) (prefix (startup) startup:))
  (define current identity:current)
  (define call-as identity:call-as)
  (define identity? identity:valid?)
  (define audience? identity:audience?)
  (define in-audience? identity:in-audience?)
  (define register!
    (case-lambda
      [(actor deliver!) (register! actor deliver! #f)]
      [(actor deliver! capabilities)
       (let ([identity (client:claim! actor (file:canonical (file:expand (startup:socket))))])
         (client:subscribe! 'event
           (lambda (message) (call-as identity (lambda () (deliver! message)))))
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
    (client:request 'pending))
  (define (answer! ticket answer) (client:request 'answer ticket answer))
)
