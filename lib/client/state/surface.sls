;; Demand only the rows a head renders. Headers are invalidated by surface
;; publication and withdrawal; generations still guard every range, and a
;; text commit that outruns its surface renders plainly until the notice.
(library (surface)
  (export snapshot rows subscribe! unsubscribe!)
  (import (chezscheme) (prefix (client) client:) (prefix (kernel) kernel:) (prefix (datum) datum:))
  (define headers (make-eqv-hashtable))
  (define (invalidate! batch)
    (if batch (for-each (lambda (entry) (hashtable-delete! headers (car entry))) batch)
        (hashtable-clear! headers)))
  (define invalidations
    (kernel:call-with-runtime-registrations
      (lambda () (client:subscribe! 'surface invalidate!))))
  (define (snapshot id)
    (unless (hashtable-contains? headers id)
      (hashtable-set! headers id (client:request 'surface id)))
    (datum:copy (hashtable-ref headers id #f)))
  (define (rows id generation from to)
    (let ([result (client:request 'rows id generation from to)])
      (unless result (hashtable-delete! headers id))
      result))
  (define (subscribe! id procedure)
    (client:subscribe! 'surface
      (lambda (batch)
        ;; The actual head consumes only a wake. Rescan also wakes it;
        ;; frame contents always come from snapshot/rows, never this hint.
        (if batch
            (for-each (lambda (entry)
                        (when (or (not id) (eqv? id (car entry))) (procedure (cdr entry)))) batch)
            (procedure #f)))))
  (define unsubscribe! client:unsubscribe!)
)
