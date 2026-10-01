;; Demand only the rows a head renders. Headers are invalidated by surface
;; publication and withdrawal; generations still guard every range, and a
;; text commit that outruns its surface renders plainly until the notice.
(import (only (foundation edoc) elibrary))
(elibrary (state surface)
  (export rows snapshot subscribe! unsubscribe!)
  (import (chezscheme) (prefix (core client) client:) (prefix (core handle) handle:) (prefix (core kernel) kernel:) (prefix (foundation datum) datum:))
  (define headers (make-hashtable equal-hash equal?))
  (define (invalidate! batch)
    (if batch (for-each (lambda (entry) (hashtable-delete! headers (car entry))) batch)
        (hashtable-clear! headers)))
  (define invalidations
    (kernel:call-with-runtime-registrations
      (lambda () (client:subscribe! 'surface invalidate!))))

  (edoc "A buffer's live frame header from the base, cached: (generation text-revision cursor size), or #f."
        (id buffer "the buffer id")
        (returns (or list #f))
        (effects internal))
  (define (snapshot id)
    (unless (handle:buffer? id) (error 'snapshot "expected a buffer reference" id))
    (unless (hashtable-contains? headers id)
      (hashtable-set! headers (datum:copy id) (client:request 'surface id)))
    (datum:copy (hashtable-ref headers id #f)))

  (edoc "Row data of a frame for [from, to) from the base, or #f when withdrawn or superseded."
        (id buffer "the buffer id")
        (generation integer "the frame generation")
        (from integer "the first row")
        (to integer "the row after the last")
        (returns (or list #f))
        (effects internal))
  (define (rows id generation from to)
    (let ([result (client:request 'rows id generation from to)])
      (unless result (hashtable-delete! headers id))
      result))

  (edoc "Subscribe to frame changes of a buffer, or all with #f, as wakeups; the token unsubscribes."
        (id (or buffer #f) "the buffer, or #f for all")
        (procedure procedure "the subscriber")
        (returns any))
  (define (subscribe! id procedure)
    (unless (or (not id) (handle:buffer? id)) (error 'subscribe! "expected a buffer reference or false" id))
    (let ([id (datum:copy id)])
      (client:subscribe! 'surface
        (lambda (batch)
          ;; The actual head consumes only a wake. Rescan also wakes it;
          ;; frame contents always come from snapshot/rows, never this hint.
          (if batch
            (for-each (lambda (entry)
                        (when (or (not id) (equal? id (car entry))) (procedure (cdr entry)))) batch)
            (procedure #f))))))
  (define unsubscribe! client:unsubscribe!)
)
