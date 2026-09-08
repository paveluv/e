;; The head formats and presents records, but keeps no second log history.
(library (journal)
  (export add! snapshot subscribe! unsubscribe! progress)
  (import (chezscheme) (prefix (client) client:) (prefix (identity) identity:))
  (define progress (make-thread-parameter #f))
  (define snapshot
    (case-lambda
      [() (snapshot 0)]
      [(start) (apply values (client:request 'log-snapshot start))]))
  (define (add! component datum . show)
    (let ([entry (client:request 'log-add component datum
                   (and (or (null? show) (car show)) (if (progress) 'progress 'append)))])
      (client:pump!)
      entry))
  (define (subscribe! procedure)
    (client:subscribe! 'logged
      (lambda (entry presentation)
        (identity:call-as (cadr entry)
          (lambda ()
            (parameterize ([progress (eq? presentation 'progress)])
              (procedure entry presentation)))))))
  (define unsubscribe! client:unsubscribe!)
)
