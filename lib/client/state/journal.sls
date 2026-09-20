;; The head formats and presents records, but keeps no second log history.
(import (only (foundation edoc) elibrary))
(elibrary (state journal)
  (export add! progress retention snapshot subscribe! unsubscribe!)
  (import (chezscheme) (prefix (core client) client:) (prefix (core identity) identity:))

  (edoc "Whether a logged message supersedes its component's newest echo line rather than stacking."
        (value boolean))
  (define progress (make-thread-parameter #f))

  (edoc "Log records from the base: (values records end first), from a start index, a limit, a component and an actor."
        (args (list-of any) "start, limit, component and actor, each optional"))
  (define (snapshot . args)
    (apply values (apply client:request 'log-snapshot (if (null? args) '(0) args))))

  (edoc "How many records the base's log keeps, or set it."
        (args (list-of integer) "a new record count, at most one")
        (returns any))
  (define (retention . args)
    (apply client:request 'log-retention args))

  (edoc "Add a record to the base's log under a component; the record."
        (component symbol "the component")
        (datum datum "the record's data")
        (show (list-of any) "#f, #t or progress, at most one")
        (returns list))
  (define (add! component datum . show)
    (let ([entry (client:request 'log-add component datum
                   (and (or (null? show) (car show)) (if (progress) 'progress 'append)))])
      (client:pump!)
      entry))

  (edoc "Watch the log: (procedure entry presentation), run as the entry's actor; the token unsubscribes."
        (procedure procedure "the subscriber")
        (returns any))
  (define (subscribe! procedure)
    (client:subscribe! 'logged
      (lambda (entry presentation)
        (identity:call-as (cadr entry)
          (lambda ()
            (parameterize ([progress (eq? presentation 'progress)])
              (procedure entry presentation)))))))
  (define unsubscribe! client:unsubscribe!)
)
