;; The head formats and presents records, but keeps no second log history.
(library (journal)
  (export add! snapshot retention subscribe! unsubscribe! progress)
  (import (only (edoc) edefine edoc) (chezscheme) (prefix (client) client:) (prefix (identity) identity:))
  (edefine progress
    (edoc "Whether a logged message supersedes its component's newest echo line rather than stacking."
          (value boolean))
    (make-thread-parameter #f))
  (edefine (snapshot . args)
    (edoc "Log records from the base: (values records end first), from a start index, a limit, a component and an actor."
          (args (list-of any) "start, limit, component and actor, each optional"))
    (apply values (apply client:request 'log-snapshot (if (null? args) '(0) args))))
  (edefine (retention . args)
    (edoc "How many records the base's log keeps, or set it."
          (args (list-of integer) "a new record count, at most one")
          (returns any))
    (apply client:request 'log-retention args))
  (edefine (add! component datum . show)
    (edoc "Add a record to the base's log under a component; the record."
          (component symbol "the component")
          (datum datum "the record's data")
          (show (list-of any) "#f, #t or progress, at most one")
          (returns list))
    (let ([entry (client:request 'log-add component datum
                   (and (or (null? show) (car show)) (if (progress) 'progress 'append)))])
      (client:pump!)
      entry))
  (edefine (subscribe! procedure)
    (edoc "Watch the log: (procedure entry presentation), run as the entry's actor; the token unsubscribes."
          (procedure procedure "the subscriber")
          (returns any))
    (client:subscribe! 'logged
      (lambda (entry presentation)
        (identity:call-as (cadr entry)
          (lambda ()
            (parameterize ([progress (eq? presentation 'progress)])
              (procedure entry presentation)))))))
  (define unsubscribe! client:unsubscribe!)
)
