;; journal.sls -- the base's log writer, history, and ordered delivery.
(library (journal)
  (export add! snapshot retention subscribe! unsubscribe! progress)
  (import (only (edoc) edefine edoc) (rnrs)
          (only (chezscheme) current-time time-second time-nanosecond
                make-mutex with-mutex void make-thread-parameter parameterize)
          (prefix (kernel) kernel:) (prefix (actor) actor:) (prefix (datum) datum:))

  ;; This owner is pinned for the base's lifetime. Absolute append indexes
  ;; survive eviction; the ring bounds retained records, not payload bytes.
  (define lock (make-mutex))
  (define subscriptions (kernel:make-registry))
  (define deliveries (kernel:make-delivery-queue))
  (edefine progress
    (edoc "Whether a logged message supersedes its component's newest echo line rather than stacking."
          (value boolean))
    (make-thread-parameter #f))
  (define records (make-vector 1000000 #f))
  (define count 0)
  ;; Growing the ring must not move this floor back over evicted entries.
  (define first 0)
  (define serial 0)
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (edefine retention
    (case-lambda
      [()
       (edoc "How many records the log keeps."
             (returns integer))
       (with-mutex lock (vector-length records))]
      [(size)
       (edoc "Set how many records the log keeps, dropping the oldest."
             (size integer "the record count"))
       (unless (and (fixnum? size) (> size 0))
         (error 'retention "expected a positive exact record count" size))
       (with-mutex lock
         (unless (= size (vector-length records))
           (let ([next (make-vector size #f)] [from (max first (- count size))])
             (do ([i from (+ i 1)]) ((= i count))
               (vector-set! next (mod i size) (vector-ref records (mod i (vector-length records)))))
             (set! records next)
             (set! first from)))
         (vector-length records))]))
  (edefine snapshot
    (case-lambda
      [()
       (edoc "Every log record: (values records end first).")
       (snapshot 0 #f #f)]
      [(start)
       (edoc "The log records from an index on: (values records end first)."
             (start integer "the first index"))
       (snapshot start #f #f)]
      [(start limit)
       (edoc "At most limit log records from an index on: (values records end first)."
             (start integer "the first index")
             (limit (or integer #f) "how many at most"))
       (snapshot start limit #f)]
      [(start limit component)
       (edoc "A component's log records from an index on, at most limit: (values records end first)."
             (start integer "the first index")
             (limit (or integer #f) "how many at most")
             (component (or symbol #f) "the component, or #f for all"))
       (snapshot start limit component #f)]
      [(start limit component actor)
       (edoc "An actor's records of a component from an index on, at most limit: (values records end first)."
             (start integer "the first index")
             (limit (or integer #f) "how many at most")
             (component (or symbol #f) "the component, or #f for all")
             (actor any "the actor, or #f for all"))
       ;; Newest matching records, end bookmark, and global retention floor
       ;; from one commit. Expired starts clamp to the floor. Filter and limit
       ;; before copying; immutable entries can be copied outside the writer.
       (unless (and (natural? start) (or (not limit) (natural? limit))
                    (or (not component) (symbol? component))
                    (or (not actor) (actor:identity? actor)))
         (error 'snapshot "expected a start, optional count, component and actor" start limit component actor))
       (let-values ([(selected end first)
                     (with-mutex lock
                       (unless (<= start count) (error 'snapshot "start outside the log" start))
                       (let ([from (max start first)])
                         (let loop ([i (- count 1)] [left limit] [out '()])
                           (if (or (< i from) (eqv? left 0))
                               (values (reverse out) count first)
                               (let ([entry (vector-ref records (mod i (vector-length records)))])
                                 (if (and (or (not component) (eq? component (caddr entry)))
                                          (or (not actor) (equal? actor (cadr entry))))
                                     (loop (- i 1) (and left (- left 1)) (cons entry out))
                                     (loop (- i 1) left out)))))))])
         (values (map datum:copy selected) end first))]))

  (edefine (subscribe! procedure)
    (edoc "Watch the log: (procedure entry presentation), presentation #f, append or progress; the token unsubscribes."
          (procedure procedure "the subscriber")
          (returns integer))
    ;; -> revocable token. procedure receives (entry presentation), where
    ;; presentation is #f, append or progress. Registration has module lifetime.
    (unless (procedure? procedure) (error 'subscribe! "expected a procedure" procedure))
    (let ([token (with-mutex lock (set! serial (+ serial 1)) serial)])
      (kernel:registry-add! subscriptions (cons token procedure))
      token))

  (edefine (unsubscribe! token)
    (edoc "Stop watching the log, by token."
          (token integer "the token"))
    (kernel:registry-remove! subscriptions (lambda (entry) (eqv? (car entry) token)))
    (void))

  (edefine (add! component datum . show)
    (edoc "Add a record under a component, attributed to the current actor; show says how heads present it. The record."
          (component symbol "the component")
          (datum datum "the record's data")
          (show (list-of any) "#f, #t or progress, at most one")
          (returns list))
    ;; Attribution follows the work; registration is not required to log it.
    (unless (symbol? component) (error 'add! "expected a component symbol" component))
    (let* ([actor (or (actor:current) '(base e))]
           [payload (datum:copy datum)] [now (current-time 'time-utc)]
           [entry (list (+ (* (time-second now) 1000000000) (time-nanosecond now))
                        actor component payload)]
           [progress? (and (progress) #t)]
           [presentation (and (or (null? show) (car show)) (if progress? 'progress 'append))])
      (with-mutex lock
        (vector-set! records (mod count (vector-length records)) entry)
        (set! count (+ count 1))
        (set! first (max first (- count (vector-length records))))
        (for-each
          (lambda (subscriber)
            (kernel:enqueue-delivery! deliveries
              (lambda ()
                (let ([current (kernel:registry-find subscriptions
                                 (lambda (candidate) (eqv? (car candidate) (car subscriber))))])
                  (when current
                    (actor:call-as actor
                      (lambda ()
                        (parameterize ([progress progress?])
                          ((cdr current) (datum:copy entry) presentation)))))))))
          (kernel:call-with-runtime-registrations
            (lambda () (reverse (kernel:registry-items subscriptions))))))
      (kernel:drain-deliveries! deliveries)
      (datum:copy entry)))

)
