;; journal.e -- the base's log writer, history, and ordered delivery.
(library (journal)
  (export add! snapshot subscribe! unsubscribe! progress)
  (import (rnrs)
          (only (chezscheme) current-time time-second time-nanosecond
                make-mutex with-mutex void make-thread-parameter parameterize)
          (prefix (kernel) kernel:) (prefix (actor) actor:) (prefix (datum) datum:))

  ;; This owner is pinned for the base's lifetime. Absolute append indexes
  ;; survive eviction; the ring bounds retained records, not payload bytes.
  (define lock (make-mutex))
  (define subscriptions (kernel:make-registry))
  (define deliveries (kernel:make-delivery-queue))
  (define progress (make-thread-parameter #f))
  (define records (make-vector 4096 #f))
  (define count 0)
  (define serial 0)
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define snapshot
    (case-lambda
      [() (snapshot 0 #f #f)]
      [(start) (snapshot start #f #f)]
      [(start limit) (snapshot start limit #f)]
      [(start limit component)
       ;; Newest matching records, end bookmark, and global retention floor
       ;; from one commit. Expired starts clamp to the floor. Filter and limit
       ;; before copying; immutable entries can be copied outside the writer.
       (unless (and (natural? start) (or (not limit) (natural? limit))
                    (or (not component) (symbol? component)))
         (error 'snapshot "expected a start, optional count and component" start limit component))
       (let-values ([(selected end first)
                     (with-mutex lock
                       (let ([first (max 0 (- count (vector-length records)))])
                         (unless (<= start count) (error 'snapshot "start outside the log" start))
                         (let loop ([i (- count 1)] [left limit] [out '()])
                           (if (or (< i (max start first)) (eqv? left 0))
                               (values (reverse out) count first)
                               (let ([entry (vector-ref records (mod i (vector-length records)))])
                                 (if (or (not component) (eq? component (caddr entry)))
                                     (loop (- i 1) (and left (- left 1)) (cons entry out))
                                     (loop (- i 1) left out)))))))])
         (values (map datum:copy selected) end first))]))

  (define (subscribe! procedure)
    ;; -> revocable token. procedure receives (entry presentation), where
    ;; presentation is #f, append or progress. Registration has module lifetime.
    (unless (procedure? procedure) (error 'subscribe! "expected a procedure" procedure))
    (let ([token (with-mutex lock (set! serial (+ serial 1)) serial)])
      (kernel:registry-add! subscriptions (cons token procedure))
      token))

  (define (unsubscribe! token)
    (kernel:registry-remove! subscriptions (lambda (entry) (eqv? (car entry) token)))
    (void))

  (define (add! component datum . show)
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
