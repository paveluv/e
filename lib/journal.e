;; journal.e -- the base's log writer, history, and ordered delivery.
(library (journal)
  (export add! snapshot subscribe! unsubscribe! progress)
  (import (rnrs)
          (only (chezscheme) unbox current-time time-second time-nanosecond
                make-mutex with-mutex void make-thread-parameter parameterize)
          (prefix (kernel) kernel:) (prefix (actor) actor:) (prefix (datum) datum:))

  (define-record-type state
    (nongenerative e-log-state-v2)
    (fields lock subscriptions deliveries progress
            (mutable records) (mutable count) (mutable serial)))
  (define data
    (unbox (kernel:persistent-cell 'log-store
             (lambda () (make-state (make-mutex) (kernel:make-registry)
                          (kernel:make-delivery-queue) (make-thread-parameter #f)
                          (make-vector 64 #f) 0 0)))))

  (define progress (state-progress data))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define snapshot
    (case-lambda
      [() (snapshot 0)]
      [(start)
       ;; Newest first from start through one captured count. Stored records
       ;; never mutate, so deep copying can happen after releasing the writer.
       (let-values ([(records end)
                     (with-mutex (state-lock data)
                       (let ([end (state-count data)] [v (state-records data)])
                         (unless (and (natural? start) (<= start end))
                           (error 'snapshot "start outside the log" start))
                         (let loop ([i start] [out '()])
                           (if (= i end) (values out end)
                               (loop (+ i 1) (cons (vector-ref v i) out))))))])
         (values (map datum:copy records) end))]))

  (define (subscribe! procedure)
    ;; -> revocable token. procedure receives (entry presentation), where
    ;; presentation is #f, append or progress. Registration has module lifetime.
    (unless (procedure? procedure) (error 'subscribe! "expected a procedure" procedure))
    (let ([token (with-mutex (state-lock data)
                   (let ([n (+ (state-serial data) 1)]) (state-serial-set! data n) n))])
      (kernel:registry-add! (state-subscriptions data) (cons token procedure))
      token))

  (define (unsubscribe! token)
    (kernel:registry-remove! (state-subscriptions data) (lambda (entry) (eqv? (car entry) token)))
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
      (with-mutex (state-lock data)
        (let ([v (state-records data)] [n (state-count data)])
          (when (= n (vector-length v))
            (let ([bigger (make-vector (* 2 (vector-length v)) #f)])
              (do ([i 0 (+ i 1)]) ((= i n)) (vector-set! bigger i (vector-ref v i)))
              (set! v bigger)
              (state-records-set! data v)))
          (vector-set! v n entry)
          (state-count-set! data (+ n 1))
          (for-each
            (lambda (subscriber)
              (kernel:enqueue-delivery! (state-deliveries data)
                (lambda ()
                  (let ([current (kernel:registry-find (state-subscriptions data)
                                   (lambda (candidate) (eqv? (car candidate) (car subscriber))))])
                    (when current
                      (actor:call-as actor
                        (lambda ()
                          (parameterize ([progress progress?])
                            ((cdr current) (datum:copy entry) presentation)))))))))
            (kernel:call-with-runtime-registrations
              (lambda () (reverse (kernel:registry-items (state-subscriptions data))))))))
      (kernel:drain-deliveries! (state-deliveries data))
      (datum:copy entry)))

)
