;; log.e -- the base's structured log. Records are owned plain snapshots:
;; (utc-nanoseconds actor component datum), indexed in append order. Views
;; and echo presentation subscribe; neither owns a second history.
(library (log)
  (export add! record length snapshot entries history
          (rename (car time) (cadr actor) (caddr component) (cadddr datum))
          register-formatter! styler format-entry subscribe! unsubscribe! progress)
  (import (except (rnrs) length)
          (only (chezscheme) unbox format current-time time-second time-nanosecond
                make-mutex with-mutex void make-thread-parameter parameterize print-graph)
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
  (define (length) (with-mutex (state-lock data) (state-count data)))

  (define (record i)
    (datum:copy
      (with-mutex (state-lock data)
        (unless (and (natural? i) (< i (state-count data)))
          (error 'record "index outside the log" i))
        (vector-ref (state-records data) i))))

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

  (define (own-datum value)
    ;; Keep structured data queryable. An arbitrary runtime/cyclic result is
    ;; its written representation at admission, never a retained live object.
    (guard (ex [else (parameterize ([print-graph #t]) (format "~s" value))])
      (datum:copy value)))

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
           [payload (own-datum datum)] [now (current-time 'time-utc)]
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

  ;;; Component presentation ------------------------------------------------

  (define formatters (kernel:make-registry))
  (define (register-formatter! component fmt . style)
    (kernel:registry-add! formatters (list component fmt (and (pair? style) (car style)))))
  (define (formatter component)
    (kernel:registry-find formatters (lambda (x) (eq? (car x) component))))
  (define (styler component)
    (let ([f (formatter component)]) (and f (caddr f))))
  (define (format-entry entry)
    (let ([f (formatter (caddr entry))] [d (cadddr entry)])
      (guard (ex [else (format "~s" d)])
        (if f ((cadr f) d) (if (string? d) d (format "~s" d))))))

  ;;; Queries ---------------------------------------------------------------

  (define (entries . component)
    (let-values ([(records end) (snapshot)])
      (if (pair? component)
          (filter (lambda (e) (eq? (caddr e) (car component))) records)
          records)))

  (define (history component . select)
    ;; Select strings from owned data, newest first, collapsing consecutive
    ;; repeats. Eval selects car from (query . result), file prompts cdr.
    (let ([sel (if (pair? select) (car select) (lambda (d) d))])
      (let loop ([es (entries component)] [last #f])
        (if (null? es) '()
            (let ([x (guard (ex [else #f]) (sel (cadddr (car es))))])
              (if (and (string? x) (not (equal? x last)))
                  (cons x (loop (cdr es) x))
                  (loop (cdr es) last))))))))
