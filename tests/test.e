;; Shared assertions and bounded concurrency fixtures. Keep scenarios in
;; their suites; this library only handles reporting and worker coordination.
(library (test)
  (export check finish! raises? await gate worker parallel recorder)
  (import (rnrs)
          (only (chezscheme) format fork-thread make-mutex with-mutex
                sleep make-time iota))

  (define checks 0)

  (define (check label actual expected)
    (set! checks (+ checks 1))
    (unless (equal? actual expected)
      (error 'test (format "~s" label) actual expected)))

  (define (finish! suite)
    (format #t "~a ~a checks passed\n" checks suite))

  (define raises?
    (case-lambda
      [(thunk) (raises? thunk (lambda (ex) #t))]
      [(thunk predicate)
       (guard (ex [(predicate ex) #t] [else (raise ex)]) (thunk) #f)]))

  (define (recorder)
    (let ([lock (make-mutex)] [items '()])
      (case-lambda
        [() (with-mutex lock (reverse items))]
        [(item) (with-mutex lock (set! items (cons item items)))])))

  (define (await label ready?)
    (let wait ([tries 1000])
      (unless (ready?)
        (when (zero? tries) (error 'test "worker timeout" label))
        (sleep (make-time 'time-duration 5000000 0))
        (wait (- tries 1)))))

  (define (gate)
    (let ([lock (make-mutex)] [value #f])
      (case-lambda
        [() (with-mutex lock value)]
        [(next) (with-mutex lock (set! value next))])))

  (define (worker thunk)
    (let ([result (gate)])
      (fork-thread
        (lambda ()
          (guard (ex [else (result (cons #f ex))])
            (result (cons #t (thunk))))))
      (lambda ()
        (await 'worker result)
        (let ([outcome (result)])
          (if (car outcome) (cdr outcome) (raise (cdr outcome)))))))

  (define (parallel count task)
    (let* ([start (gate)]
           [workers
            (map (lambda (index)
                   (worker (lambda () (await 'start start) (task index))))
                 (iota count))])
      (start #t)
      (map (lambda (finish) (finish)) workers))))
