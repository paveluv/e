;; Shared assertions and bounded concurrency fixtures. Keep scenarios in
;; their suites; this library only handles reporting and resource observation.
(library (test)
  (export check finish! raises? await gate worker parallel recorder child-pids fd-count)
  (import (rnrs)
          (only (chezscheme) format fork-thread make-mutex with-mutex
                sleep make-time iota file-directory? directory-list))

  (define checks 0)

  (define (check label actual expected)
    (set! checks (+ checks 1))
    (unless (equal? actual expected)
      (error 'test (format "~s" label) actual expected)))

  (define (finish! suite)
    (format #t "~a ~a checks passed\n" checks suite))

  (define (fd-count)
    (let ([directory (cond [(file-directory? "/proc/self/fd") "/proc/self/fd"]
                           [(file-directory? "/dev/fd") "/dev/fd"] [else #f])])
      (and directory (length (directory-list directory)))))

  (define (child-pids)
    ;; Observe in-process: tool invocations may have different PID namespaces.
    ;; A task can disappear between listing and reading its children file.
    (and (file-directory? "/proc/self/task")
         (list-sort <
           (apply append
             (map (lambda (task)
                    (guard (ex [(i/o-file-does-not-exist-error? ex) '()])
                      (call-with-input-file (string-append "/proc/self/task/" task "/children")
                        (lambda (port)
                          (let loop ([out '()])
                            (let ([pid (read port)])
                              (if (eof-object? pid) out (loop (cons pid out)))))))))
                  (directory-list "/proc/self/task"))))))

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
