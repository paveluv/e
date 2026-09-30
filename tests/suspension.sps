;; No reader thread, terminal or sleeps: callbacks only queue a continuation.
(let ([actor '(head "suspension")] [first #f] [second #f] [events '()] [wakeups 0]
      [context (make-parameter 'outside)])
  (define (record value) (set! events (append events (list value))))
  (define (wake) (set! wakeups (+ wakeups 1)))
  (define (report ex) (record 'error))
  (define (request save)
    (suspension:wait! (lambda (ticket) (save ticket) (lambda () (record 'clean)))))
  (suspension:call! actor wake
    (lambda ()
      (parameterize ([context 'inside])
        (record (list 'first (request (lambda (t) (set! first t))) (context)))
        (record (list 'second (request (lambda (t) (set! second t))) (context))))))
  (record 'pump-progress)
  (check 'suspension-returns-to-pump-without-running-caller
    (list events (context) wakeups) '((pump-progress) outside 0))
  (check 'outcomes-are-one-shot-and-deferred
    (list (suspension:resolve! first 42) (suspension:resolve! first 99) events wakeups)
    '(#t #f (pump-progress) 1))
  (suspension:drain! report)
  (suspension:cancel! second) (suspension:drain! report)
  (check 'resumption-restores-context-and-can-suspend-again
    (list events (context)) '((pump-progress clean (first 42 inside) clean (second #f inside)) outside))
  (set! events '())
  ;; Synchronous completion still waits for the pump. A failed resumed task
  ;; cannot replay, retain a ticket, or prevent another result being delivered.
  (for-each
    (lambda (fail?)
      (suspension:call! actor wake
        (lambda ()
          (suspension:wait!
            (lambda (ticket) (suspension:resolve! ticket 'cached) (lambda () (record 'clean))))
          (if fail? (error 'fixture "failed") (record 'done))))) '(#t #f))
  (suspension:drain! report)
  (check 'cached-results-and-failed-callers-share-one-pump events '(clean error clean done))
  (set! events '())
  (for-each
    (lambda (ready?)
      (suspension:call! actor wake
        (lambda () (request (lambda (t) (set! first t) (when ready? (suspension:resolve! t 'late)))) (record 'wrong)))) '(#t #f))
  (suspension:close! actor) (suspension:drain! report)
  (check 'departure-cleans-without-resuming-or-accepting-late-results
    (list events (suspension:resolve! first 'late)) '((clean clean) #f))
  (check 'suspension-requires-command-boundary-and-no-edit-transaction
    (list (test:raises? (lambda () (request void)))
      (test:raises? (lambda () (suspension:call! actor wake
                                 (lambda () (text-source:call-grouped! actor #f (lambda () (request void)))))))
      (test:raises? (lambda () (suspension:call! actor wake
                                 (lambda () (text-source:call-segmented! actor "automatic"
                                              (lambda () (text-source:call-grouped! actor #f (lambda () (request void)))))))))) '(#t #t #t)))
