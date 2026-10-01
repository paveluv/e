;; Two namespaces, two actors, one explicit shared resource, one process fixture.
(let ()
  (define alice '(head "environment-a"))
  (define bob '(head "environment-b"))
  (define (get r k) (cdr (assq k r)))
  (define (value id) (get (model:snapshot id) 'value))
  (define (generation id) (get (value id) 'generation))
  (define (done id)
    (test:await 'environment-job (lambda () (not (memq (get (value id) 'status) '(queued running)))))
    (value id))
  (define (result id) (get (done id) 'result))
  (define (run actor id source) (environment:evaluate! actor id (generation id) source))
  (define baseline (test:child-pids))
  (define shared (store:create! alice "environment-shared" '("abc")))
  (define recipe
    (list (cons 'directory (current-directory)) '(roots) '(values (seed . 11))
      '(imports (chezscheme) (prefix (service resource) resource:))
      (list 'resources (cons 'shared (list 'buffer shared)))))
  (define a (environment:for-document! alice shared recipe))
  (define b (environment:create! bob recipe 'transient))
  (dynamic-wind void
    (lambda ()
      (test:check 'environments-are-lazy (test:child-pids) baseline)
      (test:check 'document-environment-is-shared-across-heads
        (environment:for-document! bob shared recipe) a)
      (let* ([first (run alice a "(define private (+ seed 1)) private")]
             [second (run bob a "(+ private 1)")]
             [independent (run bob b "private")])
        (test:check 'environment-jobs-are-ordered-shared-explicitly-and-isolated
          (list (result first) (result second) (get (done independent) 'status))
          '((value (12) "(12)") (value (13) "(13)") error)))
      (let ([job (environment:evaluate! alice a (generation a)
                   "(set! private (+ private 1)) (values private (lambda () private))"
                   '(lambda (results) (list (car results) (procedure? (cadr results)))))])
        (test:check 'projection-runs-inside-worker-after-one-evaluation
          (list (result job) (result (run alice a "private")))
          '((value ((13 #t)) "((13 #t))") (value (13) "(13)"))))
      (let* ([j (run bob a "(resource:edit! 'shared 0 '(0 3 0 3) '(\"!\")) (resource:edit! 'shared 0 '(0 0 0 0) '(\"bad\"))")]
             [outcome (result j)])
        (test:check 'broker-uses-job-actor-and-exact-revisions
          (list (cadr outcome) (store:line shared 0)
            (let-values ([(status info) (store:undo! bob shared)]) status) (store:line shared 0))
          '(((stale)) "abc!" applied "abc")))
      (let* ([j (run alice a "(display \"out\") (display \"err\" (current-error-port)) (lambda (x) x)")]
             [outcome (result j)] [output (cadr (get (value j) 'output))]
             [v (value a)] [page (environment:completion a (generation a) (get v 'catalogue) 0 256)])
        (test:check 'job-output-and-catalogue-are-base-resources
          (list (car outcome) (list-sort char<? (string->list (store:line output 0))) (get (value j) 'status)
            (<= (length (list-ref page 3)) 256)
            (let-values ([(next states) (store:export)]) (and (assv output states) #t))
            (environment:cancel! alice j))
          '(handle (#\e #\o #\r #\r #\t #\u) ok #t #t finished))
        (environment:for-document! alice shared (append recipe '()))
        (let ([changed (map (lambda (p) (if (eq? (car p) 'values) '(values (seed . 11) (extra . 1)) p)) recipe)])
          (environment:for-document! bob shared changed))
        (test:check 'reset-fences-handles-and-catalogue
          (list (car (get (value j) 'result))
            (environment:completion a (car page) (cadr page) 0 256)
            (test:raises? (lambda () (environment:evaluate! alice a 1 "'late"))))
          '(expired #f #t))
        (test:check 'release-owns-output-only
          (list (environment:release! alice j) (model:snapshot j) (store:exists? output) (store:exists? shared))
          '(#t #f #f #t)))
      (let* ([running (run alice a "(display \"ready\") (let loop () (loop))")]
             [output (cadr (get (value running) 'output))]
             [queued (run bob a "(error 'test \"must not execute\")")])
        (test:await 'streaming-job (lambda () (string=? (store:line output 0) "ready")))
        (test:check 'queued-cancel-and-independent-work-do-not-reset-running-group
          (list (environment:cancel! bob queued) (get (value queued) 'status) (cadr (result (run bob b "seed"))))
          '(queued cancelled (11)))
        (let ([cancelled (environment:cancel! alice running)])
          (test:check 'running-cancel-fences-and-reaps
            (list cancelled (get (value running) 'status) (cadr (result (run bob a "seed"))))
            '(reset reset (11)))))
      ;; Recovery transforms saved records, never evaluating their source.
      (environment:close! bob b (generation b))
      (let ([saved (run alice a "(define transient-binding 9) (display \"retained\") '(portable)")])
        (done saved)
        (environment:stop!)
        (let ([process (sys:open-process '("scheme" "--script" "tests/environment-recovery.sps"))])
          (dynamic-wind void
            (lambda ()
              (sys:write-process! process
                (string->utf8 (format "~s" (list (call-with-values store:export list)
                                             (call-with-values model:export list) a saved shared))))
              (let* ([bytes (get-bytevector-all (sys:process-input process))]
                     [out (if (eof-object? bytes) "" (utf8->string bytes))])
                (let-values ([(status errors) (sys:process-result process)])
                  (test:check 'environment-snapshot-restores-in-a-fresh-process
                    (list status errors out) '(0 "" "1 environment-recovery checks passed\n")))))
            (lambda () (sys:close-process! process))))
        (environment:restore!)
        (test:check 'recovery-keeps-portable-outcomes-but-not-native-bindings
          (list (get (value saved) 'result) (get (value a) 'status)
            (store:line (cadr (get (value saved) 'output)) 0)
            (get (value saved) 'channels)
            (get (done (run alice a "transient-binding")) 'status))
          '((value ((portable)) "((portable))") reset "retained" ((stdout 0 0)) error)))
      (let ([failed (run alice a "(exit 0)")])
        (test:check 'worker-failure-fences-namespace-and-next-job-can-restart
          (get (done failed) 'status) 'reset)
        (test:check 'failed-worker-can-be-replaced (cadr (result (run alice a "seed"))) '(11)))
      (let* ([reset? #f]
             [watch (store:subscribe! shared
                      (lambda (event)
                        (when (and (not reset?) (eq? (car event) 'edit))
                          (set! reset? #t)
                          (environment:reset! alice a (generation a)))))]
             [job (run alice a (format "(resource:edit! 'shared ~a '(0 0 0 0) '(\"kept \")) (display \"late\")"
                                 (store:revision shared)))])
        (test:check 'resource-observer-can-reset-its-worker-without-deadlock-or-late-effects
          (list (get (done job) 'status) reset? (store:line shared 0)
            (store:line (cadr (get (value job) 'output)) 0)) '(reset #t "kept abc" ""))
        (store:unsubscribe! watch))
      (environment:close! alice a (generation a))
      (test:await 'workers-reaped (lambda () (equal? (test:child-pids) baseline)))
      (test:check 'closing-groups-keeps-borrowed-resources
        (list (model:snapshot a) (model:snapshot b) (store:exists? shared)) '(#f #f #t)))
    environment:stop!))
