#!/usr/bin/env scheme-script

;; The shared log: owned records, actor context, coherent snapshots and
;; ordered delivery. Reuse the same records for formatting/history checks.
(import (chezscheme))
(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (log) log:) (prefix (actor) actor:)
             (prefix (kernel) kernel:) (prefix (test) test:))
     (define (record-count)
       (let-values ([(records end) (log:snapshot)]) end))
     (define head '(head "log test"))
     (define base (record-count))
     (define entry (actor:call-as head (lambda () (log:add! 'probe "hello" #f))))
     (let-values ([(records end) (log:snapshot base)])
       (test:check 'owned-actor-record-and-snapshot
         (list (and (integer? (log:time entry)) (exact? (log:time entry)) (> (log:time entry) 0))
               (cdr entry) (= end (+ base 1))
               (equal? records (list entry)) (eq? entry (car records)))
         (list #t (list head 'probe "hello") #t #t #f)))
     (log:add! 'other '(a . b) #f)
     (log:add! 'probe "again" #f)
     (test:check 'newest-first-component-queries-and-anonymous-base-work
       (list (map log:datum (log:entries 'probe))
             (map log:actor (log:entries 'other)))
       '(("again" "hello") ((base e))))
     (test:check 'snapshot-captures-one-tail-and-count
       (let-values ([(records end) (log:snapshot (+ base 1))])
         (list (map log:datum records) end))
       (list '("again" (a . b)) (+ base 3)))
     (test:check 'invalid-snapshots-and-subscriptions-refuse
       (map test:raises?
         (list (lambda () (log:snapshot -1)) (lambda () (log:snapshot 1.0))
               (lambda () (log:snapshot (+ (record-count) 1)))
               (lambda () (log:subscribe! #f)) (lambda () (log:add! "component" 'bad))))
       '(#t #t #t #t #t))

     ;; Formatters and histories operate on owned data. A formatter can
     ;; still fail or log recursively without changing the canonical record.
     (define (styler text) 'styles)
     (log:register-formatter! 'probe (lambda (d) (string-append "P: " d)) styler)
     (log:register-formatter! 'bad (lambda (d) (car d)))
     (test:check 'component-presentation-and-fallback
       (list (log:format-entry entry) (eq? (log:styler 'probe) styler) (log:styler 'other)
             (log:format-entry (log:add! 'raw '(1 2) #f))
             (log:format-entry (log:add! 'raw "verbatim" #f))
             (log:format-entry (log:add! 'bad "not-a-pair" #f)))
       '("P: hello" #t #f "(1 2)" "verbatim" "\"not-a-pair\""))
     (for-each (lambda (datum) (log:add! 'eval-like datum #f))
       '(("(+ 1 2)" . 3) ("(+ 1 2)" . 3) (ignored . value) ("(car x)" . err)))
     (for-each (lambda (datum) (log:add! 'file-like datum #f))
       '((Loaded . "/first") (Wrote . "/first") (Loaded . "/second")))
     (test:check 'histories-select-strings-and-collapse-consecutive-repeats
       (list (log:history 'eval-like car) (log:history 'file-like cdr))
       '(("(car x)" "(+ 1 2)") ("/second" "/first")))

     (let* ([actor (list 'head (string-copy "owned"))]
            [payload (vector (string-copy "first") (list 'item) (u8-list->bytevector '(1 2)))]
            [index (record-count)] [observed (test:recorder)])
       (define (change! e)
         (set-car! e 0)
         (string-set! (cadr (log:actor e)) 0 #\X)
         (let ([d (log:datum e)])
           (string-set! (vector-ref d 0) 0 #\X)
           (set-car! (vector-ref d 1) 'rewritten)
           (bytevector-u8-set! (vector-ref d 2) 0 9))
         (set-car! (cddr e) 'rewritten))
       (let* ([first (log:subscribe! (lambda (e mode) (when (eq? (log:component e) 'owned) (change! e))))]
              [second (log:subscribe! (lambda (e mode) (when (eq? (log:component e) 'owned) (observed e))))]
              [returned (actor:call-as actor (lambda () (log:add! 'owned payload #f)))]
              [original (car (log:entries 'owned))])
         (string-set! (cadr actor) 0 #\Y)
         (string-set! (vector-ref payload 0) 0 #\Y)
         (set-car! (vector-ref payload 1) 'caller)
         (bytevector-u8-set! (vector-ref payload 2) 0 8)
         (for-each change!
           (list returned (car (log:entries 'owned))
                 (let-values ([(records end) (log:snapshot index)]) (car records))))
         (log:register-formatter! 'owned
           (lambda (d) (string-set! (vector-ref d 0) 0 #\Z) (vector-ref d 0)))
         (log:format-entry (car (log:entries 'owned)))
         (log:history 'owned (lambda (d) (string-set! (vector-ref d 0) 0 #\Z) (vector-ref d 0)))
         (test:check 'input-results-readers-and-subscribers-own-independent-data
           (list (car (log:entries 'owned)) (observed)) (list original (list original)))
         (for-each log:unsubscribe! (list first second))))
     (let ([box (box 1)] [cycle (cons 'loop '())])
       (set-cdr! cycle cycle)
       (let ([opaque (log:add! 'opaque box #f)] [cyclic (log:add! 'opaque cycle #f)])
         (set-box! box 2)
         (set-car! cycle 'changed)
         (test:check 'runtime-and-cyclic-values-are-stable-written-snapshots
           (list (log:format-entry opaque) (log:format-entry cyclic))
           '("#&1" "#0=(loop . #0#)"))))

     ;; Initializer rollback does not undo runtime log records or enroll its
     ;; staged subscribers. One failed subscriber cannot stop another.
     (let ([heard (test:recorder)])
       (define (listen tag)
         (log:subscribe! (lambda (e mode) (heard (list tag (log:datum e))))))
       (parameterize ([kernel:registering-module 'log-test]) (listen 'old))
       (test:raises?
         (lambda ()
           (kernel:call-with-registration-update
             (lambda ()
               (kernel:retract-module! 'log-test)
               (parameterize ([kernel:registering-module 'log-test]) (listen 'aborted))
               (log:add! 'lifetime 'during #f)
               (error 'fixture "abort registration")))))
       (kernel:call-with-registration-update
         (lambda ()
           (kernel:retract-module! 'log-test)
           (parameterize ([kernel:registering-module 'log-test])
             (log:subscribe! (lambda (e mode) (error 'fixture "bad subscriber")))
             (listen 'new))))
       (log:add! 'lifetime 'after #f)
       (kernel:retract-module! 'log-test)
       (log:add! 'lifetime 'retired #f)
       (test:check 'runtime-delivery-uses-committed-registrations-and-isolates-failures
         (heard) '((old during) (new after))))

     ;; Hold delivery while writers cross growth boundaries. Readers see a
     ;; committed prefix; writers complete before the first callback is freed.
     ;; A reentrant append follows that prefix, with its parent's context.
     (let* ([start (record-count)] [arrived (test:gate)] [release (test:gate)]
            [heard (test:recorder)] [revoked (test:recorder)] [late (test:recorder)]
            [late-token #f] [fixed #f] [writer-count 4] [per-writer 80])
       (define (worker-actor i) (list 'agent (format "writer-~a" i)))
       (define (expected-mode datum)
         (cond [(eq? datum 'hold) 'progress] [(symbol? datum) 'append]
               [(zero? (mod (cdr datum) 3)) #f] [(odd? (cdr datum)) 'progress] [else 'append]))
       (define first-token
         (log:subscribe!
           (lambda (e mode)
             (when (eq? (log:component e) 'concurrent)
               (heard (list e mode (actor:current)))
               (when (eq? (log:datum e) 'hold)
                 (arrived #t)
                 (test:await 'release-log-delivery release))
               (when (equal? (log:datum e) '(0 . 0))
                 (log:add! 'concurrent 'nested))))))
       (define removed-token (log:subscribe! (lambda (e mode) (revoked e))))
       (define finish
         (test:worker
           (lambda ()
             (actor:call-as head
               (lambda () (parameterize ([log:progress #t]) (log:add! 'concurrent 'hold)))))))
       (test:await 'log-delivery-entered arrived)
       (dynamic-wind
         void
         (lambda ()
           (test:check 'writers-have-local-progress-and-complete-while-delivery-waits
             (let ([outside-progress (log:progress)])
               (cons outside-progress
                 (test:parallel (+ writer-count 1)
                   (lambda (i)
                     (if (< i writer-count)
                       (actor:call-as (worker-actor i)
                         (lambda ()
                           (do ([j 0 (+ j 1)]) ((= j per-writer) #t)
                             (parameterize ([log:progress (odd? j)])
                               (log:add! 'concurrent (cons i j) (not (zero? (mod j 3))))))))
                       (let loop ([n 40])
                         (or (zero? n)
                           (let-values ([(records end) (log:snapshot start)])
                             (and (= (length records) (- end start))
                                  (for-all (lambda (e) (eq? (log:component e) 'concurrent)) records)
                                  (loop (- n 1)))))))))))
             '(#f #t #t #t #t #t))
           (set! fixed (call-with-values (lambda () (log:snapshot start)) list))
           (log:unsubscribe! removed-token)
           (set! late-token (log:subscribe! (lambda (e mode) (late (log:datum e)))))
           ;; Old and fresh producer code share the writer, subscriptions,
           ;; progress context and queue even while an old callback is active.
           (load "lib/log.e")
           (eval '(begin (import (prefix (log) reloaded-log:)) (reloaded-log:add! 'concurrent 'fresh))))
         (lambda () (release #t)))
       (finish)
       (let-values ([(records end) (log:snapshot start)])
         (let ([ordered (reverse records)] [delivered (heard)])
           (test:check 'every-record-survives-concurrent-growth-once
             (list (- end start)
                   (list-sort < (map (lambda (d) (+ (* (car d) per-writer) (cdr d)))
                                     (filter pair? (map log:datum records)))))
             (list (+ 3 (* writer-count per-writer)) (iota (* writer-count per-writer))))
           (test:check 'delivery-keeps-append-order-modes-and-originating-actors
             delivered
             (map (lambda (e) (list e (expected-mode (log:datum e)) (log:actor e))) ordered))
           (test:check 'actor-attribution-survives-threads-reentry-and-redefinition
             (for-all (lambda (e)
                        (equal? (log:actor e)
                          (case (log:datum e)
                            [(hold) head] [(nested) (worker-actor 0)] [(fresh) '(base e)]
                            [else (worker-actor (car (log:datum e)))]))) ordered)
             #t)
           (test:check 'snapshots-do-not-grow-and-subscription-lifetimes-honor-the-commit
             (list (length (car fixed)) (- (cadr fixed) start) (revoked) (late))
             (list (+ 1 (* writer-count per-writer)) (+ 1 (* writer-count per-writer)) '() '(fresh nested)))))
       (for-each log:unsubscribe! (list first-token late-token)))

     (test:finish! 'log)))
