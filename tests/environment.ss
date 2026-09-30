#!/usr/bin/env scheme-script
;; Process boundaries share one compact fixture; service invariants stay local.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (core worker) worker:) (prefix (test) test:))
     (define recipe
       (list (cons 'directory (current-directory)) '(roots) '(values (seed . 7))
         '(imports (chezscheme) (prefix (service resource) resource:))))
     (define output '())
     (define names '())
     (define (emit message)
       (case (car message)
         [(output) (set! output (cons (cdr message) output))]
         [(catalogue) (set! names (append names (list-ref message 3)))]))
     (define (broker operation . args)
       (if (equal? (cons operation args) '(read shared)) '(3 #(borrowed))
         (error 'broker "unlisted resource")))
     (define a (worker:open!))
     (define b (worker:open!))
     (define (run w text) (worker:request! w (list 'evaluate text) emit broker))
     (define (values-of result) (cadr (caddr result)))
     (dynamic-wind void
       (lambda ()
         (worker:request! a (list 'initialize recipe) emit broker)
         (worker:request! b (list 'initialize recipe) (lambda (m) (void)) broker)
         (test:check 'workers-keep-native-definitions-separate-and-broker-resources
           (list (values-of (run a "(define private (+ seed 1)) private"))
             (cadr (run b "private"))
             (values-of (run a "(resource:read 'shared)"))
             (cadr (run a "(resource:read 'missing)"))
             (and (member "private" names) #t))
           '((8) error ((3 #(borrowed))) error #t))
         (set! output '())
         (let ([result (run a "(display (make-string 18000 #\\x)) (display \"warning\" (current-error-port)) (values #f 42)")])
           (test:check 'worker-output-is-bounded-without-newlines-and-values-are-portable
             (list (values-of result) (for-all (lambda (p) (<= (string-length (cadr p)) 4096)) output)
               (apply + (map (lambda (p) (if (eq? (car p) 'stdout) (string-length (cadr p)) 0)) output))
               (apply string-append (map cadr (reverse (filter (lambda (p) (eq? (car p) 'stderr)) output)))))
             '((#f 42) #t 18000 "warning")))
         (for-each
           (lambda (text)
             (let* ([result (run a text)] [value (caddr result)])
               (test:check 'live-results-use-bounded-generation-local-handles
                 (list (cadr result) (car value) (<= (string-length (caddr value)) 1025)
                   (worker:request! a (list 'release (list (cadr value))) (lambda (m) (void)) broker))
                 '(ok handle #t (released)))))
           '("(lambda (x) (+ x private))" "(let ([x (list 1)]) (set-cdr! x x) x)" "(make-string 20000 #\\z)"))
         ;; Readiness is observed through actual captured output, not a sleep.
         (let* ([ready (test:gate)]
                [finish (test:worker (lambda ()
                                       (guard (ex [else (unless (ready) (ready (list 'failed ex))) #t])
                                         (worker:request! a '(evaluate "(display \"ready\\n\") (let loop () (loop))")
                                           (lambda (m) (when (eq? (car m) 'output) (ready #t))) broker)
                                         (unless (ready) (ready 'completed)) #f)))])
           (test:await 'worker-running ready)
           (test:check 'worker-streams-before-finishing (ready) #t)
           (worker:close! a)
           (test:check 'running-worker-cancellation-reaps-without-affecting-other-groups
             (list (finish) (values-of (run b "seed"))) '(#t (7)))))
       (lambda () (worker:close! a) (worker:close! b)))
     (test:finish! 'environment)))
