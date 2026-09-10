#!/usr/bin/env scheme-script

;; Owned protocol data: copies share nothing mutable, refuse cycles and
;; runtime objects, and cost one step per node.  Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (datum) datum:) (prefix (test) test:))

     (define check test:check)
     (define (refused? thunk)
       (guard (ex [(and (message-condition? ex) (string=? (condition-message ex) "cyclic protocol data")) 'cyclic]
                  [(message-condition? ex) (condition-message ex)])
         (thunk) 'copied))

     (define original (list "text" (vector 1 "two" (cons 3 4)) 'symbol #\c 2.5 #vu8(1 2) '()))
     (define copied (datum:copy original))
     (check 'copies-are-equal-and-share-nothing-mutable
            (list (equal? copied original)
                  (eq? (car copied) (car original))
                  (eq? (cadr copied) (cadr original))
                  (eq? (vector-ref (cadr copied) 1) (vector-ref (cadr original) 1))
                  (eq? (list-ref copied 5) (list-ref original 5)))
            '(#t #f #f #f #f))

     (check 'improper-tails-and-nested-lists-survive
            (datum:copy '((1 2 . 3) (4 (5 (6))) . "tail"))
            '((1 2 . 3) (4 (5 (6))) . "tail"))

     (check 'sharing-is-not-a-cycle
            (let* ([shared (list 1 2)] [twice (list shared shared (vector shared))])
              (refused? (lambda () (datum:copy twice))))
            'copied)

     (check 'forward-references-along-a-spine-are-sharing
            (let ([spine (list 1 2 3)])
              (set-car! spine (cdr spine))
              (refused? (lambda () (datum:copy spine))))
            'copied)

     (check 'cycles-are-refused
            (map refused?
                 (list (lambda () (let ([l (list 1 2)]) (set-cdr! (cdr l) l) (datum:copy l)))
                       (lambda () (let ([l (list 1 2 3)]) (set-car! (cddr l) (cdr l)) (datum:copy l)))
                       (lambda () (let ([v (vector 1 #f)]) (vector-set! v 1 v) (datum:copy v)))
                       (lambda () (let ([v (vector 1 #f)]) (vector-set! v 1 (list v)) (datum:copy v)))))
            '(cyclic cyclic cyclic cyclic))

     (check 'runtime-objects-are-refused-or-handed-to-the-leaf-copier
            (list (refused? (lambda () (datum:copy (list (lambda () 1)))))
                  (datum:copy (list 1 (lambda () 1)) (lambda (leaf) 'leaf)))
            '("expected plain protocol data" (1 leaf)))

     ;; The path-based check cost N^2/2 steps on a list: 1.5 s for 40,000
     ;; elements. One step per node leaves the bound far below that even on
     ;; a loaded machine.
     (define (elapsed-ms thunk)
       (let ([start (current-time 'time-monotonic)])
         (thunk)
         (let ([end (current-time 'time-monotonic)])
           (+ (* 1000 (- (time-second end) (time-second start)))
              (div (- (time-nanosecond end) (time-nanosecond start)) 1000000)))))
     (check 'long-lists-copy-in-linear-time
            (let* ([lines (map (lambda (i) "line") (iota 40000))]
                   [ms (elapsed-ms (lambda () (unless (= (length (datum:copy lines)) 40000)
                                               (error 'datum-test "lost elements"))))])
              (< ms 200))
            #t)

     (test:finish! 'datum)))
