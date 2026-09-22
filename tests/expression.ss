#!/usr/bin/env scheme-script

;; Motion by expression and evaluation of the expression before point or of
;; the top-level form around it, on the spans Chez's reader gives a buffer's
;; text; *scratch* speaks Scheme.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (head expression) expression:)
             (prefix (head mode) mode:)
             (prefix (apps eval) eval:)
             (prefix (service log) log:)
             (prefix (modes scheme-mode) scheme-mode:))

     (define check test:check)
     (define (fresh name lines)
       (let ([b (head:new-buffer! name)])
         (head:buffer-lines-set! b (list->vector lines))
         (head:show-buffer! b)
         (head:goto! '(0 . 0))
         b))
     (define (spans->list text) (map vector->list (expression:spans text)))

     ;; spans: every datum at every depth, an outer span before its parts
     (check 'spans-list-every-datum-outer-first
       (spans->list "(a (b c) d)")
       '((0 11 #t) (1 2 #f) (3 8 #t) (4 5 #f) (6 7 #f) (9 10 #f)))
     (check 'unfinished-forms-keep-their-complete-parts
       (list (spans->list "(a (b") (spans->list "x) 'y") (spans->list "\"s)\" ; c\n#;(gone) z"))
       '(((1 2 #f) (4 5 #f)) ((0 1 #f) (3 5 #t) (4 5 #f)) ((0 4 #f) (18 19 #f))))

     ;; motion crosses whole expressions, strings and quotes included, and
     ;; stays inside the enclosing one
     (fresh "expressions" '("(define (f x)" "  (+ x 1))" "'(a b) \"s)\" ; c" "z"))
     (define (after thunk) (thunk) (head:point))
     (check 'forward-crosses-top-level-expressions-then-stops
       (map after (list forward-expression! forward-expression! forward-expression! forward-expression! forward-expression!))
       '((1 . 10) (2 . 6) (2 . 11) (3 . 1) (3 . 1)))
     (check 'backward-crosses-them-back-then-stops
       (map after (list backward-expression! backward-expression! backward-expression! backward-expression! backward-expression!))
       '((3 . 0) (2 . 7) (2 . 0) (0 . 0) (0 . 0)))
     (head:goto! '(1 . 6))
     (check 'inside-a-list-motion-stays-inside-it
       (list (after backward-expression!) (begin (head:goto! '(1 . 6)) (after forward-expression!)) (after forward-expression!))
       '((1 . 5) (1 . 8) (1 . 8)))
     (head:goto! '(0 . 3))
     (check 'inside-an-atom-motion-crosses-the-atom
       (list (after forward-expression!) (begin (head:goto! '(0 . 5)) (after backward-expression!)))
       '((0 . 7) (0 . 1)))

     ;; evaluation of the expression before point and of the top-level form around it
     (fresh "evaluations" '("(define ex-forty 40)" "(list 1 (+ 2 3) 4)" "(+ ex-forty 2)" ""))
     (define (last-eval) (log:datum (car (log:entries 'eval))))
     (head:goto! '(0 . 20))
     (eval:last-expression!)
     (head:goto! '(1 . 15))
     (eval:last-expression!)
     (check 'last-expression-evaluates-the-expression-before-point (last-eval) '("(+ 2 3)" . "5"))
     (head:goto! '(2 . 14))
     (eval:last-expression!)
     (check 'a-definition-evaluated-before-point-took-effect (last-eval) '("(+ ex-forty 2)" . "42"))
     (head:goto! '(1 . 9))
     (eval:top-level-form!)
     (check 'top-level-form-evaluates-the-form-around-point (last-eval) '("(list 1 (+ 2 3) 4)" . "(1 5 4)"))
     (head:goto! '(3 . 0))
     (eval:top-level-form!)
     (check 'top-level-form-after-the-last-takes-the-last (last-eval) '("(+ ex-forty 2)" . "42"))
     (fresh "nothing" '(""))
     (check 'no-expression-before-point-is-an-error (test:raises? eval:last-expression!) #t)

     ;; *scratch* speaks Scheme once the mode is registered
     (scheme-mode:init!)
     (mode:refresh!)
     (check 'scratch-has-scheme-mode-by-default
       (mode:name-of (head:buffer-named "*scratch*")) "scheme")

     (test:finish! 'expression)))
