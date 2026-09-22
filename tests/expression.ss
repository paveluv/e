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
             (prefix (head keymap) keymap:)
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

     ;; up, down and over lists, the edges of top-level forms, marks, kills,
     ;; a transposition and an indentation, all on the same spans
     (scheme-mode:init!)
     (define lists (fresh "lists" '("(define (f x)" "  (+ x 1))" "(g 1 2)" "")))
     (head:goto! '(1 . 5))
     (check 'up-leaves-the-enclosing-lists-then-stops
       (map after (list up-expression! up-expression! up-expression!)) '((1 . 2) (0 . 0) (0 . 0)))
     (check 'down-enters-the-next-lists-then-stops
       (map after (list down-expression! down-expression! down-expression!)) '((0 . 1) (0 . 9) (0 . 9)))
     (head:goto! '(0 . 0))
     (check 'next-list-skips-atoms-then-stops
       (map after (list next-list! next-list! next-list!)) '((1 . 10) (2 . 7) (2 . 7)))
     (check 'previous-list-comes-back-then-stops
       (map after (list previous-list! previous-list! previous-list!)) '((2 . 0) (0 . 0) (0 . 0)))
     (head:goto! '(1 . 5))
     (check 'beginning-and-end-of-form-walk-the-top-level-forms
       (list (after beginning-of-form!) (after beginning-of-form!) (after end-of-form!) (after end-of-form!) (after end-of-form!))
       '((0 . 0) (0 . 0) (1 . 10) (2 . 7) (2 . 7)))
     (head:goto! '(1 . 5))
     (mark-form!)
     (check 'mark-form-selects-the-top-level-form (list (head:point) (head:mark)) '((0 . 0) (1 . 10)))
     (head:goto! '(2 . 1))
     (mark-expression!) (mark-expression!) (mark-expression!)
     (check 'mark-expression-extends-by-one-expression-each-time (list (head:point) (head:mark)) '((2 . 1) (2 . 6)))
     (head:goto! '(2 . 3))
     (transpose-expressions!)
     (check 'transpose-swaps-the-expressions-around-point
       (list (head:buffer-line lists 2) (head:point)) '("(1 g 2)" (2 . 4)))
     (head:goto! '(2 . 1))
     (kill-expression!)
     (head:set-last-command! kill-expression!)
     (kill-expression!)
     (check 'kill-expression-accumulates-forward (list (head:buffer-line lists 2) (head:copy-text)) '("( 2)" "1 g"))
     (head:goto! '(2 . 4))
     (head:set-last-command! kill-expression!)
     (backward-kill-expression!)
     (check 'backward-kill-prepends-to-the-accumulated-kill (list (head:buffer-line lists 2) (head:copy-text)) '("" "( 2)1 g"))
     (define indenting (fresh "indenting" '("(define (h)" "(+ 1" "2))" "")))
     (mode:choose! indenting "scheme")
     (head:goto! '(0 . 0))
     (indent-expression!)
     (check 'indent-expression-indents-the-lines-below-the-first
       (list (head:buffer-line indenting 0) (head:buffer-line indenting 1) (head:buffer-line indenting 2))
       '("(define (h)" "  (+ 1" "    2))"))

     ;; the long Control-Meta spellings parse, so the default bindings install
     (check 'control-meta-spellings-of-special-keys-parse
       (list (keymap:spec "C-M-BACKSPACE") (keymap:spec "C-M-SPC") (keymap:spec "C-M-@"))
       '(("C-M-BACKSPACE") ("C-M-SPC") ("C-M-@")))

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

     ;; *scratch* speaks Scheme once the mode is registered (it was, above)
     (check 'scratch-has-scheme-mode-by-default
       (mode:name-of (head:buffer-named "*scratch*")) "scheme")

     (test:finish! 'expression)))
