#!/usr/bin/env scheme-script

;; Motion by expression and evaluation of the expression before point or of
;; the top-level form around it, on the spans Chez's reader gives a buffer's
;; text; *scratch* speaks Scheme.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-evaluate!
  '(begin
     (import (prefix (test) test:)
             (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (state store) store:) (prefix (state view) view:) (prefix (head interaction) interaction:) (prefix (head widget) widget:)
             (prefix (core kernel) kernel:)
             (prefix (head expression) expression:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:)
             (prefix (apps eval) eval:)
             (prefix (service log) log:)
             (prefix (modes scheme-mode) scheme-mode:))

     (define check test:check)
     (for-each kernel:load-module! '("widget" "edit" "scheme-mode"))
     (define editor #f)
     (define (point) (car (view:state (interaction:snapshot editor))))
     (define (mark) (cadr (view:state (interaction:snapshot editor))))
     (define (fresh name lines)
       (when editor (widget:unmount! editor))
       (let ([b (store:create! head:ui-actor name lines)])
         (set! editor (edit:create-view! head:ui-actor b '()))
         (widget:mount! editor 'expression-test)
         (widget:present! (list (list (widget:prepare! editor 80 12) 0 0))) b))
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
     (define (after thunk) (guard (ex [(kernel:refusal? ex) (void)]) (thunk editor)) (point))
     (check 'forward-crosses-top-level-expressions-then-stops
       (map after (list edit:forward-expression! edit:forward-expression! edit:forward-expression! edit:forward-expression! edit:forward-expression!))
       '((1 . 10) (2 . 6) (2 . 11) (3 . 1) (3 . 1)))
     (check 'backward-crosses-them-back-then-stops
       (map after (list edit:backward-expression! edit:backward-expression! edit:backward-expression! edit:backward-expression! edit:backward-expression!))
       '((3 . 0) (2 . 7) (2 . 0) (0 . 0) (0 . 0)))
     (edit:move! editor '(1 . 6))
     (check 'inside-a-list-motion-stays-inside-it
       (list (after edit:backward-expression!) (begin (edit:move! editor '(1 . 6)) (after edit:forward-expression!)) (after edit:forward-expression!))
       '((1 . 5) (1 . 8) (1 . 8)))
     (edit:move! editor '(0 . 3))
     (check 'inside-an-atom-motion-crosses-the-atom
       (list (after edit:forward-expression!) (begin (edit:move! editor '(0 . 5)) (after edit:backward-expression!)))
       '((0 . 7) (0 . 1)))

     ;; up, down and over lists, the edges of top-level forms, marks, kills,
     ;; a transposition and an indentation, all on the same spans
     (define lists (fresh "lists" '("(define (f x)" "  (+ x 1))" "(g 1 2)" "")))
     (edit:move! editor '(1 . 5))
     (check 'up-leaves-the-enclosing-lists-then-stops
       (map after (list edit:up-expression! edit:up-expression! edit:up-expression!)) '((1 . 2) (0 . 0) (0 . 0)))
     (check 'down-enters-the-next-lists-then-stops
       (map after (list edit:down-expression! edit:down-expression! edit:down-expression!)) '((0 . 1) (0 . 9) (0 . 9)))
     (edit:move! editor '(0 . 0))
     (check 'next-list-skips-atoms-then-stops
       (map after (list edit:next-list! edit:next-list! edit:next-list!)) '((1 . 10) (2 . 7) (2 . 7)))
     (check 'previous-list-comes-back-then-stops
       (map after (list edit:previous-list! edit:previous-list! edit:previous-list!)) '((2 . 0) (0 . 0) (0 . 0)))
     (edit:move! editor '(1 . 5))
     (check 'beginning-and-end-of-form-walk-the-top-level-forms
       (list (after edit:beginning-of-form!) (after edit:beginning-of-form!) (after edit:end-of-form!) (after edit:end-of-form!) (after edit:end-of-form!))
       '((0 . 0) (0 . 0) (1 . 10) (2 . 7) (2 . 7)))
     (edit:move! editor '(1 . 5))
     (edit:mark-form! editor)
     (check 'mark-form-selects-the-top-level-form (list (point) (mark)) '((0 . 0) (1 . 10)))
     (edit:move! editor '(2 . 1))
     (edit:mark-expression! editor) (edit:mark-expression! editor) (edit:mark-expression! editor)
     (check 'mark-expression-extends-by-one-expression-each-time (list (point) (mark)) '((2 . 1) (2 . 6)))
     (edit:move! editor '(2 . 3))
     (edit:transpose-expressions! editor)
     (check 'transpose-swaps-the-expressions-around-point
       (list (store:line lists 2) (point)) '("(1 g 2)" (2 . 4)))
     (edit:move! editor '(2 . 1))
     (edit:kill-expression! editor)
     (head:set-last-command! edit:kill-expression!)
     (edit:kill-expression! editor)
     (check 'kill-expression-accumulates-forward (list (store:line lists 2) (edit:copy-text)) '("( 2)" "1 g"))
     (edit:move! editor '(2 . 4))
     (head:set-last-command! edit:kill-expression!)
     (edit:backward-kill-expression! editor)
     (check 'moving-point-ends-kill-accumulation (list (store:line lists 2) (edit:copy-text)) '("" "( 2)"))
     (define indenting (fresh "indenting" '("(define (h)" "(+ 1" "2))" "")))
     (mode:choose! "scheme" indenting)
     (edit:move! editor '(0 . 0))
     (edit:indent-expression! editor)
     (check 'indent-expression-indents-the-lines-below-the-first
       (list (store:line indenting 0) (store:line indenting 1) (store:line indenting 2))
       '("(define (h)" "  (+ 1" "    2))"))

     ;; the long Control-Meta spellings parse, so the default bindings install
     (check 'control-meta-spellings-of-special-keys-parse
       (list (keymap:spec "C-M-BACKSPACE") (keymap:spec "C-M-SPC") (keymap:spec "C-M-@"))
       '(("C-M-BACKSPACE") ("C-M-SPC") ("C-M-@")))

     ;; evaluation of the expression before point and of the top-level form around it
     (fresh "evaluations" '("(define ex-forty 40)" "(list 1 (+ 2 3) 4)" "(+ ex-forty 2)" ""))
     (define (last-eval) (log:datum (car (log:entries 'eval:report!))))
     (edit:move! editor '(0 . 20))
     (eval:last-expression! editor)
     (edit:move! editor '(1 . 15))
     (eval:last-expression! editor)
     (check 'last-expression-evaluates-the-expression-before-point (last-eval) '("(+ 2 3)" . "5"))
     (edit:move! editor '(2 . 14))
     (eval:last-expression! editor)
     (check 'a-definition-evaluated-before-point-took-effect (last-eval) '("(+ ex-forty 2)" . "42"))
     (edit:move! editor '(1 . 9))
     (eval:top-level-form! editor)
     (check 'top-level-form-evaluates-the-form-around-point (last-eval) '("(list 1 (+ 2 3) 4)" . "'(1 5 4)"))
     (edit:move! editor '(3 . 0))
     (eval:top-level-form! editor)
     (check 'top-level-form-after-the-last-takes-the-last (last-eval) '("(+ ex-forty 2)" . "42"))
     (fresh "nothing" '(""))
     (check 'no-expression-before-point-is-an-error (test:raises? (lambda () (eval:last-expression! editor))) #t)

     ;; *scratch* speaks Scheme once the mode is registered (it was, above)
     (check 'scratch-has-scheme-mode-by-default
       (mode:name-of (fresh "*scratch*" '(""))) "scheme")

     (test:finish! 'expression)))
