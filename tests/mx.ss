#!/usr/bin/env scheme-script

;; M-x settles a sole completion: forms close with their matching bracket
;; and the cursor steps to the next argument while every enclosing operator
;; has a fixed arity; an unknown arity, a quoted form or text after the
;; cursor leaves the cursor at the symbol. Headless, against the live
;; environment's own procedures. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (edit) init!) (prefix (eval) eval:) (prefix (head) head:) (prefix (text) text:)
             (prefix (test) test:))

     (define check test:check)
     (define (settled text) (eval:settle-completion text (string-length text)))

     (for-each
       (lambda (case)
         (check (list 'settle (car case)) (settled (car case)) (cons (cadr case) (string-length (cadr case)))))
       '(;; a nullary operator closes its form; one taking arguments steps to the first
         ("(split-window-right!" "(split-window-right!)")
         ("(head:window-index" "(head:window-index ")
         ;; the last argument closes the form, an earlier one steps on
         ("(head:window-index (head:current" "(head:window-index (head:current))")
         ("(text:make-span 1 2" "(text:make-span 1 2 ")
         ;; closing a form settles it as an argument of its parent, recursively
         ("(head:window-numbered (head:window-index (head:current" "(head:window-numbered (head:window-index (head:current)))")
         ;; brackets close with their own kind; a closed form settles as an
         ;; argument of its parent, or stops at an operator without an arity
         ("(vector-ref {head:current" "(vector-ref {head:current} ")
         ("(let ([x (head:current" "(let ([x (head:current)")
         ;; rest and optional parameters, syntax, and unbound names are unknown arities
         ("(list foo" "(list foo")
         ("(define foo" "(define foo")
         ("(no-such-procedure-here" "(no-such-procedure-here")
         ;; a quoted or quasiquoted form is data
         ("'(split-window-right!" "'(split-window-right!")
         ("`(head:current" "`(head:current")
         ("(list '(head:current" "(list '(head:current")
         ;; too many arguments already: nothing to close
         ("(head:current x" "(head:current x")
         ;; a bare symbol has no form
         ("head:current" "head:current")))

     ;; Text after the cursor is left alone; blank text after it is kept.
     (check 'text-after-the-symbol-stops-the-settling
       (eval:settle-completion "(head:current 1)" 13) '("(head:current 1)" . 13))
     (check 'blank-tail-is-kept
       (eval:settle-completion "(head:current  " 13) '("(head:current)  " . 14))

     (test:finish! 'mx)))
