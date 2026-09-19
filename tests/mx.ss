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
     (import (except (edit) init!) (literal) (prefix (eval) eval:) (prefix (head) head:) (prefix (text) text:)
             (prefix (string) string:) (prefix (test) test:))

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

     ;; At an argument position the type documented for it decides what Tab
     ;; offers: the type's values as expressions, the procedures producing
     ;; one, and the variables holding one; symbols complete elsewhere.
     (define (labels text) (eval:completion-candidates text (string-length text)))
     (define (has? needle candidates) (and candidates (exists (lambda (l) (string=? l needle)) candidates) #t))
     (define (has-prefix? needle candidates) (and candidates (exists (lambda (l) (string:prefix? needle l)) candidates) #t))
     (eval '(define myb (buffer "*scratch*")) (interaction-environment))
     (check 'a-buffer-argument-offers-buffers-producers-and-variables
       (let ([offered (labels "(show-buffer! ")])
         (list (has? "(buffer \"*scratch*\")" offered) (has? "(current-buffer)" offered)
               (has? "(fresh-buffer name)" offered) (has? "(head:new-local-buffer name)" offered) (has? "myb" offered)
               ;; a typed token narrows, and the buffer's spelling leads
               (car (labels "(show-buffer! scr")) (has? "myb" (labels "(show-buffer! my"))
               ;; a token matches a candidate's own text, never the formals of its label
               (has? "(head:new-local-buffer name)" (labels "(show-buffer! name"))
               ;; the alias of a symbol completing elsewhere: an operator position
               (labels "(show-buff")))
       '(#t #t #t #t #t "(buffer \"*scratch*\")" #t #f #f))
     ;; The operator position of a nested form takes the enclosing argument's
     ;; type: (bu offers what bu offers less the bare variables, and Tab
     ;; extends a token to the longest text every candidate still matches,
     ;; a sole candidate whole.
     (define (extensions text) (eval:completion-extensions text (string-length text)))
     (check 'a-nested-operator-completes-to-the-enclosing-arguments-type
       (let ([nested (labels "(show-buffer! (bu")])
         (list (has? "(buffer \"*scratch*\")" nested) (has? "(fresh-buffer name)" nested) (has? "myb" nested)
               (labels "(show-buffer! (curr") (extensions "(show-buffer! (curr")
               (extensions "(show-buffer! bu") (extensions "(show-buffer! (bu")
               ;; a variable holding a buffer keeps the token bare
               (extensions "(show-buffer! my") (extensions "(show-buffer! ")
               (extensions "(set-buffer-wrap! b 'c") (extensions "(visit-file! \"man")
               ;; a quoted form, or one whose operator is undocumented, completes symbols
               (labels "(show-buffer! '(bu") (labels "(list (bu")))
       '(#t #t #f ("(current-buffer)") ("(current-buffer)") ("(buffer") ("(buffer") ("myb") ("")
         ("'clean") ("manual/") #f #f))
     (check 'literals-and-strings-complete-in-place
       (list (has? "'clean" (labels "(set-buffer-wrap! b ")) (has? "#f" (labels "(set-buffer-wrap! b "))
             ;; the language's types offer their own values but no producers
             (length (labels "(set-buffer-wrap! b ")) (labels "(wrap! ")
             (has-prefix? "manual/" (labels "(visit-file! \"man"))
             (has? "*scratch*" (labels "(buffer \""))
             ;; an undocumented operator falls back to symbols
             (labels "(car "))
       '(#t #t 4 ("#t" "#f") #t #t #f))
     (check 'a-completed-value-settles-its-form
       (list (settled "(show-buffer! (buffer \"*scratch*\")") (settled "(visit-file! \"manual/EVAL.md\""))
       '(("(show-buffer! (buffer \"*scratch*\"))" . 35) ("(visit-file! \"manual/EVAL.md\")" . 30)))

     (test:finish! 'mx)))
