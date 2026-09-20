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
     (import (except (edit) init!) (literal) (prefix (eval) eval:) (prefix (actor) actor:) (prefix (keymap) keymap:) (prefix (head) head:)
             (prefix (window) window:) (prefix (text) text:)
             (prefix (string) string:) (prefix (test) test:))

     (define check test:check)
     (define (settled text) (eval:settle-completion text (string-length text)))

     (for-each
       (lambda (case)
         (check (list 'settle (car case)) (settled (car case)) (cons (cadr case) (string-length (cadr case)))))
       '(;; a nullary operator closes its form; one taking arguments steps to the first
         ("(window:split-right!" "(window:split-right!)")
         ("(head:window-index" "(head:window-index ")
         ;; the last argument closes the form, an earlier one steps on
         ("(head:window-index (head:current-window" "(head:window-index (head:current-window))")
         ("(text:make-span 1 2" "(text:make-span 1 2 ")
         ;; closing a form settles it as an argument of its parent, recursively
         ("(head:window-numbered (head:window-index (head:current-window" "(head:window-numbered (head:window-index (head:current-window)))")
         ;; brackets close with their own kind; a closed form settles as an
         ;; argument of its parent, or stops at an operator without an arity
         ("(vector-ref {head:current-window" "(vector-ref {head:current-window} ")
         ("(let ([x (head:current-window" "(let ([x (head:current-window)")
         ;; rest and optional parameters, syntax, and unbound names are unknown arities
         ("(list foo" "(list foo")
         ("(define foo" "(define foo")
         ("(no-such-procedure-here" "(no-such-procedure-here")
         ;; a quoted or quasiquoted form is data
         ("'(window:split-right!" "'(window:split-right!")
         ("`(head:current-window" "`(head:current-window")
         ("(list '(head:current-window" "(list '(head:current-window")
         ;; too many arguments already: nothing to close
         ("(head:current-window x" "(head:current-window x")
         ;; a bare symbol has no form
         ("head:current-window" "head:current-window")))

     ;; Text after the cursor is left alone; blank text after it is kept.
     (check 'text-after-the-symbol-stops-the-settling
       (eval:settle-completion "(head:current-window 1)" 20) '("(head:current-window 1)" . 20))
     (check 'blank-tail-is-kept
       (eval:settle-completion "(head:current-window  " 20) '("(head:current-window)  " . 21))

     ;; At an argument position the type documented for it decides what Tab
     ;; offers: the type's values as expressions, the procedures producing
     ;; one, and the variables holding one; symbols complete elsewhere.
     (define (labels text) (eval:completion-candidates text (string-length text)))
     (define (has? needle candidates) (and candidates (exists (lambda (l) (string=? l needle)) candidates) #t))
     (define (has-prefix? needle candidates) (and candidates (exists (lambda (l) (string:prefix? needle l)) candidates) #t))
     (eval '(define myb (buffer "*scratch*")) (interaction-environment))
     (check 'a-buffer-argument-offers-buffers-producers-and-variables
       (let ([offered (labels "(head:show-buffer! ")])
         (list (has? "(buffer \"*scratch*\")" offered) (has? "(head:current-buffer)" offered)
               (has? "(fresh-buffer! name)" offered) (has? "(head:new-local-buffer! name)" offered) (has? "myb" offered)
               ;; a typed token narrows, and the buffer's spelling leads
               (car (labels "(head:show-buffer! scr")) (has? "myb" (labels "(head:show-buffer! my"))
               ;; a token matches a candidate's own text, never the formals of its label
               (has? "(head:new-local-buffer! name)" (labels "(head:show-buffer! name"))
               ;; the alias of a symbol completing elsewhere: an operator position
               (labels "(show-buff")))
       '(#t #t #t #t #t "(buffer \"*scratch*\")" #t #f #f))
     ;; The operator position of a nested form takes the enclosing argument's
     ;; type: (bu offers what bu offers less the bare variables, and Tab
     ;; extends a token to the longest text every candidate still matches,
     ;; a sole candidate whole.
     (define (extensions text) (eval:completion-extensions text (string-length text)))
     (check 'a-nested-operator-completes-to-the-enclosing-arguments-type
       (let ([nested (labels "(head:show-buffer! (bu")])
         (list (has? "(buffer \"*scratch*\")" nested) (has? "(fresh-buffer! name)" nested) (has? "myb" nested)
               (labels "(head:show-buffer! (curr") (extensions "(head:show-buffer! (curr")
               (extensions "(head:show-buffer! bu") (extensions "(head:show-buffer! (bu")
               ;; a variable holding a buffer keeps the token bare
               (extensions "(head:show-buffer! my") (extensions "(head:show-buffer! ")
               (extensions "(set-buffer-wrap! b 'c") (extensions "(visit-file! \"man")
               ;; a quoted form, or one whose operator is undocumented, completes symbols
               (labels "(head:show-buffer! '(bu") (labels "(list (bu")))
       '(#t #t #f ("(head:current-buffer)") ("(head:current-buffer)") ("(buffer") ("(buffer") ("myb") ("")
         ("'clean") ("manual/") #f #f))
     (check 'literals-and-strings-complete-in-place
       (list (has? "'clean" (labels "(set-buffer-wrap! b ")) (has? "#f" (labels "(set-buffer-wrap! b "))
             ;; the language's types offer their own values but no producers
             (length (labels "(set-buffer-wrap! b ")) (labels "(window:set-wrap! ")
             (has-prefix? "manual/" (labels "(visit-file! \"man"))
             (has? "*scratch*" (labels "(buffer \""))
             ;; an undocumented operator falls back to symbols
             (labels "(car "))
       '(#t #t 4 ("#t" "#f" "'default") #t #t #f))
     ;; a scope form's argument completes by type, syntax or not
     (check 'a-scope-form-completes-its-argument-by-type
       (list (has-prefix? "(buffer \"" (labels "(head:with-buffer (bu")) (has? "(head:current-buffer)" (labels "(head:with-buffer (bu"))
             (has? "(current-region)" (labels "(with-region (re")) (has-prefix? "(window " (labels "(window:with-window (wi")))
       '(#t #t #t #t))
     (check 'a-completed-value-settles-its-form
       (list (settled "(head:show-buffer! (buffer \"*scratch*\")") (settled "(visit-file! \"manual/EVAL.md\""))
       '(("(head:show-buffer! (buffer \"*scratch*\"))" . 40) ("(visit-file! \"manual/EVAL.md\")" . 30)))

     ;; Identities are literals too: (head "desk") and (agent "claude") read
     ;; back as they print, and an actor argument completes from the directory.
     (check 'identities-read-back-as-they-print
       (list (head "desk") (agent 'tester) (base 'e) (guard (ex [else 'refused]) (head "")))
       '((head "desk") (agent tester) (base e) refused))
     (actor:register! '(agent "helper") (lambda (m) (void)))
     (check 'an-actor-argument-offers-the-directory
       (list (has? "(agent \"helper\")" (labels "(actor:send! ")) (has? "(agent \"helper\")" (labels "(actor:describe "))
             ;; the constructors produce identities, a head's refining an actor's
             (has? "(head name . more)" (labels "(actor:send! ")) (has? "(actor:current)" (labels "(actor:send! "))
             ;; the token extends to what the value and the constructor share,
             ;; never into a string no documented operator opened
             (extensions "(actor:send! (age")
             (exists (lambda (e) (memv #\" (string->list e))) (extensions "(actor:send! (he"))
             ;; inside the constructor the name completes from the directory
             (labels "(actor:send! (agent \"h") (extensions "(actor:send! (agent \"h")
             (labels "(actor:send! (agent \"helper\") "))
       '(#t #t #t #t ("(agent") #f ("helper") ("helper\"") #f))

     ;; Record procedures complete like any documented callable: an accessor's
     ;; argument is typed, the named type meets the record type it denotes,
     ;; and an accessor returning a buffer is one of its producers.
     (check 'record-procedures-complete-by-their-signatures
       (list (has? "(region b start end)" (labels "(region-buffer ")) (has? "(region-buffer region)" (labels "(head:show-buffer! ")))
       '(#t #t))

     ;; Keys bind structure, not spelled names: a call with its producers and
     ;; a pre-filled M-x describe themselves by their procedures' names.
     (check 'structured-key-actions-describe-themselves
       (list (keymap:action-text (keymap:call kill-buffer! head:current-buffer))
             (keymap:action-text (keymap:prefill answer!))
             (keymap:prefill-text (keymap:prefill replace! "old")))
       '("(kill-buffer! (head:current-buffer))" "M-x (answer! " "(replace! \"old\" "))

     (test:finish! 'mx)))
