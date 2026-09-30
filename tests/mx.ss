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
     (import (except (head edit) init!) (head literal) (prefix (apps search) search:) (prefix (apps eval) eval:) (prefix (core extension) extension:) (prefix (service file) file:) (prefix (state actor) actor:) (prefix (head keymap) keymap:) (prefix (head head) head:) (prefix (head prompt) prompt:) (prefix (head completion) completion:)
             (prefix (head window) window:) (prefix (head widget) widget:) (prefix (head completion-state) completion-state:)
             (prefix (only (head edit) init!) edit:) (prefix (foundation text) text:)
             (prefix (state model) model:) (prefix (state view) view:) (prefix (head table) table:)
             (prefix (foundation string) string:) (prefix (test) test:) (prefix (service doc) doc:)
             (prefix (head mode) mode:) (prefix (modes scheme-mode) scheme-mode:)
             (prefix (foundation edoc) edoc:) (prefix (head paint) paint:) (prefix (state store) store:)
             (prefix (head namespace) namespace:) (prefix (service environment) environment:)
             (prefix (head control) control:) (prefix (head entry) entry:) (prefix (head layout) layout:))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
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

     ;; Inside a string the settle step judges the typed session: a value
     ;; that still completes on, a directory with entries, stays open and
     ;; unsettled; one at its dead end closes the literal and settles on; an
     ;; untyped string is left alone
     (for-each
       (lambda (case)
         (check (list 'settle-in-string (car case)) (settled (car case)) (cons (cadr case) (string-length (cadr case)))))
       '(("(visit-file! \"manual/" "(visit-file! \"manual/")
         ("(visit-file! \"manual/EVAL.md" "(visit-file! \"manual/EVAL.md\"")
         ("(display \"manual/EVAL.md" "(display \"manual/EVAL.md")))
     ;; an input that does not read is never settled
     (check 'an-unreadable-input-is-left-alone (settled "(head:current-window]") '("(head:current-window]" . 21))

     ;; The input reads as data once its open string and forms are closed,
     ;; or the ghost says why not
     (check 'inputs-that-close-have-no-complaint
       (map eval:input-diagnostic '("(head:current-window" "(visit-file! \"manual/" "(let ([x 1" "" "(" "'(a b" "(f #\\( " "(f \"a)\" ; c"))
       '(#f #f #f #f #f #f #f #f))
     (check 'inputs-that-cannot-close-say-why
       (map eval:input-diagnostic '("(f x))" "(f x]" "(f #\\foo)" "(f . )"))
       '("unexpected )" "] closes (" "invalid character name #\\foo" "expected one item after dot (.)"))
     (check 'closers-complete-the-input
       (map eval:input-closers '("(f (g \"x" "(let ([x 1" "(f x))" "done" "(f \"a\\\"b"))
       '("\"))" "]))" #f "" "\")"))
     (check 'a-trailing-comment-puts-the-closers-on-their-own-line (eval:input-closers "(f x ; c") "\n)")

     ;; Text after the cursor is left alone; blank text after it is kept.
     (check 'text-after-the-symbol-stops-the-settling
       (eval:settle-completion "(head:current-window 1)" 20) '("(head:current-window 1)" . 20))
     (check 'blank-tail-is-kept
       (eval:settle-completion "(head:current-window  " 20) '("(head:current-window)  " . 21))

     ;; At an argument position the type documented for it decides what Tab
     ;; offers: the type's values as expressions, the procedures producing
     ;; one, and the variables holding one; symbols complete elsewhere.
     (define (labels text) (eval:completion-candidates text (string-length text)))
     (model:register-kind! 'completion-fixture 1 string?)
     (define model-ref (model:create! head:ui-actor 'completion-fixture 1 'session 'transient '() "payload"))
     (define model-text (format "(model ~a)" (cadr model-ref)))
     (check 'model-literal-completes-live-references-and-numbers
       (list (equal? (model (cadr model-ref)) model-ref)
         (and (member model-text (labels "(model:snapshot ")) #t)
         (and (member model-text (labels "(table:move! ")) #t)
         (and (member (number->string (cadr model-ref)) (labels "(model ")) #t)
         (assoc model-ref (model:metadata)))
       (list #t #t #t #t (list model-ref 'completion-fixture)))
     (check 'model-spelling-is-type-driven-and-round-trips
       (let ([text (keymap:action-text (keymap:call model:snapshot model-ref))])
         (list text (equal? (eval (read (open-input-string text))) (model:snapshot model-ref))
           (keymap:prefill-text (keymap:prefill model:snapshot model-ref))
           (edoc:type-spelling 'list model-ref)))
       (list (format "(model:snapshot ~a)" model-text) #t (format "(model:snapshot ~a " model-text) (format "'~s" model-ref)))
     (model:retire! head:ui-actor model-ref 0)
     (check 'retired-models-leave-completion-but-can-be-named
       (list (member model-text (labels "(model:snapshot "))
         (model (cadr model-ref))
         (map (lambda (v) (test:raises? (lambda () (model v)))) '(0 -1 1.0 "1" (model 1))))
       (list #f model-ref '(#t #t #t #t #t)))
     ;; a mode is named, not spelled as a literal: an argument of type mode
     ;; completes to the registered names, and no producer sneaks in
     (scheme-mode:init!)
     (check 'a-mode-argument-completes-to-its-literals
       (list (and (member "(mode \"scheme\")" (labels "(mode:choose! ")) #t)
             (filter (lambda (label) (and (>= (string-length label) 5) (string=? (substring label 0 5) "(echo"))) (labels "(mode:choose! "))
             (format "~a" (mode:find "scheme")))
       '(#t () "#<mode scheme>"))


     ;; every completing type spells its values as a literal derived from the
     ;; type, and a command takes the bare value and the literal alike
     (check 'literals-derive-from-completing-types
       (list (and (memq 'mode (edoc:type-literals)) (memq 'file (edoc:type-literals)) (memq 'buffer (edoc:type-literals)) #t)
             (memq 'boolean (edoc:type-literals)) (memq 'region (edoc:type-literals))
             ((edoc:type-literal 'mode) "scheme")
             (test:raises? (lambda () ((edoc:type-literal 'mode) 42)))
             (eq? ((edoc:type-literal 'buffer) "*scratch*") (buffer "*scratch*"))
             (edoc:type-value 'mode "scheme")
             (eq? (edoc:type-value 'buffer "*scratch*") (buffer "*scratch*"))
             (eq? (edoc:type-value 'buffer (buffer "*scratch*")) (buffer "*scratch*"))
             (test:raises? (lambda () (edoc:type-value 'mode 42))))
       '(#t #f #f "scheme" #t #t "scheme" #t #t #t))

     ;; A producer returning (or integer #f) is no completion for a
     ;; (or mode #f) argument merely because both allow #f.
     (check 'a-union-member-false-serves-no-producer
       (list (eval:type-fits? '(or mode #f) '(or integer #f)) (eval:type-fits? 'buffer '(or buffer #f))
             (eval:type-fits? '(or mode #f) 'mode) (eval:type-fits? '(or integer #f) '(or integer #f)))
       '(#f #t #t #t))

     (define (has? needle candidates) (and candidates (exists (lambda (l) (string=? l needle)) candidates) #t))
     (define (has-prefix? needle candidates) (and candidates (exists (lambda (l) (string:prefix? needle l)) candidates) #t))
     (eval '(define myb (buffer "*scratch*")) (interaction-environment))
     (check 'a-buffer-argument-offers-buffers-producers-and-variables
       (let ([offered (labels "(head:show-buffer! ")])
         (list (has? "(buffer \"*scratch*\")" offered) (has? "(head:current-buffer)" offered)
               (has? "(head:fresh-buffer! name)" offered) (has? "(head:new-local-buffer! name)" offered) (has? "myb" offered)
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
     (let* ([expansions 0]
            [source (completion:make-source
                      (lambda (text caret)
                        (values 0 (string-length text)
                          (lambda () (set! expansions (+ expansions 1)) '("abc" "bca" "cba"))
                          (if (string=? text "z") '()
                            (map (lambda (name) (completion:make-candidate name name #f #f
                                                  (list '(type . choice) (cons 'value name)))) '("abc" "bca" "cba"))))))]
            [s (completion-state:create source #f)])
       (completion-state:refresh! s "a" 1)
       (completion-state:normalize! s source)
       (completion-state:normalize! s source)
       (let ([page (completion-state:snapshot s)])
         (check 'completion-normalizes-and-cycles-without-recomputing-extensions
           (list (cadr page) (map completion:candidate-value (list-ref page 3)) expansions) '("bca" ("abc" "bca" "cba") 1))
         (completion-state:refresh! s "z" 1)
         (check 'completion-refuses-stale-page-without-expansion-or-text-change
           (list (completion-state:choose! s (car page) "abc")
             (cadr (completion-state:snapshot s)) expansions (list-ref (completion-state:snapshot s) 7)) '(#f "z" 1 #f)))
       (completion-state:refresh! s "a" 1)
       (completion-state:normalize! s source) (completion-state:normalize! s source)
       (check 'completion-selects-current-value-and-hides-its-page
         (list (completion-state:choose! s (car (completion-state:snapshot s)) "cba")
           (cadr (completion-state:snapshot s)) (list-ref (completion-state:snapshot s) 3)
           (completion:candidate-context (list-ref (completion-state:snapshot s) 7)))
         '(#t "cba" #f ((type . choice) (value . "cba")))))
     (check 'a-nested-operator-completes-to-the-enclosing-arguments-type
       (let ([nested (labels "(head:show-buffer! (bu")])
         (list (has? "(buffer \"*scratch*\")" nested) (has? "(head:fresh-buffer! name)" nested) (has? "myb" nested)
               (labels "(head:show-buffer! (curr") (extensions "(head:show-buffer! (curr")
               (extensions "(head:show-buffer! bu") (extensions "(head:show-buffer! (bu")
               ;; a variable holding a buffer keeps the token bare
               (extensions "(head:show-buffer! my") (extensions "(head:show-buffer! ")
               (extensions "(head:buffer-wrap-set! b 'c") (extensions "(visit-file! \"man")
               ;; a quoted form, or one whose operator is undocumented, completes symbols
               (labels "(head:show-buffer! '(bu") (labels "(list (bu")))
       '(#t #t #f ("(head:current-buffer)") ("(head:current-buffer)") ("(buffer") ("(buffer") ("myb") ("")
         ("'clean") ("(file \"manual/") #f #f))
     ;; The file completion parameter: prefix offers the directory's entries
     ;; extending the component, fuzzy all of them for the matcher's
     ;; segments, deep the entries below it too
     (check 'file-completion-modes
       (list (parameterize ([file:completion 'prefix]) (labels "(visit-file! \"lib/apps/evsl"))
             (parameterize ([file:completion 'fuzzy]) (has? "(file \"lib/apps/eval.sls\")" (labels "(visit-file! \"lib/apps/evsl")))
             (parameterize ([file:completion 'fuzzy]) (labels "(visit-file! \"lib/evsl"))
             (parameterize ([file:completion 'deep]) (has? "(file \"lib/apps/eval.sls\")" (labels "(visit-file! \"lib/evsl")))
             ;; a directory opens its literal, to descend into; a file's own name is its dead end, closed
             (extensions "(visit-file! \"man") (extensions "(visit-file! \"manual/EVAL.m"))
       '(#f #t #f #t ("(file \"manual/") ("(file \"manual/EVAL.md\")")))
     ;; Inside a string at a path argument, Tab expands the string into the
     ;; type's literal and extends the path to the candidates' longest common
     ;; prefix, as a shell does: a listing sharing nothing stays put, a deep
     ;; path stays quick, and shared characters extend
     ;; the kernel publishes each type's literal after a module initializes;
     ;; the test stands in for it, so (file " completes inside the literal
     (for-each (lambda (name) (unless (top-level-bound? name) (define-top-level-value name (edoc:type-literal name) (interaction-environment))))
               (edoc:type-literals))
     (define scratch-dir (format "/tmp/e-mx-~a" (get-process-id)))
     (mkdir scratch-dir)
     (for-each (lambda (name) (call-with-output-file (string-append scratch-dir "/" name) (lambda (p) (put-string p "x"))))
               '("alpha-one.txt" "alpha-two.txt" "a b.txt" "quo\"te.txt"))
     (check 'string-extensions-are-common-prefixes
       (list (extensions "(visit-file! \"manual/") (extensions "(visit-file! \"lib/apps/")
             (extensions (string-append "(visit-file! \"" scratch-dir "/al")))
       (list '("(file \"manual/") '("(file \"lib/apps/") (list (string-append "(file \"" scratch-dir "/alpha-"))))
     ;; a name with a space completes like any other; a quote in a name is
     ;; escaped as the string literal holds it, inside the string and in the
     ;; literal alike, and a token typed with the escape reads the same way
     (check 'special-characters-in-a-name-are-escaped-in-the-string
       (let ([quoted (string-append scratch-dir "/quo\\\"te.txt")])
         (list (extensions (string-append "(visit-file! \"" scratch-dir "/a "))
               (extensions (string-append "(visit-file! (file \"" scratch-dir "/quo"))
               (extensions (string-append "(visit-file! (file \"" scratch-dir "/quo\\\""))
               (extensions (string-append "(visit-file! \"" scratch-dir "/quo"))
               (settled (string-append "(visit-file! \"" quoted "\""))
               (read (open-input-string (string-append "(visit-file! \"" quoted "\")")))))
       (let ([quoted (string-append scratch-dir "/quo\\\"te.txt")] [closed (string-append "(visit-file! \"" scratch-dir "/quo\\\"te.txt\"")])
         (list (list (string-append "(file \"" scratch-dir "/a b.txt\")"))
               (list quoted) (list quoted)
               (list (string-append "(file \"" quoted "\")"))
               (cons closed (string-length closed))
               (list 'visit-file! (string-append scratch-dir "/quo\"te.txt")))))
     (for-each (lambda (name) (delete-file (string-append scratch-dir "/" name))) '("alpha-one.txt" "alpha-two.txt" "a b.txt" "quo\"te.txt"))
     (delete-directory scratch-dir)
     ;; A roots argument, one directory or a list of them, completes as a
     ;; directory inside the string and inside each element of a quoted list;
     ;; a quoted list elsewhere still completes symbols
     (check 'a-list-of-argument-completes-its-elements
       (list (has-prefix? "(directory \"manual/" (labels "(extension:load! \"x\" \"y\" \"man"))
             (has-prefix? "manual/" (labels "(extension:load! \"x\" \"y\" '(\"man"))
             (has-prefix? "manual/" (labels "(extension:load! \"x\" \"y\" '(\"lib\" \"man"))
             (labels "(head:show-buffer! '(bu"))
       '(#t #t #t #f))
     (check 'literals-and-strings-complete-in-place
       (list (has? "'clean" (labels "(head:buffer-wrap-set! b ")) (has? "#f" (labels "(head:buffer-wrap-set! b "))
             ;; the language's types offer their own values but no producers
             (length (labels "(head:buffer-wrap-set! b ")) (labels "(window:set-wrap! ")
             (has-prefix? "(file \"manual/" (labels "(visit-file! \"man"))
             (has? "*scratch*" (labels "(buffer \""))
             ;; an undocumented operator falls back to symbols
             (labels "(car "))
       '(#t #t 4 ("#t" "#f" "'default") #t #t #f))
     ;; a scope form's argument completes by type, syntax or not
     (check 'a-scope-form-completes-its-argument-by-type
       (list (has-prefix? "(buffer \"" (labels "(head:with-buffer (bu")) (has? "(head:current-buffer)" (labels "(head:with-buffer (bu"))
             (has? "(current-region)" (labels "(with-region (re")) (has-prefix? "(window " (labels "(head:with-window (wi")))
       '(#t #t #t #t))
     (check 'a-completed-value-settles-its-form
       (list (settled "(head:show-buffer! (buffer \"*scratch*\")") (settled "(visit-file! \"manual/EVAL.md\""))
       '(("(head:show-buffer! (buffer \"*scratch*\"))" . 40) ("(visit-file! \"manual/EVAL.md\"" . 29)))


     ;; A string at an argument whose type spells its values as literals
     ;; expands into the literal from its quote, Tab replacing the whole
     ;; string; a bare token does the same; inside the constructor the values
     ;; spell bare.
     (define (span text) (eval:completion-span text (string-length text)))
     (check 'a-string-or-token-expands-into-its-literal
       (list (extensions "(mode:choose! \"sch") (span "(mode:choose! \"sch") (extensions "(mode:choose! sch") (span "(mode:choose! sch")
             (labels "(mode:choose! (mode \"sc") (extensions "(mode:choose! (mode \"sc") (extensions "(mode:choose! (mode sc")
             (settled "(mode:choose! (mode \"scheme\")")
             ;; a bare token the values alone match opens their literal
             (has? "(buffer \"*scratch*\")" (labels "(head:show-buffer! *")))
       '(("(mode \"scheme\")") (14 . 18) ("(mode \"scheme\")") (14 . 17) ("scheme") ("scheme") ("\"scheme\"")
         ("(mode:choose! (mode \"scheme\")" . 29) #t))
     ;; Tab at a final datum, a closed string or form, settles: each enclosing
     ;; form with a fixed arity closes once its arguments are there, the
     ;; cursor steps to a due argument past a separator already typed, and a
     ;; closed string is never completed further, existing or not
     (check 'a-final-datum-settles-the-forms-around-it
       (list (settled "(save-file! (file \"~/ddd\"") (settled "(save-file! (file \"~/ddd") (settled "(save-file! \"~/ddd\"")
             (settled "(head:show-buffer! (buffer \"*scratch*\")") (settled "(window:split-right! ")
             (settled "(head:set-window-buffer! (window 1)") (settled "(head:set-window-buffer! (window 1) ")
             (labels "(extension:load! \"x\" \"y\"") (labels "(visit-file! \"manual/\""))
       '(("(save-file! (file \"~/ddd\"))" . 27) ("(save-file! (file \"~/ddd\"))" . 27) ("(save-file! \"~/ddd\")" . 20)
         ("(head:show-buffer! (buffer \"*scratch*\"))" . 40) ("(window:split-right!)" . 21)
         ("(head:set-window-buffer! (window 1) " . 36) ("(head:set-window-buffer! (window 1) " . 36) #f #f))

     ;; ~ and / lead the home and the root directory, though the matcher has
     ;; no segment for them: at a file or directory argument they open the
     ;; literal, bare or in a string, and complete bare inside the constructor
     (check 'home-and-root-open-a-path-literal
       (list (extensions "(visit-file! ~") (extensions "(visit-file! \"~") (extensions "(visit-file! (file ~") (extensions "(visit-file! (file \"~")
             (extensions "(visit-file! /") (extensions "(visit-file! \"/") (extensions "(visit-file! (file /")
             (extensions "(extension:load! \"x\" \"y\" ~") (extensions "(extension:load! \"x\" \"y\" /")
             (span "(visit-file! ~") (span "(visit-file! \"~"))
       '(("(file \"~/") ("(file \"~/") ("\"~/") ("~/") ("(file \"/") ("(file \"/") ("\"/") ("(directory \"~/") ("(directory \"/") (13 . 14) (13 . 15)))
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
       '(#t #t #t #t ("(agent") #f ("helper") ("helper") #f))
     ;; the sole name inserts bare; the settle step closes its literal at the
     ;; dead end and stops there, since the constructor takes a rest argument
     (check 'a-completed-name-closes-its-literal-and-stops-at-a-rest-parameter
       (settled "(actor:send! (agent \"helper")
       (let ([out "(actor:send! (agent \"helper\""]) (cons out (string-length out))))

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
             (keymap:prefill-text (keymap:prefill search:replace! "old")))
       '("(kill-buffer! (head:current-buffer))" "λ (answer! " "(search:replace! \"old\" "))

     ;; a procedure without an edoc shows its described parameters, the
     ;; corpus's or a module's, in its completion hint, before its arity
     (define-top-level-value 'mx-plain-proc (lambda (a b . c) #f))
     (doc:register! '(((mx-plain-proc) (("procedure" . "(mx-plain-proc alpha beta ...)")) "void" ("(mx)") mx "Fixture" #f "A fixture.")))
     (check 'a-described-procedure-completes-with-its-parameter-names (eval:completion-hint 'mx-plain-proc) "(alpha beta ...)")
     (check 'an-undescribed-procedure-completes-with-its-source-parameters
       (begin (define-top-level-value 'mx-bare-proc (lambda (a b . c) #f)) (eval:completion-hint 'mx-bare-proc)) "(a b . c)")
     ;; a hint cached against one version of the documentation is forgotten
     ;; when the documentation changes, a fetch or a registration later
     (doc:register! '(((mx-bare-proc) (("procedure" . "(mx-bare-proc gamma)")) "void" ("(mx)") mx "Fixture" #f "Documented later.")))
     (check 'a-cached-hint-follows-newly-arrived-documentation (eval:completion-hint 'mx-bare-proc) "(gamma)")

     ;; Context is finite declared structure. A sibling only participates
     ;; when its parent exposes it; ambiguity never chooses by numeric ID.
     (eval:init!)
     (eval '(edoc:elibrary (receiver-probe)
              (export change! optional!) (import (chezscheme))
              (edoc "A receiver command." (id model) (receiver id (view receiver-leaf)))
              (define (change! id) id)
              (edoc "An optional receiver." (id model) (receiver id (view receiver-leaf)))
              (define optional! (case-lambda [() #f] [(id) id]))))
     (eval '(import (prefix (receiver-probe) receiver-probe:)))
     (let* ([a (view:create! head:ui-actor #f 'receiver-leaf 1 '((name . "First")) '())]
            [b (view:create! head:ui-actor #f 'receiver-leaf 1 '((name . "Second")) '())]
            [hidden (view:create! head:ui-actor #f 'receiver-leaf 1 '() '())]
            [root (view:create! head:ui-actor #f 'receiver-parent 1 '() '())])
       (widget:register! 'receiver-leaf 1 (list (cons 'actions (list (cons 'change (eval 'receiver-probe:change!))))))
       (widget:register! 'receiver-parent 1 '((receivers (Other second))))
       (view:arrange! head:ui-actor
         (list (list root 0 (list (list 'first a 'fit) (list 'second b 'fit) (list 'hidden hidden 'fit)) '())) '())
       (widget:mount! root 'receiver-test)
       (let* ([captured (widget:receivers a)] [single (filter (lambda (r) (equal? (car r) a)) captured)]
              [factory (completion:provider '(scheme 1 ()))]
              [source (factory '() (list (cons 'receivers captured)))]
              [one (factory '() (list (cons 'receivers single)))]
              [none (factory '() '())])
         (define (lookup source text)
           (let-values ([(from to extensions candidates) ((completion:source-lookup source) text (string-length text))])
             (list from to (if (procedure? extensions) (extensions) extensions)
               (map completion:candidate-value candidates))))
         (check 'receiver-capture-is-bounded-and-distinct
           (list (map car captured) (map edoc:signature-receiver (edoc:edoc-of (eval 'receiver-probe:optional!))))
           (list (list a root b) '(#f (id (view receiver-leaf)))))
         (check 'receiver-registration-checks-the-declared-contract
           (test:raises? (lambda () (widget:register! 'wrong-receiver 1
                                      (list (cons 'actions (list (cons 'change (eval 'receiver-probe:change!)))))))) #t)
         (let* ([text "(receiver-probe:change! "] [lit (format "(model ~a)" (cadr a))])
           (check 'receiver-arguments-insert-only-a-sole-explicit-literal
             (list (caddr (lookup one text)) (caddr (lookup source text))
               (list-ref (lookup source text) 3))
             (list (list lit) '("") (list lit (format "(model ~a)" (cadr b))))))
         (check 'contextual-symbols-and-nested-calls-use-the-same-origin
           (map (lambda (src) (list-ref (lookup src "(list (receiver-probe:ch") 3)) (list one none))
           '(("receiver-probe:change!") ()))
         (check 'existing-receiver-expressions-do-not-get-replaced
           (map (lambda (text) (let ([r (lookup one text)])
                                 (and (= (car r) (string-length text))
                                   (not (string=? ((completion:source-kind one) text (string-length text)) "receiver")))))
             '("(receiver-probe:change! existing " "(receiver-probe:change! (list " "'(receiver-probe:change! "))
           '(#t #t #t))
         (widget:unmount! root)
         (check 'captured-receiver-retirement-removes-it-from-discovery
           (list (widget:receiver-live? (car single))
             (list-ref (lookup one "(receiver-probe:ch") 3)) '(#f ()))))

     (include "tests/environment-widget.sps")
     (test:finish! 'mx)))
