#!/usr/bin/env scheme-script

;; M-x settles a sole completion: forms close with their matching bracket
;; and the cursor steps to the next argument while every enclosing operator
;; has a fixed arity; an unknown arity, a quoted form or text after the
;; cursor leaves the cursor at the symbol. Headless, against the live
;; environment's own procedures. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (except (head edit) init!) (head literal) (prefix (apps search) search:) (prefix (apps eval) eval:) (prefix (core extension) extension:) (prefix (service file) file:) (prefix (state actor) actor:) (prefix (head keymap) keymap:) (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head prompt) prompt:) (prefix (head completion) completion:)
             (prefix (head window-host) window-host:) (prefix (head widget) widget:) (prefix (head completion-state) completion-state:)
             (prefix (only (head edit) init!) edit:) (prefix (foundation text) text:)
             (prefix (state model) model:) (prefix (state view) view:) (prefix (head table) table:)
             (prefix (foundation string) string:) (prefix (test) test:) (prefix (service doc) doc:)
             (prefix (head mode) mode:) (prefix (modes scheme-mode) scheme-mode:)
             (prefix (foundation edoc) edoc:) (prefix (head paint) paint:) (prefix (state store) store:)
             (prefix (head namespace) namespace:) (prefix (service environment) environment:)
             (prefix (apps history-view) history-view:) (prefix (service history) history:)
             (prefix (state collection) collection:) (prefix (state connection) connection:)
             (prefix (head range) range:) (prefix (sys glyph) glyph:)
             (prefix (apps delta-log) delta-log:)
             (prefix (core region) region:) (prefix (core kernel) kernel:)
             (prefix (head control) control:) (prefix (head entry) entry:) (prefix (head layout) layout:))

     (define check test:check)
     (widget:init!) (edit:init!) (window-host:init!)
     (kernel:load-module! "region")
     (kernel:load-module! "literal")
     (check 'loading-modules-does-not-publish-value-constructors
       (filter top-level-bound? '(file directory mode revision batch conflict head agent base model buffer)) '())
     (define (settled text) (eval:settle-completion text (string-length text)))

     (for-each
       (lambda (case)
         (check (list 'settle (car case)) (settled (car case)) (cons (cadr case) (string-length (cadr case)))))
       '(;; a nullary operator closes its form; one taking arguments steps to the first
         ("(window-host:split-right!" "(window-host:split-right!)")
         ("(seat:window-index" "(seat:window-index ")
         ;; the last argument closes the form, an earlier one steps on
         ("(seat:window-index (seat:current-window" "(seat:window-index (seat:current-window))")
         ("(text:make-span 1 2" "(text:make-span 1 2 ")
         ;; closing a form settles it as an argument of its parent, recursively
         ("(seat:window-numbered (seat:window-index (seat:current-window" "(seat:window-numbered (seat:window-index (seat:current-window)))")
         ;; brackets close with their own kind; a closed form settles as an
         ;; argument of its parent, or stops at an operator without an arity
         ("(vector-ref {seat:current-window" "(vector-ref {seat:current-window} ")
         ("(let ([x (seat:current-window" "(let ([x (seat:current-window)")
         ;; rest and optional parameters, syntax, and unbound names are unknown arities
         ("(list foo" "(list foo")
         ("(define foo" "(define foo")
         ("(no-such-procedure-here" "(no-such-procedure-here")
         ;; a quoted or quasiquoted form is data
         ("'(window-host:split-right!" "'(window-host:split-right!")
         ("`(seat:current-window" "`(seat:current-window")
         ("(list '(seat:current-window" "(list '(seat:current-window")
         ;; too many arguments already: nothing to close
         ("(seat:current-window x" "(seat:current-window x")
         ;; a bare symbol has no form
         ("seat:current-window" "seat:current-window")))

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
     (check 'an-unreadable-input-is-left-alone (settled "(seat:current-window]") '("(seat:current-window]" . 21))

     ;; The input reads as data once its open string and forms are closed,
     ;; or the ghost says why not
     (check 'inputs-that-close-have-no-complaint
       (map eval:input-diagnostic '("(seat:current-window" "(visit-file! \"manual/" "(let ([x 1" "" "(" "'(a b" "(f #\\( " "(f \"a)\" ; c"))
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
       (eval:settle-completion "(seat:current-window 1)" 20) '("(seat:current-window 1)" . 20))
     (check 'blank-tail-is-kept
       (eval:settle-completion "(seat:current-window  " 20) '("(seat:current-window)  " . 21))

     ;; At an argument position the type documented for it decides what Tab
     ;; offers: the type's values as expressions, the procedures producing
     ;; one, and the variables holding one; symbols complete elsewhere.
     (define (labels text) (eval:completion-candidates text (string-length text)))
     (model:register-kind! 'completion-fixture 1 string?)
     (define model-ref (model:create! head:ui-actor 'completion-fixture 1 'session 'transient '() "payload"))
     (define model-text (format "'~s" model-ref))
     (check 'model-completion-offers-live-references
       (list (and (member model-text (labels "(model:snapshot ")) #t)
         (and (member model-text (labels "(table:move! ")) #t)
         (assoc model-ref (model:metadata)))
       (list #t #t (list model-ref 'completion-fixture)))
     (check 'model-spelling-round-trips-with-or-without-types
       (let ([text (keymap:action-text (keymap:call model:snapshot model-ref))])
         (list text (equal? (eval (read (open-input-string text))) (model:snapshot model-ref))
           (keymap:prefill-text (keymap:prefill model:snapshot model-ref))
           (edoc:type-spelling 'list model-ref)))
       (list (format "(model:snapshot ~a)" model-text) #t (format "(model:snapshot ~a " model-text) (format "'~s" model-ref)))
     (model:retire! head:ui-actor model-ref 0)
     (check 'retired-models-leave-completion-but-can-be-named
       (list (member model-text (labels "(model:snapshot "))
         (eval (read (open-input-string model-text)))
         (map (lambda (v) (not (edoc:type-accepts? 'model v))) '(0 -1 1.0 "1" (model 0) (model 1.0))))
       (list #f model-ref '(#t #t #t #t #t #t)))
     ;; A mode argument completes to its string name;
     ;; completes to the registered names, and no producer sneaks in
     (scheme-mode:init!)
     (check 'a-mode-argument-completes-to-its-name
       (list (and (member "\"scheme\"" (labels "(mode:choose! ")) #t)
             (filter (lambda (label) (and (>= (string-length label) 5) (string=? (substring label 0 5) "(echo"))) (labels "(mode:choose! "))
             (format "~a" (mode:find "scheme")))
       '(#t () "#<mode scheme>"))


     (check 'mode-selection-validates-values-without-a-wrapper
       (list (begin (mode:choose! "scheme") (mode:name-of))
         (test:raises? (lambda () (mode:choose! 42)))
         (test:raises? (lambda () (mode:choose! ""))))
       '("scheme" #t #t))

     ;; A producer returning (or integer #f) is no completion for a
     ;; (or mode #f) argument merely because both allow #f.
     (check 'a-union-member-false-serves-no-producer
       (list (eval:type-fits? '(or mode #f) '(or integer #f)) (eval:type-fits? 'buffer '(or buffer #f))
             (eval:type-fits? '(or mode #f) 'mode) (eval:type-fits? '(or integer #f) '(or integer #f)))
       '(#f #t #t #t))

     (define (has? needle candidates) (and candidates (exists (lambda (l) (string=? l needle)) candidates) #t))
     (define (has-prefix? needle candidates) (and candidates (exists (lambda (l) (string:prefix? needle l)) candidates) #t))
     (eval '(define myb (store:find-named "*scratch*")) (interaction-environment))
     (define scratch-text (format "'~s" myb))
     (check 'buffer-completion-separates-labels-from-portable-values
       (let ([offered (labels "(seat:show-buffer! ")])
         (list (has? "*scratch*" offered) (has? "(seat:current-buffer)" offered)
           (has? "(new-buffer! name)" offered) (has? "myb" offered)
           (has? "(seat:new-buffer! name)" offered) (has? "(seat:current-buffer-mirror)" offered)
           (has? "*scratch*" (labels "(seat:show-buffer! scr")) (labels "(seat:show-buffer! my")))
       '(#t #t #t #t #f #f #t ("myb")))
     ;; The operator position of a nested form takes the enclosing argument's
     ;; type: (bu offers what bu offers less the bare variables, and Tab
     ;; extends a token to the longest text every candidate still matches,
     ;; a sole candidate whole.
     (define (extensions text) (eval:completion-extensions text (string-length text)))
     (check 'named-reference-normalization-preserves-the-match-set
       (let* ([a (store:create! head:ui-actor "(completion-label-a)" '(""))]
              [b (store:create! head:ui-actor "(completion-label-b)" '(""))]
              [prefix "(seat:show-buffer! "] [input (string-append prefix "completionlabel")]
              [before (list-sort string<? (labels input))]
              [same? (and (= (length before) 2)
                       (for-all (lambda (text)
                                  (equal? before (list-sort string<? (labels (string-append prefix text)))))
                         (extensions input)))])
         (store:delete! head:ui-actor a) (store:delete! head:ui-actor b) same?) #t)
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
     (check 'buffer-completion-retains-free-expressions-and-canonical-data
       (list (has? scratch-text (labels "(seat:show-buffer! '(buffer"))
         (extensions "(seat:show-buffer! *scratch*")
         (extensions "(seat:show-buffer! my")
         (extensions "(seat:show-buffer! (curr")
         (labels "(list (bu")
         (edoc:type-spelling 'buffer myb) (edoc:type-spelling 'list myb))
       (list #t (list scratch-text) '("myb") '("(seat:current-buffer)") #f scratch-text scratch-text))
     ;; The file completion parameter: prefix offers the directory's entries
     ;; extending the component, fuzzy all of them for the matcher's
     ;; segments, deep the entries below it too
     (check 'file-completion-modes
       (list (parameterize ([file:completion 'prefix]) (labels "(visit-file! \"lib/apps/evsl"))
             (parameterize ([file:completion 'fuzzy]) (has? "lib/apps/eval.sls" (labels "(visit-file! \"lib/apps/evsl")))
             (parameterize ([file:completion 'fuzzy]) (labels "(visit-file! \"lib/evsl"))
             (parameterize ([file:completion 'deep]) (has? "lib/apps/eval.sls" (labels "(visit-file! \"lib/evsl")))
             ;; a directory opens its literal, to descend into; a file's own name is its dead end, closed
             (extensions "(visit-file! \"man") (extensions "(visit-file! \"manual/EVAL.m"))
       '(#f #t #f #t ("manual/") ("manual/EVAL.md")))
     ;; Strings extend by common path prefix, including spaces and escaped quotes.
     (define scratch-dir (format "/tmp/e-mx-~a" (get-process-id)))
     (mkdir scratch-dir)
     (for-each (lambda (name) (call-with-output-file (string-append scratch-dir "/" name) (lambda (p) (put-string p "x"))))
               '("alpha-one.txt" "alpha-two.txt" "a b.txt" "quo\"te.txt"))
     (check 'string-extensions-are-common-prefixes
       (list (extensions "(visit-file! \"manual/") (extensions "(visit-file! \"lib/apps/")
             (extensions (string-append "(visit-file! \"" scratch-dir "/al")))
       (list '("manual/") '("lib/apps/") (list (string-append scratch-dir "/alpha-"))))
     ;; a name with a space completes like any other; a quote in a name is
     ;; escaped as the string holds it, and a token typed with the escape
     ;; reads the same way
     (check 'special-characters-in-a-name-are-escaped-in-the-string
       (let ([quoted (string-append scratch-dir "/quo\\\"te.txt")])
         (list (extensions (string-append "(visit-file! \"" scratch-dir "/a "))
               (extensions (string-append "(visit-file! \"" scratch-dir "/quo\\\""))
               (extensions (string-append "(visit-file! \"" scratch-dir "/quo"))
               (settled (string-append "(visit-file! \"" quoted "\""))
               (read (open-input-string (string-append "(visit-file! \"" quoted "\")")))))
       (let ([quoted (string-append scratch-dir "/quo\\\"te.txt")] [closed (string-append "(visit-file! \"" scratch-dir "/quo\\\"te.txt\"")])
         (list (list (string-append scratch-dir "/a b.txt"))
               (list quoted) (list quoted)
               (cons closed (string-length closed))
               (list 'visit-file! (string-append scratch-dir "/quo\"te.txt")))))
     (for-each (lambda (name) (delete-file (string-append scratch-dir "/" name))) '("alpha-one.txt" "alpha-two.txt" "a b.txt" "quo\"te.txt"))
     (delete-directory scratch-dir)
     ;; A roots argument completes as a directory string, including inside
     ;; each element of a quoted list;
     ;; a quoted list elsewhere still completes symbols
     (check 'a-list-of-argument-completes-its-elements
       (list (has-prefix? "manual/" (labels "(extension:load! \"x\" \"y\" \"man"))
             (has-prefix? "manual/" (labels "(extension:load! \"x\" \"y\" '(\"man"))
             (has-prefix? "manual/" (labels "(extension:load! \"x\" \"y\" '(\"lib\" \"man"))
             (has? scratch-text (labels "(seat:show-buffer! '(bu")))
       '(#t #t #t #t))
     (check 'literals-and-strings-complete-in-place
       (list (has? "'clean" (labels "(seat:buffer-wrap-set! b ")) (has? "#f" (labels "(seat:buffer-wrap-set! b "))
             ;; the language's types offer their own values but no producers
             (length (labels "(seat:buffer-wrap-set! b ")) (labels "(window-host:set-wrap! ")
             (has-prefix? "manual/" (labels "(visit-file! \"man"))
             (has? "*scratch*" (labels "(seat:show-buffer! *scr"))
             ;; an undocumented operator falls back to symbols
             (labels "(car "))
       '(#t #t 4 ("#t" "#f" "'default") #t #t #f))
     ;; a scope form's argument completes by type, syntax or not
     (check 'a-scope-form-completes-its-argument-by-type
       (list (has? scratch-text (labels "(seat:with-buffer '(bu")) (has? "(seat:current-buffer)" (labels "(seat:with-buffer (curr"))
             (has? "(current-region)" (labels "(with-region (re")) (has-prefix? "(window " (labels "(seat:with-window (wi")))
       '(#t #t #t #t))
     (check 'a-completed-value-settles-its-form
       (let* ([s (string-append "(seat:show-buffer! " scratch-text)] [out (string-append s ")")])
         (equal? (settled s) (cons out (string-length out)))) #t)


     ;; String arguments complete their contents, while a bare token inserts
     ;; the quoted string. No constructor is introduced in either case.
     (define (span text) (eval:completion-span text (string-length text)))
     (check 'a-string-or-token-completes-to-the-same-value
       (list (extensions "(mode:choose! \"sch") (span "(mode:choose! \"sch") (extensions "(mode:choose! sch") (span "(mode:choose! sch")
             (settled "(mode:choose! \"scheme")
             ;; a bare token the values alone match opens their literal
             (has? "*scratch*" (labels "(seat:show-buffer! *")))
       '(("scheme") (15 . 18) ("\"scheme\"") (14 . 17)
         ("(mode:choose! \"scheme\"" . 22) #t))
     ;; Tab at a final datum, a closed string or form, settles: each enclosing
     ;; form with a fixed arity closes once its arguments are there, the
     ;; cursor steps to a due argument past a separator already typed, and a
     ;; closed string is never completed further, existing or not
     (check 'a-final-datum-settles-the-forms-around-it
       (list (settled "(save-file! \"~/ddd\"") (settled "(save-file! \"~/ddd")
             (settled "(seat:show-buffer! '(buffer 1)") (settled "(window-host:split-right! ")
             (settled "(seat:set-window-buffer! (window 1)") (settled "(seat:set-window-buffer! (window 1) ")
             (labels "(extension:load! \"x\" \"y\"") (labels "(visit-file! \"manual/\""))
       '(("(save-file! \"~/ddd\")" . 20) ("(save-file! \"~/ddd\")" . 20)
         ("(seat:show-buffer! '(buffer 1))" . 31) ("(window-host:split-right!)" . 26)
         ("(seat:set-window-buffer! (window 1) " . 36) ("(seat:set-window-buffer! (window 1) " . 36) #f #f))

     ;; ~ and / lead the home and the root directory, though the matcher has
     ;; no segment for them: at a file or directory argument they open the
     ;; path string, bare or already inside a string
     (check 'home-and-root-open-a-path-string
       (list (extensions "(visit-file! ~") (extensions "(visit-file! \"~")
             (extensions "(visit-file! /") (extensions "(visit-file! \"/")
             (extensions "(extension:load! \"x\" \"y\" ~") (extensions "(extension:load! \"x\" \"y\" /")
             (span "(visit-file! ~") (span "(visit-file! \"~"))
       '(("\"~/") ("~/") ("\"/") ("/") ("\"~/") ("\"/") (13 . 14) (14 . 15)))
     (actor:register! '(agent "helper") (lambda (m) (void)))
     (check 'an-actor-argument-offers-the-directory
       (list (has? "'(agent \"helper\")" (labels "(actor:send! ")) (has? "'(agent \"helper\")" (labels "(actor:describe "))
             (has? "(actor:current)" (labels "(actor:send! "))
             (extensions "(actor:send! helper"))
       '(#t #t #t ("'(agent \"helper\")")))

     (eval '(edoc:elibrary (completion-value-probe)
              (export one many nested)
              (import (chezscheme))
              (edoc-type reference-choice "fixture references" (predicate (lambda (v) (and (member v '((model 701) (model 702))) #t))) (portable #t) (within list)
                (complete (lambda (partial) '(((model 701) #f #f) ((model 702) #f #f)))))
              (edoc "Choose a reference." (value reference-choice))
              (define (one value) value)
              (edoc "Choose references." (values (list-of reference-choice)))
              (define (many values) values)
              (edoc "Choose nested references." (values (list-of (list-of reference-choice))))
              (define (nested values) values)))
     (eval '(import (prefix (completion-value-probe) value-probe:)))
     (check 'completion-follows-quote-and-quasiquote-context
       (map labels
         '("(value-probe:one mo" "(value-probe:one '(mo" "(value-probe:one (quote (mo"
           "(value-probe:many '(mo" "(value-probe:many (quote (mo" "(value-probe:many `(mo"
           "(value-probe:nested '((mo" "(value-probe:many `(,(value-probe:one mo"))
       '(("'(model 701)" "'(model 702)") ("'(model 701)" "'(model 702)") ("'(model 701)" "'(model 702)")
         ("(model 701)" "(model 702)") ("(model 701)" "(model 702)") ("(model 701)" "(model 702)")
         ("(model 701)" "(model 702)") ("'(model 701)" "'(model 702)")))
     (check 'unquote-and-comments-preserve-the-inner-application-context
       (map labels
         '("(value-probe:many (quasiquote ((unquote (value-probe:one mo"
           "(value-probe:one `(,(value-probe:one '(mo"
           "; \" ignored\n(value-probe:one mo"
           "(value-probe:one #; (ignored \"datum\") mo"))
       '(("'(model 701)" "'(model 702)") ("'(model 701)" "'(model 702)")
         ("'(model 701)" "'(model 702)") ("'(model 701)" "'(model 702)")))
     (actor:register! '(agent helper) (lambda (m) (void)))
     (check 'normalizing-a-reference-preserves-its-candidate-set
       (for-all
         (lambda (text)
           (let* ([before (labels text)] [span (eval:completion-span text (string-length text))])
             (for-all (lambda (insert)
                        (equal? (list-sort string<? before)
                          (list-sort string<? (labels (string-append (substring text 0 (car span)) insert)))))
               (extensions text))))
         '("(value-probe:one mo" "(value-probe:one '(mo" "(value-probe:one (quote (mo"
           "(value-probe:many '(mo" "(value-probe:nested '((mo" "(actor:send! age" "(actor:send! he")) #t)
     (check 'completed-reference-collections-evaluate-to-the-offered-values
       (map (lambda (text)
              (let* ([span (eval:completion-span text (string-length text))]
                     [out (string-append (substring text 0 (car span)) (car (extensions text)))])
                (eval (read (open-input-string (string-append out (eval:input-closers out)))))))
         '("(value-probe:one 701" "(value-probe:many '(701" "(value-probe:nested '((701"))
       '((model 701) ((model 701)) (((model 701)))))
     (check 'completion-replaces-existing-value-closers-without-touching-its-neighbors
       (map (lambda (parts)
              (let* ([text (string-append (car parts) (cadr parts))] [pos (string-length (car parts))]
                     [span (eval:completion-span text pos)] [insert (car (eval:completion-extensions text pos))])
                (eval (read (open-input-string
                              (string-append (substring text 0 (car span)) insert (string:tail text (cdr span))))))))
         '(("(value-probe:one '(model 701" "))")
           ("(value-probe:one (quote (model 701" ")))")
           ("(value-probe:many '((model 701" ") (model 702)))")))
       '((model 701) (model 701) ((model 701) (model 702))))
     (actor:register! '(app describe) (lambda (m) (void)))
     (check 'actor-completion-preserves-identity-and-never-invents-a-constructor
       (list (has? "'(agent helper)" (labels "(actor:send! helper"))
         (has? "'(agent \"helper\")" (labels "(actor:send! helper"))
         (extensions "(actor:send! descr"))
       '(#t #t ("'(app describe)")))

     (check 'region-producers-complete-by-their-portable-types
       (list (has? "(region:make buffer start end)" (labels "(region:buffer "))
         (has? "(region:buffer r)" (labels "(store:buffer-name ")))
       '(#t #t))
     (check 'regions-and-positions-use-ordinary-scheme-expressions
       (let* ([r '(region (buffer 999999) (0 . 2) (1 . 3))]
              [text (edoc:type-spelling 'region r)]
              [call (keymap:action-text (keymap:call region:buffer r))])
         (list text call (eval (read (open-input-string text)))
           (eval (read (open-input-string call)))
           (eval (read (open-input-string (edoc:value-expression (list r '(0 . 2))))))))
       '("'(region (buffer 999999) (0 . 2) (1 . 3))"
         "(region:buffer '(region (buffer 999999) (0 . 2) (1 . 3)))"
         (region (buffer 999999) (0 . 2) (1 . 3)) (buffer 999999)
         ((region (buffer 999999) (0 . 2) (1 . 3)) (0 . 2))))

     ;; Keys bind structure, not spelled names: a call with its producers and
     ;; a pre-filled M-x describe themselves by their procedures' names.
     (check 'structured-key-actions-describe-themselves
       (list (keymap:action-text (keymap:call kill-buffer! seat:current-buffer-mirror))
             (keymap:action-text (keymap:prefill answer!))
             (keymap:prefill-text (keymap:prefill search:replace! "old")))
       '("(kill-buffer! (seat:current-buffer-mirror))" "λ (answer! " "(search:replace! \"old\" "))

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
         (check 'numeric-preview-context-never-evaluates-strings-or-expressions
           (map (lambda (input)
                  (cond [(assq 'value ((completion:source-context none) input (string-length input))) => cdr] [else #f]))
             '("(delta-log:show! 2" "(delta-log:show! '2" "(delta-log:show! \"2"
               "(delta-log:show! saved-revision" "(delta-log:show! (+ 1"))
           '(2 2 #f #f #f))
         (check 'receiver-capture-is-bounded-and-distinct
           (list (map car captured) (map edoc:signature-receiver (edoc:edoc-of (eval 'receiver-probe:optional!))))
           (list (list a root b) '(#f (id (view receiver-leaf)))))
         (check 'receiver-registration-checks-the-declared-contract
           (test:raises? (lambda () (widget:register! 'wrong-receiver 1
                                      (list (cons 'actions (list (cons 'change (eval 'receiver-probe:change!)))))))) #t)
         (let* ([text "(receiver-probe:change! "] [lit (format "'~s" a)])
           (check 'receiver-arguments-insert-only-a-sole-explicit-literal
             (list (caddr (lookup one text)) (caddr (lookup source text))
               (list-ref (lookup source text) 3))
             (list (list lit) '("") (list lit (format "'~s" b)))))
         (check 'contextual-symbols-and-nested-calls-use-the-same-origin
           (map (lambda (src) (list-ref (lookup src "(list (receiver-probe:ch") 3)) (list one none))
           '(("receiver-probe:change!") ()))
         (check 'existing-receiver-expressions-do-not-get-replaced
           (map (lambda (text) (let ([r (lookup one text)])
                                 (and (or (not (car r)) (= (car r) (string-length text)))
                                   (not (string=? ((completion:source-kind one) text (string-length text)) "receiver")))))
             '("(receiver-probe:change! existing " "(receiver-probe:change! (list " "'(receiver-probe:change! "))
           '(#t #t #t))
         (widget:unmount! root)
         (check 'captured-receiver-retirement-removes-it-from-discovery
           (list (widget:receiver-live? (car single))
             (list-ref (lookup one "(receiver-probe:ch") 3)) '(#f ()))))

     (let* ([document (store:create! head:ui-actor "completion origin" '("source"))]
            [editor (create-view! head:ui-actor document '())]
            [factory (completion:provider '(scheme 1 ()))])
       (widget:mount! editor 'completion-origin)
       (widget:present! (list (list (widget:prepare! editor 20 3) 0 0)))
       (let* ([source (factory '() (list (cons 'view editor) (cons 'receivers (widget:receivers editor))))]
              [input "(delta-log:show! 2"]
              [context ((completion:source-context source) input (string-length input))])
         (check 'composed-completion-keeps-canonical-editor-and-document-origin
           (list (widget:focused) (cdr (assq 'editor context)) (cdr (assq 'document context)))
           (list editor editor document))
         (widget:set-active! editor #f)
         (check 'inactive-presentation-is-not-the-current-receiver (widget:focused) #f))
       (widget:unmount! editor)
       (view:retire! head:ui-actor editor (model:revision editor)))

     (include "tests/environment-widget.sps")
     (test:finish! 'mx)))
