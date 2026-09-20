#!/usr/bin/env scheme-script

;; Documented libraries: an elibrary checks each edoc annotation against the
;; definition that follows it while expanding, refuses undocumented exports,
;; and records the edocs at initialization; describe entries are shaped
;; from them. Also a coverage count over the editor's procedures. Run from
;; the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (edit) init!) (edoc) (prefix (kernel) kernel:) (prefix (test) test:))

     (define check test:check)

     ;; Malformed forms are rejected while expanding, with the reason.
     (define (rejection form)
       (guard (ex [(syntax-violation? ex) (condition-message ex)] [else (list 'other (condition-message ex))])
         (eval form (interaction-environment))
         'accepted))

     ;; A documented library: annotations precede the definitions they
     ;; document, which stay ordinary forms.
     (eval '(elibrary (probe)
              (export add join twice width limit make-place place? place-x place-y place-y-set!
                      make-tagged tagged? tagged-count swap! make-stale stale? stale-revision plain
                      (rename (twice again)) shared hidden paint)
              (import (rnrs) (only (chezscheme) make-parameter format))
              (edoc "Add two numbers. Slowly, for the test." (x integer "the first addend") (y integer) (returns integer))
              (define (add x y) (+ x y))
              (edoc "Join strings." (sep string) (parts (list-of string)))
              (define (join sep . parts) (fold-left (lambda (out p) (string-append out sep p)) (car parts) (cdr parts)))
              (edoc "Apply a procedure twice." (f procedure "the procedure") (x any))
              (define twice (let () (lambda (f x) (f (f x)))))
              (edoc "The width of the box." (value integer "in columns"))
              (define width (make-parameter 80))
              (edoc "How many rows at most." (value integer))
              (define limit 40)
              (edoc "A place on the screen." (x integer "the column") (y integer))
              (define-record-type place (fields (immutable x) (mutable y)))
              (edoc "A tagged value." (tag symbol "the tag") (value any) (count integer "how often it was seen")
                    (constructor tag value))
              (define-record-type (tagged make-tagged tagged?)
                (fields tag value (mutable count))
                (protocol (lambda (new) (lambda (tag value) (new tag value 0)))))
              (edoc "Exchange the values of two variables." (a symbol "a variable") (b symbol))
              (define-syntax swap! (syntax-rules () [(_ a b) (let ([t a]) (set! a b) (set! b t))]))
              (edoc "A stale basis was used." (revision integer "the revision that was current"))
              (define-condition-type &stale &error make-stale stale? (revision stale-revision))
              (edoc "Multiply, by a step or by one."
                    (n integer "the number") (step integer "the step, 1 when omitted") (returns integer))
              (define plain (case-lambda [(n) (plain n 1)] [(n step) (* n step)]))
              (define shared twice)
              ;; a type of the library's own, with a completer and a writer
              (edoc-type hue "a hue, by name"
                (predicate (lambda (v) (and (memq v '(red green blue)) #t)))
                (complete (lambda (partial) (map (lambda (h) (cons h "a hue")) '(red green blue))))
                (write (lambda (v) (format "'~a" v))))
              (edoc "Paint a place with a hue." (p (record place) "the place") (h hue "the hue") (returns list))
              (define (paint p h) (list p h))
              ;; a definition another macro produces is documented by name
              (define-syntax defthing (syntax-rules () [(_ n v) (define n v)]))
              (edoc hidden "A value a macro defined." (value integer))
              (defthing hidden 3)
              (define helper 1))
           (interaction-environment))
     (eval '(import (probe)) (interaction-environment))
     (define (value-of n) (top-level-value n))
     (define (signatures n) (or (and (top-level-bound? n) (edoc-of (value-of n))) (edoc-named n)))
     (define (kinds-of . names) (map (lambda (n) (signature-kind (car (signatures n)))) names))

     (check 'the-definitions-run
       (let ([a 1] [b 2])
         (eval '(let ([a 1] [b 2]) (swap! a b) (list a b)) (interaction-environment))
         (list ((value-of 'add) 1 2) ((value-of 'join) "-" "a" "b") ((value-of 'twice) (lambda (n) (* n 2)) 3)
               ((value-of 'width)) (value-of 'limit) ((value-of 'place-x) ((value-of 'make-place) 4 5))
               ((value-of 'tagged-count) ((value-of 'make-tagged) 'a 1)) ((value-of 'plain) 3) ((value-of 'plain) 3 2)
               ((value-of 'stale-revision) ((value-of 'make-stale) 4)) (value-of 'hidden)))
       '(3 "a-b" 12 80 40 4 0 3 6 4 3))

     (check 'every-kind-reads-back
       (list (kinds-of 'add 'twice 'width 'make-place 'place? 'place-x 'place-y-set! 'make-tagged 'stale? 'stale-revision 'plain 'shared)
             (map (lambda (n) (signature-kind (car (edoc-named n)))) '(limit place tagged swap! &stale hidden))
             (edoc-of (lambda (x) x)) (edoc-of car) (edoc-of 3))
       '((procedure procedure parameter constructor predicate accessor mutator constructor predicate accessor procedure procedure)
         (value record record syntax condition value)
         #f #f #f))

     (let ([sig (car (signatures 'add))])
       (check 'a-signature-reads-back
         (list (length (signatures 'add)) (signature-formals sig) (signature-summary sig)
               (map (lambda (a) (list (argument-name a) (argument-type a) (argument-notes a))) (signature-arguments sig))
               (argument-type (signature-returns sig)) (signature-library sig))
         '(1 (x y) "Add two numbers. Slowly, for the test." ((x integer ("the first addend")) (y integer ())) integer "(probe)")))
     (check 'rest-parameters-and-clauses-read-back
       (let ([sig (car (signatures 'join))])
         (list (signature-formals sig) (map argument-type (signature-arguments sig)) (signature-returns sig)
               (map signature-formals (signatures 'plain))
               (map (lambda (s) (map argument-name (signature-arguments s))) (signatures 'plain))
               (signature-formals (car (signatures 'twice)))))
       '((sep . parts) (string (list-of string)) #f ((n) (n step)) ((n) (n step)) (f x)))
     (check 'records-and-conditions-derive-their-procedures
       (list (map argument-name (signature-arguments (car (signatures 'make-tagged))))
             (signature-summary (car (signatures 'place-x))) (signature-summary (car (signatures 'place?)))
             (map (lambda (a) (list (argument-name a) (argument-type a))) (signature-arguments (car (signatures 'place-y-set!))))
             (signature-summary (car (signatures 'stale-revision))))
       '((tag value) "The x of a place: the column" "Whether a value is a place." ((place (record place)) (value integer))
         "The revision of a stale: the revision that was current"))
     (check 'an-alias-shares-its-origin
       (signature-summary (car (signatures 'shared))) "Apply a procedure twice.")

     ;; Types are data the library registered when it initialized.
     (check 'types-resolve-to-records-with-behavior
       (list (type-owner (type-named 'hue)) (type-owner (type-named 'string)) (type-owner (type-named 'buffer))
             (type-prose 'hue) (type-prose 'integer) (type-prose '(or hue #f))
             (type-accepts? 'hue 'red) (type-accepts? 'hue 'pink) (type-accepts? 'integer 2) (type-accepts? 'integer 2.5)
             (type-accepts? 'thunk (lambda () 1)) (type-accepts? 'thunk (lambda (x) x))
             (type-accepts? '(or string #f) #f) (type-accepts? '(or string #f) 'x)
             (type-accepts? '(list-of integer) '(1 2)) (type-accepts? '(list-of integer) '(1 a))
             (type-accepts? '(record place) ((value-of 'make-place) 1 2)) (type-accepts? '(record place) 3)
             (type-accepts? '(one-of utf-8 latin-1) 'utf-8) (type-accepts? 'datum '(1 "a" #(b)))
             (type-accepts? 'datum (lambda (x) x)) (type-accepts? 'nonsense 1)
             (type-completions 'hue "") (type-completions '(one-of utf-8 latin-1) "") (type-completions '(or hue #f) "x")
             (type-spelling 'hue 'red) (type-spelling 'string "a") (type-spelling '(one-of a b) 'a) (type-spelling '(or string hue) 'blue)
             (edoc-type? 'hue) (edoc-type? 'nonsense) (edoc-type? '(list-of hue)))
       '("(probe)" "(edoc)" #f "a hue, by name" "an exact integer" "hue or #f"
         #t #f #t #f #t #f #t #f #t #f #t #f #t #t #f #t
         ((red . "a hue") (green . "a hue") (blue . "a hue")) ((utf-8 . #f) (latin-1 . #f))
         ((red . "a hue") (green . "a hue") (blue . "a hue") (#f . #f))
         "'red" "\"a\"" "'a" "'blue" #t #f #t))
     ;; Defining a library only visits it; a reference invokes it, and that is
     ;; when its names resolve. The steps are sequenced explicitly, and the
     ;; broken libraries are referenced in a copy of the environment, so the
     ;; scan of every top-level value below does not trip over them.
     (define (invoked library name)
       (guard (ex [else (condition-message ex)])
         (eval (list 'begin (list 'import library) name) (copy-environment (interaction-environment) #t))
         'imported))
     (define defined-badtype
       (rejection '(elibrary (badtype) (export f) (import (rnrs)) (edoc "x" (a nonsense-type)) (define (f a) a))))
     (define invoked-badtype (invoked '(badtype) 'f))
     (define defined-badtype2
       (rejection '(elibrary (badtype2) (export g) (import (rnrs))
                     (edoc-type hue "another hue" (predicate symbol?)) (edoc "x") (define (g) 1))))
     (define invoked-badtype2 (invoked '(badtype2) 'g))
     (check 'unknown-types-fail-when-the-library-initializes
       (list defined-badtype invoked-badtype defined-badtype2 invoked-badtype2)
       '(accepted "unknown edoc type nonsense-type in the edoc of f" accepted "type hue is defined by (probe)"))

     (check 'entries-follow-the-kind
       (map (lambda (name) (let ([entry (edoc-entry name (signatures name))]) (list (cadr entry) (caddr entry))))
            '(width limit swap! make-place place-y-set! plain place))
       '(((("parameter" . "(width [value])")) "integer: in columns")
         ((("variable" . "limit")) "integer")
         ((("syntax" . "(swap! a b)")) #f)
         ((("procedure" . "(make-place x y)")) "place record")
         ((("procedure" . "(place-y-set! place value)")) #f)
         ((("procedure" . "(plain n)") ("procedure" . "(plain n step)")) "integer")
         ((("record" . "place")) #f)))
     (check 'describe-entry-is-shaped-from-the-signatures
       (let ([entry (edoc-entry 'add (signatures 'add))])
         (list (car entry) (cadr entry) (caddr entry) (cadddr entry) (list-ref entry 4) (list-ref entry 5) (list-ref entry 6)
               (list-ref entry 7)))
       '((add) (("procedure" . "(add x y)")) "integer" ("(probe)") edoc "Documented definitions" #f
         "Add two numbers. Slowly, for the test.\n\n- `x` (integer): the first addend\n- `y` (integer)"))

     (check 'presentation-helpers
       (list (edoc-template 'visit '(path . more)) (first-sentence "Add two numbers. Slowly.") (first-sentence "No end")
             (type-text '(one-of utf-8 latin-1)) (type-text '(or string #f)) (type-text '(list-of buffer)) (type-text '(record frame))
             (edoc-type? 'file) (edoc-type? '(list-of (or window buffer))) (edoc-type? '(record frame))
             (edoc-type? '(or position #f)) (edoc-type? 'nonsense) (edoc-type? '(list-of)))
       '("(visit path . more)" "Add two numbers." "No end" "one of utf-8 or latin-1" "string or #f" "list of buffer"
         "frame record" #t #t #t #t #f #f))

     (check 'an-elibrary-refuses-malformed-and-missing-edocs
       (map rejection
         '((elibrary (bad1) (export f) (import (rnrs)) (define (f) 1))
           (elibrary (bad2) (export) (import (rnrs)) (edoc "x") (display 1))
           (elibrary (bad3) (export f) (import (rnrs)) (edoc "x" (y integer)) (define (f x) x))
           (elibrary (bad4) (export g) (import (rnrs)) (define (f) 1) (edoc "x") (define g f))
           (elibrary (bad5) (export f) (import (rnrs)) (edoc "x") (edoc "y") (define (f) 1))
           (elibrary (bad6) (export f) (import (rnrs)) (edoc f "x") (edoc "y") (define (f) 1))
           (elibrary (bad7) (export f) (import (rnrs)) (edoc "x" (rest integer)) (define (f . rest) rest))
           (elibrary (bad8) (export f) (import (rnrs)) (edoc "x"))
           (elibrary (bad9) (export f) (import (rnrs)) (edoc "x") (define (f x y) x))
           (elibrary (bad11) (export f) (import (rnrs)) (edoc 7 (x integer)) (define (f x) x))
           (elibrary (bad12) (export f) (import (rnrs)) (edoc "x" (x integer) (x integer)) (define (f x) x))
           (elibrary (bad13) (export f) (import (rnrs)) (edoc "x" (x integer) (returns integer) (returns string)) (define (f x) x))
           (elibrary (bad14) (export f) (import (rnrs)) (edoc "x" (x integer 7)) (define (f x) x))
           (elibrary (bad15) (export v) (import (rnrs)) (edoc "x" (value integer) (x integer)) (define v 3))
           (elibrary (bad16) (export t) (import (rnrs)) (edoc "x") (edoc-type t "a t" (predicate symbol?)))
           (elibrary (bad17) (export r) (import (rnrs)) (edoc "x" (x integer) (z integer)) (define-record-type r (fields x)))
           (elibrary (bad18) (export r) (import (rnrs)) (edoc "x") (define-record-type r (fields x)))
           (elibrary (bad19) (export r) (import (rnrs)) (edoc "x" (x integer) (x integer)) (define-record-type r (fields x)))
           (elibrary (bad20) (export r) (import (rnrs)) (edoc "x" (x integer) (constructor x)) (define-record-type r (fields x)))
           (elibrary (bad21) (export c?) (import (rnrs)) (edoc "x") (define-condition-type &c &error make-c c? (x c-x)))
           (elibrary (bad22) (export place-x) (import (rnrs)) (define-record-type place (fields x)))
           (edoc "text")))
       '("export has no edoc" "an edoc annotates the definition that follows it" "an edoc clause names a formal"
         "an alias takes its documentation from its origin" "two edocs annotate one definition"
         "two edocs document one definition" "a rest parameter is a list-of"
         "an edoc annotates the definition that follows it"
         "every formal needs an edoc clause"
         "expected (edoc summary clause ...) or (edoc name summary clause ...)"
         "one edoc clause per formal" "one returns clause at most" "edoc notes must be strings"
         "a value clause stands alone" "an edoc annotates the definition that follows it"
         "an edoc clause names a field" "every field needs an edoc clause" "one edoc clause per field"
         "a constructor clause belongs to a record with a protocol or parent" "every field needs an edoc clause"
         "export has no edoc" "edoc annotates a definition inside an elibrary"))

     (check 'the-edoc-library-documents-itself
       (list (map (lambda (name) (signature-kind (car (edoc-named name)))) '(edoc elibrary))
             (signature-kind (car (edoc-of edoc-of))) (signature-library (car (edoc-of edoc-of)))
             (signature-kind (car (edoc-of signature-kind))) (signature-kind (car (edoc-of edoc-types))))
       '((syntax syntax) procedure "(edoc)" accessor value))

     ;; The command layer's definitions read back, and every documented
     ;; editor procedure's clauses match its formals.
     (define (formal-list formals)
       (let loop ([f formals] [out '()])
         (cond [(null? f) (reverse out)] [(pair? f) (loop (cdr f) (cons (car f) out))] [else (reverse (cons f out))])))
     (define editor-procedures
       (filter (lambda (sym) (and (kernel:editor-symbol? sym) (procedure? (top-level-value sym))))
               (environment-symbols (interaction-environment))))
     (define documented (filter (lambda (sym) (edoc-of (top-level-value sym))) editor-procedures))
     (check 'the-command-layer-reads-back
       (list (map argument-type (signature-arguments (car (edoc-of visit-file!))))
             (map argument-type (signature-arguments (car (edoc-of present-log-entries!))))
             (signature-arguments (car (edoc-of split-window-below!)))
             (argument-type (signature-returns (car (edoc-of select-window!))))
             (signature-library (car (edoc-of delete-window!))))
       '((file) ((list-of datum)) () boolean "(edit)"))
     (check 'documented-clauses-match-their-formals
       (filter (lambda (sym)
                 (not (for-all (lambda (sig)
                                 (or (not (eq? (signature-kind sig) 'procedure))
                                     (equal? (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                                                        (map argument-name (signature-arguments sig)))
                                             (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                                                        (formal-list (signature-formals sig))))))
                               (edoc-of (top-level-value sym)))))
               documented)
       '())
     (printf "edoc coverage: ~a of ~a editor procedures\n" (length documented) (length editor-procedures))

     (test:finish! 'edoc)))
