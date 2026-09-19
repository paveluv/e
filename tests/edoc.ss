#!/usr/bin/env scheme-script

;; Documented definitions: edefine checks its edoc against the formals while
;; expanding, the quoted form costs nothing to run and reads back through
;; the inspector, and describe entries are shaped from it. Also a coverage
;; count over the command layer's procedures. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (edit) init!) (edoc) (prefix (test) test:))
     (import (only (chezscheme) make-parameter))

     (define check test:check)

     ;; Malformed forms are rejected while expanding, with the reason.
     (define (rejection form)
       (guard (ex [(syntax-violation? ex) (condition-message ex)] [else (list 'other (condition-message ex))])
         (eval form (interaction-environment))
         'accepted))

     (edefine (add x y)
       (edoc "Add two numbers. Slowly, for the test." (x integer "the first addend") (y integer) (returns integer))
       (+ x y))
     (edefine (join sep . parts)
       (edoc "Join strings." (sep string) (parts (list-of string)))
       (fold-left (lambda (out p) (string-append out sep p)) (car parts) (cdr parts)))
     (edefine visit
       (case-lambda
         [(path) (edoc "Visit a file." (path file)) (visit path 'utf-8)]
         [(path encoding) (edoc "Visit a file in an encoding." (path file) (encoding (one-of utf-8 latin-1))) (list path encoding)]))

     (check 'edefined-procedures-run (list (add 1 2) (join "-" "a" "b") (visit "p")) '(3 "a-b" ("p" utf-8)))

     ;; Values, parameters, keywords and records carry edocs too: attached to
     ;; the object, or recorded under the name when the object has no identity.
     (edefine width (edoc "The width of the box." (value integer "in columns")) (make-parameter 80))
     (edefine limit (edoc "How many rows at most." (value integer)) 40)
     (edefine twice (edoc "Apply a procedure twice." (f procedure "the procedure") (x any)) (let () (lambda (f x) (f (f x)))))
     (edefine-syntax swap!
       (edoc "Exchange the values of two variables." (a symbol "a variable") (b symbol))
       (syntax-rules () [(_ a b) (let ([t a]) (set! a b) (set! b t))]))
     (edefine-record-type point
       (edoc "A place on the screen." (x integer "the column") (y integer))
       (fields (immutable x) (mutable y)))
     (edefine-record-type (tagged make-tagged tagged?)
       (edoc "A tagged value." (tag symbol "the tag") (value any) (count integer "how often it was seen")
             (constructor tag value))
       (fields tag value (mutable count))
       (protocol (lambda (new) (lambda (tag value) (new tag value 0)))))
     (edefine-condition-type &stale &error make-stale stale?
       (edoc "A stale basis was used." (revision integer "the revision that was current"))
       (revision stale-revision))
     (define (kind-of sigs) (and sigs (signature-kind (car sigs))))
     (check 'protocol-constructors-and-conditions-read-back
       (list (map argument-name (signature-arguments (car (edoc-of make-tagged)))) (kind-of (edoc-of make-tagged))
             (tagged-count (make-tagged 'a 1)) (kind-of (edoc-of make-stale)) (kind-of (edoc-of stale?))
             (signature-summary (car (edoc-of stale-revision))) (stale-revision (make-stale 4))
             (cdr (car (cadr (edoc-entry 'make-stale (edoc-of make-stale))))))
       '((tag value) constructor 0 constructor predicate "The revision of a stale: the revision that was current" 4
         "(make-stale revision)"))
     (check 'values-and-keywords-run
       (let ([a 1] [b 2]) (swap! a b) (list (width) limit (twice (lambda (n) (* n 2)) 3) a b (point-x (make-point 4 5))))
       '(80 40 12 2 1 4))
     (check 'attached-edocs-read-back-by-kind
       (list (kind-of (edoc-of width)) (edoc-of limit) (kind-of (edoc-named 'limit)) (kind-of (edoc-of twice))
             (signature-formals (car (edoc-of twice))) (kind-of (edoc-named 'swap!)) (edoc-of 40)
             (map (lambda (p) (kind-of (edoc-of p))) (list make-point point? point-x point-y point-y-set!))
             (signature-summary (car (edoc-of point-x))) (signature-summary (car (edoc-of point?)))
             (map (lambda (a) (list (argument-name a) (argument-type a))) (signature-arguments (car (edoc-of point-y-set!)))))
       '(parameter #f value procedure (f x) syntax #f (constructor predicate accessor accessor mutator)
          "The x of a point: the column" "Whether a value is a point." ((point (record point)) (value integer))))
     (check 'entries-follow-the-kind
       (map (lambda (name sigs) (let ([entry (edoc-entry name sigs)]) (list (cadr entry) (caddr entry))))
            '(width limit swap! make-point point-y-set!)
            (list (edoc-of width) (edoc-named 'limit) (edoc-named 'swap!) (edoc-of make-point) (edoc-of point-y-set!)))
       '(((("parameter" . "(width [value])")) "integer: in columns")
         ((("variable" . "limit")) "integer")
         ((("syntax" . "(swap! a b)")) #f)
         ((("procedure" . "(make-point x y)")) #f)
         ((("procedure" . "(point-y-set! point value)")) #f)))

     (let ([sig (car (edoc-of add))])
       (check 'signature-reads-back
         (list (length (edoc-of add)) (signature-formals sig) (signature-summary sig)
               (map (lambda (a) (list (argument-name a) (argument-type a) (argument-notes a))) (signature-arguments sig))
               (argument-type (signature-returns sig)) (signature-library sig))
         '(1 (x y) "Add two numbers. Slowly, for the test." ((x integer ("the first addend")) (y integer ())) integer #f)))
     (check 'rest-parameter-reads-back
       (let ([sig (car (edoc-of join))])
         (list (signature-formals sig) (map argument-type (signature-arguments sig)) (signature-returns sig)))
       '((sep . parts) (string (list-of string)) #f))
     (check 'case-lambda-clauses-read-back
       (map (lambda (sig) (cons (signature-formals sig) (map argument-type (signature-arguments sig)))) (edoc-of visit))
       '(((path) file) ((path encoding) file (one-of utf-8 latin-1))))
     (check 'undocumented-procedures-have-no-signature (list (edoc-of car) (edoc-of (lambda (x) x)) (edoc-of 3)) '(#f #f #f))

     ;; A documented library: annotations precede the definitions they
     ;; document, which stay ordinary define forms.
     (eval '(elibrary (probe)
              (export twice width limit make-point point? point-x point-y-set! swap! stale? stale-revision plain
                      (rename (twice again)) shared)
              (import (rnrs) (only (chezscheme) make-parameter))
              (edoc "Apply a procedure twice." (f procedure "the procedure") (x any))
              (define (twice f x) (f (f x)))
              (edoc "The width." (value integer))
              (define width (make-parameter 80))
              (edoc "The limit." (value integer))
              (define limit 40)
              (edoc "A place." (x integer "the column") (y integer))
              (define-record-type point (fields (immutable x) (mutable y)))
              (edoc "Exchange two variables." (a symbol) (b symbol))
              (define-syntax swap! (syntax-rules () [(_ a b) (let ([t a]) (set! a b) (set! b t))]))
              (edoc "A stale basis." (revision integer "the revision that was current"))
              (define-condition-type &stale &error make-stale stale? (revision stale-revision))
              (edoc "Multiply, by a step or by one."
                    (n integer "the number") (step integer "the step, 1 when omitted") (returns integer))
              (define plain (case-lambda [(n) (plain n 1)] [(n step) (* n step)]))
              (define shared twice)
              (define helper 1))
           (interaction-environment))
     (eval '(import (probe)) (interaction-environment))
     (define (kinds-of . names) (map (lambda (n) (signature-kind (car (or (edoc-of (top-level-value n)) (edoc-named n))))) names))
     (check 'an-elibrary-documents-its-definitions-by-annotation
       (list (kinds-of 'twice 'width 'make-point 'point? 'point-x 'point-y-set! 'stale? 'stale-revision 'plain)
             (map (lambda (n) (signature-kind (car (edoc-named n)))) '(limit point swap! &stale))
             (signature-formals (car (edoc-of (top-level-value 'twice)))) (signature-library (car (edoc-of (top-level-value 'twice))))
             (map signature-formals (edoc-of (top-level-value 'plain)))
             (map (lambda (s) (map argument-name (signature-arguments s))) (edoc-of (top-level-value 'plain)))
             (signature-summary (car (edoc-of (top-level-value 'shared))))
             (cadr (edoc-entry 'plain (edoc-of (top-level-value 'plain)))))
       '((procedure parameter constructor predicate accessor mutator predicate accessor procedure)
         (value record syntax condition)
         (f x) "(probe)" ((n) (n step)) ((n) (n step)) "Apply a procedure twice."
         (("procedure" . "(plain n)") ("procedure" . "(plain n step)"))))
     (check 'an-elibrary-refuses-undocumented-and-misplaced-edocs
       (map rejection
         '((elibrary (bad1) (export f) (import (rnrs)) (define (f) 1))
           (elibrary (bad2) (export) (import (rnrs)) (edoc "x") (display 1))
           (elibrary (bad3) (export f) (import (rnrs)) (edoc "x" (y integer)) (define (f x) x))
           (elibrary (bad4) (export g) (import (rnrs)) (define (f) 1) (edoc "x") (define g f))
           (elibrary (bad5) (export f) (import (rnrs)) (edoc "x") (edoc "y") (define (f) 1))
           (elibrary (bad6) (export f) (import (rnrs)) (edoc f "x") (edoc "y") (define (f) 1))
           (elibrary (bad7) (export f) (import (rnrs)) (edoc "x" (rest integer)) (define (f . rest) rest))
           (elibrary (bad8) (export f) (import (rnrs)) (edoc "x"))))
       '("export has no edoc" "an edoc annotates the definition that follows it" "an edoc clause names a formal"
         "an alias takes its documentation from its origin" "two edocs annotate one definition"
         "two edocs document one definition" "a rest parameter is a list-of"
         "an edoc annotates the definition that follows it"))
     (check 'the-edoc-library-documents-itself
       (list (map (lambda (name) (signature-kind (car (edoc-named name)))) '(edoc edefine edefine-syntax edefine-record-type edefine-condition-type))
             (signature-kind (car (edoc-of edoc-of))) (signature-library (car (edoc-of edoc-of)))
             (signature-kind (car (edoc-of signature-kind))) (signature-kind (car (edoc-of edoc-types))))
       '((syntax syntax syntax syntax syntax) procedure "(edoc)" accessor value))

     (check 'presentation-helpers
       (list (edoc-template 'visit '(path . more)) (first-sentence "Add two numbers. Slowly.") (first-sentence "No end")
             (type-text '(one-of utf-8 latin-1)) (type-text '(or string #f)) (type-text '(list-of buffer)) (type-text '(record frame))
             (edoc-type? 'file) (edoc-type? '(list-of (or window buffer))) (edoc-type? '(record frame))
             (edoc-type? '(or position #f)) (edoc-type? 'nonsense) (edoc-type? '(list-of)))
       '("(visit path . more)" "Add two numbers." "No end" "one of utf-8 or latin-1" "string or #f" "list of buffer"
         "frame record" #t #t #t #t #f #f))

     (check 'describe-entry-is-shaped-from-the-signatures
       (let ([entry (edoc-entry 'visit (edoc-of visit))])
         (list (car entry) (cadr entry) (caddr entry) (cadddr entry) (list-ref entry 4) (list-ref entry 5) (list-ref entry 6)
               (list-ref entry 7)))
       '((visit) (("procedure" . "(visit path)") ("procedure" . "(visit path encoding)")) #f () edoc "Documented definitions" #f
         "Visit a file.\nVisit a file in an encoding.\n\n- `path` (file)\n- `encoding` (one of utf-8 or latin-1)"))
     (check 'returns-and-notes-reach-the-entry
       (let ([entry (edoc-entry 'add (edoc-of add))]) (list (caddr entry) (list-ref entry 7)))
       '("integer" "Add two numbers. Slowly, for the test.\n\n- `x` (integer): the first addend\n- `y` (integer)"))

     (check 'malformed-edefines-are-rejected
       (map rejection
         '((edefine (f x y) (edoc "text" (x integer)) x)
           (edefine (f x) (edoc "text" (x integer) (z integer)) x)
           (edefine (f x) (edoc "text" (x nonsense)) x)
           (edefine (f x) (edoc 7 (x integer)) x)
           (edefine (f x) (edoc "text" (x integer) (x integer)) x)
           (edefine (f . rest) (edoc "text" (rest string)) rest)
           (edefine (f x) (edoc "text" (x integer) (returns integer) (returns string)) x)
           (edefine (f x) (edoc "text" (x integer 7)) x)
           (edefine (f x) (edoc "text" (x integer)))
           (edefine (f x) x)
           (edoc "text")
           (edefine v (edoc "text" (value integer) (x integer)) 3)
           (edefine v (edoc "text" (x integer) (x string)) (lambda (x) x))
           (edefine-syntax s (edoc "text" (a nonsense)) (syntax-rules () [(_ a) a]))
           (edefine-record-type r (edoc "text" (x integer) (z integer)) (fields x))
           (edefine-record-type r (edoc "text") (fields x))
           (edefine-record-type r (edoc "text" (x integer) (x integer)) (fields x))
           (edefine-record-type r (edoc "text" (x integer) (constructor x)) (fields x))
           (edefine-condition-type &c &error make-c c? (edoc "text") (x c-x))))
       '("every formal needs an edoc clause" "an edoc clause names a formal" "unknown edoc type"
         "the edoc summary must be a string" "one edoc clause per formal" "a rest parameter is a list-of"
         "one returns clause at most" "edoc notes must be strings"
         "expected (edefine (name . formals) (edoc summary clause ...) body ...), a case-lambda whose clauses open with edoc, or (edefine name (edoc summary clause ...) expression)"
         "expected (edefine (name . formals) (edoc summary clause ...) body ...), a case-lambda whose clauses open with edoc, or (edefine name (edoc summary clause ...) expression)"
         "edoc annotates a definition inside an elibrary, or heads an edefine form"
         "a value clause stands alone" "one edoc clause per name" "unknown edoc type"
         "an edoc clause names a field" "every field needs an edoc clause" "one edoc clause per field"
         "a constructor clause belongs to a record with a protocol or parent" "every field needs an edoc clause"))

     ;; The command layer's converted definitions read back, and every
     ;; documented editor procedure's clauses match its formals.
     (define (formal-list formals)
       (let loop ([f formals] [out '()])
         (cond [(null? f) (reverse out)] [(pair? f) (loop (cdr f) (cons (car f) out))] [else (reverse (cons f out))])))
     (define editor-procedures
       (filter (lambda (sym) (and (editor-symbol? sym) (procedure? (top-level-value sym))))
               (environment-symbols (interaction-environment))))
     (define documented (filter (lambda (sym) (edoc-of (top-level-value sym))) editor-procedures))
     (check 'converted-commands-read-back
       (list (map argument-type (signature-arguments (car (edoc-of visit-file!))))
             (map argument-type (signature-arguments (car (edoc-of wrap!))))
             (signature-arguments (car (edoc-of split-window-below!)))
             (argument-type (signature-returns (car (edoc-of select-window!))))
             (signature-library (car (edoc-of delete-window!))))
       '((file) ((list-of boolean)) () boolean "(edit)"))
     (check 'documented-clauses-match-their-formals
       (filter (lambda (sym)
                 (not (for-all (lambda (sig)
                                 (or (not (eq? (signature-kind sig) 'procedure))
                                     (equal? (map argument-name (signature-arguments sig)) (formal-list (signature-formals sig)))))
                               (edoc-of (top-level-value sym)))))
               documented)
       '())
     (printf "edoc coverage: ~a of ~a editor procedures\n" (length documented) (length editor-procedures))

     (test:finish! 'edoc)))
