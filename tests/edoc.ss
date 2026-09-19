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

     (define check test:check)

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

     (check 'presentation-helpers
       (list (edoc-template 'visit '(path . more)) (first-sentence "Add two numbers. Slowly.") (first-sentence "No end")
             (type-text '(one-of utf-8 latin-1)) (type-text '(or string #f)) (type-text '(list-of buffer))
             (edoc-type? 'file) (edoc-type? '(list-of (or window buffer))) (edoc-type? 'nonsense) (edoc-type? '(list-of)))
       '("(visit path . more)" "Add two numbers." "No end" "one of utf-8 or latin-1" "string or #f" "list of buffer" #t #t #f #f))

     (check 'describe-entry-is-shaped-from-the-signatures
       (let ([entry (edoc-entry 'visit visit)])
         (list (car entry) (cadr entry) (caddr entry) (cadddr entry) (list-ref entry 4) (list-ref entry 5) (list-ref entry 6)
               (list-ref entry 7)))
       '((visit) (("procedure" . "(visit path)") ("procedure" . "(visit path encoding)")) #f () edoc "Documented definitions" #f
         "Visit a file.\nVisit a file in an encoding.\n\n- `path` (file)\n- `encoding` (one of utf-8 or latin-1)"))
     (check 'returns-and-notes-reach-the-entry
       (let ([entry (edoc-entry 'add add)]) (list (caddr entry) (list-ref entry 7)))
       '("integer" "Add two numbers. Slowly, for the test.\n\n- `x` (integer): the first addend\n- `y` (integer)"))

     ;; Malformed forms are rejected while expanding, with the reason.
     (define (rejection form)
       (guard (ex [(syntax-violation? ex) (condition-message ex)] [else (list 'other (condition-message ex))])
         (eval form (interaction-environment))
         'accepted))
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
           (edoc "text")))
       '("every formal needs an edoc clause" "an edoc clause names a formal" "unknown edoc type"
         "the edoc summary must be a string" "one edoc clause per formal" "a rest parameter is a list-of"
         "one returns clause at most" "edoc notes must be strings"
         "expected (edefine (name . formals) (edoc summary clause ...) body ...) or a case-lambda whose clauses open with edoc"
         "expected (edefine (name . formals) (edoc summary clause ...) body ...) or a case-lambda whose clauses open with edoc"
         "edoc belongs at the head of an edefine body"))

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
                 (not (for-all (lambda (sig) (equal? (map argument-name (signature-arguments sig)) (formal-list (signature-formals sig))))
                               (edoc-of (top-level-value sym)))))
               documented)
       '())
     (printf "edoc coverage: ~a of ~a editor procedures\n" (length documented) (length editor-procedures))

     (test:finish! 'edoc)))
