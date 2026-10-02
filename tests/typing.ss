#!/usr/bin/env scheme-script

;; Typing through the widget router: a run of typed characters, backspaces and
;; forward deletes is one undo step and one batch of the delta log, each
;; key its own entry, a typo and its correction together; moving point or
;; any other command starts a new run, and a run stops at twenty keys.
;; Headless, the editor's own key table installed.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (test) test:)
             (prefix (core kernel) kernel:)
             (prefix (head edit) edit:)
             (prefix (head head) head:)
             (prefix (head routing) routing:)
             (prefix (head widget) widget:)
             (prefix (head keymap) keymap:)
             (prefix (state store) store:))

     (define check test:check)
     (kernel:load-module! "widget") (kernel:load-module! "edit")
     (define b (store:create! head:ui-actor "typing" '("")))
     (define editor (edit:create-view! head:ui-actor b '()))
     (widget:mount! editor 'typing)
     (widget:present! (list (list (widget:prepare! editor 40 5) 0 0)))
     (define (text) (let-values ([(lines revision) (store:snapshot b)]) (vector->list lines)))
     (define (type! s)
       (for-each (lambda (ch) (routing:input! editor (list 'text (string ch) 'keyboard))) (string->list s)))
     (define (press! key . times)
       (do ([n (if (pair? times) (car times) 1) (- n 1)]) ((= n 0)) (routing:input! editor (list 'key key))))
     (define (batch-at i) (cdr (assq 'batch (caddr (list-ref (store:log b) i)))))
     (define (same-batch? . is) (for-all (lambda (i) (equal? (batch-at i) (batch-at (car is)))) (cdr is)))
     (define (label) (cadr (car (store:undo-labels b))))

     ;; typed characters are entries of one batch under one label; moving
     ;; point starts a new run
     (type! "abc")
     (check 'typed-characters-are-entries-of-one-batch
       (list (text) (length (store:log b)) (same-batch? 0 1 2) (label)) '(("abc") 3 #t "insert \"abc\""))
     (press! "LEFT")
     (type! "xz")
     (check 'moving-point-starts-a-new-run (list (text) (same-batch? 0 1) (same-batch? 1 2)) '(("abxzc") #t #f))

     ;; a typo corrected with backspace stays in the run, and one undo takes
     ;; the whole run back
     (press! "BACKSPACE")
     (type! "y")
     (check 'a-correction-joins-the-run (list (text) (same-batch? 0 1 2 3) (same-batch? 3 4) (label)) '(("abxyc") #t #f "insert \"xy\""))
     (press! "C-_")
     (check 'one-undo-takes-the-run-back (text) '("abc"))

     ;; forward deletes coalesce too, and a backspace past the run's own text
     ;; joins the run as older text deleted before it
     (press! "M-<")
     (press! "DELETE" 2)
     (check 'forward-deletes-coalesce (list (text) (same-batch? 0 1) (label)) '(("c") #t "delete \"ab\""))
     (press! "RIGHT")
     (type! "q")
     (press! "BACKSPACE" 2)
     (check 'a-run-mixes-typing-and-deleting-under-one-label
       (list (text) (same-batch? 0 1 2) (same-batch? 2 3) (label)) '(("") #t #f "delete \"c\""))
     (type! "one")
     (press! "DELETE")
     (check 'typing-after-deleting-continues-the-run-as-a-replacement
       (list (text) (same-batch? 0 1 2 3 4 5) (label)) '(("one") #t "replace \"c\" with \"one\""))

     ;; a run stops at twenty keys; a line break is another command and
     ;; breaks the run, while deleting one continues it
     (press! "M->")
     (type! (make-string 25 #\a))
     (check 'a-run-stops-at-twenty-keys (list (same-batch? 0 4) (same-batch? 4 5) (same-batch? 5 24)) '(#t #f #t))
     (press! "RET")
     (type! "b")
     (press! "BACKSPACE" 2)
     (check 'deleting-across-a-line-break-continues-the-run
       (list (text) (same-batch? 0 1 2) (same-batch? 2 3) (label))
       (list (list (string-append "one" (make-string 25 #\a))) #t #f "delete \"\\n\""))
     (define (unchanged) #t)
     (keymap:bind! 'widget-editor "F12" unchanged)
     (type! "x") (press! "F12") (type! "y") (press! "C-_")
     (check 'a-command-with-no-text-or-selection-change-ends-typing
       (text) (list (string-append "one" (make-string 25 #\a) "x")))
     (test:finish! 'typing)))
