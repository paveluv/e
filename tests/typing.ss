#!/usr/bin/env scheme-script

;; Typing through the dispatcher: a run of typed characters, backspaces and
;; forward deletes is one undo step and one batch of the delta log, each
;; key its own entry, a typo and its correction together; moving point or
;; any other command starts a new run, and a run stops at twenty keys.
;; Headless, the editor's own key table installed.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (rename (head edit) (init! edit-init!))
             (head literal)
             (prefix (apps delta-log) delta-log:)
             (prefix (head dispatch) dispatch:)
             (prefix (head head) head:)
             (prefix (state store) store:))

     (define check test:check)
     (edit-init!)
     (delta-log:init!)
     (define b (head:new-buffer! "typing"))
     (head:show-buffer! b)
     (head:goto! '(0 . 0))
     (define (text) (vector->list (head:buffer-lines b)))
     (define (type! s) (for-each dispatch:key! (string->list s)))
     (define (press! key . times) (do ([n (if (pair? times) (car times) 1) (- n 1)]) ((= n 0)) (dispatch:key! key)))
     (define (batch-at i) (cdr (assq 'batch (caddr (list-ref (delta-log:log) i)))))
     (define (same-batch? . is) (for-all (lambda (i) (equal? (batch-at i) (batch-at (car is)))) (cdr is)))
     (define (label) (cadr (car (store:undo-labels (head:buffer-store-id b)))))

     ;; typed characters are entries of one batch under one label; moving
     ;; point starts a new run
     (type! "abc")
     (check 'typed-characters-are-entries-of-one-batch
       (list (text) (length (delta-log:log)) (same-batch? 0 1 2) (label)) '(("abc") 3 #t "insert \"abc\""))
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
     (test:finish! 'typing)))
