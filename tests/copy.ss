#!/usr/bin/env scheme-script

;; The copy buffer is a buffer of its own, <copy>: copies and kills land
;; there as undo entries, yank reads the text back exactly, and it is
;; created, capped and recreated like any tool buffer.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:))

     (define check test:check)
     (define (fresh name lines)
       (let ([b (head:new-buffer! name)])
         (head:buffer-lines-set! b (list->vector lines))
         (head:show-buffer! b)
         (head:goto! '(0 . 0))
         b))
     (define (copy) (head:buffer-named "<copy>"))
     (define (undo-entries) (length (vector-ref (head:buffer-history (copy)) 0)))

     (check 'no-copy-buffer-before-the-first-copy (list (copy) (head:copy-text) (copy-text)) '(#f "" ""))
     (copy-text! "abc\n")
     (check 'the-first-copy-creates-the-local-buffer
       (list (eq? (copy) (head:copy-buffer)) (head:buffer-name (copy)) (and (memq (copy) (head:buffers)) #t)
             (head:buffer-lines (copy)) (head:buffer-trailing (copy)) (head:copy-text) (copy-text))
       (list #t "<copy>" #t (vector "abc") #t "abc\n" "abc\n"))
     (check 'one-undo-entry-per-copy-a-limit-fact-and-no-modified-mark
       (list (undo-entries) (head:buffer-fact (copy) 'history-limit #f) (head:buffer-modified (copy)))
       '(1 1024 #f))

     (check 'copies-round-trip-exactly
       (map (lambda (s) (copy-text! s) (head:copy-text)) '("" "\n" "a" "a\n\nb" "a\n\n"))
       '("" "\n" "a" "a\n\nb" "a\n\n"))

     ;; yank inserts the text as buffer structure and leaves point after it
     (define target (fresh "copy-target" '("xy")))
     (head:goto! '(0 . 1))
     (copy-text! "1\n2\n")
     (yank!)
     (check 'yank-inserts-the-copy-text-with-its-line-breaks
       (list (vector->list (head:buffer-lines target)) (head:point)) '(("x1" "2" "y") (2 . 0)))

     ;; consecutive kills accumulate in the copy buffer
     (define source (fresh "copy-source" '("one" "two" "three")))
     (kill-line!)
     (head:set-last-command! kill-line!)
     (kill-line!)
     (head:set-last-command! kill-line!)
     (kill-line!)
     (check 'consecutive-kills-accumulate
       (list (head:copy-text) (vector->list (head:buffer-lines source))) '("one\ntwo" ("" "three")))

     ;; undo in <copy> brings the previous copy back; redo the later one
     (copy-text! "first")
     (copy-text! "second")
     (head:with-buffer (head:copy-buffer) (undo!))
     (define after-undo (head:copy-text))
     (head:with-buffer (head:copy-buffer) (redo!))
     (check 'undo-and-redo-in-the-copy-buffer-walk-the-copies (list after-undo (head:copy-text)) '("first" "second"))

     ;; the raw setter is a new baseline, without an undo entry
     (define entries (undo-entries))
     (head:set-copy-text! "raw\n")
     (check 'the-raw-setter-adds-no-undo-entry (list (head:copy-text) (undo-entries)) (list "raw\n" entries))

     ;; the history-limit fact caps the undo entries
     (head:buffer-fact-set! (copy) 'history-limit 3)
     (for-each copy-text! '("a" "b" "c" "d" "e"))
     (check 'the-history-limit-fact-caps-the-undo-entries (list (undo-entries) (head:copy-text)) '(3 "e"))
     (head:with-buffer (head:copy-buffer) (undo!) (undo!) (undo!))
     (check 'the-capped-history-still-walks-back (head:copy-text) "b")

     ;; killing <copy> asks nothing; the next copy recreates it, without history
     (define old (copy))
     (kill-buffer! old)
     (define gone (copy))
     (copy-text! "again")
     (check 'a-killed-copy-buffer-is-recreated-by-the-next-copy
       (list gone (eq? (copy) old) (head:copy-text) (undo-entries)) '(#f #f "again" 1))

     ;; a window showing <copy> follows the arriving text
     (head:show-buffer! (head:copy-buffer))
     (head:goto! '(0 . 0))
     (copy-text! "l1\nl2\nl3")
     (check 'a-window-showing-the-copy-buffer-follows-the-arriving-text
       (list (eq? (head:current-buffer) (copy)) (head:point)) '(#t (2 . 2)))

     ;; a plain local buffer returns on resume with its text and facts; <copy> is one
     (define notes (head:new-local-buffer! "notes"))
     (head:buffer-fact-set! notes 'history-limit 7)
     (head:buffer-lines-set! notes (vector "keep" "me"))
     (head:add-buffer! notes)
     (copy-text! "before")
     (head:checkpoint!)
     (head:buffer-lines-set! notes (vector "lost"))
     (copy-text! "lost too")
     (head:resume!)
     (check 'plain-local-buffers-return-on-resume-with-text-and-facts
       (list (vector->list (head:buffer-lines (head:buffer-named "<notes>")))
             (head:buffer-fact (head:buffer-named "<notes>") 'history-limit #f)
             (head:copy-text) (eq? (head:buffer-named "<notes>") notes))
       '(("keep" "me") 7 "before" #t))

     (test:finish! 'copy)))
