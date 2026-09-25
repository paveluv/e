#!/usr/bin/env scheme-script

;; The copy buffer is a buffer of its own, *copy*, shared through the base
;; in this head's audience: copies and kills land there as entries of its
;; log, yank reads the text back exactly, and it is created and recreated
;; when needed.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (state store) store:))

     (define check test:check)
     (define (fresh name lines)
       (let ([b (head:new-buffer! name)])
         (head:buffer-lines-set! b (list->vector lines))
         (head:show-buffer! b)
         (head:goto! '(0 . 0))
         b))
     (define (copy) (head:copy-buffer #f))
     (define (entries) (length (store:log (head:buffer-store-id (copy)))))

     (check 'no-copy-buffer-before-the-first-copy (list (copy) (head:copy-text) (copy-text)) '(#f "" ""))
     (copy-text! "abc\n")
     ;; the head shows a buffer that is its alone, its audience restricted, in square brackets
     (check 'the-first-copy-creates-the-shared-buffer-in-this-heads-audience-shown-in-brackets
       (list (eq? (copy) (head:copy-buffer)) (head:buffer-name (copy)) (store:buffer-name (head:buffer-store-id (copy)))
             (and (memq (copy) (head:buffers)) #t)
             (head:buffer-fact (copy) 'copy #f) (head:buffer-fact (copy) 'audience #f) (head:buffer-fact (copy) 'disposable #f)
             (head:buffer-lines (copy)) (head:buffer-trailing (copy)) (head:copy-text) (copy-text))
       (list #t "[copy]" "*copy*" #t #t (list head:ui-actor) #t (vector "abc") #t "abc\n" "abc\n"))
     (check 'one-log-entry-per-copy (entries) 1)

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

     ;; undo in *copy* brings the previous copy back; redo the later one
     (copy-text! "first")
     (copy-text! "second")
     (head:with-buffer (head:copy-buffer) (undo!))
     (define after-undo (head:copy-text))
     (head:with-buffer (head:copy-buffer) (redo!))
     (check 'undo-and-redo-in-the-copy-buffer-walk-the-copies (list after-undo (head:copy-text)) '("first" "second"))

     ;; the setter is one more entry, so a prompt's kill is undone like a copy
     (define before (entries))
     (head:set-copy-text! "raw\n")
     (check 'the-setter-adds-one-entry (list (head:copy-text) (- (entries) before)) (list "raw\n" 1))
     (head:with-buffer (head:copy-buffer) (undo!))
     (check 'undo-takes-the-set-text-back (head:copy-text) "second")

     ;; killing *copy* asks nothing and deletes it from the store; the next copy recreates it
     (define old (copy))
     (define old-id (head:buffer-store-id old))
     (kill-buffer! old)
     (define gone (copy))
     (copy-text! "again")
     (check 'a-killed-copy-buffer-is-recreated-by-the-next-copy
       (list gone (store:exists? old-id) (eq? (copy) old) (head:copy-text) (entries)) '(#f #f #f "again" 1))

     ;; the store names a second head's copy buffer apart, *copy*<2>; a head
     ;; shows its own as [copy] whatever the store's suffix, unless it already
     ;; shows a buffer under that label, when the suffix stays
     (kill-buffer! (copy))
     (define other '(head "other seat"))
     (define others (store:create! other "*copy*" '("theirs") (list (cons 'copy #t) (cons 'audience (list other)) (cons 'disposable #t))))
     (copy-text! "mine")
     (check 'a-second-heads-copy-buffer-is-named-apart-in-the-store-and-shown-plain-here
       (list (store:buffer-name others) (store:buffer-name (head:buffer-store-id (copy))) (head:buffer-name (copy)) (head:copy-text)
             (and (head:buffer-named "*copy*<2>") #t))
       '("*copy*" "*copy*<2>" "[copy]" "mine" #f))
     (define twin (store:create! head:ui-actor "*copy*" '("twin") (list (cons 'audience (list head:ui-actor)) (cons 'disposable #t))))
     (head:adopt-store-buffer! twin)
     (check 'a-per-head-buffer-whose-label-is-taken-here-keeps-the-stores-suffix
       (list (store:buffer-name twin) (head:buffer-name (head:buffer-named "[copy<3>]")) (head:buffer-name (copy)))
       '("*copy*<3>" "[copy<3>]" "[copy]"))
     (kill-buffer! (head:buffer-named "[copy<3>]"))
     (store:delete! other others)

     ;; a window showing *copy* follows the arriving text
     (head:show-buffer! (head:copy-buffer))
     (head:goto! '(0 . 0))
     (copy-text! "l1\nl2\nl3")
     (check 'a-window-showing-the-copy-buffer-follows-the-arriving-text
       (list (eq? (head:current-buffer) (copy)) (head:point)) '(#t (2 . 2)))

     ;; a plain local buffer returns on resume with its text and facts; the
     ;; copy buffer lives in the base and needs no checkpoint
     (define notes (head:new-local-buffer! "notes"))
     (head:buffer-fact-set! notes 'note 7)
     (head:buffer-lines-set! notes (vector "keep" "me"))
     (head:add-buffer! notes)
     (copy-text! "before")
     (head:checkpoint!)
     (head:buffer-lines-set! notes (vector "lost"))
     (copy-text! "kept in the base")
     (head:resume!)
     (check 'plain-local-buffers-return-on-resume-and-the-copy-buffer-stays-as-the-base-has-it
       (list (vector->list (head:buffer-lines (head:buffer-named "<notes>")))
             (head:buffer-fact (head:buffer-named "<notes>") 'note #f)
             (head:copy-text) (eq? (head:buffer-named "<notes>") notes) (eq? (copy) (head:buffer-named "[copy]")))
       '(("keep" "me") 7 "kept in the base" #t #t))

     (test:finish! 'copy)))
