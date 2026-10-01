#!/usr/bin/env scheme-script

;; The copy buffer is a buffer of its own, *copy*, shared through the base
;; in this head's audience: copies and kills land there as entries of its
;; log, yank reads the text back exactly, and it is created and recreated
;; when needed.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head window) window:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:)
             (prefix (state store) store:))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
     (define (fresh name lines)
       (let ([b (seat:new-buffer! name)])
         (seat:buffer-lines-set! b (list->vector lines))
         (seat:show-buffer-mirror! b)
         (seat:goto! '(0 . 0))
         b))
     (define (copy) (seat:copy-buffer #f))
     (define (entries) (length (store:log (seat:buffer-store-id (copy)))))

     (check 'no-copy-buffer-before-the-first-copy (list (copy) (seat:copy-text) (copy-text)) '(#f "" ""))
     (copy-text! "abc\n")
     ;; the head shows a buffer that is its alone, its audience restricted, in square brackets
     (check 'the-first-copy-creates-the-shared-buffer-in-this-heads-audience-shown-in-brackets
       (list (eq? (copy) (seat:copy-buffer)) (seat:buffer-name (copy)) (store:buffer-name (seat:buffer-store-id (copy)))
             (and (memq (copy) (seat:buffers)) #t)
             (seat:buffer-fact (copy) 'copy #f) (seat:buffer-fact (copy) 'audience #f) (seat:buffer-fact (copy) 'disposable #f)
             (seat:buffer-lines (copy)) (seat:buffer-trailing (copy)) (seat:copy-text) (copy-text))
       (list #t "[copy]" "*copy*" #t #t (list head:ui-actor) #t (vector "abc") #t "abc\n" "abc\n"))
     (check 'one-log-entry-per-copy (entries) 1)

     (check 'copies-round-trip-exactly
       (map (lambda (s) (copy-text! s) (seat:copy-text)) '("" "\n" "a" "a\n\nb" "a\n\n"))
       '("" "\n" "a" "a\n\nb" "a\n\n"))

     ;; yank inserts the text as buffer structure and leaves point after it
     (define target (fresh "copy-target" '("xy")))
     (seat:goto! '(0 . 1))
     (copy-text! "1\n2\n")
     (yank!)
     (check 'yank-inserts-the-copy-text-with-its-line-breaks
       (list (vector->list (seat:buffer-lines target)) (seat:point)) '(("x1" "2" "y") (2 . 0)))

     ;; consecutive kills accumulate in the copy buffer
     (define source (fresh "copy-source" '("one" "two" "three")))
     (kill-line!)
     (head:set-last-command! kill-line!)
     (kill-line!)
     (head:set-last-command! kill-line!)
     (kill-line!)
     (check 'consecutive-kills-accumulate
       (list (seat:copy-text) (vector->list (seat:buffer-lines source))) '("one\ntwo" ("" "three")))

     ;; undo in *copy* brings the previous copy back; redo the later one
     (copy-text! "first")
     (copy-text! "second")
     (seat:with-buffer-mirror (seat:copy-buffer) (undo!))
     (define after-undo (seat:copy-text))
     (seat:with-buffer-mirror (seat:copy-buffer) (redo!))
     (check 'undo-and-redo-in-the-copy-buffer-walk-the-copies (list after-undo (seat:copy-text)) '("first" "second"))

     ;; the setter is one more entry, so a prompt's kill is undone like a copy
     (define before (entries))
     (seat:set-copy-text! "raw\n")
     (check 'the-setter-adds-one-entry (list (seat:copy-text) (- (entries) before)) (list "raw\n" 1))
     (seat:with-buffer-mirror (seat:copy-buffer) (undo!))
     (check 'undo-takes-the-set-text-back (seat:copy-text) "second")

     ;; killing *copy* asks nothing and deletes it from the store; the next copy recreates it
     (define old (copy))
     (define old-id (seat:buffer-store-id old))
     (kill-buffer! (seat:buffer-store-id old))
     (define gone (copy))
     (copy-text! "again")
     (check 'a-killed-copy-buffer-is-recreated-by-the-next-copy
       (list gone (store:exists? old-id) (eq? (copy) old) (seat:copy-text) (entries)) '(#f #f #f "again" 1))

     ;; the store names a second head's copy buffer apart, *copy*<2>; a head
     ;; shows its own as [copy] whatever the store's suffix, unless it already
     ;; shows a buffer under that label, when the suffix stays
     (kill-buffer! (seat:buffer-store-id (copy)))
     (define other '(head "other seat"))
     (define others (store:create! other "*copy*" '("theirs") (list (cons 'copy #t) (cons 'audience (list other)) (cons 'disposable #t))))
     (copy-text! "mine")
     (check 'a-second-heads-copy-buffer-is-named-apart-in-the-store-and-shown-plain-here
       (list (store:buffer-name others) (store:buffer-name (seat:buffer-store-id (copy))) (seat:buffer-name (copy)) (seat:copy-text)
             (and (seat:buffer-named "*copy*<2>") #t))
       '("*copy*" "*copy*<2>" "[copy]" "mine" #f))
     (define twin (store:create! head:ui-actor "*copy*" '("twin") (list (cons 'audience (list head:ui-actor)) (cons 'disposable #t))))
     (seat:adopt-store-buffer! twin)
     (check 'a-per-head-buffer-whose-label-is-taken-here-keeps-the-stores-suffix
       (list (store:buffer-name twin) (seat:buffer-name (seat:buffer-named "[copy<3>]")) (seat:buffer-name (copy)))
       '("*copy*<3>" "[copy<3>]" "[copy]"))
     (kill-buffer! (seat:buffer-store-id (seat:buffer-named "[copy<3>]")))
     (store:delete! other others)

     ;; a window showing *copy* follows the arriving text
     (seat:show-buffer-mirror! (seat:copy-buffer))
     (seat:goto! '(0 . 0))
     (copy-text! "l1\nl2\nl3")
     (check 'a-window-showing-the-copy-buffer-follows-the-arriving-text
       (list (eq? (seat:current-buffer-mirror) (copy)) (seat:point)) '(#t (2 . 2)))

     ;; a plain local buffer returns on resume with its text and facts; the
     ;; copy buffer lives in the base and needs no checkpoint
     (define notes (seat:new-local-buffer! "notes"))
     (seat:buffer-fact-set! notes 'note 7)
     (seat:buffer-lines-set! notes (vector "keep" "me"))
     (seat:add-buffer! notes)
     (copy-text! "before")
     (seat:checkpoint!)
     (seat:buffer-lines-set! notes (vector "lost"))
     (copy-text! "kept in the base")
     (seat:resume!)
     (check 'plain-local-buffers-return-on-resume-and-the-copy-buffer-stays-as-the-base-has-it
       (list (vector->list (seat:buffer-lines (seat:buffer-named "<notes>")))
             (seat:buffer-fact (seat:buffer-named "<notes>") 'note #f)
             (seat:copy-text) (eq? (seat:buffer-named "<notes>") notes) (eq? (copy) (seat:buffer-named "[copy]")))
       '(("keep" "me") 7 "kept in the base" #t #t))

     (test:finish! 'copy)))
