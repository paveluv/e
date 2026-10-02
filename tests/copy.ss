#!/usr/bin/env scheme-script

;; Clipboard identity is base-owned; text, undo and selection use ordinary APIs.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (prefix (test) test:) (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head widget) widget:)
             (prefix (head interaction) interaction:) (prefix (service clipboard) clipboard:)
             (prefix (state store) store:) (prefix (state actor) actor:)
             (prefix (state view) view:) (prefix (state model) model:))
     (widget:init!) (edit:init!)
     (define (copy) (actor:call-as head:ui-actor (lambda () (clipboard:open! #f))))
     (define (with-editor document proc)
       (let ([id (edit:create-view! head:ui-actor document '())])
         (widget:mount! id 'copy-fixture)
         (widget:pump!) (widget:present! (list (list (widget:prepare! id 30 4) 0 0)))
         (dynamic-wind void (lambda () (proc id))
           (lambda () (widget:unmount! id) (view:retire! head:ui-actor id (model:revision id))))))
     (test:check 'copy-is-lazy (list (copy) (edit:copy-text)) '(#f ""))
     (edit:copy-text! "abc\n")
     (define id (copy))
     (test:check 'copy-uses-an-actor-owned-document-without-a-seat-mirror
       (list (store:property id 'copy #f) (store:property id 'audience #f)
         (store:property id 'disposable #f) (seat:buffer-of-store-id id)
         (edit:copy-text) (length (store:log id)))
       (list #t (list head:ui-actor) #t #f "abc\n" 1))
     (test:check 'copies-round-trip-exactly
       (map (lambda (s) (edit:copy-text! s) (edit:copy-text)) '("" "\n" "a" "a\n\nb" "a\n\n"))
       '("" "\n" "a" "a\n\nb" "a\n\n"))
     (with-editor (store:create! head:ui-actor "copy target" '("xy"))
       (lambda (editor)
         (edit:select! editor '(0 . 1) '(0 . 1)) (edit:copy-text! "1\n2\n") (edit:yank! editor)
         (test:check 'yank-preserves-line-breaks-and-settles-point
           (let-values ([(lines revision) (store:snapshot (view:source (interaction:snapshot editor)))])
             (list lines (car (view:state (interaction:snapshot editor))))) '(#("x1" "2" "y") (2 . 0)))))
     (with-editor (store:create! head:ui-actor "copy source" '("one" "two" "three"))
       (lambda (editor)
         (edit:kill-line! editor)
         (head:set-last-command! edit:kill-line!) (edit:kill-line! editor)
         (head:set-last-command! edit:kill-line!) (edit:kill-line! editor)
         (test:check 'consecutive-kills-accumulate (edit:copy-text) "one\ntwo")))
     (with-editor id
       (lambda (editor)
         (edit:copy-text! "first") (edit:copy-text! "second\nlast")
         (test:check 'shown-copy-follows-the-new-text
           (car (view:state (interaction:snapshot editor))) '(1 . 4))
         (edit:undo! editor)
         (let ([before (edit:copy-text)])
           (edit:redo! editor)
           (test:check 'copy-undo-redo-preserves-trailing-newlines (list before (edit:copy-text)) '("first" "second\nlast")))))
     (test:check 'reopening-never-resets-the-copy-journal
       (let ([before (store:log id)])
         (list (actor:call-as head:ui-actor (lambda () (clipboard:open! #t)))
           (equal? before (store:log id)))) (list id #t))
     (store:delete! head:ui-actor id) (edit:copy-text! "again")
     (test:check 'deleting-the-copy-document-recreates-it
       (list (equal? (copy) id) (edit:copy-text) (length (store:log (copy)))) '(#f "again" 1))
     (define other (actor:call-as '(head "another copier") (lambda () (clipboard:open! #t))))
     (test:check 'another-head-has-an-independent-copy
       (list (equal? other (copy)) (store:property other 'audience #f) (edit:copy-text))
       '(#f ((head "another copier")) "again"))
     (test:finish! 'copy)))
