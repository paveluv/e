#!/usr/bin/env scheme-script

;; The delta log at M-x: a buffer's entries as data, a view with entries
;; disabled shown live in a local buffer, committed as a rewrite of the
;; trunk or abandoned, the revision type completing from the log and
;; previewing an entry's span. Headless, one head over the base store.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (test) test:) (prefix (core kernel) kernel:) (prefix (apps delta-log) delta-log:)
       (prefix (apps search) search:) (prefix (foundation edoc) edoc:) (prefix (foundation string) string:)
       (prefix (foundation text) text:) (prefix (head routing) routing:) (prefix (head head) head:)
       (prefix (head keymap) keymap:) (prefix (head mode) mode:)
       (prefix (head widget) widget:) (prefix (state store) store:))
     (define check test:check)
     (include "tests/editor-fixture.sps")
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (define (bound-to context key)
       (let ([hit (keymap:resolved-binding context (list key))])
         (and hit (keymap:binding-action (cdr hit)))))
     (delta-log:init!)
     (define bot '(agent delta-test))
     (include "tests/rewrite-draft.sps")
     (include "tests/conflict-review.sps")
     (include "tests/change-preview.sps")
     (define b (store:create! head:ui-actor "log-me" '("") '((trailing . #t))))
     (show! b)
     (goto! '(0 . 0))
     (define id b)
     (edit:paste! editing "a")
     (edit:paste! editing "b")
     (edit:paste! editing "c")
     (check
       'the-log-lists-the-buffers-entries-newest-first
       (map car (delta-log:log (view:source (interaction:snapshot editing))))
       '(3 2 1))
     (check
       'a-selector-narrows-the-log
       (map car (delta-log:log (view:source (interaction:snapshot editing)) '((count . 1))))
       '(3))
     (check
       'a-revision-completes-from-the-log-with-a-hint
       (let ([offered (parameterize ([widget:target editing]) (edoc:type-completions 'revision ""))])
         (list
           (map car offered)
           (and (string:search (caddr (car offered)) "+\"c\"" 0 (string-length (caddr (car offered)))) #t)))
       '((3 2 1) #t))
     (check
       'a-batch-completes-once-per-batch-with-its-count
       (let ([offered (parameterize ([widget:target editing]) (edoc:type-completions 'batch ""))])
         (list
           (length offered)
           (and (string:search (caddr (car offered)) "Edits by" 0 (string-length (caddr (car offered)))) #t)))
       '(3 #t))
     (define d (store:create! head:ui-actor "replace-me" '("") '((trailing . #t))))
     (show! d)
     (goto! '(0 . 0))
     (edit:insert! editing "one old two old\nthree\nold four old five")
     (goto! '(0 . 9))
     (define before (length (delta-log:log (view:source (interaction:snapshot editing)))))
     (check
       'a-replacement-replaces-every-occurrence-and-keeps-point
       (list (search:replace! editing "old" "new") (vector->list (lines-of d)) (point))
       (list 4 '("one new two new" "three" "new four new five") '(0 . 9)))
     (define added (list-head (delta-log:log (view:source (interaction:snapshot editing))) 4))
     (define batch-id (cdr (assq 'batch (caddr (car added)))))
     (check
       'reverse-order-rewrite-logs-each-range-under-one-batch
       (list
         (- (length (delta-log:log (view:source (interaction:snapshot editing)))) before)
         (map (lambda (r) (cadddr r)) added)
         (for-all (lambda (r) (equal? (cdr (assq 'batch (caddr r))) batch-id)) added))
       (list
         4
         '(((0 4 0 7) ("old") ("new"))
           ((0 12 0 15) ("old") ("new"))
           ((2 0 2 3) ("old") ("new"))
           ((2 9 2 12) ("old") ("new")))
         #t))
     (check
       'the-log-takes-the-batch-alone-as-its-selector
       (length (delta-log:log (view:source (interaction:snapshot editing)) batch-id))
       4)
     (check
       'undo-takes-the-replacement-back-as-one-step
       (begin (edit:undo! editing) (vector->list (lines-of d)))
       '("one old two old" "three" "old four old five"))
     (define e (store:create! head:ui-actor "replace-under" '("") '((trailing . #t))))
     (show! e)
     (goto! '(0 . 0))
     (edit:insert! editing "old and old and old")
     (define fired #f)
     (store:subscribe!
       e
       (lambda (event)
         (when (and (not fired) (eq? (car event) 'edit) (equal? (list-ref event 3) head:ui-actor))
           (set! fired #t)
           (store:edit! bot e (store:revision e) (text:make-span 0 0 0 0) '("Q ")))))
     (check
       'a-foreign-edit-between-occurrences-moves-the-rest
       (list (search:replace! editing "old" "new") (vector->list (lines-of e)))
       (list 3 '("Q new and new and new")))
     (include "tests/review-widget.sps")
     (test:finish! 'delta-log)))
