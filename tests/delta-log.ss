#!/usr/bin/env scheme-script

;; The delta log at M-x: a buffer's entries as data, a view with entries
;; disabled shown live in a local buffer, committed as a rewrite of the
;; trunk or abandoned, the revision type completing from the log and
;; previewing an entry's span. Headless, one head over the base store.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (apps delta-log) delta-log:)
             (prefix (apps search) search:)
             (prefix (foundation edoc) edoc:)
             (prefix (foundation string) string:)
             (prefix (foundation text) text:)
             (prefix (head dispatch) dispatch:)
             (prefix (head head) head:)
             (prefix (head keymap) keymap:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:)
             (prefix (head window) window:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:)
             (prefix (state store) store:))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (define (bound-to context key) (let ([hit (keymap:resolved-binding context (list key))]) (and hit (keymap:binding-action (cdr hit)))))
     (delta-log:init!)
     (define bot '(agent delta-test))
     (include "tests/rewrite-draft.sps")
     (include "tests/conflict-review.sps")
     (include "tests/change-preview.sps")
     (define b (head:new-buffer! "log-me"))
     (head:show-buffer! b)
     (head:goto! '(0 . 0))
     (define id (head:buffer-store-id b))
     (insert-text! "a")
     (insert-text! "b")
     (insert-text! "c")

     ;; the log as data, for the current buffer, narrowed by a selector
     (check 'the-log-lists-the-buffers-entries-newest-first (map car (delta-log:log)) '(3 2 1))
     (check 'a-selector-narrows-the-log (map car (delta-log:log '((count . 1)))) '(3))
     (check 'a-local-buffer-has-no-log
       (guard (ex [else 'refused]) (head:with-buffer (head:new-local-buffer! "loose") (delta-log:log)))
       'refused)

     ;; the revision type completes from the log, each entry hinted with its
     ;; actor, place and text; the batch type from the labels
     (check 'a-revision-completes-from-the-log-with-a-hint
       (let ([offered (edoc:type-completions 'revision "")])
         (list (map car offered)
               (and (string:search (cdr (car offered)) "+\"c\"" 0 (string-length (cdr (car offered)))) #t)))
       '((3 2 1) #t))
     (check 'a-batch-completes-once-per-batch-with-its-count
       (let ([offered (edoc:type-completions 'batch "")])
         (list (length offered) (and (string:search (cdr (car offered)) "Edits by" 0 (string-length (cdr (car offered)))) #t)))
       '(3 #t))

     ;; Replacement rebasing and grouping are independent of the browser.
     (define d (head:new-buffer! "replace-me"))
     (head:show-buffer! d)
     (head:goto! '(0 . 0))
     (insert-text! "one old two old\nthree\nold four old five")
     (head:goto! '(0 . 9))
     (define before (length (delta-log:log)))
     (check 'a-replacement-replaces-every-occurrence-and-keeps-point
       (list (search:replace! "old" "new") (vector->list (head:buffer-lines d)) (head:point))
       (list 4 '("one new two new" "three" "new four new five") '(0 . 9)))
     (define added (list-head (delta-log:log) 4))
     (define batch-id (cdr (assq 'batch (caddr (car added)))))
     (check 'reverse-order-rewrite-logs-each-range-under-one-batch
       (list (- (length (delta-log:log)) before) (map (lambda (r) (cadddr r)) added)
             (for-all (lambda (r) (equal? (cdr (assq 'batch (caddr r))) batch-id)) added))
       (list 4 '(((0 4 0 7) ("old") ("new")) ((0 12 0 15) ("old") ("new")) ((2 0 2 3) ("old") ("new")) ((2 9 2 12) ("old") ("new"))) #t))
     (check 'the-log-takes-the-batch-alone-as-its-selector (length (delta-log:log batch-id)) 4)
     (check 'undo-takes-the-replacement-back-as-one-step
       (begin (undo!) (vector->list (head:buffer-lines d))) '("one old two old" "three" "old four old five"))
     ;; another actor's edit landing after the first occurrence's edit moves
     ;; the remaining occurrences, which are still replaced where they are
     (define e (head:new-buffer! "replace-under"))
     (head:show-buffer! e)
     (head:goto! '(0 . 0))
     (insert-text! "old and old and old")
     (define fired #f)
     (store:subscribe! (head:buffer-store-id e)
       (lambda (event)
         (when (and (not fired) (eq? (car event) 'edit) (equal? (list-ref event 3) head:ui-actor))
           (set! fired #t)
           (store:edit! bot (head:buffer-store-id e) (store:revision (head:buffer-store-id e)) (text:make-span 0 0 0 0) '("Q ")))))
     (check 'a-foreign-edit-between-occurrences-moves-the-rest
       (list (search:replace! "old" "new") (vector->list (head:buffer-lines e)))
       (list 3 '("Q new and new and new")))

     (include "tests/review-widget.sps")

     (test:finish! 'delta-log)))
