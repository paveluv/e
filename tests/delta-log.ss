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
             (prefix (head window) window:)
             (prefix (state store) store:))

     (define check test:check)
     (define (contains? s part) (and (string:search s part 0 (string-length s)) #t))
     (define (bound-to context key) (let ([hit (keymap:resolved-binding context (list key))]) (and hit (keymap:binding-action (cdr hit)))))
     (search:review-replacements #f)
     (delta-log:init!)
     (define bot '(agent delta-test))
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
               (and (string:search (cdr (car offered)) "0:2  +\"c\"" 0 (string-length (cdr (car offered)))) #t)))
       '((3 2 1) #t))
     (check 'a-batch-completes-once-per-batch-with-its-count
       (let ([offered (edoc:type-completions 'batch "")])
         (list (length offered) (and (string:search (cdr (car offered)) "1 entry by" 0 (string-length (cdr (car offered)))) #t)))
       '(3 #t))

     ;; a toggle shows the view, the rest rebased over the disabled entry, in
     ;; a read-only local buffer in the window; further toggles re-render it
     (check 'toggling-shows-the-view-in-a-local-buffer
       (begin (delta-log:toggle! 2)
              (list (head:buffer-name (head:current-buffer)) (vector->list (head:buffer-lines (head:current-buffer)))
                    (head:buffer-read-only (head:current-buffer)) (delta-log:view) (head:buffer-store-id (head:current-buffer))))
       (list "<view: log-me>" '("ac") #t '("log-me" (2) ()) #f))
     (check 'toggling-again-changes-the-view-live
       (begin (delta-log:toggle! 3)
              (list (vector->list (head:buffer-lines (head:current-buffer))) (delta-log:disabled)))
       '(("a") (3 2)))
     (check 'the-view-buffer-shows-its-trunks-log (map car (delta-log:log)) '(3 2 1))
     (check 'the-trunk-is-untouched-by-the-view (vector->list (head:buffer-lines b)) '("abc"))
     (delta-log:toggle! 3)

     ;; a commit rewrites the trunk for everyone and ends the view
     (check 'committing-rewrites-the-trunk-and-ends-the-view
       (list (delta-log:commit!) (head:buffer-name (head:current-buffer)) (vector->list (head:buffer-lines b)) (delta-log:view)
             (map (lambda (row) (list (car row) (list-ref row 5))) (store:log id))
             (list-ref (car (store:log id)) 4))
       (list 'applied "log-me" '("ac") #f '((4 enabled) (3 enabled) (2 disabled) (1 enabled)) (list 'rewrite head:ui-actor 4 2)))
     (check 'committing-without-a-view-is-refused (guard (ex [else 'refused]) (delta-log:commit!)) 'refused)

     ;; a later entry overlapping a disabled one is a conflict: the view names
     ;; it, shows the text as it stands, and a commit is blocked until the
     ;; view is abandoned
     (store:edit! bot id (store:revision id) (text:make-span 0 0 0 2) '("Q"))
     (head:before-frame!)
     (check 'the-foreign-edit-arrived (vector->list (head:buffer-lines b)) '("Q"))
     (check 'a-conflicting-toggle-names-the-later-entry
       (begin (delta-log:toggle! 1)
              (list (delta-log:view) (vector->list (head:buffer-lines (head:current-buffer)))))
       (list '("log-me" (1) ((1 . 5))) '("Q")))
     (check 'a-conflicted-commit-is-blocked (delta-log:commit!) 'blocked)
     (check 'the-view-stays-until-abandoned (and (delta-log:view) #t) #t)
     (delta-log:revert!)
     (check 'abandoning-returns-to-the-trunk (list (head:buffer-name (head:current-buffer)) (delta-log:view)) '("log-me" #f))
     (check 'toggling-every-disabled-entry-back-ends-the-view
       (begin (delta-log:toggle! 5) (delta-log:toggle! 5) (list (delta-log:view) (head:buffer-name (head:current-buffer))))
       '(#f "log-me"))

     ;; previewing a revision brings its span under point and highlights it;
     ;; the thunk undoes both
     (head:goto! '(0 . 1))
     (check 'a-preview-highlights-the-entrys-span-and-undoes-itself
       (let* ([restore ((edoc:type-preview 'revision) 5)]
              [during (list (head:point) (map (lambda (r) (if (eq? (car r) b) (cdr r) r)) (paint:highlight-ranges)))])
         (restore)
         (list (and (procedure? restore) #t) during (head:point) (paint:highlight-ranges)))
       (list #t '((0 . 0) ((0 0 1 match))) '(0 . 1) '()))
     (check 'a-revision-outside-the-log-has-no-preview ((edoc:type-preview 'revision) 99) #f)
     (check 'show-describes-an-entry (begin (delta-log:show! 5) 'shown) 'shown)
     (check 'show-refuses-a-missing-entry (guard (ex [else 'refused]) (delta-log:show! 99)) 'refused)

     ;; the browser lists a fresh buffer's entries beside it, newest first,
     ;; the current row's text highlighted in the buffer's window; its keys
     ;; move, toggle the row's entry into a view shown where the buffer was,
     ;; commit the view and close the browser
     (define c (head:new-buffer! "browse-me"))
     (head:show-buffer! c)
     (head:goto! '(0 . 0))
     (insert-text! "x")
     (insert-text! "y")
     (insert-text! "z")
     (define (ordinary-windows) (filter (lambda (w) (not (head:popup? w))) (head:windows)))
     (define (window-showing name) (find (lambda (w) (equal? (head:buffer-name (head:window-buffer w)) name)) (ordinary-windows)))
     (define (marked-ranges) (map (lambda (r) (if (eq? (car r) c) (cdr r) r)) (paint:highlight-ranges)))
     (delta-log:open!)
     (head:before-frame!)
     (check 'the-browser-opens-beside-the-buffer-with-a-row-per-entry
       (list (head:buffer-name (head:current-buffer)) (length (ordinary-windows)) (and (window-showing "browse-me") #t)
             (map (lambda (l) (substring l 0 3)) (vector->list (head:buffer-lines (head:current-buffer)))) (head:point))
       (list "<delta-log>" 2 #t '("  3" "  2" "  1") '(0 . 0)))
     (check 'the-current-rows-text-is-highlighted-in-the-buffer-and-point-is-on-it
       (list (marked-ranges) (head:buffer-point c)) '(((0 2 3 match)) (0 . 2)))
     ;; the browser's keys are its commands, bound in its mode's context and
     ;; the entries' state context, so the keys listing shows them
     (check 'the-browsers-keys-are-commands-in-its-contexts
       (list (mode:key-contexts (head:current-buffer))
             (eq? (bound-to 'delta-log "M-n") delta-log:next!) (eq? (bound-to 'delta-log "RET") delta-log:show-row!)
             (eq? (bound-to 'delta-log-entries "M-t") delta-log:toggle-row!) (eq? (bound-to 'delta-log-entries "M-RET") delta-log:commit!)
             (bound-to 'global "M-t"))
       '((delta-log-entries delta-log) #t #t #t #t #f))
     (dispatch:key! "M-n")
     (head:before-frame!)
     (check 'm-n-moves-a-row-and-the-highlight-and-point-follow
       (list (head:point) (marked-ranges) (head:buffer-point c)) '((1 . 0) ((0 1 2 match)) (0 . 1)))
     (dispatch:key! "M-t")
     (check 'm-t-toggles-the-rows-entry-into-a-view-shown-where-the-buffer-was
       (list (head:buffer-name (head:current-buffer)) (delta-log:view)
             (let ([w (window-showing "<view: browse-me>")]) (and w (vector->list (head:buffer-lines (head:window-buffer w)))))
             (substring (vector-ref (head:buffer-lines (head:current-buffer)) 1) 0 3))
       (list "<delta-log>" '("browse-me" (2) ()) '("xz") "- 2"))
     (dispatch:key! "M-RET")
     (check 'm-ret-commits-the-view-and-the-buffer-returns
       (list (delta-log:view) (vector->list (head:buffer-lines c)) (and (window-showing "browse-me") #t)
             (map (lambda (l) (substring l 0 3)) (vector->list (head:buffer-lines (head:current-buffer)))))
       (list #f '("xz") #t '("  4" "  3" "  2" "  1")))
     (dispatch:key! "M-p")
     (check 'ret-describes-the-row (begin (dispatch:key! "RET") (head:point)) '(0 . 0))
     (delta-log:filter! '((count . 2)))
     (check 'a-filter-narrows-the-rows (length (vector->list (head:buffer-lines (head:current-buffer)))) 2)
     (dispatch:key! "ESC")
     (head:before-frame!)
     (check 'esc-closes-the-browser-and-clears-the-highlight
       (list (head:buffer-named "<delta-log>") (paint:highlight-ranges))
       '(#f ()))

     ;; a replacement is one entry per occurrence under one batch, in the
     ;; text's order and one undo step; the log and the browser take the
     ;; batch alone as a selector, and it completes newest first
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
     (check 'one-entry-per-occurrence-in-the-texts-order-under-one-batch
       (list (- (length (delta-log:log)) before) (map (lambda (r) (cadddr r)) (reverse added))
             (for-all (lambda (r) (equal? (cdr (assq 'batch (caddr r))) batch-id)) added))
       (list 4 '(((0 4 0 7) ("old") ("new")) ((0 12 0 15) ("old") ("new")) ((2 0 2 3) ("old") ("new")) ((2 9 2 12) ("old") ("new"))) #t))
     (check 'the-log-takes-the-batch-alone-as-its-selector (length (delta-log:log batch-id)) 4)
     (check 'the-batch-completes-newest-first-with-its-count
       (let ([offered (edoc:type-completions 'batch "")])
         (list (equal? (car (car offered)) batch-id) (contains? (cdr (car offered)) "4 entries")))
       '(#t #t))
     (delta-log:open! batch-id)
     (check 'the-browser-opens-on-the-batchs-entries
       (list (length (vector->list (head:buffer-lines (head:current-buffer))))
             (contains? (vector-ref (head:buffer-lines (head:current-buffer)) 3) "0:4  -\"old\"  +\"new\""))
       '(4 #t))
     (delta-log:filter! #f)
     (check 'a-false-filter-widens-the-rows-again (length (vector->list (head:buffer-lines (head:current-buffer)))) (+ before 4))
     (dispatch:key! "ESC")
     (head:show-buffer! d)
     (check 'undo-takes-the-replacement-back-as-one-step
       (begin (undo!) (vector->list (head:buffer-lines d))) '("one old two old" "three" "old four old five"))
     (delta-log:open!)
     (check 'rows-name-the-batch-what-an-inverse-undoes-and-what-undid-an-entry
       (let* ([log (delta-log:log)] [rows (vector->list (head:buffer-lines (head:current-buffer)))]
              [target (list-ref (list-ref (car log) 4) 3)]
              [at (let find ([i 0] [l log]) (if (= (car (car l)) target) i (find (+ i 1) (cdr l))))])
         ;; an inverse carries no batch of its own; the original names its batch
         (list (contains? (car rows) (format "undoes ~a" target)) (contains? (list-ref rows at) "  batch ")
               (contains? (list-ref rows at) (format "undone by ~a" (car (car log))))))
       '(#t #t #t))
     (dispatch:key! "ESC")
     (head:show-buffer! d)

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

     ;; a replacement opens the browser on its batch in the companion below
     ;; the buffer's window; the next one finds that window as the sibling
     ;; below and reuses it instead of splitting again
     (define f (head:new-buffer! "review-me"))
     (head:show-buffer! f)
     (head:goto! '(0 . 0))
     (insert-text! "old old")
     (window:delete-others!)
     (define fw (head:current-window))
     (define (rows) (vector->list (head:buffer-lines (head:current-buffer))))
     (define (below? upper lower)
       ;; lower is in the lower half of the split upper is the upper half of
       (let ([p (head:layout-parent (head:root) upper)])
         (and p (eq? (head:layout-split-orientation p) 'below) (eq? (head:layout-split-first p) upper)
              (memq lower (head:layout-leaves (head:layout-split-second p))) #t)))
     (define windows-before (length (ordinary-windows)))
     (parameterize ([search:review-replacements #t]) (search:replace! "old" "new"))
     (check 'a-replacement-opens-the-browser-on-its-batch-below-the-buffer
       (list (head:buffer-name (head:current-buffer)) (length (rows)) (for-all (lambda (l) (contains? l "-\"old\"  +\"new\"")) (rows))
             (below? fw (head:current-window)) (eq? (head:window-buffer fw) f)
             (- (length (ordinary-windows)) windows-before) (head:buffer-point f))
       '("<delta-log>" 2 #t #t #t 1 (0 . 4)))
     (dispatch:key! "C-g")
     (check 'c-g-closes-the-browser-and-puts-point-back
       (list (head:buffer-named "<delta-log>") (head:buffer-point f) (eq? (head:current-window) fw)) '(#f (0 . 7) #t))
     (parameterize ([search:review-replacements #t]) (search:replace! "new" "old"))
     (check 'the-next-replacement-reuses-the-companion
       (list (head:buffer-name (head:current-buffer)) (length (rows)) (contains? (car (rows)) "-\"new\"  +\"old\"")
             (- (length (ordinary-windows)) windows-before))
       '("<delta-log>" 2 #t 1))
     (dispatch:key! "ESC")
     (check 'esc-leaves-point-on-the-rows-text (list (head:buffer-named "<delta-log>") (head:buffer-point f)) '(#f (0 . 4)))

     (test:finish! 'delta-log)))
