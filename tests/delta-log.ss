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

     ;; the browser opens in a window of the caller's choosing, the pop-up
     ;; here, and lists every shown buffer's entries under a heading, newest
     ;; first, the current row's text highlighted in the buffer's window;
     ;; its keys move, toggle the row's entry into a view shown where the
     ;; buffer was, commit the view and close the browser
     (define c (head:new-buffer! "browse-me"))
     (head:show-buffer! c)
     (head:goto! '(0 . 0))
     (insert-text! "x")
     (insert-text! "y")
     (insert-text! "z")
     (define (ordinary-windows) (filter (lambda (w) (not (head:popup? w))) (head:windows)))
     (define (window-showing name) (find (lambda (w) (equal? (head:buffer-name (head:window-buffer w)) name)) (ordinary-windows)))
     (define (marked-ranges) (map cdr (filter (lambda (r) (eq? (car r) c)) (paint:highlight-ranges))))
     (define (rows) (cdr (vector->list (head:buffer-lines (head:current-buffer)))))
     (define (inserted l) (cond [(contains? l "+\"z\"") 'z] [(contains? l "+\"y\"") 'y] [(contains? l "+\"x\"") 'x] [else #f]))
     (delta-log:open! 0)
     (head:before-frame!)
     (check 'the-browser-opens-in-the-pop-up-with-a-heading-and-a-row-per-entry
       (list (head:buffer-name (head:current-buffer)) (head:popup? (head:current-window)) (> (head:popup-rows) 0)
             (and (window-showing "browse-me") #t)
             (contains? (car (vector->list (head:buffer-lines (head:current-buffer)))) "Buffer")
             (map (lambda (l) (contains? l "browse-me")) (rows)) (map inserted (rows)) (head:point))
       (list "<delta-log>" #t #t #t #t '(#t #t #t) '(z y x) '(1 . 0)))
     (check 'the-current-rows-text-is-highlighted-in-the-buffer-and-point-is-on-it
       (list (marked-ranges) (head:buffer-point c)) '(((0 2 3 match)) (0 . 2)))
     ;; the browser's keys are its commands, bound in its mode's context, so
     ;; the keys listing shows them
     (check 'the-browsers-keys-are-commands-in-its-context
       (list (mode:key-contexts (head:current-buffer))
             (eq? (bound-to 'delta-log "M-n") delta-log:next!) (eq? (bound-to 'delta-log "RET") delta-log:choose!)
             (eq? (bound-to 'delta-log "M-t") delta-log:toggle-row!) (eq? (bound-to 'delta-log "M-RET") delta-log:commit!)
             (bound-to 'global "M-t"))
       '((delta-log) #t #t #t #t #f))
     (dispatch:key! "M-n")
     (head:before-frame!)
     (check 'm-n-moves-a-row-and-the-highlight-and-point-follow
       (list (head:point) (marked-ranges) (head:buffer-point c)) '((2 . 0) ((0 1 2 match)) (0 . 1)))
     (dispatch:key! "M-t")
     (head:before-frame!)
     (check 'm-t-toggles-the-rows-entry-into-a-view-shown-where-the-buffer-was
       (list (head:buffer-name (head:current-buffer)) (delta-log:view)
             (let ([w (window-showing "<view: browse-me>")]) (and w (vector->list (head:buffer-lines (head:window-buffer w)))))
             (contains? (cadr (rows)) "- "))
       (list "<delta-log>" '("browse-me" (2) ()) '("xz") #t))
     (dispatch:key! "M-RET")
     (head:before-frame!)
     (check 'm-ret-commits-the-view-and-the-buffer-returns
       (list (delta-log:view) (vector->list (head:buffer-lines c)) (and (window-showing "browse-me") #t) (length (rows)))
       (list #f '("xz") #t 4))
     (check 'ret-describes-the-row-and-stays (begin (dispatch:key! "RET") (head:point)) '(3 . 0))
     (delta-log:filter! '((count . 2)))
     (check 'a-filter-narrows-the-rows (length (rows)) 2)
     (dispatch:key! "ESC")
     (head:before-frame!)
     (check 'esc-closes-the-browser-hides-the-pop-up-and-clears-the-highlight
       (list (head:buffer-named "<delta-log>") (head:popup-rows) (paint:highlight-ranges))
       '(#f 0 ()))

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
     (delta-log:open! 0)
     (delta-log:filter! batch-id)
     (check 'the-browser-narrowed-to-the-batch-lists-its-entries
       (list (length (rows)) (contains? (list-ref (rows) 3) "0:4") (contains? (list-ref (rows) 3) "-\"old\"  +\"new\"")
             (for-all (lambda (l) (contains? l "replace-me")) (rows)))
       '(4 #t #t #t))
     (delta-log:filter! #f)
     (check 'a-false-filter-widens-the-rows-again (length (rows)) (+ before 4))
     (dispatch:key! "ESC")
     (head:show-buffer! d)
     (check 'undo-takes-the-replacement-back-as-one-step
       (begin (undo!) (vector->list (head:buffer-lines d))) '("one old two old" "three" "old four old five"))
     (define log-of-d (delta-log:log))
     (delta-log:open! 0)
     (check 'rows-name-the-batch-what-an-inverse-undoes-and-what-undid-an-entry
       (let* ([log log-of-d] [rows (rows)]
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

     ;; the browser goes where it is asked, the pop-up by C-x l, and a window
     ;; returns to what it showed when the browser closes: ESC leaves point
     ;; on the row's text, C-g puts it back where it stood
     (define f (head:new-buffer! "review-me"))
     (head:show-buffer! f)
     (head:goto! '(0 . 0))
     (insert-text! "old old")
     (window:delete-others!)
     (define fw (head:current-window))
     (define batch-f (begin (search:replace! "old" "new") (cdr (assq 'batch (caddr (car (delta-log:log)))))))
     (delta-log:open! 0)
     (delta-log:filter! batch-f)
     (check 'the-browser-in-the-pop-up-lists-the-replacement-and-the-buffer-follows
       (list (head:buffer-name (head:current-buffer)) (head:popup? (head:current-window)) (length (rows))
             (for-all (lambda (l) (and (contains? l "review-me") (contains? l "-\"old\"  +\"new\""))) (rows))
             (eq? (head:window-buffer fw) f) (head:buffer-point f))
       '("<delta-log>" #t 2 #t #t (0 . 4)))
     (dispatch:key! "C-g")
     (check 'c-g-closes-the-browser-and-puts-point-back
       (list (head:buffer-named "<delta-log>") (head:popup-rows) (head:buffer-point f) (eq? (head:current-window) fw)) '(#f 0 (0 . 7) #t))
     (delta-log:open!)
     (check 'the-browser-in-the-buffers-own-window-still-lists-it
       (list (eq? (head:window-buffer fw) (head:current-buffer)) (for-all (lambda (l) (contains? l "review-me")) (rows)) (> (length (rows)) 0))
       '(#t #t #t))
     (check 'switching-browsers-keeps-the-underlying-buffer-without-recursion
       (begin (delta-log:conflicts!) (delta-log:open!) (head:before-frame!)
              (list (> (length (rows)) 0) (for-all (lambda (l) (contains? l "review-me")) (rows))))
       '(#t #t))
     (dispatch:key! "ESC")
     (check 'esc-returns-the-window-to-its-buffer-and-leaves-point-on-the-rows-text
       (list (head:buffer-named "<delta-log>") (eq? (head:window-buffer fw) f) (head:buffer-point f)) '(#f #t (0 . 4)))

     ;; Resolving a buffer is independent of the other visible buffers;
     ;; committing picks is the explicit operation across the whole review.
     (define (conflicted name)
       (let ([id (store:create! head:ui-actor name '("base") '((base . "base") (trailing . #f)))])
         (store:edit! head:ui-actor id 0 (text:make-span 0 0 0 4) '("mine 界"))
         (store:reload! bot id '("disk 始") '((base . "disk 始") (trailing . #f)))
         (head:adopt-store-buffer! id)))
     (define g (conflicted "review-界"))
     (define h (conflicted "review-other"))
     (head:show-buffer! g)
     (window:split-right!)
     (head:show-buffer! h)
     (window:focus! (window-showing "review-界"))
     (delta-log:conflicts! 0)
     (define panel (head:current-window))
     (check 'conflicts-opens-on-the-invoking-buffer-not-the-first-visible-one
       (list (head:point) (contains? (head:buffer-line (head:current-buffer) (car (head:point))) "review-界"))
       '((2 . 0) #t))
     (define (click! row col)
       (parameterize ([head:app-event-buffer-position (cons row col)] [head:app-event-focus fw])
         (head:dispatch-app-event! "MOUSE-CLICK"))
       (head:before-frame!))
     (define (click-side! text)
       (let ([line (vector-ref (head:window-lines panel) 1)])
         (click! 1 (string:search line text 0 (string-length line)))))
     (define (click-all! label)
       (let ([line (vector-ref (head:window-lines panel) 0)])
         (click! 0 (+ 6 (string:search line label 0 (string-length line))))))
     (paint:window-layout)
     (head:before-frame!)
     (check 'headers-side-cells-and-settle-share-their-hover-and-click-ranges
       (map (lambda (target)
              (let* ([row (car target)] [line (vector-ref (head:window-lines panel) row)]
                     [col (+ (caddr target) (string:search line (cadr target) 0 (string-length line)))]
                     [position (paint:window-screen-position panel row col)])
                (head:set-mouse-position! (cons (cdr position) (car position)))
                (and (exists (lambda (r) (and (eq? (car r) panel) (= (cadr r) row)
                                              (<= (caddr r) col) (< col (cadddr r)) (eq? (list-ref r 4) 'hover)))
                             (paint:highlight-ranges)) #t)))
         '((0 "Mine (all)" 6) (1 "disk 始" 2) (3 "Settle" 1)))
       '(#t #t #t))
     (head:set-mouse-position! #f)
     (check 'header-and-shift-arrow-picks-cover-the-visible-review-without-writing
       (let ([sides
              (reverse
                (fold-left
                  (lambda (out action)
                    (action)
                    (cons (map (lambda (b) (map cdr (head:with-buffer b (delta-log:picks)))) (list g h)) out))
                  '() (list (lambda () (click-all! "Mine (all)")) (lambda () (dispatch:key! "S-RIGHT"))
                            (lambda () (dispatch:key! "S-LEFT")) (lambda () (click-all! "Disk (all)")))))])
         (list sides (map (lambda (b) (vector->list (head:buffer-lines b))) (list g h))))
       '((((mine) (mine)) ((disk) (disk)) ((mine) (mine)) ((disk) (disk))) (("disk 始") ("disk 始"))))
     (check 'resolve-all-without-a-choice-only-settles-the-current-buffer
       (list (head:with-buffer g (delta-log:resolve-all!))
             (length (store:conflicts (head:buffer-store-id h)))) '(1 1))
     (check 'mouse-side-picks-preview-without-writing-even-from-another-pane
       (begin (click-side! "mine 界")
              (let ([mine (head:with-buffer h (delta-log:picks))])
                (click-side! "disk 始")
                (list (map cdr mine) (map cdr (head:with-buffer h (delta-log:picks)))
                      (vector->list (head:buffer-lines h)) (length (store:conflicts (head:buffer-store-id h))))))
       '((mine) (disk) ("disk 始") 1))
     (click-side! "mine 界")
     (head:buffer-read-only-set! h #t)
     (check 'a-refused-commit-keeps-the-picks-for-retry
       (list (delta-log:commit-picks!) (map cdr (head:with-buffer h (delta-log:picks)))) '(0 (mine)))
     (head:buffer-read-only-set! h #f)
     (click! 2 1)
     (check 'clicking-settle-commits-the-picked-side
       (list (vector->list (head:buffer-lines h)) (store:conflicts (head:buffer-store-id h)))
       '(("mine 界") ()))

     ;; A foreign edit joining pending regions keeps one coherent Mine
     ;; image, which the bulk controls can preview and settle directly.
     (delta-log:close!)
     (check 'bulk-mine-keeps-the-complete-image-of-joined-regions
       (let ([overlap (store:create! bot "overlap" '("alpha beta gamma") '((base . "alpha beta gamma") (trailing . #f)))])
         (store:edit! head:ui-actor overlap 0 (text:make-span 0 11 0 16) '("G"))
         (store:edit! head:ui-actor overlap 1 (text:make-span 0 0 0 5) '("A"))
         (store:reload! bot overlap '("DISK beta DISK") '((base . "DISK beta DISK") (trailing . #f)))
         (head:show-buffer! (head:adopt-store-buffer! overlap))
         (delta-log:pick! 1 'mine)
         (delta-log:pick! 2 'disk)
         (store:edit! bot overlap (store:revision overlap) (text:make-span 0 0 0 14) '(""))
         ;; No frame between the edit and Settle: a reused revision cannot
         ;; expand an old Mine choice into the other region picked Disk.
         (check 'settlement-rechecks-the-reviewed-alternatives-before-writing
           (list (delta-log:resolve-all!) (store:line overlap 0) (map cdr (delta-log:picks)))
           '(0 "" (disk)))
         (head:show-buffer! (head:adopt-store-buffer! overlap))
         (list (delta-log:pick-all! 'mine) (delta-log:resolve-all! 'mine)
               (map cdr (delta-log:picks)) (vector->list (head:buffer-lines (head:current-buffer)))))
       '(1 1 () ("A beta G")))

     ;; The displayed record may change under the same ID, or disappear
     ;; into another group, between painting a row and activating its cell.
     (for-each
       (lambda (mouse?)
         (let ([id (store:create! bot "stale row" '("alpha beta gamma") '((base . "alpha beta gamma") (trailing . #f)))])
           (store:edit! head:ui-actor id 0 (text:make-span 0 11 0 16) '("G"))
           (store:edit! head:ui-actor id 1 (text:make-span 0 0 0 5) '("A"))
           (store:reload! bot id '("DISK beta DISK") '((base . "DISK beta DISK") (trailing . #f)))
           (head:show-buffer! (head:adopt-store-buffer! id))
           (delta-log:conflicts!)
           (head:before-frame!)
           (let* ([row (if mouse? 2 1)] [line (vector-ref (head:window-lines (head:current-window)) row)]
                  [col (string:search line (if mouse? "\"G\"" "\"A\"") 0 (string-length line))])
             (head:goto! (cons row 0))
             (store:edit! bot id (store:revision id) (text:make-span 0 0 0 14) '("custom"))
             (if mouse?
                 (parameterize ([head:app-event-buffer-position (cons row col)]) (head:dispatch-app-event! "MOUSE-CLICK"))
                 (delta-log:pick-mine!))
             (let* ([sides (map cdr (delta-log:picks))]
                    [shown? (contains? (vector-ref (head:buffer-lines (head:current-buffer)) 1) "A beta G")]
                    [settled (delta-log:commit-picks!)])
               (check 'stale-row-activation-refreshes-without-selecting-an-unseen-mine
                 (list sides shown? settled (store:line id 0)) '((disk) #t 1 "custom"))))
           (delta-log:close!)))
       '(#f #t))

     (let ([source (head:new-buffer! "live view")])
       (head:show-buffer! source)
       (head:buffer-lines-set! source '#("ac"))
       (head:goto! '(0 . 1))
       (insert-text! "b")
       (let ([id (head:buffer-store-id source)])
         (delta-log:toggle! (store:revision id))
         (store:edit! bot id (store:revision id) (text:make-span 0 3 0 3) '("x"))
         (head:before-frame!)
         (check 'a-live-view-shows-foreign-edits-before-commit
           (let* ([preview (head:buffer-lines (head:current-buffer))] [status (delta-log:commit!)])
             (list preview status (head:buffer-lines source))) '(#("acx") applied #("acx")))))
     (let ([id (store:create! bot "live picks" '("alpha" "beta") '((base . "alpha\nbeta") (trailing . #f)))])
       (store:edit! head:ui-actor id 0 (text:make-span 0 0 0 5) '("X"))
       (store:edit! head:ui-actor id 1 (text:make-span 1 0 1 4) '("Y"))
       (store:reload! bot id '("ALPHA" "BETA") '((base . "ALPHA\nBETA") (trailing . #f)))
       (head:show-buffer! (head:adopt-store-buffer! id))
       (let* ([cs (store:conflicts id)] [revision (store:revision id)])
         (delta-log:pick! (caadr cs) 'mine)
         (store:resolve! bot id (caar cs) 'disk)
         (head:before-frame!)
         (check 'preview-regions-follow-settlement-without-a-text-revision
           (list (= revision (store:revision id))
                 (map cdr (filter (lambda (r) (eq? (car r) (head:current-buffer))) (paint:highlight-ranges))))
           '(#t ((0 0 1 conflict-mine))))))

     (test:finish! 'delta-log)))
