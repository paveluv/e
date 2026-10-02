#!/usr/bin/env scheme-script

;; Command intent, adopted text, and all head anchors share one revision,
;; including reentrant notifications and callbacks computing a proposal.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head window-host) window-host:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:)
             (prefix (apps search) search:)
             (prefix (service search-request) search-request:)
             (prefix (state model) model:) (prefix (state view) view:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (head mode) mode:)
             (prefix (core kernel) kernel:))

     (define bot '(agent position-test))
     (define check test:check)
     (include "tests/search-request.sps")
     (widget:init!) (edit:init!) (window-host:init!)
     (store:log-retention 256)   ; the bound these checks exercise
     (define (fresh name lines . local?)
       (let ([b ((if (and (pair? local?) (car local?)) seat:new-local-buffer! seat:new-buffer!) name)])
         (seat:buffer-lines-set! b (list->vector lines))
         (seat:show-buffer-mirror! b)
         (seat:goto! '(0 . 0))
         b))
     (define (text-of b) (vector->list (seat:buffer-lines b)))
     (define (foreign! b span replacement)
       (let ([id (seat:buffer-store-id b)])
         (store:edit! bot id (store:revision id) span replacement)))
     (define (wpoint w) (cons (seat:window-prow w) (seat:window-pcol w)))
     (define (bmark b) (cons (seat:buffer-mark-row b) (seat:buffer-mark-col b)))
     (define (refused? thunk)
       (guard (ex [(kernel:refusal? ex) #t] [else (raise ex)]) (thunk) #f))
     (define (after-own! b thunk)
       (store:subscribe! (seat:buffer-store-id b)
         (lambda (event)
           (when (and (eq? (car event) 'edit) (equal? (list-ref event 3) head:ui-actor))
             (thunk)))))

     ;; A missed multiline insertion precedes this head's character.
     ;; Nonselected windows and saved/selection positions must follow too.
     (define b (fresh "position-before" '("abcdef" "tail")))
     (define w (seat:current-window))
     (define w2 (seat:make-window b 1 0 0 1 2 12 80 80 'default))
     (define w3 (seat:make-window b 0 0 0 0 5 12 80 80 'default))
     (seat:set-windows! (append (seat:windows) (list w2 w3)))
     (seat:buffer-spot-row-set! b 1)
     (seat:buffer-spot-col-set! b 1)
     (seat:buffer-spot-top-set! b 1)
     (seat:buffer-mark-row-set! b 1)
     (seat:buffer-mark-col-set! b 1)
     (seat:buffer-marked-set! b #t)
     (seat:goto! '(0 . 2))
     (foreign! b (text:make-span 0 0 0 0) '("pre" "Q"))
     (insert-text! "X")
     (check 'command-follows-its-rebased-insertion (seat:point) '(1 . 4))
     (check 'both-texts-survive (text-of b) '("pre" "QabXcdef" "tail"))
     (check 'other-window-follows-lines (wpoint w2) '(2 . 2))
     (check 'other-window-follows-own-character (wpoint w3) '(1 . 7))
     (check 'other-viewport-follows-content (seat:window-top w2) 2)
     (check 'saved-point-follows-content
            (cons (seat:buffer-spot-row b) (seat:buffer-spot-col b)) '(2 . 1))
     (check 'saved-viewport-follows-content (seat:buffer-spot-top b) 2)
     (check 'completed-insertion-collapses-the-view-mark (bmark b) '(1 . 4))
     (head:before-frame!)
     (check 'queued-events-do-not-shift-again (seat:point) '(1 . 4))
     (check 'published-point-matches-adopted-point
            (store:mark head:ui-actor (seat:buffer-store-id b) 'point) (seat:point))

     ;; The store's edit insertion bias at a replacement's left boundary
     ;; differs from ordinary cursor rebasing.  Place after OUR character.
     (define boundary (fresh "position-boundary" '("abcdef")))
     (seat:goto! '(0 . 2))
     (foreign! boundary (text:make-span 0 2 0 4) '("R"))
     (insert-text! "X")
     (check 'insertion-before-foreign-replacement (text-of boundary) '("abXRef"))
     (check 'point-stays-after-own-character (seat:point) '(0 . 3))
     (head:before-frame!)
     (check 'boundary-point-stays-stable (seat:point) '(0 . 3))

     ;; A subscriber writes and adopts a newer snapshot before the outer
     ;; edit even returns.  Neither text nor anchors may replay a prefix.
     (define reentrant (fresh "position-reentrant" '("abcdef")))
     (seat:goto! '(0 . 2))
     (define token
       (after-own! reentrant
         (lambda ()
           (foreign! reentrant (text:make-span 0 0 0 0) '("G"))
           (head:before-frame!))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'reentrant-adoption-keeps-current-text (text-of reentrant) '("GabXcdef"))
     (check 'reentrant-adoption-places-once (seat:point) '(0 . 4))
     (head:before-frame!)
     (check 'reentrant-queued-events-do-not-repeat (seat:point) '(0 . 4))

     ;; Dynamic edit guards are head-local callbacks; shared facts are data.
     ;; A guard changing local source after capture must not retarget intent.
     ;; A receipt must retain a chain that was complete on acceptance,
     ;; even if this very commit trims its oldest entry out of the log.
     (define retained (fresh "position-retention-boundary" '("abcdef")))
     (seat:goto! '(0 . 2))
     ;; These writes stay ahead of the head's adopted command basis.
     (do ([i 0 (+ i 1)]) ((= i 256))
       (foreign! retained (text:make-span 0 0 0 0) '("q")))
     (insert-text! "X")
     (check 'acceptance-retains-the-whole-chain (seat:point) '(0 . 259))
     (check 'acceptance-at-retention-boundary-keeps-text
            (substring (car (text-of retained)) 256 263) "abXcdef")
     (head:before-frame!)
     (check 'retention-boundary-events-do-not-shift-again (seat:point) '(0 . 259))

     ;; A reset after our commit: the cursor crosses it on the reset's line
     ;; diff, its line replaced whole, to the replacement's end, ab|cdef ->
     ;; abX|cdef -> other|; the command must not write its old new-end (0 . 3).
     (define reset (fresh "position-reset-after-commit" '("abcdef")))
     (seat:goto! '(0 . 2))
     (set! token
       (after-own! reset
         (lambda () (store:reset! bot (seat:buffer-store-id reset) '("other")))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'post-commit-reset-keeps-latest-text (text-of reset) '("other"))
     (check 'post-commit-reset-carries-the-cursor-not-the-old-new-end (seat:point) '(0 . 5))

     (define truncated (fresh "position-truncated-after-commit" '("abcdef")))
     (seat:goto! '(0 . 2))
     (set! token
       (after-own! truncated
         (lambda ()
           (do ([i 0 (+ i 1)]) ((= i 257))
             (foreign! truncated (text:make-span 0 0 0 0) '("q"))))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'post-commit-truncation-clamps-without-partial-replay (seat:point) '(0 . 2))
     (check 'post-commit-truncation-keeps-all-text
            (substring (car (text-of truncated)) 257 264) "abXcdef")

     ;; Structural commands place their declared endpoint, not a line/
     ;; column computed before the store rebased the actual range.
     (define structural (fresh "position-structural" '("abcdef")))
     (seat:goto! '(0 . 2))
     (foreign! structural (text:make-span 0 0 0 0) '("Q"))
     (open-line!)
     (check 'open-line-keeps-before-inserted-break (seat:point) '(0 . 3))
     (check 'open-line-splits-the-rebased-position (text-of structural) '("Qab" "cdef"))
     (seat:goto! '(1 . 0))
     (foreign! structural (text:make-span 0 0 0 0) '("R"))
     (backspace!)
     (check 'backspace-follows-rebased-join (seat:point) '(0 . 4))
     (check 'backspace-preserves-foreign-prefix (text-of structural) '("RQabcdef"))
     (foreign! structural (text:make-span 0 0 0 0) '("S"))
     (replace-region-text! '(0 . 2) '(0 . 4) "x\ny")
     (check 'replacement-follows-rebased-end (seat:point) '(1 . 1))
     (check 'explicit-range-addresses-current-mirrored-text (text-of structural) '("SRx" "ybcdef"))

     ;; Formatter output and its point keep their original snapshot even
     ;; when the provider pumps a frame and an after-commit observer does too.
     (mode:register! "position-format" '() '() (lambda (line) #f))
     (define formatted (fresh "position-format-source" '("abc" "tail")))
     (seat:with-buffer-mirror formatted (mode:choose! "position-format"))
     (seat:goto! '(0 . 2))
     (mode:register-formatter! "position-format"
       (lambda (b from to)
         (foreign! formatted (text:make-span 0 0 0 0) '("Q"))
         (head:before-frame!)
         '("ABC" "tail")))
     (set! token
       (after-own! formatted
         (lambda ()
           (foreign! formatted (text:make-span 0 0 0 0) '("R"))
           (head:before-frame!))))
     (format-buffer!)
     (store:unsubscribe! token)
     (check 'format-retains-both-foreign-edits (text-of formatted) '("RQABC" "tail"))
     (check 'format-projects-its-preserved-point (seat:point) '(0 . 4))
     (undo!)
     (check 'format-undo-keeps-both-foreign-edits (text-of formatted) '("RQabc" "tail"))

     (define conflict (fresh "position-format-conflict" '("abc")))
     (seat:with-buffer-mirror conflict (mode:choose! "position-format"))
     (mode:register-formatter! "position-format"
       (lambda (b from to)
         (foreign! conflict (text:make-span 0 1 0 2) '("R"))
         (head:before-frame!)
         '("ABC")))
     (check 'format-refuses-even-after-head-adoption (refused? format-buffer!) #t)
     (check 'format-cannot-overwrite-adopted-foreign-text (text-of conflict) '("aRc"))
     (check 'format-refusal-records-no-head-action
            (filter (lambda (group) (equal? (car group) head:ui-actor)) (store:undo-labels (seat:buffer-store-id conflict))) '())

     (define indented (fresh "position-indent-source" '("  abc" "tail")))
     (seat:with-buffer-mirror indented (mode:choose! "position-format"))
     (seat:goto! '(1 . 2))
     (set-mark-command!)
     (seat:goto! '(0 . 3))
     (mode:register-indenter! "position-format"
       (lambda (b from to)
         (foreign! indented (text:make-span 0 0 0 0) '("pre" ""))
         (head:before-frame!)
         '(6 #f)))
     (indent-buffer!)
     (check 'indent-keeps-source-row-identity (text-of indented) '("pre" "      abc" "tail"))
     (check 'indent-projects-content-point (seat:point) '(1 . 7))
     (check 'indent-projects-selection-endpoint (bmark indented) '(2 . 2))

     ;; Replace-all's preserved point is expressed in its proposed result;
     ;; a store write ahead of the head must not expand the intended target.
     (define replaced (fresh "position-replace" '("aba" "tail")))
     (seat:goto! '(0 . 1))
     (foreign! replaced (text:make-span 0 0 0 0) '("Q"))
     (search:replace! "a" "ZZ")
     (check 'replace-preserves-unseen-prefix (text-of replaced) '("QZZbZZ" "tZZil"))
     (check 'replace-projects-preserved-point (seat:point) '(0 . 3))

     ;; Append is an insertion for shared and local buffers.  It cannot
     ;; reset shared history or make an empty local buffer zero lines long.
     (define appended (fresh "position-append" '("abc" "tail")))
     (insert-text! "X")
     (foreign! appended (text:make-span 0 0 0 0) '("Q"))
     (seat:buffer-append! appended "new")
     (check 'append-keeps-foreign-edit (text-of appended) '("QXabc" "tail" "new"))
     (undo!)
     (check 'append-is-undoable (text-of appended) '("QXabc" "tail"))
     (undo!)
     (check 'append-retains-earlier-undo (text-of appended) '("Qabc" "tail"))
     (define empty (fresh "position-empty-append" '("")))
     (seat:buffer-append! empty)
     (check 'empty-append-retains-one-line (text-of empty) '(""))
     (check 'empty-append-retains-valid-point (seat:point) '(0 . 0))

     ;; R5: both directions of a live selection retain the selected text
     ;; through foreign and own edits before it, then clamp a deleted part.
     (for-each
       (lambda (backwards?)
         (let* ([b (fresh "position-selection" '("abcdef"))]
                [id (seat:buffer-store-id b)])
           (seat:goto! (if backwards? '(0 . 6) '(0 . 2)))
           (set-mark-command!)
           (seat:goto! (if backwards? '(0 . 2) '(0 . 6)))
           (foreign! b (text:make-span 0 0 0 0) '("Q"))
           (head:before-frame!)
           (seat:store-edit! b (text:make-span 0 0 0 0) '("X"))
           (head:before-frame!)
           (check 'selection-retains-orientation-after-both-authors
                  (list (seat:point) (seat:mark))
                  (if backwards? '((0 . 4) (0 . 8)) '((0 . 8) (0 . 4))))
           (let ([region (store:mark head:ui-actor id 'region)])
             (check 'published-selection-agrees-with-head
                    (list (text:span-start region) (text:span-end region)) '((0 . 4) (0 . 8))))
           (copy-region!)
           (check 'selection-keeps-the-original-text (copy-text) "cdef")
           (seat:buffer-marked-set! b #t)
           (foreign! b (text:make-span 0 0 0 2) '(""))
           (head:before-frame!)
           (check 'selection-follows-prefix-deletion
                  (list (seat:point) (seat:mark))
                  (if backwards? '((0 . 2) (0 . 6)) '((0 . 6) (0 . 2))))
           (foreign! b (text:make-span 0 3 0 5) '(""))
           (head:before-frame!)
           (copy-region!)
           (check 'selection-keeps-the-surviving-text (copy-text) "cf")))
       '(#f #t))

     (test:finish! 'edit-position)))
