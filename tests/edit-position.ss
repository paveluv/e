#!/usr/bin/env scheme-script

;; Command intent, adopted text, and all head anchors share one revision,
;; including reentrant notifications and callbacks computing a proposal.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (edit) init!)
             (prefix (head) head:)
             (prefix (search) search:)
             (prefix (store) store:)
             (prefix (text) text:)
             (prefix (mode) mode:)
             (prefix (kernel) kernel:))

     (define bot '(agent position-test))
     (define check test:check)
     (define (fresh name lines . local?)
       (let ([b ((if (and (pair? local?) (car local?)) head:new-local-buffer! head:new-buffer!) name)])
         (head:buffer-lines-set! b (list->vector lines))
         (head:show-buffer! b)
         (head:goto! '(0 . 0))
         b))
     (define (text-of b) (vector->list (head:buffer-lines b)))
     (define (foreign! b span replacement)
       (let ([id (head:buffer-store-id b)])
         (store:edit! bot id (store:revision id) span replacement)))
     (define (wpoint w) (cons (head:window-prow w) (head:window-pcol w)))
     (define (bmark b) (cons (head:buffer-mark-row b) (head:buffer-mark-col b)))
     (define (refused? thunk)
       (guard (ex [(kernel:refusal? ex) #t] [else (raise ex)]) (thunk) #f))
     (define (after-own! b thunk)
       (store:subscribe! (head:buffer-store-id b)
         (lambda (event)
           (when (and (eq? (car event) 'edit) (equal? (list-ref event 3) head:ui-actor))
             (thunk)))))

     ;; A missed multiline insertion precedes this head's character.
     ;; Nonselected windows and saved/selection positions must follow too.
     (define b (fresh "position-before" '("abcdef" "tail")))
     (define w (head:current-window))
     (define w2 (head:make-window b 1 0 0 1 2 12 80 80 'default))
     (define w3 (head:make-window b 0 0 0 0 5 12 80 80 'default))
     (head:set-windows! (append (head:windows) (list w2 w3)))
     (head:buffer-spot-row-set! b 1)
     (head:buffer-spot-col-set! b 1)
     (head:buffer-spot-top-set! b 1)
     (head:buffer-mark-row-set! b 1)
     (head:buffer-mark-col-set! b 1)
     (head:buffer-marked-set! b #t)
     (head:goto! '(0 . 2))
     (foreign! b (text:make-span 0 0 0 0) '("pre" "Q"))
     (insert-text! "X")
     (check 'command-follows-its-rebased-insertion (head:point) '(1 . 4))
     (check 'both-texts-survive (text-of b) '("pre" "QabXcdef" "tail"))
     (check 'other-window-follows-lines (wpoint w2) '(2 . 2))
     (check 'other-window-follows-own-character (wpoint w3) '(1 . 7))
     (check 'other-viewport-follows-content (head:window-top w2) 2)
     (check 'saved-point-follows-content
            (cons (head:buffer-spot-row b) (head:buffer-spot-col b)) '(2 . 1))
     (check 'saved-viewport-follows-content (head:buffer-spot-top b) 2)
     (check 'mark-follows-content (bmark b) '(2 . 1))
     (head:before-frame!)
     (check 'queued-events-do-not-shift-again (head:point) '(1 . 4))
     (check 'published-point-matches-adopted-point
            (store:mark head:ui-actor (head:buffer-store-id b) 'point) (head:point))

     ;; The store's edit insertion bias at a replacement's left boundary
     ;; differs from ordinary cursor rebasing.  Place after OUR character.
     (define boundary (fresh "position-boundary" '("abcdef")))
     (head:goto! '(0 . 2))
     (foreign! boundary (text:make-span 0 2 0 4) '("R"))
     (insert-text! "X")
     (check 'insertion-before-foreign-replacement (text-of boundary) '("abXRef"))
     (check 'point-stays-after-own-character (head:point) '(0 . 3))
     (head:before-frame!)
     (check 'boundary-point-stays-stable (head:point) '(0 . 3))

     ;; A subscriber writes and adopts a newer snapshot before the outer
     ;; edit even returns.  Neither text nor anchors may replay a prefix.
     (define reentrant (fresh "position-reentrant" '("abcdef")))
     (head:goto! '(0 . 2))
     (define token
       (after-own! reentrant
         (lambda ()
           (foreign! reentrant (text:make-span 0 0 0 0) '("G"))
           (head:before-frame!))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'reentrant-adoption-keeps-current-text (text-of reentrant) '("GabXcdef"))
     (check 'reentrant-adoption-places-once (head:point) '(0 . 4))
     (head:before-frame!)
     (check 'reentrant-queued-events-do-not-repeat (head:point) '(0 . 4))

     ;; Dynamic edit guards are head-local callbacks; shared facts are data.
     ;; A guard changing local source after capture must not retarget intent.
     ;; Shared callbacks and reentrant adoption are covered by the formatter
     ;; and subscriber fixtures below, without storing procedures in the base.
     (define guarded (fresh "position-guard" '("abcdef") #t))
     (head:goto! '(0 . 2))
     (head:buffer-read-only-set! guarded
       (lambda ()
         (head:store-edit! guarded (text:make-span 0 0 0 0) '("Q"))
         #t))
     (check 'guard-cannot-retarget-captured-local-intent
            (let ([refused (refused? (lambda () (insert-text! "X")))])
              (list refused (text-of guarded) (head:point)))
            '(#t ("Qabcdef") (0 . 3)))
     (head:buffer-read-only-set! guarded #f)

     ;; A receipt must retain a chain that was complete on acceptance,
     ;; even if this very commit trims its oldest entry out of the log.
     (define retained (fresh "position-retention-boundary" '("abcdef")))
     (head:goto! '(0 . 2))
     ;; These writes stay ahead of the head's adopted command basis.
     (do ([i 0 (+ i 1)]) ((= i 256))
       (foreign! retained (text:make-span 0 0 0 0) '("q")))
     (insert-text! "X")
     (check 'acceptance-retains-the-whole-chain (head:point) '(0 . 259))
     (check 'acceptance-at-retention-boundary-keeps-text
            (substring (car (text-of retained)) 256 263) "abXcdef")
     (head:before-frame!)
     (check 'retention-boundary-events-do-not-shift-again (head:point) '(0 . 259))

     ;; A reset after our commit destroys the anchor proof.  Resync and
     ;; clamp; the command must not subsequently write its old new-end.
     (define reset (fresh "position-reset-after-commit" '("abcdef")))
     (head:goto! '(0 . 2))
     (set! token
       (after-own! reset
         (lambda () (store:reset! bot (head:buffer-store-id reset) '("other")))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'post-commit-reset-keeps-latest-text (text-of reset) '("other"))
     (check 'post-commit-reset-does-not-install-old-cursor (head:point) '(0 . 2))

     (define truncated (fresh "position-truncated-after-commit" '("abcdef")))
     (head:goto! '(0 . 2))
     (set! token
       (after-own! truncated
         (lambda ()
           (do ([i 0 (+ i 1)]) ((= i 257))
             (foreign! truncated (text:make-span 0 0 0 0) '("q"))))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'post-commit-truncation-clamps-without-partial-replay (head:point) '(0 . 2))
     (check 'post-commit-truncation-keeps-all-text
            (substring (car (text-of truncated)) 257 264) "abXcdef")

     ;; Structural commands place their declared endpoint, not a line/
     ;; column computed before the store rebased the actual range.
     (define structural (fresh "position-structural" '("abcdef")))
     (head:goto! '(0 . 2))
     (foreign! structural (text:make-span 0 0 0 0) '("Q"))
     (open-line!)
     (check 'open-line-keeps-before-inserted-break (head:point) '(0 . 3))
     (check 'open-line-splits-the-rebased-position (text-of structural) '("Qab" "cdef"))
     (head:goto! '(1 . 0))
     (foreign! structural (text:make-span 0 0 0 0) '("R"))
     (backspace!)
     (check 'backspace-follows-rebased-join (head:point) '(0 . 4))
     (check 'backspace-preserves-foreign-prefix (text-of structural) '("RQabcdef"))
     (foreign! structural (text:make-span 0 0 0 0) '("S"))
     (replace-region-text! '(0 . 2) '(0 . 4) "x\ny")
     (check 'replacement-follows-rebased-end (head:point) '(1 . 1))
     (check 'replacement-preserves-newer-prefix (text-of structural) '("SRQx" "ycdef"))

     ;; Formatter output and its point keep their original snapshot even
     ;; when the provider pumps a frame and an after-commit observer does too.
     (mode:register! "position-format" '() '() (lambda (line) #f))
     (define formatted (fresh "position-format-source" '("abc" "tail")))
     (mode:choose! formatted "position-format")
     (head:goto! '(0 . 2))
     (mode:register-formatter! "position-format"
       (lambda (b from to)
         (foreign! b (text:make-span 0 0 0 0) '("Q"))
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
     (check 'format-projects-its-preserved-point (head:point) '(0 . 4))
     (undo!)
     (check 'format-undo-keeps-both-foreign-edits (text-of formatted) '("RQabc" "tail"))

     (define conflict (fresh "position-format-conflict" '("abc")))
     (mode:choose! conflict "position-format")
     (mode:register-formatter! "position-format"
       (lambda (b from to)
         (foreign! b (text:make-span 0 1 0 2) '("R"))
         (head:before-frame!)
         '("ABC")))
     (check 'format-refuses-even-after-head-adoption (refused? format-buffer!) #t)
     (check 'format-cannot-overwrite-adopted-foreign-text (text-of conflict) '("aRc"))
     (check 'format-refusal-keeps-head-history (vector-ref (head:buffer-history conflict) 0) '())

     (define indented (fresh "position-indent-source" '("  abc" "tail")))
     (mode:choose! indented "position-format")
     (head:goto! '(1 . 2))
     (set-mark-command!)
     (head:goto! '(0 . 3))
     (mode:register-indenter! "position-format"
       (lambda (b from to)
         (foreign! b (text:make-span 0 0 0 0) '("pre" ""))
         (head:before-frame!)
         '(6 #f)))
     (indent-buffer!)
     (check 'indent-keeps-source-row-identity (text-of indented) '("pre" "      abc" "tail"))
     (check 'indent-projects-content-point (head:point) '(1 . 7))
     (check 'indent-projects-selection-endpoint (bmark indented) '(2 . 2))

     ;; Replace-all's preserved point is expressed in its proposed result;
     ;; a store write ahead of the head must not expand the intended target.
     (define replaced (fresh "position-replace-all" '("aba" "tail")))
     (head:goto! '(0 . 1))
     (foreign! replaced (text:make-span 0 0 0 0) '("Q"))
     (search:replace-all! "a" "ZZ")
     (check 'replace-all-preserves-unseen-prefix (text-of replaced) '("QZZbZZ" "tZZil"))
     (check 'replace-all-projects-preserved-point (head:point) '(0 . 2))

     ;; Local edits and local undo use the same anchor geometry.
     (define local (fresh "position-local" '("abc" "tail") #t))
     (head:set-window-buffer! w2 local)
     (head:window-prow-set! w2 1)
     (head:window-pcol-set! w2 2)
     (head:goto! '(0 . 1))
     (newline!)
     (check 'local-other-window-follows-edit (wpoint w2) '(2 . 2))
     (undo!)
     (check 'local-other-window-follows-undo (wpoint w2) '(1 . 2))
     (check 'local-undo-restores-command-point (head:point) '(0 . 1))
     (mode:choose! local "position-format")
     (mode:register-formatter! "position-format"
       (lambda (b from to)
         (head:buffer-lines-set! b '#("new local text"))
         '("ABC" "tail")))
     (check 'local-stale-computation-refuses (refused? format-buffer!) #t)
     (check 'local-stale-computation-keeps-newer-text (text-of local) '("new local text"))

     ;; Append is an insertion for shared and local buffers.  It cannot
     ;; reset shared history or make an empty local buffer zero lines long.
     (define appended (fresh "position-append" '("abc" "tail")))
     (insert-text! "X")
     (foreign! appended (text:make-span 0 0 0 0) '("Q"))
     (head:buffer-append! appended "new")
     (check 'append-keeps-foreign-edit (text-of appended) '("QXabc" "tail" "new"))
     (undo!)
     (check 'append-is-undoable (text-of appended) '("QXabc" "tail"))
     (undo!)
     (check 'append-retains-earlier-undo (text-of appended) '("Qabc" "tail"))
     (define empty (fresh "position-empty-append" '("") #t))
     (head:buffer-append! empty)
     (check 'empty-append-retains-one-line (text-of empty) '(""))
     (check 'empty-append-retains-valid-point (head:point) '(0 . 0))

     ;; R5: both directions of a live selection retain the selected text
     ;; through foreign and own edits before it, then clamp a deleted part.
     (for-each
       (lambda (backwards?)
         (let* ([b (fresh "position-selection" '("abcdef"))]
                [id (head:buffer-store-id b)])
           (head:goto! (if backwards? '(0 . 6) '(0 . 2)))
           (set-mark-command!)
           (head:goto! (if backwards? '(0 . 2) '(0 . 6)))
           (foreign! b (text:make-span 0 0 0 0) '("Q"))
           (head:before-frame!)
           (head:store-edit! b (text:make-span 0 0 0 0) '("X"))
           (head:before-frame!)
           (check 'selection-retains-orientation-after-both-authors
                  (list (head:point) (head:mark))
                  (if backwards? '((0 . 4) (0 . 8)) '((0 . 8) (0 . 4))))
           (let ([region (store:mark head:ui-actor id 'region)])
             (check 'published-selection-agrees-with-head
                    (list (text:span-start region) (text:span-end region)) '((0 . 4) (0 . 8))))
           (copy-region!)
           (check 'selection-keeps-the-original-text (current-kill-ring) "cdef")
           (head:buffer-marked-set! b #t)
           (foreign! b (text:make-span 0 0 0 2) '(""))
           (head:before-frame!)
           (check 'selection-follows-prefix-deletion
                  (list (head:point) (head:mark))
                  (if backwards? '((0 . 2) (0 . 6)) '((0 . 6) (0 . 2))))
           (foreign! b (text:make-span 0 3 0 5) '(""))
           (head:before-frame!)
           (copy-region!)
           (check 'selection-keeps-the-surviving-text (current-kill-ring) "cf")))
       '(#f #t))

     (test:finish! 'edit-position)))
