#!/usr/bin/env scheme-script

;; Command intent, adopted text, and all head anchors share one revision,
;; including reentrant notifications and callbacks computing a proposal.

(import (chezscheme))
(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (except (edit) init!)
             (prefix (head) head:)
             (prefix (store) store:)
             (prefix (text) text:)
             (prefix (mode) mode:)
             (prefix (kernel) kernel:))

     (define checks 0)
     (define bot '(agent position-test))
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'edit-position-test (symbol->string label) actual expected)))
     (define (fresh name lines . local?)
       (let ([b ((if (and (pair? local?) (car local?)) head:new-local-buffer head:new-buffer) name)])
         (head:buffer-lines-set! b (list->vector lines))
         (show-buffer! b)
         (goto-point! '(0 . 0))
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
     (define w (head:current))
     (define w2 (head:make-window b 1 0 0 1 2 12 1 80 80 1 'default))
     (define w3 (head:make-window b 0 0 0 0 5 12 1 80 80 1 'default))
     (head:set-windows! (append (head:windows) (list w2 w3)))
     (head:buffer-spot-row-set! b 1)
     (head:buffer-spot-col-set! b 1)
     (head:buffer-spot-top-set! b 1)
     (head:buffer-mark-row-set! b 1)
     (head:buffer-mark-col-set! b 1)
     (head:buffer-marked-set! b #t)
     (goto-point! '(0 . 2))
     (foreign! b (text:make-span 0 0 0 0) '("pre" "Q"))
     (insert-text! "X")
     (check 'command-follows-its-rebased-insertion (point) '(1 . 4))
     (check 'both-texts-survive (text-of b) '("pre" "QabXcdef" "tail"))
     (check 'other-window-follows-lines (wpoint w2) '(2 . 2))
     (check 'other-window-follows-own-character (wpoint w3) '(1 . 7))
     (check 'other-viewport-follows-content (head:window-top w2) 2)
     (check 'saved-point-follows-content
            (cons (head:buffer-spot-row b) (head:buffer-spot-col b)) '(2 . 1))
     (check 'saved-viewport-follows-content (head:buffer-spot-top b) 2)
     (check 'mark-follows-content (bmark b) '(2 . 1))
     (head:before-frame!)
     (check 'queued-events-do-not-shift-again (point) '(1 . 4))
     (check 'published-point-matches-adopted-point
            (store:mark head:ui-actor (head:buffer-store-id b) 'point) (point))

     ;; The store's edit insertion bias at a replacement's left boundary
     ;; differs from ordinary cursor rebasing.  Place after OUR character.
     (define boundary (fresh "position-boundary" '("abcdef")))
     (goto-point! '(0 . 2))
     (foreign! boundary (text:make-span 0 2 0 4) '("R"))
     (insert-text! "X")
     (check 'insertion-before-foreign-replacement (text-of boundary) '("abXRef"))
     (check 'point-stays-after-own-character (point) '(0 . 3))
     (head:before-frame!)
     (check 'boundary-point-stays-stable (point) '(0 . 3))

     ;; A subscriber writes and adopts a newer snapshot before the outer
     ;; edit even returns.  Neither text nor anchors may replay a prefix.
     (define reentrant (fresh "position-reentrant" '("abcdef")))
     (goto-point! '(0 . 2))
     (define token
       (after-own! reentrant
         (lambda ()
           (foreign! reentrant (text:make-span 0 0 0 0) '("G"))
           (head:before-frame!))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'reentrant-adoption-keeps-current-text (text-of reentrant) '("GabXcdef"))
     (check 'reentrant-adoption-places-once (point) '(0 . 4))
     (head:before-frame!)
     (check 'reentrant-queued-events-do-not-repeat (point) '(0 . 4))

     ;; A callback before mutation may advance the cache too.  Commands
     ;; keep their original range and basis across an editability check.
     (define guarded (fresh "position-guard" '("abcdef")))
     (goto-point! '(0 . 2))
     (head:buffer-read-only-set! guarded
       (lambda ()
         (foreign! guarded (text:make-span 0 0 0 0) '("Q"))
         (head:before-frame!)
         #t))
     (insert-text! "X")
     (head:buffer-read-only-set! guarded #f)
     (check 'preflight-adoption-preserves-intent (text-of guarded) '("QabXcdef"))
     (check 'preflight-adoption-preserves-point (point) '(0 . 4))
     (goto-point! '(0 . 2))
     (head:buffer-read-only-set! guarded
       (lambda ()
         (foreign! guarded (text:make-span 0 1 0 5) '("R"))
         (head:before-frame!)
         #t))
     (check 'preflight-overlap-still-refuses (refused? delete-forward!) #t)
     (head:buffer-read-only-set! guarded #f)
     (check 'preflight-overlap-cannot-delete-a-new-neighbor (text-of guarded) '("QRdef"))

     ;; A receipt must retain a chain that was complete on acceptance,
     ;; even if this very commit trims its oldest entry out of the log.
     (define retained (fresh "position-retention-boundary" '("abcdef")))
     (goto-point! '(0 . 2))
     (head:buffer-read-only-set! retained
       (lambda ()
         (do ([i 0 (+ i 1)]) ((= i 256))
           (foreign! retained (text:make-span 0 0 0 0) '("q")))
         #t))
     (insert-text! "X")
     (head:buffer-read-only-set! retained #f)
     (check 'acceptance-retains-the-whole-chain (point) '(0 . 259))
     (check 'acceptance-at-retention-boundary-keeps-text
            (substring (car (text-of retained)) 256 263) "abXcdef")
     (head:before-frame!)
     (check 'retention-boundary-events-do-not-shift-again (point) '(0 . 259))

     ;; A reset after our commit destroys the anchor proof.  Resync and
     ;; clamp; the command must not subsequently write its old new-end.
     (define reset (fresh "position-reset-after-commit" '("abcdef")))
     (goto-point! '(0 . 2))
     (set! token
       (after-own! reset
         (lambda () (store:reset! bot (head:buffer-store-id reset) '("other")))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'post-commit-reset-keeps-latest-text (text-of reset) '("other"))
     (check 'post-commit-reset-does-not-install-old-cursor (point) '(0 . 2))

     (define truncated (fresh "position-truncated-after-commit" '("abcdef")))
     (goto-point! '(0 . 2))
     (set! token
       (after-own! truncated
         (lambda ()
           (do ([i 0 (+ i 1)]) ((= i 257))
             (foreign! truncated (text:make-span 0 0 0 0) '("q"))))))
     (insert-text! "X")
     (store:unsubscribe! token)
     (check 'post-commit-truncation-clamps-without-partial-replay (point) '(0 . 2))
     (check 'post-commit-truncation-keeps-all-text
            (substring (car (text-of truncated)) 257 264) "abXcdef")

     ;; Structural commands place their declared endpoint, not a line/
     ;; column computed before the store rebased the actual range.
     (define structural (fresh "position-structural" '("abcdef")))
     (goto-point! '(0 . 2))
     (foreign! structural (text:make-span 0 0 0 0) '("Q"))
     (open-line!)
     (check 'open-line-keeps-before-inserted-break (point) '(0 . 3))
     (check 'open-line-splits-the-rebased-position (text-of structural) '("Qab" "cdef"))
     (goto-point! '(1 . 0))
     (foreign! structural (text:make-span 0 0 0 0) '("R"))
     (backspace!)
     (check 'backspace-follows-rebased-join (point) '(0 . 4))
     (check 'backspace-preserves-foreign-prefix (text-of structural) '("RQabcdef"))
     (foreign! structural (text:make-span 0 0 0 0) '("S"))
     (replace-region-text! '(0 . 2) '(0 . 4) "x\ny")
     (check 'replacement-follows-rebased-end (point) '(1 . 1))
     (check 'replacement-preserves-newer-prefix (text-of structural) '("SRQx" "ycdef"))

     ;; Formatter output and its point keep their original snapshot even
     ;; when the provider pumps a frame and an after-commit observer does too.
     (mode:register! "position-format" '() '() (lambda (line) #f))
     (define formatted (fresh "position-format-source" '("abc" "tail")))
     (mode:choose! formatted "position-format")
     (goto-point! '(0 . 2))
     (register-formatter! "position-format"
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
     (check 'format-projects-its-preserved-point (point) '(0 . 4))
     (undo!)
     (check 'format-undo-keeps-both-foreign-edits (text-of formatted) '("RQabc" "tail"))

     (define conflict (fresh "position-format-conflict" '("abc")))
     (mode:choose! conflict "position-format")
     (register-formatter! "position-format"
       (lambda (b from to)
         (foreign! b (text:make-span 0 1 0 2) '("R"))
         (head:before-frame!)
         '("ABC")))
     (check 'format-refuses-even-after-head-adoption (refused? format-buffer!) #t)
     (check 'format-cannot-overwrite-adopted-foreign-text (text-of conflict) '("aRc"))
     (check 'format-refusal-keeps-head-history (vector-ref (head:buffer-history conflict) 0) '())

     (define indented (fresh "position-indent-source" '("  abc" "tail")))
     (mode:choose! indented "position-format")
     (goto-point! '(1 . 2))
     (set-mark-command!)
     (goto-point! '(0 . 3))
     (register-indenter! "position-format"
       (lambda (b from to)
         (foreign! b (text:make-span 0 0 0 0) '("pre" ""))
         (head:before-frame!)
         '(6 #f)))
     (indent-buffer!)
     (check 'indent-keeps-source-row-identity (text-of indented) '("pre" "      abc" "tail"))
     (check 'indent-projects-content-point (point) '(1 . 7))
     (check 'indent-projects-selection-endpoint (bmark indented) '(2 . 2))

     ;; Replace-all's preserved point is expressed in its proposed result;
     ;; a permission callback may not cause its target to include new text.
     (define replaced (fresh "position-replace-all" '("aba" "tail")))
     (goto-point! '(0 . 1))
     (head:buffer-read-only-set! replaced
       (lambda ()
         (foreign! replaced (text:make-span 0 0 0 0) '("Q"))
         (head:before-frame!)
         #t))
     (replace-all! "a" "ZZ")
     (head:buffer-read-only-set! replaced #f)
     (check 'replace-all-preserves-unseen-prefix (text-of replaced) '("QZZbZZ" "tZZil"))
     (check 'replace-all-projects-preserved-point (point) '(0 . 2))

     ;; Local edits and local undo use the same anchor geometry.
     (define local (fresh "position-local" '("abc" "tail") #t))
     (head:set-window-buffer! w2 local)
     (head:window-prow-set! w2 1)
     (head:window-pcol-set! w2 2)
     (goto-point! '(0 . 1))
     (newline!)
     (check 'local-other-window-follows-edit (wpoint w2) '(2 . 2))
     (undo!)
     (check 'local-other-window-follows-undo (wpoint w2) '(1 . 2))
     (check 'local-undo-restores-command-point (point) '(0 . 1))
     (mode:choose! local "position-format")
     (register-formatter! "position-format"
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
     (buffer-append! appended "new")
     (check 'append-keeps-foreign-edit (text-of appended) '("QXabc" "tail" "new"))
     (undo!)
     (check 'append-is-undoable (text-of appended) '("QXabc" "tail"))
     (undo!)
     (check 'append-retains-earlier-undo (text-of appended) '("Qabc" "tail"))
     (define empty (fresh "position-empty-append" '("") #t))
     (buffer-append! empty)
     (check 'empty-append-retains-one-line (text-of empty) '(""))
     (check 'empty-append-retains-valid-point (point) '(0 . 0))

     ;; R5: both directions of a live selection retain the selected text
     ;; through foreign and own edits before it, then clamp a deleted part.
     (for-each
       (lambda (backwards?)
         (let* ([b (fresh "position-selection" '("abcdef"))]
                [id (head:buffer-store-id b)])
           (goto-point! (if backwards? '(0 . 6) '(0 . 2)))
           (set-mark-command!)
           (goto-point! (if backwards? '(0 . 2) '(0 . 6)))
           (foreign! b (text:make-span 0 0 0 0) '("Q"))
           (head:before-frame!)
           (head:store-edit! b (text:make-span 0 0 0 0) '("X"))
           (head:before-frame!)
           (check 'selection-retains-orientation-after-both-authors
                  (list (point) (mark))
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
                  (list (point) (mark))
                  (if backwards? '((0 . 2) (0 . 6)) '((0 . 6) (0 . 2))))
           (foreign! b (text:make-span 0 3 0 5) '(""))
           (head:before-frame!)
           (copy-region!)
           (check 'selection-keeps-the-surviving-text (current-kill-ring) "cf")))
       '(#f #t))

     (format #t "~a edit position checks passed\n" checks)))
