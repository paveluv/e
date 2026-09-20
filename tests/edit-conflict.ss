#!/usr/bin/env scheme-script

;; Shared commands either commit their declared edit or stop without
;; overwriting other actors, forking a cache, or recording a phantom action.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (head mode) mode:)
             (prefix (core kernel) kernel:))

     (define bot '(agent conflict-test))
     (define check test:check)
     (define (fresh name lines)
       (let ([b (head:new-buffer! name)])
         (head:buffer-lines-set! b (list->vector lines))
         (head:show-buffer! b)
         (head:goto! '(0 . 0))
         b))
     (define (text-of b) (vector->list (head:buffer-lines b)))
     (define (store-text id)
       (let-values ([(lines revision) (store:snapshot id)]) (vector->list lines)))
     (define (foreign! b span replacement)
       (let ([id (head:buffer-store-id b)])
         (store:edit! bot id (store:revision id) span replacement)))
     (define (refused? thunk)
       (guard (ex [(kernel:refusal? ex) #t] [else (raise ex)]) (thunk) #f))
     (define (failed? thunk)
       (guard (ex [else #t]) (thunk) #f))

     ;; A precise insertion can coexist with an unseen change elsewhere
     ;; on the same line.  Its undo must retain that foreign change too.
     (define disjoint (fresh "edit-disjoint" '("abcdef")))
     (define disjoint-id (head:buffer-store-id disjoint))
     (head:goto! '(0 . 2))
     (foreign! disjoint (text:make-span 0 6 0 6) '("Z"))
     (insert-text! "X")
     (check 'same-line-disjoint-edits-compose (text-of disjoint) '("abXcdefZ"))
     (check 'insertion-retains-its-exact-span
            (list-head (cddar (store:history disjoint-id 1)) 2)
            '((0 . 2) (0 . 2)))
     (undo!)
     (check 'undo-preserves-the-same-line-foreign-edit (text-of disjoint) '("abcdefZ"))

     ;; Preserve an existing redo entry, the kill ring, and selection
     ;; activity when a foreign replacement consumes the insertion point.
     (define overlap (fresh "edit-overlap" '("abcdef" "tail")))
     (define overlap-id (head:buffer-store-id overlap))
     (head:goto! '(1 . 4))
     (insert-text! "own")
     (undo!)
     (head:goto! '(0 . 2))
     (set-mark-command!)
     (copy-to-kill-buffer! "saved kill")
     (define old-undo (vector-ref (head:buffer-history overlap) 0))
     (define old-redo (vector-ref (head:buffer-history overlap) 1))
     (foreign! overlap (text:make-span 0 1 0 5) '("RIV"))
     (define foreign-revision (store:revision overlap-id))
     (check 'overlap-refuses-the-command (refused? (lambda () (insert-text! "X"))) #t)
     (check 'refusal-preserves-foreign-text (store-text overlap-id) '("aRIVf" "tail"))
     (check 'refusal-makes-no-transaction (store:revision overlap-id) foreign-revision)
     (check 'refusal-refreshes-the-cache (text-of overlap) (store-text overlap-id))
     (check 'refusal-keeps-undo-list (eq? old-undo (vector-ref (head:buffer-history overlap) 0)) #t)
     (check 'refusal-keeps-redo-list (eq? old-redo (vector-ref (head:buffer-history overlap) 1)) #t)
     (check 'refusal-keeps-selection-active (head:buffer-marked overlap) #t)
     (check 'refusal-keeps-kill-ring (current-kill-ring) "saved kill")
     (check 'refusal-rebases-point-only-through-the-foreign-edit (head:point) '(0 . 4))
     (redo!)
     (check 'redo-still-works-after-refusal (text-of overlap) '("aRIVf" "tailown"))

     ;; A refusal caught inside a group must not consume its first entry.
     ;; A later successful primitive becomes the group's sole undo action.
     (define grouped (fresh "edit-refused-group" '("abcdef")))
     (head:goto! '(0 . 2))
     (foreign! grouped (text:make-span 0 1 0 5) '("R"))
     (call-as-one-edit! "accepted part"
       (lambda ()
         (check 'group-can-catch-refusal (refused? (lambda () (insert-text! "lost"))) #t)
         (head:goto! '(0 . 0))
         (insert-text! "kept")))
     (check 'group-records-only-accepted-work
            (map car (vector-ref (head:buffer-history grouped) 0)) '("accepted part"))
     (undo!)
     (check 'group-undo-keeps-foreign-result (text-of grouped) '("aRf"))

     ;; Undo inside an explicit group ends that entry's live membership.
     ;; A subsequent edit must have its own snapshot and invalidate redo,
     ;; including in local buffers where history is entirely head-owned.
     (for-each
       (lambda (shared?)
         (let ([b ((if shared? head:new-buffer! head:new-local-buffer!) "edit-after-group-undo")])
           (head:buffer-lines-set! b '#("base"))
           (head:show-buffer! b)
           (call-as-one-edit! "group with undo"
             (lambda () (insert-text! "A") (undo!) (insert-text! "B")))
           (check 'post-undo-group-records-new-entry (length (vector-ref (head:buffer-history b) 0)) 1)
           (check 'post-undo-group-invalidates-redo (vector-ref (head:buffer-history b) 1) '())
           (undo!)
           (check 'post-undo-group-retains-undo (text-of b) '("base"))
           (redo!)
           (check 'post-undo-group-retains-new-redo (text-of b) '("Bbase"))))
       '(#t #f))

     ;; A missing basis (reset or truncated provenance) is not permission
     ;; to replay a cached replacement over the current buffer.
     (define reset (fresh "edit-reset" '("abcdef")))
     (define reset-id (head:buffer-store-id reset))
     (head:goto! '(0 . 2))
     (store:reset! bot reset-id '("remote baseline"))
     (check 'reset-refuses-old-intent (refused? (lambda () (insert-text! "X"))) #t)
     (check 'reset-refusal-keeps-baseline (store-text reset-id) '("remote baseline"))
     (check 'reset-refusal-records-no-history (vector-ref (head:buffer-history reset) 0) '())
     (do ([i 0 (+ i 1)]) ((= i 257))
       (foreign! reset (text:make-span 0 0 0 0) '("x")))
     (define truncated-revision (store:revision reset-id))
     (check 'truncation-refuses-old-intent (refused? (lambda () (insert-text! "X"))) #t)
     (check 'truncation-refusal-keeps-revision (store:revision reset-id) truncated-revision)

     ;; A pre-commit store error used to silently change the local cache,
     ;; then reset the shared text on the next frame.  Invalid context is
     ;; a deterministic failure before mutation, without replacing a seam.
     (define failed (fresh "edit-store-failure" '("abcdef")))
     (define failed-id (head:buffer-store-id failed))
     (define before-failure (head:buffer-lines failed))
     (define before-failure-revision (store:revision failed-id))
     (check 'store-error-propagates
            (failed? (lambda ()
                       (head:store-edit! failed (text:make-span 0 2 0 2) '("LOST")
                                         '(bad-context "invalid" ((42 . bad)))))) #t)
     (check 'store-error-keeps-cache (eq? before-failure (head:buffer-lines failed)) #t)
     (check 'store-error-keeps-revision (store:revision failed-id) before-failure-revision)
     (foreign! failed (text:make-span 0 0 0 0) '("remote "))
     (head:before-frame!)
     (check 'frame-cannot-reset-after-failure (store-text failed-id) '("remote abcdef"))
     (check 'cache-recovers-by-reading (text-of failed) '("remote abcdef"))
     (head:goto! '(0 . 0))
     (insert-text! "ok ")
     (check 'next-valid-edit-uses-store (store-text failed-id) '("ok remote abcdef"))

     ;; A deletion during notification delivery can make adoption fail
     ;; AFTER a successful commit.  That must not create a local fork.
     (define deleted (fresh "edit-deleted-during-delivery" '("before")))
     (define deleted-id (head:buffer-store-id deleted))
     (define deleted-before (head:buffer-lines deleted))
     (define delete-token
       (store:subscribe! deleted-id
         (lambda (event)
           (when (eq? (car event) 'edit) (store:delete! bot deleted-id)))))
     (check 'post-commit-adoption-error-propagates (failed? (lambda () (insert-text! "new"))) #t)
     (check 'post-commit-error-does-not-install-a-local-proposal
            (eq? deleted-before (head:buffer-lines deleted)) #t)
     (check 'failed-baseline-reset-does-not-fork
            (failed? (lambda () (head:buffer-lines-set! deleted '#("replacement")))) #t)
     (check 'failed-reset-keeps-cache (eq? deleted-before (head:buffer-lines deleted)) #t)
     (head:before-frame!)
     (check 'deleted-buffer-is-not-resurrected (store:exists? deleted-id) #f)
     (check 'deleted-record-is-forgotten (and (memq deleted (head:buffers)) #t) #f)
     (store:unsubscribe! delete-token)

     ;; Each structural primitive makes one transaction.  Subscribers can
     ;; never observe the temporary half of a line split/join/replacement.
     (define structural (fresh "edit-structural" '("abc" "tail")))
     (define structural-id (head:buffer-store-id structural))
     (define observations '())
     (define structural-token
       (store:subscribe! structural-id
         (lambda (event)
           (when (eq? (car event) 'edit)
             (set! observations (cons (store-text structural-id) observations))))))
     (head:goto! '(0 . 1))
     (newline!)
     (check 'newline-has-one-complete-observation observations '(("a" "bc" "tail")))
     (set! observations '())
     (backspace!)
     (check 'backspace-joins-in-one-transaction observations '(("abc" "tail")))
     (set! observations '())
     (head:goto! '(0 . 3))
     (delete-forward!)
     (check 'delete-newline-has-one-observation observations '(("abctail")))
     (set! observations '())
     (replace-region-text! '(0 . 1) '(0 . 5) "Q\nR")
     (check 'replacement-has-one-complete-observation observations '(("aQ" "Ril")))
     (set! observations '())
     (copy-to-kill-buffer! "x\ny\n")
     (yank!)
     (check 'multiline-yank-including-final-break-is-atomic observations '(("aQ" "Rx" "y" "il")))
     (store:unsubscribe! structural-token)

     ;; Refused kills must neither send unrelated text to the clipboard
     ;; nor move point before their edit is known to have succeeded.
     (define killed (fresh "edit-kill-refusal" '("abcdef" "tail")))
     (head:goto! '(0 . 2))
     (copy-to-kill-buffer! "keep")
     (foreign! killed (text:make-span 0 1 0 5) '("R"))
     (check 'kill-overlap-refuses (refused? kill-line!) #t)
     (check 'refused-kill-keeps-kill-ring (current-kill-ring) "keep")
     (check 'refused-kill-records-no-entry (vector-ref (head:buffer-history killed) 0) '())
     (head:goto! '(0 . 3))
     (head:buffer-read-only-set! killed #t)
     (check 'read-only-newline-kill-refuses
            (guard (ex [(kernel:read-only-error? ex) #t] [else (raise ex)])
              (kill-line!) #f) #t)
     (check 'read-only-newline-kill-keeps-kill-ring (current-kill-ring) "keep")

     ;; An indenter can be overtaken while computing its proposal.  Its
     ;; desired cursor movement cannot be installed before a refused edit.
     (define indented (fresh "edit-indent-refusal" '("  abc")))
     (mode:register! "conflict-indent" '() '() (lambda (line) #f))
     (mode:choose! indented "conflict-indent")
     (head:goto! '(0 . 2))
     (mode:register-indenter! "conflict-indent"
       (lambda (b from to)
         (foreign! b (text:make-span 0 0 0 4) '("R"))
         '(6)))
     (check 'indent-overlap-refuses (refused? indent-buffer!) #t)
     (check 'refused-indent-keeps-foreign-text (text-of indented) '("Rc"))
     (check 'refused-indent-only-follows-foreign-delta (head:point) '(0 . 1))
     (check 'refused-indent-records-no-entry (vector-ref (head:buffer-history indented) 0) '())

     (test:finish! 'edit-conflict)))
