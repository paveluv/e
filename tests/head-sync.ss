#!/usr/bin/env scheme-script

;; Coherent adoption of shared text and positions.  Notifications wake
;; the head; the store snapshot's complete delta chain advances it.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (head) head:)
             (prefix (store) store:)
             (prefix (text) text:))

     (define checks 0)
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'head-sync-test (symbol->string label) actual expected)))

     (define b (head:window-buffer (head:current)))
     (define id (head:buffer-store-id b))
     (define w (head:current))
     (define bot '(agent sync-test))
     (define (point) (cons (head:window-prow w) (head:window-pcol w)))
     (define (edit! span replacement)
       (store:edit! bot id (store:revision id) span replacement))

     (head:buffer-lines-set! b '#("aaa" "bbb" "ccc"))
     (head:window-prow-set! w 1)
     (head:window-pcol-set! w 2)
     (head:window-top-set! w 1)
     (head:buffer-spot-row-set! b 2)
     (head:buffer-spot-col-set! b 2)
     (head:buffer-spot-top-set! b 2)
     (head:before-frame!)

     (edit! (text:make-span 0 0 0 0) '("new" ""))
     (head:before-frame!)
     (check 'cursor-follows-content (point) '(2 . 2))
     (check 'viewport-follows-content (head:window-top w) 2)
     (check 'saved-position-follows-content
            (cons (head:buffer-spot-row b) (head:buffer-spot-col b)) '(3 . 2))
     (check 'saved-viewport-follows-content (head:buffer-spot-top b) 3)
     (check 'published-point-agrees (store:mark head:ui-actor id 'point) (point))
     (check 'text-and-revision-agree
            (let-values ([(text revision) (store:snapshot id)])
              (and (eq? text (head:buffer-lines b))
                   (= revision (head:buffer-store-rev b))))
            #t)

     ;; A reset cuts history.  Do not apply the queued pre-reset edit or
     ;; a partial post-reset suffix to positions from the old baseline.
     (head:buffer-lines-set! b '#("abcdef"))
     (head:window-prow-set! w 0)
     (head:window-pcol-set! w 2)
     (head:window-top-set! w 0)
     (head:before-frame!)
     (edit! (text:make-span 0 0 0 0) '("X"))
     (store:reset! bot id '("text"))
     (edit! (text:make-span 0 0 0 0) '("YZ"))
     (head:before-frame!)
     (check 'reset-gap-adopts-current-text (head:buffer-lines b) '#("YZtext"))
     (check 'reset-gap-does-not-replay-partial-history (point) '(0 . 2))
     (check 'reset-gap-publishes-clamped-point
            (store:mark head:ui-actor id 'point) '(0 . 2))

     ;; Even a long queued stream is not evidence of a complete chain:
     ;; once the retained history is too short, resync explicitly.
     (do ([i 0 (+ i 1)]) ((= i 257))
       (edit! (text:make-span 0 0 0 0) '("x")))
     (head:before-frame!)
     (check 'truncated-history-does-not-replay-partial-history (point) '(0 . 2))
     (check 'truncated-history-adopts-current-revision
            (head:buffer-store-rev b) (store:revision id))
     (check 'truncated-history-adopts-all-text
            (string-length (vector-ref (head:buffer-lines b) 0)) 263)

     (store:reset! bot id '(""))
     (head:before-frame!)
     (check 'resync-clamps-into-shorter-text (point) '(0 . 0))
     (check 'resync-clamps-saved-viewport (head:buffer-spot-top b) 0)

     ;; An explicit baseline reset also invalidates publication.  A
     ;; subscriber can edit after reset before the head adopts its snapshot;
     ;; unchanged numeric head coordinates must still replace the store's
     ;; cursor that followed that subscriber's insertion.
     (head:buffer-lines-set! b '#("abcdef"))
     (head:window-pcol-set! w 2)
     (head:before-frame!)
     (define reset-token
       (store:subscribe! id
         (lambda (event)
           (when (eq? (car event) 'reset)
             (edit! (text:make-span 0 0 0 0) '("Q"))))))
     (head:buffer-lines-set! b '#("abcdef"))
     (store:unsubscribe! reset-token)
     (head:before-frame!)
     (check 'explicit-reset-adopts-the-subscribers-text (head:buffer-lines b) '#("Qabcdef"))
     (check 'explicit-reset-keeps-clamped-coordinates (point) '(0 . 2))
     (check 'explicit-reset-republishes-unchanged-coordinates
            (store:mark head:ui-actor id 'point) (point))

     ;; Derived readers follow this head's adopted source, for either
     ;; owner. Repaint/fact changes are not content revisions, and a
     ;; store writer may be ahead of the text the head has adopted.
     (define (since source basis)
       (call-with-values (lambda () (head:snapshot-since source basis)) list))
     (for-each
       (lambda (local?)
         (let ([source ((if local? head:new-local-buffer head:new-buffer) "source-history")])
           (head:add-buffer! source)
           (head:buffer-lines-set! source '#("alpha" "middle" "omega"))
           (let* ([initial (head:edit-basis source)] [basis (caddr initial)])
             (head:bump-buffer-revision! source)
             (head:buffer-fact-set! source 'custom 'changed)
             (check 'repaint-and-facts-preserve-content-basis (head:edit-basis source) initial)
             (head:store-edit! source (text:make-span 0 0 0 0) '("before" ""))
             (head:store-edit! source (text:make-span 3 5 3 5) '("!"))
             (let* ([snapshot (since source basis)] [changes (caddr snapshot)])
               (check 'source-chain-is-complete (map car changes) (list (+ basis 1) (+ basis 2)))
               (check 'source-chain-preserves-unchanged-middle
                      (fold-left text:rebase-position '(1 . 2) (map caddr changes)) '(2 . 2))
               (check 'source-chain-ends-at-snapshot
                      (fold-left
                        (lambda (lines entry)
                          (let ([delta (caddr entry)])
                            (let-values ([(next ignored)
                                          (text:apply-edit lines (text:delta-span delta)
                                                           (text:delta-inserted delta))])
                              next)))
                        (car initial) changes)
                      (car snapshot))
               (check 'source-snapshot-keeps-adopted-vector
                      (eq? (car snapshot) (head:buffer-lines source)) #t)
               (check 'same-source-basis-is-empty (caddr (since source (cadr snapshot))) '())
               (check 'future-source-basis-is-unavailable (caddr (since source (+ (cadr snapshot) 1))) #f)
               (set-car! (car changes) -1)
               (check 'source-history-read-does-not-expose-its-list
                      (caar (caddr (since source basis))) (+ basis 1)))
             (unless local?
               (let ([snapshot (since source basis)] [id (head:buffer-store-id source)])
                 (store:edit! bot id (store:revision id) (text:make-span 0 0 0 0) '("ahead" ""))
                 (check 'unadopted-store-text-does-not-leak-into-source-read
                        (since source basis) snapshot)
                 (head:before-frame!)
                 (check 'adoption-extends-source-chain (length (caddr (since source basis))) 3)
                 ;; Store history can disappear while the head's source
                 ;; and its already-adopted provenance remain coherent.
                 (store:reset! bot id '("reset"))
                 (check 'unadopted-reset-keeps-cached-source-chain
                        (length (caddr (since source basis))) 3)
                 (head:before-frame!)
                 (check 'adopted-reset-cuts-source-chain (caddr (since source basis)) #f)))
             (head:buffer-lines-set! source '#("new baseline"))
             (head:store-edit! source (text:make-span 0 0 0 0) '("suffix" ""))
             (check 'reset-with-new-edits-never-returns-a-partial-chain
                    (caddr (since source basis)) #f)
             (let ([basis (caddr (head:edit-basis source))])
               (do ([i 0 (+ i 1)]) ((= i 256))
                 (head:store-edit! source (text:make-span 0 0 0 0) '("x")))
               (check 'source-history-retains-256-changes (length (caddr (since source basis))) 256)
               (head:store-edit! source (text:make-span 0 0 0 0) '("x"))
               (check 'expired-source-basis-is-unavailable (caddr (since source basis)) #f)
               (check 'retained-source-suffix-still-complete
                      (length (caddr (since source (+ basis 1)))) 256)))))
       '(#f #t))

     (format #t "~a head synchronization checks passed\n" checks)))
