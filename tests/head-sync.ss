#!/usr/bin/env scheme-script

;; Coherent adoption of shared text and positions.  Notifications wake
;; the head; the store snapshot's complete delta chain advances it.

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (head) head:)
             (prefix (store) store:)
             (prefix (text) text:)
             (prefix (kernel) kernel:)
             (prefix (test) test:))

     (define check test:check)

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

     ;; Shared names come from the store, including hidden reservations.
     ;; Rename commits before changing the head; local tools yield to the
     ;; accepted shared label, and failures leave the cached name intact.
     (define hidden-name (store:create! bot "claimed" '("") '((audience))))
     (define named (head:new-buffer "claimed"))
     (define rival (head:new-buffer "claimed"))
     (head:buffer-name-set! rival "claimed")
     (check 'shared-construction-and-rename-adopt-store-claims
            (list (head:buffer-name named) (head:buffer-name rival)) '("claimed<2>" "claimed<3>"))
     (define rename-tool (head:tool-buffer "shared-rename"))
     (head:buffer-name-set! named "<shared-rename>")
     (check 'accepted-shared-name-displaces-local-label
            (list (head:buffer-name named) (head:buffer-name rename-tool)
                  (eq? (head:find-tool-buffer "shared-rename") rename-tool))
            '("<shared-rename>" "<shared-rename 2>" #t))
     (define store-cell (kernel:persistent-cell 'store (lambda () (error 'head-sync "missing store"))))
     (define saved-store (unbox store-cell))
     (dynamic-wind
       (lambda () (set-box! store-cell #f))
       (lambda ()
         (check 'rename-failure-is-reported
                (test:raises? (lambda () (head:buffer-name-set! named "uncommitted"))) #t))
       (lambda () (set-box! store-cell saved-store)))
     (check 'failed-rename-does-not-change-presentation
            (head:buffer-name named) "<shared-rename>")

     ;; A rename subscriber can advance lifecycle and reenter a frame before
     ;; the call returns. Reconcile the canonical current record, even when
     ;; hiding and readmitting has retired the record passed to the setter.
     (for-each
       (lambda (kind)
         (let* ([source (head:new-buffer (format "rename-~a" kind))]
                [id (head:buffer-store-id source)]
                [pending (format "pending-~a" kind)]
                [final (format "final-~a" kind)]
                [token
                 (store:subscribe! id
                   (lambda (event)
                     (when (and (eq? (car event) 'rename) (string=? (caddr event) pending))
                       (case kind
                         [(rename) (store:rename! bot id final)]
                         [(hide readmit) (store:set-property! bot id 'audience '())]
                         [(delete) (store:delete! bot id)])
                       (head:before-frame!)
                       (when (eq? kind 'readmit)
                         (store:drop-property! bot id 'audience)
                         (head:adopt-store-buffer! id)
                         (store:rename! bot id final)))))]
                [visible? (and (memq kind '(rename readmit)) #t)])
           (head:set-window-buffer! w source)
           (head:buffer-name-set! source pending)
           (store:unsubscribe! token)
           (let ([current (head:buffer-of-store-id id)])
             (check (list kind 'rename-adopts-current-lifecycle)
                    (list (and current (head:buffer-name current))
                          (eq? current source) (store:exists? id)
                          (eq? (head:window-buffer w) source))
                    (list (and visible? final) (eq? kind 'rename)
                          (not (eq? kind 'delete)) (eq? kind 'rename)))
             (head:before-frame!)
             (check 'queued-rename-cannot-resurrect-retired-records
                    (eq? (head:buffer-of-store-id id) current) #t))))
       '(rename hide delete readmit))

     ;; Audience is a head lifecycle fact. Initial/private content never
     ;; gets a record or displaces a local label; later transitions adopt
     ;; current content and retire only this head's state.
     (define created-during-callback #f)
     (define creation-token
       (store:subscribe! #f
         (lambda (event)
           (when (string=? (store:buffer-name (cadr event)) "reentrant construction")
             (case (car event)
               [(create)
                (store:reset! bot (cadr event) '("subscriber content"))
                (head:adopt-store-buffer! (cadr event))]
               ;; All subscribers finish create before this reset callback;
               ;; the head has queued creation, but create! has not returned.
               [(reset)
                (head:before-frame!)
                (set! created-during-callback (head:buffer-of-store-id (cadr event)))])))))
     (define constructed (head:new-buffer "reentrant construction"))
     (store:unsubscribe! creation-token)
     (head:add-buffer! constructed)
     (check 'shared-construction-reuses-reentrant-adoption
            (list (eq? constructed created-during-callback)
                  (head:buffer-lines constructed)
                  (length (filter (lambda (b) (eqv? (head:buffer-store-id b)
                                                    (head:buffer-store-id constructed)))
                                  (head:buffers))))
            '(#t #("subscriber content") 1))
     (define other '(head "other"))
     (define private
       (store:create! head:ui-actor "<private>" '("seed")
                      (list (cons 'audience (list other)) '(wrap . #f))))
     (define local-tool (head:tool-buffer "private"))
     (head:before-frame!)
     (check 'private-creation-is-invisible
            (list (head:buffer-of-store-id private) (head:adopt-store-buffer! private)
                  (head:buffer-name local-tool)) '(#f #f "<private>"))
     (define renaming-tool (head:tool-buffer "private-renamed"))
     (store:rename! bot private "<private-renamed>")
     (head:before-frame!)
     (check 'hidden-rename-does-not-reserve-local-labels
            (head:buffer-name renaming-tool) "<private-renamed>")
     (define adoptions 0)
     (head:set-adopt-hook!
       (lambda (source)
         (set! adoptions (+ adoptions 1))
         (check 'adoption-is-canonical-before-callbacks
                (eq? source (head:adopt-store-buffer! (head:buffer-store-id source))) #t)))
     (define retained #f)
     (store:edit! bot private 0 (text:make-span 0 0 0 4) '("kept"))
     (define history (store:history private))
     (store:set-mark! bot private 'point '(0 . 1))
     (for-each
       (lambda (entry)
         (let ([author (car entry)] [audience (cadr entry)] [visible? (caddr entry)])
           (store:set-property! author private 'audience audience)
           (head:before-frame!)
           (check 'audience-transition
                  (and (head:buffer-of-store-id private) #t) visible?)
           (if visible?
               (begin
                 (let ([current (head:buffer-of-store-id private)])
                   (when retained
                     (check 'retired-record-cannot-duplicate-readoption
                            (test:raises? (lambda () (head:add-buffer! retained))) #t))
                   (set! retained current))
                 (head:set-window-buffer! w retained)
                 (head:before-frame!)
                 (check 'readmitted-content-and-facts
                        (list (head:buffer-lines retained) (head:buffer-fact retained 'wrap 'missing)
                              (store:mark head:ui-actor private 'point))
                        '(#("kept") #f (0 . 0))))
               (check 'retirement-preserves-store-and-rejects-redisplay
                      (list (store:line private 0) (store:mark bot private 'point)
                            (equal? (store:history private) history)
                            (store:mark head:ui-actor private 'point)
                            (eq? (head:window-buffer w) retained)
                            (test:raises? (lambda () (head:add-buffer! retained)))
                            (test:raises? (lambda () (head:set-window-buffer! w retained))))
                      '("kept" (0 . 1) #t #f #f #t #t)))))
       (list (list bot 'all #t) (list head:ui-actor '() #f)
             (list head:ui-actor (list head:ui-actor) #t)
             (list bot (list other) #f)))
     (check 'each-visible-lifetime-detects-once adoptions 2)
     ;; A superseded reveal/rename must never adopt or reserve its old name.
     (store:set-property! bot private 'audience 'all)
     (store:rename! bot private "<private>")
     (store:set-property! bot private 'audience '())
     (head:before-frame!)
     (check 'coalesced-transitions-read-current-truth
            (list adoptions (head:buffer-of-store-id private) (head:buffer-name local-tool))
            '(2 #f "<private>"))
     ;; Dropping the fact restores the default. A detection callback may
     ;; itself hide the buffer and reenter a frame without resurrection.
     (define cleanups 0)
     (head:add-buffer-kill-hook!
       (lambda (source)
         (when (eqv? (head:buffer-store-id source) private)
           (set! cleanups (+ cleanups 1)))))
     (head:set-adopt-hook!
       (lambda (source)
         (store:set-property! head:ui-actor private 'audience '())
         (head:before-frame!)))
     (store:drop-property! head:ui-actor private 'audience)
     (head:before-frame!)
     (check 'callback-retirement-is-not-resurrected-or-repeated
            (list (head:buffer-of-store-id private) cleanups (store:exists? private)) '(#f 1 #t))
     (head:set-adopt-hook! (lambda (source) (void)))

     ;; With no visible alternative, retirement creates a fresh scratch
     ;; without stealing the hidden scratch's still-reserved store label.
     (define last-visible (head:new-buffer "*scratch*<last>"))
     (head:set-buffers! (list last-visible))
     (head:set-window-buffer! w last-visible)
     (store:set-property! head:ui-actor (head:buffer-store-id last-visible) 'audience '())
     (head:before-frame!)
     (check 'last-visible-buffer-gets-a-visible-fallback
            (list (= (length (head:buffers)) 1)
                  (store:visible? head:ui-actor (head:buffer-store-id (head:window-buffer w)))
                  (eq? (head:window-buffer w) last-visible)
                  (store:exists? (head:buffer-store-id last-visible)))
            '(#t #t #f #t))

     (test:finish! 'head-sync)))
