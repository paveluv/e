#!/usr/bin/env scheme-script

;; Coherent adoption of shared text and positions.  Notifications wake
;; the head; the store snapshot's complete delta chain advances it.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (head checkpoint) checkpoint:)
       (prefix (core publication) publication:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (state actor) actor:)
             (prefix (state view) view:)
             (prefix (head interaction) interaction:)
             (prefix (core kernel) kernel:)
             (prefix (test) test:))

     (define check test:check)
     (store:log-retention 256)   ; the bound these checks exercise

     (define b (seat:window-buffer (seat:current-window)))
     (define id (seat:buffer-store-id b))
     (define w (seat:current-window))
     (define bot '(agent sync-test))
     (define (seat:point) (cons (seat:window-prow w) (seat:window-pcol w)))
     (define (edit! span replacement)
       (store:edit! bot id (store:revision id) span replacement))

     (seat:buffer-lines-set! b '#("aaa" "bbb" "ccc"))
     (seat:window-prow-set! w 1)
     (seat:window-pcol-set! w 2)
     (seat:window-top-set! w 1)
     (seat:buffer-spot-row-set! b 2)
     (seat:buffer-spot-col-set! b 2)
     (seat:buffer-spot-top-set! b 2)
     (head:before-frame!)

     (edit! (text:make-span 0 0 0 0) '("new" ""))
     (head:before-frame!)
     (check 'cursor-follows-content (seat:point) '(2 . 2))
     (check 'viewport-follows-content (seat:window-top w) 2)
     (check 'saved-position-follows-content
            (cons (seat:buffer-spot-row b) (seat:buffer-spot-col b)) '(3 . 2))
     (check 'saved-viewport-follows-content (seat:buffer-spot-top b) 3)
     (check 'published-point-agrees (store:mark head:ui-actor id 'point) (seat:point))
     (check 'text-and-revision-agree
            (let-values ([(text revision) (store:snapshot id)])
              (and (eq? text (seat:buffer-lines b))
                   (= revision (seat:buffer-store-rev b))))
            #t)

     ;; A reset clears the log, but a reader crosses it whole: the queued
     ;; pre-reset edit survives as a bridge, the reset is bridged by a line
     ;; diff of the two texts, and the post-reset edit follows, so the point
     ;; is carried, not clamped; its line replaced whole, it collapses to the
     ;; replacement's end, ab|cdef -> Xab|cdef -> text| -> YZtext|.
     (seat:buffer-lines-set! b '#("abcdef"))
     (seat:window-prow-set! w 0)
     (seat:window-pcol-set! w 2)
     (seat:window-top-set! w 0)
     (head:before-frame!)
     (edit! (text:make-span 0 0 0 0) '("X"))
     (store:reset! bot id '("text"))
     (edit! (text:make-span 0 0 0 0) '("YZ"))
     (head:before-frame!)
     (check 'reset-adopts-current-text (seat:buffer-lines b) '#("YZtext"))
     (check 'reset-carries-the-point-across-its-line-diff (seat:point) '(0 . 6))
     (check 'reset-publishes-the-carried-point
            (store:mark head:ui-actor id 'point) '(0 . 6))

     ;; Even a long queued stream is not evidence of a complete chain:
     ;; once the retained history is too short, resync explicitly.
     (do ([i 0 (+ i 1)]) ((= i 257))
       (edit! (text:make-span 0 0 0 0) '("x")))
     (head:before-frame!)
     (check 'truncated-history-does-not-replay-partial-history (seat:point) '(0 . 6))
     (check 'truncated-history-adopts-current-revision
            (seat:buffer-store-rev b) (store:revision id))
     (check 'truncated-history-adopts-all-text
            (string-length (vector-ref (seat:buffer-lines b) 0)) 263)

     (store:reset! bot id '(""))
     (head:before-frame!)
     (check 'resync-clamps-into-shorter-text (seat:point) '(0 . 0))
     (check 'resync-clamps-saved-viewport (seat:buffer-spot-top b) 0)

     ;; An explicit baseline reset also invalidates publication.  A
     ;; subscriber can edit after reset before the head adopts its snapshot;
     ;; unchanged numeric head coordinates must still replace the store's
     ;; cursor that followed that subscriber's insertion.
     (seat:buffer-lines-set! b '#("abcdef"))
     (seat:window-pcol-set! w 2)
     (head:before-frame!)
     (define reset-token
       (store:subscribe! id
         (lambda (event)
           (when (eq? (car event) 'reset)
             (edit! (text:make-span 0 0 0 0) '("Q"))))))
     (seat:buffer-lines-set! b '#("abcdef"))
     (store:unsubscribe! reset-token)
     (head:before-frame!)
     (check 'explicit-reset-adopts-the-subscribers-text (seat:buffer-lines b) '#("Qabcdef"))
     (check 'explicit-reset-keeps-clamped-coordinates (seat:point) '(0 . 2))
     (check 'explicit-reset-republishes-unchanged-coordinates
            (store:mark head:ui-actor id 'point) (seat:point))

     ;; Derived readers follow this head's adopted source, for either
     ;; owner. Repaint/fact changes are not content revisions, and a
     ;; store writer may be ahead of the text the head has adopted.
     (define (since source basis)
       (call-with-values (lambda () (seat:snapshot-since source basis)) list))
     (for-each
       (lambda (local?)
         (let ([source ((if local? seat:new-local-buffer! seat:new-buffer!) "source-history")])
           (seat:add-buffer! source)
           (seat:buffer-lines-set! source '#("alpha" "middle" "omega"))
           (let* ([initial (seat:edit-basis source)] [basis (caddr initial)])
             (seat:bump-buffer-revision! source)
             (seat:buffer-fact-set! source 'custom 'changed)
             (check 'repaint-and-facts-preserve-content-basis (seat:edit-basis source) initial)
             (seat:store-edit! source (text:make-span 0 0 0 0) '("before" ""))
             (seat:store-edit! source (text:make-span 3 5 3 5) '("!"))
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
                      (eq? (car snapshot) (seat:buffer-lines source)) #t)
               (check 'same-source-basis-is-empty (caddr (since source (cadr snapshot))) '())
               (check 'future-source-basis-is-unavailable (caddr (since source (+ (cadr snapshot) 1))) #f)
               (set-car! (car changes) -1)
               (check 'source-history-read-does-not-expose-its-list
                      (caar (caddr (since source basis))) (+ basis 1)))
             (unless local?
               (let ([snapshot (since source basis)] [id (seat:buffer-store-id source)])
                 (store:edit! bot id (store:revision id) (text:make-span 0 0 0 0) '("ahead" ""))
                 (check 'unadopted-store-text-does-not-leak-into-source-read
                        (since source basis) snapshot)
                 (head:before-frame!)
                 (check 'adoption-extends-source-chain (length (caddr (since source basis))) 3)
                 ;; Store history can disappear while the head's source
                 ;; and its already-adopted provenance remain coherent; a
                 ;; reset's line diff extends the chain by one step.
                 (store:reset! bot id '("reset"))
                 (check 'unadopted-reset-keeps-cached-source-chain
                        (length (caddr (since source basis))) 3)
                 (head:before-frame!)
                 (check 'adopted-reset-extends-source-chain-across-its-line-diff (length (caddr (since source basis))) 4)))
             (seat:buffer-lines-set! source '#("new baseline"))
             (seat:store-edit! source (text:make-span 0 0 0 0) '("suffix" ""))
             (check 'reset-with-new-edits-never-returns-a-partial-chain
                    (caddr (since source basis)) #f)
             (let ([basis (caddr (seat:edit-basis source))])
               (do ([i 0 (+ i 1)]) ((= i 256))
                 (seat:store-edit! source (text:make-span 0 0 0 0) '("x")))
               (check 'source-history-retains-256-changes (length (caddr (since source basis))) 256)
               (seat:store-edit! source (text:make-span 0 0 0 0) '("x"))
               (check 'expired-source-basis-is-unavailable (caddr (since source basis)) #f)
               (check 'retained-source-suffix-still-complete
                      (length (caddr (since source (+ basis 1)))) 256)))))
       '(#f #t))

     ;; A single adopted revision can contain several anchor steps, and a
     ;; rebaseline can jump revisions. Readers must receive every step.
     (for-each
       (lambda (read!)
         (let* ([id (store:create! bot "bridge-history" '("alpha" "middle" "omega")
                      '((base . "alpha\nmiddle\nomega") (trailing . #f)))]
                [source (seat:adopt-store-buffer! id)])
           (seat:add-buffer! source)
           (seat:store-edit! source (text:make-span 1 6 1 6) '("!"))
           (let ([basis (seat:edit-basis source)])
             (read! bot id '("ALPHA" "middle" "OMEGA") '((base . "ALPHA\nmiddle\nOMEGA") (trailing . #f)))
             (head:before-frame!)
             (let-values ([(text revision changes) (seat:snapshot-since source (caddr basis))])
               (check 'source-history-keeps-multiple-steps-and-revision-jumps
                 (and changes (> (length changes) 1)
                      (equal? text (fold-left
                                     (lambda (lines entry)
                                       (let-values ([(next delta) (text:apply-edit lines (text:delta-span (caddr entry)) (text:delta-inserted (caddr entry)))]) next))
                                     (car basis) changes))) #t)))))
       (list store:reload! store:reread!))

     ;; Shared names come from the store, including hidden reservations.
     ;; Rename commits before changing the head; local tools yield to the
     ;; accepted shared label, and failures leave the cached name intact.
     (define hidden-name (store:create! bot "claimed" '("") '((audience))))
     (define named (seat:new-buffer! "claimed"))
     (define rival (seat:new-buffer! "claimed"))
     (seat:buffer-name-set! rival "claimed")
     (check 'shared-construction-and-rename-adopt-store-claims
            (list (seat:buffer-name named) (seat:buffer-name rival)) '("claimed<2>" "claimed<3>"))
     (define rename-tool (let ([b (seat:new-local-buffer! "shared-rename")]) (seat:buffer-fact-set! b 'tool-key "shared-rename") (seat:add-buffer! b)))
     (seat:buffer-name-set! named "<shared-rename>")
     (check 'accepted-shared-name-displaces-local-label
            (list (seat:buffer-name named) (seat:buffer-name rename-tool)
                  (eq? (seat:find-tool-buffer "shared-rename") rename-tool))
            '("<shared-rename>" "<shared-rename 2>" #t))
     (define store-cell (kernel:persistent-cell 'store (lambda () (error 'head-sync "missing store"))))
     (define saved-store (unbox store-cell))
     (dynamic-wind
       (lambda () (set-box! store-cell #f))
       (lambda ()
         (check 'rename-failure-is-reported
                (test:raises? (lambda () (seat:buffer-name-set! named "uncommitted"))) #t))
       (lambda () (set-box! store-cell saved-store)))
     (check 'failed-rename-does-not-change-presentation
            (seat:buffer-name named) "<shared-rename>")

     ;; A rename subscriber can advance lifecycle and reenter a frame before
     ;; the call returns. Reconcile the canonical current record, even when
     ;; hiding and readmitting has retired the record passed to the setter.
     (for-each
       (lambda (kind)
         (let* ([source (seat:new-buffer! (format "rename-~a" kind))]
                [id (seat:buffer-store-id source)]
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
                         (seat:adopt-store-buffer! id)
                         (store:rename! bot id final)))))]
                [visible? (and (memq kind '(rename readmit)) #t)])
           (seat:set-window-buffer! w source)
           (seat:buffer-name-set! source pending)
           (store:unsubscribe! token)
           (let ([current (seat:buffer-of-store-id id)])
             (check (list kind 'rename-adopts-current-lifecycle)
                    (list (and current (seat:buffer-name current))
                          (eq? current source) (store:exists? id)
                          (eq? (seat:window-buffer w) source))
                    (list (and visible? final) (eq? kind 'rename)
                          (not (eq? kind 'delete)) (eq? kind 'rename)))
             (head:before-frame!)
             (check 'queued-rename-cannot-resurrect-retired-records
                    (eq? (seat:buffer-of-store-id id) current) #t))))
       '(rename hide delete readmit))

     ;; Audience is a head lifecycle fact. Initial/private content never
     ;; gets a record or displaces a local label; later transitions adopt
     ;; current content and retire only this head's state.
     (define created-during-callback #f)
     (define creation-state #f)
     (define creation-token
       (store:subscribe! #f
         (lambda (event)
           (when (string=? (store:buffer-name (cadr event)) "reentrant construction")
             (case (car event)
               [(create)
                (set! creation-state
                  (list (call-with-values (lambda () (store:snapshot (cadr event))) list)
                        (map (lambda (key) (store:property (cadr event) key 'missing))
                          '(base trailing mode mode-auto wrap modified))))
                (store:edit! bot (cadr event) 0 (text:make-span 0 7 0 7) '(" subscriber"))
                (seat:adopt-store-buffer! (cadr event))]
               ;; All subscribers finish create before this edit callback;
               ;; the head has queued creation, but create! has not returned.
               [(edit)
                (head:before-frame!)
                (set! created-during-callback (seat:buffer-of-store-id (cadr event)))])))))
     (define initial-lines (vector "initial"))
     (define initial-facts
       (list (cons 'base (string-copy "initial")) '(trailing . #f) '(mode . #f) '(mode-auto . #f) '(wrap . #f)))
     (define constructed (seat:new-buffer! "reentrant construction" initial-lines initial-facts))
     (store:unsubscribe! creation-token)
     (vector-set! initial-lines 0 "lost")
     (string-set! (cdar initial-facts) 0 #\X)
     (seat:add-buffer! constructed)
     (check 'shared-construction-publishes-owned-inputs-and-reuses-reentrant-adoption
            (list creation-state (eq? constructed created-during-callback)
                  (seat:buffer-lines constructed)
                  (seat:buffer-base constructed) (length (store:history (seat:buffer-store-id constructed)))
                  (length (filter (lambda (b) (equal? (seat:buffer-store-id b) (seat:buffer-store-id constructed)))
                                  (seat:buffers))))
            '(((#("initial") 0) ("initial" #f #f #f #f #f)) #t #("initial subscriber") "initial" 1 1))
     (define other '(head "other"))
     (define private
       (store:create! head:ui-actor "<private>" '("seed")
                      (list (cons 'audience (list other)) '(wrap . #f))))
     (define local-tool (let ([b (seat:new-local-buffer! "private")]) (seat:buffer-fact-set! b 'tool-key "private") (seat:add-buffer! b)))
     (head:before-frame!)
     (check 'private-creation-is-invisible
            (list (seat:buffer-of-store-id private) (seat:adopt-store-buffer! private)
                  (seat:buffer-name local-tool)) '(#f #f "<private>"))
     (define renaming-tool (let ([b (seat:new-local-buffer! "private-renamed")]) (seat:buffer-fact-set! b 'tool-key "private-renamed") (seat:add-buffer! b)))
     (store:rename! bot private "<private-renamed>")
     (head:before-frame!)
     (check 'hidden-rename-does-not-reserve-local-labels
            (seat:buffer-name renaming-tool) "<private-renamed>")
     (define adoptions 0)
     (seat:set-adopt-hook!
       (lambda (source)
         (set! adoptions (+ adoptions 1))
         (check 'adoption-is-canonical-before-callbacks
                (eq? source (seat:adopt-store-buffer! (seat:buffer-store-id source))) #t)))
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
                  (and (seat:buffer-of-store-id private) #t) visible?)
           (if visible?
               (begin
                 (let ([current (seat:buffer-of-store-id private)])
                   (when retained
                     (check 'retired-record-cannot-duplicate-readoption
                            (test:raises? (lambda () (seat:add-buffer! retained))) #t))
                   (set! retained current))
                 (seat:set-window-buffer! w retained)
                 (head:before-frame!)
                 (check 'readmitted-content-and-facts
                        (list (seat:buffer-lines retained) (seat:buffer-fact retained 'wrap 'missing)
                              (store:mark head:ui-actor private 'point))
                        '(#("kept") #f (0 . 0))))
               (check 'retirement-preserves-store-and-rejects-redisplay
                      (list (store:line private 0) (store:mark bot private 'point)
                            (equal? (store:history private) history)
                            (store:mark head:ui-actor private 'point)
                            (eq? (seat:window-buffer w) retained)
                            (test:raises? (lambda () (seat:add-buffer! retained)))
                            (test:raises? (lambda () (seat:set-window-buffer! w retained))))
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
            (list adoptions (seat:buffer-of-store-id private) (seat:buffer-name local-tool))
            '(2 #f "<private>"))
     ;; Dropping the fact restores the default. A detection callback may
     ;; itself hide the buffer and reenter a frame without resurrection.
     (define cleanups 0)
     (seat:add-buffer-kill-hook!
       (lambda (source)
         (when (equal? (seat:buffer-store-id source) private)
           (set! cleanups (+ cleanups 1)))))
     (seat:set-adopt-hook!
       (lambda (source)
         (store:set-property! head:ui-actor private 'audience '())
         (head:before-frame!)))
     (store:drop-property! head:ui-actor private 'audience)
     (head:before-frame!)
     (check 'callback-retirement-is-not-resurrected-or-repeated
            (list (seat:buffer-of-store-id private) cleanups (store:exists? private)) '(#f 1 #t))
     (seat:set-adopt-hook! (lambda (source) (void)))

     ;; A lifecycle burst can exceed the retained invalidation set. Rescan
     ;; both current inventory and old head records, so deleted/hidden ids
     ;; retire while newly visible sources and local tools keep their identity.
     (let* ([deleted (seat:new-buffer! "overflow-deleted")]
            [hidden (seat:new-buffer! "overflow-hidden")]
            [fresh (store:create! bot "overflow-fresh" '("latest"))])
       (head:before-frame!)
       (let ([adopted (seat:buffer-of-store-id fresh)])
         (store:delete! bot (seat:buffer-store-id deleted))
         (store:set-property! bot (seat:buffer-store-id hidden) 'audience '())
         (do ([i 0 (+ i 1)]) ((= i 257))
           (let ([id (store:create! bot "temporary" '(""))]) (store:delete! bot id)))
         (store:rename! bot fresh "overflow-renamed")
         (store:reset! bot fresh '("after overflow"))
         (store:create! bot "overflow-new" '("created after overflow"))
         (head:before-frame!)
         (check 'overflow-adopts-current-inventory-and-retires-stale-records
           (list (seat:buffer-of-store-id (seat:buffer-store-id deleted))
                 (seat:buffer-of-store-id (seat:buffer-store-id hidden))
                 (eq? adopted (seat:buffer-of-store-id fresh))
                 (seat:buffer-name adopted) (seat:buffer-lines adopted)
                 (seat:buffer-lines (seat:buffer-of-store-id (store:find-named "overflow-new")))
                 (eq? local-tool (seat:find-tool-buffer "private")))
           '(#f #f #t "overflow-renamed" #("after overflow") #("created after overflow") #t))))

     ;; With no visible alternative, retirement creates a fresh scratch
     ;; without stealing the hidden scratch's still-reserved store label.
     (define last-visible (seat:new-buffer! "*scratch*<last>"))
     (seat:set-buffers! (list last-visible))
     (seat:set-window-buffer! w last-visible)
     (store:set-property! head:ui-actor (seat:buffer-store-id last-visible) 'audience '())
     (head:before-frame!)
     (check 'last-visible-buffer-gets-a-visible-fallback
            (list (= (length (seat:buffers)) 1)
                  (store:visible? head:ui-actor (seat:buffer-store-id (seat:window-buffer w)))
                  (eq? (seat:window-buffer w) last-visible)
                  (store:exists? (seat:buffer-store-id last-visible)))
            '(#t #t #f #t))

     ;; A document's placements own separate retained views, even while one
     ;; switches away. The window adapters and explicit descriptor agree.
     (let* ([source (seat:new-buffer! "independent selections" '#("alpha" "beta") '())]
            [other (seat:new-buffer! "other document")]
            [one (seat:current-window)])
       (seat:set-window-buffer! one source)
       (seat:window-pcol-set! one 2)
       (seat:buffer-mark-col-set! source 1)
       (seat:buffer-marked-set! source #t)
       (seat:window-wrap-set! one #f)
       (let* ([first (seat:window-editor one)]
              [two (seat:make-window source 0 0 0 0 2 10 0 40 'default)]
              [second (seat:window-editor two)])
         (seat:set-layout-root! (seat:make-layout-split 'right one two 1 1))
         (seat:with-window two
           (seat:window-prow-set! two 1)
           (seat:buffer-marked-set! source #f))
         (seat:set-window-buffer! one other)
         (seat:set-window-buffer! one source)
         (check 'placements-retain-independent-view-state
           (list (equal? first second) (equal? first (seat:window-editor one))
             (view:state (interaction:snapshot first)) (view:state (interaction:snapshot second)))
           '(#f #t ((0 . 2) (0 . 1) (0 . 0) #t) ((1 . 2) (0 . 1) (0 . 0) #f)))
         (check 'placement-wrap-preference-survives-switching
           (list (seat:window-wrap one) (seat:window-wrap two)) '(#f default))
         (seat:checkpoint!)
         (let ([state (actor:checkpoint head:ui-actor)])
           (check 'checkpoint-retains-views-not-copied-window-anchors
             (list (cadr state)
               (exists (lambda (entry)
                         (exists (lambda (p) (or (integer? (car p)) (pair? (car p)))) (caddr entry))) (list-ref state 4)))
             '(6 #f)))
         (seat:set-layout-root! one)
         (check 'closed-placement-releases-ownership
           (list (view:owner (view:snapshot second)) (and (interaction:snapshot first) #t)) '(#f #t))))

     ;; Resume follows the saved basis, not the fresh process's initial
     ;; cache. The table covers rebasing, old versions and unavailable views.
     (let ([b (seat:new-buffer! "resume positions")])
       (seat:set-buffers! (list b))
       (seat:set-layout-root! (seat:current-window))
       (seat:set-window-buffer! (seat:current-window) b)
       (for-each
         (lambda (kind expected)
           (seat:store-reset! b '("zero" "middle" "last"))
           (let ([w (seat:current-window)] [id (seat:buffer-store-id b)])
             (seat:window-prow-set! w 1)
             (seat:window-pcol-set! w 3)
             (seat:window-top-set! w 1)
             (seat:buffer-mark-row-set! b 2)
             (seat:buffer-mark-col-set! b 2)
             (seat:buffer-marked-set! b #t)
             (seat:buffer-spot-row-set! b 2)
             (seat:buffer-spot-col-set! b 4)
             (seat:buffer-spot-top-set! b 1)
             (seat:set-copy-text! (string-copy "saved kill"))
             (seat:checkpoint!)
             (case kind
               [(missing-provider)
                (let ([state (actor:checkpoint head:ui-actor)])
                  (set-car! (car (list-ref state 4)) '(uninstalled-view "old view"))
                  (actor:checkpoint! head:ui-actor state))]
               [(edit) (store:edit! bot id (store:revision id) (text:make-span 0 0 0 0) '("new" ""))]
               [(reset) (store:reset! bot id '("x"))]
               [(expired)
                (do ([i 0 (+ i 1)]) ((= i 257))
                  (store:edit! bot id (store:revision id) (text:make-span 0 0 0 0) '("x")))])
             ;; A fresh placement has its own editor view. Mutating w here
             ;; would now change the saved view itself, not just a throwaway
             ;; process-local coordinate as it did before view-owned state.
             (let ([fresh (seat:make-window b 0 0 0 0 0 20 0 80 'default)])
               (seat:set-layout-root! fresh)
               (seat:set-current! fresh))
             ;; the copy buffer is the base's, so the resume leaves its text as it stands
             (seat:set-copy-text! "as the base has it")
             (let ([truth (call-with-values (lambda () (store:snapshot-state id)) list)])
               (check (list 'resume-from-saved-revision kind)
                 (list (seat:resume!)
                       (map cdr (seat:buffer-placements b)) (seat:buffer-marked b) (seat:copy-text)
                       (equal? truth (call-with-values (lambda () (store:snapshot-state id)) list)))
                 (list #t expected #t "as the base has it" #t)))))
         '(edit reset expired missing-provider)
         '(((3 . 4) (2 . 0) (3 . 2) (2 . 3) (2 . 0))
           ((0 . 1) (0 . 0) (0 . 1) (0 . 1) (0 . 0))
           ((2 . 4) (1 . 0) (2 . 2) (1 . 3) (1 . 0))
           ((2 . 4) (1 . 0) (2 . 2) (1 . 3) (1 . 0))))
       ;; The unchanged-frame comparison owns its data too.
       (seat:set-copy-text! "Xaved kill")
       (seat:checkpoint!)
       (check 'the-copy-text-lives-in-the-base-not-the-checkpoint
         (list (store:line (seat:buffer-store-id (seat:copy-buffer)) 0)
               (exists (lambda (entry)
                         (let ([reference (car entry)])
                           (and (pair? reference) (eq? (car reference) 'local) (equal? (cadr reference) "<copy>"))))
                       (list-ref (actor:checkpoint head:ui-actor) 4)))
         '("Xaved kill" #f)))

     ;; Checkpoint transmission can stall without blocking capture. Exercise
     ;; coalescing against the real store's retained-text behavior; gates, not
     ;; timing thresholds, delimit the in-flight and replaceable snapshots.
     (let* ([owner '(head "queued checkpoints")]
            [entered (test:gate)] [release (test:gate)] [sent (test:recorder)]
            [one (checkpoint:text #f '#("one"))] [two (checkpoint:text #f '#("two"))]
            [writer (checkpoint:make!
                      (lambda (state)
                        (sent state) (entered #t)
                        (test:await 'checkpoint-release release)
                        (actor:checkpoint! owner state))
                      void)])
       (define (screen point text)
         (list 'screen 4 point '(layout)
           (if text (list (list (list 'local (string-copy "<draft>") 1 '() text) #f '())) '())))
       (define (payload state) (and (pair? (list-ref state 4)) (list-ref (caar (list-ref state 4)) 4)))
       (actor:register! owner void)
       (publication:submit! writer (screen 0 one))
       (test:await 'checkpoint-started entered)
       (do ([point 1 (+ point 1)]) ((= point 100))
         (publication:submit! writer (screen point two)))
       (let ([last (screen 100 two)])
         (publication:submit! writer last)
         (set-car! (cddr last) 'mutated)
         (string-set! (cadr (caar (list-ref last 4))) 1 #\X))
       (let* ([flushing (test:gate)] [flushed (test:gate)]
              [join (test:worker (lambda () (flushing #t) (publication:flush! writer) (flushed #t)))])
         (test:await 'checkpoint-flush-started flushing)
         (check 'checkpoint-flush-waits-for-acknowledgement (flushed) #f)
         (release #t) (join))
       (check 'checkpoint-burst-keeps-only-latest-owned-state-and-full-unsent-text
         (list (map caddr (sent)) (map payload (sent)) (caddr (actor:checkpoint owner))
               (publication:submit! writer (screen 100 two)))
         '((0 100) (("one") ("two")) 100 #f))
       (publication:submit! writer (screen 101 two))
       (publication:flush! writer)
       (check 'checkpoint-omits-only-acknowledged-text (payload (car (reverse (sent)))) 'kept)
       ;; Removing and reintroducing a local while removal is in flight
       ;; must not keep text from the older acknowledgement that still has it.
       (release #f) (entered #f)
       (publication:submit! writer (screen 102 #f))
       (test:await 'checkpoint-removal-started entered)
       (publication:submit! writer (screen 103 two))
       (release #t) (publication:flush! writer)
       (check 'checkpoint-reintroduction-restores-text-after-in-flight-removal
         (list (payload (car (reverse (sent)))) (payload (actor:checkpoint owner)))
         '(("two") ("two")))
       (publication:submit! writer (screen 104 (checkpoint:text #f '#("new buffer"))))
       (publication:flush! writer)
       (check 'checkpoint-reused-name-and-revision-do-not-reuse-old-text
         (payload (actor:checkpoint owner)) '("new buffer"))
       (actor:detach! owner))

     ;; Delivery errors wake the head and fail every subsequent fence/offer.
     ;; Pending replacements are not sent after an uncertain failed write.
     (let* ([entered (test:gate)] [release (test:gate)] [notified (test:gate)]
            [sent (test:recorder)] [failure (make-error)]
            [writer (checkpoint:make!
                      (lambda (state) (sent state) (entered #t)
                        (test:await 'checkpoint-failure-release release) (raise failure))
                      (lambda () (notified #t)))]
            [first '(screen 4 0 (layout) ())] [last '(screen 4 1 (layout) ())])
       (publication:submit! writer first)
       (test:await 'checkpoint-failure-started entered)
       (publication:submit! writer last)
       (release #t)
       (test:await 'checkpoint-failure-notified notified)
       (check 'checkpoint-failure-is-observed-without-retry-or-later-writes
         (list (sent)
               (map (lambda (operation) (test:raises? operation (lambda (ex) (eq? ex failure))))
                 (list (lambda () (publication:flush! writer))
                       (lambda () (publication:submit! writer last))
                       (lambda () (publication:changed? writer last)))))
         (list (list first) '(#t #t #t))))

     ;; One frame deadline serves both outer and nested pumps. Multiple
     ;; providers choose the earliest, the head owns its time value, and a
     ;; consumed request cannot keep repainting. The final worker only bounds
     ;; the test if deadline delivery regresses; join it before the next row.
     (for-each
       (lambda (outer?)
         (define posted-context #t)
         ;; Drain earlier store wakes before timing this isolated request.
         (call/cc
           (lambda (done)
             (head:run-on-main!
               (lambda () (set! posted-context (head:in-main-pump)) (done #t)))
             (parameterize ([head:in-main-pump #t]) (head:read-key-event))))
         (let ([first? #t] [frames 0] [outer-callback? #f] [finished? (test:gate)])
           (parameterize ([kernel:registering-module 'frame-deadline-test])
             (head:add-pre-redraw-hook!
               (lambda ()
                 (when first?
                   (set! first? #f)
                   (let* ([now (current-time 'time-monotonic)]
                          [early (add-duration now (make-time 'time-duration 10000000 0))]
                          [late (add-duration now (make-time 'time-duration 0 10))])
                     (head:request-frame-at! late)
                     (head:request-frame-at! early)
                     (head:request-frame-at! late)
                     (set-time-second! early (time-second late)))))))
           (head:before-frame!)
           (let ([stop (test:worker
                         (lambda ()
                           (sleep (make-time 'time-duration 200000000 0))
                           (finished? #t)
                           (head:wake-main!)))])
             (dynamic-wind
               void
               (lambda ()
                 (call/cc
                   (lambda (done)
                     (head:set-frame-hook!
                       (lambda (coalesce?)
                         (set! outer-callback? (or outer-callback? (head:in-main-pump)))
                         (head:before-frame!)
                         (if (finished?) (done #t)
                             (begin
                               (set! frames (+ frames 1))
                               (when (= frames 1)
                                 (head:wake-main!)
                                 (head:wake-main!))))))
                     (parameterize ([head:in-main-pump outer?]) (head:read-key-event)))))
               (lambda ()
                 (head:set-frame-hook! void)
                 (kernel:retract-module! 'frame-deadline-test)
                 (stop)))
             (check (list 'frame-deadline-and-coalesced-wake outer?)
               (list frames posted-context outer-callback?) '(2 #f #f)))))
       '(#t #f))

     (test:finish! 'head-sync)))
