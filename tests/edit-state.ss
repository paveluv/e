#!/usr/bin/env scheme-script

;; Shared dirty state and atomic facts, including save callbacks, failed
;; store access, and the common validation boundary for either text owner.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (edit) init!)
             (prefix (head) head:)
             (prefix (store) store:)
             (prefix (property) property:)
             (prefix (text) text:)
             (prefix (file) file:)
             (prefix (log) log:)
             (prefix (string) string:)
             (prefix (mode) mode:)
             (prefix (kernel) kernel:))

     (define checks 0)
     (define bot '(agent state-test))
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'edit-state-test (format "~s" label) actual expected)))
     (define (raises? thunk) (guard (ex [else #t]) (thunk) #f))
     (define (fresh name shared?)
       (let ([b ((if shared? head:new-buffer head:new-local-buffer) name)])
         (show-buffer! b)
         (goto-point! '(0 . 0))
         b))
     (define (state b)
       (let-values ([(text revision facts) (head:buffer-state b)])
         (list text revision
               (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) facts)
               (head:buffer-name b))))
     (define (insert! id at replacement)
       (store:edit! bot id (store:revision id)
                    (text:make-span 0 at 0 at) (list replacement)))
     (define (interrupt-during! change! thunk)
       ;; Resume in place so the real file port keeps its normal unwind owner.
       (let ([handler (timer-interrupt-handler)] [interrupted? #f])
         (dynamic-wind
           (lambda () (timer-interrupt-handler (lambda () (set! interrupted? #t) (change!))))
           (lambda () (let ([result (thunk)]) (set-timer 0) (list interrupted? result)))
           (lambda () (set-timer 0) (timer-interrupt-handler handler)))))

     ;; Store authors cannot bypass dirty state by avoiding command code.
     (define b (fresh "shared-state" #t))
     (define id (head:buffer-store-id b))
     (check 'empty-scratch-is-clean (store:property id 'modified) #f)
     (insert! id 0 "agent work")
     (check 'foreign-edit-is-dirty-before-adoption (store:property id 'modified) #t)
     (check 'head-still-has-old-empty-cache (head:buffer-lines b) '#(""))
     (check 'discard-reads-current-text-not-empty-cache (buffer-clean? b) #f)
     (head:before-frame!)
     (check 'adoption-retains-dirty-state (head:buffer-modified b) #t)
     (store:undo! bot id)
     (check 'foreign-undo-to-empty-is-clean (store:property id 'modified) #f)
     (store:redo! bot id)
     (check 'foreign-redo-is-dirty (store:property id 'modified) #t)
     (store:set-properties! bot id '((base . "agent work\n") (trailing . #t)))
     (check 'matching-disk-baseline-is-clean (store:property id 'modified) #f)
     (head:before-frame!)
     (check 'late-adoption-cannot-redirty-a-save (head:buffer-modified b) #f)
     (store:set-property! bot id 'trailing #f)
     (check 'final-newline-only-change-is-dirty (store:property id 'modified) #t)
     (store:set-property! bot id 'trailing #t)
     (check 'restoring-final-newline-is-clean (store:property id 'modified) #f)
     (insert! id 10 "!")
     (store:undo! bot id)
     (check 'store-undo-to-saved-text-is-clean (store:property id 'modified) #f)
     (check 'modification-facts-belong-to-the-store
       (map (lambda (key)
              (list (raises? (lambda () (store:set-property! bot id key #f)))
                    (raises? (lambda () (store:drop-property! bot id key))))) property:edit-keys)
       '((#t #t) (#t #t)))
     (define equivalent (store:create! bot "equivalent-lines" '("line" "")))
     (store:set-properties! bot equivalent '((trailing . #f) (base . "line\n")))
     (check 'equivalent-line-representation-is-clean (store:property equivalent 'modified) #f)
     (store:set-property! bot equivalent 'trailing #t)
     (check 'extra-blank-line-is-dirty (store:property equivalent 'modified) #t)

     ;; A newly created buffer with content is unsaved even before any edit.
     (define born (store:create! bot "born-with-work" '("new work")))
     (check 'creation-with-content-is-dirty (store:property born 'modified) #t)
     (head:before-frame!)
     (define adopted (head:buffer-of-store-id born))
     (check 'newly-adopted-work-needs-protection (buffer-clean? adopted) #f)
     (head:buffer-read-only-set! adopted #t)
     (check 'read-only-does-not-authorize-disposal (buffer-clean? adopted) #f)
     (head:buffer-fact-set! adopted 'disposable #t)
     (check 'generated-output-explicitly-allows-disposal (buffer-clean? adopted) #t)
     (define local-work (fresh "local-work" #f))
     (insert-text! "keep")
     (head:buffer-read-only-set! local-work #t)
     (check 'local-read-only-work-is-protected (buffer-clean? local-work) #f)
     (check 'snapshot-tools-declare-disposal
            (head:buffer-fact (fresh-buffer "state-generated") 'disposable #f) #t)

     ;; One sequence covers clock ownership and no-op/save preservation for
     ;; shared and local buffers. Actual changes must fall within UTC bounds.
     (define (utc-nanos)
       (let ([now (current-time 'time-utc)])
         (+ (* (time-second now) 1000000000) (time-nanosecond now))))
     (define time-steps
       '((insert . #t) (same . #f) (metadata . #f) (save . #f) (newline . #t)
         (edit . #t) (undo . #t) (redo . #t) (reset . #t) (reset . #f)
         (representation . #t) (representation . #f)))
     (check 'modification-times-follow-content-in-either-owner
       (map
         (lambda (shared?)
           (let ([b (fresh (if shared? "timed-shared" "timed-local") shared?)])
             (reverse
               (fold-left
                 (lambda (results step)
                   (let ([before (head:buffer-modified-at b)] [started (utc-nanos)])
                     (case (car step)
                       [(insert) (insert-text! "a")]
                       [(same) (head:store-edit! b (text:make-span 0 0 0 1) '("a"))]
                       [(metadata) (head:buffer-fact-set! b 'status "metadata")]
                       [(save) (head:buffer-facts-set! b
                                 (append '((base . "a\n")) (if shared? '() '((modified . #f)))))]
                       [(newline) (head:buffer-trailing-set! b #f)]
                       [(edit) (insert-text! "b")]
                       [(undo) (undo!)] [(redo) (redo!)]
                       [(reset) (head:store-reset! b '("reset")
                                  (append '((base . "reset\n") (trailing . #t))
                                    (if shared? '() '((modified . #f)))))]
                       ;; Equal saved bytes can still change the displayed
                       ;; line structure, just as an ordinary edit can.
                       [(representation) (head:store-reset! b '("reset" "") '((trailing . #f)))])
                     (let ([after (head:buffer-modified-at b)])
                       (cons (cons (car step)
                               (if (cdr step)
                                   (and (integer? after) (exact? after) (<= started after (utc-nanos))
                                        (not (equal? before after)))
                                   (equal? before after))) results))))
                 '() time-steps)))) '(#t #f))
       (make-list 2 (map (lambda (step) (cons (car step) #t)) time-steps)))

     ;; Text and facts are one immutable snapshot.  Every property callback
     ;; observes all members of a batch, never a half-published file state.
     (define observations '())
     (define token
       (store:subscribe! id
         (lambda (event)
           (when (eq? (car event) 'property)
             (set! observations
               (cons (list (store:property id 'base) (store:property id 'stamp)
                           (store:property id 'modified)) observations))))))
     (store:set-properties! bot id '((base . "agent work\n") (stamp . 123)))
     (store:unsubscribe! token)
     (check 'property-batch-observers-see-complete-state
            observations '(("agent work\n" 123 #f) ("agent work\n" 123 #f)))
     (define captured (state b))
     (insert! id 10 "!")
     (check 'captured-state-keeps-its-text (car captured) '#("agent work"))
     (check 'captured-state-keeps-its-dirty-fact (cdr (assq 'modified (caddr captured))) #f)
     (check 'current-state-moved-on (store:property id 'modified) #t)
     (check 'absent-fact-uses-default (head:buffer-fact b 'missing 'fallback) 'fallback)
     (head:buffer-fact-set! b 'explicit-false #f)
     (check 'explicit-false-does-not-use-default (head:buffer-fact b 'explicit-false 'fallback) #f)

     ;; Non-undoable external facts commit with an edit.  The inverse only
     ;; restores text-related facts, so a merge never forgets its disk base.
     (define merged (fresh "merged-state" #t))
     (define merged-id (head:buffer-store-id merged))
     (head:store-reset! merged '("mine") '((base . "old\n") (trailing . #t)))
     (store:edit! bot merged-id (store:revision merged-id) (text:make-span 0 0 0 4) '("disk")
                  '(merge "merge" ((trailing . #f)) ((base . "disk") (stamp . 456) (stale . #f))
                     ((base . "old\n") (trailing . #t))))
     (check 'merge-text-and-new-baseline-are-clean (store:property merged-id 'modified) #f)
     (store:undo! bot merged-id)
     (check 'undo-keeps-the-incorporated-disk-baseline
            (list (store:property merged-id 'base) (store:property merged-id 'stamp)) '("disk" 456))
     (check 'undo-restores-text-and-trailing
            (list (store:line merged-id 0) (store:property merged-id 'trailing)) '("mine" #t))
     (check 'undo-relative-to-new-disk-is-dirty (store:property merged-id 'modified) #t)
     (store:redo! bot merged-id)
     (check 'redo-relative-to-new-disk-is-clean (store:property merged-id 'modified) #f)

     ;; The head must not adopt or clean up a refused reset, including when
     ;; its cache has not seen the racing edit. Local runtime facts stay local.
     (for-each
       (lambda (shared?)
         (let* ([b (fresh (if shared? "reset-shared" "reset-local") shared?)]
                [id (head:buffer-store-id b)])
           (define (head-state)
             (list (head:edit-basis b) (head:buffer-history b) (point)
                   (head:buffer-marked b) (head:buffer-mark-row b) (head:buffer-mark-col b)))
           (insert-text! "keep")
           (head:buffer-marked-set! b #t)
           (unless shared? (head:buffer-fact-set! b 'source (head:current)))
           (check 'reviewed-reset-refusal-keeps-either-owner-and-head-state
             (map
               (lambda (change)
                 (let-values ([(text revision facts) (head:buffer-state b)])
                   (case change
                     [(text) (if id (insert! id 0 "new ") (insert-text! "new "))]
                     [(facts) (head:buffer-trailing-set! b #f)])
                   (let* ([before (state b)] [view (head-state)]
                          [accepted (head:store-reset! b '("lost") '((base . "lost"))
                                                       (cons revision facts))])
                     (list accepted (equal? before (state b)) (equal? view (head-state))))))
               '(text facts))
             '((#f #t #t) (#f #t #t)))
           (let-values ([(text revision facts) (head:buffer-state b)])
             (let ([accepted (head:store-reset! b '("disk") '((base . "disk\n") (trailing . #t))
                               (cons revision facts))])
               (check 'fresh-review-adopts-a-new-baseline-in-either-owner
                 (list accepted (head:buffer-lines b) (point))
                 (list (+ revision 1) '#("disk") '(0 . 4)))))
           ;; A fact predicate distinguishes absence from false or an empty
           ;; value. Local runtime metadata need not become shared plain data.
           (let ([expected (property:select (caddr (state b)) '(base missing))])
             (head:buffer-fact-set! b 'missing #f)
             (let ([before (state b)] [view (head-state)])
               (check 'conditional-facts-and-edits-refuse-in-either-owner
                 (list (head:buffer-facts-set! b '((base . "lost")) expected "lost name")
                       (raises? (lambda ()
                                  (head:store-edit! b (text:make-span 0 0 0 0) '("lost")
                                    (list 'merge "merge" '() '((base . "lost")) expected))))
                       (equal? before (state b)) (equal? view (head-state)))
                 '(#f #t #t #t)))
             (head:buffer-fact-set! b 'missing '())
             (check 'fresh-fact-review-publishes-in-either-owner
               (list (head:buffer-facts-set! b '((stamp . 1))
                       (property:select (caddr (state b)) '(base missing absent))
                       "accepted facts")
                     (head:buffer-name b))
               (list #t (if shared? "accepted facts" "<accepted facts>"))))))
       '(#t #f))

     ;; Validate all inputs before either owner changes text or facts.
     (mode:register! "invalid-line-output" '() '() (lambda (line) #f))
     (register-formatter! "invalid-line-output" (lambda args '("embedded\nnewline")))
     (for-each
       (lambda (shared?)
         (let ([b (fresh (if shared? "validate-shared" "validate-local") shared?)])
           (head:store-reset! b '())
           (check 'empty-list-normalizes (head:buffer-lines b) '#(""))
           (head:store-reset! b '#())
           (check 'empty-vector-normalizes (head:buffer-lines b) '#(""))
           (head:store-reset! b '("one" "two"))
           (check 'line-list-normalizes (head:buffer-lines b) '#("one" "two"))
           (let ([input (vector "before")])
             (head:store-reset! b input)
             (vector-set! input 0 "caller mutation")
             (check 'baseline-owns-its-vector (head:buffer-lines b) '#("before")))
           (mode:choose! b "invalid-line-output")
           (let ([before (state b)] [history (head:buffer-history b)])
             (check 'invalid-inputs-refuse-before-changing-either-owner
               (map
                 (lambda (operation)
                   (list (raises? operation) (equal? (state b) before)
                         (equal? (head:buffer-history b) history)))
                 (list
                   (lambda () (head:store-reset! b '#("ok" 7)))
                   (lambda () (head:store-reset! b '("lost") '((trailing . 7))))
                   (lambda () (head:buffer-facts-set! b '((file . "changed") (7 . bad))))
                   (lambda () (head:buffer-facts-set! b '((file . "changed") (modified-at . 1.5))))
                   (lambda () (head:buffer-facts-set! b '((file . "changed")) '(base (base . #f))))
                   (lambda () (head:buffer-facts-set! b '((file . "changed")) #f ""))
                   (lambda () (head:store-edit! b (text:make-span 0 0 0 0) '("lost")
                                '(key "bad" ((trailing . #t) (trailing . #f)))))
                   (lambda () (head:store-edit! b (text:make-span 0 0 0 0) '("lost")
                                '(key "bad" ((base . "a")) ((base . "b")))))
                   (lambda () (head:store-edit! b (text:make-span 0 0 0 0) '("lost")
                                '(key "bad" () () ((trailing . 7)))))
                   (lambda () (head:store-reset! b '("embedded\nnewline")))
                   (lambda () (head:store-edit! b (text:make-span 0 0 0 0) '("embedded\nnewline")))
                   (lambda () (buffer-append! b "embedded\nnewline"))
                   (lambda () (store:create! bot "invalid-line-input" '("embedded\nnewline")))
                   (lambda () (format-buffer!)))) (make-list 14 '(#t #t #t))))))
       '(#t #f))

     ;; The buffer still exists when the store rejects the fact write.
     (check 'shared-fact-error-is-not-silent-success
            (raises? (lambda () (head:buffer-fact-set! b "not-a-symbol" #t))) #t)
     (check 'failed-fact-write-does-not-delete-buffer (store:exists? id) #t)
     (define store-cell (kernel:persistent-cell 'store (lambda () (error 'test "missing store"))))
     (define saved-store (unbox store-cell))
     (dynamic-wind
       (lambda () (set-box! store-cell #f))
       (lambda ()
         (check 'store-read-failure-propagates
                (raises? (lambda () (head:buffer-fact b 'file 'fallback))) #t)
         (check 'store-write-failure-propagates
                (raises? (lambda () (head:buffer-file-set! b "lost"))) #t)
         (check 'unavailable-shared-state-is-not-disposable (buffer-clean? b) #f)
         (check 'failed-deletion-does-not-retire-the-head-buffer
           (list (raises? (lambda () (kill-buffer! b))) (and (memq b (head:buffers)) #t)) '(#t #t)))
       (lambda () (set-box! store-cell saved-store)))
     (check 'failure-recovery-keeps-shared-text (store:line id 0) "agent work!")

     ;; A file is born with its complete content/facts. Create subscribers
     ;; see that baseline, then edit or retarget it before visit returns.
     (define path (format "/tmp/e-state-~a-~a.txt" (time-second (current-time)) (random 1000000)))
     (mode:register! "visit-state" '(".state") '("statesh") (lambda (line) #f))
     (check 'file-opening-publishes-once-and-keeps-callback-work
       (map
         (lambda (scenario)
           (let* ([content (car scenario)] [target (string-append path (cadr scenario))]
                  [lines (caddr scenario)] [trailing (cadddr scenario)]
                  [mode (list-ref scenario 4)] [effect (list-ref scenario 5)]
                  [id #f] [opened #f] [observed #f] [seen #f] [events '()])
             (define (current) (list (state opened) (store:history id)))
             (when content (call-with-output-file target (lambda (p) (display content p)) 'replace))
             (let* ([stamp (and content (file:stamp target))]
                    [token
                     (store:subscribe! #f
                       (lambda (event)
                         (when (and (eq? (car event) 'create) (string=? (caddr event) (file:base-name target)))
                           (set! id (cadr event)))
                         (when (eqv? (cadr event) id)
                           (set! events (cons (car event) events))
                           (when (eq? (car event) 'create)
                             (set! observed
                               (let-values ([(text revision facts) (store:snapshot-state id)])
                                 (list text revision
                                   (property:select facts '(file base stamp trailing mode mode-auto wrap modified)))))
                             (set! opened
                               (if (eq? effect 'revisit)
                                   (begin (visit-file! target) (current-buffer))
                                   (head:adopt-store-buffer! id)))
                             (case effect
                               [(edit revisit) (insert! id 0 "agent ")]
                               [(metadata) (head:buffer-facts-set! opened
                                             `((file . ,(string-append target ".other")) (base . "new baseline\n")
                                               (mode . "invalid-line-output") (mode-auto . #f)) #f "callback file")])
                             (head:before-frame!)
                             (set! seen (current))))))])
               (dynamic-wind void
                 (lambda ()
                   (visit-file! target)
                   (head:before-frame!)
                   (let ([kept? (and seen (equal? seen (current)))])
                     (list (equal? observed
                             (list lines 0
                               (list (cons 'file target) (if content (cons 'base content) 'base)
                                     (if content (cons 'stamp stamp) 'stamp) (cons 'trailing trailing)
                                     (cons 'mode mode) '(mode-auto . #t) '(wrap . default) '(modified . #f))))
                           (eq? opened (current-buffer)) kept?
                           (equal? (reverse events)
                             (case effect [(edit) '(create edit)]
                               [(revisit) (if content '(create property property edit) '(create edit))]
                               [(metadata) '(create rename property property property property)] [else '(create)]))
                           (equal? (and (file-exists? target) (file:read target)) content)
                           (if (memq effect '(edit revisit))
                               (begin (store:undo! bot id) (head:before-frame!)
                                      (and (equal? (head:buffer-lines opened) lines) (not (head:buffer-modified opened))))
                               #t))))
                 (lambda ()
                   (store:unsubscribe! token)
                   (when id (store:delete! bot id))
                   (head:before-frame!)
                   (when (file-exists? target) (delete-file target)))))))
         '(("disk\n" ".state" #("disk") #t "visit-state" edit)
           ("#!/usr/bin/env statesh\nλ text" "" #("#!/usr/bin/env statesh" "λ text") #f "visit-state" edit)
           (#f ".state" #("") #t "visit-state" edit)
           ("disk" ".state" #("disk") #f "visit-state" metadata)
           (#f ".state" #("") #t "visit-state" metadata)
           ("disk\n" ".state" #("disk") #t "visit-state" revisit)
           (#f ".state" #("") #t "visit-state" revisit)
           ("" "" #("") #f #f none)))
       (make-list 8 '(#t #t #t #t #t #t)))

     ;; Saving publishes the file, label and detected mode before callbacks.
     ;; A subscriber can then edit or choose newer metadata, and pump a frame;
     ;; neither first save nor re-save may overwrite that newer state on return.
     (define saved (fresh "save-state" #t))
     (define saved-id (head:buffer-store-id saved))
     (mode:register! "save-state" '(".txt") '() (lambda (line) #f))
     (dynamic-wind
       void
       (lambda ()
         (for-each
           (lambda (scenario)
             (let ([effect (car scenario)] [adopt? (cadr scenario)] [armed? #t] [seen #f] [observed #f])
               (define (current)
                 (list (state saved) (store:history saved-id) (head:buffer-history saved) (point) (mark)))
               (when (file-exists? path) (delete-file path))
               (unless adopt? (file:write! path '#("before") #t))
               (head:store-reset! saved '#("")
                 `((file . ,(and (not adopt?) path)) (base . ,(and (not adopt?) "before\n"))
                   (trailing . #t) (read-only . #f)))
               (insert-text! "written")
               (head:buffer-facts-set! saved '((read-only . #t) (disposable . #t) (stale . #t)))
               (head:buffer-name-set! saved "before save")
               (mode:choose! saved "invalid-line-output")
               (let ([token
                      (store:subscribe! saved-id
                        (lambda (event)
                          (when (and armed? (eq? (car event) 'property) (eq? (caddr event) 'file))
                            (set! armed? #f)
                            (set! observed
                              (cons (store:buffer-name saved-id)
                                (map (lambda (key) (store:property saved-id key))
                                  '(file base mode mode-auto read-only disposable stale modified))))
                            (case effect
                              [(text) (insert! saved-id 7 "-later")]
                              [(retarget) (head:buffer-facts-set! saved
                                            '((file . "/tmp/retargeted.ss") (base . "new baseline\n")))])
                            (when (memq effect '(name retarget)) (head:buffer-name-set! saved "callback name"))
                            (when (memq effect '(mode retarget)) (mode:choose! saved "invalid-line-output"))
                            (head:before-frame!)
                            (set! seen (current)))))])
                 (dynamic-wind void
                   (lambda ()
                     (check (list scenario 'save-adoption-and-newer-callback-state)
                       (list (save-file! path) observed (file:read path)
                             (and seen (equal? seen (current)))
                             (head:buffer-lines saved) (head:buffer-modified saved)
                             (buffer-clean? saved))
                       (let ([dirty? (and (memq effect '(text retarget)) #t)])
                         (list #t (list (file:base-name path) path "written\n"
                                        (if adopt? "save-state" "invalid-line-output") adopt?
                                        (not adopt?) (not adopt?) #f #f)
                               "written\n" #t (if (eq? effect 'text) '#("written-later") '#("written"))
                               dirty? (or (not adopt?) (not dirty?))))))
                   (lambda () (store:unsubscribe! token))))))
           '((name #f) (mode #f) (name #t) (mode #t) (retarget #t) (text #t)))
         (mode:choose! saved "invalid-line-output")
         (check 'second-save-writes-later-text-and-keeps-manual-mode
           (list (save-file! path) (file:read path) (head:buffer-modified saved)
                 (mode:name-of saved) (head:buffer-mode-auto saved))
           '(#t "written-later\n" #f "invalid-line-output" #f))
         ;; Pre-save edits need not have reached the head's cached text.
         (parameterize ([kernel:registering-module 'state-save-hook])
           (file:add-pre-save-hook!
             (lambda (target) (when (string=? target path) (insert! saved-id 0 "pre-")))))
         (check 'unadopted-pre-save-edit-is-written (save-file! path) #t)
         (check 'save-captures-current-store-text (file:read path) "pre-written-later\n")
         (head:before-frame!)
         (check 'late-head-adoption-keeps-save-clean (head:buffer-modified saved) #f))
       (lambda ()
         (kernel:retract-module! 'state-save-hook)
         (when (file-exists? path) (delete-file path))))

     ;; Interrupt a real save after its review but before fact publication.
     ;; The written bytes survive refusal; newer text alone still permits save.
     (for-each
       (lambda (shared?)
         (check (list 'in-flight-save shared?)
           (map
             (lambda (change)
               (let* ([b (fresh "save in flight" shared?)] [before #f] [post-saves 0]
                      [lines (make-vector 100000 "ordinary line")]
                      [written (file:text lines #t)] [name (head:buffer-name b)])
                 (head:store-reset! b lines (if shared? '() '((modified . #t))))
                 (dynamic-wind
                   (lambda ()
                     (parameterize ([kernel:registering-module 'in-flight-save])
                       (file:add-pre-save-hook! (lambda (target) (set-timer 10000)))
                       (file:add-post-save-hook! (lambda (target) (set! post-saves (+ post-saves 1))))))
                   (lambda ()
                     (let ([result
                            (interrupt-during!
                              (lambda ()
                                (case change
                                  [(file) (head:buffer-facts-set! b
                                            '((file . "/tmp/retargeted.txt") (base . "new baseline\n")))]
                                  [(protection) (head:buffer-facts-set! b '((read-only . #t) (disposable . #t)))]
                                  [(mode) (mode:choose! b "invalid-line-output")]
                                  [(trailing) (head:buffer-trailing-set! b #f)]
                                  [(text) (if shared? (insert! (head:buffer-store-id b) 0 "later ")
                                              (insert-text! "later "))])
                                (set! before (state b)))
                              (lambda () (save-file! path)))])
                       (list result (string=? (file:read path) written) post-saves
                             (if (memq change '(text trailing))
                                 (and (equal? (head:buffer-base b) written) (head:buffer-modified b)
                                      (if (eq? change 'text)
                                          (equal? (vector-ref (car (state b)) 0) "later ordinary line")
                                          (not (head:buffer-trailing b))))
                                 (and (equal? before (state b)) (equal? name (head:buffer-name b))
                                      (let ([message (log:datum (car (log:entries 'save-file! 1)))])
                                        (and (string:prefix? (format "Wrote ~a, but could not finish saving:" path) message)
                                             (string:suffix? "saved baseline was not updated." message))))))))
                   (lambda ()
                     (kernel:retract-module! 'in-flight-save)
                     (when (file-exists? path) (delete-file path))))))
             '(file protection mode text trailing))
           '(((#t #f) #t 0 #t) ((#t #f) #t 0 #t) ((#t #f) #t 0 #t)
             ((#t #t) #t 1 #t) ((#t #t) #t 1 #t))))
       '(#t #f))

     ;; Old disk readers cannot attach their stamp/status to newer file facts
     ;; or replace a newer reader's observation of the same baseline.
     (let ([disk (make-vector 100000 "ordinary line")])
       (dynamic-wind
         (lambda () (file:write! path disk #t))
         (lambda ()
           (check 'disk-observations-publish-only-against-their-captured-facts
             (map
               (lambda (operation)
                 (let* ([b (fresh "disk observation" #t)]
                        [updates (append '((stamp . 456) (stale . #t))
                                   (if (eq? operation 'edit) '((file . "/tmp/retargeted.txt") (base . "new baseline\n")) '()))])
                   (head:store-reset! b '("mine")
                     (list (cons 'file path) (cons 'base (file:text disk #t)) '(stamp . #f) '(stale . #f)))
                   (let ([result (interrupt-during!
                                   (lambda () (head:buffer-facts-set! b updates))
                                   (lambda ()
                                     (set-timer 10000)
                                     (if (eq? operation 'edit) (insert-text! "edited ") (visit-file! path))))])
                     (list (car result) (property:matches? updates (caddr (state b)))))))
               '(edit visit))
             '((#t #t) (#t #t))))
         (lambda () (when (file-exists? path) (delete-file path)))))

     (display checks) (display " edit state checks passed\n")))
