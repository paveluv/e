#!/usr/bin/env scheme-script

;; Base-owned dirty state, atomic file facts and guarded save/read callbacks.
;; Explicit editor placement preserves newer work without an ambient host.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (test) test:) (prefix (head head) head:) (prefix (head widget) widget:) (prefix (state store) store:)
       (prefix (core property) property:) (prefix (foundation text) text:) (prefix (service document) document:)
       (prefix (service file) file:) (prefix (service log) log:) (prefix (foundation string) string:)
       (prefix (head mode) mode:) (prefix (core kernel) kernel:))
     (define bot '(agent state-test))
     (define check test:check)
     (define raises? test:raises?)
     (include "tests/editor-fixture.sps")
     (define (reset! id lines . options)
       (let ([result (apply store:reset! head:ui-actor id lines options)])
         (when result (text-source:open! head:ui-actor id))
         result))
     (define (state b)
       (let-values ([(text revision facts) (store:snapshot-state b)])
         (list
           text
           revision
           (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b)))) facts)
           (store:buffer-name b))))
     (define (insert! id at replacement)
       (store:edit! bot id (store:revision id) (text:make-span 0 at 0 at) (list replacement)))
     (define (interrupt-during! change! thunk)
       (let ([handler (timer-interrupt-handler)] [interrupted? #f])
         (dynamic-wind
           (lambda () (timer-interrupt-handler (lambda () (set! interrupted? #t) (change!))))
           (lambda () (let ([result (thunk)]) (set-timer 0) (list interrupted? result)))
           (lambda () (set-timer 0) (timer-interrupt-handler handler)))))
     (define b (fresh "shared-state" '("")))
     (define id b)
     (check 'empty-scratch-is-clean (store:property id 'modified) #f)
     (insert! id 0 "agent work")
     (check 'foreign-edit-is-dirty-before-adoption (store:property id 'modified) #t)
     (check 'discard-reads-current-text-not-empty-cache (edit:buffer-clean? b) #f)
     (refresh!)
     (check 'adoption-retains-dirty-state (store:property b 'modified #f) #t)
     (store:undo! bot id)
     (check 'foreign-undo-to-empty-is-clean (store:property id 'modified) #f)
     (store:redo! bot id)
     (check 'foreign-redo-is-dirty (store:property id 'modified) #t)
     (store:set-properties! bot id '((base . "agent work\n") (trailing . #t)))
     (check 'matching-disk-baseline-is-clean (store:property id 'modified) #f)
     (refresh!)
     (check 'late-adoption-cannot-redirty-a-save (store:property b 'modified #f) #f)
     (store:set-property! bot id 'trailing #f)
     (check 'final-newline-only-change-is-dirty (store:property id 'modified) #t)
     (store:set-property! bot id 'trailing #t)
     (check 'restoring-final-newline-is-clean (store:property id 'modified) #f)
     (insert! id 10 "!")
     (store:undo! bot id)
     (check 'store-undo-to-saved-text-is-clean (store:property id 'modified) #f)
     (check
       'modification-facts-belong-to-the-store
       (map (lambda (key)
              (list
                (raises? (lambda () (store:set-property! bot id key #f)))
                (raises? (lambda () (store:drop-property! bot id key)))))
            property:edit-keys)
       '((#t #t) (#t #t) (#t #t)))
     (define equivalent (store:create! bot "equivalent-lines" '("line" "")))
     (store:set-properties! bot equivalent '((trailing . #f) (base . "line\n")))
     (check 'equivalent-line-representation-is-clean (store:property equivalent 'modified) #f)
     (store:set-property! bot equivalent 'trailing #t)
     (check 'extra-blank-line-is-dirty (store:property equivalent 'modified) #t)
     (define born (store:create! bot "born-with-work" '("new work")))
     (check 'creation-with-content-is-dirty (store:property born 'modified) #t)
     (refresh!)
     (define adopted born)
     (check 'newly-adopted-work-needs-protection (edit:buffer-clean? adopted) #f)
     (store:set-property! head:ui-actor adopted 'read-only #t)
     (check 'read-only-does-not-authorize-disposal (edit:buffer-clean? adopted) #f)
     (store:set-property! head:ui-actor adopted 'disposable #t)
     (check 'generated-output-explicitly-allows-disposal (edit:buffer-clean? adopted) #t)
     (define (utc-nanos) (let ([now (current-time 'time-utc)]) (+ (* (time-second now) 1000000000) (time-nanosecond now))))
     (define time-steps
       '((insert . #t) (same . #f) (metadata . #f) (save . #f) (newline . #t) (edit . #t) (undo . #t) (redo . #t) (reset . #t)
         (reset . #f) (representation . #t) (representation . #f)))
     (check
       'modification-times-follow-content
       (list
         (let ()
           (let ([b (fresh "timed-shared" '(""))])
             (reverse
               (fold-left
                 (lambda (results step)
                   (let ([before (store:property b 'modified-at #f)] [started (utc-nanos)])
                     (case (car step)
                       [(insert) (edit:insert! editing "a")]
                       [(same) (submit! b (text:make-span 0 0 0 1) '("a"))]
                       [(metadata) (store:set-property! head:ui-actor b 'status "metadata")]
                       [(save) (store:set-properties! head:ui-actor b (append '((base . "a\n")) '()))]
                       [(newline) (store:set-property! head:ui-actor b 'trailing #f)]
                       [(edit) (edit:insert! editing "b")]
                       [(undo) (edit:undo! editing)]
                       [(redo) (edit:redo! editing)]
                       [(reset) (reset! b '("reset") (append '((base . "reset\n") (trailing . #t)) '()))]
                       [(representation) (reset! b '("reset" "") '((trailing . #f)))])
                     (let ([after (store:property b 'modified-at #f)])
                       (cons
                         (cons
                           (car step)
                           (if (cdr step)
                               (and (integer? after) (exact? after) (<= started after (utc-nanos)) (not (equal? before after)))
                               (equal? before after)))
                         results))))
                 '()
                 time-steps)))))
       (make-list 1 (map (lambda (step) (cons (car step) #t)) time-steps)))
     (define observations '())
     (define token
       (store:subscribe!
         id
         (lambda (event)
           (when (eq? (car event) 'property)
             (set! observations
               (cons
                 (list (store:property id 'base) (store:property id 'stamp) (store:property id 'modified))
                 observations))))))
     (store:set-properties! bot id '((base . "agent work\n") (stamp . 123)))
     (store:unsubscribe! token)
     (check 'property-batch-observers-see-complete-state observations '(("agent work\n" 123 #f) ("agent work\n" 123 #f)))
     (define captured (state b))
     (insert! id 10 "!")
     (check 'captured-state-keeps-its-text (car captured) '#("agent work"))
     (check 'captured-state-keeps-its-dirty-fact (cdr (assq 'modified (caddr captured))) #f)
     (check 'current-state-moved-on (store:property id 'modified) #t)
     (check 'absent-fact-uses-default (store:property b 'missing 'fallback) 'fallback)
     (store:set-property! head:ui-actor b 'explicit-false #f)
     (check 'explicit-false-does-not-use-default (store:property b 'explicit-false 'fallback) #f)
     (define merged (fresh "merged-state" '("")))
     (define merged-id merged)
     (reset! merged '("mine") '((base . "old\n") (trailing . #t)))
     (store:edit! bot merged-id (store:revision merged-id) (text:make-span 0 0 0 4) '("disk")
       '(merge
          "merge"
          (undo (trailing . #f))
          (commit (base . "disk") (stamp . 456) (stale . #f))
          (expected (base . "old\n") (trailing . #t))))
     (check 'merge-text-and-new-baseline-are-clean (store:property merged-id 'modified) #f)
     (store:undo! bot merged-id)
     (check
       'undo-keeps-the-incorporated-disk-baseline
       (list (store:property merged-id 'base) (store:property merged-id 'stamp))
       '("disk" 456))
     (check
       'undo-restores-text-and-trailing
       (list (store:line merged-id 0) (store:property merged-id 'trailing))
       '("mine" #t))
     (check 'undo-relative-to-new-disk-is-dirty (store:property merged-id 'modified) #t)
     (store:redo! bot merged-id)
     (check 'redo-relative-to-new-disk-is-clean (store:property merged-id 'modified) #f)
     (let ()
       (let* ([b (fresh "reset-shared" '(""))] [id b])
         (define (head-state)
           (list (edit:basis (document-editor b)) (point) (cadddr (state-of b)) (car (cadr (state-of b)))
             (cdr (cadr (state-of b)))))
         (edit:insert! editing "keep")
         (edit:set-mark! (document-editor b) #t)
         (check
           'reviewed-reset-refusal-keeps-document-and-view-state
           (map (lambda (change)
                  (let-values ([(text revision facts) (store:snapshot-state b)])
                    (case change [(text) (insert! id 0 "new ")] [(facts) (store:set-property! head:ui-actor b 'trailing #f)])
                    (let* ([before (state b)]
                           [view (head-state)]
                           [accepted (reset! b '("lost") '((base . "lost")) (cons revision facts))])
                      (list accepted (equal? before (state b)) (equal? view (head-state))))))
                '(text facts))
           '((#f #t #t) (#f #t #t)))
         (let-values ([(text revision facts) (store:snapshot-state b)])
           (let ([accepted (reset! b '("disk") '((base . "disk\n") (trailing . #t)) (cons revision facts))])
             (check
               'fresh-review-adopts-a-new-baseline-atomically
               (list accepted (lines-of b) (point))
               (list (+ revision 1) '#("disk") '(0 . 4)))))
         (let ([expected (property:select (caddr (state b)) '(base missing))])
           (store:set-property! head:ui-actor b 'missing #f)
           (let ([before (state b)] [view (head-state)])
             (check
               'conditional-facts-and-edits-refuse-atomically
               (list
                 (store:set-properties! head:ui-actor b '((base . "lost")) expected "lost name")
                 (raises?
                   (lambda ()
                     (submit!
                       b
                       (text:make-span 0 0 0 0)
                       '("lost")
                       (list 'merge "merge" (cons 'commit '((base . "lost"))) (cons 'expected expected)))))
                 (equal? before (state b))
                 (equal? view (head-state)))
               '(#f #t #t #t)))
           (store:set-property! head:ui-actor b 'missing '())
           (check
             'fresh-fact-review-publishes-atomically
             (list
               (store:set-properties! head:ui-actor b '((stamp . 1)) (property:select (caddr (state b)) '(base missing absent))
                 "accepted facts")
               (store:buffer-name b))
             (list #t "accepted facts")))))
     (mode:register! "invalid-line-output" '() '() (lambda (line) #f))
     (mode:register-formatter! "invalid-line-output" (lambda args '("embedded\nnewline")))
     (let ()
       (let ([b (fresh "validate-shared" '(""))])
         (reset! b '())
         (check 'empty-list-normalizes (lines-of b) '#(""))
         (reset! b '#())
         (check 'empty-vector-normalizes (lines-of b) '#(""))
         (reset! b '("one" "two"))
         (check 'line-list-normalizes (lines-of b) '#("one" "two"))
         (let ([input (vector "before")])
           (reset! b input)
           (vector-set! input 0 "caller mutation")
           (check 'baseline-owns-its-vector (lines-of b) '#("before")))
         (store:set-properties! head:ui-actor b '((mode . "invalid-line-output") (mode-auto . #f)))
         (let ([before (state b)])
           (check
             'invalid-inputs-refuse-before-changing-document
             (map (lambda (operation) (list (raises? operation) (equal? (state b) before)))
                  (list (lambda () (reset! b '#("ok" 7))) (lambda () (reset! b '("lost") '((trailing . 7))))
                    (lambda () (store:set-properties! head:ui-actor b '((file . "changed") (7 . bad))))
                    (lambda () (store:set-properties! head:ui-actor b '((file . "changed") (modified-at . 1.5))))
                    (lambda () (store:set-properties! head:ui-actor b '((file . "changed")) '(base (base . #f))))
                    (lambda () (store:set-properties! head:ui-actor b '((file . "changed")) #f ""))
                    (lambda ()
                      (submit! b (text:make-span 0 0 0 0) '("lost") '(key "bad" (undo (trailing . #t) (trailing . #f)))))
                    (lambda ()
                      (submit! b (text:make-span 0 0 0 0) '("lost") '(key "bad" (undo (base . "a")) (commit (base . "b")))))
                    (lambda () (submit! b (text:make-span 0 0 0 0) '("lost") '(key "bad" (expected (trailing . 7)))))
                    (lambda () (reset! b '("embedded\nnewline")))
                    (lambda () (submit! b (text:make-span 0 0 0 0) '("embedded\nnewline")))
                    (lambda () (store:create! bot "invalid-line-input" '("embedded\nnewline")))
                    (lambda () (edit:format-buffer! editing))))
             (make-list 13 '(#t #t))))))
     (check
       'shared-fact-error-is-not-silent-success
       (raises? (lambda () (store:set-property! head:ui-actor b "not-a-symbol" #t)))
       #t)
     (check 'failed-fact-write-does-not-delete-buffer (store:exists? id) #t)
     (define store-cell (kernel:persistent-cell 'store (lambda () (error 'test "missing store"))))
     (define saved-store (unbox store-cell))
     (dynamic-wind
       (lambda () (set-box! store-cell #f))
       (lambda ()
         (check 'store-read-failure-propagates (raises? (lambda () (store:property b 'file 'fallback))) #t)
         (check 'store-write-failure-propagates (raises? (lambda () (store:set-property! head:ui-actor b 'file "lost"))) #t)
         (check 'unavailable-shared-state-is-not-disposable (edit:buffer-clean? b) #f)
         (check
           'failed-deletion-does-not-retire-the-editor
           (list (raises? (lambda () (edit:kill-buffer! b))) (and (interaction:snapshot editing) #t))
           '(#t #t)))
       (lambda () (set-box! store-cell saved-store)))
     (check 'failure-recovery-keeps-shared-text (store:line id 0) "agent work!")
     (define path (format "/tmp/e-state-~a-~a.txt" (time-second (current-time)) (random 1000000)))
     (mode:register! "visit-state" '(".state") '("statesh") (lambda (line) #f))
     (check
       'file-opening-publishes-once-and-keeps-callback-work
       (map (lambda (scenario)
              (let* ([content (car scenario)]
                     [target (string-append path (cadr scenario))]
                     [lines (caddr scenario)]
                     [trailing (cadddr scenario)]
                     [mode (list-ref scenario 4)]
                     [effect (list-ref scenario 5)]
                     [id #f]
                     [opened #f]
                     [observed #f]
                     [seen #f]
                     [events '()])
                (define (current)
                  (let ([s (state opened)])
                    (list (car s) (cadr s) (filter (lambda (p) (not (memq (car p) '(mode mode-auto)))) (caddr s)) (cadddr s)
                      (store:history id))))
                (when content (call-with-output-file target (lambda (p) (display content p)) 'replace))
                (let* ([stamp (and content (file:stamp target))]
                       [token (store:subscribe!
                                #f
                                (lambda (event)
                                  (when (and (eq? (car event) 'create) (string=? (caddr event) (file:base-name target)))
                                    (set! id (cadr event)))
                                  (when (equal? (cadr event) id)
                                    (set! events (cons (car event) events))
                                    (when (eq? (car event) 'create)
                                      (set! observed
                                        (let-values ([(text revision facts) (store:snapshot-state id)])
                                          (list text revision (property:select facts '(file base stamp trailing modified)))))
                                      (set! opened
                                        (if (eq? effect 'revisit)
                                            (begin (visit! target) (view:source (interaction:snapshot editing)))
                                            id))
                                      (case effect
                                        [(edit revisit) (insert! id 0 "agent ")]
                                        [(metadata)
                                         (store:set-properties! head:ui-actor opened
                                           `((file . ,(string-append target ".other"))
                                             (base . "new baseline\n")
                                             (mode . "invalid-line-output")
                                             (mode-auto . #f))
                                           #f "callback file")])
                                      (refresh!)
                                      (set! seen (current))))))])
                  (dynamic-wind
                    void
                    (lambda ()
                      (visit! target)
                      (refresh!)
                      (let ([kept? (and seen (equal? seen (current)))])
                        (list
                          (equal?
                            observed
                            (list
                              lines
                              0
                              (list (cons 'file target) (cons 'base (or content ""))
                                (cons 'stamp (or stamp (file:stamp target))) (cons 'trailing trailing) '(modified . #f))))
                          (equal? opened (view:source (interaction:snapshot editing)))
                          (and kept? (or (eq? effect 'metadata) (equal? (mode:name-of opened) mode)))
                          (= 1 (length (filter (lambda (event) (eq? event 'create)) events)))
                          (equal? (and (file-exists? target) (file:read target)) (or content ""))
                          (if (memq effect '(edit revisit))
                              (begin
                                (store:undo! bot id)
                                (refresh!)
                                (and (equal? (lines-of opened) lines) (not (store:property opened 'modified #f))))
                              #t))))
                    (lambda ()
                      (store:unsubscribe! token)
                      (when id (store:delete! bot id))
                      (refresh!)
                      (when (file-exists? target) (delete-file target)))))))
            '(("disk\n" ".state" #("disk") #t "visit-state" edit) ("#!/usr/bin/env statesh\nλ text" "" #("#!/usr/bin/env statesh" "λ text") #f #f edit)
              (#f ".state" #("") #f "visit-state" edit) ("disk" ".state" #("disk") #f "visit-state" metadata)
              (#f ".state" #("") #f "visit-state" metadata) ("disk\n" ".state" #("disk") #t "visit-state" revisit)
              (#f ".state" #("") #f "visit-state" revisit) ("" "" #("") #f #f none)))
       (make-list 8 '(#t #t #t #t #t #t)))
     (check
       'save-refuses-presentations-and-allows-stopped-shared-output
       (map (lambda (kind)
              (let ([b (fresh "app-output" '(""))] [before #f])
                (define (own!)
                  (store:set-properties! head:ui-actor b '((app app save-test) (alive . #t) (read-only . #t)))
                  (set! before (state b)))
                (submit! b (text:make-span 0 0 0 0) '("output"))
                (dynamic-wind
                  (lambda ()
                    (if (eq? kind 'hook)
                        (parameterize ([kernel:registering-module 'state-app-save-hook])
                          (file:add-pre-save-hook! (lambda (target editor) (own!))))
                        (own!)))
                  (lambda ()
                    (let* ([result (guard (ex [(kernel:refusal? ex) 'refused]) (edit:save-file! editing path))]
                           [unchanged? (and (equal? before (state b)) (not (file-exists? path)))])
                      (kernel:retract-module! 'state-app-save-hook)
                      (store:set-property! head:ui-actor b 'alive #f)
                      (let ([saved? (guard (ex [(kernel:refusal? ex) 'refused]) (edit:save-file! editing path))])
                        (list result unchanged? saved? (and (file-exists? path) (file:read path))))))
                  (lambda ()
                    (kernel:retract-module! 'state-app-save-hook)
                    (when b (store:delete! head:ui-actor b))
                    (widget:unmount! editing)
                    (when (file-exists? path) (delete-file path))))))
            '(shared hook))
       '((refused #t #t "output\n") (refused #t #t "output\n")))
     (define saved (fresh "save-state" '("")))
     (define saved-id saved)
     (mode:register! "save-state" '(".txt") '() (lambda (line) #f))
     (dynamic-wind
       void
       (lambda ()
         (for-each
           (lambda (scenario)
             (let ([effect (car scenario)] [adopt? (cadr scenario)] [armed? #t] [seen #f] [observed #f])
               (define (current) (list (state saved) (store:history saved-id)))
               (when (file-exists? path) (delete-file path))
               (unless adopt? (file:write! path '#("before") #t))
               (reset!
                 saved
                 '#("")
                 `((file . ,(and (not adopt?) path)) (base . ,(and (not adopt?) "before\n")) (trailing . #t) (read-only . #f)))
               (edit:insert! editing "written")
               (store:set-properties! head:ui-actor saved '((read-only . #t) (disposable . #t)))
               (store:rename! head:ui-actor saved "before save")
               (mode:choose! "invalid-line-output" saved)
               (let ([token (store:subscribe!
                              saved-id
                              (lambda (event)
                                (when (and armed? (eq? (car event) 'property) (eq? (caddr event) 'file))
                                  (set! armed? #f)
                                  (set! observed
                                    (cons
                                      (store:buffer-name saved-id)
                                      (map (lambda (key) (store:property saved-id key))
                                           '(file base mode mode-auto read-only disposable modified))))
                                  (case effect
                                    [(text) (insert! saved-id 7 "-later")]
                                    [(retarget)
                                     (store:set-properties!
                                       head:ui-actor
                                       saved
                                       '((file . "/tmp/retargeted.ss") (base . "new baseline\n")))])
                                  (when (memq effect '(name retarget)) (store:rename! head:ui-actor saved "callback name"))
                                  (when (memq effect '(mode retarget)) (mode:choose! "invalid-line-output" saved))
                                  (refresh!)
                                  (set! seen (current)))))])
                 (dynamic-wind
                   void
                   (lambda ()
                     (check
                       (list scenario 'save-adoption-and-newer-callback-state)
                       (let* ([saved? (edit:save-file! editing path)] [written (file:read path)])
                         (list saved? observed written (and seen (equal? seen (current))) (lines-of saved)
                           (store:property saved 'modified #f) (edit:buffer-clean? saved)))
                       (let ([dirty? (and (memq effect '(text retarget)) #t)])
                         (list #t
                           (list (file:base-name path) path "written\n" (if adopt? "save-state" "invalid-line-output") adopt?
                             (not adopt?) (not adopt?) #f)
                           "written\n" #t (if (eq? effect 'text) '#("written-later") '#("written")) dirty?
                           (or (not adopt?) (not dirty?))))))
                   (lambda () (store:unsubscribe! token))))))
           '((name #f) (mode #f) (name #t) (mode #t) (retarget #t) (text #t)))
         (mode:choose! "invalid-line-output" saved)
         (check
           'second-save-writes-later-text-and-keeps-manual-mode
           (let* ([saved? (edit:save-file! editing path)] [written (file:read path)])
             (list saved? written (store:property saved 'modified #f) (mode:name-of saved)
               (store:property saved 'mode-auto #f)))
           '(#t "written-later\n" #f "invalid-line-output" #f))
         (parameterize ([kernel:registering-module 'state-save-hook])
           (file:add-pre-save-hook! (lambda (target editor) (when (string=? target path) (insert! saved-id 0 "pre-")))))
         (check 'unadopted-pre-save-edit-is-written (edit:save-file! editing path) #t)
         (check 'save-captures-current-store-text (file:read path) "pre-written-later\n")
         (refresh!)
         (check 'late-head-adoption-keeps-save-clean (store:property saved 'modified #f) #f))
       (lambda () (kernel:retract-module! 'state-save-hook) (when (file-exists? path) (delete-file path))))
     (check
       'in-flight-save
       (map (lambda (change)
              (let* ([b (fresh "save in flight" '(""))]
                     [before #f]
                     [post-saves 0]
                     [lines (make-vector 100000 "ordinary line")]
                     [written (file:text lines #t)]
                     [name (store:buffer-name b)])
                (reset! b lines '())
                (dynamic-wind
                  (lambda ()
                    (parameterize ([kernel:registering-module 'in-flight-save])
                      (file:add-pre-save-hook! (lambda (target editor) (set-timer 10000)))
                      (file:add-post-save-hook! (lambda (target editor) (set! post-saves (+ post-saves 1))))))
                  (lambda ()
                    (let ([result (interrupt-during!
                                    (lambda ()
                                      (case change
                                        [(file)
                                         (store:set-properties!
                                           head:ui-actor
                                           b
                                           '((file . "/tmp/retargeted.txt") (base . "new baseline\n")))]
                                        [(protection)
                                         (store:set-properties! head:ui-actor b '((read-only . #t) (disposable . #t)))]
                                        [(mode) (mode:choose! "invalid-line-output" b)]
                                        [(trailing) (store:set-property! head:ui-actor b 'trailing #f)]
                                        [(text) (insert! b 0 "later ")])
                                      (set! before (state b)))
                                    (lambda () (edit:save-file! editing path)))])
                      (list
                        result
                        (string=? (file:read path) written)
                        post-saves
                        (if (memq change '(text trailing))
                            (and (equal? (store:property b 'base #f) written)
                                 (store:property b 'modified #f)
                                 (if (eq? change 'text)
                                     (equal? (vector-ref (car (state b)) 0) "later ordinary line")
                                     (not (store:property b 'trailing #f))))
                            (and (equal? before (state b))
                                 (equal? name (store:buffer-name b))
                                 (let ([message (log:datum (car (log:entries 'document:save-document! 1)))])
                                   (and (string:prefix? (format "Wrote ~a, but could not finish saving:" path) message)
                                        (string:suffix? "saved baseline was not updated." message))))))))
                  (lambda () (kernel:retract-module! 'in-flight-save) (when (file-exists? path) (delete-file path))))))
            '(file protection mode text trailing))
       '(((#t #f) #t 0 #t) ((#t #f) #t 0 #t) ((#t #f) #t 0 #t) ((#t #t) #t 1 #t) ((#t #t) #t 1 #t)))
     (let ([disk (make-vector 100000 "ordinary line")])
       (dynamic-wind
         (lambda () (file:write! path disk #t))
         (lambda ()
           (check
             'disk-observations-publish-only-against-their-captured-facts
             (map (lambda (operation)
                    (let* ([b (fresh "disk observation" '(""))]
                           [updates (append
                                      '((stamp . 456) (stale . #t))
                                      (if (eq? operation 'edit)
                                          '((file . "/tmp/retargeted.txt") (base . "new baseline\n"))
                                          '()))])
                      (reset! b '("mine") (list (cons 'file path) (cons 'base (file:text disk #t)) '(stamp . #f) '(stale . #f)))
                      (let ([result (interrupt-during!
                                      (lambda () (store:set-properties! head:ui-actor b updates))
                                      (lambda ()
                                        (set-timer 10000)
                                        (if (eq? operation 'edit) (edit:insert! editing "edited ") (visit! path))))])
                        (list (car result) (property:matches? updates (caddr (state b)))))))
                  '(edit visit))
             '((#t #t) (#t #t)))
           (check
             'base-document-reads-refuse-concurrent-text-and-file-changes
             (map (lambda (operation change)
                    (let* ([b (fresh "reviewed reload" '(""))] [id b] [newer #f])
                      (reset!
                        b
                        '("mine")
                        (list (cons 'file path) (cons 'base (if (eq? operation 'save) "old\n" (file:text disk #t)))))
                      (let ([result (interrupt-during!
                                      (lambda ()
                                        (case change
                                          [(text) (insert! id 0 "concurrent ")]
                                          [(file)
                                           (store:set-properties!
                                             bot
                                             id
                                             '((file . "/tmp/retargeted.txt") (base . "new baseline\n")))])
                                        (set! newer (call-with-values (lambda () (store:snapshot-state id)) list)))
                                      (lambda ()
                                        (set-timer 10000)
                                        (case operation
                                          [(save)
                                           (let ([reply (document:save! head:ui-actor id path '("mine" #f))])
                                             (list
                                               (car reply)
                                               (and (string:search (cadr reply) "stale-review" 0 (string-length (cadr reply)))
                                                    #t)))]
                                          [else
                                           (call-with-values
                                             (lambda ()
                                               ((if (eq? operation 'reload) document:reload! document:reread!)
                                                head:ui-actor
                                                id))
                                             list)])))])
                        (list result (equal? newer (call-with-values (lambda () (store:snapshot-state id)) list))))))
                  '(reload reread save save)
                  '(text file text file))
             '(((#t (refused stale-review)) #t)
               ((#t (refused stale-review)) #t)
               ((#t (refused #t)) #t)
               ((#t (refused #t)) #t))))
         (lambda () (when (file-exists? path) (delete-file path)))))
     (check 'input-is-not-live-before-the-reader-runs (head:input-live?) #f)
     (define batched (fresh "batched" '("")))
     (define batched-id batched)
     (edit:paste! editing "a")
     (edit:paste! editing "b")
     (edit:call-as-one-edit! "two at once" (lambda () (edit:insert! editing "c") (edit:insert! editing "d")))
     (define batches (map (lambda (row) (cdr (assq 'batch (caddr row)))) (store:log batched-id)))
     (check
       'head-edits-carry-a-batch-per-action-and-per-group
       (list
         (length batches)
         (equal? (car batches) (cadr batches))
         (equal? (caddr batches) (cadddr batches))
         (equal? (car (car batches)) head:ui-actor))
       '(4 #t #f #t))
     (test:finish! 'edit-state)))
