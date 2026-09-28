#!/usr/bin/env scheme-script

;; Local tools and apps: identity survives label changes and reload;
;; a colliding ordinary buffer is never reused or overwritten.  Run
;; from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (state store) store:)
             (prefix (state model) model:)
             (prefix (state connection) connection:) (prefix (core port) port:)
             (prefix (state collection) collection:) (prefix (head range) range:)
             (prefix (state view) view:)
             (prefix (head interaction) interaction:)
             (prefix (head widget) widget:) (prefix (head window) window:)
             (prefix (head entry) entry:) (prefix (foundation text) text:)
             (prefix (sys glyph) glyph:)
             (prefix (core kernel) kernel:)
             (prefix (service log) log:)
             (prefix (test) test:)
             (prefix (state actor) actor:) (prefix (state surface) surface:) (prefix (head render) render:)
             (prefix (head paint) paint:) (prefix (head mode) mode:) (prefix (head keymap) keymap:)
             (prefix (head dispatch) dispatch:)
             (prefix (apps git-view) git-view:)
             (prefix (apps log-view) log-view:))

     (define check test:check)
     (define refused? test:raises?)
     (define (store-ids) (list-sort < (store:buffer-list)))

     ;; Invalid registrations do not allocate or mutate anything.
     (define initial-head (head:buffers))
     (define initial-store (store-ids))
     (check 'invalid-refresh-refused
            (refused? (lambda () (head:register-view! "*bad-refresh*" #f))) #t)
     (check 'invalid-handler-refused
            (refused? (lambda () (head:register-app! "*bad-handler*" void #f))) #t)
     (check 'invalid-name-refused
            (refused? (lambda () (head:register-view! "" void))) #t)
     (check 'invalid-registration-keeps-head (head:buffers) initial-head)
     (check 'invalid-registration-keeps-store (store-ids) initial-store)
     (check 'false-key-is-not-a-tool (head:find-tool-buffer #f) #f)

     ;; Callers with an existing local identity can register it directly.
     (define explicit (head:new-local-buffer! "explicit app"))
     (check 'register-existing-local
            (eq? explicit (head:register-view! explicit void)) #t)
     (check 'registered-local-is-listed (and (memq explicit (head:buffers)) #t) #t)
     (check 'register-existing-shared-refused
            (refused? (lambda ()
                        (head:register-view! (head:window-buffer (head:current-window)) void)))
            #t)
     (check 'rejected-shared-registration-keeps-flags
            (head:buffer-read-only (head:window-buffer (head:current-window))) #f)

     ;; A view never captures an ordinary buffer's label as identity.
     (define ordinary (head:new-buffer! "<app-collision>"))
     (head:buffer-lines-set! ordinary (vector "keep my work"))
     (head:add-buffer! ordinary)
     (define app (head:register-view! "*app-collision*" void))
     (check 'app-gets-distinct-buffer (eq? app ordinary) #f)
     (check 'app-local (head:buffer-store-id app) #f)
     (check 'app-label-suffixed (head:buffer-name app) "<app-collision 2>")
     (head:view-replace! app '("generated"))
     (check 'ordinary-text-kept (buffer-text ordinary) "keep my work\n")
     (check 'ordinary-flags-kept (head:buffer-read-only ordinary) #f)
     (check 'app-is-read-only (head:buffer-read-only app) #t)
     (check 'view-replacement-cannot-reset-shared-source
            (refused? (lambda () (head:view-replace! ordinary '("bad")))) #t)
     (check 'rejected-view-replacement-keeps-shared-source
            (buffer-text ordinary) "keep my work\n")
     (define app-basis (head:edit-basis app))
     (check 'view-replacement-validates-all-facts
            (refused? (lambda () (head:view-replace! app '("bad") '((custom . wrong) (trailing . invalid))))) #t)
     (check 'view-replacement-validates-placement-owner
            (refused? (lambda ()
                        (head:view-replace! app '("bad") '((custom . wrong))
                          (list (cons (head:current-window) '(0 . 0)))))) #t)
     (check 'view-replacement-requires-numeric-placements
            (refused? (lambda () (head:view-replace! app '("bad") '((custom . wrong)) '((mark . end))))) #t)
     (check 'view-replacement-rejects-negative-positions
            (refused? (lambda () (head:view-replace! app '("bad") '((custom . wrong)) '((spot -1 . 0))))) #t)
     (check 'invalid-view-state-keeps-text-and-revision (head:edit-basis app) app-basis)
     (check 'invalid-view-state-keeps-facts (head:buffer-fact app 'custom 'absent) 'absent)
     (head:set-app-presentation! app 1 #t #f 'bar)
     (head:buffer-name-set! app "renamed app")
     (define calls 0)
     (define again
       (head:register-view! "*app-collision*"
         (lambda () (set! calls (+ calls 1)))))
     (check 'registration-after-rename-reuses-buffer (eq? again app) #t)
     (check 'registration-keeps-renamed-label (head:buffer-name app) "<renamed app>")
     (check 'registration-keeps-presentation (head:buffer-fact app 'sticky-lines #f) 1)
     (check 'registration-replaces-handler
            (length (filter (lambda (a) (eq? (head:app-buffer a) app))
                            (head:registered-apps)))
            1)
     (head:show-buffer! app)
     (head:refresh-visible-views!)
     (check 'new-refresh-runs-once calls 1)
     (head:detach-app! app)
     (check 'detached-keeps-text (head:buffer-lines app) '#("generated"))
     (check 'detached-is-ordinary (head:app-buffer? app) #f)
     (check 'reattach-reuses-tool
            (eq? (head:register-view! "*app-collision*" void) app) #t)

     ;; An offscreen refresh must leave a valid selection and viewport
     ;; when the user reopens the app, even if its text became shorter.
     (head:view-replace! explicit '("first" "second" "third"))
     (head:show-buffer! explicit)
     (head:goto! '(2 . 5))
     (set-mark-command!)
     (head:window-top-set! (head:current-window) 2)
     (head:show-buffer! app)
     (head:view-replace! explicit '("x"))
     (head:show-buffer! explicit)
     (check 'shorter-view-clamps-saved-point (head:point) '(0 . 1))
     (check 'shorter-view-clamps-selection (head:mark) '(0 . 1))
     (check 'shorter-view-clamps-saved-viewport (head:window-top (head:current-window)) 0)
     (head:set-app-selectable! explicit #f)
     (define disabled-mark (head:mark))
     (set-mark-command!)
     (head:buffer-marked-set! explicit #t)
     (check 'app-selection-opt-out-clears-and-refuses-marks
       (list disabled-mark (head:mark) (head:buffer-selectable? explicit)) '(#f #f #f))
     (head:set-app-selectable! explicit #t)
     (set-mark-command!)
     (head:goto! '(0 . 0))
     (copy-region!)
     (check 'shorter-view-selection-can-be-copied (head:mark) #f)
     (head:show-buffer! app)

     ;; One model can publish different row layouts to its windows. Geometry
     ;; reads the same presentation, while resizing changes no source text.
     (let* ([root (head:root)] [w (head:current-window)] [was (head:current-buffer)]
            [b (head:register-view! "window presentation" void)]
            [other (head:make-window b 0 0 0 0 0 4 41 20 'default)]
            [source '("heading" "complete source row")]
            [wide '#("wide heading" "界e\x301;Z")]
            [narrow '#("heading" "e\x301;Z")]
            [observed #f])
       (head:show-buffer! b)
       (head:set-layout-root! (head:make-layout-split 'right w other 2 1))
       (head:set-app-selectable! b #f)
       (head:set-repaint-hook!
         (lambda () (set! observed (map head:window-lines (list w other)))))
       (head:view-replace! b source '() '() (list (cons w wide) (cons other narrow)))
       (check 'window-presentations-land-together-with-their-own-glyph-geometry
         (list observed (buffer-text b)
               (render:column (head:window-rendition w) 1 3)
               (paint:column-at-cell other 1 #f 0 1)
               (begin (head:goto! '(1 . 99)) (head:point))
               (begin (beginning-of-line!) (end-of-line!) (head:point)))
         (list (list wide narrow) "heading\ncomplete source row\n" 3 2 '(1 . 4) '(1 . 4)))
       (let ([basis (head:edit-basis b)] [retained (head:window-lines other)]
             [changed '#("wider heading" "界界e\x301;Z")])
         (head:view-replace! b source '() '() (list (cons w changed) (cons other narrow)))
         (check 'refitting-one-window-preserves-source-and-the-other-presentation
           (list (equal? basis (head:edit-basis b)) (eq? retained (head:window-lines other))
                 (head:window-lines w)) (list #t #t changed))
         (check 'invalid-window-presentations-refuse-the-whole-update
           (map (lambda (bad)
                  (and (refused? (lambda () (head:view-replace! b '("bad" "source") '() '() bad)))
                       (equal? basis (head:edit-basis b))
                       (equal? changed (head:window-lines w)) (eq? retained (head:window-lines other))))
             (list (list (cons w '("missing row")))
                   (list (cons w wide) (cons w narrow))
                   (list (cons w '("embedded\nnewline" "row")))
                   (list (cons 'spot wide)))) '(#t #t #t #t)))
       (head:detach-app! b)
       (check 'detachment-and-source-replacement-retire-window-presentations
         (list (map head:window-lines (list w other))
               (begin (head:register-view! b void)
                      (head:view-replace! b source '() '() (list (cons w wide) (cons other narrow)))
                      (head:buffer-lines-set! b '#("replacement"))
                      (map head:window-lines (list w other))))
         (list (make-list 2 (list->vector source)) '(#("replacement") #("replacement"))))
       (head:set-repaint-hook! paint:invalidate-screen-cache!)
       (head:set-layout-root! root)
       (head:show-buffer! was)
       (head:forget-buffer! b))

     ;; A shared label wins even when it arrives after the local tool.
     (define collision
       (store:create! '(agent test) "<renamed app>" '("shared")))
     (head:sync-foreign-edits!)
     (check 'foreign-create-displaces-local (head:buffer-name app) "<renamed app 2>")
     (check 'foreign-label-resolves-to-store
            (head:buffer-store-id (head:buffer-named "<renamed app>")) collision)
     (check 'identity-after-foreign-collision
            (eq? (head:register-view! "*app-collision*" void) app) #t)
     (store:rename! '(agent test) collision "<renamed app 2>")
     (head:sync-foreign-edits!)
     (check 'foreign-rename-displaces-local
            (head:buffer-name app) "<renamed app 2 2>")
     (check 'ordinary-user-rename-avoids-store
            (begin (head:buffer-name-set! app "<renamed app 2>") (head:buffer-name app))
            "<renamed app 2 2>")

     ;; Names are claimed on list entry as well as construction.
     (define first (head:new-local-buffer! "pending"))
     (define second (head:new-local-buffer! "pending"))
     (head:show-buffer! first)
     (head:show-buffer! second)
     (check 'late-name-claim-keeps-first (head:buffer-name first) "<pending>")
     (check 'late-name-claim-suffixes-second (head:buffer-name second) "<pending 2>")

     ;; Snapshot tools share the same identity rule, never user text.
     (define user-help (head:new-buffer! "*help*"))
     (head:buffer-lines-set! user-help (vector "my notes"))
     (head:buffer-read-only-set! user-help #t)
     (head:add-buffer! user-help)
     (define tool (head:fresh-buffer! "*help*"))
     (check 'snapshot-is-local (head:buffer-store-id tool) #f)
     (check 'snapshot-label-is-local (head:buffer-name tool) "<help>")
     (check 'shared-tool-like-name-is-unchanged (head:buffer-name user-help) "*help*")
     (check 'snapshot-preserves-user-text (head:buffer-lines user-help) '#("my notes"))
     (check 'snapshot-preserves-user-dirty (head:buffer-modified user-help) #t)
     (check 'snapshot-preserves-user-read-only (head:buffer-read-only user-help) #t)
     (head:buffer-name-set! tool "help renamed")
     (check 'snapshot-reuses-renamed-tool (eq? (head:fresh-buffer! "*help*") tool) #t)

     ;; Refresh and kill use local lifecycle only.
     (define events '())
     (define subscription
       (store:subscribe! #f (lambda (event) (set! events (cons event events)))))
     (define before-local (store-ids))
     (define transient (head:register-view! "*transient*" void))
     (head:view-replace! transient '("one"))
     (head:view-append! transient '("two"))
     (head:buffer-name-set! transient "transient renamed")
     (kill-buffer! transient)
     (check 'killed-tool-not-found (head:find-tool-buffer "*transient*") #f)
     (check 'killed-app-not-registered (head:app-of transient) #f)
     (check 'local-app-store-list-unchanged (store-ids) before-local)
     (check 'local-app-no-store-events events '())
     (define recreated (head:register-view! "*transient*" void))
     (check 'killed-tool-gets-new-identity (eq? transient recreated) #f)
     (store:unsubscribe! subscription)

     ;; Git's lazy views use tool identity too, including after a kill.
     ;; Seed their keys without opening a repository or spawning Git.
     (define git-log (head:register-view! "*git-log*" void))
     (define git-diff (head:register-view! "*git-diff*" void))
     (head:buffer-name-set! git-log "renamed git log")
     (head:buffer-name-set! git-diff "renamed git diff")
     (parameterize ([kernel:registering-module 'git-view-test]) (git-view:init!))
     (check 'git-rebinds-renamed-log (head:buffer-fact git-log 'mode #f) "git:log")
     (check 'git-rebinds-renamed-diff (head:buffer-fact git-diff 'mode #f) "git:diff")
     (kill-buffer! git-log)
     (parameterize ([kernel:registering-module 'git-view-test]) (git-view:init!))
     (define new-git-log (head:find-tool-buffer "*git-log*"))
     (check 'git-recreates-killed-log (and new-git-log (not (eq? new-git-log git-log))) #t)
     (check 'recreated-git-log-is-an-app (head:app-buffer? new-git-log) #t)
     (check 'git-retains-surviving-diff
            (eq? git-diff (head:find-tool-buffer "*git-diff*")) #t)
     (kernel:retract-module! 'git-view-test)
     (parameterize ([kernel:registering-module 'git-view-test]) (git-view:init!))
     (check 'git-rebinds-after-registry-retraction (head:app-buffer? new-git-log) #t)

     ;; Simulate the registry retraction performed during module reload.
     ;; Both log views must rebind, retain identity, and rebuild once.
     (log:register-formatter! 'app-probe values (lambda (text) (make-vector (string-length text) 'string)))
     (actor:call-as '(head "alice:\tλ") (lambda () (log:add! 'app-probe "before reload" #f)))
     (define all-log #f)
     (define filtered-log #f)
     (parameterize ([kernel:registering-module 'log-view-test])
       (log-view:init!)
       (set! all-log (log-view:buffer!))
       (set! filtered-log (log-view:buffer! 'app-probe)))
     (head:buffer-name-set! all-log "renamed log")
     (head:buffer-name-set! filtered-log "renamed filtered log")
     (define old-all (head:buffer-lines all-log))
     (define old-filtered (head:buffer-lines filtered-log))
     (check 'log-actor-prefix-is-separate-from-component-styling
       (let* ([line (vector-ref old-filtered 0)]
              [styles ((mode:styles (mode:find "log")) line)]
              [prefix (- (string-length line) (string-length "before reload"))])
         (list (for-all (lambda (i) (eq? (vector-ref styles i) 'comment)) (iota prefix))
               (for-all (lambda (i) (eq? (vector-ref styles (+ prefix i)) 'string))
                        (iota (string-length "before reload")))))
       '(#t #t))
     (define log-records (log:entries))
     (let ([views (list all-log filtered-log)])
       (kernel:retract-module! 'log-view-test)
       (check 'log-view-registrations-retracted (map head:app-buffer? views) '(#f #f))
       (parameterize ([kernel:registering-module 'log-view-test]) (log-view:init!))
       (check 'log-views-rebind-with-the-same-identities-and-renderings
         (list (map eq? views (list (log-view:buffer!) (log-view:buffer! 'app-probe)))
               (map head:app-buffer? views) (map head:buffer-lines views))
         (list '(#t #t) '(#t #t) (list old-all old-filtered))))
     (check 'rebuild-does-not-change-records (log:entries) log-records)
     (log:add! 'app-probe "after reload" #f)
     (head:show-buffer! filtered-log)
     (head:refresh-visible-views!)
     (check 'filtered-refresh-after-reload
            (vector-length (head:buffer-lines filtered-log))
            (+ 1 (vector-length old-filtered)))
     (head:show-buffer! all-log)
     (head:refresh-visible-views!)
     (check 'log-refresh-after-reload
            (vector-length (head:buffer-lines all-log))
            (+ 1 (vector-length old-all)))

     ;; A formatter may log while a refresh is building its rows.  That
     ;; new record belongs to the next refresh, not to the completed one.
     (define logged-during-refresh? #f)
     (log:register-formatter! 'during-refresh
       (lambda (datum)
         (unless logged-during-refresh?
           (set! logged-during-refresh? #t)
           (log:add! 'during-refresh "arrived while formatting" #f)
           (head:refresh-visible-views!))
         datum))
     (log:add! 'during-refresh "first" #f)
     (define arrivals (log-view:buffer! 'during-refresh))
     (check 'refresh-has-a-fixed-record-bound
            (vector-length (head:buffer-lines arrivals)) 1)
     (head:show-buffer! arrivals)
     (head:refresh-visible-views!)
     (check 'next-refresh-picks-up-interleaved-record
            (vector-length (head:buffer-lines arrivals)) 2)
     (set! logged-during-refresh? #f)
     (log:add! 'during-refresh "visible formatter reentry" #f)
     (head:refresh-visible-views!)
     (check 'reentrant-refresh-does-not-adopt-the-same-batch-twice
       (list (vector-length (head:buffer-lines arrivals))
             (begin (head:refresh-visible-views!) (vector-length (head:buffer-lines arrivals))))
       '(3 4))

     ;; A runtime-created view is registered outside module init.  It
     ;; survives owner retraction, but init must still replace its code.
     (define runtime-refresh (head:app-refresh! (head:app-of arrivals)))
     (parameterize ([kernel:registering-module 'log-view-test]) (log-view:init!))
     (check 'runtime-log-identity-survives-reload
            (eq? arrivals (log-view:buffer! 'during-refresh)) #t)
     (check 'runtime-log-callback-is-rebound
            (eq? runtime-refresh (head:app-refresh! (head:app-of arrivals))) #f)

     ;; Views keep a recent window independently of the larger journal. Expiry
     ;; removes complete multiline records and gaps from other components.
     (let ([formatted 0] [w (head:current-window)])
       (log:retention 10000)
       (log:register-formatter! 'retention
         (lambda (datum) (set! formatted (+ formatted 1)) datum))
       (log:add! 'retention "expired\ntwo" #f)
       (log:add! 'noise "between" #f)
       (log:add! 'retention "kept\nthree\nlines" #f)
       (let ([b (log-view:buffer! 'retention)])
         (head:show-buffer! b)
         (head:window-prow-set! w 2)
         (head:window-pcol-set! w 5)
         (head:window-top-set! w 2)
         (head:buffer-mark-row-set! b 3)
         (head:buffer-mark-col-set! b 6)
         (do ([i 0 (+ i 1)]) ((= i 4094)) (log:add! 'noise i #f))
         (head:refresh-visible-views!)
         (check 'filtered-expiry-keeps-surviving-text-and-positions-without-reformatting
           (list (vector-length (head:buffer-lines b)) (head:buffer-point b)
                 (head:window-top w) (head:buffer-mark-row b) (head:buffer-mark-col b) formatted
                 (map log:datum (log:entries 'retention)))
           '(3 (0 . 5) 0 1 6 2 ("kept\nthree\nlines" "expired\ntwo")))
         (head:show-buffer! all-log)
         (head:refresh-visible-views!)
         (check 'hidden-log-resyncs-to-the-retained-range-including-multiline-rows
           (vector-length (head:buffer-lines all-log)) 4098)
         (do ([i 0 (+ i 1)]) ((= i 4096)) (log:add! 'noise i #f))
         (head:refresh-visible-views!)
         (head:show-buffer! b)
         (head:refresh-visible-views!)
         (check 'views-expire-all-old-rows-even-with-no-new-matching-record
           (list (head:buffer-lines b) (head:buffer-point b)
                 (vector-length (head:buffer-lines all-log)))
           '(#("") (0 . 0) 4096))
         (log:add! 'retention "returned" #f)
         (head:refresh-visible-views!)
         (check 'empty-filtered-view-resumes-on-the-next-match
           (list (vector-length (head:buffer-lines b))
                 (let ([line (vector-ref (head:buffer-lines b) 0)])
                   (substring line (- (string-length line) 8) (string-length line))))
           '(1 "returned"))
         (head:show-buffer! all-log)
         (head:refresh-visible-views!)
         (let ([before (let-values ([(records end first) (log:snapshot 0 0)]) end)])
           (log:retention 3)
           (head:refresh-visible-views!)
           (log:retention 4096)
           (head:refresh-visible-views!)
           (check 'retention-shrink-refreshes-without-an-append-and-grow-keeps-expiry
             (list (vector-length (head:buffer-lines all-log))
                   (let-values ([(records end first) (log:snapshot 0 0)])
                     (list (= end before) (- end first))))
             '(3 (#t 3))))))

     ;; Mouse routing needs the handler's focus decision, not only truth.
     (let* ([previous (head:current-buffer)] [result #f]
            [b (head:register-app! "dispatch-results" void (lambda (event) result))])
       (head:show-buffer! b)
       (check 'local-dispatch-preserves-focus-results
         (map (lambda (value) (set! result value) (head:dispatch-app-event! "MOUSE-CLICK"))
           '(#f #t keep-focus ignore-click))
         '(#f #t keep-focus ignore-click))
       (head:show-buffer! previous)
       (head:forget-buffer! b))

     ;; One shared app exercises the complete head adapter. Its endpoint only
     ;; receives data; no head app record or terminal renderer is registered.
     (let* ([previous (head:current-buffer)] [w (head:current-window)] [owner '(app adapter-test)]
            [messages (test:recorder)] [reenter #f]
            [text (make-vector 50 "abc")])
       (define (receive message)
         (messages message)
         (when reenter
           (let ([next reenter]) (set! reenter #f) (next))))
       (define (received kind) (filter (lambda (message) (eq? (car message) kind)) (messages)))
       (vector-set! text 48 "界e\x301;Z")
       (vector-set! text 49 "界e\x301;Z")
       (actor:register! owner receive)
       (let* ([id (store:create! owner "*adapter-test*" text
                    `((app . ,owner) (alive . #t) (capture . all) (status . "working")
                      (read-only . #t) (wrap . #f) (manages-viewport . #t) (cursor-style . bar)))]
              [b (head:adopt-store-buffer! id)]
              [other (head:make-window b 0 0 0 0 0 2 7 3 'default)])
         (define (publish row visible?)
           (surface:publish! id (let ([old (surface:snapshot id)]) (and old (car old))) 0
             '((48 #(plain plain plain plain) #(#f #f #f #f) ((clusters (1 . 2) (2 . 1) (1 . 1))))
               (49 #(plain plain plain plain) #(#f #f #f #f) ((clusters (1 . 2) (2 . 1) (1 . 1)))))
             (list row 2 visible?) '(4 4)))
         (define (position w) (list (head:window-prow w) (head:window-pcol w) (head:window-top w)))
         (publish 48 #t)
         (head:window-size-set! w 0)
         (head:window-width-set! w 0)
         (head:set-layout-root! (head:make-layout-split 'right w other 1 1))
         (let ([seen #f])
           (head:set-repaint-hook!
             (lambda () (set! seen (list (position w) (head:app-cursor-style b)
                                         (and (render:row (head:buffer-rendition b) 48) #t)))))
           (head:set-window-buffer! w b)
           (check 'shared-adoption-follows-before-first-layout-with-its-own-projection seen '((48 1 48) bar #t)))
         (head:set-repaint-hook! paint:invalidate-screen-cache!)
         (head:window-size-set! w 4)
         (head:window-width-set! w 6)
         (head:refresh-renditions!)
         (check 'shared-grid-follows-in-different-window-sizes
           (list (position w) (position other) (head:app-buffer? b) (head:app-of b))
           '((48 1 46) (48 1 47) #t #f))
         (head:follow-app! other #f)
         (head:window-prow-set! other 0)
         (head:window-pcol-set! other 0)
         (head:window-top-set! other 0)
         (publish 49 #f)
         (head:refresh-renditions!)
         (check 'following-is-per-window-and-honors-published-visibility
           (list (position w) (position other) (head:app-cursor-visible-in? w)
                 (head:app-cursor-visible-in? other)) '((49 1 46) (0 0 0) #f #t))
         (check 'capture-is-declarative
           (map (lambda (rule)
                  (store:set-property! owner id 'capture rule)
                  (map head:dispatch-app-event! '("UP" "x")))
             '(#f all () ("UP") (except "UP")))
           '((#f #f) (#t #t) (#f #f) (#t #f) (#f #t)))
         (store:set-property! owner id 'capture 'all)
         (let ([before (store:properties id)])
           (check 'invalid-app-facts-refuse-the-whole-batch
             (map (lambda (bad)
                    (and (refused? (lambda () (store:set-properties! owner id (list '(status . "wrong") bad))))
                         (equal? before (store:properties id))))
               '((app . (head "wrong")) (alive . yes) (capture . #t) (capture except 4)
                 (status . 3) (sticky-lines . -1) (cursor-style . invalid) (manages-viewport . 1)))
             '(#t #t #t #t #t #t #t #t)))
         (let ([paste (string-copy "paste me")] [raw (cons 48 1)] [identity (actor:current)])
           (head:set-pending-paste! paste)
           (parameterize ([head:app-event-buffer-position raw] [head:app-event-position '(3 . 2)] [head:app-event-button 0])
             (head:dispatch-app-event! "PASTE"))
           (string-set! paste 0 #\X)
           (set-car! raw 999)
           (let* ([message (car (reverse (received 'input)))] [data (list-ref message 4)])
             (check 'shared-input-owns-paste-and-both-coordinate-spaces
               (list (cadr message) (caddr message) (cadddr message) data)
               (list head:ui-actor id "PASTE"
                 `((paste . "paste me") (point 48 . 1) (cell 48 . 2) (viewport 3 . 2) (button . 0)
                   (size 4 6) (color-scheme . #f) (revision . 0) (generation . ,(car (surface:snapshot id))))))
             (set-car! (cadr message) 'damaged)
             (check 'delivery-cannot-mutate-head-identity-or-context
               (list (car head:ui-actor) (actor:current) (head:app-event-buffer-position)) (list 'head identity #f))))
         (let ([before (length (received 'input))] [ran? #f] [commands 0])
           (define (toggle!) (head:set-full-capture! (head:current-window) (not (head:full-capture? (head:current-window)))))
           (mode:register! "adapter-test" '() '() (lambda (line) #f))
           (head:with-buffer b (mode:choose! "adapter-test"))
           (keymap:bind-default! 'adapter-test "UP" (lambda () (set! ran? #t)))
           (keymap:set-context-capture! 'adapter-test "C-]" toggle! '("C-x" "M-x"))
           (keymap:bind-default! "M-x" (lambda () (set! commands (+ commands 1))))
           (dispatch:key! "UP")
           (check 'mode-command-wins-and-pauses-following
             (list ran? (head:app-following? w) (- (length (received 'input)) before)) '(#t #f 0))
           (dispatch:key! "x")
           (check 'captured-key-resumes-following (head:app-following? w) #t)
           (check 'capture-routing-and-toggle-preserve-cursor-following
             (reverse
               (fold-left (lambda (out event)
                            (let ([before (length (received 'input))])
                              (dispatch:key! event)
                              (cons (list (head:full-capture? w) commands (- (length (received 'input)) before)
                                          (head:app-following? w)) out)))
                 '() '("M-x" "x" "C-]" "M-x" "C-x" "C-]")))
             '((#f 1 0 #f) (#f 1 1 #t) (#t 1 0 #t) (#t 1 1 #t) (#t 1 1 #t) (#f 1 0 #t)))
           (head:follow-app! w #f)
           (dispatch:key! "C-]")
           (check 'capture-is-per-window-and-keeps-scrollback-and-producer-status
             (list (head:full-capture? w) (head:full-capture? other) (head:app-following? w)
                   (head:app-cursor-visible-in? w) (head:app-status b))
             '(#t #f #f #t "working"))
           (dispatch:key! "C-]"))
         (set! reenter head:request-app-size!)
         (head:request-app-size!)
         (head:request-app-size!)
         (head:window-width-set! w 5)
         (head:request-app-size!)
         (check 'size-offers-coalesce-before-reentrant-delivery
           (received 'request)
           (list (list 'request head:ui-actor id 'resize '(4 6))
                 (list 'request head:ui-actor id 'resize '(4 5))))
         (set! reenter (lambda () (head:follow-app! w #f) (head:refresh-renditions!)))
         (head:dispatch-app-event! "x")
         (check 'delivery-does-not-overwrite-reentrant-follow-state (head:app-following? w) #f)
         (store:set-property! owner id 'audience '())
         (check 'hidden-app-denies-retained-facts-and-input-before-cleanup
           (list (head:app-facts b) (head:dispatch-app-event! "x")) '(#f #f))
         (store:set-property! owner id 'audience 'all)
         (actor:detach! owner)
         (check 'unreachable-endpoint-declines-input (head:dispatch-app-event! "x") #f)
         (actor:register! owner receive)
         (store:set-properties! owner id '((alive . #f) (capture . #f) (status . "exited")))
         (surface:withdraw! id (car (surface:snapshot id)))
         (head:refresh-renditions!)
         (check 'death-restores-ordinary-input-and-cursor-but-keeps-status
           (list (head:app-buffer? b) (head:dispatch-app-event! "x") (head:app-cursor-style b)
                 (head:app-cursor-visible-in? w) (head:app-manages-window-viewport? w)
                 (head:app-status b) (store:line id 48) (mode:name-of b)
                 (map (lambda (full?)
                        (head:set-full-capture! w full?)
                        (dispatch:key! "C-]")
                        (list (mode:key-context b) (head:full-capture? w))) '(#f #t)))
           '(#f #f #f #t #f "exited" "界e\x301;Z" "adapter-test" ((#f #f) (#f #t))))
         (head:set-layout-root! w)
         (head:set-window-buffer! w previous)
         (store:delete! owner id)
         (head:before-frame!)
         (actor:detach! owner)))

     ;; The ordinary buffer path validates generated hyperlink ranges too.
     (let ([b (head:new-buffer! "*hyperlink-test*")])
       (head:with-buffer b (insert-text! "https://example.com/path"))
       (check 'buffer-link-ranges (paint:buffer-line-hyperlinks b 0)
         '((0 24 "https://example.com/path")))
       (kill-buffer! b))

     ;; Two view identities share data, while geometry, selection and renderer
     ;; lifetime remain independent. Reuse the app fixture and its windows.
     (interaction:init!) (widget:init!) (window:init!) (entry:init!)
     (define model-checks 0)
     (model:register-kind! 'widget-test 1 (lambda (value) (set! model-checks (+ model-checks 1)) (string? value)))
     (let* ([root (head:root)] [w (head:current-window)] [was (head:current-buffer)]
            [owner (string-copy "widget-test-renderer")] [calls 0]
            [data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "original")]
            [first (view:create! head:ui-actor data 'probe 1 '() 0)]
            [second (view:create! head:ui-actor data 'probe 1 '() 0)]
            [other (head:make-window was 0 0 0 0 0 2 7 24 'default)]
            [a (window:show-widget! w first)] [b (window:show-widget! other second)])
       (define (install!)
         (parameterize ([kernel:registering-module owner])
           (widget:register! (quote probe) 1 (list (cons (quote render) (lambda (model descriptor width height range) (define state (view:state descriptor)) (set! calls (+ calls 1)) (make-list (+ height 2) (format "~a ~a 界界界界界界界界" (cdr (assq (quote value) model)) state)))) (cons (quote actions) (list (cons (quote choose) (lambda (id) (let-values ([(model descriptor inputs) (widget:context id)]) (values (cdr (assq (quote revision) model)) (view:state descriptor)))))))))))
       (define (refresh!) (for-each (lambda (buffer) ((head:app-refresh! (head:app-of buffer)))) (list a b)))
       (install!)
       (head:show-buffer! a)
       (head:set-layout-root! (head:make-layout-split 'right w other 1 2))
       (head:window-width-set! w 12) (head:window-size-set! w 4)
       (head:window-width-set! other 24) (head:window-size-set! other 2)
       (refresh!)
       (let ([before model-checks])
         (refresh!)
         (check 'widget-warm-frame-reuses-its-owned-model-snapshot model-checks before))
       (check 'widget-host-bounds-and-cached-rendering
         (list calls (map (lambda (window) (map glyph:cells (vector->list (head:window-lines window)))) (list w other)))
         '(2 ((12 12 12 12) (24 24))))
       (interaction:set-state! head:ui-actor first 0 9)
       (check 'widget-action-uses-provisional-target-before-ack
         (list (call-with-values (lambda () (widget:act! first 'choose)) list)
               (view:state (view:snapshot first)) (view:state (interaction:snapshot second))) '((0 9) 0 0))
       (head:checkpoint!)
       (check 'widget-lifecycle-checkpoint-fences-state (view:state (view:snapshot first)) 9)
       (model:commit! '(base test) (list (list data 0 '() "new")))
       (refresh!)
       (let ([before calls])
         (head:window-width-set! other 8) (refresh!)
         (check 'widget-resize-only-rerenders-affected-view (- calls before) 1))
       (kernel:retract-module! owner) (refresh!)
       (check 'widget-unavailable-renderer-keeps-mount-without-actions
         (list (widget:actions first) (head:app-refresh-error (head:app-of a))) '(() #f))
       (install!) (refresh!)
       (check 'widget-late-renderer-reclaims-view (widget:actions first) '(choose))
       (head:forget-buffer! a) (head:forget-buffer! b)
       (check 'widget-unmount-and-buffer-kill-retain-model-and-descriptors
         (list (map (lambda (id) (view:owner (view:snapshot id))) (list first second))
               (cdr (assq 'value (model:snapshot data))) (head:app-of a) (head:app-of b)) '((#f #f) "new" #f #f))
       (head:set-layout-root! root) (head:show-buffer! was)
       (kernel:retract-module! owner))

     ;; Tree mounts allocate no buffers. Reorder retains identity and state;
     ;; failed ownership changes and duplicate hosts cannot release the tree.
     (let* ([data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "one\ntwo")]
            [parent (view:create! head:ui-actor #f 'column 1 '() '())]
            [a (view:create! head:ui-actor data 'text 2 '() 0)]
            [b (view:create! head:ui-actor data 'text 2 '() 0)]
            [count (length (head:buffers))])
       (view:arrange! head:ui-actor (list (list parent 0 (list (list 'a a 'fit) (list 'b b '(grow 1))) '())) '())
       (let ([m (widget:mount! parent 'slot)])
         (check 'recursive-mount-idempotent-with-no-adapter-buffers
           (list (eq? m (widget:mount! parent 'slot)) (= count (length (head:buffers)))
             (refused? (lambda () (widget:mount! parent 'another)))
             (refused? (lambda () (widget:mount! a 'nested)))) '(#t #t #t #t))
         (widget:act! a 'move 1)
         (let-values ([(status rows) (widget:arrange! (list (list parent (cdr (assq 'revision (model:snapshot parent)))
                                                              (list (list 'b b '(grow 1)) (list 'a a 'fit)) '())))])
           (check 'reordered-child-keeps-local-state-and-parent
             (list status (view:state (interaction:snapshot a)) (view:parent (interaction:snapshot a)))
             (list 'applied 1 parent)))
         (let-values ([(status rows) (widget:arrange! (list (list parent (cdr (assq 'revision (model:snapshot parent))) (list (list 'a a 'fit)) '())))])
           (check 'removed-child-releases-runtime-and-owner
             (list status (refused? (lambda () (widget:actions b))) (view:owner (view:snapshot b))) '(applied #t #f)))
         (widget:unmount! parent)
         (check 'recursive-release (map (lambda (id) (view:owner (view:snapshot id))) (list parent a)) '(#f #f))))

     ;; Additional window placement forks descriptors; reopening hidden roots
     ;; reuses their adapter and remembered interaction.
     (let* ([w (head:current-window)] [was (head:current-buffer)]
            [data (model:create! head:ui-actor 'widget-test 1 'session 'persistent '() "shared")]
            [id (view:create! head:ui-actor data 'text 2 '() 0)]
            [b (window:show-widget! w id)]
            [other (head:make-window b 0 0 0 0 0 2 0 12 'default)]
            [copy (head:window-buffer other)] [fork (head:buffer-fact copy 'widget-id #f)])
       (check 'placement-forks-only-views (list (equal? id fork) (view:source (view:snapshot fork))) (list #f data))
       (head:show-buffer! was)
       (check 'hidden-root-reuses-buffer (eq? b (window:show-widget! w id)) #t)
       (widget:unmount! id)
       (check 'explicitly-unmounted-adapter-remounts-on-show
         (begin (window:show-widget! w id) (widget:actions id)) '(move select choose))
       (actor:checkpoint! head:ui-actor
         `(screen 4 1 (split right 1 1 (window 1 0 0 0 default #t #f default) (window 2 0 0 0 default #t #f default))
            (((widget ,id) #f ()))))
       (head:resume!)
       (let* ([windows (filter (lambda (w) (not (head:popup? w))) (head:windows))]
              [ids (map (lambda (w) (head:buffer-fact (head:window-buffer w) 'widget-id #f)) windows)])
         (check 'resume-resolves-duplicate-widget-placements-before-installing-layout
           (list (length ids) (equal? (car ids) (cadr ids))
                 (map (lambda (id) (view:source (view:snapshot id))) ids))
           (list 2 #f (list data data))))
       (window:delete-others!) (head:show-buffer! was)
       (for-each (lambda (b) (when (head:buffer-fact b 'widget-id #f) (head:forget-buffer! b))) (head:buffers)))

     ;; A bare composition exercises the same routing used by window hosts.
     (let* ([events '()] [owner 'routing-fixture]
            [a (view:create! head:ui-actor #f 'route-leaf 1 '() '())]
            [b (view:create! head:ui-actor #f 'route-leaf 1 '() '())]
            [row (view:create! head:ui-actor #f 'route-row 1 '() '())]
            [root (view:create! head:ui-actor #f 'overlay 1 '() '())]
            [barrier (view:create! head:ui-actor #f 'overlay 1 '((modal . #t)) '())])
       (define (record! id tag) (set! events (cons (list tag id) events)))
       (define (install-leaf! full?)
         (parameterize ([kernel:registering-module owner])
           (widget:register! 'route-leaf 1
             (list (cons 'focus #t) (cons 'contexts '(route-leaf)) (cons 'capture (if full? 'full 'partial))
               (cons 'actions (list (cons 'record record!)))
               (cons 'event (lambda (id source d event)
                              (case (car event)
                                [(text) (record! id (cadr event)) #t]
                                [(pointer) (and (not (eq? (caddr event) 'middle))
                                             (begin (when (eq? (cadr event) 'press) (widget:capture! id)) (record! id (cadr event)) #t))]
                                [(cancel) (record! id 'cancel) #t]
                                [else #f])))))))
       (define (show!) (widget:present! (list (list (widget:prepare! root 10 2) 0 0))))
       (define (key! key) (dispatch:input! root (list 'key key (and (= 1 (string-length key)) key))))
       (define (take) (let ([out (reverse events)]) (set! events '()) out))
       (install-leaf! #f)
       (widget:register! 'route-row 1
         (list (cons 'contexts '(route-parent)) (cons 'capture-contexts '(route-capture))
           (cons 'actions (list (cons 'record record!)))
           (cons 'event (lambda (id source d event) (and (eq? (car event) 'pointer) (begin (record! id 'bubbled) #t))))
           (cons 'layout (lambda (d width height measure locate)
                           (map (lambda (child x) (list (cadr child) (list x 0 5 height))) (view:children d) '(0 5))))))
       (for-each (lambda (entry)
                   (keymap:bind-default! (car entry) (cadr entry) (keymap:call widget:act! widget:target 'record (caddr entry))))
         '((route-leaf "C-x a" leaf) (route-leaf "F2" shadowed) (route-leaf "z" shortcut)
           (route-parent "F1" parent) (route-capture "F2" capture)))
       (view:arrange! head:ui-actor
         (list (list root 0 (list (list 'body row '(grow 1))) '())
               (list row 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 1))) '())) '())
       (widget:mount! root 'routing-test) (show!) (widget:focus! root a)
       (key! "C-x")
       (check 'chord-start-returns-without-reading-input (dispatch:pending?) #t)
       (key! "a") (key! "F1") (key! "F2") (key! "z")
       (dispatch:input! root '(text "z z" paste))
       (check 'explicit-receivers-capture-phase-and-text-not-as-keys (take)
         (list (list 'leaf a) (list 'parent row) (list 'capture row) (list 'shortcut a) (list "z z" a)))
       (key! "C-x") (widget:focus! root b) (key! "a")
       (key! "C-x") (keymap:bind-default! 'unrelated "F9" void) (key! "a")
       (check 'focus-and-binding-change-do-not-replay-chord-suffix (take) '())
       (widget:focus! root a) (show!)
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:pointer! '(pointer move primary ()) 99 99)
       (widget:pointer! '(pointer release primary ()) 99 99)
       (widget:pointer! '(pointer move none ()) 99 99)
       (check 'pointer-capture-survives-outside-and-ends-on-release (take)
         (list (list 'press a) (list 'move a) (list 'release a) (list 'leave a)))
       (widget:pointer! '(pointer press middle ()) 1 0)
       (check 'unhandled-pointer-bubbles-to-its-own-parent (take) (list (list 'bubbled row)))
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:present! (list (list (widget:prepare! root 0 0) 0 0)))
       (widget:pointer! '(pointer release primary ()) 1 0)
       (check 'hidden-capture-cancels-without-retargeting-or-forgetting-focus
         (list (take) (view:focus (interaction:snapshot root)))
         (list (list (list 'press a) (list 'cancel a)) a))
       (show!)
       (key! "C-x") (kernel:retract-module! owner) (install-leaf! #t) (show!) (key! "a")
       (key! "F1") (key! "x") (key! "F2")
       (check 'reload-cancels-chord-full-capture-still-types-and-ancestor-capture-wins (take)
         (list (list "x" a) (list 'capture row)))
       (interaction:flush!)
       (let-values ([(status rows) (widget:arrange! (list (list root (cdr (assq 'revision (model:snapshot root)))
                                                            (list (list 'body row '(grow 1)) (list 'modal barrier '(grow 1))) '())))
                    ])
         (check 'modal-attachment-commits status 'applied))
       (show!) (key! "x") (widget:pointer! '(pointer press primary ()) 1 0)
       (check 'empty-modal-has-no-key-or-pointer-click-through (take) '())
       (widget:unmount! root) (kernel:retract-module! owner)
       (widget:invalidate!))

     ;; Entries share authored text and its journal, but never cursor state.
     (let* ([source (store:create! head:ui-actor "widget entry" '("a界éz"))]
            [a (view:create! head:ui-actor (list 'buffer source) 'entry 1 '() '((0 . 0) (0 . 0)))]
            [b (view:create! head:ui-actor (list 'buffer source) 'entry 1 '() '((0 . 0) (0 . 0)))]
            [root (view:create! head:ui-actor #f 'row 1 '() '())]
            [ambient (head:current-buffer)])
       (define (line) (let-values ([(text rev) (store:snapshot source)]) (vector-ref text 0)))
       (define (show!) (widget:present! (list (list (widget:prepare! root 20 1) 0 0))))
       (define (foreign! start end replacement)
         (let-values ([(text rev) (store:snapshot source)])
           (store:edit! '(agent "entry-test") source rev (text:make-span 0 start 0 end) replacement))
         (head:sync-foreign-edits! source))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'a a '(grow 1)) (list 'b b '(grow 1))) '())) '())
       (widget:mount! root 'entry-test) (show!)
       (entry:move! a 'right) (entry:move! a 'right) (entry:move! a 'right)
       (check 'entry-moves-by-graphemes-and-keeps-independent-selection
         (list (view:state (interaction:snapshot a)) (view:state (interaction:snapshot b)))
         '(((0 . 4) (0 . 4)) ((0 . 0) (0 . 0))))
       (entry:move! a 'left #t) (show!)
       (check 'entry-caret-and-selection-share-cell-geometry
         (list (widget:caret (widget:prepared root))
               (vector-ref (widget:frame-styles (widget:prepared root) 0 (car (widget:frame-lines (widget:prepared root)))) 2))
         '((3 . 0) selection))
       (widget:focus! root b) (show!)
       (check 'entry-caret-translates-into-the-second-child (widget:caret (widget:prepared root)) '(10 . 0))
       (widget:pointer! '(pointer move primary ()) 14 0)
       (check 'entry-does-not-start-a-drag-from-another-widget (view:state (interaction:snapshot b)) '((0 . 0) (0 . 0)))
       (widget:pointer! '(pointer press primary ()) 11 0)
       (widget:pointer! '(pointer move primary ()) 14 0)
       (widget:pointer! '(pointer release primary ()) 14 0)
       (check 'entry-drag-selects-whole-wide-and-combining-graphemes
         (view:state (interaction:snapshot b)) '((0 . 4) (0 . 1)))
       (widget:focus! root a) (show!)
       (dispatch:input! root '(key "TAB" #f))
       (check 'entry-tab-uses-host-traversal (view:focus (interaction:snapshot root)) b)
       (dispatch:input! root '(key "S-TAB" #f))
       (dispatch:input! root '(text "Q" paste))
       (check 'entry-paste-replaces-selection-once (line) "a界Qz")
       (entry:undo! a) (entry:redo! a)
       (check 'entry-uses-existing-undo-redo (line) "a界Qz")
       (widget:act! a 'delete 'backward)
       (check 'entry-delete-action-keeps-the-original-edit-basis (line) "a界z")
       (entry:undo! a)
       (check 'entry-refuses-multiline-paste-without-mutation
         (list (refused? (lambda () (entry:insert! a "bad\npaste"))) (line)) '(#t "a界Qz"))
       (entry:select! a 1 1) (show!)
       (widget:pointer! '(pointer press primary ()) 1 0)
       (widget:pointer! '(pointer release primary ()) 1 0)
       (foreign! 0 0 '("R"))
       (entry:insert! a "X")
       (check 'entry-click-intent-rebases-through-remote-insertion
         (list (line) (view:state (interaction:snapshot a))) '("RaX界Qz" ((0 . 3) (0 . 3))))
       (entry:select! a 5 3)
       (foreign! 4 4 '("remote"))
       (let ([before (line)])
         (check 'entry-overlap-refuses-without-losing-foreign-text
           (list (refused? (lambda () (entry:insert! a "lost"))) (string=? before (line))) '(#t #t)))
       (foreign! 0 0 '("first" "second")) (show!)
       (check 'entry-external-multiline-is-an-inert-field-not-a-readonly-source
         (list (widget:caret (widget:prepared root)) (head:buffer-read-only (head:buffer-of-store-id source))
               (refused? (lambda () (entry:insert! a "no"))) (eq? ambient (head:current-buffer))) '(#f #f #t #t))
       (widget:unmount! root) (widget:invalidate!))

     (let* ([root (view:create! head:ui-actor #f 'row 1 '() '())]
            [producer (view:create! head:ui-actor #f 'connected-producer 1 '() '((choice . "first")))]
            [consumer (view:create! head:ui-actor #f 'connected-consumer 1 '((choice . "default")) '())])
       (port:register! '(view connected-producer 1) '((output choice string (state choice))))
       (port:register! '(view connected-consumer 1) '((input choice string (options choice))))
       (widget:register! 'connected-producer 1 '())
       (widget:register! 'connected-consumer 1
         (list (cons 'prepare (lambda (source inputs) (caddr (assq 'choice inputs))))
           (cons 'render (lambda (data descriptor width height range) (list data)))
           (cons 'actions (list (cons 'inspect (lambda (id)
                                                 (let-values ([(source descriptor inputs) (widget:context id)])
                                                   (caddr (assq 'choice inputs)))))))))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'producer producer 'fit) (list 'consumer consumer '(grow 1))) '())) '())
       (connection:bind! head:ui-actor root (list (list consumer 'choice #f (list producer 'choice))))
       (widget:mount! root 'connection-fixture)
       (let* ([frame (widget:prepare! root 30 2)] [shown (cadr (widget:frame-children frame))])
         (interaction:set-state! head:ui-actor producer #f '((choice . "next")))
         (check 'widget-connected-input-uses-provisional-state-and-pins-shown-bundle
           (list (widget:act! consumer 'inspect)
             (parameterize ([widget:event-frame shown]) (widget:act! consumer 'inspect))
             (cadr (connection:read consumer 'choice)))
           '("next" "first" "first"))
         (interaction:flush!)
         (check 'widget-published-input-is-visible-at-base (cadr (connection:read consumer 'choice)) "next"))
       (widget:unmount! root) (widget:invalidate!))

     (model:register-kind! 'widget-view 3 string?)
     (let* ([id (model:create! head:ui-actor 'widget-view 3 'session 'persistent '() "future descriptor")]
            [previous (head:current-buffer)] [b (window:show-widget! (head:current-window) id)])
       (head:show-buffer! b)
       ((head:app-refresh! (head:app-of b)))
       (check 'widget-unknown-descriptor-is-inspectable-without-claiming-an-owner
         (list (widget:actions id) (interaction:snapshot id) (cdr (assq 'value (model:snapshot id))))
         '(() #f "future descriptor"))
       (head:forget-buffer! b) (head:show-buffer! previous))

     (include "tests/range.sps")
     (test:finish! 'app)))
