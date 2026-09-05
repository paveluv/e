#!/usr/bin/env scheme-script

;; Local tools and apps: identity survives label changes and reload;
;; a colliding ordinary buffer is never reused or overwritten.  Run
;; from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (except (edit) init!)
             (prefix (head) head:)
             (prefix (store) store:)
             (prefix (kernel) kernel:)
             (prefix (log) log:)
             (prefix (git-view) git-view:)
             (prefix (log-view) log-view:))

     (define checks 0)
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'app-test label actual expected)))
     (define (refused? thunk)
       (guard (ex [else #t]) (thunk) #f))
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
     (define explicit (head:new-local-buffer "explicit app"))
     (check 'register-existing-local
            (eq? explicit (head:register-view! explicit void)) #t)
     (check 'registered-local-is-listed (and (memq explicit (head:buffers)) #t) #t)
     (check 'register-existing-shared-refused
            (refused? (lambda ()
                        (head:register-view! (head:window-buffer (head:current)) void)))
            #t)
     (check 'rejected-shared-registration-keeps-flags
            (head:buffer-read-only (head:window-buffer (head:current))) #f)

     ;; A view never captures an ordinary buffer's label as identity.
     (define ordinary (head:new-buffer "<app-collision>"))
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
                          (list (cons (head:current) '(0 . 0)))))) #t)
     (check 'view-replacement-requires-numeric-placements
            (refused? (lambda () (head:view-replace! app '("bad") '((custom . wrong)) '((mark . end))))) #t)
     (check 'view-replacement-rejects-negative-positions
            (refused? (lambda () (head:view-replace! app '("bad") '((custom . wrong)) '((spot -1 . 0))))) #t)
     (check 'invalid-view-state-keeps-text-and-revision (head:edit-basis app) app-basis)
     (check 'invalid-view-state-keeps-facts (head:buffer-fact app 'custom 'absent) 'absent)
     (head:set-app-presentation! app 1 #t #f 'bar)
     (set-buffer-name! app "renamed app")
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
     (show-buffer! app)
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
     (show-buffer! explicit)
     (goto-point! '(2 . 5))
     (set-mark-command!)
     (head:window-top-set! (head:current) 2)
     (show-buffer! app)
     (head:view-replace! explicit '("x"))
     (show-buffer! explicit)
     (check 'shorter-view-clamps-saved-point (point) '(0 . 1))
     (check 'shorter-view-clamps-selection (mark) '(0 . 1))
     (check 'shorter-view-clamps-saved-viewport (head:window-top (head:current)) 0)
     (goto-point! '(0 . 0))
     (copy-region!)
     (check 'shorter-view-selection-can-be-copied (mark) #f)
     (show-buffer! app)

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
            (head:buffer-name (set-buffer-name! app "<renamed app 2>"))
            "<renamed app 2 2>")

     ;; Names are claimed on list entry as well as construction.
     (define first (head:new-local-buffer "pending"))
     (define second (head:new-local-buffer "pending"))
     (show-buffer! first)
     (show-buffer! second)
     (check 'late-name-claim-keeps-first (head:buffer-name first) "<pending>")
     (check 'late-name-claim-suffixes-second (head:buffer-name second) "<pending 2>")

     ;; Snapshot tools share the same identity rule, never user text.
     (define user-help (head:new-buffer "*help*"))
     (head:buffer-lines-set! user-help (vector "my notes"))
     (head:buffer-read-only-set! user-help #t)
     (define user-history (vector '(saved) '()))
     (head:buffer-history-set! user-help user-history)
     (head:add-buffer! user-help)
     (define tool (fresh-buffer "*help*"))
     (check 'snapshot-is-local (head:buffer-store-id tool) #f)
     (check 'snapshot-label-is-local (head:buffer-name tool) "<help>")
     (check 'shared-tool-like-name-is-unchanged (head:buffer-name user-help) "*help*")
     (check 'snapshot-preserves-user-text (head:buffer-lines user-help) '#("my notes"))
     (check 'snapshot-preserves-user-dirty (head:buffer-modified user-help) #t)
     (check 'snapshot-preserves-user-read-only (head:buffer-read-only user-help) #t)
     (check 'snapshot-preserves-user-history
            (eq? (head:buffer-history user-help) user-history) #t)
     (set-buffer-name! tool "help renamed")
     (check 'snapshot-reuses-renamed-tool (eq? (fresh-buffer "*help*") tool) #t)

     ;; Refresh and kill use local lifecycle only.
     (define events '())
     (define subscription
       (store:subscribe! #f (lambda (event) (set! events (cons event events)))))
     (define before-local (store-ids))
     (define transient (head:register-view! "*transient*" void))
     (head:view-replace! transient '("one"))
     (head:view-append! transient '("two"))
     (set-buffer-name! transient "transient renamed")
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
     (set-buffer-name! git-log "renamed git log")
     (set-buffer-name! git-diff "renamed git diff")
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
     (log:add! 'app-probe "before reload" #f)
     (define all-log #f)
     (define filtered-log #f)
     (parameterize ([kernel:registering-module 'log-view-test])
       (log-view:init!)
       (set! all-log (log-view:buffer))
       (set! filtered-log (log-view:buffer 'app-probe)))
     (set-buffer-name! all-log "renamed log")
     (set-buffer-name! filtered-log "renamed filtered log")
     (define old-all (head:buffer-lines all-log))
     (define old-filtered (head:buffer-lines filtered-log))
     (define log-count (log:length))
     (kernel:retract-module! 'log-view-test)
     (check 'log-registration-retracted (head:app-buffer? all-log) #f)
     (check 'filtered-registration-retracted (head:app-buffer? filtered-log) #f)
     (parameterize ([kernel:registering-module 'log-view-test]) (log-view:init!))
     (check 'log-identity-after-rebind (eq? all-log (log-view:buffer)) #t)
     (check 'filtered-identity-after-rebind
            (eq? filtered-log (log-view:buffer 'app-probe)) #t)
     (check 'log-rebound (head:app-buffer? all-log) #t)
     (check 'filtered-log-rebound (head:app-buffer? filtered-log) #t)
     (check 'log-rebuilt-without-duplicates (head:buffer-lines all-log) old-all)
     (check 'filtered-rebuilt-without-duplicates
            (head:buffer-lines filtered-log) old-filtered)
     (check 'rebuild-does-not-add-records (log:length) log-count)
     (log:add! 'app-probe "after reload" #f)
     (show-buffer! filtered-log)
     (head:refresh-visible-views!)
     (check 'filtered-refresh-after-reload
            (vector-length (head:buffer-lines filtered-log))
            (+ 1 (vector-length old-filtered)))
     (show-buffer! all-log)
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
           (log:add! 'during-refresh "arrived while formatting" #f))
         datum))
     (log:add! 'during-refresh "first" #f)
     (define arrivals (log-view:buffer 'during-refresh))
     (check 'refresh-has-a-fixed-record-bound
            (vector-length (head:buffer-lines arrivals)) 1)
     (show-buffer! arrivals)
     (head:refresh-visible-views!)
     (check 'next-refresh-picks-up-interleaved-record
            (vector-length (head:buffer-lines arrivals)) 2)

     ;; A runtime-created view is registered outside module init.  It
     ;; survives owner retraction, but init must still replace its code.
     (define runtime-refresh (head:app-refresh! (head:app-of arrivals)))
     (parameterize ([kernel:registering-module 'log-view-test]) (log-view:init!))
     (check 'runtime-log-identity-survives-reload
            (eq? arrivals (log-view:buffer 'during-refresh)) #t)
     (check 'runtime-log-callback-is-rebound
            (eq? runtime-refresh (head:app-refresh! (head:app-of arrivals))) #f)

     (format #t "~a app checks passed\n" checks)))
