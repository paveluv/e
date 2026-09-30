;; Composition exercises real entry actions, command references and captures.
(let* ([actor head:ui-actor] [source (store:create! actor "control text" '("original"))]
       [filter (control:create-filter! actor (list 'buffer source) "Filter:" "[ready]")]
       [entry (cadr (assq 'entry (view:children (view:snapshot filter))))]
       [button (view:create! actor #f 'action-text 1
                 (list '(text . "Replace") '(enabled . #t) (list 'commands (list 'activate entry 'insert '("new")))) '())]
       [root (view:create! actor #f 'column 1 '() '())])
  (define (line) (vector-ref (text-source:lines (text-source:lookup source)) 0))
  (define (show!) (let ([f (widget:prepare! root 40 3)]) (widget:present! (list (list f 0 0))) f))
  (define (rebind! action)
    (interaction:flush!)
    (let ([revision (cdr (assq 'revision (model:snapshot button)))]
          [lease (view:generation (interaction:snapshot root))])
      (interaction:arrange! actor
        (list (list button revision '()
                (list '(text . "Replace") '(enabled . #t) (list 'commands (list 'activate entry action '("new"))))))
        (list (list root lease)))))
  (define enabled
    (begin (model:register-kind! 'control-enabled 1 boolean?)
      (model:create! actor 'control-enabled 1 'session 'transient '() #t)))
  (define (enable! value)
    (let ([r (model:snapshot enabled)])
      (model:commit! actor (list (list enabled (cdr (assq 'revision r)) '() value)))))
  (control:init!)
  (port:register! '(model control-enabled 1) '((output enabled boolean (value))))
  (view:arrange! actor (list (list root 0 (list (list 'filter filter 'fit) (list 'button button 'fit)) '())) '())
  (connection:bind! actor root (list (list button 'enabled #f (list enabled 'enabled))))
  (check 'control-validates-command-bindings-and-references
    (list (member entry (descriptor:references (view:snapshot button)))
      (refused? (lambda () (view:create! actor #f 'action-text 1 '((commands (activate (model 1) insert "not a list"))) '()))))
    (list (list entry) #t))
  (let* ([copy (view:fork! actor root)] [children (view:children (view:snapshot copy))]
         [new-filter (cadar children)] [new-button (cadadr children)]
         [new-entry (cadr (assq 'entry (view:children (view:snapshot new-filter))))])
    (check 'control-fork-remaps-internal-command-target
      (cadar (descriptor:commands (view:snapshot new-button))) new-entry))
  (widget:mount! root 'control-fixture) (show!)
  (let ([focus (widget:focused root)])
    (check 'command-discovery-includes-descendants-and-fixed-arguments-without-invocation
      (list (widget:command-bindings root) (widget:focused root) (line))
      (list (list (list button '(button) 'action-text
                    (list (list 'activate entry 'insert '("new") entry:insert! #t)))) focus "original")))
  (entry:select! entry 8 0)
  (control:activate! button)
  (check 'control-direct-command-edits-real-source (line) "new")
  (entry:undo! entry)
  (check 'control-command-is-undoable (line) "original")
  (entry:select! entry 8 0) (show!)
  (let ([action (cadr (assoc '(click primary ()) (widget:pointer-bindings 2 1)))])
    (check 'control-mouse-binding-is-its-public-command-without-activation
      (list (keymap:call-action-procedure action) (keymap:call-action-arguments action) (line))
      (list control:activate! (list button) "original")))
  (widget:pointer! '(pointer press primary ()) 2 1)
  (widget:pointer! '(pointer release primary ()) 2 1)
  (widget:pointer! '(pointer release primary ()) 2 1)
  (check 'control-pointer-activates-only-once-after-owned-press (line) "new")
  (entry:undo! entry) (entry:select! entry 8 0) (show!)
  (widget:pointer! '(pointer press primary ()) 2 1)
  (widget:cancel! root 'test)
  (widget:pointer! '(pointer release primary ()) 2 1)
  (check 'control-cancel-prevents-activation (line) "original")
  (show!) (widget:pointer! '(pointer press primary ()) 2 1)
  (enable! #f) (widget:pump!)
  (check 'disabled-control-has-no-mouse-binding (widget:pointer-bindings 2 1) '())
  (enable! #t) (widget:pump!)
  (widget:pointer! '(pointer release primary ()) 2 1)
  (check 'control-disable-and-reenable-do-not-resurrect-a-held-press (line) "original")
  (let-values ([(text rev) (store:snapshot source)])
    (store:edit! '(agent "control") source rev (text:make-span 0 1 0 3) '("X"))
    (head:sync-foreign-edits! source))
  (let ([before (line)])
    (check 'control-stale-overlap-refuses-without-losing-text
      (list (refused? (lambda () (control:activate! button))) (line)) (list #t before)))
  ;; Rewiring to an absent action stays inspectable, but cannot execute.
  (rebind! 'absent)
  (check 'command-discovery-retains-unavailable-connections
    (list (widget:command-bindings button) (widget:commands button))
    (list (list (list button '() 'action-text
                  (list (list 'activate entry 'absent '("new") #f #f)))) '()))
  (rebind! 'insert)
  (check 'command-discovery-follows-rewiring
    (list-ref (car (cadddr (car (widget:command-bindings button)))) 4) entry:insert!)
  (widget:unmount! root) (widget:invalidate!))

;; Base resource retirement may race unpublished interaction. Reconcile the
;; acquired topology while retaining the surviving view's newer local state.
(let* ([actor head:ui-actor] [root (view:create! actor #f 'column 1 '() '())]
       [child (view:create! actor #f 'label 1 '((text . "Temporary")) '())])
  (view:arrange! actor (list (list root 0 (list (list 'child child 'fit)) '())) '())
  (widget:mount! root 'retired-control)
  (interaction:set-state! actor root #f 'newer)
  (view:retire! actor child (cdr (assq 'revision (model:snapshot child))))
  (interaction:flush!)
  (check 'revocation-mirror-preserves-surviving-provisional-interaction
    (list (interaction:snapshot child) (view:children (interaction:snapshot root))
      (view:state (view:snapshot root))) '(#f () newer))
  (widget:unmount! root))

;; Exact-revision proposals must refuse even endpoint edits, which ordinary
;; range rebasing intentionally accepts. Successful replacements remain undoable.
(let* ([actor head:ui-actor] [source (store:create! actor "entry proposal" '("abc"))]
       [id (view:create! actor (list 'buffer source) 'entry 1 '() '((0 . 3) (0 . 3)))])
  (widget:mount! id 'entry-proposal)
  (widget:prepare! id 20 1)
  (let-values ([(old revision) (store:snapshot source)])
    (store:edit! '(agent "entry") source revision (text:make-span 0 3 0 3) '("X"))
    (let-values ([(status reason) (store:edit! actor source revision (text:make-span 0 0 0 3) '("expanded")
                                    (list #f "Completion" (cons 'revision revision)))])
      (check 'entry-proposal-refuses-changed-endpoint (list status reason) '(stale revision-changed)))
    (head:sync-foreign-edits! source)
    (check 'entry-api-refuses-old-completion-revision
      (refused? (lambda () (entry:set-text! id "expanded" revision))) #t))
  (entry:set-text! id "new") (entry:undo! id)
  (let-values ([(text revision) (store:snapshot source)])
    (check 'entry-replacement-preserves-prior-history (vector-ref text 0) "abcX"))
  (widget:unmount! id))

;; No buffer/window is materialized for a field. A callback may close or
;; remount it after commit; the accepted edit cannot place an old caret in
;; that newer mount. Both routes reuse the same mirror and journal.
(check 'entry-commit-does-not-resurrect-a-closed-or-reclaimed-selection
  (map
    (lambda (remount?)
      (let* ([actor head:ui-actor]
             [source (store:create! actor "independent entry" '("abc") '((internal . #t)))]
             [id (view:create! actor (list 'buffer source) 'entry 1 '() '((0 . 3) (0 . 3)))]
             [fired? #f])
        (widget:mount! id 'independent-entry)
        (let ([mirror (text-source:lookup source)]
              [token (store:subscribe! source
                       (lambda (event)
                         (when (and (not fired?) (eq? (car event) 'edit))
                           (set! fired? #t) (widget:unmount! id)
                           (when remount? (widget:mount! id 'independent-entry) (entry:select! id 0 0)))))])
          (dynamic-wind void
            (lambda ()
              (entry:insert! id "X")
              (list fired? (store:line source 0) (not (head:buffer-of-store-id source))
                (eq? mirror (text-source:lookup source))
                (if remount? (view:state (interaction:snapshot id))
                  (refused? (lambda () (entry:insert! id "lost"))))))
            (lambda () (store:unsubscribe! token) (when remount? (widget:unmount! id)))))))
    '(#f #t))
  '((#t "abcX" #t #t #t) (#t "abcX" #t #t ((0 . 0) (0 . 0)))))
