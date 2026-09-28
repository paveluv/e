;; Composition exercises real entry actions, command references and captures.
(let* ([actor head:ui-actor] [source (store:create! actor "control text" '("original"))]
       [filter (control:create-filter! actor (list 'buffer source) "Filter:" "[ready]")]
       [entry (cadr (assq 'entry (view:children (view:snapshot filter))))]
       [button (view:create! actor #f 'action-text 1
                 (list '(text . "Replace") '(enabled . #t) (list 'commands (list 'activate entry 'insert '("new")))) '())]
       [root (view:create! actor #f 'column 1 '() '())])
  (define (line) (vector-ref (head:buffer-lines (head:buffer-of-store-id source)) 0))
  (define (show!) (let ([f (widget:prepare! root 40 3)]) (widget:present! (list (list f 0 0))) f))
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
  (widget:unmount! root) (widget:invalidate!))
