;; Request/domain invariants share the existing store process.
(let ()
  (define owner '(head "requests"))
  (define other '(head "other-requests"))
  (define (get r k) (cdr (assq k r)))
  (define (value id) (get (model:snapshot id) 'value))
  (define (draft id) (cadr (get (value id) 'draft)))
  (define (new actor parent text)
    (prompt-request:create! actor parent #f text '((origin model 999)) '(head-symbols 1)))
  (define request (new owner #f "alpha"))
  (define source (draft request))
  (define borrowed (store:create! owner "persistent-repl-draft" '("keep")))
  (define independent (prompt-request:create! other #f (list 'buffer borrowed) "" '() #f))
  (check 'prompt-drafts-are-authored-but-not-catalogue-or-recovery-documents
    (list (store:line source 0) (store:property source 'internal)
      (store:property source 'audience)
      (let-values ([(next states) (store:export)])
        (and (not (assv source states)) (assv borrowed states) #t))
      (let-values ([(next states) (model:export)])
        (not (exists (lambda (r) (member (get r 'id) (list request independent))) states))))
    '("alpha" #t ((head "requests")) #t #t))
  (let-values ([(status info) (store:edit! owner source 0 (text:make-span 0 5 0 5) '("!"))]) (void))
  (check 'prompt-acceptance-is-owned-reviewed-and-exactly-once
    (list (prompt-request:accept! other request 0 1)
      (prompt-request:accept! owner request 1 1)
      (prompt-request:accept! owner request 0 0)
      (prompt-request:accept! owner request 0 1)
      (prompt-request:accept! owner request 0 1)
      (prompt-request:cancel! owner request)
      (get (value request) 'outcome))
    '(unavailable stale stale applied closed #f (1 #("alpha!") ((origin model 999)))))
  (store:reset! owner source '("later"))
  (check 'prompt-accepted-text-is-a-snapshot
    (get (value request) 'outcome) '(1 #("alpha!") ((origin model 999))))
  (let* ([host (view:create! owner #f 'column 1 '() '())]
         [target (view:create! owner (list 'buffer borrowed) 'entry 1 '() '((0 . 0) (0 . 0)))]
         [root (view:create! owner request 'column 1 '() '() request)]
         [child (view:create! owner (list 'buffer source) 'entry 1
                  (list (list 'commands (list 'accepted target 'insert '()))) '((0 . 0) (0 . 0)) root)])
    (view:arrange! owner (list (list root 0 (list (list 'entry child '(grow 1))) '())) '())
    (let* ([copy (view:fork! owner root)] [copied-child (cadar (view:children (view:snapshot copy)))]
           [owned (list root child copy copied-child)])
      (check 'prompt-scoped-views-and-forks-retain-transient-resource-ownership
        (list (map (lambda (id) (get (model:snapshot id) 'persistence)) owned)
          (map (lambda (id) (get (model:snapshot id) 'scope)) owned)
          (let-values ([(next states) (model:export)])
            (not (exists (lambda (r) (member (get r 'id) owned)) states))))
        (list '(transient transient transient transient) (list request root request copy) #t))
      (view:arrange! owner
        (list (list root 1 (list (list 'entry child '(grow 1)) (list 'borrowed target 'fit)) '())
          (list host 0 (list (list 'prompt root '(grow 1))) '())) '())
      (view:claim! owner host)
      (view:publish! owner (list (list host 1 1 #f '() target)))
      (check 'view-retirement-refuses-a-stale-guard-without-unlinking
        (list (car (call-with-values (lambda () (view:retire! owner root 0)) list))
          (view:parent (view:snapshot root)) (view:owner (view:snapshot target)))
        (list 'stale host owner))
      (prompt-request:close! owner request)
      (check 'prompt-close-releases-scoped-views-but-keeps-borrowed-targets
        (list (map model:snapshot owned) (and (view:snapshot target) #t)
          (test:raises? (lambda () (view:create! owner #f 'label 1 '() '() request))))
        '((#f #f #f #f) #t #t))
      (check 'prompt-retirement-unlinks-the-live-host-and-releases-borrowed-children
        (list (view:children (view:snapshot host)) (view:focus (view:snapshot host))
          (view:parent (view:snapshot target)) (view:owner (view:snapshot target))
          (car (call-with-values (lambda () (view:claim! other target)) list)))
        '(() #f #f #f applied)))
    (view:retire! owner target (get (model:snapshot target) 'revision))
    (view:retire! owner host (get (model:snapshot host) 'revision)))
  (check 'prompt-close-forgets-identity-and-owned-text
    (list (model:snapshot request) (store:exists? source)
      (prompt-request:accept! owner request 0 0)) '(#f #f unavailable))
  (let* ([request (new owner #f "controller")]
         [controller (view:create! owner request 'prompt 1 '() '() request)]
         [wrong (view:create! owner request 'prompt 1 '() '())])
    (check 'prompt-controller-binding-is-owned-scoped-reviewed-and-once-only
      (list (prompt-request:bind! other request 0 controller)
        (prompt-request:bind! owner request 0 wrong)
        (prompt-request:bind! owner request 1 controller)
        (prompt-request:bind! owner request 0 controller)
        (prompt-request:bind! owner request 1 controller)
        (get (value request) 'controller))
      (list 'unavailable 'unavailable 'stale 'applied 'bound controller))
    (prompt-request:close! owner request)
    (view:retire! owner wrong 0))
  (let* ([parent (new owner #f "outer")] [child (new owner parent "inner")]
         [grandchild (new owner child "last")] [sources (map draft (list parent child grandchild))])
    (check 'prompt-nesting-requires-live-same-owner-parent
      (list (new other parent "wrong owner") (prompt-request:cancel! other parent)
        (prompt-request:cancel! owner parent)
        (map (lambda (id) (get (value id) 'status)) (list parent child grandchild))
        (new owner parent "too late") (prompt-request:accept! owner child 0 0)
        (get (value independent) 'status))
      '(#f #f #t (cancelled cancelled cancelled) #f closed editing))
    (prompt-request:close! owner parent)
    (check 'prompt-close-releases-the-owned-tree
      (list (map model:snapshot (list parent child grandchild)) (map store:exists? sources))
      '((#f #f #f) (#f #f #f))))
  ;; Closing a parent after its read but before a child's allocation must
  ;; fail the unchanged ancestor witness and release the new draft.
  (let* ([parent (new owner #f "outer")] [before (list-sort < (store:buffer-list))]
         [watch (store:subscribe! #f
                  (lambda (event) (when (eq? (car event) 'create) (prompt-request:cancel! owner parent))))])
    (check 'prompt-parent-cancellation-fences-child-creation
      (list (new owner parent "too late") (list-sort < (store:buffer-list))) (list #f before))
    (store:unsubscribe! watch)
    (prompt-request:close! owner parent))
  (let* ([parent (new owner #f "outer\n")] [child (new owner parent "inner")])
    (check 'prompt-parent-acceptance-cancels-nested-input
      (list (prompt-request:accept! owner parent 0 0) (get (value child) 'status) (get (value parent) 'outcome))
      '(applied cancelled (0 #("outer" "") ((origin model 999)))))
    (prompt-request:close-owner! owner)
    (check 'prompt-departure-is-owner-scoped
      (list (model:snapshot parent) (model:snapshot child) (get (value independent) 'status)) '(#f #f editing)))
  (prompt-request:close-owner! other)
  (check 'prompt-borrowed-draft-survives-departure
    (list (model:snapshot independent) (store:line borrowed 0)) '(#f "keep"))
  (store:delete! owner borrowed))
