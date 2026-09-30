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
  (prompt-request:close! owner request)
  (check 'prompt-close-forgets-identity-and-owned-text
    (list (model:snapshot request) (store:exists? source)
      (prompt-request:accept! owner request 0 0)) '(#f #f unavailable))
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
