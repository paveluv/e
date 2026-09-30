(let ()
  (import (prefix (service rewrite) rewrite:) (prefix (service rewrite-source) rewrite-source:)
          (prefix (service review-preview) review-preview:) (prefix (state collection) collection:) (prefix (state model) model:))
  (define (get r k) (cdr (assq k r)))
  (define actor head:ui-actor)
  (define document (store:create! actor "independent rewrites" '("base")))
  (define (lines) (let-values ([(lines revision) (store:snapshot document)]) (vector->list lines)))
  (define (disabled id) (get (get (model:snapshot id) 'value) 'disabled))
  (define (revision id) (model:revision id))
  (store:edit! actor document 0 (text:make-span 0 0 0 0) '("A"))
  (store:edit! bot document 1 (text:make-span 0 5 0 5) '("B"))
  (let ([a (rewrite:create! actor document)] [b (rewrite:create! actor document)])
    (rewrite:toggle! actor a 0 '(1)) (rewrite:toggle! actor b 0 '(2))
    (check 'rewrite-drafts-keep-independent-choices-and-coherent-bases
      (map (lambda (id) (let ([p (rewrite:preview id)]) (list (caddr p) (vector->list (list-ref p 3)) (list-ref p 6)))) (list a b))
      '(((1) ("baseB") 2) ((2) ("Abase") 2)))
    (let* ([preview (review-preview:create! actor a)] [request (car preview)] [output (cadr preview)]
           [token (model:subscribe! (list request) (lambda (notice) (void)))])
      (test:await 'rewrite-publication (lambda () (equal? (store:line output 0) "baseB")))
      (store:set-property! actor document 'mode "scheme")
      (test:await 'rewrite-mode (lambda () (equal? (store:property output 'mode #f) "scheme")))
      (check 'rewrite-preview-publishes-read-only-text-and-follows-source-mode
        (list (store:property output 'read-only) (store:line output 0) (lines)) '(#t "baseB" ("AbaseB")))
      (model:unsubscribe! token)
      (review-preview:close! actor request))
    (check 'rewrite-invalid-or-stale-toggles-are-atomic
      (list (test:raises? (lambda () (rewrite:toggle! actor a 0 '(2))))
        (test:raises? (lambda () (rewrite:toggle! actor a 1 '(2 999)))) (disabled a)) '(#t #t (1)))
    (store:edit! bot document 2 (text:make-span 0 6 0 6) '("!"))
    (store:set-property! actor document 'read-only #t)
    (check 'rewrite-settlement-respects-source-read-only
      (list (call-with-values (lambda () (rewrite:settle! actor a 1)) list) (disabled a) (lines))
      '((refused read-only) (1) ("AbaseB!")))
    (store:set-property! actor document 'read-only #f)
    (check 'rewrite-settlement-preserves-unrelated-edits-and-the-other-draft
      (list (car (call-with-values (lambda () (rewrite:settle! actor a 1)) list)) (lines) (disabled a) (disabled b))
      '(applied ("baseB!") () (2)))
    (store:undo! actor document)
    (check 'rewrite-settlement-is-one-undo-step (lines) '("AbaseB!"))
    (let* ([query (collection:create! actor b "" '() 'persistent)]
           [token (model:subscribe! (list query) (lambda (notice) (void)))])
      (define (meta) (get (collection:summary query) 'value))
      (define (ready) (eq? (get (meta) 'status) 'ready))
      (test:await 'rewrite-history ready)
      (let* ([v (meta)] [generation (get v 'generation)] [basis (get v 'basis)]
             [selection (list query generation (list document 2))])
        (rewrite-source:toggle! actor selection basis)
        (check 'history-selection-toggles-only-its-draft-and-refuses-stale-results
          (list (disabled b) (disabled a) (test:raises? (lambda () (rewrite-source:toggle! actor selection basis)))) '(() () #t)))
      (test:await 'rewrite-history-refresh ready)
      (let ([v (meta)])
        (store:edit! bot document (store:revision document) (text:make-span 0 7 0 7) '("?"))
        (check 'history-source-change-refuses-stale-settlement
          (test:raises? (lambda () (rewrite-source:settle! actor query (get v 'generation) (get v 'basis)))) #t))
      (model:unsubscribe! token)
      (model:retire! actor query (model:revision query)))
    (for-each (lambda (id) (rewrite:close! actor id (revision id))) (list a b))
    (check 'closing-rewrite-drafts-keeps-the-borrowed-source
      (list (model:snapshot a) (model:snapshot b) (lines)) '(#f #f ("AbaseB!?")))))
