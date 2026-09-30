;; Base ownership and publication fences, within the existing bindings process.
(let ()
  (import (prefix (service inspection) inspection:) (prefix (state model) model:))
  (define owner '(head "inspection-fixture"))
  (define rows '(((keys test) "Keys" () "" "" ()) ((key test) #f ("C-k") "(kill! target)" "Kill the selected document." ((7 . 13)))))
  (define id (begin (actor:register! owner void) (inspection:create! owner)))
  (define (revision) (cdr (assq 'revision (model:snapshot id))))
  (define (publish actor rev) (inspection:publish! actor id rev '(view 42) 1 rows #f))
  (check 'hidden-and-foreign-inspection-publications-do-not-write
    (list (publish owner 0) (publish '(head "other") 0) (revision)) '(hidden unavailable 0))
  (let ([token (model:subscribe! (list id) void)])
    (check 'inspection-publication-is-revision-fenced-and-noop-aware
      (list (publish owner 0) (publish owner 0) (publish owner 1) (revision)) '(applied stale unchanged 1))
    (check 'inspection-rejects-oversize-and-invalid-symbolic-spans
      (list (test:raises? (lambda () (inspection:publish! owner id 1 #f 1 (make-list 2049 (car rows)) #f)))
        (test:raises? (lambda () (inspection:publish! owner id 1 #f 1 '((x #f () "x" "" ((0 . 2)))) #f))) (revision)) '(#t #t 1))
    (actor:detach! owner) (actor:register! owner void)
    (check 'a-reattachment-cannot-publish-as-the-lost-attachment
      (list (cdr (assq 'status (cdr (assq 'value (model:snapshot id))))) (publish owner (revision))) '(unavailable unavailable))
    (model:unsubscribe! token))
  (inspection:close! owner id) (actor:detach! owner))
