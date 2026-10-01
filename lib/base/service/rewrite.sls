;; Independent rewrite choices; the store retains the inversion algorithm.
(import (only (foundation edoc) elibrary))
(elibrary (service rewrite)
  (export close! create! preview settle! toggle!)
  (import (chezscheme) (prefix (core handle) handle:)
    (prefix (state model) model:) (prefix (state store) store:)
    (prefix (state view) view:))
  (define (get r k) (cdr (assq k r)))
  (define (revision? n) (and (integer? n) (exact? n) (> n 0)))
  (define kind (model:register-kind! 'rewrite-draft 1
                 (lambda (v) (and (list? v) (for-all pair? v) (equal? (map car v) '(document disabled))
                               (handle:buffer? (get v 'document)) (list? (get v 'disabled)) (for-all revision? (get v 'disabled))
                               (let loop ([xs (get v 'disabled)])
                                 (or (null? xs) (and (not (memv (car xs) (cdr xs))) (loop (cdr xs)))))))))
  (define lock (make-mutex))
  (define (record id)
    (let ([r (model:snapshot id)])
      (unless (and r (eq? (get r 'kind) 'rewrite-draft)) (error 'rewrite "draft is unavailable" id)) r))
  (define (current id revision)
    (let ([r (record id)])
      (unless (equal? revision (get r 'revision)) (error 'rewrite "draft changed; review its current choices")) r))

  (edoc "Create an independent persistent rewrite draft over a borrowed document. Other drafts over the same document keep their own disabled revisions."
        (actor actor "creator") (document buffer "source document") (returns model))
  (define (create! actor document)
    (unless (store:visible? actor document) (error 'create! "document is unavailable"))
    (model:create! actor 'rewrite-draft 1 'session 'persistent (list document)
      (list (cons 'document document) '(disabled))))

  (edoc "Toggle retained revisions in an explicit draft against its model revision. Validate the complete choice before changing anything; source text is never edited. Return the updated model envelope."
        (actor actor "caller") (id model "rewrite draft") (revision integer "expected draft revision")
        (revisions (list-of integer) "entries to toggle") (returns list))
  (define (toggle! actor id revision revisions)
    (unless (and (list? revisions) (for-all revision? revisions)) (error 'toggle! "expected retained revisions"))
    (with-mutex lock
      (let* ([r (current id revision)] [v (get r 'value)]
             [disabled (fold-left (lambda (out n) (if (memv n out) (remv n out) (cons n out))) (get v 'disabled) revisions)])
        (call-with-values (lambda () (store:rewrite-preview (get v 'document) disabled)) (lambda values (void)))
        (let-values ([(status rows) (model:commit! actor
                                      (list (list id revision (get r 'references) (list (assq 'document v) (cons 'disabled disabled)))))])
          (unless (eq? status 'applied) (error 'toggle! "draft changed")) (record id)))))

  (edoc "Derive a rewrite draft without editing its source. Return (draft-revision document disabled text mapping conflicts source-revision); text and mapping share one store snapshot. Retired history refuses and leaves choices intact."
        (id model "rewrite draft") (returns list))
  (define (preview id)
    (let* ([r (record id)] [v (get r 'value)])
      (let-values ([(text mapping conflicts revision) (store:rewrite-preview (get v 'document) (get v 'disabled))])
        (list (get r 'revision) (get v 'document) (get v 'disabled) text mapping conflicts revision))))

  (edoc "Settle the reviewed draft using the store's overlap-checked, undoable inversion. Return status and detail. Failure keeps choices; success clears them. Source edits elsewhere survive."
        (actor actor "caller") (id model "rewrite draft") (revision integer "expected draft revision") (returns any))
  (define (settle! actor id revision)
    (with-mutex lock
      (let* ([r (current id revision)] [v (get r 'value)])
        (let-values ([(status detail) (store:rewrite! actor (get v 'document) (get v 'disabled) 'any)])
          (when (eq? status 'applied)
            (model:commit! actor (list (list id revision (get r 'references) (list (assq 'document v) '(disabled))))))
          (values status detail)))))

  (edoc "Retire a rewrite draft at its model revision and revoke its scoped views. The borrowed document and its undo history survive."
        (actor actor "caller") (id model "rewrite draft") (revision integer "expected draft revision"))
  (define (close! actor id revision)
    (with-mutex lock
      (current id revision)
      (let-values ([(status r) (model:retire! actor id revision)])
        (when (eq? status 'applied) (view:retire-scope! actor id)) status))))
