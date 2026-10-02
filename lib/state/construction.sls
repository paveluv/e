;; Failed builders release only the resources they explicitly created.
(import (only (foundation edoc) elibrary))
(elibrary (state construction)
  (export call!)
  (import (chezscheme)
          (prefix (core handle) handle:)
          (prefix (core kernel) kernel:)
          (prefix (state journal) journal:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (state view) view:))
  (define collector (make-thread-parameter #f))
  (define (retire! actor id)
    (if (handle:buffer? id) (when (store:exists? id) (store:delete! actor id))
      (let retry ()
        (let ([r (caddar (cadr (model:snapshots (list id))))])
          (when r
            (let-values ([(status ignored)
                          ((if (eq? (cdr (assq 'kind r)) 'widget-view) view:retire! model:retire!)
                           actor id (cdr (assq 'revision r)))])
              (case status [(stale) (retry)] [(applied) (void)]
                [else (error 'retire! "construction cleanup refused" id status)])))))))

  (edoc "Build resources under an explicit failure boundary. The procedure receives remember!, which returns a model or buffer and registers its retirement if the builder raises; an optional cleanup thunk replaces ordinary retirement. Register only newly owned resources, never borrowed ones. Successful nested builders transfer cleanup to their caller until the outer builder returns. Finish the boundary at successful admission; this does not roll back edits or admission."
        (actor actor "resource owner") (build procedure "(remember!) -> values")
        (returns any "builder results"))
  (define (call! actor build)
    (let ([parent (collector)] [cleanups '()])
      (define (collect! entries) (set! cleanups (append entries cleanups)))
      (define remember!
        (case-lambda
          [(id) (remember! id (lambda () (retire! actor id)))]
          [(id cleanup) (collect! (list cleanup)) id]))
      (guard (ex [else
                  (for-each (lambda (cleanup)
                              (guard (failure [else (guard (unavailable [else (void)])
                                                      (journal:add! 'construction:call! (kernel:condition-text failure)))])
                                (cleanup))) cleanups)
                  (raise ex)])
        (let ([results (parameterize ([collector collect!]) (call-with-values (lambda () (build remember!)) list))])
          (when parent (parent cleanups))
          (apply values results))))))
