;; Portable terminal composition; the process belongs to the base.
(import (only (foundation edoc) elibrary))
(elibrary (state terminal-state)
  (export create! state)
  (import (chezscheme) (prefix (core editor-schema) editor-schema:) (prefix (state connection) connection:)
    (prefix (state model) model:) (prefix (state view) view:))

  (edoc "Read a terminal view's capture preference and follow state."
        (d list "terminal descriptor") (returns list))
  (define (state d)
    (let ([s (view:state d)])
      (unless (and (list? s) (= (length s) 2) (memq (car s) '(partial full)) (boolean? (cadr s)))
        (error 'terminal "expected (partial-or-full following?)" s)) s))

  (edoc "Create an unmounted terminal view with a read-only editor child over an existing process document. Creating or forking views never spawns a process."
        (actor actor "creator") (owner (or model #f) "lifetime owner, false for a session root") (document buffer "terminal document") (returns model))
  (define (create! actor owner document)
    (let ([id (view:create! actor document 'terminal 1 '() '(partial #t) owner)])
      (guard (ex [else (let ([r (model:snapshot id)])
                         (when r (view:retire! actor id (cdr (assq 'revision r))))) (raise ex)])
        (let* ([d (editor-schema:make document '((read-only . #t) (wrap . #f)))]
               [child (view:create! actor document (view:kind d) (view:schema d) (view:options d) (view:state d) id)])
          (let-values ([(status rows) (view:arrange! actor (list (list id 0 (list (list 'text child '(grow 1))) '())) '())])
            (unless (eq? status 'applied) (error 'create! "cannot compose terminal" status)))
          (let-values ([(status details) (connection:bind! actor id (list (list child 'follow #f (list id 'following))))])
            (unless (eq? status 'applied) (error 'create! "cannot connect terminal viewport" status))) id))))
)
