;; Load, then (environment-example:open! window). Each panel is a prompt/result
;; composition. The first two share definitions; the third is independent.
;; Jobs and authored drafts stay in the base. Unmounting a panel keeps them;
;; close its environment explicitly when finished with its jobs and handles.
(import (prefix (only (foundation edoc) expression) edoc:))
(define (environment-example:get r k) (cdr (assq k r)))
(edoc:expression
  (define (environment-example:accepted! id outcome)
    (let* ([origin (caddr outcome)]
           [env (environment-example:get origin 'environment)]
           [generation (environment-example:get origin 'generation)]
           [job (environment:evaluate! head:ui-actor env generation
                  (string:join (vector->list (cadr outcome)) "\n"))]
           [d (view:snapshot id)]
           [draft (environment-example:get (view:options d) 'draft)]
           [result (eval:create-result-view! head:ui-actor #f job)]
           [prompt (eval:create-model-prompt! env generation draft (list (list 'accepted id 'accepted '())))])
      (widget:arrange!
        (list (list id (environment-example:get (caddar (cadr (model:snapshots (list id)))) 'revision)
                (list (car (view:children d)) (list 'result result '(grow 1)) (list 'prompt prompt 'fit))
                (view:options d))))
      (widget:focus! id (widget:descendant prompt 'input 'entry)))))
(widget:register! 'environment-example 1
  (append (layout:container 'y)
    (list '(source-receiver . environment) (cons 'actions (list (cons 'accepted environment-example:accepted!))))))

(define (environment-example:panel! env title)
  (let* ([who head:ui-actor]
         [draft (store:create! who "worksheet draft" '("(+ seed 1)") '((internal . #t)))]
         [root (view:create! who env 'environment-example 1 (list (cons 'draft draft)) '())]
         [title (view:create! who #f 'label 1 (list (cons 'text title)) '())]
         [prompt (eval:create-model-prompt! env 1 draft (list (list 'accepted root 'accepted '())))])
    (view:arrange! who (list (list root 0 (list (list 'title title 'fit) (list 'prompt prompt '(grow 1)))
                               (list (cons 'draft draft)))) '()) root))

(define (environment-example:open! window)
  (window-control:open-app! window "environments"
    (lambda (owner commands)
      (let* ([recipe (list (cons 'directory (current-directory)) '(roots)
                       '(imports (chezscheme) (prefix (service resource) resource:)) '(values (seed . 10)))]
             [shared (environment:create! head:ui-actor recipe 'persistent)]
             [independent (environment:create! head:ui-actor recipe 'persistent)]
             [a (environment-example:panel! shared "Shared environment — A")]
             [b (environment-example:panel! shared "Shared environment — B")]
             [c (environment-example:panel! independent "Independent environment")]
             [root (view:create! head:ui-actor #f 'row 1 '() '() owner)])
        (view:arrange! head:ui-actor
          (list (list root 0 (list (list 'first a '(grow 1)) (list 'second b '(grow 1)) (list 'third c '(grow 1))) '())) '())
        root))))
