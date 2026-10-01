;; Load this file in a head, then call (widget-example:open! '("a.sls" "b.ss")).
;; No filesystem walk or new authored-data store is hidden in the example.
(import (prefix (only (foundation edoc) expression) edoc:))
(edoc:expression
  (define (widget-example:pick! id selection basis)
    (let ([rank (collection:rank (car selection) (cadr selection) (caddr selection))])
      (unless (and (eq? (car rank) 'ready) (list-ref rank 3) (equal? (caddr rank) basis))
        (error 'widget-example:pick! "the selected result changed"))
      (widget:invoke! id 'insert (caddr selection)))))
(widget:register! 'example-file-choice 1
  (list (cons 'actions (list (cons 'pick widget-example:pick!)))))

(define (widget-example:open! filenames)
  (let* ([who head:ui-actor]
         [source (collection:create-source! who '((name "Filename" string))
                   (list->vector (map (lambda (name) (list name (list (cons 'name name)) '())) filenames)) 'persistent)]
         [needle (store:create! who "widget filter" '("") '((internal . #t)))]
         [query (collection:create! who source "" '() 'persistent (list source needle))]
         [answer (store:create! who "widget answer" '(""))]
         [filter (control:create-filter! who needle "Filter:" "")]
         [output (view:create! who answer 'entry 1 '() '((0 . 0) (0 . 0)))]
         [target (view:create! who #f 'example-file-choice 1
                   (list (list 'commands (list 'insert output 'insert '()))) '())]
         [table (table:create! who query '(name))]
         [undo (view:create! who #f 'action-text 1
                 (list '(text . "Undo insertion") '(enabled . #t) (list 'commands (list 'activate output 'undo '()))) '())]
         [root (view:create! who #f 'column 1 '() '())])
    (view:arrange! who
      ;; The filter belongs to the table's keyboard scope: its entry handles
      ;; text editing, while Up/Down and Return reach the table's commands.
      (list (list table 1 (cons (list 'filter filter 'fit) (view:children (view:snapshot table)))
              (list '(columns name) (list 'commands (list 'activate target 'pick '()))))
        (list root 0 (list (list 'table table '(grow 1))
                       (list 'answer output 'fit) (list 'undo undo 'fit) (list 'target target 'fit)) '())) '())
    (connection:bind! who query (list (list query 'filter #f (list needle 'text))))
    (window:show-widget! (seat:current-window) root)
    root))
