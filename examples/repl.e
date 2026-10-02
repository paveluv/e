;; ./e --start examples/repl.e
;; Scheme runs in an explicit base environment. M-x runs head commands.
;; To share definitions with another head, pass this root's environment to
;; repl:create! there and install the returned root with root:install!.
(for-each (lambda (failure) (raise (cdr failure)))
  (kernel:load-modules!
    '("construction" "control" "edit" "entry" "environment" "eval" "head" "history" "history-view"
      "keymap" "layout" "message" "modal" "model" "prompt" "root" "store" "view" "widget")))
(import (prefix (only (foundation edoc) expression) edoc:)
        (prefix (foundation string) string:) (prefix (head interaction) interaction:)
        (prefix (service prompt-request) prompt-request:))

(define (repl:get r key) (cdr (assq key r)))
(define (repl:record id) (caddar (cadr (model:snapshots (list id)))))

(edoc:expression
  (define (repl:renew! id . ignored)
    (interaction:flush!)
    (let* ([r (repl:record id)] [d (repl:get r 'value)]
           [env (view:source d)] [generation (repl:get (repl:get (repl:record env) 'value) 'generation)]
           [old (assq 'prompt (view:children d))])
      (construction:call! head:ui-actor
        (lambda (remember!)
          (let* ([prompt (eval:create-model-prompt! env generation (repl:get (view:options d) 'draft)
                           (list (list 'accepted id 'submit '()) (list 'cancelled id 'renew '())))]
                 [request (view:source (repl:get (repl:record prompt) 'value))])
            (remember! request (lambda () (prompt-request:close! head:ui-actor request)))
            (let-values ([(status rows)
                          (widget:arrange!
                            (list (list id (repl:get r 'revision)
                                    (append (remp (lambda (c) (eq? (car c) 'prompt)) (view:children d))
                                      (list (list 'prompt prompt 'fit))) (view:options d))))])
              (unless (eq? status 'applied) (error 'repl:renew! "prompt placement changed" status))
              (interaction:focus! id (widget:descendant prompt 'input 'entry))))))
      ;; Closing a superseded request preserves its borrowed authored draft.
      (when old
        (let ([r (repl:record (cadr old))])
          (when r (prompt-request:close! head:ui-actor (view:source (repl:get r 'value)))))))))

(edoc:expression
  (define (repl:submit! id outcome)
    (let* ([origin (caddr outcome)]
           [job (environment:evaluate! head:ui-actor (repl:get origin 'environment)
                  (repl:get origin 'generation) (string:join (vector->list (cadr outcome)) "\n"))]
           [history (repl:get (view:options (interaction:snapshot id)) 'history)])
      ;; Retry only appending the already accepted job; never evaluate twice.
      (let loop ()
        (unless (history:append! head:ui-actor history (repl:get (repl:record history) 'revision)
                  (list 'result 1 job) #f (list job)) (loop)))
      (repl:renew! id))))

(define (repl:service! id frame)
  (let* ([d (interaction:snapshot id)] [p (assq 'prompt (view:children d))]
         [prompt (and p (interaction:snapshot (cadr p)))]
         [request (and prompt (model:snapshot (view:source prompt)))])
    ;; Transient requests disappear on detach/restart; authored drafts remain.
    ;; Preparation may acquire demand for an unowned candidate. Create the
    ;; transient interaction only once admission has given this head ownership.
    (when (and (widget:mounted? id) (not request)) (repl:renew! id))))

(edoc:expression
  (define (repl:edit! id)
    ;; This is explicitly a head action. The base evaluator has no UI imports.
    (kernel:load-module! "window-control")
    (kernel:evaluate!
      `(construction:call! head:ui-actor
         (lambda (remember!)
           (let* ([draft (repl:get (view:options (interaction:snapshot ',id)) 'draft)]
                  [manager (remember! (window:create-manager! #f))])
             (store:set-property! head:ui-actor draft 'internal #f)
             (window:open-document! manager (window:current manager) draft)
             (let-values ([(status binding) (root:install! (root:current) manager 'retire)])
               (unless (eq? status 'applied) (error 'repl:edit! "replacement refused" status))))))
      (interaction-environment))))

(widget:register! 'example-repl 1
  (append (layout:container 'y)
    (list '(source-receiver . environment) '(contexts . (example-repl))
      (cons 'service repl:service!)
      (cons 'actions (list (cons 'submit repl:submit!) (cons 'renew repl:renew!) (cons 'edit repl:edit!))))))
(keymap:bind-default! 'example-repl "M-x" (keymap:call eval:prompt!))
(keymap:bind-default! 'example-repl "C-x C-c" (keymap:call head:quit!))

(define (repl:create! environment)
  (construction:call! head:ui-actor
    (lambda (remember!)
      (let* ([who head:ui-actor]
             [draft (remember! (store:create! who "REPL draft" '("(+ 20 22)")
                                 (list '(internal . #t) (cons 'audience (list who)))))]
             [history (remember! (history:create! who 'persistent))]
             [root (remember! (view:create! who environment 'example-repl 1 '() '()))]
             [page (history-view:create! root history 3)]
             [messages (message:create! root)]
             [prompts (modal:create! root (list (list root 'example-repl)))]
             [tools (view:create! who #f 'row 1 '((spacing . normal)) '() root)]
             [command (view:create! who #f 'label 1
                        '((text . "M-x: head command; C-x C-c: detach")) '() root)]
             [edit (view:create! who #f 'action-text 1
                     (list '(text . "Edit draft in windows") '(enabled . #t)
                       (list 'commands (list 'activate root 'edit '()))) '() root)])
        (view:arrange! who
          (list (list tools 0 (list (list 'command command 'fit) (list 'editor edit 'fit)) '((spacing . normal)))
            (list root 0
              (list (list 'history page '(grow 1)) (list 'tools tools 'fit)
                (list 'messages messages 'fit) (list 'modals prompts 'fit))
              (list (cons 'draft draft) (cons 'history history)
                (list 'commands (list 'prompt prompts 'prepare '()) (list 'message messages 'show '())
                  (list 'notification messages 'present '()))))) '())
        root))))

(list (cons 'profile "repl")
  (cons 'entry
    (lambda (context)
      (let ([saved (repl:get context 'saved)])
        (if saved
          (let ([root (repl:get (repl:get saved 'value) 'root)])
            (when (and root (eq? (view:kind (repl:get (repl:record root) 'value)) 'window-manager))
              (kernel:load-module! "window-control"))
            root)
          (construction:call! (repl:get context 'head)
            (lambda (remember!)
              (repl:create!
                (remember! (environment:create! (repl:get context 'head)
                             (list (cons 'directory (current-directory)) '(roots) '(imports (chezscheme)))
                             'persistent))))))))))
