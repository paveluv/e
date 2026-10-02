;; The default head is an ordinary composition. Alternative --start scripts
;; choose their own definitions, configuration policy, profile and root.
(for-each
  (lambda (failure) (raise (cdr failure)))
  (kernel:load-modules!
    '("bindings" "blame" "buffet" "c-mode" "completion" "configuration" "construction" "control" "delta-log"
      "describe" "edit" "entry" "environment" "eval" "extension" "finder"
      "git-view" "head" "history" "history-view" "keymap" "layout" "lifecycle" "log-view"
      "markdown" "md-mode" "message" "modal" "mode" "model" "namespace" "paren"
      "pretty-scheme" "prompt" "range" "region" "render" "routing"
      "scheme-format" "conflict-review" "conflict-source" "review-preview"
      "rewrite" "rewrite-source" "scheme-mode" "screen" "search" "store" "style"
      "split-control" "table" "terminal" "text-source" "view" "widget" "window-control")))
(configuration:load!)

(list
  (cons 'profile "editor")
  (cons 'entry
    (lambda (context)
      (let ([saved (cdr (assq 'saved context))])
        (if saved (cdr (assq 'root (cdr (assq 'value saved))))
          (construction:call! (cdr (assq 'head context))
            (lambda (remember!)
              (let* ([actor (cdr (assq 'head context))]
                     [root (remember! (view:create! actor #f 'screen 1 '() '()))])
                (let* ([area (view:create! actor #f 'split 1 '((axis . y)) '(3 1) root)]
                       [restored (window:restore-manager! area)]
                       [manager (or restored (window:create-manager! area))]
                       [auxiliary (window:create-manager! area)]
                       [auxiliary-record (caddar (cadr (model:snapshots (list auxiliary))))]
                       [auxiliary-view (cdr (assq 'value auxiliary-record))]
                       [messages (message:create! root)]
                       [prompts (modal:create! root (list (list root 'screen 'global)))]
                       [scratch (and (not restored) (or (store:find-named "*scratch*") (store:create! actor "*scratch*" '(""))))])
                  (view:arrange! actor
                    (list (list area 0 (list (list 'windows manager '(grow 1)))
                            (list '(axis . y) (list 'owned auxiliary)))
                      (list auxiliary (cdr (assq 'revision auxiliary-record)) (view:children auxiliary-view)
                        (cons (list 'commands (list 'dismiss root 'hide-auxiliary '()))
                          (view:options auxiliary-view)))
                      (list root 0
                        (list (list 'content area '(grow 1)) (list 'prompts prompts 'fit) (list 'messages messages 'fit))
                        (list (list 'commands (list 'open-file root 'open-file '())
                                (list 'open-document root 'open-document '())
                                (list 'auxiliary root 'auxiliary '())
                                (list 'hide-auxiliary root 'hide-auxiliary '())
                                (list 'quit root 'quit '()) (list 'review root 'review '())
                                (list 'prompt prompts 'prepare '()) (list 'message messages 'show '())
                                (list 'notification messages 'present '()))))) '())
                  (when scratch (window:open-document! manager (window:current manager) scratch))
                  (window:select! manager (window:current manager))
                  root)))))))))
