;; The default head is an ordinary composition. Alternative --start scripts
;; choose their own definitions, configuration policy, profile and root.
(for-each
  (lambda (failure) (raise (cdr failure)))
  (kernel:load-modules!
    '("bindings" "blame" "buffet" "c-mode" "completion" "control" "delta-log"
      "describe" "edit" "entry" "environment" "eval" "extension" "finder"
      "git-view" "head" "history" "history-view" "keymap" "layout" "log-view"
      "markdown" "md-mode" "message" "modal" "mode" "model" "namespace" "paren"
      "pretty-scheme" "prompt" "range" "region" "render" "routing"
      "scheme-format" "conflict-review" "conflict-source" "review-preview"
      "rewrite" "rewrite-source" "scheme-mode" "screen" "search" "store" "style"
      "split-control" "table" "terminal" "text-source" "view" "widget" "window-control")))
(main:load-config!)

(list
  (cons 'profile "editor")
  (cons 'entry
    (lambda (context)
      (let ([saved (cdr (assq 'saved context))])
        (if saved (cdr (assq 'root (cdr (assq 'value saved))))
          (let* ([actor (cdr (assq 'head context))]
                 [root (view:create! actor #f 'screen 1 '() '())])
            (guard (ex [else (view:retire! actor root (cdr (assq 'revision (caddar (cadr (model:snapshots (list root))))))) (raise ex)])
              (let* ([area (view:create! actor #f 'split 1 '((axis . y)) '(3 1) root)]
                     [manager (window:create-manager! area)]
                     [auxiliary (window:create-manager! area)]
                     [auxiliary-record (caddar (cadr (model:snapshots (list auxiliary))))]
                     [auxiliary-view (cdr (assq 'value auxiliary-record))]
                     [messages (message:create! root)]
                     [prompts (modal:create! root '(screen global))]
                     [scratch (or (store:find-named "*scratch*") (store:create! actor "*scratch*" '("")))])
                (view:arrange! actor
                  (list (list area 0 (list (list 'windows manager '(grow 1)))
                          (list '(axis . y) (list 'owned auxiliary)))
                        (list auxiliary (cdr (assq 'revision auxiliary-record)) (view:children auxiliary-view)
                          (cons (list 'commands (list 'dismiss root 'hide-auxiliary '()))
                            (view:options auxiliary-view)))
                        (list root 0
                          (list (list 'content area '(grow 1)) (list 'prompts prompts 'fit) (list 'messages messages 'fit))
                          (list (list 'commands (list 'open-file root 'open-file '())
                                  (list 'auxiliary root 'auxiliary '())
                                  (list 'hide-auxiliary root 'hide-auxiliary '())
                                  (list 'prompt prompts 'prepare '()) (list 'message messages 'show '())
                                  (list 'notification messages 'present '()))))) '())
                (window:open-document! manager (window:current manager) scratch)
                root))))))))
