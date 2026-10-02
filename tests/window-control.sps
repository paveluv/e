;; Exercise actual composed windows and app commands in the existing head fixture.
(let ()
  (import (prefix (service window) window:) (prefix (head window-control) window-control:)
          (prefix (apps buffet) buffet:) (prefix (apps finder) finder:)
          (prefix (apps bindings) bindings:) (prefix (apps delta-log) delta-log:)
          (prefix (apps markdown) markdown:)
          (prefix (foundation edoc) edoc:))
  (let ([before (list (model:ids) (seat:buffers) (seat:windows))])
    (kernel:load-module! "window-control")
    (check 'window-controls-load-without-allocating-an-editor-host
      (list (model:ids) (seat:buffers) (seat:windows)) before))
  (actor:call-as head:ui-actor
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)]
             [second (window:split! manager first 'right)] [third (window:split! manager second 'below)]
             [a (store:create! head:ui-actor "composed a" '("First"))]
             [b (store:create! head:ui-actor "composed b" '("Second"))]
             [opened (store:create! head:ui-actor "composed opened" '("Opened"))]
             [app (view:create! head:ui-actor #f 'column 1
                    (list '(name . "<opener>") '(catalogue . #t) (cons 'commands (window-control:commands second))) '() second)]
             [button (view:create! head:ui-actor #f 'action-text 1
                       (list '(text . "Open") '(enabled . #t) (list 'commands (list 'activate second 'open-document (list opened)))) '() app)])
        (define (frame id f)
          (if (equal? id (widget:frame-id f)) f (exists (lambda (f) (frame id f)) (widget:frame-children f))))
        (define (show!)
          (widget:pump!)
          (let ([f (widget:prepare! manager 40 6)]) (widget:present! (list (list f 0 0))) f))
        (define (click phase) (widget:pointer! (list 'pointer phase 'primary '()) 21 0))
        (view:arrange! head:ui-actor
          (list (list app (model:revision app) (list (list 'button button 'fit)) (view:options (view:snapshot app)))) '())
        (widget:mount! manager 'composed-windows)
        (let ([f (show!)] [ids (model:ids)])
          (check 'window-container-geometry-and-empty-focus-use-the-canonical-tree
            (list (map (lambda (id) (widget:frame-rect (frame id f))) (list first second third))
              (widget:focused manager)
              (begin (widget:prepare! manager 0 0) (model:ids)))
            (list '((0 0 20 6) (21 0 19 3) (21 3 19 3)) first ids)))
        (window:open-document! manager first a) (window:open-document! manager second b)
        (window:open-document! manager second app) (show!)
        (window:select! manager second) (show!)
        (check 'window-selection-enters-its-own-app-without-extra-container-tab-stops
          (list (widget:focused manager)
            (begin (widget:focus! manager (widget:descendant first 'document))
              (widget:pointer! '(pointer press primary ()) 21 1) (widget:focused manager))
            (widget:focus-next! manager))
          (list button (widget:descendant first 'document) button))
        (widget:focus! manager (widget:descendant first 'document)) (show!)
        (let ([before (map model:snapshot (list second app button))])
          (click 'press) (show!) (click 'release) (show!)
          (check 'window-inactive-panel-release-opens-in-previous-window-and-keeps-panel-state
            (list (window:document manager first) (window:document manager second) (window:current manager)
              (map model:snapshot (list second app button)))
            (list opened app first before)))
        (edoc:expression (widget:invoke! app 'return)) (show!)
        (check 'window-app-return-binding-restores-origin-without-focusing-an-inactive-panel
          (list (window:document manager second) (window:current manager)) (list b first))
        (window:open-document! manager second app) (show!)
        ;; A later explicit focus choice must win over the press's saved origin.
        (click 'press) (widget:focus! manager third) (click 'release) (show!) (interaction:flush!)
        (check 'window-captured-release-cancels-after-focus-moves-elsewhere
          (list (window:document manager third) (window:current manager) (window:document manager second))
          (list #f third app))
        (window:select! manager second) (show!)
        (routing:input! manager '(key "RET" #f)) (show!)
        (check 'window-keyboard-command-opens-in-own-window-and-hidden-app-cannot-dispatch
          (list (window:document manager second) (window:current manager)
            (refused? (lambda () (edoc:expression (widget:invoke! app 'return))))) (list opened second #t))
        (window:open-document! manager second app) (show!)
        (let* ([copy (view:fork! head:ui-actor manager)] [window (window:numbered copy 2)]
               [copy-app (window:document copy window)])
          (check 'window-app-command-targets-follow-a-composition-copy
            (descriptor:commands (view:snapshot copy-app)) (window-control:commands window)))
        (let ([copy (view:fork! head:ui-actor app
                      (list (cons 'owner third) (list 'receivers (list second third))))])
          (window:open-document! manager third copy) (show!)
          (edoc:expression (widget:invoke! copy 'open opened)) (show!)
          (check 'app-copy-rebinds-external-window-commands-before-admission
            (list (window:document manager third) (window:document manager second)
              (descriptor:commands (view:snapshot copy)))
            (list opened app (window-control:commands third))))
        (let ([split (view:parent (interaction:snapshot second))])
          (show!)
          (widget:pointer! '(pointer press primary ()) 22 2)
          (widget:pointer! '(pointer move primary ()) 22 3) (show!)
          (widget:pointer! '(pointer move primary ()) 22 1) (show!)
          (widget:pointer! '(pointer release primary ()) 22 1)
          (check 'upper-status-drag-resizes-down-and-up-without-an-extra-separator-row
            (list (view:state (interaction:snapshot split))
              (widget:frame-rect (frame second (show!)))) '((2 4) (21 0 19 2)))
          (window-control:resize! split (map cadr (view:children (interaction:snapshot split))) '(1 1))
          (interaction:flush!) (show!))
        (let* ([split (view:parent (interaction:snapshot first))]
               [expected (map cadr (view:children (interaction:snapshot split)))]
               [before (model:snapshot split)])
          (show!)
          (widget:pointer! '(pointer press primary ()) 20 1)
          (widget:pointer! '(pointer move primary ()) 25 1) (show!)
          (widget:pointer! '(pointer move primary ()) 12 1)
          (let ([f (show!)])
            (check 'split-drag-is-immediate-in-both-directions-without-synchronous-publication
              (list (view:state (interaction:snapshot split)) (widget:frame-rect (frame first f))
                (equal? before (model:snapshot split))) '((12 27) (0 0 12 6) #t)))
          (widget:pointer! '(pointer release primary ()) 12 1) (interaction:flush!)
          (check 'split-drag-publishes-logical-proportions-without-replacing-children
            (list (view:state (view:snapshot split)) (map cadr (view:children (view:snapshot split))))
            (list '(12 27) expected))
          (show!) (widget:pointer! '(pointer press primary ()) 12 1)
          (window:close! manager third) (show!)
          (widget:pointer! '(pointer move primary ()) 30 1)
          (widget:pointer! '(pointer release primary ()) 30 1)
          (check 'changed-split-topology-cancels-captured-resize
            (list (view:state (interaction:snapshot split))
              (refused? (lambda () (window-control:resize! split expected '(1 1))))) '((12 27) #t)))
        (widget:unmount! manager) (widget:invalidate!)
        (let* ([commands (window-control:commands first)]
               [apps (list (buffet:create! first commands) (finder:create! first commands ".")
                       (bindings:create! first commands #f) (delta-log:create! first commands 'conflicts (list a))
                       (git-view:create! first ".") (log-view:create! first #f)
                       (markdown:create! head:ui-actor first a commands) (terminal:create-view! head:ui-actor first a))]
               [trees (map view:tree apps)]
               [ids (apply append (map (lambda (tree) (map car tree)) trees))])
          (for-each
            (lambda (app)
              (let ([d (view:snapshot app)])
                (view:arrange! head:ui-actor
                  (list (list app (model:revision app) (view:children d)
                          (append '((name . "<owned app>") (catalogue . #t)) (view:options d)))) '()))
              (window:open-document! manager first app)) apps)
          (let ([queries (map (lambda (tree)
                                (view:source (cdr (find (lambda (row) (eq? (view:kind (cdr row)) 'table)) tree)))) (list-head trees 2))])
            (window:close! manager first)
            (check 'window-owned-app-trees-retire-without-retiring-shared-queries
              (list (exists model:snapshot ids) (and (for-all model:snapshot queries) #t)) '(#f #t))))
        (widget:mount! manager 'status-buttons) (show!)
        (widget:pointer! '(pointer move none ()) 38 5)
        (let* ([f (show!)] [status (frame (widget:descendant second 'status) f)]
               [binding (assoc '(click primary ()) (widget:pointer-bindings 38 5))])
          (check 'status-buttons-use-inspectable-commands-and-shared-hover-style
            (list (eq? (keymap:call-action-procedure (cadr binding)) window-control:close!)
              (vector-ref (widget:frame-cell-styles status 0) 38)
              (substring (car (widget:frame-lines status)) 33 40))
            '(#t (status hover) "│↕│↔│×│")))
        (for-each (lambda (phase) (widget:pointer! (list 'pointer phase 'primary '()) 34 5)) '(press release))
        (show!)
        (check 'status-split-and-close-share-the-window-model-lifecycle
          (list (length (window:list manager))
            (begin
              (for-each (lambda (phase) (widget:pointer! (list 'pointer phase 'primary '()) 38 5)) '(press release))
              (show!) (window:list manager))
            (window-control:close! second)) (list 2 (list second) #f))
        (let* ([builds 0] [failed #f]
               [build (lambda (owner commands)
                        (set! builds (+ builds 1))
                        (view:create! head:ui-actor #f 'column 1 (list (cons 'commands commands)) '() owner))]
               [app (window-control:open-app! second "Builder" build)]
               [destination (begin (show!) (window-control:split! second 'right))])
          (show!)
          (let ([copy (window-control:open-app! destination "Builder" build)])
            (show!)
            (check 'named-app-reuse-and-cross-window-copy-have-one-constructor-and-explicit-hosts
              (list builds (equal? app copy) (window-control:open-app! second "Builder" build)
                (descriptor:commands (view:snapshot copy)) (window:find-app manager destination "Builder"))
              (list 1 #f app (window-control:commands destination) (list destination copy))))
          (let ([before (window:documents manager second)])
            (check 'failed-app-admission-cleans-only-the-new-window-owned-candidate
              (list (refused? (lambda ()
                                (window-control:open-app! second "Unlisted"
                                  (lambda (owner commands)
                                    (set! failed (view:create! head:ui-actor #f 'column 1 '((catalogue . #f)) '() owner)) failed))))
                (model:snapshot failed) (window:documents manager second)
                (window:find-app manager second "Unlisted"))
              (list #t #f before #f))))
        (widget:unmount! manager)))))
