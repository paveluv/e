;; Exercise the actual shipped recipe's entry, without reloading fixture-owned
;; definitions or applying the user's configuration inside the test process.
(let ()
  (import (prefix (apps screen) screen:) (prefix (apps buffet) buffet:)
          (prefix (apps bindings) bindings:)
          (prefix (apps describe) describe:)
          (prefix (apps markdown) markdown:) (prefix (apps delta-log) delta-log:)
          (prefix (service vt) vt:) (prefix (head modal) modal:)
          (prefix (head message) message:) (prefix (service window) window:)
          (prefix (head window-control) window-control:))
  (define (get xs key) (cdr (assq key xs)))
  (define recipe
    (eval
      (call-with-input-file "start.e"
        (lambda (p) (let loop ([last #f]) (let ([next (read p)]) (if (eof-object? next) last (loop next))))))
      (environment '(chezscheme) '(prefix (state view) view:) '(prefix (state store) store:)
        '(prefix (state model) model:) '(prefix (state construction) construction:)
        '(prefix (head modal) modal:) '(prefix (head message) message:)
        '(prefix (service window) window:))))
  (screen:init!)
  (actor:checkpoint! head:ui-actor #f)
  (for-each kernel:load-module! '("finder" "buffet" "markdown" "delta-log" "bindings" "describe"))
  (actor:call-as head:ui-actor
    (lambda ()
      (let* ([screen ((get recipe 'entry) (list (cons 'head head:ui-actor) '(saved . #f)))]
             [area (cadr (assq 'content (view:children (view:snapshot screen))))]
             [manager (cadr (assq 'windows (view:children (view:snapshot area))))]
             [window (window:current manager)]
             [scratch (window:document manager window)]
             [file (format "/tmp/e-screen-~a.txt" (get-process-id))])
        (define (show!)
          (range:pump!) (widget:pump!) (widget:present! (list (list (widget:prepare! screen 48 12) 0 0))))
        (widget:mount! screen 'shipped-screen) (show!)
        (let* ([focus (widget:focused screen)] [aux (screen:auxiliary! screen)])
          (show!)
          (check 'auxiliary-manager-is-an-ordinary-owned-split-preserving-focus
            (list (widget:focused screen) (length (view:children (view:snapshot area)))
              (get (model:snapshot (window-control:manager aux)) 'scope))
            (list focus 2 area))
          (window-control:select! aux) (show!)
          (window-control:close! aux) (show!)
          (check 'closing-the-last-auxiliary-window-hides-and-retains-it
            (list (widget:focused screen) (length (view:children (view:snapshot area)))
              (get (model:snapshot aux) 'kind) (view:state (view:snapshot area)))
            (list focus 1 'widget-view '(3 1))))
        (let ([editor (widget:descendant window 'document)])
          (routing:input! screen '(text "Screen" keyboard)) (show!)
          (routing:input! screen '(key "C-x" #f)) (routing:input! screen '(key "3" "3")) (show!)
          (check 'shipped-screen-edits-and-splits-without-legacy-placement
            (list (get recipe 'profile) (length (window:list manager)) (store:line scratch 0)
              (equal? editor (widget:focused screen)))
            (list "editor" 2 "Screen" #t)))
        (call-with-output-file file (lambda (p) (display "Visited" p)) 'replace)
        (screen:open-file! screen file) (show!)
        (let* ([focus (widget:focused screen)] [count (length (window:list manager))]
               [app (bindings:show! screen)] [source (view:source (view:snapshot app))])
          (define (ready?) (show!) (eq? (get (get (model:snapshot source) 'value) 'status) 'ready))
          (test:await 'composed-bindings-ready ready?)
          (check 'auxiliary-inspection-keeps-the-origin-focused
            (list (widget:focused screen) (car (get (get (model:snapshot source) 'value) 'subject)))
            (list focus window))
          (bindings:hide! screen) (show!)
          (check 'hidden-auxiliary-inspection-releases-its-source-demand
            (model:demanded? source) #f)
          (check 'auxiliary-inspection-reuses-its-retained-presentation (bindings:show! screen) app)
          (show!) (bindings:key! screen) (show!)
          (routing:input! screen '(key "C-x" #f)) (show!)
          (routing:input! screen '(key "3" "3")) (show!)
          (test:await 'composed-key-inspection
            (lambda () (ready?)
              (let* ([value (get (model:snapshot source) 'value)]
                     [listing (get (get value 'parts) 'listing)]
                     [text (format "~s" (get (model:snapshot listing) 'value))])
                (and (equal? (list-tail (get value 'subject) 5) '(("C-x" "3")))
                  (string:search text "window-control:split!" 0 (string-length text))))))
          (check 'captured-key-does-not-run-its-original-window-command (length (window:list manager)) count)
          (bindings:hide! screen) (show!)
          (check 'key-inspection-dismissal-restores-the-captured-editor
            (list (widget:focused screen) (get (view:state (view:snapshot screen)) 'return-focus))
            (list focus focus)))
        (let* ([focus (widget:focused screen)] [document (window:document manager window)]
               [source (describe:show! 'markdown:view! screen)]
               [aux (screen:auxiliary! screen)]
               [app (window:document (window-control:manager aux) aux)])
          (show!)
          (check 'composed-describe-keeps-the-origin-and-updates-an-explicit-page
            (list (widget:focused screen) (window:document manager window)
              (view:kind (view:snapshot app)) (describe:show! 'car screen source))
            (list focus document 'describe source))
          (show!)
          (keymap:run! (keymap:call widget:invoke! (widget:descendant app 'body) 'open document '((point 0 . 2)))) (show!)
          (check 'auxiliary-document-links-target-the-main-manager
            (list (window:document manager window)
              (car (view:state (interaction:snapshot (widget:descendant window 'document))))
              (window:current manager))
            (list document '(0 . 2) window))
          (window-control:select! aux) (show!)
          (routing:input! screen '(key "ESC" #f)) (show!)
          (check 'auxiliary-return-hides-instead-of-switching-to-another-retained-tool
            (length (view:children (view:snapshot area))) 1))
        (let ([document (window:document manager window)])
          (check 'screen-file-request-places-canonical-document-and-restores-its-root
            (list (store:line document 0)
              ((get recipe 'entry) (list (cons 'head head:ui-actor) (list 'saved (list 'value (cons 'root screen))))))
            (list "Visited" screen)))
        (routing:input! screen '(key "C-x" #f)) (routing:input! screen '(key "C-f" #f)) (show!)
        (let ([finder (window:document manager window)])
          (check 'screen-finder-key-uses-an-owned-app-and-explicit-window-binding
            (list (view:kind (view:snapshot finder)) (get (model:snapshot finder) 'scope)
              (assq 'open (descriptor:commands (view:snapshot finder))))
            (list 'finder window (list 'open window 'open-document '())))
          (let* ([other (cadr (window:list manager))]
                 [copy (window-control:open-document! other finder)])
            (show!)
            (check 'catalogue-app-opening-forks-into-the-destination-with-rebound-commands
              (list (equal? finder copy) (get (model:snapshot copy) 'scope)
                (view:source (view:snapshot copy))
                (assq 'open (descriptor:commands (view:snapshot copy)))
                (window-control:open-document! other finder))
              (list #f other (view:source (view:snapshot finder)) (list 'open other 'open-document '()) copy))))
        (window-control:select! window) (show!)
        (let ([previous (cadr (window:documents manager window))])
          (routing:input! screen '(key "C-x" #f)) (routing:input! screen '(key "b" "b")) (show!)
          (let* ([buffet (window:document manager window)] [table (widget:descendant buffet 'table)]
                 [entry (widget:descendant table 'filter 'entry)]
                 [query (view:source (view:snapshot table))])
            (test:await 'composed-buffet-selection
              (lambda () (show!)
                (let ([selection (get (view:state (interaction:snapshot table)) 'selection)])
                  (and selection (equal? previous (caddr selection))))))
            (check 'composed-buffet-selects-the-previous-document-and-focuses-its-filter
              (list (view:kind (view:snapshot buffet)) (widget:focused screen)
                (store:line (view:source (view:snapshot entry)) 0)) (list 'buffet entry ""))
            (table:toggle-sort! table 'name) (table:toggle-sort! table 'name)
            (window-control:open-document! window previous) (show!)
            (let ([expected (catalogue:neighbor head:ui-actor query previous 'next)])
              (buffet:next! window) (show!)
              (check 'composed-buffer-switching-uses-the-retained-buffet-order
                (window:document manager window) expected))))
        (screen:open-file! screen file) (show!)
        (let* ([document (window:document manager window)]
               [page (markdown:view! window)] [editor #f])
          (show!)
          (check 'composed-markdown-presentation-reuses-its-owned-view
            (list (view:kind (view:snapshot page)) (get (model:snapshot page) 'scope)
              (markdown:view! window document)) (list 'markdown-page window page))
          (markdown:open-source! page document (store:revision document) '(0 . 3)) (show!)
          (set! editor (widget:descendant window 'document))
          (check 'presentation-source-command-restores-the-editor-at-the-reviewed-point
            (list (window:document manager window) (car (view:state (interaction:snapshot editor))))
            (list document '(0 . 3)))
          (check 'unknown-presentations-refuse-before-changing-placement
            (list (refused? (lambda () (window-control:open-document! window document '((presentation . missing)))))
              (window:document manager window)) (list #t document))
          (for-each
            (lambda (opener kind)
              (window-control:open-document! window document) (show!)
              (let ([app (opener)])
                (show!)
                (check (list 'composed-app-opener kind)
                  (list (view:kind (view:snapshot app)) (get (model:snapshot app) 'scope)
                    (begin (routing:input! screen '(key "ESC" #f)) (window:document manager window)))
                  (list kind window document))))
            (list (lambda () (delta-log:open! window)) (lambda () (delta-log:conflicts! window))
              (lambda () (git-view:open! window file)) (lambda () (log-view:open! window)))
            '(delta-review delta-review git-history log))
          (let ([terminal (terminal:open! window "exit 0")])
            (show!)
            (check 'composed-terminal-launch-places-its-base-owned-document
              (list (window:document manager window)
                (view:kind (view:snapshot (widget:descendant window 'document)))
                (refused? (lambda () (window-control:open-document! window terminal '((point 0 . 0))))))
              (list terminal 'terminal #t))
            (vt:close! terminal)))
        (widget:unmount! screen)
        (view:retire! head:ui-actor screen (model:revision screen))
        (check 'screen-retirement-owns-manager-and-keeps-shared-text
          (list (model:snapshot manager) (model:snapshot window) (store:exists? scratch)) '(#f #f #t))
        (delete-file file)))))
