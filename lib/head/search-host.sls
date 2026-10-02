;; Search placement and retargeting; matching and its reusable control have no windows.
(import (only (foundation edoc) elibrary))
(elibrary (head search-host)
  (export finish! init! navigate! open!)
  (import (except (chezscheme) display)
          (prefix (head dispatch) dispatch:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head routing) routing:)
          (prefix (head search-control) search-control:)
          (prefix (head seat) seat:)
          (prefix (head widget) widget:)
          (prefix (head window-host) window-host:)
          (prefix (service search-request) search-request:)
          (prefix (state model) model:)
          (prefix (state view) view:))

  (define-record-type display (fields root search buffer prior rows origin (mutable target-window)))
  (define current #f)
  (define remembered "")
  (define pending (make-hashtable equal-hash equal?))
  (define (ancestor id kind)
    (let ([d (and id (interaction:snapshot id))])
      (and d (if (eq? (view:kind d) kind) id (ancestor (view:parent d) kind)))))
  (define (root-of id)
    (let ([d (interaction:snapshot id)]) (if (view:parent d) (root-of (view:parent d)) id)))
  (define (panel-in id)
    (let ([d (interaction:snapshot id)])
      (and d (if (eq? (view:kind d) 'search-panel) id
               (exists (lambda (child) (panel-in (cadr child))) (view:children d))))))
  (define (change id children)
    (let ([d (interaction:snapshot id)])
      (list id (cdr (assq 'revision (model:snapshot id))) children (view:options d))))
  (define (attach! panel destination)
    (interaction:flush!)
    (let* ([old (view:parent (interaction:snapshot panel))]
           [children (view:children (interaction:snapshot destination))])
      (unless (equal? old destination)
        (let-values ([(status changed)
                      (widget:arrange!
                        (append
                          (if old (list (change old (remp (lambda (child) (equal? (cadr child) panel))
                                                          (view:children (interaction:snapshot old))))) '())
                          (list (change destination (append children (list (list 'search panel 'fit)))))))])
          (unless (eq? status 'applied) (error 'open! "search placement changed" status))))))
  (define (focus-entry! panel)
    (widget:focus! panel (widget:descendant panel 'search 'entry)))
  (define (panel-service! id frame)
    ;; Observe acquired focus, never read or publish remotely on a frame.
    ;; Reparenting and retargeting run at the next command boundary and
    ;; refuse to take focus after a newer explicit interaction.
    (let* ([root (root-of id)] [focus (widget:focused root)]
           [d (and focus (interaction:snapshot focus))]
           [window (and d (eq? (view:kind d) 'editor) (ancestor focus 'window))])
      (when (and window (not (hashtable-ref pending id #f)))
        (hashtable-set! pending id #t)
        (head:run-on-main!
          (lambda ()
            (hashtable-delete! pending id)
            (when (and (interaction:snapshot id) (equal? focus (widget:focused root)))
              (let ([search (widget:descendant id 'search)])
                (search-control:retarget! search focus)
                (attach! id window)
                (focus-entry! id))))))))
  (define (close-panel! id accepted? origin)
    (let* ([d (interaction:snapshot id)] [parent (view:parent d)]
           [root (root-of id)] [search (widget:descendant id 'search)]
           [target (if accepted? (car (view:state (interaction:snapshot search))) origin)])
      (interaction:flush!)
      (let-values ([(status changed)
                    (widget:arrange! (list (change parent (remp (lambda (child) (equal? (cadr child) id))
                                                                (view:children (interaction:snapshot parent))))))])
        (unless (eq? status 'applied) (error 'finish! "search placement changed" status)))
      (when (and target (interaction:snapshot target)) (widget:focus! root target))))

  (define (visible? s) (and s (eq? (display-buffer s) (seat:window-buffer (seat:popup))) (> (seat:popup-rows) 0)))
  (define (close! s accepted?)
    (when (eq? s current) (set! current #f))
    (when (visible? s)
      (if (and (> (display-rows s) 0) (memq (display-prior s) (seat:buffers)))
        (begin (seat:set-window-buffer! (seat:popup) (display-prior s)) (seat:show-popup! (display-rows s)))
        (seat:hide-popup!)))
    (when (or (not accepted?) (eq? (seat:current-window) (seat:popup)))
      (let ([w (if accepted? (display-target-window s) (display-origin s))])
        (when (memq w (seat:windows)) (window-host:focus! w))))
    (widget:unmount! (display-root s))
    (when (memq (display-buffer s) (seat:buffers)) (seat:forget-buffer! (display-buffer s))))

  (edoc "Remove a search panel and retain its nonempty needle for repeat. Acceptance restores the target editor; cancellation restores the original surviving editor. The temporary legacy host restores its previous pop-up."
        (id model "search host") (accepted? boolean "accept or cancel") (origin any "captured editor") (needle string "last needle"))
  (define (finish! id accepted? origin needle)
    (unless (string=? needle "") (set! remembered needle))
    (if (and current (equal? id (display-root current))) (close! current accepted?)
      (when (interaction:snapshot id) (close-panel! id accepted? origin))))

  (edoc "Accept this search's displayed position immediately, then route the navigation key through the restored editor's ordinary bindings."
        (receiver id (view search-panel)) (id model "search panel") (key string "navigation key"))
  (define (navigate! id key)
    (let ([root (root-of id)] [search (widget:descendant id 'search)])
      (search-control:accept! search #f)
      (routing:input! root (list 'key key #f))))
  (define (retarget! s)
    (unless (eq? (seat:current-window) (seat:popup))
      (let ([target (seat:window-editor (seat:current-window))])
        (when target
          (unless (equal? target (car (view:state (interaction:snapshot (display-search s)))))
            (routing:cancel!) (search-control:retarget! (display-search s) target))
          (display-target-window-set! s (seat:current-window)) target))))
  (define (refresh!)
    (when current
      (if (and (visible? current) (interaction:snapshot (display-search current))) (retarget! current)
        (close! current #t))))
  (define (route ordinary event)
    (refresh!)
    (and current
      (or (eq? (seat:current-window) (seat:popup)) (seat:window-editor (seat:current-window)))
      (if (member event '("UP" "DOWN" "LEFT" "RIGHT" "HOME" "END" "PAGEUP" "PAGEDOWN"))
        (begin (search-control:accept! (display-search current) #f) (seat:window-widget (seat:current-window)))
        (display-root current))))

  (edoc "Place incremental search under an explicit editor's composed window, or repeat the active search in its root. Window switches retarget the same entry. False uses the temporary legacy host."
        (target (or model #f) "mounted editor")
        (policy (one-of smart fold exact) "case policy") (returns model "search view"))
  (define (open! target policy)
    (if (or (not target)
          (and (not (ancestor target 'window)) (seat:current-window)
            (equal? target (seat:window-editor (seat:current-window))))) (open-legacy! policy)
      (let ([window (ancestor target 'window)] [existing (panel-in (root-of target))])
        (unless window (error 'open! "Incremental search requires a composed window" target))
        (if existing
          (let ([search (widget:descendant existing 'search)])
            (search-control:retarget! search target) (attach! existing window) (focus-entry! existing)
            (search-control:repeat! search) search)
          (let* ([search (search-control:create! target policy remembered '())]
                 [record (caddar (cadr (model:snapshots (list search))))]
                 [d (cdr (assq 'value record))] [query (view:source d)])
            (guard (ex [else (search-request:close! head:ui-actor query) (raise ex)])
              (let ([panel (view:create! head:ui-actor #f 'search-panel 1 '() '() query)])
                (view:arrange! head:ui-actor
                  (list (list panel 0 (list (list 'search search 'fit)) '())
                    (list search (cdr (assq 'revision record)) (view:children d)
                      (cons (list 'commands (list 'finished panel 'finished '()))
                        (remp (lambda (p) (eq? (car p) 'commands)) (view:options d))))) '())
                ;; The new detached panel is not subscribed yet. Attach it
                ;; together with the acquired destination in one transaction.
                (interaction:flush!)
                (let-values ([(status changed)
                              (widget:arrange!
                                (list (change window (append (view:children (interaction:snapshot window))
                                                       (list (list 'search panel 'fit))))))])
                  (unless (eq? status 'applied) (error 'open! "search placement changed" status)))
                (focus-entry! panel) search)))))))

  (define (open-legacy! policy)
    (refresh!)
    (if current (begin (search-control:repeat! (display-search current)) (display-search current))
      (let* ([w (seat:current-window)] [target (seat:window-editor w)])
        (unless target (error 'open! "Incremental search requires a text editor"))
        (let* ([search (search-control:create! target policy remembered '())]
               [record (caddar (cadr (model:snapshots (list search))))]
               [d (cdr (assq 'value record))] [query (view:source d)]
               [root (view:create! head:ui-actor #f 'search-host 1 '((name . "search")) '() query)]
               [prior (seat:window-buffer (seat:popup))] [rows (seat:popup-rows)])
          (guard (ex [else
                      (when (and current (equal? root (display-root current))) (close! current #t))
                      (search-request:close! head:ui-actor query)
                      (raise ex)])
            (view:arrange! head:ui-actor
              (list (list root 0 (list (list 'search search '(grow 1))) '((name . "search")))
                (list search (cdr (assq 'revision record)) (view:children d)
                  (cons (list 'commands (list 'finished root 'finished '())) (remp (lambda (p) (eq? (car p) 'commands)) (view:options d))))) '())
            (let* ([buffer (window-host:show-widget! (seat:popup) root)] [s (make-display root search buffer prior rows w w)])
              (set! current s)
              (seat:buffer-fact-set! buffer 'internal #t)
              (seat:buffer-fact-set! buffer 'resume-kind #f)
              (seat:show-popup! 1)
              (widget:prepare! root (max 1 (seat:window-content-width (seat:popup))) 1)
              (widget:focus! root (widget:descendant search 'entry)) search))))))

  (edoc "Install the composed search panel and its navigation bindings, plus the temporary legacy placement adapter. Nested search controls can supply their own host." (public))
  (define (init!)
    (search-control:init!)
    (widget:register! 'search-host 1
      (append (layout:container 'y) (list (cons 'actions (list (cons 'finished finish!))))))
    (widget:register! 'search-panel 1
      (append (layout:container 'y)
        (list (cons 'service panel-service!)
          (cons 'release (lambda (id) (hashtable-delete! pending id)))
          (cons 'capture-contexts '(search-panel))
          (cons 'actions (list (cons 'finished finish!) (cons 'navigate navigate!))))))
    (for-each (lambda (key) (keymap:bind-default! 'search-panel key (keymap:call navigate! widget:target key)))
      '("UP" "DOWN" "LEFT" "RIGHT" "HOME" "END" "PAGEUP" "PAGEDOWN"))
    (dispatch:register-input-root! route
      (lambda (ordinary)
        (and (visible? current) (or (eq? (seat:current-window) (seat:popup)) (seat:window-editor (seat:current-window)))
          (display-root current))))
    (head:add-pre-redraw-hook! refresh!)))
