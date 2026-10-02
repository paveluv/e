;; Presentation and input adaptation for the base-owned window tree.
(import (only (foundation edoc) elibrary))
(elibrary (head window-control)
  (export close! commands drag! init! open-app! open-document! resize! return! select! split!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core handle) handle:)
          (prefix (core kernel) kernel:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head widget) widget:) (prefix (service window) window:)
          (prefix (state model) model:) (prefix (state store) store:) (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define horizontal (layout:container 'x))
  (define vertical (layout:container 'y))
  (define dragging (make-hashtable equal-hash equal?))
  (define (horizontal? d) (eq? (cdr (assq 'axis (view:options d))) 'x))
  (define (split-layout d width height measure locate)
    (let* ([across? (horizontal? d)]
           [sizes (layout:linear (if across? width height) (if across? 1 0)
                    (map (lambda (n) (list 0 0 (list 'grow n))) (view:state d)))])
      (map (lambda (child size)
             (list (cadr child) (if across? (list (car size) 0 (cadr size) height)
                                  (list 0 (car size) width (cadr size))))) (view:children d) sizes)))
  (define (divider d width height)
    (let* ([parts (split-layout d width height #f #f)] [rect (cadr (car parts))]
           [at (if (horizontal? d) (caddr rect) (- (cadddr rect) 1))])
      (and (<= 0 at) (< at (if (horizontal? d) width height)) at)))
  (define (split-render data d width height range)
    (let ([at (divider d width height)])
      (map (lambda (y)
             (let ([line (make-string width #\space)])
               (when at
                 (if (horizontal? d) (string-set! line at #\│)
                   (when (= y at) (string-fill! line #\─)))) line))
        (map (lambda (n) (+ n (car range))) (iota (cdr range))))))
  (define (divider? f x y)
    (let* ([d (widget:frame-descriptor f)] [rect (widget:frame-rect f)]
           [at (divider d (caddr rect) (cadddr rect))])
      (and at (= at (if (horizontal? d) x y)))))
  (define (split-bindings f x y)
    (if (divider? f x y) (list (list '(drag primary ()) (keymap:call drag! (widget:frame-id f)))) '()))

  (edoc "Set a mounted split's logical proportions immediately. The displayed child identities guard the operation; publication coalesces through the normal interaction channel, without a synchronous request on each pointer movement."
        (receiver id (view window-split)) (id model "split") (expected list "two displayed child references")
        (weights list "two positive rational proportions"))
  (define (resize! id expected weights)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (and (eq? (view:kind d) 'window-split) (equal? expected (map cadr (view:children d)))
                (list? weights) (= (length weights) 2) (for-all (lambda (n) (and (rational? n) (> n 0))) weights))
        (error 'resize! "split changed or proportions are invalid"))
      (interaction:set-state! head:ui-actor id #f weights)))

  (edoc "Begin resizing the shown split divider. Pointer movement supplies backend geometry; only logical proportions are published. A changed topology or cancelled capture ends the gesture."
        (receiver id (view window-split)) (id model "split under the pointer"))
  (define (drag! id)
    (let ([f (widget:event-frame)])
      (unless (and f (equal? id (widget:frame-id f))) (error 'drag! "expected a shown pointer target"))
      (hashtable-set! dragging id (map cadr (view:children (widget:frame-descriptor f))))
      (widget:capture! id)))
  (define (split-event! id source d event)
    (case (car event)
      [(cancel blur) (hashtable-delete! dragging id)]
      [(pointer)
       (let* ([f (widget:event-frame)] [phase (cadr event)] [expected (hashtable-ref dragging id #f)])
         (cond
           [(and (eq? phase 'press) (eq? (caddr event) 'primary)
              (divider? f (list-ref event 4) (list-ref event 5))) (drag! id) #t]
           [(and expected (memq phase '(move release)))
            (let* ([rect (widget:frame-rect f)] [extent (if (horizontal? d) (- (caddr rect) 1) (cadddr rect))]
                   [at (min (- extent 1) (max 1 (+ (list-ref event (if (horizontal? d) 4 5)) (if (horizontal? d) 0 1))))])
              (when (>= extent 2) (resize! id expected (list at (- extent at))))
              (when (eq? phase 'release) (hashtable-delete! dragging id)) #t)]
           [else #f]))]
      [else #f]))
  (define (ancestor id kind)
    (let ([d (and id (interaction:snapshot id))])
      (and d (if (eq? (view:kind d) kind) id (ancestor (view:parent d) kind)))))
  (define (manager id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (eq? (view:kind d) 'window) (error 'window-control "expected a mounted window" id))
      (or (ancestor id 'window-manager) (error 'window-control "window has no manager" id))))

  (edoc "Select this composed window and record it as the manager's current window, preserving its existing document presentation."
        (receiver id (view window)) (id model "window"))
  (define (select! id)
    (let ([owner (manager id)]) (interaction:flush!) (window:select! owner id)))

  (edoc "Split beside this composed window, creating an empty independent window while retaining selection."
        (receiver id (view window)) (id model "window")
        (direction (one-of above below left right) "new window side") (returns model))
  (define (split! id direction)
    (let ([owner (manager id)]) (interaction:flush!) (window:split! owner id direction)))

  (edoc "Close this composed window and its owned presentations. The last window remains."
        (receiver id (view window)) (id model "window") (returns boolean))
  (define (close! id)
    (let ([owner (manager id)]) (interaction:flush!) (window:close! owner id)))

  (define (window-layout d width height measure locate)
    (let* ([status (assq 'status (view:children d))] [rows (if status (min 1 height) 0)]
           [body (descriptor:with d (list (cons 'children (remq status (view:children d)))))])
      (append ((cdr (assq 'layout vertical)) body width (- height rows) measure locate)
        (if status (list (list (cadr status) (list 0 (- height rows) width rows))) '()))))
  (define status-cache (make-hashtable equal-hash equal?))
  (define hovered (make-hashtable equal-hash equal?))
  (define pressed (make-hashtable equal-hash equal?))
  (define (get r key fallback) (cond [(assq key r) => cdr] [else fallback]))
  (define (status-service! id frame)
    (let* ([w (view:parent (interaction:snapshot id))] [d (interaction:snapshot w)]
           [child (assq 'document (view:children d))] [app (and child (cadr child))]
           [presentation (and app (interaction:snapshot app))]
           [document (and presentation (view:source presentation))]
           [text? (and (handle:buffer? document) (store:exists? document))]
           [facts (if text? (map (lambda (key) (cons key (store:property document key (if (eq? key 'conflicts) 0 #f))))
                              '(conflicts read-only modified)) '())]
           [active? (equal? w (ancestor (widget:focused w) 'window))]
           [spans
            (append (list (cons (format "~a▏" (get (view:options d) 'number "")) #f))
              (if text?
                (list (cons (cond [(> (get facts 'conflicts 0) 0) "!! "]
                              [(get facts 'read-only #f) "%% "] [(get facts 'modified #f) "** "] [else "-- "]) #f)
                  (cons (store:buffer-name document) 'bold))
                (list (cons (if presentation (get (view:options presentation) 'name (format "<~a>" (view:kind presentation))) "Empty") 'bold)))
              (if (and presentation (eq? (view:kind presentation) 'editor))
                (let ([point (car (view:state presentation))]) (list (cons (format "  L~a C~a" (+ 1 (car point)) (+ 1 (cdr point))) #f))) '())
              (if app (widget:status app active?) '()))]
           [next (list w active? spans)] [old (hashtable-ref status-cache id #f)])
      (unless (equal? next old)
        (hashtable-set! status-cache id next) (widget:repaint! id #t))))
  (define (status-data id source inputs)
    (cons id (or (hashtable-ref status-cache id #f) (list (view:parent (interaction:snapshot id)) #f '()))))
  (define (status-viewport data d width height range)
    (let* ([id (car data)] [w (cadr data)] [active? (caddr data)] [spans (cadddr data)]
           [limit (max 0 (- width 8))] [at 0] [text ""] [decorations '()] [actions '()])
      (for-each (lambda (span)
                  (let* ([part (glyph:slice (car span) 0 (max 0 (- limit at)))] [size (glyph:cells part)] [face (cdr span)])
                    (set! text (string-append text part))
                    (when (> size 0)
                      (if (keymap:call-action? face)
                        (set! actions (cons (list at (+ at size) face) actions))
                        (when face (set! decorations (cons (list (list at 0 size 1) face) decorations)))))
                    (set! at (+ at size)))) spans)
      (let ([start (max 0 (- width 7))])
        (for-each (lambda (offset action)
                    (when (< (+ start offset) width)
                      (set! actions (cons (list (+ start offset) (+ start offset 1) action) actions))))
          '(1 3 5) (list (keymap:call split! w 'below) (keymap:call split! w 'right) (keymap:call close! w)))
        (let ([hover (hashtable-ref hovered id #f)])
          (list (glyph:fit (string-append (glyph:fit text start) "│↕│↔│×│") width)
            (append (list (list (list 0 0 width 1) (if active? 'status 'status-inactive)))
              (map (lambda (p) (list (car p) (list (if active? 'status 'status-inactive) (cadr p)))) decorations)
              (if hover
                (let ([hit (find (lambda (p) (equal? hover (keymap:action-text (caddr p)))) actions)])
                  (if hit (list (list (list (car hit) 0 (- (cadr hit) (car hit)) 1) (list (if active? 'status 'status-inactive) 'hover))) '())) '()))
            actions)))))
  (define (status-action f x y)
    (and (= y 0) (find (lambda (p) (<= (car p) x (- (cadr p) 1))) (caddr (widget:frame-data f)))))
  (define (status-bindings f x y)
    (let ([hit (status-action f x y)] [w (view:parent (widget:frame-descriptor f))])
      (list (list '(click primary ()) (if hit (caddr hit) (keymap:call select! w))))))
  (define (status-event! id source d event)
    (case (car event)
      [(cancel blur) (hashtable-delete! pressed id) (hashtable-delete! hovered id) (widget:repaint! id)]
      [(pointer)
       (let* ([phase (cadr event)] [hit (and (not (eq? phase 'leave)) (status-action (widget:event-frame) (list-ref event 4) (list-ref event 5)))]
              [key (and hit (keymap:action-text (caddr hit)))] [old (hashtable-ref hovered id #f)])
         (hashtable-set! hovered id key)
         (unless (equal? key old) (widget:repaint! id))
         (case phase
           [(press)
            (and (eq? (caddr event) 'primary)
              (begin
                (select! (view:parent d))
                (and hit (begin (hashtable-set! pressed id key) (widget:capture! id) #t))))]
           [(release)
            (let ([before (hashtable-ref pressed id #f)])
              (hashtable-delete! pressed id)
              (when (and before (equal? before key)) (keymap:run! (caddr hit))) (and before #t))]
           [else #f]))]
      [else #f]))
  (define (status-release! id)
    (hashtable-delete! status-cache id) (hashtable-delete! hovered id) (hashtable-delete! pressed id))

  (edoc "Build open/return command bindings for an explicit logical window. Pass them to an app constructor before preparing that app under the window's lifetime. This allocates nothing and reads no remote state."
        (id model "window view") (returns list))
  (define (commands id)
    (unless (model:reference? id) (error 'commands "expected a window model reference" id))
    (list (list 'open id 'open-document '()) (list 'return id 'return '())))

  (edoc "Open a retained named app in this window. Reuse its existing presentation, or explicitly fork another pane's matching app with shared sources and rebound host commands. Otherwise call build with this window and its open/return bindings; build must return a fresh unmounted root scoped to the window and clean up if construction raises. Failed admission retires a returned candidate. An optional stable key distinguishes apps with the same display name."
        (receiver id (view window)) (id model "destination window") (name string "display name without brackets")
        (build procedure "(owner commands) -> fresh app model") (identity (list-of string) "optional nonempty stable key, default name") (returns model))
  (define (open-app! id name build . identity)
    (unless (and (string? name) (> (string-length name) 0) (procedure? build)
              (<= (length identity) 1) (for-all (lambda (s) (and (string? s) (> (string-length s) 0))) identity))
      (error 'open-app! "expected a name, builder and optional stable key"))
    (let ([owner (manager id)] [key (if (null? identity) name (car identity))] [created #f])
      (interaction:flush!)
      (guard (ex [else
                  (when (model:reference? created)
                    (let ([r (model:snapshot created)])
                      (when (and r (eq? (get r 'kind #f) 'widget-view) (= (get r 'schema 0) 3) (equal? id (get r 'scope #f))
                              (not (view:parent (get r 'value '()))) (not (view:owner (get r 'value '()))))
                        (view:retire! head:ui-actor created (get r 'revision 0)))))
                  (raise ex)])
        (let* ([found (window:find-app owner id key)]
               [app
                (cond
                  [(and found (equal? (car found) id)) (cadr found)]
                  [found
                   (let ([rewire? (exists (lambda (row)
                                            (member (car found) (map cadr (descriptor:commands (cdr row)))))
                                    (view:tree (cadr found)))])
                     (set! created (view:fork! head:ui-actor (cadr found)
                                     (append (list (cons 'owner id))
                                       (if rewire? (list (list 'receivers (list (car found) id))) '())))) created)]
                  [else
                   (set! created (build id (commands id)))
                   (let* ([r (model:snapshot created)] [d (and r (get r 'value #f))])
                     (unless (and r (eq? (get r 'kind #f) 'widget-view) (= (get r 'schema 0) 3)
                               (equal? id (get r 'scope #f)) (not (view:parent d)) (not (view:owner d)))
                       (error 'open-app! "builder must return a fresh unmounted app under this window" created))
                     (let-values ([(status rows)
                                   (view:arrange! head:ui-actor
                                     (list (list created (get r 'revision 0) (view:children d)
                                             (append (list (cons 'app-key key))
                                               (if (assq 'name (view:options d)) '() (list (cons 'name (string-append "<" name ">"))))
                                               (if (assq 'catalogue (view:options d)) '() '((catalogue . #t)))
                                               (if (assq 'audience (view:options d)) '() (list (list 'audience head:ui-actor)))
                                               (remp (lambda (p) (eq? (car p) 'app-key)) (view:options d))))) '())])
                       (unless (eq? status 'applied) (error 'open-app! "app changed before preparation" status)))) created])])
          (window:open-document! owner id app)))))

  (edoc "Open a catalogue document through this window. Keyboard and scripted calls use this window; a pointer action in an inactive panel opens in the previously focused window of the same manager. Prepared apps must already belong to the destination window. Preserve the panel's focus."
        (receiver id (view window)) (id model "hosting window") (document (or buffer model) "catalogue text or prepared app") (returns model))
  (define (open-document! id document)
    (widget:keep-host-focus!)
    (let* ([owner (manager id)]
           [focused (and (widget:event-frame) (ancestor (widget:focused id) 'window))]
           [destination (if (and focused (equal? owner (ancestor focused 'window-manager))) focused id)])
      (interaction:flush!)
      (window:open-document! owner destination document)))

  (edoc "Return the app currently shown in this explicit window to its saved origin or retained fallback. Preserve an inactive panel's previous focus; base validation refuses a changed active app."
        (receiver id (view window)) (id model "hosting window") (returns (or model #f)))
  (define (return! id)
    (widget:keep-host-focus!)
    (let* ([owner (manager id)] [d (interaction:snapshot id)] [app (assq 'document (view:children d))])
      (unless app (error 'return! "window is empty" id))
      (interaction:flush!)
      (window:return! owner id (cadr app))))

  (edoc "Install window-manager, split and window containers with explicit open/return actions and draggable separators. Loading constructs no windows, mounts or legacy host records." (public))
  (define (init!)
    (kernel:load-module! "window") (kernel:load-module! "widget")
    (widget:register! 'window-manager 2 vertical)
    (widget:register! 'window-split 2
      (list (cons 'layout split-layout) (cons 'render split-render)
        (cons 'measure (lambda (data d axis cross measure)
                         ((cdr (assq 'measure (if (horizontal? d) horizontal vertical))) data d axis cross measure)))
        (cons 'pointer-bindings split-bindings) (cons 'event split-event!)
        (cons 'release (lambda (id) (hashtable-delete! dragging id)))
        (cons 'actions (list (cons 'resize resize!) (cons 'drag drag!)))))
    (widget:register! 'window 1
      (list (cons 'layout window-layout) (assq 'measure vertical) '(focus . fallback)
        (cons 'actions (list (cons 'open-document open-document!) (cons 'return return!)
                         (cons 'select select!) (cons 'split split!) (cons 'close close!)))))
    (widget:register! 'window-status 1
      (list (cons 'service status-service!) (cons 'prepare status-data) (cons 'viewport status-viewport)
        (cons 'render (lambda (data d width height range) (if (zero? (car range)) (list (car data)) '())))
        (cons 'decorate (lambda (data d width height range) (cadr data)))
        (cons 'measure (lambda (data d axis cross measure) (if (eq? axis 'y) '(1 1) '(0 0))))
        (cons 'event status-event!) (cons 'pointer-bindings status-bindings) (cons 'release status-release!)))))
