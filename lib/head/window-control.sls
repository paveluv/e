;; Presentation and input adaptation for the base-owned window tree.
(import (only (foundation edoc) elibrary))
(elibrary (head window-control)
  (export close! commands discard! init! keep! line-numbers manager navigate! open-app! open-document! register-presentation! return! scroll! scrollbar select! split! toggle-display!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core handle) handle:)
          (prefix (core kernel) kernel:)
          (prefix (head editor) editor:) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head layout) layout:) (prefix (head split-control) split-control:)
          (prefix (head text-layout) text-layout:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (service log) log:) (prefix (service window) window:)
          (prefix (state construction) construction:) (prefix (state model) model:) (prefix (state store) store:) (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define vertical (layout:container 'y))
  (define presentations (kernel:make-registry car))
  (define (read-model id) (caddar (cadr (model:snapshots (list id)))))

  (edoc "Register an optional named document presentation. The builder receives lifetime owner, source document and explicit host commands, returning a fresh unmounted view. Locate applies a source position after placement, including reuse. Registration belongs to the defining module; the builder must clean partial construction."
        (name symbol "presentation name") (build procedure "(owner document commands) -> view")
        (locate procedure "(view point) -> unspecified"))
  (define (register-presentation! name build locate)
    (unless (and (symbol? name) (procedure? build) (procedure? locate)) (error 'register-presentation! "expected a name, builder and locator"))
    (kernel:registry-add! presentations (list name build locate)))

  (edoc "Whether composed windows show line numbers beside ordinary text by default. A window's explicit preference overrides this head setting." (value boolean))
  (define line-numbers
    (make-parameter #f (lambda (v) (unless (boolean? v) (error 'line-numbers "expected a boolean")) v)))

  (edoc "Default position bar for ordinary text in composed windows: false hides it, true or right places it on the right, left on the left, and auto shows it for overflowing source lines. A window's preference overrides this head setting."
        (value (or boolean (one-of left right auto))))
  (define scrollbar
    (make-parameter #f (lambda (v) (unless (memq v '(#t #f left right auto)) (error 'scrollbar "invalid position bar setting")) v)))

  (define (preference d key fallback)
    (let ([v (get (view:options d) key 'default)]) (if (eq? v 'default) fallback v)))
  (define (text-document d)
    (let* ([child (assq 'document (view:children d))] [editor (and child (interaction:snapshot (cadr child)))])
      (and editor (eq? (view:kind editor) 'editor)
        (let ([source (text-source:lookup (view:source editor))])
          (and source (list (cadr child) (vector-length (text-source:lines source))))))))
  (define (chrome d width height)
    (let* ([text (text-document d)] [count (if text (cadr text) 0)]
           [bar (and text (preference d 'scrollbar (scrollbar)))]
           [side (and (> width 1) bar (if (eq? bar 'auto) (and (> count height) 'right) (if (eq? bar 'left) 'left 'right)))]
           [gutter (if (and text (preference d 'line-numbers (line-numbers)))
                     (min (max 0 (- width (if side 2 1))) (+ 1 (string-length (number->string count)))) 0)])
      (list gutter side count)))
  (define (ancestor id kind)
    (let ([d (and id (interaction:snapshot id))])
      (and d (if (eq? (view:kind d) kind) id (ancestor (view:parent d) kind)))))

  (edoc "Resolve a mounted window's containing manager from its acquired ancestry. This performs no remote read and refuses an unavailable or non-window receiver."
        (id model "mounted window") (returns model) (inspect))
  (define (manager id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (unless (eq? (view:kind d) 'window) (error 'window-control "expected a mounted window" id))
      (or (ancestor id 'window-manager) (error 'window-control "window has no manager" id))))

  (edoc "Select this composed window and record it as the manager's current window, preserving its existing document presentation."
        (receiver id (view window)) (id model "window"))
  (define (select! id)
    (let ([owner (manager id)]) (interaction:flush!) (window:select! owner id)))

  (define (shown-frame id)
    (define (find-in f)
      (if (equal? id (widget:frame-id f)) f (exists find-in (widget:frame-children f))))
    (exists (lambda (p) (find-in (car p))) (widget:shown)))
  (define (window-frames f)
    (if (eq? (view:kind (widget:frame-descriptor f)) 'window) (list f)
      (apply append (map window-frames (widget:frame-children f)))))

  (edoc "Move from this window in topology order, or toward the nearest visible window along the caret's row or column. Without a visible caret, use the window center. Geometry comes from the shown frame; the base validates and records the selected window. At an outer edge, keep focus."
        (receiver id (view window)) (id model "origin window")
        (direction (one-of next previous up down left right) "selection direction"))
  (define (navigate! id direction)
    (unless (memq direction '(next previous up down left right)) (error 'navigate! "invalid direction"))
    (if (memq direction '(next previous))
      (let* ([ids (window:list (manager id))] [ids (if (eq? direction 'previous) (reverse ids) ids)]
             [tail (member id ids)])
        (when tail (select! (car (if (pair? (cdr tail)) (cdr tail) ids)))))
      (let* ([owner (manager id)] [f (shown-frame owner)] [origin (shown-frame id)])
        (when (and f origin)
          (let* ([frames (filter (lambda (f) (let ([r (widget:frame-clip f)]) (and (> (caddr r) 0) (> (cadddr r) 0)))) (window-frames f))]
                 [rect (widget:frame-clip origin)]
                 [caret (exists (lambda (p) (widget:caret (car p))) (widget:shown))]
                 [point (if (and caret (layout:contains? rect (car caret) (cdr caret))) caret
                          (cons (+ (car rect) (div (caddr rect) 2)) (+ (cadr rect) (div (cadddr rect) 2))))])
            (define (distance f)
              (let* ([r (widget:frame-clip f)] [x (car r)] [y (cadr r)]
                     [right (+ x (caddr r))] [bottom (+ y (cadddr r))] [cx (car point)] [cy (cdr point)])
                (and (not (equal? id (widget:frame-id f)))
                  (case direction
                    [(left) (and (<= right cx) (<= y cy) (< cy bottom) (- cx right -1))]
                    [(right) (and (> x cx) (<= y cy) (< cy bottom) (- x cx))]
                    [(up) (and (<= bottom cy) (<= x cx) (< cx right) (- cy bottom -1))]
                    [(down) (and (> y cy) (<= x cx) (< cx right) (- y cy))]))))
            (let ([next
                   (let ([best (fold-left (lambda (best f)
                                            (let ([d (distance f)])
                                              (if (and d (or (not best) (< d (car best)))) (cons d (widget:frame-id f)) best))) #f frames)])
                     (and best (cdr best)))])
              (when (and next (not (equal? next id))) (select! next))))))))

  (edoc "Keep this window and close the manager's other windows through their ordinary disposal policy. Shared documents survive."
        (receiver id (view window)) (id model "window to keep"))
  (define (keep! id)
    (let ([owner (manager id)])
      (interaction:flush!) (window:select! owner id)
      (for-each (lambda (other) (unless (equal? id other) (window:close! owner other))) (window:list owner))
      (widget:refresh! owner)))

  (edoc "Split beside this composed window with an independent copy of its current presentation and app-return chain. Keep selection, shared documents and shared terminal processes."
        (receiver id (view window)) (id model "window")
        (direction (one-of above below left right) "new window side") (returns model))
  (define (split! id direction)
    (let ([owner (manager id)])
      (interaction:flush!)
      (let ([result (window:split! owner id direction)]) (widget:refresh! id) result)))

  (edoc "Close this composed window and its owned presentations. The last window remains."
        (receiver id (view window)) (id model "window") (returns boolean))
  (define (close! id)
    (let ([owner (manager id)])
      (interaction:flush!)
      (or (and (window:close! owner id) (begin (widget:refresh! owner) #t))
        (let ([target (widget:command-owner id 'dismiss)])
          (and target (begin (widget:invoke! target 'dismiss) #t))))))

  (define (window-layout d width height measure locate)
    (let* ([status (assq 'status (view:children d))] [rows (if status (min 1 height) 0)]
           [body (descriptor:with d (list (cons 'children (remq status (view:children d)))))]
           [chrome (chrome d width (- height rows))] [gutter (car chrome)] [side (cadr chrome)]
           [x (+ gutter (if (eq? side 'left) 1 0))] [body-width (max 0 (- width gutter (if side 1 0)))])
      (append (map (lambda (p) (list (car p) (layout:translate (cadr p) x 0)))
                ((cdr (assq 'layout vertical)) body body-width (- height rows) measure locate))
        (if status (list (list (cadr status) (list 0 (- height rows) width rows))) '()))))

  (define (window-render data d width height range children)
    (let* ([status (assq 'status (view:children d))] [body-height (max 0 (- height (if status 1 0)))]
           [chrome (chrome d width body-height)] [gutter (car chrome)] [side (cadr chrome)] [count (caddr chrome)]
           [editor (find (lambda (f) (eq? (view:kind (widget:frame-descriptor f)) 'editor)) children)]
           [state (and editor (widget:frame-data editor) (editor:frame-state editor))]
           [top (if state (car (caddr state)) 0)]
           [thumb (layout:scroll-thumb count body-height top)])
      (map (lambda (y)
             (let ([line (make-string width #\space)])
               (when (< y body-height)
                 (when side (string-set! line (if (eq? side 'left) 0 (- width 1))
                              (if (<= (car thumb) y (- (+ (car thumb) (cdr thumb)) 1)) #\┃ #\│)))
                 (when (and editor (widget:frame-data editor) (> gutter 0))
                   (let* ([row (editor:frame-row editor y)]
                          [label (if (and row (zero? (list-ref row 3))) (number->string (+ 1 (car row))) "")]
                          [label (if (> (string-length label) (- gutter 1)) "" label)]
                          [start (+ (if (eq? side 'left) 1 0) (- gutter 1 (string-length label)))])
                     (string-copy! label 0 line start (string-length label))))) line))
        (map (lambda (n) (+ n (car range))) (iota (cdr range))))))

  (edoc "Toggle a composed window's wrapping, line numbers or position bar. Resolve its current preference and update the base policy, including retained ordinary editors. The change does not select another window."
        (receiver id (view window)) (id model "window") (preference (one-of wrap line-numbers scrollbar) "display preference"))
  (define (toggle-display! id preference)
    (unless (memq preference '(wrap line-numbers scrollbar)) (error 'toggle-display! "invalid preference"))
    (let* ([owner (manager id)] [d (interaction:snapshot id)]
           [fallback (case preference [(wrap) (text-layout:wrap-lines)] [(line-numbers) (line-numbers)] [else (scrollbar)])]
           [current (let ([v (get (view:options d) preference 'default)]) (if (eq? v 'default) fallback v))])
      (interaction:flush!) (window:set-display! owner id (list (cons preference (not current))))))

  (edoc "Scroll the shown document from a window's surrounding controls or non-scrolling app headers, preserving focus and selection. Use the first visible scroll target in the active presentation. A wheel event already offered to an inner scroller does not scroll another control at its edge. Return the unconsumed rows."
        (receiver id (view window)) (id model "window") (rows integer "display rows") (returns integer))
  (define (scroll! id rows)
    (define (find-scroll f)
      (and f (> (cadddr (widget:frame-clip f)) 0)
        (if (memq 'scroll (widget:actions (widget:frame-id f))) (widget:frame-id f)
          (exists find-scroll (widget:frame-children f)))))
    (let* ([d (interaction:snapshot id)] [document (and d (assq 'document (view:children d)))]
           [event (widget:event-frame)]
           [offered? (and event (let walk ([at (widget:frame-id event)])
                                  (and at (not (equal? at id))
                                    (or (memq 'scroll (widget:actions at))
                                      (walk (view:parent (interaction:snapshot at)))))))])
      (if (or offered? (not document)) rows
        (let ([target (find-scroll (shown-frame (cadr document)))])
          (if target (widget:act! target 'scroll rows) rows)))))
  (define (window-bindings f x y)
    (append
      (if (exists (lambda (child) (layout:contains? (widget:frame-rect child)
                                    (+ x (car (widget:frame-rect f))) (+ y (cadr (widget:frame-rect f)))))
            (widget:frame-children f)) '()
        (list (list '(click primary ()) (keymap:call select! (widget:frame-id f)))))
      (list (list '(wheel up ()) (keymap:call scroll! (widget:frame-id f) -3))
        (list '(wheel down ()) (keymap:call scroll! (widget:frame-id f) 3)))))
  (define (window-event! id source d event)
    (and (eq? (car event) 'pointer) (eq? (cadr event) 'press) (eq? (caddr event) 'primary)
      (assoc '(click primary ()) (window-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5)))
      (begin (select! id) #t)))
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
                  (let* ([part (glyph:slice (car span) 0 (min (glyph:cells (car span)) (max 0 (- limit at))))]
                         [size (glyph:cells part)] [face (cdr span)])
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
    (let ([owner (manager id)] [key (if (null? identity) name (car identity))])
      (interaction:flush!)
      (let ([result (construction:call! head:ui-actor
                      (lambda (remember!)
                        (define (own! created)
                          (remember! created (lambda ()
                                               (when (model:reference? created)
                                                 (let ([r (read-model created)])
                                                   (when (and r (eq? (get r 'kind #f) 'widget-view) (= (get r 'schema 0) 3) (equal? id (get r 'scope #f))
                                                           (not (view:parent (get r 'value '()))) (not (view:owner (get r 'value '()))))
                                                     (view:retire! head:ui-actor created (get r 'revision 0))))))))
                        (let* ([found (window:find-app owner id key)]
                               [app
                                (cond
                                  [(and found (equal? (car found) id)) (cadr found)]
                                  [found
                                   (let ([rewire? (exists (lambda (row)
                                                            (member (car found) (map cadr (descriptor:commands (cdr row)))))
                                                    (view:tree (cadr found)))])
                                     (own! (view:fork! head:ui-actor (cadr found)
                                                       (append (list (cons 'owner id))
                                                         (if rewire? (list (list 'receivers (list (car found) id))) '())))))]
                                  [else
                                   (let* ([created (own! (build id (commands id)))]
                                          [r (read-model created)] [d (and r (get r 'value #f))])
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
                                       (unless (eq? status 'applied) (error 'open-app! "app changed before preparation" status))) created)])])
                          (window:open-document! owner id app))))])
        ;; The app is now admitted. Display acquisition may fail, but must
        ;; not roll back its durable resources or completed placement.
        (widget:refresh! id) result)))

  (edoc "Open a catalogue document through this window. Keyboard and scripted calls use this window; a pointer action in an inactive panel opens in the previously focused window of the same manager. An app from another window gets an independent presentation with its window commands rebound; an existing named presentation is reused. Preserve the panel's focus."
        (receiver id (view window)) (id model "hosting window") (document (or buffer model) "catalogue text or prepared app")
        (preferences (list-of list) "optional alist: point (row . character), presentation name") (returns model))
  (define (open-document! id document . preferences)
    (unless (and (<= (length preferences) 1)
              (or (null? preferences)
                (let ([xs (car preferences)])
                  (and (list? xs) (for-all (lambda (p) (and (pair? p) (memq (car p) '(point presentation)))) xs)
                    (= (length xs) (length (filter values (list (assq 'point xs) (assq 'presentation xs)))))
                    (let ([point (assq 'point xs)] [presentation (assq 'presentation xs)])
                      (and (or (not point) (let ([p (cdr point)]) (and (pair? p) (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) (list (car p) (cdr p))))))
                        (or (not presentation) (symbol? (cdr presentation)))))))))
      (error 'open-document! "invalid placement preferences" preferences))
    (widget:keep-host-focus!)
    (let* ([owner (manager id)]
           [focused (and (widget:event-frame) (ancestor (widget:focused id) 'window))]
           [destination (if (and focused (equal? owner (ancestor focused 'window-manager))) focused id)]
           [options (if (null? preferences) '() (car preferences))]
           [presentation (assq 'presentation options)]
           [builder (and presentation (kernel:registry-find presentations (lambda (p) (eq? (car p) (cdr presentation)))))])
      (when (and presentation (or (not builder) (not (handle:buffer? document))))
        (error 'open-document! "presentation is unavailable or source is not text" options document))
      (when (and (not builder) (assq 'point options)
              (or (not (handle:buffer? document))
                (let ([app (store:property document 'app #f)]) (and app (eq? (cadr app) 'terminal)))))
        (error 'open-document! "point requires an editor presentation"))
      (interaction:flush!)
      (let ([result
             (if builder
               (open-app! destination (format "~a ~a" (car builder) (store:buffer-name document))
                 (lambda (owner commands) ((cadr builder) owner document commands)) (format "~a:~s" (car builder) document))
               (let* ([r (and (model:reference? document) (read-model document))]
                      [scope (and r (get r 'scope #f))])
                 (if (or (not r) (equal? scope destination))
                   (let ([result (window:open-document! owner destination document)])
                     (widget:refresh! destination) result)
                   (let* ([d (get r 'value '())] [options (view:options d)]
                          [key (get options 'app-key (format "~s" document))])
                     (unless (and (eq? (get r 'kind #f) 'widget-view) (get options 'catalogue #f))
                       (error 'open-document! "expected a listed app" document))
                     (open-app! destination (get options 'name (symbol->string (view:kind d)))
                       (lambda (owner commands)
                         (view:fork! head:ui-actor document
                           (append (list (cons 'owner owner))
                             (if (and (model:reference? scope)
                                   (exists (lambda (row) (member scope (map cadr (descriptor:commands (cdr row))))) (view:tree document)))
                               (list (list 'receivers (list scope owner))) '())))) key)))))])
        (when (assq 'point options)
          (widget:pump!)
          (if builder ((caddr builder) result (cdr (assq 'point options)))
            (editor:move! result (cdr (assq 'point options)))))
        result)))

  (edoc "Discard the document shown in this window. Shared text goes to Trash with its history; disposable output is deleted. An app is retired with its owned presentation resources, preserving borrowed data. Every displaying window chooses its retained fallback."
        (receiver id (view window)) (id model "window showing the document"))
  (define (discard! id)
    (interaction:flush!)
    (let ([document (window:document (manager id) id)])
      (when document
        (if (handle:buffer? document)
          (let ([metadata (cadar (cadr (store:metadata (list document))))])
            (unless metadata (error 'discard! "document is unavailable" document))
            (let-values ([(status current) (store:archive! head:ui-actor document (get metadata 'version #f) 'trash)])
              (unless (eq? status 'applied) (error 'discard! "document changed; choose it again" status)))
            (log:add! 'window-control:discard!
              (format "Killed ~a~a" (get metadata 'name "")
                (cond [(get metadata 'disposable #f) ""]
                  [(get metadata 'modified #f) "; its unsaved work is in the trash"] [else "; it is in the trash"]))))
          (let ([r (read-model document)])
            (unless r (error 'discard! "app is unavailable" document))
            (let-values ([(status current) (view:retire! head:ui-actor document (get r 'revision #f))])
              (unless (eq? status 'applied) (error 'discard! "app changed; choose it again" status)))))
        (widget:refresh! id))))

  (edoc "Return the app currently shown in this explicit window to its saved origin or retained fallback. Preserve an inactive panel's previous focus; base validation refuses a changed active app."
        (receiver id (view window)) (id model "hosting window") (returns (or model #f)))
  (define (return! id)
    (widget:keep-host-focus!)
    (let* ([owner (manager id)] [d (interaction:snapshot id)] [app (assq 'document (view:children d))])
      (unless app (error 'return! "window is empty" id))
      (interaction:flush!)
      (let ([target (widget:command-owner id 'dismiss)])
        (if target (begin (widget:invoke! target 'dismiss) #t)
          (let ([result (window:return! owner id (cadr app))])
            (widget:refresh! id) result)))))

  (edoc "Install window-manager, split and window containers with explicit open/return actions and draggable separators. Loading constructs no windows, mounts or legacy host records." (public))
  (define (init!)
    (kernel:load-module! "window") (kernel:load-module! "widget")
    (widget:register! 'window-manager 2 vertical)
    (kernel:load-module! "split-control")
    (widget:register! 'window-split 2 (split-control:definition))
    (widget:register! 'window 1
      (list (cons 'layout window-layout) (assq 'measure vertical) '(focus . fallback)
        (cons 'contexts
          (lambda (id d)
            (let* ([active (assq 'document (view:children d))]
                   [placement (and active (find (lambda (p) (equal? (cadr p) (cadr active))) (get (view:options d) 'presentations '())))])
              (if (and placement (model:reference? (car placement))) '(window-tool composed-window) '(composed-window)))))
        (cons 'render-children window-render)
        (cons 'decorate (lambda (data d width height range) (list (list (list 0 0 width height) 'chrome))))
        (cons 'event window-event!) (cons 'pointer-bindings window-bindings)
        (cons 'actions (list (cons 'open-document open-document!) (cons 'return return!) (cons 'discard discard!)
                         (cons 'select select!) (cons 'split split!) (cons 'close close!) (cons 'navigate navigate!) (cons 'keep keep!)
                         (cons 'scroll scroll!) (cons 'toggle-display toggle-display!)))))
    (widget:register! 'window-status 1
      (list (cons 'service status-service!) (cons 'prepare status-data) (cons 'viewport status-viewport)
        (cons 'render (lambda (data d width height range) (if (zero? (car range)) (list (car data)) '())))
        (cons 'decorate (lambda (data d width height range) (cadr data)))
        (cons 'measure (lambda (data d axis cross measure) (if (eq? axis 'y) '(1 1) '(0 0))))
        (cons 'event status-event!) (cons 'pointer-bindings status-bindings) (cons 'release status-release!)))
    (for-each (lambda (p) (keymap:bind-default! 'composed-window (car p) (keymap:call navigate! widget:target (cdr p))))
      '(("C-x o" . next) ("M-UP" . up) ("M-DOWN" . down) ("M-LEFT" . left) ("M-RIGHT" . right)))
    (keymap:bind-default! 'composed-window "C-x 0" (keymap:call close! widget:target))
    (keymap:bind-default! 'composed-window "C-x 1" (keymap:call keep! widget:target))
    (keymap:bind-default! 'composed-window "C-x k" (keymap:call discard! widget:target))
    (keymap:bind-default! 'composed-window "C-x l" (keymap:call toggle-display! widget:target 'line-numbers))
    (keymap:bind-default! 'composed-window "C-x t" (keymap:call toggle-display! widget:target 'wrap))
    (keymap:bind-default! 'composed-window "C-x 2" (keymap:call split! widget:target 'below))
    (keymap:bind-default! 'composed-window "C-x 3" (keymap:call split! widget:target 'right))
    (keymap:bind-default! 'window-tool "ESC" (keymap:call return! widget:target))))
