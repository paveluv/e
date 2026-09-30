;; window.sls -- the window commands: the library (window).
;;
;; Focus, splits, resizing and deletion over the head's layout tree, the
;; per-window settings, wrap and line numbers, and the placement
;; commands that show a buffer without leaving the current window.  The
;; tree, the window record and the current window are the head's, the
;; geometry is the painter's; this library is the commands over them,
;; and edit and the apps call it.  Its default keys are bound in init!,
;; owned by the module for reload.

(import (only (foundation edoc) elibrary))
(elibrary (head window)
  (export clear-pop-up! delete! delete-others! display! focus! focus-down! focus-left! focus-next! focus-right! focus-up! init! link! link-target! linked (rename (links-data links)) open-document! pop-up-or-reuse! register-link-tag! resize! return! set-line-numbers! set-wrap! show-widget! split-above! split-below! split-left! split-right! toggle-line-numbers! toggle-wrap! tool! unlink!)
  (import (rnrs)
          (only (chezscheme) format void quotient)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (head catalogue-host) catalogue-host:)
          (prefix (head editor) editor:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head terminal-control) terminal:)
          (prefix (head widget) widget:)
          (prefix (state view) view:))

  (define (buffer-widget b)
    ;; Widget hosts are local. Placement and cleanup also see shared buffers
    ;; after deletion, when their base-owned facts no longer exist.
    (and (not (head:buffer-store-id b)) (head:buffer-fact b 'widget-id #f)))

  (define mounted '()) ; window-hosted roots only; embedded hosts manage themselves
  (define (mount-buffer! b)
    (let ([id (buffer-widget b)])
      (when id
        (widget:mount! id b)
        (unless (assoc id mounted) (set! mounted (cons (cons id b) mounted))))) b)
  (define (release-hidden!)
    ;; Run after placement/actions, so hiding the invoking app cannot release
    ;; its context midway through a command. Work depends on mounted windows,
    ;; not on the number of retained buffers or views.
    (let ([visible (map head:window-widget (head:windows))])
      (for-each (lambda (p) (unless (member (car p) visible) (widget:unmount! (car p)))) mounted)
      (set! mounted (filter (lambda (p) (member (car p) visible)) mounted))))

  (define (mount-window! w)
    (let ([id (and (head:buffer-store-id (head:window-buffer w)) (head:window-widget w))])
      ;; A document remains the actual window buffer. The window is merely
      ;; an opaque host slot for its retained editor view.
      (for-each (lambda (p)
                  (when (or (and (eq? (cdr p) w) (not (equal? (car p) id)))
                          (and (equal? (car p) id) (not (eq? (cdr p) w))))
                    (widget:unmount! (car p)))) mounted)
      (set! mounted (filter (lambda (p)
                              (if (eq? (cdr p) w) (equal? (car p) id) (not (equal? (car p) id)))) mounted))
      (when id
        (unless (assoc id mounted)
          (widget:mount! id w)
          (set! mounted (cons (cons id w) mounted))
          (widget:prepare! id (if (paint:window-wrapped? w) (paint:wrap-width w) (head:window-content-width w)) (head:window-size w)))
        (widget:set-active! id (eq? w (head:current-window))))))

  (define (widget-buffer! id)
    (cond [(find (lambda (b) (equal? id (buffer-widget b))) (head:buffers))
           => values]
      [else (kernel:call-with-runtime-registrations
              (lambda ()
                (let* ([d (view:snapshot id)] [options (if d (view:options d) '())]
                       [name (and options (assq 'name options))]
                       [b (head:new-local-buffer! (if name (cdr name) (format "widget ~a" (cadr id))))])
                  (guard (ex [else (head:forget-buffer! b) (raise ex)])
                    (head:register-app! b
                      (lambda ()
                        (let ([w (find (lambda (w) (eq? (head:window-buffer w) b)) (head:windows))])
                          (when w
                            (widget:set-active! id (eq? w (head:current-window)))
                            (let ([lines (widget:frame-lines (widget:prepare! id (head:window-content-width w) (head:window-size w)))])
                              (head:view-replace! b (if (null? lines) '("") lines) '()
                                (list (cons w '(0 . 0)) (cons (cons 'top w) '(0 . 0))))))))
                      (lambda (event)
                        (cond [(string=? event "BLUR") (widget:set-active! id #f) (widget:cancel! id 'blur) #t]
                          [(string=? event "FOCUS") (widget:set-active! id #t) (widget:key-scopes! id "") #t]
                          [else #f])))
                    (head:buffer-fact-set! b 'resume-kind 'widget)
                    (head:buffer-fact-set! b 'widget-id id)
                    (head:buffer-fact-set! b 'mode "widget")
                    (for-each (lambda (key) (let ([p (assq key options)]) (when p (head:buffer-fact-set! b key (cdr p))))) '(tool-key recency))
                    (head:set-app-presentation! b 0 #f #f)
                    (head:set-app-cursor-visible! b #f)
                    (head:set-app-selectable! b #f)
                    (head:set-app-manages-viewport! b #t)
                    (head:set-app-status-position! b (lambda (b) "")) b))))]))

  (edoc "Show a widget tree in an existing window. Simultaneous additional placements fork views while sharing sources; hidden roots are reused."
        (w window "outer host") (id model "root view id") (returns buffer))
  (define (show-widget! w id)
    (let ([origin (catalogue-host:reference (head:window-buffer w))])
      (head:set-window-buffer! w (widget-buffer! id))
      (let* ([b (head:window-buffer w)] [actual (buffer-widget b)] [d (interaction:snapshot actual)])
        (when (and d (eq? (view:kind d) 'window-tool))
          (when (and origin (not (equal? origin actual)))
            (interaction:set-state! head:ui-actor actual #f (list (cons 'origin origin))))
          (widget:set-active! actual (eq? w (head:current-window)))
          (widget:prepare! actual (head:window-content-width w) (head:window-size w)))
        (head:window-buffer w))))

  (define (tool-window id)
    (let ([slot (widget:host id)])
      (or (find (lambda (w) (eq? (head:window-buffer w) slot)) (head:windows))
        (error 'window "tool has no visible window" id))))

  (edoc "Open a semantic document reference through an explicit window-tool host. Inactive panel clicks use the previously focused window and preserve its focus; keyboard actions use the tool's own window."
        (id model "window-tool view") (ref datum "catalogue document reference"))
  (define (open-document! id ref)
    (let* ([own (tool-window id)] [event-target (head:app-event-focus)]
           [target (if (and event-target (memq event-target (head:windows))) event-target own)]
           [b (catalogue-host:resolve! ref)])
      (unless b (error 'open-document! "document is unavailable" ref))
      (widget:keep-host-focus!)
      (let ([targets (linked 'target target)])
        (for-each (lambda (w) (head:with-window w (head:show-buffer! b)))
          (if (null? targets) (list target) targets)))))

  (edoc "Return an explicitly hosted tool to its saved origin, or the most recent surviving document."
        (id model "window-tool view"))
  (define (return! id)
    (let* ([w (tool-window id)] [d (interaction:snapshot id)] [p (assq 'origin (view:state d))]
           [b (or (and p (catalogue-host:resolve! (cdr p)))
                (find (lambda (b) (not (eq? b (head:window-buffer w)))) (head:buffers)))])
      (when b (head:with-window w (head:show-buffer! b)))))

  (edoc "Retain one named widget tool in this head. Build receives explicit open/return command bindings and returns an unmounted app root; hidden tools are reused. The returned outer view is mounted for the caller's action and can be shown or forked. Hidden mounts are released at the next frame."
        (name string "tool name without brackets") (build procedure "commands -> app view") (returns list))
  (define (tool! name build)
    (let* ([key (string-append "*" name "*")] [old (head:find-tool-buffer key)])
      (if old (buffer-widget (mount-buffer! old))
        (let* ([options (list (cons 'name (string-append "<" name ">")) (cons 'tool-key key) '(recency . behind))]
               [host (view:create! head:ui-actor #f 'window-tool 1 options '())]
               [app (build (list (list 'open host 'open-document '()) (list 'return host 'return '())))])
          (view:arrange! head:ui-actor (list (list host 0 (list (list 'app app '(grow 1))) options)) '())
          (mount-buffer! (widget-buffer! host)) host))))

  (define (tool-service! id frame)
    (let* ([d (interaction:snapshot id)] [app (cadr (assq 'app (view:children d)))])
      (when (assq 'current (widget:commands app))
        (widget:invoke! app 'current
          (and (not (eq? (head:current-buffer) (widget:host id))) (catalogue-host:reference (head:current-buffer)))))))

  (define (init-widget-host!)
    ;; Definition reload keeps runtime mounts. Rediscover only at installation;
    ;; the per-frame release pass never enumerates the document catalogue.
    (set! mounted
      (append (filter values
                (map (lambda (b)
                       (let ([id (buffer-widget b)])
                         (and id (guard (ex [else #f]) (and (eq? (widget:host id) b) (cons id b)))))) (head:buffers)))
        (filter values (map (lambda (w)
                              (let ([id (and (head:buffer-store-id (head:window-buffer w)) (head:window-widget w))])
                                (and id (guard (ex [else #f]) (and (eq? (widget:host id) w) (cons id w)))))) (head:windows)))))
    (widget:register! 'window-tool 1
      (append (layout:container 'y)
        (list (cons 'service tool-service!) (cons 'actions (list (cons 'open-document open-document!) (cons 'return return!))))))
    (mode:register! "widget" '() '() #f #f
      (lambda (source row line)
        (let* ([id (mode:source-fact source 'widget-id #f)] [f (and id (widget:prepared id))])
          (and f (widget:frame-styles f row line)))) '(widget-id))
    (head:add-buffer-placement-hook!
      (lambda (w b peers)
        (let ([id (buffer-widget b)] [old (buffer-widget (head:window-buffer w))])
          (when (and old (not (eq? b (head:window-buffer w)))) (widget:cancel! old 'hidden))
          (mount-buffer! (if (and id (exists (lambda (other) (and (not (eq? w other)) (eq? b (head:window-buffer other)))) peers))
                           (begin (interaction:flush!) (widget-buffer! (view:fork! head:ui-actor id)))
                           b)))))
    (head:add-pre-redraw-hook! release-hidden!)
    (head:set-window-mounter! mount-window!)
    (head:set-editor-state-reader! (lambda (id) (let ([f (widget:prepared id)]) (and f (editor:frame-state f)))))
    (head:set-point-mover!
      (lambda (w p)
        (let ([id (and (head:window-widget w) (head:window-editor w))])
          (and id (begin
                    (let* ([root (head:window-widget w)] [d (interaction:snapshot root)])
                      (when (eq? (view:kind d) 'terminal) (terminal:follow! root #f)))
                    (editor:move! id p) #t)))))
    (head:add-buffer-kill-hook!
      (lambda (b) (let ([id (buffer-widget b)])
                    (when id (widget:unmount! id) (set! mounted (remp (lambda (p) (equal? id (car p))) mounted))))))
    (head:register-resume! 'widget
      (lambda (b positions) (values (list (buffer-widget b)) '()))
      (lambda (reference positions) (values (widget-buffer! (car reference)) '()))))

  (define (message! text)
    ;; an indicator in the echo area: shown, never logged
    (paint:show-message! text #f))

  (define (refuse! message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))

  (define (edit-window!)
    ;; the current window while it shows an edit buffer: the window
    ;; settings, wrap and line numbers, are for text a user edits; an
    ;; app's buffer shows itself as the app decides
    (when (head:app-buffer? (head:current-buffer)) (refuse! "Not an edit buffer"))
    (head:current-window))

  ;;; Focus -----------------------------------------------------------------------

  (define (ordinary-windows)
    ;; the ring of ordinary windows: the pop-up is never in it
    (remq (head:popup) (head:layout-leaves (head:root))))

  (define (focus-ring)
    ;; the windows focus cycles through: the ordinary ones and the pop-up
    ;; while it is shown
    (filter (lambda (w) (or (not (head:popup? w)) (> (head:popup-rows) 0))) (head:layout-leaves (head:root))))

  (define (next-in ring w)
    (let ([tail (cdr (or (memq w ring) (cons #f ring)))])
      (if (pair? tail) (car tail) (car ring))))

  (define (next-window w) (next-in (ordinary-windows) w))

  (edoc "Select a window while it is on screen, the apps left and entered hearing BLUR and FOCUS; the pop-up is selected while it is shown. Whether the window was selected."
        (w window "the window to select")
        (returns boolean))
  (define (focus! w)
    ;; All user-visible focus changes pass here; head:set-current! is
    ;; the raw setter and tells no app.
    (let ([w (edoc:type-value 'window w)])
      (cond
        [(not (and (memq w (head:windows)) (or (not (head:popup? w)) (> (head:popup-rows) 0)))) #f]
        [(eq? w (head:current-window)) #t]
        [else
         (head:dispatch-app-event! "BLUR")
         (let ([id (head:window-widget (head:current-window))])
           (when id (widget:set-active! id #f) (widget:cancel! id 'blur)))
         (head:set-current! w)
         (head:dispatch-app-event! "FOCUS")
         (let ([id (head:window-widget w)])
           (when id (widget:set-active! id #t) (widget:key-scopes! id "")))
         #t])))

  (edoc "Select the next window in layout order, the pop-up among them while it is shown; the window now selected."
        (returns window))
  (define (focus-next!)
    (focus! (next-in (focus-ring) (head:current-window)))
    (head:current-window))

  (define (focus-direction! direction)
    ;; the layout lists the pop-up only while it is shown
    (let* ([layout (paint:window-layout)]
           [current (head:current-window)]
           [cursor (paint:window-screen-position current (head:window-prow current) (head:window-pcol current))]
           [cx (- (cdr cursor) 1)]
           [cy (- (car cursor) 1)])
      ;; Cast a ray from point. This matters in asymmetric trees: from a tall
      ;; right-hand window, for example, the cursor row chooses which of two
      ;; stacked windows on the left receives focus.
      (define (distance entry)
        (let* ([w (car entry)]
               [x0 (head:window-xoff w)] [x1 (+ x0 (head:window-width w) -1)]
               [y0 (cadr entry)] [y1 (+ y0 (caddr entry))])
          (case direction
            [(left) (and (< x1 cx) (<= y0 cy y1) (- cx x1))]
            [(right) (and (> x0 cx) (<= y0 cy y1) (- x0 cx))]
            [(up) (and (< y1 cy) (<= x0 cx x1) (- cy y1))]
            [(down) (and (> y0 cy) (<= x0 cx x1) (- y0 cy))])))
      (let loop ([entries layout] [best #f] [best-distance #f])
        (if (null? entries)
            (when best (focus! (car best)))
            (let ([d (and (not (eq? (caar entries) current)) (distance (car entries)))])
              (if (and d (or (not best-distance) (< d best-distance)))
                  (loop (cdr entries) (car entries) d)
                  (loop (cdr entries) best best-distance)))))))

  (edoc "Select the window above the cursor, the cursor's column choosing among stacked candidates.")
  (define (focus-up!)
    (focus-direction! 'up))

  (edoc "Select the window below the cursor, the cursor's column choosing among stacked candidates.")
  (define (focus-down!)
    (focus-direction! 'down))

  (edoc "Select the window left of the cursor, the cursor's row choosing among side-by-side candidates.")
  (define (focus-left!)
    (focus-direction! 'left))

  (edoc "Select the window right of the cursor, the cursor's row choosing among side-by-side candidates.")
  (define (focus-right!)
    (focus-direction! 'right))

  ;;; Splits ----------------------------------------------------------------------

  (define (split-current-window! orientation b first?)
    ;; Divide the selected leaf along orientation, the new window second
    ;; (below or right) unless first? asks for it above or to the left;
    ;; the new window, or #f without the room.
    (paint:window-layout)
    (let* ([current (head:current-window)]
           [vertical? (eq? orientation 'below)]
           [extent (if vertical? (+ (head:window-size current) 1) (head:window-width current))]
           [minimum (if vertical? (+ (head:min-window-lines) 1) 20)]
           [usable (- extent (if vertical? 0 1))])
      (and (not (head:popup? current)) (>= usable (* 2 minimum))
           (let* ([second (quotient usable 2)]
                  [first (- usable second)]
                  [w (head:make-window b (head:window-top current) (head:window-topseg current)
                                       (head:window-left current) (head:window-prow current) (head:window-pcol current)
                                       (max 1 (- second 1)) 0 0
                                       (head:window-wrap current))]
                  [node (if first?
                            (head:make-layout-split orientation w current first second)
                            (head:make-layout-split orientation current w first second))])
             (head:window-line-numbers-set! w (head:window-line-numbers current))
             (head:replace-layout-window! current node)
             w))))

  (define (split! orientation first?)
    ;; Split only the selected leaf, as in Emacs, showing the same buffer.
    (or (split-current-window! orientation (head:current-buffer) first?)
        (begin (message! "Not enough room to split") #f)))

  (edoc "Split the selected window into a stacked pair; the new window is below and shows the same buffer. The new window, or #f with a message when there is no room."
        (returns (or window #f)))
  (define (split-below!)
    (split! 'below #f))

  (edoc "Split the selected window into a side-by-side pair; the new window is to the right and shows the same buffer. The new window, or #f with a message when there is no room."
        (returns (or window #f)))
  (define (split-right!)
    (split! 'right #f))

  (edoc "Split the selected window into a stacked pair; the new window is above and shows the same buffer. The new window, or #f with a message when there is no room."
        (returns (or window #f)))
  (define (split-above!)
    (split! 'below #t))

  (edoc "Split the selected window into a side-by-side pair; the new window is to the left and shows the same buffer. The new window, or #f with a message when there is no room."
        (returns (or window #f)))
  (define (split-left!)
    (split! 'right #t))

  (edoc "Move the boundary of the nearest enclosing stacked split."
        (delta integer "rows to give the selected side; negative takes them"))
  (define (resize! delta)
    (let loop ([child (head:current-window)])
      (let ([parent (head:layout-parent (head:root) child)])
        (cond
          [(head:popup? child) (head:resize-popup! delta)]
          [(and parent (head:popup? (head:layout-split-second parent)) (> (head:popup-rows) 0))
           ;; the windows above grow at the shown pop-up's expense
           (head:resize-popup! (- delta))]
          [(or (not parent) (head:popup? (head:layout-split-second parent)))
           (message! "No vertical split")]
          [(eq? (head:layout-split-orientation parent) 'below)
           (let ([signed (if (eq? child (head:layout-split-first parent)) delta (- delta))])
             (head:layout-split-first-weight-set!
               parent (max 1 (+ (head:layout-split-first-weight parent) signed)))
             (head:layout-split-second-weight-set!
               parent (max 1 (- (head:layout-split-second-weight parent) signed))))]
          [else (loop parent)]))))

  (edoc "Close the selected window; its sibling subtree takes the space. The last window and the pop-up stay.")
  (define (delete!)
    (let ([current (head:current-window)])
      (cond
        [(head:popup? current) (message! "The pop-up window stays")]
        [(null? (cdr (ordinary-windows))) (message! "Only one window")]
        [else
         (let* ([next (next-window current)]
                [parent (head:layout-parent (head:root) current)]
                [sibling (if (eq? current (head:layout-split-first parent))
                             (head:layout-split-second parent)
                             (head:layout-split-first parent))])
           (head:replace-layout-window! parent sibling)
           (prune-links!)
           (focus! next))])))

  (edoc "Empty the pop-up, window 0: a buffer sent there, by a link say, gives way to the pane's own placeholder and the pane hides; the buffer stays in the list. The ↓ at the right of the pane's status line, where the other windows' × is, does the same.")
  (define (clear-pop-up!)
    (head:hide-popup!))

  (edoc "Keep only the selected window, the pop-up hidden too; the links go with the other windows. The pop-up cannot be kept alone.")
  (define (delete-others!)
    (if (head:popup? (head:current-window))
        (message! "The pop-up window stays")
        (begin
          (head:set-layout-root! (head:current-window))
          (head:hide-popup!)
          (prune-links!)))
    (void))

  ;;; Links -----------------------------------------------------------------------

  ;; Directed links between windows, tagged, many to many: (from to tag)
  ;; triples in the order made.  A link lives while both windows are in the
  ;; layout: reading the links forgets the dead ones, and deleting a window
  ;; drops its links at once.  Tags are registered with a description, so
  ;; the window-link-tag type completes them; target is the one the apps
  ;; know, the window a chooser opens its pick in.
  (define links '())
  (define link-tags (list (cons 'target "the window a chooser in the linked window opens its pick in")))

  (edoc-type window-link-tag "a tag on a link between windows, one the code registered, target say"
    (predicate (lambda (v) (and (symbol? v) (assq v link-tags) #t)))
    (complete (lambda (partial) (map (lambda (entry) (cons (car entry) (cdr entry))) link-tags)))
    (write (lambda (v) (format "'~s" v))))

  (define (live-links)
    ;; the links whose windows are both in the layout
    (let ([alive (head:layout-leaves (head:root))])
      (filter (lambda (l) (and (memq (car l) alive) (memq (cadr l) alive))) links)))

  (define (prune-links!)
    ;; the dead links forgotten, when a window closes or a link changes
    (set! links (live-links)))

  (define (link-data l) (list (head:window-index (car l)) (head:window-index (cadr l)) (caddr l)))

  (define (live-window who w)
    (let ([w (edoc:type-value 'window w)])
      (unless (memq w (head:layout-leaves (head:root))) (error who "not a live window" w))
      w))

  (edoc "Register a tag for links between windows, with a description for its completion; target is registered already, the window a chooser in the linked window opens its pick in."
        (tag symbol "the tag")
        (description string "what a link so tagged means"))
  (define (register-link-tag! tag description)
    (unless (symbol? tag) (error 'register-link-tag! "expected a symbol" tag))
    (unless (string? description) (error 'register-link-tag! "expected a description" description))
    (set! link-tags (append (remp (lambda (entry) (eq? (car entry) tag)) link-tags) (list (cons tag description))))
    (void))

  (edoc "Link the current window to another under a tag: a directed link, of which a window may have many out and many in; the same link asked twice is one. The link as data, (from to tag) by window indexes."
        (w window "the window linked to")
        (tag window-link-tag "the tag, registered")
        (returns list))
  (define (link! w tag)
    (let ([to (live-window 'link! w)] [from (head:current-window)])
      (unless (and (symbol? tag) (assq tag link-tags)) (error 'link! "not a registered link tag" tag))
      (when (eq? to from) (error 'link! "a window cannot link to itself"))
      (prune-links!)
      (unless (exists (lambda (l) (and (eq? (car l) from) (eq? (cadr l) to) (eq? (caddr l) tag))) links)
        (set! links (append links (list (list from to tag)))))
      (link-data (list from to tag))))

  (edoc "Link the current window to another as its target, the window a chooser in this one opens its pick in: the files app opens a chosen file in every target and keeps its own window. The link as data."
        (w window "the target window")
        (returns list))
  (define (link-target! w) (link! w 'target))

  (edoc "Remove the current window's links to another, under one tag or under all of them."
        (w window "the window linked to")
        (tag (list-of window-link-tag) "the tag, at most one; all tags without"))
  (define (unlink! w . tag)
    (let ([to (edoc:type-value 'window w)] [from (head:current-window)])
      (set! links (remp (lambda (l) (and (eq? (car l) from) (eq? (cadr l) to) (or (null? tag) (eq? (caddr l) (car tag)))))
                        (live-links)))
      (void)))

  (edoc "The windows a window links to under a tag, in the order linked: the current window's, or a given window's."
        (tag window-link-tag "the tag")
        (w (list-of window) "the window, at most one; the current one without")
        (returns (list-of window)))
  (define (linked tag . w)
    (let ([from (if (pair? w) (edoc:type-value 'window (car w)) (head:current-window))])
      (map cadr (filter (lambda (l) (and (eq? (car l) from) (eq? (caddr l) tag))) (live-links)))))

  (edoc "Every live link between windows as data, (from to tag) by window indexes, in the order made."
        (returns list))
  (define (links-data) (map link-data (live-links)))

  ;;; The window's settings ---------------------------------------------------------

  (edoc "Toggle soft wrapping of long lines in the current window, shown beside an edit buffer; an app's buffer shows itself.")
  (define (toggle-wrap!)
    (set-wrap! (not (paint:window-wrapped? (edit-window!)))))

  (edoc "Set soft wrapping of long lines in the current window: #t wraps, #f truncates, default follows the buffer's wrap fact, else paint:wrap-lines; it applies beside an edit buffer, an app's buffer shows itself."
        (setting (or boolean (one-of default)) "the window's setting"))
  (define (set-wrap! setting)
    (unless (memq setting '(default #t #f))
      (error 'set-wrap! "expected default, #t or #f" setting))
    (let ([w (edit-window!)])
      (head:window-wrap-set! w setting)
      (head:window-left-set! w 0)
      (head:window-goal-set! w #f)     ; the goal column changes meaning
      (message! (format "Wrap ~a" (if (paint:window-wrapped? w) "on" "off")))))

  (edoc "Toggle the line-number gutter of the current window, shown beside an edit buffer; an app's buffer shows itself.")
  (define (toggle-line-numbers!)
    (set-line-numbers! (not (head:window-line-numbers? (edit-window!)))))

  (edoc "Set the line-number gutter of the current window: #t, #f, or default for the head's line-numbers setting; it shows beside an edit buffer, an app's buffer shows itself."
        (setting (or boolean (one-of default)) "the window's setting"))
  (define (set-line-numbers! setting)
    (unless (memq setting '(default #t #f))
      (error 'set-line-numbers! "expected default, #t or #f" setting))
    (let ([w (edit-window!)])
      (head:window-line-numbers-set! w setting)
      (paint:invalidate-screen-cache!)
      (message! (format "Line numbers ~a" (if (head:window-line-numbers? w) "on" "off")))))

  ;;; Placement -----------------------------------------------------------------------

  (define (window-showing b)
    (find (lambda (w) (eq? (head:window-buffer w) b)) (head:windows)))

  (edoc "Show a buffer without leaving the current window: in the window already showing it, else the next window, else a fresh split below. The window, or #f when the screen has no room for one."
        (b buffer "the buffer to show")
        (returns (or window #f)))
  (define (display! b)
    (let ([b (edoc:type-value 'buffer b)])
      (head:add-buffer! b)
      (cond
        [(window-showing b)]
        [(pair? (cdr (ordinary-windows)))
         (let ([w (next-window (head:current-window))])
           (head:set-window-buffer! w b)
           w)]
        [(split-current-window! 'below b #f)]
        [else #f])))

  (edoc "Show a help-like buffer in the window already displaying it, else in a new window below the current one; focus stays where it was. The window, or #f when there was no room."
        (b buffer "the buffer to show")
        (returns (or window #f)))
  (define (pop-up-or-reuse! b)
    ;; Help-like buffers never appropriate another leaf: the buffer stays a
    ;; reference beside the command that asked for it.
    (let ([b (edoc:type-value 'buffer b)])
      (head:add-buffer! b)
      (or (window-showing b)
          (split-current-window! 'below b #f))))

  ;;; Registration -------------------------------------------------------------------

  (edoc "Install the default window keys, C-x ESC and C-x C-g emptying the pop-up among them, and allow the window commands inside a prompt.")
  (define (init!)
    (init-widget-host!)
    ;; the global commands a prompt may run without losing its input:
    ;; pure window management
    (for-each prompt:allow!
              (list focus-up! focus-down! focus-left! focus-right! focus-next!
                    split-below! split-right! split-above! split-left!
                    delete! delete-others!))
    (for-each
      (lambda (entry) (keymap:bind-default! (car entry) (cadr entry)))
      `(("C-x o" ,focus-next!) ("C-x 0" ,delete!) ("C-x 1" ,delete-others!)
        ("C-x ESC" ,clear-pop-up!) ("C-x C-g" ,clear-pop-up!)
        ("C-x 2" ,split-below!) ("C-x 3" ,split-right!)
        ("C-x l" ,toggle-line-numbers!) ("C-x t" ,toggle-wrap!)
        ("M-UP" ,focus-up!) ("M-DOWN" ,focus-down!)
        ("M-LEFT" ,focus-left!) ("M-RIGHT" ,focus-right!)))))
