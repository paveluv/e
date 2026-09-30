;; Live facts are captured in the head; the base owns the inspected listing.
(import (only (foundation edoc) elibrary))
(elibrary (apps bindings)
  (export capture-key! copy! create! hide! init! inspect! key! open! page! page-up! press! select! show!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (foundation text) text:) (prefix (head binding-list) listing:)
          (prefix (head dispatch) dispatch:) (prefix (head edit) edit:) (prefix (head head) head:) (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:) (prefix (head layout) layout:) (prefix (head mode) mode:)
          (prefix (head mouse) mouse:) (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service inspection) inspection:) (prefix (state model) model:) (prefix (state view) view:)
          (prefix (sys glyph) glyph:))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))
  (define sessions (make-hashtable equal-hash equal?))
  (define instances (make-hashtable equal-hash equal?))
  (define-record-type session (fields (mutable subject) (mutable basis) (mutable pointer) (mutable parts)))
  (define (release! id)
    (let ([query (hashtable-ref instances id #f)])
      (hashtable-delete! instances id)
      (unless (member query (vector->list (hashtable-values instances))) (hashtable-delete! sessions query))))
  (define (subject root) (list root '(global) #f (if root (format "~s" root) "Global keys")))
  (define (session! id value)
    (or (hashtable-ref sessions id #f)
      (let ([s (make-session (get value 'subject (subject #f)) #f '() '())]) (hashtable-set! sessions id s) s)))
  (define (over? id)
    (let ([point (mouse:position)])
      (define (inside? frame x y)
        (let ([r (widget:frame-clip frame)])
          (or (and (equal? id (widget:frame-id frame))
                (<= (car r) x) (< x (+ (car r) (caddr r))) (<= (cadr r) y) (< y (+ (cadr r) (cadddr r))))
            (exists (lambda (f) (inside? f x y)) (widget:frame-children frame)))))
      (and point (exists (lambda (p) (inside? (car p) (- (car point) 1 (cadr p)) (- (cdr point) 1 (caddr p)))) (widget:shown)))))
  (define (request! id target)
    (let-values ([(source d inputs) (widget:context id)])
      (let ([s (session! (view:source d) (get source 'value '()))])
        (unless (equal? target (session-subject s))
          (session-subject-set! s target) (session-basis-set! s #f)
          (interaction:set-state! head:ui-actor (widget:descendant id 'viewport) #f #f)))))

  (edoc "Select an explicit mounted composition for this inspector. Selection changes the saved inspection subject when its next bounded snapshot is published. No current-window lookup or action evaluation occurs."
        (receiver id (view bindings)) (id model "inspector view") (root model "mounted composition"))
  (define (inspect! id root)
    (widget:inspect root 1) (request! id (subject root)))
  (define (service! id frame)
    (define (mouse-row? r) (let ([key (car r)]) (or (eq? key 'mouse) (and (pair? key) (eq? (car key) 'mouse)))))
    (let-values ([(source d inputs) (widget:context id)])
      (let* ([v (get source 'value '())] [query (view:source d)] [s (session! query v)] [target (session-subject s)] [root (car target)])
        (hashtable-set! instances id query)
        (when (and (equal? (get v 'owner #f) head:ui-actor) (not (eq? (get v 'status #f) 'unavailable))
                (not (exists (lambda (other) (and (< (cadr other) (cadr id)) (equal? query (hashtable-ref instances other #f))))
                       (vector->list (hashtable-keys instances)))))
          (let* ([pointer (if (exists (lambda (other) (and (equal? query (hashtable-ref instances other #f)) (over? other)))
                                (vector->list (hashtable-keys instances))) (session-pointer s) (mouse:bindings))]
                 [available? (or (not root) (interaction:snapshot root))]
                 [basis (if available? (listing:basis root (cadr target) (caddr target) pointer) (list 'unavailable root))]
                 [full-basis (cons target basis)])
            (unless (equal? full-basis (session-basis s))
              (let* ([capture (if available? (apply listing:capture basis pointer (list-tail target 4))
                                '(((unavailable "[Inspected view unavailable]" () "" "" ())) #f))]
                     [parts (list (cons 'mouse (filter mouse-row? (car capture))) (cons 'listing (remp mouse-row? (car capture))))]
                     [changes (filter (lambda (p) (not (equal? p (assq (car p) (session-parts s))))) parts)]
                     [status (inspection:publish! head:ui-actor (view:source d) (get source 'revision 0)
                               target (+ (keymap:generation) (widget:generation)) changes (cadr capture))])
                (when (memq status '(applied unchanged))
                  (session-basis-set! s full-basis) (session-pointer-set! s pointer) (session-parts-set! s parts)))))))))

  (define-record-type presentation (fields value revision cache))
  (define dragging #f)
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (fitted data width)
    (let* ([width (max 1 width)] [cache (presentation-cache data)] [old (hashtable-ref cache width #f)])
      (or old
        (let* ([v (presentation-value data)] [status (get v 'status 'unavailable)]
               [rows (append
                       (if (eq? status 'unavailable) '((unavailable "[Producing attachment unavailable]" () "" "" ())) '())
                       (get v 'rows '())
                       (if (get v 'truncated? #f) '((truncated "[Inspection truncated; inspect a smaller subtree]" () "" "" ())) '()))]
               [result (listing:fit rows width)])
          (when (>= (hashtable-size cache) 4) (hashtable-clear! cache))
          (hashtable-set! cache width result) result))))
  (define (page-rows data width range)
    (let* ([rows (fitted data width)] [start (min (car range) (vector-length rows))]
           [end (min (vector-length rows) (+ start (cdr range)))])
      (map (lambda (i) (cons i (vector-ref rows i))) (map (lambda (i) (+ start i)) (iota (- end start))))))
  (define (render data d width height range)
    (map (lambda (r) (caddr r)) (page-rows data width range)))
  (define (selected data d width)
    (let ([s (view:state d)] [f (fitted data width)])
      (and (equal? (view:basis d) (presentation-revision data)) (list? s) (= (length s) 2)
        (let ([a (listing:position f (car s))] [b (listing:position f (cadr s))])
          (and a b (if (text:position<? a b) (list a b) (list b a)))))))
  (define (decorate data d width height range)
    (let ([selection (selected data d width)])
      (append (apply append
                (map (lambda (r)
                       (let ([text (caddr r)] [spans (cadddr r)] [heading? (list-ref r 4)])
                         (if heading? (list (list (list 0 (car r) (glyph:cells text) 1) (if (and (> (string-length text) 0) (char=? (string-ref text 0) #\[)) 'ghost 'bold)))
                           (map (lambda (span)
                                  (let ([a (glyph:cells (substring text 0 (car span)))] [b (glyph:cells (substring text 0 (cdr span)))])
                                    (list (list a (car r) (- b a) 1) 'italic))) spans)))) (page-rows data width range)))
        (if (not selection) '()
          (apply append (map (lambda (r)
                               (let* ([i (car r)] [s (caddr r)] [a (car selection)] [b (cadr selection)]
                                      [left (if (= i (car a)) (cdr a) 0)] [right (if (= i (car b)) (cdr b) (string-length s))])
                                 (if (and (<= (car a) i (car b)) (< left right))
                                   (let ([x (glyph:cells (substring s 0 left))] [end (glyph:cells (substring s 0 right))])
                                     (list (list (list x i (- end x) 1) 'selection))) '())))
                          (page-rows data width range)))))))

  (edoc "Select characters in a displayed inspection section. Logical anchors retain row identity through wrapping; a changed source refuses the old pointer action."
        (id model "listing view") (caret list "active row, field and character") (fixed list "fixed anchor") (basis integer "displayed section revision"))
  (define (select! id caret fixed basis)
    (let-values ([(source d inputs) (widget:context id)])
      (let ([frame (widget:prepared id)])
        (unless (and frame (= basis (get source 'revision -1))
                  (for-all (lambda (p) (listing:position (fitted (widget:frame-data frame) (caddr (widget:frame-rect frame))) p)) (list caret fixed)))
          (refuse "The displayed inspection selection changed"))
        (interaction:set-state! head:ui-actor id basis (list caret fixed)))))

  (edoc "Copy the selected displayed inspection text at its current source basis." (id model "listing view"))
  (define (copy! id)
    (let-values ([(source d inputs) (widget:context id)])
      (let* ([frame (widget:prepared id)] [data (and frame (widget:frame-data frame))]
             [width (and frame (caddr (widget:frame-rect frame)))] [span (and data (selected data d width))])
        (unless (and span (= (get source 'revision -1) (presentation-revision data))) (refuse "No current inspection selection"))
        (edit:copy-text! (text:to-string
                           (list->vector (text:extract (vector-map cadr (fitted data width)) (text:make-span (caar span) (cdar span) (caadr span) (cdadr span)))) #f)))))
  (define (pointer-bindings frame x y)
    (let* ([data (widget:frame-data frame)] [rows (fitted data (caddr (widget:frame-rect frame)))]
           [d (widget:frame-descriptor frame)] [s (view:state d)])
      (if (not (<= 0 y (- (vector-length rows) 1))) '()
        (let* ([r (vector-ref rows y)] [column (let scan ([cs (glyph:clusters (cadr r))] [i 0] [cells 0])
                                                 (if (or (null? cs) (> (+ cells (cdar cs)) x)) i (scan (cdr cs) (+ i (caar cs)) (+ cells (cdar cs)))))]
               [p (vector-ref (list-ref r 4) column)]
               [fixed (if (and (equal? (view:basis d) (presentation-revision data)) (list? s) (= (length s) 2)) (cadr s) p)]
               [id (widget:frame-id frame)] [basis (presentation-revision data)])
          (list (list '(click primary ()) (keymap:call select! id p p basis))
            (list '(click primary (shift)) (keymap:call select! id p fixed basis))
            (list '(drag primary ()) (keymap:call select! id p fixed basis)))))))
  (define (event! id source d event)
    (and (pair? event) (eq? (car event) 'pointer)
      (cond [(and (eq? (cadr event) 'release) (equal? dragging id)) (set! dragging #f) #t]
        [(and (eq? (caddr event) 'primary) (or (eq? (cadr event) 'press) (and (eq? (cadr event) 'move) (equal? dragging id))))
         (let* ([extend? (or (eq? (cadr event) 'move) (memq 'shift (cadddr event)))]
                [bindings (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5))]
                [binding (assoc (if extend? '(click primary (shift)) '(click primary ())) bindings)])
           (and binding (begin (keymap:run! (cadr binding)) (set! dragging id) (widget:capture! id) #t)))]
        [else #f])))

  (edoc "Create an unmounted inspector for an explicit mounted subject, or false for global keys. The composition has a standard scroll viewport and per-width layout; only demanded views capture live facts."
        (commands list "explicit host commands") (root (or model #f) "mounted subject") (returns model) (public))
  (define (create! commands root)
    (let* ([created (inspection:create! head:ui-actor (subject root) '(mouse listing))] [source (car created)]
           [app (view:create! head:ui-actor source 'bindings 1 (list (cons 'commands commands)) '() source)]
           [scroll (view:create! head:ui-actor #f 'scroll 1 '() #f app)]
           [content (view:create! head:ui-actor #f 'column 1 '() '() app)]
           [status (view:create! head:ui-actor source 'binding-list 1 '() '() app)]
           [parts (map (lambda (p) (list (car p) (view:create! head:ui-actor (cdr p) 'binding-list 1 '() '() app) 'fit)) (cadr created))])
      (view:arrange! head:ui-actor
        (list (list app 0 (list (list 'status status 'fit) (list 'viewport scroll '(grow 1))) (list (cons 'commands commands)))
          (list scroll 0 (list (list 'content content '(grow 1))) '()) (list content 0 parts '())) '()) app))

  (edoc "Page this inspector by its shown viewport. Passing an endpoint wraps to the other end."
        (receiver id (view bindings)) (id model "inspector view") (direction (one-of up down) "page direction"))
  (define (page! id direction)
    (unless (memq direction '(up down)) (error 'page! "expected up or down"))
    (let* ([scroll (widget:descendant id 'viewport)] [f (widget:prepared scroll)])
      (when f (let* ([delta (* (if (eq? direction 'up) -1 1) (max 1 (cadddr (widget:frame-rect f))))]
                     [left (widget:act! scroll 'scroll delta)])
                (when (= left delta) (widget:act! scroll 'scroll (if (positive? delta) -1000000000 1000000000)))))))

  (edoc "Capture one key or chord through the ordinary event pump, then inspect it in this view. Escape or C-g cancels without running a binding."
        (receiver id (view bindings)) (id model "inspector"))
  (define (capture-key! id)
    (let-values ([(source d inputs) (widget:context id)])
      (unless (assq 'reader (view:children d))
        (let* ([s (session! (view:source d) (get source 'value '()))]
               [reader (view:create! head:ui-actor #f 'binding-reader 1
                         (list '(modal . #t) (cons 'subject (list-head (session-subject s) 4))) '() id)])
          (widget:arrange! (list (list id (get (model:snapshot id) 'revision 0)
                                   (cons (list 'reader reader 'fit) (view:children d)) (view:options d))))))))
  (define (finish-key! id sequence)
    (let-values ([(source d inputs) (widget:context id)])
      (let* ([parent (view:parent d)] [app (interaction:snapshot parent)])
        (when sequence (request! parent (append (get (view:options d) 'subject '()) (list sequence))))
        (let-values ([(status changed)
                      (widget:arrange! (list (list parent (get (model:snapshot parent) 'revision 0)
                                               (remp (lambda (c) (equal? (cadr c) id)) (view:children app)) (view:options app))))])
          (unless (eq? status 'applied) (refuse "Key inspection changed while closing its capture"))
          ;; Arrangement flushes provisional input and releases its mirror.
          ;; Read the resulting revision once for this explicit retirement.
          (let ([r (caddar (cadr (model:snapshots (list id))))])
            (when r (view:retire! head:ui-actor id (get r 'revision 0))))))))

  (edoc "Deliver a normalized key to a key inspector's capture control; prefixes wait, completed chords show their bindings, and Escape/C-g cancel. The captured command never runs."
        (id model "capture control") (key string "normalized key event"))
  (define (press! id key)
    (if (member key '("ESC" "C-g")) (finish-key! id #f)
      (let-values ([(source d inputs) (widget:context id)])
        (let* ([target (get (view:options d) 'subject '())] [root (car target)] [sequence (append (view:state d) (list key))])
          (if (and (or (not root) (interaction:snapshot root)) (listing:key-prefix? root (cadr target) sequence))
            (interaction:set-state! head:ui-actor id #f sequence)
            (finish-key! id sequence))))))
  (define (capture-event! id source d event)
    (case (car event) [(key) (press! id (cadr event)) #t] [(text) #t] [else #f]))

  ;; The default placement is the only part that knows about windows.
  (define default-root #f)
  (define (active-app)
    (and default-root
      (exists (lambda (w) (and (equal? (head:window-widget w) default-root) (or (not (head:popup? w)) (> (head:popup-rows) 0)))) (head:windows))
      (interaction:snapshot default-root) (widget:descendant default-root 'app)))
  (define (default-subject)
    (let* ([w (head:current-window)] [root (dispatch:input-root)] [b (head:window-buffer w)])
      (and (not (and default-root (equal? (head:window-widget w) default-root)))
        (list root (if root '(global) (append (mode:key-contexts b) '(global)))
          (and (or (head:app-buffer? b) (head:buffer-read-only b)) #t) (head:buffer-name b)))))
  (define (follow!)
    (let ([app (active-app)])
      (when app (let ([target (default-subject)]) (when target (request! app target))))))
  (define (ensure! target)
    (set! default-root (window:tool! "bindings" (lambda (commands) (create! commands (and target (car target))))))
    (when target (request! (widget:descendant default-root 'app) target)) default-root)

  (edoc "Show mouse, keyboard, command and composition bindings in the default pop-up. An already visible inspector pages down."
        (returns model))
  (define (show!)
    (let ([app (active-app)] [target (default-subject)])
      (if app (begin (when target (request! app target)) (page! app 'down) default-root)
        (let ([root (ensure! target)])
          (if (head:popup? (head:current-window)) (window:pop-up-or-reuse! (widget:host root))
            (begin (window:show-widget! (head:popup) root) (head:show-popup! (head:popup-default-rows)))) root))))

  (edoc "Page the visible default inspector up, or show it when hidden.")
  (define (page-up!) (let ([app (active-app)]) (if app (page! app 'up) (show!))))

  (edoc "Show the inspector in the current window, following its captured subject until another window becomes active." (returns model) (public))
  (define (open!)
    (let* ([target (default-subject)] [root (ensure! target)]) (window:show-widget! (head:current-window) root) root))

  (edoc "Capture a key or chord and show its contextual resolution, binding origin, forwarding trace and shadowed definitions in the default inspector. Return immediately; the ordinary event pump collects the keys.")
  (define (key!)
    (let ([root (show!)])
      (let ([w (find (lambda (w) (and (equal? root (head:window-widget w)) (or (not (head:popup? w)) (> (head:popup-rows) 0)))) (head:windows))])
        (when w (window:focus! w) (capture-key! (widget:descendant root 'app))))))

  (edoc "Hide the default inspector's placements. Its saved subject and scrolling remain for reopening." (public))
  (define (hide!)
    (when default-root
      (for-each (lambda (w)
                  (when (equal? (head:window-widget w) default-root)
                    (if (head:popup? w) (head:hide-popup!) (window:return! default-root)))) (head:windows))))

  (edoc "Register the Bindings composition and commands; hidden inspectors do no capture or trace work." (public))
  (define (init!)
    (let ([b (head:find-tool-buffer "*bindings*")]) (when b (set! default-root (head:buffer-fact b 'widget-id #f))))
    (widget:register! 'bindings 1
      (append (layout:container 'y) (list '(contexts . (widget-bindings)) (cons 'service service!) (cons 'release release!)
                                      (cons 'actions (list (cons 'inspect inspect!) (cons 'page page!))))))
    (widget:register! 'binding-reader 1
      (list '(focus . #t) '(capture . full)
        (cons 'prepare (lambda (id source inputs) id))
        (cons 'render (lambda (id d width height range) (list (string-append "Describe key: " (keymap:sequence-text (view:state d)) "…"))))
        (cons 'measure (lambda (id d axis cross measure) (if (eq? axis 'y) '(1 1) '(1 30))))
        (cons 'event capture-event!) (cons 'actions (list (cons 'press press!)))))
    (widget:register! 'binding-list 1
      (list (cons 'prepare (lambda (id source inputs)
                             (make-presentation (if (eq? (get source 'kind #f) 'inspection-rows)
                                                  (list (cons 'rows (get source 'value '())) '(status . ready))
                                                  (get source 'value '())) (get source 'revision 0) (make-eqv-hashtable))))
        (cons 'render render) (cons 'decorate decorate) '(focus . #t) '(contexts . (widget-binding-list))
        (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'release (lambda (id) (when (equal? id dragging) (set! dragging #f))))
        (cons 'measure (lambda (data d axis cross measure) (if (eq? axis 'y) (let ([n (vector-length (fitted data cross))]) (list (min 1 n) n)) '(1 40))))
        (cons 'anchor (lambda (data at width) (listing:anchor (fitted data width) at)))
        (cons 'locate (lambda (data anchor width) (listing:locate (fitted data width) anchor)))))
    (keymap:bind-default! "C-x TAB" show!) (keymap:bind-default! "C-x S-TAB" page-up!)
    (keymap:bind-default! "C-h k" key!)
    (keymap:bind-default! 'widget-binding-list "M-w" (keymap:call copy! widget:target))
    (for-each (lambda (p) (keymap:bind-default! 'widget-bindings (car p) (keymap:call page! widget:target (cadr p))))
      '(("PAGEUP" up) ("PAGEDOWN" down) ("M-v" up) ("C-v" down)))
    (for-each (lambda (p) (keymap:bind-default! 'widget-bindings (car p)
                            (keymap:call widget:act! (keymap:call widget:descendant widget:target 'viewport) 'scroll (cadr p))))
      '(("UP" -1) ("DOWN" 1) ("C-p" -1) ("C-n" 1) ("HOME" -1000000000) ("END" 1000000000)))
    (for-each (lambda (key) (keymap:bind-default! 'widget-bindings key (keymap:call widget:invoke! widget:target 'return))) '("ESC" "C-g"))
    (head:add-pre-redraw-hook! follow!)))
