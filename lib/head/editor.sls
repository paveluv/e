;; Editor widget implementation. Public commands are re-exported by edit.
(import (only (foundation edoc) elibrary))
(elibrary (head editor)
  (export basis (rename (editor-state:create! create-view!)) delete! expression! format! frame-hit frame-position frame-row frame-state history! insert! insert-at! move! page! paste! register! register-effect! replace-region! rewrite-regions! scroll! select! selection set-mark! transfer!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (foundation string) string:) (prefix (foundation text) text:) (prefix (head editor-state) editor-state:) (prefix (head expression) expression:)
          (prefix (head head) head:) (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head mode) mode:) (prefix (head render) render:)
          (prefix (head text-control) text-control:) (prefix (head text-layout) text-layout:)
          (prefix (head text-source) text-source:) (prefix (head widget) widget:)
          (prefix (service document) document:) (prefix (state store) store:)
          (prefix (state surface) surface:) (prefix (state view) view:) (prefix (sys glyph) glyph:) (prefix (sys tty) tty:))

  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (option d key fallback) (cond [(assq key (view:options d)) => cdr] [else fallback]))
  (define (position? p)
    (and (pair? p) (integer? (car p)) (exact? (car p)) (>= (car p) 0)
      (integer? (cdr p)) (exact? (cdr p)) (>= (cdr p) 0)))
  (define (points source d)
    (editor-state:points (text-control:mirror source) (text-control:revision source) d))
  (define-record-type mount (fields (mutable mode) (mutable facts) (mutable dimensions) (mutable goal) (mutable group) (mutable annotations) (mutable backend?) (mutable surface) (mutable effects)))
  (define effects (kernel:make-registry))

  (edoc "Register a transient source effect for mounted editors. The procedure receives an already acquired text mirror and returns a revisioned annotation batch and optional monotonic expiry time. Called on the service path, never paint; perform no remote reads or model writes. Module retraction removes the provider."
        (proc procedure "source to annotations and deadline"))
  (define (register-effect! proc) (kernel:registry-add! effects proc))
  (define mounts (make-hashtable equal-hash equal?))
  (define (mounted id)
    (or (hashtable-ref mounts id #f)
      (let ([m (make-mount #f '() #f #f #f #f #f #f #f)]) (hashtable-set! mounts id m) m)))
  (define dragging #f)
  (define (release! id)
    (hashtable-delete! mounts id)
    (when (equal? dragging id) (set! dragging #f)))

  (define (annotation-index source batch)
    (let* ([mirror (text-control:mirror source)] [lines (text-control:lines source)])
      (unless (editor-state:annotations? batch) (error 'editor "invalid annotations" batch))
      (let* ([changes (and (pair? batch) (equal? (car batch) (text-source:id mirror))
                        (text-source:changes mirror (cadr batch) (text-control:revision source)))]
             [ranges (if (not changes) '()
                       (filter values
                         (map (lambda (p)
                                (let ([s (fold-left (lambda (s delta) (and s (text:rebase-span s delta)))
                                           (text:datum->span (car p)) changes)])
                                  (and s (not (text:span-empty? s))
                                    (for-all (lambda (p) (and (< (car p) (vector-length lines))
                                                           (<= (cdr p) (string-length (vector-ref lines (car p))))))
                                      (list (text:span-start s) (text:span-end s)))
                                    (list s (cadr p))))) (caddr batch))))]
             [ordered (list-sort (lambda (a b) (text:position<? (text:span-start (car a)) (text:span-start (car b)))) ranges)])
        ;; Prefix maximum ends bound visible-range lookup, even with overlaps.
        (list->vector (let loop ([rest ordered] [end 0])
                        (if (null? rest) '()
                          (let ([end (max end (car (text:span-end (caar rest))))])
                            (cons (append (car rest) (list end)) (loop (cdr rest) end)))))))))
  (define (annotations id source inputs)
    (let ([input (assq 'annotations inputs)])
      (if (not (and input (eq? (cadr input) 'ready))) '#()
        (let* ([m (mounted id)]
               ;; Our selection is not an annotation dependency. Options do
               ;; carry a model revision; external provisional producers keep
               ;; their full dependency stamp, including interaction state.
               [key (list (text-control:mirror source) (text-control:revision source)
                      (map (lambda (row) (if (equal? id (car row)) (list-head row 2) row)) (cadddr input)))]
               [old (mount-annotations m)])
          (if (and old (equal? key (car old))) (cdr old)
            (let ([index (annotation-index source (caddr input))])
              (mount-annotations-set! m (cons key index)) index))))))
  (define (refresh-effects! id source)
    (let* ([m (mounted id)]
           [batches (map (lambda (proc)
                           (let-values ([(batch deadline) (proc (text-control:mirror source))])
                             (when deadline (head:request-frame-at! deadline)) batch)) (kernel:registry-items effects))]
           [key (cons (text-control:revision source) batches)] [old (mount-effects m)])
      (unless (and old (equal? key (car old)))
        (mount-effects-set! m (cons key (map (lambda (batch) (annotation-index source batch)) batches)))
        (widget:repaint! id #t))))
  (define (row-annotations index row)
    (let ([end (let search ([lo 0] [hi (vector-length index)])
                 (if (= lo hi) lo
                   (let ([mid (div (+ lo hi) 2)])
                     (if (<= (car (text:span-start (car (vector-ref index mid)))) row)
                       (search (+ mid 1) hi) (search lo mid)))))])
      (let loop ([i (- end 1)] [out '()])
        (if (or (< i 0) (< (caddr (vector-ref index i)) row)) out
          (let ([p (vector-ref index i)])
            (loop (- i 1) (if (<= row (car (text:span-end (car p)))) (cons p out) out)))))))

  (define (snapshot id source)
    (let ([m (mounted id)])
      (if (mount-backend? m)
        (and (mount-surface m) (equal? (assq 'id source) (assq 'id (car (mount-surface m))))
          (car (mount-surface m))) source)))
  (define (following? inputs)
    (let ([p (assq 'follow inputs)]) (and p (eq? (cadr p) 'ready) (caddr p))))
  (define (acquire-surface! id mirror d allocation)
    (let* ([m (mounted id)] [document (text-source:id mirror)]
           [backend? (or (store:property document 'manages-viewport #f) (and (surface:snapshot document) #t))]
           [old (and (mount-surface m)
                  (equal? (cdr (assq 'id (car (mount-surface m)))) document) (mount-surface m))])
      (unless (eq? backend? (mount-backend? m)) (widget:repaint! id #t))
      (mount-backend?-set! m backend?)
      (if (not backend?) (mount-surface-set! m #f)
        (let* ([revision (text-source:revision mirror)] [lines (text-source:lines mirror)]
               [source (if (and old (= revision (text-control:revision (car old)))
                             (eq? lines (text-control:lines (car old)))) (car old)
                         (list (cons 'id document) (cons 'revision revision) (cons 'value lines)))]
               [ps (points source d)] [top (if ps (car (caddr ps)) 0)]
               [height (if allocation (cadddr (widget:frame-rect allocation)) 1)]
               [grid (store:property document 'size '(1 1))]
               [frame (render:prepare (and old (cdr old)) document lines revision
                        (list (cons (max 0 (- top height)) (+ top (* 2 height)))) (car grid))])
          ;; Text and rendition are separate publications. Keep the last
          ;; coherent pair until both arrive; never flash unstyled new text.
          (when (or (render:header frame) (not (store:property document 'alive #f)))
            (unless (and old (eq? source (car old)) (eq? frame (cdr old)))
              (mount-surface-set! m (cons source frame)) (widget:repaint! id #t)))))))

  (define (service! id frame)
    (let* ([d (interaction:snapshot id)] [ref (and d (view:source d))])
      (when (and ref (eq? (car ref) 'buffer) (text-source:lookup ref)
              (store:visible? head:ui-actor ref))
        (mode:refresh! ref)
        (acquire-surface! id (text-source:lookup ref) d frame)
        (when (or (not (mount-backend? (mounted id))) (mount-surface (mounted id)))
          (let-values ([(source d inputs) (widget:context id 'current)])
            (refresh-effects! id source)
            (unless (following? inputs)
              (text-control:advance! id source d (list-head (editor-state:state d) 3)
                (lambda (ps) (append ps (list (cadddr (editor-state:state d)))))))
            (let* ([m (mounted id)] [document (text-source:id (text-control:mirror source))]
                   [name (option d 'mode (store:property document 'mode #f))] [mode (and name (mode:find name))]
                   [facts (cons (cons 'wrap (store:property document 'wrap 'default))
                            (map (lambda (key) (cons key (store:property document key #f))) (remq 'wrap (mode:required-facts mode))))]
                   [signature (list mode (and mode (mode:render mode)) (and mode (mode:row-styles mode)) (and mode (mode:styles mode))
                                (text-layout:wrap-lines) (text-layout:scroll-margin))])
              (unless (equal? (widget:focused id) id) (mount-group-set! m #f))
              ;; Metadata acquisition is on the service path, never paint or motion.
              (unless (and (equal? signature (mount-mode m)) (equal? facts (mount-facts m)))
                (mount-mode-set! m signature) (mount-facts-set! m facts) (widget:repaint! id #t))))))))
  (define (prepare id source inputs)
    (text-control:mirror source)
    (let* ([m (mounted id)] [lines (text-control:lines source)]
           [frame (or (and (mount-surface m) (eq? source (car (mount-surface m))) (cdr (mount-surface m)))
                    (render:prepare #f #f lines (text-control:revision source) '()))])
      (list id source frame (mode:source lines (mount-facts m)) (and (mount-mode m) (car (mount-mode m)))
        (annotations id source inputs)
        (if (mount-mode m) (list-ref (mount-mode m) 4) (text-layout:wrap-lines))
        (if (mount-mode m) (list-ref (mount-mode m) 5) (text-layout:scroll-margin)) (following? inputs)
        (if (mount-effects m) (cdr (mount-effects m)) '()))))
  (define (wrap-setting data d)
    (let* ([own (option d 'wrap 'default)]
           [source (mode:source-fact (list-ref data 3) 'wrap 'default)]
           [setting (if (eq? own 'default) source own)])
      (and (not (render:header (caddr data))) (if (eq? setting 'default) (list-ref data 6) setting))))
  (define (clean-wrap? setting)
    (or (eq? setting 'clean) (and (pair? setting) (eq? (car setting) 'clean))))
  (define (layout-width data d width)
    (let ([setting (wrap-setting data d)])
      (and setting (max 1 (cond [(and (pair? setting) (eq? (car setting) 'clean)) (min width (cdr setting))]
                            [(eq? setting 'clean) width] [else (- width 1)])))))

  (define (contexts id d)
    (let ([signature (mount-mode (mounted id))])
      (append (mode:key-contexts (and signature (car signature))) '(widget-editor))))
  (define (snap lines frame p)
    (let* ([row (min (car p) (- (vector-length lines) 1))]
           [col (min (cdr p) (string-length (vector-ref lines row)))])
      (cons row (render:character frame row (render:column frame row col)))))
  (define (address lines width p)
    (cons (car p) (if width (text-layout:segment (text-layout:breaks (vector-ref lines (car p)) width) (cdr p)) 0)))
  ;; A projection is (data points top left width height rows). Wrapped segment
  ;; addresses, horizontal cells and desired columns never enter the view.
  (define (project data d width height reveal?)
    (let* ([source (cadr data)] [lines (text-control:lines source)] [frame (caddr data)]
           [header (and (list-ref data 8) (render:header frame))]
           [cursor (and header (caddr header))]
           [ps (if cursor
                 (let* ([p (cons (car cursor) (render:character frame (car cursor) (cadr cursor)))]
                        [start (max 0 (- (vector-length lines) (car (cadddr header))))])
                   (list p p (cons (max start (- (car cursor) height -1)) 0))) (points source d))]
           [wrap (layout-width data d width)]
           [ps (and ps (map (lambda (p) (snap lines frame p)) ps))])
      (if (not ps) (list data #f '(0 . 0) 0 width height)
        (let-values ([(caret top left) (text-layout:scroll lines frame wrap width height 0
                                         (address lines wrap (caddr ps)) 0 (car ps) (list-ref data 7))])
          (list data ps (if reveal? top (address lines wrap (caddr ps))) left width height)))))
  (define (viewport data d width height range)
    (let* ([m (mounted (car data))] [dimensions (cons width height)]
           [g (project data d width height #f)] [source (cadr data)] [lines (text-control:lines source)]
           [top (caddr g)] [left (cadddr g)] [wrap (layout-width data d width)]
           [frame (if (render:header (caddr data)) (caddr data)
                    (render:prepare (caddr data) #f lines (text-control:revision source)
                      (list (cons (car top) (+ (car top) height)))))]
           [data (cons* (car data) source frame (cdddr data))] [cache (make-eqv-hashtable)])
      (unless (equal? dimensions (mount-dimensions m))
        (mount-goal-set! m #f) (when (mount-backend? m) (head:wake-main!)))
      (mount-dimensions-set! m dimensions)
      (cons data (append (cdr g) (list (if (not (cadr g)) '()
                                         (let loop ([y 0] [row (car top)] [segment (cdr top)] [out '()])
                                           (if (or (>= y (+ (car range) (cdr range))) (>= row (vector-length lines))) (reverse out)
                                             (let* ([breaks (and wrap (text-layout:breaks (vector-ref lines row) wrap))]
                                                    [next? (and breaks (< (+ segment 1) (vector-length breaks)))]
                                                    [start (if breaks (render:column frame row (vector-ref breaks segment)) left)]
                                                    [end (if next? (render:column frame row (vector-ref breaks (+ segment 1)))
                                                           (render:column frame row (string-length (vector-ref lines row))))])
                                               (loop (+ y 1) (if next? row (+ row 1)) (if next? (+ segment 1) 0)
                                                 (if (< y (car range)) out
                                                   (cons (append (list y row start end)
                                                           (or (hashtable-ref cache row #f)
                                                             (let ([value (call-with-values (lambda () (row-presentation data row)) list)])
                                                               (hashtable-set! cache row value) value))
                                                           (list (and (> width 1) (not (render:header frame))
                                                                   (if wrap (and next? (not (clean-wrap? (wrap-setting data d))) #\\)
                                                                     (and (> end (+ start width)) #\$))))) out))))))))))))
  (define (row-presentation data row)
    (let ([surface (render:row (caddr data) row)])
      (if surface (values (car surface) (cadr surface))
        (let* ([source (list-ref data 3)] [mode (list-ref data 4)] [line (vector-ref (mode:source-lines source) row)]
               [replacement (and mode (mode:render mode))] [styler (and mode (mode:row-styles mode))])
          (render:present (caddr data) row line
            (and replacement (guard (ex [else #f]) (replacement source row line)))
            (or (and styler (guard (ex [else #f]) (styler source row line))) ((mode:line-styles mode) line)))))))
  (define (render projection d width height range)
    (if (not (cadr projection)) (if (zero? (car range)) (list "[Selection history unavailable]") '())
      (map (lambda (row)
             (let* ([shown (list-ref row 4)] [text (if (vector? shown) (apply string-append (vector->list shown)) shown)]
                    [edge (list-ref row 6)] [limit (- width (if edge 1 0))]
                    [visible (glyph:slice text (caddr row) (max 0 (min limit (- (cadddr row) (caddr row)))))]
                    [visible (let scan ([i 0] [out #f])
                               (if (= i (string-length visible)) (or out visible)
                                 (let ([n (char->integer (string-ref visible i))])
                                   (if (or (< n 32) (<= 127 n 159))
                                     (let ([out (or out (string-copy visible))]) (string-set! out i #\space) (scan (+ i 1) out))
                                     (scan (+ i 1) out)))))])
               (if edge (string-append (glyph:fit visible limit) (string edge)) visible)))
        (list-ref projection 6))))
  (define (decorate projection d width height range)
    (if (not (cadr projection)) (list (list (list 0 0 width 1) 'ghost))
      (let* ([data (car projection)] [lines (text-control:lines (cadr data))] [frame (caddr data)]
             [selected (and (cadddr (editor-state:state d)) (text-source:span (cadr projection)))]
             [highlights (if (equal? (widget:focused (car data)) (car data))
                           (mode:highlights (list-ref data 3) (list-ref data 4) (caadr projection)) '())])
        (define (paint-range span face r y left)
          (if (<= (car (text:span-start span)) r (car (text:span-end span)))
            (let* ([a (if (= r (car (text:span-start span))) (cdr (text:span-start span)) 0)]
                   [b (if (= r (car (text:span-end span))) (cdr (text:span-end span)) (+ 1 (string-length (vector-ref lines r))))]
                   [a (max 0 (- (render:column frame r a) left))]
                   [b (min width (- (render:column frame r b #t) left))])
              (if (> b a) (list (list (list a y (- b a) 1) face)) '())) '()))
        (apply append
          (map (lambda (row)
                 (let ([y (car row)] [r (cadr row)] [left (caddr row)] [end (min (cadddr row) (+ (caddr row) width))])
                   (let ([styles (list-ref row 5)])
                     (append
                       (if (vector? styles)
                         (let loop ([i left] [out '()])
                           (if (>= i (min end (vector-length styles))) (reverse out)
                             (let* ([style (vector-ref styles i)]
                                    [j (let run ([j (+ i 1)]) (if (and (< j (min end (vector-length styles))) (equal? style (vector-ref styles j))) (run (+ j 1)) j))])
                               (loop j (if (or (not style) (eq? style 'plain)) out (cons (list (list (- i left) y (- j i) 1) style) out)))))) '())
                       (apply append (map (lambda (index)
                                            (apply append (map (lambda (p) (paint-range (car p) (cadr p) r y left)) (row-annotations index r))))
                                       (append (list-ref data 9) (list (list-ref data 5)))))
                       (apply append (map (lambda (p) (paint-range (text:datum->span (car p)) (cadr p) r y left)) highlights))
                       (if selected (paint-range selected 'selection r y left) '())
                       (if (list-ref row 6) (list (list (list (- width 1) y 1 1) 'chrome)) '())))))
            (list-ref projection 6))))))
  (define (caret projection d width height)
    (and (cadr projection)
      (let* ([data (car projection)] [lines (text-control:lines (cadr data))]
             [header (and (list-ref data 8) (render:header (caddr data)))])
        (and (or (not header) (not (caddr header)) (caddr (caddr header)))
          (text-layout:locate lines (caddr data) (layout-width data d width)
            (caddr projection) (cadddr projection) (caadr projection))))))

  (define (links projection d width height range)
    (if (not (cadr projection)) '()
      (let* ([data (car projection)] [frame (caddr data)] [lines (text-control:lines (cadr data))])
        (apply append
          (map (lambda (row)
                 (let* ([y (car row)] [r (cadr row)] [left (caddr row)] [end (min (cadddr row) (+ left width (if (list-ref row 6) -1 0)))]
                        [surface (render:row frame r)]
                        [ranges (append
                                  (map (lambda (link)
                                         (cons (render:column frame r (car link))
                                           (cons (render:column frame r (cadr link) #t) (cddr link))))
                                    (render:detect-links (vector-ref lines r)))
                                  (if surface (caddr surface) '()))])
                   (filter values
                     (map (lambda (link)
                            (let ([a (max left (car link))] [b (min end (cadr link))])
                              (and (< a b) (cons (list (- a left) y (- b a) 1) (cddr link))))) ranges))))
            (list-ref projection 6))))))

  (edoc "Read a prepared editor's logical state, including a followed source cursor and top anchor. A host can retain this state once when leaving follow mode; no geometry or per-frame publication enters the base."
        (frame any "prepared editor frame") (returns any))
  (define (frame-state frame)
    (let* ([g (widget:frame-data frame)] [data (car g)] [d (widget:frame-descriptor frame)])
      (and (cadr g) (list (caadr g) (cadadr g)
                      (text-layout:anchor (text-control:lines (cadr data))
                        (layout-width data d (list-ref g 4)) (caddr g))
                      (and (not (list-ref data 8)) (cadddr (editor-state:state d)))))))

  (edoc "Read a prepared editor display row for host gutters and decorations. Returns (line text rendition first-cell end-cell shown), or false after the source."
        (frame any "editor frame") (row integer "display row") (returns any))
  (define (frame-row frame row)
    (let* ([g (widget:frame-data frame)] [data (car g)]
           [entry (and (cadr g) (assv row (list-ref g 6)))])
      (and entry (list (cadr entry) (vector-ref (text-control:lines (cadr data)) (cadr entry))
                   (caddr data) (caddr entry) (cadddr entry) (list-ref entry 4)))))

  (edoc "Map a logical position into a prepared editor's backend coordinates. The result may lie outside its clip."
        (frame any "editor frame") (position position "logical source position") (returns pair))
  (define (frame-position frame position)
    (let* ([g (widget:frame-data frame)] [data (car g)])
      (text-layout:locate (text-control:lines (cadr data)) (caddr data)
        (layout-width data (widget:frame-descriptor frame) (caddr (widget:frame-rect frame)))
        (caddr g) (cadddr g) position)))

  (edoc "Map backend coordinates through a prepared editor's source and geometry, snapping to a complete grapheme."
        (frame any "editor frame") (x integer "column") (y integer "row") (returns position))
  (define (frame-hit frame x y)
    (let* ([g (widget:frame-data frame)] [data (car g)] [lines (text-control:lines (cadr data))])
      (snap lines (caddr data)
        (text-layout:hit lines (caddr data)
          (layout-width data (widget:frame-descriptor frame) (caddr (widget:frame-rect frame)))
          (caddr g) (cadddr g) (max 0 x) (max 0 y)))))

  (define (geometry id source d reveal?)
    (let* ([f (widget:event-frame)]
           [size (if (and f (equal? id (widget:frame-id f)))
                   (cons (caddr (widget:frame-rect f)) (cadddr (widget:frame-rect f))) (mount-dimensions (mounted id)))])
      (unless size (refuse "Editor motion requires an allocated view"))
      (project (prepare id source '()) d (car size) (cdr size) reveal?)))
  (define (next-state id source d ps marked? reveal?)
    (let* ([m (mounted id)] [frame (render:prepare #f #f (text-control:lines source) (text-control:revision source) '())]
           [ps (map (lambda (p) (snap (text-control:lines source) frame p)) ps)])
      (when (mount-dimensions m)
        (let* ([proposed (descriptor:with d (list (cons 'state (append ps (list marked?))) (cons 'basis (text-control:revision source))))]
               [g (geometry id source proposed reveal?)] [wrap (layout-width (car g) d (list-ref g 4))])
          (set! ps (list (car ps) (cadr ps) (text-layout:anchor (text-control:lines source) wrap (caddr g))))))
      (append ps (list marked?))))
  (define (publish! id source d ps marked? reveal?)
    (unless (text-control:current? id source d) (refuse "The editor view changed"))
    (interaction:set-state! head:ui-actor id (text-control:revision source) (next-state id source d ps marked? reveal?))
    (mount-group-set! (mounted id) #f))

  (edoc "Set an editor view's logical caret and selection anchor, snapping to whole graphemes. An equal pair clears mark activity. This also establishes a fresh selection when retained history is unavailable." (id model "mounted editor view") (caret position "active endpoint") (anchor position "fixed endpoint") (receiver id (view editor)))
  (define (select! id caret anchor)
    (unless (and (position? caret) (position? anchor)) (error 'select! "expected logical text positions"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([ps (points source d)])
        (publish! id source d (list caret anchor (if ps (caddr ps) caret)) (not (equal? caret anchor)) #t)
        (mount-goal-set! (mounted id) #f))))

  (edoc "Move an explicit editor caret by grapheme, displayed row, endpoint or to a logical position. Up/down require an allocated view and retain a head-local display column. Extend preserves the selection anchor." (id model "editor view") (direction (or position (one-of left right up down home end start finish)) "motion or absolute position") (extend (list-of boolean) "optional selection extension") (receiver id (view editor)))
  (define (move! id direction . extend)
    (unless (and (or (position? direction) (memq direction '(left right up down home end start finish))) (<= (length extend) 1) (for-all boolean? extend)) (error 'move! "invalid movement"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([ps (points source d)] [lines (text-control:lines source)] [m (mounted id)] [mark? (if (pair? extend) (car extend) (cadddr (editor-state:state d)))])
        (when (not ps)
          (if (position? direction) (begin (set! ps (list direction direction direction)) (set! mark? #f))
            (refuse "Editor selection history is unavailable; select a fresh position")))
        (let* ([p (car ps)] [span (text-source:span ps)]
               [next (if (position? direction) direction (case direction
                                                           [(left right) (if (and (not mark?) (cadddr (editor-state:state d)) (not (equal? (car ps) (cadr ps))))
                                                                           (if (eq? direction 'left) (text:span-start span) (text:span-end span)) (text-layout:adjacent lines p direction))]
                                                           [(home) (cons (car p) 0)] [(end) (cons (car p) (string-length (vector-ref lines (car p))))]
                                                           [(start) '(0 . 0)] [(finish) (let ([r (- (vector-length lines) 1)]) (cons r (string-length (vector-ref lines r))))]
                                                           [else (let* ([g (geometry id source d #f)] [data (car g)] [frame (caddr data)] [width (list-ref g 4)]
                                                                        [wrap (layout-width data d width)]
                                                                        [goal (or (mount-goal m) (car (text-layout:locate lines frame wrap (caddr g) 0 p)))])
                                                                   (mount-goal-set! m goal)
                                                                   (text-layout:move lines frame wrap p (if (eq? direction 'up) -1 1) goal))]))])
          (publish! id source d (list next (if mark? (cadr ps) next) (caddr ps)) mark? #t)
          (unless (memq direction '(up down)) (mount-goal-set! m #f))))))

  (define (typing-basis d)
    (list (car (keymap:command-state)) (view:generation d) (view:sequence d) (view:basis d)
      (text-source:current-batch head:ui-actor)
      (let loop ([d d])
        (if (view:parent d) (loop (interaction:snapshot (view:parent d)))
          (list (view:generation d) (view:sequence d))))))
  (define (continues? m source d kind)
    (let ([group (mount-group m)] [turn (keymap:command-state)])
      (and group (eq? (car group) kind) (= (or (view:basis d) (text-control:revision source)) (text-control:revision source))
        (or (= (caadr group) (car turn)) (and (cdr turn) (= (+ 1 (caadr group)) (car turn))))
        (equal? (cdadr group) (cdr (typing-basis d))))))

  (define (current-source source)
    (let ([m (text-control:mirror source)])
      (list (cons 'id (text-source:id m)) (cons 'revision (text-source:revision m)) (cons 'value (text-source:lines m)))))

  (edoc "Borrow an explicit editor's immutable text basis for a later bulk rewrite, without remote reads." (id model "editor view") (returns list "(lines document-id revision)") (receiver id (view editor)))
  (define (basis id)
    (let-values ([(source d) (text-control:context id 'editor)])
      (list (text-control:lines source) (text-source:id (text-control:mirror source)) (text-control:revision source))))

  (edoc "Read an explicit editor's logical caret, anchor, top and mark activity at its acquired text basis, without geometry, remote reads or publication. Missing selection history refuses. Returned positions are owned copies; capture with basis before deferred work."
        (id model "editor view") (returns list "(caret anchor top marked?)") (receiver id (view editor)))
  (define (selection id)
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([ps (points source d)])
        (unless ps (refuse "Editor selection history is unavailable"))
        (append (map (lambda (p) (cons (car p) (cdr p))) ps) (list (cadddr (editor-state:state d)))))))

  (edoc "Rewrite ordered disjoint ranges computed against a captured basis, preserving selection and sharing one undo action. Concurrently changed ranges are skipped; missing history or a changed view owner refuses the remaining work." (id model "editor view") (basis list "(immutable-lines document-id revision)") (regions list "(start end replacement-string) entries in basis order") (returns integer "ranges changed") (receiver id (view editor)))
  (define (rewrite-regions! id basis regions)
    (let-values ([(initial original) (text-control:context id 'editor)])
      (let ([document (text-source:id (text-control:mirror initial))])
        (unless (and (list? basis) (= (length basis) 3) (vector? (car basis)) (> (vector-length (car basis)) 0)
                  (equal? document (cadr basis)) (integer? (caddr basis)) (exact? (caddr basis)) (>= (caddr basis) 0)
                  (list? regions)) (error 'rewrite-regions! "expected this editor's basis and ordered ranges"))
        (let ([regions
               (let validate ([rest regions] [end '(0 . 0)])
                 (if (null? rest) '()
                   (let ([r (car rest)])
                     (unless (and (list? r) (= (length r) 3) (position? (car r)) (position? (cadr r)) (string? (caddr r))
                               (text:position<=? end (car r)) (text:position<=? (car r) (cadr r)))
                       (error 'rewrite-regions! "expected ordered disjoint ranges" r))
                     (let ([span (text-source:span r)])
                       (text:extract (car basis) span)
                       (let-values ([(lines trailing?) (text:from-string (caddr r))])
                         (cons (cons span (append (vector->list lines) (if trailing? '("") '())))
                           (validate (cdr rest) (cadr r))))))))])
          (if (null? regions) 0
            (let* ([reload? (document:check! head:ui-actor document)]
                   [context (list (list 'editor head:ui-actor id (gensym->unique-string (gensym))) "Rewrite text")]
                   [count
                    ;; Work backwards: our own edits cannot shift earlier
                    ;; ranges. Only interleaved changes before their end need
                    ;; to traverse the remaining list.
                    (let loop ([regions (reverse regions)] [from (caddr basis)] [count 0])
                      (if (null? regions) count
                        (let-values ([(source d) (text-control:context id 'editor 'current)])
                          (unless (text-control:current? id initial original) (refuse "The editor source or owner changed"))
                          (let* ([revision (text-control:revision source)] [mirror (text-control:mirror source)]
                                 [changes (text-source:changes mirror from revision)])
                            (unless changes (refuse "The rewrite's text history is unavailable"))
                            (let* ([last (caar regions)]
                                   [changes (let skip ([rest changes])
                                              (if (null? rest) '()
                                                (let ([start (text:span-start (text:delta-span (car rest)))] [end (text:span-end last)])
                                                  (if (or (text:position<? end start)
                                                          (and (text:position=? end start) (not (text:span-empty? last))))
                                                    (skip (cdr rest)) rest))))]
                                   [regions (if (null? changes) regions
                                              (filter values (map (lambda (r)
                                                                    (let ([span (fold-left (lambda (s delta) (and s (text:rebase-span s delta))) (car r) changes)])
                                                                      (and span (cons span (cdr r))))) regions)))])
                              (if (null? regions) count
                                (let* ([r (car regions)] [old (text-control:lines source)] [ps (points source d)])
                                  (unless ps (refuse "Editor selection history is unavailable"))
                                  (if (equal? (text:extract old (car r)) (cdr r))
                                    (loop (cdr regions) revision count)
                                    (let-values ([(proposed delta) (text:apply-edit old (car r) (cdr r))])
                                      (mount-group-set! (mounted id) #f) (mount-goal-set! (mounted id) #f)
                                      (text-control:submit! id source d old revision (car r) (cdr r) context
                                        (map (lambda (p) (text:rebase-position p delta)) ps)
                                        (lambda (ps) (next-state id (current-source source) d ps (cadddr (editor-state:state d)) #t)))
                                      (loop (cdr regions) revision (+ count 1)))))))))))])
              (when reload? (document:reload! head:ui-actor document) (text-source:open! head:ui-actor document))
              count))))))
  (define (rewrite! id source d old proposed positions properties label)
    (text-control:call-with-intent! id (lambda ()
                                         (let-values ([(span replacement) (text:difference old proposed)])
                                           (let* ([document (text-source:id (text-control:mirror source))] [m (mounted id)]
                                                  [basis (or (view:basis d) (text-control:revision source))]
                                                  [properties (filter (lambda (p) (not (equal? (store:property document (car p) #f) (cdr p)))) properties)])
                                             (mount-group-set! m #f) (mount-goal-set! m #f)
                                             (if (and (equal? old proposed) (null? properties))
                                               (let ([ps (text-source:rebase positions (text-source:changes (text-control:mirror source) basis (text-control:revision source)))])
                                                 (unless ps (refuse "Editor selection history is unavailable"))
                                                 (unless (= (view:sequence d) (view:sequence (interaction:snapshot id))) (refuse "The editor selection changed"))
                                                 (publish! id source d ps (cadddr (editor-state:state d)) #t))
                                               (let ([reload? (document:check! head:ui-actor document)])
                                                 (text-control:submit! id source d old basis span replacement
                                                   (list (list 'editor head:ui-actor id (gensym->unique-string (gensym))) label (cons 'undo properties)) positions
                                                   (lambda (ps) (next-state id (current-source source) d ps (cadddr (editor-state:state d)) #t)))
                                                 (when reload? (document:reload! head:ui-actor document) (text-source:open! head:ui-actor document)))))))) )
  (define (replace! id source d selection replacement typing? placement . accepted)
    (text-control:call-with-intent! id (lambda ()
                                         (unless (and (equal? (car selection) (cadr selection)) (equal? replacement '("")))
                                           (let* ([m (mounted id)] [old (text-control:basis-text source d)] [basis (or (view:basis d) (text-control:revision source))]
                                                  [old-group (mount-group m)]
                                                  [join? (and typing? (continues? m source d 'typing) (< (list-ref old-group 3) 20))]
                                                  [key (if join? (caddr old-group) (list 'editor head:ui-actor id (gensym->unique-string (gensym))))]
                                                  [removed (string:join (text:extract old (text-source:span selection)) "\n")]
                                                  [inserted (string:join replacement "\n")]
                                                  [count (if join? (+ 1 (list-ref old-group 3)) 1)]
                                                  [left (if join? (list-ref old-group 4) "")]
                                                  [before (if join? (list-ref old-group 5) "")]
                                                  [after (if join? (list-ref old-group 6) "")]
                                                  [erased (if (eq? typing? 'backward) (min (string-length left) (string-length removed)) 0)]
                                                  [left (string-append (substring left 0 (- (string-length left) erased)) inserted)]
                                                  [before (if (eq? typing? 'backward)
                                                            (string-append (substring removed 0 (- (string-length removed) erased)) before) before)]
                                                  [after (if (eq? typing? 'backward) after (string-append after removed))]
                                                  [deleted (string-append before after)]
                                                  [label (cond [(string=? deleted "") (format "insert ~s" left)]
                                                           [(string=? left "") (format "delete ~s" deleted)]
                                                           [else (format "replace ~s with ~s" deleted left)])]
                                                  [document (text-source:id (text-control:mirror source))]
                                                  [reload? (and (not join?) (document:check! head:ui-actor document))])
                                             (mount-group-set! m #f)
                                             (let ([settled? (text-control:submit! id source d old basis (text-source:span selection) replacement (list key label (list 'labels (cons 'batch key)))
                                                               (list placement)
                                                               (lambda (ps)
                                                                 (let* ([mirror (text-control:mirror source)]
                                                                        [top (text-source:rebase (list (caddr (editor-state:state d))) (text-source:changes mirror basis (text-source:revision mirror)))])
                                                                   (next-state id (current-source source) d
                                                                     (list (car ps) (car ps) (if top (car top) (car ps))) #f #t))) #t)])
                                               (mount-goal-set! m #f)
                                               (when (and settled? typing? (text-control:current? id source d))
                                                 (let ([now (interaction:snapshot id)])
                                                   (mount-group-set! m (list 'typing (typing-basis now) key count left before after))))
                                               ;; Clipboard publication follows admission even when a callback has
                                               ;; closed the view or established a newer selection.
                                               (for-each (lambda (callback) (callback settled?)) accepted))
                                             (when reload? (document:reload! head:ui-actor document) (text-source:open! head:ui-actor document)))))) )

  (edoc "Insert multiline text into an explicit editor, replacing its active selection. Consecutive insertions at the unchanged resulting caret share an undo group. Stale or read-only edits refuse through the shared journal." (id model "editor view") (text string "inserted text") (receiver id (view editor)))
  (define (insert! id text)
    (insert-text! id text #t))

  (edoc "Paste text into an explicit editor as one undo action, separate from surrounding typing." (id model "editor view") (text string "inserted text") (receiver id (view editor)))
  (define (paste! id text)
    (insert-text! id text #f))

  (edoc "Insert at the declared caret without replacing a selection. Keep the caret before the insertion when stay? is true; settlement never overwrites a newer interaction." (id model "editor view") (text string "inserted text") (stay? boolean "keep point before the inserted text") (receiver id (view editor)))
  (define (insert-at! id text stay?)
    (unless (and (string? text) (boolean? stay?)) (error 'insert-at! "expected text and a boolean placement"))
    (let-values ([(source d) (text-control:context id 'editor)] [(lines trailing?) (text:from-string text)])
      (let ([p (car (editor-state:state d))])
        (replace! id source d (list p p) (append (vector->list lines) (if trailing? '("") '())) #f (if stay? 'start 'end)))))
  (define (insert-text! id text typing?)
    (unless (string? text) (error 'insert! "expected text"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([s (editor-state:state d)])
        (replace! id source d (if (cadddr s) s (list (car s) (car s)))
          (let loop ([start 0] [end 0] [out '()])
            (cond [(= end (string-length text)) (reverse (cons (substring text start end) out))]
              [(char=? (string-ref text end) #\newline) (loop (+ end 1) (+ end 1) (cons (substring text start end) out))]
              [else (loop start (+ end 1) out)])) (and typing? 'insert) 'end))))

  (edoc "Replace an explicit range of the editor's current mirrored text as one undo action, placing the caret after the replacement. Use basis and rewrite-regions! for ranges computed before other commands." (id model "editor view") (start position "first endpoint") (end position "last endpoint") (text string "replacement") (receiver id (view editor)))
  (define (replace-region! id start end text)
    (unless (and (position? start) (position? end) (string? text)) (error 'replace-region! "invalid replacement"))
    (let-values ([(source d) (text-control:context id 'editor)] [(lines trailing?) (text:from-string text)])
      (let ([ps (points source d)])
        (unless ps (refuse "Editor selection history is unavailable"))
        (replace! id source (descriptor:with d
                              (list (cons 'basis (text-control:revision source))
                                (cons 'state (append ps (list (cadddr (editor-state:state d)))))))
          (list start end) (append (vector->list lines) (if trailing? '("") '())) #f 'end))))

  (edoc "Compute indentation or formatting through the document's mode and admit it against the original source revision. Preserve logical selections through the accepted result; callbacks cannot retarget a newer view." (id model "editor view") (operation (one-of indent-line indent-region indent-buffer indent-expression tab format-region format-buffer) "transformation") (receiver id (view editor)))
  (define (format! id operation)
    (text-control:call-with-intent! id (lambda ()
                                         (let-values ([(source d) (text-control:context id 'editor)])
                                           (let* ([old (text-control:basis-text source d)] [ps (list-head (editor-state:state d) 3)]
                                                  [document (text-source:id (text-control:mirror source))] [name (option d 'mode (store:property document 'mode #f))]
                                                  [mode (and name (mode:find name))]
                                                  [input (mode:source old (map (lambda (key) (cons key (store:property document key #f))) (mode:required-facts mode)))]
                                                  [span (text-source:span ps)] [last (- (vector-length old) 1)])
                                             (when (and (memq operation '(indent-region format-region)) (not (cadddr (editor-state:state d)))) (refuse "The mark is not set"))
                                             (let-values ([(from to)
                                                           (case operation
                                                             [(tab indent-line) (values (caar ps) (caar ps))]
                                                             [(indent-buffer format-buffer) (values 0 last)]
                                                             [(indent-region format-region) (values (car (text:span-start span)) (car (text:span-end span)))]
                                                             [(indent-expression) (let-values ([(a b) (expression:forward old (car ps))])
                                                                                    (unless a (refuse "No expression after the caret")) (values (+ (car a) 1) (car b)))]
                                                             [else (error 'format! "invalid transformation" operation)])])
                                               (when (and (<= from to) (or (not (eq? operation 'tab)) (and name (mode:indent-on-tab? name))))
                                                 (if (memq operation '(format-region format-buffer))
                                                   (let* ([formatter (and name (mode:formatter name))]
                                                          [lines (and formatter (formatter input from to))])
                                                     (unless lines (refuse "No formatter result for these lines"))
                                                     (unless (and (list? lines) (for-all text:line? lines)) (error 'format! "invalid formatter lines" lines))
                                                     (let* ([out (text:splice old from (+ to 1) lines)] [next (if (zero? (vector-length out)) '#("") out)])
                                                       (rewrite! id source d old next ps (if (= to last) '((trailing . #t)) '()) "Format text")))
                                                   (let-values ([(next points) (mode:indent name input from to (and (memq operation '(tab indent-line)) #t) ps)])
                                                     (unless next (refuse "No indenter for this mode"))
                                                     (rewrite! id source d old next points '() "Indent text"))))))))) )

  (edoc "Transfer a selected region or the rest of a line through an explicit clipboard capability. The publisher receives text and whether a preceding kill in this view can accumulate; rejected cuts never publish." (id model "editor view") (operation (one-of copy cut line forward backward) "transfer") (publish procedure "(text accumulate?)") (receiver id (view editor)))
  (define (transfer! id operation publish)
    (unless (memq operation '(copy cut line forward backward)) (error 'transfer! "invalid transfer"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([s (editor-state:state d)] [p (car s)] [old (text-control:basis-text source d)]
             [selection (case operation
                          [(line) (list p (let ([end (string-length (vector-ref old (car p)))])
                                            (if (< (cdr p) end) (cons (car p) end) (text-layout:adjacent old p 'right))))]
                          [(forward backward)
                           (let-values ([(start end) ((if (eq? operation 'forward) expression:forward expression:backward) old p)])
                             (unless start (refuse "No expression in that direction"))
                             (list p (if (eq? operation 'forward) end start)))]
                          [else s])]
             [span (text-source:span selection)] [m (mounted id)] [join? (continues? m source d 'kill)])
        (unless (or (memq operation '(line forward backward)) (cadddr s)) (refuse "The mark is not set"))
        (unless (text:span-empty? span)
          (let ([text (text:to-string (list->vector (text:extract old span)) #f)])
            (if (eq? operation 'copy)
              (let* ([changes (text-source:changes (text-control:mirror source) (or (view:basis d) (text-control:revision source)) (text-control:revision source))]
                     [kept (and changes (fold-left (lambda (span change) (and span (text:rebase-span span change))) span changes))]
                     [ps (points source d)])
                (unless (and kept ps) (refuse "The selected text changed; select it again"))
                (publish! id source d (list (car ps) (car ps) (caddr ps)) #f #f)
                (publish text #f))
              (replace! id source d selection '("") #f 'end
                (lambda (settled?)
                  (let ([after (and settled? (typing-basis (interaction:snapshot id)))])
                    (publish text join?)
                    (when (and after (text-control:current? id source d)
                            (equal? after (typing-basis (interaction:snapshot id))))
                      (mount-group-set! m (list 'kill after #f))))))))))))

  (edoc "Operate on Scheme expressions in an explicit editor, sharing immutable source analysis across views. Motions and marks use current mirrored text; transposition is admitted against its captured revision." (id model "editor view") (operation (one-of forward backward up down next previous start end mark form transpose) "expression operation") (receiver id (view editor)))
  (define (expression! id operation)
    (let-values ([(source d) (text-control:context id 'editor)])
      (if (eq? operation 'transpose)
        (let* ([old (text-control:basis-text source d)] [p (car (editor-state:state d))])
          (let-values ([(as ae) (expression:backward old p)] [(bs be) (expression:forward old p)])
            (unless (and as bs (not (equal? as bs))) (refuse "No two expressions around the caret"))
            (let-values ([(lines trailing?) (text:from-string (string-append (expression:text old bs be)
                                                                (expression:text old ae bs) (expression:text old as ae)))])
              (replace! id source d (list as be) (append (vector->list lines) (if trailing? '("") '())) #f 'end))))
        (let* ([ps (points source d)] [lines (text-control:lines source)] [marked? (cadddr (editor-state:state d))])
          (unless ps (refuse "Editor selection history is unavailable"))
          (let* ([p (car ps)] [anchor (cadr ps)]
                 [from (if (and (eq? operation 'mark) marked? (or (< (car p) (car anchor))
                                                                (and (= (car p) (car anchor)) (< (cdr p) (cdr anchor))))) anchor p)])
            (let-values ([(a b)
                          (case operation
                            [(forward mark) (expression:forward lines from)] [(backward) (expression:backward lines p)]
                            [(up) (expression:container lines p)] [(next) (expression:next-list lines p)]
                            [(previous) (expression:previous-list lines p)] [(form) (expression:top-level lines p)]
                            [(down) (values (expression:down lines p) #f)] [(start) (values (expression:form-start lines p) #f)]
                            [(end) (values (expression:form-end lines p) #f)]
                            [else (error 'expression! "invalid operation" operation)])])
              (unless a (refuse "No expression in that direction"))
              (let ([next (if (memq operation '(forward next)) b a)])
                (publish! id source d
                  (list (if (eq? operation 'mark) p next)
                    (cond [(memq operation '(mark form)) b] [marked? anchor] [else next]) (caddr ps))
                  (or marked? (and (memq operation '(mark form)) #t)) #t)
                (mount-goal-set! (mounted id) #f))))))))

  (edoc "Page an allocated editor by a fraction of its height, retaining mark activity and the desired display column. At an already reached edge, move the caret to that edge." (id model "editor view") (direction integer "negative up, positive down") (fraction integer "positive page divisor") (receiver id (view editor)))
  (define (page! id direction fraction)
    (unless (and (integer? direction) (exact? direction) (not (zero? direction))
              (integer? fraction) (exact? fraction) (> fraction 0)) (error 'page! "invalid page direction or divisor"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([g (geometry id source d #f)] [ps (cadr g)] [lines (text-control:lines source)]
             [frame (caddr (car g))] [wrap (layout-width (car g) d (list-ref g 4))] [m (mounted id)])
        (unless ps (refuse "Editor selection history is unavailable"))
        (let ([goal (or (mount-goal m) (car (text-layout:locate lines frame wrap (caddr g) 0 (car ps))))]
              [marked? (cadddr (editor-state:state d))])
          (let-values ([(top caret) (text-layout:page lines frame wrap 0 (list-ref g 5) (caddr g) goal direction fraction)])
            (publish! id source d (list caret (if marked? (cadr ps) caret) (text-layout:anchor lines wrap top)) marked? #f)
            (mount-goal-set! m goal))))))

  (edoc "Delete an editor's active selection, or an adjacent grapheme or newline." (id model "editor view") (direction (one-of backward forward) "deletion direction") (receiver id (view editor)))
  (define (delete! id direction)
    (unless (memq direction '(backward forward)) (error 'delete! "invalid deletion direction"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([s (editor-state:state d)] [p (car s)] [old (text-control:basis-text source d)])
        (replace! id source d (if (and (cadddr s) (not (equal? p (cadr s)))) s
                                (list p (text-layout:adjacent old p (if (eq? direction 'backward) 'left 'right)))) '("") direction 'end))))

  (edoc "Move an editor's source journal and rebase its view without changing any other selection." (id model "editor view") (direction symbol "undo or redo") (scope any "undo actor scope") (receiver id (view editor)))
  (define (history! id direction scope)
    (let-values ([(source d) (text-control:context id 'editor)])
      (mount-group-set! (mounted id) #f) (mount-goal-set! (mounted id) #f)
      (text-control:history! id source d direction scope (list-head (editor-state:state d) 3)
        (lambda (ps) (list (car ps) (car ps) (caddr ps) #f)))))

  (edoc "Set or clear an editor's mark. Setting it anchors at the current caret; clearing collapses the selection." (id model "editor view") (active boolean "mark activity") (receiver id (view editor)))
  (define (set-mark! id active)
    (unless (boolean? active) (error 'set-mark! "expected a boolean"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let ([ps (points source d)])
        (unless ps (refuse "Editor selection history is unavailable"))
        (publish! id source d (list (car ps) (car ps) (caddr ps)) active #t))))

  (edoc "Scroll an editor viewport by displayed rows without changing its caret or selection. Uses the mounted geometry and persists only a logical top anchor." (id model "editor view") (rows integer "positive down, negative up") (receiver id (view editor)))
  (define (scroll! id rows)
    (unless (and (integer? rows) (exact? rows)) (error 'scroll! "expected displayed rows"))
    (let-values ([(source d) (text-control:context id 'editor)])
      (let* ([g (geometry id source d #f)] [ps (cadr g)] [data (car g)] [lines (text-control:lines source)]
             [width (layout-width (car g) d (list-ref g 4))])
        (unless ps (refuse "Editor selection history is unavailable"))
        (let ([top (text-layout:move lines (caddr data) width (text-layout:anchor lines width (caddr g)) rows 0)])
          (publish! id source d (list (car ps) (cadr ps) top) (cadddr (editor-state:state d)) #f)
          (- rows (text-layout:distance lines width (caddr g) top))))))

  (define (pointer-bindings frame x y)
    (let* ([g (widget:frame-data frame)] [data (car g)] [d (widget:frame-descriptor frame)] [ps (cadr g)])
      (if (not ps) '()
        (let* ([p (frame-hit frame x y)]
               [word (editor-state:word-range (vector-ref (text-control:lines (cadr data)) (car p)) (cdr p))]
               [id (widget:frame-id frame)] [current (points (cadr data) (interaction:snapshot id))])
          (append (list (list '(click primary ()) (keymap:call select! id p p))
                    (list '(wheel up ()) (keymap:call scroll! id -3)) (list '(wheel down ()) (keymap:call scroll! id 3)))
            (if word (list (list '(double-click primary ())
                             (keymap:call select! id (cons (car p) (cdr word)) (cons (car p) (car word))))) '())
            (if current
              (list (list '(click primary (shift)) (keymap:call select! id p (cadr current)))
                (list '(drag primary ()) (keymap:call select! id p (cadr current)))) '()))))))

  (define (status id d active?)
    ;; A display transform can hide the character being edited. Expose that
    ;; character through the ordinary widget status seam, using acquired text.
    (let* ([m (mounted id)] [mode (and (mount-mode m) (car (mount-mode m)))]
           [transform (and active? mode (mode:render mode))]
           [source (and transform (text-source:lookup (view:source d)))]
           [ps (and source (editor-state:points source (text-source:revision source) d))])
      (if (not ps) '()
        (let* ([p (car ps)] [lines (text-source:lines source)] [line (vector-ref lines (car p))]
               [shown (guard (ex [else #f])
                        (transform (mode:source lines (mount-facts m)) (car p) line))]
               [c (and (< (cdr p) (string-length line)) (string-ref line (cdr p)))])
          (if (and c
                (cond [(and (string? shown) (= (string-length shown) (string-length line)))
                       (not (char=? c (string-ref shown (cdr p))))]
                  [(and (vector? shown) (= (vector-length shown) (string-length line)))
                   (not (equal? (string c) (vector-ref shown (cdr p))))]
                  [else #f]))
            (list (cons (format "  src ~c" c) #f)) '())))))
  (define (event! id source d event)
    (case (car event)
      [(text) (if (eq? (caddr event) 'paste)
                (paste! id (string:join (tty:paste-lines (cadr event)) "\n"))
                (insert! id (cadr event))) #t]
      [(cancel) (when (equal? dragging id) (set! dragging #f)) #t]
      [(pointer)
       (cond [(and (eq? (cadr event) 'release) (equal? dragging id)) (set! dragging #f) #t]
         [else (and (eq? (caddr event) 'primary)
                 (or (eq? (cadr event) 'press) (and (eq? (cadr event) 'move) (equal? dragging id)))
                 (let* ([extend? (or (eq? (cadr event) 'move) (memq 'shift (cadddr event)))]
                        [bindings (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5))]
                        [binding (or (and (not extend?) (= (length event) 7) (> (list-ref event 6) 1) (assoc '(double-click primary ()) bindings))
                                   (assoc (if extend? '(click primary (shift)) '(click primary ())) bindings))])
                   (and binding (begin (keymap:run! (cadr binding))
                                  (when (eq? (cadr event) 'press) (set! dragging id) (widget:capture! id)) #t))))])]
      [else #f]))

  (edoc "Install the editor widget definition and explicit command bindings. History and clipboard policy are supplied by the canonical edit API."
        (commands list "named command procedures"))
  (define (register! commands)
    (widget:register! 'editor 1
      (list (cons 'snapshot snapshot) (cons 'prepare prepare) (cons 'viewport viewport) (cons 'render render) (cons 'decorate decorate) (cons 'caret caret)
        (cons 'links links)
        (cons 'service service!) (cons 'release release!) (cons 'focus #t) (cons 'contexts contexts)
        (cons 'event event!) (cons 'pointer-bindings pointer-bindings) (cons 'status status)
        (cons 'actions (append (list (cons 'insert insert!) (cons 'delete delete!) (cons 'select select!) (cons 'move move!) (cons 'scroll scroll!) (cons 'set-mark set-mark!)) commands))))
    (for-each (lambda (b) (keymap:bind-default! 'widget-editor (car b)
                            (if (caddr b) (keymap:call move! widget:target (cadr b) #t) (keymap:call move! widget:target (cadr b)))))
      '(("LEFT" left #f) ("RIGHT" right #f) ("UP" up #f) ("DOWN" down #f)
        ("C-b" left #f) ("C-f" right #f) ("C-p" up #f) ("C-n" down #f)
        ("C-a" home #f) ("C-e" end #f) ("M-<" start #f) ("M->" finish #f)
        ("HOME" home #f) ("END" end #f) ("C-HOME" start #f) ("C-END" finish #f)
        ("S-LEFT" left #t) ("S-RIGHT" right #t) ("S-UP" up #t) ("S-DOWN" down #t)))
    (keymap:bind-default! 'widget-editor "BACKSPACE" (keymap:call delete! widget:target 'backward))
    (keymap:bind-default! 'widget-editor "DELETE" (keymap:call delete! widget:target 'forward))
    (keymap:bind-default! 'widget-editor "C-d" (keymap:call delete! widget:target 'forward))
    (keymap:bind-default! 'widget-editor "RET" (keymap:call paste! widget:target "\n"))
    (keymap:bind-default! 'widget-editor "C-@" (keymap:call set-mark! widget:target #t))
    (keymap:bind-default! 'widget-editor "C-g" (keymap:call set-mark! widget:target #f))
    (keymap:bind-default! 'widget-editor "ESC" (keymap:call set-mark! widget:target #f))
    (for-each (lambda (b) (keymap:bind-default! 'widget-editor (car b)
                            (keymap:call (apply (cdr (assq (cadr b) commands)) (cons widget:target (cddr b))))))
      '(("C-_" undo) ("C-M-_" redo) ("C-k" kill-line) ("C-w" kill-region) ("M-w" copy-region) ("C-y" yank)
        ("PAGEUP" page -1 1) ("PAGEDOWN" page 1 1) ("M-v" page -1 1) ("C-v" page 1 1)
        ("C-M-f" forward-expression) ("C-M-b" backward-expression) ("C-M-u" up-expression) ("C-M-d" down-expression)
        ("C-M-n" next-list) ("C-M-p" previous-list) ("C-M-a" beginning-of-form) ("C-M-e" end-of-form)
        ("C-M-@" mark-expression) ("C-M-h" mark-form) ("C-M-t" transpose-expressions)
        ("C-M-k" kill-expression) ("C-M-BACKSPACE" backward-kill-expression)
        ("TAB" indent-tab) ("C-M-q" indent-expression) ("C-M-\\" indent-region)))))
