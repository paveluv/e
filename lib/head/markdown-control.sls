;; Read-only semantic Markdown, independently fitted by each mounted view.
(import (only (foundation edoc) elibrary))
(elibrary (head markdown-control)
  (export copy! create-view! move! register! scroll! select! set-mark!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation text) text:) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head markdown-layout) markdown-layout:) (prefix (head range) range:)
          (prefix (head render) render:) (prefix (head text-layout) text-layout:)
          (prefix (head text-source) text-source:) (prefix (head widget) widget:)
          (prefix (service markup-source) markup-source:) (prefix (state view) view:))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))
  (define (block row)
    (let ([cell (assq 'block (caddr row))]) (and cell (eq? (cadr cell) 'ready) (caddr cell))))
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define-record-type session
    (fields query token (mutable display) (mutable fitted) (mutable dimensions) (mutable goal)))
  (define sessions (make-hashtable equal-hash equal?))
  (define copy-text! #f)
  (define (release! id)
    (let ([s (hashtable-ref sessions id #f)])
      (when s (range:release! (session-token s)) (hashtable-delete! sessions id))))
  (define (metadata s) (get (range:summary (session-query s)) 'value '()))
  (define (details v k fallback) (get (get v 'details '()) k fallback))
  (define (anchors s d v)
    (let* ([state (view:state d)] [revision (details v 'revision #f)]
           [mirror (and revision (text-source:lookup (details v 'document #f)))]
           [changes (and mirror (text-source:changes mirror (or (view:basis d) revision) revision))])
      (and changes
        (append
          (map (lambda (a)
                 (let ([rows (text-source:rebase (list (cons (car a) 0) (cons (cadr a) 0)) changes)])
                   (cons* (caar rows) (caadr rows) (cddr a)))) (list-head state 3))
          (list (cadddr state))))))
  (define (service! id frame)
    (let* ([d (interaction:snapshot id)] [query (and d (view:source d))]
           [old (hashtable-ref sessions id #f)])
      (when (and old (not (equal? query (session-query old)))) (release! id) (set! old #f))
      (when query
        (let* ([s (or old (let ([s (make-session query (range:acquire! query (lambda () (widget:repaint! id #t))) #f #f '(1 . 1) #f)])
                            (hashtable-set! sessions id s) s))]
               [v (metadata s)] [generation (get v 'generation 0)] [prior (session-display s)])
          (when frame (session-dimensions-set! s (cons (caddr (widget:frame-rect frame)) (cadddr (widget:frame-rect frame)))))
          (cond [(eq? (get v 'status #f) 'ready)
                 (unless (and prior (= (details (car prior) 'revision -1) (details v 'revision -2)))
                   (text-source:open! head:ui-actor (details v 'document #f) (or (view:basis d) (details v 'revision 0))))
                 (let* ([state (anchors s d v)] [key (and state (car (caddr state)))]
                        [rank (and key (range:locate query generation key))]
                        [at (and rank (eq? (car rank) 'ready) (or (list-ref rank 3) 0))]
                        [start (max 0 (- (or at 0) 2))]
                        [count (min 256 (max 32 (* 2 (cdr (session-dimensions s)))))]
                        [page (and at (range:read query generation start count '(block)))])
                   (range:request! (session-token s) generation start (if at count 0) '(block) (if key (list key) '()))
                   (when (and page (eq? (car page) 'ready))
                     (when (and state (view:basis d) (not (equal? (view:basis d) (details v 'revision #f))))
                       (interaction:set-state! head:ui-actor id (details v 'revision #f) state))
                     (when (not (view:basis d))
                       (let* ([first (and (pair? (list-ref page 4)) (car (list-ref page 4)))]
                              [block (and first (block first))]
                              [a (if block (list (cadr block) (cadr block) (if (eq? (car block) 'table) 1 0) 0) '(0 0 0 0))])
                         (interaction:set-state! head:ui-actor id (details v 'revision #f) (list a a a #f))))
                     (let ([next (list v start (list-ref page 4))])
                       (unless (equal? prior next)
                         (session-display-set! s next) (session-fitted-set! s #f) (widget:repaint! id #t)))))]
            [(eq? (get v 'status #f) 'unavailable)
             (when prior (session-display-set! s #f) (session-fitted-set! s #f) (widget:repaint! id #t))])))))

  (edoc "Create an unmounted Markdown view over a borrowed document, with independent selection and scrolling. The base shares interpretation; each head fits its own width."
        (actor actor "creator") (document integer "source document") (returns model) (public))
  (define (create-view! actor document)
    (let ([query (markup-source:create! actor document)])
      (view:create! actor query 'markdown 1 '((name . "<markdown>")) '((0 0 0 0) (0 0 0 0) (0 0 0 0) #f) query)))

  ;; A fitted page owns immutable text, character styles, links and semantic
  ;; anchors. Only this head cache contains terminal widths or rendered rows.
  (define-record-type fit (fields display width lines faces links anchors positions frame))
  (define (fitted s width)
    (let* ([display (session-display s)] [old (session-fitted s)])
      (and display
        (if (and old (= width (fit-width old)) (eq? display (fit-display old))) old
          (let ([lines '()] [faces '()] [links '()] [anchors '()])
            (for-each
              (lambda (row)
                (let-values ([(ls fs us rs ps) (markdown-layout:render (list (block row)) width)])
                  (set! lines (append (reverse ls) lines)) (set! faces (append (reverse fs) faces))
                  (set! links (append (reverse us) links))
                  (set! anchors (append (reverse (map (lambda (v) (vector-map (lambda (p) (cons (cadr row) p)) v)) ps)) anchors))))
              (caddr display))
            (let* ([lines (list->vector (if (null? lines) '("") (reverse lines)))]
                   [faces (list->vector (if (null? faces) '(#()) (reverse faces)))]
                   [links (list->vector (if (null? links) '(()) (reverse links)))]
                   [anchors (list->vector (if (null? anchors) '(#((0 0 0 0))) (reverse anchors)))]
                   [positions (make-hashtable equal-hash equal?)]
                   [f (make-fit display width lines faces links anchors positions (render:prepare #f #f lines 0 '()))])
              (do ([row 0 (+ row 1)]) ((= row (vector-length anchors)))
                (let ([v (vector-ref anchors row)])
                  (do ([col 0 (+ col 1)]) ((= col (vector-length v)))
                    (unless (hashtable-contains? positions (vector-ref v col))
                      (hashtable-set! positions (vector-ref v col) (cons row col))))))
              (session-fitted-set! s f) f))))))
  (define (position f a)
    (or (hashtable-ref (fit-positions f) a #f)
      ;; A source edit may shorten a semantic field. Clamp within that same
      ;; field only; never turn an obsolete anchor into another table cell.
      (let ([best #f] [distance +inf.0])
        (vector-for-each
          (lambda (row)
            (vector-for-each
              (lambda (p)
                (when (and (equal? (list-head a 3) (list-head p 3)) (< (abs (- (cadddr a) (cadddr p))) distance))
                  (set! best (hashtable-ref (fit-positions f) p #f)) (set! distance (abs (- (cadddr a) (cadddr p)))))) row)) (fit-anchors f)) best)))
  (define (anchor f p) (vector-ref (vector-ref (fit-anchors f) (car p)) (cdr p)))
  (define (prepare id source inputs) id)
  (define (viewport id d width height range)
    (let* ([s (hashtable-ref sessions id #f)] [width (max 1 width)] [f (and s (fitted s width))]
           [state (and f (anchors s d (car (fit-display f))))]
           [points (and state (map (lambda (a) (position f a)) (list-head state 3)))]
           [top (and points (caddr points))])
      (when s (session-dimensions-set! s (cons width height)))
      (list id f state points top width height)))
  (define (rows g range)
    (let ([f (cadr g)] [top (list-ref g 4)] [width (list-ref g 5)])
      (if (not top) '()
        (let loop ([row (car top)] [segment (text-layout:segment (text-layout:breaks (vector-ref (fit-lines f) (car top)) width) (cdr top))] [y 0] [out '()])
          (if (or (>= y (+ (car range) (cdr range))) (= row (vector-length (fit-lines f)))) (reverse out)
            (let* ([line (vector-ref (fit-lines f) row)] [breaks (text-layout:breaks line width)]
                   [start (vector-ref breaks segment)] [next? (< (+ segment 1) (vector-length breaks))]
                   [end (if next? (vector-ref breaks (+ segment 1)) (string-length line))])
              (loop (if next? row (+ row 1)) (if next? (+ segment 1) 0) (+ y 1)
                (if (< y (car range)) out (cons (list y row start end) out)))))))))
  (define (render g d width height range)
    (if (not (list-ref g 4)) (if (zero? (car range)) (list (if (cadr g) "[Source anchor unavailable]" "")) '())
      (map (lambda (r) (substring (vector-ref (fit-lines (cadr g)) (cadr r)) (caddr r) (cadddr r))) (rows g range))))
  (define (decorate g d width height range)
    (let ([f (cadr g)] [points (cadddr g)] [state (caddr g)])
      (apply append
        (map (lambda (r)
               (let* ([y (car r)] [row (cadr r)] [start (caddr r)] [end (cadddr r)]
                      [line (vector-ref (fit-lines f) row)] [styles (vector-ref (fit-faces f) row)]
                      [selection (and state (cadddr state) (car points) (cadr points) (text-source:span points))])
                 (let loop ([i start] [out '()])
                   (if (= i end) (reverse out)
                     (let* ([face (if (and selection (not (text:position<? (cons row i) (text:span-start selection)))
                                           (text:position<? (cons row i) (text:span-end selection))) 'selection (vector-ref styles i))]
                            [x (- (render:column (fit-frame f) row i) (render:column (fit-frame f) row start))]
                            [w (- (render:column (fit-frame f) row (+ i 1)) (render:column (fit-frame f) row i))])
                       (loop (+ i 1) (if (or (zero? w) (eq? face 'plain)) out (cons (list (list x y w 1) face) out))))))))
          (rows g range)))))
  (define (caret g d width height)
    (and (list-ref g 4) (car (cadddr g))
      (let* ([f (cadr g)] [top (list-ref g 4)] [w (list-ref g 5)])
        (text-layout:locate (fit-lines f) (fit-frame f) w
          (cons (car top) (text-layout:segment (text-layout:breaks (vector-ref (fit-lines f) (car top)) w) (cdr top))) 0 (car (cadddr g))))))
  (define (geometry id)
    (let-values ([(source d inputs) (widget:context id)])
      (let* ([s (hashtable-ref sessions id #f)] [dimensions (and s (session-dimensions s))]
             [g (and dimensions (viewport id d (car dimensions) (cdr dimensions) (cons 0 (cdr dimensions))))])
        (unless (and g (list-ref g 4)) (refuse "Markdown source is pending or its anchor is unavailable"))
        (unless (and (= (view:sequence d) (view:sequence (interaction:snapshot id)))
                  (= (get (get source 'value '()) 'generation -1) (get (car (fit-display (cadr g))) 'generation -2)))
          (refuse "The displayed Markdown basis changed"))
        (values d s g))))
  (define (publish! id d s g points marked? reveal?)
    (let* ([f (cadr g)] [w (list-ref g 5)] [top (caddr points)])
      (when reveal?
        (let-values ([(p address left) (text-layout:scroll (fit-lines f) (fit-frame f) w w (list-ref g 6) 0
                                         (cons (car top) (text-layout:segment (text-layout:breaks (vector-ref (fit-lines f) (car top)) w) (cdr top)))
                                         0 (car points) 2)])
          (set! top (text-layout:anchor (fit-lines f) w address))))
      (interaction:set-state! head:ui-actor id (details (car (fit-display f)) 'revision #f)
        (list (if (car points) (anchor f (car points)) (car (caddr g)))
          (if (cadr points) (anchor f (cadr points)) (cadr (caddr g))) (anchor f top) marked?))))

  (edoc "Select logical Markdown anchors (block-source-row source-row field character), independent of terminal wrapping."
        (id model "Markdown view") (caret list "active anchor") (fixed list "selection anchor"))
  (define (select! id caret fixed)
    (unless (for-all (lambda (p) (and (list? p) (= (length p) 4)
                                   (for-all (lambda (i) (and (integer? i) (exact? i) (>= i 0))) p))) (list caret fixed))
      (error 'select! "expected semantic Markdown anchors"))
    (let-values ([(d s g) (geometry id)])
      (let ([a (position (cadr g) caret)] [b (position (cadr g) fixed)])
        (unless (and a b) (refuse "Markdown selection is outside the acquired page"))
        (publish! id d s g (list a b (list-ref g 4)) (not (equal? a b)) #t)
        (session-goal-set! s #f))))

  (edoc "Move a Markdown caret in the mounted geometry; only semantic character anchors are published."
        (id model "Markdown view") (direction (one-of up down left right home end) "motion")
        (extend (list-of boolean) "optional selection extension"))
  (define (move! id direction . extend)
    (unless (and (memq direction '(up down left right home end)) (<= (length extend) 1) (for-all boolean? extend))
      (error 'move! "invalid Markdown motion"))
    (let-values ([(d s g) (geometry id)])
      (unless (car (cadddr g)) (refuse "Markdown caret is outside the acquired page"))
      (let* ([f (cadr g)] [lines (fit-lines f)] [p (car (cadddr g))] [row (car p)] [col (cdr p)]
             [w (list-ref g 5)] [n (string-length (vector-ref lines row))]
             [goal (or (session-goal s) (car (caret g d w (list-ref g 6))))]
             [next (case direction
                     [(up down) (text-layout:move lines (fit-frame f) w p (if (eq? direction 'up) -1 1) goal)]
                     [(home) (cons row 0)] [(end) (cons row n)]
                     [(left right) (text-layout:adjacent lines p direction)])]
             [marked? (if (pair? extend) (car extend) (cadddr (caddr g)))])
        (publish! id d s g (list next (if marked? (cadr (cadddr g)) next) (list-ref g 4)) marked? #t)
        (session-goal-set! s (and (memq direction '(up down)) goal)))))

  (edoc "Scroll Markdown by displayed rows, retaining caret and selection."
        (id model "Markdown view") (delta integer "positive down") (returns integer "unconsumed rows"))
  (define (scroll! id delta)
    (let-values ([(d s g) (geometry id)])
      (let* ([f (cadr g)] [lines (fit-lines f)] [top (list-ref g 4)] [w (list-ref g 5)]
             [next (text-layout:move lines (fit-frame f) w top delta 0)]
             [address (cons (car top) (text-layout:segment (text-layout:breaks (vector-ref lines (car top)) w) (cdr top)))])
        (publish! id d s g (list (car (cadddr g)) (cadr (cadddr g)) next) (cadddr (caddr g)) #f)
        (- delta (text-layout:distance lines w address next)))))

  (edoc "Set or clear the Markdown selection mark." (id model "Markdown view") (active boolean "mark activity"))
  (define (set-mark! id active)
    (unless (boolean? active) (error 'set-mark! "expected boolean"))
    (let-values ([(d s g) (geometry id)])
      (unless (car (cadddr g)) (refuse "Markdown caret is outside the acquired page"))
      (publish! id d s g (list (car (cadddr g)) (car (cadddr g)) (list-ref g 4)) active #f)))

  (edoc "Copy the selected displayed Markdown text at its acquired basis. Source markup is unchanged."
        (id model "Markdown view"))
  (define (copy! id)
    (let-values ([(d s g) (geometry id)])
      (unless (and (cadddr (caddr g)) (cadr (cadddr g))) (refuse "No Markdown selection"))
      (copy-text! (text:to-string (list->vector (text:extract (fit-lines (cadr g)) (text-source:span (cadddr g)))) #f))))
  (define (pointer-bindings frame x y)
    (let* ([g (widget:frame-data frame)] [r (assv y (rows g (cons y 1)))])
      (if (not r) '()
        (let* ([f (cadr g)] [line (vector-ref (fit-lines f) (cadr r))]
               [col (min (cadddr r) (render:character (fit-frame f) (cadr r)
                                      (+ (render:column (fit-frame f) (cadr r) (caddr r)) x)))]
               [p (anchor f (cons (cadr r) col))])
          (list (list '(click primary ()) (keymap:call select! (widget:frame-id frame) p p)))))))
  (define (event! id source d event)
    (and (eq? (car event) 'pointer) (eq? (cadr event) 'press) (eq? (caddr event) 'primary)
      (let ([bindings (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5))])
        (and (pair? bindings) (begin (keymap:run! (cadar bindings)) #t)))))

  (edoc "Register the Markdown control and its named keyboard/pointer operations."
        (copy procedure "head clipboard command"))
  (define (register! copy)
    (set! copy-text! copy)
    (widget:register! 'markdown 1
      (list (cons 'prepare prepare) (cons 'viewport viewport) (cons 'render render) (cons 'decorate decorate) (cons 'caret caret)
        (cons 'service service!) (cons 'release release!) (cons 'focus #t) '(contexts . (widget-markdown))
        (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'actions (list (cons 'scroll scroll!) (cons 'move move!) (cons 'select select!) (cons 'copy copy!)))))
    (for-each (lambda (binding) (keymap:bind-default! 'widget-markdown (car binding) (keymap:call move! widget:target (cadr binding))))
      '(("UP" up) ("DOWN" down) ("LEFT" left) ("RIGHT" right) ("HOME" home) ("END" end)
        ("C-p" up) ("C-n" down) ("C-b" left) ("C-f" right) ("C-a" home) ("C-e" end)))
    (for-each (lambda (binding) (keymap:bind-default! 'widget-markdown (car binding) (keymap:call move! widget:target (cadr binding) #t)))
      '(("S-UP" up) ("S-DOWN" down) ("S-LEFT" left) ("S-RIGHT" right)))
    (keymap:bind-default! 'widget-markdown "C-@" (keymap:call set-mark! widget:target #t))
    (keymap:bind-default! 'widget-markdown "C-g" (keymap:call set-mark! widget:target #f))
    (keymap:bind-default! 'widget-markdown "M-w" (keymap:call copy! widget:target))))
