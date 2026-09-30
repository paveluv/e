;; Read-only semantic Markdown, independently fitted by each mounted view.
(import (only (foundation edoc) elibrary))
(elibrary (head markdown-control)
  (export copy! create-view! follow! locate! move! register! scroll! select! set-mark! source! width-limit)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation text) text:) (prefix (head head) head:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (head markdown-layout) markdown-layout:) (prefix (head range) range:)
          (prefix (head render) render:) (prefix (head text-layout) text-layout:)
          (prefix (head text-source) text-source:) (prefix (head widget) widget:)
          (prefix (service log) log:)
          (prefix (service markup-source) markup-source:) (prefix (state view) view:))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))
  (define (block row)
    (let ([cell (assq 'block (caddr row))])
      (if (and cell (eq? (cadr cell) 'ready)) (caddr cell)
        (let ([message "[Markdown block unavailable; C-c v opens source]"])
          (list 'line (cadr row) (list message (list (list 0 (string-length message) 'ghost)) '()))))))
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define-record-type session
    (fields query token (mutable display) (mutable fitted) (mutable dimensions) (mutable goal) (mutable intent) (mutable hover) (mutable copy) (mutable diagnostic)))
  (define-record-type copying
    (fields token generation sequence width anchors (mutable ordinal) (mutable chunks)))
  (define sessions (make-hashtable equal-hash equal?))
  (define deferred (make-hashtable equal-hash equal?))
  (define copy-text! #f)
  (define dragging #f)

  (edoc "Maximum terminal reading width for Markdown presentations." (value integer))
  (define width-limit (make-parameter 80 (lambda (n)
                                           (unless (and (fixnum? n) (>= n 20)) (error 'width-limit "expected at least 20 columns")) n)))
  (define (cancel-copy! s)
    (when (session-copy s) (range:release! (copying-token (session-copy s))) (session-copy-set! s #f)))
  (define (release! id)
    (hashtable-delete! deferred id)
    (when (equal? dragging id) (set! dragging #f))
    (let ([s (hashtable-ref sessions id #f)])
      (when s (cancel-copy! s) (range:release! (session-token s)) (hashtable-delete! sessions id))))
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
        (let* ([s (or old (let ([s (make-session query (range:acquire! query (lambda () (widget:repaint! id #t))) #f #f '(1 . 1) #f (hashtable-ref deferred id #f) #f #f #f)])
                            (hashtable-delete! deferred id) (hashtable-set! sessions id s) s))]
               [v (metadata s)] [generation (get v 'generation 0)] [prior (session-display s)])
          (when (and (session-intent s) (not (= (cadr (session-intent s)) (view:sequence d))))
            (session-intent-set! s #f))
          (when frame (session-dimensions-set! s (cons (caddr (widget:frame-rect frame)) (cadddr (widget:frame-rect frame)))))
          (cond [(eq? (get v 'status #f) 'ready)
                 (unless (and prior (= (details (car prior) 'revision -1) (details v 'revision -2)))
                   (text-source:open! head:ui-actor (details v 'document #f) (or (view:basis d) (details v 'revision 0))))
                 (let* ([state (anchors s d v)] [intent (session-intent s)]
                        [key (if (and intent (eq? (car intent) 'source-row)) (list 'source-row (caddr intent))
                                 (and state (if (view:basis d)
                                              (car (if (and intent (eq? (car intent) 'move)) (car state) (caddr state)))
                                              (list 'source-row (caar state)))))]
                        [rank (and key (range:locate query generation key))]
                        [at (cond [(and intent (eq? (car intent) 'start)) 0]
                              [(and intent (eq? (car intent) 'finish)) (max 0 (- (get v 'count 0) 1))]
                              [else (and rank (eq? (car rank) 'ready) (or (list-ref rank 3) 0))])]
                        [count (min 256 (max 32 (* 2 (cdr (session-dimensions s)))))]
                        [start (max 0 (- (or at 0) (div count 2)))]
                        [page (and at (range:read query generation start count '(block)))])
                   (range:request! (session-token s) generation start (if at count 0) '(block) (if key (list key) '()))
                   (when (and page (eq? (car page) 'unavailable) (not (session-diagnostic s)))
                     (session-diagnostic-set! s "[Markdown range unavailable; C-c v opens source]") (widget:repaint! id #t))
                   (when (and page (eq? (car page) 'ready))
                     (when (session-diagnostic s) (session-diagnostic-set! s #f) (widget:repaint! id #t))
                     (when (and state (view:basis d) (not (equal? (view:basis d) (details v 'revision #f))))
                       (interaction:set-state! head:ui-actor id (details v 'revision #f) state))
                     (let ([next (list v start (list-ref page 4))])
                       (unless (equal? prior next)
                         (session-display-set! s next) (session-fitted-set! s #f) (widget:repaint! id #t))
                       (unless (view:basis d)
                         (let* ([f (fitted s (max 1 (car (session-dimensions s))))]
                                [a (source-anchor f (caar state))])
                           (interaction:set-state! head:ui-actor id (details v 'revision #f) (list a a a #f))))
                       (when intent
                         (session-intent-set! s #f)
                         (if (memq (car intent) '(move scroll))
                           (let* ([d (interaction:snapshot id)] [size (session-dimensions s)]
                                  [g (viewport id d (car size) (cdr size) (cons 0 (cdr size)))])
                             (when (caddr g) (perform! id d s g (car intent) (caddr intent))))
                           (let* ([f (fitted s (max 1 (car (session-dimensions s))))]
                                  [line (if (eq? (car intent) 'finish) (- (vector-length (fit-lines f)) 1) 0)]
                                  [at (cons line (if (eq? (car intent) 'finish) (string-length (vector-ref (fit-lines f) line)) 0))]
                                  [p (case (car intent) [(source-row) (source-anchor f (caddr intent))] [else (anchor f at)])]
                                  [top (if (eq? (car intent) 'finish)
                                         (anchor f (text-layout:move (fit-lines f) (fit-frame f) (fit-width f) at
                                                     (- 1 (max 1 (cdr (session-dimensions s)))) 0)) p)])
                             (interaction:set-state! head:ui-actor id (details v 'revision #f)
                               (cond [(eq? (car intent) 'source-row) (list p p p #f)]
                                 [(and state (caddr intent)) (list p (cadr state) top #t)] [else (list p p top #f)]))
                             (widget:repaint! id #t)))))))]
            [(eq? (get v 'status #f) 'unavailable)
             (unless (session-diagnostic s) (session-diagnostic-set! s "[Markdown source unavailable]") (widget:repaint! id #t))
             (when prior (session-display-set! s #f) (session-fitted-set! s #f) (widget:repaint! id #t))])
          (when (session-copy s) (service-copy! id s))))))

  (edoc "Create an unmounted Markdown view over a borrowed document, with independent selection and scrolling. The base shares interpretation; each head fits its own width."
        (actor actor "creator") (document integer "source document")
        (origin (list-of integer) "optional initial source row") (returns model) (public))
  (define (create-view! actor document . origin)
    (unless (and (<= (length origin) 1) (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) origin))
      (error 'create-view! "expected at most one source row"))
    (let* ([query (markup-source:create! actor document)] [row (if (pair? origin) (car origin) 0)] [a (list row row 0 0)])
      (view:create! actor query 'markdown 1 '((name . "<markdown>")) (list a a a #f) query)))

  ;; A fitted page owns immutable text, character styles, links and semantic
  ;; anchors. Only this head cache contains terminal widths or rendered rows.
  (define-record-type fit (fields display width lines faces links anchors positions frame))
  (define (fitted s width)
    (let* ([width (min width (width-limit))] [display (session-display s)] [old (session-fitted s)])
      (and display
        (if (and old (= width (fit-width old)) (eq? display (fit-display old))) old
          (let ([f (fit-page display width)]) (session-fitted-set! s f) f)))))
  (define (fit-page display width)
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
        f)))
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
  (define (source-anchor f row)
    (let ([best (anchor f '(0 . 0))] [distance +inf.0])
      (vector-for-each (lambda (line)
                         (vector-for-each (lambda (p)
                                            (when (< (abs (- row (cadr p))) distance)
                                              (set! best p) (set! distance (abs (- row (cadr p)))))) line)) (fit-anchors f)) best))
  (define (last-point f)
    (let ([row (- (vector-length (fit-lines f)) 1)]) (cons row (string-length (vector-ref (fit-lines f) row)))))
  (define (selection-points g)
    (let ([f (cadr g)] [state (caddr g)])
      (and state (cadddr state)
        (let ([points (map (lambda (a p)
                             (or p (cond [(< (car a) (car (anchor f '(0 . 0)))) '(0 . 0)]
                                     [(> (car a) (car (anchor f (last-point f)))) (last-point f)] [else #f])))
                        (list-head state 2) (list-head (cadddr g) 2))])
          (and (for-all values points) points)))))
  (define (prepare id source inputs) id)
  (define (viewport id d width height range)
    (let* ([s (hashtable-ref sessions id #f)] [width (min (width-limit) (max 1 width))] [f (and s (fitted s width))]
           [state (and f (not (session-diagnostic s)) (anchors s d (car (fit-display f))))]
           [points (and state (map (lambda (a) (position f a)) (list-head state 3)))]
           [top (and points (caddr points))])
      (when s (session-dimensions-set! s (cons width height)))
      (list id f state points top width height (and s (session-diagnostic s)))))
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
    (if (not (list-ref g 4)) (if (zero? (car range)) (list (or (list-ref g 7) (and (cadr g) "[Source anchor unavailable]") "")) '())
      (map (lambda (r) (substring (vector-ref (fit-lines (cadr g)) (cadr r)) (caddr r) (cadddr r))) (rows g range))))
  (define (decorate g d width height range)
    (if (not (list-ref g 4)) (list (list (list 0 0 width 1) 'ghost))
      (let* ([f (cadr g)] [points (cadddr g)] [state (caddr g)]
             [s (hashtable-ref sessions (car g) #f)] [hover (and s (session-hover s))])
        (apply append
          (map (lambda (r)
                 (let* ([y (car r)] [row (cadr r)] [start (caddr r)] [end (cadddr r)]
                        [line (vector-ref (fit-lines f) row)] [styles (vector-ref (fit-faces f) row)]
                        [selection (let ([points (selection-points g)]) (and points (text-source:span points)))])
                   (let loop ([i start] [out '()])
                     (if (= i end) (reverse out)
                       (let* ([face (if (and selection (not (text:position<? (cons row i) (text:span-start selection)))
                                          (text:position<? (cons row i) (text:span-end selection))) 'selection
                                      (if (and hover (eq? (car hover) f) (= (cadr hover) row)
                                            (<= (caaddr hover) i (- (cadr (caddr hover)) 1)))
                                        '(md-link hover) (vector-ref styles i)))]
                              [x (- (render:column (fit-frame f) row i) (render:column (fit-frame f) row start))]
                              [w (- (render:column (fit-frame f) row (+ i 1)) (render:column (fit-frame f) row i))])
                         (loop (+ i 1) (if (or (zero? w) (eq? face 'plain)) out (cons (list (list x y w 1) face) out))))))))
            (rows g range))))))
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
  (define (publish! id d s g points marked? reveal? . fixed)
    (let* ([f (cadr g)] [w (list-ref g 5)] [top (caddr points)])
      (when reveal?
        (let-values ([(p address left) (text-layout:scroll (fit-lines f) (fit-frame f) w w (list-ref g 6) 0
                                         (cons (car top) (text-layout:segment (text-layout:breaks (vector-ref (fit-lines f) (car top)) w) (cdr top)))
                                         0 (car points) 2)])
          (set! top (text-layout:anchor (fit-lines f) w address))))
      (interaction:set-state! head:ui-actor id (details (car (fit-display f)) 'revision #f)
        (list (if (car points) (anchor f (car points)) (car (caddr g)))
          (if (pair? fixed) (car fixed) (if (cadr points) (anchor f (cadr points)) (cadr (caddr g)))) (anchor f top) marked?))))

  (edoc "Select logical Markdown anchors (block-source-row source-row field character), independent of terminal wrapping."
        (id model "Markdown view") (caret list "active anchor") (fixed list "selection anchor"))
  (define (select! id caret fixed)
    (unless (for-all (lambda (p) (and (list? p) (= (length p) 4)
                                   (for-all (lambda (i) (and (integer? i) (exact? i) (>= i 0))) p))) (list caret fixed))
      (error 'select! "expected semantic Markdown anchors"))
    (let-values ([(d s g) (geometry id)])
      (let ([a (position (cadr g) caret)] [b (position (cadr g) fixed)])
        (unless a (refuse "Markdown caret is outside the acquired page"))
        (publish! id d s g (list a b (list-ref g 4)) (not (equal? a b)) #t fixed)
        (session-goal-set! s #f))))

  (define (queue! id s kind arguments)
    (session-intent-set! s (list kind (view:sequence (interaction:snapshot id)) arguments))
    (head:wake-main!))
  (define (distance f p next)
    (text-layout:distance (fit-lines f) (fit-width f)
      (cons (car p) (text-layout:segment (text-layout:breaks (vector-ref (fit-lines f) (car p)) (fit-width f)) (cdr p))) next))
  (define (more? f delta)
    (let ([display (fit-display f)])
      (if (negative? delta) (> (cadr display) 0)
        (< (+ (cadr display) (length (caddr display))) (get (car display) 'count 0)))))
  ;; Consume only the acquired geometry. Remaining motion is a single local
  ;; intent, fenced by the interaction sequence, rather than a blocked input loop.
  (define (perform! id d s g kind arguments)
    (let* ([f (cadr g)] [lines (fit-lines f)] [w (fit-width f)]
           [mode (car arguments)] [delta (cadr arguments)] [marked? (caddr arguments)]
           [p (if (eq? kind 'scroll) (list-ref g 4) (car (cadddr g)))])
      (if (not p) (begin (queue! id s kind arguments) 0)
        (let* ([row (car p)] [col (cdr p)]
               [start (vector-ref (text-layout:breaks (vector-ref lines row) w)
                        (text-layout:segment (text-layout:breaks (vector-ref lines row) w) col))]
               [goal (or (cadddr arguments) (- (render:column (fit-frame f) row col) (render:column (fit-frame f) row start)))]
               [remaining delta]
               [next (case mode
                       [(rows) (let ([next (text-layout:move lines (fit-frame f) w p delta goal)])
                                 (set! remaining (- delta (distance f p next))) next)]
                       [(home) (cons row 0)] [(end) (cons row (string-length (vector-ref lines row)))]
                       [(characters)
                        (let walk ([p p])
                          (if (zero? remaining) p
                            (let skip ([at p])
                              (let ([next (text-layout:adjacent lines at (if (negative? delta) 'left 'right))])
                                (cond [(equal? next at) p]
                                  [(equal? (anchor f next) (anchor f p)) (skip next)]
                                  [else (set! remaining (+ remaining (if (negative? delta) 1 -1))) (walk next)])))))])])
          (if (eq? kind 'scroll)
            (publish! id d s g (list (car (cadddr g)) (cadr (cadddr g)) next) (cadddr (caddr g)) #f)
            (begin
              (publish! id d s g (list next (if marked? (cadr (cadddr g)) next) (or (list-ref g 4) p)) marked? #t)
              (session-goal-set! s (and (eq? mode 'rows) goal))))
          (if (and (not (zero? remaining)) (more? f remaining))
            (begin (queue! id s kind (list mode remaining marked? goal)) 0)
            remaining)))))
  (define (accumulate s d kind arguments)
    (let ([pending (session-intent s)])
      (if (and pending (= (cadr pending) (view:sequence d)) (eq? (car pending) kind)
            (memq (car arguments) '(rows characters))
            (eq? (car arguments) (caaddr pending)) (eq? (caddr arguments) (caddr (caddr pending))))
        (list (car arguments) (+ (cadr arguments) (cadr (caddr pending))) (caddr arguments) (cadddr (caddr pending)))
        arguments)))

  (edoc "Move a Markdown caret in the mounted geometry; only semantic character anchors are published."
        (id model "Markdown view") (direction (one-of up down left right home end start finish page-up page-down) "motion")
        (extend (list-of boolean) "optional selection extension"))
  (define (move! id direction . extend)
    (unless (and (memq direction '(up down left right home end start finish page-up page-down)) (<= (length extend) 1) (for-all boolean? extend))
      (error 'move! "invalid Markdown motion"))
    (if (memq direction '(start finish))
      (let* ([s (hashtable-ref sessions id #f)] [d (interaction:snapshot id)])
        (unless s (refuse "Markdown view is not mounted"))
        (session-intent-set! s (list direction (view:sequence d) (if (pair? extend) (car extend) (cadddr (view:state d))))) (head:wake-main!))
      (let-values ([(d s g) (geometry id)])
        (let* ([mode (case direction [(up down page-up page-down) 'rows] [(left right) 'characters] [else direction])]
               [delta (* (case direction [(up left page-up) -1] [(down right page-down) 1] [else 0])
                        (if (memq direction '(page-up page-down)) (max 1 (list-ref g 6)) 1))]
               [arguments (accumulate s d 'move
                            (list mode delta (if (pair? extend) (car extend) (cadddr (caddr g))) (session-goal s)))])
          (session-intent-set! s #f)
          (perform! id d s g 'move arguments)))))

  (edoc "Reveal the semantic content nearest a source row; a later interaction cancels pending acquisition."
        (id model "Markdown view") (row integer "zero-based source row"))
  (define (locate! id row)
    (unless (and (integer? row) (exact? row) (>= row 0)) (error 'locate! "expected a source row"))
    (let* ([s (hashtable-ref sessions id #f)] [d (interaction:snapshot id)])
      (widget:host id)
      (unless d (refuse "Markdown view is not mounted"))
      (let ([intent (list 'source-row (view:sequence d) row)])
        (if s (session-intent-set! s intent) (hashtable-set! deferred id intent))) (head:wake-main!)))

  (edoc "Scroll Markdown by displayed rows, retaining caret and selection."
        (id model "Markdown view") (delta integer "positive down") (returns integer "unconsumed rows"))
  (define (scroll! id delta)
    (unless (and (integer? delta) (exact? delta)) (error 'scroll! "expected an integer row count"))
    (let-values ([(d s g) (geometry id)])
      (let ([arguments (accumulate s d 'scroll (list 'rows delta #f 0))])
        (session-intent-set! s #f)
        (perform! id d s g 'scroll arguments))))

  (edoc "Set or clear the Markdown selection mark." (id model "Markdown view") (active boolean "mark activity"))
  (define (set-mark! id active)
    (unless (boolean? active) (error 'set-mark! "expected boolean"))
    (let-values ([(d s g) (geometry id)])
      (unless (car (cadddr g)) (refuse "Markdown caret is outside the acquired page"))
      (publish! id d s g (list (car (cadddr g)) (car (cadddr g)) (list-ref g 4)) active #f)))

  (edoc "Copy selected displayed Markdown text. Off-page selections acquire bounded pages asynchronously; changed source or selection cancels the copy. Source markup is unchanged."
        (id model "Markdown view"))
  (define (copy! id)
    (let-values ([(d s g) (geometry id)])
      (unless (cadddr (caddr g)) (refuse "No Markdown selection"))
      (cancel-copy! s)
      (if (and (car (cadddr g)) (cadr (cadddr g)))
        (copy-text! (text:to-string (list->vector (text:extract (fit-lines (cadr g)) (text-source:span (cadddr g)))) #f))
        (begin
          (session-copy-set! s
            (make-copying (range:acquire! (session-query s) (lambda () (head:wake-main!)))
              (get (car (fit-display (cadr g))) 'generation 0) (view:sequence d) (fit-width (cadr g))
              (list-sort (lambda (a b) (< (car a) (car b))) (list-head (caddr g) 2)) #f '()))
          (head:wake-main!)))))
  (define (service-copy! id s)
    (let* ([job (session-copy s)] [v (metadata s)] [query (session-query s)]
           [generation (copying-generation job)] [ends (copying-anchors job)])
      (if (or (not (= (get v 'generation -1) generation))
              (not (= (copying-sequence job) (view:sequence (interaction:snapshot id)))))
        (cancel-copy! s)
        (guard (ex [else (cancel-copy! s) (log:add! 'markdown-control:service-copy! (kernel:condition-text ex))])
          (let* ([ranks (map (lambda (a) (range:locate query generation (car a))) ends)]
                 [ready? (for-all (lambda (r) (eq? (car r) 'ready)) ranks)]
                 [ordinals (and ready? (map (lambda (r) (list-ref r 3)) ranks))]
                 [at (or (copying-ordinal job) (and ordinals (car ordinals)))]
                 [count (if (and at ordinals (cadr ordinals)) (min 16 (+ 1 (- (cadr ordinals) at))) 0)])
            (when (and ready? (not (for-all values ordinals))) (refuse "Markdown selection no longer exists"))
            (range:request! (copying-token job) generation (or at 0) count '(block) (map car ends))
            (when (> count 0)
              (let ([page (range:read query generation at count '(block))])
                (case (car page)
                  [(unavailable) (refuse "Markdown selection is unavailable")]
                  [(ready)
                   (let* ([f (fit-page (list v at (list-ref page 4)) (copying-width job))]
                          [first (if (= at (car ordinals)) (position f (car ends)) '(0 . 0))]
                          [last (if (> (+ at count) (cadr ordinals)) (position f (cadr ends)) (last-point f))])
                     (unless (and first last) (refuse "Markdown selection anchor is unavailable"))
                     (copying-chunks-set! job (cons (text:to-string (list->vector (text:extract (fit-lines f) (text:make-span (car first) (cdr first) (car last) (cdr last)))) #f)
                                                (copying-chunks job)))
                     (if (> (+ at count) (cadr ordinals))
                       (let ([text (text:to-string (list->vector (reverse (copying-chunks job))) #f)])
                         (cancel-copy! s) (copy-text! text))
                       (begin (copying-ordinal-set! job (+ at count)) (head:wake-main!))))]))))))))

  (edoc "Ask the host to open the underlying source document at the shown source row. Pass document, revision and logical text position to open-source."
        (id model "Markdown view"))
  (define (source! id)
    (let-values ([(source d inputs) (widget:context id)])
      (let* ([v (get source 'value '())] [document (details v 'document #f)])
        (unless (and document (= (view:sequence d) (view:sequence (interaction:snapshot id))))
          (refuse "The Markdown source basis is unavailable"))
        (widget:invoke! id 'open-source document (or (view:basis d) (details v 'revision #f))
          (cons (cadar (view:state d)) 0)))))
  (define (link-at f p)
    (and p (find (lambda (link) (<= (car link) (cdr p) (- (cadr link) 1))) (vector-ref (fit-links f) (car p)))))

  (edoc "Follow a Markdown link through the host's open-uri command, passing the source document and URI. Without an explicit URI, use the caret's link."
        (id model "Markdown view") (uri (list-of string) "optional displayed URI"))
  (define (follow! id . uri)
    (unless (and (<= (length uri) 1) (for-all string? uri)) (error 'follow! "expected at most one URI"))
    (let-values ([(d s g) (geometry id)])
      (let* ([f (cadr g)] [link (and (null? uri) (link-at f (car (cadddr g))))]
             [target (if (pair? uri) (car uri) (and link (caddr link)))])
        (unless target (refuse "No Markdown link at the caret"))
        (widget:invoke! id 'open-uri (details (car (fit-display f)) 'document #f) target))))
  (define (pointer-point frame x y)
    (let* ([g (widget:frame-data frame)] [r (assv y (rows g (cons y 1)))])
      (and r
        (let* ([f (cadr g)] [line (vector-ref (fit-lines f) (cadr r))]
               [col (min (cadddr r) (render:character (fit-frame f) (cadr r)
                                      (+ (render:column (fit-frame f) (cadr r) (caddr r)) x)))])
          (cons (cadr r) col)))))
  (define (pointer-bindings frame x y)
    (let* ([g (widget:frame-data frame)] [point (pointer-point frame x y)])
      (if (not point) '()
        (let* ([f (cadr g)] [p (anchor f point)] [id (widget:frame-id frame)]
               [link (link-at f point)] [fixed (cadr (caddr g))])
          (append
            (list (list '(click primary ()) (if link (keymap:call follow! id (caddr link)) (keymap:call select! id p p))))
            (list (list '(click primary (shift)) (keymap:call select! id p fixed))
              (list '(drag primary ()) (keymap:call select! id p fixed))))))))
  (define (event! id source d event)
    (let ([s (hashtable-ref sessions id #f)])
      (case (car event)
        [(cancel blur) (when (equal? dragging id) (set! dragging #f))
         (when s (session-hover-set! s #f) (widget:repaint! id #t)) #t]
        [(pointer)
         (let* ([frame (widget:event-frame)] [f (cadr (widget:frame-data frame))]
                [p (and (not (eq? (cadr event) 'leave)) (pointer-point frame (list-ref event 4) (list-ref event 5)))]
                [link (and p (link-at f p))] [hover (and link (list f (car p) link))])
           (when (and s (not (equal? hover (session-hover s))))
             (session-hover-set! s hover) (widget:repaint! id #t))
           (cond [(and (eq? (cadr event) 'release) (equal? dragging id)) (set! dragging #f) #t]
             [else
              (and (eq? (caddr event) 'primary)
                (or (eq? (cadr event) 'press) (and (eq? (cadr event) 'move) (equal? dragging id)))
                (let* ([extend? (or (eq? (cadr event) 'move) (memq 'shift (cadddr event)))]
                       [bindings (pointer-bindings (widget:event-frame) (list-ref event 4) (list-ref event 5))]
                       [binding (assoc (if extend? '(click primary (shift)) '(click primary ())) bindings)])
                  (and binding (begin (keymap:run! (cadr binding))
                                 (when (and (eq? (cadr event) 'press) (or extend? (not link)))
                                   (set! dragging id) (widget:capture! id)) #t))))]))]
        [else #f])))

  (edoc "Register the Markdown control and its named keyboard/pointer operations."
        (copy procedure "head clipboard command"))
  (define (register! copy)
    (set! copy-text! copy)
    (widget:register! 'markdown 1
      (list (cons 'prepare prepare) (cons 'viewport viewport) (cons 'render render) (cons 'decorate decorate) (cons 'caret caret)
        (cons 'service service!) (cons 'release release!) (cons 'focus #t) '(contexts . (widget-markdown))
        (cons 'event event!) (cons 'pointer-bindings pointer-bindings)
        (cons 'actions (list (cons 'scroll scroll!) (cons 'move move!) (cons 'select select!) (cons 'copy copy!) (cons 'follow follow!) (cons 'source source!)))))
    (for-each (lambda (binding) (keymap:bind-default! 'widget-markdown (car binding) (keymap:call move! widget:target (cadr binding))))
      '(("UP" up) ("DOWN" down) ("LEFT" left) ("RIGHT" right) ("HOME" home) ("END" end)
        ("C-p" up) ("C-n" down) ("C-b" left) ("C-f" right) ("C-a" home) ("C-e" end)
        ("M-<" start) ("M->" finish) ("C-HOME" start) ("C-END" finish)
        ("PAGEUP" page-up) ("PAGEDOWN" page-down) ("M-v" page-up) ("C-v" page-down)))
    (for-each (lambda (binding) (keymap:bind-default! 'widget-markdown (car binding) (keymap:call move! widget:target (cadr binding) #t)))
      '(("S-UP" up) ("S-DOWN" down) ("S-LEFT" left) ("S-RIGHT" right)))
    (keymap:bind-default! 'widget-markdown "C-@" (keymap:call set-mark! widget:target #t))
    (keymap:bind-default! 'widget-markdown "C-g" (keymap:call set-mark! widget:target #f))
    (keymap:bind-default! 'widget-markdown "RET" (keymap:call follow! widget:target))
    (keymap:bind-default! 'widget-markdown "C-c v" (keymap:call source! widget:target))
    (keymap:bind-default! 'widget-markdown "M-w" (keymap:call copy! widget:target))))
