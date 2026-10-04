;; TUI text geometry over an explicit source. No windows, views or device I/O.
(import (only (foundation edoc) elibrary))
(elibrary (head text-layout)
  (export adjacent anchor breaks column distance hit locate move overflows? page scroll scroll-margin segment wrap-lines)
  (import (chezscheme) (prefix (head render) render:) (prefix (sys glyph) glyph:))

  (edoc "Move to the preceding or following grapheme, crossing a line boundary when needed."
        (lines vector "text lines") (p position "logical caret") (direction (one-of left right) "motion") (returns position))
  (define (adjacent lines p direction)
    (let* ([r (car p)] [c (cdr p)] [line (vector-ref lines r)] [n (string-length line)] [left? (eq? direction 'left)])
      (cond [(and left? (zero? c))
             (if (> r 0) (cons (- r 1) (string-length (vector-ref lines (- r 1)))) p)]
        [(and left? (> c n)) (cons r n)]
        [(and (not left?) (>= c n)) (if (< (+ r 1) (vector-length lines)) (cons (+ r 1) 0) p)]
        [else
         (let ([next (+ c (if left? -1 1))])
           ;; Endpoints and adjacent ASCII characters guarantee a boundary.
           ;; A Unicode neighbour can extend/prepend a cluster: keep the full
           ;; segmentation there, including when the caret is inside one.
           (if (or (zero? next) (= next n)
                   (and (< (char->integer (string-ref line (- next 1))) 128)
                        (< (char->integer (string-ref line next)) 128)))
             (cons r next)
             (let ([edges (fold-left (lambda (out cluster) (cons (+ (car out) (car cluster)) out)) '(0) (glyph:clusters line))])
               (cons r (if left? (find (lambda (n) (< n c)) edges)
                         (find (lambda (n) (> n c)) (reverse edges)))))))])))


  (define wrap-cache (make-weak-eq-hashtable))

  (edoc "Whether text views soft-wrap by default; an explicit view or source preference takes precedence."
        (value boolean))
  (define wrap-lines (make-parameter #t))

  (edoc "The rows kept between the caret and the edges while revealing it, bounded by the available viewport."
        (value integer))
  (define scroll-margin (make-parameter 8 (lambda (v) (max 0 v))))

  (edoc "Reuse immutable soft-wrap boundaries for a line at a resolved cell width."
        (line string "source line") (width integer "positive cell width") (returns vector) (effects internal))
  (define (breaks line width)
    (let* ([hit (eq-hashtable-ref wrap-cache line '())] [found (assv width hit)])
      (if found (cdr found)
        (let ([result (render:breaks line width)])
          (eq-hashtable-set! wrap-cache line (cons (cons width result) hit)) result))))

  (edoc "The wrapped segment containing a logical character position."
        (breaks vector "segment starts") (column integer "character position") (returns integer))
  (define (segment breaks column)
    (let loop ([k (- (vector-length breaks) 1)])
      (if (or (= k 0) (>= column (vector-ref breaks k))) k (loop (- k 1)))))

  (edoc "The last character position addressable in a wrapped segment."
        (breaks vector "segment starts") (index integer "segment") (length integer "line length") (returns integer))
  (define (segment-close breaks index length)
    (if (< (+ index 1) (vector-length breaks)) (- (vector-ref breaks (+ index 1)) 1) length))

  (define (count lines width row)
    (if width (vector-length (breaks (render:line-ref lines row) width)) 1))
  (define (address lines width point)
    (cons (car point) (if width (segment (breaks (render:line-ref lines (car point)) width) (cdr point)) 0)))

  (edoc "Convert a transient (line . wrapped-segment) address to a logical text anchor. Persist this anchor, never the segment number."
        (lines any "source lines") (width (or integer #f) "wrap width, false for unwrapped")
        (at position "line and segment") (returns position))
  (define (anchor lines width at)
    (cons (car at) (if width (vector-ref (breaks (render:line-ref lines (car at)) width) (cdr at)) 0)))

  (edoc "Land at a cell within a wrapped segment, clamped to that segment and snapped to a whole grapheme."
        (lines any "source lines") (frame any "cell projection") (row integer "source row")
        (breaks (or vector #f) "segment starts") (index integer "segment") (cell integer "relative visual cell") (returns integer))
  (define (column lines frame row breaks index cell)
    (let* ([length (string-length (render:line-ref lines row))]
           [start (if breaks (vector-ref breaks index) 0)]
           [end (if breaks (segment-close breaks index length) length)]
           [at (min end (render:character frame row (+ (render:column frame row start) cell)))])
      (render:character frame row (render:column frame row at))))

  (define (walk lines width at delta)
    (let loop ([row (car at)] [seg (cdr at)] [delta delta])
      (cond [(zero? delta) (cons row seg)]
        [(negative? delta)
         (cond [(>= seg (- delta)) (cons row (+ seg delta))]
           [(zero? row) '(0 . 0)]
           [else (loop (- row 1) (- (count lines width (- row 1)) 1) (+ delta seg 1))])]
        [else
         (let ([remaining (- (count lines width row) seg 1)])
           (cond [(<= delta remaining) (cons row (+ seg delta))]
             [(= (+ row 1) (render:line-count lines)) (cons row (+ seg remaining))]
             [else (loop (+ row 1) 0 (- delta remaining 1))]))])))

  (define (before? a b)
    (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (edoc "Move by displayed rows toward a retained cell goal, without inspecting a window."
        (lines any "source lines") (frame any "cell projection") (width (or integer #f) "wrap width")
        (point position "logical caret") (delta integer "displayed rows") (goal integer "cell within a displayed row") (returns position))
  (define (move lines frame width point delta goal)
    (let* ([at (if width (walk lines width (address lines width point) delta)
                   (cons (max 0 (min (+ (car point) delta) (- (render:line-count lines) 1))) 0))]
           [row (car at)])
      (cons row (column lines frame row (and width (breaks (render:line-ref lines row) width)) (cdr at) goal))))

  (edoc "Displayed rows from a transient top address to a logical point. Negative means the point is above the top."
        (lines any "source lines") (width (or integer #f) "wrap width") (top position "line and segment")
        (point position "logical point") (returns integer))
  (define (distance lines width top point)
    (distance-up-to lines width top point #f))
  (define (distance-up-to lines width top point limit)
    (if (not width) (- (car point) (car top))
      (let ([end (address lines width point)])
        (let loop ([row (min (car top) (car end))] [n 0])
          (cond [(= row (max (car top) (car end)))
                 (+ (if (< (car end) (car top)) (- n) n) (cdr end) (- (cdr top)))]
            [(and limit (> (car end) (car top)) (>= (- n (cdr top)) limit)) limit]
            [else (loop (+ row 1) (+ n (count lines width row)))])))))

  (edoc "Whether source rows extend beyond a viewport's displayed height; work stops at its lower edge."
        (lines any "source lines") (width (or integer #f) "wrap width") (top position "line and segment")
        (height integer "displayed height") (returns boolean))
  (define (overflows? lines width top height)
    (let loop ([row (car top)] [n (- (cdr top))])
      (cond [(> n height) #t] [(>= row (render:line-count lines)) #f]
        [else (loop (+ row 1) (+ n (count lines width row)))])))

  (edoc "A logical point's viewport-relative (x . y), sharing the hit tester's wrap and grapheme mapping."
        (lines any "source lines") (frame any "cell projection") (width (or integer #f) "wrap width")
        (top position "line and segment") (left integer "unwrapped cell offset") (point position "logical point") (returns position))
  (define (locate lines frame width top left point)
    (let* ([row (car point)] [col (cdr point)]
           [start (if width (cdr (anchor lines width (address lines width point))) 0)])
      (cons (- (render:column frame row col) (if width (render:column frame row start) left))
        (distance lines width top point))))

  (edoc "Map viewport-relative cells to a logical point. Preserve blank space below the source for hit testing; selecting a caret is a separate clamping policy."
        (lines any "source lines") (frame any "cell projection") (width (or integer #f) "wrap width")
        (top position "line and segment") (left integer "unwrapped cell offset") (x integer "column") (y integer "row") (returns position))
  (define (hit lines frame width top left x y)
    (if width
      (let loop ([row (car top)] [offset (+ y (cdr top))])
        (if (>= row (render:line-count lines)) (cons row x)
          (let ([bs (breaks (render:line-ref lines row) width)])
            (if (< offset (vector-length bs)) (cons row (column lines frame row bs offset x))
              (loop (+ row 1) (- offset (vector-length bs)))))))
      (let ([row (+ (car top) y)]) (cons row (render:character frame row (+ left x))))))

  (edoc "Page an explicit viewport and land its caret centrally; paging outward at an edge selects that edge. Returns top address and logical caret."
        (lines any "source lines") (frame any "cell projection") (width (or integer #f) "wrap width")
        (first integer "first scrollable source row") (height integer "body height") (top position "line and segment")
        (goal integer "caret cell within a displayed row") (direction integer "negative up, positive down")
        (fraction integer "positive page divisor"))
  (define (page lines frame width first height top goal direction fraction)
    (let* ([last (- (render:line-count lines) 1)] [height (max 1 height)] [start (cons first 0)]
           [end (cons last (- (count lines width last) 1))]
           [last-top (walk lines width end (- 1 height))]
           [fits? (not (overflows? lines width start height))] [up? (negative? direction)]
           [step (max 1 (quotient height fraction))])
      (define (clamp at) (cond [(or fits? (before? at start)) start] [(before? last-top at) last-top] [else at]))
      (let* ([old (clamp top)] [at-edge? (equal? old (if up? start last-top))]
             [next (clamp (walk lines width old (if up? (- step) step)))]
             [caret (if (or fits? at-edge?) (if up? start end) (walk lines width next (quotient (- height 1) 2)))]
             [row (car caret)])
        (values next (cons row (column lines frame row (and width (breaks (render:line-ref lines row) width)) (cdr caret) goal))))))

  (edoc "Fit a caret and viewport with a scroll margin. Returns clamped point, transient top address and left cell offset. Geometry stays local to the head."
        (lines any "source lines") (frame any "cell projection") (width (or integer #f) "wrap width")
        (columns integer "content width") (height integer "body height") (first integer "first scrollable source row")
        (top position "line and segment") (left integer "unwrapped cell offset") (point position "logical caret")
        (margin integer "preferred margin"))
  (define (scroll lines frame width columns height first top left point margin)
    (let* ([last (- (render:line-count lines) 1)] [row (max 0 (min (car point) last))]
           [col (max 0 (min (cdr point) (string-length (render:line-ref lines row))))]
           [point (cons row col)] [height (max 1 height)] [columns (max 1 columns)] [m (min margin (div (- height 1) 2))]
           [first (min first last)])
      (if width
        (let* ([target (if (< row first) (cons first 0) point)]
               [start (max first (min (car top) last))]
               [top (cons start (min (cdr top) (- (count lines width start) 1)))]
               [end (address lines width target)])
          ;; A fixed header lies before the body: reveal its beginning,
          ;; just as for unwrapped text, without walking from a distant top.
          (when (before? end top) (set! top end))
          (let retreat ()
            (when (and (< (distance-up-to lines width top target height) m) (or (> (car top) first) (> (cdr top) 0)))
              (set! top (walk lines width top -1)) (retreat)))
          (when (>= (distance-up-to lines width top target height) (- height m))
            ;; Seek back from the caret instead of walking every intervening
            ;; line. At EOF, retain the last complete viewport if it fits.
            (let* ([candidate (walk lines width end (- (+ m 1) height))]
                   [candidate (if (overflows? lines width candidate (- height 1)) candidate
                                (walk lines width (cons last (- (count lines width last) 1)) (- 1 height)))])
              (when (before? top candidate)
                (set! top candidate))))
          (values point top left))
        (let ([start (car top)] [cell (render:column frame row col)])
          (when (< row (+ start m)) (set! start (max first (- row m))))
          (when (>= row (+ start height (- m)))
            (set! start (min (- row (- height 1 m)) (max first (- (render:line-count lines) height)))))
          (when (< cell left) (set! left cell))
          (when (>= cell (+ left columns)) (set! left (- cell columns -1)))
          (values point (cons start 0) left)))))
)
