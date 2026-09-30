;; Fit semantic Markdown blocks to this mount's terminal width. Code faces
;; come from head modes; the source blocks remain immutable and portable.
(import (only (foundation edoc) elibrary))
(elibrary (head markdown-layout) (export render)
  (import
    (chezscheme)
    (prefix (foundation string) string:)
    (prefix (head mode) mode:)
    (prefix (only (sys sys) terminal-character-width) sys:))
  (define (display-width text)
    (fold-left
      (lambda (total c)
        (+ total (sys:terminal-character-width c)))
      0
      (string->list text)))
  (define (presented inline)
    (let* ([text (car inline)]
           [faces (make-vector (string-length text) 'plain)])
      (for-each
        (lambda (run)
          (do ([i (car run) (+ i 1)])
              ((= i (cadr run)))
            (vector-set! faces i (caddr run))))
        (cadr inline))
      (list text faces (caddr inline))))

  (edoc
    "Fit interpreted Markdown blocks to a terminal width. Return text, character styles, links, source rows and character anchor vectors. Anchors are (source-row field character): field zero for text, positive for table cells."
    (blocks list "markup:parse blocks")
    (target-width integer "available columns"))
  (define (render blocks target-width)
    (define out-lines '())
    (define out-styles '())
    (define out-links '())
    (define out-rows '())
    (define out-anchors '())
    (define (emit! line styles links row . anchors)
      (set! out-lines (cons line out-lines))
      (set! out-styles (cons styles out-styles))
      (set! out-links (cons links out-links))
      (set! out-rows (cons row out-rows))
      (set! out-anchors
        (cons (if (pair? anchors) (car anchors)
                (list->vector (map (lambda (i) (list row 0 i)) (iota (+ 1 (string-length line)))))) out-anchors)))
    (define (chrome! line row)
      (emit! line (make-vector (string-length line) 'chrome) '() row
        (make-vector (+ 1 (string-length line)) (list row 0 0))))
    (define (longest-word-width text)
      (let loop ([i 0] [word 0] [best 0])
        (cond
          [(= i (string-length text)) (max best word)]
          [(char=? (string-ref text i) #\space)
           (loop (+ i 1) 0 (max best word))]
          [else
           (loop
             (+ i 1)
             (+ word (sys:terminal-character-width (string-ref text i)))
             best)])))
    (define (allocate-widths naturals minimums avail)
      (let ([nat (fold-left + 0 naturals)]
            [floor-sum (fold-left + 0 minimums)])
        (cond
          [(<= nat avail) naturals]
          [(>= floor-sum avail)
           (map (lambda (m) (max 1 (div (* m avail) floor-sum)))
                minimums)]
          [else
           (let* ([slack (map - naturals minimums)]
                  [total (fold-left + 0 slack)]
                  [extra (- avail floor-sum)]
                  [shares (map (lambda (s) (div (* s extra) total)) slack)]
                  [left (- extra (fold-left + 0 shares))]
                  [widest (fold-left max 0 slack)])
             (let give ([ws (map + minimums shares)]
                        [ss slack]
                        [left left])
               (cond
                 [(null? ws) '()]
                 [(and (> left 0) (= (car ss) widest))
                  (cons (+ (car ws) left) (give (cdr ws) (cdr ss) 0))]
                 [else
                  (cons (car ws) (give (cdr ws) (cdr ss) left))])))])))
    (define (improve-widths widths minimums naturals rendered)
      (define wv (list->vector widths))
      (define mv (list->vector minimums))
      (define nv (list->vector naturals))
      (define k (vector-length wv))
      (define (cell-height cell width)
        (length (wrap-cell (car cell) '#() '() width)))
      (define (height)
        (fold-left
          (lambda (total row)
            (+ total
               (let cells ([cs row] [i 0] [m 1])
                 (if (or (null? cs) (= i k))
                     m
                     (cells
                       (cdr cs)
                       (+ i 1)
                       (max m
                            (cell-height (car cs) (vector-ref wv i))))))))
          0
          rendered))
      (define (find-improvement h0)
        (let taker ([i 0])
          (and (< i k)
               (if (>= (vector-ref wv i) (vector-ref nv i))
                   (taker (+ i 1))
                   (let donor ([j 0])
                     (cond
                       [(= j k) (taker (+ i 1))]
                       [(or (= j i)
                            (<= (vector-ref wv j)
                                (max 1 (vector-ref mv j))))
                        (donor (+ j 1))]
                       [else
                        (vector-set! wv i (+ (vector-ref wv i) 1))
                        (vector-set! wv j (- (vector-ref wv j) 1))
                        (let ([h1 (height)])
                          (if (or (< h1 h0)
                                  (and (= h1 h0)
                                       (< (vector-ref nv i)
                                          (vector-ref wv j))))
                              #t
                              (begin
                                (vector-set! wv i (- (vector-ref wv i) 1))
                                (vector-set! wv j (+ (vector-ref wv j) 1))
                                (donor (+ j 1)))))]))))))
      (let climb ([budget 32])
        (when (and (> budget 0) (find-improvement (height)))
          (climb (- budget 1))))
      (vector->list wv))
    (define (wrap-cell text styles links width)
      (define n (string-length text))
      (define (cut-point from)
        (let scan ([i from] [used 0] [space #f])
          (if (= i n)
              (values n n)
              (let ([w (sys:terminal-character-width
                         (string-ref text i))])
                (cond
                  [(<= (+ used w) width)
                   (scan
                     (+ i 1)
                     (+ used w)
                     (if (char=? (string-ref text i) #\space) i space))]
                  [space (values space (+ space 1))]
                  [(= i from) (values (+ i 1) (+ i 1))]
                  [else (values i i)])))))
      (define (line-slice from end)
        (list
          (substring text from end)
          (let ([v (make-vector (- end from) 'plain)])
            (do ([p from (+ p 1)])
                ((or (= p end) (>= p (vector-length styles))))
              (vector-set! v (- p from) (vector-ref styles p)))
            v)
          (filter
            (lambda (l) l)
            (map (lambda (l)
                   (let ([s (max (car l) from)] [e (min (cadr l) end)])
                     (and (< s e) (list (- s from) (- e from) (caddr l)))))
                 links)) from))
      (let build ([from 0] [acc '()])
        (if (>= from n)
            (if (null? acc) (list (list "" '#() '() 0)) (reverse acc))
            (let-values ([(end next) (cut-point from)])
              (build
                (let skip ([j next])
                  (if (and (< j n) (char=? (string-ref text j) #\space))
                      (skip (+ j 1))
                      j))
                (cons (line-slice from end) acc))))))
    (define (pad-to text width)
      (let ([shortfall (- width (display-width text))])
        (if (> shortfall 0)
            (string-append text (make-string shortfall #\space))
            text)))
    (define (table! rows header?)
      (let* ([rendered (map (lambda (row)
                              (map presented (cdr row)))
                            rows)]
             [columns (fold-left max 0 (map length rendered))]
             [naturals (let column ([k 0] [acc '()])
                         (if (= k columns)
                             (reverse acc)
                             (column
                               (+ k 1)
                               (cons
                                 (fold-left
                                   (lambda (m row-cells)
                                     (if (< k (length row-cells))
                                         (max m
                                              (display-width
                                                (car (list-ref
                                                       row-cells
                                                       k))))
                                         m))
                                   0
                                   rendered)
                                 acc))))]
             [minimums (let column ([k 0] [acc '()])
                         (if (= k columns)
                             (reverse acc)
                             (column
                               (+ k 1)
                               (cons
                                 (fold-left
                                   (lambda (m row-cells)
                                     (if (< k (length row-cells))
                                         (max m
                                              (longest-word-width
                                                (car (list-ref
                                                       row-cells
                                                       k))))
                                         m))
                                   1
                                   rendered)
                                 acc))))]
             [widths (improve-widths
                       (allocate-widths
                         naturals
                         minimums
                         (max 1 (- target-width (* 2 (max 0 (- columns 1))))))
                       minimums
                       naturals
                       rendered)])
        (let build ([rows rendered]
                    [anchors (map car rows)]
                    [first #t])
          (unless (null? rows)
            (let* ([row-cells (car rows)]
                   [wrapped (let fill ([i 0] [cells row-cells] [acc '()])
                              (if (= i columns)
                                  (reverse acc)
                                  (fill
                                    (+ i 1)
                                    (if (pair? cells) (cdr cells) '())
                                    (cons
                                      (if (pair? cells)
                                          (apply
                                            wrap-cell
                                            (append
                                              (car cells)
                                              (list (list-ref widths i))))
                                          (list (list "" '#() '() 0)))
                                      acc))))]
                   [height (fold-left
                             (lambda (m lines) (max m (length lines)))
                             1
                             wrapped)])
              (do ([v 0 (+ v 1)])
                  ((= v height))
                (let* ([segments (map (lambda (lines)
                                        (if (< v (length lines))
                                            (list-ref lines v)
                                            (list "" '#() '() #f)))
                                      wrapped)]
                       [parts (map (lambda (seg w) (pad-to (car seg) w))
                                   segments
                                   widths)]
                       [joined (string:trim-spaces
                                 (string:join parts "  ")
                                 #f)]
                       [vec (make-vector (string-length joined) 'plain)]
                       [anchors-map (make-vector (+ 1 (string-length joined)) #f)]
                       [row-links '()])
                  (let paint ([at 0] [segs segments] [parts parts] [field 1])
                    (when (pair? segs)
                      (let* ([seg (car segs)]
                             [text (car seg)]
                             [styles (cadr seg)])
                        (do ([p 0 (+ p 1)])
                            ((or (> p (+ 2 (string-length (car parts))))
                               (>= (+ at p) (vector-length anchors-map))))
                          (when (cadddr seg)
                            (vector-set! anchors-map (+ at p)
                              (list (car anchors) field (+ (cadddr seg) (min p (string-length text)))))))
                        (do ([p 0 (+ p 1)])
                            ((or (= p (string-length text))
                                 (>= (+ at p) (vector-length vec))))
                          (let ([st (if (< p (vector-length styles))
                                        (vector-ref styles p)
                                        'plain)])
                            (vector-set!
                              vec
                              (+ at p)
                              (if (and first header? (eq? st 'plain))
                                  'bold
                                  st))))
                        (for-each
                          (lambda (l)
                            (set! row-links
                              (cons
                                (list
                                  (+ at (car l))
                                  (+ at (cadr l))
                                  (caddr l))
                                row-links)))
                          (caddr seg))
                        (paint
                          (+ at (string-length (car parts)) 2)
                          (cdr segs)
                          (cdr parts) (+ field 1)))))
                  ;; Empty continuation cells have no character on this row.
                  ;; Anchor their padding to the nearest present cell, so a
                  ;; viewport starting here cannot jump back to the first row.
                  (let fill ([i (- (vector-length anchors-map) 1)] [next (list (car anchors) 0 0)])
                    (when (>= i 0)
                      (let ([p (or (vector-ref anchors-map i) next)])
                        (vector-set! anchors-map i p) (fill (- i 1) p))))
                  (emit! joined vec (reverse row-links) (car anchors) anchors-map)))
              (when (and first header?)
                (chrome!
                  (string:join
                    (map (lambda (w) (make-string w #\─)) widths)
                    "  ")
                  (car anchors)))
              (build (cdr rows) (cdr anchors) #f))))))
    (define (code! row end tag body)
      (let* ([label (if (string=? tag "")
                        ""
                        (string-append "┄ " tag " "))]
             [width (max (fold-left
                           (lambda (n s) (max n (display-width s)))
                           0
                           body)
                         (+ 1 (display-width label)))]
             [top (string-append label (make-string (- width (display-width label)) #\┄))])
        (chrome! top row)
        (let ([mode (and (not (string=? tag "")) (mode:find tag))])
          (for-each
            (lambda (s row)
              (let ([faces (make-vector (string-length s) 'md-code)]
                    [syntax (and mode
                                 (guard (ex [else #f])
                                   ((mode:styles mode) s)))])
                (when (vector? syntax)
                  (do ([i 0 (+ i 1)])
                      ((= i
                          (min (string-length s) (vector-length syntax))))
                    (unless (eq? (vector-ref syntax i) 'plain)
                      (vector-set! faces i (vector-ref syntax i)))))
                (emit! s faces '() row)))
            body
            (map (lambda (i) (+ row 1 i)) (iota (length body)))))
        (chrome! (make-string width #\┄) end)))
    (for-each
      (lambda (block)
        (case (car block)
          [(line)
           (apply
             (lambda (text roles links)
               (emit! text roles links (cadr block)))
             (presented (caddr block)))]
          [(rule)
           (chrome! (make-string 40 #\─) (cadr block))]
          [(code) (apply code! (cdr block))]
          [(table) (table! (cadddr block) (caddr block))]))
      blocks)
    (values
      (reverse out-lines)
      (reverse out-styles)
      (reverse out-links)
      (reverse out-rows)
      (reverse out-anchors))))
