;; render.sls -- a head's text and demanded surface rows in terminal cells.
;; No head records, emulator, or terminal I/O: one projection supplies glyphs
;; and both coordinate directions. Public metadata reads own their data.
(library (render)
  (export prepare header row column character width present breaks)
  (import (rnrs)
          (prefix (surface) surface:)
          (prefix (datum) datum:)
          (prefix (glyph) glyph:))

  (define-record-type frame (fields id text header rows))
  (define-record-type line (fields shown styles links columns characters))

  (define (positive-integer? n)
    (and (integer? n) (exact? n) (> n 0)))

  (define (link-ranges links)
    (let scan ([start 0] [out '()])
      (if (= start (vector-length links)) (reverse out)
          (let* ([link (vector-ref links start)]
                 [end (let run ([end (+ start 1)])
                        (if (and (< end (vector-length links))
                                 (equal? link (vector-ref links end)))
                            (run (+ end 1)) end))])
            (scan end (if link (cons (append (list start end) link) out) out))))))

  (define (project text data)
    ;; ASCII keeps its identity geometry without per-character allocations.
    (if (and (not data)
             (let ascii? ([i 0])
               (or (= i (string-length text))
                   (and (< (char->integer (string-ref text i)) 128) (ascii? (+ i 1))))))
        'plain
        (let* ([styles (and data (car data))] [links (and data (cadr data))]
               [attributes (and data (caddr data))]
               [count (string-length text)]
               [entry (and (list? attributes) (for-all pair? attributes)
                           (assq 'clusters attributes))]
               [clusters (cond [entry (cdr entry)]
                               [data (map (lambda (c) '(1 . 1)) (string->list text))]
                               [else (glyph:clusters text)])]
               [width (if data (vector-length styles)
                          (fold-left (lambda (n c) (+ n (cdr c))) 0 clusters))])
          (and (list? clusters)
               (for-all (lambda (cluster)
                          (and (pair? cluster) (positive-integer? (car cluster))
                               (positive-integer? (cdr cluster)))) clusters)
               (= (fold-left (lambda (n c) (+ n (car c))) 0 clusters) count)
               (= (fold-left (lambda (n c) (+ n (cdr c))) 0 clusters) width)
               (let ([shown (make-vector width "")]
                     [columns (make-vector (+ count 1) width)]
                     [characters (make-vector (+ width 1) count)])
                 (and
                   (let fill ([rest clusters] [at 0] [cell 0])
                     (if (null? rest) #t
                         (let* ([end (+ at (caar rest))] [edge (+ cell (cdar rest))]
                                [glyph (substring text at end)]
                                [control? (exists (lambda (c)
                                                    (let ([n (char->integer c)])
                                                      (or (< n 32) (<= 127 n 159))))
                                                  (string->list glyph))])
                           ;; A publisher's totals alone cannot make a wide
                           ;; glyph fit one cell. Reject impossible claims
                           ;; before they can spill into another pane.
                           (and (or (not data) control?
                                    (= (- edge cell)
                                       (fold-left (lambda (n c) (+ n (cdr c))) 0 (glyph:clusters glyph))))
                                (begin
                                  (vector-set! shown cell
                                    (cond [control? (make-string (- edge cell) #\space)]
                                          [(= (glyph:width glyph) 0) (string-append " " glyph)]
                                          [else glyph]))
                                  (do ([i at (+ i 1)]) ((= i end)) (vector-set! columns i cell))
                                  (do ([i cell (+ i 1)]) ((= i edge))
                                    (vector-set! characters i at)
                                    ;; Continuations share the leading style
                                    ;; and link; a run cannot bisect a glyph.
                                    (when styles (vector-set! styles i (vector-ref styles cell)))
                                    (when links (vector-set! links i (vector-ref links cell))))
                                  (fill (cdr rest) end edge))))))
                   (make-line shown styles (if links (link-ranges links) '()) columns characters)))))))

  (define (requested-rows ranges count)
    (let ([wanted (make-eqv-hashtable)])
      (for-each
        (lambda (range)
          (do ([i (max 0 (car range)) (+ i 1)]) ((>= i (min count (cdr range))))
            (hashtable-set! wanted i #t))) ranges)
      (list-sort < (vector->list (hashtable-keys wanted)))))

  (define (prepare previous id text revision ranges . follow-height)
    ;; A bounded retry handles publication between a header and its ranges.
    ;; Retain only demanded rows on a refill; scrolling cannot grow a history
    ;; cache. An unchanged complete request reuses the private projection.
    (define (plain wanted)
      (let ([table (make-eqv-hashtable)]
            [old (and previous (not (frame-header previous))
                      (eq? text (frame-text previous)) (frame-rows previous))])
        (for-each
          (lambda (i)
            (hashtable-set! table i
              (or (and old (hashtable-ref old i #f)) (project (vector-ref text i) #f)))) wanted)
        (make-frame id text #f table)))
    (let ([height (if (null? follow-height) 0 (car follow-height))])
      (let retry ([attempts 2])
        (let* ([snapshot (and id (surface:snapshot id))]
               [next (and snapshot (= (cadr snapshot) revision) snapshot)])
          ;; A following viewport demands rows around this very cursor,
          ;; inside the same generation read/retry as all other ranges.
          (let* ([cursor (and next (caddr next))]
                 [ranges (if (and cursor (> height 0))
                             (cons (cons (- (car cursor) height -1) (+ (car cursor) height)) ranges) ranges)]
                 [wanted (requested-rows ranges (vector-length text))])
            (if (and previous (eqv? (frame-id previous) id)
                     (eq? (frame-text previous) text)
                     (equal? (frame-header previous) next)
                     (for-all (lambda (i) (hashtable-contains? (frame-rows previous) i)) wanted))
                previous
                (if (not next) (plain wanted)
                    (let ([table (make-eqv-hashtable)])
                      (let fetch ([rest wanted])
                        (if (null? rest) (make-frame id text next table)
                          (let* ([start (car rest)]
                                 [tail (let run ([tail (cdr rest)] [end (+ start 1)])
                                         (if (and (pair? tail) (= (car tail) end))
                                             (run (cdr tail) (+ end 1)) (cons end tail)))]
                                 [rows (surface:rows id (car next) start (car tail))])
                            (cond
                              [(not rows) (if (> attempts 0) (retry (- attempts 1)) (plain wanted))]
                              [(for-all
                                 (lambda (entry)
                                   (let ([line (project (vector-ref text (car entry)) (cdr entry))])
                                     (and line (begin (hashtable-set! table (car entry) line) #t)))) rows)
                               (fetch (cdr tail))]
                              [else (plain wanted)]))))))))))))

  (define (header frame) (and frame (datum:copy (frame-header frame))))
  (define (line-at frame row)
    (and frame
         (<= 0 row) (< row (vector-length (frame-text frame)))
         (let ([line (or (hashtable-ref (frame-rows frame) row #f)
                         ;; Navigation can address an undemanded plain row.
                         ;; Derive it without retaining a scrollback cache.
                         (project (vector-ref (frame-text frame) row) #f))])
           (and (line? line) line))))
  (define (row frame index)
    ;; -> owned (cell-strings styles cell-link-ranges), or #f for plain text.
    (let ([line (line-at frame index)])
      (and line (line-styles line)
           (datum:copy (list (line-shown line) (line-styles line) (line-links line))))))
  (define (width frame row fallback)
    (let ([line (line-at frame row)]) (if line (vector-length (line-shown line)) fallback)))
  (define (coordinate table at)
    ;; Preserve addressed columns past the text for unclamped pointer input.
    (let ([last (- (vector-length table) 1)])
      (+ (vector-ref table (min at last)) (max 0 (- at last)))))
  (define (column frame row at . end?)
    ;; Character -> cell. Interior positions snap to the glyph start;
    ;; an interval's end expands to include a partially selected glyph.
    (let ([line (line-at frame row)])
      (if (not line) at
          (let* ([columns (line-columns line)] [cell (coordinate columns at)])
            (if (and (pair? end?) (car end?) (> at 0) (< at (vector-length columns))
                     (= cell (vector-ref columns (- at 1))))
                (let end ([i (+ at 1)])
                  (if (= (vector-ref columns i) cell) (end (+ i 1)) (vector-ref columns i)))
                cell)))))
  (define (character frame row at)
    (let ([line (line-at frame row)])
      (if line (coordinate (line-characters line) at) at)))

  (define (present frame row text replacement styles)
    ;; Modes supply character styles and geometry-preserving substitutions.
    ;; Surfaces already supply cell styles. Invalid substitutions fall back
    ;; to source, so painting never changes navigation's coordinate system.
    (let* ([source (line-at frame row)]
           [replacement (if (and (vector? replacement)
                                 (= (vector-length replacement) (string-length text))
                                 (for-all string? (vector->list replacement)))
                            (apply string-append (vector->list replacement)) replacement)]
           [candidate (and (string? replacement) (= (string-length replacement) (string-length text))
                           (project replacement #f))]
           [compatible? (and candidate
                             (let same? ([i 0])
                               (or (> i (string-length text))
                                   (and (= (if source (vector-ref (line-columns source) i) i)
                                           (if (line? candidate) (vector-ref (line-columns candidate) i) i))
                                        (same? (+ i 1))))))]
           [display (if compatible? candidate source)]
           [shown (if (line? display) (line-shown display)
                      (if compatible? replacement text))])
      ;; Only cached cell data needs copying; identity text and source styles
      ;; were supplied by this caller, so returning them adds no private alias.
      (values (if (vector? shown) (datum:copy shown) shown)
              (if (and source (vector? styles))
                  (let* ([characters (line-characters source)]
                         [out (make-vector (- (vector-length characters) 1) 'plain)])
                    (do ([i 0 (+ i 1)]) ((= i (vector-length out)) out)
                      (let ([at (vector-ref characters i)])
                        (when (< at (vector-length styles))
                          (vector-set! out i (vector-ref styles at))))))
                  styles))))

  (define (breaks text width)
    ;; Return character starts, measuring cells and breaking only between
    ;; whole clusters. A glyph wider than the viewport still advances once.
    (let* ([line (project text #f)] [n (string-length text)]
           [columns (and (line? line) (line-columns line))]
           [characters (and (line? line) (line-characters line))]
           [cells (if columns (vector-ref columns n) n)])
      (define (cell at) (if columns (vector-ref columns at) at))
      (define (char at) (if characters (vector-ref characters at) at))
      (let loop ([start 0] [out '(0)])
        (if (<= (- cells (cell start)) width) (list->vector (reverse out))
            (let* ([limit (char (+ (cell start) width))]
                   [limit (if (> limit start) limit
                              (let next ([i (+ start 1)])
                                (if (or (= i n) (> (cell i) (cell start))) i (next (+ i 1)))))]
                   [end (let find ([j limit])
                          (cond [(<= j start) limit]
                                [(and (char=? (string-ref text (- j 1)) #\space)
                                      (or (= j n) (not (= (cell j) (cell (- j 1)))))) j]
                                [else (find (- j 1))]))])
              (if (= end n) (list->vector (reverse out))
                  (loop end (cons end out))))))))
)
