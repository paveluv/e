;; render.e -- a head's demanded surface rows, paired with its adopted text.
;; No head records, emulator, or terminal I/O: one projection supplies glyphs
;; and both coordinate directions. Public metadata reads own their data.
(library (render)
  (export prepare header row column character width)
  (import (rnrs)
          (prefix (surface) surface:)
          (prefix (datum) datum:))

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
    (if (not data) 'plain
        (let* ([styles (car data)] [links (cadr data)] [attributes (caddr data)]
               [count (string-length text)] [width (vector-length styles)]
               [entry (and (list? attributes) (for-all pair? attributes)
                           (assq 'clusters attributes))]
               [clusters (if entry (cdr entry) (map (lambda (c) '(1 . 1)) (string->list text)))])
          (and (list? clusters)
               (for-all (lambda (cluster)
                          (and (pair? cluster) (positive-integer? (car cluster))
                               (positive-integer? (cdr cluster)))) clusters)
               (= (fold-left (lambda (n c) (+ n (car c))) 0 clusters) count)
               (= (fold-left (lambda (n c) (+ n (cdr c))) 0 clusters) width)
               (let ([shown (make-vector width "")]
                     [columns (make-vector (+ count 1) width)]
                     [characters (make-vector (+ width 1) count)])
                 (let fill ([rest clusters] [at 0] [cell 0])
                   (unless (null? rest)
                     (let ([end (+ at (caar rest))] [edge (+ cell (cdar rest))])
                       (let ([glyph (substring text at end)])
                         (vector-set! shown cell
                           (if (exists (lambda (c)
                                         (let ([n (char->integer c)])
                                           (or (< n 32) (<= 127 n 159))))
                                       (string->list glyph))
                               (make-string (- edge cell) #\space) glyph)))
                       (do ([i at (+ i 1)]) ((= i end)) (vector-set! columns i cell))
                       (do ([i cell (+ i 1)]) ((= i edge))
                         (vector-set! characters i at)
                         ;; A glyph has one rendition. Continuations use its
                         ;; leading cell so a style run cannot bisect it.
                         (vector-set! styles i (vector-ref styles cell))
                         (vector-set! links i (vector-ref links cell)))
                       (fill (cdr rest) end edge))))
                 (make-line shown styles (link-ranges links) columns characters))))))

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
    (let ([height (if (null? follow-height) 0 (car follow-height))])
      (let retry ([attempts 2])
        (let ([next (surface:snapshot id)])
          (and next (= (cadr next) revision)
               ;; A following viewport demands rows around this very cursor,
               ;; inside the same generation read/retry as all other ranges.
               (let* ([cursor (caddr next)]
                      [ranges (if (and cursor (> height 0))
                                  (cons (cons (- (car cursor) height -1) (+ (car cursor) height)) ranges) ranges)]
                      [wanted (requested-rows ranges (vector-length text))])
                 (if (and previous (= (frame-id previous) id)
                       (eq? (frame-text previous) text)
                       (equal? (frame-header previous) next)
                       (for-all (lambda (i) (hashtable-contains? (frame-rows previous) i)) wanted))
                   previous
                   (let ([table (make-eqv-hashtable)])
                     (let fetch ([rest wanted])
                       (if (null? rest) (make-frame id text next table)
                           (let* ([start (car rest)]
                                  [tail (let run ([tail (cdr rest)] [end (+ start 1)])
                                          (if (and (pair? tail) (= (car tail) end))
                                              (run (cdr tail) (+ end 1)) (cons end tail)))]
                                  [rows (surface:rows id (car next) start (car tail))])
                             (cond
                               [(not rows) (and (> attempts 0) (retry (- attempts 1)))]
                               [(for-all
                                  (lambda (entry)
                                    (let ([line (project (vector-ref text (car entry)) (cdr entry))])
                                      (and line (begin (hashtable-set! table (car entry) line) #t)))) rows)
                                (fetch (cdr tail))]
                               [else #f]))))))))))))

  (define (header frame) (and frame (datum:copy (frame-header frame))))
  (define (line-at frame row)
    (and frame
         (let ([line (hashtable-ref (frame-rows frame) row #f)])
           (and (line? line) line))))
  (define (row frame index)
    ;; -> owned (cell-strings styles cell-link-ranges), or #f for plain text.
    (let ([line (line-at frame index)])
      (and line (datum:copy (list (line-shown line) (line-styles line) (line-links line))))))
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
      (if line (coordinate (line-characters line) at) at))))
