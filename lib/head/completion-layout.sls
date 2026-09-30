;; Shared TUI candidate projection; no prompt lifetime or input loop.
(import (only (foundation edoc) elibrary))
(elibrary (head completion-layout)
  (export format-columns make-row row-choices row-input row-styles row-text)
  (import (chezscheme) (prefix (head completion) completion:)
          (prefix (head text-layout) text-layout:) (prefix (sys glyph) glyph:))

  (edoc "A projected row with character styles, optional authored input range and clickable insertion spans."
        (text string) (styles any) (input any) (choices list))
  (define-record-type row (fields text styles input choices))

  (define (format-rows candidates width)
    ;; One candidate per row. A label made of the value, two spaces and a
    ;; hint wraps at word boundaries with its continuation rows indented to
    ;; the hint; any other label wraps from the margin. Every row of a
    ;; candidate chooses it.
    (define (slice styles from to)
      (let ([out (make-vector (- to from) 'plain)])
        (do ([i from (+ i 1)]) ((= i to) out)
          (when (< i (vector-length styles)) (vector-set! out (- i from) (vector-ref styles i))))))
    (list->vector
      (apply append
        (map (lambda (value)
               (let* ([rich? (completion:candidate? value)]
                      [chosen (if rich? (completion:candidate-value value) value)]
                      [label (if rich? (completion:candidate-label value) value)]
                      [styles (or (and rich? (completion:candidate-styles value)) (make-vector (string-length label) 'plain))]
                      [head (+ (string-length chosen) 2)]
                      [indent (if (and (< (* 2 head) width) (> (string-length label) head)
                                       (string=? (substring label 0 head) (string-append chosen "  ")))
                                  head 0)]
                      [tail (substring label indent (string-length label))]
                      [breaks (text-layout:breaks tail (max 1 (- width indent)))]
                      [count (vector-length breaks)])
                 (map (lambda (i)
                        (let* ([from (vector-ref breaks i)]
                               [to (if (= (+ i 1) count) (string-length tail) (vector-ref breaks (+ i 1)))]
                               [segment (substring tail from to)]
                               [text (if (= i 0)
                                         (string-append (substring label 0 indent) segment)
                                         (string-append (make-string indent #\space) segment))]
                               [faces (if (= i 0)
                                          (slice styles 0 (+ indent to))
                                          (list->vector
                                            (append (make-list indent 'plain)
                                                    (vector->list (slice styles (+ indent from) (+ indent to))))))])
                          (make-row text faces #f (list (list 0 (string-length text) chosen)))))
                      (iota count))))
             candidates))))

  (edoc "Format prepared candidates as TUI rows sharing text, style and hit coordinates. This only projects supplied values; it never queries completion."
        (candidates list) (width integer) (labeler procedure) (highlight? procedure) (returns vector))
  (define (format-columns candidates width labeler highlight?)
    ;; Labelled candidates take a row each; plain strings fill columns.
    (if (exists completion:candidate? candidates)
        (format-rows candidates width)
        (format-grid candidates width labeler highlight?)))

  (define (format-grid candidates width labeler highlight?)
    (let* ([labels (map (lambda (value) (if (completion:candidate? value) (completion:candidate-label value) (labeler value)))
                     candidates)]
           [column (min width (+ 2 (fold-left max 0 (map glyph:cells labels))))]
           [columns (max 1 (div width (max 1 column)))])
      (let rows ([values candidates] [labels labels] [out '()])
        (if (null? values) (list->vector (reverse out))
            (let fill ([values values] [labels labels] [count 0]
                       [text ""] [styles '()] [choices '()])
              (if (or (= count columns) (null? values))
                  (rows values labels
                    (cons (make-row text (list->vector (apply append (reverse styles)))
                            #f (reverse choices)) out))
                  (let* ([label (car labels)] [shown (glyph:fit label column)]
                         [value (car values)]
                         [base (if (completion:candidate? value) 'plain (if (highlight? label) 'editor 'plain))]
                         [faces (make-vector (string-length shown) base)]
                         [start (string-length text)] [end (+ start (string-length shown))])
                    (when (completion:candidate? value)
                      ;; fit preserves a prefix of whole glyph clusters. Stop
                      ;; copying at its ellipsis/padding so neither is underlined.
                      (let ([visible
                             (if (<= (glyph:cells label) column) (string-length label)
                                 (let trim ([i (- (string-length shown) 1)])
                                   (if (char=? (string-ref shown i) #\space) (trim (- i 1)) i)))])
                        (do ([i 0 (+ i 1)]) ((= i visible))
                          (vector-set! faces i (vector-ref (completion:candidate-styles value) i)))))
                    (fill (cdr values) (cdr labels) (+ count 1)
                      (string-append text shown)
                      (cons (vector->list faces) styles)
                      (cons (list start end (if (completion:candidate? value) (completion:candidate-value value) value))
                        choices)))))))))

)
