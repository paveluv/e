;; glyph.sls -- shared terminal-cell widths and cluster boundaries.
(import (only (edoc) elibrary))
(elibrary (glyph)
  (export width extends? clusters cells fit)
  (import (chezscheme) (prefix (sys) sys:))

  (edoc "The terminal cells one grapheme cluster takes: the widest character, 2 for emoji and flags."
        (text string "the cluster")
        (returns integer))
  (define (width text)
    (let loop ([i 0] [cells 0] [indicators 0] [emoji? #f])
      (if (= i (string-length text))
          (if (or emoji? (>= indicators 2)) (max 2 cells) cells)
          (let ([c (string-ref text i)])
            (loop (+ i 1) (max cells (sys:terminal-character-width c))
                  (+ indicators (if (eq? (char-grapheme-break-property c) 'Regional_Indicator) 1 0))
                  (or emoji? (memv c '(#\xfe0f #\x20e3))))))))

  (edoc "Whether a character continues the cluster of the previous one."
        (previous (or char #f) "the previous character")
        (character char "the character")
        (indicators integer "regional indicators seen so far")
        (returns boolean))
  (define (extends? previous character indicators)
    (let ([property (char-grapheme-break-property character)]
          [before (and previous (char-grapheme-break-property previous))])
      (or (memq property '(Extend ZWJ SpacingMark))
          (eq? before 'Prepend)
          (and (eq? before 'L) (memq property '(L V LV LVT)))
          (and (memq before '(LV V)) (memq property '(V T)))
          (and (memq before '(LVT T)) (eq? property 'T))
          (and (eq? property 'Regional_Indicator) (odd? indicators))
          (eqv? previous #\x200d))))

  (define (control? c)
    (let ([n (char->integer c)]) (or (< n 32) (<= 127 n 159))))

  (define (plain? text)
    ;; Printable ASCII: one cell per character, and nothing extends a cluster.
    (let ([n (string-length text)])
      (let loop ([i 0])
        (or (fx= i n)
            (and (let ([c (char->integer (string-ref text i))]) (and (fx>= c 32) (fx< c 127)))
                 (loop (fx+ i 1)))))))

  (edoc "The terminal cells a text takes."
        (text string "the text")
        (returns integer))
  (define (cells text)
    (if (plain? text) (string-length text)
        (fold-left (lambda (n cluster) (+ n (cdr cluster))) 0 (clusters text))))

  (edoc "A text fitted to exactly a width in cells, padded on the right or cut at whole clusters with an ellipsis, on the left when asked."
        (text string "the text")
        (width integer "the cells")
        (side (list-of (one-of left)) "left to cut the start, at most one")
        (returns string))
  (define (fit text width . side)
    ;; Fit a label to exactly width terminal cells, padding on the right.
    ;; Truncate whole clusters, with an ellipsis on the right by default
    ;; or on the left to retain a path's informative tail.
    (let* ([left? (and (pair? side) (eq? (car side) 'left))]
           [plain (plain? text)]
           [parts (if plain '() (clusters text))]
           [size (if plain (string-length text) (fold-left (lambda (n part) (+ n (cdr part))) 0 parts))])
      (cond [(<= width 0) ""]
            [(<= size width) (string-append text (make-string (- width size) #\space))]
            [plain
             ;; One cell per character: width - 1 of them stay beside the ellipsis.
             (if left?
                 (string-append "…" (substring text (- size (- width 1)) size))
                 (string-append (substring text 0 (- width 1)) "…"))]
            [else
             (let keep ([parts (if left? (reverse parts) parts)] [chars 0] [used 0])
               (if (or (null? parts) (> (+ used (cdar parts)) (- width 1)))
                   (string-append
                     (if left?
                         (string-append "…" (substring text (- (string-length text) chars) (string-length text)))
                         (string-append (substring text 0 chars) "…"))
                     (make-string (- width used 1) #\space))
                   (keep (cdr parts) (+ chars (caar parts)) (+ used (cdar parts)))))])))

  (edoc "A text's grapheme clusters as (character-count . cell-count) pairs."
        (text string "the text")
        (returns list))
  (define (clusters text)
    ;; Positive (character-count . cell-count) pairs. A control occupies one
    ;; blank cell, including tabs; an isolated zero-width cluster gets a
    ;; blank anchor. Controls cannot absorb an adjacent combining character.
    (let ([n (string-length text)])
      (if (plain? text) (map (lambda (i) '(1 . 1)) (iota n))
        (let scan ([start 0] [out '()])
          (if (= start n) (reverse out)
            (let end ([i (+ start 1)]
                      [indicators (if (eq? (char-grapheme-break-property (string-ref text start))
                                           'Regional_Indicator) 1 0)])
              (if (and (< i n)
                       (not (control? (string-ref text (- i 1))))
                       (not (control? (string-ref text i)))
                       (or (= (sys:terminal-character-width (string-ref text i)) 0)
                           (extends? (string-ref text (- i 1)) (string-ref text i) indicators)))
                  (end (+ i 1)
                       (+ indicators (if (eq? (char-grapheme-break-property (string-ref text i))
                                              'Regional_Indicator) 1 0)))
                  (scan i (cons (cons (- i start)
                                      (if (control? (string-ref text start)) 1
                                          (max 1 (width (substring text start i))))) out))))))))))
