;; glyph.sls -- shared terminal-cell widths and cluster boundaries.
(library (glyph)
  (export width extends? clusters cells fit)
  (import (chezscheme) (prefix (sys) sys:))

  (define (width text)
    (let loop ([i 0] [cells 0] [indicators 0] [emoji? #f])
      (if (= i (string-length text))
          (if (or emoji? (>= indicators 2)) (max 2 cells) cells)
          (let ([c (string-ref text i)])
            (loop (+ i 1) (max cells (sys:terminal-character-width c))
                  (+ indicators (if (eq? (char-grapheme-break-property c) 'Regional_Indicator) 1 0))
                  (or emoji? (memv c '(#\xfe0f #\x20e3))))))))

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

  (define (cells text)
    (fold-left (lambda (n cluster) (+ n (cdr cluster))) 0 (clusters text)))

  (define (fit text width . side)
    ;; Fit a label to exactly width terminal cells, padding on the right.
    ;; Truncate whole clusters, with an ellipsis on the right by default
    ;; or on the left to retain a path's informative tail.
    (let* ([left? (and (pair? side) (eq? (car side) 'left))]
           [parts (clusters text)]
           [size (fold-left (lambda (n part) (+ n (cdr part))) 0 parts)])
      (cond [(<= width 0) ""]
            [(<= size width) (string-append text (make-string (- width size) #\space))]
            [else
             (let keep ([parts (if left? (reverse parts) parts)] [chars 0] [used 0])
               (if (or (null? parts) (> (+ used (cdar parts)) (- width 1)))
                   (string-append
                     (if left?
                         (string-append "…" (substring text (- (string-length text) chars) (string-length text)))
                         (string-append (substring text 0 chars) "…"))
                     (make-string (- width used 1) #\space))
                   (keep (cdr parts) (+ chars (caar parts)) (+ used (cdar parts)))))])))

  (define (clusters text)
    ;; Positive (character-count . cell-count) pairs. A control occupies one
    ;; blank cell, including tabs; an isolated zero-width cluster gets a
    ;; blank anchor. Controls cannot absorb an adjacent combining character.
    (let ([n (string-length text)])
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
                                          (max 1 (width (substring text start i))))) out)))))))))
