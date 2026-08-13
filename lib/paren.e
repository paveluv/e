;; paren.e -- matching-bracket highlighting for the e editor.
;;
;; An e extension module: the library (paren), loaded at startup by the
;; e loader, which calls init!.  Registers a highlighter (the core's
;; generic context-highlighting hook) that marks the bracket at point
;; and its partner, as in Emacs's show-paren-mode: the opener point
;; sits on, or the closer just before it.  Brackets inside strings and
;; comments don't count, per the buffer's syntax styles; in a buffer
;; without a mode every bracket counts.

(library (paren)
  (export init!)
  (import (chezscheme) (core))

  (define (scan-paren b styles-of start-row start-col dir)
    ;; Find the bracket balancing the one at (start-row, start-col),
    ;; scanning forward (dir 1) or backward (dir -1).  The scan is bounded
    ;; so pathological buffers stay responsive; #f when nothing balances.
    (define count (buffer-line-count b))
    (define (line r) (buffer-line b r))
    (let walk ([row start-row] [col start-col]
               [styles (styles-of (line start-row))]
               [depth 0] [budget 50000])
      (and (> budget 0)
           (if (or (< col 0) (>= col (string-length (line row))))
               (let ([row (+ row dir)])
                 (and (>= row 0) (< row count)
                      (walk row
                            (if (> dir 0) 0 (- (string-length (line row)) 1))
                            (styles-of (line row))
                            depth (- budget 1))))
               (let* ([c (string-ref (line row) col)]
                      [delta (if (or (not styles)
                                     (eq? (vector-ref styles col) 'delimiter))
                                 (cond [(memv c '(#\( #\[ #\{)) dir]
                                       [(memv c '(#\) #\] #\})) (- dir)]
                                       [else 0])
                                 0)])
                 (if (and (not (= delta 0)) (= (+ depth delta) 0))
                     (cons row col)
                     (walk row (+ col dir) styles (+ depth delta) (- budget 1))))))))

  (define (paren-highlights)
    ;; The bracket at point and its partner, as (row start end) ranges;
    ;; empty when neither applies.
    (let* ([b (current-buffer)]
           [styles-of (buffer-line-styles b)]
           [pt (point)]
           [row (car pt)]
           [line (buffer-line b row)]
           [styles (styles-of line)])
      (define (bracket-at col kinds)
        (and (>= col 0) (< col (string-length line))
             (memv (string-ref line col) kinds)
             (or (not styles) (eq? (vector-ref styles col) 'delimiter))
             col))
      (let* ([closer (bracket-at (- (cdr pt) 1) '(#\) #\] #\}))]
             [opener (and (not closer) (bracket-at (cdr pt) '(#\( #\[ #\{)))]
             [col (or closer opener)]
             [match (and col (scan-paren b styles-of row col (if closer -1 1)))])
        (if match
            (list (list row col (+ col 1))
                  (list (car match) (cdr match) (+ (cdr match) 1)))
            '()))))

  (define (init!)
    (add-highlighter! paren-highlights)))
