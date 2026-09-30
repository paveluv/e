;; paren.sls -- matching-bracket highlighting for the e editor.
;;
;; An e extension module: the library (paren), loaded at startup by the
;; kernel, which calls init!. Registers a pure mode highlighter that marks the bracket at point
;; and its partner, as in Emacs's show-paren-mode: the opener point
;; sits on, or the closer just before it.  Brackets inside strings and
;; comments don't count, per the source's syntax styles; in text
;; without a mode every bracket counts.

(import (only (foundation edoc) elibrary))
(elibrary (apps paren)
  (export init! (rename (matching-paren-style matching-style)))
  (import (chezscheme)
          (prefix (head mode) mode:)
          (prefix (head style) style:)
          (prefix (service doc) doc:))

  ;; The named looks for the matched pair, in the style DSL. Box draws a
  ;; line above and below the bracket -- the closest widely rendered
  ;; approximation of a frame; a terminal that really draws SGR 51 can
  ;; have it with (style:set! 'matching-paren '(framed)). Colored uses
  ;; the accent violet that also marks choices and resizes.
  (define matching-paren-style-table
    '((underline (underline))
      (box (underline overline))
      ;; Bold wears the terminal's regular text color: against the
      ;; usually colored delimiters, bold default-foreground text reads
      ;; as the pair lighting up without changing hue family.
      (bold (bold (foreground default)))
      (colored (bold (foreground 135)))))

  (edoc "How the matched bracket pair is marked: bold, underline, box or colored."
        (value (one-of bold underline box colored)) (public))
  (define matching-paren-style (make-parameter 'bold
                                 (lambda (name)
                                   (let ([hit (assq name matching-paren-style-table)])
                                     (unless hit
                                       (error 'matching-paren-style
                                         "must be underline, box, bold, or colored" name))
                                     (style:set! 'matching-paren (cadr hit))
                                     name))))

  (define (scan-paren lines styles-of start-row start-col dir)
    ;; Find the bracket balancing the one at (start-row, start-col),
    ;; scanning forward (dir 1) or backward (dir -1).  The scan is bounded
    ;; so pathological buffers stay responsive; #f when nothing balances.
    (define count (vector-length lines))
    (define (line r) (vector-ref lines r))
    (define pairs (if (> dir 0) '((#\( . #\)) (#\[ . #\]) (#\{ . #\})) '((#\) . #\() (#\] . #\[) (#\} . #\{))))
    (let walk ([row start-row] [col start-col]
               [styles (styles-of (line start-row))]
               [stack '()] [budget 50000])
      (and (> budget 0)
           (if (or (< col 0) (>= col (string-length (line row))))
               (let ([row (+ row dir)])
                 (and (>= row 0) (< row count)
                      (walk row
                            (if (> dir 0) 0 (- (string-length (line row)) 1))
                            (styles-of (line row))
                            stack (- budget 1))))
               (let* ([c (string-ref (line row) col)]
                      [delimiter? (or (not styles) (eq? (vector-ref styles col) 'delimiter))]
                      [opener (and delimiter? (assv c pairs))])
                 (cond
                   [opener (walk row (+ col dir) styles (cons (cdr opener) stack) (- budget 1))]
                   [(and delimiter? (memv c '(#\( #\[ #\{ #\) #\] #\})))
                    (and (pair? stack) (char=? c (car stack))
                      (if (null? (cdr stack)) (cons row col)
                        (walk row (+ col dir) styles (cdr stack) (- budget 1))))]
                   [else (walk row (+ col dir) styles stack (- budget 1))]))))))

  (define (paren-highlights source mode pt)
    ;; The bracket at point and its partner, as logical span/face pairs;
    ;; empty when neither applies.
    (let* ([lines (mode:source-lines source)]
           [styles-of (mode:line-styles mode)]
           [row (car pt)]
           [line (vector-ref lines row)]
           [styles (styles-of line)])
      (define (bracket-at col kinds)
        (and (>= col 0) (< col (string-length line))
             (memv (string-ref line col) kinds)
             (or (not styles) (eq? (vector-ref styles col) 'delimiter))
             col))
      (let* ([closer (bracket-at (- (cdr pt) 1) '(#\) #\] #\}))]
             [opener (and (not closer) (bracket-at (cdr pt) '(#\( #\[ #\{)))]
             [col (or closer opener)]
             [match (and col (scan-paren lines styles-of row col (if closer -1 1)))])
        (if match
            (list (list (list row col row (+ col 1)) 'matching-paren)
                  (list (list (car match) (cdr match) (car match) (+ (cdr match) 1)) 'matching-paren))
            '()))))

  (edoc "Install the matching-bracket highlighter and its describe entry." (public))
  (define (init!)
    (mode:add-highlighter! paren-highlights)
    (doc:register!
      '(((paren:matching-style)
         (("parameter" . "(paren:matching-style [name])")) "symbol"
         ("(apps paren)") paren "Editing" #f
         "Get or set how the matched bracket pair is marked: bold (the default; bold in the regular text color), underline, box (a line above and below), or colored (bold accent violet).")))))
