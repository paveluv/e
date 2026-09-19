;; table.sls -- the shared column rules for local picker apps.
;; Pure formatting and ordered sort keys; rows, selection and input stay
;; with the app. A layout belongs to a window, never to its shared buffer.
(import (only (edoc) elibrary))
(elibrary (table)
  (export make heading cycle-sort less? layout)
  (import (chezscheme) (prefix (glyph) glyph:) (prefix (string) string:))

  (edoc "A table's columns: their headings, minimum widths, which column identifies a row, which may be dropped when narrow, and how cells align."
        (headings vector "the column headings")
        (minimum vector "the minimum width of each column")
        (keep integer "the column kept whatever the width")
        (drop list "the columns to drop first, in order")
        (alignment vector "text, right or tail per column"))
  (define-record-type (table make table?)
    (fields headings minimum keep drop alignment))

  (edoc "The sort keys after a column's heading is pressed: ascending, then descending, then off; direction changes keep priority."
        (keys list "(column . descending?) in priority order")
        (column integer "the column")
        (returns list))
  (define (cycle-sort keys column)
    ;; Ascending -> descending -> off. Direction retains priority; enabling
    ;; a new or previously disabled key appends it to the compound order.
    (let ([key (assv column keys)])
      (cond [(not key) (append keys (list (cons column #f)))]
            [(cdr key) (remq key keys)]
            [else (map (lambda (k) (if (eq? k key) (cons column #t) k)) keys)])))

  (edoc "A column's heading with its sort priority in superscript and direction arrow when it is a key."
        (table (record table) "the table")
        (keys list "the sort keys")
        (column integer "the column")
        (returns string))
  (define (heading table keys column)
    (string-append (vector-ref (table-headings table) column)
      (let loop ([keys keys] [priority 1])
        (cond [(null? keys) ""]
              [(= column (caar keys))
               (string-append
                 (list->string
                   (map (lambda (c) (string-ref "⁰¹²³⁴⁵⁶⁷⁸⁹" (- (char->integer c) 48)))
                     (string->list (number->string priority))))
                 (if (cdar keys) "↓" "↑"))]
              [else (loop (cdr keys) (+ priority 1))]))))

  (define (value<? a b)
    (cond [(not a) (and b #t)]
          [(not b) #f]
          [(boolean? a) #f]
          [(number? a) (< a b)]
          [else (string-ci<? a b)]))

  (edoc "Whether row a sorts before row b by the keys, the fallback deciding ties."
        (keys list "the sort keys")
        (value procedure "(value row column) giving a cell value")
        (fallback procedure "(fallback a b) for ties")
        (a any "one row")
        (b any "the other")
        (returns boolean))
  (define (less? keys value fallback a b)
    (let compare ([keys keys])
      (if (null? keys) (fallback a b)
          (let ([x (value a (caar keys))] [y (value b (caar keys))])
            (cond [(value<? x y) (not (cdar keys))]
                  [(value<? y x) (cdar keys)]
                  [else (compare (cdr keys))])))))

  (edoc "Fit a table into a width from its unfiltered rows: (values row columns), row a procedure formatting a row's data, columns the shown (column start end) spans."
        (table (record table) "the table")
        (keys list "the sort keys")
        (all list "every row's data")
        (cell procedure "(cell data column) giving a cell's text")
        (width integer "the columns available"))
  (define (layout table keys all cell width)
    ;; Size from the unfiltered rows so typing does not make columns jump.
    ;; Retain the identity column; drop unsorted metadata before sort keys.
    ;; Growth is bounded by the pane, not by the longest name on disk.
    (let* ([minimum (table-minimum table)] [sizes (vector-copy minimum)]
           [keep (table-keep table)] [indices (iota (vector-length minimum))]
           [columns
            (let fit ([columns indices]
                      [drop (append (filter (lambda (i) (not (assv i keys))) (table-drop table))
                              (remv keep (reverse (map car keys))))])
              (if (or (null? drop)
                      (<= (+ (* 2 (- (length columns) 1))
                             (apply + (map (lambda (i) (vector-ref minimum i)) columns))) width))
                  columns
                  (fit (remv (car drop) columns) (cdr drop))))]
           [natural
            (list->vector
              (map (lambda (i)
                     (fold-left (lambda (n row) (max n (glyph:cells (cell row i))))
                       (vector-ref minimum i) all)) indices))])
      (define (row data)
        (string:join
          (map (lambda (i)
                 (let* ([text (if data (cell data i) (heading table keys i))]
                        [size (vector-ref sizes i)]
                        [align (and data (vector-ref (table-alignment table) i))])
                   (if (eq? align 'right)
                       (let ([n (glyph:cells text)])
                         (if (> n size) (glyph:fit text size 'left)
                             (string-append (make-string (- size n) #\space) text)))
                       (glyph:fit text size (if (eq? align 'tail) 'left 'right)))))
            columns) "  "))
      (when (null? (cdr columns)) (vector-set! sizes keep width))
      (let grow ([room (- width (* 2 (- (length columns) 1))
                         (apply + (map (lambda (i) (vector-ref sizes i)) columns)))])
        (let ([want (filter (lambda (i) (< (vector-ref sizes i) (vector-ref natural i))) columns)])
          (when (and (> room 0) (pair? want))
            (let ([given (list-head want (min room (length want)))])
              (for-each (lambda (i) (vector-set! sizes i (+ 1 (vector-ref sizes i)))) given)
              (grow (- room (length given)))))))
      (values row
        (let bounds ([columns columns] [start 0])
          (if (null? columns) '()
              (let ([end (+ start (vector-ref sizes (car columns)))])
                (cons (list (car columns) start end) (bounds (cdr columns) (+ end 2)))))))))
)
