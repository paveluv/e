;; text.e -- the pure text and span algebra: the library (text).
;;
;; No state and no editor knowledge live here: a text is an immutable
;; vector of line strings, a position is a (line . column) pair, a
;; span is a half-open [start, end) region between two positions.
;; apply-edit returns a fresh text plus a delta -- the record of what
;; changed -- and everything else is derived from deltas: rebasing
;; other actors' positions and spans across an edit, and inverting an
;; edit for undo.
;;
;; The settled semantics:
;; - rebasing is line-based: positions strictly after the edit shift
;;   by whole lines (plus a column shift on the edit's last line);
;; - a position inside the replaced region collapses to the edit's
;;   end -- or its start under the 'stay bias;
;; - rebasing an edit *span* is strict: any overlap with the changed
;;   region returns #f (stale) instead of guessing.
;;
;; Naming reads behind the import prefix: (text:apply-edit ...),
;; (text:rebase-position ...).

(library (text)
  (export normalize from-string to-string content=? splice
          make-span span? span-start span-end
          normalize-span span-empty? contains? overlap?
          position? position<? position<=? position=?
          apply-edit extract invert invert-delta difference
          delta? delta-span delta-new-end delta-removed delta-inserted
          delta-line-shift
          rebase-position rebase-span rebase-delta rebase-result-position)
  (import (rnrs) (only (chezscheme) format))

  ;;; Text boundaries ------------------------------------------------------

  (define (normalize lines)
    ;; Own the vector while sharing immutable line strings.  Baselines
    ;; enter through this same boundary for local and shared buffers.
    (unless (or (vector? lines) (list? lines))
      (error 'normalize "expected a line vector or list" lines))
    (let ([items (if (vector? lines) (vector->list lines) lines)])
      (unless (for-all string? items)
        (error 'normalize "expected line strings" lines))
      (list->vector (if (null? items) '("") items))))

  (define (from-string s)
    ;; File contents as lines plus the final-newline fact.  Keeping this
    ;; pure lets the store compare a disk baseline without doing I/O.
    (let* ([n (string-length s)]
           [trailing? (and (> n 0) (char=? (string-ref s (- n 1)) #\newline))]
           [end (if trailing? (- n 1) n)])
      (let loop ([i 0] [start 0] [lines '()])
        (cond [(= i end)
               (values (list->vector (reverse (cons (substring s start end) lines))) trailing?)]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) lines))]
              [else (loop (+ i 1) start lines)]))))

  (define (to-string lines trailing?)
    (let ([n (vector-length lines)])
      (if (zero? n)
          (if trailing? "\n" "")
          (let loop ([i (- n 1)] [parts (if trailing? '("\n") '())])
            (let ([parts (cons (vector-ref lines i) parts)])
              (if (zero? i) (apply string-append parts)
                  (loop (- i 1) (cons "\n" parts))))))))

  (define (content=? left left-trailing? right right-trailing?)
    ;; A final empty row without a trailing newline represents the same
    ;; bytes as the preceding rows WITH one.  Compare those normal forms
    ;; without allocating strings or copying a whole text on every edit.
    (define (normal-length lines trailing?)
      (let ([n (vector-length lines)])
        (if (and (not trailing?) (> n 1) (string=? (vector-ref lines (- n 1)) ""))
            (- n 1) n)))
    (let ([nl (normal-length left left-trailing?)]
          [nr (normal-length right right-trailing?)])
      (and (= nl nr)
           (eq? (or left-trailing? (< nl (vector-length left)))
                (or right-trailing? (< nr (vector-length right))))
           (let same ([i 0])
             (or (= i nl)
                 (and (string=? (vector-ref left i) (vector-ref right i))
                      (same (+ i 1))))))))

  ;;; Positions and spans --------------------------------------------------

  ;; A position is (line . column), both zero-based.  A span is
  ;; half-open: it covers [start, end) and an empty span (start = end)
  ;; is a bare insertion point.

  (define (position? p)
    (and (pair? p) (fixnum? (car p)) (fixnum? (cdr p))
         (>= (car p) 0) (>= (cdr p) 0)))

  (define (position<? a b)
    (or (< (car a) (car b))
        (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (define (position<=? a b) (not (position<? b a)))

  (define (position=? a b)
    (and (= (car a) (car b)) (= (cdr a) (cdr b))))

  (define-record-type (span span-of-positions span?)
    (fields start end))

  (define (make-span start-line start-column end-line end-column)
    (normalize-span
      (span-of-positions (cons start-line start-column)
                         (cons end-line end-column))))

  (define (normalize-span s)
    ;; endpoints in order, whichever way they were given
    (if (position<? (span-end s) (span-start s))
        (span-of-positions (span-end s) (span-start s))
        s))

  (define (span-empty? s) (position=? (span-start s) (span-end s)))

  (define (contains? s position)
    ;; strictly inside the half-open region
    (and (position<=? (span-start s) position)
         (position<? position (span-end s))))

  (define (overlap? a b)
    ;; do the half-open regions share any content?  An empty span
    ;; overlaps nothing (an insertion point has no content), but a
    ;; non-empty span does contain an insertion point strictly inside
    ;; it -- callers who care use contains?.
    (and (not (span-empty? a)) (not (span-empty? b))
         (position<? (span-start a) (span-end b))
         (position<? (span-start b) (span-end a))))

  ;;; Edits -----------------------------------------------------------------

  ;; A text is an immutable vector of line strings.  apply-edit
  ;; replaces a span with replacement lines (a non-empty list of
  ;; strings; a single string inserts no newline) and returns the new
  ;; text plus the delta.  Line strings are shared, never mutated.

  (define-record-type (delta make-delta delta?)
    (fields span        ; the replaced span, in the old text
            new-end     ; where the replacement ends, in the new text
            removed     ; the replaced content, as replacement lines
            inserted))  ; the replacement, so deltas can be inverted twice

  (define (delta-line-shift d)
    (- (car (delta-new-end d)) (car (span-end (delta-span d)))))

  (define (check-span who text s)
    (let ([start (span-start s)] [end (span-end s)]
          [lines (vector-length text)])
      (define (check-position p)
        (unless (and (position? p)
                     (< (car p) lines)
                     (<= (cdr p) (string-length
                                   (vector-ref text (car p)))))
          (error who (format "position ~a is outside the text" p))))
      (check-position start)
      (check-position end)))

  (define (extract text s)
    ;; the span's content, as replacement lines
    (let* ([s (normalize-span s)]
           [start (span-start s)] [end (span-end s)])
      (check-span 'extract text s)
      (if (= (car start) (car end))
          (list (substring (vector-ref text (car start))
                           (cdr start) (cdr end)))
          (let loop ([line (+ (car start) 1)]
                     [acc (list (substring
                                  (vector-ref text (car start))
                                  (cdr start)
                                  (string-length
                                    (vector-ref text (car start)))))])
            (if (= line (car end))
                (reverse
                  (cons (substring (vector-ref text line) 0 (cdr end))
                        acc))
                (loop (+ line 1)
                      (cons (vector-ref text line) acc)))))))

  (define (apply-edit text s replacement)
    ;; -> (values new-text delta)
    (unless (and (list? replacement) (pair? replacement)
                 (for-all string? replacement))
      (error 'apply-edit "replacement must be a non-empty string list"
             replacement))
    (let* ([s (normalize-span s)]
           [start (span-start s)] [end (span-end s)])
      (check-span 'apply-edit text s)
      (let* ([start-line (car start)] [start-column (cdr start)]
             [end-line (car end)] [end-column (cdr end)]
             [prefix (substring (vector-ref text start-line)
                                0 start-column)]
             [suffix (let ([line (vector-ref text end-line)])
                       (substring line end-column
                                  (string-length line)))]
             [pieces (length replacement)]
             [first-piece (string-append prefix (car replacement))]
             [last-piece
              (if (= pieces 1)
                  (string-append first-piece suffix)
                  (string-append (list-ref replacement (- pieces 1))
                                 suffix))]
             [new-end
              (cons (+ start-line (- pieces 1))
                    (if (= pieces 1)
                        (+ start-column (string-length (car replacement)))
                        (string-length
                          (list-ref replacement (- pieces 1)))))]
             [new-line-count (+ (vector-length text)
                                (- pieces 1)
                                (- (- end-line start-line)))]
             [new-text (make-vector new-line-count)])
        ;; untouched head
        (do ([i 0 (+ i 1)]) ((= i start-line))
          (vector-set! new-text i (vector-ref text i)))
        ;; the replacement block
        (if (= pieces 1)
            (vector-set! new-text start-line last-piece)
            (begin
              (vector-set! new-text start-line first-piece)
              (do ([k 1 (+ k 1)]) ((= k (- pieces 1)))
                (vector-set! new-text (+ start-line k)
                             (list-ref replacement k)))
              (vector-set! new-text (+ start-line (- pieces 1))
                           last-piece)))
        ;; untouched tail
        (do ([i (+ end-line 1) (+ i 1)]
             [j (+ start-line pieces) (+ j 1)])
            ((= i (vector-length text)))
          (vector-set! new-text j (vector-ref text i)))
        (values new-text
                (make-delta s new-end (extract text s) replacement)))))

  (define (invert d)
    ;; the edit that undoes a delta: -> (values span replacement)
    (values (span-of-positions (span-start (delta-span d))
                               (delta-new-end d))
            (delta-removed d)))

  (define (invert-delta d)
    ;; The inverse as another delta, including its content.  Inverting
    ;; twice recovers the original operation; no document snapshot is
    ;; needed to reason about an edit followed by its compensation.
    (make-delta (span-of-positions (span-start (delta-span d))
                                   (delta-new-end d))
                (span-end (delta-span d))
                (delta-inserted d) (delta-removed d)))

  (define (difference before after)
    ;; The smallest single replacement taking before to after: trim a
    ;; common prefix, then a non-overlapping common suffix.  Walk line
    ;; vectors with their implicit newlines, without flattening/copying
    ;; the whole document.  Identical texts return an empty edit at EOF.
    (define (end-of lines)
      (let ([row (- (vector-length lines) 1)])
        (cons row (string-length (vector-ref lines row)))))
    (define (at lines p)
      (let ([line (vector-ref lines (car p))])
        (if (= (cdr p) (string-length line)) #\newline
            (string-ref line (cdr p)))))
    (define (next lines p)
      (if (= (cdr p) (string-length (vector-ref lines (car p))))
          (cons (+ (car p) 1) 0)
          (cons (car p) (+ (cdr p) 1))))
    (define (previous lines p)
      (if (positive? (cdr p))
          (cons (car p) (- (cdr p) 1))
          (let ([row (- (car p) 1)])
            (cons row (string-length (vector-ref lines row))))))
    (let ([before-end (end-of before)] [after-end (end-of after)])
      (let prefix ([start '(0 . 0)])
        (if (and (position<? start before-end) (position<? start after-end)
                 (char=? (at before start) (at after start)))
            (prefix (next before start))
            (let suffix ([old-end before-end] [new-end after-end])
              (if (and (position<? start old-end) (position<? start new-end)
                       (char=? (at before (previous before old-end))
                               (at after (previous after new-end))))
                  (suffix (previous before old-end) (previous after new-end))
                  (values (span-of-positions start old-end)
                          (extract after (span-of-positions start new-end)))))))))

  ;;; Rebasing --------------------------------------------------------------

  (define (rebase-position position d . bias)
    ;; Map a position in the old text to the new one.  Inside the
    ;; replaced region it collapses to the edit's end -- or to its
    ;; start under the 'stay bias, which also keeps a mark sitting
    ;; exactly at an insertion point in place.
    (let* ([stay (and (pair? bias) (eq? (car bias) 'stay))]
           [s (delta-span d)]
           [start (span-start s)] [end (span-end s)]
           [new-end (delta-new-end d)])
      (cond
        [(position<? position start) position]
        [(and stay (position=? position start)) position]
        [(position<? position end) (if stay start new-end)]
        [(position=? position end) new-end]
        [(= (car position) (car end))
         ;; the tail of the edit's last line moves with it
         (cons (car new-end)
               (+ (cdr new-end) (- (cdr position) (cdr end))))]
        [else
         (cons (+ (car position) (delta-line-shift d))
               (cdr position))])))

  (define (strictly-inside? position s)
    (and (position<? (span-start s) position)
         (position<? position (span-end s))))

  (define (rebase-span s d . bias)
    ;; Map an edit's span across a delta -- strictly: any overlap with
    ;; the changed content, an insertion strictly inside the span, or
    ;; the span's own insertion point swallowed by the change, returns
    ;; #f (stale) rather than guessing.  A surviving span still covers
    ;; the same content: its start chases the content forward past an
    ;; insertion at its left edge, while its exclusive end refuses to
    ;; absorb text inserted exactly at it.
    (let* ([s (normalize-span s)]
           [changed (delta-span d)])
      (if (or (overlap? s changed)
              (and (span-empty? s)
                   (strictly-inside? (span-start s) changed))
              (and (span-empty? changed)
                   (strictly-inside? (span-start changed) s)))
          #f
          (if (span-empty? s)
              ;; An insertion at the left edge of replaced content
              ;; stays before that content's replacement.  Only two
              ;; insertions at the same point need an ordering bias.
              ;; Cursor rebasing still uses its normal forward bias.
              (let ([p (if (and (not (span-empty? changed))
                                (position=? (span-start s) (span-start changed)))
                           (span-start changed)
                           (apply rebase-position (span-start s) d bias))])
                (span-of-positions p p))
              (span-of-positions
                (rebase-position (span-start s) d)
                (rebase-position (span-end s) d 'stay))))))

  (define (rebase-delta d across . bias)
    ;; Carry an operation through a disjoint edit, retaining both its
    ;; removed and inserted text.  'stay gives an existing insertion
    ;; priority when commuting a later inverse backwards past it.
    (let ([s (apply rebase-span (delta-span d) across bias)])
      (and s
           (let* ([old-start (span-start (delta-span d))]
                  [old-end (delta-new-end d)]
                  [start (span-start s)]
                  [rows (- (car old-end) (car old-start))])
             (make-delta s
                         (cons (+ (car start) rows)
                               (if (zero? rows)
                                   (+ (cdr start) (- (cdr old-end) (cdr old-start)))
                                   (cdr old-end)))
                         (delta-removed d) (delta-inserted d))))))

  (define (rebase-result-position position intended actual before)
    ;; A command chooses a position in its intended edit's result.  The
    ;; accepted edit may have moved through BEFORE's intervening deltas.
    ;; Inside the replacement, preserve the offset into that same inserted
    ;; text (including both boundaries).  Outside it, recover the original
    ;; content anchor and follow the real chain.  ACTUAL must be INTENDED
    ;; rebased through BEFORE; later deltas use ordinary rebase-position.
    (let ([start (span-start (delta-span intended))]
          [end (delta-new-end intended)]
          [landed (span-start (delta-span actual))])
      (if (and (position<=? start position) (position<=? position end))
          (let ([rows (- (car position) (car start))])
            (cons (+ (car landed) rows)
                  (if (zero? rows)
                      (+ (cdr landed) (- (cdr position) (cdr start)))
                      (cdr position))))
          (rebase-position
            (fold-left rebase-position
                       (rebase-position position (invert-delta intended))
                       before)
            actual))))
  ;;; Line-vector splicing -----------------------------------------------------------

  (define (splice v from to inserted)
    ;; A fresh vector: v's elements [from, to) replaced by the list
    ;; inserted; v itself is untouched (line vectors are immutable).
    (let* ([tail (- (vector-length v) to)]
           [ins (list->vector inserted)]
           [out (make-vector (+ from (vector-length ins) tail))])
      (do ([i 0 (+ i 1)]) ((= i from)) (vector-set! out i (vector-ref v i)))
      (do ([i 0 (+ i 1)]) ((= i (vector-length ins)))
        (vector-set! out (+ from i) (vector-ref ins i)))
      (do ([i 0 (+ i 1)]) ((= i tail))
        (vector-set! out (+ from (vector-length ins) i) (vector-ref v (+ to i))))
      out)))
