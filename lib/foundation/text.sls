;; text.sls -- the pure text and span algebra: the library (text).
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

(import (only (foundation edoc) elibrary))
(elibrary (foundation text)
  (export line? normalize from-string to-string content=? splice
          make-span span? span-start span-end
          span->datum datum->span delta->datum datum->delta
          normalize-span span-empty? contains? overlap?
          position? position<? position<=? position=?
          apply-edit extract invert invert-delta difference
          delta? delta-span delta-new-end delta-removed delta-inserted
          delta-line-shift
          rebase-position rebase-span rebase-delta rebase-result-position)
  (import (rnrs) (only (chezscheme) format) (prefix (foundation datum) datum:))

  ;;; Text boundaries ------------------------------------------------------

  (edoc "Whether a value is a line: a string without newlines."
        (value any "the value")
        (returns boolean))
  (define (line? value)
    (and (string? value)
         (let scan ([i 0])
           (or (= i (string-length value))
               (and (not (char=? (string-ref value i) #\newline)) (scan (+ i 1)))))))

  (edoc "A fresh line vector from a vector or list of lines, sharing the strings; an error for anything else."
        (lines (or vector list) "the lines")
        (returns vector))
  (define (normalize lines)
    ;; Own the vector while sharing immutable line strings.  Baselines
    ;; enter through this same boundary for local and shared buffers.
    (unless (or (vector? lines) (list? lines))
      (error 'normalize "expected a line vector or list" lines))
    (let ([items (if (vector? lines) (vector->list lines) lines)])
      (unless (for-all line? items)
        (error 'normalize "expected strings without embedded newlines" lines))
      (list->vector (if (null? items) '("") items))))

  (edoc "A text as lines plus whether it ended in a newline: (values lines trailing?)."
        (s string "the text"))
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

  (edoc "Lines joined with newlines, a final one when trailing?."
        (lines vector "the lines")
        (trailing? boolean "whether to end in a newline")
        (returns string))
  (define (to-string lines trailing?)
    (let ([n (vector-length lines)])
      (if (zero? n)
          (if trailing? "\n" "")
          (let loop ([i (- n 1)] [parts (if trailing? '("\n") '())])
            (let ([parts (cons (vector-ref lines i) parts)])
              (if (zero? i) (apply string-append parts)
                  (loop (- i 1) (cons "\n" parts))))))))

  (edoc "Whether two texts hold the same bytes, a final empty row without a trailing newline counting as the rows before it with one."
        (left vector "one text")
        (left-trailing? boolean "whether it ends in a newline")
        (right vector "the other text")
        (right-trailing? boolean "whether it ends in a newline")
        (returns boolean))
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

  (edoc "Whether a value is a (row . col) position of nonnegative fixnums."
        (p any "the value")
        (returns boolean))
  (define (position? p)
    (and (pair? p) (fixnum? (car p)) (fixnum? (cdr p))
         (>= (car p) 0) (>= (cdr p) 0)))

  (edoc "Whether one position comes before another."
        (a position "one position")
        (b position "the other")
        (returns boolean))
  (define (position<? a b)
    (or (< (car a) (car b))
        (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (edoc "Whether one position comes before another or equals it."
        (a position "one position")
        (b position "the other")
        (returns boolean))
  (define (position<=? a b)
    (not (position<? b a)))

  (edoc "Whether two positions are the same."
        (a position "one position")
        (b position "the other")
        (returns boolean))
  (define (position=? a b)
    (and (= (car a) (car b)) (= (cdr a) (cdr b))))

  (edoc "A half-open region of a text between two positions."
        (start position "where it starts")
        (end position "where it ends"))
  (define-record-type (span span-of-positions span?)
    (fields start end))

  (edoc "A span between two positions given as coordinates, its ends put in order."
        (start-line integer "the start row")
        (start-column integer "the start column")
        (end-line integer "the end row")
        (end-column integer "the end column")
        (returns (record span)))
  (define (make-span start-line start-column end-line end-column)
    (normalize-span
      (span-of-positions (cons start-line start-column)
                         (cons end-line end-column))))

  (edoc "A span with its endpoints in order."
        (s (record span) "the span")
        (returns (record span)))
  (define (normalize-span s)
    ;; endpoints in order, whichever way they were given
    (if (position<? (span-end s) (span-start s))
        (span-of-positions (span-end s) (span-start s))
        s))

  (edoc "Whether a span holds no content."
        (s (record span) "the span")
        (returns boolean))
  (define (span-empty? s)
    (position=? (span-start s) (span-end s)))

  (edoc "A span as (start-row start-col end-row end-col)."
        (s (record span) "the span")
        (returns list))
  (define (span->datum s)
    (list (car (span-start s)) (cdr (span-start s))
          (car (span-end s)) (cdr (span-end s))))

  (edoc "A span from four nonnegative coordinates."
        (value list "(start-row start-col end-row end-col)")
        (returns (record span)))
  (define (datum->span value)
    (unless (and (list? value) (= (length value) 4)
                 (for-all (lambda (n) (and (fixnum? n) (>= n 0))) value))
      (error 'datum->span "expected four nonnegative coordinates" value))
    (apply make-span value))

  (edoc "Whether a position lies strictly inside a span's half-open region."
        (s (record span) "the span")
        (position position "the position")
        (returns boolean))
  (define (contains? s position)
    ;; strictly inside the half-open region
    (and (position<=? (span-start s) position)
         (position<? position (span-end s))))

  (edoc "Whether two half-open spans share content; an empty span overlaps nothing."
        (a (record span) "one span")
        (b (record span) "the other")
        (returns boolean))
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

  (edoc "One edit as applied: what it replaced and what it inserted, enough to invert or rebase it."
        (span (record span) "the replaced span, in the old text")
        (new-end position "where the replacement ends, in the new text")
        (removed list "the replaced content, as replacement lines")
        (inserted list "the replacement lines"))
  (define-record-type (delta make-delta delta?)
    (fields span        ; the replaced span, in the old text
            new-end     ; where the replacement ends, in the new text
            removed     ; the replaced content, as replacement lines
            inserted))  ; the replacement, so deltas can be inverted twice

  (define (replacement-end start lines)
    (let next ([lines lines] [row (car start)] [column (cdr start)])
      (if (null? (cdr lines)) (cons row (+ column (string-length (car lines))))
          (next (cdr lines) (+ row 1) 0))))

  (edoc "A delta as (span-datum removed-lines inserted-lines), copied."
        (d (record delta) "the delta")
        (returns list))
  (define (delta->datum d)
    ;; new-end is derived from start/inserted, never a second wire authority.
    (datum:copy (list (span->datum (delta-span d)) (delta-removed d) (delta-inserted d))))

  (edoc "A delta from (span-datum removed-lines inserted-lines), checked for consistency."
        (value list "the datum")
        (returns (record delta)))
  (define (datum->delta value)
    (unless (and (list? value) (= (length value) 3)
                 (for-all (lambda (lines) (and (list? lines) (pair? lines) (for-all line? lines)))
                          (cdr value)))
      (error 'datum->delta "expected span, removed and inserted lines" value))
    (let* ([s (datum->span (car value))] [removed (datum:copy (cadr value))]
           [inserted (datum:copy (caddr value))] [start (span-start s)])
      (unless (position=? (span-end s) (replacement-end start removed))
        (error 'datum->delta "removed lines disagree with the span" value))
      (make-delta s (replacement-end start inserted) removed inserted)))

  (edoc "How many lines a delta added, negative for removed."
        (d (record delta) "the delta")
        (returns integer))
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

  (edoc "A span's content as replacement lines."
        (text vector "the lines")
        (s (record span) "the span")
        (returns list))
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

  (edoc "Replace a span of a text with lines: (values new-text delta)."
        (text vector "the lines")
        (s (record span) "the span replaced")
        (replacement list "nonempty replacement lines"))
  (define (apply-edit text s replacement)
    ;; -> (values new-text delta)
    (unless (and (list? replacement) (pair? replacement)
                 (for-all line? replacement))
      (error 'apply-edit "replacement must be nonempty line strings without embedded newlines"
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
             [new-end (replacement-end start replacement)]
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
              ;; walk the middle pieces once: a paste of N lines costs N steps
              (let middle ([k 1] [rest (cdr replacement)])
                (when (< k (- pieces 1))
                  (vector-set! new-text (+ start-line k) (car rest))
                  (middle (+ k 1) (cdr rest))))
              (vector-set! new-text (+ start-line (- pieces 1))
                           last-piece)))
        ;; untouched tail
        (do ([i (+ end-line 1) (+ i 1)]
             [j (+ start-line pieces) (+ j 1)])
            ((= i (vector-length text)))
          (vector-set! new-text j (vector-ref text i)))
        (values new-text
                (make-delta s new-end (extract text s) replacement)))))

  (edoc "The edit undoing a delta: (values span replacement)."
        (d (record delta) "the delta"))
  (define (invert d)
    ;; the edit that undoes a delta: -> (values span replacement)
    (values (span-of-positions (span-start (delta-span d))
                               (delta-new-end d))
            (delta-removed d)))

  (edoc "The inverse of a delta as a delta, content included."
        (d (record delta) "the delta")
        (returns (record delta)))
  (define (invert-delta d)
    ;; The inverse as another delta, including its content.  Inverting
    ;; twice recovers the original operation; no document snapshot is
    ;; needed to reason about an edit followed by its compensation.
    (make-delta (span-of-positions (span-start (delta-span d))
                                   (delta-new-end d))
                (span-end (delta-span d))
                (delta-inserted d) (delta-removed d)))

  (edoc "The smallest single replacement taking one text to another: (values span replacement); identical texts give an empty edit at the end."
        (before vector "the old lines")
        (after vector "the new lines"))
  (define (difference before after)
    ;; The smallest single replacement taking before to after: trim a
    ;; common prefix, then a non-overlapping common suffix.  Walk line
    ;; vectors with their implicit newlines, without flattening/copying
    ;; the whole document. Skip equal rows at a time so a change at the
    ;; end of a long transcript does not allocate per unchanged character.
    ;; Identical texts return an empty edit at EOF.
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
        (cond
          [(and (zero? (cdr start))
                (< (car start) (min (car before-end) (car after-end)))
                (string=? (vector-ref before (car start)) (vector-ref after (car start))))
           (prefix (cons (+ (car start) 1) 0))]
          [(and (position<? start before-end) (position<? start after-end)
                (char=? (at before start) (at after start)))
           (prefix (next before start))]
          [else
           (let suffix ([old-end before-end] [new-end after-end])
             (cond
               [(and (> (car old-end) (car start)) (> (car new-end) (car start))
                     (= (cdr old-end) (string-length (vector-ref before (car old-end))))
                     (= (cdr new-end) (string-length (vector-ref after (car new-end))))
                     (string=? (vector-ref before (car old-end)) (vector-ref after (car new-end))))
                (suffix (previous before (cons (car old-end) 0))
                        (previous after (cons (car new-end) 0)))]
               [(and (position<? start old-end) (position<? start new-end)
                     (char=? (at before (previous before old-end))
                             (at after (previous after new-end))))
                (suffix (previous before old-end) (previous after new-end))]
               [else (values (span-of-positions start old-end)
                             (extract after (span-of-positions start new-end)))]))]))))

  ;;; Rebasing --------------------------------------------------------------

  (edoc "A position of the old text mapped through a delta; inside the replaced region it collapses to the edit's end, or its start under the stay bias."
        (position position "the position")
        (d (record delta) "the delta")
        (bias (list-of (one-of stay)) "stay to hold a mark at an insertion point, at most one")
        (returns position))
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

  (edoc "A span mapped through a disjoint delta, or #f when the change touched its content."
        (s (record span) "the span")
        (d (record delta) "the delta")
        (bias (list-of (one-of stay)) "stay for insertion priority, at most one")
        (returns (or (record span) #f)))
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

  (edoc "A delta carried through a disjoint edit, its removed and inserted text retained, or #f when they overlap."
        (d (record delta) "the delta")
        (across (record delta) "the edit to move past")
        (bias (list-of (one-of stay)) "stay for insertion priority, at most one")
        (returns (or (record delta) #f)))
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

  (edoc "A position chosen in an intended edit's result, mapped into the accepted edit's result after intervening deltas."
        (position position "the chosen position")
        (intended (record delta) "the edit as proposed")
        (actual (record delta) "the edit as accepted")
        (before (list-of (record delta)) "the deltas that came between")
        (returns position))
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

  (edoc "A fresh vector with the elements [from, to) replaced by a list."
        (v vector "the vector")
        (from integer "the first index replaced")
        (to integer "the index after the last")
        (inserted list "the replacement")
        (returns vector))
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
