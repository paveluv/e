;; text.e -- the pure text and span algebra: the library (text), the
;; first v2 module (dev/DESIGN2.md, stage 1).
;;
;; No state and no editor knowledge live here: a text is an immutable
;; vector of line strings, a position is a (line . column) pair, a
;; span is a half-open [start, end) region between two positions.
;; apply-edit returns a fresh text plus a delta -- the record of what
;; changed -- and everything else is derived from deltas: rebasing
;; other actors' positions and spans across an edit, and inverting an
;; edit for undo.
;;
;; The settled v2 semantics (dev/DESIGN2.md):
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
  (export splice
          make-span span? span-start span-end
          normalize-span span-empty? contains? overlap?
          position<? position<=? position=?
          apply-edit extract invert
          delta? delta-span delta-new-end delta-removed
          delta-line-shift
          rebase-position rebase-span)
  (import (rnrs) (only (chezscheme) format))

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
            removed))   ; the replaced content, as replacement lines

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
                (make-delta s new-end (extract text s))))))

  (define (invert d)
    ;; the edit that undoes a delta: -> (values span replacement)
    (values (span-of-positions (span-start (delta-span d))
                               (delta-new-end d))
            (delta-removed d)))

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

  (define (rebase-span s d)
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
              ;; one point: both endpoints move together
              (let ([p (rebase-position (span-start s) d)])
                (span-of-positions p p))
              (span-of-positions
                (rebase-position (span-start s) d)
                (rebase-position (span-end s) d 'stay))))))
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
