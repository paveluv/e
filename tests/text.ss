#!/usr/bin/env scheme-script

;; The pure text algebra: spans, edits, deltas, inversion, and
;; rebasing.  Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (text) text:) (prefix (string) string:)
             (prefix (test) test:))

     (define check test:check)

     (define (span sl sc el ec) (text:make-span sl sc el ec))
     (define (span->list s)
       (list (text:span-start s) (text:span-end s)))

     (define base '#("alpha bravo" "charlie" "delta echo" "foxtrot"))

     (check 'equivalent-final-empty-row
            (text:content=? '#("a" "") #f '#("a") #t) #t)
     (check 'blank-line-is-significant
            (text:content=? '#("a" "") #t '#("a") #t) #f)
     (check 'empty-file-differs-from-newline
            (text:content=? '#("") #f '#("") #t) #f)
     (check 'different-lines-with-same-final-newline
            (text:content=? '#("a" "") #f '#("b") #t) #f)

     ;; -- spans ----------------------------------------------------------

     (check 'span-normalizes
            (span->list (span 2 3 1 0))
            '((1 . 0) (2 . 3)))

     (check 'span-emptiness
            (list (text:span-empty? (span 1 2 1 2))
                  (text:span-empty? (span 1 2 1 3)))
            '(#t #f))

     (check 'containment
            (list (text:contains? (span 1 0 2 5) '(1 . 0))
                  (text:contains? (span 1 0 2 5) '(2 . 5))
                  (text:contains? (span 1 0 2 5) '(2 . 4)))
            '(#t #f #t))

     (check 'overlap
            (list (text:overlap? (span 0 0 1 0) (span 1 0 2 0))
                  (text:overlap? (span 0 0 1 1) (span 1 0 2 0))
                  (text:overlap? (span 1 2 1 2) (span 0 0 9 0)))
            '(#f #t #f))

     ;; -- extraction ------------------------------------------------------

     (check 'extract-within-line
            (text:extract base (span 0 6 0 11))
            '("bravo"))

     (check 'extract-across-lines
            (text:extract base (span 0 6 2 5))
            '("bravo" "charlie" "delta"))

     (check 'extract-empty
            (text:extract base (span 1 3 1 3))
            '(""))

     ;; -- edits -----------------------------------------------------------

     (define (apply-to text sl sc el ec replacement)
       (let-values ([(new-text delta)
                     (text:apply-edit text (span sl sc el ec)
                                      replacement)])
         (list (vector->list new-text) delta)))

     (check 'replace-within-line
            (car (apply-to base 0 6 0 11 '("BRAVO")))
            '("alpha BRAVO" "charlie" "delta echo" "foxtrot"))

     (check 'insert-at-point
            (car (apply-to base 1 7 1 7 '("!")))
            '("alpha bravo" "charlie!" "delta echo" "foxtrot"))

     (check 'delete-across-lines
            (car (apply-to base 0 6 2 6 '("")))
            '("alpha echo" "foxtrot"))

     (check 'split-a-line
            (car (apply-to base 1 3 1 3 '("" "")))
            '("alpha bravo" "cha" "rlie" "delta echo" "foxtrot"))

     (check 'join-lines
            (car (apply-to base 0 11 1 0 '("")))
            '("alpha bravocharlie" "delta echo" "foxtrot"))

     (check 'multi-line-replacement
            (car (apply-to base 1 0 2 10 '("one" "two" "three")))
            '("alpha bravo" "one" "two" "three" "foxtrot"))

     (check 'whole-text-replacement
            (car (apply-to base 0 0 3 7 '("fresh")))
            '("fresh"))

     ;; A snapshot change is represented by one compact edit, preserving
     ;; unchanged prefix/suffix content and its marks when undo applies it.
     (define (difference-list before after)
       (let-values ([(span replacement) (text:difference before after)])
         (list (span->list span) replacement)))
     (check 'minimal-differences-across-characters-and-whole-rows
       (map (lambda (entry) (apply difference-list entry))
         '((#("abc") #("aXbc"))
           (#("aXbc") #("abc"))
           (#("ab") #("a" "b"))
           (#("a" "b") #("ab"))
           (#("a" "") #("a" ""))
           (#("first" "abc" "last") #("first" "aXbc" "last"))
           (#("first" "abc" "last" "") #("first" "aXbc" "last" ""))
           (#("x" "tail" "") #("a" "b" "tail" ""))
           (#("a" "" "tail") #("a" "tail"))))
       '((((0 . 1) (0 . 1)) ("X"))
         (((0 . 1) (0 . 2)) (""))
         (((0 . 1) (0 . 1)) ("" ""))
         (((0 . 1) (1 . 0)) (""))
         (((1 . 0) (1 . 0)) (""))
         (((1 . 1) (1 . 1)) ("X"))
         (((1 . 1) (1 . 1)) ("X"))
         (((0 . 0) (0 . 1)) ("a" "b"))
         (((1 . 0) (2 . 0)) (""))))

     ;; Exhaust all pairs of short documents, including empty rows and
     ;; newlines at either end.  This catches prefix/suffix overlap and
     ;; cross-line coordinate errors beyond the hand-picked examples.
     (define (short-strings depth)
       (if (zero? depth)
           '("")
           (cons "" (apply append
                      (map (lambda (prefix)
                             (map (lambda (ch) (string-append prefix (string ch)))
                                  '(#\a #\b #\newline)))
                           (short-strings (- depth 1)))))))
     (define samples (map (lambda (s) (list->vector (string:lines s))) (short-strings 3)))
     (check 'difference-round-trips-short-documents
            (for-all
              (lambda (before)
                (for-all
                  (lambda (after)
                    (let*-values ([(span replacement) (text:difference before after)]
                                  [(result delta) (text:apply-edit before span replacement)])
                      (let* ([decoded (text:datum->delta (text:delta->datum delta))]
                             [applied (call-with-values
                                        (lambda () (text:apply-edit before (text:delta-span decoded)
                                                     (text:delta-inserted decoded))) list)])
                        (and (equal? result after) (equal? (car applied) after)
                             (equal? (text:delta-removed decoded)
                                     (text:extract before (text:delta-span decoded)))))))
                  samples))
              samples)
            #t)

     (let-values ([(new-text delta)
                   (text:apply-edit base (span 1 0 2 10
                                         ) '("one" "two"))])
       (check 'delta-records-the-change
              (list (span->list (text:delta-span delta))
                    (text:delta-new-end delta)
                    (text:delta-removed delta)
                    (text:delta-line-shift delta))
              '(((1 . 0) (2 . 10)) (2 . 3) ("charlie" "delta echo") 0)))

     ;; -- inversion round-trips --------------------------------------------

     (define (round-trip sl sc el ec replacement)
       (let*-values ([(edited delta)
                      (text:apply-edit base (span sl sc el ec)
                                       replacement)]
                     [(inverse-span inverse-replacement)
                      (text:invert delta)]
                     [(restored _)
                      (text:apply-edit edited inverse-span
                                       inverse-replacement)])
         (equal? restored base)))

     (check 'inversion-restores
            (list (round-trip 0 6 0 11 '("BRAVO"))
                  (round-trip 1 7 1 7 '("!"))
                  (round-trip 0 6 2 6 '(""))
                  (round-trip 1 0 2 10 '("one" "two" "three"))
                  (round-trip 0 0 3 7 '("x" "y")))
            '(#t #t #t #t #t))

     ;; -- rebasing positions -----------------------------------------------

     ;; the delta: lines 1-2 replaced by two lines ending at (2 . 3)
     (define d
       (let-values ([(new-text delta)
                     (text:apply-edit base (span 1 2 2 5)
                                      '("NGE" "LO"))])
         delta))

     (check 'position-before-stays
            (text:rebase-position '(0 . 4) d) '(0 . 4))
     (check 'position-inside-collapses-to-end
            (text:rebase-position '(2 . 1) d) '(2 . 2))
     (check 'position-inside-stays-at-start
            (text:rebase-position '(2 . 1) d 'stay) '(1 . 2))
     (check 'position-at-edit-end
            (text:rebase-position '(2 . 5) d) '(2 . 2))
     (check 'tail-of-last-line-shifts-columns
            (text:rebase-position '(2 . 8) d) '(2 . 5))
     (check 'later-lines-shift
            (text:rebase-position '(3 . 4) d) '(3 . 4))

     (let-values ([(new-text shrink)
                   (text:apply-edit base (span 1 0 2 10 ) '("x"))])
       (check 'later-lines-shift-up
              (text:rebase-position '(3 . 2) shrink) '(2 . 2)))

     ;; insertion bias: a mark at the insertion point
     (let-values ([(new-text insertion)
                   (text:apply-edit base (span 1 3 1 3) '("XY"))])
       (check 'insertion-pushes-marks-forward
              (text:rebase-position '(1 . 3) insertion) '(1 . 5))
       (check 'insertion-stay-bias-holds-ground
              (text:rebase-position '(1 . 3) insertion 'stay) '(1 . 3)))

     ;; -- rebasing spans ----------------------------------------------------

     (check 'span-before-the-edit-survives
            (span->list (text:rebase-span (span 0 0 1 1) d))
            '((0 . 0) (1 . 1)))

     (check 'span-after-the-edit-shifts
            (span->list (text:rebase-span (span 3 1 3 4) d))
            '((3 . 1) (3 . 4)))

     (check 'overlapping-span-is-stale
            (text:rebase-span (span 1 0 1 5) d) #f)

     (check 'span-swallowing-the-edit-is-stale
            (text:rebase-span (span 0 0 3 0) d) #f)

     (let-values ([(new-text insertion)
                   (text:apply-edit base (span 1 3 1 3) '("XY"))])
       (check 'insertion-inside-a-span-is-stale
              (text:rebase-span (span 1 0 1 7) insertion) #f)
       (check 'insertion-at-span-start-chases-content
              (span->list (text:rebase-span (span 1 3 1 7) insertion))
              '((1 . 5) (1 . 9)))
       (check 'insertion-at-span-end-is-not-absorbed
              (span->list (text:rebase-span (span 1 0 1 3) insertion))
              '((1 . 0) (1 . 3)))
       (check 'cursor-span-after-insertion-shifts
              (span->list (text:rebase-span (span 1 4 1 4) insertion))
              '((1 . 6) (1 . 6)))
       ;; one point stays one point: concurrent insertions at the
       ;; same spot queue up instead of swallowing each other
       (check 'cursor-span-at-insertion-point-follows
              (span->list (text:rebase-span (span 1 3 1 3) insertion))
              '((1 . 5) (1 . 5))))

     ;; an empty span strictly inside a replaced region is stale
     (check 'cursor-span-inside-replacement-is-stale
            (text:rebase-span (span 2 1 2 1) d) #f)

     ;; -- reversible deltas and commuting compensation ----------------------

     (define (delta-data d)
       (cons (text:delta-new-end d) (text:delta->datum d)))
     (let* ([input (list (list 0 1 1 2) (list (string-copy "b") "cd") (list (string-copy "λ") "" "z"))]
            [decoded (text:datum->delta input)] [output (text:delta->datum decoded)])
       (set-car! (car input) 99)
       (string-set! (caaddr input) 0 #\X)
       (string-set! (caadr output) 0 #\Y)
       (set-car! (car output) 99)
       (check 'wire-delta-owns-both-directions-and-derives-its-end
         (list (text:delta->datum decoded) (text:delta-new-end decoded))
         '(((0 1 1 2) ("b" "cd") ("λ" "" "z")) (2 . 1))))
     (check 'wire-geometry-refuses-malformed-data
       (list
         (map (lambda (d) (test:raises? (lambda () (text:datum->span d))))
              '((0 0 0) (0 0 -1 0) (0 0 0 1.0) (0 0 0 x)))
         (map (lambda (d) (test:raises? (lambda () (text:datum->delta d))))
              '(((0 0 0 1) () ("x")) ((0 0 0 1) ("x") "y")
                ((0 0 0 1) ("xx") ("y")) ((0 0 1 1) ("x") ("y"))
                ((0 0 0 1) ("x") ("embedded\nnewline")) ((0 0 0 2) ("\nx") ("y")))))
       '((#t #t #t #t) (#t #t #t #t #t #t)))
     (check 'double-inversion-recovers-delta
            (delta-data (text:invert-delta (text:invert-delta d)))
            (delta-data d))

     (define (positions text)
       (let rows ([r 0] [out '()])
         (if (= r (vector-length text)) (reverse out)
             (let cols ([c 0] [out out])
               (if (> c (string-length (vector-ref text r)))
                   (rows (+ r 1) out)
                   (cols (+ c 1) (cons (cons r c) out)))))))
     (define (spans text)
       (let ([ps (positions text)])
         (apply append
           (map (lambda (start)
                  (map (lambda (end)
                         (span (car start) (cdr start) (car end) (cdr end)))
                       (filter (lambda (end) (text:position<=? start end)) ps)))
                ps))))
     (define replacements '(("") ("X") ("" "") ("Y" "Z")))
     (define commuting-base '#("abcd" "ef"))
     (define commuting-cases 0)
     (define (apply-delta text d)
       (unless (equal? (text:extract text (text:delta-span d))
                       (text:delta-removed d))
         (error 'text-test "rebased delta changed its removed content" (delta-data d)))
       (let-values ([(out applied)
                     (text:apply-edit text (text:delta-span d) (text:delta-inserted d))])
         (unless (equal? (delta-data applied) (delta-data d))
           (error 'text-test "rebased delta geometry disagrees"))
         out))
     (check 'compensation-commutes-through-disjoint-edits
            (for-all
              (lambda (first-span)
                (for-all
                  (lambda (first-replacement)
                    (let-values ([(after-first first)
                                  (text:apply-edit commuting-base first-span first-replacement)])
                      (let ([inverse (text:invert-delta first)])
                        (for-all
                          (lambda (second-span)
                            (for-all
                              (lambda (second-replacement)
                                (let*-values ([(after-second second)
                                               (text:apply-edit after-first second-span second-replacement)]
                                              [(after-inverse) (text:rebase-delta inverse second)])
                                  (or (not after-inverse)
                                      (let ([before-second (text:rebase-delta second inverse 'stay)])
                                        (set! commuting-cases (+ commuting-cases 1))
                                        (or (and before-second
                                                 (equal? (apply-delta after-second after-inverse)
                                                         (apply-delta commuting-base before-second)))
                                            (error 'text-test "compensation did not commute"
                                                   (delta-data first) (delta-data second)
                                                   (delta-data after-inverse)
                                                   (and before-second (delta-data before-second))))))))
                              replacements))
                          (spans after-first)))))
                  replacements))
              (spans commuting-base))
            #t)
     (check 'compensation-exercises-many-boundaries (> commuting-cases 1000) #t)

     ;; Project positions chosen in a proposed result through an accepted
     ;; rebase: inserted content keeps its offset, surrounding content follows
     ;; the actual edits.  Test both replacement boundaries and later rows.
     (let*-values ([(proposed intended)
                    (text:apply-edit '#("abcdef" "ghij") (span 0 2 0 4) '("UV" "W"))]
                   [(intervening foreign)
                    (text:apply-edit '#("abcdef" "ghij") (span 0 0 0 0) '("before" ""))]
                   [(actual) (text:rebase-delta intended foreign)])
       (check 'result-points-retain-replacement-offsets
              (map (lambda (p) (text:rebase-result-position p intended actual (list foreign)))
                   '((0 . 2) (0 . 3) (1 . 0) (1 . 1)))
              '((1 . 2) (1 . 3) (2 . 0) (2 . 1)))
       (check 'result-points-follow-surrounding-content
              (map (lambda (p) (text:rebase-result-position p intended actual (list foreign)))
                   '((0 . 1) (1 . 2) (2 . 2)))
              '((1 . 1) (2 . 2) (3 . 2))))
     (let*-values ([(proposed intended) (text:apply-edit '#("abcdef") (span 0 2 0 4) '("X"))]
                   [(intervening foreign) (text:apply-edit '#("abcdef") (span 0 4 0 4) '("Y"))]
                   [(actual) (text:rebase-delta intended foreign)])
       (check 'result-end-excludes-prior-insertion-at-right-boundary
              (text:rebase-result-position '(0 . 3) intended actual (list foreign)) '(0 . 3)))

     ;; -- validation ---------------------------------------------------------

     (check 'positions-outside-rejected
            (map (lambda (thunk) (guard (ex [else 'rejected]) (thunk)))
                 (list (lambda () (text:extract base (span 0 0 4 0)))
                       (lambda () (text:extract base (span 0 0 0 99)))
                       (lambda () (text:apply-edit base (span 0 0 0 1)
                                                   '()))))
            '(rejected rejected rejected))

     ;; -- cost ---------------------------------------------------------------

     ;; A multi-line paste walks its lines once. The quadratic version took
     ;; 770 ms for 40,000 lines; the bound leaves room for a loaded machine.
     (define (elapsed-ms thunk)
       (let ([start (current-time 'time-monotonic)])
         (thunk)
         (let ([end (current-time 'time-monotonic)])
           (+ (* 1000 (- (time-second end) (time-second start)))
              (div (- (time-nanosecond end) (time-nanosecond start)) 1000000)))))
     (define big-paste (map (lambda (i) (string-append "line " (number->string i))) (iota 40000)))
     (check 'multi-line-insertion-is-linear
            (let ([ms (elapsed-ms
                        (lambda ()
                          (let-values ([(result delta) (text:apply-edit base (span 1 3 1 3) big-paste)])
                            (unless (and (= (vector-length result) (+ 4 39999))
                                         (equal? (vector-ref result 1) "chaline 0")
                                         (equal? (vector-ref result 20000) "line 19999")
                                         (equal? (vector-ref result 40000) "line 39999rlie"))
                              (error 'text-test "wrong paste result")))))])
              (< ms 200))
            #t)

     (test:finish! 'text)))
