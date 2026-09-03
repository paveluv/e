#!/usr/bin/env scheme-script

;; The pure text algebra: spans, edits, deltas, inversion, and
;; rebasing -- the v2 foundation (docs/DESIGN2.md, stage 1).  Run
;; from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (text) text:) (only (chezscheme) format))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'text-test label actual expected)))

     (define (span sl sc el ec) (text:make-span sl sc el ec))
     (define (span->list s)
       (list (text:span-start s) (text:span-end s)))

     (define base '#("alpha bravo" "charlie" "delta echo" "foxtrot"))

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
              '((1 . 6) (1 . 6))))

     ;; an empty span strictly inside a replaced region is stale
     (check 'cursor-span-inside-replacement-is-stale
            (text:rebase-span (span 2 1 2 1) d) #f)

     ;; -- validation ---------------------------------------------------------

     (check 'positions-outside-rejected
            (map (lambda (thunk) (guard (ex [else 'rejected]) (thunk)))
                 (list (lambda () (text:extract base (span 0 0 4 0)))
                       (lambda () (text:extract base (span 0 0 0 99)))
                       (lambda () (text:apply-edit base (span 0 0 0 1)
                                                   '()))))
            '(rejected rejected rejected))

     (format #t "~a text checks passed\n" checks)))
