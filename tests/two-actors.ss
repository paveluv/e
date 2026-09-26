#!/usr/bin/env scheme-script

;; Two actors on one file, one editing in e and one on disk, an editor or a
;; formatter of their own. The reload merges their work: what they changed
;; apart combines, what they changed together pends as a conflict whose
;; sides are the two images of one region, and settling either side gives
;; a clean text. Store level, headless.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (state store) store:) (prefix (foundation text) text:) (prefix (test) test:))

     (define check test:check)
     (define alice '(human alice))
     (define (joined lines) (apply string-append (map (lambda (l) (string-append l "\n")) lines)))
     (define (text-of id)
       (let count ([i (store:line-count id)] [acc '()])
         (if (= i 0) acc (count (- i 1) (cons (store:line id (- i 1)) acc)))))
     (define (visit base)
       ;; the file as both actors start from it
       (let ([id (store:create! alice "shared.ss" base)])
         (store:set-properties! alice id (list (cons 'base (joined base)) (cons 'trailing #t)))
         id))
     (define batch 0)
     (define (edit! id . edits)
       ;; one batch of edits in e, its own undo group, each ((row . col) (row . col) lines) against the current text
       (set! batch (+ batch 1))
       (for-each (lambda (e)
                   (store:edit! alice id (store:revision id)
                     (text:make-span (car (car e)) (cdr (car e)) (car (cadr e)) (cdr (cadr e))) (caddr e)
                     (list (list 'typing batch) "typing" (cons 'labels (list (cons 'batch (list alice batch)))))))
                 edits))
     (define (merged base edits disk)
       ;; the reload of one batch of edits against the disk
       (let ([id (visit base)]) (apply edit! id edits) (reload! id disk)))
     (define (clean text) (list text '()))
     (define (undo! id) (call-with-values (lambda () (store:undo! alice id)) list))
     (define (redo! id) (call-with-values (lambda () (store:redo! alice id)) list))
     (define (reload! id disk)
       ;; the disk as the other actor left it: the merged text and the conflicts as (region mine disk)
       (let-values ([(status detail) (store:reload! alice id disk (list (cons 'base (joined disk)) (cons 'trailing #t)))])
         (list (text-of id) (map (lambda (c) (list (cadddr c) (list-ref c 4) (list-ref c 5))) (cadr detail)))))
     (define (keep! id side)
       ;; every pending conflict settled one way: the text
       (for-each (lambda (c) (call-with-values (lambda () (store:resolve! alice id (car c) side)) list)) (store:conflicts id))
       (text-of id))

     (define program '("(define (f x)" "  (+ x 1))" "" "(define (g y)" "  (* y 2))"))

     ;; changes apart combine, whether on different lines or on one line
     (let ([id (visit program)])
       (edit! id '((1 . 7) (1 . 8) ("10")))
       (check 'edits-to-different-functions-combine
         (reload! id '("(define (f x)" "  (+ x 1))" "" "(define (g y)" "  (* y 3))"))
         '(("(define (f x)" "  (+ x 10))" "" "(define (g y)" "  (* y 3))") ())))
     (let ([id (visit program)])
       (edit! id '((0 . 9) (0 . 10) ("fn")))
       (check 'edits-apart-on-one-line-combine
         (reload! id '("(define (f x y)" "  (+ x 1))" "" "(define (g y)" "  (* y 2))"))
         '(("(define (fn x y)" "  (+ x 1))" "" "(define (g y)" "  (* y 2))") ())))

     ;; the disk deletes a line e edited: the conflict's side in e is the whole
     ;; line with its break, so keeping it puts the line back where it was
     (let ([id (visit '("alpha" "beta" "gamma"))])
       (edit! id '((1 . 0) (1 . 4) ("BETA")))
       (check 'a-deleted-line-edited-here-conflicts-with-the-whole-line
         (reload! id '("alpha" "gamma")) '(("alpha" "gamma") (((0 5 0 5) ("" "BETA") ("")))))
       (check 'keeping-mine-restores-the-edited-line (keep! id 'mine) '("alpha" "BETA" "gamma")))

     ;; the disk rewrites a word e changed a letter of: a word changes whole,
     ;; so the sides are the two words and neither settlement loses letters
     (let ([id (visit '("alpha" "beta" "gamma"))])
       (edit! id '((1 . 0) (1 . 1) ("B")))
       (check 'a-rewritten-word-conflicts-as-a-word
         (reload! id '("alpha" "delta" "gamma")) '(("alpha" "delta" "gamma") (((1 0 1 5) ("Beta") ("delta")))))
       (check 'keeping-mine-keeps-the-rest-of-the-word (keep! id 'mine) '("alpha" "Beta" "gamma")))

     ;; e deleted a line the disk changed: keeping mine deletes the disk's version
     (let ([id (visit '("alpha" "beta" "gamma"))])
       (edit! id '((0 . 5) (1 . 4) ("")))
       (check 'a-line-deleted-here-and-changed-on-disk-conflicts
         (reload! id '("alpha" "BETA" "gamma")) '(("alpha" "BETA" "gamma") (((0 5 1 4) ("") ("" "BETA")))))
       (check 'keeping-mine-deletes-the-disks-version (keep! id 'mine) '("alpha" "gamma")))

     ;; e inserted inside a word the disk replaced: the region is the word
     (let ([id (visit '("abc defgh ij"))])
       (edit! id '((0 . 5) (0 . 5) ("XY")))
       (check 'an-insertion-inside-a-replaced-word-conflicts-over-the-word
         (reload! id '("abc DEFGH ij")) '(("abc DEFGH ij") (((0 4 0 9) ("dXYefgh") ("DEFGH")))))
       (check 'keeping-mine-restores-the-insertion (keep! id 'mine) '("abc dXYefgh ij")))

     ;; two conflicts, the later change above the earlier one and adding a
     ;; line: each region is where its text stands once both are disabled
     (let ([id (visit '("one" "two" "three" "four"))])
       (edit! id '((3 . 0) (3 . 4) ("FOUR")))
       (edit! id '((0 . 0) (0 . 3) ("ONE" "extra")))
       (check 'stacked-conflicts-each-keep-their-place
         (reload! id '("1" "two" "three" "4"))
         '(("1" "two" "three" "4") (((0 0 0 1) ("ONE" "extra") ("1")) ((3 0 3 1) ("FOUR") ("4")))))
       (check 'keeping-mine-restores-both (keep! id 'mine) '("ONE" "extra" "two" "three" "FOUR")))

     ;; both made the same change: nothing to review
     (let ([id (visit '("abcdefgh"))])
       (edit! id '((0 . 3) (0 . 4) ("D")))
       (check 'identical-changes-settle-themselves (reload! id '("abcDefgh")) '(("abcDefgh") ())))
     (let ([id (visit '("alpha" "beta" "gamma"))])
       (edit! id '((0 . 5) (1 . 4) ("")))
       (check 'both-deleting-a-line-settles-itself (reload! id '("alpha" "gamma")) '(("alpha" "gamma") ())))

     ;; the disk adds a line: typing at the end of the line above stays on
     ;; it, typing at the start of the line below follows it down, and lines
     ;; both actors add at one place come disk first, however e typed its own
     (define (with-line-added edits) (let ([id (visit '("alpha" "beta" "gamma"))]) (apply edit! id edits) (reload! id '("alpha" "theirs" "beta" "gamma"))))
     (check 'typing-at-a-lines-end-stays-there-when-the-disk-adds-a-line-below
       (with-line-added '(((0 . 5) (0 . 5) ("!")))) '(("alpha!" "theirs" "beta" "gamma") ()))
     (check 'typing-at-the-next-lines-start-follows-it-down
       (with-line-added '(((1 . 0) (1 . 0) ("!")))) '(("alpha" "theirs" "!beta" "gamma") ()))
     (check 'a-line-added-by-both-comes-disk-first-typed-from-the-line-above
       (with-line-added '(((0 . 5) (0 . 5) ("" "mine")))) '(("alpha" "theirs" "mine" "beta" "gamma") ()))
     (check 'a-line-added-by-both-comes-disk-first-typed-from-the-line-below
       (with-line-added '(((1 . 0) (1 . 0) ("mine" "")))) '(("alpha" "theirs" "mine" "beta" "gamma") ()))
     (let ([id (visit '("alpha" "beta"))])
       (edit! id '((1 . 4) (1 . 4) ("!")))
       (check 'typing-at-the-files-end-stays-on-its-line-when-the-disk-appends
         (reload! id '("alpha" "beta" "theirs")) '(("alpha" "beta!" "theirs") ())))
     (let ([id (visit '("alpha" "beta"))])
       (edit! id '((0 . 0) (0 . 0) ("!")))
       (check 'a-line-added-at-the-top-goes-before-typing-on-the-old-first-line
         (reload! id '("zero" "alpha" "beta")) '(("zero" "!alpha" "beta") ())))

     ;; the disk deletes a line: typing around it is kept, typing into it conflicts
     (define (with-line-deleted edits) (let ([id (visit '("alpha" "beta" "gamma"))]) (apply edit! id edits) (list id (reload! id '("alpha" "gamma")))))
     (check 'typing-at-the-end-of-the-line-above-a-deleted-line-is-kept
       (cadr (with-line-deleted '(((0 . 5) (0 . 5) ("!"))))) '(("alpha!" "gamma") ()))
     (check 'typing-at-the-start-of-the-line-below-a-deleted-line-is-kept
       (cadr (with-line-deleted '(((2 . 0) (2 . 0) ("!"))))) '(("alpha" "!gamma") ()))
     (let ([outcome (with-line-deleted '(((1 . 2) (1 . 2) ("!"))))])
       (check 'typing-into-a-deleted-line-conflicts-with-the-line
         (cadr outcome) '(("alpha" "gamma") (((0 5 0 5) ("" "be!ta") ("")))))
       (check 'keeping-mine-restores-the-typed-line (keep! (car outcome) 'mine) '("alpha" "be!ta" "gamma")))

     ;; the other actor runs a formatter: lines reindented, one split, two
     ;; joined, trailing blanks stripped; e's edits on the rest of those
     ;; lines combine, a line e added keeping its own indentation
     (let ([id (visit '("(define (f x)" "(let ([y 1])" "(+ x y)))"))])
       (edit! id '((1 . 7) (1 . 8) ("q")))
       (check 'a-reindented-block-keeps-an-edit-on-its-first-line
         (reload! id '("(define (f x)" "  (let ([q 1])" "    (+ x q)))"))
         '(("(define (f x)" "  (let ([q 1])" "    (+ x q)))") ())))
     (let ([id (visit '("a" "b" "c" "d"))])
       (edit! id '((2 . 0) (2 . 1) ("q")))
       (check 'every-line-reindented-keeps-an-edit (reload! id '("  a" "  b" "  c" "  d")) '(("  a" "  b" "  q" "  d") ())))
     (let ([id (visit '("(define (f x) (+ x 1))"))])
       (edit! id '((0 . 19) (0 . 20) ("2")))
       (check 'a-line-split-by-the-formatter-keeps-an-edit-on-its-tail
         (reload! id '("(define (f x)" "  (+ x 1))")) '(("(define (f x)" "  (+ x 2))") ())))
     (let ([id (visit '("(define (f x)" "  (+ x 1))"))])
       (edit! id '((1 . 7) (1 . 8) ("2")))
       (check 'lines-joined-by-the-formatter-keep-an-edit-on-the-second
         (reload! id '("(define (f x) (+ x 1))")) '(("(define (f x) (+ x 2))") ())))
     (let ([id (visit '("(foo)  " "(bar)   "))])
       (edit! id '((0 . 4) (0 . 4) ("d")))
       (check 'trailing-blanks-stripped-on-disk-combine-with-an-edit (reload! id '("(foo)" "(bar)")) '(("(food)" "(bar)") ())))
     (let ([id (visit '("(let ()" "(foo)" "(bar))"))])
       (edit! id '((1 . 5) (1 . 5) ("" "(mine)")))
       (check 'a-line-added-in-a-reindented-block-keeps-its-own-indentation
         (reload! id '("(let ()" "  (foo)" "  (bar))")) '(("(let ()" "  (foo)" "(mine)" "  (bar))") ())))

     ;; the other actor renames across lines or wraps a region in a form;
     ;; e's edits inside combine
     (let ([id (visit '("(define (f x)" "  (+ x 1))"))])
       (edit! id '((1 . 7) (1 . 8) ("10")))
       (check 'a-rename-across-lines-combines-with-an-edit-beside-it
         (reload! id '("(define (f count)" "  (+ count 1))")) '(("(define (f count)" "  (+ count 10))") ())))
     (let ([id (visit '("(foo)" "(bar)"))])
       (edit! id '((0 . 4) (0 . 4) ("d")))
       (check 'wrapping-a-region-in-a-form-keeps-an-edit-inside
         (reload! id '("(when x" "  (foo)" "  (bar))")) '(("(when x" "  (food)" "  (bar))") ())))

     ;; prose: a paragraph is one line, and words changed apart on it combine
     (let ([id (visit '("The quick brown fox jumps over the lazy dog."))])
       (edit! id '((0 . 4) (0 . 9) ("slow")))
       (check 'words-changed-apart-on-one-paragraph-line-combine
         (reload! id '("The quick red fox leaps over the lazy dog.")) '(("The slow red fox leaps over the lazy dog.") ())))

     ;; a body rewritten on disk with an edit inside: one conflict between the
     ;; tokens both versions keep, its side in e the region as e had it
     (let ([id (visit '("(define (f x)" "  (let ([y (* x 2)])" "    (+ y 1)))"))])
       (edit! id '((1 . 16) (1 . 17) ("3")))
       (check 'a-rewritten-body-conflicts-as-one-with-the-edit-inside
         (reload! id '("(define (f x)" "  (if (zero? x) 0 (g x)))"))
         '(("(define (f x)" "  (if (zero? x) 0 (g x)))")
           (((1 3 1 22) ("let ([y (* x 3)])" "    (+ y 1") ("if (zero? x) 0 (g x")))))
       (check 'keeping-mine-restores-the-body-as-e-had-it (keep! id 'mine) '("(define (f x)" "  (let ([y (* x 3)])" "    (+ y 1)))")))

     ;; moves are not detected: a paragraph moved on disk is a change of the
     ;; lines that differ, the lines equal in both places read as kept, and an
     ;; edit on one of those stays at its line number, in the other paragraph
     (let ([id (visit '("first" "para" "" "second" "para"))])
       (edit! id '((1 . 0) (1 . 4) ("PARA")))
       (check 'a-paragraph-moved-on-disk-is-not-followed-by-an-edit-inside-it
         (reload! id '("second" "para" "" "first" "para")) '(("second" "PARA" "" "first" "para") ())))

     ;; adjacent changes pass each other: neighbouring tokens, a word extended
     ;; before a changed paren, the last character before deleted lines
     (check 'changes-to-adjacent-tokens-combine (merged '("foo(x)") '(((0 . 0) (0 . 3) ("FOO"))) '("foo[x]")) (clean '("FOO[x]")))
     (check 'a-word-extended-before-a-changed-paren-combines (merged '("foo(x)") '(((0 . 3) (0 . 3) ("d"))) '("foo[x]")) (clean '("food[x]")))
     (check 'the-last-character-changed-before-deleted-lines-combines
       (merged '("alpha" "beta" "gamma" "delta") '(((0 . 4) (0 . 5) ("A"))) '("alpha" "delta")) (clean '("alphA" "delta")))
     (check 'a-block-deleted-here-and-an-edit-below-it-on-disk-combine (merged '("a" "b" "c" "d") '(((0 . 1) (2 . 1) (""))) '("a" "b" "c" "D")) (clean '("a" "D")))

     ;; texts that would fuse into one word conflict instead, wherever they meet
     (check 'typing-before-a-word-the-disk-replaced-conflicts-rather-than-fusing
       (merged '("foo bar") '(((0 . 0) (0 . 0) ("x"))) '("baz bar")) '(("baz bar") (((0 0 0 3) ("xfoo") ("baz")))))
     (check 'a-word-extended-here-and-replaced-on-disk-conflicts
       (merged '("foo bar") '(((0 . 3) (0 . 3) ("d"))) '("baz bar")) '(("baz bar") (((0 0 0 3) ("food") ("baz")))))
     (check 'a-digit-appended-to-a-number-the-disk-changed-conflicts
       (merged '("(+ x 1)") '(((0 . 6) (0 . 6) ("0"))) '("(+ x 2)")) '(("(+ x 2)") (((0 5 0 6) ("10") ("2")))))
     (check 'a-new-word-typed-after-a-replaced-word-combines (merged '("foo bar") '(((0 . 3) (0 . 3) (" more"))) '("baz bar")) (clean '("baz more bar")))
     (check 'an-empty-file-filled-by-both-conflicts (merged '("") '(((0 . 0) (0 . 0) ("hello"))) '("world")) '(("world") (((0 0 0 5) ("hello") ("world")))))
     (check 'words-typed-by-both-into-one-blank-run-conflict
       (merged '("a  b") '(((0 . 2) (0 . 2) ("mine"))) '("a theirs b")) '(("a theirs b") (((0 1 0 9) (" mine ") (" theirs ")))))

     ;; appending to text the disk deleted conflicts; typing after its blanks does not
     (check 'typing-at-the-end-of-a-deleted-line-conflicts
       (merged '("alpha" "beta" "gamma") '(((1 . 4) (1 . 4) ("!"))) '("alpha" "gamma")) '(("alpha" "gamma") (((0 5 0 5) ("" "beta!") ("")))))
     (check 'a-word-extended-here-and-deleted-on-disk-conflicts (merged '("foo bar") '(((0 . 3) (0 . 3) ("d"))) '("bar")) '(("bar") (((0 0 0 0) ("food ") ("")))))
     (check 'typing-after-a-deleted-words-blank-combines (merged '("foo bar") '(((0 . 4) (0 . 4) ("X"))) '("bar")) (clean '("Xbar")))

     ;; the same work done by both stands once
     (check 'the-same-line-added-by-both-stands-once
       (merged '("alpha" "beta") '(((0 . 5) (0 . 5) ("" "(import x)"))) '("alpha" "(import x)" "beta")) (clean '("alpha" "(import x)" "beta")))
     (check 'the-same-text-appended-by-both-stands-once (merged '("alpha" "beta") '(((0 . 5) (0 . 5) ("!"))) '("alpha!" "beta")) (clean '("alpha!" "beta")))
     (check 'a-line-split-by-both-the-same-way-stands-once
       (merged '("(define (f x) (+ x 1))") '(((0 . 13) (0 . 14) ("" "  "))) '("(define (f x)" "  (+ x 1))")) (clean '("(define (f x)" "  (+ x 1))")))
     (check 'a-line-indented-by-both-the-same-way-stands-once (merged '("(let ()" "(foo))") '(((1 . 0) (1 . 0) ("  "))) '("(let ()" "  (foo))")) (clean '("(let ()" "  (foo))")))

     ;; a line opened where the disk joins two lines conflicts, and keeping
     ;; mine puts the three lines back; opened above a deleted line or where
     ;; the disk reindents the next, it is kept
     (let ([id (visit '("(define (f x)" "  (+ x 1))"))])
       (edit! id '((0 . 13) (0 . 13) ("" "  ;; doc")))
       (check 'a-line-opened-where-the-disk-joins-two-lines-conflicts
         (reload! id '("(define (f x) (+ x 1))")) '(("(define (f x) (+ x 1))") (((0 13 0 14) ("" "  ;; doc" "  ") (" ")))))
       (check 'keeping-mine-restores-the-three-lines (keep! id 'mine) '("(define (f x)" "  ;; doc" "  (+ x 1))")))
     (check 'a-line-opened-above-a-deleted-line-is-kept (merged '("alpha" "beta" "gamma") '(((0 . 5) (0 . 5) ("" "mine"))) '("alpha" "gamma")) (clean '("alpha" "mine" "gamma")))
     (check 'a-line-opened-where-the-disk-reindents-the-next-is-kept
       (merged '("(define (f x)" "  (+ x 1))") '(((0 . 13) (0 . 13) ("" "  ;; doc"))) '("(define (f x)" "    (+ x 1))")) (clean '("(define (f x)" "  ;; doc" "    (+ x 1))")))

     ;; the formatter's indentation against typing at a line's content
     (check 'typing-at-a-lines-content-start-survives-its-reindentation (merged '("(let ()" "  (foo))") '(((1 . 2) (1 . 2) ("x"))) '("(let ()" "    (foo))")) (clean '("(let ()" "    x(foo))")))
     (check 'typing-at-a-lines-content-start-survives-its-dedent (merged '("(let ()" "    (foo))") '(((1 . 4) (1 . 4) ("x"))) '("(let ()" "  (foo))")) (clean '("(let ()" "  x(foo))")))
     (check 'a-line-reindented-here-and-changed-on-disk-combines (merged '("(let ()" "  (foo))") '(((1 . 0) (1 . 2) ("    "))) '("(let ()" "  (bar))")) (clean '("(let ()" "    (bar))")))
     (check 'a-line-deleted-here-and-reindented-on-disk-conflicts
       (merged '("(let ()" "(a)" "(b))") '(((0 . 7) (1 . 3) (""))) '("(let ()" "  (a)" "  (b))")) '(("(let ()" "  (a)" "  (b))") (((0 7 1 5) ("") ("" "  (a)")))))

     ;; the file's end, comments, joins and splits from e's side, a reflow and a swap
     (check 'a-trailing-blank-line-removed-on-disk-and-typing-at-the-last-lines-end-combine (merged '("alpha" "") '(((0 . 5) (0 . 5) ("!"))) '("alpha")) (clean '("alpha!")))
     (check 'a-comment-added-at-a-lines-end-and-its-content-replaced-here-combine (merged '("(foo)") '(((0 . 0) (0 . 5) ("(bar)"))) '("(foo) ; note")) (clean '("(bar) ; note")))
     (check 'a-line-commented-out-on-disk-keeps-an-edit-inside (merged '("(foo bar)") '(((0 . 5) (0 . 8) ("baz"))) '(";; (foo bar)")) (clean '(";; (foo baz)")))
     (check 'edits-on-both-lines-the-disk-joined-combine
       (merged '("(define (f x)" "  (+ x 1))") '(((0 . 9) (0 . 10) ("g")) ((1 . 7) (1 . 8) ("2"))) '("(define (f x) (+ x 1))")) (clean '("(define (g x) (+ x 2))")))
     (check 'lines-joined-here-keep-an-edit-on-disk-to-the-second (merged '("(define (f x)" "  (+ x 1))") '(((0 . 13) (1 . 2) (" "))) '("(define (f x)" "  (+ x 2))")) (clean '("(define (f x) (+ x 2))")))
     (check 'a-line-split-here-keeps-an-edit-on-disk-after-the-split (merged '("(define (f x) (+ x 1))") '(((0 . 13) (0 . 14) ("" "  "))) '("(define (f x) (+ x 2))")) (clean '("(define (f x)" "  (+ x 2))")))
     (check 'a-paragraph-reflowed-on-disk-keeps-a-typo-fixed-here
       (merged '("The quick brown fox jumps over" "the lazy dog and runs" "away fast.") '(((1 . 4) (1 . 8) ("LAZY")))
               '("The quick brown fox jumps over the lazy" "dog and runs away fast."))
       (clean '("The quick brown fox jumps over the LAZY" "dog and runs away fast.")))
     (check 'functions-swapped-on-disk-are-followed-by-an-edit-inside-one
       (merged '("(define (f) 1)" "" "(define (g) 2)") '(((2 . 12) (2 . 13) ("3"))) '("(define (g) 2)" "" "(define (f) 1)")) (clean '("(define (g) 3)" "" "(define (f) 1)")))

     ;; undo history: an edit undone is no edit; one undone and redone is; an
     ;; undone latest edit leaves the earlier one to combine
     (let ([id (visit '("foo bar"))])
       (edit! id '((0 . 0) (0 . 3) ("FOO")))
       (undo! id)
       (check 'an-edit-undone-here-yields-to-the-disks-change (reload! id '("baz bar")) (clean '("baz bar"))))
     (let ([id (visit '("foo bar"))])
       (edit! id '((0 . 0) (0 . 3) ("FOO")))
       (undo! id)
       (redo! id)
       (check 'an-edit-undone-and-redone-conflicts-with-the-disks-change (reload! id '("baz bar")) '(("baz bar") (((0 0 0 3) ("FOO") ("baz"))))))
     (let ([id (visit '("foo bar"))])
       (edit! id '((0 . 4) (0 . 7) ("BAR")))
       (edit! id '((0 . 0) (0 . 3) ("FOO")))
       (undo! id)
       (check 'undoing-the-latest-edit-leaves-the-earlier-one-to-combine (reload! id '("baz bar")) (clean '("baz BAR"))))

     ;; Compare final changes, even when typed in separate actions. The
     ;; same replacement must not replay a later insertion a second time;
     ;; shared and independent changes may coexist on each side.
     (for-each
       (lambda (case)
         (let ([id (visit (car case))])
           (for-each (lambda (e) (edit! id e)) (cadr case))
           (check 'shared-multi-step-changes-are-applied-once
             (reload! id (caddr case)) (clean (cadddr case)))))
       '((("gg hh ii") (((0 . 3) (0 . 5) ("X")) ((0 . 4) (0 . 5) (" a")))
          ("gg X aii") ("gg X aii"))
         (("abc" "unchanged" "local") (((0 . 0) (0 . 3) ("X")) ((0 . 1) (0 . 1) ("!")) ((2 . 0) (2 . 5) ("LOCAL")))
          ("X!" "DISK" "local") ("X!" "DISK" "LOCAL"))
         (("aa bb cc" "dd ee ff" "gg hh ii")
          (((0 . 6) (1 . 0) (" a")) ((0 . 16) (1 . 3) (" a")) ((0 . 3) (0 . 21) ("")) ((0 . 0) (0 . 3) ("")))
          ("ii") ("ii"))))
     (let ([id (visit '("alpha beta gamma"))])
       (edit! id '((0 . 11) (0 . 16) ("G")))
       (edit! id '((0 . 0) (0 . 5) ("A")))
       (check 'overlapping-conflicts-form-one-complete-alternative
         (list (reload! id '("disk")) (keep! id 'mine))
         '((("disk") (((0 0 0 4) ("A beta G") ("disk")))) ("A beta G"))))
     ;; Shared boundary text must not escape to the end of disk's new
     ;; line, including blanks that the net diff aligns with old blanks.
     (for-each
       (lambda (scenario)
         (let ([id (visit (car scenario))])
           (for-each (lambda (e) (edit! id e)) (cadr scenario))
           (check 'shared-boundaries-are-not-replayed-after-new-disk-lines
             (list (car (reload! id (caddr scenario))) (keep! id 'disk))
             (list (caddr scenario) (caddr scenario)))))
       '((("alpha" "beta") (((0 . 0) (1 . 0) ("mine")) ((0 . 8) (0 . 8) ("!")))
          ("minebeta!" "FOREIGN"))
         (("alpha beta") (((0 . 0) (0 . 6) ("mine")) ((0 . 8) (0 . 8) (" ")))
          ("minebeta " "FOREIGN"))
         (("alpha beta") (((0 . 0) (0 . 6) ("new" "line")) ((0 . 0) (0 . 0) (" ")))
          (" new" "linebeta" "FOREIGN"))))
     (let ([id (visit '("a b"))])
       (edit! id '((0 . 0) (0 . 3) ("X Y")))
       (check 'a-partly-shared-replacement-preserves-its-unshared-mine-side
         (list (reload! id '("X b")) (keep! id 'mine))
         '((("X b") (((0 0 0 3) ("X Y") ("X b")))) ("X Y"))))
     (let ([id (visit '("alpha" "beta" "gamma"))])
       (for-each (lambda (e) (edit! id e))
         '(((0 . 0) (2 . 5) ("mine")) ((0 . 0) (0 . 0) ("aa"))
           ((0 . 2) (0 . 6) ("aa")) ((0 . 1) (0 . 3) ("")) ((0 . 0) (0 . 1) (""))))
       (check 'overlapping-typing-keeps-its-complete-mine-alternative
         (list (car (reload! id '("disk"))) (keep! id 'mine)) '(("disk") ("a"))))

     ;; Reduced histories with cancelled and rewritten entries: identical
     ;; final sides coalesce regardless of the route taken to reach them.
     (for-each
       (lambda (scenario)
         (let ([id (visit (car scenario))])
           (for-each
             (lambda (op)
               (let ([actor (if (eq? (cadr op) 'a) alice '(human bob))])
                 (case (car op)
                   [(edit) (store:edit! actor id (store:revision id) (text:datum->span (caddr op)) (cadddr op))]
                   [(undo) (store:undo! actor id)]
                   [(rewrite) (store:rewrite! actor id (caddr op))])))
             (cadr scenario))
           (let ([mine (text-of id)])
             (check 'inverse-histories-reload-identical-text-without-conflicts (reload! id mine) (clean mine)))))
       '((("abc") ((edit a (0 0 0 3) ("X")) (edit b (0 0 0 0) ("" ""))
                   (undo a) (edit a (0 0 1 3) ("Y"))))
         (("abcd") ((edit a (0 1 0 3) ("")) (edit b (0 0 0 1) ("")) (undo a)))
         (("alpha beta" "gamma delta" "epsilon zeta")
          ((edit a (1 10 2 11) (" ")) (rewrite a (1)) (edit a (0 1 0 6) (""))
           (edit a (1 4 1 11) ("" "")) (edit b (1 1 1 4) ("aa"))
           (edit b (0 2 0 2) ("")) (edit a (0 5 2 0) ("X")) (rewrite a (6))))))

     (test:finish! 'two-actors)))
