;; Logical editor state shared by nested and ordinary hosts; no geometry.
(import (only (foundation edoc) elibrary))
(elibrary (head editor-state)
  (export (rename (editor-schema:annotations? annotations?)) create! points state word-range)
  (import (chezscheme) (prefix (core editor-schema) editor-schema:)
          (prefix (head text-source) text-source:) (prefix (state view) view:))

  (define (position? p)
    (and (pair? p) (integer? (car p)) (exact? (car p)) (>= (car p) 0)
      (integer? (cdr p)) (exact? (cdr p)) (>= (cdr p) 0)))

  (edoc "Read and validate an editor descriptor's logical caret, anchor, top and mark activity."
        (d list "editor view descriptor") (returns list))
  (define (state d)
    (let ([s (view:state d)])
      (unless (and (list? s) (= (length s) 4) (for-all position? (list-head s 3)) (boolean? (cadddr s)))
        (error 'editor "expected (caret anchor top marked?)" s)) s))

  (edoc "Project an editor's three logical anchors through retained source changes; false means history is unavailable. No geometry, I/O or state publication occurs."
        (mirror any "text mirror") (revision integer "target text revision") (d list "editor descriptor") (returns any))
  (define (points mirror revision d)
    (text-source:rebase (list-head (state d) 3)
      (text-source:changes mirror (or (view:basis d) revision) revision)))

  (edoc "Create an unmounted editor view over a shared document. Options include wrap (#t, #f or default), read-only (a view preference) and annotations (revision-bound logical ranges); no window is created."
        (actor actor "creator") (document buffer "store document identity") (options list "logical preferences") (owner (list-of (or model #f)) "optional lifetime owner, false for a session root") (returns model))
  (define (create! actor document options . owner)
    (let ([d (editor-schema:make document options)])
      (apply view:create! actor document (view:kind d) (view:schema d) (view:options d) (view:state d) owner)))

  (edoc "The word at or immediately before a character column, as (first . end), or false on punctuation or whitespace."
        (line string "source line") (column integer "character column") (returns any))
  (define (word-range line column)
    (define (word? i)
      (and (<= 0 i) (< i (string-length line))
        (let ([c (string-ref line i)])
          (not (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\.)))))))
    (let ([at (cond [(word? column) column] [(word? (- column 1)) (- column 1)] [else #f])])
      (and at (cons (let back ([i at]) (if (word? (- i 1)) (back (- i 1)) i))
                    (let next ([i at]) (if (word? i) (next (+ i 1)) i))))))

)
