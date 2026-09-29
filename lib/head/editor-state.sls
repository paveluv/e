;; Logical editor state shared by nested and ordinary hosts; no geometry.
(import (only (foundation edoc) elibrary))
(elibrary (head editor-state)
  (export annotations? create! points state)
  (import (chezscheme) (prefix (foundation text) text:)
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

  (edoc "Create an unmounted editor view over a shared document. Caret, anchor, logical top and mark activity belong to the view. Options include wrap (#t, #f or default) and annotations (revision-bound logical ranges); no window is created."
        (actor actor "creator") (document integer "store document identity") (options list "logical preferences") (returns model))
  (define (create! actor document options)
    (unless (and (integer? document) (exact? document) (> document 0)
              (list? options) (for-all (lambda (p) (and (pair? p) (case (car p)
                                                                    [(wrap) (memq (cdr p) '(default #t #f))] [(annotations) (annotations? (cdr p))] [else #f]))) options)
              (or (null? options) (and (<= (length options) 2) (not (assq (caar options) (cdr options))))))
      (error 'create! "invalid document or editor options"))
    (view:create! actor (list 'buffer document) 'editor 1
      (if (assq 'annotations options) options (cons '(annotations) options)) '((0 . 0) (0 . 0) (0 . 0) #f)))

  ;; An annotation batch is (document revision ((span-datum face) ...)).
  ;; It contains logical coordinates and semantic faces, never terminal cells.
  (edoc "Whether data is an empty annotation set or a document/revision-bound batch of logical spans and semantic faces."
        (value any "proposed annotation batch") (returns boolean))
  (define (annotations? value)
    (or (null? value)
      (and (list? value) (= (length value) 3)
        (integer? (car value)) (exact? (car value)) (> (car value) 0)
        (integer? (cadr value)) (exact? (cadr value)) (>= (cadr value) 0)
        (list? (caddr value))
        (for-all (lambda (p)
                   (and (list? p) (= (length p) 2) (symbol? (cadr p))
                     (guard (ex [else #f]) (text:datum->span (car p)) #t))) (caddr value)))))
)
