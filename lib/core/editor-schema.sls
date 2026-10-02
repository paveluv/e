;; Backend-independent construction of a logical text presentation.
(import (only (foundation edoc) elibrary))
(elibrary (core editor-schema)
  (export annotations? make)
  (import (chezscheme) (prefix (core descriptor) descriptor:)
          (prefix (core handle) handle:) (prefix (foundation text) text:))

  (edoc "Construct an unowned editor descriptor over borrowed text, with logical anchors and validated preferences."
        (document buffer "source identity") (options list "wrap, read-only and annotations") (returns list))
  (define (make document options)
    (unless (and (handle:buffer? document)
              (list? options) (for-all (lambda (p) (and (pair? p) (case (car p)
                                                                    [(wrap) (memq (cdr p) '(default #t #f))] [(read-only) (boolean? (cdr p))] [(annotations) (annotations? (cdr p))] [else #f]))) options)
              (let unique ([rest options]) (or (null? rest) (and (not (assq (caar rest) (cdr rest))) (unique (cdr rest))))))
      (error 'make "invalid document or editor options"))
    (descriptor:make document 'editor 1
      (if (assq 'annotations options) options (cons '(annotations) options)) '((0 . 0) (0 . 0) (0 . 0) #f)))

  (edoc "Whether data is an empty annotation set or a document/revision-bound batch of logical spans and semantic faces."
        (value any "proposed annotation batch") (returns boolean))
  (define (annotations? value)
    (or (null? value)
      (and (list? value) (= (length value) 3)
        (handle:buffer? (car value))
        (integer? (cadr value)) (exact? (cadr value)) (>= (cadr value) 0)
        (list? (caddr value))
        (for-all (lambda (p)
                   (and (list? p) (= (length p) 2) (symbol? (cadr p))
                     (guard (ex [else #f]) (text:datum->span (car p)) #t))) (caddr value)))))
)
