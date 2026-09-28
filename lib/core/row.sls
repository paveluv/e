;; Backend-independent row references and column contracts.
(import (only (foundation edoc) elibrary))
(elibrary (core row)
  (export columns? init! less? selection? source? valid?)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core port) port:) (prefix (foundation edoc) edoc:))

  (edoc "Whether a value is a tagged row-source reference; the collection service checks liveness."
        (value any "candidate") (returns boolean))
  (define (source? value)
    (and (list? value) (= (length value) 2) (eq? (car value) 'model)
      (integer? (cadr value)) (exact? (cadr value)) (> (cadr value) 0)))
  (edoc-type row-source "A tagged collection model reference." (predicate source?) (portable #t) (within list))

  (edoc "Whether a value identifies a row at a particular result generation."
        (value any "candidate") (returns boolean))
  (define (selection? value)
    (and (list? value) (= (length value) 3) (source? (car value))
      (integer? (cadr value)) (exact? (cadr value)) (>= (cadr value) 0)))
  (edoc-type row-selection "A (collection generation stable-key) selection." (predicate selection?) (portable #t) (within list))

  (edoc "Install the shared collection ports after the portable row types have initialized.")
  (define (init!)
    (let ([ports '((input source row-source (value source)) (input filter string (value filter))
                   (output rows row-source (id)) (output count integer (value count)))])
      (unless (equal? ports (port:describe '(model collection 1)))
        (parameterize ([kernel:registering-module 'row]) (port:register! '(model collection 1) ports)))))

  (edoc "Validate unique portable (column-id label type) declarations."
        (columns any "candidate columns") (returns boolean))
  (define (columns? columns)
    (and (list? columns) (pair? columns)
      (let loop ([rest columns] [seen '()])
        (or (null? rest)
          (let ([c (car rest)])
            (and (list? c) (= (length c) 3) (symbol? (car c)) (string? (cadr c))
              (edoc:type-portable? (caddr c)) (not (memq (car c) seen))
              (loop (cdr rest) (cons (car c) seen))))))))

  (edoc "Validate a portable (key cells attributes) row; missing cells are absent entries, distinct from a present false value."
        (columns list "column declarations") (value any "row") (returns boolean))
  (define (valid? columns value)
    (and (list? value) (= (length value) 3) (list? (cadr value)) (list? (caddr value))
      (let loop ([cells (cadr value)] [seen '()])
        (or (null? cells)
          (let* ([cell (car cells)] [c (and (pair? cell) (assq (car cell) columns))])
            (and c (not (memq (car cell) seen)) (edoc:type-accepts? (caddr c) (cdr cell))
              (loop (cdr cells) (cons (car cell) seen))))))))

  (edoc "Compare present typed scalar cells; missing cells are supplied as #f, present cells as (column . value)."
        (a any "left cell") (b any "right cell") (returns boolean))
  (define (less? a b)
    (cond [(not a) (and b #t)] [(not b) #f]
      [else (let ([a (cdr a)] [b (cdr b)])
              (cond [(boolean? a) (and (not a) b)]
                [(and (real? a) (real? b)) (< a b)]
                [(or (number? a) (number? b)) (error 'less? "non-real cells have no numerical sort order")]
                [(string? a) (string-ci<? a b)] [else #f]))])))
