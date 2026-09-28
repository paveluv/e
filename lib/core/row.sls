;; Backend-independent row references and column contracts.
(import (only (foundation edoc) elibrary))
(elibrary (core row)
  (export attributes? columns? init! less? selectable? selection? source? valid?)
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
        (parameterize ([kernel:registering-module 'row]) (port:register! '(model collection 1) ports))))
    (for-each
      (lambda (kind)
        (let ([ports '((input rows row-source (source)) (output selection (or row-selection #f) (state selection)))])
          (unless (equal? ports (port:describe (list 'view kind 1)))
            (parameterize ([kernel:registering-module 'row]) (port:register! (list 'view kind 1) ports))))) '(table list)))

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
    (and (list? value) (= (length value) 3) (list? (cadr value)) (for-all pair? (cadr value)) (attributes? columns (caddr value))
      (let ([spans (assq 'matches (caddr value))])
        (or (not spans) (for-all (lambda (span)
                                   (let ([cell (assq (car span) (cadr value))])
                                     (and cell (string? (cdr cell)) (<= (caddr span) (string-length (cdr cell)))))) (cdr spans))))
      (let loop ([cells (cadr value)] [seen '()])
        (or (null? cells)
          (let* ([cell (car cells)] [c (and (pair? cell) (assq (car cell) columns))])
            (and c (not (memq (car cell) seen)) (edoc:type-accepts? (caddr c) (cdr cell))
              (loop (cdr cells) (cons (car cell) seen))))))))

  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))

  (edoc "Validate portable row facts: eligibility, hierarchy, roles, raw character matches, creation and pending metadata."
        (columns list "column declarations") (attributes list "distinct named facts") (returns boolean))
  (define (attributes? columns attributes)
    (and (list? attributes)
      (let loop ([rest attributes] [seen '()])
        (or (null? rest)
          (let* ([p (car rest)] [name (and (pair? p) (car p))] [value (and (pair? p) (cdr p))])
            (and name (not (memq name seen))
              (case name
                [(selectable) (boolean? value)]
                [(depth) (natural? value)]
                [(roles) (and (list? value) (for-all symbol? value))]
                [(creation) (memq value '(file directory))]
                [(pending) (and (list? value) (for-all (lambda (c) (assq c columns)) value))]
                [(matches)
                 (and (list? value)
                   (for-all (lambda (span)
                              (and (list? span) (= (length span) 3) (assq (car span) columns)
                                (natural? (cadr span)) (natural? (caddr span)) (< (cadr span) (caddr span)))) value))]
                [else #f])
              (loop (cdr rest) (cons name seen))))))))

  (edoc "Whether a raw row is eligible for selection; section rows explicitly opt out."
        (value list "raw (key cells attributes) row") (returns boolean))
  (define (selectable? value)
    (cond [(assq 'selectable (caddr value)) => cdr] [else #t]))

  (edoc "Compare present typed scalar cells; missing cells are supplied as #f, present cells as (column . value)."
        (a any "left cell") (b any "right cell") (returns boolean))
  (define (less? a b)
    (cond [(not a) (and b #t)] [(not b) #f]
      [else (let ([a (cdr a)] [b (cdr b)])
              (cond [(boolean? a) (and (not a) b)]
                [(and (real? a) (real? b)) (< a b)]
                [(or (number? a) (number? b)) (error 'less? "non-real cells have no numerical sort order")]
                [(string? a) (string-ci<? a b)] [else #f]))])))
