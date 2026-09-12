;; property.sls -- one validation contract for shared and head-local facts.
(library (property)
  (export select validate-expected matches? edit-keys
          (rename (validate-properties validate)
                  (writable-properties writable) (validate-edit-context edit-context)))
  (import (rnrs) (prefix (identity) identity:))

  ;; Maintained by the text owner and carried with incremental edit replies.
  (define edit-keys '(modified modified-at))

  (define (validate-properties updates)
    ;; A pure boundary shared with head-local facts.  Validate the whole
    ;; batch before either owner can install any part of it.
    (unless
      (and (list? updates)
           (let valid ([rest updates] [seen '()])
             (or (null? rest)
                 (let ([entry (car rest)])
                   (and (pair? entry) (symbol? (car entry))
                        (not (memq (car entry) seen))
                        (case (car entry)
                          [(base) (or (not (cdr entry)) (string? (cdr entry)))]
                          [(trailing disposable alive manages-viewport selectable) (boolean? (cdr entry))]
                          [(modified-at) (or (not (cdr entry))
                                             (and (integer? (cdr entry)) (exact? (cdr entry))))]
                          [(app) (or (boolean? (cdr entry))
                                     (and (identity:valid? (cdr entry)) (eq? (cadr entry) 'app)))]
                          [(capture)
                           (or (not (cdr entry)) (eq? (cdr entry) 'all)
                               (and (list? (cdr entry))
                                    (for-all string? (if (and (pair? (cdr entry)) (eq? (cadr entry) 'except))
                                                         (cddr entry) (cdr entry)))))]
                          [(status) (or (not (cdr entry)) (string? (cdr entry)))]
                          [(sticky-lines) (and (integer? (cdr entry)) (exact? (cdr entry)) (>= (cdr entry) 0))]
                          [(cursor-style) (memq (cdr entry) '(#f default text block underline bar
                                                              blinking-block blinking-underline blinking-bar))]
                          [(audience) (identity:audience? (cdr entry))]
                          [else #t])
                        (valid (cdr rest) (cons (car entry) seen)))))))
      (error 'validate-properties "expected unique symbol keys and valid fact values" updates))
    updates)

  (define (writable-properties updates)
    (validate-properties updates)
    (when (exists (lambda (entry) (memq (car entry) edit-keys)) updates)
      (error 'store "modification facts are maintained by the text owner"))
    (when (assq 'publication updates)
      (error 'store "publication identity belongs to publish!"))
    updates)

  (define (select facts keys)
    ;; A pair expects that exact value; a bare key expects absence. In
    ;; particular, absence and an explicit #f can have different defaults.
    (map (lambda (key) (or (assq key facts) key)) keys))

  (define (validate-expected expected)
    (unless (or (not expected)
                (and (list? expected)
                     (let valid ([rest expected] [seen '()])
                       (or (null? rest)
                           (let ([key (if (pair? (car rest)) (caar rest) (car rest))])
                             (and (symbol? key) (not (memq key seen))
                                  (valid (cdr rest) (cons key seen))))))))
      (error 'validate-expected "expected unique fact pairs or absent symbol keys" expected))
    (when expected (validate-properties (filter pair? expected)))
    expected)

  (define (matches? expected facts)
    (or (not expected)
        (for-all (lambda (entry)
                   (if (pair? entry) (equal? entry (assq (car entry) facts))
                       (not (assq entry facts))))
                 expected)))

  (define (validate-edit-context context)
    ;; Undo facts travel with the inverse.  Commit facts describe external
    ;; state (e.g. a disk baseline) and survive undo, but commit atomically
    ;; with the text. A key cannot appear in both sets. Expected facts
    ;; guard the commit without becoming part of its undo history.
    (unless (or (not context)
                (and (list? context) (memv (length context) '(2 3 4 5))
                     (or (not (cadr context)) (string? (cadr context)))))
      (error 'validate-edit-context "expected (key label [undo-facts [commit-facts [expected]]])" context))
    (when (and context (>= (length context) 3))
      (writable-properties
        (append (validate-properties (caddr context))
                (if (>= (length context) 4) (validate-properties (cadddr context)) '()))))
    (when (and context (= (length context) 5))
      (validate-expected (list-ref context 4)))
    context)

)
