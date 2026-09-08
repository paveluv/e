;; property.e -- one validation contract for shared and head-local facts.
(library (property)
  (export (rename (validate-properties validate)
                  (writable-properties writable) (validate-edit-context edit-context)))
  (import (rnrs) (prefix (identity) identity:))

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
                          [(trailing disposable alive manages-viewport) (boolean? (cdr entry))]
                          [(app) (or (boolean? (cdr entry))
                                     (and (identity:valid? (cdr entry)) (eq? (cadr entry) 'app)))]
                          [(capture)
                           (or (not (cdr entry)) (eq? (cdr entry) 'all)
                               (and (list? (cdr entry))
                                    (for-all string? (if (and (pair? (cdr entry)) (eq? (cadr entry) 'except))
                                                         (cddr entry) (cdr entry)))))]
                          [(status) (or (not (cdr entry)) (string? (cdr entry)))]
                          [(sticky-lines) (and (integer? (cdr entry)) (exact? (cdr entry)) (>= (cdr entry) 0))]
                          [(cursor-style) (memq (cdr entry) '(#f default block underline bar
                                                              blinking-block blinking-underline blinking-bar))]
                          [(audience) (identity:audience? (cdr entry))]
                          [else #t])
                        (valid (cdr rest) (cons (car entry) seen)))))))
      (error 'validate-properties "expected unique symbol keys and valid fact values" updates))
    updates)

  (define (writable-properties updates)
    (validate-properties updates)
    (when (assq 'modified updates)
      (error 'store "modified is derived from text and its baseline"))
    (when (assq 'publication updates)
      (error 'store "publication identity belongs to publish!"))
    updates)

  (define (validate-edit-context context)
    ;; Undo facts travel with the inverse.  Commit facts describe external
    ;; state (e.g. a disk baseline) and survive undo, but commit atomically
    ;; with the text.  A key cannot appear in both sets.
    (unless (or (not context)
                (and (list? context) (memv (length context) '(2 3 4))
                     (or (not (cadr context)) (string? (cadr context)))))
      (error 'validate-edit-context "expected (key label [undo-facts [commit-facts]])" context))
    (when (and context (>= (length context) 3))
      (writable-properties
        (append (validate-properties (caddr context))
                (if (= (length context) 4) (validate-properties (cadddr context)) '()))))
    context)

)
