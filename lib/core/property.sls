;; property.sls -- one validation contract for shared and head-local facts.
(library (property)
  (export select validate-expected matches? edit-keys
          (rename (validate-properties validate)
                  (writable-properties writable) (validate-edit-context edit-context)))
  (import (only (edoc) edefine edoc) (rnrs) (prefix (identity) identity:))

  ;; Maintained by the text owner and carried with incremental edit replies.
  (edefine edit-keys
    (edoc "The facts the text owner maintains and carries with edit replies: modified and modified-at."
          (value (list-of symbol)))
    '(modified modified-at))

  (edefine (validate-properties updates)
    (edoc "Check a batch of (key . value) facts before either owner installs any of it; an error names the fault."
          (updates list "the facts"))
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

  (edefine (writable-properties updates)
    (edoc "Validate facts a writer may set: not the text owner's modification facts, nor the publication identity."
          (updates list "the facts")
          (returns list))
    (validate-properties updates)
    (when (exists (lambda (entry) (memq (car entry) edit-keys)) updates)
      (error 'store "modification facts are maintained by the text owner"))
    (when (assq 'publication updates)
      (error 'store "publication identity belongs to publish!"))
    updates)

  (edefine (select facts keys)
    (edoc "The expectations for keys among facts: the (key . value) present, or the bare key for an absent one."
          (facts list "the facts")
          (keys (list-of symbol) "the keys")
          (returns list))
    ;; A pair expects that exact value; a bare key expects absence. In
    ;; particular, absence and an explicit #f can have different defaults.
    (map (lambda (key) (or (assq key facts) key)) keys))

  (edefine (validate-expected expected)
    (edoc "Check a fact review: #f, or a list of distinct bare keys and (key . value) pairs."
          (expected any "the review"))
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

  (edefine (matches? expected facts)
    (edoc "Whether facts satisfy a review: each pair present exactly, each bare key absent."
          (expected any "the review, or #f")
          (facts list "the facts")
          (returns boolean))
    (or (not expected)
        (for-all (lambda (entry)
                   (if (pair? entry) (equal? entry (assq (car entry) facts))
                       (not (assq entry facts))))
                 expected)))

  (edefine (validate-edit-context context)
    (edoc "Check an edit context: a group label and optional undo, commit and expected facts, no key in two sets."
          (context any "the context, or #f"))
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
