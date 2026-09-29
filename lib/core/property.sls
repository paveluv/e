;; property.sls -- one validation contract for shared and head-local facts.
(import (only (foundation edoc) elibrary))
(elibrary (core property)
  (export backup-value? context-commit context-expected context-labels context-revision context-undo (rename (validate-edit-context edit-context)) edit-keys flags flags<? matches? select
          (rename (validate-properties validate)) validate-expected
          (rename (writable-properties writable)))
  (import (rnrs) (prefix (core identity) identity:))

  (edoc-type buffer-flag "a buffer flag: conflicted or read-only"
    (predicate (lambda (v) (and (memq v '(conflicted read-only)) #t)))
    (complete (lambda (partial) '((conflicted . "unsettled reload conflicts") (read-only . "ordinary editing is guarded"))))
    (portable #t) (within symbol))

  (edoc "Active buffer flags in canonical order from a fact alist; conditional edit guards count as read-only."
        (facts list "buffer facts") (returns (list-of buffer-flag)))
  (define (flags facts)
    (append (if (cond [(assq 'conflicts facts) => (lambda (p) (> (cdr p) 0))] [else #f]) '(conflicted) '())
      (if (cond [(assq 'read-only facts) => cdr] [else #f]) '(read-only) '())))

  (edoc "Compare flag sets in marker lexicographic order: empty, conflicted, both, read-only."
        (a (list-of buffer-flag) "left") (b (list-of buffer-flag) "right") (returns boolean))
  (define (flags<? a b)
    (define (rank flags) (if (memq 'conflicted flags) (if (memq 'read-only flags) 2 1) (if (memq 'read-only flags) 3 0)))
    (< (rank a) (rank b)))

  ;; Maintained by the text owner and carried with incremental edit replies.
  (edoc "The facts the text owner maintains and carries with edit replies: modified and modified-at."
        (value (list-of symbol)))
  (define edit-keys '(modified modified-at conflicts))

  (edoc "Check a batch of (key . value) facts before either owner installs any of it; an error names the fault."
        (updates list "the facts"))
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
                          [(trashed) (let ([v (cdr entry)])
                                       (or (not v)
                                           (and (list? v) (= (length v) 2) (integer? (car v)) (exact? (car v))
                                                (identity:valid? (cadr v)))))]
                          [(backup) (backup-value? (cdr entry))]
                          [else #t])
                        (valid (cdr rest) (cons (car entry) seen)))))))
      (error 'validate-properties "expected unique symbol keys and valid fact values" updates))
    updates)

  (edoc "Whether a value fits a backup fact: (path stamp checksum), the file's canonical path, its modification time as (seconds . nanoseconds) or #f, and a checksum of its text; #f withdraws the fact."
        (v any "the value")
        (returns boolean))
  (define (backup-value? v)
    (or (not v)
        (and (list? v) (= (length v) 3)
             (string? (car v)) (> (string-length (car v)) 0)
             (let ([stamp (cadr v)])
               (or (not stamp)
                   (and (pair? stamp) (integer? (car stamp)) (exact? (car stamp))
                        (integer? (cdr stamp)) (exact? (cdr stamp)) (<= 0 (cdr stamp)) (< (cdr stamp) 1000000000))))
             (string? (caddr v)))))

  (edoc "Validate facts a writer may set: not the text owner's modification facts, nor the publication identity."
        (updates list "the facts")
        (returns list))
  (define (writable-properties updates)
    (validate-properties updates)
    (when (exists (lambda (entry) (memq (car entry) edit-keys)) updates)
      (error 'store "modification facts are maintained by the text owner"))
    (when (assq 'publication updates)
      (error 'store "publication identity belongs to publish!"))
    updates)

  (edoc "The expectations for keys among facts: the (key . value) present, or the bare key for an absent one."
        (facts list "the facts")
        (keys (list-of symbol) "the keys")
        (returns list))
  (define (select facts keys)
    ;; A pair expects that exact value; a bare key expects absence. In
    ;; particular, absence and an explicit #f can have different defaults.
    (map (lambda (key) (or (assq key facts) key)) keys))

  (edoc "Check a fact review: #f, or a list of distinct bare keys and (key . value) pairs."
        (expected any "the review"))
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

  (edoc "Whether facts satisfy a review: each pair present exactly, each bare key absent."
        (expected any "the review, or #f")
        (facts list "the facts")
        (returns boolean))
  (define (matches? expected facts)
    (or (not expected)
        (for-all (lambda (entry)
                   (if (pair? entry) (equal? entry (assq (car entry) facts))
                       (not (assq entry facts))))
                 expected)))

  (edoc "Check an edit context, (key label . options): a group key and label, then an alist among undo facts, commit facts, expected facts, labels and an optional exact revision guard; no fact key in two sets."
        (context any "the context, or #f"))
  (define (validate-edit-context context)
    ;; Undo facts travel with the inverse.  Commit facts describe external
    ;; state (e.g. a disk baseline) and survive undo, but commit atomically
    ;; with the text. A key cannot appear in both sets. Expected facts
    ;; guard the commit without becoming part of its undo history. Labels
    ;; ride on the log entry: (batch . id) names the edits made together.
    (unless (or (not context)
                (and (list? context) (>= (length context) 2)
                     (or (not (cadr context)) (string? (cadr context)))
                     (for-all (lambda (option)
                                (and (pair? option) (memq (car option) '(undo commit expected labels revision))))
                              (cddr context))
                     (let unique ([options (cddr context)])
                       (or (null? options)
                           (and (not (assq (caar options) (cdr options))) (unique (cdr options)))))))
      (error 'validate-edit-context
             "expected (key label . options), the options among undo, commit, expected, labels and revision" context))
    (when context
      (writable-properties
        (append (validate-properties (context-undo context)) (validate-properties (context-commit context))))
      (validate-expected (context-expected context))
      (let ([revision (context-revision context)])
        (unless (or (not revision) (and (integer? revision) (exact? revision) (>= revision 0)))
          (error 'validate-edit-context "expected a nonnegative revision or false" revision)))
      (let ([labels (context-labels context)])
        (unless (and (list? labels) (for-all (lambda (label) (and (pair? label) (symbol? (car label)))) labels))
          (error 'validate-edit-context "expected labels as an alist with symbol keys" labels))))
    context)

  (define (context-option context key fallback)
    (cond [(and context (pair? (cdr context)) (assq key (cddr context))) => cdr]
          [else fallback]))

  (edoc "An edit context's undo facts, the facts that travel with the inverse; none without."
        (context any "the context, or #f")
        (returns list))
  (define (context-undo context) (context-option context 'undo '()))

  (edoc "An edit context's commit facts, the facts that survive undo; none without."
        (context any "the context, or #f")
        (returns list))
  (define (context-commit context) (context-option context 'commit '()))

  (edoc "An edit context's expected facts, the review guarding the commit, or #f."
        (context any "the context, or #f")
        (returns any))
  (define (context-expected context) (context-option context 'expected #f))

  (edoc "An edit context's exact revision guard, or false to allow ordinary rebasing."
        (context any "edit context") (returns (or integer #f)))
  (define (context-revision context) (context-option context 'revision #f))

  (edoc "An edit context's labels for the log entry, (batch . id) among them; none without."
        (context any "the context, or #f")
        (returns list))
  (define (context-labels context) (context-option context 'labels '()))

)
