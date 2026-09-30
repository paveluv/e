;; Corpus work and private page publication remain in the base. Local
;; module documentation accompanies a query instead of becoming global.
;; Page receivers are explicit documents, independent within one head.
(import (only (foundation edoc) elibrary))
(elibrary (service reference)
  (export browser-url create! entries fetch! lookup page select! signatures)
  (import (chezscheme)
          (prefix (core client) client:)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (service doc) doc:))
  (define (documents)
    ;; Registered module documentation, then entries read from the top-level
    ;; definitions that carry an edoc -- attached to their value, or recorded
    ;; under their name -- for the names the registry leaves out.
    (let* ([registered (doc:entries)]
           [covered (apply append (map doc:names registered))])
      (append (map doc:to-datum registered)
              (fold-left
                (lambda (out sym)
                  (let* ([signatures
                          (and (not (memq sym covered))
                               (or (and (top-level-bound? sym) (edoc:edoc-of (top-level-value sym)))
                                   (edoc:edoc-named sym)))]
                         [entry (and signatures (edoc:edoc-entry sym signatures))])
                    (if entry (cons entry out) out)))
                '() (environment-symbols (interaction-environment))))))
  (define signature-cache #f) ; the base's signatures as last fetched, until the corpus changes
  (define signature-invalidation
    (kernel:call-with-runtime-registrations
      (lambda ()
        (client:subscribe! 'logged
          (lambda (entry presentation)
            ;; A fetch under way or done: the corpus may have changed.
            (when (memq (caddr entry) '(reference:run-fetch! reference:begin-fetch!))
              (set! signature-cache #f)))))))

  (edoc "Ask the base to start downloading the reference corpus; it returns at once, and the fetch's progress arrives as this head's log records, redrawn in place in the echo area.")
  (define (fetch!)
    (set! signature-cache #f)
    (client:request 'reference-fetch))

  (edoc "The documented procedure forms of the base's corpus and registered modules, (name form ...) per name, fetched in one request and kept until the corpus changes."
        (returns list)
        (effects internal))
  (define (signatures)
    (or signature-cache
        (let ([next (client:request 'reference-signatures)])
          (set! signature-cache next)
          next)))
  (define (check-head head)
    (unless (equal? head (client:identity)) (error 'reference "expected this head's identity")))

  (edoc "Read an explicit reference page as (id revision selected-name), or false when unavailable."
        (head head "requesting head") (id integer "source document")
        (returns (or list #f))
        (effects internal))
  (define (page head id)
    (check-head head)
    (client:request 'reference-page id))

  (edoc "Create an independent private reference page with this head's documented definitions. Return its source document or false for an undocumented name."
        (head head "requesting head") (name (or symbol string) "documented name")
        (keys (list-of string) "contextual key spellings") (returns (or integer #f)))
  (define (create! head name keys)
    (check-head head)
    (client:request 'reference-create name keys (documents)))

  (edoc "Select or refresh an explicit reference page against its revision; changed or deleted sources refuse."
        (head head "requesting head") (id integer "source document")
        (revision integer "reviewed revision") (name (or symbol string) "documented name")
        (keys (list-of string) "contextual key spellings") (returns (or integer #f)))
  (define (select! head id revision name keys)
    (check-head head)
    (client:request 'reference-select id revision name keys (documents)))

  (edoc "Every entry for a name, this head's documented definitions included."
        (name (or symbol string) "the name")
        (returns (list-of (record doc-entry))))
  (define (lookup name)
    (map doc:from-datum (client:request 'reference-lookup name (documents))))

  (edoc "Every entry, optionally filtered."
        (predicate (list-of procedure) "a predicate on entries, at most one")
        (returns (list-of (record doc-entry))) (public))
  (define (entries . predicate)
    (let ([entries (map doc:from-datum (client:request 'reference-entries (documents)))])
      (if (pair? predicate) (filter (car predicate) entries) entries)))

  (edoc "An entry's documentation URL in the browser, or #f."
        (entry (record doc-entry) "the entry")
        (returns (or string #f)))
  (define (browser-url entry)
    (client:request 'reference-url (doc:to-datum entry)))
)
