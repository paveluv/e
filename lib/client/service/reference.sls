;; Corpus work and private page publication remain in the base. Local
;; module documentation accompanies a query instead of becoming global.
;; The head's page receipt is read once and kept until this head changes
;; the page or the store reports a change to its buffer.
(import (only (foundation edoc) elibrary))
(elibrary (service reference)
  (export fetch! page page! lookup entries browser-url)
  (import (chezscheme)
          (prefix (core client) client:)
          (prefix (service doc) doc:)
          (prefix (foundation edoc) edoc:)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:))
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
  (define current 'unknown)
  (define (forget-page!) (set! current 'unknown))
  (define invalidation
    (kernel:call-with-runtime-registrations
      (lambda ()
        (client:subscribe! 'changed
          (lambda (batch)
            (when (or (not batch) (and (pair? current) (assv (car current) batch)))
              (forget-page!)))))))
  (edoc "Ask the base to download the reference corpus.")
  (define (fetch!)
    (forget-page!)
    (client:request 'reference-fetch))
  (define (check-head head)
    (unless (equal? head (client:identity)) (error 'reference "a head addresses its own page")))
  (edoc "This head's describe page receipt, (id revision selected-name), or #f."
        (head head "the head's identity")
        (returns (or list #f))
        (effects internal))
  (define (page head)
    (check-head head)
    (when (eq? current 'unknown) (set! current (client:request 'reference-page)))
    (datum:copy current))
  (edoc "Publish or refresh this head's describe page for a name, sending its documented definitions along."
        (head head "the head's identity")
        (name (or symbol string) "the documented name")
        (keys (list-of string) "the key spellings bound to it")
        (basis (list-of pair) "(id . revision) to refresh, at most one"))
  (define (page! head name keys . basis)
    (check-head head)
    (forget-page!)
    (client:request 'reference-page! name keys basis (documents)))
  (edoc "Every entry for a name, this head's documented definitions included."
        (name (or symbol string) "the name")
        (returns (list-of (record doc-entry))))
  (define (lookup name)
    (map doc:from-datum (client:request 'reference-lookup name (documents))))
  (edoc "Every entry, optionally filtered."
        (predicate (list-of procedure) "a predicate on entries, at most one")
        (returns (list-of (record doc-entry))))
  (define (entries . predicate)
    (let ([entries (map doc:from-datum (client:request 'reference-entries (documents)))])
      (if (pair? predicate) (filter (car predicate) entries) entries)))
  (edoc "An entry's documentation URL in the browser, or #f."
        (entry (record doc-entry) "the entry")
        (returns (or string #f)))
  (define (browser-url entry)
    (client:request 'reference-url (doc:to-datum entry)))
)
