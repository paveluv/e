;; Corpus work and private page publication remain in the base. Local
;; module documentation accompanies a query instead of becoming global.
;; The head's page receipt is read once and kept until this head changes
;; the page or the store reports a change to its buffer.
(library (reference)
  (export fetch! page page! lookup entries browser-url)
  (import (chezscheme) (prefix (client) client:) (prefix (doc) doc:)
          (prefix (edoc) edoc:) (prefix (kernel) kernel:) (prefix (datum) datum:))
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
  (define (fetch!)
    (forget-page!)
    (client:request 'reference-fetch))
  (define (check-head head)
    (unless (equal? head (client:identity)) (error 'reference "a head addresses its own page")))
  (define (page head)
    (check-head head)
    (when (eq? current 'unknown) (set! current (client:request 'reference-page)))
    (datum:copy current))
  (define (page! head name keys . basis)
    (check-head head)
    (forget-page!)
    (client:request 'reference-page! name keys basis (documents)))
  (define (lookup name)
    (map doc:from-datum (client:request 'reference-lookup name (documents))))
  (define (entries . predicate)
    (let ([entries (map doc:from-datum (client:request 'reference-entries (documents)))])
      (if (pair? predicate) (filter (car predicate) entries) entries)))
  (define (browser-url entry) (client:request 'reference-url (doc:to-datum entry)))
)
