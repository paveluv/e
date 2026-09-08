;; doc.e -- the documentation corpus's entry: the library (doc).
;;
;; One record for every documented name -- from the reference corpus
;; reference.e extracts (TSPL, CSUG) or from a module describing its
;; own commands -- and the registry modules publish into at init!, so
;; a module's reload retracts its entries with its other
;; registrations.  Below every module that documents itself, the
;; command layer included: describing needs no import from the
;; described.  Exported names drop the module stem: (doc:register!
;; entries), (doc:entries), (doc:forms e).

(library (doc)
  (export (rename (make-doc-entry make) (doc-entry? entry?)
                  (doc-names names) (doc-forms forms) (doc-returns returns)
                  (doc-libraries libraries) (doc-source source)
                  (doc-chapter chapter) (doc-url url)
                  (doc-description description))
          register! entries to-datum from-datum call-with-entries)
  (import (rnrs) (only (chezscheme) void make-thread-parameter parameterize)
          (prefix (kernel) kernel:) (prefix (datum) datum:))

  (define-record-type (doc-entry make-doc-entry doc-entry?)
    (fields (immutable names doc-names)           ; symbols defined
            (immutable forms doc-forms)           ; ((kind . template) ...)
            (immutable returns doc-returns)       ; string or #f
            (immutable libraries doc-libraries)   ; ("(rnrs base)" ...)
            (immutable source doc-source)         ; tspl or csug
            (immutable chapter doc-chapter)       ; chapter title
            (immutable url doc-url)               ; page anchor
            (immutable description doc-description)))  ; markdown

  (define (entry-datum->doc-entry entry)
    (unless (and (list? entry) (= (length entry) 8))
      (error 'doc:register!
             "expected (names forms returns libraries source chapter url description)"
             entry))
    (apply make-doc-entry entry))

  (define (from-datum entry) (entry-datum->doc-entry (datum:copy entry)))
  (define (to-datum entry)
    (datum:copy
      (list (doc-names entry) (doc-forms entry) (doc-returns entry) (doc-libraries entry)
            (doc-source entry) (doc-chapter entry) (doc-url entry) (doc-description entry))))

  ;; A connected head contributes its own module documentation to a query.
  ;; It is scoped to that call, never registered globally under another head.
  (define query-entries (make-thread-parameter '()))
  (define (call-with-entries entries thunk)
    (parameterize ([query-entries (map from-datum entries)]) (thunk)))

  ;; Module documentation, registered in batches: a kernel registry, so
  ;; a module's reload retracts its entries along with its other
  ;; registrations.
  (define descriptions (kernel:make-registry))

  (define (register! entries)
    ;; Publish module documentation in the eight-field format of
    ;; describe.sdata -- (names forms returns libraries source chapter
    ;; url description) per entry -- validated here.
    (kernel:registry-add! descriptions (map entry-datum->doc-entry entries))
    (void))

  (define (entries)
    ;; every registered entry, oldest batch first
    (append (apply append (reverse (kernel:registry-items descriptions))) (query-entries)))

) ;; library (doc)
