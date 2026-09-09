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
  (export (rename (make-doc-entry make) (doc-entry? entry?))
          names forms returns libraries source chapter url description
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
            (immutable description doc-description))   ; markdown
    ;; Own every mutable field once at construction, whether the record came
    ;; from a module, the corpus, a request or a direct constructor call.
    (protocol (lambda (new) (lambda fields (apply new (datum:copy fields))))))

  ;; Records can be shared across queries and indexes; their contents cannot
  ;; escape through either a field accessor or the plain-data codec.
  (define (names entry) (datum:copy (doc-names entry)))
  (define (forms entry) (datum:copy (doc-forms entry)))
  (define (returns entry) (datum:copy (doc-returns entry)))
  (define (libraries entry) (datum:copy (doc-libraries entry)))
  (define (source entry) (datum:copy (doc-source entry)))
  (define (chapter entry) (datum:copy (doc-chapter entry)))
  (define (url entry) (datum:copy (doc-url entry)))
  (define (description entry) (datum:copy (doc-description entry)))

  (define (from-datum entry)
    (unless (and (list? entry) (= (length entry) 8))
      (error 'doc:from-datum
             "expected (names forms returns libraries source chapter url description)"
             entry))
    (apply make-doc-entry entry))

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
    (kernel:registry-add! descriptions (map from-datum entries))
    (void))

  (define (entries)
    ;; Every registered entry, oldest batch first. The final empty tail makes
    ;; append copy every batch's spine, including the temporary query entries.
    (apply append (append (reverse (kernel:registry-items descriptions)) (list (query-entries) '()))))

) ;; library (doc)
