;; doc.sls -- the documentation corpus's entry: the library (doc).
;;
;; One record for every documented name -- from the reference corpus
;; reference.sls extracts (TSPL, CSUG) or from a module describing its
;; own commands -- and the registry modules publish into at init!, so
;; a module's reload retracts its entries with its other
;; registrations.  Below every module that documents itself, the
;; command layer included: describing needs no import from the
;; described.  Exported names drop the module stem: (doc:register!
;; entries), (doc:entries), (doc:forms e).

(import (only (edoc) elibrary))
(elibrary (doc)
  (export (rename (make-doc-entry make) (doc-entry? entry?))
          names forms returns libraries source chapter url description
          register! entries to-datum from-datum call-with-entries)
  (import (rnrs)
          (only (chezscheme) void make-thread-parameter parameterize)
          (prefix (kernel) kernel:)
          (prefix (datum) datum:))

  (edoc "A documentation entry in the describe corpus's eight-field format."
        (names (list-of symbol) "the names it defines")
        (forms list "((kind . template) ...), kind procedure, syntax, parameter or variable")
        (returns (or string #f) "what a procedure returns")
        (libraries (list-of string) "the libraries the names come from")
        (source any "tspl, csug, a module, or edoc")
        (chapter string "the chapter title")
        (url (or string #f) "a reference URL")
        (description string "the Markdown description"))
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
  (edoc "A copy of the names a documentation entry defines."
        (entry (record doc-entry) "the entry")
        (returns (list-of symbol)))
  (define (names entry)
    (datum:copy (doc-names entry)))
  (edoc "A copy of an entry's forms, ((kind . template) ...)."
        (entry (record doc-entry) "the entry")
        (returns list))
  (define (forms entry)
    (datum:copy (doc-forms entry)))
  (edoc "A copy of what an entry's procedure returns, or #f."
        (entry (record doc-entry) "the entry")
        (returns (or string #f)))
  (define (returns entry)
    (datum:copy (doc-returns entry)))
  (edoc "A copy of the libraries an entry's names come from."
        (entry (record doc-entry) "the entry")
        (returns (list-of string)))
  (define (libraries entry)
    (datum:copy (doc-libraries entry)))
  (edoc "An entry's source: tspl, csug, a module, or edoc."
        (entry (record doc-entry) "the entry")
        (returns any))
  (define (source entry)
    (datum:copy (doc-source entry)))
  (edoc "An entry's chapter title."
        (entry (record doc-entry) "the entry")
        (returns string))
  (define (chapter entry)
    (datum:copy (doc-chapter entry)))
  (edoc "An entry's reference URL, or #f."
        (entry (record doc-entry) "the entry")
        (returns (or string #f)))
  (define (url entry)
    (datum:copy (doc-url entry)))
  (edoc "An entry's Markdown description."
        (entry (record doc-entry) "the entry")
        (returns string))
  (define (description entry)
    (datum:copy (doc-description entry)))

  (edoc "An entry from its eight-field datum, (names forms returns libraries source chapter url description)."
        (entry list "the datum")
        (returns (record doc-entry)))
  (define (from-datum entry)
    (unless (and (list? entry) (= (length entry) 8))
      (error 'doc:from-datum
             "expected (names forms returns libraries source chapter url description)"
             entry))
    (apply make-doc-entry entry))

  (edoc "A copy of an entry as its eight-field datum."
        (entry (record doc-entry) "the entry")
        (returns list))
  (define (to-datum entry)
    (datum:copy
      (list (doc-names entry) (doc-forms entry) (doc-returns entry) (doc-libraries entry)
            (doc-source entry) (doc-chapter entry) (doc-url entry) (doc-description entry))))

  ;; A connected head contributes its own module documentation to a query.
  ;; It is scoped to that call, never registered globally under another head.
  (define query-entries (make-thread-parameter '()))
  (edoc "Run a thunk with extra entries, a connected head's module documentation, visible to queries in that call only."
        (entries list "the entry data")
        (thunk thunk "the query")
        (returns any))
  (define (call-with-entries entries thunk)
    (parameterize ([query-entries (map from-datum entries)]) (thunk)))

  ;; Module documentation, registered in batches: a kernel registry, so
  ;; a module's reload retracts its entries along with its other
  ;; registrations.
  (define descriptions (kernel:make-registry))

  (edoc "Publish module documentation in the eight-field format, validated, as a registry batch a reload retracts."
        (entries list "the entry data"))
  (define (register! entries)
    ;; Publish module documentation in the eight-field format of
    ;; describe.sdata -- (names forms returns libraries source chapter
    ;; url description) per entry -- validated here.
    (kernel:registry-add! descriptions (map from-datum entries))
    (void))

  (edoc "Every registered entry, oldest batch first, then the current query's."
        (returns (list-of (record doc-entry))))
  (define (entries)
    ;; Every registered entry, oldest batch first. The final empty tail makes
    ;; append copy every batch's spine, including the temporary query entries.
    (apply append (append (reverse (kernel:registry-items descriptions)) (list (query-entries) '()))))

) ;; library (doc)
