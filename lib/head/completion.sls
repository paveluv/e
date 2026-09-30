;; Completion contracts are independent of prompt placement and input dispatch.
(import (only (foundation edoc) elibrary))
(elibrary (head completion)
  (export candidate-cells candidate-context candidate-label candidate-styles candidate-value candidate?
          make-candidate make-source provider register!
          source-basis source-context source-kind source-lookup source-release source-settle source?)
  (import (rnrs) (only (chezscheme) list-head) (prefix (core kernel) kernel:))

  (define providers (kernel:make-registry car))

  (edoc "Register a head completion namespace. The factory receives portable configuration and captured origin, and returns a source. It runs once per prompt outside painting; any filesystem or environment work stays in its owning service."
        (name symbol "namespace") (schema integer "positive recipe version")
        (factory procedure "configuration and origin -> completion source"))
  (define (register! name schema factory)
    (unless (and (symbol? name) (integer? schema) (exact? schema) (> schema 0) (procedure? factory))
      (error 'register! "invalid completion provider"))
    (kernel:registry-add! providers (cons (list name schema) factory)))

  (edoc "Resolve a portable (namespace schema configuration) recipe to its registered factory, or false. Resolving does not execute it; hosts can use factory identity to detect replacement."
        (recipe datum "provider recipe") (returns any))
  (define (provider recipe)
    (and (list? recipe) (= (length recipe) 3)
      (let ([entry (kernel:registry-find providers (lambda (p) (equal? (car p) (list-head recipe 2))))])
        (and entry (cdr entry)))))

  ;; A cursor-aware source returns (values start end expansions candidates).
  ;; Expansions may be a thunk: resolve only for a new Tab normalization,
  ;; not when refreshing the live list or cycling already prepared results.
  ;; Candidates replace [start,end); #f start means no completable token.
  ;; Unlike a prefix source, it normalizes on the first Tab and keeps its
  ;; candidate list live after the second. Existing list procedures stay simple.
  ;; An optional settle procedure, (settle text position), receives the input
  ;; after a sole match has been inserted and returns the (text . position)
  ;; to continue with: M-x closes forms and steps to the next argument. An
  ;; Kind describes the list; context names the exact argument type.
  (edoc "A head completion source independent of prompt placement."
    (lookup procedure "text and caret -> range, expansions and candidates")
    (settle (or procedure #f) "advance after a sole completion")
    (kind (or procedure string #f) "completion label")
    (basis procedure "local revision stamp") (release procedure "lifetime cleanup")
    (context (or procedure #f) "text and caret -> explicit argument context"))
  (define-record-type (source %make-source source?) (fields lookup settle kind basis release context))

  (edoc "Construct a completion source. Optional basis and release callbacks fence dynamic providers; context supplies the exact type and captured argument data to a presentation. Matching and normalization remain in lookup."
    (lookup procedure "text and caret -> range, expansions, candidates")
    (settle (or procedure #f) "optional settle step")
    (kind (or procedure string #f) "optional label")
    (basis procedure "optional revision stamp, with release") (release procedure "optional cleanup, with basis")
    (context (or procedure #f) "optional argument context"))
  (define make-source
    (case-lambda
      [(lookup) (make-source lookup #f #f)]
      [(lookup settle) (make-source lookup settle #f)]
      [(lookup settle kind) (%make-source lookup settle kind (lambda () #f) (lambda () (values)) #f)]
      [(lookup settle kind basis release) (%make-source lookup settle kind basis release #f)]
      [(lookup settle kind basis release context) (%make-source lookup settle kind basis release context)]))

  ;; A display label and its character styles are independent of the string
  ;; inserted on selection. The lookup result owns both, including during cycling.
  (edoc "A candidate's literal insertion, styled label, optional semantic cells and argument context. Context is data, never a preview callback."
    (value string "inserted text") (label string "display label") (styles (or vector #f) "label styles")
    (cells (or vector #f) "semantic label and hint") (context (or list #f) "type and explicit value/receiver context"))
  (define-record-type (candidate %make-candidate candidate?) (fields value label styles cells context))

  (edoc "Construct a literal insertion candidate, optionally with semantic label/hint cells and context for a scoped presentation. Selecting it never executes a callback."
    (value string "insertion") (label string "label") (styles (or vector #f) "label styles")
    (cells (or vector #f) "optional semantic cells") (context (or list #f) "optional typed value context") (returns (record candidate)))
  (define make-candidate
    (case-lambda
      [(value label styles) (%make-candidate value label styles #f #f)]
      [(value label styles cells) (%make-candidate value label styles cells #f)]
      [(value label styles cells context) (%make-candidate value label styles cells context)]))

)
