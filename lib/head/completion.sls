;; Completion contracts are independent of prompt placement and input dispatch.
(import (only (foundation edoc) elibrary))
(elibrary (head completion)
  (export candidate-label candidate-preview candidate-styles candidate-value candidate?
          make-candidate make-searcher make-source searcher-done searcher-find searcher-next searcher-previous searcher?
          source-kind source-lookup source-settle source-track source?)
  (import (rnrs))

  ;; A cursor-aware source returns (values start end expansions candidates).
  ;; Expansions may be a thunk: resolve only for a new Tab normalization,
  ;; not when refreshing the live list or cycling already prepared results.
  ;; Candidates replace [start,end); #f start means no completable token.
  ;; Unlike a prefix source, it normalizes on the first Tab and keeps its
  ;; candidate list live after the second. Existing list procedures stay simple.
  ;; An optional settle procedure, (settle text position), receives the input
  ;; after a sole match has been inserted and returns the (text . position)
  ;; to continue with: M-x closes forms and steps to the next argument. An
  ;; optional kind, (kind text position) or a constant string, names what
  ;; the completions are for the list's status line. An optional track,
  ;; (track text position), gives (maker . needle) where a live search
  ;; stands in for the list at the cursor: the prompt makes the searcher
  ;; once, feeds it the needle as it changes, and ends it as the cursor
  ;; leaves.
  (edoc "A head completion source independent of its host or input reader."
        (lookup procedure "text and caret -> replacement range, expansions and candidates")
        (settle (or procedure #f) "advance after a sole completion")
        (kind (or procedure string #f) "completion label")
        (track (or procedure #f) "optional live search provider"))
  (define-record-type (source %make-source source?) (fields lookup settle kind track))

  (edoc "A cursor-aware source: (lookup text position) gives (values start end expansions candidates), start #f meaning no completable token; an optional settle step, (settle text position), gives the (text . position) to continue with after a sole match is inserted; an optional kind, (kind text position) or a string, names what the completions are for the list's status line; an optional track, (track text position), gives (maker . needle) where a live search stands in for the list."
        (lookup procedure "the completion source")
        (settle (or procedure #f) "the settle step")
        (kind (or procedure string #f) "what the completions are")
        (track (or procedure #f) "where a live search stands in"))
  (define make-source
    (case-lambda
      [(lookup)
       (%make-source lookup #f #f #f)]
      [(lookup settle)
       (%make-source lookup settle #f #f)]
      [(lookup settle kind)
       (%make-source lookup settle kind #f)]
      [(lookup settle kind track)
       (%make-source lookup settle kind track)]))

  ;; A searcher stands in for the list at a typed argument: it finds the
  ;; needle's matches in the current buffer and highlights them as a search
  ;; would, Tab visits them in turn, and nothing is ever inserted.
  (edoc "A live search standing in for a completion list: (find needle) refreshes the needle's matches, giving (index . count) with index #f when none; (next) and (previous) preview the neighbouring match, giving the same; (done accepted?) ends the preview and restores any temporary selection."
        (find procedure "(find needle) giving (index . count)")
        (next procedure "(next) giving (index . count)")
        (previous procedure "(previous) giving (index . count)")
        (done procedure "(done accepted?)"))
  (define-record-type searcher (fields find next previous done))

  ;; A display label and its character styles are independent of the string
  ;; inserted on selection. The lookup result owns both, including during cycling.
  (edoc "A candidate's insertion text, styled label and optional reversible preview."
        (value string "inserted text") (label string "display label")
        (styles (or vector #f) "label character styles") (preview (or procedure #f) "show candidate and return cleanup"))
  (define-record-type (candidate %make-candidate candidate?)
    (fields value label styles preview))

  (edoc "A completion candidate: the text inserted on selection, the label the list shows with its styles, and an optional preview, a thunk that shows the candidate's value in the editor while it is the inserted one and gives the thunk undoing the showing."
        (value string "the text inserted on selection")
        (label string "the text shown in the list")
        (styles (or vector #f) "the label's styles")
        (preview (list-of procedure) "the preview thunk, at most one")
        (returns (record candidate)))
  (define (make-candidate value label styles . preview)
    (%make-candidate value label styles (and (pair? preview) (car preview))))

)
