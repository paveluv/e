;; Immutable text mirrors and declared edit intent, independent of a host.
(import (only (foundation edoc) elibrary))
(elibrary (head text-source)
  (export adopt! basis-text call-grouped! changes current-batch edit! forget! history!
          (rename (source-id id) (source-lines lines)) lookup make observe! open! project-positions rebase
          (rename (source-revision revision)) snapshot span)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core property) property:) (prefix (foundation text) text:)
          (prefix (service log) log:) (prefix (state store) store:))

  (define limit 256)
  (define mirrors (make-eqv-hashtable))
  (define observers (kernel:make-registry))

  ;; Grouping is command policy, shared by every text control and host.
  ;; The store still owns admission, attribution and undo history.
  (define group (make-parameter #f))
  (define-record-type edit-group (fields actor batch label labels))

  (edoc "The active command's batch for this actor, or false outside a group."
        (actor actor "editing actor") (returns (or list #f)))
  (define (current-batch actor)
    (and (group) (equal? actor (edit-group-actor (group))) (edit-group-batch (group))))

  (edoc "Run text submissions as one undo step per document. Nested groups for the same actor retain the outer label and batch; a false label retains each document's first accepted edit label."
        (actor actor "editing actor") (label (or string #f) "undo label") (thunk thunk "commands") (returns any))
  (define (call-grouped! actor label thunk)
    (unless (or (not label) (string? label)) (error 'call-grouped! "expected a label or false" label))
    (if (current-batch actor) (thunk)
      (parameterize ([group (make-edit-group actor
                              (list actor (gensym->unique-string (gensym "batch"))) label (make-eqv-hashtable))])
        (thunk))))

  (define (grouped-context actor id context proc)
    (let ([batch (current-batch actor)])
      (if (not batch) (proc context)
        (let* ([context (property:edit-context context)] [labels (edit-group-labels (group))]
               [existing (hashtable-ref labels id #f)]
               ;; Reserve a label before callbacks can submit another edit.
               ;; A refused attempt cannot name a later successful action.
               [entry (or existing (cons (or (edit-group-label (group)) (and context (cadr context))) #f))])
          (unless existing (hashtable-set! labels id entry))
          (guard (ex [else (when (and (not existing) (not (cdr entry))) (hashtable-delete! labels id)) (raise ex)])
            (call-with-values
              (lambda ()
                (proc (cons* batch (car entry)
                        (cons (cons 'labels (cons (cons 'batch batch)
                                              (filter (lambda (p) (not (eq? (car p) 'batch))) (property:context-labels context))))
                          (if context (filter (lambda (p) (not (eq? (car p) 'labels))) (cddr context)) '())))))
              (lambda result (set-cdr! entry #t) (apply values result))))))))

  (edoc "An adopted text source, shared by all its head projections. Local sources have no store identity; neither kind contains selection or geometry."
        (id (or integer #f) "store identity") (lines any "immutable text")
        (revision integer "content revision") (links list "bounded revision links"))
  (define-record-type source (fields id (mutable lines) (mutable revision) (mutable links)))

  (edoc "Look up an already acquired document mirror without I/O." (id integer "store identity") (returns any))
  (define (lookup id) (hashtable-ref mirrors id #f))

  (edoc "Create a local mirror or reuse a document's single mirror. Initial text is already acquired; an existing mirror is never overwritten."
        (id (or integer #f) "store identity") (lines any "immutable text") (revision integer "content revision") (returns (record source)) (effects internal))
  (define (make id lines revision)
    (or (and id (lookup id))
      (let ([source (make-source id lines revision '())])
        (when id (hashtable-set! mirrors id source)) source)))

  (edoc "Forget an unavailable document's mirror. Retained frames cannot edit it." (id integer "store identity"))
  (define (forget! id) (hashtable-delete! mirrors id))

  (edoc "Observe adopted shared text as (source basis lines revision changes). Legacy host adapters follow anchors here; observers must not perform a second edit. Module retraction removes the observer."
        (proc procedure "head-local adoption observer"))
  (define (observe! proc) (kernel:registry-add! observers proc))

  (define (links basis revision changes tail)
    (let loop ([from basis] [rest changes] [out tail])
      (if (null? rest)
        (let ([out (if (< from revision) (cons (list from revision) out) out)])
          (if (> (length out) limit) (list-head out limit) out))
        (let ([to (caar rest)])
          (let group ([rest rest] [steps '()])
            (if (and (pair? rest) (= (caar rest) to))
              (group (cdr rest) (cons (car rest) steps))
              (loop to rest (cons (cons* from to (reverse steps)) out))))))))

  (edoc "Read adopted text, revision and an exact delta chain since basis; false means history is unavailable. No remote reads or callbacks."
        (source (record source) "mirror") (basis (or integer #f) "earlier revision, false omits the chain"))
  (define (snapshot source basis)
    (unless (or (not basis) (and (integer? basis) (exact? basis) (>= basis 0)))
      (error 'snapshot "expected a content revision or false" basis))
    (values (source-lines source) (source-revision source)
      (and basis (<= basis (source-revision source))
        (let scan ([at (source-revision source)] [rest (source-links source)] [out '()])
          (cond [(= at basis) (map (lambda (row) (list (car row) (cadr row) (caddr row))) out)]
            [(or (< at basis) (null? rest)) #f]
            [(= (cadar rest) at) (scan (caar rest) (cdr rest) (append (cddar rest) out))]
            [else #f])))))

  (edoc "Adopt a coherent snapshot and its provenance. Older snapshots cannot roll back a mirror; an equal snapshot can restore missing history. Notify projections only after text is coherent."
        (source (record source) "mirror") (basis integer "chain start") (lines any "immutable text")
        (revision integer "snapshot revision") (changes any "delta rows or false"))
  (define (adopt! source basis lines revision changes)
    (let ([old (source-revision source)])
      (when (>= revision old)
        (let-values ([(unused now retained) (snapshot source basis)])
          (cond
            [(and changes (not retained)) (source-links-set! source (links basis revision changes '()))]
            [(> revision old)
             (source-links-set! source
               (if (and changes (<= basis old))
                 (links old revision (filter (lambda (row) (> (car row) old)) changes) (source-links source)) '()))]))
        (source-lines-set! source lines) (source-revision-set! source revision)
        (when (and (source-id source) (> revision old))
          (for-each (lambda (proc) (proc source basis lines revision changes)) (kernel:registry-items observers))))))

  (edoc "Acquire or refresh a visible document outside painting. Optionally retain history from a saved view basis; missing history remains explicit."
        (actor actor "head") (id integer "document") (basis (list-of integer) "optional saved revision") (returns any))
  (define (open! actor id . basis)
    (unless (<= (length basis) 1) (error 'open! "expected at most one basis"))
    (if (not (store:visible? actor id)) (begin (forget! id) #f)
      (let* ([old (lookup id)] [from (if (pair? basis) (car basis) (and old (source-revision old)))])
        (let-values ([(lines revision changes) (store:snapshot-since id from)])
          (let ([source (make id lines revision)])
            (adopt! source (or from revision) lines revision changes) source)))))

  (edoc "Get exact deltas from a selection basis to a retained displayed revision. A missing bridge refuses intent instead of inferring a diff."
        (source (record source) "mirror") (basis integer "selection revision") (end integer "displayed revision") (returns any))
  (define (changes source basis end)
    (let-values ([(lines revision rows) (snapshot source basis)])
      (and rows (<= basis end revision)
        (let-values ([(unused now tail) (snapshot source end)])
          (and tail (map caddr (filter (lambda (row) (<= (car row) end)) rows)))))))

  (edoc "Rebase logical endpoints with deterministic insertion affinity through a known delta chain. False history stays false."
        (positions list "logical text positions") (changes any "delta chain or false") (returns any))
  (define (rebase positions changes)
    (and changes (map (lambda (p) (fold-left text:rebase-position p changes)) positions)))

  (edoc "The ordered span between two logical endpoints." (positions list "caret and anchor") (returns any))
  (define (span positions)
    (let ([a (car positions)] [b (cadr positions)])
      (if (or (< (car a) (car b)) (and (= (car a) (car b)) (<= (cdr a) (cdr b))))
        (text:make-span (car a) (cdr a) (car b) (cdr b))
        (text:make-span (car b) (cdr b) (car a) (cdr a)))))
  (define (refuse message) (raise (condition (kernel:make-refusal) (make-message-condition message))))

  (edoc "Reconstruct the text at a selection's declared basis from a displayed snapshot. Missing provenance refuses, never clamps an edit to new text."
        (source (record source) "mirror") (lines vector "displayed text") (revision integer "displayed revision") (basis integer "selection revision") (returns vector))
  (define (basis-text source lines revision basis)
    (let ([steps (changes source basis revision)])
      (unless steps (refuse "Selection history is unavailable"))
      (fold-left (lambda (lines delta)
                   (let ([inverse (text:invert-delta delta)])
                     (let-values ([(lines ignored) (text:apply-edit lines (text:delta-span inverse) (text:delta-inserted inverse))]) lines)))
        lines (reverse steps))))

  (edoc "Project requested positions from a proposed edit into the actual accepted edit and subsequent changes. Start/end refer to the accepted insertion, other positions to the proposal's result."
        (old vector "proposal text") (span any "proposal span") (replacement list "replacement lines")
        (actual any "accepted delta") (before list "intervening deltas") (after list "later deltas")
        (positions list "start, end or logical positions") (returns list))
  (define (project-positions old span replacement actual before after positions)
    (let ([proposal (delay (call-with-values (lambda () (text:apply-edit old span replacement)) list))])
      (map (lambda (wanted)
             (fold-left text:rebase-position
               (case wanted
                 [(start) (text:span-start (text:delta-span actual))] [(end) (text:delta-new-end actual)]
                 [else (let* ([plan (force proposal)] [text (car plan)]
                              [row (max 0 (min (car wanted) (- (vector-length text) 1)))]
                              [position (cons row (max 0 (min (cdr wanted) (string-length (vector-ref text row)))))])
                         (text:rebase-result-position position (cadr plan) actual before))]) after)) positions)))

  (edoc "Admit declared document intent independently of a view. Return coherent text, revision, changes, projected positions and committed revision for presentation adoption. A stale/overlapping edit refuses; no retry changes its meaning."
        (actor actor "editing actor") (basis list "(old-text document-id revision)") (span any "replaced span")
        (replacement list "replacement lines") (context any "store edit context") (positions list "desired logical result positions"))
  (define (edit! actor basis span replacement context positions)
    (grouped-context actor (cadr basis) context
      (lambda (context)
        (let-values ([(status info) (store:edit-with-snapshot! actor (cadr basis) (caddr basis) span replacement context 'any)])
          (unless (eq? status 'applied)
            (let ([reason (case info
                            [(read-only) "the buffer is read-only"] [(property-changed) "the buffer's reviewed facts changed"]
                            [(revision-changed) "the reviewed text changed"] [(overlap) "another edit overlaps this change"]
                            [else "the edit's revision is no longer available"])])
              (log:add! 'text-source:edit! (format "edit refused in document ~a: ~a" (cadr basis) reason))
              (refuse (format "Edit not applied: ~a" reason))))
          (let* ([committed (car info)] [changes (caddr info)] [backwards (reverse changes)]
                 [actual (caddar backwards)] [before (map caddr (reverse (cdr backwards)))])
            (let-values ([(text revision after) (store:snapshot-since (cadr basis) committed)])
              (values text revision (and after (append changes after))
                (if after (project-positions (car basis) span replacement actual before (map caddr after) positions) '()) committed)))))))

  (edoc "Undo or redo a document through its attributed journal without consulting a window. Presentation adoption follows separately; only a service failure maps to blocked."
        (actor actor "editing actor") (id integer "document") (direction (one-of undo redo) "step") (scope any "mine, all or actor"))
  (define (history! actor id direction scope)
    (guard (ex [else (values 'blocked 'store-unavailable)]) (store:history-step! actor id direction scope 'any)))
)
