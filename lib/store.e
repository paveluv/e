;; store.e -- the buffer store: the library (store).
;;
;; Multi-actor buffer state over the pure (text) algebra.  Every
;; buffer carries a revision; every mutation is a transaction naming
;; its actor and the revision it was based on, and is either applied
;; -- rebased across whatever landed since the basis -- or rejected
;; as stale, never guessed.  Marks are actor-owned positions rebased
;; on every edit; subscribers hear about applied edits; undo is a
;; shared, attributed history where "undo my edit" applies the
;; inverse only while it still rebases cleanly.
;;
;; The store lives in a kernel persistent cell, so hot-reloading this
;; module preserves every buffer.  One mutex serializes all mutation
;; -- the design's single writer in its simplest form.  Text vectors
;; are immutable by (text)'s discipline, so snapshots handed out
;; remain valid forever.
;;
;; Naming reads behind the import prefix: (store:edit! ...),
;; (store:snapshot ...).

(library (store)
  (export create! delete! reset! rename!
          buffer-list exists? buffer-name find-named
          snapshot snapshot-since snapshot-state revision line-count line extract
          edit! edit-with-snapshot! undo! redo! history-step! undo-authors history blame
          set-mark! mark drop-mark! marks
          set-property! set-properties! drop-property! property properties
          validate-properties validate-edit-context
          subscribe! unsubscribe!)
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! make-mutex with-mutex format void remq)
          (prefix (text) text:)
          (prefix (kernel) kernel:))

  ;;; The store -------------------------------------------------------------

  (define delta-log-limit 256)

  (define-record-type (buffer make-buffer buffer?)
    (fields (mutable label)
            (mutable text)       ; immutable line vector, per (text)
            (mutable revision)
            (mutable deltas)     ; (#(revision actor delta origin facts) ...) newest first
            (mutable marks)      ; (((actor . name) . position) ...)
            (mutable undo)       ; undo groups, most recent operation first
            (mutable properties) ; ((key . datum) ...), see set-property!
            (mutable baseline)   ; cached (base-cell lines trailing?), or #f
            (mutable modified))) ; derived from text/trailing and the baseline

  (define-record-type undo-group
    (fields id actor key
            (mutable label)
            (mutable parts)      ; the group's last deltas, newest first
            (mutable live?)
            (mutable redo-actor) ; (requester), or #f when redo was invalidated
            (mutable complete?)))

  (define-record-type (store make-store store?)
    (fields lock
            buffers              ; id -> buffer
            (mutable next-id)
            (mutable events-front) ; ((event subscriber-token ...) ...)
            (mutable events-back)
            (mutable delivering?)))

  (define the-store
    (kernel:persistent-cell 'store
      (lambda ()
        (make-store (make-mutex)
                    (make-eqv-hashtable)
                    1 '() '() #f))))

  (define (current-store) (unbox the-store))

  ;; A property cell is an immutable (key . value) pair.  Its identity
  ;; is its version, including a tombstone for absence: even a write
  ;; that restores the old value invalidates an older undo precondition.
  ;; Keep the sentinel across reload along with the store's cells.
  (define missing-property
    (unbox (kernel:persistent-cell 'store-missing-property
             (lambda () (list 'missing-property)))))

  (define (property-cell properties key) (assq key properties))

  (define (replace-property-cell properties cell)
    (cons cell (remp (lambda (entry) (eq? (car entry) (car cell))) properties)))

  (define (apply-property-changes properties changes)
    (fold-left (lambda (properties change)
                 (replace-property-cell properties (caddr change)))
               properties changes))

  (define (property-value b key fallback)
    (let ([cell (property-cell (buffer-properties b) key)])
      (if (and cell (not (eq? (cdr cell) missing-property))) (cdr cell) fallback)))

  (define (refresh-modified! b)
    ;; Dirty state is store truth, never a head's bookkeeping write.
    ;; Parse the baseline once per property version; line equality can
    ;; share unchanged strings and avoids serializing a file per key.
    (let* ([cell (property-cell (buffer-properties b) 'base)]
           [base (property-value b 'base #f)]
           [cached (buffer-baseline b)]
           [baseline
            (and base
                 (if (and cached (eq? cell (car cached))) cached
                     (let-values ([(lines trailing?) (text:from-string base)])
                       (list cell lines trailing?))))]
           [text (buffer-text b)])
      (buffer-baseline-set! b baseline)
      (buffer-modified-set! b
        (if baseline
            (not (text:content=? text (property-value b 'trailing #t)
                                 (cadr baseline) (caddr baseline)))
            (not (and (= (vector-length text) 1)
                      (string=? (vector-ref text 0) "")))))))

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
                          [(trailing disposable) (boolean? (cdr entry))]
                          [else #t])
                        (valid (cdr rest) (cons (car entry) seen)))))))
      (error 'validate-properties "expected unique symbol keys and valid fact values" updates))
    updates)

  (define (writable-properties updates)
    (validate-properties updates)
    (when (assq 'modified updates)
      (error 'store "modified is derived from text and its baseline"))
    updates)

  (define (validate-edit-context context)
    ;; Undo facts travel with the inverse.  Commit facts describe external
    ;; state (e.g. a disk baseline) and survive undo, but commit atomically
    ;; with the text.  A key cannot appear in both sets.
    (unless (or (not context)
                (and (list? context) (memv (length context) '(2 3 4))
                     (or (not (cadr context)) (string? (cadr context)))))
      (error 'validate-edit-context "expected (key label [undo-facts [commit-facts]])" context))
    (when (and context (>= (length context) 3))
      (writable-properties
        (append (validate-properties (caddr context))
                (if (= (length context) 4) (validate-properties (cadddr context)) '()))))
    context)

  (define (install-properties! b updates)
    (buffer-properties-set! b
      (fold-left (lambda (facts entry) (replace-property-cell facts (cons (car entry) (cdr entry))))
                 (buffer-properties b) updates)))

  (define (property-data b)
    (cons (cons 'modified (buffer-modified b))
          (map (lambda (entry) (cons (car entry) (cdr entry)))
               (filter (lambda (cell) (not (eq? (cdr cell) missing-property)))
                       (buffer-properties b)))))

  (define (locked thunk)
    (with-mutex (store-lock (current-store)) (thunk)))

  (define (transact! thunk)
    ;; Mutation and its event enter the same critical section.  Only
    ;; after releasing it may this writer become the event drainer.
    (call-with-values
      (lambda () (locked thunk))
      (lambda result
        (drain-events!)
        (apply values result))))

  (define (buffer-of who id)
    (or (hashtable-ref (store-buffers (current-store)) id #f)
        (error who (format "no buffer ~a" id))))

  ;;; Lifecycle and reading --------------------------------------------------

  (define (create! actor buffer-name lines)
    ;; -> the new buffer's id.  Empty lines mean one empty line.
    ;; Subscribers hear (create id name actor).
    (let ([text (text:normalize lines)])
      (transact!
        (lambda ()
          (let* ([s (current-store)]
                 [id (store-next-id s)])
            (store-next-id-set! s (+ id 1))
            (let ([b (make-buffer buffer-name text 0 '() '() '() '() #f #f)])
              (refresh-modified! b)
              (hashtable-set! (store-buffers s) id b))
            (enqueue-event! `(create ,id ,buffer-name ,actor))
            id)))))

  (define (reset! actor id lines . facts)
    ;; Wholesale replacement: a new baseline, not an edit.  The delta
    ;; log and the undo history clear (a stale basis against a reset
    ;; refuses as basis-too-old), and marks clamp into the new text.
    ;; Related baseline facts may join the same transaction.  Loading
    ;; a file supplies base/trailing/stamp; ordinary edits use edit!.
    (unless (<= (length facts) 1) (error 'reset! "expected at most one fact batch" facts))
    (let* ([text (text:normalize lines)]
           [updates (writable-properties (if (pair? facts) (car facts) '()))]
           [new-revision
            (transact!
              (lambda ()
                (let ([b (buffer-of 'reset! id)]
                      [clamp (lambda (position)
                               (let* ([line (min (car position)
                                                 (- (vector-length text)
                                                    1))]
                                      [column
                                       (min (cdr position)
                                            (string-length
                                              (vector-ref text line)))])
                                 (cons line column)))])
                  (buffer-text-set! b text)
                  (install-properties! b updates)
                  (refresh-modified! b)
                  (buffer-revision-set! b (+ (buffer-revision b) 1))
                  (buffer-deltas-set! b '())
                  (buffer-undo-set! b '())
                  (buffer-marks-set!
                    b (map (lambda (entry)
                             (cons (car entry)
                                   (clamp-mark-value (cdr entry) clamp)))
                           (buffer-marks b)))
                  (enqueue-event! `(reset ,id ,(buffer-revision b) ,actor))
                  (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates)
                  (buffer-revision b))))])
      new-revision))

  (define (rename! actor id new-name)
    ;; subscribers hear (rename id new-name actor)
    (transact!
      (lambda ()
        (buffer-label-set! (buffer-of 'rename! id) new-name)
        (enqueue-event! `(rename ,id ,new-name ,actor))))
    (void))

  (define (delete! actor id)
    (transact!
      (lambda ()
        (buffer-of 'delete! id)
        (hashtable-delete! (store-buffers (current-store)) id)
        (enqueue-event! `(delete ,id ,actor))))
    (void))

  (define (buffer-list)
    (locked
      (lambda ()
        (vector->list (hashtable-keys (store-buffers (current-store)))))))

  (define (exists? id)
    (locked
      (lambda ()
        (and (hashtable-ref (store-buffers (current-store)) id #f) #t))))

  (define (buffer-name id)
    (locked (lambda () (buffer-label (buffer-of 'buffer-name id)))))

  (define (find-named wanted)
    ;; the lowest-numbered buffer with this name, or #f
    (locked
      (lambda ()
        (let ([ids (vector->list
                     (hashtable-keys
                       (store-buffers (current-store))))])
          (let scan ([ids (list-sort < ids)])
            (cond [(null? ids) #f]
                  [(equal? (buffer-label
                             (buffer-of 'find-named (car ids)))
                           wanted)
                   (car ids)]
                  [else (scan (cdr ids))]))))))

  (define (snapshot id)
    ;; -> (values text revision): the text vector is immutable, so the
    ;; snapshot stays coherent forever, at zero cost.  Racing writers
    ;; must compute their spans against a snapshot and pass its
    ;; revision as the edit's basis -- separate reads of revision and
    ;; content can straddle another actor's edit, and a span computed
    ;; that way may not survive the rebase.
    (locked
      (lambda ()
        (let ([b (buffer-of 'snapshot id)])
          (values (buffer-text b) (buffer-revision b))))))

  (define (snapshot-state id)
    ;; Save/clean checks need text and its facts from the same read.
    (locked
      (lambda ()
        (let ([b (buffer-of 'snapshot-state id)])
          (values (buffer-text b) (buffer-revision b) (property-data b))))))

  (define (snapshot-since id basis)
    ;; -> (values text revision changes), from one read.  Changes are
    ;; (revision actor delta) entries, oldest first, ending at exactly
    ;; this snapshot.  #f means a reset or history truncation removed
    ;; the basis (or it is in the future); '() means already current.
    ;; Subscribers use events as wakeups, then advance text and anchors
    ;; through this coherent chain, even if newer events are pending.
    (locked
      (lambda ()
        (let* ([b (buffer-of 'snapshot-since id)]
               [entries (entries-since b basis)])
          (values (buffer-text b) (buffer-revision b)
                  (and entries
                       (map change-data entries)))))))

  (define (change-data entry)
    (list (vector-ref entry 0) (vector-ref entry 1) (vector-ref entry 2)))

  (define (revision id)
    (locked (lambda () (buffer-revision (buffer-of 'revision id)))))

  (define (line-count id)
    (locked
      (lambda () (vector-length (buffer-text (buffer-of 'line-count id))))))

  (define (line id n)
    (locked
      (lambda ()
        (let ([text (buffer-text (buffer-of 'line id))])
          (unless (and (>= n 0) (< n (vector-length text)))
            (error 'line (format "no line ~a in buffer ~a" n id)))
          (vector-ref text n)))))

  (define (extract id span)
    (locked
      (lambda () (text:extract (buffer-text (buffer-of 'extract id)) span))))

  ;;; Edits -------------------------------------------------------------------

  (define (entries-since b basis)
    ;; The complete chain after basis, oldest first, or #f.  Edits and
    ;; incremental snapshot readers share the same retention boundary.
    (chain-since (buffer-deltas b) (buffer-revision b) basis))

  (define (chain-since deltas current basis)
    (cond
      [(= basis current) '()]
      [(or (> basis current)
           (< basis (- current (length deltas))))
       #f]
      [else
       (let take ([entries deltas] [acc '()])
         (cond [(null? entries) acc]
               [(<= (vector-ref (car entries) 0) basis) acc]
               [else (take (cdr entries)
                           (cons (car entries) acc))]))]))

  (define (rebase-through span deltas)
    ;; the span carried across each delta in order, or #f when any
    ;; step reports it stale
    (cond [(not deltas) #f]
          [(null? deltas) span]
          [else
           (let ([rebased (text:rebase-span span (car deltas))])
             (and rebased (rebase-through rebased (cdr deltas))))]))

  (define (install-edit! b id actor new-text delta origin facts commit-facts)
    ;; All committed edits, including history operations, pass here.
    ;; The attribution log always retains the actual deltas; cancelling
    ;; pairs is only a temporary proof used when planning another undo.
    (let* ([new-revision (+ (buffer-revision b) 1)]
           [entry (vector new-revision actor delta origin facts)])
      (buffer-text-set! b new-text)
      (buffer-revision-set! b new-revision)
      (buffer-deltas-set!
        b (bounded (cons entry (buffer-deltas b)) delta-log-limit))
      (buffer-properties-set! b (apply-property-changes (buffer-properties b) facts))
      (install-properties! b commit-facts)
      (refresh-modified! b)
      (buffer-marks-set!
        b (map (lambda (entry)
                 (cons (car entry) (rebase-mark-value (cdr entry) delta)))
               (buffer-marks b)))
      (enqueue-event!
        (append (list 'edit id new-revision actor delta)
                (if origin (list origin) '())))
      (for-each (lambda (change) (enqueue-event! `(property ,id ,(car change) ,actor))) facts)
      (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) commit-facts)
      (values new-revision delta)))

  (define (apply-locked! b id actor span replacement origin properties commit-facts)
    ;; the single mutation point; the caller holds the lock and has a
    ;; span valid against the buffer's current text
    (let-values ([(new-text delta)
                  (text:apply-edit (buffer-text b) span replacement)])
      (install-edit! b id actor new-text delta origin
        (map (lambda (update)
               (list (car update)
                     (or (property-cell (buffer-properties b) (car update))
                         (cons (car update) missing-property))
                     (cons (car update) (cdr update))))
             properties)
        commit-facts)))

  (define (bounded entries n)
    (let loop ([entries entries] [n n])
      (cond [(null? entries) '()]
            [(zero? n) '()]
            [else (cons (car entries)
                        (loop (cdr entries) (- n 1)))])))

  (define (remember-edit! b actor context)
    ;; A caller may identify a user-level action with (key label).
    ;; Keys are actor-local and buffer-local.  Missing keys make a new
    ;; action for each transaction; a group is never partially undone.
    (for-each
      (lambda (group)
        (when (and (undo-group-redo-actor group)
                   (equal? (car (undo-group-redo-actor group)) actor))
          (undo-group-redo-actor-set! group #f)))
      (buffer-undo b))
    (let* ([entry (car (buffer-deltas b))]
           [key (and context (car context))]
           [label (and context (cadr context))]
           [existing
            (and key
                 (find (lambda (group)
                         (and (undo-group-live? group)
                              (equal? (undo-group-actor group) actor)
                              (equal? (undo-group-key group) key)))
                       (buffer-undo b)))]
           [group (or existing
                      (make-undo-group (vector-ref entry 0) actor key label
                                       '() #t #f #t))]
           [parts (cons entry (undo-group-parts group))])
      (when label (undo-group-label-set! group label))
      (when (> (length parts) delta-log-limit)
        (undo-group-complete?-set! group #f))
      (undo-group-parts-set! group (bounded parts delta-log-limit))
      (buffer-undo-set!
        b (bounded (cons group (remq group (buffer-undo b))) delta-log-limit))))

  (define (edit! actor id basis span replacement . options)
    ;; The transaction: apply the edit as the actor meant it against
    ;; the basis revision, rebasing it across whatever landed since --
    ;; or refuse.  -> (values 'applied revision)
    ;;             |  (values 'stale 'overlap)       edited meanwhile
    ;;             |  (values 'stale 'basis-too-old) log outgrown
    ;; Optional (key label [properties]) groups transactions into one
    ;; undo and can change text-related properties in the same commit.
    (let-values ([(status detail)
                  (apply edit-with-snapshot! actor id basis span replacement options)])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (define (edit-with-snapshot! actor id basis span replacement . options)
    ;; The same transaction with an atomic acknowledgement:
    ;; (revision text changes), ending at this edit, before subscribers
    ;; can write again.  Changes include the complete chain from basis
    ;; through the accepted edit, even if committing trims its oldest
    ;; entry out of the retained log.  A head uses this to place its
    ;; command's anchors without guessing where the edit actually landed.
    (let ([context (and (pair? options) (car options))])
      (unless (<= (length options) 1) (error 'edit! "expected one context" options))
      (validate-edit-context context)
      (let ([outcome
             (transact!
               (lambda ()
                 (let* ([b (buffer-of 'edit! id)]
                        [since (entries-since b basis)]
                        [rebased (and since
                                   (rebase-through
                                     (text:normalize-span span)
                                     (map (lambda (entry) (vector-ref entry 2)) since)))])
                   (cond
                     [(not since) (list 'stale 'basis-too-old)]
                     [(not rebased) (list 'stale 'overlap)]
                     [else
                      (let-values ([(new-revision delta)
                                    (apply-locked! b id actor rebased
                                                   replacement #f
                                                   (if (and context (>= (length context) 3))
                                                       (caddr context) '())
                                                   (if (and context (= (length context) 4))
                                                       (cadddr context) '()))])
                        (remember-edit! b actor context)
                        (list 'applied new-revision (buffer-text b)
                              (append (map change-data since)
                                      (list (list new-revision actor delta)))))]))))])
        (case (car outcome)
          [(applied)
           (values 'applied (cdr outcome))]
          [else (values 'stale (cadr outcome))]))))

  (define (same-delta? a b)
    (and (equal? (text:span-start (text:delta-span a))
                 (text:span-start (text:delta-span b)))
         (equal? (text:span-end (text:delta-span a))
                 (text:span-end (text:delta-span b)))
         (equal? (text:delta-new-end a) (text:delta-new-end b))
         (equal? (text:delta-removed a) (text:delta-removed b))
         (equal? (text:delta-inserted a) (text:delta-inserted b))))

  (define (cancel-compensated entries next)
    ;; Keep a chain with the same net effect.  If next reverses an
    ;; operation in that chain, commute the inverse backwards through
    ;; the intervening disjoint edits, then remove the cancelling pair.
    ;; Their actual log entries and authorship are never removed.
    (let ([origin (vector-ref next 3)])
      (if (not origin)
          (append entries (list next))
          (let find-target ([remaining entries] [prefix '()])
            (cond
              [(null? remaining) (append entries (list next))]
              [(= (vector-ref (car remaining) 0) (list-ref origin 3))
               (let commute ([remaining (cdr remaining)]
                             [inverse (text:invert-delta (vector-ref (car remaining) 2))]
                             [shifted '()])
                 (if (null? remaining)
                     (and (same-delta? inverse (vector-ref next 2))
                          (append (reverse prefix) (reverse shifted)))
                     (let* ([entry (car remaining)]
                            [delta (vector-ref entry 2)]
                            [after (text:rebase-delta inverse delta)]
                            [before (text:rebase-delta delta inverse 'stay)])
                       (and after before
                            (commute (cdr remaining) after
                              (cons (vector (vector-ref entry 0) (vector-ref entry 1)
                                            before (vector-ref entry 3) (vector-ref entry 4))
                                    shifted))))))]
              [else (find-target (cdr remaining) (cons (car remaining) prefix))])))))

  (define (effective-chain entries)
    (fold-left (lambda (chain entry)
                 (and chain (cancel-compensated chain entry)))
               '() entries))

  (define (prepare-history b actor group direction)
    ;; Preflight every inverse against temporary immutable text.  A
    ;; later refusal leaves the whole action, its redo state, and the
    ;; live buffer untouched.  Simulated entries remain available for
    ;; the rest of this transaction even if committing will trim them.
    (if (not (undo-group-complete? group))
        (values #f 'basis-too-old)
        (let plan ([parts (undo-group-parts group)]
                   [text (buffer-text b)]
                   [properties (buffer-properties b)]
                   [revision (buffer-revision b)]
                   [deltas (buffer-deltas b)]
                   [planned '()])
          (if (null? parts)
              (values (reverse planned) #f)
              (let* ([part (car parts)]
                     [facts (vector-ref part 4)]
                     [since (chain-since deltas revision (vector-ref part 0))]
                     [chain (and since (effective-chain since))]
                     [inverse
                      (and chain
                           (fold-left
                             (lambda (inverse entry)
                               (and inverse (text:rebase-delta inverse (vector-ref entry 2))))
                             (text:invert-delta (vector-ref part 2)) chain))])
                (cond
                  [(not since) (values #f 'basis-too-old)]
                  [(not (for-all (lambda (change)
                                   (eq? (property-cell properties (car change)) (caddr change)))
                                 facts))
                   (values #f 'property-changed)]
                  [(or (not inverse)
                       (not (equal? (text:delta-removed inverse)
                                    (text:extract text (text:delta-span inverse)))))
                   (values #f 'overlap)]
                  [else
                   (let*-values ([(new-text delta)
                                  (text:apply-edit text (text:delta-span inverse)
                                                   (text:delta-inserted inverse))]
                                 [(origin)
                                  (list direction (undo-group-actor group)
                                        (undo-group-id group) (vector-ref part 0))]
                                 [(inverse-facts)
                                  (map (lambda (change) (list (car change) (caddr change) (cadr change))) facts)]
                                 [(entry) (vector (+ revision 1) actor delta origin inverse-facts)])
                     (plan (cdr parts) new-text (apply-property-changes properties inverse-facts) (+ revision 1)
                           (cons entry deltas)
                           (cons (list new-text delta origin inverse-facts) planned)))]))))))

  (define (undo-scope? scope)
    (or (memq scope '(mine all))
        (and (list? scope) (= (length scope) 2) (eq? (car scope) 'actor))))

  (define (scope-matches? scope actor group)
    (or (eq? scope 'all)
        (equal? (undo-group-actor group)
                (if (eq? scope 'mine) actor (cadr scope)))))

  (define (history-step! actor id direction scope)
    ;; The common history transaction.  Undo selects mine, all, or
    ;; (actor who).  Redo always reverses this requester's latest undo,
    ;; independently of the original author (scope must be mine).
    ;; -> applied (revision action-id original-author group-key label)
    ;;  | blocked overlap|basis-too-old | nothing #f
    (unless (and (memq direction '(undo redo)) (undo-scope? scope)
                 (or (eq? direction 'undo) (eq? scope 'mine)))
      (error 'history-step! "invalid history direction or scope" direction scope))
    (transact!
      (lambda ()
        (let* ([b (buffer-of 'history-step! id)]
               [group
                (find (lambda (group)
                        (if (eq? direction 'undo)
                            (and (undo-group-live? group) (scope-matches? scope actor group))
                            (and (not (undo-group-live? group))
                                 (undo-group-redo-actor group)
                                 (equal? (car (undo-group-redo-actor group)) actor))))
                      (buffer-undo b))])
          (if (not group)
              (values 'nothing #f)
              (let-values ([(plan reason) (prepare-history b actor group direction)])
                (if (not plan)
                    (values 'blocked reason)
                    (let ([parts '()])
                      (for-each
                        (lambda (step)
                          (install-edit! b id actor (car step) (cadr step) (caddr step) (cadddr step) '())
                          (set! parts (cons (car (buffer-deltas b)) parts)))
                        plan)
                      (undo-group-parts-set! group parts)
                      (undo-group-live?-set! group (eq? direction 'redo))
                      (undo-group-redo-actor-set! group (and (eq? direction 'undo) (list actor)))
                      (buffer-undo-set! b (cons group (remq group (buffer-undo b))))
                      (values 'applied
                              (list (buffer-revision b) (undo-group-id group)
                                    (undo-group-actor group) (undo-group-key group)
                                    (undo-group-label group)))))))))))

  (define (undo! actor id . scope)
    ;; Compatibility result: the new revision, or a refusal reason.
    ;; The optional scope uses the same selector as history-step!.
    (unless (<= (length scope) 1) (error 'undo! "expected at most one scope" scope))
    (let-values ([(status detail)
                  (history-step! actor id 'undo (if (pair? scope) (car scope) 'mine))])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (define (redo! actor id)
    (let-values ([(status detail) (history-step! actor id 'redo 'mine)])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (define (undo-authors id)
    ;; Actors with retained live actions, newest first.  Selection is
    ;; advisory: the actual history transaction rechecks under the lock.
    (locked
      (lambda ()
        (let loop ([groups (buffer-undo (buffer-of 'undo-authors id))] [authors '()])
          (cond
            [(null? groups) (reverse authors)]
            [(and (undo-group-live? (car groups))
                  (not (member (undo-group-actor (car groups)) authors)))
             (loop (cdr groups) (cons (undo-group-actor (car groups)) authors))]
            [else (loop (cdr groups) authors)])))))

  (define (history id . count)
    ;; Attribution: the newest applied edits, as plain data --
    ;; ((revision actor start end new-end) ...) newest first, bounded
    ;; by the delta log.  An inverse row appends its origin:
    ;; (direction original-author action-id reversed-revision).
    ;; Resets clear it: a reset is a new baseline.
    (let ([n (if (pair? count) (car count) 20)])
      (locked
        (lambda ()
          (let take ([entries (buffer-deltas (buffer-of 'history id))]
                     [n n])
            (if (or (zero? n) (null? entries))
                '()
                (let* ([entry (car entries)]
                       [d (vector-ref entry 2)]
                       [s (text:delta-span d)])
                  (cons (append (list (vector-ref entry 0)
                                      (vector-ref entry 1)
                                      (text:span-start s)
                                      (text:span-end s)
                                      (text:delta-new-end d))
                                (if (vector-ref entry 3) (list (vector-ref entry 3)) '()))
                        (take (cdr entries) (- n 1))))))))))

  (define (blame id . count)
    ;; Attribution with geometry: the newest applied edits with their
    ;; written spans rebased into the CURRENT text, as plain data --
    ;; ((span actor revision) ...) newest first.  A span a later edit
    ;; swallowed degrades to endpoint rebasing, like span marks:
    ;; attribution survives races, it never goes stale.  Reach: the
    ;; delta log (delta-log-limit edits); a reset clears it.  Blame is
    ;; recent-memory attribution -- deep history stays git's job.
    (let ([n (if (pair? count) (car count) 20)])
      (locked
        (lambda ()
          (let walk ([entries (buffer-deltas (buffer-of 'blame id))]
                     [later '()]      ; newer deltas, oldest first
                     [n n]
                     [acc '()])
            (if (or (zero? n) (null? entries))
                (reverse acc)
                (let* ([entry (car entries)]
                       [d (vector-ref entry 2)]
                       [written
                        (let ([s (text:span-start (text:delta-span d))]
                              [e (text:delta-new-end d)])
                          (text:make-span (car s) (cdr s)
                                          (car e) (cdr e)))]
                       [current (fold-left rebase-mark-value
                                           written later)])
                  (walk (cdr entries)
                        (cons d later)
                        (- n 1)
                        (cons (list current
                                    (vector-ref entry 1)
                                    (vector-ref entry 0))
                              acc)))))))))

  ;;; Marks -------------------------------------------------------------------

  ;; A mark is an actor-owned named value: a position (row . col),
  ;; rebased across every edit with the default forward bias -- a
  ;; cursor at an insertion point is pushed along with the text -- or
  ;; a (text) span, for published selections and other regions.  A
  ;; span mark rebases strictly while the edit misses it; an
  ;; overlapping edit degrades it to endpoint rebasing rather than
  ;; dropping it -- a selection should survive a race, never go stale.

  (define (mark-key actor mark-name) (cons actor mark-name))

  (define (rebase-mark-value value d)
    (if (text:span? value)
        (or (text:rebase-span value d)
            (let ([start (text:rebase-position (text:span-start value) d)]
                  [end (text:rebase-position (text:span-end value) d
                                             'stay)])
              (text:make-span (car start) (cdr start)
                              (car end) (cdr end))))
        (text:rebase-position value d)))

  (define (clamp-mark-value value clamp)
    (if (text:span? value)
        (let ([start (clamp (text:span-start value))]
              [end (clamp (text:span-end value))])
          (text:make-span (car start) (cdr start) (car end) (cdr end)))
        (clamp value)))

  (define (set-mark! actor id mark-name position)
    (locked
      (lambda ()
        (let* ([b (buffer-of 'set-mark! id)]
               [key (mark-key actor mark-name)]
               [kept (remp (lambda (entry) (equal? (car entry) key))
                           (buffer-marks b))])
          (buffer-marks-set! b (cons (cons key position) kept)))))
    (void))

  (define (mark actor id mark-name)
    ;; the mark's current position, or #f
    (locked
      (lambda ()
        (cond [(assoc (mark-key actor mark-name)
                      (buffer-marks (buffer-of 'mark id)))
               => cdr]
              [else #f]))))

  (define (drop-mark! actor id mark-name)
    (locked
      (lambda ()
        (let ([b (buffer-of 'drop-mark! id)]
              [key (mark-key actor mark-name)])
          (buffer-marks-set!
            b (remp (lambda (entry) (equal? (car entry) key))
                    (buffer-marks b))))))
    (void))

  (define (marks actor id)
    ;; the actor's marks in the buffer: ((name . position) ...)
    (locked
      (lambda ()
        (fold-right (lambda (entry acc)
                      (if (equal? (caar entry) actor)
                          (cons (cons (cdar entry) (cdr entry)) acc)
                          acc))
                    '()
                    (buffer-marks (buffer-of 'marks id))))))

  ;;; Properties ----------------------------------------------------------------

  ;; Buffer-level facts shared by every head -- the visited file, the
  ;; mode's name, the disk base, read-only -- live on the store buffer
  ;; as plain-data properties, so a second head (or a remote one)
  ;; reads the same truth the first one wrote.  Per-seat state --
  ;; cursors, selections, viewports -- stays with heads and their
  ;; marks.  Values are data only -- #f included: a fact may be
  ;; explicitly off -- and an absent property uses its declared fallback.
  ;; Modified is derived from text/trailing/base and cannot be written.
  ;; Its causing edit/reset/property event already notifies observers;
  ;; no redundant modified-property event is needed.  Other properties
  ;; survive resets and renames unless explicitly updated, and die with
  ;; delete!.  Subscribers hear (property id key actor).

  (define (set-property! actor id key value)
    (set-properties! actor id (list (cons key value))))

  (define (set-properties! actor id updates)
    (writable-properties updates)
    (transact!
      (lambda ()
        (let ([b (buffer-of 'set-properties! id)])
          (install-properties! b updates)
          (refresh-modified! b)
          (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates))))
    (void))

  (define (drop-property! actor id key)
    (unless (symbol? key)
      (error 'drop-property! "expected a symbol key" key))
    (when (eq? key 'modified) (error 'drop-property! "modified is derived"))
    (transact!
      (lambda ()
        (let ([b (buffer-of 'drop-property! id)])
          (buffer-properties-set!
            b (replace-property-cell (buffer-properties b) (cons key missing-property)))
          (refresh-modified! b)
          (enqueue-event! `(property ,id ,key ,actor)))))
    (void))

  (define (property id key . fallback)
    ;; Absence uses the fallback (#f by default); an explicit #f stays #f.
    (unless (<= (length fallback) 1) (error 'property "expected one fallback" fallback))
    (locked
      (lambda ()
        (let ([b (buffer-of 'property id)])
          (if (eq? key 'modified) (buffer-modified b)
              (property-value b key (and (pair? fallback) (car fallback))))))))

  (define (properties id)
    ;; every fact, as fresh pairs: ((key . value) ...)
    (locked
      (lambda ()
        (property-data (buffer-of 'properties id)))))

  ;;; Subscriptions ------------------------------------------------------------

  ;; Subscribers hear changes as data: (edit id revision actor delta),
  ;; (reset id revision actor), (property id key actor), and the
  ;; buffer lifecycle -- (create id name actor), (rename id name
  ;; actor), (delete id actor).  Commits enqueue events under the store
  ;; lock.  One writer drains them outside it, in commit order, finishing
  ;; an event's subscribers before starting the next event.  Callbacks
  ;; may read or mutate the store.  While a drainer is active, concurrent
  ;; and reentrant writers return after committing; their events follow
  ;; later.  A callback must not wait for delivery of a later event.
  ;;
  ;; Subscriptions are kernel-registry entries, so they are owned like
  ;; any registration: a module's subscription retracts when the
  ;; module reloads, before its init! subscribes afresh.  The token is
  ;; the explicit revocation handle for everything else.  Entries are
  ;; (token buffer-id proc).  A commit captures its interested tokens;
  ;; delivery resolves each live registration just before calling it.
  ;; New subscribers never hear earlier commits, and revocation skips
  ;; queued callbacks (an already running callback may still finish).

  (define subscriptions (kernel:make-registry))

  (define subscription-counter
    (kernel:persistent-cell 'store-subscription-counter (lambda () 0)))

  (define (subscribe! id proc)
    ;; -> a token for unsubscribe!; id #f hears every buffer
    (unless (procedure? proc)
      (error 'subscribe! "expected a procedure" proc))
    (locked
      (lambda ()
        (let ([token (+ (unbox subscription-counter) 1)])
          (set-box! subscription-counter token)
          (kernel:registry-add! subscriptions (list token id proc))
          token))))

  (define (unsubscribe! token)
    (locked
      (lambda ()
        (kernel:registry-remove! subscriptions
                                 (lambda (entry) (equal? (car entry) token)))))
    (void))

  (define (enqueue-event! event)
    ;; Caller holds the mutation lock.  The two-list FIFO keeps both
    ;; appends and removal amortized constant time.
    (let ([tokens
           (map car
                (filter (lambda (entry)
                          (or (not (cadr entry))
                              (equal? (cadr entry) (cadr event))))
                        (kernel:registry-items subscriptions)))]
          [s (current-store)])
      (unless (null? tokens)
        (store-events-back-set! s
          (cons (cons event tokens) (store-events-back s))))))

  (define (next-delivery!)
    (locked
      (lambda ()
        (let ([s (current-store)])
          (let next ()
            (when (null? (store-events-front s))
              (store-events-front-set! s (reverse (store-events-back s)))
              (store-events-back-set! s '()))
            (and (pair? (store-events-front s))
                 (let* ([front (store-events-front s)]
                        [item (car front)])
                   (if (null? (cdr item))
                       (begin (store-events-front-set! s (cdr front)) (next))
                       (let ([subscriber
                              (kernel:registry-find subscriptions
                                (lambda (entry) (= (car entry) (cadr item))))])
                         ;; Consume one callback before invoking it.  If
                         ;; it escapes, the rest of this event is intact.
                         (store-events-front-set! s
                           (cons (cons (car item) (cddr item)) (cdr front)))
                         (if subscriber
                             (cons (car item) (caddr subscriber))
                             (next)))))))))))

  (define (drain-events!)
    (when (locked
            (lambda ()
              (let ([s (current-store)])
                (and (not (store-delivering? s))
                     (or (pair? (store-events-front s))
                         (pair? (store-events-back s)))
                     (begin (store-delivering?-set! s #t) #t)))))
      (let ([entered? #f])
        (dynamic-wind
          (lambda ()
            ;; A captured callback may escape, but resuming a completed
            ;; drain would bypass ownership and race a newer drainer.
            (when entered? (error 'store "cannot resume completed event delivery"))
            (set! entered? #t))
          (lambda ()
            (let drain ()
              (let ([delivery (next-delivery!)])
                (when delivery
                  (guard (ex [else (void)]) ((cdr delivery) (car delivery)))
                  (drain)))))
          (lambda ()
            (locked (lambda () (store-delivering?-set! (current-store) #f)))
            ;; Finish queued work on an escape too, and cover a commit
            ;; racing the empty-queue read before releasing the drainer.
            (drain-events!)))))))
