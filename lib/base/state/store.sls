;; store.sls -- the buffer store: the library (store).
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

(import (only (foundation edoc) elibrary))
(elibrary (state store)
  (export backups-kept blame buffer-list buffer-name close! conflict-state conflicts create! delete! discard! drop-mark! drop-property!
          edit! edit-with-snapshot! exists? expire-trash! export extract find-file find-named
          history history-step! import! line line-count (rename (log-entries log)) log-retention
          mark marks properties property publication publish! redo! reload! rename! reread! reset! resolve! resolve-picks! revision
          rewrite! set-mark! set-marks! set-properties! set-property! snapshot snapshot-since
          snapshot-state state subscribe! trash-retention undo! undo-authors undo-labels unsubscribe!
          valid-import? validate-edit-context validate-properties view visible? visit! watch!)
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! set-cdr! make-mutex with-mutex format void remq
                current-time time-second time-nanosecond list-head make-parameter parameterize make-weak-eq-hashtable hashtable-values)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (foundation datum) datum:)
          (prefix (foundation diff) diff:)
          (prefix (foundation text) text:)
          (prefix (state actor) actor:)
          (prefix (sys activity) activity:))

  ;;; The store -------------------------------------------------------------

  ;; The delta log's storage bound, a parameter since the journal saves
  ;; the log with the session; history and blame scan their own counts.
  (edoc "How many delta log entries a buffer retains, and with them how far undo, blame and views reach, or set it; 4096 by default."
        (count integer "the retention in entries")
        (returns integer))
  (define log-retention
    (make-parameter 4096
      (lambda (count)
        (unless (and (integer? count) (exact? count) (>= count 1))
          (error 'log-retention "expected a positive count of entries" count))
        count)))

  ;; A delta log entry: the revision the edit produced, its actor, the
  ;; delta, the origin of an inverse -- (direction author action-id
  ;; reversed-revision) -- or #f, the undo facts it carried, and its
  ;; labels, (batch . id) among them. While conflicts pend, complete Before
  ;; and After images share this bounded journal: even an edit outside a
  ;; region can become part of its Mine image after a later composition.
  (define (make-entry revision actor delta origin facts labels . conflicts)
    (vector revision actor delta origin facts labels (if (pair? conflicts) (car conflicts) #f)))
  (define (entry-revision entry) (vector-ref entry 0))
  (define (entry-actor entry) (vector-ref entry 1))
  (define (entry-delta entry) (vector-ref entry 2))
  (define (entry-origin entry) (vector-ref entry 3))
  (define (entry-facts entry) (vector-ref entry 4))
  (define (entry-labels entry) (vector-ref entry 5))
  (define (entry-conflicts entry) (vector-ref entry 6))

  (define (remq* removed items) (remp (lambda (item) (memq item removed)) items))

  (define-record-type (buffer make-buffer buffer?)
    (fields (mutable label)
            (mutable text)       ; immutable line vector, per (text)
            (mutable revision)
            (mutable deltas)     ; entries, newest first: see make-entry
            (mutable marks)      ; (((actor . name) . position) ...)
            (mutable undo)       ; undo groups, most recent operation first
            (mutable properties) ; ((key . datum) ...), see set-property!
            (mutable baseline)   ; cached (base-cell lines trailing?), or #f
            (mutable modified)   ; derived from text/trailing and the baseline
            (mutable modified-at) ; UTC nanoseconds of the last content change
            (mutable conflicts) ; pending reload conflicts, records newest first: see make-conflict
            (mutable reload-log))) ; (base revision projected-entries), or #f; merge coordinates, never undo history

  (define-record-type undo-group
    (fields id actor key
            (mutable label)
            (mutable parts)      ; last delta of each member, including disabled members
            (mutable redo-actor) ; (requester), or #f when redo was invalidated
            (mutable complete?)))

  (define-record-type (store make-store store?)
    (fields lock
            (mutable buffers)    ; id -> buffer; replaced once during restore
            (mutable next-id)
            (mutable closing?)
            deliveries))         ; ordered callbacks, shared kernel mechanism

  (define the-store
    (kernel:persistent-cell 'store
      (lambda ()
        (make-store (make-mutex)
                    (make-eqv-hashtable)
                    1 #f (kernel:make-delivery-queue)))))

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

  (define (refresh-edit-facts! b changed?)
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
                      (string=? (vector-ref text 0) ""))))))
    (when (or changed? (and (buffer-modified b) (not (buffer-modified-at b))))
      (let ([now (current-time 'time-utc)])
        (buffer-modified-at-set! b (+ (* (time-second now) 1000000000) (time-nanosecond now))))))

  (define (edit-facts b)
    (list (cons 'modified (buffer-modified b))
          (cons 'modified-at (buffer-modified-at b))
          (cons 'conflicts (length (unsettled-conflicts b)))))

  (define validate-properties property:validate)
  (define writable-properties property:writable)
  (define validate-edit-context property:edit-context)

  (define (install-properties! b updates)
    (buffer-properties-set! b
      (fold-left (lambda (facts entry) (replace-property-cell facts (cons (car entry) (cdr entry))))
                 (buffer-properties b) updates)))

  (define (current-properties b)
    (append (edit-facts b)
            (filter (lambda (cell) (not (eq? (cdr cell) missing-property)))
                    (buffer-properties b))))

  (define (property-data b) (map datum:copy (current-properties b)))

  (define (locked thunk)
    (with-mutex (store-lock (current-store)) (thunk)))

  (define (ensure-open!)
    (when (store-closing? (current-store))
      (raise (condition (kernel:make-refusal)
                        (make-who-condition 'store)
                        (make-message-condition "Store is closing")))))

  (define (own-actor actor)
    (unless (actor:identity? actor) (error 'store "expected actor (kind name ...)" actor))
    (datum:copy actor))

  (define (transact! actor thunk)
    ;; Mutation and its event enter the same critical section.  Only
    ;; after releasing it may this writer become the event drainer.
    (activity:call-with
      (lambda ()
        (let ([actor (own-actor actor)])
          (call-with-values
            (lambda () (locked (lambda () (ensure-open!) (thunk actor))))
            (lambda result
              (kernel:drain-deliveries! (store-deliveries (current-store)))
              (apply values result)))))))

  (define (buffer-of who id)
    (or (hashtable-ref (store-buffers (current-store)) id #f)
        (error who (format "no buffer ~a" id))))

  (define (copy-buffer b)
    ;; Text, entries and property cells are immutable. Planning owns every
    ;; mutable record, including history groups and settled conflicts.
    (make-buffer (buffer-label b) (buffer-text b) (buffer-revision b)
      (buffer-deltas b) (buffer-marks b)
      (map (lambda (g)
             (make-undo-group (undo-group-id g) (undo-group-actor g) (undo-group-key g)
               (undo-group-label g) (undo-group-parts g)
               (undo-group-redo-actor g) (undo-group-complete? g)))
           (buffer-undo b))
      (buffer-properties b) (buffer-baseline b) (buffer-modified b) (buffer-modified-at b)
      (map (lambda (c)
             (make-conflict (conflict-revision c) (conflict-actor c) (conflict-labels c)
               (conflict-span c) (conflict-mine c) (conflict-disk c)
               (conflict-settled c) (conflict-unsettled-by c)))
           (buffer-conflicts b))
      (buffer-reload-log b)))

  (define planned-events (make-parameter #f))

  (define (plan-buffer! id proc)
    ;; A reload may need several inversions before it can prove the merge.
    ;; Publish the buffer and notifications only after the entire plan succeeds.
    (let* ([old (buffer-of 'reload! id)] [b (copy-buffer old)] [events (box '())]
           [limit (log-retention)])
      (hashtable-set! bridges b (hashtable-ref bridges old '()))
      (call-with-values
        (lambda ()
          (parameterize ([planned-events events]
                         [log-retention (+ limit (* 2 (length (buffer-deltas b))) 1)])
            (proc b)))
        (lambda results
          (unless (and (pair? results) (eq? (car results) 'refused))
            (buffer-deltas-set! b (bounded (buffer-deltas b) limit))
            (buffer-undo-set! b (bounded (buffer-undo b) limit))
            (for-each (lambda (group)
                        (when (> (length (undo-group-parts group)) limit)
                          (undo-group-complete?-set! group #f)
                          (undo-group-parts-set! group (bounded (undo-group-parts group) limit))))
              (buffer-undo b))
            (when (buffer-reload-log b)
              (let ([projection (buffer-reload-log b)])
                (buffer-reload-log-set! b (list (car projection) (cadr projection) (bounded (caddr projection) limit)))))
            (buffer-conflicts-set! b (retained-conflicts b))
            (hashtable-set! (store-buffers (current-store)) id b)
            (for-each enqueue-event! (reverse (unbox events))))
          (apply values results)))))

  (define (own-write-access access)
    ;; #f is a trusted producer update. Clients pass 'any or their allowed
    ;; buffer names; both respect read-only. Only data crosses this boundary,
    ;; never a policy callback under the store lock.
    (unless (or (not access) (eq? access 'any)
                (and (list? access) (for-all string? access)))
      (error 'store "expected #f, any, or allowed buffer names" access))
    (datum:copy access))

  (define (write-refusal id access)
    ;; Caller holds the mutation lock. A rename or read-only toggle cannot
    ;; land between permission checking and the edit/history transaction.
    (and access
         (let ([b (hashtable-ref (store-buffers (current-store)) id #f)])
           (cond [(not (or (eq? access 'any)
                           (and b (member (buffer-label b) access)))) 'buffer]
                 [(and b (property-value b 'read-only #f)) 'read-only]
                 [else #f]))))

  ;;; Lifecycle and reading --------------------------------------------------

  (define (own-name name)
    (unless (and (string? name) (> (string-length name) 0))
      (error 'store "expected a nonempty buffer name" name))
    (string-copy name))

  (define (unique-name base self . all?)
    ;; Caller holds the store lock. Live buffers share this namespace;
    ;; trashed ones release their names until they are restored, unless
    ;; all? asks for every buffer's. Deletion releases a name and renaming
    ;; does not compete with itself.
    (let ([used (make-hashtable string-hash string=?)] [every? (and (pair? all?) (car all?))])
      (vector-for-each
        (lambda (id)
          (let ([b (buffer-of 'unique-name id)])
            (unless (or (eqv? id self) (and (not every?) (property-value b 'trashed #f)))
              (hashtable-set! used (buffer-label b) #t))))
        (hashtable-keys (store-buffers (current-store))))
      (let next ([name base] [suffix 2])
        (if (hashtable-ref used name #f)
            (next (format "~a<~a>" base suffix) (+ suffix 1))
            name))))

  (define (evict-trashed-holder! actor name self)
    ;; A trashed buffer holding the name a live buffer just took moves to a
    ;; suffixed one: names stay unique across the store, as the session
    ;; file requires, and the live buffer keeps the plain one.
    (vector-for-each
      (lambda (id)
        (let ([b (buffer-of 'unique-name id)])
          (when (and (not (eqv? id self)) (property-value b 'trashed #f) (string=? (buffer-label b) name))
            (let ([moved (unique-name name id #t)])
              (buffer-label-set! b moved)
              (enqueue-event! `(rename ,id ,moved ,actor))))))
      (hashtable-keys (store-buffers (current-store)))))

  (edoc "Create a buffer with a name, lines and optional facts, publishing them together; its id."
        (actor actor "the actor identity")
        (buffer-name string "the name")
        (lines (or list vector) "the lines; empty means one empty line")
        (facts (list-of list) "a fact batch, at most one")
        (returns integer))
  (define (create! actor buffer-name lines . facts)
    ;; -> the new buffer's id.  Empty lines mean one empty line.
    ;; Initial facts and content publish together, before the create event.
    ;; Private content is never briefly visible to every head.
    (unless (<= (length facts) 1) (error 'create! "expected one fact batch" facts))
    (let ([name (own-name buffer-name)]
          [text (text:normalize lines)]
          [updates (datum:copy (writable-properties (if (pair? facts) (car facts) '())))])
      (transact! actor
        (lambda (actor)
          (create-buffer! actor name text updates)))))

  (define (create-buffer! actor name text updates)
    ;; Caller holds the store lock; creation and publication use one path.
    ;; A buffer born in the trash, a backup, takes a name no buffer holds,
    ;; trashed ones included: the trash's names stay distinct, as the
    ;; session file requires, and a backup's file keeps backups-kept versions.
    (let* ([s (current-store)]
           [hidden? (cond [(assq 'trashed updates) => (lambda (entry) (and (cdr entry) #t))] [else #f])]
           [name (unique-name name #f hidden?)] [id (store-next-id s)])
      (store-next-id-set! s (+ id 1))
      (let ([b (make-buffer name text 0 '() '() '() '() #f #f #f '() #f)])
        (install-properties! b updates)
        (refresh-edit-facts! b #f)
        (hashtable-set! (store-buffers s) id b))
      (unless hidden? (evict-trashed-holder! actor name id))
      (enqueue-event! `(create ,id ,name ,actor))
      (let ([backup (assq 'backup updates)])
        (when (and backup (cdr backup)) (prune-backups! actor (cadr backup) id)))
      id))

  (define (file-id path)
    ;; Caller holds the store lock. Paths are canonicalized before admission;
    ;; identity lives in the file fact, so retarget/delete need no index upkeep.
    (unless (and (string? path) (> (string-length path) 0))
      (error 'find-file "expected a nonempty canonical file path" path))
    ;; A trashed document no longer visits its file: a fresh visit starts
    ;; from disk, and restore! brings the trashed one back beside it.
    (find (lambda (id)
            (let ([b (buffer-of 'find-file id)])
              (and (not (property-value b 'trashed #f))
                   (equal? (property-value b 'file #f) path))))
          (vector->list (hashtable-keys (store-buffers (current-store))))))

  (edoc "The id of the buffer visiting a file, or #f."
        (path file "the file")
        (returns (or integer #f)))
  (define (find-file path)
    (locked (lambda () (file-id path))))

  (edoc "Visit a file as a buffer, concurrent visitors sharing the first: (values id created?)."
        (actor actor "the actor identity")
        (name string "the name")
        (lines (or list vector) "the lines read")
        (facts list "the file facts"))
  (define (visit! actor name lines facts)
    ;; -> (values id created?): concurrent visitors share the first publication.
    ;; Reuse never changes its name/text/facts/history or emits another event.
    (let ([name (own-name name)] [text (text:normalize lines)]
          [updates (datum:copy (writable-properties facts))])
      (transact! actor
        (lambda (actor)
          (let ([id (file-id (cond [(assq 'file updates) => cdr] [else #f]))])
            (if id (values id #f)
                (values (create-buffer! actor name text updates) #t)))))))

  (edoc "Replace a buffer's baseline wholesale, clearing its history, optionally with facts and a reviewed state that must still match; the new revision, or #f."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (lines (or list vector) "the lines")
        (options (list-of any) "facts, then a reviewed (revision fact ...) state")
        (returns (or integer #f)))
  (define (reset! actor id lines . options)
    ;; Wholesale replacement: a new baseline, not an edit.  The delta
    ;; log and the undo history clear (a stale basis against a reset
    ;; refuses as basis-too-old), and marks clamp into the new text.
    ;; Related baseline facts may join the same transaction.  Loading
    ;; a file supplies base/trailing/stamp; ordinary edits use edit!.
    ;; Optional (revision fact ...) review data must match the whole state
    ;; under this same writer. A changed/deleted source returns #f, untouched.
    (unless (<= (length options) 2) (error 'reset! "expected facts and optional reviewed state" options))
    (let ([text (text:normalize lines)]
          [updates (datum:copy (writable-properties (if (pair? options) (car options) '())))]
          [review (datum:copy (and (= (length options) 2) (cadr options)))])
      (transact! actor
        (lambda (actor)
          (and (or (not review)
                   (let ([b (hashtable-ref (store-buffers (current-store)) id #f)])
                     (and b (reviewed-state? b review))))
               (reset-buffer! actor id text updates))))))

  (define (reset-buffer! actor id text updates)
    ;; The log and the undo history clear, but readers keep their way
    ;; across: the cleared entries survive as one-step bridges, and the
    ;; reset itself is bridged by a line diff of the two texts, which also
    ;; carries the marks; a fresh buffer's lone empty line has nothing to carry
    (let* ([b (buffer-of 'reset! id)]
           [old (buffer-text b)] [trailing? (property-value b 'trailing #t)]
           [from (buffer-revision b)] [cleared (buffer-deltas b)]
           [steps (if (and (= (vector-length old) 1) (string=? (vector-ref old 0) "")) '()
                      (edit-deltas old (edits-between old text)))]
           [clamp (lambda (position)
                    (let* ([line (min (car position) (- (vector-length text) 1))]
                           [column (min (cdr position) (string-length (vector-ref text line)))])
                      (cons line column)))])
      (buffer-text-set! b text)
      (install-properties! b updates)
      (refresh-edit-facts! b
        (or (not (equal? old text)) (not (eq? trailing? (property-value b 'trailing #t)))))
      (buffer-revision-set! b (+ (buffer-revision b) 1))
      (buffer-deltas-set! b '())
      (buffer-undo-set! b '())
      (buffer-reload-log-set! b #f)
      (for-each (lambda (entry) (remember-bridge! b (- (entry-revision entry) 1) (entry-revision entry) (entry-actor entry) (list (entry-delta entry))))
                (reverse cleared))
      (remember-bridge! b from (buffer-revision b) actor steps)
      (let ([pending? (pair? (unsettled-conflicts b))])
        (buffer-conflicts-set! b '())
        (buffer-marks-set!
          b (map (lambda (entry) (cons (car entry) (clamp-mark-value (fold-left rebase-mark-value (cdr entry) steps) clamp)))
                 (buffer-marks b)))
        (enqueue-event! `(reset ,id ,(buffer-revision b) ,actor))
        (when pending? (note-conflicts! id actor)))
      (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates)
      (buffer-revision b)))

  (define (publication-id identity)
    (find (lambda (id) (equal? (property-value (buffer-of 'publication id) 'publication #f) identity))
          (vector->list (hashtable-keys (store-buffers (current-store))))))

  (edoc "The id of a producer's generated source under a key, or #f."
        (actor actor "the actor identity")
        (key datum "the publication key")
        (returns (or integer #f)))
  (define (publication actor key)
    ;; One generated source per producer/key, independent of its label.
    ;; Identity lives with the buffer, so deletion needs no second registry.
    (let ([identity (list (own-actor actor) (datum:copy key))])
      (locked (lambda () (publication-id identity)))))

  (edoc "Create or replace a producer's generated source atomically; an (id revision fact ...) basis refuses stale refreshes. The id, or #f when refused."
        (actor actor "the actor identity")
        (key datum "the publication key")
        (name string "the buffer name")
        (lines (or list vector) "the lines")
        (facts list "the facts")
        (basis (list-of any) "the expected state, at most one")
        (returns (or integer #f)))
  (define (publish! actor key name lines facts . basis)
    ;; Atomically create or replace a producer's source, returning its id.
    ;; An optional (id revision fact ...) refuses stale refreshes, including
    ;; changed fact preconditions; #f requires absence. Refusal returns #f.
    ;; Identical text/facts emit nothing. Changed facts share the reset's
    ;; revision even when text is identical, so query changes invalidate it.
    (unless (and (<= (length basis) 1)
                 (or (null? basis) (not (car basis))
                     (let ([b (car basis)])
                       (and (list? b) (>= (length b) 2)
                            (integer? (car b)) (exact? (car b)) (> (car b) 0)
                            (integer? (cadr b)) (exact? (cadr b)) (>= (cadr b) 0)
                            (validate-properties (cddr b))))))
      (error 'publish! "expected an optional (id revision fact ...) or #f" basis))
    (let ([key (datum:copy key)] [name (own-name name)] [text (text:normalize lines)]
          [updates (datum:copy (writable-properties facts))] [basis (datum:copy basis)])
      (transact! actor
        (lambda (actor)
          (let* ([identity (list actor key)] [id (publication-id identity)]
                 [b (and id (buffer-of 'publish! id))])
            (cond
              [(and (pair? basis)
                    (not (if b
                             (and (car basis) (= id (caar basis))
                                  (= (buffer-revision b) (cadar basis))
                                  (null? (changed-properties b (cddar basis))))
                             (not (car basis))))) #f]
              [(not b)
               (create-buffer! actor name text (cons (cons 'publication identity) updates))]
              [else
               (let ([changed (changed-properties b updates)])
                 (unless (and (null? changed) (equal? text (buffer-text b)))
                   (reset-buffer! actor id text changed)))
               id]))))))

  (define (changed-properties b updates)
    (filter (lambda (entry)
              (not (equal? (property-value b (car entry) missing-property) (cdr entry))))
            updates))

  (edoc "Rename a buffer; the accepted name."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (new-name string "the wanted name")
        (returns string))
  (define (rename! actor id new-name)
    ;; -> the accepted name at this commit, before subscribers can rename
    ;; again. The returned string and each notification own their data.
    (let ([name (own-name new-name)])
      (transact! actor
        (lambda (actor) (rename-buffer! actor id name)))))

  (define (rename-buffer! actor id name)
    ;; The same name allocator serves renames and atomic fact/name batches.
    ;; A trashed buffer renamed competes with every name; a live one with
    ;; the live ones, and evicts a trashed holder of its new name.
    (let* ([b (buffer-of 'rename! id)]
           [trashed? (and (property-value b 'trashed #f) #t)]
           [name (unique-name name id trashed?)])
      (buffer-label-set! b name)
      (enqueue-event! `(rename ,id ,name ,actor))
      (unless trashed? (evict-trashed-holder! actor name id))
      (string-copy name)))

  (edoc "Delete a buffer."
        (actor actor "the actor identity")
        (id integer "the buffer id"))
  (define (delete! actor id)
    (transact! actor
      (lambda (actor)
        (buffer-of 'delete! id)
        (delete-buffer! actor id)))
    (void))

  (define (delete-buffer! actor id)
    (hashtable-delete! (store-buffers (current-store)) id)
    (enqueue-event! `(delete ,id ,actor)))

  (edoc "Delete a buffer only while its reviewed revision and facts still hold; whether it was deleted."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (revision integer "the reviewed revision")
        (facts list "the reviewed facts")
        (returns boolean))
  (define (discard! actor id revision facts)
    ;; The user's decision covers one reviewed text/fact snapshot. A later
    ;; edit or fact change needs a fresh review; an already deleted id is done.
    (let ([facts (datum:copy facts)])
      (transact! actor
        (lambda (actor)
          (let ([b (hashtable-ref (store-buffers (current-store)) id #f)])
            (or (not b)
                (and (reviewed-state? b (cons revision facts))
                     (begin (delete-buffer! actor id) #t))))))))

  (define (reviewed-state? b review)
    (equal? review (cons (buffer-revision b) (current-properties b))))

  (edoc "Mark the store closing, refusing further transactions.")
  (define (close!)
    (locked (lambda () (store-closing?-set! (current-store) #t))))

  ;;; Saved representation -------------------------------------------------

  ;; the facts a session keeps: a buffer's file and baseline, its mode and
  ;; its wrap setting, which every head shares and a restart must not reset,
  ;; a terminal's transcript least of all, whose rows are its columns wide
  (define persistent-keys '(file base stamp trailing mode read-only modified-at trashed backup wrap))
  (define (integer-at-least? n minimum) (and (integer? n) (exact? n) (>= n minimum)))

  (define (persistent-facts? facts)
    (and (list? facts)
         (let check ([rest facts] [seen '()])
           (or (null? rest)
               (let ([entry (car rest)])
                 (and (pair? entry) (memq (car entry) persistent-keys) (not (memq (car entry) seen))
                      (case (car entry)
                        [(file mode) (or (not (cdr entry)) (and (string? (cdr entry)) (> (string-length (cdr entry)) 0)))]
                        [(base) (or (not (cdr entry)) (string? (cdr entry)))]
                        [(trailing read-only) (boolean? (cdr entry))]
                        [(wrap) (let ([v (cdr entry)])
                                  (or (memq v '(default #t #f clean))
                                      (and (pair? v) (eq? (car v) 'clean) (integer-at-least? (cdr v) 20))))]
                        [(modified-at) (or (not (cdr entry)) (and (integer? (cdr entry)) (exact? (cdr entry))))]
                        [(trashed) (let ([v (cdr entry)])
                                     (or (not v) (and (list? v) (= (length v) 2) (integer? (car v)) (exact? (car v))
                                                      (actor:identity? (cadr v)))))]
                        [(backup) (property:backup-value? (cdr entry))]
                        [(stamp)
                         (let ([stamp (cdr entry)])
                           (or (not stamp)
                               (and (pair? stamp) (integer? (car stamp)) (exact? (car stamp))
                                    (integer-at-least? (cdr stamp) 0) (< (cdr stamp) 1000000000))))])
                      (check (cdr rest) (cons (car entry) seen))))))))

  ;; The journal: a buffer's delta log and undo groups as saved data, the
  ;; sixth element of its state -- (entries groups conflicts cells projection),
  ;; an entry (revision actor labels delta origin facts conflicts) newest first,
  ;; a group (id actor key
  ;; label part-revisions live? redo-actor complete?) most recent first, the
  ;; conflicts include settlement references. Property cells carry local
  ;; version ids, including the current persistent cells in the fourth list.
  ;; The optional merge projection is (base anchor-revision entries); its
  ;; entry IDs and property versions belong to the same chronological history.
  ;; A group key that cannot be written is saved as #f, since
  ;; new edits after a restart carry new keys anyway.
  (define (writable-datum? x)
    (cond [(pair? x) (and (writable-datum? (car x)) (writable-datum? (cdr x)))]
          [(vector? x) (for-all writable-datum? (vector->list x))]
          [else (or (null? x) (string? x) (symbol? x) (number? x) (boolean? x) (char? x))]))

  (define (cell->data cell)
    ;; a property cell as data, the tombstone spelled (missing-property)
    (cons (car cell) (if (eq? (cdr cell) missing-property) '(missing-property) (cdr cell))))

  (define (journal-data b)
    (define versions (make-eq-hashtable))
    (define reverted (reverted-revisions (buffer-deltas b)))
    (define (save-cell cell)
      (let ([id (or (hashtable-ref versions cell #f)
                    (let ([id (+ (hashtable-size versions) 1)])
                      (hashtable-set! versions cell id) id))])
        (vector id (cell->data cell))))
    (define (save-entry entry)
      (list (entry-revision entry) (entry-actor entry) (entry-labels entry)
            (text:delta->datum (entry-delta entry)) (entry-origin entry)
            (map (lambda (change) (list (car change) (save-cell (cadr change)) (save-cell (caddr change))))
                 (entry-facts entry))
            (entry-conflicts entry)))
    (list (map save-entry (buffer-deltas b))
          (map (lambda (group)
                 (list (undo-group-id group) (undo-group-actor group)
                       (if (writable-datum? (undo-group-key group)) (undo-group-key group) #f)
                       (undo-group-label group)
                       (map entry-revision (undo-group-parts group))
                       (pair? (action-parts b group 'undo reverted))
                       (undo-group-redo-actor group) (undo-group-complete? group)))
               (buffer-undo b))
          (map (lambda (c) (conflict-journal-data b c))
            (retained-conflicts b))
          (map save-cell (filter (lambda (cell) (memq (car cell) persistent-keys)) (buffer-properties b)))
          (let ([projection (buffer-reload-log b)])
            (and projection (list (car projection) (cadr projection) (map save-entry (caddr projection)))))))

  (define (valid-journal? journal revision)
    (define versions (make-eqv-hashtable))
    (define (cell-data? c)
      (if (vector? c)
          (and (= (vector-length c) 2) (integer-at-least? (vector-ref c 0) 1)
               (pair? (vector-ref c 1)) (symbol? (car (vector-ref c 1)))
               (let* ([id (vector-ref c 0)] [data (vector-ref c 1)] [old (hashtable-ref versions id #f)])
                 (if old (equal? old data) (begin (hashtable-set! versions id data) #t))))
          (and (pair? c) (symbol? (car c)))))
    (define (cell-key c) (car (if (vector? c) (vector-ref c 1) c)))
    (define (fact-data? f)
      (and (list? f) (= (length f) 3) (symbol? (car f)) (cell-data? (cadr f)) (cell-data? (caddr f))
           (eq? (car f) (cell-key (cadr f))) (eq? (car f) (cell-key (caddr f)))))
    (define (entries? entries revision)
      (and (list? entries)
        (let check ([entries entries] [below (+ revision 1)])
          (or (null? entries)
              (let ([e (car entries)])
                (and (list? e) (memv (length e) '(6 7))
                     (integer-at-least? (car e) 1) (< (car e) below)
                     (actor:identity? (cadr e))
                     (list? (caddr e)) (for-all (lambda (l) (and (pair? l) (symbol? (car l)))) (caddr e))
                     (guard (ex [else #f]) (text:datum->delta (cadddr e)) #t)
                     (let ([origin (list-ref e 4)]) (or (not origin) (and (list? origin) (= (length origin) 4))))
                     (list? (list-ref e 5)) (for-all fact-data? (list-ref e 5))
                     (or (= (length e) 6) (not (list-ref e 6))
                         (let ([change (list-ref e 6)])
                           (and (list? change) (= (length change) 2)
                                (for-all (lambda (side) (and (list? side) (for-all valid-conflict-data? side))) change))))
                     (check (cdr entries) (car e))))))))
    (and (list? journal) (memv (length journal) '(2 3 4 5)) (list? (cadr journal))
         (or (< (length journal) 4)
             (and (list? (cadddr journal))
                  (for-all (lambda (c) (and (vector? c) (cell-data? c) (memq (cell-key c) persistent-keys))) (cadddr journal))))
         (or (= (length journal) 2)
             (and (list? (caddr journal))
                  (for-all valid-conflict-data? (caddr journal))))
         (entries? (car journal) revision)
         (or (< (length journal) 5)
             (let ([p (list-ref journal 4)])
               (or (not p)
                   (and (list? p) (= (length p) 3) (string? (car p))
                        (integer-at-least? (cadr p) 0) (<= (cadr p) revision)
                        (entries? (caddr p) (cadr p))))))
         (for-all (lambda (g)
                    (and (list? g) (= (length g) 8)
                         (integer-at-least? (car g) 1) (actor:identity? (cadr g))
                         (or (not (cadddr g)) (string? (cadddr g)))
                         (list? (list-ref g 4)) (for-all (lambda (r) (integer-at-least? r 1)) (list-ref g 4))
                         (boolean? (list-ref g 5))
                         (let ([redo (list-ref g 6)]) (or (not redo) (and (list? redo) (= (length redo) 1))))
                         (boolean? (list-ref g 7))))
                  (cadr journal))))

  (edoc "Whether a next id and saved buffer states form a valid saved store."
        (next-id integer "the next buffer id")
        (states list "the saved states")
        (returns boolean))
  (define (valid-import? next-id states)
    ;; A pure validation boundary. No callbacks or partial store mutation;
    ;; session startup distinguishes bad data from a later import failure.
    ;; A state carries five elements, or six with its journal.
    (and (integer-at-least? next-id 1) (list? states)
         (let ([ids (make-eqv-hashtable)] [names (make-hashtable string-hash string=?)])
           (for-all
             (lambda (state)
               (and (list? state) (memv (length state) '(5 6))
                    (integer-at-least? (car state) 1) (< (car state) next-id)
                    (integer-at-least? (cadr state) 0)
                    (string? (caddr state)) (> (string-length (caddr state)) 0)
                    (not (hashtable-contains? ids (car state)))
                    (not (hashtable-contains? names (caddr state)))
                    (vector? (cadddr state)) (> (vector-length (cadddr state)) 0)
                    (for-all text:line? (vector->list (cadddr state)))
                    (persistent-facts? (list-ref state 4))
                    (or (= (length state) 5) (valid-journal? (list-ref state 5) (cadr state)))
                    (begin (hashtable-set! ids (car state) #t) (hashtable-set! names (caddr state) #t) #t)))
             states))))

  (define (restore-journal! b journal)
    ;; Explicit cell identities preserve direct property writes between
    ;; edits and writes restoring the same value. Older journals only had
    ;; values; read them conservatively, joining adjacent equal versions.
    (define cells (make-eq-hashtable))
    (define versions (make-eqv-hashtable))
    (define (data->cell d)
      (if (vector? d)
          (let ([id (vector-ref d 0)])
            (or (hashtable-ref versions id #f)
                (let ([cell (data->cell (vector-ref d 1))])
                  (hashtable-set! versions id cell) cell)))
          (cons (car d) (if (equal? (cdr d) '(missing-property)) missing-property (cdr d)))))
    (define (thread! change)
      ;; (key old new) with cells shared along the key's history
      (let* ([key (car change)]
             [saved-old (data->cell (cadr change))]
             [previous (hashtable-ref cells key #f)]
             [old (if (and (not (vector? (cadr change))) previous (equal? previous saved-old)) previous saved-old)]
             [new (data->cell (caddr change))])
        (hashtable-set! cells key new)
        (list key old new)))
    (define (load-entry e)
      (make-entry (car e) (cadr e) (text:datum->delta (cadddr e)) (list-ref e 4)
                  (map thread! (list-ref e 5)) (caddr e)
                  (and (= (length e) 7) (list-ref e 6))))
    (let* ([entries
            ;; oldest first while threading, then back to newest first
            (reverse
              (map load-entry (reverse (car journal))))]
           [by-revision (make-eqv-hashtable)])
      (for-each (lambda (entry) (hashtable-set! by-revision (entry-revision entry) entry)) entries)
      (when (= (length journal) 5)
        (let ([projection (list-ref journal 4)])
          (when projection
            (buffer-reload-log-set! b
              (list (car projection) (cadr projection) (map load-entry (caddr projection)))))))
      (when (>= (length journal) 4)
        (hashtable-clear! cells)
        (for-each (lambda (data)
                    (let ([cell (data->cell data)]) (hashtable-set! cells (car cell) cell))) (cadddr journal)))
      ;; Adopt a saved identity only when it agrees with the snapshot's fact.
      (buffer-properties-set! b
        (map (lambda (cell)
               (let ([threaded (hashtable-ref cells (car cell) #f)])
                 (if (and threaded (equal? (cdr threaded) (cdr cell))) threaded cell)))
             (buffer-properties b)))
      (for-each (lambda (cell)
                  (when (and (eq? (cdr cell) missing-property)
                             (not (property-cell (buffer-properties b) (car cell))))
                    (buffer-properties-set! b (cons cell (buffer-properties b)))))
                (vector->list (hashtable-values cells)))
      (buffer-deltas-set! b entries)
      (buffer-undo-set! b
        (map (lambda (g)
               (let ([parts (filter values (map (lambda (r) (hashtable-ref by-revision r #f)) (list-ref g 4)))])
                 (make-undo-group (car g) (cadr g) (caddr g) (cadddr g) parts
                                  (list-ref g 6)
                                  (and (list-ref g 7) (= (length parts) (length (list-ref g 4)))))))
             (cadr journal))))
    (when (>= (length journal) 3) (buffer-conflicts-set! b (map data->conflict (caddr journal)))))

  (edoc "The store's saved representation, (values next-id states), each snapshot converted outside the lock when a converter is given."
        (convert procedure "(convert snapshot)"))
  (define export
    (case-lambda
      [()
       (export values)]
      [(convert)
       ;; Capture under this writer, then let a producer convert its plain
       ;; snapshot outside the lock (VT makes a disposable app a transcript).
       ;; Lifecycle pause keeps this and the checkpoint snapshot coherent.
       (let-values ([(next-id states)
                     (locked
                       (lambda ()
                         (values (store-next-id (current-store))
                           (map (lambda (id)
                                  (let ([b (buffer-of 'export id)])
                                    (cons (list id (buffer-revision b) (string-copy (buffer-label b))
                                            (buffer-text b) (property-data b))
                                          (journal-data b))))
                             (list-sort < (vector->list (hashtable-keys (store-buffers (current-store)))))))))])
         (values next-id
           (filter values
             (map (lambda (captured)
                    ;; the journal travels only with a state the converter
                    ;; left as it was: a converted text has no log
                    (let* ([state (convert (car captured))] [facts (list-ref state 4)])
                      (and (not (cond [(assq 'disposable facts) => cdr] [else #f]))
                           (append (list-head state 4)
                             (list (filter (lambda (entry) (memq (car entry) persistent-keys)) facts))
                             (if (eq? state (car captured)) (list (datum:copy (cdr captured))) '())))))
               states))))]))

  (edoc "Replace the store's contents from a saved representation, built privately first."
        (next-id integer "the next buffer id")
        (states list "the saved states"))
  (define (import! next-id states)
    (unless (valid-import? next-id states) (error 'import! "invalid saved store representation"))
    (let ([table (make-eqv-hashtable)])
      ;; Build privately. An unexpected failure cannot publish half a store.
      (for-each
        (lambda (state)
          (let* ([state (datum:copy state)] [facts (list-ref state 4)]
                 [b (make-buffer (caddr state) (cadddr state) (cadr state) '() '() '() '() #f #f
                      (cond [(assq 'modified-at facts) => cdr] [else #f]) '() #f)])
            (install-properties! b (filter (lambda (entry) (not (eq? (car entry) 'modified-at))) facts))
            (refresh-edit-facts! b #f)
            (when (= (length state) 6) (restore-journal! b (list-ref state 5)))
            (hashtable-set! table (car state) b))) states)
      (activity:call-with
        (lambda ()
          (locked
            (lambda ()
              (let ([s (current-store)])
                (unless (and (= (store-next-id s) 1) (zero? (hashtable-size (store-buffers s)))
                             (not (store-closing? s)))
                  (error 'import! "restore requires a fresh empty store"))
                (store-buffers-set! s table)
                (store-next-id-set! s next-id))))))))

  (edoc "Every buffer's id."
        (returns (list-of integer)))
  (define (buffer-list)
    (locked
      (lambda ()
        (vector->list (hashtable-keys (store-buffers (current-store)))))))

  (edoc "Whether a buffer id is live."
        (id integer "the buffer id")
        (returns boolean))
  (define (exists? id)
    (locked
      (lambda ()
        (and (hashtable-ref (store-buffers (current-store)) id #f) #t))))

  (edoc "Whether an actor is in a buffer's audience; a missing buffer is never visible."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (returns boolean))
  (define (visible? actor id)
    ;; Audience is presentation/routing, not permission to read the store.
    ;; Missing content is never visible; an absent audience means all; a
    ;; trashed buffer is visible to nobody until restored.
    (locked
      (lambda ()
        (let ([b (hashtable-ref (store-buffers (current-store)) id #f)])
          (and b (not (property-value b 'trashed #f))
               (actor:in-audience? actor (property-value b 'audience 'all)))))))

  (edoc "How many days a trashed buffer, a backup included, is kept before it is deleted for good, or set it; 30 by default."
        (days integer "the retention in days")
        (returns integer))
  (define trash-retention
    (make-parameter 30
      (lambda (days)
        (unless (and (integer? days) (exact? days) (>= days 1)) (error 'trash-retention "expected a positive number of days" days))
        days)))

  (edoc "Delete the trashed buffers older than the retention, backups included, as the base does daily and at startup; how many went."
        (actor actor "the actor identity")
        (returns integer))
  (define (expire-trash! actor)
    (let ([cutoff (- (time-second (current-time 'time-utc)) (* (trash-retention) 86400))] [gone 0])
      (transact! actor
        (lambda (actor)
          (for-each
            (lambda (id)
              (let* ([b (hashtable-ref (store-buffers (current-store)) id #f)]
                     [trashed (and b (property-value b 'trashed #f))])
                (when (and trashed (< (car trashed) cutoff))
                  (delete-buffer! actor id)
                  (set! gone (+ gone 1)))))
            (vector->list (hashtable-keys (store-buffers (current-store)))))))
      gone))

  (edoc "How many backups of one file the store keeps, the oldest dropped as a new one arrives, or set it; 10 by default."
        (n integer "the count")
        (returns integer))
  (define backups-kept
    (make-parameter 10
      (lambda (n)
        (unless (and (integer? n) (exact? n) (>= n 1)) (error 'backups-kept "expected a positive count" n))
        n)))

  (define (prune-backups! actor path self)
    ;; Caller holds the store lock. The backups of one file beyond
    ;; backups-kept go, the oldest first; self, the one just made, stays.
    (let* ([backups
            (filter values
              (map (lambda (id)
                     (let* ([b (hashtable-ref (store-buffers (current-store)) id #f)]
                            [backup (and b (property-value b 'backup #f))]
                            [trashed (and b (property-value b 'trashed #f))])
                       (and backup trashed (string=? (car backup) path) (list id (car trashed)))))
                   (vector->list (hashtable-keys (store-buffers (current-store))))))]
           [newest-first (list-sort (lambda (a b) (or (> (cadr a) (cadr b)) (and (= (cadr a) (cadr b)) (> (car a) (car b)))))
                           backups)])
      (let drop ([rest newest-first] [n 0])
        (unless (null? rest)
          (unless (or (< n (backups-kept)) (eqv? (car (car rest)) self))
            (delete-buffer! actor (car (car rest))))
          (drop (cdr rest) (+ n 1))))))

  (edoc "A copy of a buffer's name."
        (id integer "the buffer id")
        (returns string))
  (define (buffer-name id)
    (locked (lambda () (string-copy (buffer-label (buffer-of 'buffer-name id))))))

  (edoc "The id of the buffer with a name, or #f."
        (wanted string "the name")
        (returns (or integer #f)))
  (define (find-named wanted)
    ;; The uniquely named buffer, or #f.
    (locked
      (lambda ()
        (find (lambda (id) (equal? (buffer-label (buffer-of 'find-named id)) wanted))
              (vector->list (hashtable-keys (store-buffers (current-store))))))))

  (edoc "A buffer's text and revision from one read: (values text revision)."
        (id integer "the buffer id"))
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

  (edoc "A buffer's text, revision and facts from one read, and the changes since a basis when one is given: (values text revision facts [changes])."
        (id integer "the buffer id")
        (basis (or integer #f) "the earlier revision"))
  (define snapshot-state
    (case-lambda
      [(id)
       (snapshot-state id #f)]
      [(id basis)
       (locked (lambda () (snapshot-values (buffer-of 'snapshot-state id) basis)))]))

  (edoc "A buffer's name and snapshot for remote adoption from one read, with every fact or the selected ones: #t for all, or a list of keys."
        (id integer "the buffer id")
        (basis (or integer #f) "the earlier revision")
        (facts (or boolean list) "which facts"))
  (define state
    ;; Remote adoption needs existence/name and the snapshot from one read.
    ;; Own mutable metadata here; the caller serializes after unlocking.
    ;; The optional selector names the facts wanted: #t for all, or a list
    ;; of keys, so a reader that holds the facts pays only for the
    ;; computed ones instead of a copy of the file baseline per read.
    (case-lambda
      [(id basis)
       (state id basis #t)]
      [(id basis facts)
       (locked
         (lambda ()
           (let ([b (hashtable-ref (store-buffers (current-store)) id #f)])
             (and b (cons (string-copy (buffer-label b))
                      (call-with-values (lambda () (snapshot-values b basis facts)) list))))))]))

  (define (snapshot-values b basis . selector)
    ;; Caller holds the lock. Text/facts and the optional anchor chain must
    ;; describe the same commit, including when notifications are pending.
    (let ([facts (if (or (null? selector) (eq? (car selector) #t))
                     (property-data b)
                     (map datum:copy
                          (filter (lambda (cell) (memq (car cell) (car selector)))
                                  (current-properties b))))])
      (if basis
          (let ([entries (entries-since b basis)])
            (values (buffer-text b) (buffer-revision b) facts
              (and entries (map change-data entries))))
          (values (buffer-text b) (buffer-revision b) facts))))

  (edoc "A buffer's text, revision and the changes since a basis revision: (values text revision changes), changes #f when the basis is gone."
        (id integer "the buffer id")
        (basis (or integer #f) "the earlier revision"))
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
    (list (entry-revision entry) (datum:copy (entry-actor entry)) (entry-delta entry)))

  (edoc "A buffer's revision."
        (id integer "the buffer id")
        (returns integer))
  (define (revision id)
    (locked (lambda () (buffer-revision (buffer-of 'revision id)))))

  (edoc "How many lines a buffer has."
        (id integer "the buffer id")
        (returns integer))
  (define (line-count id)
    (locked
      (lambda () (vector-length (buffer-text (buffer-of 'line-count id))))))

  (edoc "One line of a buffer."
        (id integer "the buffer id")
        (n integer "the row")
        (returns string))
  (define (line id n)
    (locked
      (lambda ()
        (let ([text (buffer-text (buffer-of 'line id))])
          (unless (and (>= n 0) (< n (vector-length text)))
            (error 'line (format "no line ~a in buffer ~a" n id)))
          (vector-ref text n)))))

  (edoc "A span's content in a buffer, as lines."
        (id integer "the buffer id")
        (span (record span) "the span")
        (returns list))
  (define (extract id span)
    (locked
      (lambda () (text:extract (buffer-text (buffer-of 'extract id)) span))))

  ;;; Edits -------------------------------------------------------------------

  (define (entries-since b basis)
    ;; The complete chain after basis, oldest first, or #f.  Edits and
    ;; incremental snapshot readers share the same retention boundary.
    ;; Bridges preserve positions across resets and whole-text replacements.
    (let ([current (buffer-revision b)] [bridges (hashtable-ref bridges b '())])
      (let follow ([basis basis] [acc '()])
        (cond [(= basis current) acc]
              [(assv basis bridges)
               => (lambda (bridge) (follow (cadr bridge) (append acc (cddr bridge))))]
              [else
               ;; the log from here, cut where the next bridge starts: its
               ;; steps stand in for the log's entry there
               (let ([chain (chain-since (buffer-deltas b) current basis)]
                     [next (fold-left (lambda (best bridge)
                                        (let ([from (car bridge)])
                                          (if (and (> from basis) (or (not best) (< from best))) from best)))
                                      #f bridges)])
                 (and chain
                      (if next
                          (follow next (append acc (filter (lambda (entry) (<= (entry-revision entry) next)) chain)))
                          (append acc chain))))]))))

  ;; A bridge takes a reader from one revision to a later one where the log
  ;; cannot: across a reset, which clears the log, a line diff of the two texts, the
  ;; cleared entries surviving as one-step bridges before it; and over a
  ;; whole-text edit, a reread or its undo, the same line diff in place of
  ;; the one delta that would collapse every position. Per buffer, (from to
  ;; . entries) newest first, the entries pseudo ones at the revision the
  ;; bridge leads to; a bridge older than the log's retention goes.
  (define bridges (make-weak-eq-hashtable))

  (define (remember-bridge! b from to actor deltas)
    ;; Caller holds the store lock.
    (let ([oldest (- (buffer-revision b) (log-retention))])
      (hashtable-set! bridges b
        (cons (cons* from to (map (lambda (d) (make-entry to actor d #f '() '())) deltas))
              (filter (lambda (bridge) (>= (cadr bridge) oldest)) (hashtable-ref bridges b '()))))))

  (define (whole-delta? text delta)
    ;; whether a delta replaces the whole of a text with something in it
    (let* ([span (text:delta-span delta)] [last (- (vector-length text) 1)])
      (and (equal? (text:span-start span) '(0 . 0))
           (equal? (text:span-end span) (cons last (string-length (vector-ref text last))))
           (not (and (= last 0) (string=? (vector-ref text 0) ""))))))

  (define (log-since b basis)
    ;; the log's own chain after basis, oldest first, or #f: what the
    ;; reload walks from the baseline, bridges aside
    (chain-since (buffer-deltas b) (buffer-revision b) basis))

  (define projecting-reload? (make-parameter #f))

  (define (chain-since deltas current basis)
    (cond
      [(= basis current) '()]
      [(or (> basis current)
           ;; A private merge projection keeps the original IDs, with gaps.
           (< basis (if (and (projecting-reload?) (pair? deltas))
                        (- (entry-revision (car (reverse deltas))) 1)
                        (- current (length deltas)))))
       #f]
      [else
       (let take ([entries deltas] [acc '()])
         (cond [(null? entries) acc]
               [(<= (entry-revision (car entries)) basis) acc]
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

  (define (install-edit! b id actor new-text delta origin facts commit-facts labels . settling*)
    ;; All committed edits, including history operations, pass here.
    ;; The attribution log always retains the actual deltas; cancelling
    ;; pairs is only a temporary proof used when planning another undo.
    (let* ([old-text (buffer-text b)]
           [new-revision (+ (buffer-revision b) 1)]
           [trailing? (property-value b 'trailing #t)]
           [entry (make-entry new-revision actor delta origin facts labels)]
           [target (and origin (find (lambda (e) (= (entry-revision e) (cadddr origin))) (buffer-deltas b)))]
           [settling (append (if (pair? settling*) (car settling*) '())
                       (if target (filter (lambda (c) (eqv? (conflict-unsettled-by c) (entry-revision target))) (unsettled-conflicts b)) '()))]
           [previous (map (lambda (c) (conflict-journal-data b c)) (remq* settling (unsettled-conflicts b)))]
           [pending-count (length (unsettled-conflicts b))]
           [between (and target (map entry-delta (effective-chain (log-since b (entry-revision target)))))]
           [restored (and target (restore-conflict-change b target delta between))]
           ;; a whole-text replacement, a reread or its undo say, carries
           ;; positions on a line diff of the two texts, the steps a bridge
           ;; hands readers, rather than collapsing them to the text's end
           [steps (and (whole-delta? old-text delta) (edit-deltas old-text (edits-between old-text new-text)))]
           [carry (lambda (value) (if steps (fold-left rebase-mark-value value steps) (rebase-mark-value value delta)))])
      (buffer-text-set! b new-text)
      (buffer-revision-set! b new-revision)
      (buffer-deltas-set!
        b (bounded (cons entry (buffer-deltas b)) (log-retention)))
      (buffer-properties-set! b (apply-property-changes (buffer-properties b) facts))
      (install-properties! b commit-facts)
      (refresh-edit-facts! b
        (or (not (equal? (text:delta-removed delta) (text:delta-inserted delta)))
            (not (eq? trailing? (property-value b 'trailing #t)))))
      (buffer-marks-set!
        b (map (lambda (entry) (cons (car entry) (carry (cdr entry)))) (buffer-marks b)))
      ;; Explicit settlement freezes its alternatives before any ordinary
      ;; region carrying can widen or merge them.
      (buffer-conflicts-set! b (remq* settling (buffer-conflicts b)))
      (carry-conflicts! b old-text (or steps (list delta)))
      (when restored
        (let ([ids (cdr restored)])
          (buffer-conflicts-set! b
            (list-sort (lambda (a b) (> (conflict-revision a) (conflict-revision b)))
              (append (car restored) (remp (lambda (c) (memv (conflict-revision c) ids)) (buffer-conflicts b)))))))
      (vector-set! entry 6
        (let ([after (map (lambda (c) (conflict-journal-data b c)) (unsettled-conflicts b))])
          (and (or (pair? previous) (pair? after)) (list previous after))))
      (when (pair? settling)
        (settle-conflicts! b settling new-revision)
        (buffer-conflicts-set! b (list-sort (lambda (a b) (> (conflict-revision a) (conflict-revision b)))
                                   (append settling (buffer-conflicts b)))))
      (unless (= pending-count (length (unsettled-conflicts b))) (note-conflicts! id actor))
      (when target (follow-history-conflicts! b id actor entry target between))
      (buffer-conflicts-set! b (retained-conflicts b))
      (when steps (remember-bridge! b (- new-revision 1) new-revision actor steps))
      (enqueue-event!
        (append (list 'edit id new-revision actor delta)
                (if origin (list origin) '())))
      (for-each (lambda (change) (enqueue-event! `(property ,id ,(car change) ,actor))) facts)
      (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) commit-facts)
      (values new-revision delta)))

  (define (apply-locked! b id actor span replacement origin properties commit-facts labels . settling)
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
        commit-facts labels (if (pair? settling) (car settling) '()))))

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
           [reverted (reverted-revisions (buffer-deltas b))]
           [existing
            (and key
                 (find (lambda (group)
                         (and (pair? (action-parts b group 'undo reverted))
                              (equal? (undo-group-actor group) actor)
                              (equal? (undo-group-key group) key)))
                       (buffer-undo b)))]
           [group (or existing
                      (make-undo-group (entry-revision entry) actor key label
                                       '() #f #t))]
           [parts (cons entry (undo-group-parts group))])
      (when label (undo-group-label-set! group label))
      (when (> (length parts) (log-retention))
        (undo-group-complete?-set! group #f))
      (undo-group-parts-set! group (bounded parts (log-retention)))
      (buffer-undo-set!
        b (bounded (cons group (remq group (buffer-undo b))) (log-retention)))))

  (edoc "Apply an edit against a basis revision, rebased across what landed since: (values applied revision), or (values stale overlap|basis-too-old)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (basis integer "the revision edited")
        (span (record span) "the span replaced")
        (replacement list "the replacement lines")
        (options (list-of any) "an edit context, then write access"))
  (define (edit! actor id basis span replacement . options)
    ;; The transaction: apply the edit as the actor meant it against
    ;; the basis revision, rebasing it across whatever landed since --
    ;; or refuse.  -> (values 'applied revision)
    ;;             |  (values 'stale 'overlap)       edited meanwhile
    ;;             |  (values 'stale 'basis-too-old) log outgrown
    ;; Optional context (key label [undo-facts [commit-facts [expected]]]) groups edits
    ;; and commits facts with the text. Optional write access follows it;
    ;; clients pass 'any or allowed names, producers omit it (#f).
    (let-values ([(status detail)
                  (apply edit-with-snapshot! actor id basis span replacement options)])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (edoc "Apply an edit like edit!, acknowledging with (revision text changes edit-facts) before subscribers can write again."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (basis integer "the revision edited")
        (span (record span) "the span replaced")
        (replacement list "the replacement lines")
        (options (list-of any) "an edit context, then write access"))
  (define (edit-with-snapshot! actor id basis span replacement . options)
    ;; The same transaction with an atomic acknowledgement:
    ;; (revision text changes edit-facts), ending at this edit, before subscribers
    ;; can write again.  Changes include the complete chain from basis
    ;; through the accepted edit, even if committing trims its oldest
    ;; entry out of the retained log.  A head uses this to place its
    ;; command's anchors without guessing where the edit actually landed.
    (unless (<= (length options) 2) (error 'edit! "expected context and write access" options))
    (let ([context (and (pair? options) (car options))]
          [access (own-write-access (and (= (length options) 2) (cadr options)))])
      (validate-edit-context context)
      (let* ([group-context (and context (datum:copy (list (car context) (cadr context)) values))]
             [properties (datum:copy (property:context-undo context))]
             [commit-facts (datum:copy (property:context-commit context))]
             [expected (and context (datum:copy (property:context-expected context)))]
             [labels (datum:copy (property:context-labels context))]
             [outcome
              (transact! actor
                (lambda (actor)
                  (cond [(write-refusal id access) => (lambda (reason) (list 'refused reason))]
                    [else
                     (let* ([b (if expected (hashtable-ref (store-buffers (current-store)) id #f)
                                   (buffer-of 'edit! id))]
                            [since (and b (entries-since b basis))]
                            [rebased (and since
                                       (rebase-through
                                         (text:normalize-span span)
                                         (map entry-delta since)))])
                       (cond
                         [(and expected (not (and b (property:matches? expected (current-properties b)))))
                          (list 'stale 'property-changed)]
                         [(not since) (list 'stale 'basis-too-old)]
                         [(not rebased) (list 'stale 'overlap)]
                         [else
                          (let-values ([(new-revision delta)
                                        (apply-locked! b id actor rebased
                                                       replacement #f
                                                       properties commit-facts labels)])
                            (remember-edit! b actor group-context)
                            (list 'applied new-revision (buffer-text b)
                              (append (map change-data since)
                                      (list (change-data (car (buffer-deltas b)))))
                              (edit-facts b)))]))])))])
        (case (car outcome)
          [(applied)
           (values 'applied (cdr outcome))]
          [else (values (car outcome) (cadr outcome))]))))

  (define (same-delta? a b)
    (and (equal? (text:span-start (text:delta-span a))
                 (text:span-start (text:delta-span b)))
         (equal? (text:span-end (text:delta-span a))
                 (text:span-end (text:delta-span b)))
         (equal? (text:delta-new-end a) (text:delta-new-end b))
         (equal? (text:delta-removed a) (text:delta-removed b))
         (equal? (text:delta-inserted a) (text:delta-inserted b))))

  (define (cancel-compensated chain present next protected reverted)
    ;; Keep a chain with the same net effect, newest first. Commute an
    ;; inverse across intervening disjoint entries before removing its pair.
    ;; Protected entries remain explicit for the caller to invert. Keep the
    ;; original chain whenever cancellation cannot be proved.
    (define (keep)
      (hashtable-set! present (entry-revision next) #t)
      (cons next chain))
    (let ([origin (entry-origin next)])
      (if (or (not origin) (memv (entry-revision next) reverted)
              (memv (entry-revision next) protected)
              (memv (list-ref origin 3) protected)
              (not (hashtable-ref present (list-ref origin 3) #f)))
          (keep)
          (let find-target ([remaining chain] [between '()])
            (cond
              [(null? remaining) (keep)]
              [(= (entry-revision (car remaining)) (list-ref origin 3))
               (or (let commute ([between between]
                                 [inverse (text:invert-delta (entry-delta (car remaining)))]
                                 [shifted '()])
                     (if (null? between)
                       (and (same-delta? inverse (entry-delta next))
                         (begin (hashtable-delete! present (entry-revision (car remaining)))
                                (append shifted (cdr remaining))))
                       (let* ([entry (car between)]
                              [delta (entry-delta entry)]
                              [after (text:rebase-delta inverse delta)]
                              [before (text:rebase-delta delta inverse 'stay)])
                         (and after before
                           (commute (cdr between) after
                             (cons (make-entry (entry-revision entry) (entry-actor entry) before
                                               (entry-origin entry) (entry-facts entry) (entry-labels entry)
                                               (move-conflict-change entry before))
                                   shifted))))))
                   (keep))]
              [else (find-target (cdr remaining) (cons (car remaining) between))])))))

  (define (effective-chain entries . protected)
    ;; Remove provably cancelling pairs without losing their net effect.
    ;; Protected entries stay explicit while planning their inverse; other
    ;; pairs can commute across them, moving their coordinates with the text.
    ;; Leave a reverted inverse for its later inverse to cancel. Removing
    ;; its target first would orphan that later inverse and lose the proof
    ;; that undoing a reload restores the earlier action's contribution.
    ;; An unproved cancellation leaves the valid original chain intact.
    (let ([present (make-eqv-hashtable)] [reverted (reverted-revisions (reverse entries))])
      (reverse (fold-left (lambda (chain entry) (cancel-compensated chain present entry protected reverted)) '() entries))))

  (define (plan-inversion b actor targets origin-of check-facts? . projected-chain)
    ;; The inverses of targets, entries of b newest first, each rebased
    ;; across the later deltas and applied to temporary immutable text in
    ;; turn, so a later target's inverse is carried across the earlier
    ;; ones' too: (values steps conflicts), the steps (new-text delta
    ;; origin inverse-facts) oldest first, the conflicts (revision . cause)
    ;; for the targets left applied -- the cause basis-too-old when the
    ;; chain from the target is not retained, property-changed when a fact
    ;; it set changed since (facts checked for history and rewrites, not
    ;; for views), the revision of the later entry that overlaps it, or
    ;; overlap and text-changed when nothing more precise is known.
    ;; Nothing changes: a caller installs the steps, or shows them.
    (define (using effective)
      (let ([projected (make-eqv-hashtable)])
        (for-each (lambda (e) (hashtable-set! projected (entry-revision e) e)) effective)
        (let plan ([targets targets]
                   [text (buffer-text b)]
                   [properties (buffer-properties b)]
                   [revision (buffer-revision b)]
                   [deltas (reverse effective)]
                   [planned '()]
                   [conflicts '()])
          (if (null? targets)
            (values (reverse planned) (reverse conflicts))
            (let* ([target (car targets)]
                   [facts (entry-facts target)]
                   [projected-target (hashtable-ref projected (entry-revision target) #f)]
                   [since (and projected-target
                            (reverse (filter (lambda (e) (> (entry-revision e) (entry-revision target))) deltas)))]
                   [chain (and since (effective-chain since))]
                   [carried
                    ;; the inverse carried across the chain, or (overlap . revision)
                    (and chain
                      (let carry ([inverse (text:invert-delta (entry-delta projected-target))] [chain chain])
                        (cond [(null? chain) inverse]
                              [(text:rebase-delta inverse (entry-delta (car chain)))
                               => (lambda (moved) (carry moved (cdr chain)))]
                              [else
                               (let ([e (car chain)])
                                 ;; A provisional inverse names its retained
                                 ;; target, not a revision absent from the store.
                                 (cons 'overlap (if (> (entry-revision e) (buffer-revision b))
                                                    (list-ref (entry-origin e) 3) (entry-revision e))))])))]
                   [inverse (and carried (not (pair? carried)) carried)]
                   [cause
                    (cond
                      [(not since) 'basis-too-old]
                      [(and check-facts?
                         (not (for-all (lambda (change)
                                         (eq? (property-cell properties (car change)) (caddr change)))
                                       facts)))
                       'property-changed]
                      [(pair? carried) (cdr carried)]
                      [(not (equal? (text:delta-removed inverse) (text:extract text (text:delta-span inverse))))
                       'text-changed]
                      [else #f])])
              (if cause
                (plan (cdr targets) text properties revision deltas planned
                      (cons (cons (entry-revision target) cause) conflicts))
                (let*-values ([(new-text delta)
                               (text:apply-edit text (text:delta-span inverse) (text:delta-inserted inverse))]
                              [(origin) (origin-of target)]
                              [(inverse-facts)
                               (if check-facts?
                                   (map (lambda (change) (list (car change) (caddr change) (cadr change))) facts)
                                   '())]
                              [(entry) (make-entry (+ revision 1) actor delta origin inverse-facts '())])
                  (plan (cdr targets) new-text (apply-property-changes properties inverse-facts) (+ revision 1)
                        (cons entry deltas)
                        (cons (list new-text delta origin inverse-facts) planned)
                        conflicts))))))))
    ;; Keep the original coordinates whenever they give a complete plan.
    ;; Commuting cancelling pairs past a target can erase the ordering of
    ;; insertions at one point. Projection is needed only when an inverse
    ;; cannot cross the original history (for example, crossed deletions).
    (let*-values ([(steps conflicts) (using (if (pair? projected-chain) (car projected-chain) (reverse (buffer-deltas b))))]
                  [(steps conflicts)
                   (if (or (null? conflicts) (pair? projected-chain)) (values steps conflicts)
                     (let-values ([(projected trouble)
                                   (using (apply effective-chain (reverse (buffer-deltas b)) (map entry-revision targets)))])
                       (if (null? trouble) (values projected trouble) (values steps conflicts))))])
      (check-conflict-plan b actor steps conflicts)))

  (edoc "Validate conflict history on a private buffer copy with notifications captured and discarded."
        (b any "the original buffer")
        (actor actor "the inverse's actor")
        (steps list "the proposed inverses")
        (conflicts list "the text planner's refusals")
        (effects internal))
  (define (check-conflict-plan b actor steps conflicts)
    ;; Conflict alternatives participate in the same all-or-nothing proof
    ;; as text and properties. Simulate only when conflict records exist.
    (if (null? (buffer-conflicts b)) (values steps conflicts)
        (let ([scratch (copy-buffer b)] [failed #f])
          (parameterize ([planned-events (box '())] [log-retention (+ (log-retention) (length steps))])
            (for-each (lambda (step)
                        (unless failed
                          (guard (ex [(conflict-history? ex) (set! failed (cadddr (caddr step)))])
                            (install-edit! scratch #f actor (car step) (cadr step) (caddr step) (cadddr step) '() '())))) steps))
          (if failed (values '() (cons (cons failed 'conflict-changed) conflicts)) (values steps conflicts)))))

  (define (conflict-reason conflicts)
    ;; the one reason a history step reports for its first conflict
    (let ([cause (cdar conflicts)])
      (if (or (integer? cause) (eq? cause 'text-changed)) 'overlap cause)))

  (define (undo-scope? scope)
    (or (memq scope '(mine all))
        (and (list? scope) (= (length scope) 2) (eq? (car scope) 'actor))))

  (define (scope-matches? scope actor group)
    (or (eq? scope 'all)
        (equal? (undo-group-actor group)
                (if (eq? scope 'mine) actor (cadr scope)))))

  (define (logical-parts b group reverted)
    ;; A foreign rewrite can disable our newest undo/redo and expose an
    ;; earlier incarnation of that same member. Keep the saved tip intact:
    ;; undoing the foreign rewrite can make it current again.
    (map (lambda (part)
           (let follow ([part part])
             (let ([origin (entry-origin part)])
               (if (and (memv (entry-revision part) reverted) origin
                        (memq (car origin) '(undo redo))
                        (= (caddr origin) (undo-group-id group)))
                   (cond [(find (lambda (e) (= (entry-revision e) (cadddr origin))) (buffer-deltas b)) => follow]
                         [else part])
                   part))))
      (undo-group-parts group)))

  (define (action-parts b group direction reverted)
    ;; Keep membership even while a rewrite disables a part. Whether it
    ;; currently contributes follows the log; undo/redo replace only the
    ;; eligible parts, allowing another actor to restore a disabled part
    ;; after the rest of its action was undone.
    (filter (lambda (part)
              (and (not (memv (entry-revision part) reverted))
                   (eq? (and (entry-origin part) (eq? (car (entry-origin part)) 'undo))
                        (eq? direction 'redo))))
            (logical-parts b group reverted)))

  (edoc "Undo or redo in a buffer under a scope: (values status detail), status applied, blocked, nothing or refused."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (direction (one-of undo redo) "which way")
        (scope any "mine, all or (actor who)")
        (access* (list-of any) "write access, at most one"))
  (define (history-step! actor id direction scope . access*)
    ;; The common history transaction.  Undo selects mine, all, or
    ;; (actor who).  Redo always reverses this requester's latest undo,
    ;; independently of the original author (scope must be mine).
    ;; -> applied (revision action-id original-author group-key label)
    ;;  | blocked overlap|basis-too-old | nothing #f | refused read-only|buffer
    ;; Optional write access is the same client/producer rule as edit!.
    (unless (and (memq direction '(undo redo)) (undo-scope? scope)
                 (or (eq? direction 'undo) (eq? scope 'mine)))
      (error 'history-step! "invalid history direction or scope" direction scope))
    (unless (<= (length access*) 1) (error 'history-step! "expected one write access" access*))
    (let ([access (own-write-access (and (pair? access*) (car access*)))])
      (transact! actor
        (lambda (actor)
          (cond [(write-refusal id access) => (lambda (reason) (values 'refused reason))]
            [else
             (let* ([b (buffer-of 'history-step! id)]
                    [reverted (reverted-revisions (buffer-deltas b))]
                    [group
                     (find (lambda (group)
                             (and (pair? (action-parts b group direction reverted))
                               (if (eq? direction 'undo)
                                 (scope-matches? scope actor group)
                                 (and
                                   (undo-group-redo-actor group)
                                   (equal? (car (undo-group-redo-actor group)) actor)))))
                       (buffer-undo b))]
                    [members (and group (logical-parts b group reverted))]
                    [targets (and group (action-parts b group direction reverted))])
               (if (not group)
                 (values 'nothing #f)
                 (let-values ([(plan conflicts)
                               (if (undo-group-complete? group)
                                   (plan-inversion b actor targets
                                     (lambda (part)
                                       (list direction (undo-group-actor group) (undo-group-id group)
                                             (entry-revision part)))
                                     #t)
                                   (values '() '((#f . basis-too-old))))])
                   (if (pair? conflicts)
                     (values 'blocked (conflict-reason conflicts))
                     (let ([parts '()])
                       (install-inverses! b id actor plan
                         (lambda (entry) (set! parts (cons entry parts))))
                       (undo-group-parts-set! group
                         (list-sort newer? (append parts (remp (lambda (p) (memq p targets)) members))))
                       (undo-group-redo-actor-set! group (and (eq? direction 'undo) (list actor)))
                       (buffer-undo-set! b (cons group (remq group (buffer-undo b))))
                       (values 'applied
                               (datum:copy
                                 (list (buffer-revision b) (undo-group-id group)
                                       (undo-group-actor group) (undo-group-key group)
                                       (undo-group-label group)) values)))))))])))))

  (edoc "Undo in a buffer: (values status detail), the detail the new revision when applied."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (scope (list-of any) "mine, all or (actor who), at most one"))
  (define (undo! actor id . scope)
    ;; Compatibility result: the new revision, or a refusal reason.
    ;; The optional scope uses the same selector as history-step!.
    (unless (<= (length scope) 1) (error 'undo! "expected at most one scope" scope))
    (let-values ([(status detail)
                  (history-step! actor id 'undo (if (pair? scope) (car scope) 'mine))])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (edoc "Redo the actor's latest undo in a buffer: (values status detail)."
        (actor actor "the actor identity")
        (id integer "the buffer id"))
  (define (redo! actor id)
    (let-values ([(status detail) (history-step! actor id 'redo 'mine)])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (edoc "The actors with retained live actions in a buffer, newest first."
        (id integer "the buffer id")
        (returns list))
  (define (undo-authors id)
    ;; Actors with retained live actions, newest first.  Selection is
    ;; advisory: the actual history transaction rechecks under the lock.
    (locked
      (lambda ()
        (let* ([b (buffer-of 'undo-authors id)] [reverted (reverted-revisions (buffer-deltas b))])
          (let loop ([groups (buffer-undo b)] [authors '()])
            (cond
              [(null? groups) (datum:copy (reverse authors))]
              [(and (pair? (action-parts b (car groups) 'undo reverted))
                 (not (member (undo-group-actor (car groups)) authors)))
               (loop (cdr groups) (cons (undo-group-actor (car groups)) authors))]
              [else (loop (cdr groups) authors)]))))))

  (edoc "A buffer's undo groups as data, newest first, (actor label) each, an undone group's included."
        (id integer "the buffer id")
        (returns list))
  (define (undo-labels id)
    (locked (lambda () (map (lambda (group) (list (undo-group-actor group) (undo-group-label group)))
                            (buffer-undo (buffer-of 'undo-labels id))))))

  (edoc "A buffer's newest applied edits as plain data, (revision actor start end new-end) each, newest first."
        (id integer "the buffer id")
        (count (list-of integer) "how many, at most one; 20 by default")
        (returns list))
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
                       [d (entry-delta entry)]
                       [s (text:delta-span d)])
                  (cons (datum:copy (append (list (entry-revision entry)
                                              (entry-actor entry)
                                              (text:span-start s)
                                              (text:span-end s)
                                              (text:delta-new-end d))
                                      (if (entry-origin entry) (list (entry-origin entry)) '())))
                        (take (cdr entries) (- n 1))))))))))

  (edoc "A buffer's newest edits with their spans rebased into the current text: (span actor revision) each, newest first."
        (id integer "the buffer id")
        (count (list-of integer) "how many, at most one")
        (returns list))
  (define (blame id . count)
    ;; Attribution with geometry: the newest applied edits with their
    ;; written spans rebased into the CURRENT text, as plain data --
    ;; ((span actor revision) ...) newest first.  A span a later edit
    ;; swallowed degrades to endpoint rebasing, like span marks:
    ;; attribution survives races, it never goes stale.  Reach: the
    ;; delta log (log-retention entries); a reset clears it.  Blame is
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
                       [d (entry-delta entry)]
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
                        (cons (list (copy-mark-value current)
                                    (datum:copy (entry-actor entry))
                                    (entry-revision entry))
                              acc)))))))))

  (define (selected? entry state selector)
    ;; whether an entry meets every constraint of a selector but the count
    (for-all
      (lambda (constraint)
        (case (car constraint)
          [(count) #t]
          [(state) (eq? state (cdr constraint))]
          [(actor) (equal? (entry-actor entry) (cdr constraint))]
          [(batch) (let ([batch (assq 'batch (entry-labels entry))])
                     (and batch (equal? (cdr batch) (cdr constraint))))]
          [(since) (> (entry-revision entry) (cdr constraint))]
          [(until) (<= (entry-revision entry) (cdr constraint))]
          [else #f]))
      selector))

  (define (validate-selector selector)
    (define (natural? key)
      (let ([c (assq key selector)]) (or (not c) (and (integer? (cdr c)) (exact? (cdr c)) (>= (cdr c) 0)))))
    (unless (and (list? selector)
                 (for-all (lambda (c) (and (pair? c) (memq (car c) '(count actor batch since until state)))) selector)
                 (natural? 'count) (natural? 'since) (natural? 'until)
                 (let ([c (assq 'state selector)]) (or (not c) (memq (cdr c) '(enabled disabled)))))
      (error 'log "expected a selector alist among count, actor, batch, since, until and state" selector))
    selector)

  (define (reverted-revisions entries)
    ;; the revisions a live inverse reverts, from entries newest first: an
    ;; entry is live unless a later live inverse targets it, and a live
    ;; inverse -- an undo, a redo, a rewrite -- reverts its origin's target
    (let loop ([entries entries] [reverted '()])
      (if (null? entries)
          reverted
          (let ([entry (car entries)])
            (loop (cdr entries)
                  (if (and (entry-origin entry) (not (memv (entry-revision entry) reverted)))
                      (cons (list-ref (entry-origin entry) 3) reverted)
                      reverted))))))

  (define (entry-data entry state)
    ;; an entry as owned plain data
    (datum:copy
      (list (entry-revision entry) (entry-actor entry) (entry-labels entry)
            (text:delta->datum (entry-delta entry)) (entry-origin entry) state)))

  (edoc "A buffer's retained log entries as data, newest first, (revision actor labels delta origin state) each: the delta as (span-datum removed inserted), the origin of an inverse or #f, the state enabled or disabled -- reverted by a live undo or rewrite; a selector alist narrows them by (count . n), (actor . who), (batch . id), (since . revision) for later entries, (until . revision) for that one and earlier, and (state . enabled|disabled)."
        (id integer "the buffer id")
        (selector (list-of any) "constraints, at most one alist")
        (returns list))
  (define (log-entries id . selector)
    (unless (<= (length selector) 1) (error 'log "expected at most one selector" selector))
    (let* ([wanted (validate-selector (if (pair? selector) (car selector) '()))]
           [count (cond [(assq 'count wanted) => cdr] [else #f])])
      (locked
        (lambda ()
          (let* ([all (buffer-deltas (buffer-of 'log id))]
                 [reverted (reverted-revisions all)])
            (define (state-of entry) (if (memv (entry-revision entry) reverted) 'disabled 'enabled))
            (let take ([entries all] [left count] [acc '()])
              (cond
                [(or (null? entries) (and left (zero? left))) (reverse acc)]
                [(selected? (car entries) (state-of (car entries)) wanted)
                 (take (cdr entries) (and left (- left 1))
                       (cons (entry-data (car entries) (state-of (car entries))) acc))]
                [else (take (cdr entries) left acc)])))))))

  (define (entries-for who b revisions)
    ;; the enabled entries of b at the given revisions, newest first;
    ;; disabling entries is a set operation, even if an API caller repeats one
    (unless (and (list? revisions)
                 (for-all (lambda (r) (and (integer? r) (exact? r) (>= r 0))) revisions))
      (error who "expected a list of revisions" revisions))
    (let ([reverted (reverted-revisions (buffer-deltas b))]
          [seen (make-eqv-hashtable)])
      (map (lambda (r)
             (let ([entry (find (lambda (entry) (= (entry-revision entry) r)) (buffer-deltas b))])
               (unless entry (error who (format "no retained entry ~a" r)))
               (when (memv r reverted) (error who (format "entry ~a is already disabled" r)))
               entry))
           (filter (lambda (r)
                     (and (not (hashtable-ref seen r #f))
                       (begin (hashtable-set! seen r #t) #t)))
             (list-sort > revisions)))))

  (edoc "A view of a buffer with entries disabled, the rest rebased over their absence: (values text mapping conflicts), the text as lines, the mapping the deltas taking the current text to it as data, oldest first, and the conflicts as (revision . cause) for the entries left applied -- the revision of a later entry overlapping one, or basis-too-old. Nothing changes; facts are not consulted."
        (id integer "the buffer id")
        (disabled (list-of integer) "the revisions to disable, repetitions counted once")
        (returns any "(values text mapping conflicts)"))
  (define (view id disabled)
    (locked
      (lambda ()
        (let* ([b (buffer-of 'view id)]
               [targets (entries-for 'view b disabled)])
          (let-values ([(steps conflicts)
                        (plan-inversion b #f targets
                          (lambda (target) (list 'view #f (buffer-revision b) (entry-revision target)))
                          #f)])
            (values (if (null? steps) (buffer-text b) (car (car (reverse steps))))
                    (map (lambda (step) (text:delta->datum (cadr step))) steps)
                    (datum:copy conflicts)))))))

  (edoc "Disable entries of a buffer for everyone: their inverses, rebased across what followed, are installed as the actor's own action. Their original actions skip them while disabled; undoing the rewrite restores their membership. (values applied revision); (values blocked conflicts) as view reports them when any entry cannot be inverted or a fact it set changed; (values refused read-only|buffer)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (disabled (list-of integer) "the revisions to disable, repetitions counted once")
        (access* (list-of any) "write access, at most one"))
  (define (rewrite! actor id disabled . access*)
    (unless (<= (length access*) 1) (error 'rewrite! "expected one write access" access*))
    (let ([access (own-write-access (and (pair? access*) (car access*)))])
      (transact! actor
        (lambda (actor)
          (cond [(write-refusal id access) => (lambda (reason) (values 'refused reason))]
            [else
             (let* ([b (buffer-of 'rewrite! id)]
                    [targets (entries-for 'rewrite! b disabled)]
                    [first-revision (+ (buffer-revision b) 1)])
               (let-values ([(steps conflicts)
                             (plan-inversion b actor targets
                               (lambda (target) (list 'rewrite actor first-revision (entry-revision target)))
                               #t)])
                 (cond
                   [(pair? conflicts) (values 'blocked (datum:copy conflicts))]
                   [else
                    (install-inverses! b id actor steps
                      (lambda (entry) (remember-edit! b actor (list (list 'rewrite first-revision) "rewrite"))))
                    (values 'applied (buffer-revision b))])))])))))

  ;;; Reloading from disk -----------------------------------------------------------

  ;; Reloading replaces the baseline with the disk's text and reapplies the
  ;; log on top: the disk's changes since the baseline, the base fact, and
  ;; the buffer's entries since it are two chains from one text, and each
  ;; entry is carried across the disk's changes as edits by concurrent actors
  ;; are carried across each other. An entry a disk change overlaps is
  ;; disabled first, with the later entries depending on it, and pends as a
  ;; conflict: the disk's side stands in the text, the entry's side is kept
  ;; as lines, and resolve! settles it by keeping the disk's, writing the
  ;; entry's lines over the disk's region, or writing lines of its own.

  ;; A conflict pends until settled; settled by an entry, a resolution's or a
  ;; reread's, it stays in the list with its span frozen as that entry found
  ;; it, so the entry's undo brings it back, and its redo settles it again
  (define-record-type conflict (fields revision actor labels (mutable span) (mutable mine) disk (mutable settled) (mutable unsettled-by)))

  (define (unsettled-conflicts b)
    (filter (lambda (c) (not (conflict-settled c))) (buffer-conflicts b)))

  (define (retained-conflicts b)
    ;; The retained log is contiguous. Pending conflicts survive without
    ;; their originating entry; settled ones need only outlive settlement's
    ;; undo opportunity. Use the same rule in memory and in the journal.
    (let ([oldest (- (buffer-revision b) (length (buffer-deltas b)))])
      (filter (lambda (c) (or (not (conflict-settled c)) (> (conflict-settled c) oldest)))
        (buffer-conflicts b))))

  (define (conflict-data b c)
    (datum:copy
      (list (conflict-revision c) (conflict-actor c) (conflict-labels c) (text:span->datum (conflict-span c))
            (conflict-mine c)
            ;; Disk means keep the text currently occupying this region.
            ;; A settled record's span is frozen in its historical text.
            (if (conflict-settled c) (conflict-disk c) (text:extract (buffer-text b) (conflict-span c))))))

  (define (data->conflict d)
    (make-conflict (car d) (cadr d) (caddr d) (text:datum->span (cadddr d)) (list-ref d 4) (list-ref d 5)
      (and (= (length d) 8) (list-ref d 6)) (and (= (length d) 8) (list-ref d 7))))

  (define (conflict-journal-data b c)
    (append (conflict-data b c) (list (conflict-settled c) (conflict-unsettled-by c))))

  (define (note-conflicts! id actor)
    ;; the pending conflicts changed: their count is a fact, and heads redraw on it
    (enqueue-event! `(property ,id conflicts ,actor)))

  (define (valid-conflict-data? d)
    (and (list? d) (memv (length d) '(6 8))
         (or (= (length d) 6)
             (for-all (lambda (r) (or (not r) (integer-at-least? r 1))) (list-tail d 6)))
         (integer-at-least? (car d) 1) (actor:identity? (cadr d)) (list? (caddr d))
         (guard (ex [else #f]) (text:datum->span (cadddr d)) #t)
         (list? (list-ref d 4)) (pair? (list-ref d 4)) (for-all string? (list-ref d 4))
         (list? (list-ref d 5)) (for-all string? (list-ref d 5))))

  (define (entry-at b revision)
    (or (find (lambda (entry) (= (entry-revision entry) revision)) (buffer-deltas b))
        (error 'reload! (format "no retained entry ~a" revision))))

  (define (newer? a b) (> (entry-revision a) (entry-revision b)))

  (define (baseline-revision b lines)
    ;; the newest retained revision whose text is lines, walking back through
    ;; the log by inverting each entry, or #f when the log does not reach it
    (let walk ([text (buffer-text b)] [revision (buffer-revision b)] [entries (buffer-deltas b)])
      (cond
        [(equal? text lines) revision]
        [(null? entries) #f]
        [else
         (let ([inverse (text:invert-delta (entry-delta (car entries)))])
           (let-values ([(previous d) (text:apply-edit text (text:delta-span inverse) (text:delta-inserted inverse))])
             (walk previous (- (entry-revision (car entries)) 1) (cdr entries))))])))

  (define token-bound 40000) ; tokens on both sides of a hunk beyond which one replacement stands for it

  (define (word-char? ch) (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))

  (define (blank-char? ch) (or (char=? ch #\space) (char=? ch #\tab)))

  (define (tokens lines from to)
    ;; the tokens of lines from..to as (values texts positions): runs of
    ;; word characters, runs of blanks, any other character alone, and a
    ;; line break after every line but the text's last, each with the
    ;; position it starts at
    (let ([last (- (vector-length lines) 1)])
      (let rows ([row from] [texts '()] [positions '()])
        (if (= row to)
            (values (list->vector (reverse texts)) (list->vector (reverse positions)))
            (let* ([line (vector-ref lines row)] [n (string-length line)])
              (let scan ([i 0] [texts texts] [positions positions])
                (if (= i n)
                    (if (= row last)
                        (rows (+ row 1) texts positions)
                        (rows (+ row 1) (cons "\n" texts) (cons (cons row n) positions)))
                    (let* ([ch (string-ref line i)]
                           [class (cond [(word-char? ch) word-char?] [(blank-char? ch) blank-char?] [else #f])]
                           [j (if class
                                  (let run ([j (+ i 1)]) (if (and (< j n) (class (string-ref line j))) (run (+ j 1)) j))
                                  (+ i 1))])
                      (scan j (cons (substring line i j) texts) (cons (cons row i) positions))))))))))

  (define (edits-between base target)
    ;; the changes from one text to another as (span . replacement), each in
    ;; the first text's coordinates and bottom-up, so they apply in turn
    ;; without shifting one another: the line diff's hunks, one with lines
    ;; on both sides refined token by token, the runs between the tokens a
    ;; patience diff matches each a replacement, so a word both texts keep
    ;; is never inside a change and a word changed is changed whole; one
    ;; with lines on one side only borrows a matched line, so a side left
    ;; without lines still reads as a text
    (define na (vector-length base))
    (define nb (vector-length target))
    (define (slice v from to)
      (let ([out (make-vector (- to from))])
        (let fill ([i from]) (when (< i to) (vector-set! out (- i from) (vector-ref v i)) (fill (+ i 1))))
        out))
    (define (hunk-end lines to)
      ;; the position just past lines ..to: the next line's start, or the last line's end
      (if (< to (vector-length lines)) (cons to 0) (let ([r (- to 1)]) (cons r (string-length (vector-ref lines r))))))
    (define (replacement alo ahi blo bhi)
      ;; the smallest single replacement taking the hunk's base lines to the target's
      (let-values ([(span rep) (text:difference (slice base alo ahi) (slice target blo bhi))])
        (let ([start (text:span-start span)] [end (text:span-end span)])
          (cons (text:make-span (+ (car start) alo) (cdr start) (+ (car end) alo) (cdr end)) rep))))
    (define (lent alo ahi blo bhi)
      ;; lines added or deleted, the hunk with lines on one side only, read
      ;; with the matched line above, or below at the top: a line break and
      ;; the lines at the end of the line above, or the lines and a break at
      ;; the start of the line below
      (if (> alo 0)
          (replacement (- alo 1) ahi (- blo 1) bhi)
          (replacement alo (min na (+ ahi 1)) blo (min nb (+ bhi 1)))))
    (define (token-edits alo ahi blo bhi)
      ;; the hunk's differences token by token, top-down: the runs between
      ;; the tokens the patience diff matches, each the base's run replaced
      ;; by the target's; a hunk beyond the token bound is one replacement
      (let-values ([(ta pa) (tokens base alo ahi)] [(tb pb) (tokens target blo bhi)])
        (if (> (+ (vector-length ta) (vector-length tb)) token-bound)
            (list (replacement alo ahi blo bhi))
            (let ([end-a (hunk-end base ahi)] [end-b (hunk-end target bhi)])
              (define (at positions texts end i) (if (< i (vector-length texts)) (vector-ref positions i) end))
              (define (between s e) (text:make-span (car s) (cdr s) (car e) (cdr e)))
              (let runs ([matches (diff:matches ta tb)] [i 0] [j 0] [edits '()])
                (let-values ([(mi mj rest) (if (null? matches)
                                               (values (vector-length ta) (vector-length tb) '())
                                               (values (caar matches) (cdar matches) (cdr matches)))])
                  (let ([edits (if (and (= i mi) (= j mj)) edits
                                   (cons (cons (between (at pa ta end-a i) (at pa ta end-a mi))
                                               (text:extract target (between (at pb tb end-b j) (at pb tb end-b mj))))
                                         edits))])
                    (if (null? matches) (reverse edits) (runs rest (+ mi 1) (+ mj 1) edits)))))))))
    (let loop ([matches (diff:matches base target)] [alo 0] [blo 0] [edits '()])
      (let-values ([(ahi bhi rest)
                    (if (null? matches) (values na nb '())
                        (values (caar matches) (cdar matches) (cdr matches)))])
        (let ([edits (cond [(and (= alo ahi) (= blo bhi)) edits]
                           [(or (= alo ahi) (= blo bhi)) (cons (lent alo ahi blo bhi) edits)]
                           [else (fold-left (lambda (acc e) (cons e acc)) edits (token-edits alo ahi blo bhi))])])
          (if (null? matches) edits (loop rest (+ ahi 1) (+ bhi 1) edits))))))

  (define (edit-deltas base edits)
    ;; the edits as deltas, each from the base text alone
    (map (lambda (edit) (let-values ([(t d) (text:apply-edit base (car edit) (cdr edit))]) d)) edits))

  (define (inserts-at? d position)
    ;; whether a delta inserts at a position without removing anything
    (let ([span (text:delta-span d)])
      (and (equal? (text:span-start span) position) (equal? (text:span-end span) position))))

  (define (opens-line? d)
    ;; whether a delta's inserted text begins with a line break, a line
    ;; added below the one it stands at
    (let ([lines (text:delta-inserted d)])
      (and (pair? lines) (pair? (cdr lines)) (string=? (car lines) ""))))

  (define (removes-break? d)
    ;; whether a delta's removed text begins with a line break
    (let ([lines (text:delta-removed d)])
      (and (pair? lines) (pair? (cdr lines)) (string=? (car lines) ""))))

  (define (inserts-nothing? d) (equal? (text:delta-inserted d) '("")))

  (define (last-char lines)
    ;; the last character of some lines, or #f when they end in a break or are empty
    (let ([line (list-ref lines (- (length lines) 1))])
      (and (> (string-length line) 0) (string-ref line (- (string-length line) 1)))))

  (define (first-char lines)
    (let ([line (car lines)]) (and (> (string-length line) 0) (string-ref line 0))))

  (define (glue? before after)
    ;; whether two texts placed one after the other fuse into one word
    (let ([a (and (pair? before) (last-char before))] [b (and (pair? after) (first-char after))])
      (and a b (word-char? a) (word-char? b))))

  (define (shared-insertion? a b)
    ;; One insertion already includes the other's text at an edge. Treat
    ;; it as one changed region, rather than concatenate another copy.
    (let* ([a (text:to-string (list->vector a) #f)] [b (text:to-string (list->vector b) #f)]
           [short (if (< (string-length a) (string-length b)) a b)]
           [long (if (< (string-length a) (string-length b)) b a)]
           [n (string-length short)] [m (string-length long)])
      (and (> n 0)
           (or (string=? short (substring long 0 n))
               (string=? short (substring long (- m n) m))))))

  (define (shared-change? local disk)
    ;; Replacing the same base region with the same tokens is shared work,
    ;; even when disk additionally inserts text among or after those tokens.
    ;; A deletion versus replacement, or an extended word, still conflicts.
    (and (equal? (text:span->datum (text:delta-span local)) (text:span->datum (text:delta-span disk)))
         (or (equal? (text:delta-inserted local) (text:delta-inserted disk))
             (and (not (inserts-nothing? local))
                  (for-all (lambda (edit) (text:span-empty? (car edit)))
                    (edits-between (list->vector (text:delta-inserted local))
                                   (list->vector (text:delta-inserted disk))))))))

  (define (colliding? c d joins?)
    ;; whether an entry inserting at a point and a disk delta meeting it
    ;; there cannot pass each other: both inserting the same text, one copy
    ;; enough; texts that would fuse into one word, whichever goes first;
    ;; a line the entry opens at a break the disk removes to join two
    ;; lines, which would run the entry's line into the joined one; or the
    ;; entry appending to text the disk deleted, a word or a line's end
    (let* ([point (text:span-start (text:delta-span c))]
           [span (text:delta-span d)] [start (text:span-start span)] [end (text:span-end span)]
           [c-text (text:delta-inserted c)] [d-text (text:delta-inserted d)])
      (and (inserts-at? c point)
           (cond
             [(inserts-at? d point)
              (or (equal? c-text d-text) (shared-insertion? c-text d-text)
                  (if (and (opens-line? d) (not (opens-line? c))) (glue? c-text d-text) (glue? d-text c-text)))]
             [(equal? start point)
              (or (shared-insertion? c-text d-text) (glue? c-text d-text)
                  (and joins? (opens-line? c) (removes-break? d) (not (opens-line? d))))]
             [(equal? end point)
              (if (inserts-nothing? d)
                  (let ([last (last-char (text:delta-removed d))]) (and last (not (blank-char? last)) (> (cdr point) 0)))
                  (or (shared-insertion? c-text d-text) (glue? d-text c-text)))]
             [else #f]))))

  (define (carry-chain cs ds joins)
    ;; two chains from one text carried across each other, the disk's first
    ;; where both insert at one point, unless the disk's opens a line there
    ;; and the entry's continues it, typing at a line's end staying on its
    ;; line: (values carried ds*), the entries' deltas in the disk's
    ;; coordinates and the disk's in the entries', or (values #f (index .
    ;; disk-index)) naming the first entry a disk delta overlaps or collides
    ;; with and which one; joins says of each disk delta whether its span
    ;; ends inside a line
    (let loop ([cs cs] [ds (map cons ds joins)] [i 0] [carried '()])
      (if (null? cs)
          (values (reverse carried) (map car ds))
          (let carry ([c (car cs)] [rest ds] [j 0] [moved '()])
            (cond
              [(null? rest) (loop (cdr cs) (reverse moved) (+ i 1) (cons c carried))]
              [(colliding? c (car (car rest)) (cdr (car rest))) (values #f (cons i j))]
              [else
               (let* ([d (car (car rest))]
                      [point (text:span-start (text:delta-span c))]
                      [continues? (and (inserts-at? c point) (inserts-at? d point) (opens-line? d) (not (opens-line? c)))]
                      [c2 (if continues? (text:rebase-delta c d 'stay) (text:rebase-delta c d))]
                      [d2 (if continues? (text:rebase-delta d c) (text:rebase-delta d c 'stay))])
                 (if (and c2 d2)
                     (carry c2 (cdr rest) (+ j 1) (cons (cons d2 (cdr (car rest))) moved))
                     (values #f (cons i j))))])))))

  (define (plan-disabling b actor basis targets origin-of)
    ;; the inverses disabling targets and every later entry depending on one,
    ;; planned together: (values steps targets) newest target first, or
    ;; (values #f conflicts) when an inverse fails for another cause
    ;; Region discovery walks the actual log and can name both halves of
    ;; a cancelled edit. Reload disables logical contributions, so select
    ;; targets and their dependencies in one stable projected chain. Only
    ;; project since the baseline: undoing a saved edit is a new change.
    (let* ([chain (effective-chain (log-since b basis))]
           [live (map entry-revision chain)])
      (let grow ([targets (filter (lambda (e) (exists (lambda (t) (= (entry-revision t) (entry-revision e))) targets)) (reverse chain))])
        (let-values ([(steps conflicts) (plan-inversion b actor targets origin-of #f chain)])
          (let ([dependents (filter (lambda (c) (and (memv (cdr c) live) (not (exists (lambda (t) (= (entry-revision t) (cdr c))) targets))))
                                    conflicts)])
            (cond
              [(null? conflicts) (values steps targets)]
              [(null? dependents) (values #f conflicts)]
              [else (grow (filter (lambda (e)
                                    (or (exists (lambda (t) (= (entry-revision t) (entry-revision e))) targets)
                                        (exists (lambda (c) (= (cdr c) (entry-revision e))) dependents)))
                            (reverse chain)))]))))))

  (define (install-inverses! b id actor steps visit)
    ;; Every target and its conflict metadata must survive the complete
    ;; operation, even if an earlier inverse would normally evict it.
    (let ([limit (log-retention)])
      (parameterize ([log-retention (+ limit (length steps))])
        (for-each
          (lambda (step)
            (install-edit! b id actor (car step) (cadr step) (caddr step) (cadddr step) '() '())
            (visit (car (buffer-deltas b))))
          steps))
      (buffer-deltas-set! b (bounded (buffer-deltas b) limit))
      (buffer-conflicts-set! b (retained-conflicts b))))

  (define (current-span-of b entry)
    ;; an entry's written span carried into the current text, as blame does
    (let* ([d (entry-delta entry)]
           [written (let ([s (text:span-start (text:delta-span d))] [e (text:delta-new-end d)])
                      (text:make-span (car s) (cdr s) (car e) (cdr e)))]
           [later (let take ([entries (buffer-deltas b)] [acc '()])
                    (cond [(null? entries) acc]
                          [(> (entry-revision (car entries)) (entry-revision entry)) (take (cdr entries) (cons (entry-delta (car entries)) acc))]
                          [else acc]))])
      (fold-left rebase-mark-value written later)))

  (define (span-union spans)
    (let loop ([spans (cdr spans)] [start (text:span-start (car spans))] [end (text:span-end (car spans))])
      (if (null? spans) (text:make-span (car start) (cdr start) (car end) (cdr end))
          (let ([s (text:span-start (car spans))] [e (text:span-end (car spans))])
            (loop (cdr spans) (if (text:position<? s start) s start) (if (text:position<? end e) e end))))))

  (define (entry-batch entry)
    ;; the batch label an entry carries, or #f
    (cond [(assq 'batch (entry-labels entry)) => cdr] [else #f]))

  (define (spans-touch? a b)
    ;; whether two spans overlap or share an end
    (and (not (text:position<? (text:span-end a) (text:span-start b)))
         (not (text:position<? (text:span-end b) (text:span-start a)))))

  (define (batch-mates b chain blocker)
    ;; the blocker with the entries of its batch whose text adjoins its own
    ;; in the current text, transitively: a replacement typed as a deletion
    ;; and an insertion conflicts whole, both sides shown, while entries
    ;; apart, the occurrences of one replacement say, pend one by one
    (let ([batch (entry-batch blocker)])
      (if (not batch) (list blocker)
          (let grow ([members (list blocker)] [region (current-span-of b blocker)]
                     [others (filter (lambda (e) (and (not (= (entry-revision e) (entry-revision blocker))) (equal? (entry-batch e) batch)))
                                     (map (lambda (e) (entry-at b (entry-revision e))) chain))])
            (let-values ([(joining apart) (partition (lambda (e) (spans-touch? (current-span-of b e) region)) others)])
              (if (null? joining) members
                  (grow (append members joining)
                        (span-union (cons region (map (lambda (e) (current-span-of b e)) joining)))
                        apart)))))))

  (define (disk-mates b basis chain blocker disk joins?)
    ;; Collect the complete local image of the disk's disputed region
    ;; before disabling any of it. Otherwise a first inversion can strand
    ;; an adjacent contributor on the wrong side of an inverse chain.
    (let* ([base (let-values ([(lines trailing?) (text:from-string (property-value b 'base #f))]) lines)]
           [region
            (let loop ([entries (log-since b basis)] [lines base] [region (text:delta-span disk)])
              (if (null? entries) region
                  (let* ([d (entry-delta (car entries))]
                         [proposed (text:datum->delta
                                     (list (text:span->datum region) (text:extract lines region) (text:delta-inserted disk)))])
                    (let-values ([(next ignored) (text:apply-edit lines (text:delta-span d) (text:delta-inserted d))])
                      (loop (cdr entries) next
                        (if (colliding? d proposed joins?) (absorb-region region d) (carry-region region d)))))))]
           [members (append (batch-mates b chain blocker) (region-entries b basis region))]
           [live (map entry-revision chain)])
      (filter (lambda (e) (and (memv (entry-revision e) live) (memq e members))) (buffer-deltas b))))

  (define (settle-overlaps! b id actor basis ds joins)
    ;; the entries since the basis a disk delta overlaps, disabled one at a
    ;; time with their batch mates and dependents until the whole effective
    ;; chain carries: (blocker disk-index inverse-revisions) per conflict,
    ;; newest first, the index that of the disk delta the blocker met and
    ;; the revisions those of the inverses installed for it
    (let* ([base (let-values ([(ls trailing?) (text:from-string (property-value b 'base #f))]) ls)]
           [local (edit-deltas base (edits-between base (buffer-text b)))]
           [region-of (lambda (d) (fold-left carry-region (result-span d) (cdr (memq d local))))]
           [same (filter (lambda (d) (exists (lambda (disk) (shared-change? d disk)) ds)) local)]
           [remainder (fold-left (lambda (lines d)
                                   (let-values ([(next ignored) (text:apply-edit lines (text:delta-span d) (text:delta-inserted d))]) next))
                        base (remp (lambda (d) (memq d same)) local))]
           [shared-disk (let find ([rest ds] [i 0])
                          (cond [(null? rest) #f]
                                [(exists (lambda (d) (shared-change? d (car rest))) same) i]
                                [else (find (cdr rest) (+ i 1))]))]
           [seeds
            (if (= (length same) (length local)) (effective-chain (log-since b basis))
              (apply append
                (map (lambda (d)
                       (let ([region (region-of d)])
                         (region-entries b basis region))) same)))])
      (let settle ([disabled '()] [seeds seeds])
        (let* ([since (or (log-since b basis) (error 'reload! "the log no longer reaches the baseline" basis))]
               [chain (effective-chain since)]
               [net (edit-deltas base (edits-between base (buffer-text b)))]
               [net-blocker
                (let-values ([(carried outcome) (carry-chain net ds joins)])
                  (and (not carried)
                    (let* ([region (fold-left absorb-region (text:delta-span (list-ref ds (cdr outcome))) net)]
                           [members (region-entries b basis region)]
                           [members (filter (lambda (e) (exists (lambda (c) (= (entry-revision c) (entry-revision e))) chain)) members)])
                      (and (pair? members) (cons members (cdr outcome))))))])
          (let-values ([(carried outcome) (carry-chain (map entry-delta chain) ds joins)])
            (if (and carried (null? seeds) (not net-blocker))
              disabled
              (let* ([blocker (cond [(pair? seeds) (car seeds)] [net-blocker (caar net-blocker)]
                                    [else (entry-at b (entry-revision (list-ref chain (car outcome))))])]
                     [first-revision (+ (buffer-revision b) 1)])
                (let-values ([(steps targets)
                              (plan-disabling b actor basis (if (pair? seeds) (let ([revisions (map entry-revision seeds)])
                                                                                (filter (lambda (e) (memv (entry-revision e) revisions)) (buffer-deltas b)))
                                                              (if net-blocker (car net-blocker)
                                                                (disk-mates b basis chain blocker (list-ref ds (cdr outcome)) (list-ref joins (cdr outcome)))))
                                (lambda (target) (list 'reload actor first-revision (entry-revision target))))])
                  (unless steps
                    (if (exists (lambda (c) (eq? (cdr c) 'conflict-changed)) targets)
                        (raise (make-conflict-history))
                        (error 'reload! "an entry a disk change overlaps cannot be disabled" targets)))
                  (install-inverses! b id actor steps (lambda (entry) (void)))
                  ;; A single keystroke may also contain changes that disk
                  ;; did not make. Preserve those as Mine rather than silently
                  ;; discarding the whole entry along with its shared part.
                  (settle (if (and (pair? seeds) (equal? (buffer-text b) remainder)) disabled
                            (cons (list blocker (if (pair? seeds) shared-disk (if net-blocker (cdr net-blocker) (cdr outcome)))
                                        (let count ([r (buffer-revision b)] [acc '()])
                                          (if (< r first-revision) acc (count (- r 1) (cons r acc)))))
                                  disabled)) '())))))))))

  (define (region-entries b basis region)
    ;; Follow a net change backwards through its keystrokes. The footprint
    ;; grows over contributing edits; deleted text participates as a point.
    (let loop ([entries (reverse (log-since b basis))] [region region] [found '()])
      (if (null? entries) found
          (let* ([e (car entries)] [d (entry-delta e)]
                 [written (result-span d)]
                 [hit? (spans-touch? region written)])
            (loop (cdr entries)
              (if hit? (absorb-region region (text:invert-delta d))
                  (rebase-mark-value region (text:invert-delta d)))
              (if hit? (cons e found) found))))))

  (define (spans-overlap? a b)
    ;; whether two spans share content: a point strictly inside the other
    ;; counts, a shared end alone does not
    (let ([as (text:span-start a)] [ae (text:span-end a)] [bs (text:span-start b)] [be (text:span-end b)])
      (cond
        [(and (equal? as ae) (equal? bs be)) #f]
        [(equal? as ae) (and (text:position<? bs as) (text:position<? as be))]
        [(equal? bs be) (and (text:position<? as bs) (text:position<? bs ae))]
        [else (and (text:position<? as be) (text:position<? bs ae))])))

  (define (widen-over region ds)
    ;; the region grown over the deltas sharing content with it, until none does
    (let grow ([region region] [ds ds])
      (let-values ([(overlapping apart) (partition (lambda (d) (spans-overlap? region (text:delta-span d))) ds)])
        (if (null? overlapping) region
            (grow (span-union (cons region (map text:delta-span overlapping))) apart)))))

  (define (absorb-region region d)
    ;; a region carried across an edit of its own side: moved past it and
    ;; widened over what it wrote, an insertion at the region's edge included
    (if (spans-touch? region (text:delta-span d))
        (let ([start (text:rebase-position (text:span-start region) d 'stay)]
              [end (text:rebase-position (text:span-end region) d)])
          (span-union (list (text:make-span (car start) (cdr start) (car end) (cdr end)) (result-span d))))
        (rebase-mark-value region d)))

  (define (conflict-image before region previous)
    ;; Compose disjoint pending alternatives in the coordinates of before.
    (let apply-old ([rest (list-sort (lambda (a b) (text:position<? (text:span-start (cdr b)) (text:span-start (cdr a))))
                            (filter (lambda (p) (spans-touch? region (cdr p))) previous))]
                    [text before] [region region])
      (if (null? rest) (text:extract text region)
          (let-values ([(next d) (text:apply-edit text (cdar rest) (caar rest))])
            (apply-old (cdr rest) next (absorb-region region d))))))

  (define (conflict-groups items)
    ;; Items begin with a source and region. Join touching regions before
    ;; reading either alternative, retaining every source in the group.
    (fold-left
      (lambda (groups item)
        (let grow ([members (list item)] [region (cadr item)] [rest groups])
          (let-values ([(touching apart) (partition (lambda (g) (spans-touch? region (car g))) rest)])
            (if (null? touching) (cons (cons region members) apart)
                (grow (append members (apply append (map cdr touching)))
                      (span-union (cons region (map car touching))) apart)))))
      '() items))

  (define (carry-conflicts! b before steps)
    ;; A replacement may join several pending regions or consume their
    ;; surroundings. Preserve their complete pre-edit Mine image together,
    ;; rather than leave overlapping choices or clip them like cursor marks.
    (unless (null? (unsettled-conflicts b))
      (let carry ([before before] [steps steps])
        (unless (null? steps)
          (let* ([d (car steps)] [pending (unsettled-conflicts b)]
                 [previous (map (lambda (c) (cons (conflict-mine c) (conflict-span c))) pending)]
                 [groups (conflict-groups (map (lambda (c) (list c (carry-region (conflict-span c) d))) pending))])
            (for-each
              (lambda (g)
                (let* ([members (list-sort (lambda (a b) (> (conflict-revision a) (conflict-revision b))) (map car (cdr g)))]
                       [c (car members)] [region (car g)])
                  (unless (and (null? (cdr members)) (text:rebase-span (conflict-span c) d))
                    (conflict-mine-set! c
                      (conflict-image before
                        ;; Read the consumed input directly. Mapping an
                        ;; empty output backwards can omit a deleted
                        ;; separator or newline at the region's boundary.
                        (span-union (append (map conflict-span members)
                                      (if (exists (lambda (c) (not (text:rebase-span (conflict-span c) d))) members)
                                          (list (text:delta-span d)) '())))
                        previous)))
                  (conflict-span-set! c region)
                  (when (pair? (cdr members))
                    (conflict-unsettled-by-set! c #f)
                    (buffer-conflicts-set! b (remp (lambda (old) (memq old (cdr members))) (buffer-conflicts b))))))
              groups)
            (let-values ([(next ignored) (text:apply-edit before (text:delta-span d) (text:delta-inserted d))])
              (carry next (cdr steps))))))))

  (define-condition-type &conflict-history &condition make-conflict-history conflict-history?)

  (define (text-before-entry b entry)
    (let walk ([text (buffer-text b)] [entries (buffer-deltas b)])
      (let* ([e (car entries)] [d (text:invert-delta (entry-delta e))])
        (let-values ([(prior ignored) (text:apply-edit text (text:delta-span d) (text:delta-inserted d))])
          (if (= (entry-revision e) (entry-revision entry)) prior (walk prior (cdr entries)))))))

  (define (carry-conflict-data b data before steps)
    ;; Historical alternatives use the same composition as live edits.
    (let ([copy (copy-buffer b)])
      (buffer-conflicts-set! copy (map data->conflict data))
      (carry-conflicts! copy before steps)
      (buffer-text-set! copy
        (fold-left (lambda (text d)
                     (let-values ([(next ignored) (text:apply-edit text (text:delta-span d) (text:delta-inserted d))]) next))
          before steps))
      (map (lambda (c) (conflict-journal-data copy c)) (buffer-conflicts copy))))

  (define (restore-conflict-change b target inverse between)
    ;; Replay foreign edits on both historical images, commuting them onto
    ;; the inverse's result for Before. This restores alternatives and group
    ;; membership as well as regions, including changes outside the target
    ;; which a later edit absorbed into Mine. Explicitly settled groups stay
    ;; settled; an incompatible current group refuses before publication.
    (let ([change (entry-conflicts target)])
      (and change
           (let* ([prior (text-before-entry b target)]
                  [d (entry-delta target)]
                  [after-text (let-values ([(next ignored) (text:apply-edit prior (text:delta-span d) (text:delta-inserted d))]) next)]
                  [before (carry-conflict-data b (car change) prior (commute-inverse d inverse between))]
                  [after (carry-conflict-data b (cadr change) after-text between)]
                  [pending (unsettled-conflicts b)]
                  [groups (conflict-groups
                            (append (map (lambda (c)
                                           (let ([span (carry-region (text:datum->span (cadddr c)) (text:invert-delta inverse))]
                                                 [next (assv (car c) after)])
                                             (list (car c) (if next (span-union (list span (text:datum->span (cadddr next)))) span)))) before)
                              (map (lambda (c) (list (car c) (text:datum->span (cadddr c))))
                                (filter (lambda (c) (not (assv (car c) before))) after))))]
                  [restored '()] [removed '()])
             (for-each
               (lambda (group)
                 (let* ([ids (map car (cdr group))]
                        [expected (filter (lambda (c) (memv (car c) ids)) after)]
                        [live (filter (lambda (c) (memv (conflict-revision c) ids)) pending)])
                   ;; A missing ID can also have joined another region,
                   ;; including in older, partial recovery snapshots.
                   (when (exists (lambda (c) (and (not (memv (conflict-revision c) ids))
                                                  (spans-touch? (conflict-span c) (car group)))) pending)
                     (raise (make-conflict-history)))
                   ;; An empty expected image also permits redo to recreate
                   ;; conflicts removed by undoing their reload.
                   (when (or (pair? live) (null? expected))
                     (unless (and (= (length live) (length expected))
                                  (for-all (lambda (c) (member (conflict-data b c) (map (lambda (c) (list-head c 6)) expected))) live))
                       (raise (make-conflict-history)))
                     (set! restored (append (map data->conflict (filter (lambda (c) (memv (car c) ids)) before)) restored))
                     (set! removed (append ids removed))))) groups)
             (cons restored removed)))))

  (define (move-conflict-change entry delta)
    ;; Reload and history projection move both sides of an entry's metadata
    ;; with that entry, just as they move its text delta.
    (let ([change (entry-conflicts entry)])
      (and change
           (map (lambda (side old actual)
                  (map (lambda (data)
                         (append (list-head data 3)
                           (list (text:span->datum (inverse-result-span (text:datum->span (cadddr data)) old actual '())))
                           (list-tail data 4))) side))
             change (list (entry-delta entry) (text:invert-delta (entry-delta entry)))
             (list (text:invert-delta delta) delta)))))

  (define (reload-conflict-change b entry before after mapping)
    ;; An alternative may extend beyond the entry's own delta. Carry its
    ;; journal images across the complete disk change at that historical
    ;; point, not just the displacement of the entry's replacement.
    (and (entry-conflicts entry)
         (let* ([old-before (text-before-entry b entry)]
                [old-after (let ([d (entry-delta entry)])
                             (let-values ([(next ignored) (text:apply-edit old-before (text:delta-span d) (text:delta-inserted d))]) next))])
           (map (lambda (side old new)
                  (map (lambda (c)
                         (let* ([r (list-ref c 7)]
                                [old (and r (find (lambda (e) (= (entry-revision e) r)) (buffer-deltas b)))]
                                [new (and old (hashtable-ref mapping old #f))])
                           (append (list-head c 7) (list (and new (entry-revision new))))))
                       (carry-conflict-data b side old (edit-deltas old (edits-between old new)))))
             (entry-conflicts entry) (list old-before old-after) (list before after)))))

  (define (pending-conflicts b before previous disabled ds* start)
    ;; the conflicts as (source mine region disk-index),
    ;; newest first: each one region of the text with the disabled entries
    ;; inverted, the footprint of a conflict's inverses with the span of the
    ;; disk delta the blocker met, widened over the disk deltas sharing
    ;; content with it, whose two images are the sides; the disk's
    ;; is read where the disk's deltas leave the region, and mine is the
    ;; text before any inversion over the region carried back through every
    ;; inverse, those of this conflict absorbing it, so keeping mine writes
    ;; back exactly what this side had there
    ;; Older alternatives participate in the same region grouping. The live
    ;; text contains their Disk side; compose Mine from their frozen spans
    ;; in the pre-reload text before replacing any of those records.
    (define (revision source)
      (if (conflict? source) (conflict-revision source) (entry-revision source)))
    (let* ([inverses (filter (lambda (e) (> (entry-revision e) start)) (buffer-deltas b))])
      (let* ([regions
              (append (map (lambda (conflict)
                             (let* ([blocker (car conflict)]
                                    [met (cadr conflict)]
                                    [own (map (lambda (r) (entry-at b r)) (caddr conflict))]
                                    [region (widen-over (span-union (cons (text:delta-span (list-ref ds* met)) (map (lambda (e) (current-span-of b e)) own))) ds*)])
                               (list blocker region met)))
                        disabled)
                (map (lambda (c) (list c (widen-over (conflict-span c) ds*) #f)) (unsettled-conflicts b)))]
             [merged (conflict-groups regions)])
        (map (lambda (group)
               (let* ([items (list-sort (lambda (a b) (> (revision (car a)) (revision (car b)))) (cdr group))]
                      [blocker (caar items)] [region (car group)]
                      [mine (fold-left (lambda (r e) (absorb-region r (text:invert-delta (entry-delta e)))) region inverses)])
                 (list blocker (conflict-image before mine previous) region (exists caddr items)))) (reverse merged)))))

  (define (result-span d)
    ;; the region a delta's replacement occupies in the text after it
    (let ([s (text:span-start (text:delta-span d))] [e (text:delta-new-end d)])
      (text:make-span (car s) (cdr s) (car e) (cdr e))))

  (define (carry-region region d)
    ;; a conflict's region carried across a disk change: moved past a
    ;; disjoint one, widened over one it overlaps to cover its result too
    (or (text:rebase-span region d)
        (let ([start (text:rebase-position (text:span-start region) d 'stay)]
              [end (text:rebase-position (text:span-end region) d)])
          (span-union (list (text:make-span (car start) (cdr start) (car end) (cdr end)) (result-span d))))))

  (define (reload-buffer b)
    ;; Merge history is a projection onto the observed disk baseline.
    ;; The actual log stays chronological, including every reload. Later
    ;; ordinary edits extend either history with the same deltas and IDs.
    ;; If its anchor expired, the actual history may still reach a newer
    ;; occurrence of the baseline, including the current text itself.
    (let* ([p (buffer-reload-log b)] [copy (copy-buffer b)]
           [later (and p (equal? (car p) (property-value b 'base #f)) (log-since b (cadr p)))])
      (when later
        (buffer-deltas-set! copy (append (reverse later) (caddr p)))
        ;; Settled spans live in their entry's historical text.
        ;; Pending spans already describe the current text.
        (for-each
          (lambda (c)
            (let* ([r (conflict-settled c)]
                   [old (and r (find (lambda (e) (= (entry-revision e) r)) (buffer-deltas b)))]
                   [new (and old (find (lambda (e) (= (entry-revision e) r)) (caddr p)))])
              (when new
                (conflict-span-set! c
                                    (inverse-result-span (conflict-span c) (entry-delta old)
                                      (text:invert-delta (entry-delta new)) '())))))
          (buffer-conflicts copy)))
      copy))

  (define (rebaseline! b disk chain carried ds* pending)
    ;; Build the next merge projection, preserving entry IDs. This is
    ;; private planning state; it never replaces the chronological log or
    ;; the undo groups in the published buffer.
    (let ([mapping (make-eq-hashtable)])
      (let-values ([(text entries)
                    (let apply-all ([chain chain] [carried carried] [text disk] [entries '()])
                      (if (null? chain) (values text entries)
                          (let ([old (car chain)] [d (car carried)])
                            (let-values ([(next delta) (text:apply-edit text (text:delta-span d) (text:delta-inserted d))])
                              (unless (equal? (text:delta-removed delta) (text:delta-removed d))
                                (error 'reload! "a carried entry no longer matches the text" (entry-revision old)))
                              (let ([entry (make-entry (entry-revision old) (entry-actor old) delta (entry-origin old)
                                             (entry-facts old) (entry-labels old)
                                             (reload-conflict-change b (entry-at b (entry-revision old)) text next mapping))])
                                (hashtable-set! mapping (entry-at b (entry-revision old)) entry)
                                (apply-all (cdr chain) (cdr carried) next (cons entry entries)))))))])
        (let ([check (fold-left (lambda (t d)
                                  (let-values ([(next ignored) (text:apply-edit t (text:delta-span d) (text:delta-inserted d))]) next))
                       (buffer-text b) ds*)])
          (unless (equal? check text) (error 'reload! "the disk's changes and the log disagree")))
        (buffer-text-set! b text)
        (buffer-deltas-set! b entries)
        (let ([fresh
               (filter (lambda (c) (not (equal? (conflict-mine c) (conflict-disk c))))
                 (map (lambda (p)
                        (let* ([source (car p)]
                               [region (let carry ([ds ds*] [k 0] [region (caddr p)])
                                         (cond [(null? ds) region]
                                               [(eqv? k (cadddr p)) (carry (cdr ds) (+ k 1) (absorb-region region (car ds)))]
                                               [else (carry (cdr ds) (+ k 1) (carry-region region (car ds)))]))])
                          (if (conflict? source)
                              (make-conflict (conflict-revision source) (conflict-actor source) (conflict-labels source)
                                region (cadr p) (text:extract text region) #f (conflict-unsettled-by source))
                              (make-conflict (entry-revision source) (entry-actor source) (entry-labels source)
                                region (cadr p) (text:extract text region) #f #f)))) pending))])
          (buffer-conflicts-set! b
            (append (list-sort (lambda (a b) (> (conflict-revision a) (conflict-revision b))) fresh)
              (filter conflict-settled (buffer-conflicts b))))
          (unsettled-conflicts b)))))

  (define (pending-edit-revisions b basis)
    ;; Two alternatives cannot represent an older unresolved Mine, manual
    ;; edits to its standing side, and a third overlapping disk version.
    ;; Track the live edits that would need a third alternative if disabled.
    (let ([pending (filter (lambda (c) (not (equal? (conflict-mine c) (text:extract (buffer-text b) (conflict-span c)))))
                     (unsettled-conflicts b))])
      (if (null? pending) '()
          (map entry-revision
            (filter (lambda (e)
                      (let* ([actual (entry-at b (entry-revision e))] [d (entry-delta actual)])
                        (and (not (equal? (text:delta-removed d) (text:delta-inserted d)))
                             (exists (lambda (c) (spans-touch? (conflict-span c) (current-span-of b actual))) pending))))
              (effective-chain (log-since b basis)))))))

  (define (plan-reload! b id actor disk)
    (let* ([base (property-value b 'base #f)]
           [base-lines (and (string? base) (let-values ([(lines trailing?) (text:from-string base)]) lines))]
           [basis (and base-lines (baseline-revision b base-lines))])
      (cond
        [(not base-lines) (values 'refused 'no-base)]
        [(not basis) (values 'refused 'basis-too-old)]
        [else
         (let* ([ds (edit-deltas base-lines (edits-between base-lines disk))]
                [joins (map (lambda (d)
                              (let ([end (text:span-end (text:delta-span d))])
                                (< (cdr end) (string-length (vector-ref base-lines (car end)))))) ds)]
                [before (buffer-text b)]
                [previous (map (lambda (c) (cons (conflict-mine c) (conflict-span c))) (unsettled-conflicts b))]
                [start (buffer-revision b)]
                [protected (pending-edit-revisions b basis)]
                [disabled (if (null? ds) '() (settle-overlaps! b id actor basis ds joins))]
                [unsafe? (exists (lambda (group)
                                   (exists (lambda (r) (memv (cadddr (entry-origin (entry-at b r))) protected)) (caddr group))) disabled)]
                [inverses (reverse (filter (lambda (e) (> (entry-revision e) start)) (buffer-deltas b)))])
           (cond
             [unsafe? (values 'refused 'pending-edits)]
             [(null? ds) (values 'applied '())]
             [else
              (let ([chain (effective-chain (log-since b basis))])
                (let-values ([(carried ds*) (carry-chain (map entry-delta chain) ds joins)])
                  (rebaseline! b disk chain carried ds*
                    (pending-conflicts b before previous disabled ds* start))
                  (values 'applied
                    (append (map (lambda (e) (list (entry-delta e) (entry-origin e) (entry-facts e))) inverses)
                      (map (lambda (d) (list d #f '())) ds*)))))]))])))

  (define (commit-reload! b id actor plan steps updates)
    ;; Publish the actual inversions and disk edits as one normal action.
    ;; The observed baseline is a committed fact, not an undo fact: undo
    ;; restores the buffer, and a later save can overwrite that disk version.
    (let* ([start (buffer-revision b)] [key (list 'reload (+ start 1))]
           [context (list key "reload")] [labels (list (cons 'batch (cons actor key)))]
           [before (map (lambda (c) (conflict-journal-data b c)) (unsettled-conflicts b))]
           [pending (unsettled-conflicts plan)]
           [after (map (lambda (c) (conflict-journal-data plan c)) pending)]
           [base (property-value b 'base #f)]
           [trailing (assq 'trailing updates)]
           [trailing (and trailing
                          (eq? (property-value b 'trailing #t)
                               (let-values ([(ls trailing?) (text:from-string base)]) trailing?))
                          trailing)])
      (for-each
        (lambda (step)
          (let ([d (car step)])
            (let-values ([(next actual) (text:apply-edit (buffer-text b) (text:delta-span d) (text:delta-inserted d))])
              (unless (equal? (text:delta-removed actual) (text:delta-removed d))
                (error 'reload! "planned edit no longer matches the buffer"))
              (install-edit! b id actor next actual (cadr step) (caddr step) '() labels)
              (remember-edit! b actor context)))) steps)
      (when (or (and trailing (not (eq? (cdr trailing) (property-value b 'trailing #t))))
                (and (null? steps) (not (equal? before after))))
        (apply-locked! b id actor (text:make-span 0 0 0 0) '("") #f
          (if trailing (list trailing) '()) '() labels)
        (remember-edit! b actor context))
      (when (> (buffer-revision b) start)
        (let* ([entry (car (buffer-deltas b))] [change (entry-conflicts entry)]
               [previous (if change (car change) '())])
          (vector-set! entry 6 (and (or (pair? previous) (pair? after)) (list previous after))))
        (buffer-conflicts-set! b (append pending (filter conflict-settled (buffer-conflicts b))))
        (when (or (pair? before) (pair? after)) (note-conflicts! id actor)))
      (install-properties! b (remp (lambda (p) (eq? (car p) 'trailing)) updates))
      (refresh-edit-facts! b #f)
      (for-each (lambda (p) (enqueue-event! `(property ,id ,(car p) ,actor))) updates)
      (buffer-reload-log-set! b
        (list (property-value b 'base #f) (buffer-revision b) (buffer-deltas plan)))
      (values 'applied (list (buffer-revision b) (map (lambda (c) (conflict-data b c)) pending)))))

  (edoc "Merge a buffer with its file as one undoable reload action. The disk becomes the observed baseline, the buffer's edits merge on top, and overlaps keep Mine and Disk alternatives. Undo restores the pre-reload text and conflicts without forgetting that disk baseline, so saving can write the restored version; earlier edits remain undoable. Returns (values applied (revision conflicts)), or refused pending-edits, no-base, basis-too-old, or a write refusal."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (lines (or list vector) "the disk's lines")
        (facts list "the facts to commit: base, stamp, trailing")
        (access* (list-of any) "write access, at most one"))
  (define (reload! actor id lines facts . access*)
    (unless (<= (length access*) 1) (error 'reload! "expected one write access" access*))
    (let ([disk (text:normalize lines)] [updates (datum:copy (writable-properties facts))]
          [access (own-write-access (and (pair? access*) (car access*)))])
      (transact! actor
        (lambda (actor)
          (cond
            [(write-refusal id access) => (lambda (reason) (values 'refused reason))]
            [else
             (guard (ex [(conflict-history? ex) (values 'refused 'pending-edits)])
               (plan-buffer! id
                 (lambda (b)
                   (let ([plan (reload-buffer b)])
                     (let-values ([(status steps)
                                   (parameterize ([planned-events (box '())] [projecting-reload? #t])
                                     (plan-reload! plan id actor disk))])
                       (if (eq? status 'refused) (values status steps)
                           (commit-reload! b id actor plan steps updates)))))))])))))

  (edoc "A buffer's pending reload conflicts, newest first, (revision actor labels region mine disk) each: the disabled entry's revision, actor and labels, the region its disk change occupies in the current text, the lines the entry's side left there, and the current lines that choosing disk keeps."
        (id integer "the buffer id")
        (returns list))
  (define (conflicts id)
    (locked (lambda () (let ([b (buffer-of 'conflicts id)]) (map (lambda (c) (conflict-data b c)) (unsettled-conflicts b))))))

  (edoc "Read text, revision and pending reload conflicts together: (values text revision conflicts). Conflict regions and Disk alternatives describe exactly this text, even if another actor edits before the caller renders it."
        (id integer "the buffer id")
        (returns any "(values text revision conflicts)"))
  (define (conflict-state id)
    (locked (lambda ()
              (let ([b (buffer-of 'conflict-state id)])
                (values (buffer-text b) (buffer-revision b)
                  (map (lambda (c) (conflict-data b c)) (unsettled-conflicts b)))))))

  (edoc "Settle a pending reload conflict: disk keeps the disk's lines and drops the pending mark; mine writes the entry's side over the disk's region, replacement lines write those, either the actor's undoable edit labelled (conflict . revision): (values applied revision), refused no-conflict or a write refusal."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (revision integer "the conflicted entry's revision, as conflicts lists it")
        (choice any "disk, mine or the replacement lines")
        (access* (list-of any) "write access, at most one"))
  (define (resolve! actor id revision choice . access*)
    (unless (<= (length access*) 1) (error 'resolve! "expected one write access" access*))
    (unless (or (memq choice '(disk mine)) (and (list? choice) (pair? choice) (for-all string? choice)))
      (error 'resolve! "expected disk, mine or replacement lines" choice))
    (let ([choice (datum:copy choice)]
          [access (own-write-access (and (pair? access*) (car access*)))])
      (transact! actor
        (lambda (actor)
          (cond
            [(write-refusal id access) => (lambda (reason) (values 'refused reason))]
            [else
             (resolve-locked! (buffer-of 'resolve! id) id actor revision choice)])))))

  (define (resolve-locked! b id actor revision choice)
    (let ([c (find (lambda (c) (= (conflict-revision c) revision)) (unsettled-conflicts b))])
      (cond
        [(not c) (values 'refused 'no-conflict)]
        [(eq? choice 'disk)
         ;; the disk's side stands already: the mark goes, nothing to undo
         (buffer-conflicts-set! b (remq c (buffer-conflicts b)))
         (refresh-edit-facts! b #f)
         (note-conflicts! id actor)
         (values 'applied (buffer-revision b))]
        [else
         (let* ([lines (if (eq? choice 'mine) (conflict-mine c) choice)]
                [span (conflict-span c)])
           (let-values ([(new-revision delta)
                         (apply-locked! b id actor span lines #f '() '() (list (cons 'conflict revision)) (list c))])
             (remember-edit! b actor (list (list 'resolve new-revision) (if (eq? choice 'mine) "keep mine" "resolve conflict")))
             (note-conflicts! id actor)
             (values 'applied new-revision)))])))

  (edoc "Settle a reviewed conflict set atomically: Mine for the selected revisions, Disk for the rest. Expected is the complete conflicts snapshot shown to the caller. Refused conflict-changed leaves everything intact if any alternative or region changed; applied returns the new revision."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (expected list "the complete reviewed conflict records")
        (mine (list-of integer) "the revisions picked Mine")
        (access* (list-of any) "write access, at most one"))
  (define (resolve-picks! actor id expected mine . access*)
    (unless (and (list? expected) (for-all valid-conflict-data? expected)
                 (list? mine) (for-all (lambda (r) (and (integer? r) (assv r expected))) mine)
                 (<= (length access*) 1))
      (error 'resolve-picks! "expected a conflict snapshot, selected revisions and at most one write access"))
    (let ([expected (datum:copy expected)] [mine (datum:copy mine)]
          [access (own-write-access (and (pair? access*) (car access*)))])
      (transact! actor
        (lambda (actor)
          (cond
            [(write-refusal id access) => (lambda (reason) (values 'refused reason))]
            [else
             (plan-buffer! id
               (lambda (b)
                 (cond
                   [(not (equal? expected (map (lambda (c) (conflict-data b c)) (unsettled-conflicts b))))
                    (values 'refused 'conflict-changed)]
                   [(let ([chosen (filter (lambda (c) (memv (car c) mine)) expected)])
                      (exists (lambda (a)
                                (exists (lambda (other)
                                          (and (not (eq? a other))
                                               (or (equal? (cadddr a) (cadddr other))
                                                   (spans-overlap? (text:datum->span (cadddr a)) (text:datum->span (cadddr other)))))) chosen)) chosen))
                    (values 'refused 'overlap)]
                   [else
                    ;; Bottom-up keeps all remaining regions independent of
                    ;; replacements below them. Notices leave only on commit.
                    (let settle ([rest (list-sort (lambda (a b)
                                                    (text:position<? (text:span-start (text:datum->span (cadddr b)))
                                                                     (text:span-start (text:datum->span (cadddr a))))) expected)])
                      (if (null? rest) (values 'applied (buffer-revision b))
                          (let-values ([(status detail)
                                        (resolve-locked! b id actor (caar rest) (if (memv (caar rest) mine) 'mine 'disk))])
                            (if (eq? status 'applied) (settle (cdr rest)) (values status detail)))))])))])))))

  (define (settle-conflicts! b cs revision)
    ;; Each record was excluded from carrying, preserving the entry's input.
    (for-each (lambda (c)
                (conflict-settled-set! c revision)
                (conflict-unsettled-by-set! c #f))
              cs)
    (refresh-edit-facts! b #f))

  (define (commute-inverse target inverse between)
    ;; The edits BETWEEN, expressed before TARGET. Refuse unless the same
    ;; exchange proves the actual inverse selected by the history planner.
    (let commute ([rest between] [carried (text:invert-delta target)] [before '()])
      (if (null? rest)
          (if (same-delta? carried inverse) (reverse before) (raise (make-conflict-history)))
          (let ([next (text:rebase-delta carried (car rest))]
                [d (text:rebase-delta (car rest) carried 'stay)])
            (unless (and next d) (raise (make-conflict-history)))
            (commute (cdr rest) next (cons d before))))))

  (define (inverse-result-span span target inverse between)
    ;; The span lives before TARGET. Commute the intervening edits onto
    ;; that text, then carry a region through them, not two cursor anchors:
    ;; replacing a surviving boundary must not push it past restored text.
    ;; With no intervening chain, projection only changes the inverse's
    ;; location; preserve offsets into its replacement as before.
    (let ([intended (text:invert-delta target)])
      (if (null? between)
          (let ([s (text:rebase-result-position (text:span-start span) intended inverse '())]
                [e (text:rebase-result-position (text:span-end span) intended inverse '())])
            (text:make-span (car s) (cdr s) (car e) (cdr e)))
          (fold-left carry-region span (commute-inverse target inverse between)))))

  (define (follow-history-conflicts! b id actor entry target between)
    ;; Every inverse has the same conflict semantics, whether installed
    ;; by undo, redo, rewrite or reload. Run after each step so subsequent
    ;; steps carry any revived regions normally.
    (let ([changed #f])
      (let ([target-revision (entry-revision target)] [revision (entry-revision entry)])
        (for-each
          (lambda (c)
            (cond
              [(eqv? (conflict-settled c) target-revision)
               (conflict-span-set! c (inverse-result-span (conflict-span c) (entry-delta target) (entry-delta entry) between))
               (conflict-settled-set! c #f)
               (conflict-unsettled-by-set! c revision)
               (set! changed #t)]))
          (buffer-conflicts b)))
      (when changed
        (refresh-edit-facts! b #f)
        (note-conflicts! id actor))))

  (edoc "Reread a buffer from its file: the disk's text replaces the buffer's as one undoable edit, labelled reread, settling every pending conflict so that its undo brings the text and the conflicts back; the facts commit with the text: (values applied revision), or a write refusal."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (lines (or list vector) "the disk's lines")
        (facts list "the facts to commit: base, stamp, trailing")
        (access* (list-of any) "write access, at most one"))
  (define (reread! actor id lines facts . access*)
    (unless (<= (length access*) 1) (error 'reread! "expected one write access" access*))
    (let ([disk (text:normalize lines)]
          [updates (datum:copy (writable-properties facts))]
          [access (own-write-access (and (pair? access*) (car access*)))])
      (transact! actor
        (lambda (actor)
          (cond
            [(write-refusal id access) => (lambda (reason) (values 'refused reason))]
            [else
             (let* ([b (buffer-of 'reread! id)]
                    [text (buffer-text b)]
                    [pending (unsettled-conflicts b)]
                    [whole (let ([last (- (vector-length text) 1)])
                             (text:make-span 0 0 last (string-length (vector-ref text last))))])
               (if (and (equal? text disk) (null? pending)
                        (let ([p (assq 'trailing updates)])
                          (or (not p) (eq? (cdr p) (property-value b 'trailing #t)))))
                   ;; A content no-op must not replace the property version
                   ;; still owned by an earlier undoable newline change.
                   (let ([updates (remp (lambda (p) (eq? (car p) 'trailing)) updates)])
                     (install-properties! b updates)
                     (refresh-edit-facts! b #f)
                     (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates)
                     (values 'applied (buffer-revision b)))
                   (let-values ([(new-revision delta)
                                 (apply-locked! b id actor whole (vector->list disk) #f
                                   (filter (lambda (p) (eq? (car p) 'trailing)) updates)
                                   (remp (lambda (p) (eq? (car p) 'trailing)) updates) '() pending)])
                     (remember-edit! b actor (list (list 'reread new-revision) "reread"))
                     (when (pair? pending) (note-conflicts! id actor))
                     (values 'applied new-revision))))])))))

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

  (define (copy-mark-value value)
    ;; Own validated position pairs and return fresh ones on reads.  A
    ;; caller must not be able to poison a later edit by mutating a pair.
    (cond
      [(text:position? value) (cons (car value) (cdr value))]
      [(and (text:span? value) (text:position? (text:span-start value))
            (text:position? (text:span-end value)))
       (let ([start (text:span-start value)] [end (text:span-end value)])
         (text:make-span (car start) (cdr start) (car end) (cdr end)))]
      [else (error 'set-marks! "expected a nonnegative position or span" value)]))

  (define (mark-in-text? value text)
    (define (inside? p)
      (and (< (car p) (vector-length text))
           (<= (cdr p) (string-length (vector-ref text (car p))))))
    (if (text:span? value)
        (and (inside? (text:span-start value)) (inside? (text:span-end value)))
        (inside? value)))

  (edoc "Set and drop an actor's marks in a buffer as one publication against a basis: (values applied revision) or (values stale revision)."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (basis (or integer #f) "the revision the positions describe, or #f for current")
        (updates list "(name . position) marks")
        (drops list "names to remove"))
  (define (set-marks! actor id basis updates drops)
    ;; Positions and removals form one publication.  A numeric basis must
    ;; match exactly; stale coordinates never overwrite rebased marks.
    ;; #f explicitly addresses current text (legacy single-mark calls).
    ;; -> (values applied revision) or (values stale current-revision).
    (unless (or (not basis) (and (integer? basis) (exact? basis) (>= basis 0)))
      (error 'set-marks! "expected a revision or #f" basis))
    (unless (and (list? updates) (for-all pair? updates) (list? drops)
                 (let unique ([names (append (map car updates) drops)] [seen '()])
                   (or (null? names)
                       (and (not (member (car names) seen))
                            (unique (cdr names) (cons (car names) seen))))))
      (error 'set-marks! "expected disjoint updates and removals with unique names" updates drops))
    (let ([actor (own-actor actor)]
          [updates (map (lambda (entry) (cons (datum:copy (car entry)) (copy-mark-value (cdr entry)))) updates)]
          [drops (datum:copy drops)])
      (locked
        (lambda ()
          (ensure-open!)
          (let ([b (buffer-of 'set-marks! id)])
            (if (and basis (not (= basis (buffer-revision b))))
                (values 'stale (buffer-revision b))
                (begin
                  (unless (for-all (lambda (entry) (mark-in-text? (cdr entry) (buffer-text b))) updates)
                    (error 'set-marks! "a position is outside the declared text" updates))
                  (buffer-marks-set! b
                    (append (map (lambda (entry) (cons (mark-key actor (car entry)) (cdr entry))) updates)
                            (remp (lambda (entry)
                                    (and (equal? (caar entry) actor)
                                         (or (assoc (cdar entry) updates) (member (cdar entry) drops))))
                                  (buffer-marks b))))
                  (values 'applied (buffer-revision b)))))))))

  (edoc "Set one mark of an actor in a buffer against the current text."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (mark-name datum "the mark")
        (position any "a position or span"))
  (define (set-mark! actor id mark-name position)
    (let-values ([(status revision)
                  (set-marks! actor id #f (list (cons mark-name position)) '())])
      (void)))

  (edoc "The current position of an actor's mark in a buffer, or #f."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (mark-name datum "the mark")
        (returns any))
  (define (mark actor id mark-name)
    ;; the mark's current position, or #f
    (locked
      (lambda ()
        (cond [(assoc (mark-key actor mark-name)
                      (buffer-marks (buffer-of 'mark id)))
               => (lambda (entry) (copy-mark-value (cdr entry)))]
              [else #f]))))

  (edoc "Remove an actor's mark from a buffer."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (mark-name datum "the mark"))
  (define (drop-mark! actor id mark-name)
    (let-values ([(status revision) (set-marks! actor id #f '() (list mark-name))])
      (void)))

  (edoc "An actor's marks in a buffer, (name . position) each."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (returns list))
  (define (marks actor id)
    ;; the actor's marks in the buffer: ((name . position) ...)
    (locked
      (lambda ()
        (fold-right (lambda (entry acc)
                      (if (equal? (caar entry) actor)
                          (cons (cons (datum:copy (cdar entry)) (copy-mark-value (cdr entry))) acc)
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
  ;; Modification facts belong to the writer and cannot be set by clients.
  ;; Their causing edit/reset/property event already notifies observers;
  ;; incremental replies carry both facts without redundant events. Other properties
  ;; survive resets and renames unless explicitly updated, and die with
  ;; delete!.  Subscribers hear (property id key actor).

  (edoc "Set one fact of a buffer."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (key symbol "the fact")
        (value datum "its value"))
  (define (set-property! actor id key value)
    (set-properties! actor id (list (cons key value))))

  (edoc "Set facts of a buffer, optionally only while a review still holds, and optionally renaming it; whether accepted."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (updates list "(key . value) facts")
        (options (list-of any) "a fact review, then a new name")
        (returns boolean))
  (define (set-properties! actor id updates . options)
    ;; Compare and publish under the writer, never across the caller's I/O.
    ;; An empty review still requires a live buffer; #f is unguarded.
    ;; An optional name joins the facts before any subscriber can run.
    (unless (<= (length options) 2) (error 'set-properties! "expected fact review and optional name" options))
    (let ([updates (datum:copy (writable-properties updates))]
          [expected (datum:copy (property:validate-expected (and (pair? options) (car options))))]
          [name (and (= (length options) 2) (own-name (cadr options)))])
      (transact! actor
        (lambda (actor)
          (let* ([b (if expected (hashtable-ref (store-buffers (current-store)) id #f)
                        (buffer-of 'set-properties! id))]
                 [trailing? (and b (property-value b 'trailing #t))]
                 [trashed? (and b (property-value b 'trashed #f) #t)])
            (and b (or (not expected) (property:matches? expected (current-properties b)))
                 (begin
                   (unless (null? updates)
                     (install-properties! b updates)
                     (refresh-edit-facts! b (not (eq? trailing? (property-value b 'trailing #t)))))
                   ;; a restored buffer takes a unique name: while it sat in
                   ;; the trash, a fresh visit may have taken its own
                   (when (and trashed? (not (property-value b 'trashed #f)))
                     (let ([unique (unique-name (buffer-label b) id)])
                       (unless (string=? unique (buffer-label b)) (rename-buffer! actor id unique))))
                   (when name (rename-buffer! actor id name))
                   (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates)
                   #t)))))))

  (edoc "Remove a fact from a buffer."
        (actor actor "the actor identity")
        (id integer "the buffer id")
        (key symbol "the fact"))
  (define (drop-property! actor id key)
    (unless (symbol? key)
      (error 'drop-property! "expected a symbol key" key))
    (when (memq key property:edit-keys) (error 'drop-property! "modification facts belong to the text owner"))
    (when (eq? key 'publication) (error 'drop-property! "publication identity belongs to publish!"))
    (transact! actor
      (lambda (actor)
        (let* ([b (buffer-of 'drop-property! id)] [trailing? (property-value b 'trailing #t)])
          (buffer-properties-set!
            b (replace-property-cell (buffer-properties b) (cons key missing-property)))
          (refresh-edit-facts! b (not (eq? trailing? (property-value b 'trailing #t))))
          (enqueue-event! `(property ,id ,key ,actor)))))
    (void))

  (edoc "A buffer's fact, or a fallback when absent, #f by default."
        (id integer "the buffer id")
        (key symbol "the fact")
        (fallback (list-of any) "the value when absent, at most one")
        (returns any))
  (define (property id key . fallback)
    ;; Absence uses the fallback (#f by default); an explicit #f stays #f.
    (unless (<= (length fallback) 1) (error 'property "expected one fallback" fallback))
    (locked
      (lambda ()
        (let ([b (buffer-of 'property id)])
          (case key
            [(modified) (buffer-modified b)]
            [(modified-at) (buffer-modified-at b)]
            [(conflicts) (length (unsettled-conflicts b))]
            [else
             (let ([cell (property-cell (buffer-properties b) key)])
               (if (and cell (not (eq? (cdr cell) missing-property)))
                   (datum:copy (cdr cell))
                   (and (pair? fallback) (car fallback))))])))))

  (edoc "Every fact of a buffer, as fresh pairs."
        (id integer "the buffer id")
        (returns list))
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

  (edoc "Subscribe a procedure to a buffer's events, or to every buffer's with #f; the token unsubscribes."
        (id (or integer #f) "the buffer, or #f for all")
        (proc procedure "the subscriber")
        (returns integer))
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

  (edoc "Cancel a subscription by token."
        (token integer "the token"))
  (define (unsubscribe! token)
    (locked
      (lambda ()
        (kernel:registry-remove! subscriptions
                                 (lambda (entry) (equal? (car entry) token)))))
    (void))

  (edoc "Subscribe to invalidations: (values token take), take giving the pending (id . facts-or-lifecycle?) pairs, or #f to rescan."
        (wake thunk "run when something changes"))
  (define (watch! wake)
    ;; A reader needs invalidations, not a second retained edit history.
    ;; Return the ordinary subscription token and a procedure that takes
    ;; pending (id . facts-or-lifecycle?) pairs. #f means rescan inventory,
    ;; including previously adopted ids that may now be deleted/hidden.
    ;; At most 256 ids survive a stalled reader; repeated edits cost no space.
    (unless (procedure? wake) (error 'watch! "expected a procedure" wake))
    (let ([lock (make-mutex)] [pending '()])
      (values
        (subscribe! #f
          (lambda (event)
            (let ([notify?
                   (with-mutex lock
                     (let ([empty? (null? pending)] [id (cadr event)]
                           [facts? (and (memq (car event) '(create rename delete property)) #t)])
                       (when pending
                         (let ([entry (assv id pending)])
                           (cond [entry (when facts? (set-cdr! entry #t))]
                             [(= (length pending) 256) (set! pending #f)]
                             [else (set! pending (cons (cons id facts?) pending))])))
                       empty?))])
              ;; Never invoke a consumer under either writer lock.
              (when notify? (wake)))))
        (lambda ()
          (with-mutex lock
            (let ([out pending])
              (set! pending '())
              (and out (reverse out))))))))

  (define (enqueue-event! event)
    (if (planned-events)
        (set-box! (planned-events) (cons event (unbox (planned-events))))
        (deliver-event! event)))

  (define (deliver-event! event)
    ;; Caller holds the mutation lock. Capture recipients at commit;
    ;; resolve their registrations again when the shared queue delivers.
    (let ([tokens
           (kernel:call-with-runtime-registrations
             (lambda ()
               (map car
                    (filter (lambda (entry)
                              (or (not (cadr entry))
                                  (equal? (cadr entry) (cadr event))))
                            (kernel:registry-items subscriptions)))))]
          [queue (store-deliveries (current-store))])
      (for-each
        (lambda (token)
          (kernel:enqueue-delivery! queue
            (lambda ()
              (let ([subscriber (kernel:registry-find subscriptions
                                  (lambda (entry) (= (car entry) token)))])
                ;; Each recipient owns the envelope and metadata. The only
                ;; opaque leaf is the immutable text delta, shared as before.
                (when subscriber ((caddr subscriber) (datum:copy event values)))))))
        tokens))))
