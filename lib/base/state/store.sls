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

(import (only (edoc) elibrary))
(elibrary (store)
  (export create! visit! delete! discard! close! reset! rename! publication publish!
          buffer-list exists? visible? buffer-name find-named find-file
          snapshot snapshot-since snapshot-state state revision line-count line extract
          edit! edit-with-snapshot! undo! redo! history-step! undo-authors history blame
          set-mark! set-marks! mark drop-mark! marks
          set-property! set-properties! drop-property! property properties
          validate-properties validate-edit-context
          subscribe! unsubscribe! watch! export import! valid-import?)
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! set-cdr! make-mutex with-mutex format void remq
                current-time time-second time-nanosecond list-head)
          (prefix (text) text:)
          (prefix (property) property:)
          (prefix (actor) actor:)
          (prefix (activity) activity:)
          (prefix (datum) datum:)
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
            (mutable modified)   ; derived from text/trailing and the baseline
            (mutable modified-at))) ; UTC nanoseconds of the last content change

  (define-record-type undo-group
    (fields id actor key
            (mutable label)
            (mutable parts)      ; the group's last deltas, newest first
            (mutable live?)
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
          (cons 'modified-at (buffer-modified-at b))))

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

  (define (unique-name base self)
    ;; Caller holds the store lock. Hidden buffers share this namespace;
    ;; deletion releases a name and renaming does not compete with itself.
    (let ([used (make-hashtable string-hash string=?)])
      (vector-for-each
        (lambda (id)
          (unless (eqv? id self)
            (hashtable-set! used (buffer-label (buffer-of 'unique-name id)) #t)))
        (hashtable-keys (store-buffers (current-store))))
      (let next ([name base] [suffix 2])
        (if (hashtable-ref used name #f)
            (next (format "~a<~a>" base suffix) (+ suffix 1))
            name))))

  (edoc "Create a buffer with a name, lines and optional facts, publishing them together; its id."
        (actor any "the actor identity")
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
    (let* ([s (current-store)] [name (unique-name name #f)] [id (store-next-id s)])
      (store-next-id-set! s (+ id 1))
      (let ([b (make-buffer name text 0 '() '() '() '() #f #f #f)])
        (install-properties! b updates)
        (refresh-edit-facts! b #f)
        (hashtable-set! (store-buffers s) id b))
      (enqueue-event! `(create ,id ,name ,actor))
      id))

  (define (file-id path)
    ;; Caller holds the store lock. Paths are canonicalized before admission;
    ;; identity lives in the file fact, so retarget/delete need no index upkeep.
    (unless (and (string? path) (> (string-length path) 0))
      (error 'find-file "expected a nonempty canonical file path" path))
    (find (lambda (id) (equal? (property-value (buffer-of 'find-file id) 'file #f) path))
          (vector->list (hashtable-keys (store-buffers (current-store))))))

  (edoc "The id of the buffer visiting a file, or #f."
        (path file "the file")
        (returns (or integer #f)))
  (define (find-file path)
    (locked (lambda () (file-id path))))

  (edoc "Visit a file as a buffer, concurrent visitors sharing the first: (values id created?)."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
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
    (let* ([b (buffer-of 'reset! id)]
           [old (buffer-text b)] [trailing? (property-value b 'trailing #t)]
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
      (buffer-marks-set!
        b (map (lambda (entry) (cons (car entry) (clamp-mark-value (cdr entry) clamp)))
               (buffer-marks b)))
      (enqueue-event! `(reset ,id ,(buffer-revision b) ,actor))
      (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates)
      (buffer-revision b)))

  (define (publication-id identity)
    (find (lambda (id) (equal? (property-value (buffer-of 'publication id) 'publication #f) identity))
          (vector->list (hashtable-keys (store-buffers (current-store))))))

  (edoc "The id of a producer's generated source under a key, or #f."
        (actor any "the actor identity")
        (key datum "the publication key")
        (returns (or integer #f)))
  (define (publication actor key)
    ;; One generated source per producer/key, independent of its label.
    ;; Identity lives with the buffer, so deletion needs no second registry.
    (let ([identity (list (own-actor actor) (datum:copy key))])
      (locked (lambda () (publication-id identity)))))

  (edoc "Create or replace a producer's generated source atomically; an (id revision fact ...) basis refuses stale refreshes. The id, or #f when refused."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
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
    (let* ([b (buffer-of 'rename! id)] [name (unique-name name id)])
      (buffer-label-set! b name)
      (enqueue-event! `(rename ,id ,name ,actor))
      (string-copy name)))

  (edoc "Delete a buffer."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
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

  (define persistent-keys '(file base stamp trailing mode read-only modified-at))
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
                        [(modified-at) (or (not (cdr entry)) (and (integer? (cdr entry)) (exact? (cdr entry))))]
                        [(stamp)
                         (let ([stamp (cdr entry)])
                           (or (not stamp)
                               (and (pair? stamp) (integer? (car stamp)) (exact? (car stamp))
                                    (integer-at-least? (cdr stamp) 0) (< (cdr stamp) 1000000000))))])
                      (check (cdr rest) (cons (car entry) seen))))))))

  (edoc "Whether a next id and saved buffer states form a valid saved store."
        (next-id integer "the next buffer id")
        (states list "the saved states")
        (returns boolean))
  (define (valid-import? next-id states)
    ;; A pure validation boundary. No callbacks or partial store mutation;
    ;; session startup distinguishes bad data from a later import failure.
    (and (integer-at-least? next-id 1) (list? states)
         (let ([ids (make-eqv-hashtable)] [names (make-hashtable string-hash string=?)])
           (for-all
             (lambda (state)
               (and (list? state) (= (length state) 5)
                    (integer-at-least? (car state) 1) (< (car state) next-id)
                    (integer-at-least? (cadr state) 0)
                    (string? (caddr state)) (> (string-length (caddr state)) 0)
                    (not (hashtable-contains? ids (car state)))
                    (not (hashtable-contains? names (caddr state)))
                    (vector? (cadddr state)) (> (vector-length (cadddr state)) 0)
                    (for-all text:line? (vector->list (cadddr state)))
                    (persistent-facts? (list-ref state 4))
                    (begin (hashtable-set! ids (car state) #t) (hashtable-set! names (caddr state) #t) #t)))
             states))))

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
                                    (list id (buffer-revision b) (string-copy (buffer-label b))
                                      (buffer-text b) (property-data b))))
                             (list-sort < (vector->list (hashtable-keys (store-buffers (current-store)))))))))])
         (values next-id
           (filter values
             (map (lambda (state)
                    (let* ([state (convert state)] [facts (list-ref state 4)])
                      (and (not (cond [(assq 'disposable facts) => cdr] [else #f]))
                           (append (list-head state 4)
                             (list (filter (lambda (entry) (memq (car entry) persistent-keys)) facts)))))) states))))]))

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
                      (cond [(assq 'modified-at facts) => cdr] [else #f]))])
            (install-properties! b (filter (lambda (entry) (not (eq? (car entry) 'modified-at))) facts))
            (refresh-edit-facts! b #f)
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
        (actor any "the actor identity")
        (id integer "the buffer id")
        (returns boolean))
  (define (visible? actor id)
    ;; Audience is presentation/routing, not permission to read the store.
    ;; Missing content is never visible; an absent audience means all.
    (locked
      (lambda ()
        (let ([b (hashtable-ref (store-buffers (current-store)) id #f)])
          (and b (actor:in-audience? actor (property-value b 'audience 'all)))))))

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
    (list (vector-ref entry 0) (datum:copy (vector-ref entry 1)) (vector-ref entry 2)))

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
           [trailing? (property-value b 'trailing #t)]
           [entry (vector new-revision actor delta origin facts)])
      (buffer-text-set! b new-text)
      (buffer-revision-set! b new-revision)
      (buffer-deltas-set!
        b (bounded (cons entry (buffer-deltas b)) delta-log-limit))
      (buffer-properties-set! b (apply-property-changes (buffer-properties b) facts))
      (install-properties! b commit-facts)
      (refresh-edit-facts! b
        (or (not (equal? (text:delta-removed delta) (text:delta-inserted delta)))
            (not (eq? trailing? (property-value b 'trailing #t)))))
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

  (edoc "Apply an edit against a basis revision, rebased across what landed since: (values applied revision), or (values stale overlap|basis-too-old)."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
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
             [properties (if (and context (>= (length context) 3))
                           (datum:copy (caddr context)) '())]
             [commit-facts (if (and context (>= (length context) 4))
                             (datum:copy (cadddr context)) '())]
             [expected (and context (= (length context) 5) (datum:copy (list-ref context 4)))]
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
                                         (map (lambda (entry) (vector-ref entry 2)) since)))])
                       (cond
                         [(and expected (not (and b (property:matches? expected (current-properties b)))))
                          (list 'stale 'property-changed)]
                         [(not since) (list 'stale 'basis-too-old)]
                         [(not rebased) (list 'stale 'overlap)]
                         [else
                          (let-values ([(new-revision delta)
                                        (apply-locked! b id actor rebased
                                                       replacement #f
                                                       properties commit-facts)])
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

  (edoc "Undo or redo in a buffer under a scope: (values status detail), status applied, blocked, nothing or refused."
        (actor any "the actor identity")
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
                               (datum:copy
                                 (list (buffer-revision b) (undo-group-id group)
                                       (undo-group-actor group) (undo-group-key group)
                                       (undo-group-label group)) values)))))))])))))

  (edoc "Undo in a buffer: (values status detail), the detail the new revision when applied."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
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
        (let loop ([groups (buffer-undo (buffer-of 'undo-authors id))] [authors '()])
          (cond
            [(null? groups) (datum:copy (reverse authors))]
            [(and (undo-group-live? (car groups))
                  (not (member (undo-group-actor (car groups)) authors)))
             (loop (cdr groups) (cons (undo-group-actor (car groups)) authors))]
            [else (loop (cdr groups) authors)])))))

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
                       [d (vector-ref entry 2)]
                       [s (text:delta-span d)])
                  (cons (datum:copy (append (list (vector-ref entry 0)
                                              (vector-ref entry 1)
                                              (text:span-start s)
                                              (text:span-end s)
                                              (text:delta-new-end d))
                                      (if (vector-ref entry 3) (list (vector-ref entry 3)) '())))
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
                        (cons (list (copy-mark-value current)
                                    (datum:copy (vector-ref entry 1))
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
        (actor any "the actor identity")
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
        (actor any "the actor identity")
        (id integer "the buffer id")
        (mark-name datum "the mark")
        (position any "a position or span"))
  (define (set-mark! actor id mark-name position)
    (let-values ([(status revision)
                  (set-marks! actor id #f (list (cons mark-name position)) '())])
      (void)))

  (edoc "The current position of an actor's mark in a buffer, or #f."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
        (id integer "the buffer id")
        (mark-name datum "the mark"))
  (define (drop-mark! actor id mark-name)
    (let-values ([(status revision) (set-marks! actor id #f '() (list mark-name))])
      (void)))

  (edoc "An actor's marks in a buffer, (name . position) each."
        (actor any "the actor identity")
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
        (actor any "the actor identity")
        (id integer "the buffer id")
        (key symbol "the fact")
        (value datum "its value"))
  (define (set-property! actor id key value)
    (set-properties! actor id (list (cons key value))))

  (edoc "Set facts of a buffer, optionally only while a review still holds, and optionally renaming it; whether accepted."
        (actor any "the actor identity")
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
                 [trailing? (and b (property-value b 'trailing #t))])
            (and b (or (not expected) (property:matches? expected (current-properties b)))
                 (begin
                   (unless (null? updates)
                     (install-properties! b updates)
                     (refresh-edit-facts! b (not (eq? trailing? (property-value b 'trailing #t)))))
                   (when name (rename-buffer! actor id name))
                   (for-each (lambda (entry) (enqueue-event! `(property ,id ,(car entry) ,actor))) updates)
                   #t)))))))

  (edoc "Remove a fact from a buffer."
        (actor any "the actor identity")
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
