;; state.e -- the buffer store: the library (state).
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
;; Naming reads behind the import prefix: (state:edit! ...),
;; (state:snapshot ...).

(library (state)
  (export create! delete! reset! rename!
          buffer-list exists? buffer-name find-named
          snapshot revision line-count line extract
          edit! undo! history blame
          set-mark! mark drop-mark! marks
          set-property! drop-property! property properties
          subscribe! unsubscribe!)
  (import (rnrs)
          (only (chezscheme)
                box unbox set-box! make-mutex with-mutex format void)
          (prefix (text) text:)
          (only (kernel)
                persistent-cell make-registry registry-add!
                registry-items registry-remove!))

  ;;; The store -------------------------------------------------------------

  (define delta-log-limit 256)

  (define-record-type (buffer make-buffer buffer?)
    (fields (mutable label)
            (mutable text)       ; immutable line vector, per (text)
            (mutable revision)
            (mutable deltas)     ; (#(revision actor delta) ...) newest first
            (mutable marks)      ; (((actor . name) . position) ...)
            (mutable undo)       ; (#(revision actor delta live?) ...)
            (mutable properties))) ; ((key . datum) ...), see set-property!

  (define-record-type (store make-store store?)
    (fields lock
            buffers              ; id -> buffer
            (mutable next-id)))

  (define the-store
    (persistent-cell 'state-store
      (lambda ()
        (make-store (make-mutex)
                    (make-eqv-hashtable)
                    1))))

  (define (current-store) (unbox the-store))

  (define (locked thunk)
    (with-mutex (store-lock (current-store)) (thunk)))

  (define (buffer-of who id)
    (or (hashtable-ref (store-buffers (current-store)) id #f)
        (error who (format "no buffer ~a" id))))

  ;;; Lifecycle and reading --------------------------------------------------

  (define (create-locked! actor buffer-name lines)
    (unless (and (list? lines) (for-all string? lines))
      (error 'create! "lines must be a list of strings" lines))
    (locked
      (lambda ()
        (let* ([s (current-store)]
               [id (store-next-id s)])
          (store-next-id-set! s (+ id 1))
          (hashtable-set!
            (store-buffers s) id
            (make-buffer buffer-name
                         (list->vector (if (null? lines) '("") lines))
                         0 '() '() '() '()))
          id))))

  (define (create! actor buffer-name lines)
    ;; -> the new buffer's id.  lines: a list of strings; empty means
    ;; one empty line, since a text always has at least one line.
    ;; Subscribers hear (create id name actor): a head adopts buffers
    ;; other actors open.
    (let ([id (create-locked! actor buffer-name lines)])
      (notify! `(create ,id ,buffer-name ,actor))
      id))

  (define (reset! actor id lines)
    ;; Wholesale replacement: a new baseline, not an edit.  The delta
    ;; log and the undo history clear (a stale basis against a reset
    ;; refuses as basis-too-old), and marks clamp into the new text.
    ;; Views that regenerate their whole content use this; edits
    ;; should use edit!.
    (let* ([text (cond [(vector? lines)
                        (let ([copy (make-vector (vector-length lines))])
                          (do ([i 0 (+ i 1)])
                              ((= i (vector-length lines)) copy)
                            (vector-set! copy i (vector-ref lines i))))]
                       [(null? lines) (vector "")]
                       [else (list->vector lines)])]
           [text (if (zero? (vector-length text)) (vector "") text)]
           [new-revision
            (locked
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
                  (buffer-revision-set! b (+ (buffer-revision b) 1))
                  (buffer-deltas-set! b '())
                  (buffer-undo-set! b '())
                  (buffer-marks-set!
                    b (map (lambda (entry)
                             (cons (car entry)
                                   (clamp-mark-value (cdr entry) clamp)))
                           (buffer-marks b)))
                  (buffer-revision b))))])
      (notify! `(reset ,id ,new-revision ,actor))
      new-revision))

  (define (rename! actor id new-name)
    ;; subscribers hear (rename id new-name actor)
    (locked
      (lambda ()
        (buffer-label-set! (buffer-of 'rename! id) new-name)))
    (notify! `(rename ,id ,new-name ,actor))
    (void))

  (define (delete! actor id)
    (locked
      (lambda ()
        (buffer-of 'delete! id)
        (hashtable-delete! (store-buffers (current-store)) id)))
    (notify! `(delete ,id ,actor))
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

  (define (deltas-since b basis)
    ;; the deltas applied after the basis revision, oldest first, or
    ;; #f when the basis has fallen out of the log
    (let ([current (buffer-revision b)])
      (cond
        [(= basis current) '()]
        [(or (> basis current)
             (< basis (- current (length (buffer-deltas b)))))
         #f]
        [else
         (let take ([entries (buffer-deltas b)] [acc '()])
           (cond [(null? entries) acc]
                 [(<= (vector-ref (car entries) 0) basis) acc]
                 [else (take (cdr entries)
                             (cons (vector-ref (car entries) 2)
                                   acc))]))])))

  (define (rebase-through span deltas)
    ;; the span carried across each delta in order, or #f when any
    ;; step reports it stale
    (cond [(not deltas) #f]
          [(null? deltas) span]
          [else
           (let ([rebased (text:rebase-span span (car deltas))])
             (and rebased (rebase-through rebased (cdr deltas))))]))

  (define (apply-locked! b actor span replacement record-undo?)
    ;; the single mutation point; the caller holds the lock and has a
    ;; span valid against the buffer's current text
    (let-values ([(new-text delta)
                  (text:apply-edit (buffer-text b) span replacement)])
      (let ([new-revision (+ (buffer-revision b) 1)])
        (buffer-text-set! b new-text)
        (buffer-revision-set! b new-revision)
        (buffer-deltas-set!
          b (bounded (cons (vector new-revision actor delta)
                           (buffer-deltas b))
                     delta-log-limit))
        (buffer-marks-set!
          b (map (lambda (entry)
                   (cons (car entry)
                         (rebase-mark-value (cdr entry) delta)))
                 (buffer-marks b)))
        (when record-undo?
          (buffer-undo-set!
            b (bounded (cons (vector new-revision actor delta #t)
                             (buffer-undo b))
                       delta-log-limit)))
        (values new-revision delta))))

  (define (bounded entries n)
    (let loop ([entries entries] [n n])
      (cond [(null? entries) '()]
            [(zero? n) '()]
            [else (cons (car entries)
                        (loop (cdr entries) (- n 1)))])))

  (define (edit! actor id basis span replacement)
    ;; The transaction: apply the edit as the actor meant it against
    ;; the basis revision, rebasing it across whatever landed since --
    ;; or refuse.  -> (values 'applied revision)
    ;;             |  (values 'stale 'overlap)       edited meanwhile
    ;;             |  (values 'stale 'basis-too-old) log outgrown
    (let ([outcome
           (locked
             (lambda ()
               (let* ([b (buffer-of 'edit! id)]
                      [since (deltas-since b basis)]
                      [rebased (and since
                                    (rebase-through
                                      (text:normalize-span span)
                                      since))])
                 (cond
                   [(not since) (list 'stale 'basis-too-old)]
                   [(not rebased) (list 'stale 'overlap)]
                   [else
                    (let-values ([(new-revision delta)
                                  (apply-locked! b actor rebased
                                                 replacement #t)])
                      (list 'applied new-revision delta))]))))])
      (case (car outcome)
        [(applied)
         (notify! `(edit ,id ,(cadr outcome) ,actor ,(caddr outcome)))
         (values 'applied (cadr outcome))]
        [else (values 'stale (cadr outcome))])))

  (define (undo! actor id)
    ;; Undo the actor's newest live edit -- only while its inverse
    ;; still rebases cleanly across everything after it.
    ;; -> (values 'applied revision) | (values 'blocked 'overlap)
    ;;  | (values 'nothing #f)
    (let ([outcome
           (locked
             (lambda ()
               (let* ([b (buffer-of 'undo! id)]
                      [entry (find (lambda (entry)
                                     (and (vector-ref entry 3)
                                          (equal? (vector-ref entry 1)
                                                  actor)))
                                   (buffer-undo b))])
                 (if (not entry)
                     (list 'nothing #f)
                     (let*-values
                       ([(inverse-span replacement)
                         (text:invert (vector-ref entry 2))]
                        [(rebased)
                         (rebase-through
                           inverse-span
                           (deltas-since b (vector-ref entry 0)))])
                       (cond
                         [(not rebased) (list 'blocked 'overlap)]
                         [else
                          (vector-set! entry 3 #f)
                          (let-values ([(new-revision delta)
                                        (apply-locked!
                                          b actor rebased
                                          replacement #f)])
                            (list 'applied new-revision delta))]))))))])
      (case (car outcome)
        [(applied)
         (notify! `(edit ,id ,(cadr outcome) ,actor ,(caddr outcome)))
         (values 'applied (cadr outcome))]
        [else (values (car outcome) (cadr outcome))])))

  (define (history id . count)
    ;; Attribution: the newest applied edits, as plain data --
    ;; ((revision actor start end new-end) ...) newest first, bounded
    ;; by the delta log.  Resets clear it: a reset is a new baseline.
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
                  (cons (list (vector-ref entry 0)
                              (vector-ref entry 1)
                              (text:span-start s)
                              (text:span-end s)
                              (text:delta-new-end d))
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
  ;; explicitly off -- and an absent property reads as #f;
  ;; drop-property! forgets one.  Properties survive resets and
  ;; renames (they are not text) and die with delete!.  Subscribers
  ;; hear (property id key actor).

  (define (set-property! actor id key value)
    (unless (symbol? key)
      (error 'set-property! "expected a symbol key" key))
    (locked
      (lambda ()
        (let ([b (buffer-of 'set-property! id)])
          (buffer-properties-set!
            b (cons (cons key value)
                    (remp (lambda (entry) (eq? (car entry) key))
                          (buffer-properties b)))))))
    (notify! `(property ,id ,key ,actor))
    (void))

  (define (drop-property! actor id key)
    (locked
      (lambda ()
        (let ([b (buffer-of 'drop-property! id)])
          (buffer-properties-set!
            b (remp (lambda (entry) (eq? (car entry) key))
                    (buffer-properties b))))))
    (notify! `(property ,id ,key ,actor))
    (void))

  (define (property id key)
    ;; the buffer's fact under key, or #f
    (locked
      (lambda ()
        (cond [(assq key (buffer-properties (buffer-of 'property id)))
               => cdr]
              [else #f]))))

  (define (properties id)
    ;; every fact, as fresh pairs: ((key . value) ...)
    (locked
      (lambda ()
        (map (lambda (entry) (cons (car entry) (cdr entry)))
             (buffer-properties (buffer-of 'properties id))))))

  ;;; Subscriptions ------------------------------------------------------------

  ;; Subscribers hear changes as data: (edit id revision actor delta),
  ;; (reset id revision actor), (property id key actor), and the
  ;; buffer lifecycle -- (create id name actor), (rename id name
  ;; actor), (delete id actor).  Delivery is synchronous and
  ;; outside the store lock -- a subscriber may read state, but slow
  ;; subscribers slow the writer; asynchronous delivery (to an agent's
  ;; mailbox, over the wire) is future work.
  ;;
  ;; Subscriptions are kernel-registry entries, so they are owned like
  ;; any registration: a module's subscription retracts when the
  ;; module reloads, before its init! subscribes afresh.  The token is
  ;; the explicit revocation handle for everything else.  Entries are
  ;; (token buffer-id proc); the store mutex still serializes
  ;; subscribe!/unsubscribe! against notify!'s read.

  (define subscriptions (make-registry))

  (define subscription-counter
    (persistent-cell 'state-subscription-counter (lambda () 0)))

  (define (subscribe! id proc)
    ;; -> a token for unsubscribe!; id #f hears every buffer
    (unless (procedure? proc)
      (error 'subscribe! "expected a procedure" proc))
    (locked
      (lambda ()
        (let ([token (+ (unbox subscription-counter) 1)])
          (set-box! subscription-counter token)
          (registry-add! subscriptions (list token id proc))
          token))))

  (define (unsubscribe! token)
    (locked
      (lambda ()
        (registry-remove! subscriptions
                          (lambda (entry) (equal? (car entry) token)))))
    (void))

  (define (notify! event)
    (let ([interested
           (locked
             (lambda ()
               (filter (lambda (entry)
                         (or (not (cadr entry))
                             (equal? (cadr entry) (cadr event))))
                       (registry-items subscriptions))))])
      (for-each (lambda (entry)
                  (guard (ex [else (void)]) ((caddr entry) event)))
                interested))))
