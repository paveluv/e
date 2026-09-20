;; kernel.sls -- the kernel: the substrate every other module stands
;; on.  Persistent cells (module state that survives hot reloads), the
;; registries and the module lifecycle, the mailboxes actors wait on,
;; and the text of a caught condition.  The kernel imports nothing
;; above itself and, like main, is never reloaded in place.

(import (only (foundation edoc) elibrary))
(elibrary (core kernel)
  (export add-after-reload-hook! call-with-registration-update call-with-runtime-registrations
          condition-text config-file drain-deliveries! editor-symbol? enqueue-delivery!
          fingerprint init-module! installation-directory load-config! load-module!
          load-modules! loaded-modules mailbox-post! mailbox-receive! make-delivery-queue
          make-mailbox make-read-only-error make-refusal make-registry module-library
          module-requires? module-source persistent-cell pin-modules! read-only-error? refusal?
          registering-module registration-conflict? registry-add! registry-entries registry-find
          registry-items registry-observe! registry-remove! registry-unobserve! reload-module!
          retract-module!)
  (import (rnrs)
          (only (chezscheme)
                box unbox make-hashtable equal-hash
                make-parameter make-thread-parameter current-directory format interaction-environment eval
                scheme-environment environment-symbols top-level-bound?
                library-exports library-requirements library-requirements-options
                library-directories load directory-list file-directory? file-regular? path-extension
                parameterize make-mutex with-mutex make-condition
                condition-wait condition-signal condition-broadcast
                with-interrupts-disabled make-time
                current-time time? time-type time<? time-difference
                get-thread-id box? display-condition void)
          (prefix (sys path) path:))

  ;;; Conditions --------------------------------------------------------------

  (edoc "A raised object as the text the log and the echo area show for it."
        (ex any "the condition or object")
        (returns string))
  (define (condition-text ex)
    ;; a caught condition -- or any raised object -- as the text the
    ;; log and the echo area show for it
    (if (condition? ex)
        (call-with-string-output-port (lambda (p) (display-condition ex p)))
        (format "~a" ex)))

  ;;; Refusals ----------------------------------------------------------------

  ;; Two distinguished conditions the loop shows as plain messages
  ;; rather than exception reports: an edit refused by a read-only
  ;; buffer, and a command the user declined mid-flight (an edit in a
  ;; buffer whose file changed on disk, say).
  (edoc "An edit was refused because the buffer is read-only.")
  (define-condition-type &read-only &error make-read-only-error read-only-error?)

  (edoc "A command the user declined mid-flight, or the store refused.")
  (define-condition-type &refused &error make-refusal refusal?)

  ;;; Mailboxes ---------------------------------------------------------------

  ;; The scheduling substrate: a mailbox is a thread-safe FIFO with a
  ;; blocking receive.  Actors -- the main loop first of all -- wait
  ;; on their mailbox; any thread posts.

  (define-record-type (mailbox %make-mailbox mailbox?)
    (fields lock signal (mutable head) (mutable tail)))

  (edoc "A thread-safe FIFO mailbox."
        (returns any))
  (define (make-mailbox)
    (%make-mailbox (make-mutex) (make-condition) '() '()))

  (edoc "Post a message to a mailbox, waking a receiver."
        (mb any "the mailbox")
        (message any "the message"))
  (define (mailbox-post! mb message)
    (with-mutex (mailbox-lock mb)
      (with-interrupts-disabled
        (mailbox-tail-set! mb (cons message (mailbox-tail mb))))
      (condition-signal (mailbox-signal mb))))

  (define signal-check-interval (make-time 'time-duration 100000000 0))

  (edoc "Take the next message from a mailbox, waiting for one; #f at a monotonic deadline when one is given, optionally checking for interrupts between bounded waits."
        (mb any "the mailbox")
        (deadline (or any #f) "a monotonic time, or #f to wait")
        (service-signals? boolean "whether to check interrupts")
        (returns any))
  (define mailbox-receive!
    ;; Strictly FIFO. A monotonic deadline returns #f on timeout. Signal
    ;; owners can opt into bounded waits and an interrupt check outside the
    ;; mailbox lock. These checks neither return a timeout nor replace an
    ;; evaluation's timer; only actual messages/deadlines reach the caller.
    (case-lambda
      [(mb)
       (mailbox-receive! mb #f #f)]
      [(mb deadline)
       (mailbox-receive! mb deadline #f)]
      [(mb deadline service-signals?)
       (unless (or (not deadline) (and (time? deadline) (eq? (time-type deadline) 'time-monotonic)))
         (error 'mailbox-receive! "expected a monotonic deadline or #f" deadline))
       (unless (boolean? service-signals?)
         (error 'mailbox-receive! "expected a signal-service flag" service-signals?))
       (let wait ()
         (when service-signals? (with-interrupts-disabled (void)))
         (let-values ([(again? message)
                       (with-mutex (mailbox-lock mb)
                         (let receive ()
                           (cond
                             [(pair? (mailbox-head mb))
                              (with-interrupts-disabled
                                (let ([message (car (mailbox-head mb))])
                                  (mailbox-head-set! mb (cdr (mailbox-head mb)))
                                  (values #f message)))]
                             [(pair? (mailbox-tail mb))
                              ;; A signal can post while reverse allocates.
                              ;; Publish only if that tail is still current;
                              ;; keep reversal interruptible and the two writes
                              ;; indivisible, including for a same-thread post.
                              (let* ([tail (mailbox-tail mb)] [front (reverse tail)])
                                (with-interrupts-disabled
                                  (when (eq? tail (mailbox-tail mb))
                                    (mailbox-head-set! mb front)
                                    (mailbox-tail-set! mb '()))))
                              (receive)]
                             [else
                              (let* ([now (current-time 'time-monotonic)]
                                     [remaining (and deadline (time-difference deadline now))])
                                (if (and deadline (not (time<? now deadline)))
                                  (values #f #f)
                                  (begin
                                    (condition-wait (mailbox-signal mb) (mailbox-lock mb)
                                      (if (and service-signals?
                                               (or (not remaining) (time<? signal-check-interval remaining)))
                                          signal-check-interval remaining))
                                    (values #t #f))))])))])
           (if again? (wait) message)))]))

  ;;; Ordered delivery -----------------------------------------------------

  ;; A writer queues callbacks inside its own commit lock, then drains
  ;; after releasing it. One drainer finishes them in order; concurrent
  ;; and reentrant writers never wait for their callbacks to run.
  (define-record-type (delivery-queue %make-delivery-queue delivery-queue?)
    (fields lock (mutable front) (mutable back) (mutable active?)))

  (edoc "A queue of deliveries, thunks drained on one thread."
        (returns any))
  (define (make-delivery-queue)
    (%make-delivery-queue (make-mutex) '() '() #f))

  (edoc "Queue a delivery thunk."
        (queue any "the queue")
        (thunk thunk "the delivery"))
  (define (enqueue-delivery! queue thunk)
    (with-mutex (delivery-queue-lock queue)
      (delivery-queue-back-set! queue (cons thunk (delivery-queue-back queue)))))

  (define (next-delivery! queue)
    (with-mutex (delivery-queue-lock queue)
      (when (null? (delivery-queue-front queue))
        (delivery-queue-front-set! queue (reverse (delivery-queue-back queue)))
        (delivery-queue-back-set! queue '()))
      (and (pair? (delivery-queue-front queue))
           (let ([next (car (delivery-queue-front queue))])
             (delivery-queue-front-set! queue (cdr (delivery-queue-front queue)))
             next))))

  (edoc "Run the queued deliveries in order, once, on the calling thread."
        (queue any "the queue"))
  (define (drain-deliveries! queue)
    (let ([entered? #f] [owns? #f])
      (dynamic-wind
        (lambda ()
          (when entered? (error 'kernel "cannot resume completed event delivery"))
          (set! entered? #t))
        (lambda ()
          ;; Arm cleanup before claiming the queue, including interruption
          ;; between admission and the first callback.
          (with-mutex (delivery-queue-lock queue)
            (when (and (not (delivery-queue-active? queue))
                       (or (pair? (delivery-queue-front queue))
                           (pair? (delivery-queue-back queue))))
              (set! owns? #t)
              (delivery-queue-active?-set! queue #t)))
          (when owns?
            (call-with-runtime-registrations
              (lambda ()
                (let drain ()
                  (let ([next (next-delivery! queue)])
                    (when next
                      (guard (ex [else (void)]) (next))
                      (drain))))))))
        (lambda ()
          (when owns?
            (with-mutex (delivery-queue-lock queue)
              (delivery-queue-active?-set! queue #f))
            (set! owns? #f)
            ;; Finish the remainder on escape, and cover a writer racing
            ;; the empty read before we relinquished the queue.
            (drain-deliveries! queue))))))

  ;;; Registries ------------------------------------------------------------

  ;; Everything a module registers -- key bindings, modes,
  ;; highlighters, whatever a future hook adds -- goes through a
  ;; registry and is tagged with the module whose init! is running.
  ;; Reloading a module retracts its entries wholesale before running
  ;; its init! afresh, so registration is replace-by-module by
  ;; construction: a new hook gets it by using make-registry, with
  ;; nothing to remember.  Entries registered outside any module
  ;; (M-x, say) have owner #f and survive reloads.  Lookups prefer
  ;; newer entries.

  (edoc "The module whose registrations are being made, or #f for none."
        (value (or string #f)))
  (define registering-module (make-thread-parameter #f))
  (define registry-lock (make-mutex))
  (define-record-type (registry %make-registry registry?)
    (fields key-of (mutable contents)))
  (define-record-type registration
    (fields owner key item))

  (edoc "Two registrations claimed the same key in one registry.")
  (define-condition-type &registration-conflict &error make-registration-conflict registration-conflict?)
  (define registries '())

  ;; Registration updates stage entry identities, never whole snapshots.
  ;; The initiating thread reads its own changes; other threads see the
  ;; committed lists until the outer update publishes all its deltas.
  (define-record-type registration-update
    (fields thread parent (mutable state) (mutable deltas)))
  (define-record-type registration-delta
    (fields registry (mutable additions) (mutable removals)))
  (define current-registration-update (make-thread-parameter #f))

  (edoc "Run a thunk whose registrations belong to no module and no staged update, as a runtime effect independent of its trigger."
        (thunk thunk "the effect")
        (returns any))
  (define (call-with-runtime-registrations thunk)
    ;; A runtime effect is independent of any initializer that triggered
    ;; it: resolve published callbacks, and do not give their registrations
    ;; the caller's staging or module ownership. Explicit ownership inside
    ;; the callback still works normally.
    (parameterize ([current-registration-update #f] [registering-module #f])
      (thunk)))

  (define (active-registration-update)
    (let ([update (current-registration-update)])
      ;; Chez threads inherit parameters. A worker's registrations are
      ;; independent of the scope that happened to start that thread.
      (and update (= (registration-update-thread update) (get-thread-id))
           (begin
             (unless (eq? (registration-update-state update) 'active)
               (error 'registry "registration update is already closed"))
             update))))

  (define (find-registration-delta update r)
    (find (lambda (delta) (eq? (registration-delta-registry delta) r))
          (registration-update-deltas update)))

  (define (registration-delta! update r)
    ;; Caller holds registry-lock.
    (or (find-registration-delta update r)
        (let ([delta (make-registration-delta r '() '())])
          (registration-update-deltas-set! update
            (cons delta (registration-update-deltas update)))
          delta)))

  (define (apply-registration-delta delta entries)
    (if (null? (registration-delta-removals delta))
        (append (registration-delta-additions delta) entries)
        (let ([removed? (lambda (entry) (memq entry (registration-delta-removals delta)))])
          (append (remp removed? (registration-delta-additions delta))
                  (remp removed? entries)))))

  (define (visible-registry-entries r update)
    ;; Caller holds registry-lock; entry records and list spines are
    ;; private and never mutated after admission.
    (if update
        (let ([entries (visible-registry-entries r (registration-update-parent update))]
              [delta (find-registration-delta update r)])
          (if delta (apply-registration-delta delta entries) entries))
        (registry-contents r)))

  (define (registry-read r)
    (with-mutex registry-lock
      (visible-registry-entries r (active-registration-update))))

  (edoc "A registry of items owned by the modules that register them; with a key procedure, keys are unique across one publication."
        (key-of (or procedure #f) "(key-of item) giving the key, or #f")
        (returns any)
        (effects internal))
  (define make-registry
    (case-lambda
      [()
       (make-registry #f)]
      [(key-of)
       (unless (or (not key-of) (procedure? key-of))
         (error 'make-registry "expected a key procedure or #f" key-of))
       (let ([r (%make-registry key-of '())]) ; newest first
         (with-mutex registry-lock (set! registries (cons r registries)))
         r)]))

  (define registration-observers (make-registry))
  (define registration-deliveries (make-delivery-queue))

  (define (mutate-registrations! thunk)
    (if (active-registration-update) (thunk) (call-with-registration-update thunk)))

  (edoc "Add an item to a registry, owned by the registering module and published with the current update."
        (r any "the registry")
        (item any "the item"))
  (define (registry-add! r item)
    ;; Extract the stable key once, outside the lock. Uniqueness is
    ;; checked against the whole candidate state at outer publication.
    (let ([entry (make-registration (registering-module)
                   (and (registry-key-of r) ((registry-key-of r) item)) item)])
      (mutate-registrations!
        (lambda ()
          (with-mutex registry-lock
            (let ([delta (registration-delta! (active-registration-update) r)])
              (registration-delta-additions-set! delta
                (cons entry (registration-delta-additions delta)))))))))

  (edoc "A registry's items, newest first."
        (r any "the registry")
        (returns list))
  (define (registry-items r)
    (map registration-item (registry-read r)))

  (edoc "A registry's (owner . item) entries, newest first."
        (r any "the registry")
        (returns list))
  (define (registry-entries r)
    ;; Preserve the public shape without exposing ownership/identity
    ;; wrappers to mutation. The registering module still owns its item.
    (map (lambda (entry) (cons (registration-owner entry) (registration-item entry)))
         (registry-read r)))

  (edoc "The first registry item satisfying a predicate, or #f."
        (r any "the registry")
        (match? procedure "the predicate")
        (returns any))
  (define (registry-find r match?)
    ;; Predicates run against one snapshot, outside registry-lock.
    (let loop ([entries (registry-read r)])
      (cond [(null? entries) #f]
            [(match? (registration-item (car entries))) (registration-item (car entries))]
            [else (loop (cdr entries))])))

  (define (remove-registration-entries! r entries update)
    ;; Caller holds registry-lock. Remove only selected identities from
    ;; the latest list, preserving intervening registration/retraction.
    (unless (null? entries)
      (let ([delta (registration-delta! update r)])
        (registration-delta-removals-set! delta
          (append entries (registration-delta-removals delta))))))

  (edoc "Remove the registry entries whose items satisfy a predicate, whoever owns them."
        (r any "the registry")
        (match? procedure "the predicate"))
  (define (registry-remove! r match?)
    ;; drop entries whose item satisfies match?, whoever owns them --
    ;; for registrations with an explicit revocation handle (a store
    ;; subscription's token, say), alongside ownership retraction.
    ;; Each predicate runs once per captured entry, without the lock.
    (let ([entries (filter (lambda (entry) (match? (registration-item entry))) (registry-read r))])
      (mutate-registrations!
        (lambda ()
          (with-mutex registry-lock
            (remove-registration-entries! r entries (active-registration-update)))))))

  (edoc "Remove every registration a module owns, from every registry."
        (owner string "the module"))
  (define (retract-module! owner)
    (mutate-registrations!
      (lambda ()
        (with-mutex registry-lock
          (let ([update (active-registration-update)])
            (for-each
              (lambda (r)
                (remove-registration-entries! r
                  (filter (lambda (entry) (eq? (registration-owner entry) owner))
                          (visible-registry-entries r update))
                  update))
              registries))))))

  (edoc "Watch a registry: (proc removed-items added-items) once per commit; the token unobserves."
        (r any "the registry")
        (proc procedure "the observer")
        (returns any))
  (define (registry-observe! r proc)
    ;; proc receives (removed-items added-items), one batch per commit.
    ;; Like store subscribers, observers only hear future commits and
    ;; can revoke callbacks already queued, but not one already running.
    (unless (procedure? proc) (error 'registry-observe! "expected a procedure" proc))
    (let ([token (list 'observer)])
      (registry-add! registration-observers (list token r proc))
      token))

  (edoc "Stop watching a registry, by the token registry-observe! gave."
        (token any "the token"))
  (define (registry-unobserve! token)
    (registry-remove! registration-observers (lambda (entry) (eq? (car entry) token))))

  (define (commit-registrations! changes)
    ;; Caller holds registry-lock. Validate every candidate BEFORE any
    ;; registry is installed, so one lost claim discards the entire update.
    (for-each
      (lambda (change)
        (when (registry-key-of (car change))
          (let ([seen (make-hashtable equal-hash equal?)])
            (for-each
              (lambda (entry)
                (let ([key (registration-key entry)])
                  (when (hashtable-contains? seen key)
                    (raise (condition (make-registration-conflict)
                             (make-who-condition 'registry-add!)
                             (make-message-condition "duplicate registry key"))))
                  (hashtable-set! seen key #t)))
              (caddr change)))))
      changes)
    (for-each (lambda (change) (registry-contents-set! (car change) (caddr change))) changes)
    (let ([queued? #f])
      (for-each
        (lambda (change)
          (let ([observers
                 (filter (lambda (observer) (eq? (cadr (registration-item observer)) (car change)))
                         (registry-contents registration-observers))])
            (unless (null? observers)
              (let* ([before (cadr change)] [after (caddr change)]
                     [removed (remp (lambda (entry) (memq entry after)) before)]
                     [added (remp (lambda (entry) (memq entry before)) after)])
                (unless (and (null? removed) (null? added))
                  (for-each
                    (lambda (observer)
                      (let ([item (registration-item observer)])
                        (set! queued? #t)
                        (enqueue-delivery! registration-deliveries
                          (lambda ()
                            (when (memq observer (registry-read registration-observers))
                              ((caddr item) (map registration-item removed)
                               (map registration-item added)))))))
                    observers))))))
        changes)
      queued?))

  (define (publish-registration-update! update)
    ;; Caller holds registry-lock. Nested success merges into the parent;
    ;; outer success applies deltas to current committed lists, so a
    ;; concurrent revocation is never resurrected by rollback or commit.
    (let* ([parent (registration-update-parent update)]
           [queued?
            (if parent
                (begin
                  (for-each
                    (lambda (delta)
                      (let ([target (registration-delta! parent (registration-delta-registry delta))])
                        (registration-delta-additions-set! target
                          (append (registration-delta-additions delta)
                            (registration-delta-additions target)))
                        (registration-delta-removals-set! target
                          (append (registration-delta-removals delta)
                                  (registration-delta-removals target)))))
                    (registration-update-deltas update))
                  #f)
                (commit-registrations!
                  (map (lambda (delta)
                         (let* ([r (registration-delta-registry delta)] [before (registry-contents r)])
                           (list r before (apply-registration-delta delta before))))
                       (registration-update-deltas update))))])
      (registration-update-state-set! update 'committed)
      (registration-update-deltas-set! update '())
      queued?))

  (edoc "Publish the registry changes a thunk makes atomically, or none when it raises."
        (thunk thunk "the changes")
        (returns any))
  (define (call-with-registration-update thunk)
    ;; Atomic publication of registry changes only, not arbitrary state
    ;; or resource rollback. Do not publish new handles to other threads
    ;; until this returns. Module loading/reloading runs on the main pump.
    (let ([update (make-registration-update (get-thread-id)
                    (active-registration-update) 'new '())]
          [queued? #f])
      (call-with-values
        (lambda ()
          (dynamic-wind
            (lambda ()
              (with-mutex registry-lock
                (unless (eq? (registration-update-state update) 'new)
                  (error 'call-with-registration-update "cannot resume a closed update"))
                (registration-update-state-set! update 'active)))
            (lambda ()
              (parameterize ([current-registration-update update])
                (call-with-values thunk
                  (lambda results
                    (with-mutex registry-lock
                      (set! queued? (publish-registration-update! update)))
                    (apply values results)))))
            (lambda ()
              (with-mutex registry-lock
                (when (eq? (registration-update-state update) 'active)
                  (registration-update-state-set! update 'aborted)
                  (registration-update-deltas-set! update '()))))))
        (lambda results
          ;; Only drain for a commit that queued observations. An unrelated
          ;; registry mutation may be called with its owner's lock held.
          (when queued? (drain-deliveries! registration-deliveries))
          (apply values results)))))

  ;;; Persistent cells ------------------------------------------------------

  (define persistent-cells (make-hashtable equal-hash equal?))
  (define cells-lock (make-mutex))
  (define-record-type cell-initialization
    (fields thread signal))

  (edoc "A box that survives module reloads: the first request under a key initializes it, other callers wait for that."
        (key any "the cell's key")
        (make-initial thunk "the constructor")
        (returns any)
        (effects internal))
  (define (persistent-cell key make-initial)
    ;; A box that survives module reloads: the first request under a
    ;; key initializes it, with other callers waiting for that result.
    ;; Constructors run outside cells-lock and may request other keys.
    (let ([initializing #f] [entered? #f])
      ;; Arm cleanup before publishing an initialization reservation, so
      ;; an interruption before the constructor starts cannot strand it.
      (dynamic-wind
        (lambda ()
          (when entered? (error 'persistent-cell "cannot resume a closed initialization" key))
          (set! entered? #t))
        (lambda ()
          (let ([entry
                 (with-mutex cells-lock
                   (let wait ()
                     (let ([entry (hashtable-ref persistent-cells key #f)])
                       (cond
                         [(box? entry) entry]
                         [entry
                          (when (= (cell-initialization-thread entry) (get-thread-id))
                            (error 'persistent-cell "recursive initialization of the same key" key))
                          (condition-wait (cell-initialization-signal entry) cells-lock)
                          (wait)]
                         [else
                          (set! initializing (make-cell-initialization (get-thread-id) (make-condition)))
                          (hashtable-set! persistent-cells key initializing)
                          initializing]))))])
            (if (box? entry)
                entry
                (let ([cell (box (make-initial))])
                  (with-mutex cells-lock (hashtable-set! persistent-cells key cell))
                  cell))))
        (lambda ()
          (when initializing
            (with-mutex cells-lock
              (when (eq? (hashtable-ref persistent-cells key #f) initializing)
                (hashtable-delete! persistent-cells key))
              (condition-broadcast (cell-initialization-signal initializing))))))))

  ;;; Module lifecycle --------------------------------------------------------

  ;; Extension modules are libraries in the lib directory, loaded
  ;; through here -- by the loader at startup, or later by hand -- so
  ;; the kernel knows which modules exist and owns their
  ;; registrations. Loading/reloading runs on the process's owning pump.
  ;; Bootstrap pins its runtime roots and their imports before starting work.
  ;; The kernel itself never reloads.

  (define restart-roots '("kernel"))

  (edoc "Keep modules linked for the life of this image, so a restart cannot discard them."
        (names (list-of string) "the modules"))
  (define (pin-modules! names)
    ;; Monotonic for this image: a running owner cannot discard its linkage.
    (set! restart-roots
      (append (map string-copy names) restart-roots)))

  ;; Membership commits with a module's registrations, including nested
  ;; loads and continuation escapes. This private owner is the kernel's
  ;; catalog lifetime, independent of any extension being reinitialized.
  (define module-catalog (make-registry))
  (define module-catalog-owner (list 'kernel-module-catalog))

  (edoc "The loaded modules, in load order."
        (returns (list-of string)))
  (define (loaded-modules)
    (reverse (registry-items module-catalog)))

  (define (record-module! name)
    (parameterize ([registering-module module-catalog-owner])
      (registry-add! module-catalog name)))

  (define (module-location name)
    ;; (path . library) of a module in the first library root holding it:
    ;; flat in the root as <root>/<name>.sls, the library (name), else under
    ;; a kind directory as <root>/<kind>/<name>.sls, the library (kind name);
    ;; #f when no root has it
    (let roots ([directories (library-directories)])
      (if (null? directories) #f
          (let* ([root (caar directories)] [flat (format "~a/~a.sls" root name)])
            (if (file-exists? flat)
                (cons flat (list (string->symbol name)))
                (let scan ([kinds (guard (ex [else '()]) (directory-list root))])
                  (cond
                    [(null? kinds) (roots (cdr directories))]
                    [(and (not (member (car kinds) '("base" "client")))
                          (file-directory? (string-append root "/" (car kinds)))
                          (file-exists? (format "~a/~a/~a.sls" root (car kinds) name)))
                     (cons (format "~a/~a/~a.sls" root (car kinds) name)
                           (list (string->symbol (car kinds)) (string->symbol name)))]
                    [else (scan (cdr kinds))])))))))

  (edoc "The source file of a module, in the first library root holding it under a kind directory or flat; a missing module's path in the first root."
        (name string "the module")
        (returns file))
  (define (module-source name)
    (let ([hit (module-location name)])
      (if hit (car hit) (format "~a/~a.sls" (caar (library-directories)) name))))

  (edoc "The library a module declares: (kind name) for a module under a library root's kind directory, (name) for one flat in a root."
        (name string "the module")
        (returns list))
  (define (module-library name)
    (let ([hit (module-location name)])
      (if hit (cdr hit) (list (string->symbol name)))))

  ;; The bindings Chez itself provides, so that the editor's public API and
  ;; the modules' definitions can be told apart from builtins: M-x completion
  ;; highlights them, and typed completion scans only them.
  (define baseline-bindings
    (let ([table (make-eq-hashtable)])
      (for-each (lambda (sym) (hashtable-set! table sym #t))
                (environment-symbols (scheme-environment)))
      table))

  (edoc "Whether a symbol is bound at the top level by the editor or its modules rather than by Chez Scheme itself."
        (sym symbol "the name to classify")
        (returns boolean))
  (define (editor-symbol? sym)
    (and (top-level-bound? sym)
         (not (hashtable-ref baseline-bindings sym #f))))

  (edoc "Import a module's library into the editor's top level, compiling it when stale, and run its init! owning its registrations."
        (name string "the module"))
  (define (init-module! name)
    ;; Import the module's library into the editor's top level
    ;; (compiling it when stale) and run its init!, if any, owning its
    ;; registrations.
    (let ([lib (module-library name)])
      ;; every module but one arrives prefixed in the editor's top level,
      ;; exactly as code imports it -- M-x says (store:edit! ...) and
      ;; (edit:save! ...) too. (literal) is bare because its names are how
      ;; values print, (buffer "name") and (window n)
      (eval (if (string=? name "literal")
                `(import ,lib)
                `(import (prefix ,lib
                                 ,(string->symbol
                                    (string-append name ":")))))
            (interaction-environment))
      (when (memq 'init! (library-exports lib))
        (parameterize ([registering-module (string->symbol name)])
          (eval `(let () (import (only ,lib init!)) (init!))
                (interaction-environment))))))

  (edoc "Load a module once: import it, run its init!, and record it; a failed first initialization discards its staged registrations."
        (name string "the module"))
  (define (load-module! name)
    ;; Loading is idempotent.  A failed first initialization also
    ;; discards any registrations it staged before raising.
    (unless (member name (loaded-modules))
      (call-with-registration-update
        (lambda ()
          (init-module! name)
          (record-module! name)))))

  (edoc "Load modules in order, continuing past failures; ((file . condition) ...) for those that failed."
        (names (list-of string) "the modules")
        (returns list))
  (define (load-modules! names)
    ;; Bootstrap selects the modules explicitly. A broken module must not
    ;; keep the others from loading; return ((file . condition) ...) as before.
    (fold-left
      (lambda (failures name)
        (guard (ex [else (cons (cons (string-append name ".sls") ex) failures)])
          (load-module! name)
          failures))
      '() names))

  (edoc "Whether a module's library builds on another, directly or through others."
        (name string "the module")
        (target string "the module it may need")
        (returns boolean))
  (define (module-requires? name target)
    ;; Does library (name) build on (target), directly or through
    ;; others?
    (let ([t (string->symbol target)]
          [seen (make-hashtable equal-hash equal?)])
      (define (leaf lib) (if (pair? (cdr lib)) (leaf (cdr lib)) (car lib)))
      (let walk ([lib (module-library name)])
        (if (hashtable-ref seen lib #f)
            #f
            (begin
              (hashtable-set! seen lib #t)
              (exists (lambda (req) (or (eq? (leaf req) t) (walk req)))
                      (guard (ex [else '()])
                        ;; Import metadata exists even when lazy runtime
                        ;; code has never been invoked (e.g. the sandbox).
                        (library-requirements lib (library-requirements-options import)))))))))

  ;;; The user's configuration -------------------------------------------------

  ;; Bootstrap sets the installation explicitly, independently of source
  ;; lookup and runtime overlays. Direct library users default to their
  ;; initial working directory; capture an absolute name when it is set.
  (edoc "The installation's root directory, canonical."
        (value directory))
  (define installation-directory (make-parameter (current-directory) path:canonical))

  (edoc "A hash of the installation's sources, independent of timestamps and caches: consistency, not authentication."
        (returns string))
  (define (fingerprint)
    ;; Source consistency, independent of runtime roots, timestamps and cache.
    ;; FNV-1a/64 over sorted relative paths and raw contents, each prefixed by
    ;; its byte length (u64 little-endian). This is not an authentication hash.
    (let ([root (string-append (installation-directory) "/lib/")]
          [high #xcbf29ce4] [low #x84222325] [length-bytes (make-bytevector 8)])
      (define (sources relative)
        (apply append
          (map (lambda (name)
                 (let ([path (string-append relative name)])
                   (cond [(file-directory? (string-append root path)) (sources (string-append path "/"))]
                         [(equal? (path-extension name) "sls") (list path)]
                         [else '()])))
            (directory-list (string-append root relative)))))
      (define (add! bytes)
        (do ([i 0 (+ i 1)]) ((= i (bytevector-length bytes)))
          ;; Two 32-bit limbs avoid allocating bignums for every source byte
          ;; on 64-bit Chez. The FNV prime is 2^40 + 435.
          (let* ([next (bitwise-xor low (bytevector-u8-ref bytes i))] [product (* next 435)])
            (set! high (bitwise-and #xffffffff
                         (+ (* high 435) (bitwise-arithmetic-shift-left next 8)
                            (bitwise-arithmetic-shift-right product 32))))
            (set! low (bitwise-and #xffffffff product)))))
      (define (part! bytes)
        (bytevector-u64-set! length-bytes 0 (bytevector-length bytes) (endianness little))
        (add! length-bytes) (add! bytes))
      (for-each
        (lambda (relative)
          (let ([path (string-append root relative)])
            (unless (file-regular? path) (error 'fingerprint "expected a regular library source" path))
            (part! (string->utf8 relative))
            (let ([port (open-file-input-port path)])
              (dynamic-wind void
                (lambda ()
                  (let ([bytes (get-bytevector-all port)])
                    (part! (if (eof-object? bytes) #vu8() bytes))))
                (lambda () (close-port port))))))
        (list-sort string<? (sources "")))
      (let ([hex (string-downcase (number->string (+ (bitwise-arithmetic-shift-left high 32) low) 16))])
        (string-append "fnv1a64:" (make-string (- 16 (string-length hex)) #\0) hex))))

  (define (config-owner side)
    (case side
      [(head) 'config]
      [(base) 'base-config]
      [else (error 'config-file "expected base or head" side)]))

  (edoc "The configuration file of a side, head or base: config.e in the installation for the head, which is the default."
        (side (one-of head base) "the side")
        (returns file))
  (define config-file
    (case-lambda
      [()
       (config-file 'head)]
      [(side)
       (path:canonical
         (string-append (installation-directory) "/"
                        (symbol->string (config-owner side)) ".e"))]))

  (edoc "Load a side's configuration file, the head's config.e by default, into the editor's top level, its registrations owned like a module's and retracted before each load: absent, #t, or the condition an error raised."
        (side (one-of head base) "the side")
        (returns any))
  (define load-config!
    ;; The user's configuration: config.e, plain expressions evaluated
    ;; in the editor's top level (the M-x environment).  Loaded at
    ;; startup once the modules are up, and again after every module
    ;; reload so its settings reapply on top of fresh registrations --
    ;; it must tolerate being loaded any number of times.  Its own
    ;; registrations are owned like a module's, retracted before each
    ;; load, so nothing accumulates.  -> 'absent without a config.e, #t
    ;; when it loaded cleanly, or the condition an error raised (the
    ;; rest of the file unread) for the caller to report.
    (case-lambda
      [()
       (load-config! 'head)]
      [(side)
       (let ([path (config-file side)] [owner (config-owner side)])
         (if (not (file-exists? path))
           'absent
           (guard (ex [else ex])
             (call-with-registration-update
               (lambda ()
                 (retract-module! owner)
                 (parameterize ([registering-module owner])
                   (load path))
                 #t)))))]))

  ;; Layers above hang their after-reload work here (main reapplies
  ;; config, refreshes buffer modes, repaints); hooks receive the
  ;; reloaded module's name and run inside the reload's rollback guard.
  (define after-reload-hooks (make-registry))

  (edoc "Register a hook run after a module reload republishes its registrations."
        (proc thunk "the hook"))
  (define (add-after-reload-hook! proc)
    (registry-add! after-reload-hooks proc))

  (define (reload-order name)
    ;; Capture the affected import graph before redefining any library, and
    ;; load dependencies before their clients, independent of catalog order.
    (let loop ([pending (cons name (filter (lambda (m)
                                             (and (not (string=? m name)) (module-requires? m name)))
                                           (loaded-modules)))]
               [out '()])
      (if (null? pending) (reverse out)
          (let ([next (find (lambda (m)
                              (not (exists (lambda (dependency) (module-requires? m dependency)) pending)))
                            pending)])
            (unless next (error 'reload-module! "cyclic module dependencies" pending))
            (loop (remove next pending) (cons next out))))))

  (edoc "Reload a module in place from its edited source, with every loaded module built on it, re-running their init! before publishing."
        (name* (or string symbol) "the module"))
  (define (reload-module! name*)
    ;; Reload a module in place: redefine its library from the
    ;; (edited) source, likewise every loaded module built on it, then
    ;; stage retraction and run every init! afresh before publishing the
    ;; replacement registrations. Captured closures keep running old code.
    ;; A module's own state starts over unless held in a persistent cell;
    ;; library redefinition and arbitrary effects are outside rollback.
    (let* ([name (if (symbol? name*) (symbol->string name*) name*)]
           [source (module-source name)])
      (call-with-registration-update
        (lambda ()
          (cond [(find (lambda (root)
                         (or (string=? root name) (module-requires? root name)))
                       restart-roots)
                 => (lambda (root)
                      (error 'reload-module!
                        (format "~a pins ~a: restart e to pick up changes" root name)))])
          (unless (file-exists? source)
            (error 'reload-module! "no module source" source))
          (let ([affected (reload-order name)])
            (for-each (lambda (m) (load (module-source m))) affected)
            (unless (member name (loaded-modules))
              (record-module! name))
            ;; Unrelated owners keep their registrations and active work.
            (for-each (lambda (m) (retract-module! (string->symbol m))) affected)
            (for-each init-module! affected))
          (for-each (lambda (hook) (hook name))
                    (registry-items after-reload-hooks)))))))
