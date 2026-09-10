;; kernel.e -- the kernel: the substrate every other module stands
;; on.  Persistent cells (module state that survives hot reloads), the
;; registries and the module lifecycle, the mailboxes actors wait on,
;; and the text of a caught condition.  The kernel imports nothing
;; above itself and, like main, is never reloaded in place.

(library (kernel)
  (export persistent-cell
          registering-module make-registry registry-add!
          registry-items registry-entries registry-find
          registry-remove! registry-observe! registry-unobserve!
          registration-conflict?
          retract-module! call-with-registration-update call-with-runtime-registrations
          module-source loaded-modules
          init-module! load-module! load-modules! module-requires? pin-modules!
          reload-module! add-after-reload-hook!
          installation-directory config-file load-config!
          make-read-only-error read-only-error? make-refusal refusal?
          make-mailbox mailbox-post! mailbox-receive!
          make-delivery-queue enqueue-delivery! drain-deliveries!
          condition-text)
  (import (rnrs)
          (only (chezscheme)
                box unbox make-hashtable equal-hash
                make-parameter make-thread-parameter current-directory format interaction-environment eval
                library-exports library-requirements library-requirements-options
                library-directories load
                parameterize make-mutex with-mutex make-condition
                condition-wait condition-signal condition-broadcast
                with-interrupts-disabled make-time
                current-time time? time-type time<? time-difference
                get-thread-id box? display-condition void)
          (prefix (path) path:))

  ;;; Conditions --------------------------------------------------------------

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
  (define-condition-type &read-only &error make-read-only-error
    read-only-error?)
  (define-condition-type &refused &error make-refusal refusal?)

  ;;; Mailboxes ---------------------------------------------------------------

  ;; The scheduling substrate: a mailbox is a thread-safe FIFO with a
  ;; blocking receive.  Actors -- the main loop first of all -- wait
  ;; on their mailbox; any thread posts.

  (define-record-type (mailbox %make-mailbox mailbox?)
    (fields lock signal (mutable head) (mutable tail)))

  (define (make-mailbox)
    (%make-mailbox (make-mutex) (make-condition) '() '()))

  (define (mailbox-post! mb message)
    (with-mutex (mailbox-lock mb)
      (with-interrupts-disabled
        (mailbox-tail-set! mb (cons message (mailbox-tail mb))))
      (condition-signal (mailbox-signal mb))))

  (define signal-check-interval (make-time 'time-duration 100000000 0))

  (define mailbox-receive!
    ;; Strictly FIFO. A monotonic deadline returns #f on timeout. Signal
    ;; owners can opt into bounded waits and an interrupt check outside the
    ;; mailbox lock. These checks neither return a timeout nor replace an
    ;; evaluation's timer; only actual messages/deadlines reach the caller.
    (case-lambda
      [(mb) (mailbox-receive! mb #f #f)]
      [(mb deadline) (mailbox-receive! mb deadline #f)]
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

  (define (make-delivery-queue)
    (%make-delivery-queue (make-mutex) '() '() #f))

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

  (define registering-module (make-thread-parameter #f))
  (define registry-lock (make-mutex))
  (define-record-type (registry %make-registry registry?)
    (fields key-of (mutable contents)))
  (define-record-type registration
    (fields owner key item))
  (define-condition-type &registration-conflict &error make-registration-conflict
    registration-conflict?)
  (define registries '())

  ;; Registration updates stage entry identities, never whole snapshots.
  ;; The initiating thread reads its own changes; other threads see the
  ;; committed lists until the outer update publishes all its deltas.
  (define-record-type registration-update
    (fields thread parent (mutable state) (mutable deltas)))
  (define-record-type registration-delta
    (fields registry (mutable additions) (mutable removals)))
  (define current-registration-update (make-thread-parameter #f))

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

  (define make-registry
    (case-lambda
      [() (make-registry #f)]
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

  (define (registry-items r) (map registration-item (registry-read r)))

  (define (registry-entries r)
    ;; Preserve the public shape without exposing ownership/identity
    ;; wrappers to mutation. The registering module still owns its item.
    (map (lambda (entry) (cons (registration-owner entry) (registration-item entry)))
         (registry-read r)))

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

  (define (registry-observe! r proc)
    ;; proc receives (removed-items added-items), one batch per commit.
    ;; Like store subscribers, observers only hear future commits and
    ;; can revoke callbacks already queued, but not one already running.
    (unless (procedure? proc) (error 'registry-observe! "expected a procedure" proc))
    (let ([token (list 'observer)])
      (registry-add! registration-observers (list token r proc))
      token))

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

  (define (pin-modules! names)
    ;; Monotonic for this image: a running owner cannot discard its linkage.
    (set! restart-roots
      (append (map string-copy names) restart-roots)))

  ;; Membership commits with a module's registrations, including nested
  ;; loads and continuation escapes. This private owner is the kernel's
  ;; catalog lifetime, independent of any extension being reinitialized.
  (define module-catalog (make-registry))
  (define module-catalog-owner (list 'kernel-module-catalog))

  (define (loaded-modules) (reverse (registry-items module-catalog)))

  (define (record-module! name)
    (parameterize ([registering-module module-catalog-owner])
      (registry-add! module-catalog name)))

  (define (module-source name)
    (let loop ([directories (library-directories)])
      (let ([path (format "~a/~a.e" (caar directories) name)])
        (if (or (file-exists? path) (null? (cdr directories))) path
            (loop (cdr directories))))))

  (define (init-module! name)
    ;; Import the module's library into the editor's top level
    ;; (compiling it when stale) and run its init!, if any, owning its
    ;; registrations.
    (let ([lib (list (string->symbol name))])
      ;; every module but the command layer arrives prefixed in the
      ;; editor's top level, exactly as code imports it -- M-x says
      ;; (store:edit! ...) and (terminal:open!!) too; (edit) alone is
      ;; bare, being what M-x is for
      (eval (if (string=? name "edit")
                `(import ,lib)
                `(import (prefix ,lib
                                 ,(string->symbol
                                    (string-append name ":")))))
            (interaction-environment))
      (when (memq 'init! (library-exports lib))
        (parameterize ([registering-module (string->symbol name)])
          (eval `(let () (import (only ,lib init!)) (init!))
                (interaction-environment))))))

  (define (load-module! name)
    ;; Loading is idempotent.  A failed first initialization also
    ;; discards any registrations it staged before raising.
    (unless (member name (loaded-modules))
      (call-with-registration-update
        (lambda ()
          (init-module! name)
          (record-module! name)))))

  (define (load-modules! names)
    ;; Bootstrap selects the modules explicitly. A broken module must not
    ;; keep the others from loading; return ((file . condition) ...) as before.
    (fold-left
      (lambda (failures name)
        (guard (ex [else (cons (cons (string-append name ".e") ex) failures)])
          (load-module! name)
          failures))
      '() names))

  (define (module-requires? name target)
    ;; Does library (name) build on (target), directly or through
    ;; others?
    (let ([t (string->symbol target)]
          [seen (make-hashtable equal-hash equal?)])
      (let walk ([lib (list (string->symbol name))])
        (if (hashtable-ref seen lib #f)
            #f
            (begin
              (hashtable-set! seen lib #t)
              (exists (lambda (req) (or (eq? (car req) t) (walk req)))
                      (guard (ex [else '()])
                        ;; Import metadata exists even when lazy runtime
                        ;; code has never been invoked (e.g. the sandbox).
                        (library-requirements lib (library-requirements-options import)))))))))

  ;;; The user's configuration -------------------------------------------------

  ;; Bootstrap sets the installation explicitly, independently of source
  ;; lookup and runtime overlays. Direct library users default to their
  ;; initial working directory; capture an absolute name when it is set.
  (define installation-directory (make-parameter (current-directory) path:canonical))

  (define (config-owner side)
    (case side
      [(head) 'config]
      [(base) 'base-config]
      [else (error 'config-file "expected base or head" side)]))

  (define config-file
    (case-lambda
      [() (config-file 'head)]
      [(side)
       (path:canonical
         (string-append (installation-directory) "/"
                        (symbol->string (config-owner side)) ".e"))]))

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
      [() (load-config! 'head)]
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
