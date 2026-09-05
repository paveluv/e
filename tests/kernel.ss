#!/usr/bin/env scheme-script

;; Registry concurrency, atomic module registration, and persistent-cell
;; initialization. Run from the repository root.

(import (chezscheme))
(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (kernel) kernel:)
             (prefix (store) store:)
             (prefix (actor) actor:))

     (define checks 0)
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'kernel-test (symbol->string label) actual expected)))

     (define test-lock (make-mutex))
     (define (await label ready?)
       (let wait ([tries 1000])
         (unless (ready?)
           (when (zero? tries) (error 'kernel-test "worker timeout" label))
           (sleep (make-time 'time-duration 5000000 0))
           (wait (- tries 1)))))

     (define (worker thunk)
       (let ([done? #f] [result #f] [failure #f])
         (fork-thread
           (lambda ()
             (guard (ex [else (with-mutex test-lock (set! failure ex) (set! done? #t))])
               (let ([value (thunk)])
                 (with-mutex test-lock (set! result value) (set! done? #t))))))
         (lambda ()
           (await 'worker (lambda () (with-mutex test-lock done?)))
           (if failure (raise failure) result))))

     (define (gate)
       (let ([open? #f])
         (case-lambda
           [() (with-mutex test-lock open?)]
           [(value) (with-mutex test-lock (set! open? value))])))

     (define (raises? thunk)
       (guard (ex [else #t]) (thunk) #f))

     (define (parallel count thunk)
       (let* ([start (gate)]
              [workers (map (lambda (index)
                              (worker (lambda () (await 'start start) (thunk index))))
                            (iota count))])
         (start #t)
         (map (lambda (finish) (finish)) workers)))

     ;; Hold predicate selection while another thread retracts an owner
     ;; and adds an unrelated registration. Removal must only consume its
     ;; selected entries, never restore or overwrite the older snapshot.
     (define registry (kernel:make-registry))
     (parameterize ([kernel:registering-module 'retired])
       (kernel:registry-add! registry 'retired))
     (kernel:registry-add! registry 'remove)
     (define entered? #f)
     (define release? #f)
     (define removal
       (worker
         (lambda ()
           (kernel:registry-remove! registry
             (lambda (item)
               (unless entered?
                 (with-mutex test-lock (set! entered? #t))
                 (await 'release (lambda () (with-mutex test-lock release?))))
               (eq? item 'remove))))))
     (await 'selection (lambda () (with-mutex test-lock entered?)))
     ((worker (lambda ()
                (kernel:retract-module! 'retired)
                (kernel:registry-add! registry 'concurrent))))
     (with-mutex test-lock (set! release? #t))
     (removal)
     (check 'removal-preserves-concurrent-add-and-retraction
            (kernel:registry-items registry) '(concurrent))

     (define additions (kernel:make-registry))
     (parallel 8
       (lambda (index)
         (for-each (lambda (n) (kernel:registry-add! additions (list index n))) (iota 300))))
     (check 'concurrent-additions-retained (length (kernel:registry-items additions)) 2400)
     (check 'concurrent-additions-distinct
            (let ([seen (make-hashtable equal-hash equal?)])
              (for-each (lambda (item) (hashtable-set! seen item #t)) (kernel:registry-items additions))
              (hashtable-size seen)) 2400)

     (define owned-copy (kernel:registry-entries registry))
     (set-car! (car owned-copy) 'retired)
     (set-cdr! owned-copy '(fake))
     (kernel:retract-module! 'retired)
     (check 'ownership-and-spines-are-private
            (kernel:registry-entries registry) '((#f . concurrent)))
     (check 'find-predicate-may-register
            ((worker (lambda ()
                       (kernel:registry-find registry
                         (lambda (item) (kernel:registry-add! registry 'from-find) #t)))))
            'concurrent)
     (check 'find-predicate-registration-retained
            (kernel:registry-items registry) '(from-find concurrent))
     (check 'removal-predicate-error-propagates
            (raises? (lambda ()
                       (kernel:registry-remove! registry
                         (lambda (item)
                           (when (eq? item 'concurrent) (error 'fixture "predicate failed"))
                           #t)))) #t)
     (check 'failed-selection-removes-nothing
            (kernel:registry-items registry) '(from-find concurrent))

     ;; A staged replacement stays private, then merges into the latest
     ;; committed state, including a concurrent addition and revocation.
     (define staged (kernel:make-registry))
     (parameterize ([kernel:registering-module 'updating]) (kernel:registry-add! staged 'old))
     (kernel:registry-add! staged 'original)
     (kernel:registry-add! staged 'revoked)
     (define stage-ready (gate))
     (define stage-release (gate))
     (define secondary #f)
     (define stage-worker
       (worker
         (lambda ()
           (kernel:call-with-registration-update
             (lambda ()
               (kernel:retract-module! 'updating)
               (parameterize ([kernel:registering-module 'updating]) (kernel:registry-add! staged 'new))
               (kernel:registry-add! staged 'temporary)
               (kernel:registry-remove! staged (lambda (item) (eq? item 'temporary)))
               (set! secondary (kernel:make-registry))
               (kernel:registry-add! secondary 'second)
               (let ([before (kernel:registry-items staged)])
                 (stage-ready #t)
                 (await 'stage-release stage-release)
                 (list before (kernel:registry-items staged))))))))
     (await 'stage-ready stage-ready)
     (define during-stage
       ((worker
          (lambda ()
            (let ([before (list (kernel:registry-items staged) (kernel:registry-items secondary))])
              (kernel:registry-remove! staged (lambda (item) (eq? item 'revoked)))
              (kernel:registry-add! staged 'outsider)
              before)))))
     (stage-release #t)
     (check 'other-threads-see-committed-registrations during-stage '((revoked original old) ()))
     (check 'updating-thread-sees-own-and-later-committed-work
            (stage-worker) '((new revoked original) (new outsider original)))
     (check 'commit-merges-concurrent-registration-and-revocation
            (kernel:registry-items staged) '(new outsider original))
     (check 'new-registry-publishes-with-the-batch (kernel:registry-items secondary) '(second))

     ;; Failure discards only staged effects, including new registries.
     (define rollback (kernel:make-registry))
     (parameterize ([kernel:registering-module 'rollback-owner])
       (kernel:registry-add! rollback 'kept)
       (kernel:registry-add! rollback 'revoked))
     (define rollback-ready (gate))
     (define rollback-release (gate))
     (define abandoned #f)
     (define rollback-worker
       (worker
         (lambda ()
           (kernel:call-with-registration-update
             (lambda ()
               (kernel:retract-module! 'rollback-owner)
               (kernel:registry-add! rollback 'discarded)
               (set! abandoned (kernel:make-registry))
               (kernel:registry-add! abandoned 'discarded)
               (rollback-ready #t)
               (await 'rollback-release rollback-release)
               (error 'fixture "abort"))))))
     (await 'rollback-ready rollback-ready)
     ((worker
        (lambda ()
          (kernel:registry-remove! rollback (lambda (item) (eq? item 'revoked)))
          (kernel:registry-add! rollback 'outsider))))
     (rollback-release #t)
     (check 'update-failure-propagates (raises? rollback-worker) #t)
     (check 'rollback-keeps-concurrent-effects (kernel:registry-items rollback) '(outsider kept))
     (check 'rollback-clears-new-registry (kernel:registry-items abandoned) '())

     (define nested (kernel:make-registry))
     (check 'updates-preserve-multiple-values
            (call-with-values
              (lambda ()
                (kernel:call-with-registration-update
                  (lambda ()
                    (kernel:registry-add! nested 'outer)
                    (kernel:call-with-registration-update (lambda () (kernel:registry-add! nested 'child)))
                    (guard (ex [else (void)])
                      (kernel:call-with-registration-update
                        (lambda ()
                          (kernel:registry-remove! nested (lambda (item) #t))
                          (kernel:registry-add! nested 'failed)
                          (error 'fixture "inner abort"))))
                    (values 'result (kernel:registry-items nested)))))
              list)
            '(result (child outer)))
     (check 'nested-commit-publishes-once (kernel:registry-items nested) '(child outer))
     (check 'parent-failure-propagates
            (raises? (lambda ()
                       (kernel:call-with-registration-update
                         (lambda ()
                           (kernel:call-with-registration-update (lambda () (kernel:registry-add! nested 'lost)))
                           (error 'fixture "outer abort"))))) #t)
     (check 'parent-failure-discards-successful-child (kernel:registry-items nested) '(child outer))
     (check 'escape-leaves-update
            (call/cc (lambda (escape)
                       (kernel:call-with-registration-update
                         (lambda () (kernel:registry-add! nested 'escaped) (escape 'out)))))
            'out)
     (check 'escape-discards-staged-state (kernel:registry-items nested) '(child outer))

     ;; Forked workers do not inherit staging from their creator, even if
     ;; its dynamic parameter is still installed when they register.
     (check 'creator-failure-propagates
            (raises? (lambda ()
                       (kernel:call-with-registration-update
                         (lambda ()
                           (kernel:registry-add! nested 'creator)
                           ((worker (lambda () (kernel:registry-add! nested 'worker))))
                           (error 'fixture "creator failed"))))) #t)
     (check 'worker-registration-survives-creator
            (kernel:registry-items nested) '(worker child outer))

     ;; Store mutations and actor messages are runtime effects even when
     ;; an initializer triggers them. They use published registrations;
     ;; their callbacks must not join the caller's staged update/owner.
     (define runtime-registry (kernel:make-registry))
     (define runtime-events '())
     (define runtime-buffer (store:create! '(head test) "runtime" '("")))
     (define runtime-token
       (store:subscribe! runtime-buffer
         (lambda (event)
           (set! runtime-events (cons 'published runtime-events))
           (kernel:registry-add! runtime-registry 'store-callback))))
     (raises?
       (lambda ()
         (parameterize ([kernel:registering-module 'initializer])
           (kernel:call-with-registration-update
             (lambda ()
               (store:unsubscribe! runtime-token)
               (store:subscribe! runtime-buffer (lambda (event) (set! runtime-events (cons 'staged runtime-events))))
               (store:rename! '(head test) runtime-buffer "renamed")
               (kernel:registry-add! runtime-registry 'initializer)
               (error 'fixture "abort after publishing event"))))))
     (check 'store-events-use-published-subscriptions runtime-events '(published))
     (check 'store-callbacks-have-independent-runtime-registrations
            (kernel:registry-entries runtime-registry) '((#f . store-callback)))
     (store:unsubscribe! runtime-token)
     (store:delete! '(head test) runtime-buffer)

     (define recipient '(agent "registry-runtime"))
     (define delivered '())
     (actor:register! recipient
       (lambda (message)
         (set! delivered (cons 'published delivered))
         (kernel:registry-add! runtime-registry 'actor-callback)))
     (raises?
       (lambda ()
         (parameterize ([kernel:registering-module 'initializer])
           (kernel:call-with-registration-update
             (lambda ()
               (actor:register! recipient (lambda (message) (set! delivered (cons 'staged delivered))))
               (actor:send! recipient 'message)
               (kernel:registry-add! runtime-registry 'initializer)
               (error 'fixture "abort after sending"))))))
     (check 'actor-messages-use-published-delivery delivered '(published))
     (check 'actor-delivery-has-independent-runtime-registrations
            (kernel:registry-entries runtime-registry) '((#f . actor-callback) (#f . store-callback)))
     (define runtime-ticket
       (actor:ask! '(agent "asker") recipient "Reply?" '()
         (lambda (answer) (kernel:registry-add! runtime-registry 'reply-callback))))
     (raises?
       (lambda ()
         (parameterize ([kernel:registering-module 'initializer])
           (kernel:call-with-registration-update
             (lambda ()
               (actor:answer! runtime-ticket "yes")
               (error 'fixture "abort after replying"))))))
     (check 'actor-reply-has-independent-runtime-registrations
            (car (kernel:registry-entries runtime-registry)) '(#f . reply-callback))
     (check 'actor-ticket-stays-consumed (actor:answer! runtime-ticket "again") #f)

     ;; Persistent initialization is single-owner, but does not hold the
     ;; table lock while calling out. Nested keys and concurrent waiters
     ;; share the final box rather than constructing rival cells.
     (define constructors 0)
     (define cells
       (parallel 8
         (lambda (index)
           (kernel:persistent-cell 'kernel-parallel
             (lambda ()
               (with-mutex test-lock (set! constructors (+ constructors 1)))
               (sleep (make-time 'time-duration 20000000 0))
               '(initial))))))
     (check 'one-initializer constructors 1)
     (check 'one-persistent-box (for-all (lambda (cell) (eq? cell (car cells))) cells) #t)
     (check 'existing-cell-skips-constructor
            (eq? (car cells) (kernel:persistent-cell 'kernel-parallel (lambda () (error 'fixture "unused")))) #t)
     (check 'constructor-can-request-another-key
            (unbox ((worker (lambda ()
                              (kernel:persistent-cell 'kernel-outer
                                (lambda () (unbox (kernel:persistent-cell 'kernel-inner (lambda () 'inner)))))))))
            'inner)

     (define initialization-ready (gate))
     (define initialization-release (gate))
     (define failed-initialization
       (worker
         (lambda ()
           (kernel:persistent-cell 'kernel-retry
             (lambda ()
               (initialization-ready #t)
               (await 'initialization-release initialization-release)
               (error 'fixture "initialization failed"))))))
     (await 'initialization-ready initialization-ready)
     (define waiting 0)
     (define retries 0)
     (define waiters
       (map (lambda (index)
              (worker
                (lambda ()
                  (with-mutex test-lock (set! waiting (+ waiting 1)))
                  (kernel:persistent-cell 'kernel-retry
                    (lambda ()
                      (with-mutex test-lock (set! retries (+ retries 1)))
                      'retried)))))
            (iota 8)))
     (await 'waiters (lambda () (with-mutex test-lock (= waiting 8))))
     (check 'unrelated-key-initializes-while-waiters-block
            (unbox ((worker (lambda () (kernel:persistent-cell 'kernel-unrelated (lambda () 'independent))))))
            'independent)
     (initialization-release #t)
     (check 'constructor-failure-propagates (raises? failed-initialization) #t)
     (define retried (map (lambda (finish) (finish)) waiters))
     (check 'waiters-retry-one-constructor retries 1)
     (check 'waiters-share-retried-box (for-all (lambda (cell) (eq? cell (car retried))) retried) #t)
     (check 'waiters-get-retried-value (unbox (car retried)) 'retried)
     (check 'recursive-key-is-rejected
            (raises? (lambda ()
                       (kernel:persistent-cell 'kernel-recursive
                         (lambda () (kernel:persistent-cell 'kernel-recursive (lambda () 'bad)))))) #t)
     (check 'recursive-failure-releases-key
            (unbox (kernel:persistent-cell 'kernel-recursive (lambda () 'recovered))) 'recovered)
     (check 'constructor-can-escape
            (call/cc (lambda (escape)
                       (kernel:persistent-cell 'kernel-escape (lambda () (escape 'escaped))))) 'escaped)
     (check 'constructor-escape-releases-key
            (unbox (kernel:persistent-cell 'kernel-escape (lambda () 'recovered))) 'recovered)

     (define (failure-message thunk)
       (guard (ex [else (and (condition? ex) (message-condition? ex) (condition-message ex))])
         (thunk) #f))
     (define once (kernel:make-registry))
     (check 'completed-update-cannot-be-resumed
            (failure-message
              (worker
                (lambda ()
                  (let ([resume #f])
                    (kernel:call-with-registration-update
                      (lambda ()
                        (call/cc (lambda (k) (set! resume k)))
                        (kernel:registry-add! once 'once)))
                    (resume #t)))))
            "cannot resume a closed update")
     (check 'resumption-cannot-republish (kernel:registry-items once) '(once))
     (check 'completed-initializer-cannot-be-resumed
            (failure-message
              (worker
                (lambda ()
                  (let ([resume #f])
                    (kernel:persistent-cell 'kernel-once
                      (lambda ()
                        (call/cc (lambda (k) (set! resume k)))
                        'once))
                    (resume #t)))))
            "cannot resume a closed initialization")
     (check 'resumption-keeps-persistent-cell
            (unbox (kernel:persistent-cell 'kernel-once (lambda () (error 'fixture "unused")))) 'once)

     ;; Real library initialization, redefinition, after-reload hooks, and
     ;; configuration share the same publication boundary. Fixtures live
     ;; in an isolated installation, never in the user's lib/config paths.
     (define scratch (format "/tmp/e-kernel-test-~a-~a" (time-second (current-time)) (random 1000000)))
     (define sources (string-append scratch "/lib"))
     (define objects (string-append scratch "/eo"))
     (mkdir scratch)
     (mkdir sources)
     (mkdir objects)
     (define (fixture-control name)
       (kernel:persistent-cell (list 'kernel-fixture name) (lambda () (lambda (version) (void)))))
     (define (write-fixture name version)
       (call-with-output-file (string-append sources "/" name ".e")
         (lambda (port)
           (pretty-print
             `(library (,(string->symbol name))
                (export init! version)
                (import (rnrs) (only (chezscheme) unbox) (prefix (kernel) kernel:))
                (define (version) ',version)
                (define (init!)
                  ((unbox (kernel:persistent-cell '(kernel-fixture ,name)
                            (lambda () (error 'fixture "missing control")))) version)))
             port))
         'replace))
     (define fixture-registry (kernel:make-registry))
     (define family-registry (kernel:make-registry))
     (define fixture-created #f)
     (define config-registry (kernel:make-registry))
     (define config-control (kernel:persistent-cell 'kernel-fixture-config (lambda () void)))
     (define module-baseline (kernel:loaded-modules))
     (define hook-control (lambda (name) (void)))
     (parameterize ([kernel:registering-module 'kernel-test-hook])
       (kernel:add-after-reload-hook! (lambda (name) (hook-control name))))
     (define (labels) (map car (kernel:registry-items fixture-registry)))
     (define (active-version)
       ((cdr (kernel:registry-find fixture-registry (lambda (entry) (eq? (car entry) 'active))))))

     (dynamic-wind
       void
       (lambda ()
         (parameterize ([library-directories (cons (cons sources objects) (library-directories))])
           (write-fixture "kernel-child" 'child)
           (write-fixture "kernel-parent" 'parent)
           (write-fixture "kernel-fixture" 'version-one)
           (set-box! (fixture-control "kernel-child")
             (lambda (version) (kernel:registry-add! family-registry 'child)))
           (set-box! (fixture-control "kernel-parent")
             (lambda (version)
               (kernel:load-module! "kernel-child")
               (kernel:registry-add! family-registry 'parent)
               (error 'fixture "parent failed")))
           (check 'nested-load-failure-propagates (raises? (lambda () (kernel:load-module! "kernel-parent"))) #t)
           (check 'failed-parent-discards-child-membership (kernel:loaded-modules) module-baseline)
           (check 'failed-parent-discards-child-registration (kernel:registry-items family-registry) '())

           (set-box! (fixture-control "kernel-parent")
             (lambda (version)
               (kernel:load-module! "kernel-child")
               (kernel:registry-add! family-registry 'parent)))
           (check 'explicit-update-can-abort-successful-load
                  (raises? (lambda ()
                             (kernel:call-with-registration-update
                               (lambda ()
                                 (kernel:load-module! "kernel-parent")
                                 (error 'fixture "outer failure"))))) #t)
           (check 'membership-follows-outer-update (kernel:loaded-modules) module-baseline)
           (check 'registration-follows-outer-update (kernel:registry-items family-registry) '())
           (check 'reload-initializer-can-escape
                  (call/cc (lambda (escape)
                             (set-box! (fixture-control "kernel-parent")
                               (lambda (version)
                                 (kernel:load-module! "kernel-child")
                                 (escape 'escaped)))
                             (kernel:reload-module! "kernel-parent"))) 'escaped)
           (check 'reload-escape-discards-new-membership (kernel:loaded-modules) module-baseline)
           (check 'reload-escape-discards-child-registration (kernel:registry-items family-registry) '())

           (set-box! (fixture-control "kernel-fixture")
             (lambda (version)
               (kernel:registry-add! fixture-registry (cons 'discarded version))
               (set! fixture-created (kernel:make-registry))
               (kernel:registry-add! fixture-created 'discarded)
               ((worker (lambda ()
                          (parameterize ([kernel:registering-module #f])
                            (kernel:registry-add! fixture-registry (cons 'outsider void))))))
               (error 'fixture "first load failed")))
           (check 'first-load-failure-propagates (raises? (lambda () (kernel:load-module! "kernel-fixture"))) #t)
           (check 'first-load-keeps-runtime-registration (labels) '(outsider))
           (check 'first-load-discards-new-registry (kernel:registry-items fixture-created) '())
           (check 'first-load-keeps-module-catalog (kernel:loaded-modules) module-baseline)

           (set-box! (fixture-control "kernel-fixture")
             (lambda (version) (kernel:registry-add! fixture-registry (cons 'active version))))
           (kernel:load-module! "kernel-fixture")
           (check 'successful-load-publishes-catalog (kernel:loaded-modules) (append module-baseline '("kernel-fixture")))
           (check 'successful-load-publishes-callback (active-version) 'version-one)
           (kernel:load-module! "kernel-fixture")
           (check 'module-load-is-idempotent (labels) '(active outsider))

           (write-fixture "kernel-fixture" 'version-two)
           (set! hook-control
             (lambda (name)
               ((worker (lambda ()
                          (kernel:registry-add! fixture-registry (cons 'late void)))))
               (error 'fixture "reload hook failed")))
           (check 'reload-hook-failure-propagates (raises? (lambda () (kernel:reload-module! "kernel-fixture"))) #t)
           (check 'reload-failure-keeps-runtime-addition (labels) '(late active outsider))
           (check 'reload-failure-keeps-old-callback (active-version) 'version-one)
           ;; Library redefinition is not a registry operation: a failed
           ;; reload can retain old callbacks while the new export exists.
           (check 'library-redefinition-is-not-rolled-back
                  (eval '(kernel-fixture:version) (interaction-environment)) 'version-two)

           (set! hook-control
             (lambda (name)
               ((worker (lambda ()
                          (kernel:registry-remove! fixture-registry (lambda (entry) (eq? (car entry) 'active)))
                          (kernel:registry-add! fixture-registry (cons 'revoker void)))))
               (error 'fixture "reload hook failed after revocation")))
           (check 'reload-revocation-failure-propagates (raises? (lambda () (kernel:reload-module! "kernel-fixture"))) #t)
           (check 'reload-failure-does-not-resurrect-revoked-callback (labels) '(revoker late outsider))
           (check 'reload-failure-does-not-duplicate-catalog
                  (kernel:loaded-modules) (append module-baseline '("kernel-fixture")))
           (set! hook-control (lambda (name) (void)))
           (kernel:reload-module! "kernel-fixture")
           (check 'reload-success-publishes-one-new-callback (labels) '(active revoker late outsider))
           (check 'reload-success-publishes-new-version (active-version) 'version-two)

           (call-with-output-file (kernel:config-file)
             (lambda (port)
               (write '((unbox (kernel:persistent-cell 'kernel-fixture-config
                                (lambda () (error 'fixture "missing configuration control"))))) port))
             'replace)
           (set-box! config-control (lambda () (kernel:registry-add! config-registry 'old-config)))
           (check 'config-load-succeeds (kernel:load-config!) #t)
           (set-box! config-control
             (lambda ()
               (kernel:registry-add! config-registry 'partial-config)
               (kernel:load-module! "kernel-child")
               ((worker (lambda ()
                          (parameterize ([kernel:registering-module #f])
                            (kernel:registry-add! config-registry 'runtime)))))
               (error 'fixture "configuration failed")))
           (check 'config-failure-returns-condition (condition? (kernel:load-config!)) #t)
           (check 'config-failure-keeps-old-and-concurrent-registrations
                  (kernel:registry-items config-registry) '(runtime old-config))
           (check 'config-failure-discards-nested-module
                  (kernel:loaded-modules) (append module-baseline '("kernel-fixture")))
           (set-box! config-control (lambda () (kernel:registry-add! config-registry 'new-config)))
           (set! hook-control
             (lambda (name)
               (kernel:load-config!)
               (error 'fixture "outer reload failed after config")))
           (check 'reload-failure-after-config-propagates (raises? (lambda () (kernel:reload-module! "kernel-fixture"))) #t)
           (check 'nested-config-follows-reload-rollback
                  (kernel:registry-items config-registry) '(runtime old-config))
           (set! hook-control (lambda (name) (void)))
           (check 'config-retry-succeeds (kernel:load-config!) #t)
           (check 'config-retry-publishes-complete-replacement
                  (kernel:registry-items config-registry) '(new-config runtime))))
       (lambda ()
         (kernel:retract-module! 'kernel-test-hook)
         (for-each
           (lambda (directory)
             (for-each (lambda (file) (delete-file (string-append directory "/" file))) (directory-list directory))
             (delete-directory directory))
           (list objects sources))
         (when (file-exists? (string-append scratch "/config.e")) (delete-file (string-append scratch "/config.e")))
         (delete-directory scratch)))

     (format #t "~a kernel checks passed\n" checks)))
