#!/usr/bin/env scheme-script

;; Registry concurrency, atomic module registration, and persistent-cell
;; initialization. Run from the repository root.

(import (chezscheme))
(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (test) test:)
             (prefix (kernel) kernel:)
             (prefix (store) store:)
             (prefix (actor) actor:))

     (define test-lock (make-mutex))

     ;; Timed receives retain FIFO and later messages after a timeout.
     (let ([mail (kernel:make-mailbox)] [entered (test:gate)])
       (define (after ns) (add-duration (current-time 'time-monotonic) (make-time 'time-duration ns 0)))
       (kernel:mailbox-post! mail 'first)
       (kernel:mailbox-post! mail 'second)
       (let* ([first (kernel:mailbox-receive! mail (current-time 'time-monotonic))]
              [second (kernel:mailbox-receive! mail)]
              [expired (kernel:mailbox-receive! mail (after 1000000))]
              [waiting (test:worker (lambda () (entered #t) (kernel:mailbox-receive! mail (after 900000000))))])
         (test:await 'receiving entered)
         (kernel:mailbox-post! mail 'awake)
         (test:check 'mailbox-deadline-keeps-queue-and-wake-semantics
           (list first second expired (waiting)) '(first second #f awake))))

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
       (test:worker
         (lambda ()
           (kernel:registry-remove! registry
             (lambda (item)
               (unless entered?
                 (with-mutex test-lock (set! entered? #t))
                 (test:await 'release (lambda () (with-mutex test-lock release?))))
               (eq? item 'remove))))))
     (test:await 'selection (lambda () (with-mutex test-lock entered?)))
     ((test:worker (lambda ()
                     (kernel:retract-module! 'retired)
                     (kernel:registry-add! registry 'concurrent))))
     (with-mutex test-lock (set! release? #t))
     (removal)
     (test:check 'removal-preserves-concurrent-add-and-retraction
       (kernel:registry-items registry) '(concurrent))

     (define additions (kernel:make-registry))
     (test:parallel 8
       (lambda (index)
         (for-each (lambda (n) (kernel:registry-add! additions (list index n))) (iota 300))))
     (test:check 'concurrent-additions-retained (length (kernel:registry-items additions)) 2400)
     (test:check 'concurrent-additions-distinct
       (let ([seen (make-hashtable equal-hash equal?)])
         (for-each (lambda (item) (hashtable-set! seen item #t)) (kernel:registry-items additions))
         (hashtable-size seen)) 2400)

     (define owned-copy (kernel:registry-entries registry))
     (set-car! (car owned-copy) 'retired)
     (set-cdr! owned-copy '(fake))
     (kernel:retract-module! 'retired)
     (test:check 'ownership-and-spines-are-private
       (kernel:registry-entries registry) '((#f . concurrent)))
     (test:check 'find-predicate-may-register
       ((test:worker (lambda ()
                       (kernel:registry-find registry
                         (lambda (item) (kernel:registry-add! registry 'from-find) #t)))))
       'concurrent)
     (test:check 'find-predicate-registration-retained
       (kernel:registry-items registry) '(from-find concurrent))
     (test:check 'removal-predicate-error-propagates
       (test:raises? (lambda ()
                       (kernel:registry-remove! registry
                         (lambda (item)
                           (when (eq? item 'concurrent) (error 'fixture "predicate failed"))
                           #t)))) #t)
     (test:check 'failed-selection-removes-nothing
       (kernel:registry-items registry) '(from-find concurrent))

     ;; A staged replacement stays private, then merges into the latest
     ;; committed state, including a concurrent addition and revocation.
     (define staged (kernel:make-registry))
     (parameterize ([kernel:registering-module 'updating]) (kernel:registry-add! staged 'old))
     (kernel:registry-add! staged 'original)
     (kernel:registry-add! staged 'revoked)
     (define stage-ready (test:gate))
     (define stage-release (test:gate))
     (define secondary #f)
     (define stage-worker
       (test:worker
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
                 (test:await 'stage-release stage-release)
                 (list before (kernel:registry-items staged))))))))
     (test:await 'stage-ready stage-ready)
     (define during-stage
       ((test:worker
          (lambda ()
            (let ([before (list (kernel:registry-items staged) (kernel:registry-items secondary))])
              (kernel:registry-remove! staged (lambda (item) (eq? item 'revoked)))
              (kernel:registry-add! staged 'outsider)
              before)))))
     (stage-release #t)
     (test:check 'other-threads-see-committed-registrations during-stage '((revoked original old) ()))
     (test:check 'updating-thread-sees-own-and-later-committed-work
       (stage-worker) '((new revoked original) (new outsider original)))
     (test:check 'commit-merges-concurrent-registration-and-revocation
       (kernel:registry-items staged) '(new outsider original))
     (test:check 'new-registry-publishes-with-the-batch (kernel:registry-items secondary) '(second))

     ;; Failure discards only staged effects, including new registries.
     (define rollback (kernel:make-registry))
     (parameterize ([kernel:registering-module 'rollback-owner])
       (kernel:registry-add! rollback 'kept)
       (kernel:registry-add! rollback 'revoked))
     (define rollback-ready (test:gate))
     (define rollback-release (test:gate))
     (define abandoned #f)
     (define rollback-worker
       (test:worker
         (lambda ()
           (kernel:call-with-registration-update
             (lambda ()
               (kernel:retract-module! 'rollback-owner)
               (kernel:registry-add! rollback 'discarded)
               (set! abandoned (kernel:make-registry))
               (kernel:registry-add! abandoned 'discarded)
               (rollback-ready #t)
               (test:await 'rollback-release rollback-release)
               (error 'fixture "abort"))))))
     (test:await 'rollback-ready rollback-ready)
     ((test:worker
        (lambda ()
          (kernel:registry-remove! rollback (lambda (item) (eq? item 'revoked)))
          (kernel:registry-add! rollback 'outsider))))
     (rollback-release #t)
     (test:check 'update-failure-propagates (test:raises? rollback-worker) #t)
     (test:check 'rollback-keeps-concurrent-effects (kernel:registry-items rollback) '(outsider kept))
     (test:check 'rollback-clears-new-registry (kernel:registry-items abandoned) '())

     (define nested (kernel:make-registry))
     (test:check 'updates-preserve-multiple-values
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
     (test:check 'nested-commit-publishes-once (kernel:registry-items nested) '(child outer))
     (test:check 'parent-failure-propagates
       (test:raises? (lambda ()
                       (kernel:call-with-registration-update
                         (lambda ()
                           (kernel:call-with-registration-update (lambda () (kernel:registry-add! nested 'lost)))
                           (error 'fixture "outer abort"))))) #t)
     (test:check 'parent-failure-discards-successful-child (kernel:registry-items nested) '(child outer))
     (test:check 'escape-leaves-update
       (call/cc (lambda (escape)
                  (kernel:call-with-registration-update
                    (lambda () (kernel:registry-add! nested 'escaped) (escape 'out)))))
       'out)
     (test:check 'escape-discards-staged-state (kernel:registry-items nested) '(child outer))

     ;; Forked workers do not inherit staging from their creator, even if
     ;; its dynamic parameter is still installed when they register.
     (test:check 'creator-failure-propagates
       (test:raises? (lambda ()
                       (kernel:call-with-registration-update
                         (lambda ()
                           (kernel:registry-add! nested 'creator)
                           ((test:worker (lambda () (kernel:registry-add! nested 'worker))))
                           (error 'fixture "creator failed"))))) #t)
     (test:check 'worker-registration-survives-creator
       (kernel:registry-items nested) '(worker child outer))

     ;; Two updates overlap deliberately. Chez make-parameter is shared
     ;; across threads: the old scopes could overwrite each other's owner
     ;; and active update. Both contenders stage the same key; only one
     ;; whole update may publish, including its unrelated registrations.
     (define companions (kernel:make-registry))
     (define claims
       (kernel:make-registry
         (lambda (item) (kernel:registry-items companions) (car item))))
     (define claim-events (test:recorder))
     (define claim-watch
       (kernel:registry-observe! claims
         (lambda (removed added)
           (claim-events (list removed added (kernel:registry-entries companions)
                               (kernel:registering-module))))))
     (define (contender owner)
       (let* ([ready (test:gate)] [release (test:gate)]
              [finish
               (test:worker
                 (lambda ()
                   (guard (ex [(kernel:registration-conflict? ex) 'conflict])
                     (parameterize ([kernel:registering-module owner])
                       (kernel:call-with-registration-update
                         (lambda ()
                           (kernel:registry-add! claims (cons 'name owner))
                           (kernel:registry-add! companions owner)
                           (ready #t)
                           (test:await owner release)
                           (list (kernel:registering-module) (kernel:registry-items claims))))))))])
         (test:await owner ready)
         (cons release finish)))
     (define left (contender 'left))
     (define right (contender 'right))
     (test:check 'overlapping-scopes-stay-private
       (list (kernel:registering-module) (kernel:registry-items claims)
             (kernel:registry-items companions) (claim-events)) '(#f () () ()))
     (kernel:call-with-runtime-registrations
       (lambda () (kernel:registry-add! companions 'runtime)))
     ((car left) #t)
     (test:check 'first-claim-publishes-with-its-owner ((cdr left)) '(left ((name . left))))
     ((car right) #t)
     (test:check 'lost-claim-discards-all-registrations
       (list ((cdr right)) (kernel:registry-items claims) (kernel:registry-entries companions)
             (claim-events))
       '(conflict ((name . left)) ((left . left) (#f . runtime))
                  ((() ((name . left)) ((left . left) (#f . runtime)) #f))))
     ;; Duplicate keys in a single nested update and a direct add obey the
     ;; same admission rule. An add removed before commit never appears.
     (test:check 'duplicate-claims-rejected
       (map (lambda (update?)
              (test:raises?
                (lambda ()
                  (if update?
                      (kernel:call-with-registration-update
                        (lambda ()
                          (kernel:registry-remove! claims (lambda (item) #t))
                          (kernel:registry-add! claims '(fresh . outer))
                          (kernel:call-with-registration-update
                            (lambda () (kernel:registry-add! claims '(fresh . inner))))))
                      (kernel:registry-add! claims '(name . direct))))
                kernel:registration-conflict?))
            '(#f #t)) '(#t #t))
     (kernel:call-with-registration-update
       (lambda ()
         (kernel:registry-add! claims '(temporary . removed))
         (kernel:registry-remove! claims (lambda (item) (eq? (car item) 'temporary)))))
     (test:check 'failed-and-empty-updates-publish-nothing
       (list (kernel:registry-items claims) (length (claim-events))) '(((name . left)) 1))
     (define owned-observations (test:recorder))
     (kernel:call-with-registration-update
       (lambda ()
         (parameterize ([kernel:registering-module 'observed-module])
           (kernel:registry-observe! claims
             (lambda (removed added) (owned-observations (list removed added))))
           (kernel:registry-add! claims '(owned . registration)))))
     (kernel:retract-module! 'observed-module)
     (test:check 'observation-starts-and-stops-at-its-own-commit
       (owned-observations) '((() ((owned . registration)))))
     (kernel:registry-unobserve! claim-watch)

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
     (test:raises?
       (lambda ()
         (parameterize ([kernel:registering-module 'initializer])
           (kernel:call-with-registration-update
             (lambda ()
               (store:unsubscribe! runtime-token)
               (store:subscribe! runtime-buffer (lambda (event) (set! runtime-events (cons 'staged runtime-events))))
               (store:rename! '(head test) runtime-buffer "renamed")
               (kernel:registry-add! runtime-registry 'initializer)
               (error 'fixture "abort after publishing event"))))))
     (test:check 'store-events-use-published-subscriptions runtime-events '(published))
     (test:check 'store-callbacks-have-independent-runtime-registrations
       (kernel:registry-entries runtime-registry) '((#f . store-callback)))
     (store:unsubscribe! runtime-token)
     (store:delete! '(head test) runtime-buffer)

     (define recipient '(agent "registry-runtime"))
     (define delivered '())
     (actor:register! recipient
       (lambda (message)
         (set! delivered (cons 'published delivered))
         (kernel:registry-add! runtime-registry 'actor-callback)))
     (test:raises?
       (lambda ()
         (parameterize ([kernel:registering-module 'initializer])
           (kernel:call-with-registration-update
             (lambda ()
               (actor:detach! recipient)
               (actor:register! recipient (lambda (message) (set! delivered (cons 'staged delivered))))
               (actor:send! recipient 'message)
               (kernel:registry-add! runtime-registry 'initializer)
               (error 'fixture "abort after sending"))))))
     (test:check 'actor-messages-use-published-delivery delivered '(published))
     (test:check 'actor-delivery-has-independent-runtime-registrations
       (kernel:registry-entries runtime-registry) '((#f . actor-callback) (#f . store-callback)))
     (define runtime-ticket
       (actor:ask! '(agent "asker") recipient "Reply?" '()
         (lambda (answer) (kernel:registry-add! runtime-registry 'reply-callback))))
     (test:raises?
       (lambda ()
         (parameterize ([kernel:registering-module 'initializer])
           (kernel:call-with-registration-update
             (lambda ()
               (actor:answer! runtime-ticket "yes")
               (error 'fixture "abort after replying"))))))
     (test:check 'actor-reply-has-independent-runtime-registrations
       (car (kernel:registry-entries runtime-registry)) '(#f . reply-callback))
     (test:check 'actor-ticket-stays-consumed (actor:answer! runtime-ticket "again") #f)

     ;; Persistent initialization is single-owner, but does not hold the
     ;; table lock while calling out. Nested keys and concurrent waiters
     ;; share the final box rather than constructing rival cells.
     (define constructors 0)
     (define cells
       (test:parallel 8
         (lambda (index)
           (kernel:persistent-cell 'kernel-parallel
             (lambda ()
               (with-mutex test-lock (set! constructors (+ constructors 1)))
               (sleep (make-time 'time-duration 20000000 0))
               '(initial))))))
     (test:check 'one-initializer constructors 1)
     (test:check 'one-persistent-box (for-all (lambda (cell) (eq? cell (car cells))) cells) #t)
     (test:check 'existing-cell-skips-constructor
       (eq? (car cells) (kernel:persistent-cell 'kernel-parallel (lambda () (error 'fixture "unused")))) #t)
     (test:check 'constructor-can-request-another-key
       (unbox ((test:worker (lambda ()
                              (kernel:persistent-cell 'kernel-outer
                                (lambda () (unbox (kernel:persistent-cell 'kernel-inner (lambda () 'inner)))))))))
       'inner)

     (define initialization-ready (test:gate))
     (define initialization-release (test:gate))
     (define failed-initialization
       (test:worker
         (lambda ()
           (kernel:persistent-cell 'kernel-retry
             (lambda ()
               (initialization-ready #t)
               (test:await 'initialization-release initialization-release)
               (error 'fixture "initialization failed"))))))
     (test:await 'initialization-ready initialization-ready)
     (define waiting 0)
     (define retries 0)
     (define waiters
       (map (lambda (index)
              (test:worker
                (lambda ()
                  (with-mutex test-lock (set! waiting (+ waiting 1)))
                  (kernel:persistent-cell 'kernel-retry
                    (lambda ()
                      (with-mutex test-lock (set! retries (+ retries 1)))
                      'retried)))))
            (iota 8)))
     (test:await 'waiters (lambda () (with-mutex test-lock (= waiting 8))))
     (test:check 'unrelated-key-initializes-while-waiters-block
       (unbox ((test:worker (lambda () (kernel:persistent-cell 'kernel-unrelated (lambda () 'independent))))))
       'independent)
     (initialization-release #t)
     (test:check 'constructor-failure-propagates (test:raises? failed-initialization) #t)
     (define retried (map (lambda (finish) (finish)) waiters))
     (test:check 'waiters-retry-one-constructor retries 1)
     (test:check 'waiters-share-retried-box (for-all (lambda (cell) (eq? cell (car retried))) retried) #t)
     (test:check 'waiters-get-retried-value (unbox (car retried)) 'retried)
     (test:check 'recursive-key-is-rejected
       (test:raises? (lambda ()
                       (kernel:persistent-cell 'kernel-recursive
                         (lambda () (kernel:persistent-cell 'kernel-recursive (lambda () 'bad)))))) #t)
     (test:check 'recursive-failure-releases-key
       (unbox (kernel:persistent-cell 'kernel-recursive (lambda () 'recovered))) 'recovered)
     (test:check 'constructor-can-escape
       (call/cc (lambda (escape)
                  (kernel:persistent-cell 'kernel-escape (lambda () (escape 'escaped))))) 'escaped)
     (test:check 'constructor-escape-releases-key
       (unbox (kernel:persistent-cell 'kernel-escape (lambda () 'recovered))) 'recovered)

     (define (failure-message thunk)
       (guard (ex [else (and (condition? ex) (message-condition? ex) (condition-message ex))])
         (thunk) #f))
     (define once (kernel:make-registry))
     (test:check 'completed-update-cannot-be-resumed
       (failure-message
         (test:worker
           (lambda ()
             (let ([resume #f])
               (kernel:call-with-registration-update
                 (lambda ()
                   (call/cc (lambda (k) (set! resume k)))
                   (kernel:registry-add! once 'once)))
               (resume #t)))))
       "cannot resume a closed update")
     (test:check 'resumption-cannot-republish (kernel:registry-items once) '(once))
     (test:check 'completed-initializer-cannot-be-resumed
       (failure-message
         (test:worker
           (lambda ()
             (let ([resume #f])
               (kernel:persistent-cell 'kernel-once
                 (lambda ()
                   (call/cc (lambda (k) (set! resume k)))
                   'once))
               (resume #t)))))
       "cannot resume a closed initialization")
     (test:check 'resumption-keeps-persistent-cell
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
     (define (write-library name form)
       (call-with-output-file (string-append sources "/" name ".e")
         (lambda (port) (pretty-print form port))
         'replace))
     (define (write-fixture name version)
       (write-library name
         `(library (,(string->symbol name))
            (export init! version)
            (import (rnrs) (only (chezscheme) unbox) (prefix (kernel) kernel:))
            (define (version) ',version)
            (define (init!)
              ((unbox (kernel:persistent-cell '(kernel-fixture ,name)
                        (lambda () (error 'fixture "missing control")))) version)))))
     (define (write-reload-root version)
       (write-library "reload-z"
         `(library (reload-z) (export value) (import (rnrs))
            (define (value) ,version))))
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
           (test:check 'nested-load-failure-propagates (test:raises? (lambda () (kernel:load-module! "kernel-parent"))) #t)
           (test:check 'failed-parent-discards-child-membership (kernel:loaded-modules) module-baseline)
           (test:check 'failed-parent-discards-child-registration (kernel:registry-items family-registry) '())

           (set-box! (fixture-control "kernel-parent")
             (lambda (version)
               (kernel:load-module! "kernel-child")
               (kernel:registry-add! family-registry 'parent)))
           (test:check 'explicit-update-can-abort-successful-load
             (test:raises? (lambda ()
                             (kernel:call-with-registration-update
                               (lambda ()
                                 (kernel:load-module! "kernel-parent")
                                 (error 'fixture "outer failure"))))) #t)
           (test:check 'membership-follows-outer-update (kernel:loaded-modules) module-baseline)
           (test:check 'registration-follows-outer-update (kernel:registry-items family-registry) '())
           (test:check 'reload-initializer-can-escape
             (call/cc (lambda (escape)
                        (set-box! (fixture-control "kernel-parent")
                                  (lambda (version)
                                    (kernel:load-module! "kernel-child")
                                    (escape 'escaped)))
                        (kernel:reload-module! "kernel-parent"))) 'escaped)
           (test:check 'reload-escape-discards-new-membership (kernel:loaded-modules) module-baseline)
           (test:check 'reload-escape-discards-child-registration (kernel:registry-items family-registry) '())

           (set-box! (fixture-control "kernel-fixture")
             (lambda (version)
               (kernel:registry-add! fixture-registry (cons 'discarded version))
               (set! fixture-created (kernel:make-registry))
               (kernel:registry-add! fixture-created 'discarded)
               ((test:worker (lambda ()
                               (parameterize ([kernel:registering-module #f])
                                 (kernel:registry-add! fixture-registry (cons 'outsider void))))))
               (error 'fixture "first load failed")))
           (test:check 'first-load-failure-propagates (test:raises? (lambda () (kernel:load-module! "kernel-fixture"))) #t)
           (test:check 'first-load-keeps-runtime-registration (labels) '(outsider))
           (test:check 'first-load-discards-new-registry (kernel:registry-items fixture-created) '())
           (test:check 'first-load-keeps-module-catalog (kernel:loaded-modules) module-baseline)

           (set-box! (fixture-control "kernel-fixture")
             (lambda (version) (kernel:registry-add! fixture-registry (cons 'active version))))
           (kernel:load-module! "kernel-fixture")
           (test:check 'successful-load-publishes-catalog (kernel:loaded-modules) (append module-baseline '("kernel-fixture")))
           (test:check 'successful-load-publishes-callback (active-version) 'version-one)
           (kernel:load-module! "kernel-fixture")
           (test:check 'module-load-is-idempotent (labels) '(active outsider))

           (write-fixture "kernel-fixture" 'version-two)
           (set! hook-control
             (lambda (name)
               ((test:worker (lambda ()
                               (kernel:registry-add! fixture-registry (cons 'late void)))))
               (error 'fixture "reload hook failed")))
           (test:check 'reload-hook-failure-propagates (test:raises? (lambda () (kernel:reload-module! "kernel-fixture"))) #t)
           (test:check 'reload-failure-keeps-runtime-addition (labels) '(late active outsider))
           (test:check 'reload-failure-keeps-old-callback (active-version) 'version-one)
           ;; Library redefinition is not a registry operation: a failed
           ;; reload can retain old callbacks while the new export exists.
           (test:check 'library-redefinition-is-not-rolled-back
             (eval '(kernel-fixture:version) (interaction-environment)) 'version-two)

           (set! hook-control
             (lambda (name)
               ((test:worker (lambda ()
                               (kernel:registry-remove! fixture-registry (lambda (entry) (eq? (car entry) 'active)))
                               (kernel:registry-add! fixture-registry (cons 'revoker void)))))
               (error 'fixture "reload hook failed after revocation")))
           (test:check 'reload-revocation-failure-propagates (test:raises? (lambda () (kernel:reload-module! "kernel-fixture"))) #t)
           (test:check 'reload-failure-does-not-resurrect-revoked-callback (labels) '(revoker late outsider))
           (test:check 'reload-failure-does-not-duplicate-catalog
             (kernel:loaded-modules) (append module-baseline '("kernel-fixture")))
           (set! hook-control (lambda (name) (void)))
           (kernel:reload-module! "kernel-fixture")
           (test:check 'reload-success-publishes-one-new-callback (labels) '(active revoker late outsider))
           (test:check 'reload-success-publishes-new-version (active-version) 'version-two)

           (call-with-output-file (kernel:config-file)
             (lambda (port)
               (write '((unbox (kernel:persistent-cell 'kernel-fixture-config
                                 (lambda () (error 'fixture "missing configuration control"))))) port))
             'replace)
           (set-box! config-control (lambda () (kernel:registry-add! config-registry 'old-config)))
           (test:check 'config-load-succeeds (kernel:load-config!) #t)
           (set-box! config-control
             (lambda ()
               (kernel:registry-add! config-registry 'partial-config)
               (kernel:load-module! "kernel-child")
               ((test:worker (lambda ()
                               (parameterize ([kernel:registering-module #f])
                                 (kernel:registry-add! config-registry 'runtime)))))
               (error 'fixture "configuration failed")))
           (test:check 'config-failure-returns-condition (condition? (kernel:load-config!)) #t)
           (test:check 'config-failure-keeps-old-and-concurrent-registrations
             (kernel:registry-items config-registry) '(runtime old-config))
           (test:check 'config-failure-discards-nested-module
             (kernel:loaded-modules) (append module-baseline '("kernel-fixture")))
           (set-box! config-control (lambda () (kernel:registry-add! config-registry 'new-config)))
           (set! hook-control
             (lambda (name)
               (kernel:load-config!)
               (error 'fixture "outer reload failed after config")))
           (test:check 'reload-failure-after-config-propagates (test:raises? (lambda () (kernel:reload-module! "kernel-fixture"))) #t)
           (test:check 'nested-config-follows-reload-rollback
             (kernel:registry-items config-registry) '(runtime old-config))
           (set! hook-control (lambda (name) (void)))
           (test:check 'config-retry-succeeds (kernel:load-config!) #t)
           (test:check 'config-retry-publishes-complete-replacement
             (kernel:registry-items config-registry) '(new-config runtime))

           ;; Imported, uninvoked clients have no runtime metadata yet.
           ;; Register the chain in reverse dependency order, then redefine
           ;; its root before any client is used (the sandbox reload case).
           (write-reload-root 1)
           (for-each
             (lambda (entry)
               (write-library (symbol->string (car entry))
                 `(library (,(car entry)) (export value)
                    (import (rnrs) (prefix (,(cadr entry)) upstream:))
                    (define (value) (upstream:value)))))
             '((reload-m reload-z) (reload-a reload-m)))
           (for-each kernel:load-module! '("reload-a" "reload-m" "reload-z"))
           (test:check 'uninvoked-import-dependencies-are-visible
             (kernel:module-requires? "reload-a" "reload-z") #t)
           (write-reload-root 2)
           (kernel:reload-module! "reload-z")
           (test:check 'reload-orders-the-complete-import-graph
             (eval '(list (reload-a:value) (reload-m:value) (reload-z:value))) '(2 2 2))))
       (lambda ()
         (kernel:retract-module! 'kernel-test-hook)
         (for-each
           (lambda (directory)
             (for-each (lambda (file) (delete-file (string-append directory "/" file))) (directory-list directory))
             (delete-directory directory))
           (list objects sources))
         (when (file-exists? (string-append scratch "/config.e")) (delete-file (string-append scratch "/config.e")))
         (delete-directory scratch)))

     (test:finish! 'kernel)))
