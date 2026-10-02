;; Included by store.ss: model contracts need no additional test process.
(let ()
  (import (prefix (core descriptor) descriptor:))
  (define author '(head "models"))
  (define validator-owner (string-copy "model-validator-fixture"))
  (define (get entry key) (cdr (assq key entry)))
  (define (set entry key value)
    (map (lambda (cell) (if (eq? (car cell) key) (cons key value) cell)) entry))
  (define (saved n kind schema value)
    (map cons '(id kind schema scope persistence revision actor references value)
      (list (list 'model n) kind schema 'session 'persistent 4 author '((buffer 99)) value)))
  (define (strings? value) (edoc:type-accepts? '(list-of string) value))
  (define (commit actor changes)
    (call-with-values (lambda () (model:commit! actor changes)) list))
  (define (retire id revision)
    (call-with-values (lambda () (model:retire! author id revision)) list))
  (define (exported) (call-with-values model:export list))
  (define incoming (list (saved 1 'sample 2 (list (string-copy "future")))
                         (saved 3 'unknown 1 '#(opaque (nested . payload)))))

  (let ([owner (string-copy "ports")])
    (parameterize ([kernel:registering-module owner])
      (port:register! '(model port-fixture 1)
        '((output files (list-of file) (value files)) (input optional (or string #f) (value optional)))))
    (let ([r '((id model 999) (kind . port-fixture) (schema . 1)
               (value (files "/tmp/a") (optional . #f)))])
      (test:check 'ports-project-owned-values-and-preserve-false
        (list (port:project r 'files #f) (port:project r 'optional #f)
          (port:project r 'absent #f)
          (test:raises? (lambda () (port:register! '(model invalid-port 1) '((input x (record buffer) (value))))))
          (test:raises? (lambda () (port:register! '(model invalid-port 1)
                                     '((output x string (value)) (input x string (value)))))))
        '((ready ("/tmp/a")) (ready #f) (unavailable contract) #t #t))
      (test:check 'port-definition-retraction-is-transactional
        (let* ([aborted (test:raises? (lambda () (kernel:call-with-registration-update
                                                   (lambda () (kernel:retract-module! owner) (error 'abort "abort")))))]
               [kept (and (port:describe '(model port-fixture 1)) #t)])
          (kernel:retract-module! owner)
          (list aborted kept (port:project r 'files #f)))
        '(#t #t (unavailable contract))))
    (test:check 'entry-port-uses-text-source-and-refuses-multiline
      (map (lambda (lines)
             (port:project '((kind . widget-view) (schema . 3) (value (kind . entry) (schema . 1)))
               'text (list (cons 'revision 3) (cons 'value lines))))
        '(#("hello") #("a" "b")))
      '((ready "hello") (unavailable missing-value))))
  (model:register-kind! 'sample 1 strings?)
  (test:check 'model-import-validates-envelope-and-known-payload-before-install
    (let* ([good (car incoming)] [cycle (list 'cycle)])
      (set-cdr! cycle cycle)
      (list (model:valid-import? 7 incoming)
        (for-all
          (lambda (bad) (not (model:valid-import? 7 bad)))
          (list (append incoming (list good)) (list (set good 'id '(buffer 1)))
                (list (set good 'id '(model 7))) (list (set good 'revision -1))
                (list (set good 'scope '(head ""))) (list (set good 'schema 0))
                (list (set good 'references '(99))) (list (set good 'persistence 'transient))
                (list (set (set good 'schema 1) 'value 42)) (list (set good 'value void)) cycle))
        (test:raises? (lambda () (model:import! 7 (append incoming (list good)))))
        (model:ids))) '(#t #t #t ()))
  (model:import! 7 incoming)
  (test:check 'model-unknown-kind-and-version-survive-owned-import-and-export
    (list (exported) (model:available? '(model 1)) (model:available? '(model 3))
          (car (commit author '(((model 1) 4 () ("changed")))))
          (car (retire '(model 3) 4)))
    (list (list 7 incoming) #f #f 'unavailable 'unavailable))
  (string-set! (car (get (car incoming) 'value)) 0 #\X)
  (let ([copy (model:snapshot '(model 1))]) (string-set! (car (get copy 'value)) 0 #\Y))
  (model:register-kind! 'sample 2 strings?)
  (test:check 'model-late-definition-validates-and-adopts-without-changing-record
    (list (model:available? '(model 1)) (get (model:snapshot '(model 1)) 'value)
          (get (model:snapshot '(model 1)) 'revision)) '(#t ("future") 4))

  (let* ([actor (list 'head (string-copy "owner"))]
         [scope (list 'head (string-copy "desk"))] [refs (list (list 'buffer 9))]
         [value (list (string-copy "initial"))]
         [first (model:create! actor 'sample 1 scope 'persistent refs value)]
         [second (model:create! author 'sample 1 first 'transient '() '("second"))]
         [before (map model:snapshot (list first second))])
    (string-set! (cadr actor) 0 #\X) (string-set! (cadr scope) 0 #\X)
    (set-car! (cdar refs) 100) (string-set! (car value) 0 #\X)
    (test:check 'model-create-owns-all-fields-and-retains-allocator-gaps
      (list first second (map model:snapshot (list first second))
            (test:raises? (lambda () (model:snapshot 7))))
      (list '(model 7) '(model 8) before #t))
    (test:check 'model-stale-invalid-and-duplicate-batches-install-no-prefix
      (list (car (commit author (list (list first 0 '() '("new")) (list second 1 '() '("stale")))))
            (test:raises? (lambda () (commit author (list (list first 0 '() '("new")) (list second 0 '() 99)))))
            (test:raises? (lambda () (commit author (list (list first 0 '() '("one")) (list first 0 '() '("two"))))))
            (map model:snapshot (list first second)))
      (list 'stale #t #t before))
    (let* ([changes (list (list first 0 '((model 3)) '("new")) (list second 0 '() '("other")))]
           [result (commit bot changes)] [basis (cadr result)])
      (test:check 'model-batch-commits-together-and-no-op-retains-revision-and-author
        (list (car result) (map (lambda (entry) (get entry 'revision)) basis)
              (map (lambda (entry) (get entry 'actor)) basis)
              (commit author (map (lambda (change) (cons (car change) (cons 1 (cddr change)))) changes)))
        (list 'applied '(1 1) (list bot bot) (list 'applied basis))))
    (let ([results
           (test:parallel 4
             (lambda (n) (commit author (list (list first 1 '() (list (number->string n)))))))])
      (test:check 'model-one-winner-per-revision
        (list (length (filter (lambda (result) (eq? (car result) 'applied)) results))
              (length (filter (lambda (result) (eq? (car result) 'stale)) results))
              (get (model:snapshot first) 'revision)) '(1 3 2)))
    (test:check 'model-retirement-is-guarded-and-persistence-is-explicit
      (list (car (retire second 0))
            (car (call-with-values (lambda () (model:retire! author second 1 (list (list first 0 '() '("stale neighbor"))))) list))
            (retire second 1) (retire second 1)
            (map (lambda (entry) (get entry 'id)) (cadr (exported))))
      '(stale stale (applied #f) (stale #f) ((model 1) (model 3) (model 7)))))

  ;; Definition loss during validation cannot install a stale operation.
  ;; Concurrent inspection and mutation prove no predicate holds the writer.
  (for-each
    (lambda (interference)
      (let ([entered (test:gate)] [release (test:gate)])
        (parameterize ([kernel:registering-module validator-owner])
          (model:register-kind! 'guarded 1
            (lambda (value)
              (when (equal? value "held") (entered #t) (test:await 'release-model-validator release))
              (string? value))))
        (let* ([id (model:create! author 'guarded 1 'session 'transient '() "initial")]
               [worker (test:worker (lambda () (commit author (list (list id 0 '() "held")))))])
          (test:await 'model-validator-entered entered)
          (if (eq? interference 'edit)
              (commit bot (list (list id 0 '() "intervened")))
              (kernel:retract-module! validator-owner))
          (release #t)
          (test:check (list 'model-validation-race interference)
            (list (car (worker)) (get (model:snapshot id) 'value))
            (if (eq? interference 'edit) '(stale "intervened") '(unavailable "initial"))))
        (kernel:retract-module! validator-owner)))
    '(edit retract))

  ;; Predicates see disposable copies; hostile validators cannot edit state.
  (model:register-kind! 'copy-validator 1
    (lambda (value) (string-set! value 0 #\X) #t))
  (let ([id (model:create! author 'copy-validator 1 'session 'transient '() "copy")])
    (test:check 'model-validator-cannot-mutate-admitted-data
      (list (model:available? id) (get (model:snapshot id) 'value)) '(#t "copy")))
  ;; A blocked delivery must not hold the writer, and backlog is bounded by
  ;; a rescan notice. Revoking another queued listener cancels its callback.
  (let* ([entered (test:gate)] [release (test:gate)] [notices (test:recorder)]
         [cancelled (test:recorder)] [first? #t]
         [removed (model:subscribe! #f cancelled)]
         [token (model:subscribe! #f
                  (lambda (event)
                    (notices event)
                    (when first? (set! first? #f) (entered #t)
                      (test:await 'model-delivery-release release))))]
         [worker (test:worker (lambda () (model:create! author 'sample 1 'session 'transient '() '())))])
    (test:await 'model-delivery-entered entered)
    (do ([i 0 (+ i 1)]) ((= i 257)) (model:create! author 'sample 1 'session 'transient '() '()))
    (model:unsubscribe! removed)
    (release #t) (worker)
    (test:check 'model-bounded-delivery-and-live-owner-check
      (list (length (notices)) (cadr (cadr (notices))) (cancelled)) '(2 #f ()))
    (model:unsubscribe! token))
  (let* ([id (model:create! author 'sample 1 'session 'transient '() '())]
         [seen (test:recorder)]
         [token (model:subscribe! (list id)
                  (lambda (event) (seen (list event (model:snapshots (list id))))))])
    (commit author (list (list id 0 '() '())))
    (commit author (list (list id 0 '() '("new"))))
    (test:check 'model-no-op-silent-and-callback-sees-committed-state
      (list (length (seen)) (get (caddr (car (cadr (cadr (car (seen)))))) 'value)) '(1 ("new")))
    (model:register-kind! 'unrelated 1 string?)
    (test:check 'model-definition-invalidation-does-not-change-record-revision
      (list (cadar (cadr (seen))) (get (model:snapshot id) 'revision)) '(#f 1))
    (model:unsubscribe! token))
  (let* ([id (model:create! author 'sample 1 'session 'transient '() '())]
         [view (view:create! author id 'value 1 '() '(selected . 0))]
         [another (view:create! author id 'value 1 '() '(selected . 10))]
         [claim (call-with-values (lambda () (view:claim! author view)) list)]
         [generation (view:generation (cdr (assoc view (cadr claim))))]
         [entered (test:gate)] [release (test:gate)] [sent (test:recorder)]
         [writer (publication:make!
                   (lambda (state previous)
                     (entered #t) (test:await 'view-publication-release release)
                     (sent state) (view:publish! author (list state))) void values)])
    (define (publish generation sequence) (list view generation sequence 0 (cons 'selected sequence) #f))
    (define (result thunk) (car (call-with-values thunk list)))
    (test:check 'view-ownership-independent-descriptors-and-owner-routing
      (list (car claim) (result (lambda () (view:claim! author view)))
            (result (lambda () (view:set-state! bot view 0 '(selected . 9))))
            (view:state (view:snapshot another)) (model:ids 'widget-view))
      (list 'applied 'owned 'owned '(selected . 10) (list view another)))
    (publication:submit! writer (publish generation 1))
    (test:await 'view-publication-entered entered)
    (do ([n 2 (+ n 1)]) ((= n 101)) (publication:submit! writer (publish generation n)))
    (test:check 'view-held-ack-leaves-saved-state-unchanged (view:state (view:snapshot view)) '(selected . 0))
    (release #t) (publication:flush! writer)
    (test:check 'view-bounded-publication-and-delayed-ack
      (list (map caddr (sent)) (view:state (view:snapshot view))) '((1 100) (selected . 100)))
    (view:release-owner! author)
    (view:claim! author view)
    (test:check 'view-current-generation-publishes-despite-an-unowned-peer
      (list (result (lambda () (view:publish! author (list (publish generation 101)))))
            (result (lambda () (view:release! author view generation)))
            (result (lambda () (view:publish! author (list (publish (+ generation 1) 1)
                                                       (list another 0 1 0 '(selected . 99) #f)))))
            (view:state (view:snapshot view)) (view:state (view:snapshot another)))
      '(applied stale applied (selected . 1) (selected . 10)))
    (view:reset-owners!)
    (test:check 'view-restart-clears-owner-and-keeps-acknowledged-state
      (list (view:owner (view:snapshot view)) (view:state (view:snapshot view))) '(#f (selected . 1))))
  (let* ([root (view:create! author #f 'column 1 '() '())]
         [child (view:create! author #f 'entry 1 '() '())])
    (view:arrange! author (list (list root 0 (list (list 'input child 'fit)) '())) '())
    (view:claim! author root)
    (let ([generation (view:generation (view:snapshot root))])
      (view:retire! author child (get (model:snapshot child) 'revision))
      (test:check 'publication-after-retirement-keeps-live-state-and-clears-dead-focus
        (list (car (call-with-values
                     (lambda () (view:publish! author
                                  (list (list child generation 1 #f 'lost #f)
                                    (list root generation 1 #f 'survived child)))) list))
          (view:state (view:snapshot root)) (view:focus (view:snapshot root)))
        '(applied survived #f))))
  (let* ([root (view:create! author #f 'column 1 '() '())]
         [other (view:create! author #f 'row 1 '() '())]
         [a (view:create! author '(buffer 99) 'entry 1 '() '(0 0))]
         [b (view:create! author '(model 1) 'text 1 '() '(0 0))])
    (define (revision id) (get (model:snapshot id) 'revision))
    (define (arrange rows leases) (call-with-values (lambda () (view:arrange! author rows leases)) list))
    (define (children id ids) (list id (revision id) (map (lambda (id n) (list (string->symbol (number->string n)) id '(grow 1))) ids (iota (length ids))) '()))
    (test:check 'view-tree-attachment-and-cyclic-or-duplicate-refusal
      (list (car (arrange (list (children root (list a b))) '()))
            (view:parent (view:snapshot a))
            (car (arrange (list (children a (list root))) '()))
            (test:raises? (lambda () (arrange (list (children other (list a a))) '())))
            (view:parent (view:snapshot root)))
      (list 'applied root 'invalid #t #f))
    (view:claim! author root)
    (let ([lease (view:generation (view:snapshot root))])
      (test:check 'view-guarded-reparent-atomically-releases-detached-child
        (list (car (arrange (list (children root (list b))) '()))
              (car (arrange (list (children root (list b))) (list (list root lease))))
              (view:owner (view:snapshot a)) (view:parent (view:snapshot a))
              (view:owner (view:snapshot b))
              (car (call-with-values (lambda () (view:publish! author (list (list root lease 1 #f 'old #f)))) list)))
        (list 'owned 'applied #f #f author 'applied)))
    (view:claim! '(head "foreign") a)
    (let ([before (view:tree root)])
      (test:check 'view-foreign-child-refusal-does-not-change-owned-tree
        (list (car (arrange (list (children root (list b a)))
                     (list (list root (view:generation (view:snapshot root))))))
              (equal? before (view:tree root))) '(owned #t)))
    (let* ([notices (test:recorder)] [token (model:subscribe! #f notices)]
           [copy (view:fork! author root)] [leaf (cadar (view:children (view:snapshot copy)))])
      (model:unsubscribe! token)
      (test:check 'view-fork-shares-source-and-resets-recursive-identity
        (list (not (equal? copy root)) (not (equal? leaf b)) (view:parent (view:snapshot leaf))
              (view:source (view:snapshot leaf)) (map (lambda (row) (view:owner (cdr row))) (view:tree copy))
              (length (notices)) (length (cadar (notices))))
        (list #t #t copy '(model 1) '(#f #f) 1 2)))
    (view:release-owner! author) (view:release-owner! '(head "foreign"))
    (let* ([x (view:create! author #f 'row 1 '() '())]
           [y (view:create! author #f 'row 1 '() '())]
           [plans (list (children x (list y)) (children y (list x)))]
           [results (test:parallel 2 (lambda (n) (car (arrange (list (list-ref plans n)) '()))))])
      (test:check 'view-ancestor-witness-prevents-concurrent-cycle
        (list (length (filter (lambda (status) (eq? status 'applied)) results))
              (and (view:parent (view:snapshot x)) (view:parent (view:snapshot y)))) '(1 #f))))
  (let* ([root (view:create! author #f 'copy-fixture 1 '() '())]
         [resource (model:create! author 'sample 1 root 'transient '() '("private"))]
         [failure #f] [rolled-back? #f] [output #f] [retainer #f])
    (view:register-copy! 'copy-fixture 1
      (lambda (options state mapped)
        (values
          (map (lambda (p)
                 (case (car p)
                   [(target) (cons 'target (mapped (cdr p)))]
                   [(owned) (if (eq? failure 'ownership) '(owned) p)]
                   [else p])) options)
          (map mapped state))))
    (view:register-resource-kind! 'sample 1
      (lambda (actor r)
        (set! output (store:create! author "prepared copy output" '("") '((internal . #t))))
        ;; A concurrent interaction wins after resource preparation.
        (case failure
          [(race) (view:set-state! author root #f 'moved)]
          [(owner-change) (view:set-state! author retainer #f 'changed)]
          [(owner-retire) (view:retire! author retainer (model:revision retainer))])
        (let ([prepared output]
              [staged (and (eq? failure 'prepared-persistent) (model:create! author 'sample 1 'session 'persistent '() '("prepared")))])
          (values (lambda (mapped) (list 'sample 1 (mapped (get r 'scope)) (get r 'persistence)
                                     (if staged (list prepared staged) (list prepared)) '("copy")))
            (if staged (list (cons root staged)) '())
            (lambda ()
              (when staged (model:retire! author staged (model:revision staged)))
              (store:delete! author prepared) (set! rolled-back? #t)))))
      (lambda (r) (get r 'references)))
    (view:arrange! author (list (list root 0 '()
                                  (list (list 'owned resource) (cons 'target resource) (cons 'literal resource)))) '())
    (view:set-state! author root #f (list resource '(buffer 99)))
    (let* ([copy (view:fork! author root)] [d (view:snapshot copy)] [copied-resource (car (view:owned d))])
      (test:check 'view-kind-copy-maps-private-data-without-guessing-literal-identities
        (list (view:options d) (view:state d) (get (model:snapshot copied-resource) 'scope))
        (list (list (list 'owned copied-resource) (cons 'target copied-resource) (cons 'literal resource))
          (list copied-resource '(buffer 99)) copy)))
    (let* ([external (view:create! author #f 'container 1 '() '())]
           [borrowed (store:create! author "borrowed copy text" '("keep"))]
           [child (view:create! author borrowed 'label 1 '() '() external)]
           [destination (view:create! author #f 'container 1 '() '())])
      (view:arrange! author
        (list (list root (model:revision root) (list (list 'child child 'fit)) (view:options (view:snapshot root)))) '())
      (view:claim! author destination)
      (let* ([originals (map model:snapshot (list root child resource))] [notices (test:recorder)]
             [token (model:subscribe! #f notices)] [lease (view:generation (view:snapshot destination))]
             [copy (view:fork! author root (list (cons 'owner destination)))] [copied-child (cadar (view:children (view:snapshot copy)))]
             [copied-resource (car (view:owned (view:snapshot copy)))])
        (model:unsubscribe! token)
        (test:check 'view-retained-fork-owns-one-closed-unmounted-copy-in-one-transaction
          (list (view:owned (view:snapshot destination)) (view:children (view:snapshot destination))
            (view:generation (view:snapshot destination)) (length (notices))
            (map (lambda (id) (get (model:snapshot id) 'scope)) (list copy copied-child copied-resource))
            (get (model:snapshot copied-resource) 'persistence)
            (map (lambda (row) (view:owner (cdr row))) (view:tree copy))
            (map model:snapshot (list root child resource)))
          (list (list copy) '() lease 1 (list destination copy copy) 'transient '(#f #f) originals))
        (let* ([committed (test:gate)] [release (test:gate)] [block? #t]
               [token (model:subscribe! (list destination)
                        (lambda (event)
                          (when (and block? (not (model:snapshot copy)))
                            (set! block? #f) (committed #t) (test:await 'view-disposal-release release))))]
               [worker (test:worker
                         (lambda () (call-with-values (lambda () (view:retire! author copy (model:revision copy)))
                                      (lambda (status record) status))))])
          (test:await 'view-disposal-committed committed)
          (test:check 'retained-view-retirement-removes-ownership-and-preserves-output-cleanup-before-deletion
            (list (view:owned (view:snapshot destination)) (descriptor:cleanup (view:snapshot destination))
              (store:exists? output) (map model:snapshot (list copy copied-child copied-resource)))
            (list '() (list output) #t '(#f #f #f)))
          (view:resume!) (view:resume!) (release #t)
          (test:check 'retained-view-retirement-finishes-after-idempotent-recovery (worker) 'applied)
          (model:unsubscribe! token))
        (test:check 'view-retained-fork-disposal-keeps-originals-and-borrowed-text
          (list (map model:snapshot (list copy copied-child copied-resource)) (store:exists? output)
            (store:line borrowed 0) (map model:snapshot (list root child resource)))
          (list '(#f #f #f) #f "keep" originals))
        (view:retire! author destination (model:revision destination))))
    (test:check 'view-fork-rolls-back-output-on-invalid-copy-rule-and-source-or-retainer-races
      (map (lambda (reason)
             (view:set-state! author root #f (list resource '(buffer 99)))
             (set! retainer (view:create! author #f 'container 1 '() '()))
             (set! failure reason) (set! rolled-back? #f)
             (let* ([before (model:ids)] [refused? (test:raises? (lambda () (view:fork! author root (list (cons 'owner retainer)))))])
               (list refused? rolled-back? (store:exists? output)
                 (equal? (if (eq? reason 'owner-retire) (remove retainer before) before) (model:ids)))))
           '(ownership race owner-change owner-retire))
      (make-list 4 '(#t #t #f #t)))
    (set! failure #f)
    (let* ([lifetime (model:create! author 'sample 1 'session 'transient '() '("lifetime"))]
           [destination (view:create! author #f 'container 1 '() '() lifetime)]
           [previous-output output] [before (model:ids)])
      (test:check 'view-retained-fork-refuses-incompatible-persistence-before-preparing
        (list (test:raises? (lambda () (view:fork! author root (list (cons 'owner destination)))))
          (equal? before (model:ids)) (equal? previous-output output)) '(#t #t #t))
      (let* ([prototype (view:create! author #f 'container 1 '() '() lifetime)]
             [resource (model:create! author 'sample 1 prototype 'transient '() '("private"))])
        (view:arrange! author (list (list prototype 0 '() (list (list 'owned resource)))) '())
        (set! failure 'prepared-persistent) (set! rolled-back? #f)
        (let ([before (model:ids)])
          (test:check 'view-retained-fork-rolls-back-prepared-models-with-incompatible-persistence
            (list (test:raises? (lambda () (view:fork! author prototype (list (cons 'owner destination)))))
              rolled-back? (store:exists? output) (equal? before (model:ids))) '(#t #t #f #t)))))
    (set! failure #f)
    (let* ([destination (view:create! author #f 'container 1 '() '())] [before (model:ids)] [previous-output output])
      (view:claim! '(head "foreign") destination)
      (test:check 'view-retained-fork-refuses-foreign-owner-before-resource-preparation
        (list (test:raises? (lambda () (view:fork! author root (list (cons 'owner destination)))))
          (equal? before (model:ids)) (equal? previous-output output)) '(#t #t #t))))
  (let* ([host (view:create! author #f 'container 1 '() '())]
         [destination (view:create! author #f 'container 1 '() '())]
         [template (view:create! author host 'label 1
                     (list (list 'commands (list 'open host 'open (list host)))) '())]
         [copy (view:fork! author template (list (cons 'owner destination) (list 'receivers (list host destination))))]
         [d (view:snapshot copy)] [before (model:ids)])
    (test:check 'fork-rebinds-only-explicit-command-receivers-and-rejects-ambiguous-or-unused-mappings
      (list (view:source d) (descriptor:commands d)
        (map (lambda (rows)
               (test:raises? (lambda () (view:fork! author template (list (cons 'receivers rows))))))
          (list (list (list host destination) (list host destination)) (list (list destination host))))
        (equal? before (model:ids)))
      (list host (list (list 'open destination 'open (list host))) '(#t #t) #t)))
  (let* ([legacy (saved 1 'widget-view 1 (list '(model 3) 'text 1 7 author 9 2 '(4 2)))]
         [upgraded (view:upgrade legacy)] [d (get upgraded 'value)]
         [before (car (exported))])
    (test:check 'view-leaf-migration-and-atomic-allocation-validation
      (list (get upgraded 'schema) (view:source d) (view:parent d) (view:children d)
            (view:generation d) (view:state d)
            (equal? (view:upgrade (cadr incoming)) (cadr incoming))
            (test:raises? (lambda () (model:allocate! author 2
                                       (lambda (ids) (list (list 'sample 1 'session 'persistent '() '("valid"))
                                                       (list 'sample 1 'session 'persistent '() 42))))))
            (= before (car (exported)))) '(3 (model 3) #f () 7 (4 2) #t #t #t)))
  (let* ([root (descriptor:make #f 'extension 7 '((name . "app") (audience (head "models"))) '())]
         [nested (descriptor:with root '((parent model 8)))]
         [private (descriptor:with root '((options (name . "private") (catalogue . #f))))]
         [invalid (descriptor:with root '((options (name . "private") (audience . invalid))))])
    (test:check 'catalogue-migration-preserves-only-former-entries-and-is-idempotent
      (map (lambda (d schema)
             (let* ([upgraded (view:upgrade (saved 7 'widget-view schema d))]
                    [next (get upgraded 'value)])
               (list (get upgraded 'schema) (assq 'catalogue (view:options next))
                 (view:parent next) (equal? upgraded (view:upgrade upgraded)))))
        (list root nested private invalid root root) '(2 2 2 2 3 4))
      '((3 (catalogue . #t) #f #t) (3 #f (model 8) #t) (3 (catalogue . #f) #f #t)
        (3 #f #f #t) (3 #f #f #t) (4 #f #f #t)))
    (test:check 'catalogue-options-require-a-boolean-label-and-valid-audience
      (map (lambda (options) (descriptor:valid? (descriptor:with root (list (cons 'options options)))))
        '(((catalogue . maybe) (name . "bad")) ((catalogue . #t))
          ((catalogue . #t) (name . 7)) ((catalogue . #t) (name . "bad") (audience . invalid))
          ((catalogue . #f)) ((catalogue . #t) (name . "good"))))
      '(#f #f #f #f #t #t)))
  (model:register-kind! 'unknown 1 (lambda (value) #f))
  (test:check 'model-unavailable-data-still-exports-intact
    (list (model:available? '(model 3)) (model:snapshot '(model 3))
          (find (lambda (entry) (equal? (get entry 'id) '(model 3))) (cadr (exported))))
    (list #f (cadr incoming) (cadr incoming))))
