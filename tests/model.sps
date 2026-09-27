;; Included by store.ss: model contracts need no additional test process.
(let ()
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
      (list (car (retire second 0)) (retire second 1) (retire second 1)
            (map (lambda (entry) (get entry 'id)) (cadr (exported))))
      '(stale (applied #f) (stale #f) ((model 1) (model 3) (model 7)))))

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
         [view (view:create! author id 'value 1 '(selected . 0))]
         [another (view:create! author id 'value 1 '(selected . 10))]
         [claim (call-with-values (lambda () (view:claim! author view)) list)]
         [generation (cadddr (cadr claim))]
         [entered (test:gate)] [release (test:gate)] [sent (test:recorder)]
         [writer (publication:make!
                   (lambda (state previous)
                     (entered #t) (test:await 'view-publication-release release)
                     (sent state) (view:publish! author (list state))) void values)])
    (define (publish generation sequence) (list view generation sequence 0 (cons 'selected sequence)))
    (define (result thunk) (car (call-with-values thunk list)))
    (test:check 'view-ownership-independent-descriptors-and-owner-routing
      (list (car claim) (result (lambda () (view:claim! author view)))
            (result (lambda () (view:set-state! bot view 0 '(selected . 9))))
            (list-ref (view:snapshot another) 7)) '(applied owned owned (selected . 10)))
    (publication:submit! writer (publish generation 1))
    (test:await 'view-publication-entered entered)
    (do ([n 2 (+ n 1)]) ((= n 101)) (publication:submit! writer (publish generation n)))
    (test:check 'view-held-ack-leaves-saved-state-unchanged (list-ref (view:snapshot view) 7) '(selected . 0))
    (release #t) (publication:flush! writer)
    (test:check 'view-bounded-publication-and-delayed-ack
      (list (map caddr (sent)) (list-ref (view:snapshot view) 7)) '((1 100) (selected . 100)))
    (view:release-owner! author)
    (view:claim! author view)
    (test:check 'view-old-generation-and-out-of-order-batches-cannot-overwrite
      (list (result (lambda () (view:publish! author (list (publish generation 101)))))
            (result (lambda () (view:release! author view generation)))
            (result (lambda () (view:publish! author (list (publish (+ generation 1) 1)
                                                       (list another 0 1 0 '(selected . 99))))))
            (list-ref (view:snapshot view) 7)) '(stale stale stale (selected . 100)))
    (view:reset-owners!)
    (test:check 'view-restart-clears-owner-and-keeps-acknowledged-state
      (list (list-ref (view:snapshot view) 4) (list-ref (view:snapshot view) 7)) '(#f (selected . 100))))
  (model:register-kind! 'unknown 1 (lambda (value) #f))
  (test:check 'model-unavailable-data-still-exports-intact
    (list (model:available? '(model 3)) (model:snapshot '(model 3))
          (find (lambda (entry) (equal? (get entry 'id) '(model 3))) (cadr (exported))))
    (list #f (cadr incoming) (cadr incoming))))
