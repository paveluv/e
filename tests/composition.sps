;; One fixture covers the durable protocol without any head/window runtime.
(let ()
  (define who '(head "composition"))
  (define session (policy:mint! who (policy:make 'all 1000 'any 8000)))
  (define (get r key) (cdr (assq key r)))
  (define (invoke session name contract args)
    (actor:call-as (policy:session-actor session)
      (lambda ()
        (cdr (operation:dispatch! name contract args
               (lambda (admission) (unless (eq? admission 'control) (error 'test "expected control"))) session)))))
  (define (acquire session profile)
    (invoke session 'composition:acquire! '((string) (list list)) (list profile)))
  (define (admit session binding root basis owner)
    (invoke session 'composition:admit! '((list (or model #f) list (or model #f (one-of retire))) (symbol datum list))
      (list binding root basis owner)))
  (define (finish session binding)
    (invoke session 'composition:finish! '((list) (symbol datum)) (list binding)))
  (define (root) (view:create! who #f 'unknown-widget 1 '() '()))
  (define (owner root)
    (model:create! who 'composition-test-owner 1 'session 'persistent (list root) '()))
  (define (root-state id)
    (let ([d (view:snapshot id)]) (list (view:owner d) (view:generation d))))
  (define validate-owner list?)

  (kernel:load-module! "composition")
  (model:register-kind! 'composition-test-owner 1 (lambda (value) (validate-owner value)))
  (let* ([before (model:ids)]
         [attempts (test:parallel 4 (lambda (n) (acquire session "blank")))]
         [binding (caar attempts)] [empty (admit session binding #f '() #f)])
    (check 'composition-one-slot-and-distinct-empty-state
      (list (length (filter (lambda (id) (not (member id before))) (model:ids)))
        (for-all (lambda (r) (equal? (car r) binding)) attempts)
        (get (get binding 'value) 'initialized?) (car empty)
        (get (get (cadr empty) 'value) 'initialized?) (get (get (cadr empty) 'value) 'root)
        (get (cadr empty) 'references) (caddr empty))
      '(1 #t #f applied #t #f () ()))
    (check 'composition-stale-binding-and-independent-profile
      (list (car (admit session binding #f '() #f))
        (equal? (get binding 'id) (get (car (acquire session "other")) 'id)))
      '(stale #f)))

  (let* ([binding (car (acquire session "tree"))] [a (root)] [child (root)] [b (root)])
    (let* ([owner (model:create! who 'composition-test-owner 1 'session 'transient '() '())]
           [temporary (view:create! who #f 'label 1 '() '() owner)])
      (check 'composition-refuses-roots-that-would-vanish-on-recovery
        (list (car (admit session binding temporary (view:tree temporary) #f))
          (model:snapshot (get binding 'id)) (root-state temporary))
        (list 'transient binding '(#f 0))))
    (view:arrange! who (list (list a 0 (list (list 'child child 'fit)) '())) '())
    (let* ([prepared (view:tree a)] [first (admit session binding a prepared #f)]
           [installed (cadr first)] [owned (view:tree a)] [kept (owner a)])
      (check 'composition-admission-atomically-claims-complete-tree
        (list (car first) (get installed 'references)
          (caddr first)
          (map root-state (list a child)))
        (list 'applied (list a) owned
          (list (list who 1) (list who 1))))
      (check 'composition-same-attachment-reacquire-is-idempotent
        (let ([again (acquire session "tree")])
          (list (car again) (cadr again) (car (admit session installed a (reverse owned) #f))))
        (list installed owned 'applied))
      (let ([basis (view:tree b)])
        (view:set-state! who b #f '(changed))
        (check 'composition-refusals-preserve-binding-and-both-trees
          (list (car (admit session installed b basis kept))
            (test:raises? (lambda () (admit session installed b (view:tree b) #f)))
            (model:snapshot (get installed 'id)) (view:tree a) (view:owner (view:snapshot b)))
          (list 'stale #t installed owned #f)))
      (let ([entered (test:gate)] [release (test:gate)] [block? #t])
        (set! validate-owner
          (lambda (value)
            (when block? (set! block? #f) (entered #t) (test:await 'retainer-release release))
            (list? value)))
        (let ([worker (test:worker (lambda () (admit session installed b (view:tree b) kept)))])
          (test:await 'retainer-validation entered)
          (model:retire! who kept (model:revision kept))
          (release #t)
          (check 'composition-retainer-lifetime-is-an-atomic-witness
            (list (car (worker)) (model:snapshot (get installed 'id)) (view:tree a) (root-state b))
            (list 'stale installed owned '(#f 0))))
        (set! validate-owner list?)
        (set! kept (owner a)))
      (let* ([switched (admit session installed b (view:tree b) kept)] [current (cadr switched)])
        (check 'composition-retention-releases-old-lease-without-deleting-it
          (list (car switched) (get current 'references) (map root-state (list a child b))
            (get (model:snapshot kept) 'references) (caddr switched))
          (list 'applied (list b) (list '(#f 2) '(#f 2) (list who 1)) (list a) (view:tree b)))
        (let ([other (policy:mint! '(head "other-composition") (policy:make 'all 1000 'any 8000))])
          (check 'composition-other-head-cannot-write-or-steal-root
            (list (test:raises? (lambda () (admit other current #f '() (owner b))))
              (car (admit other (car (acquire other "tree")) b (view:tree b) #f))) '(#t owned))
          (policy:revoke! other))
        (policy:revoke! session)
        (let* ([next (policy:mint! who (policy:make 'all 1000 'any 8000))]
               [resumed (acquire next "tree")])
          (check 'composition-reconnect-fences-old-attachment-and-generations
            (list (test:raises? (lambda () (acquire session "tree")))
              (test:raises? (lambda () (admit session current #f '() (owner b))))
              (car (admit next current #f '() (owner b)))
              (root-state b) (get (get (car resumed) 'value) 'root))
            (list #t #t 'stale (list who 2) b))
          ;; Validate only this fixture's records: the preceding model tests
          ;; deliberately retain values whose validators reject them.
          (let-values ([(next-id records) (model:export)])
            (check 'composition-saved-model-contract
              (model:valid-import? next-id
                (filter (lambda (r) (or (eq? (get r 'kind) 'composition-binding)
                                        (member (get r 'id) (list a child b kept)))) records)) #t)
            (policy:revoke! next)
            (view:recover!)
            (let* ([restored (policy:mint! who (policy:make 'all 1000 'any 8000))]
                   [acquired (acquire restored "tree")])
              (check 'composition-recovery-preserves-root-and-explicit-empty
                (list (get (get (car acquired) 'value) 'root) (view:owner (view:snapshot b))
                  (get (get (car (acquire restored "blank")) 'value) 'initialized?))
                (list b who #t))
              (policy:revoke! restored)))))))

  (let* ([session (policy:mint! who (policy:make 'all 1000 'any 8000))]
         [binding (car (acquire session "disposal"))] [old (root)] [candidate (root)]
         [borrowed (store:create! who "borrowed composition text" '("keep"))]
         [outputs (map (lambda (name) (store:create! who name '("owned"))) '("owned one" "owned two"))]
         [resource-check list?] [late #f])
    (model:register-kind! 'composition-output 1 (lambda (v) (resource-check v)))
    (view:register-resource-kind! 'composition-output 1
      (lambda (actor r) (error 'test "copy is unused"))
      (lambda (r) (get r 'value)))
    (let* ([resource (model:create! who 'composition-output 1 old 'persistent outputs outputs)]
           [child (view:create! who borrowed 'label 1 '() '())]
           [scoped (view:create! who borrowed 'label 1 '() '() resource)]
           [foreign (root)])
      (view:arrange! who (list (list old 0 (list (list 'child child 'fit)) (list (list 'owned resource)))
                               (list foreign 0 (list (list 'scoped scoped 'fit)) '())) '())
      (view:claim! who foreign)
      (view:publish! who (list (list foreign (view:generation (view:snapshot foreign)) 1 #f '() scoped)))
      (let* ([installed (cadr (admit session binding old (view:tree old) #f))]
             [bad (view:create! who (car outputs) 'label 1 '() '())])
        (check 'composition-cannot-retire-a-resource-the-candidate-borrows
          (list (car (admit session installed bad (view:tree bad) 'retire)) (and (model:snapshot old) #t)) '(invalid #t))
        (model:commit! who (list (list resource (model:revision resource) (list (get installed 'id)) (list (get installed 'id)))))
        (check 'composition-ownership-cannot-consume-its-binding
          (list (car (admit session installed #f '() 'retire)) (model:snapshot (get installed 'id)))
          (list 'invalid installed))
        (model:commit! who (list (list resource (model:revision resource) outputs outputs)))
        (let ([entered (test:gate)] [release (test:gate)] [block? #t])
          (set! resource-check (lambda (v)
                                 (when block? (set! block? #f) (entered #t) (test:await 'disposal-commit release))
                                 (list? v)))
          (let ([worker (test:worker (lambda () (admit session installed candidate (view:tree candidate) 'retire)))])
            (test:await 'disposal-validation entered)
            (set! late (view:create! who borrowed 'label 1 '() '() resource))
            (release #t)
            (check 'composition-scoped-allocation-race-refuses-the-whole-disposal
              (list (car (worker)) (model:snapshot (get installed 'id)) (view:owner (view:snapshot candidate)))
              (list 'stale installed #f)))
          (set! resource-check list?))
        (let* ([events (test:recorder)]
               [token (model:subscribe! #f events)]
               [admitted (admit session installed candidate (view:tree candidate) 'retire)]
               [pending (cadr admitted)] [retired (list old resource child scoped late)])
          (model:unsubscribe! token)
          (check 'composition-disposal-closes-models-and-persists-one-output-intent-atomically
            (list (car admitted) (map model:snapshot retired) (length (events))
              (view:children (view:snapshot foreign)) (view:focus (view:snapshot foreign))
              (list-sort (lambda (a b) (< (cadr a) (cadr b))) (get (get pending 'value) 'cleanup))
              (map store:exists? (cons borrowed outputs))
              (car (admit session pending #f '() 'retire))
              (test:raises? (lambda () (model:create! who 'composition-output 1 resource 'persistent '() '()))))
            (list 'applied (make-list 5 #f) 1 '() #f outputs '(#t #t #t) 'pending #t))
          (check 'composition-obsolete-cleanup-refuses-before-deleting-output
            (list (car (finish session installed)) (map store:exists? outputs)) '(stale (#t #t)))
          ;; Interrupt after deleting one output, and reconnect before the
          ;; old attachment's finalizer runs. Its cleanup cannot act on the
          ;; replacement attachment; startup recovery can finish the intent.
          (store:delete! who (car outputs))
          (policy:revoke! session)
          (let* ([next (policy:mint! who (policy:make 'all 1000 'any 8000))]
                 [current (car (acquire next "disposal"))])
            (root-binding:resume! session)
            (check 'composition-departure-cleanup-is-attachment-specific
              (store:exists? (cadr outputs)) #t)
            (root-binding:resume! #f)
            (let* ([cleaned (model:snapshot (get current 'id))] [again (finish next cleaned)])
              (check 'composition-resumes-partial-disposal-and-keeps-borrowed-state
                (list (map store:exists? outputs) (store:line borrowed 0) (get (get cleaned 'value) 'root)
                  (get (get cleaned 'value) 'cleanup) (car again) (equal? cleaned (cadr again))
                  (view:owner (view:snapshot candidate)))
                (list '(#f #f) "keep" candidate '() 'applied #t who)))
            (policy:revoke! next))))))
)
