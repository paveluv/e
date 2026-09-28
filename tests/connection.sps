;; Connection invariants share the store fixture and deterministic barriers.
(let ()
  (define author '(head "connection"))
  (define contracts (string-copy "connection-ports"))
  (define (field r k) (cdr (assq k r)))
  (define (status thunk) (car (call-with-values thunk list)))
  (define (bind owner consumer expected producer)
    (status (lambda () (connection:bind! author owner (list (list consumer 'in expected producer))))))
  (define (register!)
    (parameterize ([kernel:registering-module contracts])
      (port:register! '(model connection-fixture 1)
        '((input in string (value fallback)) (output out string (value text))))))
  (model:register-kind! 'connection-fixture 1 (lambda (v) #t))
  (register!)
  (let* ([a (model:create! author 'connection-fixture 1 'session 'persistent '() '((fallback . "a") (text . "A")))]
         [b (model:create! author 'connection-fixture 1 'session 'persistent '() '((fallback . "b") (text . "B")))]
         [result (test:parallel 2 (lambda (n) (if (zero? n) (bind a a #f (list b 'out)) (bind b b #f (list a 'out)))))])
    (test:check 'connection-topology-witness-prevents-cross-owner-phantom-cycle
      (length (filter (lambda (s) (eq? s 'applied)) result)) 1)
    (for-each (lambda (owner) (for-each (lambda (e) (bind owner (car e) (caddr e) #f)) (connection:bindings owner))) (list a b)))
  (let* ([owner (model:create! author 'connection-fixture 1 'session 'persistent '() '())]
         [a (model:create! author 'connection-fixture 1 owner 'persistent '() '((fallback . "a") (text . "A")))]
         [b (model:create! author 'connection-fixture 1 owner 'persistent '() '((fallback . "b") (text . "B")))]
         [c (model:create! author 'connection-fixture 1 owner 'persistent '() '((fallback . "c") (text . "C")))])
    (test:check 'connection-default-type-guards-and-idempotency
      (list (list-head (connection:read b 'in) 2)
        (bind owner b #f (list a 'in))
        (bind owner b #f (list a 'out))
        (bind owner b #f (list a 'out))
        (bind owner b #f (list c 'out))
        (list-head (connection:read b 'in) 2))
      '((ready "b") incompatible applied applied stale (ready "A")))
    (test:check 'connection-cycles-atomic-batches-and-scope
      (list (bind owner a #f (list b 'out))
        (status (lambda () (connection:bind! author owner
                             (list (list c 'in #f (list a 'out)) (list a 'in #f (list b 'out))))))
        (list-head (connection:read c 'in) 2)
        (bind c b (list a 'out) (list c 'out)))
      '(cycle cycle (ready "c") scope))
    (bind owner b (list a 'out) #f)
    (let ([result (test:parallel 2 (lambda (n) (if (zero? n) (bind owner a #f (list b 'out))
                                                   (bind owner b #f (list a 'out)))))])
      (test:check 'connection-concurrent-cycle-has-one-winner
        (length (filter (lambda (s) (eq? s 'applied)) result)) 1))
    (for-each (lambda (e) (bind owner (car e) (caddr e) #f)) (connection:bindings owner))
    (bind owner b #f (list a 'out))
    (kernel:retract-module! contracts)
    (test:check 'connection-unknown-contract-keeps-edge-unavailable
      (list (car (connection:read b 'in)) (length (connection:bindings owner))) '(unavailable 1))
    (register!)
    (let ([r (model:snapshot a)]) (model:retire! author a (field r 'revision)))
    (test:check 'connection-retired-producer-removes-owned-edge
      (list (connection:bindings owner) (list-head (connection:read b 'in) 2)) '(() (ready "b")))
    (bind owner b (list a 'out) #f)
    (test:check 'connection-disconnect-restores-default (list-head (connection:read b 'in) 2) '(ready "b")))
  (port:register! '(view connected-fixture 1)
    '((input in string (options fallback)) (output out string (state text))))
  (let* ([root (view:create! author #f 'row 1 '() '())]
         [a (view:create! author #f 'connected-fixture 1 '((fallback . "a")) '((text . "A")))]
         [b (view:create! author #f 'connected-fixture 1 '((fallback . "b")) '((text . "B")))])
    (view:arrange! author (list (list root 0 (list (list 'a a 'fit) (list 'b b 'fit)) '())) '())
    (bind root b #f (list a 'out))
    (let* ([copy (view:fork! author root)] [children (view:children (view:snapshot copy))]
           [a2 (cadar children)] [b2 (cadadr children)])
      (test:check 'connection-fork-remaps-owned-edges-atomically
        (list (connection:bindings copy) (list-head (connection:read b2 'in) 2))
        (list (list (list b2 'in (list a2 'out))) '(ready "A"))))
    (view:claim! '(head "foreign") root)
    (test:check 'connection-foreign-owned-view-refuses-rewire
      (bind root b (list a 'out) #f) 'owned)
    (view:release-owner! '(head "foreign"))))
