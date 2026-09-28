;; Compact index and cancellation contracts; no new runner or wall-clock sleep.
(let ()
  (define actor '(head "collections"))
  (define (field r k) (cdr (assq k r)))
  (define (ready id)
    (test:await 'collection-ready
      (lambda () (let ([s (collection:summary id)])
                   (not (eq? (field (field s 'value) 'status) 'pending)))))
    (collection:summary id))
  (define (generation s) (field (field s 'value) 'generation))
  (define (keys packet) (map cadr (list-ref packet 4)))
  (let* ([source (collection:create-source! actor '((name "Name" string) (size "Size" integer) (flag "Flag" boolean))
                   '#((one ((name . "SAME") (size . 2) (flag . #f)) ())
                      (two ((name . "same") (size . 1) (flag . #t)) ())
                      (three ((name . "third") (size . 1)) ())) 'persistent)]
         [query (collection:create! actor source "" '((size ascending) (name ascending)) 'persistent)]
         [s (ready query)] [g (generation s)])
    (test:check 'collection-raw-compound-sort-stable-keys-and-bounded-rank
      (list (keys (collection:range query g 0 256 '(name size flag)))
        (list-ref (collection:rank query g 'one) 3)
        (list-ref (collection:rank query g 'absent) 3)
        (member source (map car (caddr (connection:snapshot (list query))))))
      '((two three one) 2 #f #f))
    (collection:configure! actor query (field s 'revision) '((sort (size ascending) (name ascending))))
    (test:check 'collection-identical-recipe-keeps-ready-generation (collection:summary query) s)
    (let-values ([(status ignored) (collection:configure! actor query (field s 'revision) '((sort (flag ascending))))])
      (let* ([s (ready query)] [g2 (generation s)] [p (collection:range query g2 0 10 '(flag))])
        (test:check 'collection-missing-cell-differs-from-false-and-old-result-is-stale
          (list status (keys p) (caddr (car (list-ref p 4)))
            (caddr (cadr (list-ref p 4))) (collection:range query g 0 1 '(name)))
          '(applied (three one two) ((flag absent)) ((flag ready #f)) (stale)))
        (collection:configure! actor query (field s 'revision) '((filter . "SaMe")))))
    (let ([s (ready query)])
      (test:check 'collection-case-insensitive-filter-and-page-boundaries
        (list (field (field s 'value) 'count) (keys (collection:range query (generation s) 1 20 '(name)))
          (keys (collection:range query (generation s) 50 20 '(name)))) '(2 (two) ())))
    (let* ([nested (collection:create! actor query "" '() 'transient)] [s (ready nested)])
      (test:check 'collection-can-index-a-prepared-query-without-a-row-copy
        (list (keys (collection:range nested (generation s) 0 10 '(name)))
          (list-ref (collection:rank nested (generation s) 'two) 3)) '((one two) 1)))
    (let* ([rows '#((one ((name . "renamed") (size . 0)) ()))]
           [r (model:snapshot source)])
      (model:commit! actor (list (list source (field r 'revision) '() (list (car (field r 'value)) rows))))
      (let ([s (ready query)])
        (test:check 'collection-source-revision-rebuilds-current-filter
          (list (field (field s 'value) 'count) (list-ref (collection:rank query (generation s) 'one) 3)) '(0 #f)))))
  (let* ([source (collection:create-source! actor '((name "Name" string))
                   (vector (list 'big (list (cons 'name (make-string 70000 #\x))) '())) 'transient)]
         [query (collection:create! actor source "" '() 'transient)] [s (ready query)])
    (test:check 'collection-oversized-scalar-is-an-explicit-cell-diagnostic
      (caddr (car (list-ref (collection:range query (generation s) 0 1 '(name)) 4)))
      '((name unavailable oversized-cell))))
  (let ([reads 0] [captures 0] [entered (test:gate)] [release (test:gate)] [hold? #t])
    (model:register-kind! 'indexed-fixture 1 (lambda (v) (and (integer? v) (> v 0))))
    (collection:register! 'indexed-fixture 1
      (lambda (r checkpoint!)
        (set! captures (+ captures 1))
        (let ([n (field r 'value)])
          (list '((name "Name" string)) n
            (lambda (i)
              (set! reads (+ reads 1))
              (when (and (= n 256) (= i 0) hold?)
                (set! hold? #f) (entered #t) (test:await 'release-query release))
              (list i (list (cons 'name (number->string i))) '()))
            (lambda (key) (and (integer? key) (<= 0 key) (< key n) key))))))
    (let* ([source (model:create! actor 'indexed-fixture 1 'session 'transient '() 10000000)]
           [a (collection:create! actor source "" '() 'transient)] [sa (ready a)]
           [b (collection:create! actor source "" '() 'transient)] [sb (ready b)])
      (test:check 'collection-ten-million-identity-query-does-not-enumerate-or-recopy
        (list reads captures (field (field sa 'value) 'count)
          (list-ref (collection:rank a (generation sa) 9999999) 3)) '(0 1 10000000 9999999))
      (let ([p (collection:range b (generation sb) 9999990 100 '(name))])
        (test:check 'collection-only-requested-tail-rows-are-read (list (length (list-ref p 4)) reads) '(10 10))))
    (let* ([source (model:create! actor 'indexed-fixture 1 'session 'transient '() 256)]
           [query (collection:create! actor source "never" '() 'transient)])
      (test:await 'query-entered entered)
      (let ([s (collection:summary query)])
        (collection:configure! actor query (field s 'revision) '((filter . ""))))
      (release #t)
      (let ([s (ready query)])
        (test:check 'collection-cancelled-scan-cannot-publish-over-replacement
          (list (field (field s 'value) 'filter) (field (field s 'value) 'count)
            (<= reads 138) captures) '("" 256 #t 2))))))
