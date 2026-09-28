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
  (let ([reads 0] [started (test:gate)])
    (define (register-provider!)
      (kernel:call-with-registration-update
        (lambda ()
          (kernel:retract-module! 'indexed-fixture-provider)
          (parameterize ([kernel:registering-module 'indexed-fixture-provider])
            (collection:register! 'indexed-fixture 1
              (lambda (r query cancelled? publish!) (started (list (field query 'id) cancelled? publish!))))))))
    (define (request query)
      (test:await 'provider-dispatched (lambda () (and (started) (equal? (car (started)) query))))
      (let ([job (cdr (started))]) (started #f) job))
    (define (index n complete?)
      ;; Even ordinals are section rows: navigation stays O(1) at ten million.
      (collection:make-result '((name "Name" string)) n
        (lambda (i) (set! reads (+ reads 1))
          (list i (list (cons 'name (number->string i))) (list (cons 'selectable (odd? i)) '(depth . 1))))
        (lambda (key) (and (integer? key) (<= 0 key) (< key n) key))
        (lambda (at direction offset)
          (and (> n 1)
            (let* ([forward? (eq? direction 'forward)]
                   [first (if (odd? at) at (+ at (if forward? 1 -1)))])
              (max 1 (min (- n (if (even? n) 1 2)) (+ first (* 2 (if forward? offset (- offset)))))))))
        (list (cons 'complete complete?) '(default 1) (list 'details (cons 'matches (div n 2))))))
    (model:register-kind! 'indexed-fixture 1 (lambda (v) (and (integer? v) (> v 0))))
    (register-provider!)
    (let* ([source (model:create! actor 'indexed-fixture 1 'session 'transient '() 10000000)]
           [query (collection:create! actor source "A" '() 'transient)] [job (request query)])
      ((cadr job) (index 100 #f) #f)
      (let* ([partial (ready query)] [g (generation partial)])
        ((cadr job) (index 10000000 #t) #f)
        (let* ([full (ready query)] [next (generation full)])
          (test:check 'collection-partial-index-and-ten-million-section-navigation
            (list (field (field partial 'value) 'complete) (field (field full 'value) 'complete)
              (collection:range query g 0 1 '(name))
              (list-ref (collection:seek query next 0 'forward 0) 3)
              (list-ref (collection:seek query next 0 'forward 4999999) 3) reads)
            '(#f #t (stale) 1 9999999 0))
          (let ([p (collection:range query next 9999990 100 '(name))])
            (test:check 'collection-only-requested-tail-rows-are-read (list (length (list-ref p 4)) reads) '(10 10)))))
      (collection:configure! actor query (field (collection:summary query) 'revision) '((filter . "B")))
      (let ([middle (request query)])
        (collection:configure! actor query (field (collection:summary query) 'revision) '((filter . "A")))
        (let ([latest (request query)])
          (test:check 'collection-cancellation-fences-input-round-trips-and-late-publications
            (list ((car job)) ((car middle)) ((car latest))
              ((cadr job) (index 6 #t) #f) ((cadr middle) (index 8 #t) #f)
              ((cadr latest) (index 10 #t) #f)
              (field (field (ready query) 'value) 'count)) '(#t #t #f #f #f #t 10))
          (let ([snapshot (index 1 #t)])
            ((cadr latest) snapshot #f)
            (let ([s (ready query)])
              ((cadr latest) snapshot #f)
              (test:check 'collection-sections-only-identical-publication-and-unsupported-sort
                (list (list-ref (collection:seek query (generation s) 0 'forward 0) 3)
                  (equal? s (ready query))
                  (test:raises? (lambda () (collection:configure! actor query (field s 'revision) '((sort (absent ascending)))))))
                '(#f #t #t))))
          (register-provider!)
          (let ([replacement (request query)])
            ((cadr replacement) (index 2 #t) #f)
            (test:check 'collection-redefinition-cancels-old-definition-publishers
              (list ((car latest)) ((cadr latest) (index 4 #t) #f) (field (field (ready query) 'value) 'count)) '(#t #f 2))
            (model:retire! actor source (field (model:snapshot source) 'revision))
            (test:check 'collection-source-retirement-invalidates-result-and-publisher
              (list (field (field (ready query) 'value) 'status) ((cadr replacement) (index 6 #t) #f)) '(unavailable #f))))))))
