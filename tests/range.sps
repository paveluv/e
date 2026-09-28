;; The actual head cache runs against an indexed synthetic base provider.
(let ()
  (define actor head:ui-actor)
  (define reads 0)
  (define hold? #f)
  (define entered (test:gate))
  (define release (test:gate))
  (define wide (make-string 65500 #\w))
  (define (field r k) (cdr (assq k r)))
  (define (ready id)
    (test:await 'range-summary (lambda () (eq? 'ready (field (field (collection:summary id) 'value) 'status))))
    (collection:summary id))
  (define (generation s) (field (field s 'value) 'generation))
  (define (page id g start count)
    (test:await 'range-page (lambda () (range:pump!) (eq? (car (range:read id g start count '(name))) 'ready)))
    (range:read id g start count '(name)))
  (model:register-kind! 'range-fixture 1 integer?)
  (collection:register! 'range-fixture 1
    (lambda (r cancel!)
      (let ([count (field r 'value)] [revision (field r 'revision)])
        (list '((name "Name" string)) count
          (lambda (i)
            (set! reads (+ 1 reads))
            (when hold? (set! hold? #f) (entered #t) (test:await 'release-range release))
            (list i (list (cons 'name (if (< count 1000) wide (format "~a:~a" revision i)))) '()))
          (lambda (key) (and (integer? key) (<= 0 key) (< key count) key))))))
  (let* ([source (model:create! actor 'range-fixture 1 'session 'transient '() 10000000)]
         [query (collection:create! actor source "" '() 'transient)] [s (ready query)] [g (generation s)]
         [a (range:acquire! query void)] [b (range:acquire! query void)])
    (range:request! a g 0 32 '(name) '())
    (range:request! b g 16 32 '(name) '())
    (page query g 0 48)
    (let ([before reads])
      (range:read query g 3 20 '(name)) (range:read query g 18 4 '(name)) (range:pump!)
      (test:check 'range-overlapping-views-share-a-page-and-warm-reads-do-no-work
        (list before reads (list-ref (range:locate query g 30) 3)) '(64 64 30)))
    (let* ([table (table:create! actor query '(name))] [before reads])
      (widget:mount! table 'ten-million-rows)
      (for-each (lambda (width) (widget:prepare! table width 10) (widget:pump!) (range:pump!)) '(80 10 300))
      (test:check 'table-ten-million-rows-reuse-pages-and-constant-view-count
        (list reads (length (view:tree table))) (list before 4))
      (widget:unmount! table))
    (set! hold? #t)
    (range:request! a g 64 32 '(name) '())
    (range:pump!) (test:await 'range-held entered)
    (let ([r (model:snapshot source)])
      (model:commit! actor (list (list source (field r 'revision) '() 10000001))))
    (let* ([s (ready query)] [next (generation s)])
      (range:request! a next 64 32 '(name) '())
      (range:request! b next 64 16 '(name) '())
      (release #t)
      (let ([p (page query next 64 32)])
        (test:check 'range-late-old-generation-cannot-replace-current-pages
          (list (cadr p) (caddr (car (caddr (car (list-ref p 4)))))
            (range:read query g 64 1 '(name)))
          (list next "1:64" '(pending)))))
    (range:release! a) (range:release! b)
    (test:check 'range-last-release-ends-local-read-ownership
      (test:raises? (lambda () (range:summary query))) #t))
  (let* ([source (model:create! actor 'range-fixture 1 'session 'transient '() 9)]
         [query (collection:create! actor source "" '() 'transient)] [s (ready query)] [g (generation s)]
         [token (range:acquire! query void)])
    (range:request! token g 0 9 '(name) '())
    (test:check 'range-short-byte-limited-page-fetches-its-remainder
      (length (list-ref (page query g 0 9) 4)) 9)
    (range:release! token))
  (let* ([source (model:create! actor 'range-fixture 1 'session 'transient '() 160)]
         [query (collection:create! actor source "" '() 'transient)] [s (ready query)] [g (generation s)]
         [token (range:acquire! query void)])
    (range:request! token g 0 160 '(name) '())
    (test:await 'range-bounded-overload
      (lambda () (range:pump!) (eq? (car (range:read query g 0 160 '(name))) 'unavailable)))
    (let ([before reads])
      (range:pump!) (range:pump!)
      (test:check 'range-demand-over-budget-is-explicit-without-refetching
        (list (range:read query g 0 160 '(name)) reads) (list '(unavailable cache-budget) before)))
    (range:request! token g 0 1 '(name) '())
    (page query g 0 1)
    (range:release! token)))
