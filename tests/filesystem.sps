;; Base provider contracts reuse Finder's filesystem fixture and process.
(let ()
  (define actor '(head "filesystem"))
  (define (field r k) (cdr (assq k r)))
  (define (ready query)
    (test:await 'filesystem-result
      (lambda ()
        (let ([s (collection:summary query)])
          (and s (let ([v (field s 'value)])
                   (or (eq? (field v 'status) 'unavailable)
                     (and (eq? (field v 'status) 'ready) (field v 'complete))))))))
    (let ([v (field (collection:summary query) 'value)])
      (when (eq? (field v 'status) 'unavailable) (error 'filesystem-test (field v 'diagnostic))) v))
  (define (rows query columns)
    (let retry ()
      (let* ([v (ready query)] [packet (collection:range query (field v 'generation) 0 256 columns)])
        (if (eq? (car packet) 'stale) (retry) (list-ref packet 4)))))
  (define (change! query changes)
    (let retry ()
      (let-values ([(status ignored) (collection:configure! actor query (field (collection:summary query) 'revision) changes)])
        (when (eq? status 'stale) (retry)))) (ready query))
  (define (retire! id) (model:retire! actor id (field (model:snapshot id) 'revision)))
  (define source (filesystem:create-source! actor root #f 'persistent))
  (define q (collection:create! actor source (string-append root "/ needle") '() 'transient))
  (define q2 (collection:create! actor source (string-append root "/ apple") '() 'transient))
  (let* ([v (ready q)] [r (rows q '(name path count))])
    (test:check 'filesystem-base-index-hierarchy-and-independent-query-identities
      (list (field (field v 'details) 'matches)
        (map (lambda (r) (caddr (assq 'name (caddr r)))) r)
        (map (lambda (r) (field (cadddr r) 'depth)) r)
        (map (lambda (r) (caddr (assq 'name (caddr r)))) (rows q2 '(name))))
      '(5 ("large" "needle-a.txt" "needle-b.txt" "needle-c.txt" "small" "nested" "needle-only.txt" "needle-one.txt")
        (0 1 1 1 0 1 2 1) ("APPLE.txt" "apple.txt"))))
  (let* ([v (ready q2)] [g (field v 'generation)] [basis (field v 'basis)]
         [ranks (map (lambda (name) (list-ref (collection:rank q2 g (list 'path (path name) 'file)) 3)) '("APPLE.txt" "apple.txt"))]
         [r (rows q2 '(size modified))])
    (test:check 'filesystem-metadata-is-demanded-and-exact-case-ranks-stay-distinct
      (list (map (lambda (r) (map cadr (caddr r))) r)
        ranks)
      '(((pending pending) (pending pending)) (0 1)))
    (test:await 'filesystem-enriched
      (lambda () (> (field (ready q2) 'generation) g)))
    (let ([r (rows q2 '(size))])
      (test:check 'filesystem-enrichment-preserves-basis-order-and-untouched-query
        (list (map (lambda (r) (caddr (assq 'size (caddr r)))) r)
          (equal? basis (field (ready q2) 'basis)) (field (ready q) 'count)) '((8 8) #t 8))))
  (let* ([old (field (ready q2) 'generation)]
         [_ (change! q2 (list (cons 'filter (path "small/nested/needle-o"))))]
         [v (ready q2)])
    (test:check 'filesystem-old-generation-refuses-rows-and-completion
      (list (collection:range q2 old 0 1 '(name)) (filesystem:complete! actor q2 old)) '((stale) #f))
    (let ([intent (filesystem:complete! actor q2 (field v 'generation))])
      (test:await 'filesystem-completion
        (lambda () (equal? (field (field (ready q2) 'details) 'completion) (list 'ready intent (path "small/nested/needle-only.txt"))))))
    (test:check 'filesystem-completion-keeps-filter-and-publishes-basis-bearing-proposal
      (field (ready q2) 'filter) (path "small/nested/needle-o")))
  (change! q2 (list (cons 'filter (path "missing/child.txt"))))
  (let* ([v (ready q2)] [r (rows q2 '(name))])
    (test:check 'filesystem-proposals-have-separate-identities-and-do-not-inflate-matches
      (list (field (field v 'details) 'root) (field (field v 'details) 'matches)
        (map (lambda (r) (car (cadr r))) r) (map (lambda (r) (field (cadddr r) 'creation)) r))
      (list root 0 '(proposal proposal) '(directory file))))
  (change! q2 (list (cons 'filter (string-append root "/ fresh"))))
  (call-with-output-file (path "fresh.txt") (lambda (p) (display "new" p)))
  (let ([other (collection:create! actor source (string-append root "/ fresh") '() 'transient)])
    (test:check 'filesystem-shares-cached-inventory-across-queries (field (ready other) 'count) 0)
    (filesystem:refresh! actor)
    (test:check 'filesystem-refresh-invalidates-all-shared-listings
      (list (field (ready q2) 'count) (field (ready other) 'count)) '(1 1))
    (retire! other))
  (delete-file (path "fresh.txt"))
  (let ([g (field (ready q) 'generation)])
    (retire! source)
    (test:check 'filesystem-source-retirement-fences-prepared-rows-and-completion
      (list (collection:range q g 0 1 '(name)) (filesystem:complete! actor q g)) '((stale) #f)))
  (for-each retire! (list q q2))
  (let* ([source (filesystem:create-source! actor root #f 'persistent)]
         [owned (filesystem:create-query! actor source (path "apple"))]
         [query (car owned)] [buffer (cadadr owned)] [v (ready query)])
    (filesystem:complete! actor query (field v 'generation))
    (let-values ([(text revision) (store:snapshot buffer)])
      (store:edit! actor buffer revision (text:make-span 0 0 0 (string-length (vector-ref text 0))) (list (path "zeta"))))
    (let ([v (ready query)])
      (test:check 'filesystem-connected-filter-supersedes-completion-and-old-query
        (list (field (field v 'details) 'completion)
          (map (lambda (r) (caddr (assq 'name (caddr r)))) (rows query '(name)))) '(() ("zeta" "zeta.txt"))))
    (retire! query)
    (test:await 'filesystem-owned-filter-released (lambda () (not (store:exists? buffer))))
    (test:check 'filesystem-query-disposal-releases-owned-source (model:snapshot source) #f)))
