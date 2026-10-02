;; Base provider contracts reuse Finder's filesystem fixture and process.
(let ()
  (define actor '(head "filesystem"))
  (define demands '())
  (define (field r k) (cdr (assq k r)))
  (define (ready query)
    (unless (assoc query demands)
      (set! demands (cons (cons query (model:subscribe! (list query) void)) demands)))
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
  (define (complete! query)
    (let ([intent (filesystem:complete! actor query (field (ready query) 'generation))] [result #f])
      (test:await 'filesystem-completion
        (lambda ()
          (set! result (field (field (ready query) 'details) 'completion))
          (and (pair? result) (eq? (car result) 'ready) (equal? (cadr result) intent))))
      (caddr result)))
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
         [_ (change! q2 (list (cons 'filter (path "small/nested/needle-o"))))])
    (test:check 'filesystem-old-generation-refuses-rows-and-completion
      (list (collection:range q2 old 0 1 '(name)) (filesystem:complete! actor q2 old)) '((stale) #f))
    (test:check 'filesystem-completion-keeps-filter-and-publishes-basis-bearing-proposal
      (list (complete! q2) (field (ready q2) 'filter))
      (map path '("small/nested/needle-only.txt" "small/nested/needle-o"))))
  ;; A common suffix can begin with an unrelated directory's name. Tab
  ;; must preserve the connecting rows too, not admit that empty sibling.
  (let ([directories (map path '("small/empty" "large/empty"))])
    (for-each (lambda (dir) (mkdir dir) (file:write! (string-append dir "/vt.sls") '#("") #f)) directories)
    (filesystem:refresh! actor)
    (change! q2 (list (cons 'filter (string-append root "/ vt.sls"))))
    (let* ([before (map cadr (rows q2 '(name)))] [completed (complete! q2)]
           [v (change! q2 (list (cons 'filter completed)))])
      (test:check 'filesystem-completion-preserves-matches-and-connecting-rows
        (list completed (field (field v 'details) 'matches) (equal? before (map cadr (rows q2 '(name)))))
        (list (string-append root "/ empty/vt.sls") 2 #t)))
    (for-each (lambda (dir) (delete-file (string-append dir "/vt.sls")) (delete-directory dir)) directories)
    (filesystem:refresh! actor))
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
  ;; Canonical visiting invalidates inventory even outside Finder. Creation
  ;; logs each real ancestor once and mode/placement remain head concerns.
  (change! q2 (list (cons 'filter (path "acquired/child.txt"))))
  (system (format "ln -s ~s ~s" root (path "alias")))
  (let* ([destination #f] [target (path "acquired/child.txt")] [nested #f]
         [alias (collection:create! actor source (path "alias/acquired/child.txt") '() 'transient)]
         [token (model:subscribe! (list source)
                  (lambda (notice)
                    (unless nested
                      (set! nested #t)
                      (set! nested (cadr (document:acquire! actor (path "nested-acquire")))))))])
    (ready alias)
    (edit:visit-file! (path "alias/acquired/child.txt") (lambda (kind value) (set! destination value)))
    (model:unsubscribe! token)
    (test:check 'ordinary-visit-invalidates-proposals-and-logs-creation-in-order
      (list (file-exists? target) (equal? nested (store:find-file (path "nested-acquire"))) (store:property destination 'base)
        (map (lambda (q) (map (lambda (r) (car (cadr r))) (rows q '(name)))) (list q2 alias))
        (reverse (map log:datum (filter (lambda (r) (string:search (log:datum r) (path "acquired") 0 (string-length (log:datum r))))
                                  (log:entries 'file:create!)))))
      (list #t #t "" '((path) (path))
        (list (string-append "Created directory " (path "acquired/")) (string-append "Created file " target))))
    (store:delete! actor destination)
    (store:delete! actor nested) (delete-file (path "nested-acquire"))
    (delete-file target) (delete-directory (path "acquired"))
    (retire! alias) (delete-file (path "alias")))

  ;; Reopen is one undoable merge; earlier edits survive. A stale review is
  ;; refused for both merge and reread without changing newer text or facts.
  (let ([target (path "acquire-merge")])
    (file:write! target '#("one" "two") #t)
    (let ([id (cadr (document:acquire! actor target))])
      (store:edit! actor id 0 (text:make-span 0 0 0 0) '("mine "))
      (file:write! target '#("one" "disk") #t)
      (document:acquire! actor target)
      (let ([merged (store:line id 1)])
        (store:undo! actor id)
        (let ([before (call-with-values (lambda () (store:snapshot id)) (lambda (text revision) text))])
          (store:undo! actor id)
          (test:check 'acquisition-reload-preserves-earlier-undo
            (list merged before (store:line id 0)) '("disk" #("mine one" "two") "one"))))
      (let-values ([(text revision facts) (store:snapshot-state id)])
        (store:edit! actor id revision (text:make-span 0 0 0 0) '("later "))
        (test:check 'stale-file-reviews-cannot-replace-concurrent-work
          (list
            (map (lambda (operation) (call-with-values (lambda () (operation actor id '("lost") '((base . "lost\n")) 'any (cons revision facts))) list))
              (list store:reload! store:reread!))
            (store:line id 0) (store:property id 'base))
          '(((refused stale-review) (refused stale-review)) "later one" "one\ndisk\n")))
      (store:delete! actor id))
    (delete-file target))

  ;; Replacing a shown parent, including with a symlink, invalidates its
  ;; creation witness. Partial creation is kept and accurately reported.
  (for-each
    (lambda (link?)
      (let* ([parent (path "proposal-parent")] [away (path "proposal-away")]
             [target (string-append parent "/child")])
        (mkdir parent)
        (let ([witness (list 'file parent (sys:file-identity parent))])
          (rename-file parent away)
          (if link? (system (format "ln -s ~s ~s" away parent)) (mkdir parent))
          (test:check (list 'creation-refuses-replaced-parent link?)
            (list (test:raises? (lambda () (document:acquire! actor target witness)))
              (file-exists? target)) '(#t #f))
          (if link? (delete-file parent) (delete-directory parent))
          (delete-directory away)))) '(#f #t))
  (let ([parent (path "partial")] [block (path "partial/blocked")])
    (parameterize ([kernel:registering-module 'acquisition-fixture])
      (file:add-create-hook! (lambda (created) (when (string=? created parent)
                                                 (call-with-output-file block (lambda (out) (display "keep" out)))))))
    (dynamic-wind void
      (lambda ()
        (test:check 'partial-directory-creation-is-retained-and-logged
          (list (test:raises? (lambda () (document:acquire! actor (string-append block "/file"))))
            (file-directory? parent) (call-with-input-file block get-string-all)
            (log:datum (car (log:entries 'file:create!))))
          (list #t #t "keep" (string-append "Created directory " parent "/"))))
      (lambda () (kernel:retract-module! 'acquisition-fixture) (delete-file block) (delete-directory parent))))
  (let ([g (field (ready q) 'generation)])
    (retire! source)
    (test:check 'filesystem-source-retirement-fences-prepared-rows-and-completion
      (list (collection:range q g 0 1 '(name)) (filesystem:complete! actor q g)) '((stale) #f)))
  (for-each retire! (list q q2))
  (let* ([source (filesystem:create-source! actor root #f 'persistent)]
         [owned (filesystem:create-query! actor source (path "apple"))]
         [query (car owned)] [buffer (cadr owned)] [v (ready query)])
    (filesystem:complete! actor query (field v 'generation))
    (let-values ([(text revision) (store:snapshot buffer)])
      (store:edit! actor buffer revision (text:make-span 0 0 0 (string-length (vector-ref text 0))) (list (path "zeta"))))
    (let ([v (ready query)])
      (test:check 'filesystem-connected-filter-supersedes-completion-and-old-query
        (list (field (field v 'details) 'completion)
          (map (lambda (r) (caddr (assq 'name (caddr r)))) (rows query '(name)))) '(() ("zeta" "zeta.txt"))))
    (retire! query)
    (test:await 'filesystem-owned-filter-released (lambda () (not (store:exists? buffer))))
    (test:check 'filesystem-query-disposal-releases-owned-source (model:snapshot source) #f))
  (for-each (lambda (p) (model:unsubscribe! (cdr p))) demands)
  ;; Cleanup above uses external deletion; later consumers must not inherit
  ;; a cached inventory from the partial-creation fixture.
  (filesystem:refresh! actor))
