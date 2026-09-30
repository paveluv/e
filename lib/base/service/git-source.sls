;; Demand-owned Git queries. Processes never run on the UI or store writer.
(import (only (foundation edoc) elibrary))
(elibrary (service git-source)
  (export create! create-patch! expand! refresh! select-patch!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core row) row:)
          (prefix (core work-queue) work-queue:) (prefix (service file) file:)
          (prefix (service git) git:) (prefix (state collection) collection:)
          (prefix (state model) model:) (prefix (state store) store:))
  (define (get r k) (cdr (assq k r)))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define kind
    (model:register-kind! 'git-source 1
      (lambda (v)
        (and (list? v) (for-all pair? v) (equal? (map car v) '(path commit file document refresh))
          (string? (get v 'path)) (or (not (get v 'commit)) (string? (get v 'commit)))
          (natural? (get v 'refresh))
          (if (get v 'document)
            (and (eq? (not (get v 'commit)) (not (get v 'file))) (or (not (get v 'file)) (string? (get v 'file)))
              (natural? (get v 'document)) (> (get v 'document) 0))
            (not (get v 'file)))))))
  ;; Dedicated worker: a slow Git process must not stall Finder or Search.
  (define worker (work-queue:create))
  (define producer '(app git))
  (define selection-lock (make-mutex))
  (define patch-facts '((read-only . #t) (disposable . #t) (internal . #t) (mode . "git:diff") (mode-auto . #f)))
  (define columns '((commit "Commit" string) (date "Date" integer) (author "Author" string) (subject "Subject / file" string)
                    (path "Path" string) (status "Change" symbol)))
  (define (query! actor path commit file document)
    (let* ([refs (if document (list (list 'buffer document)) '())]
           [source (model:create! actor 'git-source 1 'session 'persistent refs
                     (map cons '(path commit file document refresh) (list path commit file document 0)))])
      (collection:create! actor source "" '() 'persistent (cons source refs))))

  (edoc "Create a lazy Git history collection for a path. The newest twenty commits are listed; expanding a commit fetches only its changed files. Repository discovery and processes run in base work."
        (actor actor "creator") (path file "path inside a repository") (returns row-source))
  (define (create! actor path) (query! actor (file:expand path) #f #f #f))

  (edoc "Create an independent unselected patch query and read-only document; return (query document). Selecting a file changes this request, never another browser's preview."
        (actor actor "creator") (returns list))
  (define (create-patch! actor)
    (let ([document (store:publish! producer (gensym->unique-string (gensym "git-patch")) "<git-patch>"
                      '("No patch selected") (cons '(git-request . #f) patch-facts) #f)])
      (guard (ex [else (store:delete! producer document) (raise ex)])
        (list (query! actor "" #f #f document) document))))
  (define (source query)
    (let* ([r (collection:summary query)] [s (and r (model:snapshot (get (get r 'value) 'source)))])
      (unless (and s (eq? (get s 'kind) 'git-source)) (error 'git-source "expected a Git query" query)) s))
  (define (stamp v) (map (lambda (k) (get v k)) '(path commit file refresh)))
  (define (change! actor s next)
    ;; Invalidate old output before admitting the new recipe. A worker only
    ;; publishes against this exact request stamp and document revision.
    (when (get next 'document)
      (let-values ([(lines revision facts) (store:snapshot-state (get next 'document))])
        (let ([publication (get facts 'publication)])
          (unless (store:publish! producer (cadr publication) "<git-patch>"
                    (list (if (get next 'commit) "[Loading patch]" "No patch selected"))
                    (cons (cons 'git-request (stamp next)) patch-facts)
                    (list (get next 'document) revision (cons 'publication publication)))
            (error 'change! "patch document changed")))))
    (let-values ([(status rows) (model:commit! actor (list (list (get s 'id) (get s 'revision) (get s 'references) next)))])
      (unless (eq? status 'applied) (error 'change! "Git request changed; try again"))))
  (define (selected selection basis)
    (unless (row:selection? selection) (error 'git-source "expected a shown row selection"))
    (let* ([s (source (car selection))] [meta (get (collection:summary (car selection)) 'value)]
           [reply (collection:lookup (car selection) (cadr selection) (caddr selection) '(commit path))])
      (unless (and (not (get (get s 'value) 'document)) (eq? (car reply) 'ready) (equal? basis (caddr reply))
                (= (get meta 'generation) (cadr selection)) (equal? (get meta 'basis) basis)
                (pair? (list-ref reply 4))) (error 'git-source "Git selection changed; choose it again"))
      (list s (get (get meta 'details) 'repository))))

  (edoc "Expand or collapse one exact displayed commit. A stale history generation refuses; other commits stay collapsed."
        (actor actor "caller") (selection row-selection "shown query, generation and commit key") (basis datum "shown result basis"))
  (define (expand! actor selection basis)
    (let* ([s (car (selected selection basis))] [key (caddr selection)] [v (get s 'value)])
      (unless (and (list? key) (= (length key) 2) (eq? (car key) 'commit)) (error 'expand! "choose a commit"))
      (let ([next (map (lambda (p) (if (eq? (car p) 'commit) (cons 'commit (and (not (equal? (cdr p) (cadr key))) (cadr key))) p)) v)])
        (let-values ([(status rows) (model:commit! actor (list (list (get s 'id) (get s 'revision) (get s 'references) next)))])
          (unless (eq? status 'applied) (error 'expand! "Git selection changed; choose it again"))))))

  (edoc "Select an exact displayed file into an explicit patch query. Capture repository, commit and path; stale history refuses. Late work is fenced by the patch document's request stamp and revision."
        (actor actor "caller") (query row-source "patch query") (selection row-selection "shown file") (basis datum "shown result basis"))
  (define (select-patch! actor query selection basis)
    (with-mutex selection-lock
      (let* ([chosen (selected selection basis)] [key (caddr selection)] [s (source query)] [v (get s 'value)])
        (unless (and (get v 'document) (list? key) (= (length key) 3) (eq? (car key) 'file))
          (error 'select-patch! "expected a patch query and file selection"))
        (change! actor s (map cons '(path commit file document refresh)
                           (list (cadr chosen) (cadr key) (caddr key) (get v 'document) (+ 1 (get v 'refresh))))))))

  (edoc "Refresh an explicit Git query. A new recipe revision cancels older work; hidden queries wait for demand."
        (actor actor "caller") (query row-source "Git query"))
  (define (refresh! actor query)
    (with-mutex selection-lock
      (let* ([s (source query)] [v (get s 'value)])
        (change! actor s (map (lambda (p) (if (eq? (car p) 'refresh) (cons 'refresh (+ 1 (cdr p))) p)) v)))))

  (define (result rows details)
    (let* ([rows (list->vector rows)] [n (vector-length rows)] [index (make-hashtable equal-hash equal?)])
      (do ([i 0 (+ i 1)]) ((= i n)) (hashtable-set! index (car (vector-ref rows i)) i))
      (collection:make-result columns n (lambda (i) (vector-ref rows i))
        (lambda (key) (hashtable-ref index key #f))
        (lambda (at direction offset) (and (> n 0) (max 0 (min (- n 1) (+ at (if (eq? direction 'forward) offset (- offset)))))))
        (list '(sortable) (cons 'details details)))))
  (define (history repo expanded check!)
    (apply append
      (map (lambda (commit)
             (check!)
             (let* ([hash (git:commit-hash commit)] [key (list 'commit hash)]
                    [row (list key (map cons '(commit date author subject)
                                     (list hash (git:commit-time commit) (git:commit-author-name commit) (git:commit-subject commit))) '())])
               (cons row (if (not (equal? expanded hash)) '()
                           (map (lambda (change)
                                  (check!)
                                  (let* ([path (git:diff-path change)] [old (git:diff-original-path change)])
                                    (list (list 'file hash path)
                                      (list (cons 'commit hash) (cons 'path path) (cons 'status (git:diff-status change))
                                        (cons 'subject (if old (format "~a → ~a" old path) path)))
                                      '((depth . 1))))) (git:commit-files repo hash)))))) (git:log repo 20))))
  (define (publish-patch! v lines revision facts)
    (let ([publication (get facts 'publication)] [request (assq 'git-request facts)])
      (and request (equal? (cdr request) (stamp v))
        (store:publish! producer (cadr publication) "<git-patch>" lines
          (cons request patch-facts) (list (get v 'document) revision (cons 'publication publication) request)))))
  (define (start! source query cancelled? publish)
    (unless (and (string=? (get query 'filter) "") (null? (get query 'sort))) (error 'start! "Git history retains commit order"))
    (let* ([id (get query 'id)] [v (get source 'value)] [document (get v 'document)] [publication-basis #f])
      (define (alive?) (and (not (cancelled?)) (model:demanded? id)))
      (work-queue:submit! worker id alive?
        (lambda (check!)
          (when document
            (let-values ([(old revision facts) (store:snapshot-state document)])
              (set! publication-basis (list revision facts))))
          (if (and document (not (get v 'commit))) (publish (result '() (list (cons 'document document))) #f)
            (let ([repo (git:open (get v 'path))])
              (check!)
              (if document
                (let ([revision (car publication-basis)] [facts (cadr publication-basis)])
                  (let ([patch (git:file-patch repo (get v 'commit) (get v 'file))])
                    (unless (alive?) (error 'start! "patch demand ended"))
                    (unless (publish-patch! v (cons (format "~a  ~a" (get v 'commit) (get v 'file))
                                                (map git:patch-line-text (git:patch-lines patch))) revision facts)
                      (error 'start! "patch document changed"))
                    (publish (result '() (list (cons 'document document) (cons 'repository (git:repository-path repo)))) #f)))
                (let ([rows (history repo (get v 'commit) check!)])
                  (check!) (publish (result rows (list (cons 'repository (git:repository-path repo)))) #f))))))
        (lambda (ex)
          (when publication-basis
            (publish-patch! v (list (format "[Patch unavailable: ~a]" (kernel:condition-text ex)))
              (car publication-basis) (cadr publication-basis)))
          (publish #f (kernel:condition-text ex))))))
  (define provider (collection:register! 'git-source 1 start!)))
