;; Shared filesystem inventory, cancellable queries and bounded metadata demand.
(import (only (foundation edoc) elibrary))
(elibrary (service filesystem)
  (export complete! configure! create-query! create-source! refresh!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core work-queue) work-queue:)
          (prefix (foundation path-filter) path-filter:) (prefix (foundation string) string:)
          (prefix (service directory) directory:) (prefix (service file) file:)
          (prefix (service file-query) file-query:)
          (prefix (service log) log:)
          (prefix (state collection) collection:) (prefix (state connection) connection:)
          (prefix (state model) model:) (prefix (state store) store:) (prefix (sys sys) sys:))

  (define (field r k) (cdr (assq k r)))
  (define (absolute? s) (and (string? s) (string:prefix? "/" s)))
  (define (recipe? v)
    (and (list? v) (for-all pair? v) (equal? (map car v) '(home hidden epoch))
      (absolute? (field v 'home)) (boolean? (field v 'hidden))
      (integer? (field v 'epoch)) (exact? (field v 'epoch)) (>= (field v 'epoch) 0)))
  (define kind (model:register-kind! 'filesystem-source 1 recipe?))
  (define columns '((name "Name" string) (path "Path" string) (kind "Kind" symbol) (link "Link" boolean)
                    (size "Size" integer) (modified "Modified" integer) (created "Created" integer)
                    (permissions "Permissions" integer) (count "Count" integer) (exact "Exact count" boolean)
                    (proposal "Creation basis" datum)))
  (define sortable '(name size modified created permissions count))
  (define metadata '(size modified created permissions))
  (define lock (make-mutex))
  (define worker (work-queue:create))
  (define epoch 0)
  (define cache #f) ; touched only by the filesystem worker
  (define cache-epoch -1)
  (define invalidated '())
  (define reset-cache? #t)
  (define existence (make-hashtable equal-hash equal?))
  (define jobs (make-hashtable equal-hash equal?))
  (define tracked (make-hashtable equal-hash equal?))
  (define-record-type job
    (fields source query epoch cancelled? publish
      (mutable plan) (mutable index) (mutable positions) (mutable overlay)
      (mutable skipped) (mutable done?) (mutable completion) (mutable intent) wanted (mutable enriching?) (mutable proposal)))
  (define (obsolete? job)
    (or ((job-cancelled? job)) (with-mutex lock (not (= epoch (job-epoch job))))))
  (define (queue-job! job kind procedure)
    (work-queue:submit! worker (list (field (job-query job) 'id) kind)
      (lambda () (not (obsolete? job))) procedure
      (lambda (ex) ((job-publish job) #f (kernel:condition-text ex)))))
  (define (inventory!)
    (let ([current (with-mutex lock epoch)])
      (unless (= cache-epoch current)
        (let-values ([(reset? paths) (with-mutex lock
                                       (let ([reset? reset-cache?] [paths invalidated])
                                         (set! reset-cache? #f) (set! invalidated '()) (values reset? paths)))])
          (if (not cache) (set! cache (directory:make-cache #f))
            (if reset? (directory:clear! cache)
              (for-each (lambda (path) (directory:invalidate! cache path #t)) paths))))
        ;; Recovered sources may retain inventory without ever starting a query.
        (for-each (lambda (id) (with-mutex lock (hashtable-set! tracked id #t))) (model:ids 'filesystem-source))
        (hashtable-clear! existence) (set! cache-epoch current))))
  (define (exists? path directory?)
    (let* ([key (cons path directory?)] [known (hashtable-ref existence key 'unknown)])
      (if (not (eq? known 'unknown)) known
        (let ([answer (guard (ex [else #f]) (if directory? (file-directory? path) (file-exists? path #f)))])
          (hashtable-set! existence key answer) answer))))

  ;; Persistent sparse ordinal tree: one enriched page copies O(page*log N)
  ;; small nodes, sharing the ordering and all other metadata with old readers.
  (define (overlay-ref tree lo hi at)
    (cond [(not tree) #f] [(= (- hi lo) 1) tree]
      [else (let ([mid (div (+ lo hi) 2)])
              (if (< at mid) (overlay-ref (car tree) lo mid at) (overlay-ref (cdr tree) mid hi at)))]))
  (define (overlay-set tree lo hi at value)
    (if (= (- hi lo) 1) value
      (let ([mid (div (+ lo hi) 2)] [left (and tree (car tree))] [right (and tree (cdr tree))])
        (if (< at mid) (cons (overlay-set left lo mid at value) right)
          (cons left (overlay-set right mid hi at value))))))
  (define (needs-metadata? e)
    (and (not (directory:missing? e)) (not (directory:entry-mode e)) (not (eq? (directory:entry-kind e) 'unavailable))))
  (define (key e) (list (if (directory:missing? e) 'proposal 'path) (directory:entry-path e) (directory:entry-kind e)))
  (define (raw entry plan enriched proposal)
    (let* ([e (or enriched entry)] [path (directory:entry-path entry)] [name (file:base-name path)]
           [full (directory:filter-path entry)] [start (- (string-length path) (string-length name))]
           [relative (directory:relative-path entry (file-query:plan-root plan))]
           [spans (filter values
                    (map (lambda (r)
                           (let ([a (max start (car r))] [b (min (string-length path) (cdr r))])
                             (and (< a b) (list 'name (- a start) (- b start)))))
                      (path-filter:ranges (file-query:plan-keys plan) full (directory:directory? entry))))])
      (list (key entry)
        (append (list (cons 'name name) (cons 'path path) (cons 'kind (directory:entry-kind e))
                  (cons 'link (directory:entry-link? e)) (cons 'exact (directory:entry-complete? entry)))
          (if (directory:missing? entry) (list (cons 'proposal (cons (directory:entry-kind entry) proposal))) '())
          (filter values (map (lambda (c) (let ([v (file-query:value (if (eq? c 'count) entry e) c)]) (and v (cons c v))))
                           '(size modified created permissions count))))
        (append (list (cons 'depth (length (filter (lambda (c) (char=? c #\/)) (string->list relative)))))
          (if (directory:missing? entry) (list (cons 'creation (directory:entry-kind entry)) '(roles italic)) '())
          (if (needs-metadata? e) (list (cons 'pending metadata)) '())
          (if (null? spans) '() (list (cons 'matches spans)))))))

  (define (enrich! job check!)
    (let drain ()
      (let* ([wanted (with-mutex lock
                       (let ([v (hashtable-keys (job-wanted job))]) (hashtable-clear! (job-wanted job)) (vector->list v)))]
             [index (job-index job)] [entries (file-query:index-entries index)] [n (vector-length entries)]
             [before (job-overlay job)])
        (for-each (lambda (i)
                    (check!)
                    (when (and (eq? index (job-index job)) (< i n)
                            (needs-metadata? (vector-ref entries i)) (not (overlay-ref (job-overlay job) 0 n i)))
                      (let ([e (directory:read! cache (vector-ref entries i))])
                        (job-overlay-set! job (overlay-set (job-overlay job) 0 n i e))))) wanted)
        (when (and (eq? index (job-index job)) (not (eq? before (job-overlay job)))) (publish! job))
        (when (with-mutex lock
                (if (zero? (hashtable-size (job-wanted job))) (begin (job-enriching?-set! job #f) #f) #t))
          (drain)))))
  (define (demand! job index overlay ordinals columns)
    (when (and (eq? index (job-index job)) (exists (lambda (c) (memq c metadata)) columns))
      (let* ([entries (file-query:index-entries index)] [n (vector-length entries)]
             [wanted (filter (lambda (i) (and (needs-metadata? (vector-ref entries i)) (not (overlay-ref overlay 0 n i)))) ordinals)])
        (unless (null? wanted)
          (when (with-mutex lock
                  (and (eq? index (job-index job))
                    (begin
                      (for-each (lambda (i)
                                  (when (< (hashtable-size (job-wanted job)) 4096) (hashtable-set! (job-wanted job) i #t))) wanted)
                      (and (not (job-enriching? job)) (begin (job-enriching?-set! job #t) #t)))))
            (queue-job! job 'metadata (lambda (check!) (enrich! job check!))))))))
  (define (publish! job)
    (unless (obsolete? job)
      (let* ([index (job-index job)] [entries (file-query:index-entries index)] [n (vector-length entries)]
             [plan (job-plan job)] [positions (job-positions job)] [overlay (job-overlay job)] [proposal (job-proposal job)]
             [choice (file-query:index-choice index)])
        ((job-publish job)
         (collection:make-result
           (map (lambda (c) (if (eq? (car c) 'count)
                              (list 'count (if (null? (file-query:plan-keys plan)) "Entries" "Matches") 'integer) c)) columns) n
           (lambda (i) (raw (vector-ref entries i) plan (overlay-ref overlay 0 n i) proposal))
           (lambda (key) (hashtable-ref positions key #f))
           (lambda (at direction offset) (and (> n 0) (max 0 (min (- n 1) (+ at (if (eq? direction 'forward) offset (- offset)))))))
           (list (cons 'complete (job-done? job)) (cons 'default (if choice (list (key choice)) '())) (cons 'sortable sortable)
             (cons 'details (list (cons 'root (file-query:plan-root plan)) (cons 'missing (file-query:plan-missing plan))
                              (cons 'matches (file-query:index-count index)) (cons 'unreadable (job-skipped job))
                              (cons 'hidden (file-query:plan-hidden? plan)) (cons 'completion (job-completion job)))))
           (lambda (ordinals columns) (demand! job index overlay ordinals columns))) #f))))

  (define (start! source query cancelled? publish)
    (let ([job (make-job source query (with-mutex lock epoch) cancelled? publish #f #f #f #f 0 #f '() 0 (make-eqv-hashtable) #f #f)])
      (with-mutex lock
        (hashtable-set! jobs (field query 'id) job)
        (hashtable-set! tracked (field query 'id) #t) (hashtable-set! tracked (field source 'id) #t))
      (queue-job! job 'scan
        (lambda (check!)
          (inventory!)
          (let* ([v (field source 'value)]
                 [plan (file-query:plan (field query 'filter) (field v 'home) (field v 'hidden)
                         (lambda (path directory?) (check!) (exists? path directory?)))])
            (job-plan-set! job plan)
            (when (file-query:plan-proposed plan)
              (job-proposal-set! job (list (file-query:plan-root plan) (sys:file-identity (file-query:plan-root plan)))))
            (directory:scan! cache (file-query:plan-root plan) (file-query:plan-keys plan) (file-query:plan-hidden? plan)
              (and (exists (lambda (s) (memq (car s) metadata)) (field query 'sort)) #t)
              (lambda () (check!) (obsolete? job))
              (lambda (inventory skipped done?)
                (unless (and (not done?) (null? inventory))
                  (let* ([index (file-query:prepare inventory plan (field query 'filter) (field query 'sort) done? check!)]
                         [entries (file-query:index-entries index)] [positions (make-hashtable equal-hash equal?)])
                    (do ([i 0 (+ i 1)]) ((= i (vector-length entries)))
                      (check!) (hashtable-set! positions (key (vector-ref entries i)) i))
                    (with-mutex lock (job-index-set! job index) (hashtable-clear! (job-wanted job)))
                    (job-positions-set! job positions) (job-overlay-set! job #f)
                    (job-skipped-set! job skipped) (job-done?-set! job done?)
                    (publish! job))))))))))
  (define provider (collection:register! 'filesystem-source 1 start!))

  (edoc "Create a filesystem source with explicit home and hidden-entry policy. Sources share a worker-owned inventory; watches remain disabled."
        (actor actor "creator") (home string "absolute home") (hidden? boolean "include dot entries")
        (persistence (one-of transient persistent) "restart policy") (returns row-source))
  (define (create-source! actor home hidden? persistence)
    (unless (and (absolute? home) (boolean? hidden?)) (error 'create-source! "invalid filesystem context"))
    (let ([id (model:create! actor 'filesystem-source 1 'session persistence '()
                (list (cons 'home home) (cons 'hidden hidden?) (cons 'epoch (with-mutex lock epoch))))])
      (with-mutex lock (hashtable-set! tracked id #t)) id))

  (edoc "Create a filesystem query owning its supplied persistent source and an internal editable filter; views borrow these resources."
        (actor actor "creator") (source row-source "unshared persistent filesystem source") (text string "initial rooted filter")
        (returns list "(query (buffer id))"))
  (define (create-query! actor source text)
    (let ([r (model:snapshot source)])
      (unless (and r (eq? (field r 'kind) 'filesystem-source) (eq? (field r 'persistence) 'persistent))
        (error 'create-query! "expected persistent filesystem source"))
      (let* ([filter (list 'buffer (store:create! actor "Finder filter" (list text) (list '(internal . #t) (cons 'audience (list actor)))))]
             [query (collection:create! actor source text '() 'persistent (list source filter))])
        (connection:bind! actor query (list (list query 'filter #f (list filter 'text)))) (list query filter))))

  (edoc "Change a filesystem source's hidden-entry option against its revision, preserving cached inventory."
        (actor actor "caller") (source row-source "filesystem source") (revision integer "expected model revision")
        (hidden? boolean "include dot entries"))
  (define (configure! actor source revision hidden?)
    (let ([r (model:snapshot source)])
      (unless (and r (eq? (field r 'kind) 'filesystem-source) (boolean? hidden?)) (error 'configure! "invalid source or option"))
      (model:commit! actor (list (list source revision (field r 'references)
                                   (map (lambda (p) (if (eq? (car p) 'hidden) (cons 'hidden hidden?) p)) (field r 'value)))))))

  (edoc "Invalidate the shared filesystem inventory and restart its queries. External filesystem changes become visible only on refresh."
        (actor actor "caller"))
  (define (refresh! actor)
    (invalidate! actor #f))

  (define (invalidate! actor path)
    ;; Model commits deliver callbacks. Hold only the short inventory mutex;
    ;; a subscriber may itself acquire a file or create a filesystem source.
    (with-mutex lock
      (set! epoch (+ epoch 1))
      (cond [(not path) (set! reset-cache? #t) (set! invalidated '())]
        [(and cache (not reset-cache?)) (set! invalidated (cons path invalidated))]))
    (let retry ()
      (let ([records (filter values (map model:snapshot (model:ids 'filesystem-source)))])
        (unless (null? records)
          (let-values ([(status ignored)
                        (model:commit! actor
                          (map (lambda (r) (list (field r 'id) (field r 'revision) (field r 'references)
                                             (map (lambda (p) (if (eq? (car p) 'epoch) (cons 'epoch (+ 1 (cdr p))) p)) (field r 'value)))) records))])
            (when (eq? status 'stale) (retry)))))))

  (define creation-hook
    (file:add-create-hook! (lambda (path) (invalidate! '(base filesystem) path))))

  (edoc "Queue completion for a complete readable generation, returning an intent number or false. The same index publishes details.completion as (pending intent), (ready intent text), or (unavailable intent diagnostic). Match both intent and collection basis, then apply text only to the unchanged requesting filter revision."
        (actor actor "caller") (query row-source "filesystem query") (generation integer "shown generation") (returns (or integer #f)))
  (define (complete! actor query generation)
    (let* ([r (collection:summary query)] [v (and r (field r 'value))] [job (with-mutex lock (hashtable-ref jobs query #f))])
      (and job v (eq? (field v 'status) 'ready) (= generation (field v 'generation))
        (job-done? job) (zero? (job-skipped job)) (not (obsolete? job))
        (let ([intent (with-mutex lock (job-intent-set! job (+ 1 (job-intent job))) (job-intent job))])
          (queue-job! job 'completion
            (lambda (check!)
              (job-completion-set! job (list 'pending intent)) (publish! job)
              (let* ([source (field (job-source job) 'value)]
                     [answer (guard (ex [else (list 'unavailable intent (kernel:condition-text ex))])
                               (list 'ready intent (file-query:complete (job-index job) (job-plan job) (field (job-query job) 'filter)
                                                     (field source 'home)
                                                     (lambda () (check!) (or (obsolete? job) (not (= intent (job-intent job))))))))])
                (when (and (not (obsolete? job)) (= intent (job-intent job)))
                  (job-completion-set! job answer) (publish! job))))) intent))))
  (define (cleanup! ids)
    (when (with-mutex lock (or (not ids) (exists (lambda (id) (hashtable-contains? tracked id)) ids)))
      (work-queue:submit! worker 'cleanup (lambda () #t)
        (lambda (check!)
          (for-each (lambda (j)
                      (let ([id (field (job-query j) 'id)] [source (field (job-source j) 'id)])
                        (unless (and (model:snapshot id) (model:snapshot source) (not (obsolete? j)))
                          (with-mutex lock
                            (when (eq? j (hashtable-ref jobs id #f)) (hashtable-delete! jobs id) (hashtable-delete! tracked id))))))
            (with-mutex lock (vector->list (hashtable-values jobs))))
          (for-each (lambda (id) (unless (model:snapshot id) (with-mutex lock (hashtable-delete! tracked id))))
            (with-mutex lock (vector->list (hashtable-keys tracked))))
          (when (null? (model:ids 'filesystem-source))
            (when cache (directory:close! cache) (set! cache #f))
            (hashtable-clear! existence) (set! cache-epoch -1)))
        (lambda (ex) (log:add! 'filesystem:cleanup! (kernel:condition-text ex))))))
  (define cleanup
    (list (model:subscribe! #f (lambda (notice) (cleanup! (cadr notice))))
      (model:observe-demand! cleanup!))))
