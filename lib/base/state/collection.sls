;; Provider-owned preparation, compact query recipes and bounded indexed reads.
(import (only (foundation edoc) elibrary))
(elibrary (state collection)
  (export configure! create! create-source! fetch init! lookup make-result range rank register! register-copy! seek summary)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core port) port:)
          (prefix (core row) row:) (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:) (prefix (foundation string) string:)
          (prefix (foundation wire) wire:) (prefix (state connection) connection:)
          (prefix (state model) model:) (prefix (state store) store:) (prefix (state view) view:))
  (define providers (kernel:make-registry car))
  (define copiers (kernel:make-registry car))
  (define results (make-hashtable equal-hash equal?))
  (define desired (make-hashtable equal-hash equal?))
  (define pending (make-hashtable equal-hash equal?))
  (define lock (make-mutex))
  (define ready (make-condition))
  (define worker? #f)
  (define serial 0)
  (define owned (make-hashtable equal-hash equal?))
  (define contract-generation 0)
  (define (field r k) (cdr (assq k r)))
  (define (get r k default) (cond [(assq k r) => cdr] [else default]))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (distinct? xs)
    (or (null? xs) (and (not (member (car xs) (cdr xs))) (distinct? (cdr xs)))))
  (define (set-fields v changes)
    (append (map (lambda (p) (or (assq (car p) changes) p)) v)
      (filter (lambda (p) (not (assq (car p) v))) changes)))
  (define recipe-fields '(source filter sort status generation count columns basis diagnostic))
  (define result-fields '(complete default details sortable))
  (define (configured? v)
    (and (list? v) (for-all pair? v)
      (or (equal? (map car v) recipe-fields) (equal? (map car v) (append recipe-fields result-fields))
        (and (equal? (map car v) (append recipe-fields result-fields '(owned)))
          (list? (get v 'owned '())) (for-all (lambda (ref) (or (row:source? ref)
                                                              (and (list? ref) (= (length ref) 2) (eq? (car ref) 'buffer) (natural? (cadr ref))))) (get v 'owned '()))))
      (row:source? (field v 'source)) (string? (field v 'filter)) (list? (field v 'sort))
      (memq (field v 'status) '(pending ready unavailable))
      (natural? (field v 'generation)) (natural? (field v 'count)) (list? (field v 'columns))))
  (define (recipe? v)
    (and (list? v) (for-all pair? v)
      (<= (length (filter (lambda (p) (eq? (car p) 'input-filter)) v)) 1)
      (string? (get v 'input-filter ""))
      (configured? (filter (lambda (p) (not (eq? (car p) 'input-filter))) v))))
  (define registrations
    (begin (row:init!) (model:register-kind! 'collection 1 recipe?)
      (model:register-kind! 'collection-vector 1
        (lambda (v) (and (list? v) (= (length v) 2) (row:columns? (car v)) (vector? (cadr v))
                      (let loop ([i 0]) (or (= i (vector-length (cadr v)))
                                          (and (row:valid? (car v) (vector-ref (cadr v) i)) (loop (+ i 1))))))))))
  (define-record-type (result %make-result result?)
    (fields columns count at locate seek complete default details sortable demand))
  (define-record-type publication (fields generation basis result))
  ;; A ticket is unique even if input A changes to B and back to A.
  (define-record-type job (fields id key serial mutex (mutable subscription)))
  (define (scalar? type)
    (exists (lambda (parent) (edoc:type-compatible? type parent)) '(number string boolean)))
  (define (bounded? value limit)
    (guard (ex [else #f]) (<= (bytevector-length (wire:encode value)) limit)))

  (edoc "Make an immutable prepared index. Callbacks read only this snapshot: row-at ordinal, locate key, seek ordinal direction offset. Seek includes its origin, skips ineligible rows and clamps at selectable ends. Options are complete, default (zero or one key), details and sortable column names. Optional demand queues background enrichment for a bounded list of ordinals and columns; it must return promptly without I/O."
        (columns list "raw column contracts") (count integer "display rows, including sections")
        (row-at procedure "ordinal -> raw row") (locate procedure "key -> ordinal or false")
        (seek procedure "ordinal forward|backward nonnegative-offset -> selectable ordinal or false")
        (options list "portable summary facts") (demand (list-of procedure) "optional nonblocking metadata demand callback") (returns any))
  (define (make-result columns count row-at locate seek options . demand)
    (unless (and (row:columns? columns) (natural? count) (procedure? row-at) (procedure? locate) (procedure? seek)
              (list? options) (for-all (lambda (p) (and (pair? p) (memq (car p) result-fields))) options)
              (distinct? (map car options)) (bounded? (list columns options) 16384)
              (<= (length demand) 1) (for-all procedure? demand))
      (error 'make-result "invalid prepared collection"))
    (let ([complete (get options 'complete #t)] [default (get options 'default '())]
          [details (get options 'details '())]
          [sortable (get options 'sortable (map car (filter (lambda (c) (scalar? (caddr c))) columns)))])
      (unless (and (boolean? complete) (list? default) (<= (length default) 1)
                (list? sortable) (for-all (lambda (c) (assq c columns)) sortable) (distinct? sortable))
        (error 'make-result "invalid result options" options))
      (%make-result (datum:copy columns) count row-at locate seek complete
        (datum:copy default) (datum:copy details) (datum:copy sortable) (and (pair? demand) (car demand)))))

  (edoc "Register base preparation for a source kind. Start receives source-envelope, query (id/filter/sort), cancelled? and publish! (result diagnostic); it queues work and returns promptly. Publish an immutable result or false plus a diagnostic. Only the current ticket can publish."
        (kind symbol "source kind") (schema integer "source schema") (start procedure "nonblocking provider dispatch"))
  (define (register! kind schema start)
    (unless (and (symbol? kind) (natural? schema) (> schema 0) (procedure? start)) (error 'register! "invalid provider"))
    (kernel:registry-add! providers (cons (list kind schema) start)))
  (define (definition r)
    (kernel:registry-find providers (lambda (p) (equal? (car p) (list (field r 'kind) (field r 'schema))))))

  (edoc "Register source copying for queries explicitly owned by a view. The procedure receives actor and source envelope, returning reference mappings and a rollback thunk. Mappings include the source and its private output. Borrowed queries are never copied."
    (kind symbol "source kind") (schema integer "source schema") (copy procedure "actor, source -> mappings, rollback"))
  (define (register-copy! kind schema copy)
    (unless (and (symbol? kind) (natural? schema) (> schema 0) (procedure? copy)) (error 'register-copy! "invalid copier"))
    (kernel:registry-add! copiers (cons (list kind schema) copy)))
  (define (copy-query! actor r)
    (let* ([v (field r 'value)] [source (model:snapshot (field v 'source))]
           [copy (and source (kernel:registry-find copiers
                               (lambda (p) (equal? (car p) (list (field source 'kind) (field source 'schema))))))])
      (unless copy (error 'copy-query! "provider cannot copy a privately owned query"))
      (let-values ([(mapping rollback) ((cdr copy) actor source)])
        (values (lambda (mapped)
                  (query-spec (mapped (field v 'source)) (field v 'filter) (field v 'sort) (field r 'persistence)
                    (map mapped (get v 'owned '())) (mapped (field r 'scope)))) mapping rollback))))
  (define lifecycle (view:register-resource-kind! 'collection 1 copy-query!
                      (lambda (r) (get (field r 'value) 'owned '()))))
  (define (validate-sort! sortable sort)
    (unless (and (list? sort)
              (let loop ([rest sort] [seen '()])
                (or (null? rest)
                  (let ([s (car rest)])
                    (and (list? s) (= (length s) 2) (memq (car s) sortable) (not (memq (car s) seen))
                      (memq (cadr s) '(ascending descending)) (loop (cdr rest) (cons (car s) seen)))))))
      (error 'collection "unsupported compound sort" sort)))
  (define (query-record id)
    (let ([r (model:snapshot id)]) (and r (eq? (field r 'kind) 'collection) (= (field r 'schema) 1) r)))
  (define (input-key r)
    (let* ([id (field r 'id)] [v (field r 'value)]
           [source (connection:read id 'source)] [filter-result (connection:read id 'filter)]
           [source-id (and (eq? (car source) 'ready) (cadr source))])
      (define (dependencies result) (filter (lambda (b) (not (equal? (car b) id))) (caddr result)))
      (list (list-head source 2) (list-head filter-result 2) (and source-id (model:revision source-id)) (field v 'sort)
        (list (dependencies source) (dependencies filter-result)) (with-mutex lock contract-generation))))
  (define (current? job)
    (and (with-mutex lock (eq? job (hashtable-ref desired (job-id job) #f)))
      (let ([r (query-record (job-id job))]) (and r (equal? (job-key job) (input-key r)) r))))
  (define (publish! job result diagnostic)
    (unless (or (and (result? result) (not diagnostic)) (and (not result) (string? diagnostic)))
      (error 'collection "expected a prepared result or diagnostic"))
    (when result (validate-sort! (result-sortable result) (list-ref (job-key job) 3)))
    (with-mutex (job-mutex job)
      (let retry ()
        (let ([r (current? job)])
          (and r
            (let* ([id (job-id job)] [v (field r 'value)]
                   [old (with-mutex lock (hashtable-ref results id #f))]
                   [basis (list id (job-serial job))]
                   [generation (+ 1 (field v 'generation))]
                   [next (make-publication generation basis result)])
              (if (and result old (equal? basis (publication-basis old)) (eq? result (publication-result old))) #t
                (let ([value (set-fields v
                               (list (cons 'status (if result 'ready 'unavailable)) (cons 'generation generation)
                                 (cons 'count (if result (result-count result) 0))
                                 (cons 'columns (if result (result-columns result) (field v 'columns)))
                                 (cons 'basis basis)
                                 (cons 'diagnostic (if (bounded? diagnostic 8192) diagnostic "Provider diagnostic exceeds budget"))
                                 (cons 'complete (and result (result-complete result)))
                                 (cons 'default (if result (result-default result) '()))
                                 (cons 'details (if result (result-details result) '()))
                                 (cons 'sortable (if result (result-sortable result) '()))))])
                  ;; The immutable index precedes its compact summary. A failed
                  ;; revision guard restores only the publication we replaced.
                  (and (with-mutex lock
                         (and (eq? job (hashtable-ref desired id #f)) (begin (hashtable-set! results id next) #t)))
                    (let-values ([(status ignored) (model:commit! '(base collection)
                                                     (list (list id (field r 'revision) (field r 'references) value)))])
                      (unless (eq? status 'applied)
                        (with-mutex lock
                          (when (eq? next (hashtable-ref results id #f))
                            (if old (hashtable-set! results id old) (hashtable-delete! results id)))))
                      (if (eq? status 'stale) (retry) (eq? status 'applied))))))))))))
  (define envelopes (make-hashtable equal-hash equal?))
  (define (source-envelope id)
    (let* ([revision (model:revision id)] [old (with-mutex lock (hashtable-ref envelopes id #f))])
      (if (and old (equal? revision (field old 'revision))) old
        (let ([r (model:snapshot id)])
          (unless r (error 'collection "source is unavailable" id))
          (with-mutex lock (hashtable-set! envelopes id r)) r))))
  (define (work!)
    (let loop ()
      (let ([job (with-mutex lock
                   (let wait ()
                     (when (zero? (hashtable-size pending)) (condition-wait ready lock) (wait)))
                   (let ([job (car (list-sort (lambda (a b) (< (job-serial a) (job-serial b)))
                                     (vector->list (hashtable-values pending))))])
                     (hashtable-delete! pending (job-id job)) job))])
        (if (current? job)
          (guard (ex [else (publish! job #f (kernel:condition-text ex))])
            (let* ([key (job-key job)] [source (car key)] [filter (cadr key)])
              (unless (and (eq? (car source) 'ready) (eq? (car filter) 'ready))
                (error 'collection "query input is unavailable"))
              (let* ([r (source-envelope (cadr source))] [provider (definition r)])
                (unless provider (error 'collection "row provider is unavailable"))
                ((cdr provider) r
                 (list (cons 'id (job-id job)) (cons 'filter (cadr filter)) (cons 'sort (cadddr key)))
                 (lambda () (with-mutex lock (not (eq? job (hashtable-ref desired (job-id job) #f)))))
                 (lambda (result diagnostic) (publish! job result diagnostic))))))
          (schedule! (job-id job)))
        (loop))))
  (define (release! id idle?)
    (let ([job (with-mutex lock
                 (and (or (not idle?) (not (model:demanded? id)))
                   (let ([job (hashtable-ref desired id #f)])
                     (hashtable-delete! pending id) (hashtable-delete! desired id) (hashtable-delete! results id) job)))])
      (when (and job (job-subscription job)) (model:unsubscribe! (job-subscription job)))))
  (define (schedule! id)
    (let ([r (query-record id)])
      (if (not r)
        (let* ([deleted? (not (model:snapshot id))]
               [resources (with-mutex lock
                            (let ([refs (hashtable-ref owned id '())])
                              (when deleted? (hashtable-delete! owned id))
                              refs))])
          (release! id #f)
          (when deleted?
            (for-each (lambda (ref)
                        (if (eq? (car ref) 'buffer)
                          (when (store:exists? ref) (store:delete! '(base collection) ref))
                          (let ([r (model:snapshot ref)]) (when r (model:retire! '(base collection) ref (field r 'revision)))))) resources)
            (view:retire-scope! '(base collection) id)))
        (begin
          (with-mutex lock (hashtable-set! owned id (get (field r 'value) 'owned '())))
          (if (not (model:demanded? id)) (release! id #t)
            (let* ([key (input-key r)] [previous #f]
                   [job (with-mutex lock
                          (let ([old (hashtable-ref desired id #f)])
                            (and (model:demanded? id) (or (not old) (not (equal? key (job-key old))))
                              (begin (set! serial (+ serial 1))
                                (set! previous old)
                                (let ([job (make-job id key serial (make-mutex) #f)])
                                  (hashtable-set! desired id job) job)))))])
              (when job
                ;; Derived queries retain their upstream models for exactly the
                ;; lifetime of their own demand. Acquire before releasing the old
                ;; subscription so unchanged dependencies never briefly go idle.
                (let* ([dependencies (remp (lambda (ref) (or (equal? id ref) (not (row:source? ref))))
                                       (append (if (eq? (caar key) 'ready) (list (cadar key)) '())
                                         (map car (apply append (list-ref key 4)))))]
                       [token (kernel:call-with-runtime-registrations
                                (lambda () (model:subscribe! dependencies (lambda (notice) (void)))))])
                  (job-subscription-set! job token)
                  (unless (with-mutex lock (eq? job (hashtable-ref desired id #f))) (model:unsubscribe! token)))
                (when (and previous (job-subscription previous)) (model:unsubscribe! (job-subscription previous)))
                (let publish-pending ()
                  (let ([r (current? job)])
                    (when r
                      (let* ([v (field r 'value)] [filter (cadr key)]
                             [changes (append (list '(status . pending) (cons 'basis (list id (job-serial job)))
                                                '(complete . #f) '(details) '(default))
                                        (if (eq? (car filter) 'ready) (list (cons 'input-filter (cadr filter))) '()))])
                        (let-values ([(status ignored) (model:commit! '(base collection)
                                                         (list (list id (field r 'revision) (field r 'references) (set-fields v changes))))])
                          (when (eq? status 'stale) (publish-pending)))))))
                (with-mutex lock
                  (when (eq? job (hashtable-ref desired id #f))
                    (hashtable-set! pending id job)
                    (unless worker? (set! worker? #t) (fork-thread work!))
                    (condition-signal ready))))))))))
  (define (rescan!)
    (for-each schedule! (model:ids 'collection))
    (let ([ids (with-mutex lock (vector->list (hashtable-keys owned)))])
      (for-each (lambda (id) (unless (query-record id) (schedule! id))) ids))
    (prune-envelopes!))
  (define (prune-envelopes!)
    (with-mutex lock
      (let ([sources (map (lambda (job) (and (eq? (caar (job-key job)) 'ready) (cadar (job-key job))))
                       (vector->list (hashtable-values desired)))])
        (vector-for-each (lambda (id) (unless (member id sources) (hashtable-delete! envelopes id))) (hashtable-keys envelopes)))))
  (define (invalidate! ids)
    (if (not ids) (rescan!)
      (let ([jobs (with-mutex lock (vector->list (hashtable-values desired)))])
        (for-each (lambda (p) (when (eq? (cadr p) 'collection) (schedule! (car p))))
          (model:metadata (filter (lambda (id) (and (row:source? id) (not (with-mutex lock (hashtable-contains? owned id))))) ids)))
        (for-each (lambda (id)
                    (when (with-mutex lock (and (hashtable-contains? owned id) (not (hashtable-contains? desired id))))
                      (schedule! id))) ids)
        (for-each
          (lambda (job)
            (let* ([key (job-key job)] [id (job-id job)]
                   [dependencies (cons id (append (if (eq? (caar key) 'ready) (list (cadar key)) '())
                                            (map car (apply append (list-ref key 4)))))])
              (when (exists (lambda (id) (member id ids)) dependencies) (schedule! id)))) jobs)
        (prune-envelopes!))))

  (edoc "Recover query ownership and rebuild indexes with active demand; saved recipes alone never start providers." (public))
  (define (init!) (rescan!))
  (define notices
    (list (model:subscribe! #f (lambda (notice) (invalidate! (cadr notice))))
      (model:observe-demand!
        (lambda (ids)
          (for-each (lambda (id) (when (with-mutex lock (hashtable-contains? owned id)) (schedule! id))) ids)
          (prune-envelopes!)))
      (store:subscribe! #f (lambda (event) (invalidate! (list (cadr event)))))
      (port:observe! (lambda () (with-mutex lock (set! contract-generation (+ contract-generation 1))) (rescan!)))
      (kernel:registry-observe! providers
        (lambda (removed added) (with-mutex lock (set! contract-generation (+ contract-generation 1))) (rescan!)))))

  (edoc "Create a vector-backed row source with raw portable cells, stable keys and explicit restart policy."
        (actor actor "creator") (columns list "column declarations") (rows vector "raw rows")
        (persistence (one-of transient persistent) "restart policy") (returns list))
  (define (create-source! actor columns rows persistence)
    (model:create! actor 'collection-vector 1 'session persistence '() (list columns rows)))

  (edoc "Create a query whose provider owns filtering and ordering. Scoped model subscriptions retain its work; creating or recovering an unobserved recipe does not start a provider."
        (actor actor "creator") (source row-source "source model") (filter string "provider filter")
        (sort list "compound keys") (persistence (one-of transient persistent) "restart policy")
        (resources (list-of list) "optional owned references, then optional owning view; owned resources require persistence") (returns list))
  (define (create! actor source filter sort persistence . resources)
    (unless (<= (length resources) 2) (error 'create! "expected optional owned resources and owning view"))
    (let* ([refs (if (null? resources) '() (car resources))]
           [owner (and (= (length resources) 2) (model:snapshot (cadr resources)))])
      (when (and (= (length resources) 2) (not (and owner (eq? (field owner 'kind) 'widget-view))))
        (error 'create! "owning view is unavailable"))
      (unless (and (list? refs) (or (null? refs) (eq? persistence 'persistent)))
        (error 'create! "owned resources require a persistent query" refs))
      (let ([ids (model:allocate! actor 1
                   (lambda (ids) (list (query-spec source filter sort persistence refs (if owner (field owner 'id) 'session))))
                   (lambda (ids) (if owner (list (list (field owner 'id) (field owner 'revision) (field owner 'references) (field owner 'value))) '())))])
        (unless ids (error 'create! "owning view changed"))
        (schedule! (car ids)) (car ids))))
  (define (query-spec source filter sort persistence refs scope)
    (list 'collection 1 scope persistence (cons source (remove source refs))
      (map cons (append recipe-fields result-fields '(owned))
        (list source filter sort 'pending 0 0 '() #f #f #f '() '() '() refs))))

  (edoc "Change a query recipe against its revision. A connected filter is edited through its producer."
        (actor actor "caller") (id row-source "query") (revision integer "expected revision") (changes list "filter/sort fields"))
  (define (configure! actor id revision changes)
    (unless (and (list? changes) (<= (length changes) 2)
              (for-all (lambda (p) (and (pair? p) (memq (car p) '(filter sort)))) changes) (distinct? (map car changes)))
      (error 'configure! "expected distinct filter/sort fields"))
    (let ([r (query-record id)] [bundle (connection:snapshot (list id))])
      (unless r (error 'configure! "query is unavailable" id))
      (when (assq 'sort changes)
        (validate-sort! (get (field r 'value) 'sortable '()) (cdr (assq 'sort changes))))
      (when (and (assq 'filter changes) (exists (lambda (e) (and (equal? id (cadr e)) (eq? (caddr e) 'filter))) (cadr bundle)))
        (error 'configure! "filter is connected"))
      (let* ([old (field r 'value)] [updated (set-fields old changes)]
             [v (if (equal? old updated) old (set-fields updated '((status . pending))))]
             [basis (car bundle)] [top (and basis (model:snapshot (car basis)))])
        (if (not top) (values 'stale (list r))
          (model:commit! actor (list (list id revision (field r 'references) v)
                                 (list (car basis) (cadr basis) (field top 'references) (field top 'value))))))))

  (edoc "Read compact query metadata; a changed input is pending until its provider publishes."
        (id row-source "query") (returns any) (effects internal))
  (define (summary id)
    (let* ([r (query-record id)] [job (with-mutex lock (hashtable-ref desired id #f))])
      (and r (if (and job (equal? (job-key job) (input-key r))
                   (or (eq? (field (field r 'value) 'status) 'pending)
                     (equal? (field (field r 'value) 'basis) (list id (job-serial job))))) r
               (set-fields r (list (cons 'value (set-fields (field r 'value) '((status . pending))))))))))
  (define (prepared id generation)
    (let* ([r (summary id)] [v (and r (field r 'value))] [p (with-mutex lock (hashtable-ref results id #f))])
      (and v (eq? (field v 'status) 'ready) (= generation (field v 'generation)) p
        (= generation (publication-generation p)) (equal? (field v 'basis) (publication-basis p)) p)))

  ;; Vector queries keep one computation worker. Other providers dispatch to
  ;; their own service; a directory walk never joins this queue.
  (define flat-cache (make-weak-eq-hashtable))
  (define flat-lock (make-mutex))
  (define flat-ready (make-condition))
  (define flat-pending (make-hashtable equal-hash equal?))
  (define flat-worker? #f)
  (define flat-serial 0)
  (define-condition-type &pending-source &condition make-pending-source pending-source?)
  (define (indexed-seek count eligible)
    (lambda (ordinal direction offset)
      (let ([n (if eligible (vector-length eligible) count)])
        (and (> n 0)
          (let* ([forward? (eq? direction 'forward)]
                 [index (if (not eligible) ordinal
                          (let search ([lo 0] [hi n])
                            (if (= lo hi)
                              (if forward? lo (- lo 1))
                              (let* ([mid (div (+ lo hi) 2)] [value (vector-ref eligible mid)])
                                (if (if forward? (< value ordinal) (<= value ordinal))
                                  (search (+ mid 1) hi) (search lo mid))))))]
                 [index (min (- n 1) (max 0 (+ index (if forward? offset (- offset)))))])
            (if eligible (vector-ref eligible index) index))))))
  (define (vector-capture r checkpoint!)
    (or (hashtable-ref flat-cache r #f)
      (let* ([value (field r 'value)] [columns (car value)] [rows (cadr value)]
             [n (vector-length rows)] [positions (make-hashtable equal-hash equal?)] [eligible '()])
        (do ([i 0 (+ i 1)]) ((= i n))
          (when (zero? (mod i 128)) (checkpoint!))
          (let* ([row (vector-ref rows i)] [key (car row)])
            (when (hashtable-contains? positions key) (error 'collection "duplicate row key" key))
            (hashtable-set! positions key i)
            (when (row:selectable? row) (set! eligible (cons i eligible)))))
        (let ([result (make-result columns n (lambda (i) (vector-ref rows i))
                        (lambda (key) (hashtable-ref positions key #f))
                        (indexed-seek n (and (not (= (length eligible) n)) (list->vector (reverse eligible)))) '())])
          (hashtable-set! flat-cache r result) result))))
  (define (flat-result source query checkpoint!)
    (let* ([filter (field query 'filter)] [sort (field query 'sort)] [columns (result-columns source)])
      (validate-sort! (result-sortable source) sort)
      (if (and (string=? filter "") (null? sort)) source
        (let ([match (string:searcher filter #t)] [matches '()] [ticks 0])
          (do ([i 0 (+ i 1)]) ((= i (result-count source)))
            (when (zero? (mod i 128)) (checkpoint!))
            (let ([row ((result-at source) i)])
              (when (or (string=? filter "")
                      (exists (lambda (cell) (and (string? (cdr cell)) (match (cdr cell) 0 (string-length (cdr cell))))) (cadr row)))
                (set! matches (cons i matches)))))
          (let* ([order (list->vector (reverse matches))]
                 [order (if (null? sort) order
                          (vector-sort
                            (lambda (a b)
                              (set! ticks (+ ticks 1)) (when (zero? (mod ticks 128)) (checkpoint!))
                              (let ([ar ((result-at source) a)] [br ((result-at source) b)])
                                (let compare ([keys sort])
                                  (if (null? keys) (< a b)
                                    (let ([x (assq (caar keys) (cadr ar))] [y (assq (caar keys) (cadr br))])
                                      (cond [(row:less? x y) (eq? (cadar keys) 'ascending)]
                                        [(row:less? y x) (eq? (cadar keys) 'descending)] [else (compare (cdr keys))])))))) order))]
                 [n (vector-length order)] [positions (make-hashtable equal-hash equal?)] [eligible '()])
            (do ([i 0 (+ i 1)]) ((= i n))
              (when (zero? (mod i 128)) (checkpoint!))
              (let ([row ((result-at source) (vector-ref order i))])
                (hashtable-set! positions (car row) i)
                (when (row:selectable? row) (set! eligible (cons i eligible)))))
            (apply make-result columns n (lambda (i) ((result-at source) (vector-ref order i)))
              (lambda (key) (hashtable-ref positions key #f))
              (indexed-seek n (and (not (= (length eligible) n)) (list->vector (reverse eligible))))
              (list (cons 'complete (result-complete source)))
              (if (result-demand source)
                (list (lambda (ordinals columns) ((result-demand source) (map (lambda (i) (vector-ref order i)) ordinals) columns))) '())))))))
  (define (flat-work!)
    (let loop ()
      (let ([item (with-mutex flat-lock
                    (let wait () (when (zero? (hashtable-size flat-pending)) (condition-wait flat-ready flat-lock) (wait)))
                    (let ([item (car (list-sort (lambda (a b) (< (car a) (car b))) (vector->list (hashtable-values flat-pending))))])
                      (hashtable-delete! flat-pending (cadr item)) item))])
        ((caddr item)))
      (loop)))
  (define (start-flat! r query cancelled? publish)
    (with-mutex flat-lock
      (set! flat-serial (+ flat-serial 1))
      (hashtable-set! flat-pending (field query 'id)
        (list flat-serial (field query 'id)
          (lambda ()
            (call/cc
              (lambda (cancel)
                (define (checkpoint!) (when (cancelled?) (cancel #f)))
                (guard (ex [(pending-source? ex) (void)] [else (publish #f (kernel:condition-text ex))])
                  (checkpoint!)
                  (let* ([source (if (eq? (field r 'kind) 'collection-vector) (vector-capture r checkpoint!)
                                   (let ([p (prepared (field r 'id) (field (field r 'value) 'generation))])
                                     (unless p (raise (make-pending-source))) (publication-result p)))]
                         [result (flat-result source query checkpoint!)])
                    (checkpoint!) (publish result #f))))))))
      (unless flat-worker? (set! flat-worker? #t) (fork-thread flat-work!))
      (condition-signal flat-ready)))
  (define vector-provider (register! 'collection-vector 1 start-flat!))
  (define query-provider (register! 'collection 1 start-flat!))

  (edoc "Read at most 256 prepared rows under a generation and whole-reply byte budget. Reply is (ready generation basis start rows total), stale or unavailable."
        (id row-source "query") (generation integer "result generation") (start integer "first ordinal")
        (count integer "row limit") (columns list "column IDs") (returns list) (effects internal))
  (define (range id generation start count columns)
    (unless (and (natural? generation) (natural? start) (natural? count) (list? columns) (for-all symbol? columns) (distinct? columns))
      (error 'range "invalid range request"))
    (let ([p (prepared id generation)])
      (if (not p) '(stale)
        (let* ([r (publication-result p)] [end (min (result-count r) (+ start (min 256 count)))]
               [start (min start end)] [basis (publication-basis p)]
               [header (bytevector-length (wire:encode (list 'ready generation basis start '() (result-count r))))])
          (define (reply rows)
            (when (and (pair? rows) (result-demand r)) ((result-demand r) (map car rows) columns))
            (list 'ready generation basis start (reverse rows) (result-count r)))
          (unless (for-all (lambda (c) (assq c (result-columns r))) columns) (error 'range "unknown column" columns))
          (let loop ([i start] [rows '()] [bytes header])
            (if (= i end) (reply rows)
              (let* ([raw ((result-at r) i)]
                     [_ (unless (row:valid? (result-columns r) raw) (error 'range "invalid provider row" i))]
                     [cells (map (lambda (c)
                                   (let ([cell (assq c (cadr raw))])
                                     (cond [(memq c (get (caddr raw) 'pending '())) (list c 'pending)]
                                       [(not cell) (list c 'absent)] [(not (bounded? (cdr cell) 65536)) (list c 'unavailable 'oversized-cell)]
                                       [else (list c 'ready (cdr cell))]))) columns)]
                     [row (list i (car raw) cells (caddr raw))]
                     [size (guard (ex [else 524289]) (+ 8 (bytevector-length (wire:encode row))))])
                (cond [(> (+ header size) 524288) '(unavailable oversized-row)]
                  [(> (+ bytes size) 524288) (reply rows)]
                  [else (loop (+ i 1) (cons (datum:copy row) rows) (+ bytes size))]))))))))
  (define (ordinal-reply p ordinal)
    (unless (or (not ordinal) (and (natural? ordinal) (< ordinal (result-count (publication-result p)))))
      (error 'collection "provider returned an invalid ordinal" ordinal))
    (list 'ready (publication-generation p) (publication-basis p) ordinal (result-count (publication-result p))))

  (edoc "Locate a stable key without transferring the key vector; false in a ready reply means absent."
        (id row-source "query") (generation integer "result generation") (key datum "row key") (returns list) (effects internal))
  (define (rank id generation key)
    (let ([p (prepared id generation)]) (if p (ordinal-reply p ((result-locate (publication-result p)) key)) '(stale))))

  (edoc "Read one stable key at an explicit generation, without separate rank and range requests. An absent key returns a ready range with no rows; changed preparation returns stale."
        (id row-source "query") (generation integer "result generation") (key datum "stable row key")
        (columns list "requested columns") (returns list) (effects internal))
  (define (lookup id generation key columns)
    (let ([p (prepared id generation)])
      (if (not p) '(stale)
        (let ([ordinal ((result-locate (publication-result p)) key)])
          (ordinal-reply p ordinal)
          (range id generation (or ordinal 0) (if ordinal 1 0) columns)))))

  (edoc "Navigate a prepared selectable index from an inclusive ordinal origin, clamping at its ends. Zero offset finds the first eligible row in the direction."
        (id row-source "query") (generation integer "result generation") (ordinal integer "display origin")
        (direction (one-of forward backward) "search direction") (offset integer "selectable steps") (returns list) (effects internal))
  (define (seek id generation ordinal direction offset)
    (unless (and (natural? ordinal) (natural? offset) (memq direction '(forward backward))) (error 'seek "invalid navigation"))
    (let ([p (prepared id generation)])
      (if p (ordinal-reply p ((result-seek (publication-result p)) ordinal direction offset)) '(stale))))

  (edoc "Read up to four prepared range, rank or selectable-seek requests in one bounded batch."
        (requests list "range/rank/seek operations") (returns list) (effects internal))
  (define (fetch requests)
    (unless (and (list? requests) (<= (length requests) 4)) (error 'fetch "expected at most four requests"))
    (map (lambda (r)
           (unless (and (list? r) (pair? r)) (error 'fetch "invalid request"))
           (case (car r) [(range) (apply range (cdr r))] [(rank) (apply rank (cdr r))] [(seek) (apply seek (cdr r))]
             [else (error 'fetch "unknown row operation" (car r))])) requests))
)
