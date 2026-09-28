;; Prepared base indexes, compact query recipes, and bounded row/rank reads.
(import (only (foundation edoc) elibrary))
(elibrary (state collection)
  (export configure! create! create-source! fetch range rank register! summary)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core port) port:)
          (prefix (core row) row:) (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:) (prefix (foundation string) string:)
          (prefix (foundation wire) wire:) (prefix (state connection) connection:)
          (prefix (state model) model:) (prefix (state store) store:))
  (define providers (kernel:make-registry car))
  (define cache (make-hashtable equal-hash equal?))
  (define results (make-hashtable equal-hash equal?))
  (define pending (make-hashtable equal-hash equal?))
  (define desired (make-hashtable equal-hash equal?))
  (define lock (make-mutex))
  (define ready (make-condition))
  (define worker? #f)
  (define contract-generation 0)
  (define (field r key) (cdr (assq key r)))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (set-fields v changes) (map (lambda (p) (or (assq (car p) changes) p)) v))
  (define (recipe? v)
    (and (list? v) (for-all pair? v)
      (equal? (map car v) '(source filter sort status generation count columns basis diagnostic))
      (row:source? (field v 'source)) (string? (field v 'filter)) (list? (field v 'sort))
      (memq (field v 'status) '(pending ready unavailable))
      (natural? (field v 'generation)) (natural? (field v 'count)) (list? (field v 'columns))))
  (define registrations
    (begin (row:init!) (model:register-kind! 'collection 1 recipe?)
      (model:register-kind! 'collection-vector 1
        (lambda (v) (and (list? v) (= (length v) 2) (row:columns? (car v)) (vector? (cadr v))
                      (let loop ([i 0])
                        (or (= i (vector-length (cadr v)))
                          (and (row:valid? (car v) (vector-ref (cadr v) i)) (loop (+ i 1))))))))))
  ;; A prepared source owns its immutable provider capture. Results refer to
  ;; it; many queries never copy a vector or build another key map.
  (define-record-type source (fields identity columns count at locate))
  (define-record-type result (fields generation basis source order positions count))
  (define-condition-type &pending-source &condition make-pending-source pending-source?)
  (define (definition r)
    (kernel:registry-find providers (lambda (p) (equal? (car p) (list (field r 'kind) (field r 'schema))))))
  (define (capture-source id cancel!)
    (let* ([revision (model:revision id)] [old (with-mutex lock (hashtable-ref cache id #f))])
      (if (and old (row:columns? (source-columns old)) (equal? revision (cadr (source-identity old)))
            (eq? (caddr (source-identity old)) (kernel:registry-find providers
                                                 (lambda (p) (equal? (car p) (car (caddr (source-identity old)))))))) old
        (let* ([r (model:snapshot id)] [provider (and r (definition r))])
          (unless provider (error 'collection "row provider is unavailable" id))
          (let ([captured ((cdr provider) r cancel!)])
            (unless (and (list? captured) (= (length captured) 4) (row:columns? (car captured))
                      (natural? (cadr captured)) (procedure? (caddr captured)) (procedure? (cadddr captured)))
              (error 'collection "provider returned an invalid capture"))
            (let ([s (apply make-source (list id (field r 'revision) provider) captured)])
              (with-mutex lock (hashtable-set! cache id s)) s))))))

  (edoc "Register an indexed row provider for a source kind. Capture receives an owned source envelope and a cancellation checkpoint and returns (columns count row-at locate-key); those callbacks read its immutable snapshot."
        (kind symbol "source model kind") (schema integer "version") (capture procedure "base-only snapshot/index adapter"))
  (define (register! kind schema capture)
    (unless (and (symbol? kind) (natural? schema) (> schema 0) (procedure? capture)) (error 'register! "invalid row provider"))
    (kernel:registry-add! providers (cons (list kind schema) capture)))

  (define vector-provider
    (register! 'collection-vector 1
      (lambda (r cancel!)
        (let* ([v (field r 'value)] [columns (car v)] [rows (cadr v)] [keys (make-hashtable equal-hash equal?)])
          (do ([i 0 (+ i 1)]) ((= i (vector-length rows)))
            (when (zero? (modulo i 128)) (cancel!))
            (let ([key (car (vector-ref rows i))])
              (when (hashtable-contains? keys key) (error 'collection "duplicate row key" key))
              (hashtable-set! keys key i)))
          (list columns (vector-length rows) (lambda (i) (vector-ref rows i))
            (lambda (key) (hashtable-ref keys key #f)))))))

  (define (sortable? type)
    (exists (lambda (parent) (edoc:type-compatible? type parent)) '(number string boolean)))
  (define (validate-sort! columns sort)
    (unless (and (list? sort)
              (let loop ([rest sort] [seen '()])
                (or (null? rest)
                  (let* ([s (car rest)] [column (and (list? s) (= (length s) 2) (assq (car s) columns))])
                    (and column (not (memq (car s) seen)) (sortable? (caddr column))
                      (memq (cadr s) '(ascending descending)) (loop (cdr rest) (cons (car s) seen)))))))
      (error 'collection "unsupported compound sort" sort)))
  (define (input id name) (connection:read id name))
  (define (job-key r)
    (let* ([id (field r 'id)] [v (field r 'value)] [source (input id 'source)] [filter-result (input id 'filter)]
           [source-id (and (eq? (car source) 'ready) (cadr source))])
      ;; Ignore the query's own result publication in the input revision
      ;; vector: its recipe is already captured explicitly below.
      (define (dependencies result) (filter (lambda (b) (not (equal? (car b) id))) (caddr result)))
      (list (list-head source 2) (list-head filter-result 2) (and source-id (model:revision source-id)) (field v 'sort)
        (list (dependencies source) (dependencies filter-result))
        (with-mutex lock contract-generation))))
  (define (query-record id)
    (let ([r (model:snapshot id)]) (and r (eq? (field r 'kind) 'collection) (= (field r 'schema) 1) r)))
  (define (current? id key)
    (and (with-mutex lock (equal? key (hashtable-ref desired id #f)))
      (let ([r (query-record id)]) (and r (equal? key (job-key r))))))
  (define (publish! id key status computed diagnostic)
    (let ([r (query-record id)])
      (when (and r (current? id key))
        (let* ([v (field r 'value)] [generation (+ 1 (field v 'generation))]
               [next (set-fields v
                       (list (cons 'status status) (cons 'generation generation)
                         (cons 'count (if computed (result-count computed) 0))
                         (cons 'columns (if computed (source-columns (result-source computed)) (field v 'columns)))
                         (cons 'basis key) (cons 'diagnostic diagnostic)))])
          ;; Install the immutable result before publishing its generation.
          ;; A range reader also checks the canonical summary before using it.
          (when computed
            (with-mutex lock
              (hashtable-set! results id (make-result generation key (result-source computed)
                                           (result-order computed) (result-positions computed) (result-count computed)))))
          (let-values ([(status ignored) (model:commit! '(base collection)
                                           (list (list id (field r 'revision) (field r 'references) next)))])
            (unless (eq? status 'applied) (schedule! id)))))))
  (define (compute! id key cancel!)
    (let ([source-result (car key)] [filter-result (cadr key)] [sort (cadddr key)])
      (unless (and (eq? (car source-result) 'ready) (eq? (car filter-result) 'ready))
        (error 'collection "query input is unavailable"))
      (let* ([s (capture-source (cadr source-result) cancel!)] [filter (cadr filter-result)]
             [match (string:searcher filter #t)] [count (source-count s)] [columns (source-columns s)])
        (validate-sort! columns sort)
        (if (and (string=? filter "") (null? sort)) (make-result 0 key s #f #f count)
          (let ([matches '()])
            (do ([i 0 (+ i 1)]) ((= i count))
              (when (zero? (modulo i 128)) (cancel!))
              (let ([row ((source-at s) i)])
                (when (or (string=? filter "")
                        (exists (lambda (cell) (and (string? (cdr cell)) (match (cdr cell) 0 (string-length (cdr cell))))) (cadr row)))
                  (set! matches (cons i matches)))))
            (let* ([ticks 0]
                   [order (list->vector (reverse matches))]
                   [order (if (null? sort) order
                            (vector-sort
                              (lambda (a b)
                                (set! ticks (+ ticks 1)) (when (zero? (modulo ticks 128)) (cancel!))
                                (let ([a-row ((source-at s) a)] [b-row ((source-at s) b)])
                                  (let compare ([sort sort])
                                    (if (null? sort) (< a b)
                                      (let ([x (assq (caar sort) (cadr a-row))] [y (assq (caar sort) (cadr b-row))])
                                        (cond [(row:less? x y) (eq? (cadar sort) 'ascending)]
                                          [(row:less? y x) (eq? (cadar sort) 'descending)]
                                          [else (compare (cdr sort))])))))) order))]
                   [positions (make-hashtable equal-hash equal?)])
              (do ([i 0 (+ i 1)]) ((= i (vector-length order)))
                (when (zero? (modulo i 128)) (cancel!))
                (hashtable-set! positions (car ((source-at s) (vector-ref order i))) i))
              (make-result 0 key s order positions (vector-length order))))))))
  (define (work!)
    (let loop ()
      (let-values ([(id key)
                    (with-mutex lock
                      (let wait ()
                        (when (zero? (hashtable-size pending)) (condition-wait ready lock) (wait)))
                      (let* ([id (vector-ref (hashtable-keys pending) 0)] [key (hashtable-ref pending id #f)])
                        (hashtable-delete! pending id) (values id key)))])
        (call/cc
          (lambda (cancel)
            (define (checkpoint!)
              (unless (with-mutex lock (equal? key (hashtable-ref desired id #f))) (cancel #f)))
            (guard (ex [(pending-source? ex) (void)]
                     [else (when (current? id key) (publish! id key 'unavailable #f (kernel:condition-text ex)))])
              (checkpoint!)
              (let ([result (compute! id key checkpoint!)]) (checkpoint!) (publish! id key 'ready result #f)))))
        (loop))))
  (define (schedule! id)
    (let ([r (query-record id)])
      (if (not r)
        (with-mutex lock
          (hashtable-delete! pending id) (hashtable-delete! desired id) (hashtable-delete! results id)
          (let ([sources (map (lambda (key) (and (eq? (caar key) 'ready) (cadar key))) (vector->list (hashtable-values desired)))])
            (vector-for-each (lambda (id) (unless (member id sources) (hashtable-delete! cache id))) (hashtable-keys cache))))
        (let* ([key (job-key r)]
               [changed? (with-mutex lock
                           (and (not (equal? key (hashtable-ref desired id #f)))
                             (begin (hashtable-set! desired id key) (hashtable-delete! results id) #t)))])
          (when changed?
            (let ([v (field r 'value)])
              (unless (eq? (field v 'status) 'pending)
                (model:commit! '(base collection)
                  (list (list id (field r 'revision) (field r 'references) (set-fields v '((status . pending))))))))
            (with-mutex lock
              (when (equal? key (hashtable-ref desired id #f))
                (hashtable-set! pending id key)
                (unless worker? (set! worker? #t) (fork-thread work!))
                (condition-signal ready))))))))
  (define (rescan!)
    (let ([ids (model:ids 'collection)])
      (for-each schedule! ids)
      (with-mutex lock
        (vector-for-each
          (lambda (id) (unless (member id ids)
                         (hashtable-delete! pending id) (hashtable-delete! desired id) (hashtable-delete! results id)))
          (hashtable-keys desired))
        (let ([sources (map (lambda (key) (and (eq? (caar key) 'ready) (cadar key))) (vector->list (hashtable-values desired)))])
          (vector-for-each (lambda (id) (unless (member id sources) (hashtable-delete! cache id))) (hashtable-keys cache))))))
  (define (invalidate! ids)
    (if (not ids) (rescan!)
      (let-values ([(queries keys) (with-mutex lock (hashtable-entries desired))])
        (for-each (lambda (id)
                    (when (eq? (car id) 'model)
                      (let ([revision (model:revision id)])
                        (with-mutex lock
                          (let ([old (hashtable-ref cache id #f)])
                            (when (and old (not (equal? revision (cadr (source-identity old))))) (hashtable-delete! cache id))))))) ids)
        (vector-for-each
          (lambda (id key)
            (let ([dependencies (cons id
                                  (append (if (eq? (caar key) 'ready) (list (cadar key)) '())
                                    (map car (apply append (list-ref key 4)))))])
              (when (exists (lambda (id) (member id ids)) dependencies) (schedule! id)))) queries keys))))
  (define notices
    (list (model:subscribe! #f (lambda (notice) (invalidate! (cadr notice))))
      (store:subscribe! #f (lambda (event) (invalidate! (list (list 'buffer (cadr event))))))
      (port:observe! (lambda ()
                       (with-mutex lock (set! contract-generation (+ contract-generation 1)) (hashtable-clear! cache)) (rescan!)))
      (kernel:registry-observe! providers
        (lambda (removed added)
          (with-mutex lock (set! contract-generation (+ contract-generation 1)) (hashtable-clear! cache)) (rescan!)))))

  (edoc "Create a canonical vector-backed row source. Store raw portable cells once, with stable keys and explicit restart policy."
        (actor actor "creator") (columns list "(id label type) declarations") (rows vector "(key cells attributes) rows")
        (persistence (one-of transient persistent) "restart policy") (returns list))
  (define (create-source! actor columns rows persistence)
    (model:create! actor 'collection-vector 1 'session persistence '() (list columns rows)))

  (edoc "Create a shared filter/sort query over a row provider; result work runs at the base."
        (actor actor "creator") (source row-source "source model") (filter string "literal substring")
        (sort list "(column ascending|descending) compound keys") (persistence (one-of transient persistent) "restart policy") (returns list))
  (define (create! actor source filter sort persistence)
    (let ([id (model:create! actor 'collection 1 'session persistence (list source)
                (map cons '(source filter sort status generation count columns basis diagnostic)
                  (list source filter sort 'pending 0 0 '() #f #f)))])
      (schedule! id) id))

  (edoc "Change a query recipe against its model revision. A connected filter must be edited through its producer."
        (actor actor "caller") (id row-source "query") (revision integer "expected model revision")
        (changes list "filter and/or sort fields"))
  (define (configure! actor id revision changes)
    (unless (and (list? changes) (<= (length changes) 2)
              (for-all (lambda (p) (and (pair? p) (memq (car p) '(filter sort)))) changes)
              (not (and (= (length changes) 2) (eq? (caar changes) (caadr changes)))))
      (error 'configure! "expected distinct filter/sort fields"))
    (let ([r (query-record id)] [bundle (connection:snapshot (list id))])
      (unless r (error 'configure! "query is unavailable" id))
      (when (assq 'filter changes)
        (when (exists (lambda (e) (and (equal? id (cadr e)) (eq? (caddr e) 'filter)))
                (cadr bundle)) (error 'configure! "filter is connected")))
      (let* ([old (field r 'value)] [updated (set-fields old changes)]
             [v (if (equal? old updated) old (set-fields updated '((status . pending))))]
             [basis (car bundle)] [top (and basis (model:snapshot (car basis)))])
        (if (not top) (values 'stale (list r))
          (model:commit! actor (list (list id revision (field r 'references) v)
                                 (list (car basis) (cadr basis) (field top 'references) (field top 'value))))))))

  (edoc "Read the small query envelope; a changed input reports pending until its index is published."
        (id row-source "query") (returns any) (effects internal))
  (define (summary id)
    (let ([r (query-record id)])
      (and r (let ([v (field r 'value)])
               (if (equal? (field v 'basis) (job-key r)) r
                 (set-fields r (list (cons 'value (set-fields v '((status . pending)))))))))))
  (define (prepared id generation)
    (let* ([r (summary id)] [v (and r (field r 'value))] [result (with-mutex lock (hashtable-ref results id #f))])
      (and v (eq? (field v 'status) 'ready) (= generation (field v 'generation)) result
        (= generation (result-generation result)) (equal? (field v 'basis) (result-basis result)) result)))

  (define query-provider
    (register! 'collection 1
      (lambda (r cancel!)
        (let* ([v (field r 'value)] [result (prepared (field r 'id) (field v 'generation))])
          (unless result
            (if (eq? (field v 'status) 'pending) (raise (make-pending-source))
              (error 'collection "upstream result is unavailable")))
          (let ([s (result-source result)])
            (list (source-columns s) (result-count result)
              (lambda (i) ((source-at s) (if (result-order result) (vector-ref (result-order result) i) i)))
              (lambda (key) (if (result-positions result) (hashtable-ref (result-positions result) key #f)
                              ((source-locate s) key)))))))))

  (edoc "Read at most 256 prepared rows under a result generation and byte budget. Return (ready generation basis start rows total), or explicit stale/unavailable."
        (id row-source "query") (generation integer "expected result") (start integer "first ordinal")
        (count integer "requested rows") (columns list "requested column IDs") (returns list) (effects internal))
  (define (range id generation start count columns)
    (unless (and (natural? generation) (natural? start) (natural? count) (list? columns) (for-all symbol? columns))
      (error 'range "invalid range request"))
    (let ([r (prepared id generation)])
      (if (not r) '(stale)
        (let* ([s (result-source r)] [end (min (result-count r) (+ start (min 256 count)))])
          (unless (for-all (lambda (c) (assq c (source-columns s))) columns) (error 'range "unknown column" columns))
          (let loop ([i (min start end)] [rows '()] [bytes 0])
            (if (= i end) (list 'ready generation (result-basis r) (min start end) (reverse rows) (result-count r))
              (let* ([ordinal (if (result-order r) (vector-ref (result-order r) i) i)]
                     [raw ((source-at s) ordinal)]
                     [cells (map (lambda (c)
                                   (let ([cell (assq c (cadr raw))])
                                     (cond [(not cell) (list c 'absent)]
                                       [(guard (ex [else #t]) (> (bytevector-length (wire:encode (cdr cell))) 65536))
                                        (list c 'unavailable 'oversized-cell)]
                                       [else (list c 'ready (cdr cell))]))) columns)]
                     [row (list i (car raw) cells (caddr raw))] [size (bytevector-length (wire:encode row))])
                (cond [(> size 524288) '(unavailable oversized-row)]
                  [(> (+ bytes size) 524288) (list 'ready generation (result-basis r) start (reverse rows) (result-count r))]
                  [else (loop (+ i 1) (cons (datum:copy row) rows) (+ bytes size))]))))))))

  (edoc "Locate a stable key without transferring the result's key vector; a ready false ordinal means absent."
        (id row-source "query") (generation integer "expected result") (key datum "stable key") (returns list) (effects internal))
  (define (rank id generation key)
    (let ([r (prepared id generation)])
      (if (not r) '(stale)
        (list 'ready generation (result-basis r)
          (if (result-positions r) (hashtable-ref (result-positions r) key #f)
            ((source-locate (result-source r)) key)) (result-count r)))))

  (edoc "Read up to four prepared range/rank requests in one bounded transport batch."
        (requests list "(range id generation start count columns) or (rank id generation key)") (returns list) (effects internal))
  (define (fetch requests)
    (unless (and (list? requests) (<= (length requests) 4)) (error 'fetch "expected at most four requests"))
    (map (lambda (r)
           (unless (and (list? r) (pair? r)) (error 'fetch "invalid request"))
           (case (car r) [(range) (apply range (cdr r))] [(rank) (apply rank (cdr r))]
             [else (error 'fetch "unknown row operation" (car r))])) requests)))
