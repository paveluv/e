;; Owned state bindings. A shared topology witness closes cross-owner races;
;; payloads and predicates are inspected before entering the model writer.
(import (only (foundation edoc) elibrary))
(elibrary (state connection)
  (export bind! bindings fork! read snapshot subscribe! topology unsubscribe!)
  (import (except (chezscheme) read) (prefix (core descriptor) descriptor:)
          (prefix (core kernel) kernel:) (prefix (core port) port:)
          (prefix (foundation datum) datum:) (prefix (foundation edoc) edoc:)
          (prefix (state model) model:) (prefix (state store) store:))
  (define (field r k) (cdr (assq k r)))
  (define (id? x) (and (list? x) (= (length x) 2) (eq? (car x) 'model)
                    (integer? (cadr x)) (exact? (cadr x)) (> (cadr x) 0)))
  (define (buffer? id)
    (and (list? id) (= (length id) 2) (eq? (car id) 'buffer)
      (integer? (cadr id)) (exact? (cadr id)) (> (cadr id) 0)))
  (define (endpoint id)
    (if (buffer? id)
      (and (store:exists? id) (list (cons 'id id)))
      (model:snapshot id)))
  (define (producer? p) (and (list? p) (= (length p) 2) (or (id? (car p)) (buffer? (car p))) (symbol? (cadr p))))
  (define (edges? xs)
    (and (list? xs) (for-all (lambda (e) (and (list? e) (= (length e) 3)
                                           (id? (car e)) (symbol? (cadr e)) (producer? (caddr e)))) xs)
      (= (length xs) (length (unique (map (lambda (e) (list (car e) (cadr e))) xs))))))
  (define (unique xs) (fold-left (lambda (out x) (if (member x out) out (cons x out))) '() xs))
  (define registrations
    (kernel:call-with-runtime-registrations
      (lambda ()
        (model:register-kind! 'connection-topology 1
          (lambda (v) (and (list? v) (or (= (length v) 2) (and (= (length v) 3) (list? (caddr v)))) (integer? (car v)) (>= (car v) 0)
                        (list? (cadr v)) (for-all (lambda (p) (and (pair? p) (id? (car p)) (id? (cdr p)))) (cadr v))
                        (= (length (cadr v)) (length (unique (map car (cadr v))))))))
        (model:register-kind! 'connection-bindings 1 edges?))))
  (define initialization-lock (make-mutex))
  (define (change r refs value) (list (field r 'id) (field r 'revision) refs value))
  (define (witness r) (change r (field r 'references) (field r 'value)))
  (define (owners r) (cadr (field r 'value)))
  (define (advance r owners) (change r (map cdr owners) (list (+ 1 (car (field r 'value))) owners (caddr (field r 'value)))))
  (define catalogue (port:catalogue))
  (define (synchronize-contracts! id)
    (let loop ()
      (let ([r (model:snapshot id)] [ds catalogue])
        ;; Earlier v3 recipes lack the derived catalogue. Rebuild it without
        ;; changing their owner bindings or requiring a session reset.
        (unless (and (pair? (cddr (field r 'value))) (equal? (caddr (field r 'value)) ds))
          (let-values ([(status rows) (model:commit! '(base connection)
                                        (list (change r (field r 'references) (list (+ 1 (car (field r 'value))) (owners r) ds))))])
            (unless (eq? status 'applied) (loop)))))))
  (define contracts
    (kernel:call-with-runtime-registrations
      (lambda () (port:observe! (lambda ()
                                  (set! catalogue (port:catalogue))
                                  (for-each synchronize-contracts! (model:ids 'connection-topology)))))))

  (edoc "Locate the canonical connection topology, initializing it lazily after recovery."
        (returns list) (effects internal))
  (define (topology)
    (let ([id (with-mutex initialization-lock
                (let ([ids (model:ids 'connection-topology)])
                  (cond [(null? ids) (model:create! '(base connection) 'connection-topology 1 'session 'persistent '() (list 0 '() catalogue))]
                    [(null? (cdr ids)) (car ids)] [else (error 'topology "multiple topology records")])) )])
      (synchronize-contracts! id) id))
  (define (capture)
    (let* ([top (model:snapshot (topology))]
           [records (filter values (map (lambda (p) (model:snapshot (cdr p))) (owners top)))])
      (values top records
        (apply append (map (lambda (r) (map (lambda (e) (cons (field r 'scope) e)) (field r 'value))) records)))))
  (define (port r name direction)
    (let ([ds (and r (port:describe (port:key r)))])
      (and ds (find (lambda (d) (and (eq? name (cadr d)) (eq? direction (car d)))) ds))))
  (define (view? r) (and r (assq 'kind r) (eq? (field r 'kind) 'widget-view) (descriptor:valid? (field r 'value))))
  (define (belongs? get owner id)
    (let loop ([id id] [seen '()])
      (and (not (member id seen))
        (or (equal? id owner)
          (let* ([r (get id)] [parent (and r (if (view? r) (descriptor:parent (field r 'value)) (field r 'scope)))])
            (and (id? parent) (loop parent (cons id seen))))))))
  (define (acyclic? get edges)
    (let ([finished '()])
      (define (visit id path)
        (cond [(member id path) #f] [(member id finished) #t]
          [else
           (let* ([r (get id)] [ds (and r (port:describe (port:key r)))]
                  [connected (filter (lambda (e) (equal? id (cadr e))) edges)]
                  [default (if ds
                             (filter values
                               (map (lambda (d)
                                      (and (eq? (car d) 'input)
                                        (not (exists (lambda (e) (eq? (caddr e) (cadr d))) connected))
                                        (let ([p (port:project r (cadr d) #f)])
                                          (and (eq? (car p) 'ready) (id? (cadr p)) (cadr p))))) ds)) '())])
             (and (for-all (lambda (dependency) (visit dependency (cons id path)))
                    (append (map (lambda (e) (car (cadddr e))) connected) default))
               (begin (set! finished (cons id finished)) #t)))]))
      (for-all (lambda (e) (visit (cadr e) '())) edges)))

  (edoc "Atomically bind or disconnect inputs: (consumer input expected-producer replacement-producer). Return status and this owner's bindings; an already satisfied change is idempotent. Retry internal revision races while preserving the caller's producer and interaction guards."
        (actor actor "caller") (owner list "containing view or model") (changes list "guarded input changes")
        (leases (list-of list) "optional (consumer generation sequence) guards after head publication"))
  (define (bind! actor owner changes . leases)
    (unless (and (<= (length leases) 1)
              (or (null? leases) (for-all (lambda (l) (and (list? l) (= (length l) 3) (id? (car l)))) (car leases))))
      (error 'bind! "invalid interaction leases" leases))
    (let retry ([changes (datum:copy changes)])
      (unless (and (id? owner) (list? changes)
                (for-all (lambda (c) (and (list? c) (= (length c) 4) (id? (car c)) (symbol? (cadr c))
                                       (or (not (caddr c)) (producer? (caddr c)))
                                       (or (not (cadddr c)) (producer? (cadddr c))))) changes)
                (= (length changes) (length (unique (map (lambda (c) (list (car c) (cadr c))) changes)))))
        (error 'bind! "expected distinct guarded inputs"))
      (let ([result
             (call/cc
               (lambda (fail)
                 (let-values ([(top records edges) (capture)])
                   (let ([seen (make-hashtable equal-hash equal?)] [next edges])
                     (define (get id)
                       (unless (hashtable-contains? seen id) (hashtable-set! seen id (endpoint id)))
                       (hashtable-ref seen id #f))
                     (define (need id)
                       (let ([r (get id)])
                         (unless (and r (or (buffer? id) (model:available? id))) (fail 'unavailable)) r))
                     (define (owned r)
                       (when (and (view? r) (descriptor:owner (field r 'value))
                                  (not (equal? actor (descriptor:owner (field r 'value))))) (fail 'owned)))
                     (owned (need owner))
                     (for-each
                       (lambda (c)
                         (let* ([consumer (need (car c))] [input (port consumer (cadr c) 'input)]
                                [old (find (lambda (e) (and (equal? (cadr e) (car c)) (eq? (caddr e) (cadr c)))) next)]
                                [current (and old (cadddr old))] [replacement (cadddr c)])
                           (owned consumer)
                           (when (and (view? consumer) (descriptor:owner (field consumer 'value)))
                             (let* ([d (field consumer 'value)] [lease (and (pair? leases) (assoc (car c) (car leases)))])
                               (unless (and lease (equal? (cdr lease) (list (descriptor:generation d) (descriptor:sequence d))))
                                 (fail 'owned))))
                           (unless (belongs? get owner (car c)) (fail 'scope))
                           (when (and old (not (equal? owner (car old)))) (fail 'occupied))
                           (unless (or (equal? current replacement) (equal? current (caddr c))) (fail 'stale))
                           (when replacement
                             (let ([output (port (need (car replacement)) (cadr replacement) 'output)])
                               (unless (and input output (edoc:type-compatible? (caddr output) (caddr input))) (fail 'incompatible))))
                           (unless (equal? current replacement)
                             (set! next (if old (remq old next) next))
                             (when replacement (set! next (cons (list owner (car c) (cadr c) replacement) next)))))) changes)
                     (unless (acyclic? get next) (fail 'cycle))
                     (let* ([old (find (lambda (r) (equal? owner (field r 'scope))) records)]
                            [mine (map cdr (filter (lambda (e) (equal? owner (car e))) next))]
                            [refs (unique (cons owner (apply append (map (lambda (e) (list (car e) (caaddr e))) mine))))]
                            [witnesses
                             (map (lambda (r)
                                    (let* ([id (field r 'id)] [d (and (view? r) (field r 'value))]
                                           [changed? (and d
                                                          (not (equal? (filter (lambda (e) (equal? (cadr e) id)) edges)
                                                                 (filter (lambda (e) (equal? (cadr e) id)) next))))])
                                      (if changed?
                                          (change r (field r 'references)
                                            (descriptor:with d (list (cons 'generation (+ 1 (descriptor:generation d))) '(sequence . 0))))
                                          (witness r))))
                                  (filter (lambda (r) (and r (id? (field r 'id)))) (vector->list (hashtable-values seen))))]
                            [witnesses (append witnesses (map witness (if old (remq old records) records)))])
                       (cond
                         [(and old (equal? (field old 'value) mine)) 'applied]
                         [old (let-values ([(status ignored) (model:commit! actor
                                                               (cons (advance top (owners top))
                                                                 (cons (change old refs mine) witnesses)))]) (if (eq? status 'stale) 'retry status))]
                         [(null? mine) 'applied]
                         [else
                          (if (model:allocate! actor 1
                                (lambda (ids) (list (list 'connection-bindings 1 owner (field (get owner) 'persistence) refs mine)))
                                (lambda (ids) (cons (advance top (cons (cons owner (car ids)) (owners top))) witnesses)))
                              'applied 'retry)]))))))])
        ;; Buffer and model writers are independent. Reconcile a producer
        ;; deleted during binding; later deletions use the store observer.
        (clean!)
        (if (eq? result 'retry) (retry changes) (values result (bindings owner))))))

  (edoc "Read this owner's current (consumer input producer) bindings without resolving values."
        (owner list "composition owner") (returns list) (effects internal))
  (define (bindings owner)
    (let-values ([(top records edges) (capture)])
      (map cdr (filter (lambda (e) (equal? owner (car e))) edges))))

  (define (clean!)
    (unless (null? (model:ids 'connection-topology))
      (let-values ([(top records edges) (capture)])
        (let* ([kept (filter (lambda (r) (model:snapshot (field r 'scope))) records)]
               [owners (map (lambda (r) (cons (field r 'scope) (field r 'id))) kept)]
               [changes
                (filter values
                  (map (lambda (r)
                         (let* ([es (field r 'value)]
                                [next (filter (lambda (e) (and (model:snapshot (car e)) (endpoint (caaddr e))
                                                            (belongs? model:snapshot (field r 'scope) (car e)))) es)])
                           (and (not (equal? es next))
                             (change r (unique (cons (field r 'scope)
                                                 (apply append (map (lambda (e) (list (car e) (caaddr e))) next)))) next)))) kept))])
          (unless (and (null? changes) (equal? owners (cadr (field top 'value))))
            (let-values ([(status ignored) (model:commit! '(base connection) (cons (advance top owners) changes))])
              (when (eq? status 'applied)
                (for-each (lambda (r) (unless (memq r kept)
                                        (model:retire! '(base connection) (field r 'id) (field r 'revision)))) records))))))))
  (define parents (make-hashtable equal-hash equal?))
  (define buffer-retirement
    (store:subscribe! #f (lambda (event) (when (eq? (car event) 'delete) (clean!)))))
  (define retirement
    (model:subscribe! #f
      (lambda (notice)
        (when (or (not (cadr notice))
                (fold-left
                  (lambda (changed? id)
                    (let* ([r (model:snapshot id)] [d (and (view? r) (field r 'value))]
                           [parent (and d (descriptor:parent d))] [old (hashtable-ref parents id #f)])
                      (if d (hashtable-set! parents id parent) (hashtable-delete! parents id))
                      (or changed? (not r) (not (equal? old parent))))) #f (cadr notice)))
          (clean!)))))

  (edoc "Capture a dependency bundle: (graph-basis edges endpoint-rows text-ids). Model rows are coherent snapshots; buffer rows are contract headers, with text and provisional state supplied by the host."
        (ids list "endpoints") (returns list) (effects internal))
  (define (snapshot ids)
    (let loop ([attempt 0])
      (let-values ([(top records edges) (capture)])
        (let ([seen (make-hashtable equal-hash equal?)])
          (define (get id)
            (unless (hashtable-contains? seen id)
              (hashtable-set! seen id (car (cadr (model:snapshots (list id))))))
            (let ([r (hashtable-ref seen id #f)]) (and (cadr r) (caddr r))))
          (let* ([closure (port:dependencies ids edges get)]
                 [rows (map (lambda (id) (hashtable-ref seen id #f)) (car closure))]
                 [all (append (map (lambda (r) (list (field r 'id) #t r)) (cons top records)) rows)]
                 [now (cadr (model:snapshots (map car all)))])
            (cond [(equal? all now)
                   (list (list (field top 'id) (field top 'revision)) edges
                     (append rows (map (lambda (id) (let ([r (endpoint id)]) (list id (and r #t) r))) (cadr closure))) (cadr closure))]
              [(< attempt 2) (loop (+ attempt 1))]
              [else (list #f edges (map (lambda (id) (list id #f #f)) (car closure)) (cadr closure))]))))))

  (edoc "Resolve a current port with its coherent graph, model and text basis. A busy source yields pending."
        (id list "endpoint") (name symbol "input or output") (returns list) (effects internal))
  (define (read id name)
    (let loop ([attempt 0])
      (if (= attempt 3) '(pending changing-basis ())
        (let-values ([(top records edges) (capture)])
          (let ([seen (make-hashtable equal-hash equal?)] [texts (make-hashtable equal-hash equal?)])
            (define (get id)
              (if (buffer? id) (endpoint id)
                (begin
                  (unless (hashtable-contains? seen id)
                    (hashtable-set! seen id (car (cadr (model:snapshots (list id))))))
                  (let ([row (hashtable-ref seen id #f)]) (and (cadr row) (caddr row))))))
            (define (text id)
              (unless (hashtable-contains? texts id)
                (hashtable-set! texts id
                  (guard (ex [else #f])
                    (let-values ([(lines revision) (store:snapshot id)])
                      (list (cons 'id id) (cons 'revision revision) (cons 'value lines))))))
              (hashtable-ref texts id #f))
            (let* ([result (port:resolve id name edges get text)]
                   [models (append (list top) records (filter values (map caddr (vector->list (hashtable-values seen)))))]
                   [packet (model:snapshots (map (lambda (r) (field r 'id)) models))])
              (if (and (for-all (lambda (r row) (equal? r (caddr row))) models (cadr packet))
                    (for-all (lambda (p) (let ([r (hashtable-ref texts p #f)])
                                           (guard (ex [else #f])
                                             (if r (= (field r 'revision) (store:revision p)) (not (store:exists? p))))))
                      (vector->list (hashtable-keys texts))))
                (list (car result) (cadr result)
                  (cons (list (field top 'id) (field top 'revision)) (caddr result)))
                (loop (+ attempt 1)))))))))

  (edoc "Observe dependency invalidations through the existing model and text deliveries. Subscribe before reading."
        (ids list "endpoint IDs") (procedure procedure "zero-argument invalidation") (returns list) (public))
  (define (subscribe! ids procedure)
    (list (model:subscribe! #f (lambda (event) (procedure)))
      (store:subscribe! #f (lambda (event) (procedure))) (port:observe! procedure)))

  (edoc "Release a connection observation." (token list "subscription") (public))
  (define (unsubscribe! token)
    (model:unsubscribe! (car token)) (store:unsubscribe! (cadr token)) (port:unobserve! (caddr token)))

  (edoc "Allocate a composition fork and its internal bindings in one model transaction; sources outside the fork remain borrowed."
        (actor actor "creator") (originals list "view and owned resource envelopes")
        (build procedure "new model IDs -> allocation specifications")
        (guards list "resource-owner envelopes to witness") (aliases list "prepared old-to-new resource references")
        (update (list-of procedure) "optional pure copied IDs -> guarded neighbor changes") (returns list))
  (define (fork! actor originals build guards aliases . update)
    (define (updates ids) (if (null? update) '() ((car update) ids)))
    (define witnesses
      (fold-left (lambda (out r) (if (exists (lambda (old) (equal? (field r 'id) (field old 'id))) out) out (cons r out)))
        originals guards))
    (define (with-witnesses changes)
      (fold-left (lambda (out r)
                   (let ([old (assoc (field r 'id) out)])
                     (when (and old (not (= (cadr old) (field r 'revision))))
                       (error 'fork! "resource owner changed during fork; retry"))
                     (if old out (cons (witness r) out)))) changes witnesses))
    (unless (and (<= (length update) 1) (for-all procedure? update)) (error 'fork! "expected one update builder"))
    (if (null? (model:ids 'connection-topology))
      (or (model:allocate! actor (length originals) build (lambda (ids) (with-witnesses (updates ids))))
        (error 'fork! "composition changed during fork; retry"))
      (let-values ([(top records edges) (capture)])
        (let* ([old-ids (map (lambda (r) (field r 'id)) originals)]
               [owned (filter (lambda (r) (member (field r 'scope) old-ids)) records)]
               [n (length old-ids)] [count (+ n (length owned))])
          (define (mapping ids) (append (map cons old-ids (list-head ids n)) aliases))
          (define (mapped ids id) (cond [(assoc id (mapping ids)) => cdr] [else id]))
          (or (model:allocate! actor count
                (lambda (ids)
                  (let* ([specs (build (list-head ids n))]
                         [allocated (map (lambda (r spec) (cons (field r 'id) spec)) originals specs)])
                    (append specs
                      (map (lambda (r)
                             (let* ([owner (mapped ids (field r 'scope))]
                                    [es (map (lambda (e) (list (mapped ids (car e)) (cadr e)
                                                           (list (mapped ids (caaddr e)) (cadr (caddr e)))))
                                          (filter (lambda (e) (member (car e) old-ids)) (field r 'value)))])
                               (list 'connection-bindings 1 owner (cadddr (cdr (assoc (field r 'scope) allocated)))
                                 (unique (cons owner (apply append (map (lambda (e) (list (car e) (caaddr e))) es)))) es))) owned))))
                (lambda (ids)
                  (with-witnesses (append (updates (list-head ids n)) (map witness records)
                                    (list (if (null? owned) (witness top)
                                            (advance top (append (map (lambda (r id) (cons (mapped ids (field r 'scope)) id)) owned (list-tail ids n))
                                                           (owners top)))))))))
            (error 'fork! "composition changed during fork; retry")))))))
