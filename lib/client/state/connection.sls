;; Connection demand shares the model mirror reader, never a worker per edge.
(import (only (foundation edoc) elibrary))
(elibrary (state connection)
  (export bind! bindings read snapshot subscribe! unsubscribe!)
  (import (except (chezscheme) read) (prefix (core client) client:)
          (prefix (core kernel) kernel:) (prefix (core port) port:)
          (prefix (foundation datum) datum:) (prefix (state model) model:))
  (define readers (kernel:make-registry car))
  (define top #f)
  (define token #f)
  (define demanded '())
  (define refreshing? #f)
  (define (field r k) (cdr (assq k r)))
  (define (unique xs) (fold-left (lambda (out x) (if (member x out) out (cons x out))) '() xs))
  (define (buffer? id) (and (pair? id) (eq? (car id) 'buffer)))
  (define (raw id)
    (if (buffer? id) (list (cons 'id id))
      (and (member id demanded) (model:available? id) (model:snapshot id))))
  (define (get id)
    (let* ([r (raw id)] [t (and top (raw top))] [k (and r (port:key r))]
           [declared (and t (assoc k (caddr (field t 'value))))])
      (and r (equal? (and k (port:describe k)) (and declared (cdr declared))) r)))
  (define (edges)
    (let ([r (and top (get top))])
      (if r
        (apply append
          (map (lambda (p)
                 (let ([r (get (cdr p))])
                   (if r (map (lambda (e) (cons (field r 'scope) e)) (field r 'value)) '())))
            (cadr (field r 'value)))) '())))
  (define (notify!)
    (for-each (lambda (r) (when (kernel:registry-find readers (lambda (current) (eq? r current))) ((caddr r))))
      (kernel:registry-items readers)))
  (define (refresh!)
    (unless refreshing?
      (dynamic-wind (lambda () (set! refreshing? #t))
        (lambda ()
          (let ([endpoints (unique (apply append (map cadr (kernel:registry-items readers))))])
            (if (null? endpoints)
              (begin (when token (model:unsubscribe! token)) (set! token #f) (set! demanded '()))
              (begin
                (unless top (set! top (client:request 'connection-topology)))
                (let loop ()
                  (let* ([r (get top)] [records (if r (map cdr (cadr (field r 'value))) '())]
                         [closure (port:dependencies endpoints (edges) get)]
                         [wanted (unique (append (list top) records (car closure)))])
                    (unless (and (= (length wanted) (length demanded)) (for-all (lambda (id) (member id demanded)) wanted))
                      (let ([old token]
                            [fresh (model:subscribe! wanted (lambda (notice) (refresh!) (notify!)))])
                        (set! demanded wanted) (set! token fresh)
                        (when old (model:unsubscribe! old)))
                      (loop))))))))
        (lambda () (set! refreshing? #f)))))
  ;; Registry observers run in a non-reentrant delivery queue. Acquiring a
  ;; model subscription there cannot synchronously seed its nested observer.
  ;; Explicit admission refreshes after publication; retraction/reload queues
  ;; work on the existing client pump, outside that delivery queue.
  (define queued? #f)
  (define (defer-refresh!)
    (unless queued?
      (set! queued? #t)
      (client:enqueue! (lambda () (set! queued? #f) (refresh!) (notify!)))))
  (define changes
    (kernel:call-with-runtime-registrations
      (lambda () (kernel:registry-observe! readers (lambda (removed added) (defer-refresh!))))))
  (define contracts
    (kernel:call-with-runtime-registrations (lambda () (port:observe! defer-refresh!))))
  ;; Buffer inputs are dependencies too. Refresh only readers whose acquired
  ;; text changed; the old window host must not be needed to update a mirror.
  (define texts
    (kernel:call-with-runtime-registrations
      (lambda ()
        (client:subscribe! 'changed
          (lambda (changes)
            ;; The store cache must see this invalidation before a reader
            ;; reacquires text. Subscriber registration order is not authority.
            (client:enqueue!
              (lambda ()
                (for-each
                  (lambda (reader)
                    (let ([ids (cadr (port:dependencies (cadr reader) (edges) get))])
                      (when (and (kernel:registry-find readers (lambda (current) (eq? reader current)))
                              (pair? ids) (or (not changes) (exists (lambda (id) (assoc id changes)) ids)))
                        ((caddr reader)))))
                  (kernel:registry-items readers)))))))))

  (edoc "Acquire endpoint dependency mirrors before rendering; callbacks run after adoption on the existing client pump."
        (ids list "endpoints") (procedure procedure "zero-argument invalidation") (returns any))
  (define (subscribe! ids procedure)
    (unless (and (list? ids) (procedure? procedure)) (error 'subscribe! "expected endpoints and callback"))
    (let ([token (gensym "connection")])
      (kernel:registry-add! readers (list token (datum:copy ids) procedure)) (refresh!) token))

  (edoc "Release endpoint demand; shared dependencies remain while another reader needs them."
        (token any "subscription"))
  (define (unsubscribe! token)
    (kernel:registry-remove! readers (lambda (r) (eq? token (car r)))) (refresh!))

  (edoc "Read an acquired local dependency bundle: (graph-basis edges endpoint-rows text-ids). Buffer rows are contract headers. No I/O; the host supplies mirrored text and provisional descriptors."
        (ids list "subscribed endpoints") (returns list))
  (define (snapshot ids)
    (unless (for-all (lambda (id) (or (member id demanded)
                                    (and (buffer? id)
                                      (exists (lambda (r) (member id (cadr r))) (kernel:registry-items readers))))) ids)
      (error 'snapshot "subscribe before reading connections" ids))
    (let* ([es (edges)] [closure (port:dependencies ids es get)] [r (get top)])
      (list (and r (list top (field r 'revision))) es
        (map (lambda (id) (let ([r (get id)]) (list id (and r #t) (if (buffer? id) r (model:snapshot id)))))
          (append (car closure) (cadr closure))) (cadr closure))))

  (edoc "Resolve an acquired model port locally; mounted hosts use snapshot to supply provisional descriptors and mirrored text."
        (id list "endpoint") (name symbol "port") (returns list) (public))
  (define (read id name)
    (let ([bundle (snapshot (list id))])
      (if (car bundle) (port:resolve id name (cadr bundle) get (lambda (id) #f))
        '(pending changing-basis ()))))

  (edoc "Read an owner's binding declarations from the base."
        (owner list "composition") (returns list) (effects remote))
  (define (bindings owner) (client:request 'connection-bindings owner))

  (edoc "Guardedly bind or disconnect a batch of inputs at the base. Active composition callers fence interaction before rewiring."
        (actor actor "connection supplies attribution") (owner list "composition") (changes list "guarded changes")
        (leases (list-of list) "optional active-consumer generation/sequence guards supplied by interaction:bind!"))
  (define (bind! actor owner changes . leases)
    (let ([reply (apply client:request 'connection-bind owner changes leases)])
      (unless (null? demanded) (model:snapshots demanded))
      (refresh!) (notify!) (apply values reply))))
