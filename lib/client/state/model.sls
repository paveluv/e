;; Shared head mirrors. Subscription owns demand; painting reads only copies.
;; One background batch and one pending invalidation set serve every mount.
(import (only (foundation edoc) elibrary))
(elibrary (state model)
  (export available? commit! create! ids metadata reference? retire! snapshot snapshots subscribe! unsubscribe!)
  (import (chezscheme)
          (prefix (core client) client:)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:))
  (define mirrors (make-eqv-hashtable)) ; n -> (watermark available? envelope)
  (define dirty (make-eqv-hashtable)) ; n -> generation, or #f for a rescan
  (define active? #f)
  (define subscribers (kernel:make-registry car))

  (edoc "Whether a value is a tagged model reference; this checks its spelling, not availability."
        (value any "candidate identity") (returns boolean))
  (define (reference? value)
    (and (list? value) (= (length value) 2) (eq? (car value) 'model)
      (integer? (cadr value)) (exact? (cadr value)) (> (cadr value) 0)))

  (edoc-type model "a model reference, spelled (model number); operations validate existence and kind"
    (predicate reference?)
    (portable #t) (within list)
    (complete (lambda (partial) (map (lambda (row) (cons (car row) (symbol->string (cadr row)))) (metadata))))
    (write (lambda (id) (format "(model ~a)" (cadr id)))))
  (define (number id)
    (unless (reference? id)
      (error 'model "expected (model positive-integer)" id))
    (cadr id))
  (define (mirror id)
    (or (hashtable-ref mirrors (number id) #f) (error 'model "subscribe before reading a mirror" id)))
  (define (adopt! packet)
    (let ([generation (car packet)])
      (for-each
        (lambda (row)
          (let* ([n (number (car row))] [old (hashtable-ref mirrors n #f)])
            (when (and old (>= generation (car old)))
              (hashtable-set! mirrors n (cons generation (cdr row)))
              (let ([pending (hashtable-ref dirty n #f)])
                (when (and pending (<= pending generation)) (hashtable-delete! dirty n))))))
        (cadr packet))))
  (define (notify! packet)
    (for-each
      (lambda (entry)
        (let ([rows (filter (lambda (row) (member (car row) (cadr entry))) (cadr packet))])
          (unless (null? rows)
            (when (kernel:registry-find subscribers (lambda (current) (eq? current entry)))
              (guard (ex [else (void)]) ((caddr entry) (list (car packet) (map car rows))))))))
      (kernel:registry-items subscribers)))

  (define (receive! packet) (adopt! packet) (notify! packet))

  (edoc "Read a coherent authoritative batch and refresh any subscribed mirrors. This explicit synchronization barrier may perform I/O; painting uses snapshot."
        (ids list "tagged IDs") (returns list) (effects remote))
  (define (snapshots ids)
    (let ([packet (client:request 'model-read ids)]) (receive! packet) packet))
  (define (refresh!)
    (unless (or active? (zero? (hashtable-size dirty)))
      (let ([ids (map (lambda (n) (list 'model n)) (vector->list (hashtable-keys dirty)))])
        (hashtable-clear! dirty)
        (set! active? #t)
        (fork-thread
          (lambda ()
            (guard (ex [else (client:close! (kernel:condition-text ex))])
              (let ([packet (client:request 'model-read ids)])
                (client:enqueue!
                  (lambda ()
                    (set! active? #f) (receive! packet) (refresh!))))))))))
  (define invalidations
    (kernel:call-with-runtime-registrations
      (lambda ()
        (client:subscribe! 'models
          (lambda (batch)
            (if batch
                (for-each (lambda (change)
                            (let ([old (hashtable-ref mirrors (car change) #f)])
                              (when (and old (> (cdr change) (car old)))
                                (hashtable-set! dirty (car change) (cdr change))))) batch)
                (vector-for-each (lambda (n) (hashtable-set! dirty n #f)) (hashtable-keys mirrors)))
            (refresh!))))))
  (define cleanup
    (kernel:registry-observe! subscribers
      (lambda (removed added)
        (let* ([kept (apply append (map cadr (kernel:registry-items subscribers)))]
               [fresh (fold-left (lambda (out id) (let ([n (number id)])
                                                    (if (or (hashtable-contains? mirrors n) (memv n out)) out (cons n out)))) '() kept)]
               [dropped (filter (lambda (n) (not (member (list 'model n) kept)))
                          (vector->list (hashtable-keys mirrors)))])
          (for-each (lambda (n) (hashtable-delete! mirrors n) (hashtable-delete! dirty n)) dropped)
          (guard (ex [else (client:close! (kernel:condition-text ex))])
            (unless (null? dropped) (client:request 'model-unwatch (map (lambda (n) (list 'model n)) dropped)))
            (unless (null? fresh)
              (let ([packet (client:request 'model-watch (map (lambda (n) (list 'model n)) fresh))])
                (for-each (lambda (n) (hashtable-set! mirrors n '(-1 #f #f))) fresh)
                (adopt! packet))))))))

  (edoc "Subscribe to model invalidations and seed shared mirrors; subsequent refreshes run in the background and notify on the client pump. Call on the pump thread."
        (ids list "tagged model ids") (procedure procedure "(procedure (generation ids)) after mirror adoption")
        (returns any))
  (define (subscribe! ids procedure)
    (unless (and (list? ids) (procedure? procedure)) (error 'subscribe! "expected ids and a procedure"))
    (let* ([ids (datum:copy ids)] [token (gensym "model-subscription")])
      (for-each number ids)
      (kernel:registry-add! subscribers (list token ids procedure)) token))

  (edoc "Retract an owned subscription; the last reader releases its mirror and remote interest."
        (token any "the subscription"))
  (define (unsubscribe! token)
    (kernel:registry-remove! subscribers (lambda (entry) (eq? (car entry) token))))

  (edoc "An owned subscribed snapshot, read locally without a request."
        (id model "the model") (returns (or list #f)))
  (define (snapshot id) (datum:copy (caddr (mirror id))))

  (edoc "Whether the subscribed model's mirrored kind accepts its payload, without a request."
        (id model "the tagged model id") (returns boolean))
  (define (available? id) (cadr (mirror id)))

  (edoc "Query the base for live model ids, optionally restricted to one kind without reading payloads."
        (kinds (list-of symbol) "at most one kind")
        (returns list) (effects remote) (public))
  (define (ids . kinds) (apply client:request 'model-ids kinds))

  (edoc "Query compact (reference kind) metadata for live models in allocation order, without requesting payloads. Retired models are absent; unknown kinds remain inspectable."
        (returns list) (effects remote))
  (define (metadata) (client:request 'model-metadata))

  (edoc "Create base-owned non-authored model state, attributed to this connection."
        (actor actor "attribution is supplied by the connection") (kind symbol "the registered kind") (schema integer "its version")
        (scope datum "the ownership scope") (persistence (one-of transient persistent) "restart policy")
        (references list "resource references") (value datum "initial value") (returns model) (public))
  (define (create! actor kind schema scope persistence references value)
    (client:request 'model-create kind schema scope persistence references value))

  (edoc "Commit a guarded model batch at the base; this connection supplies attribution."
        (actor actor "attribution is supplied by the connection") (changes list "(id revision references value) entries") (public))
  (define (commit! actor changes) (apply values (client:request 'model-commit changes)))

  (edoc "Retire non-authored model state at its revision; this connection supplies attribution."
        (actor actor "attribution is supplied by the connection") (id model "the tagged model id") (revision integer "expected revision") (public))
  (define (retire! actor id revision) (apply values (client:request 'model-retire id revision)))
)
