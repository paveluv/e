;; model.sls -- canonical non-text state, alongside the text store.
;; Values are immutable by ownership. Validation runs outside the writer;
;; a batch installs only against the records and definitions it inspected.
(import (only (foundation edoc) elibrary))
(elibrary (state model)
  (export available? commit! create! export ids import! register-kind! retire! snapshot snapshots subscribe! unsubscribe! valid-import?)
  (import (rnrs)
          (only (chezscheme) unbox make-mutex with-mutex void gensym)
          (prefix (core identity) identity:)
          (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:)
          (prefix (sys activity) activity:))

  (define-record-type definition
    (nongenerative e-model-definition-v1)
    (fields key accepts?))
  (define-record-type state
    (nongenerative e-model-state-v2)
    (fields lock kinds (mutable records) (mutable next-id) subscriptions deliveries (mutable generation)))
  (define-record-type subscription
    (nongenerative e-model-subscription-v1)
    (fields token ids procedure (mutable pending)))
  (define data
    (unbox (kernel:persistent-cell 'model
             (lambda () (make-state (make-mutex) (kernel:make-registry definition-key)
                                    (make-eqv-hashtable) 1 (kernel:make-registry subscription-token)
                                    (kernel:make-delivery-queue) 0)))))
  (define keys '(id kind schema scope persistence revision actor references value))
  (define (field entry key) (cdr (assq key entry)))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (positive-integer? n) (and (natural? n) (> n 0)))
  (define (tagged? value tag)
    (and (list? value) (= (length value) 2) (eq? (car value) tag) (positive-integer? (cadr value))))
  (define (scope? value)
    (or (eq? value 'session) (tagged? value 'model)
        (and (list? value) (= (length value) 2) (eq? (car value) 'head)
             (string? (cadr value)) (> (string-length (cadr value)) 0))))
  (define (references? value)
    (and (list? value)
         (for-all (lambda (id) (or (tagged? id 'model) (tagged? id 'buffer))) value)))
  (define (envelope? entry)
    (and (list? entry) (= (length entry) (length keys)) (for-all pair? entry)
         (equal? (map car entry) keys)
         (tagged? (field entry 'id) 'model) (symbol? (field entry 'kind))
         (positive-integer? (field entry 'schema)) (scope? (field entry 'scope))
         (memq (field entry 'persistence) '(transient persistent))
         (natural? (field entry 'revision)) (identity:valid? (field entry 'actor))
         (references? (field entry 'references))))
  (define (definition-of entry)
    (kernel:registry-find (state-kinds data)
      (lambda (definition)
        (equal? (definition-key definition) (list (field entry 'kind) (field entry 'schema))))))
  (define (accepts? definition entry)
    ;; A predicate cannot retain or mutate stored data, even by accident.
    ;; Failure means unavailable; it must not make opaque recovery unsavable.
    (and definition
         (guard (ex [else #f])
           (and ((definition-accepts? definition) (datum:copy (field entry 'value))) #t))))
  (define (require-id id)
    (unless (tagged? id 'model) (error 'model "expected (model positive-integer)" id))
    (cadr id))
  (define (record-of id) (hashtable-ref (state-records data) (require-id id) #f))
  (define (read-records ids)
    (with-mutex (state-lock data) (map record-of ids)))
  (define (own-actor actor)
    (let ([actor (datum:copy actor)])
      (unless (identity:valid? actor) (error 'model "expected an actor identity" actor))
      actor))

  (define (mutate! thunk)
    (activity:call-with
      (lambda ()
        (call-with-values thunk
          (lambda result
            (kernel:drain-deliveries! (state-deliveries data))
            (apply values result))))))

  (define (changed! ids)
    ;; Under the writer. One queued wake per subscriber, with at most 256
    ;; ids; #f requests a rescan. Definitions may change without revisions.
    (state-generation-set! data (+ 1 (state-generation data)))
    (for-each
      (lambda (subscriber)
        (let ([selected (and ids (filter (lambda (id) (or (not (subscription-ids subscriber))
                                                          (member id (subscription-ids subscriber)))) ids))])
          (unless (equal? selected '())
            (let* ([old (subscription-pending subscriber)]
                   [merged (and selected (or (not old) (cadr old))
                                (fold-left (lambda (out id) (if (member id out) out (cons id out)))
                                  selected (if old (cadr old) '())))])
              (subscription-pending-set! subscriber
                (list (state-generation data) (and merged (<= (length merged) 256) merged)))
              (unless old
                (kernel:enqueue-delivery! (state-deliveries data)
                  (lambda ()
                    (let ([event (with-mutex (state-lock data)
                                   (let ([event (subscription-pending subscriber)])
                                     (subscription-pending-set! subscriber #f) event))])
                      (when (kernel:registry-find (state-subscriptions data) (lambda (entry) (eq? entry subscriber)))
                        ((subscription-procedure subscriber) (datum:copy event)))))))))))
      (kernel:call-with-runtime-registrations (lambda () (kernel:registry-items (state-subscriptions data))))))

  (define definition-observer
    (kernel:registry-observe! (state-kinds data)
      (lambda (removed added)
        (mutate! (lambda () (with-mutex (state-lock data) (changed! #f)))))))

  (edoc "Subscribe to model invalidations: (procedure (generation ids-or-#f)); #f means rescan. Subscribe before reading snapshots; their watermark covers any earlier notices."
        (ids (or list #f) "tagged model ids, or #f for all") (procedure procedure "the bounded invalidation callback")
        (returns any))
  (define (subscribe! ids procedure)
    (unless (and (or (not ids) (and (list? ids) (for-all (lambda (id) (tagged? id 'model)) ids))) (procedure? procedure))
      (error 'subscribe! "expected model ids or #f and a procedure" ids procedure))
    (let ([token (gensym "model-subscription")])
      (kernel:registry-add! (state-subscriptions data) (make-subscription token (datum:copy ids) procedure #f)) token))

  (edoc "Retract a model subscription, including queued callbacks."
        (token any "the subscription token"))
  (define (unsubscribe! token)
    (kernel:registry-remove! (state-subscriptions data) (lambda (entry) (eq? token (subscription-token entry)))))

  (edoc "A coherent batch: (generation ((id available? envelope-or-#f) ...)); predicates run outside the writer on captured values."
        (ids list "tagged model ids in result order") (returns list))
  (define (snapshots ids)
    (let ([ids (datum:copy ids)])
      (unless (list? ids) (error 'snapshots "expected model ids" ids))
      (for-each require-id ids)
      (let-values ([(generation entries definitions)
                    (with-mutex (state-lock data)
                      (let ([entries (map record-of ids)] [definitions (kernel:registry-items (state-kinds data))])
                        (values (state-generation data) entries definitions)))])
        (list generation
          (map (lambda (id entry)
                 (list id (and entry
                               (accepts? (find (lambda (definition)
                                                 (equal? (definition-key definition) (list (field entry 'kind) (field entry 'schema)))) definitions) entry))
                       (datum:copy entry))) ids entries)))))

  (edoc "Register a versioned kind of transient interaction or derived state; its pure payload predicate may use edoc types. Registration is module-owned; removing it leaves records intact."
        (kind symbol "the globally unique kind")
        (schema integer "the positive schema version")
        (accepts? procedure "pure (accepts? value), validating an owned portable payload"))
  (define (register-kind! kind schema accepts?)
    (unless (and (symbol? kind) (positive-integer? schema) (procedure? accepts?))
      (error 'register-kind! "expected kind, positive schema version and predicate" kind schema accepts?))
    (mutate!
      (lambda ()
        (kernel:registry-add! (state-kinds data) (make-definition (list kind schema) accepts?)))))

  (edoc "Create non-authored model state and return its tagged id; persistence explicitly chooses whether restart saves it. Authored data must use its domain's undo/trash operations."
        (actor actor "the author") (kind symbol "the registered kind") (schema integer "its version")
        (scope datum "session, (head name), or an owning (model id)")
        (persistence (one-of transient persistent) "base lifetime only, or restart recovery")
        (references list "tagged model/buffer references; missing targets remain explicit")
        (value datum "the initial payload")
        (returns list))
  (define (create! actor kind schema scope persistence references value)
    (mutate!
      (lambda ()
        (let ([entry (datum:copy (map cons keys
                                   (list '(model 1) kind schema scope persistence 0 (own-actor actor) references value)))])
          (unless (envelope? entry) (error 'create! "invalid model envelope" entry))
          (let ([definition (definition-of entry)])
            (unless (accepts? definition entry) (error 'create! "unknown kind/schema or invalid payload" kind schema))
            (with-mutex (state-lock data)
              (unless (eq? definition (definition-of entry)) (error 'create! "kind changed during validation" kind schema))
              (let* ([n (state-next-id data)] [id (list 'model n)]
                     [entry (cons (cons 'id id) (cdr entry))])
                (hashtable-set! (state-records data) n entry)
                (state-next-id-set! data (+ n 1))
                (changed! (list id))
                (datum:copy id))))))))

  (edoc "Live model ids in allocation order, optionally restricted to a kind without reading payloads."
        (kinds (list-of symbol) "at most one kind")
        (returns list))
  (define (ids . kinds)
    (unless (and (<= (length kinds) 1) (for-all symbol? kinds)) (error 'ids "expected at most one kind" kinds))
    (with-mutex (state-lock data)
      (map (lambda (n) (list 'model n))
        (list-sort < (filter (lambda (n) (or (null? kinds)
                                           (eq? (car kinds) (field (hashtable-ref (state-records data) n #f) 'kind))))
                       (vector->list (hashtable-keys (state-records data))))))))

  (edoc "An owned model envelope alist, or #f when absent; an unknown kind retains its complete portable payload."
        (id list "the tagged model id") (returns (or list #f)))
  (define (snapshot id)
    (datum:copy (car (read-records (list id)))))

  (edoc "Whether a live model's current kind definition accepts its saved payload."
        (id list "the tagged model id") (returns boolean))
  (define (available? id)
    (let ([entry (car (read-records (list id)))])
      (and entry (accepts? (definition-of entry) entry))))

  (define (own-changes changes)
    (let ([changes (datum:copy changes)] [seen (make-eqv-hashtable)])
      (unless (and (list? changes)
                   (for-all
                     (lambda (change)
                       (and (list? change) (= (length change) 4)
                            (tagged? (car change) 'model) (natural? (cadr change))
                            (references? (caddr change))
                            (let ([n (cadar change)])
                              (and (not (hashtable-contains? seen n))
                                   (begin (hashtable-set! seen n #t) #t))))) changes))
        (error 'commit! "expected unique (model-id revision references value) changes" changes))
      changes))
  (define (matches? entries changes)
    (for-all (lambda (entry change) (and entry (= (field entry 'revision) (cadr change)))) entries changes))
  (define (replace-state entry actor references value)
    (if (and (equal? references (field entry 'references)) (equal? value (field entry 'value))) entry
        (map (lambda (cell)
               (case (car cell)
                 [(revision) (cons 'revision (+ (cdr cell) 1))]
                 [(actor) (cons 'actor actor)]
                 [(references) (cons 'references references)]
                 [(value) (cons 'value value)]
                 [else cell])) entry)))

  (edoc "Atomically update transient model state against expected revisions; values are applied, stale or unavailable and owned current envelopes in request order. Equal changes do not advance revisions."
        (actor actor "the author")
        (changes list "(model-id expected-revision references value) entries"))
  (define (commit! actor changes)
    (mutate!
      (lambda ()
        (let* ([actor (own-actor actor)] [changes (own-changes changes)]
               [ids (map car changes)] [before (read-records ids)])
          (if (not (matches? before changes)) (values 'stale (datum:copy before))
              (let* ([definitions (map definition-of before)]
                     [after (map (lambda (entry change) (replace-state entry actor (caddr change) (cadddr change))) before changes)]
                     [available (for-all accepts? definitions before)])
                (when available
                  (unless (for-all accepts? definitions after) (error 'commit! "invalid model payload" changes)))
                (with-mutex (state-lock data)
                  (let ([current (map record-of ids)])
                    (cond
                      [(not (for-all eq? before current)) (values 'stale (datum:copy current))]
                      [(or (not available) (not (for-all (lambda (definition entry) (eq? definition (definition-of entry))) definitions before)))
                       (values 'unavailable (datum:copy current))]
                      [else
                       (for-each (lambda (id entry) (hashtable-set! (state-records data) (cadr id) entry)) ids after)
                       (let ([changed (filter values (map (lambda (id old new) (and (not (eq? old new)) id)) ids before after))])
                         (unless (null? changed) (changed! changed)))
                       (values 'applied (datum:copy after))])))))))))

  (edoc "Retire non-authored model state against its revision; values are applied with #f, or stale/unavailable with the current envelope. References are not cascaded into destructive operations."
        (actor actor "the author") (id list "the tagged model id") (revision integer "the expected revision"))
  (define (retire! actor id revision)
    (mutate!
      (lambda ()
        (own-actor actor)
        (unless (natural? revision) (error 'retire! "expected a nonnegative revision" revision))
        (let* ([id (datum:copy id)] [before (car (read-records (list id)))]
               [definition (and before (definition-of before))]
               [available (and before (accepts? definition before))])
          (with-mutex (state-lock data)
            (let ([current (record-of id)])
              (cond
                [(or (not current) (not (eq? current before)) (not (= revision (field current 'revision))))
                 (values 'stale (datum:copy current))]
                [(or (not available) (not (eq? definition (definition-of current))))
                 (values 'unavailable (datum:copy current))]
                [else (hashtable-delete! (state-records data) (cadr id)) (changed! (list id)) (values 'applied #f)])))))))

  (edoc "The persistent model representation as (values next-id envelopes), including opaque unknown schemas."
        (returns any))
  (define (export)
    (with-mutex (state-lock data)
      (values (state-next-id data)
        (datum:copy
          (filter (lambda (entry) (eq? (field entry 'persistence) 'persistent))
            (map (lambda (n) (hashtable-ref (state-records data) n #f))
              (list-sort < (vector->list (hashtable-keys (state-records data))))))))))

  (edoc "Whether saved model envelopes are well formed and known payloads validate; unknown kinds and schema versions remain opaque."
        (next-id integer "the next allocation number") (entries list "the saved envelopes") (returns boolean))
  (define (valid-import? next-id entries)
    (guard (ex [(datum:invalid? ex) #f] [else (raise ex)])
      (let ([entries (datum:copy entries)] [seen (make-eqv-hashtable)])
        (and (positive-integer? next-id) (list? entries)
             (for-all
               (lambda (entry)
                 (and (envelope? entry) (eq? (field entry 'persistence) 'persistent)
                      (let ([n (cadr (field entry 'id))] [definition (definition-of entry)])
                        (and (< n next-id) (not (hashtable-contains? seen n))
                             (or (not definition) (accepts? definition entry))
                             (begin (hashtable-set! seen n #t) #t))))) entries)))))

  (edoc "Restore persistent model records into a fresh store, validating everything before installation."
        (next-id integer "the next allocation number") (entries list "the saved envelopes"))
  (define (import! next-id entries)
    (activity:call-with
      (lambda ()
        (let ([entries (datum:copy entries)] [table (make-eqv-hashtable)])
          (unless (valid-import? next-id entries) (error 'import! "invalid saved model representation"))
          (for-each (lambda (entry) (hashtable-set! table (cadr (field entry 'id)) entry)) entries)
          (with-mutex (state-lock data)
            (unless (and (= (state-next-id data) 1) (zero? (hashtable-size (state-records data))))
              (error 'import! "restore requires a fresh empty model store"))
            (state-records-set! data table)
            (state-next-id-set! data next-id))))))
)
