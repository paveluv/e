;; Canonical containment uses model transactions, including unchanged read witnesses.
(import (only (foundation edoc) elibrary))
(elibrary (state view)
  (export arrange! (rename (descriptor:basis basis))
    (rename (descriptor:children children)) claim! create!
    (rename (descriptor:focus focus)) fork!
    (rename (descriptor:generation generation))
    (rename (descriptor:kind kind))
    (rename (descriptor:options options))
    (rename (descriptor:owned owned)) (rename (descriptor:owner owner))
    (rename (descriptor:parent parent)) publish! register-resource-kind! release!
    release-owner! reset-owners! retire! retire-scope!
    (rename (descriptor:schema schema))
    (rename (descriptor:sequence sequence)) set-state! snapshot
    (rename (descriptor:source source))
    (rename (descriptor:state state)) tree upgrade)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (state connection) connection:) (prefix (state model) model:))
  (define registration
    (kernel:call-with-runtime-registrations
      (lambda () (model:register-kind! 'widget-view 2 descriptor:valid?))))
  (define (field r k) (cdr (assq k r)))
  (define (value r) (field r 'value))
  (define (supported? r) (and r (eq? (field r 'kind) 'widget-view) (= (field r 'schema) 2)
                              (descriptor:valid? (value r))))
  (define (entry id) (let ([r (model:snapshot id)]) (and (supported? r) r)))
  (define (change r d) (list (field r 'id) (field r 'revision) (descriptor:references d) d))
  (define (row r) (cons (field r 'id) (value r)))
  (define (unique xs) (fold-left (lambda (out x) (if (member x out) out (cons x out))) '() xs))
  (define resource-kinds (kernel:make-registry car))

  (edoc "Register a base lifecycle for explicitly owned per-view resources. Prepare receives actor and resource envelope, returning three values: a pure mapping-to-allocation-spec builder, extra old-to-new reference pairs and a rollback thunk. It may prepare owned output before the guarded allocation; rollback releases it if the fork fails. Release receives actor and resource ID. Borrowed models never use this protocol."
        (kind symbol "model kind") (schema integer "model schema") (prepare procedure "prepare a copy")
        (release procedure "retire resource and its owned output"))
  (define (register-resource-kind! kind schema prepare release)
    (unless (and (symbol? kind) (integer? schema) (exact? schema) (> schema 0) (procedure? prepare) (procedure? release))
      (error 'register-resource-kind! "invalid resource lifecycle"))
    (kernel:registry-add! resource-kinds (list (list kind schema) prepare release)))
  (define (resource-kind r)
    (or (kernel:registry-find resource-kinds (lambda (p) (equal? (car p) (list (field r 'kind) (field r 'schema)))))
        (error 'view "resource lifecycle is unavailable" (field r 'kind))))
  (define (resources r)
    (filter values
      (map (lambda (id)
             (let ([resource (model:snapshot id)])
               (when (and resource (not (equal? (field resource 'scope) (field r 'id))))
                 (error 'view "owned resource belongs to another view" id)) resource))
        (descriptor:owned (value r)))))
  ;; Every supported record read becomes a witness, even when unchanged.
  (define (transaction! actor plan retry? . retired)
    (let loop ()
      (let ([reply
             (call/cc
               (lambda (abort)
                 (let ([read (make-hashtable equal-hash equal?)] [next (make-hashtable equal-hash equal?)])
                   (define (get id)
                     (unless (hashtable-contains? read id) (hashtable-set! read id (model:snapshot id)))
                     (let ([r (hashtable-ref read id #f)])
                       (and (supported? r) (or (hashtable-ref next id #f) (value r)))))
                   (define (need id) (or (get id) (abort (list 'unavailable '()))))
                   (define (put id d) (need id) (hashtable-set! next id d))
                   (plan get need put read (lambda (status) (abort (list status '()))))
                   (let* ([records (filter supported? (vector->list (hashtable-values read)))]
                          [changes (map (lambda (r) (change r (or (hashtable-ref next (field r 'id) #f) (value r)))) records)])
                     (let-values ([(status current)
                                   (if (null? retired) (model:commit! actor changes)
                                     (let* ([id (car retired)] [r (hashtable-ref read id #f)])
                                       (let-values ([(status current)
                                                     (model:retire! actor id (field r 'revision)
                                                       (filter (lambda (c) (not (equal? (car c) id))) changes))])
                                         (values status (if current (list current) '())))))])
                       (list (if (eq? status 'stale) 'retry status) (map row (filter supported? current))))))))])
        (if (and retry? (eq? (car reply) 'retry)) (loop)
            (values (if (eq? (car reply) 'retry) 'stale (car reply)) (cadr reply))))))
  (define (walk get root fail)
    (let ([seen '()] [out '()])
      (define (visit id parent root?)
        (when (member id seen) (fail 'invalid))
        (set! seen (cons id seen))
        (let ([d (get id)])
          (when d
            (unless (or root? (equal? parent (descriptor:parent d))) (fail 'invalid))
            (set! out (cons id out))
            (for-each (lambda (child) (visit (cadr child) id #f)) (descriptor:children d)))))
      (visit root #f #t) (reverse out)))
  (define (root-of get id fail)
    (let loop ([id id] [seen '()])
      (when (member id seen) (fail 'invalid))
      (let ([d (get id)])
        (unless d (fail 'unavailable))
        (if (descriptor:parent d) (loop (descriptor:parent d) (cons id seen)) id))))
  (define (ownership d who)
    (descriptor:with d (list (cons 'owner who) (cons 'generation (+ 1 (descriptor:generation d))) (cons 'sequence 0))))

  (edoc "Create an unparented view; source is a model/buffer reference or false for a container. An optional owning model gives the view its scope and persistence; otherwise it is session-persistent. Allocation witnesses the owner's lifetime."
        (actor actor "creator") (source datum "source reference") (kind symbol "widget contract")
        (schema integer "contract version") (options list "logical options") (state datum "interaction")
        (scope (list-of model) "optional resource owner; distinct from containment or mount ownership") (returns model))
  (define (create! actor source kind schema options state . scope)
    (unless (and (<= (length scope) 1) (for-all model:reference? scope)) (error 'create! "expected one resource owner"))
    (let* ([d (descriptor:make source kind schema options state)] [owner (and (pair? scope) (model:snapshot (car scope)))])
      (when (and (pair? scope) (not owner)) (error 'create! "resource owner is unavailable" (car scope)))
      (let ([ids (model:allocate! actor 1
                   (lambda (ids)
                     (list (list 'widget-view 2 (if owner (field owner 'id) 'session)
                             (if owner (field owner 'persistence) 'persistent) (descriptor:references d) d)))
                   (lambda (ids) (if owner (list (list (field owner 'id) (field owner 'revision) (field owner 'references) (value owner))) '())))])
        (unless ids (error 'create! "resource owner changed; retry")) (car ids))))

  (edoc "Retire a view against its revision, atomically unlinking its parent and releasing its child subtrees as unowned roots, then releasing its explicitly owned resources. Borrowed sources and command targets survive. Head callers unmount first; base resource owners may revoke their scoped views on departure. Return status and current target envelope."
        (actor actor "resource owner") (id model "view") (revision integer "expected model revision"))
  (define (retire! actor id revision)
    (unless (and (integer? revision) (exact? revision) (>= revision 0)) (error 'retire! "expected a revision"))
    (let* ([before (entry id)]
           [owned (if (and before (= revision (field before 'revision)))
                    (map (lambda (r) (cons (field r 'id) (caddr (resource-kind r)))) (resources before)) '())])
      (let-values ([(status rows)
                    (transaction! actor
                      (lambda (get need put read fail)
                        (let* ([d (need id)] [r (hashtable-ref read id #f)] [parent (descriptor:parent d)]
                               [subtree (walk get id fail)])
                          (unless (= revision (field r 'revision)) (fail 'stale))
                          (when parent
                            (let* ([p (need parent)] [root (root-of need parent fail)] [root-d (need root)])
                              (unless (member id (map cadr (descriptor:children p))) (fail 'invalid))
                              (when (member (descriptor:focus root-d) subtree)
                                (put root (descriptor:with root-d '((focus . #f)))))
                              (put parent (descriptor:with (need parent)
                                            (list (cons 'children (filter (lambda (c) (not (equal? id (cadr c)))) (descriptor:children p))))))))
                          (for-each
                            (lambda (child)
                              (let ([root (cadr child)])
                                (for-each
                                  (lambda (id)
                                    (let* ([d (need id)] [d (if (descriptor:owner d) (ownership d #f) d)])
                                      (put id (if (equal? id root) (descriptor:with d '((parent . #f) (focus . #f))) d))))
                                  (walk get root fail)))) (descriptor:children d)))) #f id)])
        (when (eq? status 'applied) (for-each (lambda (p) ((cdr p) actor (car p))) owned))
        (values status (model:snapshot id)))))

  (edoc "Retire views scoped to an already-retired resource and their scoped descendants. Guarded allocation cannot extend this closed lifetime; borrowed sources survive."
        (actor actor "resource owner") (owner model "retired resource"))
  (define (retire-scope! actor owner)
    (when (model:snapshot owner) (error 'retire-scope! "retire the resource before its views" owner))
    (for-each
      (lambda (id)
        (let retry ()
          (let ([r (model:snapshot id)])
            (when (and r (equal? (field r 'scope) owner))
              (let-values ([(status current) (retire! actor id (field r 'revision))])
                (case status [(stale) (retry)] [(applied) (retire-scope! actor id)]))))))
      (model:ids 'widget-view)))

  (edoc "Read a canonical descriptor, or #f if unavailable." (id model "view id") (returns any))
  (define (snapshot id) (let ([r (entry id)]) (and r (value r))))

  (edoc "Read a coherent supported subtree as (id . descriptor) entries; opaque children remain referenced."
        (id model "root or child id") (returns list))
  (define (tree id)
    (let loop ()
      (let ([result
             (call/cc
               (lambda (abort)
                 (let ([read (make-hashtable equal-hash equal?)])
                   (define (get id)
                     (unless (hashtable-contains? read id) (hashtable-set! read id (model:snapshot id)))
                     (let ([r (hashtable-ref read id #f)]) (and (supported? r) (value r))))
                   (unless (get id) (abort '()))
                   (let* ([ids (walk get id (lambda (status) (abort '()))) ]
                          [packet (model:snapshots (vector->list (hashtable-keys read)))])
                     ;; Revisions never go backwards: unchanged captured records
                     ;; coexist at the instant of this atomic verification read.
                     (if (for-all (lambda (r)
                                    (let ([before (hashtable-ref read (car r) #f)] [after (caddr r)])
                                      (equal? (and before (field before 'revision)) (and after (field after 'revision)))))
                           (cadr packet))
                         (map (lambda (id) (cons id (get id))) ids) 'retry)))) )])
        (if (eq? result 'retry) (loop) result))))

  (edoc "Claim an entire unowned root atomically; return status and (id . descriptor) entries."
        (actor actor "head") (id model "root id"))
  (define (claim! actor id)
    (unless (descriptor:head? actor) (error 'claim! "expected a head" actor))
    (transaction! actor
      (lambda (get need put read fail)
        (when (descriptor:parent (need id)) (fail 'parented))
        (for-each (lambda (id) (let ([d (need id)]) (when (descriptor:owner d) (fail 'owned))
                                 (put id (ownership d actor)))) (walk get id fail))) #t))

  (edoc "Release a tree only under its current root owner generation."
        (actor actor "head") (id model "root id") (generation integer "lease"))
  (define (release! actor id generation)
    (transaction! actor
      (lambda (get need put read fail)
        (let ([d (need id)])
          (unless (and (not (descriptor:parent d)) (equal? actor (descriptor:owner d))
                       (= generation (descriptor:generation d))) (fail 'stale)))
        (for-each (lambda (id) (let ([d (need id)])
                                 (unless (equal? actor (descriptor:owner d)) (fail 'stale))
                                 (put id (descriptor:with d '((owner . #f)))))) (walk get id fail))) #t))

  (edoc "Arrange parents atomically: (id expected-revision children options) entries and (root generation) leases for owned trees."
        (actor actor "caller") (changes list "parent changes") (leases list "owner guards"))
  (define (arrange! actor changes leases)
    (unless (and (list? changes) (list? leases)
                 (for-all (lambda (c) (and (list? c) (= (length c) 4))) changes)
                 (for-all (lambda (l) (and (list? l) (= (length l) 2))) leases)
                 (= (length changes) (length (unique (map car changes)))))
      (error 'arrange! "expected distinct parent changes and root leases"))
    (transaction! actor
      (lambda (get need put read fail)
        (let ([roots '()] [old-ids '()] [desired (make-hashtable equal-hash equal?)])
          (define (include id)
            (let ([root (root-of need id fail)])
              (unless (member root roots)
                (let* ([d (need root)] [lease (assoc root leases)])
                  (when (and (descriptor:owner d)
                             (not (and (equal? actor (descriptor:owner d)) lease
                                       (= (cadr lease) (descriptor:generation d))))) (fail 'owned))
                  (let ([ids (walk get root fail)])
                    (for-each (lambda (id) (unless (equal? (descriptor:owner (need id)) (descriptor:owner d))
                                             (fail 'invalid))) ids)
                    (set! old-ids (append ids old-ids)))
                  (hashtable-set! desired root (descriptor:owner d)))
                (set! roots (cons root roots)))))
          (for-each
            (lambda (c)
              (let* ([id (car c)] [d (need id)] [r (hashtable-ref read id #f)]
                     [next (descriptor:with d (list (cons 'children (caddr c)) (cons 'options (cadddr c))))])
                (unless (= (cadr c) (field r 'revision)) (fail 'stale))
                (unless (descriptor:valid? next) (error 'arrange! "invalid children or options" c))
                (include id) (for-each (lambda (child) (include (cadr child))) (caddr c)))) changes)
          (for-each (lambda (c) (for-each (lambda (child)
                                            (let ([id (cadr child)]) (put id (descriptor:with (need id) '((parent . #f))))))
                                  (descriptor:children (need (car c))))) changes)
          (for-each (lambda (c)
                      (let ([id (car c)])
                        (put id (descriptor:with (need id) (list (cons 'children (caddr c)) (cons 'options (cadddr c))))))
                      (for-each (lambda (child)
                                  (let* ([id (cadr child)] [d (need id)])
                                    (when (descriptor:parent d) (fail 'parented))
                                    (put id (descriptor:with d (list (cons 'parent (car c))))))) (caddr c))) changes)
          (let ([new-roots (unique (map (lambda (id) (root-of need id fail)) old-ids))])
            (for-each
              (lambda (root)
                (let* ([ids (walk get root fail)] [who (hashtable-ref desired root #f)])
                  (for-each
                    (lambda (id)
                      (let* ([d (need id)] [d (if (or who (descriptor:owner d)) (ownership d who) d)]
                             [focus (descriptor:focus d)])
                        (put id (if (and focus (or (not (equal? root id)) (not (member focus ids))))
                                    (descriptor:with d '((focus . #f))) d)))) ids))) new-roots)))) #f))

  (edoc "Atomically publish the newest still-owned interaction snapshots. Retired views, old ownership generations and already acknowledged sequences are ignored. Focus outside the surviving root is cleared. Authored edits use the store's separate guarded command path."
        (actor actor "owner") (updates list "interaction entries"))
  (define (publish! actor updates)
    (unless (and (list? updates)
                 (for-all
                   (lambda (r) (and (list? r) (= (length r) 6) (model:reference? (car r))
                                 (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) (list (cadr r) (caddr r)))
                                 (or (not (list-ref r 5)) (model:reference? (list-ref r 5)))))
                   updates)
                 (= (length updates) (length (unique (map car updates)))))
      (error 'publish! "expected distinct interaction entries"))
    (let-values ([(status rows)
                  (transaction!
                    actor
                    (lambda (get need put read fail)
                      (for-each
                        (lambda (r)
                          (let* ([id (car r)] [d (get id)] [focus (list-ref r 5)])
                            (when (and d (equal? actor (descriptor:owner d))
                                       (= (cadr r) (descriptor:generation d))
                                       (> (caddr r) (descriptor:sequence d)))
                              (when (and focus
                                      (not (and (not (descriptor:parent d))
                                             (call/cc (lambda (absent)
                                                        (equal? id (root-of get focus (lambda (status) (absent #f)))))))))
                                (set! focus #f))
                              (put id
                                (descriptor:with
                                  d
                                  (map cons '(sequence basis state focus) (append (list-head (cddr r) 3) (list focus))))))))
                        updates))
                    #t)])
      (values status #f)))

  (edoc "Change saved interaction of an unowned view; active views are operated through their head."
        (actor actor "caller") (id model "view") (basis any "source revision") (state datum "interaction"))
  (define (set-state! actor id basis state)
    (transaction! actor (lambda (get need put read fail)
                          (let ([d (need id)]) (when (descriptor:owner d) (fail 'owned))
                            (put id (descriptor:with d (list (cons 'basis basis) (cons 'state state)))))) #t))

  (edoc "Fork a supported subtree, sharing borrowed sources and copying explicitly owned resources and their internal connections. Prepare resource output before guarded allocation; failed forks release it. Return the new root ID."
        (actor actor "creator") (id model "source view") (returns model))
  (define (fork! actor id)
    (let* ([rows (tree id)]
           [originals (map (lambda (row)
                             (let ([r (model:snapshot (car row))])
                               (unless (and r (equal? (value r) (cdr row)))
                                 (error 'fork! "composition changed during fork; retry")) r)) rows)]
           [owned (apply append (map resources originals))]
           [all (append originals owned)]
           [scopes (unique (filter model:reference? (map (lambda (r) (field r 'scope)) all)))]
           [owners (map (lambda (scope) (or (model:snapshot scope) (error 'fork! "resource owner is unavailable" scope))) scopes)])
      (unless (and (assoc id rows)
                   (for-all
                     (lambda (row)
                       (for-all
                         (lambda (c) (assoc (cadr c) rows))
                         (descriptor:children (cdr row))))
                     rows))
        (error 'fork!
          "subtree contains unavailable descriptors"
          id))
      (unless (= (length owned) (apply + (map (lambda (r) (length (descriptor:owned (value r)))) originals)))
        (error 'fork! "an owned resource is unavailable"))
      (let ([prepared '()])
        (guard (ex [else
                    (for-each (lambda (p) (guard (cleanup [else (void)]) ((caddr p)))) prepared)
                    (raise ex)])
          (for-each
            (lambda (r)
              (let-values ([(build aliases rollback) ((cadr (resource-kind r)) actor r)])
                (when (procedure? rollback) (set! prepared (cons (list build aliases rollback) prepared)))
                (unless (and (procedure? build) (list? aliases) (for-all pair? aliases) (procedure? rollback))
                  (error 'fork! "invalid resource copy plan" (field r 'id))))) owned)
          (set! prepared (reverse prepared))
          (let ([aliases (apply append (map cadr prepared))])
            (unless (and (= (length aliases) (length (unique (map car aliases))))
                      (not (exists (lambda (r) (assoc (field r 'id) aliases)) all)))
              (error 'fork! "ambiguous resource reference mapping"))
            (car
              (connection:fork! actor all
                (lambda (ids)
                  (let ([copies (append (map (lambda (r new) (cons (field r 'id) new)) all ids) aliases)])
                    (define (mapped id) (cond [(assoc id copies) => cdr] [else id]))
                    (append
                      (map
                        (lambda (row original)
                          (let* ([old (cdr row)]
                                 [d (descriptor:with old
                                      (list (cons 'source (mapped (descriptor:source old)))
                                        (cons 'parent (and (not (equal? (car row) id)) (mapped (descriptor:parent old))))
                                        (cons 'children (map (lambda (c) (list (car c) (mapped (cadr c)) (caddr c))) (descriptor:children old)))
                                        (cons 'focus (and (assoc (descriptor:focus old) copies) (mapped (descriptor:focus old)))) '(owner . #f)
                                        (cons 'options
                                          (map (lambda (p)
                                                 (case (car p)
                                                   [(commands) (cons 'commands (map (lambda (c) (list (car c) (mapped (cadr c)) (caddr c) (cadddr c))) (cdr p)))]
                                                   [(owned) (cons 'owned (map mapped (cdr p)))] [else p])) (descriptor:options old)))
                                        '(generation . 0) '(sequence . 0)))])
                            (list 'widget-view 2 (mapped (field original 'scope)) (field original 'persistence) (descriptor:references d) d))) rows originals)
                      (map (lambda (p) ((car p) mapped)) prepared)))) owners aliases)))))))

  (edoc "Release this disconnected head's descriptors, including detached or malformed trees."
        (actor actor "head"))
  (define (release-owner! actor)
    (when (descriptor:head? actor)
      (transaction! actor (lambda (get need put read fail)
                            (for-each (lambda (id) (let ([d (get id)])
                                                     (when (and d (equal? actor (descriptor:owner d)))
                                                       (put id (descriptor:with d '((owner . #f))))))) (model:ids 'widget-view))) #t)))

  (edoc "Clear saved ownership after restoring models.")
  (define (reset-owners!)
    (transaction! '(base view) (lambda (get need put read fail)
                                 (for-each (lambda (id) (let ([d (get id)])
                                                          (when (and d (descriptor:owner d))
                                                            (put id (descriptor:with d '((owner . #f))))))) (model:ids 'widget-view))) #t))

  (edoc "Upgrade known saved leaf envelopes before model import; unknown schemas are unchanged."
        (r list "model envelope") (returns list))
  (define (upgrade r)
    (if (and (eq? (field r 'kind) 'widget-view) (= (field r 'schema) 1))
        (let ([old (value r)])
          (unless (and (list? old) (= (length old) 8)) (error 'upgrade "invalid saved leaf view"))
          (let ([d (descriptor:with (descriptor:make (car old) (cadr old) (caddr old) '() (list-ref old 7))
                     (map cons '(generation owner sequence basis) (list-head (list-tail old 3) 4)))])
            (map (lambda (p) (case (car p) [(schema) '(schema . 2)] [(value) (cons 'value d)] [else p])) r))) r)))
