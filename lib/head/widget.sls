;; Recursive mounts are head runtime objects, independent of window buffers.
(import (only (foundation edoc) elibrary))
(elibrary (head widget)
  (export act! actions arrange! cancel! capture! caret command-bindings commands context descendant event-frame focus! focus-next! focused
          frame-cell-styles frame-children frame-clip frame-data frame-descriptor frame-id frame-inputs frame-lines frame-rect frame-source frame-styles
          host init! input! invalidate! invoke! keep-host-focus! key-scopes key-scopes! mount! pointer! pointer-bindings prepare! prepared present! pump! receiver-live? receivers register! repaint! reveal! set-active! shown status target unmount!)
  (import (chezscheme)
          (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (core port) port:)
          (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head spinner) spinner:)
          (prefix (head text-source) text-source:)
          (prefix (state connection) connection:) (prefix (state model) model:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define definitions (kernel:make-registry car))
  (define roots (make-hashtable equal-hash equal?))
  (define nodes (make-hashtable equal-hash equal?))
  (define-record-type mount (fields id slot (mutable subscription) (mutable ids) (mutable bundle)))

  (edoc "The opaque host slot supplied when this tree was mounted."
        (id model "mounted view or descendant") (returns any) (effects internal))
  (define (host id) (mount-slot (node-root (mounted id))))

  (define retain-focus? (make-parameter #f))

  (edoc "Keep the outer host's focus after this pointer action; embedded actions can open into another host without focusing their panel.")
  (define (keep-host-focus!) (retain-focus? #t))
  (define-record-type node (fields id root (mutable mirrored) (mutable key) (mutable lines) (mutable data) (mutable data-key) (mutable visible) (mutable styles) (mutable caret) (mutable activity)))

  (edoc "An immutable prepared backend frame; only successful output makes it eligible for input."
        (id model "view id") (descriptor any "interaction basis") (definition any "definition identity")
        (source any "source basis") (inputs list "resolved input values and dependency bases") (data any "owned visible projection and hit basis") (rect list "absolute allocation within the root") (clip list "visible intersection")
        (children list "back-to-front child frames") (lines list "clipped backend output")
        (cells vector "backend style cells") (caret any "root-relative caret point or #f"))
  (define-record-type frame (fields id descriptor definition source inputs data rect clip children lines cells caret))
  (define preparations (make-hashtable equal-hash equal?))
  (define services (make-hashtable equal-hash equal?))
  (define pending-scroll (make-hashtable equal-hash equal?))
  (define pending-reveal (make-hashtable equal-hash equal?))
  (define scroll-positions (make-hashtable equal-hash equal?))
  ;; Subscribers run on publisher/client threads. They only enqueue a
  ;; coalesced refresh; mount caches and source demand belong to the pump.
  (define notifications (make-eq-hashtable))
  (define notification-lock (make-mutex))
  (define pump-thread (get-thread-id))
  (define (release-service! id)
    (let ([n (hashtable-ref nodes id #f)]) (when n (node-activity-set! n #f)))
    (let ([entry (hashtable-ref services id #f)])
      (when entry ((field entry 'release (lambda (id) (void))) id) (hashtable-delete! services id)))
    (hashtable-delete! pending-scroll id) (hashtable-delete! pending-reveal id) (hashtable-delete! scroll-positions id))

  (edoc "Service mounted controls outside frame preparation; acquire demand, adopt results and release obsolete definitions.")
  (define (pump!)
    (for-each (lambda (refresh) (refresh))
      (with-mutex notification-lock
        (let ([ready (vector->list (hashtable-values notifications))])
          (hashtable-clear! notifications) ready)))
    (vector-for-each
      (lambda (id)
        (let* ([n (hashtable-ref nodes id #f)] [d (and n (read-view id))] [entry (definition d)]
               [old (hashtable-ref services id #f)])
          (unless (eq? old entry)
            (release-service! id)
            (when entry (hashtable-set! services id entry)))
          (when entry ((field entry 'service (lambda (id frame) (void))) id (allocation id)))))
      (hashtable-keys nodes))
    (vector-for-each
      (lambda (child)
        (let* ([intent (hashtable-ref pending-scroll child #f)] [parent (car intent)]
               [d (read-view parent)] [f (allocation parent)])
          (when (and d f)
            (let ([anchor (anchor! child (cdr intent) (caddr (frame-rect f)))])
              (when anchor
                (hashtable-delete! pending-scroll child)
                (interaction:set-state! head:ui-actor parent #f anchor))))))
      (hashtable-keys pending-scroll))
    (vector-for-each (lambda (id) (reveal! id (hashtable-ref pending-reveal id #f))) (hashtable-keys pending-reveal)))
  (define presentations '())
  (define inactive (make-hashtable equal-hash equal?))

  (edoc "Set host focus independently of the tree's remembered logical focus." (id model "mounted root") (active boolean "whether host has focus"))
  (define (set-active! id active)
    (let ([was (not (hashtable-ref inactive id #f))])
      (if active (hashtable-delete! inactive id) (hashtable-set! inactive id #t))
      (unless (eq? was active)
        (for-each (lambda (id) (repaint! id)) (mount-ids (node-root (mounted id)))))))

  (edoc "Read the active host's focused descendant, or false for an inactive host." (id model "mounted view") (returns any) (effects internal))
  (define (focused id)
    (let* ([root (mount-id (node-root (mounted id)))] [d (read-view root)])
      (and (not (hashtable-ref inactive root #f)) d (view:focus d))))
  (define frame-reads (make-parameter #f))
  (define (read-view id)
    ;; Measurement, layout and painting share one immutable descriptor per
    ;; view in this preparation. Interactive callbacks run outside the cache.
    (let ([cache (frame-reads)])
      (if (not cache) (interaction:snapshot id)
        (begin
          (unless (hashtable-contains? cache id) (hashtable-set! cache id (interaction:snapshot id)))
          (hashtable-ref cache id #f)))))
  (define (definition d)
    (and d (kernel:registry-find definitions
             (lambda (entry) (equal? (car entry) (list (view:kind d) (view:schema d)))))))

  (edoc "Read a mounted view's optional status spans without acquisition. Each span pairs text with a style, false, or an inspectable keymap call."
        (id model "view") (active? boolean "host focus") (returns list) (effects internal))
  (define (status id active?)
    (let* ([d (read-view id)] [provider (field (definition d) 'status #f)])
      (if provider (provider id d active?) '())))
  (define (field entry name fallback)
    (cond [(and entry (assq name (cdr entry))) => cdr] [else fallback]))
  (define (mounted id)
    (or (hashtable-ref nodes id #f) (error 'widget "view is not mounted" id)))

  (edoc "Find a descendant of a mounted view by its named child path. Read the head's current logical tree; no geometry or window discovery is involved. A missing child refuses."
        (id model "starting view") (path (list-of symbol) "child names in order") (returns model "descendant view") (effects internal) (inspect))
  (define (descendant id . path)
    (unless (for-all symbol? path) (error 'descendant "expected child names" path))
    (mounted id)
    (fold-left (lambda (id name)
                 (let* ([d (read-view id)] [child (and d (assq name (view:children d)))])
                   (unless child (error 'descendant "child is unavailable" id name))
                   (cadr child))) id path))
  (define (rows id)
    (let walk ([id id])
      (let ([d (read-view id)])
        (cons (cons id d) (if d (apply append (map (lambda (child) (walk (cadr child))) (view:children d))) '())))))

  (edoc "Register a head definition owned by the defining module; reject unknown and duplicate fields."
        (kind symbol "widget kind") (schema integer "positive version")
        (definition list "actions, contexts, focus, capture; optional render, measure, layout and event procedures. Contexts may be a list or a read-only (id descriptor) provider using already acquired state; never perform I/O there."))
  (define (register! kind schema definition)
    (unless (and (symbol? kind) (integer? schema) (exact? schema) (> schema 0) (list? definition)
              (let loop ([rest definition] [seen '()])
                (or (null? rest)
                  (let ([p (car rest)])
                    (and (pair? p) (not (memq (car p) seen))
                      (case (car p)
                        [(actions) (and (list? (cdr p))
                                     (let check ([rest (cdr p)] [names '()])
                                       (or (null? rest)
                                         (and (pair? (car rest)) (symbol? (caar rest)) (procedure? (cdar rest))
                                           (not (memq (caar rest) names)) (check (cdr rest) (cons (caar rest) names))))))]
                        [(contexts) (or (procedure? (cdr p)) (and (list? (cdr p)) (for-all symbol? (cdr p))))]
                        [(capture-contexts) (or (procedure? (cdr p)) (and (list? (cdr p)) (for-all symbol? (cdr p))))]
                        [(yield) (or (procedure? (cdr p)) (and (list? (cdr p)) (for-all string? (cdr p))))]
                        [(focus) (boolean? (cdr p))]
                        [(source-receiver) (symbol? (cdr p))]
                        [(receivers)
                         (and (list? (cdr p))
                           (for-all (lambda (r) (and (list? r) (pair? r) (symbol? (car r))
                                                  (pair? (cdr r)) (for-all symbol? (cdr r)))) (cdr p)))]
                        [(capture) (or (procedure? (cdr p)) (memq (cdr p) '(full partial)))]
                        [(snapshot prepare viewport service release render measure layout event capture-event pointer-bindings capture-pointer-bindings anchor locate decorate caret busy? status) (procedure? (cdr p))]
                        [else #f])
                      (loop (cdr rest) (cons (car p) seen)))))))
      (error 'register! "invalid widget definition" kind schema definition))
    (for-each
      (lambda (action)
        (for-each (lambda (sig)
                    (let ([r (edoc:signature-receiver sig)])
                      (when (and r (not (and (eq? (caadr r) 'view) (memq kind (cdadr r)))))
                        (error 'register! "action receiver contract differs from its widget" kind (car action) r))))
          (or (edoc:edoc-of (cdr action)) '())))
      (field (cons #f definition) 'actions '()))
    (kernel:registry-add! definitions (cons (list kind schema) (map (lambda (p) (cons (car p) (cdr p))) definition))))

  (edoc "Capture explicit receivers from ancestry, declared child paths and an explicitly exposed model source. Rows are (id kind schema witness owner label): a view witness is its generation; a model witness retains its hosting view lease and optional namespace generation. Only local mirrors are read."
        (id model "focused view") (returns list) (effects internal))
  (define (receivers id)
    (define (row id label)
      (let ([d (read-view id)])
        (and d (definition d)
          (list id (view:kind d) (view:schema d) (view:generation d) (view:owner d)
            (or label (option d 'name #f) (symbol->string (view:kind d)))))))
    (define (source-row at d label)
      (and label
        (let-values ([(available? source) (raw-source! (mounted at) d)])
          (and available? source (model:reference? (cdr (assq 'id source)))
            (let* ([value (cdr (assq 'value source))]
                   [generation (and (list? value) (for-all pair? value) (assq 'generation value))])
              (list (cdr (assq 'id source)) (cdr (assq 'kind source)) (cdr (assq 'schema source))
                (list 'source at (view:generation d) (view:owner d) generation) #f (symbol->string label)))))))
    (mounted id)
    (fold-left
      (lambda (out at)
        (let* ([d (read-view at)]
               [extra (map (lambda (p) (row (apply descendant at (cdr p)) (symbol->string (car p))))
                        (field (definition d) 'receivers '()))])
          (fold-left (lambda (out r) (if (or (not r) (assoc (car r) out)) out (append out (list r))))
            out (append (list (row at #f) (source-row at d (field (definition d) 'source-receiver #f))) extra)))) '() (reverse (path id))))

  (edoc "Whether a captured receiver still has its exact kind, schema, ownership generation and owner. No remote reads or implicit retargeting."
        (receiver list "row from receivers") (returns boolean) (effects internal))
  (define (receiver-live? receiver)
    (let* ([id (car receiver)] [witness (list-ref receiver 3)]
           [at (if (pair? witness) (cadr witness) id)]
           [d (and (hashtable-contains? nodes at) (read-view at))])
      (and d (definition d)
        (if (not (pair? witness))
          (equal? (list (view:kind d) (view:schema d) (view:generation d) (view:owner d))
            (list-head (cdr receiver) 4))
          (and (field (definition d) 'source-receiver #f)
            (equal? (list (view:generation d) (view:owner d)) (list-head (cddr witness) 2))
            (let-values ([(available? source) (raw-source! (mounted at) d)])
              (and available? source (equal? id (cdr (assq 'id source)))
                (equal? (list (cdr (assq 'kind source)) (cdr (assq 'schema source))) (list-head (cdr receiver) 2))
                (let ([value (cdr (assq 'value source))])
                  (equal? (list-ref witness 4)
                    (and (list? value) (for-all pair? value) (assq 'generation value)))))))) #t)))

  (define (source-id id d)
    (and d (view:source d)
      (let* ([ds (port:describe (list 'view (view:kind d) (view:schema d)))]
             [input (and ds (find (lambda (p) (and (eq? (car p) 'input) (equal? (cadddr p) '(source)))) ds))])
        (and (not (and input (exists (lambda (e) (and (equal? id (cadr e)) (eq? (cadr input) (caddr e))))
                                     (cadr (connection:snapshot (list id))))))
          (view:source d)))))
  (define (raw-source! n d)
    (let ([id (source-id (node-id n) d)])
      (cond
        [(not d) (values #f #f)]
        [(not id) (values #t #f)]
        [(eq? (car id) 'buffer)
         (let ([source (text-source:lookup (cadr id))])
           (if source
             (let ([rev (text-source:revision source)])
               (unless (and (node-mirrored n) (= rev (caar (node-mirrored n))))
                 (node-mirrored-set! n
                   (cons (cons rev #t) (list (cons 'id id) (cons 'revision rev) (cons 'value (text-source:lines source))))))
               (values #t (cdr (node-mirrored n))))
             (values #f #f)))]
        [else
         (unless (node-mirrored n)
           (node-mirrored-set! n (cons (cons #f (model:available? id)) (model:snapshot id))))
         (values (cdar (node-mirrored n)) (cdr (node-mirrored n)))])))

  (define (source! n d)
    (let-values ([(available? source) (raw-source! n d)])
      (let ([snapshot (field (definition d) 'snapshot #f)])
        (if (and available? snapshot source)
          (let* ([next (snapshot (node-id n) source)]
                 [revision (and (list? next) (for-all pair? next) (assq 'revision next))])
            (unless (or (not next)
                      (and revision (assq 'id next) (equal? (assq 'id next) (assq 'id source))
                        (integer? (cdr revision)) (exact? (cdr revision))
                        (<= 0 (cdr revision) (cdr (assq 'revision source)))))
              (error 'source! "snapshot must retain source identity and an acquired revision" (node-id n)))
            (values (and next #t) next))
          (values available? source)))))

  (edoc "List available named actions of a mounted view, including a nested child."
        (id model "view id") (returns list) (effects internal))
  (define (actions id)
    (let* ([n (mounted id)] [d (read-view id)] [entry (definition d)])
      (let-values ([(available? source) (source! n d)])
        (if available? (map car (field entry 'actions '())) '()))))

  (edoc "List usable explicit command bindings; absent or foreign targets are disabled." (id model "mounted control") (returns list) (effects internal))
  (define (commands id)
    (let ([d (read-view id)])
      (if (not d) '()
        (filter (lambda (c) (cdr (command-target c))) (descriptor:commands d)))))

  (define (command-target c)
    (let* ([id (cadr c)] [n (hashtable-ref nodes id #f)] [d (and n (read-view id))]
           [proc (assq (caddr c) (field (definition d) 'actions '()))])
      (cons (and proc (cdr proc))
        (and proc d (equal? (view:owner d) head:ui-actor)
          (let-values ([(available? source) (source! n d)]) available?)))))

  (edoc "Inspect all named command connections in a mounted composition from local descriptors and definitions. Rows are (view child-path kind bindings); bindings are (name target action fixed-arguments procedure-or-false available?). Include unavailable targets without invoking callbacks, reading payloads remotely or moving focus."
        (id model "composition root or subtree") (returns list) (effects internal))
  (define (command-bindings id)
    (mounted id)
    (let walk ([id id] [path '()])
      (let* ([d (read-view id)] [cs (if d (descriptor:commands d) '())])
        (append
          (if (null? cs) '()
            (list (list id path (view:kind d)
                    (map (lambda (c) (let ([target (command-target c)]) (append c (list (car target) (and (cdr target) #t))))) cs))))
          (if d (apply append (map (lambda (child) (walk (cadr child) (append path (list (car child))))) (view:children d))) '())))))

  (edoc "Invoke an explicit command target with its fixed arguments followed by control-supplied arguments."
        (id model "control") (command symbol "binding name") (arguments (list-of any) "additional arguments") (returns any))
  (define-forwarding (invoke! id command . arguments) invoke-command! inspect-command)

  (define (invoke-command! id command . arguments)
    (let ([c (assq command (commands id))])
      (unless c (error 'invoke! "command target is unavailable" id command))
      (act! (apply (cadr c) (caddr c) (append (cadddr c) arguments)))))

  (define (known-target? args)
    (and (>= (length args) 2) (eq? (caar args) 'value) (eq? (caadr args) 'value)
      (symbol? (cadadr args))))
  (define (inspect-command args)
    (if (not (known-target? args)) '()
      (let* ([id (cadar args)] [name (cadadr args)] [d (read-view id)]
             [c (and d (assq name (descriptor:commands d)))])
        (if (not c) '()
          (let ([target (command-target c)])
            (list (list dispatch-action!
                    (append (map (lambda (v) (list 'value v)) (cons* (cadr c) (caddr c) (cadddr c))) (cddr args))
                    #f (and (not (cdr target)) "Unavailable target"))))))))

  (define (inspect-action args)
    (if (not (known-target? args)) '()
      (let ([target (command-target (list 'inspect (cadar args) (cadadr args)))])
        (if (car target)
          (list (list (car target) (cons (car args) (cddr args)) #f
                  (and (not (cdr target)) "Unavailable target"))) '()))))

  (edoc "Invalidate one mounted view's derived presentation after a head-local cache or hover change."
        (id model "view") (projection (list-of boolean) "also rebuild prepared data when true"))
  (define (repaint! id . projection)
    (let ([n (hashtable-ref nodes id #f)])
      (when n
        (node-key-set! n #f)
        (when (and (pair? projection) (car projection)) (node-data-key-set! n #f))
        (head:wake-main!))))

  (define invocation (make-parameter #f))
  (define input-reads (make-parameter #f))
  (define (inputs! id)
    (let ([cache (input-reads)])
      (if (and cache (hashtable-contains? cache id)) (hashtable-ref cache id #f)
        ;; Acquire one coherent dependency bundle per mount invalidation on
        ;; the service path. Provisional view state and mirrored text below
        ;; remain live; painting does not recapture/copy the whole graph.
        (let* ([bundle (mount-bundle (node-root (mounted id)))] [rows (caddr bundle)])
          (define (get id)
            (let* ([row (assoc id rows)] [r (and row (cadr row) (caddr row))]
                   [d (and r (eq? (field r 'kind #f) 'widget-view) (interaction:snapshot id))])
              (if d (map (lambda (p) (if (eq? (car p) 'value) (cons 'value d) p)) r) r)))
          (define (text id)
            (let ([source (text-source:lookup (cadr id))])
              (and source (list (cons 'id id) (cons 'revision (text-source:revision source)) (cons 'value (text-source:lines source))))))
          (let* ([r (get id)] [ds (and r (port:describe (port:key r)))]
                 [inputs (if ds
                           (map (lambda (d)
                                  (let ([resolved (port:resolve id (cadr d) (cadr bundle) get text)])
                                    (cons (cadr d) (list (car resolved) (cadr resolved) (cons (car bundle) (caddr resolved))))))
                             (filter (lambda (d) (eq? (car d) 'input)) ds)) '())])
            (when cache (hashtable-set! cache id inputs)) inputs)))))

  (edoc "Read a mounted view's borrowed source, provisional descriptor and resolved inputs as three values, without remote reads. Actions retain their exact invocation basis."
        (id model "view") (mode (list-of symbol) "current bypasses a shown action basis") (effects internal))
  (define (context id . mode)
    (unless (or (null? mode) (equal? mode '(current))) (error 'context "expected current" mode))
    (let ([current (and (null? mode) (invocation))])
      (if (and current (equal? id (car current))) (apply values (cdr current))
        (let* ([n (mounted id)] [d (read-view id)] [f (and (null? mode) (event-frame))])
          (let-values ([(available? source) (source! n d)])
            (unless available? (error 'context "widget source is unavailable" id))
            (if (and f (equal? id (frame-id f)))
              (values (frame-source f) (frame-descriptor f) (frame-inputs f))
              (values source d (inputs! id))))))))

  (edoc "Invoke a named action with an explicit view, coherent source and provisional interaction."
        (id model "view id") (action symbol "action") (arguments (list-of any) "action arguments") (returns any))
  (define-forwarding (act! id action . arguments) dispatch-action! inspect-action)

  (define (dispatch-action! id action . arguments)
    (let* ([n (mounted id)] [d (read-view id)] [entry (definition d)]
           [proc (assq action (field entry 'actions '()))])
      (let-values ([(available? source) (source! n d)])
        (unless (and available? proc) (error 'act! "widget action is unavailable" id action))
        (call-with-values (lambda ()
                            (parameterize ([invocation (let ([f (event-frame)])
                                                         (if (and f (equal? id (frame-id f)))
                                                           (list id (frame-source f) (frame-descriptor f) (frame-inputs f))
                                                           (list id source d (inputs! id))))])
                              (apply (cdr proc) id arguments)))
          (lambda result (head:wake-main!) (apply values result))))))

  (define (subscribe! mount tree)
    (let ([tokens (list #f #f #f)] [demand #f] [endpoints (map car tree)]
          [live? #t] [acquire? #f])
      (define (refresh)
        (let ([work (with-mutex notification-lock
                      (and live? (let ([work (if acquire? 'acquire 'change)])
                                   (set! acquire? #f) work)))])
          (when work (when (eq? work 'acquire) (acquire)) (changed))))
      (define (schedule! demand?)
        (if (= pump-thread (get-thread-id))
          (when live? (when demand? (acquire)) (changed))
          (with-mutex notification-lock
            (when live?
              (set! acquire? (or demand? acquire?))
              (hashtable-set! notifications tokens refresh))))
        (head:wake-main!))
      (define (changed)
        (mount-bundle-set! mount (connection:snapshot endpoints))
        (interaction:reconcile!
          (filter values
            (map (lambda (row)
                   (let ([r (caddr row)])
                     (and (or (not r) (eq? (field r 'kind #f) 'widget-view))
                       ;; A temporarily unavailable port contract does not
                       ;; revoke the underlying view's ownership.
                       (cons (car row) (and r (field r 'value #f))))))
              (caddr (mount-bundle mount)))))
        (for-each (lambda (id) (let ([n (hashtable-ref nodes id #f)]) (when n (node-mirrored-set! n #f)))) (mount-ids mount))
        (head:wake-main!))
      (define (acquire)
        (let ([texts '()] [ids '()])
          (for-each
            (lambda (row)
              (let* ([d (cdr row)] [source (source-id (car row) d)])
                (cond [(not source) (unless d (set! ids (cons (car row) ids)))]
                  [(eq? (car source) 'buffer)
                   (let* ([id (cadr source)] [old (assv id texts)] [basis (view:basis d)])
                     (set! texts (cons (cons id (if (and old (cdr old) basis) (min (cdr old) basis) (or basis (and old (cdr old)))))
                                   (if old (remq old texts) texts))))]
                  [(not (member source ids)) (set! ids (cons source ids))]))) tree)
          (unless (equal? demand ids)
            (let ([fresh (model:subscribe! ids (lambda (notice) (schedule! #f)))] [old (car tokens)])
              (set-car! tokens fresh) (set! demand ids) (when old (model:unsubscribe! old))))
          (for-each (lambda (p)
                      (apply text-source:open! head:ui-actor (car p) (if (cdr p) (list (cdr p)) '()))) texts)
          (mount-bundle-set! mount (connection:snapshot endpoints))
          (for-each (lambda (id) (text-source:open! head:ui-actor (cadr id))) (cadddr (mount-bundle mount)))))
      (guard (ex [else (when (car tokens) (model:unsubscribe! (car tokens)))
                       (when (cadr tokens) (connection:unsubscribe! (cadr tokens)))
                       (when (caddr tokens) ((caddr tokens))) (raise ex)])
        (set-car! (cddr tokens)
          (lambda () (with-mutex notification-lock
                       (set! live? #f) (hashtable-delete! notifications tokens))))
        (set-car! (cdr tokens) (connection:subscribe! endpoints (lambda () (schedule! #t))))
        (acquire) tokens)))
  (define (unsubscribe! token)
    ((caddr token))
    (model:unsubscribe! (car token)) (connection:unsubscribe! (cadr token)))
  (define (reconcile! mount tree)
    (let ([ids (map car tree)])
      (for-each (lambda (id)
                  (let ([n (hashtable-ref nodes id #f)])
                    (when (and n (eq? (node-root n) mount) (not (member id ids)))
                      (release-service! id) (hashtable-delete! nodes id) (hashtable-delete! failures id)))) (mount-ids mount))
      (for-each
        (lambda (row)
          (let* ([id (car row)] [old (hashtable-ref nodes id #f)])
            (if (and old (eq? (node-root old) mount))
              (begin (node-mirrored-set! old #f) (node-key-set! old #f))
              (hashtable-set! nodes id (make-node id mount #f #f #f #f #f #f #f #f #f))))) tree)
      (mount-ids-set! mount ids)))

  (edoc "Attach a root tree to an opaque host slot. Repeating this attachment is idempotent; a second live host is refused."
        (id model "root view") (slot any "head-local host identity") (returns any))
  (define (mount! id slot)
    (let ([old (hashtable-ref roots id #f)])
      (cond [old (unless (eq? slot (mount-slot old)) (error 'mount! "view already has a host" id)) old]
        [else
         (kernel:call-with-runtime-registrations
           (lambda ()
             (let-values ([(status d) (interaction:claim! head:ui-actor id)])
               (unless (memq status '(applied unavailable)) (error 'mount! "view cannot be mounted" status id))
               (let* ([m (make-mount (datum:copy id) slot #f '() #f)] [tree (rows id)])
                 (guard (ex [else
                             (when (mount-subscription m) (unsubscribe! (mount-subscription m)))
                             (when d (interaction:release! head:ui-actor id (view:generation d)))
                             (raise ex)])
                   (mount-subscription-set! m (subscribe! m tree))
                   (reconcile! m tree) (hashtable-set! roots id m) (pump!) m)))))])))

  (edoc "Release a root and its recursive resources after publication; canonical views and sources survive."
        (id model "root id"))
  (define (unmount! id)
    (let ([m (hashtable-ref roots id #f)])
      (when m
        (cancel! id 'unmount)
        (let ([d (read-view id)])
          (when d (interaction:release! head:ui-actor id (view:generation d))))
        (hashtable-delete! roots id)
        (hashtable-delete! preparations id)
        (hashtable-delete! last-focus id)
        (hashtable-delete! inactive id)
        (set! presentations (filter (lambda (p) (not (equal? id (frame-id (car p))))) presentations))
        (for-each (lambda (id) (release-service! id) (hashtable-delete! nodes id) (hashtable-delete! failures id)) (mount-ids m))
        (unsubscribe! (mount-subscription m)))))

  (edoc "Arrange owned trees, staging source demand before the guarded structural commit."
        (changes list "(parent revision children options) entries"))
  (define (arrange! changes)
    (kernel:call-with-runtime-registrations (lambda ()
                                              (let* ([affected (fold-left (lambda (out change)
                                                                            (let ([root (node-root (mounted (car change)))]) (if (memq root out) out (cons root out)))) '() changes)]
                                                     [extra (apply append (map (lambda (change)
                                                                                 (apply append (map (lambda (child) (if (read-view (cadr child)) (rows (cadr child)) (view:tree (cadr child)))) (caddr change)))) changes))]
                                                     [staged '()])
                                                (for-each (lambda (change)
                                                            (for-each (lambda (child)
                                                                        (when (hashtable-contains? roots (cadr child))
                                                                          (error 'arrange! "unmount a root before nesting it" (cadr child)))) (caddr change))) changes)
                                                (dynamic-wind void
                                                  (lambda ()
                                                    (for-each (lambda (m) (set! staged (cons (cons m (subscribe! m (append (rows (mount-id m)) extra))) staged))) affected)
                                                    (let-values ([(status changed) (interaction:arrange! head:ui-actor changes
                                                                                     (map (lambda (m) (list (mount-id m) (view:generation (read-view (mount-id m))))) affected))])
                                                      (when (eq? status 'applied)
                                                        (for-each (lambda (m) (cancel! (mount-id m) 'arrange)) affected)
                                                        (for-each (lambda (m)
                                                                    (let* ([tree (rows (mount-id m))] [token (subscribe! m tree)] [old (mount-subscription m)])
                                                                      (reconcile! m tree) (mount-subscription-set! m token) (unsubscribe! old))) affected)
                                                        (head:wake-main!))
                                                      (values status changed)))
                                                  (lambda () (for-each (lambda (p) (unsubscribe! (cdr p))) staged)))))))

  (define failures (make-hashtable equal-hash equal?))
  (define (option d name fallback)
    (cond [(and d (assq name (view:options d))) => cdr] [else fallback]))
  (define (projection! n entry source)
    (let* ([inputs (inputs! (node-id n))] [key (list entry source inputs)])
      (unless (equal? key (node-data-key n))
        (node-data-set! n ((field entry 'prepare (lambda (id source inputs) source)) (node-id n) source inputs))
        (node-data-key-set! n key))
      (node-data n)))
  (define measurement-cache (make-parameter #f))
  (define (measure! id axis cross)
    (let* ([cache (measurement-cache)] [key (list id axis cross)] [old (and cache (hashtable-ref cache key #f))])
      (or old (let ([result (measure-node! id axis cross)])
                (when cache (hashtable-set! cache key result)) result))))
  (define (measure-node! id axis cross)
    (guard (ex [else '(1 1)])
      (let* ([n (mounted id)] [d (read-view id)] [entry (definition d)])
        (let-values ([(available? source) (source! n d)])
          (let* ([proc (field entry 'measure #f)]
                 [result (if (and available? proc)
                           (proc (projection! n entry source) d axis cross measure!)
                           '(1 1))])
            (unless (and (list? result) (= (length result) 2)
                         (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0))) result)
                         (<= (car result) (cadr result))) (error 'measure! "invalid measurement" result))
            result)))))
  (define (locate! id anchor width)
    (let* ([n (mounted id)] [d (read-view id)] [entry (definition d)])
      (let-values ([(available? source) (source! n d)])
        (if (and available? entry)
          (let ([proc (field entry 'locate #f)])
            (if proc (proc (projection! n entry source) anchor width) (locate-child! id anchor width)))
          0))))
  (define (overlay-layout d width height measure locate)
    (map (lambda (child) (list (cadr child) (list 0 0 width height))) (view:children d)))
  (define (scroll-layout d width height measure locate)
    (unless (= (length (view:children d)) 1) (error 'scroll "expected one child"))
    (let* ([id (cadar (view:children d))] [extent (max height (cadr (measure id 'y width)))]
           [position (locate id (view:state d) width)]
           [intent (hashtable-ref pending-scroll id #f)]
           [at (min (max 0 (- extent height)) (max 0 (if intent (cdr intent) (or position (hashtable-ref scroll-positions id 0)))))])
      ;; A logical anchor can move as content grows before it. Retain its
      ;; latest resolved position while the next result is being acquired.
      (hashtable-set! scroll-positions id at)
      (list (list id (list 0 (- at) width extent)))))
  (define (overlay-measure data d axis cross measure)
    (map (lambda (i) (apply max 0 (map (lambda (child) (list-ref (measure (cadr child) axis cross) i)) (view:children d)))) '(0 1)))
  (define (content-placements! id width)
    (let* ([d (read-view id)] [entry (definition d)] [layout (field entry 'layout #f)])
      (if layout (layout d width (cadr (measure! id 'y width)) measure! locate!) '())))
  (define (anchor! id position width)
    (let* ([n (mounted id)] [d (read-view id)] [entry (definition d)] [proc (field entry 'anchor #f)])
      (let-values ([(available? source) (source! n d)])
        (cond [(and available? proc) (proc (projection! n entry source) position width)]
          [else
           (let* ([placements (content-placements! id width)]
                  [p (or (find (lambda (p) (< position (+ (cadr (cadr p)) (cadddr (cadr p))))) placements)
                         (and (pair? placements) (car (reverse placements))))])
             (and p (let* ([ids (map car placements)] [tail (member (car p) ids)])
                      (list 'child (car p) (anchor! (car p) (max 0 (- position (cadr (cadr p)))) (caddr (cadr p)))
                        (cdr tail) (reverse (list-head ids (- (length ids) (length tail))))))))]))))
  (define (locate-child! id anchor width)
    (let* ([placements (content-placements! id width)]
           [find-id (lambda (id) (find (lambda (p) (equal? id (car p))) placements))]
           [p (and (list? anchor) (= (length anchor) 5) (eq? (car anchor) 'child)
                (or (find-id (cadr anchor))
                  (exists find-id (append (list-ref anchor 3) (list-ref anchor 4)))))])
      (if p (let ([at (if (equal? (car p) (cadr anchor)) (locate! (car p) (caddr anchor) (caddr (cadr p))) 0)])
              (and at (+ (cadr (cadr p)) at))) 0)))
  (define (find-frame frame id)
    (and frame (if (equal? id (frame-id frame)) frame (exists (lambda (f) (find-frame f id)) (frame-children frame)))))
  (define (allocation id)
    (or (exists (lambda (p) (find-frame (car p) id)) presentations)
      (find-frame (prepared (mount-id (node-root (mounted id)))) id)))
  (define (scroll-action! id delta)
    (vector-for-each (lambda (child) (when (member id (path child)) (hashtable-delete! pending-reveal child))) (hashtable-keys pending-reveal))
    (let-values ([(source d inputs) (context id)])
      (let* ([frame (allocation id)]
             [child (and (= 1 (length (view:children d)))
                         (cadar (view:children d)))])
        (unless (and frame child (integer? delta) (exact? delta))
          (error 'scroll
            "expected an allocated viewport and integer delta"))
        (let* ([width (caddr (frame-rect frame))]
               [height (cadddr (frame-rect frame))]
               [limit (max 0 (- (cadr (measure! child 'y width)) height))]
               [intent (hashtable-ref pending-scroll child #f)]
               [old (min limit (max 0 (if intent (cdr intent) (or (locate! child (view:state d) width) (hashtable-ref scroll-positions child 0)))))]
               [next (min limit (max 0 (+ old delta)))])
          (hashtable-set! scroll-positions child next)
          (let ([anchor (anchor! child next width)])
            (if anchor
              (begin (hashtable-delete! pending-scroll child) (interaction:set-state! head:ui-actor id #f anchor))
              (begin (hashtable-set! pending-scroll child (cons id next)) (head:wake-main!))))
          (- delta (- next old))))))

  (define (rectangle? r)
    (and (list? r) (= (length r) 4) (for-all (lambda (n) (and (integer? n) (exact? n))) r)
      (>= (caddr r) 0) (>= (cadddr r) 0)))
  (define (composite clip lines children)
    (let* ([width (caddr clip)] [height (cadddr clip)]
           [canvas (list->vector (append lines (make-list (max 0 (- height (length lines))) (make-string width #\space))))])
      (for-each
        (lambda (child)
          (let* ([area (frame-clip child)] [x (- (car area) (car clip))] [y (- (cadr area) (cadr clip))])
            (for-each
              (lambda (line row)
                (let ([old (vector-ref canvas (+ row y))])
                  (vector-set! canvas (+ row y)
                    (if (and (zero? x) (= (caddr area) width)) line
                      (string-append (glyph:slice old 0 x) line
                        (glyph:slice old (+ x (caddr area)) (- width x (caddr area))))))))
              (frame-lines child) (iota (length (frame-lines child)))))) children)
      (vector->list canvas)))
  (define (style-cells clip rect decorations)
    (let ([rows (list->vector (map (lambda (i) (make-vector (caddr clip) #f)) (iota (cadddr clip))))])
      (for-each (lambda (p)
                  (unless (and (list? p) (= (length p) 2) (rectangle? (car p))
                            (or (symbol? (cadr p)) (string? (cadr p))
                              (and (pair? (cadr p)) (list? (cadr p)) (for-all (lambda (face) (or (symbol? face) (string? face))) (cadr p)))))
                    (error 'prepare! "invalid decoration" p))
                  (let ([r (layout:intersect clip (layout:translate (car p) (car rect) (cadr rect)))])
                    (do ([y (cadr r) (+ y 1)]) ((= y (+ (cadr r) (cadddr r))))
                      (do ([x (car r) (+ x 1)]) ((= x (+ (car r) (caddr r))))
                        (vector-set! (vector-ref rows (- y (cadr clip))) (- x (car clip)) (cadr p)))))) decorations)
      rows))
  (define (composite-styles clip cells children)
    ;; Prepared style rows are immutable. Full-width children can lend their
    ;; rows directly; only a partial overlay needs to copy the row it changes.
    (let ([rows (if (null? children) cells (vector-copy cells))])
      (for-each (lambda (child)
                  (let* ([c (frame-clip child)] [x (- (car c) (car clip))] [y (- (cadr c) (cadr clip))]
                         [count (length (frame-lines child))])
                    (do ([row 0 (+ row 1)]) ((= row count))
                      (let ([from (and (< row (vector-length (frame-cells child))) (vector-ref (frame-cells child) row))]
                            [at (+ row y)])
                        (if (and (zero? x) (= (caddr c) (caddr clip)))
                          (vector-set! rows at (or from (make-vector (caddr clip) #f)))
                          (let ([to (vector-copy (vector-ref rows at))])
                            (do ([col 0 (+ col 1)]) ((= col (caddr c)))
                              (vector-set! to (+ col x) (and from (vector-ref from col))))
                            (vector-set! rows at to))))))) children)
      rows))

  (edoc "Read a prepared row's styles as source-character styles for the TUI window adapter."
        (frame any "widget frame") (row integer "row index") (line string "the already selected display row") (returns any))
  (define (frame-styles frame row line)
    (and (<= 0 row) (< row (vector-length (frame-cells frame)))
      (let* ([cells (vector-ref (frame-cells frame) row)]
             [styles (make-vector (string-length line) #f)])
        (let loop ([parts (glyph:clusters line)] [char 0] [cell 0])
          (unless (null? parts)
            (do ([i char (+ i 1)]) ((= i (+ char (caar parts))))
              (vector-set! styles i (and (< cell (vector-length cells)) (vector-ref cells cell))))
            (loop (cdr parts) (+ char (caar parts)) (+ cell (cdar parts)))))
        styles)))

  (edoc "Read an immutable prepared row of backend cell styles, or false outside the frame. Borrowed rows must not be mutated."
        (frame any "widget frame") (row integer "row index") (returns any))
  (define (frame-cell-styles frame row)
    (and (<= 0 row) (< row (vector-length (frame-cells frame))) (vector-ref (frame-cells frame) row)))

  (edoc "The focused view's caret within this frame, in root backend coordinates, or #f when clipped or unavailable."
        (frame any "root frame") (returns any))
  (define (caret frame)
    (let* ([d (frame-descriptor frame)] [f (and d (view:focus d) (find-frame frame (view:focus d)))]
           [p (and f (frame-caret f))])
      (and p (layout:contains? (frame-clip f) (car p) (cdr p)) p)))

  (define (build-frame! id rect parent-clip)
    (let* ([n (mounted id)] [d (read-view id)] [entry (definition d)] [clip (layout:intersect rect parent-clip)])
      (let-values ([(available? source) (source! n d)])
        (define (placeholder text)
          (make-frame id d #f source (inputs! id) #f rect clip '()
            (if (or (zero? (caddr clip)) (zero? (cadddr clip))) '() (list (glyph:fit text (caddr clip))))
            (style-cells clip rect (list (list (list 0 0 (caddr rect) (cadddr rect)) 'ghost))) #f))
        (guard (ex [else
                    (let ([basis (list entry source)])
                      (unless (equal? basis (hashtable-ref failures id #f))
                        (hashtable-set! failures id basis) (echo:set-text! (kernel:condition-text ex))))
                    (placeholder (format "[Widget failed ~s]" id))])
          (cond
            [(or (zero? (caddr clip)) (zero? (cadddr clip))) (make-frame id d entry source (inputs! id) #f rect clip '() '() (make-vector 0) #f)]
            [(not (and entry available?)) (placeholder (format "[Unavailable widget ~s]" id))]
            [else
             (let* ([data (projection! n entry source)] [width (caddr rect)] [height (cadddr rect)]
                    [range (cons (- (cadr clip) (cadr rect)) (cadddr clip))]
                    [key (list entry source (inputs! id) d width height range (- (car clip) (car rect)) (caddr clip))]
                    [render (field entry 'render #f)] [layout (field entry 'layout #f)]
                    [placements (if layout (layout d width height measure! locate!) '())])
               (unless (and (list? placements)
                         (let check ([rest placements] [seen '()])
                           (or (null? rest) (let ([p (car rest)])
                                              (and (list? p) (= (length p) 2) (member (car p) (map cadr (view:children d)))
                                                (not (member (car p) seen)) (rectangle? (cadr p)) (check (cdr rest) (cons (car p) seen)))))))
                 (error 'prepare! "invalid child placements" id placements))
               (unless (equal? key (node-key n))
                 (let* ([visible ((field entry 'viewport (lambda (data d width height range) data)) data d width height range)]
                        [lines (if render (render visible d width height range) '())])
                   (node-visible-set! n visible)
                   (unless (and (list? lines) (for-all string? lines)) (error 'prepare! "expected display lines"))
                   (node-lines-set! n
                     (map (lambda (line) (glyph:slice line (- (car clip) (car rect)) (caddr clip)))
                       (list-head lines (min (cdr range) (length lines)))))
                   (node-styles-set! n (style-cells clip rect
                                         ((field entry 'decorate (lambda args '())) (node-visible n) d width height range)))
                   (node-caret-set! n ((field entry 'caret (lambda args #f)) (node-visible n) d width height))
                   (node-key-set! n key)))
               (let* ([children (map (lambda (p) (build-frame! (car p) (layout:translate (cadr p) (car rect) (cadr rect)) clip)) placements)]
                      [lines (if (and (null? children) (option d 'pass-through #f)) (node-lines n) (composite clip (node-lines n) children))]
                      [cells (composite-styles clip (node-styles n) children)]
                      [busy? (field entry 'busy? #f)])
                 (unless busy? (node-activity-set! n #f))
                 (when (and busy? (not (node-activity n)))
                   (node-activity-set! n (spinner:make (lambda () (current-time 'time-monotonic)) head:request-frame-at!)))
                 (let-values ([(lines cells) (if busy? (spinner:render! (node-activity n) (busy? (node-visible n) d) rect clip lines cells) (values lines cells))])
                   (make-frame id d entry source (inputs! id) (node-visible n) rect clip children lines cells
                     (let ([p (node-caret n)]) (and p (cons (+ (car rect) (car p)) (+ (cadr rect) (cdr p)))))))))])))))

  (edoc "Prepare a recursive frame for a root allocation. Geometry and borrowed source snapshots stay in the head; preparation does not make hits live."
        (id model "root view") (width integer "nonnegative backend width") (height integer "nonnegative backend height") (returns any))
  (define (prepare! id width height)
    (let ([rect (list 0 0 width height)])
      (define (geometry f) (and f (list (frame-id f) (frame-rect f) (frame-clip f) (map geometry (frame-children f)))))
      (define (build)
        (parameterize ([frame-reads (make-hashtable equal-hash equal?)]
                       [input-reads (make-hashtable equal-hash equal?)] [measurement-cache (make-hashtable equal-hash equal?)])
          (build-frame! id rect rect)))
      (unless (rectangle? rect) (error 'prepare! "invalid allocation" rect))
      (let* ([old (prepared id)] [frame (build)] [d (read-view id)] [before (and d (view:focus d))])
        (hashtable-set! preparations id frame)
        (ensure-focus! id)
        (let ([d (read-view id)])
          (unless (equal? before (and d (view:focus d)))
            (set! frame (build)) (hashtable-set! preparations id frame)))
        (unless (equal? (geometry old) (geometry frame)) (head:wake-main!))
        frame)))

  (edoc "Read the latest prepared frame, which may not have been displayed."
        (id model "mounted root or descendant") (returns any))
  (define (prepared id)
    (or (hashtable-ref preparations id #f)
      (let ([n (hashtable-ref nodes id #f)])
        (and n (find-frame (hashtable-ref preparations (mount-id (node-root n)) #f) id)))))

  (edoc "Adopt the exact frames whose output was successfully flushed. The painter calls this before publication hooks."
        (placements list "(frame screen-x screen-y) entries"))
  (define (present! placements)
    (set! presentations placements)
    (reconcile-input!))

  (edoc "Read shown placements; input must use these instead of prepared geometry." (returns list))
  (define (shown) presentations)

  (edoc "Invalidate uncertain output. Geometry-dependent input stays disabled until a successful full presentation.")
  (define (invalidate!) (defer-cancel! 'output-failure) (defer-leave!) (set! presentations '()))

  ;; Routing targets and event frames are dynamic head context, never wire data.
  (edoc "The explicit receiver of the current widget key binding or event, or #f." (returns any))
  (define target (make-parameter #f))

  (edoc "The shown frame supplying the current pointer event's source basis, or #f." (returns any))
  (define event-frame (make-parameter #f))
  (define pointer-capture #f)
  (define capture-button #f)
  (define discarded-button #f)
  (define pointer-event (make-parameter #f))
  (define hover-target #f)
  (define cancelled-gestures '())
  (define (defer-cancel! reason)
    (when pointer-capture
      (set! cancelled-gestures (cons (cons pointer-capture (list 'cancel reason)) cancelled-gestures))
      (set! discarded-button capture-button))
    (set! pointer-capture #f) (set! capture-button #f))
  (define (defer-leave!)
    (when hover-target
      (set! cancelled-gestures (cons (cons hover-target '(pointer leave none () 0 0)) cancelled-gestures)))
    (set! hover-target #f))
  (define (drain-cancels!)
    (let ([pending (reverse cancelled-gestures)])
      (set! cancelled-gestures '())
      (for-each (lambda (p)
                  (let* ([f (car p)] [entry (frame-definition f)] [handler (field entry 'event #f)])
                    (when (and handler (eq? entry (definition (frame-descriptor f))))
                      (guard (ex [else (echo:set-text! (kernel:condition-text ex))])
                        (parameterize ([target (frame-id f)] [event-frame f])
                          (handler (frame-id f) (frame-source f) (frame-descriptor f) (cdr p))))))) pending)))
  (define last-focus (make-hashtable equal-hash equal?))
  (define (live-frame? f)
    (let* ([n (hashtable-ref nodes (frame-id f) #f)] [d (and n (read-view (frame-id f)))])
      (and d (frame-descriptor f) (eq? (frame-definition f) (definition d))
        (= (view:generation d) (view:generation (frame-descriptor f))))))
  (define (shown-root id)
    (exists (lambda (p) (and (equal? id (frame-id (car p))) (car p))) presentations))
  (define (visible? f)
    (and (> (caddr (frame-clip f)) 0) (> (cadddr (frame-clip f)) 0) (live-frame? f)))
  (define (modal f)
    (and (visible? f) (or (exists modal (reverse (frame-children f)))
                        (and (option (frame-descriptor f) 'modal #f) f))))
  (define (eligible-pointer? f)
    (exists (lambda (p)
              (let* ([root (car p)] [current (find-frame (or (modal root) root) (frame-id f))])
                (and current (live-frame? f) (visible? current)))) presentations))
  (define (reconcile-input!)
    ;; Presentation cannot run extension callbacks. Queue lifecycle notices
    ;; for the pump, but make stale targets ineligible immediately.
    (when (and pointer-capture (not (eligible-pointer? pointer-capture))) (defer-cancel! 'hidden))
    (when (and hover-target (not (eligible-pointer? hover-target))) (defer-leave!)))
  (define (focus-order f)
    (if (or (zero? (caddr (frame-clip f))) (zero? (cadddr (frame-clip f)))) '()
      (append (if (field (frame-definition f) 'focus #f) (list (frame-id f)) '())
        (apply append (map focus-order (frame-children f))))))
  (define (focusable f)
    (filter (lambda (id) (live-frame? (find-frame f id))) (focus-order f)))
  (define (path id)
    (let loop ([id id] [out '()])
      (let ([d (and id (read-view id))])
        (if d (loop (view:parent d) (cons id out)) out))))
  (define (send! id event frame . phase)
    (let* ([n (hashtable-ref nodes id #f)] [d (and n (read-view id))] [entry (definition d)]
           [handler (field entry (if (pair? phase) (car phase) 'event) #f)])
      (and handler (or (not frame) (live-frame? frame))
        (let-values ([(available? source) (source! n d)])
          (and available?
            (parameterize ([target id] [event-frame frame])
              (and (handler id (if frame (frame-source frame) source) d event) #t)))))))
  (define (focus-frame root)
    (let ([f (if (event-frame) (shown-root root) (or (prepared root) (shown-root root)))]) (and f (or (modal f) f))))

  (edoc "Focus a visible accepting descendant within the root's current modal scope. Hidden roots retain their remembered target."
        (root model "root view") (id model "descendant view"))
  (define (focus! root id)
    (let* ([frame (focus-frame root)] [d (read-view root)] [old (and d (view:focus d))])
      (unless (and frame d (member id (focusable frame))) (error 'focus! "target is not focusable here" root id))
      (unless (equal? old id)
        (let common ([before (path old)] [after (path id)])
          (if (and (pair? before) (pair? after) (equal? (car before) (car after))) (common (cdr before) (cdr after))
            (begin
              (for-each (lambda (id) (send! id '(blur) #f)) (reverse before))
              (interaction:focus! root id)
              (for-each (lambda (id) (send! id '(focus) #f)) after))))
        ;; Focus can affect sibling presentation too (for example, a table
        ;; with its filter focused). Reuse projections, but rebuild faces.
        (for-each repaint! (mount-ids (node-root (mounted root))))
        (hashtable-set! last-focus root id) (head:wake-main!))))
  (define (ensure-focus! root)
    (let* ([frame (focus-frame root)] [choices (if frame (focusable frame) '())]
           [d (read-view root)] [old (and d (view:focus d))]
           [previous (or old (hashtable-ref last-focus root #f))]
           [past (let ([old-frame (shown-root root)]) (if old-frame (focus-order old-frame) '()))]
           [tail (and previous (member previous past))]
           [next (or (and (member old choices) old)
                   (and tail (find (lambda (id) (member id choices)) (cdr tail)))
                   (and tail (find (lambda (id) (member id choices)) (reverse (list-head past (- (length past) (length tail))))))
                   (and (pair? choices) (car choices)))])
      (cond
        [(and frame (or (zero? (caddr (frame-clip frame))) (zero? (cadddr (frame-clip frame))))) #f]
        [next (focus! root next) next]
        [else (when (and d old) (interaction:focus! root #f)) #f])))

  (edoc "Cycle visible accepting descendants within the active modal scope, wrapping in tree order."
        (id model "root or descendant view") (backward (list-of boolean) "reverse direction, at most one") (returns any))
  (define (focus-next! id . backward)
    (unless (and (<= (length backward) 1) (for-all boolean? backward)) (error 'focus-next! "expected an optional boolean" backward))
    (let* ([root (mount-id (node-root (mounted id)))] [old (ensure-focus! root)]
           [frame (focus-frame root)] [choices (if frame (focusable frame) '())]
           [choices (if (and (pair? backward) (car backward)) (reverse choices) choices)]
           [tail (member old choices)] [next (and (pair? choices) (if (and tail (pair? (cdr tail))) (cadr tail) (car choices)))])
      (when next (focus! root next)) next))

  (edoc "Build ordered keymap receivers and a chord ownership basis for this root. The outer host may append its own contexts."
        (root model "active root") (key string "first key token") (returns list "(basis scopes focused-view)"))
  (define (key-scopes! root key)
    (drain-cancels!)
    (ensure-focus! root)
    (key-scopes root key))

  ;; Routing policies read only already acquired descriptors/service state.
  (define (routing-policy id d name fallback)
    (let* ([provider (field (definition d) name fallback)]
           [value (if (procedure? provider) (provider id d) provider)])
      (unless (case name
                [(capture) (memq value '(full partial))]
                [(yield) (and (list? value) (for-all string? value))]
                [else (and (list? value) (for-all symbol? value))])
        (error 'routing-policy "invalid routing policy" id name value)) value))
  (define (scope-path ids barrier)
    (if barrier (or (member barrier ids) '()) ids))

  (edoc "Read the remembered focus's key routing without changing focus or consuming a pending chord. Includes capture, yield and modal boundaries; hosts can append their contexts."
        (root model "mounted root") (key string "first key token, or empty for all contexts") (returns list "(basis scopes focused-view)") (effects internal))
  (define (key-scopes root key)
    (let* ([d (read-view root)] [focus (and d (view:focus d))] [scope (focus-frame root)]
           [path (if focus (path focus) (if scope (path (frame-id scope)) (list root)))] [barrier (and scope (option (frame-descriptor scope) 'modal #f) (frame-id scope))]
           [normal (let loop ([rest (reverse path)] [out '()])
                     (if (null? rest) (reverse out)
                       (let* ([id (car rest)] [d (read-view id)] [entry (definition d)]
                              [full? (eq? (routing-policy id d 'capture 'partial) 'full)]
                              [yield? (member key (routing-policy id d 'yield '()))]
                              [provider (field entry 'contexts '())]
                              [contexts (if yield? '() (if (procedure? provider) (provider id d) provider))]
                              [item (list id (if (or (equal? id root) (equal? id barrier)) (append contexts '(widget-host)) contexts)
                                      (or (and full? (not yield?)) (equal? id barrier)))])
                         (unless (and (list? contexts) (for-all symbol? contexts))
                           (error 'key-scopes "expected context symbols" id contexts))
                         (if (equal? id barrier) (reverse (cons item out)) (loop (cdr rest) (cons item out))))))]
           [captures (filter (lambda (scope) (pair? (cadr scope)))
                       (map (lambda (id)
                              (let* ([d (read-view id)] [full? (eq? (routing-policy id d 'capture 'partial) 'full)]
                                     [yield? (member key (routing-policy id d 'yield '()))])
                                (list id (if yield? '() (routing-policy id d 'capture-contexts '())) (and full? (not yield?)))))
                         (scope-path path barrier)))]
           [basis (map (lambda (id) (let ([d (read-view id)]) (list id (and d (view:generation d)) (definition d)))) path)])
      (list (list root focus barrier (let ([d (read-view root)]) (and d (view:sequence d))) basis normal captures) (append captures normal) focus)))

  (edoc "Offer committed text or an unbound normalized key to the focused path; a full capture or modal boundary stops bubbling."
        (root model "active root") (event list "(text string typed-or-paste), (key token), or cancellation") (returns boolean))
  (define (input! root event)
    (drain-cancels!)
    (let* ([focus (ensure-focus! root)] [scope (focus-frame root)]
           [barrier (and scope (option (frame-descriptor scope) 'modal #f) (frame-id scope))])
      (let ([ids (scope-path (if focus (path focus) (if scope (path (frame-id scope)) (list root))) barrier)])
        (define (yield? id d) (and (eq? (car event) 'key) (member (cadr event) (routing-policy id d 'yield '()))))
        (or (exists (lambda (id) (and (not (yield? id (read-view id))) (send! id event #f 'capture-event))) ids)
          (let loop ([ids (reverse ids)])
            (and (pair? ids)
              (let* ([id (car ids)] [d (read-view id)] [entry (definition d)])
                (or (and (not (yield? id d))
                      (or (send! id (if (eq? (car event) 'key) (list-head event 2) event) #f)
                        (and (eq? (car event) 'key) (= (length event) 3) (caddr event)
                          (send! id (list 'text (caddr event) 'typed) #f))))
                  (and (not (yield? id d)) (eq? (routing-policy id d 'capture 'partial) 'full)) (equal? id barrier) (loop (cdr ids))))))))))

  (edoc "Capture pointer motion and release for the current shown press target, including outside its allocation."
        (id model "current event receiver"))
  (define (capture! id)
    (let ([f (event-frame)] [e (pointer-event)])
      (unless (and f e (eq? (cadr e) 'press) (not (eq? (caddr e) 'none))
                (equal? id (frame-id f)) (live-frame? f))
        (error 'capture! "capture requires a live shown press target" id))
      (set! pointer-capture f) (set! capture-button (caddr e))))

  (edoc "Cancel a root's pointer gesture when its host hides, blurs or loses the device; retain logical focus."
        (root model "root") (reason symbol "cancellation reason"))
  (define (cancel! root reason)
    (let ([n (and pointer-capture (hashtable-ref nodes (frame-id pointer-capture) #f))])
      (when (and n (equal? root (mount-id (node-root n)))) (defer-cancel! reason)))
    (let ([n (and hover-target (hashtable-ref nodes (frame-id hover-target) #f))])
      (when (and n (not (memq reason '(blur keyboard))) (equal? root (mount-id (node-root n)))) (defer-leave!)))
    (drain-cancels!))
  (define (hit f x y)
    (and (layout:contains? (frame-clip f) x y)
      (or (exists (lambda (child) (hit child x y)) (reverse (frame-children f)))
        (and (or (option (frame-descriptor f) 'modal #f) (not (option (frame-descriptor f) 'pass-through #f))) f))))

  (define (pointer-location x y captured)
    (let* ([placement (if captured
                        (find (lambda (p) (find-frame (car p) (frame-id pointer-capture))) presentations)
                        (find (lambda (p) (layout:contains? (frame-clip (car p)) (- x (cadr p)) (- y (caddr p)))) (reverse presentations)))]
           [root (and placement (car placement))]
           [x (if placement (- x (cadr placement)) x)] [y (if placement (- y (caddr placement)) y)])
      (and root (list root (if captured (find-frame root (frame-id pointer-capture)) (hit (or (modal root) root) x y)) x y))))

  (edoc "Read mouse commands at a shown screen position without delivering input. Return #f outside widgets, otherwise (gesture action) pairs from the target's pointer-bindings callback and its ancestors. Gestures are (click-or-drag button modifiers) or (wheel direction modifiers). Callbacks receive a shown frame and local x/y and must only read local state. Nearer gestures shadow ancestor gestures; modal boundaries and clipping apply."
        (x integer "zero-based screen column") (y integer "zero-based screen row") (returns any) (effects internal))
  (define (pointer-bindings x y)
    (let ([at (pointer-location x y #f)])
      (and at
        (let* ([root (car at)] [f (cadr at)] [scope (or (modal root) root)])
          (if (not (and f (live-frame? f))) '()
            (let* ([ids (scope-path (path (frame-id f)) (frame-id scope))]
                   [phases (append (map (lambda (id) (cons id 'capture-pointer-bindings)) ids)
                             (map (lambda (id) (cons id 'pointer-bindings)) (reverse ids)))])
              (let loop ([ids phases] [out '()] [scroll? #f])
                (if (null? ids)
                  (append out (if scroll?
                                (list (list '(wheel up ()) (keymap:call pointer! '(scroll 0 -3 cells) x y))
                                  (list '(wheel down ()) (keymap:call pointer! '(scroll 0 3 cells) x y))) '()))
                  (let* ([frame (find-frame scope (caar ids))] [definition (and frame (frame-definition frame))]
                         [describe (field definition (cdar ids) #f)]
                         [bindings (if (and describe (live-frame? frame))
                                     (describe frame (- (caddr at) (car (frame-rect frame))) (- (cadddr at) (cadr (frame-rect frame)))) '())])
                    (loop (cdr ids)
                      (append out (filter (lambda (b) (not (assoc (car b) out))) bindings))
                      (or scroll? (and definition (assq 'scroll (field definition 'actions '()))))))))))))))

  (edoc "Route normalized pointer/scroll input through shown frames. Return (root focus-host?) when consumed, or #f outside widgets."
        (event list "(pointer phase button modifiers [click-count]) or (scroll dx dy units)")
        (x integer "screen x, zero based") (y integer "screen y, zero based") (returns any))
  (define (pointer! event x y)
    (reconcile-input!)
    (drain-cancels!)
    (cond
      [(and (eq? (car event) 'pointer) (eq? (cadr event) 'press))
       (when pointer-capture (defer-cancel! 'new-press) (drain-cancels!))
       (set! discarded-button #f)
       (route-pointer! event x y)]
      [(and discarded-button (eq? (car event) 'pointer)
         (or (eq? (caddr event) discarded-button) (eq? (caddr event) 'none)))
       (when (eq? (cadr event) 'release) (set! discarded-button #f))
       '(#f #f)]
      [else (route-pointer! event x y)]))
  (define (route-pointer! event x y)
    (retain-focus? #f)
    (let* ([captured (and pointer-capture (live-frame? pointer-capture) (not (eq? (car event) 'scroll)))]
           [at (pointer-location x y captured)] [root (and at (car at))]
           [x (if at (caddr at) x)] [y (if at (cadddr at) y)] [f (and at (cadr at))])
      (unless captured (when pointer-capture (defer-cancel! 'stale-target)))
      (when (and (eq? (car event) 'pointer) (eq? (cadr event) 'move))
        (unless (and hover-target f (equal? (frame-id hover-target) (frame-id f)))
          (when hover-target (send! (frame-id hover-target) '(pointer leave none () 0 0) hover-target))
          (set! hover-target f)))
      (and root
        (begin
          (when (and f (live-frame? f))
            (when (and (eq? (car event) 'pointer) (eq? (cadr event) 'press) (field (frame-definition f) 'focus #f))
              (parameterize ([event-frame f]) (focus! (frame-id root) (frame-id f))))
            (unless
              (let ([scope (or (modal root) root)])
                (exists (lambda (id)
                          (let ([target (find-frame scope id)])
                            (and target
                              (parameterize ([pointer-event event])
                                (send! id
                                  (append (list-head event 4)
                                    (list (- x (car (frame-rect target))) (- y (cadr (frame-rect target))))
                                    (cddddr event)) target 'capture-event)))))
                  (scope-path (path (frame-id f)) (frame-id scope))))
              (if (eq? (car event) 'scroll)
                (let loop ([ids (reverse (path (frame-id f)))] [left (caddr event)])
                  (unless (or (null? ids) (zero? left))
                    (let* ([id (car ids)] [scope (or (modal root) root)] [d (read-view id)] [entry (definition d)]
                           [scroll? (assq 'scroll (field entry 'actions '()))])
                      (when (find-frame scope id) (loop (cdr ids) (if scroll? (act! id 'scroll left) left))))))
                (begin
                  (when (or (not captured) (eq? (caddr event) capture-button))
                    (let ([scope (or (modal root) root)])
                      (let loop ([ids (reverse (path (frame-id f)))])
                        (unless (null? ids)
                          (let ([target (find-frame scope (car ids))])
                            (when target
                              (unless (parameterize ([pointer-event event])
                                        (send! (frame-id target)
                                          (append (list-head event 4)
                                            (list (- x (car (frame-rect target))) (- y (cadr (frame-rect target))))
                                            (cddddr event)) target))
                                (loop (cdr ids)))))))))))))
          (when (and (eq? (car event) 'pointer) (eq? (cadr event) 'release) (eq? (caddr event) capture-button))
            (set! pointer-capture #f) (set! capture-button #f))
          (list (frame-id root) (and (not (retain-focus?)) f (eq? (car event) 'pointer) (eq? (cadr event) 'press)
                                     (field (frame-definition f) 'focus #f)))))))

  ;; The minimal text widget deliberately consumes arbitrary model values.
  ;; It needs no extra base dataset service or evaluator allocation.
  (define (text-data id model inputs)
    (let ([value (cdr (assq 'value model))])
      (cons (cdr (assq 'revision model)) (list->vector (string:lines (if (string? value) value (format "~s" value)))))))
  (define (text-source! id source descriptor)
    (cdr (projection! (mounted id) (definition descriptor) source)))
  (define (text-state state count)
    (if (and (integer? state) (exact? state)) (max 0 (min (- count 1) state)) 0))
  (define (text-render data descriptor width height range)
    (define state (view:state descriptor))
    (let* ([data (cdr data)] [count (vector-length data)] [selected (text-state state count)] [top (min count (car range))])
      (map (lambda (i) (let ([row (+ top i)])
                         (string-append (if (= row selected) "> " "  ") (vector-ref data row)))) (iota (min (cdr range) (- count top))))))
  (define (text-move! id offset)
    (let-values ([(source d inputs) (context id)])
      (let* ([data (text-source! id source d)]
             [count (vector-length data)]
             [row (text-state
                    (+ (text-state (view:state d) count) offset)
                    count)])
        (interaction:set-state!
          head:ui-actor
          id
          (cdr (assq 'revision source))
          row)
        (reveal! id (list (cdr (assq 'revision source)) row)))))
  (define (text-select! id row)
    (let-values ([(source d inputs) (context id)])
      (interaction:set-state!
        head:ui-actor
        id
        (cdr (assq 'revision source))
        (text-state
          row
          (vector-length (text-source! id source d))))))
  (define (text-choose! id)
    (let-values ([(source d inputs) (context id)])
      (let* ([data (text-source! id source d)]
             [row (text-state (view:state d) (vector-length data))]
             [text (vector-ref data row)])
        (echo:set-text! text)
        (list
          (view:source d)
          (cdr (assq 'revision source))
          row
          text))))
  (define (text-event! id source d event)
    (and (eq? (car event) 'pointer) (eq? (cadr event) 'press) (eq? (caddr event) 'primary)
      (begin (keymap:run! (cadar (text-pointer-bindings (event-frame) (list-ref event 4) (list-ref event 5)))) #t)))
  (define (text-pointer-bindings frame x y)
    (list (list '(click primary ()) (keymap:call act! (frame-id frame) 'select y))))

  (edoc "Reveal a logical source anchor in its nearest containing scroll viewport, without changing selection."
        (id model "descendant") (anchor datum "source anchor"))
  (define (reveal! id anchor)
    (hashtable-delete! pending-reveal id)
    (let loop ([child id] [rest (cdr (reverse (path id)))] [place anchor])
      (unless (null? rest)
        (let* ([parent (car rest)] [d (read-view parent)] [f (allocation parent)])
          (if (and f (eq? (view:kind d) 'scroll))
            (let* ([width (caddr (frame-rect f))] [height (cadddr (frame-rect f))]
                   [point (locate! child place width)] [top (locate! child (view:state d) width)]
                   [delta (cond [(not (and point top)) 0] [(< point top) (- point top)] [(>= point (+ top height)) (+ 1 (- point top height))] [else 0])])
              (if (not (and point top)) (hashtable-set! pending-reveal id anchor)
                (unless (zero? delta) (act! parent 'scroll delta))))
            (loop parent (cdr rest) (list 'child child place '() '())))))))

  (edoc "Install the text definition and renderer invalidation." (public))
  (define (init!)
    (keymap:bind-default! 'widget-host "TAB" (keymap:call focus-next! target))
    (keymap:bind-default! 'widget-host "S-TAB" (keymap:call focus-next! target #t))
    (register! 'text 2 (list (cons 'prepare text-data) (cons 'render text-render)
                         (cons 'measure (lambda (data descriptor axis cross measure) (if (eq? axis 'y) (list 1 (vector-length (cdr data))) '(1 1))))
                         (cons 'locate (lambda (data anchor width)
                                         (if (and (list? anchor) (= (length anchor) 2) (equal? (car anchor) (car data)) (integer? (cadr anchor))) (max 0 (cadr anchor)) 0)))
                         (cons 'anchor (lambda (data position width) (list (car data) position))) (cons 'focus #t)
                         (cons 'contexts '(widget-text)) (cons 'event text-event!) (cons 'pointer-bindings text-pointer-bindings)
                         (cons 'actions (list (cons 'move text-move!) (cons 'select text-select!) (cons 'choose text-choose!)))))
    (keymap:bind-default! 'widget-text "UP" (keymap:call act! target 'move -1))
    (keymap:bind-default! 'widget-text "DOWN" (keymap:call act! target 'move 1))
    (keymap:bind-default! 'widget-text "RET" (keymap:call act! target 'choose))
    (register! 'row 1 (layout:container 'x))
    (register! 'column 1 (layout:container 'y))
    (register! 'overlay 1 (list (cons 'layout overlay-layout) (cons 'measure overlay-measure)))
    (register! 'scroll 1 (list (cons 'layout scroll-layout) (cons 'measure overlay-measure) (cons 'actions (list (cons 'scroll scroll-action!)))))
    (head:add-pre-redraw-hook! drain-cancels!)
    (head:add-pre-redraw-hook! pump!)
    (kernel:registry-observe! definitions
      (lambda (removed added)
        (when (and pointer-capture
                (or (not (live-frame? pointer-capture))
                  (exists (lambda (command)
                            (let ([d (read-view (cadr command))])
                              (and d (assoc (list (view:kind d) (view:schema d)) removed))))
                    (descriptor:commands (frame-descriptor pointer-capture))))) (defer-cancel! 'reload))
        (head:wake-main!))))
)
