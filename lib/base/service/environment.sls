;; Native namespaces are supervised resources; their recipes and jobs are models.
(import (only (foundation edoc) elibrary))
(elibrary (service environment)
  (export cancel! close! completion create! evaluate! for-document! release! reset! restore! stop!)
  (import (chezscheme) (prefix (core handle) handle:) (prefix (core kernel) kernel:) (prefix (core worker) worker:)
    (prefix (foundation datum) datum:) (prefix (foundation text) text:)
    (prefix (state model) model:) (prefix (state store) store:) (prefix (sys activity) activity:))

  (define (get r k) (cdr (assq k r)))
  (define (put r . changes) (map (lambda (p) (or (assq (car p) changes) p)) r))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (fields? v keys) (and (list? v) (for-all pair? v) (equal? (map car v) keys)))
  (define (absolute? s) (and (string? s) (> (string-length s) 0) (char=? (string-ref s 0) #\/)))
  (define (named? xs) (and (list? xs) (for-all (lambda (p) (and (pair? p) (symbol? (car p)))) xs)
                        (= (length xs) (length (fold-left (lambda (out p) (if (memq (car p) out) out (cons (car p) out))) (quote ()) xs)))))
  (define (resource? r) (or (model:reference? r)
                          (and (list? r) (= (length r) 2) (eq? (car r) 'buffer)
                            (natural? (cadr r)) (> (cadr r) 0))))
  (define (import? spec)
    (and (list? spec) (pair? spec)
      (if (memq (car spec) '(only except prefix rename for))
        (and (pair? (cdr spec)) (import? (cadr spec)))
        ;; External libraries and Chez/R6RS are ordinary imports. The resource
        ;; broker is the only editor implementation library in a worker recipe.
        (or (equal? spec '(service resource))
          (not (memq (car spec) '(foundation sys core state service head apps modes run)))))))
  (define (recipe input)
    (let ([r (datum:copy input)])
      (unless (and (named? r) (for-all (lambda (p) (memq (car p) '(directory roots imports values resources))) r)
                (for-all (lambda (k) (assq k r)) '(directory roots imports)))
        (error 'create! "expected directory, roots, imports and optional values/resources"))
      (let ([r (map (lambda (k) (or (assq k r) (list k))) '(directory roots imports values resources))])
        (unless (and (absolute? (get r 'directory)) (list? (get r 'roots))
                  (for-all absolute? (get r 'roots)) (list? (get r 'imports))
                  (for-all import? (get r 'imports)) (named? (get r 'values))
                  (named? (get r 'resources)) (for-all (lambda (p) (resource? (cdr p))) (get r 'resources)))
          (error 'create! "invalid environment recipe")) r)))
  (define (environment-value? v)
    (and (or (fields? v '(recipe generation status catalogue count notice))
             (and (fields? v '(recipe generation status catalogue count notice document))
                  (handle:buffer? (get v 'document))))
      (natural? (get v 'generation)) (natural? (get v 'catalogue)) (natural? (get v 'count))
      (memq (get v 'status) '(idle running reset))
      (guard (ex [else #f]) (equal? (recipe (get v 'recipe)) (get v 'recipe)))))
  (define (job? v)
    (and (or (fields? v '(environment generation source status output channels result diagnostic))
             (fields? v '(environment generation source status output channels result diagnostic projection)))
      (model:reference? (get v 'environment)) (natural? (get v 'generation))
      (string? (get v 'source)) (resource? (get v 'output))
      (memq (get v 'status) '(queued running ok error cancelled reset))))
  (define registration
    (kernel:call-with-runtime-registrations
      (lambda () (model:register-kind! 'environment 1 environment-value?)
        (model:register-kind! 'evaluation-job 1 job?))))
  (define (record id kind)
    (let ([r (model:snapshot id)]) (and r (eq? (get r 'kind) kind) (= (get r 'schema) 1) r)))
  (define (update! actor r value)
    (let-values ([(status rows) (model:commit! actor
                                  (list (list (get r 'id) (get r 'revision) (get r 'references) value)))])
      (unless (eq? status 'applied) (error 'environment "model changed outside its owning service")) value))
  (define-record-type group
    (fields id lock (mutable worker) (mutable initialized?) (mutable generation) (mutable queue) (mutable running?)
      (mutable active) (mutable catalogue) (mutable names) (mutable pages) (mutable releases)))
  (define groups (make-eqv-hashtable))
  (define groups-lock (make-mutex))
  (define (group-for id)
    (with-mutex groups-lock
      (or (hashtable-ref groups (cadr id) #f)
        (let ([r (record id 'environment)])
          (and r (let ([g (make-group id (make-mutex) #f #f (get (get r 'value) 'generation)
                            '() #f #f 0 '#() '() '())])
                   (hashtable-set! groups (cadr id) g) g))))))
  (define (admit g generation thunk)
    (activity:call-with
      (lambda ()
        (kernel:call-with-deferred-deliveries
          (lambda () (with-mutex (group-lock g)
                       (unless (and (= generation (group-generation g)) (record (group-id g) 'environment))
                         (error 'environment "retired environment generation")) (thunk)))))))
  (define (jobs id)
    (filter (lambda (r) (and r (equal? (get (get r 'value) 'environment) id)))
      (map (lambda (id) (record id 'evaluation-job)) (model:ids 'evaluation-job))))
  (define (status! actor id status diagnostic)
    (let ([r (record id 'evaluation-job)])
      (when r (update! actor r (put (get r 'value) (cons 'status status) (cons 'diagnostic diagnostic))))))
  (define (diagnostic kind message) (list (cons 'kind kind) (cons 'message message)))
  (define (reset-group! actor g notice)
    ;; Caller admitted and owns the gate. Fence before killing outside the gate.
    (let* ([r (record (group-id g) 'environment)] [next (+ 1 (group-generation g))]
           [old (group-worker g)])
      (group-generation-set! g next) (group-worker-set! g #f)
      (group-initialized?-set! g #f)
      (group-queue-set! g '()) (group-active-set! g #f)
      (group-catalogue-set! g 0) (group-names-set! g '#()) (group-pages-set! g '()) (group-releases-set! g '())
      (for-each (lambda (j)
                  (let* ([v (get j 'value)] [result (get v 'result)]
                         [pending? (memq (get v 'status) '(queued running))])
                    (when (or pending? (and (pair? result) (eq? (car result) 'handle)))
                      (update! actor j (put v (cons 'status (if pending? 'reset (get v 'status)))
                                         (cons 'result (if (and (pair? result) (eq? (car result) 'handle))
                                                         (list 'expired (list-ref result 3)) result))
                                         (cons 'diagnostic (diagnostic 'reset notice))))))) (jobs (group-id g)))
      (when r (update! actor r (put (get r 'value) (cons 'generation next) (cons 'status 'reset)
                                 '(catalogue . 0) '(count . 0) (cons 'notice notice)))) old))
  (define (append-output! actor job channel chunk)
    (let* ([r (record job 'evaluation-job)] [v (get r 'value)] [id (get v 'output)])
      (let-values ([(lines revision facts) (store:snapshot-state id)])
        (let* ([row (- (vector-length lines) 1)] [column (string-length (vector-ref lines row))]
               [runs (get v 'channels)]
               [same? (and (pair? runs) (eq? (caar runs) channel))]
               [runs (if same? runs (cons (list channel row column) runs))])
          (let-values ([(added trailing?) (text:from-string chunk)])
            (let-values ([(status detail)
                          (store:edit! actor id revision (text:make-span row column row column)
                            (append (vector->list added) (if trailing? '("") '()))
                            (list job "evaluation output" (cons 'revision revision)))])
              (unless (eq? status 'applied) (error 'environment "output changed outside its owner"))
              (unless same? (update! actor r (put v (cons 'channels runs))))))))))
  (define (broker g generation actor operation name . args)
    (admit g generation
      (lambda ()
        (let* ([r (record (group-id g) 'environment)] [entry (assq name (get (get (get r 'value) 'recipe) 'resources))]
               [ref (and entry (cdr entry))])
          (unless ref (error 'resource "resource is not declared" name))
          (case (car ref)
            [(buffer)
             (let ([state (store:state ref #f '())])
               (unless state (error 'resource "document is unavailable"))
               (case operation
                 [(read) (unless (null? args) (error 'resource "invalid read")) (list (caddr state) (cadr state))]
                 [(edit)
                  (unless (= (length args) 3) (error 'resource "invalid edit"))
                  (if (not (equal? (car args) (caddr state))) '(stale)
                    (call-with-values
                      (lambda ()
                        (store:edit! actor ref (car args) (text:datum->span (cadr args))
                          (caddr args)
                          (list
                            (group-active g)
                            "evaluation"
                            (cons 'revision (car args)))
                          'any)) list))]
                 [else (error 'resource "operation requires a data model")]))]
            [(model)
             (let ([r (model:snapshot ref)])
               (unless r (error 'resource "model is unavailable"))
               (case operation
                 [(read) (unless (null? args) (error 'resource "invalid read")) (list (get r 'revision) (get r 'value))]
                 [(commit)
                  (unless (= (length args) 2) (error 'resource "invalid commit"))
                  (when (memq (get r 'kind) '(history history-item environment evaluation-job widget-view collection buffer-catalogue
                                               connection-topology connection-bindings prompt-request))
                    (error 'resource "use the model's owning service"))
                  (call-with-values (lambda () (model:commit! actor
                                                 (list (list ref (car args) (get r 'references) (cadr args)))))
                    (lambda (status rows) (list status)))]
                 [else (error 'resource "operation requires a document")]))])))))
  (define (run-job! g generation job)
    (let* ([j (record job 'evaluation-job)] [actor (get j 'actor)]
           [w (admit g generation
                (lambda ()
                  (status! actor job 'running #f)
                  (let ([r (record (group-id g) 'environment)])
                    (update! actor r (put (get r 'value) '(status . running) '(notice . #f))))
                  (or (group-worker g) (let ([w (worker:open!)]) (group-worker-set! g w) w))))]
           [initialized? (group-initialized? g)])
      (define (emit message)
        (admit g generation
          (lambda ()
            (case (car message)
              [(output) (append-output! actor job (cadr message) (caddr message))]
              [(catalogue)
               (when (zero? (caddr message)) (group-pages-set! g '()))
               (group-pages-set! g (cons (cadddr message) (group-pages g)))]))))
      (define (request command) (worker:request! w command emit (lambda args (apply broker g generation actor args))))
      (define (catalogue! result)
        (admit g generation
          (lambda ()
            (unless (= (group-catalogue g) (list-ref result 4))
              (group-names-set! g (list->vector (apply append (reverse (group-pages g)))))
              (group-catalogue-set! g (list-ref result 4)) (group-pages-set! g '())
              (let ([r (record (group-id g) 'environment)])
                (update! actor r (put (get r 'value) (cons 'catalogue (group-catalogue g))
                                   (cons 'count (vector-length (group-names g))))))))))
      (let ([initial (and (not initialized?)
                       (request (list 'initialize (get (get (record (group-id g) 'environment) 'value) 'recipe))))])
        (when initial
          (catalogue! initial)
          (admit g generation (lambda () (group-initialized?-set! g (eq? (cadr initial) 'ok)))))
        (let* ([result (if (and initial (not (eq? (cadr initial) 'ok))) initial
                         (request (list 'evaluate (get (get j 'value) 'source)
                                    (cond [(assq 'projection (get j 'value)) => cdr] [else #f]))))]
               [value (caddr result)])
          (catalogue! result)
          (admit g generation
            (lambda ()
              (let ([r (record job 'evaluation-job)])
                (update! actor r (put (get r 'value) (cons 'status (cadr result))
                                   (cons 'result (if (and value (eq? (car value) 'handle))
                                                   (list 'handle generation (cadr value) (caddr value)) value))
                                   (cons 'diagnostic (cadddr result))))))))
        (when (and initial (not (eq? (cadr initial) 'ok)))
          (let ([old (admit g generation
                       (lambda () (reset-group! actor g "Recipe initialization failed; live bindings were reset")))])
            (when old (worker:close! old)))))))
  (define (drive! g)
    (let loop ()
      (let ([work
             (activity:call-with
               (lambda () (kernel:call-with-deferred-deliveries (lambda () (with-mutex (group-lock g)
                                                                             (cond
                                                                               [(and (group-worker g) (pair? (group-releases g)))
                                                                                (let ([ids (group-releases g)])
                                                                                  (group-releases-set! g '())
                                                                                  (list (group-generation g) #f (group-worker g) ids))]
                                                                               [(pair? (group-queue g))
                                                                                (let ([job (car (group-queue g))])
                                                                                  (group-queue-set! g (cdr (group-queue g))) (group-active-set! g job)
                                                                                  (list (group-generation g) job))]
                                                                               [else
                                                                                (group-running?-set! g #f) (group-active-set! g #f)
                                                                                (let ([r (record (group-id g) 'environment)])
                                                                                  (when (and r (eq? (get (get r 'value) 'status) 'running))
                                                                                    (update! '(base e) r (put (get r 'value) '(status . idle))))) #f]))))))])
        (when work
          (guard (ex [else
                      (let ([old (guard (ignored [else #f])
                                   (admit g (car work)
                                     (lambda ()
                                       (let ([message (kernel:condition-text ex)])
                                         (reset-group! '(base e) g
                                           (string-append "Worker failed; live bindings were reset: "
                                             (substring message 0 (min 1024 (string-length message)))))))))])
                        (when old (worker:close! old)))])
            (if (cadr work) (run-job! g (car work) (cadr work))
              (worker:request! (caddr work) (list 'release (cadddr work)) void
                (lambda args (error 'environment "unexpected resource request during release")))))
          (loop)))))
  (define (start! g)
    (unless (group-running? g)
      (group-running?-set! g #t)
      (fork-thread (lambda () (guard (ex [else (void)]) (drive! g))))))

  (edoc "Create a lazy Scheme environment from explicit imports, absolute roots/directory, copied values and named borrowed resources. No process starts until evaluation. Persistence saves recipes and portable outcomes, never native bindings or an evaluation replay."
    (actor actor "creator") (input list "imports, roots, directory; optional values and resources alists")
    (persistence (one-of transient persistent) "recovery policy") (returns model))
  (define (create! actor input persistence)
    (let ([r (recipe input)])
      (model:create! actor 'environment 1 'session persistence (map cdr (get r 'resources))
        (list (cons 'recipe r) '(generation . 1) '(status . idle) '(catalogue . 0) '(count . 0) '(notice . #f)))))

  (define document-lock (make-mutex))

  (edoc "Get the persistent isolated environment for an explicit document. Changed recipes reset its generation and live bindings, preserving completed jobs. Concurrent heads share the same document environment."
    (actor actor "caller") (document buffer "owning document ID") (input list "environment recipe") (returns model) (public))
  (define (for-document! actor document input)
    (let ([r (recipe input)] [scope document])
      (unless (store:exists? document) (error 'for-document! "document is unavailable" document))
      (kernel:call-with-deferred-deliveries
        (lambda () (with-mutex document-lock
                     (let ([existing (find (lambda (id)
                                             (let ([r (record id 'environment)])
                                               (and r (equal? (assq 'document (get r 'value)) (cons 'document document)))))
                                       (model:ids 'environment))])
                       (if (not existing)
                         (model:create! actor 'environment 1 'session 'persistent (cons scope (map cdr (get r 'resources)))
                           (list (cons 'recipe r) '(generation . 1) '(status . idle) '(catalogue . 0) '(count . 0) '(notice . #f)
                             (cons 'document document)))
                         (let* ([g (group-for existing)]
                                [old (admit g (group-generation g)
                                       (lambda ()
                                         (if (equal? r (get (get (record existing 'environment) 'value) 'recipe)) #f
                                           (let* ([old (reset-group! actor g "Environment recipe changed; live bindings were reset")]
                                                  [current (record existing 'environment)])
                                             (let-values ([(status rows) (model:commit! actor
                                                                           (list (list existing (get current 'revision) (cons scope (map cdr (get r 'resources)))
                                                                                   (put (get current 'value) (cons 'recipe r)))))])
                                               (unless (eq? status 'applied) (error 'for-document! "environment changed outside its owner"))) old))))])
                           (when old (worker:close! old)) existing))))))))

  (edoc "Submit Scheme source in environment order and return its job model immediately. The job survives head detachment. Generation mismatch refuses without creating work."
    (actor actor "submitting actor") (id model "environment") (generation integer "expected namespace generation")
    (source string "Scheme forms") (projection datum "optional procedure expression receiving the final values list inside the worker; false preserves values")
    (returns model) (receiver id (model environment)))
  (define evaluate!
    (case-lambda
      [(actor id generation source) (evaluate! actor id generation source #f)]
      [(actor id generation source projection)
       (unless (string? source) (error 'evaluate! "expected source text"))
       (let ([projection (datum:copy projection)])
         (let ([g (group-for id)])
           (unless g (error 'evaluate! "environment is unavailable"))
           (admit g generation
             (lambda ()
               (let* ([r (record id 'environment)]
                      [output
                       (store:create!
                         actor
                         "<evaluation>"
                         '("")
                         (list
                           '(internal . #t)
                           '(read-only . #t)
                           (cons 'disposable (eq? (get r 'persistence) 'transient))))]
                      [job (model:create! actor 'evaluation-job 1 id (get r 'persistence) (list id output)
                             (map cons '(environment generation source status output channels result diagnostic projection)
                               (list id generation source 'queued output '() #f #f projection)))])
                 (group-queue-set! g (append (group-queue g) (list job))) (start! g) job)))))]))

  (edoc "Cancel a queued job without changing definitions. A running job resets its entire environment generation and reaps the worker; committed external effects remain. Return queued, reset or finished."
    (actor actor "caller") (id model "job") (returns symbol) (receiver id (model evaluation-job)))
  (define (cancel! actor id)
    (let ([j (record id 'evaluation-job)] [old #f])
      (if (not j) 'finished
        (let* ([v (get j 'value)] [g (group-for (get v 'environment))]
               [result (if (not g) 'finished
                         (admit g (group-generation g)
                           (lambda ()
                             (cond [(member id (group-queue g))
                                    (group-queue-set! g (remove id (group-queue g)))
                                    (status! actor id 'cancelled #f) 'queued]
                               [(and (equal? id (group-active g))
                                  (let ([r (record id 'evaluation-job)])
                                    (and r (eq? (get (get r 'value) 'status) 'running))))
                                (set! old (reset-group! actor g "Running evaluation cancelled; live bindings were reset")) 'reset]
                               [else 'finished]))))])
          (when old (worker:close! old)) result))))

  (edoc "Reset a namespace and its live handles at the expected generation. Pending jobs become reset; borrowed resources and completed portable results survive. The next evaluation lazily initializes the same recipe."
    (actor actor "caller") (id model "environment") (generation integer "expected generation") (receiver id (model environment)))
  (define (reset! actor id generation)
    (let ([g (group-for id)])
      (unless g (error 'reset! "environment is unavailable"))
      (let ([old (admit g generation (lambda () (reset-group! actor g "Live bindings were reset")))])
        (when old (worker:close! old)))))

  (edoc "Release a completed job, its owned output and retained result. Running or queued jobs must be cancelled first. Releasing a view never implicitly releases its borrowed job."
    (actor actor "caller") (id model "job") (returns boolean) (receiver id (model evaluation-job)))
  (define (release! actor id)
    (let ([j (record id 'evaluation-job)])
      (and j (let ([g (group-for (get (get j 'value) 'environment))])
               (and g (admit g (group-generation g)
                        (lambda ()
                          (let* ([j (record id 'evaluation-job)] [v (and j (get j 'value))])
                            (and v (not (memq (get v 'status) '(queued running)))
                              (begin
                                (let ([result (get v 'result)])
                                  (when (and (pair? result) (eq? (car result) 'handle))
                                    (group-releases-set! g (cons (caddr result) (group-releases g)))
                                    (start! g)))
                                (let-values ([(status row) (model:retire! actor id (get j 'revision))])
                                  (when (eq? status 'applied)
                                    (when (store:exists? (get v 'output)) (store:delete! actor (get v 'output)))) #t)))))))))))

  (edoc "Read a bounded page of the last completed symbol catalogue. Return (generation catalogue count names), or false when the supplied basis changed. A running job keeps the preceding catalogue until its boundary."
    (id model "environment") (generation integer "namespace generation") (catalogue integer "catalogue revision")
    (offset integer "first name") (count integer "1 through 256") (returns (or list #f)))
  (define (completion id generation catalogue offset count)
    (unless (and (natural? offset) (natural? count) (<= 1 count 256)) (error 'completion "invalid page"))
    (let ([g (with-mutex groups-lock (hashtable-ref groups (cadr id) #f))])
      (and g (with-mutex (group-lock g)
               (and (= generation (group-generation g)) (= catalogue (group-catalogue g))
                 (let* ([names (group-names g)] [size (vector-length names)] [end (min size (+ offset count))])
                   (list generation catalogue size
                     (let loop ([i offset]) (if (>= i end) '() (cons (vector-ref names i) (loop (+ i 1))))))))))))

  (edoc "Close an environment, reset/reap its worker and delete its owned jobs/output. Declared borrowed documents and models survive."
    (actor actor "caller") (id model "environment") (generation integer "expected generation") (receiver id (model environment)))
  (define (close! actor id generation)
    (let ([g (group-for id)])
      (unless g (error 'close! "environment is unavailable"))
      (let ([old
             (admit g generation
               (lambda ()
                 (let ([old (reset-group! actor g "Environment closed")])
                   (for-each (lambda (j)
                               (let ([output (get (get j 'value) 'output)])
                                 (let-values ([(status row) (model:retire! actor (get j 'id) (get j 'revision))]) (void))
                                 (when (store:exists? output) (store:delete! actor output)))) (jobs id))
                   (let ([r (record id 'environment)])
                     (let-values ([(status row) (model:retire! actor id (get r 'revision))]) (void))) old)))])
        (when old (worker:close! old))
        (with-mutex groups-lock (hashtable-delete! groups (cadr id))))))

  (edoc "After restoring base models, fence every saved namespace and expire live handles without replaying source. Recipes and portable completed results remain."
    (returns any))
  (define (restore!)
    ;; Producer visibility is runtime policy; text recovery intentionally keeps
    ;; only document facts. Re-establish it from the owning recovered jobs.
    (for-each (lambda (id)
                (let* ([r (record id 'evaluation-job)] [output (and r (get (get r 'value) 'output))])
                  (when (and output (store:exists? output))
                    (store:set-property! '(base e) output 'internal #t))))
      (model:ids 'evaluation-job))
    (for-each (lambda (id)
                (let* ([g (group-for id)]
                       [old (admit g (group-generation g)
                              (lambda () (reset-group! '(base e) g "Base restarted; live bindings were reset")))])
                  (when old (worker:close! old))))
      (model:ids 'environment)))

  (edoc "Stop and reap native workers during base shutdown without changing the already saved model snapshot."
    (returns any))
  (define (stop!)
    (let ([all (with-mutex groups-lock
                 (let ([all (vector->list (hashtable-values groups))]) (hashtable-clear! groups) all))])
      (for-each (lambda (g)
                  (let ([w (with-mutex (group-lock g)
                             (group-generation-set! g (+ 1 (group-generation g)))
                             (group-queue-set! g '())
                             (let ([w (group-worker g)]) (group-worker-set! g #f) w))])
                    (when w (worker:close! w)))) all))))
