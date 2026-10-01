;; Disposable published text over borrowed conflict/rewrite drafts.
(import (only (foundation edoc) elibrary))
(elibrary (service review-preview)
  (export close! create!)
  (import (chezscheme) (prefix (core handle) handle:) (prefix (core kernel) kernel:) (prefix (core port) port:)
          (prefix (core review-contract) review-contract:) (prefix (core row) row:)
          (prefix (core work-queue) work-queue:) (prefix (foundation text) text:)
          (prefix (service conflict-review) conflict-review:) (prefix (service rewrite) rewrite:)
          (prefix (state collection) collection:) (prefix (state connection) connection:)
          (prefix (state model) model:) (prefix (state store) store:) (prefix (state view) view:))
  (define (get r k) (cdr (assq k r)))
  (define producer '(app review-preview))
  (define facts '((internal . #t) (disposable . #t) (read-only . #t) (mode-auto . #f)))
  (define kind (model:register-kind! 'review-preview 1
                 (lambda (v)
                   (and (list? v) (for-all pair? v) (equal? (map car v) '(draft document selection status basis annotations truncated?))
                     (model:reference? (get v 'draft)) (handle:buffer? (get v 'document))
                     (or (not (get v 'selection)) (row:selection? (get v 'selection)))
                     (memq (get v 'status) '(pending ready blocked unavailable)) (list? (get v 'annotations)) (boolean? (get v 'truncated?))))))
  (define ports (port:register! '(model review-preview 1) review-contract:ports))
  (define worker (work-queue:create))
  (define lock (make-mutex))
  ;; Requests keep only dependency identities and a completed input stamp here.
  (define active (make-hashtable equal-hash equal?))
  (define-record-type job (fields id draft document (mutable dependencies) (mutable token) (mutable source) (mutable stamp) (mutable projection)))
  (define (record id)
    (let ([r (model:snapshot id)]) (and r (eq? (get r 'kind) 'review-preview) r)))
  (define (draft id)
    (let ([r (model:snapshot id)])
      (unless (and r (memq (get r 'kind) '(conflict-review rewrite-draft))) (error 'review-preview "draft is unavailable")) r))
  (define (replace r key value) (map (lambda (p) (if (eq? key (car p)) (cons key value) p)) r))
  (define (pending message) (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (alive? job)
    (and (with-mutex lock (eq? job (hashtable-ref active (job-id job) #f)))
      (model:demanded? (job-id job)) (record (job-id job))))
  (define (new-document!)
    (store:publish! producer (gensym->unique-string (gensym "review-preview")) "<review-preview>" '("") facts #f))
  (define (specification draft document scope)
    (list 'review-preview 1 scope 'persistent (list draft document)
      (map cons '(draft document selection status basis annotations truncated?) (list draft document #f 'pending #f '() #f))))

  (edoc "Create an independent preview request and disposable read-only document over a borrowed conflict or rewrite draft; return (request document). Connect a table's selection output to the request's selection input. Derivation runs only while requested."
        (actor actor "creator") (id model "conflict-review or rewrite draft") (owner (list-of model) "optional owning view") (returns list))
  (define (create! actor id . owner)
    (unless (and (<= (length owner) 1) (for-all model:reference? owner)) (error 'create! "expected at most one owning view"))
    (let* ([d (draft id)] [r (and (pair? owner) (model:snapshot (car owner)))]
           [document (begin (when (and (pair? owner) (not (and r (eq? (get r 'kind) 'widget-view))))
                              (error 'create! "owning view is unavailable")) (new-document!))])
      (guard (ex [else (store:delete! producer document) (raise ex)])
        (let ([ids (model:allocate! actor 1
                     (lambda (ids) (list (specification id document (if r (get r 'id) 'session))))
                     (lambda (ids) (map (lambda (r) (list (get r 'id) (get r 'revision) (get r 'references) (get r 'value)))
                                     (if r (list r d) (list d)))))])
          (unless ids (error 'create! "draft or owner changed")) (list (car ids) document)))))

  (edoc "Retire a preview request, its scoped views and owned output document. The borrowed draft and original documents survive."
        (actor actor "caller") (id model "preview request"))
  (define (close! actor id)
    (let retry ()
      (let ([r (record id)])
        (when r
          (let-values ([(status current) (model:retire! actor id (get r 'revision))])
            (case status [(stale) (retry)]
              [(applied) (view:retire-scope! actor id)
               (let ([document (get (get r 'value) 'document)])
                 (when (store:exists? document) (store:delete! producer document)))]))))))
  (define (copy-resource! actor r)
    (let* ([v (get r 'value)] [document (new-document!)])
      (values (lambda (mapped) (specification (mapped (get v 'draft)) document (mapped (get r 'scope))))
        (list (cons (get v 'document) document))
        (lambda () (when (store:exists? document) (store:delete! producer document))))))
  (define lifecycle (view:register-resource-kind! 'review-preview 1 copy-resource! close!))

  (define (target r selection)
    (let* ([v (get r 'value)] [rewrite? (eq? (get r 'kind) 'rewrite-draft)])
      (cond
        [selection
         (let* ([query (collection:summary (car selection))] [q (and query (get query 'value))]
                [row (and q (eq? (get q 'status) 'ready) (= (cadr selection) (get q 'generation))
                       (collection:lookup (car selection) (cadr selection) (caddr selection) '()))]
                [key (caddr selection)])
           (unless (and row (eq? (car row) 'ready) (pair? (list-ref row 4)) (list? key) (= (length key) 2)
                     (equal? (get q 'source) (get r 'id))
                     (if rewrite? (equal? (car key) (get v 'document)) (member (car key) (get v 'scope))))
             (pending "selection is pending or belongs to another draft")) key)]
        [rewrite? (list (get v 'document) #f)]
        [(pair? (get v 'scope)) (list (car (get v 'scope)) #f)]
        [else #f])))
  (define (update! r status basis annotations truncated?)
    (let ([v (get r 'value)])
      (model:commit! producer
        (list (list (get r 'id) (get r 'revision) (get r 'references)
                (replace (replace (replace (replace v 'status status) 'basis basis) 'annotations annotations) 'truncated? truncated?))))))
  (define (publish! job r stamp lines regions selected source blocked?)
    (let-values ([(old revision old-facts) (store:snapshot-state (job-document job))])
      (let ([publication (get old-facts 'publication)])
        (when (and (alive? job) (equal? stamp (input-stamp job)))
          (let ([id (store:publish! producer (cadr publication) "<review-preview>" (vector->list lines)
                      (cons (cons 'mode (and source (store:property source 'mode #f))) facts)
                      (list (job-document job) revision (cons 'publication publication)))])
            (when id
              (let* ([chosen (and selected (assv selected regions))]
                     [rest (if chosen (remq chosen regions) regions)]
                     [truncated? (> (length rest) 512)]
                     [kept (append (if chosen (list chosen) '()) (list-head rest (min 512 (length rest))))]
                     [annotations (list id (store:revision id)
                                    (map (lambda (p) (list (caddr p)
                                                       (case (cadr p) [(rewrite) 'match] [(mine) (if (eq? p chosen) 'conflict-mine-current 'conflict-mine)]
                                                         [else (if (eq? p chosen) 'conflict-disk-current 'conflict-disk)]))) kept))])
                (let-values ([(status rows) (update! r (if blocked? 'blocked 'ready) stamp annotations truncated?)])
                  (when (eq? status 'applied) (job-stamp-set! job stamp))))))))))
  (define (inputs job)
    (let* ([r (record (job-id job))] [d (draft (job-draft job))]
           [selection (connection:read (job-id job) 'selection)])
      (let ([ids (remove (job-id job) (map car (caddr selection)))])
        (unless (equal? ids (job-dependencies job))
          (let ([token (kernel:call-with-runtime-registrations
                         (lambda () (model:subscribe! ids (lambda (notice) (void)))))])
            (let-values ([(installed? old)
                          (with-mutex lock
                            (if (eq? job (hashtable-ref active (job-id job) #f))
                              (let ([old (job-token job)])
                                (job-dependencies-set! job ids) (job-token-set! job token) (values #t old))
                              (values #f #f)))])
              (when old (model:unsubscribe! old))
              (unless installed? (model:unsubscribe! token))))))
      (unless (and r (eq? (car selection) 'ready)) (pending "selection is unavailable"))
      (let ([target (target d (cadr selection))])
        (job-source-set! job (and target (car target)))
        (list r d target (list (get d 'revision) (cadr selection)
                           (and target (store:revision (car target))) (and target (store:property (car target) 'conflicts 0))
                           (and target (store:property (car target) 'mode #f)))))))
  (define (input-stamp job) (list-ref (inputs job) 3))
  (define (projection job d target stamp check!)
    (let* ([key (cons (car target) (cons (car stamp) (cddr stamp)))] [cached (job-projection job)])
      (if (and cached (equal? key (car cached))) (cdr cached)
        (let ([value (if (eq? (get d 'kind) 'rewrite-draft)
                       (list (rewrite:preview (get d 'id)))
                       (list (conflict-review:preview (get d 'id) (car target))))])
          (check!) (job-projection-set! job (cons key value)) value))))
  (define (rewrite-region p selected)
    (let ([entry (and selected (store:revision-span (cadr p) selected))])
      (when (and entry (not (= (car entry) (list-ref p 6)))) (pending "source changed while locating the selected edit"))
      (if (not (and entry (cadr entry))) '()
        (let* ([original (text:datum->span (cadr entry))] [changes (map text:datum->delta (list-ref p 4))]
               [span (fold-left (lambda (span change) (and span (text:rebase-span span change))) original changes)]
               [point (and (not span) (fold-left (lambda (point change) (text:rebase-position point change))
                                        (text:span-start original) changes))])
          (list (list selected 'rewrite (if span (text:span->datum span) (list (car point) (cdr point) (car point) (cdr point)))))))))
  (define (prepare! job check!)
    (let* ([data (inputs job)] [r (car data)] [d (cadr data)] [target (caddr data)] [stamp (list-ref data 3)])
      (unless (equal? stamp (job-stamp job))
        (let ([v (get r 'value)])
          (unless (eq? (get v 'status) 'pending)
            (update! r 'pending (get v 'basis) (get v 'annotations) (get v 'truncated?))))
        (when (eq? (get d 'kind) 'conflict-review)
          (set! d (conflict-review:refresh! (get d 'actor) (get d 'id) (get d 'revision) (get (get d 'value) 'scope))))
        (check!)
        (let* ([data (inputs job)] [r (car data)] [d (cadr data)] [target (caddr data)] [stamp (list-ref data 3)])
          (cond [(not target) (publish! job r stamp '#("") '() #f #f #f)]
            [(eq? (get d 'kind) 'rewrite-draft)
             (let* ([cached (projection job d target stamp check!)] [p (car cached)])
               (check!) (publish! job r stamp (list-ref p 3) (rewrite-region p (cadr target))
                          (cadr target) (car target) (pair? (list-ref p 5))))]
            [else
             (let ([p (car (projection job d target stamp check!))])
               (check!) (publish! job r stamp (list-ref p 3) (list-ref p 4) (cadr target) (car target) #f))])))))
  (define (queue! job)
    (work-queue:submit! worker (job-id job) (lambda () (alive? job))
      (lambda (check!) (prepare! job check!))
      (lambda (ex)
        ;; Preserve the last coherent text while selection or source changes.
        (job-stamp-set! job #f)
        (let ([r (record (job-id job))])
          (when r
            (let ([v (get r 'value)])
              (if (kernel:refusal? ex) (update! r 'pending (get v 'basis) (get v 'annotations) (get v 'truncated?))
                (update! r 'unavailable #f '() #f))))))))
  (define (schedule! id)
    (let ([r (record id)])
      (if (not (and r (model:demanded? id)))
        (let ([old (with-mutex lock
                     (let ([old (hashtable-ref active id #f)]) (hashtable-delete! active id) old))])
          (when (and old (job-token old)) (model:unsubscribe! (job-token old))))
        (let ([job (with-mutex lock
                     (or (hashtable-ref active id #f)
                       (let* ([v (get r 'value)] [job (make-job id (get v 'draft) (get v 'document) '() #f #f #f #f)])
                         (hashtable-set! active id job) job)))])
          (queue! job)))))
  (define observers
    (list (model:observe-demand! (lambda (ids) (for-each schedule! ids)))
      (model:subscribe! #f
        (lambda (notice)
          (for-each (lambda (job)
                      (when (or (not (cadr notice)) (member (job-id job) (cadr notice)) (member (job-draft job) (cadr notice))
                              (exists (lambda (id) (member id (job-dependencies job))) (cadr notice)))
                        (schedule! (job-id job))))
            (with-mutex lock (vector->list (hashtable-values active))))))
      (store:subscribe! #f
        (lambda (event)
          (for-each (lambda (job)
                      (when (equal? (cadr event) (job-source job))
                        ;; Alternatives can change without a text revision or count change.
                        (job-stamp-set! job #f) (job-projection-set! job #f) (queue! job)))
            (with-mutex lock (vector->list (hashtable-values active)))))))))
