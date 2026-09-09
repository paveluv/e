;; Client implementation of the store seam. Only immutable text and owned
;; metadata are cached; the base admits all writes and computes all history.
(library (store)
  (export create! delete! discard! prepare-close reset! rename! buffer-list exists? visible? buffer-name find-named
          snapshot snapshot-state snapshot-since revision line-count line extract
          property properties set-property! set-properties!
          edit! edit-with-snapshot! history-step! undo-authors history blame marks set-marks!
          validate-properties validate-edit-context watch! unsubscribe!)
  (import (chezscheme)
          (prefix (client) client:) (prefix (kernel) kernel:)
          (prefix (identity) identity:) (prefix (property) property:)
          (prefix (datum) datum:) (prefix (text) text:))

  (define validate-properties property:validate)
  (define validate-edit-context property:edit-context)
  (define cache (make-eqv-hashtable))
  (define watching? #f)

  (define (forget! pending)
    (if pending (for-each (lambda (entry) (hashtable-delete! cache (car entry))) pending)
        (hashtable-clear! cache)))
  (define invalidation
    (kernel:call-with-runtime-registrations
      (lambda () (client:subscribe! 'changed forget!))))

  (define (watch! wake)
    (client:set-wake! wake)
    (unless watching? (client:request 'watch-head) (set! watching? #t))
    (client:watch! 'changed wake))
  (define unsubscribe! client:unsubscribe!)

  (define (decode-changes changes)
    (and changes
         (map (lambda (entry) (list (car entry) (cadr entry) (text:datum->delta (caddr entry)))) changes)))

  (define (read-state id basis)
    (let ([state (client:request 'state id basis)])
      (hashtable-set! cache id (and state (list-head state 4)))
      state))
  (define (cached id)
    (if (hashtable-contains? cache id) (hashtable-ref cache id #f) (read-state id #f)))
  (define (required id)
    (or (cached id) (error 'store "no buffer" id)))
  (define (buffer-list) (client:request 'buffers))
  (define (prepare-close)
    (error 'prepare-close "an attached head does not own the base lifetime"))
  (define (exists? id) (and (cached id) #t))
  (define (buffer-name id) (string-copy (car (required id))))
  (define (find-named name)
    (find (lambda (id) (equal? (buffer-name id) name)) (buffer-list)))
  (define (properties id) (datum:copy (cadddr (required id))))
  (define property
    (case-lambda
      [(id key) (property id key #f)]
      [(id key fallback)
       (cond [(assq key (cadddr (required id))) => (lambda (entry) (datum:copy (cdr entry)))]
         [else fallback])]))
  (define (visible? actor id)
    (and (exists? id) (identity:in-audience? actor (property id 'audience 'all))))

  (define snapshot-state
    (case-lambda
      [(id) (snapshot-state id #f)]
      [(id basis)
       ;; Explicit captures (file checks and adoption) always contact the
       ;; authority. Repeated facts in painting use the invalidated cache.
       (let ([state (or (read-state id basis) (error 'snapshot-state "no buffer" id))])
         (if basis
             (values (cadr state) (caddr state) (datum:copy (cadddr state))
               (decode-changes (list-ref state 4)))
             (values (cadr state) (caddr state) (datum:copy (cadddr state)))))]))
  (define (snapshot id)
    (let-values ([(text revision facts) (snapshot-state id)]) (values text revision)))
  (define (snapshot-since id basis)
    (let-values ([(text revision facts changes) (snapshot-state id basis)]) (values text revision changes)))
  (define (revision id) (caddr (required id)))
  (define (line-count id) (vector-length (cadr (required id))))
  (define (line id row) (vector-ref (cadr (required id)) row))
  (define (extract id span) (text:extract (cadr (required id)) span))

  (define (check-actor actor)
    (unless (equal? actor (client:identity)) (error 'store "an attached head writes as itself")))
  (define (mutate actor id operation args)
    (check-actor actor)
    (let ([result (apply client:request operation id args)])
      (hashtable-delete! cache id) result))
  (define (create! actor name lines . facts)
    (check-actor actor)
    (apply client:request 'create name lines facts))
  (define (delete! actor id) (mutate actor id 'delete '()) (void))
  (define (discard! actor id revision facts) (mutate actor id 'discard (list revision facts)))
  (define (reset! actor id lines . options)
    (mutate actor id 'reset (cons lines options)))
  (define (rename! actor id name) (mutate actor id 'rename (list name)))
  (define (set-properties! actor id updates . review)
    (mutate actor id 'properties (cons updates review)))
  (define (set-property! actor id key value) (set-properties! actor id (list (cons key value))))

  (define (edit-with-snapshot! actor id basis span replacement . options)
    (unless (<= (length options) 2) (error 'edit! "expected context and write access"))
    (let* ([context (and (pair? options) (car options))]
           [result (mutate actor id 'edit
                     (list basis (text:span->datum span) replacement context))]
           [status (car result)] [detail (cadr result)])
      (values status
        (if (eq? status 'applied)
            (list (car detail) (cadr detail) (decode-changes (caddr detail))) detail))))
  (define (edit! actor id basis span replacement . options)
    (let-values ([(status detail) (apply edit-with-snapshot! actor id basis span replacement options)])
      (values status (if (eq? status 'applied) (car detail) detail))))
  (define (history-step! actor id direction scope . access)
    (unless (<= (length access) 1) (error 'history-step! "expected one write access"))
    (apply values (mutate actor id 'history-step (list direction scope))))
  (define (undo-authors id) (client:request 'undo-authors id))
  (define (history id . count) (apply client:request 'history id count))
  (define (blame id . count)
    (map (lambda (entry) (cons (text:datum->span (car entry)) (cdr entry)))
      (apply client:request 'blame id count)))
  (define (set-marks! actor id basis updates drops)
    (check-actor actor)
    (apply values
      (client:request 'marks id basis
        (map (lambda (entry)
               (cons (car entry) (if (text:span? (cdr entry))
                                   (list 'span (text:span->datum (cdr entry))) (cdr entry)))) updates)
        drops)))
  (define (marks actor id)
    (check-actor actor)
    (map (lambda (entry)
           (cons (car entry)
             (if (and (pair? (cdr entry)) (eq? (cadr entry) 'span))
                 (text:datum->span (caddr entry)) (cdr entry))))
      (client:request 'read-marks id)))
)
