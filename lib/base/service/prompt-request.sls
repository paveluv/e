;; Portable prompt lifetimes. Continuations and placements belong to heads.
(import (only (foundation edoc) elibrary))
(elibrary (service prompt-request)
  (export accept! cancel! close! close-owner! create!)
  (import (chezscheme) (prefix (core descriptor) descriptor:)
          (prefix (core kernel) kernel:) (prefix (foundation datum) datum:)
          (prefix (foundation text) text:) (prefix (state model) model:)
          (prefix (state store) store:) (prefix (sys activity) activity:))

  (define (get r k) (cdr (assq k r)))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (reference? x tag)
    (and (list? x) (= (length x) 2) (eq? (car x) tag) (natural? (cadr x)) (> (cadr x) 0)))
  (define (valid? v)
    (and (list? v) (for-all pair? v)
      (equal? (map car v) '(owner parent draft owned? origin provider status outcome))
      (descriptor:head? (get v 'owner))
      (or (not (get v 'parent)) (reference? (get v 'parent) 'model))
      (reference? (get v 'draft) 'buffer) (boolean? (get v 'owned?))
      (memq (get v 'status) '(editing accepted cancelled))
      (case (get v 'status)
        [(editing cancelled) (not (get v 'outcome))]
        [(accepted)
         (let ([o (get v 'outcome)])
           (and (list? o) (= (length o) 3) (natural? (car o))
             (vector? (cadr o)) (> (vector-length (cadr o)) 0)
             (for-all string? (vector->list (cadr o)))))])))
  (define registration
    (kernel:call-with-runtime-registrations
      (lambda () (model:register-kind! 'prompt-request 1 valid?))))
  (define (record id)
    (let ([r (model:snapshot id)])
      (and r (eq? (get r 'kind) 'prompt-request) (= (get r 'schema) 1) (valid? (get r 'value)) r)))
  (define (owned r actor) (and r (equal? (get (get r 'value) 'owner) actor)))
  (define (editing? r) (eq? (get (get r 'value) 'status) 'editing))
  (define (change r v) (list (get r 'id) (get r 'revision) (get r 'references) v))
  (define (witness r) (change r (get r 'value)))
  (define (ancestors actor id)
    (let loop ([id id] [seen '()] [out '()])
      (if (not id) out
        (and (not (member id seen))
          (let ([r (record id)])
            (and (owned r actor) (editing? r)
              (loop (get (get r 'value) 'parent) (cons id seen) (cons r out))))))))
  (define (transition v status outcome)
    (map (lambda (p) (case (car p) [(status) (cons 'status status)]
                       [(outcome) (cons 'outcome outcome)] [else p])) v))
  (define (children id)
    (filter values
      (map (lambda (child)
             (let ([r (record child)])
               (and r (equal? id (get (get r 'value) 'parent)) child))) (model:ids 'prompt-request))))
  (define (create-draft! actor value)
    (let-values ([(lines trailing?) (text:from-string value)])
      (list 'buffer
        (store:create! actor "<prompt>"
          (if trailing? (list->vector (append (vector->list lines) '(""))) lines)
          (list '(internal . #t) '(disposable . #t) (cons 'audience (list actor)))))))

  (edoc "Create a transient input request with a captured origin and provider recipe. A false draft creates an owned internal disposable buffer from text; an explicit buffer borrows its authored text unchanged. Parent must still be editing and belong to this head. Return a model or false when the parent became unavailable."
        (actor actor "requesting head") (parent (or model #f) "owning request")
        (draft (or buffer #f) "borrowed authored draft, or false") (text string "initial text for a new draft")
        (origin datum "captured command context") (provider datum "completion provider recipe") (returns (or model #f)))
  (define (create! actor parent draft text origin provider)
    (unless (and (descriptor:head? actor) (or (not parent) (reference? parent 'model))
              (or (not draft) (reference? draft 'buffer)) (string? text)
              (or (not draft) (string=? text "")))
      (error 'create! "expected head, parent, optional draft and initial text"))
    (let ([actor (datum:copy actor)] [origin (datum:copy origin)] [provider (datum:copy provider)])
      (activity:call-with
        (lambda ()
          (let ([parents (ancestors actor parent)])
            (and parents
              (begin
                (when (and draft (not (store:exists? (cadr draft)))) (error 'create! "draft is unavailable" draft))
                (let ([source (or draft (create-draft! actor text))]
                      [committed? #f])
                  (dynamic-wind void
                    (lambda ()
                      (let ([ids
                             (model:allocate! actor 1
                               (lambda (ids)
                                 (list (list 'prompt-request 1 actor 'transient
                                         (cons source (if parent (list parent) '()))
                                         (map cons '(owner parent draft owned? origin provider status outcome)
                                           (list actor parent source (not draft) origin provider 'editing #f)))))
                               (lambda (ids) (map witness parents)))])
                        (set! committed? (and ids #t)) (and ids (car ids))))
                    (lambda ()
                      (when (and (not committed?) (not draft) (store:exists? (cadr source)))
                        (store:delete! actor (cadr source)))))))))))))

  (edoc "Accept a request once against its request and draft revisions. Capture the reviewed text and origin as (draft-revision lines origin); later edits cannot change that outcome. Every parent must remain editing. Return applied, stale, closed or unavailable."
        (actor actor "request owner") (id model "request") (revision integer "request revision")
        (draft-revision integer "reviewed authored text revision") (returns symbol))
  (define (accept! actor id revision draft-revision)
    (unless (and (natural? revision) (natural? draft-revision)) (error 'accept! "expected revisions"))
    (activity:call-with
      (lambda ()
        (let ([r (record id)])
          (cond [(not (owned r actor)) 'unavailable] [(not (editing? r)) 'closed]
            [(not (= revision (get r 'revision))) 'stale]
            [else
             (let* ([v (get r 'value)] [parents (ancestors actor (get v 'parent))]
                    [draft (cadr (get v 'draft))]
                    [text (store:state draft #f '())])
               (cond [(not parents) 'closed] [(not text) 'unavailable]
                 [(not (= draft-revision (caddr text))) 'stale]
                 [else
                  (let-values ([(status rows)
                                (model:commit! actor
                                  (cons (change r (transition v 'accepted
                                                    (list draft-revision (cadr text) (get v 'origin))))
                                    (map witness parents)))])
                    (when (eq? status 'applied)
                      (for-each (lambda (child) (cancel! actor child)) (children id)))
                    status)]))])))))

  (edoc "Cancel an editing request and its descendants. Terminal requests keep their existing outcome; cancelling never answers a base actor question. Return whether this request changed."
        (actor actor "request owner") (id model "request") (returns boolean))
  (define (cancel! actor id)
    (activity:call-with
      (lambda ()
        (let loop ()
          (let ([r (record id)])
            (and (owned r actor) (editing? r)
              (let-values ([(status rows) (model:commit! actor (list (change r (transition (get r 'value) 'cancelled #f))))])
                (case status
                  [(stale) (loop)]
                  [(applied) (for-each (lambda (child) (cancel! actor child)) (children id)) #t]
                  [else #f]))))))))

  (edoc "Close a request tree and release its owned transient drafts. Borrowed authored buffers survive. A closed identity cannot accept late outcomes."
        (actor actor "request owner") (id model "request"))
  (define (close! actor id)
    (activity:call-with
      (lambda ()
        (cancel! actor id)
        (let loop ()
          (let ([r (record id)])
            (when (owned r actor)
              (for-each (lambda (child) (close! actor child)) (children id))
              (let-values ([(status current) (model:retire! actor id (get r 'revision))])
                (case status
                  [(stale) (loop)]
                  [(applied)
                   (let* ([v (get r 'value)] [draft (cadr (get v 'draft))])
                     (when (and (get v 'owned?) (store:exists? draft)) (store:delete! actor draft)))]))))))))

  (edoc "Abandon the departing head's transient requests, preserving borrowed drafts and independent actor questions. Call before admitting a replacement attachment."
        (actor actor "departing head"))
  (define (close-owner! actor)
    (for-each (lambda (id) (close! actor id)) (model:ids 'prompt-request))))
