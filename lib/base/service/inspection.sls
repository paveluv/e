;; An inspector retains portable head facts, never executable head objects.
(import (only (foundation edoc) elibrary))
(elibrary (service inspection)
  (export close! create! publish!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (foundation wire) wire:) (prefix (state actor) actor:)
          (prefix (state model) model:) (prefix (state view) view:))
  (define (get r k) (cdr (assq k r)))
  (define (natural? x) (and (fixnum? x) (>= x 0)))
  ;; A row: (stable-key section-title keys command description italic-spans).
  ;; A heading has a string title, empty keys/command/description/spans.
  (define (row? r)
    (and (list? r) (= (length r) 6) (or (not (cadr r)) (string? (cadr r)))
      (list? (caddr r)) (for-all string? (caddr r)) (string? (cadddr r)) (string? (list-ref r 4))
      (list? (list-ref r 5))
      (for-all (lambda (p) (and (pair? p) (natural? (car p)) (natural? (cdr p))
                             (<= (car p) (cdr p) (string-length (cadddr r))))) (list-ref r 5))))
  (define (valid? v)
    (and (list? v) (for-all pair? v)
      (equal? (map car v) '(owner attachment subject definitions rows truncated? status))
      (descriptor:head? (get v 'owner)) (string? (get v 'attachment))
      (natural? (get v 'definitions)) (list? (get v 'rows)) (<= (length (get v 'rows)) 2048)
      (for-all row? (get v 'rows)) (boolean? (get v 'truncated?))
      (memq (get v 'status) '(pending ready unavailable))))
  (define kind (model:register-kind! 'inspection 1 valid?))
  (define (record id)
    (let ([r (model:snapshot id)]) (and r (eq? (get r 'kind) 'inspection) r)))
  (define (owned? r actor) (and r (equal? (get (get r 'value) 'owner) actor)))
  (define (change r v) (list (get r 'id) (get r 'revision) '() v))

  (edoc "Create a transient inspection for this attachment. The base retains its subject, definition basis and bounded portable listing. Departing attachments leave an unavailable snapshot; a later attachment creates a new inspector."
        (actor actor "producing head") (returns model))
  (define (create! actor)
    (unless (descriptor:head? actor) (error 'create! "expected a head"))
    (model:create! actor 'inspection 1 actor 'transient '()
      (map cons '(owner attachment subject definitions rows truncated? status)
        (list actor (gensym->unique-string (gensym "inspection")) #f 0 '() #f 'pending))))

  (edoc "Publish meaningful inspection changes against the last model revision. Rows are (key heading-or-false keys command description italic-spans), bounded to 2048 rows and 256 KiB. Only the producing attachment may publish, and only while the inspector is demanded. Unchanged content is acknowledged without a model update. Return applied, unchanged, stale, hidden or unavailable."
        (actor actor "producing head") (id model "inspection") (revision integer "expected revision")
        (subject datum "explicit inspected subject") (definitions integer "head definition generation")
        (rows list "portable listing rows") (truncated? boolean "explicit budget limit") (returns symbol))
  (define (publish! actor id revision subject definitions rows truncated?)
    (let* ([r (record id)] [v (and r (get r 'value))])
      (cond [(or (not (owned? r actor)) (eq? (get v 'status) 'unavailable)) 'unavailable]
        [(not (model:demanded? id)) 'hidden]
        [(not (equal? revision (get r 'revision))) 'stale]
        [else
         (let ([next (map cons '(owner attachment subject definitions rows truncated? status)
                       (list actor (get v 'attachment) subject definitions rows truncated? 'ready))])
           (unless (and (valid? next) (<= (bytevector-length (wire:encode next)) 262144))
             (error 'publish! "invalid or oversized inspection snapshot"))
           (if (equal? next v) 'unchanged
             (let-values ([(status ignored) (model:commit! actor (list (change r next)))]) status)))])))

  (edoc "Retire an owned inspection and its scoped presentations." (actor actor "producer") (id model "inspection"))
  (define (close! actor id)
    (let retry ()
      (let ([r (record id)])
        (when (owned? r actor)
          (let-values ([(status ignored) (model:retire! actor id (get r 'revision))])
            (case status [(stale) (retry)] [(applied) (view:retire-scope! actor id)]))))))
  (define departures
    (actor:subscribe!
      (lambda (events)
        (for-each
          (lambda (event)
            (when (eq? (car event) 'detached)
              (for-each
                (lambda (id)
                  (let retry ()
                    (let ([r (record id)])
                      (when (and (owned? r (cadr event)) (not (eq? (get (get r 'value) 'status) 'unavailable)))
                        (let ([v (map (lambda (p) (if (eq? (car p) 'status) '(status . unavailable) p)) (get r 'value))])
                          (let-values ([(status ignored) (model:commit! '(base inspection) (list (change r v)))])
                            (when (eq? status 'stale) (retry)))))))) (model:ids 'inspection)))) events)))))
