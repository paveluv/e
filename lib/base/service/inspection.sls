;; An inspector retains portable head facts, never executable head objects.
(import (only (foundation edoc) elibrary))
(elibrary (service inspection)
  (export close! create! publish!)
  (import (chezscheme) (prefix (core descriptor) descriptor:) (prefix (core kernel) kernel:)
          (prefix (foundation wire) wire:) (prefix (state actor) actor:)
          (prefix (state model) model:) (prefix (state view) view:))
  (define (get r k) (cdr (assq k r)))
  (define (natural? x) (and (fixnum? x) (>= x 0)))
  (define (unique? xs) (or (null? xs) (and (not (memq (car xs) (cdr xs))) (unique? (cdr xs)))))
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
      (equal? (map car v) '(owner attachment subject definitions parts truncated? status))
      (descriptor:head? (get v 'owner)) (string? (get v 'attachment))
      (natural? (get v 'definitions)) (list? (get v 'parts)) (<= 1 (length (get v 'parts)) 8)
      (for-all (lambda (p) (and (pair? p) (symbol? (car p)) (model:reference? (cdr p)))) (get v 'parts))
      (unique? (map car (get v 'parts)))
      (boolean? (get v 'truncated?))
      (memq (get v 'status) '(pending ready unavailable))))
  (define kind (model:register-kind! 'inspection 1 valid?))
  (define rows-kind (model:register-kind! 'inspection-rows 1
                      (lambda (v) (and (list? v) (<= (length v) 2048) (for-all row? v)))))
  (define (record id)
    (let ([r (model:snapshot id)]) (and r (eq? (get r 'kind) 'inspection) r)))
  (define (owned? r actor) (and r (equal? (get (get r 'value) 'owner) actor)))
  (define (change r v) (list (get r 'id) (get r 'revision) (get r 'references) v))

  (edoc "Create a transient inspection for this attachment. The base retains its subject, definition basis and bounded portable listing. Departing attachments leave an unavailable snapshot; a later attachment creates a new inspector."
        (actor actor "producing head") (subject datum "initial explicit subject")
        (parts (list-of symbol) "distinct independently changing sections") (returns list "inspection ID and named section references"))
  (define (create! actor subject parts)
    (unless (and (descriptor:head? actor) (list? parts) (<= 1 (length parts) 8)
              (for-all symbol? parts) (unique? parts))
      (error 'create! "expected a head and one through eight distinct section names"))
    (let ([attachment (gensym->unique-string (gensym "inspection"))])
      (let ([ids (model:allocate! actor (+ 1 (length parts))
                   (lambda (ids)
                     (cons (list 'inspection 1 actor 'transient (cdr ids)
                             (map cons '(owner attachment subject definitions parts truncated? status)
                               (list actor attachment subject 0 (map cons parts (cdr ids)) #f 'pending)))
                       (map (lambda (id) (list 'inspection-rows 1 (car ids) 'transient '() '())) (cdr ids)))))])
        (list (car ids) (map cons parts (cdr ids))))))

  (edoc "Publish changed inspection sections against the last header revision. Omitted sections retain their rows; unchanged sections produce no notification. Rows are (key heading-or-false keys command description italic-spans), bounded in total to 2048 rows and 256 KiB. Only the producing attachment may publish while demanded. Return applied, unchanged, stale, hidden or unavailable."
        (actor actor "producing head") (id model "inspection") (revision integer "expected revision")
        (subject datum "explicit inspected subject") (definitions integer "head definition generation")
        (parts list "(section-name . rows) changes") (truncated? boolean "explicit budget limit") (returns symbol))
  (define (publish! actor id revision subject definitions parts truncated?)
    (let* ([r (record id)] [v (and r (get r 'value))])
      (cond [(or (not (owned? r actor)) (eq? (get v 'status) 'unavailable)) 'unavailable]
        [(not (model:demanded? id)) 'hidden]
        [(not (equal? revision (get r 'revision))) 'stale]
        [else
         (unless (and (list? parts) (for-all (lambda (p) (and (pair? p) (assq (car p) (get v 'parts)) (list? (cdr p)))) parts)
                   (unique? (map car parts)))
           (error 'publish! "expected distinct declared sections"))
         (let* ([next (map cons '(owner attachment subject definitions parts truncated? status)
                        (list actor (get v 'attachment) subject definitions (get v 'parts) truncated? 'ready))]
                [current (map (lambda (p) (model:snapshot (cdr p))) (get v 'parts))]
                [values (map (lambda (p r) (cond [(assq (car p) parts) => cdr] [else (get r 'value)])) (get v 'parts) current)]
                [rows (apply append values)])
           (unless (and (valid? next) (<= (length rows) 2048) (for-all row? rows)
                     (<= (bytevector-length (wire:encode (list next values))) 262144))
             (error 'publish! "invalid or oversized inspection snapshot"))
           (if (and (equal? next v) (equal? values (map (lambda (r) (get r 'value)) current))) 'unchanged
             (let-values ([(status ignored) (model:commit! actor (cons (change r next) (map change current values)))]) status)))])))

  (edoc "Retire an owned inspection and its scoped presentations." (actor actor "producer") (id model "inspection"))
  (define (close! actor id)
    (let retry ()
      (let ([r (record id)])
        (when (owned? r actor)
          (let-values ([(status ignored) (model:retire! actor id (get r 'revision))])
            (case status [(stale) (retry)]
              [(applied) (view:retire-scope! actor id)
               (for-each (lambda (p)
                           (let retry ()
                             (let ([r (model:snapshot (cdr p))])
                               (when r
                                 (let-values ([(status ignored) (model:retire! actor (cdr p) (get r 'revision))])
                                   (when (eq? status 'stale) (retry))))))) (get (get r 'value) 'parts))]))))))
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
