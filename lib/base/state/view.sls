;; Persistent view descriptors share the model transaction/recovery machinery.
;; Geometry and live mounts belong to heads, never to these records.
(import (only (foundation edoc) elibrary))
(elibrary (state view)
  (export claim! create! publish! release! release-owner! reset-owners! set-state! snapshot)
  (import (chezscheme) (prefix (state model) model:))

  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (head? who) (and (list? who) (= (length who) 2) (eq? (car who) 'head) (string? (cadr who))))
  ;; Payload: (model-id renderer schema generation owner sequence basis state).
  (define (descriptor? value)
    (and (list? value) (= (length value) 8)
         (let ([id (car value)])
           (and (list? id) (= (length id) 2) (eq? (car id) 'model) (natural? (cadr id)) (> (cadr id) 0)))
         (symbol? (cadr value)) (natural? (caddr value)) (> (caddr value) 0)
         (natural? (cadddr value)) (or (not (list-ref value 4)) (head? (list-ref value 4)))
         (natural? (list-ref value 5)) (or (not (list-ref value 6)) (natural? (list-ref value 6)))))
  (define registration (model:register-kind! 'widget-view 1 descriptor?))
  (define (entry id)
    (let ([record (model:snapshot id)])
      (and record (eq? (cdr (assq 'kind record)) 'widget-view)
           (= (cdr (assq 'schema record)) 1) (descriptor? (cdr (assq 'value record))) record)))
  (define (value record) (cdr (assq 'value record)))
  (define (change record next)
    (list (cdr (assq 'id record)) (cdr (assq 'revision record)) (list (car next)) next))
  (define (update! actor id plan)
    (let loop ()
      (let ([record (entry id)])
        (if (not record) (values 'unavailable #f)
            (let-values ([(status next) (plan (value record))])
              (if (not (eq? status 'applied)) (values status (value record))
                  (let-values ([(status records) (model:commit! actor (list (change record next)))])
                    (if (eq? status 'stale) (loop)
                        (values status (and (car records) (value (car records))))))))))))
  (define (interaction old generation owner sequence basis state)
    (append (list-head old 3) (list generation owner sequence basis state)))

  (edoc "Create a persistent view of a model. Independent views share data, not interaction state; no head geometry is stored."
        (actor actor "the creator") (model list "the model id") (renderer symbol "widget kind")
        (schema integer "renderer schema") (state datum "initial interaction state") (returns list))
  (define (create! actor model renderer schema state)
    (model:create! actor 'widget-view 1 model 'persistent (list model)
      (list model renderer schema 0 #f 0 #f state)))

  (edoc "Read a view descriptor, or #f when absent or unsupported."
        (id list "view model id") (returns (or list #f)))
  (define (snapshot id) (let ([record (entry id)]) (and record (value record))))

  (edoc "Claim an unmounted view for a head. Return status and descriptor; ownership increments its generation and resets publication sequence."
        (actor actor "the mounting head") (id list "view model id"))
  (define (claim! actor id)
    (unless (head? actor) (error 'claim! "expected a head" actor))
    (update! actor id
      (lambda (old)
        (if (list-ref old 4) (values 'owned old)
            (values 'applied (interaction old (+ 1 (cadddr old)) actor 0 (list-ref old 6) (list-ref old 7)))))))

  (edoc "Commit a batch of owned view interactions. Every generation and increasing sequence must match; stale batches change nothing."
        (actor actor "the owning head") (updates list "(view-id generation sequence model-basis state) entries"))
  (define (publish! actor updates)
    (unless (and (list? updates)
                 (for-all (lambda (row) (and (list? row) (= (length row) 5)
                                             (natural? (cadr row)) (natural? (caddr row))
                                             (or (not (cadddr row)) (natural? (cadddr row))))) updates))
      (error 'publish! "expected view publication entries" updates))
    (let loop ()
      (let ([records (map (lambda (row) (entry (car row))) updates)])
        (if (not (for-all (lambda (record row)
                            (and record (let ([old (value record)])
                                          (and (equal? actor (list-ref old 4)) (= (cadr row) (cadddr old))
                                               (> (caddr row) (list-ref old 5)))))) records updates))
            (values 'stale #f)
            (let-values ([(status current)
                          (model:commit! actor
                            (map (lambda (record row)
                                   (change record (interaction (value record) (cadr row) actor (caddr row)
                                                    (cadddr row) (list-ref row 4)))) records updates))])
              (if (eq? status 'stale) (loop) (values status #f)))))))

  (edoc "Change saved interaction only while the view has no mount owner. An active view must be operated through its owner."
        (actor actor "the actor") (id list "view model id") (basis (or integer #f) "model revision") (state datum "interaction state"))
  (define (set-state! actor id basis state)
    (update! actor id (lambda (old) (if (list-ref old 4) (values 'owned old)
                                      (values 'applied (interaction old (cadddr old) #f (list-ref old 5) basis state))))))

  (edoc "Release the matching owner generation, retaining its last acknowledged interaction."
        (actor actor "the head") (id list "view model id") (generation integer "the claimed generation"))
  (define (release! actor id generation)
    (update! actor id
      (lambda (old)
        (if (and (equal? actor (list-ref old 4)) (= generation (cadddr old)))
            (values 'applied (interaction old generation #f (list-ref old 5) (list-ref old 6) (list-ref old 7)))
            (values 'stale old)))))

  (edoc "Release a disconnected head's mounts, preserving saved view state."
        (actor actor "the disconnected head"))
  (define (release-owner! actor)
    (when (head? actor)
      (for-each (lambda (id) (let ([old (snapshot id)])
                               (when (and old (equal? actor (list-ref old 4))) (release! actor id (cadddr old))))) (model:ids 'widget-view))))

  (edoc "Clear saved mount owners after session restoration; ownership is a connection lifetime, not recoverable authority.")
  (define (reset-owners!)
    (for-each (lambda (id) (let ([old (snapshot id)])
                             (when (and old (list-ref old 4)) (release! (list-ref old 4) id (cadddr old))))) (model:ids 'widget-view)))
)
