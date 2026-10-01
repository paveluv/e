;; Immediate owner state. Only publication workers touch the connection;
;; acknowledged replies never overwrite a newer provisional selection.
(import (only (foundation edoc) elibrary))
(elibrary (head interaction)
  (export adopt! arrange! bind! call-with-preview claim! flush! focus! publish! reconcile! release! set-state! snapshot start!)
  (import (chezscheme)
          (prefix (core descriptor) descriptor:)
          (prefix (core identity) identity:)
          (prefix (core publication) publication:)
          (prefix (foundation datum) datum:)
          (prefix (state connection) connection:)
          (prefix (state view) view:))

  (define owned (make-hashtable equal-hash equal?))
  ;; Model notifications can arrive on a publication/client worker. Reading a
  ;; local sequence and replacing its descriptor must be one operation; an
  ;; acknowledgement must not overwrite a newer UI selection between them.
  (define owned-lock (make-mutex))
  (define dirty? #f)
  (define queued '())
  (define owner #f)
  (define wake! void)
  (define writer #f)
  (define preview (make-parameter #f))

  (edoc "Prepare against private descriptor copies without claiming or publishing interaction. Local normalization is discarded; canonical rows remain the admission basis."
        (rows list "candidate descriptor rows") (thunk thunk "local preparation") (returns any))
  (define (call-with-preview rows thunk)
    (let ([table (make-hashtable equal-hash equal?)])
      (for-each (lambda (row) (hashtable-set! table (car row) (datum:copy (cdr row)))) rows)
      (parameterize ([preview table]) (thunk))))
  (define (state-table id)
    (if (and (preview) (hashtable-contains? (preview) id)) (preview) owned))

  (edoc "Start this head's interaction publisher with an explicit actor and failure wakeup. Repeated startup for the same actor is harmless; a different actor requires another head runtime."
        (actor actor "head identity") (notify! thunk "wake the owner on publication failure"))
  (define (start! actor notify!)
    (unless (and (identity:valid? actor) (eq? (car actor) 'head) (procedure? notify!))
      (error 'start! "expected a head identity and wakeup procedure"))
    (when (and owner (not (equal? owner actor))) (error 'start! "interaction publisher already belongs to another actor"))
    (set! owner (datum:copy actor)) (set! wake! notify!)
    (unless writer
      (set! writer (publication:make!
                     (lambda (batch previous)
                       (let ([changes (filter (lambda (row) (not (member row (or previous '())))) batch)])
                         (unless (null? changes)
                           (let ([reply (call-with-values (lambda () (view:publish! owner changes)) list)])
                             (unless (eq? (car reply) 'applied)
                               (error 'interaction:publish! "view ownership or sequence changed" reply))))))
                     (lambda () (wake!))
                     datum:copy))))

  (edoc "Claim an unowned root, or reuse its current local ownership; return status and descriptor. Parented children refuse. Call from the head's pump thread."
        (actor actor "attribution is supplied by the connection") (id model "view model id"))
  (define (claim! actor id)
    (unless (equal? owner actor) (error 'claim! "start this actor's interaction publisher before claiming a view"))
    (let ([old (snapshot id)])
      (if old
        (values (if (view:parent old) 'parented 'applied) old)
        (let ([reply (call-with-values (lambda () (view:claim! actor id)) list)])
          (when (eq? (car reply) 'applied) (adopt! (cadr reply)))
          (values (car reply) (snapshot id))))))

  (edoc "Adopt coherent admitted ownership rows without claiming again. False descriptors forget retired views; surviving rows follow their canonical owner. Call after fencing prior publication."
        (rows list "(id . descriptor-or-false) entries"))
  (define (adopt! rows)
    (with-mutex owned-lock
      (for-each (lambda (row)
                  (if (and (cdr row) (equal? owner (view:owner (cdr row))))
                    (hashtable-set! owned (datum:copy (car row)) (datum:copy (cdr row)))
                    (hashtable-delete! owned (car row)))) rows)
      (set! dirty? #t)))

  (edoc "Reconcile acquired canonical descriptors with provisional interaction. Preserve newer local state only in the same ownership generation; retirement and release revoke it. This consumes mirrors without remote reads."
        (rows list "(id . descriptor-or-false) entries"))
  (define (reconcile! rows)
    (with-mutex owned-lock
      (for-each
        (lambda (row)
          (let* ([id (car row)] [canonical (cdr row)] [local (hashtable-ref owned id #f)])
            (when (and local (or (not canonical) (>= (view:generation canonical) (view:generation local))))
              (let ([next (and canonical (equal? owner (view:owner canonical))
                            (if (and (= (view:generation local) (view:generation canonical))
                                  (> (view:sequence local) (view:sequence canonical)))
                              (descriptor:with canonical
                                (map (lambda (key) (assq key local)) '(sequence basis state focus))) canonical))])
                (unless (equal? local next)
                  (if next (hashtable-set! owned id (datum:copy next)) (hashtable-delete! owned id))
                  (set! dirty? #t)))))) rows)
      ;; A retired descendant may have been the provisional focus of a parent
      ;; whose newer selection has not reached the base yet.
      (vector-for-each
        (lambda (id)
          (let* ([d (hashtable-ref owned id #f)] [focus (view:focus d)])
            (when (and focus
                    (not (let loop ([at focus] [seen '()])
                           (and (not (member at seen))
                             (let ([target (hashtable-ref owned at #f)])
                               (and target (if (view:parent target) (loop (view:parent target) (cons at seen)) (equal? at id))))))))
              (hashtable-set! owned id (descriptor:with d '((focus . #f)))) (set! dirty? #t))))
        (hashtable-keys owned))))

  (edoc "Fence interaction and atomically arrange a tree through its owner."
        (actor actor "head") (changes list "parent changes") (leases list "root guards"))
  (define (arrange! actor changes leases)
    (flush!)
    (let-values ([(status rows) (view:arrange! actor changes leases)])
      (when (eq? status 'applied) (adopt! rows)) (values status rows)))

  (edoc "Fence owned interaction before rewiring a composition and adopt its new input generations."
        (actor actor "owner") (owner model "containing view") (changes list "connection input changes") (public))
  (define (bind! actor owner changes)
    (flush!)
    (let* ([leases (filter values (map (lambda (c)
                                         (let ([d (snapshot (car c))])
                                           (and d (list (car c) (view:generation d) (view:sequence d))))) changes))]
           [result (call-with-values (lambda () (connection:bind! actor owner changes leases)) list)])
      (when (eq? (car result) 'applied) (adopt! (view:tree owner)))
      (apply values result)))

  (edoc "Set the owned root's logical focus target locally."
        (id model "root") (target any "descendant or #f"))
  (define (focus! id target)
    (with-mutex owned-lock
      (let* ([table (state-table id)] [d (hashtable-ref table id #f)])
        (unless d (error 'focus! "root is not owned" id))
        (unless (equal? target (view:focus d))
          (hashtable-set! table id (descriptor:with d (list (cons 'focus target) (cons 'sequence (+ 1 (view:sequence d))))))
          (when (eq? table owned) (set! dirty? #t))))))

  (edoc "Read the owned view's latest provisional descriptor locally. An unclaimed view returns #f."
        (id model "view model id") (returns (or list #f)))
  (define (snapshot id)
    (with-mutex owned-lock (datum:copy (hashtable-ref (state-table id) id #f))))

  (edoc "Change interaction immediately for an owned view, without waiting for publication. Otherwise update saved unmounted state at the base. Activation must carry this actual state and model basis, not reread saved selection."
        (actor actor "attribution is supplied by the connection") (id model "view model id")
        (basis (or integer #f) "model revision") (state datum "interaction state"))
  (define (set-state! actor id basis state)
    (unless (or (not basis) (and (integer? basis) (exact? basis) (>= basis 0))) (error 'set-state! "invalid basis" basis))
    (let ([result
           (with-mutex owned-lock
             (let* ([table (state-table id)] [old (hashtable-ref table id #f)])
               (and old
                 (let ([next (if (equal? (list basis state) (list (view:basis old) (view:state old))) old
                               (descriptor:with old (list (cons 'sequence (+ 1 (view:sequence old)))
                                                      (cons 'basis basis) (cons 'state state))))])
                   (unless (eq? old next)
                     (hashtable-set! table id next) (when (eq? table owned) (set! dirty? #t)))
                   (list 'applied (datum:copy next))))))])
      (if result (apply values result) (view:set-state! actor id basis state))))

  (edoc "Queue the latest interaction after presentation or dispatch. One in-flight batch and one replacement serve every owned view; unchanged views send nothing.")
  (define (publish!)
    (let ([batch (with-mutex owned-lock
                   (when dirty?
                     (let-values ([(ids descriptors) (hashtable-entries owned)])
                       (set! queued
                         (list-sort (lambda (a b) (< (cadar a) (cadar b)))
                           (filter (lambda (row) (> (caddr row) 0))
                             (map (lambda (id descriptor)
                                    (list id (view:generation descriptor) (view:sequence descriptor)
                                      (view:basis descriptor) (view:state descriptor) (view:focus descriptor)))
                               (vector->list ids) (vector->list descriptors))))))
                     (set! dirty? #f)) queued)])
      (when writer (publication:submit! writer batch))))

  (edoc "Publish and fence acknowledged view state before detach or release." (effects remote))
  (define (flush!) (publish!) (when writer (publication:flush! writer)))

  (edoc "Fence publication and release an owned mount, keeping its acknowledged state for resume."
        (actor actor "attribution is supplied by the connection") (id model "view model id") (generation integer "the claimed generation"))
  (define (release! actor id generation)
    (flush!)
    (let ([reply (call-with-values (lambda () (view:release! actor id generation)) list)])
      (when (eq? (car reply) 'applied) (adopt! (cadr reply)))
      (apply values reply)))

)
