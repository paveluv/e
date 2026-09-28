;; Immediate owner state. Only publication workers touch the connection;
;; acknowledged replies never overwrite a newer provisional selection.
(import (only (foundation edoc) elibrary))
(elibrary (head interaction)
  (export arrange! claim! flush! focus! init! publish! release! set-state! snapshot)
  (import (chezscheme)
          (prefix (core descriptor) descriptor:)
          (prefix (core publication) publication:)
          (prefix (foundation datum) datum:)
          (prefix (head head) head:)
          (prefix (state view) view:))

  (define owned (make-hashtable equal-hash equal?))
  (define dirty? #f)
  (define queued '())
  (define writer
    (publication:make!
      (lambda (batch previous)
        (let ([changes (filter (lambda (row) (not (member row (or previous '())))) batch)])
          (unless (null? changes)
            (let ([reply (call-with-values (lambda () (view:publish! head:ui-actor changes)) list)])
              (unless (eq? (car reply) 'applied)
                (error 'interaction:publish! "view ownership or sequence changed" reply))))))
      head:wake-main!
      datum:copy))

  (edoc "Claim an unmounted view; return status and descriptor. Call from the head's pump thread."
        (actor actor "attribution is supplied by the connection") (id list "view model id"))
  (define (claim! actor id)
    (let ([reply (call-with-values (lambda () (view:claim! actor id)) list)])
      (when (eq? (car reply) 'applied) (adopt! (cadr reply)))
      (values (car reply) (snapshot id))))

  (define (adopt! rows)
    (for-each (lambda (row)
                (if (equal? head:ui-actor (view:owner (cdr row)))
                    (hashtable-set! owned (datum:copy (car row)) (datum:copy (cdr row)))
                    (hashtable-delete! owned (car row)))) rows)
    (set! dirty? #t))

  (edoc "Fence interaction and atomically arrange a tree through its owner."
        (actor actor "head") (changes list "parent changes") (leases list "root guards"))
  (define (arrange! actor changes leases)
    (flush!)
    (let-values ([(status rows) (view:arrange! actor changes leases)])
      (when (eq? status 'applied) (adopt! rows)) (values status rows)))

  (edoc "Set the owned root's logical focus target locally."
        (id list "root") (target any "descendant or #f"))
  (define (focus! id target)
    (let ([d (snapshot id)])
      (unless d (error 'focus! "root is not owned" id))
      (unless (equal? target (view:focus d))
        (hashtable-set! owned id (descriptor:with d (list (cons 'focus target) (cons 'sequence (+ 1 (view:sequence d))))))
        (set! dirty? #t))))

  (edoc "Read the owned view's latest provisional descriptor locally. An unclaimed view returns #f."
        (id list "view model id") (returns (or list #f)))
  (define (snapshot id) (datum:copy (hashtable-ref owned id #f)))

  (edoc "Change interaction immediately for an owned view, without waiting for publication. Otherwise update saved unmounted state at the base. Activation must carry this actual state and model basis, not reread saved selection."
        (actor actor "attribution is supplied by the connection") (id list "view model id")
        (basis (or integer #f) "model revision") (state datum "interaction state"))
  (define (set-state! actor id basis state)
    (unless (or (not basis) (and (integer? basis) (exact? basis) (>= basis 0))) (error 'set-state! "invalid basis" basis))
    (let ([old (hashtable-ref owned id #f)])
      (if old
          (let ([next (if (equal? (list basis state) (list (view:basis old) (view:state old))) old
                          (descriptor:with old (list (cons 'sequence (+ 1 (view:sequence old)))
                                                     (cons 'basis basis) (cons 'state state))))])
            (unless (eq? old next) (hashtable-set! owned id next) (set! dirty? #t))
            (values 'applied (datum:copy next)))
          (view:set-state! actor id basis state))))

  (edoc "Queue the latest interaction after presentation or dispatch. One in-flight batch and one replacement serve every owned view; unchanged views send nothing.")
  (define (publish!)
    (when dirty?
      (let-values ([(ids descriptors) (hashtable-entries owned)])
        (set! queued
          (list-sort (lambda (a b) (< (cadar a) (cadar b)))
            (filter (lambda (row) (> (caddr row) 0))
              (map (lambda (id descriptor)
                     (list id (view:generation descriptor) (view:sequence descriptor)
                           (view:basis descriptor) (view:state descriptor) (view:focus descriptor)))
                (vector->list ids) (vector->list descriptors))))))
      (set! dirty? #f))
    (publication:submit! writer queued))

  (edoc "Publish and fence acknowledged view state before detach or release." (effects remote))
  (define (flush!) (publish!) (publication:flush! writer))

  (edoc "Fence publication and release an owned mount, keeping its acknowledged state for resume."
        (actor actor "attribution is supplied by the connection") (id list "view model id") (generation integer "the claimed generation"))
  (define (release! actor id generation)
    (flush!)
    (let ([reply (call-with-values (lambda () (view:release! actor id generation)) list)])
      (when (eq? (car reply) 'applied) (adopt! (cadr reply)))
      (apply values reply)))

  (edoc "Integrate interaction publication with presentation and lifecycle checkpoints.")
  (define (init!)
    (head:add-publication-hook! (lambda (fence?) (if fence? (flush!) (publish!)))))
)
