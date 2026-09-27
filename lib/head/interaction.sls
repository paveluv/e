;; Immediate owner state. Only publication workers touch the connection;
;; acknowledged replies never overwrite a newer provisional selection.
(import (only (foundation edoc) elibrary))
(elibrary (head interaction)
  (export claim! flush! init! publish! release! set-state! snapshot)
  (import (chezscheme)
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
      (when (eq? (car reply) 'applied)
        (hashtable-set! owned (datum:copy id) (datum:copy (cadr reply))) (set! dirty? #t))
      (apply values reply)))

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
          (let ([next (if (equal? (list basis state) (list-tail old 6)) old
                          (datum:copy (append (list-head old 5) (list (+ 1 (list-ref old 5)) basis state))))])
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
                     (cons id (cons (cadddr descriptor) (list-tail descriptor 5))))
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
      (when (eq? (car reply) 'applied) (hashtable-delete! owned id) (set! dirty? #t))
      (apply values reply)))

  (edoc "Integrate interaction publication with presentation and lifecycle checkpoints.")
  (define (init!)
    (head:add-publication-hook! (lambda (fence?) (if fence? (flush!) (publish!)))))
)
