;; Parsed Markdown is a shared, demand-scoped base index, never fitted text.
(import (only (foundation edoc) elibrary))
(elibrary (service markup-source)
  (export create!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation markup) markup:) (prefix (state collection) collection:)
          (prefix (state model) model:) (prefix (state store) store:))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))
  (define (recipe? v)
    (and (list? v) (= (length v) 2) (for-all pair? v)
      (equal? (map car v) '(owner document))
      (integer? (get v 'document #f)) (exact? (get v 'document #f)) (> (get v 'document #f) 0)))
  (define kind (model:register-kind! 'markup-source 1 recipe?))
  (define lock (make-mutex))
  (define ready (make-condition))
  (define jobs (make-hashtable equal-hash equal?))
  (define documents (make-eqv-hashtable))
  (define dirty? #f)
  (define running? #f)
  ;; Only the worker touches parsed snapshots. Multiple queries/widths reuse
  ;; the same revision; cancelling the last query releases its interpretation.
  (define cache (make-eqv-hashtable))
  (define (wake!) (with-mutex lock (set! dirty? #t) (condition-signal ready)))
  (define changes
    (store:subscribe! #f
      (lambda (event)
        (when (with-mutex lock (hashtable-contains? documents (cadr event))) (wake!)))))
  (define demand
    (model:observe-demand!
      (lambda (ids)
        (when (with-mutex lock (exists (lambda (id) (hashtable-contains? jobs id)) ids)) (wake!)))))
  (define (prepared id)
    (let-values ([(lines revision) (store:snapshot id)])
      (let ([old (hashtable-ref cache id #f)])
        (if (and old (= (car old) revision)) old
          (let* ([blocks (list->vector (markup:parse (vector->list lines)))]
                 [n (vector-length blocks)] [positions (make-eqv-hashtable)]
                 [result
                  (collection:make-result '((block "Block" datum)) n
                    (lambda (i) (let ([b (vector-ref blocks i)]) (list (cadr b) (list (cons 'block b)) '())))
                    (lambda (key) (hashtable-ref positions key #f))
                    (lambda (at direction offset)
                      (and (> n 0) (max 0 (min (- n 1) (+ at (if (eq? direction 'forward) offset (- offset)))))))
                    (list '(sortable) (cons 'details (list (cons 'document id) (cons 'revision revision)))))])
            (do ([i 0 (+ i 1)]) ((= i n)) (hashtable-set! positions (cadr (vector-ref blocks i)) i))
            (let ([entry (cons revision result)]) (hashtable-set! cache id entry) entry))))))
  (define (work!)
    (let loop ()
      (let ([active (with-mutex lock
                      (let wait () (unless dirty? (condition-wait ready lock) (wait)))
                      (set! dirty? #f) (vector->list (hashtable-values jobs)))])
        (for-each
          (lambda (job)
            (let ([query (car job)] [recipe (cadr job)] [cancelled? (caddr job)] [publish (cadddr job)])
              (if (or (not (model:demanded? query)) (cancelled?))
                (with-mutex lock (when (eq? job (hashtable-ref jobs query #f)) (hashtable-delete! jobs query)))
                (guard (ex [else (unless (cancelled?) (publish #f (kernel:condition-text ex)))])
                  (let ([id (get recipe 'document #f)] [owner (get recipe 'owner #f)])
                    (if (not (store:visible? owner id)) (publish #f "Markdown source is unavailable")
                      (let ([entry (prepared id)])
                        ;; A raced edit is queued by the store observer; never
                        ;; publish a known obsolete interpretation meanwhile.
                        (when (and (not (cancelled?)) (= (car entry) (store:revision id)))
                          (publish (cdr entry) #f))))))))) active)
        (let ([retained (with-mutex lock
                          (hashtable-clear! documents)
                          (vector-for-each (lambda (job) (hashtable-set! documents (get (cadr job) 'document #f) #t))
                            (hashtable-values jobs))
                          (vector->list (hashtable-keys documents)))])
          (vector-for-each (lambda (id) (unless (memv id retained) (hashtable-delete! cache id))) (hashtable-keys cache))))
      (loop)))
  (define (start! source query cancelled? publish)
    (unless (and (string=? (get query 'filter "") "") (null? (get query 'sort '())))
      (error 'start! "Markdown block order is source order"))
    (let ([recipe (get source 'value '())])
      (with-mutex lock
        (hashtable-set! jobs (get query 'id #f) (list (get query 'id #f) recipe cancelled? publish))
        (hashtable-set! documents (get recipe 'document #f) #t)
        (set! dirty? #t)
        (unless running? (set! running? #t) (fork-thread work!))
        (condition-signal ready))))
  (define provider (collection:register! 'markup-source 1 start!))

  (edoc "Create a collection of portable Markdown blocks over a borrowed document. Its interpretation is shared by revision and released when no query is demanded; geometry stays in each head."
        (actor actor "requesting actor") (document integer "source document") (returns row-source))
  (define (create! actor document)
    (unless (store:visible? actor document) (error 'create! "Markdown source is unavailable" document))
    (let ([source (model:create! actor 'markup-source 1 'session 'persistent (list (list 'buffer document))
                    (list (cons 'owner actor) (cons 'document document)))])
      (collection:create! actor source "" '() 'persistent (list source)))))
