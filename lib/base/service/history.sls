;; Append-only references, indexed for ordinary bounded collection demand.
(import (only (foundation edoc) elibrary))
(elibrary (service history)
  (export append! create!)
  (import (except (chezscheme) append!) (prefix (core kernel) kernel:) (prefix (core work-queue) work-queue:) (prefix (state collection) collection:)
    (prefix (state model) model:))
  (define (get r k) (cdr (assq k r)))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (fields? v keys) (and (list? v) (for-all pair? v) (equal? (map car v) keys)))
  (define (document? x) (and (list? x) (= (length x) 2) (eq? (car x) 'buffer) (natural? (cadr x))))
  (define (recipe? x) (and (list? x) (= (length x) 3) (symbol? (car x)) (natural? (cadr x)) (> (cadr x) 0)))
  (define registrations
    (kernel:call-with-runtime-registrations
      (lambda ()
        (model:register-kind! 'history 1
          (lambda (v) (and (fields? v '(count tail)) (natural? (get v 'count))
                        (or (not (get v 'tail)) (model:reference? (get v 'tail))))))
        (model:register-kind! 'history-item 1
          (lambda (v) (and (fields? v '(index previous recipe projection)) (natural? (get v 'index))
                        (or (not (get v 'previous)) (model:reference? (get v 'previous)))
                        (recipe? (get v 'recipe)) (or (not (get v 'projection)) (document? (get v 'projection)))))))))

  (edoc "Create persistent or transient ordered history. Items borrow existing sources and jobs; no text or native values are copied into this model."
    (actor actor "creator") (persistence (one-of persistent transient) "recovery policy") (returns model) (public))
  (define (create! actor persistence)
    (model:create! actor 'history 1 'session persistence '() '((count . 0) (tail . #f))))

  (edoc "Append one explicit presentation recipe (kind schema data) at an expected history revision. Projection is borrowed text for copy/export, or false. References name borrowed jobs, environments and sources. Return the item ID, or false for stale history; never evaluate recipe data."
    (actor actor "caller") (id model "history") (revision integer "expected history revision")
    (recipe list "kind, schema and portable data") (projection datum "buffer reference or false")
    (references list "borrowed model/buffer references") (returns (or model #f)) (receiver id (model history)) (public))
  (define (append! actor id revision recipe projection references)
    (let ([r (model:snapshot id)])
      (unless (and r (eq? (get r 'kind) 'history)) (error 'append! "history is unavailable" id))
      (let* ([v (get r 'value)] [previous (get v 'tail)]
             [ids (model:allocate! actor 1
                    (lambda (ids)
                      (list (list 'history-item 1 id (get r 'persistence)
                              (append references (if projection (list projection) '()) (if previous (list previous) '()))
                              (list (cons 'index (get v 'count)) (cons 'previous previous)
                                (cons 'recipe recipe) (cons 'projection projection)))))
                    (lambda (ids) (list (list id revision ids
                                          (list (cons 'count (+ 1 (get v 'count))) (cons 'tail (car ids)))))))])
        (and ids (car ids)))))

  ;; A rebuild follows only uncached predecessors. Appending does not copy or
  ;; walk the existing index. Cache entries contain references/recipes, not text.
  (define indexes (make-hashtable equal-hash equal?))
  (define index-lock (make-mutex))
  (define worker (work-queue:create))
  (define demand
    (model:observe-demand!
      (lambda (ids)
        (with-mutex index-lock
          (for-each (lambda (id) (unless (model:demanded? id) (hashtable-delete! indexes id))) ids)))))
  (define (prepare! source query cancelled? publish check!)
    (unless (and (string=? (get query 'filter) "") (null? (get query 'sort)))
      (error 'history "History retains append order"))
    (let* ([id (get source 'id)] [v (get source 'value)] [count (get v 'count)]
           [index (with-mutex index-lock (or (hashtable-ref indexes id #f)
                                           (let ([index (cons (make-eqv-hashtable) (make-hashtable equal-hash equal?))])
                                             (hashtable-set! indexes id index) index)))])
      (let loop ([item (get v 'tail)] [pending '()])
        (check!)
        (cond [(cancelled?) (void)]
          [(or (not item) (with-mutex index-lock (hashtable-contains? (cdr index) item)))
           (with-mutex index-lock
             (for-each (lambda (p)
                         (hashtable-set! (car index) (get (cdr p) 'index) (list (car p) (list (cons 'item (cdr p))) '()))
                         (hashtable-set! (cdr index) (car p) (get (cdr p) 'index))) pending))]
          [else
           (let* ([r (model:snapshot item)] [v (and r (get r 'value))])
             (unless (and r (eq? (get r 'kind) 'history-item) (equal? (get r 'scope) id))
               (error 'history "History item is unavailable" item))
             (loop (get v 'previous) (cons (cons item v) pending)))]))
      (unless (cancelled?)
        (publish
          (collection:make-result '((item "Item" datum)) count
            (lambda (at) (with-mutex index-lock (hashtable-ref (car index) at #f)))
            (lambda (key) (with-mutex index-lock (let ([at (hashtable-ref (cdr index) key #f)]) (and at (< at count) at))))
            (lambda (at direction offset)
              (and (> count 0) (max 0 (min (- count 1) (+ at (if (eq? direction 'forward) offset (- offset)))))))
            '((sortable))) #f))))
  (define (start! source query cancelled? publish)
    (work-queue:submit! worker (get query 'id) (lambda () (not (cancelled?)))
      (lambda (check!) (prepare! source query cancelled? publish check!))
      (lambda (ex) (publish #f (kernel:condition-text ex)))))
  (define provider (collection:register! 'history 1 start!)))
