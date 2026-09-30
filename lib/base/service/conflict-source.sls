;; Bounded conflict rows over an explicit draft; no fitted text or head state.
(import (only (foundation edoc) elibrary))
(elibrary (service conflict-source)
  (export choose! choose-all! settle!)
  (import (chezscheme) (prefix (core kernel) kernel:) (prefix (core row) row:)
          (prefix (core work-queue) work-queue:) (prefix (foundation string) string:)
          (prefix (service conflict-review) conflict-review:)
          (prefix (state collection) collection:) (prefix (state model) model:) (prefix (state store) store:))
  (define (get r k) (cdr (assq k r)))
  (define worker (work-queue:create))
  (define lock (make-mutex))
  (define jobs (make-hashtable equal-hash equal?))
  (define-record-type job (fields query draft documents cancelled? publish (mutable stamp)))
  (define (record id)
    (let ([r (model:snapshot id)])
      (unless (and r (eq? (get r 'kind) 'conflict-review)) (error 'conflict-source "review is unavailable")) r))
  (define (alive? job)
    (and (with-mutex lock (eq? job (hashtable-ref jobs (job-query job) #f)))
      (model:demanded? (job-query job)) (not ((job-cancelled? job)))))
  (define (active-records r)
    (let ([v (get r 'value)])
      (map (lambda (d) (find (lambda (e) (= d (get e 'document))) (get v 'records))) (get v 'scope))))
  (define columns '((buffer "Buffer" string) (revision "Rev" integer) (actor "Actor" datum)
                    (position "At" datum) (mine "Mine" string) (disk "Disk" string) (choice "Choice" symbol)))
  (define (prepare! job check!)
    (let* ([r (record (job-draft job))]
           [r (conflict-review:refresh! (get r 'actor) (job-draft job) (get r 'revision) (get (get r 'value) 'scope))]
           [records (active-records r)] [names (map (lambda (e) (store:buffer-name (get e 'document))) records)]
           [stamp (list (get r 'revision) names)])
      (check!)
      (unless (equal? stamp (job-stamp job))
        (let* ([rows (list->vector
                       (apply append (map (lambda (e name)
                                            (map (lambda (c)
                                                   (check!)
                                                   (list (list (get e 'document) (car c))
                                                     (map cons '(buffer revision actor position mine disk choice)
                                                       (list name (car c) (cadr c) (list-head (cadddr c) 2)
                                                         (string:join (list-ref c 4) "\n") (string:join (list-ref c 5) "\n")
                                                         (if (memv (car c) (get e 'mine)) 'mine 'disk))) '())) (get e 'alternatives))) records names)))]
               [n (vector-length rows)] [positions (make-hashtable equal-hash equal?)])
          (do ([i 0 (+ i 1)]) ((= i n)) (check!) (hashtable-set! positions (car (vector-ref rows i)) i))
          (check!)
          ((job-publish job)
           (collection:make-result columns n (lambda (i) (vector-ref rows i)) (lambda (key) (hashtable-ref positions key #f))
             (lambda (at direction offset) (and (> n 0) (max 0 (min (- n 1) (+ at (if (eq? direction 'forward) offset (- offset)))))))
             (list '(sortable) (cons 'details (list (cons 'draft (job-draft job)) (cons 'revision (get r 'revision)))))) #f)
          (job-stamp-set! job stamp)))))
  (define (queue! job)
    (work-queue:submit! worker (job-query job) (lambda () (alive? job))
      (lambda (check!) (prepare! job check!))
      (lambda (ex) (job-stamp-set! job #f) ((job-publish job) #f (kernel:condition-text ex)))))
  (define changes
    (store:subscribe! #f
      (lambda (event)
        (for-each
          (lambda (job) (when (memv (cadr event) (job-documents job)) (queue! job)))
          (with-mutex lock (vector->list (hashtable-values jobs)))))))
  (define demand
    (model:observe-demand!
      (lambda (ids)
        (with-mutex lock
          (for-each (lambda (id) (unless (model:demanded? id) (hashtable-delete! jobs id))) ids)))))
  (define (start! source query cancelled? publish)
    (unless (and (string=? (get query 'filter) "") (null? (get query 'sort)))
      (error 'start! "Conflict reviews retain scope and revision order"))
    (let ([job (make-job (get query 'id) (get source 'id) (get (get source 'value) 'scope) cancelled? publish #f)])
      (with-mutex lock (hashtable-set! jobs (job-query job) job)) (queue! job)))
  (define provider (collection:register! 'conflict-review 1 start!))

  (define (review query generation basis)
    (let* ([meta (collection:summary query)] [v (and meta (get meta 'value))]
           [details (and v (assq 'details v))])
      (unless (and v details (eq? (get v 'status) 'ready) (= generation (get v 'generation))
                (equal? basis (get v 'basis)) (assq 'draft (cdr details)) (assq 'revision (cdr details)))
        (error 'conflict-source "shown review changed"))
      (let ([r (record (get (cdr details) 'draft))])
        (unless (and (equal? (get v 'source) (get r 'id)) (= (get r 'revision) (get (cdr details) 'revision)))
          (error 'conflict-source "review choices changed")) r)))

  (edoc "Choose a side for an exact displayed conflict row. The query generation, result basis and complete draft revision fence the choice; no alternative text crosses the command channel."
        (actor actor "caller") (selection row-selection "shown query, generation and conflict key") (basis datum "result basis")
        (side (one-of mine disk flip) "choice, or flip the reviewed choice"))
  (define (choose! actor selection basis side)
    (unless (row:selection? selection) (error 'choose! "expected a shown selection"))
    (let* ([r (review (car selection) (cadr selection) basis)] [key (caddr selection)]
           [row (collection:lookup (car selection) (cadr selection) key '())]
           [e (and (eq? (car row) 'ready) (pair? (list-ref row 4))
                (find (lambda (e) (= (car key) (get e 'document))) (active-records r)))])
      (unless e (error 'choose! "conflict row is unavailable"))
      (conflict-review:choose! actor (get r 'id) (get r 'revision)
        (list (list (car key) (assv (cadr key) (get e 'alternatives))))
        (if (eq? side 'flip) (if (memv (cadr key) (get e 'mine)) 'disk 'mine) side)) (void)))

  (edoc "Choose one side for all documents in the exact displayed review, without settling. An incompatible Mine group refuses before any choices change."
        (actor actor "caller") (query row-source "review rows") (generation integer "shown generation")
        (basis datum "shown result basis") (side (one-of mine disk) "choice"))
  (define (choose-all! actor query generation basis side)
    (let ([r (review query generation basis)])
      (conflict-review:choose! actor (get r 'id) (get r 'revision)
        (map (lambda (e) (cons (get e 'document) (get e 'alternatives))) (active-records r)) side) (void)))

  (edoc "Settle the complete displayed review. Refuse an outdated listing; return each scoped document's settlement result."
        (actor actor "caller") (query row-source "review rows") (generation integer "shown generation")
        (basis datum "shown result basis") (returns list))
  (define (settle! actor query generation basis)
    (let ([r (review query generation basis)])
      (conflict-review:settle! actor (get r 'id) (get r 'revision) (get (get r 'value) 'scope)))))
