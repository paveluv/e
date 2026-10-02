;; Bounded, demand-owned journal indexes. Formatting remains in each head.
(import (only (foundation edoc) elibrary))
(elibrary (service journal-source)
  (export create!)
  (import (chezscheme) (prefix (core work-queue) work-queue:)
          (prefix (state collection) collection:) (prefix (state construction) construction:) (prefix (state journal) journal:)
          (prefix (state model) model:))
  (define limit 4096)
  (define epoch (gensym->unique-string (gensym "journal")))
  (define (get r k) (cdr (assq k r)))
  (define kind (model:register-kind! 'journal-source 1
                 (lambda (v) (and (list? v) (= (length v) 1) (pair? (car v)) (eq? (caar v) 'component)
                               (or (not (cdar v)) (symbol? (cdar v)))))))
  (define worker (work-queue:create))
  (define lock (make-mutex))
  (define jobs (make-hashtable equal-hash equal?))
  ;; Cache belongs to one provider ticket: (end first indexed-records).
  (define-record-type job (fields id component cancelled? publish (mutable cache)))
  (define (alive? job)
    (and (with-mutex lock (eq? job (hashtable-ref jobs (job-id job) #f)))
      (not ((job-cancelled? job))) (model:demanded? (job-id job))))
  (define (prepare! job check!)
    (let* ([old (job-cache job)] [start (if old (car old) 0)])
      (let-values ([(records end first) (journal:indexed-snapshot start limit #f #f)])
        (let ([first (max first (- end limit))])
          (unless (and old (= end (car old)) (= first (cadr old)))
            (let* ([selected (filter (lambda (p) (and (>= (car p) first)
                                                   (or (not (job-component job)) (eq? (cadddr p) (job-component job)))))
                               (append (if old (caddr old) '()) (reverse records)))]
                   [rows (list->vector selected)] [count (vector-length rows)] [positions (make-hashtable equal-hash equal?)]
                   [result (collection:make-result '((entry "Record" datum)) count
                             (lambda (i) (let ([p (vector-ref rows i)]) (list (list epoch (car p)) (list (cons 'entry (cdr p))) '())))
                             (lambda (key) (hashtable-ref positions key #f))
                             (lambda (at direction offset)
                               (and (> count 0) (max 0 (min (- count 1) (+ at (if (eq? direction 'forward) offset (- offset)))))))
                             (list '(sortable) (cons 'details (list (cons 'first first) (cons 'end end) (cons 'epoch epoch)))) )])
              (do ([i 0 (+ i 1)]) ((= i count)) (check!) (hashtable-set! positions (list epoch (car (vector-ref rows i))) i))
              (check!)
              ((job-publish job) result #f)
              (job-cache-set! job (list end first selected))))))))
  (define (queue! job)
    (work-queue:submit! worker (job-id job) (lambda () (alive? job))
      (lambda (check!) (prepare! job check!))
      (lambda (ex) ((job-publish job) #f "Journal records are unavailable"))))
  (define changes
    (journal:observe!
      (lambda () (for-each queue! (with-mutex lock (vector->list (hashtable-values jobs)))))))
  (define demand
    (model:observe-demand!
      (lambda (ids)
        (with-mutex lock
          (for-each (lambda (id) (unless (model:demanded? id) (hashtable-delete! jobs id))) ids)))))
  (define (start! source query cancelled? publish)
    (unless (and (string=? (get query 'filter) "") (null? (get query 'sort)))
      (error 'start! "Journal records retain append order"))
    (let ([job (make-job (get query 'id) (get (get source 'value) 'component) cancelled? publish #f)])
      (with-mutex lock (hashtable-set! jobs (job-id job) job)) (queue! job)))
  (define provider (collection:register! 'journal-source 1 start!))

  (edoc "Create a demand-owned journal collection over at most the latest 4096 retained records, optionally narrowed to a component. Keys include the journal lifetime and absolute append index; each view owns its scrolling."
        (actor actor "creator") (component (or symbol #f) "component, or all") (returns row-source))
  (define (create! actor component)
    (construction:call! actor
      (lambda (remember!)
        (let ([source (remember! (model:create! actor 'journal-source 1 'session 'persistent '() (list (cons 'component component))))])
          (remember! (collection:create! actor source "" '() 'persistent (list source))))))))
