;; Included in the existing two-real-head scenario. Run the shipped example.
(let* ([example (string-append (current-directory) "/examples/widgets.e")]
       [root-a (head-read a `(begin (load ,example) (widget-example:open! '("alpha.sls" "beta.ss" "gamma.e"))))]
       [table-a (head-read a `(cadr (assq 'table (view:children (interaction:snapshot ',root-a)))))]
       [filter-a (head-read a `(cadr (assq 'filter (view:children (interaction:snapshot ',root-a)))))]
       [entry-a (head-read a `(cadr (assq 'entry (view:children (interaction:snapshot ',filter-a)))))]
       [answer-a (head-read a `(cadr (assq 'answer (view:children (interaction:snapshot ',root-a)))))]
       [query (head-read a `(view:source (interaction:snapshot ',table-a)))]
       [root-b (head-read b `(begin (load ,example)
                               (let ([root (view:fork! head:ui-actor ',root-a)])
                                 (window:show-widget! (head:current-window) root) root)))]
       [table-b (head-read b `(cadr (assq 'table (view:children (interaction:snapshot ',root-b)))))])
  (define (key ui table)
    (head-read ui `(let ([s (cdr (assq 'selection (view:state (interaction:snapshot ',table))))]) (and s (caddr s)))))
  (for-each (lambda (ui table) (head-wait 'collection-first-page ui (lambda () (equal? (key ui table) "alpha.sls")))) (list a b) (list table-a table-b))
  (test:check 'collection-real-head-selection-and-immediate-activation
    (head-read a `(begin (table:move! ',table-a 'next) (table:activate! ',table-a)
                    (let-values ([(source d inputs) (widget:context ',answer-a)])
                      (vector-ref (cdr (assq 'value source)) 0)))) "beta.ss")
  (test:check 'collection-fork-keeps-selection-independent (key b table-b) "alpha.sls")
  (test:check 'collection-example-action-uses-real-undo
    (head-read a `(begin (entry:undo! ',answer-a)
                    (let-values ([(source d inputs) (widget:context ',answer-a)]) (vector-ref (cdr (assq 'value source)) 0)))) "")
  (test:check 'collection-warm-frames-send-no-wire-data
    (head-read a
      `(let ([io (lambda () (call-with-input-file "/proc/self/io"
                              (lambda (p) (let loop () (let* ([k (read p)] [v (read p)])
                                                         (if (eq? k 'wchar:) v (loop)))))))])
         (let ([before (io)])
           (do ([i 0 (+ i 1)]) ((= i 100)) (widget:prepare! ',root-a (+ 20 (modulo i 20)) 10))
           (- (io) before)))) 0)
  (head-read a `(begin (entry:insert! ',entry-a "beta") #t))
  (for-each (lambda (ui table) (head-wait 'connected-filter-across-heads ui
                                 (lambda () (and (equal? (key ui table) "beta.ss")
                                              (= (head-read ui `(cdr (assq 'count (cdr (assq 'value (range:summary ',query)))))) 1)))))
    (list a b) (list table-a table-b))
  (test:check 'collection-head-contract-mismatch-is-unavailable
    (head-read b
      `(begin
         (kernel:retract-module! 'row)
         ((eval 'restore-types! (environment '(foundation edoc))) "(core row)")
         (parameterize ([kernel:registering-module 'row])
           (port:register! '(view table 1) '((input rows row-source (options rows)) (output selection (or row-selection #f) (state selection)))))
         (car (connection:read ',table-b 'rows)))) 'unavailable)
  (head-read b '(begin (kernel:retract-module! 'row)
                       ((eval 'restore-types! (environment '(foundation edoc))) "(core row)") (row:init!) #t))
  (head-wait 'collection-contract-restored b (lambda () (equal? (key b table-b) "beta.ss")))
  ;; Release and remount the same descriptor, preserving the canonical recipe
  ;; and selection while disposing all head-only range/cache resources.
  (head-read b `(begin (widget:unmount! ',root-b) (widget:mount! ',root-b 'remounted-collection) #t))
  (head-wait 'collection-remount b (lambda () (equal? (key b table-b) "beta.ss")))
  (test:check 'collection-disconnect-restores-filter-default
    (rpc head 'connection-bind query (list (list query 'filter (list filter-a 'text) #f)))
    (list 'applied '()))
  (head-wait 'collection-default-restored a
    (lambda () (= (head-read a `(cdr (assq 'count (cdr (assq 'value (range:summary ',query)))))) 3)))
  (for-each
    (lambda (ui root)
      (head-read ui `(begin
                       (for-each head:forget-buffer! (filter (lambda (b) (equal? ',root (head:buffer-fact b 'widget-id #f))) (head:buffers)))
                       (head:show-buffer! (head:adopt-store-buffer! ,id)) #t))) (list a b) (list root-a root-b)))
