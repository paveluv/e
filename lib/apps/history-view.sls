;; A bounded page of independently hosted children, not a second text store.
(import (only (foundation edoc) elibrary))
(elibrary (apps history-view)
  (export create! init! move! projection register!)
  (import (chezscheme) (prefix (apps eval) eval:) (prefix (core kernel) kernel:)
    (prefix (foundation text) text:)
    (prefix (head edit) edit:) (prefix (head head) head:) (prefix (head interaction) interaction:)
    (prefix (head keymap) keymap:) (prefix (head layout) layout:) (prefix (head range) range:)
    (prefix (head widget) widget:) (prefix (state collection) collection:)
    (prefix (state model) model:) (prefix (state store) store:) (prefix (state view) view:))
  (define factories (kernel:make-registry car))
  (define sessions (make-hashtable equal-hash equal?))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))
  (define (record id) (caddar (cadr (model:snapshots (list id)))))
  (define (revision id) (get (record id) 'revision #f))

  (edoc "Register an explicit history presentation recipe. The module-owned factory receives portable data and returns a fresh unmounted view. Registration never evaluates saved data; absent definitions show an inert marker."
    (kind symbol "recipe kind") (schema integer "positive version") (factory procedure "data -> unmounted view") (public))
  (define (register! kind schema factory)
    (unless (and (symbol? kind) (integer? schema) (positive? schema) (procedure? factory))
      (error 'register! "expected kind, positive schema and factory"))
    (kernel:registry-add! factories (cons (list kind schema) factory)))
  (define (factory recipe) (kernel:registry-find factories (lambda (p) (equal? (car p) (list-head recipe 2)))))
  (define (release! id)
    (let ([s (hashtable-ref sessions id #f)])
      (when s (range:release! (car s)) (hashtable-delete! sessions id))))
  (define (child! data)
    (let* ([recipe (get data 'recipe #f)] [f (factory recipe)])
      (if f ((cdr f) (caddr recipe))
        (view:create! head:ui-actor #f 'label 1
          (list (cons 'text (format "[Unavailable presentation: ~a ~a]" (car recipe) (cadr recipe))) '(face . ghost)
            '(history-missing . #t)) '()))))
  (define (service! id frame)
    (let* ([d (interaction:snapshot id)] [query (view:source d)]
           [token (or (hashtable-ref sessions id #f)
                    (let ([s (list (range:acquire! query (lambda () (widget:repaint! id #t))) #f)])
                      (hashtable-set! sessions id s) s))]
           [summary (range:summary query)] [v (and summary (get summary 'value '()))]
           [count (and v (get v 'count 0))]
           [offset (car (view:state d))] [size (get (view:options d) 'page-size 3)])
      (when (and v (eq? (get v 'status #f) 'ready))
        (let* ([g (get v 'generation 0)] [start (min offset (max 0 (- count 1)))]
               [reply (range:read query g start size '(item))])
          (range:request! (car token) g start size '(item) '())
          (when (and reply (eq? (car reply) 'ready))
            (let* ([rows (list-ref reply 4)]
                   [ready? (for-all (lambda (r) (eq? (cadr (assq 'item (caddr r))) 'ready)) rows)]
                   [signature (and ready? (map (lambda (r)
                                                 (list (cadr r) (factory (get (caddr (assq 'item (caddr r))) 'recipe #f)))) rows))])
              (when (and ready? (not (equal? signature (cadr token))))
                ;; A restored page keeps its saved child interaction until its
                ;; item set or definition changes. Off-page views are disposable;
                ;; their source documents/jobs are always borrowed.
                (let* ([keys (map cadr rows)] [old (cdr (view:children d))]
                       [same? (and (not (cadr token)) (equal? keys (get (view:options d) 'items #f))
                                (= (length old) (length keys))
                                   (for-all (lambda (c r)
                                              (eq? (and (factory (get (caddr (assq 'item (caddr r))) 'recipe #f)) #t)
                                                (not (get (view:options (view:snapshot (cadr c))) 'history-missing #f)))) old rows))]
                       [children (if same? old
                                   (map (lambda (r i) (list (string->symbol (format "item-~a" i))
                                                        (child! (caddr (assq 'item (caddr r)))) '(grow 1))) rows (iota (length rows))))]
                       [options (list (cons 'page-size size) (cons 'items keys))])
                  (if same? (set-car! (cdr token) signature)
                    (begin
                      (interaction:flush!)
                      (let-values ([(status rows) (widget:arrange!
                                                    (list (list id (revision id) (cons (car (view:children d)) children) options)))])
                        (for-each (lambda (c) (let-values ([(status row) (view:retire! head:ui-actor (cadr c) (revision (cadr c)))]) (void)))
                          (if (eq? status 'applied) old children))
                        (when (eq? status 'applied) (set-car! (cdr token) signature)))))))))))))

  (edoc "Move a history view's first item by a signed item count. Each view retains its own logical page anchor; no geometry or history scan is published."
    (id model "history view") (delta integer "signed item count") (receiver id (view history)) (public))
  (define (move! id delta)
    (let* ([d (interaction:snapshot id)] [s (range:summary (view:source d))]
           [v (and s (get s 'value '()))])
      (when v
        (interaction:set-state! head:ui-actor id (get v 'generation 0)
          (list (max 0 (min (max 0 (- (get v 'count 0) 1)) (+ (car (view:state d)) delta))))))))

  (edoc "Read an item's explicit borrowed text projection for copy/export. A nontext item without a projection returns an unavailable marker, never its recipe or object representation."
    (item model "history item") (returns string) (effects remote) (public))
  (define (projection item)
    (let* ([r (record item)] [v (and r (get r 'value '()))]
           [ref (and r (eq? (get r 'kind #f) 'history-item) (get v 'projection #f))])
      (if (and ref (store:exists? (cadr ref)))
        (let-values ([(lines revision facts) (store:snapshot-state (cadr ref))])
          (text:to-string lines (get facts 'trailing #f)))
        "[Text projection unavailable]")))

  (edoc "Create an independently paged history composition. At most page-size child presentations are mounted; sizing and typography remain in the head. Source items/jobs survive hiding and view retirement."
    (source model "history") (page-size integer "1 through 16 visible items") (returns model) (public))
  (define (create! source page-size)
    (unless (and (integer? page-size) (<= 1 page-size 16)) (error 'create! "expected page size 1 through 16"))
    (let* ([who head:ui-actor] [query (collection:create! who source "" '() 'persistent '())]
           [root (view:create! who query 'history 1 (list (cons 'page-size page-size) '(items)) '(0))]
           [navigation (view:create! who #f 'row 1 '((spacing . normal)) '())]
           [buttons (map (lambda (text delta)
                           (view:create! who #f 'action-text 1
                             (list (cons 'text text) '(enabled . #t) (list 'commands (list 'activate root 'move (list delta)))) '()))
                      '("Previous" "Next") (list (- page-size) page-size))])
      (view:arrange! who (list (list navigation 0 (map (lambda (name child) (list name child 'fit)) '(previous next) buttons) '((spacing . normal)))
                           (list root 0 (list (list 'navigation navigation 'fit)) (list (cons 'page-size page-size) '(items)))) '()) root))

  (edoc "Register history composition, standard text/result recipes and named page commands." (public))
  (define (init!)
    (register! 'text 1 (lambda (document) (edit:create-view! head:ui-actor (cadr document) '((read-only . #t)))))
    (register! 'result 1 (lambda (job) (eval:create-result-view! head:ui-actor job)))
    (widget:register! 'history 1
      (append (layout:container 'y)
        (list (cons 'service service!) (cons 'release release!) '(contexts . (widget-history))
          (cons 'actions (list (cons 'move move!))))))
    (keymap:bind-default! 'widget-history "M-n" (keymap:call move! widget:target 1))
    (keymap:bind-default! 'widget-history "M-p" (keymap:call move! widget:target -1))))
