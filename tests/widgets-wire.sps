;; Included in the existing two-real-head scenario. Run the shipped example.
(let* ([example (string-append (current-directory) "/examples/widgets.e")]
       [opened (begin
                 (head-send! a (string-append "\x1b;xbegin (load " (format "~s" example) ") (widget-example:open! '(\"alpha.sls\" \"beta.ss\" \"gamma.e\")))\r"))
                 (head-wait 'example-opened-through-mx a (lambda () (head-sees? a "Undo insertion"))))]
       [root-a (head-read a '(head:buffer-fact (head:current-buffer) 'widget-id #f))]
       [table-a (head-read a `(cadr (assq 'table (view:children (interaction:snapshot ',root-a)))))]
       [filter-a (head-read a `(cadr (assq 'filter (view:children (interaction:snapshot ',table-a)))))]
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
  (head-send! a "\x1b;[B\x1b;[A\x1b;[B\r")
  (head-wait 'collection-terminal-keys a
    (lambda () (equal? (head-read a `(let-values ([(source d inputs) (widget:context ',answer-a)])
                                       (vector-ref (cdr (assq 'value source)) 0))) "beta.ss")))
  (test:check 'collection-real-head-keyboard-selection-and-immediate-activation
    (head-read a `(begin
                    (let-values ([(source d inputs) (widget:context ',answer-a)])
                      (let ([focused (equal? (widget:focused ',root-a) ',entry-a)])
                        (widget:focus! ',root-a ',answer-a) (widget:prepare! ',root-a 40 10)
                        (widget:focus! ',root-a ',entry-a)
                        (let ([f (widget:prepare! ',root-a 40 10)])
                          (list (vector-ref (cdr (assq 'value source)) 0) focused
                            (vector-ref (widget:frame-styles f 3 (list-ref (widget:frame-lines f) 3)) 0)))))))
    '("beta.ss" #t candidate))
  (test:check 'collection-fork-keeps-selection-independent (key b table-b) "alpha.sls")
  (test:check 'collection-example-action-uses-real-undo
    (head-read a `(begin (entry:undo! ',answer-a)
                    (let-values ([(source d inputs) (widget:context ',answer-a)]) (vector-ref (cdr (assq 'value source)) 0)))) "")
  (test:check 'collection-warm-hover-and-frames-send-no-wire-data
    (head-read a
      `(let ([io (lambda () (call-with-input-file "/proc/self/io"
                              (lambda (p) (let loop () (let* ([k (read p)] [v (read p)])
                                                         (if (eq? k 'wchar:) v (loop)))))))])
         (let ([before (io)]
               [p (find (lambda (p) (equal? (widget:frame-id (car p)) ',root-a)) (widget:shown))])
           (do ([i 0 (+ i 1)]) ((= i 100))
             (widget:pointer! '(pointer move none ()) (+ (cadr p) 2) (+ (caddr p) 2 (modulo i 2)))
             (widget:prepare! ',root-a (+ 20 (modulo i 20)) 10))
           (widget:pointer! '(pointer move none ()) -1 -1)
           (- (io) before)))) 0)
  (head-read a `(begin (dispatch:input! ',root-a '(text "beta" typed)) #t))
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
  (test:check 'collection-direct-buffer-demand-uses-contract-headers-and-text-mirrors
    (head-read b
      `(let* ([source (view:source (view:snapshot ',entry-a))] [token (connection:subscribe! (list source) void)]
              [bundle (connection:snapshot (list source))])
         (connection:unsubscribe! token)
         (list (cadar (caddr bundle)) (equal? (cadddr bundle) (list source))))) '(#t #t))
  ;; Release and remount the same descriptor, preserving the canonical recipe
  ;; and selection while disposing all head-only range/cache resources.
  (head-read b `(begin (widget:unmount! ',root-b) (widget:mount! ',root-b 'remounted-collection) #t))
  (head-wait 'collection-remount b (lambda () (equal? (key b table-b) "beta.ss")))
  (test:check 'collection-disconnect-restores-filter-default
    (rpc head 'connection-bind query (list (list query 'filter (list (head-read a `(view:source (interaction:snapshot ',entry-a))) 'text) #f)))
    (list 'applied '()))
  (head-wait 'collection-default-restored a
    (lambda () (= (head-read a `(cdr (assq 'count (cdr (assq 'value (range:summary ',query)))))) 3)))
  (for-each
    (lambda (ui root)
      (head-read ui `(begin
                       (for-each head:forget-buffer! (filter (lambda (b) (equal? ',root (head:buffer-fact b 'widget-id #f))) (head:buffers)))
                       (head:show-buffer! (head:adopt-store-buffer! ,id)) #t))) (list a b) (list root-a root-b)))

;; Buffet uses the same controls through the actual client/base split. Reopen
;; from an empty result and immediately accept the previous document.
(let* ([previous (head-read a '(head:buffer-store-id (head:current-buffer)))]
       [origin (head-read a '(let ([b (head:new-buffer! "buffet wire origin")]) (head:show-buffer! b) (head:buffer-store-id b)))])
  (head-send! a "\x18;b")
  (head-wait 'buffet-wire-open a (lambda () (head-sees? a "Filter:")))
  (let* ([app (head-read a '(cadr (assq 'app (view:children (interaction:snapshot (head:buffer-fact (head:current-buffer) 'widget-id #f))))))]
         [table (head-read a `(cadr (assq 'table (view:children (interaction:snapshot ',app)))))]
         [query (head-read a `(view:source (interaction:snapshot ',table)))])
    (head-send! a "__buffet-absent__")
    (head-wait 'buffet-wire-empty a (lambda () (head-sees? a "No matching buffers")))
    (head-send! a "\x07;\x18;b\r")
    (head-wait 'buffet-wire-immediate-previous a
      (lambda () (equal? (head-read a '(head:buffer-store-id (head:current-buffer))) previous)))
    (test:check 'buffet-client-reopen-clears-filter-and-keeps-previous
      (head-read a `(let-values ([(lines revision) (store:snapshot
                                                     (cadr (find (lambda (r) (eq? (car r) 'buffer))
                                                             (cdr (assq 'owned (cdr (assq 'value (collection:summary ',query))))))))])
                      lines)) '#("")))
  (head-read a `(begin (store:delete! head:ui-actor ,origin) (head:sync-foreign-edits!) #t)))

;; Exercise the real head bridge and connection-owned cleanup without another
;; terminal process. The provider sees raw local metadata, never fitted text.
(let* ([source (head-read a '(begin (kernel:load-modules! '("document")) (document:create-source! 'transient)))]
       [ref (head-read a '(let ([b (head:register-view! "catalogue-wire" void)]) (document:reference b)))]
       [query (rpc head 'collection-create source "catalogue-wire" '() 'transient)]
       [temporary (connect)])
  (define (count query)
    (let ([v (cdr (assq 'value (rpc head 'collection-summary query)))])
      (and (eq? (cdr (assq 'status v)) 'ready) (cdr (assq 'count v)))))
  (define (retire id)
    (let* ([packet (rpc head 'model-read (list id))] [r (caddar (cadr packet))])
      (rpc head 'model-retire id (cdr (assq 'revision r)))))
  (test:await 'catalogue-wire-local (lambda () (equal? (count query) 1)))
  (test:check 'catalogue-wire-cannot-resolve-another-heads-token
    (head-read b `(begin (kernel:load-modules! '("document")) (document:resolve! ',ref))) #f)
  (head-read a `(begin (head:forget-buffer! (document:resolve! ',ref)) #t))
  (test:await 'catalogue-wire-removal (lambda () (equal? (count query) 0)))
  (hello temporary '(head "catalogue-wire"))
  (receive temporary)
  (let* ([token (rpc temporary 'catalogue-attach)]
         [source (rpc temporary 'catalogue-source "/home" 'transient)]
         [query (rpc head 'collection-create source "catalogue-wire" '() 'transient)])
    (rpc temporary 'catalogue-contribute token '((1 ((name . "catalogue-wire") (version . 0)))))
    (test:await 'catalogue-wire-contributed (lambda () (equal? (count query) 1)))
    (sys:close-connection! temporary)
    (test:await 'catalogue-wire-detached (lambda () (equal? (count query) 0)))
    (test:check 'catalogue-wire-detach-retires-contribution (count query) 0)
    (for-each retire (list query source)))
  (for-each retire (list query source)))
