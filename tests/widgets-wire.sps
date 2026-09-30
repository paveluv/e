;; A prompt composition runs through real head input and the ordinary pump.
(let ([saved (head-read a '(head:buffer-name (head:current-buffer)))])
  (head-read a '(begin (kernel:load-module! "prompt-request") (kernel:load-module! "prompt-control") #t))
  (head-read a
    '(begin
       (define wire-prompt-answer #f)
       (define (wire-prompt-accepted! id value) (set! wire-prompt-answer value))
       (widget:register! 'wire-prompt-host 1
         (append (layout:container 'y) (list (cons 'actions (list (cons 'accepted wire-prompt-accepted!))))))
       (define wire-prompt-host (view:create! head:ui-actor #f 'wire-prompt-host 1 '() '()))
       (define wire-prompt-request (prompt-request:create! head:ui-actor #f #f "" '(captured-wire-origin) #f))
       (define wire-prompt-view
         (prompt-control:create! wire-prompt-request '((label . "Wire prompt:"))
           (list (list 'accepted wire-prompt-host 'accepted '()))))
       (view:arrange! head:ui-actor (list (list wire-prompt-host 0 (list (list 'prompt wire-prompt-view '(grow 1))) '())) '())
       (window:show-widget! (head:current-window) wire-prompt-host)
       #t))
  (head-wait 'prompt-control-is-shown a (lambda () (head-sees? a "Wire prompt:")))
  (head-send! a "value\r")
  (head-wait 'prompt-outcome-delivered-by-pump a (lambda () (head-read a '(and wire-prompt-answer #t))))
  (test:check 'prompt-real-head-input-and-base-outcome
    (head-read a '(cdr wire-prompt-answer)) '(#("value") (captured-wire-origin)))
  (head-read a `(begin (head:forget-buffer! (head:current-buffer))
                       (head:show-buffer! (head:buffer-named ,saved)) #t))
  (head-wait 'prompt-request-released-on-host-removal a
    (lambda () (head-read a '(not (caddr (caadr (model:snapshots (list wire-prompt-request)))))))))

;; Editor views use the same journal across real clients. Warm navigation
;; and preparation read only mirrored text, even through mode callbacks.
(let* ([source (head-read a '(store:create! head:ui-actor "editor wire" '("first" "second" "third") '((internal . #t))))]
       [left (head-read a `(let ([id (edit:create-view! head:ui-actor ,source '())])
                             (widget:mount! id 'editor-wire) (widget:prepare! id 12 3) id))]
       [right (head-read b `(let ([id (edit:create-view! head:ui-actor ,source '())])
                              (widget:mount! id 'editor-wire) (widget:prepare! id 20 3) id))])
  (test:check 'editor-warm-navigation-and-resize-send-no-wire-data
    (head-read a
      `(let ([io (lambda () (call-with-input-file "/proc/self/io"
                              (lambda (p) (let loop () (let* ([k (read p)] [v (read p)])
                                                         (if (eq? k 'wchar:) v (loop)))))))])
         (let ([before (io)])
           (do ([i 0 (+ i 1)]) ((= i 40))
             (edit:move! ',left (if (even? i) 'down 'up))
             (edit:page! ',left (if (even? i) 1 -1) 1)
             (widget:prepare! ',left (+ 12 (modulo i 3)) 3))
           (- (io) before)))) 0)
  (head-read a `(begin (edit:select! ',left '(0 . 5) '(0 . 5)) (edit:insert! ',left "!") #t))
  (head-wait 'editor-mirror-reaches-second-head b
    (lambda () (equal? (head-read b `(vector-ref (text-source:lines (text-source:lookup ,source)) 0)) "first!")))
  (head-read b `(begin (edit:select! ',right '(1 . 6) '(1 . 6)) (edit:insert! ',right "?") #t))
  (test:check 'editor-actor-undo-retains-the-other-head-edit
    (head-read a `(begin (edit:undo! ',left)
                    (let-values ([(lines revision) (store:snapshot ,source)]) lines))) '#("first" "second?" "third"))
  (test:check 'editor-client-has-no-shadow-head-buffer
    (head-read b `(list (not (head:buffer-of-store-id ,source)) (car (view:state (interaction:snapshot ',right)))
                    (keymap:action-text edit:move!))) '(#t (1 . 7) "edit:move!"))
  (test:check 'ordinary-editor-host-has-no-navigation-wire-cost
    (head-read a
      `(let* ([w (head:current-window)] [was (head:current-buffer)]
              [b (head:adopt-store-buffer! ,source)]
              [io (lambda () (call-with-input-file "/proc/self/io"
                               (lambda (p) (let loop () (let* ([k (read p)] [v (read p)])
                                                          (if (eq? k 'wchar:) v (loop)))))))])
         (head:show-buffer! b)
         (let* ([root (head:window-widget w)] [before (io)])
           (do ([i 0 (+ i 1)]) ((= i 40))
             (dispatch:input! root (list 'key (if (even? i) "DOWN" "UP")))
             (widget:prepare! root (+ 12 (modulo i 3)) 3))
           (let ([written (- (io) before)]) (head:show-buffer! was) written)))) 0)
  (head-read a `(begin (widget:unmount! ',left) #t))
  (head-read b `(begin (widget:unmount! ',right) #t)))

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
  (head-read a `(begin (head:show-buffer! (head:adopt-store-buffer! ,id)) (head:before-frame!) #t))
  (test:check 'hidden-collection-releases-one-mount-without-stopping-the-other
    (list (head-read a `(interaction:snapshot ',root-a))
      (cdr (assq 'status (cdr (assq 'value (rpc head 'collection-summary query)))))) '(#f ready))
  (head-read b `(begin (head:show-buffer! (head:adopt-store-buffer! ,id)) (head:before-frame!) #t))
  (test:await 'last-hidden-collection-releases-base-work
    (lambda () (eq? (cdr (assq 'status (cdr (assq 'value (rpc head 'collection-summary query))))) 'pending)))
  (head-read a `(begin (window:show-widget! (head:current-window) ',root-a) #t))
  (head-wait 'hidden-collection-resumes-retained-recipe a (lambda () (equal? (key a table-a) "beta.ss")))
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
    (head-wait 'buffet-command-bindings-ready a
      (lambda () (head-read a `(and (assq 'trash (widget:commands ',table))
                                 (assq 'delete (widget:commands ',table)) #t))))
    (test:check 'live-model-completion-uses-base-metadata
      (head-read a
        `(list (assoc ',app (model:metadata))
           (and (member ,(format "(model ~a)" (cadr app))
                  (eval:completion-candidates "(model:snapshot " 16)) #t)))
      (list (list app 'widget-view) #t))
    ;; Evaluation mail waits until a prompt closes. Observe painting through
    ;; its hook, then retrieve the result after cancelling the prompt.
    (head-read a
      `(begin
         (define completion-status-bytes #f)
         (parameterize ([kernel:registering-module 'completion-wire])
           (head:add-pre-redraw-hook!
             (lambda ()
               (when (and (not completion-status-bytes) (prompt:active?) (head:popup)
                       (string:prefix? "<completions" (head:buffer-name (head:window-buffer (head:popup)))))
                 ;; Exclude unrelated mirror workers: inspection itself runs
                 ;; on this UI thread and must not write to the base.
                 (let ([io (lambda () (call-with-input-file "/proc/thread-self/io"
                                        (lambda (p) (let loop () (let* ([k (read p)] [v (read p)])
                                                                   (if (eq? k 'wchar:) v (loop)))))))])
                   (let ([before (io)] [binding (cdr (keymap:resolved-binding 'buffet '("C-k")))])
                     (do ([i 0 (+ i 1)]) ((= i 100))
                       (keymap:action-trace (keymap:binding-action binding) (list (cons widget:target ',app)))
                       (widget:command-bindings ',app)
                       (head:buffer-status (head:window-buffer (head:popup)) (head:popup)))
                     (set! completion-status-bytes (- (io) before)))))))) #t))
    (head-send! a "\x1b;xmodel:snapshot \t\t")
    (head-wait 'model-completion-popup a (lambda () (head-sees? a "matches of model")))
    (head-send! a "\x07;__buffet-absent__")
    (head-wait 'buffet-wire-empty a (lambda () (head-sees? a "No matching buffers")))
    (test:check 'widget-discovery-and-completion-status-send-no-wire-data
      (head-read a '(begin (kernel:retract-module! 'completion-wire) completion-status-bytes)) 0)
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
;; Load before compiling expressions that refer to the newly imported names.
(for-each
  (lambda (screen)
    (head-read screen
      '(let ([failures (kernel:load-modules! '("catalogue-host"))])
         (unless (null? failures)
           (error 'catalogue-host-load (format "~s" (map (lambda (p) (cons (car p) (kernel:condition-text (cdr p)))) failures)))) #t)))
  (list a b))
(let* ([source (head-read a '(catalogue-host:create-source! 'transient))]
       [ref (head-read a '(let ([b (head:register-view! "catalogue-wire" void)]) (catalogue-host:reference b)))]
       [query (rpc head 'collection-create source "catalogue-wire" '() 'transient)]
       [temporary (connect)])
  (define (count query)
    (let ([v (cdr (assq 'value (rpc head 'collection-summary query)))])
      (and (eq? (cdr (assq 'status v)) 'ready) (cdr (assq 'count v)))))
  (define (retire id)
    (let* ([packet (rpc head 'model-read (list id))] [r (caddar (cadr packet))])
      (rpc head 'model-retire id (cdr (assq 'revision r)))))
  (rpc head 'model-watch (list query))
  (test:await 'catalogue-wire-local (lambda () (equal? (count query) 1)))
  (test:check 'catalogue-wire-cannot-resolve-another-heads-token
    (head-read b `(catalogue-host:resolve! ',ref)) #f)
  (head-read a `(begin (head:forget-buffer! (catalogue-host:resolve! ',ref)) #t))
  (test:await 'catalogue-wire-removal (lambda () (equal? (count query) 0)))
  (hello temporary '(head "catalogue-wire"))
  (receive temporary)
  (let* ([token (rpc temporary 'catalogue-attach)]
         [source (rpc temporary 'catalogue-source "/home" 'transient)]
         [query (rpc head 'collection-create source "catalogue-wire" '() 'transient)])
    (rpc temporary 'model-watch (list query))
    (rpc temporary 'catalogue-contribute token '((1 ((name . "catalogue-wire") (version . 0)))))
    (test:await 'catalogue-wire-contributed (lambda () (equal? (count query) 1)))
    (sys:close-connection! temporary)
    (test:await 'last-disconnected-reader-cancels-collection (lambda () (not (count query))))
    (rpc head 'model-watch (list query))
    (test:await 'catalogue-wire-detached (lambda () (equal? (count query) 0)))
    (test:check 'catalogue-wire-detach-retires-contribution (count query) 0)
    (rpc head 'model-unwatch (list query))
    (for-each retire (list query source)))
  (rpc head 'model-unwatch (list query))
  (for-each retire (list query source)))

;; The same prepared-range transport serves filesystem queries. No extra head
;; or process is needed to exercise client dispatch and metadata enrichment.
(let* ([source (head-read a `(begin (kernel:load-modules! '("filesystem"))
                               (filesystem:create-source! head:ui-actor ,root #f 'persistent)))]
       [pair (head-read a `(filesystem:create-query! head:ui-actor ',source
                             ,(string-append (current-directory) "/lib/service/file-query.s")))]
       [query (car pair)] [intent #f])
  (define (value) (cdr (assq 'value (rpc head 'collection-summary query))))
  (rpc head 'model-watch (list query))
  (test:await 'filesystem-wire-ready
    (lambda () (let ([v (value)]) (and (eq? (cdr (assq 'status v)) 'ready) (cdr (assq 'complete v))))))
  (let ([v (value)])
    (set! intent (head-read a `(filesystem:complete! head:ui-actor ',query ,(cdr (assq 'generation v))))))
  (test:await 'filesystem-wire-completed
    (lambda ()
      (equal? (cdr (assq 'completion (cdr (assq 'details (value)))))
        (list 'ready intent (string-append (current-directory) "/lib/service/file-query.sls")))))
  (test:check 'filesystem-wire-completion-is-a-bounded-base-result
    (let* ([v (value)] [p (rpc head 'collection-range query (cdr (assq 'generation v)) 0 2 '(name))]
           [r (find (lambda (r) (eq? (caadr r) 'path)) (list-ref p 4))])
      (list (cdr (assq 'count v)) (caddr (assq 'name (caddr r))))) '(2 "file-query.sls"))
  (let* ([before (head-read a '(catalogue-host:reference (head:current-buffer)))]
         [host (head-read a `(begin
                               (kernel:load-modules! '("finder"))
                               (let* ([host (window:tool! "finder-wire"
                                              (lambda (commands) (finder:create! commands ,root ',query)))]
                                      [b (window:show-widget! (head:current-window) host)])
                                 (head:show-buffer! b) host)))])
    (head-wait 'finder-widget-wire-ready a (lambda () (head-sees? a "file-query.sls")))
    (head-send! a "\t")
    (head-wait 'finder-widget-wire-completed a
      (lambda () (equal? (head-read a `(let-values ([(text revision) (store:snapshot ,(cadadr pair))]) text))
                   (vector (string-append (current-directory) "/lib/service/file-query.sls")))))
    (test:check 'finder-client-entry-table-and-base-completion-compose
      (head-read a `(let* ([app (widget:descendant ',host 'app)] [table (widget:descendant app 'table)]
                           [selection (cdr (assq 'selection (view:state (interaction:snapshot table))))])
                      (list (equal? (widget:focused ',host) (widget:descendant table 'filter 'entry))
                        (and selection (car (caddr selection)))))) '(#t path))
    (head-read a `(let ([b (widget:host ',host)])
                    (head:show-buffer! (catalogue-host:resolve! ',before)) (head:forget-buffer! b) #t)))
  (let* ([packet (rpc head 'model-read (list query))] [r (caddar (cadr packet))])
    (rpc head 'model-unwatch (list query))
    (rpc head 'model-retire query (cdr (assq 'revision r)))))
