;; Topology uses the same view tree under a root and over arbitrary contents.
;; Keep its independent domain invariants in the existing base process.
(let ()
  (import (prefix (state manager) manager:) (prefix (core descriptor) descriptor:))
  (define who '(head "manager"))
  (define other '(head "other-manager"))
  (define (get r k) (cdr (assq k r)))
  (define (children id) (map cadr (view:children (view:snapshot id))))
  (define (set-children! id ids)
    (let* ([d (view:snapshot id)] [root (let climb ([id id])
                                          (let ([parent (view:parent (view:snapshot id))]) (if parent (climb parent) id)))]
           [r (view:snapshot root)])
      (view:arrange! who (list (list id (model:revision id)
                                 (map (lambda (n child) (list n child '(grow 1)))
                                   (map (lambda (n) (string->symbol (number->string n))) (iota (length ids))) ids)
                                 (view:options d)))
        (if (view:owner r) (list (list root (view:generation r))) '()))))
  (let ([before (model:ids)])
    (kernel:load-module! "window")
    (check 'window-definitions-do-not-allocate-state (model:ids) before))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)] [second (window:split! manager first 'right)]
             [process '(app terminal 91234)]
             [document (store:create! process "composed terminal" '("output") (list (cons 'app process) '(read-only . #t)))]
             [ordinary (store:create! who "terminal fallback" '("text"))]
             [a (window:open-document! manager first document)] [a-text (car (children a))]
             [b (window:open-document! manager second document)])
        (view:claim! who manager)
        (window:open-document! manager first ordinary)
        (let ([before (model:ids)])
          (check 'window-terminal-retains-capture-and-child-without-changing-document-identity
            (list (view:kind (view:snapshot a)) (window:document manager second)
              (map (lambda (id) (view:owner (view:snapshot id))) (list a a-text))
              (window:open-document! manager first document) (model:ids)
              (map (lambda (id) (view:owner (view:snapshot id))) (list a a-text))
              (connection:bindings a) (equal? a b))
            (list 'terminal document '(#f #f) a before (list who who)
              (list (list a-text 'follow (list a 'following))) #f)))
        (window:open-document! manager first ordinary)
        (let ([graph (map car (view:tree a))])
          (view:retire! who a (model:revision a))
          (check 'retiring-a-hidden-terminal-presentation-preserves-its-process-and-other-pane
            (list (map model:snapshot graph) (store:exists? document)
              (window:documents manager first) (window:document manager second))
            (list (make-list (length graph) #f) #t (list ordinary) document)))
        (let ([graph (map car (view:tree b))])
          (store:delete! process document)
          (check 'terminal-document-retirement-closes-all-presentation-descendants
            (list (map model:snapshot graph) (connection:bindings b)
              (window:document manager second) (window:document manager first))
            (list (make-list (length graph) #f) '() #f ordinary))))))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)] [second (window:split! manager first 'right)]
             [a (store:create! who "origin a" '("a"))] [b (store:create! who "origin b" '("b"))]
             [editor (window:open-document! manager first a)]
             [x (view:create! who #f 'label 1 '((name . "<x>") (catalogue . #t)) '() first)]
             [y (view:create! who #f 'label 1 '((name . "<y>") (catalogue . #t)) '() first)]
             [z (view:create! who #f 'label 1 '((name . "<z>") (catalogue . #t)) '() second)])
        (define (origins window) (get (view:options (view:snapshot window)) 'origins))
        (window:open-document! manager second b)
        (view:claim! who manager) (window:select! manager first)
        (let* ([events (test:recorder)]
               [token (model:subscribe! (list first)
                        (lambda (event) (events (list (window:document manager first) (origins first)))))])
          (window:open-document! manager first x) (window:open-document! manager first x)
          (window:open-document! manager first y) (model:unsubscribe! token)
          (check 'window-origins-commit-with-placement-and-active-reopen-preserves-them
            (events) (list (list x (list (list x a))) (list y (list (list y x) (list x a))))))
        (let* ([copy (view:fork! who manager)] [window (window:numbered copy 1)]
               [copy-y (window:document copy window)] [copy-x (cadr (assoc copy-y (origins window)))])
          (check 'window-copy-remaps-nested-app-origins-but-preserves-text-identities
            (list (equal? copy-x x) (window:return! copy window copy-y)
              (begin (window:return! copy window copy-x) (window:document copy window))
              (window:document manager first))
            (list #f copy-x a y)))
        ;; Returning restores an existing placement even after catalogue opt-out;
        ;; it must not rewrite the restored app's own origin and create a loop.
        (view:arrange! who (list (list x (model:revision x) '() '((name . "<x>") (catalogue . #f)))) '())
        (check 'window-nested-return-preserves-origins-and-hidden-app-state
          (list (window:return! manager first y) (window:return! manager first x) (window:document manager first))
          (list x editor a))
        (view:arrange! who (list (list x (model:revision x) '() '((name . "<x>") (catalogue . #t)))) '())
        (window:select! manager second)
        (let ([before (map model:snapshot (list second (car (children second))))])
          (window:open-document! manager first x)
          (window:return! manager first x)
          (let ([tree (view:tree manager)])
            (check 'window-inactive-return-preserves-focus-and-stale-invocation-refuses
              (list (window:current manager) (map model:snapshot (list second (car (children second))))
                (test:raises? (lambda () (window:return! manager first x))) (view:tree manager))
              (list second before #t tree))))
        (window:open-document! manager first b) (window:open-document! manager first x)
        (store:delete! who b)
        (store:create! who "origin b" '("replacement"))
        (check 'window-retired-origin-uses-mru-without-resolving-a-same-name-replacement
          (list (assoc x (origins first)) (window:return! manager first x) (window:document manager first))
          (list #f editor a))
        (window:open-document! manager second z)
        (let ([tree (view:tree manager)] [ids (model:ids)])
          (check 'window-return-without-a-surviving-target-keeps-the-app
            (list (window:return! manager second z) (view:tree manager) (model:ids)) (list #f tree ids))))))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)]
             [second (window:split! manager first 'right)]
             [borrowed (store:create! who "window borrowed" '("keep"))]
             [prototype (view:create! who borrowed 'label 1 '((name . "<owned app>") (catalogue . #t)) '())]
             [active (view:fork! who prototype (list (cons 'owner second)))] [hidden (view:fork! who prototype (list (cons 'owner second)))]
             [outputs (map (lambda (name) (store:create! who name '("output"))) '("window output a" "window output b"))]
             [validate-resource list?])
        (model:register-kind! 'window-output 1 (lambda (v) (validate-resource v)))
        (view:register-resource-kind! 'window-output 1
          (lambda (actor r) (error 'test "copy is unused")) (lambda (r) (get r 'value)))
        (let* ([resource (model:create! who 'window-output 1 active 'persistent outputs outputs)]
               [child (view:create! who borrowed 'label 1 '() '() active)]
               [late #f])
          (view:arrange! who
            (list (list active (model:revision active) (list (list 'child child 'fit))
                        (cons (list 'owned resource) (view:options (view:snapshot active))))) '())
          (window:open-document! manager second hidden)
          (window:open-document! manager second active)
          (view:claim! who manager) (window:select! manager second)
          (let ([entered (test:gate)] [release (test:gate)] [block? #t] [before (view:tree manager)])
            (set! validate-resource
              (lambda (v)
                (when block? (set! block? #f) (entered #t) (test:await 'window-disposal-release release))
                (list? v)))
            (let ([worker (test:worker (lambda () (actor:call-as who (lambda () (test:raises? (lambda () (window:close! manager second)))))))])
              (test:await 'window-disposal-validation entered)
              (set! late (view:create! who borrowed 'label 1 '() '() resource))
              (release #t)
              (check 'window-close-refuses-late-owned-allocation-atomically
                (list (worker) (view:tree manager) (map store:exists? outputs)) (list #t before '(#t #t))))
            (set! validate-resource list?))
          (let* ([committed (test:gate)] [release (test:gate)] [block? #t]
                 [token (model:subscribe! (list manager)
                          (lambda (event)
                            (when (and block? (not (model:snapshot second)))
                              (set! block? #f) (committed #t) (test:await 'window-cleanup-release release))))]
                 [worker (test:worker (lambda () (actor:call-as who (lambda () (window:close! manager second)))))])
            (test:await 'window-close-committed committed)
            (let* ([pending (model:snapshot manager)] [intent (descriptor:cleanup (get pending 'value))]
                   [plan (view:disposal manager)])
              (check 'window-close-persists-disposal-before-deleting-output
                (list (map model:snapshot (list second active hidden child resource late))
                  (window:list manager) (view:focus (view:snapshot manager))
                  (for-all (lambda (id) (and (member id intent) (member id (get pending 'references)) (member id (cadr plan)) #t)) outputs)
                  (map store:exists? outputs) (store:line borrowed 0)
                  (test:raises? (lambda () (view:fork! who manager)))
                  (call-with-values (lambda () (view:retire! who manager (model:revision manager))) (lambda (status current) status)))
                (list (make-list 6 #f) (list first) first #t '(#t #t) "keep" #t 'pending))
              (let-values ([(next-id records) (model:export)])
                (check 'window-disposal-intent-survives-snapshot-format
                  (model:valid-import? next-id (filter (lambda (r) (member (get r 'id) (list manager first))) records)) #t))
              ;; An interrupted close can have deleted only some output. The
              ;; startup path consumes the saved intent without the old head.
              (store:delete! who (car outputs))
              (view:resume!) (view:resume!)
              (check 'window-disposal-resumes-idempotently-without-consuming-borrowed-state
                (list (map store:exists? outputs) (descriptor:cleanup (view:snapshot manager))
                  (view:source (view:snapshot prototype)) (store:line borrowed 0))
                (list '(#f #f) '() borrowed "keep")))
            (release #t) (check 'window-interrupted-close-finishes (worker) #t)
            (model:unsubscribe! token))))))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)] [second (window:split! manager first 'right)]
             [screen (view:create! who #f 'vertical 1 '() '())] [prompt (view:create! who #f 'prompt 1 '() '())]
             [source (store:create! who "app source" '("borrowed"))] [text (store:create! who "app fallback" '("fallback"))]
             [template (view:create! who #f 'app-fixture 1 '((name . "<app>") (catalogue . #t)) '())]
             [input (view:create! who source 'label 1 '() '() template)]
             [editor (window:open-document! manager first text)])
        (set-children! template (list input)) (set-children! screen (list manager prompt))
        (view:claim! who screen) (window:select! manager first)
        (let* ([app (view:fork! who template (list (cons 'owner first)))] [leaf (car (children app))]
               [other-app (view:fork! who template (list (cons 'owner second)))] [other-leaf (car (children other-app))]
               [before (map model:snapshot (list second prompt))] [events (test:recorder)]
               [token (model:subscribe! (list first app leaf screen) events)])
          (window:open-document! manager first app) (model:unsubscribe! token)
          (check 'window-admits-exact-app-and-complete-subtree-in-one-publication
            (list (window:document manager first) (window:documents manager first)
              (view:owned (view:snapshot first)) (map (lambda (id) (view:owner (view:snapshot id))) (list app leaf))
              (view:parent (view:snapshot app)) (view:focus (view:snapshot screen))
              (length (events)) (map model:snapshot (list second prompt)))
            (list app (list app text) (list app editor) (list who who) first app 1 before))
          (let ([lease (view:generation (view:snapshot leaf))])
            (view:publish! who (list (list leaf lease 1 #f '(remembered) #f)))
            (window:open-document! manager first text)
            (view:publish! who (list (list leaf lease 2 #f '(obsolete) #f)))
            (check 'window-hides-whole-app-without-losing-state-or-demanding-its-source
              (list (view:parent (view:snapshot app)) (view:parent (view:snapshot leaf))
                (map (lambda (id) (view:owner (view:snapshot id))) (list app leaf))
                (view:state (view:snapshot leaf)) (list-ref (connection:snapshot (map car (view:tree screen))) 3))
              (list #f app '(#f #f) '(remembered) (list text)))
            (let ([ids (model:ids)])
              (view:publish! who (list (list screen (view:generation (view:snapshot screen)) 1 #f '() prompt)))
              (window:open-document! manager first app) (window:open-document! manager second other-app)
              (let ([tree (view:tree screen)])
                (window:open-document! manager first app)
                (check 'window-reuses-apps-and-preserves-independent-copies-and-prompt
                  (list (equal? tree (view:tree screen)) (equal? ids (model:ids)) (view:focus (view:snapshot screen))
                    (map (lambda (id) (view:state (view:snapshot id))) (list leaf other-leaf)))
                  (list #t #t prompt '((remembered) ()))))))
          (let ([temporary (store:create! who "app temporary" '("temporary"))])
            (window:open-document! manager second temporary)
            (view:arrange! who (list (list other-app (model:revision other-app) (view:children (view:snapshot other-app))
                                           '((name . "<app>") (catalogue . #f)))) '())
            (store:delete! who temporary)
            (check 'window-fallback-mounts-retained-app-subtree-even-after-catalogue-opt-out
              (list (window:document manager second) (window:documents manager second)
                (map (lambda (id) (view:owner (view:snapshot id))) (list other-app other-leaf))
                (view:parent (view:snapshot other-app)) (view:focus (view:snapshot screen))
                (test:raises? (lambda () (window:open-document! manager second other-app))))
              (list other-app (list other-app) (list who who) second prompt #t)))
          (let* ([copy (view:fork! who manager)] [window (window:numbered copy 1)]
                 [copied-app (window:document copy window)])
            (check 'window-fork-remaps-app-document-identity-and-retains-text-identity
              (list (equal? copied-app app) (window:documents copy window)
                (window:open-document! copy window copied-app) (view:state (view:snapshot (car (children copied-app))))
                (get (model:snapshot copied-app) 'scope))
              (list #f (list copied-app text) copied-app '(remembered) window)))
          (let* ([private (view:create! who #f 'label 1 '() '() first)]
                 [foreign (view:create! who #f 'label 1 (list '(name . "foreign") '(catalogue . #t) (list 'audience other)) '() first)]
                 [unscoped (view:create! who #f 'label 1 '() '())]
                 [invalid (view:create! who #f 'label 1 '((name . "invalid lifetime") (catalogue . #t)) '() first)]
                 [transient (model:create! who 'widget-view 3 first 'transient '()
                              (descriptor:make #f 'label 1 '((name . "temporary") (catalogue . #t)) '()))])
            (set-children! invalid (list unscoped))
            (let ([before (view:tree screen)] [ids (model:ids)])
              (check 'window-refuses-unprepared-private-foreign-and-transient-apps-without-changing-placement
                (list (map (lambda (id) (test:raises? (lambda () (window:open-document! manager first id))))
                        (list template other-app private foreign invalid transient))
                  (view:tree screen) (model:ids))
                (list (make-list 6 #t) before ids))))
          (let* ([candidate (view:create! who #f 'label 1 '((name . "guarded") (catalogue . #t)) '() first)]
                 [entered (test:gate)] [release (test:gate)] [block? #f])
            (model:register-kind! 'app-lifetime 1
              (lambda (v) (when block? (set! block? #f) (entered #t) (test:await 'app-admission-release release)) (list? v)))
            (let* ([lifetime (model:create! who 'app-lifetime 1 candidate 'persistent '() '())]
                   [child (view:create! who #f 'label 1 '() '() lifetime)])
              (set-children! candidate (list child))
              (let ([before (view:tree screen)])
                (set! block? #t)
                (let ([worker (test:worker (lambda () (actor:call-as who
                                                        (lambda () (test:raises? (lambda () (window:open-document! manager first candidate)))))))])
                  (test:await 'app-lifetime-validation entered)
                  (model:retire! who lifetime (model:revision lifetime)) (release #t)
                  (check 'window-admission-witnesses-intermediate-resource-lifetimes
                    (list (worker) (view:tree screen) (view:parent (view:snapshot candidate)) (view:owner (view:snapshot child)))
                    (list #t before #f #f))))))
          ;; A raw root deletion must not lose the scopes that still belong
          ;; to that identity, nor strand focus inside its former subtree.
          (let* ([output (store:create! who "retired app output" '("owned"))]
                 [resource (model:create! who 'window-output 1 app 'persistent (list output) (list output))]
                 [d (view:snapshot app)])
            (view:arrange! who (list (list app (model:revision app) (view:children d) (cons (list 'owned resource) (view:options d))))
              (list (list screen (view:generation (view:snapshot screen)))))
            (view:publish! who (list (list screen (view:generation (view:snapshot screen)) 1 #f '() leaf)))
            (model:retire! who app (model:revision app))
            (check 'window-retired-app-falls-back-and-durably-releases-private-scopes
              (list (window:document manager first) (window:documents manager first) (view:focus (view:snapshot screen))
                (map model:snapshot (list app leaf resource)) (store:exists? output)
                (window:document manager second) (store:line source 0) (store:line text 0))
              (list text (list text) editor '(#f #f #f) #f other-app "borrowed" "fallback")))))))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)]
             [second (window:split! manager first 'right)]
             [screen (view:create! who #f 'vertical 1 '() '())] [prompt (view:create! who #f 'prompt 1 '() '())]
             [a (store:create! who "placement a" '("abc"))] [b (store:create! who "placement b" '("def"))]
             [c (store:create! who "placement c" '("ghi"))])
        (set-children! screen (list manager prompt))
        (view:claim! who screen) (window:select! manager first)
        (let* ([untouched (map model:snapshot (list second prompt))]
               [a-view (window:open-document! manager first a)]
               [lease (view:generation (view:snapshot a-view))]
               [state '((0 . 2) (0 . 1) (0 . 0) #t)])
          (view:publish! who (list (list a-view (view:generation (view:snapshot a-view)) 1 (store:revision a) state #f)))
          (let ([b-view (window:open-document! manager first b)])
            (view:publish! who (list (list a-view lease 2 (store:revision a) 'obsolete #f)))
            (check 'window-hidden-text-keeps-state-without-containment-or-dependency-demand
              (list (window:document manager first) (window:documents manager first)
                (view:parent (view:snapshot a-view)) (view:owner (view:snapshot a-view))
                (view:state (view:snapshot a-view)) (view:focus (view:snapshot screen))
                (list-ref (connection:snapshot (map car (view:tree screen))) 3))
              (list b (list b a) #f #f state b-view (list b)))
            (check 'window-reopens-same-editor-and-keeps-unrelated-leases
              (list (window:open-document! manager first a) (view:state (view:snapshot a-view))
                (window:documents manager first) (map model:snapshot (list second prompt)))
              (list a-view state (list a b) untouched))
            (let ([before (view:tree screen)])
              (window:open-document! manager first a)
              (check 'window-already-open-is-idempotent (view:tree screen) before))
            (view:publish! who (list (list screen (view:generation (view:snapshot screen)) 1 #f '() prompt)))
            (let ([other-view (window:open-document! manager second a)])
              (check 'window-shared-text-has-independent-presentations-and-preserves-prompt
                (list (equal? other-view a-view) (view:state (view:snapshot other-view))
                  (view:focus (view:snapshot screen)) (window:current manager))
                (list #f '((0 . 0) (0 . 0) (0 . 0) #f) prompt first)))
            (let* ([before (length (model:ids))]
                   [attempts (test:parallel 4
                               (lambda (n) (guard (ex [else #f])
                                             (actor:call-as who (lambda () (window:open-document! manager first c))))))]
                   [accepted (filter values attempts)])
              (check 'window-concurrent-open-allocates-only-one-editor
                (list (- (length (model:ids)) before) (pair? accepted)
                  (for-all (lambda (id) (equal? id (car accepted))) accepted)) '(1 #t #t)))
            (let* ([retainer (view:create! who #f 'vertical 1 '() '())]
                   [copy (view:fork! who manager (list (cons 'owner retainer)))] [left (window:numbered copy 1)]
                   [original-views (view:owned (view:snapshot first))]
                   [copied-views (view:owned (view:snapshot left))]
                   [copied-a (window:open-document! copy left a)])
              (check 'window-fork-copies-hidden-presentations-without-copying-text
                (list (window:documents copy left) (view:state (view:snapshot copied-a))
                  (exists (lambda (id) (and (member id original-views) #t)) copied-views)
                  (map (lambda (id) (get (model:snapshot id) 'scope)) copied-views)
                  (view:focus (view:snapshot copy)) (cadr (view:disposal copy))
                  (get (model:snapshot copy) 'scope) (view:owned (view:snapshot retainer)))
                (list (list a c b) state #f (make-list 3 left) copied-a '() retainer (list copy)))
              (window:close! copy left)
              (check 'window-close-atomically-retires-active-and-hidden-editors-only
                (list (map model:available? copied-views) (map model:available? original-views)
                  (map (lambda (id) (store:line id 0)) (list a b c)))
                (list '(#f #f #f) '(#t #t #t) '("abc" "def" "ghi"))))
            (let* ([excluded (map (lambda (props) (store:create! who "excluded placement" '("") props))
                               (list '((internal . #t)) (list (list 'trashed 0 who)) '((backup "/backup" #f "checksum")) (list (cons 'audience (list other)))))]
                   [before (model:ids)])
              (check 'window-refuses-noncatalogue-text-without-allocating
                (list (map (lambda (id) (test:raises? (lambda () (window:open-document! manager first id)))) excluded)
                  (equal? before (model:ids))) '((#t #t #t #t) #t)))
            ;; Deleting text falls back by retained identity, never by name.
            (window:open-document! manager first a)
            (store:delete! who a)
            (store:create! who "placement a" '("different document"))
            (check 'window-deleted-source-falls-back-and-refuses-same-name-replacement
              (list (window:document manager first) (window:documents manager first)
                (window:document manager second) (view:snapshot a-view)
                (test:raises? (lambda () (window:open-document! manager first a)))) (list c (list c b) #f #f #t))
            (window:select! manager second)
            (window:close! manager second)
            (check 'window-close-focuses-surviving-document
              (view:focus (view:snapshot screen)) (cadr (assq 'document (view:children (view:snapshot first))))))))))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)]
             [second (window:split! manager first 'right)]
             [screen (view:create! who #f 'vertical 1 '() '())] [prompt (view:create! who #f 'prompt 1 '() '())]
             [a (store:create! who "retiring a" '("a"))] [b (store:create! who "retiring b" '("b"))]
             [b-view (window:open-document! manager first b)]
             [a-view (window:open-document! manager first a)]
             [other-view (window:open-document! manager second a)]
             [foreign (actor:call-as other window:create-manager!)]
             [foreign-window (actor:call-as other (lambda () (window:current foreign)))]
             [foreign-view (actor:call-as other (lambda () (window:open-document! foreign foreign-window a)))])
        (view:set-state! who b-view (store:revision b) '((0 . 1) (0 . 0) (0 . 0) #f))
        (set-children! screen (list manager prompt)) (view:claim! who screen)
        (view:claim! other foreign)
        (window:select! manager first)
        (view:publish! who (list (list screen (view:generation (view:snapshot screen)) 1 #f '() prompt)))
        (let ([untouched (model:snapshot prompt)] [notices (test:recorder)]
              [lease (view:generation (view:snapshot a-view))])
          (let ([token (model:subscribe! #f notices)]
                [facts (cadar (cadr (store:metadata (list a))))])
            (store:archive! who a (get facts 'version) 'trash)
            (model:unsubscribe! token))
          (view:publish! who (list (list a-view lease 2 #f 'late a-view)))
          (check 'window-trash-reconciles-all-placements-in-one-transaction-without-stealing-prompt
            (list (window:document manager first) (window:document manager second)
              (window:documents manager first) (window:documents manager second)
              (map model:snapshot (list a-view other-view foreign-view)) (length (notices))
              (view:focus (view:snapshot screen)) (model:snapshot prompt) (store:line a 0)
              (view:state (view:snapshot b-view)))
            (list b #f (list b) '() '(#f #f #f) 2 prompt untouched "a" '((0 . 1) (0 . 0) (0 . 0) #f))))
        (store:archive! who a (get (cadar (cadr (store:metadata (list a)))) 'version) 'restore)
        (check 'window-restoring-document-does-not-reopen-retired-placements
          (list (window:document manager first) (window:document manager second)
            (actor:call-as other (lambda () (window:document foreign foreign-window)))) (list b #f #f))
        (window:select! manager first)
        (view:retire! who b-view (model:revision b-view))
        (check 'window-external-presentation-retirement-clears-retention-and-focuses-empty-window
          (list (window:document manager first) (window:documents manager first)
            (view:owned (view:snapshot first)) (view:focus (view:snapshot screen)) (store:line b 0))
          (list #f '() '() first "b"))
        ;; Hiding a document must also remove hidden presentations, while
        ;; unrelated metadata, renaming and text edits leave view leases alone.
        (window:open-document! manager first b)
        (let ([before (view:tree screen)])
          (store:rename! who b "renamed b") (store:set-property! who b 'read-only #t)
          (check 'window-ordinary-metadata-does-not-republish-placement (view:tree screen) before))
        (let ([retiring (window:open-document! manager first a)])
          (model:retire! who retiring (model:revision retiring))
          (check 'window-raw-model-retirement-repairs-dangling-containment-and-focus
            (list (window:document manager first) (window:documents manager first)
              (view:focus (view:snapshot screen)))
            (list b (list b) (cadr (assq 'document (view:children (view:snapshot first)))))))
        (let ([current (cadr (assq 'document (view:children (view:snapshot first))))])
          (for-each
            (lambda (property)
              (let* ([id (store:create! who "excluded retained" '(""))]
                     [editor (window:open-document! manager first id)])
                (window:open-document! manager first b)
                (let ([before (map model:snapshot (list screen current second prompt))])
                  (store:set-property! who id (car property) (cdr property))
                  (check (list 'window-removes-ineligible-hidden-document (car property))
                    (list (window:documents manager first) (view:snapshot editor)
                      (map model:snapshot (list screen current second prompt)) (store:exists? id))
                    (list (list b) #f before #t)))))
            (list '(internal . #t) '(backup "/archived" #f "checksum") (cons 'audience (list other))))))))
  (actor:call-as who
    (lambda ()
      (let* ([manager (window:create-manager!)] [first (window:current manager)]
             [foreign (actor:call-as other window:create-manager!)] [foreign-first (actor:call-as other (lambda () (window:current foreign)))])
        (let* ([r (model:snapshot manager)] [d (get r 'value)]
               [legacy (map (lambda (p)
                              (case (car p)
                                [(scope) (cons 'scope who)]
                                [(value) (cons 'value (descriptor:with d
                                                        (list '(schema . 1)
                                                          (cons 'options (remp (lambda (p) (eq? (car p) 'head)) (view:options d))))))]
                                [else p])) r)])
          (check 'window-upgrade-separates-head-identity-from-lifetime-without-changing-current-records
            (list (manager:upgrade legacy) (manager:upgrade r)) (list r r)))
        (check 'window-identities-labels-and-authority
          (list (equal? first foreign-first) (window:numbered manager 1)
            (actor:call-as other (lambda () (window:numbered foreign 1)))
            (test:raises? (lambda () (window:split! foreign foreign-first 'right))))
          (list #f first foreign-first #t))
        (let* ([right (window:split! manager first 'right)] [above (window:split! manager right 'above)]
               [split (view:parent (view:snapshot right))] [expected (children split)]
               [screen (view:create! who #f 'vertical 1 '() '())]
               [prompt (view:create! who #f 'prompt 1 '() '())]
               [source (store:create! who "manager document" '("survives close"))]
               [content (view:create! who source 'editor 1 '() '())])
          (set-children! screen (list manager prompt))
          (set-children! above (list content))
          (view:claim! who screen)
          (let* ([unrelated (list first right above content prompt)]
                 [before (map model:snapshot unrelated)])
            (window:select! manager above)
            (window:link! manager first right 'temporary)
            (window:unlink! manager first right 'temporary)
            (check 'window-selection-and-links-do-not-republish-unrelated-views
              (map model:snapshot unrelated) before))
          (window:resize! manager split expected '(2 3))
          (window:link! manager above right 'follow)
          (window:link! manager above right 'follow)
          (window:link! manager first right 'other)
          (window:unlink! manager first right 'other)
          (check 'window-nested-topology-selection-weights-and-idempotent-links
            (list (window:list manager) (view:focus (view:snapshot screen))
              (view:state (view:snapshot split)) (window:links manager))
            (list (list first above right) above '(2 3) (list (list above right 'follow))))
          (let* ([r (model:snapshot split)] [d (get r 'value)]
                 [legacy (map (lambda (p)
                                (if (eq? (car p) 'value)
                                  (cons 'value (descriptor:with d
                                                 (list '(schema . 1) '(state)
                                                   (cons 'children (map (lambda (c n) (list (car c) (cadr c) (list 'grow n)))
                                                                     (view:children d) '(2 3)))))) p)) r)])
            (check 'split-upgrade-preserves-proportions-without-changing-current-records
              (list (manager:upgrade legacy) (manager:upgrade r)) (list r r)))
          ;; A prompt owns root focus temporarily; the selected window survives.
          (view:publish! who (list (list screen (view:generation (view:snapshot screen)) 1 #f '() prompt)))
          (let* ([before (view:tree screen)] [copy (view:fork! who screen)]
                 [manager-copy (car (children copy))] [prompt-copy (cadr (children copy))]
                 [above-copy (window:numbered manager-copy 3)] [right-copy (window:numbered manager-copy 2)])
            (check 'window-fork-remaps-saved-selection-links-and-focus
              (list (window:current manager-copy) (window:links manager-copy)
                (view:focus (view:snapshot copy))
                (map (lambda (id) (get (model:snapshot id) 'scope)) (window:list manager-copy)))
              (list above-copy (list (list above-copy right-copy 'follow)) prompt-copy
                (make-list 3 manager-copy)))
            (window:select! manager-copy right-copy)
            (window:close! manager-copy above-copy)
            (check 'window-fork-operates-independently
              (list (window:current manager-copy) (window:links manager-copy) (view:tree screen))
              (list right-copy '() before)))
          (let ([before (view:tree screen)])
            (check 'window-stale-divider-refuses-without-moving-prompt-focus
              (list (window:current manager)
                (test:raises? (lambda () (window:resize! manager split (reverse expected) '(1 1))))
                (test:raises? (lambda () (window:resize! manager split expected '(0 1))))
                (view:tree screen)) (list above #t #t before)))
          ;; Scoped views retire with the window; unrelated borrowed content
          ;; detaches instead of being consumed by the owned graph.
          (let ([scoped (view:create! who #f 'label 1 '() '() above)])
            (window:select! manager above)
            (check 'window-close-repairs-tree-focus-links-and-releases-borrowed-content
              (list (window:close! manager above) (window:list manager) (window:current manager)
                (view:focus (view:snapshot screen)) (window:links manager)
                (model:available? above) (model:available? split) (model:available? scoped)
                (view:parent (view:snapshot content)) (view:owner (view:snapshot content))
                (store:line source 0))
              (list #t (list first right) first first '() #f #f #f #f #f "survives close")))
          (let ([new (window:split! manager first 'below)])
            (check 'window-number-reuse-keeps-new-identity
              (list (window:numbered manager 3) (equal? new above)) (list new #f))
            (window:select! manager new)
            (view:publish! who (list (list screen (view:generation (view:snapshot screen)) 1 #f '() prompt)))
            (window:close! manager new)
            (check 'window-close-keeps-focus-in-a-prompt
              (list (view:focus (view:snapshot screen)) (window:current manager)) (list prompt first)))
          (window:close! manager right)
          (check 'window-last-window-refuses (list (window:close! manager first) (window:list manager)) (list #f (list first)))
          ;; Each split sees all labels under a revision guard. Concurrent
          ;; attempts may refuse; no duplicate labels or speculative views leak.
          (let* ([before (model:ids)]
                 [attempts (test:parallel 4
                             (lambda (n) (guard (ex [else #f])
                                           (actor:call-as who (lambda () (window:split! manager first 'right))))))]
                 [accepted (filter values attempts)] [windows (window:list manager)])
            (check 'window-concurrent-splits-have-unique-numbers-without-leaked-models
              (list (length windows)
                (length (filter (lambda (id) (not (member id before))) (model:ids)))
                (map (lambda (n) (and (window:numbered manager n) #t)) (map add1 (iota (length windows))))
                (for-all (lambda (id) (and (member id windows) #t)) accepted))
              (list (+ 1 (length accepted)) (* 3 (length accepted)) (make-list (length windows) #t) #t)))
          (let-values ([(next-id records) (model:export)])
            (let* ([ids (map car (view:tree screen))]
                   [saved (filter (lambda (r) (member (get r 'id) ids)) records)]
                   [identity (window:list manager)])
              (view:reset-owners!)
              (view:claim! who screen)
              (check 'window-recovery-contract-keeps-identities-and-local-numbers
                (list (model:valid-import? next-id saved) (window:list manager)
                  (window:numbered manager 1) (window:current manager))
                (list #t identity first first)))))))))
