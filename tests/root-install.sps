;; Head lifecycle coverage runs inside paint's existing windowless process.
(let ()
  (import (only (foundation edoc) elibrary)
          (prefix (head root) root:) (prefix (head routing) routing:)
          (prefix (head interaction) interaction:) (prefix (head keymap) keymap:)
          (prefix (core operation) operation:) (prefix (service policy) policy:)
          (prefix (state actor) actor:) (prefix (state connection) connection:))
  ;; In-process base calls have the same authenticated invocation context as
  ;; generated client proxies. Keep the fixture body outside the operation.
  (eval '(elibrary (root-fixture)
           (export run!)
           (import (chezscheme) (prefix (core kernel) kernel:))
           (edoc "Run the local lifecycle fixture inside an authenticated operation.")
           (define (run!) ((unbox (kernel:persistent-cell 'root-fixture (lambda () #f)))))))
  (eval '(import (prefix (root-fixture) fixture:)))
  (kernel:load-module! "composition")
  (parameterize ([kernel:registering-module 'root-fixture])
    (operation:register! 'root-fixture:run! (eval 'fixture:run!) 'control))
  (set-box! (kernel:persistent-cell 'root-fixture (lambda () #f))
    (lambda ()
      (define (get r key) (cdr (assq key r)))
      (define output (open-output-string))
      (define released '())
      (define mode #f)
      (define pressed 0)
      (define (make-view name)
        (view:create! head:ui-actor #f 'root-test 1 (list (cons 'name name)) 0))
      (define (install id disposition)
        (call-with-values (lambda () (root:install! (root:current) id disposition)) list))
      (widget:register! 'root-test 1
        (list '(focus . #t)
          (cons 'service
            (lambda (id frame)
              (case mode
                [(normalize) (interaction:set-state! head:ui-actor id #f 17)]
                [(race) (set! mode #f) (view:set-state! head:ui-actor id #f 18)])))
          (cons 'render
            (lambda (data d width height range)
              (when (eq? mode 'fail) (error 'fixture "preparation failed"))
              (list "界é root")))
          (cons 'event (lambda (id source d event)
                         (when (eq? (car event) 'key) (set! pressed (+ 1 pressed))) #t))
          (cons 'release (lambda (id) (set! released (cons id released))))))
      (parameterize ([sys:terminal-output-port output] [tui:input-delay 0])
        (let* ([initial (root:acquire! "root-test")]
               [old (make-view "old")] [candidate (make-view "next")])
          (set! mode 'normalize)
          (let ([reply (install old #f)])
            (test:check 'root-preparation-does-not-publish-private-normalization
              (list (car reply) (view:state (view:snapshot old)) (widget:shown)) '(applied 0 ())))
          (set! mode #f)
          (head:run-deferred!)
          (test:check 'root-installs-without-editor-buffers
            (list (map (lambda (p) (widget:frame-id (car p))) (widget:shown)) (store:buffer-list))
            (list (list old) '()))
          (head:key! "a")
          (let ([shown (widget:shown)] [binding (root:current)])
            (set! mode 'fail)
            (test:check 'root-failed-preparation-preserves-display-and-releases-candidate
              (list (test:raises? (lambda () (install candidate 'retire)))
                (equal? binding (root:current)) (equal? shown (widget:shown))
                (view:owner (view:snapshot candidate)) (member candidate released))
              (list #t #t #t #f (list candidate)))
            (set! mode 'race)
            (test:check 'root-stale-admission-keeps-old-lease-and-frame
              (list (car (install candidate 'retire)) (equal? binding (root:current))
                (equal? shown (widget:shown)) (view:owner (view:snapshot old)))
              (list 'stale #t #t head:ui-actor)))
          (set! mode #f)
          (let ([before (widget:shown)] [cleanup released])
            (keymap:call-with-command!
              (lambda ()
                (install candidate 'retire)
                (head:redraw!) (head:key! "a")
                (test:check 'root-admission-pins-frame-until-command-boundary
                  (list (equal? before (widget:shown)) (equal? cleanup released)
                    (view:snapshot old) (get (get (root:current) 'value) 'root) pressed)
                  (list #t #t #f candidate 1))))
            ;; A write failure cannot undo admission. No input map is published;
            ;; the next successful output uses that same admitted root.
            (let ([bad (make-custom-textual-output-port "failed-root"
                         (lambda args (error 'fixture "output failed")) #f #f void)])
              (parameterize ([sys:terminal-output-port bad]) (head:run-deferred!)))
            (test:check 'root-output-failure-disables-input-but-keeps-admission
              (list (widget:shown) (get (get (root:current) 'value) 'root) (and (member old released) #t))
              (list '() candidate #t))
            (head:key! "a")
            (head:redraw!) (head:key! "a")
            (test:check 'root-retry-publishes-canonical-generation
              (list pressed (map (lambda (p) (widget:frame-id (car p))) (widget:shown))
                (view:generation (widget:frame-descriptor (caar (widget:shown)))))
              (list 2 (list candidate) (view:generation (view:snapshot candidate)))))
          (model:register-kind! 'root-retainer 1 list?)
          (let ([retainer (model:create! head:ui-actor 'root-retainer 1 'session 'persistent (list candidate) '())])
            (install #f retainer) (head:run-deferred!)
            (test:check 'root-empty-retains-only-explicitly-owned-graph
              (list (widget:shown) (view:owner (view:snapshot candidate)) (get (get (root:current) 'value) 'root)) '( () #f #f))
            (model:retire! head:ui-actor retainer (model:revision retainer))
            (view:retire! head:ui-actor candidate (model:revision candidate)))
          ;; Unavailable extension definitions retain an inert placeholder, not
          ;; an accidental default editor or a failed replacement.
          (let ([unknown (view:create! head:ui-actor #f 'missing-root-definition 1 '() '())])
            (install unknown #f) (head:run-deferred!)
            (test:check 'root-missing-definition-is-an-inert-placeholder
              (list (widget:frame-id (caar (widget:shown))) (widget:caret (caar (widget:shown)))) (list unknown #f))
            (install #f 'retire) (head:run-deferred!))))
      (head:set-frame-hook! void)
      (head:set-key-handler! (lambda (event) (void)))
      (void)))
  (let ([session (policy:mint! head:ui-actor (policy:make 'all 1000 'any 8000))])
    (actor:call-as head:ui-actor
      (lambda () (operation:dispatch! 'root-fixture:run! '(() #f) '() (lambda (admission) (void)) session))))
  (kernel:retract-module! 'root-fixture))
