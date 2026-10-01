;; Embedded prompts use ordinary text controls and explicit named outcomes.
(let ()
  (define who head:ui-actor)
  (define outcomes '())
  (define (field r k) (cdr (assq k r)))
  (define (accepted! id value) (set! outcomes (cons (list 'accepted value) outcomes)))
  (define (cancelled! id value) (set! outcomes (cons (list 'cancelled value) outcomes)))
  (define (new text origin) (prompt-request:create! who #f #f text origin #f))
  (define (install!)
    (kernel:call-with-registration-update
      (lambda ()
        (kernel:retract-module! 'prompt-control-fixture)
        (parameterize ([kernel:registering-module 'prompt-control-fixture]) (prompt:init!)))))
  (install!)
  (let* ([request (new "" '(question))]
         [root (prompt:create! request '((label . "A question long enough to wrap: yes or no?") (choices . "yn")) '())])
    (widget:mount! root 'question-fixture) (widget:prepare! root 18 6)
    (widget:focus! root (widget:descendant root 'input 'entry))
    (for-each (lambda (event) (dispatch:input! root event))
      '((key "UP") (text "ny" paste) (text "x" typed) (key "RET")))
    (check 'question-refuses-navigation-unlisted-input-and-multiple-answers
      (field (field (model:snapshot request) 'value) 'status) 'editing)
    (dispatch:input! root '(key "Y" "Y"))
    (check 'question-captures-one-case-insensitive-answer
      (cadr (field (field (model:snapshot request) 'value) 'outcome)) '#("Y"))
    (widget:unmount! root) (prompt:drain!))
  (let ([hints 0])
    (parameterize ([kernel:registering-module 'prompt-profile-fixture])
      (prompt:register-profile! 'fixture 1
        (lambda (configuration origin)
          (list '(history "first" "second") (cons 'normalize string-upcase)
            (cons 'validate (lambda (text) (and (not (string=? text "GOOD")) "Use GOOD")))
            (cons 'ghost (lambda (text position) (set! hints (+ hints 1)) "help"))))))
    (let* ([request (new "draft" '(profile-origin))]
           [root (prompt:create! request '((profile fixture 1 ())) '())]
           [entry #f])
      (define (text) (vector-ref (text-source:lines (text-source:lookup (field (field (model:snapshot request) 'value) 'draft))) 0))
      (widget:mount! root 'profile-fixture) (widget:pump!)
      (set! entry (widget:descendant root 'input 'entry))
      (let ([before hints])
        (widget:prepare! root 30 4) (widget:prepare! root 50 3)
        (check 'prompt-hints-are-prepared-without-paint-callbacks hints before))
      (check 'prompt-history-restores-unfinished-input
        (map (lambda (direction) (prompt:history! root direction) (text)) '(previous previous next next))
        '("first" "second" "first" "draft"))
      (entry:set-text! entry "bad")
      (let ([status (prompt:accept! root)])
        (check 'prompt-validates-normalized-authored-input-before-accepting
          (list status (text) (field (field (model:snapshot request) 'value) 'status)
            (and (find (lambda (line) (string:search line "[Use GOOD]" 0 (string-length line)))
                   (widget:frame-lines (widget:prepare! root 30 4))) #t)) '(invalid "BAD" editing #t)))
      (entry:set-text! entry "good") (prompt:accept! root)
      (check 'prompt-acceptance-captures-normalized-text
        (cadr (field (field (model:snapshot request) 'value) 'outcome)) '#("GOOD"))
      (widget:unmount! root) (prompt:drain!))
    (kernel:retract-module! 'prompt-profile-fixture))
  (widget:register! 'prompt-fixture 1
    (append (layout:container 'y)
      (list (cons 'actions (list (cons 'accepted accepted!) (cons 'cancelled cancelled!))))))
  (let* ([host (view:create! who #f 'prompt-fixture 1 '() '())]
         [commands (list (list 'accepted host 'accepted '()) (list 'cancelled host 'cancelled '()))]
         [first (new "one" '(first-origin))] [second (new "two\nlines" '(second-origin))]
         [a (prompt:create! first '((label . "Single:") (help . "First prompt")) commands)]
         [b (prompt:create! second '((label . "Scheme:") (multiline? . #t)) commands)]
         [fork (view:fork! who a)])
    (view:arrange! who (list (list host 0 (list (list 'first a 'fit) (list 'second b '(grow 1))) '())) '())
    (widget:mount! fork 'forked-prompt)
    (widget:mount! host 'prompt-fixture)
    (check 'prompt-controller-is-explicit-and-cannot-be-rebound
      (list (field (field (model:snapshot first) 'value) 'controller)
        (test:raises? (lambda () (prompt:create! first '() commands)))
        (field (field (model:snapshot first) 'value) 'status)) (list a #t 'editing))
    (widget:prepare! host 32 8)
    (widget:focus! host (widget:descendant a 'input 'entry))
    (dispatch:input! host '(text "!" typed))
    (dispatch:input! host '(key "RET"))
    (widget:pump!)
    (check 'prompt-named-outcome-is-deferred (list outcomes (field (field (model:snapshot first) 'value) 'status)) '(() accepted))
    (prompt:drain!)
    (check 'prompt-fork-cannot-deliver-the-shared-outcome-twice
      outcomes '((accepted (1 #("!one") (first-origin)))))
    (prompt:accept! a) (widget:pump!) (prompt:drain!)
    (widget:focus! host (widget:descendant b 'input 'entry))
    (dispatch:input! host '(key "C-g")) (widget:pump!) (prompt:drain!)
    (check 'prompt-capture-cancels-multiline-input-without-affecting-other-outcomes
      outcomes '((cancelled (second-origin)) (accepted (1 #("!one") (first-origin)))))
    (widget:unmount! fork) (widget:unmount! host) (prompt:drain!)
    (check 'prompt-host-removal-releases-transient-requests-and-controls
      (map model:snapshot (list first second a b fork)) '(#f #f #f #f #f))
    (view:retire! who host (field (model:snapshot host) 'revision)))
  (let ([lookups 0] [extensions 0] [factory-origins '()])
    (parameterize ([kernel:registering-module 'prompt-completion-fixture])
      (completion:register! 'prompt-fixture 1
        (lambda (configuration origin)
          (set! factory-origins (cons origin factory-origins))
          (completion:make-source
            (lambda (text caret)
              (set! lookups (+ lookups 1))
              (values 0 (string-length text)
                (lambda () (set! extensions (+ extensions 1)) (if (string? configuration) (list configuration) '("alpha!")))
                (if (string? configuration) (list configuration)
                  (if (string=? text "none") '() '("alpha!" "alpha-beta!")))))))))
    (let* ([request (prompt-request:create! who #f #f "a" '(multiline) '(prompt-fixture 1 "a\nb\n"))]
           [root (prompt:create! request '((multiline? . #t)) '())]
           [draft (field (field (model:snapshot request) 'value) 'draft)])
      (widget:mount! root 'multiline-completion)
      (widget:prepare! root 30 5)
      (prompt:complete! root #f)
      (check 'completion-preserves-authored-trailing-newline-and-logical-caret
        (list (text-source:lines (text-source:lookup draft))
          (car (view:state (interaction:snapshot (widget:descendant root 'input 'entry))))) '(#("a" "b" "") (2 . 0)))
      (widget:unmount! root) (prompt:drain!)
      (set! factory-origins '()) (set! extensions 0))
    (let* ([request (prompt-request:create! who #f #f "a" '(completion-origin) '(prompt-fixture 1 ()))]
           [root (prompt:create! request '() '())]
           [entry #f]
           [draft (field (field (model:snapshot request) 'value) 'draft)])
      (define (line) (vector-ref (text-source:lines (text-source:lookup draft)) 0))
      (define (show!) (widget:pump!) (widget:present! (list (list (widget:prepare! root 30 5) 0 0))))
      (widget:mount! root 'completion-prompt)
      (set! entry (widget:descendant root 'input 'entry))
      (show!) (widget:focus! root entry) (entry:select! entry 1 1)
      (show!) (dispatch:input! root '(key "TAB")) (show!)
      (check 'embedded-prompt-normalizes-authored-text (list (line) factory-origins) '("alpha!" ((completion-origin))))
      (dispatch:input! root '(key "TAB")) (show!)
      (let* ([binding (cadr (assoc '(click primary ()) (widget:pointer-bindings 1 0)))] [before lookups])
        (widget:prepare! root 30 5) (widget:pointer-bindings 2 0)
        (check 'completion-paint-and-hit-discovery-do-not-query-provider (list lookups extensions) (list before 1))
        (entry:set-text! entry "none")
        (check 'embedded-completion-rejects-stale-mouse-binding
          (list (keymap:run! binding) (line)) '(#f "none")))
      (entry:set-text! entry "a") (show!)
      (dispatch:input! root '(key "TAB")) (dispatch:input! root '(key "TAB")) (show!)
      (widget:pointer! '(pointer press primary ()) 15 0)
      (check 'embedded-completion-mouse-chooses-current-value (line) "alpha-beta!")
      (kernel:retract-module! 'prompt-completion-fixture) (widget:pump!) (prompt:drain!)
      (check 'completion-provider-removal-cancels-owned-request (model:snapshot request) #f)
      (widget:unmount! root)))
  (let ([builds 0] [type '(one-of alpha beta)])
    (define (factory request context commands)
      (set! builds (+ builds 1))
      (let* ([root (view:create! who #f 'column 1 '() '() request)]
             [body (prompt:create-choices! request commands 'table root)])
        (view:arrange! who (list (list root 0 (list (list 'body body 'fit)) '())) '()) root))
    (parameterize ([kernel:registering-module 'completion-type-fixture])
      (prompt:register-presentation! type factory)
      (completion:register! 'typed-fixture 1
        (lambda (config origin)
          (completion:make-source
            (lambda (text caret) (values 0 (string-length text) '("alpha") '("alpha" "beta")))
            #f "choice" (lambda () #f) void
            (lambda (text caret) (list (cons 'type (if (string=? text "other") '(one-of alpha gamma) type))))))))
    (check 'exact-completion-type-claims-conflict-across-module-owners
      (test:raises? (lambda () (parameterize ([kernel:registering-module 'completion-type-conflict]) (prompt:register-presentation! type factory)))) #t)
    (let* ([request (prompt-request:create! who #f #f "a" '(typed-origin) '(typed-fixture 1 ()))]
           [root (prompt:create! request '() '())])
      (define (show!) (widget:pump!) (widget:present! (list (list (widget:prepare! root 36 6) 0 0))))
      (define (choice) (widget:descendant root 'choices 'presentation))
      (widget:mount! root 'typed-completion) (show!)
      (let ([entry (widget:descendant root 'input 'entry)] [first (choice)] [body (widget:descendant (choice) 'body)])
        (prompt:complete! root #f) (prompt:complete! root #f) (show!)
        (check 'exact-compound-type-chooses-a-table-over-the-same-provider
          (list builds (field (view:options (interaction:snapshot body)) 'layout)
            (list-ref (field (prompt:completion-context request) 'snapshot) 3)) '(1 table ("alpha" "beta")))
        (entry:set-text! entry "a2") (show!)
        (check 'typing-within-one-type-keeps-its-composition (list builds (equal? first (choice))) '(1 #t))
        (entry:set-text! entry "other")
        (test:await 'completion-type-replacement (lambda () (show!) (not (equal? first (choice)))))
        (check 'unregistered-compound-type-falls-back-and-retires-old-views
          (list (field (view:options (interaction:snapshot (choice))) 'layout) (map model:snapshot (list first body))) '(columns (#f #f)))
        (entry:set-text! entry "a")
        (test:await 'completion-type-restored (lambda () (show!) (eq? (view:kind (interaction:snapshot (choice))) 'column)))
        (entry:set-text! entry "other") (prompt:accept! root)
        (let ([generation (view:generation (interaction:snapshot root))] [shown (choice)])
          (show!)
          (check 'accepted-prompt-cannot-renew-the-queued-outcomes-lease
            (list (= generation (view:generation (interaction:snapshot root))) (equal? shown (choice))) '(#t #t)))
        (kernel:retract-module! 'completion-type-fixture) (show!) (prompt:drain!)
        (check 'completion-type-provider-removal-releases-the-request (model:snapshot request) #f))
      (widget:unmount! root)))
  (for-each
    (lambda (reason)
      (let* ([request (new "late" '(late-origin))] [before outcomes]
             [receiver (view:create! who #f 'prompt-fixture 1 '() '())]
             [root (prompt:create! request '() (list (list 'accepted receiver 'accepted '())))])
        (widget:mount! receiver 'surviving-receiver)
        (widget:mount! root 'removed-prompt)
        (prompt:accept! root) (widget:pump!)
        (if (eq? reason 'reload) (begin (install!) (widget:pump!)) (widget:unmount! root))
        (prompt:drain!)
        (check (list 'prompt-host-lifetime-fences-queued-delivery reason)
          (list (model:snapshot request) outcomes) (list #f before))
        (widget:unmount! root) (widget:unmount! receiver) (prompt:drain!)
        (view:retire! who receiver (field (model:snapshot receiver) 'revision)))) '(unmount reload))
  (let ([answers '()] [original (head:current-window)] [buffer (head:current-buffer-mirror)] [requests '()])
    (define (start! text)
      (suspension:call! who void
        (lambda () (set! answers (cons (prompt:read! "Input:" text #f '()) answers))))
      (widget:pump!)
      (let* ([root (head:window-widget (head:popup))]
             [receiver (cadr (car (reverse (view:children (interaction:snapshot root)))))]
             [request (view:source (interaction:snapshot receiver))])
        (set! requests (cons request requests))
        (widget:descendant receiver 'prompt)))
    (define (settle!)
      (widget:pump!) (prompt:drain!)
      (suspension:drain! raise) (prompt:drain!))
    (parameterize ([kernel:registering-module 'prompt-host-fixture]) (prompt-host:init!))
    (let* ([outer (start! "outer")] [inner (start! "inner")])
      (check 'linear-prompts-park-and-capture-parent
        (list answers (field (field (model:snapshot (car requests)) 'value) 'parent)) (list '() (cadr requests)))
      (prompt:cancel! inner) (settle!)
      (check 'nested-prompt-cancellation-keeps-outer-mounted
        (list answers (field (field (model:snapshot (cadr requests)) 'value) 'status)) '((#f) editing))
      (prompt:accept! outer) (settle!)
      (check 'linear-prompt-restores-host-and-retires-request-tree
        (list answers (eq? original (head:current-window)) (eq? buffer (head:current-buffer-mirror)) (map model:snapshot requests))
        '(("outer" #f) #t #t (#f #f))))
    (start! "reload")
    (kernel:retract-module! 'prompt-host-fixture)
    (settle!)
    (check 'prompt-host-removal-cancels-suspended-caller
      (list answers (model:snapshot (car requests))) '((#f "outer" #f) #f))))
