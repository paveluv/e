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
        (parameterize ([kernel:registering-module 'prompt-control-fixture]) (prompt-control:init!)))))
  (install!)
  (widget:register! 'prompt-fixture 1
    (append (layout:container 'y)
      (list (cons 'actions (list (cons 'accepted accepted!) (cons 'cancelled cancelled!))))))
  (let* ([host (view:create! who #f 'prompt-fixture 1 '() '())]
         [commands (list (list 'accepted host 'accepted '()) (list 'cancelled host 'cancelled '()))]
         [first (new "one" '(first-origin))] [second (new "two\nlines" '(second-origin))]
         [a (prompt-control:create! first '((label . "Single:") (help . "First prompt")) commands)]
         [b (prompt-control:create! second '((label . "Scheme:") (multiline? . #t)) commands)]
         [fork (view:fork! who a)])
    (view:arrange! who (list (list host 0 (list (list 'first a 'fit) (list 'second b '(grow 1))) '())) '())
    (widget:mount! fork 'forked-prompt)
    (widget:mount! host 'prompt-fixture)
    (check 'prompt-controller-is-explicit-and-cannot-be-rebound
      (list (field (field (model:snapshot first) 'value) 'controller)
        (test:raises? (lambda () (prompt-control:create! first '() commands)))
        (field (field (model:snapshot first) 'value) 'status)) (list a #t 'editing))
    (widget:prepare! host 32 8)
    (widget:focus! host (widget:descendant a 'input 'entry))
    (dispatch:input! host '(text "!" typed))
    (dispatch:input! host '(key "RET"))
    (widget:pump!)
    (check 'prompt-named-outcome-is-deferred (list outcomes (field (field (model:snapshot first) 'value) 'status)) '(() accepted))
    (prompt-control:drain!)
    (check 'prompt-fork-cannot-deliver-the-shared-outcome-twice
      outcomes '((accepted (1 #("!one") (first-origin)))))
    (prompt-control:accept! a) (widget:pump!) (prompt-control:drain!)
    (widget:focus! host (widget:descendant b 'input 'entry))
    (dispatch:input! host '(key "C-g")) (widget:pump!) (prompt-control:drain!)
    (check 'prompt-capture-cancels-multiline-input-without-affecting-other-outcomes
      outcomes '((cancelled (second-origin)) (accepted (1 #("!one") (first-origin)))))
    (widget:unmount! fork) (widget:unmount! host) (prompt-control:drain!)
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
           [root (prompt-control:create! request '((multiline? . #t)) '())]
           [draft (cadr (field (field (model:snapshot request) 'value) 'draft))])
      (widget:mount! root 'multiline-completion)
      (widget:prepare! root 30 5)
      (prompt-control:complete! root #f)
      (check 'completion-preserves-authored-trailing-newline-and-logical-caret
        (list (text-source:lines (text-source:lookup draft))
          (car (view:state (interaction:snapshot (widget:descendant root 'input 'entry))))) '(#("a" "b" "") (2 . 0)))
      (widget:unmount! root) (prompt-control:drain!)
      (set! factory-origins '()) (set! extensions 0))
    (let* ([request (prompt-request:create! who #f #f "a" '(completion-origin) '(prompt-fixture 1 ()))]
           [root (prompt-control:create! request '() '())]
           [entry #f]
           [draft (cadr (field (field (model:snapshot request) 'value) 'draft))])
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
      (kernel:retract-module! 'prompt-completion-fixture) (widget:pump!) (prompt-control:drain!)
      (check 'completion-provider-removal-cancels-owned-request (model:snapshot request) #f)
      (widget:unmount! root)))
  (for-each
    (lambda (reason)
      (let* ([request (new "late" '(late-origin))] [before outcomes]
             [receiver (view:create! who #f 'prompt-fixture 1 '() '())]
             [root (prompt-control:create! request '() (list (list 'accepted receiver 'accepted '())))])
        (widget:mount! receiver 'surviving-receiver)
        (widget:mount! root 'removed-prompt)
        (prompt-control:accept! root) (widget:pump!)
        (if (eq? reason 'reload) (begin (install!) (widget:pump!)) (widget:unmount! root))
        (prompt-control:drain!)
        (check (list 'prompt-host-lifetime-fences-queued-delivery reason)
          (list (model:snapshot request) outcomes) (list #f before))
        (widget:unmount! root) (widget:unmount! receiver) (prompt-control:drain!)
        (view:retire! who receiver (field (model:snapshot receiver) 'revision)))) '(unmount reload)))
