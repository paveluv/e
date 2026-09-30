;; The same base jobs are completed, edited and presented without an outer window.
(let ()
  (define who head:ui-actor)
  (define (get r k) (cdr (assq k r)))
  (define (value id) (get (model:snapshot id) 'value))
  (define env (environment:create! who
                (list (cons 'directory (current-directory)) '(roots) '(imports (chezscheme))) 'transient))
  (define job (environment:evaluate! who env 1 "(define private-name 42) (display \"output\") private-name"))
  (define (pump!) (namespace:pump!) (widget:pump!))
  (define (lookup source text)
    (let-values ([(start end expansions names) ((completion:source-lookup source) text (string-length text))]) names))
  (define (finished? id) (not (memq (get (value id) 'status) '(running queued))))
  (dynamic-wind void
    (lambda ()
      (test:await 'widget-evaluation (lambda () (finished? job)))
      (control:init!) (entry:init!) (prompt:init!)
      (load "examples/environments.e")
      (let* ([panel (environment-example:panel! env "An embedded worksheet")])
        (widget:mount! panel 'example-panel)
        (let* ([prompt (widget:descendant panel 'prompt)] [entry (widget:descendant prompt 'input 'entry)])
          (let* ([receivers (widget:receivers entry)]
                 [source ((completion:provider '(scheme 1 ())) '() (list (cons 'receivers receivers)))]
                 [text "(environment:reset! head:ui-actor "])
            (let-values ([(start end expand candidates) ((completion:source-lookup source) text (string-length text))])
              (check 'model-receiver-is-explicitly-exposed-and-completes-without-id-lookup
                (list (and (assoc env receivers) (widget:receiver-live? (assoc env receivers)))
                  (map completion:candidate-value candidates))
                (list #t (list (format "(model ~a)" (cadr env)))))))
          (let ([draft (cadr (get (view:options (view:snapshot panel)) 'draft))])
            (store:reset! who draft '("(+ private-name 1)")))
          (pump!) (prompt:accept! prompt) (pump!) (prompt:drain!)
          (pump!)
          (let* ([result (widget:descendant panel 'result)] [job (view:source (view:snapshot result))])
            (test:await 'example-submits-through-public-api (lambda () (finished? job)))
            (check 'worksheet-example-composes-prompt-job-and-result
              (get (value job) 'result) '(value (43) "(43)"))))
        (widget:unmount! panel) (prompt:drain!))
      (let* ([factory (completion:provider '(environment 1 ignored))]
             [a (factory env '())] [b (factory env '())])
        (test:await 'environment-symbol-pages
          (lambda () (pump!) (member "private-name" (lookup a "(privat"))))
        (check 'model-completion-uses-native-names-and-free-nested-scheme
          (list (lookup a "(list (privat") (lookup b "(privat") (lookup a "(head:currentwin"))
          '(("private-name") ("private-name") ()))
        (let* ([s (completion-state:create a #f)] [text "(privat"]
               [old ((completion:source-basis a))])
          (completion-state:refresh! s text (string-length text))
          (completion-state:normalize! s a #f)
          (completion-state:normalize! s a #f)
          (let ([shown (completion-state:snapshot s)]
                [next (environment:evaluate! who env 1 "(define private-next 43)")])
            (test:await 'new-catalogue
              (lambda () (pump!) (and (finished? next) (not (equal? old ((completion:source-basis a)))))))
            (check 'changed-catalogue-refuses-stale-completion-choice
              (completion-state:choose! s (car shown) "private-name") #f))
          (completion-state:finish! s #f))
        ((completion:source-release b)))
      (let* ([view (eval:create-result-view! who job)] [fork (view:fork! who view)]
             [children (test:child-pids)]
             [draft (list 'buffer (store:create! who "model prompt" '("(+ private-name 1)") '((internal . #t))))]
             [prompt (eval:create-model-prompt! env 1 draft '())])
        (widget:mount! view 'result-a) (widget:mount! fork 'result-b) (widget:mount! prompt 'model-prompt)
        (pump!)
        (check 'result-views-share-output-with-independent-geometry-and-no-worker-allocation
          (list (map (lambda (id width)
                       (let ([lines (widget:frame-lines (widget:prepare! id width 4))])
                         (and (exists (lambda (line) (string:search line "output" 0 (string-length line))) lines)
                           (exists (lambda (line) (string:search line "=> (42)" 0 (string-length line))) lines) #t)))
                  (list view fork) '(20 60))
            (equal? children (test:child-pids))) '((#t #t) #t))
        (environment:reset! who env 1) (pump!)
        (check 'model-prompt-refuses-a-reset-origin (prompt:accept! prompt) 'invalid)
        (for-each widget:unmount! (list view fork prompt)) (prompt:drain!)
        (check 'unmount-borrows-job-and-authored-draft
          (list (and (model:snapshot job) #t) (store:exists? (cadr draft))) '(#t #t))))
    (lambda () (environment:close! who env (get (value env) 'generation)))))
