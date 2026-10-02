;; A linear prompt uses the same composed root and needs no window adapter.
(let ()
  (import (prefix (head modal) modal:) (prefix (head editor) editor:)
          (prefix (head message) message:) (prefix (apps eval) eval:))
  (define who head:ui-actor)
  (define answers '())
  (define requests '())
  (define keys 0)
  (define (get r k) (cdr (assq k r)))
  (modal:init!) (eval:init!)
  (let* ([root (view:create! who #f 'column 1 '() '())]
         [source (store:create! who "modal origin" '("Original document"))]
         [body (editor:create-view! who source '() root)] [host (modal:create! root '(modal-fixture))]
         [messages (message:create! root)])
    (define (show!)
      (widget:pump!)
      (let ([f (widget:prepare! root 40 10)]) (widget:present! (list (list f 0 0))) f))
    (define (start! text)
      (suspension:call! who void
        (lambda ()
          ;; A command can belong to an ancestor while the edit origin is
          ;; still the deepest focused descendant.
          (parameterize ([widget:target root])
            (set! answers (cons (prompt:read! "Input:" text #f '()) answers)))))
      (show!)
      (let* ([receiver (cadr (car (reverse (view:children (interaction:snapshot host)))))]
             [request (view:source (interaction:snapshot receiver))])
        (set! requests (cons request requests))
        (widget:descendant receiver 'prompt)))
    (define (settle!)
      (widget:pump!) (prompt:drain!) (suspension:drain! raise) (prompt:drain!) (show!))
    (view:arrange! who
      (list (list root 0 (list (list 'body body '(grow 1)) (list 'prompts host 'fit) (list 'messages messages 'fit))
              (list (list 'commands (list 'prompt host 'prepare '())
                      (list 'notification messages 'present '()) (list 'message messages 'show '()))))) '())
    (keymap:bind-default! 'modal-fixture "F8" (lambda () (set! keys (+ keys 1))))
    (widget:mount! root 'composed-prompt) (show!)
    (let* ([outer (start! "outer")] [inner (start! "inner")])
      (routing:input! root '(key "F8" #f))
      (check 'composed-prompts-capture-origin-parent-and-explicit-key-context
        (list (prompt:active?) answers keys
          (get (get (model:snapshot (car requests)) 'value) 'parent)
          (get (get (get (model:snapshot (cadr requests)) 'value) 'origin) 'view)
          (exists (lambda (line) (string:search line "outer" 0 (string-length line))) (widget:frame-lines (show!))))
        (list #t '() 1 (cadr requests) body #f))
      (prompt:cancel! inner) (settle!)
      (check 'composed-nested-cancel-restores-the-surviving-prompt
        (list answers (widget:focused root)) (list '(#f) (widget:descendant outer 'input 'entry)))
      (prompt:accept! outer) (settle!)
      (check 'composed-prompt-completion-restores-origin-and-closes-request-lifetimes
        (list answers (widget:focused root) (prompt:active?) (map model:snapshot requests))
        (list '("outer" #f) body #f '(#f #f))))
    (parameterize ([widget:target root])
      (suspension:call! who void eval:prompt!))
    (show!)
    (let* ([receiver (cadr (car (view:children (interaction:snapshot host))))]
           [prompt (widget:descendant receiver 'prompt)] [entry (widget:descendant prompt 'input 'entry)])
      (editor:insert! entry "+ 20 22)")
      (prompt:accept! prompt) (settle!)
      (check 'composed-mx-evaluates-and-presents-through-explicit-notification-host
        (list (widget:focused) (log:datum (car (log:entries 'eval:report!)))
          (exists (lambda (line) (and (string:search line "=> 42" 0 (string-length line)) #t))
            (widget:frame-lines (widget:prepared messages)))
          (store:line source 0))
        (list body '("(+ 20 22)" . "42") #t "Original document")))
    (start! "unmount")
    (widget:unmount! root) (prompt:drain!) (suspension:drain! raise)
    (check 'composed-host-removal-cancels-its-continuation-without-a-popup
      (list answers (model:snapshot (car requests)) (store:line source 0))
      '((#f "outer" #f) #f "Original document"))
    (view:retire! who root (model:revision root))))
