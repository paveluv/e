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
