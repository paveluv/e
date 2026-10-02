;; Supported checkpoint ingress: one mixed layout and one failed retry.
(let ()
  (import (prefix (state screen-import) screen-import:))
  (define who '(head "screen import"))
  (define (get r k) (cdr (assq k r)))
  (define (option id k) (get (view:options (view:snapshot id)) k))
  (actor:register! who (lambda args (void)))
  (let* ([shared (store:create! who "old shared" '("abcdef"))]
         [saved-editor (view:create! who shared 'editor 1 '() '((0 . 4) (0 . 1) (0 . 0) #t))]
         [host (view:create! who #f 'window-tool 1 '((name . "<old app>") (catalogue . #t)) '())]
         [app (view:create! who #f 'label 1 (list '(text . "extension state") (list 'commands (list 'return host 'return '()))) '((authored . "keep")))]
         [foreign '((extension (window 8) (buffer 123) (lambda () (error 'never "run"))) #f ())]
         [entries (list (list (list 'shared shared 0) #f '())
                    '((local "<draft>" 13 ((trailing . #f)) ("private text")) #t ((1 . (0 . 7)) (mark . (0 . 2))))
                    '((local "<hidden draft>" 4 () ("hidden text")) #f ())
                    (list (list 'widget host) #f '()) foreign)]
         [layout (list 'split 'right 3 2 (list 'window 8 0 0 0 #t #f (list (cons shared saved-editor)))
                   '(split below 1 4 (window 1 1 0 0 #f #t ()) (window 3 3 0 0 default default ())))]
         [checkpoint (list 'screen 6 1 layout entries)])
    (view:arrange! who (list (list host 0 (list (list 'app app '(grow 1))) (view:options (view:snapshot host)))) '())
    (view:set-state! who saved-editor 0 (view:state (view:snapshot saved-editor)))
    (actor:checkpoint! who checkpoint)
    (let ([before (list (model:ids) (store:buffer-list))])
      (check 'screen-import-validates-before-allocation-and-does-not-execute-payloads
        (list (test:raises? (lambda () (screen-import:create! who #f (list 'screen 6 99 layout entries))))
          (list (model:ids) (store:buffer-list))) (list #t before)))
    (actor:call-as who
      (lambda ()
        (let* ([attempts (test:parallel 2 (lambda (n) (screen-import:create! who #f checkpoint)))]
               [manager (car attempts)] [windows (window:list manager)]
               [first (window:numbered manager 8)] [draft-window (window:numbered manager 1)]
               [app-window (window:numbered manager 3)] [draft (window:document manager draft-window)]
               [editor (cadr (assq 'document (view:children (view:snapshot first))))]
               [app-copy (window:document manager app-window)]
               [split (cadr (assq 'layout (view:children (view:snapshot manager))))]
               [payloads (filter model:reference? (window:documents manager first))])
          (check 'screen-import-preserves-layout-selection-and-retained-logical-state
            (list (map (lambda (id) (option id 'number)) windows) (window:current manager)
              (view:state (view:snapshot split))
              (view:state (view:snapshot (view:parent (view:snapshot draft-window))))
              (view:basis (view:snapshot editor)) (view:state (view:snapshot editor))
              (option draft-window 'wrap) (option draft-window 'line-numbers))
            (list '(8 1 3) draft-window '(3 2) '(1 4) 0 '((0 . 4) (0 . 1) (0 . 0) #t) #f #t))
          (check 'screen-import-preserves-hidden-text-and-extension-payloads-and-remaps-host-commands
            (list (store:line draft 0) (store:visible? '(head "another") draft)
              (store:line (store:find-named "<hidden draft>") 0)
              (view:state (view:snapshot app-copy))
              (option app-copy 'commands)
              (map (lambda (id) (option id 'recovery))
                (filter (lambda (id) (assq 'recovery (view:options (view:snapshot id)))) payloads))
              (actor:checkpoint who))
            (list "private text" #f "hidden text" '((authored . "keep"))
              (list (list 'return app-window 'return '())) (list foreign) checkpoint))
          (check 'screen-import-concurrent-first-attempts-share-the-promoted-document
            (window:document (cadr attempts) (window:numbered (cadr attempts) 1)) draft)
          (view:retire! who (cadr attempts) (model:revision (cadr attempts)))
          (store:edit! who draft 0 (text:make-span 0 0 0 7) '("changed"))
          (let* ([buffers (store:buffer-list)] [retry (window:restore-manager! #f)])
            (check 'screen-import-retries-reuse-authored-text-without-overwriting-new-edits
              (list (store:buffer-list) (window:document retry (window:numbered retry 1)) (store:line draft 0))
              (list buffers draft "changed text"))
            (view:retire! who retry (model:revision retry)))
          ;; A late construction failure must keep promotions but discard its
          ;; complete candidate. Retrying then adopts the same private text.
          (let* ([fresh (list 'screen 6 1 '(window 1 0 0 0 default default ())
                          '(((local "<failure draft>" 0 () ("important")) #f ())))]
                 [before (model:ids)] [fail? #t]
                 [token (model:subscribe! #f
                          (lambda (event)
                            (when fail?
                              (let ([ids (filter (lambda (id) (and (not (member id before))
                                                                (eq? (view:kind (view:snapshot id)) 'window-manager))) (model:ids 'widget-view))])
                                (when (pair? ids) (set! fail? #f) (view:retire! who (car ids) (model:revision (car ids))))))))])
            (check 'screen-import-failed-candidate-cleans-models-and-keeps-sole-authored-copy
              (list (test:raises? (lambda () (screen-import:create! who #f fresh))) (model:ids)
                (store:line (store:find-named "<failure draft>") 0)) (list #t before "important"))
            (model:unsubscribe! token))
          (view:retire! who manager (model:revision manager)))))))

(let ()
  (define who '(head "screen admission"))
  (define session (policy:mint! who (policy:make 'all 1000 'any 8000)))
  (define (get r key) (cdr (assq key r)))
  (define (invoke name contract args)
    (actor:call-as who
      (lambda () (cdr (operation:dispatch! name contract args (lambda (admission) (void)) session)))))
  (define (acquire)
    (car (invoke 'composition:acquire! '((string) (list list)) '("editor"))))
  (define (admit binding root)
    (invoke 'composition:admit! '((list (or model #f) list (or model #f (one-of retire))) (symbol datum list))
      (list binding root (if root (view:tree root) '()) (if root #f 'retire))))
  (define old '(screen 6 1 (window 1 0 0 0 default default ()) (((local "<admitted draft>" 0 () ("old")) #f ()))))
  (define newer '(screen 6 1 (window 1 0 0 0 default default ()) (((local "<admitted draft>" 1 () ("new")) #f ()))))
  (actor:register! who (lambda args (void))) (actor:checkpoint! who old)
  (let* ([binding (acquire)] [candidate (actor:call-as who (lambda () (window:restore-manager! #f)))])
    (actor:checkpoint! who newer)
    (check 'screen-admission-refuses-changed-input-without-touching-either-version
      (list (test:raises? (lambda () (admit binding candidate))) (model:snapshot (get binding 'id))
        (actor:checkpoint who) (view:owner (view:snapshot candidate))) (list #t binding newer #f))
    (actor:checkpoint! who old)
    (let* ([pending? #t] [token (model:subscribe! (list (get binding 'id))
                                  (lambda (event)
                                    (when pending? (set! pending? #f) (actor:checkpoint! who newer))))]
           [result (admit binding candidate)])
      (model:unsubscribe! token)
      (check 'screen-admission-keeps-new-input-arriving-during-commit-delivery
        (list (car result) (get (get (cadr result) 'value) 'root) (actor:checkpoint who))
        (list 'applied candidate newer)))
    ;; Simulate a session saved after admission but before acknowledgement.
    (actor:checkpoint! who old)
    (let ([restored (acquire)])
      (check 'screen-acquisition-finishes-interrupted-input-consumption
        (list (get (get restored 'value) 'root) (actor:checkpoint who)) (list candidate #f))
      (admit restored #f))))
