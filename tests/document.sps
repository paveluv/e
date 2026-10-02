;; Explicit catalogue entries keep identity across placement and containment.
(let ()
  (define (field r k) (cdr (assq k r)))
  (define source (catalogue:create-source! head:ui-actor "/home" 'transient))
  (define root (view:create! head:ui-actor #f 'row 1
                 (list '(name . "catalogue-host widget") '(catalogue . #t) (list 'audience head:ui-actor)) '()))
  (define foreign (view:create! head:ui-actor #f 'row 1
                    '((name . "catalogue-host foreign") (catalogue . #t) (audience (head "foreign"))) '()))
  (define child (view:create! head:ui-actor #f 'row 1 '((name . "catalogue-host child")) '()))
  (define listed (view:create! head:ui-actor #f 'row 1 '((name . "catalogue-host nested app") (catalogue . #t)) '()))
  (define container (view:create! head:ui-actor #f 'row 1 '((name . "catalogue-host container") (catalogue . #f)) '()))
  (define demands '())
  (define (query!)
    (let ([q (collection:create! head:ui-actor source "catalogue-host" '() 'transient)])
      (set! demands (cons (model:subscribe! (list q) void) demands)) q))
  (define (rows q)
    (let* ([s (collection:summary q)] [v (field s 'value)])
      (and (eq? (field v 'status) 'ready)
        (let ([p (collection:range q (field v 'generation) 0 20 '(name lines version))])
          (and (eq? (car p) 'ready) (list-ref p 4))))))
  (define (wait q n)
    (test:await 'host-catalogue-ready
      (lambda ()
        (let ([r (rows q)])
          (and r (= (length r) n)
            (for-all (lambda (row)
                       (let ([d (view:snapshot (cadr row))])
                         (and d (equal? (assq 'version (caddr row)) (list 'version 'ready (view:generation d)))))) r))))))
  (define (retire id) (let ([r (model:snapshot id)]) (when r (view:retire! head:ui-actor id (field r 'revision)))))
  (view:arrange! head:ui-actor
    (list (list root 0 (list (list 'control child 'fit) (list 'app listed 'fit)) (view:options (view:snapshot root)))) '())
  (let ([q (query!)])
    (wait q 2)
    (test:check 'catalogue-lists-explicit-entries-regardless-of-parent-without-generated-lines
      (list (map cadr (rows q)) (assq 'lines (caddar (rows q)))) (list (list listed root) '(lines absent)))
    (let* ([host (window-host:show-widget! (seat:current-window) root)] [barrier (query!)])
      (wait barrier 2)
      (test:check 'window-host-reference-keeps-base-identity
        (list (catalogue-host:reference host) (eq? host (catalogue-host:resolve! root))) (list root #t))
      (test:check 'foreign-head-cannot-retire-a-mounted-view
        (car (call-with-values (lambda () (view:retire! '(head "foreign") root (model:revision root))) list)) 'owned)
      (let ([revision (field (model:snapshot source) 'revision)])
        (interaction:set-state! head:ui-actor root #f '((selection . changed)))
        (interaction:flush!)
        (let ([probe (query!)])
          (wait probe 2)
          (test:check 'catalogue-selection-does-not-publish-metadata
            (field (model:snapshot source) 'revision) revision)
          (model:retire! head:ui-actor probe (model:revision probe))))
      (seat:forget-buffer! host)
      (wait q 2) ; hidden views retain identity across head departure
      (model:retire! head:ui-actor barrier (model:revision barrier)))
    ;; Removal releases borrowed children but never opts a control into the
    ;; catalogue merely because it has become a named root.
    (let ([host (window-host:show-widget! (seat:current-window) root)])
      (test:check 'own-mounted-view-retires-atomically
        (list (catalogue-host:retire! root (view:generation (view:snapshot root))) (memq host (seat:buffers))) '(#t #f)))
    (wait q 1)
    (test:check 'released-controls-stay-private-and-listed-apps-stay-visible
      (list (map cadr (rows q)) (view:parent (view:snapshot child)) (view:parent (view:snapshot listed)))
      (list (list listed) #f #f))
    (let ([host (window-host:show-widget! (seat:current-window) container)])
      (test:check 'explicit-catalogue-opt-out-survives-default-window-placement
        (assq 'catalogue (view:options (view:snapshot container))) '(catalogue . #f))
      (seat:forget-buffer! host))
    (view:arrange! head:ui-actor
      (list (list container (model:revision container) (list (list 'app listed 'fit)) (view:options (view:snapshot container)))) '())
    (view:claim! '(head "foreign") container)
    (test:check 'foreign-owned-parented-catalogue-entry-refuses-retirement
      (catalogue-host:retire! listed (view:generation (view:snapshot listed))) #f)
    (let ([stale (view:generation (view:snapshot listed))])
      (view:release! '(head "foreign") container (view:generation (view:snapshot container)))
      (view:claim! head:ui-actor container)
      (wait q 1)
      (test:check 'parented-entry-retirement-guards-generation-and-unlinks-only-its-identity
        (list (map cadr (rows q)) (catalogue-host:retire! listed stale)
          (catalogue-host:retire! listed (view:generation (view:snapshot listed)))
          (view:children (view:snapshot container)) (and (view:snapshot child) #t))
        (list (list listed) #f #t '() #t)))
    (retire container) (retire child) (wait q 0)
    (for-each (lambda (id) (model:retire! head:ui-actor id (model:revision id))) (list q source)))
  (retire foreign)
  (for-each model:unsubscribe! demands))
