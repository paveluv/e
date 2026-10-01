;; Named views are catalogue entries without a head contribution stream.
(let ()
  (define (field r k) (cdr (assq k r)))
  (define source (catalogue:create-source! head:ui-actor "/home" 'transient))
  (define root (view:create! head:ui-actor #f 'row 1
                 (list '(name . "catalogue-host widget") (list 'audience head:ui-actor)) '()))
  (define foreign (view:create! head:ui-actor #f 'row 1
                    '((name . "catalogue-host foreign") (audience (head "foreign"))) '()))
  (define child (view:create! head:ui-actor #f 'row 1 '((name . "catalogue-host child")) '()))
  (define demands '())
  (define (query!)
    (let ([q (collection:create! head:ui-actor source "catalogue-host" '() 'transient)])
      (set! demands (cons (model:subscribe! (list q) void) demands)) q))
  (define (rows q)
    (let* ([s (collection:summary q)] [v (field s 'value)])
      (and (eq? (field v 'status) 'ready)
        (let ([p (collection:range q (field v 'generation) 0 20 '(name lines version))])
          (and (eq? (car p) 'ready) (list-ref p 4))))))
  (define (wait q n) (test:await 'host-catalogue-ready (lambda () (let ([r (rows q)]) (and r (= (length r) n))))))
  (define (retire id) (let ([r (model:snapshot id)]) (when r (view:retire! head:ui-actor id (field r 'revision)))))
  (view:arrange! head:ui-actor
    (list (list root 0 (list (list 'nested child 'fit)) (view:options (view:snapshot root)))) '())
  (let ([q (query!)])
    (wait q 1)
    (test:check 'catalogue-lists-only-visible-named-roots-without-generated-lines
      (list (map cadr (rows q)) (assq 'lines (caddar (rows q)))) (list (list root) '(lines absent)))
    (let* ([host (window:show-widget! (head:current-window) root)] [barrier (query!)])
      (wait barrier 1)
      (test:check 'window-host-reference-keeps-base-identity
        (list (catalogue-host:reference host) (eq? host (catalogue-host:resolve! root))) (list root #t))
      (test:check 'foreign-head-cannot-retire-a-mounted-view
        (car (call-with-values (lambda () (view:retire! '(head "foreign") root (model:revision root))) list)) 'owned)
      (let ([revision (field (model:snapshot source) 'revision)])
        (interaction:set-state! head:ui-actor root #f '((selection . changed)))
        (interaction:flush!)
        (let ([probe (query!)])
          (wait probe 1)
          (test:check 'catalogue-selection-does-not-publish-metadata
            (field (model:snapshot source) 'revision) revision)
          (model:retire! head:ui-actor probe (model:revision probe))))
      (head:forget-buffer! host)
      (wait q 1) ; hidden views retain identity across head departure
      (model:retire! head:ui-actor barrier (model:revision barrier)))
    ;; Removing the named root also removes its row; the borrowed child is
    ;; retained as a newly discoverable root, not deleted with its host.
    (let ([host (window:show-widget! (head:current-window) root)])
      (test:check 'own-mounted-view-retires-atomically
        (list (catalogue-host:retire! root (view:generation (view:snapshot root))) (memq host (head:buffers))) '(#t #f)))
    (test:await 'catalogue-released-child (lambda () (let ([r (rows q)]) (and r (equal? (map cadr r) (list child))))))
    (retire child) (wait q 0)
    (for-each (lambda (id) (model:retire! head:ui-actor id (model:revision id))) (list q source)))
  (retire foreign)
  (for-each model:unsubscribe! demands))
