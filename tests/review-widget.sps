;; Two independent reviews and a rewrite compose inside an ordinary nested host.
(let ()
  (import (prefix (head control) control:) (prefix (head table) table:) (prefix (head range) range:)
          (prefix (head interaction) interaction:) (prefix (state view) view:)
          (prefix (state model) model:) (prefix (state collection) collection:))
  (define actor head:ui-actor)
  (define (get r k) (cdr (assq k r)))
  (define (child id name) (cadr (assq name (view:children (or (interaction:snapshot id) (view:snapshot id))))))
  (define (query id) (view:source (or (interaction:snapshot id) (view:snapshot id))))
  (define (selected id) (get (view:state (interaction:snapshot (child id 'table))) 'selection))
  (define (basis id) (get (view:state (interaction:snapshot (child id 'table))) 'basis))
  (define document (store:create! actor "review 界" '("base" "tail") '((base . "base\ntail") (trailing . #f))))
  (define rewrite-document (store:create! actor "history" '("abc")))
  (define other-lines (append (make-list 20 "context") '("base")))
  (define other (store:create! actor "second reviewed document" other-lines
                  (list (cons 'base (string:join other-lines "\n")) '(trailing . #f))))
  (define prepared
    (begin
      (store:edit! actor document 0 (text:make-span 0 0 0 4) '("mine"))
      (store:reload! bot document '("disk" "tail") '((base . "disk\ntail") (trailing . #f)))
      (store:edit! actor rewrite-document 0 (text:make-span 0 1 0 2) '("B"))
      (store:edit! actor rewrite-document 1 (text:make-span 0 3 0 3) '("X"))
      (store:edit! actor other 0 (text:make-span 20 0 20 4) '("other mine"))
      (let ([disk (append (make-list 20 "context") '("other disk"))])
        (store:reload! bot other disk (list (cons 'base (string:join disk "\n")) '(trailing . #f))))
      (control:init!) (table:init!)))
  (define a (delta-log:create! '() 'conflicts (list document other)))
  (define b (delta-log:create! '() 'conflicts (list document)))
  (define c (delta-log:create! '() 'rewrite (list rewrite-document)))
  (define root (view:create! actor #f 'row 1 '() '()))
  (define (preview id) (query (child id 'preview)))
  (define (output id) (cadr (query (child (child id 'preview) 'text))))
  (define (pump!)
    (head:before-frame!) (range:pump!) (widget:pump!)
    (widget:present! (list (list (widget:prepare! root 240 16) 0 0)))
    (interaction:publish!))
  (define (ready id)
    (let* ([v (get (collection:summary (query id)) 'value)] [s (selected id)]
           [p (get (model:snapshot (preview id)) 'value)])
      (and (eq? (get v 'status) 'ready) s (= (cadr s) (get v 'generation))
           (eq? (get p 'status) 'ready) (equal? s (cadr (get p 'basis))))))
  (define (await id) (test:await 'review-ready (lambda () (pump!) (ready id))))
  (view:arrange! actor (list (list root 0 (list (list 'a a '(grow 2)) (list 'b b '(grow 1)) (list 'c c '(grow 1))) '())) '())
  (widget:mount! root 'review-fixture)
  (for-each await (list a b c))
  (check 'independent-review-widgets-borrow-sources-and-own-output
    (list (not (equal? (query a) (query b))) (store:line (output a) 0) (store:property (output a) 'read-only)) '(#t "disk" #t))
  (let ([selection (selected a)] [shown (basis a)])
    (table:invoke! (child a 'table) 'mine)
    (await a)
    (check 'row-choices-project-without-settlement-or-cross-review-changes
      (list (store:line (output a) 0) (store:line (output b) 0) (store:line document 0)
        (test:raises? (lambda () (delta-log:choose! a 'disk selection shown)))) '("mine" "disk" "disk" #t)))
  (control:activate! (child (child a 'heading) 'disk)) (await a)
  (table:move! (child a 'table) 'next) (await a)
  (check 'selection-connection-retargets-preview-and-reveals-the-region
    (list (store:line (output a) 20)
      (car (view:state (interaction:snapshot (child (child a 'preview) 'text))))) '("other disk" (20 . 0)))
  (check 'bulk-control-and-key-command-share-one-draft
    (begin (delta-log:choose-all! a 'mine) (await a) (store:line (output a) 20)) "other mine")
  (let ([before (store:revision (output c))])
    (table:select! (child c 'table) (list rewrite-document 1)) (await c)
    (check 'rewrite-row-navigation-only-changes-provenance
      (list (= before (store:revision (output c)))
        (caddr (get (get (model:snapshot (preview c)) 'value) 'annotations))) '(#t (((0 1 0 2) match)))))
  (table:invoke! (child c 'table) 'activate) (await c)
  (check 'rewrite-widget-previews-without-touching-the-source
    (list (store:line (output c) 0) (store:line rewrite-document 0)) '("abcX" "aBcX"))
  (check 'rewrite-widget-settles-through-its-shown-basis
    (list (car (delta-log:settle! c)) (store:line rewrite-document 0)) '(applied "abcX"))
  (delta-log:filter! c '((count . 1))) (await c)
  (check 'history-filter-keeps-the-query-and-bounds-the-visible-entries
    (get (get (collection:summary (query c)) 'value) 'count) 1)
  (check 'explicit-settle-control-writes-the-chosen-conflict
    (begin (control:activate! (child (child a 'heading) 'settle))
      (list (store:line document 0) (store:conflicts document))) '("mine" ()))
  (check 'review-tiny-geometry-does-not-create-local-app-buffers
    (let ([before (head:buffers)])
      (for-each (lambda (size) (widget:prepare! root (car size) (cadr size))) '((0 0) (1 1) (4 2)))
      (equal? before (head:buffers))) #t)
  (widget:unmount! root)
  (check 'hidden-review-releases-derived-demand
    (map (lambda (id) (list (model:demanded? (query id)) (model:demanded? (preview id)))) (list a b c)) '((#f #f) (#f #f) (#f #f)))
  (let ([q (query a)] [p (preview a)] [out (output a)])
    (model:retire! actor q (model:revision q))
    (test:await 'review-resource-retirement (lambda () (not (model:snapshot p))))
    (check 'query-retirement-deletes-owned-output-not-original
      (list (store:exists? out) (store:exists? document)) '(#f #t))))
