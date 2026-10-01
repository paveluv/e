#!/usr/bin/env scheme-script

;; The scope forms: with-buffer, with-window and with-region make another
;; buffer, window or region current for a body, invisibly, and the
;; current-context commands act on what is current.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (core kernel) kernel:)
             (prefix (core region) region:) (prefix (state store) store:)
             (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (apps search) search:)
             (prefix (head window) window:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:))

     (define check test:check)
     (widget:init!) (edit:init!) (window:init!)
     (window 1)
     (define (fresh name lines)
       (let ([b (seat:new-buffer! name)])
         (seat:buffer-lines-set! b (list->vector lines))
         b))
     (define (text-of b) (vector->list (seat:buffer-lines b)))

     (define a (fresh "scope-a" '("x one x" "two x")))
     (define b (fresh "scope-b" '("x three" "x x")))
     (define aid (seat:buffer-store-id a))
     (define bid (seat:buffer-store-id b))
     (seat:show-buffer-mirror! a)
     (seat:goto! '(0 . 0))

     (check 'buffer-command-boundaries-reject-names-records-and-other-handles
       (map (lambda (bad)
              (list (test:raises? (lambda () (seat:show-buffer! bad)))
                (test:raises? (lambda () (seat:with-buffer bad 'ran)))
                (test:raises? (lambda () (buffer-text bad)))
                (test:raises? (lambda () (kill-buffer! bad)))))
         (list "scope-b" b '(model 1) #f '(buffer 0)))
       '((#t #t #t #t) (#t #t #t #t) (#t #t #t #t) (#t #t #t #t) (#t #t #t #t)))

     ;; with-buffer: the buffer is current inside, the old one returns after,
     ;; the recency order is untouched, and an escape restores too
     (define before (seat:buffers))
     (check 'with-buffer-makes-the-buffer-current
       (list (seat:with-buffer (store:find-named "scope-b") (seat:current-buffer-mirror)) (seat:current-buffer-mirror) (equal? (seat:buffers) before))
       (list b a #t))
     (check 'with-buffer-restores-on-an-escape
       (begin (guard (ex [else #f]) (seat:with-buffer bid (error 'scope "out"))) (seat:current-buffer-mirror))
       a)

     ;; the current region: the whole buffer without a mark, the selection with one
     (check 'current-region-is-the-whole-buffer-without-a-mark
       (current-region) (list 'region aid '(0 . 0) '(1 . 5)))
     (check 'region-shape-does-not-resolve-a-document
       (map region:valid?
         '((region (buffer 999999) (0 . 0) (1 . 2))
           (region (buffer 1) (0 . 0) (999999999999999999999 . 0))
           (region (buffer 1) (1 . 0) (0 . 0)) (region (model 1) (0 . 0) (0 . 0))
           (region (buffer 1) (-1 . 0) (0 . 0)) (region (buffer 1) (0 . 0) (0 . 1.0))
           (region (buffer 1) (0 0) (1 . 0)) (region (buffer 1) (0 . 0))))
       '(#t #t #f #f #f #f #f #f))
     (check 'region-construction-orders-and-owns-its-data
       (let* ([id (list 'buffer (cadr aid))] [p (cons 1 2)] [q (cons 0 3)]
              [r (region:make id p q)])
         (set-car! (cdr id) 999999) (set-car! p 99) (set-cdr! q 99)
         (list r (region:buffer r) (region:start r) (region:end r)
           (test:raises? (lambda () (region:make a '(0 . 0) '(0 . 0))))))
       (list (list 'region aid '(0 . 3) '(1 . 2)) aid '(0 . 3) '(1 . 2) #t))
     (check 'region-extraction-uses-exact-character-endpoints
       (map region-text (list (region:make aid '(0 . 2) '(1 . 3)) (region:make aid '(1 . 5) '(1 . 5))))
       '("one x\ntwo" ""))
     (check 'count-matches-counts-the-current-buffer (search:count "x") 3)
     (check 'with-buffer-retargets-a-current-context-query (seat:with-buffer bid (search:count "x")) 3)

     ;; with-region selects the region, the commands stay inside it, and the
     ;; previous selection and point return
     (define r (region:make bid '(1 . 0) '(1 . 3)))
     (check 'with-region-selects-the-region
       (with-region r (list (seat:current-buffer-mirror) (seat:mark) (seat:point)))
       (list b '(1 . 0) '(1 . 3)))
     (check 'replace-stays-inside-the-region (with-region r (search:replace! "x" "y")) 2)
     (check 'the-rest-of-the-buffer-is-untouched (text-of b) '("x three" "y y"))
     (check 'the-selection-and-point-return (list (seat:current-buffer-mirror) (seat:mark) (seat:point)) (list a #f '(0 . 0)))
     (check 'count-matches-under-with-region (with-region (region:make aid '(0 . 0) '(0 . 3)) (search:count "x")) 1)
     (check 'region-refusals-and-escapes-preserve-selection-and-text
       (list
         (map (lambda (bad)
                (list (test:raises? (lambda () (region-text bad)))
                  (test:raises? (lambda () (with-region bad 'body-ran)))))
           (list (region:make bid '(0 . 0) '(0 . 99)) (region:make bid '(0 . 0) '(2 . 0))
             (list 'region bid '(1 . 3) '(0 . 0))))
         (test:raises? (lambda () (with-region r (error 'scope "escape"))))
         (list (seat:current-buffer-mirror) (seat:mark) (seat:point))
         (seat:with-buffer bid (list (seat:mark) (seat:point))) (text-of b))
       (list '((#t #t) (#t #t) (#t #t)) #t (list a #f '(0 . 0)) '(#f (0 . 0)) '("x three" "y y")))
     ;; A store-only document needs no head record to read, and no name
     ;; lookup can redirect a retained region after a rename or deletion.
     (define unseen (store:create! head:ui-actor "scope-unseen" '("abc")))
     (define saved (region:make unseen '(0 . 0) '(0 . 3)))
     (check 'buffer-queries-do-not-adopt-a-head-mirror
       (list (buffer-text unseen) (buffer-clean? unseen) (seat:buffer-of-store-id unseen))
       '("abc" #f #f))
     (check 'region-resolves-identity-instead-of-name-or-head-record
       (list (seat:buffer-of-store-id unseen) (region-text saved)
         (begin (store:rename! head:ui-actor unseen "scope-renamed") (region-text saved))
         (with-region saved (equal? (current-region) saved))
         (begin
           (store:delete! head:ui-actor unseen)
           (store:create! head:ui-actor "scope-renamed" '("replacement"))
           (list (test:raises? (lambda () (region-text saved)))
             (test:raises? (lambda () (with-region saved #t))))))
       '(#f "abc" "abc" #t (#t #t)))

     ;; with-window selects a window for the body only
     (window:split-below!)
     (define here (seat:current-window))
     (define other (find (lambda (w) (and (not (eq? w here)) (not (seat:popup? w)))) (seat:windows)))
     (check 'with-window-selects-the-window
       (list (seat:with-window other (seat:current-window)) (seat:current-window))
       (list other here))
     (seat:with-window other (seat:show-buffer-mirror! b))
     (check 'a-command-under-with-window-acts-there
       (list (seat:window-buffer other) (seat:window-buffer here) (seat:current-window))
       (list b a here))
     (check 'with-window-wants-a-live-window
       (guard (ex [else 'refused]) (seat:with-window 'nowhere (seat:current-window)))
       'refused)

     ;; Buffer names require explicit lookup. Window indices remain selectors
     ;; until model-backed windows replace the legacy host.
     (check 'scope-forms-take-explicit-buffer-lookups-and-window-indices
       (list (seat:with-buffer (store:find-named "scope-b") (seat:current-buffer-mirror))
             (seat:with-window (seat:window-index other) (seat:current-window))
             (equal? (buffer-text (store:find-named "scope-b")) (buffer-text bid)) (eq? (buffer-clean? (store:find-named "scope-b")) (buffer-clean? bid)))
       (list b other #t #t))
     (check 'window-commands-take-buffer-references-and-window-indices
       (list (seat:window-buffer (window:display! (store:find-named "scope-b"))) (window:focus! (seat:window-index other)) (seat:current-window)
             (begin (window:focus! here) (seat:current-window)))
       (list b #t other here))

     ;; exact arities: no optional scope or setting
     (check 'undo-takes-no-scope (guard (ex [else 'refused]) (undo! 'all)) 'refused)
     (window:set-wrap! #f)
     (check 'set-wrap-sets-the-window (seat:window-wrap here) #f)
     (window:toggle-wrap!)
     (check 'toggle-wrap-flips-it (seat:window-wrap here) #t)
     (window:set-wrap! 'default)
     (check 'set-wrap-takes-default (seat:window-wrap here) 'default)
     (check 'set-wrap-refuses-a-buffer-setting (guard (ex [else 'refused]) (window:set-wrap! 'clean)) 'refused)

     ;; line numbers are the window's too, beside an edit buffer; an app's
     ;; buffer shows itself and refuses the window toggles
     (window:set-line-numbers! #t)
     (check 'set-line-numbers-sets-the-window
       (list (seat:window-line-numbers here) (seat:window-line-numbers? here) (seat:window-line-numbers? other))
       '(#t #t #f))
     (window:toggle-line-numbers!)
     (check 'toggle-line-numbers-flips-it (seat:window-line-numbers here) #f)
     (window:set-line-numbers! 'default)
     (define app (seat:register-widget-host! (seat:new-local-buffer! "scope-app") void void))
     (seat:show-buffer-mirror! app)
     (check 'local-host-has-no-buffer-identity (seat:current-buffer) #f)
     (check 'local-apps-have-no-document-region
       (guard (ex [(kernel:refusal? ex) 'refused]) (current-region)) 'refused)
     (check 'an-app-buffer-refuses-the-window-toggles
       (list (guard (ex [(kernel:refusal? ex) 'refused]) (window:toggle-wrap!))
             (guard (ex [(kernel:refusal? ex) 'refused]) (window:set-line-numbers! #t))
             (seat:window-line-numbers? here) (seat:window-line-numbers here))
       '(refused refused #f default))

     (check 'kill-buffer-takes-a-reference (begin (kill-buffer! bid) (and (memq b (seat:buffers)) #t)) #f)

     (check 'buffer-selection-is-owned-and-references-survive-rename
       (let* ([id (new-buffer! "scope-created")] [copy (seat:current-buffer)])
         (set-car! (cdr copy) 999999)
         (store:rename! head:ui-actor id "scope-renamed-again")
         (let ([same (equal? (seat:current-buffer) id)])
           (kill-buffer! id)
           (list same (equal? (restore! "scope-renamed-again") id)
             (equal? (seat:current-buffer) id)))) '(#t #t #t))

     (check 'unseen-buffer-can-be-trashed-without-adoption
       (let ([id (store:create! head:ui-actor "scope-unseen-trash" '("kept"))])
         (kill-buffer! id)
         (list (seat:buffer-of-store-id id) (store:exists? id)
           (store:visible? head:ui-actor id) (buffer-text id))) '(#f #t #f "kept"))

     (test:finish! 'scope)))
