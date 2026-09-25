#!/usr/bin/env scheme-script

;; The scope forms: with-buffer, with-window and with-region make another
;; buffer, window or region current for a body, invisibly, and the
;; current-context commands act on what is current.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (core kernel) kernel:)
             (prefix (head head) head:)
             (prefix (apps search) search:)
             (prefix (head window) window:))

     (define check test:check)
     (define (fresh name lines)
       (let ([b (head:new-buffer! name)])
         (head:buffer-lines-set! b (list->vector lines))
         b))
     (define (text-of b) (vector->list (head:buffer-lines b)))

     (define a (fresh "scope-a" '("x one x" "two x")))
     (define b (fresh "scope-b" '("x three" "x x")))
     (head:show-buffer! a)
     (head:goto! '(0 . 0))

     ;; with-buffer: the buffer is current inside, the old one returns after,
     ;; the recency order is untouched, and an escape restores too
     (define before (head:buffers))
     (check 'with-buffer-makes-the-buffer-current
       (list (head:with-buffer b (head:current-buffer)) (head:current-buffer) (equal? (head:buffers) before))
       (list b a #t))
     (check 'with-buffer-restores-on-an-escape
       (begin (guard (ex [else #f]) (head:with-buffer b (error 'scope "out"))) (head:current-buffer))
       a)

     ;; the current region: the whole buffer without a mark, the selection with one
     (check 'current-region-is-the-whole-buffer-without-a-mark
       (let ([r (current-region)]) (list (region-buffer r) (region-start r) (region-end r)))
       (list a '(0 . 0) '(1 . 5)))
     (check 'count-matches-counts-the-current-buffer (search:count "x") 3)
     (check 'with-buffer-retargets-a-current-context-query (head:with-buffer b (search:count "x")) 3)

     ;; with-region selects the region, the commands stay inside it, and the
     ;; previous selection and point return
     (define r (region b '(1 . 0) '(1 . 3)))
     (check 'with-region-selects-the-region
       (with-region r (list (head:current-buffer) (head:mark) (head:point)))
       (list b '(1 . 0) '(1 . 3)))
     (check 'replace-stays-inside-the-region (with-region r (search:replace! "x" "y")) 2)
     (check 'the-rest-of-the-buffer-is-untouched (text-of b) '("x three" "y y"))
     (check 'the-selection-and-point-return (list (head:current-buffer) (head:mark) (head:point)) (list a #f '(0 . 0)))
     (check 'count-matches-under-with-region (with-region (region a '(0 . 0) '(0 . 3)) (search:count "x")) 1)

     ;; with-window selects a window for the body only
     (window:split-below!)
     (define here (head:current-window))
     (define other (find (lambda (w) (and (not (eq? w here)) (not (head:popup? w)))) (head:windows)))
     (check 'with-window-selects-the-window
       (list (head:with-window other (head:current-window)) (head:current-window))
       (list other here))
     (head:with-window other (head:show-buffer! b))
     (check 'a-command-under-with-window-acts-there
       (list (head:window-buffer other) (head:window-buffer here) (head:current-window))
       (list b a here))
     (check 'with-window-wants-a-live-window
       (guard (ex [else 'refused]) (head:with-window 'nowhere (head:current-window)))
       'refused)

     ;; a buffer goes by name and a window by index as well as by literal,
     ;; in the scope forms and the commands alike, as completion writes them
     (check 'scope-forms-take-a-name-or-an-index
       (list (head:with-buffer "scope-b" (head:current-buffer))
             (head:with-window (head:window-index other) (head:current-window))
             (equal? (buffer-text "scope-b") (buffer-text b)) (eq? (buffer-clean? "scope-b") (buffer-clean? b)))
       (list b other #t #t))
     (check 'window-commands-take-a-name-or-an-index
       (list (head:window-buffer (window:display! "scope-b")) (window:focus! (head:window-index other)) (head:current-window)
             (begin (window:focus! here) (head:current-window)))
       (list b #t other here))

     ;; exact arities: no optional scope or setting
     (check 'undo-takes-no-scope (guard (ex [else 'refused]) (undo! 'all)) 'refused)
     (window:set-wrap! #f)
     (check 'set-wrap-sets-the-window (head:window-wrap here) #f)
     (window:toggle-wrap!)
     (check 'toggle-wrap-flips-it (head:window-wrap here) #t)
     (window:set-wrap! 'default)
     (check 'set-wrap-takes-default (head:window-wrap here) 'default)
     (check 'set-wrap-refuses-a-buffer-setting (guard (ex [else 'refused]) (window:set-wrap! 'clean)) 'refused)

     ;; line numbers are the window's too, beside an edit buffer; an app's
     ;; buffer shows itself and refuses the window toggles
     (window:set-line-numbers! #t)
     (check 'set-line-numbers-sets-the-window
       (list (head:window-line-numbers here) (head:window-line-numbers? here) (head:window-line-numbers? other))
       '(#t #t #f))
     (window:toggle-line-numbers!)
     (check 'toggle-line-numbers-flips-it (head:window-line-numbers here) #f)
     (window:set-line-numbers! 'default)
     (define app (head:register-app! "scope-app" void))
     (head:show-buffer! app)
     (check 'an-app-buffer-refuses-the-window-toggles
       (list (guard (ex [(kernel:refusal? ex) 'refused]) (window:toggle-wrap!))
             (guard (ex [(kernel:refusal? ex) 'refused]) (window:set-line-numbers! #t))
             (head:window-line-numbers? here) (head:window-line-numbers here))
       '(refused refused #f default))

     (check 'kill-buffer-takes-a-name (begin (kill-buffer! "scope-b") (and (memq b (head:buffers)) #t)) #f)

     (test:finish! 'scope)))
