#!/usr/bin/env scheme-script

;; Navigation publishes a complete seat state before repaint callbacks,
;; including composed source/view toggles and reentrant window switches.
(import (chezscheme))
(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)
(eval
  '(begin
     (import (except (edit) init!)
             (prefix (head) head:)
             (prefix (text) text:)
             (prefix (mode) mode:)
             (prefix (md-mode) md-mode:)
             (prefix (markdown) markdown:)
             (prefix (paint) paint:))

     (define checks 0)
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'navigation-test (symbol->string label) actual expected)))
     (define (fresh name)
       (let ([b (head:new-local-buffer name)])
         (head:buffer-lines-set! b '#("alpha" "middle" "omega"))
         (head:add-buffer! b)
         b))
     (define w (head:current))
     (head:window-width-set! w 80)
     (head:window-size-set! w 12)
     (define a (fresh "navigation-a"))
     (define b (fresh "navigation-b"))
     (define c (fresh "navigation-c"))
     (define (position) (head:buffer-point (current-buffer)))
     (define observations '())
     (head:set-repaint-hook!
       (lambda ()
         (set! observations
           (cons (list (current-buffer) (position) (head:window-top w)
                       (head:window-topseg w) (head:window-left w)) observations))))
     (show-buffer! a)
     (goto-point! '(1 . 3))
     (head:window-top-set! w 1)
     (head:window-topseg-set! w 2)
     (head:window-left-set! w 3)
     (set! observations '())
     (show-buffer! a)
     (check 'redisplay-preserves-live-window
            (list (position) (head:window-top w) (head:window-topseg w) (head:window-left w))
            '((1 . 3) 1 2 3))
     (check 'redisplay-needs-no-notification observations '())
     (head:buffer-spot-row-set! b 2)
     (head:buffer-spot-col-set! b 4)
     (head:buffer-spot-top-set! b 1)
     (check 'hidden-point-read (head:buffer-point b) '(2 . 4))
     (check 'point-read-does-not-switch (eq? (current-buffer) a) #t)
     (check 'point-read-does-not-notify observations '())
     (show-buffer! b)
     (check 'callback-observes-complete-window
            observations (list (list b '(2 . 4) 1 0 0)))
     (check 'old-point-is-saved (head:buffer-point a) '(1 . 3))

     ;; Nested scopes produce one notification after every placement.
     (set! observations '())
     (check 'display-scope-preserves-multiple-values
            (call-with-values
              (lambda ()
                (head:call-with-display-update
                  (lambda ()
                    (show-buffer! a)
                    (head:call-with-display-update (lambda () (show-buffer! b)))
                    (goto-point! '(1 . 2))
                    (values 'one 'two)))) list)
            '(one two))
     (check 'nested-scope-publishes-final-state observations (list (list b '(1 . 2) 1 0 0)))
     (set! observations '())
     (head:call-with-display-update void)
     (check 'empty-scope-does-not-notify observations '())
     (guard (ex [else (void)])
       (head:call-with-display-update
         (lambda () (show-buffer! a) (goto-point! '(2 . 1)) (error 'probe "stop"))))
     (check 'exception-flushes-completed-state observations (list (list a '(2 . 1) 1 0 0)))
     (set! observations '())
     (call/cc
       (lambda (escape)
         (head:call-with-display-update
           (lambda () (show-buffer! b) (goto-point! '(0 . 1)) (escape 'done)))))
     (check 'escape-flushes-completed-state observations (list (list b '(0 . 1) 1 0 0)))

     ;; A callback starts a fresh scope, and no outer transition can
     ;; overwrite the window chosen there when it returns.
     (define once #t)
     (head:set-repaint-hook!
       (lambda ()
         (when once
           (set! once #f)
           (check 'callback-sees-requested-buffer (eq? (current-buffer) a) #t)
           (head:call-with-display-update (lambda () (show-buffer! c) (goto-point! '(2 . 2)))))))
     (show-buffer! a)
     (check 'reentrant-switch-survives (eq? (current-buffer) c) #t)
     (check 'reentrant-point-survives (position) '(2 . 2))
     (head:set-repaint-hook! (lambda () (error 'callback "failure")))
     (check 'callback-error-propagates
            (guard (ex [else #t]) (head:call-with-display-update (lambda () (show-buffer! a))) #f) #t)
     (set! observations '())
     (head:set-repaint-hook! (lambda () (set! observations (cons (current-buffer) observations))))
     (show-buffer! b)
     (check 'callback-failure-does-not-poison-next-update observations (list b))

     (head:set-repaint-hook! (lambda () (paint:invalidate-screen-cache!)))
     (md-mode:init!)
     (markdown:init!)
     (for-each
       (lambda (local?)
         (let ([source ((if local? head:new-local-buffer head:new-buffer) "navigation.md")])
           (head:buffer-lines-set! source '#("# Alpha" "" "# Middle" "" "# Omega"))
           (mode:choose! source "markdown")
           (show-buffer! source)
           (goto-point! '(2 . 1))
           (let ([entered #f] [once #t])
             (head:set-repaint-hook!
               (lambda ()
                 (paint:invalidate-screen-cache!)
                 (when once
                   (set! once #f)
                   (set! entered (list (head:buffer-store-id (current-buffer))
                                       (buffer-line (current-buffer) (car (point)))))
                   (head:store-edit! source (text:make-span 0 0 0 0) '("# Before" "" ""))
                   (head:before-frame!))))
             (markdown:view!)
             (check 'toggle-callback-sees-ready-view entered '(#f "Middle"))
             (check 'source-to-view-follows-callback-edit (point) '(4 . 0))
             (check 'source-to-view-keeps-content (buffer-line (current-buffer) (car (point))) "Middle"))
           (let ([entered #f] [once #t])
             (head:set-repaint-hook!
               (lambda ()
                 (paint:invalidate-screen-cache!)
                 (when once
                   (set! once #f)
                   (set! entered (list (eq? (current-buffer) source) (point)))
                   (head:store-edit! source (text:make-span 0 0 0 0) '("# Later" "" ""))
                   (head:before-frame!))))
             (markdown:edit!)
             (check 'toggle-callback-sees-ready-source entered '(#t (4 . 0)))
             (check 'view-to-source-follows-callback-edit (point) '(6 . 0))
             (check 'view-to-source-keeps-content (buffer-line source (car (point))) "# Middle"))
           (let ([once #t])
             (head:set-repaint-hook!
               (lambda ()
                 (when once (set! once #f) (show-buffer! c) (goto-point! '(1 . 2)))))
             (markdown:view!)
             (check 'toggle-keeps-callback-navigation (eq? (current-buffer) c) #t)
             (check 'toggle-keeps-callback-position (point) '(1 . 2)))
           (head:set-repaint-hook! (lambda () (paint:invalidate-screen-cache!)))))
       '(#f #t))
     (format #t "~a navigation checks passed\n" checks)))
