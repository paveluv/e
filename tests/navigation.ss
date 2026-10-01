#!/usr/bin/env scheme-script

;; Navigation publishes a complete seat state before repaint callbacks,
;; including composed source/view toggles and reentrant window switches.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(eval
  '(begin
     (import (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (foundation text) text:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:) (prefix (test) test:))

     (define check test:check)
     (define (fresh name)
       (let ([b (head:new-local-buffer! name)])
         (head:buffer-lines-set! b '#("alpha" "middle" "omega"))
         (head:add-buffer! b)
         b))
     (define w (head:current-window))
     (head:window-width-set! w 80)
     (head:window-size-set! w 12)
     (define a (fresh "navigation-a"))
     (define b (fresh "navigation-b"))
     (define c (fresh "navigation-c"))
     (define (position) (head:buffer-point (head:current-buffer-mirror)))
     (define observations '())
     ;; The first edit command invokes (edit) and, through it, the painter,
     ;; whose initialization installs its own repaint hook; the test's goes after.
     (head:show-buffer-mirror! a)
     (head:goto! '(0 . 0))
     (head:set-repaint-hook!
       (lambda ()
         (set! observations
           (cons (list (head:current-buffer-mirror) (position) (head:window-top w)
                       (head:window-topseg w) (head:window-left w)) observations))))
     (head:goto! '(1 . 3))
     (head:window-top-set! w 1)
     (head:window-topseg-set! w 2)
     (head:window-left-set! w 3)
     (set! observations '())
     (head:show-buffer-mirror! a)
     (check 'redisplay-preserves-live-window
            (list (position) (head:window-top w) (head:window-topseg w) (head:window-left w))
            '((1 . 3) 1 2 3))
     (check 'redisplay-needs-no-notification observations '())
     (head:buffer-spot-row-set! b 2)
     (head:buffer-spot-col-set! b 4)
     (head:buffer-spot-top-set! b 1)
     (check 'hidden-point-read (head:buffer-point b) '(2 . 4))
     (check 'point-read-does-not-switch (eq? (head:current-buffer-mirror) a) #t)
     (check 'point-read-does-not-notify observations '())
     (head:show-buffer-mirror! b)
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
                    (head:show-buffer-mirror! a)
                    (head:call-with-display-update (lambda () (head:show-buffer-mirror! b)))
                    (head:goto! '(1 . 2))
                    (values 'one 'two)))) list)
            '(one two))
     (check 'nested-scope-publishes-final-state observations (list (list b '(1 . 2) 1 0 0)))
     (set! observations '())
     (head:call-with-display-update void)
     (check 'empty-scope-does-not-notify observations '())
     (guard (ex [else (void)])
       (head:call-with-display-update
         (lambda () (head:show-buffer-mirror! a) (head:goto! '(2 . 1)) (error 'probe "stop"))))
     (check 'exception-flushes-completed-state observations (list (list a '(2 . 1) 1 0 0)))
     (set! observations '())
     (call/cc
       (lambda (escape)
         (head:call-with-display-update
           (lambda () (head:show-buffer-mirror! b) (head:goto! '(0 . 1)) (escape 'done)))))
     (check 'escape-flushes-completed-state observations (list (list b '(0 . 1) 1 0 0)))

     ;; A callback starts a fresh scope, and no outer transition can
     ;; overwrite the window chosen there when it returns.
     (define once #t)
     (head:set-repaint-hook!
       (lambda ()
         (when once
           (set! once #f)
           (check 'callback-sees-requested-buffer (eq? (head:current-buffer-mirror) a) #t)
           (head:call-with-display-update (lambda () (head:show-buffer-mirror! c) (head:goto! '(2 . 2)))))))
     (head:show-buffer-mirror! a)
     (check 'reentrant-switch-survives (eq? (head:current-buffer-mirror) c) #t)
     (check 'reentrant-point-survives (position) '(2 . 2))
     (head:set-repaint-hook! (lambda () (error 'callback "failure")))
     (check 'callback-error-propagates
            (guard (ex [else #t]) (head:call-with-display-update (lambda () (head:show-buffer-mirror! a))) #f) #t)
     (set! observations '())
     (head:set-repaint-hook! (lambda () (set! observations (cons (head:current-buffer-mirror) observations))))
     (head:show-buffer-mirror! b)
     (check 'callback-failure-does-not-poison-next-update observations (list b))

     (head:set-repaint-hook! (lambda () (paint:invalidate-screen-cache!)))
     ;; Ordinary source positions remain characters; a vertical goal is in
     ;; cells, survives short rows and wide-glyph interiors, and is also the
     ;; column used by paging into rows outside the current rendition demand.
     (head:buffer-lines-set! a '#("界ab" "a\x301;bcde" "x" "界ab"))
     (head:show-buffer-mirror! a)
     (head:window-wrap-set! w #f)
     (head:goto! '(0 . 1))
     (define (steps action arguments)
       (reverse (fold-left (lambda (out argument) (cons (action argument) out)) '() arguments)))
     (check 'vertical-goal-survives-short-rows-and-character-counts
       (steps (lambda (delta) (move-vertical! delta) (head:point)) '(1 1 1 -1 -1 -1))
       '((1 . 3) (2 . 1) (3 . 1) (2 . 1) (1 . 3) (0 . 1)))
     (head:buffer-lines-set! a '#("ab界e\x301;xy"))
     (head:window-width-set! w 4)
     (head:window-wrap-set! w #t)
     (head:goto! '(0 . 1))
     (check 'wrapped-goal-snaps-to-whole-glyph-and-recovers
       (steps (lambda (delta) (move-vertical! delta) (head:point)) '(1 1 -1 -1))
       '((0 . 2) (0 . 6) (0 . 2) (0 . 1)))
     (head:buffer-lines-set! a
       (list->vector (map (lambda (i) (if (even? i) "abcd" "界abc")) (iota 30))))
     (head:window-wrap-set! w #f)
     (paint:set-screen-rows! 7)
     (paint:set-screen-cols! 12)
     (head:window-top-set! w 0)
     (head:goto! '(0 . 2))
     (check 'paging-keeps-cell-column-outside-the-demanded-rows
       (steps (lambda (direction)
                (page! direction 1)
                (let ([p (head:point)])
                  (list (car p) (cdr p)
                    (cdr (paint:window-screen-position w (car p) (cdr p))))))
              '(1 1 -1))
       '((7 1 3) (12 2 3) (7 1 3)))
     (test:finish! 'navigation)))
