#!/usr/bin/env scheme-script

;; Navigation publishes a complete seat state before repaint callbacks,
;; including composed source/view toggles and reentrant window switches.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)
(eval
  '(begin
     (import (except (head edit) init!)
             (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (foundation text) text:)
             (prefix (head mode) mode:)
             (prefix (head paint) paint:) (prefix (head tui) tui:) (prefix (test) test:))

     (define check test:check)
     (define (fresh name)
       (let ([b (seat:new-local-buffer! name)])
         (seat:buffer-lines-set! b '#("alpha" "middle" "omega"))
         (seat:add-buffer! b)
         b))
     (define w (seat:current-window))
     (seat:window-width-set! w 80)
     (seat:window-size-set! w 12)
     (define a (fresh "navigation-a"))
     (define b (fresh "navigation-b"))
     (define c (fresh "navigation-c"))
     (define (position) (seat:buffer-point (seat:current-buffer-mirror)))
     (define observations '())
     ;; The first edit command invokes (edit) and, through it, the painter,
     ;; whose initialization installs its own repaint hook; the test's goes after.
     (seat:show-buffer-mirror! a)
     (seat:goto! '(0 . 0))
     (seat:set-repaint-hook!
       (lambda ()
         (set! observations
           (cons (list (seat:current-buffer-mirror) (position) (seat:window-top w)
                       (seat:window-topseg w) (seat:window-left w)) observations))))
     (seat:goto! '(1 . 3))
     (seat:window-top-set! w 1)
     (seat:window-topseg-set! w 2)
     (seat:window-left-set! w 3)
     (set! observations '())
     (seat:show-buffer-mirror! a)
     (check 'redisplay-preserves-live-window
            (list (position) (seat:window-top w) (seat:window-topseg w) (seat:window-left w))
            '((1 . 3) 1 2 3))
     (check 'redisplay-needs-no-notification observations '())
     (seat:buffer-spot-row-set! b 2)
     (seat:buffer-spot-col-set! b 4)
     (seat:buffer-spot-top-set! b 1)
     (check 'hidden-point-read (seat:buffer-point b) '(2 . 4))
     (check 'point-read-does-not-switch (eq? (seat:current-buffer-mirror) a) #t)
     (check 'point-read-does-not-notify observations '())
     (seat:show-buffer-mirror! b)
     (check 'callback-observes-complete-window
            observations (list (list b '(2 . 4) 1 0 0)))
     (check 'old-point-is-saved (seat:buffer-point a) '(1 . 3))

     ;; Nested scopes produce one notification after every placement.
     (set! observations '())
     (check 'display-scope-preserves-multiple-values
            (call-with-values
              (lambda ()
                (seat:call-with-display-update
                  (lambda ()
                    (seat:show-buffer-mirror! a)
                    (seat:call-with-display-update (lambda () (seat:show-buffer-mirror! b)))
                    (seat:goto! '(1 . 2))
                    (values 'one 'two)))) list)
            '(one two))
     (check 'nested-scope-publishes-final-state observations (list (list b '(1 . 2) 1 0 0)))
     (set! observations '())
     (seat:call-with-display-update void)
     (check 'empty-scope-does-not-notify observations '())
     (guard (ex [else (void)])
       (seat:call-with-display-update
         (lambda () (seat:show-buffer-mirror! a) (seat:goto! '(2 . 1)) (error 'probe "stop"))))
     (check 'exception-flushes-completed-state observations (list (list a '(2 . 1) 1 0 0)))
     (set! observations '())
     (call/cc
       (lambda (escape)
         (seat:call-with-display-update
           (lambda () (seat:show-buffer-mirror! b) (seat:goto! '(0 . 1)) (escape 'done)))))
     (check 'escape-flushes-completed-state observations (list (list b '(0 . 1) 1 0 0)))

     ;; A callback starts a fresh scope, and no outer transition can
     ;; overwrite the window chosen there when it returns.
     (define once #t)
     (seat:set-repaint-hook!
       (lambda ()
         (when once
           (set! once #f)
           (check 'callback-sees-requested-buffer (eq? (seat:current-buffer-mirror) a) #t)
           (seat:call-with-display-update (lambda () (seat:show-buffer-mirror! c) (seat:goto! '(2 . 2)))))))
     (seat:show-buffer-mirror! a)
     (check 'reentrant-switch-survives (eq? (seat:current-buffer-mirror) c) #t)
     (check 'reentrant-point-survives (position) '(2 . 2))
     (seat:set-repaint-hook! (lambda () (error 'callback "failure")))
     (check 'callback-error-propagates
            (guard (ex [else #t]) (seat:call-with-display-update (lambda () (seat:show-buffer-mirror! a))) #f) #t)
     (set! observations '())
     (seat:set-repaint-hook! (lambda () (set! observations (cons (seat:current-buffer-mirror) observations))))
     (seat:show-buffer-mirror! b)
     (check 'callback-failure-does-not-poison-next-update observations (list b))

     (seat:set-repaint-hook! (lambda () (tui:invalidate-screen-cache!)))
     ;; Ordinary source positions remain characters; a vertical goal is in
     ;; cells, survives short rows and wide-glyph interiors, and is also the
     ;; column used by paging into rows outside the current rendition demand.
     (seat:buffer-lines-set! a '#("界ab" "a\x301;bcde" "x" "界ab"))
     (seat:show-buffer-mirror! a)
     (seat:window-wrap-set! w #f)
     (seat:goto! '(0 . 1))
     (define (steps action arguments)
       (reverse (fold-left (lambda (out argument) (cons (action argument) out)) '() arguments)))
     (check 'vertical-goal-survives-short-rows-and-character-counts
       (steps (lambda (delta) (move-vertical! delta) (seat:point)) '(1 1 1 -1 -1 -1))
       '((1 . 3) (2 . 1) (3 . 1) (2 . 1) (1 . 3) (0 . 1)))
     (seat:buffer-lines-set! a '#("ab界e\x301;xy"))
     (seat:window-width-set! w 4)
     (seat:window-wrap-set! w #t)
     (seat:goto! '(0 . 1))
     (check 'wrapped-goal-snaps-to-whole-glyph-and-recovers
       (steps (lambda (delta) (move-vertical! delta) (seat:point)) '(1 1 -1 -1))
       '((0 . 2) (0 . 6) (0 . 2) (0 . 1)))
     (seat:buffer-lines-set! a
       (list->vector (map (lambda (i) (if (even? i) "abcd" "界abc")) (iota 30))))
     (seat:window-wrap-set! w #f)
     (tui:set-screen-rows! 7)
     (tui:set-screen-cols! 12)
     (seat:window-top-set! w 0)
     (seat:goto! '(0 . 2))
     (check 'paging-keeps-cell-column-outside-the-demanded-rows
       (steps (lambda (direction)
                (page! direction 1)
                (let ([p (seat:point)])
                  (list (car p) (cdr p)
                    (cdr (paint:window-screen-position w (car p) (cdr p))))))
              '(1 1 -1))
       '((7 1 3) (12 2 3) (7 1 3)))
     (test:finish! 'navigation)))
