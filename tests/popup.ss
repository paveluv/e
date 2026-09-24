#!/usr/bin/env scheme-script

;; The pop-up, window 0, as a place for other buffers: a buffer sent there
;; shows it, its status line carries one × at the left that clears it, and
;; clear-pop-up! puts the placeholder back and hides the pane while the
;; buffer stays in the list. Headless.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (head literal)
             (prefix (head head) head:)
             (prefix (head paint) paint:)
             (prefix (head window) window:))

     (define check test:check)
     (define path (format "/tmp/e-popup-~a.txt" (get-process-id)))
     (call-with-output-file path (lambda (p) (display "hello\n" p)))
     (define popup (head:popup))
     (define (popup-name) (head:buffer-name (head:window-buffer popup)))
     (define (popup-entry) (find (lambda (e) (eq? (car e) popup)) (paint:window-layout)))
     (head:before-frame!)
     (check 'the-pop-up-starts-hidden-with-its-placeholder (list (head:popup-rows) (popup-name)) '(0 "<pop-up>"))

     (window:link-target! popup)
     (head:with-window popup (visit-file! path))
     (head:before-frame!)
     (define entry (popup-entry))
     (check 'a-buffer-sent-to-the-pop-up-shows-it
       (list (> (head:popup-rows) 0) (> (caddr entry) 0) (popup-name) (equal? (window:linked 'target) (list popup)))
       (list #t #t (let ([s path]) (substring s (+ 1 (let loop ([i (- (string-length s) 1)]) (if (char=? (string-ref s i) #\/) i (loop (- i 1))))) (string-length s))) #t))
     (define status-row (+ (cadr entry) (caddr entry)))
     (define left (head:window-xoff popup))
     (define right (+ left (head:window-width popup)))
     (check 'the-pop-ups-bar-has-one-clear-button-where-the-close-button-would-be
       (list (head:window-button-at (- right 2) status-row) (head:window-button-at (- right 1) status-row)
             (head:window-button-at (- right 4) status-row) (head:window-button-at left status-row))
       (list (cons 'clear popup) #f #f #f))
     (define b (head:window-buffer popup))
     ;; a checkpoint keeps no place of the pop-up's, and resumes while the
     ;; buffer shows there
     (check 'a-checkpoint-keeps-no-place-in-the-pop-up
       (list (exists (lambda (entry) (let ([place (car entry)]) (or (eq? place popup) (and (pair? place) (eq? (cdr place) popup)))))
                     (head:buffer-placements b))
             (begin (head:checkpoint!) (head:resume!)))
       '(#f #t))
     ;; a resize by hand sets the size, which sticks as the most the pane takes
     (define rows (head:popup-rows))
     (head:resize-popup! -1)
     (check 'a-resize-by-hand-sets-the-size-and-its-limit
       (list (head:popup-rows) (head:popup-limit) (begin (head:show-popup! 100) (head:popup-rows)) (begin (head:show-popup! 1) (head:popup-rows)))
       (list (- rows 1) (- rows 1) (- rows 1) 1))
     (head:before-frame!)
     (paint:window-layout) ; the dividers come with a tiling
     (check 'the-boundary-above-the-shown-pop-up-is-a-divider
       (and (exists (lambda (d) (and (eq? (car d) 'below) (eq? (cadr d) (head:root)))) (head:dividers)) #t) #t)
     (window:delete-others!)
     (check 'keeping-one-window-hides-the-pop-up-too (head:popup-rows) 0)
     (head:with-window popup (visit-file! path))
     (window:clear-pop-up!)
     (head:before-frame!)
     (check 'clearing-restores-the-placeholder-and-hides-the-pane
       ;; hidden, the pane has no layout entry to paint or hit
       (list (head:popup-rows) (popup-name) (if (popup-entry) (caddr (popup-entry)) 'hidden) (and (memq b (head:buffers)) #t))
       '(0 "<pop-up>" hidden #t))
     (delete-file path)
     (test:finish! 'popup)))
