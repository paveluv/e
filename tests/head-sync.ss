#!/usr/bin/env scheme-script

;; Coherent adoption of shared text and positions.  Notifications wake
;; the head; the store snapshot's complete delta chain advances it.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (head) head:)
             (prefix (store) store:)
             (prefix (text) text:))

     (define checks 0)
     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'head-sync-test (symbol->string label) actual expected)))

     (define b (head:window-buffer (head:current)))
     (define id (head:buffer-store-id b))
     (define w (head:current))
     (define bot '(agent sync-test))
     (define (point) (cons (head:window-prow w) (head:window-pcol w)))
     (define (edit! span replacement)
       (store:edit! bot id (store:revision id) span replacement))

     (head:buffer-lines-set! b '#("aaa" "bbb" "ccc"))
     (head:window-prow-set! w 1)
     (head:window-pcol-set! w 2)
     (head:window-top-set! w 1)
     (head:buffer-spot-row-set! b 2)
     (head:buffer-spot-col-set! b 2)
     (head:buffer-spot-top-set! b 2)
     (head:before-frame!)

     (edit! (text:make-span 0 0 0 0) '("new" ""))
     (head:before-frame!)
     (check 'cursor-follows-content (point) '(2 . 2))
     (check 'viewport-follows-content (head:window-top w) 2)
     (check 'saved-position-follows-content
            (cons (head:buffer-spot-row b) (head:buffer-spot-col b)) '(3 . 2))
     (check 'saved-viewport-follows-content (head:buffer-spot-top b) 3)
     (check 'published-point-agrees (store:mark head:ui-actor id 'point) (point))
     (check 'text-and-revision-agree
            (let-values ([(text revision) (store:snapshot id)])
              (and (eq? text (head:buffer-lines b))
                   (= revision (head:buffer-store-rev b))))
            #t)

     ;; A reset cuts history.  Do not apply the queued pre-reset edit or
     ;; a partial post-reset suffix to positions from the old baseline.
     (head:buffer-lines-set! b '#("abcdef"))
     (head:window-prow-set! w 0)
     (head:window-pcol-set! w 2)
     (head:window-top-set! w 0)
     (head:before-frame!)
     (edit! (text:make-span 0 0 0 0) '("X"))
     (store:reset! bot id '("text"))
     (edit! (text:make-span 0 0 0 0) '("YZ"))
     (head:before-frame!)
     (check 'reset-gap-adopts-current-text (head:buffer-lines b) '#("YZtext"))
     (check 'reset-gap-does-not-replay-partial-history (point) '(0 . 2))
     (check 'reset-gap-publishes-clamped-point
            (store:mark head:ui-actor id 'point) '(0 . 2))

     ;; Even a long queued stream is not evidence of a complete chain:
     ;; once the retained history is too short, resync explicitly.
     (do ([i 0 (+ i 1)]) ((= i 257))
       (edit! (text:make-span 0 0 0 0) '("x")))
     (head:before-frame!)
     (check 'truncated-history-does-not-replay-partial-history (point) '(0 . 2))
     (check 'truncated-history-adopts-current-revision
            (head:buffer-store-rev b) (store:revision id))
     (check 'truncated-history-adopts-all-text
            (string-length (vector-ref (head:buffer-lines b) 0)) 263)

     (store:reset! bot id '(""))
     (head:before-frame!)
     (check 'resync-clamps-into-shorter-text (point) '(0 . 0))
     (check 'resync-clamps-saved-viewport (head:buffer-spot-top b) 0)

     (format #t "~a head synchronization checks passed\n" checks)))
