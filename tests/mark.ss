#!/usr/bin/env scheme-script

;; The mark boundary: invalid input cannot poison a text transaction,
;; and a head only acknowledges publication at the intended revision.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (head head) head:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (core kernel) kernel:)
             (prefix (test) test:))

     (define bot '(agent mark-test))
     (define check test:check)
     (define raises? test:raises?)

     (define id (store:create! bot "mark-validation" '("abcdef" "tail")))
     (store:set-mark! bot id 'point '(0 . 2))
     (check 'invalid-mark-is-refused
            (raises? (lambda () (store:set-mark! bot id 'point '(-1 . 2)))) #t)
     (check 'refused-mark-leaves-old-position (store:mark bot id 'point) '(0 . 2))
     (define (ends span) (list (text:span-start span) (text:span-end span)))
     (define before (call-with-values (lambda () (store:snapshot-state id)) list))
     (for-each
       (lambda (bad)
         (check 'malformed-or-outside-mark-refuses
                (raises? (lambda () (store:set-mark! bot id 'point bad))) #t))
       (list '(0 . -1) '(0.0 . 1) '(2 . 0) '(0 . 7) 'bad
             (text:make-span 0 1 1 5)))
     (check 'mark-errors-leave-text-revision-facts-unchanged
            (call-with-values (lambda () (store:snapshot-state id)) list) before)
     (check 'mark-errors-leave-history-unchanged (store:history id) '())

     ;; Accepted input and read results do not expose mutable stored positions.
     (define supplied (cons 0 2))
     (store:set-mark! bot id 'point supplied)
     (set-car! supplied -1)
     (check 'input-position-is-owned (store:mark bot id 'point) '(0 . 2))
     (set-car! (store:mark bot id 'point) -1)
     (set-cdr! (cdar (store:marks bot id)) -1)
     (check 'read-positions-are-copies (store:mark bot id 'point) '(0 . 2))
     (define supplied-span (text:make-span 0 1 0 4))
     (store:set-mark! bot id 'region supplied-span)
     (set-car! (text:span-start supplied-span) -1)
     (set-cdr! (text:span-end (store:mark bot id 'region)) -1)
     (check 'span-endpoints-are-owned-and-read-as-copies
            (ends (store:mark bot id 'region)) '((0 . 1) (0 . 4)))

     (let ([id (store:create! bot "mark-identity" '("abcdef"))]
           [mark-owner (list 'agent (string-copy "marker"))]
           [mark-name (vector (string-copy "bookmark"))])
       (store:set-mark! mark-owner id mark-name '(0 . 2))
       (string-set! (cadr mark-owner) 0 #\X)
       (string-set! (vector-ref mark-name 0) 0 #\X)
       (string-set! (vector-ref (caar (store:marks '(agent "marker") id)) 0) 0 #\Y)
       (check 'marks-own-actor-and-name-through-admission-and-reads
              (store:mark '(agent "marker") id '#("bookmark")) '(0 . 2))
       (check 'nondata-mark-names-refuse-the-whole-batch
              (raises? (lambda () (store:set-marks! '(agent "marker") id 0
                                                    (list (cons void '(0 . 0))) '(#("bookmark"))))) #t)
       (store:edit! bot id 0 (text:make-span 0 0 0 0) '("X"))
       (check 'owned-mark-identity-survives-edit-and-remains-addressable
              (store:mark '(agent "marker") id '#("bookmark")) '(0 . 3))
       (store:drop-mark! '(agent "marker") id '#("bookmark"))
       (check 'owned-name-can-be-removed (store:marks '(agent "marker") id) '()))

     ;; A batch is actor-owned, atomic, and does not advance text revision.
     (store:set-mark! '(agent other) id 'point '(1 . 2))
     (check 'mark-batch-acknowledges-exact-basis
            (call-with-values
              (lambda () (store:set-marks! bot id 0 '((point . (0 . 3)) (extra . (1 . 1))) '(region)))
              list)
            '(applied 0))
     (check 'batch-updates-point (store:mark bot id 'point) '(0 . 3))
     (check 'batch-removes-region (store:mark bot id 'region) #f)
     (check 'batch-keeps-other-actors (store:mark '(agent other) id 'point) '(1 . 2))
     (define old-marks (store:marks bot id))
     (check 'bad-batch-refuses-before-any-removal
            (raises? (lambda () (store:set-marks! bot id 0 '((point . (0 . 0)) (bad . (9 . 0))) '(extra)))) #t)
     (check 'invalid-batch-preserves-all-marks (store:marks bot id) old-marks)
     (check 'same-name-cannot-be-set-and-dropped
            (raises? (lambda () (store:set-marks! bot id 0 '((point . (0 . 0))) '(point)))) #t)
     (check 'duplicate-mark-names-refuse
            (raises? (lambda () (store:set-marks! bot id 0 '((point . (0 . 0)) (point . (0 . 1))) '()))) #t)
     (check 'future-basis-refuses
            (call-with-values (lambda () (store:set-marks! bot id 1 '() '(point extra))) list)
            '(stale 0))
     (store:edit! bot id 0 (text:make-span 0 0 0 0) '("Q"))
     (check 'valid-edit-after-invalid-mark-succeeds (store:line id 0) "Qabcdef")
     (store:undo! bot id)
     (check 'history-after-invalid-mark-succeeds (store:line id 0) "abcdef")
     (define old-revision (store:revision id))
     (store:reset! bot id '("a"))
     (check 'reset-after-invalid-mark-clamps-valid-mark (store:mark bot id 'point) '(0 . 1))
     (check 'stale-valid-old-position-is-refused-before-current-bounds-check
            (call-with-values
              (lambda () (store:set-marks! bot id old-revision '((point . (1 . 4))) '(extra))) list)
            (list 'stale (store:revision id)))
     (check 'stale-batch-does-not-drop-a-mark (store:mark bot id 'extra) '(0 . 1))

     ;; Pause after head adoption and let a writer commit before publication.
     ;; Reset's normal repaint hook provides the barrier without a test hook.
     (define b (head:window-buffer (head:current-window)))
     (define hid (head:buffer-store-id b))
     (define w (head:current-window))
     (head:store-reset! b '("abcdef"))
     (head:window-pcol-set! w 4)
     (head:buffer-mark-col-set! b 1)
     (head:buffer-marked-set! b #t)
     (define w2 (head:make-window b 0 0 0 0 2 12 80 80 'default))
     (head:set-windows! (list w w2))
     (head:before-frame!)
     (store:reset! bot hid '("abcdef"))
     (define reset-revision (store:revision hid))
     (define adopted (test:gate))
     (define written (test:gate))
     (define armed? #t)
     (define writer
       (test:worker
         (lambda ()
           (test:await 'mark-adoption adopted)
           (dynamic-wind
             (lambda () (void))
             (lambda () (store:edit! bot hid (store:revision hid) (text:make-span 0 0 0 0) '("Q")) 'done)
             (lambda () (written #t))))))
     (dynamic-wind
       (lambda ()
         (head:set-repaint-hook!
           (lambda ()
             (when (and armed? (= (head:buffer-store-rev b) reset-revision))
               (set! armed? #f)
               (adopted #t)
               (test:await 'mark-write written)))))
       (lambda () (head:before-frame!))
       (lambda () (head:set-repaint-hook! (lambda () (void)))))
     (check 'writer-crossed-publication-barrier (writer) 'done)
     (check 'stale-publication-keeps-rebased-point (store:mark head:ui-actor hid 'point) '(0 . 5))
     (check 'stale-publication-keeps-rebased-region
            (ends (store:mark head:ui-actor hid 'region)) '((0 . 2) (0 . 5)))
     (check 'stale-publication-keeps-both-window-points
            (list-sort (lambda (a b) (< (cdr a) (cdr b)))
              (map cdr (filter (lambda (entry) (and (pair? (car entry)) (eq? (caar entry) 'point)))
                               (store:marks head:ui-actor hid))))
            '((0 . 3) (0 . 5)))
     (head:before-frame!)
     (check 'retry-follows-the-new-text-once (head:buffer-lines b) '#("Qabcdef"))
     (check 'retry-keeps-head-and-published-point-in-agreement
            (list (head:window-pcol w) (store:mark head:ui-actor hid 'point)) '(5 (0 . 5)))
     (check 'retry-keeps-head-and-published-region-in-agreement
            (list (head:buffer-mark-col b) (ends (store:mark head:ui-actor hid 'region)))
            '(2 ((0 . 2) (0 . 5))))

     ;; Failed updates/removals remain pending even without new input or
     ;; another text event.  A store outage must not advance the diff cache.
     (head:window-pcol-set! w 6)
     (head:window-pcol-set! w2 1)
     (head:buffer-marked-set! b #f)
     (define store-cell (kernel:persistent-cell 'store (lambda () (error 'mark-test "missing store"))))
     (define saved-store (unbox store-cell))
     (dynamic-wind
       (lambda () (set-box! store-cell #f))
       (lambda () (head:before-frame!))
       (lambda () (set-box! store-cell saved-store)))
     (check 'failed-publication-keeps-old-point (store:mark head:ui-actor hid 'point) '(0 . 5))
     (check 'failed-publication-keeps-old-region
            (ends (store:mark head:ui-actor hid 'region)) '((0 . 2) (0 . 5)))
     (head:before-frame!)
     (check 'unchanged-desired-point-retries-after-failure (store:mark head:ui-actor hid 'point) '(0 . 6))
     (check 'unchanged-desired-removal-retries-after-failure (store:mark head:ui-actor hid 'region) #f)
     (check 'region-removal-includes-window-name
            (exists (lambda (entry) (and (pair? (car entry)) (eq? (caar entry) 'region)))
                    (store:marks head:ui-actor hid)) #f)

     ;; Publication failure is isolated per buffer; other windows progress.
     (define other-buffer (head:new-buffer! "other-window-marks"))
     (head:add-buffer! other-buffer)
     (head:store-reset! other-buffer '("xyz"))
     (define other-id (head:buffer-store-id other-buffer))
     (define w3 (head:make-window other-buffer 0 0 0 0 2 12 80 80 'default))
     (head:set-windows! (list w w2 w3))
     (head:window-pcol-set! w 100)
     (head:before-frame!)
     (check 'invalid-head-position-does-not-replace-old-mark (store:mark head:ui-actor hid 'point) '(0 . 6))
     (check 'other-buffer-publishes-despite-failure
            (map cdr (store:marks head:ui-actor other-id)) '((0 . 2)))
     (head:window-pcol-set! w 1)
     (head:before-frame!)
     (check 'corrected-buffer-retries (store:mark head:ui-actor hid 'point) '(0 . 1))
     (store:set-mark! head:ui-actor other-id 'custom '(0 . 1))
     (head:set-windows! (list w w2))
     (head:before-frame!)
     (check 'window-removal-keeps-unmanaged-actor-marks
            (store:marks head:ui-actor other-id) '((custom . (0 . 1))))

     ;; A new process has no publication diff, but the same named actor
     ;; still owns its old window/selection marks in the daemon.
     (store:set-mark! head:ui-actor hid '(point . 900) '(0 . 0))
     (store:set-mark! head:ui-actor other-id '(point . 901) '(0 . 0))
     (store:set-mark! head:ui-actor other-id 'region (text:make-span 0 0 0 1))
     (store:set-mark! bot other-id 'point '(0 . 2))
     (head:resume!)
     (head:before-frame!)
     (check 'resume-reconciles-abandoned-window-and-region-names-across-buffers
       (list (store:mark head:ui-actor hid '(point . 900))
             (length (store:marks head:ui-actor hid))
             (store:marks head:ui-actor other-id) (store:marks bot other-id))
       '(#f 3 ((custom . (0 . 1))) ((point . (0 . 2)))))

     (test:finish! 'mark)))
