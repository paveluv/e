#!/usr/bin/env scheme-script

;; The mark boundary: invalid input cannot poison a text transaction,
;; and a head only acknowledges publication at the intended revision.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (state store) store:)
             (prefix (foundation text) text:)
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

     (test:finish! 'mark)))
