#!/usr/bin/env scheme-script

;; A saved store read back into a fresh one: the journal's entries, undo
;; groups and pending conflicts return as the live records they were, so
;; the conflicts count, a resolution and undo work after a base restart.
;; The state is one a reload with a conflict exported.  Run from the
;; repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (state store) store:) (prefix (test) test:))

     (define check test:check)
     (define alice '(human alice))
     (define saved
       '((1 5 "notes" #("ALPHA" "gamma")
          ((modified-at . 1790368638425969412) (trailing . #t) (base . "ALPHA\nbeta\n") (file . "/tmp/notes.txt"))
          (((5 (human alice) ((batch (human alice) 1)) ((1 0 1 4) ("beta") ("gamma")) #f ()))
           ((1 (human alice) typing "typing" (5) #t #f #t))
           ((1 (human alice) ((batch (human alice) 1)) (0 0 0 5) ("omega") ("ALPHA")))))))
     (store:import! 2 saved)

     ;; the conflict pends again, counted, and the log holds the reapplied entry
     (check 'a-pending-conflict-returns-as-a-record-with-its-count
       (list (store:conflicts 1) (store:property 1 'conflicts #f) (store:property 1 'modified #f) (length (store:log 1)))
       '(((1 (human alice) ((batch (human alice) 1)) (0 0 0 5) ("omega") ("ALPHA"))) 1 #t 1))
     ;; a resolution settles the restored record; its undo revives the conflict
     (check 'the-restored-conflict-settles-and-revives
       (let* ([status (car (call-with-values (lambda () (store:resolve! alice 1 1 'mine)) list))]
              [resolved (list (store:line 1 0) (store:property 1 'conflicts #f))])
         (store:undo! alice 1)
         (list status resolved (store:line 1 0) (store:property 1 'conflicts #f)))
       '(applied ("omega" 0) "ALPHA" 1))
     ;; the restored undo group undoes the entry the reload reapplied
     (check 'the-restored-undo-group-undoes-its-entry
       (begin (store:undo! alice 1) (list (store:line 1 1) (store:property 1 'modified #f)))
       '("beta" #f))
     (test:finish! 'journal)))
