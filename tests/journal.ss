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
     (import (prefix (state store) store:) (prefix (foundation text) text:) (prefix (test) test:))

     (define check test:check)
     (define alice '(human alice))
     (define bob '(human bob))
     (define (exercise!)
       (define (undo id) (car (call-with-values (lambda () (store:undo! alice id)) list)))
       (list
         (list (undo 1) (store:line 1 0) (store:property 1 'conflicts))
         (list (undo 2) (store:line 2 0) (store:property 2 'trailing) (undo 2))
         (undo 3)
         (list (undo 4) (store:line 4 0) (undo 4) (store:line 4 0))
         (let* ([undone (car (call-with-values (lambda () (store:undo! bob 5)) list))]
                [pending (length (store:conflicts 5))]
                [redo (lambda () (car (call-with-values (lambda () (store:redo! alice 5)) list)))])
           (list undone pending (redo) (redo) (store:line 5 0) (store:conflicts 5)))
         (let* ([back (begin (undo 6) (list (store:line 6 0) (store:property 6 'trailing) (store:conflicts 6)))]
                [again (begin (store:redo! alice 6) (list (store:line 6 0) (length (store:conflicts 6))))])
           (undo 6) (undo 6) (undo 6)
           (let ([original (store:line 6 0)])
             (store:redo! alice 6) (store:redo! alice 6) (store:redo! alice 6)
             (list back again original (store:line 6 0) (length (store:conflicts 6)))))
         (let ([changed (car (call-with-values
                               (lambda () (store:reload! alice 7 '("alpha GAMMA") '((base . "alpha GAMMA") (trailing . #f)))) list))])
           (list changed (store:line 7 0)
             (reverse (fold-left (lambda (out ignored) (undo 7) (cons (store:line 7 0) out)) '() '(1 2 3)))))
         (list (undo 8) (store:line 8 0) (store:property 8 'trailing))))
     (when (pair? (command-line-arguments))
       (let ([data (call-with-input-file (car (command-line-arguments)) read)])
         (store:import! (car data) (cadr data))
         (check 'recovery-preserves-the-live-history-behavior (exercise!) (caddr data))
         (exit 0)))
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
     ;; Save actual live records, then run the same operations against a
     ;; fresh process. Direct fact writes (including same-value writes)
     ;; must retain their version boundaries, and settled conflicts and
     ;; rewrite-disabled action membership must survive alongside the log.
     (store:resolve! alice 1 1 'mine)
     (define properties (store:create! alice "properties" '("a") '((trailing . #f))))
     (store:edit! alice properties 0 (text:make-span 0 1 0 1) '("A") '(first "first" (undo (trailing . #t))))
     (store:set-property! alice properties 'trailing #f)
     (store:edit! alice properties 1 (text:make-span 0 2 0 2) '("B") '(second "second" (undo (trailing . #t))))
     (define same (store:create! alice "same-value" '("a") '((trailing . #f))))
     (store:edit! alice same 0 (text:make-span 0 1 0 1) '("A") '(first "first" (undo (trailing . #t))))
     (store:set-property! alice same 'trailing #t)
     (define rewritten (store:create! alice "rewrite" '("a")))
     (store:edit! alice rewritten 0 (text:make-span 0 0 0 1) '("b"))
     (store:rewrite! alice rewritten '(1))
     (define pending (store:create! alice "pending" '("alpha beta gamma") '((base . "alpha beta gamma") (trailing . #f))))
     (store:edit! alice pending 0 (text:make-span 0 11 0 16) '("G"))
     (store:edit! alice pending 1 (text:make-span 0 0 0 5) '("A"))
     (store:reload! alice pending '("DISK beta DISK") '((base . "DISK beta DISK")))
     (for-each (lambda (c) (store:resolve! alice pending (car c) 'mine)) (store:conflicts pending))
     (store:undo! alice pending)
     (store:undo! alice pending)
     (store:edit! bob pending (store:revision pending) (text:make-span 0 0 0 14) '("custom"))
     (for-each
       (lambda (disk)
         (let ([id (store:create! alice "reload history" '("alpha beta") '((base . "alpha beta\n") (trailing . #t)))])
           (store:edit! alice id 0 (text:make-span 0 0 0 5) '("ALPHA"))
           (when (string=? disk "disk beta")
             (store:edit! alice id 1 (text:make-span 0 0 0 5) '("Mine")))
           (store:reload! alice id (list disk) (list (cons 'base disk) '(trailing . #f)))))
       '("disk beta" "alpha BETA"))
     ;; Identical reread must preserve the reload's newline version.
     (define unchanged (store:create! alice "unchanged reread" '("abc") '((base . "abc\n") (trailing . #t))))
     (store:reload! alice unchanged '("xyz") '((base . "xyz") (trailing . #f)))
     (store:reread! alice unchanged '("xyz") '((base . "xyz") (trailing . #f)))
     (define path (format "/tmp/e-journal-~a" (get-process-id)))
     (let*-values ([(next states) (store:export)] [(expected) (exercise!)])
       (check 'live-history-covers-resolutions-and-property-version-boundaries expected
         '((applied "ALPHA" 1) (applied "aA" #f blocked) blocked (applied "b" applied "a")
           (applied 2 applied applied "A beta G" ())
           (("Mine beta" #t ()) ("disk beta" 1) "alpha beta" "disk beta" 1)
           (applied "ALPHA GAMMA" ("ALPHA BETA" "ALPHA beta" "alpha beta"))
           (applied "abc" #t)))
       (dynamic-wind
         (lambda () (call-with-output-file path (lambda (p) (write (list next states expected) p))))
         (lambda () (check 'journal-round-trip (system (format "scheme --script tests/journal.ss ~a" path)) 0))
         (lambda () (delete-file path))))
     (test:finish! 'journal)))
