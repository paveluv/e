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
     (import (prefix (core descriptor) descriptor:) (prefix (core startup) startup:)
             (prefix (service session) session:) (prefix (service rewrite) rewrite:)
             (prefix (state actor) actor:) (prefix (state model) model:)
             (prefix (state store) store:) (prefix (foundation text) text:) (prefix (test) test:))

     (define check test:check)
     (define alice '(human alice))
     (define bob '(human bob))
     (define (exercise!)
       (define (undo id) (car (call-with-values (lambda () (store:undo! alice id)) list)))
       (list
         (list (undo '(buffer 1)) (store:line '(buffer 1) 0) (store:property '(buffer 1) 'conflicts))
         (list (undo '(buffer 2)) (store:line '(buffer 2) 0) (store:property '(buffer 2) 'trailing) (undo '(buffer 2)))
         (undo '(buffer 3))
         (list (undo '(buffer 4)) (store:line '(buffer 4) 0) (undo '(buffer 4)) (store:line '(buffer 4) 0))
         (let* ([undone (car (call-with-values (lambda () (store:undo! bob '(buffer 5))) list))]
                [pending (length (store:conflicts '(buffer 5)))]
                [redo (lambda () (car (call-with-values (lambda () (store:redo! alice '(buffer 5))) list)))])
           (list undone pending (redo) (redo) (store:line '(buffer 5) 0) (store:conflicts '(buffer 5))))
         (let* ([back
                 (begin
                   (undo '(buffer 6))
                   (list
                     (store:line '(buffer 6) 0)
                     (store:property '(buffer 6) 'trailing)
                     (store:conflicts '(buffer 6))))]
                [again (begin (store:redo! alice '(buffer 6)) (list (store:line '(buffer 6) 0) (length (store:conflicts '(buffer 6)))))])
           (undo '(buffer 6)) (undo '(buffer 6)) (undo '(buffer 6))
           (let ([original (store:line '(buffer 6) 0)])
             (store:redo! alice '(buffer 6)) (store:redo! alice '(buffer 6)) (store:redo! alice '(buffer 6))
             (list back again original (store:line '(buffer 6) 0) (length (store:conflicts '(buffer 6))))))
         (let ([changed (car (call-with-values
                               (lambda () (store:reload! alice '(buffer 7) '("alpha GAMMA") '((base . "alpha GAMMA") (trailing . #f)))) list))])
           (list changed (store:line '(buffer 7) 0)
             (reverse (fold-left (lambda (out ignored) (undo '(buffer 7)) (cons (store:line '(buffer 7) 0) out)) '() '(1 2 3)))))
         (list (undo '(buffer 8)) (store:line '(buffer 8) 0) (store:property '(buffer 8) 'trailing))))
     (when (pair? (command-line-arguments))
       (let ([data (call-with-input-file (car (command-line-arguments)) read)])
         (store:import! (car data) (cadr data))
         (check 'recovery-preserves-the-live-history-behavior (exercise!) (caddr data))
         (exit 0)))
     (define saved
       '(((buffer 1) 5 "notes" #("ALPHA" "gamma")
          ((modified-at . 1790368638425969412) (trailing . #t) (base . "ALPHA\nbeta\n") (file . "/tmp/notes.txt"))
          (((5 (human alice) ((batch (human alice) 1)) ((1 0 1 4) ("beta") ("gamma")) #f ()))
           ((1 (human alice) typing "typing" (5) #t #f #t))
           ((1 (human alice) ((batch (human alice) 1)) (0 0 0 5) ("omega") ("ALPHA")))))))
     ;; The previous v3 format used numeric document fields. Recovery tags
     ;; known schemas without rewriting extension payloads or revision numbers.
     (let* ([directory (format "/tmp/e-journal-~a" (get-process-id))]
            [path (string-append directory "/session")]
            [view (descriptor:make '(buffer 1) 'editor 1 '((annotations 1 5 ())) '((0 . 0) (0 . 0) (0 . 0) #f))]
            [extension-view (descriptor:make #f 'extension-widget 1 '((annotations . "opaque")) '())]
            [newer-view (descriptor:make #f 'editor 2 '((annotations . "opaque")) '())]
            [checkpoint '(screen 6 1 (window 1 0 0 0 #t #f ((1 model 2))) (((shared 1 5) #f ())))])
       (define (record n kind schema payload)
         (map cons '(id kind schema scope persistence revision actor references value)
           (list (list 'model n) kind schema 'session 'persistent 0 alice '((buffer 1)) payload)))
       (define unknown '((document . 1) (data buffer 1)))
       (mkdir directory #o700)
       (dynamic-wind void
         (lambda ()
           (call-with-output-file path
             (lambda (out)
               (write (list 'session 2 0 2
                        (cons 'buffers (map (lambda (s) (cons (cadar s) (cdr s))) saved))
                        (list 'checkpoints (list "desk" checkpoint))
                        (list 'models 7
                          (record 1 'rewrite-draft 1 '((document . 1) (disabled)))
                          (record 2 'widget-view 2 view)
                          (record 3 'extension-fixture 1 unknown)
                          (record 4 'rewrite-draft 2 unknown)
                          (record 5 'widget-view 2 extension-view)
                          (record 6 'widget-view 2 newer-view))) out)))
           (chmod path #o600)
           (startup:call-with-options (list "--base-working-dir" directory) session:restore!)
           (let ([payload (lambda (n) (cdr (assq 'value (model:snapshot (list 'model n)))))])
             (check 'recovery-upgrades-only-known-document-fields
               (list (store:buffer-list) (payload 1)
                 (cdr (assq 'options (payload 2))) (payload 3) (payload 4) (payload 5) (payload 6)
                 (actor:checkpoint '(head "desk")))
               (list '((buffer 1)) '((document buffer 1) (disabled))
                 '((annotations (buffer 1) 5 ())) unknown unknown extension-view newer-view
                 '(screen 6 1 (window 1 0 0 0 #t #f (((buffer 1) model 2))) (((shared (buffer 1) 5) #f ())))))))
         (lambda () (delete-file path) (delete-directory directory))))

     ;; the conflict pends again, counted, and the log holds the reapplied entry
     (check 'a-pending-conflict-returns-as-a-record-with-its-count
       (list
         (store:conflicts '(buffer 1))
         (store:property '(buffer 1) 'conflicts #f)
         (store:property '(buffer 1) 'modified #f)
         (length (store:log '(buffer 1))))
       '(((1 (human alice) ((batch (human alice) 1)) (0 0 0 5) ("omega") ("ALPHA"))) 1 #t 1))
     ;; a resolution settles the restored record; its undo revives the conflict
     (check 'the-restored-conflict-settles-and-revives
       (let* ([status (car (call-with-values (lambda () (store:resolve! alice '(buffer 1) 1 'mine)) list))]
              [resolved (list (store:line '(buffer 1) 0) (store:property '(buffer 1) 'conflicts #f))])
         (store:undo! alice '(buffer 1))
         (list status resolved (store:line '(buffer 1) 0) (store:property '(buffer 1) 'conflicts #f)))
       '(applied ("omega" 0) "ALPHA" 1))
     ;; the restored undo group undoes the entry the reload reapplied
     (check 'the-restored-undo-group-undoes-its-entry
       (begin (store:undo! alice '(buffer 1)) (list (store:line '(buffer 1) 1) (store:property '(buffer 1) 'modified #f)))
       '("beta" #f))
     ;; Save actual live records, then run the same operations against a
     ;; fresh process. Direct fact writes (including same-value writes)
     ;; must retain their version boundaries, and settled conflicts and
     ;; rewrite-disabled action membership must survive alongside the log.
     (store:resolve! alice '(buffer 1) 1 'mine)
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
