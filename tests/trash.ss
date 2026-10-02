#!/usr/bin/env scheme-script

;; A killed document sits in the trash while its file is visited afresh
;; from disk: the visit takes the plain name, restore! brings the trashed
;; one back beside it under a unique name, the newest of its name first.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(test-evaluate!
  '(begin
     (import (prefix (test) test:) (prefix (core kernel) kernel:) (prefix (foundation text) text:)
       (prefix (head head) head:) (prefix (head widget) widget:) (prefix (state store) store:))
     (define check test:check)
     (include "tests/editor-fixture.sps")
     (define path (format "/tmp/e-trash-~a-~a.txt" (get-process-id) (random 1000000)))
     (call-with-output-file path (lambda (p) (display "on disk\n" p)))
     (define (text b) (let-values ([(lines revision) (store:snapshot b)]) (vector->list lines)))
     (visit! path)
     (define first (view:source (interaction:snapshot editing)))
     (define first-id first)
     (define name (store:buffer-name first))
     (edit:insert! editing "unsaved ")
     (edit:kill-buffer! first)
     (visit! path)
     (define fresh (view:source (interaction:snapshot editing)))
     (check
       'a-visit-after-a-kill-reads-the-disk-into-a-fresh-buffer-under-the-plain-name
       (list
         (and fresh (not (equal? fresh first-id)))
         (text fresh)
         (store:buffer-name fresh)
         (map car (edit:trash)))
       (list #t '("on disk") name (list (string-append name "<2>"))))
     (check
       'the-store-stays-saveable-with-a-trashed-namesake
       (let-values ([(next states) (store:export)]) (store:valid-import? next states))
       #t)
     (edit:insert! editing "second ")
     (edit:kill-buffer! fresh)
     (check
       'the-trash-holds-both-kills-under-distinct-names
       (map car (edit:trash))
       (list name (string-append name "<2>")))
     (define newest (edit:restore! name))
     (define older (edit:restore! (string-append name "<2>")))
     (check
       'restore-brings-both-back-under-their-distinct-names
       (list (equal? newest fresh) (text newest) (store:buffer-name newest) (equal? older first-id) (text older)
         (store:buffer-name older) (equal? (store:property older 'file #f) (store:property newest 'file #f))
         (edit:trash) (equal? (view:source (interaction:snapshot editing)) older))
       (list #t '("second on disk") name #t '("unsaved on disk") (string-append name "<2>") #t '() #f))
     (check
       'permanent-deletion-refuses-live-or-missing-names
       (map (lambda (name) (test:raises? (lambda () (edit:delete-trashed! name))))
            (list (store:buffer-name older) "not-in-trash"))
       '(#t #t))
     (define older-name (store:buffer-name older))
     (edit:kill-buffer! older)
     (edit:delete-trashed! (store:buffer-name older))
     (check
       'permanent-deletion-removes-history-but-keeps-the-file-and-live-namesake
       (list (store:exists? first-id) (edit:trash) (text newest) (call-with-input-file path get-string-all)
         (test:raises? (lambda () (edit:restore! older-name))))
       '(#f () ("second on disk") "on disk\n" #t))
     (delete-file path)
     (test:finish! 'trash)))
