#!/usr/bin/env scheme-script

;; A killed document sits in the trash while its file is visited afresh
;; from disk: the visit takes the plain name, restore! brings the trashed
;; one back beside it under a unique name, the newest of its name first.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:)
             (prefix (state store) store:))

     (define check test:check)
     (define path (format "/tmp/e-trash-~a-~a.txt" (get-process-id) (random 1000000)))
     (call-with-output-file path (lambda (p) (display "on disk\n" p)))
     (define (text b) (vector->list (head:buffer-lines b)))

     ;; issue #7: open, kill, open again
     (visit-file! path)
     (define first (head:current-buffer))
     (define first-id (head:buffer-store-id first))
     (define name (head:buffer-name first))
     (insert-text! "unsaved ")
     (kill-buffer! first)
     (visit-file! path)
     (define fresh (head:current-buffer))
     (check 'a-visit-after-a-kill-reads-the-disk-into-a-fresh-buffer-under-the-plain-name
       (list (and (head:buffer-store-id fresh) (not (eqv? (head:buffer-store-id fresh) first-id)))
             (text fresh) (head:buffer-name fresh) (map car (trash)))
       (list #t '("on disk") name (list (string-append name "<2>"))))

     (check 'the-store-stays-saveable-with-a-trashed-namesake
       (let-values ([(next states) (store:export)]) (store:valid-import? next states)) #t)
     ;; a second kill of the same name: the trash lists the newest first
     (insert-text! "second ")
     (kill-buffer! fresh)
     (check 'the-trash-holds-both-kills-under-distinct-names (map car (trash)) (list name (string-append name "<2>")))

     ;; restore! takes the newest; the next restore! the older, under a unique name
     (define newest (restore! name))
     (define older (restore! (string-append name "<2>")))
     (check 'restore-brings-both-back-under-their-distinct-names
       (list (eqv? (head:buffer-store-id newest) (head:buffer-store-id fresh)) (text newest) (head:buffer-name newest)
             (eqv? (head:buffer-store-id older) first-id) (text older) (head:buffer-name older)
             (equal? (head:buffer-file older) (head:buffer-file newest)) (trash) (eq? (head:current-buffer) older))
       (list #t '("second on disk") name #t '("unsaved on disk") (string-append name "<2>") #t '() #t))

     (check 'permanent-deletion-refuses-live-or-missing-names
       (map (lambda (name) (test:raises? (lambda () (delete-trashed! name))))
         (list (head:buffer-name older) "not-in-trash")) '(#t #t))
     (kill-buffer! older)
     (delete-trashed! (head:buffer-name older))
     (check 'permanent-deletion-removes-history-but-keeps-the-file-and-live-namesake
       (list (store:exists? first-id) (trash) (text newest)
             (call-with-input-file path get-string-all)
             (test:raises? (lambda () (restore! (head:buffer-name older)))))
       '(#f () ("second on disk") "on disk\n" #t))

     (delete-file path)
     (test:finish! 'trash)))
