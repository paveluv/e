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
             (prefix (head head) head:))

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
       (list #t '("on disk") name (list name)))

     ;; a second kill of the same name: the trash lists the newest first
     (insert-text! "second ")
     (kill-buffer! fresh)
     (check 'the-trash-holds-both-kills-of-the-name (map car (trash)) (list name name))

     ;; restore! takes the newest; the next restore! the older, under a unique name
     (define newest (restore! name))
     (define older (restore! name))
     (check 'restore-takes-the-newest-then-the-older-under-a-unique-name
       (list (eqv? (head:buffer-store-id newest) (head:buffer-store-id fresh)) (text newest) (head:buffer-name newest)
             (eqv? (head:buffer-store-id older) first-id) (text older) (head:buffer-name older)
             (equal? (head:buffer-file older) (head:buffer-file newest)) (trash) (eq? (head:current-buffer) older))
       (list #t '("second on disk") name #t '("unsaved on disk") (string-append name "<2>") #t '() #t))

     (delete-file path)
     (test:finish! 'trash)))
