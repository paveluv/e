#!/usr/bin/env scheme-script

;; A killed document sits in the trash while its file is visited afresh
;; from disk: the visit takes the plain name, restore! brings the trashed
;; one back beside it under a unique name, the newest of its name first.

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-host!)

(eval
  '(begin
     (import (prefix (test) test:)
             (except (head edit) init!)
             (prefix (head head) head:) (prefix (head seat) seat:) (prefix (head window-host) window-host:) (prefix (head widget) widget:)
             (prefix (only (head edit) init!) edit:)
             (prefix (state store) store:))

     (define check test:check)
     (widget:init!) (edit:init!) (window-host:init!)
     (define path (format "/tmp/e-trash-~a-~a.txt" (get-process-id) (random 1000000)))
     (call-with-output-file path (lambda (p) (display "on disk\n" p)))
     (define (text b) (vector->list (seat:buffer-lines b)))

     ;; issue #7: open, kill, open again
     (visit-file! path)
     (define first (seat:current-buffer-mirror))
     (define first-id (seat:buffer-store-id first))
     (define name (seat:buffer-name first))
     (insert-text! "unsaved ")
     (kill-buffer! (seat:buffer-store-id first))
     (visit-file! path)
     (define fresh (seat:current-buffer-mirror))
     (check 'a-visit-after-a-kill-reads-the-disk-into-a-fresh-buffer-under-the-plain-name
       (list (and (seat:buffer-store-id fresh) (not (equal? (seat:buffer-store-id fresh) first-id)))
             (text fresh) (seat:buffer-name fresh) (map car (trash)))
       (list #t '("on disk") name (list (string-append name "<2>"))))

     (check 'the-store-stays-saveable-with-a-trashed-namesake
       (let-values ([(next states) (store:export)]) (store:valid-import? next states)) #t)
     ;; a second kill of the same name: the trash lists the newest first
     (insert-text! "second ")
     (kill-buffer! (seat:buffer-store-id fresh))
     (check 'the-trash-holds-both-kills-under-distinct-names (map car (trash)) (list name (string-append name "<2>")))

     ;; restore! takes the newest; the next restore! the older, under a unique name
     (define newest (seat:adopt-store-buffer! (restore! name)))
     (define older (seat:adopt-store-buffer! (restore! (string-append name "<2>"))))
     (check 'restore-brings-both-back-under-their-distinct-names
       (list (equal? (seat:buffer-store-id newest) (seat:buffer-store-id fresh)) (text newest) (seat:buffer-name newest)
             (equal? (seat:buffer-store-id older) first-id) (text older) (seat:buffer-name older)
             (equal? (seat:buffer-file older) (seat:buffer-file newest)) (trash) (eq? (seat:current-buffer-mirror) older))
       (list #t '("second on disk") name #t '("unsaved on disk") (string-append name "<2>") #t '() #f))

     (check 'permanent-deletion-refuses-live-or-missing-names
       (map (lambda (name) (test:raises? (lambda () (delete-trashed! name))))
         (list (seat:buffer-name older) "not-in-trash")) '(#t #t))
     (kill-buffer! (seat:buffer-store-id older))
     (delete-trashed! (seat:buffer-name older))
     (check 'permanent-deletion-removes-history-but-keeps-the-file-and-live-namesake
       (list (store:exists? first-id) (trash) (text newest)
             (call-with-input-file path get-string-all)
             (test:raises? (lambda () (restore! (seat:buffer-name older)))))
       '(#f () ("second on disk") "on disk\n" #t))

     (delete-file path)
     (test:finish! 'trash)))
