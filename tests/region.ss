#!/usr/bin/env scheme-script

;; Regions are owned portable values, resolved by identity and exact bounds.
(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)
(test-evaluate!
  '(begin
     (import (prefix (core region) region:) (prefix (head edit) edit:)
             (prefix (head head) head:) (prefix (state store) store:) (prefix (test) test:))
     (define check test:check)
     (define id (store:create! head:ui-actor "region" '("x one x" "two x")))
     (check 'shape-does-not-resolve-document
       (map region:valid?
         '((region (buffer 999999) (0 . 0) (1 . 2))
           (region (buffer 1) (0 . 0) (999999999999999999999 . 0))
           (region (buffer 1) (1 . 0) (0 . 0)) (region (model 1) (0 . 0) (0 . 0))
           (region (buffer 1) (-1 . 0) (0 . 0)) (region (buffer 1) (0 . 0) (0 . 1.0))
           (region (buffer 1) (0 0) (1 . 0)) (region (buffer 1) (0 . 0))))
       '(#t #t #f #f #f #f #f #f))
     (check 'construction-orders-and-owns-data
       (let* ([ref (list 'buffer (cadr id))] [p (cons 1 2)] [q (cons 0 3)] [r (region:make ref p q)])
         (set-car! (cdr ref) 999999) (set-car! p 99) (set-cdr! q 99)
         (list r (region:buffer r) (region:start r) (region:end r)))
       (list (list 'region id '(0 . 3) '(1 . 2)) id '(0 . 3) '(1 . 2)))
     (check 'extraction-uses-exact-character-endpoints
       (map edit:region-text (list (region:make id '(0 . 2) '(1 . 3)) (region:make id '(1 . 5) '(1 . 5))))
       '("one x\ntwo" ""))
     (check 'malformed-and-outside-regions-refuse
       (map (lambda (bad) (test:raises? (lambda () (edit:region-text bad))))
         (list (region:make id '(0 . 0) '(0 . 99)) (region:make id '(0 . 0) '(2 . 0))
           (list 'region id '(1 . 3) '(0 . 0)))) '(#t #t #t))
     (define saved (region:make id '(0 . 0) '(0 . 3)))
     (store:rename! head:ui-actor id "renamed region")
     (check 'rename-keeps-region-identity (edit:region-text saved) "x o")
     (store:delete! head:ui-actor id)
     (store:create! head:ui-actor "renamed region" '("replacement"))
     (check 'deletion-cannot-retarget-a-region (test:raises? (lambda () (edit:region-text saved))) #t)
     (check 'document-queries-need-no-presentation
       (let ([id (store:create! head:ui-actor "unseen" '("kept"))])
         (edit:kill-buffer! id)
         (list (store:exists? id) (store:visible? head:ui-actor id) (edit:buffer-text id) (edit:buffer-clean? id)
           (equal? (edit:restore! "unseen") id))) '(#t #f "kept" #f #t))
     (test:finish! 'region)))
