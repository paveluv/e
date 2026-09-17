#!/usr/bin/env scheme-script

;; Import every library of the base runtime and the test libraries, so their
;; objects are current before concurrent suites import them without a lock.
;; Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(define (stems directory)
  (let loop ([names (directory-list directory)] [acc '()])
    (cond [(null? names) acc]
          [(file-directory? (string-append directory "/" (car names)))
           (loop (cdr names) (append (stems (string-append directory "/" (car names))) acc))]
          [(let ([name (car names)] [n (string-length (car names))])
             (and (> n 4) (string=? (substring name (- n 4) n) ".sls")))
           (loop (cdr names) (cons (substring (car names) 0 (- (string-length (car names)) 4)) acc))]
          [else (loop (cdr names) acc)])))

(define (library-stems)
  ;; The client tree is compiled by the heads that use it; tests run on base roots.
  (append (stems "lib/base")
          (apply append
            (map (lambda (kind) (stems (string-append "lib/" kind)))
              (filter (lambda (name) (and (not (member name '("base" "client")))
                                          (file-directory? (string-append "lib/" name))))
                (directory-list "lib"))))
          (stems "tests")))

(for-each
  (lambda (stem)
    (unless (string=? stem "cache")   ; bootstrap-only, loaded from source by the loader
      (eval `(import (,(string->symbol stem))) (interaction-environment))))
  (list-sort string<? (library-stems)))
(display "libraries current\n")
