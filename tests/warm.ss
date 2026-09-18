#!/usr/bin/env scheme-script

;; Import every library of one runtime -- base by default, or client -- and
;; the test libraries, so their objects are current before concurrent suites
;; and the wire fixtures' seeded copies use them without a lock. Run from the
;; repository root: scheme --script tests/warm.ss [base|client]

(import (chezscheme))

(include "tests/roots.ss")
(define runtime
  (if (and (pair? (command-line-arguments)) (string=? (car (command-line-arguments)) "client")) 'client 'base))
(test-roots! runtime)

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
  (append (stems (string-append "lib/" (symbol->string runtime)))
          (apply append
            (map (lambda (kind) (stems (string-append "lib/" kind)))
              (filter (lambda (name) (and (not (member name '("base" "client")))
                                          (file-directory? (string-append "lib/" name))))
                (directory-list "lib"))))
          (stems "tests")))

(for-each
  (lambda (stem)
    ;; cache is bootstrap-only, loaded from source by the loader; the daemon
    ;; entrypoint and the base-only services stay out of the client runtime.
    (unless (or (string=? stem "cache")
                (and (eq? runtime 'client) (member stem '("base" "policy" "sandbox"))))
      (eval `(import (,(string->symbol stem))) (interaction-environment))))
  (list-sort string<? (library-stems)))
(display (format "~a libraries current\n" runtime))
