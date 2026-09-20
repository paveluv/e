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

(define (libraries directory qualified?)
  ;; the library names of the .sls files below directory: (kind stem) under
  ;; a kind directory, (stem) for the flat ones of the tests root
  (let loop ([names (directory-list directory)] [acc '()])
    (cond [(null? names) acc]
          [(file-directory? (string-append directory "/" (car names)))
           (loop (cdr names) (append (libraries (string-append directory "/" (car names)) qualified?) acc))]
          [(let ([name (car names)] [n (string-length (car names))])
             (and (> n 4) (string=? (substring name (- n 4) n) ".sls")))
           (let ([stem (string->symbol (substring (car names) 0 (- (string-length (car names)) 4)))])
             (loop (cdr names)
                   (cons (if qualified? (list (string->symbol (path-last directory)) stem) (list stem)) acc)))]
          [else (loop (cdr names) acc)])))

(define (library-names)
  (append (libraries (string-append "lib/" (symbol->string runtime)) #t)
          (apply append
            (map (lambda (kind) (libraries (string-append "lib/" kind) #t))
              (filter (lambda (name) (and (not (member name '("base" "client")))
                                          (file-directory? (string-append "lib/" name))))
                (directory-list "lib"))))
          (libraries "tests" #f)))

(for-each
  (lambda (name)
    ;; cache is bootstrap-only, loaded from source by the loader; the daemon
    ;; entrypoint and the base-only services stay out of the client runtime.
    (unless (or (equal? name '(sys cache))
                (and (eq? runtime 'client) (member name '((run base) (service policy) (service sandbox)))))
      (eval `(import ,name) (interaction-environment))))
  (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) (library-names)))
(display (format "~a libraries current\n" runtime))
