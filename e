#!/usr/bin/env scheme-script
;; e -- loader for the e editor.
;; Run:  ./e [file]
;;
;; The editor lives in the lib directory next to this script as R6RS
;; libraries with the .e extension: the core (core.e, the library
;; (core)) and the extension modules, each a library named after its
;; file -- eval.e is (eval) -- exporting an init! that performs its
;; registrations.  Chez's library system does the rest: sources compile
;; on demand into the eo directory next to lib, recompile when stale
;; (their own source or a library they import changed), and modules'
;; imports of one another order their initialization.
;;
;; scheme-script runs this file with R6RS program semantics, where a
;; literal (import (core)) would resolve before library-directories is
;; set below -- so the editor's libraries are imported at run time, in
;; the interaction environment, which is also where M-x evaluates and
;; where the modules must land anyway.

(import (chezscheme))

(define (directory-part path)
  (let loop ([i (- (string-length path) 1)])
    (cond [(< i 0) "."]
          [(char=? (string-ref path i) #\/) (substring path 0 i)]
          [else (loop (- i 1))])))

(define (dot-e? file)
  (let ([n (string-length file)])
    (and (> n 2) (string=? (substring file (- n 2) n) ".e"))))

(define (error->string ex)
  (if (condition? ex)
      (with-output-to-string (lambda () (display-condition ex)))
      (format "~a" ex)))

(define e-home
  ;; Where this installation of the editor lives: strictly the directory
  ;; of the script itself, so a checkout runs in place -- as ~/.e or
  ;; inside a project -- and every installation is self-contained.
  (let ([dir (directory-part (car (command-line)))])
    (unless (file-directory? (string-append dir "/lib"))
      (display (format "e: no lib directory in ~a\n" dir)
               (current-error-port))
      (exit 1))
    dir))

(define lib-directory (string-append e-home "/lib"))

(library-directories
  (list (cons lib-directory (string-append e-home "/eo"))))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval '(import (core)) (interaction-environment))

;; Import every extension module and run its init!; a broken module
;; reports itself without keeping the editor (or the others) from
;; starting.
(for-each
  (lambda (file)
    (guard (ex [else
                (let ([msg (format "Error in ~a: ~a" file (error->string ex))])
                  (display (format "e: ~a\n" msg) (current-error-port))
                  (eval `(set-message! ,msg) (interaction-environment)))])
      (let ([lib (list (string->symbol
                         (substring file 0 (- (string-length file) 2))))])
        (eval `(import ,lib) (interaction-environment))
        (when (memq 'init! (library-exports lib))
          (eval '(init!) (interaction-environment))))))
  (sort string<?
        (filter (lambda (file)
                  (and (dot-e? file) (not (string=? file "core.e"))))
                (directory-list lib-directory))))

(eval '(main) (interaction-environment))
