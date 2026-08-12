#!/usr/bin/scheme --script
;; e -- loader for the e editor.
;; Run:  ./e [file]
;;
;; The editor lives in the lib directory next to this script (or,
;; failing that, in ~/.e/lib) as R6RS libraries with the .e extension:
;; the core (core.e, the library (core)) and the extension modules, each
;; a library named after its file -- eval.e is (eval) -- exporting an
;; init! that performs its registrations.  Chez's library system does
;; the rest: sources compile on demand into the eo directory next to
;; lib, recompile when stale (their own source or a library they import
;; changed), and modules' imports of one another order their
;; initialization.

(import (chezscheme))

(define (directory-part path)
  (let loop ([i (- (string-length path) 1)])
    (cond [(< i 0) "."]
          [(char=? (string-ref path i) #\/) (substring path 0 i)]
          [else (loop (- i 1))])))

(define e-home
  ;; Where this installation of the editor lives: next to the script
  ;; itself, so a checkout runs in place -- as ~/.e or inside a project
  ;; -- with ~/.e as the fallback for a copied loader.
  (let ([candidates (list (directory-part (car (command-line)))
                          (format "~a/.e" (or (getenv "HOME") ".")))])
    (or (find (lambda (d) (file-directory? (string-append d "/lib")))
              candidates)
        (begin
          (display (format "e: no lib directory found (looked in ~a)\n"
                           candidates)
                   (current-error-port))
          (exit 1)))))

(define lib-directory (string-append e-home "/lib"))

(library-directories
  (list (cons lib-directory (string-append e-home "/eo"))))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(import (core))

;; Import every extension module and run its init!; a broken module
;; reports itself without keeping the editor (or the others) from
;; starting.
(for-each
  (lambda (file)
    (guard (ex [else
                (display (format "e: ~a: ~a\n" file (error-text ex))
                         (current-error-port))
                (set-message! (format "Error in ~a: ~a" file (error-text ex)))])
      (let ([lib (list (string->symbol
                         (substring file 0 (- (string-length file) 2))))])
        (eval `(import ,lib) (interaction-environment))
        (when (memq 'init! (library-exports lib))
          (eval '(init!) (interaction-environment))))))
  (sort string<?
        (filter (lambda (file)
                  (and (string-suffix? ".e" file)
                       (not (string=? file "core.e"))))
                (directory-list lib-directory))))

(main)
