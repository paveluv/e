#!/usr/bin/env scheme-script

;; Usage: scheme --script /path/to/e/tools/test-extension.sps
;;          repository entry tests/smoke.ss [additional-library-root ...]
;; A headless, in-process editor; no personal config, daemon or terminal.
(import (chezscheme))

(define (absolute path)
  (if (path-absolute? path) path (string-append (current-directory) "/" path)))
(define installation (path-parent (path-parent (absolute (car (command-line))))))
(define arguments (command-line-arguments))
(unless (>= (length arguments) 3)
  (display "usage: test-extension.sps repository entry test-file [library-root ...]\n" (current-error-port))
  (exit 2))
(define repository (absolute (car arguments)))
(define entry (cadr arguments))
(define test-file (caddr arguments))
(define roots (cdddr arguments))

(current-directory installation)
(load "tools/test-roots.ss")
;; Serialize compilation with ordinary heads using this installation.
(library-directories (list (cons "lib" "eo/base")))
(compile-imported-libraries #t)
(load "lib/sys/cache.sls")
(eval `(begin (import (prefix (sys cache) cache:))
              (cache:install! ,(string-append installation "/eo"))))
(eval '(test-roots! 'base))
(eval '(begin
         (import (prefix (core kernel) kernel:) (prefix (core extension) extension:))
         (kernel:load-modules! '("edit" "eval" "scheme-mode"))))
(eval `(extension:load! ,repository ,entry ',roots))
(current-directory repository)
(load test-file)
