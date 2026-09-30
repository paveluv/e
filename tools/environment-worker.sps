#!/usr/bin/env scheme-script
;; Private worker entry point; no base, head or shadow state store is loaded.
(import (chezscheme))
(let ([root (car (command-line-arguments))])
  (compile-imported-libraries #t)
  (compile-file-message #f)
  (library-directories (list (cons (string-append root "/lib") (string-append root "/eo/base"))))
  (eval '(begin (import (prefix (run evaluator) worker:)) (worker:run!))))
