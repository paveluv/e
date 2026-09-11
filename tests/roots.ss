;; Shared bootstrap for suites and test drivers. Include from the repository
;; root and select a runtime before importing its consumers.
(import (chezscheme))

(define (test-roots! runtime)
  (unless (memq runtime '(base client)) (error 'test-roots! "expected base or client" runtime))
  ;; The loader's rule: every leaf directory under lib is a root, the
  ;; runtime's implementation tree and the common kinds; tests adds (test).
  (let ([here (current-directory)] [objects (format "~a/eo/~a" (current-directory) runtime)])
    (define (leaves parent)
      (map (lambda (name) (cons (string-append parent "/" name) objects))
        (list-sort string<?
          (filter (lambda (name)
                    (and (not (member name '("base" "client")))
                         (file-directory? (string-append parent "/" name))))
                  (directory-list parent)))))
    (library-directories
      (append (leaves (format "~a/lib/~a" here runtime))
              (leaves (string-append here "/lib"))
              (list (cons (string-append here "/tests") objects))))
    (compile-imported-libraries #t)
    (eval '(begin
             (import (prefix (kernel) kernel:))
             (kernel:installation-directory (current-directory)))
      (interaction-environment))))
