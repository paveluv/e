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
    (compile-imported-libraries #t)
    ;; The loader admits options through the common libraries from eo/base
    ;; before it selects a runtime, so a head's client objects are compiled
    ;; against those identities. Load them the same way first; a client
    ;; runtime assembled entirely from eo/client would recompile everything
    ;; a real head later loads, and vice versa. Every pair of this phase maps
    ;; to eo/base: Chez checks an object it finds against a source only from
    ;; a pair with that same object directory, so mixed pairs would take a
    ;; stale object as it is.
    (library-directories
      (map (lambda (root) (cons (car root) (format "~a/eo/base" here)))
           (append (leaves (format "~a/lib/base" here)) (leaves (string-append here "/lib")))))
    (eval '(import (prefix (startup) startup:) (prefix (kernel) kernel:) (prefix (sys) sys:) (prefix (daemon) daemon:))
      (interaction-environment))
    (library-directories
      (append (leaves (format "~a/lib/~a" here runtime))
              (leaves (string-append here "/lib"))
              (list (cons (string-append here "/tests") objects))))
    (eval '(begin
             (import (prefix (kernel) kernel:))
             (kernel:installation-directory (current-directory)))
      (interaction-environment))))
