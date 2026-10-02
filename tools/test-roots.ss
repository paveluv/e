;; Shared bootstrap for suites and test drivers. Include from the repository
;; root and select a runtime before importing its consumers.
(import (chezscheme))

(define (test-roots! runtime)
  (unless (memq runtime '(base client)) (error 'test-roots! "expected base or client" runtime))
  ;; The loader's rule: the runtime's implementation tree in front of lib,
  ;; a library (kind leaf) at <root>/kind/leaf.sls; tests adds (test), flat.
  (let ([here (current-directory)] [objects (format "~a/eo/~a" (current-directory) runtime)])
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
      (list (cons (format "~a/lib/base" here) (format "~a/eo/base" here))
            (cons (string-append here "/lib") (format "~a/eo/base" here))))
    (eval '(import (prefix (core startup) startup:) (prefix (core kernel) kernel:) (prefix (sys sys) sys:) (prefix (core daemon) daemon:))
      (interaction-environment))
    (library-directories
      (list (cons (format "~a/lib/~a" here runtime) objects)
            (cons (string-append here "/lib") objects)
            (cons (string-append here "/tests") objects)))
    (eval '(begin
             (import (prefix (core kernel) kernel:))
             (kernel:installation-directory (current-directory)))
      (interaction-environment))))
