;; Shared bootstrap for suites and test drivers. Include from the repository
;; root and select a runtime before importing its consumers.
(import (chezscheme))

(define (test-roots! runtime)
  (unless (memq runtime '(base client)) (error 'test-roots! "expected base or client" runtime))
  (let ([here (current-directory)] [objects (format "~a/eo/~a" (current-directory) runtime)])
    (library-directories
      (map (lambda (root) (cons (string-append here "/" root) objects))
        (append (map (lambda (kind) (format "lib/~a/~a" runtime kind)) '(state service))
                '("lib/foundation" "lib/sys" "lib/core" "lib/service"
                  "lib/head" "lib/apps" "lib/modes" "lib/run" "tests"))))
    (compile-imported-libraries #t)
    (eval '(begin
             (import (prefix (kernel) kernel:))
             (kernel:installation-directory (current-directory)))
      (interaction-environment))))
