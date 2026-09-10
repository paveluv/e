;; Shared bootstrap for suites and test drivers. Include from the repository
;; root and select a runtime before importing its consumers.
(import (chezscheme))

(define (test-roots! runtime)
  (let ([objects (case runtime
                   [(base) "eo"]
                   [(client) "eo/client"]
                   [else (error 'test-roots! "expected base or client" runtime)])])
    (library-directories
      (append (if (eq? runtime 'client) (list (cons "lib/client" objects)) '())
              (list (cons "lib" objects) (cons "tests" objects))))
    (library-extensions (cons '(".e" . ".eo")
                          (remove '(".e" . ".eo") (library-extensions))))
    (compile-imported-libraries #t)
    (eval '(begin
             (import (prefix (kernel) kernel:))
             (kernel:installation-directory (current-directory)))
      (interaction-environment))))
