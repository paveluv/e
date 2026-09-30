;; Execution capture shared by head commands and isolated evaluators.
(import (only (foundation edoc) elibrary))
(elibrary (core evaluation)
  (export call! (rename (evaluation-condition condition) (evaluation-status status) (evaluation-values values)))
  (import (chezscheme) (prefix (sys sys) sys:))

  (edoc "An execution outcome before presentation or serialization."
        (status (one-of ok error interrupted) "completion status")
        (values list "ordinary returned values") (condition (or condition #f) "original failure"))
  (define-record-type evaluation (fields status values condition))
  (define active? (make-thread-parameter #f))

  (edoc "Capture ordinary values or the original condition, streaming stdout, stderr and quiet library compilation through an explicit sink. Nested calls share the active capture. Continuation suspension releases descriptors and readers; reentry opens a fresh segment. No editor, journal or reporting policy is installed."
        (thunk thunk "computation") (emit procedure "(channel line) output sink")
        (interrupt procedure "run a thunk with the caller's interruption policy")
        (interrupted? procedure "classify a caught condition") (returns (record evaluation)))
  (define (call! thunk emit interrupt interrupted?)
    (define (run)
      (guard (ex [else (make-evaluation (if (interrupted? ex) 'interrupted 'error) '() ex)])
        (call-with-values thunk (lambda vals (make-evaluation 'ok vals #f)))))
    (if (active?) (run)
      (let ([lock (make-mutex)] [terminal #f] [previous #f]
            [compile-default (compile-library-handler)])
        (define (record! channel line)
          (parameterize ([sys:terminal-output-port terminal]) (with-mutex lock (emit channel line))))
        (define (compile-quietly source object)
          (record! 'compile source)
          (parameterize ([compile-file-message #f]) (compile-default source object)))
        (dynamic-wind
          (lambda ()
            (set! previous (sys:terminal-output-port))
            (set! terminal (sys:duplicate-standard-output-port))
            (sys:terminal-output-port terminal))
          (lambda ()
            (parameterize ([active? #t] [compile-library-handler compile-quietly])
              (sys:call-with-streamed-output
                (lambda (line) (record! 'stdout line)) (lambda (line) (record! 'stderr line))
                (lambda () (interrupt run)) #t)))
          (lambda () (sys:terminal-output-port previous) (close-port terminal)))))))
