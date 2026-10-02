;; Product suites and external-extension tests share runtime preparation.
(include "tools/test-roots.ss")

;; Attribute explicit composition operations to the test head. This starts
;; no host; each fixture constructs only the views whose behavior it tests.
(define (test-evaluate! expression)
  ((eval '(let ()
            (import (prefix (core kernel) kernel:) (prefix (head head) head:) (prefix (state actor) actor:))
            (lambda (expression) (actor:call-as head:ui-actor
                                   (lambda () (kernel:evaluate! expression (interaction-environment))))))) expression))
