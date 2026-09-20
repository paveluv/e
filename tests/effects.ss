#!/usr/bin/env scheme-script

;; The bang is a declaration the sources must honor: a name ending in !
;; reaches an effect, a name without one reaches none unless it says
;; (effects internal), and a command that waits for input says (prompts).
;; The coverage tool checks it statically over the whole tree; its exit
;; status counts the disagreements. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:))
     (define status (system "scheme --script tools/edoc-coverage.sps --effects"))
     (test:check 'every-bang-agrees-with-its-body status 0)
     (test:finish! 'effects)))
