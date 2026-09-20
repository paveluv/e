#!/usr/bin/env scheme-script

;; The tree's conventions are declarations the sources must honor: exports
;; and imports sorted, a blank line before an edoc and its definition on
;; the line after it, and the bang -- a name ending in ! reaches an
;; effect, a name without one reaches none unless it says (effects
;; internal), and a command that waits for input says (prompts).
;; tools/elinter.sps checks them statically over the whole tree; its exit
;; status counts the findings. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (test) test:))
     (define status (system "scheme --script tools/elinter.sps"))
     (test:check 'the-tree-honors-its-conventions status 0)
     (test:finish! 'lint)))
