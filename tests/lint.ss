#!/usr/bin/env scheme-script

;; The tree's conventions are declarations the sources must honor: exports
;; and imports sorted, a blank line before an edoc and its definition on
;; the line after it, and the bang -- a name ending in ! reaches an
;; effect, a name without one reaches none unless it says (effects
;; internal), and a command that waits for input says (prompts).
;; Log sources match the qualified, export-renamed enclosing definition.
;; tools/elinter.sps checks these statically over the whole tree; its exit
;; status counts the findings. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)
(include "tools/log-sources.ss")

(eval
  '(begin
     (import (prefix (test) test:))
     ;; One table exercises attribution independently of the current tree.
     ;; Callbacks keep their owner; public spellings come from exports.
     (for-each
       (lambda (example)
         (let ([found '()])
           (check-log-sources!
             `(elibrary (apps sample)
                (export (rename (internal public!)))
                (import (prefix (rename (only (service log) add!) (add! write!)) out:))
                ,(cadr example))
             (lambda (at expected message) (set! found (cons expected found))))
           (test:check (car example) (reverse found) (caddr example))))
       '((export-name (define (internal) (out:write! 'sample:public! "ok")) ())
         (wrong-prefix (define (internal) (out:write! 'other:public! "bad")) (sample:public!))
         (internal-spelling (define (internal) (out:write! 'sample:internal "bad")) (sample:public!))
         (wrong-function (define (internal) (out:write! 'sample:other! "bad")) (sample:public!))
         (dynamic-source (define (internal src) (out:write! src "bad")) (sample:public!))
         (alias (define (internal) (let ([logger out:write!]) (logger 'sample:public! "bad"))) (sample:public!))
         (local-callback (define internal (case-lambda [() (let ([f (lambda () (out:write! 'sample:public! "ok"))]) (f))])) ())
         (private (define (helper) (out:write! 'sample:helper "ok")) ())
         (quoted (define (internal) '(out:write! 'anything "data")) ())
         (quasiquoted (define (internal) `(out:write! 'anything ,(out:write! 'bad "call"))) (sample:public!))
         (quoted-vector (define (internal) `#(,(out:write! 'bad "call"))) (sample:public!))
         (syntax-template (define (internal) (quasisyntax (literal (unsyntax (out:write! 'bad "call"))))) (sample:public!))
         (macro (define-syntax internal (syntax-rules () [(_) (out:write! 'arbitrary "bad")])) (#f))
         (apply-source (define (internal) (apply out:write! 'sample:public! '("bad"))) (sample:public!))
         (no-owner (out:write! 'sample:init! "bad") (#f))
         (missing-source (define (internal) (out:write!)) (sample:public!))))
     (define status (system "scheme --script tools/elinter.sps"))
     (test:check 'the-tree-honors-its-conventions status 0)
     (test:finish! 'lint)))
