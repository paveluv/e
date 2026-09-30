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
(include "tools/source.ss")
(include "tools/code-health.ss")
(include "tools/code-audit.ss")

(eval
  '(begin
     (import (prefix (test) test:))
     ;; Liveness follows references, including callbacks and dead cycles;
     ;; initialization, external API and opaque syntax are retained.
     (for-each
       (lambda (example)
         (let ([found '()])
           (health-library `(library (probe) (export live) (import (chezscheme)) ,@(cadr example))
             (lambda (at message) (set! found (cons (cadr at) found))))
           (test:check (car example) (reverse found) (caddr example))))
       '((dead-cycle ((define (live) #t) (define (a) (b)) (define (b) (a))) ((a) (b)))
         (callbacks ((define (live) (list helper)) (define (helper) #t)) ())
         (shadow ((define (live helper) helper) (define (helper) #t)) ((helper)))
         (parallel-let ((define (live) (let ([helper (helper)]) helper)) (define (helper) #t)) ())
         (named-let-initializer ((define (live) (let helper ([x (helper)]) x)) (define (helper) #t)) ())
         (internal-definition ((define (live) (define (helper) #t) (helper)) (define (helper) #f)) ((helper)))
         (initializer ((define (live) #t) (define registration (install! helper)) (define (helper) #t)) ())
         (public-requires-export ((define (live) #t) (edoc "External command." (public)) (define (helper) #t)) ((helper)))
         (quoted-command ((define (live) '(helper)) (define (helper) #t)) ())
         (macro-template ((define (live) (invoke)) (define-syntax invoke (syntax-rules () [(_) (helper)])) (define (helper) #t)) ())
         (generated ((define (live) #t) (define-syntax make-it (lambda (x) (datum->syntax x 'generated))) (define (helper) #t)) ())
         (renamed-export ((define live helper) (define (helper) #t) (define unused 1)) (unused))))
     (test:check 'import-wrappers-preserve-identity
       (map (lambda (name)
              (source-import '(prefix (rename (only (probe) f g) (f renamed)) p:) name cons))
         '(p:renamed p:f p:g p:other))
       '(((probe) . f) #f ((probe) . g) #f))
     (test:check 'alpha-normalization-keeps-free-identities-and-literals
       (list (equal? (source-normalize '(lambda (x) (let ([y (f x)]) (g y 1))) values)
                     (source-normalize '(lambda (a) (let ([b (f a)]) (g b 1))) values))
             (equal? (source-normalize '(lambda (x) (f x)) values)
                     (source-normalize '(lambda (x) (g x)) values))
             (equal? (source-normalize '(let* ([x 1] [x (+ x 1)]) x) values)
                     (source-normalize '(let* ([a 1] [b (+ a 1)]) b) values))
             (equal? (source-normalize '(let*-values ([(x) 1] [(x) (+ x 1)]) x) values)
                     (source-normalize '(let*-values ([(a) 1] [(b) (+ a 1)]) b) values)))
       '(#t #f #t #t))
     ;; A pattern must reconstruct each input; repeated substitutions are
     ;; one parameter, and its cost includes both actual arguments.
     (let* ([a '(lambda (x) (if (small? x) (send! x "left") (send! x "left")))]
            [b '(lambda (x) (if (small? x) (send! x "right") (send! x "right")))])
       (let-values ([(pattern holes) (common-pattern a b)])
         (define (instantiate x side)
           (cond [(and (vector? x) (eq? (vector-ref x 0) 'hole))
                  (list-ref (car (list-ref holes (vector-ref x 1))) side)]
                 [(pair? x) (map (lambda (x) (instantiate x side)) x)] [else x]))
         (test:check 'pattern-reconstruction-and-cost
           (list (length holes) (instantiate pattern 0) (instantiate pattern 1)
             (> (pattern-saving a b pattern holes) 0)
             (< (pattern-saving '(f a) '(g b) '#(hole 0) '((((f a) (g b)) . 0))) 0))
           (list 1 a b #t #t))))
     (let* ([form '(library (apps probe) (export called external unused)
                     (import (chezscheme))
                     (define (called) #t)
                     (edoc "External API." (public)) (define (external) #t)
                     (define (unused) #t))]
            [bridge '(library (apps bridge) (export (rename (called exported)))
                       (import (only (apps probe) called)))]
            [base '(library (service seam) (export common base-only) (import (chezscheme))
                     (define (common) #t) (define (base-only) #t))]
            [client '(library (service seam) (export common) (import (chezscheme)) (define (common) #t))]
            [caller '(library (apps caller) (export run!)
                       (import (prefix (rename (only (apps bridge) exported) (exported renamed)) p:)
                               (prefix (service seam) seam:))
                       (define (run!) (p:renamed) (seam:common) (seam:base-only)))]
            [sources (map (lambda (form path) (list path "" form (health-library form (lambda args (void)))))
                       (list form bridge base client caller)
                       '("lib/apps/probe.sls" "lib/apps/bridge.sls" "lib/base/service/seam.sls"
                         "lib/client/service/seam.sls" "lib/apps/caller.sls"))])
       (test:check 'api-inventory-respects-imports-public-and-evidence
         (map (lambda (c) (list (vector-ref c 1) (vector-ref c 3)))
           (audit-apis sources '((manual probe:unused))))
         '((unused manual) (run! unreferenced))))
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
