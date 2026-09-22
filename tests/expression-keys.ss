#!/usr/bin/env scheme-script

;; In a real terminal, C-M-x arrives as ESC then C-x and evaluates the
;; top-level form around point, and C-x C-e evaluates the expression before
;; point; both show the exchange in the echo area. Run from the repository
;; root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(putenv "SHELL" "/bin/sh")

(define expression-keys-scenario
  '(begin
     (import (prefix (sys sys) sys:) (prefix (service vt) vt:) (prefix (foundation string) string:)
             (prefix (fixture) fixture:) (prefix (test) test:))

     (define checks 0)
     (define path (string-append (fixture:directory test-base) "/keyed.scm"))
     (call-with-output-file path (lambda (port) (put-string port "(define keyed 40)\n(+ keyed 2)\n")) 'replace)
     (define mirror (vt:make-emulator 24 80))
     (define process (sys:spawn-terminal-process "/bin/sh" (fixture:command test-base "--name" "keyed" path)
                                                 (current-directory) 24 80))
     (define drain! (fixture:terminal-reader process (lambda (text) (vt:emulator-feed! mirror text))))
     (define (send! text)
       (let ([port (sys:terminal-process-output process)])
         (put-bytevector port (string->utf8 text))
         (flush-output-port port)))
     (define (screen-lines) (vector->list (vt:emulator-screen mirror)))
     (define (on-screen? part)
       (exists (lambda (line) (string:search line part 0 (string-length line))) (screen-lines)))
     (define (wait-for! label predicate milliseconds)
       (set! checks (+ checks 1))
       (let loop ([left (div milliseconds 25)])
         (drain!)
         (or (predicate)
             (if (= left 0)
                 (begin (for-each (lambda (line) (display (format "|~a|\n" line))) (screen-lines))
                        (error 'expression-keys (format "~s" label)))
                 (begin (sleep (make-time 'time-duration 25000000 0)) (loop (- left 1)))))))

     (wait-for! 'the-head-shows-the-file (lambda () (on-screen? "(+ keyed 2)")) 30000)
     (send! "\x1b;\x18;")                 ; C-M-x on the first line: the definition
     (wait-for! 'control-meta-x-evaluates-the-top-level-form (lambda () (on-screen? "(define keyed 40) =>")) 5000)
     (send! "\x0e;\x05;")                 ; C-n, C-e: after (+ keyed 2)
     (send! "\x18;\x05;")                 ; C-x C-e
     (wait-for! 'control-x-control-e-evaluates-the-expression-before-point (lambda () (on-screen? "(+ keyed 2) => 42")) 5000)
     (send! "\x18;\x3;")                  ; C-x C-c
     (test:await 'head-exits drain!)
     (sys:reap-terminal-process! process)
     (format #t "~a expression-keys checks passed\n" checks)))

(eval
  `(begin
     (import (prefix (fixture) fixture:) (prefix (test) test:))
     (fixture:call-with-base (current-directory) #f
       (lambda (base)
         (set-top-level-value! 'test-base base)
         (eval ',expression-keys-scenario)))))
