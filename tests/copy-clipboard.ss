#!/usr/bin/env scheme-script

;; With clipboard forwarding on, a copy publishes its text to the terminal
;; through OSC 52 at once, and a hand edit in <copy> publishes at the next
;; frame. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(putenv "SHELL" "/bin/sh")

(define copy-clipboard-scenario
  '(begin
     (import (prefix (sys sys) sys:) (prefix (service vt) vt:) (prefix (foundation string) string:)
             (prefix (fixture) fixture:) (prefix (test) test:))

     (define checks 0)
     (define output "")                 ; everything the head wrote to its terminal
     (define path (string-append (fixture:directory test-base) "/copy.txt"))
     (call-with-output-file path (lambda (port) (put-string port "hello\n")) 'replace)
     (define mirror (vt:make-emulator 24 80))
     (define process (sys:spawn-terminal-process "/bin/sh" (fixture:command test-base "--name" "copier" path)
                                                 (current-directory) 24 80))
     (define drain! (fixture:terminal-reader process (lambda (text)
                                                       (set! output (string-append output text))
                                                       (vt:emulator-feed! mirror text))))
     (define (send! text)
       (let ([port (sys:terminal-process-output process)])
         (put-bytevector port (string->utf8 text))
         (flush-output-port port)))
     (define (screen-lines) (vector->list (vt:emulator-screen mirror)))
     (define (on-screen? part)
       (exists (lambda (line) (string:search line part 0 (string-length line))) (screen-lines)))
     (define (written? part) (and (string:search output part 0 (string-length output)) #t))
     (define (wait-for! label predicate milliseconds)
       (set! checks (+ checks 1))
       (let loop ([left (div milliseconds 25)])
         (drain!)
         (or (predicate)
             (if (= left 0)
                 (begin (for-each (lambda (line) (display (format "|~a|\n" line))) (screen-lines))
                        (error 'copy-clipboard (format "~s" label)))
                 (begin (sleep (make-time 'time-duration 25000000 0)) (loop (- left 1)))))))
     (define (evaluate! text)
       ;; type a form into M-x and run it
       (send! (string-append "\x1b;x" text "\r")))

     (wait-for! 'the-head-shows-the-file (lambda () (on-screen? "hello")) 30000)
     (evaluate! "(edit:forward-copy-buffer-to-system-clipboard #t)")
     (evaluate! "(edit:copy-text! \"abc\")")
     (wait-for! 'a-copy-publishes-its-text-at-once (lambda () (written? "\x1b;]52;c;YWJj\x1b;\\")) 5000)
     (evaluate! "(begin (head:show-buffer! (head:copy-buffer)) (void))")
     (wait-for! 'the-copy-buffer-shows (lambda () (on-screen? "<copy>")) 5000)
     (send! "d")
     (wait-for! 'a-hand-edit-in-the-copy-buffer-publishes-at-the-next-frame
                (lambda () (written? "\x1b;]52;c;YWJjZA==\x1b;\\")) 5000)
     (send! "\x18;\x3;")                ; C-x C-c
     (test:await 'head-exits drain!)
     (sys:reap-terminal-process! process)
     (format #t "~a copy-clipboard checks passed\n" checks)))

(eval
  `(begin
     (import (prefix (fixture) fixture:) (prefix (test) test:))
     (fixture:call-with-base (current-directory) #f
       (lambda (base)
         (set-top-level-value! 'test-base base)
         (eval ',copy-clipboard-scenario)))))
