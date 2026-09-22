#!/usr/bin/env scheme-script

;; A head started with a file argument visits it once the terminal is live:
;; when the base already holds the file and the disk changed since, the
;; merge/reread/cancel question appears on screen and takes its key, where
;; it used to wait before the input reader ran and hang the editor. Run
;; from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(putenv "SHELL" "/bin/sh")

(define startup-visit-scenario
  '(begin
     (import (prefix (sys sys) sys:) (prefix (service vt) vt:) (prefix (sys glyph) glyph:)
             (prefix (foundation wire) wire:) (prefix (core kernel) kernel:) (prefix (foundation string) string:)
             (prefix (service file) file:) (prefix (fixture) fixture:) (prefix (test) test:))

     (define checks 0)
     (define (check label true?)
       (set! checks (+ checks 1))
       (unless true? (error 'startup-visit (format "~s" label))))

     (define path (string-append (fixture:directory test-base) "/changed.txt"))
     (define (write-disk! text)
       (call-with-output-file path (lambda (port) (put-string port text)) 'replace))

     ;; The base holds the file with an old baseline, as a restart restores it.
     (write-disk! "old\n")
     (define seed (sys:connect-local (string-append (fixture:directory test-base) "/socket")))
     (define (rpc . request)
       (wire:send! (sys:connection-output seed) (cons 'request (cons 1 request)))
       (let loop ()
         (let ([reply (wire:receive (sys:connection-input seed))])
           (cond [(and (pair? reply) (eq? (car reply) 'reply) (eq? (caddr reply) 'ok)) (cadddr reply)]
                 [(and (pair? reply) (eq? (car reply) 'reply)) (error 'startup-visit "request failed" reply)]
                 [else (loop)]))))
     (wire:send! (sys:connection-output seed) (list 'hello wire:version '(head "seed") (kernel:fingerprint)))
     (wire:receive (sys:connection-input seed))
     (define visited
       (rpc 'visit "changed.txt" '("old")
            (list (cons 'file (file:visit-path path)) '(base . "old\n") '(trailing . #t))))
     (check (list 'the-base-holds-the-file visited) (and (pair? visited) (cadr visited)))

     (define (run-head! name answer expected-line)
       ;; a head on the file: the question shows, the answer lands, the text follows
       (let* ([mirror (vt:make-emulator 24 80)]
              [process (sys:spawn-terminal-process "/bin/sh" (fixture:command test-base "--name" name path)
                                                   (current-directory) 24 80)]
              [drain! (fixture:terminal-reader process (lambda (text) (vt:emulator-feed! mirror text)))])
         (define (send! text)
           (let ([output (sys:terminal-process-output process)])
             (put-bytevector output (string->utf8 text))
             (flush-output-port output)))
         (define (screen-lines) (vector->list (vt:emulator-screen mirror)))
         (define (find-cell part)
           (exists (lambda (line) (string:search line part 0 (string-length line))) (screen-lines)))
         (define (wait-for! label predicate milliseconds)
           (set! checks (+ checks 1))
           (let loop ([left (div milliseconds 25)])
             (drain!)
             (or (predicate)
                 (if (= left 0)
                     (begin (for-each (lambda (line) (display (format "|~a|\n" line))) (screen-lines))
                            (error 'startup-visit (format "~s" label)))
                     (begin (sleep (make-time 'time-duration 25000000 0)) (loop (- left 1)))))))
         (wait-for! (list 'the-question-shows-once-keys-arrive name)
                    (lambda () (find-cell "changed on disk: merge, reread, cancel")) 30000)
         (send! answer)
         (wait-for! (list 'the-answer-takes-effect name)
                    (lambda () (and (find-cell expected-line) (not (find-cell "changed on disk")))) 5000)
         (send! "\x18;\x3;")                ; C-x C-c
         (test:await (list 'head-exits name) drain!)
         (sys:reap-terminal-process! process)))

     ;; m)erge: the buffer had no changes of its own, so the disk side wins
     (write-disk! "new\n")
     (run-head! "merging" "m" "new")
     ;; r)eread: the buffer adopts the disk verbatim
     (write-disk! "newer\n")
     (run-head! "rereading" "r" "newer")

     (format #t "~a startup-visit checks passed\n" checks)))

(eval
  `(begin
     (import (prefix (fixture) fixture:) (prefix (test) test:))
     (fixture:call-with-base (current-directory) #f
       (lambda (base)
         (set-top-level-value! 'test-base base)
         (eval ',startup-visit-scenario)))))
