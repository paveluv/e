#!/usr/bin/env scheme-script

(import (chezscheme))
(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (core kernel) kernel:) (prefix (head head) head:) (prefix (test) test:))
     (define check test:check)
     ;; One frame deadline serves both outer and nested pumps. Multiple
     ;; providers choose the earliest, the head owns its time value, and a
     ;; consumed request cannot keep repainting. The final worker only bounds
     ;; the test if deadline delivery regresses; join it before the next row.
     (for-each
       (lambda (outer?)
         (define posted-context #t)
         ;; Drain earlier store wakes before timing this isolated request.
         (call/cc
           (lambda (done)
             (head:run-on-main!
               (lambda () (set! posted-context (head:in-main-pump)) (done #t)))
             (parameterize ([head:in-main-pump #t]) (head:read-key-event))))
         (let ([first? #t] [frames 0] [outer-callback? #f] [finished? (test:gate)])
           (parameterize ([kernel:registering-module 'frame-deadline-test])
             (head:add-pre-redraw-hook!
               (lambda ()
                 (when first?
                   (set! first? #f)
                   (let* ([now (current-time 'time-monotonic)]
                          [early (add-duration now (make-time 'time-duration 10000000 0))]
                          [late (add-duration now (make-time 'time-duration 0 10))])
                     (head:request-frame-at! late)
                     (head:request-frame-at! early)
                     (head:request-frame-at! late)
                     (set-time-second! early (time-second late)))))))
           (head:before-frame!)
           (let ([stop (test:worker
                         (lambda ()
                           (sleep (make-time 'time-duration 200000000 0))
                           (finished? #t)
                           (head:wake-main!)))])
             (dynamic-wind
               void
               (lambda ()
                 (call/cc
                   (lambda (done)
                     (head:set-frame-hook!
                       (lambda (coalesce?)
                         (set! outer-callback? (or outer-callback? (head:in-main-pump)))
                         (head:before-frame!)
                         (if (finished?) (done #t)
                             (begin
                               (set! frames (+ frames 1))
                               (when (= frames 1)
                                 (head:wake-main!)
                                 (head:wake-main!))))))
                     (parameterize ([head:in-main-pump outer?]) (head:read-key-event)))))
               (lambda ()
                 (head:set-frame-hook! void)
                 (kernel:retract-module! 'frame-deadline-test)
                 (stop)))
             (check (list 'frame-deadline-and-coalesced-wake outer?)
               (list frames posted-context outer-callback?) '(2 #f #f)))))
       '(#t #f))

     (test:finish! 'head)))
