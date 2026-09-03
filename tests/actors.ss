#!/usr/bin/env scheme-script

;; The interaction protocol: registration, delivery, ask/answer
;; round trips, tickets, and cancellation -- v2 stage 3
;; (docs/DESIGN2.md).  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (actors) actors:)
             (prefix (kernel) kernel:)
             (only (chezscheme) box unbox set-box! fork-thread
                   make-time sleep))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'actors-test label actual expected)))

     ;; two mailbox-backed actors
     (define human '(head test))
     (define agent '(agent probe 1))
     (define human-mail (kernel:make-mailbox))
     (define agent-mail (kernel:make-mailbox))
     (actors:register! human (lambda (m) (kernel:mailbox-post! human-mail m)))
     (actors:register! agent (lambda (m) (kernel:mailbox-post! agent-mail m)))

     (check 'registered
            (list (actors:unregister? human) (actors:unregister? agent)
                  (actors:unregister? '(head nobody)))
            '(#t #t #f))

     ;; -- an ask reaches the target, the answer routes back -------------

     (define answer-box (box #f))
     (define ticket
       (actors:ask! agent human "Proceed?" '("yes" "no")
                    (lambda (answer) (set-box! answer-box answer))))

     (check 'ask-returns-a-ticket (number? ticket) #t)
     (check 'ask-delivered
            (kernel:mailbox-receive! human-mail)
            (list 'ask ticket agent "Proceed?" '("yes" "no")))
     (check 'ask-is-pending
            (actors:pending human)
            (list (list ticket agent "Proceed?" '("yes" "no"))))

     (check 'answer-routes-back
            (list (actors:answer! ticket "yes") (unbox answer-box))
            '(#t "yes"))
     (check 'answer-clears-pending (actors:pending human) '())
     (check 'stale-ticket-refused (actors:answer! ticket "again") #f)

     ;; -- ordering and cancellation --------------------------------------

     (define t1 (actors:ask! agent human "First?" '() (lambda (a) a)))
     (define t2 (actors:ask! agent human "Second?" '() (lambda (a) a)))
     (check 'oldest-first
            (map caddr (actors:pending human))
            '("First?" "Second?"))
     (check 'cancel-withdraws (actors:cancel! t1) #t)
     (check 'cancel-leaves-the-rest
            (map caddr (actors:pending human))
            '("Second?"))
     (actors:cancel! t2)

     ;; -- unreachable actors ----------------------------------------------

     (check 'unreachable-ask-fails
            (actors:ask! agent '(head gone) "Anyone?" '()
                         (lambda (a) a))
            #f)
     (check 'failed-ask-not-pending (actors:pending '(head gone)) '())

     ;; -- a threaded round trip: agent asks, another thread answers ------

     ;; the cancelled asks' deliveries are still queued: drain them
     (kernel:mailbox-receive! human-mail)
     (kernel:mailbox-receive! human-mail)

     (define replied (box #f))
     (fork-thread
       (lambda ()
         (let ([message (kernel:mailbox-receive! human-mail)])
           (actors:answer! (cadr message) "granted"))))
     (actors:ask! agent human "Escalate?" '("granted" "denied")
                  (lambda (answer) (set-box! replied answer)))
     (let wait ([tries 200])
       (unless (unbox replied)
         (when (zero? tries) (error 'actors-test "no threaded reply"))
         (sleep (make-time 'time-duration 25000000 0))
         (wait (- tries 1))))
     (check 'threaded-round-trip (unbox replied) "granted")

     (format #t "~a actors checks passed\n" checks)))
