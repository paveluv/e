#!/usr/bin/env scheme-script

;; The interaction protocol: registration, delivery, ask/answer
;; round trips, tickets, and cancellation.  Run from the repository
;; root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (actor) actor:)
             (prefix (kernel) kernel:)
             (only (chezscheme) box unbox set-box! fork-thread
                   make-time sleep))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'actor-test label actual expected)))

     ;; two mailbox-backed actors
     (define human '(head test))
     (define agent '(agent probe 1))
     (define human-mail (kernel:make-mailbox))
     (define agent-mail (kernel:make-mailbox))
     (actor:register! human (lambda (m) (kernel:mailbox-post! human-mail m)))
     (actor:register! agent (lambda (m) (kernel:mailbox-post! agent-mail m)))

     (check 'registered
            (list (actor:unregister? human) (actor:unregister? agent)
                  (actor:unregister? '(head nobody)))
            '(#t #t #f))

     ;; -- an ask reaches the target, the answer routes back -------------

     (define answer-box (box #f))
     (define ticket
       (actor:ask! agent human "Proceed?" '("yes" "no")
                    (lambda (answer) (set-box! answer-box answer))))

     (check 'ask-returns-a-ticket (number? ticket) #t)
     (check 'ask-delivered
            (kernel:mailbox-receive! human-mail)
            (list 'ask ticket agent "Proceed?" '("yes" "no")))
     (check 'ask-is-pending
            (actor:pending human)
            (list (list ticket agent "Proceed?" '("yes" "no"))))

     (check 'answer-routes-back
            (list (actor:answer! ticket "yes") (unbox answer-box))
            '(#t "yes"))
     (check 'answer-clears-pending (actor:pending human) '())
     (check 'stale-ticket-refused (actor:answer! ticket "again") #f)

     ;; -- ordering and cancellation --------------------------------------

     (define t1 (actor:ask! agent human "First?" '() (lambda (a) a)))
     (define t2 (actor:ask! agent human "Second?" '() (lambda (a) a)))
     (check 'oldest-first
            (map caddr (actor:pending human))
            '("First?" "Second?"))
     (check 'cancel-withdraws (actor:cancel! t1) #t)
     (check 'cancel-leaves-the-rest
            (map caddr (actor:pending human))
            '("Second?"))
     (actor:cancel! t2)

     ;; -- unreachable actors ----------------------------------------------

     (check 'unreachable-ask-fails
            (actor:ask! agent '(head gone) "Anyone?" '()
                         (lambda (a) a))
            #f)
     (check 'failed-ask-not-pending (actor:pending '(head gone)) '())

     ;; -- a threaded round trip: agent asks, another thread answers ------

     ;; the cancelled asks' deliveries are still queued: drain them
     (kernel:mailbox-receive! human-mail)
     (kernel:mailbox-receive! human-mail)

     (define replied (box #f))
     (fork-thread
       (lambda ()
         (let ([message (kernel:mailbox-receive! human-mail)])
           (actor:answer! (cadr message) "granted"))))
     (actor:ask! agent human "Escalate?" '("granted" "denied")
                  (lambda (answer) (set-box! replied answer)))
     (let wait ([tries 200])
       (unless (unbox replied)
         (when (zero? tries) (error 'actor-test "no threaded reply"))
         (sleep (make-time 'time-duration 25000000 0))
         (wait (- tries 1))))
     (check 'threaded-round-trip (unbox replied) "granted")

     (format #t "~a actor checks passed\n" checks)))
