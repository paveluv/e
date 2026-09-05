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
         (error 'actor-test (symbol->string label) actual expected)))

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

     ;; A bounded worker harness propagates exceptions and times out if a
     ;; callback was accidentally invoked while holding the protocol lock.
     (define (run-race count task)
       (let ([lock (make-mutex)] [start? #f] [finished 0] [results '()] [failures '()])
         (for-each
           (lambda (index)
             (fork-thread
               (lambda ()
                 (let wait ()
                   (unless (with-mutex lock start?)
                     (sleep (make-time 'time-duration 1000000 0)) (wait)))
                 (guard (ex [else
                              (with-mutex lock
                                (set! failures (cons ex failures))
                                (set! finished (+ finished 1)))])
                   (let ([result (task index)])
                     (with-mutex lock
                       (set! results (cons result results))
                       (set! finished (+ finished 1))))))))
           (iota count))
         (with-mutex lock (set! start? #t))
         (let wait ([tries 1000])
           (unless (with-mutex lock (= finished count))
             (when (zero? tries) (error 'actor-test "worker timeout" finished count))
             (sleep (make-time 'time-duration 5000000 0))
             (wait (- tries 1))))
         (unless (null? failures) (raise (car failures)))
         results))

     (define stress '(head "question-stress"))
     (define counts-lock (make-mutex))
     (define delivered 0)
     (define answers 0)
     (actor:register! stress
       (lambda (message) (with-mutex counts-lock (set! delivered (+ delivered 1)))))
     (define issued
       (run-race 8
         (lambda (index)
           (map (lambda (n)
                  (actor:ask! agent stress (format "~a/~a" index n) '()
                    (lambda (answer) (with-mutex counts-lock (set! answers (+ answers 1))))))
                (iota 300)))))
     (define tickets (list-sort < (apply append issued)))
     (check 'concurrent-asks-are-all-delivered delivered 2400)
     (check 'concurrent-asks-all-remain-pending (length (actor:pending stress)) 2400)
     (check 'concurrent-tickets-are-contiguous
            (+ 1 (- (car (reverse tickets)) (car tickets))) 2400)
     (check 'concurrent-tickets-are-unique
            (let increasing ([rest tickets])
              (or (null? (cdr rest))
                  (and (< (car rest) (cadr rest)) (increasing (cdr rest))))) #t)
     (check 'pending-order-is-ticket-allocation-order
            (map car (actor:pending stress)) tickets)
     (run-race 8
       (lambda (index)
         (for-each (lambda (ticket) (actor:answer! ticket "yes")) (list-ref issued index))))
     (check 'concurrent-answers-are-all-routed answers 2400)
     (check 'concurrent-answers-clear-all-questions (actor:pending stress) '())

     (do ([iteration 0 (+ iteration 1)]) ((= iteration 32))
       (let* ([replies 0]
              [ticket (actor:ask! agent stress "Race?" '()
                        (lambda (answer) (with-mutex counts-lock (set! replies (+ replies 1)))))]
              [outcomes
               (run-race 8
                 (lambda (index)
                   (if (even? index)
                       (cons 'answer (actor:answer! ticket "yes"))
                       (cons 'cancel (actor:cancel! ticket)))))])
         (check 'answer-cancel-has-one-winner (length (filter cdr outcomes)) 1)
         (check 'reply-runs-only-for-the-winning-answer
                replies (length (filter (lambda (outcome) (and (eq? (car outcome) 'answer) (cdr outcome))) outcomes)))
         (check 'raced-ticket-is-consumed (actor:answer! ticket "again") #f)))

     ;; Delivery can synchronously answer; its reply can inspect pending
     ;; state and ask another question. Neither callback runs under the lock.
     (define synchronous '(head "synchronous"))
     (actor:register! synchronous (lambda (message) (actor:answer! (cadr message) "immediate")))
     (define nested #f)
     (define response #f)
     (run-race 1
       (lambda (index)
         (actor:ask! agent synchronous "Now?" '()
           (lambda (answer)
             (set! response (list answer (actor:pending synchronous)))
             (set! nested (actor:ask! agent stress "Next?" '() void))))))
     (check 'synchronous-reply-observes-consumed-question response '("immediate" ()))
     (check 'reply-can-ask-again (map car (actor:pending stress)) (list nested))
     (actor:cancel! nested)
     (check 'reply-question-can-be-cancelled (actor:pending stress) '())

     (define broken '(head "failed-delivery"))
     (actor:register! broken (lambda (message) (error 'delivery "failed")))
     (define kept (actor:ask! agent stress "Keep?" '() void))
     (check 'failed-deliveries-return-false
            (run-race 8 (lambda (index) (actor:ask! agent broken "Fail?" '() void)))
            (make-list 8 #f))
     (check 'failed-deliveries-leave-no-pending-questions (actor:pending broken) '())
     (check 'failed-deliveries-preserve-other-targets (map car (actor:pending stress)) (list kept))
     (actor:cancel! kept)
     (define throwing (actor:ask! agent stress "Throw?" '() (lambda (answer) (error 'reply "failed"))))
     (check 'reply-failure-still-consumes-ticket (actor:answer! throwing "yes") #t)
     (check 'reply-failure-cannot-be-replayed (actor:answer! throwing "again") #f)
     (check 'no-stress-questions-remain (actor:pending stress) '())

     (format #t "~a actor checks passed\n" checks)))
