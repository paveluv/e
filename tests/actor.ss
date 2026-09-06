#!/usr/bin/env scheme-script

;; The interaction protocol: registration, delivery, ask/answer
;; round trips, tickets, and cancellation.  Run from the repository
;; root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (actor) actor:)
             (prefix (kernel) kernel:)
             (prefix (test) test:))

     ;; two mailbox-backed actors
     (define human '(head "test"))
     (define agent '(agent "probe" 1))
     (define human-mail (kernel:make-mailbox))
     (define agent-mail (kernel:make-mailbox))
     (define delivery-context #f)
     (actor:register! human
       (lambda (m)
         (set! delivery-context (actor:current))
         (kernel:mailbox-post! human-mail m)))
     (actor:register! agent (lambda (m) (kernel:mailbox-post! agent-mail m)))

     (test:check 'registered
       (list (actor:registered? human) (actor:registered? agent)
             (actor:registered? '(head nobody)))
       '(#t #t #f))

     ;; Queued messages and replies use the same owned-data contract.
     (test:check 'messages-and-answers-own-their-payloads
       (map
         (lambda (kind)
           (let* ([payload (vector (string-copy "payload") (bytevector 1 2) (list 'original))]
                  [received
                   (if (eq? kind 'send)
                       (begin (actor:send! human payload) (kernel:mailbox-receive! human-mail))
                       (let* ([reply #f]
                              [ticket (actor:ask! agent human "Payload?" '() (lambda (value) (set! reply value)))])
                         (kernel:mailbox-receive! human-mail)
                         (actor:answer! ticket payload)
                         reply))])
             (string-set! (vector-ref payload 0) 0 #\X)
             (bytevector-u8-set! (vector-ref received 1) 0 9)
             (set-car! (vector-ref received 2) 'changed)
             (list kind (vector-ref received 0) (bytevector-u8-ref (vector-ref payload 1) 0)
                   (car (vector-ref payload 2)))))
         '(send answer))
       '((send "payload" 1 original) (answer "payload" 1 original)))
     (let* ([cycle (list 'cycle)] [reply #f]
            [ticket (actor:ask! agent human "Valid?" '() (lambda (value) (set! reply value)))])
       (kernel:mailbox-receive! human-mail)
       (set-cdr! cycle cycle)
       (test:check 'invalid-message-or-answer-is-refused-before-delivery-or-consumption
         (list (map (lambda (payload)
                      (list (test:raises? (lambda () (actor:send! human payload)))
                            (test:raises? (lambda () (actor:answer! ticket payload))))) (list cycle void))
               (map car (actor:pending human)) (actor:answer! ticket 'valid) reply)
         (list '((#t #t) (#t #t)) (list ticket) #t 'valid)))

     ;; Directory records own their data. Admission and every read/callback
     ;; return independent snapshots, including nested capability metadata.
     (define identity (list 'head (string-copy "directory")))
     (define canonical '(head "directory"))
     (define capabilities (vector (string-copy "read") '(buffers source)))
     (define original-delivery (test:recorder))
     (define (ignore-message message) (void))
     (define returned
       (parameterize ([kernel:registering-module 'directory-module])
         (actor:register! identity original-delivery capabilities)))
     (define description (actor:describe canonical))
     (test:check 'directory-shape
       (list (map car (actor:attached)) (list-head description 3)
             (and (integer? (list-ref description 3)) (exact? (list-ref description 3)))
             (list-ref description 4) (actor:describe '(head "absent")))
       (list (list human agent canonical) (list canonical 'head "directory")
             #t '#("read" (buffers source)) #f))
     (string-set! (cadr identity) 0 #\X)
     (string-set! (cadr returned) 0 #\Y)
     (string-set! (vector-ref capabilities 0) 0 #\Z)
     (string-set! (cadar description) 0 #\Q)
     (string-set! (vector-ref (list-ref description 4) 0) 0 #\R)
     (test:check 'directory-data-is-owned
       (let ([entry (actor:describe canonical)])
         (list (list-head entry 3) (list-ref entry 4) (actor:send! canonical 'hello)
               (original-delivery)))
       (list (list canonical 'head "directory") '#("read" (buffers source)) #t '(hello)))
     (define cycle (list 'cycle))
     (set-cdr! cycle cycle)
     (define invalid-registrations
       (list (list '() void) (list '(head) void) (list '(1 "bad") void)
             (list '(head "") void) (list '(head "unused") #f)
             (list '(head "unused") void void) (list '(head "unused") void cycle)))
     (define before-invalid (actor:attached))
     (test:check 'invalid-admission-is-inert
       (list (map (lambda (args) (test:raises? (lambda () (apply actor:register! args)))) invalid-registrations)
             (equal? before-invalid (actor:attached)))
       (list (make-list (length invalid-registrations) #t) #t))
     (test:check 'duplicate-identity-keeps-the-endpoint
       (list (test:raises? (lambda () (actor:register! canonical void)) kernel:registration-conflict?)
             (eq? original-delivery (actor:deliver canonical))) '(#t #t))

     ;; One replacement fixture covers commit, exception, and continuation
     ;; escape. Other threads and presence retain the published endpoint
     ;; until the whole update commits; the initializer sees its own view.
     (for-each
       (lambda (ending)
         (let* ([before (actor:describe canonical)] [events (test:recorder)]
                [token (actor:subscribe! events)] [during #f]
                [result
                 (call/cc
                   (lambda (escape)
                     (guard (ex [else 'abort])
                       (parameterize ([kernel:registering-module 'directory-module])
                         (kernel:call-with-registration-update
                           (lambda ()
                             (kernel:retract-module! 'directory-module)
                             (actor:register! canonical ignore-message 'updated)
                             (set! during
                               (list (list-ref (actor:describe canonical) 4)
                                     ((test:worker (lambda () (actor:describe canonical)))) (events)))
                             (case ending
                               [(abort) (error 'fixture "abort replacement")]
                               [(escape) (escape 'escape)]
                               [else 'commit])))))))])
           (test:check (list 'directory-replacement ending)
             (list result during (list-ref (actor:describe canonical) 4) (events))
             (list ending (list 'updated before '())
                   (if (eq? ending 'commit) 'updated (list-ref before 4))
                   (if (eq? ending 'commit)
                       (list (list (list 'detached canonical) (list 'attached canonical))) '())))
           (actor:unsubscribe! token)))
       '(abort escape commit))

     ;; Detach revokes delivery, not identity-owned questions. Reattaching
     ;; finds the same pending ticket; it is still consumed exactly once.
     (define detach-replies (test:recorder))
     (define waiting (actor:ask! agent canonical "Wait?" '() detach-replies))
     (define detach-events (test:recorder))
     (define detach-watch (actor:subscribe! detach-events))
     (actor:detach! canonical)
     (actor:detach! canonical)
     (define while-detached
       (list (actor:registered? canonical) (actor:send! canonical 'gone)
             (actor:ask! agent canonical "New?" '() void) (map car (actor:pending canonical))))
     (actor:register! canonical ignore-message)
     (test:check 'detach-and-reattach-keep-ticket-lifetime
       (list while-detached (map car (actor:pending canonical))
             (actor:answer! waiting 'yes) (actor:answer! waiting 'again)
             (detach-replies) (detach-events))
       (list (list #f #f #f (list waiting)) (list waiting) #t #f '(yes)
             (list (list (list 'detached canonical)) (list (list 'attached canonical)))))
     (actor:unsubscribe! detach-watch)
     (actor:detach! canonical)

     ;; A paused observer must not block a writer or let the next commit
     ;; overtake it. Revocation skips queued work; a late observer starts
     ;; with future commits. Reentrant replacement is one subsequent batch.
     (define first '(agent "first"))
     (define second '(agent "second"))
     (define third '(agent "third"))
     (define fourth '(agent "fourth"))
     (define (attach who)
       (parameterize ([kernel:registering-module 'presence-writer]) (actor:register! who ignore-message)))
     (define kept-events (test:recorder))
     (define revoked-events (test:recorder))
     (define late-events (test:recorder))
     (define kept-watch (actor:subscribe! kept-events))
     (define revoked-watch
       (parameterize ([kernel:registering-module 'presence-reader]) (actor:subscribe! revoked-events)))
     (define mutating-watch
       (actor:subscribe! (lambda (batch) (string-set! (cadr (cadar batch)) 0 #\X))))
     (define presence-ready (test:gate))
     (define presence-release (test:gate))
     (define blocker-watch
       (actor:subscribe!
         (lambda (batch)
           (cond
             [(equal? batch (list (list 'attached first)))
              (presence-ready #t) (test:await 'presence-release presence-release)]
             [(equal? batch (list (list 'attached third)))
              (kernel:call-with-registration-update
                (lambda () (actor:detach! second) (actor:register! fourth ignore-message)))]))))
     (define attach-worker (test:worker (lambda () (attach first))))
     (test:await 'presence-ready presence-ready)
     ((test:worker (lambda () (attach second) (kernel:retract-module! 'presence-reader))))
     (define late-watch (actor:subscribe! late-events))
     (test:check 'presence-callbacks-stay-serialized
       (list (actor:registered? first) (actor:registered? second) (kept-events)) '(#t #t ()))
     (presence-release #t)
     (attach-worker)
     ((test:worker (lambda () (attach third))))
     (define subsequent
       (list (list (list 'attached third)) (list (list 'detached second) (list 'attached fourth))))
     (test:check 'presence-order-revocation-and-snapshot-isolation
       (list (kept-events) (revoked-events) (late-events))
       (list (append (list (list (list 'attached first)) (list (list 'attached second))) subsequent)
             '() subsequent))
     (kernel:retract-module! 'presence-writer)
     (test:check 'retraction-covers-directory-and-delivery
       (list (map actor:registered? (list first second third fourth))
             (car (reverse (kept-events))))
       (list '(#f #f #f #t) (list (list 'detached first) (list 'detached third))))
     (for-each actor:unsubscribe! (list kept-watch revoked-watch mutating-watch blocker-watch late-watch))
     (actor:detach! fourth)

     ;; The execution context is scoped, copied, and local to each thread.
     ;; An exception or escape restores it just like an ordinary return.
     (for-each
       (lambda (ending)
         (test:check (list 'actor-context ending)
           (list
             (actor:call-as human
               (lambda ()
                 (call/cc
                   (lambda (escape)
                     (guard (ex [else (void)])
                       (actor:call-as agent
                         (lambda ()
                           (case ending
                             [(abort) (error 'fixture "abort actor work")]
                             [(escape) (escape #f)]))))))
                 (actor:current)))
             (actor:current))
           (list human #f)))
       '(return abort escape))
     (define context-ready (test:gate))
     (define context-release (test:gate))
     (test:check 'actor-context-is-copied-and-thread-local
       (actor:call-as human
         (lambda ()
           (let ([worker
                  (test:worker
                    (lambda ()
                      (let ([identity (list 'agent (string-copy "worker"))])
                        (actor:call-as identity
                          (lambda ()
                            (string-set! (cadr identity) 0 #\X)
                            (string-set! (cadr (actor:current)) 0 #\Y)
                            (context-ready #t)
                            (test:await 'context-release context-release)
                            (actor:current))))))])
             (test:await 'context-ready context-ready)
             (let ([during (actor:current)])
               (context-release #t)
               (list during (worker) (actor:current))))))
       (list human '(agent "worker") human))

     ;; Admission, delivered messages, and pending reads own separate data.
     ;; The receiver runs as itself; the reply runs as the snapshotted asker,
     ;; even when the asking and answering threads have another context.
     (define answer-box (box #f))
     (define from (list 'agent (string-copy "probe") 1))
     (define to (list 'head (string-copy "test")))
     (define question (string-copy "Proceed?"))
     (define choices (map string-copy '("yes" "no")))
     (define ticket
       (actor:call-as human
         (lambda ()
           (actor:ask! from to question choices
             (lambda (answer) (set-box! answer-box (list answer (actor:current))))))))
     (define delivered-question (kernel:mailbox-receive! human-mail))
     (define pending-question (car (actor:pending human)))
     (for-each (lambda (text) (string-set! text 0 #\X))
               (list (cadr from) (cadr to) question (car choices)))
     (test:check 'question-admission-and-delivery-context
       (list (number? ticket) delivery-context delivered-question pending-question (actor:current))
       (list #t human (list 'ask ticket agent "Proceed?" '("yes" "no"))
             (list ticket agent "Proceed?" '("yes" "no")) #f))
     (for-each (lambda (text) (string-set! text 0 #\Y))
               (list (cadr (caddr delivered-question)) (cadddr delivered-question)
                     (car (list-ref delivered-question 4))
                     (cadr (cadr pending-question)) (caddr pending-question)
                     (car (cadddr pending-question))))
     (test:check 'question-reads-and-reply-context
       (list (actor:pending human)
             (actor:call-as human
               (lambda () (list (actor:answer! ticket "yes") (actor:current))))
             (unbox answer-box) (actor:pending human) (actor:answer! ticket "again") (actor:current))
       (list (list (list ticket agent "Proceed?" '("yes" "no")))
             (list #t human) (list "yes" agent) '() #f #f))

     ;; -- ordering and cancellation --------------------------------------

     (define t1 (actor:ask! agent human "First?" '() (lambda (a) a)))
     (define t2 (actor:ask! agent human "Second?" '() (lambda (a) a)))
     (test:check 'oldest-first
       (map caddr (actor:pending human))
       '("First?" "Second?"))
     (test:check 'cancel-withdraws (actor:cancel! t1) #t)
     (test:check 'cancel-leaves-the-rest
       (map caddr (actor:pending human))
       '("Second?"))
     (actor:cancel! t2)

     ;; -- unreachable actors ----------------------------------------------

     (test:check 'unreachable-ask-fails
       (actor:ask! agent '(head gone) "Anyone?" '()
                   (lambda (a) a))
       #f)
     (test:check 'failed-ask-not-pending (actor:pending '(head gone)) '())

     ;; -- a threaded round trip: agent asks, another thread answers ------

     ;; the cancelled asks' deliveries are still queued: drain them
     (kernel:mailbox-receive! human-mail)
     (kernel:mailbox-receive! human-mail)

     (define replied (test:gate))
     (define answer-worker
       (test:worker
         (lambda ()
           (let ([message (kernel:mailbox-receive! human-mail)])
             (actor:answer! (cadr message) "granted")))))
     (actor:ask! agent human "Escalate?" '("granted" "denied")
                 replied)
     (answer-worker)
     (test:check 'threaded-round-trip (replied) "granted")

     (define stress '(head "question-stress"))
     (define counts-lock (make-mutex))
     (define delivered 0)
     (define answers 0)
     (actor:register! stress
       (lambda (message) (with-mutex counts-lock (set! delivered (+ delivered 1)))))
     (define issued
       (test:parallel 8
         (lambda (index)
           (map (lambda (n)
                  (actor:ask! agent stress (format "~a/~a" index n) '()
                    (lambda (answer) (with-mutex counts-lock (set! answers (+ answers 1))))))
                (iota 300)))))
     (define tickets (list-sort < (apply append issued)))
     (test:check 'concurrent-asks-are-all-delivered delivered 2400)
     (test:check 'concurrent-asks-all-remain-pending (length (actor:pending stress)) 2400)
     (test:check 'concurrent-tickets-are-contiguous
       (+ 1 (- (car (reverse tickets)) (car tickets))) 2400)
     (test:check 'concurrent-tickets-are-unique
       (let increasing ([rest tickets])
         (or (null? (cdr rest))
             (and (< (car rest) (cadr rest)) (increasing (cdr rest))))) #t)
     (test:check 'pending-order-is-ticket-allocation-order
       (map car (actor:pending stress)) tickets)
     (test:parallel 8
       (lambda (index)
         (for-each (lambda (ticket) (actor:answer! ticket "yes")) (list-ref issued index))))
     (test:check 'concurrent-answers-are-all-routed answers 2400)
     (test:check 'concurrent-answers-clear-all-questions (actor:pending stress) '())

     (test:check 'answer-cancel-races
       (map
         (lambda (iteration)
           (let* ([replies 0]
                  [ticket (actor:ask! agent stress "Race?" '()
                            (lambda (answer) (with-mutex counts-lock (set! replies (+ replies 1)))))]
                  [outcomes
                   (test:parallel 8
                     (lambda (index)
                       (if (even? index)
                         (cons 'answer (actor:answer! ticket "yes"))
                         (cons 'cancel (actor:cancel! ticket)))))])
             (list (length (filter cdr outcomes))
                   (= replies (length (filter (lambda (outcome) (and (eq? (car outcome) 'answer) (cdr outcome))) outcomes)))
                   (actor:answer! ticket "again"))))
         (iota 32))
       (make-list 32 '(1 #t #f)))

     ;; Delivery can synchronously answer; its reply can inspect pending
     ;; state and ask another question. Neither callback runs under the lock.
     (define synchronous '(head "synchronous"))
     (actor:register! synchronous (lambda (message) (actor:answer! (cadr message) "immediate")))
     (define nested #f)
     (define response #f)
     (test:parallel 1
       (lambda (index)
         (actor:ask! agent synchronous "Now?" '()
           (lambda (answer)
             (set! response (list answer (actor:pending synchronous)))
             (set! nested (actor:ask! agent stress "Next?" '() void))))))
     (test:check 'synchronous-reply-observes-consumed-question response '("immediate" ()))
     (test:check 'reply-can-ask-again (map car (actor:pending stress)) (list nested))
     (actor:cancel! nested)
     (test:check 'reply-question-can-be-cancelled (actor:pending stress) '())

     (define broken '(head "failed-delivery"))
     (actor:register! broken (lambda (message) (error 'delivery "failed")))
     (define kept (actor:ask! agent stress "Keep?" '() void))
     (test:check 'failed-deliveries-return-false
       (test:parallel 8 (lambda (index) (actor:ask! agent broken "Fail?" '() void)))
       (make-list 8 #f))
     (test:check 'failed-deliveries-leave-no-pending-questions (actor:pending broken) '())
     (test:check 'failed-deliveries-preserve-other-targets (map car (actor:pending stress)) (list kept))
     (actor:cancel! kept)
     (define throwing (actor:ask! agent stress "Throw?" '() (lambda (answer) (error 'reply "failed"))))
     (test:check 'reply-failure-still-consumes-ticket (actor:answer! throwing "yes") #t)
     (test:check 'reply-failure-cannot-be-replayed (actor:answer! throwing "again") #f)
     (test:check 'no-stress-questions-remain (actor:pending stress) '())

     (test:finish! 'actor)))
