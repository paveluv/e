#!/usr/bin/env scheme-script

;; Capability minting and budgets: sessions curried with an actor's
;; identity, fueled evaluation and buffer permissions, the
;; escalation path, revocation, and the audit trail.  Run from the
;; repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (service policy) policy:)
             (prefix (state store) store:)
             (prefix (foundation text) text:)
             (prefix (state actor) actor:)
             (prefix (core kernel) kernel:)
             (prefix (service log) log:)
             (prefix (test) test:))

     (define check test:check)

     ;; an owner head with a mailbox, and a buffer to work on
     (define owner '(head "test"))
     (define (from-owner thunk)
       (actor:call-as owner
         (lambda ()
           (call-with-values thunk
             (lambda results
               (unless (equal? (actor:current) owner) (error 'policy-test "caller actor leaked"))
               (apply values results))))))
     (define owner-mail (kernel:make-mailbox))
     (define pending-notices (test:recorder))
     (actor:register! owner
       (lambda (m)
         (if (equal? m '(pending))
           (pending-notices ((test:worker (lambda () (map car (actor:pending owner))))))
           (kernel:mailbox-post! owner-mail m))))
     (define agent '(agent "helper" 1))
     (define notes (store:create! owner "notes" '("one" "two")))
     (define secret (store:create! owner "secret" '("hidden")))

     ;; Observe the shared log instead of replacing the policy's audit sink.
     (define observed (test:recorder))
     (define audit-subscription
       (log:subscribe!
         (lambda (record presentation)
           (when (eq? (log:component record) 'policy)
             (observed (list (log:datum record) presentation))))))

     (define actor-input (list 'agent (string-copy "helper") 1))
     (define owner-input (list 'head (string-copy "test")))
     (define buffers-input (list (string-copy "notes")))
     (define permissions (policy:make 'all 100000000 buffers-input 4000))
     (define s (policy:mint! actor-input permissions owner-input))
     (string-set! (cadr actor-input) 0 #\X)
     (string-set! (cadr owner-input) 0 #\X)
     (set-car! buffers-input "secret")
     (set-car! (policy:buffers permissions) "secret")
     (string-set! (cadr (policy:session-actor s)) 0 #\Y)
     (string-set! (cadr (policy:session-owner s)) 0 #\Y)
     (let ([row (car (policy:sessions))])
       (set-car! (car row) 'rewritten)
       (string-set! (cadadr row) 0 #\Z))

     (check 'minted-metadata-and-permissions-own-inputs-and-queries
       (list (policy:session? s) (policy:sessions) (policy:buffers permissions)
             (map log:datum (log:entries 'policy)))
       (list #t (list (list agent owner)) '("notes") (list (list 'mint agent owner))))

     ;; -- fueled evaluation in the granted environment -----------------

     (for-each
       (lambda (example)
         (check (list 'evaluation (car example))
           (from-owner (lambda () (policy:session-eval! s (car example)))) (cadr example)))
       '(("(+ 1 2)" (ok . "=> 3"))
         ((* 2 3) (ok . "=> 6"))
         ("(display \"hi\") (+ 1 2)" (ok . "=> 3\noutput:\nhi"))
         ("(values 1 2)" (ok . "=> 1, 2"))
         ("'#0=#(#0#)" (ok . "=> #0=#(#0#)"))
         ("(buffer-text-line \"notes\" 0)" (ok . "=> \"one\""))
         (#f (ok . "=> #f"))
         ("#f" (ok . "=> #f"))
         ("" (error . "an empty expression"))
         (" ; comment only\n" (error . "an empty expression"))
         ("(" (error . "unreadable expression"))))
     (check 'evaluation-failures
       (map (lambda (form) (car (policy:session-eval! s form)))
         '("(delete-file \"x\")" "(car '())" malformed "malformed"))
       '(unbound error unbound unbound))

     ;; a tiny fuel tank: the loop cannot hang anything
     (define grants-input (list '+ 'car 'cons 'quote 'let 'lambda 'if 'make-vector))
     (define narrow (policy:make grants-input 10000 '() 4000))
     (set-car! grants-input 'delete-file)
     (set-car! (policy:grants narrow) 'delete-file)
     (define winded (policy:mint! '(agent winded) narrow owner))
     (check 'evaluation-and-result-formatting-share-the-fuel
       (map (lambda (form)
              (actor:call-as owner
                (lambda () (list (policy:session-eval! winded form) (actor:current)))))
         '("(let loop () (loop))" "(make-vector 100000 #f)"))
       (make-list 2 (list '(fuel . "the evaluation ran out of fuel (an infinite loop?)") owner)))

     ;; -- a narrowed grant subsets the sandbox -------------------------

     (check 'narrow-grant-works
            (policy:session-eval! winded "(+ 1 2)") '(ok . "=> 3"))
     (check 'narrow-grant-excludes
            (car (policy:session-eval! winded "(buffer-names)"))
            'unbound)

     ;; -- attributed edits and buffer permissions -----------------------

     (define (mutation-result thunk)
       (let-values ([(status detail) (from-owner thunk)])
         (list status (if (eq? status 'applied) #t detail))))

     (define (try-edit! session id line)
       (mutation-result
         (lambda ()
           (policy:session-edit! session id (store:revision id)
             (text:make-span 0 0 0 0) (list line)))))

     (define (try-history! session id direction)
       (mutation-result
         (lambda () ((if (eq? direction 'undo) policy:session-undo! policy:session-redo!) session id))))

     (define (try-writes! session id)
       (cons (try-edit! session id "bad")
             (map (lambda (direction) (try-history! session id direction)) '(undo redo))))

     (define input-span (text:make-span 0 0 0 0))
     (define input-lines (map string-copy '("zero" "middle" "")))
     (let-values ([(status receipt)
                   (from-owner
                     (lambda () (policy:session-edit! s notes 0 input-span input-lines)))])
       ;; Pure text/store clients promise immutable lines and coordinates;
       ;; a session must own them just as it owns context and reply data.
       (set-car! (text:span-start input-span) 1)
       (string-set! (cadr input-lines) 0 #\X)
       (set-car! input-lines "changed")
       (string-set! (vector-ref (cadr receipt) 0) 0 #\X)
       (let* ([change (car (caddr receipt))] [delta (caddr change)])
         (string-set! (cadr (cadr change)) 0 #\Y)
         (string-set! (caaddr delta) 0 #\Z))
       (check 'edit-boundary-owns-inputs-and-receipt
         (list status (car receipt) (let-values ([(lines revision) (store:snapshot notes)]) lines)
               (list? (caddr (car (caddr receipt)))))
         '(applied 1 #("zero" "middle" "one" "two") #t)))
     (check 'edit-is-attributed
            (cadr (car (store:history notes))) agent)
     (let* ([undo (try-history! s notes 'undo)]
            [restored (store:line notes 0)]
            [redo (try-history! s notes 'redo)]
            [redone (store:line notes 0)]
            [undone (try-history! s notes 'undo)]
            [again (try-edit! s notes "again ")]
            [more (try-edit! s notes "more ")])
       (check 'edits-continue-after-undo-and-redo
         (list undo restored redo redone undone again more (store:line notes 0))
         '((applied #t) "one" (applied #t) "zero" (applied #t)
           (applied #t) (applied #t) "more again one")))
     (check 'allowlist-gates-all-mutations
       (list (try-writes! s secret) (store:line secret 0))
       '(((refused buffer) (refused buffer) (refused buffer)) "hidden"))
     (let ([reader (policy:mint! '(agent reader) (policy:reader) owner)])
       (check 'reader-has-no-write-permission
         (list (policy:session-eval! reader "(buffer-text-line \"notes\" 1)")
               (try-writes! reader notes))
         '((ok . "=> \"two\"") ((refused buffer) (refused buffer) (refused buffer))))
       (policy:revoke! reader))

     (store:set-property! owner notes 'read-only #t)
     (let* ([before (call-with-values (lambda () (store:snapshot-state notes)) list)]
            [results (try-writes! s notes)]
            [bypass (mutation-result
                      (lambda () (policy:session-edit! s notes (store:revision notes)
                                   (text:make-span 0 0 0 0) '("bad")
                                   '(bypass "bypass" ((read-only . #f))))))])
       (store:rename! owner notes "renamed")
       (check 'current-read-only-and-name-guard-the-entire-session-mutation
         (list results bypass (try-writes! s notes)
               (equal? before (call-with-values (lambda () (store:snapshot-state notes)) list)))
         '(((refused read-only) (refused read-only) (refused read-only)) (refused read-only)
           ((refused buffer) (refused buffer) (refused buffer)) #t)))
     (store:rename! owner notes "notes")
     (store:set-property! owner notes 'read-only #f)

     ;; -- the escalation path: the session asks its owner --------------

     (define got (box #f))
     (define ticket
       (from-owner
         (lambda () (policy:session-ask! s "May I edit secret?" '("yes" "no")
                      (lambda (answer) (set-box! got answer))))))
     (check 'ask-reaches-the-owner
            (kernel:mailbox-receive! owner-mail)
            (list 'ask ticket agent "May I edit secret?" '("yes" "no")))
     (actor:answer! ticket "no")
     (check 'answer-routes-back (unbox got) "no")

     (let* ([peer '(agent "reviewer")]
            [peer-mail (kernel:make-mailbox)]
            [reviewer (policy:mint! peer (policy:reader) owner)])
       (actor:register! peer (lambda (message) (kernel:mailbox-post! peer-mail message)))
       (let* ([sent? (from-owner (lambda () (policy:session-send! s peer '(answer 1 forged))))]
              [message (kernel:mailbox-receive! peer-mail)]
              [reply #f]
              [question (policy:session-ask! s peer "Review?" '() (lambda (value) (set! reply value)))]
              [delivered (kernel:mailbox-receive! peer-mail)])
         (check 'session-mail-and-questions-bind-the-sender-and-recipient
           (list sent? message delivered
                 (policy:session-answer! s question 'wrong-recipient)
                 (policy:session-cancel! reviewer question)
                 (policy:session-answer! reviewer question 'reviewed)
                 (policy:session-cancel! s question) reply
                 (test:raises? (lambda () (policy:session-ask! s "Bad callback?" '() #f))))
           (list #t (list 'message agent '(answer 1 forged)) (list 'ask question agent "Review?" '())
                 #f #f #t #f 'reviewed #t)))
       (policy:revoke! reviewer)
       (actor:detach! peer))

     ;; -- revocation ----------------------------------------------------

     (define revoked-tickets
       (map (lambda (question)
              (let ([ticket (policy:session-ask! s question '() (lambda (answer) (set-box! got answer)))])
                (kernel:mailbox-receive! owner-mail) ticket))
         '("Withdraw on revoke?" "Withdraw together?")))
     (define revoked-ticket (car revoked-tickets))
     (define sibling (policy:mint! agent (policy:reader) owner))
     (define sibling-ticket (policy:session-ask! sibling "Keep this session?" '() void))
     (kernel:mailbox-receive! owner-mail)
     (define removal-offset (length (pending-notices)))
     (define foreign-cancel (policy:session-cancel! sibling revoked-ticket))
     (check 'revoke (policy:revoke! s) #t)
     (check 'revoke-withdraws-only-its-session-before-a-late-answer
       (list foreign-cancel (map car (actor:pending owner))
             (actor:answer! revoked-ticket "too late") (unbox got)
             (policy:session-cancel! sibling sibling-ticket) (actor:pending owner)
             (list-tail (pending-notices) removal-offset))
       (list #f (list sibling-ticket) #f "no" #t '() (list (list sibling-ticket) '())))
     (policy:revoke! sibling)
     (check 'all-revoked-entry-points-refuse
       (list (car (policy:session-eval! s "(+ 1 2)"))
             (try-writes! s notes)
             (policy:session-ask! s "Anyone?" '() (lambda (a) a))
             (policy:session-send! s owner 'forbidden)
             (policy:session-answer! s sibling-ticket 'forbidden)
             (policy:session-cancel! s revoked-ticket)
             (assoc agent (policy:sessions)))
       '(refused ((refused revoked) (refused revoked) (refused revoked)) #f #f #f #f #f))

     ;; Revoke while delivery is already running. Its late answer cannot
     ;; invoke a revoked continuation or leave a pending ticket behind.
     (let* ([who '(agent "revocation race")]
            [racing (policy:mint! who (policy:reader) owner)]
            [ready (test:gate)] [release (test:gate)] [late #f] [reply #f])
       (actor:register! who
         (lambda (message)
           (when (eq? (car message) 'ask)
             (ready #t) (test:await 'release-question-delivery release)
             (set! late (actor:answer! (cadr message) 'late)))))
       (let ([work (test:worker
                     (lambda () (policy:session-ask! racing who "Already admitted?" '()
                                  (lambda (answer) (set! reply answer)))))])
         (test:await 'question-delivery-started ready)
         (policy:revoke! racing)
         (release #t)
         (check 'revocation-during-question-delivery-keeps-no-continuation
           (list (number? (work)) late reply (actor:pending who)) '(#t #f #f ())))
       (actor:detach! who))

     ;; Select private sessions from one inventory version. Cleanup can
     ;; inspect that inventory, fail, or mint a same-name replacement; it
     ;; still runs once and cannot pull the replacement into the selection.
     (let* ([who '(agent "selected")]
            [closed 0] [replacement #f]
            [first (policy:mint! who (policy:reader) owner
                     (lambda ()
                       (set! closed (+ closed 1))
                       (policy:sessions)
                       (set! replacement (policy:mint! who (policy:reader) owner))))]
            [second (policy:mint! who (policy:reader) owner
                      (lambda () (set! closed (+ closed 1)) (error 'close "fixture failure")))]
            [selected ((test:worker (lambda () (from-owner (lambda () (policy:revoke-actor! who))))))])
       (policy:revoke! first)
       (policy:revoke! second)
       (check 'actor-revocation-keeps-handles-private-and-does-not-chase-replacements
         (list selected closed (map policy:revoked? (list first second replacement))
               (policy:session-eval! replacement #f)
               (policy:revoke-actor! who) (policy:revoke-actor! who)
               (test:raises? (lambda () (policy:revoke-actor! 'invalid))))
         '(2 2 (#t #t #f) (ok . "=> #f") 1 0 #t)))

     ;; The implicit owner is the initiating actor, including a named head,
     ;; or #f for headless callers. The default audit trail stays persistent.
     (for-each
       (lambda (context)
         (check 'default-owner-follows-actor-context
           (actor:call-as context
             (lambda ()
               (let* ([quiet (policy:mint! '(agent quiet) (policy:make '(+) 10000 '() 4000))]
                      [result (policy:session-eval! quiet "(+ 1 1)")])
                 (policy:revoke! quiet)
                 (list (policy:session-owner quiet) result (actor:current)
                       (log:actor (car (log:entries 'policy)))))))
           (list context '(ok . "=> 2") context (or context '(base e)))))
       '(#f (head "writing desk") (agent requester)))
     (let* ([records (log:entries 'policy)]
            [events (reverse (observed))])
       (set-car! (log:datum (car records)) 'rewritten)
       (check 'one-quiet-owned-audit-stream
         (list (map (lambda (record) (list (log:datum record) #f)) (log:entries 'policy))
               (map (lambda (kind) (and (assq kind (map car events)) #t))
                 '(mint eval edit undo redo ask revoke revoke-error))
               (for-all (lambda (record)
                          (let ([event (log:datum record)])
                            (if (memq (car event) '(mint revoke revoke-error)) #t
                                (equal? (log:actor record) (cadr event))))) (log:entries 'policy)))
         (list events '(#t #t #t #t #t #t #t #t) #t)))
     (log:unsubscribe! audit-subscription)
     (policy:revoke! winded)
     ;; Overlapping connection admission/teardown must retain every live
     ;; session. Inventory readers and audit callbacks never share its lock.
     (let* ([p (policy:reader)]
            [created (test:parallel 12
                       (lambda (i) (policy:mint! (list 'agent 'old i) p owner)))]
            [reading (log:subscribe! (lambda (record presentation) (policy:sessions)))]
            [replacement
             (test:parallel 12
               (lambda (i)
                 (policy:revoke! (list-ref created i))
                 (policy:mint! (list 'agent 'new i) p owner)))])
       (check 'concurrent-mint-revoke-keeps-exact-owned-inventory
         (let ([rows (policy:sessions)])
           (list (= (length rows) 12)
                 (for-all (lambda (s) (and (member (list (policy:session-actor s) owner) rows) #t)) replacement)
                 (for-all policy:revoked? created))) '(#t #t #t))
       (let ([before (length (filter (lambda (r) (eq? (car (log:datum r)) 'revoke)) (log:entries 'policy)))])
         (test:parallel 24 (lambda (i) (policy:revoke! (list-ref replacement (mod i 12)))))
         (check 'repeated-revocation-removes-and-audits-once
           (list (policy:sessions)
                 (- (length (filter (lambda (r) (eq? (car (log:datum r)) 'revoke)) (log:entries 'policy))) before))
           '(() 12)))
       (log:unsubscribe! reading))
     (store:delete! owner notes)
     (store:delete! owner secret)
     (test:finish! 'policy)))
