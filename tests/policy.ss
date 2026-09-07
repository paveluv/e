#!/usr/bin/env scheme-script

;; Capability minting and budgets: sessions curried with an actor's
;; identity, fueled evaluation and buffer permissions, the
;; escalation path, revocation, and the audit trail.  Run from the
;; repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (policy) policy:)
             (prefix (store) store:)
             (prefix (text) text:)
             (prefix (actor) actor:)
             (prefix (kernel) kernel:)
             (prefix (log) log:)
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
     (actor:register! owner
                      (lambda (m) (kernel:mailbox-post! owner-mail m)))
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
         ("(buffer-text-line \"notes\" 0)" (ok . "=> \"one\""))
         ("" (error . "an empty expression"))))
     (check 'evaluation-failures
       (map (lambda (form) (car (policy:session-eval! s form)))
         '("(delete-file \"x\")" "(car '())"))
       '(unbound error))

     ;; a tiny fuel tank: the loop cannot hang anything
     (define grants-input (list '+ 'car 'cons 'quote 'let 'lambda 'if))
     (define narrow (policy:make grants-input 10000 '() 4000))
     (set-car! grants-input 'delete-file)
     (set-car! (policy:grants narrow) 'delete-file)
     (define winded (policy:mint! '(agent winded) narrow owner))
     (check 'fuel-runs-out
            (actor:call-as owner
              (lambda ()
                (let ([result (policy:session-eval! winded "(let loop () (loop))")])
                  (list result (actor:current)))))
            (list '(fuel . "the evaluation ran out of fuel (an infinite loop?)") owner))

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

     ;; -- revocation ----------------------------------------------------

     (check 'revoke (policy:revoke! s) #t)
     (check 'all-revoked-entry-points-refuse
       (list (car (policy:session-eval! s "(+ 1 2)"))
             (try-writes! s notes)
             (policy:session-ask! s "Anyone?" '() (lambda (a) a))
             (assoc agent (policy:sessions)))
       '(refused ((refused revoked) (refused revoked) (refused revoked)) #f #f))

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
                 '(mint eval edit undo redo ask revoke))
               (for-all (lambda (record)
                          (let ([event (log:datum record)])
                            (if (memq (car event) '(mint revoke)) #t
                                (equal? (log:actor record) (cadr event))))) (log:entries 'policy)))
         (list events '(#t #t #t #t #t #t #t) #t)))
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
