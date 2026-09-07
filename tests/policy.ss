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
     (define owner '(head test))
     (define owner-mail (kernel:make-mailbox))
     (actor:register! owner
                      (lambda (m) (kernel:mailbox-post! owner-mail m)))
     (define agent '(agent helper 1))
     (define notes (store:create! owner "notes" '("one" "two")))
     (define secret (store:create! owner "secret" '("hidden")))

     ;; Observe the shared log instead of replacing the policy's audit sink.
     (define observed (test:recorder))
     (define audit-subscription
       (log:subscribe!
         (lambda (record presentation)
           (when (eq? (log:component record) 'policy)
             (observed (list (log:datum record) presentation))))))

     (define s (policy:mint!
                 agent
                 (policy:make 'all 100000000 '("notes") 4000)
                 owner))

     (check 'minted-listed-and-audited-once
       (list (policy:session? s) (policy:sessions)
             (map log:datum (log:entries 'policy)))
       (list #t (list (list agent owner)) (list (list 'mint agent owner))))

     ;; -- fueled evaluation in the granted environment -----------------

     (for-each
       (lambda (example)
         (check (list 'evaluation (car example))
           (policy:session-eval! s (car example)) (cadr example)))
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
     (define winded (policy:mint!
                      '(agent winded)
                      (policy:make '(+ car cons quote let lambda if)
                                   10000 '() 4000)
                      owner))
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
       (let-values ([(status detail) (thunk)])
         (list status (if (number? detail) #t detail))))

     (define (try-edit! session id line)
       (mutation-result
         (lambda ()
           (policy:session-edit! session id (store:revision id)
             (text:make-span 0 0 0 0) (list line)))))

     (define (try-undo! session id)
       (mutation-result (lambda () (policy:session-undo! session id))))

     (check 'edit-applies (try-edit! s notes "zero ") '(applied #t))
     (check 'edit-is-attributed
            (cadr (car (store:history notes))) agent)
     (let* ([undo (try-undo! s notes)]
            [restored (store:line notes 0)]
            [again (try-edit! s notes "again ")]
            [more (try-edit! s notes "more ")])
       (check 'edits-continue-after-undo
         (list undo restored again more (store:line notes 0))
         '((applied #t) "one" (applied #t) (applied #t) "more again one")))
     (check 'allowlist-gates-edits-and-undo
       (list (try-edit! s secret "leak ") (try-undo! s secret)
             (store:line secret 0))
       '((refused buffer) (refused buffer) "hidden"))
     (let ([reader (policy:mint! '(agent reader) (policy:reader) owner)])
       (check 'reader-has-no-write-permission
         (list (policy:session-eval! reader "(buffer-text-line \"notes\" 1)")
               (try-edit! reader notes "x") (try-undo! reader notes))
         '((ok . "=> \"two\"") (refused buffer) (refused buffer)))
       (policy:revoke! reader))

     ;; -- the escalation path: the session asks its owner --------------

     (define got (box #f))
     (define ticket
       (policy:session-ask! s "May I edit secret?" '("yes" "no")
                            (lambda (answer) (set-box! got answer))))
     (check 'ask-reaches-the-owner
            (kernel:mailbox-receive! owner-mail)
            (list 'ask ticket agent "May I edit secret?" '("yes" "no")))
     (actor:answer! ticket "no")
     (check 'answer-routes-back (unbox got) "no")

     ;; -- revocation ----------------------------------------------------

     (check 'revoke (policy:revoke! s) #t)
     (check 'all-revoked-entry-points-refuse
       (list (car (policy:session-eval! s "(+ 1 2)"))
             (try-edit! s notes "x") (try-undo! s notes)
             (policy:session-ask! s "Anyone?" '() (lambda (a) a))
             (assoc agent (policy:sessions)))
       '(refused (refused revoked) (refused revoked) #f #f))

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
                 (list (policy:session-owner quiet) result (actor:current)))))
           (list context '(ok . "=> 2") context)))
       '(#f (head "writing desk") (agent requester)))
     (let* ([records (log:entries 'policy)]
            [events (reverse (observed))])
       (set-car! (log:datum (car records)) 'rewritten)
       (check 'one-quiet-owned-audit-stream
         (list (map (lambda (record) (list (log:datum record) #f)) (log:entries 'policy))
               (map (lambda (kind) (and (assq kind (map car events)) #t))
                 '(mint eval edit undo ask revoke)))
         (list events '(#t #t #t #t #t #t))))
     (log:unsubscribe! audit-subscription)
     (policy:revoke! winded)
     (store:delete! owner notes)
     (store:delete! owner secret)
     (test:finish! 'policy)))
