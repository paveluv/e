#!/usr/bin/env scheme-script

;; Capability minting and budgets: sessions curried with an actor's
;; identity, fueled evaluation, edit quotas and allowlists, the
;; escalation path, revocation, and the audit trail.  v2 stage 4
;; (docs/DESIGN2.md).  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (policy) policy:)
             (prefix (state) state:)
             (prefix (text) text:)
             (prefix (actors) actors:)
             (prefix (kernel) kernel:)
             (only (chezscheme) box unbox set-box! format))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'policy-test label actual expected)))

     (define (contains? text needle)
       (let ([n (string-length text)] [m (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i m) n) #f]
                 [(string=? (substring text i (+ i m)) needle) #t]
                 [else (scan (+ i 1))]))))

     ;; an owner head with a mailbox, and a buffer to work on
     (define owner '(head test))
     (define owner-mail (kernel:make-mailbox))
     (actors:register! owner
                       (lambda (m) (kernel:mailbox-post! owner-mail m)))
     (define agent '(agent helper 1))
     (define notes (state:create! owner "notes" '("one" "two")))
     (define secret (state:create! owner "secret" '("hidden")))

     ;; the audit trail, injected
     (define audit '())
     (define (audit! entry) (set! audit (cons entry audit)))

     (define s (policy:mint!
                 agent
                 (policy:make-policy 'all 100000000 2 '("notes") 4000)
                 owner audit!))

     (check 'minted (policy:session? s) #t)
     (check 'session-is-listed
            (policy:sessions) (list (list agent owner 2)))

     ;; -- fueled evaluation in the granted environment -----------------

     (check 'eval-computes
            (policy:session-eval! s "(+ 1 2)") '(ok . "=> 3"))
     (check 'eval-accepts-a-datum
            (policy:session-eval! s '(* 2 3)) '(ok . "=> 6"))
     (check 'eval-captures-output
            (policy:session-eval! s "(display \"hi\") (+ 1 2)")
            '(ok . "=> 3\noutput:\nhi"))
     (check 'eval-multiple-values
            (policy:session-eval! s "(values 1 2)") '(ok . "=> 1, 2"))
     (check 'eval-reads-the-store
            (policy:session-eval! s "(buffer-text-line \"notes\" 0)")
            '(ok . "=> \"one\""))
     (check 'eval-refuses-the-unbound
            (car (policy:session-eval! s "(delete-file \"x\")"))
            'unbound)
     (check 'eval-reports-errors
            (car (policy:session-eval! s "(car '())"))
            'error)
     (check 'eval-empty
            (policy:session-eval! s "") '(error . "an empty expression"))

     ;; a tiny fuel tank: the loop cannot hang anything
     (define winded (policy:mint!
                      '(agent winded)
                      (policy:make-policy '(+ car cons quote let lambda if)
                                          10000 0 '() 4000)
                      owner audit!))
     (check 'fuel-runs-out
            (policy:session-eval! winded "(let loop () (loop))")
            '(fuel . "the evaluation ran out of fuel (an infinite loop?)"))

     ;; -- a narrowed grant subsets the sandbox -------------------------

     (check 'narrow-grant-works
            (policy:session-eval! winded "(+ 1 2)") '(ok . "=> 3"))
     (check 'narrow-grant-excludes
            (car (policy:session-eval! winded "(buffer-names)"))
            'unbound)

     ;; -- attributed, budgeted edits ------------------------------------

     (define (try-edit! session id line)
       (let-values ([(status detail)
                     (policy:session-edit!
                       session id (state:revision id)
                       (text:make-span 0 0 0 0) (list line))])
         (list status (number? detail))))

     (check 'edit-applies (try-edit! s notes "zero ") '(applied #t))
     (check 'edit-is-attributed
            (cadr (car (state:history notes))) agent)
     (check 'undo-own-edit
            (let-values ([(status detail)
                          (policy:session-undo! s notes)])
              status)
            'applied)
     (check 'second-edit-applies (try-edit! s notes "again ") '(applied #t))
     (check 'quota-exhausted
            (let-values ([(status detail)
                          (policy:session-edit!
                            s notes (state:revision notes)
                            (text:make-span 0 0 0 0) '("more "))])
              (list status detail))
            '(refused quota))
     (check 'allowlist-refuses-other-buffers
            (let-values ([(status detail)
                          (policy:session-edit!
                            s secret (state:revision secret)
                            (text:make-span 0 0 0 0) '("leak "))])
              (list status detail))
            '(refused buffer))
     (check 'secret-untouched (state:line secret 0) "hidden")

     ;; -- the escalation path: the session asks its owner --------------

     (define got (box #f))
     (define ticket
       (policy:session-ask! s "May I edit secret?" '("yes" "no")
                            (lambda (answer) (set-box! got answer))))
     (check 'ask-reaches-the-owner
            (kernel:mailbox-receive! owner-mail)
            (list 'ask ticket agent "May I edit secret?" '("yes" "no")))
     (actors:answer! ticket "no")
     (check 'answer-routes-back (unbox got) "no")

     ;; -- revocation ----------------------------------------------------

     (check 'revoke (policy:revoke! s) #t)
     (check 'revoked-eval-refused
            (car (policy:session-eval! s "(+ 1 2)")) 'refused)
     (check 'revoked-edit-refused
            (let-values ([(status detail)
                          (policy:session-edit!
                            s notes (state:revision notes)
                            (text:make-span 0 0 0 0) '("x"))])
              (list status detail))
            '(refused revoked))
     (check 'revoked-ask-refused
            (policy:session-ask! s "Anyone?" '() (lambda (a) a)) #f)
     (check 'revoked-not-listed
            (assoc agent (policy:sessions)) #f)

     ;; -- the audit trail ------------------------------------------------

     (check 'audit-kinds
            (map (lambda (kind) (and (assq kind audit) #t))
                 '(mint eval edit undo ask revoke))
            '(#t #t #t #t #t #t))

     ;; the default trail rides a persistent cell
     (define quiet (policy:mint!
                     '(agent quiet)
                     (policy:make-policy '(+) 10000 0 '() 4000)))
     (policy:session-eval! quiet "(+ 1 1)")
     (check 'default-audit-records
            (let ([entries (policy:audit-log 10)])
              (and (assq 'mint entries) (assq 'eval entries) #t))
            #t)
     (policy:revoke! quiet)

     (state:delete! owner notes)
     (state:delete! owner secret)
     (format #t "~a policy checks passed\n" checks)))
