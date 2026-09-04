;; policy.e -- permissions: capability minting per actor, budgets:
;; the library (policy), v2 stage 4 (docs/DESIGN2.md).  Pure
;; infrastructure with no init!.
;;
;; Environments give tiers -- (sandbox) is the read-only expression
;; tier -- but per-actor policy is the object-capability pattern: a
;; session is a record of procedures curried with the actor's
;; identity that check policy, attribute, and bound their results
;; internally.  Holding the session is the permission; revocation is
;; dropping it (or revoke!).  Everything a session returns is plain
;; data: a status symbol and a detail, never a structure the editor
;; holds.
;;
;; Budgets are policy too: evaluation fuel (engines -- a loop cannot
;; hang the editor), an edit quota, a result-size cap, and a buffer
;; allowlist all live in the policy record and are enforced here, at
;; the seam, not inside individual tools.
;;
;; Sessions deliberately do NOT ride a persistent cell: hot-reloading
;; policy.e revokes every outstanding session, and the minter must
;; mint afresh -- a reload can only ever narrow what actors can do.
;; The audit trail, by contrast, is state and survives reloads.
;;
;; The honest limit (docs/DESIGN2.md): in-process, all of this
;; constrains a misbehaving model, not hostile code.  One approved
;; full-power eval owns the image; the confirmation gate stays the
;; in-process trust boundary.

(library (policy)
  (export (rename (make-policy make)) policy?
          (rename (policy-grants grants)) (rename (policy-fuel fuel)) (rename (policy-edits edits)) (rename (policy-buffers buffers))
          (rename (policy-cap cap)) (rename (reader-policy reader))
          mint! session? session-actor session-owner sessions
          revoke! revoked?
          session-eval! session-edit! session-undo! session-ask!
          audit-log)
  (import (except (rnrs) current-output-port)
          (only (chezscheme)
                current-output-port
                box unbox set-box! format void environment eval
                make-engine parameterize remq
                open-string-input-port open-output-string
                get-output-string
                call-with-string-output-port)
          (prefix (state) state:)
          (prefix (actor) actor:)
          (prefix (only (log) add!) log:)
          (only (kernel) persistent-cell condition-text))

  ;;; Policies ----------------------------------------------------------------

  ;; grants:  'all, or a list of (sandbox) export names -- the
  ;;          session's evaluation environment holds those and
  ;;          nothing else
  ;; fuel:    engine ticks per evaluation
  ;; edits:   'unlimited, or how many applied edits the session may
  ;;          spend
  ;; buffers: 'any, or the list of buffer names the session may edit
  ;; cap:     result and output size, characters
  (define-record-type (policy make-policy policy?)
    (fields grants fuel edits buffers cap))

  (define (reader-policy)
    ;; the whole read-only tier, no edits anywhere
    (make-policy 'all 100000000 0 '() 8000))

  ;;; Sessions ----------------------------------------------------------------

  (define-record-type (session mint session?)
    (fields actor
            (mutable policy)
            owner               ; the actor asked when more is needed
            env                 ; the granted evaluation environment
            revoked             ; box
            edits-left          ; box: number or 'unlimited
            audit!))            ; entry -> unspecified

  (define live-sessions (box '()))

  (define audit-limit 512)
  (define audit-cell (persistent-cell 'policy-audit (lambda () '())))

  (define (bounded-audit! entry)
    ;; the persistent trail, and -- quietly -- the log stream, so
    ;; (log-view 'policy) shows what every session did
    (set-box! audit-cell
              (let take ([entries (cons entry (unbox audit-cell))]
                         [n audit-limit])
                (if (or (zero? n) (null? entries))
                    '()
                    (cons (car entries) (take (cdr entries) (- n 1))))))
    (log:add! 'policy entry #f))

  (define (audit-log . count)
    ;; the newest audit entries, newest first, as plain data
    (let ([n (if (pair? count) (car count) 50)])
      (let take ([entries (unbox audit-cell)] [n n])
        (if (or (zero? n) (null? entries))
            '()
            (cons (car entries) (take (cdr entries) (- n 1)))))))

  (define (clipped text cap)
    (if (> (string-length text) cap)
        (string-append (substring text 0 cap) " ...")
        text))

  (define (grant-environment grants)
    (environment (if (eq? grants 'all)
                     '(sandbox)
                     `(only (sandbox) ,@grants))))

  (define (mint! actor p . options)
    ;; Mint a session for the actor under a policy.  Options, in
    ;; order: the owner actor consulted for anything beyond the grant
    ;; (default (head main)) and the audit procedure (default: the
    ;; persistent audit trail read by audit-log).
    (unless (policy? p) (error 'mint! "expected a policy" p))
    (let* ([owner (if (pair? options) (car options) '(head main))]
           [audit! (if (and (pair? options) (pair? (cdr options)))
                       (cadr options)
                       bounded-audit!)]
           [s (mint actor p owner (grant-environment (policy-grants p))
                    (box #f) (box (policy-edits p)) audit!)])
      (set-box! live-sessions (cons s (unbox live-sessions)))
      (audit! (list 'mint actor owner))
      s))

  (define (revoke! s)
    (set-box! (session-revoked s) #t)
    (set-box! live-sessions (remq s (unbox live-sessions)))
    ((session-audit! s) (list 'revoke (session-actor s)))
    #t)

  (define (revoked? s) (unbox (session-revoked s)))

  (define (sessions)
    ;; the live sessions as data: ((actor owner edits-left) ...)
    (map (lambda (s)
           (list (session-actor s) (session-owner s)
                 (unbox (session-edits-left s))))
         (filter (lambda (s) (not (revoked? s)))
                 (unbox live-sessions))))

  ;;; Fueled evaluation ---------------------------------------------------------

  (define (parse-expression text)
    (let ([port (open-string-input-port text)])
      (let loop ([acc '()])
        (let ([datum (get-datum port)])
          (if (eof-object? datum)
              (cond [(null? acc) #f]
                    [(null? (cdr acc)) (car acc)]
                    [else (cons 'begin (reverse acc))])
              (loop (cons datum acc)))))))

  (define (session-eval! s text)
    ;; Evaluate an expression (a string, or a datum) in the session's
    ;; granted environment, under its fuel.  -> (status . text):
    ;;   ('ok . "=> values, plus any printed output")
    ;;   ('unbound . _)  the grant does not cover a name: ask the owner
    ;;   ('fuel . _)     the budget ran out
    ;;   ('error . _)  |  ('refused . _)
    (cond
      [(revoked? s) '(refused . "the session is revoked")]
      [else
       (let ([form (guard (ex [else 'malformed])
                     (if (string? text) (parse-expression text) text))])
         (if (or (not form) (eq? form 'malformed))
             (cons 'error
                   (if form "unreadable expression" "an empty expression"))
             (let ([outcome (fueled-eval form (session-env s)
                                         (policy-fuel (session-policy s)))]
                   [cap (policy-cap (session-policy s))])
               (let ([result
                      (case (car outcome)
                        [(ok)
                         (cons 'ok
                               (clipped
                                 (string-append
                                   (format "=> ~a"
                                           (values-text (cadr outcome)))
                                   (if (string=? (caddr outcome) "")
                                       ""
                                       (string-append
                                         "\noutput:\n" (caddr outcome))))
                                 cap))]
                        [(fuel)
                         '(fuel . "the evaluation ran out of fuel (an infinite loop?)")]
                        [else
                         (let ([ex (cadr outcome)])
                           (cons (if (undefined-violation? ex)
                                     'unbound
                                     'error)
                                 (clipped (condition-text ex) cap)))])])
                 ((session-audit! s)
                  (list 'eval (session-actor s)
                        (clipped (format "~s" form) 200)
                        (car result)))
                 result))))]))

  (define (values-text vals)
    (if (null? vals)
        "#<void>"
        (fold-left (lambda (acc v)
                     (string-append acc (if (string=? acc "") "" ", ")
                                    (format "~s" v)))
                   "" vals)))

  (define (fueled-eval form env fuel)
    ;; -> (ok vals printed) | (fuel) | (error condition)
    (let* ([sink (open-output-string)]
           [run (lambda ()
                  (guard (ex [else (list 'error ex)])
                    (list 'ok
                          (parameterize ([current-output-port sink])
                            (call-with-values
                              (lambda () (eval form env))
                              list)))))]
           [outcome ((make-engine run)
                     fuel
                     (lambda (ticks value) value)
                     (lambda (engine) (list 'fuel)))])
      (if (eq? (car outcome) 'ok)
          (list 'ok (cadr outcome) (get-output-string sink))
          outcome)))

  ;;; Attributed, budgeted mutation ---------------------------------------------

  (define (buffer-allowed? s id)
    (let ([allowed (policy-buffers (session-policy s))])
      (or (eq? allowed 'any)
          (member (guard (ex [else #f]) (state:buffer-name id))
                  allowed))))

  (define (spend-edit? s)
    (let ([left (unbox (session-edits-left s))])
      (cond [(eq? left 'unlimited) #t]
            [(> left 0) (set-box! (session-edits-left s) (- left 1)) #t]
            [else #f])))

  (define (session-edit! s id basis span lines)
    ;; state:edit! curried with the session's actor and checked
    ;; against its policy.  -> the store's (values status detail),
    ;; plus (values 'refused 'revoked|'buffer|'quota).  The quota is
    ;; spent only by applied edits.
    (cond
      [(revoked? s) (values 'refused 'revoked)]
      [(not (buffer-allowed? s id)) (values 'refused 'buffer)]
      [(and (not (eq? (unbox (session-edits-left s)) 'unlimited))
            (<= (unbox (session-edits-left s)) 0))
       (values 'refused 'quota)]
      [else
       (let-values ([(status detail)
                     (state:edit! (session-actor s) id basis span lines)])
         (when (eq? status 'applied) (spend-edit? s))
         ((session-audit! s)
          (list 'edit (session-actor s) id status detail))
         (values status detail))]))

  (define (session-undo! s id)
    ;; undo the session's own newest live edit; free of quota
    (cond
      [(revoked? s) (values 'refused 'revoked)]
      [(not (buffer-allowed? s id)) (values 'refused 'buffer)]
      [else
       (let-values ([(status detail)
                     (state:undo! (session-actor s) id)])
         ((session-audit! s)
          (list 'undo (session-actor s) id status))
         (values status detail))]))

  (define (session-ask! s question choices reply!)
    ;; ask the session's owner -- the escalation path when the grant
    ;; is not enough; -> the ticket, or #f
    (if (revoked? s)
        #f
        (begin
          ((session-audit! s)
           (list 'ask (session-actor s) (clipped question 200)))
          (actor:ask! (session-actor s) (session-owner s)
                      question choices reply!)))))
