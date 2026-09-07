;; policy.e -- per-actor permissions and bounded evaluation:
;; the library (policy).  Pure infrastructure with no init!.
;;
;; A session binds an actor, a sandbox environment, buffer permissions,
;; and an owner for questions. Operations check the session and return
;; plain results; revoke! ends permission. Metadata ownership still needs
;; Q94's admission/query copies in dev/MULTIHEAD_PROGRESS.md.
;;
;; Evaluation fuel (engines -- a loop cannot hang the editor), a
;; result-size cap, and a buffer allowlist live in the policy record
;; and are enforced here, at the seam. Edits have no count budget.
;;
;; Sessions are local to this library instance. The base bootstrap pins
;; session authority for the process lifetime; supported reload refuses
;; redefinition. Explicit revoke! ends a retained session's permission.
;; Audit records belong to the shared log.
;;
;; The honest limit: in-process, all of this
;; constrains a misbehaving model, not hostile code.  One approved
;; full-power eval owns the image; the confirmation gate stays the
;; in-process trust boundary.

(library (policy)
  (export (rename (make-policy make)) policy?
          (rename (policy-grants grants)) (rename (policy-fuel fuel)) (rename (policy-buffers buffers))
          (rename (policy-cap cap)) (rename (reader-policy reader))
          mint! session? session-actor session-owner sessions
          revoke! revoked?
          session-eval! session-edit! session-undo! session-ask!)
  (import (except (rnrs) current-output-port)
          (only (chezscheme)
                current-output-port
                box unbox set-box! format environment eval
                make-engine parameterize remq
                open-string-input-port open-output-string
                get-output-string)
          (prefix (store) store:)
          (prefix (actor) actor:)
          (prefix (only (log) add!) log:)
          (prefix (only (kernel) condition-text) kernel:))

  ;;; Policies ----------------------------------------------------------------

  ;; grants:  'all, or a list of (sandbox) export names -- the
  ;;          session's evaluation environment holds those and
  ;;          nothing else
  ;; fuel:    engine ticks per evaluation
  ;; buffers: 'any, or the list of buffer names the session may edit
  ;; cap:     result and output size, characters
  (define-record-type (policy make-policy policy?)
    (fields grants fuel buffers cap))

  (define (reader-policy)
    ;; the whole read-only tier, no edits anywhere
    (make-policy 'all 100000000 '() 8000))

  ;;; Sessions ----------------------------------------------------------------

  (define-record-type (session mint session?)
    (fields actor
            policy
            owner               ; the actor asked when more is needed
            env                 ; the granted evaluation environment
            revoked))           ; box

  (define live-sessions (box '()))

  (define (audit! entry)
    ;; One history and delivery mechanism: read log:entries 'policy or
    ;; subscribe through log. Policy events do not interrupt the head's echo.
    (log:add! 'policy entry #f))

  (define (clipped text cap)
    (if (> (string-length text) cap)
        (string-append (substring text 0 cap) " ...")
        text))

  (define (grant-environment grants)
    (environment (if (eq? grants 'all)
                     '(sandbox)
                     `(only (sandbox) ,@grants))))

  (define mint!
    ;; Mint a session for the actor under a policy. The optional owner is
    ;; consulted for anything beyond the grant (default: actor:current,
    ;; or #f outside actor work).
    (case-lambda
      [(actor p) (mint! actor p (actor:current))]
      [(actor p owner)
       (unless (policy? p) (error 'mint! "expected a policy" p))
       (let ([s (mint actor p owner (grant-environment (policy-grants p)) (box #f))])
         (set-box! live-sessions (cons s (unbox live-sessions)))
         (audit! (list 'mint actor owner))
         s)]))

  (define (revoke! s)
    (set-box! (session-revoked s) #t)
    (set-box! live-sessions (remq s (unbox live-sessions)))
    (audit! (list 'revoke (session-actor s)))
    #t)

  (define (revoked? s) (unbox (session-revoked s)))

  (define (sessions)
    ;; the live sessions as data: ((actor owner) ...)
    (map (lambda (s)
           (list (session-actor s) (session-owner s)))
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
             (let ([outcome (actor:call-as (session-actor s)
                              (lambda ()
                                (fueled-eval form (session-env s)
                                  (policy-fuel (session-policy s)))))]
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
                                 (clipped (kernel:condition-text ex) cap)))])])
                 (audit!
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

  ;;; Attributed mutation ------------------------------------------------------

  (define (buffer-allowed? s id)
    (let ([allowed (policy-buffers (session-policy s))])
      (or (eq? allowed 'any)
          (member (guard (ex [else #f]) (store:buffer-name id))
                  allowed))))

  (define (session-edit! s id basis span lines)
    ;; store:edit! curried with the session's actor and checked
    ;; against its policy.  -> the store's (values status detail),
    ;; plus (values 'refused 'revoked|'buffer).
    (cond
      [(revoked? s) (values 'refused 'revoked)]
      [(not (buffer-allowed? s id)) (values 'refused 'buffer)]
      [else
       (let-values ([(status detail)
                     (store:edit! (session-actor s) id basis span lines)])
         (audit!
           (list 'edit (session-actor s) id status detail))
         (values status detail))]))

  (define (session-undo! s id)
    ;; undo the session's own newest live edit
    (cond
      [(revoked? s) (values 'refused 'revoked)]
      [(not (buffer-allowed? s id)) (values 'refused 'buffer)]
      [else
       (let-values ([(status detail)
                     (store:undo! (session-actor s) id)])
         (audit!
           (list 'undo (session-actor s) id status))
         (values status detail))]))

  (define (session-ask! s question choices reply!)
    ;; ask the session's owner -- the escalation path when the grant
    ;; is not enough; -> the ticket, or #f
    (if (revoked? s)
        #f
        (begin
          (audit!
            (list 'ask (session-actor s) (clipped question 200)))
          (actor:ask! (session-actor s) (session-owner s)
                      question choices reply!)))))
