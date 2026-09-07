;; policy.e -- per-actor permissions and bounded evaluation:
;; the library (policy).  Pure infrastructure with no init!.
;;
;; A session binds an actor, a sandbox environment, buffer permissions,
;; and an owner for questions. Operations check the session and return
;; authoritative results; revoke! ends admission of further operations.
;; Policy/identity data is owned on admission and on every query.
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
                make-engine parameterize remq make-mutex with-mutex
                open-string-input-port open-output-string
                get-output-string)
          (prefix (store) store:)
          (prefix (text) text:)
          (prefix (actor) actor:)
          (prefix (datum) datum:)
          (prefix (only (log) add!) log:)
          (prefix (only (kernel) condition-text) kernel:))

  ;;; Policies ----------------------------------------------------------------

  ;; grants:  'all, or a list of (sandbox) export names -- the
  ;;          session's evaluation environment holds those and
  ;;          nothing else
  ;; fuel:    engine ticks per evaluation
  ;; buffers: 'any, or the list of buffer names the session may edit
  ;; cap:     result and output size, characters
  (define-record-type (policy policy-of-values policy?)
    (fields (immutable grants policy-grants-raw) fuel
            (immutable buffers policy-buffers-raw) cap))

  (define (make-policy grants fuel buffers cap)
    (unless (and (or (eq? grants 'all) (and (list? grants) (for-all symbol? grants)))
                 (fixnum? fuel) (> fuel 0) (fixnum? cap) (>= cap 0)
                 (or (eq? buffers 'any) (and (list? buffers) (for-all string? buffers))))
      (error 'make "expected grants, positive fuel, buffer names and nonnegative cap"))
    (policy-of-values (datum:copy grants) fuel (datum:copy buffers) cap))

  (define (policy-grants p) (datum:copy (policy-grants-raw p)))
  (define (policy-buffers p) (datum:copy (policy-buffers-raw p)))

  (define (reader-policy)
    ;; the whole read-only tier, no edits anywhere
    (make-policy 'all 100000000 '() 8000))

  ;;; Sessions ----------------------------------------------------------------

  (define-record-type (session mint session?)
    (fields (immutable actor session-actor-raw)
            policy
            (immutable owner session-owner-raw) ; the actor asked when more is needed
            env                 ; the granted evaluation environment
            revoked))           ; box

  (define session-lock (make-mutex))
  (define live-sessions '())
  (define (session-actor s) (datum:copy (session-actor-raw s)))
  (define (session-owner s) (datum:copy (session-owner-raw s)))

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
       (unless (and (actor:identity? actor) (or (not owner) (actor:identity? owner)))
         (error 'mint! "expected actor and optional owner identities" actor owner))
       (let ([s (mint (datum:copy actor) p (datum:copy owner)
                  (grant-environment (policy-grants-raw p)) (box #f))])
         (with-mutex session-lock (set! live-sessions (cons s live-sessions)))
         (audit! (list 'mint (session-actor s) (session-owner s)))
         s)]))

  (define (revoke! s)
    ;; Admission/inventory commit together; logging and all user callbacks
    ;; run outside this owner. An operation already admitted may finish.
    (when (with-mutex session-lock
            (and (not (revoked? s))
                 (begin
                   (set-box! (session-revoked s) #t)
                   (set! live-sessions (remq s live-sessions)) #t)))
      (audit! (list 'revoke (session-actor s))))
    #t)

  (define (revoked? s) (unbox (session-revoked s)))

  (define (sessions)
    ;; Capture one inventory version, then copy its immutable metadata.
    (map (lambda (s)
           (list (session-actor s) (session-owner s)))
         (with-mutex session-lock live-sessions)))

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
    (let ([allowed (policy-buffers-raw (session-policy s))])
      (or (eq? allowed 'any)
          (member (guard (ex [else #f]) (store:buffer-name id))
                  allowed))))

  (define (session-edit! s id basis span lines)
    ;; One owned plain receipt: applied (revision text changes), ending at
    ;; this transaction even if subscribers write again before return.
    ;; Changes use text's canonical delta datums; no store aliases escape.
    ;; Otherwise the store's (values status detail),
    ;; plus (values 'refused 'revoked|'buffer).
    (cond
      [(revoked? s) (values 'refused 'revoked)]
      [(not (buffer-allowed? s id)) (values 'refused 'buffer)]
      [else
       (let-values ([(status detail)
                     (store:edit-with-snapshot! (session-actor s) id basis span lines)])
         (audit!
           (list 'edit (session-actor s) id status
                 (if (eq? status 'applied) (car detail) detail)))
         (values status (if (eq? status 'applied) (datum:copy detail text:delta->datum) detail)))]))

  (define (session-undo! s id . scope)
    ;; Default mine, or all/(actor who), under the same buffer permission.
    (cond
      [(revoked? s) (values 'refused 'revoked)]
      [(not (buffer-allowed? s id)) (values 'refused 'buffer)]
      [else
       (let-values ([(status detail)
                     (apply store:undo! (session-actor s) id scope)])
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
