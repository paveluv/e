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
          revoke! revoke-actor! revoked?
          session-eval! session-edit! session-undo! session-redo! session-history-step!
          session-send! session-ask! session-answer! session-cancel!)
  (import (except (rnrs) current-output-port)
          (only (chezscheme)
                current-output-port
                box unbox set-box! format environment eval
                make-engine parameterize print-graph remq make-mutex with-mutex void
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
            revoked             ; box
            (mutable close)))   ; one-shot connection cleanup, outside the lock

  (define session-lock (make-mutex))
  (define live-sessions '())
  (define (session-actor s) (datum:copy (session-actor-raw s)))
  (define (session-owner s) (datum:copy (session-owner-raw s)))

  (define (audit! entry)
    ;; One history and delivery mechanism: read log:entries 'policy or
    ;; subscribe through log. Policy events do not interrupt the head's echo.
    (log:add! 'policy entry #f))

  (define (call-as-session s thunk)
    ;; Work and its audit share the session actor, for direct callers as well
    ;; as wire clients. Mint/revoke remain actions of the controlling caller.
    ;; call-as restores that caller even after fuel exhaustion or an error.
    (actor:call-as (session-actor s) thunk))

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
    ;; or #f outside actor work). A connection supplies its close procedure;
    ;; revocation invokes it once without exposing that resource to callers.
    (case-lambda
      [(actor p) (mint! actor p (actor:current))]
      [(actor p owner) (mint! actor p owner void)]
      [(actor p owner close!)
       (unless (policy? p) (error 'mint! "expected a policy" p))
       (unless (and (actor:identity? actor) (or (not owner) (actor:identity? owner)))
         (error 'mint! "expected actor and optional owner identities" actor owner))
       (unless (procedure? close!) (error 'mint! "expected a close procedure"))
       (let ([s (mint (datum:copy actor) p (datum:copy owner)
                  (grant-environment (policy-grants-raw p)) (box #f) close!)])
         (with-mutex session-lock (set! live-sessions (cons s live-sessions)))
         (audit! (list 'mint (session-actor s) (session-owner s)))
         s)]))

  (define (revoke! s)
    ;; Admission/inventory commit together; logging and all user callbacks
    ;; run outside this owner. An operation already admitted may finish.
    (let ([close!
           (with-mutex session-lock
             (and (not (revoked? s))
                  (let ([close! (session-close s)])
                    (set-box! (session-revoked s) #t)
                    (session-close-set! s void)
                    (set! live-sessions (remq s live-sessions)) close!)))])
      (when close!
        (actor:cancel-owned! s)
        (guard (ex [else (audit! (list 'revoke-error (session-actor s) (kernel:condition-text ex)))])
          (close!))
        (audit! (list 'revoke (session-actor s)))))
    #t)

  (define (revoked? s) (unbox (session-revoked s)))

  (define (revoke-actor! actor)
    ;; Trusted control selects one inventory version. Reentrant cleanup or a
    ;; concurrent reconnect cannot redirect this selection to a new session.
    ;; Return the number selected; a racing disconnect may also revoke them.
    (unless (actor:identity? actor) (error 'revoke-actor! "expected an actor identity"))
    (let* ([actor (datum:copy actor)]
           [selected (with-mutex session-lock
                       (filter (lambda (s) (equal? actor (session-actor-raw s))) live-sessions))])
      (for-each revoke! selected)
      (length selected)))

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
              (cond [(null? acc) (values #f "an empty expression")]
                    [(null? (cdr acc)) (values (car acc) #f)]
                    [else (values (cons 'begin (reverse acc)) #f)])
              (loop (cons datum acc)))))))

  (define (session-eval! s text)
    (call-as-session s
      (lambda ()
        ;; Evaluate an expression (a string, or a datum) in the session's
        ;; granted environment, under its fuel.  -> (status . text):
        ;;   ('ok . "=> values, plus any printed output")
        ;;   ('unbound . _)  the grant does not cover a name: ask the owner
        ;;   ('fuel . _)     the budget ran out
        ;;   ('error . _)  |  ('refused . _)
        (cond
          [(revoked? s) '(refused . "the session is revoked")]
          [else
           (let-values ([(form failure)
                         (guard (ex [else (values #f "unreadable expression")])
                           (if (string? text) (parse-expression text) (values text #f)))])
             (if failure
                 (cons 'error failure)
                 (let ([result (fueled-eval form (session-env s)
                                            (policy-fuel (session-policy s))
                                            (policy-cap (session-policy s)))])
                   (audit!
                     (list 'eval (session-actor s)
                           (clipped (parameterize ([print-graph #t]) (format "~s" form)) 200)
                           (car result)))
                   result)))]))))

  (define (values-text vals)
    (if (null? vals)
        "#<void>"
        (fold-left (lambda (acc v)
                     (string-append acc (if (string=? acc "") "" ", ")
                                    (format "~s" v)))
                   "" vals)))

  (define (fueled-eval form env fuel cap)
    ;; Evaluation and result/condition formatting share one allowance. The
    ;; character cap is a preview bound, not a bound on transient allocation.
    ;; Graph printing also keeps cyclic values from warning on daemon stderr.
    (let* ([sink (open-output-string)]
           [run (lambda ()
                  (parameterize ([current-output-port sink] [print-graph #t])
                    (guard (ex [else
                                (cons (if (undefined-violation? ex) 'unbound 'error)
                                      (clipped (kernel:condition-text ex) cap))])
                      (let* ([vals (call-with-values (lambda () (eval form env)) list)]
                             [printed (get-output-string sink)])
                        (cons 'ok
                              (clipped
                                (string-append "=> " (values-text vals)
                                  (if (string=? printed "") "" (string-append "\noutput:\n" printed))) cap))))))])
      ((make-engine run) fuel
       (lambda (ticks value) value)
       (lambda (engine) '(fuel . "the evaluation ran out of fuel (an infinite loop?)")))))

  ;;; Attributed mutation ------------------------------------------------------

  (define (session-mutate! s operation id transact)
    ;; The policy supplies data; the store checks the current name/read-only
    ;; fact and commits under one lock. No unlocked permission preflight.
    (if (revoked? s) (values 'refused 'revoked)
        (let-values ([(status detail)
                      (transact (session-actor s) (policy-buffers-raw (session-policy s)))])
          (audit! (list operation (session-actor s) id status
                        (if (eq? status 'applied) (car detail) detail)))
          (values status (if (eq? status 'applied) (datum:copy detail text:delta->datum) detail)))))

  (define (session-edit! s id basis span lines . context)
    (call-as-session s
      (lambda ()
        ;; One owned plain receipt (revision text changes), ending at this
        ;; commit. Optional grouping/undo/commit facts use the store context.
        ;; Own the span/lines too: the pure store shares immutable inputs.
        ;; The connection never supplies write access; the session owns it.
        (unless (<= (length context) 1) (error 'session-edit! "expected one edit context"))
        (session-mutate! s 'edit id
          (lambda (actor access)
            (store:edit-with-snapshot! actor id basis
              (text:datum->span (text:span->datum span)) (datum:copy lines)
              (and (pair? context) (datum:copy (car context))) access))))))

  (define (session-history-step! s id direction scope)
    (call-as-session s
      (lambda ()
        (session-mutate! s direction id
          (lambda (actor access) (store:history-step! actor id direction scope access))))))

  (define (session-undo! s id . scope)
    ;; Default mine, or all/(actor who), under the same buffer permission.
    (unless (<= (length scope) 1) (error 'session-undo! "expected one undo scope"))
    (let-values ([(status detail)
                  (session-history-step! s id 'undo (if (pair? scope) (car scope) 'mine))])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (define (session-redo! s id)
    ;; Redo belongs to the requester who undid, even for another author's edit.
    (let-values ([(status detail) (session-history-step! s id 'redo 'mine)])
      (values status (if (eq? status 'applied) (car detail) detail))))

  (define (session-send! s to message)
    (call-as-session s
      (lambda ()
        ;; Application control uses trusted raw delivery. Peer mail has an
        ;; envelope whose sender comes from the session, never its payload.
        (unless (actor:identity? to) (error 'session-send! "expected a recipient identity"))
        (and (not (revoked? s))
             (actor:send! to (list 'message (session-actor s) message))))))

  (define session-ask!
    (case-lambda
      [(s question choices reply!) (session-ask! s (session-owner s) question choices reply!)]
      [(s to question choices reply!)
       (call-as-session s
         (lambda ()
           (unless (procedure? reply!) (error 'session-ask! "expected a reply procedure"))
           (and (not (revoked? s))
             (let* ([to (datum:copy to)] [question (datum:copy question)]
                    [ticket
                     (actor:ask! (session-actor s) to question choices
                       (lambda (answer) (unless (revoked? s) (reply! answer))) s)])
               ;; An ask admitted before revocation can finish delivery, but
               ;; must not leave a ticket created after revoke!'s cleanup.
               (when (and ticket (revoked? s)) (actor:cancel! ticket s))
               (audit! (list 'ask (session-actor s) to (clipped question 200)))
               ticket))))]))

  (define (session-answer! s ticket answer)
    (call-as-session s
      (lambda ()
        (and (not (revoked? s)) (actor:answer! ticket answer (session-actor s))))))

  (define (session-cancel! s ticket)
    (and (not (revoked? s)) (actor:cancel! ticket s))))
