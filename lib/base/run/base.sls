;; base.sls -- process lifetime and the local daemon. Base runtime only.
(import (only (edoc) elibrary))
(elibrary (base)
  (export call-with-runtime run connection-policy connection-owner closing-hook)
  (import (chezscheme)
          (prefix (kernel) kernel:)
          (prefix (daemon) daemon:)
          (prefix (activity) activity:)
          (prefix (startup) startup:)
          (prefix (sys) sys:)
          (prefix (wire) wire:)
          (prefix (store) store:)
          (prefix (actor) actor:)
          (prefix (policy) policy:)
          (prefix (text) text:)
          (prefix (datum) datum:)
          (prefix (property) property:)
          (prefix (log) log:)
          (prefix (file) file:)
          (prefix (vt) vt:)
          (prefix (surface) surface:)
          (prefix (reference) reference:)
          (prefix (doc) doc:)
          (prefix (session) session:))

  (define modules
    '("activity" "actor" "daemon" "datum" "diff" "doc" "file" "git" "https" "identity" "journal" "log" "path" "policy"
      "property" "reference" "sandbox" "session" "startup" "store" "string" "surface" "sys" "text" "vt" "wire"))

  ;; Base configuration selects permissions from the admitted local identity.
  ;; The hello supplies no grants. Agent write access must be selected here.
  ;; The closing notice of a connection goes through this hook with the
  ;; connection, the reason and the thunk that sends it; a test's base
  ;; configuration may lose the notice to model a broken transport.
  (edoc "How a connection's closing notice is sent: (hook connection reason send!), send! sending it; a test may lose it."
        (value procedure))
  (define closing-hook (make-parameter (lambda (connection reason send!) (send!))))

  (edoc "The policy a connecting actor gets: (policy actor) giving a policy record; heads get everything, others the reader tier."
        (value procedure))
  (define connection-policy (make-parameter
                              (lambda (actor)
                                (if (eq? (car actor) 'head) (policy:make 'all 100000000 'any 8000)
                                  (policy:reader)))))

  ;; Routing is independent of permission. An agent can ask its configured
  ;; owner while the owner's head is absent; no connection supplies grants.
  (edoc "Who an actor's questions go to: (owner actor) giving an identity, or #f."
        (value procedure))
  (define connection-owner (make-parameter (lambda (actor) (and (eq? (car actor) 'head) actor))))

  (define source-fingerprint #f)

  (edoc "Run the base under its runtime: pin the modules, keep the audit, and clean up when the thunk returns."
        (thunk thunk "the base")
        (returns any))
  (define (call-with-runtime thunk)
    ;; Ownership and diagnostics are already established by the loader.
    ;; Ending a head connection never enters this cleanup.
    (kernel:pin-modules! (cons* "base" "cache" modules))
    (let ([audit #f])
      (dynamic-wind void
        (lambda ()
          (set! source-fingerprint (kernel:fingerprint))
          (session:restore!)
          ;; One producer for every head and for work while all heads are
          ;; absent. Log small operation facts, never retained text/deltas.
          (set! audit (store:subscribe! #f audit-store-event!))
          (actor:call-as '(base e)
            (lambda ()
              (let ([failures (kernel:load-modules! modules)])
                (unless (null? failures) (raise (cdar failures))))
              (let ([result (kernel:load-config! 'base)])
                (when (condition? result) (raise result)))
              ;; the trash expires by age: at startup, then at each daily rotation
              (store:expire-trash! '(base e))))
          (thunk))
        (lambda ()
          (dynamic-wind void vt:close-all!
            (lambda ()
              (for-each policy:revoke! (policy:live))
              (when audit (store:unsubscribe! audit))))))))

  (define (audit-store-event! event)
    (let* ([kind (car event)] [id (cadr event)]
           [actor (if (eq? kind 'delete) (caddr event) (cadddr event))]
           [detail
            (case kind
              [(edit)
               (append (list 'edit id (caddr event)
                         (text:span->datum (text:delta-span (list-ref event 4))))
                 (list-tail event 5))]
              [(delete) (list 'delete id)]
              [else (list kind id (caddr event))])])
      (actor:call-as actor (lambda () (log:add! 'store detail #f)))))

  (define (request session control? operation args reply!)
    (define (arity n)
      (unless (= (length args) n) (error 'wire "wrong request arity" operation)))
    (define actor (policy:session-actor session))
    (define (control!)
      (unless control?
        (error 'wire "operation requires an all-buffer head connection" operation)))
    (define (head!)
      (unless (eq? (car actor) 'head)
        (error 'wire "operation requires an active head connection" operation)))
    (case operation
      [(buffers actors)
       (arity 0)
       (if (eq? operation 'actors) (actor:attached)
           (sort < (store:buffer-list)))]
      [(name)
       (arity 1)
       (store:buffer-name (car args))]
      [(find-file) (arity 1) (store:find-file (car args))]
      [(snapshot)
       (unless (<= 1 (length args) 2) (error 'wire "expected buffer and optional basis"))
       (when (pair? (cdr args))
         (unless (and (integer? (cadr args)) (exact? (cadr args)) (>= (cadr args) 0))
           (error 'wire "expected a nonnegative basis revision")))
       ;; Audience is view routing, not a read ACL. Optional changes end at
       ;; exactly this text/facts snapshot and use the edit receipt's codec.
       (datum:copy (call-with-values (lambda () (apply store:snapshot-state args)) list)
         text:delta->datum)]
      [(state)
       (unless (and (<= 2 (length args) 3) (or (= (length args) 2) (memq (caddr args) '(#f #t facts))))
         (error 'wire "expected buffer, basis and optional delta flag"))
       ;; A client holding text at the basis asks for a delta reply: the text
       ;; slot is #f whenever the complete chain since the basis is included.
       ;; One holding the facts too (facts) gets the modification facts;
       ;; other stored facts change through events it already receives.
       (let* ([mode (and (= (length args) 3) (caddr args))]
              [complete? (lambda (state) (and state (= (length state) 5) (list-ref state 4)))]
              [state (let ([selected (store:state (car args) (cadr args)
                                       (if (eq? mode 'facts) property:edit-keys #t))])
                       ;; A broken chain needs the whole state after all.
                       (if (and (eq? mode 'facts) (not (complete? selected)))
                           (store:state (car args) (cadr args))
                           selected))])
         (datum:copy
           (if (and mode (complete? state))
               (list (car state) #f (caddr state) (cadddr state) (list-ref state 4))
               state)
           text:delta->datum))]
      [(eval) (arity 1) (policy:session-eval! session (car args))]
      [(sessions) (control!) (arity 0) (policy:sessions)]
      [(revoke)
       (control!) (arity 1)
       (unless (and (actor:identity? (car args)) (eq? (caar args) 'agent))
         (error 'wire "expected an agent identity"))
       (policy:revoke-actor! (car args))]
      [(edit)
       (unless (and (<= 4 (length args) 6) (or (< (length args) 6) (boolean? (list-ref args 5))))
         (error 'wire "expected buffer, basis, span, lines, optional context and delta flag"))
       (unless (and (integer? (cadr args)) (exact? (cadr args)) (>= (cadr args) 0))
         (error 'wire "expected a nonnegative basis revision"))
       ;; The receipt's chain runs from the basis through the accepted edit.
       ;; A client holding text at the basis asks to omit the text slot; the
       ;; policy then never copies the text it would drop.
       (let* ([delta? (and (= (length args) 6) (list-ref args 5))]
              [context (if (>= (length args) 5) (list-ref args 4) #f)])
         (let-values ([(status detail)
                       (policy:session-edit! session (car args) (cadr args)
                         (text:datum->span (caddr args)) (cadddr args) context delta?)])
           ;; The receipt already owns its text and facts from one commit.
           (list status detail)))]
      [(undo)
       (unless (<= 1 (length args) 2) (error 'wire "expected buffer and optional undo scope"))
       (call-with-values (lambda () (apply policy:session-undo! session args)) list)]
      [(redo)
       (arity 1)
       (call-with-values (lambda () (policy:session-redo! session (car args))) list)]
      [(history-step)
       (arity 3)
       (call-with-values (lambda () (apply policy:session-history-step! session args)) list)]
      [(undo-authors) (arity 1) (store:undo-authors (car args))]
      [(history blame)
       (unless (<= 1 (length args) 2) (error 'wire "expected buffer and optional count"))
       (if (eq? operation 'history) (apply store:history args)
           (map (lambda (entry) (cons (text:span->datum (car entry)) (cdr entry)))
             (apply store:blame args)))]
      [(create visit reset rename delete discard properties)
       (control!)
       (case operation
         [(create) (apply store:create! actor args)]
         [(visit) (call-with-values (lambda () (apply store:visit! actor args)) list)]
         [(reset) (apply store:reset! actor args)]
         [(rename) (arity 2) (apply store:rename! actor args)]
         [(delete) (arity 1) (apply store:delete! actor args) #t]
         [(discard) (arity 3) (apply store:discard! actor args)]
         [(properties) (apply store:set-properties! actor args)])]
      [(marks)
       (arity 4)
       (head!)
       (call-with-values
         (lambda ()
           (store:set-marks! actor (car args) (cadr args)
             (map (lambda (entry)
                    (cons (car entry)
                      (if (and (pair? (cdr entry)) (eq? (cadr entry) 'span))
                          (text:datum->span (caddr entry)) (cdr entry)))) (caddr args))
             (cadddr args))) list)]
      [(read-marks)
       (head!) (arity 1)
       (map (lambda (entry)
              (cons (car entry) (if (text:span? (cdr entry))
                                  (list 'span (text:span->datum (cdr entry))) (cdr entry))))
         (store:marks actor (car args)))]
      [(checkpoint)
       (head!)
       (case (length args)
         [(0) (actor:checkpoint actor)]
         [(1) (actor:checkpoint! actor (car args)) #t]
         [else (error 'wire "expected an optional checkpoint")])]
      [(startup-notice) (head!) (arity 0) (session:take-notice!)]
      [(surface) (arity 1) (surface:snapshot (car args))]
      [(rows) (arity 4) (apply surface:rows args)]
      [(send) (control!) (arity 2) (apply actor:send! args)]
      [(mail) (arity 2) (apply policy:session-send! session args)]
      [(owner) (arity 0) (policy:session-owner session)]
      [(ask)
       (unless (<= 2 (length args) 3) (error 'wire "expected optional recipient, question and choices"))
       (apply policy:session-ask! session (append args (list reply!)))]
      [(pending) (arity 0) (actor:pending actor)]
      [(answer) (arity 2) (apply policy:session-answer! session args)]
      [(cancel) (arity 1) (policy:session-cancel! session (car args))]
      [(log-snapshot)
       (unless (<= 1 (length args) 4) (error 'wire "expected start, optional count, component and actor"))
       (call-with-values (lambda () (apply log:snapshot args)) list)]
      [(log-retention)
       (unless (<= (length args) 1) (error 'wire "expected an optional record count"))
       (unless (null? args) (control!))
       (apply log:retention args)]
      [(log-add)
       (arity 3)
       (parameterize ([log:progress (eq? (caddr args) 'progress)])
         (log:add! (car args) (cadr args) (and (caddr args) #t)))]
      [(vt-open vt-send vt-close vt-color vt-option)
       (control!)
       (case operation
         [(vt-open) (arity 5) (apply vt:open! actor args)]
         [(vt-send) (arity 5) (apply vt:send! actor args) #t]
         [(vt-close) (arity 1) (vt:close! (car args)) #t]
         [(vt-color) (arity 1) (vt:color-scheme! (car args) actor) #t]
         [(vt-option)
          (unless (<= 1 (length args) 2) (error 'wire "expected option and optional value"))
          (let ([option (case (car args) [(shell) vt:shell] [(scrollback) vt:scrollback]
                          [else (error 'wire "unknown terminal option")])])
            (if (null? (cdr args)) (option) (begin (option (cadr args)) #t)))])]
      [(reference-fetch) (control!) (arity 0) (reference:fetch!) #t]
      [(reference-page) (arity 0) (reference:page actor)]
      [(reference-page!)
       (control!) (arity 4)
       (doc:call-with-entries (cadddr args)
         (lambda () (apply reference:page! actor (car args) (cadr args) (caddr args))))]
      [(reference-lookup)
       (arity 2)
       (doc:call-with-entries (cadr args) (lambda () (map doc:to-datum (reference:lookup (car args)))))]
      [(reference-entries)
       (arity 1)
       (doc:call-with-entries (car args) (lambda () (map doc:to-datum (reference:entries))))]
      [(reference-url) (arity 1) (reference:browser-url (doc:from-datum (car args)))]
      [else (error 'wire "unknown request" operation)]))

  ;; One participation list linearizes both admission and departure. A head
  ;; that committed its departure is excluded before its reply is queued;
  ;; concurrent quitters cannot both count each other as staying.
  (define-record-type peer
    (fields connection (mutable identity) (mutable maintenance?) (mutable leaving?) (mutable connected?) (mutable pending) (mutable finish!)))
  (define peer-lock activity:lock)
  (define peers '())
  (define finished (make-condition))
  (define instance (sys:process-identity))

  ;; The control loop is the sole lifecycle executor. Its owner and current
  ;; review share admission's mutex; producer coordination lives below the
  ;; services, so retained in-process callers use the same pause as the wire.
  (define-record-type lifecycle
    (fields (mutable operation) (mutable owner) (mutable review) (mutable serial)))
  (define lifecycle-state (make-lifecycle #f #f #f 0))
  (define-record-type review
    (fields token heads terminals agents tickets))
  (edoc "The base is busy in a lifecycle phase."
        (phase symbol "the phase"))
  (define-condition-type &busy &error make-busy busy?
    (phase busy-phase))
  (edoc "The base runs sources older than the connecting head's.")
  (define-condition-type &stale-base &error make-stale-base stale-base?)

  (define (phase)
    (if (eq? (activity:phase) 'running)
        (if (lifecycle-owner lifecycle-state) 'reviewing 'running)
        (activity:phase)))
  (define (busy!)
    (raise (condition (make-busy (phase))
             (make-message-condition (format "The base is ~a; retry after the review finishes" (phase))))))

  (define (participating-heads)
    ;; Caller owns peer-lock, including during a handshake reservation.
    (filter (lambda (peer)
              (and (peer-connected? peer) (not (peer-leaving? peer))
                   (not (peer-maintenance? peer))
                   (peer-identity peer) (eq? (car (peer-identity peer)) 'head))) peers))

  (define (agent-sessions)
    (filter (lambda (s) (eq? (car (policy:session-actor s)) 'agent)) (policy:live)))

  (define (release-review! peer)
    (with-mutex peer-lock
      (when (and (eq? (activity:phase) 'running) (eq? peer (lifecycle-owner lifecycle-state)))
        (lifecycle-owner-set! lifecycle-state #f)
        (lifecycle-operation-set! lifecycle-state #f)
        (lifecycle-review-set! lifecycle-state #f))))

  (define (claim-review! peer operation)
    ;; Caller owns peer-lock. Taking or reusing a reservation performs no I/O.
    (unless (and (peer-connected? peer) (not (peer-leaving? peer)))
      (error operation "this head has left"))
    (unless (and (eq? (activity:phase) 'running)
                 (or (not (lifecycle-owner lifecycle-state))
                     (and (eq? peer (lifecycle-owner lifecycle-state))
                          (eq? operation (lifecycle-operation lifecycle-state)))))
      (busy!))
    (lifecycle-owner-set! lifecycle-state peer)
    (lifecycle-operation-set! lifecycle-state operation))

  (define (prepare-review! peer operation)
    (with-mutex peer-lock (claim-review! peer operation))
    (let* ([heads (with-mutex peer-lock (participating-heads))]
           [terminals (vt:running)] [agents (agent-sessions)] [tickets (actor:pending-tickets)]
           [token (with-mutex peer-lock
                    (unless (peer-connected? peer) (error operation "reviewing head disconnected"))
                    (lifecycle-serial-set! lifecycle-state (+ 1 (lifecycle-serial lifecycle-state)))
                    (lifecycle-serial lifecycle-state))]
           [review (make-review token heads terminals agents tickets)]
           [counts (status (map peer-identity heads))])
      (with-mutex peer-lock (lifecycle-review-set! lifecycle-state review))
      ;; Keep the maintenance review envelope; shared text is always saved
      ;; and no longer needs a separate discard summary or retained copy.
      (list 'review token '()
        (cons* (cons 'terminals (length terminals)) (cons 'agents (length agents)) (cons 'pending (length tickets))
               (filter (lambda (entry) (not (memq (car entry) '(terminals agents pending)))) counts)))))

  (define (current-review peer token operation)
    (with-mutex peer-lock
      (let ([review (lifecycle-review lifecycle-state)])
        (unless (and (eq? peer (lifecycle-owner lifecycle-state))
                     (peer-connected? peer) (not (peer-leaving? peer))
                     (or (not operation) (eq? (lifecycle-operation lifecycle-state) operation))
                     review (equal? token (review-token review)))
          (error 'review "review token is no longer current for this connection"))
        review)))

  (define (save-and-stop!)
    ;; Every graceful stop saves under the same pause before closing any
    ;; producer. A failed save leaves the existing session usable.
    (session:save!)
    (store:close!)
    (activity:stop!)
    ;; Mark the priority notice before waking an accepting RPC's worker;
    ;; success is delivered as closing, never as an ordinary detach reply.
    (for-each (lambda (peer) ((peer-finish! peer) (lifecycle-operation lifecycle-state)))
      (with-mutex peer-lock peers)))

  (define (stop! peer token operation)
    (let ([review (current-review peer token operation)])
      (dynamic-wind void
        (lambda ()
          (activity:pause! (sys:after 2))
          (if (and (for-all (lambda (p) (memq p (review-heads review)))
                     (with-mutex peer-lock (participating-heads)))
                   (for-all (lambda (owner) (member owner (review-terminals review))) (vt:running))
                   (for-all (lambda (s) (memq s (review-agents review))) (agent-sessions))
                   (for-all (lambda (ticket) (memv ticket (review-tickets review))) (actor:pending-tickets)))
              (begin
                (unless (and (with-mutex peer-lock (peer-connected? peer))
                             (sys:connection-alive? (peer-connection peer)))
                  (error operation "reviewing head disconnected before acceptance"))
                ;; This is the acceptance point. The control loop now owns
                ;; the durable operation, independently of socket lifetime.
                (daemon:call-with-stop save-and-stop!)
                #t)
              (begin
                (activity:resume!)
                (prepare-review! peer operation))))
        ;; A durable failure resumes the existing processes, even after the
        ;; requesting socket has gone. Only a successful save commits the stop.
        (lambda () (unless (eq? (activity:phase) 'stopping) (activity:resume!))))))

  (define (lifecycle-request peer control? operation args)
    (unless (eq? (car (peer-identity peer)) 'head) (error operation "expected a head connection"))
    (unless (or (eq? operation 'leaving) control?)
      (error operation "operation requires an all-buffer head connection"))
    ;; A superseded token cannot cancel or accept the replacement review.
    ;; Validate it before the failure cleanup for an owned operation.
    (when (memq operation '(shutdown restart cancel-review))
      (unless (= (length args) 1) (error operation "expected a review token"))
      (current-review peer (car args) (and (not (eq? operation 'cancel-review)) operation)))
    (guard (ex [else
                (release-review! peer)
                (when (memq operation '(shutdown restart)) (report-stop-error! ex))
                (raise ex)])
      (case operation
        [(leaving)
         (unless (and (= (length args) 1) (boolean? (car args))) (error 'leaving "expected shutdown-on-exit boolean"))
         (let-values ([(last? identities)
                       (with-mutex peer-lock
                         (unless (eq? (activity:phase) 'running) (busy!))
                         (let ([last? (and control? (car args) (= (length (participating-heads)) 1))])
                           (if last? (claim-review! peer 'shutdown) (peer-leaving?-set! peer #t))
                           (values last? (participants))))])
           (let ([counts (status identities)]) (if last? (list 'last counts) counts)))]
        [(prepare-close prepare-restart)
         (unless (null? args) (error operation "expected no arguments"))
         (prepare-review! peer (if (eq? operation 'prepare-close) 'shutdown 'restart))]
        [(shutdown restart) (stop! peer (car args) operation)]
        [(cancel-review)
         (release-review! peer) #t])))

  (define (control-call peer control? operation args)
    (let ([answer (kernel:make-mailbox)])
      (dynamic-wind
        (lambda ()
          (with-mutex peer-lock
            (unless (peer-connected? peer) (error operation "head disconnected"))
            (unless (and (eq? (activity:phase) 'running)
                         (or (eq? operation 'leaving)
                             (not (lifecycle-owner lifecycle-state))
                             (eq? peer (lifecycle-owner lifecycle-state))))
              (busy!))
            (peer-pending-set! peer answer)))
        (lambda ()
          (kernel:mailbox-post! daemon:control
            (lambda ()
              (kernel:mailbox-post! answer
                (guard (ex [else (cons #f ex)])
                  (cons #t (lifecycle-request peer control? operation args))))))
          (let ([result (kernel:mailbox-receive! answer)])
            (if (car result) (cdr result) (raise (cdr result)))))
        (lambda () (with-mutex peer-lock (peer-pending-set! peer #f))))))

  (define (participants)
    ;; Caller owns peer-lock. Never call store/actor services under it.
    (filter values (map (lambda (peer) (and (peer-connected? peer) (not (peer-leaving? peer))
                                         (not (peer-maintenance? peer)) (peer-identity peer))) peers)))

  (define (report-stop-error! ex)
    (let ([message (kernel:condition-text ex)])
      ;; A full or failed disk can break diagnostics as well as saving.
      ;; Reporting failure must not undo the return to a running base.
      (guard (ignored [else (void)]) (log:add! 'base message #t))
      (guard (ignored [else (void)])
        (format (current-error-port) "e: ~a\n" message)
        (flush-output-port (current-error-port)))))

  (define (status identities)
    (let* ([heads (map cadr (filter (lambda (identity) (eq? (car identity) 'head)) identities))]
           [names (append heads (filter (lambda (name) (not (member name heads))) (actor:head-names)))]
           [facts (filter values
                    (map (lambda (id)
                           (let ([state (store:state id #f '(modified mode alive))])
                             (and state (cadddr state))))
                      (store:buffer-list)))])
      (define (fact key facts) (cond [(assq key facts) => cdr] [else #f]))
      (append (session:status) (list (cons 'buffers (length facts))
                                 (cons 'modified (length (filter (lambda (facts) (fact 'modified facts)) facts)))
                                 (cons 'heads (length heads))
                                 (cons 'head-states (map (lambda (name) (list name (if (member name heads) 'attached 'detached))) names))
                                 (cons 'terminals (length (filter (lambda (facts) (and (equal? (fact 'mode facts) "terminal")
                                                                                    (fact 'alive facts))) facts)))
                                 (cons 'agents (length (agent-sessions)))
                                 (cons 'pending (length (actor:pending-tickets)))
                                 (cons 'instance instance)
                                 (cons 'phase (with-mutex peer-lock (phase)))
                                 (cons 'wire-version wire:version)
                                 (cons 'fingerprint source-fingerprint)))))

  (define (serve-connection peer)
    (let* ([connection (peer-connection peer)]
           [owner (list 'connection connection)] [out (kernel:make-mailbox)]
           [out-lock (make-mutex)] [queued-bytes 0] [queued-count 0] [closed? #f]
           [writer #f] [session #f] [changes #f] [head-watch? #f] [control? #f] [closing #f])
      (define (close!)
        (when (with-mutex out-lock
                (and (not closed?) (begin (set! closed? #t) #t)))
          (sys:close-connection! connection)
          (kernel:mailbox-post! out #f)
          (let ([pending (with-mutex peer-lock
                           (peer-connected?-set! peer #f)
                           (peer-pending peer))])
            (when pending
              (kernel:mailbox-post! pending
                (cons #f (condition (make-error) (make-message-condition "Head disconnected"))))))
          (kernel:mailbox-post! daemon:control (lambda () (release-review! peer)))))
      (define (post! message)
        (guard (ex [else (close!) (raise ex)])
          (when (with-mutex out-lock closed?) (error 'wire "connection is closed"))
          ;; #t is one coalesced watch wakeup; all other work is owned bytes.
          ;; Count includes an in-flight write. A stalled peer cannot retain
          ;; unlimited store versions, tiny mail envelopes or encoded replies.
          (let* ([frame (if (eq? message #t) #t (wire:encode message))]
                 [size (if (bytevector? frame) (bytevector-length frame) 0)])
            (unless (with-mutex out-lock
                      (and (not closed?) (< queued-count 256)
                           (<= (+ queued-bytes size) #x2000000)
                           (begin
                             (set! queued-count (+ queued-count 1))
                             (set! queued-bytes (+ queued-bytes size))
                             (kernel:mailbox-post! out frame) #t)))
              ;; Overload is a disconnect, never a silently dropped reply or
              ;; actor message. Only invalidations may coalesce.
              (error 'wire "pending output limit reached")))))
      (define (watch!)
        (unless changes
          ;; Publish the take procedure before the writer can consume a wake.
          ;; Store callbacks run outside its lock; post! takes only out-lock.
          (with-mutex out-lock
            (parameterize ([kernel:registering-module owner])
              (let-values ([(token take!) (store:watch! (lambda () (post! #t)))])
                (set! changes take!)))))
        ;; Subscribe before inventory so a racing commit is in one or both.
        (sort < (store:buffer-list)))
      (define (watch-head!)
        (unless (eq? (car (policy:session-actor session)) 'head) (error 'wire "expected a head connection"))
        (unless head-watch?
          (parameterize ([kernel:registering-module owner])
            (surface:subscribe! #f
              (lambda (event)
                ;; These are invalidations. A client may coalesce several
                ;; generations, so no partial row list survives the seam.
                (post! (list 'surface
                         (list (cons (cadr event) (append (list-head event 4) '(all) (list-tail event 5))))))))
            (actor:subscribe! (lambda (events) (post! '(presence))))
            (log:subscribe! (lambda (entry presentation) (post! (list 'logged entry presentation)))))
          (set! head-watch? #t))
        (watch!))
      (define (reserve! actor maintenance?)
        ;; Only normal heads enter the service barrier and claim a screen.
        ;; Maintenance can read status while a save holds that barrier.
        (unless (if maintenance? (not (eq? (activity:phase) 'stopping)) (eq? (phase) 'running)) (busy!))
        (peer-identity-set! peer (datum:copy actor))
        (peer-maintenance?-set! peer maintenance?)
        (peer-finish!-set! peer
          (lambda (reason)
            (with-mutex out-lock (set! closing reason))
            (kernel:mailbox-post! out #f))))
      (dynamic-wind void
        (lambda ()
          (guard (ex [else
                      (unless writer
                        (guard (ignored [else (void)])
                          (wire:send! (sys:connection-output connection)
                            (list 'error #f
                              (cond [(kernel:registration-conflict? ex) 'name-in-use]
                                    [(busy? ex) (list 'busy (busy-phase ex))]
                                    [(stale-base? ex) (list 'stale-base (status (with-mutex peer-lock (participants))))]
                                    [else (kernel:condition-text ex)])))))])
            (let* ([hello (wire:receive (sys:connection-input connection))]
                   [maintenance? (and (list? hello) (= (length hello) 3)
                                      (eq? (car hello) 'maintenance) (equal? (cadr hello) 1))]
                   [actor (and (or maintenance?
                                   (and (list? hello) (= (length hello) 4) (eq? (car hello) 'hello)
                                        (integer? (cadr hello)) (exact? (cadr hello)) (>= (cadr hello) 0)
                                        (string? (cadddr hello))))
                               (caddr hello))])
              (unless (and (actor:identity? actor) (= (length actor) 2)
                           (memq (car actor) (if maintenance? '(head) '(head agent))) (string? (cadr actor)))
                (error 'wire (format "expected (hello ~a (head-or-agent name) fingerprint) or (maintenance 1 (head name))" wire:version)))
              ;; Refuse before admission, policy sessions, actor registration
              ;; and callbacks. Existing owners and the recovery notice stay put.
              (when (and (not maintenance?)
                         (or (not (= (cadr hello) wire:version)) (not (equal? (cadddr hello) source-fingerprint))))
                (raise (make-stale-base)))
              (if maintenance?
                  (begin
                    (unless (eq? (policy:buffers ((connection-policy) (datum:copy actor))) 'any)
                      (error 'maintenance "operation requires an all-buffer head connection"))
                    (set! control? #t)
                    (with-mutex peer-lock (reserve! actor #t))
                    (post! (list 'maintenance 1 (status (with-mutex peer-lock (participants))))))
                  (activity:call-with
                    (lambda ()
                      (let* ([p ((connection-policy) (datum:copy actor))]
                             [capabilities (if (null? (policy:buffers p)) '(read) '(read edit undo redo))])
                        (set! session (policy:mint! actor p ((connection-owner) (datum:copy actor)) close!))
                        (set! control? (and (eq? (car actor) 'head) (eq? (policy:buffers p) 'any)))
                        ;; Queue hello before publishing; name refusal still revokes
                        ;; this connection's session without touching the old owner.
                        (post! (list 'hello wire:version actor capabilities))
                        (parameterize ([kernel:registering-module owner])
                          (actor:register! actor (lambda (message) (post! (list 'event message))) capabilities))))
                    (lambda ()
                      ;; Reservation and review ownership linearize here. The
                      ;; handshake finishes outside the mutex as admitted work.
                      (reserve! actor #f))))
              (set! writer
                (fork-thread
                  (lambda ()
                    (guard (ex [else (close!)])
                      (let loop ()
                        (let ([item (kernel:mailbox-receive! out)])
                          (cond
                            [(with-mutex out-lock closing)
                             => (lambda (reason)
                                  ;; The control notice takes the next frame
                                  ;; slot; presentation backlog is discarded.
                                  ((closing-hook) connection reason
                                   (lambda () (wire:send! (sys:connection-output connection) (list 'closing reason))))
                                  (close!))]
                            [item
                             (let ([frame
                                    (if (bytevector? item) item
                                      (wire:encode (list 'changed ((with-mutex out-lock changes)))))])
                               (put-bytevector (sys:connection-output connection) frame)
                               (flush-output-port (sys:connection-output connection)))
                             (with-mutex out-lock
                               (set! queued-count (- queued-count 1))
                               (when (bytevector? item)
                                 (set! queued-bytes (- queued-bytes (bytevector-length item)))))
                             (loop)])))))))
              (actor:call-as actor
                (lambda ()
                  (let loop ()
                    (let ([message (wire:receive (sys:connection-input connection))])
                      (unless (eof-object? message)
                        (unless (and (list? message) (>= (length message) 3)
                                     (eq? (car message) 'request)
                                     (integer? (cadr message)) (exact? (cadr message)) (>= (cadr message) 0)
                                     (symbol? (caddr message)))
                          (error 'wire "expected (request id operation argument ...)"))
                        ;; Revocation closes even an idle connection. A frame
                        ;; already read still needs the same admission check,
                        ;; including reads and the subscription requests below.
                        (when (and session (policy:revoked? session)) (error 'wire "the session is revoked"))
                        (when (peer-leaving? peer) (error 'wire "this head has already left"))
                        (post!
                          (guard (ex [else (list 'reply (cadr message) 'error (kernel:condition-text ex))])
                            (unless (if maintenance?
                                        (memq (caddr message) '(status prepare-restart restart cancel-review))
                                        (not (memq (caddr message) '(prepare-restart restart))))
                              (error 'wire "operation is not available on this connection" (caddr message)))
                            (list 'reply (cadr message) 'ok
                              (case (caddr message)
                                [(status)
                                 (unless (= (length message) 3) (error 'wire "status takes no arguments"))
                                 (status (with-mutex peer-lock (participants)))]
                                [(leaving prepare-close shutdown prepare-restart restart cancel-review)
                                 (control-call peer control? (caddr message) (cdddr message))]
                                [(watch watch-head)
                                 (unless (= (length message) 3) (error 'wire "watch takes no arguments"))
                                 (if (eq? (caddr message) 'watch) (watch!) (watch-head!))]
                                [else
                                 ;; Capture only the id, not the entire request.
                                 ;; An answer may precede the ticket reply.
                                 (let ([id (cadr message)])
                                   (request session control? (caddr message) (cdddr message)
                                     (lambda (answer) (post! (list 'event (list 'answer id answer))))))]))))
                        (loop)))))))))
        (lambda ()
          (close!)
          (activity:call-with-retirement
            (lambda ()
              (when session (policy:revoke! session))
              (kernel:retract-module! owner)))
          (when writer (thread-join writer))))))

  (edoc "Run the base: listen on its socket, admit heads and agents, and serve them until stopped."
        (returns any))
  (define (run)
    ;; Bind last, after module initialization and configuration have succeeded.
    ;; The loader still holds the directory's lifetime lock during all cleanup.
    (let* ([path (daemon:socket)] [control daemon:control]
           [stopping? #f] [acceptor #f] [reason 'shutdown])
      (let ([listener (sys:listen-local path)])
        (dynamic-wind void
          (lambda ()
            (set! acceptor
              (fork-thread
                (lambda ()
                  (guard (ex [else (kernel:mailbox-post! control ex)])
                    (let loop ()
                      (let ([connection (sys:accept-local listener)])
                        (when connection
                          (with-mutex peer-lock
                            (if stopping? (sys:close-connection! connection)
                                (let ([peer (make-peer connection #f #f #f #t #f
                                              (lambda (reason) (sys:close-connection! connection)))])
                                  (set! peers (cons peer peers))
                                  (fork-thread
                                    (lambda ()
                                      (dynamic-wind void
                                        (lambda () (serve-connection peer))
                                        (lambda ()
                                          (with-mutex peer-lock
                                            (set! peers (remq peer peers))
                                            (condition-broadcast finished)))))))))
                          (loop))))))))
            (format #t "e: listening on ~a\n" path)
            (flush-output-port (current-output-port))
            ;; The signal receiver posts here; the idle base needs no polling.
            (let loop ()
              (let ([message (kernel:mailbox-receive! control (daemon:log-deadline))])
                (cond [(not message) (daemon:rotate-logs!) (store:expire-trash! '(base e)) (loop)]
                      [(condition? message) (raise message)]
                      [(procedure? message)
                       (message)
                       (if (eq? (activity:phase) 'stopping)
                           (set! reason (lifecycle-operation lifecycle-state))
                           (loop))]
                      [(daemon:take-stop-signal! message)
                       (guard (ex [else
                                   (activity:resume!)
                                   (with-mutex peer-lock
                                     (lifecycle-owner-set! lifecycle-state #f)
                                     (lifecycle-review-set! lifecycle-state #f)
                                     (lifecycle-operation-set! lifecycle-state #f))
                                   (report-stop-error! ex)
                                   (loop)])
                         (with-mutex peer-lock
                           (lifecycle-operation-set! lifecycle-state 'signal)
                           (lifecycle-owner-set! lifecycle-state 'base)
                           (lifecycle-review-set! lifecycle-state #f))
                         (daemon:call-with-stop
                           (lambda ()
                             (activity:pause! (sys:after 2))
                             (save-and-stop!)))
                         (set! reason 'signal))]
                      [else (loop)]))))
          (lambda ()
            (let ([active (with-mutex peer-lock (set! stopping? #t) peers)]
                  [deadline (sys:after 1)])
              (sys:close-local-listener! listener)
              (for-each (lambda (peer) ((peer-finish! peer) reason)) active)
              (with-mutex peer-lock
                (let flush ()
                  (let ([now (current-time 'time-monotonic)])
                    (when (and (pair? peers) (time<? now deadline))
                      (condition-wait finished peer-lock (time-difference deadline now))
                      (flush)))))
              (for-each (lambda (peer) (sys:close-connection! (peer-connection peer))) active)
              (when acceptor (thread-join acceptor))
              (with-mutex peer-lock
                (let wait ()
                  (unless (null? peers)
                    ;; Client owners finish their endpoint and writer cleanup.
                    (condition-wait finished peer-lock)
                    (wait))))))))))
)
