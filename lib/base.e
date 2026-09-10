;; base.e -- process lifetime and the local daemon. No head imports.
(library (base)
  (export call-with-runtime run connection-policy connection-owner)
  (import (chezscheme)
          (prefix (kernel) kernel:) (prefix (startup) startup:)
          (prefix (sys) sys:) (prefix (wire) wire:)
          (prefix (store) store:) (prefix (actor) actor:)
          (prefix (policy) policy:) (prefix (text) text:) (prefix (datum) datum:)
          (prefix (log) log:)
          (prefix (file) file:) (prefix (vt) vt:)
          (prefix (surface) surface:) (prefix (reference) reference:) (prefix (doc) doc:))

  (define modules
    '("actor" "datum" "diff" "doc" "file" "git" "https" "identity" "journal" "log" "path" "policy"
      "property" "reference" "sandbox" "startup" "store" "string" "surface" "sys" "text" "vt" "wire"))

  ;; Base configuration selects permissions from the admitted local identity.
  ;; The hello supplies no grants. Agent write access must be selected here.
  (define connection-policy
    (make-parameter
      (lambda (actor)
        (if (eq? (car actor) 'head) (policy:make 'all 100000000 'any 8000)
            (policy:reader)))))

  ;; Routing is independent of permission. An agent can ask its configured
  ;; owner while the owner's head is absent; no connection supplies grants.
  (define connection-owner
    (make-parameter (lambda (actor) (and (eq? (car actor) 'head) actor))))

  (define (call-with-runtime thunk)
    ;; Pin before config can start active work. Plain e owns this same base
    ;; lifetime; ending a head connection never enters this cleanup.
    (kernel:pin-modules! (cons "base" modules))
    (let ([audit #f])
      (dynamic-wind void
        (lambda ()
          ;; One producer for every head and for work while all heads are
          ;; absent. Log small operation facts, never retained text/deltas.
          (set! audit (store:subscribe! #f audit-store-event!))
          (actor:call-as '(base e)
            (lambda ()
              (let ([failures (kernel:load-modules! modules)])
                (unless (null? failures) (raise (cdar failures))))
              (let ([result (kernel:load-config! 'base)])
                (when (condition? result) (raise result)))))
          (thunk))
        (lambda ()
          (dynamic-wind void vt:close-all!
            (lambda () (when audit (store:unsubscribe! audit))))))))

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
       ;; One holding the facts too (facts) gets only the computed modified
       ;; flag; stored facts change through events it already receives.
       (let* ([mode (and (= (length args) 3) (caddr args))]
              [complete? (lambda (state) (and state (= (length state) 5) (list-ref state 4)))]
              [state (let ([selected (store:state (car args) (cadr args)
                                       (if (eq? mode 'facts) '(modified) #t))])
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
           (list status
             (if (and (eq? status 'applied) delta?)
                 ;; The only fact an edit changes by itself rides along, so the
                 ;; client's facts stay current without another read.
                 (list (car detail) #f (caddr detail) (store:property (car args) 'modified #f))
                 detail))))]
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
       (unless (<= 1 (length args) 3) (error 'wire "expected start, optional count and component"))
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

  (define (serve-connection connection)
    (let ([owner (list 'connection connection)] [out (kernel:make-mailbox)]
          [out-lock (make-mutex)] [queued-bytes 0] [queued-count 0] [closed? #f]
          [writer #f] [session #f] [changes #f] [head-watch? #f] [control? #f])
      (define (close!)
        (when (with-mutex out-lock
                (and (not closed?) (begin (set! closed? #t) #t)))
          (sys:close-connection! connection)
          (kernel:mailbox-post! out #f)))
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
      (dynamic-wind void
        (lambda ()
          (guard (ex [else
                      (unless writer
                        (guard (ignored [else (void)])
                          (wire:send! (sys:connection-output connection)
                            (list 'error #f (if (kernel:registration-conflict? ex) 'name-in-use
                                              (kernel:condition-text ex))))))])
            (let* ([hello (wire:receive (sys:connection-input connection))]
                   [actor (and (list? hello) (= (length hello) 3)
                               (eq? (car hello) 'hello) (equal? (cadr hello) wire:version)
                               (caddr hello))])
              (unless (and (actor:identity? actor) (= (length actor) 2)
                           (memq (car actor) '(head agent)) (string? (cadr actor)))
                (error 'wire "expected (hello 1 (head-or-agent name))"))
              (let* ([p ((connection-policy) (datum:copy actor))]
                     [capabilities (if (null? (policy:buffers p)) '(read) '(read edit undo redo))])
                (set! session (policy:mint! actor p ((connection-owner) (datum:copy actor)) close!))
                (set! control? (and (eq? (car actor) 'head) (eq? (policy:buffers p) 'any)))
                ;; Queue hello before publishing; name refusal still revokes
                ;; this connection's session without touching the old owner.
                (post! (list 'hello wire:version actor capabilities))
                (parameterize ([kernel:registering-module owner])
                  (actor:register! actor (lambda (message) (post! (list 'event message))) capabilities)))
              (set! writer
                (fork-thread
                  (lambda ()
                    (guard (ex [else (close!)])
                      (let loop ()
                        (let ([item (kernel:mailbox-receive! out)])
                          (when item
                            (let ([frame
                                   (if (bytevector? item) item
                                       (wire:encode (list 'changed ((with-mutex out-lock changes)))))])
                              (put-bytevector (sys:connection-output connection) frame)
                              (flush-output-port (sys:connection-output connection)))
                            (with-mutex out-lock
                              (set! queued-count (- queued-count 1))
                              (when (bytevector? item)
                                (set! queued-bytes (- queued-bytes (bytevector-length item)))))
                            (loop))))))))
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
                        (when (policy:revoked? session) (error 'wire "the session is revoked"))
                        (post!
                          (guard (ex [else (list 'reply (cadr message) 'error (kernel:condition-text ex))])
                            (list 'reply (cadr message) 'ok
                              (case (caddr message)
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
          (when session (policy:revoke! session))
          (kernel:retract-module! owner)
          (when writer (thread-join writer))))))

  (define (run)
    ;; Foreground for a supervisor or shell background job; SIGHUP leaves the
    ;; base alive. A process stop is distinct from a connection disconnect.
    (let* ([path (file:canonical (file:expand (startup:socket)))]
           [directory (file:directory-part path)]
           [control (kernel:make-mailbox)] [lock (make-mutex)] [finished (make-condition)]
           [connections '()] [stopping? #f] [acceptor #f])
      (unless (file-exists? directory) (mkdir directory #o700))
      (let ([listener (sys:listen-local path)])
        (dynamic-wind void
          (lambda ()
            (sys:watch-daemon-signals! (lambda () (kernel:mailbox-post! control 'stop)))
            (set! acceptor
              (fork-thread
                (lambda ()
                  (guard (ex [else (kernel:mailbox-post! control ex)])
                    (let loop ()
                      (let ([connection (sys:accept-local listener)])
                        (when connection
                          (with-mutex lock
                            (if stopping? (sys:close-connection! connection)
                                (begin
                                  (set! connections (cons connection connections))
                                  (fork-thread
                                    (lambda ()
                                      (dynamic-wind void
                                        (lambda () (serve-connection connection))
                                        (lambda ()
                                          (with-mutex lock
                                            (set! connections (remq connection connections))
                                            (condition-broadcast finished)))))))))
                          (loop))))))))
            (format #t "e: listening on ~a\n" path)
            (flush-output-port (current-output-port))
            ;; The control owner services OS signals while idle, through the
            ;; same mailbox wait as a head. No separate timer or polling loop.
            (let ([message (kernel:mailbox-receive! control #f #t)])
              (when (condition? message) (raise message))))
          (lambda ()
            (let ([active (with-mutex lock (set! stopping? #t) connections)])
              (sys:close-local-listener! listener)
              (for-each sys:close-connection! active)
              (when acceptor (thread-join acceptor))
              (with-mutex lock
                (let wait ()
                  (unless (null? connections)
                    ;; Client owners finish their endpoint and writer cleanup.
                    (condition-wait finished lock)
                    (wait))))))))))
)
