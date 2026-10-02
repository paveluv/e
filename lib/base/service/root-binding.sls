;; Persistent profile selection; view remains the sole mount-ownership authority.
(import (only (foundation edoc) elibrary))
(elibrary (service root-binding)
  (export acquire! admit! finish! resume! upgrade)
  (import (chezscheme)
          (prefix (core descriptor) descriptor:) (prefix (core handle) handle:) (prefix (core kernel) kernel:) (prefix (core operation) operation:)
          (prefix (service log) log:) (prefix (service policy) policy:) (prefix (state actor) actor:)
          (prefix (state model) model:) (prefix (state view) view:))

  (define (field r key) (cdr (assq key r)))
  (define (nonempty? x) (and (string? x) (> (string-length x) 0)))
  (define (valid? x)
    (and (list? x) (for-all pair? x)
         (equal? (map car x) '(profile attachment initialized? root cleanup))
         (nonempty? (field x 'profile))
         (or (not (field x 'attachment)) (nonempty? (field x 'attachment)))
         (boolean? (field x 'initialized?))
         (or (not (field x 'root)) (and (field x 'initialized?) (model:reference? (field x 'root))))
         (list? (field x 'cleanup)) (for-all handle:buffer? (field x 'cleanup))))
  (define registration
    (kernel:call-with-runtime-registrations
      (lambda () (model:register-kind! 'composition-binding 2 valid?))))
  (define attachments
    (unbox (kernel:persistent-cell 'composition-attachments
             (lambda () (cons (make-mutex) (make-weak-eq-hashtable))))))
  (define (attachment!)
    (let ([session (operation:attachment)] [actor (actor:current)])
      (unless (and (policy:session? session) (not (policy:revoked? session))
                   (descriptor:head? actor) (equal? actor (policy:session-actor session)))
        (error 'composition "expected an authenticated live head attachment"))
      (values session actor
        (with-mutex (car attachments)
          (or (hashtable-ref (cdr attachments) session #f)
              (let ([token (gensym->unique-string (gensym "composition"))])
                (hashtable-set! (cdr attachments) session token) token))))))
  (define (supported? r)
    (and r (eq? (field r 'kind) 'composition-binding) (= (field r 'schema) 2)
         (eq? (field r 'persistence) 'persistent) (valid? (field r 'value))))
  (define (slot actor profile)
    (unbox (kernel:persistent-cell (list 'composition-slot actor profile)
             (lambda ()
               (let ([found
                      (filter (lambda (r)
                                (and (equal? actor (field r 'scope))
                                  (begin
                                    (unless (supported? r)
                                      (error 'composition "saved profile schema is unavailable" (field r 'id)))
                                    (equal? profile (field (field r 'value) 'profile)))))
                        (map model:snapshot (model:ids 'composition-binding)))])
                 (when (> (length found) 1) (error 'composition "duplicate saved profile" profile))
                 (if (pair? found) (field (car found) 'id)
                   (model:create! actor 'composition-binding 2 actor 'persistent '()
                     (list (cons 'profile profile) '(attachment . #f) '(initialized? . #f) '(root . #f) '(cleanup)))))))))
  (define (change r attachment initialized? root cleanup)
    (list (field r 'id) (field r 'revision) (append (if root (list root) '()) cleanup)
      (list (cons 'profile (field (field r 'value) 'profile))
        (cons 'attachment attachment) (cons 'initialized? initialized?) (cons 'root root) (cons 'cleanup cleanup))))
  (define (reply id records)
    (values (find (lambda (r) (and r (equal? (field r 'id) id))) records)
      (map (lambda (r) (cons (field r 'id) (field r 'value)))
        (filter (lambda (r) (and r (eq? (field r 'kind) 'widget-view))) records))))

  (edoc "Acquire a persistent profile and its canonical root lease for the invoking attachment. Empty and uninitialized are distinct. Reacquiring from the same attachment is idempotent; another attachment fences every old view generation."
        (profile string "nonempty profile") (returns (values list list)))
  (define (acquire! profile)
    (unless (nonempty? profile) (error 'composition "expected a nonempty profile"))
    (let-values ([(session actor token) (attachment!)])
      (let ([id (slot actor profile)])
        (let retry ()
          (let ([r (model:snapshot id)])
            (unless (and (supported? r) (equal? (field r 'scope) actor))
              (error 'composition "saved binding is unavailable" id))
            (when (policy:revoked? session) (error 'composition "attachment was revoked"))
            (let* ([v (field r 'value)] [root (field v 'root)])
              (let-values ([(status records)
                            (view:exchange! actor root root #f (not (equal? token (field v 'attachment)))
                              (list (change r token (field v 'initialized?) root (field v 'cleanup))) #f)])
                (case status
                  [(stale) (retry)]
                  [(applied) (reply id records)]
                  [else (error 'composition "saved root cannot be acquired" status)]))))))))

  (edoc "Admit a prepared root against its binding. Retire atomically deletes the old owned model graph and records its output cleanup; otherwise require an existing retaining owner. Pending cleanup blocks another replacement. Borrowed sources survive."
        (expected list "acquired model envelope") (candidate (or model #f) "prepared root or explicit empty")
        (basis list "candidate descriptor tree") (disposition (or model #f (one-of retire)) "retire or existing retaining owner")
        (returns (values symbol datum list)))
  (define (admit! expected candidate basis disposition)
    (let-values ([(session actor token) (attachment!)])
      (unless (and (list? expected) (for-all pair? expected) (assq 'id expected)
                   (model:reference? (field expected 'id))) (error 'composition "expected a binding snapshot"))
      (let* ([id (field expected 'id)] [r (model:snapshot id)])
        (unless (and (supported? r) (equal? actor (field r 'scope)))
          (error 'composition "binding belongs to another head or is unavailable" id))
        (cond [(not (and (equal? expected r) (equal? token (field (field r 'value) 'attachment))))
               (values 'stale r '())]
          [(pair? (field (field r 'value) 'cleanup)) (values 'pending r '())]
          [else
           (let* ([old (field (field r 'value) 'root)]
                  [retiring? (and old (not (equal? old candidate)) (eq? disposition 'retire))]
                  [plan (and retiring? (view:disposal old))]
                  [owner (and old (not (equal? old candidate)) (model:reference? disposition) (model:snapshot disposition))])
             (when (and old (not (equal? old candidate))
                        (not retiring?)
                        (not (and owner (eq? (field owner 'persistence) 'persistent)
                                  (not (member disposition (list id old)))
                                  (member old (field owner 'references)))))
               (error 'composition "replacement requires an existing persistent owner of the previous root" old))
             (when (policy:revoked? session) (error 'composition "attachment was revoked"))
             (let-values ([(status records)
                           (view:exchange! actor old candidate basis #f
                             (cons (change r token #t candidate (if plan (cadr plan) '()))
                               (if owner (list (list disposition (field owner 'revision)
                                                 (field owner 'references) (field owner 'value))) '())) plan)])
               (let-values ([(binding rows) (reply id records)])
                 (values status binding rows))))]))))

  (define (clean! r)
    (let ([v (field r 'value)])
      ;; Canonical models are already gone. IDs never repeat, and the intent
      ;; survives both output deletion and an interruption before this commit.
      (let-values ([(status rows)
                    (view:finish-disposal! '(base composition) (field v 'cleanup)
                      (list (change r (field v 'attachment) (field v 'initialized?) (field v 'root) '())))])
        (values status (car rows)))))

  (edoc "Finish admitted output disposal at a head command boundary. An obsolete attachment or binding refuses before doing work. Repeating cleanup is harmless."
        (expected list "admitted binding snapshot") (returns (values symbol datum)))
  (define (finish! expected)
    (let-values ([(session actor token) (attachment!)])
      (unless (and (list? expected) (for-all pair? expected) (assq 'id expected)
                   (model:reference? (field expected 'id))) (error 'composition "expected a binding snapshot"))
      (let ([r (model:snapshot (field expected 'id))])
        (unless (and (supported? r) (equal? actor (field r 'scope)))
          (error 'composition "binding belongs to another head or is unavailable"))
        (if (and (equal? expected r) (equal? token (field (field r 'value) 'attachment)))
          (clean! r) (values 'stale r)))))

  (edoc "Resume base-owned output disposal after recovery or a head's departure. Failures retain their intent and are logged; they do not erase the admitted root or block unrelated profiles."
        (attachment (or (record session) #f) "departing attachment, or false for startup"))
  (define (resume! attachment)
    (let ([token (and attachment (with-mutex (car attachments) (hashtable-ref (cdr attachments) attachment #f)))])
      (for-each
        (lambda (id)
          (guard (ex [else (log:add! 'root-binding:resume! (list id (kernel:condition-text ex)))])
            (let retry ()
              (let ([r (model:snapshot id)])
                (when (and (supported? r) (or (not attachment)
                                            (and token (equal? token (field (field r 'value) 'attachment))
                                              (equal? (policy:session-actor attachment) (field r 'scope))))
                        (pair? (field (field r 'value) 'cleanup)))
                  (let-values ([(status current) (clean! r)])
                    (case status [(stale) (retry)] [(applied) (void)]
                      [else (error 'resume! "cleanup binding is unavailable" id)])))))))
        (model:ids 'composition-binding))))

  (edoc "Upgrade the previously supported binding schema by adding an empty cleanup intent; unknown schemas stay opaque."
        (r list "saved model envelope") (returns list))
  (define (upgrade r)
    (if (and (eq? (field r 'kind) 'composition-binding) (= (field r 'schema) 1))
      (map (lambda (p) (case (car p) [(schema) '(schema . 2)]
                         [(value) (cons 'value (append (cdr p) '((cleanup))))] [else p])) r) r))
)
