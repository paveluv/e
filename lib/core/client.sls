;; client.sls -- one attached head's socket, reader, and main-pump delivery.
;; Replies never wait behind UI callbacks. Invalidations coalesce; actor
;; mail and log presentation have the same finite budget as the base outbox.
(library (client)
  (export call-with-runtime claim! identity request subscribe! unsubscribe!
          set-wake! pump! close! watch!)
  (import (chezscheme)
          (prefix (kernel) kernel:) (prefix (startup) startup:)
          (prefix (wire) wire:) (prefix (sys) sys:) (prefix (datum) datum:))

  (define connection #f)
  (define reader #f)
  (define who #f)
  (define pump-thread #f)
  (define lock (make-mutex))
  (define requests (make-mutex))
  (define ready (make-condition))
  (define serial 0)
  (define waiting #f)
  (define reply #f)
  (define failure #f)
  (define wake void)
  (define notices '())
  (define count 0)
  (define bytes 0)
  (define changes '())
  (define surfaces '())
  (define presence? #f)
  (define subscriptions (kernel:make-registry car))
  (define deliveries (kernel:make-delivery-queue))

  (define (identity) (datum:copy who))
  (define (set-wake! procedure) (with-mutex lock (set! wake procedure)))

  (define (close! . reason)
    (let ([notify
           (with-mutex lock
             (unless failure
               (set! failure (if (pair? reason) (car reason) "Disconnected from the base")))
             (when connection (sys:close-connection! connection))
             (condition-broadcast ready)
             wake)])
      (notify)))

  (define (merge-pending old next)
    ;; Boolean metadata flags merge; surface headers replace older ones.
    ;; #f for the entire batch means rescan.
    (and old next
         (let merge ([rest next] [out old])
           (cond [(null? rest) out]
             [(assv (caar rest) out)
              => (lambda (entry)
                   (merge (cdr rest)
                     (cons (cons (car entry)
                             (if (boolean? (cdar rest)) (or (cdr entry) (cdar rest)) (cdar rest)))
                           (remq entry out))))]
             [(>= (length out) 256) #f]
             [else (merge (cdr rest) (cons (car rest) out))]))))

  (define (receive!)
    (guard (ex [else (close! (kernel:condition-text ex))])
      (let loop ()
        (let ([message (wire:receive (sys:connection-input connection))])
          (when (eof-object? message) (error 'client "base closed the connection"))
          (unless (and (list? message) (pair? message)) (error 'client "invalid server message"))
          (let ([notify
                 (with-mutex lock
                   (case (car message)
                     [(reply)
                      (unless (and (= (length message) 4) (equal? (cadr message) waiting) (not reply))
                        (error 'client "unexpected reply"))
                      (set! reply message)
                      (condition-signal ready)
                      void]
                     [(changed)
                      (set! changes (merge-pending changes (cadr message))) wake]
                     [(surface)
                      (set! surfaces (merge-pending surfaces (cadr message))) wake]
                     [(presence) (set! presence? #t) wake]
                     [(event logged)
                      (let ([size (bytevector-length (wire:encode message))])
                        (when (or (>= count 256) (> (+ bytes size) #x2000000))
                          (error 'client "pending input limit reached"))
                        (set! notices (cons message notices))
                        (set! count (+ count 1))
                        (set! bytes (+ bytes size)))
                      wake]
                     [else (error 'client "unknown server message" (car message))]))])
            (notify))
          (loop)))))

  (define (claim! actor path)
    (when connection (error 'client "this process already has a head"))
    (let retry ([actor actor] [suffix 2])
      (let ([next (sys:connect-local path)])
        (guard (ex [else (sys:close-connection! next) (raise ex)])
          (wire:send! (sys:connection-output next) (list 'hello wire:version actor))
          (let ([hello (wire:receive (sys:connection-input next))])
            (cond
              [(equal? hello '(error #f name-in-use))
               (sys:close-connection! next)
               (if (startup:name) (error 'e "head name already in use" (startup:name))
                   (retry (list 'head (string-append (startup:default-name) " " (number->string suffix)))
                          (+ suffix 1)))]
              [(and (list? hello) (= (length hello) 4)
                    (equal? (list-head hello 3) (list 'hello wire:version actor)))
               (set! connection next)
               (set! who (datum:copy actor))
               (set! pump-thread (get-thread-id))
               (set! reader (fork-thread receive!))
               (identity)]
              [else (error 'client "base refused attachment" hello)]))))))

  (define (request operation . args)
    ;; Exactly one call in flight. An interrupted call closes the socket:
    ;; an unknown commit is never replayed on this or a replacement session.
    (with-mutex requests
      (let ([completed? #f])
        (dynamic-wind void
          (lambda ()
            (let ([id (with-mutex lock
                        (unless (and connection (not failure))
                          (error 'client (or failure "head is not attached")))
                        (set! serial (+ serial 1))
                        (set! waiting serial)
                        (set! reply #f)
                        serial)])
              (wire:send! (sys:connection-output connection) (append (list 'request id operation) args))
              (let ([result
                     (with-mutex lock
                       (let wait ()
                         (cond [failure (error 'client failure)]
                           [reply (set! waiting #f) reply]
                           [else (condition-wait ready lock) (wait)])))])
                (set! completed? #t)
                (if (eq? (caddr result) 'ok) (cadddr result)
                    (error operation (cadddr result))))))
          (lambda () (unless completed? (close! "Connection interrupted; reattach to inspect the base")))))))

  (define (subscribe! kind procedure)
    (let ([token (list kind procedure)])
      (kernel:registry-add! subscriptions (list token kind procedure)) token))
  (define (unsubscribe! token)
    (kernel:registry-remove! subscriptions (lambda (entry) (eq? (car entry) token))))

  (define (watch! kind notify)
    (let ([pending '()])
      (let ([token (subscribe! kind
                     (lambda (batch)
                       (set! pending (merge-pending pending batch))
                       (notify)))])
        (values token
          (lambda ()
            (pump!)
            (let ([batch pending]) (set! pending '()) batch))))))

  (define (pump!)
    ;; Output workers can append a log record, but its delivery must not
    ;; drain store/UI callbacks there. The reader already wakes the head.
    (when (eqv? pump-thread (get-thread-id))
      (let ([batch
             (with-mutex lock
               (when failure (error 'client failure))
               (let ([batch (append
                              (if (equal? changes '()) '() (list (list 'changed changes)))
                              (if (equal? surfaces '()) '() (list (list 'surface surfaces)))
                              (if presence? '((presence)) '())
                              (reverse notices))])
                 (set! changes '()) (set! surfaces '()) (set! presence? #f)
                 (set! notices '()) (set! count 0) (set! bytes 0)
                 batch))])
        (for-each
          (lambda (message)
            (for-each
              (lambda (entry)
                (when (eq? (cadr entry) (car message))
                  (kernel:enqueue-delivery! deliveries
                    (lambda ()
                      (when (kernel:registry-find subscriptions (lambda (current) (eq? current entry)))
                        (apply (caddr entry) (datum:copy (cdr message))))))))
              (reverse (kernel:registry-items subscriptions)))) batch)
        ;; The common queue preserves order across callback reentry, ignores
        ;; a retracted recipient, isolates failures and leaves config staging.
        (kernel:drain-deliveries! deliveries))))

  (define (call-with-runtime thunk)
    (let ([modules '("actor" "datum" "diff" "doc" "file" "git" "https" "identity" "journal" "log" "path"
                     "property" "reference" "startup" "store" "string" "surface" "sys" "text" "vt" "wire")])
      (kernel:pin-modules! (cons "client" modules))
      (dynamic-wind void
        (lambda ()
          ;; These are the same prefixed names available to configuration
          ;; and M-x in a standalone head, bound to client service libraries.
          (let ([failures (kernel:load-modules! modules)])
            (unless (null? failures) (raise (cdar failures))))
          (thunk))
        (lambda () (close!) (when reader (thread-join reader))))))
)
