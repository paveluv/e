;; client.sls -- one attached head's socket, reader, and main-pump delivery.
;; Replies never wait behind UI callbacks. Invalidations coalesce; actor
;; mail and log presentation have the same finite budget as the base outbox.
(library (client)
  (export call-with-runtime claim! identity request subscribe! unsubscribe!
          set-wake! pump! close! watch! ended? leave!)
  (import (chezscheme)
          (prefix (kernel) kernel:) (prefix (startup) startup:)
          (prefix (wire) wire:) (prefix (sys) sys:) (prefix (datum) datum:))

  (define-condition-type &ended &condition make-ended ended?)
  (define closing-reason #f)
  (define departure #f)

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
    (guard (ex [(or (ended? ex) (i/o-error? ex)) (close! 'gone)]
               [else (close! (kernel:condition-text ex))])
      (let loop ()
        (let ([message (wire:receive (sys:connection-input connection))])
          (when (eof-object? message) (raise (make-ended)))
          (unless (and (list? message) (pair? message)) (error 'client "invalid server message"))
          (if (and (= (length message) 2) (eq? (car message) 'closing)
                   (memq (cadr message) '(shutdown restart signal)))
              (begin
                (with-mutex lock (set! closing-reason (cadr message)))
                (close! "The base is closing"))
            (begin
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
              (loop)))))))

  (define (claim! actor path)
    (when connection (error 'client "this process already has a head"))
    (let retry ([actor actor] [suffix 2])
      (let ([next (sys:connect-local path)])
        (guard (ex [else (sys:close-connection! next) (raise ex)])
          (let ([hello
                 (sys:call-with-connection-deadline next
                   (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 10))
                   (lambda ()
                     (wire:send! (sys:connection-output next) (list 'hello wire:version actor))
                     (wire:receive (sys:connection-input next))))])
            (cond
              [(equal? hello '(error #f name-in-use))
               (sys:close-connection! next)
               (if (startup:name) (error 'e "head name already in use" (startup:name))
                   (retry (list 'head (string-append (startup:default-name) " " (number->string suffix)))
                          (+ suffix 1)))]
              [(and (list? hello) (= (length hello) 3) (eq? (car hello) 'error)
                    (list? (caddr hello)) (= (length (caddr hello)) 2) (eq? (caaddr hello) 'busy))
               (error 'e (format "the base is ~a; retry after the review finishes" (cadr (caddr hello))))]
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
                          (if failure (raise (make-ended)) (error 'client "head is not attached")))
                        (set! serial (+ serial 1))
                        (set! waiting serial)
                        (set! reply #f)
                        serial)])
              (wire:send! (sys:connection-output connection) (append (list 'request id operation) args))
              (let ([result
                     (with-mutex lock
                       (let wait ()
                         (cond [failure (raise (make-ended))]
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
               (when failure (raise (make-ended)))
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

  (define (shell-quote text)
    (string-append "'" (apply string-append
                         (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))

  (define (leave! shutdown-on-exit?)
    (let ([result (request 'leaving shutdown-on-exit?)])
      (unless (and (pair? result) (eq? (car result) 'last)) (set! departure result))
      result))

  (define (farewell status)
    (define (count key noun)
      (let ([n (cdr (assq key status))]) (format "~a ~a~a" n noun (if (= n 1) "" "s"))))
    (format #t "e: detached; the base holds ~a (~a modified), ~a, ~a and ~a.\n"
      (count 'buffers "buffer") (cdr (assq 'modified status))
      (count 'heads "other head") (count 'terminals "running terminal") (count 'agents "agent"))
    (format #t "Resume: ~a --name ~a --base-working-dir ~a\n"
      (shell-quote (string-append (kernel:installation-directory) "/e"))
      (shell-quote (cadr who)) (shell-quote (startup:base-working-directory)))
    (display "Stop the base: M-x (main:shutdown!!)\n"))

  (define (call-with-runtime thunk)
    (let ([modules '("activity" "actor" "daemon" "datum" "diff" "doc" "file" "git" "https" "identity" "journal" "log" "path"
                     "property" "reference" "startup" "store" "string" "surface" "sys" "text" "vt" "wire")])
      (kernel:pin-modules! (cons* "client" "cache" modules))
      (guard (ex [(ended? ex)
                  ;; run-head has already restored the terminal, including
                  ;; when the connection failed inside a nested command.
                  (display
                    (case closing-reason
                      [(shutdown) "e: the base shut down\n"]
                      [(restart) "e: the base is restarting; run e to reattach\n"]
                      [(signal) "e: the base stopped (signal)\n"]
                      [else (if (eq? failure 'gone) "e: the base is gone\n"
                                (format "e: ~a\n" failure))]))
                  (if closing-reason 0 1)])
        (dynamic-wind void
          (lambda ()
            (let ([failures (kernel:load-modules! modules)])
              (unless (null? failures) (raise (cdar failures))))
            (thunk)
            (farewell (or departure (leave! #f)))
            0)
          (lambda () (close!) (when reader (thread-join reader)))))))
)
