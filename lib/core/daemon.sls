;; daemon.sls -- installation-local process ownership and head bootstrap.
;; No store or head state: enter this lifetime before importing either runtime.
(library (daemon)
  (export call-with-base call-with-head socket rotate-logs! log-deadline control
          call-with-stop take-stop-signal! help head-command status-summary report-start!)
  (import (chezscheme) (prefix (startup) startup:) (prefix (sys) sys:)
          (prefix (kernel) kernel:) (prefix (string) string:) (prefix (wire) wire:))

  (define (socket) (string-append (startup:base-working-directory) "/socket"))
  (define starting-process (make-thread-parameter #f))
  (define log-day #f)
  (define next-rotation #f)
  (define control (kernel:make-mailbox))
  (define signal-lock (make-mutex))
  (define signal-generation 0)
  (define signal-pending? #f)
  (define stop-accepted? #f)

  (define (post-stop-signal!)
    (with-mutex signal-lock
      (unless (or signal-pending? stop-accepted?)
        (set! signal-pending? #t)
        (kernel:mailbox-post! control (cons 'signal signal-generation)))))

  (define (take-stop-signal! message)
    (with-mutex signal-lock
      (and (equal? message (cons 'signal signal-generation)) signal-pending?
           (begin (set! signal-pending? #f) #t))))

  (define (call-with-stop thunk)
    ;; Acceptance absorbs queued signals too. On failure a later, newly
    ;; received signal may retry, but a burst never starts a second writer.
    (dynamic-wind
      (lambda ()
        (with-mutex signal-lock
          (set! stop-accepted? #t) (set! signal-pending? #f)
          (set! signal-generation (+ signal-generation 1))))
      thunk
      (lambda () (with-mutex signal-lock (set! stop-accepted? #f)))))
  (define (log-deadline) next-rotation)

  (define (day-name date)
    (format "~4,'0d-~2,'0d-~2,'0d" (date-year date) (date-month date) (date-day date)))

  (define (rotate-logs!)
    (let* ([now (current-time 'time-utc)] [today (time-utc->date now)]
           [day (day-name today)]
           [directory (string-append (startup:base-working-directory) "/log")]
           ;; Advance the civil date in UTC, then interpret its midnight in
           ;; the local zone. Month ends and DST need no special branches.
           [tomorrow (time-utc->date
                       (add-duration (date->time-utc (make-date 0 0 0 0
                                                       (date-day today) (date-month today) (date-year today) 0))
                                     (make-time 'time-duration 0 86400)) 0)]
           [midnight (date->time-utc (make-date 0 0 0 0
                                       (date-day tomorrow) (date-month tomorrow) (date-year tomorrow)))])
      (unless (equal? day log-day)
        (sys:ensure-private-directory! directory)
        (sys:redirect-daemon-ports! (string-append directory "/" day ".log") (not log-day))
        (set! log-day day)
        (let ([oldest (day-name (time-utc->date
                                  (subtract-duration now (make-time 'time-duration 0 (* 14 86400)))))])
          (for-each
            (lambda (name)
              (when (and (= (string-length name) 14) (string:suffix? ".log" name)
                         (char=? (string-ref name 4) #\-) (char=? (string-ref name 7) #\-)
                         (for-all char-numeric?
                           (string->list (string-append (substring name 0 4) (substring name 5 7) (substring name 8 10))))
                         (string<? (substring name 0 10) oldest))
                (delete-file (string-append directory "/" name))))
            (directory-list directory))))
      (set! next-rotation (add-duration (current-time 'time-monotonic) (time-difference midnight now)))))

  (define (call-with-base thunk)
    (let* ([directory (startup:base-working-directory)]
           [pid-path (string-append directory "/pid")])
      (sys:ensure-private-directory! directory)
      (let ([lock (sys:acquire-file-lock (string-append directory "/lock"))] [published? #f])
        (if (not lock)
            (begin (format (current-error-port) "e: a base already owns ~a\n" directory) 3)
            (dynamic-wind void
              (lambda ()
                (sys:watch-daemon-signals! post-stop-signal!)
                (sys:remove-stale-socket! (socket))
                ;; The record is diagnostic identity, never ownership proof:
                ;; only this still-open flock permits endpoint changes.
                (sys:call-with-private-output-file pid-path
                  (lambda (port)
                    (set! published? #t)
                    (write (cons 'base (sys:process-identity)) port) (newline port)))
                (rotate-logs!)
                (current-directory directory)
                (guard (ex [else
                            (format (current-error-port) "e: ~a\n" (kernel:condition-text ex))
                            (flush-output-port (current-error-port)) 1])
                  (thunk) 0))
              (lambda ()
                (dynamic-wind void
                  (lambda () (when published? (delete-file pid-path)))
                  (lambda () (sys:release-file-lock! lock)))))))))

  (define (latest-diagnostics)
    (guard (ex [else ""])
      (let* ([directory (string-append (startup:base-working-directory) "/log")]
             [names (list-sort string>?
                      (filter (lambda (name) (string:suffix? ".log" name)) (directory-list directory)))])
        (if (null? names) ""
            (call-with-input-file (string-append directory "/" (car names))
              (lambda (port)
                (let read-tail ([lines '()])
                  (let ([line (get-line port)])
                    (if (eof-object? line) (string:join (reverse lines) "\n")
                        (read-tail (cons line (if (>= (length lines) 20) (list-head lines 19) lines))))))))))))

  (define (after seconds)
    (add-duration (current-time 'time-monotonic) (make-time 'time-duration 0 seconds)))

  (define (shell-quote text)
    (string-append "'" (apply string-append
                         (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list text))) "'"))

  (define (head-command name restart?)
    (let ([directory (startup:base-working-directory)])
      (format "~a~a~a~a"
        (shell-quote (string-append (kernel:installation-directory) "/e"))
        (if restart? " --restart" "") (if name (string-append " --name " (shell-quote name)) "")
        (if (string=? directory (startup:default-base-working-directory)) ""
            (string-append " --base-working-dir " (shell-quote directory))))))

  (define (guidance status port)
    (format port "Stop the base: M-x (main:shutdown!!) or kill -TERM ~a (save session)\n"
      (car (cdr (assq 'instance status)))))

  (define (status-summary status head-noun)
    (define (count key noun)
      (let ([n (cdr (assq key status))]) (format "~a ~a~a" n noun (if (= n 1) "" "s"))))
    (format "~a (~a modified), ~a, ~a and ~a"
      (count 'buffers "buffer") (cdr (assq 'modified status))
      (count 'heads head-noun) (count 'terminals "running terminal") (count 'agents "agent")))

  (define (base-description status)
    (format "pid ~a; wire ~a~a"
      (car (cdr (assq 'instance status))) (cdr (assq 'wire-version status))
      (cond [(assq 'fingerprint status) => (lambda (entry) (format "; source ~a" (cdr entry)))]
            [else ""])))

  (define (report-start! get-status)
    ;; Only the launcher whose live child won ownership announces startup.
    ;; A normal reattachment does not make an extra status request.
    (let ([child (starting-process)])
      (when (and child (not (sys:process-status child)))
        (let ([status (get-status)])
          (when (= (sys:process-pid child) (car (cdr (assq 'instance status))))
            (format (current-error-port) "e: started base (~a) in ~s\n"
              (base-description status) (startup:base-working-directory))
            (guidance status (current-error-port))
            (format (current-error-port) "Status and resume commands: ~a --help\n" (head-command #f #f))
            (flush-output-port (current-error-port)))))))

  (define (help)
    (define (show-status line)
      (format #t "\n~a\nBase directory: ~s\n" line (startup:base-working-directory)))
    (display "Usage: e [--restart [--force]] [--name NAME] [--base-working-dir DIR] [--] [file]\n")
    (display "       e --base [--base-working-dir DIR]\n")
    (display "       e --help [--base-working-dir DIR]\n")
    (display "A tiny, fully customizable, self-aware, Emacs-like editor.\n")
    (display "Head names default to user@host:tty (pid without a terminal).\n")
    ;; Help never creates a directory, launches a base, claims a name or
    ;; consumes the recovery notice. One bounded maintenance read suffices.
    (guard (ex [else (show-status (format "Base status unavailable: ~a" (kernel:condition-text ex)))])
      (let* ([deadline (after 2)] [connection (sys:try-connect-local (socket) deadline)])
        (if (not connection)
            (show-status "Base: no base is listening.")
            (dynamic-wind void
              (lambda ()
                (let ([hello (sys:call-with-connection-deadline connection deadline
                               (lambda ()
                                 (wire:send! (sys:connection-output connection)
                                   (list 'maintenance 1 (list 'head (or (startup:name) (startup:default-name)))))
                                 (wire:receive (sys:connection-input connection))))])
                  (unless (and (list? hello) (= (length hello) 3) (equal? (list-head hello 2) '(maintenance 1)))
                    (error 'help "base refused status" hello))
                  (let ([status (caddr hello)])
                    (show-status (format "Base: alive (~a); ~a" (base-description status) (cdr (assq 'phase status))))
                    (guidance status (current-output-port))
                    (format #t "The base holds ~a.\n" (status-summary status "attached head"))
                    (cond
                      [(assq 'head-states status)
                       => (lambda (entry)
                            (display "Heads:\n")
                            (if (null? (cdr entry)) (display "  none\n")
                                (for-each
                                  (lambda (head)
                                    (format #t "  ~a ~s\n" (cadr head) (car head))
                                    (when (eq? (cadr head) 'detached)
                                      (format #t "    Resume: ~a\n" (head-command (car head) #f))))
                                  (list-sort (lambda (a b) (string<? (car a) (car b))) (cdr entry)))))]
                      [else (format #t "Heads: ~a attached; names unavailable from this base.\n" (cdr (assq 'heads status)))]))))
              (lambda () (sys:close-connection! connection))))))
    (flush-output-port (current-output-port)))

  (define (lock-free? directory)
    (let ([lock (sys:acquire-file-lock (string-append directory "/lock"))])
      (and lock (begin (sys:release-file-lock! lock) #t))))

  (define (wait-for-exit! instance deadline)
    (let wait ()
      (unless (sys:process-exited? instance)
        (when (time>=? (current-time 'time-monotonic) deadline)
          (error 'restart "the stopped base has not exited; inspect its diagnostics before retrying"))
        (sleep (make-time 'time-duration 50000000 0)) (wait))))

  (define (force-restart! directory)
    (let ([deadline (after 120)])
      (sys:call-with-verified-base directory
        (lambda (signal! wait!)
          (display "e: the base is unresponsive; sending SIGTERM to the verified instance\n" (current-error-port))
          (flush-output-port (current-error-port))
          (signal! 15)
          (unless (wait! (after 10))
            (display "e: sending SIGKILL to that same instance; work newer than the last snapshot is lost\n" (current-error-port))
            (flush-output-port (current-error-port))
            (signal! 9)
            (unless (wait! deadline) (error 'restart "the verified base did not exit; manual recovery is required")))))))

  (define-condition-type &unanswered &error make-unanswered unanswered?)

  (define (restart! directory)
    (let ([connection #f] [token #f] [accepting? #f] [serial 0] [instance #f])
      (define (read!)
        (let ([message (wire:receive (sys:connection-input connection))])
          (when (eof-object? message)
            (raise (condition (make-unanswered) (make-message-condition "the base closed the maintenance connection"))))
          message))
      (define (exchange! message seconds)
        (guard (ex [(i/o-error? ex)
                    (raise (condition (make-unanswered) (make-message-condition (kernel:condition-text ex))))]
                   [else (raise ex)])
          (sys:call-with-connection-deadline connection (after seconds)
            (lambda () (wire:send! (sys:connection-output connection) message) (read!)))))
      (define (request! operation . args)
        (set! serial (+ serial 1))
        (let ([reply (exchange! (append (list 'request serial operation) args)
                       (if (eq? operation 'restart) 120 10))])
          (cond [(equal? reply '(closing restart)) 'stopped]
                [(and (list? reply) (= (length reply) 4) (eq? (car reply) 'reply) (equal? (cadr reply) serial))
                 (set! accepting? #f)
                 (if (eq? (caddr reply) 'ok) (cadddr reply)
                     (error operation "the base refused the operation" (cadddr reply)))]
                [else (error 'restart "unexpected maintenance reply; inspect the base before retrying" reply)])))
      (define (review! review)
        (unless (and (list? review) (= (length review) 4) (eq? (car review) 'review))
          (error 'restart "invalid restart review" review))
        (set! token (cadr review))
        (let* ([status (cadddr review)]
               [counts (map (lambda (key)
                              (let ([entry (assq key status)])
                                (unless (and entry (integer? (cdr entry)) (exact? (cdr entry)) (>= (cdr entry) 0))
                                  (error 'restart "invalid maintenance status" status))
                                (cdr entry))) '(heads terminals agents pending))])
          (format (current-error-port)
            "e: restart keeps shared text and named views. Undo/redo history, local drafts and pending interactions are not kept; terminal processes and agent sessions end.\n")
          (let ([agreed?
                 (or (for-all zero? counts)
                     (begin
                       (apply format (current-error-port)
                         "The base has ~a head(s), ~a terminal process(es), ~a agent session(s) and ~a pending interaction(s). Restart anyway? [y/N] " counts)
                       (flush-output-port (current-error-port))
                       (let ([answer (get-line (current-input-port))])
                         (and (string? answer) (member (string-downcase answer) '("y" "yes"))))))])
            (flush-output-port (current-error-port))
            (and agreed?
                 (begin
                   ;; From this write until an explicit reply, transport loss
                   ;; has an unknown outcome. Neither replay nor force is safe.
                   (set! accepting? #t)
                   (let ([result (request! 'restart token)])
                     (if (eq? result 'stopped)
                         (begin (set! accepting? #f) (set! token #f) (wait-for-exit! instance (after 120)) #t)
                         (begin
                           (display "e: new live work appeared; review the restart again\n" (current-error-port))
                           (review! result)))))))))
      (guard (ex [(and (not accepting?) (or (sys:unresponsive? ex) (unanswered? ex)))
                  (if (startup:force?) (begin (force-restart! directory) #t) (raise ex))]
                 [accepting? (error 'restart "restart outcome is unknown; inspect the base before retrying" (kernel:condition-text ex))]
                 [else (raise ex)])
        (dynamic-wind void
          (lambda ()
            (set! connection (sys:try-connect-local (socket) (after 10)))
            (if (not connection)
                (begin (when (startup:force?) (force-restart! directory)) #t)
                (let ([hello (exchange! (list 'maintenance 1 (list 'head (or (startup:name) (startup:default-name)))) 10)])
                  (unless (and (list? hello) (= (length hello) 3) (equal? (list-head hello 2) '(maintenance 1))
                               (list? (caddr hello)) (assq 'instance (caddr hello)))
                    (error 'restart "base refused maintenance" hello))
                  (set! instance (cdr (assq 'instance (caddr hello))))
                  (unless (and (list? instance) (= (length instance) 2)
                               (integer? (car instance)) (exact? (car instance)) (> (car instance) 0))
                    (error 'restart "invalid base instance" instance))
                  (review! (request! 'prepare-restart)))))
          (lambda ()
            (when connection
              (unless (or accepting? (not token))
                (guard (ex [else (void)]) (request! 'cancel-review token)))
              (sys:close-connection! connection)))))))

  (define (attach-or-start thunk)
    (let* ([directory (startup:base-working-directory)] [child #f]
           [started (current-time 'time-monotonic)]
           [deadline (add-duration started (make-time 'time-duration 0 120))]
           [notice-at (add-duration started (make-time 'time-duration 0 2))] [noticed? #f])
      ;; A foreign/unsafe existing directory is a hard error, even when its
      ;; socket answers. Only absent/refused endpoints permit a spawn.
      (sys:ensure-private-directory! directory)
      (dynamic-wind void
        (lambda ()
          (let wait ()
            (let ([connection (sys:try-connect-local (socket) deadline)])
              (if connection
                  (sys:close-connection! connection)
                  (begin
                    (when (and (not child) (lock-free? directory))
                      (set! child (sys:open-process
                                    (list (string-append (kernel:installation-directory) "/e")
                                      "--base" "--base-working-dir" directory)))
                      (sys:write-process! child #f))
                    (cond [(and child (sys:process-status child))
                           => (lambda (status)
                                (unless (= status 3)
                                  (get-bytevector-all (sys:process-input child))
                                  (let-values ([(code errors) (sys:process-result child)])
                                    (error 'e "could not start the base" code errors (latest-diagnostics))))
                                (sys:release-process! child)
                                (set! child #f))])
                    (when (and (not noticed?) (time>=? (current-time 'time-monotonic) notice-at))
                      (display "e: starting the base ...\n" (current-error-port))
                      (flush-output-port (current-error-port))
                      (set! noticed? #t))
                    (sleep (make-time 'time-duration 50000000 0))
                    (wait)))))
          (parameterize ([starting-process child]) (thunk)))
        (lambda () (when child (sys:release-process! child))))))

  (define (call-with-head thunk)
    (sys:ensure-private-directory! (startup:base-working-directory))
    (if (and (startup:restart?) (not (restart! (startup:base-working-directory)))) 0
        (attach-or-start thunk)))
)
