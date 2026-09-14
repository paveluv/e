;; daemon.sls -- installation-local process ownership and head bootstrap.
;; No store or head state: enter this lifetime before importing either runtime.
(library (daemon)
  (export call-with-base call-with-head socket rotate-logs! log-deadline control)
  (import (chezscheme) (prefix (startup) startup:) (prefix (sys) sys:)
          (prefix (kernel) kernel:) (prefix (string) string:))

  (define (socket) (string-append (startup:base-working-directory) "/socket"))
  (define log-day #f)
  (define next-rotation #f)
  (define control (kernel:make-mailbox))
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
                (sys:watch-daemon-signals! (lambda () (kernel:mailbox-post! control 'signal)))
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

  (define (call-with-head thunk)
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
                    (unless child
                      (set! child (sys:open-process
                                    (list (string-append (kernel:installation-directory) "/e")
                                      "--base" "--base-working-dir" directory)))
                      (sys:write-process! child #f))
                    (cond [(sys:process-status child)
                           => (lambda (status)
                                (unless (= status 3)
                                  (get-bytevector-all (sys:process-input child))
                                  (let-values ([(code errors) (sys:process-result child)])
                                    (error 'e "could not start the base" code errors (latest-diagnostics)))))])
                    (when (and (not noticed?) (time>=? (current-time 'time-monotonic) notice-at))
                      (display "e: starting the base ...\n" (current-error-port))
                      (flush-output-port (current-error-port))
                      (set! noticed? #t))
                    (sleep (make-time 'time-duration 50000000 0))
                    (wait)))))
          (thunk))
        (lambda () (when child (sys:release-process! child))))))
)
