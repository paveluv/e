;; session.sls -- one recovery snapshot of shared text and named views.
;; The lifecycle pauses writers; the store and VT own their representation.
(library (session)
  (export restore! save! discard! status take-notice!)
  (import (chezscheme) (prefix (store) store:) (prefix (actor) actor:)
          (prefix (vt) vt:) (prefix (sys) sys:) (prefix (startup) startup:)
          (prefix (activity) activity:) (prefix (datum) datum:)
          (prefix (string) string:))

  (define format-version 1)
  (define lock (make-mutex))
  (define saved-at #f)
  (define restored-at #f)
  (define uncertain? #f)
  (define archives '())
  (define notice-pending? #f)

  (define (now)
    (let ([time (current-time 'time-utc)])
      (+ (* (time-second time) 1000000000) (time-nanosecond time))))

  (define (valid? value)
    (and (list? value) (= (length value) 6) (eq? (car value) 'session)
         (equal? (cadr value) format-version)
         (integer? (caddr value)) (exact? (caddr value)) (>= (caddr value) 0)
         (list? (list-ref value 4)) (pair? (list-ref value 4))
         (eq? (car (list-ref value 4)) 'buffers)
         (list? (list-ref value 5)) (pair? (list-ref value 5))
         (eq? (car (list-ref value 5)) 'checkpoints)
         (store:valid-import? (cadddr value) (cdr (list-ref value 4)))
         (actor:valid-import? (cdr (list-ref value 5)))))

  (define (parse bytes)
    ;; The whole read has already succeeded. Only datum/format errors may
    ;; take the archive path; import and filesystem failures stay visible.
    (guard (ex [(or (lexical-violation? ex) (i/o-decoding-error? ex) (datum:invalid? ex)) #f]
               [else (raise ex)])
      (let* ([port (open-bytevector-input-port bytes
                     (make-transcoder (utf-8-codec) 'none 'raise))]
             [value (datum:copy (read port))])
        (and (eof-object? (read port)) (valid? value) value))))

  (define (recovery-name? name)
    (or (string=? name "session.incompatible")
        (and (string:prefix? "session.incompatible." name)
             (> (string-length name) 21)
             (for-all char-numeric? (string->list (string:tail name 21))))))

  (define (restore!)
    (let* ([directory (startup:base-working-directory)]
           [bytes (sys:call-with-private-input-file (string-append directory "/session")
                    (lambda (port)
                      (let ([bytes (get-bytevector-all port)])
                        (if (eof-object? bytes) #vu8() bytes))))]
           [value (and bytes (parse bytes))]
           [retained (if (file-exists? directory)
                         (map (lambda (name) (string-append directory "/" name))
                           (list-sort string<? (filter recovery-name? (directory-list directory)))) '())])
      (cond
        [value
         ;; Configuration and listener binding have not happened. An
         ;; unexpected import error aborts this startup with session intact.
         (store:import! (cadddr value) (cdr (list-ref value 4)))
         (actor:import! (cdr (list-ref value 5)))]
        [bytes (set! retained (append retained (list (sys:archive-session! directory))))])
      (with-mutex lock
        (set! saved-at (and value (caddr value)))
        (set! restored-at saved-at)
        (set! archives retained)
        (set! notice-pending? (or saved-at (pair? archives))))))

  (define (require-pause!)
    (unless (eq? (activity:phase) 'paused) (error 'session "saving requires a paused base")))

  (define (save!)
    (require-pause!)
    (let-values ([(next-id buffers) (store:export vt:transcript)])
      (let* ([written-at (now)]
             [value (list 'session format-version written-at next-id
                      (cons 'buffers buffers) (cons 'checkpoints (actor:export)))])
        (unless (valid? value) (error 'session "current state cannot be saved"))
        (guard (ex [else
                    (when (sys:durability-uncertain? ex)
                      (with-mutex lock (set! saved-at written-at) (set! uncertain? #t)))
                    (raise ex)])
          (sys:write-session! (startup:base-working-directory)
            (lambda (port)
              (parameterize ([print-length #f] [print-level #f] [print-graph #f])
                (write value port) (newline port))))
          (with-mutex lock (set! saved-at written-at) (set! uncertain? #f))))))

  (define (discard!)
    (require-pause!)
    (guard (ex [else
                (when (sys:durability-uncertain? ex)
                  (with-mutex lock (set! saved-at #f) (set! uncertain? #t)))
                (raise ex)])
      (sys:remove-session! (startup:base-working-directory))
      (with-mutex lock (set! saved-at #f) (set! uncertain? #f))))

  (define (status)
    (with-mutex lock
      (datum:copy (list (cons 'saved-at saved-at) (cons 'restored-at restored-at) (cons 'session-uncertain? uncertain?)
                    (cons 'recovery-archives archives)))))

  (define (age timestamp)
    (let* ([seconds (max 0 (div (- (now) timestamp) 1000000000))]
           [unit (cond [(>= seconds 86400) '(86400 . "day")]
                       [(>= seconds 3600) '(3600 . "hour")]
                       [(>= seconds 60) '(60 . "minute")]
                       [else '(1 . "second")])]
           [count (div seconds (car unit))])
      (format "~a ~a~a ago" count (cdr unit) (if (= count 1) "" "s"))))

  (define (take-notice!)
    (with-mutex lock
      (and notice-pending?
           (begin
             (set! notice-pending? #f)
             (string-append
               (if restored-at (format "e: restored a session saved ~a\n" (age restored-at)) "")
               (apply string-append
                 (map (lambda (path)
                        (format "e: a saved session could not be restored; it is kept at ~a\n" path)) archives)))))))
)
