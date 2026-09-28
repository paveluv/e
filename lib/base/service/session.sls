;; session.sls -- one recovery snapshot of text, models and named heads.
;; The lifecycle pauses writers; each state component owns its representation.
(import (only (foundation edoc) elibrary))
(elibrary (service session)
  (export restore! save! status take-notice!)
  (import (chezscheme)
          (prefix (core startup) startup:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (service vt) vt:)
          (prefix (state actor) actor:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (state view) view:)
          (prefix (sys activity) activity:)
          (prefix (sys sys) sys:))

  (define format-version 2)
  (define lock (make-mutex))
  (define saved-at #f)
  (define restored-at #f)
  (define uncertain? #f)
  (define archives '())
  (define rejected-archive #f)
  (define notice-pending? #f)

  (define (now)
    (let ([time (current-time 'time-utc)])
      (+ (* (time-second time) 1000000000) (time-nanosecond time))))

  (define (valid? value restoring?)
    (and (list? value) (memv (length value) '(6 7)) (eq? (car value) 'session)
         (or (and (equal? (cadr value) 1) (= (length value) 6))
             (and (equal? (cadr value) format-version) (= (length value) 7)
                  (let ([models (list-ref value 6)])
                    (and (list? models) (>= (length models) 2) (eq? (car models) 'models)
                         ;; Export owns valid envelopes. Do not invoke kind
                         ;; code while paused or let an unavailable payload
                         ;; prevent saving the rest of the session.
                         (or (not restoring?)
                             (and (model:valid-import? (cadr models) (cddr models))
                                  (guard (ex [else #f])
                                    (model:valid-import? (cadr models) (map view:upgrade (cddr models))))))))))
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
        (and (eof-object? (read port)) (valid? value #t) value))))

  (define (recovery-name? name)
    (or (string=? name "session.incompatible")
        (and (string:prefix? "session.incompatible." name)
             (> (string-length name) 21)
             (for-all char-numeric? (string->list (string:tail name 21))))))

  (edoc "Restore buffers, persistent models and named heads' checkpoints from the base directory.")
  (define (restore!)
    (let* ([directory (startup:base-working-directory)]
           [bytes (sys:call-with-private-input-file (string-append directory "/session")
                    (lambda (port)
                      (let ([bytes (get-bytevector-all port)])
                        (if (eof-object? bytes) #vu8() bytes))))]
           [value (and bytes (parse bytes))]
           [rejected #f]
           [retained (if (file-exists? directory)
                         (map (lambda (name) (string-append directory "/" name))
                           (list-sort string<? (filter recovery-name? (directory-list directory)))) '())])
      (cond
        [value
         ;; Configuration and listener binding have not happened. An
         ;; unexpected import error aborts this startup with session intact.
         (store:import! (cadddr value) (cdr (list-ref value 4)))
         (if (= (length value) 7)
             (let ([models (list-ref value 6)]) (model:import! (cadr models) (map view:upgrade (cddr models))))
             (model:import! 1 '()))
         (actor:import! (cdr (list-ref value 5)))]
        [bytes
         (set! rejected (sys:archive-session! directory))
         (set! retained (append retained (list rejected)))])
      (with-mutex lock
        (set! saved-at (and value (caddr value)))
        (set! restored-at saved-at)
        (set! archives retained)
        (set! rejected-archive rejected)
        (set! notice-pending? (or saved-at (pair? archives))))))

  (define (require-pause!)
    (unless (eq? (activity:phase) 'paused) (error 'session "saving requires a paused base")))

  (edoc "Save the session to the base directory atomically, during a lifecycle pause.")
  (define (save!)
    (require-pause!)
    (let-values ([(next-id buffers) (store:export vt:transcript)]
                 [(next-model-id models) (model:export)])
      (let* ([written-at (now)]
             [value (list 'session format-version written-at next-id
                      (cons 'buffers buffers) (cons 'checkpoints (actor:export))
                      (cons* 'models next-model-id models))])
        (unless (valid? value #f) (error 'session "current state cannot be saved"))
        (guard (ex [else
                    (when (sys:durability-uncertain? ex)
                      (with-mutex lock (set! saved-at written-at) (set! uncertain? #t)))
                    (raise ex)])
          (sys:write-session! (startup:base-working-directory)
            (lambda (port)
              (parameterize ([print-length #f] [print-level #f] [print-graph #f])
                (write value port) (newline port))))
          (with-mutex lock (set! saved-at written-at) (set! uncertain? #f))))))

  (edoc "When the session was saved and restored, whether it is uncertain, and its recovery archives."
        (returns list))
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

  (edoc "The pending session notice for the next head, once, or #f."
        (returns (or string #f)))
  (define (take-notice!)
    (with-mutex lock
      (and notice-pending?
           (begin
             (set! notice-pending? #f)
             (let ([older (- (length archives) (if rejected-archive 1 0))])
               (string-append
                 (if restored-at (format "e: restored a session saved ~a\n" (age restored-at)) "")
                 (if rejected-archive
                     (format "e: the saved session could not be restored; it is kept at ~a\n" rejected-archive) "")
                 (if (> older 0)
                     (format "e: ~a older recovery archive~a remain~a in ~a (session.incompatible*).\n"
                       older (if (= older 1) "" "s") (if (= older 1) "s" "") (path-parent (car archives)))
                     "")))))))
)
