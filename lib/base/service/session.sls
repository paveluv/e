;; session.sls -- one recovery snapshot of text, models and named heads.
;; The lifecycle pauses writers; each state component owns its representation.
(import (only (foundation edoc) elibrary))
(elibrary (service session)
  (export restore! save! status take-notice!)
  (import (chezscheme)
          (prefix (core startup) startup:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (service root-binding) root-binding:)
          (prefix (service vt) vt:)
          (prefix (state actor) actor:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (state view) view:)
          (prefix (sys activity) activity:)
          (prefix (sys sys) sys:))

  (define format-version 3)
  (define lock (make-mutex))
  (define saved-at #f)
  (define restored-at #f)
  (define uncertain? #f)
  (define archives '())
  (define rejected-archive #f)
  (define notice-pending? #f)

  (define (upgrade-model r) (root-binding:upgrade (view:upgrade r)))

  (define (upgrade value)
    ;; Session 3 tags document identities. Convert only fields owned by a
    ;; known saved schema; extension payloads, Scheme code and user text are
    ;; opaque. Allocation counters and history revisions remain integers.
    (define (buffer n) (if n (list 'buffer n) #f))
    (define (field value key) (cond [(assq key value) => cdr] [else #f]))
    (define (update value key proc)
      (map (lambda (p) (if (eq? (car p) key) (cons key (proc (cdr p))) p)) value))
    (define (annotations value)
      (if (null? value) value (cons (buffer (car value)) (cdr value))))
    (define (layout node)
      (case (car node)
        [(split) (append (list-head node 4) (map layout (list-tail node 4)))]
        [(window)
         (if (= (length node) 8)
           (append (list-head node 7)
             (list (map (lambda (p) (cons (buffer (car p)) (cdr p))) (list-ref node 7)))) node)]
        [else node]))
    (define (checkpoint state)
      (if (and (list? state) (= (length state) 5) (eq? (car state) 'screen)
               (memv (cadr state) '(4 5 6)))
        (append (list-head state 3)
          (list (layout (list-ref state 3))
            (map (lambda (entry)
                   (let ([ref (car entry)])
                     (if (and (pair? ref) (eq? (car ref) 'shared))
                       (cons (cons* 'shared (buffer (cadr ref)) (cddr ref)) (cdr entry)) entry)))
              (list-ref state 4)))) state))
    (if (not (and (list? value) (memv (length value) '(6 7)) (eq? (car value) 'session)
                  (or (and (= (length value) 6) (equal? (cadr value) 1))
                      (and (= (length value) 7) (equal? (cadr value) 2))))) value
      (let ([models (if (= (length value) 7) (list-ref value 6) '(models 1))])
        (define (record id) (find (lambda (r) (equal? id (field r 'id))) (cddr models)))
        (define (review-query? id)
          (let* ([q (record id)] [source (and q (eq? (field q 'kind) 'collection) (equal? (field q 'schema) 1)
                                           (record (field (field q 'value) 'source)))])
            (and source (equal? (field source 'schema) 1)
              (memq (field source 'kind) '(conflict-review rewrite-draft)))))
        (define (selection value)
          (if (and (list? value) (= (length value) 3) (review-query? (car value))
                   (pair? (caddr value)) (integer? (caaddr value)))
            (list (car value) (cadr value) (cons (buffer (caaddr value)) (cdaddr value))) value))
        (define (model r)
          (if (not (equal? (field r 'schema) (if (eq? (field r 'kind) 'widget-view) 2 1))) r
            (update r 'value
              (lambda (v)
                (case (field r 'kind)
                  [(rewrite-draft markup-source git-source environment)
                   (update v 'document buffer)]
                  [(conflict-review)
                   (update (update v 'scope (lambda (xs) (map buffer xs))) 'records
                     (lambda (xs) (map (lambda (e) (update e 'document buffer)) xs)))]
                  [(review-preview)
                   (update (update (update v 'document buffer) 'annotations annotations) 'selection selection)]
                  [(widget-view)
                   (if (equal? (field v 'schema) 1)
                     (case (field v 'kind)
                       [(editor) (update v 'options (lambda (o) (update o 'annotations annotations)))]
                       [(table list) (update v 'state (lambda (s) (update s 'selection selection)))]
                       [(scroll) (update v 'state selection)]
                       [else v]) v)]
                  [else v])))))
        (list 'session format-version (caddr value) (cadddr value)
          (cons 'buffers (map (lambda (state) (cons (buffer (car state)) (cdr state))) (cdr (list-ref value 4))))
          (cons 'checkpoints (map (lambda (entry) (list (car entry) (checkpoint (cadr entry)))) (cdr (list-ref value 5))))
          (cons* 'models (cadr models) (map model (cddr models)))))))

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
                                    (model:valid-import? (cadr models) (map upgrade-model (cddr models))))))))))
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
             [value (datum:copy (read port))]
             [value (guard (ex [else #f]) (upgrade value))])
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
             (let ([models (list-ref value 6)]) (model:import! (cadr models) (map upgrade-model (cddr models))))
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
