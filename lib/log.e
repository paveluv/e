;; log.e -- the structured log and audit stream: the library (log).
;; State, not UI: pure infrastructure with no init!.
;;
;; The editor's syslog: structured records -- (time component datum),
;; time with nanosecond precision -- indexed in a growable vector,
;; appended by log! and by every message that passes through the echo
;; area.  The records ride a persistent cell, so the log survives
;; module reloads; presentation is the head's business, installed
;; through set-presenter! (the command layer shows entries in the echo
;; area; the log-view module renders the *log* view from the records
;; themselves).
;;
;; Per-component presentation of structured entries: modules register
;; a formatter (datum -> string) and optionally a styler (formatted
;; text -> styles vector), used identically in the echo area and the
;; *log* view.  The datum itself stays queriable -- eval logs
;; (query . result) and its history reads only the queries.

(library (log)
  (export (rename (log! add!)) (rename (log-record record)) (rename (log-length length)) (rename (log-entries entries)) (rename (log-history history))
          (rename (register-log-formatter! register-formatter!)) (rename (log-styler styler)) (rename (format-log-entry format-entry))
          set-presenter!)
  (import (rnrs)
          (only (chezscheme) box unbox set-box! format current-time void)
          (prefix (kernel) kernel:))

  ;;; The records ---------------------------------------------------------------

  ;; (vector . count) in a persistent cell: reloading this module
  ;; keeps every record.
  (define log-cell
    (kernel:persistent-cell 'log-store
      (lambda () (cons (make-vector 64 #f) 0))))

  (define (log-record i) (vector-ref (car (unbox log-cell)) i))
  (define (log-length) (cdr (unbox log-cell)))

  (define (append-record! e)
    (let* ([store (unbox log-cell)]
           [v (car store)]
           [count (cdr store)]
           [v (if (= count (vector-length v))
                  (let ([bigger (make-vector (* 2 (vector-length v)) #f)])
                    (do ([i 0 (+ i 1)]) ((= i count))
                      (vector-set! bigger i (vector-ref v i)))
                    bigger)
                  v)])
      (vector-set! v count e)
      (set-box! log-cell (cons v (+ count 1)))))

  ;;; Formatters ----------------------------------------------------------------

  (define log-formatters (kernel:make-registry))

  (define (register-log-formatter! component fmt . style)
    (kernel:registry-add! log-formatters
                          (list component fmt
                                (and (pair? style) (car style)))))

  (define (log-formatter component)
    (kernel:registry-find log-formatters
                          (lambda (x) (eq? (car x) component))))

  (define (log-styler component)
    ;; The component's registered styler (formatted text -> styles
    ;; vector), or #f -- views style their rows with it.
    (let ([f (log-formatter component)]) (and f (caddr f))))

  (define (format-log-entry e)
    ;; The entry's presentation text: its component's formatter, or the
    ;; datum itself (a string as it is, anything else written).
    (let ([f (log-formatter (cadr e))]
          [d (caddr e)])
      (guard (ex [else (format "~s" d)])
        (if f ((cadr f) d) (if (string? d) d (format "~s" d))))))

  ;;; Appending -----------------------------------------------------------------

  ;; The head's presenter, told about every appended entry as
  ;; (present! entry show?).  In a persistent cell so it outlives a
  ;; reload of this module; a presentation failure never loses the
  ;; record.
  (define presenter-cell
    (kernel:persistent-cell 'log-presenter (lambda () #f)))

  (define (set-presenter! present!) (set-box! presenter-cell present!))

  (define (log! component datum . show)
    ;; Append a structured record and hand it to the presenter --
    ;; pass #f to log quietly.  -> the entry.
    (let ([e (list (current-time) component datum)])
      (append-record! e)
      (let ([present! (unbox presenter-cell)])
        (when present!
          (guard (ex [else (void)])
            (present! e (or (null? show) (car show))))))
      e))

  ;;; Queries -------------------------------------------------------------------

  (define (log-entries . component)
    ;; The records, newest first, each (time component datum) --
    ;; filtered when a component is given.
    (let loop ([i 0] [acc '()])
      (if (= i (log-length))
          (if (pair? component)
              (filter (lambda (e) (eq? (cadr e) (car component))) acc)
              acc)
          (loop (+ i 1) (cons (log-record i) acc)))))

  (define (log-history component . select)
    ;; Command history off the log: a component's datums through select
    ;; -- car for eval's (query . result), cdr for the file commands'
    ;; (verb . path) -- newest first, non-strings dropped, consecutive
    ;; repeats collapsed.
    (let ([sel (if (pair? select) (car select) (lambda (d) d))])
      (let loop ([es (log-entries component)] [last #f])
        (if (null? es)
            '()
            (let ([x (guard (ex [else #f]) (sel (caddr (car es))))])
              (if (and (string? x) (not (equal? x last)))
                  (cons x (loop (cdr es) x))
                  (loop (cdr es) last))))))))
