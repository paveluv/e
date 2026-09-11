;; log.sls -- the base's structured log. Records are owned plain snapshots:
;; (utc-nanoseconds actor component datum), indexed in append order. Views
;; and echo presentation subscribe; neither owns a second history.
(library (log)
  (export add! snapshot retention entries history
          (rename (car time) (cadr actor) (caddr component) (cadddr datum))
          register-formatter! styler format-entry subscribe! unsubscribe! progress)
  (import (rnrs)
          (only (chezscheme) format parameterize print-graph)
          (prefix (kernel) kernel:) (prefix (journal) journal:) (prefix (datum) datum:))

  (define progress journal:progress)
  (define snapshot journal:snapshot)
  (define retention journal:retention)
  (define subscribe! journal:subscribe!)
  (define unsubscribe! journal:unsubscribe!)

  (define (own-datum value)
    ;; Keep structured data queryable. An arbitrary runtime/cyclic result is
    ;; its written representation at admission, never a retained live object.
    (guard (ex [else (parameterize ([print-graph #t]) (format "~s" value))])
      (datum:copy value)))

  (define (add! component datum . show)
    (apply journal:add! component (own-datum datum) show))

  ;;; Component presentation ------------------------------------------------

  (define formatters (kernel:make-registry))
  (define (register-formatter! component fmt . style)
    (kernel:registry-add! formatters (list component fmt (and (pair? style) (car style)))))
  (define (formatter component)
    (kernel:registry-find formatters (lambda (x) (eq? (car x) component))))
  (define (styler component)
    (let ([f (formatter component)]) (and f (caddr f))))
  (define (format-entry entry)
    (let ([f (formatter (caddr entry))] [d (cadddr entry)])
      (guard (ex [else (format "~s" d)])
        (if f ((cadr f) d) (if (string? d) d (format "~s" d))))))

  ;;; Queries ---------------------------------------------------------------

  (define entries
    (case-lambda
      [() (entries #f #f)]
      [(component) (entries component #f)]
      [(component count)
       (let-values ([(records end first) (snapshot 0 count component)]) records)]))

  (define (history component . select)
    ;; Select strings from owned data, newest first, collapsing consecutive
    ;; repeats among the newest 200 matching records. Eval selects car from
    ;; (query . result), file prompts cdr. Unrelated payloads are never copied.
    (let ([sel (if (pair? select) (car select) (lambda (d) d))])
      (let loop ([es (entries component 200)] [last #f])
        (if (null? es) '()
            (let ([x (guard (ex [else #f]) (sel (cadddr (car es))))])
              (if (and (string? x) (not (equal? x last)))
                  (cons x (loop (cdr es) x))
                  (loop (cdr es) last))))))))
