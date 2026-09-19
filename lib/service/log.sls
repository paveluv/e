;; log.sls -- the base's structured log. Records are owned plain snapshots:
;; (utc-nanoseconds actor component datum), indexed in append order. Views
;; and echo presentation subscribe; neither owns a second history.
(import (only (edoc) elibrary))
(elibrary (log)
  (export add! snapshot retention entries history
          time actor component datum
          register-formatter! styler format-entry subscribe! unsubscribe! progress)
  (import (rnrs)
          (only (chezscheme) format parameterize print-graph)
          (prefix (kernel) kernel:)
          (prefix (journal) journal:)
          (prefix (datum) datum:))

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

  (edoc "When a log record was made, in UTC nanoseconds."
        (e list "the record")
        (returns integer))
  (define (time e)
    (car e))

  (edoc "The actor whose work made a log record."
        (e list "the record")
        (returns any))
  (define (actor e)
    (cadr e))

  (edoc "The component a log record belongs to."
        (e list "the record")
        (returns symbol))
  (define (component e)
    (caddr e))

  (edoc "A log record's data."
        (e list "the record")
        (returns any))
  (define (datum e)
    (cadddr e))

  (edoc "Add a record to the log under a component, with an owned copy of its datum; show says how heads present it."
        (component symbol "the component")
        (datum any "the record's data")
        (show (list-of any) "the presentation, at most one: #f, #t or progress"))
  (define (add! component datum . show)
    (apply journal:add! component (own-datum datum) show))

  ;;; Component presentation ------------------------------------------------

  (define formatters (kernel:make-registry))
  (edoc "Register how a component's records are shown: a formatter from datum to text, and optionally a styler."
        (component symbol "the component")
        (fmt procedure "(fmt datum) giving the text")
        (style (list-of procedure) "a styler, at most one"))
  (define (register-formatter! component fmt . style)
    (kernel:registry-add! formatters (list component fmt (and (pair? style) (car style)))))
  (define (formatter component)
    (kernel:registry-find formatters (lambda (x) (eq? (car x) component))))
  (edoc "A component's registered styler, or #f."
        (component symbol "the component")
        (returns (or procedure #f)))
  (define (styler component)
    (let ([f (formatter component)]) (and f (caddr f))))
  (edoc "A record's text through its component's formatter, or written as data."
        (entry list "the record")
        (returns string))
  (define (format-entry entry)
    (let ([f (formatter (caddr entry))] [d (cadddr entry)])
      (guard (ex [else (format "~s" d)])
        (if f ((cadr f) d) (if (string? d) d (format "~s" d))))))

  ;;; Queries ---------------------------------------------------------------

  (edoc "Log records, oldest first: every record, a component's, or at most count of a component's newest; #f for every component."
        (component (or symbol #f) "the component")
        (count (or integer #f) "how many at most")
        (returns list))
  (define entries
    (case-lambda
      [()
       (entries #f #f)]
      [(component)
       (entries component #f)]
      [(component count)
       (let-values ([(records end first) (snapshot 0 count component)]) records)]))

  (edoc "Strings selected from a component's records, newest first, consecutive repeats collapsed among the newest 200; optionally selecting a field and limiting to one actor."
        (component symbol "the component")
        (select (list-of any) "a selector from datum to string, then an actor")
        (returns (list-of string)))
  (define (history component . select)
    ;; Select strings from owned data, newest first, collapsing consecutive
    ;; repeats among the newest 200 matching records. Eval selects car from
    ;; (query . result), file prompts cdr. An optional actor limits history
    ;; to that head's choices before capping/copying, without another store.
    (let ([sel (if (pair? select) (car select) (lambda (d) d))]
          [who (and (pair? select) (pair? (cdr select)) (cadr select))])
      (let-values ([(records end first) (snapshot 0 200 component who)])
        (let loop ([es records] [last #f])
          (if (null? es) '()
            (let ([x (guard (ex [else #f]) (sel (cadddr (car es))))])
              (if (and (string? x) (not (equal? x last)))
                  (cons x (loop (cdr es) x))
                  (loop (cdr es) last)))))))))
