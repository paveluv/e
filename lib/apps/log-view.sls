;; log-view!.sls -- buffer views over the editor's log, for the e editor.
;;
;; An e extension module: the library (log-view!), loaded at startup by
;; the kernel, which calls init!.  The log library owns the records
;; (log:add!) and the command layer its echo-area presentation; this
;; module renders the records as buffers: <log> (every record, created
;; at startup so it is always in the buffer list) and filtered views
;; like <log eval>, one timestamped, actor/component-prefixed row per line,
;; appended incrementally past a high-water mark.

(import (only (edoc) elibrary))
(elibrary (log-view)
  (export init! (rename (log-view! buffer!)) (rename (show-log! show!)))
  (import (chezscheme)
          (except (edit) init!)
          (prefix (style) style:)
          (prefix (mode) mode:)
          (prefix (string) string:)
          (prefix (head) head:)
          (prefix (log) log:)
          (prefix (doc) doc:))

  (define (log-line-prefix e)
    ;; The view's row prefix; the stored time keeps nanoseconds, the
    ;; rendering shows seconds.
    (let* ([stamp (log:time e)]
           [d (time-utc->date (make-time 'time-utc (mod stamp 1000000000) (div stamp 1000000000)))])
      (format "~2,'0d:~2,'0d:~2,'0d ~s\t~a: "
              (date-hour d) (date-minute d) (date-second d) (log:actor e) (log:component e))))

  (define (style-log-line s)
    ;; The mode's styler: the timestamp, actor and component prefix grey, the
    ;; text styled by the component's registered styler.
    (let* ([n (string-length s)]
           [styles (make-vector n 'comment)]
           [start (and (> n 9) (string:search s "\t" 9 n))]
           [sep (and start (string:search s ": " (+ start 1) n))])
      (when sep
        (let* ([component (string->symbol (substring s (+ start 1) sep))]
               [styler (log:styler component)]
               [from (+ sep 2)]
               [inner (and styler
                           (guard (ex [else #f])
                             (styler (string:tail s from))))])
          (if inner
              (let loop ([i from])
                (when (< i n)
                  (vector-set! styles i (vector-ref inner (- i from)))
                  (loop (+ i 1))))
              (style:fill-range! styles from n 'plain))))
      styles))

  (define (make-log-view name components)
    ;; A view over the selected components, each record formatted by
    ;; its component's formatter, one prefixed row per line, appended
    ;; past a high-water mark.
    (define (accept? e)
      (or (null? components) (eq? (log:component e) (car components))))
    (define b #f)
    (define rendered #f)
    ;; Only a rendering index, oldest first: (absolute record . row count).
    ;; It lets multiline/filtered views drop exactly the rows that expired.
    (define sizes '())
    (define refreshing? #f)
    (define (refresh!)
      (unless refreshing?
        (dynamic-wind
          (lambda () (set! refreshing? #t))
          (lambda ()
            ;; Keep attachment and refresh reads bounded independently of
            ;; the journal's retention; old records remain queryable there.
            (let-values ([(records end retained) (log:snapshot (or rendered 0) 4096)])
              (let ([first (max retained (- end 4096))])
                (when (or (not rendered) (< rendered end)
                        (and (pair? sizes) (< (caar sizes) first)))
                  (let expire ([kept sizes] [drop 0])
                    (if (and (pair? kept) (< (caar kept) first))
                      (expire (cdr kept) (+ drop (cdar kept)))
                      (let format ([records records] [i (- end 1)] [lines '()] [added '()])
                        (cond
                          [(null? records)
                           (let ([append? rendered])
                             ;; Publish the bookmark/index before repaint;
                             ;; formatting may log, picked up next refresh.
                             (set! rendered end)
                             (set! sizes (append kept added))
                             (if append? (head:view-append! b lines drop)
                                 (head:view-replace! b lines)))]
                          [(accept? (car records))
                           (let* ([e (car records)] [prefix (log-line-prefix e)]
                                  [rows (map (lambda (l) (string-append prefix l))
                                             (string:lines (log:format-entry e)))])
                             (format (cdr records) (- i 1) (append rows lines)
                                     (cons (cons i (length rows)) added)))]
                          [else (format (cdr records) (- i 1) lines added)]))))))))
          (lambda () (set! refreshing? #f)))))
    (set! b (head:register-view! name refresh!))
    (head:buffer-fact-set! b 'log-filter components)
    (mode:choose! b "log")
    (refresh!)
    b)

  (edoc "The log view buffer, or a view filtered to one component, created on demand."
        (component (list-of symbol) "the component to show alone, at most one")
        (returns buffer))
  (define (log-view! . component)
    ;; The *log* view -- or a dynamic filtered one, *log eval* for
    ;; (log-view!:buffer 'eval) -- created (or recreated after a kill) on
    ;; demand.
    (let* ([name (if (null? component) "*log*"
                   (format "*log ~a*" (car component)))]
           [b (head:find-tool-buffer name)])
      (if (and b (head:app-buffer? b))
          b
          (make-log-view name component))))

  (edoc "Pop up the log view.")
  (define (show-log!)
    ;; Pop up the *log* view.
    (pop-up-or-reuse! (log-view!))
    (void))

  (edoc "Install the log view: its describe entry, its mode and the saved filters of views that survived a reload.")
  (define (init!)
    (doc:register!
      '(((log-view!:show!) (("procedure" . "(log-view!:show!)")) "void"
         ("(log-view!:buffer)") log-view! "Log commands" #f
         "Display the live `<log>` view, containing timestamped editor messages and command results.")))
    (mode:register! "log" '() '() style-log-line)
    ;; Registry retraction on reload leaves the local buffers alive.
    ;; Rebind every saved filter, including views already on screen.
    (for-each
      (lambda (b)
        (let ([components (head:buffer-fact b 'log-filter #f)])
          (when (and (not (head:buffer-store-id b)) (list? components))
            ;; Views opened at runtime may have registrations owned by
            ;; #f rather than this module.  Rebuild them too, replacing
            ;; old callbacks even when retraction left them registered.
            (make-log-view (if (null? components) "*log*"
                               (format "*log ~a*" (car components)))
                           components))))
      (head:buffers))
    (log-view!)))                ; the *log* view, listed from startup
