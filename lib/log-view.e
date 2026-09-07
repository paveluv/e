;; log-view.e -- buffer views over the editor's log, for the e editor.
;;
;; An e extension module: the library (log-view), loaded at startup by
;; the kernel, which calls init!.  The log library owns the records
;; (log:add!) and the command layer its echo-area presentation; this
;; module renders the records as buffers: <log> (every record, created
;; at startup so it is always in the buffer list) and filtered views
;; like <log eval>, one timestamped, actor/component-prefixed row per line,
;; appended incrementally past a high-water mark.

(library (log-view)
  (export init! (rename (log-view buffer)) (rename (show-log! show!)))
  (import (chezscheme) (except (edit) init!)
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
    (define (refresh!)
      (let-values ([(records end) (log:snapshot (or rendered 0))])
        (when (or (not rendered) (< rendered end))
          (let ([lines
                 (fold-left
                   (lambda (lines e)
                     (if (accept? e)
                         (append (let ([prefix (log-line-prefix e)])
                                   (map (lambda (l) (string-append prefix l))
                                        (string:lines (log:format-entry e)))) lines)
                         lines))
                   '() records)])
            ;; A new registration rebuilds from the records, replacing
            ;; a previous rendering once; later refreshes only append.
            (if rendered
                (head:view-append! b lines)
                (head:view-replace! b lines))
            (set! rendered end)))))
    (set! b (head:register-view! name refresh!))
    (head:buffer-fact-set! b 'log-filter components)
    (mode:choose! b "log")
    (refresh!)
    b)

  (define (log-view . component)
    ;; The *log* view -- or a dynamic filtered one, *log eval* for
    ;; (log-view:buffer 'eval) -- created (or recreated after a kill) on
    ;; demand.
    (let* ([name (if (null? component) "*log*"
                   (format "*log ~a*" (car component)))]
           [b (head:find-tool-buffer name)])
      (if (and b (head:app-buffer? b))
          b
          (make-log-view name component))))

  (define (show-log!)
    ;; Pop up the *log* view.
    (pop-up-or-reuse! (log-view))
    (void))

  (define (init!)
    (doc:register!
      '(((log-view:show!) (("procedure" . "(log-view:show!)")) "void"
         ("(log-view:buffer)") log-view "Log commands" #f
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
    (log-view)))                ; the *log* view, listed from startup
