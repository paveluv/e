;; log-view.e -- buffer views over the editor's log, for the e editor.
;;
;; An e extension module: the library (log-view), loaded at startup by
;; the core, which calls init!.  The core owns the log itself -- the
;; records, log!, the echo-area presentation; this module renders the
;; records as buffers: *log* (every record, created at startup so it
;; is always in the buffer list) and filtered views like *log eval*,
;; one timestamped, component-prefixed row per line, appended
;; incrementally past a high-water mark.

(library (log-view)
  (export init! log-view show-log!)
  (import (chezscheme) (core)
          (prefix (styles) styles:)
          (prefix (modes) modes:)
          (prefix (strings) strings:)
          (prefix (head) head:)
          (prefix (log) log:)
          (only (describe) register-descriptions!))

  (define (log-line-prefix e)
    ;; The view's row prefix; the stored time keeps nanoseconds, the
    ;; rendering shows seconds.
    (let ([d (time-utc->date (car e))])
      (format "~2,'0d:~2,'0d:~2,'0d ~a: "
              (date-hour d) (date-minute d) (date-second d) (cadr e))))

  (define (style-log-line s)
    ;; The mode's styler: the timestamp and component prefix grey, the
    ;; text styled by the component's registered styler.
    (let* ([n (string-length s)]
           [styles (make-vector n 'comment)]
           [sep (and (> n 9) (strings:search s ": " 9 n))])
      (when sep
        (let* ([component (string->symbol (substring s 9 sep))]
               [styler (log:log-styler component)]
               [from (+ sep 2)]
               [inner (and styler
                           (guard (ex [else #f])
                             (styler (strings:tail s from))))])
          (if inner
              (let loop ([i from])
                (when (< i n)
                  (vector-set! styles i (vector-ref inner (- i from)))
                  (loop (+ i 1))))
              (styles:fill-range! styles from n 'plain))))
      styles))

  (define (make-log-view name pred)
    ;; A view over the log: the records pred accepts, each formatted by
    ;; its component's formatter, one prefixed row per line, appended
    ;; past a high-water mark.
    (define b #f)
    (define rendered 0)
    (define (refresh!)
      (when (< rendered (log:log-length))
        (let ([lines '()])
          (do ([i (- (log:log-length) 1) (- i 1)]) ((< i rendered))
            (let ([e (log:log-record i)])
              (when (pred e)
                (set! lines
                  (append (let ([prefix (log-line-prefix e)])
                            (map (lambda (l) (string-append prefix l))
                                 (strings:lines (log:format-log-entry e))))
                          lines)))))
          (set! rendered (log:log-length))
          (head:view-append! b lines))))
    (set! b (head:register-view! name refresh!))
    (modes:choose! b "log")
    (refresh!)
    b)

  (define (log-view . component)
    ;; The *log* view -- or a dynamic filtered one, *log eval* for
    ;; (log-view 'eval) -- created (or recreated after a kill) on
    ;; demand.
    (if (null? component)
        (or (head:buffer-named "*log*")
            (make-log-view "*log*" (lambda (e) #t)))
        (let ([name (format "*log ~a*" (car component))])
          (or (head:buffer-named name)
              (make-log-view name
                (lambda (e) (eq? (cadr e) (car component))))))))

  (define (show-log!)
    ;; Pop up the *log* view.
    (pop-up-or-reuse! (log-view))
    (void))

  (define (init!)
    (register-descriptions!
      '(((show-log!) (("procedure" . "(show-log!)")) "void"
         ("(log-view)") log-view "Log commands" #f
         "Display the live `*log*` view, containing timestamped editor messages and command results.")))
    (modes:register! "log" '() '() style-log-line)
    (log-view)))                ; the *log* view, listed from startup
