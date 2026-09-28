;; A head-local activity overlay, with an injectable clock for deterministic tests.
(import (only (foundation edoc) elibrary))
(elibrary (head spinner)
  (export make render!)
  (import (chezscheme) (prefix (head layout) layout:) (prefix (sys glyph) glyph:))

  (edoc "Create transient activity presentation driven by a monotonic clock and the host's frame scheduler."
        (now thunk "current monotonic time") (schedule procedure "request a frame at a monotonic deadline")
        (since (or any #f) "when the current busy period began")
        (constructor now schedule))
  (define-record-type (activity make activity?)
    (fields now schedule (mutable since))
    (protocol (lambda (new) (lambda (now schedule) (new now schedule #f)))))

  (edoc "Overlay a delayed single-cell spinner at the allocation's top left. Return lines and styles without changing the supplied frame; clipped corners schedule nothing."
        (activity any "transient presentation state") (busy boolean "whether work is pending")
        (rect list "widget allocation") (clip list "visible intersection")
        (lines list "composited display lines") (cells vector "composited cell styles")
        (returns any "two values: lines and cell styles"))
  (define (render! activity busy rect clip lines cells)
    (let ([now ((activity-now activity))])
      (cond [(not busy) (activity-since-set! activity #f)]
        [(not (activity-since activity)) (activity-since-set! activity (copy-time now))])
      (if (not (and busy (pair? lines) (layout:contains? clip (car rect) (cadr rect))))
        (values lines cells)
        (let* ([since (activity-since activity)] [age (time-difference now since)]
               [ms (+ (* 1000 (time-second age)) (div (time-nanosecond age) 1000000))]
               [phase (if (< ms 1000) 0 (+ 1 (div (- ms 1000) 125)))]
               [next (cond [(< ms 200) 200] [(< ms 1000) 1000] [else (+ 1000 (* phase 125))])])
          ((activity-schedule activity)
           (add-duration since (make-time 'time-duration (* (mod next 1000) 1000000) (div next 1000))))
          (if (< ms 200) (values lines cells)
            (let* ([styles (vector-copy cells)] [first (vector-copy (vector-ref styles 0))]
                   [old (vector-ref first 0)])
              (vector-set! first 0 (append (cond [(not old) '()] [(symbol? old) (list old)] [else old]) '(ghost)))
              (vector-set! styles 0 first)
              (values
                (cons (string-append (string (string-ref "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" (mod phase 10)))
                        (glyph:slice (car lines) 1 (- (caddr clip) 1))) (cdr lines)) styles))))))))
