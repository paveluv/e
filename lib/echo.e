;; echo.e -- the notification area's model: the library (echo).
;; Pure infrastructure with no init!.
;;
;; The echo area's state and geometry math live here: the live
;; message with its ghost and styler, the transient log queue, the
;; prompt indent bookkeeping, and the span computations that decide
;; how content wraps into visual rows.  Painting is the painter's
;; (paint), the modal loop that grows the area the prompt's (prompt);
;; both, and the command layer, still reach this model through
;; identifier-syntax facades in places, so a `message` read or write
;; there lands here unchanged.
;;
;; Width is always passed in: this module knows how text folds, not
;; how wide the terminal is.

(library (echo)
  (export text set-text! ghost set-ghost! styles set-styles!
          pending set-pending! cursor set-cursor!
          indent set-indent! input-end set-input-end!
          height set-height! scroll set-scroll!
          spans set-spans! live-height set-live-height!
          indent-now queue! settle!
          compute-spans log-prefix log-spans log-rows)
  (import (rnrs) (rnrs r5rs)
          (only (chezscheme) format))

  ;;; The model -----------------------------------------------------------------

  ;; main links against this library, so it is never reloaded in place
  ;; and plain module state is as durable as a persistent cell.

  (define the-text "")       ; the live message
  (define the-ghost "")      ; grey suggestion drawn after it
  (define the-styles #f)     ; (text . styler) for the current message
  (define the-pending '())   ; transient-log lines (component text styler ghost)
  (define the-cursor #f)     ; content index to park the cursor at, or #f
  (define the-indent #f)     ; prompt continuation indent; #f = no prompt
  (define the-input-end #f)  ; content index past the prompt's input
  (define the-height 1)      ; echo area rows
  (define the-scroll 0)
  (define the-spans '((0 . 0)))
  (define the-live-height 1) ; rows of the live line inside the-height

  (define (text) the-text)
  (define (set-text! s) (set! the-text s))
  (define (ghost) the-ghost)
  (define (set-ghost! s) (set! the-ghost s))
  (define (styles) the-styles)
  (define (set-styles! s) (set! the-styles s))
  (define (pending) the-pending)
  (define (set-pending! entries) (set! the-pending entries))
  (define (cursor) the-cursor)
  (define (set-cursor! at) (set! the-cursor at))
  (define (indent) the-indent)
  (define (set-indent! i) (set! the-indent i))
  (define (input-end) the-input-end)
  (define (set-input-end! at) (set! the-input-end at))
  (define (height) the-height)
  (define (set-height! h) (set! the-height h))
  (define (scroll) the-scroll)
  (define (set-scroll! s) (set! the-scroll s))
  (define (spans) the-spans)
  (define (set-spans! s) (set! the-spans s))
  (define (live-height) the-live-height)
  (define (set-live-height! h) (set! the-live-height h))

  (define (indent-now width)
    ;; The continuation indent, capped at half the width so a prompt
    ;; whose label alone overflows the screen still wraps usefully.
    (min (or the-indent 0) (quotient width 2)))

  (define (queue! component text styler replace? ghost keep-live?)
    ;; Append one line to the transient log without painting it;
    ;; batch publishers call this before one final present.  With
    ;; replace? true the component's newest line is superseded when it
    ;; is also the newest overall -- progress redrawn in place --
    ;; never another component's.  Unless the caller keeps the live
    ;; line (a prompt's input, a captured app's passthrough), a
    ;; queued line supersedes the message and any prompt bookkeeping.
    (let* ([entry (list component text styler ghost)]
           [rev (reverse the-pending)]
           [rev (if (and replace? (pair? rev) (eq? (caar rev) component))
                    (cdr rev)
                    rev)])
      (set! the-pending (reverse (cons entry rev))))
    (unless keep-live?
      (set! the-text "")
      (set! the-ghost "")
      (set! the-styles #f)
      (set! the-indent #f)
      (set! the-input-end #f)))

  (define (settle!)
    ;; the next keystroke: transient lines and the message give way
    (set! the-text "")
    (set! the-pending '()))

  ;;; Geometry -------------------------------------------------------------------

  (define (compute-spans content len width)
    ;; Content index ranges of the echo area's visual lines: the first
    ;; line spans the full width, explicit newlines force a new visual line,
    ;; continuations start at the indent, and every soft-wrapped line gives
    ;; its last column to the wrap mark.
    (let ([indent (indent-now width)])
      (let loop ([start 0] [first? #t] [acc '()])
        (let* ([avail (if first? width (- width indent))]
               [limit (min len (+ start avail))]
               [hard (let find ([i start])
                       (cond [(>= i (min limit (string-length content))) #f]
                             [(char=? (string-ref content i) #\newline) i]
                             [else (find (+ i 1))]))])
          (cond [hard
                 (loop (+ hard 1) #f (cons (cons start hard) acc))]
                [(<= (- len start) avail)
                 (reverse (cons (cons start len) acc))]
                [else
                 (let ([take (- avail 1)])
                   (loop (+ start take) #f
                         (cons (cons start (+ start take)) acc)))])))))

  (define (log-prefix e width)
    (let ([p (format "~a: " (car e))])
      (if (> (string-length p) width) (substring p 0 width) p)))

  (define (log-spans prefix-len content width)
    ;; Content index ranges of a transient-log entry's visual rows: a
    ;; long line wraps rather than being cut -- there is no way to
    ;; scroll past the echo area's edge.  The first row follows the
    ;; prefix, continuations indent to it (capped at half the width),
    ;; and every wrapped row gives its last column to the wrap mark.
    (let ([indent (min prefix-len (quotient width 2))])
      (let ([len (string-length content)])
        (let loop ([start 0] [first? #t] [acc '()])
          (let* ([avail (max 1 (- width (if first? prefix-len indent)))]
                 [limit (min len (+ start avail))]
                 [hard (let find ([i start])
                         (cond [(>= i limit) #f]
                               [(char=? (string-ref content i) #\newline)
                                i]
                               [else (find (+ i 1))]))])
            (cond [hard
                   (loop (+ hard 1) #f (cons (cons start hard) acc))]
                  [(<= (- len start) avail)
                   (reverse (cons (cons start len) acc))]
                  [else
                   (let ([take (max 1 (- avail 1))])
                     (loop (+ start take) #f
                           (cons (cons start (+ start take)) acc)))]))))))

  (define (log-rows e width)
    (length (log-spans (string-length (log-prefix e width))
                       (string-append (cadr e) (cadddr e))
                       width))))
