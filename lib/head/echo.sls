;; echo.sls -- the notification area's model: the library (echo).
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

(import (only (foundation edoc) elibrary))
(elibrary (head echo)
  (export compute-spans cursor ghost height indent indent-now input-end live-height log-prefix
          log-rows log-spans pending queue! scroll set-cursor! set-ghost! set-height!
          set-indent! set-input-end! set-live-height! set-pending! set-scroll! set-spans!
          set-styles! set-text! settle! spans styles text text-owner)
  (import (rnrs)
          (rnrs r5rs)
          (only (chezscheme) format))

  ;;; The model -----------------------------------------------------------------

  ;; main links against this library, so it is never reloaded in place
  ;; and plain module state is as durable as a persistent cell.

  (define the-text "")       ; the live message
  (define the-text-owner #f) ; optional identity for a refreshable indicator
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

  (edoc "The echo area's live text."
        (returns string))
  (define (text)
    the-text)

  (edoc "Who owns the live text: an indicator's component, or #f."
        (returns any))
  (define (text-owner)
    the-text-owner)

  (edoc "Set the live text and who owns it, an indicator's component."
        (s string "the text")
        (owner any "the owner, or #f"))
  (define set-text!
    ;; Ordinary messages always replace an indicator's ownership, even if
    ;; their text is identical. Queueing and settling use the same boundary.
    (case-lambda
      [(s)
       (set-text! s #f)]
      [(s owner)
       (set! the-text s) (set! the-text-owner owner)]))

  (edoc "The grey suggestion after the live text."
        (returns string))
  (define (ghost)
    the-ghost)

  (edoc "Set the grey suggestion after the live text."
        (s string "the suggestion"))
  (define (set-ghost! s)
    (set! the-ghost s))

  (edoc "The live text's styles, (content . styler) applied while the text still matches, or #f."
        (returns (or pair #f)))
  (define (styles)
    the-styles)

  (edoc "Set the live text's styles."
        (s (or pair #f) "(content . styler), or #f"))
  (define (set-styles! s)
    (set! the-styles s))

  (edoc "The queued transient-log entries, oldest first."
        (returns list))
  (define (pending)
    the-pending)

  (edoc "Replace the queued transient-log entries."
        (entries list "the entries"))
  (define (set-pending! entries)
    (set! the-pending entries))

  (edoc "The prompt cursor's content index, or #f without a prompt."
        (returns (or integer #f)))
  (define (cursor)
    the-cursor)

  (edoc "Set the prompt cursor's content index, or #f without a prompt."
        (at (or integer #f) "the index"))
  (define (set-cursor! at)
    (set! the-cursor at))

  (edoc "The continuation indent of wrapped content, or #f for none."
        (returns (or integer #f)))
  (define (indent)
    the-indent)

  (edoc "Set the continuation indent of wrapped content."
        (i (or integer #f) "the indent, or #f"))
  (define (set-indent! i)
    (set! the-indent i))

  (edoc "Where the prompt's input ends in the content, or #f."
        (returns (or integer #f)))
  (define (input-end)
    the-input-end)

  (edoc "Set where the prompt's input ends in the content."
        (at (or integer #f) "the index, or #f"))
  (define (set-input-end! at)
    (set! the-input-end at))

  (edoc "The echo area's height in rows."
        (returns integer))
  (define (height)
    the-height)

  (edoc "Set the echo area's height in rows."
        (h integer "the rows"))
  (define (set-height! h)
    (set! the-height h))

  (edoc "How many visual lines of the live content are scrolled off above."
        (returns integer))
  (define (scroll)
    the-scroll)

  (edoc "Set how many visual lines of the live content are scrolled off above."
        (s integer "the lines"))
  (define (set-scroll! s)
    (set! the-scroll s))

  (edoc "The content index ranges of the live content's visual lines."
        (returns list))
  (define (spans)
    the-spans)

  (edoc "Set the content index ranges of the live content's visual lines."
        (s list "the spans"))
  (define (set-spans! s)
    (set! the-spans s))

  (edoc "The rows the live line takes."
        (returns integer))
  (define (live-height)
    the-live-height)

  (edoc "Set the rows the live line takes."
        (h integer "the rows"))
  (define (set-live-height! h)
    (set! the-live-height h))

  (edoc "The continuation indent for a width, capped at half of it."
        (width integer "the echo width")
        (returns integer))
  (define (indent-now width)
    ;; The continuation indent, capped at half the width so a prompt
    ;; whose label alone overflows the screen still wraps usefully.
    (min (or the-indent 0) (quotient width 2)))

  (edoc "Append a line to the transient log without painting: replace? supersedes the component's newest line when it is the newest overall; unless keep-live?, the message and prompt bookkeeping give way."
        (component symbol "the log component")
        (text string "the line")
        (styler (or procedure #f) "the component's styler")
        (replace? boolean "whether to redraw in place")
        (ghost string "the grey tail")
        (keep-live? boolean "whether the live line stays"))
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
      (set-text! "")
      (set! the-ghost "")
      (set! the-styles #f)
      (set! the-indent #f)
      (set! the-input-end #f)))

  (edoc "Clear the live text and the transient lines, as the next keystroke does.")
  (define (settle!)
    ;; the next keystroke: transient lines and the message give way
    (set-text! "")
    (set! the-pending '()))

  ;;; Geometry -------------------------------------------------------------------

  (edoc "The content index ranges of the visual lines of content of length len at a width: newlines force lines, continuations start at the indent, wrapped lines give their last column to the mark."
        (content string "the content")
        (len integer "its length")
        (width integer "the echo width")
        (returns list))
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

  (edoc "The component prefix of a transient-log entry, cut to the width."
        (e datum "the log entry")
        (width integer "the echo width")
        (returns string))
  (define (log-prefix e width)
    (let ([p (format "~a: " (car e))])
      (if (> (string-length p) width) (substring p 0 width) p)))

  (edoc "The content index ranges of a transient-log entry's rows: the first after the prefix, continuations indented to it."
        (prefix-len integer "the prefix length")
        (content string "the entry text")
        (width integer "the echo width")
        (returns list))
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

  (edoc "How many rows a transient-log entry takes at a width."
        (e datum "the log entry")
        (width integer "the echo width")
        (returns integer))
  (define (log-rows e width)
    (length (log-spans (string-length (log-prefix e width))
                       (string-append (cadr e) (cadddr e))
                       width))))
