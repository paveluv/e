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

(library (echo)
  (export text text-owner set-text! ghost set-ghost! styles set-styles!
          pending set-pending! cursor set-cursor!
          indent set-indent! input-end set-input-end!
          height set-height! scroll set-scroll!
          spans set-spans! live-height set-live-height!
          indent-now queue! settle!
          compute-spans log-prefix log-spans log-rows)
  (import (rnrs) (only (edoc) edefine edoc) (rnrs r5rs)
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

  (edefine (text)
    (edoc "The echo area's live text."
          (returns string))
    the-text)
  (edefine (text-owner)
    (edoc "Who owns the live text: an indicator's component, or #f."
          (returns any))
    the-text-owner)
  (edefine set-text!
    ;; Ordinary messages always replace an indicator's ownership, even if
    ;; their text is identical. Queueing and settling use the same boundary.
    (case-lambda
      [(s)
       (edoc "Set the live text, owned by nobody." (s string "the text"))
       (set-text! s #f)]
      [(s owner)
       (edoc "Set the live text and who owns it, an indicator's component." (s string "the text") (owner any "the owner, or #f"))
       (set! the-text s) (set! the-text-owner owner)]))
  (edefine (ghost)
    (edoc "The grey suggestion after the live text."
          (returns string))
    the-ghost)
  (edefine (set-ghost! s)
    (edoc "Set the grey suggestion after the live text."
          (s string "the suggestion"))
    (set! the-ghost s))
  (edefine (styles)
    (edoc "The live text's styles, (content . styler) applied while the text still matches, or #f."
          (returns (or pair #f)))
    the-styles)
  (edefine (set-styles! s)
    (edoc "Set the live text's styles."
          (s (or pair #f) "(content . styler), or #f"))
    (set! the-styles s))
  (edefine (pending)
    (edoc "The queued transient-log entries, oldest first."
          (returns list))
    the-pending)
  (edefine (set-pending! entries)
    (edoc "Replace the queued transient-log entries."
          (entries list "the entries"))
    (set! the-pending entries))
  (edefine (cursor)
    (edoc "The prompt cursor's content index, or #f without a prompt."
          (returns (or integer #f)))
    the-cursor)
  (edefine (set-cursor! at)
    (edoc "Set the prompt cursor's content index, or #f without a prompt."
          (at (or integer #f) "the index"))
    (set! the-cursor at))
  (edefine (indent)
    (edoc "The continuation indent of wrapped content, or #f for none."
          (returns (or integer #f)))
    the-indent)
  (edefine (set-indent! i)
    (edoc "Set the continuation indent of wrapped content."
          (i (or integer #f) "the indent, or #f"))
    (set! the-indent i))
  (edefine (input-end)
    (edoc "Where the prompt's input ends in the content, or #f."
          (returns (or integer #f)))
    the-input-end)
  (edefine (set-input-end! at)
    (edoc "Set where the prompt's input ends in the content."
          (at (or integer #f) "the index, or #f"))
    (set! the-input-end at))
  (edefine (height)
    (edoc "The echo area's height in rows."
          (returns integer))
    the-height)
  (edefine (set-height! h)
    (edoc "Set the echo area's height in rows."
          (h integer "the rows"))
    (set! the-height h))
  (edefine (scroll)
    (edoc "How many visual lines of the live content are scrolled off above."
          (returns integer))
    the-scroll)
  (edefine (set-scroll! s)
    (edoc "Set how many visual lines of the live content are scrolled off above."
          (s integer "the lines"))
    (set! the-scroll s))
  (edefine (spans)
    (edoc "The content index ranges of the live content's visual lines."
          (returns list))
    the-spans)
  (edefine (set-spans! s)
    (edoc "Set the content index ranges of the live content's visual lines."
          (s list "the spans"))
    (set! the-spans s))
  (edefine (live-height)
    (edoc "The rows the live line takes."
          (returns integer))
    the-live-height)
  (edefine (set-live-height! h)
    (edoc "Set the rows the live line takes."
          (h integer "the rows"))
    (set! the-live-height h))

  (edefine (indent-now width)
    (edoc "The continuation indent for a width, capped at half of it."
          (width integer "the echo width")
          (returns integer))
    ;; The continuation indent, capped at half the width so a prompt
    ;; whose label alone overflows the screen still wraps usefully.
    (min (or the-indent 0) (quotient width 2)))

  (edefine (queue! component text styler replace? ghost keep-live?)
    (edoc "Append a line to the transient log without painting: replace? supersedes the component's newest line when it is the newest overall; unless keep-live?, the message and prompt bookkeeping give way."
          (component symbol "the log component")
          (text string "the line")
          (styler (or procedure #f) "the component's styler")
          (replace? boolean "whether to redraw in place")
          (ghost string "the grey tail")
          (keep-live? boolean "whether the live line stays"))
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

  (edefine (settle!)
    (edoc "Clear the live text and the transient lines, as the next keystroke does.")
    ;; the next keystroke: transient lines and the message give way
    (set-text! "")
    (set! the-pending '()))

  ;;; Geometry -------------------------------------------------------------------

  (edefine (compute-spans content len width)
    (edoc "The content index ranges of the visual lines of content of length len at a width: newlines force lines, continuations start at the indent, wrapped lines give their last column to the mark."
          (content string "the content")
          (len integer "its length")
          (width integer "the echo width")
          (returns list))
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

  (edefine (log-prefix e width)
    (edoc "The component prefix of a transient-log entry, cut to the width."
          (e datum "the log entry")
          (width integer "the echo width")
          (returns string))
    (let ([p (format "~a: " (car e))])
      (if (> (string-length p) width) (substring p 0 width) p)))

  (edefine (log-spans prefix-len content width)
    (edoc "The content index ranges of a transient-log entry's rows: the first after the prefix, continuations indented to it."
          (prefix-len integer "the prefix length")
          (content string "the entry text")
          (width integer "the echo width")
          (returns list))
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

  (edefine (log-rows e width)
    (edoc "How many rows a transient-log entry takes at a width."
          (e datum "the log entry")
          (width integer "the echo width")
          (returns integer))
    (length (log-spans (string-length (log-prefix e width))
                       (string-append (cadr e) (cadddr e))
                       width))))
