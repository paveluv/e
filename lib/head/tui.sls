;; TUI backend: one transactional frame shadow, row diff and terminal output.
;; It accepts painters and widget placements without importing a window host.
(import (only (foundation edoc) elibrary))
(elibrary (head tui)
  (export ansi! begin-frame! bell? call-with-output
    cursor-style! display-editor-line! draw-root! emit-runs! enter!
    erase-screen! fit goto! input-delay invalidate-screen-cache!
    leave! paint!
    redraw-lock render! reset-cursor-style! screen-cols
    screen-live? screen-rows set-placements! set-screen-cols!
    set-screen-live! set-screen-rows! terminal-size! title!
    visual-bell!)
  (import (chezscheme)
          (prefix (head head) head:)
          (prefix (head render) render:)
          (prefix (head style) style:)
          (prefix (head widget) widget:)
          (prefix
            (only (sys sys) terminal-output-port terminal-character-width
              terminal-size watch-terminal-resize! terminal-raw!
              terminal-restore!)
            sys:)
          (prefix (sys tty) tty:))

  ;;; Output primitives ---------------------------------------------------------

  (edoc "Write values to the terminal output port, displayed and unflushed."
        (xs (list-of any) "what to write"))
  (define (ansi! . xs)
    (for-each (lambda (x) (display x (sys:terminal-output-port))) xs))

  (edoc "Move the terminal cursor to a 1-based row and column."
        (r integer "the row")
        (c integer "the column"))
  (define (goto! r c)
    (ansi! "\x1b;[" (number->string r) ";" (number->string c) "H"))

  (edoc "A string padded with spaces or cut to exactly width characters."
        (s string "the text")
        (width integer "the wanted length")
        (returns string))
  (define (fit s width)
    (let ([n (string-length s)])
      (if (> n width)
          (substring s 0 width)
          (string-append s (make-string (- width n) #\space)))))

  ;;; The row painter ------------------------------------------------------------

  (edoc "Paint one screen row of a line: its columns from left on, styled per column, the region span and marks as backgrounds, hyperlinks as OSC 8, an edge mark for wrapping or truncation, blank past bound."
        (s (or string vector) "the line, or a surface row of cells")
        (shown (or string vector) "the display transform of the line, usually the line itself")
        (span (or pair #f) "the selected columns, (start . end)")
        (marks list "(start end [face]) highlight ranges")
        (links list "(start end url [id]) hyperlink ranges")
        (left integer "the first column shown")
        (styles (or vector #f) "the per-column styles")
        (edge (or (one-of wrap trunc) #f) "the continuation mark for the last column")
        (width integer "the row width in cells")
        (bound integer "the first column past this row's content")
        (style-origin (list-of integer) "optional cell offset of the supplied style vector"))
  (define (display-editor-line! s shown span marks links left styles edge
                                width bound . style-origin)
    ;; edge: #f, or the continuation mark for the last column -- 'wrap
    ;; (the line goes on below) or 'trunc (past the right edge).
    ;; bound: the first column past this row's content (a word-wrapped
    ;; segment may end short of the width; the rest pads blank).
    (define n (min (if (vector? s) (vector-length s) (string-length s)) bound))
    (define limit (+ left width (if edge -1 0)))
    (define (style-at col)
      (let ([at (- col (if (null? style-origin) 0 (car style-origin)))])
        (if (and (vector? styles) (< col n) (<= 0 at) (< at (vector-length styles))) (vector-ref styles at) 'plain)))
    (define (mark-style m)
      (if (pair? (cddr m)) (caddr m) 'mark))
    (define (covers? m col)
      (and (<= (car m) col) (< col (cadr m))))
    ;; Direct scans avoid allocating a capturing predicate for every cell,
    ;; including the common case with no marks or links at all.
    (define (bg-at col)
      ;; The strongest background among the marks covering col:
      ;; match-point and app selections over match, or #f.
      (let loop ([rest marks] [face #f])
        (if (or (>= col n) (null? rest)) face
            (let ([m (car rest)])
              (loop (cdr rest)
                (if (covers? m col)
                    (case (mark-style m)
                      [(match-point) 'match-point]
                      [(active) 'active]
                      [(match) (or face 'match)]
                      [else face]) face))))))
    (define (selected? col)
      (and (< col n) span (<= (car span) col) (< col (cdr span))))
    (define (link-at col)
      (let loop ([rest links])
        (and (pair? rest)
             (if (covers? (car rest) col) (car rest) (loop (cdr rest))))))
    (define (safe-link-text text)
      (list->string
        (filter (lambda (character)
                  (let ([code (char->integer character)])
                    (and (>= code 32)
                         (not (<= 127 code 159)))))
                (string->list text))))
    (define (safe-link-id text)
      (list->string
        (filter (lambda (character)
                  (or (char-alphabetic? character)
                      (char-numeric? character)
                      (memv character '(#\- #\_ #\.))))
                (string->list text))))
    (define (open-link link)
      (let ([id (and (pair? (cdddr link)) (cadddr link))])
        (ansi! "\x1b;]8;"
               (if (and id (string? id))
                 (string-append "id=" (safe-link-id id)) "")
               ";" (safe-link-text (caddr link)) "\x1b;\\")))
    (define (close-link) (ansi! "\x1b;]8;;\x1b;\\"))
    (define (segment from to)
      ;; The characters of columns [from, to), off the shown text (the
      ;; mode's display transform, usually the line itself); control
      ;; characters (notably tabs) and columns past the end of the line
      ;; become spaces, so every column is exactly one cell wide.
      (if (vector? shown)
          (let loop ([i from] [parts '()])
            (if (= i to)
                (apply string-append (reverse parts))
                (if (or (>= i (min (vector-length shown) bound))
                        (string=? (vector-ref shown i) ""))
                    (loop (+ i 1) (cons " " parts))
                    (let* ([end (let scan ([end (+ i 1)])
                                  (if (and (< end (vector-length shown))
                                           (string=? (vector-ref shown end) ""))
                                      (scan (+ end 1)) end))]
                           [edge (min end to bound)])
                      ;; A clipped glyph occupies blanks, never half a wide
                      ;; glyph spilling into a gutter or neighboring pane.
                      (loop edge (cons (if (= edge end) (vector-ref shown i)
                                           (make-string (- edge i) #\space)) parts))))))
          (let ([out (make-string (- to from) #\space)])
            (let loop ([i from])
              (when (and (< i to) (< i (min (string-length shown) bound)))
                (let ([ch (string-ref shown i)])
                  (unless (or (< (char->integer ch) 32) (<= 127 (char->integer ch) 159))
                    (string-set! out (- i from) ch)))
                (loop (+ i 1))))
            out)))
    (define (overlay-at col)
      ;; The overlay style covering col: any mark style that is not one
      ;; of the background styles is emitted on top of the base style,
      ;; so highlighters can name their own faces (the bracket match
      ;; does).
      ;; Pointer feedback wins over a keyboard candidate or other text
      ;; overlay, independently of highlighter registration order.
      (and (< col n)
           (let loop ([rest marks] [face #f])
             (if (null? rest) face
                 (let* ([m (car rest)] [next (mark-style m)])
                   (loop (cdr rest)
                     (cond [(not (covers? m col)) face]
                           [(memq next '(hover candidate-hover)) next]
                           [(or face (memq next '(match match-point active))) face]
                           [else next])))))))
    ;; Emit runs of identically-attributed columns as single writes.
    (let loop ([col left])
      (when (< col limit)
        (let* ([style (style-at col)]
               [bg (bg-at col)]
               [sel (selected? col)]
               [mk (overlay-at col)]
               [link (link-at col)]
               [end (let run ([j (+ col 1)])
                      (if (and (< j limit)
                               (equal? (style-at j) style)
                               (eq? (bg-at j) bg)
                               (eq? (selected? j) sel)
                               (eq? (overlay-at j) mk)
                               (equal? (link-at j) link))
                          (run (+ j 1))
                          j))])
          (ansi! "\x1b;[0m" (style:code style))
          (when sel (ansi! (style:code 'selection)))
          (case bg
            [(match-point) (ansi! (style:code 'match-point))]
            [(active) (ansi! (style:code 'active))]
            [(match) (ansi! (style:code 'match))]
            [else (void)])
          (when mk (ansi! (style:code mk)))
          (when link (open-link link))
          (ansi! (segment col end))
          (when link (close-link))
          (loop end))))
    (when edge
      (ansi! "\x1b;[0m" (style:code 'chrome)
             (if (eq? edge 'wrap) "\\" "$")))
    (ansi! "\x1b;[0m"))

  (edoc "Write content[start, end) as styled runs, each under its style's code; positions past the styles vector paint plain."
        (content string "the text")
        (styles vector "the per-column styles")
        (start integer "the first column")
        (end integer "the column after the last"))
  (define (emit-runs! content styles start end)
    ;; content[start,end) in styled runs, each under its style's code;
    ;; positions past the styles vector paint plain.
    (let emit ([i start])
      (when (< i end)
        (let* ([at (lambda (k)
                     (if (< k (vector-length styles))
                         (vector-ref styles k)
                         'plain))]
               [st (at i)]
               [j (let run ([j (+ i 1)])
                    (if (and (< j end) (equal? (at j) st))
                        (run (+ j 1))
                        j))])
          (ansi! "\x1b;[0m" (style:code st) (substring content i j))
          (emit j)))))

  ;;; Frame composition ------------------------------------------------------------

  ;; The screen model: painted rows are cached by a key describing
  ;; what they show, so a frame repaints only what changed.  A frame
  ;; begins by naming its view -- the terminal size and the window
  ;; geometry -- and a view unlike the cached one discards every key.
  ;; A buffer's mode-driven presentation -- name, display transform,
  ;; row styler, memoized line styler -- comes from the mode registry
  ;; (mode), gathered once per window paint.

  ;; A frame draws against a private shadow. Only completed terminal output
  ;; becomes the next diff baseline; failed or superseded preparations do not.
  (define-record-type shadow
    (fields (mutable rows) (mutable view) (mutable cursor) (mutable title) (mutable widgets)))
  (define shown-shadow (make-shadow '#() #f "\x1b;[0 q" #f '()))
  (define preparing-shadow (make-parameter #f))
  (define frame-terminal (make-parameter #f))

  (edoc "Milliseconds between input receipt and frame publication, including command and rendering time. A small budget absorbs rendering jitter; 0 presents immediately."
        (value integer "0 to 50; default 8"))
  (define input-delay
    (make-parameter 8
      (lambda (value)
        (unless (and (integer? value) (exact? value) (<= 0 value 50))
          (error 'input-delay "expected an integer from 0 to 50 milliseconds" value))
        value)))
  (define (current-shadow) (or (preparing-shadow) shown-shadow))

  (edoc "Start a frame for a view description: a changed view, the terminal size or layout say, empties the row cache."
        (view any "what the frame shows, compared with the last")
        (rows integer "the screen height"))
  (define (begin-frame! view rows)
    (let ([shadow (current-shadow)])
      (unless (equal? view (shadow-view shadow))
        (shadow-rows-set! shadow (make-vector rows #f))
        (shadow-view-set! shadow view))))

  (edoc "Forget every cached row, so the next frame repaints all of it.")
  (define (invalidate-screen-cache!)
    ;; Invalidation requests a fresh next frame, including when a painter
    ;; requests it on every call. It must not cause an endless retry here.
    (shadow-view-set! shown-shadow #f)
    (when (preparing-shadow)
      (shadow-view-set! (preparing-shadow) #f)))

  (edoc "Blank the terminal, its selection highlight included, and schedule the full repaint.")
  (define (erase-screen!)
    ;; Blank the terminal and schedule the full repaint -- an actual
    ;; erase, which also clears the terminal's own selection highlight
    ;; where an identical overwrite would not.
    (ansi! "\x1b;[2J")
    (invalidate-screen-cache!))

  (edoc "Repaint the segment of a 0-based screen row starting at column xoff by calling draw, unless it already shows key."
        (row integer "the screen row")
        (xoff integer "the first column")
        (key any "what the segment shows")
        (draw thunk "the painter"))
  (define (paint! row xoff key draw)
    ;; Repaint the segment of the 0-based screen row starting at
    ;; column xoff unless it already shows key; a row shared by
    ;; side-by-side windows caches one key per segment.
    (let* ([screen-cache (shadow-rows (current-shadow))]
           [entry (vector-ref screen-cache row)]
           [hit (and (pair? entry) (assv xoff entry))])
      (unless (and hit (equal? (cdr hit) key))
        (ansi! "\x1b;[?25l") (goto! (+ row 1) (+ xoff 1))
        (draw)
        (vector-set! screen-cache row
          (cons (cons xoff key)
                (if hit (remq hit entry) (or entry '())))))))

  ;;; The frame driver ----------------------------------------------------------------

  ;; Widget hosts prepare geometry and logical state. This backend owns
  ;; terminal size, transactional output, row diffs, cursor and feedback.

  (define rows 24)
  (define cols 80)

  (edoc "The screen height in rows."
        (returns integer))
  (define (screen-rows)
    rows)

  (edoc "Set the screen height in rows."
        (n integer "the rows"))
  (define (set-screen-rows! n)
    (set! rows n))

  (edoc "The screen width in columns."
        (returns integer))
  (define (screen-cols)
    cols)

  (edoc "Set the screen width in columns."
        (n integer "the columns"))
  (define (set-screen-cols! n)
    (set! cols n))

  (edoc "Say whether the screen is the editor's to paint; leaving it forgets the row cache and the bell."
        (on? boolean "whether painting may proceed"))
  (define (set-screen-live! on?)
    (set! the-screen-live? on?)
    (unless on?
      (set! visual-bell-deadline #f)
      (invalidate-screen-cache!)))

  (edoc "Enter the TUI, enabling raw input, the alternate screen, paste, pointer and color reports. A failed entry restores terminal modes.")
  (define (enter!)
    (guard (ex [else (leave!) (raise ex)])
      (sys:terminal-raw!)
      (ansi! "\x1b;[?1049h\x1b;[2J\x1b;[?2004h\x1b;[?2031h")
      (tty:query-color-scheme!)
      (tty:mouse-reporting! #t)
      (set-screen-live! #t)))

  (edoc "Restore shell terminal modes and discard the visible frame, even if output fails. No widget root is required.")
  (define (leave!)
    (dynamic-wind void
      (lambda ()
        (set-screen-live! #f)
        (widget:invalidate!)
        (reset-cursor-style!)
        (tty:mouse-reporting! #f)
        (ansi! "\x1b;[?2026l\x1b;[?2031l\x1b;[?2004l\x1b;[?25h\x1b;[?1049l\x1b;[0m")
        (flush-output-port (sys:terminal-output-port)))
      sys:terminal-restore!))

  (edoc "Whether the screen is the editor's to paint."
        (returns boolean))
  (define (screen-live?)
    the-screen-live?)

  (edoc "Restore the terminal's default cursor shape, on the way out.")
  (define (reset-cursor-style!)
    ;; on the way out: the terminal's default cursor, unless it already shows
    (unless (equal? (shadow-cursor shown-shadow) "\x1b;[0 q")
      (ansi! "\x1b;[0 q")))

  ;;; Terminal size ---------------------------------------------------------

  ;; The system-specific work (termios, ioctl, SIGWINCH) lives in (sys);
  ;; here only the editor's idea of its size.  Without a terminal, sizes
  ;; fall back to LINES/COLUMNS.

  (define size-dirty? #t)

  ;; C-l also forces a size refresh in case resize events are unavailable.
  (define sigwinch-registered
    (sys:watch-terminal-resize!
      (lambda () (set! size-dirty? #t) (head:wake-main!))))

  (define (env-number name fallback)
    (let* ([s (getenv name)]
           [n (and s (string->number s))])
      (if (and n (exact? n) (integer? n) (> n 0)) n fallback)))

  (edoc "Measure the terminal when its size is dirty: at least one row and column."
        (returns boolean "whether a new measurement was taken"))
  (define (terminal-size!)
    (and size-dirty?
      (begin
        (set! size-dirty? #f)
        (set! rows (max 1 (env-number "LINES" 24)))
        (set! cols (max 1 (env-number "COLUMNS" 80)))
        (let ([size (sys:terminal-size)])
          (when size
            (set! rows (max 1 (car size)))
            (set! cols (max 1 (cdr size)))))
        #t)))

  ;; The cache holds, per screen row, the key describing what that row
  ;; currently shows; a row is repainted only when its key changes.  Any
  ;; change of view (size, search highlight, window arrangement) discards
  ;; the whole cache.
  (define the-screen-live? #f)

  (define (safe-terminal-title s)
    ;; OSC is terminated by BEL or ST. Do not let a buffer name inject either
    ;; terminator (or another terminal control) into the host terminal.
    (list->string
      (map (lambda (c)
             (let ([n (char->integer c)])
               (if (or (< n 32) (= n 127)) #\space c)))
           (string->list s))))

  ;;; The frame -----------------------------------------------------------------------

  ;; The head's pump owns painting. The redraw lock keeps its cache and
  ;; output together with other terminal writes (the clipboard's OSC 52).
  (edoc "The mutex around painting and the other terminal writes, such as the clipboard's OSC 52."
        (value any))
  (define redraw-lock (make-mutex))

  (edoc "Serialize terminal work and restore the real output port when a preparation callback reenters painting."
        (thunk thunk "terminal work") (returns any))
  (define (call-with-output thunk)
    (with-mutex redraw-lock
      (parameterize ([sys:terminal-output-port (or (frame-terminal) (sys:terminal-output-port))]
                     [preparing-shadow #f])
        (thunk))))

  (edoc "Stage the widget placements to publish after successful frame output."
        (placements list "(prepared-frame column row) entries"))
  (define (set-placements! placements) (shadow-widgets-set! (current-shadow) placements))

  (edoc "Paint a prepared fullscreen widget or an empty composition through the ordinary row diff. Call inside render!; only its successful output publishes these placements."
        (frame any "prepared frame or false"))
  (define (draw-root! frame)
    (begin-frame! (list 'root rows cols) rows)
    (set-placements! (if frame (list (list frame 0 0)) '()))
    (let ([lines (if frame (widget:frame-lines frame) '())])
      (do ([y 0 (+ y 1)] [rest lines (if (pair? rest) (cdr rest) '())]) ((= y rows))
        (let ([line (if (pair? rest) (car rest) "")]
              [styles (and frame (widget:frame-cell-styles frame y))]
              [links (if frame (widget:frame-row-links frame y) '())])
          (paint! y 0 (list line styles links cols)
            (lambda ()
              (let-values ([(cells unused) (render:present (render:prepare #f #f (vector line) 0 '((0 . 1))) 0 line #f #f)])
                (display-editor-line! cells cells #f '() links 0 styles #f cols
                  (if (vector? cells) (vector-length cells) (string-length cells)))))))))
    (let ([caret (and frame (widget:caret frame))])
      ;; Feedback belongs to the backend, so blank and custom compositions
      ;; share the same bell without requiring an echo-area widget.
      (when (and (bell?) (> rows 0))
        (goto! rows 1)
        (ansi! "\x1b;[7m" (make-string cols #\space) "\x1b;[0m"))
      (when caret (goto! (+ 1 (cdr caret)) (+ 1 (car caret))))
      (cursor-style! "\x1b;[1 q")
      (ansi! (if caret "\x1b;[?25h" "\x1b;[?25l"))))

  (edoc "Write a cursor shape escape only when it differs from the current frame's terminal state."
        (code string "DECSCUSR escape"))
  (define (cursor-style! code)
    (unless (equal? code (shadow-cursor (current-shadow)))
      (shadow-cursor-set! (current-shadow) code)
      (ansi! code)))

  (edoc "Set the terminal title, removing control characters and omitting unchanged output."
        (text string "the title"))
  (define (title! text)
    (unless (equal? text (shadow-title (current-shadow)))
      (shadow-title-set! (current-shadow) text)
      (ansi! "\x1b;]2;" (safe-terminal-title text) "\x1b;\\")))

  (define (present-frame! output shadow)
    ;; No application callbacks run while synchronization is open. The
    ;; terminal receives a finished packet, followed by its release, even
    ;; if a write fails. An uncertain write invalidates all terminal state.
    (let ([complete? #f])
      (dynamic-wind
        (lambda () (set! complete? #f))
        (lambda ()
          (ansi! "\x1b;[?2026h" output)
          (ansi! "\x1b;[?2026l")
          (flush-output-port (sys:terminal-output-port))
          (set! shown-shadow shadow)
          (widget:present! (shadow-widgets shadow))
          (head:frame-presented!)
          (set! complete? #t))
        (lambda ()
          (unless complete?
            (widget:invalidate!)
            (set! shown-shadow (make-shadow (make-vector rows #f) #f #f #f '()))
            (ansi! "\x1b;[?2026l")
            (flush-output-port (sys:terminal-output-port)))))))

  (edoc "Prepare and paint one TUI frame using the shared diff, pacing and synchronized-output transaction."
        (prepare thunk "geometry and source preparation") (draw thunk "the painter")
        (coalesce? boolean "whether expired queued keyboard input may defer publication"))
  (define (render! prepare draw coalesce?)
    (call-with-output
      ;; A callback may present a message or even prompt for input. Its
      ;; nested frame goes to the real terminal, never into the outer packet.
      (lambda ()
        (let retry ()
          (prepare)
          ;; before-frame! consumes the previous deadline. Register feedback
          ;; after that preparation, alongside this frame's new demands.
          (prepare-bell!)
          (let* ([basis shown-shadow]
                 [shadow (make-shadow (vector-map (lambda (row) row) (shadow-rows basis))
                           (shadow-view basis) (shadow-cursor basis) (shadow-title basis) (shadow-widgets basis))]
                 [output (call-with-string-output-port
                           (lambda (port)
                             (parameterize ([frame-terminal (sys:terminal-output-port)]
                                            [sys:terminal-output-port port]
                                            [preparing-shadow shadow])
                               (draw))))])
            (cond
              [(not (eq? basis shown-shadow)) (retry)]
              [(and coalesce? (head:defer-frame! (input-delay)))
               ;; Geometry still advances for the next command, but this
               ;; output and shadow never become a terminal baseline. Only
               ;; expired keyboard input may follow; the pump fences others.
               (void)]
              [else
               ;; Wait without pumping input; recheck the shadow in case a
               ;; signal reentered painting. No wait holds mode 2026 open.
               (head:wait-for-frame! (input-delay))
               (if (eq? basis shown-shadow)
                   (present-frame! output shadow)
                   (retry))]))))))

  (define visual-bell-deadline #f)

  (edoc "Whether the current frame should show the brief visual bell." (returns boolean))
  (define (bell?) (and visual-bell-deadline #t))
  (define (prepare-bell!)
    (when visual-bell-deadline
      (if (time<? (current-time 'time-monotonic) visual-bell-deadline)
          (head:request-frame-at! visual-bell-deadline)
          (begin
            (set! visual-bell-deadline #f)
            (invalidate-screen-cache!)))))

  (edoc "Flash the composition briefly through the main pump, instead of ringing.")
  (define (visual-bell!)
    ;; Main-thread presentation state: a retrigger replaces the deadline.
    ;; Request the first frame too; an invalid prompt key otherwise goes
    ;; straight back to waiting. Expiry uses that same pump, without a worker.
    (when the-screen-live?
      (set! visual-bell-deadline
        (add-duration (current-time 'time-monotonic) (make-time 'time-duration 50000000 0)))
      (head:wake-main!)))
)
