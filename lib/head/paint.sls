;; Default window/echo painter. Generic terminal output and frame state live
;; in tui; this adapter supplies only the default host's composition policy.
(import (only (foundation edoc) elibrary))
(elibrary (head paint)
  (export add-buffer-status-hint! add-highlighter!
    add-status-hint! buffer-line-hyperlinks buffer-wrap-setting
    clean-wrap? column-at-cell compute-breaks compute-echo-spans
    cursor-in-echo display-echo-log-row!
    echo-append! echo-box-border echo-box-width echo-cap
    echo-cursor-now echo-highlight echo-indent-now echo-index-at
    echo-log-prefix echo-log-rows echo-log-spans echo-position
    echo-queue! echo-width highlight-ranges hover-ranges
    line-breaks line-segments page-size place-cursor!
    present-echo! prompt-styler ranges-on-row redraw!
    region-span rows-before
    (rename (text-layout:scroll-margin scroll-margin))
    scroll-window! set-buffer-viewports! set-conflicts-action!
    show-message! show-prompt-message! update-echo-geometry!
    view-overflows? window-layout window-position
    window-screen-position window-wrapped?
    (rename (text-layout:wrap-lines wrap-lines)) wrap-width)
  (import (rnrs)
          (rnrs mutable-strings)
          (rnrs r5rs)
          (only (chezscheme) void format make-parameter)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head echo) echo:)
          (prefix (head editor) editor:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head render) render:)
          (prefix (head seat) seat:)
          (prefix (head style) style:)
          (prefix (head text-layout) text-layout:)
          (prefix (head tui) tui:)
          (prefix (head widget) widget:)
          (prefix (sys glyph) glyph:))

  (define-syntax rows (identifier-syntax (tui:screen-rows)))
  (define-syntax cols (identifier-syntax (tui:screen-cols)))
  (define-syntax the-screen-live? (identifier-syntax (tui:screen-live?)))

  ;;; Soft wrap -------------------------------------------------------------------

  (define compute-breaks render:breaks)

  ;;; Hyperlinks -------------------------------------------------------------------

  (define editor-name "e")

  (define (mode-info b lines)
    ;; The legacy window supplies only declared presentation facts. Modes
    ;; receive this window's text projection, never its mutable buffer record.
    (let ([m (mode:find (seat:buffer-fact b 'mode #f))])
      (vector (and m (mode:name m))
              (and m (mode:render m))
              (and m (mode:row-styles m))
              (mode:line-styles m)
              (mode:source lines
                (map (lambda (key) (cons key (seat:buffer-fact b key #f))) (mode:required-facts m))))))

  ;; a store change under this seat's buffers invalidates painted rows
  (define repaint-hooked
    (seat:set-repaint-hook! (lambda () (tui:invalidate-screen-cache!))))

  (edoc "The columns of a row inside the selected window's active region, as (start . end), or #f."
        (row integer "the row")
        (line-length integer "the line's length")
        (returns (or pair #f)))
  (define (region-span row line-length)
    ;; The columns of `row` inside the selected window's active
    ;; region, as (start . end), or #f.
    (let* ([w (seat:current-window)] [b (seat:window-buffer w)])
      (and (seat:buffer-marked b)
           (let* ([mr (seat:buffer-mark-row b)] [mc (seat:buffer-mark-col b)]
                  [pr (seat:window-prow w)] [pc (seat:window-pcol w)]
                  [before? (or (< pr mr) (and (= pr mr) (< pc mc)))]
                  [sr (if before? pr mr)] [sc (if before? pc mc)]
                  [er (if before? mr pr)] [ec (if before? mc pc)])
             (cond [(or (< row sr) (> row er)) #f]
                   [(= sr er) (cons sc ec)]
                   [(= row sr) (cons sc line-length)]
                   [(= row er) (cons 0 ec)]
                   [else (cons 0 line-length)])))))

  (edoc "A buffer's wrap fact: default, #t, #f, clean or (clean . columns), shared by every window and head showing it."
        (b (record buffer) "the buffer")
        (returns (or (one-of default #t #f clean) pair)))
  (define (buffer-wrap-setting b)
    ;; the buffer's wrap fact -- default, #t, #f, clean, or (clean . n)
    ;; -- a store property, shared by every window and head showing it
    (seat:buffer-fact b 'wrap 'default))

  (edoc "Whether a window soft-wraps its lines now: beside an edit buffer its own setting, else the buffer's wrap fact, else wrap-lines; an app's buffer follows its fact, else wrap-lines; never a surfaced row grid."
        (w window "the window")
        (returns boolean))
  (define (window-wrapped? w)
    ;; A surfaced row is an app's cell grid. Reflow belongs to its publisher;
    ;; withdrawal restores the head's ordinary buffer/window wrap setting.
    ;; The window's setting is for text a user edits; an app's buffer shows
    ;; itself as the app decides, through the buffer's fact.
    (let* ([b (seat:window-buffer w)] [fact (buffer-wrap-setting b)] [own (seat:window-wrap w)]
           [x (cond [(seat:app-buffer? b) fact]
                    [(not (eq? own 'default)) own]
                    [else fact])])
      (and (not (render:header (seat:window-rendition w))) (if (eq? x 'default) (text-layout:wrap-lines) x))))

  (edoc "Whether a window's buffer wraps cleanly: no continuation marks, the full width."
        (w window "the window")
        (returns boolean))
  (define (clean-wrap? w)
    (let ([x (buffer-wrap-setting (seat:window-buffer w))])
      (or (eq? x 'clean) (and (pair? x) (eq? (car x) 'clean)))))

  (edoc "The columns a wrapped row of a window may use: the content width less the continuation mark, or a clean wrap's full width or cap."
        (w window "the window")
        (returns integer))
  (define (wrap-width w)
    ;; a wrapped row keeps its last column for the \ continuation mark;
    ;; a clean wrap draws none and uses the full width -- or its own
    ;; cap: (clean . n) wraps at n columns inside a wider window
    (let ([x (buffer-wrap-setting (seat:window-buffer w))])
      (max 1 (cond
               [(and (pair? x) (eq? (car x) 'clean))
                (min (cdr x) (seat:window-content-width w))]
               [(eq? x 'clean) (seat:window-content-width w)]
               [else (- (seat:window-content-width w) 1)]))))

  ;; Context highlighting is provided by modules: a highlighter, registered
  ;; with add-highlighter!, is called at every redraw and returns ranges
  ;; of the current buffer to mark up -- a list of (row start end) or
  ;; (row start end style) entries, drawn in the current window on top
  ;; of the syntax styles. A scoped (buffer row start end style) or
  ;; (window row start end style) entry may decorate inactive buffers or one
  ;; particular window. Styles: mark (the default) underlines --
  ;; the paren module matches brackets this way -- while match and
  ;; match-point are the search's cyan and yellow backgrounds, and active is
  ;; the selected row in an app. A
  ;; broken highlighter is ignored for that redraw rather than taking
  ;; the editor down.
  ;; Modules may add a status hint: a thunk returning a short string
  ;; (or #f) appended to the current window's status line -- the
  ;; pretty-parens mode shows the source paren under point this way.
  (define status-hints (kernel:make-registry))
  (define buffer-status-hints (kernel:make-registry))

  (edoc "Register a status-line hint for the focused window: (proc) gives the text, or #f."
        (proc procedure "the hint"))
  (define (add-status-hint! proc)
    (kernel:registry-add! status-hints proc))

  (edoc "Register a status hint evaluated for every painted window as (proc buffer active?): a list of (text . face), or #f."
        (proc procedure "the hint"))
  (define (add-buffer-status-hint! proc)
    ;; Unlike a conventional status hint, this is evaluated for every painted
    ;; window as (proc buffer active?) and can therefore describe passive
    ;; windows too.
    (kernel:registry-add! buffer-status-hints proc))

  (define (spaced value)
    ;; a hint starts one space after what precedes it, the mode tag say,
    ;; whatever spacing its provider wrote
    (if (and (pair? value) (pair? (car value)) (string? (caar value)))
        (let* ([text (caar value)]
               [start (let skip ([i 0]) (if (and (< i (string-length text)) (char=? (string-ref text i) #\space)) (skip (+ i 1)) i))])
          (cons (cons (string-append " " (substring text start (string-length text))) (cdar value)) (cdr value)))
        value))

  (define (status-hint-values b active?)
    (let loop ([procs (append (if active? (kernel:registry-items status-hints) '())
                              (kernel:registry-items buffer-status-hints))]
               [ordinary (and active? (length (kernel:registry-items status-hints)))]
               [out '()])
      (if (null? procs)
          (reverse out)
          (let ([value
                 (guard (ex [else #f])
                   (let ([v (if (and ordinary (> ordinary 0))
                                ((car procs))
                                ((car procs) b active?))])
                     (cond
                       [(string? v) (list (cons v #f))]
                       [(and (pair? v) (string? (car v))) (list v)]
                       [(and (list? v)
                             (for-all (lambda (span)
                                        (and (pair? span)
                                             (string? (car span))))
                                      v))
                        v]
                       [else #f])))])
            (loop (cdr procs)
                  (and ordinary (> ordinary 1) (- ordinary 1))
                  (if value (append (reverse (spaced value)) out) out))))))

  (define (app-status-values w active?)
    (let ([id (seat:window-widget w)]) (if id (widget:status id active?) '())))

  (define (status-actions prefix spans visible-cells)
    ;; Hints already carry style spans. A procedure in the style slot makes
    ;; the text a control. Project complete, visible labels into terminal
    ;; cells once; hit testing never searches text for a special symbol.
    (let loop ([spans spans] [column (glyph:cells prefix)] [out '()])
      (if (null? spans) (reverse out)
          (let* ([span (car spans)] [end (+ column (glyph:cells (car span)))])
            (loop (cdr spans) end
              (if (and (status-action? (cdr span)) (<= end visible-cells))
                  (cons (list column end (cdr span)) out) out))))))

  (define (status-action? action) (or (procedure? action) (keymap:call-action? action)))

  (define conflicts-action #f)

  (edoc "Make the red !! of a conflicted buffer's status line a control running a thunk, the conflicts browser's opener say; #f takes the control away."
        (action (or procedure #f) "the thunk, or #f"))
  (define (set-conflicts-action! action)
    (set! conflicts-action action))

  (define highlighters (kernel:make-registry))

  (edoc "Register a highlighter: (proc) gives the ranges to mark this frame, each (row start end [face]), or scoped by a leading buffer or window."
        (proc procedure "the highlighter"))
  (define (add-highlighter! proc)
    (kernel:registry-add! highlighters proc))

  (define (mode-highlights)
    ;; The ordinary-window boundary adapts logical source spans to the old
    ;; painter protocol. Providers themselves never see a window or buffer.
    (let* ([b (seat:current-buffer-mirror)] [lines (seat:window-text (seat:current-window))])
      (if (or (editor-frame (seat:current-window)) (seat:app-buffer? b) (not (vector? lines))) '()
        (apply append
          (map (lambda (p)
                 (let* ([s (text:datum->span (car p))] [start (text:span-start s)] [end (text:span-end s)])
                   (let loop ([row (car start)] [out '()])
                     (if (> row (car end)) (reverse out)
                       (loop (+ row 1) (cons (list row (if (= row (car start)) (cdr start) 0)
                                               (if (= row (car end)) (cdr end) (+ 1 (string-length (vector-ref lines row)))) (cadr p)) out))))))
            (mode:highlights (vector-ref (mode-info b lines) 4) (mode:find (seat:buffer-fact b 'mode #f)) (seat:point)))))))

  (edoc "Every highlighter's ranges for this frame, the hovered hyperlink included; a raising highlighter contributes none."
        (returns list))
  (define (highlight-ranges)
    (fold-left (lambda (acc h) (append (guard (ex [else '()]) (h)) acc))
      (append (mode-highlights) (hover-ranges
                                  (lambda (w row column)
                                    (find (lambda (link) (<= (car link) column (- (cadr link) 1)))
                                      (line-hyperlinks (seat:window-buffer w) row (seat:window-line w row) (seat:window-rendition w))))))
      (kernel:registry-items highlighters)))

  (edoc "The highlight range under the mouse pointer: a hit query (hit window row column) gives (start end ...) or #f, painted with the face a chooser gives the hit, else hover."
        (hit procedure "the hit query")
        (face procedure "(face range) choosing the face")
        (returns list))
  (define hover-ranges
    (case-lambda
      [(hit) (hover-ranges-with hit #f)]
      [(hit face) (hover-ranges-with hit face)]))
  (define (hover-ranges-with hit face)
    ;; Reuse click geometry and the existing highlighter protocol. A hit
    ;; query takes (window row character-column) and returns (start end ...)
    ;; or #f. An optional face procedure maps that hit to a style.
    ;; Resolve against the current viewport on every frame, so a
    ;; refresh, scroll, resize or buffer switch cannot leave stale ink.
    (let ([position (head:mouse-position)])
      (or (and position
               (seat:window-at (- (car position) 1) (- (cdr position) 1)
                 (lambda (entry)
                   (let* ([w (car entry)] [start (cadr entry)] [height (caddr entry)]
                          [left (+ (seat:window-xoff w)
                                   (if (eq? (seat:window-scrollbar? w) 'left) 1 0)
                                   (seat:window-line-number-width w))])
                     (and (< (- (cdr position) 1) (+ start height))
                          (<= left (- (car position) 1))
                          (< (- (car position) 1) (+ left (seat:window-content-width w)))
                          (let* ([at (window-position w start height (car position) (cdr position))]
                                 [lines (seat:window-text w)])
                            (and (< (car at) (render:line-count lines))
                                 (let ([range (hit w (car at) (cdr at))])
                                   (and range (list (list w (car at) (car range) (cadr range)
                                                      (if face (face range) 'hover))))))))))))
          '())))

  (edoc "The hyperlinks of a buffer row as source character ranges, on or off screen."
        (buffer (record buffer) "the buffer")
        (row integer "the row")
        (returns list))
  (define (buffer-line-hyperlinks buffer row)
    ;; The public query returns source character ranges, including outside
    ;; the visible viewport. Painting uses its already prepared frame below.
    (line-hyperlinks buffer row (seat:buffer-line buffer row)
      (seat:read-rendition buffer (list (cons row (+ row 1))))))

  (define (line-hyperlinks buffer row text frame)
    (let ([data (render:row frame row)])
      (append
        (if data
            (map (lambda (range)
                   (cons (render:character frame row (car range))
                         (cons (render:character frame row (cadr range)) (cddr range))))
                 (caddr data)) '())
        (render:detect-links text))))

  (define (cell-ranges frame row ranges)
    (map (lambda (range)
           (cons (render:column frame row (car range))
                 (cons (render:column frame row (cadr range) #t) (cddr range)))) ranges))

  (edoc "The ranges among a frame's highlight ranges that fall on a row of a window: scoped ones for that buffer or window, unscoped ones when the window is current."
        (ranges list "the frame's ranges")
        (w window "the window")
        (b (record buffer) "its buffer")
        (row integer "the row")
        (current? boolean "whether the window is current")
        (returns list))
  (define (ranges-on-row ranges w b row current?)
    (fold-left (lambda (acc r)
                 (let* ([buffer-scoped? (and (pair? r) (seat:buffer? (car r)))]
                        [window-scoped? (and (pair? r) (seat:window? (car r)))]
                        [scoped? (or buffer-scoped? window-scoped?)]
                        [range (if scoped? (cdr r) r)])
                   (if (and (or (and buffer-scoped? (eq? (car r) b))
                                (and window-scoped? (eq? (car r) w))
                                (and (not scoped?) current?))
                            (= (car range) row))
                       (cons (cdr range) acc)
                       acc)))
               '() ranges))

  (edoc "The break table of a line in a window, a vector of its segment start columns, memoized per line string and width."
        (w window "the window")
        (line string "the line")
        (returns vector)
        (effects internal))
  (define (line-breaks w line)
    (text-layout:breaks line (wrap-width w)))

  (edoc "How many screen rows a line takes in a window: 1, or its soft-wrapped segment count."
        (w window "the window")
        (line string "the line")
        (returns integer))
  (define (line-segments w line)
    ;; How many screen rows the line takes in w: 1, or its soft-wrapped
    ;; segment count.
    (if (window-wrapped? w)
        (vector-length (line-breaks w line))
        1))

  (edoc "The character column of a row at a visual cell within a segment, clamped into it and snapped to a glyph's start."
        (w window "the window")
        (row integer "the row")
        (breaks (or vector #f) "the break table, or #f unwrapped")
        (segment integer "the segment")
        (cell integer "the visual cell")
        (returns integer))
  (define (column-at-cell w row breaks segment cell)
    (text-layout:column (seat:window-text w) (seat:window-rendition w) row breaks segment cell))

  (edoc "The displayed (row . col) at a 1-based screen (x, y) inside a window's text band, given the band's start row and height."
        (w window "the window")
        (start integer "the band's first screen row")
        (height integer "the band's height")
        (x integer "the screen column")
        (y integer "the screen row")
        (returns position))
  (define (window-position w start height x y)
    (let* ([v (seat:window-text w)] [frame (seat:window-rendition w)]
           [sticky (seat:buffer-sticky-lines (seat:window-buffer w))]
           [k (max 0 (- y 1 start))]
           [col (max 0 (- x 1 (seat:window-xoff w)
                          (if (eq? (seat:window-scrollbar? w) 'left) 1 0)
                          (seat:window-line-number-width w)))])
      (cond [(editor-frame w) => (lambda (f) (editor:frame-hit f col k))]
        [else (if (< k sticky)
                (let ([row (min k (- (render:line-count v) 1))]) (cons row (render:character frame row col)))
                (text-layout:hit v frame (and (window-wrapped? w) (wrap-width w))
                  (cons (max sticky (seat:window-top w)) (seat:window-topseg w))
                  (seat:window-left w) col (- k sticky)))])))

  (define (editor-frame w)
    (let* ([id (seat:window-editor w)] [f (and id (widget:prepared id))])
      (and f (pair? (widget:frame-data f)) f)))

  (define (paint-editor! w f start height ranges)
    (let* ([b (seat:window-buffer w)] [current? (eq? w (seat:current-window))]
           [gutter (seat:window-line-number-width w)]
           [x (+ (seat:window-xoff w) (if (eq? (seat:window-scrollbar? w) 'left) 1 0))]
           [width (seat:window-content-width w)] [n (seat:buffer-line-count b)]
           [top (seat:window-top w)] [wrapped? (window-wrapped? w)]
           [clean? (and wrapped? (clean-wrap? w))])
      (do ([y 0 (+ y 1)]) ((= y height))
        (paint-scrollbar! w (+ start y) y height 0 top n)
        (let ([row (editor:frame-row f y)])
          (paint-line-number! (+ start y) x gutter (and row (car row)) (and row (zero? (list-ref row 3))))
          (if (not row)
            (tui:paint! (+ start y) (+ x gutter) '(empty) (lambda () (tui:ansi! (tui:fit "" width))))
            (let* ([i (car row)] [line (cadr row)] [frame (caddr row)]
                   [left (list-ref row 3)] [bound (list-ref row 4)] [shown (list-ref row 5)]
                   [styles (widget:frame-cell-styles f y)]
                   [marks (cell-ranges frame i (ranges-on-row ranges w b i current?))]
                   [links (cell-ranges frame i (line-hyperlinks b i line frame))]
                   [edge (if wrapped?
                           (and (not clean?) (< bound (render:width frame i (string-length line))) 'wrap)
                           (and (> bound (+ left width)) 'trunc))])
              (tui:paint! (+ start y) (+ x gutter) (list 'editor shown left bound styles marks links edge width)
                (lambda () (tui:display-editor-line! shown shown #f marks links left styles edge width bound left)))))))))

  (define (paint-dividers! layout)
    ;; Paint vertical boundaries from the same recursive geometry used for
    ;; hit testing. Stacked boundaries are the upper leaves' status bars. At
    ;; an intersection, a spanning stacked split owns the cell and connects
    ;; its thin horizontal stroke to the divider above with a light `┴`;
    ;; otherwise the vertical split continues through as a thin stroke.
    (define (stacked-divider-crosses? x row)
      (exists (lambda (d)
                (and (eq? (car d) 'below)
                     (= row (cadddr d))
                     (<= (caddr d) x)
                     (< x (+ (caddr d) (list-ref d 4)))))
              (seat:dividers)))
    (for-each
      (lambda (divider)
        (when (eq? (car divider) 'right)
          (let ([x (caddr divider)] [start (cadddr divider)]
                [height (list-ref divider 4)])
            (do ([r start (+ r 1)]) ((>= r (+ start height)))
              ;; The final row always meets the full-width region that
              ;; ends the divider -- the echo area -- and connects to it.
              (let ([junction? (or (stacked-divider-crosses? x r)
                                   (= r (+ start height -1)))])
                (tui:paint! r x (list 'divider junction?)
                            (lambda ()
                              (if junction?
                                (tui:ansi! (style:code 'chrome)
                                  "\x2534;\x1b;[0m")
                                (tui:ansi! (style:code 'chrome)
                                  "\x2502;\x1b;[0m")))))))))
      (seat:dividers)))

  (define (paint-scrollbar! w row k height sticky top total)
    (let ([side (seat:window-scrollbar? w)])
      (when side
        (let* ([body-height (max 1 (- height sticky))]
               [body-total (max 0 (- total sticky))]
               [thumb-size (if (<= body-total body-height)
                             body-height
                             (max 1 (quotient (* body-height body-height)
                                              body-total)))]
               [travel (max 0 (- body-height thumb-size))]
               [scrollable (max 1 (- body-total body-height))]
               [thumb-start (if (= travel 0) 0
                              (quotient (* (max 0 (- top sticky)) travel)
                                        scrollable))]
               [j (- k sticky)]
               [thumb? (and (>= j thumb-start)
                            (< j (+ thumb-start thumb-size)))]
               [glyph (cond [(< k sticky) " "]
                        [thumb?
                         ;; Heavy box drawing stays centered and joins adjacent
                         ;; thumb rows without seams.
                         "\x2503;"]
                        [else "\x2502;"])])
          (tui:paint! row
                      (+ (seat:window-xoff w)
                        (if (eq? side 'right) (- (seat:window-width w) 1) 0))
                      (list 'scrollbar glyph)
                      (lambda ()
                        (tui:ansi! (style:code 'chrome) glyph "\x1b;[0m")))))))

  (define (paint-line-number! row x width line first-segment?)
    (when (> width 0)
      (let* ([label (if (and line first-segment?)
                        (number->string (+ line 1))
                        "")]
             [text (string-append
                     (make-string (max 0 (- width 1 (string-length label)))
                                  #\space)
                     label " ")])
        (tui:paint! row x (list 'line-number text)
                    (lambda ()
                      (tui:ansi! (style:code 'chrome) text "\x1b;[0m"))))))

  (define (paint-window! w start height ranges)
    (let* ([b (seat:window-buffer w)]
           [v (seat:window-text w)]
           [frame (seat:window-rendition w)]
           [wrap? (window-wrapped? w)]
           [n (render:line-count v)]
           [sticky (min height (seat:buffer-sticky-lines b))]
           [top (max sticky (seat:window-top w))]
           [left (seat:window-left w)]
           [gutter-width (seat:window-line-number-width w)]
           [gutter-x (+ (seat:window-xoff w)
                        (if (eq? (seat:window-scrollbar? w) 'left) 1 0))]
           [content-x (+ gutter-x gutter-width)]
           [content-width (seat:window-content-width w)]
           [info (mode-info b v)]
           [styles-of (vector-ref info 3)]
           [mode-tag (vector-ref info 0)]
           [current? (eq? w (seat:current-window))])
      ;; Walk buffer lines from the top -- its first visible segment --
      ;; a soft-wrapping window painting a long line as successive
      ;; slices (the same line at successive left offsets), others one
      ;; row per line.
      (if (editor-frame w) (paint-editor! w (editor-frame w) start height ranges)
        (let loop ([k 0] [i (if (> sticky 0) 0 top)]
                   [seg (if (> sticky 0) 0 (seat:window-topseg w))])
          (when (< k height)
            (let ([row (+ start k)])
              (paint-scrollbar! w row k height sticky top n)
              (paint-line-number! row gutter-x gutter-width
                                  (and (< i n) i) (= seg 0))
              (if (< i n)
                (let* ([line (render:line-ref v i)]
                       [data (render:row frame i)]
                       [width (render:width frame i (string-length line))]
                       [replacement (and (not data)
                                         (let ([r (vector-ref info 1)])
                                           (and r (guard (ex [else #f]) (r (vector-ref info 4) i line)))))]
                       [wrapped? (and (>= i sticky) wrap?)]
                       [breaks (and wrapped? (line-breaks w line))]
                       [slice-left (if wrapped?
                                       (render:column frame i (vector-ref breaks seg))
                                       left)]
                       [bound (if (and wrapped?
                                       (< (+ seg 1)
                                          (vector-length breaks)))
                                  (render:column frame i (vector-ref breaks (+ seg 1)))
                                  width)]
                       [edge (cond
                               [(and wrapped?
                                     (< (+ seg 1) (vector-length breaks)))
                                (if (clean-wrap? w) #f 'wrap)]
                               [(and (not wrapped?)
                                     (> width
                                        (+ left content-width)))
                                'trunc]    ; it continues past the edge: $
                               [else #f])]
                       [span (and current? (region-span i (string-length line)))]
                       [span (and span (cons (render:column frame i (car span))
                                             (render:column frame i (cdr span) #t)))]
                       [marks (cell-ranges frame i (ranges-on-row ranges w b i current?))]
                       [links (append (if data (caddr data) '())
                                      (cell-ranges frame i (render:detect-links line)))])
                  (let-values ([(shown row-styles)
                                (if data (values (car data) (cadr data))
                                    (render:present frame i line replacement
                                      (or (let ([f (vector-ref info 2)])
                                            (and f (guard (ex [else #f]) (f (vector-ref info 4) i line))))
                                          (styles-of line))))])
                    (tui:paint! row content-x
                                (list i line shown span marks links slice-left
                                  mode-tag row-styles edge)
                                (lambda ()
                                  (tui:display-editor-line! shown shown span marks links
                                                            slice-left
                                                            row-styles
                                                            edge
                                                            content-width
                                                            bound))))
                  (if (and (>= i sticky)
                           wrapped? (< (+ seg 1) (vector-length breaks)))
                      (loop (+ k 1) i (+ seg 1))
                      (loop (+ k 1)
                            (if (= (+ k 1) sticky) top (+ i 1)) 0)))
                (begin
                  (tui:paint! row content-x '(empty)
                              (lambda () (tui:ansi! (tui:fit "" content-width))))
                  (loop (+ k 1) (+ i 1) 0)))))))
      ;; the window's number, then a hairline flush against it (U+258F,
      ;; the left one-eighth block: single width, in every monospace
      ;; font's block range) so the gap falls after the line, not before
      (let* ([number (format "~a\x258F;" (seat:window-index w))]
             ;; the buffer's own status text, when it has a provider: it follows
             ;; the buffer's name, which every status line shows, an app's too
             [app-position (guard (ex [else #f]) (seat:buffer-status b w))]
             [head-prefix
              (if (string? app-position) number
                (format "~a~a~a  "
                        number
                        (cond [(seat:buffer-conflicted b) "!!"]
                          [(seat:app-buffer? b) "[]"]
                          [(seat:buffer-read-only b) "%%"]
                          [(seat:buffer-modified b) "**"]
                          [else "--"])
                        editor-name))]
             [name (seat:buffer-name b)]
             [status-row (if (pair? app-position)
                             (car app-position) (seat:window-prow w))]
             [status-col (if (pair? app-position)
                             (cdr app-position) (seat:window-pcol w))]
             [head (cond [(not (string? app-position))
                          (format "~a~a  L~a C~a" head-prefix name (+ status-row 1) (+ status-col 1))]
                         [(string=? app-position "") (string-append head-prefix name)]
                         [else (string-append head-prefix name "  " app-position)])]
             [mode-text (if (and mode-tag (not (string? app-position))) (format "  (~a)" mode-tag) "")]
             [hint-values
              (append (app-status-values w current?) (status-hint-values b current?))]
             [hint-text (apply string-append (map car hint-values))]
             [status (string-append head mode-text hint-text)]
             ;; the pop-up is never split or closed: its bar has one button
             ;; instead, ↓ where the others' × is, clearing it
             [buttons (if (seat:popup? w) seat:popup-buttons seat:window-buttons)]
             [status-width (max 0 (- (seat:window-width w) (seat:buttons-width buttons) 1))]
             [pointed (let ([at (head:mouse-position)])
                        (seat:window-status-actions-set! w
                          (append
                            ;; the !! of a conflicted buffer opens the conflicts browser
                            (if (and conflicts-action (not (string? app-position)) (seat:buffer-conflicted b))
                                (list (list (glyph:cells number) (+ (glyph:cells number) 2) conflicts-action))
                                '())
                            (status-actions (string-append head mode-text) hint-values
                              (- status-width (if (> (glyph:cells status) status-width) 1 0)))))
                        (and at (seat:window-button-at (- (car at) 1) (- (cdr at) 1))))]
             [hovered (and pointed (eq? (cdr pointed) w) (car pointed))])
        (let ([stale? (and (not (string? app-position)) (seat:buffer-conflicted b))])
          (tui:paint! (+ start height) (seat:window-xoff w)
                      (list 'status status current? stale? hovered)
                      (lambda ()
                        ;; Reversed cells take the bar's shade from the
                        ;; foreground color, so full reverse tracks the
                        ;; terminal's scheme (dark bar on light, light on
                        ;; dark) and an explicit mid grey marks inactive
                        ;; on either -- dim, the old marker, vanishes in
                        ;; reverse on light schemes.
                        (let* ([bar (cond [current? "\x1b;[7m"]
                                      [else "\x1b;[7;38;5;245m"])]
                               [fg (cond [current? "\x1b;[39m"]
                                     [else "\x1b;[38;5;245m"])]
                               [fitted (glyph:fit status status-width)]
                               ;; Geometry is in cells; the style spans below
                               ;; index characters in this already fitted text.
                               [content-end (string-length fitted)]
                               [text fitted]
                               [cs (min (string-length head) content-end)]
                               [ns (min (string-length head-prefix) content-end)]
                               [ne (min (+ ns (string-length name)) content-end)]
                               [hs (min (+ (string-length head)
                                          (string-length mode-text))
                                     content-end)]
                               [he (min (+ hs (string-length hint-text))
                                     content-end)]
                               [number-end (min (string-length number) content-end)]
                               [normal-start
                                (if stale? (min (+ number-end 2) content-end) number-end)])
                          (tui:ansi! bar)
                          ;; the window's number and its bar, then the state
                          ;; marker -- a conflicted buffer's !! in red
                          (tui:ansi! (substring text 0 number-end))
                          (when stale?
                            (tui:ansi! "\x1b;[31m" (substring text number-end normal-start)
                              fg))
                          (tui:ansi! (substring text normal-start ns)
                            "\x1b;[1m" (substring text ns ne)
                            "\x1b;[22m" (substring text ne cs))
                          (tui:ansi! (substring text cs hs))
                          (let loop ([values hint-values] [at hs])
                            (when (and (pair? values) (< at he))
                              (let* ([value (car values)]
                                     [end (min (+ at (string-length (car value)))
                                            he)])
                                (case (cdr value)
                                  [(italic) (tui:ansi! "\x1b;[3m")]
                                  [(red) (tui:ansi! "\x1b;[31m")])
                                (when (and (status-action? (cdr value)) (eq? (cdr value) hovered))
                                  (tui:ansi! (style:code 'hover)))
                                (tui:ansi! (substring text at end))
                                (when (and (status-action? (cdr value)) (eq? (cdr value) hovered))
                                  (tui:ansi! "\x1b;[0m" bar))
                                (case (cdr value)
                                  [(italic) (tui:ansi! "\x1b;[23m")]
                                  [(red) (tui:ansi! fg)])
                                (loop (cdr values) end))))
                          (tui:ansi! (substring text he content-end) " │")
                          (for-each
                            (lambda (button)
                              (when (eq? (car button) hovered) (tui:ansi! (style:code 'hover)))
                              (tui:ansi! (cdr button))
                              (when (eq? (car button) hovered) (tui:ansi! "\x1b;[0m" bar))
                              (tui:ansi! "│"))
                            buttons)
                          (tui:ansi! "\x1b;[0m"))))))))

  (edoc "Tile the split tree into the screen above the echo area: ((window start text-height) ...), start 0-based, remembered for mouse hit-testing."
        (returns list)
        (effects internal))
  (define (window-layout)
    ;; Tile the persistent split tree into the screen minus the echo
    ;; area; -> ((window start text-height) ...), start 0-based.  The
    ;; head remembers the tiling for mouse hit-testing.
    (seat:tile! cols (max 2 (- rows (echo:height)))))

  (edoc "The scrollable body height of the selected window, without its sticky app rows."
        (returns integer))
  (define (page-size)
    ;; The scrollable body height. Sticky app rows are fixed chrome and do not
    ;; form part of a page.
    (let ([height (caddr (assq (seat:current-window) (window-layout)))])
      (max 1 (- height
                (min height
                     (seat:buffer-sticky-lines (seat:window-buffer (seat:current-window))))))))

  ;; Soft wrap breaks at word boundaries: each line has a break table
  ;; -- the start position of every visual segment -- computed
  ;; greedily (the last space that fits; a word longer than the width
  ;; breaks mid-word) and memoized per line string and width, like the
  ;; style cache: edits replace line strings, so identity keys it.
  (edoc "Put point and the top row of a buffer, and of every window showing it but the excluded ones, at a position and top clamped into the text."
        (b (record buffer) "the buffer")
        (position position "where point goes")
        (top integer "the top row")
        (excluded-windows (list-of window) "windows left alone")
        (returns (record buffer)))
  (define (set-buffer-viewports! b position top excluded-windows)
    (let* ([count (seat:buffer-line-count b)]
           [row (max 0 (min (car position) (- count 1)))]
           [col (max 0 (min (cdr position)
                            (string-length (seat:buffer-line b row))))]
           [top (max 0 (min top (- count 1)))])
      (seat:buffer-spot-row-set! b row)
      (seat:buffer-spot-col-set! b col)
      (seat:buffer-spot-top-set! b top)
      (for-each
        (lambda (w)
          (when (and (eq? (seat:window-buffer w) b)
                     (not (memq w excluded-windows)))
            (seat:window-top-set! w top)
            (seat:window-topseg-set! w 0)
            (seat:window-left-set! w 0)
            (seat:window-prow-set! w row)
            (seat:window-pcol-set! w col)))
        (seat:windows))
      b))

  (edoc "How many screen rows lie between a window's top and a position, counting wrapped segments."
        (w window "the window")
        (prow integer "the row")
        (pcol integer "the column")
        (returns integer))
  (define (rows-before w prow pcol)
    (text-layout:distance (seat:window-text w) (and (window-wrapped? w) (wrap-width w))
      (cons (max (seat:buffer-sticky-lines (seat:window-buffer w)) (seat:window-top w)) (seat:window-topseg w))
      (cons prow pcol)))

  (define (decide-scrollbar! w height)
    ;; An auto scrollbar appears only while the whole content overflows the
    ;; window, judged at the full content width: a bar takes a column, which
    ;; can only make content longer, so what overflows without it overflows
    ;; with it and what fits without it needs none.  Sticky rows count.
    (let ([b (seat:window-buffer w)])
      (when (eq? (seat:buffer-fact b 'scrollbar #f) 'auto)
        (seat:window-auto-scrollbar-set! w
          (let* ([v (seat:window-text w)]
                 [width (max 1 (- (seat:window-width w) (seat:window-line-number-width w)))]
                 [wrapped? (window-wrapped? w)])
            (let loop ([i 0] [n 0])
              (cond [(> n height) #t]
                    [(>= i (render:line-count v)) #f]
                    [else (loop (+ i 1)
                                (+ n (if wrapped?
                                         (vector-length (compute-breaks (render:line-ref v i) width))
                                         1)))])))))))

  (edoc "Whether lines hold more content than a window of a height shows from its top segment."
        (w window "the window")
        (v vector "the lines")
        (height integer "the text height")
        (returns boolean))
  (define (view-overflows? w v height)
    (text-layout:overflows? v (and (window-wrapped? w) (wrap-width w))
      (cons (max (seat:buffer-sticky-lines (seat:window-buffer w)) (seat:window-top w)) (seat:window-topseg w)) height))

  (edoc "Clamp a window's point into its buffer and scroll so point stays visible, at least scroll-margin rows from the edges where the buffer allows."
        (w window "the window")
        (height integer "its text height"))
  (define (scroll-window! w height)
    (let* ([v (seat:window-text w)] [row (max 0 (min (seat:window-prow w) (- (render:line-count v) 1)))]
           [point (cons row (max 0 (min (seat:window-pcol w) (string-length (render:line-ref v row)))))]
           [sticky (seat:buffer-sticky-lines (seat:window-buffer w))])
      (seat:window-prow-set! w (car point)) (seat:window-pcol-set! w (cdr point))
      (unless (seat:app-manages-window-viewport? w)
        (let-values ([(point top left)
                      (text-layout:scroll v (seat:window-rendition w) (and (window-wrapped? w) (wrap-width w))
                        (seat:window-content-width w) (- height sticky) sticky
                        (cons (seat:window-top w) (seat:window-topseg w)) (seat:window-left w) point (text-layout:scroll-margin))])
          (seat:window-top-set! w (car top)) (seat:window-topseg-set! w (cdr top)) (seat:window-left-set! w left))))) ; the terminal is ours only between main's
                           ; alternate-screen enter and exit

  ;; The echo area is a box of at most echo-box-width columns, borders
  ;; included, centered on the screen; a narrower screen is the whole box.
  ;; Every row wraps inside the borders, with its text at the left one.
  (edoc "The echo box's width in columns, borders included; a narrower screen is the whole box."
        (value integer))
  (define echo-box-width (make-parameter 100
                           (lambda (n)
                             (unless (and (integer? n) (exact? n) (>= n 4))
                               (error 'echo-box-width "expected an exact integer of at least 4" n))
                             n)))

  (edoc "The glyph on both sides of the echo box: any single terminal cell."
        (value (or string char)))
  (define echo-box-border ;; The glyph on both sides of the box: any single terminal cell.
    (make-parameter "┊"
      (lambda (glyph)
        (let ([glyph (if (char? glyph) (string glyph) glyph)])
          (unless (and (string? glyph) (= (glyph:cells glyph) 1))
            (error 'echo-box-border "expected a one-cell string or character" glyph))
          glyph))))
  (define (echo-box-columns) (min cols (echo-box-width)))
  (define (echo-box-offset) (quotient (- cols (echo-box-columns)) 2))

  (edoc "The columns inside the echo box's borders."
        (returns integer))
  (define (echo-width)
    (max 1 (- (echo-box-columns) 2)))

  (edoc "The echo area's continuation indent, capped at half its width."
        (returns integer))
  (define (echo-indent-now)
    (echo:indent-now (echo-width)))

  (edoc "The content index ranges of the echo area's visual lines, for content of length len."
        (content string "the content")
        (len integer "its length")
        (returns list))
  (define (compute-echo-spans content len)
    (echo:compute-spans content len (echo-width)))

  (define (echo-line-lead line)
    ;; Inner column where visual line `line` of the live content starts.
    (if (= line 0) 0 (echo-indent-now)))

  (edoc "The visual (line . inner column) of an echo content index."
        (k integer "the content index")
        (returns pair))
  (define (echo-position k)
    ;; Visual (line . inner column) of content index k, per echo-spans.
    (let loop ([spans (echo:spans)] [line 0])
      (let ([span (car spans)])
        (if (or (null? (cdr spans)) (< k (cdr span))
                (and (= k (cdr span)) (< k (string-length (echo:text)))
                     (char=? (string-ref (echo:text) k) #\newline)))
            (cons line (+ (echo-line-lead line) (- k (car span))))
            (loop (cdr spans) (+ line 1))))))

  (edoc "The echo content index under an inner column of a visual line, clamped to the line: the inverse of echo-position."
        (line integer "the visual line")
        (column integer "the inner column")
        (returns integer))
  (define (echo-index-at line column)
    ;; The content index under inner column `column` of visual line
    ;; `line`, clamped to that line's span: the inverse of echo-position.
    (let ([span (list-ref (echo:spans) line)])
      (max (car span) (min (cdr span) (+ (car span) (- column (echo-line-lead line)))))))

  ;; Parameterized on (by eval, around an evaluation), the cursor parks
  ;; at the end of the echo area's content -- and is drawn as a blinking
  ;; underline, so a running evaluation is visible at a glance.
  (edoc "Whether the cursor parks at the end of the echo content as a blinking underline: on around an evaluation."
        (value boolean))
  (define cursor-in-echo (make-parameter #f))

  ;; Prompts may parameterize this to style the echo content -- M-x
  ;; gives the expression Scheme highlighting.  A procedure from the
  ;; content string to a styles vector (as modes produce), or #f to
  ;; style nothing; a raising styler paints plain.
  (edoc "A styler of the echo content for a prompt, content string to styles vector, or #f to style nothing."
        (value (or procedure #f)))
  (define echo-highlight (make-parameter #f))

  (edoc "Lift a styler of the editable input into one of the whole echo content: the label and notes grey, the input delegated."
        (label string "the prompt label")
        (input-styler procedure "input string to styles vector")
        (returns procedure))
  (define (prompt-styler label input-styler)
    ;; Lift a styler for the editable input into one for the complete echo
    ;; content. The prompt label and any note stay grey; only the input is
    ;; delegated. Shared by file, symbol, and expression prompts.
    (let ([llen (string-length label)])
      (lambda (content)
        (and (string:prefix? label content)
             (let* ([styles (make-vector (string-length content) 'comment)]
                    [end (min (or (echo:input-end) (string-length content))
                              (string-length content))]
                    [input (substring content (min llen end) end)]
                    [inner (input-styler input)])
               (and inner
                    (begin
                      (let loop ([i llen])
                        (when (< i end)
                          (vector-set! styles i (vector-ref inner (- i llen)))
                          (loop (+ i 1))))
                      styles)))))))

  (edoc "The end of a running evaluation's echo text, or #f. Prompt widgets own their carets."
        (returns (or integer #f)))
  (define (echo-cursor-now)
    (and (cursor-in-echo)
      (+ (string-length (echo:text)) (string-length (echo:ghost)))))

                              ; applied while the text still matches

  (edoc "Put a message in the echo area and paint it right away, once the screen is the editor's."
        (s string "the message")
        (styles-pair (or pair #f) "(content . styler), applied while the text still matches"))
  (define (show-message! s styles-pair)
    ;; Put s in the echo area and paint right away (once the screen is
    ;; the editor's).
    (echo:set-indent! #f)
    (echo:set-input-end! #f)
    (echo:set-text! s)
    (echo:set-ghost! "")
    (echo:set-styles! styles-pair)
    (present-echo!))

  (edoc "Keep a completed prompt's layout and styling in the echo area while its command runs."
        (label string "the prompt label")
        (input string "the accepted input")
        (styler (or procedure #f) "the content styler"))
  (define (show-prompt-message! label input styler)
    ;; Preserve a completed prompt's exact layout and styling while its
    ;; command runs.  In particular, hard-newline continuations retain the
    ;; prompt indentation instead of becoming an unrelated plain message.
    (let ([content (string-append label input)])
      (echo:set-indent! (string-length label))
      (echo:set-input-end! (string-length content))
      (echo:set-text! content)
      (echo:set-ghost! "")
      (echo:set-styles! (and styler (cons content styler)))
      (present-echo!)))

  (edoc "Append a line to the echo area's transient log and present it, component-prefixed and stacked until the next key; replace? supersedes the component's newest line when it is the newest overall."
        (component symbol "the log component")
        (text string "the line")
        (styler (or procedure #f) "the component's styler")
        (replace? boolean "whether to redraw in place"))
  (define (echo-append! component text styler replace?)
    ;; Append one line to the echo area's transient log: every logged
    ;; message stacks up there, component-prefixed, until the next key
    ;; settles the area.  With replace? true the component's newest
    ;; line is superseded when it is also the newest overall --
    ;; progress redrawn in place -- never another component's.  A
    ;; stale indicator gives way; a prompt's input line stays put
    ;; below, and so does a running evaluation's kept query -- the
    ;; user sees what is running.
    (echo-queue! component text styler replace?)
    (present-echo!))

  (edoc "Queue a transient-log line without painting it, for batch publishers that present once at the end, with a ghost text after the line when one is given."
        (component symbol "the log component")
        (text string "the line")
        (styler (or procedure #f) "the component's styler")
        (replace? boolean "whether to redraw in place")
        (ghost string "a ghost text after the line"))
  (define echo-queue!
    ;; Update transient echo state without painting it; batch publishers use
    ;; this before one final present-echo!.
    (case-lambda
      [(component text styler replace?) (echo-queue! component text styler replace? "")]
      [(component text styler replace? ghost)
       (echo:queue! component text styler replace? ghost (and (echo-cursor-now) #t))]))

  (edoc "Present the echo area now, mid-command included: a full redraw when its height changed, else just the area.")
  (define (present-echo!)
    ;; Present the echo area now, mid-command included (once the
    ;; screen is the editor's).  Grown or shrunk it takes a full
    ;; redraw -- the windows above shift, their status bars with them
    ;; -- otherwise painting the area suffices.
    (when the-screen-live?
      (let ([h (echo:height)])
        (update-echo-geometry!)
        (if (and (= h (echo:height)) (not (tui:preparing?)))
            (draw-partial-frame! paint-echo-area!)
            (redraw!)))))

  (edoc "The grey component prefix of a transient-log entry, fitted to the echo width."
        (e datum "the log entry")
        (returns string))
  (define (echo-log-prefix e)
    (echo:log-prefix e (echo-width)))

  (edoc "The content index ranges of a transient-log entry's visual rows."
        (prefix-len integer "the prefix length")
        (content string "the entry text")
        (returns list))
  (define (echo-log-spans prefix-len content)
    (echo:log-spans prefix-len content (echo-width)))

  (edoc "How many visual rows a transient-log entry takes."
        (e datum "the log entry")
        (returns integer))
  (define (echo-log-rows e)
    (echo:log-rows e (echo-width)))

  (define (echo-frame! draw used wrapped?)
    ;; Paint one echo row: the margin, the left border, the inner cells
    ;; that draw emits (used of them), the fill up to a wrap mark or the
    ;; right border, and the margin after it. The borders are echo-box-border
    ;; in the mid grey of an inactive status bar (see the bar's painter for
    ;; why that shade is explicit); light dashes by default, unlike the
    ;; light dividers between windows.
    (let* ([offset (echo-box-offset)] [width (echo-width)]
           [border (string-append "\x1b;[38;5;245m" (echo-box-border) "\x1b;[0m")])
      (tui:ansi! "\x1b;[0m" (make-string offset #\space) border)
      (draw)
      (tui:ansi! "\x1b;[0m"
        (make-string (max 0 (- width used (if wrapped? 1 0))) #\space)
        (if wrapped? "\\" "")
        border
        (make-string (max 0 (- cols offset (echo-box-columns))) #\space))))

  (edoc "Paint one visual row of a transient-log entry: the prefix or its indent, the slice under the styler, a mark when wrapped."
        (prefix string "the component prefix")
        (text string "the entry text")
        (styler (or procedure #f) "the component's styler")
        (ghost string "the grey tail")
        (k integer "the row within the entry")
        (span pair "the content indices of the row")
        (wrapped? boolean "whether the row wraps on"))
  (define (display-echo-log-row! prefix text styler ghost k span wrapped?)
    ;; One visual row of a transient-log entry: the grey prefix on the
    ;; first, its indent on continuations, the slice under the
    ;; component's styler, a mark closing every wrapped row.
    (let* ([lead (if (= k 0)
                     prefix
                     (make-string (min (string-length prefix)
                                       (quotient (echo-width) 2))
                                  #\space))]
           [start (car span)]
           [end (cdr span)]
           [styles (and styler (guard (ex [else #f]) (styler text)))]
           [text-end (min end (string-length text))]
           [ghost-start (max start (string-length text))]
           [content (string-append text ghost)])
      (echo-frame!
        (lambda ()
          (tui:ansi! (style:code 'chrome) lead)
          (when (< start text-end)
            (if styles
                (tui:emit-runs! text styles start text-end)
                (tui:ansi! "\x1b;[0m" (substring text start text-end))))
          (when (< ghost-start end)
            (tui:ansi! "\x1b;[0m" (style:code 'ghost)
              (substring content ghost-start end))))
        (+ (string-length lead) (- end start))
        wrapped?)))

  (define (paint-echo-area!)
    ;; Paint the pending transient-log lines, then the visible
    ;; (wrapped) live line under them.  Recompute the geometry first:
    ;; set-message! and echo-append! come here directly, with the
    ;; content just changed (from redraw! it is a no-op).
    (update-echo-geometry!)
    (let loop ([es (echo:pending)] [row (- rows (echo:height))])
      (when (pair? es)
        (let* ([e (car es)]
               [prefix (echo-log-prefix e)]
               [text (cadr e)]
               [ghost (cadddr e)]
               [spans (echo-log-spans (string-length prefix)
                                      (string-append text ghost))]
               [limit (- rows (echo:live-height))])
          ;; the entry's rows in turn; clipped at the area's edge when
          ;; a single entry alone overflows the cap (the tail is in
          ;; *log*)
          (let rloop ([spans spans] [k 0] [row row])
            (if (or (null? spans) (>= row limit))
                (loop (cdr es) row)
                (let ([span (car spans)]
                      [wrapped? (pair? (cdr spans))])
                  (tui:paint! row 0 (list 'echo-log e k span wrapped? (echo-box-width) (echo-box-border) cols)
                    (lambda ()
                      (display-echo-log-row! prefix text (caddr e) ghost
                                             k span wrapped?)))
                  (rloop (cdr spans) (+ k 1) (+ row 1))))))))
    (when (> (echo:live-height) 0)
      (let* ([content (string-append (echo:text) (echo:ghost))]
             [ghost-at (string-length (echo:text))]
             [total (length (echo:spans))])
        (let loop ([line (echo:scroll)] [row (- rows (echo:live-height))])
          (when (< row rows)
            (let* ([span (list-ref (echo:spans) line)]
                   [start (car span)]
                   [end (min (cdr span) (string-length content))]
                   [end (max end start)]
                   [wrapped? (< line (- total 1))]
                   [lead (echo-line-lead line)]
                   [cut (min (max (- ghost-at start) 0) (- end start))]
                   ;; a prompt's label -- content up to (echo:indent) on
                   ;; the first visual line -- is painted grey, the
                   ;; transient log's shade: quiet chrome, the input
                   ;; carries the emphasis
                   [lb (if (= line 0)
                           (min (or (echo:indent) 0) (+ start cut))
                           0)])
              (tui:paint! row 0
                (list 'echo line (substring content start end)
                      cut lead lb wrapped? (echo-box-width) (echo-box-border) cols (and (echo-highlight) #t)
                      (and (echo:styles) #t))
                (lambda ()
                  (let ([styles
                         (or (and (echo-highlight)
                                  (guard (ex [else #f])
                                    ((echo-highlight) content)))
                             (and (echo:styles)
                                  (string:prefix? (car (echo:styles))
                                                  content)
                                  (guard (ex [else #f])
                                    ((cdr (echo:styles))
                                     (car (echo:styles))))))])
                    (echo-frame!
                      (lambda ()
                        (tui:ansi! (make-string lead #\space))
                        (when (> lb 0)
                          (tui:ansi! (style:code 'chrome)
                                     (substring content 0 lb) "\x1b;[0m"))
                        (if styles
                            ;; styled runs for the typed part
                            (tui:emit-runs! content styles (+ start lb)
                                            (+ start cut))
                            (tui:ansi! (substring content (+ start lb)
                                                  (+ start cut))))
                        (tui:ansi! "\x1b;[0m" (style:code 'ghost)
                                   (substring content (+ start cut) end)
                                   "\x1b;[0m"))
                      (+ lead (- end start))
                      wrapped?)))))
            (loop (+ line 1) (+ row 1)))))))

  (edoc "How tall the echo area may grow: the screen less every window's minimum."
        (returns integer))
  (define (echo-cap)
    ;; How tall the whole echo area may grow: everything but each
    ;; window's minimum -- seat:min-window-lines of text (at least 1)
    ;; plus its status line.
    (max 1 (- rows (seat:layout-min-height (seat:root)))))

  (edoc "Lay out the echo area: the pending transient lines above the live line, wrapped, capped and scrolled to keep the prompt cursor visible.")
  (define (update-echo-geometry!)
    ;; The echo area stacks the pending transient-log lines above the
    ;; live line.  The live line's height follows its wrapped content
    ;; (the grey suggestion included): prompt input wraps with
    ;; continuations indented to the prompt text, and a plain message
    ;; that overflows the width wraps the same way at indent zero --
    ;; up to eight lines, after which it scrolls, keeping the prompt
    ;; cursor's line visible; empty behind pending lines it folds
    ;; away.  The whole area grows until the windows above hit their
    ;; minimum; past that the oldest pending lines are evicted -- they
    ;; remain in *log*.
    (let* ([content (string-append (echo:text) (echo:ghost))]
           [len (string-length content)]
           [cursor (echo-cursor-now)]
           [padded (max len (if cursor (+ cursor 1) 1))])
      (echo:set-spans! (compute-echo-spans content padded))
      (let* ([total (length (echo:spans))]
             [live (if (or cursor (> len 0) (null? (echo:pending)))
                       (min total (max 1 (min 8 (- rows 3))))
                       0)]
             [room (max (if (= live 0) 1 0) (- (echo-cap) live))]
             [pending-rows (lambda ()
                             (fold-left + 0 (map echo-log-rows
                                                 (echo:pending))))])
        ;; a long entry wraps over several rows, so eviction counts
        ;; rows, whole oldest entries first; a lone entry past the cap
        ;; stays, clipped by the painter
        (let drop ()
          (when (and (pair? (echo:pending)) (pair? (cdr (echo:pending)))
                     (> (pending-rows) room))
            (echo:set-pending! (cdr (echo:pending)))
            (drop)))
        (echo:set-live-height! live)
        (echo:set-height! (+ live (min room (pending-rows))))
        (when cursor
          (let ([line (car (echo-position cursor))])
            (when (< line (echo:scroll)) (echo:set-scroll! line))
            (when (>= line (+ (echo:scroll) live))
              (echo:set-scroll! (- line (- live 1))))))
        (echo:set-scroll!
          (max 0 (min (echo:scroll) (- total (max live 1))))))))

  (define (paint-bell!)
    (when (tui:bell?)
      (let loop ([row (- rows (echo:height))])
        (when (< row rows)
          (tui:goto! (+ row 1) 1)
          (tui:ansi! "\x1b;[7m" (make-string cols #\space) "\x1b;[0m")
          (loop (+ row 1))))))

  (define (paint-terminal-title!)
    ;; OSC 2 is understood by GNOME Terminal, xterm, and nested e terminals.
    (let ([title (string-append "e: " (seat:buffer-name (seat:window-buffer (seat:current-window))))])
      (tui:title! title)))

  (edoc "The 1-based screen (row . col) of a displayed position in a window, wrap-aware."
        (w window "the window")
        (prow integer "the row")
        (pcol integer "the column")
        (returns pair))
  (define (window-screen-position w prow pcol)
    (let* ([entry (assq w (window-layout))] [sticky (seat:buffer-sticky-lines (seat:window-buffer w))]
           [frame (seat:window-rendition w)]
           [x (+ (seat:window-xoff w) (if (eq? (seat:window-scrollbar? w) 'left) 1 0) (seat:window-line-number-width w))]
           [p (cond [(editor-frame w) => (lambda (f) (editor:frame-position f (cons prow pcol)))]
                [else (if (< prow sticky) (cons (- (render:column frame prow pcol) (seat:window-left w)) (- prow sticky))
                        (text-layout:locate (seat:window-text w) frame (and (window-wrapped? w) (wrap-width w))
                          (cons (max sticky (seat:window-top w)) (seat:window-topseg w)) (seat:window-left w) (cons prow pcol)))])])
      (cons (+ 1 (cadr entry) sticky (cdr p)) (+ 1 x (car p)))))

  (edoc "Park the cursor in the echo area for a prompt or a running evaluation, else at point in the current window.")
  (define (place-cursor!)
    (draw-partial-frame! paint-cursor!))

  (define (paint-cursor!)
    ;; Park the cursor in the echo area (a prompt, or a running
    ;; evaluation -- the latter drawn as a blinking underline), else
    ;; put it at point in the current window.  Also called on its own
    ;; when an interaction is about to wait for a key, so its cursor
    ;; rules take effect without a repaint.
    (let* ([cursor (echo-cursor-now)]
           [root (seat:window-widget (seat:current-window))]
           [placement (and root (find (lambda (p) (equal? root (widget:frame-id (car p)))) (tui:placements)))]
           [widget-caret (and placement (widget:caret (car placement)))]
           [visible? (or cursor widget-caret (and (not root) (seat:app-cursor-visible-in? (seat:current-window))))])
      (if cursor
          (let ([p (echo-position cursor)])
            (tui:goto! (+ (- rows (echo:live-height)) (- (car p) (echo:scroll)) 1)
              (min (+ (echo-box-offset) 1 (cdr p) 1) cols)))
          (if widget-caret
              (tui:goto! (+ 1 (caddr placement) (cdr widget-caret)) (+ 1 (cadr placement) (car widget-caret)))
              (when visible?
                (let ([p (window-screen-position (seat:current-window)
                                                 (seat:window-prow (seat:current-window)) (seat:window-pcol (seat:current-window)))])
                  (tui:goto! (min (car p) rows) (min (cdr p) cols))))))
      (let* ([app-style (seat:app-cursor-style (seat:window-buffer (seat:current-window)))]
             [style (cond
                      [(cursor-in-echo) "\x1b;[3 q"]
                      [(and app-style (not (eq? app-style 'default)))
                       (case app-style
                         [(text) "\x1b;[0 q"]
                         [(block) "\x1b;[2 q"]
                         [(underline) "\x1b;[4 q"]
                         [(bar) "\x1b;[6 q"]
                         [(blinking-block) "\x1b;[1 q"]
                         [(blinking-underline) "\x1b;[3 q"]
                         [(blinking-bar) "\x1b;[5 q"])]
                      [widget-caret "\x1b;[1 q"]
                      ;; a bar where typing cannot land: a read-only buffer
                      [(seat:buffer-read-only (seat:window-buffer (seat:current-window)))
                       "\x1b;[5 q"]
                      [else "\x1b;[0 q"])])
        (tui:cursor-style! style))
      (tui:ansi! (if visible? "\x1b;[?25h" "\x1b;[?25l"))))

  (define (prepare-layout!)
    ;; Refresh here so direct prompt frames share the same bell lifetime.
    ;; The head derives its next wait from the live work in each frame.
    (update-echo-geometry!)
    ;; window geometry is otherwise set while painting, one frame
    ;; stale from here -- refresh views against the current layout
    (window-layout)
    (seat:refresh-visible-views!)
    ;; a terminal too small for the splits collapses back to one window
    (seat:fit-layout! cols (- rows (echo:height)))
    ;; A newly needed scrollbar changes the width an app just rendered for.
    ;; Refit before painting or offering sizes, rather than clipping the
    ;; informative end of a fitted row for one frame after a resize/filter.
    (let* ([layout (window-layout)]
           [widths (map (lambda (entry) (seat:window-content-width (car entry))) layout)])
      (for-each (lambda (entry) (decide-scrollbar! (car entry) (caddr entry))) layout)
      (unless (equal? widths (map (lambda (entry) (seat:window-content-width (car entry))) layout))
        (seat:refresh-visible-views!)))
    (window-layout))

  (define (paint-frame!)
    ;; Delivery may reenter and change the layout. Prepare and paint the
    ;; current windows after that callout, with no later resize delivery.
    (let ([layout (window-layout)])
      (seat:refresh-renditions!)
      (for-each (lambda (entry) (decide-scrollbar! (car entry) (caddr entry))) layout)
      (let ([view (list rows cols
                        (map (lambda (entry)
                               (list (cadr entry) (caddr entry)
                                     (seat:window-xoff (car entry))
                                     (seat:window-width (car entry))))
                             layout)
                        ;; A scrollbar changes one window row from a single
                        ;; full-width cached segment into two overlapping
                        ;; segments (the bar and the content).  Row cache
                        ;; entries are keyed by their starting column, so a
                        ;; later full-width paint cannot selectively evict a
                        ;; covered content segment.  Treat presentation
                        ;; topology as part of the view and discard those
                        ;; incompatible segment keys when buffers are switched.
                        (map (lambda (w)
                               (list (window-wrapped? w)
                                     (seat:window-scrollbar? w)
                                     (seat:window-line-number-width w)
                                     (seat:buffer-sticky-lines (seat:window-buffer w))))
                             (seat:windows)))])
        (for-each (lambda (entry)
                    (let* ([w (car entry)] [id (and (seat:buffer-store-id (seat:window-buffer w)) (seat:window-widget w))])
                      (if (and id (guard (ex [else #f]) (widget:host id)))
                        (begin (widget:set-active! id (eq? w (seat:current-window)))
                          (widget:prepare! id (if (window-wrapped? w) (wrap-width w) (seat:window-content-width w)) (caddr entry)))
                        (scroll-window! w (caddr entry)))))
                  layout)
        (tui:begin-frame! view rows)
        (tui:set-placements!
          (filter values (map (lambda (entry)
                                (let* ([w (car entry)] [id (seat:window-widget w)]
                                       [frame (and id (widget:prepared id))])
                                  (and frame (list frame
                                               (+ (seat:window-xoff w)
                                                  (if (eq? (seat:window-scrollbar? w) 'left) 1 0)
                                                  (seat:window-line-number-width w))
                                               (cadr entry))))) layout)))
        (paint-dividers! layout)
        (let ([ranges (highlight-ranges)])
          (for-each (lambda (entry)
                      (paint-window! (car entry) (cadr entry) (caddr entry) ranges))
                    layout))
        (paint-echo-area!)
        (paint-bell!)))
    (paint-terminal-title!)
    (paint-cursor!))

  (define (draw-partial-frame! draw)
    ;; A full pending frame owns viewport changes too. Publishing only a
    ;; cursor or echo diff would falsely commit its still-unshown geometry.
    (unless (head:finish-frame!) (tui:render! void draw #f)))

  (edoc "Paint a frame: measure the terminal, tile, adopt foreign edits, then repaint what changed. Explicit redraws publish; the outer loop may coalesce expired keyboard input."
        (coalesce? boolean "whether to allow bounded publication coalescing; default #f"))
  (define redraw!
    (case-lambda
      [() (redraw! #f)]
      [(coalesce?)
       ;; Every entry, including direct prompt redraws, prepares against current
       ;; geometry. Hooks can present messages and reenter, so finish them before
       ;; opening this frame's synchronized update.
       (tui:call-with-output
         (lambda ()
           (when (tui:terminal-size!)
             ;; The temporary window host needs one text/status/echo row and
             ;; a usable status width; these are not backend constraints.
             (tui:set-screen-rows! (max 3 rows))
             (tui:set-screen-cols! (max 20 cols)))
           (window-layout)
           (head:before-frame!)
           (tui:render! prepare-layout! paint-frame! coalesce?)))]))

)
