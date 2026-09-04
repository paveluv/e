;; paint.e -- the row painter: the library (paint), v2 core
;; dissolution (dev/DESIGN2.md).  Pure infrastructure with no init!.
;;
;; Two layers.  The row painter is the data-in, ANSI-out half: given
;; a line, its style vector, the marks/links/selection covering it,
;; and the column window to show, emit the minimal styled runs to the
;; terminal-output-port -- everything passed in, so it tests
;; headlessly against a string port.  Above it, the window painter
;; composes a window from the head's records: soft-wrap geometry,
;; gutters and scrollbars, the status line, the highlighter and
;; hyperlinker and status-hint registries, and the screen cache that
;; repaints only rows whose key changed.  The mode registry stays with
;; the head; the painter learns a buffer's mode-driven presentation
;; through one hook.  The frame driver (scrolling, the echo area,
;; redraw!, the cursor) is still the core's.

(library (paint)
  (export ansi goto fit
          display-editor-line emit-runs
          detect-hyperlinks valid-hyperlink? compute-breaks
          wrap-lines buffer-wrap-setting window-wrapped? clean-wrap? wrap-width
          line-breaks segment-of segment-start segment-close line-segments
          add-status-hint! add-buffer-status-hint! add-highlighter!
          highlight-ranges add-hyperlinker! buffer-line-hyperlinks
          ranges-on-row region-span
          begin-frame! invalidate-screen-cache! erase-screen! paint!

          screen-rows set-screen-rows! screen-cols set-screen-cols!
          mark-size-dirty! screen-live? set-screen-live! reset-cursor-style!
          redraw! redraw-lock visual-bell!
          window-layout page-size set-buffer-viewports!
          reset-buffer-viewports! view-invalidate! point-visible?
          rows-before scroll-margin view-overflows? scroll-window!
          echo-indent-now compute-echo-spans echo-position
          cursor-in-echo echo-highlight prompt-styler
          completion-styler echo-cursor-now show-message!
          show-prompt-message! echo-append! echo-queue!
          present-echo! echo-log-prefix echo-log-spans
          echo-log-rows display-echo-log-row
          echo-cap update-echo-geometry!
          update-terminal-title! window-screen-position
          place-cursor! terminal-size!)
  (import (rnrs) (rnrs mutable-strings) (rnrs r5rs)
          (only (chezscheme)
                box void format make-parameter make-weak-eq-hashtable
                eq-hashtable-ref eq-hashtable-set! remq getenv
                make-mutex with-mutex unbox set-box! parameterize
                fork-thread sleep make-time)
          (only (sys) terminal-output-port terminal-character-width
                terminal-size watch-terminal-resize!)
          (prefix (style) style:)
          (prefix (string) string:)
          (prefix (head) head:)
          (prefix (mode) mode:)
          (prefix (echo) echo:)
          (prefix (kernel) kernel:))

  ;;; Output primitives ---------------------------------------------------------

  (define (ansi . xs)
    (for-each (lambda (x) (display x (terminal-output-port))) xs))

  (define (goto r c)
    (ansi "\x1b;[" (number->string r) ";" (number->string c) "H"))

  (define (fit s width)
    (let ([n (string-length s)])
      (if (> n width)
          (substring s 0 width)
          (string-append s (make-string (- width n) #\space)))))

  ;;; The row painter ------------------------------------------------------------

  (define (display-editor-line s shown span marks links left styles edge
                               width bound)
    ;; edge: #f, or the continuation mark for the last column -- 'wrap
    ;; (the line goes on below) or 'trunc (past the right edge).
    ;; bound: the first column past this row's content (a word-wrapped
    ;; segment may end short of the width; the rest pads blank).
    (define n (min (string-length s) bound))
    (define limit (+ left width (if edge -1 0)))
    (define (style-at col)
      (if (and styles (< col n)) (vector-ref styles col) 'plain))
    (define (mark-style m)
      (if (pair? (cddr m)) (caddr m) 'mark))
    (define (covers? m col)
      (and (<= (car m) col) (< col (cadr m))))
    (define (bg-at col)
      ;; The strongest background among the marks covering col:
      ;; match-point and app selections over match, or #f.
      (fold-left (lambda (acc m)
                   (if (and (< col n) (covers? m col))
                       (case (mark-style m)
                         [(match-point) 'match-point]
                         [(active) 'active]
                         [(active-shadow) (or acc 'active-shadow)]
                         [(match) (or acc 'match)]
                         [else acc])
                       acc))
                 #f marks))
    (define (selected? col)
      (and (< col n) span (<= (car span) col) (< col (cdr span))))
    (define (link-at col)
      (find (lambda (link) (and (<= (car link) col) (< col (cadr link))))
            links))
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
        (ansi "\x1b;]8;"
              (if (and id (string? id))
                  (string-append "id=" (safe-link-id id)) "")
              ";" (safe-link-text (caddr link)) "\x1b;\\")))
    (define (close-link) (ansi "\x1b;]8;;\x1b;\\"))
    (define (segment from to)
      ;; The characters of columns [from, to), off the shown text (the
      ;; mode's display transform, usually the line itself); control
      ;; characters (notably tabs) and columns past the end of the line
      ;; become spaces, so every column is exactly one cell wide.
      (if (vector? shown)
          (let loop ([i from] [parts '()])
            (if (= i to)
                (apply string-append (reverse parts))
                (loop (+ i 1)
                      (cons (if (and (< i (vector-length shown))
                                     (< i bound))
                                (vector-ref shown i) " ")
                            parts))))
          (let ([out (make-string (- to from) #\space)])
            (let loop ([i from])
              (when (and (< i to) (< i (min (string-length shown) bound)))
                (let ([ch (string-ref shown i)])
                  (unless (< (char->integer ch) 32)
                    (string-set! out (- i from) ch)))
                (loop (+ i 1))))
            out)))
    (define (overlay-at col)
      ;; The overlay style covering col: any mark style that is not one
      ;; of the background styles is emitted on top of the base style,
      ;; so highlighters can name their own faces (the bracket match
      ;; does).
      (and (< col n)
           (let ([m (find (lambda (m)
                            (and (covers? m col)
                                 (not (memq (mark-style m)
                                            '(match match-point active
                                                    active-shadow)))))
                          marks)])
             (and m (mark-style m)))))
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
                               (eq? (style-at j) style)
                               (eq? (bg-at j) bg)
                               (eq? (selected? j) sel)
                               (eq? (overlay-at j) mk)
                               (equal? (link-at j) link))
                          (run (+ j 1))
                          j))])
          (ansi "\x1b;[0m" (style:code style))
          (when sel (ansi (style:code 'selection)))
          (case bg
            [(match-point) (ansi (style:code 'match-point))]
            [(active) (ansi (style:code 'active))]
            [(active-shadow) (ansi (style:code 'active-shadow))]
            [(match) (ansi (style:code 'match))]
            [else (void)])
          (when mk (ansi (style:code mk)))
          (when link (open-link link))
          (ansi (segment col end))
          (when link (close-link))
          (loop end))))
    (when edge
      (ansi "\x1b;[0m" (style:code 'chrome)
            (if (eq? edge 'wrap) "\\" "$")))
    (ansi "\x1b;[0m"))

  (define (emit-runs content styles start end)
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
                    (if (and (< j end) (eq? (at j) st))
                        (run (+ j 1))
                        j))])
          (ansi "\x1b;[0m" (style:code st) (substring content i j))
          (emit j)))))

  ;;; Soft wrap -------------------------------------------------------------------

  (define (compute-breaks s width)
    (let ([n (string-length s)])
      (let loop ([start 0] [acc '(0)])
        (if (<= (- n start) width)
            (list->vector (reverse acc))
            (let* ([limit (+ start width)]
                   [p (let find ([j limit])
                        (cond [(<= j start) limit]
                              [(char=? (string-ref s (- j 1)) #\space) j]
                              [else (find (- j 1))]))])
              (loop p (cons p acc)))))))

  ;;; Hyperlinks -------------------------------------------------------------------

  (define (url-end-character? character)
    (or (char-whitespace? character)
        (memv character '(#\< #\> #\" #\' #\`))))

  (define (url-trailing-character? character)
    (memv character '(#\. #\, #\; #\: #\! #\? #\) #\] #\})))

  (define (detect-hyperlinks text)
    ;; Return explicit ranges rather than styling URLs directly, so callers
    ;; can inspect the destination and modes can add non-URL labels later.
    (let ([length (string-length text)])
      (let loop ([from 0] [links '()])
        (let ([http (string:search text "http://" from length)]
              [https (string:search text "https://" from length)])
          (let ([start (cond [(and http https) (min http https)]
                             [http http]
                             [else https])])
            (if (not start)
                (reverse links)
                (let* ([raw-end
                        (let scan ([at start])
                          (if (or (= at length)
                                  (url-end-character? (string-ref text at)))
                              at
                              (scan (+ at 1))))]
                       [end
                        (let trim ([at raw-end])
                          (if (and (> at start)
                                   (url-trailing-character?
                                     (string-ref text (- at 1))))
                              (trim (- at 1))
                              at))])
                  (if (= end (+ start
                                (if (and https (= start https)) 8 7)))
                      (loop (max (+ start 1) raw-end) links)
                      (loop (max end (+ start 1))
                            (cons (list start end
                                        (substring text start end))
                                  links))))))))))

  (define (valid-hyperlink? link line-length)
    (and (list? link) (<= 3 (length link) 4)
         (integer? (car link)) (integer? (cadr link))
         (<= 0 (car link)) (< (car link) (cadr link))
         (<= (cadr link) line-length) (string? (caddr link))))
  ;;; Frame composition ------------------------------------------------------------

  ;; The screen model: painted rows are cached by a key describing
  ;; what they show, so a frame repaints only what changed.  A frame
  ;; begins by naming its view -- the terminal size and the window
  ;; geometry -- and a view unlike the cached one discards every key.
  ;; A buffer's mode-driven presentation -- name, display transform,
  ;; row styler, memoized line styler -- comes from the mode registry
  ;; (mode), gathered once per window paint.

  (define screen-cache '#())
  (define cached-view #f)
  (define editor-name "e")

  (define (mode-info b)
    ;; #(name render row-styler line-styler) for b's mode, plain without one
    (let ([m (mode:of b)])
      (vector (and m (mode:name m))
              (and m (mode:render m))
              (and m (mode:row-styles m))
              (mode:line-styles b))))

  ;; a store change under this seat's buffers invalidates painted rows
  (define repaint-hooked
    (head:set-repaint-hook! (lambda () (invalidate-screen-cache!))))

  (define (begin-frame! view rows)
    (unless (equal? view cached-view)
      (set! screen-cache (make-vector rows #f))
      (set! cached-view view)))

  (define (invalidate-screen-cache!) (set! cached-view #f))

  (define (region-span row line-length)
    ;; The columns of `row` inside the selected window's active
    ;; region, as (start . end), or #f.
    (let* ([w (head:current)] [b (head:window-buffer w)])
      (and (head:buffer-marked b)
           (let* ([mr (head:buffer-mark-row b)] [mc (head:buffer-mark-col b)]
                  [pr (head:window-prow w)] [pc (head:window-pcol w)]
                  [before? (or (< pr mr) (and (= pr mr) (< pc mc)))]
                  [sr (if before? pr mr)] [sc (if before? pc mc)]
                  [er (if before? mr pr)] [ec (if before? mc pc)])
             (cond [(or (< row sr) (> row er)) #f]
                   [(= sr er) (cons sc ec)]
                   [(= row sr) (cons sc line-length)]
                   [(= row er) (cons 0 ec)]
                   [else (cons 0 line-length)])))))

  ;; Whether windows soft-wrap by default -- for config.e; a window
  ;; toggled by hand (wrap!, C-x t) keeps its own setting.
  (define wrap-lines (make-parameter #t))

  (define (buffer-wrap-setting b)
    ;; the buffer's wrap fact -- default, #t, #f, clean, or (clean . n)
    ;; -- a store property, shared by every window and head showing it
    (head:buffer-fact b 'wrap 'default))

  (define (window-wrapped? w)
    (let* ([choice (buffer-wrap-setting (head:window-buffer w))]
           [x (if (eq? choice 'default) (head:window-wrap w) choice)])
      (if (eq? x 'default) (wrap-lines) x)))

  (define (clean-wrap? w)
    (let ([x (buffer-wrap-setting (head:window-buffer w))])
      (or (eq? x 'clean) (and (pair? x) (eq? (car x) 'clean)))))

  (define (wrap-width w)
    ;; a wrapped row keeps its last column for the \ continuation mark;
    ;; a clean wrap draws none and uses the full width -- or its own
    ;; cap: (clean . n) wraps at n columns inside a wider window
    (let ([x (buffer-wrap-setting (head:window-buffer w))])
      (max 1 (cond
               [(and (pair? x) (eq? (car x) 'clean))
                (min (cdr x) (head:window-content-width w))]
               [(eq? x 'clean) (head:window-content-width w)]
               [else (- (head:window-content-width w) 1)]))))

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

  (define (add-status-hint! proc)
    (kernel:registry-add! status-hints proc))

  (define (add-buffer-status-hint! proc)
    ;; Unlike a conventional status hint, this is evaluated for every painted
    ;; window as (proc buffer active?) and can therefore describe passive
    ;; windows too.
    (kernel:registry-add! buffer-status-hints proc))

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
                  (if value (append (reverse value) out) out))))))

  (define highlighters (kernel:make-registry))

  (define (add-highlighter! proc)
    (kernel:registry-add! highlighters proc))

  (define (highlight-ranges)
    (fold-left (lambda (acc h) (append (guard (ex [else '()]) (h)) acc))
               '() (kernel:registry-items highlighters)))

  ;; Hyperlinkers produce (start end URI [id]) ranges for one buffer line.
  ;; They are deliberately separate from visual highlighters: links carry a
  ;; payload, participate in hit testing, and are also exposed to an upstream
  ;; terminal through OSC 8. Newer providers take precedence on overlap.
  (define hyperlinkers (kernel:make-registry))

  (define (add-hyperlinker! proc)
    (kernel:registry-add! hyperlinkers proc))

  (define (buffer-line-hyperlinks buffer row)
    (let ([line (vector-ref (head:buffer-lines buffer) row)])
      (fold-left
        (lambda (links proc)
          (append
            (filter (lambda (link)
                      (valid-hyperlink? link (string-length line)))
                    (guard (ex [else '()]) (proc buffer row line)))
            links))
        (detect-hyperlinks line) (kernel:registry-items hyperlinkers))))

  (define (ranges-on-row ranges w b row current?)
    (fold-left (lambda (acc r)
                 (let* ([buffer-scoped? (and (pair? r) (head:buffer? (car r)))]
                        [window-scoped? (and (pair? r) (head:window? (car r)))]
                        [scoped? (or buffer-scoped? window-scoped?)]
                        [range (if scoped? (cdr r) r)])
                   (if (and (or (and buffer-scoped? (eq? (car r) b))
                                (and window-scoped? (eq? (car r) w))
                                (and (not scoped?) current?))
                            (= (car range) row))
                       (cons (cdr range) acc)
                       acc)))
               '() ranges))


  (define wrap-cache (make-weak-eq-hashtable))

  (define (line-breaks w line)
    ;; The break table for line in w: a vector of segment starts.
    (let* ([width (wrap-width w)]
           [hit (eq-hashtable-ref wrap-cache line '())]
           [found (assv width hit)])
      (if found
          (cdr found)
          (let ([breaks (compute-breaks line width)])
            (eq-hashtable-set! wrap-cache line
                               (cons (cons width breaks) hit))
            breaks))))

  (define (segment-of breaks col)
    ;; The segment holding column col.
    (let loop ([k (- (vector-length breaks) 1)])
      (if (or (= k 0) (>= col (vector-ref breaks k)))
          k
          (loop (- k 1)))))

  (define (segment-start breaks k) (vector-ref breaks k))

  (define (segment-close breaks k len)
    ;; The last column the cursor may occupy within segment k.
    (if (< (+ k 1) (vector-length breaks))
        (- (vector-ref breaks (+ k 1)) 1)
        len))

  (define (line-segments w line)
    ;; How many screen rows the line takes in w: 1, or its soft-wrapped
    ;; segment count.
    (if (window-wrapped? w)
        (vector-length (line-breaks w line))
        1))

  (define (erase-screen!)
    ;; Blank the terminal and schedule the full repaint -- an actual
    ;; erase, which also clears the terminal's own selection highlight
    ;; where an identical overwrite would not.
    (ansi "\x1b;[2J")
    (invalidate-screen-cache!))

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
              (head:dividers)))
    (for-each
      (lambda (divider)
        (when (eq? (car divider) 'right)
          (let ([x (caddr divider)] [start (cadddr divider)]
                [height (list-ref divider 4)])
            (do ([r start (+ r 1)]) ((>= r (+ start height)))
              ;; The final row always meets whatever full-width region
              ;; ends the divider -- the echo area, or a transient
              ;; pop-up such as completions -- and connects to it.
              (let ([junction? (or (stacked-divider-crosses? x r)
                                   (= r (+ start height -1)))])
                (paint! r x (list 'divider junction?)
                        (lambda ()
                          (if junction?
                              (ansi (style:code 'chrome)
                                "\x2534;\x1b;[0m")
                              (ansi (style:code 'chrome)
                                "\x2502;\x1b;[0m")))))))))
      (head:dividers)))

  (define (paint! row xoff key draw)
    ;; Repaint the segment of the 0-based screen row starting at
    ;; column xoff unless it already shows key; a row shared by
    ;; side-by-side windows caches one key per segment.
    (let* ([entry (vector-ref screen-cache row)]
           [hit (and (pair? entry) (assv xoff entry))])
      (unless (and hit (equal? (cdr hit) key))
        (ansi "\x1b;[?25l") (goto (+ row 1) (+ xoff 1))
        (draw)
        (vector-set! screen-cache row
          (cons (cons xoff key)
                (if hit (remq hit entry) (or entry '())))))))

  (define (paint-scrollbar! w row k height sticky top total)
    (let ([side (head:window-scrollbar? w)])
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
          (paint! row
                  (+ (head:window-xoff w)
                    (if (eq? side 'right) (- (head:window-width w) 1) 0))
                  (list 'scrollbar glyph)
                  (lambda ()
                    (ansi (style:code 'chrome) glyph "\x1b;[0m")))))))

  (define (paint-line-number! row x width line first-segment?)
    (when (> width 0)
      (let* ([label (if (and line first-segment?)
                        (number->string (+ line 1))
                        "")]
             [text (string-append
                     (make-string (max 0 (- width 1 (string-length label)))
                                  #\space)
                     label " ")])
        (paint! row x (list 'line-number text)
                (lambda ()
                  (ansi (style:code 'chrome) text "\x1b;[0m"))))))

  (define (paint-window! w start height ranges)
    (let* ([b (head:window-buffer w)]
           [v (head:buffer-lines b)]
           [n (vector-length v)]
           [sticky (min height (head:buffer-sticky-lines b))]
           [top (max sticky (head:window-top w))]
           [left (head:window-left w)]
           [gutter-width (head:window-line-number-width w)]
           [gutter-x (+ (head:window-xoff w)
                        (if (eq? (head:window-scrollbar? w) 'left) 1 0))]
           [content-x (+ gutter-x gutter-width)]
           [content-width (head:window-content-width w)]
           [info (mode-info b)]
           [styles-of (vector-ref info 3)]
           [mode-tag (vector-ref info 0)]
           [current? (eq? w (head:current))])
      ;; Walk buffer lines from the top -- its first visible segment --
      ;; a soft-wrapping window painting a long line as successive
      ;; slices (the same line at successive left offsets), others one
      ;; row per line.
      (let loop ([k 0] [i (if (> sticky 0) 0 top)]
                 [seg (if (> sticky 0) 0 (head:window-topseg w))])
        (when (< k height)
          (let ([row (+ start k)])
            (paint-scrollbar! w row k height sticky top n)
            (paint-line-number! row gutter-x gutter-width
                                (and (< i n) i) (= seg 0))
            (if (< i n)
                (let* ([line (vector-ref v i)]
                       [shown (let ([r (vector-ref info 1)])
                                (or (and r (guard (ex [else #f])
                                             (let ([t (r b i line)])
                                               (and (or (and (string? t)
                                                             (= (string-length t)
                                                                (string-length line)))
                                                        (and (vector? t)
                                                             (= (vector-length t)
                                                                (string-length line))
                                                             (for-all string?
                                                                      (vector->list t))))
                                                    t))))
                                    line))]
                       [wrapped? (and (>= i sticky) (window-wrapped? w))]
                       [breaks (and wrapped? (line-breaks w line))]
                       [slice-left (if wrapped?
                                       (segment-start breaks seg)
                                       left)]
                       [bound (if (and wrapped?
                                       (< (+ seg 1)
                                          (vector-length breaks)))
                                  (segment-start breaks (+ seg 1))
                                  (string-length line))]
                       [edge (cond
                               [(and wrapped?
                                     (< (+ seg 1) (vector-length breaks)))
                                (if (clean-wrap? w) #f 'wrap)]
                               [(and (not wrapped?)
                                     (> (string-length line)
                                        (+ left content-width)))
                                'trunc]    ; it continues past the edge: $
                               [else #f])]
                       [span (and current? (region-span i (string-length line)))]
                       [marks (ranges-on-row ranges w b i current?)]
                       [links (buffer-line-hyperlinks b i)])
                  (let ([row-styles
                         (let ([f (vector-ref info 2)])
                           (and f (guard (ex [else #f]) (f b i line))))])
                    (paint! row content-x
                            (list i line shown span marks links slice-left
                                  mode-tag row-styles edge)
                            (lambda ()
                              (display-editor-line line shown span marks links
                                                   slice-left
                                                   (or row-styles
                                                       (styles-of line))
                                                   edge
                                                   content-width
                                                   bound))))
                  (if (and (>= i sticky)
                           (< (+ seg 1) (line-segments w line)))
                      (loop (+ k 1) i (+ seg 1))
                      (loop (+ k 1)
                            (if (= (+ k 1) sticky) top (+ i 1)) 0)))
                (begin
                  (paint! row content-x '(empty)
                          (lambda () (ansi (fit "" content-width))))
                  (loop (+ k 1) (+ i 1) 0))))))
      (let* ([head-prefix
              (format "~a~a~a  "
                      " "
                      (cond [(head:buffer-stale b) "!!"]
                            [(head:view-buffer? b) "[]"]
                            [(head:buffer-read-only b) "%%"]
                            [(head:buffer-modified b) "**"]
                            [else "--"])
                      editor-name)]
             [name (head:buffer-name b)]
             [app-position
              (let* ([a (head:app-of b)]
                     [position (and a (head:app-status-position a))])
                (and position
                     (guard (ex [else #f]) (position b))))]
             [status-row (if (pair? app-position)
                             (car app-position) (head:window-prow w))]
             [status-col (if (pair? app-position)
                             (cdr app-position) (head:window-pcol w))]
             [head (format "~a~a  L~a C~a"
                           head-prefix name
                           (+ status-row 1) (+ status-col 1))]
             [mode-text (if mode-tag (format "  (~a)" mode-tag) "")]
             [hint-values (status-hint-values b current?)]
             [hint-text (apply string-append (map car hint-values))]
             [hint-wide-extra
              (fold-left
                (lambda (extra character)
                  (+ extra
                     (max 0 (- (terminal-character-width character) 1))))
                0 (string->list hint-text))]
             [status (format "~a~a~a " head mode-text hint-text)]
             [window-buttons " [↕][↔][×]"])
        (let ([stale? (head:buffer-stale b)])
          (paint! (+ start height) (head:window-xoff w)
                  (list 'status status current? stale?)
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
                           [content-width
                            (max 0 (- (head:window-width w)
                                      (string-length window-buttons)
                                      hint-wide-extra))]
                           [text (string-append (fit status content-width)
                                                window-buttons)]
                           [n (string-length text)]
                           [cs (min (string-length head) content-width)]
                           [ns (min (string-length head-prefix) content-width)]
                           [ne (min (+ ns (string-length name)) content-width)]
                           [hs (min (+ (string-length head)
                                       (string-length mode-text))
                                    content-width)]
                           [he (min (+ hs (string-length hint-text))
                                    content-width)]
                           [normal-start
                            (if stale? (min 3 content-width) 0)])
                      (ansi bar)
                      (when stale?
                        ;; the !! flag in red, the rest as usual
                        (ansi "\x1b;[31m" (substring text 0 normal-start)
                          fg))
                      (ansi (substring text normal-start ns)
                        "\x1b;[1m" (substring text ns ne)
                        "\x1b;[22m" (substring text ne cs))
                      (ansi (substring text cs hs))
                      (let loop ([values hint-values] [at hs])
                        (when (and (pair? values) (< at he))
                          (let* ([value (car values)]
                                 [end (min (+ at (string-length (car value)))
                                           he)])
                            (case (cdr value)
                              [(italic) (ansi "\x1b;[3m")]
                              [(red) (ansi "\x1b;[31m")])
                            (ansi (substring text at end))
                            (case (cdr value)
                              [(italic) (ansi "\x1b;[23m")]
                              [(red) (ansi fg)])
                            (loop (cdr values) end))))
                      (ansi (substring text he content-width)
                        "\x1b;[1m"
                        (substring text content-width n)
                        "\x1b;[0m"))))))))


  ;;; The frame driver ----------------------------------------------------------------

  ;; The screen's size, whether the terminal is ours, the viewport
  ;; logic that keeps point visible, the echo area's painting, the
  ;; cursor, the title, the visual bell -- and the frame itself:
  ;; redraw! composes one under the redraw lock (size, echo geometry,
  ;; layout, view refresh, viewports, painting, cursor).

  (define rows 24)
  (define cols 80)
  (define (screen-rows) rows)
  (define (set-screen-rows! n) (set! rows n))
  (define (screen-cols) cols)
  (define (set-screen-cols! n) (set! cols n))
  (define (mark-size-dirty!) (set! size-dirty? #t))
  (define (set-screen-live! on?) (set! the-screen-live? on?))
  (define (screen-live?) the-screen-live?)
  (define (reset-cursor-style!)
    ;; on the way out: the terminal's default cursor, unless it already shows
    (unless (string=? cursor-style-shown "\x1b;[0 q")
      (ansi "\x1b;[0 q")))
  (define the-visual-bell-active? #f)
  (define (current-lines) (head:buffer-lines (head:window-buffer (head:current))))

  ;;; Terminal size ---------------------------------------------------------

  ;; The system-specific work (termios, ioctl, SIGWINCH) lives in (sys);
  ;; here only the editor's idea of its size.  Without a terminal, sizes
  ;; fall back to LINES/COLUMNS.

  (define size-dirty? #t)

  ;; C-l also forces a size refresh in case resize events are unavailable.
  (define sigwinch-registered
    (watch-terminal-resize! (lambda () (set! size-dirty? #t))))

  (define (env-number name fallback)
    (let* ([s (getenv name)]
           [n (and s (string->number s))])
      (if (and n (exact? n) (integer? n) (> n 0)) n fallback)))

  (define (terminal-size!)
    (when size-dirty?
      (set! size-dirty? #f)
      (set! rows (max 4 (env-number "LINES" 24)))
      (set! cols (max 20 (env-number "COLUMNS" 80)))
      (let ([size (terminal-size)])
        (when size
          (set! rows (max 4 (car size)))
          (set! cols (max 20 (cdr size)))))))
  (define (window-layout)
    ;; Tile the persistent split tree into the screen minus the echo
    ;; area; -> ((window start text-height) ...), start 0-based.  The
    ;; head remembers the tiling for mouse hit-testing.
    (head:tile! cols (max 2 (- rows (echo:height)))))

  (define (page-size)
    ;; The scrollable body height. Sticky app rows are fixed chrome and do not
    ;; form part of a page.
    (let ([height (caddr (assq (head:current) (window-layout)))])
      (max 1 (- height
                (min height
                     (head:buffer-sticky-lines (head:window-buffer (head:current))))))))

  ;; Soft wrap breaks at word boundaries: each line has a break table
  ;; -- the start position of every visual segment -- computed
  ;; greedily (the last space that fits; a word longer than the width
  ;; breaks mid-word) and memoized per line string and width, like the
  ;; style cache: edits replace line strings, so identity keys it.
  (define (set-buffer-viewports! b position top excluded-windows)
    (let* ([v (head:buffer-lines b)]
           [row (max 0 (min (car position) (- (vector-length v) 1)))]
           [col (max 0 (min (cdr position)
                            (string-length (vector-ref v row))))]
           [top (max 0 (min top (- (vector-length v) 1)))])
      (head:buffer-spot-row-set! b row)
      (head:buffer-spot-col-set! b col)
      (head:buffer-spot-top-set! b top)
      (for-each
        (lambda (w)
          (when (and (eq? (head:window-buffer w) b)
                     (not (memq w excluded-windows)))
            (head:window-top-set! w top)
            (head:window-topseg-set! w 0)
            (head:window-left-set! w 0)
            (head:window-prow-set! w row)
            (head:window-pcol-set! w col)))
        (head:windows))
      b))

  (define (reset-buffer-viewports! b position)
    (set-buffer-viewports! b position 0 '()))

  (define (view-invalidate! b)
    ;; Dynamic row renderers can change their presentation while the view's
    ;; structural placeholder (current-lines) remain equal. Mark the display stale
    ;; explicitly so the next frame asks the renderer for every visible row.
    (unless (head:app-buffer? b)
      (error 'view-invalidate! "not an app or view buffer" b))
    (invalidate-screen-cache!)
    b)

  (define (point-visible?)
    (let* ([entry (assq (head:current) (window-layout))]
           [p (window-screen-position (head:current) (head:window-prow (head:current)) (head:window-pcol (head:current)))])
      (and entry (< (cadr entry) (car p))
           (<= (car p) (+ (cadr entry) (caddr entry))))))

  (define (rows-before w prow pcol)
    ;; Screen rows between w's top -- its first visible segment -- and
    ;; point, wrap-aware.
    (let* ([v (head:buffer-lines (head:window-buffer w))]
           [sticky (head:buffer-sticky-lines (head:window-buffer w))])
      (let loop ([i (max sticky (head:window-top w))]
                 [n (- (head:window-topseg w))])
        (if (>= i prow)
            (+ n (if (window-wrapped? w)
                     (segment-of (line-breaks w (vector-ref v prow)) pcol)
                     0))
            (loop (+ i 1) (+ n (line-segments w (vector-ref v i))))))))

  ;; The minimal visual distance kept between the cursor and the
  ;; window's top and bottom edges: scrolling starts that early, and
  ;; the cursor enters the zone only where the view cannot scroll any
  ;; further (the ends of the buffer).  Configurable in config.e.
  (define scroll-margin
    (make-parameter 8 (lambda (v) (max 0 v))))

  (define (view-overflows? w v height)
    ;; Is there more content than the window holds, counting from its
    ;; top segment?
    (let loop ([i (max (head:buffer-sticky-lines (head:window-buffer w))
                       (head:window-top w))]
               [n (- (head:window-topseg w))])
      (cond [(> n height) #t]
            [(>= i (vector-length v)) #f]
            [else (loop (+ i 1)
                        (+ n (line-segments w (vector-ref v i))))])))

  (define (scroll-window! w height)
    ;; Clamp w's point to its buffer (edits in another window may have moved
    ;; the ground under it) and scroll so point stays visible -- at
    ;; least scroll-margin rows from the edges, where the buffer's
    ;; ends allow.
    (let* ([v (head:buffer-lines (head:window-buffer w))]
           [sticky (head:buffer-sticky-lines (head:window-buffer w))]
           [height (max 1 (- height sticky))]
           [prow (max 0 (min (head:window-prow w) (- (vector-length v) 1)))]
           [pcol (max 0 (min (head:window-pcol w)
                             (string-length (vector-ref v prow))))]
           [m (min (scroll-margin) (div (max 0 (- height 1)) 2))])
      (head:window-prow-set! w prow)
      (head:window-pcol-set! w pcol)
      (unless (or (not (head:app-cursor-visible-in? w))
                  (head:app-manages-window-viewport? w))
        (if (window-wrapped? w)
          (let ([pseg (segment-of (line-breaks w (vector-ref v prow))
                                  pcol)])
            ;; a stale top (edits, toggles) clamps into the buffer
            (head:window-top-set! w
              (max sticky
                   (min (head:window-top w) (- (vector-length v) 1))))
            (head:window-topseg-set!
              w (min (head:window-topseg w)
                     (- (line-segments w (vector-ref v (head:window-top w)))
                        1)))
            ;; point above the view: its own segment row becomes the
            ;; top, so moving up scrolls by one visual row, not by the
            ;; whole wrapped line
            (when (and (>= prow sticky)
                       (or (< prow (head:window-top w))
                         (and (= prow (head:window-top w))
                           (< pseg (head:window-topseg w)))))
              (head:window-top-set! w prow)
              (head:window-topseg-set! w pseg))
            ;; the margin above: retreat while the top of the buffer
            ;; still allows
            (let retreat ()
              (when (and (< (rows-before w prow pcol) m)
                         (or (> (head:window-top w) sticky)
                             (> (head:window-topseg w) 0)))
                (if (> (head:window-topseg w) 0)
                    (head:window-topseg-set! w (- (head:window-topseg w) 1))
                    (begin
                      (head:window-top-set! w (- (head:window-top w) 1))
                      (head:window-topseg-set!
                        w (- (line-segments
                               w (vector-ref v (head:window-top w)))
                             1))))
                (retreat)))
            ;; and below: advance one visual row at a time, only while
            ;; content actually overflows the window.  Each step reduces
            ;; distance by exactly one; carrying it avoids rescanning from
            ;; top to point at every step on a large jump.
            (let advance ([distance (rows-before w prow pcol)])
              (when (and (>= distance (- height m))
                         (or (< (head:window-top w) prow)
                             (< (head:window-topseg w) pseg))
                         (view-overflows? w v height))
                (if (< (+ (head:window-topseg w) 1)
                       (line-segments w (vector-ref v (head:window-top w))))
                    (head:window-topseg-set! w (+ (head:window-topseg w) 1))
                    (begin (head:window-top-set! w (+ (head:window-top w) 1))
                           (head:window-topseg-set! w 0)))
                (advance (- distance 1)))))
          (begin
            (head:window-topseg-set! w 0)
            (when (< prow (+ (head:window-top w) m))
              (head:window-top-set! w (max sticky (- prow m))))
            (when (>= prow (+ (head:window-top w) height (- m)))
              (head:window-top-set! w
                (min (- prow (- height 1 m))
                     (max sticky (- (vector-length v) height)))))
            (when (< pcol (head:window-left w)) (head:window-left-set! w pcol))
            (when (>= pcol (+ (head:window-left w) (head:window-content-width w)))
              (head:window-left-set! w
                (- pcol (head:window-content-width w) -1))))))))

  ;; The cache holds, per screen row, the key describing what that row
  ;; currently shows; a row is repainted only when its key changes.  Any
  ;; change of view (size, search highlight, window arrangement) discards
  ;; the whole cache.
  (define the-screen-live? #f) ; the terminal is ours only between main's
                           ; alternate-screen enter and exit
  (define cursor-style-shown "\x1b;[0 q")   ; DECSCUSR last emitted

  (define (echo-indent-now) (echo:indent-now cols))

  (define (compute-echo-spans content len)
    (echo:compute-spans content len cols))

  (define (echo-position k)
    ;; Visual (line . column) of content index k, per echo-spans.
    (let loop ([spans (echo:spans)] [line 0])
      (let ([span (car spans)])
        (if (or (null? (cdr spans)) (< k (cdr span))
                (and (= k (cdr span)) (< k (string-length (echo:text)))
                     (char=? (string-ref (echo:text) k) #\newline)))
            (cons line (+ (if (= line 0) 0 (echo-indent-now))
                          (- k (car span))))
            (loop (cdr spans) (+ line 1))))))

  ;; Parameterized on (by eval, around an evaluation), the cursor parks
  ;; at the end of the echo area's content -- and is drawn as a blinking
  ;; underline, so a running evaluation is visible at a glance.
  (define cursor-in-echo (make-parameter #f))

  ;; Prompts may parameterize this to style the echo content -- M-x
  ;; gives the expression Scheme highlighting.  A procedure from the
  ;; content string to a styles vector (as modes produce), or #f to
  ;; style nothing; a raising styler paints plain.
  (define echo-highlight (make-parameter #f))

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

  (define (completion-styler match? highlight?)
    ;; Style one completion input by its semantic state: an incomplete or
    ;; unknown value is italic, an exact match is plain, and a distinguished
    ;; match (an editor symbol, for example) uses the editor face.
    (lambda (input)
      (make-vector (string-length input)
                   (cond [(highlight? input) 'editor]
                         [(match? input) 'plain]
                         [else 'italic]))))

  (define (echo-cursor-now)
    (or (echo:cursor)
        (and (cursor-in-echo)
             (+ (string-length (echo:text)) (string-length (echo:ghost))))))

                              ; applied while the text still matches

  (define (show-message! s styles-pair)
    ;; Put s in the echo area and paint right away (once the screen is
    ;; the editor's).
    (echo:set-indent! #f)
    (echo:set-input-end! #f)
    (echo:set-text! s)
    (echo:set-ghost! "")
    (echo:set-styles! styles-pair)
    (present-echo!))

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

  (define (echo-append! component text styler replace?)
    ;; Append one line to the echo area's transient log: every logged
    ;; (echo:text) stacks up there, component-prefixed, until the next key
    ;; settles the area.  With replace? true the component's newest
    ;; line is superseded when it is also the newest overall --
    ;; progress redrawn in place -- never another component's.  A
    ;; stale indicator gives way; a prompt's input line stays put
    ;; below, and so does a running evaluation's kept query -- the
    ;; user sees what is running.
    (echo-queue! component text styler replace?)
    (present-echo!))

  (define (echo-queue! component text styler replace? . rest)
    ;; Update transient echo state without painting it; batch publishers use
    ;; this before one final present-echo!.
    (echo:queue! component text styler replace?
                 (if (pair? rest) (car rest) "")
                 (and (echo-cursor-now) #t)))

  (define (present-echo!)
    ;; Present the echo area now, mid-command included (once the
    ;; screen is the editor's).  Grown or shrunk it takes a full
    ;; redraw -- the windows above shift, their status bars with them
    ;; -- otherwise painting the area suffices.
    (when the-screen-live?
      (let ([h (echo:height)])
        (update-echo-geometry!)
        (if (= h (echo:height))
            (paint-echo-area!)
            (redraw!)))
      (flush-output-port (terminal-output-port))))

  (define (echo-log-prefix e) (echo:log-prefix e cols))
  (define (echo-log-spans prefix-len content)
    (echo:log-spans prefix-len content cols))
  (define (echo-log-rows e) (echo:log-rows e cols))

  (define (display-echo-log-row prefix text styler ghost k span wrapped?)
    ;; One visual row of a transient-log entry: the grey prefix on the
    ;; first, its indent on continuations, the slice under the
    ;; component's styler, a mark closing every wrapped row.
    (let* ([lead (if (= k 0)
                     prefix
                     (make-string (min (string-length prefix)
                                       (quotient cols 2))
                                  #\space))]
           [start (car span)]
           [end (cdr span)]
           [styles (and styler (guard (ex [else #f]) (styler text)))]
           [text-end (min end (string-length text))]
           [ghost-start (max start (string-length text))]
           [content (string-append text ghost)])
      (ansi "\x1b;[0m" (style:code 'chrome) lead)
      (when (< start text-end)
        (if styles
            (emit-runs text styles start text-end)
            (ansi "\x1b;[0m" (substring text start text-end))))
      (when (< ghost-start end)
        (ansi "\x1b;[0m" (style:code 'chrome)
          (substring content ghost-start end)))
      (ansi "\x1b;[0m"
        (make-string (max 0 (- cols (string-length lead) (- end start)
                               (if wrapped? 1 0)))
                     #\space)
        (if wrapped? "\\" ""))))

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
                  (paint! row 0 (list 'echo-log e k span wrapped?)
                    (lambda ()
                      (display-echo-log-row prefix text (caddr e) ghost
                                            k span wrapped?)))
                  (rloop (cdr spans) (+ k 1) (+ row 1))))))))
    (when (> (echo:live-height) 0)
      (let* ([content (string-append (echo:text) (echo:ghost))]
             [ghost-at (string-length (echo:text))]
             [total (length (echo:spans))]
             [indent (echo-indent-now)])
        (let loop ([line (echo:scroll)] [row (- rows (echo:live-height))])
          (when (< row rows)
            (let* ([span (list-ref (echo:spans) line)]
                   [start (car span)]
                   [end (min (cdr span) (string-length content))]
                   [end (max end start)]
                   [lead (if (= line 0) 0 indent)]
                   [wrapped? (< line (- total 1))]
                   [cut (min (max (- ghost-at start) 0) (- end start))]
                   ;; a prompt's label -- content up to (echo:indent) on
                   ;; the first visual line -- is painted grey, the
                   ;; transient log's shade: quiet chrome, the input
                   ;; carries the emphasis
                   [lb (if (= line 0)
                           (min (or (echo:indent) 0) (+ start cut))
                           0)])
              (paint! row 0
                (list 'echo line (substring content start end)
                      cut lead lb wrapped? (and (echo-highlight) #t)
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
                    (ansi (make-string lead #\space))
                    (when (> lb 0)
                      (ansi (style:code 'chrome)
                            (substring content 0 lb) "\x1b;[0m"))
                    (if styles
                        ;; styled runs for the typed part
                        (emit-runs content styles (+ start lb)
                                   (+ start cut))
                        (ansi (substring content (+ start lb)
                                         (+ start cut))))
                    (ansi "\x1b;[0m" (style:code 'chrome)
                          (substring content (+ start cut) end)
                          "\x1b;[0m"
                          (make-string
                            (max 0 (- cols lead (- end start)
                                      (if wrapped? 1 0)))
                            #\space)
                          (if wrapped? "\\" ""))))))
            (loop (+ line 1) (+ row 1)))))))

  (define (echo-cap)
    ;; How tall the whole echo area may grow: everything but each
    ;; window's minimum -- head:min-window-lines of text (at least 2,
    ;; redraw!'s collapse threshold) plus its status line.
    (max 1 (- rows (head:layout-min-height (head:root)))))

  (define (update-echo-geometry!)
    ;; The echo area stacks the pending transient-log (current-lines) above the
    ;; live line.  The live line's height follows its wrapped content
    ;; (the grey suggestion included): prompt input wraps with
    ;; continuations indented to the prompt text, and a plain (echo:text)
    ;; that overflows the width wraps the same way at indent zero --
    ;; up to eight lines, after which it scrolls, keeping the prompt
    ;; cursor's line visible; empty behind pending (current-lines) it folds
    ;; away.  The whole area grows until the windows above hit their
    ;; minimum; past that the oldest pending (current-lines) are evicted -- they
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

  (define (paint-visual-bell!)
    (when the-visual-bell-active?
      (let loop ([row (- rows (echo:height))])
        (when (< row rows)
          (goto (+ row 1) 1)
          (ansi "\x1b;[7m" (make-string cols #\space) "\x1b;[0m")
          (loop (+ row 1))))))


  (define terminal-title-shown #f)

  (define (safe-terminal-title s)
    ;; OSC is terminated by BEL or ST. Do not let a buffer name inject either
    ;; terminator (or another terminal control) into the host terminal.
    (list->string
      (map (lambda (c)
             (let ([n (char->integer c)])
               (if (or (< n 32) (= n 127)) #\space c)))
           (string->list s))))

  (define (update-terminal-title!)
    ;; OSC 2 is understood by GNOME Terminal, xterm, and nested e terminals.
    (let ([title (string-append "e: " (head:buffer-name (head:window-buffer (head:current))))])
      (unless (equal? title terminal-title-shown)
        (set! terminal-title-shown title)
        (ansi "\x1b;]2;" (safe-terminal-title title) "\x1b;\\"))))

  (define (window-screen-position w prow pcol)
    ;; 1-based screen (row . col) of a buffer position in w, wrap-aware.
    (let* ([entry (assq w (window-layout))]
           [sticky (head:buffer-sticky-lines (head:window-buffer w))]
           [x (+ (head:window-xoff w)
                 (if (eq? (head:window-scrollbar? w) 'left) 1 0)
                 (head:window-line-number-width w))]
           [screen-row (if (< prow sticky)
                           (+ (cadr entry) prow 1)
                           (+ (cadr entry) sticky
                              (rows-before w prow pcol) 1))])
      (if (and (>= prow sticky) (window-wrapped? w))
          (cons screen-row
                (let ([breaks (line-breaks
                                w (vector-ref
                                    (head:buffer-lines (head:window-buffer w)) prow))])
                  (+ x
                     (- pcol (segment-start breaks (segment-of breaks pcol)))
                     1)))
          (cons screen-row (+ x (- pcol (head:window-left w)) 1)))))

  (define (place-cursor!)
    ;; Park the cursor in the echo area (a prompt, or a running
    ;; evaluation -- the latter drawn as a blinking underline), else
    ;; put it at point in the current window.  Also called on its own
    ;; when an interaction is about to wait for a key, so its cursor
    ;; rules take effect without a repaint.
    (let* ([cursor (echo-cursor-now)]
           [a (head:app-of (head:window-buffer (head:current)))]
           [visible? (or cursor (head:app-cursor-visible-in? (head:current)))])
      (if cursor
          (let ([p (echo-position cursor)])
            (goto (+ (- rows (echo:live-height)) (- (car p) (echo:scroll)) 1)
              (min (+ (cdr p) 1) cols)))
          (when visible?
            (let ([p (window-screen-position (head:current)
                                             (head:window-prow (head:current)) (head:window-pcol (head:current)))])
              (goto (min (car p) rows) (min (cdr p) cols)))))
      (let* ([a (head:app-of (head:window-buffer (head:current)))]
             [app-style (head:buffer-fact (head:window-buffer (head:current))
                                          'cursor-style #f)]
             [style (cond
                      [(cursor-in-echo) "\x1b;[3 q"]
                      ;; a prompt: the cursor is in the echo area's input,
                      ;; which is editable whatever the buffer behind it
                      [(echo:cursor) "\x1b;[0 q"]
                      [(and app-style (not (eq? app-style 'default)))
                       (case app-style
                         [(block) "\x1b;[2 q"]
                         [(underline) "\x1b;[4 q"]
                         [(bar) "\x1b;[6 q"]
                         [(blinking-block) "\x1b;[1 q"]
                         [(blinking-underline) "\x1b;[3 q"]
                         [(blinking-bar) "\x1b;[5 q"])]
                      ;; a bar where typing cannot land: a read-only buffer
                      [(head:buffer-read-only (head:window-buffer (head:current)))
                       "\x1b;[5 q"]
                      [else "\x1b;[0 q"])])
        (unless (string=? style cursor-style-shown)
          (set! cursor-style-shown style)
          (ansi style)))
      (ansi (if visible? "\x1b;[?25h" "\x1b;[?25l"))
      (flush-output-port (terminal-output-port))))

  ;;; The frame -----------------------------------------------------------------------

  ;; A frame is one indivisible transaction on the terminal -- the
  ;; cache and the output happen under the redraw lock, since PTY
  ;; readers and the bell's timer request frames while the main thread
  ;; waits for input.  Anyone else who writes to the terminal (the
  ;; clipboard's OSC 52) takes the lock too.
  (define redraw-lock (make-mutex))

  (define (redraw-frame!)
    ;; The frame goes out inside a synchronized update (mode 2026):
    ;; a supporting terminal holds rendering until the closing pair,
    ;; so a scroll and the repaint over it appear as one; others
    ;; ignore the mode.
    (ansi "\x1b;[?2026h")
    (terminal-size!)
    (update-echo-geometry!)
    ;; window geometry is otherwise set while painting, one frame
    ;; stale from here -- refresh views against the current layout
    (window-layout)
    (head:refresh-visible-views!)
    ;; a terminal too small for the splits collapses back to one window
    (head:fit-layout! cols (- rows (echo:height)))
    (let* ([layout (window-layout)]
           [view (list rows cols
                       (map (lambda (entry)
                              (list (cadr entry) (caddr entry)
                                    (head:window-xoff (car entry))
                                    (head:window-width (car entry))))
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
                                    (head:window-scrollbar? w)
                                    (head:window-line-number-width w)
                                    (head:buffer-sticky-lines (head:window-buffer w))))
                            (head:windows)))])
      (for-each (lambda (entry) (scroll-window! (car entry) (caddr entry)))
                layout)
      (begin-frame! view rows)
      (paint-dividers! layout)
      (let ([ranges (highlight-ranges)])
        (for-each (lambda (entry)
                    (paint-window! (car entry) (cadr entry) (caddr entry) ranges))
                  layout))
      (paint-echo-area!)
      (paint-visual-bell!))
    (place-cursor!)
    (ansi "\x1b;[?2026l")
    (flush-output-port (terminal-output-port)))

  (define (redraw!)
    ;; a whole frame, title included, as one transaction
    (with-mutex redraw-lock
      (update-terminal-title!)
      (redraw-frame!)))

  (define visual-bell-generation 0)

  (define (visual-bell!)
    ;; Arm an overlay but let the caller's ordinary frame paint it. A PTY
    ;; reader invokes this between decoding output and its scheduled redraw;
    ;; starting a nested frame here would stop it at BEL before the diagnostic
    ;; bytes which commonly follow.
    (when the-screen-live?
      (let ([display (terminal-output-port)]
            [generation
             (with-mutex redraw-lock
               (set! visual-bell-generation (+ visual-bell-generation 1))
               (set! the-visual-bell-active? #t)
               visual-bell-generation)])
        (fork-thread
          (lambda ()
            (sleep (make-time 'time-duration 50000000 0))
            (when the-screen-live?
              (parameterize ([terminal-output-port display])
                (with-mutex redraw-lock
                  ;; A newer bell owns the deadline and must not be cleared by
                  ;; an older animation's expiry.
                  (when (= generation visual-bell-generation)
                    (set! the-visual-bell-active? #f)
                    (invalidate-screen-cache!)
                    (update-terminal-title!)
                    (redraw-frame!))))))))))
)
