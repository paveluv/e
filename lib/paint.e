;; paint.e -- the row painter: the library (paint), v2 core
;; dissolution (docs/DESIGN2.md).  Pure infrastructure with no init!.
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
          paint-dividers! paint-window! set-mode-hook!)
  (import (rnrs) (rnrs mutable-strings) (rnrs r5rs)
          (only (chezscheme)
                box void format make-parameter make-weak-eq-hashtable
                eq-hashtable-ref eq-hashtable-set! remq)
          (only (sys) terminal-output-port terminal-character-width)
          (prefix (styles) styles:)
          (prefix (strings) strings:)
          (prefix (head) head:)
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
          (ansi "\x1b;[0m" (styles:style-code style))
          (when sel (ansi (styles:style-code 'selection)))
          (case bg
            [(match-point) (ansi (styles:style-code 'match-point))]
            [(active) (ansi (styles:style-code 'active))]
            [(active-shadow) (ansi (styles:style-code 'active-shadow))]
            [(match) (ansi (styles:style-code 'match))]
            [else (void)])
          (when mk (ansi (styles:style-code mk)))
          (when link (open-link link))
          (ansi (segment col end))
          (when link (close-link))
          (loop end))))
    (when edge
      (ansi "\x1b;[0m" (styles:style-code 'chrome)
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
          (ansi "\x1b;[0m" (styles:style-code st) (substring content i j))
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
        (let ([http (strings:search text "http://" from length)]
              [https (strings:search text "https://" from length)])
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
  ;; The mode registry is the head's; the painter learns a buffer's
  ;; mode-driven presentation through one hook, as a vector:
  ;; #(mode-name render row-styler line-styler).

  (define screen-cache '#())
  (define cached-view #f)
  (define editor-name "e")

  (define mode-hook (lambda (b) (vector #f #f #f (lambda (line) #f))))
  (define (set-mode-hook! proc) (set! mode-hook proc))

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

  ;; Whether (head:windows) soft-wrap by default -- for config.e; a window
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
    ;; (head:windows) too.
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
                              (ansi (styles:style-code 'chrome)
                                "\x2534;\x1b;[0m")
                              (ansi (styles:style-code 'chrome)
                                "\x2502;\x1b;[0m")))))))))
      (head:dividers)))

  (define (paint! row xoff key draw)
    ;; Repaint the segment of the 0-based screen row starting at
    ;; column xoff unless it already shows key; a row shared by
    ;; side-by-side (head:windows) caches one key per segment.
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
                    (ansi (styles:style-code 'chrome) glyph "\x1b;[0m")))))))

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
                  (ansi (styles:style-code 'chrome) text "\x1b;[0m"))))))

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
           [info (mode-hook b)]
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

)
