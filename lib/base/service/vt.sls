;; vt.sls -- base-owned terminal emulator, PTY actors, and shared publication.

(import (only (foundation edoc) elibrary))
(elibrary (service vt)
  (export close! close-all! (rename (terminal-color-scheme! color-scheme!))
          (rename (terminal-emulator-feed! emulator-feed!))
          (rename (terminal-emulator-frame emulator-frame))
          (rename (terminal-emulator-hyperlinks emulator-hyperlinks))
          (rename (terminal-emulator-input emulator-input))
          (rename (terminal-emulator-mouse-input emulator-mouse-input))
          (rename (terminal-emulator-replies emulator-replies))
          (rename (terminal-emulator-resize! emulator-resize!))
          (rename (terminal-emulator-screen emulator-screen))
          (rename (terminal-emulator-state emulator-state))
          (rename (terminal-emulator-styles emulator-styles))
          (rename (terminal-emulator-unsupported emulator-unsupported))
          (rename (terminal-emulator? emulator?)) init!
          (rename (make-terminal-emulator make-emulator)) open! running
          (rename (terminal-scrollback scrollback)) send! (rename (terminal-shell shell))
          transcript)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation color) color:)
          (prefix (foundation datum) datum:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (state actor) actor:)
          (prefix (state store) store:)
          (prefix (state surface) surface:)
          (prefix (sys activity) activity:)
          (prefix (sys glyph) glyph:)
          (prefix (sys sys) sys:))

  (define-record-type terminal-state
    (nongenerative e-vt-terminal-state-v1)
    (fields owner buffer process lock mailbox cache unsupported
            (mutable queued?) (mutable controller) (mutable scheme)
            (mutable generation) (mutable revision) (mutable title)
            (mutable clipboard-sequence) (mutable clipboard-target) (mutable failure)
            (mutable sync-deadline)
            (mutable rows) (mutable cols) (mutable screen) (mutable wrapped)
            (mutable row) (mutable col)
            (mutable saved-row) (mutable saved-col) (mutable saved-state)
            (mutable scroll-top) (mutable scroll-bottom)
            (mutable left-margin) (mutable right-margin) (mutable margin-mode)
            (mutable memory-lock)
            (mutable parser) (mutable parameters)
            (mutable osc-escape) (mutable osc-text) (mutable replies)
            (mutable charset) (mutable charset-g1) (mutable charset-target)
            (mutable shift)
            (mutable wrap-pending) (mutable autowrap) (mutable origin)
            (mutable insert) (mutable newline) (mutable reverse-screen)
            (mutable cursor-keys) (mutable keypad) (mutable meta-eight-bit)
            (mutable controls-eight-bit)
            (mutable cursor-visible) (mutable cursor-shape) (mutable tab-stops)
            (mutable last-character)
            (mutable dirty) (mutable alive) (mutable bell)
            (mutable bell-visible) (mutable bell-deadline)
            (mutable mouse) (mutable mouse-sgr) (mutable mouse-utf8)
            (mutable mouse-urxvt) (mutable focus-reporting)
            (mutable bracketed) (mutable main-screen) (mutable main-wrapped)
            (mutable main-row) (mutable main-col) (mutable main-state)
            (mutable alternate-screen) (mutable alternate-wrapped)
            (mutable alternate-styles)
            (mutable alternate-state)
            (mutable history)
            (mutable styles) (mutable main-styles)
            (mutable rendered)
            (mutable sgr) (mutable style) (mutable link) (mutable clipboard)
            (mutable palette) (mutable default-foreground)
            (mutable default-background)
            (mutable printer-controller) (mutable printer-pending)
            (mutable printer-output)
            ;; VT100 setup toggles xterm accepts: smooth scroll (4),
            ;; autorepeat (8), 80/132 switching (40), NRC (42), and reverse
            ;; wraparound (45). Only 45 changes behavior here, but all are
            ;; tracked so DECRQM answers honestly and a vttest run does
            ;; not flood the log with modes every terminal accepts.
            (mutable extra-modes)
            ;; Per-row DECSWL/DECDWL/DECDHL attribute: 'single, 'wide,
            ;; 'top, or 'bottom. Display metadata only -- the buffer keeps
            ;; full-width content so DECSWL restores it, and the rendering
            ;; expands the left half of decorated rows.
            (mutable line-attributes)
            (mutable main-line-attributes)
            (mutable alternate-line-attributes)))

  ;; Captured under the emulator lock, immutable after capture. Publication
  ;; releases that lock before calling store/surface; public reads own data.
  (define-record-type frame
    (nongenerative e-vt-frame-v1)
    (fields rows cursor size top cursor-shape))
  (define-record-type rendition
    (nongenerative e-vt-rendition-v1)
    (fields cells styles links text))

  ;; Hyperlinks are rendition metadata just like color and attributes. Keeping
  ;; both in the existing style grid makes every cell-moving operation carry
  ;; the OSC 8 identity automatically.
  (define-record-type terminal-cell-style
    (nongenerative e-vt-cell-style-v1)
    (fields face link))

  (define (cell-style-face style)
    (if (terminal-cell-style? style)
        (terminal-cell-style-face style) style))

  (define (cell-style-link style)
    (and (terminal-cell-style? style)
         (terminal-cell-style-link style)))

  (define (printed-cell-style state)
    (if (terminal-state-link state)
        (make-terminal-cell-style (terminal-state-style state)
                                  (terminal-state-link state))
        (terminal-state-style state)))

  ;; Scrollback grows by one line per scroll and is bounded by
  ;; terminal-scrollback, so it is kept as an amortized queue: new lines
  ;; are consed onto the front and evictions pop the back, reversing the
  ;; front into the back only when it empties. Every push is O(1)
  ;; amortized where a per-line list append would copy the whole history.
  (define-record-type scrollback
    (nongenerative e-vt-scrollback-v1)
    (fields (mutable front) (mutable back) (mutable count)))

  (define-record-type scrollback-line
    (nongenerative e-vt-scrollback-line-v1)
    (fields cells styles wrapped))

  (define (make-empty-scrollback) (make-scrollback '() '() 0))

  (define (scrollback-push! queue line limit)
    (when (> limit 0)
      (let evict ()
        (when (>= (scrollback-count queue) limit)
          (when (null? (scrollback-back queue))
            (scrollback-back-set! queue (reverse (scrollback-front queue)))
            (scrollback-front-set! queue '()))
          (scrollback-back-set! queue (cdr (scrollback-back queue)))
          (scrollback-count-set! queue (- (scrollback-count queue) 1))
          (evict)))
      (scrollback-front-set! queue (cons line (scrollback-front queue)))
      (scrollback-count-set! queue (+ (scrollback-count queue) 1))))

  (define (scrollback-entries queue)
    ;; Oldest first, the order the buffer presents.
    (append (scrollback-back queue) (reverse (scrollback-front queue))))

  (define (scrollback-of-entries entries)
    (make-scrollback '() entries (length entries)))

  ;; Scrollback lines are immutable once pushed, but their effective styles
  ;; depend on the palette, the default colors, and reverse video. Cache
  ;; each line's rendered form per emulator so a dirty frame restyles only
  ;; the live screen, and drop the cache whenever one of those inputs
  ;; changes. Each emulator owns a weak row cache under its existing lock;
  ;; evicted scrollback entries release their cached rendition.

  (define (invalidate-rendered-scrollback! state)
    (hashtable-clear! (terminal-state-cache state)))

  ;; Synchronized output (private mode 2026): while set, dirty frames are
  ;; parsed but not presented, so an application can compose a frame
  ;; without tearing. A deadline bounds the hold in case the application
  ;; dies mid-frame.
  (define (start-synchronized-update! state)
    (terminal-state-sync-deadline-set! state (sys:after 1)))

  (define (synchronized-update-pending? state)
    (and (memv 2026 (terminal-state-extra-modes state))
         (let ([deadline (terminal-state-sync-deadline state)])
           (and deadline
                (time<? (current-time 'time-monotonic) deadline)))))

  (define (report-unsupported! state feature)
    ;; Diagnostics are data. A head may present them after publication.
    (hashtable-set! (terminal-state-unsupported state) feature #t))

  (define (unsupported state)
    (sort string<? (vector->list (hashtable-keys (terminal-state-unsupported state)))))

  (define (color-scheme-report scheme)
    (format "\x1b;[?997;~an" (if (eq? scheme 'light) 2 1)))

  (define default-scheme (kernel:persistent-cell 'vt-default-scheme (lambda () #f)))

  (define (state-color-scheme state)
    (if (terminal-state-buffer state) (terminal-state-scheme state) (unbox default-scheme)))

  (define (control-signature family text final)
    ;; Keep parameters because private mode numbers identify the actual
    ;; capability, but bound diagnostics in case a malformed child sends a
    ;; large payload.
    (let* ([body (string-append text (if final (string final) ""))]
           [shown (if (> (string-length body) 80)
                      (string-append (substring body 0 77) "...") body)])
      (format "~a ~s" family shown)))

  (edoc "How many scrolled-off lines a terminal keeps."
        (value integer))
  (define terminal-scrollback (make-parameter 10000
                                (lambda (lines)
                                  (unless (and (integer? lines) (exact? lines) (>= lines 0))
                                    (error 'terminal-scrollback "must be a nonnegative integer" lines))
                                  lines)))

  (edoc "The shell a terminal runs without a command: SHELL, or /bin/sh."
        (value string))
  (define terminal-shell (make-parameter
                           (let ([shell (getenv "SHELL")])
                             (if (and shell (not (string=? shell ""))) shell "/bin/sh"))
                           (lambda (shell)
                             (unless (and (string? shell) (not (string=? shell "")))
                               (error 'terminal-shell "must be a nonempty string" shell))
                             shell)))

  (define (make-default-palette)
    (let* ([palette (make-vector 256 #f)]
           [base '((0 0 0) (205 0 0) (0 205 0) (205 205 0)
                   (0 0 238) (205 0 205) (0 205 205) (229 229 229)
                   (127 127 127) (255 0 0) (0 255 0) (255 255 0)
                   (92 92 255) (255 0 255) (0 255 255) (255 255 255))]
           [levels '#(0 95 135 175 215 255)])
      (do ([colors base (cdr colors)] [index 0 (+ index 1)])
          ((null? colors))
        (vector-set! palette index (car colors)))
      (do ([red 0 (+ red 1)]) ((= red 6))
        (do ([green 0 (+ green 1)]) ((= green 6))
          (do ([blue 0 (+ blue 1)]) ((= blue 6))
            (vector-set! palette
                         (+ 16 (* red 36) (* green 6) blue)
                         (list (vector-ref levels red)
                               (vector-ref levels green)
                               (vector-ref levels blue))))))
      (do ([index 232 (+ index 1)]) ((= index 256))
        (let ([level (+ 8 (* 10 (- index 232)))])
          (vector-set! palette index (list level level level))))
      palette))

  (define (blank-terminal-state owner buffer process rows cols live?)
    ;; The single place a terminal-state record is built, so the long
    ;; positional field list exists exactly once. Grouped as declared;
    ;; live terminals start dirty and alive, headless emulators idle.
    (make-terminal-state
      owner buffer process (make-mutex) (and live? (kernel:make-mailbox))
      (make-weak-eq-hashtable) (make-hashtable string-hash string=?)
      #f #f #f #f 0 #f 0 #f #f #f                 ; actor/publication state
      rows cols (make-screen rows cols) (make-vector rows #f) ; screen
      0 0                                        ; row col
      0 0 #f                                     ; saved cursor and state
      0 (- rows 1)                               ; scroll region
      0 (- cols 1) #f                            ; horizontal margins
      #f                                         ; memory lock
      'normal ""                                 ; parser, parameters
      #f (empty-control-text) '()                ; osc escape/text, replies
      'ascii 'ascii 0 0                          ; charsets, target, shift
      #f #t #f                                   ; wrap-pending autowrap origin
      #f #f #f                                   ; insert newline reverse
      #f #f #f #f                                ; cursor-keys keypad meta c1
      #t 'blinking-block (default-tab-stops cols) #\space ; cursor, tabs, last char
      live? live? #f                             ; dirty alive bell
      #f #f                                      ; bell visible, deadline
      #f #f #f #f #f                             ; mouse modes, focus
      #f                                         ; bracketed paste
      #f #f 0 0 #f                               ; saved main screen
      #f #f #f #f                                ; saved alternate screen
      (make-empty-scrollback)                    ; history
      (make-style-screen rows cols 'plain) #f    ; styles, main styles
      #f                                         ; captured frame
      "" 'plain #f #f                            ; sgr style link clipboard
      (make-vector 256 #f) #f #f                 ; palette, default colors
      #f "" (cons 0 '())                         ; printer
      '(8)                                       ; extra modes
      (make-vector rows 'single) #f #f))         ; line attributes

  (edoc "Whether a value is a terminal emulator."
        (value any "the value")
        (returns boolean))
  (define (terminal-emulator? value)
    (terminal-state? value))

  (edoc "A standalone terminal emulator of a size, for tests and tools."
        (rows integer "the rows")
        (cols integer "the columns")
        (returns (record terminal-state)))
  (define (make-terminal-emulator rows cols)
    (unless (and (integer? rows) (exact? rows) (> rows 0)
                 (integer? cols) (exact? cols) (> cols 0))
      (error 'make-terminal-emulator
             "rows and columns must be positive exact integers" rows cols))
    (blank-terminal-state #f #f #f rows cols #f))

  (edoc "Feed program output to an emulator."
        (emulator (record terminal-state) "the emulator")
        (text string "the output"))
  (define (terminal-emulator-feed! emulator text)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-feed! "expected a terminal emulator" emulator))
    (unless (string? text)
      (error 'terminal-emulator-feed! "expected a string" text))
    (with-mutex (terminal-state-lock emulator)
      (string-for-each (lambda (character) (feed-character! emulator character)) text)
      (unless (string=? text "") (terminal-state-dirty-set! emulator #t)))
    (void))

  (edoc "Resize an emulator's screen."
        (emulator (record terminal-state) "the emulator")
        (rows integer "the rows")
        (cols integer "the columns"))
  (define (terminal-emulator-resize! emulator rows cols)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-resize! "expected a terminal emulator" emulator))
    (unless (and (integer? rows) (exact? rows) (> rows 0)
                 (integer? cols) (exact? cols) (> cols 0))
      (error 'terminal-emulator-resize!
             "rows and columns must be positive exact integers" rows cols))
    (with-mutex (terminal-state-lock emulator) (resize-screen! emulator rows cols))
    (void))

  (define (displayed-screen-row emulator row)
    ;; The presented cells of one screen row, decorations expanded.
    (let-values ([(cells styles fresh?) (displayed-row emulator row)])
      cells))

  (define (displayed-style-row emulator row)
    (let-values ([(cells styles fresh?) (displayed-row emulator row)])
      styles))

  (define (screen-row-indexes emulator)
    (iota (terminal-state-rows emulator)))

  (define (read-emulator emulator who thunk)
    (unless (terminal-emulator? emulator) (error who "expected a terminal emulator" emulator))
    (with-mutex (terminal-state-lock emulator) (datum:copy (thunk))))

  (edoc "An emulator's screen as a vector of row strings."
        (emulator (record terminal-state) "the emulator")
        (returns vector))
  (define (terminal-emulator-screen emulator)
    (read-emulator emulator 'emulator-screen
      (lambda ()
        (list->vector
          (map (lambda (row) (cell-row->string (displayed-screen-row emulator row)))
               (screen-row-indexes emulator))))))

  (edoc "An emulator's screen styles, a vector of per-cell style rows."
        (emulator (record terminal-state) "the emulator")
        (returns vector))
  (define (terminal-emulator-styles emulator)
    (read-emulator emulator 'emulator-styles
      (lambda ()
        (list->vector
          (map (lambda (row) (effective-style-row emulator (displayed-style-row emulator row)))
               (screen-row-indexes emulator))))))

  (edoc "The hyperlinks on an emulator's screen, a vector of per-cell link rows."
        (emulator (record terminal-state) "the emulator")
        (returns vector))
  (define (terminal-emulator-hyperlinks emulator)
    (read-emulator emulator 'emulator-hyperlinks
      (lambda ()
        (list->vector
          (map (lambda (row) (vector-map cell-style-link (displayed-style-row emulator row)))
               (screen-row-indexes emulator))))))

  (edoc "An emulator's state as an alist: size, scrollback, wrapped rows, cursor, modes and more."
        (emulator (record terminal-state) "the emulator")
        (returns list))
  (define (terminal-emulator-state emulator)
    (read-emulator emulator 'emulator-state
      (lambda ()
        `((rows . ,(terminal-state-rows emulator))
          (columns . ,(terminal-state-cols emulator))
          (scrollback-lines . ,(scrollback-count (terminal-state-history emulator)))
          (wrapped-rows . ,(vector->list (terminal-state-wrapped emulator)))
          (cursor . ,(cons (terminal-state-row emulator)
                       (terminal-state-col emulator)))
          (scroll-region . ,(cons (terminal-state-scroll-top emulator)
                              (terminal-state-scroll-bottom emulator)))
          (horizontal-margins . ,(cons (terminal-state-left-margin emulator)
                                   (terminal-state-right-margin emulator)))
          (horizontal-margin-mode . ,(terminal-state-margin-mode emulator))
          (memory-lock . ,(terminal-state-memory-lock emulator))
          (printer-controller . ,(terminal-state-printer-controller emulator))
          (printer-output . ,(printer-output-text emulator))
          (wrap-pending . ,(terminal-state-wrap-pending emulator))
          (autowrap . ,(terminal-state-autowrap emulator))
          (origin . ,(terminal-state-origin emulator))
          (insert . ,(terminal-state-insert emulator))
          (newline . ,(terminal-state-newline emulator))
          (reverse-screen . ,(terminal-state-reverse-screen emulator))
          (bell-pending . ,(terminal-state-bell emulator))
          (bell-visible . ,(terminal-state-bell-visible emulator))
          (cursor-visible . ,(terminal-state-cursor-visible emulator))
          (cursor-style . ,(terminal-state-cursor-shape emulator))
          (application-cursor-keys . ,(terminal-state-cursor-keys emulator))
          (application-keypad . ,(terminal-state-keypad emulator))
          (eight-bit-meta . ,(terminal-state-meta-eight-bit emulator))
          (eight-bit-controls . ,(terminal-state-controls-eight-bit emulator))
          (clipboard . ,(terminal-state-clipboard emulator))
          (mouse-tracking . ,(terminal-state-mouse emulator))
          (sgr-mouse . ,(terminal-state-mouse-sgr emulator))
          (utf8-mouse . ,(terminal-state-mouse-utf8 emulator))
          (urxvt-mouse . ,(terminal-state-mouse-urxvt emulator))
          (focus-reporting . ,(terminal-state-focus-reporting emulator))
          (bracketed-paste . ,(terminal-state-bracketed emulator))
          (reverse-wraparound . ,(and (memv 45 (terminal-state-extra-modes
                                                 emulator))
                                   #t))
          (default-colors . ,(cons (terminal-state-default-foreground emulator)
                               (terminal-state-default-background emulator)))))))

  (edoc "The bytes a key event sends to the program, under the emulator's input modes."
        (emulator (record terminal-state) "the emulator")
        (event string "the key event")
        (returns string))
  (define (terminal-emulator-input emulator event)
    (unless (string? event)
      (error 'terminal-emulator-input "expected an event string" event))
    (read-emulator emulator 'emulator-input (lambda () (event-bytes emulator event))))

  (edoc "The replies the emulator owes the program, oldest first."
        (emulator (record terminal-state) "the emulator")
        (returns list))
  (define (terminal-emulator-replies emulator)
    (read-emulator emulator 'emulator-replies (lambda () (reverse (terminal-state-replies emulator)))))

  (edoc "The control sequences the emulator saw and does not implement."
        (emulator (record terminal-state) "the emulator")
        (returns list))
  (define (terminal-emulator-unsupported emulator)
    (read-emulator emulator 'emulator-unsupported
      (lambda () (unsupported emulator))))

  (define (state-cursor-position state)
    ;; A decorated row shows each character two cells wide, so the
    ;; presented cursor column doubles there.
    (let* ([row (terminal-state-row state)]
           [col (min (- (terminal-state-cols state) 1)
                     (terminal-state-col state))]
           [col (if (eq? (vector-ref (terminal-state-line-attributes state)
                                     row)
                         'single)
                    col
                    (min (- (terminal-state-cols state) 1) (* 2 col)))])
      (cons (+ (if (terminal-state-main-screen state)
                   0 (scrollback-count (terminal-state-history state)))
               row)
            col)))

  (define (blank-line cols) (make-vector cols " "))

  (define (row-columns state row)
    ;; The addressable width of a row: DECDWL and DECDHL rows hold half as
    ;; many characters, each displayed two cells wide.
    (if (eq? (vector-ref (terminal-state-line-attributes state) row) 'single)
        (terminal-state-cols state)
        (div (terminal-state-cols state) 2)))

  (define (clamp-to-row-columns! state)
    (let ([limit (row-columns state (terminal-state-row state))])
      (when (>= (terminal-state-col state) limit)
        (terminal-state-col-set! state (max 0 (- limit 1))))))

  (define (double-width-pair cell)
    ;; The two display cells for one character of a double-width row.
    ;; ASCII has fullwidth forms; wide clusters are already two cells; the
    ;; rest pad with a following space.
    (let ([code (and (= (string-length cell) 1)
                     (char->integer (string-ref cell 0)))])
      (cond
        [(eqv? code 32) (values "\x3000;" "")]
        [(and code (<= #x21 code #x7e))
         (values (string (integer->char (+ code #xfee0))) "")]
        [(>= (glyph:width cell) 2) (values cell "")]
        [else (values cell " ")])))

  (define (expanded-wide-row cells styles)
    ;; The displayed form of a decorated row: the left half of the buffer,
    ;; one character per two cells. Double-height halves render the same
    ;; way; a cell grid cannot stretch glyphs vertically.
    (let* ([cols (vector-length cells)]
           [new-cells (make-vector cols " ")]
           [new-styles (make-vector cols 'plain)])
      (do ([i 0 (+ i 1)]) ((= i (div cols 2)))
        (let ([cell (vector-ref cells i)] [style (vector-ref styles i)])
          (let-values ([(first second)
                        (if (string=? cell "")
                            ;; The wide glyph already occupies the first
                            ;; pair. Its source continuation needs two cells
                            ;; of padding; more empty continuations would
                            ;; claim a four-cell glyph the host cannot draw.
                            (values " " " ")
                            (double-width-pair cell))])
            (vector-set! new-cells (* 2 i) first)
            (vector-set! new-cells (+ (* 2 i) 1) second)
            (vector-set! new-styles (* 2 i) style)
            (vector-set! new-styles (+ (* 2 i) 1) style))))
      (values new-cells new-styles)))

  (define (displayed-row state row)
    ;; (values cells styles fresh?) for one screen row as presented;
    ;; fresh? tells the caller whether the vectors are private copies.
    (let ([cells (vector-ref (terminal-state-screen state) row)]
          [styles (vector-ref (terminal-state-styles state) row)])
      (if (eq? (vector-ref (terminal-state-line-attributes state) row)
               'single)
          (values cells styles #f)
          (let-values ([(wide-cells wide-styles)
                        (expanded-wide-row cells styles)])
            (values wide-cells wide-styles #t)))))

  (define (left-bound state)
    (if (terminal-state-margin-mode state)
        (terminal-state-left-margin state) 0))

  (define (right-bound state)
    (if (terminal-state-margin-mode state)
        (terminal-state-right-margin state)
        (- (terminal-state-cols state) 1)))

  (define (erase-row-range! state row start end)
    (let ([line (vector-ref (terminal-state-screen state) row)]
          [styles (vector-ref (terminal-state-styles state) row)])
      (do ([col start (+ col 1)]) ((= col end))
        (clear-cell! line styles col (terminal-state-style state))
        (vector-set! styles col (terminal-state-style state)))))

  (define (scrolling-top state)
    (let ([top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [lock (terminal-state-memory-lock state)])
      (if (and lock (<= lock bottom)) (max top lock) top)))
  (define (blank-styles cols style) (make-vector cols style))

  (define (cell-row->string row)
    (apply string-append (vector->list row)))

  (define (default-tab-stops cols)
    (let ([stops (make-vector cols #f)])
      (do ([col 8 (+ col 8)]) ((>= col cols) stops)
        (vector-set! stops col #t))))

  (define (copy-vector-range! source source-start target target-start count)
    (if (<= target-start source-start)
        (do ([index 0 (+ index 1)]) ((= index count))
          (vector-set! target (+ target-start index)
                       (vector-ref source (+ source-start index))))
        (do ([index (- count 1) (- index 1)]) ((< index 0))
          (vector-set! target (+ target-start index)
                       (vector-ref source (+ source-start index))))))

  (define (make-screen rows cols)
    (let ([screen (make-vector rows)])
      (do ([row 0 (+ row 1)]) ((= row rows) screen)
        (vector-set! screen row (blank-line cols)))))

  (define (make-style-screen rows cols style)
    (let ([screen (make-vector rows)])
      (do ([row 0 (+ row 1)]) ((= row rows) screen)
        (vector-set! screen row (blank-styles cols style)))))

  (define (resized-flags old rows fill)
    (let ([new (make-vector rows fill)])
      (do ([index 0 (+ index 1)]) ((= index (min rows (vector-length old))) new)
        (vector-set! new index (vector-ref old index)))))

  (define (resized-screen old old-rows old-cols rows cols)
    (let ([new (make-screen rows cols)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (copy-vector-range! (vector-ref old row) 0
                            (vector-ref new row) 0 copy-cols))))

  (define (resized-styles old old-rows old-cols rows cols style)
    (let ([new (make-style-screen rows cols style)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (copy-vector-range! (vector-ref old row) 0
                            (vector-ref new row) 0 copy-cols))))

  (define (resized-tab-stops old old-cols cols)
    (let ([new (default-tab-stops cols)])
      (do ([col 0 (+ col 1)]) ((= col (min old-cols cols)) new)
        (vector-set! new col (vector-ref old col)))))

  (define (clamp value low high) (min high (max low value)))

  (define (save-cursor! state)
    (terminal-state-saved-row-set! state (terminal-state-row state))
    (terminal-state-saved-col-set! state (terminal-state-col state))
    (terminal-state-saved-state-set!
      state
      (list (terminal-state-sgr state)
            (terminal-state-style state)
            (terminal-state-charset state)
            (terminal-state-charset-g1 state)
            (terminal-state-shift state)
            (terminal-state-origin state)
            (terminal-state-autowrap state)
            (terminal-state-wrap-pending state))))

  (define (restore-cursor! state)
    (terminal-state-row-set!
      state (clamp (terminal-state-saved-row state)
                   0 (- (terminal-state-rows state) 1)))
    (terminal-state-col-set!
      state (clamp (terminal-state-saved-col state)
                   0 (- (terminal-state-cols state) 1)))
    (when (terminal-state-saved-state state)
      (let ([saved (terminal-state-saved-state state)])
        (terminal-state-sgr-set! state (list-ref saved 0))
        (terminal-state-style-set! state (list-ref saved 1))
        (terminal-state-charset-set! state (list-ref saved 2))
        (terminal-state-charset-g1-set! state (list-ref saved 3))
        (terminal-state-shift-set! state (list-ref saved 4))
        (terminal-state-origin-set! state (list-ref saved 5))
        (terminal-state-autowrap-set! state (list-ref saved 6))
        (terminal-state-wrap-pending-set! state (list-ref saved 7)))))

  (define (capture-screen-state state)
    (list (terminal-state-row state)
          (terminal-state-col state)
          (terminal-state-saved-row state)
          (terminal-state-saved-col state)
          (terminal-state-saved-state state)
          (terminal-state-scroll-top state)
          (terminal-state-scroll-bottom state)
          (terminal-state-charset state)
          (terminal-state-charset-g1 state)
          (terminal-state-charset-target state)
          (terminal-state-shift state)
          (terminal-state-wrap-pending state)
          (terminal-state-autowrap state)
          (terminal-state-origin state)
          (terminal-state-insert state)
          (terminal-state-sgr state)
          (terminal-state-style state)
          (vector-copy (terminal-state-tab-stops state))
          (terminal-state-last-character state)
          (terminal-state-left-margin state)
          (terminal-state-right-margin state)
          (terminal-state-margin-mode state)
          (terminal-state-memory-lock state)))

  (define (restore-screen-state! state saved)
    (terminal-state-row-set!
      state (clamp (list-ref saved 0) 0 (- (terminal-state-rows state) 1)))
    (terminal-state-col-set!
      state (clamp (list-ref saved 1) 0 (- (terminal-state-cols state) 1)))
    (terminal-state-saved-row-set! state (list-ref saved 2))
    (terminal-state-saved-col-set! state (list-ref saved 3))
    (terminal-state-saved-state-set! state (list-ref saved 4))
    (let* ([rows (terminal-state-rows state)]
           [top (clamp (list-ref saved 5) 0 (- rows 1))]
           [bottom (clamp (list-ref saved 6) 0 (- rows 1))])
      (terminal-state-scroll-top-set! state (if (< top bottom) top 0))
      (terminal-state-scroll-bottom-set!
        state (if (< top bottom) bottom (- rows 1))))
    (terminal-state-charset-set! state (list-ref saved 7))
    (terminal-state-charset-g1-set! state (list-ref saved 8))
    (terminal-state-charset-target-set! state (list-ref saved 9))
    (terminal-state-shift-set! state (list-ref saved 10))
    (terminal-state-wrap-pending-set! state (list-ref saved 11))
    (terminal-state-autowrap-set! state (list-ref saved 12))
    (terminal-state-origin-set! state (list-ref saved 13))
    (terminal-state-insert-set! state (list-ref saved 14))
    (terminal-state-sgr-set! state (list-ref saved 15))
    (terminal-state-style-set! state (list-ref saved 16))
    (let ([tabs (list-ref saved 17)])
      (terminal-state-tab-stops-set!
        state (resized-tab-stops tabs (vector-length tabs)
                                 (terminal-state-cols state))))
    (terminal-state-last-character-set! state (list-ref saved 18))
    (let* ([cols (terminal-state-cols state)]
           [left (clamp (list-ref saved 19) 0 (- cols 1))]
           [right (clamp (list-ref saved 20) 0 (- cols 1))])
      (terminal-state-left-margin-set! state (if (< left right) left 0))
      (terminal-state-right-margin-set!
        state (if (< left right) right (- cols 1))))
    (terminal-state-margin-mode-set! state (list-ref saved 21))
    (let ([lock (list-ref saved 22)])
      (terminal-state-memory-lock-set!
        state (and lock (clamp lock 0 (- (terminal-state-rows state) 1))))))

  (define (enter-alternate-screen! state mode)
    ;; Modes 47 and 1047 swap screen contents only: the cursor, margins,
    ;; and rendition carry across, as in xterm. Mode 1049 additionally
    ;; saves the primary screen's state and starts from a cleared page.
    (unless (terminal-state-main-screen state)
      (terminal-state-main-screen-set! state (terminal-state-screen state))
      (terminal-state-main-wrapped-set! state (terminal-state-wrapped state))
      (terminal-state-main-styles-set! state (terminal-state-styles state))
      (terminal-state-main-line-attributes-set!
        state (terminal-state-line-attributes state))
      (terminal-state-main-row-set! state (terminal-state-row state))
      (terminal-state-main-col-set! state (terminal-state-col state))
      (terminal-state-main-state-set! state (capture-screen-state state))
      (if (and (not (= mode 1049)) (terminal-state-alternate-screen state))
          (begin
            (terminal-state-screen-set!
              state (terminal-state-alternate-screen state))
            (terminal-state-wrapped-set!
              state (terminal-state-alternate-wrapped state))
            (terminal-state-styles-set!
              state (terminal-state-alternate-styles state))
            (terminal-state-line-attributes-set!
              state (terminal-state-alternate-line-attributes state)))
          (begin
            (terminal-state-screen-set!
              state (make-screen (terminal-state-rows state)
                                 (terminal-state-cols state)))
            (terminal-state-wrapped-set!
              state (make-vector (terminal-state-rows state) #f))
            (terminal-state-styles-set!
              state (make-style-screen (terminal-state-rows state)
                                       (terminal-state-cols state)
                                       (terminal-state-style state)))
            (terminal-state-line-attributes-set!
              state (make-vector (terminal-state-rows state) 'single))
            (when (= mode 1049)
              (terminal-state-row-set! state 0)
              (terminal-state-col-set! state 0)
              (terminal-state-scroll-top-set! state 0)
              (terminal-state-scroll-bottom-set!
                state (- (terminal-state-rows state) 1))
              (terminal-state-origin-set! state #f)
              (terminal-state-wrap-pending-set! state #f))))))

  (define (leave-alternate-screen! state mode)
    (when (terminal-state-main-screen state)
      (if (= mode 1047)
          ;; Mode 1047 clears the alternate screen as it is left.
          (begin
            (terminal-state-alternate-screen-set! state #f)
            (terminal-state-alternate-wrapped-set! state #f)
            (terminal-state-alternate-styles-set! state #f)
            (terminal-state-alternate-line-attributes-set! state #f)
            (terminal-state-alternate-state-set! state #f))
          (begin
            (terminal-state-alternate-screen-set!
              state (terminal-state-screen state))
            (terminal-state-alternate-wrapped-set!
              state (terminal-state-wrapped state))
            (terminal-state-alternate-styles-set!
              state (terminal-state-styles state))
            (terminal-state-alternate-line-attributes-set!
              state (terminal-state-line-attributes state))
            (terminal-state-alternate-state-set!
              state (capture-screen-state state))))
      (terminal-state-screen-set! state (terminal-state-main-screen state))
      (terminal-state-wrapped-set! state (terminal-state-main-wrapped state))
      (terminal-state-styles-set! state (terminal-state-main-styles state))
      (terminal-state-line-attributes-set!
        state (or (terminal-state-main-line-attributes state)
                  (make-vector (terminal-state-rows state) 'single)))
      (when (and (= mode 1049) (terminal-state-main-state state))
        (restore-screen-state! state (terminal-state-main-state state)))
      (terminal-state-main-screen-set! state #f)
      (terminal-state-main-wrapped-set! state #f)
      (terminal-state-main-styles-set! state #f)
      (terminal-state-main-line-attributes-set! state #f)
      (terminal-state-main-state-set! state #f)))

  (define (meaningful-row-length cells styles wrapped? cursor-column)
    (if wrapped? (vector-length cells)
        (let find ([index (- (vector-length cells) 1)])
          (cond [(< index 0) (or cursor-column 0)]
                [(or (not (string=? (vector-ref cells index) " "))
                     (not (eq? (vector-ref styles index) 'plain)))
                 (max (+ index 1) (or cursor-column 0))]
                [else (find (- index 1))]))))

  (define (terminal-logical-lines cells styles wrapped cursor-row cursor-col)
    (let loop ([row 0] [start 0] [cell-parts '()] [style-parts '()]
               [cell-count 0] [logical-cursor #f] [out '()])
      (let* ([line (vector-ref cells row)]
             [faces (vector-ref styles row)]
             [continues? (vector-ref wrapped row)]
             [wide-wrap-padding?
              (and continues? (< (+ row 1) (vector-length cells))
                   (> (vector-length line) 1)
                   (string=? (vector-ref line (- (vector-length line) 1)) " ")
                   (> (vector-length (vector-ref cells (+ row 1))) 1)
                   (string=? (vector-ref (vector-ref cells (+ row 1)) 1) ""))]
             [row-length (if wide-wrap-padding? (- (vector-length line) 1)
                             (meaningful-row-length
                               line faces continues?
                               (and (= row cursor-row) cursor-col)))]
             [new-cells (append
                          (reverse
                            (vector->list (vector-copy line 0 row-length)))
                          cell-parts)]
             [new-styles (append
                           (reverse
                             (vector->list (vector-copy faces 0 row-length)))
                           style-parts)]
             [logical-cursor
              (if (= row cursor-row)
                  (+ cell-count (min cursor-col row-length))
                  logical-cursor)])
        (if (and continues? (< (+ row 1) (vector-length cells)))
            (loop (+ row 1) start new-cells new-styles
                  (+ cell-count row-length) logical-cursor out)
            (let ([entry (list (list->vector (reverse new-cells))
                               (list->vector (reverse new-styles)) start row
                               logical-cursor)])
              (if (= (+ row 1) (vector-length cells))
                  (reverse (cons entry out))
                  (loop (+ row 1) (+ row 1) '() '() 0 #f
                        (cons entry out))))))))

  (define (list-take items count)
    (if (= count 0) '()
        (cons (car items) (list-take (cdr items) (- count 1)))))

  (define (reflow-logical-lines logical cols)
    (let ([rows '()] [faces '()] [flags '()] [new-cursor #f]
          [new-wrap-pending #f] [output-count 0])
      (for-each
        (lambda (entry)
          (let* ([cells (car entry)] [styles (cadr entry)]
                 [cursor-offset (list-ref entry 4)]
                 [cursor? (not (eq? cursor-offset #f))]
                 [cell-count (vector-length cells)])
            (let chunk ([at 0])
              (let* ([remaining (- cell-count at)]
                     [take0 (min cols remaining)]
                     [take (if (and (> take0 1) (< (+ at take0) cell-count)
                                    (string=? (vector-ref cells (+ at take0)) ""))
                               (- take0 1) take0)]
                     [take (if (= take 0) (min 1 remaining) take)]
                     [line (blank-line cols)]
                     [style-row (blank-styles cols 'plain)]
                     [output-row output-count])
                (when (> take 0)
                  (copy-vector-range! cells at line 0 take)
                  (copy-vector-range! styles at style-row 0 take))
                (when (and cursor? (not new-cursor)
                           (or (< cursor-offset (+ at take))
                               (= (+ at take) cell-count)))
                  (set! new-cursor
                    (cons output-row (min (- cols 1)
                                          (max 0 (- cursor-offset at)))))
                  (set! new-wrap-pending
                    (and (= cursor-offset cell-count) (= take cols))))
                (set! rows (cons line rows))
                (set! faces (cons style-row faces))
                (set! flags (cons (< (+ at take) cell-count) flags))
                (set! output-count (+ output-count 1))
                (when (< (+ at take) cell-count)
                  (chunk (+ at take)))))))
        logical)
      (values (reverse rows) (reverse faces) (reverse flags)
              (or new-cursor '(0 . 0)) new-wrap-pending)))

  (define (reflow-primary-screen! state rows cols)
    (let* ([entries (scrollback-entries (terminal-state-history state))]
           [history-count (scrollback-count (terminal-state-history state))]
           [all-cells (list->vector
                        (append (map scrollback-line-cells entries)
                                (vector->list (terminal-state-screen state))))]
           [all-styles (list->vector
                         (append (map scrollback-line-styles entries)
                                 (vector->list (terminal-state-styles state))))]
           [all-wrapped (list->vector
                          (append (map scrollback-line-wrapped entries)
                                  (vector->list (terminal-state-wrapped state))))]
           [cursor-row (+ history-count (terminal-state-row state))]
           [cursor-col (+ (terminal-state-col state)
                          (if (terminal-state-wrap-pending state) 1 0))]
           [used-rows
            (let find ([row (- (vector-length all-cells) 1)])
              (if (<= row cursor-row) (+ cursor-row 1)
                  (if (> (meaningful-row-length
                           (vector-ref all-cells row)
                           (vector-ref all-styles row) #f #f)
                         0)
                      (+ row 1)
                      (find (- row 1)))))]
           [all-cells (vector-copy all-cells 0 used-rows)]
           [all-styles (vector-copy all-styles 0 used-rows)]
           [all-wrapped (vector-copy all-wrapped 0 used-rows)])
      (let-values ([(new-cells new-styles new-wrapped cursor wrap-pending?)
                    (reflow-logical-lines
                      (terminal-logical-lines all-cells all-styles all-wrapped
                                              cursor-row
                                              cursor-col)
                      cols)])
        (let* ([missing (max 0 (- rows (length new-cells)))]
               [new-cells (append new-cells
                                  (map (lambda (ignored) (blank-line cols))
                                       (iota missing)))]
               [new-styles (append new-styles
                                   (map (lambda (ignored)
                                          (blank-styles cols 'plain))
                                        (iota missing)))]
               [new-wrapped (append new-wrapped (make-list missing #f))]
               [history-count (max 0 (- (length new-cells) rows))]
               [history-count (min history-count (terminal-scrollback))]
               [display-start (- (length new-cells) rows)])
          (terminal-state-history-set!
            state
            (scrollback-of-entries
              (let ([keep (lambda (lines)
                            (list-tail (list-take lines display-start)
                                       (max 0 (- display-start
                                                 history-count))))])
                (map make-scrollback-line
                     (keep new-cells) (keep new-styles)
                     (keep new-wrapped)))))
          (terminal-state-screen-set!
            state (list->vector (list-tail new-cells display-start)))
          (terminal-state-styles-set!
            state (list->vector (list-tail new-styles display-start)))
          (terminal-state-wrapped-set!
            state (list->vector (list-tail new-wrapped display-start)))
          (terminal-state-row-set!
            state (clamp (- (car cursor) display-start) 0 (- rows 1)))
          (terminal-state-col-set! state (cdr cursor))
          ;; Reflow rebuilds rows from logical lines; DECDWL decorations
          ;; do not survive it, as full-screen programs redraw on resize.
          (terminal-state-line-attributes-set!
            state (make-vector rows 'single))
          (terminal-state-wrap-pending-set! state wrap-pending?)))))

  (define (reflow-saved-primary! state rows cols)
    (let ([alternate-screen (terminal-state-screen state)]
          [alternate-wrapped (terminal-state-wrapped state)]
          [alternate-styles (terminal-state-styles state)]
          [alternate-attributes (terminal-state-line-attributes state)]
          [alternate-state (capture-screen-state state)])
      (terminal-state-screen-set! state (terminal-state-main-screen state))
      (terminal-state-wrapped-set! state (terminal-state-main-wrapped state))
      (terminal-state-styles-set! state (terminal-state-main-styles state))
      (terminal-state-line-attributes-set!
        state (or (terminal-state-main-line-attributes state)
                  (make-vector (terminal-state-rows state) 'single)))
      (restore-screen-state! state (terminal-state-main-state state))
      (reflow-primary-screen! state rows cols)
      (terminal-state-main-screen-set! state (terminal-state-screen state))
      (terminal-state-main-wrapped-set! state (terminal-state-wrapped state))
      (terminal-state-main-styles-set! state (terminal-state-styles state))
      (terminal-state-main-line-attributes-set!
        state (terminal-state-line-attributes state))
      (terminal-state-main-row-set! state (terminal-state-row state))
      (terminal-state-main-col-set! state (terminal-state-col state))
      (terminal-state-main-state-set! state (capture-screen-state state))
      (terminal-state-screen-set! state alternate-screen)
      (terminal-state-wrapped-set! state alternate-wrapped)
      (terminal-state-styles-set! state alternate-styles)
      (terminal-state-line-attributes-set! state alternate-attributes)
      (restore-screen-state! state alternate-state)))

  (define (resize-stashed-alternate! state rows cols)
    ;; The inactive alternate-screen stash was saved at whatever size
    ;; the terminal had then; resize it by its own dimensions -- the
    ;; live screen's may differ, and a resize between sessions used to
    ;; leave it stale entirely.
    (let ([alt (terminal-state-alternate-screen state)])
      (when alt
        (let* ([alt-rows (vector-length alt)]
               [alt-cols (if (> alt-rows 0)
                             (vector-length (vector-ref alt 0))
                             cols)])
          (terminal-state-alternate-screen-set!
            state (resized-screen alt alt-rows alt-cols rows cols))
          (when (terminal-state-alternate-styles state)
            (terminal-state-alternate-styles-set!
              state (resized-styles (terminal-state-alternate-styles state)
                                    alt-rows alt-cols rows cols
                                    (terminal-state-style state))))
          (when (terminal-state-alternate-wrapped state)
            (terminal-state-alternate-wrapped-set!
              state (resized-flags (terminal-state-alternate-wrapped state)
                                   rows #f)))
          (when (terminal-state-alternate-line-attributes state)
            (terminal-state-alternate-line-attributes-set!
              state (resized-flags
                      (terminal-state-alternate-line-attributes state)
                      rows 'single)))))))

  (define (resize-screen! state rows cols)
    (let ([changed? (or (not (= rows (terminal-state-rows state)))
                        (not (= cols (terminal-state-cols state))))])
      (when changed?
        (if (not (terminal-state-main-screen state))
          (let ([old-cols (terminal-state-cols state)])
            (reflow-primary-screen! state rows cols)
            (resize-stashed-alternate! state rows cols)
            (terminal-state-tab-stops-set!
              state
              (resized-tab-stops (terminal-state-tab-stops state)
                                 old-cols cols))
            (terminal-state-rows-set! state rows)
            (terminal-state-cols-set! state cols))
          (let* ([old-rows (terminal-state-rows state)]
                 [old-cols (terminal-state-cols state)]
                 [new (resized-screen (terminal-state-screen state)
                                      old-rows old-cols rows cols)]
                 [new-styles (resized-styles (terminal-state-styles state)
                                             old-rows old-cols rows cols
                                             (terminal-state-style state))])
            (reflow-saved-primary! state rows cols)
            (resize-stashed-alternate! state rows cols)
            (terminal-state-screen-set! state new)
            (terminal-state-wrapped-set!
              state (resized-flags (terminal-state-wrapped state) rows #f))
            (terminal-state-line-attributes-set!
              state (resized-flags (terminal-state-line-attributes state)
                                   rows 'single))
            (terminal-state-styles-set! state new-styles)
            (terminal-state-tab-stops-set!
              state
              (resized-tab-stops (terminal-state-tab-stops state) old-cols cols))
            (terminal-state-rows-set! state rows)
            (terminal-state-cols-set! state cols)
            (terminal-state-row-set! state
                                     (clamp (terminal-state-row state) 0 (- rows 1)))
            (terminal-state-col-set! state
                                     (clamp (terminal-state-col state) 0 (- cols 1)))
            (terminal-state-wrap-pending-set! state #f)
            (terminal-state-scroll-top-set! state 0)
            (terminal-state-scroll-bottom-set! state (- rows 1))
            (terminal-state-dirty-set! state #t)))
        (terminal-state-scroll-top-set! state 0)
        (terminal-state-scroll-bottom-set! state (- rows 1))
        (terminal-state-left-margin-set! state 0)
        (terminal-state-right-margin-set! state (- cols 1))
        (terminal-state-margin-mode-set! state #f)
        (terminal-state-memory-lock-set! state #f)
        (terminal-state-dirty-set! state #t)
        (when (terminal-state-process state)
          (sys:resize-terminal-process!
            (terminal-state-process state) rows cols)))))

  (define (scroll-up! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [wrapped (terminal-state-wrapped state)]
          [attributes (terminal-state-line-attributes state)]
          [top (scrolling-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)]
          [left (left-bound state)]
          [right (right-bound state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (when (and (= left 0) (= right (- cols 1))
                   (= top 0) (= bottom (- (terminal-state-rows state) 1))
                   (not (terminal-state-main-screen state))
                   (> (terminal-scrollback) 0))
          ;; Decorated rows enter the transcript in their displayed form.
          (let-values ([(cells faces)
                        (if (eq? (vector-ref attributes top) 'single)
                            (values (vector-copy (vector-ref screen top))
                                    (vector-copy (vector-ref styles top)))
                            (expanded-wide-row (vector-ref screen top)
                                               (vector-ref styles top)))])
            (scrollback-push!
              (terminal-state-history state)
              (make-scrollback-line cells faces (vector-ref wrapped top))
              (terminal-scrollback))))
        (do ([row top (+ row 1)]) ((= row bottom))
          (if (and (= left 0) (= right (- cols 1)))
              (begin
                (vector-set! screen row (vector-ref screen (+ row 1)))
                (vector-set! styles row (vector-ref styles (+ row 1)))
                (vector-set! wrapped row (vector-ref wrapped (+ row 1)))
                (vector-set! attributes row
                             (vector-ref attributes (+ row 1))))
              (begin
                (copy-vector-range! (vector-ref screen (+ row 1)) left
                                    (vector-ref screen row) left
                                    (+ 1 (- right left)))
                (copy-vector-range! (vector-ref styles (+ row 1)) left
                                    (vector-ref styles row) left
                                    (+ 1 (- right left)))
                (vector-set! wrapped row #f))))
        (if (and (= left 0) (= right (- cols 1)))
            (begin
              (vector-set! screen bottom (blank-line cols))
              (vector-set! styles bottom
                           (blank-styles cols (terminal-state-style state)))
              (vector-set! attributes bottom 'single))
            (erase-row-range! state bottom left (+ right 1)))
        (vector-set! wrapped bottom #f))))

  (define (scroll-down! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [wrapped (terminal-state-wrapped state)]
          [attributes (terminal-state-line-attributes state)]
          [top (scrolling-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)]
          [left (left-bound state)]
          [right (right-bound state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (do ([row bottom (- row 1)]) ((= row top))
          (if (and (= left 0) (= right (- cols 1)))
              (begin
                (vector-set! attributes row
                             (vector-ref attributes (- row 1)))
                (vector-set! screen row (vector-ref screen (- row 1)))
                (vector-set! styles row (vector-ref styles (- row 1)))
                (vector-set! wrapped row (vector-ref wrapped (- row 1))))
              (begin
                (copy-vector-range! (vector-ref screen (- row 1)) left
                                    (vector-ref screen row) left
                                    (+ 1 (- right left)))
                (copy-vector-range! (vector-ref styles (- row 1)) left
                                    (vector-ref styles row) left
                                    (+ 1 (- right left)))
                (vector-set! wrapped row #f))))
        (if (and (= left 0) (= right (- cols 1)))
            (begin
              (vector-set! screen top (blank-line cols))
              (vector-set! styles top
                           (blank-styles cols (terminal-state-style state)))
              (vector-set! attributes top 'single))
            (erase-row-range! state top left (+ right 1)))
        (vector-set! wrapped top #f))))

  (define (line-feed! state)
    (if (= (terminal-state-row state) (terminal-state-scroll-bottom state))
        (scroll-up! state 1)
        (terminal-state-row-set!
          state (min (- (terminal-state-rows state) 1)
                     (+ (terminal-state-row state) 1))))
    (clamp-to-row-columns! state))

  (define (cell-owner-index line col)
    (let find ([index col])
      (cond [(< index 0) #f]
            [(string=? (vector-ref line index) "") (find (- index 1))]
            [else index])))

  (define (regional-indicator-count text)
    (let loop ([characters (string->list text)] [count 0])
      (if (null? characters) count
          (loop (cdr characters)
                (+ count
                   (if (eq? (char-grapheme-break-property (car characters))
                            'Regional_Indicator)
                       1 0))))))

  (define (put-character! state character)
    ;; VT autowrap is delayed until the next printable character. Cursor
    ;; motion and controls can therefore cancel a pending wrap at the margin.
    (define (previous-cluster line col)
      (let ([index (cell-owner-index line (- col 1))])
        (and index (vector-ref line index))))
    (define (cluster-extension? character line col)
      (let ([previous (previous-cluster line col)])
        (glyph:extends?
          (and previous (> (string-length previous) 0)
               (string-ref previous (- (string-length previous) 1)))
          character
          (if (and previous (eq? (char-grapheme-break-property character) 'Regional_Indicator))
              (regional-indicator-count previous) 0))))
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [width (sys:terminal-character-width character)])
      (when (or (= width 0)
                (cluster-extension? character line
                                    (terminal-state-col state)))
        (let* ([candidate (if (terminal-state-wrap-pending state)
                              (terminal-state-col state)
                              (- (terminal-state-col state) 1))]
               [col (cell-owner-index line candidate)])
          (if col
              (let* ([old (vector-ref line col)]
                     [updated (string-normalize-nfc
                                (string-append old (string character)))]
                     [old-width (max 1 (glyph:width old))]
                     [new-width (min (terminal-state-cols state)
                                     (max 1 (glyph:width updated)))]
                     [extra (- new-width old-width)])
                (vector-set! line col updated)
                (when (and (> extra 0)
                           (<= (+ (terminal-state-col state) extra)
                               (terminal-state-cols state)))
                  (let ([styles (vector-ref (terminal-state-styles state)
                                            (terminal-state-row state))])
                    (do ([index (terminal-state-col state) (+ index 1)])
                        ((= index (+ (terminal-state-col state) extra)))
                      (clear-cell! line styles index (vector-ref styles col))
                      (vector-set! line index "")
                      (vector-set! styles index (vector-ref styles col)))
                    (terminal-state-col-set!
                      state (+ (terminal-state-col state) extra)))))
              (begin
                (vector-set! line (terminal-state-col state)
                             (string-normalize-nfc
                               (string-append " " (string character))))
                (terminal-state-col-set!
                  state (min (- (terminal-state-cols state) 1)
                             (+ (terminal-state-col state) 1))))))
        (set! width 0))
      (when (> width 0)
        (put-spacing-character! state character width))))

  (define (clear-cell! line styles col style)
    (let find ([start col])
      (if (and (> start 0) (string=? (vector-ref line start) ""))
          (find (- start 1))
          (begin
            (vector-set! line start " ")
            (vector-set! styles start style)
            (let loop ([index (+ start 1)])
              (when (and (< index (vector-length line))
                         (string=? (vector-ref line index) ""))
                (vector-set! line index " ")
                (vector-set! styles index style)
                (loop (+ index 1))))))))

  (define (put-spacing-character! state character requested-width)
    (let ([left (left-bound state)] [right (right-bound state)])
      (when (terminal-state-wrap-pending state)
        (terminal-state-wrap-pending-set! state #f)
        (when (terminal-state-autowrap state)
          (vector-set! (terminal-state-wrapped state)
                       (terminal-state-row state) #t)
          (terminal-state-col-set! state left)
          (line-feed! state)))
      (clamp-to-row-columns! state)
      (let* ([cols (terminal-state-cols state)]
             [limit (min (if (<= left (terminal-state-col state) right)
                             (+ right 1) cols)
                         (row-columns state (terminal-state-row state)))]
             [width (min requested-width (- limit left))])
        (when (> width (- limit (terminal-state-col state)))
          (if (terminal-state-autowrap state)
              (begin
                (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #t)
                (terminal-state-col-set! state left)
                (line-feed! state))
              ;; Without autowrap a glyph too wide for the remaining cells
              ;; backs the cursor up so the character stays whole instead
              ;; of overflowing its final cell.
              (terminal-state-col-set! state (max left (- limit width)))))
        (let* ([line (vector-ref (terminal-state-screen state)
                                 (terminal-state-row state))]
               [styles (vector-ref (terminal-state-styles state)
                                   (terminal-state-row state))]
               [printed-style (printed-cell-style state)]
               [col (terminal-state-col state)]
               [limit (min (if (<= left col right) (+ right 1) cols)
                           (row-columns state (terminal-state-row state)))]
               [width (min width (- limit col))])
          (when (terminal-state-insert state) (insert-characters! state width))
          (do ([index col (+ index 1)]) ((= index (+ col width)))
            (clear-cell! line styles index (terminal-state-style state)))
          (vector-set! line col (string character))
          (vector-set! styles col printed-style)
          (do ([index (+ col 1) (+ index 1)]) ((= index (+ col width)))
            (vector-set! line index "")
            (vector-set! styles index printed-style))
          (if (= (+ col width) limit)
            (begin
              (terminal-state-col-set! state (- limit 1))
              (terminal-state-wrap-pending-set! state #t))
            (terminal-state-col-set! state (+ col width)))))
      (terminal-state-last-character-set! state character)
      (void)))

  (define (erase-line! state start end)
    (let ([line (vector-ref (terminal-state-screen state)
                            (terminal-state-row state))]
          [styles (vector-ref (terminal-state-styles state)
                              (terminal-state-row state))])
      (do ([col (max 0 start) (+ col 1)])
          ((>= col (min end (terminal-state-cols state))))
        (clear-cell! line styles col (terminal-state-style state))
        (vector-set! styles col (terminal-state-style state)))))

  (define (clear-rows! state start end)
    (do ([row (max 0 start) (+ row 1)])
        ((>= row (min end (terminal-state-rows state))))
      (vector-set! (terminal-state-screen state) row
                   (blank-line (terminal-state-cols state)))
      (vector-set! (terminal-state-wrapped state) row #f)
      (vector-set! (terminal-state-styles state) row
                   (blank-styles (terminal-state-cols state)
                                 (terminal-state-style state)))))

  (define (screen-alignment-test! state)
    (let ([rows (terminal-state-rows state)]
          [cols (terminal-state-cols state)]
          [style (terminal-state-style state)])
      (let ([screen (make-vector rows)])
        (do ([row 0 (+ row 1)]) ((= row rows))
          (vector-set! screen row (make-vector cols "E")))
        (terminal-state-screen-set! state screen))
      (terminal-state-wrapped-set! state (make-vector rows #f))
      (terminal-state-styles-set! state (make-style-screen rows cols style))
      (terminal-state-line-attributes-set!
        state (make-vector rows 'single))
      (terminal-state-row-set! state 0)
      (terminal-state-col-set! state 0)
      (terminal-state-wrap-pending-set! state #f)
      (terminal-state-dirty-set! state #t)))

  (define (parameter-list text)
    (let* ([plain (if (and (> (string-length text) 0)
                           (memv (string-ref text 0) '(#\? #\> #\!)))
                      (substring text 1 (string-length text)) text)]
           [parts (string:lines
                    (list->string
                      (map (lambda (c) (if (char=? c #\;) #\newline c))
                           (string->list plain))))])
      (map (lambda (part) (or (string->number part) 0)) parts)))

  (define (split-parameter part separator)
    (let loop ([chars (string->list part)] [field '()] [out '()])
      (cond
        [(null? chars)
         (reverse (cons (list->string (reverse field)) out))]
        [(char=? (car chars) separator)
         (loop (cdr chars) '()
               (cons (list->string (reverse field)) out))]
        [else (loop (cdr chars) (cons (car chars) field) out)])))

  (define (sgr-parameter-list text)
    ;; ISO 8613-6 permits colon-delimited color subparameters. Normalize
    ;; 38:5:n and 38:2:[colorspace:]r:g:b (and their 48 background forms) to
    ;; the semicolon form understood by the canonical SGR state machine.
    ;; Underline subparameters stay grouped so they retain their meaning.
    (apply append
      (map (lambda (part)
             (let ([fields (split-parameter part #\:)])
               (if (null? (cdr fields))
                   (list (or (string->number part) 0))
                   (let ([values (map (lambda (field)
                                        (and (not (string=? field ""))
                                             (string->number field)))
                                      fields)])
                     ;; A truncated or non-numeric subparameter must never
                     ;; crash the parser or degrade into SGR 0: drop the
                     ;; malformed color and keep the surrounding rendition.
                     (cond
                       [(eqv? (car values) 4)
                        ;; Keep underline subparameters as one operation;
                        ;; flattening 4:4 loses the dotted variant.
                        (if (and (= (length values) 2) (memv (cadr values) '(0 1 2 3 4 5)))
                            (case (cadr values) [(0) '(24)] [(1) '(4)] [else (list values)])
                            '())]
                       [(and (memv (car values) '(38 48 58))
                             (pair? (cdr values))
                             (eqv? (cadr values) 5))
                        (list (car values) 5
                              (or (and (pair? (cddr values)) (caddr values))
                                  0))]
                       [(and (memv (car values) '(38 48 58))
                             (pair? (cdr values))
                             (eqv? (cadr values) 2))
                        (let ([rgb (filter number? (cddr values))])
                          (if (>= (length rgb) 3)
                              (list (car values) 2
                                    (list-ref rgb (- (length rgb) 3))
                                    (list-ref rgb (- (length rgb) 2))
                                    (list-ref rgb (- (length rgb) 1)))
                              '()))]
                       [(memv (car values) '(38 48 58)) '()]
                       [else (list (or (car values) 0))])))))
           (split-parameter text #\;))))

  (define (param parameters index default)
    (let ([value (and (< index (length parameters))
                      (list-ref parameters index))])
      (if (or (not value) (= value 0)) default value)))

  ;; OSC and DCS payloads accumulate one character at a time and can reach
  ;; the megabyte ceiling (clipboard writes), so they are collected as a
  ;; length-counted reversed character list and materialized at dispatch:
  ;; O(1) per character where string appending would be quadratic.
  (define (empty-control-text) (list 0))

  (define (control-text-add! state limit character)
    ;; #f when the payload has reached its ceiling.
    (let ([text (terminal-state-osc-text state)])
      (and (< (car text) limit)
           (begin
             (terminal-state-osc-text-set!
               state (cons (+ (car text) 1) (cons character (cdr text))))
             #t))))

  (define (control-text state)
    (list->string (reverse (cdr (terminal-state-osc-text state)))))

  (define (terminal-reply-wire state text)
    (if (not (terminal-state-controls-eight-bit state))
        (values text (string->utf8 text))
        (cond
          [(string:prefix? "\x1b;[" text)
           (let* ([tail (substring text 2 (string-length text))]
                  [bytes (string->utf8 tail)]
                  [wire (make-bytevector (+ (bytevector-length bytes) 1))])
             (bytevector-u8-set! wire 0 #x9b)
             (bytevector-copy! bytes 0 wire 1 (bytevector-length bytes))
             (values (string-append (string (integer->char #x9b)) tail) wire))]
          [(and (string:prefix? "\x1b;P" text)
                (>= (string-length text) 4)
                (string=? (substring text (- (string-length text) 2)
                                     (string-length text))
                          "\x1b;\\"))
           (let* ([tail (substring text 2 (- (string-length text) 2))]
                  [bytes (string->utf8 tail)]
                  [wire (make-bytevector (+ (bytevector-length bytes) 2))]
                  [end (- (bytevector-length wire) 1)])
             (bytevector-u8-set! wire 0 #x90)
             (bytevector-copy! bytes 0 wire 1 (bytevector-length bytes))
             (bytevector-u8-set! wire end #x9c)
             (values (string-append (string (integer->char #x90)) tail
                                    (string (integer->char #x9c)))
                     wire))]
          [else (values text (string->utf8 text))])))

  (define (terminal-reply! state text)
    (let-values ([(reported wire) (terminal-reply-wire state text)])
      (if (terminal-state-process state)
          ;; A live terminal's replies go to the child; recording them as
          ;; well would grow without bound over a session.
          (let ([output (sys:terminal-process-output
                          (terminal-state-process state))])
            (put-bytevector output wire)
            (flush-output-port output))
          ;; Headless replies accumulate newest first so each is O(1);
          ;; terminal-emulator-replies restores emission order.
          (terminal-state-replies-set!
            state (cons reported (terminal-state-replies state))))))

  (define (primary-device-attributes! state)
    ;; VT100 with advanced video: matches xterm-256color's terminfo probe.
    (terminal-reply! state "\x1b;[?1;2c"))

  (define (osc-color-text color)
    (define (component value)
      (let ([hex (format "~x" value)])
        (let ([byte (if (= (string-length hex) 1)
                        (string-append "0" hex) hex)])
          (string-append byte byte))))
    (format "rgb:~a/~a/~a"
            (component (car color))
            (component (cadr color))
            (component (caddr color))))

  (define (default-color state foreground?)
    (or (if foreground?
            (terminal-state-default-foreground state)
            (terminal-state-default-background state))
        (if foreground? '(0 0 0) '(255 255 255))))

  (define (set-default-color! state foreground? specification)
    (cond
      [(string=? specification "?")
       (terminal-reply!
         state
         (format "\x1b;]~a;~a\x1b;\\"
                 (if foreground? 10 11)
                 (osc-color-text (default-color state foreground?))))]
      [(color:parse specification) =>
       (lambda (color)
         (if foreground?
             (terminal-state-default-foreground-set! state color)
             (terminal-state-default-background-set! state color))
         (invalidate-rendered-scrollback! state)
         (terminal-state-dirty-set! state #t))]))

  (define (dispatch-palette! state fields)
    (let ([palette (terminal-state-palette state)])
      (let loop ([fields fields])
        (when (and (pair? fields) (pair? (cdr fields)))
          (let ([index (string->number (car fields))]
                [specification (cadr fields)])
            (when (and (integer? index) (exact? index) (<= 0 index 255))
              (if (string=? specification "?")
                  (terminal-reply!
                    state
                    (format "\x1b;]4;~a;~a\x1b;\\"
                            index
                            (osc-color-text
                              (or (vector-ref palette index)
                                  (vector-ref (make-default-palette) index)))))
                  (cond [(color:parse specification) =>
                         (lambda (color)
                           (vector-set! palette index color)
                           (invalidate-rendered-scrollback! state)
                           (terminal-state-dirty-set! state #t))])))
            (loop (cddr fields)))))))

  (define (printable-character? character)
    ;; C0 and C1 controls (and DEL) inside child-supplied metadata could
    ;; act on the host terminal when the text is redisplayed.
    (let ([code (char->integer character)])
      (and (>= code 32) (not (= code 127)) (not (<= 128 code 159)))))

  (define (dispatch-hyperlink! state text)
    ;; OSC 8 ; params ; URI ST. Only id is semantic to the emulator; unknown
    ;; parameters remain safely ignored as tmux does. An empty URI closes it.
    (let ([separator (string:search text ";" 2 (string-length text))])
      (when separator
        (let* ([parameters (substring text 2 separator)]
               [uri (substring text (+ separator 1) (string-length text))]
               [id (find (lambda (field) (string:prefix? "id=" field))
                         (split-parameter parameters #\:))])
          (terminal-state-link-set!
            state
            (and (not (string=? uri ""))
                 (list uri (and id (substring id 3 (string-length id))))))))))

  (define clipboard-base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define (base64-value character)
    (string:search clipboard-base64-alphabet (string character)
                   0 (string-length clipboard-base64-alphabet)))

  (define (decode-base64 text)
    (let ([length (string-length text)])
      (and (= (modulo length 4) 0)
           (let loop ([at 0] [bytes '()])
             (if (= at length)
                 (guard (ex [else #f])
                   (utf8->string (u8-list->bytevector (reverse bytes))))
                 (let* ([last? (= (+ at 4) length)]
                        [ca (string-ref text at)]
                        [cb (string-ref text (+ at 1))]
                        [cc (string-ref text (+ at 2))]
                        [cd (string-ref text (+ at 3))]
                        [a (base64-value ca)] [b (base64-value cb)]
                        [c (and (not (char=? cc #\=)) (base64-value cc))]
                        [d (and (not (char=? cd #\=)) (base64-value cd))])
                   (and a b
                        (or c (and last? (char=? cc #\=)
                                   (char=? cd #\=)))
                        (or d (and last? (char=? cd #\=)))
                        (let* ([bits (+
                                       (bitwise-arithmetic-shift-left a 18)
                                       (bitwise-arithmetic-shift-left b 12)
                                       (bitwise-arithmetic-shift-left (or c 0) 6)
                                       (or d 0))]
                               [bytes (cons
                                        (bitwise-and
                                          (bitwise-arithmetic-shift-right
                                            bits 16) 255)
                                        bytes)]
                               [bytes (if (char=? cc #\=) bytes
                                          (cons (bitwise-and
                                                  (bitwise-arithmetic-shift-right
                                                    bits 8) 255)
                                                bytes))]
                               [bytes (if (char=? cd #\=) bytes
                                          (cons (bitwise-and bits 255) bytes))])
                          (loop (+ at 4) bytes)))))))))

  (define (dispatch-clipboard! state text)
    ;; OSC 52 ; selection ; base64-data ST. Queries are deliberately ignored:
    ;; importing clipboard contents is useful, exposing the copy buffer to an
    ;; untrusted child is not.
    (let ([separator (string:search text ";" 3 (string-length text))])
      (when separator
        (let ([payload (substring text (+ separator 1) (string-length text))])
          (unless (string=? payload "?")
            (let ([clipboard (decode-base64 payload)])
              (when clipboard
                (terminal-state-clipboard-set! state clipboard)
                (terminal-state-clipboard-sequence-set! state
                  (+ 1 (terminal-state-clipboard-sequence state)))
                (terminal-state-clipboard-target-set! state (terminal-state-controller state)))))))))

  (define (dispatch-osc! state)
    (let* ([text (control-text state)]
           [fields (split-parameter text #\;)])
      (cond
        [(and (pair? fields) (string=? (car fields) "4"))
         (dispatch-palette! state (cdr fields))]
        [(string:prefix? "8;" text)
         (dispatch-hyperlink! state text)]
        [(string:prefix? "52;" text)
         (dispatch-clipboard! state text)]
        [(and (= (length fields) 2) (string=? (car fields) "10"))
         (set-default-color! state #t (cadr fields))]
        [(and (= (length fields) 2) (string=? (car fields) "11"))
         (set-default-color! state #f (cadr fields))]
        [(and (pair? fields) (string=? (car fields) "104"))
         (if (null? (cdr fields))
             (vector-fill! (terminal-state-palette state) #f)
             (for-each
               (lambda (field)
                 (let ([index (string->number field)])
                   (when (and (integer? index) (exact? index) (<= 0 index 255))
                     (vector-set! (terminal-state-palette state) index #f))))
               (cdr fields)))
         (invalidate-rendered-scrollback! state)
         (terminal-state-dirty-set! state #t)]
        [(string=? text "110")
         (terminal-state-default-foreground-set! state #f)
         (invalidate-rendered-scrollback! state)
         (terminal-state-dirty-set! state #t)]
        [(string=? text "111")
         (terminal-state-default-background-set! state #f)
         (invalidate-rendered-scrollback! state)
         (terminal-state-dirty-set! state #t)]
        [(and (>= (string-length text) 2)
              (memv (string-ref text 0) '(#\0 #\1 #\2))
              (char=? (string-ref text 1) #\;))
         ;; An empty title is a legitimate update (xterm clears its title);
         ;; consume it silently and keep the buffer's current name.
         (let ([title (list->string
                        (filter printable-character?
                                (string->list
                                  (substring text 2
                                             (string-length text)))))])
           (unless (string=? title "") (terminal-state-title-set! state title)))]
        [else
         ;; OSC payloads can contain secrets (titles, paths, clipboard data),
         ;; so identify an unsupported command by its numeric selector only.
         (let ([selector (and (pair? fields) (car fields))])
           (report-unsupported!
             state (format "OSC ~a" (if (and selector
                                             (not (string=? selector "")))
                                        selector "sequence"))))])))

  (define (hex-string text)
    (apply string-append
      (map (lambda (character)
             (let ([hex (format "~x" (char->integer character))])
               (if (= (string-length hex) 1)
                   (string-append "0" hex) hex)))
           (string->list text))))

  (define (unhex-string text)
    (and (even? (string-length text))
         (let loop ([at 0] [characters '()])
           (if (= at (string-length text))
               (list->string (reverse characters))
               (let ([value (string->number (substring text at (+ at 2)) 16)])
                 (and value
                      (loop (+ at 2)
                            (cons (integer->char value) characters))))))))

  (define terminal-capabilities
    '(("TN" . "xterm-256color")
      ("Co" . "256")
      ("RGB" . "8")
      ("colors" . "256")
      ("pairs" . "65536")
      ("Tc" . #t)))

  (define (reply-terminal-capability! state encoded-name)
    (let* ([name (unhex-string encoded-name)]
           [entry (and name (assoc name terminal-capabilities))])
      (terminal-reply!
        state
        (if entry
            (format "\x1b;P1+r~a~a\x1b;\\"
                    encoded-name
                    (if (eq? (cdr entry) #t) ""
                        (string-append "=" (hex-string (cdr entry)))))
            (format "\x1b;P0+r~a\x1b;\\" encoded-name)))))

  (define (dispatch-dcs! state)
    (let ([text (control-text state)])
      (cond
        [(string:prefix? "$q" text)
         (let* ([request (substring text 2 (string-length text))]
                [value
                 (cond
                   [(string=? request "m")
                    (format "~am"
                            (if (string=? (terminal-state-sgr state) "")
                                "0" (terminal-state-sgr state)))]
                   [(string=? request "r")
                    (format "~a;~ar"
                            (+ (terminal-state-scroll-top state) 1)
                            (+ (terminal-state-scroll-bottom state) 1))]
                   [(string=? request "s")
                    (format "~a;~as"
                            (+ (terminal-state-left-margin state) 1)
                            (+ (terminal-state-right-margin state) 1))]
                   [else #f])])
           (terminal-reply!
             state
             (format "\x1b;P~a$r~a\x1b;\\"
                     (if value 1 0) (or value request))))]
        [(string:prefix? "+q" text)
         (for-each
           (lambda (name) (reply-terminal-capability! state name))
           (split-parameter
             (substring text 2 (string-length text)) #\;))]
        [else
         ;; DCS payloads are application-defined and may be sensitive. The
         ;; two-byte introducer is enough to identify its protocol family.
         (report-unsupported!
           state
           (format "DCS ~s"
                   (substring text 0 (min 2 (string-length text)))))])))

  (define (normalize-cell-row! cells styles)
    (let ([cols (vector-length cells)])
      (do ([col 0 (+ col 1)]) ((= col cols))
        (let ([cell (vector-ref cells col)])
          (cond [(string=? cell "")
                 (when (or (= col 0)
                           (< (glyph:width
                                (vector-ref cells (- col 1))) 2))
                   (vector-set! cells col " "))]
                [(>= (glyph:width cell) 2)
                 (if (= (+ col 1) cols)
                     (vector-set! cells col " ")
                     (begin
                       (vector-set! cells (+ col 1) "")
                       (vector-set! styles (+ col 1)
                                    (vector-ref styles col))))])))))

  (define (delete-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [end (if (<= (left-bound state) col (right-bound state))
                    (+ (right-bound state) 1)
                    (terminal-state-cols state))]
           [count (min count (- end col))])
      (copy-vector-range! line (+ col count) line col (- end col count))
      (copy-vector-range! styles (+ col count) styles col (- end col count))
      (do ([i (- end count) (+ i 1)]) ((= i end))
        (vector-set! line i " ")
        (vector-set! styles i (terminal-state-style state)))
      (normalize-cell-row! line styles)))

  (define (insert-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [end (if (<= (left-bound state) col (right-bound state))
                    (+ (right-bound state) 1)
                    (terminal-state-cols state))]
           [count (min count (- end col))])
      (do ([i (- end 1) (- i 1)]) ((< i (+ col count)))
        (vector-set! line i (vector-ref line (- i count)))
        (vector-set! styles i (vector-ref styles (- i count))))
      (do ([i col (+ i 1)]) ((= i (+ col count)))
        (vector-set! line i " ")
        (vector-set! styles i (terminal-state-style state)))
      (normalize-cell-row! line styles)))

  (define (scroll-horizontally! state count right?)
    (let ([row (terminal-state-row state)]
          [col (terminal-state-col state)])
      (do ([line (scrolling-top state) (+ line 1)])
          ((> line (terminal-state-scroll-bottom state)))
        (terminal-state-row-set! state line)
        (terminal-state-col-set! state (left-bound state))
        ((if right? insert-characters! delete-characters!) state count)
        (vector-set! (terminal-state-wrapped state) line #f))
      (terminal-state-row-set! state row)
      (terminal-state-col-set! state col)))

  (define (sgr-style sequence)
    ;; A face is a value each head can render, never a head registration.
    ;; Reset first so a cell's complete rendition does not inherit prior SGR.
    (if (string=? sequence "") 'plain (string-append "0;" sequence)))

  (define (style-parameters style)
    (if (eq? style 'plain) "" (substring style 2 (string-length style))))

  (define (sgr-text operations)
    (string:join
      (map (lambda (op) (string:join (map number->string op) (if (= (car op) 4) ":" ";"))) operations)
      ";"))

  (define (reversed-style style)
    (let* ([sequence (style-parameters style)]
           [operations (and sequence
                            (sgr-operations (sgr-parameter-list sequence)))])
      (if (not operations) style
          (let* ([has-reverse?
                  (exists (lambda (operation)
                            (eq? (sgr-category operation) 'reverse))
                          operations)]
                 [updated
                  (if has-reverse?
                      (remp (lambda (operation)
                              (eq? (sgr-category operation) 'reverse))
                            operations)
                      (append operations '((7))))])
            (sgr-style (sgr-text updated))))))

  (define (palette-index operation foreground?)
    (let ([code (car operation)])
      (cond
        [(and foreground? (<= 30 code 37)) (- code 30)]
        [(and foreground? (<= 90 code 97)) (+ 8 (- code 90))]
        [(and (not foreground?) (<= 40 code 47)) (- code 40)]
        [(and (not foreground?) (<= 100 code 107)) (+ 8 (- code 100))]
        [(and (= code (if foreground? 38 48))
              (= (length operation) 3) (= (cadr operation) 5))
         (caddr operation)]
        [else #f])))

  (define (rgb-operation foreground? color)
    (append (list (if foreground? 38 48) 2) color))

  (define (resolved-style state style)
    (let* ([sequence (style-parameters style)]
           [operations (if (string=? sequence "") '()
                           (sgr-operations (sgr-parameter-list sequence)))]
           [palette (terminal-state-palette state)]
           [resolved
            (map (lambda (operation)
                   (let* ([foreground-index
                           (palette-index operation #t)]
                          [background-index
                           (palette-index operation #f)]
                          [index (or foreground-index background-index)]
                          [color (and index (vector-ref palette index))])
                     (cond
                       [color
                        (rgb-operation (and foreground-index #t) color)]
                       [(and (= (car operation) 39)
                             (terminal-state-default-foreground state))
                        (rgb-operation
                          #t (terminal-state-default-foreground state))]
                       [(and (= (car operation) 49)
                             (terminal-state-default-background state))
                        (rgb-operation
                          #f (terminal-state-default-background state))]
                       [else operation])))
                 operations)]
           [resolved
            (if (and (terminal-state-default-foreground state)
                     (not (exists (lambda (operation)
                                    (eq? (sgr-category operation) 'foreground))
                                  resolved)))
                (append resolved
                        (list (rgb-operation
                                #t (terminal-state-default-foreground state))))
                resolved)]
           [resolved
            (if (and (terminal-state-default-background state)
                     (not (exists (lambda (operation)
                                    (eq? (sgr-category operation) 'background))
                                  resolved)))
                (append resolved
                        (list (rgb-operation
                                #f (terminal-state-default-background state))))
                resolved)])
      (if (null? resolved) 'plain
          (sgr-style (sgr-text resolved)))))

  (define (effective-style-row state row)
    (vector-map
      (lambda (style)
        (let ([style (resolved-style state (cell-style-face style))])
          (if (terminal-state-reverse-screen state)
              (reversed-style style) style)))
      row))

  (define (sgr-operations codes)
    ;; Group extended colors so zero-valued components remain color data;
    ;; the parser has already grouped colon-delimited underline variants.
    (let loop ([xs codes] [out '()])
      (cond [(null? xs) (reverse out)]
            [(pair? (car xs)) (loop (cdr xs) (cons (car xs) out))]
            [else
             (let* ([code (car xs)]
                    [count (if (and (memv code '(38 48 58)) (pair? (cdr xs)))
                             (case (cadr xs) [(5) 3] [(2) 5] [else 1])
                             1)])
               (let take ([ys xs] [n count] [op '()])
                 (if (or (= n 0) (null? ys))
                   ;; A grouped attribute cannot be a color component.
                   (loop ys (if (for-all number? op) (cons (reverse op) out) out))
                   (take (cdr ys) (- n 1) (cons (car ys) op)))))])))

  (define (sgr-category op)
    (let ([code (car op)])
      (cond
        [(or (= code 1) (= code 2)) 'intensity]
        [(= code 3) 'italic]
        [(or (= code 4) (= code 21)) 'underline]
        [(or (= code 5) (= code 6)) 'blink]
        [(= code 7) 'reverse]
        [(= code 8) 'hidden]
        [(= code 9) 'strike]
        [(or (= code 38) (<= 30 code 37) (<= 90 code 97)) 'foreground]
        [(or (= code 48) (<= 40 code 47) (<= 100 code 107)) 'background]
        [(= code 58) 'underline-color]
        [else code])))

  (define (canonical-sgr current additions)
    (define (remove-category state category)
      (remp (lambda (op) (eqv? (sgr-category op) category)) state))
    (let loop ([ops (append (sgr-operations (sgr-parameter-list current))
                            (sgr-operations additions))]
               [state '()])
      (if (null? ops)
          (reverse state)
          (let* ([op (car ops)] [code (car op)])
            (cond
              [(= code 0) (loop (cdr ops) '())]
              [(= code 22) (loop (cdr ops)
                                 (remove-category state 'intensity))]
              [(memv code '(23 24 25 27 28 29))
               (loop (cdr ops)
                     (remove-category
                       state
                       (case code
                         [(23) 'italic] [(24) 'underline] [(25) 'blink]
                         [(27) 'reverse] [(28) 'hidden] [else 'strike])))]
              [(= code 39) (loop (cdr ops)
                                 (remove-category state 'foreground))]
              [(= code 49) (loop (cdr ops)
                                 (remove-category state 'background))]
              [(= code 59) (loop (cdr ops)
                                 (remove-category state 'underline-color))]
              [else
               (let ([category (sgr-category op)])
                 (loop (cdr ops)
                       (cons op (remove-category state category))))])))))

  (define (set-sgr! state text)
    ;; Store the effective face, not a history of every SGR command. This
    ;; makes selective resets exact and keeps emitted sequences bounded.
    (let* ([operations (canonical-sgr (terminal-state-sgr state)
                                      (sgr-parameter-list text))]
           [sequence (sgr-text operations)])
      (terminal-state-sgr-set! state sequence)
      (terminal-state-style-set! state (sgr-style sequence))))

  (define (reset-terminal-state! state)
    (let ([rows (terminal-state-rows state)]
          [cols (terminal-state-cols state)])
      (terminal-state-screen-set! state (make-screen rows cols))
      (terminal-state-wrapped-set! state (make-vector rows #f))
      (terminal-state-styles-set! state (make-style-screen rows cols 'plain))
      (terminal-state-row-set! state 0)
      (terminal-state-col-set! state 0)
      (terminal-state-saved-row-set! state 0)
      (terminal-state-saved-col-set! state 0)
      (terminal-state-saved-state-set! state #f)
      (terminal-state-scroll-top-set! state 0)
      (terminal-state-scroll-bottom-set! state (- rows 1))
      (terminal-state-left-margin-set! state 0)
      (terminal-state-right-margin-set! state (- cols 1))
      (terminal-state-margin-mode-set! state #f)
      (terminal-state-memory-lock-set! state #f)
      (terminal-state-parameters-set! state "")
      (terminal-state-osc-escape-set! state #f)
      (terminal-state-charset-set! state 'ascii)
      (terminal-state-charset-g1-set! state 'ascii)
      (terminal-state-charset-target-set! state 0)
      (terminal-state-shift-set! state 0)
      (terminal-state-wrap-pending-set! state #f)
      (terminal-state-autowrap-set! state #t)
      (terminal-state-origin-set! state #f)
      (terminal-state-insert-set! state #f)
      (terminal-state-newline-set! state #f)
      (terminal-state-reverse-screen-set! state #f)
      (terminal-state-cursor-keys-set! state #f)
      (terminal-state-keypad-set! state #f)
      (terminal-state-meta-eight-bit-set! state #f)
      (terminal-state-controls-eight-bit-set! state #f)
      (terminal-state-cursor-visible-set! state #t)
      (terminal-state-tab-stops-set! state (default-tab-stops cols))
      (terminal-state-last-character-set! state #\space)
      (terminal-state-bell-set! state #f)
      (terminal-state-bell-visible-set! state #f)
      (terminal-state-bell-deadline-set! state #f)
      (terminal-state-mouse-set! state #f)
      (terminal-state-mouse-sgr-set! state #f)
      (terminal-state-mouse-utf8-set! state #f)
      (terminal-state-mouse-urxvt-set! state #f)
      (terminal-state-focus-reporting-set! state #f)
      (terminal-state-bracketed-set! state #f)
      (terminal-state-main-screen-set! state #f)
      (terminal-state-main-wrapped-set! state #f)
      (terminal-state-main-styles-set! state #f)
      (terminal-state-main-state-set! state #f)
      (terminal-state-alternate-screen-set! state #f)
      (terminal-state-alternate-wrapped-set! state #f)
      (terminal-state-alternate-styles-set! state #f)
      (terminal-state-alternate-state-set! state #f)
      (terminal-state-history-set! state (make-empty-scrollback))
      (terminal-state-sgr-set! state "")
      (terminal-state-style-set! state 'plain)
      (terminal-state-link-set! state #f)
      (terminal-state-palette-set! state (make-vector 256 #f))
      (terminal-state-default-foreground-set! state #f)
      (terminal-state-default-background-set! state #f)
      (invalidate-rendered-scrollback! state)
      (terminal-state-printer-controller-set! state #f)
      (terminal-state-printer-pending-set! state "")
      (terminal-state-printer-output-set! state (cons 0 '()))
      (terminal-state-extra-modes-set! state '(8))
      (terminal-state-line-attributes-set! state (make-vector rows 'single))
      (terminal-state-main-line-attributes-set! state #f)
      (terminal-state-alternate-line-attributes-set! state #f)
      (terminal-state-dirty-set! state #t)
      (set-cursor-shape! state 'blinking-block)))

  (define (set-cursor-shape! state shape)
    (terminal-state-cursor-shape-set! state shape)
    (terminal-state-dirty-set! state #t))

  (define (soft-reset-terminal-state! state)
    ;; DECSTR restores operational modes and rendition without erasing text.
    (let ([rows (terminal-state-rows state)]
          [cols (terminal-state-cols state)])
      (terminal-state-row-set! state 0)
      (terminal-state-col-set! state 0)
      (terminal-state-saved-row-set! state 0)
      (terminal-state-saved-col-set! state 0)
      (terminal-state-saved-state-set! state #f)
      (terminal-state-scroll-top-set! state 0)
      (terminal-state-scroll-bottom-set! state (- rows 1))
      (terminal-state-left-margin-set! state 0)
      (terminal-state-right-margin-set! state (- cols 1))
      (terminal-state-margin-mode-set! state #f)
      (terminal-state-memory-lock-set! state #f)
      (terminal-state-charset-set! state 'ascii)
      (terminal-state-charset-g1-set! state 'ascii)
      (terminal-state-charset-target-set! state 0)
      (terminal-state-shift-set! state 0)
      (terminal-state-wrap-pending-set! state #f)
      (terminal-state-autowrap-set! state #t)
      (terminal-state-origin-set! state #f)
      (terminal-state-insert-set! state #f)
      (terminal-state-newline-set! state #f)
      (terminal-state-reverse-screen-set! state #f)
      (terminal-state-cursor-keys-set! state #f)
      (terminal-state-keypad-set! state #f)
      (terminal-state-meta-eight-bit-set! state #f)
      (terminal-state-controls-eight-bit-set! state #f)
      (terminal-state-cursor-visible-set! state #t)
      (terminal-state-last-character-set! state #\space)
      (terminal-state-sgr-set! state "")
      (terminal-state-style-set! state 'plain)
      (terminal-state-link-set! state #f)
      (terminal-state-extra-modes-set! state '(8))
      (terminal-state-dirty-set! state #t)))

  (define (mode-report-value state mode private?)
    ;; DECRPM values: 0 unrecognized, 1 set, 2 reset, 4 permanently reset.
    (define (flag value) (if value 1 2))
    (if private?
        (case mode
          [(1) (flag (terminal-state-cursor-keys state))]
          ;; DECCOLM's mandatory resets are honored, but a tiled window can
          ;; never actually be 132 columns wide.
          [(3) 4]
          [(5) (flag (terminal-state-reverse-screen state))]
          [(6) (flag (terminal-state-origin state))]
          [(7) (flag (terminal-state-autowrap state))]
          [(4 8 12 40 42 45 2026 2031)
           (flag (memv mode (terminal-state-extra-modes state)))]
          [(9 1000 1002 1003) (flag (eqv? (terminal-state-mouse state) mode))]
          [(25) (flag (terminal-state-cursor-visible state))]
          [(47 1047 1049) (flag (and (terminal-state-main-screen state) #t))]
          [(69) (flag (terminal-state-margin-mode state))]
          [(1004) (flag (terminal-state-focus-reporting state))]
          [(1005) (flag (terminal-state-mouse-utf8 state))]
          [(1006) (flag (terminal-state-mouse-sgr state))]
          [(1015) (flag (terminal-state-mouse-urxvt state))]
          [(1034) (flag (terminal-state-meta-eight-bit state))]
          [(2004) (flag (terminal-state-bracketed state))]
          [else 0])
        (case mode
          [(4) (flag (terminal-state-insert state))]
          [(20) (flag (terminal-state-newline state))]
          [else 0])))

  (define csi-decorations
    ;; The decorations (private prefixes and intermediate bytes) each final
    ;; accepts. A decorated sequence selects a different control from its
    ;; plain final -- CSI >4;2m configures xterm's modifyOtherKeys, not SGR
    ;; -- so anything not listed here is reported, never executed as the
    ;; plain action.
    '((#\@ "" " ") (#\A "" " ")
      (#\h "" "?") (#\l "" "?")
      (#\J "" "?") (#\K "" "?")
      (#\c "" ">" "=")
      (#\n "" "?")
      (#\p "!" "$" "?$")
      (#\q " " ">")
      (#\m "" ">" "?") (#\i "") (#\u "" "<") (#\x "") (#\y "")
      (#\r "") (#\s "") (#\S "") (#\T "")))

  (define (csi-decoration text)
    (list->string
      (filter (lambda (character)
                (not (or (char<=? #\0 character #\9)
                         (char=? character #\;)
                         (char=? character #\:))))
              (string->list text))))

  (define (csi-decoration-allowed? final text)
    (let ([decoration (csi-decoration text)]
          [entry (assv final csi-decorations)])
      (if entry
          (and (member decoration (cdr entry)) #t)
          (string=? decoration ""))))

  (define (dispatch-csi! state final text)
    (if (not (csi-decoration-allowed? final text))
        (report-unsupported! state (control-signature "CSI" text final))
        (dispatch-plain-csi! state final text)))

  (define (dispatch-plain-csi! state final text)
    ;; HT and CHT preserve delayed wrap at the right margin.
    (unless (memv final '(#\m #\I))
      (terminal-state-wrap-pending-set! state #f))
    (let* ([cursor-shape? (and (char=? final #\q)
                               (> (string-length text) 0)
                               (char=? (string-ref text
                                                   (- (string-length text) 1))
                                       #\space))]
           [space-intermediate?
            (and (> (string-length text) 0)
                 (char=? (string-ref text (- (string-length text) 1))
                         #\space))]
           [parameter-text (if (or cursor-shape? space-intermediate?)
                               (substring text 0 (- (string-length text) 1))
                               text)]
           [parameters (parameter-list parameter-text)]
           [n (param parameters 0 1)]
           [row (terminal-state-row state)]
           [col (terminal-state-col state)]
           [rows (terminal-state-rows state)]
           [cols (terminal-state-cols state)])
      (if (and (> (string-length text) 0)
               (char=? (string-ref text 0) #\?)
               (memv final '(#\h #\l)))
          (let ([on? (char=? final #\h)])
            (for-each
              (lambda (mode)
                (case mode
                  [(1) (terminal-state-cursor-keys-set! state on?)]
                  [(3)
                   ;; DECCOLM normally asks the host to switch between 80 and
                   ;; 132 columns. A tiled editor window cannot resize itself
                   ;; to that width, but the mode's mandatory screen, cursor,
                   ;; and margin reset semantics still apply.
                   (clear-rows! state 0 rows)
                   (vector-fill!
                     (terminal-state-line-attributes state) 'single)
                   (terminal-state-scroll-top-set! state 0)
                   (terminal-state-scroll-bottom-set! state (- rows 1))
                   (terminal-state-left-margin-set! state 0)
                   (terminal-state-right-margin-set! state (- cols 1))
                   (terminal-state-row-set! state 0)
                   (terminal-state-col-set! state 0)
                   (terminal-state-dirty-set! state #t)]
                  [(5)
                   (terminal-state-reverse-screen-set! state on?)
                   (invalidate-rendered-scrollback! state)
                   (terminal-state-dirty-set! state #t)]
                  [(6)
                   (terminal-state-origin-set! state on?)
                   (terminal-state-row-set!
                     state (if on? (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set!
                     state (if on? (left-bound state) 0))]
                  [(7) (terminal-state-autowrap-set! state on?)]
                  [(4 8 12 40 42 45 2026 2031)
                   (let ([extra (terminal-state-extra-modes state)])
                     (terminal-state-extra-modes-set!
                       state
                       (if on?
                           (if (memv mode extra) extra (cons mode extra))
                           (remv mode extra))))
                   (when (= mode 2026)
                     (if on?
                         (start-synchronized-update! state)
                         (terminal-state-dirty-set! state #t)))
                   ;; A color-scheme subscription learns the current
                   ;; scheme right away when the host has reported one.
                   (when (and (= mode 2031) on? (state-color-scheme state))
                     (terminal-reply!
                       state (color-scheme-report (state-color-scheme state))))]
                  [(69)
                   (terminal-state-margin-mode-set! state on?)
                   (unless on?
                     (terminal-state-left-margin-set! state 0)
                     (terminal-state-right-margin-set! state (- cols 1)))
                   (terminal-state-row-set!
                     state (if (terminal-state-origin state)
                               (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set! state (if on? (left-bound state) 0))]
                  [(25) (terminal-state-cursor-visible-set! state on?)]
                  [(9 1000 1002 1003)
                   (if on?
                       (terminal-state-mouse-set! state mode)
                       (when (eqv? (terminal-state-mouse state) mode)
                         (terminal-state-mouse-set! state #f)))]
                  [(1004) (terminal-state-focus-reporting-set! state on?)]
                  [(1005) (terminal-state-mouse-utf8-set! state on?)]
                  [(1006) (terminal-state-mouse-sgr-set! state on?)]
                  [(1015) (terminal-state-mouse-urxvt-set! state on?)]
                  [(1034) (terminal-state-meta-eight-bit-set! state on?)]
                  [(2004) (terminal-state-bracketed-set! state on?)]
                  [(1048) (if on? (save-cursor! state) (restore-cursor! state))]
                  [(47 1047 1049)
                   ;; Viewports belong to the head. Adoption observes the
                   ;; screen switch in its captured frame after this lock.
                   (if on?
                       (enter-alternate-screen! state mode)
                       (leave-alternate-screen! state mode))]
                  [else
                   (report-unsupported!
                     state (format "private mode ~a" mode))]))
              parameters))
          (case final
            [(#\h #\l)
             (when (not (string:prefix? "?" text))
               (for-each
                 (lambda (mode)
                   (case mode
                     [(4) (terminal-state-insert-set! state
                                                      (char=? final #\h))]
                     [(20) (terminal-state-newline-set! state
                                                        (char=? final #\h))]
                     [else
                      (report-unsupported! state (format "mode ~a" mode))]))
                 parameters))]
            [(#\A)
             (if space-intermediate?
                 (scroll-horizontally! state n #t)
                 (terminal-state-row-set!
                   state (max (if (terminal-state-origin state)
                                  (terminal-state-scroll-top state) 0)
                              (- row n))))]
            [(#\B #\e)
             (terminal-state-row-set!
               state (min (if (terminal-state-origin state)
                              (terminal-state-scroll-bottom state) (- rows 1))
                          (+ row n)))]
            [(#\C #\a)
             (terminal-state-col-set!
               state (min (if (<= (left-bound state) col (right-bound state))
                              (right-bound state) (- cols 1))
                          (+ col n)))]
            [(#\D)
             (terminal-state-col-set!
               state (max (if (<= (left-bound state) col (right-bound state))
                              (left-bound state) 0)
                          (- col n)))]
            [(#\I)
             (do ([left n (- left 1)])
                 ((= left 0))
               (terminal-state-col-set! state (next-tab-stop state)))]
            [(#\E) (terminal-state-row-set!
                     state
                     (min (if (terminal-state-origin state)
                              (terminal-state-scroll-bottom state) (- rows 1))
                          (+ row n)))
             (terminal-state-col-set! state (left-bound state))]
            [(#\F) (terminal-state-row-set!
                     state
                     (max (if (terminal-state-origin state)
                              (terminal-state-scroll-top state) 0)
                          (- row n)))
             (terminal-state-col-set! state (left-bound state))]
            [(#\G #\`)
             (terminal-state-col-set!
               state (if (terminal-state-origin state)
                         (clamp (+ (left-bound state) (- n 1))
                                (left-bound state) (right-bound state))
                         (clamp (- n 1) 0 (- cols 1))))]
            [(#\d) (terminal-state-row-set! state
                                            (if (terminal-state-origin state)
                                                (clamp (+ (terminal-state-scroll-top state)
                                                          (- n 1))
                                                       (terminal-state-scroll-top state)
                                                       (terminal-state-scroll-bottom state))
                                                (clamp (- n 1) 0 (- rows 1))))]
            [(#\H #\f)
             (terminal-state-row-set! state
                                      (if (terminal-state-origin state)
                                          (clamp (+ (terminal-state-scroll-top state)
                                                    (- (param parameters 0 1) 1))
                                                 (terminal-state-scroll-top state)
                                                 (terminal-state-scroll-bottom state))
                                          (clamp (- (param parameters 0 1) 1)
                                                 0 (- rows 1))))
             (terminal-state-col-set!
               state (if (terminal-state-origin state)
                         (clamp (+ (left-bound state)
                                   (- (param parameters 1 1) 1))
                                (left-bound state) (right-bound state))
                         (clamp (- (param parameters 1 1) 1) 0 (- cols 1))))]
            [(#\J)
             (case (param parameters 0 0)
               [(0) (erase-line! state col cols)
                (clear-rows! state (+ row 1) rows)]
               [(1) (clear-rows! state 0 row)
                (erase-line! state 0 (+ col 1))]
               [(2) (clear-rows! state 0 rows)]
               [(3)
                ;; xterm's ED 3 erases only the saved lines; clear(1) sends
                ;; it after ED 2 to leave nothing above the fresh screen.
                (terminal-state-history-set! state (make-empty-scrollback))
                (terminal-state-dirty-set! state #t)]
               [else
                (report-unsupported!
                  state (control-signature "CSI" text final))])]
            [(#\K)
             (case (param parameters 0 0)
               [(0) (erase-line! state col cols)]
               [(1) (erase-line! state 0 (+ col 1))]
               [(2) (erase-line! state 0 cols)]
               [else
                (report-unsupported!
                  state (control-signature "CSI" text final))])]
            [(#\S) (scroll-up! state n)]
            [(#\T) (scroll-down! state n)]
            [(#\P) (delete-characters! state n)]
            [(#\@) (if space-intermediate?
                       (scroll-horizontally! state n #f)
                       (insert-characters! state n))]
            [(#\X) (erase-line! state col (+ col n))]
            [(#\Z)
             (let loop ([candidate (- col 1)] [left n])
               (cond [(or (< candidate 0) (= left 0))
                      (terminal-state-col-set! state (max 0 candidate))]
                     [(vector-ref (terminal-state-tab-stops state) candidate)
                      (if (= left 1)
                          (terminal-state-col-set! state candidate)
                          (loop (- candidate 1) (- left 1)))]
                     [else (loop (- candidate 1) left)]))]
            [(#\g)
             (case (param parameters 0 0)
               [(0) (vector-set! (terminal-state-tab-stops state) col #f)]
               ;; 1 and 2 clear line tab stops, which this class of
               ;; terminal does not have; they are defined no-ops.
               [(1 2) (void)]
               [(3) (vector-fill! (terminal-state-tab-stops state) #f)]
               [else
                (report-unsupported!
                  state (control-signature "CSI" text final))])]
            [(#\L)
             (when (<= (terminal-state-scroll-top state) row
                       (terminal-state-scroll-bottom state))
               (let ([old (terminal-state-scroll-top state)])
                 (terminal-state-scroll-top-set! state row)
                 (scroll-down!
                   state (min n (+ 1 (- (terminal-state-scroll-bottom state)
                                        row))))
                 (terminal-state-scroll-top-set! state old)))]
            [(#\M)
             (when (<= (terminal-state-scroll-top state) row
                       (terminal-state-scroll-bottom state))
               (let ([old (terminal-state-scroll-top state)])
                 (terminal-state-scroll-top-set! state row)
                 (scroll-up!
                   state (min n (+ 1 (- (terminal-state-scroll-bottom state)
                                        row))))
                 (terminal-state-scroll-top-set! state old)))]
            [(#\r)
             (let ([top (clamp (- (param parameters 0 1) 1) 0 (- rows 1))]
                   [bottom (clamp (- (param parameters 1 rows) 1)
                                  0 (- rows 1))])
               (when (< top bottom)
                 (terminal-state-scroll-top-set! state top)
                 (terminal-state-scroll-bottom-set! state bottom)))
             (terminal-state-row-set!
               state (if (terminal-state-origin state)
                         (terminal-state-scroll-top state) 0))
             (terminal-state-col-set! state
                                      (if (terminal-state-origin state)
                                          (left-bound state) 0))]
            [(#\s)
             (if (terminal-state-margin-mode state)
                 (let ([left (clamp (- (param parameters 0 1) 1)
                                    0 (- cols 1))]
                       [right (clamp (- (param parameters 1 cols) 1)
                                     0 (- cols 1))])
                   (when (< left right)
                     (terminal-state-left-margin-set! state left)
                     (terminal-state-right-margin-set! state right))
                   (terminal-state-row-set!
                     state (if (terminal-state-origin state)
                               (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set! state (left-bound state)))
                 (save-cursor! state))]
            [(#\u)
             (if (string:prefix? "<" text)
                 ;; A kitty keyboard-protocol pop. Nothing is ever pushed
                 ;; here, and the protocol defines popping an empty stack
                 ;; as a no-op, so exiting programs that pop defensively
                 ;; stay quiet.
                 (void)
                 (restore-cursor! state))]
            [(#\m)
             (cond
               [(string:prefix? ">" text)
                ;; XTMODKEYS. The key encodings never change, matching the
                ;; disabled level reported to XTQMODKEYS, so accepting the
                ;; setting silently keeps vim's startup and exit quiet.
                (unless (= (param parameters 0 0) 4)
                  (report-unsupported!
                    state (control-signature "CSI" text final)))]
               [(string:prefix? "?" text)
                ;; XTQMODKEYS: modifyOtherKeys is permanently off.
                (if (= (param parameters 0 0) 4)
                    (terminal-reply! state "\x1b;[>4;0m")
                    (report-unsupported!
                      state (control-signature "CSI" text final)))]
               [else (set-sgr! state text)])]
            [(#\b)
             (let ([character (terminal-state-last-character state)])
               (when character
                 (do ([left n (- left 1)])
                     ((= left 0))
                   (put-character! state character)))
               ;; REP itself is now the preceding control sequence, so a
               ;; second REP without an intervening graphic is ignored.
               (terminal-state-last-character-set! state #f))]
            [(#\p)
             (cond
               [(string:prefix? "!" text)
                (soft-reset-terminal-state! state)]
               [(and (> (string-length text) 0)
                     (char=? (string-ref text (- (string-length text) 1))
                             #\$))
                ;; DECRQM: even an unrecognized mode deserves a reply, or
                ;; the requester waits on a timeout.
                (let* ([private? (string:prefix? "?" text)]
                       [body (substring text (if private? 1 0)
                                        (- (string-length text) 1))]
                       [mode (or (string->number body) 0)])
                  (terminal-reply!
                    state
                    (format "\x1b;[~a~a;~a$y"
                            (if private? "?" "") mode
                            (mode-report-value state mode private?))))])]
            [(#\t)
             ;; XTWINOPS. The text-area size report keeps applications
             ;; that probe geometry from timing out; the title-stack
             ;; operations are no-ops, as in xterm with window operations
             ;; disallowed.
             (case (param parameters 0 0)
               [(18)
                (terminal-reply!
                  state (format "\x1b;[8;~a;~at" rows cols))]
               [(22 23) (void)]
               [else
                (report-unsupported!
                  state (control-signature "CSI" text final))])]
            [(#\y)
             ;; DECTST asks the terminal to run its built-in confidence test.
             ;; A software terminal with no failing hardware completes it
             ;; silently, as xterm does.
             (void)]
            [(#\n)
             (cond
               [(= (param parameters 0 0) 5)
                (terminal-reply!
                  state (if (string:prefix? "?" text)
                            "\x1b;[?0n" "\x1b;[0n"))]
               [(= (param parameters 0 0) 6)
                ;; DECOM makes the cursor report relative to both margins.
                (let ([reported-row
                       (if (terminal-state-origin state)
                           (- row (terminal-state-scroll-top state)) row)]
                      [reported-col
                       (if (terminal-state-origin state)
                           (- col (left-bound state)) col)])
                  (terminal-reply!
                    state
                    (format "\x1b;[~a~a;~aR"
                            (if (string:prefix? "?" text) "?" "")
                            (+ reported-row 1) (+ reported-col 1))))]
               [(and (string:prefix? "?" text)
                     (= (param parameters 0 0) 996))
                ;; Color-scheme query: answerable only once the host has
                ;; reported a scheme; silence matches a host without the
                ;; feature.
                (when (state-color-scheme state)
                  (terminal-reply!
                    state (color-scheme-report (state-color-scheme state))))]
               [else
                (report-unsupported!
                  state (control-signature "CSI" text final))])]
            [(#\c)
             (cond [(or (string=? text "") (string=? text "0"))
                    (primary-device-attributes! state)]
                   [(or (string=? text ">") (string=? text ">0"))
                    (terminal-reply! state "\x1b;[>0;276;0c")]
                   [(or (string=? text "=") (string=? text "=0"))
                    (terminal-reply! state "\x1b;P!|00000000\x1b;\\")]
                   [else
                    (report-unsupported!
                      state (control-signature "CSI" text final))])]
            [(#\x)
             (if (memv (param parameters 0 0) '(0 1))
                 (terminal-reply!
                   state
                   (format "\x1b;[~a;1;1;128;128;1;0x"
                           (if (= (param parameters 0 0) 0) 2 3)))
                 (report-unsupported!
                   state (control-signature "CSI" text final)))]
            [(#\i)
             (case (param parameters 0 0)
               [(0)
                (let ([printed
                       (apply string-append
                              (map (lambda (line)
                                     (string-append (cell-row->string line)
                                                    "\r\n"))
                                   (vector->list
                                     (terminal-state-screen state))))])
                  (append-printer-output! state printed))]
               [(4)
                (terminal-state-printer-controller-set! state #f)
                (terminal-state-printer-pending-set! state "")]
               [(5)
                (terminal-state-printer-controller-set! state #t)
                (terminal-state-printer-pending-set! state "")]
               [else
                (report-unsupported!
                  state (control-signature "CSI" text final))])]
            [(#\q)
             (cond
               [(string:prefix? ">" text)
                ;; XTVERSION: report the terminal's name so probing
                ;; programs learn what they are talking to.
                (if (= (param parameters 0 0) 0)
                    (terminal-reply! state "\x1b;P>|e\x1b;\\")
                    (report-unsupported!
                      state (control-signature "CSI" text final)))]
               [cursor-shape?
                (set-cursor-shape!
                  state
                  (case (param parameters 0 0)
                    [(0 1) 'blinking-block]
                    [(2) 'block]
                    [(3) 'blinking-underline]
                    [(4) 'underline]
                    [(5) 'blinking-bar]
                    [(6) 'bar]
                    [else 'blinking-block]))])]
            [else
             (report-unsupported!
               state (control-signature "CSI" text final))]))
      ;; DECDWL and DECDHL rows address only half the columns.
      (when (memv final '(#\A #\B #\C #\D #\E #\F #\G #\H
                          #\d #\e #\f #\`))
        (clamp-to-row-columns! state))))

  (define line-drawing
    '((#\_ . #\space) (#\` . #\x25c6) (#\a . #\x2592) (#\b . #\x2409)
      (#\c . #\x240c) (#\d . #\x240d) (#\e . #\x240a) (#\f . #\x00b0)
      (#\g . #\x00b1) (#\h . #\x2424) (#\i . #\x240b) (#\j . #\x2518)
      (#\k . #\x2510) (#\l . #\x250c)
      (#\m . #\x2514) (#\n . #\x253c) (#\o . #\x23ba) (#\p . #\x23bb)
      (#\q . #\x2500) (#\r . #\x23bc) (#\s . #\x23bd) (#\t . #\x251c)
      (#\u . #\x2524) (#\v . #\x2534) (#\w . #\x252c) (#\x . #\x2502)
      (#\y . #\x2264) (#\z . #\x2265) (#\{ . #\x03c0) (#\| . #\x2260)
      (#\} . #\x00a3) (#\~ . #\x00b7)))

  ;; DEC national replacement character sets: each replaces a handful of
  ;; ASCII positions, as tabulated by xterm.
  (define national-character-sets
    '((british (#\# . #\x00a3))
      (dutch (#\# . #\x00a3) (#\@ . #\x00be) (#\[ . #\x0133)
             (#\\ . #\x00bd) (#\] . #\|) (#\{ . #\x00a8)
             (#\| . #\x0192) (#\} . #\x00bc) (#\~ . #\x00b4))
      (finnish (#\[ . #\x00c4) (#\\ . #\x00d6) (#\] . #\x00c5)
               (#\^ . #\x00dc) (#\` . #\x00e9) (#\{ . #\x00e4)
               (#\| . #\x00f6) (#\} . #\x00e5) (#\~ . #\x00fc))
      (french (#\# . #\x00a3) (#\@ . #\x00e0) (#\[ . #\x00b0)
              (#\\ . #\x00e7) (#\] . #\x00a7) (#\{ . #\x00e9)
              (#\| . #\x00f9) (#\} . #\x00e8) (#\~ . #\x00a8))
      (french-canadian (#\@ . #\x00e0) (#\[ . #\x00e2) (#\\ . #\x00e7)
                       (#\] . #\x00ea) (#\^ . #\x00ee) (#\` . #\x00f4)
                       (#\{ . #\x00e9) (#\| . #\x00f9) (#\} . #\x00e8)
                       (#\~ . #\x00fb))
      (german (#\@ . #\x00a7) (#\[ . #\x00c4) (#\\ . #\x00d6)
              (#\] . #\x00dc) (#\{ . #\x00e4) (#\| . #\x00f6)
              (#\} . #\x00fc) (#\~ . #\x00df))
      (italian (#\# . #\x00a3) (#\@ . #\x00a7) (#\[ . #\x00b0)
               (#\\ . #\x00e7) (#\] . #\x00e9) (#\` . #\x00f9)
               (#\{ . #\x00e0) (#\| . #\x00f2) (#\} . #\x00e8)
               (#\~ . #\x00ec))
      (norwegian-danish (#\@ . #\x00c4) (#\[ . #\x00c6) (#\\ . #\x00d8)
                        (#\] . #\x00c5) (#\^ . #\x00dc) (#\` . #\x00e4)
                        (#\{ . #\x00e6) (#\| . #\x00f8) (#\} . #\x00e5)
                        (#\~ . #\x00fc))
      (spanish (#\# . #\x00a3) (#\@ . #\x00a7) (#\[ . #\x00a1)
               (#\\ . #\x00d1) (#\] . #\x00bf) (#\{ . #\x00b0)
               (#\| . #\x00f1) (#\} . #\x00e7))
      (swedish (#\@ . #\x00c9) (#\[ . #\x00c4) (#\\ . #\x00d6)
               (#\] . #\x00c5) (#\^ . #\x00dc) (#\` . #\x00e9)
               (#\{ . #\x00e4) (#\| . #\x00f6) (#\} . #\x00e5)
               (#\~ . #\x00fc))
      (swiss (#\# . #\x00f9) (#\@ . #\x00e0) (#\[ . #\x00e9)
             (#\\ . #\x00e7) (#\] . #\x00ea) (#\^ . #\x00ee)
             (#\_ . #\x00e8) (#\` . #\x00f4) (#\{ . #\x00e4)
             (#\| . #\x00f6) (#\} . #\x00fc) (#\~ . #\x00fb))))

  (define charset-designations
    '((#\0 . line) (#\2 . line) (#\1 . ascii) (#\B . ascii)
      (#\A . british) (#\4 . dutch) (#\C . finnish) (#\5 . finnish)
      (#\R . french) (#\f . french) (#\Q . french-canadian)
      (#\9 . french-canadian) (#\K . german) (#\Y . italian)
      (#\E . norwegian-danish) (#\6 . norwegian-danish)
      (#\` . norwegian-danish) (#\Z . spanish) (#\H . swedish)
      (#\7 . swedish) (#\= . swiss)))

  (define (mapped-character state character)
    (let ([set (if (= (terminal-state-shift state) 0)
                   (terminal-state-charset state)
                   (terminal-state-charset-g1 state))])
      (cond
        [(eq? set 'line)
         (cond [(assv character line-drawing) => cdr] [else character])]
        [(assq set national-character-sets) =>
         (lambda (entry)
           (cond [(assv character (cdr entry)) => cdr] [else character]))]
        [else character])))

  (define (next-tab-stop state)
    (let ([cols (terminal-state-cols state)]
          [stops (terminal-state-tab-stops state)])
      (let loop ([col (+ (terminal-state-col state) 1)])
        (cond [(>= col cols) (- cols 1)]
              [(vector-ref stops col) col]
              [else (loop (+ col 1))]))))

  ;; The virtual printer accumulates until the controller is turned off, so
  ;; a chunked representation keeps appends cheap and a ceiling keeps a child
  ;; that never sends CSI 4i from growing the capture without bound.
  (define printer-output-limit 1048576)

  (define (append-printer-output! state text)
    (let* ([output (terminal-state-printer-output state)]
           [room (- printer-output-limit (car output))]
           [taken (min (string-length text) (max 0 room))])
      (when (> taken 0)
        (terminal-state-printer-output-set!
          state
          (cons (+ (car output) taken)
                (cons (if (= taken (string-length text))
                          text (substring text 0 taken))
                      (cdr output)))))))

  (define (printer-output-text state)
    (apply string-append
      (reverse (cdr (terminal-state-printer-output state)))))

  (define printer-controller-terminators '("\x1b;[4i" "\x9b;4i"))

  (define (string-prefix-of? prefix text)
    (and (<= (string-length prefix) (string-length text))
         (string=? prefix (substring text 0 (string-length prefix)))))

  (define (printer-prefix? text)
    (exists (lambda (terminator) (string-prefix-of? text terminator))
            printer-controller-terminators))

  (define (printer-feed-character! state character)
    (let ([candidate
           (string-append (terminal-state-printer-pending state)
                          (string character))])
      (cond
        [(member candidate printer-controller-terminators)
         (terminal-state-printer-controller-set! state #f)
         (terminal-state-printer-pending-set! state "")]
        [(printer-prefix? candidate)
         (terminal-state-printer-pending-set! state candidate)]
        [else
         (append-printer-output! state candidate)
         (terminal-state-printer-pending-set! state "")])))

  (define (execute-c0! state code)
    (case code
      [(7) (terminal-state-bell-set! state #t)]
      [(8)
       (terminal-state-wrap-pending-set! state #f)
       (if (and (memv 45 (terminal-state-extra-modes state))
                (terminal-state-autowrap state)
                (= (terminal-state-col state) (left-bound state))
                (> (terminal-state-row state)
                   (terminal-state-scroll-top state)))
           ;; Reverse wraparound: xterm backs the cursor up onto the end
           ;; of the previous line.
           (begin
             (terminal-state-row-set!
               state (- (terminal-state-row state) 1))
             (terminal-state-col-set! state (right-bound state)))
           (terminal-state-col-set!
             state
             (max (if (<= (left-bound state) (terminal-state-col state)
                          (right-bound state))
                      (left-bound state) 0)
                  (- (terminal-state-col state) 1))))]
      [(9)
       (terminal-state-col-set! state (next-tab-stop state))
       (clamp-to-row-columns! state)]
      [(10 11 12)
       (terminal-state-wrap-pending-set! state #f)
       (vector-set! (terminal-state-wrapped state)
                    (terminal-state-row state) #f)
       (line-feed! state)]
      [(13)
       (terminal-state-wrap-pending-set! state #f)
       (terminal-state-col-set! state (left-bound state))]
      [(14) (terminal-state-shift-set! state 1)]
      [(15) (terminal-state-shift-set! state 0)]))

  (define (feed-character! state character)
    (if (terminal-state-printer-controller state)
        (printer-feed-character! state character)
        (feed-display-character! state character)))

  (define (feed-display-character! state character)
    (case (terminal-state-parser state)
      [(normal)
       (case (char->integer character)
         [(7 8 9 10 11 12 13 14 15)
          (execute-c0! state (char->integer character))]
         [(27) (terminal-state-parser-set! state 'escape)]
         [(132) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state)]                          ; IND
         [(133) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state)                           ; NEL
          (terminal-state-col-set! state (left-bound state))]
         [(136) (vector-set! (terminal-state-tab-stops state)
                             (terminal-state-col state) #t)] ; HTS
         [(141) (if (= (terminal-state-row state)
                       (terminal-state-scroll-top state))
                    (scroll-down! state 1)
                    (terminal-state-row-set!
                      state (max 0 (- (terminal-state-row state) 1))))] ; RI
         [(144)                                      ; DCS
          (terminal-state-parser-set! state 'dcs)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state (empty-control-text))]
         [(152 158 159)                              ; SOS, PM, APC
          (report-unsupported!
            state
            (case (char->integer character)
              [(152) "SOS control string"]
              [(158) "PM control string"]
              [else "APC control string"]))
          (terminal-state-parser-set! state 'control-string)
          (terminal-state-osc-escape-set! state #f)]
         [(155) (terminal-state-parser-set! state 'csi) ; CSI
          (terminal-state-parameters-set! state "")]
         [(157) (terminal-state-parser-set! state 'osc) ; OSC
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state (empty-control-text))]
         [else
          (let ([code (char->integer character)])
            ;; DEL is a padding character and never occupies a cell.
            (when (and (>= code 32) (not (= code 127))
                       (not (<= 128 code 159)))
              (put-character! state (mapped-character state character)))
            (when (and (< code 32) (not (memv code '(0 1 2 3 4 5 6
                                                     7 8 9 10 11 12 13
                                                     14 15 24 26 27))))
              (report-unsupported! state (format "C0 control 0x~x" code)))
            ;; A stray ST (0x9c) legally terminates nothing; every other
            ;; unhandled C1 control names a missing capability.
            (when (and (<= 128 code 159) (not (= code 156)))
              (report-unsupported!
                state (format "C1 control 0x~x" code))))])]
      [(escape)
       (case character
         [(#\[) (terminal-state-parser-set! state 'csi)
          (terminal-state-parameters-set! state "")]
         [(#\]) (terminal-state-parser-set! state 'osc)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state (empty-control-text))]
         ;; String controls carry arbitrary printable payload terminated by
         ;; ST (ESC \). They are metadata/protocol traffic, never screen text.
         [(#\P)
          (terminal-state-parser-set! state 'dcs)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state (empty-control-text))]
         [(#\X #\^ #\_)
          (report-unsupported!
            state
            (case character [(#\X) "SOS control string"]
              [(#\^) "PM control string"]
              [else "APC control string"]))
          (terminal-state-parser-set! state 'control-string)
          (terminal-state-osc-escape-set! state #f)]
         [(#\7) (save-cursor! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\8) (restore-cursor! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\D) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state) (terminal-state-parser-set! state 'normal)]
         [(#\E) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state)
          (terminal-state-col-set! state 0)
          (terminal-state-parser-set! state 'normal)]
         [(#\H)
          (vector-set! (terminal-state-tab-stops state)
                       (terminal-state-col state) #t)
          (terminal-state-parser-set! state 'normal)]
         [(#\M) (if (= (terminal-state-row state)
                       (terminal-state-scroll-top state))
                    (scroll-down! state 1)
                    (terminal-state-row-set!
                      state (max 0 (- (terminal-state-row state) 1))))
          (terminal-state-parser-set! state 'normal)]
         [(#\c) (reset-terminal-state! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\Z) (primary-device-attributes! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\=) (terminal-state-keypad-set! state #t)
          (terminal-state-parser-set! state 'normal)]
         [(#\>) (terminal-state-keypad-set! state #f)
          (terminal-state-parser-set! state 'normal)]
         [(#\#) (terminal-state-parser-set! state 'escape-hash)]
         [(#\space) (terminal-state-parser-set! state 'escape-space)]
         [(#\%) (terminal-state-parser-set! state 'escape-percent)]
         [(#\l)
          ;; Lock rows above the cursor; the cursor row remains the first
          ;; scrollable row, as specified by xterm's memory-lock capability.
          (terminal-state-memory-lock-set! state (terminal-state-row state))
          (terminal-state-parser-set! state 'normal)]
         [(#\m)
          (terminal-state-memory-lock-set! state #f)
          (terminal-state-parser-set! state 'normal)]
         [(#\() (terminal-state-charset-target-set! state 0)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [(#\)) (terminal-state-charset-target-set! state 1)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [(#\*) (terminal-state-charset-target-set! state 2)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [(#\+) (terminal-state-charset-target-set! state 3)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [(#\-) (terminal-state-charset-target-set! state 1)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [(#\.) (terminal-state-charset-target-set! state 2)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [(#\/) (terminal-state-charset-target-set! state 3)
          (terminal-state-parameters-set! state "")
          (terminal-state-parser-set! state 'charset)]
         [else
          (if (char<=? #\space character #\/)
              ;; An unhandled intermediate opens a longer sequence; consume
              ;; it wholly so its final byte is not painted as text.
              (begin
                (terminal-state-parameters-set! state (string character))
                (terminal-state-parser-set! state 'escape-unknown))
              (begin
                (report-unsupported!
                  state (control-signature "ESC" "" character))
                (terminal-state-parser-set! state 'normal)))])]
      [(escape-unknown)
       (if (char<=? #\space character #\/)
           (terminal-state-parameters-set!
             state
             (string-append (terminal-state-parameters state)
                            (string character)))
           (begin
             (report-unsupported!
               state
               (control-signature "ESC" (terminal-state-parameters state)
                                  character))
             (terminal-state-parameters-set! state "")
             (terminal-state-parser-set! state 'normal)))]
      [(escape-space)
       (case character
         [(#\F) (terminal-state-controls-eight-bit-set! state #f)]
         [(#\G) (terminal-state-controls-eight-bit-set! state #t)]
         [else
          (report-unsupported!
            state (control-signature "ESC" " " character))])
       (terminal-state-parser-set! state 'normal)]
      [(escape-hash)
       (case character
         [(#\8) (screen-alignment-test! state)]
         [(#\3 #\4 #\5 #\6)
          (vector-set! (terminal-state-line-attributes state)
                       (terminal-state-row state)
                       (case character
                         [(#\3) 'top] [(#\4) 'bottom]
                         [(#\6) 'wide] [else 'single]))
          (clamp-to-row-columns! state)
          (terminal-state-dirty-set! state #t)]
         [else
          (report-unsupported!
            state (control-signature "ESC" "#" character))])
       (terminal-state-parser-set! state 'normal)]
      [(escape-percent)
       ;; ESC % G selects UTF-8, the permanent state here; switching to
       ;; another coded character set is not possible.
       (unless (char=? character #\G)
         (report-unsupported!
           state (control-signature "ESC" "%" character)))
       (terminal-state-parser-set! state 'normal)]
      [(charset)
       (if (char<=? #\space character #\/)
           (terminal-state-parameters-set!
             state
             (string-append (terminal-state-parameters state)
                            (string character)))
           (let ([designation
                  (cond [(assv character charset-designations) => cdr]
                        [else 'ascii])])
             ;; An unrecognized character set silently degrades to ASCII;
             ;; name it so the wrong glyphs are traceable to the gap.
             (when (or (> (string-length
                            (terminal-state-parameters state))
                          0)
                       (not (assv character charset-designations)))
               (report-unsupported!
                 state
                 (format "G~a charset designator ~s"
                         (terminal-state-charset-target state)
                         (string-append (terminal-state-parameters state)
                                        (string character)))))
             ;; G2 and G3 designations must be parsed even though e does not
             ;; currently invoke those banks into GL.  Otherwise their final
             ;; byte is painted as ordinary text (vttest exposes this as BB).
             (case (terminal-state-charset-target state)
               [(0) (terminal-state-charset-set! state designation)]
               [(1) (terminal-state-charset-g1-set! state designation)])
             (terminal-state-parser-set! state 'normal)
             (terminal-state-parameters-set! state "")))]
      [(csi)
       (cond [(memv (char->integer character) '(24 26))
              (terminal-state-parser-set! state 'normal)
              (terminal-state-parameters-set! state "")]
             [(char=? character #\esc)
              (terminal-state-parser-set! state 'escape)
              (terminal-state-parameters-set! state "")]
             [(< (char->integer character) 32)
              ;; ECMA-48 C0 controls execute immediately inside a control
              ;; sequence without cancelling it or becoming parameter text.
              (execute-c0! state (char->integer character))]
             [(char<=? #\@ character #\~)
              (dispatch-csi! state character
                             (terminal-state-parameters state))
              (terminal-state-parser-set! state 'normal)]
             [(< (string-length (terminal-state-parameters state)) 1024)
              (terminal-state-parameters-set!
                state (string-append (terminal-state-parameters state)
                                     (string character)))]
             [else
              (terminal-state-parser-set! state 'normal)
              (terminal-state-parameters-set! state "")])]
      [(osc)
       (cond [(memv (char->integer character) '(7 156)) ; BEL or ST
              (dispatch-osc! state)
              (terminal-state-parser-set! state 'normal)]
             [(and (terminal-state-osc-escape state) (char=? character #\\))
              (dispatch-osc! state)
              (terminal-state-parser-set! state 'normal)
              (terminal-state-osc-escape-set! state #f)]
             [else
              (if (char=? character #\esc)
                  (terminal-state-osc-escape-set! state #t)
                  (begin
                    (terminal-state-osc-escape-set! state #f)
                    ;; Clipboard payloads are base64 and routinely exceed a
                    ;; status string. Match tmux's practical one-megabyte
                    ;; control-string ceiling rather than truncating normal
                    ;; copied regions at 8 KiB.
                    (unless (control-text-add! state 1048576 character)
                      (terminal-state-parser-set! state 'normal)
                      (terminal-state-osc-text-set!
                        state (empty-control-text)))))])]
      [(control-string)
       (cond
         [(memv (char->integer character) '(24 26))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(= (char->integer character) 156) ; ST
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(and (terminal-state-osc-escape state) (char=? character #\\))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [else
          (terminal-state-osc-escape-set! state
                                          (char=? character #\esc))])]
      [(dcs)
       (cond
         [(memv (char->integer character) '(24 26))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state (empty-control-text))]
         [(= (char->integer character) 156) ; ST
          (dispatch-dcs! state)
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(and (terminal-state-osc-escape state) (char=? character #\\))
          (dispatch-dcs! state)
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [else
          (if (char=? character #\esc)
              (terminal-state-osc-escape-set! state #t)
              (begin
                (terminal-state-osc-escape-set! state #f)
                (unless (control-text-add! state 8192 character)
                  (terminal-state-parser-set! state 'normal)
                  (terminal-state-osc-text-set!
                    state (empty-control-text)))))])]))

  (define (capture-rendition state cells styles)
    (make-rendition cells (effective-style-row state styles)
                    (vector-map cell-style-link styles) (cell-row->string cells)))

  (edoc "One frame of the emulator: the scrollback rendition from the state's cache, filled as needed, and the live rows copied; the main screen's scrollback is left out unless transcript? says otherwise."
        (state (record terminal-state) "the emulator")
        (transcript? (list-of boolean) "whether the main screen's scrollback counts, at most one")
        (returns (record frame))
        (effects internal))
  (define (capture-frame state . transcript?)
    ;; Caller holds the emulator lock. Unchanged scrollback rendition is
    ;; shared privately; live vectors are copied before the writer resumes.
    (let* ([entries (if (and (terminal-state-main-screen state)
                             (not (and (pair? transcript?) (car transcript?)))) '()
                      (scrollback-entries (terminal-state-history state)))]
           [table (terminal-state-cache state)]
           [history
            (map (lambda (entry)
                   (or (hashtable-ref table entry #f)
                       (let ([row (capture-rendition state (scrollback-line-cells entry)
                                                     (scrollback-line-styles entry))])
                         (hashtable-set! table entry row) row))) entries)]
           [screen
            (map (lambda (row)
                   (let-values ([(cells styles fresh?) (displayed-row state row)])
                     (capture-rendition state (if fresh? cells (vector-copy cells)) styles)))
                 (screen-row-indexes state))]
           [point (state-cursor-position state)]
           [top (length history)])
      (make-frame (list->vector (append history screen))
                  (list (+ top (terminal-state-row state)) (cdr point) (terminal-state-cursor-visible state))
                  (list (terminal-state-rows state) (terminal-state-cols state))
                  top (terminal-state-cursor-shape state))))

  (define (cell-clusters cells)
    (let scan ([at 0] [out '()])
      (if (= at (vector-length cells)) (reverse out)
          (let ([end (let run ([end (+ at 1)])
                       (if (and (< end (vector-length cells))
                                (string=? (vector-ref cells end) ""))
                           (run (+ end 1)) end))])
            (scan end (cons (cons (string-length (vector-ref cells at)) (- end at)) out))))))

  (edoc "One owned (text rows cursor size facts) snapshot for the surface publisher, or #f during a synchronized update."
        (emulator (record terminal-state) "the emulator")
        (returns (or list #f)))
  (define (terminal-emulator-frame emulator)
    ;; One owned (text rows cursor size facts) snapshot, ready for the
    ;; store/surface publisher. A child composing a synchronized frame gets
    ;; its existing bounded hold; older inspection APIs still read raw state.
    (read-emulator emulator 'emulator-frame
      (lambda ()
        (and (not (synchronized-update-pending? emulator))
             (let* ([frame (capture-frame emulator)] [rows (frame-rows frame)])
               (list (vector-map rendition-text rows)
                     (map (lambda (i) (surface-row i (vector-ref rows i)))
                          (iota (vector-length rows)))
                     (frame-cursor frame) (frame-size frame)
                     (list (cons 'cursor-style (frame-cursor-shape frame)))))))))

  (define (surface-row index row)
    (list index (rendition-styles row) (rendition-links row)
          (list (cons 'clusters (cell-clusters (rendition-cells row))))))

  (define (write-bytes! state bytes)
    (when (terminal-state-alive state)
      (let ([output (sys:terminal-process-output (terminal-state-process state))])
        (put-bytevector output bytes)
        (flush-output-port output))))

  (define (send-paste! state text)
    ;; Pasted text is data. Control characters other than plain
    ;; whitespace could act as typed escape sequences or forge the
    ;; bracketed-paste closer, so strip them, as modern terminals do.
    (let ([clean (list->string
                   (filter
                     (lambda (character)
                       (or (memv (char->integer character) '(9 10 13))
                           (printable-character? character)))
                     (string->list text)))])
      (write-bytes!
        state
        (string->utf8
          (if (terminal-state-bracketed state)
              (string-append "\x1b;[200~" clean "\x1b;[201~")
              clean)))))

  (define (control-byte letter)
    (bytevector (- (char->integer (char-upcase letter)) 64)))

  (define (meta-bytes state bytes)
    (if (and (terminal-state-meta-eight-bit state)
             (= (bytevector-length bytes) 1)
             (< (bytevector-u8-ref bytes 0) 128))
        (bytevector (bitwise-ior 128 (bytevector-u8-ref bytes 0)))
        (bytes-append (bytevector 27) bytes)))

  (define (bytes-append left right)
    (let ([result (make-bytevector (+ (bytevector-length left)
                                      (bytevector-length right)))])
      (bytevector-copy! left 0 result 0 (bytevector-length left))
      (bytevector-copy! right 0 result (bytevector-length left)
                        (bytevector-length right))
      result))

  (define key-modifiers
    '(("C-M-S-" . 8) ("C-M-" . 7) ("C-S-" . 6) ("M-S-" . 4)
      ("C-" . 5) ("M-" . 3) ("S-" . 2)))

  (define (modified-key event)
    (find (lambda (entry) (string:prefix? (car entry) event)) key-modifiers))

  (define (function-key-number event)
    (and (> (string-length event) 1)
         (char=? (string-ref event 0) #\F)
         (string->number (substring event 1 (string-length event)))))

  (define function-key-codes '#(0 0 0 0 0 15 17 18 19 20 21 23 24))

  (define (function-key-base-bytes base modifier)
    (string->utf8
      (cond [(<= base 4)
             (if (= modifier 1)
                 (format "\x1b;O~c" (integer->char (+ 79 base)))
                 (format "\x1b;[1;~a~c" modifier
                         (integer->char (+ 79 base))))]
            [(= modifier 1)
             (format "\x1b;[~a~~" (vector-ref function-key-codes base))]
            [else
             (format "\x1b;[~a;~a~~"
                     (vector-ref function-key-codes base) modifier)])))

  (define (function-key-bytes number)
    (let-values ([(base modifier)
                  (cond [(<= 1 number 12) (values number 1)]
                        [(<= 13 number 24) (values (- number 12) 2)]
                        [(<= 25 number 36) (values (- number 24) 5)]
                        [(<= 37 number 48) (values (- number 36) 6)]
                        [(<= 49 number 60) (values (- number 48) 3)]
                        [(<= 61 number 63) (values (- number 60) 4)]
                        [else (values #f #f)])])
      (and base (function-key-base-bytes base modifier))))

  (define (named-key-bytes state event)
    (let* ([modified (modified-key event)]
           [modifier (if modified (cdr modified) 1)]
           [base (if modified
                     (substring event (string-length (car modified))
                                (string-length event))
                     event)]
           [cursor-final (assoc base '(("UP" . "A") ("DOWN" . "B")
                                       ("RIGHT" . "C") ("LEFT" . "D")
                                       ("HOME" . "H") ("END" . "F")
                                       ("BEGIN" . "E")))]
           [tilde-code (assoc base '(("INSERT" . 2) ("DELETE" . 3)
                                     ("PAGEUP" . 5) ("PAGEDOWN" . 6)))]
           [function (function-key-number base)])
      (cond [cursor-final
             (string->utf8
               (if (and (= modifier 1) (string=? base "BEGIN"))
                   "\x1b;OE"
                   (if (= modifier 1)
                     (format "\x1b;~a~a"
                             (if (terminal-state-cursor-keys state) "O" "[")
                             (cdr cursor-final))
                     (format "\x1b;[1;~a~a" modifier
                             (cdr cursor-final)))))]
            [tilde-code
             (string->utf8
               (if (= modifier 1)
                   (format "\x1b;[~a~~" (cdr tilde-code))
                   (format "\x1b;[~a;~a~~" (cdr tilde-code) modifier)))]
            [(and function (<= 1 function 12))
             (function-key-base-bytes function modifier)]
            [else #f])))

  (define keypad-keys
    '(("KP-0" "0" . "p") ("KP-1" "1" . "q")
      ("KP-2" "2" . "r") ("KP-3" "3" . "s")
      ("KP-4" "4" . "t") ("KP-5" "5" . "u")
      ("KP-6" "6" . "v") ("KP-7" "7" . "w")
      ("KP-8" "8" . "x") ("KP-9" "9" . "y")
      ("KP-DECIMAL" "." . "n") ("KP-DIVIDE" "/" . "o")
      ("KP-MULTIPLY" "*" . "j") ("KP-SUBTRACT" "-" . "m")
      ("KP-ADD" "+" . "k") ("KP-COMMA" "," . "l")
      ("KP-EQUAL" "=" . "X") ("KP-ENTER" "\r" . "M")))

  (define (keypad-bytes state event)
    (let ([entry (assoc event keypad-keys)])
      (and entry
           (string->utf8
             (if (terminal-state-keypad state)
                 (string-append "\x1b;O" (cddr entry))
                 (if (and (string=? event "KP-ENTER")
                          (terminal-state-newline state))
                     "\r\n" (cadr entry)))))))

  (define (event-bytes state event)
    (cond
      [(and (terminal-state-focus-reporting state)
            (member event '("FOCUS" "BLUR")))
       (string->utf8 (if (string=? event "FOCUS") "\x1b;[I" "\x1b;[O"))]
      [(= (string-length event) 1) (string->utf8 event)]
      [(function-key-number event) => function-key-bytes]
      [(named-key-bytes state event) => values]
      [(keypad-bytes state event) => values]
      [(string:prefix? "C-M-" event)
       (let ([base (substring event 4 (string-length event))])
         (and (= (string-length base) 1)
              (meta-bytes state (control-byte (string-ref base 0)))))]
      [(string:prefix? "M-" event)
       (let ([bytes (event-bytes
                      state (substring event 2 (string-length event)))])
         (and bytes (meta-bytes state bytes)))]
      [(and (string:prefix? "C-" event) (= (string-length event) 3))
       (control-byte (string-ref event 2))]
      [else
       (cond [(string=? event "RET")
              (string->utf8
                (if (terminal-state-newline state) "\r\n" "\r"))]
             [(assoc event
                     '(("TAB" . "\t")
                       ("BACKSPACE" . "\x7f;") ("ESC" . "\x1b;")
                       ("S-TAB" . "\x1b;[Z")
                       ;; The editor names a space "SPC" only under a
                       ;; modifier, so this entry makes M-SPC send ESC SP.
                       ("SPC" . " ")))
              => (lambda (entry) (string->utf8 (cdr entry)))]
             [else #f])]))

  (define (mouse-bytes state code x y release?)
    (and (not (and (eqv? (terminal-state-mouse state) 9) release?))
      (let ([button (if release? 3 code)])
        (cond
          [(terminal-state-mouse-sgr state)
           (string->utf8
             (format "\x1b;[<~a;~a;~a~a"
                     code x y (if release? "m" "M")))]
          [(terminal-state-mouse-urxvt state)
           (string->utf8 (format "\x1b;[~a;~a;~aM" (+ 32 button) x y))]
          [(terminal-state-mouse-utf8 state)
           (string->utf8
             (string-append "\x1b;[M"
                            (string (integer->char (+ 32 button))
                                    (integer->char (+ 32 x))
                                    (integer->char (+ 32 y)))))]
          [else
           ;; The original X10 encoding is limited to coordinates below 223.
           (bytevector 27 91 77
                       (+ 32 button)
                       (+ 32 (min x 223))
                       (+ 32 (min y 223)))]))))

  (edoc "The bytes a mouse event sends to the program, under the emulator's mouse modes."
        (emulator (record terminal-state) "the emulator")
        (code integer "the button code")
        (x integer "the column")
        (y integer "the row")
        (release? boolean "whether the button was released")
        (returns string))
  (define (terminal-emulator-mouse-input emulator code x y release?)
    (unless (and (integer? code) (integer? x) (> x 0)
                 (integer? y) (> y 0) (boolean? release?))
      (error 'terminal-emulator-mouse-input
             "expected a button code, positive coordinates, and release flag"
             code x y release?))
    (read-emulator emulator 'emulator-mouse-input
      (lambda () (and (terminal-state-mouse emulator) (mouse-bytes emulator code x y release?)))))


  ;;; Base app ownership -------------------------------------------------------

  (define-record-type runtime
    (nongenerative e-vt-runtime-v1)
    (fields lock (mutable serial) (mutable apps) (mutable store-token)))
  (define live (unbox (kernel:persistent-cell 'vt-runtime
                        (lambda () (make-runtime (make-mutex) 0 '() #f)))))

  (define (instances) (with-mutex (runtime-lock live) (runtime-apps live)))

  (edoc "A saved VT buffer's snapshot as a read-only transcript, for the session file."
        (snapshot list "the buffer snapshot")
        (returns list))
  (define (transcript snapshot)
    ;; Convert only a VT-owned source, including one whose process already
    ;; ended. Save the last accepted store text, not concurrent emulator data.
    ;; The mode stays, as it does when the process ends in a session: the
    ;; transcript reads (terminal) on its bar, its capture context inactive
    ;; without a live app either way.
    (let* ([facts (list-ref snapshot 4)] [app (cond [(assq 'app facts) => cdr] [else #f])])
      (if (and (list? app) (= (length app) 3) (equal? (list-head app 2) '(app terminal)))
          (append (list-head snapshot 4)
            (list (cons* '(disposable . #f) '(read-only . #t)
                    (filter (lambda (entry) (not (memq (car entry) '(disposable read-only)))) facts))))
          snapshot)))

  ;; Monotonic owner identities distinguish replacement processes. Copy the
  ;; inventory first, then inspect each emulator without nesting its locks.
  (edoc "The owner identities of the terminals whose processes still run."
        (returns list))
  (define (running)
    (filter values
      (map (lambda (state)
             (with-mutex (terminal-state-lock state)
               (and (terminal-state-alive state) (datum:copy (terminal-state-owner state)))))
        (instances))))
  (define (instance id)
    (find (lambda (state) (eqv? id (terminal-state-buffer state))) (instances)))
  (define (fact facts key fallback)
    (cond [(assq key facts) => cdr] [else fallback]))
  (define (size? size)
    (and (list? size) (= (length size) 2)
         (for-all (lambda (n) (and (integer? n) (exact? n) (> n 0))) size)))

  (define (wake! state)
    ;; One pending wake, independent of how many PTY chunks arrive.
    (when (with-mutex (terminal-state-lock state)
            (and (not (terminal-state-queued? state))
                 (begin (terminal-state-queued?-set! state #t) #t)))
      (kernel:mailbox-post! (terminal-state-mailbox state) '(wake))))

  (define (close-state! state)
    ;; Every close path, including a failed publisher or a deleted source,
    ;; waits out a reversible pause. Retirement can finish after commit.
    (activity:call-with-retirement
      (lambda ()
        (with-mutex (terminal-state-lock state) (terminal-state-alive-set! state #f))
        (sys:close-terminal-process! (terminal-state-process state))
        (wake! state))))

  (edoc "Close a terminal by its buffer id, ending its process."
        (id integer "the buffer id"))
  (define (close! id)
    (activity:call-with
      (lambda ()
        (cond [(instance id) => close-state!])
        (void))))

  (edoc "Close every terminal, at base shutdown.")
  (define (close-all!)
    ;; Called by the base runtime owner, independent of head attachment.
    (activity:call-with-retirement (lambda () (for-each close-state! (instances)))))

  (edoc "Tell the terminals the color scheme, dark, light or #f, so their default colors follow it."
        (scheme (or (one-of dark light) #f) "the scheme")
        (source (list-of any) "which head reported it, at most one"))
  (define (terminal-color-scheme! scheme . source)
    (unless (memq scheme '(dark light #f))
      (error 'color-scheme! "expected dark, light, or #f" scheme))
    (set-box! default-scheme scheme)
    (for-each
      (lambda (state)
        (let ([from (if (pair? source) (car source)
                        (with-mutex (terminal-state-lock state) (terminal-state-controller state)))])
          (actor:send! (terminal-state-owner state)
            (list 'request from (terminal-state-buffer state) 'color-scheme scheme))))
      (instances)))

  (define (set-scheme! state scheme)
    (unless (eq? scheme (terminal-state-scheme state))
      (terminal-state-scheme-set! state scheme)
      (when (and scheme (memv 2031 (terminal-state-extra-modes state)))
        (write-bytes! state (string->utf8 (color-scheme-report scheme))))))

  (define (input-data? data event)
    (and (list? data) (for-all (lambda (entry) (and (pair? entry) (symbol? (car entry)))) data)
         (size? (fact data 'size #f))
         (memq (fact data 'color-scheme #f) '(dark light #f))
         (or (not (member event '("PASTE" "TEXT")))
             (string? (fact data (if (string=? event "PASTE") 'paste 'text) #f)))
         (let ([cell (fact data 'cell #f)] [button (fact data 'button #f)])
           (and (or (not cell) (and (pair? cell)
                                    (for-all (lambda (n) (and (integer? n) (exact? n) (>= n 0)))
                                             (list (car cell) (cdr cell)))))
                (or (not button) (and (integer? button) (exact? button) (<= 0 button 223)))))))

  (define (valid-message? state message)
    (and (list? message) (= (length message) 5) (actor:identity? (cadr message))
         (eqv? (caddr message) (terminal-state-buffer state))
         (let ([what (cadddr message)] [data (list-ref message 4)])
           (case (car message)
             [(input) (and (string? what) (input-data? data what))]
             [(request) (case what [(resize) (size? data)] [(close) (null? data)]
                          [(color-scheme) (memq data '(dark light #f))] [else #f])]
             [else #f]))))

  (edoc "Send text to a terminal's program as typed or pasted input, with the sender's screen size."
        (from head "the sending head")
        (id integer "the buffer id")
        (text string "the text")
        (size list "(rows cols)")
        (paste? boolean "whether it is a paste")
        (scheme (list-of any) "the color scheme, at most one"))
  (define (send! from id text size paste? . scheme)
    (unless (and (actor:identity? from) (string? text) (size? size) (boolean? paste?))
      (error 'send! "expected actor, text, size, and paste flag"))
    (let ([owner (store:property id 'app)])
      (actor:send! owner
        (list 'input from id (if paste? "PASTE" "TEXT")
              (list (cons (if paste? 'paste 'text) text) (cons 'size size)
                    (cons 'color-scheme (if (pair? scheme) (car scheme) #f)))))))

  (define mouse-events '("MOUSE-CLICK" "MOUSE-DRAG" "MOUSE-RELEASE"
                         "WHEEL-UP" "WHEEL-DOWN" "WHEEL-LEFT" "WHEEL-RIGHT"))

  (define (handle-message! state message)
    (activity:call-with
      (lambda ()
        (let ([from (cadr message)] [what (cadddr message)] [data (list-ref message 4)])
          (if (and (eq? (car message) 'request) (eq? what 'close))
              (close-state! state)
              (with-mutex (terminal-state-lock state)
                (when (terminal-state-alive state)
                  (case (car message)
                    [(request)
                     (when (equal? from (terminal-state-controller state))
                       (case what
                         [(resize) (resize-screen! state (car data) (cadr data))]
                         [(color-scheme) (set-scheme! state data)]))]
                    [(input)
                     (let* ([mouse? (member what mouse-events)] [cell (fact data 'cell #f)]
                            [frame (terminal-state-rendered state)]
                            [y (and mouse? cell frame (+ 1 (- (car cell) (frame-top frame))))])
                       ;; A pointer addresses the published grid, not an arbitrary
                       ;; scrollback row or a newer frame the sender has not seen.
                       (when (or (not mouse?)
                                 (and (terminal-state-mouse state) y
                                      (equal? (fact data 'revision #f) (terminal-state-revision state))
                                      (equal? (fact data 'generation #f) (terminal-state-generation state))
                                      (<= 1 y (car (frame-size frame)))
                                      (< (cdr cell) (cadr (frame-size frame)))))
                         (unless (member what '("FOCUS" "BLUR"))
                           (terminal-state-controller-set! state from)
                           (let ([size (fact data 'size #f)])
                             (resize-screen! state (car size) (cadr size)))
                           (set-scheme! state (fact data 'color-scheme #f)))
                         (cond
                           [mouse?
                            (cond [(mouse-bytes state (or (fact data 'button #f) 0)
                                                (+ 1 (cdr cell)) y (string=? what "MOUSE-RELEASE"))
                                   => (lambda (bytes) (write-bytes! state bytes))])]
                           [(string=? what "PASTE") (send-paste! state (fact data 'paste ""))]
                           [(string=? what "TEXT") (write-bytes! state (string->utf8 (fact data 'text "")))]
                           [(event-bytes state what) => (lambda (bytes) (write-bytes! state bytes))])))]))))))))

  (define (app-facts state)
    (let* ([alive? (terminal-state-alive state)] [mouse (terminal-state-mouse state)]
           [clipboard (terminal-state-clipboard state)])
      `((alive . ,alive?)
        (capture . ,(and alive?
                         (cons 'except
                           (append '("MOUSE" "S-WHEEL-UP" "S-WHEEL-DOWN" "S-WHEEL-LEFT" "S-WHEEL-RIGHT")
                             (cond [(not mouse) mouse-events]
                                   [(< mouse 1002) '("MOUSE-DRAG")] [else '()])))))
        (status . ,(cond [(not alive?) (if (terminal-state-failure state) "■ error" "■")]
                         [(terminal-state-bell-visible state) "♪"] [else "▶"]))
        (cursor-style . ,(terminal-state-cursor-shape state))
        (size . ,(list (terminal-state-rows state) (terminal-state-cols state)))
        (title . ,(terminal-state-title state))
        (clipboard . ,(and clipboard
                           (list (terminal-state-clipboard-sequence state)
                                 (terminal-state-clipboard-target state) clipboard)))
        (diagnostics . ,(append (map (lambda (feature) (string-append "Unsupported " feature)) (unsupported state))
                                (if (terminal-state-failure state) (list (terminal-state-failure state)) '()))))))

  (define (same-row? a b)
    (or (eq? a b)
        (and (equal? (rendition-cells a) (rendition-cells b))
             (equal? (rendition-styles a) (rendition-styles b))
             (equal? (rendition-links a) (rendition-links b)))))

  (define (frame-changes before after)
    (let* ([old (if before (frame-rows before) '#())] [rows (frame-rows after)]
           [count (vector-length rows)])
      (let loop ([i (- (max count (vector-length old)) 1)] [out '()])
        (cond [(< i 0) out]
              [(>= i count) (loop (- i 1) (cons (cons i #f) out))]
              [(and (< i (vector-length old)) (same-row? (vector-ref old i) (vector-ref rows i)))
               (loop (- i 1) out)]
              [else (loop (- i 1) (cons (surface-row i (vector-ref rows i)) out))]))))

  (define (changed-facts id facts)
    (let ([current (store:properties id)])
      (filter (lambda (entry)
                (let ([old (assq (car entry) current)])
                  (or (not old) (not (equal? (cdr old) (cdr entry)))))) facts)))

  (define (publish-output! state final?)
    (activity:call-with
      (lambda ()
        ;; The process may have exited while this publisher waited for resume.
        (set! final? (or final? (with-mutex (terminal-state-lock state) (not (terminal-state-alive state)))))
        (let* ([id (terminal-state-buffer state)] [owner (terminal-state-owner state)]
               [captured
                (with-mutex (terminal-state-lock state)
                  (let ([now (current-time 'time-monotonic)])
                    (when (terminal-state-bell state)
                      (terminal-state-bell-set! state #f)
                      (terminal-state-bell-visible-set! state #t)
                      (terminal-state-bell-deadline-set! state
                                                         (add-duration now (make-time 'time-duration 500000000 0))))
                    (when (and (terminal-state-bell-visible state)
                               (not (time<? now (terminal-state-bell-deadline state))))
                      (terminal-state-bell-visible-set! state #f)
                      (terminal-state-dirty-set! state #t)))
                  (and (or final? (terminal-state-dirty state))
                       (or final? (not (synchronized-update-pending? state)))
                       (begin
                         (terminal-state-dirty-set! state #f)
                         (list (capture-frame state #t) (app-facts state)))))])
          (when (and captured (store:exists? id))
            (let* ([frame (car captured)] [facts (cadr captured)]
                   [text (vector-map rendition-text (frame-rows frame))]
                   [live-facts (changed-facts id
                                              (if final? (filter (lambda (entry) (not (memq (car entry) '(alive capture status)))) facts) facts))]
                   [before (terminal-state-rendered state)]
                   [old-title (store:property id 'title)])
              ;; The live read-only grid is authoritative. A forced external edit
              ;; is reconciled through an attributed edit, never a destructive reset.
              (let-values ([(old revision) (store:snapshot id)])
                (let-values ([(status receipt)
                              (if (equal? old text)
                                  (begin
                                    (when (pair? live-facts) (store:set-properties! owner id live-facts))
                                    (values 'applied (list revision old '())))
                                  (let-values ([(span replacement) (text:difference old text)])
                                    (store:edit-with-snapshot! owner id revision span replacement
                                                               (list (list owner 'output) "terminal output" '() live-facts))))])
                  (cond
                    [(or (not (eq? status 'applied)) (not (equal? text (cadr receipt)))
                         (not (= (car receipt) (store:revision id))))
                     (with-mutex (terminal-state-lock state) (terminal-state-dirty-set! state #t))]
                    [final?
                     ;; Withdraw before announcing death so observers of alive=#f
                     ;; already see ordinary text and no remaining surface layer.
                     (let ([surface (surface:snapshot id)])
                       (surface:withdraw! id (and surface (car surface))))
                     ;; Withdrawal callbacks can also supersede the receipt.
                     (if (and (= (car receipt) (store:revision id)) (not (surface:snapshot id)))
                         (store:set-properties! owner id (changed-facts id facts))
                         (with-mutex (terminal-state-lock state) (terminal-state-dirty-set! state #t)))]
                    [else
                     (let* ([surface (surface:snapshot id)]
                            [same? (equal? (and surface (car surface)) (terminal-state-generation state))]
                            ;; An intervening publisher may have added rows that
                            ;; our previous frame does not describe. Start a full
                            ;; replacement from absence, under the same CAS rule.
                            [basis (if same? (and surface (car surface))
                                       (begin
                                         (when surface (surface:withdraw! id (car surface)))
                                         #f))])
                       (let-values ([(result generation)
                                     (surface:publish! id basis (car receipt)
                                                       (frame-changes (and same? before) frame) (frame-cursor frame) (frame-size frame))])
                         (if (eq? result 'applied)
                             (begin (terminal-state-rendered-set! state frame)
                                    (terminal-state-generation-set! state generation)
                                    (terminal-state-revision-set! state (car receipt)))
                             (with-mutex (terminal-state-lock state) (terminal-state-dirty-set! state #t)))))]))
                (let ([title (fact facts 'title #f)])
                  (when (and title (not (equal? title old-title)))
                    (store:rename! owner id (format "*~a*" title)))))))))))

  (define (retire! state)
    (guard (ex [(activity:stopped? ex) (void)]
               [else
                ;; A failed final commit must not leave a phantom live app.
                ;; Preserve the last accepted text and report the failure as data.
                (guard (ignored [else (void)])
                  (let ([id (terminal-state-buffer state)])
                    (when (store:exists? id)
                      (with-mutex (terminal-state-lock state)
                        (terminal-state-failure-set! state
                          (string-append "Final publication failed: " (kernel:condition-text ex))))
                      (let ([surface (surface:snapshot id)])
                        (surface:withdraw! id (and surface (car surface))))
                      (store:set-properties! (terminal-state-owner state) id
                        (with-mutex (terminal-state-lock state) (app-facts state))))))])
      (let finish ()
        (publish-output! state #t)
        (when (and (store:exists? (terminal-state-buffer state))
                   (with-mutex (terminal-state-lock state) (terminal-state-dirty state)))
          (sleep (make-time 'time-duration 16000000 0))
          (finish))))
    (actor:detach! (terminal-state-owner state))
    (with-mutex (runtime-lock live)
      (runtime-apps-set! live (remq state (runtime-apps live)))))

  (define (publisher-loop state)
    (guard (ex [(activity:stopped? ex) (close-state! state) (retire! state)]
               [else
                (with-mutex (terminal-state-lock state)
                  (terminal-state-failure-set! state (string-append "Publisher failed: " (kernel:condition-text ex))))
                (close-state! state)
                (retire! state)])
      (let loop ([deadline #f])
        (let ([alive? (with-mutex (terminal-state-lock state) (terminal-state-alive state))])
          (if (or (not alive?) (not (store:exists? (terminal-state-buffer state))))
            (begin
              (when alive? (close-state! state))
              (retire! state))
            (let* ([due? (and deadline (not (time<? (current-time 'time-monotonic) deadline)))])
              (when due? (publish-output! state #f))
              (let* ([pending? (with-mutex (terminal-state-lock state)
                                 (or (terminal-state-dirty state) (terminal-state-bell-visible state)))]
                     [next (and pending? (if (and deadline (not due?)) deadline
                                           (add-duration (current-time 'time-monotonic)
                                             (make-time 'time-duration 16000000 0))))]
                     [message (kernel:mailbox-receive! (terminal-state-mailbox state) next)])
                (when message
                  (case (car message)
                    [(wake) (with-mutex (terminal-state-lock state) (terminal-state-queued?-set! state #f))]
                    [(close) (activity:call-with (lambda () (close-state! state)))]
                    [else (handle-message! state message)]))
                (loop next))))))))

  (define (reader-loop state)
    (define (finished! failure)
      (with-mutex (terminal-state-lock state)
        (terminal-state-alive-set! state #f)
        (when failure (terminal-state-failure-set! state failure)))
      (wake! state)
      (guard (ex [else (void)]) (sys:reap-terminal-process! (terminal-state-process state))))
    (guard (ex [else (finished! (and (not (i/o-read-error? ex))
                                  (string-append "Reader failed: " (kernel:condition-text ex))))])
      (let ([input (transcoded-port (sys:terminal-process-input (terminal-state-process state))
                     (make-transcoder (utf-8-codec) 'none 'replace))])
        (let loop ()
          (let ([character (get-char input)])
            (if (eof-object? character) (finished! #f)
                (begin
                  (with-mutex (terminal-state-lock state)
                    (feed-character! state character)
                    (let drain ([remaining 4095])
                      (when (and (> remaining 0) (char-ready? input))
                        (let ([next (get-char input)])
                          (unless (eof-object? next)
                            (feed-character! state next) (drain (- remaining 1))))))
                    (terminal-state-dirty-set! state #t))
                  (wake! state)
                  (loop))))))))

  (edoc "Open a terminal running a command, or the shell, in a directory at a size; its buffer id."
        (from head "the opening head")
        (command (or string #f) "the command line, or #f for the shell")
        (directory directory "the working directory")
        (rows integer "the rows")
        (cols integer "the columns")
        (scheme (list-of any) "the color scheme, at most one")
        (returns integer))
  (define (open! from command directory rows cols . scheme)
    (activity:call-with
      (lambda ()
        (unless (and (actor:identity? from) (or (not command) (string? command))
                     (string? directory) (size? (list rows cols))
                     (or (null? scheme) (memq (car scheme) '(dark light #f))))
          (error 'open! "expected actor, command, directory, size, and optional scheme"))
        (let* ([serial (with-mutex (runtime-lock live)
                         (runtime-serial-set! live (+ 1 (runtime-serial live))) (runtime-serial live))]
               [owner (list 'app 'terminal serial)] [process #f] [id #f] [state #f] [registered? #f])
          (guard (ex [else
                      (when process (guard (ignored [else (void)]) (sys:close-terminal-process! process)))
                      (when registered? (actor:detach! owner))
                      (when id (guard (ignored [else (void)]) (store:delete! owner id)))
                      (with-mutex (runtime-lock live) (runtime-apps-set! live (remq state (runtime-apps live))))
                      (raise ex)])
            (set! process (sys:spawn-terminal-process (terminal-shell) command directory rows cols))
            (set! id (store:create! owner (if (= serial 1) "*terminal*" (format "*terminal ~a*" serial))
                                    (make-vector rows (make-string cols #\space))
                                    `((app . ,owner) (alive . #f) (capture . #f) (status . "starting")
                                      (read-only . #t) (disposable . #t) (mode . "terminal") (directory . ,directory)
                                      (wrap . #f) (scrollbar . #f) (manages-viewport . #t))))
            (set! state (blank-terminal-state owner id process rows cols #t))
            (terminal-state-controller-set! state (datum:copy from))
            (terminal-state-scheme-set! state (if (pair? scheme) (car scheme) (unbox default-scheme)))
            (kernel:call-with-runtime-registrations
              (lambda ()
                (actor:register! owner
                                 (lambda (message)
                                   (unless (valid-message? state message) (error 'terminal "invalid message" message))
                                   (kernel:mailbox-post! (terminal-state-mailbox state) message)))))
            (set! registered? #t)
            (with-mutex (runtime-lock live) (runtime-apps-set! live (cons state (runtime-apps live))))
            (publish-output! state #f)
            (fork-thread (lambda () (actor:call-as owner (lambda () (publisher-loop state)))))
            (fork-thread (lambda () (actor:call-as owner (lambda () (reader-loop state)))))
            id)))))

  (edoc "Install the terminal service: its store subscription and runtime registrations, refreshed on reload.")
  (define (init!)
    (kernel:call-with-runtime-registrations
      (lambda ()
        (with-mutex (runtime-lock live)
          (kernel:call-with-registration-update
            (lambda ()
              (when (runtime-store-token live) (store:unsubscribe! (runtime-store-token live)))
              (runtime-store-token-set! live
                (store:subscribe! #f
                  (lambda (event)
                    (when (eq? (car event) 'delete)
                      (cond [(instance (cadr event))
                             => (lambda (state) (kernel:mailbox-post! (terminal-state-mailbox state) '(close)))]))))))))))
    (for-each (lambda (state)
                (unless (store:exists? (terminal-state-buffer state))
                  (kernel:mailbox-post! (terminal-state-mailbox state) '(close)))) (instances))
    (void))

  (define initialized (init!))
)
