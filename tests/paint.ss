#!/usr/bin/env scheme-script

;; Row painting and complete frames: data in, ANSI out, including
;; synchronized scrolling and cleanup after a failed frame.
;; Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

;; Import and exercise the engine before loading any default window policy.
;; The same backend used below by paint also presents a windowless widget.
(eval
  '(begin
     (import (prefix (head head) head:) (prefix (head tui) tui:)
             (prefix (head widget) widget:) (prefix (head root) root:) (prefix (state model) model:)
             (prefix (state store) store:) (prefix (state view) view:)
             (prefix (core kernel) kernel:)
             (prefix (test) test:) (prefix (sys sys) sys:))
     (parameterize ([kernel:registering-module 'bare-engine]) (widget:init!))
     (head:before-frame!)
     (test:check 'engine-definitions-create-no-editor-resources
       (list (store:buffer-list) (model:ids)) '(() ()))
     (model:register-kind! 'bare-frame 1 string?)
     (let* ([source (model:create! head:ui-actor 'bare-frame 1 'session 'transient '() "Hello")]
            [root (view:create! head:ui-actor source 'text 2 '() 0)]
            [output (open-output-string)] [drawn 0])
       (widget:mount! root 'bare-screen)
       (parameterize ([sys:terminal-output-port output])
         (do ([n 0 (+ n 1)]) ((= n 2))
           (tui:render! head:before-frame!
             (lambda ()
               (let* ([frame (widget:prepare! root 12 1)] [row (car (widget:frame-lines frame))])
                 (tui:begin-frame! '(bare-screen 12 1) 1)
                 (tui:set-placements! (list (list frame 0 0)))
                 (tui:paint! 0 0 row (lambda () (set! drawn (+ drawn 1)) (tui:ansi! row))))) #f)))
       (test:check 'windowless-frame-shares-diff-and-published-geometry
         (list drawn (equal? (caar (widget:shown)) (widget:prepared root)) (store:buffer-list)) '(1 #t ()))
       (widget:unmount! root)
       (view:retire! head:ui-actor root (model:revision root))
       (model:retire! head:ui-actor source (model:revision source))
       (let ([errors (open-output-string)])
         (parameterize ([current-error-port errors]) (head:report! "No composition"))
         (test:check 'head-diagnostic-without-an-echo-area (get-output-string errors) "No composition\n")))
     (include "tests/root-install.sps")
     (kernel:retract-module! 'bare-engine)))

(test-host!)

(define evaluate! (eval '(let () (import (prefix (core kernel) kernel:)) kernel:evaluate!)))

(evaluate!
  '(begin
     (import (prefix (head paint) paint:) (prefix (head tui) tui:) (prefix (head widget) widget:)
             (prefix (head edit) edit:) (prefix (head dispatch) dispatch:)
             (prefix (head entry) entry:) (prefix (state store) store:)
             (prefix (head interaction) interaction:) (prefix (head window-host) window-host:)
             (prefix (state model) model:) (prefix (state view) view:)
             (prefix (head pacing) pacing:)
             (prefix (head spinner) spinner:)
             (prefix (head style) style:)
             (prefix (head head) head:) (prefix (head seat) seat:)
             (prefix (head echo) echo:)
             (prefix (core kernel) kernel:)
             (prefix (sys glyph) glyph:)
             (prefix (test) test:)
             (prefix (only (sys sys) terminal-output-port) sys:)
             (only (chezscheme)
                   format open-output-string get-output-string
                   parameterize))

     (define check test:check)

     ;; A fake clock covers the presentation policy without sleeping. Work
     ;; before publication consumes the budget; early wakes do not extend it.
     (define (stamp ms)
       (make-time 'time-monotonic (* (mod ms 1000) 1000000) (div ms 1000)))
     (define (milliseconds time)
       (+ (* (time-second time) 1000) (/ (time-nanosecond time) 1000000)))
     ;; Activity is local presentation: fast updates never flash, a slow one
     ;; animates at a fixed corner without mutating the cached underlying row.
     (let* ([now 0] [deadlines '()]
            [activity (spinner:make (lambda () (stamp now)) (lambda (at) (set! deadlines (cons (milliseconds at) deadlines))))]
            [rect '(4 2 6 1)] [lines '("界abcd")] [cells (vector (make-vector 6 'header))])
       (define (show busy clip)
         (let-values ([(text styles) (spinner:render! activity busy rect clip lines cells)])
           (list (car text) (vector-ref (vector-ref styles 0) 0))))
       (check 'activity-delay-animation-completion-and-clipping
         (reverse (fold-left (lambda (out case)
                               (set! now (car case))
                               (cons (show (cadr case) (if (caddr case) '(5 2 5 1) rect)) out)) '()
                    '((0 #t #f) (199 #t #f) (200 #t #f) (1000 #t #f) (1125 #t #f)
                      (1250 #t #t) (1300 #f #f) (1400 #t #f))))
         '(("界abcd" header) ("界abcd" header) ("⠋ abcd" (header ghost))
           ("⠙ abcd" (header ghost)) ("⠹ abcd" (header ghost))
           ("界abcd" header) ("界abcd" header) ("界abcd" header)))
       (check 'activity-requests-only-local-visible-deadlines-and-preserves-input
         (list (reverse deadlines) lines (vector-ref (vector-ref cells 0) 0))
         '((200 200 1000 1125 1250 1600) ("界abcd") header)))
     (check 'input-relative-presentation-deadlines
       (map
         (lambda (case)
           (let* ([now (cadr case)] [pauses '()]
                  [clock (pacing:make
                           (lambda () (stamp now))
                           (lambda (duration)
                             (let ([ms (milliseconds duration)])
                               (set! pauses (cons ms pauses))
                               (set! now (+ now (min ms (cadddr case)))))))])
             (for-each
               (lambda (ms)
                 (let ([received (stamp ms)])
                   (pacing:input! clock received)
                   (set-time-second! received 999)))
               (car case))
             (pacing:wait! clock (caddr case))
             ;; A second publication, even with a larger budget, must not
             ;; impose another wait on the same input (nested prompt frames).
             (pacing:wait! clock 50)
             (list now (reverse pauses))))
         '((() 103 8 50) ((100) 103 8 50) ((100) 108 8 50)
           ((100) 130 8 50) ((100) 103 0 50) ((100 104) 105 8 50)
           ((999) 1002 8 50) ((100) 103 8 2)))
       '((103 ()) (108 (5)) (108 ()) (130 ()) (103 ()) (112 (7))
         (1007 (5)) (108 (5 3 1))))
     (check 'input-delay-stays-bounded
       (map (lambda (value)
              (test:raises? (lambda () (tui:input-delay value))))
         '(-1 51 1.5 8.0 #f))
       '(#t #t #t #t #t))

     ;; Expired input can coalesce only after a real publication, and only
     ;; while its bounded interval remains. Checking does not move the bound.
     (let* ([now (stamp 100)] [clock (pacing:make (lambda () now) void)])
       (define (defer? received next at budget)
         ;; This clock reuses its time object; publication must own its time.
         (set-time-nanosecond! now (* at 1000000))
         (when received (pacing:input! clock (stamp received)))
         (pacing:defer? clock (stamp next) budget))
       (check 'coalescing-needs-a-published-frame (defer? 0 0 100 8) #f)
       (pacing:presented! clock)
       (check 'coalescing-obeys-input-deadlines-and-publication-bound
         (list (defer? 0 0 100 8) (defer? 0 100 105 8)
               (defer? 0 100 108 8) (defer? 0 0 115 8)
               (defer? 0 0 116 8) (defer? 0 0 110 0)
               (begin (pacing:wait! clock 0) (defer? #f 0 110 8)))
         '(#t #f #t #t #f #f #f)))

     ;; Text ownership survives only a deliberately retained live line.
     ;; Reusing the same string through an ordinary setter is a new message.
     (check 'echo-text-ownership-follows-every-replacement
       (map (lambda (replace!)
              (echo:set-text! "Indicator" 'ask)
              (replace!)
              (list (echo:text) (echo:text-owner)))
         (list (lambda () (echo:set-text! (echo:text)))
               (lambda () (echo:queue! 'test "log" #f #f "" #f))
               (lambda () (echo:queue! 'test "log" #f #f "" #t))
               echo:settle!))
       '(("Indicator" #f) ("" #f) ("Indicator" ask) ("" #f)))

     (define (contains? text needle)
       (let ([n (string-length text)] [m (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i m) n) #f]
                 [(string=? (substring text i (+ i m)) needle) #t]
                 [else (scan (+ i 1))]))))

     (define (painted thunk)
       (let ([sink (open-output-string)])
         (parameterize ([sys:terminal-output-port sink])
           (thunk))
         (get-output-string sink)))

     (define (stripped text)
       ;; the visible characters: ANSI escapes removed
       (let loop ([chars (string->list text)] [out '()] [in-escape #f])
         (cond [(null? chars) (list->string (reverse out))]
               [in-escape
                (loop (cdr chars) out
                      (not (or (char-alphabetic? (car chars))
                             (char=? (car chars) #\\))))]
               [(char=? (car chars) #\esc)
                (loop (cdr chars) out #t)]
               [else (loop (cdr chars) (cons (car chars) out) #f)])))

     ;; -- fit ------------------------------------------------------------------

     (check 'fit-pads (tui:fit "ab" 4) "ab  ")
     (check 'fit-truncates (tui:fit "abcdef" 4) "abcd")

     ;; -- goto -----------------------------------------------------------------

     (check 'goto (painted (lambda () (tui:goto! 3 7))) "\x1b;[3;7H")

     ;; -- the row painter ---------------------------------------------------------

     (define (paint-line . args) (painted (lambda () (apply tui:display-editor-line! args))))

     ;; plain text pads to the width
     (check 'plain-row
            (stripped (paint-line "hello" "hello" #f '() '() 0 #f #f 10
                                  1000))
            "hello     ")

     ;; the selection span emits the selection style over its columns
     (check 'selection-styled
            (contains? (paint-line "hello" "hello" '(1 . 3) '() '() 0 #f
                                   #f 10 1000)
                       (style:code 'selection))
            #t)

     ;; a wrap edge paints a backslash in the last column
     (check 'wrap-edge
            (stripped (paint-line "abcdefgh" "abcdefgh" #f '() '() 0 #f
                                  'wrap 5 1000))
            "abcd\\")
     (check 'truncation-edge
            (stripped (paint-line "abcdefgh" "abcdefgh" #f '() '() 0 #f
                                  'trunc 5 1000))
            "abcd$")

     ;; control characters paint as single-cell spaces
     (check 'tab-is-one-cell
            (stripped (paint-line "a\tb" "a\tb" #f '() '() 0 #f #f 5
                                  1000))
            "a b  ")

     ;; The same row painter handles cell grids. Clip a wide glyph to
     ;; blanks at either edge, and tolerate partial style vectors.
     (check 'cell-grid-clipping
       (map (lambda (range)
              (stripped (paint-line "  x" '#("界" "" "x")
                                    #f '() '() (car range) #f #f (cadr range) 3)))
            '((0 1) (1 2) (0 3) (2 3)))
       '(" " " x" "界x" "x  "))
     (check 'invalid-or-short-row-styles-fall-back-to-plain
       (map (lambda (styles)
              (stripped (paint-line "abc" "abc" #f '() '() 0 styles #f 3 3)))
            '(#(red) #() #t (red))) '("abc" "abc" "abc" "abc"))
     (check 'equal-sgr-values-share-one-run
       (paint-line "abc" "abc" #f '() '() 0
                   (vector (string-copy "31") (string-copy "31") (string-copy "31")) #f 3 3)
       "\x1b;[0m\x1b;[31mabc\x1b;[0m")

     ;; Run boundaries combine syntax, selection, overlapping backgrounds,
     ;; hover and first-link precedence. Padding past bound clears the ink.
     (check 'clipped-row-runs-preserve-overlays-and-link-precedence
       (paint-line "abcde" "abcde" '(1 . 3)
         '((1 4 match) (2 9 match-point) (3 9 active) (0 4 candidate) (2 3 hover))
         '((1 3 "https://first") (2 4 "https://second"))
         1 '#(keyword keyword number number plain) 'trunc 6 4)
       (string-append
         "\x1b;[0m" (style:code 'keyword) (style:code 'selection) (style:code 'match) (style:code 'candidate)
         "\x1b;]8;;https://first\x1b;\\b\x1b;]8;;\x1b;\\"
         "\x1b;[0m" (style:code 'number) (style:code 'selection) (style:code 'match-point) (style:code 'hover)
         "\x1b;]8;;https://first\x1b;\\c\x1b;]8;;\x1b;\\"
         "\x1b;[0m" (style:code 'number) (style:code 'active) (style:code 'candidate)
         "\x1b;]8;;https://second\x1b;\\d\x1b;]8;;\x1b;\\"
         "\x1b;[0m" (style:code 'plain) "  \x1b;[0m" (style:code 'chrome) "$\x1b;[0m"))

     ;; Text overlays retain the base/background; hover consistently wins
     ;; over keyboard emphasis regardless of registry order.
     (check 'overlay-faces-and-hover-precedence
       (map (lambda (marks)
              (let ([out (paint-line "abc" "abc" #f marks '() 0 '#(keyword keyword keyword) #f 3 3)])
                (map (lambda (face) (contains? out (style:code face))) '(keyword mark candidate hover candidate-hover active))))
         '(((0 3 mark)) ((0 3 candidate) (0 3 hover) (0 3 active))
           ((0 3 hover) (0 3 candidate) (0 3 active))
           ((0 3 candidate) (0 3 candidate-hover)) ((0 3 candidate-hover) (0 3 candidate))))
       '((#t #t #f #f #f #f) (#t #f #f #t #f #t) (#t #f #f #t #f #t)
         (#t #f #f #f #t #f) (#t #f #f #f #t #f)))

     ;; -- emit-runs ----------------------------------------------------------------

     (let ([out (painted
                  (lambda ()
                    (tui:emit-runs! "abcd"
                                    (vector 'keyword 'keyword 'plain
                                            'plain)
                                    0 4)))])
       (check 'runs-coalesce (stripped out) "abcd")
       (check 'runs-styled (contains? out (style:code 'keyword))
              #t))

     ;; -- soft-wrap breaks ----------------------------------------------------------

     (check 'wrap-breaks-measure-cells-and-preserve-whole-clusters
       (map (lambda (case) (paint:compute-breaks (car case) (cadr case)))
         '(("" 1) ("short" 10) ("aaa bbb ccc" 5) ("aaaaaaaaaa" 4)
           ("界x界y" 3) ("ae\x301;bx" 2) ("界 界 x" 4) ("界x" 1)
           ("e\x301;x" 1) ("🇺🇸x" 2) ("a\tb" 2)))
       '(#(0) #(0) #(0 4 8) #(0 4 8) #(0 2) #(0 3) #(0 2) #(0 1)
         #(0 2) #(0 2) #(0 2)))

     ;; Viewport walks share line breaks, but never retain another window's
     ;; width or a previous wrap setting. Check partial top segments and the
     ;; exact overflow boundary while geometry changes on the same text.
     (let* ([b (seat:new-local-buffer! "viewport geometry")]
            [w (seat:make-window b 0 0 0 1 9 10 0 5 #t)]
            [other (seat:make-window b 0 0 0 1 9 10 0 11 #t)])
       (seat:buffer-lines-set! b '#("abcdefghij" "klmnopqrst" "uvwx"))
       (seat:window-line-numbers-set! other #f)
       (parameterize ([seat:scrollbar #f])
         (check 'viewport-walks-follow-current-window-geometry
           (map (lambda (case)
                  (let ([width (car case)] [wrap (cadr case)] [numbers (caddr case)]
                        [fact (list-ref case 3)] [topseg (list-ref case 4)] [height (list-ref case 5)])
                    (seat:window-width-set! w width)
                    (seat:window-wrap-set! w wrap)
                    (seat:window-line-numbers-set! w numbers)
                    (seat:buffer-fact-set! b 'wrap fact)
                    (seat:window-topseg-set! w topseg)
                    (list (paint:rows-before w 1 9) (paint:rows-before other 1 9)
                          (paint:view-overflows? w (seat:window-text w) (- height 1))
                          (paint:view-overflows? w (seat:window-text w) height))))
             '((5 #t #f default 1 6) (7 #t #f default 1 4)
               (7 #t #t default 1 6) (7 #f #f default 0 3)
               (7 #t #f clean 1 4) (7 #t #f (clean . 3) 1 9)
               (5 #t #f default 1 6)))
           '((4 1 #t #f) (2 1 #t #f) (4 1 #t #f) (1 1 #t #f)
             (2 1 #t #f) (6 7 #t #f) (4 1 #t #f)))))

     ;; -- hyperlink detection ---------------------------------------------------------

     (check 'detects-http
            (paint:detect-hyperlinks "see http://a.example/x here")
            '((4 22 "http://a.example/x")))
     (check 'trims-trailing-punctuation
            (map paint:detect-hyperlinks '("at https://e.dev/p, then" "See https://example.com/path."))
            '(((3 18 "https://e.dev/p")) ((4 28 "https://example.com/path"))))
     (check 'angle-brackets-end-a-url
            (paint:detect-hyperlinks "<http://a.example>")
            '((1 17 "http://a.example")))
     (check 'bare-scheme-skipped
            (paint:detect-hyperlinks "http:// is not a link") '())
     (check 'several-links
            (map caddr (paint:detect-hyperlinks
                         "http://one.example and https://two.example"))
            '("http://one.example" "https://two.example"))

     ;; -- complete synchronized frames ----------------------------------

     (define (sync-events text)
       (let ([n (string-length text)]
             [begin "\x1b;[?2026h"] [end "\x1b;[?2026l"])
         (let scan ([at 0] [events '()])
           (cond [(> (+ at 8) n) (reverse events)]
                 [(string=? (substring text at (+ at 8)) begin)
                  (scan (+ at 8) (cons 'begin events))]
                 [(string=? (substring text at (+ at 8)) end)
                  (scan (+ at 8) (cons 'end events))]
                 [else (scan (+ at 1) events)]))))

     (define document (seat:new-local-buffer! "paint frames"))
     (seat:buffer-lines-set! document
       (list->vector
         (map (lambda (row) (format "paint row ~a" row)) (iota 200))))
     (seat:add-buffer! document)
     (seat:set-window-buffer! (seat:current-window) document)
     ;; Let initial size detection settle, then use a fixed test grid.
     (painted paint:redraw!)
     (tui:set-screen-rows! 24)
     (tui:set-screen-cols! 80)
     ;; Apps may project source coordinates or replace generated details
     ;; with operation text. A stale fact must not leave a partial marker
     ;; or invalid substring bounds when that standard header is replaced.
     (let ([view (seat:register-widget-host! (seat:new-local-buffer! "status projection") void void)])
       (seat:buffer-lines-set! view '("generated"))
       (seat:buffer-fact-set! view 'conflicts 1)
       (seat:set-window-buffer! (seat:current-window) view)
       (check 'app-status-projection-keeps-default-coordinates-and-operation-text-coherent
         (map (lambda (value)
                (seat:set-app-status-position! view (and value (lambda (b) value)))
                (let ([frame (stripped (painted paint:redraw!))])
                  (list (contains? frame "!!") (contains? frame "L8 C3") (contains? frame "Pick a file"))))
              '(#f (7 . 2) "Pick a file"))
         '((#t #f #f) (#t #t #f) (#f #f #t)))
       ;; a provider's text follows the buffer's name, which every status
       ;; line shows; the empty text leaves the name alone
       (check 'status-text-follows-the-buffer-name
         (map (lambda (value)
                (seat:set-app-status-position! view (lambda (b) value))
                (let ([frame (stripped (painted paint:redraw!))])
                  (list (contains? frame (format "▏~a  3 of 5" (seat:buffer-name view)))
                        (contains? frame (format "▏~a" (seat:buffer-name view))))))
              '("3 of 5" ""))
         '((#t #t) (#f #t)))
       (seat:set-app-status-position! view #f)
       (seat:buffer-lines-set! view (map (lambda (i) (format "choice ~a" i)) (iota 50)))
       (check 'hidden-cursor-still-follows-keyboard-selection
         (map (lambda (visible?)
                (seat:set-app-cursor-visible! view visible?)
                (seat:window-prow-set! (seat:current-window) 49)
                (seat:window-top-set! (seat:current-window) 0)
                (let ([frame (painted paint:redraw!)])
                  (list (> (seat:window-top (seat:current-window)) 0)
                        (contains? frame "\x1b;[?25h")))) '(#t #f))
         '((#t #t) (#t #f)))
       ;; Clickable status spans use cell geometry, including wide/combining
       ;; labels. Ellipsizing a control makes the entire control inert.
       (let* ([prefix "界e\x301; 🔒"] [toggle void]
              [start (+ (glyph:cells (format "~a▏~a  ~a" (seat:window-index (seat:current-window)) (seat:buffer-name view) prefix)) 1)]
              [edge (+ start 2 seat:window-buttons-width 1)])
         (define (hits)
           (let* ([entry (car (seat:layout))] [row (+ (cadr entry) (caddr entry))])
             (map (lambda (column)
                    (let ([hit (seat:window-button-at column row)]) (and hit (eq? (car hit) toggle))))
               (list (- start 2) start (+ start 1) (+ start 2)))))
         (seat:set-app-status-position! view (lambda (b) prefix))
         (parameterize ([kernel:registering-module 'paint-control-test])
           (paint:add-buffer-status-hint!
             (lambda (b active?) (and (eq? b view) (list '(" " . #f) (cons "🔓" toggle) '(" tail" . #f))))))
         (check 'status-controls-hit-only-complete-visible-labels
           (map (lambda (width) (tui:set-screen-cols! width) (painted paint:redraw!) (hits))
             (list 80 edge (+ edge 1)))
           '((#f #t #t #f) (#f #f #f #f) (#f #t #t #f)))
         (seat:set-window-buffer! (seat:current-window) document)
         (check 'replacing-buffer-clears-painted-controls (hits) '(#f #f #f #f))
         (kernel:retract-module! 'paint-control-test)
         (tui:set-screen-cols! 80))
       (seat:forget-buffer! view))
     ;; A preparation hook may present a notice, causing a direct redraw.
     ;; Both frames prepare at the current width; their synchronized updates
     ;; must not nest, since the inner end would release the outer update.
     (let ([prepared '()])
       (parameterize ([kernel:registering-module 'paint-prepare-test])
         (head:add-pre-redraw-hook!
           (lambda ()
             (set! prepared (cons (list (tui:screen-cols) (seat:window-width (seat:current-window))) prepared))
             (when (null? (cdr prepared))
               (paint:echo-queue! 'eval "42" #f #f " [copied]")
               (paint:show-message! "Prepared\nmessage" #f)))))
       (dynamic-wind
         (lambda () (tui:set-screen-live! #t))
         (lambda ()
           (let ([frame (painted paint:redraw!)])
             (check 'frame-prepares-before-synchronized-paint
               (list prepared (sync-events frame) (contains? frame "Prepared") (contains? frame "message")
                     (contains? frame (string-append (style:code 'ghost) " [copied]")))
               '(((80 80) (80 80)) (begin end begin end) #t #t #t))))
         (lambda ()
           (tui:set-screen-live! #f)
           (kernel:retract-module! 'paint-prepare-test)
           (echo:settle!))))
     ;; The echo area is a bordered box of at most 100 columns, centered: a
     ;; narrower screen is the whole box. Rows wrap inside the borders with
     ;; their text at the left one, and cursor geometry counts inner columns.
     (define (echo-box-frame width text)
       (tui:set-screen-cols! width)
       (tui:invalidate-screen-cache!)
       (echo:set-text! text)
       (paint:update-echo-geometry!)
       (dynamic-wind
         (lambda () (tui:set-screen-live! #t))
         (lambda () (stripped (painted paint:present-echo!)))
         (lambda () (tui:set-screen-live! #f))))
     (check 'echo-rows-are-a-centered-bordered-box
       (list
         (let ([frame (echo-box-frame 80 "Boxed")])
           (list (paint:echo-width) (paint:echo-position 5) (paint:echo-index-at 0 5)
                 (and (>= (string-length frame) 80)
                      (string=? (substring frame 0 80)
                                (string-append "┊Boxed" (make-string 73 #\space) "┊")))))
         (let ([frame (echo-box-frame 140 "Boxed")])
           (list (paint:echo-width)
                 (and (>= (string-length frame) 140)
                      (string=? (substring frame 0 140)
                                (string-append (make-string 20 #\space) "┊Boxed" (make-string 93 #\space) "┊"
                                               (make-string 20 #\space))))))
         (let ([frame (echo-box-frame 60 (make-string 70 #\a))])
           ;; two rows: the first wraps at inner column 57 with its mark
           (list (paint:echo-width) (length (echo:spans)) (paint:echo-position 57)
                 (and (>= (string-length frame) 120)
                      (string=? (substring frame 0 60) (string-append "┊" (make-string 57 #\a) "\\┊"))
                      (string=? (substring frame 60 120)
                                (string-append "┊" (make-string 13 #\a) (make-string 45 #\space) "┊"))))))
       '((78 (0 . 5) 5 #t) (98 #t) (58 2 (1 . 0) #t)))
     (check 'echo-border-follows-its-setting
       (begin
         (tui:set-screen-cols! 80)
         (tui:invalidate-screen-cache!)
         (echo:set-text! "Framed")
         (paint:update-echo-geometry!)
         (dynamic-wind
           (lambda () (tui:set-screen-live! #t))
           (lambda ()
             ;; no cache reset between the two frames: the key carries the glyph
             (list (contains? (painted paint:present-echo!) "\x1b;[38;5;245m┊")
                   (parameterize ([paint:echo-box-border "║"])
                     (contains? (painted paint:present-echo!) "\x1b;[38;5;245m║"))
                   (test:raises? (lambda () (paint:echo-box-border "ab")))
                   (parameterize ([paint:echo-box-border #\|]) (paint:echo-box-border))))
           (lambda () (tui:set-screen-live! #f))))
       '(#t #t #t "|"))
     (check 'echo-border-wears-the-inactive-bar-shade
       (begin
         (tui:set-screen-cols! 80)
         (tui:invalidate-screen-cache!)
         (echo:set-text! "Shaded")
         (paint:update-echo-geometry!)
         (dynamic-wind
           (lambda () (tui:set-screen-live! #t))
           (lambda () (contains? (painted paint:present-echo!) "\x1b;[38;5;245m┊"))
           (lambda () (tui:set-screen-live! #f))))
       #t)
     (tui:set-screen-cols! 80)
     (echo:settle!)
     (define top-before (seat:window-top (seat:current-window)))
     (define scrolling
       (let loop ([row 0] [frames '()])
         (if (= row 40)
           (reverse frames)
           (begin
             (seat:window-prow-set! (seat:current-window) row)
             (let ([frame (painted paint:redraw!)])
               (loop (+ row 1) (cons frame frames)))))))
     (check 'scrolling-frames-are-synchronized
            (map sync-events scrolling) (make-list 40 '(begin end)))
     (check 'scrolling-moves-the-viewport
            (> (seat:window-top (seat:current-window)) top-before) #t)
     (define (current-top-line)
       (vector-ref (seat:buffer-lines document) (seat:window-top (seat:current-window))))
     (check 'scrolling-paints-visible-text
            (contains? (car (reverse scrolling)) (current-top-line)) #t)

     ;; A late callback can reenter after rows have already been prepared.
     ;; It must see no partial output, can publish immediately, and supersedes
     ;; the outer diff. The retry uses the new geometry and visible baseline.
     (let ([sink (open-output-string)] [before #f] [nested #f]
           [old-top (current-top-line)])
       (parameterize ([kernel:registering-module 'paint-nested-test])
         (paint:add-status-hint!
           (lambda ()
             (unless before
               (set! before (get-output-string sink))
               (seat:window-prow-set! (seat:current-window) 95)
               (tui:set-screen-cols! 60)
               (paint:redraw!)
               (set! nested (get-output-string sink)))
             ;; Asking for a fresh next frame is not a nested publication
             ;; and must not cause this callback to be retried indefinitely.
             (tui:invalidate-screen-cache!)
             #f)))
       (parameterize ([sys:terminal-output-port sink]) (paint:redraw!))
       (let ([frame (string-append nested (get-output-string sink))])
         (check 'nested-paint-publishes-before-return-and-discards-obsolete-output
           (list before (sync-events nested) (sync-events frame)
                 (contains? frame old-top) (contains? frame (current-top-line))
                 (seat:window-width (seat:current-window)))
           '("" (begin end) (begin end begin end) #f #t 60)))
       (kernel:retract-module! 'paint-nested-test)
       (tui:set-screen-cols! 80))

     ;; A malformed highlighter or an escape after row preparation leaves
     ;; the terminal untouched. Neither failed diff may become the baseline.
     (seat:window-prow-set! (seat:current-window) 120)
     (parameterize ([kernel:registering-module 'paint-failure-test])
       (paint:add-highlighter! (lambda () #f)))
     (define failed-output (open-output-string))
     (check 'frame-error-propagates
            (parameterize ([sys:terminal-output-port failed-output])
              (guard (ex [else #t]) (paint:redraw!) #f)) #t)
     (check 'failed-preparation-writes-nothing (get-output-string failed-output) "")
     (kernel:retract-module! 'paint-failure-test)
     (check 'failed-frame-forces-repaint
            (contains? (painted paint:redraw!) (current-top-line)) #t)

     ;; Escape late, after this new viewport has updated the private shadow.
     (seat:window-prow-set! (seat:current-window) 150)
     (define escaped-output (open-output-string))
     (check 'frame-can-unwind
            (call/cc
              (lambda (escape)
                (parameterize ([kernel:registering-module 'paint-escape-test])
                  (paint:add-status-hint! (lambda () (escape 'escaped))))
                (parameterize ([sys:terminal-output-port escaped-output])
                  (paint:redraw!))
                'returned))
            'escaped)
     (check 'escaped-preparation-writes-nothing (get-output-string escaped-output) "")
     (kernel:retract-module! 'paint-escape-test)
     (check 'unwound-frame-forces-repaint
            (contains? (painted paint:redraw!) (current-top-line)) #t)

     ;; A failed terminal write is different: its bytes may have landed.
     ;; Release synchronization and resend text, title and cursor next time.
     (let ([failed? #f] [writes '()])
       (define sink
         (make-custom-textual-output-port "failed frame"
           (lambda (text start count)
             (set! writes (cons (substring text start (+ start count)) writes))
             (unless failed?
               (set! failed? #t)
               (error 'test "terminal write failed"))
             count)
           #f #f #f))
       (define raised?
         (parameterize ([sys:terminal-output-port sink])
           (test:raises? paint:redraw!)))
       (close-output-port sink)
       (let ([frame (painted paint:redraw!)])
         (check 'failed-write-releases-and-forgets-all-terminal-state
           (list raised?
                 (car (reverse (sync-events (apply string-append (reverse writes)))))
                 (contains? frame (current-top-line))
                 (contains? frame "\x1b;]2;e: ")
                 (contains? frame "\x1b;[0 q"))
           '(#t end #t #t #t))))

     ;; A bell uses the ordinary nested input pump for both frames. Retrigger
     ;; it at the old expiry, before painting, including a burst of bad keys.
     ;; One watchdog bounds the fixture; it never paints or changes UI state.
     (define (flashing? frame)
       (contains? frame (string-append "\x1b;[7m" (make-string 80 #\space) "\x1b;[0m")))
     (call/cc
       (lambda (done)
         (head:run-on-main! (lambda () (done #t)))
         (parameterize ([head:in-main-pump #t]) (head:read-key-event))))
     (echo:set-text! "Question survives the bell")
     (let ([frames '()] [prepared 0] [owner (get-thread-id)]
           [painters (test:recorder)] [timed-out? (test:gate)])
       (parameterize ([kernel:registering-module 'paint-bell-test])
         (paint:add-highlighter! (lambda () (painters (get-thread-id)) '()))
         (head:add-pre-redraw-hook!
           (lambda ()
             (set! prepared (+ prepared 1))
             (when (= prepared 2)
               (do ([i 0 (+ i 1)]) ((= i 100)) (tui:visual-bell!))))))
       (let ([stop (test:worker
                     (lambda ()
                       (sleep (make-time 'time-duration 400000000 0))
                       (timed-out? #t)
                       (head:wake-main!)))])
         (dynamic-wind
           (lambda () (tui:set-screen-live! #t))
           (lambda ()
             (let ([result
                    (call/cc
                      (lambda (done)
                        (head:set-frame-hook!
                          (lambda (coalesce?)
                            (when (timed-out?) (done 'timed-out))
                            (let ([frame (painted paint:redraw!)])
                              (set! frames (cons frame frames))
                              (unless (flashing? frame) (done 'expired)))))
                        (tui:visual-bell!)
                        (head:read-key-event #f)))])
               (check 'bell-retrigger-and-expiry-share-the-owning-pump
                 (list result (map flashing? (reverse frames)) prepared (painters))
                 (list 'expired '(#t #t #t #f) 4 (make-list 4 owner)))
               (check 'bell-frames-restore-the-question-with-synchronized-output
                 (list (map sync-events frames) (echo:text)
                   (contains? (car frames) "Question survives the bell"))
                 (list (make-list 4 '(begin end)) "Question survives the bell" #t))))
           (lambda ()
             (tui:set-screen-live! #f)
             (head:set-frame-hook! void)
             (kernel:retract-module! 'paint-bell-test)
             (stop)))))

     ;; Arming never writes to a captured display. An inactive screen ignores
     ;; it; retirement discards it; even a direct frame expires an overdue bell.
     (check 'bell-lifetime-without-an-input-pump
       (map
         (lambda (state)
           (tui:set-screen-live! (not (eq? state 'inactive)))
           (let ([old-display (open-output-string)])
             (parameterize ([sys:terminal-output-port old-display]) (tui:visual-bell!))
             (when (eq? state 'retired)
               (tui:set-screen-live! #f)
               (tui:set-screen-live! #t))
             (let ([frame (and (not (eq? state 'expired)) (painted paint:redraw!))])
               (sleep (make-time 'time-duration 75000000 0))
               (let ([frame (or frame (painted paint:redraw!))])
                 (tui:set-screen-live! #f)
                 (list (get-output-string old-display) (flashing? frame)
                   (sync-events frame) (contains? frame "Question survives the bell"))))))
         '(inactive retired expired))
       (make-list 3 '("" #f (begin end) #t)))

     ;; The visible frame owns hit geometry, including across partial output
     ;; and uncertain terminal writes. No extra test process or timing wait.
     (widget:init!) (window-host:init!)
     ;; A partial overlay must not mutate the full-width child's lent style
     ;; row or an earlier frame, including when that child is cached.
     (widget:register! 'style-row 1
       (list (cons 'render (lambda (s d w h r) (list (make-string w #\x))))
         (cons 'decorate (lambda (s d w h r) (list (list (list 0 0 w h) (cdr (assq 'face (view:options d)))))))))
     (widget:register! 'style-overlay 1
       (list (cons 'layout (lambda (d w h measure locate)
                             (map (lambda (child rect) (list (cadr child) rect))
                               (view:children d) (list (list 0 0 w h) '(1 0 2 1)))))))
     (let* ([a (view:create! head:ui-actor #f 'style-row 1 '((face . header)) '())]
            [b (view:create! head:ui-actor #f 'style-row 1 '((face . bold)) '())]
            [root (view:create! head:ui-actor #f 'style-overlay 1 '() '())])
       (define (styles f) (vector->list (widget:frame-styles f 0 (car (widget:frame-lines f)))))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'full a 'fit) (list 'overlay b 'fit)) '())) '())
       (widget:mount! root 'style-sharing)
       (let ([first (widget:prepare! root 5 1)])
         (widget:prepare! root 5 1)
         (check 'composited-style-rows-preserve-cached-children-and-older-frames
           (list (styles first) (styles (car (widget:frame-children first))))
           '((header bold bold header header) (header header header header header))))
       (widget:unmount! root))
     (model:register-kind! 'frame-test 1 string?)
     (let* ([w (seat:current-window)] [was (seat:current-buffer-mirror)]
            [source (model:create! head:ui-actor 'frame-test 1 'session 'persistent '() "a\nb\nc\nd\ne")]
            [text (view:create! head:ui-actor source 'text 2 '() 0)]
            [scroll (view:create! head:ui-actor #f 'scroll 1 '() #f)])
       (view:arrange! head:ui-actor (list (list scroll 0 (list (list 'text text 'fit)) '())) '())
       (let* ([b (window-host:show-widget! w scroll)] [first (widget:prepare! scroll 6 2)])
         (check 'scroll-renders-only-its-visible-range (widget:frame-lines first) '("> a   " "  b   "))
         (widget:act! scroll 'scroll 2)
         (check 'scroll-anchor-is-logical-and-selection-independent
           (list (view:state (interaction:snapshot scroll)) (view:state (interaction:snapshot text))
             (widget:frame-lines (widget:prepare! scroll 6 2)))
           '((0 2) 0 ("  c   " "  d   ")))
         (check 'zero-allocation-produces-no-output (widget:frame-lines (widget:prepare! scroll 0 0)) '())
         (painted paint:redraw!)
         (let* ([layout (seat:root)] [other (seat:make-window was 0 0 0 0 0 4 40 10 'default)])
           (check 'painted-widget-pointer-uses-the-same-origin-as-window-text
             (map (lambda (layout)
                    (seat:set-layout-root! layout)
                    (widget:act! scroll 'scroll -100)
                    (painted paint:redraw!)
                    (let ([p (paint:window-screen-position w 1 0)])
                      (widget:pointer! '(pointer press primary ()) (- (cdr p) 1) (- (car p) 1))
                      (view:state (interaction:snapshot text))))
               (list w (seat:make-layout-split 'right other w 1 1) (seat:make-layout-split 'below other w 1 1)))
             '(1 1 1))
           (seat:set-layout-root! layout) (painted paint:redraw!))
         (let ([shown (widget:shown)])
           (widget:prepare! scroll 1 1) (painted paint:present-echo!)
           (check 'partial-output-retains-exact-shown-widget-frame (eq? shown (widget:shown)) #t))
         (let ([failed? #f])
           (let ([port (make-custom-textual-output-port "widget failed output"
                         (lambda (s start count) (unless failed? (set! failed? #t) (error 'test "write failed")) count) #f #f void)])
             (test:raises? (lambda () (parameterize ([sys:terminal-output-port port]) (paint:redraw!)))))
           (check 'uncertain-output-disables-widget-hits (widget:shown) '())
           (painted paint:present-echo!)
           (check 'partial-output-cannot-reenable-uncertain-hits (widget:shown) '())
           (painted paint:redraw!)
           (check 'full-output-reenables-widget-frame (widget:frame-id (caar (widget:shown))) scroll))
         (seat:show-buffer-mirror! was) (seat:forget-buffer! b)))
     (entry:init!)
     (let* ([w (seat:current-window)] [was (seat:current-buffer-mirror)]
            [source (store:create! head:ui-actor "entry paint" '("abcdef"))]
            [id (view:create! head:ui-actor source 'entry 1 '() '((0 . 2) (0 . 0)))]
            [b (window-host:show-widget! w id)])
       (let ([output (painted paint:redraw!)])
         (check 'entry-selection-and-blinking-block-reach-the-window-painter
           (list (contains? output (style:code 'selection)) (contains? output "\x1b;[1 q")) '(#t #t)))
       (entry:select! id 4 4)
       (widget:prepare! id 10 1) (painted paint:present-echo!)
       (check 'partial-paint-retains-shown-entry-caret (widget:caret (caar (widget:shown))) '(2 . 0))
       (let ([output (painted paint:redraw!)])
         (check 'unchanged-entry-text-still-updates-selection-and-caret
           (list (contains? output "abcdef") (widget:caret (caar (widget:shown)))) '(#t (4 . 0))))
       (seat:show-buffer-mirror! was) (seat:forget-buffer! b))
     ;; Ordinary documents use the same editor projection as embedded ones;
     ;; source identity, wrapped input, outer chrome and resume agree.
     (edit:init!)
     (let* ([w (seat:current-window)] [b (seat:new-buffer! "hosted editor")])
       (seat:buffer-lines-set! b (list->vector (cons (make-string 180 #\a) (make-list 30 "界éz"))))
       (seat:show-buffer-mirror! b) (seat:window-line-numbers-set! w #t)
       (tui:set-screen-cols! 100) (tui:set-screen-rows! 24)
       (let* ([other (window-host:split-right!)] [a (seat:window-editor w)] [c (seat:window-editor other)])
         (window-host:focus! w)
         (let ([running (painted (lambda () (parameterize ([paint:cursor-in-echo #t]) (paint:place-cursor!))))])
           (check 'ordinary-editing-restores-blinking-block-after-evaluation
             (list (contains? running "\x1b;[3 q") (contains? (painted paint:redraw!) "\x1b;[1 q")) '(#t #t)))
         (dispatch:key! "DOWN") (painted paint:redraw!)
         (check 'ordinary-split-uses-independent-editor-roots-and-wrapped-input
           (list (eq? b (seat:window-buffer w)) (eq? b (seat:window-buffer other))
             (not (equal? a c)) (car (view:state (interaction:snapshot a)))
             (car (view:state (interaction:snapshot c))))
           (list #t #t #t (cons 0 (paint:wrap-width w)) '(0 . 0)))
         (let ([at (paint:window-screen-position w 0 3)])
           (widget:pointer! '(pointer press primary ()) (- (cdr at) 1) (- (car at) 1))
           (widget:pointer! '(pointer release primary ()) (- (cdr at) 1) (- (car at) 1)))
         (edit:scroll! a 3)
         (let ([top (caddr (view:state (interaction:snapshot a)))])
           (painted paint:redraw!)
           (check 'ordinary-pointer-and-scroll-retain-widget-geometry
             (list (car (view:state (interaction:snapshot a)))
               (equal? top (caddr (view:state (interaction:snapshot a))))) '((0 . 3) #t)))
         (edit:select! a '(1 . 1) '(1 . 1)) (painted paint:redraw!)
         (let ([at (paint:window-screen-position w 1 1)])
           (widget:pointer! '(pointer press primary () 2) (- (cdr at) 1) (- (car at) 1))
           (widget:pointer! '(pointer release primary ()) (- (cdr at) 1) (- (car at) 1)))
         (seat:set-window-buffer! (seat:popup) b)
         (check 'ordinary-word-selection-and-read-only-popup-survive-hosting
           (list (list-head (view:state (interaction:snapshot a)) 2)
             (test:raises? (lambda () (edit:insert! (seat:window-editor (seat:popup)) "denied"))))
           '(((1 . 4) (1 . 0)) #t))
         (window-host:clear-pop-up!)
         (edit:end-of-buffer!) (painted paint:redraw!)
         (let ([bottom (car (view:state (interaction:snapshot a)))]
               [top (seat:window-top w)])
           (seat:goto! '(2 . 4)) (edit:beginning-of-line!) (painted paint:redraw!)
           (check 'current-window-api-and-search-jumps-use-editor-reveal
             (list bottom (> top 0) (car (view:state (interaction:snapshot a)))
               (let ([p (paint:window-screen-position w 2 0)]) (<= 1 (car p) (seat:window-size w))))
             '((30 . 4) #t (2 . 0) #t)))
         (seat:checkpoint!)
         (check 'ordinary-resume-rehosts-retained-editor-roots
           (and (seat:resume!)
             (begin (painted paint:redraw!)
               (for-all (lambda (id)
                          (and (find (lambda (w) (equal? id (seat:window-widget w))) (seat:windows))
                            (find (lambda (p) (equal? id (widget:frame-id (car p)))) (widget:shown)))) (list a c))) #t)
           #t)))
     (test:finish! 'paint))
  (interaction-environment))
