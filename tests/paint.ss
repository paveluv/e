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
             (prefix (head widget) widget:) (prefix (head root) root:) (prefix (head interaction) interaction:) (prefix (state model) model:)
             (prefix (state store) store:) (prefix (state view) view:)
             (prefix (core kernel) kernel:)
             (prefix (service vt) vt:)
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
     (let* ([root (view:create! head:ui-actor #f 'link-fixture 1 '() "https://one")]
            [child (view:create! head:ui-actor #f 'link-overlay 1 '() '())]
            [output (open-output-string)] [mirror (vt:make-emulator 24 80)])
       (widget:register! 'link-overlay 1 (list (cons 'render (lambda args '("covered")))))
       (widget:register! 'link-fixture 1
         (list (cons 'render (lambda args '("abcdefgh")))
           (cons 'links (lambda (data d width height range) (list (list '(0 0 8 1) (view:state d) "fixture"))))
           (cons 'layout (lambda (d width height measure locate) (list (list child '(2 0 10 1)))))))
       (view:arrange! head:ui-actor (list (list root 0 (list (list 'overlay child '(grow 1))) '())) '())
       (widget:mount! root 'link-fixture)
       (parameterize ([sys:terminal-output-port output])
         (tui:render! head:before-frame! (lambda () (tui:draw-root! (widget:prepare! root 8 1))) #f)
         (vt:emulator-feed! mirror (get-output-string output))
         (test:check 'hyperlinks-compose-and-clip-with-text-and-opaque-overlays
           (list (widget:frame-row-links (widget:prepared root) 0)
             (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 0)
             (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 2))
           '(((0 2 "https://one" "fixture")) ("https://one" "fixture") #f))
         (interaction:set-state! head:ui-actor root #f "https://two")
         (tui:render! head:before-frame! (lambda () (tui:draw-root! (widget:prepare! root 8 1))) #f)
         (vt:emulator-feed! mirror (get-output-string output))
         (test:check 'destination-only-change-repaints-root-row
           (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 0) '("https://two" "fixture")))
       (widget:unmount! root) (view:retire! head:ui-actor root (model:revision root)))
     (include "tests/root-install.sps")
     (kernel:retract-module! 'bare-engine)))


(define evaluate! test-evaluate!)

(evaluate!
  '(begin
     (import (prefix (head render) render:) (prefix (head tui) tui:) (prefix (head widget) widget:)
             (prefix (foundation edoc) edoc:)
             (prefix (head edit) edit:)
             (prefix (head entry) entry:) (prefix (state store) store:)
             (prefix (head interaction) interaction:)
             (prefix (state model) model:) (prefix (state view) view:)
             (prefix (head pacing) pacing:)
             (prefix (head spinner) spinner:)
             (prefix (head style) style:)
             (prefix (head head) head:)
             (prefix (core kernel) kernel:)
             (prefix (sys glyph) glyph:)
             (prefix (test) test:)
             (prefix (only (sys sys) terminal-output-port) sys:)
             (only (chezscheme)
                   format open-output-string get-output-string
                   parameterize))

     (define check test:check)

     (kernel:load-module! "widget")

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
       (map (lambda (case) (render:breaks (car case) (cadr case)))
         '(("" 1) ("short" 10) ("aaa bbb ccc" 5) ("aaaaaaaaaa" 4)
           ("界x界y" 3) ("ae\x301;bx" 2) ("界 界 x" 4) ("界x" 1)
           ("e\x301;x" 1) ("🇺🇸x" 2) ("a\tb" 2)))
       '(#(0) #(0) #(0 4 8) #(0 4 8) #(0 2) #(0 3) #(0 2) #(0 1)
         #(0 2) #(0 2) #(0 2)))

     ;; -- hyperlink detection ---------------------------------------------------------

     (check 'detects-http
            (render:detect-links "see http://a.example/x here")
            '((4 22 "http://a.example/x")))
     (check 'trims-trailing-punctuation
            (map render:detect-links '("at https://e.dev/p, then" "See https://example.com/path."))
            '(((3 18 "https://e.dev/p")) ((4 28 "https://example.com/path"))))
     (check 'angle-brackets-end-a-url
            (render:detect-links "<http://a.example>")
            '((1 17 "http://a.example")))
     (check 'bare-scheme-skipped
            (render:detect-links "http:// is not a link") '())
     (check 'several-links
            (map caddr (render:detect-links
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

     (include "tests/frame-output.sps")

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
     (test:finish! 'paint)))
