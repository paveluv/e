#!/usr/bin/env scheme-script

;; Row painting and complete frames: data in, ANSI out, including
;; synchronized scrolling and cleanup after a failed frame.
;; Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (paint) paint:)
             (prefix (style) style:)
             (prefix (head) head:)
             (prefix (echo) echo:)
             (prefix (kernel) kernel:)
             (prefix (test) test:)
             (prefix (only (sys) terminal-output-port) sys:)
             (only (chezscheme)
                   format open-output-string get-output-string
                   parameterize))

     (define check test:check)

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

     (check 'fit-pads (paint:fit "ab" 4) "ab  ")
     (check 'fit-truncates (paint:fit "abcdef" 4) "abcd")

     ;; -- goto -----------------------------------------------------------------

     (check 'goto (painted (lambda () (paint:goto 3 7))) "\x1b;[3;7H")

     ;; -- the row painter ---------------------------------------------------------

     (define (paint-line . args) (painted (lambda () (apply paint:display-editor-line args))))

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

     ;; a link opens and closes an OSC 8 around its run
     (let ([out (paint-line "see http://x.example now"
                            "see http://x.example now"
                            #f '() '((4 20 "http://x.example")) 0 #f #f
                            24 1000)])
       (check 'link-opens (contains? out "\x1b;]8;;http://x.example\x1b;\\")
              #t)
       (check 'link-closes (contains? out "\x1b;]8;;\x1b;\\") #t))

     ;; a highlight mark emits its named face on top of the base style
     (check 'mark-face
            (contains? (paint-line "abc" "abc" #f '((0 3 mark)) '() 0 #f
                                   #f 3 1000)
                       (style:code 'mark))
            #t)

     ;; -- emit-runs ----------------------------------------------------------------

     (let ([out (painted
                  (lambda ()
                    (paint:emit-runs "abcd"
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

     ;; -- hyperlink detection ---------------------------------------------------------

     (check 'detects-http
            (paint:detect-hyperlinks "see http://a.example/x here")
            '((4 22 "http://a.example/x")))
     (check 'trims-trailing-punctuation
            (paint:detect-hyperlinks "at https://e.dev/p, then")
            '((3 18 "https://e.dev/p")))
     (check 'angle-brackets-end-a-url
            (paint:detect-hyperlinks "<http://a.example>")
            '((1 17 "http://a.example")))
     (check 'bare-scheme-skipped
            (paint:detect-hyperlinks "http:// is not a link") '())
     (check 'several-links
            (map caddr (paint:detect-hyperlinks
                         "http://one.example and https://two.example"))
            '("http://one.example" "https://two.example"))

     (check 'valid-hyperlink
            (paint:valid-hyperlink? '(0 5 "http://x") 10) #t)
     (check 'invalid-hyperlink-range
            (paint:valid-hyperlink? '(5 3 "http://x") 10) #f)

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

     (define document (head:new-local-buffer "paint frames"))
     (head:buffer-lines-set! document
       (list->vector
         (map (lambda (row) (format "paint row ~a" row)) (iota 200))))
     (head:add-buffer! document)
     (head:set-window-buffer! (head:current) document)
     ;; Let initial size detection settle, then use a fixed test grid.
     (painted paint:redraw!)
     (paint:set-screen-rows! 24)
     (paint:set-screen-cols! 80)
     ;; A preparation hook may present a notice, causing a direct redraw.
     ;; Both frames prepare at the current width; their synchronized updates
     ;; must not nest, since the inner end would release the outer update.
     (let ([prepared '()])
       (parameterize ([kernel:registering-module 'paint-prepare-test])
         (head:add-pre-redraw-hook!
           (lambda ()
             (set! prepared (cons (list (paint:screen-cols) (head:window-width (head:current))) prepared))
             (when (null? (cdr prepared))
               (paint:show-message! "Prepared\nmessage" #f)))))
       (dynamic-wind
         (lambda () (paint:set-screen-live! #t))
         (lambda ()
           (let ([frame (painted paint:redraw!)])
             (check 'frame-prepares-before-synchronized-paint
               (list prepared (sync-events frame) (contains? frame "Prepared") (contains? frame "message"))
               '(((80 80) (80 80)) (begin end begin end) #t #t))))
         (lambda ()
           (paint:set-screen-live! #f)
           (kernel:retract-module! 'paint-prepare-test)
           (echo:settle!))))
     (define top-before (head:window-top (head:current)))
     (define scrolling
       (let loop ([row 0] [frames '()])
         (if (= row 40)
             (reverse frames)
             (begin
               (head:window-prow-set! (head:current) row)
               (let ([frame (painted paint:redraw!)])
                 (loop (+ row 1) (cons frame frames)))))))
     (check 'scrolling-frames-are-synchronized
            (map sync-events scrolling) (make-list 40 '(begin end)))
     (check 'scrolling-moves-the-viewport
            (> (head:window-top (head:current)) top-before) #t)
     (define (current-top-line)
       (vector-ref (head:buffer-lines document) (head:window-top (head:current))))
     (check 'scrolling-paints-visible-text
            (contains? (car (reverse scrolling)) (current-top-line)) #t)

     ;; Malformed extension output can fail after the update begins.
     ;; The error still reaches the caller, but the host must be released
     ;; and the next frame must rebuild the invalidated shadow.
     (parameterize ([kernel:registering-module 'paint-failure-test])
       (paint:add-highlighter! (lambda () #f)))
     (define failed-output (open-output-string))
     (check 'frame-error-propagates
            (parameterize ([sys:terminal-output-port failed-output])
              (guard (ex [else #t]) (paint:redraw!) #f)) #t)
     (check 'failed-frame-releases-synchronization
            (sync-events (get-output-string failed-output)) '(begin end))
     (kernel:retract-module! 'paint-failure-test)
     (check 'failed-frame-forces-repaint
            (contains? (painted paint:redraw!) (current-top-line)) #t)

     ;; A nonlocal return must follow the same frame lifetime rule.
     (define escaped-output (open-output-string))
     (check 'frame-can-unwind
            (call/cc
              (lambda (escape)
                (parameterize ([kernel:registering-module 'paint-escape-test])
                  (paint:add-highlighter! (lambda () (escape 'escaped))))
                (parameterize ([sys:terminal-output-port escaped-output])
                  (paint:redraw!))
                'returned))
            'escaped)
     (check 'unwound-frame-releases-synchronization
            (sync-events (get-output-string escaped-output)) '(begin end))
     (kernel:retract-module! 'paint-escape-test)
     (check 'unwound-frame-forces-repaint
            (contains? (painted paint:redraw!) (current-top-line)) #t)

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
               (do ([i 0 (+ i 1)]) ((= i 100)) (paint:visual-bell!))))))
       (let ([stop (test:worker
                     (lambda ()
                       (sleep (make-time 'time-duration 0 1))
                       (timed-out? #t)
                       (head:wake-main!)))])
         (dynamic-wind
           (lambda () (paint:set-screen-live! #t))
           (lambda ()
             (let ([result
                    (call/cc
                      (lambda (done)
                        (head:set-frame-hook!
                          (lambda ()
                            (when (timed-out?) (done 'timed-out))
                            (let ([frame (painted paint:redraw!)])
                              (set! frames (cons frame frames))
                              (unless (flashing? frame) (done 'expired)))))
                        (paint:visual-bell!)
                        (head:read-key-event #f)))])
               (check 'bell-retrigger-and-expiry-share-the-owning-pump
                 (list result (map flashing? (reverse frames)) prepared (painters))
                 (list 'expired '(#t #t #t #f) 4 (make-list 4 owner)))
               (check 'bell-frames-restore-the-question-with-synchronized-output
                 (list (map sync-events frames) (echo:text)
                   (contains? (car frames) "Question survives the bell"))
                 (list (make-list 4 '(begin end)) "Question survives the bell" #t))))
           (lambda ()
             (paint:set-screen-live! #f)
             (head:set-frame-hook! void)
             (kernel:retract-module! 'paint-bell-test)
             (stop)))))

     ;; Arming never writes to a captured display. An inactive screen ignores
     ;; it; retirement discards it; even a direct frame expires an overdue bell.
     (check 'bell-lifetime-without-an-input-pump
       (map
         (lambda (state)
           (paint:set-screen-live! (not (eq? state 'inactive)))
           (let ([old-display (open-output-string)])
             (parameterize ([sys:terminal-output-port old-display]) (paint:visual-bell!))
             (when (eq? state 'retired)
               (paint:set-screen-live! #f)
               (paint:set-screen-live! #t))
             (let ([frame (and (not (eq? state 'expired)) (painted paint:redraw!))])
               (sleep (make-time 'time-duration 75000000 0))
               (let ([frame (or frame (painted paint:redraw!))])
                 (paint:set-screen-live! #f)
                 (list (get-output-string old-display) (flashing? frame)
                   (sync-events frame) (contains? frame "Question survives the bell"))))))
         '(inactive retired expired))
       (make-list 3 '("" #f (begin end) #t)))

     (test:finish! 'paint)))
