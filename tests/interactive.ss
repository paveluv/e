#!/usr/bin/env scheme-script

;; End-to-end drive of the real editor: spawn ./e on a PTY, mirror its
;; display into the headless terminal emulator, and interact with a nested
;; terminal the way a user would. This layer covers behavior only a live
;; editor exercises: scrollback presentation, recoloring of cached
;; scrollback rows after a palette change, ED 3 sent by a real shell, and
;; clean shutdown. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(define interactive-scenario
  '(begin
     (import (prefix (sys) sys:) (prefix (vt) vt:) (prefix (glyph) glyph:)
             (prefix (wire) wire:) (prefix (kernel) kernel:))

     ;; The nested terminal must run a predictable shell.
     (putenv "SHELL" "/bin/sh")

     (define checks 0)
     (define mirror (vt:make-emulator 24 80))
     (define process
       (sys:spawn-terminal-process "/bin/sh" (fixture:command test-base "--name" "interactive")
                                   (current-directory) 24 80))
     (define from-editor
       (transcoded-port (sys:terminal-process-input process)
                        (make-transcoder (utf-8-codec) 'none 'replace)))
     (define transcript '())

     (define (drain!)
       (when (guard (ex [else #f]) (char-ready? from-editor))
         (let ([character (guard (ex [else (eof-object)])
                            (get-char from-editor))])
           (unless (eof-object? character)
             (set! transcript (cons character transcript))
             (vt:emulator-feed! mirror (string character))
             (drain!)))))

     (define (settle! milliseconds)
       (let loop ([left (div milliseconds 25)])
         (drain!)
         (when (> left 0)
           (sleep (make-time 'time-duration 25000000 0))
           (loop (- left 1)))))

     (define (send! text)
       (let ([output (sys:terminal-process-output process)])
         (put-bytevector output (string->utf8 text))
         (flush-output-port output)))

     (define (screen-lines)
       (vector->list (vt:emulator-screen mirror)))

     (define (fail! label)
       (for-each (lambda (line) (display (format "|~a|\n" line)))
         (screen-lines))
       (error 'interactive-test (format "~s" label)))

     (define (check label true?)
       (set! checks (+ checks 1))
       (unless true? (fail! label)))

     (define (wait-for! label predicate milliseconds)
       ;; Poll the editor's output until the screen satisfies predicate;
       ;; a stage that never settles fails with the final screen shown.
       (set! checks (+ checks 1))
       (let loop ([left (div milliseconds 25)])
         (drain!)
         (or (predicate)
             (if (= left 0)
                 (fail! label)
                 (begin
                   (sleep (make-time 'time-duration 25000000 0))
                   (loop (- left 1)))))))

     (define (contains? text part)
       (let ([n (string-length text)] [m (string-length part)])
         (let loop ([at 0])
           (and (<= (+ at m) n)
                (or (string=? (substring text at (+ at m)) part)
                  (loop (+ at 1)))))))

     (define (find-cell part . start)
       ;; (row . column) of the first screen position showing part.
       (let loop ([lines (list-tail (screen-lines) (if (pair? start) (car start) 0))]
                  [row (if (pair? start) (car start) 0)])
         (cond
           [(null? lines) #f]
           [(contains? (car lines) part)
            (let ([line (car lines)] [m (string-length part)])
              (let find ([at 0])
                (if (string=? (substring line at (+ at m)) part)
                    (cons row (glyph:cells (substring line 0 at)))
                    (find (+ at 1)))))]
           [else (loop (cdr lines) (+ row 1))])))

     (define (style-at cell)
       (vector-ref (vector-ref (vt:emulator-styles mirror) (car cell))
                   (cdr cell)))

     ;; -- start the editor and open a nested terminal ---------------------
     (wait-for! 'editor-starts
                (lambda () (find-cell "*scratch*")) 30000)

     ;; Resize while idle and inside a populated prompt. The pending question
     ;; must use the new width in that frame, before another key. A terminal
     ;; mirror can reflow old cells itself, so require actual frame output too.
     (define resize-question
       "This pending question expands to the new terminal width before another key.")
     (define asker (sys:connect-local (string-append (fixture:directory test-base) "/socket")))
     (wire:send! (sys:connection-output asker) (list 'hello wire:version '(head "resize") (kernel:fingerprint)))
     (wire:receive (sys:connection-input asker))
     (wire:send! (sys:connection-output asker) (list 'request 1 'ask '(head "interactive") resize-question '()))
     (wire:receive (sys:connection-input asker))
     (settle! 200)
     (send! "\x1;")                    ; C-a settles the evaluation's echo log
     (settle! 150)
     (wait-for! 'resize-question-starts-elided
       (lambda () (and (find-cell "(head") (find-cell "asks:") (not (find-cell resize-question)))) 3000)
     (for-each
       (lambda (case)
         (let ([prompt? (car case)] [rows (cadr case)] [cols (caddr case)])
           (when prompt? (send! "\x1b;xresize-input") (settle! 150))
           (set! transcript '())
           (vt:emulator-resize! mirror rows cols)
           (sys:resize-terminal-process! process rows cols)
           (wait-for! (list 'idle-resize-refreshes-the-screen prompt?)
             (lambda ()
               (let ([buffer (find-cell "*scratch*")] [close (find-cell "│×│")])
                 (and (contains? (list->string (reverse transcript)) "\x1b;[?2026h")
                      buffer close (= (car buffer) (- rows 2)) (= (cdr close) (- cols 3))
                      (if prompt? (find-cell "M-x (resize-input")
                        (find-cell resize-question))))) 3000)))
       '((#f 18 160) (#t 24 80)))
     (settle! 200)
     (set! transcript '())
     (settle! 350)
     (check 'idle-signal-checks-do-not-paint (null? transcript))
     (send! "\x7;")
     (sys:close-connection! asker)
     (settle! 150)

     ;; Invalid single-key input flashes the echo area, then restores the
     ;; question while its nested pump remains idle. Inspect the transcript
     ;; too: the brief flash can start and end between two polling passes.
     (send! "\x1b;xlist (prompt:key! \"Bell check: y)es or n)o\" \"yn\") (quote bell-answer)\r")
     (wait-for! 'single-key-question-opens
       (lambda () (find-cell "Bell check: yes or no")) 5000)
     (set! transcript '())
     (send! "x")
     (wait-for! 'invalid-question-key-flashes-and-restores-without-input
       (lambda ()
         (and (contains? (list->string (reverse transcript))
                (string-append "\x1b;[7m" (make-string 80 #\space) "\x1b;[0m"))
              (find-cell "Bell check: yes or no"))) 5000)
     (send! "n")
     (wait-for! 'bell-leaves-the-question-answerable
       (lambda () (find-cell "(#\\n bell-answer)")) 5000)

     (send! "\x3;t")                    ; C-c t
     (wait-for! 'nested-terminal-opens
                (lambda () (and (find-cell "▶ ◐") (find-cell "C-] toggle capture"))) 10000)

     ;; Default capture lets whole editor commands through; other keys are
     ;; still the child's. The toggle is immediate and never reaches it.
     (send! "\x1b;x")                     ; M-x
     (wait-for! 'partial-capture-opens-the-global-prompt
                (lambda () (and (find-cell "M-x (") (find-cell "▶ ◐")))
                5000)
     (send! "\x7;\x18;2")                 ; cancel, C-x 2
     (wait-for! 'partial-capture-prefix-splits-the-terminal
       (lambda () (= 2 (length (filter (lambda (line) (contains? line "▶ ◐")) (screen-lines))))) 5000)
     (let ([status (find-cell "▶ ◐" (+ (car (find-cell "▶ ◐")) 1))])
       (send! (format "\x1b;[<0;~a;~aM\x1b;[<0;~a;~am"
                      (+ (cdr status) 3) (+ (car status) 1) (+ (cdr status) 3) (+ (car status) 1)))
       (wait-for! 'click-focuses-and-toggles-only-the-pointed-terminal-window
         (lambda () (and (find-cell "▶ ●") (find-cell "▶ ◐"))) 3000)
       (send! "\x1d;"))                   ; restore partial capture in the newly focused window
     (send! "\x18;1\x18;k")               ; C-x 1, C-x k
     (wait-for! 'partial-capture-prefix-opens-kill-buffer (lambda () (find-cell "Kill buffer (default")) 5000)
     (send! "\x7;\x1b;xhead:buffer-name-set! (current-buffer) \"界● term\"\r")
     (settle! 300)
     (send! "cat -v\r")
     (settle! 500)
     (send! "\x1d;\x18;\x1b;x\r")         ; full capture, then child C-x/M-x
     (wait-for! 'full-capture-forwards-editor-prefixes
       (lambda () (and (find-cell "▶ ●") (find-cell "^X^[x"))) 5000)
     ;; A capture symbol in the buffer name is just text; the actual control
     ;; uses its single painted cell, even after a wide buffer-name prefix.
     (let ([decoy (find-cell "●")])
       (send! (format "\x1b;[<0;~a;~aM\x1b;[<0;~a;~am"
                      (+ (cdr decoy) 1) (+ (car decoy) 1) (+ (cdr decoy) 1) (+ (car decoy) 1)))
       (settle! 150)
       (check 'capture-symbol-in-buffer-name-is-inert (find-cell "▶ ●")))
     (for-each
       (lambda (icons)
         (let* ([status (find-cell (car icons))] [column (+ (cdr status) 2)] [row (car status)])
           (send! (format "\x1b;[<35;~a;~aM" (+ column 1) (+ row 1)))
           (settle! 150)
           (let ([style (string-append ";" (style-at (cons row column)) ";")])
             (check 'capture-control-is-single-cell-with-bold-dotted-hover
                    (and (= (glyph:cells (car icons)) 3) (contains? style ";1;") (contains? style ";4:4;")
                      (not (contains? (format ";~a;" (style-at (cons row (+ column 1)))) ";4:4;")))))
           (send! (format "\x1b;[<0;~a;~aM\x1b;[<0;~a;~am"
                          (+ column 1) (+ row 1) (+ column 1) (+ row 1)))
           (wait-for! 'click-toggles-capture (lambda () (find-cell (cadr icons))) 3000)))
       '(("▶ ●" "▶ ◐") ("▶ ◐" "▶ ●")))
     (send! "\x1d;")
     (settle! 150)
     (check 'capture-control-never-sends-its-byte (and (find-cell "▶ ◐") (not (find-cell "^]"))))
     (send! "\x4;")                       ; C-d ends cat
     (settle! 500)
     (send! "\x1b;xhead:buffer-name-set! (current-buffer) \"*terminal*\"\r")
     (settle! 200)

     ;; -- fill scrollback with palette-red lines --------------------------
     (send! "for i in $(seq 1 40); do printf '\\033[31mred line %d\\033[0m\\n' \"$i\"; done\r")
     (wait-for! 'output-reaches-live-screen
                (lambda () (find-cell "red line 40")) 10000)
     (check 'early-lines-scrolled-away (not (find-cell "red line 1 ")))

     ;; -- scroll back: history rows present, correct, and styled ----------
     (send! "\x1b;[5;2~")               ; S-PAGEUP
     (wait-for! 'scrollback-shows-early-lines
                (lambda () (find-cell "red line 2 ")) 5000)
     (let ([red-style (style-at (find-cell "red line 2 "))])
       (check 'scrollback-line-is-styled (not (eq? red-style 'plain)))

       ;; -- change palette color 1; cached history rows must recolor ------
       (send! "\x1b;[6;2~")             ; S-PAGEDOWN back to the live screen
       (settle! 500)
       (set! transcript '())
       (send! "printf '\\033]4;1;#0055ff\\007'\r")
       (wait-for! 'recolor-uses-new-palette-rgb
                  (lambda ()
                    (contains? (list->string (reverse transcript))
                               "38;2;0;85;255"))
                  5000)
       (send! "\x1b;[5;2~")             ; S-PAGEUP into history again
       (wait-for! 'scrollback-still-shows-line
                  (lambda () (find-cell "red line 2 ")) 5000)
       (check 'palette-change-recolors-cached-rows
              (not (equal? red-style (style-at (find-cell "red line 2 "))))))

     ;; -- clear(1)'s ED 3 erases the scrollback ---------------------------
     (send! "\x1b;[6;2~")
     (settle! 500)
     (send! "printf '\\033[H\\033[2J\\033[3J'\r")
     (wait-for! 'screen-cleared
                (lambda () (not (find-cell "red line"))) 5000)
     (send! "\x1b;[5;2~\x1b;[5;2~")     ; page up twice: nothing above
     (settle! 800)
     (check 'ed3-empties-scrollback (not (find-cell "red line")))

     ;; -- host color-scheme reports forward to subscribed children -------
     ;; The child subscribes with ?2031h and blocks reading its terminal;
     ;; the host (this driver) then reports a light scheme to e, which
     ;; must forward it into the child's PTY.
     (send! "printf '\\033[?2031h'; head -c 9 | cat -v; echo\r")
     (settle! 800)
     (send! "\x1b;[?997;2n")
     (wait-for! 'host-theme-report-forwarded
                (lambda () (find-cell "997;2")) 3000)

     ;; -- diagnostics from the reader thread must not stall the frame ----
     ;; An unsupported sequence logs from the PTY reader thread; the output
     ;; after it must still appear without any further input arriving
     ;; (regression: the console ports share a lock with the main thread's
     ;; blocking keyboard read).
     (send! "printf '\\033[9999z'; printf 'after-report'; sleep 3\r")
     (wait-for! 'reader-thread-log-does-not-stall-output
                (lambda () (find-cell "after-report")) 2000)
     (settle! 3200)

     ;; -- shut down cleanly ----------------------------------------------
     (send! "\x1b;[6;2~")
     (settle! 300)
     (set! transcript '())
     (send! "\x1d;exit\r")              ; exit while full capture is selected
     (wait-for! 'shell-exit-frees-buffer
       (lambda () (and (find-cell "■") (not (find-cell "■ ●")) (not (find-cell "■ ◐"))
                       (not (find-cell "C-] toggle capture")))) 10000)
     ;; The dead terminal must not log a failed refresh: its detachment
     ;; happens on the main thread, never under a frame in progress
     ;; (regression: the reader thread detached the app mid-refresh).
     (settle! 500)
     (check 'shell-exit-refreshes-cleanly (not (find-cell "refresh failed")))
     ;; An ordinary read-only buffer now: the app's blinking block gives
     ;; way to the read-only bar (DECSCUSR 5), since app presentation
     ;; facts apply only while an app owns the buffer.
     (wait-for! 'dead-terminal-shows-the-read-only-cursor
                (lambda ()
                  (contains? (list->string (reverse transcript)) "\x1b;[5 q"))
                5000)
     (for-each
       (lambda (case)
         (send! (car case))
         (wait-for! (list 'dead-terminal-capture-control-is-unavailable (cadr case))
           (lambda () (find-cell (cadr case))) 3000))
       '(("\x1d;" "C-] is undefined")
         ("\x1b;xterminal:toggle-capture!\r" "not a live terminal")))
     ;; -- window navigation and a window prompt --------------------------
     ;; Split, start find-file, move focus right: the prompt, which lives
     ;; in its window, cancels as focus leaves.  Find the file again from
     ;; the right-hand window: it opens there.
     (send! "\x18;3")                   ; C-x 3
     (settle! 500)
     (send! "\x1b;xfind-file!!\r")       ; direct prompt through M-x
     (settle! 500)
     (send! "\x1b;[1;3C")               ; M-RIGHT cancels the window prompt
     (settle! 500)
     (send! "\x1b;xfind-file!!\r")       ; now in the right window
     (settle! 500)
     (send! "README.md\r")
     (wait-for! 'prompt-navigation-targets-focused-window
                (lambda ()
                  (let ([readme (find-cell "README.md  L1")]
                        [left (find-cell "*terminal*")])
                    (and readme left
                         (= (car readme) (car left))
                         (> (cdr readme) (cdr left)))))
                5000)
     (send! "\x18;0")                   ; C-x 0: back to one window
     (settle! 500)

     ;; -- completions borrow the window and give it back ------------------
     ;; M-x, a partial name, TAB: the <completions> view takes the window
     ;; and lists the candidates; C-g hands the window's buffer back.  A
     ;; second prompt creates its own view.
     (send! "\x1b;x")                   ; M-x
     (settle! 500)
     (send! "split-w\t\t")
     (wait-for! 'completions-take-the-window
                (lambda () (and (find-cell "<completions>")
                                (find-cell "split-window!")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'completions-give-the-window-back
                (lambda () (and (not (find-cell "<completions>"))
                                (find-cell "*terminal*")))
                5000)
     (send! "\x1b;x")
     (settle! 500)
     (send! "split-w\t\t")
     (wait-for! 'completions-view-opens-again
                (lambda () (find-cell "split-window!")) 5000)
     (send! "\x7;")
     (settle! 500)

     ;; -- M3 exit: a private source and its rendered local companion ------
     (send! "\x8;fmarkdown:view!\r") ; C-h f, then the documented name
     (wait-for! 'describe-opens-a-rendered-page
                (lambda () (and (find-cell "<describe>") (find-cell "procedure: (markdown:view!")))
                5000)
     (send!
       (format "\x1b;xlist ~s (quote describe-source-check)\r"
               '(let* ([page (reference:page head:ui-actor)] [id (car page)]
                       [source (head:buffer-of-store-id id)] [view (markdown:companion source)])
                  (and (store:exists? id) (equal? (store:property id 'audience) (list head:ui-actor))
                    (store:visible? head:ui-actor id) (not (store:visible? '(head "interactive-other") id))
                    (head:buffer-read-only source) (not (head:buffer-store-id view))
                    (eq? source (head:buffer-fact view 'markdown-input #f))))))
     (wait-for! 'describe-source-is-shared-and-private-to-the-requester
                (lambda ()
                  ;; M-x can wrap its result across terminal rows.
                  (contains?
                    (list->string
                      (filter (lambda (c) (not (memv c '(#\space #\\))))
                              (string->list (apply string-append (screen-lines)))))
                    "(#tdescribe-source-check)")) 5000)

     (send! "\x18;\x3;")                ; C-x C-c
     (let loop ()                       ; block until the editor exits
       (let ([character (guard (ex [else (eof-object)])
                          (get-char from-editor))])
         (unless (eof-object? character) (loop))))
     (sys:reap-terminal-process! process)
     (check 'editor-quits #t)

     (format #t "~a interactive checks passed\n" checks)))

(eval
  `(begin
     (import (prefix (fixture) fixture:))
     (fixture:call-with-base (current-directory) #f
       (lambda (base)
         (set-top-level-value! 'test-base base)
         (eval ',interactive-scenario)))))
