#!/usr/bin/env scheme-script

;; End-to-end drive of the real editor: spawn ./e on a PTY, mirror its
;; display into the headless terminal emulator, and interact with a nested
;; terminal the way a user would. This layer covers behavior only a live
;; editor exercises: scrollback presentation, recoloring of cached
;; scrollback rows after a palette change, ED 3 sent by a real shell, and
;; clean shutdown. Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (terminal) terminal:))

     ;; The nested terminal must run a predictable shell.
     (putenv "SHELL" "/bin/sh")

     (define checks 0)
     (define mirror (terminal:make-emulator 24 80))
     (define process
       (sys:spawn-terminal-process "/bin/sh" "exec ./e" (current-directory) 24 80))
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
             (terminal:emulator-feed! mirror (string character))
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
       (vector->list (terminal:emulator-screen mirror)))

     (define (fail! label)
       (for-each (lambda (line) (display (format "|~a|\n" line)))
                 (screen-lines))
       (error 'interactive-test label))

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

     (define (find-cell part)
       ;; (row . column) of the first screen position showing part.
       (let loop ([lines (screen-lines)] [row 0])
         (cond
           [(null? lines) #f]
           [(contains? (car lines) part)
            (let ([line (car lines)] [m (string-length part)])
              (let find ([at 0])
                (if (string=? (substring line at (+ at m)) part)
                    (cons row at)
                    (find (+ at 1)))))]
           [else (loop (cdr lines) (+ row 1))])))

     (define (style-at cell)
       (vector-ref (vector-ref (terminal:emulator-styles mirror) (car cell))
                   (cdr cell)))

     ;; -- start the editor and open a nested terminal ---------------------
     (wait-for! 'editor-starts
                (lambda () (find-cell "*scratch*")) 30000)
     (send! "\x3;t")                    ; C-c t
     (wait-for! 'nested-terminal-opens
                (lambda () (find-cell "capturing input")) 10000)

     ;; -- C-] gets out of the capture -------------------------------------
     ;; The terminal's handler can encode C-] for the child, so the
     ;; dispatcher must hand the context's escape to the keymaps before
     ;; the handler sees it (regression: the escape became keymap data
     ;; while the handler kept first refusal, and C-] went to the shell).
     (send! "\x1d;")                      ; C-]
     (wait-for! 'escape-shows-in-the-status-line
                (lambda () (find-cell "▶ escaped")) 5000)
     (send! "\x1b;x")                     ; M-x
     (wait-for! 'escape-opens-the-global-prompt
                (lambda () (and (find-cell "M-x (") (find-cell "▶ escaped")))
                5000)
     (send! "\x7;")                       ; C-g: back to the capture
     (wait-for! 'capture-resumes-after-the-command
                (lambda () (and (find-cell "capturing input")
                                (not (find-cell "▶ escaped"))))
                5000)
     (send! "cat -v\r")
     (settle! 500)
     (send! "\x1d;\x1d;\r")               ; C-] C-]: the literal character
     (wait-for! 'literal-escape-reaches-the-child
                (lambda () (find-cell "^]")) 5000)
     (send! "\x4;")                       ; C-d ends cat
     (settle! 500)

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
              (not (eq? red-style (style-at (find-cell "red line 2 "))))))

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
     (send! "exit\r")
     (wait-for! 'shell-exit-frees-buffer
                (lambda () (not (find-cell "capturing input"))) 10000)
     ;; The dead terminal must not log a failed refresh: its detachment
     ;; happens on the main thread, never under a frame in progress
     ;; (regression: the reader thread detached the app mid-refresh).
     (settle! 500)
     (check 'shell-exit-refreshes-cleanly (not (find-cell "refresh failed")))
     ;; -- window navigation inside a prompt ------------------------------
     ;; Split, start find-file, move focus right mid-prompt, accept: the
     ;; file must open in the newly focused right-hand window.
     (send! "\x18;3")                   ; C-x 3
     (settle! 500)
     (send! "\x18;\x6;")                ; C-x C-f
     (settle! 500)
     (send! "\x1b;[1;3C")               ; M-RIGHT, prompt keeps running
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
     ;; M-x, a partial name, TAB: the *completions* view takes the window
     ;; and lists the candidates; C-g hands the window's buffer back.  A
     ;; second prompt reuses the same view.
     (send! "\x1b;x")                   ; M-x
     (settle! 500)
     (send! "split-w\t\t")
     (wait-for! 'completions-take-the-window
                (lambda () (and (find-cell "*completions*")
                                (find-cell "split-window!")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'completions-give-the-window-back
                (lambda () (and (not (find-cell "*completions*"))
                                (find-cell "*terminal*")))
                5000)
     (send! "\x1b;x")
     (settle! 500)
     (send! "split-w\t\t")
     (wait-for! 'completions-view-reused
                (lambda () (find-cell "split-window!")) 5000)
     (send! "\x7;")
     (settle! 500)

     (send! "\x18;\x3;")                ; C-x C-c
     (let loop ()                       ; block until the editor exits
       (let ([character (guard (ex [else (eof-object)])
                          (get-char from-editor))])
         (unless (eof-object? character) (loop))))
     (sys:reap-terminal-process! process)
     (check 'editor-quits #t)

     (format #t "~a interactive checks passed\n" checks)))
