#!/usr/bin/env scheme-script

;; End-to-end drive of the real editor: spawn ./e on a PTY, mirror its
;; display into the headless terminal emulator, and interact with a nested
;; terminal the way a user would. This layer keeps what only a live process
;; proves: attaching to a base, resizing under a pending question, capture
;; routing into a real shell, scrollback of real output, host theme
;; forwarding, process exit, M-x completion in the borrowed window, a live
;; describe page, and a clean detach. Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

;; The nested terminal must run a predictable shell. The base spawns the
;; terminal children, so it must inherit this before it starts; the head
;; inherits the evaluation-mail switch the same way.
(putenv "SHELL" "/bin/sh")
(putenv "E_TEST_EVAL" "1")

(define interactive-scenario
  '(begin
     (import (prefix (sys) sys:) (prefix (vt) vt:) (prefix (glyph) glyph:)
             (prefix (wire) wire:) (prefix (kernel) kernel:) (prefix (string) string:))

     (define checks 0)
     (define mirror (vt:make-emulator 24 80))
     (define process
       (sys:spawn-terminal-process "/bin/sh" (fixture:command test-base "--name" "interactive")
                                   (current-directory) 24 80))
     (define transcript '())

     (define drain!
       (fixture:terminal-reader process
         (lambda (text)
           (set! transcript (cons text transcript))
           (vt:emulator-feed! mirror text))))

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
       (and (string:search text part 0 (string-length text)) #t))

     (define (transcript-text) (apply string-append (reverse transcript)))

     (define (find-cell part)
       ;; (row . column) of the first screen position showing part.
       (let loop ([lines (screen-lines)] [row 0])
         (cond
           [(null? lines) #f]
           [(string:search (car lines) part 0 (string-length (car lines)))
            => (lambda (at) (cons row (glyph:cells (substring (car lines) 0 at))))]
           [else (loop (cdr lines) (+ row 1))])))

     (define (style-at cell)
       (vector-ref (vector-ref (vt:emulator-styles mirror) (car cell))
                   (cdr cell)))

     ;; -- attach, then resize while idle and inside a populated prompt. The
     ;; pending question must use the new width in that frame, before another
     ;; key. A terminal mirror can reflow old cells itself, so require actual
     ;; frame output too.
     (wait-for! 'editor-starts
                (lambda () (find-cell "*scratch*")) 30000)
     ;; Evaluation mail reads the head's state without typing into M-x.
     (define evaluator
       (fixture:evaluator (current-directory) (fixture:directory test-base) '(agent "evaluator")))
     (define (evaluate expression) (fixture:evaluate evaluator '(head "interactive") expression))
     (define resize-question
       "This pending question expands to the new terminal width before another key.")
     (define asker (sys:connect-local (string-append (fixture:directory test-base) "/socket")))
     (wire:send! (sys:connection-output asker) (list 'hello wire:version '(head "resize") (kernel:fingerprint)))
     (wire:receive (sys:connection-input asker))
     (wire:send! (sys:connection-output asker) (list 'request 1 'ask '(head "interactive") resize-question '()))
     (wire:receive (sys:connection-input asker))
     (send! "\x1;")                    ; C-a settles the startup echo
     (wait-for! 'resize-question-starts-elided
       (lambda () (and (find-cell "(head") (find-cell "asks:") (not (find-cell resize-question)))) 3000)
     (for-each
       (lambda (case)
         (let ([prompt? (car case)] [rows (cadr case)] [cols (caddr case)])
           (when prompt?
             (send! "\x1b;xresize-input")
             (wait-for! 'prompt-holds-input (lambda () (find-cell "M-x (resize-input")) 3000))
           (set! transcript '())
           (vt:emulator-resize! mirror rows cols)
           (sys:resize-terminal-process! process rows cols)
           (wait-for! (list 'idle-resize-refreshes-the-screen prompt?)
             (lambda ()
               (let ([buffer (find-cell "*scratch*")] [close (find-cell "│×│")])
                 (and (contains? (transcript-text) "\x1b;[?2026h")
                      buffer close (= (car buffer) (- rows 2)) (= (cdr close) (- cols 3))
                      (if prompt? (find-cell "M-x (resize-input")
                        (find-cell resize-question))))) 3000)))
       '((#f 18 160) (#t 24 80)))
     (send! "\x7;")                     ; C-g leaves the prompt
     (sys:close-connection! asker)

     ;; -- a nested terminal: default partial capture lets whole editor
     ;; commands through while other keys reach the child; full capture
     ;; forwards the editor prefixes too -------------------------------------
     (send! "\x3;t")                    ; C-c t
     (wait-for! 'nested-terminal-opens
                (lambda () (and (find-cell "▶ ◐") (find-cell "C-] toggle capture"))) 10000)
     (send! "\x1b;x")                     ; M-x
     (wait-for! 'partial-capture-opens-the-global-prompt
                (lambda () (and (find-cell "M-x (") (find-cell "▶ ◐")))
                5000)
     (send! "\x7;\x18;2")                 ; cancel, C-x 2
     (wait-for! 'partial-capture-prefix-splits-the-terminal
       (lambda () (= 2 (length (filter (lambda (line) (contains? line "▶ ◐")) (screen-lines))))) 5000)
     (send! "\x18;1")                     ; C-x 1
     (wait-for! 'partial-capture-prefix-closes-the-split
       (lambda () (= 1 (length (filter (lambda (line) (contains? line "▶ ◐")) (screen-lines))))) 5000)
     (send! "printf sta''rted; cat -v\r")
     (wait-for! 'child-command-runs (lambda () (find-cell "started")) 5000)
     (send! "\x1d;\x18;\x1b;x\r")         ; full capture, then child C-x/M-x
     (wait-for! 'full-capture-forwards-editor-prefixes
       (lambda () (and (find-cell "▶ ●") (find-cell "^X^[x"))) 5000)
     (send! "\x1d;")
     (wait-for! 'toggle-restores-partial-capture (lambda () (find-cell "▶ ◐")) 3000)
     (send! "\x4;")                       ; C-d ends cat

     ;; -- scrollback: real output scrolls away and comes back styled ---------
     (send! "for i in $(seq 1 40); do printf '\\033[31mred line %d\\033[0m\\n' \"$i\"; done\r")
     (wait-for! 'output-reaches-live-screen
                (lambda () (find-cell "red line 40")) 10000)
     (check 'early-lines-scrolled-away (not (find-cell "red line 1 ")))
     (send! "\x1b;[5;2~")               ; S-PAGEUP
     (wait-for! 'scrollback-shows-early-lines
                (lambda () (find-cell "red line 2 ")) 5000)
     (check 'scrollback-line-is-styled (not (eq? (style-at (find-cell "red line 2 ")) 'plain)))
     (send! "\x1b;[6;2~")               ; S-PAGEDOWN back to the live screen
     (wait-for! 'live-screen-returns (lambda () (find-cell "red line 40")) 5000)

     ;; -- host color-scheme reports forward to subscribed children -------
     ;; The child subscribes with ?2031h and blocks reading its terminal;
     ;; the host (this driver) then reports a light scheme to e, which
     ;; must forward it into the child's PTY.
     (send! "printf 'subs''cribed\\033[?2031h'; head -c 9 | cat -v; echo\r")
     (wait-for! 'child-subscribes (lambda () (find-cell "subscribed")) 5000)
     (send! "\x1b;[?997;2n")
     (wait-for! 'host-theme-report-forwarded
                (lambda () (find-cell "997;2")) 3000)

     ;; -- process exit frees the buffer: the capture control and its hint
     ;; leave with the process ---------------------------------------------
     (send! "exit\r")
     (wait-for! 'shell-exit-frees-buffer
       (lambda () (and (find-cell "■") (not (find-cell "■ ●")) (not (find-cell "■ ◐"))
                       (not (find-cell "C-] toggle capture")))) 10000)

     ;; -- M-x completion borrows the window: the first Tab normalizes without
     ;; choosing, the second lists candidates where the buffer was, and the
     ;; prompt's end hands the window back ------------------------------------
     (send! "\x1b;xwindowsplit\t")
     (wait-for! 'first-tab-normalizes-without-choosing
       (lambda () (and (find-cell "M-x (split-window")
                       (not (find-cell "<completions>")))) 5000)
     (send! "\t")
     (wait-for! 'completions-take-the-window
                (lambda () (and (find-cell "<completions>")
                                (find-cell "split-window! [2 segments]")
                                (find-cell "split-window-right! [2 segments]")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'completions-give-the-window-back
                (lambda () (not (find-cell "<completions>"))) 5000)
     ;; Completing a sole candidate includes punctuation and remains executable.
     (send! "\x1b;xspwir\t\r")
     (wait-for! 'normalized-full-match-can-run
       (lambda () (and (find-cell "0▏") (find-cell "1▏"))) 5000)
     (send! "\x18;0")
     (wait-for! 'close-the-test-split
       (lambda () (not (and (find-cell "0▏") (find-cell "1▏")))) 5000)
     ;; Subword prefixes may reorder. Complete a nested operator from inside
     ;; its token, retaining arguments; Enter runs the completed expression.
     (send! (string-append "\x1b;xlist (appstring \"a\" \"b\"))\x1;" (make-string 10 (integer->char 6)) "\t"))
     (wait-for! 'completion-replaces-only-the-token-at-point
       (lambda () (find-cell "M-x (list (string-append \"a\" \"b\"))")) 5000)
     (send! "\r")
     (wait-for! 'completed-expression-keeps-its-arguments
       (lambda () (find-cell "=> (\"ab\")")) 5000)

     ;; -- a private source and its rendered local companion ------------------
     (send! "\x8;fmarkdown:view!\r") ; C-h f, then the documented name
     (wait-for! 'describe-opens-a-rendered-page (lambda () (find-cell "<describe>")) 5000)
     ;; The page opens beside the requesting window, whose short viewport may
     ;; scroll the header away: read the rendered companion itself.
     (check 'describe-source-is-shared-and-private-to-the-requester
       (evaluate
         '(let* ([page (reference:page head:ui-actor)] [id (car page)]
                 [source (head:buffer-of-store-id id)] [view (markdown:companion source)])
            (and (store:exists? id) (equal? (store:property id 'audience) (list head:ui-actor))
              (store:visible? head:ui-actor id) (not (store:visible? '(head "interactive-other") id))
              (head:buffer-read-only source) (not (head:buffer-store-id view))
              (eq? source (head:buffer-fact view 'markdown-input #f))
              (exists (lambda (line) (string:prefix? "procedure: (markdown:view!" line))
                (vector->list (head:buffer-lines view)))))))

     (send! "\x18;\x3;")                ; C-x C-c
     (test:await 'editor-exits drain!)
     (sys:reap-terminal-process! process)
     (check 'editor-quits #t)

     (format #t "~a interactive checks passed\n" checks)))

(eval
  `(begin
     (import (prefix (fixture) fixture:) (prefix (test) test:))
     (fixture:call-with-base (current-directory) #f
       (lambda (base)
         (set-top-level-value! 'test-base base)
         (eval ',interactive-scenario)))))
