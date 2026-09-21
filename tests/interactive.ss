#!/usr/bin/env scheme-script

;; End-to-end drive of the real editor: spawn ./e on a PTY, mirror its
;; display into the headless terminal emulator, and interact with a nested
;; terminal the way a user would. This layer keeps what only a live process
;; proves: attaching to a base, resizing under a pending question, capture
;; routing into a real shell, scrollback of real output, host theme
;; forwarding, process exit, M-x completion in the pop-up window, a live
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
     (import (prefix (sys sys) sys:) (prefix (service vt) vt:) (prefix (sys glyph) glyph:)
             (prefix (foundation wire) wire:) (prefix (core kernel) kernel:) (prefix (foundation string) string:))

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
             (wait-for! 'prompt-holds-input (lambda () (find-cell "λ (resize-input")) 3000))
           (set! transcript '())
           (vt:emulator-resize! mirror rows cols)
           (sys:resize-terminal-process! process rows cols)
           ;; The echo area is a box of at most 100 columns, recentered on the
           ;; new width: the question fits its first row behind the border,
           ;; the answer hint wraps to a second, and the status line sits above.
           (wait-for! (list 'idle-resize-refreshes-the-screen prompt?)
             (lambda ()
               (let ([buffer (find-cell "*scratch*")] [close (find-cell "│×│")]
                     [edge (find-cell (if prompt? "┊λ (resize-input" "┊(head"))]
                     [echo-rows (if prompt? 1 2)])
                 (and (contains? (transcript-text) "\x1b;[?2026h")
                      buffer close (= (car buffer) (- rows 1 echo-rows)) (= (cdr close) (- cols 3))
                      edge (= (cdr edge) (quotient (- cols (min cols 100)) 2))
                      (or prompt? (find-cell resize-question))))) 3000)))
       '((#f 18 160) (#t 24 80)))
     (send! "\x7;")                     ; C-g leaves the prompt
     (sys:close-connection! asker)

     ;; -- a nested terminal: default partial capture lets whole editor
     ;; commands through while other keys reach the child; full capture
     ;; forwards the editor prefixes too -------------------------------------
     (send! "\x3;t")                    ; C-c t
     (wait-for! 'nested-terminal-opens
                (lambda () (and (find-cell "▶ ◐") (not (find-cell "toggle capture")))) 10000)
     (send! "\x1b;x")                     ; M-x
     (wait-for! 'partial-capture-opens-the-global-prompt
                (lambda () (and (find-cell "λ (") (find-cell "▶ ◐")))
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

     ;; -- process exit frees the buffer: the capture control leaves with
     ;; the process ------------------------------------------------------------
     (send! "exit\r")
     (wait-for! 'shell-exit-frees-buffer
       (lambda () (and (find-cell "■") (not (find-cell "■ ●")) (not (find-cell "■ ◐")))) 10000)

     ;; -- M-x completion opens the pop-up window: the first Tab normalizes
     ;; without choosing, the second lists candidates above the echo area, and
     ;; the prompt's end hides the pop-up again -------------------------------
     (send! "\x1b;xwindowsplit\t")
     (wait-for! 'first-tab-normalizes-without-choosing
       (lambda () (and (find-cell "λ (window:split-")
                       (not (find-cell "<completions>")))) 5000)
     (send! "\t")
     (wait-for! 'completions-take-the-window
                (lambda () (and (find-cell "<completions>  4 matches of symbol")
                                ;; each candidate carries its edoc hint, one per
                                ;; row, a long hint wrapping under itself
                                (find-cell "window:split-below!  ()  Split the selected")
                                (find-cell "window:split-above!  ()  Split the selected")
                                (find-cell "shows the same buffer.")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'completions-give-the-window-back
                (lambda () (not (find-cell "<completions>"))) 5000)
     ;; An argument whose type is documented completes to its values: the
     ;; buffers, spelled as the expressions that denote them.
     (send! "\x1b;xhead:show-buffer! \t\t")
     (wait-for! 'a-typed-argument-lists-its-values
                (lambda () (and (find-cell "matches of buffer")
                                (find-cell "(buffer \"*scratch*\")")
                                (find-cell "(buffer \"*terminal*\")  terminal")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'typed-completions-give-the-window-back
                (lambda () (not (find-cell "<completions>"))) 5000)
     ;; A string argument completes as a session: a sole directory opens the
     ;; type's literal and stays open without settling, and the pop-up lists
     ;; its entries at once.
     (send! "\x1b;xedit:visit-file! \"man\t")
     (wait-for! 'a-directory-completion-stays-open-and-lists-its-entries
                (lambda () (and (find-cell "λ (edit:visit-file! (file \"manual/")
                                (find-cell "matches of file")
                                (find-cell "manual/EVAL.md")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'the-session-gives-the-window-back
                (lambda () (not (find-cell "<completions>"))) 5000)
     ;; A needle argument searches as it is typed: the note counts the
     ;; matches, Tab visits the next without inserting, and C-g restores point.
     (evaluate '(let ([b (head:fresh-buffer! "needles")])
                  (head:buffer-lines-set! b (vector "alpha beta alpha" "gamma alpha"))
                  (head:show-buffer! b)
                  (head:goto! '(0 . 0))
                  (head:buffer-name b)))
     (send! "\x1b;xsearch:replace! \"alp")
     (wait-for! 'a-needle-argument-counts-its-matches
                (lambda () (find-cell "λ (search:replace! \"alp [1 of 3]")) 5000)
     (send! "\t")
     (wait-for! 'tab-visits-the-next-match
                (lambda () (find-cell "λ (search:replace! \"alp [2 of 3]")) 5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'the-search-quits-with-the-prompt (lambda () (find-cell "Quit")) 5000)
     (check 'cancelling-the-search-restores-point (equal? (evaluate '(head:point)) '(0 . 0)))
     (evaluate '(begin (head:show-buffer! (head:buffer-named "*scratch*")) #t))
     ;; A closed string is final: Tab settles the forms around it, the file
     ;; literal closing at its one argument and the command at its one,
     ;; whether or not the path exists.
     (send! "\x1b;xedit:save-file! (file \"~/ddd\"\t")
     (wait-for! 'a-final-datum-settles-the-forms-around-it
                (lambda () (find-cell "λ (edit:save-file! (file \"~/ddd\"))")) 5000)
     (send! "\x7;")                     ; C-g
     ;; Completing a sole candidate includes punctuation and remains executable.
     (send! "\x1b;xspwir\t\r")
     (wait-for! 'normalized-full-match-can-run
       (lambda () (and (find-cell "1▏") (find-cell "2▏"))) 5000)
     (send! "\x18;0")
     (wait-for! 'close-the-test-split
       (lambda () (not (and (find-cell "1▏") (find-cell "2▏")))) 5000)
     ;; C-x TAB works inside a prompt, listing the prompt's keys in the pop-up;
     ;; pressed again after the prompt closes, the listing returns to the buffer's
     (send! "\x1b;x")
     (send! "\x18;\t")
     (wait-for! 'c-x-tab-inside-a-prompt-lists-the-prompts-keys
       (lambda () (and (find-cell "prompt keys") (find-cell "prompt:"))) 5000)
     (send! "\x7;")                     ; C-g leaves the prompt
     (send! "\x18;\t")
     (wait-for! 'the-listing-returns-to-the-buffers-keys
       (lambda () (and (find-cell "<keys>") (not (find-cell "prompt keys")))) 5000)
     ;; Subword prefixes may reorder. Complete a nested operator from inside
     ;; its token, retaining arguments; Enter runs the completed expression.
     (send! (string-append "\x1b;xlist (appstring \"a\" \"b\"))\x1;" (make-string 10 (integer->char 6)) "\t"))
     (wait-for! 'completion-replaces-only-the-token-at-point
       (lambda () (find-cell "λ (list (string-append \"a\" \"b\"))")) 5000)
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
     ;; A definition documented in its own body reaches the page through the
     ;; head's contribution to the query, with no registry batch involved. Read
     ;; the screen: an evaluation right after a full-page repaint would wait on
     ;; the wire while the head waits on the PTY.
     (send! "\x8;fwindow:split-left!\r")
     (wait-for! 'edoc-documents-a-definition-without-a-registry-entry
       (lambda () (and (find-cell "libraries: (head window)")
                       (find-cell "source: edoc, Documented definitions")
                       (find-cell "side-by-side pair")))
       5000)

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
