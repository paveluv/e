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
     (define (evaluate expression) (fixture:evaluate evaluator '(head "interactive") (fixture:composed expression)))
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
           ;; Prompts and messages use ordinary full-width allocations.
           (wait-for! (list 'idle-resize-refreshes-the-screen prompt?)
             (lambda ()
               (let ([buffer (find-cell "*scratch*")] [close (find-cell "│×│")]
                     [edge (find-cell (if prompt? "λ (resize-input" "(head"))])
                 (and (contains? (transcript-text) "\x1b;[?2026h")
                      buffer close (= (cdr close) (- cols 3))
                      edge (if prompt? (and (> (car edge) (car buffer)) (= (cdr edge) 0))
                             (and (= (car buffer) (- rows 2)) (= (cdr edge) 0)))
                      (or prompt? (find-cell resize-question))))) 3000)))
       '((#f 18 160) (#t 24 80)))
     (send! "\x7;")                     ; C-g leaves the prompt
     (wait-for! 'resize-prompt-closes (lambda () (not (find-cell "λ (resize-input"))) 3000)
     (sys:close-connection! asker)

     ;; A queued keyboard burst must have the same command/viewport semantics
     ;; with or without coalescing. Mix wrapped motion, paging, mouse targeting,
     ;; a multi-key binding and a bare modal read; compare state, not timings.
     (let ([results
            (map
              (lambda (budget)
                (evaluate
                  `(begin
                     (tui:input-delay ,budget)
                     (window-control:keep! (test-window))
                     (let ([b (store:create! head:ui-actor "scroll-burst"
                                (map (lambda (i) (format "~a ~a" i (make-string 90 #\x))) (iota 100)))])
                       (test-show! b))
                     (window:set-display! (test-manager) (test-window) (list (cons (quote wrap) #t)))
                     (window-control:split! (test-window) (quote right))
                     (window:set-display! (test-manager) (test-window) (list (cons (quote wrap) #t)))
                     (keymap:bind! "F12"
                       (lambda ()
                         (let ([answer (head:read-key-event #f)])
                           (head:report! (format "Burst complete ~a ~s" (unquote budget) answer)))))
                     #t))
                (send! (string-append
                         (apply string-append (make-list 40 "\x1b;[B"))
                         "\x1b;[6~\x1b;[5~\x1b;[<0;43;3M\x1b;[<0;43;3m"
                         "\x18;o\x18;o\x1b;[24~x"))
                (wait-for! 'burst-reaches-its-modal-answer
                  (lambda () (find-cell (format "Burst complete ~a ~s" budget "x"))) 5000)
                (evaluate
                  '(list (car (widget:frame-rect (test-frame (test-window))))
                         (map (lambda (w)
                                (view:state (interaction:snapshot (widget:descendant w 'document))))
                              (window:list (test-manager))))))
              '(0 8))])
       (check 'coalescing-preserves-wrapped-motion-paging-mouse-and-modal-input
         (equal? (car results) (cadr results))))
     (evaluate '(begin
                  (keymap:unbind! "F12") (tui:input-delay 8)
                  (window-control:keep! (test-window))
                  (test-show! (store:find-named "*scratch*")) #t))

     ;; A divider owns its drag across widget contents, on either axis.
     ;; Release inside the widget must also retire the host's gesture.
     (for-each
       (lambda (split)
         (evaluate `(begin
                      (window-control:keep! (test-window)) (buffet:open! (test-window)) (window-control:split! (test-window) ',split)
                      (let ([step 0])
                        (keymap:bind! 'buffet "F12" (lambda ()
                                                      (set! step (+ step 1))
                                                      (head:report! (format "Divider ~a step ~a" (quote (unquote split)) step))))) #t))
         (for-each
           (lambda (delta step)
             (send! "\x1b;[24~")
             (wait-for! 'divider-frame-published
               (lambda () (find-cell (format "Divider ~a step ~a" split step))) 3000)
             (let* ([before (evaluate '(test-divider))]
                    [horizontal? (eq? (car before) 'right)]
                    [x (if horizontal? (+ 1 (cadr before)) 2)]
                    [y (if horizontal? 3 (+ 1 (caddr before)))]
                    [to-x (+ x (if horizontal? delta 0))] [to-y (+ y (if horizontal? 0 delta))])
               (send! (format "\x1b;[<0;~a;~aM\x1b;[<32;~a;~aM\x1b;[<0;~a;~am\x1b;[<35;~a;~aM\x1b;[24~~"
                        x y to-x to-y to-x to-y (+ to-x 1) (+ to-y 1)))
               (wait-for! 'divider-gesture-applied
                 (lambda () (find-cell (format "Divider ~a step ~a" split (+ step 1)))) 3000)
               (check (list 'divider-crosses-widgets-and-releases split delta)
                 (equal? (evaluate '(cdr (test-divider)))
                   (list (+ (cadr before) (if horizontal? delta 0)) (+ (caddr before) (if horizontal? 0 delta)))))))
           '(-2 3) '(1 3)))
       '(right below))
     (evaluate '(begin
                  (keymap:unbind! 'buffet "F12") (window-control:keep! (test-window))
                  (test-show! (store:find-named "*scratch*")) #t))

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
     (send! "\x1b;xwindow:\t")
     (wait-for! 'first-tab-normalizes-without-choosing
       (lambda () (and (find-cell "λ (window:")
                       (not (find-cell "<completions>")))) 5000)
     (send! "\t")
     (wait-for! 'completions-take-the-window
                (lambda () (and (find-cell "matches of symbol") (find-cell "window:")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'completions-give-the-window-back
                (lambda () (not (find-cell "λ ("))) 5000)
     ;; An argument whose type is documented completes to its values: the
     ;; buffers, spelled as the expressions that denote them.
     (send! "\x1b;xstore:line \t\t")
     (wait-for! 'a-typed-argument-lists-its-values
                (lambda () (and (find-cell "matches of buffer")
                                (find-cell "*scratch*") (find-cell "*terminal*")
                                (find-cell "Completion") (find-cell "Details") (find-cell "'(buffer ") (find-cell "terminal  modified")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'typed-completions-give-the-window-back
                (lambda () (not (find-cell "λ ("))) 5000)
     ;; A string argument completes as a session: a sole directory opens the
     ;; path string and stays open without settling, and the pop-up lists
     ;; its entries at once.
     (send! "\x1b;xedit:visit-file! \"man\t")
     (wait-for! 'a-directory-completion-stays-open-and-lists-its-entries
                (lambda () (and (find-cell "λ (edit:visit-file! \"manual/")
                                (find-cell "matches of file")
                                (find-cell "manual/APPS.md")))
                5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'the-session-gives-the-window-back
                (lambda () (not (find-cell "λ ("))) 5000)
     ;; A needle argument searches as it is typed: the note counts the
     ;; matches in a separate editor; Tab navigates without moving the source.
     (evaluate '(let ([b (store:create! head:ui-actor "needles" '(""))])
                  (test-show! b)
                  (edit:insert! (test-editor) "alpha beta alpha\ngamma alpha")
                  (test-go! (quote (0 . 0)))
                  (store:buffer-name b)))
     (send! "\x1b;%\"alp")
     (wait-for! 'a-needle-argument-counts-its-matches
                (lambda () (and (find-cell "λ (search:replace! '(model ") (find-cell "[1 of 3]"))) 5000)
     (send! "\t")
     (wait-for! 'tab-visits-the-next-match
                (lambda () (find-cell "[2 of 3]")) 5000)
     (check 'needle-preview-leaves-original-caret-alone
       (equal? (evaluate '(test-point)) '(0 . 0)))
     (send! "\x1b;[Z")
     (wait-for! 'shift-tab-visits-the-previous-match
       (lambda () (find-cell "[1 of 3]")) 5000)
     (send! "\x7;")                     ; C-g
     (wait-for! 'the-search-quits-with-the-prompt (lambda () (find-cell "Quit")) 5000)
     (check 'cancelling-the-search-restores-point (equal? (evaluate '(test-point)) '(0 . 0)))
     (send! "\x13;alpha")
     (wait-for! 'incremental-search-entry-on-main-pump
       (lambda () (and (find-cell "I-search:") (equal? (evaluate '(test-point)) '(0 . 5)))) 5000)
     (send! "\x13;")
     (wait-for! 'incremental-search-repeat
       (lambda () (equal? (evaluate '(test-point)) '(0 . 16))) 5000)
     (check 'incremental-search-preserves-document
       (equal? (evaluate '(car (call-with-values (lambda () (store:snapshot (test-document))) list))) '#("alpha beta alpha" "gamma alpha")))
     (send! "\x7;")
     (wait-for! 'incremental-search-cancel
       (lambda () (and (not (find-cell "I-search:")) (equal? (evaluate '(test-point)) '(0 . 0)))) 5000)
     (send! "\x13;\x13;")
     (wait-for! 'incremental-search-remembers-needle
       (lambda () (equal? (evaluate '(test-point)) '(0 . 5))) 5000)
     (send! "\x1b;[C")
     (wait-for! 'arrow-finishes-search-and-moves-editor
       (lambda () (and (not (find-cell "I-search:")) (equal? (evaluate '(test-point)) '(0 . 6)))) 5000)
     (evaluate '(begin (test-go! (quote (0 . 0))) #t))
     (send! "\x13;gamma\r")
     (wait-for! 'return-settles-the-last-needle-before-closing
       (lambda () (and (not (find-cell "I-search:")) (equal? (evaluate '(test-point)) '(1 . 5)))) 5000)
     (evaluate '(begin (test-show! (store:find-named "*scratch*")) #t))
     ;; A revision candidate highlights a separate read-only editor; the
     ;; source editor's caret and style stay unchanged throughout.
     (evaluate '(let ([b (store:create! head:ui-actor "previews" '(""))])
                  (test-show! b)
                  (test-go! (quote (0 . 0)))
                  (edit:insert! (test-editor) "alpha ")
                  (edit:insert! (test-editor) "beta")
                  (test-go! (quote (0 . 0)))
                  (store:buffer-name b)))
     (define revision-call (format "(delta-log:show! '~s 2" (evaluate '(test-editor))))
     (send! (string-append "\x1b;x" (substring revision-call 1 (string-length revision-call))))
     (wait-for! 'the-previewed-entry-is-highlighted
       (lambda ()
         (exists (lambda (row)
                   (let* ([line (list-ref (screen-lines) row)] [at (string:search line "alpha beta" 0 (string-length line))])
                     (and at (not (eq? (style-at (cons row (+ at 6))) 'plain)))))
           (iota (length (screen-lines))))) 5000)
     (send! "\t")
     (wait-for! 'a-sole-revision-settles-and-previews
                (lambda () (find-cell (string-append "λ " revision-call ")"))) 5000)
     (check 'revision-preview-does-not-move-or-restyle-original
       (let ([cell (find-cell "alpha beta")])
         (and (equal? (evaluate '(test-point)) '(0 . 0)) cell
           (eq? (style-at (cons (car cell) (+ (cdr cell) 6))) 'plain))))
     (send! "\x7;")                     ; C-g
     (wait-for! 'the-preview-quits-with-the-prompt (lambda () (find-cell "Quit")) 5000)
     (check 'cancelling-the-preview-restores-point-and-the-style
       (equal? (list (evaluate '(test-point))
                     (let ([cell (find-cell "alpha beta")]) (and cell (style-at (cons (car cell) (+ (cdr cell) 6))))))
               (list '(0 . 0) 'plain)))
     (evaluate '(begin (test-show! (store:find-named "*scratch*")) #t))
     ;; A closed string is final: Tab settles the forms around it, the file
     ;; path argument and its enclosing single-argument call,
     ;; whether or not the path exists.
     (send! "\x1b;xfile:stamp \"~/ddd\"\t")
     (wait-for! 'a-final-datum-settles-the-forms-around-it
                (lambda () (find-cell "λ (file:stamp \"~/ddd\")")) 5000)
     (send! "\x7;")                     ; C-g
     ;; Completing a sole candidate includes punctuation and remains executable.
     (send! "\x1b;xwindowcontrolsplit\t")
     (send! " (test-window) 'right)\r")
     (wait-for! 'normalized-full-match-can-run
       (lambda () (and (find-cell "1▏") (find-cell "2▏"))) 5000)
     (send! "\x18;0")
     (wait-for! 'close-the-test-split
       (lambda () (not (and (find-cell "1▏") (find-cell "2▏")))) 5000)
     ;; C-x TAB works inside a prompt, listing the prompt's keys in the pop-up;
     ;; pressed again after the prompt closes, the listing returns to the buffer's.
     ;; Move into the echo area so the hover-following mouse section is empty
     ;; and these keyboard bindings are on the first page.
     (send! (format "\x1b;[<35;1;~aM" (vector-length (vt:emulator-screen mirror))))
     (send! "\x1b;x")
     (send! "\x18;\t")
     (wait-for! 'c-x-tab-inside-a-prompt-lists-the-prompts-keys
       (lambda () (and (find-cell "prompt keys") (find-cell "prompt:"))) 5000)
     (send! "\x7;")                     ; C-g leaves the prompt
     (send! "\x18;\t")
     (wait-for! 'the-listing-returns-to-the-buffers-keys
       (lambda () (and (find-cell "<bindings>") (not (find-cell "prompt keys")))) 5000)
     (evaluate '(begin (bindings:hide! (test-root)) (window-control:keep! (test-window)) #t))
     (send! "\x8;k")
     (wait-for! 'describe-key-uses-the-normal-pump (lambda () (find-cell "Describe key:")) 5000)
     (send! "\x18;2")
     (wait-for! 'describe-key-publishes-a-contextual-listing (lambda () (find-cell "Key: C-x 2")) 5000)
     (check 'describing-split-does-not-split (= (evaluate '(length (window:list (test-manager)))) 1))
     (evaluate '(begin (bindings:hide! (test-root)) #t))
     ;; Subword prefixes may reorder. Complete a nested operator from inside
     ;; its token, retaining arguments; Enter runs the completed expression.
     (send! (string-append "\x1b;xlist (appstring \"a\" \"b\"))\x1;" (make-string 10 (integer->char 6)) "\t"))
     (wait-for! 'completion-replaces-only-the-token-at-point
       (lambda () (find-cell "λ (list (string-append \"a\" \"b\"))")) 5000)
     (send! "\r")
     (wait-for! 'completed-expression-keeps-its-arguments
       (lambda () (find-cell "=> '(\"ab\")")) 5000)

     ;; -- a private source and its asynchronous widget presentation -----------
     (send! "\x8;fmarkdown:view!\r") ; C-h f, then the documented name
     (wait-for! 'describe-opens-a-rendered-page (lambda () (find-cell "procedure: (markdown:view!")) 5000)
     ;; The page opens beside the requesting window, whose short viewport may
     ;; scroll the header away: inspect the prepared host projection itself.
     (check 'describe-source-is-shared-and-private-to-the-requester
       (evaluate
         '(let* ([id (find (lambda (b) (eq? (store:property b 'reference-query #f) 'markdown:view!)) (store:buffer-list))])
            (and (store:exists? id) (equal? (store:property id 'audience) (list head:ui-actor))
              (store:visible? head:ui-actor id) (not (store:visible? '(head "interactive-other") id))
              (store:property id 'read-only #f)
              (exists (lambda (line) (and (string:search line "(markdown:view!" 0 (string-length line)) #t))
                (vector->list (car (call-with-values (lambda () (store:snapshot id)) list))))))))
     ;; A definition documented in its own body reaches the page through the
     ;; head's contribution to the query, with no registry batch involved. Read
     ;; the screen: an evaluation right after a full-page repaint would wait on
     ;; the wire while the head waits on the PTY.
     (send! "\x8;fwindow-control:split!\r")
     (wait-for! 'edoc-documents-a-definition-without-a-registry-entry
       (lambda () (and (find-cell "libraries: (head window-control)")
                       (find-cell "source: edoc, Documented definitions")))
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
