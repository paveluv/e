#!/usr/bin/env scheme-script

;; The head-to-store wiring under a real PTY: typed keys arrive in the store
;; transactionally, a foreign actor's edit reaches the screen without a
;; keypress, questions and quit reviews interleave with live input, and the
;; head detaches cleanly. The headless suites cover the same seams in depth;
;; this driver keeps only what a live loop proves. Run from the repository
;; root. Real process separation is covered by wire.ss and interactive.ss.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(when (and (pair? (command-line-arguments)) (string=? (car (command-line-arguments)) "--head"))
  (eval '(begin
           (import (prefix (startup) startup:) (prefix (base) base:))
           (startup:call-with-options (cdr (command-line-arguments))
             (lambda ()
               (base:call-with-runtime
                 (lambda () (eval '(begin (import (edit) (prefix (main) main:)) (main:run)))))))))
  (exit 0))

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (vt) vt:)
             (prefix (string) string:) (prefix (test) test:) (prefix (fixture) fixture:))

     (define (check label actual expected)
       (guard (ex [else (error 'wiring-test (format "~s" label) actual expected
                               (vector->list (vt:emulator-screen mirror)))])
         (test:check label actual expected)))

     (define probe (format "/tmp/e-wiring-~a" (get-process-id)))
     (define answer-file (string-append probe "-answer"))

     (putenv "SHELL" "/bin/sh")
     (define mirror (vt:make-emulator 24 100))
     ;; Even this in-process runtime restores sessions. Keep it away from
     ;; the installation's saved state, just like the process fixtures.
     (define process
       (sys:spawn-terminal-process "/bin/sh"
         (format "exec scheme-script tests/wiring.ss --head --name 'wired head λ' --base-working-dir ~a"
           (fixture:quote-shell (format "~a-base-~a" probe (random 1000000000))))
         (current-directory) 24 100))
     (define drain! (fixture:terminal-reader process (lambda (text) (vt:emulator-feed! mirror text))))
     (define exited? #f)
     (define (pump! ms)
       (let loop ([left (div ms 25)])
         (set! exited? (drain!))
         (when (> left 0)
           (sleep (make-time 'time-duration 25000000 0))
           (loop (- left 1)))))
     (define (send! text)
       (put-bytevector (sys:terminal-process-output process) (string->utf8 text))
       (flush-output-port (sys:terminal-process-output process)))
     (define (screen-line n) (vector-ref (vt:emulator-screen mirror) n))
     (define (screen-has? n needle)
       (and (string:search (screen-line n) needle 0 (string-length (screen-line n))) #t))
     (define (echo-has? needle) (or (screen-has? 22 needle) (screen-has? 23 needle)))
     (define (visible? needle) (exists (lambda (row) (screen-has? row needle)) (iota 24)))

     (define (await! label ready?)
       (guard (ex [else (error 'wiring-test "waiting for editor" label ex
                               (vector->list (vt:emulator-screen mirror)))])
         (test:await label (lambda () (pump! 0) (ready?)))))

     (define (read-editor expression)
       (let ([result (fixture:query probe expression send! await!)])
         ;; M-x keeps its prompt visible until evaluation finishes. A complete
         ;; reply precedes that frame, so also await the ordinary presentation
         ;; path before callers inspect cells or send the next command.
         (await! 'evaluation-presented
           (lambda ()
             ;; The prompt row is centered, so the label sits mid-line.
             (not (exists (lambda (line) (string:search line "M-x " 0 (string-length line)))
                    (vector->list (vt:emulator-screen mirror))))))
         result))

     (define (mirror-agrees?)
       (read-editor
         '(let* ([b (head:current-buffer)] [id (head:buffer-store-id b)])
            (and (= (head:buffer-line-count b) (store:line-count id))
                 (for-all (lambda (i) (string=? (head:buffer-line b i) (store:line id i)))
                   (iota (head:buffer-line-count b)))))))

     (await! 'head-starts (lambda () (screen-has? 22 "*scratch*")))
     (check 'startup-identity-and-store
       (read-editor '(list head:ui-actor (actor:current) (map store:buffer-name (store:buffer-list))))
       '((head "wired head λ") (head "wired head λ") ("*scratch*")))

     ;; -- head edits mirror. Input is sequential: a query sent after the keys
     ;; observes their committed transactions without waiting -----------------
     (send! "hello")
     (check 'typing-mirrors (list (mirror-agrees?) (screen-has? 0 "hello")) '(#t #t))
     (send! "\rworld")                 ; RET: the splice path
     (check 'newline-splice-mirrors (mirror-agrees?) #t)
     (send! "\x1;\xb;")               ; C-a C-k: kill to end of line
     (check 'kill-mirrors (mirror-agrees?) #t)
     (send! "\x1f;")                   ; C-_: undo (the inverse path)
     (check 'undo-mirrors (mirror-agrees?) #t)

     ;; -- a foreign actor's edit reaches the screen ---------------------------
     (send! "\x1b;xstore:edit! (quote (agent tester)) (head:buffer-store-id (head:current-buffer)) (store:revision (head:buffer-store-id (head:current-buffer))) (text:make-span 0 0 0 0) (list \"AGENT \")\r")
     (await! 'foreign-edit-lands-on-screen (lambda () (screen-has? 0 "AGENT ")))
     (check 'foreign-edit-mirrors (mirror-agrees?) #t)
     (send! "\x5;!")                   ; C-e then a character
     (check 'typing-after-sync-mirrors (mirror-agrees?) #t)
     ;; Adoption does not produce another operation audit. The base records
     ;; this edit once, under its author even though M-x ran as this head.
     (check 'foreign-edit-audited-once-under-its-author
       (read-editor
         '(map (lambda (entry) (list (log:actor entry) (car (log:datum entry))))
            (filter (lambda (entry) (equal? (log:actor entry) '(agent tester)))
              (log:entries 'store))))
       '(((agent tester) edit)))

     ;; bracketed paste rides the reader thread into the buffer
     (send! "\x5;\x1b;[200~[pasted]\x1b;[201~")
     (await! 'paste-content-on-screen (lambda () (visible? "[pasted]")))
     (check 'bracketed-paste-mirrors (mirror-agrees?) #t)

     ;; the wake path: a worker-thread edit appears with NO keypress. Awaiting
     ;; only drains output, so the wake alone paints.
     (send! "\x1b;xfork-thread (lambda () (sleep (make-time (quote time-duration) 100000000 0)) (store:edit! (quote (agent background)) (head:buffer-store-id (head:current-buffer)) (store:revision (head:buffer-store-id (head:current-buffer))) (text:make-span 0 0 0 0) (list \"WOKEN \")))\r")
     (await! 'foreign-edit-appears-without-a-keypress (lambda () (screen-has? 0 "WOKEN ")))

     ;; wake coalescing: a racing burst of foreign edits must land on
     ;; the screen in full -- a wake arriving mid-paint is not lost
     (send! "\x1b;xfork-thread (lambda () (sleep (make-time (quote time-duration) 100000000 0)) (let ([id (head:buffer-store-id (head:current-buffer))]) (let loop ([i 0]) (when (< i 30) (store:edit! (quote (agent burst)) id (store:revision id) (text:make-span 0 0 0 0) (list \"x\")) (loop (+ i 1))))))\r")
     (await! 'racing-burst-lands-without-a-lost-wake (lambda () (screen-has? 0 (make-string 30 #\x))))

     ;; UI summaries remain coalesced: adoption of a rival's new text flushes
     ;; the three-keystroke burst with its own revision range.
     (send! "\x5;xyz")
     (send! "\x1b;xlet ([id (head:buffer-store-id (head:current-buffer))]) (store:edit! (quote (agent rival)) id (store:revision id) (text:make-span 0 0 0 0) (list \"r\"))\r")
     (check 'ui-burst-coalesced-on-the-audit-stream
       (read-editor
         '(and (exists (lambda (entry) (string:prefix? "ui: 3 edits i" (log:format-entry entry)))
                 (log:entries 'store)) #t))
       #t)

     ;; the interaction protocol: an agent asks, the head answers through
     ;; C-c a. Input stays ordered, so the answer follows the prompt; the
     ;; callback publishes one complete datum by renaming.
     (send! (format "\x1b;xactor:ask! (quote (agent tester)) head:ui-actor \"Proceed with the plan?\" (list \"yes\" \"no\") (lambda (answer) (call-with-output-file ~s (lambda (p) (write answer p)) (quote replace)) (rename-file ~s ~s))\r"
              (string-append answer-file ".pending") (string-append answer-file ".pending") answer-file))
     (await! 'ask-indicator-shows (lambda () (echo-has? "asks: Proceed with the plan?")))
     (send! "\x3;ayes\r")
     (await! 'answer-arrives (lambda () (file-exists? answer-file)))
     (check 'answer-routes-to-the-asker (call-with-input-file answer-file read) "yes")
     (delete-file answer-file)

     ;; -- window numbers: window 0 is the hidden pop-up, a split takes the
     ;; smallest free number, a deleted number is reused, and a window prints
     ;; as the literal that finds it ----------------------------------------
     (check 'window-numbers-are-reused-and-print-as-literals
       (read-editor
         '(let ([indices (lambda () (list-sort < (map head:window-index (head:windows))))])
            (split-window-below!) (split-window-below!)
            (let ([split (indices)])
              (select-window! (window 1)) (delete-window!)
              (paint:window-layout)
              (let ([deleted (indices)])
                (split-window-below!)
                (let ([reused (indices)] [literal (format "~s" (window 2))])
                  (select-window! (window 1)) (delete-other-windows!)
                  (list split deleted reused literal))))))
       '((0 1 2 3) (0 2 3) (0 1 2 3) "(window 2)"))
     (check 'status-lines-lead-with-the-number
       (exists (lambda (row) (string:prefix? "1\x258F;" (screen-line row))) (iota 24)) #t)
     ;; The pop-up is window 0: hidden, never focused, split or deleted, and
     ;; the rest of the layout is the root split's first subtree.
     (check 'the-pop-up-is-window-0-and-stays-out-of-the-way
       (read-editor
         '(list (head:window-index (head:popup)) (head:popup-rows)
                (select-window! (window 0)) (eq? (other-window!) (head:current-window))
                (begin (delete-window!) (head:window-index (head:current-window)))
                (head:window? (head:layout-split-first (head:root)))
                (eq? (head:layout-split-second (head:root)) (head:popup))))
       '(0 0 #f #t 1 #t #t))
     ;; The above and left splits are the stacked and side-by-side splits
     ;; with the new window first: the selected one becomes the second leaf.
     (check 'above-and-left-splits-put-the-new-window-first
       (read-editor
         '(let ([rest (lambda () (head:layout-split-first (head:root)))]
                [position (lambda ()
                            (let ([leaves (head:layout-leaves (head:layout-split-first (head:root)))])
                              (- (length leaves) (length (memq (head:current-window) leaves)))))])
            (split-window-above!)
            (let ([above (list (position) (head:layout-split-orientation (rest)))])
              (delete-other-windows!)
              (split-window-left!)
              (let ([left (list (position) (head:layout-split-orientation (rest)))])
                (delete-other-windows!)
                (list above left)))))
       '((1 below) (1 right)))

     ;; -- quit reviews protected local work while input keeps arriving. A
     ;; frame callback interleaves a change after the question is visible; a
     ;; worker wakes the normal pump on the head thread, where local work lives.
     (for-each
       (lambda (kind)
         (check 'quit-review-setup
           (read-editor
             `(let* ([target (head:new-local-buffer "quit work")] [armed? #t])
                (head:add-buffer! target)
                (head:store-reset! target '("keep"))
                (head:buffer-modified-set! target #t)
                ;; Model a file replaced by an unreadable directory.
                (when (eq? ',kind 'local-facts) (head:buffer-file-set! target (current-directory)))
                (head:buffer-fact-set! target 'source (head:current-buffer))
                (parameterize ([kernel:registering-module 'wiring-quit])
                  (head:add-pre-redraw-hook!
                    (lambda ()
                      (when (and armed? (prompt:active?) (string:prefix? "Modified buffers exist" (echo:text)))
                        (set! armed? #f)
                        (case ',kind
                          [(local-text) (head:store-reset! target '("keep!"))]
                          [else (head:buffer-trailing-set! target #f)])))))
                (fork-thread
                  (lambda ()
                    (let wait ([tries 2000])
                      (cond [(and (prompt:active?) (string:prefix? "Modified buffers exist" (echo:text)))
                             (head:wake-main!)]
                            [(zero? tries) (error 'quit-race "quit never reached review")]
                            [else (sleep (make-time 'time-duration 5000000 0)) (wait (- tries 1))]))))
                (list (not (head:buffer-store-id target)) (head:buffer-modified target))))
           '(#t #t))
         (send! "\x1b;xquit!!\r")
         (await! 'protected-work-prompts-before-exit (lambda () (echo-has? "Modified buffers exist")))
         (send! "y")
         (await! 'quit-reviews-a-change-during-confirmation (lambda () (echo-has? "Buffers changed")))
         (send! "n")
         (check 'cancelled-quit-keeps-work-and-the-store-open
           (read-editor
             `(let* ([b (head:buffer-named "<quit work>")]
                     [result (list (head:buffer-line b 0) (not (head:buffer-store-id b)) (head:quitting?))])
                (kernel:retract-module! 'wiring-quit)
                (kill-buffer! b)
                result))
           (list (if (eq? kind 'local-facts) "keep" "keep!") #t #f)))
       '(local-text local-facts))

     ;; -- a shared surface paints without a local app, and a style/link-only
     ;; publisher wakes the otherwise idle editor ------------------------------
     (define surface-id
       (read-editor
         '(let ([id (store:create! '(app surface-live) "*surface-live*"
                                   '("界e\x301;Z") '((read-only . #t) (wrap . #t)))])
            (delete-other-windows!)
            (head:scrollbar #f)
            (surface:publish! id #f 0
              '((0 #("31" "31" "1" #f)
                 #(("https://surface.example" "wide") ("https://surface.example" "wide") #f #f)
                 ((clusters (1 . 2) (2 . 1) (1 . 1))))) #f '(1 4))
            (show-buffer! (head:adopt-store-buffer! id))
            (head:buffer-line-numbers-setting-set! (head:current-buffer) #f)
            id)))
     (check 'surface-paints-real-shared-text-and-cell-links
       (list (screen-has? 0 "界éZ") (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 1))
       '(#t ("https://surface.example" "wide")))
     (read-editor
       `(begin
          (fork-thread
            (lambda ()
              (sleep (make-time 'time-duration 100000000 0))
              (surface:publish! ,surface-id (car (surface:snapshot ,surface-id)) 0
                '((0 #("32" "32" "1" #f)
                   #(("https://updated.example" #f) ("https://updated.example" #f) #f #f)
                   ((clusters (1 . 2) (2 . 1) (1 . 1))))) #f '(1 4)))) #t))
     (await! 'surface-only-worker-wakes-idle-head
       (lambda () (equal? (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 1)
                          '("https://updated.example" #f))))
     (read-editor `(begin (show-buffer! (buffer "*scratch*"))
                          (kill-buffer! (head:buffer-of-store-id ,surface-id)) #t))

     ;; One live describe page exercises the source/companion boundary and
     ;; head-app reloads and core refusal. Fresh commands resolve the exports;
     ;; retaining an old procedure inside this driver would test old code.
     (check 'describe-page-retains-selection-and-refreshes-through-reload
       (read-editor
         '(let ([request-window (head:current-window)] [request-buffer (head:current-buffer)])
            (define (show! name) ((top-level-value 'describe:show!) name))
            (define (page) ((top-level-value 'reference:page) head:ui-actor))
            (define (document! body)
              (kernel:call-with-registration-update
                (lambda ()
                  (kernel:retract-module! 'wired-reference-fixture)
                  (parameterize ([kernel:registering-module 'wired-reference-fixture])
                    (doc:register!
                      `(((markdown:view!) (("procedure" . "(markdown:view! fixture)"))
                         #f ("(fixture)") fixture "Live reference" #f ,body)))
                    (keymap:bind-default! "C-c F11" (top-level-value 'markdown:view!))
                    (keymap:bind! "C-c F10" void)
                    (keymap:bind-default! "C-c F10" (top-level-value 'markdown:view!))
                    (keymap:bind! 'markdown "F12" (top-level-value 'markdown:view!))
                    (keymap:bind-default! "C-c F11" (top-level-value 'markdown:view!))
                    (keymap:bind! "C-c F12" (top-level-value 'markdown:view!))))))
            (delete-other-windows!)
            (show! 'describe:show!)
            (let* ([id (car (page))] [source (head:buffer-of-store-id id)]
                   [view (markdown:companion source)]
                   [initial
                    (list (eq? request-window (head:current-window)) (eq? request-buffer (head:current-buffer))
                          (not (head:buffer-store-id view)) (head:buffer-name view)
                          (mode:name-of source) (head:buffer-read-only source))])
              (set-buffer-name! source "reference source")
              (set-buffer-name! view "reference view")
              (show! 'markdown:view!)
              (let* ([reloads
                      (fold-left
                        (lambda (out module)
                          (append out (list (let* ([callback (head:app-refresh! (head:app-of view))]
                                                   [outcome
                                                    (guard (ex [(and (string=? module "reference")
                                                                     (message-condition? ex)
                                                                     (string:search (condition-message ex) "pins reference"
                                                                       0 (string-length (condition-message ex))))
                                                                'refused])
                                                      (kernel:reload-module! module) 'reloaded)])
                                              (document! (string-append "Refreshed " module))
                                              (head:before-frame!)
                                              (head:refresh-visible-views!)
                                              (let ([revision (store:revision id)])
                                                (head:before-frame!)
                                                (head:before-frame!)
                                                (list outcome (= id (car (page))) (caddr (page))
                                                  (eq? view (markdown:companion source))
                                                  (not (eq? callback (head:app-refresh! (head:app-of view))))
                                                  (and (member (string-append "Refreshed " module)
                                                         (vector->list (head:buffer-lines view))) #t)
                                                  (store:line id 0) (keymap:command-key 'markdown:view!)
                                                  (= revision (store:revision id))))))))
                        '() '("describe" "markdown" "reference"))]
                     [labels (list (head:buffer-name source) (head:buffer-name view))]
                     [unbound
                      (begin
                        (kernel:retract-module! 'wired-reference-fixture)
                        (head:before-frame!)
                        (list (keymap:command-keys 'markdown:view!) (store:line id 0)))])
                (kill-buffer! view)
                (delete-other-windows!)
                (show! 'markdown:view!)
                (let* ([replacement (markdown:companion source)]
                       [keeps-source (and (= id (car (page))) (not (eq? view replacement)))])
                  (kill-buffer! source)
                  (head:before-frame!)
                  (let ([retired (list (page) (store:exists? id)
                                       (and (memq replacement (head:buffers)) #t))])
                    (kernel:retract-module! 'wired-reference-fixture)
                    (delete-other-windows!)
                    (list initial reloads labels unbound keeps-source retired)))))))
       '((#t #t #t "<describe>" "markdown" #t)
         ((reloaded #t markdown:view! #t #f #t "**keys**: C-c F12, C-c F11  " "C-c F12" #t)
          (reloaded #t markdown:view! #t #t #t "**keys**: C-c F12, C-c F11  " "C-c F12" #t)
          (refused #t markdown:view! #t #f #t "**keys**: C-c F12, C-c F11  " "C-c F12" #t))
         ("reference source" "<reference view>")
         (() "**procedure**: `(markdown:view! [buffer])`  ") #t (#f #f #f)))

     ;; Local work still requires review. The base remains writable while
     ;; a quitting head runs its hooks and publishes its final checkpoint.
     (read-editor
       `(begin
          (let ([local (head:new-local-buffer "quit review")])
            (head:add-buffer! local) (head:store-reset! local '("local work"))
            (head:buffer-modified-set! local #t))
          (head:add-shutdown-hook!
            (lambda ()
              (call-with-output-file ,probe
                (lambda (p)
                  (write (list (head:quitting?) (pair? (store:buffer-list))
                           (integer? (store:create! head:ui-actor "after quit" '("kept")))) p))
                'replace))) #t))
     (define mouse-before-quit (bytevector? (vt:emulator-mouse-input mirror 35 2 2 #f)))
     (delete-file probe)
     (send! "\x18;\x03;")
     (await! 'final-quit-reaches-confirmation (lambda () (echo-has? "Modified buffers exist")))
     (send! "y")
     (test:await 'head-quit-exits (lambda () (pump! 25) exited?))
     (check 'head-quit-keeps-shared-admission-and-releases-mouse-reporting
       (list (call-with-input-file probe read) mouse-before-quit
             (map (lambda (key) (cdr (assq key (vt:emulator-state mirror)))) '(mouse-tracking sgr-mouse)))
       '((#t #t #t) #t (#f #f)))
     (delete-file probe)
     (sys:close-terminal-process! process)
     (test:finish! 'wiring)))
