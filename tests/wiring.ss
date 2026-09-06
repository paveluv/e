#!/usr/bin/env scheme-script

;; The head-to-store wiring: shared buffers mirror into the store,
;; local apps do not; the head's edits arrive transactionally, and
;; a foreign actor's store edit appears on the user's screen.  Drives
;; a live editor over a PTY; run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (terminal) terminal:))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'wiring-test (symbol->string label) actual expected
                (map screen-line '(20 21 22 23)))))

     (define probe (format "/tmp/e-wiring-~a" (getenv "USER")))

     (putenv "SHELL" "/bin/sh")
     (define mirror (terminal:make-emulator 24 100))
     (define process
       (sys:spawn-terminal-process "/bin/sh" "exec ./e --name 'wired head λ'"
                                   (current-directory) 24 100))
     (define from (transcoded-port
                    (sys:terminal-process-input process)
                    (make-transcoder (utf-8-codec) 'none 'replace)))
     (define (pump! ms)
       (let loop ([left (div ms 25)])
         (let drain ()
           (when (guard (ex [else #f]) (char-ready? from))
             (let ([c (guard (ex [else (eof-object)]) (get-char from))])
               (unless (eof-object? c)
                 (terminal:emulator-feed! mirror (string c)) (drain)))))
         (when (> left 0)
           (sleep (make-time 'time-duration 25000000 0))
           (loop (- left 1)))))
     (define (send! text)
       (put-bytevector (sys:terminal-process-output process)
                       (string->utf8 text))
       (flush-output-port (sys:terminal-process-output process)))
     (define (screen-line n)
       (vector-ref (terminal:emulator-screen mirror) n))
     (define (screen-has? n needle)
       (let* ([line (screen-line n)]
              [len (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i len) (string-length line)) #f]
                 [(string=? (substring line i (+ i len)) needle) #t]
                 [else (scan (+ i 1))]))))

     ;; ask the editor whether the current buffer's lines equal its
     ;; store twin's, writing the verdict to the probe file
     (define (mirror-agrees? label)
       (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (let* ([b (current-buffer)] [id (head:buffer-store-id b)] [n (buffer-line-count b)]) (and (= n (store:line-count id)) (let all ([i 0]) (or (= i n) (and (string=? (buffer-line b i) (store:line id i)) (all (+ i 1))))))) p)) (quote replace)\r"
                      probe))
       (pump! 900)
       (equal? (call-with-input-file probe read) #t))

     (define (read-editor expression)
       (when (file-exists? probe) (delete-file probe))
       (send! (format "\x1b;xcall-with-output-file ~s (lambda (p) (write ~s p)) (quote replace)\r"
                      probe expression))
       (pump! 900)
       (call-with-input-file probe read))

     (pump! 3000)

     ;; -- generated head views stay out of the store ---------------------
     (check 'startup-identity-and-store
            (read-editor '(list head:ui-actor (actor:current)
                                (map store:buffer-name (store:buffer-list))))
            '((head "wired head λ") (head "wired head λ") ("*scratch*")))
     (check 'registered-apps-are-local
            (read-editor
              '(for-all (lambda (a) (not (head:buffer-store-id (head:app-buffer a))))
                        (head:registered-apps)))
            #t)
     (check 'local-tools-have-local-labels
            (read-editor
              '(map (lambda (key) (head:buffer-name (head:find-tool-buffer key)))
                    '("*buffers*" "*log*" "*completions*")))
            '("<buffers>" "<log>" "<completions>"))
     (check 'buffer-list-refresh-never-mutates-store
            (read-editor
              '(let ([before (list-sort < (store:buffer-list))]
                     [events '()]
                     [was (current-buffer)])
                 (let ([token (store:subscribe! #f
                                (lambda (event) (set! events (cons event events))))])
                   (dynamic-wind
                     void
                     (lambda ()
                       (list-buffers!)
                       (head:refresh-visible-views!)
                       (show-buffer! was)
                       (list (equal? before (list-sort < (store:buffer-list)))
                             (null? events)))
                     (lambda () (store:unsubscribe! token))))))
            '(#t #t))

     ;; A direct store edit of the otherwise empty scratch buffer is
     ;; unsaved work.  Even read-only protection cannot make it disposable.
     (check 'foreign-scratch-is-protected-before-head-adoption
            (read-editor
              '(let* ([b (current-buffer)] [id (head:buffer-store-id b)])
                 (store:edit! '(agent scratch-state) id (store:revision id)
                              (text:make-span 0 0 0 0) '("foreign work"))
                 (head:buffer-read-only-set! b #t)
                 (list (store:property id 'modified) (buffer-clean? b))))
            '(#t #f))
     (send! "\x1b;xquit!!\r")
     (pump! 500)
     (check 'foreign-scratch-triggers-quit-protection
            (or (screen-has? 22 "Modified buffers exist")
                (screen-has? 23 "Modified buffers exist")) #t)
     (send! "n")
     (pump! 300)
     (send! "\x18;k\r")
     (pump! 500)
     (check 'foreign-read-only-scratch-triggers-kill-protection
            (or (screen-has? 22 "kill anyway?") (screen-has? 23 "kill anyway?")) #t)
     (send! "n")
     (pump! 300)
     (check 'cancelled-kill-keeps-foreign-scratch
            (read-editor
              '(let ([b (current-buffer)])
                 (and (store:exists? (head:buffer-store-id b))
                      (equal? (head:buffer-lines b) '#("foreign work")))))
            #t)
     (read-editor
       '(begin
          (head:buffer-read-only-set! (current-buffer) #f)
          (head:store-reset! (current-buffer) '#(""))
          (goto-point! '(0 . 0))
          #t))

     ;; -- head edits mirror --------------------------------------------------

     (send! "hello")
     (pump! 400)
     (check 'typing-mirrors (mirror-agrees? 'typing) #t)

     ;; Quit's review option must find the buffer app by identity even
     ;; after a user rename.  Restore the original window after review.
     (check 'renaming-local-tool-keeps-angle-brackets
            (read-editor
              '(head:buffer-name
                 (set-buffer-name! (head:find-tool-buffer "*buffers*")
                                   "renamed buffer list")))
            "<renamed buffer list>")
     (send! "\x1b;xquit!!\r")
     (pump! 500)
     (send! "v")
     (pump! 700)
     (check 'quit-review-finds-renamed-local-tool
            (read-editor
              '(eq? (current-buffer) (head:find-tool-buffer "*buffers*")))
            #t)
     (read-editor
       '(begin
          (set-buffer-name! (head:find-tool-buffer "*buffers*") "buffers")
          (select-window! (window 0))
          (delete-other-windows!)
          #t))

     (send! "\rworld")                 ; RET: the splice path
     (pump! 400)
     (check 'newline-splice-mirrors (mirror-agrees? 'newline) #t)

     (send! "\x1;\xb;")               ; C-a C-k: kill to end of line
     (pump! 400)
     (check 'kill-mirrors (mirror-agrees? 'kill) #t)

     (send! "\x1f;")                   ; C-_: undo (the inverse path)
     (pump! 400)
     (check 'undo-mirrors (mirror-agrees? 'undo) #t)

     ;; -- a foreign actor's edit reaches the screen ---------------------------

     (send! "\x1b;xstore:edit! (quote (agent tester)) (head:buffer-store-id (current-buffer)) (store:revision (head:buffer-store-id (current-buffer))) (text:make-span 0 0 0 0) (list \"AGENT \")\r")
     (pump! 1200)
     (check 'foreign-edit-lands-on-screen
            (let ([line (screen-line 0)])
              (substring line 0 6))
            "AGENT ")
     (check 'foreign-edit-mirrors (mirror-agrees? 'foreign) #t)

     ;; typing keeps working, and keeps agreeing, after the sync
     (send! "\x5;!")                   ; C-e then a character
     (pump! 400)
     (check 'typing-after-sync-mirrors (mirror-agrees? 'after) #t)

     ;; the foreign edit is on the audit stream
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (exists (lambda (entry) (eq? (cadr entry) (quote store))) (log:entries)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'foreign-edit-audited (call-with-input-file probe read) #t)

     ;; the human's cursor is a mark other actors can read
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (equal? (store:mark head:ui-actor (head:buffer-store-id (current-buffer)) (quote point)) (point)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'point-published-as-mark
            (call-with-input-file probe read) #t)

     ;; a foreign edit above the cursor rebases it, not clamps it
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (car (point)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (let ([row-before (call-with-input-file probe read)])
       (send! "\x1b;xstore:edit! (quote (agent tester)) (head:buffer-store-id (current-buffer)) (store:revision (head:buffer-store-id (current-buffer))) (text:make-span 0 0 0 0) (list \"above\" \"\")\r")
       (pump! 1200)
       (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (car (point)) p)) (quote replace)\r"
                      probe))
       (pump! 900)
       (check 'cursor-rebases-across-foreign-insert
              (call-with-input-file probe read)
              (+ row-before 1)))

     ;; A subscriber's nested edit used to notify the head first.  The
     ;; insert then delete must move point 0 -> 2 -> 1, matching the
     ;; store's mark, rather than replaying the deltas as delete/insert.
     (check 'ordered-events-keep-cursor-and-mark-together
            (read-editor
              '(let* ([was (current-buffer)]
                      [b (head:new-buffer "event-order-test")]
                      [id (head:buffer-store-id b)]
                      [token #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (head:buffer-lines-set! b '#("abcdef"))
                     (show-buffer! b)
                     (goto-point! '(0 . 0))
                     (head:before-frame!)
                     (set! token
                       (store:subscribe! id
                         (lambda (event)
                           (when (and (eq? (car event) 'edit) (= (caddr event) 2))
                             (store:edit! '(agent nested) id 2
                                          (text:make-span 0 0 0 1) '(""))))))
                     (store:edit! '(agent first) id 1
                                  (text:make-span 0 0 0 0) '("XY"))
                     (head:before-frame!)
                     (list (point) (store:mark head:ui-actor id 'point)
                           (head:buffer-lines b) (head:buffer-store-rev b)))
                   (lambda ()
                     (when token (store:unsubscribe! token))
                     (show-buffer! was)
                     (head:forget-buffer! b)
                     (store:delete! head:ui-actor id)))))
            '((0 . 1) (0 . 1) #("Yabcdef") 3))

     ;; Revision 3 is already committed while its callback is running.
     ;; A frame consuming revision 2 must adopt BOTH deltas with the
     ;; revision-3 text, and the later notification must not move point
     ;; again.  Calling the frame from this main-thread subscriber makes
     ;; the missing-notification window deterministic without a sleep.
     (check 'snapshot-ahead-of-notifications-keeps-positions-coherent
            (read-editor
              '(let* ([was (current-buffer)]
                      [b (head:new-buffer "snapshot-order-test")]
                      [id (head:buffer-store-id b)]
                      [token #f]
                      [during #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (head:buffer-lines-set! b '#("abcdef"))
                     (show-buffer! b)
                     (goto-point! '(0 . 0))
                     (head:before-frame!)
                     (store:edit! '(agent first) id 1
                                  (text:make-span 0 0 0 0) '("XY"))
                     (set! token
                       (store:subscribe! id
                         (lambda (event)
                           (when (and (eq? (car event) 'edit) (= (caddr event) 3))
                             (head:before-frame!)
                             (set! during
                               (list (point) (store:mark head:ui-actor id 'point)
                                     (head:buffer-store-rev b)))))))
                     (store:edit! '(agent second) id 2
                                  (text:make-span 0 0 0 1) '(""))
                     (head:before-frame!)
                     (list during (point) (store:mark head:ui-actor id 'point)
                           (head:buffer-lines b)))
                   (lambda ()
                     (when token (store:unsubscribe! token))
                     (show-buffer! was)
                     (head:forget-buffer! b)
                     (store:delete! head:ui-actor id)))))
            '(((0 . 1) (0 . 1) 3) (0 . 1) (0 . 1) #("Yabcdef")))

     ;; Own undo rebases through disjoint foreign changes and preserves
     ;; their text and provenance rather than restoring a whole snapshot.
     (send! "\x1b;xundo!\r")
     (pump! 900)
     (check 'own-undo-preserves-foreign-edits
            (read-editor
              '(and (string=? (buffer-line (current-buffer) 0) "above")
                    (string:prefix? "AGENT " (buffer-line (current-buffer) 1))))
            #t)

     ;; R1: separate command invocations create two own actions with a
     ;; foreign action between them.  Two undos and redos preserve it.
     (check 'live-undo-fixture
            (read-editor
              '(let ([b (head:new-buffer "undo-live")])
                 (head:buffer-lines-set! b '#("base" "other"))
                 (show-buffer! b)
                 (goto-point! '(0 . 0))
                 (insert-text! "A")
                 (head:buffer-lines b)))
            '#("Abase" "other"))
     (check 'live-first-undo-retains-foreign-provenance
            (read-editor
              '(let* ([b (current-buffer)] [id (head:buffer-store-id b)])
                 (store:edit! '(agent undo-live) id (store:revision id)
                              (text:make-span 1 0 1 0) '("G"))
                 (head:before-frame!)
                 (goto-point! '(0 . 5))
                 (insert-text! "B")
                 (undo!)
                 (list (head:buffer-lines b)
                       (and (exists (lambda (entry) (equal? (cadr entry) '(agent undo-live)))
                                    (store:history id)) #t))))
            '(#("Abase" "Gother") #t))
     (check 'live-second-undo-keeps-foreign-text
            (read-editor '(begin (undo!) (head:buffer-lines (current-buffer))))
            '#("base" "Gother"))
     (check 'live-redos-keep-foreign-text
            (read-editor '(begin (redo!) (redo!) (head:buffer-lines (current-buffer))))
            '#("AbaseB" "Gother"))

     ;; The actor picker can undo that agent despite newer own actions.
     (send! "\x1b;xundo-actor!!\r")
     (pump! 500)
     (send! "(agent undo-live)\r")
     (pump! 600)
     (check 'actor-picker-undoes-selected-agent
            (read-editor '(list (head:buffer-lines (current-buffer)) (undo-scope)))
            '(#("AbaseB" "other") mine))
     (read-editor '(begin (redo!) (undo-scope 'all) #t))
     (send! "\x1f;")
     (pump! 500)
     (check 'undo-key-honors-all-actor-preference
            (read-editor '(head:buffer-lines (current-buffer)))
            '#("AbaseB" "other"))
     (send! "\x1f;")
     (pump! 500)
     (check 'all-actor-undo-continues-to-own-history
            (read-editor '(head:buffer-lines (current-buffer)))
            '#("Abase" "other"))
     (read-editor '(begin (undo-scope 'mine) #t))

     (check 'live-missing-history-does-not-restore-snapshots
            (read-editor
              '(let* ([b (current-buffer)] [id (head:buffer-store-id b)])
                 (do ([i 0 (+ i 1)]) ((= i 257))
                   (store:edit! '(agent undo-live) id (store:revision id)
                                (text:make-span 1 0 1 0) '("x") '(long "long action")))
                 (head:before-frame!)
                 (let ([text (head:buffer-lines b)] [revision (store:revision id)]
                       [report (undo!)])
                   (list (and (string:search report "blocked" 0 (string-length report)) #t)
                         (equal? text (head:buffer-lines b)) (= revision (store:revision id))))))
            '(#t #t #t))
     (check 'live-reset-clears-undo-without-reviving-head-history
            (read-editor
              '(let* ([b (current-buffer)] [id (head:buffer-store-id b)])
                 (store:reset! '(agent undo-live) id '("foreign baseline"))
                 (head:before-frame!)
                 (undo! 'all)
                 (head:buffer-lines b)))
            '#("foreign baseline"))
     (read-editor
       '(let ([b (current-buffer)])
          (show-buffer! (buffer "*scratch*"))
          (kill-buffer! b)
          #t))

     ;; A disk merge is an ordinary undoable edit.  Its report keeps a
     ;; stable tool identity and returns its actual, possibly renamed,
     ;; local label to the user (Q20).
     (define merge-path (string-append probe "-merge.txt"))
     (call-with-output-file merge-path
       (lambda (p) (display "base\nsame\nsame2\nsame3\nother\n" p)) 'replace)
     (read-editor
       `(begin
          (visit-file! ,merge-path)
          (goto-point! '(0 . 0))
          (insert-text! "A")
          (set-buffer-name!
            (fresh-buffer (format "*merge-~a*" (head:buffer-name (current-buffer))))
            "review merge")
          #t))
     (call-with-output-file merge-path
       (lambda (p) (display "base\nsame\nsame2\nsame3\nGother\n" p)) 'replace)
     (send! (format "\x1b;xvisit-file! ~s\r" merge-path))
     (pump! 500)
     (send! "m")
     (pump! 900)
     (check 'merge-reports-its-renamed-local-label
            (read-editor
              '(list (head:buffer-lines (current-buffer))
                     (and (exists
                            (lambda (entry)
                              (string:suffix? "details in <review merge>" (log:format-entry entry)))
                            (log:entries 'visit-file!)) #t)))
            '(#("Abase" "same" "same2" "same3" "Gother") #t))
     (check 'live-merge-undo-retains-prior-own-edit
            (read-editor '(begin (undo!) (head:buffer-lines (current-buffer))))
            '#("Abase" "same" "same2" "same3" "other"))
     (check 'live-merge-redo
            (read-editor '(begin (redo!) (head:buffer-lines (current-buffer))))
            '#("Abase" "same" "same2" "same3" "Gother"))
     (read-editor
       '(let* ([b (current-buffer)]
               [report (head:find-tool-buffer (format "*merge-~a*" (head:buffer-name b)))])
          (show-buffer! (buffer "*scratch*"))
          (kill-buffer! b)
          (kill-buffer! report)
          #t))
     (delete-file merge-path)

     ;; bracketed paste rides the reader thread into the buffer
     (send! "\x5;")                    ; C-e
     (send! "\x1b;[200~[pasted]\x1b;[201~")
     (pump! 600)
     (check 'bracketed-paste-inserts (mirror-agrees? 'paste) #t)
     (check 'paste-content-on-screen
            (or (screen-has? 0 "[pasted]") (screen-has? 1 "[pasted]")
                (screen-has? 2 "[pasted]"))
            #t)

     ;; the wake path: a worker-thread edit appears with NO keypress
     (send! "\x1b;xfork-thread (lambda () (sleep (make-time (quote time-duration) 400000000 0)) (store:edit! (quote (agent background)) (head:buffer-store-id (current-buffer)) (store:revision (head:buffer-store-id (current-buffer))) (text:make-span 0 0 0 0) (list \"WOKEN \")))\r")
     (pump! 300)                       ; the eval returns; the loop sleeps
     (pump! 1700)                      ; no keys: only the wake can paint
     (check 'foreign-edit-appears-without-a-keypress
            (substring (screen-line 0) 0 6)
            "WOKEN ")

     ;; mastery: the head's line cache IS the store's immutable text

     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (let-values ([(text rev) (store:snapshot (head:buffer-store-id (current-buffer)))]) (eq? text (head:buffer-lines (current-buffer)))) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'cache-is-the-store-text
            (call-with-input-file probe read) #t)

     ;; the interaction protocol: an agent asks, the head answers ------
     (send! (format "\x1b;xactor:ask! (quote (agent tester)) head:ui-actor \"Proceed with the plan?\" (list \"yes\" \"no\") (lambda (answer) (call-with-output-file \"~a\" (lambda (p) (write answer p)) (quote replace)))\r"
                    probe))
     (pump! 1200)
     (check 'ask-indicator-shows
            (screen-has? 23 "asks: Proceed with the plan?") #t)
     (send! "\x3;a")                   ; C-c a opens the answer prompt
     (pump! 600)
     (send! "yes\r")
     (pump! 900)
     (check 'answer-routes-to-the-asker
            (call-with-input-file probe read) "yes")

     ;; the scheduling substrate: a marshaled thunk runs with NO
     ;; keypress -- the idle main loop's pump executes it inline
     (send! (format "\x1b;xfork-thread (lambda () (sleep (make-time (quote time-duration) 300000000 0)) (head:run-on-main! (lambda () (call-with-output-file \"~a\" (lambda (p) (write (quote ran) p)) (quote replace))))))\r"
                    probe))
     (pump! 300)                       ; the eval returns; the loop sleeps
     (pump! 1500)                      ; no keys: the pump must run it
     (check 'posted-thunk-runs-without-a-keypress
            (call-with-input-file probe read) 'ran)

     ;; wake coalescing: a racing burst of foreign edits must land on
     ;; the screen in full -- a wake arriving mid-paint is not lost
     (send! "\x1b;xfork-thread (lambda () (sleep (make-time (quote time-duration) 300000000 0)) (let ([id (head:buffer-store-id (current-buffer))]) (let loop ([i 0]) (when (< i 30) (store:edit! (quote (agent burst)) id (store:revision id) (text:make-span 0 0 0 0) (list \"x\")) (loop (+ i 1))))))\r")
     (pump! 300)
     (pump! 1700)                      ; no keys: only wakes can paint
     (check 'racing-burst-lands-without-a-lost-wake
            (substring (screen-line 0) 0 30)
            (make-string 30 #\x))

     ;; A rival replaces the insertion point between the UI's basis and
     ;; its keystroke (same eval, without a frame sync).  Refuse the
     ;; keystroke, retain the rival's edit, and leave head history intact.
     (define before-conflict-history
       (read-editor '(map length (vector->list (head:buffer-history (current-buffer))))))
     (send! "\x1b;xlet ([id (head:buffer-store-id (current-buffer))]) (main:dispatch-key! \"M-<\") (main:dispatch-key! \"C-f\") (main:dispatch-key! \"C-f\") (store:edit! (quote (agent rival)) id (store:revision id) (text:make-span 0 1 0 5) (list \"RIV\")) (main:dispatch-key! \"z\")\r")
     (pump! 900)
     (check 'conflict-is-reported
            (or (screen-has? 22 "not applied:") (screen-has? 23 "not applied:")) #t)
     (check 'conflict-preserves-foreign-text (screen-has? 0 "RIV") #t)
     (check 'conflict-keeps-head-history
            (read-editor '(map length (vector->list (head:buffer-history (current-buffer)))))
            before-conflict-history)

     (check 'racing-keystroke-keeps-its-cursor
            (read-editor
              '(let* ([old (current-buffer)]
                      [b (head:new-buffer "cursor-live")]
                      [id (head:buffer-store-id b)]
                      [token #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (head:buffer-lines-set! b '#("abcdef"))
                     (show-buffer! b)
                     (goto-point! '(0 . 2))
                     (store:edit! '(agent cursor-live) id (store:revision id)
                                  (text:make-span 0 0 0 0) '("Q"))
                     (set! token
                       (store:subscribe! id
                         (lambda (event)
                           (when (and (eq? (car event) 'edit)
                                      (equal? (list-ref event 3) head:ui-actor))
                             (store:edit! '(agent cursor-live) id (store:revision id)
                                          (text:make-span 0 0 0 0) '("R"))
                             (head:before-frame!)))))
                     (insert-text! "X")
                     (head:before-frame!)
                     (list (vector->list (head:buffer-lines b)) (point)
                           (store:mark head:ui-actor id 'point)))
                   (lambda ()
                     (when token (store:unsubscribe! token))
                     (show-buffer! old)
                     (kill-buffer! b)))))
            '(("RQabXcdef") (0 . 5) (0 . 5)))

     ;; Q17: a repaint callback writes after the head adopts a reset but
     ;; before its marks publish.  Keep the store's rebased marks, then retry.
     (check 'stale-publication-keeps-live-point-and-selection
            (read-editor
              '(let* ([old (current-buffer)]
                      [b (head:new-buffer "publication-live")]
                      [id (head:buffer-store-id b)]
                      [armed? #t])
                 (dynamic-wind
                   void
                   (lambda ()
                     (head:store-reset! b '("abcdef"))
                     (show-buffer! b)
                     (goto-point! '(0 . 1))
                     (set-mark-command!)
                     (goto-point! '(0 . 4))
                     (head:before-frame!)
                     (store:reset! '(agent publication-live) id '("abcdef"))
                     (let ([basis (store:revision id)])
                       (head:set-repaint-hook!
                         (lambda ()
                           (when (and armed? (= (head:buffer-store-rev b) basis))
                             (set! armed? #f)
                             (store:edit! '(agent publication-live) id (store:revision id)
                                          (text:make-span 0 0 0 0) '("Q")))
                           (paint:invalidate-screen-cache!)))
                       (head:before-frame!)
                       (let ([published (store:mark head:ui-actor id 'point)]
                             [region (store:mark head:ui-actor id 'region)])
                         (head:before-frame!)
                         (list published (list (text:span-start region) (text:span-end region))
                               (point) (mark)))))
                   (lambda ()
                     (head:set-repaint-hook! (lambda () (paint:invalidate-screen-cache!)))
                     (show-buffer! old)
                     (kill-buffer! b)))))
            '((0 . 5) ((0 . 2) (0 . 5)) (0 . 5) (0 . 2)))

     ;; R5: preserve both directions of a selection through inserted and
     ;; removed rows, then retain only the surviving part of selected text.
     (for-each
       (lambda (backwards?)
         (check (if backwards? 'backward-selection-follows-edits 'forward-selection-follows-edits)
                (read-editor
                  `(let* ([old (current-buffer)]
                          [b (head:new-buffer "selection-live")]
                          [id (head:buffer-store-id b)])
                     (define (state)
                       (let ([region (store:mark head:ui-actor id 'region)])
                         (list (text:extract (head:buffer-lines b) region)
                               (text:span-start region) (text:span-end region)
                               (point) (mark))))
                     (dynamic-wind
                       void
                       (lambda ()
                         (head:buffer-lines-set! b '#("zero" "pick" "tail"))
                         (show-buffer! b)
                         (goto-point! (if ,backwards? '(1 . 4) '(1 . 0)))
                         (set-mark-command!)
                         (goto-point! (if ,backwards? '(1 . 0) '(1 . 4)))
                         (store:edit! '(agent selection-live) id (store:revision id)
                                      (text:make-span 0 0 0 0) '("new" ""))
                         (head:before-frame!)
                         (head:store-edit! b (text:make-span 0 0 0 0) '("own" ""))
                         (head:before-frame!)
                         (let ([inserted (state)])
                           (store:edit! '(agent selection-live) id (store:revision id)
                                        (text:make-span 0 0 2 0) '(""))
                           (head:before-frame!)
                           (store:edit! '(agent selection-live) id (store:revision id)
                                        (text:make-span 0 4 1 2) '(""))
                           (head:before-frame!)
                           (list inserted (state))))
                       (lambda () (show-buffer! old) (kill-buffer! b)))))
                (if backwards?
                    '((("pick") (3 . 0) (3 . 4) (3 . 0) (3 . 4))
                      (("ck") (0 . 4) (0 . 6) (0 . 4) (0 . 6)))
                    '((("pick") (3 . 0) (3 . 4) (3 . 4) (3 . 0))
                      (("ck") (0 . 4) (0 . 6) (0 . 6) (0 . 4))))))
       '(#f #t))

     ;; the selection is published: mark plus motion becomes the ui's
     ;; 'region span mark in the store; C-g deactivates and drops it
     (send! "\x1b;<\x0;\x6;\x6;\x6;")   ; M-<, C-@, then three C-f
     (pump! 600)
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (let ([s (store:mark head:ui-actor (head:buffer-store-id (current-buffer)) (quote region))]) (write (list (text:span-start s) (text:span-end s)) p))) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'region-published-as-a-span
            (call-with-input-file probe read)
            '((0 . 0) (0 . 3)))
     (send! "\x7;")                     ; C-g: the mark deactivates
     (pump! 600)
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (store:mark head:ui-actor (head:buffer-store-id (current-buffer)) (quote region)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'region-dropped-on-quit
            (call-with-input-file probe read) #f)

     ;; every window's cursor is published: a split adds a second
     ;; (point . serial) mark, closing it drops the mark
     (define (count-window-points)
       (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (length (filter (lambda (m) (and (pair? (car m)) (eq? (caar m) (quote point)))) (store:marks head:ui-actor (head:buffer-store-id (current-buffer))))) p)) (quote replace)\r"
                      probe))
       (pump! 900)
       (call-with-input-file probe read))
     (send! "\x18;2")                   ; C-x 2: split
     (pump! 600)
     (check 'split-publishes-two-window-points (count-window-points) 2)
     (send! "\x18;1")                   ; C-x 1: back to one window
     (pump! 600)
     (check 'closed-window-point-dropped (count-window-points) 1)

     ;; Named-head edits stay untinted; foreign ink gets a blame face.
     (read-editor '(begin (head:add-buffer! (head:new-buffer "blame-naming")) #t))
     (check 'blame-tints-only-foreign-edits
       (map
         (lambda (actor-expression)
           (read-editor
             `(let ([id (head:buffer-store-id (head:buffer-named "blame-naming"))])
                (store:edit! ,actor-expression id (store:revision id)
                  (text:make-span 0 0 0 0) '("ink"))
                #t))
           ;; read-editor returns to the real pump, which delivers the
           ;; blame subscriber's posted work before the next query.
           (read-editor
             '(let ([b (head:buffer-named "blame-naming")])
                (length (filter (lambda (range)
                                  (and (= (length range) 5) (eq? (car range) b)
                                       (memq (list-ref range 4) '(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))))
                                (paint:highlight-ranges))))))
         '(head:ui-actor (quote (agent rival))))
       '(0 1))
     (read-editor '(begin (kill-buffer! (head:buffer-named "blame-naming")) #t))

     ;; A rival's edit is attributed at point from the store's delta log.
     (send! "\x1b;xlet ([id (head:buffer-store-id (current-buffer))]) (store:edit! (quote (agent rival)) id (store:revision id) (text:make-span 0 0 0 2) (list \"BL\"))\r")
     (pump! 600)
     (send! "\x1b;<")                   ; onto the rival's span
     (pump! 300)
     (send! "\x1b;xblame:at-point!\r")
     (pump! 900)
     (check 'blame-names-the-rival-at-point
            (or (screen-has? 22 "(agent rival) wrote this at revision")
                (screen-has? 23 "(agent rival) wrote this at revision"))
            #t)

     ;; ui edits are audited too, coalesced: three keystrokes become
     ;; one entry, flushed before the rival's interleaving operation
     ;; so the record reads in true order
     (send! "xyz")
     (pump! 300)
     (send! "\x1b;xlet ([id (head:buffer-store-id (current-buffer))]) (store:edit! (quote (agent rival)) id (store:revision id) (text:make-span 0 0 0 0) (list \"r\"))\r")
     (pump! 900)
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (let ([texts (map (lambda (e) (log:format-entry e)) (log:entries (quote store)))]) (write (let scan ([ts texts]) (cond [(null? ts) (quote missing)] [(and (> (string-length (car ts)) 13) (string=? (substring (car ts) 0 13) \"ui: 3 edits i\")) (quote coalesced)] [else (scan (cdr ts))])) p))) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'ui-burst-coalesced-on-the-audit-stream
            (call-with-input-file probe read) 'coalesced)

     ;; buffer facts are shared truth: a rival setting a property is
     ;; what the head's own accessors read back, and the head's edits
     ;; flip the shared modified flag
     (send! "\x1b;xstore:set-property! (quote (agent rival)) (head:buffer-store-id (current-buffer)) (quote file) \"/tmp/rival-owned.txt\"\r")
     (pump! 900)
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (list (head:buffer-file (current-buffer)) (store:property (head:buffer-store-id (current-buffer)) (quote modified))) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'facts-are-shared-truth
            (call-with-input-file probe read)
            '("/tmp/rival-owned.txt" #t))

     ;; the buffer lifecycle crosses heads: a rival's new buffer appears
     ;; in this head's list, its rename follows, and its deletion drops
     ;; the record -- even while a window is showing it. Hidden labels
     ;; reserve the shared namespace for both store and head commands.
     (define (buffer-names) (read-editor '(map head:buffer-name (buffer-list))))
     (read-editor
       '(begin
          (store:create! '(agent rival) "rival-log" '("") '((audience)))
          (store:create! '(agent rival) "rival-notes" '("from the rival")
                         '((audience (head "elsewhere"))))))
     (check 'private-foreign-buffer-stays-hidden
            (member "rival-notes" (buffer-names)) #f)
     (read-editor
       '(begin (store:set-property! '(agent rival) (store:find-named "rival-notes")
                                    'audience (list head:ui-actor)) #t))
     (check 'foreign-buffer-adopted
            (and (member "rival-notes" (buffer-names)) #t) #t)
     (define rival-label
       (read-editor '(store:rename! '(agent rival) (store:find-named "rival-notes") "rival-log")))
     (check 'foreign-rename-follows
            (list rival-label (and (member rival-label (buffer-names)) #t)
                  (member "rival-notes" (buffer-names)))
            '("rival-log<2>" #t #f))
     (check 'head-rename-adopts-the-store-claim
            (read-editor `(head:buffer-name (set-buffer-name! (head:buffer-named ,rival-label) "rival-log")))
            rival-label)
     (for-each
       (lambda (hide?)
         (send! (string-append "\x18;b" rival-label "\r")) ; C-x b: look at it
         (pump! 900)
         (check 'showing-the-foreign-buffer (screen-has? 0 "from the rival") #t)
         (read-editor
           (if hide?
               '(begin (head:buffer-fact-set! (current-buffer) 'audience '()) #t)
               `(begin (store:delete! '(agent rival) (store:find-named ,rival-label)) #t)))
         (check 'retirement-moves-the-window-on
                (list (member rival-label (buffer-names))
                      (screen-has? 0 "from the rival")
                      (read-editor `(and (store:find-named ,rival-label) #t)))
                (list #f #f hide?))
         (when hide?
           (read-editor
             `(begin (store:drop-property! head:ui-actor (store:find-named ,rival-label) 'audience) #t))))
       '(#t #f))
     (read-editor '(begin (store:delete! '(agent rival) (store:find-named "rival-log")) #t))

     ;; completions borrow the prompt's target window -- no pop-ups:
     ;; TAB on an ambiguous M-x prefix shows <completions> where the
     ;; buffer was; the prompt's end hands the window back intact
     (send! "\x1b;xblame\t")            ; first TAB extends to "blame:"
     (pump! 400)
     (send! "\t")                       ; second TAB lists the candidates
     (pump! 900)
     ;; the status line sits on row 21 or 22 depending on the echo height
     (define (status-has? needle)
       (or (screen-has? 21 needle) (screen-has? 22 needle)))
     (check 'completions-borrow-the-target-window
            (list (status-has? "<completions>")
                  (screen-has? 0 "blame:at-point!"))
            '(#t #t))
     (send! "\x7;")                     ; C-g: the prompt ends
     (pump! 600)
     (check 'target-window-handed-back
            (list (status-has? "<completions>") (status-has? "*scratch*"))
            '(#f #t))

     ;; the policy seam is live -- mint a session at M-x,
     ;; evaluate through its sandbox, and hit the edit allowlist
     (check 'minted-session-evals-and-is-fenced
       (read-editor
         '(let* ([s (policy:mint! '(agent wired) (policy:make 'all 10000000 0 '() 4000))]
                 [result (policy:session-eval! s "(+ 1 2)")]
                 [edit-result
                  (let-values ([(status detail)
                                (policy:session-edit! s (head:buffer-store-id (current-buffer))
                                  1 (text:make-span 0 0 0 0) '("x"))])
                    (list status detail))])
            (policy:revoke! s)
            (list (policy:session-owner s) (actor:current) result edit-result)))
       '((head "wired head λ") (head "wired head λ") (ok . "=> 3") (refused buffer)))

     ;; a seam module main links against refuses to reload in place: main
     ;; cannot follow, and two library instances would fork
     (send! "\x1b;xkernel:reload-module! \"store\"\r")
     (pump! 1200)
     ;; the message wraps across the echo area's two rows (a trailing
     ;; backslash, an indented continuation): read them as one
     (define (echo-text)
       (define (trim-right s)
         (let loop ([n (string-length s)])
           (if (and (> n 0) (memv (string-ref s (- n 1)) '(#\space #\\)))
               (loop (- n 1))
               (substring s 0 n))))
       (define (trim-left s)
         (let loop ([i 0])
           (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
               (loop (+ i 1))
               (substring s i (string-length s)))))
       (string-append (trim-right (screen-line 22)) (trim-left (screen-line 23))))
     (define (echo-has? needle)
       (let ([text (echo-text)] [len (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i len) (string-length text)) #f]
                 [(string=? (substring text i (+ i len)) needle) #t]
                 [else (scan (+ i 1))]))))
     (check 'main-linked-module-refuses-reload
            (echo-has? "main links against store")
            #t)

     (check 'log-view-reload-rebinds-without-duplicating-rows
            (read-editor
              '(let ([b (head:find-tool-buffer "*log*")]
                     [was (current-buffer)])
                 (show-buffer! b)
                 (head:refresh-visible-views!)
                 (let ([old (head:buffer-lines b)])
                   (kernel:reload-module! "log-view")
                   (show-buffer! was)
                   (list (eq? b (head:find-tool-buffer "*log*"))
                         (head:app-buffer? b)
                         (equal? old (head:buffer-lines b))))))
            '(#t #t #t))

     ;; -- window numbers --------------------------------------------------
     ;; The first window is 0 and a split takes the smallest free number
     (send! "\x1b;xbegin (split-window!) (split-window!) (list-sort < (map head:window-index (head:windows)))\r")
     (pump! 900)
     (check 'splits-take-the-smallest-free-numbers (echo-has? "(0 1 2)") #t)
     ;; a deleted window's number is free again; the (window n) literal
     ;; finds a window as (buffer "name") finds a buffer
     (send! "\x1b;xbegin (select-window! (window 1)) (delete-window!) (list-sort < (map head:window-index (head:windows)))\r")
     (pump! 900)
     (check 'deleting-frees-the-number (echo-has? "(0 2)") #t)
     ;; ... and goes to the next window created (after a frame: the
     ;; layout hands the deleted window's rows to its neighbors at redraw)
     (send! "\x1b;xbegin (split-window!) (list-sort < (map head:window-index (head:windows)))\r")
     (pump! 900)
     (check 'a-closed-number-is-reused (echo-has? "(0 1 2)") #t)
     ;; windows print as the literal that finds them
     (send! "\x1b;xwindow 2\r")
     (pump! 900)
     (check 'windows-print-as-their-literal (echo-has? "=> (window 2)") #t)
     ;; and the number leads every status line, a hairline after it
     (check 'status-lines-lead-with-the-number
            (let scan ([row 0])
              (cond [(> row 23) #f]
                    [(let ([line (screen-line row)])
                       (and (>= (string-length line) 2)
                            (string=? (substring line 0 2) "0\x258F;")))
                     #t]
                    [else (scan (+ row 1))]))
            #t)

     ;; A rendered document stays local through a real module reload.
     (check 'markdown-view-keeps-source-through-reload
            (read-editor
              '(let ([source (head:new-buffer "wired-markdown.md")])
                 (head:buffer-lines-set! source (vector "# Wired" "" "body"))
                 (mode:choose! source "markdown")
                 (show-buffer! source)
                 (let ([revision (head:buffer-store-rev source)]
                       [text (head:buffer-lines source)])
                   (markdown:view!)
                   (let* ([view (current-buffer)]
                          [refresh (head:app-refresh! (head:app-of view))])
                     (kernel:reload-module! "markdown")
                     (let ([result
                            (list (not (head:buffer-store-id view))
                                  (eq? source (head:buffer-fact view 'markdown-input #f))
                                  (not (eq? refresh (head:app-refresh! (head:app-of view))))
                                  (eq? text (head:buffer-lines source))
                                  (= revision (store:revision (head:buffer-store-id source)))
                                  (vector-ref ((mode:row-styles (mode:of view))
                                               view 0 (buffer-line view 0)) 0))])
                       (markdown:edit!)
                       (append result (list (eq? source (current-buffer)))))))))
            '(#t #t #t #t #t md-h1 #t))

     (check 'markdown-source-edits-preserve-both-view-windows
            (read-editor
              '(let* ([was (current-buffer)] [w1 (head:current)]
                      [w2 (find (lambda (w) (not (eq? w w1))) (head:windows))]
                      [other (head:window-buffer w2)]
                      [source (head:new-buffer "wired-anchors.md")])
                 (head:buffer-lines-set! source (vector "# One" "" "# Two" "" "# Three"))
                 (mode:choose! source "markdown")
                 (show-buffer! source)
                 (markdown:view!)
                 (let ([view (current-buffer)] [id (head:buffer-store-id source)])
                   (head:set-window-buffer! w2 view)
                   (goto-point! '(2 . 1))
                   (head:window-top-set! w1 2)
                   (head:window-prow-set! w2 4)
                   (head:window-pcol-set! w2 2)
                   (head:window-top-set! w2 4)
                   (head:buffer-spot-row-set! view 4)
                   (head:buffer-spot-col-set! view 2)
                   (head:buffer-spot-top-set! view 2)
                   (head:buffer-mark-row-set! view 4)
                   (head:buffer-mark-col-set! view 2)
                   (head:buffer-marked-set! view #t)
                   (store:edit! '(agent wired-anchors) id (store:revision id)
                                (text:make-span 0 0 0 0) '("# Before" "" ""))
                   (store:edit! '(agent wired-anchors) id (store:revision id)
                                (text:make-span 6 7 6 7) '("!"))
                   (kernel:reload-module! "markdown")
                   (head:before-frame!)
                   (let ([result
                          (list (buffer-line view (head:window-prow w1)) (head:window-pcol w1)
                                (buffer-line view (head:window-prow w2)) (head:window-pcol w2)
                                ;; Reload repaints while the M-x prompt
                                ;; occupies screen rows, so the painter
                                ;; may adjust visible tops to keep point.
                                (<= 0 (head:window-top w1) (head:window-prow w1))
                                (<= 0 (head:window-top w2) (head:window-prow w2))
                                (buffer-line view (head:buffer-spot-row view))
                                (buffer-line view (head:buffer-spot-top view))
                                (buffer-line view (head:buffer-mark-row view)) (head:buffer-mark-col view))])
                     (head:set-window-buffer! w2 other)
                     (show-buffer! was)
                     (kill-buffer! source)
                     result))))
            '("Two" 1 "Three!" 2 #t #t "Three!" "Two" "Three!" 2))

     (check 'markdown-toggles-keep-point-through-repaint-edits
            (read-editor
              '(let ([source (head:new-buffer "wired-navigation.md")]
                     [was (current-buffer)] [once #t])
                 (head:buffer-lines-set! source (vector "# Alpha" "" "# Middle" "" "# Omega"))
                 (mode:choose! source "markdown")
                 (show-buffer! source)
                 (goto-point! '(2 . 0))
                 (let ([result
                        (dynamic-wind
                          (lambda ()
                            (head:set-repaint-hook!
                              (lambda ()
                                (paint:invalidate-screen-cache!)
                                (when once
                                  (set! once #f)
                                  (head:store-edit! source (text:make-span 0 0 0 0) '("# Before" "" ""))
                                  (head:before-frame!)))))
                          (lambda ()
                            (markdown:view!)
                            (let ([view-point (point)] [view-line (buffer-line (current-buffer) (car (point)))])
                              (set! once #t)
                              (markdown:edit!)
                              (list view-point view-line (point) (buffer-line source (car (point))))))
                          (lambda () (head:set-repaint-hook! (lambda () (paint:invalidate-screen-cache!)))))])
                   (show-buffer! was)
                   (kill-buffer! source)
                   result)))
            '((4 . 0) "Middle" (6 . 0) "# Middle"))

     (delete-file probe)
     (sys:close-terminal-process! process)
     (format #t "~a wiring checks passed\n" checks)))
