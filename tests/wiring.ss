#!/usr/bin/env scheme-script

;; The head-to-store wiring: shared buffers mirror into the store,
;; local apps do not; the head's edits arrive transactionally, and
;; a foreign actor's store edit appears on the user's screen.  Drives
;; a live editor over a PTY; run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (vt) vt:)
             (prefix (string) string:) (prefix (test) test:))

     (define (check label actual expected)
       (guard (ex [else (error 'wiring-test (format "~s" label) actual expected
                               (map screen-line '(20 21 22 23)))])
         (test:check label actual expected)))

     (define probe (format "/tmp/e-wiring-~a" (getenv "USER")))

     (putenv "SHELL" "/bin/sh")
     (define mirror (vt:make-emulator 24 100))
     (define process
       (sys:spawn-terminal-process "/bin/sh" "exec ./e --name 'wired head λ'"
                                   (current-directory) 24 100))
     (define from (transcoded-port
                    (sys:terminal-process-input process)
                    (make-transcoder (utf-8-codec) 'none 'replace)))
     (define exited? #f)
     (define (pump! ms)
       (let loop ([left (div ms 25)])
         (let drain ()
           (when (guard (ex [else (set! exited? #t) #f]) (char-ready? from))
             (let ([c (guard (ex [else (eof-object)]) (get-char from))])
               (if (eof-object? c) (set! exited? #t)
                   (begin (vt:emulator-feed! mirror (string c)) (drain))))))
         (when (> left 0)
           (sleep (make-time 'time-duration 25000000 0))
           (loop (- left 1)))))
     (define (send! text)
       (put-bytevector (sys:terminal-process-output process)
                       (string->utf8 text))
       (flush-output-port (sys:terminal-process-output process)))
     (define (screen-line n)
       (vector-ref (vt:emulator-screen mirror) n))
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

     (define (read-editor expression . prefix)
       (when (file-exists? probe) (delete-file probe))
       (send! (format "~a\x1b;xcall-with-output-file ~s (lambda (p) (write ~s p)) (quote replace)\r"
                      (if (null? prefix) "" (car prefix)) probe expression))
       (pump! 900)
       (guard (ex [else (error 'read-editor "probe did not return a datum" expression
                               (map screen-line '(20 21 22 23)))])
         (let ([result (call-with-input-file probe read)])
           (when (eof-object? result) (error 'read-editor "empty result" expression))
           result)))

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

     ;; A frame callback interleaves a change after the question is visible.
     ;; A worker wakes the normal pump on the head thread, where local work lives.
     ;; The store fixture separately checks concurrent writers under its lock.
     (for-each
       (lambda (kind)
         (define shared? (eq? kind 'shared))
         (check 'quit-review-setup
           (read-editor
             `(let* ([who '(agent quit-race)]
                     [target (if ,shared?
                                 (store:create! who "hidden quit work" '("keep") '((audience)))
                                 (head:new-local-buffer "quit work"))]
                     [armed? #t])
                (unless ,shared?
                  (head:add-buffer! target)
                  (head:store-reset! target '("keep"))
                  (head:buffer-modified-set! target #t)
                  ;; Model a file replaced by an unreadable directory.
                  (when (eq? ',kind 'local-facts) (head:buffer-file-set! target (current-directory)))
                  (head:buffer-fact-set! target 'source (current-buffer)))
                (parameterize ([kernel:registering-module 'wiring-quit])
                  (head:add-pre-redraw-hook!
                    (lambda ()
                      (when (and armed? (prompt:active?) (string:prefix? "Modified buffers exist" (echo:text)))
                        (set! armed? #f)
                        (case ',kind
                          [(shared) (store:edit! who target 0 (text:make-span 0 4 0 4) '("!"))]
                          [(local-text) (head:store-reset! target '("keep!"))]
                          [else (head:buffer-trailing-set! target #f)])))))
                (fork-thread
                  (lambda ()
                    (let wait ([tries 2000])
                      (cond [(and (prompt:active?) (string:prefix? "Modified buffers exist" (echo:text)))
                             (head:wake-main!)]
                            [(zero? tries) (error 'quit-race "quit never reached review")]
                            [else (sleep (make-time 'time-duration 5000000 0)) (wait (- tries 1))]))))
                (list (if ,shared? (not (head:buffer-of-store-id target)) (not (head:buffer-store-id target)))
                      (if ,shared? (store:property target 'modified) (head:buffer-modified target))))) '(#t #t))
         (send! "\x1b;xquit!!\r")
         (pump! 500)
         (check 'protected-work-prompts-before-exit
           (or (screen-has? 22 "Modified buffers exist") (screen-has? 23 "Modified buffers exist")) #t)
         (send! "y")
         (pump! 500)
         (check 'quit-reviews-a-change-during-confirmation
           (or (screen-has? 22 "Buffers changed") (screen-has? 23 "Buffers changed")) #t)
         (send! "n")
         (pump! 200)
         (check 'cancelled-quit-keeps-work-and-the-store-open
           (read-editor
             `(let* ([id (and ,shared? (store:find-named "hidden quit work"))]
                     [b (and (not ,shared?) (head:buffer-named "<quit work>"))]
                     [result (list (if id (store:line id 0) (buffer-line b 0))
                               (if id (not (head:buffer-of-store-id id)) (not (head:buffer-store-id b)))
                               (head:quitting?))])
                (kernel:retract-module! 'wiring-quit)
                (if id (store:delete! head:ui-actor id) (kill-buffer! b))
                result)) (list (if (eq? kind 'local-facts) "keep" "keep!") #t #f)))
       '(shared local-text local-facts))

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

     ;; Adoption does not produce another operation audit. The base records
     ;; this edit once, under its author even though M-x ran as this head.
     (check 'foreign-edit-audited-once-under-its-author
       (read-editor
         '(map (lambda (entry) (list (log:actor entry) (car (log:datum entry))))
            (filter (lambda (entry) (equal? (log:actor entry) '(agent tester)))
              (log:entries 'store))))
       '(((agent tester) edit)))

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
     (check 'live-file-opening-keeps-create-callback-edit-and-mode
       (read-editor
         `(let* ([who '(agent open-review)] [observed #f]
                 [token
                  (store:subscribe! #f
                    (lambda (event)
                      (when (and (eq? (car event) 'create) (string=? (caddr event) (file:base-name ,merge-path)))
                        (let ([id (cadr event)])
                          (set! observed (list (store:line id 0) (store:property id 'file)
                                           (store:property id 'mode) (store:property id 'mode-auto)))
                          (visit-file! ,merge-path)
                          (set! observed (cons (= id (head:buffer-store-id (current-buffer))) observed))
                          (store:edit! who id 0 (text:make-span 0 0 0 0) '("later "))
                          (store:set-properties! who id '((mode . "scheme") (mode-auto . #f)))
                          (head:before-frame!)))))])
            (dynamic-wind void (lambda () (visit-file! ,merge-path)) (lambda () (store:unsubscribe! token)))
            (let* ([b (current-buffer)] [id (head:buffer-store-id b)]
                   [result (list observed (head:buffer-lines b) (head:buffer-base b)
                                 (mode:name-of b) (head:buffer-mode-auto b) (length (store:history id)))])
              (store:undo! who id) (head:before-frame!) (mode:assign! b)
              (goto-point! '(0 . 0)) (insert-text! "A")
              (set-buffer-name! (fresh-buffer (format "*merge-~a*" (head:buffer-name b))) "review merge")
              result)))
       (list (list #t "base" merge-path #f #t) '#("later base" "same" "same2" "same3" "other")
             "base\nsame\nsame2\nsame3\nother\n" "scheme" #f 1))
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
     ;; The merge can publish callbacks before its following save. Recheck
     ;; disk/file facts there, and propagate a failed write instead of claiming
     ;; "Merged and saved". Keep success and failure on the same real path.
     (for-each
       (lambda (effect)
         (call-with-output-file merge-path
           (lambda (p) (display "base\nsame\nsame2\nsame3\nother\n" p)) 'replace)
         (read-editor
           `(let* ([b (current-buffer)] [id (head:buffer-store-id b)] [armed? #t] [seen #f])
              (define (state)
                (list (call-with-values (lambda () (head:buffer-state b)) list)
                      (store:history id) (head:buffer-history b) (point) (mark) (head:buffer-name b)))
              (head:store-reset! b '#("base" "same" "same2" "same3" "other")
                '((base . "base\nsame\nsame2\nsame3\nother\n") (trailing . #t)))
              (goto-point! '(0 . 0)) (insert-text! "A")
              (head:buffer-stamp-set! b #f)
              (parameterize ([kernel:registering-module 'wiring-save-review])
                (store:subscribe! id
                  (lambda (event)
                    (when (and armed?
                               (case ',effect
                                 [(during-base during-trailing)
                                  (and (eq? (car event) 'property) (eq? (caddr event) 'stamp))]
                                 [(after-save) (and (eq? (car event) 'property) (eq? (caddr event) 'file))]
                                 [else (eq? (car event) 'edit)]))
                      (set! armed? #f)
                      (case ',effect
                        [(disk) (file:write! ,merge-path '#("later") #t)]
                        [(unreadable) (delete-file ,merge-path) (mkdir ,merge-path)]
                        [(base during-base) (store:set-property! '(agent save-review) id 'base "foreign baseline\n")]
                        [(after-save) (head:buffer-name-set! b "callback name") (mode:choose! b "scheme")]
                        [(during-trailing) (store:set-property! '(agent save-review) id 'trailing #f)])
                      (set! seen (state))))))
              (set-top-level-value! 'save-review-unchanged? (lambda () (and seen (equal? seen (state)))))
              #t))
         (call-with-output-file merge-path
           (lambda (p) (display "base\nsame\nsame2\nsame3\nGother\n" p)) 'replace)
         (send! (format "\x1b;xset-top-level-value! 'save-review-result (guard (ex [(kernel:refusal? ex) 'refused]) (save-file! ~s))\r" merge-path))
         (pump! 500)
         (send! "m")
         (pump! 700)
         (check (list effect 'merge-save-rechecks-after-callbacks-and-reports-the-write-result)
           (read-editor
             `(begin (kernel:retract-module! 'wiring-save-review)
                (let ([b (current-buffer)])
                  (list (top-level-value 'save-review-result)
                        (head:buffer-lines b) (head:buffer-base b) (head:buffer-modified b)
                        (if (file-directory? ,merge-path) 'directory (file:read ,merge-path))
                        (if (memq ',effect '(during-base during-trailing after-save)) (save-review-unchanged?) #t)))))
           (list (if (memq effect '(during-base during-trailing)) 'refused (and (memq effect '(ok after-save)) #t))
             (if (memq effect '(during-base during-trailing)) '#("Abase" "same" "same2" "same3" "other")
                 '#("Abase" "same" "same2" "same3" "Gother"))
             (case effect [(ok after-save) "Abase\nsame\nsame2\nsame3\nGother\n"]
               [(base during-base) "foreign baseline\n"]
               [(during-trailing) "base\nsame\nsame2\nsame3\nother\n"]
               [else "base\nsame\nsame2\nsame3\nGother\n"])
             (not (memq effect '(ok after-save)))
             (case effect [(ok after-save) "Abase\nsame\nsame2\nsame3\nGother\n"]
               [(disk) "later\n"] [(unreadable) 'directory] [else "base\nsame\nsame2\nsame3\nGother\n"])
             #t))
         (when (eq? effect 'unreadable) (delete-directory merge-path)))
       '(ok after-save disk unreadable base during-base during-trailing))
     ;; Reread clears old head state only if its accepted revision is still
     ;; current. A reset subscriber can adopt and edit before reset returns.
     (for-each
       (lambda (reenter?)
         (read-editor
           `(let* ([b (current-buffer)] [id (head:buffer-store-id b)] [seen #f] [armed? #t])
              (define (view) (list (head:buffer-history b) (mark) (point)))
              (insert-text! "old ")
              (head:buffer-marked-set! b #t)
              (parameterize ([kernel:registering-module 'wiring-reread])
                (store:subscribe! id
                  (lambda (event)
                    (when (and ,reenter? armed? (eq? (car event) 'reset))
                      (set! armed? #f)
                      (guard (ex [else (set! seen (kernel:condition-text ex))])
                        ;; This subscriber can run before the head's notice.
                        (head:sync-foreign-edits! id)
                        (goto-point! '(0 . 0))
                        (insert-text! "later ")
                        (head:buffer-mark-row-set! b 0)
                        (head:buffer-mark-col-set! b 2)
                        (head:buffer-marked-set! b #t)
                        (set! seen (view)))))))
              (set-top-level-value! 'reread-observation
                (lambda ()
                  (list (head:buffer-lines b) (head:buffer-base b) (head:buffer-modified b)
                        (if ,reenter? (if (string? seen) seen (and seen (equal? seen (view))))
                            (equal? (list (head:buffer-history b) (mark)) '(#(() ()) #f)))
                        (length (store:history id)))))
              #t))
         (let ([disk (if reenter? "again\n" "reread\n")])
           (call-with-output-file merge-path (lambda (p) (display disk p)) 'replace)
           (send! (format "\x1b;xvisit-file! ~s\r" merge-path))
           (pump! 500)
           (send! "r")
           (pump! 700)
           (check 'reread-preserves-newer-reentrant-head-state
             (read-editor '(begin (kernel:retract-module! 'wiring-reread) (reread-observation)))
             (if reenter? (list '#("later again") disk #t #t 1)
                 (list '#("reread") disk #f #t 0)))))
       '(#f #t))
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
     (send! "\x1b;xlet ([id (head:buffer-store-id (current-buffer))]) (dispatch:key! \"M-<\") (dispatch:key! \"C-f\") (dispatch:key! \"C-f\") (store:edit! (quote (agent rival)) id (store:revision id) (text:make-span 0 1 0 5) (list \"RIV\")) (dispatch:key! \"z\")\r")
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

     ;; Tints follow surviving content. An overlap drops only that tint;
     ;; own/app edits add none, while new collaborator ink gets its own range.
     (define blame-return-name
       (read-editor '(begin (head:add-buffer! (head:new-buffer "blame-naming"))
                            (head:buffer-name (current-buffer)))))
     (define (read-blame . forms)
       (read-editor
         `(let* ([b (head:buffer-named "blame-naming")] [id (head:buffer-store-id b)])
            (define (ranges)
              (map (lambda (range) (list (cadr range) (caddr range) (cadddr range)))
                (filter (lambda (range)
                          (and (= (length range) 5) (eq? (car range) b)
                               (memq (list-ref range 4) '(blame-1 blame-2 blame-3 blame-4 blame-5 blame-6))))
                  (paint:highlight-ranges))))
            ,@forms)))
     (check 'blame-keeps-surviving-ink-and-all-authors
       (map
         (lambda (example)
           (read-blame
             '(head:store-reset! b '("abcd" "base"))
             '(head:before-frame!)
             '(store:edit! '(agent seed) id (store:revision id) (text:make-span 0 1 0 1) '("INK"))
             '(store:edit! '(agent seed) id (store:revision id) (text:make-span 1 0 1 0) '("KEPT"))
             '(head:before-frame!)
             `(store:edit! ,(car example) id (store:revision id)
                (text:make-span ,@(cadr example)) ',(caddr example)) #t)
           ;; Return to the real pump so blame observes the adopted revision.
           (read-blame
             `(list (ranges) (equal? (cadar (store:blame id 1)) ,(car example)))))
         '((head:ui-actor (0 2 0 2) ("X"))       ; inside
           (head:ui-actor (0 1 0 1) ("X"))       ; left boundary
           (head:ui-actor (0 4 0 4) ("X"))       ; right boundary
           (head:ui-actor (0 0 0 0) ("" "x"))   ; before, changing rows/columns
           (head:ui-actor (1 7 1 7) ("X"))       ; after both ranges
           (head:ui-actor (0 0 0 2) ("X"))       ; partial replacement
           (head:ui-actor (0 2 0 3) (""))        ; deletion inside
           ((quote (app producer)) (0 2 0 2) ("X"))
           ((quote (head "rival")) (0 2 0 2) ("X"))
           ((quote (agent rival)) (0 2 0 2) ("X"))))
       '((((1 0 4)) #t) (((0 2 5) (1 0 4)) #t) (((0 1 4) (1 0 4)) #t)
         (((1 2 5) (2 0 4)) #t) (((0 1 4) (1 0 4)) #t) (((1 0 4)) #t)
         (((1 0 4)) #t) (((1 0 4)) #t) (((1 0 4) (0 2 3)) #t) (((1 0 4) (0 2 3)) #t)))

     ;; The overlay cap must bound all fade work, including after reset,
     ;; retirement and a supported reload. Avoid counting Chez's GC helpers
     ;; as editor workers during this small burst; count native tasks on Linux.
     (check 'blame-burst-keeps-only-newest-ink-without-workers
       (map
         (lambda (ending)
           (read-blame
             '(blame:tint-seconds 8)
             '(head:store-reset! b '(""))
             '(head:before-frame!)
             `(parameterize ([collect-trip-bytes (* 128 1024 1024)])
                (let* ([count (lambda () (and (file-directory? "/proc/self/task")
                                           (length (directory-list "/proc/self/task"))))]
                       [before (count)])
                  (do ([i 0 (+ i 1)]) ((= i 160))
                    (store:edit! '(agent burst) id (store:revision id)
                      (text:make-span 0 0 0 0) '("x")))
                  (head:before-frame!)
                  (let ([ink (ranges)] [bounded? (or (not before) (<= (count) before))])
                    (case ',ending
                      [(reset) (store:reset! '(agent burst) id '(""))]
                      [(retire) (store:set-property! head:ui-actor id 'audience '())]
                      [(reload) (kernel:reload-module! "blame")])
                    (head:before-frame!)
                    (let ([cleared (ranges)])
                      (when (eq? ',ending 'retire)
                        (store:set-property! head:ui-actor id 'audience 'all))
                      (list ink bounded? cleared)))))))
         '(reset retire reload))
       (make-list 3 '(((0 7 8) (0 6 7) (0 5 6) (0 4 5) (0 3 4) (0 2 3) (0 1 2) (0 0 1)) #t ())))

     ;; Observe the terminal's painted cells without injecting a key after
     ;; expiry. Fractional durations work in the outer loop and an open prompt.
     (define (blame-screen-style word)
       (let ([col (string:search (screen-line 0) word 0 100)])
         (and col (vector-ref (vector-ref (vt:emulator-styles mirror) 0) col))))
     (for-each
       (lambda (nested?)
         (read-blame '(head:store-reset! b '("base")) '(show-buffer! b) '(goto-point! '(0 . 0)) #t)
         (let ([plain (blame-screen-style "base")])
           (read-blame '(blame:tint-seconds 3/2)
             '(store:edit! '(agent fade) id (store:revision id) (text:make-span 0 0 0 4) '("ink!")) #t)
           (let ([tinted (blame-screen-style "ink!")])
             (when nested? (send! "\x1b;x") (pump! 100))
             (let ([echo (map screen-line '(22 23))])
               (pump! 900)
               (check (list 'blame-fades-on-idle-screen nested?)
                 (list (and plain tinted (not (equal? plain tinted)))
                       (equal? (blame-screen-style "ink!") plain)
                       (equal? (map screen-line '(22 23)) echo))
                 '(#t #t #t)))
             (when nested? (send! "\x7;") (pump! 100)))))
       '(#f #t))
     (read-editor `(begin (blame:tint-seconds 8)
                          (show-buffer! (head:buffer-named ,blame-return-name))
                          (kill-buffer! (head:buffer-named "blame-naming")) #t))

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

     ;; UI summaries remain coalesced: adoption of the rival's new text
     ;; flushes the three-keystroke burst with its own revision range.
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
         '(let* ([s (policy:mint! '(agent wired) (policy:make 'all 10000000 '() 4000))]
                 [result (policy:session-eval! s "(+ 1 2)")]
                 [edit-result
                  (let-values ([(status detail)
                                (policy:session-edit! s (head:buffer-store-id (current-buffer))
                                  1 (text:make-span 0 0 0 0) '("x"))])
                    (list status detail))])
            (policy:revoke! s)
            (list (policy:session-owner s) (actor:current) result edit-result)))
       '((head "wired head λ") (head "wired head λ") (ok . "=> 3") (refused buffer)))

     ;; Process roots pin the seam before any library can be redefined.
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
     (check 'runtime-pinned-module-refuses-reload
            (echo-has? "pins store")
            #t)

     (check 'shared-log-keeps-actors-local-presentation-and-reload-identity
            (read-editor
              '(let ([b (head:find-tool-buffer "*log*")] [was (current-buffer)]
                     [ids (list-sort < (store:buffer-list))] [component 'multihead-log])
                 (define (messages)
                   (map cadr (filter (lambda (entry) (eq? (car entry) component)) (echo:pending))))
                 (define (message! text) (parameterize ([message-source component]) (set-message! text)))
                 (define (filtered!) ((top-level-value 'log-view:buffer) component))
                 (message! "m4-one")
                 (parameterize ([message-progress #t]) (message! "m4-two"))
                 (actor:call-as '(head "other logger") (lambda () (log:add! component "m4-other")))
                 (log:add! component "m4-quiet" #f)
                 (show-buffer! b)
                 (head:refresh-visible-views!)
                 (let* ([old (head:buffer-lines b)] [filtered (filtered!)]
                        [old-filtered (head:buffer-lines filtered)] [echo-before (messages)])
                   (kernel:reload-module! "log-view")
                   (let ([rebound (list (eq? b (head:find-tool-buffer "*log*"))
                                        (eq? filtered (filtered!)) (head:app-buffer? b)
                                        (equal? old (head:buffer-lines b))
                                        (equal? old-filtered (head:buffer-lines filtered)))])
                     (message! "m4-after")
                     (show-buffer! filtered)
                     (head:refresh-visible-views!)
                     (show-buffer! was)
                     (list rebound echo-before (messages)
                           (map log:actor (reverse (log:entries component)))
                           (map (lambda (line) (substring line 9 (string-length line)))
                                (vector->list (head:buffer-lines filtered)))
                           (and (not (head:buffer-store-id b)) (not (head:buffer-store-id filtered))
                                (equal? ids (list-sort < (store:buffer-list)))))))))
            '((#t #t #t #t #t) ("m4-two") ("m4-two" "m4-after")
              ((head "wired head λ") (head "wired head λ") (head "other logger")
               (head "wired head λ") (head "wired head λ"))
              ("(head \"wired head λ\")\tmultihead-log: m4-one"
               "(head \"wired head λ\")\tmultihead-log: m4-two"
               "(head \"other logger\")\tmultihead-log: m4-other"
               "(head \"wired head λ\")\tmultihead-log: m4-quiet"
               "(head \"wired head λ\")\tmultihead-log: m4-after") #t))

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

     ;; One live describe page exercises the source/companion boundary and
     ;; head-app reloads and core refusal. Fresh commands resolve the exports;
     ;; retaining an old procedure inside this driver would test old code.
     (check 'describe-page-retains-selection-and-refreshes-through-reload
       (read-editor
         '(let ([request-window (head:current)] [request-buffer (current-buffer)])
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
                    (list (eq? request-window (head:current)) (eq? request-buffer (current-buffer))
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

     (check 'reference-queries-share-one-base-after-head-reloads
       (read-editor
         '(list (eq? describe:fetch-data! reference:fetch!)
                (map (lambda (name) (kernel:module-requires? name "describe")) '("sandbox" "eval"))
                (kernel:module-requires? "sandbox" "head")
                (length (reference:lookup 'describe:show!))
                (let ([text (sandbox:describe-text 'describe:show!)])
                  (and (string:search text "(describe:show! name)" 0 (string-length text)) #t))))
       '(#t (#f #f) #f 1 #t))

     ;; M2 exit, deliberately after the reload scenarios above (Q71): the
     ;; base's live transcript is readable by an actor, then
     ;; its retired plain text and a real file share geometry in split panes.
     (let ([child (string-append probe "-cells.ss")]
           [path (string-append probe "-cells.txt")])
       (call-with-output-file child
         (lambda (port)
           (for-each (lambda (form) (write form port) (newline port))
             '((import (chezscheme))
               (display "\x1b;[2J\x1b;[H界e\x301;Z\r\n")
               (flush-output-port (current-output-port))
               (get-line (current-input-port))))) 'replace)
       (call-with-output-file path
         (lambda (port)
           (display "界e\x301;Z\n" port)
           (display (make-string 19 #\a) port)
           (display "界e\x301;xy\n" port)) 'replace)
       (let* ([terminal-id
               (read-editor
                 `(begin (delete-other-windows!) (head:scrollbar #f)
                         (terminal:open!! ,(string-append "exec scheme-script " child))
                         (head:buffer-store-id (current-buffer))))]
              [live
               (read-editor
                 '(let* ([name (head:buffer-name (current-buffer))]
                         [text (sandbox:read-buffer name 0 1)]
                         [ready (list name (head:buffer-fact (current-buffer) 'alive #f)
                                      (and (string:search text "界éZ" 0 (string-length text)) #t))])
                    (terminal:send! "\n") ready) "\x1d;")]
              [split
               (read-editor
                 `(let ([terminal (head:buffer-of-store-id ,terminal-id)])
                    (set-buffer-wrap! terminal #f)
                    (split-window-right!) (other-window!) (visit-file! ,path)
                    (set-buffer-wrap! (current-buffer) #f)
                    (head:buffer-line-numbers-setting-set! (current-buffer) #f)
                    (head:before-frame!)
                    (list (head:buffer-fact terminal 'alive #t)
                          (surface:snapshot ,terminal-id)
                          (head:buffer-store-id (current-buffer))
                          (map head:window-xoff (head:windows)))))]
              [file-id (caddr split)] [offsets (cadddr split)])
         (check 'live-terminal-has-readable-shared-text live '("*terminal*" #t #t))
         (check 'terminal-death-withdraws-rendition (list (car split) (cadr split)) '(#f #f))
         (check 'dead-terminal-and-file-render-their-glyphs-in-both-panes
           (let* ([line (screen-line 0)] [n (string-length line)]
                  [first (string:search line "界éZ" 0 n)])
             (list first (and first (string:search line "界éZ" (+ first 1) n))))
           (list 0 (- (cadr offsets) 1)))
         (check 'plain-cursors-use-cells-in-both-panes
           (read-editor
             '(map (lambda (w) (paint:window-screen-position w 0 1)) (head:windows)))
           (map (lambda (x) (cons 1 (+ x 3))) offsets))
         (check 'plain-mouse-input-shares-file-and-dead-terminal-geometry
           (let ([points '()])
             (for-each
               (lambda (offset)
                 (for-each
                   (lambda (cell)
                     (let ([x (+ offset cell)])
                       (send! (format "\x1b;[<0;~a;1M\x1b;[<0;~a;1m" x x))
                       (pump! 200)
                       (set! points (cons (read-editor '(point)) points)))) '(2 3))) offsets)
             (reverse points))
           '((0 . 0) (0 . 1) (0 . 0) (0 . 1)))
         (let ([cell
                (read-editor
                  '(begin (set-buffer-wrap! (current-buffer) '(clean . 20))
                          (goto-point! '(1 . 1))
                          (paint:window-screen-position (selected-window) 1 19)))])
           (send! (format "\x1b;[<0;~a;~aM\x1b;[<0;~a;~am"
                          (+ (cdr cell) 1) (car cell) (+ (cdr cell) 1) (car cell)))
           (pump! 200)
           (check 'wrapped-mouse-cell-selects-the-whole-wide-glyph
             (read-editor '(point)) '(1 . 19)))
         (read-editor
           `(begin (delete-other-windows!)
                   (kill-buffer! (head:buffer-of-store-id ,file-id))
                   (kill-buffer! (head:buffer-of-store-id ,terminal-id)) #t)))
       (delete-file child) (delete-file path))

     ;; A shared surface renders without a local app or a mode override.
     ;; Character positions survive a cell grid, including real mouse input;
     ;; a style/link-only publisher wakes the otherwise idle editor.
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
            (head:buffer-line-numbers-setting-set! (current-buffer) #f)
            id)))
     (check 'surface-paints-real-shared-text-and-cell-links
       (list (screen-has? 0 "界éZ")
             (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 1))
       '(#t ("https://surface.example" "wide")))
     (check 'surface-mouse-cells-map-to-source-characters
       (map (lambda (x)
              (send! (format "\x1b;[<0;~a;1M\x1b;[<0;~a;1m" x x))
              (pump! 200)
              (read-editor '(point))) '(2 3))
       '((0 . 0) (0 . 1)))
     (read-editor
       `(begin
          (fork-thread
            (lambda ()
              (sleep (make-time 'time-duration 400000000 0))
              (surface:publish! ,surface-id (car (surface:snapshot ,surface-id)) 0
                '((0 #("32" "32" "1" #f)
                   #(("https://updated.example" #f) ("https://updated.example" #f) #f #f)
                   ((clusters (1 . 2) (2 . 1) (1 . 1))))) #f '(1 4)))) #t))
     (check 'surface-only-worker-wakes-idle-head
       (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) 1)
       '("https://updated.example" #f))
     (define surface-offsets
       (read-editor '(begin (split-window-right!) (map head:window-xoff (head:windows)))))
     (check 'surface-rendition-is-consistent-in-both-panes
       (map (lambda (x) (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) (+ x 1)))
            surface-offsets)
       '(("https://updated.example" #f) ("https://updated.example" #f)))
     (check 'surface-withdrawal-preserves-readable-source
       (read-editor `(begin (surface:withdraw! ,surface-id (car (surface:snapshot ,surface-id)))
                            (store:line ,surface-id 0))) "界e\x301;Z")
     (check 'surface-withdrawal-removes-links-from-both-panes
       (map (lambda (x) (vector-ref (vector-ref (vt:emulator-hyperlinks mirror) 0) (+ x 1)))
            surface-offsets) '(#f #f))
     (read-editor `(begin (delete-other-windows!) (kill-buffer! (head:buffer-of-store-id ,surface-id)) #t))

     ;; A local handler's focus result survives the real mouse dispatcher.
     (define app-cell
       (read-editor
         '(let ([b (head:register-app! "mouse-focus-live" void
                     (lambda (event)
                       (and (string=? event "MOUSE-CLICK")
                            (head:buffer-fact (current-buffer) 'reply #f))))])
            (head:view-replace! b '("first" "second"))
            (split-window-right!)
            (let ([w (cadr (head:windows))])
              (head:set-window-buffer! w b)
              (head:set-current! (car (head:windows)))
              (paint:window-layout)
              (cons (+ (head:window-xoff w) 2) (+ (cadr (assq w (head:layout))) 1))))))
     (check 'app-mouse-focus-and-ignore-results-survive-dispatch
       (map (lambda (reply offset)
              (read-editor
                `(begin (head:buffer-fact-set! (head:window-buffer (cadr (head:windows))) 'reply ',reply) #t))
              (send! (format "\x1b;[<0;~a;~aM\x1b;[<0;~a;~am"
                       (+ (car app-cell) offset) (+ (cdr app-cell) offset)
                       (+ (car app-cell) offset) (+ (cdr app-cell) offset)))
              (pump! 200)
              (read-editor
                '(list (eq? (selected-window) (car (head:windows)))
                       (head:buffer-point (head:window-buffer (cadr (head:windows)))))))
         '(keep-focus ignore-click) '(0 1))
       '((#t (0 . 1)) (#t (0 . 1))))
     (read-editor '(begin (kill-buffer! (head:window-buffer (cadr (head:windows))))
                          (delete-other-windows!) #t))

     ;; A shared endpoint receives keys/paste/pointers while the same surface
     ;; projection drives actual output. Chrome is excluded from cell positions.
     (define shared-pointer
       (read-editor
         '(let* ([owner '(app adapter-live)]
                 [events (kernel:persistent-cell 'wiring-app-events (lambda () '()))]
                 [text (make-vector 12 "abcdefgh")])
            (vector-set! text 8 "界e\x301;Z    ")
            (actor:register! owner (lambda (message) (set-box! events (cons message (unbox events)))))
            (mode:register! "adapter-live" '() '() (lambda (line) #f))
            (keymap:set-context-escape! 'adapter-live "C-]")
            (let* ([id (store:create! owner "*adapter-live*" text
                         `((app . ,owner) (alive . #t) (capture . all) (status . "ready")
                           (read-only . #t) (wrap . #f) (scrollbar . left)
                           (manages-viewport . #t) (cursor-style . bar)))]
                   [b (head:adopt-store-buffer! id)])
              (surface:publish! id #f 0
                '((8 #(plain plain plain plain plain plain plain plain) #(#f #f #f #f #f #f #f #f)
                   ((clusters (1 . 2) (2 . 1) (1 . 1) (1 . 1) (1 . 1) (1 . 1) (1 . 1)))))
                '(8 2 #t) '(4 8))
              (mode:choose! b "adapter-live")
              (head:buffer-line-numbers-setting-set! b #t)
              (show-buffer! b)
              (head:follow-app! (selected-window) #t)
              (paint:window-layout)
              (cons (+ (head:window-xoff (selected-window))
                       (head:window-line-number-width (selected-window)) 4) 1)))))
     (check 'shared-app-paints-grid-and-declared-status
       (list (screen-has? 0 "界éZ")
             (exists (lambda (row) (screen-has? row "ready capturing input")) (iota 24))) '(#t #t))
     (send! "x\x1b;[200~paste\ntext\x1b;[201~")
     (pump! 250)
     (check 'shared-app-receives-real-key-and-paste-as-owned-data
       (read-editor
         '(map (lambda (message)
                 (list (cadr message) (cadddr message)
                       (let ([paste (assq 'paste (list-ref message 4))]) (and paste (cdr paste)))))
            (reverse (filter (lambda (message)
                               (and (eq? (car message) 'input) (member (cadddr message) '("x" "PASTE"))))
                       (unbox (kernel:persistent-cell 'wiring-app-events (lambda () '()))))))
         "\x1d;")
       '(((head "wired head λ") "x" #f) ((head "wired head λ") "PASTE" "paste\ntext")))
     ;; The escaped probe is an editor command and pauses following. Resume
     ;; with real input before addressing cells in the live grid again.
     (send! "x")
     (pump! 150)
     (send! (format "\x1b;[<0;~a;~aM\x1b;[<32;~a;~aM\x1b;[<0;~a;~am\x1b;[<64;~a;~aM"
              (car shared-pointer) (cdr shared-pointer) (car shared-pointer) (cdr shared-pointer)
              (car shared-pointer) (cdr shared-pointer) (car shared-pointer) (cdr shared-pointer)))
     (pump! 250)
     (check 'all-pointer-phases-share-character-cell-and-viewport-coordinates
       (read-editor
         '(map (lambda (message)
                 (let ([data (list-ref message 4)])
                   (list (cadddr message) (cdr (assq 'point data)) (cdr (assq 'cell data))
                         (cdr (assq 'viewport data)) (cdr (assq 'button data)))))
            (reverse (filter (lambda (message)
                               (and (eq? (car message) 'input)
                                    (member (cadddr message) '("MOUSE-CLICK" "MOUSE-DRAG" "MOUSE-RELEASE" "WHEEL-UP"))))
                       (unbox (kernel:persistent-cell 'wiring-app-events (lambda () '()))))))
         "\x1d;")
       '(("MOUSE-CLICK" (8 . 1) (8 . 2) (3 . 1) 0)
         ("MOUSE-DRAG" (8 . 1) (8 . 2) (3 . 1) 32)
         ("MOUSE-RELEASE" (8 . 1) (8 . 2) (3 . 1) 0)
         ("WHEEL-UP" (8 . 1) (8 . 2) (3 . 1) 64)))
     (check 'shared-app-escape-runs-a-complete-editor-command
       (read-editor '(list (eq? (head:escaped-buffer) (current-buffer))
                           (head:app-status (current-buffer) #t)) "\x1d;")
       '(#t "ready escaped"))
     (read-editor '(let ([b (current-buffer)])
                     (actor:detach! '(app adapter-live))
                     (kill-buffer! b) #t) "\x1d;")

     ;; Finish through the real quit path. A shutdown hook can still read
     ;; reviewed work, but cannot commit new work after the user's consent.
     (read-editor
       `(begin
          (head:add-shutdown-hook!
            (lambda ()
              (call-with-output-file ,probe
                (lambda (p)
                  (write (list (head:quitting?) (pair? (store:buffer-list))
                           (guard (ex [(kernel:refusal? ex) #t] [else (raise ex)])
                             (store:create! head:ui-actor "after quit" '("lost")) #f)) p))
                'replace))) #t))
     (delete-file probe)
     (send! "\x18;\x03;")
     (pump! 500)
     (check 'final-quit-reaches-confirmation
       (or (screen-has? 22 "Modified buffers exist") (screen-has? 23 "Modified buffers exist")) #t)
     (send! "y")
     (test:await 'standalone-quit-exits (lambda () (pump! 25) exited?))
     (check 'standalone-quit-closes-admission-before-shutdown-hooks
       (call-with-input-file probe read) '(#t #t #t))
     (delete-file probe)
     (sys:close-terminal-process! process)
     (test:finish! 'wiring)))
