#!/usr/bin/env scheme-script

;; The core-to-state wiring: every core buffer mirrors into the
;; (state) store, core edits arrive there transactionally, and a
;; foreign actor's store edit appears on the user's screen -- v2
;; stage 1 (docs/DESIGN2.md).  Drives a live editor over a PTY; run
;; from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (sys) (terminal))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'wiring-test label actual expected)))

     (define probe (format "/tmp/e-wiring-~a" (getenv "USER")))

     (putenv "SHELL" "/bin/sh")
     (define mirror (make-terminal-emulator 24 100))
     (define process
       (spawn-terminal-process "/bin/sh" "exec ./e"
                               (current-directory) 24 100))
     (define from (transcoded-port
                    (terminal-process-input process)
                    (make-transcoder (utf-8-codec) 'none 'replace)))
     (define (pump! ms)
       (let loop ([left (div ms 25)])
         (let drain ()
           (when (guard (ex [else #f]) (char-ready? from))
             (let ([c (guard (ex [else (eof-object)]) (get-char from))])
               (unless (eof-object? c)
                 (terminal-emulator-feed! mirror (string c)) (drain)))))
         (when (> left 0)
           (sleep (make-time 'time-duration 25000000 0))
           (loop (- left 1)))))
     (define (send! text)
       (put-bytevector (terminal-process-output process)
                       (string->utf8 text))
       (flush-output-port (terminal-process-output process)))
     (define (screen-line n)
       (vector-ref (terminal-emulator-screen mirror) n))
     (define (screen-has? n needle)
       (let* ([line (screen-line n)]
              [len (string-length needle)])
         (let scan ([i 0])
           (cond [(> (+ i len) (string-length line)) #f]
                 [(string=? (substring line i (+ i len)) needle) #t]
                 [else (scan (+ i 1))]))))

     ;; ask the editor whether the current buffer's lines equal its
     ;; state twin's, writing the verdict to the probe file
     (define (mirror-agrees? label)
       (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (let* ([b (current-buffer)] [id (buffer-state-id b)] [n (buffer-line-count b)]) (and (= n (state:line-count id)) (let all ([i 0]) (or (= i n) (and (string=? (buffer-line b i) (state:line id i)) (all (+ i 1))))))) p)) (quote replace)\r"
                      probe))
       (pump! 900)
       (equal? (call-with-input-file probe read) #t))

     (pump! 3000)

     ;; -- core edits mirror --------------------------------------------------

     (send! "hello")
     (pump! 400)
     (check 'typing-mirrors (mirror-agrees? 'typing) #t)

     (send! "\rworld")                 ; RET: the splice path
     (pump! 400)
     (check 'newline-splice-mirrors (mirror-agrees? 'newline) #t)

     (send! "\x1;\xb;")               ; C-a C-k: kill to end of line
     (pump! 400)
     (check 'kill-mirrors (mirror-agrees? 'kill) #t)

     (send! "\x1f;")                   ; C-_: undo (the reset path)
     (pump! 400)
     (check 'undo-mirrors (mirror-agrees? 'undo) #t)

     ;; -- a foreign actor's edit reaches the screen ---------------------------

     (send! "\x1b;xstate:edit! (quote (agent tester)) (buffer-state-id (current-buffer)) (state:revision (buffer-state-id (current-buffer))) (text:make-span 0 0 0 0) (list \"AGENT \")\r")
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
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (exists (lambda (entry) (eq? (cadr entry) (quote state))) (log-entries)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'foreign-edit-audited (call-with-input-file probe read) #t)

     ;; the human's cursor is a mark other actors can read
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (equal? (state:mark (quote (head main)) (buffer-state-id (current-buffer)) (quote point)) (point)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'point-published-as-mark
            (call-with-input-file probe read) #t)

     ;; a foreign edit above the cursor rebases it, not clamps it
     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (car (point)) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (let ([row-before (call-with-input-file probe read)])
       (send! "\x1b;xstate:edit! (quote (agent tester)) (buffer-state-id (current-buffer)) (state:revision (buffer-state-id (current-buffer))) (text:make-span 0 0 0 0) (list \"above\" \"\")\r")
       (pump! 1200)
       (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (car (point)) p)) (quote replace)\r"
                      probe))
       (pump! 900)
       (check 'cursor-rebases-across-foreign-insert
              (call-with-input-file probe read)
              (+ row-before 1)))

     ;; undo refuses to time-travel over the agent's work
     (send! "\x1b;xundo!\r")
     (pump! 900)
     (check 'undo-blocked-after-foreign-edit
            (screen-has? 23 "blocked") #t)

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
     (send! "\x1b;xfork-thread (lambda () (sleep (make-time (quote time-duration) 400000000 0)) (state:edit! (quote (agent background)) (buffer-state-id (current-buffer)) (state:revision (buffer-state-id (current-buffer))) (text:make-span 0 0 0 0) (list \"WOKEN \")))\r")
     (pump! 300)                       ; the eval returns; the loop sleeps
     (pump! 1700)                      ; no keys: only the wake can paint
     (check 'foreign-edit-appears-without-a-keypress
            (substring (screen-line 0) 0 6)
            "WOKEN ")

     ;; mastery: the core's line cache IS the store's immutable text

     (send! (format "\x1b;xcall-with-output-file \"~a\" (lambda (p) (write (let-values ([(text rev) (state:snapshot (buffer-state-id (current-buffer)))]) (eq? text (buffer-lines (current-buffer)))) p)) (quote replace)\r"
                    probe))
     (pump! 900)
     (check 'cache-is-the-store-text
            (call-with-input-file probe read) #t)

     ;; the interaction protocol: an agent asks, the head answers ------
     (send! (format "\x1b;xactors:ask! (quote (agent tester)) (quote (head main)) \"Proceed with the plan?\" (list \"yes\" \"no\") (lambda (answer) (call-with-output-file \"~a\" (lambda (p) (write answer p)) (quote replace)))\r"
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

     (delete-file probe)
     (close-terminal-process! process)
     (format #t "~a wiring checks passed\n" checks)))
