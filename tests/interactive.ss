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
     (import (sys) (terminal))

     ;; The nested terminal must run a predictable shell.
     (putenv "SHELL" "/bin/sh")

     (define checks 0)
     (define mirror (make-terminal-emulator 24 80))
     (define process
       (spawn-terminal-process "/bin/sh" "exec ./e" (current-directory) 24 80))
     (define from-editor
       (transcoded-port (terminal-process-input process)
                        (make-transcoder (utf-8-codec) 'none 'replace)))
     (define transcript '())

     (define (drain!)
       (when (guard (ex [else #f]) (char-ready? from-editor))
         (let ([character (guard (ex [else (eof-object)])
                            (get-char from-editor))])
           (unless (eof-object? character)
             (set! transcript (cons character transcript))
             (terminal-emulator-feed! mirror (string character))
             (drain!)))))

     (define (settle! milliseconds)
       (let loop ([left (div milliseconds 25)])
         (drain!)
         (when (> left 0)
           (sleep (make-time 'time-duration 25000000 0))
           (loop (- left 1)))))

     (define (send! text)
       (let ([output (terminal-process-output process)])
         (put-bytevector output (string->utf8 text))
         (flush-output-port output)))

     (define (screen-lines)
       (vector->list (terminal-emulator-screen mirror)))

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
       (vector-ref (vector-ref (terminal-emulator-styles mirror) (car cell))
                   (cdr cell)))

     ;; -- start the editor and open a nested terminal ---------------------
     (wait-for! 'editor-starts
                (lambda () (find-cell "*scratch*")) 30000)
     (send! "\x3;t")                    ; C-c t
     (wait-for! 'nested-terminal-opens
                (lambda () (find-cell "capturing input")) 10000)

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

     ;; -- shut down cleanly ----------------------------------------------
     (send! "\x1b;[6;2~")
     (settle! 300)
     (send! "exit\r")
     (wait-for! 'shell-exit-frees-buffer
                (lambda () (not (find-cell "capturing input"))) 10000)
     (send! "\x18;\x3;")                ; C-x C-c
     (let loop ()                       ; block until the editor exits
       (let ([character (guard (ex [else (eof-object)])
                          (get-char from-editor))])
         (unless (eof-object? character) (loop))))
     (reap-terminal-process! process)
     (check 'editor-quits #t)

     (format #t "~a interactive checks passed\n" checks)))
