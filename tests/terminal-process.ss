#!/usr/bin/env scheme-script

(import (chezscheme))

(library-directories (list (cons "lib" "eo") (cons "tests" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (prefix (sys) sys:) (prefix (terminal) terminal:)
             (prefix (head) head:) (prefix (mode) mode:) (prefix (paint) paint:)
             (prefix (test) test:))

     (define (check label true?)
       (test:check label true? #t))

     (define (contains? text part)
       (let ([n (string-length text)] [m (string-length part)])
         (let loop ([at 0])
           (and (<= (+ at m) n)
                (or (string=? (substring text at (+ at m)) part)
                    (loop (+ at 1)))))))

     (define (read-process process)
       (let ([input (transcoded-port
                      (sys:terminal-process-input process)
                      (make-transcoder (utf-8-codec) 'none 'replace))])
         (let loop ([characters '()])
           (guard (ex [(i/o-read-error? ex)
                       (list->string (reverse characters))]
                      [else (raise ex)])
             (let ([character (get-char input)])
               (if (eof-object? character)
                   (list->string (reverse characters))
                   (loop (cons character characters))))))))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "printf 'pid=%s tty=' $$; if test -t 0; then printf yes; else printf no; fi; printf ' size='; stty size; printf ' term=%s' \"$TERM\""
               (current-directory) 13 47)]
            [pid (sys:terminal-process-pid process)]
            [output (read-process process)])
       (sys:reap-terminal-process! process)
       (check 'direct-exec-pid
              (contains? output (format "pid=~a" pid)))
       (check 'controlling-terminal (contains? output "tty=yes"))
       (check 'initial-window-size (contains? output "size=13 47"))
       (check 'advertised-terminal (contains? output "term=xterm-256color")))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "read value; printf 'input=<%s>\\n' \"$value\"; printf 'stderr-line\\n' >&2"
               (current-directory) 5 20)]
            [output (sys:terminal-process-output process)])
       (put-bytevector output (string->utf8 "hello terminal\n"))
       (flush-output-port output)
       (let ([text (read-process process)])
         (sys:reap-terminal-process! process)
         (check 'interactive-input (contains? text "input=<hello terminal>"))
         (check 'stderr-shares-pty (contains? text "stderr-line"))))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "trap 'printf resized=; stty size; exit 0' WINCH; echo ready; while :; do sleep 1; done"
               (current-directory) 5 20)]
            [input (transcoded-port
                     (sys:terminal-process-input process)
                     (make-transcoder (utf-8-codec) 'none 'replace))])
       (check 'resize-child-ready (string=? (get-line input) "ready\r"))
       (sys:resize-terminal-process! process 9 37)
       (let loop ([characters '()])
         (guard (ex [(i/o-read-error? ex)
                     (let ([output (list->string (reverse characters))])
                       (check 'resized-window-size
                              (contains? output "resized=9 37")))]
                    [else (raise ex)])
           (let ([character (get-char input)])
             (unless (eof-object? character)
               (loop (cons character characters))))))
       (sys:reap-terminal-process! process))

     (let* ([process
             (sys:spawn-terminal-process
               "/bin/sh"
               "trap '' TERM; echo ready; while :; do sleep 1; done"
               (current-directory) 5 20)]
            [start (current-time 'time-monotonic)])
       ;; Let the shell install its ignored-SIGTERM disposition.
       (sleep (make-time 'time-duration 50000000 0))
       (sys:close-terminal-process! process)
       (let* ([elapsed (time-difference (current-time 'time-monotonic) start)]
              [milliseconds (+ (* (time-second elapsed) 1000)
                               (quotient (time-nanosecond elapsed) 1000000))])
         (check 'bounded-stubborn-child (< milliseconds 1000))))

     ;; A real reader must keep parsing while a repaint callback reenters
     ;; refresh. One child supplies a second frame, then exits mid-2026 hold.
     (let ([child (format "/tmp/e-terminal-frame-~a.ss" (get-process-id))]
           [previous (head:window-buffer (head:current))]
           [buffer #f] [phase 'initial] [complete? #f] [coherent? #f]
           [retired (test:recorder)])
       (define (cells)
         ((mode:render (mode:of buffer)) buffer 0 (vector-ref (head:buffer-lines buffer) 0)))
       (define (row-text)
         (let ([row (cells)]) (and row (apply string-append (vector->list row)))))
       (define (pump)
         (call/cc
           (lambda (drained)
             (head:run-on-main! (lambda () (drained #t)))
             (parameterize ([head:in-main-pump #t]) (head:read-key-event))))
         (head:run-deferred!)
         (head:refresh-visible-views!))
       (dynamic-wind
         (lambda ()
           (call-with-output-file child
             (lambda (port)
               (for-each (lambda (form) (write form port) (newline port))
                 '((import (chezscheme))
                   (read)
                   (display "\x1b;[?1049h\x1b;[?25l\x1b;[6 q\x1b;[32m\x1b;]8;id=live;https://frame.example\x1b;\\界q\x301;NEW\x1b;]8;;\x1b;\\")
                   (flush-output-port)
                   (read)
                   (display "\x1b;[?2026h\x1b;[2J\x1b;[H\x1b;[35m\x1b;]8;id=final;https://final.example\x1b;\\界q\x301;FINAL\x1b;]8;;\x1b;\\")
                   (flush-output-port)))) 'replace)
           (terminal:init!)
           (terminal:shell "/bin/sh")
           (head:set-frame-hook! (lambda () (void)))
           (paint:set-screen-rows! 6)
           (paint:set-screen-cols! 20)
           (paint:window-layout)
           (terminal:open!! (format "exec scheme-script ~a" child))
           (set! buffer (head:window-buffer (head:current))))
         (lambda ()
           (head:set-repaint-hook!
             (lambda ()
               (paint:invalidate-screen-cache!)
               (case phase
                 [(initial)
                  (set! phase 'waiting)
                  (let ([row (cells)]
                        [styles ((mode:row-styles (mode:of buffer)) buffer 0
                                 (vector-ref (head:buffer-lines buffer) 0))])
                    (set! coherent? (and (vector? row) (vector? styles)
                                         (= (vector-length row) (vector-length styles)
                                            (string-length (vector-ref (head:buffer-lines buffer) 0))))))
                  (terminal:send! "next\n")
                  (test:await 'reader-during-repaint
                    (lambda ()
                      (pump)
                      (let ([text (row-text)]) (and text (contains? text "NEW")))))
                  (set! complete? #t)]
                 [(exit)
                  (when (not (head:app-buffer? buffer))
                    (retired (list (cells) (paint:buffer-line-hyperlinks buffer 0)
                                   (head:app-cursor-style buffer))))])))
           (head:refresh-visible-views!)
           (check 'rendition-is-installed-before-the-first-repaint coherent?)
           (check 'reader-and-reentrant-refresh-complete-during-repaint complete?)
           (test:check 'outer-refresh-does-not-overwrite-the-newer-frame
             (list (substring (row-text) 0 6) (head:buffer-fact buffer 'cursor-style #f)
                   (head:app-cursor-visible-in? (head:current))
                   (paint:buffer-line-hyperlinks buffer 0))
             '("界q\x301;NEW" bar #f ((0 6 "https://frame.example" "live"))))
           (set! phase 'exit)
           (terminal:send! "exit\n")
           (head:set-window-buffer! (head:current) previous)
           (test:await 'terminal-transcript
             (lambda () (pump) (not (head:app-buffer? buffer))))
           (check 'offscreen-exit-flushes-an-unfinished-synchronized-frame
             (contains? (vector-ref (head:buffer-lines buffer) 0) "界q\x301;FINAL"))
           (test:check 'death-retires-all-rendition-before-notifying
             (retired) '((#f () #f))))
         (lambda ()
           (head:set-repaint-hook! paint:invalidate-screen-cache!)
           (head:set-frame-hook! paint:redraw!)
           (when buffer
             (guard (ex [else (void)]) (terminal:close! buffer))
             (head:forget-buffer! buffer))
           (when (file-exists? child) (delete-file child)))))

     (test:finish! 'terminal-process)))
