#!/usr/bin/env scheme-script

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (sys))

     (define checks 0)

     (define (check label true?)
       (set! checks (+ checks 1))
       (unless true? (error 'terminal-process-test label)))

     (define (contains? text part)
       (let ([n (string-length text)] [m (string-length part)])
         (let loop ([at 0])
           (and (<= (+ at m) n)
                (or (string=? (substring text at (+ at m)) part)
                    (loop (+ at 1)))))))

     (define (read-process process)
       (let ([input (transcoded-port
                      (terminal-process-input process)
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
             (spawn-terminal-process
               "/bin/sh"
               "printf 'pid=%s tty=' $$; if test -t 0; then printf yes; else printf no; fi; printf ' size='; stty size"
               (current-directory) 13 47)]
            [pid (terminal-process-pid process)]
            [output (read-process process)])
       (reap-terminal-process! process)
       (check 'direct-exec-pid
              (contains? output (format "pid=~a" pid)))
       (check 'controlling-terminal (contains? output "tty=yes"))
       (check 'initial-window-size (contains? output "size=13 47")))

     (let* ([process
             (spawn-terminal-process
               "/bin/sh"
               "trap 'printf resized=; stty size; exit 0' WINCH; echo ready; while :; do sleep 1; done"
               (current-directory) 5 20)]
            [input (transcoded-port
                     (terminal-process-input process)
                     (make-transcoder (utf-8-codec) 'none 'replace))])
       (check 'resize-child-ready (string=? (get-line input) "ready\r"))
       (resize-terminal-process! process 9 37)
       (let loop ([characters '()])
         (guard (ex [(i/o-read-error? ex)
                     (let ([output (list->string (reverse characters))])
                       (check 'resized-window-size
                              (contains? output "resized=9 37")))]
                    [else (raise ex)])
           (let ([character (get-char input)])
             (unless (eof-object? character)
               (loop (cons character characters))))))
       (reap-terminal-process! process))

     (let* ([process
             (spawn-terminal-process
               "/bin/sh"
               "trap '' TERM; echo ready; while :; do sleep 1; done"
               (current-directory) 5 20)]
            [start (current-time 'time-monotonic)])
       ;; Let the shell install its ignored-SIGTERM disposition.
       (sleep (make-time 'time-duration 50000000 0))
       (close-terminal-process! process)
       (let* ([elapsed (time-difference (current-time 'time-monotonic) start)]
              [milliseconds (+ (* (time-second elapsed) 1000)
                               (quotient (time-nanosecond elapsed) 1000000))])
         (check 'bounded-stubborn-child (< milliseconds 1000))))

     (format #t "~a terminal process checks passed\n" checks)))
