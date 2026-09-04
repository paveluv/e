#!/usr/bin/env scheme-script

;; Drive vttest against the headless terminal emulator.
;;
;; vttest runs in a PTY, its output feeds a headless emulator, and the
;; emulator's replies (DSR, DA, DECRQM, ...) are written back so the
;; report tests complete. Every settled screen is appended to a dump file
;; for review, the raw byte stream is tee'd next to it for byte-level
;; analysis, and the run ends with the emulator's unsupported-feature
;; report -- anything the emulator silently mishandles shows up either as
;; a wrong frame or as a missing entry there.
;;
;; Run from the repository root with a dump file and a menu plan:
;;
;;     scheme --script tests/vttest-drive.ss /tmp/vt.txt 1 2 3 8
;;
;; Each number is typed at whatever menu is currently showing, so nested
;; menus are visited by listing their choices in order, with 0 to leave:
;;
;;     ... vt.txt 6 1 2 3 4 5 6 7 0        # menu 6 and its report tests
;;     ... vt.txt 11 5 1 2 3 4 5 6 7 8 9 0 # ISO-6429 cursor movement
;;
;; Between choices the driver presses RETURN through "Push <RETURN>"
;; screens until a menu reappears; a screen that wants other input is
;; dumped with a STUCK label and skipped. Keyboard, mouse, and VT52 menus
;; need a human. The geometry argument to vttest (24x80, maximum 80
;; columns) matches the emulator and disables the 132-column passes,
;; which need a resizable host.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  `(begin
     (import (prefix (sys) sys:) (prefix (terminal) terminal:))

     (define dump-file ,(if (>= (length (command-line)) 2)
                            (cadr (command-line))
                            (error 'vttest-drive
                                   "usage: vttest-drive.ss DUMP-FILE MENU...")))
     (define menus ',(map string->number (cddr (command-line))))

     (define vt (terminal:make-emulator 24 80))
     (define process
       (sys:spawn-terminal-process "/bin/sh" "exec vttest -u 24x80.80"
                               (current-directory) 24 80))
     (define from (transcoded-port
                    (sys:terminal-process-input process)
                    (make-transcoder (latin-1-codec) 'none 'replace)))
     (define out (open-output-file dump-file 'replace))
     (define raw (open-file-output-port
                   (string-append dump-file ".raw")
                   (file-options no-fail)))
     (define replies-sent 0)

     (define (send-bytes! bytes)
       (let ([port (sys:terminal-process-output process)])
         (put-bytevector port bytes)
         (flush-output-port port)))

     (define (send! text)
       (send-bytes! (string->utf8 text)))

     (define (pump-replies!)
       (let ([replies (terminal:emulator-replies vt)])
         (let loop ([pending (list-tail replies replies-sent)])
           (unless (null? pending)
             (let* ([text (car pending)]
                    [bytes (make-bytevector (string-length text))])
               (do ([i 0 (+ i 1)]) ((= i (string-length text)))
                 (bytevector-u8-set!
                   bytes i (char->integer (string-ref text i))))
               (send-bytes! bytes))
             (set! replies-sent (+ replies-sent 1))
             (loop (cdr pending))))))

     (define (drain!)
       ;; #t when anything arrived.
       (let loop ([any #f])
         (if (guard (ex [else #f]) (char-ready? from))
             (let ([c (guard (ex [else (eof-object)]) (get-char from))])
               (if (eof-object? c) any
                   (begin (put-u8 raw (bitwise-and (char->integer c) 255))
                          (terminal:emulator-feed! vt (string c))
                          (loop #t))))
             any)))

     (define (settle!)
       ;; Wait until output has been quiet for ~400 ms (10 s cap covers
       ;; screens vttest paints with deliberate delays).
       (let loop ([quiet 0] [total 0])
         (let ([any (drain!)])
           (pump-replies!)
           (cond [(> total 400) (void)]
                 [(and (not any) (> quiet 16)) (void)]
                 [else
                  (sleep (make-time 'time-duration 25000000 0))
                  (loop (if any 0 (+ quiet 1)) (+ total 1))]))))

     (define frame 0)
     (define (dump! label)
       (set! frame (+ frame 1))
       (put-string out (format "==== frame ~a ~a ====\n" frame label))
       (vector-for-each
         (lambda (line) (put-string out (format "|~a|\n" line)))
         (terminal:emulator-screen vt))
       (flush-output-port out))

     (define (screen-contains? part)
       (let ([m (string-length part)])
         (call/cc
           (lambda (return)
             (vector-for-each
               (lambda (line)
                 (let ([n (string-length line)])
                   (do ([at 0 (+ at 1)]) ((> (+ at m) n))
                     (when (string=? (substring line at (+ at m)) part)
                       (return #t)))))
               (terminal:emulator-screen vt))
             #f))))

     (define (at-menu?)
       (and (screen-contains? "Enter choice number")
            (screen-contains? "Exit")))

     (define (run-test! number)
       (send! (format "~a\r" number))
       (settle!)
       (dump! (format "menu-~a-entry" number))
       (let loop ([presses 0])
         (cond
           [(at-menu?) (void)]
           [(> presses 40)
            (dump! (format "menu-~a-STUCK" number))]
           [else
            (send! "\r")
            (settle!)
            (dump! (format "menu-~a-screen-~a" number (+ presses 1)))
            (loop (+ presses 1))])))

     (settle!)
     (dump! 'intro)
     (unless (at-menu?) (send! "\r") (settle!) (dump! 'main-menu))
     (for-each run-test! menus)
     (send! "0\r")
     (settle!)
     (put-string out "==== unsupported ====\n")
     (for-each (lambda (item) (put-string out (format "~a\n" item)))
               (terminal:emulator-unsupported vt))
     (close-port out)
     (close-port raw)
     (sys:close-terminal-process! process)
     (display (format "done: ~a frames, ~a unsupported\n"
                      frame
                      (length (terminal:emulator-unsupported vt))))))
