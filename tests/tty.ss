#!/usr/bin/env scheme-script

;; Terminal input decoding: bytes in, data events out, testable
;; without a PTY.  Run from the repository root.

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (tty) tty:)
             (only (chezscheme) format open-string-input-port))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'tty-test label actual expected)))

     (define (events text)
       ;; decode the whole byte string
       (let ([port (open-string-input-port text)])
         (let loop ([acc '()])
           (let ([event (tty:read-event port)])
             (if (eof-object? event)
                 (reverse acc)
                 (loop (cons event acc)))))))

     ;; -- plain keys ---------------------------------------------------------

     (check 'plain (events "ab") '("a" "b"))
     (check 'control (events "\x1;\x18;") '("C-a" "C-x"))
     (check 'specials (events "\x0;\x9;\xd;\x7f;")
            '("C-@" "TAB" "RET" "BACKSPACE"))
     (check 'high-controls (events "\x1c;\x1d;\x1e;\x1f;")
            '("C-\\" "C-]" "C-^" "C-_"))
     (check 'unicode (events "λ") '("λ"))

     ;; -- escape and meta ------------------------------------------------------

     (check 'meta (events "\x1b;f") '("M-f"))
     (check 'meta-space (events "\x1b; ") '("M-SPC"))
     (check 'control-meta (events "\x1b;\x6;") '("C-M-f"))

     ;; a lone ESC at the end of input is the ESC key
     (check 'bare-esc (events "\x1b;") '("ESC"))

     ;; -- CSI keys -------------------------------------------------------------

     (check 'arrows (events "\x1b;[A\x1b;[B\x1b;[C\x1b;[D")
            '("UP" "DOWN" "RIGHT" "LEFT"))
     (check 'modified-arrow (events "\x1b;[1;5A") '("C-UP"))
     (check 'shift-meta-arrow (events "\x1b;[1;4C") '("M-S-RIGHT"))
     (check 'home-end (events "\x1b;[H\x1b;[F") '("HOME" "END"))
     (check 'tilde-keys (events "\x1b;[3~\x1b;[5~\x1b;[6~")
            '("DELETE" "PAGEUP" "PAGEDOWN"))
     (check 'insert-plain (events "\x1b;[2~") '("INSERT"))
     (check 'modified-tilde (events "\x1b;[3;5~\x1b;[5;3~")
            '("C-DELETE" "M-PAGEUP"))
     (check 'function-keys (events "\x1b;OP\x1b;[15~\x1b;[24~")
            '("F1" "F5" "F12"))
     (check 'modified-function (events "\x1b;[15;2~") '("F17"))
     (check 'shift-tab (events "\x1b;[Z") '("S-TAB"))
     (check 'keypad (events "\x1b;Op\x1b;OM") '("KP-0" "KP-ENTER"))

     ;; -- mouse, paste, host reports ------------------------------------------

     (check 'mouse-press (events "\x1b;[<0;12;3M")
            '((mouse #\M 0 12 3)))
     (check 'mouse-release (events "\x1b;[<0;12;3m")
            '((mouse #\m 0 12 3)))
     (check 'wheel (events "\x1b;[<64;5;7M")
            '((mouse #\M 64 5 7)))

     (check 'bracketed-paste
            (events "\x1b;[200~hello\nworld\x1b;[201~x")
            '((paste . "hello\nworld") "x"))
     (check 'paste-with-esc-inside
            (events "\x1b;[200~a\x1b;[Bb\x1b;[201~")
            '((paste . "a\x1b;[Bb")))

     (check 'theme-reports-and-background-color-formats
       (map events '("\x1b;[?997;1n" "\x1b;[?997;2n"
                     "\x1b;]11;rgb:0/0/0\x7;" "\x1b;]11;rgb:ff/ff/ff\x1b;\\"
                     "\x1b;]11;rgb:1234/abcd/FfFf\x7;" "\x1b;]11;#123456\x7;"))
       '(((host-color-scheme dark)) ((host-color-scheme light))
         ((host-background 0 0 0)) ((host-background 255 255 255))
         ((host-background 18 171 255)) ((host-background 18 52 86))))

     ;; Invalid or unknown replies never become typed text or kill the reader.
     (let ([reports
            (append '("\x1b;[?42;0n" "\x1b;[?997n" "\x1b;[?997;0n" "\x1b;[?997;2;1n"
                      "\x1b;]10;rgb:ff/ff/ff\x7;" "\x1b;]11;rgb:/ff/ff\x7;"
                      "\x1b;]11;rgb:1+i/ff/ff\x7;" "\x1b;]11;rgb:+f/ff/ff\x7;"
                      "\x1b;]11;rgb:12345/ff/ff\x1b;\\" "\x1b;]11;rgb:ff/ff/ff/ff\x7;"
                      "\x1b;]11;rgb:ff/ff/ff\x18;")
                    (list (string-append "\x1b;]11;" (make-string 1024 #\a) "\x1b;\\")))])
       (check 'unknown-and-invalid-reports-swallowed
         (map (lambda (report) (events (string-append report "q"))) reports)
         (map (lambda (_) '("q")) reports)))
     (check 'unfinished-background-report-at-eof
       (events "\x1b;]11;rgb:ff/ff/ff\x1b;") '())
     (check 'unknown-csi-swallowed (events "\x1b;[99jq") '("q"))

     ;; -- event helpers ---------------------------------------------------------

     (check 'event-character (tty:key-event-character "a") #\a)
     (check 'control-has-no-character (tty:key-event-character "C-a") #f)
     (check 'character-event (tty:character-event #\x2) "C-b")

     (format #t "~a tty checks passed\n" checks)))
