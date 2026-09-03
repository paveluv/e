#!/usr/bin/env scheme-script

;; Terminal input decoding: bytes in, data events out -- the first
;; time the parser is testable without a PTY.  v2 core dissolution
;; (docs/DESIGN2.md).  Run from the repository root.

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

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

     (check 'color-scheme-report (events "\x1b;[?997;1n")
            '((host-color-scheme dark)))
     (check 'light-report (events "\x1b;[?997;2n")
            '((host-color-scheme light)))

     ;; an unknown host report is swallowed, never leaks as typed text
     (check 'unknown-report-swallowed (events "\x1b;[?42;0nq") '("q"))
     (check 'unknown-csi-swallowed (events "\x1b;[99jq") '("q"))

     ;; -- event helpers ---------------------------------------------------------

     (check 'event-character (tty:key-event-character "a") #\a)
     (check 'control-has-no-character (tty:key-event-character "C-a") #f)
     (check 'character-event (tty:character-event #\x2) "C-b")

     (format #t "~a tty checks passed\n" checks)))
