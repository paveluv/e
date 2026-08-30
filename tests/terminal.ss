#!/usr/bin/env scheme-script

(import (chezscheme))

(library-directories (list (cons "lib" "eo")))
(library-extensions (cons '(".e" . ".eo") (library-extensions)))
(compile-imported-libraries #t)

(eval
  '(begin
     (import (terminal))

     (define checks 0)

     (define (check label actual expected)
       (set! checks (+ checks 1))
       (unless (equal? actual expected)
         (error 'terminal-test label actual expected)))

     (define (state-ref emulator key)
       (cdr (assq key (terminal-emulator-state emulator))))

     (let ([terminal (make-terminal-emulator 2 4)])
       (terminal-emulator-feed! terminal "abcd")
       (check 'delayed-wrap-screen
              (vector->list (terminal-emulator-screen terminal))
              '("abcd" "    "))
       (check 'delayed-wrap-cursor (state-ref terminal 'cursor) '(0 . 3))
       (check 'delayed-wrap-pending (state-ref terminal 'wrap-pending) #t)
       (terminal-emulator-feed! terminal "e")
       (check 'delayed-wrap-next-character
              (vector->list (terminal-emulator-screen terminal))
              '("abcd" "e   ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1b;[?7labcde")
       (check 'autowrap-disabled
              (vector->list (terminal-emulator-screen terminal))
              '("abce")))

     (let ([terminal (make-terminal-emulator 5 5)])
       (terminal-emulator-feed! terminal "\x1b;[2;4r\x1b;[?6h\x1b;[2;3HX")
       (check 'origin-relative-cursor (state-ref terminal 'cursor) '(2 . 3))
       (check 'origin-relative-screen
              (vector-ref (terminal-emulator-screen terminal) 2)
              "  X  "))

     (let ([terminal (make-terminal-emulator 1 5)])
       (terminal-emulator-feed! terminal "abc\x1b;[2G\x1b;[4hX")
       (check 'insert-mode
              (vector->list (terminal-emulator-screen terminal))
              '("aXbc ")))

     (let ([terminal (make-terminal-emulator 1 10)])
       (terminal-emulator-feed! terminal "a\tb")
       (check 'default-tabs
              (vector->list (terminal-emulator-screen terminal))
              '("a       b ")))

     (let ([terminal (make-terminal-emulator 1 5)])
       (terminal-emulator-feed! terminal "z\x1b;[3b")
       (check 'repeat-character
              (vector->list (terminal-emulator-screen terminal))
              '("zzzz ")))

     (let ([terminal (make-terminal-emulator 1 2)])
       (terminal-emulator-feed! terminal "\x1b;[1mX")
       (check 'sgr-cell-style
              (eq? (vector-ref (vector-ref
                                 (terminal-emulator-styles terminal) 0) 0)
                               'plain)
              #f))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[?25l\x1b;[?1h\x1b;[?1002;1006h")
       (check 'cursor-hidden (state-ref terminal 'cursor-visible) #f)
       (check 'application-cursor-keys
              (terminal-emulator-input terminal "UP")
              (string->utf8 "\x1b;OA"))
       (check 'mouse-tracking (state-ref terminal 'mouse-tracking) 1002)
       (check 'mouse-encoding (state-ref terminal 'sgr-mouse) #t))

     (let ([terminal (make-terminal-emulator 4 5)])
       (terminal-emulator-feed!
         terminal "\x1b;[2;3r\x1b;[?6;7h\x1b;7\x1b;[?6;7l\x1b;8")
       (check 'restore-origin-mode (state-ref terminal 'origin) #t)
       (check 'restore-autowrap-mode (state-ref terminal 'autowrap) #t)
       (check 'restore-cursor (state-ref terminal 'cursor) '(1 . 0)))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[123\x18;A")
       (check 'cancel-csi
              (vector-ref (terminal-emulator-screen terminal) 0)
              "A    ")
       (terminal-emulator-feed! terminal "\x1b;[6n")
       (check 'cursor-report
              (terminal-emulator-replies terminal)
              '("\x1b;[1;2R")))

     (format #t "~a terminal checks passed\n" checks)))
