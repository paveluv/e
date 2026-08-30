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

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1b;)0\x0e;q\x0f;q")
       (check 'g1-shift-in-out
              (vector->list (terminal-emulator-screen terminal))
              '("\x2500;q  ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1b;(0lqk")
       (check 'vt100-line-drawing
              (vector->list (terminal-emulator-screen terminal))
              '("\x250c;\x2500;\x2510; ")))

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

     (let ([terminal (make-terminal-emulator 1 2)])
       (terminal-emulator-feed! terminal "A\x1b;[7mB")
       (let ([normal (terminal-emulator-styles terminal)])
         (terminal-emulator-feed! terminal "\x1b;[?5h")
         (check 'reverse-screen-mode (state-ref terminal 'reverse-screen) #t)
         (let ([reversed (terminal-emulator-styles terminal)])
           (check 'reverse-screen-inverts-plain
                  (eq? (vector-ref (vector-ref normal 0) 0)
                       (vector-ref (vector-ref reversed 0) 0))
                  #f)
           (check 'reverse-screen-inverts-reversed
                  (eq? (vector-ref (vector-ref normal 0) 1)
                       (vector-ref (vector-ref reversed 0) 1))
                  #f))
         (terminal-emulator-feed! terminal "\x1b;[?5l")
         (check 'reverse-screen-restores-styles
                (terminal-emulator-styles terminal) normal)))

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

     (let ([terminal (make-terminal-emulator 3 5)])
       (terminal-emulator-feed! terminal "abc\x1b;[2G\x1b;[2;3r\x1b;[?1049h")
       (check 'alternate-screen-cleared
              (vector->list (terminal-emulator-screen terminal))
              '("     " "     " "     "))
       (terminal-emulator-feed! terminal "X\x1b;[?1049l")
       (check 'primary-screen-restored
              (vector->list (terminal-emulator-screen terminal))
              '("abc  " "     " "     "))
       (check 'primary-cursor-restored (state-ref terminal 'cursor) '(0 . 0))
       (check 'primary-margins-restored
              (state-ref terminal 'scroll-region) '(1 . 2)))

     (let ([terminal (make-terminal-emulator 2 3)])
       (terminal-emulator-feed! terminal "\x1b;[?47hq\x1b;[?47lX\x1b;[?47h")
       (check 'alternate-screen-persists
              (vector->list (terminal-emulator-screen terminal))
              '("q  " "   "))
       (terminal-emulator-feed! terminal "\x1b;[?47l\x1b;[?1047hq\x1b;[?1047l\x1b;[?1047h")
       (check 'alternate-screen-1047-clears
              (vector->list (terminal-emulator-screen terminal))
              '("   " "   ")))

     (let ([terminal (make-terminal-emulator 3 4)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;1HAAAA\x1b;[2;1HBBBB\x1b;[3;1HCCCC\x1b;[2;3r\x1b;[2;1H\x1b;[L")
       (check 'insert-line-in-region
              (vector->list (terminal-emulator-screen terminal))
              '("AAAA" "    " "BBBB")))

     (let ([terminal (make-terminal-emulator 3 4)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;1HAAAA\x1b;[2;1HBBBB\x1b;[3;1HCCCC\x1b;[2;3r\x1b;[2;1H\x1b;[M")
       (check 'delete-line-in-region
              (vector->list (terminal-emulator-screen terminal))
              '("AAAA" "CCCC" "    ")))

     (let ([terminal (make-terminal-emulator 3 4)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;1HAAAA\x1b;[2;1HBBBB\x1b;[3;1HCCCC\x1b;[2;3r\x1b;[1;1H\x1b;[L")
       (check 'insert-line-outside-region
              (vector->list (terminal-emulator-screen terminal))
              '("AAAA" "BBBB" "CCCC")))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[123\x18;A")
       (check 'cancel-csi
              (vector-ref (terminal-emulator-screen terminal) 0)
              "A    ")
       (terminal-emulator-feed! terminal "\x1b;[6n")
       (check 'cursor-report
              (terminal-emulator-replies terminal)
              '("\x1b;[1;2R")))

     (let ([terminal (make-terminal-emulator 5 8)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[5n\x1b;[?5n\x1b;[3;5r\x1b;[?6h\x1b;[2;4H\x1b;[?6n\x1b;[c\x1b;[>c\x1b;Z")
       (check 'device-and-status-reports
              (terminal-emulator-replies terminal)
              '("\x1b;[0n" "\x1b;[?0n" "\x1b;[?2;4R" "\x1b;[?1;2c"
                "\x1b;[>0;276;0c" "\x1b;[?1;2c")))

     (let ([terminal (make-terminal-emulator 3 5)])
       (terminal-emulator-feed!
         terminal "a\x85;\x9b;2CX\x9d;2;c1-title\x9c;Y")
       (check 'c1-controls
              (vector->list (terminal-emulator-screen terminal))
              '("a    " "  XY " "     ")))

     (let ([terminal (make-terminal-emulator 1 8)])
       (terminal-emulator-feed!
         terminal "a\x90;hidden\x9c;b\x9e;hidden\x9c;c\x81;d")
       (check 'c1-controls-never-print
              (vector->list (terminal-emulator-screen terminal))
              '("abcd    ")))

     (let ([terminal (make-terminal-emulator 2 8)])
       (check 'control-shift-navigation
              (terminal-emulator-input terminal "C-S-LEFT")
              (string->utf8 "\x1b;[1;6D"))
       (check 'modified-page-key
              (terminal-emulator-input terminal "M-PAGEDOWN")
              (string->utf8 "\x1b;[6;3~"))
       (check 'insert-key
              (terminal-emulator-input terminal "INSERT")
              (string->utf8 "\x1b;[2~"))
       (check 'extended-function-key
              (terminal-emulator-input terminal "F37")
              (string->utf8 "\x1b;[1;6P"))
       (check 'named-modified-function-key
              (terminal-emulator-input terminal "S-F12")
              (string->utf8 "\x1b;[24;2~"))
       (check 'keypad-numeric
              (terminal-emulator-input terminal "KP-7")
              (string->utf8 "7"))
       (terminal-emulator-feed! terminal "\x1b;=")
       (check 'keypad-application
              (terminal-emulator-input terminal "KP-7")
              (string->utf8 "\x1b;Ow")))

     (let ([terminal (make-terminal-emulator 2 6)])
       (terminal-emulator-feed! terminal "e\x301;\x4e2d;X")
       (check 'unicode-cell-geometry
              (vector->list (terminal-emulator-screen terminal))
              '("\xe9;\x4e2d;X  " "      "))
       (check 'unicode-cell-cursor (state-ref terminal 'cursor) '(0 . 4)))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1f469;\x200d;\x1f4bb;X")
       (check 'emoji-grapheme-cluster
              (vector->list (terminal-emulator-screen terminal))
              '("\x1f469;\x200d;\x1f4bb;X "))
       (check 'emoji-cell-cursor (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1f1fa;\x1f1f8;X")
       (check 'regional-indicator-grapheme
              (vector->list (terminal-emulator-screen terminal))
              '("\x1f1fa;\x1f1f8;X "))
       (check 'regional-indicator-width (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1100;\x1161;\x11a8;X")
       (check 'hangul-grapheme-cluster
              (vector->list (terminal-emulator-screen terminal))
              '("\xac01;X "))
       (check 'hangul-cell-width (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x4e2d;X\x1b;[2G\x1b;[K")
       (check 'erase-wide-cell-atomically
              (vector->list (terminal-emulator-screen terminal))
              '("    ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x4e2d;X\x1b;[1G\x1b;[P")
       (check 'delete-wide-cell-boundary
              (vector->list (terminal-emulator-screen terminal))
              '(" X  ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x7;")
       (check 'terminal-bell-is-pending
              (state-ref terminal 'bell-pending) #t)
       (check 'terminal-bell-does-not-print
              (vector->list (terminal-emulator-screen terminal))
              '("    ")))

     (let ([terminal (make-terminal-emulator 3 5)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;1H11111\x1b;[2;1H22222\x1b;[3;1H33333\x1b;[?69h\x1b;[2;4s\x1b;[S")
       (check 'horizontal-margins-enabled
              (state-ref terminal 'horizontal-margins) '(1 . 3))
       (check 'horizontal-margin-scroll
              (vector->list (terminal-emulator-screen terminal))
              '("12221" "23332" "3   3"))
       (terminal-emulator-feed! terminal "\x1b;[?69l")
       (check 'horizontal-margins-disabled
              (state-ref terminal 'horizontal-margins) '(0 . 4)))

     (let ([terminal (make-terminal-emulator 2 6)])
       (terminal-emulator-feed!
         terminal "\x1b;[?69h\x1b;[2;4s\x1b;[?6h\x1b;[1;1HABCD")
       (check 'horizontal-margin-autowrap
              (vector->list (terminal-emulator-screen terminal))
              '(" ABC  " " D    ")))

     (let ([terminal (make-terminal-emulator 2 6)])
       (terminal-emulator-feed! terminal "abcdefg")
       (terminal-emulator-resize! terminal 2 4)
       (check 'narrow-resize-reflows
              (vector->list (terminal-emulator-screen terminal))
              '("abcd" "efg "))
       (check 'narrow-resize-cursor (state-ref terminal 'cursor) '(1 . 3))
       (terminal-emulator-resize! terminal 2 6)
       (check 'wide-resize-reflows
              (vector->list (terminal-emulator-screen terminal))
              '("abcdef" "g     "))
       (check 'wide-resize-cursor (state-ref terminal 'cursor) '(1 . 1)))

     (let ([terminal (make-terminal-emulator 2 4)])
       (terminal-emulator-feed! terminal "AB\x4e2d;")
       (terminal-emulator-resize! terminal 2 3)
       (check 'wide-cell-never-split
              (vector->list (terminal-emulator-screen terminal))
              '("AB " "\x4e2d; "))
       (terminal-emulator-resize! terminal 2 4)
       (check 'wide-cell-reflow-restores
              (vector->list (terminal-emulator-screen terminal))
              '("AB\x4e2d;" "    ")))

     (let ([terminal (make-terminal-emulator 2 6)])
       (terminal-emulator-feed! terminal "abcdefg\x1b;[?1049hALT")
       (terminal-emulator-resize! terminal 2 4)
       (terminal-emulator-feed! terminal "\x1b;[?1049l")
       (check 'primary-reflows-behind-alternate-screen
              (vector->list (terminal-emulator-screen terminal))
              '("abcd" "efg ")))

     (let ([terminal (make-terminal-emulator 2 4)])
       (terminal-emulator-feed! terminal "abcdefghij")
       (check 'scrollback-before-reflow
              (state-ref terminal 'scrollback-lines) 1)
       (terminal-emulator-resize! terminal 2 5)
       (check 'scrollback-participates-in-reflow
              (vector->list (terminal-emulator-screen terminal))
              '("abcde" "fghij"))
       (check 'reflow-can-consume-scrollback
              (state-ref terminal 'scrollback-lines) 0))

     (let ([terminal (make-terminal-emulator 2 4)])
       (terminal-emulator-feed! terminal "abcd\r\nX")
       (terminal-emulator-resize! terminal 3 3)
       (check 'explicit-newline-survives-reflow
              (vector->list (terminal-emulator-screen terminal))
              '("abc" "d  " "X  ")))

     (format #t "~a terminal checks passed\n" checks)))
