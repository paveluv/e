#!/usr/bin/env scheme-script

(import (chezscheme))

(include "tests/roots.ss")
(test-roots! 'base)

(eval
  '(begin
     (import (prefix (terminal) terminal:) (prefix (vt) vt:) (prefix (test) test:)
             (prefix (head) head:) (prefix (paint) paint:))

     (define check test:check)

     (define (state-ref emulator key)
       (cdr (assq key (vt:emulator-state emulator))))

     (define (style-at emulator row column)
       (vector-ref (vector-ref (vt:emulator-styles emulator) row)
                   column))

     (let ([terminal (vt:make-emulator 4 5)])
       (do ([row 1 (+ row 1)]) ((> row 4))
         (do ([column 1 (+ column 1)]) ((> column 5))
           (vt:emulator-feed!
             terminal
             (format "\x1b;[4;5H\x1b;7\x1b;[~a;~aHA\x1b;8" row column))))
       (vt:emulator-resize! terminal 5 5)
       (check 'dec-save-restore-repeated-positioning
              (vector->list (vt:emulator-screen terminal))
              '("AAAAA" "AAAAA" "AAAAA" "AAAAA" "     ")))

     (let ([terminal (vt:make-emulator 21 79)])
       (do ([row 1 (+ row 1)]) ((> row 4))
         (do ([column 1 (+ column 1)]) ((> column 5))
           (vt:emulator-feed!
             terminal
             (format "\x1b;[~a;~aH" (+ 8 (* 2 (- row 1)))
                     (+ 12 (* 12 (- column 1)))))
           (vt:emulator-feed!
             terminal (if (even? row) "\x1b;(0" "\x1b;(B"))
           (vt:emulator-feed! terminal "*****\x1b;7")
           (vt:emulator-feed!
             terminal (format "\x1b;[~a;~aH" row column))
           (vt:emulator-feed! terminal "\x1b;[0m\x1b;(BA\x1b;8*****")))
       (vt:emulator-resize! terminal 22 79)
       (check 'dec-save-restore-vttest-pattern-after-resize
              (map (lambda (line) (substring line 0 5))
                   (vector->list (vt:emulator-screen terminal)))
              (append '("AAAAA" "AAAAA" "AAAAA" "AAAAA")
                      (make-list 18 "     "))))

     (let ([terminal (vt:make-emulator 22 79)])
       (vt:emulator-feed!
         terminal "\x1b;[2J\x1b;[?6h\x1b;[1;22r\x1b;[2J\x1b;[22B")
       (do ([line 1 (+ line 1)]) ((> line 27))
         (vt:emulator-feed!
           terminal
           (format "Soft scroll up region [1..22] size 22 Line ~a\r\n" line)))
       (vt:emulator-feed! terminal "\x1b;[22A")
       (do ([line 1 (+ line 1)]) ((> line 27))
         (vt:emulator-feed!
           terminal
           (format "Soft scroll down region [1..22] size 22 Line ~a\r\n\x1b;M\x1b;M"
                   line)))
       (vt:emulator-feed! terminal "Push <RETURN>")
       (check 'full-screen-reverse-index-scroll
              (vector->list (vt:emulator-screen terminal))
              (cons (string-append "Push <RETURN>" (make-string 66 #\space))
                    (map
                      (lambda (line)
                        (let ([text
                               (format
                                 "Soft scroll down region [1..22] size 22 Line ~a"
                                 line)])
                          (string-append
                            text
                            (make-string (- 79 (string-length text)) #\space))))
                      (reverse (map (lambda (n) (+ n 7)) (iota 21)))))))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "abcd")
       (check 'delayed-wrap-screen
              (vector->list (vt:emulator-screen terminal))
              '("abcd" "    "))
       (check 'delayed-wrap-cursor (state-ref terminal 'cursor) '(0 . 3))
       (check 'delayed-wrap-pending (state-ref terminal 'wrap-pending) #t)
       (vt:emulator-feed! terminal "e")
       (check 'delayed-wrap-next-character
              (vector->list (vt:emulator-screen terminal))
              '("abcd" "e   ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1b;[?7labcde")
       (check 'autowrap-disabled
              (vector->list (vt:emulator-screen terminal))
              '("abce")))

     (let ([terminal (vt:make-emulator 5 5)])
       (vt:emulator-feed! terminal "\x1b;[2;4r\x1b;[?6h\x1b;[2;3HX")
       (check 'origin-relative-cursor (state-ref terminal 'cursor) '(2 . 3))
       (check 'origin-relative-screen
              (vector-ref (vt:emulator-screen terminal) 2)
              "  X  "))

     (let ([terminal (vt:make-emulator 5 8)])
       (vt:emulator-feed!
         terminal "\x1b;[3;4H\x1b;[0A\x1b;[2C\x1b;[B\x1b;[D")
       (check 'relative-cursor-defaults
              (state-ref terminal 'cursor) '(2 . 4))
       (vt:emulator-feed! terminal "\x1b;[2E\x1b;[F")
       (check 'cursor-next-previous-line
              (state-ref terminal 'cursor) '(3 . 0))
       (vt:emulator-feed! terminal "\x1b;[7G\x1b;[2d")
       (check 'absolute-row-column
              (state-ref terminal 'cursor) '(1 . 6)))

     (let ([terminal (vt:make-emulator 3 5)])
       (vt:emulator-feed!
         terminal "11111\x1b;[2;1H22222\x1b;[3;1H33333\x1b;[2;3H\x1b;[J")
       (check 'erase-display-after-cursor
              (vector->list (vt:emulator-screen terminal))
              '("11111" "22   " "     "))
       (vt:emulator-feed!
         terminal "\x1b;[1;1H11111\x1b;[2;1H22222\x1b;[3;1H33333\x1b;[2;3H\x1b;[1J")
       (check 'erase-display-before-cursor
              (vector->list (vt:emulator-screen terminal))
              '("     " "   22" "33333"))
       (vt:emulator-feed! terminal "\x1b;[2J")
       (check 'erase-display-all
              (vector->list (vt:emulator-screen terminal))
              '("     " "     " "     ")))

     (let ([terminal (vt:make-emulator 1 7)])
       (vt:emulator-feed! terminal "abcdef\x1b;[3G\x1b;[2P")
       (check 'delete-characters
              (vector->list (vt:emulator-screen terminal))
              '("abef   "))
       (vt:emulator-feed! terminal "\x1b;[3G\x1b;[2@")
       (check 'insert-characters
              (vector->list (vt:emulator-screen terminal))
              '("ab  ef "))
       (vt:emulator-feed! terminal "\x1b;[3G\x1b;[3X")
       (check 'erase-characters
              (vector->list (vt:emulator-screen terminal))
              '("ab   f ")))

     (let ([terminal (vt:make-emulator 6 10)])
       (do ([row 1 (+ row 1)]) ((> row 6))
         (vt:emulator-feed!
           terminal
           (format "\x1b;[~a;1H~a\x1b;[~a;~aH\x1b;[~aP"
                   row (make-string 10 (integer->char (+ 64 row)))
                   row (- 10 row) row)))
       (check 'delete-characters-staggered-right-edge
              (vector->list (vt:emulator-screen terminal))
              '("AAAAAAAAA " "BBBBBBBB  " "CCCCCCC   " "DDDDDD    "
                "EEEEE     " "FFFF      ")))

     (let ([terminal (vt:make-emulator 3 5)])
       (vt:emulator-feed!
         terminal
         "ABCDE\x1b;[2;1HFGHIJ\x1b;[3;1HKLMNO\x1b;[2 @")
       (check 'scroll-left
              (vector->list (vt:emulator-screen terminal))
              '("CDE  " "HIJ  " "MNO  "))
       (vt:emulator-feed! terminal "\x1b;[ A")
       (check 'scroll-right
              (vector->list (vt:emulator-screen terminal))
              '(" CDE " " HIJ " " MNO ")))

     (let ([terminal (vt:make-emulator 1 20)])
       (vt:emulator-feed! terminal "\t\t\x1b;[Z")
       (check 'back-tab (state-ref terminal 'cursor) '(0 . 8))
       (vt:emulator-feed! terminal "\x1b;[g\x1b;[Z")
       (check 'clear-current-tab-stop
              (state-ref terminal 'cursor) '(0 . 0))
       (vt:emulator-feed! terminal "\x1b;H\x1b;[3g\t")
       (check 'clear-all-tab-stops
              (state-ref terminal 'cursor) '(0 . 19)))

     (let ([terminal (vt:make-emulator 1 32)])
       (vt:emulator-feed! terminal "\x1b;[I")
       (check 'cursor-horizontal-tab-default
              (state-ref terminal 'cursor) '(0 . 8))
       (vt:emulator-feed! terminal "\x1b;[2I")
       (check 'cursor-horizontal-tab-parameter
              (state-ref terminal 'cursor) '(0 . 24)))

     (let ([terminal (vt:make-emulator 2 8)])
       (vt:emulator-feed! terminal "\t*\t*")
       (check 'horizontal-tab-preserves-delayed-wrap
              (vector->list (vt:emulator-screen terminal))
              '("       *" "*       "))
       (vt:emulator-feed! terminal "\x1b;c\x1b;[I*\x1b;[I*")
       (check 'cursor-horizontal-tab-preserves-delayed-wrap
              (vector->list (vt:emulator-screen terminal))
              '("       *" "*       ")))

     (let ([terminal (vt:make-emulator 1 5)])
       (vt:emulator-feed! terminal "abc\x1b;[2G\x1b;[4hX")
       (check 'insert-mode
              (vector->list (vt:emulator-screen terminal))
              '("aXbc ")))

     (let ([terminal (vt:make-emulator 1 10)])
       (vt:emulator-feed! terminal "a\tb")
       (check 'default-tabs
              (vector->list (vt:emulator-screen terminal))
              '("a       b ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1b;)0\x0e;q\x0f;q")
       (check 'g1-shift-in-out
              (vector->list (vt:emulator-screen terminal))
              '("\x2500;q  ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1b;(0lqk")
       (check 'vt100-line-drawing
              (vector->list (vt:emulator-screen terminal))
              '("\x250c;\x2500;\x2510; ")))

     (let ([terminal (vt:make-emulator 1 8)])
       (vt:emulator-feed! terminal "\x1b;(0bcdehi")
       (check 'vt100-graphics-control-pictures
              (vector-ref (vt:emulator-screen terminal) 0)
              "\x2409;\x240c;\x240d;\x240a;\x2424;\x240b;  "))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1b;(A#\x1b;(B#")
       (check 'vt100-british-character-set
              (vector->list (vt:emulator-screen terminal))
              '("\xa3;#  ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1b;)A\x0e;#\x0f;#")
       (check 'vt100-british-g1-shift
              (vector->list (vt:emulator-screen terminal))
              '("\xa3;#  ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1b;(1q\x1b;(2q")
       (check 'vt100-alternate-rom-aliases
              (vector->list (vt:emulator-screen terminal))
              '("q\x2500;  ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed!
         terminal "\x1b;(B\x1b;)B\x1b;*B\x1b;+B\x1b;-%5\x1b;.&4\x1b;/>Z")
       (check 'iso-2022-g2-g3-and-multibyte-designations
              (vector->list (vt:emulator-screen terminal))
              '("Z   ")))

     (let ([terminal (vt:make-emulator 26 79)])
       (vt:emulator-feed!
         terminal
         (string-append
           "\x1b;[22;48H\x1b;)2\x1b;(B\x0e;"
           (list->string
             (map (lambda (number) (integer->char (+ 96 number)))
                  (iota 32)))
           "\x1b;(B\x1b;)B\x0f;\x1b;[26;1H"))
       (check 'vt100-charset-reset-after-right-margin
              (substring
                (vector-ref (vt:emulator-screen terminal) 21) 0 9)
              "         "))

     (let ([terminal (vt:make-emulator 3 4)])
       (vt:emulator-feed! terminal "abc\x1b;#8")
       (check 'screen-alignment-pattern
              (vector->list (vt:emulator-screen terminal))
              '("EEEE" "EEEE" "EEEE"))
       (check 'screen-alignment-cursor
              (state-ref terminal 'cursor) '(0 . 0)))

     (let ([terminal (vt:make-emulator 3 5)])
       (vt:emulator-feed!
         terminal "abc\x1b;[2;3r\x1b;[3;4H\x1b;[?3l")
       (check 'column-mode-clears-screen
              (vector->list (vt:emulator-screen terminal))
              '("     " "     " "     "))
       (check 'column-mode-homes-cursor
              (state-ref terminal 'cursor) '(0 . 0))
       (check 'column-mode-resets-margins
              (state-ref terminal 'scroll-region) '(0 . 2)))

     (let ([terminal (vt:make-emulator 1 5)])
       (vt:emulator-feed! terminal "z\x1b;[3b\x1b;[10b")
       (check 'repeat-character
              (vector->list (vt:emulator-screen terminal))
              '("zzzz "))
       (check 'repeat-after-control-is-ignored
              (state-ref terminal 'cursor) '(0 . 4)))

     (let ([terminal (vt:make-emulator 1 2)])
       (vt:emulator-feed! terminal "\x1b;[1mX")
       (check 'sgr-cell-style
              (eq? (vector-ref (vector-ref
                                 (vt:emulator-styles terminal) 0) 0)
                   'plain)
              #f))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;3;4;5;7;8;9;38;5;123;48;2;1;2;3mA\x1b;[22;23;24;25;27;28;29;39;49mB")
       (check 'combined-sgr-is-styled
              (eq? (style-at terminal 0 0) 'plain) #f)
       (check 'selective-sgr-resets-to-plain
              (style-at terminal 0 1) 'plain))

     (let ([semicolon (vt:make-emulator 1 2)]
           [colon (vt:make-emulator 1 2)])
       (vt:emulator-feed! semicolon "\x1b;[38;2;10;20;30mX")
       (vt:emulator-feed! colon "\x1b;[38:2::10:20:30mX")
       (check 'colon-rgb-matches-semicolon-rgb
              (style-at colon 0 0) (style-at semicolon 0 0)))

     (let ([semicolon (vt:make-emulator 1 4)]
           [colon (vt:make-emulator 1 4)]
           [plain (vt:make-emulator 1 4)])
       ;; Underline colors group like the other extended colors instead of
       ;; leaking their components as blink and unrelated codes.
       (vt:emulator-feed! semicolon "\x1b;[4m\x1b;[58;5;196mX")
       (vt:emulator-feed! colon "\x1b;[4m\x1b;[58:5:196mX")
       (vt:emulator-feed! plain "\x1b;[4m\x1b;[5mX")
       (check 'underline-color-groups-as-one-operation
              (equal? (style-at semicolon 0 0) (style-at colon 0 0)) #t)
       (check 'underline-color-is-not-blink
              (equal? (style-at semicolon 0 0) (style-at plain 0 0)) #f)
       (vt:emulator-feed! semicolon "\x1b;[59mY")
       (vt:emulator-feed! plain "\x1b;[2G\x1b;[0m\x1b;[4mY")
       (check 'underline-color-reset-keeps-underline
              (equal? (style-at semicolon 0 1)
                (style-at plain 0 1))
              #t))

     (let ([truncated (vt:make-emulator 1 4)]
           [bold (vt:make-emulator 1 4)])
       (vt:emulator-feed! truncated
                          "\x1b;[1m\x1b;[38:5m\x1b;[38:2m\x1b;[48:2:1mX")
       (vt:emulator-feed! bold "\x1b;[1m\x1b;[38;5;0mX")
       (check 'truncated-colon-color-keeps-rendition
              (style-at truncated 0 0) (style-at bold 0 0)))

     (let ([terminal (vt:make-emulator 1 2)])
       (vt:emulator-feed! terminal "A\x1b;[7mB")
       (let ([normal (vt:emulator-styles terminal)])
         (vt:emulator-feed! terminal "\x1b;[?5h")
         (check 'reverse-screen-mode (state-ref terminal 'reverse-screen) #t)
         (let ([reversed (vt:emulator-styles terminal)])
           (check 'reverse-screen-inverts-plain
                  (eq? (vector-ref (vector-ref normal 0) 0)
                       (vector-ref (vector-ref reversed 0) 0))
                  #f)
           (check 'reverse-screen-inverts-reversed
                  (eq? (vector-ref (vector-ref normal 0) 1)
                       (vector-ref (vector-ref reversed 0) 1))
                  #f))
         (vt:emulator-feed! terminal "\x1b;[?5l")
         (check 'reverse-screen-restores-styles
                (vt:emulator-styles terminal) normal)))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[?25l\x1b;[?1h\x1b;[?1002;1006h")
       (check 'cursor-hidden (state-ref terminal 'cursor-visible) #f)
       (check 'application-cursor-keys
              (vt:emulator-input terminal "UP")
              (string->utf8 "\x1b;OA"))
       (check 'mouse-tracking (state-ref terminal 'mouse-tracking) 1002)
       (check 'mouse-encoding (state-ref terminal 'sgr-mouse) #t))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[?1004;1005;1015h")
       (check 'focus-reporting-mode
              (state-ref terminal 'focus-reporting) #t)
       (check 'focus-in-encoding
              (vt:emulator-input terminal "FOCUS")
              (string->utf8 "\x1b;[I"))
       (check 'focus-out-encoding
              (vt:emulator-input terminal "BLUR")
              (string->utf8 "\x1b;[O"))
       (check 'utf8-mouse-mode (state-ref terminal 'utf8-mouse) #t)
       (check 'urxvt-mouse-mode (state-ref terminal 'urxvt-mouse) #t)
       (vt:emulator-feed! terminal "\x1b;[?1004;1005;1015l")
       (check 'focus-reporting-disabled
              (vt:emulator-input terminal "FOCUS") #f))

     (let ([terminal (vt:make-emulator 2 5)])
       (check 'mouse-input-disabled
              (vt:emulator-mouse-input terminal 0 4 5 #f) #f)
       (vt:emulator-feed! terminal "\x1b;[?9h")
       (check 'x10-legacy-mode (state-ref terminal 'mouse-tracking) 9)
       (check 'x10-legacy-press
              (vt:emulator-mouse-input terminal 0 4 5 #f)
              (bytevector 27 91 77 32 36 37))
       (check 'x10-legacy-release-suppressed
              (vt:emulator-mouse-input terminal 0 4 5 #t) #f)
       (check 'x10-legacy-wheel
              (vt:emulator-mouse-input terminal 64 4 5 #f)
              (bytevector 27 91 77 96 36 37))
       (vt:emulator-feed! terminal "\x1b;[?9l")
       (check 'x10-legacy-mode-disabled
              (state-ref terminal 'mouse-tracking) #f)
       (vt:emulator-feed! terminal "\x1b;[?1000h")
       (check 'x10-mouse-press
              (vt:emulator-mouse-input terminal 0 4 5 #f)
              (bytevector 27 91 77 32 36 37))
       (check 'x10-mouse-release
              (vt:emulator-mouse-input terminal 0 4 5 #t)
              (bytevector 27 91 77 35 36 37))
       (check 'x10-mouse-coordinate-limit
              (vt:emulator-mouse-input terminal 64 300 400 #f)
              (bytevector 27 91 77 96 255 255)))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[?1002;1006h")
       (check 'sgr-mouse-motion
              (vt:emulator-mouse-input terminal 32 4 5 #f)
              (string->utf8 "\x1b;[<32;4;5M"))
       (check 'sgr-mouse-release
              (vt:emulator-mouse-input terminal 0 4 5 #t)
              (string->utf8 "\x1b;[<0;4;5m")))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[?1000;1015h")
       (check 'urxvt-mouse-press
              (vt:emulator-mouse-input terminal 4 4 5 #f)
              (string->utf8 "\x1b;[36;4;5M"))
       (check 'urxvt-mouse-release
              (vt:emulator-mouse-input terminal 4 4 5 #t)
              (string->utf8 "\x1b;[35;4;5M")))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[?1000;1005h")
       (check 'utf8-mouse-wheel
              (vt:emulator-mouse-input terminal 64 4 5 #f)
              (string->utf8 "\x1b;[M`$%")))

     (let ([terminal (vt:make-emulator 1 2)])
       (check 'meta-defaults-to-escape
              (vt:emulator-input terminal "M-x")
              (bytevector 27 120))
       (vt:emulator-feed! terminal "\x1b;[?1034h")
       (check 'eight-bit-meta-mode (state-ref terminal 'eight-bit-meta) #t)
       (check 'eight-bit-meta-character
              (vt:emulator-input terminal "M-x")
              (bytevector 248))
       (check 'eight-bit-control-meta-character
              (vt:emulator-input terminal "C-M-a")
              (bytevector 129))
       (vt:emulator-feed! terminal "\x1b;[?1034l")
       (check 'eight-bit-meta-disabled
              (vt:emulator-input terminal "M-x")
              (bytevector 27 120)))

     (let ([terminal (vt:make-emulator 1 2)])
       (check 'meta-space
              (vt:emulator-input terminal "M-SPC")
              (bytevector 27 32)))

     (let ([terminal (vt:make-emulator 1 2)])
       (check 'meta-return
              (vt:emulator-input terminal "M-RET")
              (bytevector 27 13))
       (check 'meta-backspace
              (vt:emulator-input terminal "M-BACKSPACE")
              (bytevector 27 127))
       (check 'meta-shift-tab
              (vt:emulator-input terminal "M-S-TAB")
              (string->utf8 "\x1b;\x1b;[Z")))

     (let ([terminal (vt:make-emulator 4 5)])
       (vt:emulator-feed!
         terminal "\x1b;[2;3r\x1b;[?6;7h\x1b;7\x1b;[?6;7l\x1b;8")
       (check 'restore-origin-mode (state-ref terminal 'origin) #t)
       (check 'restore-autowrap-mode (state-ref terminal 'autowrap) #t)
       (check 'restore-cursor (state-ref terminal 'cursor) '(1 . 0)))

     (let ([terminal (vt:make-emulator 3 5)])
       (vt:emulator-feed! terminal "abc\x1b;[2G\x1b;[2;3r\x1b;[?1049h")
       (check 'alternate-screen-cleared
              (vector->list (vt:emulator-screen terminal))
              '("     " "     " "     "))
       (vt:emulator-feed! terminal "X\x1b;[?1049l")
       (check 'primary-screen-restored
              (vector->list (vt:emulator-screen terminal))
              '("abc  " "     " "     "))
       (check 'primary-cursor-restored (state-ref terminal 'cursor) '(0 . 0))
       (check 'primary-margins-restored
              (state-ref terminal 'scroll-region) '(1 . 2)))

     (let ([terminal (vt:make-emulator 2 3)])
       (vt:emulator-feed! terminal "\x1b;[?47hq\x1b;[?47lX\x1b;[?47h")
       (check 'alternate-screen-persists
              (vector->list (vt:emulator-screen terminal))
              '("q  " "   "))
       (vt:emulator-feed! terminal "\x1b;[?47l\x1b;[?1047hq\x1b;[?1047l\x1b;[?1047h")
       (check 'alternate-screen-1047-clears
              (vector->list (vt:emulator-screen terminal))
              '("   " "   ")))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "ab\x1b;[?47h")
       (check 'legacy-alternate-screen-keeps-cursor
              (state-ref terminal 'cursor) '(0 . 2))
       (vt:emulator-feed! terminal "C\x1b;[?47l")
       (check 'legacy-alternate-screen-cursor-carries-back
              (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (vt:make-emulator 3 4)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;1HAAAA\x1b;[2;1HBBBB\x1b;[3;1HCCCC\x1b;[2;3r\x1b;[2;1H\x1b;[L")
       (check 'insert-line-in-region
              (vector->list (vt:emulator-screen terminal))
              '("AAAA" "    " "BBBB")))

     (let ([terminal (vt:make-emulator 3 4)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;1HAAAA\x1b;[2;1HBBBB\x1b;[3;1HCCCC\x1b;[2;3r\x1b;[2;1H\x1b;[M")
       (check 'delete-line-in-region
              (vector->list (vt:emulator-screen terminal))
              '("AAAA" "CCCC" "    ")))

     (let ([terminal (vt:make-emulator 3 4)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;1HAAAA\x1b;[2;1HBBBB\x1b;[3;1HCCCC\x1b;[2;3r\x1b;[1;1H\x1b;[L")
       (check 'insert-line-outside-region
              (vector->list (vt:emulator-screen terminal))
              '("AAAA" "BBBB" "CCCC")))

     (let ([terminal (vt:make-emulator 4 3)])
       (vt:emulator-feed!
         terminal
         "111\x1b;[2;1H222\x1b;[3;1H333\x1b;[4;1H444\x1b;[2;3r\x1b;[3;1H\x1b;D")
       (check 'index-scrolls-active-region
              (vector->list (vt:emulator-screen terminal))
              '("111" "333" "   " "444"))
       (vt:emulator-feed! terminal "\x1b;[2;1H\x1b;M")
       (check 'reverse-index-scrolls-active-region
              (vector->list (vt:emulator-screen terminal))
              '("111" "   " "333" "444")))

     (let ([terminal (vt:make-emulator 4 3)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;1H111\x1b;[2;1H222\x1b;[3;1H333\x1b;[4;1H444\x1b;[2;1H\x1b;l\x1b;[4;1H\n")
       (check 'memory-lock-state (state-ref terminal 'memory-lock) 1)
       (check 'memory-lock-preserves-upper-rows
              (vector->list (vt:emulator-screen terminal))
              '("111" "333" "444" "   "))
       (vt:emulator-feed! terminal "\x1b;m\x1b;[4;1H\n")
       (check 'memory-unlock-state (state-ref terminal 'memory-lock) #f)
       (check 'memory-unlock-restores-full-scroll
              (vector->list (vt:emulator-screen terminal))
              '("333" "444" "   " "   ")))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "AB\x1b;[i")
       (check 'printer-screen-copy
              (state-ref terminal 'printer-output)
              "AB  \r\n    \r\n")
       (vt:emulator-feed! terminal "\x1b;[5iprinted\x1b;[4iX")
       (check 'printer-controller-stops-echo
              (vector->list (vt:emulator-screen terminal))
              '("ABX " "    "))
       (check 'printer-controller-output
              (state-ref terminal 'printer-output)
              "AB  \r\n    \r\nprinted")
       (check 'printer-controller-disabled
              (state-ref terminal 'printer-controller) #f)
       (vt:emulator-feed! terminal "\x1b;[5ic1\x9b;4iY")
       (check 'printer-controller-c1-termination
              (state-ref terminal 'printer-output)
              "AB  \r\n    \r\nprintedc1")
       (check 'printer-controller-c1-resumes-display
              (vector->list (vt:emulator-screen terminal))
              '("ABXY" "    "))
       (vt:emulator-feed! terminal "\x1b;c")
       (check 'full-reset-clears-printer-output
              (state-ref terminal 'printer-output) ""))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[123\x18;A")
       (check 'cancel-csi
              (vector-ref (vt:emulator-screen terminal) 0)
              "A    ")
       (vt:emulator-feed! terminal "\x1b;[6n")
       (check 'cursor-report
              (vt:emulator-replies terminal)
              '("\x1b;[1;2R")))

     (let ([terminal (vt:make-emulator 6 20)])
       (vt:emulator-feed!
         terminal
         "A B C D E F G H I\r\nA\x1b;[2\x8;CB\x1b;[2\x8;CC\x1b;[2\x8;CD\x1b;[2\x8;CE\x1b;[2\x8;CF\x1b;[2\x8;CG\x1b;[2\x8;CH\x1b;[2\x8;CI\x1b;[2\x8;C\r\nA \x1b;[\r2CB\x1b;[\r4CC\x1b;[\r6CD\x1b;[\r8CE\x1b;[\r10CF\x1b;[\r12CG\x1b;[\r14CH\x1b;[\r16CI\r\nA \x1b;[1\vAB \x1b;[1\vAC \x1b;[1\vAD \x1b;[1\vAE \x1b;[1\vAF \x1b;[1\vAG \x1b;[1\vAH \x1b;[1\vAI \x1b;[1\vA")
       (check 'c0-controls-inside-csi
              (vector->list (vt:emulator-screen terminal))
              '("A B C D E F G H I   " "A B C D E F G H I   "
                "A B C D E F G H I   " "A B C D E F G H I   "
                "                    " "                    ")))

     (let ([terminal (vt:make-emulator 5 8)])
       (vt:emulator-feed!
         terminal
         "\x1b;[5n\x1b;[?5n\x1b;[3;5r\x1b;[?6h\x1b;[2;4H\x1b;[?6n\x1b;[c\x1b;[>c\x1b;Z")
       (check 'device-and-status-reports
              (vt:emulator-replies terminal)
              '("\x1b;[0n" "\x1b;[?0n" "\x1b;[?2;4R" "\x1b;[?1;2c"
                "\x1b;[>0;276;0c" "\x1b;[?1;2c")))

     (let ([terminal (vt:make-emulator 1 8)])
       (vt:emulator-feed! terminal "\x1b;[20h")
       (check 'newline-mode-enabled (state-ref terminal 'newline) #t)
       (check 'newline-mode-return
              (vt:emulator-input terminal "RET")
              (string->utf8 "\r\n"))
       (check 'newline-mode-keypad-return
              (vt:emulator-input terminal "KP-ENTER")
              (string->utf8 "\r\n"))
       (vt:emulator-feed! terminal "\x1b;[20l")
       (check 'newline-mode-reset-return
              (vt:emulator-input terminal "RET")
              (string->utf8 "\r")))

     (let ([terminal (vt:make-emulator 1 8)])
       (vt:emulator-feed!
         terminal "\x1b;[=c\x1b;[0x\x1b;[1x")
       (check 'tertiary-attributes-and-terminal-parameters
              (vt:emulator-replies terminal)
              '("\x1b;P!|00000000\x1b;\\"
                "\x1b;[2;1;1;128;128;1;0x"
                "\x1b;[3;1;1;128;128;1;0x")))

     (let ([terminal (vt:make-emulator 2 8)])
       (vt:emulator-feed!
         terminal "\x1b; G\x1b;[6n\x1b;[=c\x1b; F\x1b;[6n")
       (check 's8c1t-state (state-ref terminal 'eight-bit-controls) #f)
       (check 's8c1t-and-s7c1t-replies
              (vt:emulator-replies terminal)
              (list
                (string-append (string (integer->char #x9b)) "1;1R")
                (string-append (string (integer->char #x90))
                               "!|00000000"
                               (string (integer->char #x9c)))
                "\x1b;[1;1R")))

     (let ([terminal (vt:make-emulator 3 8)])
       (vt:emulator-feed!
         terminal
         (string-append
           "contents\x1b;[?1h\x1b;[?5h\x1b;[?6h\x1b;[?7l\x1b;[4h"
           "\x1b;[20h\x1b;[?25l\x1b;[?69h\x1b;[2;7s\x1b;[1;2r"
           "\x1b;[31m\x1b;(0" (string (integer->char 14)) "\x1b;[!p"))
       (check 'soft-reset-preserves-screen
              (vector-ref (vt:emulator-screen terminal) 0)
              "contents")
       (check 'soft-reset-cursor (state-ref terminal 'cursor) '(0 . 0))
       (check 'soft-reset-scroll-region
              (state-ref terminal 'scroll-region) '(0 . 2))
       (check 'soft-reset-horizontal-margins
              (state-ref terminal 'horizontal-margins) '(0 . 7))
       (for-each
         (lambda (key)
           (check (string->symbol (format "soft-reset-~a" key))
                  (state-ref terminal key) #f))
         '(origin insert newline reverse-screen application-cursor-keys
            application-keypad eight-bit-meta horizontal-margin-mode))
       (check 'soft-reset-autowrap (state-ref terminal 'autowrap) #t)
       (check 'soft-reset-cursor-visible
              (state-ref terminal 'cursor-visible) #t)
       (vt:emulator-feed! terminal "q\x1b;[2;1y")
       (check 'soft-reset-character-set-rendition-and-dectst
              (vector-ref (vt:emulator-screen terminal) 0)
              "qontents")
       (check 'soft-reset-rendition (style-at terminal 0 0) 'plain))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed!
         terminal "dirty\x1b;[?5h\x1b;[?25l\x1b;[31m\x1b;c")
       (check 'ris-clears-screen
              (vector->list (vt:emulator-screen terminal))
              '("     " "     "))
       (check 'ris-cursor (state-ref terminal 'cursor) '(0 . 0))
       (check 'ris-reverse-screen
              (state-ref terminal 'reverse-screen) #f)
       (check 'ris-cursor-visible (state-ref terminal 'cursor-visible) #t))

     (let ([terminal (vt:make-emulator 3 5)])
       (vt:emulator-feed!
         terminal "a\x85;\x9b;2CX\x9d;2;c1-title\x9c;Y")
       (check 'c1-controls
              (vector->list (vt:emulator-screen terminal))
              '("a    " "  XY " "     ")))

     (let ([terminal (vt:make-emulator 1 8)])
       (vt:emulator-feed!
         terminal "a\x90;hidden\x9c;b\x9e;hidden\x9c;c\x81;d")
       (check 'c1-controls-never-print
              (vector->list (vt:emulator-screen terminal))
              '("abcd    ")))

     (let ([terminal (vt:make-emulator 2 8)])
       (check 'control-shift-navigation
              (vt:emulator-input terminal "C-S-LEFT")
              (string->utf8 "\x1b;[1;6D"))
       (check 'modified-page-key
              (vt:emulator-input terminal "M-PAGEDOWN")
              (string->utf8 "\x1b;[6;3~"))
       (check 'insert-key
              (vt:emulator-input terminal "INSERT")
              (string->utf8 "\x1b;[2~"))
       (check 'extended-function-key
              (vt:emulator-input terminal "F37")
              (string->utf8 "\x1b;[1;6P"))
       (check 'named-modified-function-key
              (vt:emulator-input terminal "S-F12")
              (string->utf8 "\x1b;[24;2~"))
       (check 'keypad-numeric
              (vt:emulator-input terminal "KP-7")
              (string->utf8 "7"))
       (vt:emulator-feed! terminal "\x1b;=")
       (check 'keypad-application
              (vt:emulator-input terminal "KP-7")
              (string->utf8 "\x1b;Ow")))

     (let ([terminal (vt:make-emulator 2 6)])
       (vt:emulator-feed! terminal "e\x301;\x4e2d;X")
       (check 'unicode-cell-geometry
              (vector->list (vt:emulator-screen terminal))
              '("\xe9;\x4e2d;X  " "      "))
       (check 'unicode-cell-cursor (state-ref terminal 'cursor) '(0 . 4)))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1f469;\x200d;\x1f4bb;X")
       (check 'emoji-grapheme-cluster
              (vector->list (vt:emulator-screen terminal))
              '("\x1f469;\x200d;\x1f4bb;X "))
       (check 'emoji-cell-cursor (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1f1fa;\x1f1f8;X")
       (check 'regional-indicator-grapheme
              (vector->list (vt:emulator-screen terminal))
              '("\x1f1fa;\x1f1f8;X "))
       (check 'regional-indicator-width (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x1100;\x1161;\x11a8;X")
       (check 'hangul-grapheme-cluster
              (vector->list (vt:emulator-screen terminal))
              '("\xac01;X "))
       (check 'hangul-cell-width (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x4e2d;X\x1b;[2G\x1b;[K")
       (check 'erase-wide-cell-atomically
              (vector->list (vt:emulator-screen terminal))
              '("    ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x4e2d;X\x1b;[1G\x1b;[P")
       (check 'delete-wide-cell-boundary
              (vector->list (vt:emulator-screen terminal))
              '(" X  ")))

     (let ([terminal (vt:make-emulator 1 4)])
       (vt:emulator-feed! terminal "\x7;")
       (check 'terminal-bell-is-pending
              (state-ref terminal 'bell-pending) #t)
       (check 'terminal-bell-does-not-print
              (vector->list (vt:emulator-screen terminal))
              '("    ")))

     (let ([terminal (vt:make-emulator 3 5)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;1H11111\x1b;[2;1H22222\x1b;[3;1H33333\x1b;[?69h\x1b;[2;4s\x1b;[S")
       (check 'horizontal-margins-enabled
              (state-ref terminal 'horizontal-margins) '(1 . 3))
       (check 'horizontal-margin-scroll
              (vector->list (vt:emulator-screen terminal))
              '("12221" "23332" "3   3"))
       (vt:emulator-resize! terminal 3 5)
       (check 'unchanged-size-preserves-horizontal-margins
              (state-ref terminal 'horizontal-margins) '(1 . 3))
       (vt:emulator-feed! terminal "\x1b;[?69l")
       (check 'horizontal-margins-disabled
              (state-ref terminal 'horizontal-margins) '(0 . 4)))

     (let ([terminal (vt:make-emulator 2 6)])
       (vt:emulator-feed!
         terminal "\x1b;[?69h\x1b;[2;4s\x1b;[?6h\x1b;[1;1HABCD")
       (check 'horizontal-margin-autowrap
              (vector->list (vt:emulator-screen terminal))
              '(" ABC  " " D    ")))

     (let ([terminal (vt:make-emulator 10 20)])
       (vt:emulator-feed!
         terminal "\x1b;[?69h\x1b;[3;10s\x1b;[2;5r\x1b;[?6h")
       (check 'origin-mode-homes-to-left-margin
              (state-ref terminal 'cursor) '(1 . 2))
       (vt:emulator-feed! terminal "\x1b;[6n")
       (check 'cursor-report-relative-to-margins
              (vt:emulator-replies terminal)
              '("\x1b;[1;1R")))

     (let ([terminal (vt:make-emulator 1 2)])
       (vt:emulator-feed! terminal "\x1b;[31mX")
       (let ([before (vector-ref
                       (vector-ref (vt:emulator-styles terminal) 0) 0)])
         (vt:emulator-feed! terminal "\x1b;]4;1;#123456\x7;")
         (check 'osc-palette-recolors-existing-cells
                (eq? before
                     (vector-ref
                       (vector-ref (vt:emulator-styles terminal) 0) 0))
                #f))
       (vt:emulator-feed! terminal "\x1b;]4;1;?\x7;")
       (check 'osc-palette-query
              (vt:emulator-replies terminal)
              '("\x1b;]4;1;rgb:1212/3434/5656\x1b;\\")))

     (let ([terminal (vt:make-emulator 1 2)])
       (vt:emulator-feed! terminal "\x1b;]10;#abcdef\x7;")
       (check 'osc-default-foreground-state
              (state-ref terminal 'default-colors) '((171 205 239) . #f))
       (check 'osc-default-recolors-plain
              (eq? (vector-ref
                     (vector-ref (vt:emulator-styles terminal) 0) 0)
                   'plain)
              #f)
       (vt:emulator-feed! terminal "\x1b;]110\x7;")
       (check 'osc-default-foreground-reset
              (state-ref terminal 'default-colors) '(#f . #f)))

     (let ([terminal (vt:make-emulator 1 5)]
           [link '("https://example.com/path" "sample")])
       (vt:emulator-feed!
         terminal
         "\x1b;]8;id=sample;https://example.com/path\x1b;\\abc\x1b;]8;;\x1b;\\d")
       (check 'osc8-hyperlink-cells
              (vector->list
                (vector-ref (vt:emulator-hyperlinks terminal) 0))
              (list link link link #f #f))
       (vt:emulator-resize! terminal 2 3)
       (check 'osc8-hyperlink-survives-reflow
              (map vector->list
                   (vector->list
                     (vt:emulator-hyperlinks terminal)))
              (list (list link link link) (list #f #f #f))))

     (let ([terminal (vt:make-emulator 1 1)])
       (vt:emulator-feed! terminal "\x1b;]52;c;aMOp\x1b;\\")
       (check 'osc52-utf8-clipboard
              (state-ref terminal 'clipboard) "hé")
       (vt:emulator-feed! terminal "\x1b;]52;c;not-base64!\x7;")
       (check 'osc52-invalid-payload-ignored
              (state-ref terminal 'clipboard) "hé")
       (vt:emulator-feed! terminal "\x1b;]52;c;\x1b;\\")
       (check 'osc52-empty-clipboard
              (state-ref terminal 'clipboard) ""))

     (let ([terminal (vt:make-emulator 4 8)])
       (vt:emulator-feed!
         terminal
         "\x1b;[1;31m\x1b;[2;4r\x1b;P$qm\x1b;\\\x1b;P$qr\x1b;\\\x1b;P$qz\x1b;\\")
       (check 'decrqss-replies
              (vt:emulator-replies terminal)
              '("\x1b;P1$r1;31m\x1b;\\"
                "\x1b;P1$r2;4r\x1b;\\"
                "\x1b;P0$rz\x1b;\\")))

     (let ([terminal (vt:make-emulator 1 2)])
       (vt:emulator-feed!
         terminal "\x1b;P+q544e;436f;626f677573\x1b;\\")
       (check 'xtgettcap-replies
              (vt:emulator-replies terminal)
              '("\x1b;P1+r544e=787465726D2D323536636F6C6F72\x1b;\\"
                "\x1b;P1+r436f=323536\x1b;\\"
                "\x1b;P0+r626f677573\x1b;\\")))

     (let ([terminal (vt:make-emulator 2 6)])
       (vt:emulator-feed! terminal "abcdefg")
       (vt:emulator-resize! terminal 2 4)
       (check 'narrow-resize-reflows
              (vector->list (vt:emulator-screen terminal))
              '("abcd" "efg "))
       (check 'narrow-resize-cursor (state-ref terminal 'cursor) '(1 . 3))
       (vt:emulator-resize! terminal 2 6)
       (check 'wide-resize-reflows
              (vector->list (vt:emulator-screen terminal))
              '("abcdef" "g     "))
       (check 'wide-resize-cursor (state-ref terminal 'cursor) '(1 . 1)))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "\x1b;[?7l\x1b;[1;4H\x4e2d;")
       (check 'wide-cell-backs-up-at-margin-without-autowrap
              (vector->list (vt:emulator-screen terminal))
              '("  \x4e2d;" "    "))
       (check 'wide-cell-margin-cursor (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "AB\x4e2d;")
       (vt:emulator-resize! terminal 2 3)
       (check 'wide-cell-never-split
              (vector->list (vt:emulator-screen terminal))
              '("AB " "\x4e2d; "))
       (vt:emulator-resize! terminal 2 4)
       (check 'wide-cell-reflow-restores
              (vector->list (vt:emulator-screen terminal))
              '("AB\x4e2d;" "    ")))

     (let ([terminal (vt:make-emulator 2 6)])
       (vt:emulator-feed! terminal "abcdefg\x1b;[?1049hALT")
       (vt:emulator-resize! terminal 2 4)
       (vt:emulator-feed! terminal "\x1b;[?1049l")
       (check 'primary-reflows-behind-alternate-screen
              (vector->list (vt:emulator-screen terminal))
              '("abcd" "efg ")))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "abcdefghij")
       (check 'scrollback-before-reflow
              (state-ref terminal 'scrollback-lines) 1)
       (vt:emulator-resize! terminal 2 5)
       (check 'scrollback-participates-in-reflow
              (vector->list (vt:emulator-screen terminal))
              '("abcde" "fghij"))
       (check 'reflow-can-consume-scrollback
              (state-ref terminal 'scrollback-lines) 0))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "abcd\r\nX")
       (vt:emulator-resize! terminal 3 3)
       (check 'explicit-newline-survives-reflow
              (vector->list (vt:emulator-screen terminal))
              '("abc" "d  " "X  ")))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "\x1b;[?1023h\x1b;[?1023h\x1b;[9999z")
       (check 'unsupported-reports-recorded-once
              (vt:emulator-unsupported terminal)
              '("CSI \"9999z\"" "private mode 1023")))

     (let ([terminal (vt:make-emulator 4 10)])
       ;; xterm's modifyOtherKeys configuration must not reach SGR, and
       ;; its query reports the feature disabled.
       (vt:emulator-feed! terminal "\x1b;[1mX\x1b;[>4;2mY\x1b;[>4;m")
       (check 'xtmodkeys-does-not-alter-rendition
              (equal? (style-at terminal 0 0) (style-at terminal 0 1)) #t)
       (vt:emulator-feed! terminal "\x1b;[?4m")
       (check 'xtqmodkeys-reports-disabled
              (vt:emulator-replies terminal) '("\x1b;[>4;0m"))
       ;; XTVERSION and color-scheme subscriptions, as Claude Code sends.
       (vt:emulator-feed! terminal "\x1b;[?2031h\x1b;[>0q\x1b;[>q")
       (check 'xtversion-names-the-terminal
              (vt:emulator-replies terminal)
              '("\x1b;[>4;0m" "\x1b;P>|e\x1b;\\" "\x1b;P>|e\x1b;\\"))
       (vt:emulator-feed! terminal "\x1b;[?2031l\x1b;[?2031$p")
       (check 'color-scheme-mode-reset-after-unsubscribe
              (list-ref (vt:emulator-replies terminal) 3)
              "\x1b;[?2031;2$y")
       ;; Private media copy must not enter printer-controller mode.
       (vt:emulator-feed! terminal "\x1b;[?5iZ")
       (check 'private-media-copy-not-misexecuted
              (state-ref terminal 'printer-controller) #f)
       ;; A kitty keyboard-protocol push is not a cursor restore, and a
       ;; defensive pop of the empty stack is a silent no-op.
       (vt:emulator-feed! terminal "\x1b;[3;4H\x1b;[>1u\x1b;[<u\x1b;[<1u")
       (check 'kitty-keyboard-push-keeps-cursor
              (state-ref terminal 'cursor) '(2 . 3))
       ;; Private-mode save/restore and DECSCL are not DECSTBM/SCOSC/DECSTR.
       (vt:emulator-feed! terminal "\x1b;[?1049r\x1b;[?1049s\x1b;[61;1\"p")
       (check 'decorated-region-controls-keep-cursor
              (state-ref terminal 'cursor) '(2 . 3))
       ;; A graphics attribute query is not a scroll.
       (vt:emulator-feed! terminal "\x1b;[?1;1;0S")
       (check 'graphics-query-does-not-scroll
              (substring (vector-ref (vt:emulator-screen terminal) 0)
                         0 3)
              "XYZ")
       (check 'decorated-controls-reported
              (vt:emulator-unsupported terminal)
              '("CSI \"61;1\\\"p\"" "CSI \">1u\""
                "CSI \"?1049r\"" "CSI \"?1049s\"" "CSI \"?1;1;0S\""
                "CSI \"?5i\"")))

     (let ([terminal (vt:make-emulator 2 8)])
       (vt:emulator-feed! terminal "abcdefgh\x1b;[1;1H\x1b;#6")
       (check 'decdwl-displays-left-half-fullwidth
              (vector-ref (vt:emulator-screen terminal) 0)
              "\xff41;\xff42;\xff43;\xff44;")
       (vt:emulator-feed! terminal "\x1b;#5")
       (check 'decswl-restores-the-right-half
              (vector-ref (vt:emulator-screen terminal) 0)
              "abcdefgh"))

     (let ([terminal (vt:make-emulator 2 8)])
       (vt:emulator-feed! terminal "\x1b;#612345")
       (check 'decdwl-wraps-at-half-width
              (vector->list (vt:emulator-screen terminal))
              '("\xff11;\xff12;\xff13;\xff14;" "5       "))
       (check 'decdwl-wrap-cursor (state-ref terminal 'cursor) '(1 . 1)))

     (let ([terminal (vt:make-emulator 1 8)])
       (vt:emulator-feed! terminal "\x1b;#6\x1b;[1;8H")
       (check 'decdwl-clamps-cursor-to-half
              (state-ref terminal 'cursor) '(0 . 3))
       (vt:emulator-feed! terminal "\x1b;[1;3HX")
       (check 'decdwl-addresses-logical-columns
              (vector-ref (vt:emulator-screen terminal) 0)
              "\x3000;\x3000;\xff38;\x3000;"))

     (let ([terminal (vt:make-emulator 3 6)])
       (vt:emulator-feed!
         terminal "\x1b;[1;1HTop\x1b;#3\x1b;[2;1HTop\x1b;#4")
       (check 'decdhl-halves-render-double-width
              (vector->list (vt:emulator-screen terminal))
              '("\xff34;\xff4f;\xff50;" "\xff34;\xff4f;\xff50;"
                "      ")))

     (let ([terminal (vt:make-emulator 2 4)])
       (vt:emulator-feed! terminal "\x1b;#6\x1b;#8")
       (check 'decaln-resets-line-attributes
              (vector-ref (vt:emulator-screen terminal) 0) "EEEE"))

     (let ([terminal (vt:make-emulator 2 6)])
       (vt:emulator-feed! terminal "hi\x1b;#6\x1b;[?1049h\x1b;[?1049l")
       (check 'line-attributes-survive-alternate-screen
              (vector-ref (vt:emulator-screen terminal) 0)
              "\xff48;\xff49;\x3000;"))

     (let ([terminal (vt:make-emulator 2 5)])
       ;; VT100 setup modes are tracked silently; only reverse wraparound
       ;; changes behavior.
       (vt:emulator-feed!
         terminal "\x1b;[?4l\x1b;[?5l\x1b;[?8l\x1b;[?40h\x1b;[?42h\x1b;[?45l")
       (vt:emulator-feed! terminal "\x1b;[1g\x1b;[2g\x1b;(1\x1b;%G")
       (check 'setup-modes-accepted-silently
              (vt:emulator-unsupported terminal) '())
       (vt:emulator-feed!
         terminal "\x1b;[?4$p\x1b;[?8$p\x1b;[?40$p\x1b;[?42$p\x1b;[?45$p")
       (check 'setup-mode-reports
              (vt:emulator-replies terminal)
              '("\x1b;[?4;2$y" "\x1b;[?8;2$y" "\x1b;[?40;1$y"
                "\x1b;[?42;1$y" "\x1b;[?45;2$y")))

     (let ([terminal (vt:make-emulator 2 5)])
       ;; Color-scheme plumbing: silent while unknown, then DSR 996
       ;; answers and a fresh subscription hears the current scheme.
       (terminal:color-scheme! #f)
       (vt:emulator-feed! terminal "\x1b;[?996n")
       (check 'color-scheme-unknown-stays-silent
              (vt:emulator-replies terminal) '())
       (terminal:color-scheme! 'light)
       (vt:emulator-feed! terminal "\x1b;[?996n\x1b;[?2031h")
       (check 'color-scheme-query-and-subscription-report
              (vt:emulator-replies terminal)
              '("\x1b;[?997;2n" "\x1b;[?997;2n"))
       (vt:emulator-feed! terminal "\x1b;[?2031$p")
       (check 'color-scheme-subscription-tracked
              (list-ref (vt:emulator-replies terminal) 2)
              "\x1b;[?2031;1$y")
       (terminal:color-scheme! #f))

     (let ([terminal (vt:make-emulator 2 10)])
       (vt:emulator-feed! terminal "\x1b;(K[\\]{|}~\x1b;(B#")
       (check 'german-replacement-set
              (vector-ref (vt:emulator-screen terminal) 0)
              "\x00c4;\x00d6;\x00dc;\x00e4;\x00f6;\x00fc;\x00df;#  ")
       (vt:emulator-feed! terminal "\x1b;[2;1H\x1b;)=\x0e;#_\x0f;")
       (check 'swiss-replacement-set-in-g1
              (substring (vector-ref (vt:emulator-screen terminal) 1)
                         0 2)
              "\x00f9;\x00e8;")
       (check 'national-designators-silent
              (vt:emulator-unsupported terminal) '()))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed! terminal "abcdef")
       (check 'no-reverse-wrap-stops-at-left
              (state-ref terminal 'cursor) '(1 . 1))
       (vt:emulator-feed! terminal "\x08;\x08;")
       (check 'backspace-stops-at-left-margin
              (state-ref terminal 'cursor) '(1 . 0))
       (vt:emulator-feed! terminal "\x1b;[?45h\x08;")
       (check 'reverse-wraparound-backs-onto-previous-line
              (state-ref terminal 'cursor) '(0 . 4))
       (check 'reverse-wraparound-reported
              (state-ref terminal 'reverse-wraparound) #t))

     (let ([terminal (vt:make-emulator 2 10)])
       (vt:emulator-feed! terminal "AB\x7f;C")
       (check 'del-is-ignored
              (vector-ref (vt:emulator-screen terminal) 0)
              "ABC       ")
       (check 'del-does-not-move-cursor
              (state-ref terminal 'cursor) '(0 . 3)))

     (let ([terminal (vt:make-emulator 2 10)])
       ;; SS2, unknown ESC # and ESC SP finals, an unknown charset, and an
       ;; unknown ESC intermediate sequence; supported designators stay
       ;; silent and no final byte leaks onto the screen.
       (vt:emulator-feed! terminal (string (integer->char #x8e)))
       (vt:emulator-feed! terminal "\x1b;(B\x1b;)0\x1b;#7\x1b; L\x1b;(>\x1b;%G")
       (check 'unknown-escape-payload-not-painted
              (vector-ref (vt:emulator-screen terminal) 0)
              "          ")
       (vt:emulator-feed! terminal "\x1b;%@")
       (check 'silent-escape-paths-report
              (vt:emulator-unsupported terminal)
              '("C1 control 0x8E" "ESC \" L\"" "ESC \"#7\"" "ESC \"%@\""
                "G0 charset designator \">\"")))

     (let ([terminal (vt:make-emulator 2 5)])
       (vt:emulator-feed!
         terminal "\x1b;[?2004h\x1b;[?2004$p\x1b;[?7$p\x1b;[?2026$p")
       (vt:emulator-feed! terminal "\x1b;[4h\x1b;[4$p\x1b;[20$p")
       (vt:emulator-feed! terminal "\x1b;[?2026h\x1b;[?2026$p")
       (vt:emulator-feed! terminal "\x1b;[22;0t\x1b;[23;0t\x1b;[18t")
       (check 'mode-state-reports
              (vt:emulator-replies terminal)
              '("\x1b;[?2004;1$y" "\x1b;[?7;1$y" "\x1b;[?2026;2$y"
                "\x1b;[4;1$y" "\x1b;[20;2$y" "\x1b;[?2026;1$y"
                "\x1b;[8;2;5t")))

     (let ([terminal (vt:make-emulator 3 10)])
       (vt:emulator-feed! terminal "1\r\n2\r\n3\r\n4\r\n5")
       (check 'erase-saved-lines-before
              (state-ref terminal 'scrollback-lines) 2)
       (vt:emulator-feed! terminal "\x1b;[3J")
       (check 'erase-saved-lines-clears-scrollback
              (state-ref terminal 'scrollback-lines) 0)
       (check 'erase-saved-lines-keeps-screen
              (vector->list (vt:emulator-screen terminal))
              '("3         " "4         " "5         ")))

     ;; The inactive alternate-screen stash keeps its own dimensions;
     ;; a resize between alternate sessions must not corrupt it.
     (let ([terminal (vt:make-emulator 24 80)])
       (vt:emulator-feed! terminal "\x1b;[?1049h")
       (vt:emulator-resize! terminal 12 76)
       (vt:emulator-feed! terminal "\x1b;[?1049l")
       (vt:emulator-resize! terminal 15 83)
       (vt:emulator-feed! terminal "\x1b;[?1049h")
       (vt:emulator-resize! terminal 23 103)
       (check 'resize-between-alternate-sessions
              (vector-length (vt:emulator-screen terminal)) 23))

     (let ([terminal (vt:make-emulator 24 80)])
       (vt:emulator-feed! terminal "\x1b;[?47habc")
       (vt:emulator-feed! terminal "\x1b;[?47l")
       (vt:emulator-resize! terminal 30 80)
       (vt:emulator-feed! terminal "\x1b;[?47h")
       (check 'stashed-alternate-grows-with-resize
              (list (vector-length (vt:emulator-screen terminal))
                    (substring
                      (vector-ref (vt:emulator-screen terminal) 0)
                      0 3))
              '(30 "abc")))

     ;; The publication boundary owns text, rendition, geometry and facts;
     ;; no generated face registration or head callback is needed to read it.
     (let ([emulator (vt:make-emulator 1 5)] [calls (test:recorder)])
       (dynamic-wind
         (lambda () (head:set-repaint-hook! (lambda () (calls 'repaint))))
         (lambda ()
           (vt:emulator-feed! emulator
             "\x1b;[31m\x1b;]8;id=wide;https://frame.example\x1b;\\界\x1b;]8;;\x1b;\\q\x301;Z\x1b;[6 q")
           (let* ([frame (vt:emulator-frame emulator)]
                  [row (car (cadr frame))])
             (check 'frame-pairs-text-rendition-cursor-and-facts
               frame
               '(#("界q\x301;Z ")
                 ((0 #("0;31" "0;31" "0;31" "0;31" plain)
                   #(("https://frame.example" "wide") ("https://frame.example" "wide") #f #f #f)
                   ((clusters (1 . 2) (2 . 1) (1 . 1) (1 . 1)))))
                 (0 4 #t) (1 5) ((cursor-style . bar))))
             (string-set! (vector-ref (car frame) 0) 0 #\X)
             (string-set! (vector-ref (cadr row) 0) 0 #\X)
             (string-set! (car (vector-ref (caddr row) 0)) 0 #\X)
             (set-car! (caddr frame) 99)
             (set-car! (car (cdar (cadddr row))) 99)
             (check 'frame-mutation-cannot-change-the-emulator
               (list (vector-ref (vt:emulator-screen emulator) 0)
                     (style-at emulator 0 0)
                     (vector-ref (vector-ref (vt:emulator-hyperlinks emulator) 0) 0)
                     (caddr (vt:emulator-frame emulator)))
               '("界q\x301;Z " "0;31" ("https://frame.example" "wide") (0 4 #t))))
           (head:run-deferred!)
           (check 'frame-feed-and-read-do-not-call-the-head (calls) '()))
         (lambda () (head:set-repaint-hook! paint:invalidate-screen-cache!)))
       (let* ([links (vt:emulator-hyperlinks emulator)]
              [uri (car (vector-ref (vector-ref links 0) 0))])
         (string-set! uri 0 #\X)
         (check 'legacy-link-reads-also-own-their-payload
           (car (vector-ref (vector-ref (vt:emulator-hyperlinks emulator) 0) 0))
           "https://frame.example"))
       (let ([frame (vt:emulator-frame emulator)])
         (vt:emulator-feed! emulator "\x1b;[?2026h\x1b;[HNEW\x1b;[?25l")
         (check 'synchronized-frame-is-held (vt:emulator-frame emulator) #f)
         (vt:emulator-feed! emulator "\x1b;[?2026l")
         (check 'synchronized-release-keeps-the-old-snapshot-independent
           (list (vector-ref (car frame) 0) (caddr frame)
                 (vector-ref (car (vt:emulator-frame emulator)) 0)
                 (caddr (vt:emulator-frame emulator)))
           '("界q\x301;Z " (0 4 #t) "NEWZ " (0 3 #f)))))

     ;; Feed and resize use the same lock as frame capture. Any sampled
     ;; frame must contain the dimensions and cursor of its own text grid.
     (let* ([emulator (vt:make-emulator 2 8)]
            [writer (test:worker
                      (lambda ()
                        (do ([i 0 (+ i 1)]) ((= i 60))
                          (vt:emulator-resize! emulator (+ 2 (mod i 2)) (+ 5 (mod i 4)))
                          (vt:emulator-feed! emulator "\x1b;[2J\x1b;[Habc\x1b;[32mZ"))))]
            [coherent?
             (for-all
               (lambda (i)
                 (let* ([frame (vt:emulator-frame emulator)]
                        [text (car frame)] [rows (cadr frame)] [cursor (caddr frame)] [size (cadddr frame)])
                   (and (>= (vector-length text) (car size))
                        (= (vector-length text) (length rows))
                        (for-all (lambda (row)
                                   (and (= (vector-length (cadr row))
                                           (string-length (vector-ref text (car row))))
                                        (or (< (car row) (- (length rows) (car size)))
                                            (= (vector-length (cadr row)) (cadr size))))) rows)
                        (<= (- (length rows) (car size)) (car cursor))
                        (< (car cursor) (length rows)) (< (cadr cursor) (cadr size))))) (iota 60))])
       (writer)
       (check 'concurrent-frames-never-mix-dimensions coherent? #t))

     (test:finish! 'terminal)))
