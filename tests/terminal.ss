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

     (define (style-at emulator row column)
       (vector-ref (vector-ref (terminal-emulator-styles emulator) row)
                   column))

     (let ([terminal (make-terminal-emulator 4 5)])
       (do ([row 1 (+ row 1)]) ((> row 4))
         (do ([column 1 (+ column 1)]) ((> column 5))
           (terminal-emulator-feed!
             terminal
             (format "\x1b;[4;5H\x1b;7\x1b;[~a;~aHA\x1b;8" row column))))
       (terminal-emulator-resize! terminal 5 5)
       (check 'dec-save-restore-repeated-positioning
              (vector->list (terminal-emulator-screen terminal))
              '("AAAAA" "AAAAA" "AAAAA" "AAAAA" "     ")))

     (let ([terminal (make-terminal-emulator 21 79)])
       (do ([row 1 (+ row 1)]) ((> row 4))
         (do ([column 1 (+ column 1)]) ((> column 5))
           (terminal-emulator-feed!
             terminal
             (format "\x1b;[~a;~aH" (+ 8 (* 2 (- row 1)))
                     (+ 12 (* 12 (- column 1)))))
           (terminal-emulator-feed!
             terminal (if (even? row) "\x1b;(0" "\x1b;(B"))
           (terminal-emulator-feed! terminal "*****\x1b;7")
           (terminal-emulator-feed!
             terminal (format "\x1b;[~a;~aH" row column))
           (terminal-emulator-feed! terminal "\x1b;[0m\x1b;(BA\x1b;8*****")))
       (terminal-emulator-resize! terminal 22 79)
       (check 'dec-save-restore-vttest-pattern-after-resize
              (map (lambda (line) (substring line 0 5))
                   (vector->list (terminal-emulator-screen terminal)))
              (append '("AAAAA" "AAAAA" "AAAAA" "AAAAA")
                      (make-list 18 "     "))))

     (let ([terminal (make-terminal-emulator 22 79)])
       (terminal-emulator-feed!
         terminal "\x1b;[2J\x1b;[?6h\x1b;[1;22r\x1b;[2J\x1b;[22B")
       (do ([line 1 (+ line 1)]) ((> line 27))
         (terminal-emulator-feed!
           terminal
           (format "Soft scroll up region [1..22] size 22 Line ~a\r\n" line)))
       (terminal-emulator-feed! terminal "\x1b;[22A")
       (do ([line 1 (+ line 1)]) ((> line 27))
         (terminal-emulator-feed!
           terminal
           (format "Soft scroll down region [1..22] size 22 Line ~a\r\n\x1b;M\x1b;M"
                   line)))
       (terminal-emulator-feed! terminal "Push <RETURN>")
       (check 'full-screen-reverse-index-scroll
              (vector->list (terminal-emulator-screen terminal))
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

     (let ([terminal (make-terminal-emulator 5 8)])
       (terminal-emulator-feed!
         terminal "\x1b;[3;4H\x1b;[0A\x1b;[2C\x1b;[B\x1b;[D")
       (check 'relative-cursor-defaults
              (state-ref terminal 'cursor) '(2 . 4))
       (terminal-emulator-feed! terminal "\x1b;[2E\x1b;[F")
       (check 'cursor-next-previous-line
              (state-ref terminal 'cursor) '(3 . 0))
       (terminal-emulator-feed! terminal "\x1b;[7G\x1b;[2d")
       (check 'absolute-row-column
              (state-ref terminal 'cursor) '(1 . 6)))

     (let ([terminal (make-terminal-emulator 3 5)])
       (terminal-emulator-feed!
         terminal "11111\x1b;[2;1H22222\x1b;[3;1H33333\x1b;[2;3H\x1b;[J")
       (check 'erase-display-after-cursor
              (vector->list (terminal-emulator-screen terminal))
              '("11111" "22   " "     "))
       (terminal-emulator-feed!
         terminal "\x1b;[1;1H11111\x1b;[2;1H22222\x1b;[3;1H33333\x1b;[2;3H\x1b;[1J")
       (check 'erase-display-before-cursor
              (vector->list (terminal-emulator-screen terminal))
              '("     " "   22" "33333"))
       (terminal-emulator-feed! terminal "\x1b;[2J")
       (check 'erase-display-all
              (vector->list (terminal-emulator-screen terminal))
              '("     " "     " "     ")))

     (let ([terminal (make-terminal-emulator 1 7)])
       (terminal-emulator-feed! terminal "abcdef\x1b;[3G\x1b;[2P")
       (check 'delete-characters
              (vector->list (terminal-emulator-screen terminal))
              '("abef   "))
       (terminal-emulator-feed! terminal "\x1b;[3G\x1b;[2@")
       (check 'insert-characters
              (vector->list (terminal-emulator-screen terminal))
              '("ab  ef "))
       (terminal-emulator-feed! terminal "\x1b;[3G\x1b;[3X")
       (check 'erase-characters
              (vector->list (terminal-emulator-screen terminal))
              '("ab   f ")))

     (let ([terminal (make-terminal-emulator 1 20)])
       (terminal-emulator-feed! terminal "\t\t\x1b;[Z")
       (check 'back-tab (state-ref terminal 'cursor) '(0 . 8))
       (terminal-emulator-feed! terminal "\x1b;[g\x1b;[Z")
       (check 'clear-current-tab-stop
              (state-ref terminal 'cursor) '(0 . 0))
       (terminal-emulator-feed! terminal "\x1b;H\x1b;[3g\t")
       (check 'clear-all-tab-stops
              (state-ref terminal 'cursor) '(0 . 19)))

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

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1b;(A#\x1b;(B#")
       (check 'vt100-british-character-set
              (vector->list (terminal-emulator-screen terminal))
              '("\xa3;#  ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1b;)A\x0e;#\x0f;#")
       (check 'vt100-british-g1-shift
              (vector->list (terminal-emulator-screen terminal))
              '("\xa3;#  ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed! terminal "\x1b;(1q\x1b;(2q")
       (check 'vt100-alternate-rom-aliases
              (vector->list (terminal-emulator-screen terminal))
              '("q\x2500;  ")))

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed!
         terminal "\x1b;(B\x1b;)B\x1b;*B\x1b;+B\x1b;-%5\x1b;.&4\x1b;/>Z")
       (check 'iso-2022-g2-g3-and-multibyte-designations
              (vector->list (terminal-emulator-screen terminal))
              '("Z   ")))

     (let ([terminal (make-terminal-emulator 26 79)])
       (terminal-emulator-feed!
         terminal
         (string-append
           "\x1b;[22;48H\x1b;)2\x1b;(B\x0e;"
           (list->string
             (map (lambda (number) (integer->char (+ 96 number)))
                  (iota 32)))
           "\x1b;(B\x1b;)B\x0f;\x1b;[26;1H"))
       (check 'vt100-charset-reset-after-right-margin
              (substring
                (vector-ref (terminal-emulator-screen terminal) 21) 0 9)
              "         "))

     (let ([terminal (make-terminal-emulator 3 4)])
       (terminal-emulator-feed! terminal "abc\x1b;#8")
       (check 'screen-alignment-pattern
              (vector->list (terminal-emulator-screen terminal))
              '("EEEE" "EEEE" "EEEE"))
       (check 'screen-alignment-cursor
              (state-ref terminal 'cursor) '(0 . 0)))

     (let ([terminal (make-terminal-emulator 3 5)])
       (terminal-emulator-feed!
         terminal "abc\x1b;[2;3r\x1b;[3;4H\x1b;[?3l")
       (check 'column-mode-clears-screen
              (vector->list (terminal-emulator-screen terminal))
              '("     " "     " "     "))
       (check 'column-mode-homes-cursor
              (state-ref terminal 'cursor) '(0 . 0))
       (check 'column-mode-resets-margins
              (state-ref terminal 'scroll-region) '(0 . 2)))

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

     (let ([terminal (make-terminal-emulator 1 4)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;3;4;5;7;8;9;38;5;123;48;2;1;2;3mA\x1b;[22;23;24;25;27;28;29;39;49mB")
       (check 'combined-sgr-is-styled
              (eq? (style-at terminal 0 0) 'plain) #f)
       (check 'selective-sgr-resets-to-plain
              (style-at terminal 0 1) 'plain))

     (let ([semicolon (make-terminal-emulator 1 2)]
           [colon (make-terminal-emulator 1 2)])
       (terminal-emulator-feed! semicolon "\x1b;[38;2;10;20;30mX")
       (terminal-emulator-feed! colon "\x1b;[38:2::10:20:30mX")
       (check 'colon-rgb-matches-semicolon-rgb
              (style-at colon 0 0) (style-at semicolon 0 0)))

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

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[?1004;1005;1015h")
       (check 'focus-reporting-mode
              (state-ref terminal 'focus-reporting) #t)
       (check 'focus-in-encoding
              (terminal-emulator-input terminal "FOCUS")
              (string->utf8 "\x1b;[I"))
       (check 'focus-out-encoding
              (terminal-emulator-input terminal "BLUR")
              (string->utf8 "\x1b;[O"))
       (check 'utf8-mouse-mode (state-ref terminal 'utf8-mouse) #t)
       (check 'urxvt-mouse-mode (state-ref terminal 'urxvt-mouse) #t)
       (terminal-emulator-feed! terminal "\x1b;[?1004;1005;1015l")
       (check 'focus-reporting-disabled
              (terminal-emulator-input terminal "FOCUS") #f))

     (let ([terminal (make-terminal-emulator 2 5)])
       (check 'mouse-input-disabled
              (terminal-emulator-mouse-input terminal 0 4 5 #f) #f)
       (terminal-emulator-feed! terminal "\x1b;[?1000h")
       (check 'x10-mouse-press
              (terminal-emulator-mouse-input terminal 0 4 5 #f)
              (bytevector 27 91 77 32 36 37))
       (check 'x10-mouse-release
              (terminal-emulator-mouse-input terminal 0 4 5 #t)
              (bytevector 27 91 77 35 36 37))
       (check 'x10-mouse-coordinate-limit
              (terminal-emulator-mouse-input terminal 64 300 400 #f)
              (bytevector 27 91 77 96 255 255)))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[?1002;1006h")
       (check 'sgr-mouse-motion
              (terminal-emulator-mouse-input terminal 32 4 5 #f)
              (string->utf8 "\x1b;[<32;4;5M"))
       (check 'sgr-mouse-release
              (terminal-emulator-mouse-input terminal 0 4 5 #t)
              (string->utf8 "\x1b;[<0;4;5m")))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[?1000;1015h")
       (check 'urxvt-mouse-press
              (terminal-emulator-mouse-input terminal 4 4 5 #f)
              (string->utf8 "\x1b;[36;4;5M"))
       (check 'urxvt-mouse-release
              (terminal-emulator-mouse-input terminal 4 4 5 #t)
              (string->utf8 "\x1b;[35;4;5M")))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[?1000;1005h")
       (check 'utf8-mouse-wheel
              (terminal-emulator-mouse-input terminal 64 4 5 #f)
              (string->utf8 "\x1b;[M`$%")))

     (let ([terminal (make-terminal-emulator 1 2)])
       (check 'meta-defaults-to-escape
              (terminal-emulator-input terminal "M-x")
              (bytevector 27 120))
       (terminal-emulator-feed! terminal "\x1b;[?1034h")
       (check 'eight-bit-meta-mode (state-ref terminal 'eight-bit-meta) #t)
       (check 'eight-bit-meta-character
              (terminal-emulator-input terminal "M-x")
              (bytevector 248))
       (check 'eight-bit-control-meta-character
              (terminal-emulator-input terminal "C-M-a")
              (bytevector 129))
       (terminal-emulator-feed! terminal "\x1b;[?1034l")
       (check 'eight-bit-meta-disabled
              (terminal-emulator-input terminal "M-x")
              (bytevector 27 120)))

     (let ([terminal (make-terminal-emulator 1 2)])
       (check 'meta-return
              (terminal-emulator-input terminal "M-RET")
              (bytevector 27 13))
       (check 'meta-backspace
              (terminal-emulator-input terminal "M-BACKSPACE")
              (bytevector 27 127))
       (check 'meta-shift-tab
              (terminal-emulator-input terminal "M-S-TAB")
              (string->utf8 "\x1b;\x1b;[Z")))

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

     (let ([terminal (make-terminal-emulator 4 3)])
       (terminal-emulator-feed!
         terminal
         "111\x1b;[2;1H222\x1b;[3;1H333\x1b;[4;1H444\x1b;[2;3r\x1b;[3;1H\x1b;D")
       (check 'index-scrolls-active-region
              (vector->list (terminal-emulator-screen terminal))
              '("111" "333" "   " "444"))
       (terminal-emulator-feed! terminal "\x1b;[2;1H\x1b;M")
       (check 'reverse-index-scrolls-active-region
              (vector->list (terminal-emulator-screen terminal))
              '("111" "   " "333" "444")))

     (let ([terminal (make-terminal-emulator 4 3)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;1H111\x1b;[2;1H222\x1b;[3;1H333\x1b;[4;1H444\x1b;[2;1H\x1b;l\x1b;[4;1H\n")
       (check 'memory-lock-state (state-ref terminal 'memory-lock) 1)
       (check 'memory-lock-preserves-upper-rows
              (vector->list (terminal-emulator-screen terminal))
              '("111" "333" "444" "   "))
       (terminal-emulator-feed! terminal "\x1b;m\x1b;[4;1H\n")
       (check 'memory-unlock-state (state-ref terminal 'memory-lock) #f)
       (check 'memory-unlock-restores-full-scroll
              (vector->list (terminal-emulator-screen terminal))
              '("333" "444" "   " "   ")))

     (let ([terminal (make-terminal-emulator 2 4)])
       (terminal-emulator-feed! terminal "AB\x1b;[i")
       (check 'printer-screen-copy
              (state-ref terminal 'printer-output)
              "AB  \r\n    \r\n")
       (terminal-emulator-feed! terminal "\x1b;[5iprinted\x1b;[4iX")
       (check 'printer-controller-stops-echo
              (vector->list (terminal-emulator-screen terminal))
              '("ABX " "    "))
       (check 'printer-controller-output
              (state-ref terminal 'printer-output)
              "AB  \r\n    \r\nprinted")
       (check 'printer-controller-disabled
              (state-ref terminal 'printer-controller) #f)
       (terminal-emulator-feed! terminal "\x1b;[5ic1\x9b;4iY")
       (check 'printer-controller-c1-termination
              (state-ref terminal 'printer-output)
              "AB  \r\n    \r\nprintedc1")
       (check 'printer-controller-c1-resumes-display
              (vector->list (terminal-emulator-screen terminal))
              '("ABXY" "    ")))

     (let ([terminal (make-terminal-emulator 2 5)])
       (terminal-emulator-feed! terminal "\x1b;[123\x18;A")
       (check 'cancel-csi
              (vector-ref (terminal-emulator-screen terminal) 0)
              "A    ")
       (terminal-emulator-feed! terminal "\x1b;[6n")
       (check 'cursor-report
              (terminal-emulator-replies terminal)
              '("\x1b;[1;2R")))

     (let ([terminal (make-terminal-emulator 6 20)])
       (terminal-emulator-feed!
         terminal
         "A B C D E F G H I\r\nA\x1b;[2\x8;CB\x1b;[2\x8;CC\x1b;[2\x8;CD\x1b;[2\x8;CE\x1b;[2\x8;CF\x1b;[2\x8;CG\x1b;[2\x8;CH\x1b;[2\x8;CI\x1b;[2\x8;C\r\nA \x1b;[\r2CB\x1b;[\r4CC\x1b;[\r6CD\x1b;[\r8CE\x1b;[\r10CF\x1b;[\r12CG\x1b;[\r14CH\x1b;[\r16CI\r\nA \x1b;[1\vAB \x1b;[1\vAC \x1b;[1\vAD \x1b;[1\vAE \x1b;[1\vAF \x1b;[1\vAG \x1b;[1\vAH \x1b;[1\vAI \x1b;[1\vA")
       (check 'c0-controls-inside-csi
              (vector->list (terminal-emulator-screen terminal))
              '("A B C D E F G H I   " "A B C D E F G H I   "
                "A B C D E F G H I   " "A B C D E F G H I   "
                "                    " "                    ")))

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
       (terminal-emulator-resize! terminal 3 5)
       (check 'unchanged-size-preserves-horizontal-margins
              (state-ref terminal 'horizontal-margins) '(1 . 3))
       (terminal-emulator-feed! terminal "\x1b;[?69l")
       (check 'horizontal-margins-disabled
              (state-ref terminal 'horizontal-margins) '(0 . 4)))

     (let ([terminal (make-terminal-emulator 2 6)])
       (terminal-emulator-feed!
         terminal "\x1b;[?69h\x1b;[2;4s\x1b;[?6h\x1b;[1;1HABCD")
       (check 'horizontal-margin-autowrap
              (vector->list (terminal-emulator-screen terminal))
              '(" ABC  " " D    ")))

     (let ([terminal (make-terminal-emulator 1 2)])
       (terminal-emulator-feed! terminal "\x1b;[31mX")
       (let ([before (vector-ref
                       (vector-ref (terminal-emulator-styles terminal) 0) 0)])
         (terminal-emulator-feed! terminal "\x1b;]4;1;#123456\x7;")
         (check 'osc-palette-recolors-existing-cells
                (eq? before
                     (vector-ref
                       (vector-ref (terminal-emulator-styles terminal) 0) 0))
                #f))
       (terminal-emulator-feed! terminal "\x1b;]4;1;?\x7;")
       (check 'osc-palette-query
              (terminal-emulator-replies terminal)
              '("\x1b;]4;1;rgb:1212/3434/5656\x1b;\\")))

     (let ([terminal (make-terminal-emulator 1 2)])
       (terminal-emulator-feed! terminal "\x1b;]10;#abcdef\x7;")
       (check 'osc-default-foreground-state
              (state-ref terminal 'default-colors) '((171 205 239) . #f))
       (check 'osc-default-recolors-plain
              (eq? (vector-ref
                      (vector-ref (terminal-emulator-styles terminal) 0) 0)
                    'plain)
              #f)
       (terminal-emulator-feed! terminal "\x1b;]110\x7;")
       (check 'osc-default-foreground-reset
              (state-ref terminal 'default-colors) '(#f . #f)))

     (let ([terminal (make-terminal-emulator 4 8)])
       (terminal-emulator-feed!
         terminal
         "\x1b;[1;31m\x1b;[2;4r\x1b;P$qm\x1b;\\\x1b;P$qr\x1b;\\\x1b;P$qz\x1b;\\")
       (check 'decrqss-replies
              (terminal-emulator-replies terminal)
              '("\x1b;P1$r1;31m\x1b;\\"
                "\x1b;P1$r2;4r\x1b;\\"
                "\x1b;P0$rz\x1b;\\")))

     (let ([terminal (make-terminal-emulator 1 2)])
       (terminal-emulator-feed!
         terminal "\x1b;P+q544e;436f;626f677573\x1b;\\")
       (check 'xtgettcap-replies
              (terminal-emulator-replies terminal)
              '("\x1b;P1+r544e=787465726D2D323536636F6C6F72\x1b;\\"
                "\x1b;P1+r436f=323536\x1b;\\"
                "\x1b;P0+r626f677573\x1b;\\")))

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
