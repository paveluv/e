;; terminal.e -- PTY-backed terminal emulator app.

(library (terminal)
  (export init! terminal!! terminal-send! terminal-close! terminal-scrollback
          terminal-shell
          make-terminal-emulator terminal-emulator?
          terminal-emulator-feed! terminal-emulator-resize!
          terminal-emulator-screen
          terminal-emulator-styles terminal-emulator-state terminal-emulator-input
          terminal-emulator-replies)
  (import (chezscheme) (core) (sys)
          (only (describe) register-descriptions!))

  (define-record-type terminal-state
    (fields buffer process display lock
            (mutable rows) (mutable cols) (mutable screen) (mutable wrapped)
            (mutable row) (mutable col)
            (mutable saved-row) (mutable saved-col) (mutable saved-state)
            (mutable scroll-top) (mutable scroll-bottom)
            (mutable left-margin) (mutable right-margin) (mutable margin-mode)
            (mutable parser) (mutable parameters)
            (mutable osc-escape) (mutable osc-text) (mutable replies)
            (mutable charset) (mutable charset-g1) (mutable charset-target)
            (mutable shift)
            (mutable wrap-pending) (mutable autowrap) (mutable origin)
            (mutable insert) (mutable reverse-screen)
            (mutable cursor-keys) (mutable keypad) (mutable meta-eight-bit)
            (mutable cursor-visible) (mutable tab-stops)
            (mutable last-character)
            (mutable dirty) (mutable alive) (mutable bell) (mutable prefix)
            (mutable bell-visible) (mutable bell-generation)
            (mutable mouse) (mutable mouse-sgr) (mutable mouse-utf8)
            (mutable mouse-urxvt) (mutable focus-reporting)
            (mutable bracketed) (mutable main-screen) (mutable main-wrapped)
            (mutable main-row) (mutable main-col) (mutable main-state)
            (mutable alternate-screen) (mutable alternate-wrapped)
            (mutable alternate-styles)
            (mutable alternate-state)
            (mutable history) (mutable history-wrapped)
            (mutable unfollowed-windows)
            (mutable styles) (mutable main-styles) (mutable history-styles)
            (mutable rendered-cells) (mutable rendered-styles)
            (mutable sgr) (mutable style)
            (mutable palette) (mutable default-foreground)
            (mutable default-background)))

  (define terminals '())
  (define serial 0)
  (define style-serial 0)
  (define style-cache (make-hashtable string-hash string=?))
  (define style-sequences (make-eq-hashtable))
  (define style-lock (make-mutex))
  (define terminal-scrollback
    (make-parameter 10000
      (lambda (lines)
        (unless (and (integer? lines) (exact? lines) (>= lines 0))
          (error 'terminal-scrollback "must be a nonnegative integer" lines))
        lines)))
  (define terminal-shell
    (make-parameter
      (let ([shell (getenv "SHELL")])
        (if (and shell (not (string=? shell ""))) shell "/bin/sh"))
      (lambda (shell)
        (unless (and (string? shell) (not (string=? shell "")))
          (error 'terminal-shell "must be a nonempty string" shell))
        shell)))

  (define (make-default-palette)
    (let* ([palette (make-vector 256 #f)]
           [base '((0 0 0) (205 0 0) (0 205 0) (205 205 0)
                   (0 0 238) (205 0 205) (0 205 205) (229 229 229)
                   (127 127 127) (255 0 0) (0 255 0) (255 255 0)
                   (92 92 255) (255 0 255) (0 255 255) (255 255 255))]
           [levels '#(0 95 135 175 215 255)])
      (do ([colors base (cdr colors)] [index 0 (+ index 1)])
          ((null? colors))
        (vector-set! palette index (car colors)))
      (do ([red 0 (+ red 1)]) ((= red 6))
        (do ([green 0 (+ green 1)]) ((= green 6))
          (do ([blue 0 (+ blue 1)]) ((= blue 6))
            (vector-set! palette
                         (+ 16 (* red 36) (* green 6) blue)
                         (list (vector-ref levels red)
                               (vector-ref levels green)
                               (vector-ref levels blue))))))
      (do ([index 232 (+ index 1)]) ((= index 256))
        (let ([level (+ 8 (* 10 (- index 232)))])
          (vector-set! palette index (list level level level))))
      palette))

  (define (terminal-emulator? value) (terminal-state? value))

  (define (make-terminal-emulator rows cols)
    (unless (and (integer? rows) (exact? rows) (> rows 0)
                 (integer? cols) (exact? cols) (> cols 0))
      (error 'make-terminal-emulator
             "rows and columns must be positive exact integers" rows cols))
    (make-terminal-state #f #f #f (make-mutex)
                         rows cols (make-screen rows cols) (make-vector rows #f)
                         0 0 0 0 #f 0 (- rows 1) 0 (- cols 1) #f
                         'normal "" #f "" '() 'ascii 'ascii 0 0
                         #f #t #f #f #f #f #f #f #t
                         (default-tab-stops cols) #\space
                         #f #f #f #f #f 0 #f #f #f #f #f #f #f #f
                         0 0 #f #f #f #f #f '() '() '()
                         (make-style-screen rows cols 'plain)
                         #f '() #f (make-style-screen rows cols 'plain)
                         "" 'plain (make-vector 256 #f) #f #f))

  (define (terminal-emulator-feed! emulator text)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-feed! "expected a terminal emulator" emulator))
    (unless (string? text)
      (error 'terminal-emulator-feed! "expected a string" text))
    (string-for-each (lambda (character) (feed-character! emulator character))
                     text)
    (void))

  (define (terminal-emulator-resize! emulator rows cols)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-resize! "expected a terminal emulator" emulator))
    (unless (and (integer? rows) (exact? rows) (> rows 0)
                 (integer? cols) (exact? cols) (> cols 0))
      (error 'terminal-emulator-resize!
             "rows and columns must be positive exact integers" rows cols))
    (resize-screen! emulator rows cols)
    (void))

  (define (terminal-emulator-screen emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-screen "expected a terminal emulator" emulator))
    (vector-map cell-row->string (terminal-state-screen emulator)))

  (define (terminal-emulator-styles emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-styles "expected a terminal emulator" emulator))
    (effective-style-screen emulator (terminal-state-styles emulator)))

  (define (terminal-emulator-state emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-state "expected a terminal emulator" emulator))
    `((rows . ,(terminal-state-rows emulator))
      (columns . ,(terminal-state-cols emulator))
      (scrollback-lines . ,(length (terminal-state-history emulator)))
      (wrapped-rows . ,(vector->list (terminal-state-wrapped emulator)))
      (cursor . ,(cons (terminal-state-row emulator)
                       (terminal-state-col emulator)))
      (scroll-region . ,(cons (terminal-state-scroll-top emulator)
                              (terminal-state-scroll-bottom emulator)))
      (horizontal-margins . ,(cons (terminal-state-left-margin emulator)
                                   (terminal-state-right-margin emulator)))
      (horizontal-margin-mode . ,(terminal-state-margin-mode emulator))
      (wrap-pending . ,(terminal-state-wrap-pending emulator))
      (autowrap . ,(terminal-state-autowrap emulator))
      (origin . ,(terminal-state-origin emulator))
      (insert . ,(terminal-state-insert emulator))
      (reverse-screen . ,(terminal-state-reverse-screen emulator))
      (bell-pending . ,(terminal-state-bell emulator))
      (bell-visible . ,(terminal-state-bell-visible emulator))
      (cursor-visible . ,(terminal-state-cursor-visible emulator))
      (application-cursor-keys . ,(terminal-state-cursor-keys emulator))
      (application-keypad . ,(terminal-state-keypad emulator))
      (eight-bit-meta . ,(terminal-state-meta-eight-bit emulator))
      (mouse-tracking . ,(terminal-state-mouse emulator))
      (sgr-mouse . ,(terminal-state-mouse-sgr emulator))
      (utf8-mouse . ,(terminal-state-mouse-utf8 emulator))
      (urxvt-mouse . ,(terminal-state-mouse-urxvt emulator))
      (focus-reporting . ,(terminal-state-focus-reporting emulator))
      (bracketed-paste . ,(terminal-state-bracketed emulator))
      (default-colors . ,(cons (terminal-state-default-foreground emulator)
                               (terminal-state-default-background emulator)))))

  (define (terminal-emulator-input emulator event)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-input "expected a terminal emulator" emulator))
    (unless (string? event)
      (error 'terminal-emulator-input "expected an event string" event))
    (event-bytes emulator event))

  (define (terminal-emulator-replies emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-replies "expected a terminal emulator" emulator))
    (list-copy (terminal-state-replies emulator)))

  (define (terminal-of buffer)
    (find (lambda (state) (eq? (terminal-state-buffer state) buffer))
          terminals))

  (define (terminal-cursor-position state)
    (cons (+ (if (terminal-state-main-screen state)
                 0 (length (terminal-state-history state)))
             (terminal-state-row state))
          (min (- (terminal-state-cols state) 1)
               (terminal-state-col state))))

  (define (terminal-follow! state)
    (terminal-state-unfollowed-windows-set!
      state (remq (selected-window)
                  (terminal-state-unfollowed-windows state)))
    (goto-point! (terminal-cursor-position state)))

  (define (terminal-scroll! state direction fraction)
    (unless (memq (selected-window)
                  (terminal-state-unfollowed-windows state))
      (terminal-state-unfollowed-windows-set!
        state (cons (selected-window)
                    (terminal-state-unfollowed-windows state))))
    (page-window-fraction! direction fraction)
    (set-point-without-scroll! (terminal-cursor-position state))
    (when (point-visible?)
      (terminal-state-unfollowed-windows-set!
        state (remq (selected-window)
                    (terminal-state-unfollowed-windows state)))))

  (define (blank-line cols) (make-vector cols " "))

  (define (left-bound state)
    (if (terminal-state-margin-mode state)
        (terminal-state-left-margin state) 0))

  (define (right-bound state)
    (if (terminal-state-margin-mode state)
        (terminal-state-right-margin state)
        (- (terminal-state-cols state) 1)))

  (define (erase-row-range! state row start end)
    (let ([line (vector-ref (terminal-state-screen state) row)]
          [styles (vector-ref (terminal-state-styles state) row)])
      (do ([col start (+ col 1)]) ((= col end))
        (clear-cell! line styles col (terminal-state-style state))
        (vector-set! styles col (terminal-state-style state)))))
  (define (blank-styles cols style) (make-vector cols style))

  (define (cell-row->string row)
    (apply string-append (vector->list row)))

  (define (placeholder-line row)
    (make-string (vector-length row) #\space))

  (define (default-tab-stops cols)
    (let ([stops (make-vector cols #f)])
      (do ([col 8 (+ col 8)]) ((>= col cols) stops)
        (vector-set! stops col #t))))

  (define (copy-vector-range! source source-start target target-start count)
    (if (<= target-start source-start)
        (do ([index 0 (+ index 1)]) ((= index count))
          (vector-set! target (+ target-start index)
                       (vector-ref source (+ source-start index))))
        (do ([index (- count 1) (- index 1)]) ((< index 0))
          (vector-set! target (+ target-start index)
                       (vector-ref source (+ source-start index))))))

  (define (make-screen rows cols)
    (let ([screen (make-vector rows)])
      (do ([row 0 (+ row 1)]) ((= row rows) screen)
        (vector-set! screen row (blank-line cols)))))

  (define (make-style-screen rows cols style)
    (let ([screen (make-vector rows)])
      (do ([row 0 (+ row 1)]) ((= row rows) screen)
        (vector-set! screen row (blank-styles cols style)))))

  (define (resized-flags old rows)
    (let ([new (make-vector rows #f)])
      (do ([index 0 (+ index 1)]) ((= index (min rows (vector-length old))) new)
        (vector-set! new index (vector-ref old index)))))

  (define (resized-screen old old-rows old-cols rows cols)
    (let ([new (make-screen rows cols)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (copy-vector-range! (vector-ref old row) 0
                            (vector-ref new row) 0 copy-cols))))

  (define (resized-styles old old-rows old-cols rows cols style)
    (let ([new (make-style-screen rows cols style)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (copy-vector-range! (vector-ref old row) 0
                            (vector-ref new row) 0 copy-cols))))

  (define (resized-tab-stops old old-cols cols)
    (let ([new (default-tab-stops cols)])
      (do ([col 0 (+ col 1)]) ((= col (min old-cols cols)) new)
        (vector-set! new col (vector-ref old col)))))

  (define (clamp value low high) (min high (max low value)))

  (define (save-cursor! state)
    (terminal-state-saved-row-set! state (terminal-state-row state))
    (terminal-state-saved-col-set! state (terminal-state-col state))
    (terminal-state-saved-state-set!
      state
      (list (terminal-state-sgr state)
            (terminal-state-style state)
            (terminal-state-charset state)
            (terminal-state-charset-g1 state)
            (terminal-state-shift state)
            (terminal-state-origin state)
            (terminal-state-autowrap state)
            (terminal-state-wrap-pending state))))

  (define (restore-cursor! state)
    (terminal-state-row-set!
      state (clamp (terminal-state-saved-row state)
                   0 (- (terminal-state-rows state) 1)))
    (terminal-state-col-set!
      state (clamp (terminal-state-saved-col state)
                   0 (- (terminal-state-cols state) 1)))
    (when (terminal-state-saved-state state)
      (let ([saved (terminal-state-saved-state state)])
        (terminal-state-sgr-set! state (list-ref saved 0))
        (terminal-state-style-set! state (list-ref saved 1))
        (terminal-state-charset-set! state (list-ref saved 2))
        (terminal-state-charset-g1-set! state (list-ref saved 3))
        (terminal-state-shift-set! state (list-ref saved 4))
        (terminal-state-origin-set! state (list-ref saved 5))
        (terminal-state-autowrap-set! state (list-ref saved 6))
        (terminal-state-wrap-pending-set! state (list-ref saved 7)))))

  (define (capture-screen-state state)
    (list (terminal-state-row state)
          (terminal-state-col state)
          (terminal-state-saved-row state)
          (terminal-state-saved-col state)
          (terminal-state-saved-state state)
          (terminal-state-scroll-top state)
          (terminal-state-scroll-bottom state)
          (terminal-state-charset state)
          (terminal-state-charset-g1 state)
          (terminal-state-charset-target state)
          (terminal-state-shift state)
          (terminal-state-wrap-pending state)
          (terminal-state-autowrap state)
          (terminal-state-origin state)
          (terminal-state-insert state)
          (terminal-state-sgr state)
          (terminal-state-style state)
          (vector-copy (terminal-state-tab-stops state))
          (terminal-state-last-character state)
          (terminal-state-left-margin state)
          (terminal-state-right-margin state)
          (terminal-state-margin-mode state)))

  (define (restore-screen-state! state saved)
    (terminal-state-row-set!
      state (clamp (list-ref saved 0) 0 (- (terminal-state-rows state) 1)))
    (terminal-state-col-set!
      state (clamp (list-ref saved 1) 0 (- (terminal-state-cols state) 1)))
    (terminal-state-saved-row-set! state (list-ref saved 2))
    (terminal-state-saved-col-set! state (list-ref saved 3))
    (terminal-state-saved-state-set! state (list-ref saved 4))
    (let* ([rows (terminal-state-rows state)]
           [top (clamp (list-ref saved 5) 0 (- rows 1))]
           [bottom (clamp (list-ref saved 6) 0 (- rows 1))])
      (terminal-state-scroll-top-set! state (if (< top bottom) top 0))
      (terminal-state-scroll-bottom-set!
        state (if (< top bottom) bottom (- rows 1))))
    (terminal-state-charset-set! state (list-ref saved 7))
    (terminal-state-charset-g1-set! state (list-ref saved 8))
    (terminal-state-charset-target-set! state (list-ref saved 9))
    (terminal-state-shift-set! state (list-ref saved 10))
    (terminal-state-wrap-pending-set! state (list-ref saved 11))
    (terminal-state-autowrap-set! state (list-ref saved 12))
    (terminal-state-origin-set! state (list-ref saved 13))
    (terminal-state-insert-set! state (list-ref saved 14))
    (terminal-state-sgr-set! state (list-ref saved 15))
    (terminal-state-style-set! state (list-ref saved 16))
    (let ([tabs (list-ref saved 17)])
      (terminal-state-tab-stops-set!
        state (resized-tab-stops tabs (vector-length tabs)
                                 (terminal-state-cols state))))
    (terminal-state-last-character-set! state (list-ref saved 18))
    (let* ([cols (terminal-state-cols state)]
           [left (clamp (list-ref saved 19) 0 (- cols 1))]
           [right (clamp (list-ref saved 20) 0 (- cols 1))])
      (terminal-state-left-margin-set! state (if (< left right) left 0))
      (terminal-state-right-margin-set!
        state (if (< left right) right (- cols 1))))
    (terminal-state-margin-mode-set! state (list-ref saved 21)))

  (define (enter-alternate-screen! state clear?)
    (unless (terminal-state-main-screen state)
      (terminal-state-main-screen-set! state (terminal-state-screen state))
      (terminal-state-main-wrapped-set! state (terminal-state-wrapped state))
      (terminal-state-main-styles-set! state (terminal-state-styles state))
      (terminal-state-main-row-set! state (terminal-state-row state))
      (terminal-state-main-col-set! state (terminal-state-col state))
      (terminal-state-main-state-set! state (capture-screen-state state))
      (if (and (not clear?) (terminal-state-alternate-screen state))
          (begin
            (terminal-state-screen-set!
              state (terminal-state-alternate-screen state))
            (terminal-state-wrapped-set!
              state (terminal-state-alternate-wrapped state))
            (terminal-state-styles-set!
              state (terminal-state-alternate-styles state))
            (restore-screen-state! state (terminal-state-alternate-state state)))
          (begin
            (terminal-state-screen-set!
              state (make-screen (terminal-state-rows state)
                                 (terminal-state-cols state)))
            (terminal-state-wrapped-set!
              state (make-vector (terminal-state-rows state) #f))
            (terminal-state-styles-set!
              state (make-style-screen (terminal-state-rows state)
                                       (terminal-state-cols state)
                                       (terminal-state-style state)))
            (terminal-state-row-set! state 0)
            (terminal-state-col-set! state 0)
            (terminal-state-scroll-top-set! state 0)
            (terminal-state-scroll-bottom-set!
              state (- (terminal-state-rows state) 1))
            (terminal-state-origin-set! state #f)
            (terminal-state-wrap-pending-set! state #f)))))

  (define (leave-alternate-screen! state)
    (when (terminal-state-main-screen state)
      (terminal-state-alternate-screen-set! state (terminal-state-screen state))
      (terminal-state-alternate-wrapped-set! state (terminal-state-wrapped state))
      (terminal-state-alternate-styles-set! state (terminal-state-styles state))
      (terminal-state-alternate-state-set! state (capture-screen-state state))
      (terminal-state-screen-set! state (terminal-state-main-screen state))
      (terminal-state-wrapped-set! state (terminal-state-main-wrapped state))
      (terminal-state-styles-set! state (terminal-state-main-styles state))
      (when (terminal-state-main-state state)
        (restore-screen-state! state (terminal-state-main-state state)))
      (terminal-state-main-screen-set! state #f)
      (terminal-state-main-wrapped-set! state #f)
      (terminal-state-main-styles-set! state #f)
      (terminal-state-main-state-set! state #f)))

  (define (meaningful-row-length cells styles wrapped? cursor-column)
    (if wrapped? (vector-length cells)
        (let find ([index (- (vector-length cells) 1)])
          (cond [(< index 0) (or cursor-column 0)]
                [(or (not (string=? (vector-ref cells index) " "))
                     (not (eq? (vector-ref styles index) 'plain)))
                 (max (+ index 1) (or cursor-column 0))]
                [else (find (- index 1))]))))

  (define (terminal-logical-lines cells styles wrapped cursor-row cursor-col)
    (let loop ([row 0] [start 0] [cell-parts '()] [style-parts '()]
               [cell-count 0] [logical-cursor #f] [out '()])
      (let* ([line (vector-ref cells row)]
             [faces (vector-ref styles row)]
             [continues? (vector-ref wrapped row)]
             [wide-wrap-padding?
              (and continues? (< (+ row 1) (vector-length cells))
                   (> (vector-length line) 1)
                   (string=? (vector-ref line (- (vector-length line) 1)) " ")
                   (> (vector-length (vector-ref cells (+ row 1))) 1)
                   (string=? (vector-ref (vector-ref cells (+ row 1)) 1) ""))]
             [row-length (if wide-wrap-padding? (- (vector-length line) 1)
                             (meaningful-row-length
                               line faces continues?
                               (and (= row cursor-row) cursor-col)))]
             [new-cells (append
                          (reverse
                            (vector->list (vector-copy line 0 row-length)))
                          cell-parts)]
             [new-styles (append
                           (reverse
                             (vector->list (vector-copy faces 0 row-length)))
                           style-parts)]
             [logical-cursor
              (if (= row cursor-row)
                  (+ cell-count (min cursor-col row-length))
                  logical-cursor)])
        (if (and continues? (< (+ row 1) (vector-length cells)))
            (loop (+ row 1) start new-cells new-styles
                  (+ cell-count row-length) logical-cursor out)
            (let ([entry (list (list->vector (reverse new-cells))
                               (list->vector (reverse new-styles)) start row
                               logical-cursor)])
              (if (= (+ row 1) (vector-length cells))
                  (reverse (cons entry out))
                  (loop (+ row 1) (+ row 1) '() '() 0 #f
                        (cons entry out))))))))

  (define (list-take items count)
    (if (= count 0) '()
        (cons (car items) (list-take (cdr items) (- count 1)))))

  (define (reflow-logical-lines logical cols)
    (let ([rows '()] [faces '()] [flags '()] [new-cursor #f]
          [new-wrap-pending #f] [output-count 0])
      (for-each
        (lambda (entry)
          (let* ([cells (car entry)] [styles (cadr entry)]
                 [cursor-offset (list-ref entry 4)]
                 [cursor? (not (eq? cursor-offset #f))]
                 [cell-count (vector-length cells)])
            (let chunk ([at 0])
              (let* ([remaining (- cell-count at)]
                     [take0 (min cols remaining)]
                     [take (if (and (> take0 1) (< (+ at take0) cell-count)
                                    (string=? (vector-ref cells (+ at take0)) ""))
                               (- take0 1) take0)]
                     [take (if (= take 0) (min 1 remaining) take)]
                     [line (blank-line cols)]
                     [style-row (blank-styles cols 'plain)]
                     [output-row output-count])
                (when (> take 0)
                  (copy-vector-range! cells at line 0 take)
                  (copy-vector-range! styles at style-row 0 take))
                (when (and cursor? (not new-cursor)
                           (or (< cursor-offset (+ at take))
                               (= (+ at take) cell-count)))
                  (set! new-cursor
                    (cons output-row (min (- cols 1)
                                          (max 0 (- cursor-offset at)))))
                  (set! new-wrap-pending
                    (and (= cursor-offset cell-count) (= take cols))))
                (set! rows (cons line rows))
                (set! faces (cons style-row faces))
                (set! flags (cons (< (+ at take) cell-count) flags))
                (set! output-count (+ output-count 1))
                (when (< (+ at take) cell-count)
                  (chunk (+ at take)))))))
        logical)
      (values (reverse rows) (reverse faces) (reverse flags)
              (or new-cursor '(0 . 0)) new-wrap-pending)))

  (define (reflow-primary-screen! state rows cols)
    (let* ([history-count (length (terminal-state-history state))]
           [all-cells (list->vector
                        (append (terminal-state-history state)
                                (vector->list (terminal-state-screen state))))]
           [all-styles (list->vector
                         (append (terminal-state-history-styles state)
                                 (vector->list (terminal-state-styles state))))]
           [all-wrapped (list->vector
                          (append (terminal-state-history-wrapped state)
                                  (vector->list (terminal-state-wrapped state))))]
           [cursor-row (+ history-count (terminal-state-row state))]
           [cursor-col (+ (terminal-state-col state)
                          (if (terminal-state-wrap-pending state) 1 0))]
           [used-rows
            (let find ([row (- (vector-length all-cells) 1)])
              (if (<= row cursor-row) (+ cursor-row 1)
                  (if (> (meaningful-row-length
                           (vector-ref all-cells row)
                           (vector-ref all-styles row) #f #f)
                         0)
                      (+ row 1)
                      (find (- row 1)))))]
           [all-cells (vector-copy all-cells 0 used-rows)]
           [all-styles (vector-copy all-styles 0 used-rows)]
           [all-wrapped (vector-copy all-wrapped 0 used-rows)])
      (let-values ([(new-cells new-styles new-wrapped cursor wrap-pending?)
                    (reflow-logical-lines
                      (terminal-logical-lines all-cells all-styles all-wrapped
                                              cursor-row
                                              cursor-col)
                      cols)])
        (let* ([missing (max 0 (- rows (length new-cells)))]
               [new-cells (append new-cells
                                  (map (lambda (ignored) (blank-line cols))
                                       (iota missing)))]
               [new-styles (append new-styles
                                   (map (lambda (ignored)
                                          (blank-styles cols 'plain))
                                        (iota missing)))]
               [new-wrapped (append new-wrapped (make-list missing #f))]
               [history-count (max 0 (- (length new-cells) rows))]
               [history-count (min history-count (terminal-scrollback))]
               [display-start (- (length new-cells) rows)])
          (terminal-state-history-set!
            state (list-tail (list-take new-cells display-start)
                             (max 0 (- display-start history-count))))
          (terminal-state-history-styles-set!
            state (list-tail (list-take new-styles display-start)
                             (max 0 (- display-start history-count))))
          (terminal-state-history-wrapped-set!
            state (list-tail (list-take new-wrapped display-start)
                             (max 0 (- display-start history-count))))
          (terminal-state-screen-set!
            state (list->vector (list-tail new-cells display-start)))
          (terminal-state-styles-set!
            state (list->vector (list-tail new-styles display-start)))
          (terminal-state-wrapped-set!
            state (list->vector (list-tail new-wrapped display-start)))
          (terminal-state-row-set!
            state (clamp (- (car cursor) display-start) 0 (- rows 1)))
          (terminal-state-col-set! state (cdr cursor))
          (terminal-state-wrap-pending-set! state wrap-pending?)))))

  (define (reflow-saved-primary! state rows cols)
    (let ([alternate-screen (terminal-state-screen state)]
          [alternate-wrapped (terminal-state-wrapped state)]
          [alternate-styles (terminal-state-styles state)]
          [alternate-state (capture-screen-state state)])
      (terminal-state-screen-set! state (terminal-state-main-screen state))
      (terminal-state-wrapped-set! state (terminal-state-main-wrapped state))
      (terminal-state-styles-set! state (terminal-state-main-styles state))
      (restore-screen-state! state (terminal-state-main-state state))
      (reflow-primary-screen! state rows cols)
      (terminal-state-main-screen-set! state (terminal-state-screen state))
      (terminal-state-main-wrapped-set! state (terminal-state-wrapped state))
      (terminal-state-main-styles-set! state (terminal-state-styles state))
      (terminal-state-main-row-set! state (terminal-state-row state))
      (terminal-state-main-col-set! state (terminal-state-col state))
      (terminal-state-main-state-set! state (capture-screen-state state))
      (terminal-state-screen-set! state alternate-screen)
      (terminal-state-wrapped-set! state alternate-wrapped)
      (terminal-state-styles-set! state alternate-styles)
      (restore-screen-state! state alternate-state)))

  (define (resize-screen! state rows cols)
    (let ([changed? (or (not (= rows (terminal-state-rows state)))
                        (not (= cols (terminal-state-cols state))))])
      (when changed?
        (if (not (terminal-state-main-screen state))
          (let ([old-cols (terminal-state-cols state)])
            (reflow-primary-screen! state rows cols)
            (terminal-state-tab-stops-set!
              state
              (resized-tab-stops (terminal-state-tab-stops state)
                                 old-cols cols))
            (terminal-state-rows-set! state rows)
            (terminal-state-cols-set! state cols))
          (let* ([old-rows (terminal-state-rows state)]
                 [old-cols (terminal-state-cols state)]
                 [new (resized-screen (terminal-state-screen state)
                                      old-rows old-cols rows cols)]
                 [new-styles (resized-styles (terminal-state-styles state)
                                             old-rows old-cols rows cols
                                             (terminal-state-style state))])
            (reflow-saved-primary! state rows cols)
            (when (terminal-state-alternate-screen state)
              (terminal-state-alternate-screen-set!
                state (resized-screen (terminal-state-alternate-screen state)
                                      old-rows old-cols rows cols)))
            (when (terminal-state-alternate-wrapped state)
              (terminal-state-alternate-wrapped-set!
                state (resized-flags
                        (terminal-state-alternate-wrapped state) rows)))
            (when (terminal-state-alternate-styles state)
              (terminal-state-alternate-styles-set!
                state (resized-styles (terminal-state-alternate-styles state)
                                      old-rows old-cols rows cols
                                      (terminal-state-style state))))
            (terminal-state-screen-set! state new)
            (terminal-state-wrapped-set!
              state (resized-flags (terminal-state-wrapped state) rows))
            (terminal-state-styles-set! state new-styles)
            (terminal-state-tab-stops-set!
              state
              (resized-tab-stops (terminal-state-tab-stops state) old-cols cols))
            (terminal-state-rows-set! state rows)
            (terminal-state-cols-set! state cols)
            (terminal-state-row-set! state
                                     (clamp (terminal-state-row state) 0 (- rows 1)))
            (terminal-state-col-set! state
                                     (clamp (terminal-state-col state) 0 (- cols 1)))
            (terminal-state-wrap-pending-set! state #f)
            (terminal-state-scroll-top-set! state 0)
            (terminal-state-scroll-bottom-set! state (- rows 1))
            (terminal-state-dirty-set! state #t)))
        (terminal-state-scroll-top-set! state 0)
        (terminal-state-scroll-bottom-set! state (- rows 1))
        (terminal-state-left-margin-set! state 0)
        (terminal-state-right-margin-set! state (- cols 1))
        (terminal-state-margin-mode-set! state #f)
        (terminal-state-dirty-set! state #t)
        (when (terminal-state-process state)
          (resize-terminal-process!
            (terminal-state-process state) rows cols)))))

  (define (scroll-up! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [wrapped (terminal-state-wrapped state)]
          [top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)]
          [left (left-bound state)]
          [right (right-bound state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (when (and (= left 0) (= right (- cols 1))
                   (= top 0) (= bottom (- (terminal-state-rows state) 1))
                   (not (terminal-state-main-screen state))
                   (> (terminal-scrollback) 0))
          (let ([history (append (terminal-state-history state)
                                 (list (vector-copy (vector-ref screen top))))])
            (terminal-state-history-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history))))
          (let ([history (append (terminal-state-history-styles state)
                                 (list (vector-copy (vector-ref styles top))))])
            (terminal-state-history-styles-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history))))
          (let ([history (append (terminal-state-history-wrapped state)
                                 (list (vector-ref wrapped top)))])
            (terminal-state-history-wrapped-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history)))))
        (do ([row top (+ row 1)]) ((= row bottom))
          (if (and (= left 0) (= right (- cols 1)))
              (begin
                (vector-set! screen row (vector-ref screen (+ row 1)))
                (vector-set! styles row (vector-ref styles (+ row 1)))
                (vector-set! wrapped row (vector-ref wrapped (+ row 1))))
              (begin
                (copy-vector-range! (vector-ref screen (+ row 1)) left
                                    (vector-ref screen row) left
                                    (+ 1 (- right left)))
                (copy-vector-range! (vector-ref styles (+ row 1)) left
                                    (vector-ref styles row) left
                                    (+ 1 (- right left)))
                (vector-set! wrapped row #f))))
        (if (and (= left 0) (= right (- cols 1)))
            (begin
              (vector-set! screen bottom (blank-line cols))
              (vector-set! styles bottom
                           (blank-styles cols (terminal-state-style state))))
            (erase-row-range! state bottom left (+ right 1)))
        (vector-set! wrapped bottom #f))))

  (define (scroll-down! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [wrapped (terminal-state-wrapped state)]
          [top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)]
          [left (left-bound state)]
          [right (right-bound state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (do ([row bottom (- row 1)]) ((= row top))
          (if (and (= left 0) (= right (- cols 1)))
              (begin
                (vector-set! screen row (vector-ref screen (- row 1)))
                (vector-set! styles row (vector-ref styles (- row 1)))
                (vector-set! wrapped row (vector-ref wrapped (- row 1))))
              (begin
                (copy-vector-range! (vector-ref screen (- row 1)) left
                                    (vector-ref screen row) left
                                    (+ 1 (- right left)))
                (copy-vector-range! (vector-ref styles (- row 1)) left
                                    (vector-ref styles row) left
                                    (+ 1 (- right left)))
                (vector-set! wrapped row #f))))
        (if (and (= left 0) (= right (- cols 1)))
            (begin
              (vector-set! screen top (blank-line cols))
              (vector-set! styles top
                           (blank-styles cols (terminal-state-style state))))
            (erase-row-range! state top left (+ right 1)))
        (vector-set! wrapped top #f))))

  (define (line-feed! state)
    (if (= (terminal-state-row state) (terminal-state-scroll-bottom state))
        (scroll-up! state 1)
        (terminal-state-row-set!
          state (min (- (terminal-state-rows state) 1)
                     (+ (terminal-state-row state) 1)))))

  (define (cell-owner-index line col)
    (let find ([index col])
      (cond [(< index 0) #f]
            [(string=? (vector-ref line index) "") (find (- index 1))]
            [else index])))

  (define (regional-indicator-count text)
    (let loop ([characters (string->list text)] [count 0])
      (if (null? characters) count
          (loop (cdr characters)
                (+ count
                   (if (eq? (char-grapheme-break-property (car characters))
                            'Regional_Indicator)
                       1 0))))))

  (define (grapheme-cell-width text)
    (let ([width (fold-left
                   (lambda (current character)
                     (max current (terminal-character-width character)))
                   0 (string->list text))])
      (if (or (>= (regional-indicator-count text) 2)
              (exists (lambda (character) (memv character '(#\xfe0f #\x20e3)))
                      (string->list text)))
          (max 2 width)
          width)))

  (define (put-character! state character)
    ;; VT autowrap is delayed until the next printable character. Cursor
    ;; motion and controls can therefore cancel a pending wrap at the margin.
    (define (previous-cluster line col)
      (let ([index (cell-owner-index line (- col 1))])
        (and index (vector-ref line index))))
    (define (cluster-extension? character line col)
      (let* ([property (char-grapheme-break-property character)]
             [previous (previous-cluster line col)]
             [previous-property
              (and previous (> (string-length previous) 0)
                   (char-grapheme-break-property
                     (string-ref previous (- (string-length previous) 1))))])
        (or (memq property '(Extend ZWJ SpacingMark))
          (eq? previous-property 'Prepend)
          (and (eq? previous-property 'L)
               (memq property '(L V LV LVT)))
          (and (memq previous-property '(LV V))
               (memq property '(V T)))
          (and (memq previous-property '(LVT T)) (eq? property 'T))
          (and (eq? (char-grapheme-break-property character)
                    'Regional_Indicator)
               previous (odd? (regional-indicator-count previous)))
          (and previous (> (string-length previous) 0)
               (char=? (string-ref previous
                                   (- (string-length previous) 1))
                       #\x200d)))))
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [width (terminal-character-width character)])
      (when (or (= width 0)
                (cluster-extension? character line
                                    (terminal-state-col state)))
        (let* ([candidate (if (terminal-state-wrap-pending state)
                              (terminal-state-col state)
                              (- (terminal-state-col state) 1))]
               [col (cell-owner-index line candidate)])
          (if col
              (let* ([old (vector-ref line col)]
                     [updated (string-normalize-nfc
                                (string-append old (string character)))]
                     [old-width (max 1 (grapheme-cell-width old))]
                     [new-width (min (terminal-state-cols state)
                                     (max 1 (grapheme-cell-width updated)))]
                     [extra (- new-width old-width)])
                (vector-set! line col updated)
                (when (and (> extra 0)
                           (<= (+ (terminal-state-col state) extra)
                               (terminal-state-cols state)))
                  (let ([styles (vector-ref (terminal-state-styles state)
                                            (terminal-state-row state))])
                    (do ([index (terminal-state-col state) (+ index 1)])
                        ((= index (+ (terminal-state-col state) extra)))
                      (clear-cell! line styles index (vector-ref styles col))
                      (vector-set! line index "")
                      (vector-set! styles index (vector-ref styles col)))
                    (terminal-state-col-set!
                      state (+ (terminal-state-col state) extra)))))
              (begin
                (vector-set! line (terminal-state-col state)
                             (string-normalize-nfc
                               (string-append " " (string character))))
                (terminal-state-col-set!
                  state (min (- (terminal-state-cols state) 1)
                             (+ (terminal-state-col state) 1))))))
        (set! width 0))
      (when (> width 0)
        (put-spacing-character! state character width))))

  (define (clear-cell! line styles col style)
    (let find ([start col])
      (if (and (> start 0) (string=? (vector-ref line start) ""))
          (find (- start 1))
          (begin
            (vector-set! line start " ")
            (vector-set! styles start style)
            (let loop ([index (+ start 1)])
              (when (and (< index (vector-length line))
                         (string=? (vector-ref line index) ""))
                (vector-set! line index " ")
                (vector-set! styles index style)
                (loop (+ index 1))))))))

  (define (put-spacing-character! state character requested-width)
    (let ([left (left-bound state)] [right (right-bound state)])
      (when (terminal-state-wrap-pending state)
        (terminal-state-wrap-pending-set! state #f)
        (when (terminal-state-autowrap state)
          (vector-set! (terminal-state-wrapped state)
                       (terminal-state-row state) #t)
          (terminal-state-col-set! state left)
          (line-feed! state)))
      (let* ([cols (terminal-state-cols state)]
             [limit (if (<= left (terminal-state-col state) right)
                      (+ right 1) cols)]
             [width (min requested-width (- limit left))])
        (when (and (> width (- limit (terminal-state-col state)))
                (terminal-state-autowrap state))
          (vector-set! (terminal-state-wrapped state)
                       (terminal-state-row state) #t)
          (terminal-state-col-set! state left)
          (line-feed! state))
        (let* ([line (vector-ref (terminal-state-screen state)
                                 (terminal-state-row state))]
               [styles (vector-ref (terminal-state-styles state)
                                   (terminal-state-row state))]
               [col (terminal-state-col state)]
               [limit (if (<= left col right) (+ right 1) cols)]
               [width (min width (- limit col))])
          (when (terminal-state-insert state) (insert-characters! state width))
          (do ([index col (+ index 1)]) ((= index (+ col width)))
            (clear-cell! line styles index (terminal-state-style state)))
          (vector-set! line col (string character))
          (vector-set! styles col (terminal-state-style state))
          (do ([index (+ col 1) (+ index 1)]) ((= index (+ col width)))
            (vector-set! line index "")
            (vector-set! styles index (terminal-state-style state)))
          (if (= (+ col width) limit)
            (begin
              (terminal-state-col-set! state (- limit 1))
              (terminal-state-wrap-pending-set! state #t))
            (terminal-state-col-set! state (+ col width)))))
      (terminal-state-last-character-set! state character)
      (void)))

  (define (erase-line! state start end)
    (let ([line (vector-ref (terminal-state-screen state)
                            (terminal-state-row state))]
          [styles (vector-ref (terminal-state-styles state)
                              (terminal-state-row state))])
      (do ([col (max 0 start) (+ col 1)])
          ((>= col (min end (terminal-state-cols state))))
        (clear-cell! line styles col (terminal-state-style state))
        (vector-set! styles col (terminal-state-style state)))))

  (define (clear-rows! state start end)
    (do ([row (max 0 start) (+ row 1)])
        ((>= row (min end (terminal-state-rows state))))
      (vector-set! (terminal-state-screen state) row
                   (blank-line (terminal-state-cols state)))
      (vector-set! (terminal-state-wrapped state) row #f)
      (vector-set! (terminal-state-styles state) row
                   (blank-styles (terminal-state-cols state)
                                 (terminal-state-style state)))))

  (define (parameter-list text)
    (let* ([plain (if (and (> (string-length text) 0)
                           (memv (string-ref text 0) '(#\? #\> #\!)))
                      (substring text 1 (string-length text)) text)]
           [parts (split-lines
                    (list->string
                      (map (lambda (c) (if (char=? c #\;) #\newline c))
                           (string->list plain))))])
      (map (lambda (part) (or (string->number part) 0)) parts)))

  (define (split-parameter part separator)
    (let loop ([chars (string->list part)] [field '()] [out '()])
      (cond
        [(null? chars)
         (reverse (cons (list->string (reverse field)) out))]
        [(char=? (car chars) separator)
         (loop (cdr chars) '()
               (cons (list->string (reverse field)) out))]
        [else (loop (cdr chars) (cons (car chars) field) out)])))

  (define (sgr-parameter-list text)
    ;; ISO 8613-6 permits colon-delimited color subparameters. Normalize
    ;; 38:5:n and 38:2:[colorspace:]r:g:b (and their 48 background forms) to
    ;; the semicolon form understood by the canonical SGR state machine.
    (apply append
      (map (lambda (part)
             (let ([fields (split-parameter part #\:)])
               (if (null? (cdr fields))
                   (list (or (string->number part) 0))
                   (let ([values (map (lambda (field)
                                        (and (not (string=? field ""))
                                             (string->number field)))
                                      fields)])
                     (cond
                       [(and (memv (car values) '(38 48))
                             (pair? (cdr values))
                             (= (cadr values) 5))
                        (list (car values) 5 (or (caddr values) 0))]
                       [(and (memv (car values) '(38 48))
                             (pair? (cdr values))
                             (= (cadr values) 2))
                        (let ([rgb (filter number? (cddr values))])
                          (if (>= (length rgb) 3)
                              (list (car values) 2
                                    (list-ref rgb (- (length rgb) 3))
                                    (list-ref rgb (- (length rgb) 2))
                                    (list-ref rgb (- (length rgb) 1)))
                              '(0)))]
                       [else (list (or (car values) 0))])))))
           (split-parameter text #\;))))

  (define (param parameters index default)
    (let ([value (and (< index (length parameters))
                      (list-ref parameters index))])
      (if (or (not value) (= value 0)) default value)))

  (define (terminal-reply! state text)
    (terminal-state-replies-set!
      state (append (terminal-state-replies state) (list text)))
    (when (terminal-state-process state)
      (let ([output (terminal-process-output (terminal-state-process state))])
        (put-bytevector output (string->utf8 text))
        (flush-output-port output))))

  (define (primary-device-attributes! state)
    ;; VT100 with advanced video: matches xterm-256color's terminfo probe.
    (terminal-reply! state "\x1b;[?1;2c"))

  (define (hex-component text)
    (and (> (string-length text) 0)
         (let ([value (string->number text 16)]
               [maximum (- (expt 16 (string-length text)) 1)])
           (and value (inexact->exact (round (* 255 (/ value maximum))))))))

  (define (parse-osc-color text)
    (cond
      [(string-prefix? "rgb:" text)
       (let ([parts (split-parameter
                      (substring text 4 (string-length text)) #\/)])
         (and (= (length parts) 3)
              (let ([values (map hex-component parts)])
                (and (for-all integer? values) values))))]
      [(and (= (string-length text) 7) (char=? (string-ref text 0) #\#))
       (let ([values
              (map (lambda (start)
                     (hex-component (substring text start (+ start 2))))
                   '(1 3 5))])
         (and (for-all integer? values) values))]
      [else #f]))

  (define (osc-color-text color)
    (define (component value)
      (let ([hex (format "~x" value)])
        (let ([byte (if (= (string-length hex) 1)
                        (string-append "0" hex) hex)])
          (string-append byte byte))))
    (format "rgb:~a/~a/~a"
            (component (car color))
            (component (cadr color))
            (component (caddr color))))

  (define (default-color state foreground?)
    (or (if foreground?
            (terminal-state-default-foreground state)
            (terminal-state-default-background state))
        (if foreground? '(0 0 0) '(255 255 255))))

  (define (set-default-color! state foreground? specification)
    (cond
      [(string=? specification "?")
       (terminal-reply!
         state
         (format "\x1b;]~a;~a\x1b;\\"
                 (if foreground? 10 11)
                 (osc-color-text (default-color state foreground?))))]
      [(parse-osc-color specification) =>
       (lambda (color)
         (if foreground?
             (terminal-state-default-foreground-set! state color)
             (terminal-state-default-background-set! state color))
         (terminal-state-dirty-set! state #t))]))

  (define (dispatch-palette! state fields)
    (let ([palette (terminal-state-palette state)])
      (let loop ([fields fields])
        (when (and (pair? fields) (pair? (cdr fields)))
          (let ([index (string->number (car fields))]
                [specification (cadr fields)])
            (when (and (integer? index) (<= 0 index 255))
              (if (string=? specification "?")
                  (terminal-reply!
                    state
                    (format "\x1b;]4;~a;~a\x1b;\\"
                            index
                            (osc-color-text
                              (or (vector-ref palette index)
                                  (vector-ref (make-default-palette) index)))))
                  (cond [(parse-osc-color specification) =>
                         (lambda (color)
                           (vector-set! palette index color)
                           (terminal-state-dirty-set! state #t))])))
            (loop (cddr fields)))))))

  (define (dispatch-osc! state)
    (let* ([text (terminal-state-osc-text state)]
           [fields (split-parameter text #\;)])
      (cond
        [(and (pair? fields) (string=? (car fields) "4"))
         (dispatch-palette! state (cdr fields))]
        [(and (= (length fields) 2) (string=? (car fields) "10"))
         (set-default-color! state #t (cadr fields))]
        [(and (= (length fields) 2) (string=? (car fields) "11"))
         (set-default-color! state #f (cadr fields))]
        [(and (pair? fields) (string=? (car fields) "104"))
         (if (null? (cdr fields))
             (vector-fill! (terminal-state-palette state) #f)
             (for-each
               (lambda (field)
                 (let ([index (string->number field)])
                   (when (and (integer? index) (<= 0 index 255))
                     (vector-set! (terminal-state-palette state) index #f))))
               (cdr fields)))
         (terminal-state-dirty-set! state #t)]
        [(string=? text "110")
         (terminal-state-default-foreground-set! state #f)
         (terminal-state-dirty-set! state #t)]
        [(string=? text "111")
         (terminal-state-default-background-set! state #f)
         (terminal-state-dirty-set! state #t)]
        [(and (> (string-length text) 2)
              (memv (string-ref text 0) '(#\0 #\1 #\2))
              (char=? (string-ref text 1) #\;))
         (let ([title (substring text 2 (string-length text))])
           (unless (or (string=? title "") (not (terminal-state-buffer state)))
             (set-buffer-name! (terminal-state-buffer state)
                               (format "*~a*" title))))]
        [else (void)])))

  (define (hex-string text)
    (apply string-append
      (map (lambda (character)
             (let ([hex (format "~x" (char->integer character))])
               (if (= (string-length hex) 1)
                   (string-append "0" hex) hex)))
           (string->list text))))

  (define (unhex-string text)
    (and (even? (string-length text))
         (let loop ([at 0] [characters '()])
           (if (= at (string-length text))
               (list->string (reverse characters))
               (let ([value (string->number (substring text at (+ at 2)) 16)])
                 (and value
                      (loop (+ at 2)
                            (cons (integer->char value) characters))))))))

  (define terminal-capabilities
    '(("TN" . "xterm-256color")
      ("Co" . "256")
      ("RGB" . "8")
      ("colors" . "256")
      ("pairs" . "65536")
      ("Tc" . #t)))

  (define (reply-terminal-capability! state encoded-name)
    (let* ([name (unhex-string encoded-name)]
           [entry (and name (assoc name terminal-capabilities))])
      (terminal-reply!
        state
        (if entry
            (format "\x1b;P1+r~a~a\x1b;\\"
                    encoded-name
                    (if (eq? (cdr entry) #t) ""
                        (string-append "=" (hex-string (cdr entry)))))
            (format "\x1b;P0+r~a\x1b;\\" encoded-name)))))

  (define (dispatch-dcs! state)
    (let ([text (terminal-state-osc-text state)])
      (cond
        [(string-prefix? "$q" text)
         (let* ([request (substring text 2 (string-length text))]
                [value
                 (cond
                   [(string=? request "m")
                    (format "~am"
                            (if (string=? (terminal-state-sgr state) "")
                                "0" (terminal-state-sgr state)))]
                   [(string=? request "r")
                    (format "~a;~ar"
                            (+ (terminal-state-scroll-top state) 1)
                            (+ (terminal-state-scroll-bottom state) 1))]
                   [(string=? request "s")
                    (format "~a;~as"
                            (+ (terminal-state-left-margin state) 1)
                            (+ (terminal-state-right-margin state) 1))]
                   [else #f])])
           (terminal-reply!
             state
             (format "\x1b;P~a$r~a\x1b;\\"
                     (if value 1 0) (or value request))))]
        [(string-prefix? "+q" text)
         (for-each
           (lambda (name) (reply-terminal-capability! state name))
           (split-parameter
             (substring text 2 (string-length text)) #\;))]
        [else (void)])))

  (define (normalize-cell-row! cells styles)
    (let ([cols (vector-length cells)])
      (do ([col 0 (+ col 1)]) ((= col cols))
        (let ([cell (vector-ref cells col)])
          (cond [(string=? cell "")
                 (when (or (= col 0)
                           (< (grapheme-cell-width
                                (vector-ref cells (- col 1))) 2))
                   (vector-set! cells col " "))]
                [(>= (grapheme-cell-width cell) 2)
                 (if (= (+ col 1) cols)
                     (vector-set! cells col " ")
                     (begin
                       (vector-set! cells (+ col 1) "")
                       (vector-set! styles (+ col 1)
                                    (vector-ref styles col))))])))))

  (define (delete-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [end (if (<= (left-bound state) col (right-bound state))
                    (+ (right-bound state) 1)
                    (terminal-state-cols state))]
           [count (min count (- end col))])
      (copy-vector-range! line (+ col count) line col (- end col count))
      (copy-vector-range! styles (+ col count) styles col (- end col count))
      (do ([i (- end count) (+ i 1)]) ((= i end))
        (vector-set! line i " ")
        (vector-set! styles i (terminal-state-style state)))
      (normalize-cell-row! line styles)))

  (define (insert-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [end (if (<= (left-bound state) col (right-bound state))
                    (+ (right-bound state) 1)
                    (terminal-state-cols state))]
           [count (min count (- end col))])
      (do ([i (- end 1) (- i 1)]) ((< i (+ col count)))
        (vector-set! line i (vector-ref line (- i count)))
        (vector-set! styles i (vector-ref styles (- i count))))
      (do ([i col (+ i 1)]) ((= i (+ col count)))
        (vector-set! line i " ")
        (vector-set! styles i (terminal-state-style state)))
      (normalize-cell-row! line styles)))

  (define (sgr-style sequence)
    (if (string=? sequence "")
        'plain
        (with-mutex style-lock
          (or (hashtable-ref style-cache sequence #f)
              (let ([name (string->symbol
                            (format "terminal-sgr-~a" style-serial))])
                (set! style-serial (+ style-serial 1))
                ;; set-style! accepts SGR parameters, not a complete escape
                ;; sequence. Passing CSI here produced CSI CSI ... m m; the
                ;; second trailing `m` was painted as text by the host
                ;; terminal, most visibly during top's frequent SGR changes.
                (set-style! name (format "0;~a" sequence))
                (hashtable-set! style-cache sequence name)
                (hashtable-set! style-sequences name sequence)
                name)))))

  (define (reversed-style style)
    (let* ([sequence
            (if (eq? style 'plain) ""
                (with-mutex style-lock
                  (hashtable-ref style-sequences style #f)))]
           [operations (and sequence
                            (sgr-operations (parameter-list sequence)))])
      (if (not operations) style
          (let* ([has-reverse?
                  (exists (lambda (operation)
                            (eq? (sgr-category operation) 'reverse))
                          operations)]
                 [updated
                  (if has-reverse?
                      (remp (lambda (operation)
                              (eq? (sgr-category operation) 'reverse))
                            operations)
                      (append operations '((7))))])
            (sgr-style
              (string-join (map number->string (apply append updated)) ";"))))))

  (define (palette-index operation foreground?)
    (let ([code (car operation)])
      (cond
        [(and foreground? (<= 30 code 37)) (- code 30)]
        [(and foreground? (<= 90 code 97)) (+ 8 (- code 90))]
        [(and (not foreground?) (<= 40 code 47)) (- code 40)]
        [(and (not foreground?) (<= 100 code 107)) (+ 8 (- code 100))]
        [(and (= code (if foreground? 38 48))
              (= (length operation) 3) (= (cadr operation) 5))
         (caddr operation)]
        [else #f])))

  (define (rgb-operation foreground? color)
    (append (list (if foreground? 38 48) 2) color))

  (define (resolved-style state style)
    (let* ([sequence
            (if (eq? style 'plain) ""
                (with-mutex style-lock
                  (hashtable-ref style-sequences style "")))]
           [operations (if (string=? sequence "") '()
                           (sgr-operations (parameter-list sequence)))]
           [palette (terminal-state-palette state)]
           [resolved
            (map (lambda (operation)
                   (let* ([foreground-index
                           (palette-index operation #t)]
                          [background-index
                           (palette-index operation #f)]
                          [index (or foreground-index background-index)]
                          [color (and index (vector-ref palette index))])
                     (cond
                       [color
                        (rgb-operation (and foreground-index #t) color)]
                       [(and (= (car operation) 39)
                             (terminal-state-default-foreground state))
                        (rgb-operation
                          #t (terminal-state-default-foreground state))]
                       [(and (= (car operation) 49)
                             (terminal-state-default-background state))
                        (rgb-operation
                          #f (terminal-state-default-background state))]
                       [else operation])))
                 operations)]
           [resolved
            (if (and (terminal-state-default-foreground state)
                     (not (exists (lambda (operation)
                                    (eq? (sgr-category operation) 'foreground))
                                  resolved)))
                (append resolved
                        (list (rgb-operation
                                #t (terminal-state-default-foreground state))))
                resolved)]
           [resolved
            (if (and (terminal-state-default-background state)
                     (not (exists (lambda (operation)
                                    (eq? (sgr-category operation) 'background))
                                  resolved)))
                (append resolved
                        (list (rgb-operation
                                #f (terminal-state-default-background state))))
                resolved)])
      (if (null? resolved) 'plain
          (sgr-style
            (string-join (map number->string (apply append resolved)) ";")))))

  (define (effective-style-row state row)
    (vector-map
      (lambda (style)
        (let ([style (resolved-style state style)])
          (if (terminal-state-reverse-screen state)
              (reversed-style style) style)))
      row))

  (define (effective-style-screen state styles)
    (vector-map (lambda (row) (effective-style-row state row)) styles))

  (define (sgr-operations codes)
    ;; Group extended colors so zero-valued components remain color data.
    (let loop ([xs codes] [out '()])
      (if (null? xs)
          (reverse out)
          (let* ([code (car xs)]
                 [count (if (and (memv code '(38 48)) (pair? (cdr xs)))
                            (case (cadr xs) [(5) 3] [(2) 5] [else 1])
                            1)])
            (let take ([ys xs] [n count] [op '()])
              (if (or (= n 0) (null? ys))
                  (loop ys (cons (reverse op) out))
                  (take (cdr ys) (- n 1) (cons (car ys) op))))))))

  (define (sgr-category op)
    (let ([code (car op)])
      (cond
        [(or (= code 1) (= code 2)) 'intensity]
        [(= code 3) 'italic]
        [(or (= code 4) (= code 21)) 'underline]
        [(or (= code 5) (= code 6)) 'blink]
        [(= code 7) 'reverse]
        [(= code 8) 'hidden]
        [(= code 9) 'strike]
        [(or (= code 38) (<= 30 code 37) (<= 90 code 97)) 'foreground]
        [(or (= code 48) (<= 40 code 47) (<= 100 code 107)) 'background]
        [else code])))

  (define (canonical-sgr current additions)
    (define (remove-category state category)
      (remp (lambda (op) (eqv? (sgr-category op) category)) state))
    (let loop ([ops (append (sgr-operations (parameter-list current))
                            (sgr-operations additions))]
               [state '()])
      (if (null? ops)
          (apply append (reverse state))
          (let* ([op (car ops)] [code (car op)])
            (cond
              [(= code 0) (loop (cdr ops) '())]
              [(= code 22) (loop (cdr ops)
                                 (remove-category state 'intensity))]
              [(memv code '(23 24 25 27 28 29))
               (loop (cdr ops)
                     (remove-category
                       state
                       (case code
                         [(23) 'italic] [(24) 'underline] [(25) 'blink]
                         [(27) 'reverse] [(28) 'hidden] [else 'strike])))]
              [(= code 39) (loop (cdr ops)
                                 (remove-category state 'foreground))]
              [(= code 49) (loop (cdr ops)
                                 (remove-category state 'background))]
              [else
               (let ([category (sgr-category op)])
                 (loop (cdr ops)
                       (cons op (remove-category state category))))])))))

  (define (set-sgr! state text)
    ;; Store the effective face, not a history of every SGR command. This
    ;; makes selective resets exact and keeps emitted sequences bounded.
    (let* ([codes (canonical-sgr (terminal-state-sgr state)
                                 (sgr-parameter-list text))]
           [sequence (string-join (map number->string codes) ";")])
      (terminal-state-sgr-set! state sequence)
      (terminal-state-style-set! state (sgr-style sequence))))

  (define (reset-terminal-state! state)
    (let ([rows (terminal-state-rows state)]
          [cols (terminal-state-cols state)])
      (terminal-state-screen-set! state (make-screen rows cols))
      (terminal-state-wrapped-set! state (make-vector rows #f))
      (terminal-state-styles-set! state (make-style-screen rows cols 'plain))
      (terminal-state-row-set! state 0)
      (terminal-state-col-set! state 0)
      (terminal-state-saved-row-set! state 0)
      (terminal-state-saved-col-set! state 0)
      (terminal-state-saved-state-set! state #f)
      (terminal-state-scroll-top-set! state 0)
      (terminal-state-scroll-bottom-set! state (- rows 1))
      (terminal-state-left-margin-set! state 0)
      (terminal-state-right-margin-set! state (- cols 1))
      (terminal-state-margin-mode-set! state #f)
      (terminal-state-parameters-set! state "")
      (terminal-state-osc-escape-set! state #f)
      (terminal-state-charset-set! state 'ascii)
      (terminal-state-charset-g1-set! state 'ascii)
      (terminal-state-charset-target-set! state 0)
      (terminal-state-shift-set! state 0)
      (terminal-state-wrap-pending-set! state #f)
      (terminal-state-autowrap-set! state #t)
      (terminal-state-origin-set! state #f)
      (terminal-state-insert-set! state #f)
      (terminal-state-reverse-screen-set! state #f)
      (terminal-state-cursor-keys-set! state #f)
      (terminal-state-keypad-set! state #f)
      (terminal-state-meta-eight-bit-set! state #f)
      (terminal-state-cursor-visible-set! state #t)
      (terminal-state-tab-stops-set! state (default-tab-stops cols))
      (terminal-state-last-character-set! state #\space)
      (terminal-state-bell-set! state #f)
      (terminal-state-bell-visible-set! state #f)
      (terminal-state-bell-generation-set! state 0)
      (terminal-state-mouse-set! state #f)
      (terminal-state-mouse-sgr-set! state #f)
      (terminal-state-mouse-utf8-set! state #f)
      (terminal-state-mouse-urxvt-set! state #f)
      (terminal-state-focus-reporting-set! state #f)
      (terminal-state-bracketed-set! state #f)
      (terminal-state-main-screen-set! state #f)
      (terminal-state-main-wrapped-set! state #f)
      (terminal-state-main-styles-set! state #f)
      (terminal-state-main-state-set! state #f)
      (terminal-state-alternate-screen-set! state #f)
      (terminal-state-alternate-wrapped-set! state #f)
      (terminal-state-alternate-styles-set! state #f)
      (terminal-state-alternate-state-set! state #f)
      (terminal-state-history-set! state '())
      (terminal-state-history-wrapped-set! state '())
      (terminal-state-history-styles-set! state '())
      (terminal-state-sgr-set! state "")
      (terminal-state-style-set! state 'plain)
      (terminal-state-palette-set! state (make-vector 256 #f))
      (terminal-state-default-foreground-set! state #f)
      (terminal-state-default-background-set! state #f)
      (terminal-state-dirty-set! state #t)
      (when (terminal-state-buffer state)
        (set-app-presentation! (terminal-state-buffer state)
                               0 #f #f 'blinking-block))))

  (define (dispatch-csi! state final text)
    (unless (char=? final #\m)
      (terminal-state-wrap-pending-set! state #f))
    (let* ([cursor-shape? (and (char=? final #\q)
                               (> (string-length text) 0)
                               (char=? (string-ref text
                                                   (- (string-length text) 1))
                                       #\space))]
           [parameter-text (if cursor-shape?
                               (substring text 0 (- (string-length text) 1))
                               text)]
           [parameters (parameter-list parameter-text)]
           [n (param parameters 0 1)]
           [row (terminal-state-row state)]
           [col (terminal-state-col state)]
           [rows (terminal-state-rows state)]
           [cols (terminal-state-cols state)])
      (if (and (> (string-length text) 0)
               (char=? (string-ref text 0) #\?)
               (memv final '(#\h #\l)))
          (let ([on? (char=? final #\h)])
            (for-each
              (lambda (mode)
                (case mode
                  [(1) (terminal-state-cursor-keys-set! state on?)]
                  [(5)
                   (terminal-state-reverse-screen-set! state on?)
                   (terminal-state-dirty-set! state #t)]
                  [(6)
                   (terminal-state-origin-set! state on?)
                   (terminal-state-row-set!
                     state (if on? (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set! state 0)]
                  [(7) (terminal-state-autowrap-set! state on?)]
                  [(69)
                   (terminal-state-margin-mode-set! state on?)
                   (unless on?
                     (terminal-state-left-margin-set! state 0)
                     (terminal-state-right-margin-set! state (- cols 1)))
                   (terminal-state-row-set!
                     state (if (terminal-state-origin state)
                               (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set! state (if on? (left-bound state) 0))]
                  [(25) (terminal-state-cursor-visible-set! state on?)]
                  [(1000 1002 1003)
                   (if on?
                       (terminal-state-mouse-set! state mode)
                       (when (eqv? (terminal-state-mouse state) mode)
                         (terminal-state-mouse-set! state #f)))]
                  [(1004) (terminal-state-focus-reporting-set! state on?)]
                  [(1005) (terminal-state-mouse-utf8-set! state on?)]
                  [(1006) (terminal-state-mouse-sgr-set! state on?)]
                  [(1015) (terminal-state-mouse-urxvt-set! state on?)]
                  [(1034) (terminal-state-meta-eight-bit-set! state on?)]
                  [(2004) (terminal-state-bracketed-set! state on?)]
                  [(1048) (if on? (save-cursor! state) (restore-cursor! state))]
                  [(47 1047 1049)
                   ;; The alternate and primary screens have unrelated row
                   ;; spaces. A scrollback offset from one is meaningless in
                   ;; the other and can crop a nested full-screen program.
                   (terminal-state-unfollowed-windows-set! state '())
                   (if on?
                       (enter-alternate-screen! state (not (= mode 47)))
                       (leave-alternate-screen! state))
                   (when (terminal-state-buffer state)
                     (reset-buffer-viewports!
                       (terminal-state-buffer state)
                       (terminal-cursor-position state)))]
                  [else (void)]))
              parameters))
          (case final
            [(#\h #\l)
             (when (and (not (string-prefix? "?" text))
                        (memv 4 parameters))
               (terminal-state-insert-set! state (char=? final #\h)))]
            [(#\A)
             (terminal-state-row-set!
               state (max (if (terminal-state-origin state)
                              (terminal-state-scroll-top state) 0)
                          (- row n)))]
            [(#\B #\e)
             (terminal-state-row-set!
               state (min (if (terminal-state-origin state)
                              (terminal-state-scroll-bottom state) (- rows 1))
                          (+ row n)))]
            [(#\C #\a)
             (terminal-state-col-set!
               state (min (if (<= (left-bound state) col (right-bound state))
                              (right-bound state) (- cols 1))
                          (+ col n)))]
            [(#\D)
             (terminal-state-col-set!
               state (max (if (<= (left-bound state) col (right-bound state))
                              (left-bound state) 0)
                          (- col n)))]
            [(#\E) (terminal-state-row-set!
                     state
                     (min (if (terminal-state-origin state)
                              (terminal-state-scroll-bottom state) (- rows 1))
                          (+ row n)))
             (terminal-state-col-set! state (left-bound state))]
            [(#\F) (terminal-state-row-set!
                     state
                     (max (if (terminal-state-origin state)
                              (terminal-state-scroll-top state) 0)
                          (- row n)))
             (terminal-state-col-set! state (left-bound state))]
            [(#\G #\`)
             (terminal-state-col-set!
               state (if (terminal-state-origin state)
                         (clamp (+ (left-bound state) (- n 1))
                                (left-bound state) (right-bound state))
                         (clamp (- n 1) 0 (- cols 1))))]
            [(#\d) (terminal-state-row-set! state
                                            (if (terminal-state-origin state)
                                                (clamp (+ (terminal-state-scroll-top state)
                                                          (- n 1))
                                                       (terminal-state-scroll-top state)
                                                       (terminal-state-scroll-bottom state))
                                                (clamp (- n 1) 0 (- rows 1))))]
            [(#\H #\f)
             (terminal-state-row-set! state
                                      (if (terminal-state-origin state)
                                          (clamp (+ (terminal-state-scroll-top state)
                                                    (- (param parameters 0 1) 1))
                                                 (terminal-state-scroll-top state)
                                                 (terminal-state-scroll-bottom state))
                                          (clamp (- (param parameters 0 1) 1)
                                                 0 (- rows 1))))
             (terminal-state-col-set!
               state (if (terminal-state-origin state)
                         (clamp (+ (left-bound state)
                                   (- (param parameters 1 1) 1))
                                (left-bound state) (right-bound state))
                         (clamp (- (param parameters 1 1) 1) 0 (- cols 1))))]
            [(#\J)
             (case (param parameters 0 0)
               [(0) (erase-line! state col cols)
                (clear-rows! state (+ row 1) rows)]
               [(1) (clear-rows! state 0 row)
                (erase-line! state 0 (+ col 1))]
               [(2 3) (clear-rows! state 0 rows)])]
            [(#\K)
             (case (param parameters 0 0)
               [(0) (erase-line! state col cols)]
               [(1) (erase-line! state 0 (+ col 1))]
               [(2) (erase-line! state 0 cols)])]
            [(#\S) (scroll-up! state n)]
            [(#\T) (scroll-down! state n)]
            [(#\P) (delete-characters! state n)]
            [(#\@) (insert-characters! state n)]
            [(#\X) (erase-line! state col (+ col n))]
            [(#\Z)
             (let loop ([candidate (- col 1)] [left n])
               (cond [(or (< candidate 0) (= left 0))
                      (terminal-state-col-set! state (max 0 candidate))]
                     [(vector-ref (terminal-state-tab-stops state) candidate)
                      (if (= left 1)
                          (terminal-state-col-set! state candidate)
                          (loop (- candidate 1) (- left 1)))]
                     [else (loop (- candidate 1) left)]))]
            [(#\g)
             (case (param parameters 0 0)
               [(0) (vector-set! (terminal-state-tab-stops state) col #f)]
               [(3) (vector-fill! (terminal-state-tab-stops state) #f)])]
            [(#\L)
             (when (<= (terminal-state-scroll-top state) row
                       (terminal-state-scroll-bottom state))
               (let ([old (terminal-state-scroll-top state)])
                 (terminal-state-scroll-top-set! state row)
                 (scroll-down!
                   state (min n (+ 1 (- (terminal-state-scroll-bottom state)
                                        row))))
                 (terminal-state-scroll-top-set! state old)))]
            [(#\M)
             (when (<= (terminal-state-scroll-top state) row
                       (terminal-state-scroll-bottom state))
               (let ([old (terminal-state-scroll-top state)])
                 (terminal-state-scroll-top-set! state row)
                 (scroll-up!
                   state (min n (+ 1 (- (terminal-state-scroll-bottom state)
                                        row))))
                 (terminal-state-scroll-top-set! state old)))]
            [(#\r)
             (let ([top (clamp (- (param parameters 0 1) 1) 0 (- rows 1))]
                   [bottom (clamp (- (param parameters 1 rows) 1)
                                  0 (- rows 1))])
               (when (< top bottom)
                 (terminal-state-scroll-top-set! state top)
                 (terminal-state-scroll-bottom-set! state bottom)))
             (terminal-state-row-set!
               state (if (terminal-state-origin state)
                         (terminal-state-scroll-top state) 0))
             (terminal-state-col-set! state
                                      (if (terminal-state-origin state)
                                          (left-bound state) 0))]
            [(#\s)
             (if (terminal-state-margin-mode state)
                 (let ([left (clamp (- (param parameters 0 1) 1)
                                    0 (- cols 1))]
                       [right (clamp (- (param parameters 1 cols) 1)
                                     0 (- cols 1))])
                   (when (< left right)
                     (terminal-state-left-margin-set! state left)
                     (terminal-state-right-margin-set! state right))
                   (terminal-state-row-set!
                     state (if (terminal-state-origin state)
                               (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set! state (left-bound state)))
                 (save-cursor! state))]
            [(#\u) (restore-cursor! state)]
            [(#\m) (set-sgr! state text)]
            [(#\b)
             (do ([left n (- left 1)])
                 ((= left 0))
               (put-character! state (terminal-state-last-character state)))]
            [(#\p)
             (when (string-prefix? "!" text)
               (terminal-state-origin-set! state #f)
               (terminal-state-autowrap-set! state #t)
               (terminal-state-insert-set! state #f)
               (terminal-state-cursor-keys-set! state #f)
               (terminal-state-keypad-set! state #f)
               (terminal-state-meta-eight-bit-set! state #f)
               (terminal-state-row-set! state 0)
               (terminal-state-col-set! state 0)
               (terminal-state-scroll-top-set! state 0)
               (terminal-state-scroll-bottom-set! state (- rows 1))
               (terminal-state-left-margin-set! state 0)
               (terminal-state-right-margin-set! state (- cols 1))
               (terminal-state-margin-mode-set! state #f)
               (terminal-state-sgr-set! state "")
               (terminal-state-style-set! state 'plain))]
            [(#\n)
             (cond
               [(= (param parameters 0 0) 5)
                (terminal-reply!
                  state (if (string-prefix? "?" text)
                            "\x1b;[?0n" "\x1b;[0n"))]
               [(= (param parameters 0 0) 6)
                (let ([reported-row
                       (if (terminal-state-origin state)
                           (- row (terminal-state-scroll-top state)) row)])
                  (terminal-reply!
                    state
                    (format "\x1b;[~a~a;~aR"
                            (if (string-prefix? "?" text) "?" "")
                            (+ reported-row 1) (+ col 1))))])]
            [(#\c)
             (cond [(or (string=? text "") (string=? text "0"))
                    (primary-device-attributes! state)]
                   [(or (string=? text ">") (string=? text ">0"))
                    (terminal-reply! state "\x1b;[>0;276;0c")])]
            [(#\q)
             (when (and cursor-shape? (terminal-state-buffer state))
               (set-app-presentation!
                 (terminal-state-buffer state) 0 #f #f
                 (case (param parameters 0 0)
                   [(0 1) 'blinking-block]
                   [(2) 'block]
                   [(3) 'blinking-underline]
                   [(4) 'underline]
                   [(5) 'blinking-bar]
                   [(6) 'bar]
                   [else 'blinking-block])))]
            [else (void)]))))

  (define line-drawing
    '((#\_ . #\space) (#\` . #\x25c6) (#\a . #\x2592) (#\f . #\x00b0)
      (#\g . #\x00b1) (#\j . #\x2518) (#\k . #\x2510) (#\l . #\x250c)
      (#\m . #\x2514) (#\n . #\x253c) (#\o . #\x23ba) (#\p . #\x23bb)
      (#\q . #\x2500) (#\r . #\x23bc) (#\s . #\x23bd) (#\t . #\x251c)
      (#\u . #\x2524) (#\v . #\x2534) (#\w . #\x252c) (#\x . #\x2502)
      (#\y . #\x2264) (#\z . #\x2265) (#\{ . #\x03c0) (#\| . #\x2260)
      (#\} . #\x00a3) (#\~ . #\x00b7)))

  (define (mapped-character state character)
    (if (eq? (if (= (terminal-state-shift state) 0)
                 (terminal-state-charset state)
                 (terminal-state-charset-g1 state))
             'line)
        (cond [(assv character line-drawing) => cdr] [else character])
        character))

  (define (next-tab-stop state)
    (let ([cols (terminal-state-cols state)]
          [stops (terminal-state-tab-stops state)])
      (let loop ([col (+ (terminal-state-col state) 1)])
        (cond [(>= col cols) (- cols 1)]
              [(vector-ref stops col) col]
              [else (loop (+ col 1))]))))

  (define (feed-character! state character)
    (case (terminal-state-parser state)
      [(normal)
       (case (char->integer character)
         [(7) (terminal-state-bell-set! state #t)]
         [(8) (terminal-state-wrap-pending-set! state #f)
          (terminal-state-col-set! state
                                   (max (if (<= (left-bound state)
                                                (terminal-state-col state)
                                                (right-bound state))
                                            (left-bound state) 0)
                                        (- (terminal-state-col state) 1)))]
         [(9) (terminal-state-wrap-pending-set! state #f)
          (terminal-state-col-set! state (next-tab-stop state))]
         [(10 11 12) (terminal-state-wrap-pending-set! state #f)
          (vector-set! (terminal-state-wrapped state)
                       (terminal-state-row state) #f)
          (line-feed! state)]
         [(13) (terminal-state-wrap-pending-set! state #f)
          (terminal-state-col-set! state (left-bound state))]
         [(14) (terminal-state-shift-set! state 1)]
         [(15) (terminal-state-shift-set! state 0)]
         [(27) (terminal-state-parser-set! state 'escape)]
         [(132) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state)]                          ; IND
         [(133) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state)                           ; NEL
          (terminal-state-col-set! state (left-bound state))]
         [(136) (vector-set! (terminal-state-tab-stops state)
                             (terminal-state-col state) #t)] ; HTS
         [(141) (if (= (terminal-state-row state)
                       (terminal-state-scroll-top state))
                    (scroll-down! state 1)
                    (terminal-state-row-set!
                      state (max 0 (- (terminal-state-row state) 1))))] ; RI
         [(144)                                      ; DCS
          (terminal-state-parser-set! state 'dcs)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         [(152 158 159)                              ; SOS, PM, APC
          (terminal-state-parser-set! state 'control-string)
          (terminal-state-osc-escape-set! state #f)]
         [(155) (terminal-state-parser-set! state 'csi) ; CSI
          (terminal-state-parameters-set! state "")]
         [(157) (terminal-state-parser-set! state 'osc) ; OSC
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         [else
          (let ([code (char->integer character)])
            (when (and (>= code 32) (not (<= 128 code 159)))
              (put-character! state (mapped-character state character))))])]
      [(escape)
       (case character
         [(#\[) (terminal-state-parser-set! state 'csi)
          (terminal-state-parameters-set! state "")]
         [(#\]) (terminal-state-parser-set! state 'osc)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         ;; String controls carry arbitrary printable payload terminated by
         ;; ST (ESC \). They are metadata/protocol traffic, never screen text.
         [(#\P)
          (terminal-state-parser-set! state 'dcs)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         [(#\X #\^ #\_)
          (terminal-state-parser-set! state 'control-string)
          (terminal-state-osc-escape-set! state #f)]
         [(#\7) (save-cursor! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\8) (restore-cursor! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\D) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state) (terminal-state-parser-set! state 'normal)]
         [(#\E) (vector-set! (terminal-state-wrapped state)
                             (terminal-state-row state) #f)
          (line-feed! state)
          (terminal-state-col-set! state 0)
          (terminal-state-parser-set! state 'normal)]
         [(#\H)
          (vector-set! (terminal-state-tab-stops state)
                       (terminal-state-col state) #t)
          (terminal-state-parser-set! state 'normal)]
         [(#\M) (if (= (terminal-state-row state)
                       (terminal-state-scroll-top state))
                    (scroll-down! state 1)
                    (terminal-state-row-set!
                      state (max 0 (- (terminal-state-row state) 1))))
          (terminal-state-parser-set! state 'normal)]
         [(#\c) (reset-terminal-state! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\Z) (primary-device-attributes! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\=) (terminal-state-keypad-set! state #t)
          (terminal-state-parser-set! state 'normal)]
         [(#\>) (terminal-state-keypad-set! state #f)
          (terminal-state-parser-set! state 'normal)]
         [(#\() (terminal-state-charset-target-set! state 0)
          (terminal-state-parser-set! state 'charset)]
         [(#\)) (terminal-state-charset-target-set! state 1)
          (terminal-state-parser-set! state 'charset)]
         [else (terminal-state-parser-set! state 'normal)])]
      [(charset)
       (let ([designation (if (char=? character #\0) 'line 'ascii)])
         (if (= (terminal-state-charset-target state) 0)
             (terminal-state-charset-set! state designation)
             (terminal-state-charset-g1-set! state designation)))
       (terminal-state-parser-set! state 'normal)]
      [(csi)
       (cond [(memv (char->integer character) '(24 26))
              (terminal-state-parser-set! state 'normal)
              (terminal-state-parameters-set! state "")]
             [(char=? character #\esc)
              (terminal-state-parser-set! state 'escape)
              (terminal-state-parameters-set! state "")]
             [(char<=? #\@ character #\~)
              (dispatch-csi! state character
                             (terminal-state-parameters state))
              (terminal-state-parser-set! state 'normal)]
             [(< (string-length (terminal-state-parameters state)) 1024)
              (terminal-state-parameters-set!
                state (string-append (terminal-state-parameters state)
                                     (string character)))]
             [else
              (terminal-state-parser-set! state 'normal)
              (terminal-state-parameters-set! state "")])]
      [(osc)
       (cond [(memv (char->integer character) '(7 156)) ; BEL or ST
              (dispatch-osc! state)
              (terminal-state-parser-set! state 'normal)]
             [(and (terminal-state-osc-escape state) (char=? character #\\))
              (dispatch-osc! state)
              (terminal-state-parser-set! state 'normal)
              (terminal-state-osc-escape-set! state #f)]
             [else
              (if (char=? character #\esc)
                  (terminal-state-osc-escape-set! state #t)
                  (begin
                    (terminal-state-osc-escape-set! state #f)
                    (if (< (string-length (terminal-state-osc-text state))
                           8192)
                        (terminal-state-osc-text-set!
                          state (string-append (terminal-state-osc-text state)
                                               (string character)))
                        (begin
                          (terminal-state-parser-set! state 'normal)
                          (terminal-state-osc-text-set! state "")))))])]
      [(control-string)
       (cond
         [(memv (char->integer character) '(24 26))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(= (char->integer character) 156) ; ST
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(and (terminal-state-osc-escape state) (char=? character #\\))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [else
          (terminal-state-osc-escape-set! state
                                          (char=? character #\esc))])]
      [(dcs)
       (cond
         [(memv (char->integer character) '(24 26))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         [(= (char->integer character) 156) ; ST
          (dispatch-dcs! state)
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(and (terminal-state-osc-escape state) (char=? character #\\))
          (dispatch-dcs! state)
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [else
          (if (char=? character #\esc)
              (terminal-state-osc-escape-set! state #t)
              (begin
                (terminal-state-osc-escape-set! state #f)
                (if (< (string-length (terminal-state-osc-text state)) 8192)
                    (terminal-state-osc-text-set!
                      state (string-append (terminal-state-osc-text state)
                                           (string character)))
                    (begin
                      (terminal-state-parser-set! state 'normal)
                      (terminal-state-osc-text-set! state "")))))])]))

  (define (refresh-terminal! state)
    (let ([size (buffer-window-size (terminal-state-buffer state))])
      (when size
        (with-mutex (terminal-state-lock state)
          (resize-screen! state (max 1 (car size)) (max 1 (cdr size)))
          (when (terminal-state-dirty state)
            ;; Snapshot cells and faces together while the PTY grid is locked.
            ;; Painting either one live can combine different stages of a
            ;; full-screen application's redisplay into one torn frame.
            (let ([cells
                   (append (if (terminal-state-main-screen state)
                               '() (terminal-state-history state))
                           (vector->list (terminal-state-screen state)))]
                  [styles
                   (map (lambda (row) (effective-style-row state row))
                        (append (if (terminal-state-main-screen state)
                                    '() (terminal-state-history-styles state))
                                (vector->list
                                  (terminal-state-styles state))))])
              (view-replace! (terminal-state-buffer state)
                             (map placeholder-line cells))
              (terminal-state-rendered-cells-set!
                state (list->vector (map vector-copy cells)))
              (terminal-state-rendered-styles-set!
                state (list->vector (map vector-copy styles)))
              ;; Cell/style rows are dynamic renderer data; their structural
              ;; placeholder lines often remain identical across frames.
              (view-invalidate! (terminal-state-buffer state)))
            (terminal-state-dirty-set! state #f))
          (when (and (eq? (current-buffer) (terminal-state-buffer state))
                     (not (memq (selected-window)
                                (terminal-state-unfollowed-windows state))))
            (goto-point! (terminal-cursor-position state)))))))

  (define (terminal-row-styles buffer row line)
    (let ([state (terminal-of buffer)])
      (and state (terminal-state-rendered-styles state)
           (< row (vector-length (terminal-state-rendered-styles state)))
           (vector-ref (terminal-state-rendered-styles state) row))))

  (define (terminal-row-render buffer row line)
    (let ([state (terminal-of buffer)])
      (and state (terminal-state-rendered-cells state)
           (< row (vector-length (terminal-state-rendered-cells state)))
           (vector-ref (terminal-state-rendered-cells state) row))))

  (define (materialize-terminal-transcript! state)
    (when (terminal-state-rendered-cells state)
      (view-replace!
        (terminal-state-buffer state)
        (map cell-row->string
             (vector->list (terminal-state-rendered-cells state))))
      (terminal-state-rendered-cells-set! state #f)
      (terminal-state-rendered-styles-set! state #f)))

  (define (reader-loop state)
    (define (display-redraw!)
      (parameterize ([terminal-output-port (terminal-state-display state)])
        (redraw!)))
    (define (show-bell!)
      (let ([generation
             (with-mutex (terminal-state-lock state)
               (let ([next (+ (terminal-state-bell-generation state) 1)])
                 (terminal-state-bell-generation-set! state next)
                 (terminal-state-bell-visible-set! state #t)
                 next))])
        (fork-thread
          (lambda ()
            ;; A status glyph needs longer than a video flash to be legible.
            (sleep (make-time 'time-duration 500000000 0))
            (let ([clear?
                   (with-mutex (terminal-state-lock state)
                     (and (= generation
                             (terminal-state-bell-generation state))
                          (begin
                            (terminal-state-bell-visible-set! state #f)
                            #t)))])
              (when (and clear? (terminal-state-alive state))
                (guard (ex [else (void)]) (display-redraw!))))))))
    (define (finished!)
      (terminal-state-alive-set! state #f)
      (guard (ex [else (void)])
        (set-app-capture! (terminal-state-buffer state) #f))
      (guard (ex [else (void)])
        (set-buffer-wrap! (terminal-state-buffer state) #f))
      (guard (ex [else (void)]) (display-redraw!))
      (guard (ex [else (void)]) (materialize-terminal-transcript! state))
      (guard (ex [else (void)])
        (detach-app! (terminal-state-buffer state)))
      ;; Publish the dead state before waiting for the session leader.  A
      ;; platform-specific wait must never make the editor appear frozen.
      (guard (ex [else (void)]) (display-redraw!))
      (guard (ex [else (void)])
        (reap-terminal-process! (terminal-state-process state)))
      (guard (ex [else (void)])
        (close-port (terminal-state-display state))))
    (guard (ex [else
                ;; Linux reports PTY-master closure as EIO rather than EOF.
                ;; Other reader failures indicate an emulator or redraw bug
                ;; and must not masquerade as an ordinary process exit.
                (unless (i/o-read-error? ex)
                  (parameterize ([message-source 'terminal])
                    (set-message!
                      (format "Terminal reader failed: ~a" (error-text ex)))))
                (finished!)])
      (let ([input (transcoded-port
                     (terminal-process-input (terminal-state-process state))
                     (make-transcoder (utf-8-codec) 'none 'replace))])
        (let loop ()
          (let ([character (get-char input)])
            (if (eof-object? character)
                (finished!)
                (begin
                  (let ([bell?
                         (with-mutex (terminal-state-lock state)
                           (feed-character! state character)
                           (let drain ([remaining 4095])
                             (when (and (> remaining 0) (char-ready? input))
                               (let ([next (get-char input)])
                                 (unless (eof-object? next)
                                   (feed-character! state next)
                                   (drain (- remaining 1))))))
                           (terminal-state-dirty-set! state #t)
                           (let ([bell? (terminal-state-bell state)])
                             (terminal-state-bell-set! state #f)
                             bell?))])
                    (when bell? (show-bell!)))
                  ;; Output must become visible while the main thread is
                  ;; blocked reading the editor's keyboard.
                  (display-redraw!)
                  (loop))))))))

  (define (write-bytes! state bytes)
    (when (terminal-state-alive state)
      (let ([output (terminal-process-output (terminal-state-process state))])
        (put-bytevector output bytes)
        (flush-output-port output))))

  (define (terminal-send! text)
    (let ([state (terminal-of (current-buffer))])
      (unless state (error 'terminal-send! "current buffer is not a terminal"))
      (write-bytes! state (string->utf8 text))))

  (define (control-byte letter)
    (bytevector (- (char->integer (char-upcase letter)) 64)))

  (define (meta-bytes state bytes)
    (if (and (terminal-state-meta-eight-bit state)
             (= (bytevector-length bytes) 1)
             (< (bytevector-u8-ref bytes 0) 128))
        (bytevector (bitwise-ior 128 (bytevector-u8-ref bytes 0)))
        (bytes-append (bytevector 27) bytes)))

  (define (bytes-append left right)
    (let ([result (make-bytevector (+ (bytevector-length left)
                                      (bytevector-length right)))])
      (bytevector-copy! left 0 result 0 (bytevector-length left))
      (bytevector-copy! right 0 result (bytevector-length left)
                        (bytevector-length right))
      result))

  (define key-modifiers
    '(("C-M-S-" . 8) ("C-M-" . 7) ("C-S-" . 6) ("M-S-" . 4)
      ("C-" . 5) ("M-" . 3) ("S-" . 2)))

  (define (modified-key event)
    (find (lambda (entry) (string-prefix? (car entry) event)) key-modifiers))

  (define (function-key-number event)
    (and (> (string-length event) 1)
         (char=? (string-ref event 0) #\F)
         (string->number (substring event 1 (string-length event)))))

  (define function-key-codes '#(0 0 0 0 0 15 17 18 19 20 21 23 24))

  (define (function-key-base-bytes base modifier)
    (string->utf8
      (cond [(<= base 4)
             (if (= modifier 1)
                 (format "\x1b;O~c" (integer->char (+ 79 base)))
                 (format "\x1b;[1;~a~c" modifier
                         (integer->char (+ 79 base))))]
            [(= modifier 1)
             (format "\x1b;[~a~~" (vector-ref function-key-codes base))]
            [else
             (format "\x1b;[~a;~a~~"
                     (vector-ref function-key-codes base) modifier)])))

  (define (function-key-bytes number)
    (let-values ([(base modifier)
                  (cond [(<= 1 number 12) (values number 1)]
                        [(<= 13 number 24) (values (- number 12) 2)]
                        [(<= 25 number 36) (values (- number 24) 5)]
                        [(<= 37 number 48) (values (- number 36) 6)]
                        [(<= 49 number 60) (values (- number 48) 3)]
                        [(<= 61 number 63) (values (- number 60) 4)]
                        [else (values #f #f)])])
      (and base (function-key-base-bytes base modifier))))

  (define (named-key-bytes state event)
    (let* ([modified (modified-key event)]
           [modifier (if modified (cdr modified) 1)]
           [base (if modified
                     (substring event (string-length (car modified))
                                (string-length event))
                     event)]
           [cursor-final (assoc base '(("UP" . "A") ("DOWN" . "B")
                                       ("RIGHT" . "C") ("LEFT" . "D")
                                       ("HOME" . "H") ("END" . "F")
                                       ("BEGIN" . "E")))]
           [tilde-code (assoc base '(("INSERT" . 2) ("DELETE" . 3)
                                     ("PAGEUP" . 5) ("PAGEDOWN" . 6)))]
           [function (function-key-number base)])
      (cond [cursor-final
             (string->utf8
               (if (and (= modifier 1) (string=? base "BEGIN"))
                   "\x1b;OE"
                   (if (= modifier 1)
                     (format "\x1b;~a~a"
                             (if (terminal-state-cursor-keys state) "O" "[")
                             (cdr cursor-final))
                     (format "\x1b;[1;~a~a" modifier
                             (cdr cursor-final)))))]
            [tilde-code
             (string->utf8
               (if (= modifier 1)
                   (format "\x1b;[~a~~" (cdr tilde-code))
                   (format "\x1b;[~a;~a~~" (cdr tilde-code) modifier)))]
            [(and function (<= 1 function 12))
             (function-key-base-bytes function modifier)]
            [else #f])))

  (define keypad-keys
    '(("KP-0" "0" . "p") ("KP-1" "1" . "q")
      ("KP-2" "2" . "r") ("KP-3" "3" . "s")
      ("KP-4" "4" . "t") ("KP-5" "5" . "u")
      ("KP-6" "6" . "v") ("KP-7" "7" . "w")
      ("KP-8" "8" . "x") ("KP-9" "9" . "y")
      ("KP-DECIMAL" "." . "n") ("KP-DIVIDE" "/" . "o")
      ("KP-MULTIPLY" "*" . "j") ("KP-SUBTRACT" "-" . "m")
      ("KP-ADD" "+" . "k") ("KP-COMMA" "," . "l")
      ("KP-EQUAL" "=" . "X") ("KP-ENTER" "\r" . "M")))

  (define (keypad-bytes state event)
    (let ([entry (assoc event keypad-keys)])
      (and entry
           (string->utf8
             (if (terminal-state-keypad state)
                 (string-append "\x1b;O" (cddr entry))
                 (cadr entry))))))

  (define (event-bytes state event)
    (cond
      [(and (terminal-state-focus-reporting state)
            (member event '("FOCUS" "BLUR")))
       (string->utf8 (if (string=? event "FOCUS") "\x1b;[I" "\x1b;[O"))]
      [(= (string-length event) 1) (string->utf8 event)]
      [(function-key-number event) => function-key-bytes]
      [(named-key-bytes state event) => values]
      [(keypad-bytes state event) => values]
      [(string-prefix? "C-M-" event)
       (meta-bytes state (control-byte (string-ref event 4)))]
      [(string-prefix? "M-" event)
       (meta-bytes state
                   (string->utf8 (substring event 2
                                            (string-length event))))]
      [(and (string-prefix? "C-" event) (= (string-length event) 3))
       (control-byte (string-ref event 2))]
      [else
       (cond [(assoc event
                     '(("RET" . "\r") ("TAB" . "\t")
                       ("BACKSPACE" . "\x7f;") ("ESC" . "\x1b;")
                       ("S-TAB" . "\x1b;[Z")
                      ))
              => (lambda (entry) (string->utf8 (cdr entry)))]
             [else #f])]))

  (define (terminal-close! . buffer*)
    (let* ([buffer (if (pair? buffer*) (car buffer*) (current-buffer))]
           [state (terminal-of buffer)])
      (when state
        (terminal-state-alive-set! state #f)
        (close-terminal-process! (terminal-state-process state))
        (set! terminals (remq state terminals)))))

  (define (terminal-close-all!)
    (for-each
      (lambda (state)
        (terminal-state-alive-set! state #f)
        (close-terminal-process! (terminal-state-process state)))
      (list-copy terminals))
    (set! terminals '()))

  (define (send-mouse! state code x y release?)
    (let ([button (if release? 3 code)])
      (cond
        [(terminal-state-mouse-sgr state)
         (write-bytes!
           state
           (string->utf8
             (format "\x1b;[<~a;~a;~a~a"
                     code x y (if release? "m" "M"))))]
        [(terminal-state-mouse-urxvt state)
         (write-bytes!
           state
           (string->utf8 (format "\x1b;[~a;~a;~aM" (+ 32 button) x y)))]
        [(terminal-state-mouse-utf8 state)
         (write-bytes!
           state
           (string->utf8
             (string-append "\x1b;[M"
                            (string (integer->char (+ 32 button))
                                    (integer->char (+ 32 x))
                                    (integer->char (+ 32 y))))))]
        [else
         ;; The original X10 encoding is limited to coordinates below 223.
         (write-bytes!
           state
           (bytevector 27 91 77
                       (+ 32 button)
                       (+ 32 (min x 223))
                       (+ 32 (min y 223))))])))

  (define (mouse-position state)
    (or (app-event-position)
        (cons (+ (cdr (point)) 1)
              (+ (- (car (point)) (length (terminal-state-history state))) 1))))

  (define (handle-terminal-event! state event)
    (cond
      ;; Once the PTY has closed, this is an ordinary read-only app again.
      ;; Let global chords such as C-x b, C-x k, and C-x o escape naturally
      ;; instead of silently sending them into a dead descriptor.
      [(not (terminal-state-alive state)) #f]
      [(string=? event "C-]")
       (escape-app-capture!
         "C-]" (lambda () (write-bytes! state (bytevector 29))))
       #t]
      [(member event '("FOCUS" "BLUR"))
       (cond [(event-bytes state event) =>
              (lambda (bytes) (write-bytes! state bytes))])
       #t]
      [(string=? event "PASTE")
       (terminal-follow! state)
       (let ([text (read-paste)])
         (write-bytes!
           state
           (string->utf8
             (if (terminal-state-bracketed state)
                 (string-append "\x1b;[200~" text "\x1b;[201~") text))))
       #t]
      [(string=? event "S-PAGEUP")
       (terminal-scroll! state -1 1)
       #t]
      [(string=? event "S-PAGEDOWN")
       (terminal-scroll! state 1 1)
       #t]
      [(member event '("S-WHEEL-UP" "S-WHEEL-DOWN"))
       (terminal-scroll! state (if (string=? event "S-WHEEL-UP") -1 1) 8)
       #t]
      [(string=? event "MOUSE-CLICK")
       (if (terminal-state-mouse state)
           (let* ([position (mouse-position state)]
                  [x (car position)] [y (cdr position)])
             (send-mouse! state 0 x y #f)
             #t)
           #f)]
      [(string=? event "MOUSE-DRAG")
       (if (and (terminal-state-mouse state)
                (>= (terminal-state-mouse state) 1002))
           (let* ([position (mouse-position state)]
                  [x (car position)] [y (cdr position)])
             (send-mouse! state 32 x y #f)
             #t)
           #f)]
      [(string=? event "MOUSE-RELEASE")
       (if (terminal-state-mouse state)
           (let* ([position (mouse-position state)]
                  [x (car position)] [y (cdr position)])
             (send-mouse! state 0 x y #t)
             #t)
           #f)]
      [(member event '("WHEEL-UP" "WHEEL-DOWN"))
       (if (terminal-state-mouse state)
           (let* ([position (mouse-position state)]
                  [x (car position)] [y (cdr position)])
             (send-mouse! state
                          (if (string=? event "WHEEL-UP") 64 65) x y #f)
             #t)
           (begin
             (terminal-scroll!
               state (if (string=? event "WHEEL-UP") -1 1) 8)
             #t))]
      [(string=? event "MOUSE") #f]
      [(event-bytes state event)
       => (lambda (bytes)
            (terminal-follow! state)
            (write-bytes! state bytes)
            #t)]
      [else #t]))

  (define (terminal!! . command*)
    (let* ([command (and (pair? command*) (car command*))]
           [directory (or (and (buffer-file (current-buffer))
                               (let ([path (buffer-file (current-buffer))])
                                 (let loop ([index (- (string-length path) 1)])
                                   (cond [(< index 0) (current-directory)]
                                         [(char=? (string-ref path index) #\/)
                                          (substring path 0 (max 1 index))]
                                         [else (loop (- index 1))]))))
                          (current-directory))]
          )
      (set! serial (+ serial 1))
      (let* ([name (if (= serial 1) "*terminal*"
                       (format "*terminal*<~a>" serial))]
             [prior (current-buffer)]
             [buffer #f]
             [state #f]
             [display #f]
             [process #f])
        (guard
          (ex [else
               ;; Terminal creation is transactional. In particular, never
               ;; leave a state-less app capturing every key after a display
               ;; or PTY setup error.
               (when process
                 (guard (ignored [else (void)])
                   (close-terminal-process! process)))
               (when display
                 (guard (ignored [else (void)]) (close-port display)))
               (when buffer
                 (guard (ignored [else (void)]) (set-app-capture! buffer #f))
                 (guard (ignored [else (void)]) (detach-app! buffer))
                 (when (eq? (current-buffer) buffer) (show-buffer! prior))
                 (guard (ignored [else (void)]) (kill-buffer! buffer)))
               (raise ex)])
          (set! display (duplicate-output-port (terminal-output-port)))
          (set! buffer
            (register-app!
              name
              (lambda () (when state (refresh-terminal! state)))
              (lambda (event) (and state (handle-terminal-event! state event)))))
          (set-app-presentation! buffer 0 #f #f 'blinking-block)
          (set-app-capture! buffer #t)
          (set-app-cursor-visible!
            buffer
            (lambda (window)
              (and state
                   (terminal-state-cursor-visible state)
                   (not (memq window
                              (terminal-state-unfollowed-windows state))))))
          (set-buffer-mode! buffer "terminal")
          (show-buffer! buffer)
          (let* ([size (or (buffer-window-size buffer) '(24 . 80))]
                 [rows (max 1 (car size))]
                 [cols (max 1 (cdr size))])
            (set! process
              (spawn-terminal-process (terminal-shell) command
                                      directory rows cols))
            (set! state
              (make-terminal-state buffer process display (make-mutex)
                                   rows cols (make-screen rows cols)
                                   (make-vector rows #f)
                                   0 0 0 0 #f 0 (- rows 1) 0 (- cols 1) #f
                                   'normal "" #f "" '() 'ascii 'ascii 0 0
                                   #f #t #f #f #f #f #f #f #t
                                   (default-tab-stops cols) #\space
                                   #t #t #f #f #f 0 #f #f #f #f #f #f #f #f
                                   0 0 #f #f #f #f #f
                                   '() '() '()
                                   (make-style-screen rows cols 'plain)
                                   #f '() #f
                                   (make-style-screen rows cols 'plain)
                                   "" 'plain (make-vector 256 #f) #f #f))
            (set! terminals (cons state terminals))
            (fork-thread (lambda () (reader-loop state)))
            (void))))))

  (define (init!)
    (register-mode! "terminal" '() '() (lambda (line) #f)
                    terminal-row-render terminal-row-styles)
    (bind-key! "C-c t" terminal!!)
    (add-buffer-kill-hook! terminal-close!)
    (add-shutdown-hook! terminal-close-all!)
    (add-buffer-status-hint!
      (lambda (buffer active?)
        (let ([state (terminal-of buffer)])
          (and state
               (if (terminal-state-alive state)
                   (let ([tail
                          (cond
                            [(not active?) ""]
                            [(app-capture-escaped? buffer) " escaped"]
                            [else " capturing input, C-] to escape"])])
                     (if (terminal-state-bell-visible state)
                         (list '(" " . #f)
                               '("♪" . red)
                               (cons tail 'italic))
                         (list '(" " . #f)
                               '("▶" . #f)
                               (cons tail 'italic))))
                   '((" " . #f) ("■" . #f)))))))
    (register-descriptions!
      '(((terminal!!)
         (("procedure" . "(terminal!! [command])")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Open a PTY-backed terminal app running `terminal-shell`, or interpret `command` with that shell when supplied. It captures keyboard, paste, and mouse input. C-] suspends capture for one complete global e command; C-] C-] sends the character literally.")
        ((terminal-send!)
         (("procedure" . "(terminal-send! text)")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Send text to the process in the current terminal buffer.")
        ((terminal-close!)
         (("procedure" . "(terminal-close! [buffer])")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Terminate and detach the process owned by a terminal buffer.")
        ((terminal-shell)
         (("parameter" . "(terminal-shell [path])")) "string"
         ("(terminal)") terminal "Terminal" #f
         "Get or set the shell used by terminal!!. It defaults to $SHELL, then /bin/sh.")
        ((make-terminal-emulator)
         (("procedure" . "(make-terminal-emulator rows columns)"))
         "terminal-emulator" ("(terminal)") terminal "Terminal" #f
         "Create a headless terminal emulator for tests and structured protocol processing.")
        ((terminal-emulator-feed!)
         (("procedure" . "(terminal-emulator-feed! emulator text)")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Feed terminal output into a headless emulator.")
        ((terminal-emulator-resize!)
         (("procedure" . "(terminal-emulator-resize! emulator rows columns)"))
         "void" ("(terminal)") terminal "Terminal" #f
         "Resize a headless terminal emulator, reflowing primary-screen scrollback and preserving its logical cursor.")
        ((terminal-emulator-screen)
         (("procedure" . "(terminal-emulator-screen emulator)")) "vector"
         ("(terminal)") terminal "Terminal" #f
         "Return a copy of a headless emulator's visible cell rows.")
        ((terminal-emulator-styles)
         (("procedure" . "(terminal-emulator-styles emulator)")) "vector"
         ("(terminal)") terminal "Terminal" #f
         "Return copies of the style rows for a headless emulator's visible cells.")
        ((terminal-emulator-state)
         (("procedure" . "(terminal-emulator-state emulator)")) "alist"
         ("(terminal)") terminal "Terminal" #f
         "Return the cursor, dimensions, and active modes of a headless emulator.")
        ((terminal-emulator-input)
         (("procedure" . "(terminal-emulator-input emulator event)"))
         "bytevector or #f" ("(terminal)") terminal "Terminal" #f
         "Encode an editor key event according to a headless emulator's active modes.")
        ((terminal-emulator-replies)
         (("procedure" . "(terminal-emulator-replies emulator)")) "list"
         ("(terminal)") terminal "Terminal" #f
         "Return protocol replies emitted by a headless emulator."))))

) ;; library (terminal)
