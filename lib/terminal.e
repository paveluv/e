;; terminal.e -- PTY-backed terminal emulator app.

(library (terminal)
  (export init! terminal!! terminal-send! terminal-close! terminal-scrollback
          terminal-shell
          make-terminal-emulator terminal-emulator?
          terminal-emulator-feed! terminal-emulator-screen
          terminal-emulator-styles terminal-emulator-state terminal-emulator-input
          terminal-emulator-replies)
  (import (chezscheme) (core) (sys)
          (only (describe) register-descriptions!))

  (define-record-type terminal-state
    (fields buffer process display lock
            (mutable rows) (mutable cols) (mutable screen)
            (mutable row) (mutable col)
            (mutable saved-row) (mutable saved-col) (mutable saved-state)
            (mutable scroll-top) (mutable scroll-bottom)
            (mutable parser) (mutable parameters)
            (mutable osc-escape) (mutable osc-text) (mutable replies)
            (mutable charset) (mutable charset-g1) (mutable charset-target)
            (mutable shift)
            (mutable wrap-pending) (mutable autowrap) (mutable origin)
            (mutable insert) (mutable cursor-keys) (mutable keypad)
            (mutable cursor-visible) (mutable tab-stops)
            (mutable last-character)
            (mutable dirty) (mutable alive) (mutable prefix)
            (mutable mouse) (mutable mouse-sgr)
            (mutable bracketed) (mutable main-screen)
            (mutable main-row) (mutable main-col) (mutable main-state)
            (mutable alternate-screen) (mutable alternate-styles)
            (mutable alternate-state)
            (mutable history) (mutable unfollowed-windows)
            (mutable styles) (mutable main-styles) (mutable history-styles)
            (mutable rendered-styles)
            (mutable sgr) (mutable style)))

  (define terminals '())
  (define serial 0)
  (define style-serial 0)
  (define style-cache (make-hashtable string-hash string=?))
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

  (define (terminal-emulator? value) (terminal-state? value))

  (define (make-terminal-emulator rows cols)
    (unless (and (integer? rows) (exact? rows) (> rows 0)
                 (integer? cols) (exact? cols) (> cols 0))
      (error 'make-terminal-emulator
             "rows and columns must be positive exact integers" rows cols))
    (make-terminal-state #f #f #f (make-mutex)
                         rows cols (make-screen rows cols)
                         0 0 0 0 #f 0 (- rows 1)
                         'normal "" #f "" '() 'ascii 'ascii 0 0
                         #f #t #f #f #f #f #t
                         (default-tab-stops cols) #\space
                         #f #f #f #f #f #f #f 0 0 #f #f #f #f '() '()
                         (make-style-screen rows cols 'plain)
                         #f '() (make-style-screen rows cols 'plain)
                         "" 'plain))

  (define (terminal-emulator-feed! emulator text)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-feed! "expected a terminal emulator" emulator))
    (unless (string? text)
      (error 'terminal-emulator-feed! "expected a string" text))
    (string-for-each (lambda (character) (feed-character! emulator character))
                     text)
    (void))

  (define (terminal-emulator-screen emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-screen "expected a terminal emulator" emulator))
    (vector-map string-copy (terminal-state-screen emulator)))

  (define (terminal-emulator-styles emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-styles "expected a terminal emulator" emulator))
    (vector-map vector-copy (terminal-state-styles emulator)))

  (define (terminal-emulator-state emulator)
    (unless (terminal-emulator? emulator)
      (error 'terminal-emulator-state "expected a terminal emulator" emulator))
    `((rows . ,(terminal-state-rows emulator))
      (columns . ,(terminal-state-cols emulator))
      (cursor . ,(cons (terminal-state-row emulator)
                       (terminal-state-col emulator)))
      (scroll-region . ,(cons (terminal-state-scroll-top emulator)
                              (terminal-state-scroll-bottom emulator)))
      (wrap-pending . ,(terminal-state-wrap-pending emulator))
      (autowrap . ,(terminal-state-autowrap emulator))
      (origin . ,(terminal-state-origin emulator))
      (insert . ,(terminal-state-insert emulator))
      (cursor-visible . ,(terminal-state-cursor-visible emulator))
      (application-cursor-keys . ,(terminal-state-cursor-keys emulator))
      (application-keypad . ,(terminal-state-keypad emulator))
      (mouse-tracking . ,(terminal-state-mouse emulator))
      (sgr-mouse . ,(terminal-state-mouse-sgr emulator))
      (bracketed-paste . ,(terminal-state-bracketed emulator))))

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

  (define (blank-line cols) (make-string cols #\space))
  (define (blank-styles cols style) (make-vector cols style))

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

  (define (resized-screen old old-rows old-cols rows cols)
    (let ([new (make-screen rows cols)]
          [copy-rows (min rows old-rows)]
          [copy-cols (min cols old-cols)])
      (do ([row 0 (+ row 1)]) ((= row copy-rows) new)
        (string-copy! (vector-ref old row) 0
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
          (terminal-state-last-character state)))

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
    (terminal-state-last-character-set! state (list-ref saved 18)))

  (define (enter-alternate-screen! state clear?)
    (unless (terminal-state-main-screen state)
      (terminal-state-main-screen-set! state (terminal-state-screen state))
      (terminal-state-main-styles-set! state (terminal-state-styles state))
      (terminal-state-main-row-set! state (terminal-state-row state))
      (terminal-state-main-col-set! state (terminal-state-col state))
      (terminal-state-main-state-set! state (capture-screen-state state))
      (if (and (not clear?) (terminal-state-alternate-screen state))
          (begin
            (terminal-state-screen-set!
              state (terminal-state-alternate-screen state))
            (terminal-state-styles-set!
              state (terminal-state-alternate-styles state))
            (restore-screen-state! state (terminal-state-alternate-state state)))
          (begin
            (terminal-state-screen-set!
              state (make-screen (terminal-state-rows state)
                                 (terminal-state-cols state)))
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
      (terminal-state-alternate-styles-set! state (terminal-state-styles state))
      (terminal-state-alternate-state-set! state (capture-screen-state state))
      (terminal-state-screen-set! state (terminal-state-main-screen state))
      (terminal-state-styles-set! state (terminal-state-main-styles state))
      (when (terminal-state-main-state state)
        (restore-screen-state! state (terminal-state-main-state state)))
      (terminal-state-main-screen-set! state #f)
      (terminal-state-main-styles-set! state #f)
      (terminal-state-main-state-set! state #f)))

  (define (resize-screen! state rows cols)
    (unless (and (= rows (terminal-state-rows state))
                 (= cols (terminal-state-cols state)))
      (let* ([old-rows (terminal-state-rows state)]
             [old-cols (terminal-state-cols state)]
             [new (resized-screen (terminal-state-screen state)
                                  old-rows old-cols rows cols)]
             [new-styles (resized-styles (terminal-state-styles state)
                                         old-rows old-cols rows cols
                                         (terminal-state-style state))])
        (when (terminal-state-main-screen state)
          (terminal-state-main-screen-set!
            state (resized-screen (terminal-state-main-screen state)
                                  old-rows old-cols rows cols)))
        (when (terminal-state-main-styles state)
          (terminal-state-main-styles-set!
            state (resized-styles (terminal-state-main-styles state)
                                  old-rows old-cols rows cols
                                  (terminal-state-style state))))
        (when (terminal-state-alternate-screen state)
          (terminal-state-alternate-screen-set!
            state (resized-screen (terminal-state-alternate-screen state)
                                  old-rows old-cols rows cols)))
        (when (terminal-state-alternate-styles state)
          (terminal-state-alternate-styles-set!
            state (resized-styles (terminal-state-alternate-styles state)
                                  old-rows old-cols rows cols
                                  (terminal-state-style state))))
        (terminal-state-screen-set! state new)
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
        (terminal-state-dirty-set! state #t)
        (resize-terminal-process! (terminal-state-process state) rows cols))))

  (define (scroll-up! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (when (and (= top 0) (= bottom (- (terminal-state-rows state) 1))
                   (not (terminal-state-main-screen state))
                   (> (terminal-scrollback) 0))
          (let ([history (append (terminal-state-history state)
                                 (list (string-copy (vector-ref screen top))))])
            (terminal-state-history-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history))))
          (let ([history (append (terminal-state-history-styles state)
                                 (list (vector-copy (vector-ref styles top))))])
            (terminal-state-history-styles-set!
              state
              (let ([extra (- (length history) (terminal-scrollback))])
                (if (> extra 0) (list-tail history extra) history)))))
        (do ([row top (+ row 1)]) ((= row bottom))
          (vector-set! screen row (vector-ref screen (+ row 1)))
          (vector-set! styles row (vector-ref styles (+ row 1))))
        (vector-set! screen bottom (blank-line cols))
        (vector-set! styles bottom
                     (blank-styles cols (terminal-state-style state))))))

  (define (scroll-down! state count)
    (let ([screen (terminal-state-screen state)]
          [styles (terminal-state-styles state)]
          [top (terminal-state-scroll-top state)]
          [bottom (terminal-state-scroll-bottom state)]
          [cols (terminal-state-cols state)])
      (do ([n 0 (+ n 1)]) ((= n count))
        (do ([row bottom (- row 1)]) ((= row top))
          (vector-set! screen row (vector-ref screen (- row 1)))
          (vector-set! styles row (vector-ref styles (- row 1))))
        (vector-set! screen top (blank-line cols))
        (vector-set! styles top
                     (blank-styles cols (terminal-state-style state))))))

  (define (line-feed! state)
    (if (= (terminal-state-row state) (terminal-state-scroll-bottom state))
        (scroll-up! state 1)
        (terminal-state-row-set!
          state (min (- (terminal-state-rows state) 1)
                     (+ (terminal-state-row state) 1)))))

  (define (put-character! state character)
    ;; VT autowrap is delayed until the next printable character. Cursor
    ;; motion and controls can therefore cancel a pending wrap at the margin.
    (when (terminal-state-wrap-pending state)
      (terminal-state-wrap-pending-set! state #f)
      (when (terminal-state-autowrap state)
        (terminal-state-col-set! state 0)
        (line-feed! state)))
    (when (terminal-state-insert state) (insert-characters! state 1))
    (string-set! (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))
                 (terminal-state-col state) character)
    (vector-set! (vector-ref (terminal-state-styles state)
                             (terminal-state-row state))
                 (terminal-state-col state) (terminal-state-style state))
    (terminal-state-last-character-set! state character)
    (if (= (terminal-state-col state) (- (terminal-state-cols state) 1))
        (terminal-state-wrap-pending-set! state #t)
        (terminal-state-col-set! state (+ (terminal-state-col state) 1))))

  (define (erase-line! state start end)
    (let ([line (vector-ref (terminal-state-screen state)
                            (terminal-state-row state))]
          [styles (vector-ref (terminal-state-styles state)
                              (terminal-state-row state))])
      (do ([col (max 0 start) (+ col 1)])
          ((>= col (min end (terminal-state-cols state))))
        (string-set! line col #\space)
        (vector-set! styles col (terminal-state-style state)))))

  (define (clear-rows! state start end)
    (do ([row (max 0 start) (+ row 1)])
        ((>= row (min end (terminal-state-rows state))))
      (vector-set! (terminal-state-screen state) row
                   (blank-line (terminal-state-cols state)))
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

  (define (dispatch-osc! state)
    (let ([text (terminal-state-osc-text state)])
      (cond
        [(string=? text "10;?")
         (terminal-reply! state "\x1b;]10;rgb:0000/0000/0000\x1b;\\")]
        [(string=? text "11;?")
         (terminal-reply! state "\x1b;]11;rgb:ffff/ffff/ffff\x1b;\\")]
        [(and (> (string-length text) 2)
              (memv (string-ref text 0) '(#\0 #\1 #\2))
              (char=? (string-ref text 1) #\;))
         (let ([title (substring text 2 (string-length text))])
           (unless (or (string=? title "") (not (terminal-state-buffer state)))
             (set-buffer-name! (terminal-state-buffer state)
                               (format "*~a*" title))))]
        [else (void)])))

  (define (delete-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [cols (terminal-state-cols state)]
           [count (min count (- cols col))])
      (string-copy! line (+ col count) line col (- cols col count))
      (copy-vector-range! styles (+ col count) styles col (- cols col count))
      (do ([i (- cols count) (+ i 1)]) ((= i cols))
        (string-set! line i #\space)
        (vector-set! styles i (terminal-state-style state)))))

  (define (insert-characters! state count)
    (let* ([line (vector-ref (terminal-state-screen state)
                             (terminal-state-row state))]
           [styles (vector-ref (terminal-state-styles state)
                               (terminal-state-row state))]
           [col (terminal-state-col state)]
           [cols (terminal-state-cols state)]
           [count (min count (- cols col))])
      (do ([i (- cols 1) (- i 1)]) ((< i (+ col count)))
        (string-set! line i (string-ref line (- i count)))
        (vector-set! styles i (vector-ref styles (- i count))))
      (do ([i col (+ i 1)]) ((= i (+ col count)))
        (string-set! line i #\space)
        (vector-set! styles i (terminal-state-style state)))))

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
                name)))))

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
      (terminal-state-styles-set! state (make-style-screen rows cols 'plain))
      (terminal-state-row-set! state 0)
      (terminal-state-col-set! state 0)
      (terminal-state-saved-row-set! state 0)
      (terminal-state-saved-col-set! state 0)
      (terminal-state-saved-state-set! state #f)
      (terminal-state-scroll-top-set! state 0)
      (terminal-state-scroll-bottom-set! state (- rows 1))
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
      (terminal-state-cursor-keys-set! state #f)
      (terminal-state-keypad-set! state #f)
      (terminal-state-cursor-visible-set! state #t)
      (terminal-state-tab-stops-set! state (default-tab-stops cols))
      (terminal-state-last-character-set! state #\space)
      (terminal-state-mouse-set! state #f)
      (terminal-state-mouse-sgr-set! state #f)
      (terminal-state-bracketed-set! state #f)
      (terminal-state-main-screen-set! state #f)
      (terminal-state-main-styles-set! state #f)
      (terminal-state-main-state-set! state #f)
      (terminal-state-alternate-screen-set! state #f)
      (terminal-state-alternate-styles-set! state #f)
      (terminal-state-alternate-state-set! state #f)
      (terminal-state-history-set! state '())
      (terminal-state-history-styles-set! state '())
      (terminal-state-sgr-set! state "")
      (terminal-state-style-set! state 'plain)
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
                  [(6)
                   (terminal-state-origin-set! state on?)
                   (terminal-state-row-set!
                     state (if on? (terminal-state-scroll-top state) 0))
                   (terminal-state-col-set! state 0)]
                  [(7) (terminal-state-autowrap-set! state on?)]
                  [(25) (terminal-state-cursor-visible-set! state on?)]
                  [(1000 1002 1003)
                   (if on?
                       (terminal-state-mouse-set! state mode)
                       (when (eqv? (terminal-state-mouse state) mode)
                         (terminal-state-mouse-set! state #f)))]
                  [(1006) (terminal-state-mouse-sgr-set! state on?)]
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
            [(#\C #\a) (terminal-state-col-set! state (min (- cols 1) (+ col n)))]
            [(#\D) (terminal-state-col-set! state (max 0 (- col n)))]
            [(#\E) (terminal-state-row-set!
                     state
                     (min (if (terminal-state-origin state)
                              (terminal-state-scroll-bottom state) (- rows 1))
                          (+ row n)))
             (terminal-state-col-set! state 0)]
            [(#\F) (terminal-state-row-set!
                     state
                     (max (if (terminal-state-origin state)
                              (terminal-state-scroll-top state) 0)
                          (- row n)))
             (terminal-state-col-set! state 0)]
            [(#\G #\`) (terminal-state-col-set! state
                                                (clamp (- n 1) 0 (- cols 1)))]
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
             (terminal-state-col-set! state
                                      (clamp (- (param parameters 1 1) 1)
                                        0 (- cols 1)))]
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
             (terminal-state-col-set! state 0)]
            [(#\s) (save-cursor! state)]
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
               (terminal-state-row-set! state 0)
               (terminal-state-col-set! state 0)
               (terminal-state-scroll-top-set! state 0)
               (terminal-state-scroll-bottom-set! state (- rows 1))
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
         [(7) (void)]
         [(8) (terminal-state-wrap-pending-set! state #f)
          (terminal-state-col-set! state
                                   (max 0 (- (terminal-state-col state) 1)))]
         [(9) (terminal-state-wrap-pending-set! state #f)
          (terminal-state-col-set! state (next-tab-stop state))]
         [(10 11 12) (terminal-state-wrap-pending-set! state #f)
          (line-feed! state)]
         [(13) (terminal-state-wrap-pending-set! state #f)
          (terminal-state-col-set! state 0)]
         [(14) (terminal-state-shift-set! state 1)]
         [(15) (terminal-state-shift-set! state 0)]
         [(27) (terminal-state-parser-set! state 'escape)]
         [(132) (line-feed! state)]                    ; IND
         [(133) (line-feed! state)                     ; NEL
          (terminal-state-col-set! state 0)]
         [(136) (vector-set! (terminal-state-tab-stops state)
                             (terminal-state-col state) #t)] ; HTS
         [(141) (if (= (terminal-state-row state)
                       (terminal-state-scroll-top state))
                    (scroll-down! state 1)
                    (terminal-state-row-set!
                      state (max 0 (- (terminal-state-row state) 1))))] ; RI
         [(155) (terminal-state-parser-set! state 'csi) ; CSI
          (terminal-state-parameters-set! state "")]
         [(157) (terminal-state-parser-set! state 'osc) ; OSC
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         [else (when (>= (char->integer character) 32)
                 (put-character! state (mapped-character state character)))])]
      [(escape)
       (case character
         [(#\[) (terminal-state-parser-set! state 'csi)
          (terminal-state-parameters-set! state "")]
         [(#\]) (terminal-state-parser-set! state 'osc)
          (terminal-state-osc-escape-set! state #f)
          (terminal-state-osc-text-set! state "")]
         ;; String controls carry arbitrary printable payload terminated by
         ;; ST (ESC \). They are metadata/protocol traffic, never screen text.
         [(#\P #\X #\^ #\_)
          (terminal-state-parser-set! state 'control-string)
          (terminal-state-osc-escape-set! state #f)]
         [(#\7) (save-cursor! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\8) (restore-cursor! state)
          (terminal-state-parser-set! state 'normal)]
         [(#\D) (line-feed! state) (terminal-state-parser-set! state 'normal)]
         [(#\E) (line-feed! state)
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
         [(= (char->integer character) 156) ; ST
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [(and (terminal-state-osc-escape state) (char=? character #\\))
          (terminal-state-parser-set! state 'normal)
          (terminal-state-osc-escape-set! state #f)]
         [else
          (terminal-state-osc-escape-set! state
                                          (char=? character #\esc))])]))

  (define (refresh-terminal! state)
    (let ([size (buffer-window-size (terminal-state-buffer state))])
      (when size
        (with-mutex (terminal-state-lock state)
          (resize-screen! state (max 1 (car size)) (max 1 (cdr size)))
          (when (terminal-state-dirty state)
            ;; Snapshot cells and faces together while the PTY grid is locked.
            ;; Painting either one live can combine different stages of a
            ;; full-screen application's redisplay into one torn frame.
            (let ([lines
                   (append (if (terminal-state-main-screen state)
                               '() (terminal-state-history state))
                           (vector->list (terminal-state-screen state)))]
                  [styles
                   (append (if (terminal-state-main-screen state)
                               '() (terminal-state-history-styles state))
                           (vector->list (terminal-state-styles state)))])
              (view-replace! (terminal-state-buffer state)
                             (map string-copy lines))
              (terminal-state-rendered-styles-set!
                state (list->vector (map vector-copy styles))))
            (terminal-state-dirty-set! state #f))
          (when (and (eq? (current-buffer) (terminal-state-buffer state))
                     (not (memq (selected-window)
                                (terminal-state-unfollowed-windows state))))
            (goto-point! (terminal-cursor-position state)))))))

  (define (terminal-row-styles buffer row line)
    (let ([state (terminal-of buffer)])
      (and state
           (< row (vector-length (terminal-state-rendered-styles state)))
           (vector-ref (terminal-state-rendered-styles state) row))))

  (define (reader-loop state)
    (define (display-redraw!)
      (parameterize ([terminal-output-port (terminal-state-display state)])
        (redraw!)))
    (define (finished!)
      (terminal-state-alive-set! state #f)
      (guard (ex [else (void)])
        (set-app-capture! (terminal-state-buffer state) #f))
      (guard (ex [else (void)])
        (set-buffer-wrap! (terminal-state-buffer state) #f))
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
                  (with-mutex (terminal-state-lock state)
                    (feed-character! state character)
                    (let drain ([remaining 4095])
                      (when (and (> remaining 0) (char-ready? input))
                        (let ([next (get-char input)])
                          (unless (eof-object? next)
                            (feed-character! state next)
                            (drain (- remaining 1))))))
                    (terminal-state-dirty-set! state #t))
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

  (define (bytes-append left right)
    (let ([result (make-bytevector (+ (bytevector-length left)
                                      (bytevector-length right)))])
      (bytevector-copy! left 0 result 0 (bytevector-length left))
      (bytevector-copy! right 0 result (bytevector-length left)
                        (bytevector-length right))
      result))

  (define (event-bytes state event)
    (cond
      [(= (string-length event) 1) (string->utf8 event)]
      [(assoc event
              '(("S-UP" . "\x1b;[1;2A") ("S-DOWN" . "\x1b;[1;2B")
                ("S-RIGHT" . "\x1b;[1;2C") ("S-LEFT" . "\x1b;[1;2D")
                ("M-UP" . "\x1b;[1;3A") ("M-DOWN" . "\x1b;[1;3B")
                ("M-RIGHT" . "\x1b;[1;3C") ("M-LEFT" . "\x1b;[1;3D")
                ("M-S-UP" . "\x1b;[1;4A") ("M-S-DOWN" . "\x1b;[1;4B")
                ("M-S-RIGHT" . "\x1b;[1;4C") ("M-S-LEFT" . "\x1b;[1;4D")))
       => (lambda (entry) (string->utf8 (cdr entry)))]
      [(string-prefix? "C-M-" event)
       (bytes-append (bytevector 27)
                     (control-byte (string-ref event 4)))]
      [(string-prefix? "M-" event)
       (bytes-append (bytevector 27)
                     (string->utf8 (substring event 2
                                              (string-length event))))]
      [(and (string-prefix? "C-" event) (= (string-length event) 3))
       (control-byte (string-ref event 2))]
      [else
       (cond [(and (terminal-state-cursor-keys state)
                   (assoc event
                          '(("UP" . "\x1b;OA") ("DOWN" . "\x1b;OB")
                            ("RIGHT" . "\x1b;OC") ("LEFT" . "\x1b;OD")
                            ("HOME" . "\x1b;OH") ("END" . "\x1b;OF"))))
              => (lambda (entry) (string->utf8 (cdr entry)))]
             [(assoc event
                     '(("RET" . "\r") ("TAB" . "\t")
                       ("BACKSPACE" . "\x7f;") ("ESC" . "\x1b;")
                       ("UP" . "\x1b;[A") ("DOWN" . "\x1b;[B")
                       ("RIGHT" . "\x1b;[C") ("LEFT" . "\x1b;[D")
                       ("HOME" . "\x1b;[H") ("END" . "\x1b;[F")
                       ("DELETE" . "\x1b;[3~")
                       ("PAGEUP" . "\x1b;[5~")
                       ("PAGEDOWN" . "\x1b;[6~")
                       ("S-TAB" . "\x1b;[Z")
                       ("F1" . "\x1b;OP") ("F2" . "\x1b;OQ")
                       ("F3" . "\x1b;OR") ("F4" . "\x1b;OS")
                       ("F5" . "\x1b;[15~") ("F6" . "\x1b;[17~")
                       ("F7" . "\x1b;[18~") ("F8" . "\x1b;[19~")
                       ("F9" . "\x1b;[20~") ("F10" . "\x1b;[21~")
                       ("F11" . "\x1b;[23~") ("F12" . "\x1b;[24~")))
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
    (if (terminal-state-mouse-sgr state)
        (write-bytes!
          state
          (string->utf8
            (format "\x1b;[<~a;~a;~a~a" code x y (if release? "m" "M"))))
        ;; The original X10 encoding is limited to coordinates below 224.
        (write-bytes!
          state
          (bytevector 27 91 77
                      (+ 32 (if release? 3 code))
                      (+ 32 (min x 223))
                      (+ 32 (min y 223))))))

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
                                   0 0 0 0 #f 0 (- rows 1)
                                   'normal "" #f "" '() 'ascii 'ascii 0 0
                                   #f #t #f #f #f #f #t
                                   (default-tab-stops cols) #\space
                                   #t #t #f #f #f #f #f 0 0 #f #f #f #f '() '()
                                   (make-style-screen rows cols 'plain)
                                   #f '() (make-style-screen rows cols 'plain)
                                   "" 'plain))
            (set! terminals (cons state terminals))
            (fork-thread (lambda () (reader-loop state)))
            (void))))))

  (define (init!)
    (register-mode! "terminal" '() '() (lambda (line) #f)
                    #f terminal-row-styles)
    (bind-key! "C-c t" terminal!!)
    (add-buffer-kill-hook! terminal-close!)
    (add-shutdown-hook! terminal-close-all!)
    (add-buffer-status-hint!
      (lambda (buffer active?)
        (let ([state (terminal-of buffer)])
          (and state
               (if (terminal-state-alive state)
                   (cond
                     [(not active?) '("  running" . italic)]
                     [(app-capture-escaped? buffer)
                      '("  running; escaped" . italic)]
                     [else
                      '("  running; capturing input, C-] to escape" . italic)])
                   '("  exited" . italic))))))
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
