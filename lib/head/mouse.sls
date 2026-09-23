;; mouse.sls -- the mouse: the library (mouse).
;;
;; SGR mouse tracking: clicks focus the window under the pointer and
;; place point at the clicked cell, dragging selects as though the mark
;; were set at the press and point moved, the wheel scrolls the window
;; under the pointer wherever the focus is, and pointer motion tells the
;; local app under it.  Hit-testing over the remembered tiling and the
;; gesture state live in (head); the actions they trigger are here, and
;; init! installs the handler on the head's pump.  The cost of tracking
;; is the terminal's native mouse selection -- hold Shift for that -- so
;; track! turns the whole thing on or off at run time.

(import (only (foundation edoc) elibrary))
(elibrary (head mouse)
  (export init! track!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (head dispatch) dispatch:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head window) window:)
          (prefix (sys tty) tty:))

  (edoc "Turn mouse tracking on or off; off restores the terminal's native selection."
        (on boolean "whether to track the mouse"))
  (define (track! on)
    (tty:mouse-reporting! on)
    (head:set-mouse-position! #f)
    (paint:show-message! (format "Mouse ~a" (if on "on" "off")) #f)
    (void))

  ;;; The selection a press arms ------------------------------------------------------

  (define (text-gesture? w)
    (let ([gesture (head:drag)])
      (and (pair? gesture) (eq? (car gesture) w)
           (eq? (cdr gesture) (head:window-buffer w)))))

  (define (word-char? c)
    (not (or (char-whitespace? c)
             (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\.)))))

  (define (select-word!)
    ;; Select the word point is on (or just after): mark at its start,
    ;; point at its end.
    (let* ([w (head:current-window)] [b (head:window-buffer w)]
           [row (head:window-prow w)] [col (head:window-pcol w)]
           [s (vector-ref (head:buffer-lines b) row)]
           [n (string-length s)]
           [on? (lambda (i)
                  (and (>= i 0) (< i n) (word-char? (string-ref s i))))]
           [at (cond [(on? col) col]
                     [(on? (- col 1)) (- col 1)]
                     [else #f])])
      (when at
        (head:buffer-mark-row-set! b row)
        (head:buffer-mark-col-set! b (let back ([i at])
                                       (if (on? (- i 1)) (back (- i 1)) i)))
        (head:window-pcol-set! w (let fwd ([i at])
                                   (if (on? i) (fwd (+ i 1)) i)))
        (head:buffer-marked-set! b #t))))

  (define (arm-text-selection! double?)
    ;; the mark at point, inactive: dragging activates it, a motionless
    ;; click does not; a double click selects the word there
    (let* ([w (head:current-window)] [b (head:window-buffer w)])
      (head:buffer-mark-row-set! b (head:window-prow w))
      (head:buffer-mark-col-set! b (head:window-pcol w))
      (head:buffer-marked-set! b #f)
      (when double? (select-word!))))

  (define (call-with-app-mouse-event w start height x y button thunk)
    ;; One coordinate boundary for clicks, drags, releases, and wheel ticks.
    ;; Exclude chrome from viewport cells; retain raw character positions
    ;; beyond text so apps can distinguish blank space from the last glyph.
    (parameterize
      ([head:app-event-position
        (cons (max 1 (- x (head:window-xoff w)
                        (if (eq? (head:window-scrollbar? w) 'left) 1 0)
                        (head:window-line-number-width w)))
              (max 1 (- y start)))]
       [head:app-event-buffer-position (paint:window-position w start height x y)]
       [head:app-event-button button])
      (thunk)))

  ;;; Presses, drags and releases -------------------------------------------------------

  (define (mouse-press! x y button)
    ;; A normal-buffer press focuses its window and places point. An app text
    ;; press instead updates and invokes the app without stealing focus; only
    ;; an app status-bar press focuses that window. A text press also arms the
    ;; mark there -- dragging activates it, a motionless click does not;
    ;; a second press on the same cell within half a second is a double
    ;; click, selecting the word there.  A press on a status bar (other
    ;; than the lowest) arms a resize drag instead.
    ;; The terminal's own Shift-selection highlight is not touched here
    ;; (erasing on every press flickers); C-l clears it.
    (head:set-drag! #f)
    (let ([double? (head:double-click? x y (real-time))])
      (cond
        [(head:window-button-at (- x 1) (- y 1)) =>
         (lambda (control)
           (let ([action (car control)] [w (cdr control)])
             (unless (head:popup? w) (window:focus! w))
             (if (procedure? action) (action)
               (case action
                 [(below) (window:split-below!)]
                 [(right) (window:split-right!)]
                 [(close) (window:delete!)]
                 [(clear) (window:clear-pop-up!)])))
           "MOUSE-HANDLED")]
        [(head:divider-at (- x 1) (- y 1)) =>
         (lambda (divider)
           ;; a below divider doubles as the upper window's status bar:
           ;; pressing it focuses that window, as any status bar does,
           ;; and still arms the drag
           (when (eq? (car divider) 'below)
             (head:window-at (- x 1) (- y 1)
               (lambda (entry) (window:focus! (car entry)))))
           (head:set-drag! divider)
           "MOUSE-HANDLED")]
        [else
         (head:window-at (- x 1) (- y 1)
           (lambda (entry)
             (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
               (cond
                 [(= (- y 1) (+ start height))        ; the status bar
                  (window:focus! w)
                  "MOUSE-HANDLED"]
                 [(and (head:window-scrollbar-column w)
                       (= (- x 1) (head:window-scrollbar-column w)))
                  ;; App bars navigate like their wheel controls: they do not
                  ;; take focus and do not invoke the row's click action.
                  (let ([old (head:current-window)])
                    (unless (head:app-buffer? (head:window-buffer w))
                      (window:focus! w))
                    (head:set-current! w)
                    (when (and (head:app-buffer? (head:window-buffer w))
                               (memq old (head:windows)))
                      (head:set-current! old)))
                  "MOUSE-HANDLED"]
                 [(head:app-buffer? (head:window-buffer w))
                  (let ([old (head:current-window)])
                    (head:set-current! w)
                    (let ([old-point (head:point)]
                          [clicked (paint:window-position w start height x y)])
                      (head:goto! clicked)
                      (head:buffer-marked-set! (head:window-buffer w) #f)
                      (head:set-drag! (cons w (head:window-buffer w)))
                      ;; Focusing the clicked window is the default. An app may
                      ;; act on the click and explicitly preserve the old
                      ;; focus by returning keep-focus for MOUSE-CLICK.
                      (let ([result
                             (parameterize ([head:app-event-focus old])
                               (call-with-app-mouse-event w start height x y button
                                 (lambda () (head:dispatch-app-event! "MOUSE-CLICK"))))])
                        (cond [(eq? result 'ignore-click)
                               (head:set-drag! #f)
                               (head:goto! old-point)
                               (when (memq old (head:windows))
                                 (head:set-current! old))]
                              [(and (eq? result 'keep-focus) (memq old (head:windows)))
                               (head:set-current! old)]
                              [(not result)
                               ;; Views and unhandled app text select like
                               ;; ordinary read-only buffer text. Arm the mark at
                               ;; this press instead of reusing stale state.
                               (arm-text-selection! double?)])))
                    "MOUSE-HANDLED")]
                 [else                                ; a text row
                  (window:focus! w)
                  (head:goto! (paint:window-position w start height x y))
                  (arm-text-selection! double?)
                  (head:set-drag! (cons w (head:window-buffer w)))
                  ;; A mode may act on the click -- following a link,
                  ;; say -- through a MOUSE-CLICK binding in its keymap.
                  (let ([context (mode:key-context (head:current-buffer))])
                    (when context
                      (let ([action (keymap:event-binding context "MOUSE-CLICK")])
                        (when (procedure? action)
                          (guard (ex [else (paint:show-message! (kernel:condition-text ex) #f)])
                            (action))))))
                  "MOUSE-HANDLED"]))))])))

  (define (mouse-drag! x y button)
    ;; A split-divider drag resizes its two subtrees; otherwise extend
    ;; the selection armed by the press --
    ;; the mark activates and point follows the pointer within the
    ;; focused window's text area.
    (let ([gesture (head:drag)])
      (cond
        [(and (pair? gesture) (memq (car gesture) '(right below)))
         (let* ([orientation (car gesture)]
                [split (cadr gesture)]
                [old (if (eq? orientation 'right) (caddr gesture) (cadddr gesture))]
                [now (if (eq? orientation 'right) (- x 1) (- y 1))]
                [delta (- now old)])
           (unless (= delta 0)
             (head:transfer-split! split delta)
             (if (eq? orientation 'right)
                 (set-car! (cddr gesture) now)
                 (set-car! (cdddr gesture) now))))]
        [else
         (head:window-at (- x 1) (- y 1)
           (lambda (entry)
             (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
               (when (and (eq? w (head:current-window)) (text-gesture? w)
                          (< (- y 1) (+ start height)))
                 (head:goto! (paint:window-position w start height x y))
                 (if (head:app-buffer? (head:window-buffer w))
                     (unless (call-with-app-mouse-event w start height x y button
                               (lambda () (head:dispatch-app-event! "MOUSE-DRAG")))
                       (head:buffer-marked-set! (head:window-buffer w) #t))
                     (head:buffer-marked-set! (head:window-buffer w) #t))))))])))

  (define (mouse-release! x y button)
    (head:window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
          (when (and (eq? w (head:current-window)) (text-gesture? w)
                     (< (- y 1) (+ start height))
                     (head:app-buffer? (head:window-buffer w)))
            (head:goto! (paint:window-position w start height x y))
            (call-with-app-mouse-event w start height x y button
              (lambda () (head:dispatch-app-event! "MOUSE-RELEASE"))))))))

  ;;; The wheel -----------------------------------------------------------------------

  (define (wheel-mover dir)
    ;; Wheel direction (the low bits of a 64-flagged button): up, down,
    ;; left, right. Vertical ticks move the hovered viewport by one eighth
    ;; of its height; horizontal ones move point sideways within its line.
    (case dir
      [(0) (lambda () (edit:page-window! -1 8))]
      [(1) (lambda () (edit:page-window! 1 8))]
      [(2) (lambda () (let ([p (head:point)]) (head:goto! (cons (car p) (- (cdr p) 3)))))]
      [(3) (lambda () (let ([p (head:point)]) (head:goto! (cons (car p) (+ (cdr p) 3)))))]
      [else (lambda () (void))]))

  (define (mouse-wheel! x y button dir meta? shift?)
    ;; Scroll the window under the pointer; the focused window stays focused.
    ;; Meta-wheel applies the corresponding global buffer-switch binding to
    ;; the hovered window instead. Apps get an ordinary directional tick
    ;; first so list controls can choose their wheel step.
    (head:window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([old (head:current-window)] [w (car entry)])
          (head:set-current! w)
          (head:follow-app! w #f)
          (if (and meta? (memv dir '(0 1)))
              (dispatch:global-key! (if (= dir 0) "M-S-UP" "M-S-DOWN"))
              (unless (parameterize ([head:app-event-focus old])
                        (call-with-app-mouse-event w (cadr entry) (caddr entry) x y button
                          (lambda ()
                            (head:dispatch-app-event!
                              (string-append
                                (if shift? "S-" "")
                                (case dir
                                  [(0) "WHEEL-UP"]
                                  [(1) "WHEEL-DOWN"]
                                  [(2) "WHEEL-LEFT"]
                                  [(3) "WHEEL-RIGHT"]
                                  [else "WHEEL"]))))))
                ((wheel-mover dir))))
          (when (memq old (head:windows)) (head:set-current! old))
          "MOUSE-HANDLED"))))

  ;;; Pointer motion --------------------------------------------------------------------

  ;; Input decoding lives in (tty): the head's reader thread calls
  ;; (tty:read-event stdin); the main thread applies the parsed mouse
  ;; data below, through the handler init! installs on the pump.

  (define hover-window #f)   ; the window whose local app last heard MOUSE-MOVE

  (define (tell-app! w event entry x y)
    ;; Deliver a pointer event to w's local app as the selected window,
    ;; then restore the selection: pointing focuses nothing.  Shared apps
    ;; are not told; their capture is for the keys and clicks the wire
    ;; carries.
    (when (and (memq w (head:windows)) (head:app-of (head:window-buffer w)))
      (let ([old (head:current-window)])
        (head:set-current! w)
        (parameterize ([head:app-event-focus old])
          (if entry
              (call-with-app-mouse-event w (cadr entry) (caddr entry) x y 35
                (lambda () (head:dispatch-app-event! event)))
              (head:dispatch-app-event! event)))
        (when (memq old (head:windows)) (head:set-current! old)))))

  (define (mouse-move! x y)
    ;; Pointer motion without a button (any-event tracking).  The local
    ;; app whose text is under the pointer hears MOUSE-MOVE with the
    ;; usual event coordinates; the one the pointer left hears
    ;; MOUSE-LEAVE.  Chrome -- status bars, dividers, scrollbars, the
    ;; echo area -- counts as leaving.
    (let ([target
           (head:window-at (- x 1) (- y 1)
             (lambda (entry)
               (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
                 (and (< (- y 1) (+ start height))
                      (head:app-of (head:window-buffer w))
                      (not (and (head:window-scrollbar-column w)
                                (= (- x 1) (head:window-scrollbar-column w))))
                      entry))))])
      (when (and hover-window (not (eq? hover-window (and target (car target)))))
        (tell-app! hover-window "MOUSE-LEAVE" #f x y)
        (set! hover-window #f))
      (when target
        (tell-app! (car target) "MOUSE-MOVE" target x y)
        (set! hover-window (car target)))))

  (define (apply-mouse-event! handle? c b x y)
    ;; Wheel is button 64/65; releases are ignored.  Pointer motion
    ;; without a button only moves hover state and is never an event
    ;; for the loop, so it settles nothing.  A context that must not
    ;; change editor focus passes handle? #f: the report is consumed
    ;; without being applied.
    (cond [(and (char=? c #\M) (= (bitwise-and b 3) 3)      ; motion
                (= (bitwise-and b 32) 32) (zero? (bitwise-and b 64)))
           (when handle? (mouse-move! x y))
           'ignore]
          [(not handle?) #f]
          [(char=? c #\m)                         ; release
           (mouse-release! x y b)
           (head:set-drag! #f)
           "MOUSE-HANDLED"]
          [(= (bitwise-and b 64) 64)               ; wheel
           (mouse-wheel! x y b (bitwise-and b 3)
                         (= (bitwise-and b 8) 8)
                         (= (bitwise-and b 4) 4))]
          [(= (bitwise-and b 32) 32)               ; drag
           (when (< (bitwise-and b 3) 3)
             (mouse-drag! x y b))
           "MOUSE-HANDLED"]
          [(< (bitwise-and b 3) 3)                 ; a press
           (mouse-press! x y b)]
          [else "MOUSE-HANDLED"]))

  (edoc "Install the mouse: the handler the head's pump applies to every parsed mouse report.")
  (define (init!)
    (head:set-mouse-handler! apply-mouse-event!)))
