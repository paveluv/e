;; head.e -- the head: the library (head), v2 core dissolution
;; (docs/DESIGN2.md).  Pure infrastructure with no init!.
;;
;; A head is one user's seat -- in the wire's terms, the client side:
;; its buffers as it sees them, its windows and their layout, which
;; one is selected, and the loop that feeds it events.  This module
;; owns the records and the geometry (the seat's buffer record, the
;; window record, the persistent split tree and its tiling) and the
;; scheduling pump (the mailbox, wakes, posted thunks, the input
;; reader).  Routing (apps, capture), wrap policy, painting, and the
;; command loop's body stay with the core until they move; the core
;; reaches this state through identifier-syntax facades and installs
;; the pump's handlers.

(library (head)
  (export buffer make-buffer buffer?
          buffer-name buffer-name-set!
          buffer-lines buffer-lines-raw-set!
          buffer-revision buffer-revision-set!
          buffer-history buffer-history-set!
          buffer-mark-row buffer-mark-row-set!
          buffer-mark-col buffer-mark-col-set!
          buffer-marked buffer-marked-set!
          buffer-spot-row buffer-spot-row-set!
          buffer-spot-col buffer-spot-col-set!
          buffer-spot-top buffer-spot-top-set!
          buffer-line-numbers-setting buffer-line-numbers-setting-set!
          buffer-state-id buffer-state-id-set!
          buffer-state-rev buffer-state-rev-set!
          buffers set-buffers!
          make-window window?
          window-buffer window-buffer-set!
          window-top window-top-set!
          window-topseg window-topseg-set!
          window-left window-left-set!
          window-prow window-prow-set!
          window-pcol window-pcol-set!
          window-shown-top window-shown-top-set!
          window-size window-size-set!
          window-goal window-goal-set!
          window-xoff window-xoff-set!
          window-width window-width-set!
          window-wgoal window-wgoal-set!
          window-wrap window-wrap-set!
          make-layout-split layout-split?
          layout-split-orientation
          layout-split-first layout-split-first-set!
          layout-split-second layout-split-second-set!
          layout-split-first-weight layout-split-first-weight-set!
          layout-split-second-weight layout-split-second-weight-set!
          layout-leaves layout-replace layout-parent
          layout-min-width layout-min-height weighted-first
          layout-node!
          min-window-lines
          windows set-windows! root set-root! current set-current!
          dividers set-dividers!
          read-key-event run-on-main! wake-main! in-main-pump
          take-deferred! start-input-reader! set-pump-handlers!
          tile! layout window-at window-button-at divider-at
          transfer-split! drag set-drag! double-click?)
  (import (rnrs) (rnrs r5rs)
          (only (chezscheme)
                make-parameter make-mutex with-mutex fork-thread void)
          (only (sys) duplicate-standard-input-port)
          (prefix (kernel) kernel:)
          (prefix (tty) tty:))

  ;;; The records ----------------------------------------------------------------

  ;; The seat's working record for a store buffer: a client-side cache
  ;; of the store's immutable text (adopted, never mutated in place)
  ;; plus what only this seat cares about -- its selection, where it
  ;; last was, its line-number toggle.  Buffer-level facts (file,
  ;; mode, read-only, disk base, wrap, an app's presentation) are
  ;; store properties, not fields:
  ;; every head, local or across the wire, reads the same truth.
  (define-record-type buffer
    (fields (mutable name)          ; a cache of the store's label
            (mutable lines buffer-lines buffer-lines-raw-set!)
            (mutable revision)      ; the seat's repaint counter
            (mutable history)       ; the seat's snapshot undo
            (mutable mark-row) (mutable mark-col)
            (mutable marked)
            ;; where point was when the buffer was last displayed
            (mutable spot-row) (mutable spot-col) (mutable spot-top)
            ;; #t/#f after a local toggle, or default to follow the global
            ;; line-numbers parameter
            (mutable line-numbers buffer-line-numbers-setting
                     buffer-line-numbers-setting-set!)
            ;; the buffer's twin in the (state) store, and the store
            ;; revision this buffer's lines last agreed with
            (mutable state-id) (mutable state-rev)))

  (define-record-type window
    (fields (mutable buffer) (mutable top)
            ;; a soft-wrapping window may start mid-line: the first
            ;; visible segment of the top line (0 elsewhere)
            (mutable topseg)
            (mutable left)
            (mutable prow) (mutable pcol)
            ;; the top row last drawn, for native scrolling
            (mutable shown-top)
            ;; text height in screen lines, written by the layout: the
            ;; goal is the user's chosen proportion, and the layout
            ;; realizes the goals in whatever space is there --
            ;; recomputed fresh each redraw, so temporary changes (a
            ;; grown echo area, a pop-up split) never drift them
            (mutable size)
            (mutable goal)
            ;; horizontal band geometry, written by the layout: the
            ;; window's first screen column and its width
            (mutable xoff)
            (mutable width)
            ;; column proportion within a band shared side by side
            (mutable wgoal)
            ;; soft-wrap long lines onto continuation rows instead of
            ;; scrolling horizontally
            (mutable wrap)))

  (define-record-type layout-split
    (fields orientation (mutable first) (mutable second)
            (mutable first-weight) (mutable second-weight)))

  ;;; The head's seat -------------------------------------------------------------

  ;; Core-linked (never reloaded in place): plain module state.  The
  ;; core initializes these at startup and reads/writes them through
  ;; its facades; set-layout-root! (app-aware) stays there.

  (define the-buffers '())      ; the seat's buffers, most recent first
  (define the-windows '())      ; every live window, layout order
  (define the-root #f)          ; the persistent split tree
  (define the-current #f)       ; the selected window
  (define the-dividers '())     ; layout output: divider rectangles

  (define (buffers) the-buffers)
  (define (set-buffers! bs) (set! the-buffers bs))
  (define (windows) the-windows)
  (define (set-windows! ws) (set! the-windows ws))
  (define (root) the-root)
  (define (set-root! node) (set! the-root node))
  (define (current) the-current)
  (define (set-current! w) (set! the-current w))
  (define (dividers) the-dividers)
  (define (set-dividers! ds) (set! the-dividers ds))

  (define min-window-lines
    ;; the smallest text height a split may leave a window
    (make-parameter 3 (lambda (v) (max 1 v))))

  ;;; Tree geometry ----------------------------------------------------------------

  (define (layout-leaves node)
    (if (layout-split? node)
        (append (layout-leaves (layout-split-first node))
                (layout-leaves (layout-split-second node)))
        (list node)))

  (define (layout-replace node old replacement)
    (cond
      [(eq? node old) replacement]
      [(layout-split? node)
       (layout-split-first-set!
         node (layout-replace (layout-split-first node) old replacement))
       (layout-split-second-set!
         node (layout-replace (layout-split-second node) old replacement))
       node]
      [else node]))

  (define (layout-parent node child)
    (and (layout-split? node)
         (if (or (eq? child (layout-split-first node))
                 (eq? child (layout-split-second node)))
             node
             (or (layout-parent (layout-split-first node) child)
                 (layout-parent (layout-split-second node) child)))))

  (define (layout-min-width node)
    (if (layout-split? node)
        (if (eq? (layout-split-orientation node) 'right)
            (+ 1 (layout-min-width (layout-split-first node))
               (layout-min-width (layout-split-second node)))
            (max (layout-min-width (layout-split-first node))
                 (layout-min-width (layout-split-second node))))
        20))

  (define (layout-min-height node)
    (if (layout-split? node)
        (if (eq? (layout-split-orientation node) 'below)
            (+ (layout-min-height (layout-split-first node))
               (layout-min-height (layout-split-second node)))
            (max (layout-min-height (layout-split-first node))
                 (layout-min-height (layout-split-second node))))
        (+ (min-window-lines) 1)))

  (define (weighted-first total minimum-first minimum-second a b)
    (min (- total minimum-second)
         (max minimum-first (quotient (* total (max 1 a))
                                      (+ (max 1 a) (max 1 b))))))

  (define (layout-node! node x y width height)
    ;; Lay out one persistent subtree. Rectangles include leaf status rows;
    ;; side-by-side nodes reserve one visible divider column.  Divider
    ;; rectangles accumulate in (dividers) for the painter and the
    ;; mouse's drag hit-testing.
    (if (not (layout-split? node))
        (begin
          (window-xoff-set! node x)
          (window-width-set! node (max 1 width))
          (window-size-set! node (max 1 (- height 1)))
          (list (list node y (max 1 (- height 1)))))
        (let* ([first (layout-split-first node)]
               [second (layout-split-second node)]
               [below? (eq? (layout-split-orientation node) 'below)]
               [total (- (if below? height width) (if below? 0 1))]
               [m1 (if below? (layout-min-height first)
                       (layout-min-width first))]
               [m2 (if below? (layout-min-height second)
                       (layout-min-width second))]
               [one (weighted-first total m1 m2
                                    (layout-split-first-weight node)
                                    (layout-split-second-weight node))]
               [two (- total one)])
          (if below?
              (begin
                (set! the-dividers
                  (cons (list 'below node x (+ y one -1) width)
                        the-dividers))
                (append (layout-node! first x y width one)
                        (layout-node! second x (+ y one) width two)))
              (begin
                (set! the-dividers
                  (cons (list 'right node (+ x one) y height)
                        the-dividers))
                (append (layout-node! first x y one height)
                        (layout-node! second (+ x one 1) y two
                                      height)))))))

  ;;; The seat's loop -------------------------------------------------------------

  ;; The scheduling substrate: a dedicated thread owns the terminal
  ;; input (through a private dup'd port, so its blocking reads never
  ;; hold a console lock) and posts parsed events to the seat's
  ;; mailbox; any thread may post a wake or a thunk.  read-key-event --
  ;; called synchronously by the main loop, prompts, i-search,
  ;; everything -- is the mailbox pump: between keys it services
  ;; wake-ups and posted thunks.  How those are serviced is the
  ;; owner's business, installed once as handlers.  A remote head runs
  ;; the same loop with a socket reader posting in place of the tty.

  (define mailbox (kernel:make-mailbox))
  (define deferred '())          ; thunks posted during a nested pump

  (define (run-on-main! thunk)
    ;; run thunk on the main thread: immediately when the main loop is
    ;; the one pumping the mailbox, otherwise at the top of its loop
    (kernel:mailbox-post! mailbox (cons 'run thunk)))

  ;; A burst of foreign edits (an agent's tight loop, a chatty PTY)
  ;; must not queue one repaint per event: a wake is posted only when
  ;; none is outstanding, so a burst collapses into one frame.  The
  ;; claim happens before the frame is painted, never after -- a wake
  ;; arriving mid-paint queues the next frame instead of being lost.
  (define wake-lock (make-mutex))
  (define wake-queued #f)

  (define (wake-main!)
    (when (with-mutex wake-lock
            (and (not wake-queued) (begin (set! wake-queued #t) #t)))
      (kernel:mailbox-post! mailbox '(wake))))

  (define (claim-wake!)
    (with-mutex wake-lock (set! wake-queued #f)))

  ;; #t while the main loop itself pumps the mailbox: posted thunks
  ;; may run right away.  Nested pumps (prompts, i-search, key
  ;; describers) leave it #f and defer them, so a foreign thunk never
  ;; runs in the middle of a modal read.
  (define in-main-pump (make-parameter #f))

  (define (take-deferred!)
    ;; the thunks a nested pump set aside, oldest first
    (let ([runs (reverse deferred)])
      (set! deferred '())
      runs))

  ;; The owner's handlers: on-wake services a frame (sync foreign
  ;; state, repaint); on-run runs a posted thunk with error reporting;
  ;; on-mouse applies a report -- (on-mouse handle? c b x y) -> an
  ;; event string or #f; on-paste stashes pasted text; on-host takes a
  ;; host color-scheme report.
  (define on-wake void)
  (define on-run (lambda (thunk) (thunk)))
  (define on-mouse (lambda (handle? c b x y) #f))
  (define on-paste (lambda (text) (void)))
  (define on-host (lambda (scheme) (void)))

  (define (set-pump-handlers! wake run mouse paste host)
    (set! on-wake wake)
    (set! on-run run)
    (set! on-mouse mouse)
    (set! on-paste paste)
    (set! on-host host))

  (define (start-input-reader!)
    (let ([stdin (duplicate-standard-input-port)])
      (fork-thread
        (lambda ()
          (let loop ()
            (let ([event (guard (ex [else (eof-object)])
                           (tty:read-event stdin))])
              (kernel:mailbox-post! mailbox (cons 'key event))
              (unless (eof-object? event) (loop))))))))

  (define read-key-event
    ;; Consumers see the same names whether they are the main editor,
    ;; I-search, a prompt, or a key describer.  A context that must not
    ;; change editor focus passes #f: mouse reports are consumed
    ;; without being applied.
    (case-lambda
      [() (read-key-event #t)]
      [(handle-mouse?)
       (let pump ()
         (let ([message (kernel:mailbox-receive! mailbox)])
           (case (car message)
             [(key)
              (let ([event (cdr message)])
                (cond
                  [(not (pair? event)) event]
                  [(eq? (car event) 'mouse)
                   (or (apply on-mouse handle-mouse? (cdr event))
                       "MOUSE-HANDLED")]
                  [(eq? (car event) 'paste)
                   (on-paste (cdr event))
                   "PASTE"]
                  [(eq? (car event) 'host-color-scheme)
                   (on-host (cadr event))
                   (pump)]
                  [else (pump)]))]
             [(wake)
              (claim-wake!)
              (on-wake)
              (pump)]
             [(run)
              (cond [(in-main-pump)
                     (on-run (cdr message))
                     (on-wake)]
                    [else
                     (set! deferred (cons (cdr message) deferred))])
              (pump)]
             [else (pump)])))]))
  ;;; Tiling and hit-testing -------------------------------------------------------

  ;; The last tiling is remembered: mouse hit-testing asks where the
  ;; user clicked on the screen the user saw.  Entries are
  ;; (window start text-height), start 0-based, status row at
  ;; start + text-height; divider descriptors are
  ;; (orientation split x y span).

  (define the-layout '())

  (define (layout) the-layout)

  (define (tile! width height)
    ;; Tile the tree into width x height (the screen minus the echo
    ;; area).  -> the entries, also remembered along with the dividers.
    (set! the-dividers '())
    (set! the-layout (layout-node! the-root 0 0 width height))
    the-layout)

  (define (window-at x0 r0 receiver)
    ;; Call receiver with the layout entry containing 0-based screen
    ;; position (x0, r0) (text rows or the status line); #f in the
    ;; echo area or on a divider.
    (let loop ([entries the-layout])
      (cond [(null? entries) #f]
            [(and (<= (cadr (car entries)) r0
                      (+ (cadr (car entries)) (caddr (car entries))))
                  (<= (window-xoff (caar entries)) x0
                      (+ (window-xoff (caar entries))
                         (window-width (caar entries))
                         -1)))
             (receiver (car entries))]
            [else (loop (cdr entries))])))

  (define (window-button-at x0 r0)
    ;; The three bracketed status-line controls occupy the last nine
    ;; columns: (close . w), (right . w), (below . w), or #f.
    (window-at x0 r0
      (lambda (entry)
        (let ([w (car entry)])
          (and (= r0 (+ (cadr entry) (caddr entry)))
               (let ([from-end (- (+ (window-xoff w) (window-width w)) x0)])
                 (cond [(<= 1 from-end 3) (cons 'close w)]
                       [(<= 4 from-end 6) (cons 'right w)]
                       [(<= 7 from-end 9) (cons 'below w)]
                       [else #f])))))))

  (define (divider-at x0 r0)
    ;; The divider descriptor under (x0, r0), or #f.  A crossing
    ;; visually belongs to the spanning horizontal split.
    (define (hit? orientation d)
      (and (eq? (car d) orientation)
           (if (eq? orientation 'right)
               (and (= x0 (caddr d))
                    (<= (cadddr d) r0)
                    (< r0 (+ (cadddr d) (list-ref d 4))))
               (and (= r0 (cadddr d))
                    (<= (caddr d) x0)
                    (< x0 (+ (caddr d) (list-ref d 4)))))))
    (or (find (lambda (d) (hit? 'below d)) the-dividers)
        (find (lambda (d) (hit? 'right d)) the-dividers)))

  (define (transfer-split! split delta)
    ;; Normalize stale ratio weights to the currently realized cell
    ;; extents, then move the boundary by delta cells -- so a mouse
    ;; drag (or a keyboard step) is exact even after a resize.
    (unless (= delta 0)
      (let* ([orientation (layout-split-orientation split)]
             [first (layout-split-first split)]
             [second (layout-split-second split)])
        (define (extent node)
          (let ([entries
                 (map (lambda (w) (assq w the-layout)) (layout-leaves node))])
            (if (eq? orientation 'right)
                (- (apply max
                          (map (lambda (entry)
                                 (+ (window-xoff (car entry))
                                    (window-width (car entry))))
                               entries))
                   (apply min
                          (map (lambda (entry) (window-xoff (car entry)))
                               entries)))
                (- (apply max
                          (map (lambda (entry)
                                 (+ (cadr entry) (caddr entry) 1))
                               entries))
                   (apply min (map cadr entries))))))
        (let* ([one (extent first)] [two (extent second)]
               [m1 (if (eq? orientation 'right)
                       (layout-min-width first)
                       (layout-min-height first))]
               [m2 (if (eq? orientation 'right)
                       (layout-min-width second)
                       (layout-min-height second))]
               [delta (min delta (- two m2))]
               [delta (max delta (- m1 one))])
          (layout-split-first-weight-set! split (+ one delta))
          (layout-split-second-weight-set! split (- two delta))))))

  ;;; Gestures -----------------------------------------------------------------------

  (define the-drag #f)        ; the divider descriptor being dragged, or #f
  (define the-last-press #f)  ; (x y ms) of the previous button press

  (define (drag) the-drag)
  (define (set-drag! d) (set! the-drag d))

  (define (double-click? x y now)
    ;; Record a press at (x, y) at time now (ms); #t when it repeats
    ;; the previous press's cell within half a second.
    (let ([prev the-last-press])
      (set! the-last-press (list x y now))
      (and prev
           (= (car prev) x) (= (cadr prev) y)
           (< (- now (caddr prev)) 450))))
)
