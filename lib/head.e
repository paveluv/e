;; head.e -- the head: the library (head).  Pure infrastructure with
;; no init!.
;;
;; A head is one user's seat -- in the wire's terms, the client side:
;; its buffers as it sees them, its windows and their layout, which
;; one is selected, and the pump that feeds it events.  This module
;; owns the records and the geometry (the seat's buffer record, the
;; window record, the persistent split tree and its tiling), the
;; scheduling pump (the mailbox, wakes, posted thunks, the input
;; reader), the store client, the app registry, and the seat's
;; per-user state (kill ring, paste text, the last command).  Key
;; dispatch and the loop's body live in (main), painting in (paint),
;; the commands in (edit); the command layer still reaches the seat's
;; state through identifier-syntax facades, and two hooks reach up
;; from the pump: the frame and the mouse.

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
          kill-ring set-kill-ring! read-paste set-pending-paste!
          call-uninterrupted call-with-interrupt interrupted? make-interrupted
          make-window window?
          window-buffer window-buffer-set!
          window-top window-top-set!
          window-topseg window-topseg-set!
          window-left window-left-set!
          window-prow window-prow-set!
          window-pcol window-pcol-set!
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
          set-layout-root! replace-layout-window! fit-layout!
          layout-min-width layout-min-height weighted-first
          layout-node!
          min-window-lines
          windows set-windows! root set-root! current set-current!
          dividers set-dividers!
          read-key-event run-on-main! wake-main! in-main-pump
          run-deferred! start-input-reader! set-frame-hook! set-mouse-handler!
          quit! quitting? last-command set-last-command!
          current-keys set-current-keys! escaped-buffer set-escaped-buffer!
          dispatch-app-event!
          host-color-scheme add-color-scheme-hook!
          tile! layout window-at window-button-at divider-at
          transfer-split! drag set-drag! double-click?
          ui-actor buffer-fact buffer-fact-set!
          buffer-file buffer-file-set! buffer-trailing buffer-trailing-set!
          buffer-modified buffer-modified-set!
          buffer-mode-auto buffer-mode-auto-set!
          buffer-read-only buffer-read-only-set!
          buffer-stamp buffer-stamp-set! buffer-base buffer-base-set!
          buffer-stale buffer-stale-set!
          mirror-create! adopt-state! adopt-local! reconverge-forked!
          state-reset! state-edit! mirror-rename! new-buffer
          bump-buffer-revision! buffer-of-state-id adopt-store-buffer!
          buffer-lines-set! clamp-buffer-positions!
          sync-foreign-edits! flush-ui-audit!
          set-repaint-hook! set-adopt-hook!
          add-buffer-kill-hook! add-pre-redraw-hook!
          before-frame! add-shutdown-hook! run-shutdown-hooks!
          registered-apps app-of app-buffer? detach-app! register-app!
          set-app-cursor-visible! set-app-manages-viewport!
          set-app-status-position! app-cursor-visible-in?
          app-manages-window-viewport? set-app-presentation!
          buffer-sticky-lines scrollbar scrollbar-position line-numbers
          buffer-line-numbers window-line-number-width
          window-scrollbar? window-content-width buffer-narrowest-width
          buffer-window-size window-scrollbar-column register-view!
          view-buffer? refresh-visible-views! view-append!
          view-replace! forget-buffer! set-window-buffer! buffer-named
          app-buffer app-refresh! app-handle-event! app-refresh-error
          app-refresh-error-set! app-cursor-visible?
          app-cursor-visible?-set! app-status-position
          app-status-position-set! make-app app?)
  (import (rnrs) (rnrs r5rs)
          (only (chezscheme) keyboard-interrupt-handler
                make-parameter make-mutex with-mutex fork-thread void
                format remq cons* time-second current-time
                make-weak-eq-hashtable
                call-with-string-output-port)
          (prefix (only (sys) terminal-isig! duplicate-standard-input-port) sys:)
          (prefix (kernel) kernel:)
          (prefix (tty) tty:)
          (prefix (state) state:)
          (prefix (text) text:)
          (prefix (actor) actor:)
          (prefix (log) log:))

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
            ;; text height in screen lines, written by the layout: the
            ;; goal is the user's chosen proportion, and the layout
            ;; realizes the goals in whatever space is there --
            ;; recomputed fresh each redraw, so temporary changes (a
            ;; grown echo area) never drift them
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

  ;; main links against this library, so it is never reloaded in place
  ;; and plain module state suffices.  The seat's first state (a
  ;; *scratch* buffer in one window) is set at the end of this file;
  ;; the command layer reads and writes these through its facades.

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

  ;; The seat's kill ring: one string, the last kill; commands and
  ;; prompts read and replace it.
  (define the-kill-ring "")
  (define (kill-ring) the-kill-ring)
  (define (set-kill-ring! s) (set! the-kill-ring s))

  ;; The text of the bracketed paste just consumed: the pump's paste
  ;; handler stashes it, the PASTE key's command reads it.
  (define pending-paste "")
  (define (read-paste) pending-paste)
  (define (set-pending-paste! text) (set! pending-paste text))
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

  (define (set-layout-root! root)
    ;; the tree is the seat's windows: replacing it replaces them
    (set! the-root root)
    (set! the-windows (layout-leaves root)))

  (define (replace-layout-window! old replacement)
    (set-layout-root! (layout-replace the-root old replacement)))

  (define (fit-layout! width height)
    ;; A screen too small for the splits collapses the tree back to
    ;; one window -- the current one.
    (when (and (pair? (cdr (layout-leaves the-root)))
               (or (< width (layout-min-width the-root))
                   (< height (layout-min-height the-root))))
      (set-layout-root! (if (memq the-current (layout-leaves the-root))
                            the-current
                            (car (layout-leaves the-root))))))

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
  ;; wake-ups and posted thunks.  The seat services its own side
  ;; effects -- a paste's text, a host's color report, a posted thunk's
  ;; error, the store's news before a frame; two hooks reach up: the
  ;; frame (the painter's) and the mouse (the commands').  A remote
  ;; head runs the same loop with a socket reader posting in place of
  ;; the tty.

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

  (define (run-posted! thunk)
    ;; a posted thunk's error is news, not a crash
    (guard (ex [else (log:add! 'run-on-main! (kernel:condition-text ex))])
      (thunk)))

  (define (run-deferred!)
    ;; the thunks a nested pump set aside, oldest first
    (let ([runs (reverse deferred)])
      (set! deferred '())
      (for-each run-posted! runs)))

  ;; The two hooks: the frame hook paints a frame (the painter's,
  ;; above); the mouse handler applies a report -- (handler handle? c b
  ;; x y) -> an event string or #f (the commands', above).
  (define frame-hook void)
  (define mouse-handler (lambda (handle? c b x y) #f))

  (define (set-frame-hook! proc) (set! frame-hook proc))
  (define (set-mouse-handler! proc) (set! mouse-handler proc))

  (define (frame!)
    ;; a frame on a wake: the store's news first, then the paint
    (before-frame!)
    (frame-hook))

  ;; The host's color scheme, learned from its DSR 997 reports (mode
  ;; 2031 subscribes to them at startup): #f until the host says, then
  ;; 'dark or 'light.  Hooks run on the main thread whenever a report
  ;; arrives, so the terminal module can forward the change to
  ;; subscribed children.
  (define host-color-scheme-value #f)

  (define (host-color-scheme) host-color-scheme-value)

  (define color-scheme-hooks '())

  (define (add-color-scheme-hook! hook)
    (unless (procedure? hook)
      (error 'add-color-scheme-hook! "expected a procedure" hook))
    (set! color-scheme-hooks (cons hook color-scheme-hooks)))

  (define (note-color-scheme! scheme)
    (set! host-color-scheme-value scheme)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook scheme)))
              color-scheme-hooks))

  ;; The seat's lifetime, and the command the dispatcher ran last (kill
  ;; chaining and typed runs ask).
  (define quit-requested #f)
  (define (quit!) (set! quit-requested #t))
  (define (quitting?) quit-requested)

  (define the-last-command #f)
  (define (last-command) the-last-command)
  (define (set-last-command! c) (set! the-last-command c))

  ;; the key sequence being dispatched -- the self-inserting command
  ;; reads its character here
  (define the-current-keys '())
  (define (current-keys) the-current-keys)
  (define (set-current-keys! keys) (set! the-current-keys keys))

  ;; the app buffer whose capture the dispatcher is escaping, from the
  ;; escape key until the command it introduces returns; #f otherwise.
  ;; The buffer's status hint and cursor show that the keys are the
  ;; editor's meanwhile.
  (define the-escaped-buffer #f)
  (define (escaped-buffer) the-escaped-buffer)
  (define (set-escaped-buffer! b) (set! the-escaped-buffer b))

  (define (start-input-reader!)
    (let ([stdin (sys:duplicate-standard-input-port)])
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
                   (or (apply mouse-handler handle-mouse? (cdr event))
                       "MOUSE-HANDLED")]
                  [(eq? (car event) 'paste)
                   (set! pending-paste (cdr event))
                   "PASTE"]
                  [(eq? (car event) 'host-color-scheme)
                   (note-color-scheme! (cadr event))
                   (pump)]
                  [else (pump)]))]
             [(wake)
              (claim-wake!)
              (frame!)
              (pump)]
             [(run)
              (cond [(in-main-pump)
                     (run-posted! (cdr message))
                     (frame!)]
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

  ;;; The store client -----------------------------------------------------------

  ;; The bridge between this seat's buffer records and the (state)
  ;; store: the seat is the store's client -- over the wire, a remote
  ;; seat is exactly this code with a socket under the state: calls.
  ;; Records cache the store's immutable text and adopt it after every
  ;; operation; this seat's edits enter the store transactionally;
  ;; wholesale replacements are resets; foreign actors' operations
  ;; flow back before each frame (sync-foreign-edits!); buffer facts
  ;; are store properties.  If the store refuses (a foreign edit
  ;; overlapped mid-command) or breaks, the seat keeps editing:
  ;; content wins locally and the store is reset to match.
  ;;
  ;; Two hooks reach upward, each installed by the module that owns
  ;; the answer: the painter invalidates its screen (set-repaint-hook!),
  ;; the mode registry gives an adopted buffer a mode (set-adopt-hook!).

  (define repaint-hook void)
  (define adopt-hook (lambda (b) (void)))

  (define (set-repaint-hook! proc) (set! repaint-hook proc))
  (define (set-adopt-hook! proc) (set! adopt-hook proc))

  (define (line-count b) (vector-length (buffer-lines b)))

  ;; Shared facts, read and written through the store.  The fallbacks
  ;; only cover a buffer whose twin is missing (a store outage, a
  ;; failed mirror creation); every created buffer initializes its
  ;; managed facts, so an absent property reads honestly as #f.
  ;;
  ;;   file    the visited path, or #f
  ;;   trailing whether the file ends in a newline
  ;;   modified unsaved changes (any actor's)
  ;;   mode    the buffer's mode NAME -- the registry record never
  ;;           crosses the seam; find-mode resolves it on read, so a
  ;;           reloaded mode module is picked up live
  ;;   mode-auto whether the mode came from detection
  ;;   read-only
  ;;   stamp/base the disk state last agreed with: the mtime raising
  ;;           suspicion cheaply, and the content as loaded or last
  ;;           saved -- the base for comparisons and three-way merges
  ;;   stale   a detected external change, worn as a red !! until a
  ;;           save settles it

  (define (buffer-fact b key fallback)
    (let ([id (buffer-state-id b)])
      (if id
          (guard (ex [else fallback]) (state:property id key))
          fallback)))

  (define (buffer-fact-set! b key value)
    (guard (ex [else (void)])
      (when (buffer-state-id b)
        (state:set-property! ui-actor (buffer-state-id b) key value))))

  (define (buffer-file b) (buffer-fact b 'file #f))
  (define (buffer-file-set! b v) (buffer-fact-set! b 'file v))
  (define (buffer-trailing b) (buffer-fact b 'trailing #t))
  (define (buffer-trailing-set! b v) (buffer-fact-set! b 'trailing v))
  (define (buffer-modified b) (buffer-fact b 'modified #f))
  (define (buffer-modified-set! b v) (buffer-fact-set! b 'modified v))
  (define (buffer-mode-auto b) (buffer-fact b 'mode-auto #t))
  (define (buffer-mode-auto-set! b v) (buffer-fact-set! b 'mode-auto v))
  (define (buffer-read-only b) (buffer-fact b 'read-only #f))
  (define (buffer-read-only-set! b v) (buffer-fact-set! b 'read-only v))
  (define (buffer-stamp b) (buffer-fact b 'stamp #f))
  (define (buffer-stamp-set! b v) (buffer-fact-set! b 'stamp v))
  (define (buffer-base b) (buffer-fact b 'base #f))
  (define (buffer-base-set! b v) (buffer-fact-set! b 'base v))
  (define (buffer-stale b) (buffer-fact b 'stale #f))
  (define (buffer-stale-set! b v) (buffer-fact-set! b 'stale v))

  (define ui-actor '(head main))

  (define (mirror-create! b)
    (guard (ex [else (void)])
      (buffer-state-id-set!
        b (state:create! ui-actor (buffer-name b)
                         (vector->list (buffer-lines b))))
      (buffer-state-rev-set! b 0)))

  (define (adopt-state! b)
    ;; make the cache the store's current text -- the vectors are
    ;; immutable, so adoption is reference sharing, never a copy
    (let-values ([(text revision) (state:snapshot (buffer-state-id b))])
      (buffer-lines-raw-set! b text)
      (buffer-state-rev-set! b revision)
      (bump-buffer-revision! b)))

  ;; Store outage: buffers whose cache forked from the store because a
  ;; store call failed.  Each fork is logged once, edits stay local
  ;; (never half-and-half), and every frame re-converges what it can.
  (define forked-buffers '())

  (define (adopt-local! b text)
    ;; the store is unreachable: keep editing on the local cache
    ;; alone -- on the record, and queued for re-convergence
    (buffer-lines-raw-set! b text)
    (bump-buffer-revision! b)
    (when (and (buffer-state-id b) (not (memq b forked-buffers)))
      (set! forked-buffers (cons b forked-buffers))
      (guard (ex [else (void)])
        (log:add! 'state
          (format "store outage: ~s forked from the store"
                  (buffer-name b))))))

  (define (reconverge-forked!)
    ;; recovery, at frame time: re-baseline each forked buffer from
    ;; its cache; a store still down keeps the buffer queued, a dead
    ;; buffer drops out.  A twin that no longer exists means another
    ;; actor deleted the buffer: the head forgets it rather than
    ;; resurrecting what someone killed (the lifecycle sync normally
    ;; gets there first).
    (when (pair? forked-buffers)
      (set! forked-buffers
        (filter
          (lambda (b)
            (guard (ex [else #t])
              (cond
                [(not (memq b the-buffers)) #f]
                [(not (state:exists? (buffer-state-id b)))
                 (buffer-state-id-set! b #f)
                 (forget-buffer! b)
                 #f]
                [else
                 (state:reset! ui-actor (buffer-state-id b)
                               (buffer-lines b))
                 (adopt-state! b)
                 (log-reconvergence! b)
                 #f])))
          forked-buffers))))

  (define (log-reconvergence! b)
    (guard (ex [else (void)])
      (log:add! 'state
        (format "store recovered: ~s re-baselined from the editor"
                (buffer-name b)))))

  (define (state-reset! b new-lines)
    ;; wholesale replacement: a new store baseline, adopted back --
    ;; which is exactly re-convergence, so a success unforks
    (if (buffer-state-id b)
        (guard (ex [else (adopt-local! b new-lines)])
          (state:reset! ui-actor (buffer-state-id b) new-lines)
          (adopt-state! b)
          (set! forked-buffers (remq b forked-buffers)))
        (adopt-local! b new-lines)))

  (define (state-edit! b span replacement)
    ;; The ui's text edits go through the store first and the cache
    ;; adopts the result.  A stale refusal means a foreign edit
    ;; overlapped mid-command: this seat's content wins -- the edit
    ;; applies to the cache's coordinates and resets the store (the
    ;; conflict is in the audit log; see the tech debt ledger).
    (define (local-text)
      (let-values ([(new-text delta)
                    (text:apply-edit (buffer-lines b) span replacement)])
        new-text))
    (if (and (buffer-state-id b) (not (memq b forked-buffers)))
        (guard (ex [else (adopt-local! b (local-text))])
          (let-values ([(status info)
                        (state:edit! ui-actor (buffer-state-id b)
                                     (buffer-state-rev b)
                                     span replacement)])
            (if (eq? status 'applied)
                (begin (adopt-state! b) (note-ui-edit! b))
                (let ([foreign
                       (guard (ex [else #f])
                         (find (lambda (entry)
                                 (not (equal? (cadr entry) ui-actor)))
                               (state:history (buffer-state-id b) 8)))])
                  ;; the conflict is on the record before the seat wins
                  (guard (ex [else (void)])
                    (log:add! 'state
                      (format "conflict: ui overrode ~a in ~s"
                              (if foreign (cadr foreign) "another actor")
                              (buffer-name b))))
                  (state-reset! b (local-text))
                  ;; ... and the losing actor is told, after the reset
                  ;; settles, so a re-read sees the truth:
                  ;; (conflict buffer-id buffer-name winning-actor)
                  (when foreign
                    (guard (ex [else (void)])
                      (actor:send! (cadr foreign)
                                   (list 'conflict (buffer-state-id b)
                                         (buffer-name b)
                                         ui-actor))))))))
        (adopt-local! b (local-text))))

  (define (mirror-rename! b)
    (when (buffer-state-id b)
      (guard (ex [else (void)])
        (state:rename! ui-actor (buffer-state-id b) (buffer-name b)))))

  (define (new-buffer name)
    (let ([b (make-buffer name (vector "") 0 (vector '() '())
                          0 0 #f 0 0 0 'default #f 0)])
      (mirror-create! b)
      ;; the managed facts start explicit, so absence stays honest
      (buffer-trailing-set! b #t)
      (buffer-mode-auto-set! b #t)
      (buffer-fact-set! b 'wrap 'default)
      b))

  (define (bump-buffer-revision! b)
    (buffer-revision-set! b (+ (buffer-revision b) 1)))

  (define (buffer-of-state-id id)
    (find (lambda (b) (eqv? (buffer-state-id b) id)) the-buffers))

  (define (adopt-store-buffer! id)
    ;; Another actor created a store buffer: give this head a record
    ;; for it, so it shows in the buffer list like any other -- unless
    ;; its creator marked it ephemeral (a head's own chrome, the
    ;; *completions* view).  It
    ;; joins at the end: this seat did not ask for it.  A buffer with
    ;; no mode yet gets detection, recorded as the shared fact.
    (unless (or (buffer-of-state-id id)
                (state:property id 'ephemeral))
      (let-values ([(text revision) (state:snapshot id)])
        (let ([b (make-buffer (state:buffer-name id) text 0
                              (vector '() '()) 0 0 #f 0 0 0
                              'default id revision)])
          (unless (buffer-fact b 'mode #f) (adopt-hook b))
          (unless (buffer-fact b 'wrap #f) (buffer-fact-set! b 'wrap 'default))
          (set! the-buffers (append the-buffers (list b)))))))

  (define (buffer-lines-set! b new-lines)
    (state-reset! b new-lines))

  (define (clamp-buffer-positions! b)
    ;; keep the buffer's spot and every window's point inside the
    ;; (possibly shorter) current lines
    (let* ([v (buffer-lines b)]
           [last (- (vector-length v) 1)])
      (buffer-spot-row-set! b (min (buffer-spot-row b) last))
      (buffer-spot-col-set!
        b (min (buffer-spot-col b)
               (string-length (vector-ref v (buffer-spot-row b)))))
      (for-each
        (lambda (w)
          (when (eq? (window-buffer w) b)
            (window-prow-set! w (min (window-prow w) last))
            (window-pcol-set!
              w (min (window-pcol w)
                     (string-length (vector-ref v (window-prow w)))))
            (window-top-set! w (min (window-top w) last))))
        the-windows)))

  ;; Foreign actors edit the (state) store directly; their changes
  ;; flow back into this seat's line caches before each frame.  The
  ;; subscription callback runs on whichever thread edited, so it only
  ;; records the buffer id; the main loop does the adoption.
  (define foreign-lock (make-mutex))
  (define foreign-pending '())

  (define (event-actor event)
    ;; every store event names its actor last: (delete id actor) is
    ;; the one three-element shape
    (if (eq? (car event) 'delete) (caddr event) (list-ref event 3)))

  (define (note-foreign-event event)
    (when (and (memq (car event) '(edit reset property create rename delete))
               (not (equal? (event-actor event) ui-actor)))
      (with-mutex foreign-lock
        (set! foreign-pending (cons event foreign-pending)))
      (wake-main!)))

  (define state-subscription (state:subscribe! #f note-foreign-event))

  ;; The ui's own side of the audit stream, coalesced: keystrokes are
  ;; too many to log one by one, so consecutive ui edits to a buffer
  ;; batch into one entry -- flushed before a foreign actor's
  ;; operation on the same buffer (so the record reads in true
  ;; order), when a burst goes stale, and at shutdown.
  (define ui-audit-bursts '())  ; (id . #(name first-rev last-rev n time))

  (define (note-ui-edit! b)
    (guard (ex [else (void)])
      (let* ([id (buffer-state-id b)]
             [rev (buffer-state-rev b)]
             [hit (assv id ui-audit-bursts)]
             [now (time-second (current-time 'time-monotonic))])
        (if hit
            (let ([v (cdr hit)])
              (vector-set! v 2 rev)
              (vector-set! v 3 (+ (vector-ref v 3) 1))
              (vector-set! v 4 now))
            (set! ui-audit-bursts
              (cons (cons id (vector (buffer-name b) rev rev 1 now))
                    ui-audit-bursts))))))

  (define (flush-ui-audit! which)
    ;; which: a buffer id, 'stale (idle bursts), or 'all
    (let ([now (time-second (current-time 'time-monotonic))])
      (let-values ([(flushed kept)
                    (partition
                      (lambda (entry)
                        (case which
                          [(all) #t]
                          [(stale)
                           (> (- now (vector-ref (cdr entry) 4)) 3)]
                          [else (eqv? (car entry) which)]))
                      ui-audit-bursts)])
        (set! ui-audit-bursts kept)
        (for-each
          (lambda (entry)
            (guard (ex [else (void)])
              (let ([v (cdr entry)])
                (log:add! 'state
                  (format "ui: ~a edit~a in ~s (revisions ~a-~a)"
                          (vector-ref v 3)
                          (if (= (vector-ref v 3) 1) "" "s")
                          (vector-ref v 0)
                          (vector-ref v 1) (vector-ref v 2))
                  #f))))
          (reverse flushed)))))

  (define (sync-foreign-edits!)
    (let ([events (with-mutex foreign-lock
                    (let ([pending foreign-pending])
                      (set! foreign-pending '())
                      (reverse pending)))])
      ;; the audit stream: every foreign operation is on the record --
      ;; (log-view 'state) shows what other actors did
      (for-each
        (lambda (event)
          (guard (ex [else (void)])
            (let ([id (cadr event)] [actor (event-actor event)])
              ;; the modified flag flips on every edit: audit the
              ;; edits, not their bookkeeping shadow
              (unless (and (eq? (car event) 'property)
                           (eq? (caddr event) 'modified))
                (flush-ui-audit! id)
                (log:add! 'state
                  (case (car event)
                    [(create)
                     (format "~a created ~s" actor (caddr event))]
                    [(rename)
                     (format "~a renamed ~s to ~s" actor
                             (let ([b (buffer-of-state-id id)])
                               (if b (buffer-name b) id))
                             (caddr event))]
                    [(delete)
                     (format "~a deleted ~s" actor
                             (let ([b (buffer-of-state-id id)])
                               (if b (buffer-name b) id)))]
                    [(property)
                     (format "~a set ~a of ~s"
                             actor (caddr event)
                             (state:buffer-name id))]
                    [else
                     (format "~a ~a ~s~a"
                             actor
                             (if (eq? (car event) 'reset)
                                 "reset" "edited")
                             (state:buffer-name id)
                             (if (eq? (car event) 'edit)
                                 (format " at ~a"
                                         (text:span-start
                                           (text:delta-span
                                             (list-ref event 4))))
                                 ""))]))))))
        events)
      ;; the buffer lifecycle across heads: another actor's buffers
      ;; appear in this head's list, renames follow, and a deletion
      ;; drops the record -- any window showing it moves on
      (for-each
        (lambda (event)
          (guard (ex [else (void)])
            (case (car event)
              [(create) (adopt-store-buffer! (cadr event))]
              [(rename)
               (let ([b (buffer-of-state-id (cadr event))])
                 (when b (buffer-name-set! b (caddr event))))]
              [(delete)
               (let ([b (buffer-of-state-id (cadr event))])
                 (when b
                   (buffer-state-id-set! b #f)   ; the twin is gone
                   (forget-buffer! b)))]
              [else (void)])))
        events)
      ;; a foreign fact changed (mode, file, read-only): the status
      ;; line must repaint even though no text moved
      (for-each
        (lambda (event)
          (when (eq? (car event) 'property)
            (let ([b (find (lambda (b)
                             (eqv? (buffer-state-id b) (cadr event)))
                           the-buffers)])
              (when b
                (bump-buffer-revision! b)
                (repaint-hook)))))
        events)
      ;; carry every view's point across the foreign deltas, so a
      ;; cursor keeps its content when an agent edits above it
      (for-each
        (lambda (event)
          (when (eq? (car event) 'edit)
            (let ([b (find (lambda (b)
                             (eqv? (buffer-state-id b) (cadr event)))
                           the-buffers)])
              (when b
                (guard (ex [else (void)])
                  (let ([d (list-ref event 4)])
                    (for-each
                      (lambda (w)
                        (when (eq? (window-buffer w) b)
                          (let ([p (text:rebase-position
                                     (cons (window-prow w)
                                           (window-pcol w))
                                     d)])
                            (window-prow-set! w (car p))
                            (window-pcol-set! w (cdr p)))
                          (window-top-set!
                            w (car (text:rebase-position
                                     (cons (window-top w) 0) d)))))
                      the-windows)
                    (let ([p (text:rebase-position
                               (cons (buffer-spot-row b)
                                     (buffer-spot-col b))
                               d)])
                      (buffer-spot-row-set! b (car p))
                      (buffer-spot-col-set! b (cdr p)))))))))
        events)
      (for-each
        (lambda (id)
          (let ([b (find (lambda (b) (eqv? (buffer-state-id b) id))
                         the-buffers)])
            (when b
              (guard (ex [else (void)])
                (let-values ([(text revision) (state:snapshot id)])
                  (unless (= revision (buffer-state-rev b))
                    ;; adoption is sharing: nothing mutates in place
                    (buffer-lines-raw-set! b text)
                    (bump-buffer-revision! b)
                    (buffer-state-rev-set! b revision)
                    (when (buffer-file b) (buffer-modified-set! b #t))
                    (clamp-buffer-positions! b)
                    (repaint-hook)))))))
        (let dedupe ([ids (map cadr events)] [seen '()])
          (cond [(null? ids) (reverse seen)]
                [(memv (car ids) seen) (dedupe (cdr ids) seen)]
                [else (dedupe (cdr ids) (cons (car ids) seen))])))))

  ;; What the head looks at, published as state marks other actors can
  ;; read, refreshed per frame by a desired-versus-published diff:
  ;; every window's cursor as (point . serial), the selected window's
  ;; additionally as plain 'point, and the active region as 'region
  ;; and (region . serial).  A mark drops when its window closes,
  ;; looks at another buffer, or the selection deactivates.  Serials
  ;; ride a weak table, so closed windows carry theirs to the grave.

  (define window-serial-counter 0)
  (define window-serials (make-weak-eq-hashtable))

  (define (window-serial w)
    (or (hashtable-ref window-serials w #f)
        (begin
          (set! window-serial-counter (+ window-serial-counter 1))
          (hashtable-set! window-serials w window-serial-counter)
          window-serial-counter)))

  ;; (((id . name) . value) ...): value is (row . col) for a point,
  ;; ((row . col) . (row . col)) for a region -- plain data, so frames
  ;; without changes are equal? and publish nothing
  (define published-marks '())

  (define (desired-head-marks)
    (fold-left
      (lambda (acc w)
        (let ([id (buffer-state-id (window-buffer w))])
          (if (not id)
              acc
              (let* ([serial (window-serial w)]
                     [selected? (eq? w the-current)]
                     [p (cons (window-prow w) (window-pcol w))]
                     [acc (cons (cons (cons id (cons 'point serial)) p)
                                acc)]
                     [acc (if selected?
                              (cons (cons (cons id 'point) p) acc)
                              acc)])
                (if (and selected? (buffer-marked (window-buffer the-current)))
                    (let ([region (cons (cons (buffer-mark-row (window-buffer the-current)) (buffer-mark-col (window-buffer the-current))) p)])
                      (cons* (cons (cons id (cons 'region serial)) region)
                             (cons (cons id 'region) region)
                             acc))
                    acc)))))
      '() the-windows))

  (define (mark-value value)
    ;; a region value becomes a normalized span; a point stays a pair
    (if (pair? (car value))
        (text:normalize-span
          (text:make-span (caar value) (cdar value)
                          (cadr value) (cddr value)))
        value))

  (define (publish-head-marks!)
    (guard (ex [else (void)])
      (let ([desired (desired-head-marks)])
        (unless (equal? desired published-marks)
          (for-each
            (lambda (entry)
              (unless (assoc (car entry) desired)
                (guard (ex [else (void)])
                  (state:drop-mark! ui-actor (caar entry) (cdar entry)))))
            published-marks)
          (for-each
            (lambda (entry)
              (let ([old (assoc (car entry) published-marks)])
                (unless (and old (equal? (cdr old) (cdr entry)))
                  (guard (ex [else (void)])
                    (state:set-mark! ui-actor (caar entry) (cdar entry)
                                     (mark-value (cdr entry)))))))
            desired)
          (set! published-marks desired)))))


  ;;; Apps and views ------------------------------------------------------------

  ;; An app is a dynamic read-only buffer with a renderer and, optionally, an
  ;; event handler with first refusal on keys (what it declines goes through
  ;; the keymaps). A view is the degenerate app with no handler. Apps act on
  ;; the selected window -- their own, when it is selected.
  (define-record-type app
    (fields buffer refresh! handle-event!
            (mutable refresh-error)
            (mutable cursor-visible?) (mutable status-position)))

  (define app-registry (kernel:make-registry))
  (define buffer-kill-hook-registry (kernel:make-registry))
  (define shutdown-hook-registry (kernel:make-registry))
  (define pre-redraw-hook-registry (kernel:make-registry))

  (define (add-buffer-kill-hook! proc)
    (unless (procedure? proc)
      (error 'add-buffer-kill-hook! "expected a procedure" proc))
    (kernel:registry-add! buffer-kill-hook-registry proc))

  (define (add-pre-redraw-hook! proc)
    (unless (procedure? proc)
      (error 'add-pre-redraw-hook! "expected a procedure" proc))
    (kernel:registry-add! pre-redraw-hook-registry proc))

  (define (before-frame!)
    ;; What every frame is preceded by: the store's news -- lifecycle
    ;; first, so a foreign deletion forgets the buffer before outage
    ;; recovery could mistake its missing twin for a store fault --
    ;; then the layers above, through the pre-redraw hooks.
    (sync-foreign-edits!)
    (reconverge-forked!)
    (flush-ui-audit! 'stale)
    (publish-head-marks!)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
              (kernel:registry-items pre-redraw-hook-registry)))

  (define (add-shutdown-hook! proc)
    (unless (procedure? proc)
      (error 'add-shutdown-hook! "expected a procedure" proc))
    (kernel:registry-add! shutdown-hook-registry proc))

  (define (run-shutdown-hooks!)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
              (kernel:registry-items shutdown-hook-registry)))

  (define (registered-apps) (kernel:registry-items app-registry))

  (define (app-of b)
    (find (lambda (a) (eq? (app-buffer a) b)) (registered-apps)))

  (define (app-buffer? b) (and (app-of b) #t))

  (define (dispatch-app-event! event)
    ;; the current buffer's app handler: #t when it consumed the event
    (let* ([a (app-of (window-buffer the-current))]
           [handler (and a (app-handle-event! a))])
      (and handler (handler event) #t)))

  (define (detach-app! b)
    ;; Preserve the app's current buffer contents while removing its
    ;; dynamic refresh and event handler; its presentation facts stay
    ;; with the buffer.  It behaves like an ordinary read-only buffer.
    (let ([a (app-of b)])
      (when a
        (kernel:registry-remove! app-registry
                                 (lambda (x) (eq? (app-buffer x) b))))
      (buffer-read-only-set! b #t)
      b))

  (define (register-app! name refresh! . handler)
    (let* ([named (buffer-named name)]
           [_ (when (and named (not (buffer-fact named 'app #f)))
                (error 'register-app! "buffer name is already in use" name))]
           [b (or named (new-buffer name))]
           [a (make-app b refresh! (and (pair? handler) (car handler))
                        #f 'default #f)])
      (unless (procedure? refresh!)
        (error 'register-app! "refresh must be a procedure" refresh!))
      (when (and (pair? handler) (not (procedure? (car handler))))
        (error 'register-app! "event handler must be a procedure"
               (car handler)))
      (buffer-read-only-set! b #t)
      ;; the buffer is an app's for good: a re-registration (a module
      ;; reloading) may take the name back, and the presentation facts
      ;; set on it persist as store properties
      (buffer-fact-set! b 'app #t)
      (unless (memq b the-buffers) (set! the-buffers (append the-buffers (list b))))
      ;; Re-registration in one init replaces rather than duplicates refreshes.
      (kernel:registry-remove! app-registry
                               (lambda (x) (eq? (app-buffer x) b)))
      (kernel:registry-add! app-registry a)
      b))

  (define (set-app-cursor-visible! b visible?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-cursor-visible! "not an app buffer" b))
      (unless (or (boolean? visible?) (procedure? visible?))
        (error 'set-app-cursor-visible!
               "visibility must be a boolean or procedure" visible?))
      (app-cursor-visible?-set! a visible?)
      b))

  (define (set-app-manages-viewport! b manages?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-manages-viewport! "not an app buffer" b))
      (unless (boolean? manages?)
        (error 'set-app-manages-viewport! "manages must be #t or #f" manages?))
      (buffer-fact-set! b 'manages-viewport manages?)
      b))

  (define (set-app-status-position! b position)
    (let ([a (app-of b)])
      (unless a (error 'set-app-status-position! "not an app buffer" b))
      (unless (or (not position) (procedure? position))
        (error 'set-app-status-position!
               "position must be #f or a procedure" position))
      (app-status-position-set! a position)
      b))

  (define (app-cursor-visible-in? w)
    (let* ([a (app-of (window-buffer w))]
           [visibility (and a (app-cursor-visible? a))])
      (cond [(not a) #t]
            [(eq? visibility 'default) #t]
            [(procedure? visibility)
             (guard (ex [else #t]) (visibility w))]
            [(boolean? visibility) visibility]
            [else #t])))

  (define (app-manages-window-viewport? w)
    (buffer-fact (window-buffer w) 'manages-viewport #f))

  (define (set-app-presentation! b sticky-lines scrollbar . options)
    ;; Configure buffer-level presentation shared by every window -- and
    ;; every head -- showing the app: store properties.  Sticky rows stay
    ;; above the scrollable body; scrollbar is #f, #t (enabled using the
    ;; configured side), left, or right.
    (let ([a (app-of b)])
      (unless a (error 'set-app-presentation! "not an app buffer" b))
      (unless (and (integer? sticky-lines) (exact? sticky-lines)
                   (>= sticky-lines 0))
        (error 'set-app-presentation! "sticky line count must be nonnegative"
               sticky-lines))
      (unless (memq scrollbar '(#f #t left right))
        (error 'set-app-presentation!
               "scrollbar must be #f, #t, left, or right" scrollbar))
      (let ([wrap (if (pair? options) (car options) 'default)]
            [cursor-style (if (and (pair? options) (pair? (cdr options)))
                              (cadr options) 'default)])
        (unless (memq wrap '(default #t #f))
          (error 'set-app-presentation!
                 "wrap must be default, #t, or #f" wrap))
        (unless (memq cursor-style
                      '(default block underline bar
                                blinking-block blinking-underline blinking-bar))
          (error 'set-app-presentation!
                 "invalid cursor style"
                 cursor-style))
        (buffer-fact-set! b 'wrap wrap)
        (buffer-fact-set! b 'cursor-style cursor-style))
      (buffer-fact-set! b 'sticky-lines sticky-lines)
      (buffer-fact-set! b 'scrollbar scrollbar)
      (repaint-hook)
      b))

  (define (buffer-sticky-lines b)
    (min (or (buffer-fact b 'sticky-lines #f) 0) (line-count b)))

  ;; Ordinary buffers use the global setting. An app can force the bar on
  ;; with #t, force a particular side, or otherwise inherit the global choice.
  (define scrollbar
    (make-parameter #f
      (lambda (visible?)
        (unless (boolean? visible?)
          (error 'scrollbar "must be #t or #f" visible?))
        visible?)))
  (define scrollbar-position
    (make-parameter 'right
      (lambda (side)
        (unless (memq side '(left right))
          (error 'scrollbar-position "must be left or right" side))
        side)))

  (define line-numbers
    (make-parameter #f
      (lambda (visible?)
        (unless (boolean? visible?)
          (error 'line-numbers "must be #t or #f" visible?))
        visible?)))

  (define (buffer-line-numbers b)
    (let ([setting (buffer-line-numbers-setting b)])
      (if (eq? setting 'default) (line-numbers) setting)))

  (define (window-line-number-width w)
    (if (buffer-line-numbers (window-buffer w))
        (+ 1 (string-length
               (number->string (line-count (window-buffer w)))))
        0))

  (define (window-scrollbar? w)
    (let ([choice (buffer-fact (window-buffer w) 'scrollbar #f)])
      (cond [(memq choice '(left right)) choice]
            [(or choice (scrollbar)) (scrollbar-position)]
            [else #f])))

  (define (window-content-width w)
    (max 1 (- (window-width w)
              (if (window-scrollbar? w) 1 0)
              (window-line-number-width w))))

  (define (buffer-narrowest-width b)
    ;; The smallest content width among the windows showing b, or #f
    ;; -- what a rendering shared by every window must fit.
    (let ([ws (filter (lambda (w) (eq? (window-buffer w) b)) the-windows)])
      (and (pair? ws)
           (fold-left (lambda (m w) (min m (window-content-width w)))
                      (window-content-width (car ws))
                      (cdr ws)))))

  (define (buffer-window-size b)
    ;; The text grid of the preferred window displaying b.  App-owned terminal
    ;; state uses one grid per buffer, so the focused window wins when several
    ;; windows mirror it.
    (let ([w (if (eq? (window-buffer the-current) b)
                 the-current
                 (find (lambda (candidate)
                         (eq? (window-buffer candidate) b))
                       the-windows))])
      (and w (cons (window-size w) (window-content-width w)))))

  (define (window-scrollbar-column w)
    (case (window-scrollbar? w)
      [(left) (window-xoff w)]
      [(right) (+ (window-xoff w) (window-width w) -1)]
      [else #f]))

  (define (register-view! name refresh!)
    (register-app! name refresh!))

  (define (view-buffer? b)
    (app-buffer? b))

  (define (refresh-visible-views!)
    (for-each (lambda (a)
                (when (find (lambda (w) (eq? (window-buffer w) (app-buffer a)))
                            the-windows)
                  (guard (ex [else
                              (let ([text
                                     (format "App ~a refresh failed: ~a"
                                             (buffer-name (app-buffer a))
                                             (kernel:condition-text ex))])
                                (unless (equal? text (app-refresh-error a))
                                  (app-refresh-error-set! a text)
                                  (log:add! 'app text)))])
                    ((app-refresh! a))
                    (app-refresh-error-set! a #f))))
              (filter (lambda (a) (memq (app-buffer a) the-buffers))
                      (registered-apps))))

  (define (view-append! b lines)
    ;; Append lines to view b: windows whose point was at the very end
    ;; follow the tail; others hold their viewport still.
    (when (pair? lines)
      (let* ([v (buffer-lines b)]
             [n (vector-length v)]
             [virgin? (and (= n 1) (string=? (vector-ref v 0) ""))]
             [tail? (lambda (w)
                      (and (eq? (window-buffer w) b)
                           (= (window-prow w) (- n 1))
                           (= (window-pcol w)
                              (string-length (vector-ref v (- n 1))))))]
             [tails (filter tail? the-windows)])
        (buffer-lines-set! b
          (if virgin?
              (list->vector lines)
              (text:splice v n n lines)))
        (let* ([nv (buffer-lines b)]
               [last (- (vector-length nv) 1)])
          (for-each (lambda (w)
                      (window-prow-set! w last)
                      (window-pcol-set! w
                        (string-length (vector-ref nv last))))
                    tails)))))

  (define (view-replace! b lines)
    ;; Replace a view's rendering without disturbing windows when it has not
    ;; changed. On a real change, keep point and the viewport where possible,
    ;; clamping them only when the new rendering is shorter.
    (let ([new (if (null? lines) (vector "") (list->vector lines))])
      (unless (equal? (buffer-lines b) new)
        (buffer-lines-set! b new)
        ;; A view may be refreshed by a worker thread while the main input
        ;; loop is between frames.  Its old row keys can otherwise survive a
        ;; racing redraw even though the buffer revision changed.  Dynamic
        ;; view replacement is comparatively rare (terminal emulation is the
        ;; demanding case), so prefer a guaranteed coherent frame.
        (repaint-hook)
        (clamp-buffer-positions! b))))


  (define (buffer-named name)
    (find (lambda (b) (string=? (buffer-name b) name)) the-buffers))

  (define (set-window-buffer! w b)
    ;; Display b in w, remembering where point was in the old buffer and
    ;; restoring where it last was in the new one.
    (let ([old (window-buffer w)])
      (unless (eq? old b)
        (buffer-spot-row-set! old (window-prow w))
        (buffer-spot-col-set! old (window-pcol w))
        (buffer-spot-top-set! old (window-top w))
        ;; Buffer identity is part of every content and status row, even when
        ;; the new buffer happens to have equal text and presentation chrome.
        ;; This is especially important when an asynchronous app paints its
        ;; final frame while the main thread replaces it.
        (repaint-hook)))
    (window-buffer-set! w b)
    (window-prow-set! w (buffer-spot-row b))
    (window-pcol-set! w (buffer-spot-col b))
    (window-top-set! w (buffer-spot-top b))
    (window-topseg-set! w 0)
    (window-left-set! w 0))

  (define (forget-buffer! b)
    ;; drop this head's record of a buffer whose twin is gone -- kill
    ;; hooks, the buffer list, apps, and every window showing it
    (for-each
      (lambda (hook)
        (guard (ex [else
                    (log:add! 'kill-buffer!
                      (format "Buffer cleanup failed for ~a: ~a"
                              (buffer-name b) (kernel:condition-text ex)))])
          (hook b)))
      (kernel:registry-items buffer-kill-hook-registry))
    (set! the-buffers (remq b the-buffers))
    (kernel:registry-remove! app-registry (lambda (x) (eq? (app-buffer x) b)))
    (when (null? the-buffers) (set! the-buffers (list (new-buffer "*scratch*"))))
    (for-each (lambda (w)
                (when (eq? (window-buffer w) b)
                  (set-window-buffer! w (car the-buffers))))
              the-windows))


  ;;; Interruptible execution -----------------------------------------------------------

  ;; A runaway computation run on the user's behalf (an M-x expression, a
  ;; shell command, ...) would freeze the editor, so for its duration the
  ;; terminal turns C-g into SIGINT (outside it the editor runs with
  ;; signals off), and SIGINT becomes a raised condition, answering #t to
  ;; interrupted?, that unwinds the computation -- C-g aborts an
  ;; evaluation just as it cancels a prompt.  Limitation: only running
  ;; Scheme can be interrupted this way -- a blocking foreign call runs
  ;; to completion.
  (define-condition-type &interrupted &serious make-interrupted interrupted?)

  ;; Interaction owns C-g; interruption applies to computation.  While
  ;; the editor waits for the user -- a prompt, a key query, a search --
  ;; isig is off and C-g arrives as an ordinary key the interaction
  ;; handles, so a command cancels the same way however it was invoked;
  ;; between interactions an evaluation is interruptible.
  (define isig-on? #f)

  (define (set-isig! on)
    (unless (eq? on isig-on?)
      (set! isig-on? on)
      (sys:terminal-isig! on)))

  (define (call-uninterrupted thunk)
    ;; run thunk as an interaction: C-g is a key while it lasts
    (let ([old isig-on?])
      (dynamic-wind
        (lambda () (set-isig! #f))
        thunk
        (lambda () (set-isig! old)))))

  (define (call-with-interrupt thunk)
    ;; Run thunk interruptibly by C-g.
    (let ([saved (keyboard-interrupt-handler)]
          [old isig-on?])
      (dynamic-wind
        (lambda ()
          (keyboard-interrupt-handler
            (lambda () (raise (make-interrupted))))
          (set-isig! #t))
        thunk
        (lambda ()
          (set-isig! old)
          (keyboard-interrupt-handler saved)))))

  ;;; The seat as an actor -----------------------------------------------------------

  ;; another actor's message to this head wakes its loop; the question
  ;; is presented before the next frame
  (define ui-actor-registered
    (actor:register! ui-actor (lambda (message) (wake-main!))))

  ;;; The seat's first state ---------------------------------------------------------

  ;; the seat begins as *scratch* in one window; views the modules above
  ;; register while loading join the list behind it
  (define seat-initialized
    (let ([b (new-buffer "*scratch*")])
      (set! the-buffers (list b))
      (let ([w (make-window b 0 0 0 0 0 0 0 0 0 1 'default)])
        (set! the-windows (list w))
        (set! the-root w)
        (set! the-current w))))
)
