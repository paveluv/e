;; head.sls -- the head: the library (head).  Pure infrastructure with
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
;; dispatch lives in (dispatch), the loop's body in (main), painting in
;; (paint), the commands in (edit); the command layer still reaches the seat's
;; state through identifier-syntax facades. Hooks connect the pump and
;; loop to painting, mouse handling, and the reloadable commands.

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
          buffer-store-id
          buffer-store-rev buffer-store-rev-set!
          buffer-rendition read-rendition refresh-renditions!
          buffers set-buffers!
          kill-ring set-kill-ring! read-paste set-pending-paste!
          call-uninterrupted call-with-interrupt interrupted? make-interrupted
          window make-window window? window-index window-numbered
          window-buffer window-buffer-set!
          window-top window-top-set!
          window-topseg window-topseg-set!
          window-left window-left-set!
          window-prow window-prow-set!
          window-pcol window-pcol-set!
          window-size window-size-set!
          window-xoff window-xoff-set!
          window-width window-width-set!
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
          read-key-event run-on-main! wake-main! request-frame-at! in-main-pump
          run-deferred! start-input-reader! set-frame-hook! set-mouse-handler!
          set-file-opener! set-quit-command! set-after-key!
          open-file! quit-command! after-key!
          quit! quitting? last-command set-last-command!
          current-keys set-current-keys! escaped-buffer set-escaped-buffer!
          dispatch-app-event! app-event-position app-event-buffer-position app-event-button
          app-event-focus
          app-facts app-status follow-app! app-following? request-app-size!
          host-color-scheme add-color-scheme-hook!
          tile! layout window-at window-button-at divider-at
          transfer-split! drag set-drag! double-click?
          ui-actor buffer-fact buffer-fact-set! buffer-facts-set! buffer-state
          buffer-file buffer-file-set! buffer-trailing buffer-trailing-set!
          buffer-modified buffer-modified-set!
          buffer-mode-auto buffer-mode-auto-set!
          buffer-read-only buffer-read-only-set!
          buffer-stamp buffer-stamp-set! buffer-base buffer-base-set!
          buffer-stale buffer-stale-set!
          adopt-store!
          edit-basis snapshot-since store-reset! store-edit! store-history! new-buffer new-local-buffer visit-file!
          add-buffer! tool-buffer find-tool-buffer
          bump-buffer-revision! buffer-of-store-id adopt-store-buffer!
          buffer-lines-set! clamp-buffer-positions!
          sync-foreign-edits! flush-ui-audit!
          set-repaint-hook! set-adopt-hook! call-with-display-update buffer-point
          add-buffer-kill-hook! add-pre-redraw-hook!
          before-frame! add-shutdown-hook! run-shutdown-hooks!
          checkpoint! resume! register-resume! resume-source buffer-placements
          registered-apps app-of app-buffer? detach-app! register-app!
          set-app-cursor-visible! set-app-manages-viewport!
          set-app-status-position! app-cursor-visible-in?
          app-manages-window-viewport? app-cursor-style set-app-presentation!
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
                make-parameter make-thread-parameter parameterize make-mutex with-mutex fork-thread void
                format remq cons* iota time-second current-time time? time-type time<? time<=? copy-time
                make-time add-duration
                make-weak-eq-hashtable box unbox set-box!
                call-with-string-output-port)
          (prefix (only (sys) terminal-isig! duplicate-standard-input-port) sys:)
          (prefix (kernel) kernel:)
          (prefix (startup) startup:)
          (prefix (tty) tty:)
          (prefix (store) store:)
          (prefix (property) property:)
          (prefix (surface) surface:)
          (prefix (render) render:)
          (prefix (text) text:)
          (prefix (datum) datum:)
          (prefix (actor) actor:)
          (prefix (log) log:))

  ;;; The records ----------------------------------------------------------------

  (define delta-log-limit 256)

  ;; A store buffer caches immutable text and reads its facts from the
  ;; store.  A local buffer has no store id: its text and local-facts
  ;; live here alone.  Selection, saved position, and line-number
  ;; toggles belong to this seat in either case.
  (define-record-type buffer
    (fields (mutable name buffer-name buffer-name-raw-set!)
                                   ; shared label cache or local <name>
            (mutable lines buffer-lines buffer-lines-raw-set!)
            (mutable revision)      ; the seat's repaint counter
            (mutable history)       ; local undo; shared group labels/presentation
            (mutable mark-row) (mutable mark-col)
            (mutable marked)
            ;; where point was when the buffer was last displayed
            (mutable spot-row) (mutable spot-col) (mutable spot-top)
            ;; #t/#f after a local toggle, or default to follow the global
            ;; line-numbers parameter
            (mutable line-numbers buffer-line-numbers-setting
                     buffer-line-numbers-setting-set!)
            ;; the buffer's twin in the (store), and the store
            ;; revision this buffer's lines last agreed with
            store-id (mutable store-rev)
            ;; Local content has its own revision, independent of repaint.
            ;; Either owner retains adopted deltas in a bounded ring, so
            ;; derived views can follow exactly the text this head sees.
            (mutable local-rev) (mutable changes)
            local-facts
            (mutable rendition buffer-rendition-raw buffer-rendition-set!))
    ;; Keep the public constructor's shape: each record gets private
    ;; facts and content history, including extension/adoption records.
    (protocol
      (lambda (new)
        (lambda (name lines revision history mark-row mark-col marked
                  spot-row spot-col spot-top line-numbers store-id store-rev)
          (new name lines revision history mark-row mark-col marked
               spot-row spot-col spot-top line-numbers store-id store-rev
               0 #f (make-eq-hashtable) #f)))))

  (define-record-type (window %make-window window?)
    (fields
      ;; the window's number, shown at the left of its status line: 0
      ;; for the first, and every new window the smallest number no
      ;; live window holds -- a closed window's number is reused, so
      ;; the numbers on screen stay small.  (window n) finds it.
      index
      (mutable buffer) (mutable top)
      ;; a soft-wrapping window may start mid-line: the first
      ;; visible segment of the top line (0 elsewhere)
      (mutable topseg)
      (mutable left)
      (mutable prow) (mutable pcol)
      ;; Text height is layout output; proportions belong to the split tree.
      (mutable size)
      ;; horizontal band geometry, written by the layout: the
      ;; window's first screen column and its width
      (mutable xoff)
      (mutable width)
      ;; soft-wrap long lines onto continuation rows instead of
      ;; scrolling horizontally
      (mutable wrap)
      ;; Following a shared app is a window preference, never a store fact.
      (mutable following?)))

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

  (define (free-window-index)
    ;; the smallest number no live window holds
    (let ([taken (map window-index the-windows)])
      (let loop ([n 0])
        (if (memv n taken) (loop (+ n 1)) n))))

  (define (make-window buffer top topseg left prow pcol size xoff width wrap)
    ;; a window is born numbered; the layout it joins decides the rest
    (%make-window (free-window-index) buffer top topseg left prow pcol
                  size xoff width wrap #t))

  (define (window-numbered n)
    ;; the live window numbered n, or #f
    (find (lambda (w) (eqv? (window-index w) n)) the-windows))

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

  ;; Main-thread presentation state: each frame derives its next deadline
  ;; anew. Providers request only still-live work while preparing or painting
  ;; a frame, so expiry, eviction and replacement need no alarm cancellation.
  (define frame-deadline #f)

  (define (request-frame-at! deadline)
    (unless (and (time? deadline) (eq? (time-type deadline) 'time-monotonic))
      (error 'request-frame-at! "expected a monotonic deadline" deadline))
    (when (or (not frame-deadline) (time<? deadline frame-deadline))
      (set! frame-deadline (copy-time deadline))))

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

  ;; The pump's hooks: the frame hook prepares and paints a frame (the
  ;; painter's, above); the mouse handler applies a report -- (handler handle? c b
  ;; x y) -> an event string, #f, or the symbol ignore for a report the
  ;; loop need not hear at all, such as pointer motion (the commands', above).
  (define frame-hook void)
  (define mouse-handler (lambda (handle? c b x y) #f))

  (define (set-frame-hook! proc) (set! frame-hook proc))
  (define (set-mouse-handler! proc) (set! mouse-handler proc))

  ;; What the loop asks of the commands, installed by them: how to open
  ;; the file argument, how to quit (the modified-buffers check), and
  ;; what runs after every key. The command layer reloads; the loop
  ;; does not, so these calls always use the latest installed hooks.
  (define file-opener (lambda (path) (void)))
  (define quit-command (lambda () (quit!)))
  (define after-key-hook void)

  (define (set-file-opener! proc) (set! file-opener proc))
  (define (set-quit-command! proc) (set! quit-command proc))
  (define (set-after-key! proc) (set! after-key-hook proc))

  (define (open-file! path) (file-opener path))
  (define (quit-command!) (quit-command))
  (define (after-key!) (after-key-hook))

  (define (frame!)
    ;; Wakes and deadlines use the same preparation as direct redraws.
    (frame-hook)
    ;; Nested prompts can temporarily borrow windows. Only an outer pump
    ;; frame checkpoints the screen the user will return to.
    (when (and (in-main-pump) (eq? (startup:mode) 'attach)) (checkpoint! 'idle)))

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
         (let ([message (kernel:mailbox-receive! mailbox frame-deadline #t)])
           (case (and message (car message))
             [(#f)
              (frame!)
              (pump)]
             [(key)
              (let ([event (cdr message)])
                (cond
                  [(not (pair? event)) event]
                  [(eq? (car event) 'mouse)
                   (let ([result (apply mouse-handler handle-mouse? (cdr event))])
                     (cond [(eq? result 'ignore)
                            ;; Swallowed, but it may have moved hover state:
                            ;; frame once the burst of reports has drained.
                            (request-frame-at! (current-time 'time-monotonic))
                            (pump)]
                           [else (or result "MOUSE-HANDLED")]))]
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

  ;; The bridge between this seat's buffer records and the (store):
  ;; the seat is the store's client -- over the wire, a remote
  ;; seat is exactly this code with a socket under the store: calls.
  ;; Records cache the store's immutable text and adopt it after every
  ;; operation; this seat's edits enter the store transactionally;
  ;; wholesale replacements are resets; foreign actors' operations
  ;; flow back before each frame (sync-foreign-edits!); buffer facts
  ;; are store properties.  Shared mutations must commit in the store;
  ;; a refusal or failure cannot turn a shared cache into local truth.
  ;;
  ;; Two hooks reach upward, each installed by the module that owns
  ;; the answer: the painter invalidates its screen (set-repaint-hook!),
  ;; the mode registry gives an adopted buffer a mode (set-adopt-hook!).

  (define repaint-hook void)
  (define display-update (make-parameter #f))

  (define (request-repaint!)
    (let ([pending (display-update)])
      (if pending (set-box! pending #t) (repaint-hook))))

  (define (call-with-display-update thunk)
    ;; Compose seat changes before notifying the painter. Nested updates
    ;; share one notification; callbacks run outside the scope and may
    ;; start another update. This batches notification, not rollback or
    ;; arbitrary extension callbacks. Seat work still runs on the head pump.
    (if (display-update) (thunk)
        (let ([pending (box #f)])
          (dynamic-wind
            void
            (lambda () (parameterize ([display-update pending]) (thunk)))
            (lambda ()
              (when (unbox pending)
                (set-box! pending #f)
                (repaint-hook)))))))
  (define adopt-hook (lambda (b) (void)))

  (define (set-repaint-hook! proc) (set! repaint-hook proc))
  (define (set-adopt-hook! proc) (set! adopt-hook proc))

  (define (line-count b) (vector-length (buffer-lines b)))

  ;; Facts have one owner: the store for shared buffers, the record's
  ;; table for local ones.  Both distinguish an absent key (the caller's
  ;; fallback) from an explicit #f.  Store failures propagate; no fallback
  ;; can make missing shared truth look like a successful read or write.
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
    (let ([id (buffer-store-id b)])
      (if id
          (store:property id key fallback)
          (hashtable-ref (buffer-local-facts b) key fallback))))

  (define (buffer-fact-set! b key value)
    (buffer-facts-set! b (list (cons key value))))

  (define (local-facts-match? b expected)
    (or (not expected)
        (and (memq b the-buffers)
             (let-values ([(text revision facts) (buffer-state b)])
               (property:matches? expected facts)))))

  (define (buffer-facts-set! b updates . options)
    (store:validate-properties updates)
    (unless (<= (length options) 2) (error 'buffer-facts-set! "expected fact review and optional name" options))
    (let ([id (buffer-store-id b)]
          [expected (property:validate-expected (and (pair? options) (car options)))]
          [name (and (= (length options) 2) (cadr options))])
      (when (and (= (length options) 2)
                 (not (and (string? name) (> (string-length name) 0))))
        (error 'buffer-facts-set! "expected a nonempty name" name))
      (if id
          (and (apply store:set-properties! ui-actor id updates options)
               (begin
                 ;; Subscribers can rename, hide, delete or readmit this id.
                 ;; Reconcile current truth instead of installing a stale ack.
                 (when name (sync-foreign-edits! id))
                 #t))
          (let ([name (and name (unique-local-name (string-copy name) b))])
            (and (local-facts-match? b expected)
                 (begin
                   (for-each (lambda (entry) (hashtable-set! (buffer-local-facts b) (car entry) (cdr entry)))
                             updates)
                   (when name (buffer-name-raw-set! b name))
                   #t))))))

  (define (buffer-state b)
    ;; Unlike the command basis, this is current shared truth for save
    ;; and discard decisions, including text not yet adopted by the head.
    (if (buffer-store-id b)
        (store:snapshot-state (buffer-store-id b))
        (let-values ([(keys data) (hashtable-entries (buffer-local-facts b))])
          (values (buffer-lines b) (content-revision b)
                  (map cons (vector->list keys) (vector->list data))))))

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

  (define (local-name name)
    ;; Locality is visible in every label, including user renames.
    ;; Existing tool keys keep their identity: *tool* names become
    ;; <tool> labels, and an already bracketed label is idempotent.
    (let ([n (string-length name)])
      (cond
        [(and (>= n 2) (char=? (string-ref name 0) #\<)
              (char=? (string-ref name (- n 1)) #\>))
         name]
        [(and (>= n 2) (char=? (string-ref name 0) #\*)
              (char=? (string-ref name (- n 1)) #\*))
         (string-append "<" (substring name 1 (- n 1)) ">")]
        [else (string-append "<" name ">")])))

  (define (buffer-name-set! b name)
    (unless (and (buffer? b) (string? name) (> (string-length name) 0))
      (error 'buffer-name-set! "expected a buffer and nonempty name" b name))
    (when (buffer-store-id b) (ensure-buffer-visible! b))
    (buffer-facts-set! b '() #f name))

  (define (unique-local-name base self)
    ;; Local labels only compete with content visible to this head,
    ;; including pending adoption. The store allocates all shared names.
    (let* ([used (make-hashtable string-hash string=?)]
           [base (local-name base)])
      (for-each (lambda (b)
                  (when (and (not (eq? b self)) (buffer-visible? b))
                    (hashtable-set! used (buffer-name b) #t)))
                the-buffers)
      (guard (ex [else (void)])
        (for-each (lambda (id)
                    (when (store:visible? ui-actor id)
                      (hashtable-set! used (store:buffer-name id) #t)))
                  (store:buffer-list)))
      (let loop ([k 1])
        (let ([name (if (= k 1) base
                      (format "<~a ~a>" (substring base 1 (- (string-length base) 1)) k))])
          (if (hashtable-ref used name #f)
              (loop (+ k 1))
              name)))))

  (define (reserve-store-name! name)
    ;; A shared label takes precedence.  Tool identity survives this
    ;; local rename because it is independent of the displayed label.
    (for-each
      (lambda (b)
        (when (and (not (buffer-store-id b))
                   (string=? (buffer-name b) name))
          (buffer-name-set! b name)))
      the-buffers))

  (define initial-buffer-facts '((trailing . #t) (mode-auto . #t) (wrap . default)))

  (define (content-revision b)
    (if (buffer-store-id b) (buffer-store-rev b) (buffer-local-rev b)))

  (define (adopt-text! b text revision changes)
    ;; Keep only actual deltas ending at the adopted snapshot.  A reset or
    ;; incomplete chain cuts provenance; it is never inferred from a diff.
    ;; The lazy ring records each delta in O(1), without old text vectors.
    (cond
      [(not changes) (buffer-changes-set! b #f)]
      [(pair? changes)
       (let ([log (or (buffer-changes b) (make-vector delta-log-limit #f))])
         (for-each (lambda (entry)
                     (vector-set! log (mod (car entry) delta-log-limit) entry))
                   changes)
         (buffer-changes-set! b log))])
    ;; No old surface can describe newly adopted text, even if a callback
    ;; asks for rendition before the next demanded frame has been prepared.
    (buffer-rendition-set! b #f)
    (buffer-lines-raw-set! b text)
    (if (buffer-store-id b)
        (buffer-store-rev-set! b revision)
        (buffer-local-rev-set! b revision))
    (bump-buffer-revision! b))

  (define (snapshot-since b basis)
    ;; Like store:snapshot-since, but ends at this head's cached source,
    ;; including for local buffers.  Read on the head's pump: it does not
    ;; pull newer store text or run callbacks.  #f omits an earlier basis.
    (unless (or (not basis) (and (integer? basis) (exact? basis) (>= basis 0)))
      (error 'snapshot-since "expected a content revision or #f" basis))
    (let* ([text (buffer-lines b)] [revision (content-revision b)]
           [log (buffer-changes b)]
           [changes
            (and basis (<= basis revision) (<= (- revision basis) delta-log-limit)
                 (let scan ([next (+ basis 1)] [out '()])
                   (if (> next revision) (reverse out)
                       (let ([entry (and log (vector-ref log (mod next delta-log-limit)))])
                         (and entry (= (car entry) next)
                              ;; Own the public list spines, as the store does.
                              (scan (+ next 1) (cons (list (car entry) (cadr entry) (caddr entry)) out)))))))])
      (values text revision changes)))

  (define (adopt-store! b)
    ;; make the cache the store's current text -- the vectors are
    ;; immutable, so adoption is reference sharing, never a copy
    (let-values ([(text revision) (store:snapshot (buffer-store-id b))])
      (adopt-text! b text revision #f)
      (clamp-buffer-positions! b)
      (refresh-buffer-rendition! b)
      (invalidate-buffer-marks! (buffer-store-id b))))

  (define (buffer-rendition b)
    ;; Optional presentation fails closed when visibility cannot be read.
    ;; The next frame retries; a store outage must not stop the head pump.
    (guard (ex [else #f])
      (and (memq b the-buffers) (buffer-visible? b)
           (or (buffer-rendition-raw b)
               ;; Commands may ask for geometry immediately after an edit,
               ;; before the next surface demand. Adopted text is sufficient.
               (render:prepare #f #f (buffer-lines b) (content-revision b) '())))))

  (define (read-rendition b ranges . follow-height)
    (read-source-rendition b (buffer-lines b) (content-revision b) ranges
                           (if (null? follow-height) 0 (car follow-height))))

  (define (read-source-rendition b text revision ranges follow-height)
    ;; Explicit demand reads obey head visibility even through a retained
    ;; reference whose retirement notification has not reached the pump.
    (guard (ex [else #f])
      (and (memq b the-buffers) (buffer-visible? b)
           (render:prepare (buffer-rendition-raw b) (buffer-store-id b)
                           text revision ranges follow-height))))

  (define (prepare-buffer-rendition b text revision changes facts)
    ;; Fetch the current viewport and every row a viewport containing point
    ;; could expose. Geometry can then scroll against one prepared generation
    ;; without reading another frame halfway through layout. Project pending
    ;; anchors before adoption, so a live grid's text and rendition can land
    ;; together. Cache size stays bounded by window heights.
    (define deltas (if changes (map caddr changes) '()))
    (define (future-row row col)
      (car (clamp-text-position text
             (fold-left text:rebase-position (cons row col) deltas))))
    (let* ([following (if (app-live? facts)
                          (filter (lambda (w) (and (eq? (window-buffer w) b) (follows-app? w))) the-windows)
                          '())]
           [ranges
            (fold-left
              (lambda (out w)
                (if (eq? (window-buffer w) b)
                    (let ([height (max 1 (window-size w))]
                          [point (future-row (window-prow w) (window-pcol w))]
                          [top (future-row (window-top w) 0)])
                      (cons* (cons 0 (buffer-sticky-lines b))
                             (cons top (+ top height))
                             (cons (- point height -1) (+ point height)) out))
                    out)) '() the-windows)]
           [next (read-source-rendition b text revision ranges
                   (fold-left (lambda (height w) (max height 1 (window-size w))) 0 following))])
      (values next following)))

  (define (app-grid? facts)
    ;; An app owning the viewport needs its surface's geometry and cursor.
    (and (app-live? facts) (app-fact facts 'manages-viewport #f)))

  (define (rendition-ready? facts next)
    (and next (or (not (app-grid? facts)) (render:header next))))

  (define (install-buffer-rendition! b next following facts)
    (let ([old (buffer-rendition-raw b)])
      (unless (eq? old next)
        (buffer-rendition-set! b next)
        ;; Row keys already describe the complete rendition. A cursor-only
        ;; update or viewport refill must not invalidate the whole screen.
        (unless (equal? (render:header old) (render:header next))
          (bump-buffer-revision! b)))
      (let ([header (render:header next)])
        (when (and header (caddr header))
          (for-each (lambda (w) (follow-rendition! w next header facts)) following)))))

  (define (refresh-buffer-rendition! b)
    (let ([facts (app-facts b)])
      (let-values ([(next following)
                    (prepare-buffer-rendition b (buffer-lines b) (content-revision b) '() facts)])
        (when (rendition-ready? facts next)
          (install-buffer-rendition! b next following facts)))))

  (define (refresh-renditions!)
    (for-each refresh-buffer-rendition!
      (fold-left (lambda (seen w)
                   (let ([b (window-buffer w)]) (if (memq b seen) seen (cons b seen))))
                 '() the-windows)))

  (define (adopt-local! b text delta)
    ;; Only explicitly local buffers own their text in this head.
    (when (buffer-store-id b)
      (error 'adopt-local! "a shared buffer must commit in the store"))
    (let ([revision (+ (buffer-local-rev b) 1)])
      (adopt-text! b text revision (and delta (list (list revision ui-actor delta))))))

  (define (store-reset! b new-lines . options)
    ;; Explicit baseline replacement (loading/rereading), never an
    ;; automatic response to a failed edit.  Failure leaves the cache
    ;; untouched; a later frame cannot write it back over shared text.
    ;; -> accepted revision, or #f when the optional (revision fact ...)
    ;; review no longer matches. Local state is checked on its owning head.
    (unless (<= (length options) 2) (error 'store-reset! "expected facts and optional reviewed state" options))
    (let ([updates (store:validate-properties (if (pair? options) (car options) '()))]
          [review (and (= (length options) 2) (cadr options))])
      (if (buffer-store-id b)
          (let ([accepted (apply store:reset! ui-actor (buffer-store-id b) new-lines options)])
            (when accepted (adopt-store! b))
            accepted)
          (let ([text (text:normalize new-lines)])
            (let-values ([(old revision facts) (buffer-state b)])
              (and (or (not review)
                       (and (memq b the-buffers) (equal? review (cons revision facts))))
                   (begin
                     (adopt-local! b text #f)
                     (clamp-buffer-positions! b)
                     (buffer-facts-set! b updates)
                     (content-revision b))))))))

  (define (edit-basis b)
    ;; A proposal retains the text it was computed from, its owner, and
    ;; its revision even if a callback advances the head while computing.
    (list (buffer-lines b) (buffer-store-id b) (content-revision b)))

  (define (store-edit! b span replacement . options)
    ;; The store rebases this declared intent under its mutation lock.
    ;; A stale result is already an unresolvable overlap/missing basis,
    ;; not permission to recompute a replacement against newer text.
    ;; Errors also propagate: no shared edit can fall back to a local
    ;; fork, including an error after the transaction has committed.
    ;; Optional head placements are (place . desired) entries: place is
    ;; a window, 'mark, 'spot, (top . window), or 'spot-top; desired is
    ;; 'start, 'end, or a position in the proposed result.  A third option
    ;; is a retained edit-basis.
    ;; Placements are installed during adoption, before
    ;; callbacks can advance the head again.  They never cross the store.
    (unless (<= (length options) 3) (error 'store-edit! "too many options" options))
    (let* ([context (and (pair? options) (car options))]
           [placements (if (and (pair? options) (pair? (cdr options))) (cadr options) '())]
           [source (if (= (length options) 3) (caddr options) (edit-basis b))]
           [old (car source)]
           [basis (caddr source)]
           [proposal (delay (call-with-values (lambda () (text:apply-edit old span replacement)) list))])
      (define (project-placements actual before after)
        (map
          (lambda (entry)
            (let ([wanted (cdr entry)])
              (cons (car entry)
                    (fold-left text:rebase-position
                      (case wanted
                        [(start) (text:span-start (text:delta-span actual))]
                        [(end) (text:delta-new-end actual)]
                        [else
                         (let ([plan (force proposal)])
                           (text:rebase-result-position
                             (clamp-text-position (car plan) wanted)
                             (cadr plan) actual before))])
                      after))))
          placements))
      (check-placements! b placements)
      (store:validate-edit-context context)
      (unless (and (eqv? (cadr source) (buffer-store-id b))
                   (or (buffer-store-id b) (eq? old (buffer-lines b))))
        (raise (condition (kernel:make-refusal)
                          (make-message-condition "Edit not applied: the source buffer changed"))))
      (if (buffer-store-id b)
          (let-values ([(status info)
                        (store:edit-with-snapshot! ui-actor (buffer-store-id b)
                                                   basis span replacement context 'any)])
            (if (eq? status 'applied)
                (let* ([committed (car info)]
                       [changes (caddr info)]
                       [backwards (reverse changes)]
                       [actual (caddar backwards)]
                       [before (map caddr (reverse (cdr backwards)))])
                  (let-values ([(text revision after)
                                (store:snapshot-since (buffer-store-id b) committed)])
                    (adopt-snapshot! b basis text revision
                                     (and after (append changes after))
                                     (if after (project-placements actual before (map caddr after)) '()))
                    (note-ui-edit! b committed)))
                (let ([reason (case info
                                [(read-only) "the buffer is read-only"]
                                [(property-changed) "the buffer's reviewed facts changed"]
                                [(overlap) "another edit overlaps this change"]
                                [else "the edit's revision is no longer available"])])
                  (guard (ex [else (void)]) (sync-store-buffer! b))
                  (guard (ex [else (void)])
                    (log:add! 'store
                      (format "edit refused in ~s: ~a" (buffer-name b) reason)))
                  (raise (condition (kernel:make-refusal)
                                    (make-message-condition
                                      (format "Edit not applied: ~a" reason)))))))
          (let* ([plan (force proposal)] [text (car plan)] [delta (cadr plan)]
                 [placed (project-placements delta '() '())])
            (unless (local-facts-match? b (and context (= (length context) 5) (list-ref context 4)))
              (raise (condition (kernel:make-refusal)
                                (make-message-condition "Edit not applied: the buffer's reviewed facts changed"))))
            (rebase-buffer-positions! b delta)
            (adopt-local! b text delta)
            (apply-placements! b placed)
            (clamp-buffer-positions! b)
            (when (and context (>= (length context) 3))
              (buffer-facts-set! b
                (append (caddr context) (if (>= (length context) 4) (cadddr context) '()))))))))

  (define (placement-window place)
    (cond [(window? place) place]
          [(and (pair? place) (eq? (car place) 'top) (window? (cdr place))) (cdr place)]
          [else #f]))

  (define (check-placements! b placements)
    (unless
      (and (list? placements)
           (for-all
             (lambda (entry)
               (and (pair? entry)
                    (or (memq (car entry) '(mark spot spot-top))
                        (let ([w (placement-window (car entry))])
                          (and w (eq? (window-buffer w) b))))
                    (or (memq (cdr entry) '(start end))
                        (and (pair? (cdr entry))
                             (integer? (cadr entry)) (exact? (cadr entry)) (>= (cadr entry) 0)
                             (integer? (cddr entry)) (exact? (cddr entry)) (>= (cddr entry) 0)))))
             placements))
      (error 'head "invalid position placements" placements)))

  (define (apply-placements! b placements)
    (for-each
      (lambda (entry)
        (let ([place (car entry)] [p (cdr entry)])
          (case place
            [(mark) (buffer-mark-row-set! b (car p)) (buffer-mark-col-set! b (cdr p))]
            [(spot) (buffer-spot-row-set! b (car p)) (buffer-spot-col-set! b (cdr p))]
            [(spot-top) (buffer-spot-top-set! b (car p))]
            [else
             (let ([w (placement-window place)])
               (when (eq? (window-buffer w) b)
                 (if (window? place)
                     (begin (window-prow-set! w (car p)) (window-pcol-set! w (cdr p)))
                     (begin (window-top-set! w (car p)) (window-topseg-set! w 0)))))])))
      placements))

  (define (clamp-text-position text p)
    (let ([row (max 0 (min (car p) (- (vector-length text) 1)))])
      (cons row (max 0 (min (cdr p) (string-length (vector-ref text row)))))))

  (define (buffer-placements b)
    ;; The same anchors travel through edits, derived refits and resume.
    (append
      (list (cons 'spot (cons (buffer-spot-row b) (buffer-spot-col b)))
            (cons 'spot-top (cons (buffer-spot-top b) 0))
            (cons 'mark (cons (buffer-mark-row b) (buffer-mark-col b))))
      (apply append
        (map (lambda (w)
               (list (cons w (cons (window-prow w) (window-pcol w)))
                     (cons (cons 'top w) (cons (window-top w) 0))))
          (filter (lambda (w) (eq? (window-buffer w) b)) the-windows)))))

  (define (store-history! b direction scope)
    ;; Shared text always uses the store's attributed inverse journal.
    ;; A head snapshot is presentation state, never a source of shared
    ;; replacement text.  Only the store call maps errors to refusal;
    ;; a post-commit presentation error must not be called a refusal.
    (cond
      [(not (buffer-store-id b)) (values 'nothing #f)]
      [else
       (let-values ([(status detail)
                     (guard (ex [else (values 'blocked 'store-unavailable)])
                       (store:history-step! ui-actor (buffer-store-id b) direction scope 'any))])
         (when (eq? status 'applied)
           (sync-store-buffer! b)
           (flush-ui-audit! (buffer-store-id b)))
         (values status detail))]))

  (define new-buffer
    (case-lambda
      [(name) (new-buffer name '("") '())]
      [(name lines facts)
       (require-store-buffer! (store:create! ui-actor name lines (complete-buffer-facts facts)))]))

  (define (visit-file! name lines facts)
    (let-values ([(id created?) (store:visit! ui-actor name lines (complete-buffer-facts facts))])
      (values (require-store-buffer! id) created?)))

  (define (complete-buffer-facts facts)
    ;; Publish initial content and caller facts before callbacks. Fill only
    ;; absent defaults; both constructors share canonical reentrant adoption.
    (store:validate-properties facts)
    (append facts (remp (lambda (entry) (assq (car entry) facts)) initial-buffer-facts)))

  (define (require-store-buffer! id)
    (or (adopt-store-buffer! id) (error 'head "buffer is no longer visible" id)))

  (define (new-local-buffer name)
    ;; Local construction has no shared lifecycle. Its caller decides when
    ;; to add/show it; opaque local facts and generated content stay here.
    (let ([b (make-buffer (local-name name) (vector "") 0 (vector '() '())
                          0 0 #f 0 0 0 'default #f 0)])
      (buffer-facts-set! b initial-buffer-facts)
      (buffer-name-set! b (buffer-name b))
      b))

  (define (buffer-visible? b)
    (let ([id (buffer-store-id b)])
      (or (not id) (store:visible? ui-actor id))))

  (define (ensure-buffer-visible! b)
    (unless (buffer-visible? b)
      (error 'head "buffer is not visible to this head" (buffer-name b)))
    (let ([current (buffer-of-store-id (buffer-store-id b))])
      (when (and current (not (eq? current b)))
        (error 'head "buffer record has been retired" (buffer-name b)))))

  (define (add-buffer! b)
    ;; Enter the head's buffer list without changing its MRU order.
    ;; Claim a local label here too: another buffer may have taken
    ;; the constructor's suggested name before this one was shown.
    (ensure-buffer-visible! b)
    (unless (memq b the-buffers)
      (if (buffer-store-id b)
          (reserve-store-name! (buffer-name b))
          (buffer-name-set! b (buffer-name b)))
      (set! the-buffers (append the-buffers (list b))))
    b)

  (define (find-tool-buffer key)
    ;; A tool's key is stable across label changes and module reloads.
    ;; The live buffer list owns its lifetime; no second registry of
    ;; buffer identities needs cleanup or reload reconciliation.
    (and (string? key)
         (find (lambda (b)
                 (and (not (buffer-store-id b))
                      (equal? (buffer-fact b 'tool-key #f) key)))
               the-buffers)))

  (define (tool-buffer key)
    (unless (and (string? key) (> (string-length key) 0))
      (error 'tool-buffer "expected a nonempty string key" key))
    (or (find-tool-buffer key)
        (let ([b (new-local-buffer key)])
          (buffer-fact-set! b 'tool-key key)
          (buffer-fact-set! b 'disposable #t)
          (add-buffer! b))))

  (define (bump-buffer-revision! b)
    (buffer-revision-set! b (+ (buffer-revision b) 1)))

  (define (buffer-of-store-id id)
    (and id
         (find (lambda (b) (eqv? (buffer-store-id b) id)) the-buffers)))

  (define (adopt-store-buffer! id)
    ;; Initial content and audience come from one snapshot. Register the
    ;; record before detection can reenter adoption; a hook may also hide
    ;; or delete it, so never unconditionally add it again afterward.
    (and (store:visible? ui-actor id)
         (or (buffer-of-store-id id)
             (let-values ([(text revision facts) (store:snapshot-state id)])
               (let ([audience (assq 'audience facts)])
                 (and (actor:in-audience? ui-actor (if audience (cdr audience) 'all))
                      (call-with-display-update
                        (lambda ()
                          (let ([b (make-buffer (store:buffer-name id) text 0
                                                (vector '() '()) 0 0 #f 0 0 0
                                                'default id revision)])
                            (add-buffer! b)
                            (refresh-buffer-rendition! b)
                            (unless (assq 'wrap facts) (buffer-fact-set! b 'wrap 'default))
                            ;; Explicit #f is a mode choice, not an absent
                            ;; fact. Reattachment must not detect over it.
                            (let ([missing (list 'missing-mode)])
                              (when (eq? (buffer-fact b 'mode missing) missing) (adopt-hook b)))
                            (if (buffer-visible? b)
                                (buffer-of-store-id id)
                                (begin
                                  (forget-buffer! b)
                                  #f)))))))))))

  (define (buffer-lines-set! b new-lines)
    (store-reset! b new-lines))

  (define (clamp-buffer-positions! b)
    ;; Keep selection, saved position/viewport, and every window inside
    ;; the (possibly shorter) current lines.
    (let* ([v (buffer-lines b)]
           [last (- (vector-length v) 1)])
      (buffer-spot-row-set! b (min (buffer-spot-row b) last))
      (buffer-spot-col-set!
        b (min (buffer-spot-col b)
               (string-length (vector-ref v (buffer-spot-row b)))))
      (buffer-spot-top-set! b (min (buffer-spot-top b) last))
      (buffer-mark-row-set! b (min (buffer-mark-row b) last))
      (buffer-mark-col-set!
        b (min (buffer-mark-col b)
               (string-length (vector-ref v (buffer-mark-row b)))))
      (for-each
        (lambda (w)
          (when (eq? (window-buffer w) b)
            (window-prow-set! w (min (window-prow w) last))
            (window-pcol-set!
              w (min (window-pcol w)
                     (string-length (vector-ref v (window-prow w)))))
            (window-top-set! w (min (window-top w) last))))
        the-windows)))

  ;; The store owns a bounded set of invalidations for this reader. The
  ;; main loop adopts current truth, never retained notification payloads.
  (define take-store-changes! #f)

  ;; Intentional UI summaries supplement the base's canonical audit. Their
  ;; revision ranges describe the burst; flush on adoption of newer work,
  ;; when a burst goes stale, and at shutdown.
  (define ui-audit-bursts '())  ; (id . #(name first-rev last-rev n time))

  (define (note-ui-edit! b revision)
    (guard (ex [else (void)])
      (let* ([id (buffer-store-id b)]
             [rev revision]
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
                (log:add! 'store
                  (format "ui: ~a edit~a in ~s (revisions ~a-~a)"
                          (vector-ref v 3)
                          (if (= (vector-ref v 3) 1) "" "s")
                          (vector-ref v 0)
                          (vector-ref v 1) (vector-ref v 2))
                  #f))))
          (reverse flushed)))))

  (define (rebase-buffer-positions! b delta)
    (for-each
      (lambda (w)
        (when (eq? (window-buffer w) b)
          (let ([p (text:rebase-position
                     (cons (window-prow w) (window-pcol w)) delta)])
            (window-prow-set! w (car p))
            (window-pcol-set! w (cdr p)))
          (window-top-set!
            w (car (text:rebase-position (cons (window-top w) 0) delta)))))
      the-windows)
    (let ([p (text:rebase-position
               (cons (buffer-spot-row b) (buffer-spot-col b)) delta)])
      (buffer-spot-row-set! b (car p))
      (buffer-spot-col-set! b (cdr p)))
    (buffer-spot-top-set!
      b (car (text:rebase-position (cons (buffer-spot-top b) 0) delta)))
    (let ([p (text:rebase-position (cons (buffer-mark-row b) (buffer-mark-col b)) delta)])
      (buffer-mark-row-set! b (car p))
      (buffer-mark-col-set! b (cdr p))))

  ;; A live grid may have committed text before its matching surface. Keep
  ;; one retry id, not the intermediate text or an event backlog. Surface
  ;; publication wakes the pump even after its store notice was consumed.
  (define deferred-store-ids '())

  (define (adopt-snapshot! b basis text revision changes placements)
    ;; All anchors use the same chain, including our own edits.  A command
    ;; can explicitly place an anchor in its accepted result, but never
    ;; writes a coordinate from an older revision after adoption returns.
    ;; A callback may already have adopted part or all of this snapshot.
    (let* ([old (buffer-store-rev b)]
           [advance? (> revision old)]
           [complete? (and changes (<= basis old))]
           [deltas (and complete? (filter (lambda (entry) (> (car entry) old)) changes))]
           [facts (app-facts b)])
      (when (>= revision old)
        (let-values ([(next following)
                      (if (app-grid? facts)
                          (prepare-buffer-rendition b text revision deltas facts)
                          (values #f '()))])
          (if (and (app-grid? facts) (not (rendition-ready? facts next)))
              (let ([id (buffer-store-id b)])
                (unless (memv id deferred-store-ids)
                  (set! deferred-store-ids (cons id deferred-store-ids))))
              (begin
                (when advance?
                  (when deltas
                    (for-each (lambda (entry) (rebase-buffer-positions! b (caddr entry))) deltas)
                    (rebase-published-marks! (buffer-store-id b) deltas revision))
                  (adopt-text! b text revision deltas))
                (apply-placements! b placements)
                (clamp-buffer-positions! b)
                (if (app-grid? facts)
                    (install-buffer-rendition! b next following facts)
                    (refresh-buffer-rendition! b))
                ;; All head state is coherent before any callback can run.
                ;; Adoption only reads shared truth; it never re-dirties a save.
                (when advance?
                  (unless complete?
                    (invalidate-buffer-marks! (buffer-store-id b))
                    (log:add! 'store
                      (format "resync: ~s has no continuous history from revision ~a to ~a; positions clamped"
                              (buffer-name b) old revision))
                    (request-repaint!)))))))))

  (define (sync-store-buffer! b)
    ;; Event arrival is only a wakeup.  Reading text separately from
    ;; its deltas can adopt a newer revision than the anchors follow.
    ;; Read both atomically, ignore already adopted events, and never
    ;; replay a partial or out-of-order chain across a missing basis.
    (let ([basis (buffer-store-rev b)])
      (let-values ([(text revision changes) (store:snapshot-since (buffer-store-id b) basis)])
        (when (> revision basis) (flush-ui-audit! (buffer-store-id b)))
        (adopt-snapshot! b basis text revision changes '()))))

  (define (sync-foreign-edits! . changed-ids)
    (let* ([pending (take-store-changes!)]
           [ids (append
                  (if pending (append initial-store-ids (map car pending))
                      (append (store:buffer-list) (filter values (map buffer-store-id the-buffers))))
                  changed-ids deferred-store-ids)])
      ;; Consume the initial inventory before callbacks, just like events.
      ;; Subsequent frames only visit buffers whose store state changed.
      (set! initial-store-ids '())
      (set! deferred-store-ids '())
      (call-with-display-update
        (lambda ()
          ;; Reconcile each id once from current truth. Queued create/rename/
          ;; audience changes may already be superseded; hidden labels do not
          ;; displace local tools. Finish lifecycle, text and geometry before
          ;; notifying the painter about either text or fact changes.
          (for-each
            (lambda (id)
              (guard (ex [else (void)])
                (let ([b (buffer-of-store-id id)])
                  (if (store:visible? ui-actor id)
                    (let ([b (or b (adopt-store-buffer! id))])
                      (when b
                        (let ([name (store:buffer-name id)])
                          (unless (string=? name (buffer-name b))
                            (buffer-name-raw-set! b name)
                            (reserve-store-name! name)))
                        (sync-store-buffer! b)
                        (when (or (not pending) (memv id changed-ids)
                                  (cond [(assv id pending) => cdr] [else #f]))
                          (bump-buffer-revision! b)
                          (request-repaint!))))
                    (when b (forget-buffer! b))))))
            (let dedupe ([ids ids] [seen '()])
              (cond [(null? ids) (reverse seen)]
                [(memv (car ids) seen) (dedupe (cdr ids) seen)]
                [else (dedupe (cdr ids) (cons (car ids) seen))])))))))

  ;; What the head looks at, published as store marks other actors can
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

  ;; ((id revision marks) ...), acknowledged per buffer.  Values are
  ;; positions or endpoint pairs for regions.  Revision is part of the
  ;; comparison: the same numeric coordinates at a new revision are new
  ;; intent, not proof that the store's rebased marks already agree.
  (define published-marks '())

  (define (invalidate-buffer-marks! id)
    ;; A resync's clamped positions may equal their last published
    ;; numbers while the store has rebased the actual marks elsewhere.
    ;; Keep names for removal, but force every desired mark to republish.
    (set! published-marks
      (map (lambda (group)
             (if (eqv? (car group) id) (list id #f (caddr group)) group))
           published-marks)))

  (define (rebase-published-marks! id changes revision)
    ;; Carry acknowledged marks across an adopted chain the way the
    ;; positions moved: a cursor that did not move relative to the text
    ;; then needs no republication for the new revision.  The store
    ;; rebased its copies through the same deltas; a clamp or an actual
    ;; move still shows up as a difference and republishes.
    (define (carry value delta)
      (if (pair? (car value))
          (cons (text:rebase-position (car value) delta)
                (text:rebase-position (cdr value) delta))
          (text:rebase-position value delta)))
    (set! published-marks
      (map (lambda (group)
             (if (and (eqv? (car group) id) (cadr group))
                 (list id revision
                   (map (lambda (entry)
                          (cons (car entry)
                            (fold-left (lambda (value change) (carry value (caddr change)))
                                       (cdr entry) changes)))
                     (caddr group)))
                 group))
        published-marks)))

  (define (acknowledge-marks! id group)
    (let ([kept (remp (lambda (entry) (eqv? (car entry) id)) published-marks)])
      (set! published-marks (if group (cons group kept) kept))))

  (define (desired-head-marks)
    (fold-left
      (lambda (acc w)
        (let* ([b (window-buffer w)] [id (buffer-store-id b)])
          (if (not id)
              acc
              (let* ([old (assv id acc)]
                     [marks (if old (caddr old) '())]
                     [serial (window-serial w)]
                     [selected? (eq? w the-current)]
                     [p (cons (window-prow w) (window-pcol w))]
                     [marks (cons (cons (cons 'point serial) p) marks)]
                     [marks (if selected? (cons (cons 'point p) marks) marks)]
                     [marks
                      (if (and selected? (buffer-marked b))
                          (let ([region (cons (cons (buffer-mark-row b) (buffer-mark-col b)) p)])
                            (cons* (cons (cons 'region serial) region) (cons 'region region) marks))
                          marks)])
                (cons (list id (buffer-store-rev b) marks)
                      (remp (lambda (group) (eqv? (car group) id)) acc))))))
      '() the-windows))

  (define (mark-value value)
    ;; a region value becomes a normalized span; a point stays a pair
    (if (pair? (car value))
        (text:normalize-span
          (text:make-span (caar value) (cdar value)
                          (cadr value) (cddr value)))
        value))

  (define (publish-head-marks!)
    (let* ([desired (desired-head-marks)]
           [ids (append (map car desired)
                        (map car (filter (lambda (group) (not (assv (car group) desired)))
                                         published-marks)))]
           [resync '()])
      (for-each
        (lambda (id)
          ;; Visibility is part of publication, inside the same per-buffer
          ;; failure boundary. An outage retains acknowledgements/removal
          ;; keys; it is neither a successful publication nor a hide event.
          (guard (ex [else (void)])
            (let ([wanted (and (store:visible? ui-actor id) (assv id desired))]
                  [old (assv id published-marks)])
              (unless (equal? wanted old)
                (if (not (store:exists? id))
                    (acknowledge-marks! id #f)
                    (let* ([marks (if wanted (caddr wanted) '())]
                           [basis (if wanted (cadr wanted) (store:revision id))]
                           [updates (map (lambda (entry) (cons (car entry) (mark-value (cdr entry)))) marks)]
                           [drops (if old
                                      (map car (filter (lambda (entry) (not (assoc (car entry) marks)))
                                                       (caddr old)))
                                      '())])
                      (let-values ([(status revision) (store:set-marks! ui-actor id basis updates drops)])
                        (if (eq? status 'applied)
                            (acknowledge-marks! id wanted)
                            (begin
                              (invalidate-buffer-marks! id)
                              (let ([b (buffer-of-store-id id)])
                                (when b (set! resync (cons b resync)))))))))))))
        ids)
      ;; Finish all acknowledgements before adoption can run callbacks or
      ;; reenter a frame.  Refresh even when notification delivery lags, then
      ;; request another frame rather than spinning publication in a loop.
      (for-each (lambda (b) (guard (ex [else (void)]) (sync-store-buffer! b))) resync)
      (unless (null? resync) (wake-main!))))


  ;;; Named screen resume ------------------------------------------------------

  ;; A checkpoint is (screen 1 kill-text selected-number layout buffers).
  ;; Splits retain their ordinary orientation/weights; leaves retain a buffer
  ;; slot and window preferences. A buffer entry is (reference numbers marked
  ;; placements), where placements use window numbers instead of records.
  ;; Shared references are (shared id revision); local views register a plain
  ;; descriptor and project their coordinates without exporting their cache.
  (define resume-registry (kernel:make-registry car))
  (define last-checkpoint #f)

  (define (register-resume! kind capture restore)
    (unless (and (symbol? kind) (not (memq kind '(shared tool)))
                 (procedure? capture) (procedure? restore))
      (error 'register-resume! "expected a local view kind and two procedures"))
    (kernel:registry-add! resume-registry (list kind capture restore)))

  (define (resumer kind)
    (kernel:registry-find resume-registry (lambda (entry) (eq? (car entry) kind))))

  (define (capture-buffer b)
    (let ([positions
           (map (lambda (entry)
                  (let ([place (car entry)])
                    (cons (cond [(window? place) (window-index place)]
                                [(placement-window place) => (lambda (w) (cons 'top (window-index w)))]
                                [else place])
                      (cdr entry))))
             (buffer-placements b))])
      (let-values ([(reference positions)
                    (cond
                      [(buffer-store-id b) => (lambda (id) (values (list 'shared id (buffer-store-rev b)) positions))]
                      [(resumer (buffer-fact b 'resume-kind #f))
                       => (lambda (entry)
                            (let-values ([(reference positions) ((cadr entry) b positions)])
                              (values (and reference (cons (car entry) reference)) positions)))]
                      [(buffer-fact b 'tool-key #f)
                       => (lambda (key) (values (list 'tool key (buffer-name b)) positions))]
                      [else (values #f positions)])])
        (list reference (buffer-line-numbers-setting b) (buffer-marked b) positions))))

  ;; An idle checkpoint (a wake frame: foreign edits moved this head's
  ;; positions) goes at most once a second: resume projects the saved
  ;; positions across later edits anyway, and a foreign burst must not
  ;; publish this head's whole screen per keystroke. A changed state
  ;; inside the interval requests a frame at its end. The main loop's
  ;; own checkpoints, after this head's keys and at detach, go at once.
  (define checkpoint-sent-at #f)
  (define checkpoint-interval (make-time 'time-duration 0 1))

  (define (checkpoint! . mode)
    ;; No store reads here: every coordinate describes exactly the adopted
    ;; text/view the head just painted. Unchanged wake frames send nothing.
    (let* ([slots (map cons the-buffers (iota (length the-buffers)))]
           [layout
            (let capture ([node the-root])
              (if (window? node)
                  (list 'window (window-index node) (cdr (assq (window-buffer node) slots))
                    (window-topseg node) (window-left node) (window-wrap node) (window-following? node))
                  (list 'split (layout-split-orientation node)
                    (layout-split-first-weight node) (layout-split-second-weight node)
                    (capture (layout-split-first node)) (capture (layout-split-second node)))))]
           [state (list 'screen 1 the-kill-ring (window-index the-current) layout (map capture-buffer the-buffers))])
      (unless (equal? state last-checkpoint)
        (let ([now (current-time 'time-monotonic)]
              [due (and checkpoint-sent-at (add-duration checkpoint-sent-at checkpoint-interval))])
          (if (or (not (memq 'idle mode)) (not due) (time<=? due now))
              (let ([state (datum:copy state)])
                ;; Kill text travels only when it changed; the base keeps the
                ;; last one it received under the kept marker.
                (actor:checkpoint! ui-actor
                  (if (and last-checkpoint (equal? (caddr state) (caddr last-checkpoint)))
                      (cons* 'screen 1 'kept (cdddr state))
                      state))
                (set! last-checkpoint state)
                (set! checkpoint-sent-at now))
              (request-frame-at! due))))))

  (define (project-resume-positions positions lines changes)
    (let ([deltas (if changes (map caddr changes) '())])
      (map (lambda (entry)
             (cons (car entry) (clamp-text-position lines (fold-left text:rebase-position (cdr entry) deltas))))
        positions)))

  (define (resume-source id basis positions)
    ;; Local projections and ordinary shared buffers use one source path.
    ;; Ask for the complete chain at the saved revision, adopt current truth,
    ;; then account for any reentrant adoption before returning coordinates.
    (let ([b (adopt-store-buffer! id)])
      (if (not b) (values #f positions)
          (let-values ([(lines revision changes) (store:snapshot-since id basis)])
            (let ([positions (project-resume-positions positions lines changes)])
              (adopt-snapshot! b basis lines revision changes '())
              (let-values ([(lines revision changes) (snapshot-since b revision)])
                (values b (project-resume-positions positions lines changes))))))))

  (define (restore-buffer entry)
    (apply
      (lambda (reference numbers marked positions)
        (unless (and (memq numbers '(default #t #f)) (boolean? marked))
          (error 'resume! "invalid buffer preferences"))
        (let-values ([(b positions)
                      (if (not reference) (values #f positions)
                          (guard (ex [else (values #f positions)])
                            (case (car reference)
                              [(shared) (apply resume-source (append (cdr reference) (list positions)))]
                              [(tool)
                               (let ([b (find-tool-buffer (cadr reference))])
                                 (when b
                                   (buffer-name-set! b (caddr reference))
                                   (let ([app (app-of b)]) (when app ((app-refresh! app)))))
                                 (values b positions))]
                              [else
                               (let ([entry (resumer (car reference))])
                                 (if entry ((caddr entry) (cdr reference) positions) (values #f positions)))])))])
          (vector b (and b (content-revision b)) numbers marked positions)))
      entry))

  (define (restore-screen! state)
    (apply
      (lambda (tag version kill selected layout entries)
        (unless (and (eq? tag 'screen) (eqv? version 1) (string? kill))
          (error 'resume! "unsupported screen checkpoint"))
        (let* ([fallback (window-buffer the-current)]
               [buffers (list->vector (map restore-buffer entries))]
               [indices '()]
               [natural? (lambda (n) (and (integer? n) (exact? n) (>= n 0)))]
               [root
                (let restore ([node layout])
                  (case (car node)
                    [(window)
                     (apply
                       (lambda (tag index slot topseg left wrap following?)
                         (unless (and (for-all natural? (list index slot topseg left))
                                      (< slot (vector-length buffers)) (not (memv index indices)) (boolean? following?))
                           (error 'resume! "invalid window checkpoint"))
                         (set! indices (cons index indices))
                         (%make-window index (or (vector-ref (vector-ref buffers slot) 0) fallback)
                           0 topseg left 0 0 1 0 80 wrap following?)) node)]
                    [(split)
                     (apply
                       (lambda (tag orientation first-weight second-weight first second)
                         (unless (and (memq orientation '(right below))
                                      (for-all (lambda (n) (and (rational? n) (> n 0))) (list first-weight second-weight)))
                           (error 'resume! "invalid split checkpoint"))
                         (make-layout-split orientation (restore first) (restore second) first-weight second-weight)) node)]
                    [else (error 'resume! "invalid layout checkpoint")]))]
               [windows (layout-leaves root)]
               [current (find (lambda (w) (eqv? (window-index w) selected)) windows)])
          (unless current (error 'resume! "selected window is missing"))
          ;; Validate and translate every placement before installing the tree.
          (vector-for-each
            (lambda (entry)
              (let ([b (vector-ref entry 0)])
                (when b
                  (let ([positions
                         (map (lambda (entry)
                                (let* ([place (car entry)] [top? (pair? place)]
                                       [index (if top? (cdr place) place)]
                                       [w (and (natural? index)
                                               (find (lambda (w) (= (window-index w) index)) windows))])
                                  (cons (if w (if top? (cons 'top w) w) place) (cdr entry))))
                           (vector-ref entry 4))])
                    (check-placements! b positions)
                    (let-values ([(lines revision changes) (snapshot-since b (vector-ref entry 1))])
                      (vector-set! entry 4 (project-resume-positions positions lines changes))))))) buffers)
          (set-layout-root! root)
          (set-current! current)
          (set-kill-ring! kill)
          (let ([restored (filter values (map (lambda (entry) (vector-ref entry 0)) (vector->list buffers)))])
            (set! the-buffers (append restored (filter (lambda (b) (not (memq b restored))) the-buffers))))
          (vector-for-each
            (lambda (entry)
              (let ([b (vector-ref entry 0)])
                (when b
                  (buffer-line-numbers-setting-set! b (vector-ref entry 2))
                  (buffer-marked-set! b (vector-ref entry 3))
                  ;; apply-placements resets topseg for refits. Resume keeps
                  ;; the saved segment; the painter clamps it for this width.
                  (for-each (lambda (entry)
                              (let* ([w (placement-window (car entry))] [seg (and w (window-topseg w))])
                                (apply-placements! b (list entry))
                                (when seg (window-topseg-set! w seg)))) (vector-ref entry 4))
                  (clamp-buffer-positions! b)))) buffers)
          (request-repaint!)
          #t)) state))

  (define (resume!)
    ;; Recover the old publication's *names*, not its stale coordinates.
    ;; The ordinary exact-revision diff removes abandoned windows/regions,
    ;; including buffers no longer displayed, without touching custom marks.
    (set! published-marks
      (filter values
        (map (lambda (id)
               (guard (ex [else #f])
                 (let ([names
                        (filter (lambda (entry)
                                  (let ([name (car entry)])
                                    (or (memq name '(point region))
                                        (and (pair? name) (memq (car name) '(point region))
                                             (integer? (cdr name)) (exact? (cdr name)) (> (cdr name) 0)))))
                          (store:marks ui-actor id))])
                   (and (pair? names) (list id #f (map (lambda (entry) (cons (car entry) #f)) names))))))
          (store:buffer-list))))
    (let ([state (actor:checkpoint ui-actor)])
      (set! last-checkpoint (datum:copy state))
      (and state
           (guard (ex [else (log:add! 'head (format "Screen checkpoint ignored: ~a" (kernel:condition-text ex))) #f])
             (call-with-display-update (lambda () (restore-screen! state)))))))


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
    ;; Adopt the store's news before the layers above refresh their
    ;; views.  A frame never writes an old shared cache back to the store.
    ;; The painter supplies current terminal geometry before this fence.
    (set! frame-deadline #f)
    (sync-foreign-edits!)
    (refresh-renditions!)
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

  (define (app-fact facts key fallback)
    (let ([entry (and facts (assq key facts))]) (if entry (cdr entry) fallback)))

  (define (app-facts b)
    ;; Read one owned fact batch, including audience and endpoint identity.
    ;; Ordinary buffers avoid copying unrelated facts such as a file baseline.
    (guard (ex [else #f])
      (and (buffer-store-id b) (memq b the-buffers)
           (actor:identity? (buffer-fact b 'app #f))
           (let* ([facts (store:properties (buffer-store-id b))]
                  [owner (app-fact facts 'app #f)])
             (and (actor:identity? owner) (eq? (car owner) 'app)
                  (actor:in-audience? ui-actor (app-fact facts 'audience 'all)) facts)))))

  (define (app-live? facts) (eq? (app-fact facts 'alive #f) #t))

  (define (app-buffer? b) (or (and (app-of b) #t) (app-live? (app-facts b))))

  ;; Mouse context is head-owned; edit reexports these same parameters.
  ;; Position is a one-based viewport cell pair, buffer position is an
  ;; unclamped character pair, and button is the raw xterm code.
  (define app-event-position (make-thread-parameter #f))
  (define app-event-buffer-position (make-thread-parameter #f))
  (define app-event-button (make-thread-parameter #f))
  ;; The window with keyboard focus when a pointer event began.  The app's
  ;; own window is selected while its handler runs; a control panel that
  ;; acts on the focused window addresses this one instead.
  (define app-event-focus (make-thread-parameter #f))

  (define (captures? rule event)
    (or (eq? rule 'all)
        (and (pair? rule)
             (if (eq? (car rule) 'except) (not (member event (cdr rule)))
                 (and (member event rule) #t)))))

  (define (follows-app? w)
    (and (window-following? w) (not (eq? (window-buffer w) the-escaped-buffer))))

  (define (app-following? w)
    (and (follows-app? w) (app-live? (app-facts (window-buffer w)))
         (let ([header (render:header (buffer-rendition (window-buffer w)))])
           (and header (caddr header) #t))))

  (define (follow-app! w following?)
    (unless (boolean? following?) (error 'follow-app! "expected a boolean" following?))
    (window-following?-set! w following?)
    (void))

  (define (follow-rendition! w frame header facts)
    (let* ([b (window-buffer w)] [cursor (caddr header)] [row (car cursor)] [cell (cadr cursor)])
      (window-prow-set! w row)
      (window-pcol-set! w
        (min (render:character frame row cell) (string-length (vector-ref (buffer-lines b) row))))
      (when (app-fact facts 'manages-viewport #f)
        ;; A managed grid occupies the transcript's tail. Smaller windows
        ;; clip it around the cursor; ordinary apps keep normal scrolling.
        (let* ([count (line-count b)] [height (max 1 (window-size w))]
               [start (max 0 (- count (car (cadddr header))))])
          (window-top-set! w
            (max start (- row height -1)
                 (min (window-top w) row (max start (- count height)))))
          (window-topseg-set! w 0)
          (window-left-set! w
            (max 0 (- cell (window-content-width w) -1)
                 (min (window-left w) cell (max 0 (- (cadr (cadddr header)) (window-content-width w))))))))))

  (define (app-status b active?)
    (let ([facts (app-facts b)])
      (and facts
           (string-append (or (app-fact facts 'status #f) "")
             (if (and active? (app-live? facts)
                      (let ([rule (app-fact facts 'capture #f)]) (and rule (not (null? rule)))))
                 (if (eq? b the-escaped-buffer) " escaped" " capturing input") "")))))

  (define last-app-size #f)

  (define (request-app-size!)
    ;; One offer per focused endpoint/window/grid. The producer decides which
    ;; head owns sizing. Install the receipt before delivery can reenter.
    (let* ([w the-current] [b (window-buffer w)] [facts (app-facts b)]
           [next (and (app-live? facts)
                      (list (app-fact facts 'app #f) (buffer-store-id b) w
                            (max 1 (window-size w)) (window-content-width w)))])
      (unless (equal? next last-app-size)
        (set! last-app-size next)
        (when next
          (unless (actor:send! (car next)
                    (list 'request ui-actor (cadr next) 'resize (cdddr next)))
            (when (eq? last-app-size next) (set! last-app-size #f)))))))

  (define (dispatch-app-event! event)
    ;; Local handlers retain their result (including mouse focus decisions).
    ;; Shared capture is decided here, before sending an owned message.
    (when (string=? event "FOCUS") (set! last-app-size #f))
    (let* ([w the-current] [b (window-buffer w)] [a (app-of b)]
           [handler (and a (app-handle-event! a))])
      (if a (and handler (handler event))
          (let ([facts (app-facts b)])
            (and (app-live? facts) (not (eq? b the-escaped-buffer))
                 (captures? (app-fact facts 'capture #f) event)
                 (let* ([frame (buffer-rendition b)] [header (render:header frame)]
                        [point (or (app-event-buffer-position) (cons (window-prow w) (window-pcol w)))]
                        [data (list (cons 'point point)
                                    (cons 'cell (cons (car point) (render:column frame (car point) (cdr point))))
                                    (cons 'viewport (app-event-position)) (cons 'button (app-event-button))
                                    (list 'size (max 1 (window-size w)) (window-content-width w))
                                    (cons 'color-scheme (host-color-scheme))
                                    (cons 'revision (buffer-store-rev b))
                                    (cons 'generation (and header (car header))))])
                   ;; Focus reports are notifications, not a request to stop
                   ;; inspecting scrollback. Actual captured input resumes it.
                   (unless (member event '("FOCUS" "BLUR")) (follow-app! w #t))
                   (actor:send! (app-fact facts 'app #f)
                     (list 'input ui-actor (buffer-store-id b) event
                           (if (string=? event "PASTE") (cons (cons 'paste (read-paste)) data) data)))))))))

  (define (detach-app! b)
    ;; Preserve the app's current buffer contents while removing its
    ;; dynamic refresh and event handler.  Its presentation facts stay
    ;; with the buffer but apply only while an app owns it (see the
    ;; readers below), so it behaves like an ordinary read-only buffer
    ;; until a re-registration takes it back.
    (let ([a (app-of b)])
      (when a
        (kernel:registry-remove! app-registry
                                 (lambda (x) (eq? (app-buffer x) b))))
      (buffer-read-only-set! b #t)
      b))

  (define (register-app! target refresh! . handler)
    ;; Validate before allocating a buffer or changing registrations.
    (unless (procedure? refresh!)
      (error 'register-app! "refresh must be a procedure" refresh!))
    (when (and (pair? handler) (not (procedure? (car handler))))
      (error 'register-app! "event handler must be a procedure"
             (car handler)))
    (let* ([b (if (buffer? target)
                  (begin
                    (when (buffer-store-id target)
                      (error 'register-app! "head apps require a local buffer" target))
                    target)
                  (tool-buffer target))]
           [a (make-app b refresh! (and (pair? handler) (car handler))
                        #f 'default #f)])
      (buffer-read-only-set! b #t)
      ;; the buffer is an app's for good: a re-registration (a module
      ;; reloading) takes back the same tool, and its local facts stay.
      (buffer-fact-set! b 'app #t)
      (buffer-fact-set! b 'disposable #t)
      (add-buffer! b)
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
      (cond [(and (not a) (app-following? w))
             (caddr (caddr (render:header (buffer-rendition (window-buffer w)))))]
            [(not a) #t]
            [(eq? visibility 'default) #t]
            [(procedure? visibility)
             (guard (ex [else #t]) (visibility w))]
            [(boolean? visibility) visibility]
            [else #t])))

  ;; App presentation facts belong to the buffer, so they outlive the
  ;; app record -- a reload's re-registration finds them in place -- but
  ;; they mean something only while an app owns the buffer.  Every reader
  ;; therefore asks app-of first: a detached buffer (a dead terminal's
  ;; transcript) presents as an ordinary read-only buffer, its cursor
  ;; the read-only bar and its viewport the editor's to manage.

  (define (app-manages-window-viewport? w)
    (let ([b (window-buffer w)])
      (and (or (app-of b) (app-following? w)) (buffer-fact b 'manages-viewport #f))))

  (define (app-cursor-style b)
    ;; Local presentation or the followed shared app's shape, otherwise #f.
    (if (app-of b) (buffer-fact b 'cursor-style #f)
        (and (eq? b (window-buffer the-current)) (app-following? the-current)
             (app-fact (app-facts b) 'cursor-style #f))))

  (define (set-app-presentation! b sticky-lines scrollbar . options)
    ;; Configure presentation shared by every window showing this local
    ;; app.  Sticky rows stay above the scrollable body; scrollbar is
    ;; #f, #t (enabled using the
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
      (request-repaint!)
      b))

  (define (buffer-sticky-lines b)
    (if (app-buffer? b)
        (min (or (buffer-fact b 'sticky-lines #f) 0) (line-count b))
        0))

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

  (define (register-view! target refresh!)
    (register-app! target refresh!))

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

  (define view-append!
    (case-lambda
      [(b lines) (view-append! b lines 0)]
      [(b lines drop)
       ;; Append while optionally expiring a prefix. Tail points follow the
       ;; new end; surviving anchors keep their text, expired ones go to the
       ;; start. Reuse replacement's one adoption before repaint can reenter.
       (unless (and (buffer? b) (not (buffer-store-id b)))
         (error 'view-append! "expected a local buffer" b))
       (let* ([v (buffer-lines b)] [n (vector-length v)])
         (unless (and (list? lines) (fixnum? drop) (<= 0 drop n))
           (error 'view-append! "expected lines and a prefix length to drop" lines drop))
         (when (or (pair? lines) (> drop 0))
           (let* ([virgin? (and (= n 1) (string=? (vector-ref v 0) ""))]
                  [tails (filter (lambda (w)
                                   (and (eq? (window-buffer w) b)
                                        (= (window-prow w) (- n 1))
                                        (= (window-pcol w) (string-length (vector-ref v (- n 1))))))
                                 the-windows)]
                  [new (text:normalize (append (list-tail (vector->list v) (if virgin? 1 drop)) lines))]
                  [last (- (vector-length new) 1)]
                  [end (cons last (string-length (vector-ref new last)))])
             (view-replace! b new '()
               (map (lambda (entry)
                      (cons (car entry)
                        (cond [(memq (car entry) tails) end]
                              [(< (cadr entry) drop) '(0 . 0)]
                              [else (cons (- (cadr entry) drop) (cddr entry))])))
                    (buffer-placements b))))))]))

  (define (view-replace! b lines . options)
    ;; Adopt a local rendering, optional facts, and numeric placements as
    ;; one state before repaint callbacks can reenter.  Unplaced anchors
    ;; keep their coordinates, clamped into the new text.  A top placement
    ;; uses the key (top . window), or spot-top for the saved viewport.
    (unless (and (buffer? b) (not (buffer-store-id b)))
      (error 'view-replace! "expected a local buffer" b))
    (unless (<= (length options) 2)
      (error 'view-replace! "expected facts and position placements" options))
    (let* ([new (text:normalize lines)]
           [facts (store:validate-properties (if (pair? options) (car options) '()))]
           [placements (if (= (length options) 2) (cadr options) '())]
           [text-changed? (not (equal? (buffer-lines b) new))]
           [facts-changed?
            (exists (lambda (entry)
                      (or (not (hashtable-contains? (buffer-local-facts b) (car entry)))
                          (not (equal? (buffer-fact b (car entry) #f) (cdr entry)))))
                    facts)])
      (check-placements! b placements)
      (unless (for-all (lambda (entry) (text:position? (cdr entry))) placements)
        (error 'view-replace! "expected numeric position placements" placements))
      (when text-changed? (adopt-local! b new #f))
      (buffer-facts-set! b facts)
      (apply-placements! b placements)
      (clamp-buffer-positions! b)
      (when (and facts-changed? (not text-changed?)) (bump-buffer-revision! b))
      ;; Styles can change even when rendered text is equal.  Invalidate
      ;; cached rows for either change, and never write older state after
      ;; the callback returns: it may have adopted a newer rendering.
      (when (or text-changed? facts-changed?) (request-repaint!))))


  (define (buffer-named name)
    (find (lambda (b) (string=? (buffer-name b) name)) the-buffers))

  (define (buffer-point b)
    ;; Reading a position must not switch a window or invoke callbacks.
    (let ([w (if (eq? (window-buffer the-current) b) the-current
                 (find (lambda (w) (eq? (window-buffer w) b)) the-windows))])
      (if w (cons (window-prow w) (window-pcol w))
          (cons (buffer-spot-row b) (buffer-spot-col b)))))

  (define (set-window-buffer! w b)
    ;; Display b in w, remembering where point was in the old buffer and
    ;; restoring where it last was in the new one. Redisplaying the same
    ;; buffer preserves the live window; saved spots belong to hidden ones.
    (ensure-buffer-visible! b)
    (let ([old (window-buffer w)])
      (unless (eq? old b)
        (buffer-spot-row-set! old (window-prow w))
        (buffer-spot-col-set! old (window-pcol w))
        (buffer-spot-top-set! old (window-top w))
        (window-buffer-set! w b)
        (window-following?-set! w #t)
        (window-prow-set! w (buffer-spot-row b))
        (window-pcol-set! w (buffer-spot-col b))
        (window-top-set! w (buffer-spot-top b))
        (window-topseg-set! w 0)
        (window-left-set! w 0)
        (clamp-buffer-positions! b))
      (refresh-buffer-rendition! b)
      ;; Identity, geometry, and rendition agree before a callback can switch.
      (unless (eq? old b) (request-repaint!))))

  (define (forget-buffer! b)
    ;; Retire this head's record, never the store content. Keep its store
    ;; id: a retained reference to hidden/deleted content cannot turn local.
    ;; Remove it from fallback candidates and move windows before cleanup
    ;; callbacks; cascading/repeated retirement has one cleanup and repaint.
    (when (or (memq b the-buffers) (app-of b)
              (exists (lambda (w) (eq? (window-buffer w) b)) the-windows))
      (call-with-display-update
        (lambda ()
          (set! the-buffers (remq b the-buffers))
          (buffer-rendition-set! b #f)
          (kernel:registry-remove! app-registry (lambda (x) (eq? (app-buffer x) b)))
          (let ([fallback (or (find buffer-visible? the-buffers)
                            (new-buffer "*scratch*"))])
            (for-each (lambda (w)
                        (when (eq? (window-buffer w) b)
                          (set-window-buffer! w fallback)))
                      the-windows))
          (for-each
            (lambda (hook)
              (guard (ex [else
                          (log:add! 'kill-buffer!
                            (format "Buffer cleanup failed for ~a: ~a"
                                    (buffer-name b) (kernel:condition-text ex)))])
                (hook b)))
            (kernel:registry-items buffer-kill-hook-registry))))))


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
  (define ui-actor
    ;; Claim and subscribe together before creating buffers or marks. Both
    ;; process-root registrations outlive any extension that first imports
    ;; the head. A presence callback can immediately write to the store, so
    ;; its subscriber captures the identity before ui-actor is initialized.
    (kernel:call-with-runtime-registrations
      (lambda ()
        (let* ([requested (startup:name)]
               [seed (or requested (startup:default-name))])
          (let claim ([name seed] [suffix 2])
            (guard (ex [(kernel:registration-conflict? ex)
                        (if requested
                            (error 'e "head name already in use" requested)
                            (claim (string-append seed " " (number->string suffix))
                                   (+ suffix 1)))])
              (kernel:call-with-registration-update
                (lambda ()
                  (let ([identity (actor:register! (list 'head name)
                                    (lambda (message) (wake-main!)) 'all)])
                    (let-values ([(token take!) (store:watch! wake-main!)])
                      (set! take-store-changes! take!))
                    ;; Surface events are wakeups. The head prepares current
                    ;; demanded rows on its pump, never on a publisher thread.
                    (surface:subscribe! #f (lambda (event) (wake-main!)))
                    (actor:subscribe!
                      (lambda (events)
                        (run-on-main! (lambda () (set! last-app-size #f)))))
                    identity)))))))))

  ;;; The seat's first state ---------------------------------------------------------

  ;; Subscribe before taking the initial inventory: writes during the read
  ;; are in the inventory, the queue, or both. Deduplicate at first adoption.
  (define initial-store-ids (list-sort < (store:buffer-list)))

  ;; the seat begins as *scratch* in one window; views the modules above
  ;; register while loading join the list behind it
  (define seat-initialized
    (let ([b (or (let ([id (store:find-named "*scratch*")])
                   (and id (adopt-store-buffer! id)))
                 (new-buffer "*scratch*"))])
      (set! the-buffers (cons b (remq b the-buffers)))
      (let ([w (make-window b 0 0 0 0 0 0 0 0 'default)])
        (set! the-windows (list w))
        (set! the-root w)
        (set! the-current w))))
)
