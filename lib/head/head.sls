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
;; per-user state (copy buffer, paste text, the last command).  Key
;; dispatch lives in (dispatch), the loop's body in (main), painting in
;; (paint), the commands in (edit); the command layer still reaches the seat's
;; state through identifier-syntax facades. Hooks connect the pump and
;; loop to painting, mouse handling, and the reloadable commands.

(import (only (foundation edoc) elibrary))
(elibrary (head head)
  (export add-buffer! add-buffer-kill-hook!
    add-buffer-placement-hook! add-color-scheme-hook!
    add-pre-redraw-hook! add-publication-hook!
    add-shutdown-hook! adopt-store! adopt-store-buffer!
    after-key! app-buffer app-buffer? app-cursor-style
    app-cursor-visible-in? app-cursor-visible?
    app-cursor-visible?-set! app-event-buffer-position
    app-event-button app-event-focus app-event-position
    app-facts app-following? app-handle-event!
    app-manages-window-viewport? app-of app-refresh!
    app-refresh-error app-refresh-error-set! app?
    before-frame! buffer buffer-append! buffer-base
    buffer-conflicted buffer-fact
    buffer-fact-set! buffer-facts-set! buffer-file
    buffer-file-set! buffer-flags buffer-line buffer-line-count
    buffer-lines buffer-lines-set!
    buffer-mark-col buffer-mark-col-set! buffer-mark-row
    buffer-mark-row-set! buffer-marked buffer-marked-set!
    buffer-mode-auto buffer-modified
    buffer-modified-at buffer-modified-set! buffer-name
    buffer-name-set! buffer-named
    buffer-of-store-id buffer-placements buffer-point
    buffer-read-only buffer-read-only-set! buffer-rendition
    buffer-revision buffer-revision-set! buffer-selectable?
    buffer-spot-col buffer-spot-col-set! buffer-spot-row
    buffer-spot-row-set! buffer-spot-top buffer-spot-top-set!
    buffer-state buffer-status
    buffer-sticky-lines buffer-store-id buffer-store-rev
    buffer-trailing buffer-trailing-set!
    buffer-window-size buffer-wrap-set! buffer? buffers
    bump-buffer-revision! buttons-width call-uninterrupted
    call-with-display-update call-with-interrupt checkpoint!
    clamp-buffer-positions! content-revision copy-buffer
    copy-text current-buffer current-keys
    (rename (current current-window)) default-directory
    defer-frame! depart! dispatch-app-event!
    divider-at dividers double-click? drag edit-basis
    find-tool-buffer finish-frame! fit-layout! flush-ui-audit!
    forget-buffer! frame-presented!     goto!
    hide-popup! host-color-scheme in-main-pump input-live?
    interrupted? last-command layout layout-leaves
    layout-min-height layout-min-width layout-node!
    layout-parent layout-replace! layout-split-first
    layout-split-first-set! layout-split-first-weight
    layout-split-first-weight-set! layout-split-orientation
    layout-split-second layout-split-second-set!
    layout-split-second-weight layout-split-second-weight-set!
    layout-split? line-numbers make-app make-buffer
    make-interrupted make-layout-split make-window mark
    min-window-lines mouse-position new-buffer!
    new-local-buffer! note-ui-edit! open-directory! open-file! point popup
    popup-buttons popup-default-rows popup-limit popup-rows
    popup? prepare-quit previous-window quit! quit-command!
    quitting? read-key-event read-paste read-rendition
    refresh-renditions! refresh-visible-views! register-resume! register-widget-host! registered-apps
    replace-layout-window! replace-widget-frame! request-frame-at!
    resize-popup! resume! resume-source! root run-deferred!
    run-on-main! run-shutdown-hooks! scrollbar
    scrollbar-position set-adopt-hook! set-after-key!
    set-app-cursor-visible! set-app-manages-viewport!
    set-app-presentation! set-app-selectable!
    set-app-status-position! set-buffer-status! set-buffers!
    set-copy-text! set-current! set-current-keys! set-departure!
    set-directory-opener! set-drag!
    set-editor-state-reader! set-file-opener! set-frame-hook!
    set-last-command! set-layout-root! set-mouse-handler!
    set-mouse-position! set-point-mover! set-quit-command!
    set-repaint-hook! set-review-viewer! set-root!
    set-window-buffer! set-window-mounter! set-windows! show-buffer! show-popup!
    snapshot-since start-input-reader! store-edit!
    store-history! store-reset!
    sync-foreign-edits! tile! transfer-split!
    typed-text ui-actor     view-review!  wait-for-frame! wake-main!
    weighted-first window window-at window-auto-scrollbar-set!
    window-buffer window-buffer-set! window-button-at
    window-buttons window-buttons-width window-content-width
    window-editor window-goal window-goal-set! window-index window-left
    window-left-set! window-line window-line-number-width
    window-line-numbers window-line-numbers-set!
    window-line-numbers? window-lines window-numbered
    window-pcol window-pcol-set! window-prow window-prow-set!
    window-rendition window-scrollbar-column window-scrollbar?
    window-size window-size-set! window-status-actions-set!
    window-text window-top window-top-set! window-topseg
    window-topseg-set! window-widget window-width window-width-set!
    window-wrap window-wrap-set! window-xoff window-xoff-set!
    window? windows with-buffer with-window)
  (import (rnrs)
          (rnrs r5rs)
          (only (chezscheme) current-directory keyboard-interrupt-handler getenv eval interaction-environment open-input-string
                logbit? procedure-arity-mask
                make-parameter make-thread-parameter parameterize make-mutex with-mutex fork-thread void
                format remq cons* list-head iota time-second time-nanosecond current-time time? time-type time<? time<=? copy-time
                make-time add-duration sleep get-thread-id
                make-weak-eq-hashtable box unbox set-box!
                call-with-string-output-port)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (core publication) publication:)
          (prefix (core startup) startup:)
          (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation text) text:)
          (prefix (head checkpoint) checkpoint:)
          (prefix (head editor-state) editor-state:)
          (prefix (head interaction) interaction:)
          (prefix (head pacing) pacing:)
          (prefix (head render) render:)
          (prefix (head suspension) suspension:)
          (prefix (head terminal-state) terminal-state:)
          (prefix (head text-source) text-source:)
          (prefix (service file) file:)
          (prefix (service log) log:)
          (prefix (state actor) actor:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (state surface) surface:)
          (prefix (state view) view:)
          (prefix (only (sys sys) terminal-isig! duplicate-standard-input-port) sys:)
          (prefix (sys tty) tty:))

  ;;; The records ----------------------------------------------------------------

  ;; A store buffer caches immutable text and reads its facts from the
  ;; store.  A local buffer has no store id: its text and local-facts
  ;; live here alone.  Selection, saved position, and line-number
  ;; toggles belong to this seat in either case.
  (edoc "A buffer as this seat sees it: a cache of a store buffer's text plus per-seat presentation, or a local buffer of its own."
        (name string "the label: a shared buffer's cached, a local one's own")
        (revision integer "the seat's repaint counter")
        (mark-row integer "the mark's row")
        (mark-col integer "the mark's column")
        (marked boolean "whether the mark is active")
        (spot-row integer "point's row when last displayed")
        (spot-col integer "point's column when last displayed")
        (spot-top integer "the top row when last displayed")
        (store-id (or integer #f) "the twin in the store, or #f for a local buffer")
        (store-rev (or integer #f) "the store revision the lines last agreed with")
        (lines any "constructor text; retained internally as a shared text-source mirror")
        (local-facts hashtable "a local buffer's facts")
        (rendition (or (record frame) #f) "the cached cell projection")
        (constructor name lines revision mark-row mark-col marked spot-row spot-col spot-top store-id store-rev))
  (define-record-type buffer
    (fields (mutable name buffer-name buffer-name-raw-set!)
                                   ; shared label cache or local <name>
            (immutable lines buffer-source)
            (mutable revision)      ; the seat's repaint counter
            (mutable mark-row buffer-mark-row-raw buffer-mark-row-raw-set!)
            (mutable mark-col buffer-mark-col-raw buffer-mark-col-raw-set!)
            (mutable marked buffer-marked-raw buffer-marked-raw-set!)
            ;; where point was when the buffer was last displayed
            (mutable spot-row) (mutable spot-col) (mutable spot-top)
            ;; the buffer's twin in the (store), and the store
            ;; revision this buffer's lines last agreed with
            store-id (mutable store-rev)
            local-facts
            (mutable rendition buffer-rendition-raw buffer-rendition-set!))
    ;; The public constructor's shape: each record gets private facts and
    ;; a content revision, including extension/adoption records.
    (protocol
      (lambda (new)
        (lambda (name lines revision mark-row mark-col marked
                  spot-row spot-col spot-top store-id store-rev)
          (new name (text-source:make store-id lines (or store-rev 0)) revision mark-row mark-col marked
               spot-row spot-col spot-top store-id store-rev
               (make-eq-hashtable) #f)))))

  (define (buffer-text b) (text-source:lines (buffer-source b)))

  (edoc "A window: a view of a buffer at a place in the layout."
        (index integer "the number at the left of its status line")
        (buffer buffer "the buffer shown")
        (top integer "the first visible line")
        (topseg integer "the first visible segment of the top line")
        (left integer "the first visible column")
        (prow integer "point's row")
        (pcol integer "point's column")
        (size integer "the text height, laid out")
        (xoff integer "the first screen column")
        (width integer "the width in columns")
        (wrap (or boolean (one-of default)) "whether long lines wrap here")
        (line-numbers (or boolean (one-of default)) "whether an edit buffer shows line numbers here")
        (goal (or pair #f) "the vertical-motion goal, (column . context) while the context holds")
        (status-actions list "the painted status-line controls")
        (document-views list "retained document/widget identities; no copied interaction state"))
  (define-record-type (window %make-window window?)
    (fields
      ;; the window's number, shown at the left of its status line: 0
      ;; for the first, and every new window the smallest number no
      ;; live window holds -- a closed window's number is reused, so
      ;; the numbers on screen stay small.  (window n) finds it.
      index
      (mutable buffer) (mutable top window-top-raw window-top-raw-set!)
      ;; a soft-wrapping window may start mid-line: the first
      ;; visible segment of the top line (0 elsewhere)
      (mutable topseg)
      (mutable left)
      (mutable prow window-prow-raw window-prow-raw-set!)
      (mutable pcol window-pcol-raw window-pcol-raw-set!)
      ;; Text height is layout output; proportions belong to the split tree.
      (mutable size)
      ;; horizontal band geometry, written by the layout: the
      ;; window's first screen column and its width
      (mutable xoff)
      (mutable width)
      ;; soft-wrap long lines onto continuation rows instead of
      ;; scrolling horizontally
      (mutable wrap window-wrap-raw window-wrap-raw-set!)
      ;; line numbers beside an edit buffer's text: #t, #f, or default for
      ;; the head's line-numbers parameter
      (mutable line-numbers)
      ;; the goal column of vertical motion, with the navigation context
      ;; that set it: the goal survives exactly as long as the context
      (mutable goal)
      (mutable status-actions)
      (mutable document-views)))


  ;; The outer host retains identities, never another copy of an editor's
  ;; selection. Raw window coordinates serve the outer frame adapter and
  ;; legacy local text until the default window manager becomes a widget.

  (edoc "The editor view retained for this window's current shared document, or false for a legacy app. Reading it performs no acquisition."
        (w window "outer placement") (returns (or model #f)))
  (define (window-editor w)
    (let* ([entry (assv (buffer-store-id (window-buffer w)) (window-document-views w))]
           [d (and entry (interaction:snapshot (cdr entry)))])
      (and d (if (eq? (view:kind d) 'terminal) (cadr (assq 'text (view:children d))) (cdr entry)))))

  (define (window-document-view w)
    (let ([entry (assv (buffer-store-id (window-buffer w)) (window-document-views w))])
      (and entry (interaction:snapshot (cdr entry)) (cdr entry))))

  (edoc "The widget root hosted by a window, or false for a legacy app. This reads placement identity without acquiring a source or querying the base."
        (w window "outer placement") (returns (or model #f)))
  (define (window-widget w)
    (or (and window-mounter (window-document-view w))
      (let ([b (window-buffer w)])
        (and (not (buffer-store-id b)) (buffer-fact b 'widget-id #f)))))

  (define window-mounter #f)

  (edoc "Install the outer host's widget attachment callback, called after a placement has its document and retained view identity."
        (proc procedure "window -> unspecified"))
  (define (set-window-mounter! proc)
    (set! window-mounter proc)
    (for-each proc the-windows))

  (define (ensure-window-document-view! w)
    (let* ([b (window-buffer w)] [source (buffer-store-id b)]
           [entry (assv source (window-document-views w))] [old (and entry (cdr entry))]
           [facts (app-facts b)] [owner (and facts (cdr (assq 'app facts)))]
           [terminal? (and owner (eq? (cadr owner) 'terminal))])
      (if (not (and source (or terminal? (not facts))))
        (let ([d (and old (interaction:snapshot old))])
          (when d (interaction:release! ui-actor old (view:generation d))))
        (let* ([peer (and the-current (eq? (window-buffer the-current) b) (window-document-view the-current))]
               [id (or old
                     (if terminal?
                       (if peer (begin (interaction:flush!) (view:fork! ui-actor peer)) (terminal-state:create! ui-actor source))
                       (editor-state:create! ui-actor source
                         (cons (cons 'wrap (window-wrap-raw w)) (if (popup? w) '((read-only . #t)) '())))))])
          (let-values ([(status d) (interaction:claim! ui-actor id)])
            (unless (eq? status 'applied) (error 'ensure-window-document-view! "cannot claim editor" id status))
            (unless old
              (unless terminal?
                (let* ([peer (and the-current (eq? (window-buffer the-current) b) (window-editor the-current) the-current)]
                       [state (and peer (window-editor-state peer))])
                  (interaction:set-state! ui-actor id (content-revision b)
                    (list (cons (window-prow-raw w) (window-pcol-raw w))
                      (if state (cadr state) (cons (buffer-mark-row-raw b) (buffer-mark-col-raw b)))
                      (cons (window-top-raw w) 0) (if state (cadddr state) (buffer-marked-raw b))))))
              (window-document-views-set! w (cons (cons source id) (window-document-views w)))))))))

  (define editor-state-reader (lambda (id) #f))

  (edoc "Install the outer host's read-only adapter for a viewport's derived logical state. The callback must use an already prepared frame."
        (reader procedure "editor view -> logical state or false"))
  (define (set-editor-state-reader! reader) (set! editor-state-reader reader))

  (define (window-editor-state w)
    (let* ([entry (assv (buffer-store-id (window-buffer w)) (window-document-views w))]
           [parent (and entry (interaction:snapshot (cdr entry)))]
           [terminal? (and parent (eq? (view:kind parent) 'terminal))]
           [id (and parent (if terminal? (cadr (assq 'text (view:children parent))) (cdr entry)))]
           [d (if terminal? (interaction:snapshot id) parent)])
      (and d
        (or (and terminal? (cadr (terminal-state:state parent)) (editor-state-reader id))
          (let* ([b (window-buffer w)] [state (editor-state:state d)]
                 [points (editor-state:points (buffer-source b) (content-revision b) d)])
            (append (or points (map (lambda (p) (clamp-text-position (buffer-text b) p)) (list-head state 3)))
              (list (cadddr state))))))))

  (edoc "The text wrapping preference of this placement: default follows the source and head preference. Editor preferences belong to the retained view."
        (w window "outer placement") (returns (or boolean (one-of default))))
  (define (window-wrap w)
    (let* ([id (window-editor w)] [d (and id (interaction:snapshot id))])
      (if d (cond [(assq 'wrap (view:options d)) => cdr] [else 'default]) (window-wrap-raw w))))

  (edoc "Set a placement's wrapping preference; an editor view is configured through its current ownership lease."
        (w window "outer placement") (setting (or boolean (one-of default)) "wrapping policy"))
  (define (window-wrap-set! w setting)
    (unless (memq setting '(default #t #f)) (error 'window-wrap-set! "expected default, #t or #f" setting))
    (unless (eq? setting (window-wrap w))
      (let ([id (window-editor w)])
        (if (not id) (window-wrap-raw-set! w setting)
          (begin
            (interaction:flush!)
            (let* ([d (interaction:snapshot id)] [packet (model:snapshots (list id))]
                   [record (caddr (car (cadr packet)))]
                   [options (cons (cons 'wrap setting) (remp (lambda (p) (eq? (car p) 'wrap)) (view:options d)))])
              (unless (and (equal? id (window-editor w)) record)
                (error 'window-wrap-set! "editor placement changed"))
              (let-values ([(status rows)
                            (interaction:arrange! ui-actor
                              (list (list id (cdr (assq 'revision record)) (view:children d) options))
                              (list (list id (view:generation d))))])
                (unless (eq? status 'applied) (error 'window-wrap-set! "editor preference changed" status)))))))))

  (define (editor-references? entries)
    (and (list? entries)
         (for-all (lambda (p) (and (pair? p) (integer? (car p)) (exact? (car p)) (> (car p) 0)
                                   (model:reference? (cdr p)))) entries)
         (= (length entries) (length (fold-left (lambda (xs p) (if (memv (car p) xs) xs (cons (car p) xs))) '() entries)))))

  (define (restore-window-document-views! w entries)
    (window-document-views-set! w
      (filter (lambda (p)
                (let* ([id (cdr p)] [d (or (interaction:snapshot id) (view:snapshot id))])
                  (and d (memq (view:kind d) '(editor terminal)) (= (view:schema d) 1)
                    (equal? (view:source d) (list 'buffer (car p)))
                    (buffer-of-store-id (car p))
                    (or (not (view:owner d)) (equal? (view:owner d) ui-actor))))) entries)))

  (define (update-window-editor! w index value)
    (let ([state (window-editor-state w)])
      (interaction:set-state! ui-actor (window-editor w) (content-revision (window-buffer w))
        (map (lambda (old i) (if (= index i) value old)) state '(0 1 2 3)))))


  (edoc "The current editor caret's row, or the legacy app's point row."
        (w window "outer host") (returns integer))
  (define (window-prow w) (let ([s (window-editor-state w)]) (if s (caar s) (window-prow-raw w))))

  (edoc "The current editor caret's character column, or the legacy app's point column."
        (w window "outer host") (returns integer))
  (define (window-pcol w) (let ([s (window-editor-state w)]) (if s (cdar s) (window-pcol-raw w))))

  (edoc "The current editor's logical top row, or the legacy app's viewport row."
        (w window "outer host") (returns integer))
  (define (window-top w) (let ([s (window-editor-state w)]) (if s (car (caddr s)) (window-top-raw w))))

  (edoc "Set the caret row through the retained editor view; legacy apps keep their local point."
        (w window "outer host")
        (row integer "logical coordinate"))
  (define (window-prow-set! w row)
    (if (window-editor w) (update-window-editor! w 0 (cons row (window-pcol w))) (window-prow-raw-set! w row)))

  (edoc "Set the caret column through the retained editor view; legacy apps keep their local point."
        (w window "outer host")
        (col integer "logical coordinate"))
  (define (window-pcol-set! w col)
    (if (window-editor w) (update-window-editor! w 0 (cons (window-prow w) col)) (window-pcol-raw-set! w col)))

  (edoc "Set the logical top row through the retained editor view; legacy apps keep their local viewport."
        (w window "outer host")
        (row integer "logical coordinate"))
  (define (window-top-set! w row)
    (if (window-editor w) (update-window-editor! w 2 (cons row 0)) (window-top-raw-set! w row)))

  ;; Compatibility for commands whose receiver is still a catalogue buffer.
  ;; Their mark addresses the selected placement, never another window's mark.
  (define (buffer-editor-window b)
    (let ([w (if (and the-current (eq? b (window-buffer the-current))) the-current
               (find (lambda (w) (eq? b (window-buffer w))) the-windows))])
      (and w (window-editor w) w)))

  (edoc "The mark row of the selected placement of this document, or its legacy saved mark."
        (b buffer "outer host") (returns integer))
  (define (buffer-mark-row b)
    (let ([w (buffer-editor-window b)]) (if w (caadr (window-editor-state w)) (buffer-mark-row-raw b))))

  (edoc "The mark character column of the selected placement of this document, or its legacy saved mark."
        (b buffer "outer host") (returns integer))
  (define (buffer-mark-col b)
    (let ([w (buffer-editor-window b)]) (if w (cdadr (window-editor-state w)) (buffer-mark-col-raw b))))

  (edoc "Whether the selected placement of this document has an active mark, or its legacy mark activity."
        (b buffer "outer host") (returns boolean))
  (define (buffer-marked b)
    (let ([w (buffer-editor-window b)]) (if w (cadddr (window-editor-state w)) (buffer-marked-raw b))))

  (edoc "Set the mark row in the selected placement of this document."
        (b buffer "outer host")
        (row integer "logical coordinate"))
  (define (buffer-mark-row-set! b row)
    (let ([w (buffer-editor-window b)])
      (if w (update-window-editor! w 1 (cons row (buffer-mark-col b))) (buffer-mark-row-raw-set! b row))))

  (edoc "Set the mark character column in the selected placement of this document."
        (b buffer "outer host")
        (col integer "logical coordinate"))
  (define (buffer-mark-col-set! b col)
    (let ([w (buffer-editor-window b)])
      (if w (update-window-editor! w 1 (cons (buffer-mark-row b) col)) (buffer-mark-col-raw-set! b col))))

  (edoc "The lines in the window's document or prepared widget frame."
        (w window "the window")
        (returns vector) (effects internal))
  (define (window-lines w)
    (render:lines-vector (window-text w)))

  (edoc "The window's line source, possibly deferred; read it with render:line-ref and render:line-count."
        (w window "the window") (returns any))
  (define (window-text w)
    (buffer-text (window-buffer w)))

  (edoc "One displayed line, without formatting the rest of the window's app."
        (w window "the window") (row integer "the row") (returns string))
  (define (window-line w row) (render:line-ref (window-text w) row))

  (edoc "The cell projection of the window's document or prepared widget frame."
        (w window "the window")
        (returns (or (record frame) #f)))
  (define (window-rendition w)
    (buffer-rendition (window-buffer w)))

  (edoc "A split of the layout into two children, stacked or side by side, sharing the space by weight."
        (orientation (one-of below right) "how the children are arranged")
        (first (or window (record layout-split)) "the first child")
        (second (or window (record layout-split)) "the second child")
        (first-weight integer "the first child's share")
        (second-weight integer "the second child's share"))
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

  ;; The pop-up: window 0, the root split's second leaf, above the echo
  ;; area. It has no rows and no status line until a completion list or
  ;; another echo-area pop-up shows it, and it is never split, deleted or
  ;; focused; the rest of the layout is the root's first subtree.
  (define the-popup #f)
  (define popup-buffer #f)      ; its placeholder while hidden, outside the buffer list
  (define the-popup-rows 0)
  (define the-popup-limit #f) ; the rows a resize gave the pop-up by hand: its size from then on, at most
  (define the-screen-height 0) ; the rows the last tiling had, for the pop-up's default height
  (define the-previous #f) ; the window selected before the current one

  (edoc "The pop-up window, window 0: hidden until something is shown in it."
        (returns window))
  (define (popup)
    the-popup)

  (edoc "Whether a window is the pop-up."
        (w window "the window")
        (returns boolean))
  (define (popup? w)
    (eq? w the-popup))

  (edoc "How many text rows the pop-up has now; 0 while it is hidden."
        (returns integer))
  (define (popup-rows)
    the-popup-rows)

  (edoc "Give the pop-up a number of text rows, the windows above keeping their minimum, and repaint."
        (rows integer "the text rows"))
  (define (show-popup! rows)
    ;; Give the pop-up rows text rows (the layout keeps the windows above
    ;; their minimum) and repaint.
    (unless (and (integer? rows) (exact? rows) (> rows 0))
      (error 'show-popup! "expected a positive row count" rows))
    (set! the-popup-rows (if the-popup-limit (min rows the-popup-limit) rows))
    (request-repaint!))

  (edoc "The rows a resize by hand gave the pop-up, the most it takes from then on, or #f while it takes what its content asks."
        (returns (or integer #f)))
  (define (popup-limit)
    the-popup-limit)

  (edoc "Resize the pop-up by hand, by a number of rows, within what the windows above can spare: its size from then on, at most, a shorter content taking less."
        (delta integer "the rows to add, negative to take"))
  (define (resize-popup! delta)
    (let* ([above (if (layout-split? the-root) (layout-min-height (layout-split-first the-root)) 0)]
           [most (max 1 (- the-screen-height above 1))]
           [rows (max 1 (min most (+ (if (> the-popup-rows 0) the-popup-rows (popup-default-rows)) delta)))])
      (set! the-popup-limit rows)
      (when (> the-popup-rows 0) (set! the-popup-rows rows))
      (request-repaint!)))

  (edoc "The rows the pop-up opens with for a buffer sent to it: a third of the screen, or the size a resize by hand gave it."
        (returns integer))
  (define (popup-default-rows)
    (or the-popup-limit (max 3 (quotient the-screen-height 3))))

  (define (popup-hidden!)
    ;; the pop-up hides while it is selected: the selection returns to the
    ;; window selected before it, else the first ordinary window, told so
    (when (eq? the-current the-popup)
      (let ([w (or (and (memq the-previous the-windows) (not (popup? the-previous)) the-previous)
                   (find (lambda (w) (not (popup? w))) the-windows))])
        (when w
          (set-current! w)
          (dispatch-app-event! "FOCUS")))))

  (edoc "Hide the pop-up, restoring its own buffer, and repaint.")
  (define (hide-popup!)
    (set! the-popup-rows 0)
    (unless (eq? (window-buffer the-popup) popup-buffer)
      (set-window-buffer! the-popup popup-buffer))
    (popup-hidden!)
    (request-repaint!))

  (edoc "The seat's buffers, most recently shown first, as a fresh list."
        (returns (list-of buffer)))
  (define (buffers)
    ;; collections the head hands out are snapshots: callers keep them
    ;; without seeing later changes, and cannot disturb the seat's own
    (filter (lambda (b) (not (hashtable-ref (buffer-local-facts b) 'internal #f))) the-buffers))

  (edoc "Replace the seat's buffer list."
        (bs (list-of buffer) "the buffers, most recent first"))
  (define (set-buffers! bs)
    (set! the-buffers bs))

  (edoc "Every live window, in layout order, as a fresh list."
        (returns (list-of window)))
  (define (windows)
    (append the-windows '()))

  (edoc "Replace the seat's window list."
        (ws (list-of window) "the windows"))
  (define (set-windows! ws)
    (let ([retained (apply append (map window-document-views ws))])
      (for-each
        (lambda (w)
          (unless (memq w ws)
            (for-each (lambda (p)
                        (unless (exists (lambda (kept) (equal? (cdr kept) (cdr p))) retained)
                          (let ([d (interaction:snapshot (cdr p))])
                            (when d (interaction:release! ui-actor (cdr p) (view:generation d))))))
              (window-document-views w)))) the-windows))
    (set! the-windows ws))

  (edoc "The root of the layout tree."
        (returns (or window (record layout-split))))
  (define (root)
    the-root)

  (edoc "Replace the root of the layout tree without rederiving the windows."
        (node (or window (record layout-split)) "the tree"))
  (define (set-root! node)
    (set! the-root node))

  (define (free-window-index)
    ;; the smallest number no live window holds
    (let ([taken (map window-index the-windows)])
      (let loop ([n 0])
        (if (memv n taken) (loop (+ n 1)) n))))

  (edoc "A new window on a buffer, numbered with the smallest free number; the layout it joins decides its geometry."
        (buffer buffer "the buffer shown")
        (top integer "the first visible line")
        (topseg integer "the first visible segment of the top line")
        (left integer "the first visible column")
        (prow integer "point's row")
        (pcol integer "point's column")
        (size integer "the text height")
        (xoff integer "the first screen column")
        (width integer "the width in columns")
        (wrap (or boolean (one-of default)) "whether long lines wrap")
        (returns window) (effects internal))
  (define (make-window buffer top topseg left prow pcol size xoff width wrap)
    ;; a window is born numbered; the layout it joins decides the rest
    (let ([w (%make-window (free-window-index) buffer top topseg left prow pcol
               size xoff width wrap 'default #f '() '())])
      (window-buffer-set! w (placed-buffer! w buffer the-windows))
      (ensure-window-document-view! w) (when window-mounter (window-mounter w)) w))

  (edoc "The live window numbered n, or #f."
        (n integer "the number")
        (returns (or window #f)))
  (define (window-numbered n)
    ;; the live window numbered n, or #f
    (find (lambda (w) (eqv? (window-index w) n)) the-windows))

  ;; The head's copy buffer, *copy*, shown as [copy]: a shared buffer of
  ;; the base holding the last kill or copy as text, in this head's audience
  ;; alone, so every head has its own. Commands and prompts read and replace it, and the user can
  ;; show, edit and undo it like any buffer: each copy is one entry of its
  ;; log. It is created when first needed and disposable, so killing it asks
  ;; nothing and it does not outlive the base; the next copy recreates it.
  (define copy-name "*copy*")

  (define (local-buffer-named name)
    ;; this head's local buffer with a name, or #f; shared names never wear brackets
    (find (lambda (b) (and (not (buffer-store-id b)) (string=? (buffer-name b) name))) the-buffers))

  (define (existing-copy-buffer)
    ;; this head's copy buffer among the adopted shared buffers, or #f
    (find (lambda (b) (and (buffer-store-id b) (buffer-fact b 'copy #f))) the-buffers))

  (edoc "The head's copy buffer, the shared buffer *copy* in this head's audience, shown as [copy], holding the last kill or copy, created when first needed; asked not to create it, #f without one."
        (create? (list-of boolean) "whether to create it, at most one; #t by default")
        (returns (or buffer #f))
        (effects internal))
  (define (copy-buffer . create?)
    (or (existing-copy-buffer)
        (and (or (null? create?) (car create?))
             (let ([id (store:create! ui-actor copy-name '("")
                         (list (cons 'copy #t) (cons 'audience (list ui-actor)) (cons 'disposable #t) (cons 'trailing #f)))])
               (or (adopt-store-buffer! id) (error 'copy-buffer "the copy buffer was created but not adopted"))))))

  (edoc "The copy buffer's text, the empty string while there is no copy buffer."
        (returns string))
  (define (copy-text)
    (let ([b (existing-copy-buffer)])
      (if b (text:to-string (buffer-lines b) (buffer-trailing b)) "")))

  (edoc "Replace the copy buffer's text as one entry of its log, so undo there brings the previous copy back."
        (s string "the text"))
  (define (set-copy-text! s)
    (unless (string? s) (error 'set-copy-text! "expected a string" s))
    (unless (and (string=? s "") (not (existing-copy-buffer)))
      (let ([b (copy-buffer)])
        (let-values ([(lines trailing?) (text:from-string s)])
          (unless (and (equal? lines (buffer-lines b)) (eq? trailing? (buffer-trailing b)))
            (let-values ([(span replacement) (text:difference (buffer-lines b) lines)])
              (store-edit! b span replacement
                (list (list 'set-copy (buffer-store-rev b)) "set copy" (cons 'undo (list (cons 'trailing trailing?)))))))))))

  ;; The text of the bracketed paste just consumed: the pump's paste
  ;; handler stashes it, the PASTE key's command reads it.
  (define pending-paste "")

  (edoc "The text of the bracketed paste just consumed."
        (returns string))
  (define (read-paste)
    pending-paste)

  (edoc "The selected window."
        (returns window))
  (define (current)
    the-current)

  (edoc "The buffer shown in the selected window."
        (returns buffer))
  (define (current-buffer)
    (window-buffer the-current))

  (edoc "Point in the selected window, as (row . col)."
        (returns position))
  (define (point)
    (cons (window-prow the-current) (window-pcol the-current)))

  (edoc "The mark of the buffer in the selected window as (row . col) while it is active, else #f."
        (returns (or position #f)))
  (define (mark)
    (let ([b (window-buffer the-current)])
      (and (buffer-marked b) (cons (buffer-mark-row b) (buffer-mark-col b)))))

  (edoc "Move point in the selected window straight to a (row . col) position, clamped into the buffer's rows and the displayed line; the window stops following its app."
        (p position "where point goes"))
  (define (goto! p)
    ;; Point belongs to the selected window, for apps and text alike.
    (let ([w the-current])
      (unless (point-mover w (cons (max 0 (car p)) (max 0 (cdr p))))
        (window-prow-set! w (max 0 (min (car p) (- (render:line-count (buffer-text (window-buffer w))) 1))))
        (window-pcol-set! w (max 0 (min (cdr p) (string-length (render:line-ref (window-text w) (window-prow w)))))))))

  (define point-mover (lambda (w p) #f))

  (edoc "Install the default host's logical caret mover. Return true after moving an editor view, false to use a legacy app's point adapter."
        (proc procedure "(window position) -> handled?"))
  (define (set-point-mover! proc) (set! point-mover proc))

  (edoc "The current file's parent, an app's working directory, or the head's launch directory: absolute, abbreviated, with a trailing slash."
        (returns directory))
  (define (default-directory)
    ;; All callers get an absolute, abbreviated directory with a trailing
    ;; slash, ready for appending another path component.
    (let* ([b (window-buffer the-current)]
           [file (buffer-file b)]
           [dir (file:absolute
                  (or (and file (file:directory-part file))
                      (buffer-fact b 'directory #f)
                      (current-directory)))])
      (file:abbreviate
        (if (and (> (string-length dir) 0) (char=? (string-ref dir (- (string-length dir) 1)) #\/))
            dir
            (string-append dir "/")))))

  (edoc "Select a window, without telling the apps."
        (w window "the window"))
  (define (set-current! w)
    (unless (eq? w the-current)
      (set! the-previous the-current)
      (set! the-current w)))

  (edoc "The window selected before the current one, or #f."
        (returns (or window #f)))
  (define (previous-window)
    (and (memq the-previous the-windows) the-previous))

  (edoc "The divider rectangles of the last tiling, for painting and drag hit-testing."
        (returns list))
  (define (dividers)
    the-dividers)

  (edoc "The smallest text height a split may leave a window."
        (value integer))
  (define min-window-lines ;; the smallest text height a split may leave a window
    (make-parameter 3 (lambda (v) (max 1 v))))

  ;;; Tree geometry ----------------------------------------------------------------

  (edoc "The windows of a layout subtree, in layout order."
        (node (or window (record layout-split)) "the subtree")
        (returns (list-of window)))
  (define (layout-leaves node)
    (if (layout-split? node)
        (append (layout-leaves (layout-split-first node))
                (layout-leaves (layout-split-second node)))
        (list node)))

  (edoc "A layout tree with one node replaced by another, rewritten in place."
        (node (or window (record layout-split)) "the tree")
        (old (or window (record layout-split)) "the node to replace")
        (replacement (or window (record layout-split)) "its replacement")
        (returns (or window (record layout-split))))
  (define (layout-replace! node old replacement)
    (cond
      [(eq? node old) replacement]
      [(layout-split? node)
       (layout-split-first-set!
         node (layout-replace! (layout-split-first node) old replacement))
       (layout-split-second-set!
         node (layout-replace! (layout-split-second node) old replacement))
       node]
      [else node]))

  (edoc "The split holding a child directly within a tree, or #f."
        (node (or window (record layout-split)) "the tree")
        (child (or window (record layout-split)) "the child")
        (returns (or (record layout-split) #f)))
  (define (layout-parent node child)
    (and (layout-split? node)
         (if (or (eq? child (layout-split-first node))
                 (eq? child (layout-split-second node)))
             node
             (or (layout-parent (layout-split-first node) child)
                 (layout-parent (layout-split-second node) child)))))

  (edoc "Make a tree the seat's layout, so its leaves are the windows; the pop-up stays the root split's second leaf whatever tree arrives."
        (root (or window (record layout-split)) "the tree"))
  (define (set-layout-root! root)
    ;; the tree is the seat's windows: replacing it replaces them; the
    ;; pop-up stays the root split's second leaf whatever tree arrives
    (set! the-root (if (memq the-popup (layout-leaves root)) root
                       (make-layout-split 'below root the-popup 1 1)))
    (set-windows! (layout-leaves the-root)))

  (edoc "Replace a node of the layout by another and adopt the result."
        (old (or window (record layout-split)) "the node to replace")
        (replacement (or window (record layout-split)) "its replacement"))
  (define (replace-layout-window! old replacement)
    (set-layout-root! (layout-replace! the-root old replacement)))

  (edoc "Collapse the layout to the current window beside the pop-up when the screen is too small for its splits."
        (width integer "the screen width")
        (height integer "the height above the echo area"))
  (define (fit-layout! width height)
    ;; A screen too small for the splits collapses the tree back to
    ;; one window -- the current one -- beside the pop-up.
    (let ([rest (layout-split-first the-root)])
      (when (and (layout-split? rest)
                 (or (< width (layout-min-width rest))
                     (< height (layout-min-height the-root))))
        (set-layout-root! (if (memq the-current (layout-leaves rest))
                              the-current
                              (car (layout-leaves rest)))))))

  (edoc "The narrowest width a layout subtree fits in."
        (node (or window (record layout-split)) "the subtree")
        (returns integer))
  (define (layout-min-width node)
    (if (layout-split? node)
        (if (eq? (layout-split-orientation node) 'right)
            (+ 1 (layout-min-width (layout-split-first node))
               (layout-min-width (layout-split-second node)))
            (max (layout-min-width (layout-split-first node))
                 (layout-min-width (layout-split-second node))))
        (if (popup? node) 0 20)))

  (edoc "The shortest height a layout subtree fits in, status lines included."
        (node (or window (record layout-split)) "the subtree")
        (returns integer))
  (define (layout-min-height node)
    (if (layout-split? node)
        (if (eq? (layout-split-orientation node) 'below)
            (+ (layout-min-height (layout-split-first node))
               (layout-min-height (layout-split-second node)))
            (max (layout-min-height (layout-split-first node))
                 (layout-min-height (layout-split-second node))))
        (cond [(not (popup? node)) (+ (min-window-lines) 1)]
              ;; the pop-up's rows and status line, or nothing while hidden
              [(> the-popup-rows 0) (+ the-popup-rows 1)]
              [else 0])))

  (edoc "The extent of the first of two weighted siblings within a total, each side keeping its minimum."
        (total integer "the extent to share")
        (minimum-first integer "the first side's minimum")
        (minimum-second integer "the second side's minimum")
        (a integer "the first weight")
        (b integer "the second weight")
        (returns integer))
  (define (weighted-first total minimum-first minimum-second a b)
    (min (- total minimum-second)
         (max minimum-first (quotient (* total (max 1 a))
                                      (+ (max 1 a) (max 1 b))))))

  (edoc "Lay out a subtree into a rectangle, status rows included and one divider column between side-by-side nodes: ((window start text-height) ...), the dividers accumulating for the painter and the mouse."
        (node (or window (record layout-split)) "the subtree")
        (x integer "the left column")
        (y integer "the top row")
        (width integer "the width")
        (height integer "the height")
        (returns list))
  (define (layout-node! node x y width height)
    ;; Lay out one persistent subtree. Rectangles include leaf status rows;
    ;; side-by-side nodes reserve one visible divider column.  Divider
    ;; rectangles accumulate in (dividers) for the painter and the
    ;; mouse's drag hit-testing.
    (cond
      [(and (popup? node) (= the-popup-rows 0))
       ;; hidden: no rows, no status line, no entry to hit or paint
       (window-xoff-set! node x)
       (window-width-set! node (max 1 width))
       (window-size-set! node 0)
       '()]
      [(not (layout-split? node))
       (window-xoff-set! node x)
       (window-width-set! node (max 1 width))
       (window-size-set! node (max 1 (- height 1)))
       (list (list node y (max 1 (- height 1))))]
      [else
       (let* ([first (layout-split-first node)]
              [second (layout-split-second node)]
              [below? (eq? (layout-split-orientation node) 'below)]
              [total (- (if below? height width) (if below? 0 1))]
              [m1 (if below? (layout-min-height first)
                      (layout-min-width first))]
              [m2 (if below? (layout-min-height second)
                      (layout-min-width second))]
              ;; the pop-up takes exactly its rows, whatever the weights,
              ;; and only what the windows above can spare
              [one (if (popup? second)
                       (- total (min (max 0 (- total m1)) m2))
                       (weighted-first total m1 m2
                                       (layout-split-first-weight node)
                                       (layout-split-second-weight node)))]
              [two (- total one)])
         (if below?
             (begin
               ;; the boundary above a shown pop-up drags like any other
               (unless (and (popup? second) (= the-popup-rows 0))
                 (set! the-dividers
                   (cons (list 'below node x (+ y one -1) width)
                         the-dividers)))
               (append (layout-node! first x y width one)
                       (layout-node! second x (+ y one) width two)))
             (begin
               (set! the-dividers
                 (cons (list 'right node (+ x one) y height)
                       the-dividers))
               (append (layout-node! first x y one height)
                       (layout-node! second (+ x one 1) y two
                                     height)))))]))

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
  (define main-thread (get-thread-id))
  (define deferred '())          ; thunks posted during a nested pump

  ;; Main-thread presentation state: each frame derives its next deadline
  ;; anew. Providers request only still-live work while preparing or painting
  ;; a frame, so expiry, eviction and replacement need no alarm cancellation.
  (define frame-deadline #f)
  (define frame-pending? #f)

  (edoc "Publish any deferred full frame before a partial update or interaction; whether a frame was needed."
        (returns boolean))
  (define (finish-frame!)
    (and frame-pending?
      ;; A hook may itself prompt or redraw. Do not recursively flush the
      ;; same pending frame; an unsuccessful flush remains pending.
      (let ([complete? #f])
        (dynamic-wind
          (lambda () (set! complete? #f) (set! frame-pending? #f))
          (lambda () (frame!) (set! complete? #t) #t)
          (lambda () (unless complete? (set! frame-pending? #t)))))))

  (edoc "Ask for a frame by a monotonic deadline, the earliest request winning."
        (deadline any "a monotonic time"))
  (define (request-frame-at! deadline)
    (unless (and (time? deadline) (eq? (time-type deadline) 'time-monotonic))
      (error 'request-frame-at! "expected a monotonic deadline" deadline))
    (when (or (not frame-deadline) (time<? deadline frame-deadline))
      (set! frame-deadline (copy-time deadline))))

  (edoc "Queue a thunk for the next main-thread command boundary. Work posted by the UI runs before its next input; other threads enter through the mailbox. Nested readers defer it."
        (thunk thunk "what to run"))
  (define (run-on-main! thunk)
    (if (= main-thread (get-thread-id))
      (begin (set! deferred (cons thunk deferred)) (wake-main!))
      (kernel:mailbox-post! mailbox (cons 'run thunk))))

  ;; A burst of foreign edits (an agent's tight loop, a chatty PTY)
  ;; must not queue one repaint per event: a wake is posted only when
  ;; none is outstanding, so a burst collapses into one frame.  The
  ;; claim happens before the frame is painted, never after -- a wake
  ;; arriving mid-paint queues the next frame instead of being lost.
  (define wake-lock (make-mutex))
  (define wake-queued #f)

  (edoc "Wake the main loop for a frame, once per burst of events.")
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
  (edoc "Whether the main loop itself pumps the mailbox, so posted thunks may run right away; nested pumps defer them."
        (value boolean))
  (define in-main-pump (make-parameter #f))

  (define (run-posted! thunk)
    ;; a posted thunk's error is news, not a crash
    (guard (ex [else (log:add! 'head:run-posted! (kernel:condition-text ex))])
      (parameterize ([in-main-pump #f]) (thunk))))

  (edoc "Run the thunks a nested pump set aside, oldest first.")
  (define (run-deferred!)
    ;; Complete the causal chain before admitting another key, including a
    ;; prompt outcome followed by its parked caller's resumption.
    (let loop ()
      (let ([runs (reverse deferred)])
        (set! deferred '())
        (unless (null? runs)
          (finish-frame!) (for-each run-posted! runs) (loop)))))

  ;; The pump's hooks: the frame hook prepares and paints a frame (the
  ;; painter's, above); the mouse handler applies a report -- (handler handle? c b
  ;; x y) -> an event string, #f, or the symbol ignore for a report the
  ;; loop need not hear at all, such as pointer motion (the commands', above).
  (define frame-hook void)
  (define mouse-handler (lambda (handle? c b x y) #f))
  ;; Last reported pointer cell (1-based x . y). Keyboard input retires
  ;; mouse emphasis; the next report restores it without moving point.
  (define the-mouse-position #f)

  (edoc "The pointer's last reported (column . row), or #f."
        (returns (or pair #f)))
  (define (mouse-position)
    the-mouse-position)

  (edoc "Record the pointer's position, or #f when unknown."
        (position (or pair #f) "(column . row)"))
  (define (set-mouse-position! position)
    (set! the-mouse-position position))

  (edoc "Install the frame hook, which prepares and paints a frame."
        (proc thunk "the hook"))
  (define (set-frame-hook! proc)
    (set! frame-hook proc))

  (edoc "Install the mouse handler: (handler handle? c b x y) applies a decoded mouse report."
        (proc procedure "the handler"))
  (define (set-mouse-handler! proc)
    (set! mouse-handler proc))

  ;; What the loop asks of the commands, installed by them: how to open
  ;; the file argument, how to quit (the modified-buffers check), and
  ;; what runs after every key. The command layer reloads; the loop
  ;; does not, so these calls always use the latest installed hooks.
  (define file-opener (lambda (path) (void)))
  (define directory-opener (lambda (path) (error 'open-directory! "no directory browser is installed" path)))
  (define quit-command (lambda () (quit!)))
  (define after-key-hook void)
  (define departure (lambda () (quit!)))
  (define review-viewer void)

  (edoc "Install how the loop opens its file argument."
        (proc procedure "(open path)"))
  (define (set-file-opener! proc)
    (set! file-opener proc))

  (edoc "Install the directory browser used by file-visiting commands."
        (proc procedure "(open path)"))
  (define (set-directory-opener! proc)
    (set! directory-opener proc))

  (edoc "Install the quit command, the modified-buffers check."
        (proc thunk "the command"))
  (define (set-quit-command! proc)
    (set! quit-command proc))

  (edoc "Install what runs after every key."
        (proc thunk "the hook"))
  (define (set-after-key! proc)
    (set! after-key-hook proc))

  (edoc "Install how the editor leaves once quitting is confirmed."
        (proc thunk "the departure"))
  (define (set-departure! proc)
    (set! departure proc))

  (edoc "Install how modified buffers are shown for review before quitting."
        (proc thunk "the viewer"))
  (define (set-review-viewer! proc)
    (set! review-viewer proc))

  (edoc "Open a file through the installed opener."
        (path file "the file"))
  (define (open-file! path)
    (file-opener path))

  (edoc "Show an existing directory through the installed browser."
        (path directory "the directory"))
  (define (open-directory! path)
    (directory-opener path))

  (edoc "Run the installed quit command.")
  (define (quit-command!)
    (quit-command))

  (edoc "Run the installed after-key hook.")
  (define (after-key!)
    (after-key-hook))

  (edoc "Leave the editor through the installed departure.")
  (define (depart!)
    (departure))

  (edoc "Show the modified buffers through the installed viewer.")
  (define (view-review!)
    (review-viewer))

  (define (frame!)
    ;; Wakes and deadlines use the same preparation as direct redraws.
    (parameterize ([in-main-pump #f]) (frame-hook))
    ;; Nested prompts can temporarily borrow windows. Only an outer pump
    ;; frame checkpoints the screen the user will return to.
    (when (in-main-pump) (checkpoint! 'idle)))

  ;; The host's color scheme: prefer DSR 997 reports, with the OSC 11
  ;; background as a fallback for older terminals. Hooks run on the main
  ;; thread when the scheme changes, updating faces and terminal children.
  (define host-color-scheme-value #f)
  (define host-color-scheme-reported? #f)

  (edoc "The terminal's color scheme as detected: dark, light or #f."
        (returns (or (one-of dark light) #f)))
  (define (host-color-scheme)
    host-color-scheme-value)

  (define color-scheme-hooks (kernel:make-registry))

  (edoc "Register a hook run with the scheme when the terminal reports one."
        (hook procedure "(hook scheme)"))
  (define (add-color-scheme-hook! hook)
    (unless (procedure? hook)
      (error 'add-color-scheme-hook! "expected a procedure" hook))
    (kernel:registry-add! color-scheme-hooks hook))

  (define (note-color-scheme! scheme)
    (unless (eq? scheme host-color-scheme-value)
      (set! host-color-scheme-value scheme)
      (for-each (lambda (hook) (guard (ex [else (void)]) (hook scheme)))
                (kernel:registry-items color-scheme-hooks))
      (wake-main!)))

  ;; The seat's lifetime, and the command the dispatcher ran last (kill
  ;; chaining and typed runs ask).
  (define quit-requested #f)

  (edoc "Request that the main loop end.")
  (define (quit!)
    (set! quit-requested #t))

  (edoc "Whether quitting was requested."
        (returns boolean))
  (define (quitting?)
    quit-requested)

  (define (local-quit-state)
    ;; Own only discard facts. Local metadata may contain runtime handles
    ;; and cycles; none belongs in consent data or a wire message.
    (map (lambda (b)
           (let-values ([(text revision facts) (buffer-state b)])
             (list b text revision
               (datum:copy (filter (lambda (entry) (memq (car entry) '(disposable modified file trailing))) facts)))))
      (filter (lambda (b) (not (buffer-store-id b))) (buffers))))

  (edoc "Count the local buffers with unsaved changes: (values unsaved valid?), valid? a thunk confirming their state is unchanged.")
  (define (prepare-quit)
    ;; Ordinary quit and base shutdown use one local snapshot/recheck rule.
    (let* ([local (local-quit-state)]
           [clean (map (lambda (state)
                         (cons (car state) (file:state-clean? (cadr state) (cadddr state)))) local)])
      (values
        (length (filter (lambda (entry) (not (cdr entry))) clean))
        (lambda ()
          (for-all
            (lambda (state)
              (or (cond [(assq 'disposable (cadddr state)) => cdr] [else #f])
                  (let ([old (assq (car state) local)])
                    (and old (= (caddr state) (caddr old)) (equal? (cadddr state) (cadddr old))
                         (or (not (cdr (assq (car state) clean)))
                             (file:state-clean? (cadr state) (cadddr state)))))))
            (local-quit-state))))))

  (define the-last-command #f)

  (edoc "The command the last key ran, for commands that chain, such as consecutive kills."
        (returns any))
  (define (last-command)
    the-last-command)

  (edoc "Record the command the last key ran."
        (c any "the command"))
  (define (set-last-command! c)
    (set! the-last-command c))

  ;; the key sequence being dispatched -- the self-inserting command
  ;; reads its character here
  (define the-current-keys '())

  (edoc "The key sequence being dispatched; the self-inserting command reads its character here."
        (returns list))
  (define (current-keys)
    the-current-keys)

  (edoc "The text the key being dispatched types, a one-character string, or #f for a key that is no character: what a SELF-INSERT binding's command receives, (keymap:call edit:type! head:typed-text) say."
        (returns (or string #f)))
  (define (typed-text)
    (let ([keys (current-keys)])
      (and (pair? keys) (string? (car keys))
           (let ([c (tty:key-event-character (car keys))]) (and c (string c))))))

  (edoc "Record the key sequence being dispatched."
        (keys list "the events"))
  (define (set-current-keys! keys)
    (set! the-current-keys keys))

  ;; Whether the reader runs: a question asked before it does would wait
  ;; forever on a mailbox nothing feeds, so the prompts ask first.
  (define input-reader-started? #f)

  (define presentation-clock
    (pacing:make (lambda () (current-time 'time-monotonic)) sleep))

  (edoc "Wait for the current input's presentation deadline without pumping another event."
        (milliseconds integer "input-to-presentation budget, from 0 to 50 milliseconds"))
  (define (wait-for-frame! milliseconds)
    (pacing:wait! presentation-clock milliseconds))

  (define (keyboard-message? message)
    (and (pair? message) (eq? (car message) 'key)
         (let ([event (caddr message)]) (or (char? event) (string? event)))))

  (edoc "Defer this prepared frame only for an already-expired keyboard event at the front of the queue, within the presentation clock's bound."
        (milliseconds integer "the input-to-presentation budget")
        (returns boolean))
  (define (defer-frame! milliseconds)
    (let ([message (kernel:mailbox-peek mailbox)])
      (and (keyboard-message? message)
           (or (not frame-deadline) (time<? (current-time 'time-monotonic) frame-deadline))
           (pacing:defer? presentation-clock (cadr message) milliseconds)
           (begin (set! frame-pending? #t) #t))))

  (edoc "Record successful terminal publication; the prepared geometry is now displayed.")
  (define (frame-presented!)
    (set! frame-pending? #f)
    (pacing:presented! presentation-clock)
    (for-each (lambda (hook) (hook #f)) (kernel:registry-items publication-hooks)))

  (edoc "Whether terminal input reaches the pump: the input reader has started, so a prompt can be answered."
        (returns boolean))
  (define (input-live?) input-reader-started?)

  (edoc "Start the thread that reads terminal events into the pump's mailbox.")
  (define (start-input-reader!)
    (set! input-reader-started? #t)
    (let ([stdin (sys:duplicate-standard-input-port)])
      (fork-thread
        (lambda ()
          (let loop ()
            (let ([event (guard (ex [else (eof-object)])
                           (tty:read-event stdin))])
              (kernel:mailbox-post! mailbox
                (list 'key (current-time 'time-monotonic) event))
              (unless (eof-object? event) (loop))))))))

  (edoc "Read the next key from the pump, applying mouse reports unless handle-mouse? is #f, in which case they are consumed without being applied, for a context that must not change focus; frames and posted thunks run while waiting."
        (handle-mouse? boolean "whether to apply mouse reports")
        (returns (or char string any) "a character, an event string, or eof")
        (effects internal)
        (prompts))
  (define read-key-event
    ;; Consumers see the same names whether they are the main editor,
    ;; I-search, a prompt, or a key describer.  A context that must not
    ;; change editor focus passes #f: mouse reports are consumed
    ;; without being applied.
    (case-lambda
      [()
       (read-key-event #t)]
      [(handle-mouse?)
       (let pump ()
         (when (in-main-pump) (run-deferred!))
         ;; Fence before dequeueing: a rendering hook can itself read input.
         ;; Only ordinary keys in the outer pump may use prepared geometry;
         ;; mouse events, callbacks and modal readers require publication.
         (when frame-pending?
           (unless (and (in-main-pump)
                        (keyboard-message? (kernel:mailbox-peek mailbox)))
             (finish-frame!)))
         (let ([message (kernel:mailbox-receive! mailbox frame-deadline #t)])
           (case (and message (car message))
             [(#f)
              (frame!)
              (pump)]
             [(key)
              (let ([event (caddr message)])
                (unless (and (pair? event)
                             (memq (car event) '(host-color-scheme host-background)))
                  (pacing:input! presentation-clock (cadr message)))
                (cond
                  [(not (pair? event)) (set-mouse-position! #f) event]
                  [(eq? (car event) 'mouse)
                   (set-mouse-position! (and handle-mouse?
                                             (cons (list-ref event 3) (list-ref event 4))))
                   (let ([result (apply mouse-handler handle-mouse? (cdr event))])
                     (cond [(eq? result 'ignore)
                            ;; Swallowed, but it may have moved hover state:
                            ;; frame once the burst of reports has drained.
                            (request-frame-at! (current-time 'time-monotonic))
                            (pump)]
                           [else (or result "MOUSE-HANDLED")]))]
                  [(eq? (car event) 'paste)
                   (set-mouse-position! #f)
                   (set! pending-paste (cdr event))
                   "PASTE"]
                  [(eq? (car event) 'host-color-scheme)
                   (set! host-color-scheme-reported? #t)
                   (note-color-scheme! (cadr event))
                   (pump)]
                  [(eq? (car event) 'host-background)
                   (unless host-color-scheme-reported?
                     ;; Approximate brightness on the reported RGB scale.
                     (note-color-scheme!
                       (if (< (apply + (map * '(299 587 114) (cdr event))) 127500)
                           'dark 'light)))
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

  (edoc "The entries of the last tiling."
        (returns list))
  (define (layout)
    the-layout)

  (edoc "Tile the layout into a width and height, remembering the entries and dividers: ((window start text-height) ...)."
        (width integer "the screen width")
        (height integer "the height above the echo area")
        (returns list))
  (define (tile! width height)
    ;; Tile the tree into width x height (the screen minus the echo
    ;; area).  -> the entries, also remembered along with the dividers.
    (set! the-dividers '())
    (set! the-screen-height height)
    (set! the-layout (layout-node! the-root 0 0 width height))
    the-layout)

  (edoc "Call a receiver with the layout entry under a 0-based screen position, text rows or status line; #f in the echo area or on a divider."
        (x0 integer "the column")
        (r0 integer "the row")
        (receiver procedure "(receiver entry)")
        (returns any))
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

  (edoc "The status-line buttons: (action . label) for splitting below, splitting right and closing, flush right on the bar."
        (value list))
  (define window-buttons '((below . "↕") (right . "↔") (close . "×")))

  (edoc "The pop-up's one status-line button, where the closing × of the other windows is: ↓, the clear action, emptying the pane."
        (value list))
  (define popup-buttons '((clear . "↓")))

  (edoc "The columns a list of status-line buttons takes, a bar before each label and after the last."
        (buttons list "(action . label) pairs")
        (returns integer))
  (define (buttons-width buttons) (+ 1 (apply + (map (lambda (b) (+ 1 (string-length (cdr b)))) buttons))))

  (edoc "The columns the ordinary windows' status-line buttons take."
        (value integer))
  (define window-buttons-width (buttons-width window-buttons))

  (edoc "The (action . window) of the status-line button under a screen position, or #f."
        (x0 integer "the column")
        (r0 integer "the row")
        (returns (or pair #f)))
  (define (window-button-at x0 r0)
    ;; Paint and hit-test the same single-cell labels, flush right,
    ;; with an inert │ before each label and after the final one; the
    ;; pop-up has its own one label where the others' × is.
    ;; Inline controls carry painted, window-relative cell ranges. Return
    ;; (action . window), or #f outside a visible control.
    (window-at x0 r0
      (lambda (entry)
        (let* ([w (car entry)]
               [buttons (if (popup? w) popup-buttons window-buttons)]
               [taken (buttons-width buttons)])
          (and (= r0 (+ (cadr entry) (caddr entry)))
               (or (let ([column (- x0 (window-xoff w))])
                     (cond [(find (lambda (span)
                                    (and (<= 0 (car span) column) (< column (cadr span))
                                         (<= (cadr span) (- (window-width w) taken 1))))
                              (window-status-actions w))
                            => (lambda (span) (cons (caddr span) w))]
                           [else #f]))
                   (let loop ([buttons buttons]
                              [column (- x0 (+ (window-xoff w) (window-width w) (- taken)))])
                     (and (pair? buttons) (> column 0)
                       (let ([width (string-length (cdar buttons))])
                         (if (<= column width) (cons (caar buttons) w)
                             (loop (cdr buttons) (- column width 1))))))))))))

  (edoc "The divider descriptor under a screen position, or #f; a crossing belongs to the horizontal split."
        (x0 integer "the column")
        (r0 integer "the row")
        (returns (or list #f)))
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

  (edoc "Move a split's boundary by delta cells, normalizing its weights to the realized extents first."
        (split (record layout-split) "the split")
        (delta integer "cells toward the second side"))
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
        (if (popup? second)
            ;; the boundary moving down takes rows from the pop-up, by hand
            (resize-popup! (- delta))
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
            (layout-split-second-weight-set! split (- two delta)))))))

  ;;; Gestures -----------------------------------------------------------------------

  ;; One press owns subsequent motion/release: a divider descriptor,
  ;; (window . buffer) for text, or #f for an action that consumed the press.
  (define the-drag #f)
  (define the-last-press #f)  ; (x y ms) of the previous button press

  (edoc "The mouse gesture in progress: a divider being dragged, the (window . buffer) of a text selection, or #f."
        (returns any))
  (define (drag)
    the-drag)

  (edoc "Record the mouse gesture in progress."
        (d any "the gesture, or #f"))
  (define (set-drag! d)
    (set! the-drag d))

  (edoc "Record a press at a cell at a time in milliseconds; whether it repeats the previous press's cell within half a second."
        (x integer "the column")
        (y integer "the row")
        (now number "the time in milliseconds")
        (returns boolean)
        (effects internal))
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

  (edoc "Compose seat changes into one repaint notification; nested updates share it."
        (thunk thunk "the changes")
        (returns any "what the thunk returns"))
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

  (edoc "Install the hook that requests a repaint."
        (proc thunk "the hook"))
  (define (set-repaint-hook! proc)
    (set! repaint-hook proc))

  (edoc "Install the hook run after text is adopted from the store."
        (proc procedure "the hook"))
  (define (set-adopt-hook! proc)
    (set! adopt-hook proc))

  (define (line-count b) (render:line-count (buffer-text b)))

  ;; Facts have one owner: the store for shared buffers, the record's
  ;; table for local ones.  Both distinguish an absent key (the caller's
  ;; fallback) from an explicit #f.  Store failures propagate; no fallback
  ;; can make missing shared truth look like a successful read or write.
  ;;
  ;;   file    the visited path, or #f
  ;;   trailing whether the file ends in a newline
  ;;   modified unsaved changes (any actor's)
  ;;   modified-at last content change, as UTC nanoseconds, or #f
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

  (edoc "A buffer's fact by key, from the store for a shared buffer, else its local facts; the fallback when absent."
        (b buffer "the buffer")
        (key symbol "the fact")
        (fallback any "the value when absent")
        (returns any))
  (define (buffer-fact b key fallback)
    (let ([id (buffer-store-id b)])
      (if id
          (store:property id key fallback)
          (hashtable-ref (buffer-local-facts b) key fallback))))

  (edoc "Set one fact of a buffer."
        (b buffer "the buffer")
        (key symbol "the fact")
        (value any "its value"))
  (define (buffer-fact-set! b key value)
    (buffer-facts-set! b (list (cons key value))))

  (edoc "Set how a buffer's long lines wrap, a fact every head shares: default, #t, #f, clean for wrapping at full width without continuation marks, or (clean . columns) capping the width."
        (b buffer "the buffer to set")
        (setting (or (one-of default #t #f clean) pair) "the wrap setting") (public))
  (define (buffer-wrap-set! b setting)
    ;; clean wraps like #t but draws no continuation marks and lets the
    ;; text use the full width -- for formatted read-only presentations;
    ;; (clean . n) additionally caps the wrapping width at n columns.
    (unless (or (memq setting '(default #t #f clean))
                (and (pair? setting) (eq? (car setting) 'clean)
                     (fixnum? (cdr setting)) (>= (cdr setting) 20)))
      (error 'buffer-wrap-set! "expected default, #t, #f, clean, or (clean . columns)" setting))
    (buffer-fact-set! b 'wrap setting))

  (define (local-facts-match? b expected)
    (or (not expected)
        (and (memq b the-buffers)
             (let-values ([(text revision facts) (buffer-state b)])
               (property:matches? expected facts)))))

  (edoc "Set several facts of a buffer at once, optionally only while a review of expected facts holds, and optionally renaming it; whether the update was accepted."
        (b buffer "the buffer")
        (updates list "(key . value) facts")
        (options (list-of any) "a fact review, then a new name")
        (returns boolean))
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
                 (when (or name (assq 'internal updates)) (sync-foreign-edits! id))
                 #t))
          (let ([name (and name (unique-local-name (string-copy name) b))]
                [trailing? (buffer-trailing b)])
            (and (local-facts-match? b expected)
                 (begin
                   (for-each (lambda (entry) (hashtable-set! (buffer-local-facts b) (car entry) (cdr entry)))
                             updates)
                   (when (or (not (eq? trailing? (buffer-trailing b)))
                             (and (buffer-modified b) (not (buffer-modified-at b))))
                     (note-local-modification! b))
                   (when name (buffer-name-raw-set! b name))
                   #t))))))

  (edoc "The current truth of a buffer for save and discard decisions, shared text not yet adopted here included: (values text revision facts)."
        (b buffer "the buffer"))
  (define (buffer-state b)
    ;; Unlike the command basis, this is current shared truth for save
    ;; and discard decisions, including text not yet adopted by the head.
    (if (buffer-store-id b)
        (store:snapshot-state (buffer-store-id b))
        (let-values ([(keys data) (hashtable-entries (buffer-local-facts b))])
          (values (buffer-lines b) (content-revision b)
                  (map cons (vector->list keys) (vector->list data))))))

  (edoc "A buffer's file fact: the path it visits, or #f."
        (b buffer "the buffer")
        (returns (or file #f)))
  (define (buffer-file b)
    (buffer-fact b 'file #f))

  (edoc "Set a buffer's file fact."
        (b buffer "the buffer")
        (v (or file #f) "the new value"))
  (define (buffer-file-set! b v)
    (buffer-fact-set! b 'file v))

  (edoc "A buffer's trailing fact: whether its text ends in a newline."
        (b buffer "the buffer")
        (returns boolean))
  (define (buffer-trailing b)
    (buffer-fact b 'trailing #t))

  (edoc "Set a buffer's trailing fact."
        (b buffer "the buffer")
        (v boolean "the new value"))
  (define (buffer-trailing-set! b v)
    (buffer-fact-set! b 'trailing v))

  (edoc "A buffer's modified fact: whether a local buffer has unsaved changes."
        (b buffer "the buffer")
        (returns boolean))
  (define (buffer-modified b)
    (buffer-fact b 'modified #f))

  (edoc "Set a buffer's modified fact."
        (b buffer "the buffer")
        (v boolean "the new value"))
  (define (buffer-modified-set! b v)
    (buffer-fact-set! b 'modified v))

  (edoc "A buffer's modified-at fact: when it was last edited, or #f."
        (b buffer "the buffer")
        (returns (or integer #f)))
  (define (buffer-modified-at b)
    (buffer-fact b 'modified-at #f))

  (define (note-local-modification! b)
    (let ([now (current-time 'time-utc)])
      (hashtable-set! (buffer-local-facts b) 'modified-at
        (+ (* (time-second now) 1000000000) (time-nanosecond now)))))

  (edoc "A buffer's mode-auto fact: whether its mode follows detection."
        (b buffer "the buffer")
        (returns boolean))
  (define (buffer-mode-auto b)
    (buffer-fact b 'mode-auto #t))

  (edoc "A buffer's read-only fact: #t, #f, or a procedure deciding per edit."
        (b buffer "the buffer")
        (returns (or boolean procedure)))
  (define (buffer-read-only b)
    (buffer-fact b 'read-only #f))

  (edoc "Set a buffer's read-only fact."
        (b buffer "the buffer")
        (v (or boolean procedure) "the new value"))
  (define (buffer-read-only-set! b v)
    (buffer-fact-set! b 'read-only v))

  (edoc "A buffer's base fact: the text its file held when loaded or last saved, or #f."
        (b buffer "the buffer")
        (returns (or string #f)))
  (define (buffer-base b)
    (buffer-fact b 'base #f))

  (edoc "Whether a buffer has reload conflicts pending, the red !! of its status line: its conflicts fact, the store's count, above zero."
        (b buffer "the buffer")
        (returns boolean))
  (define (buffer-conflicted b)
    (> (buffer-fact b 'conflicts 0) 0))

  (edoc "A buffer's active flags, in canonical order: conflicted, then read-only; a conditional edit guard counts as read-only. Modification time is separate."
        (b buffer "the buffer")
        (returns (list-of buffer-flag)))
  (define (buffer-flags b)
    (property:flags (list (cons 'conflicts (buffer-fact b 'conflicts 0)) (cons 'read-only (buffer-read-only b)))))

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

  (define (per-head? audience)
    ;; whether an audience fact restricts a shared buffer to some heads, one head's alone
    (not (eq? audience 'all)))

  (define (star-stem name)
    ;; a *name*'s stem, or a name without stars itself
    (let ([n (string-length name)])
      (if (and (>= n 2) (char=? (string-ref name 0) #\*) (char=? (string-ref name (- n 1)) #\*))
          (substring name 1 (- n 1))
          name)))

  (define (split-suffix name)
    ;; a name and the suffix the store gave it against a taken name, <n>, or ""
    (let ([n (string-length name)])
      (if (and (>= n 3) (char=? (string-ref name (- n 1)) #\>))
          (let scan ([i (- n 2)])
            (cond [(and (> i 0) (char-numeric? (string-ref name i))) (scan (- i 1))]
                  [(and (> i 0) (< i (- n 2)) (char=? (string-ref name i) #\<)) (values (substring name 0 i) (substring name i n))]
                  [else (values name "")]))
          (values name ""))))

  (define (shown-name name audience self)
    ;; a shared buffer's name as this head shows it: the store's, or for a
    ;; buffer that is this head's alone, its audience restricted, the stem
    ;; in square brackets, *copy* shown as [copy]: three shapes of one
    ;; label, *scratch* shared by every head, [copy] shared through the
    ;; base but one head's, <bindings> local to the head. The suffix the store
    ;; gives a name taken by another head's buffer is the store's business,
    ;; *copy*<2> showing as [copy] too, unless this head already shows a
    ;; buffer under that label; then the suffix stays, [copy<2>]
    (if (per-head? audience)
        (let-values ([(bare suffix) (split-suffix name)])
          (let ([plain (string-append "[" (star-stem bare) "]")])
            (if (exists (lambda (b) (and (not (eq? b self)) (string=? (buffer-name b) plain))) the-buffers)
                (string-append "[" (star-stem bare) suffix "]")
                plain)))
        name))

  (define (store-name name)
    ;; the store's name behind a per-head buffer's shown one: the brackets
    ;; come off and the stars go back on, [copy] naming *copy*
    (let* ([n (string-length name)]
           [stem (if (and (>= n 3) (char=? (string-ref name 0) #\[) (char=? (string-ref name (- n 1)) #\]))
                     (substring name 1 (- n 1))
                     name)])
      (string-append "*" (star-stem stem) "*")))

  (edoc "Rename a buffer, a shared one through the store; the name must be nonempty, and a per-head buffer's brackets are the head's, not the name's."
        (b buffer "the buffer")
        (name string "its new name"))
  (define (buffer-name-set! b name)
    (unless (and (buffer? b) (string? name) (> (string-length name) 0))
      (error 'buffer-name-set! "expected a buffer and nonempty name" b name))
    (when (buffer-store-id b) (ensure-buffer-visible! b))
    (buffer-facts-set! b '() #f (if (and (buffer-store-id b) (per-head? (buffer-fact b 'audience 'all))) (store-name name) name)))

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

  (edoc "The revision of this head's adopted text source, independent of its display rendition."
        (b buffer "text source") (returns integer))
  (define (content-revision b) (text-source:revision (buffer-source b)))

  (define (adopt-text! b text revision changes)
    (let ([basis (content-revision b)])
      (buffer-rendition-set! b #f)
      ;; Mark this legacy projection adopted before notifying other readers.
      (when (buffer-store-id b) (buffer-store-rev-set! b revision))
      (text-source:adopt! (buffer-source b) basis text revision changes)
      (bump-buffer-revision! b)))

  (edoc "The adopted text, revision and exact changes since a basis. This legacy presentation adapter never fetches."
        (b buffer "the buffer") (basis (or integer #f) "earlier revision"))
  (define (snapshot-since b basis)
    (let-values ([(text revision changes) (text-source:snapshot (buffer-source b) basis)])
      (values (buffer-lines b) revision changes)))

  (edoc "Make a buffer's cache the store's current text, by reference, and refit its positions and rendition."
        (b buffer "the buffer"))
  (define (adopt-store! b)
    ;; make the cache the store's current text -- the vectors are
    ;; immutable, so adoption is reference sharing, never a copy
    (let-values ([(text revision) (store:snapshot (buffer-store-id b))])
      (adopt-text! b text revision #f)
      (clamp-buffer-positions! b)
      (refresh-buffer-rendition! b)
      (invalidate-buffer-marks! (buffer-store-id b))))

  (edoc "The projection of a visible buffer's text into cells, built on demand, or #f when its visibility cannot be read."
        (b buffer "the buffer")
        (returns (or (record frame) #f)))
  (define (buffer-rendition b)
    ;; Optional presentation fails closed when visibility cannot be read.
    ;; The next frame retries; a store outage must not stop the head pump.
    (guard (ex [else #f])
      (and (memq b the-buffers) (buffer-visible? b)
           (or (buffer-rendition-raw b)
               ;; Commands may ask for geometry immediately after an edit,
               ;; before the next surface demand. Adopted text is sufficient.
               (render:prepare #f #f (buffer-text b) (content-revision b) '())))))

  (edoc "A projection of a buffer's current text for the demanded row ranges, following a surface with a height when one is given."
        (b buffer "the buffer")
        (ranges list "the (from . to) row ranges")
        (follow-height integer "the rows to follow a surface with")
        (returns (record frame)))
  (define read-rendition
    (case-lambda
      [(b ranges) (read-rendition b ranges 0)]
      [(b ranges follow-height)
       (read-source-rendition b (buffer-text b) (content-revision b) ranges follow-height)]))

  (define (read-source-rendition b text revision ranges follow-height)
    ;; Explicit demand reads obey head visibility even through a retained
    ;; reference whose retirement notification has not reached the pump.
    (guard (ex [else #f])
      (and (memq b the-buffers) (buffer-visible? b)
           (render:prepare (buffer-rendition-raw b) (buffer-store-id b)
                           text revision ranges follow-height))))

  (define (prepare-buffer-rendition b)
    ;; Only legacy local presentation uses this adapter. Mounted editors
    ;; acquire their coherent source/rendition pair in their own service path.
    (let* ([ranges
            (fold-left
              (lambda (out w)
                (if (and (eq? (window-buffer w) b) (not (window-widget w)))
                    (let ([height (max 1 (window-size w))]
                          [point (window-prow w)]
                          [top (window-top w)])
                      (cons* (cons 0 (buffer-sticky-lines b))
                             (cons top (+ top height))
                             (cons (- point height -1) (+ point height)) out))
                    out)) '() the-windows)]
           [next (and (pair? ranges) (read-rendition b ranges))]) next))

  (define (install-buffer-rendition! b next)
    (let ([old (buffer-rendition-raw b)])
      (unless (eq? old next)
        (buffer-rendition-set! b next)
        ;; Row keys already describe the complete rendition. A cursor-only
        ;; update or viewport refill must not invalidate the whole screen.
        (unless (equal? (render:header old) (render:header next))
          (bump-buffer-revision! b)))
    ))

  (define (refresh-buffer-rendition! b)
    (let ([next (prepare-buffer-rendition b)])
      (when next (install-buffer-rendition! b next))))

  (edoc "Rebuild the cell projection of every buffer shown in a window.")
  (define (refresh-renditions!)
    (for-each refresh-buffer-rendition!
      (fold-left (lambda (seen w)
                   (let ([b (window-buffer w)]) (if (memq b seen) seen (cons b seen))))
                 '() the-windows)))

  (define (adopt-local! b text delta)
    ;; Only explicitly local buffers own their text in this head.
    (when (buffer-store-id b)
      (error 'adopt-local! "a shared buffer must commit in the store"))
    (unless (if delta (equal? (text:delta-removed delta) (text:delta-inserted delta))
                (equal? (buffer-text b) text))
      (note-local-modification! b))
    (let ([revision (+ (content-revision b) 1)])
      (adopt-text! b text revision (and delta (list (list revision ui-actor delta))))))

  (edoc "Replace a buffer's baseline, loading or rereading, with new lines and facts, optionally only while a reviewed state still matches; the accepted revision, or #f."
        (b buffer "the buffer")
        (new-lines vector "the lines")
        (options (list-of any) "facts, then a reviewed (revision fact ...) state")
        (returns (or integer #f)))
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

  (edoc "What a proposal is computed from: (lines store-id revision) of a buffer now."
        (b buffer "the buffer")
        (returns list))
  (define (edit-basis b)
    ;; A proposal retains the text it was computed from, its owner, and
    ;; its revision even if a callback advances the head while computing.
    (list (buffer-lines b) (buffer-store-id b) (content-revision b)))

  (edoc "Submit a declared edit of a buffer, a span replaced by lines, to the store under its lock, with optional presentation, head placements and a retained basis; errors propagate, never into a local fork."
        (b buffer "the buffer")
        (span any "the text span replaced")
        (replacement list "the replacement lines")
        (options (list-of any) "presentation properties, then (place . desired) placements, then an edit basis"))
  (define (store-edit! b span replacement . options)
    ;; The store rebases this declared intent under its mutation lock.
    ;; A stale result is already an unresolvable overlap/missing basis,
    ;; not permission to recompute a replacement against newer text.
    ;; Errors also propagate: no shared edit can fall back to a local
    ;; fork, including an error after the transaction has committed.
    ;; Optional head placements are (place . desired) entries: place is
    ;; a window, 'mark, 'spot, (top . window), 'spot-top, or a head-local
    ;; procedure receiving (position revision); desired is
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
        (map cons (map car placements)
          (text-source:project-positions old span replacement actual before after (map cdr placements))))
      (check-placements! b placements)
      (store:validate-edit-context context)
      (unless (and (eqv? (cadr source) (buffer-store-id b))
                   (or (buffer-store-id b) (eq? old (buffer-lines b))))
        (raise (condition (kernel:make-refusal)
                          (make-message-condition "Edit not applied: the source buffer changed"))))
      (if (buffer-store-id b)
          (let-values ([(text revision changes placed committed)
                        (guard (ex [(kernel:refusal? ex)
                                    (guard (ex [else (void)]) (sync-store-buffer! b)) (raise ex)])
                          (text-source:edit! ui-actor source span replacement context (map cdr placements)))])
            (adopt-snapshot! b basis text revision changes
              (if (null? placed) '() (map cons (map car placements) placed)))
            (note-ui-edit! (buffer-store-id b) committed))
          (let* ([plan (force proposal)] [text (car plan)] [delta (cadr plan)]
                 [placed (project-placements delta '() '())])
            (when (and (property:context-revision context)
                    (not (= (property:context-revision context) (content-revision b))))
              (raise (condition (kernel:make-refusal) (make-message-condition "Edit not applied: the reviewed text changed"))))
            (unless (local-facts-match? b (property:context-expected context))
              (raise (condition (kernel:make-refusal)
                                (make-message-condition "Edit not applied: the buffer's reviewed facts changed"))))
            (rebase-buffer-positions! b delta)
            (adopt-local! b text delta)
            (apply-placements! b placed)
            (clamp-buffer-positions! b)
            (let ([facts (append (property:context-undo context) (property:context-commit context))])
              (when (pair? facts) (buffer-facts-set! b facts)))))))

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
                    (or (procedure? (car entry)) (memq (car entry) '(mark spot spot-top))
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
          (if (procedure? place) (place p (content-revision b))
            (case place
              [(mark) (buffer-mark-row-set! b (car p)) (buffer-mark-col-set! b (cdr p))]
              [(spot) (buffer-spot-row-set! b (car p)) (buffer-spot-col-set! b (cdr p))]
              [(spot-top) (buffer-spot-top-set! b (car p))]
              [else
               (let ([w (placement-window place)])
                 (when (eq? (window-buffer w) b)
                   (if (window? place)
                     (begin (window-prow-set! w (car p)) (window-pcol-set! w (cdr p)))
                     (begin (window-top-set! w (car p)) (window-topseg-set! w 0)))))]))))
      placements))

  (define (clamp-text-position text p)
    (let ([row (max 0 (min (car p) (- (render:line-count text) 1)))])
      (cons row (max 0 (min (cdr p) (string-length (render:line-ref text row)))))))

  (edoc "The anchors of a buffer that travel through edits and resume: spot, spot-top, mark and every window's point."
        (b buffer "the buffer")
        (returns list))
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
          ;; the pop-up's view of a buffer is transient: no place of it is kept
          (filter (lambda (w) (and (eq? (window-buffer w) b) (not (popup? w)))) the-windows)))))

  (edoc "Undo or redo in a shared buffer through the store's attributed journal: (values status detail), status applied, nothing or blocked."
        (b buffer "the buffer")
        (direction (one-of undo redo) "which way")
        (scope any "whose actions: mine, all, or (actor who)"))
  (define (store-history! b direction scope)
    ;; Shared text always uses the store's attributed inverse journal.
    ;; A head snapshot is presentation state, never a source of shared
    ;; replacement text.  Only the store call maps errors to refusal;
    ;; a post-commit presentation error must not be called a refusal.
    (cond
      [(not (buffer-store-id b)) (values 'nothing #f)]
      [else
       (let-values ([(status detail)
                     (text-source:history! ui-actor (buffer-store-id b) direction scope)])
         (when (eq? status 'applied)
           (sync-store-buffer! b)
           (flush-ui-audit! (buffer-store-id b)))
         (values status detail))]))

  (edoc "Create a shared buffer with a name, and with lines and facts or one empty line, and adopt it here."
        (name string "the buffer name")
        (lines list "the lines")
        (facts list "the (key . value) facts")
        (returns buffer))
  (define new-buffer!
    (case-lambda
      [(name)
       (new-buffer! name '("") '())]
      [(name lines facts)
       (require-store-buffer! (store:create! ui-actor name lines (complete-buffer-facts facts)))]))

  (define (complete-buffer-facts facts)
    ;; Publish initial content and caller facts before callbacks. Fill only
    ;; absent defaults; both constructors share canonical reentrant adoption.
    (store:validate-properties facts)
    (append facts (remp (lambda (entry) (assq (car entry) facts)) initial-buffer-facts)))

  (define (require-store-buffer! id)
    (or (adopt-store-buffer! id) (error 'head "buffer is no longer visible" id)))

  (edoc "A local buffer, this head's alone, with one empty line; the caller adds or shows it."
        (name string "the buffer name")
        (returns buffer))
  (define (new-local-buffer! name)
    ;; Local construction has no shared lifecycle. Its caller decides when
    ;; to add/show it; opaque local facts and generated content stay here.
    (let ([b (make-buffer (local-name name) (vector "") 0
                          0 0 #f 0 0 0 #f 0)])
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

  (edoc "Enter a buffer into the head's list without changing the recency order, claiming its label."
        (b buffer "the buffer"))
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

  (edoc "The live local tool buffer with a key, or #f."
        (key string "the tool key")
        (returns (or buffer #f)))
  (define (find-tool-buffer key)
    ;; A tool's key is stable across label changes and module reloads.
    ;; The live buffer list owns its lifetime; no second registry of
    ;; buffer identities needs cleanup or reload reconciliation.
    (and (string? key)
         (find (lambda (b)
                 (and (not (buffer-store-id b))
                      (equal? (buffer-fact b 'tool-key #f) key)))
               the-buffers)))

  (edoc "Append lines to a buffer, transcript style: a fresh buffer's single empty line is replaced, and point follows to the last line in every window showing it."
        (b buffer "the buffer to extend")
        (new-lines (list-of string) "the lines to add"))
  (define (buffer-append! b . new-lines)
    ;; A transcript belongs in the buffer list even before it is shown; the
    ;; display follows -- point moves to the last line in every window
    ;; showing b, and in ones that show it later.
    (unless (for-all string? new-lines) (error 'buffer-append! "expected line strings" new-lines))
    (add-buffer! b)
    (when (pair? new-lines)
      (let* ([v (buffer-lines b)]
             [last (- (vector-length v) 1)]
             [col (string-length (vector-ref v last))]
             [empty? (and (zero? last) (zero? col))])
        (store-edit! b (text:make-span last col last col)
                     (if empty? new-lines (cons "" new-lines))))
      (let ([last (- (render:line-count (buffer-text b)) 1)])
        (buffer-spot-row-set! b last)
        (buffer-spot-col-set! b 0)
        (for-each (lambda (w)
                    (when (eq? (window-buffer w) b)
                      (window-prow-set! w last)
                      (window-pcol-set! w 0)))
                  the-windows))))

  (edoc "Advance a buffer's repaint counter."
        (b buffer "the buffer"))
  (define (bump-buffer-revision! b)
    (buffer-revision-set! b (+ (buffer-revision b) 1)))

  (edoc "This head's buffer for a store id, or #f."
        (id (or integer #f) "the store id")
        (returns (or buffer #f)))
  (define (buffer-of-store-id id)
    (and id
         (find (lambda (b) (eqv? (buffer-store-id b) id)) the-buffers)))

  (edoc "Adopt a store buffer visible to this head, creating its record from one snapshot, or #f when it is not visible."
        (id integer "the store id")
        (returns (or buffer #f)))
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
                          (let ([b (make-buffer (shown-name (store:buffer-name id) (cond [(assq 'audience facts) => cdr] [else 'all]) #f) text 0
                                                0 0 #f 0 0 0
                                                id revision)])
                            (hashtable-set! (buffer-local-facts b) 'internal (cond [(assq 'internal facts) => cdr] [else #f]))
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

  (edoc "Materialize a buffer's complete text as a vector; use buffer-line and buffer-line-count to read a local view without rendering unseen rows."
        (b buffer "the buffer") (returns vector) (effects internal))
  (define (buffer-lines b) (render:lines-vector (buffer-text b)))

  (edoc "How many lines a buffer has."
        (b buffer "the buffer to measure")
        (returns integer))
  (define (buffer-line-count b)
    (render:line-count (buffer-text b)))

  (edoc "One line of a buffer, by zero-based row."
        (b buffer "the buffer to read")
        (row integer "the row")
        (returns string))
  (define (buffer-line b row)
    (render:line-ref (buffer-text b) row))

  (edoc "Replace a buffer's text as a new baseline, through store-reset!."
        (b buffer "the buffer")
        (new-lines vector "the lines"))
  (define (buffer-lines-set! b new-lines)
    (store-reset! b new-lines))

  (edoc "Keep a buffer's selection, saved position and every window showing it inside its current lines."
        (b buffer "the buffer"))
  (define (clamp-buffer-positions! b)
    ;; Keep selection, saved position/viewport, and every window inside
    ;; the (possibly shorter) current lines.
    (let* ([v (buffer-text b)]
           [last (- (render:line-count v) 1)])
      (buffer-spot-row-set! b (min (buffer-spot-row b) last))
      (buffer-spot-col-set!
        b (min (buffer-spot-col b)
               (string-length (render:line-ref v (buffer-spot-row b)))))
      (buffer-spot-top-set! b (min (buffer-spot-top b) last))
      (if window-mounter
        (begin
          (buffer-mark-row-raw-set! b (min (buffer-mark-row-raw b) last))
          (buffer-mark-col-raw-set! b (min (buffer-mark-col-raw b) (string-length (render:line-ref v (buffer-mark-row-raw b))))))
        (begin
          (buffer-mark-row-set! b (min (buffer-mark-row b) last))
          (buffer-mark-col-set! b (min (buffer-mark-col b) (string-length (render:line-ref v (buffer-mark-row b)))))))
      (for-each
        (lambda (w)
          (when (and (eq? (window-buffer w) b) (not (and window-mounter (window-editor w))))
            (window-prow-set! w (min (window-prow w) last))
            (window-pcol-set!
              w (min (window-pcol w)
                     (string-length (render:line-ref (window-text w) (window-prow w)))))
            (window-top-set! w (min (window-top w) last))))
        the-windows)))

  ;; The store owns a bounded set of invalidations for this reader. The
  ;; main loop adopts current truth, never retained notification payloads.
  (define take-store-changes! #f)

  ;; Intentional UI summaries supplement the base's canonical audit. Their
  ;; revision ranges describe the burst; flush on adoption of newer work,
  ;; when a burst goes stale, and at shutdown.
  (define ui-audit-bursts '())  ; (id . #(name first-rev last-rev n time))

  (edoc "Record an admitted UI edit for the displayed document's coalesced audit. Drafts without a document catalogue entry do not create audit messages."
        (id integer "source document") (revision (or integer #f) "committed revision, false for no text change"))
  (define (note-ui-edit! id revision)
    (guard (ex [else (void)])
      (let* ([b (buffer-of-store-id id)] [rev revision]
             [hit (assv id ui-audit-bursts)]
             [now (time-second (current-time 'time-monotonic))])
        (when (and b rev)
          (if hit
            (let ([v (cdr hit)])
              (vector-set! v 2 rev)
              (vector-set! v 3 (+ (vector-ref v 3) 1))
              (vector-set! v 4 now))
            (set! ui-audit-bursts
              (cons (cons id (vector (buffer-name b) rev rev 1 now))
                    ui-audit-bursts)))))))

  (edoc "Send this head's batched audit records: a buffer id's, the stale ones, or all."
        (which (or integer (one-of stale all)) "which records"))
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
                (log:add! 'head:flush-ui-audit!
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
        (when (and (eq? (window-buffer w) b) (not (window-editor w)))
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
    (let ([p (text:rebase-position (cons (buffer-mark-row-raw b) (buffer-mark-col-raw b)) delta)])
      (buffer-mark-row-raw-set! b (car p))
      (buffer-mark-col-raw-set! b (cdr p))))

  (define (adopt-snapshot! b basis text revision changes placements)
    ;; Mirrors adopt latest text independently of presentation. Each editor
    ;; retains its own coherent source/surface packet across publication gaps.
    (let* ([old (buffer-store-rev b)] [advance? (> revision old)]
           [complete? (and changes (<= basis old))]
           [deltas (and complete? (filter (lambda (entry) (> (car entry) old)) changes))])
      (when (>= revision old)
        (when advance?
          (when (or (not complete?) (exists (lambda (entry) (not (equal? (cadr entry) ui-actor))) deltas))
            (flush-ui-audit! (buffer-store-id b)))
          (when deltas
            (for-each (lambda (entry) (rebase-buffer-positions! b (caddr entry))) deltas)
            (rebase-published-marks! (buffer-store-id b) deltas revision))
          (adopt-text! b text revision deltas))
        (apply-placements! b placements)
        (clamp-buffer-positions! b)
        (refresh-buffer-rendition! b)
        (when (and advance? (not complete?))
          (invalidate-buffer-marks! (buffer-store-id b))
          (log:add! 'head:adopt-snapshot!
            (format "resync: ~s has no continuous history from revision ~a to ~a; positions clamped"
              (buffer-name b) old revision))
          (request-repaint!)))))

  (define source-observer
    (text-source:observe!
      (lambda (source basis text revision changes)
        (let ([b (buffer-of-store-id (text-source:id source))])
          (when (and b (> revision (buffer-store-rev b)))
            (adopt-snapshot! b basis text revision changes '()))))))

  (define (sync-store-buffer! b)
    ;; Event arrival is only a wakeup.  Reading text separately from
    ;; its deltas can adopt a newer revision than the anchors follow.
    ;; Read both atomically, ignore already adopted events, and never
    ;; replay a partial or out-of-order chain across a missing basis.
    (let ([basis (buffer-store-rev b)])
      (let-values ([(text revision changes) (store:snapshot-since (buffer-store-id b) basis)])
        (adopt-snapshot! b basis text revision changes '()))))

  (edoc "Adopt the store's pending changes, and those of given buffer ids, before a frame."
        (changed-ids (list-of integer) "store ids to adopt as well"))
  (define (sync-foreign-edits! . changed-ids)
    (let* ([pending (take-store-changes!)]
           [ids (append
                  (if pending (append initial-store-ids (map car pending))
                      (append (store:buffer-list) (filter values (map buffer-store-id the-buffers))))
                  changed-ids)])
      ;; Consume the initial inventory before callbacks, just like events.
      ;; Subsequent frames only visit buffers whose store state changed.
      (set! initial-store-ids '())
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
                    (if (and (not b) (store:property id 'internal #f))
                      (text-source:open! ui-actor id)
                      (let ([b (or b (adopt-store-buffer! id))])
                        (when b
                          (hashtable-set! (buffer-local-facts b) 'internal (buffer-fact b 'internal #f))
                          (let ([name (shown-name (store:buffer-name id) (buffer-fact b 'audience 'all) b)])
                            (unless (string=? name (buffer-name b))
                              (buffer-name-raw-set! b name)
                              (reserve-store-name! (store:buffer-name id))))
                          (sync-store-buffer! b)
                          (when (or (not pending) (memv id changed-ids)
                                  (cond [(assv id pending) => cdr] [else #f]))
                            (bump-buffer-revision! b)
                            (request-repaint!)))))
                    (begin (text-source:forget! id) (when b (forget-buffer! b)))))))
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

  ;; A checkpoint is (screen 6 selected-number layout buffers).
  ;; Version 1 had no capture preference; restore those windows with partial
  ;; capture. Version 2 kept line numbers per buffer; a window restored from
  ;; it follows the default. Splits retain their ordinary orientation/weights;
  ;; leaves retain a buffer slot and window preferences. A buffer entry is
  ;; (reference marked placements), where placements use window numbers
  ;; instead of records.
  ;; Shared references are (shared id revision); local views register a plain
  ;; descriptor and project their coordinates without exporting their cache.
  (define resume-registry (kernel:make-registry car))
  (define checkpoint-writer
    (checkpoint:make! (lambda (state) (actor:checkpoint! ui-actor state)) wake-main!))
  ;; Keep only the current captured text per local buffer, not another copy
  ;; of every historical vector that undo or an extension might retain.
  (define checkpoint-texts (make-weak-eq-hashtable))
  (define publication-hooks (kernel:make-registry))

  (edoc "Register head state publication after presentation and before checkpoints. The hook must queue without waiting unless fence? is true before a lifecycle checkpoint."
        (hook procedure "(hook fence?)"))
  (define (add-publication-hook! hook)
    (unless (procedure? hook) (error 'add-publication-hook! "expected a procedure"))
    (kernel:registry-add! publication-hooks hook))

  (define (without-copy-slot state)
    ;; screen checkpoints before version 4 carried the copy text third; it is not restored
    (if (and (list? state) (= (length state) 6) (memv (cadr state) '(1 2 3)))
        (cons* (car state) (cadr state) (cdddr state))
        state))

  (edoc "Register how a kind of local view is captured for a checkpoint and restored on resume."
        (kind symbol "the view kind")
        (capture procedure "the capture")
        (restore procedure "the restore"))
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
             (filter (lambda (entry)
                       (let ([w (placement-window (car entry))])
                         (not (and w (window-editor w)))))
               (cons (cons 'mark (cons (buffer-mark-row-raw b) (buffer-mark-col-raw b)))
                 (remp (lambda (entry) (eq? (car entry) 'mark)) (buffer-placements b)))))])
      (let-values ([(reference positions)
                    (cond
                      [(buffer-store-id b) => (lambda (id) (values (list 'shared id (buffer-store-rev b)) positions))]
                      [(resumer (buffer-fact b 'resume-kind #f))
                       => (lambda (entry)
                            (let-values ([(reference positions) ((cadr entry) b positions)])
                              (values (and reference (cons (car entry) reference)) positions)))]
                      [(buffer-fact b 'tool-key #f)
                       => (lambda (key) (values (list 'tool key (buffer-name b)) positions))]
                      [(app-of b) (values #f positions)]
                      [else
                       ;; Queue a self-contained text snapshot. The writer can
                       ;; omit it only against acknowledged state, never against
                       ;; a pending checkpoint that may be replaced.
                       (let-values ([(lines revision facts) (buffer-state b)])
                         (let ([text (checkpoint:text (hashtable-ref checkpoint-texts b #f) lines)])
                           (hashtable-set! checkpoint-texts b text)
                           (values (list 'local (buffer-name b) revision
                                         (list-sort (lambda (x y) (string<? (symbol->string (car x)) (symbol->string (car y)))) facts)
                                         text)
                                   positions)))])])
        (list reference (buffer-marked-raw b) positions))))

  ;; An idle checkpoint (a wake frame: foreign edits moved this head's
  ;; positions) goes at most once a second: resume projects the saved
  ;; positions across later edits anyway, and a foreign burst must not
  ;; publish this head's whole screen per keystroke. A changed state
  ;; inside the interval requests a frame at its end. The main loop's
  ;; own checkpoints queue at once. Explicit checkpoints fence all delivery
  ;; before detach, shutdown and resume; the writer never reads live UI state.
  (define checkpoint-queued-at #f)
  (define checkpoint-interval (make-time 'time-duration 0 1))

  (edoc "Capture this head's screen for resume. By default wait for acknowledgement; async queues without waiting, and idle also limits publication to once a second. Unchanged snapshots send nothing."
        (mode (one-of async idle) "optional background publication mode"))
  (define checkpoint!
    (case-lambda
      [()
       ;; Even a failing capture provider must not abandon a snapshot that
       ;; was already queued when the head performs its final checkpoint.
       (dynamic-wind void
         (lambda ()
           (for-each (lambda (hook) (hook #t)) (kernel:registry-items publication-hooks))
           (publish-checkpoint! #f))
         (lambda () (publication:flush! checkpoint-writer)))]
      [(mode)
       (unless (memq mode '(async idle)) (error 'checkpoint! "expected async or idle" mode))
       (for-each (lambda (hook) (hook #f)) (kernel:registry-items publication-hooks))
       (publish-checkpoint! (eq? mode 'idle))]))
  (define (publish-checkpoint! idle?)
    ;; No store reads here: every coordinate describes exactly the adopted
    ;; text/view the head just painted. Unchanged wake frames send nothing.
    (let* ([slots (map cons the-buffers (iota (length the-buffers)))]
           ;; the pop-up is not saved: every head has its own, hidden
           [layout
            (let capture ([node (layout-split-first the-root)])
              (if (window? node)
                  (list 'window (window-index node) (cdr (assq (window-buffer node) slots))
                    (window-topseg node) (window-left node) (window-wrap node) (window-line-numbers node) (window-document-views node))
                  (list 'split (layout-split-orientation node)
                    (layout-split-first-weight node) (layout-split-second-weight node)
                    (capture (layout-split-first node)) (capture (layout-split-second node)))))]
           [ordinary (layout-leaves (layout-split-first the-root))]
           [selected (cond [(memq the-current ordinary) the-current]
                       [(memq the-previous ordinary) the-previous] [else (car ordinary)])]
           ;; A pop-up has no retained layout slot. Preserve the preceding
           ;; ordinary selection, including after loss during a prompt.
           [state (list 'screen 6 (window-index selected) layout (map capture-buffer the-buffers))])
      (when (publication:changed? checkpoint-writer state)
        (let ([now (current-time 'time-monotonic)]
              [due (and checkpoint-queued-at (add-duration checkpoint-queued-at checkpoint-interval))])
          (if (or (not idle?) (not due) (time<=? due now))
              (when (publication:submit! checkpoint-writer state)
                (set! checkpoint-queued-at now))
              (request-frame-at! due))))))

  (define (project-resume-positions positions lines changes)
    (let ([deltas (if changes (map caddr changes) '())])
      (map (lambda (entry)
             (cons (car entry) (clamp-text-position lines (fold-left text:rebase-position (cdr entry) deltas))))
        positions)))

  (edoc "Adopt a resumed buffer at its saved revision and bring its positions forward: (values buffer positions)."
        (id integer "the store id")
        (basis integer "the saved revision")
        (positions list "the saved positions"))
  (define (resume-source! id basis positions)
    ;; Local projections and ordinary shared buffers use one source path.
    ;; Ask for the complete chain at the saved revision, adopt current truth,
    ;; then account for any reentrant adoption before returning coordinates.
    (let ([b (adopt-store-buffer! id)])
      (if (not b) (values #f positions)
          (let-values ([(lines revision changes) (store:snapshot-since id basis)])
            (let ([positions (project-resume-positions positions lines changes)])
              (adopt-snapshot! b basis lines revision changes '())
              ;; Keep the complete saved-view bridge in the common mirror.
              (when (and basis changes)
                (let-values ([(text now after) (snapshot-since b revision)])
                  (when after (text-source:adopt! (buffer-source b) basis text now (append changes after)))))
              (let-values ([(lines revision changes) (snapshot-since b revision)])
                (values b (project-resume-positions positions lines changes))))))))

  (define (restore-buffer entry)
    (apply
      (lambda (reference marked positions)
        (unless (boolean? marked)
          (error 'resume! "invalid buffer preferences"))
        (let-values ([(b positions)
                      (if (not reference) (values #f positions)
                          (guard (ex [else (values #f positions)])
                            (case (car reference)
                              [(shared) (apply resume-source! (append (cdr reference) (list positions)))]
                              [(tool)
                               (let ([b (find-tool-buffer (cadr reference))])
                                 (when b
                                   (buffer-name-set! b (caddr reference))
                                   (let ([app (app-of b)]) (when app ((app-refresh! app)))))
                                 (values b positions))]
                              [(local)
                               (apply
                                 (lambda (name revision facts text)
                                   (unless (and (string? name) (list? facts) (list? text) (for-all string? text))
                                     (error 'resume! "invalid local buffer checkpoint"))
                                   (let ([b (or (local-buffer-named name) (new-local-buffer! name))])
                                     (buffer-facts-set! b facts)
                                     (buffer-lines-set! b (list->vector text))
                                     (values (add-buffer! b) positions)))
                                 (cdr reference))]
                              [else
                               (let ([entry (resumer (car reference))])
                                 (if entry ((caddr entry) (cdr reference) positions) (values #f positions)))])))])
          (vector b (and b (content-revision b)) #f marked positions)))   ; slot 2 was the buffer's line numbers
      entry))

  (define (restore-screen! state)
    (apply
      (lambda (tag version selected layout entries)
        (unless (and (eq? tag 'screen) (memv version '(1 2 3 4 5 6)))
          (error 'resume! "unsupported screen checkpoint"))
        (let* ([fallback (window-buffer the-current)]
               ;; before version 3 a buffer entry carried its line numbers second
               [buffers (list->vector
                          (map (lambda (entry)
                                 (restore-buffer (if (and (< version 3) (list? entry) (>= (length entry) 4))
                                                     (cons (car entry) (cddr entry))
                                                     entry)))
                               entries))]
               [indices '()] [editor-placements '()]
               [natural? (lambda (n) (and (integer? n) (exact? n) (>= n 0)))]
               ;; Window 0 is the pop-up now. A screen saved before it
               ;; numbered an ordinary window 0: that window takes the
               ;; smallest number the saved screen leaves free.
               [saved-indices
                (let scan ([node layout] [acc '()])
                  (cond [(and (pair? node) (eq? (car node) 'window) (pair? (cdr node)))
                         (cons (cadr node) acc)]
                        [(and (pair? node) (eq? (car node) 'split) (= (length node) 6))
                         (scan (list-ref node 5) (scan (list-ref node 4) acc))]
                        [else acc]))]
               [renumbered (and (memv 0 saved-indices)
                                (let free ([n 1]) (if (memv n saved-indices) (free (+ n 1)) n)))]
               [remap (lambda (index) (if (and renumbered (eqv? index 0)) renumbered index))]
               [selected (remap selected)]
               [root
                (let restore ([node layout])
                  (case (car node)
                    [(window)
                     (apply
                       (lambda (tag index slot topseg left wrap numbers editors)
                         (unless (and (for-all natural? (list index slot topseg left))
                                      (< slot (vector-length buffers)) (not (memv index indices))
                                      (memq numbers '(default #t #f))
                                      (editor-references? editors)
                                      (let ([used (apply append (map (lambda (p) (map cdr (cdr p))) editor-placements))])
                                        (let loop ([entries editors] [seen used])
                                          (or (null? entries)
                                            (and (not (member (cdar entries) seen))
                                              (loop (cdr entries) (cons (cdar entries) seen)))))))
                           (error 'resume! "invalid window checkpoint"))
                         (set! indices (cons index indices))
                         (let ([w (%make-window (remap index) (or (vector-ref (vector-ref buffers slot) 0) fallback)
                                    0 topseg left 0 0 1 0 80 wrap numbers #f '() '())])
                           (set! editor-placements (cons (cons w editors) editor-placements)) w))
                       (if (= version 6) node
                         (let ([old (cond [(= version 1) (append node '(#f default ()))]
                                      [(= version 2) (append node '(default ()))]
                                      [(< version 5) (append node '(()))] [else node])])
                           (append (list-head old 6) (list-tail old 8)))))]
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
          ;; Resolve placements against the staged layout, not the screen
          ;; being replaced. Hosts may fork a multiply placed logical view.
          (fold-left (lambda (placed w)
                       (window-buffer-set! w (placed-buffer! w (window-buffer w) placed))
                       (cons w placed)) '() windows)
          ;; Validate and translate every placement before installing the tree.
          (vector-for-each
            (lambda (entry)
              (let ([b (vector-ref entry 0)])
                (when b
                  (let ([positions
                         ;; a place in a window the saved layout has no more, the
                         ;; pop-up's from an older checkpoint say, is dropped
                         (filter values
                           (map (lambda (entry)
                                  (let* ([place (car entry)] [top? (pair? place)]
                                         [index (if top? (cdr place) place)]
                                         [w (and (natural? index)
                                                 (find (lambda (w) (= (window-index w) (remap index))) windows))])
                                    (cond [(memq place '(mark spot spot-top)) entry]
                                          [(and w (eq? (window-buffer w) b)) (cons (if top? (cons 'top w) w) (cdr entry))]
                                          [else #f])))
                             (vector-ref entry 4)))])
                    (check-placements! b positions)
                    (let-values ([(lines revision changes) (snapshot-since b (vector-ref entry 1))])
                      (vector-set! entry 4 (project-resume-positions positions lines changes))))))) buffers)
          (set-layout-root! root)
          (set-current! current)
          (let ([restored (filter values (map (lambda (entry) (vector-ref entry 0)) (vector->list buffers)))])
            (set! the-buffers (append restored (filter (lambda (b) (not (memq b restored))) the-buffers))))
          (vector-for-each
            (lambda (entry)
              (let ([b (vector-ref entry 0)])
                (when b
                  (buffer-marked-set! b (vector-ref entry 3))
                  ;; apply-placements resets topseg for refits. Resume keeps
                  ;; the saved segment; the painter clamps it for this width.
                  (for-each (lambda (entry)
                              (let* ([w (placement-window (car entry))] [seg (and w (window-topseg w))])
                                (apply-placements! b (list entry))
                                (when seg (window-topseg-set! w seg)))) (vector-ref entry 4))
                  (clamp-buffer-positions! b)))) buffers)
          (for-each (lambda (entry)
                      (let ([w (car entry)])
                        (restore-window-document-views! w (cdr entry))
                        (ensure-window-document-view! w) (when window-mounter (window-mounter w)))) editor-placements)
          (request-repaint!)
          #t)) state))

  (edoc "Restore this head's windows and positions from its last publication, by names rather than stale coordinates.")
  (define (resume!)
    (publication:flush! checkpoint-writer)
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
      (and state
           (guard (ex [else (log:add! 'head:resume! (format "Screen checkpoint ignored: ~a" (kernel:condition-text ex))) #f])
             (call-with-display-update (lambda () (restore-screen! (without-copy-slot state))))))))


  ;;; Apps and views ------------------------------------------------------------

  ;; An app is a dynamic read-only buffer with a renderer and, optionally, an
  ;; event handler with first refusal on keys (what it declines goes through
  ;; the keymaps). A view is the degenerate app with no handler. Apps act on
  ;; the selected window -- their own, when it is selected.
  (edoc "A local app: a buffer rebuilt by a refresh procedure and fed events by a handler."
        (buffer buffer "the buffer the app owns")
        (refresh! thunk "rebuilds the buffer")
        (handle-event! (or procedure #f) "(handle-event! event) takes a key or pointer event, or #f")
        (refresh-error (or string #f) "the last failed refresh's text, or #f")
        (cursor-visible? (or boolean procedure symbol) "whether the cursor shows: a boolean, a procedure, or default"))
  (define-record-type app
    (fields buffer refresh! handle-event!
            (mutable refresh-error)
            (mutable cursor-visible?)))

  (define app-registry (kernel:make-registry))
  (define buffer-kill-hook-registry (kernel:make-registry))
  (define buffer-placement-hook-registry (kernel:make-registry))

  (edoc "Register an outer-host adapter that resolves a buffer before placement in a window."
        (proc procedure "(procedure window buffer peer-windows) returns the placed buffer; peers can be a staged resume layout"))
  (define (add-buffer-placement-hook! proc)
    (unless (procedure? proc) (error 'add-buffer-placement-hook! "expected a procedure"))
    (kernel:registry-add! buffer-placement-hook-registry proc))
  (define (placed-buffer! w b peers)
    (fold-left (lambda (b hook) (hook w b peers)) b (kernel:registry-items buffer-placement-hook-registry)))

  (define shutdown-hook-registry (kernel:make-registry))
  (define pre-redraw-hook-registry (kernel:make-registry))

  (edoc "Register a hook run with a buffer when this head forgets it."
        (proc procedure "(hook buffer)"))
  (define (add-buffer-kill-hook! proc)
    (unless (procedure? proc)
      (error 'add-buffer-kill-hook! "expected a procedure" proc))
    (kernel:registry-add! buffer-kill-hook-registry proc))

  (edoc "Register a hook run before every frame, after foreign edits are adopted."
        (proc thunk "the hook"))
  (define (add-pre-redraw-hook! proc)
    (unless (procedure? proc)
      (error 'add-pre-redraw-hook! "expected a procedure" proc))
    (kernel:registry-add! pre-redraw-hook-registry proc))

  (edoc "Adopt the store's news and refresh the renditions and views before a frame paints.")
  (define (before-frame!)
    ;; Adopt the store's news before the layers above refresh their
    ;; views.  A frame never writes an old shared cache back to the store.
    ;; The painter supplies current terminal geometry before this fence.
    (set! frame-deadline #f)
    (sync-foreign-edits!)
    (refresh-renditions!)
    (for-each (lambda (w) (ensure-window-document-view! w) (when window-mounter (window-mounter w))) the-windows)
    (flush-ui-audit! 'stale)
    (publish-head-marks!)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
              (kernel:registry-items pre-redraw-hook-registry)))

  (edoc "Register a hook run when the head shuts down."
        (proc thunk "the hook"))
  (define (add-shutdown-hook! proc)
    (unless (procedure? proc)
      (error 'add-shutdown-hook! "expected a procedure" proc))
    (kernel:registry-add! shutdown-hook-registry proc))

  (edoc "Run the shutdown hooks, ignoring their errors.")
  (define (run-shutdown-hooks!)
    (for-each (lambda (hook) (guard (ex [else (void)]) (hook)))
              (kernel:registry-items shutdown-hook-registry)))

  (edoc "The registered local apps."
        (returns (list-of (record app))))
  (define (registered-apps)
    (kernel:registry-items app-registry))

  (edoc "The local app registered on a buffer, or #f."
        (b buffer "the buffer")
        (returns (or (record app) #f)))
  (define (app-of b)
    (find (lambda (a) (eq? (app-buffer a) b)) (registered-apps)))

  (define (app-fact facts key fallback)
    (let ([entry (and facts (assq key facts))]) (if entry (cdr entry) fallback)))

  (edoc "One owned batch of a shared app buffer's facts, audience and endpoint identity included, or #f."
        (b buffer "the buffer")
        (returns (or list #f)))
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

  (edoc "Whether a buffer belongs to a local app or a live shared one."
        (b buffer "the buffer")
        (returns boolean))
  (define (app-buffer? b)
    (or (and (app-of b) #t) (app-live? (app-facts b))))

  (edoc "Whether text in a buffer can be selected: any ordinary buffer, and an app that allows it."
        (b buffer "the buffer")
        (returns boolean))
  (define (buffer-selectable? b)
    (or (not (app-of b)) (buffer-fact b 'selectable #t)))

  (edoc "Activate or deactivate a buffer's mark; a non-selectable app's stays off."
        (b buffer "the buffer")
        (marked? boolean "whether the mark is active"))
  (define (buffer-marked-set! b marked?)
    (let ([w (buffer-editor-window b)] [value (and marked? (buffer-selectable? b))])
      (if w (update-window-editor! w 3 value) (buffer-marked-raw-set! b value))))

  ;; Mouse context is head-owned; edit reexports these same parameters.
  ;; Position is a one-based viewport cell pair, buffer position is an
  ;; unclamped character pair, and button is the raw xterm code.
  (edoc "The viewport cell a pointer event hit, one-based (column . row), while an app handler runs."
        (value (or pair #f)))
  (define app-event-position (make-thread-parameter #f))

  (edoc "The unclamped character position, (row . col), a pointer event hit, while an app handler runs."
        (value (or pair #f)))
  (define app-event-buffer-position (make-thread-parameter #f))

  (edoc "The raw xterm button code of the pointer event an app handler is running for."
        (value (or integer #f)))
  (define app-event-button (make-thread-parameter #f))

  ;; The window with keyboard focus when a pointer event began.  The app's
  ;; own window is selected while its handler runs; a control panel that
  ;; acts on the focused window addresses this one instead.
  (edoc "The window with keyboard focus when a pointer event began, while an app handler runs in the app's own window."
        (value (or window #f)))
  (define app-event-focus (make-thread-parameter #f))

  (edoc "Whether a window's terminal view follows its live process."
        (w window "the window")
        (returns boolean))
  (define (app-following? w)
    (and (let* ([id (window-document-view w)] [d (and id (interaction:snapshot id))])
           (and d (eq? (view:kind d) 'terminal) (cadr (terminal-state:state d))))
         (app-live? (app-facts (window-buffer w)))))

  (edoc "Deliver an event to the current legacy local app, preserving its focus decision. Widget hosts use recursive routing."
        (event string "normalized event") (returns any))
  (define (dispatch-app-event! event)
    (let ([app (app-of (current-buffer))])
      (and app (cond [(app-handle-event! app) => (lambda (handler) (handler event))] [else #f]))))

  (edoc "Register the default window host's local presentation bridge. Extensions define widgets instead; their models, input and layout are independent of this outer buffer."
        (b buffer "existing local host buffer") (refresh! thunk "prepare its widget frame")
        (handler procedure "focus/blur callback") (returns buffer))
  (define (register-widget-host! b refresh! handler)
    (unless (and (buffer? b) (not (buffer-store-id b)) (procedure? refresh!) (procedure? handler))
      (error 'register-widget-host! "expected a local buffer and host callbacks"))
    (buffer-read-only-set! b #t)
    (buffer-fact-set! b 'app #t)
    (buffer-fact-set! b 'disposable #t)
    (add-buffer! b)
    (kernel:registry-remove! app-registry (lambda (a) (eq? (app-buffer a) b)))
    (kernel:registry-add! app-registry (make-app b refresh! handler #f 'default))
    b)

  (edoc "Say whether an app buffer shows the cursor: a boolean, or a procedure deciding per frame."
        (b buffer "the app buffer")
        (visible? (or boolean procedure) "the visibility")
        (returns buffer))
  (define (set-app-cursor-visible! b visible?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-cursor-visible! "not an app buffer" b))
      (unless (or (boolean? visible?) (procedure? visible?))
        (error 'set-app-cursor-visible!
               "visibility must be a boolean or procedure" visible?))
      (app-cursor-visible?-set! a visible?)
      b))

  (edoc "Say whether an app manages its windows' viewports itself."
        (b buffer "the app buffer")
        (manages? boolean "whether it does")
        (returns buffer))
  (define (set-app-manages-viewport! b manages?)
    (let ([a (app-of b)])
      (unless a (error 'set-app-manages-viewport! "not an app buffer" b))
      (unless (boolean? manages?)
        (error 'set-app-manages-viewport! "manages must be #t or #f" manages?))
      (buffer-fact-set! b 'manages-viewport manages?)
      b))

  (edoc "Say whether text in an app buffer can be selected."
        (b buffer "the app buffer")
        (selectable? boolean "whether it can")
        (returns buffer))
  (define (set-app-selectable! b selectable?)
    (unless (and (app-of b) (boolean? selectable?))
      (error 'set-app-selectable! "expected an app buffer and boolean" b selectable?))
    (buffer-fact-set! b 'selectable selectable?)
    (unless selectable? (buffer-marked-raw-set! b #f))
    b)

  ;; A buffer's own status text, in place of the generated name, coordinates
  ;; and mode tag: a provider kept beside the buffer rather than in a fact,
  ;; since a checkpoint saves facts as data
  (define buffer-statuses (make-weak-eq-hashtable))

  (edoc "Give a buffer its own status text after its name, which every status line shows, in place of the generated state marker, coordinates and mode tag: a procedure of the buffer, or of the buffer and the window painted, giving a string, the empty one for the name alone, a zero-based (row . column) to project as the position, or #f for the generated details; #f takes the provider away."
        (b buffer "the buffer")
        (status (or procedure #f) "the provider, or #f"))
  (define (set-buffer-status! b status)
    (unless (or (not status) (procedure? status))
      (error 'set-buffer-status! "status must be #f or a procedure" status))
    (if status (hashtable-set! buffer-statuses b status) (hashtable-delete! buffer-statuses b))
    b)

  (edoc "A buffer's own status text for a window painted, its provider's answer: a string, a (row . column), or #f; #f without a provider."
        (b buffer "the buffer")
        (w window "the window showing it")
        (returns any))
  (define (buffer-status b w)
    (let ([status (hashtable-ref buffer-statuses b #f)])
      (and status
           (if (logbit? 2 (procedure-arity-mask status)) (status b w) (status b)))))

  (edoc "Say how an app buffer's status line reads after its name: a procedure giving the text, the empty string for the name alone, or #f for the buffer coordinates; set-buffer-status! for an app buffer, which it must be."
        (b buffer "the app buffer")
        (position (or procedure #f) "the position source"))
  (define (set-app-status-position! b position)
    (unless (app-of b) (error 'set-app-status-position! "not an app buffer" b))
    (set-buffer-status! b position))

  (edoc "Whether a legacy local app permits the window cursor. Mounted widgets supply their own caret."
        (w window "the window")
        (returns boolean))
  (define (app-cursor-visible-in? w)
    (let* ([a (app-of (window-buffer w))]
           [visibility (and a (app-cursor-visible? a))])
      (cond [(not a) #t]
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

  (edoc "Whether the app shown in a window manages the viewport itself."
        (w window "the window")
        (returns boolean))
  (define (app-manages-window-viewport? w)
    (let ([b (window-buffer w)])
      (and (or (app-of b) (app-following? w)) (buffer-fact b 'manages-viewport #f))))

  (edoc "The cursor shape a local app asks for, or the followed shared app's, or #f."
        (b buffer "the buffer")
        (returns any))
  (define (app-cursor-style b)
    ;; Local presentation or the followed shared app's shape, otherwise #f.
    (if (app-of b) (buffer-fact b 'cursor-style #f)
        (and (eq? b (window-buffer the-current)) (app-following? the-current)
             (app-fact (app-facts b) 'cursor-style #f))))

  (edoc "Configure a local app's presentation in every window: the sticky rows above the body, its scrollbar, #f, #t, left, right or auto, then optionally its wrap and cursor style."
        (b buffer "the app buffer")
        (sticky-lines integer "rows kept above the scrollable body")
        (scrollbar (or boolean (one-of left right auto)) "the scrollbar")
        (options (list-of any) "a wrap setting, then a cursor style"))
  (define (set-app-presentation! b sticky-lines scrollbar . options)
    ;; Configure presentation shared by every window showing this local
    ;; app.  Sticky rows stay above the scrollable body; scrollbar is
    ;; #f, #t (enabled using the configured side), left, right, or auto
    ;; (the configured side, only while the content overflows the window).
    (let ([a (app-of b)])
      (unless a (error 'set-app-presentation! "not an app buffer" b))
      (unless (and (integer? sticky-lines) (exact? sticky-lines)
                   (>= sticky-lines 0))
        (error 'set-app-presentation! "sticky line count must be nonnegative"
               sticky-lines))
      (unless (memq scrollbar '(#f #t left right auto))
        (error 'set-app-presentation!
               "scrollbar must be #f, #t, left, right, or auto" scrollbar))
      (let ([wrap (if (pair? options) (car options) 'default)]
            [cursor-style (if (and (pair? options) (pair? (cdr options)))
                              (cadr options) 'default)])
        (unless (memq wrap '(default #t #f))
          (error 'set-app-presentation!
                 "wrap must be default, #t, or #f" wrap))
        ;; text is the editor's own shape for editable text, for an app
        ;; whose rows are typed into although the buffer is read-only
        (unless (memq cursor-style
                      '(default text block underline bar
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

  (edoc "How many rows of an app buffer stay above the scrollable body."
        (b buffer "the buffer")
        (returns integer))
  (define (buffer-sticky-lines b)
    (if (app-buffer? b)
        (min (or (buffer-fact b 'sticky-lines #f) 0) (line-count b))
        0))

  ;; Ordinary buffers use the global setting. An app can force the bar on
  ;; with #t, force a particular side, or otherwise inherit the global choice.
  (edoc "Whether ordinary buffers show a scrollbar."
        (value boolean))
  (define scrollbar (make-parameter #f
                      (lambda (visible?)
                        (unless (boolean? visible?)
                          (error 'scrollbar "must be #t or #f" visible?))
                        visible?)))

  (edoc "Which side a scrollbar shows on, left or right."
        (value (one-of left right)))
  (define scrollbar-position (make-parameter 'right
                               (lambda (side)
                                 (unless (memq side '(left right))
                                   (error 'scrollbar-position "must be left or right" side))
                                 side)))

  (edoc "Whether windows show line numbers beside an edit buffer by default."
        (value boolean))
  (define line-numbers (make-parameter #f
                         (lambda (visible?)
                           (unless (boolean? visible?)
                             (error 'line-numbers "must be #t or #f" visible?))
                           visible?)))

  (edoc "Whether a window shows line numbers now: never beside an app's buffer, else its own setting, else the default."
        (w window "the window")
        (returns boolean))
  (define (window-line-numbers? w)
    (and (not (app-buffer? (window-buffer w)))
         (let ([setting (window-line-numbers w)])
           (if (eq? setting 'default) (line-numbers) setting))))

  (edoc "The columns a window's line numbers take, 0 without them."
        (w window "the window")
        (returns integer))
  (define (window-line-number-width w)
    (if (window-line-numbers? w)
        (+ 1 (string-length
               (number->string (line-count (window-buffer w)))))
        0))

  ;; An auto scrollbar shows only while the window's content overflows it.
  ;; The painter judges that per window when it prepares a frame and records
  ;; the verdict here, so geometry and hit-testing agree with the last frame.
  (define auto-scrollbars (make-weak-eq-hashtable))

  (edoc "Record whether a window's auto scrollbar shows now, as its content overflows."
        (w window "the window")
        (shown? boolean "whether it shows"))
  (define (window-auto-scrollbar-set! w shown?)
    (hashtable-set! auto-scrollbars w (and shown? #t)))

  (edoc "The side a window's scrollbar shows on, left or right, or #f."
        (w window "the window")
        (returns (or (one-of left right) #f)))
  (define (window-scrollbar? w)
    (let ([choice (buffer-fact (window-buffer w) 'scrollbar #f)])
      (cond [(memq choice '(left right)) choice]
            [(eq? choice 'auto)
             (and (hashtable-ref auto-scrollbars w #f) (scrollbar-position))]
            [(or choice (scrollbar)) (scrollbar-position)]
            [else #f])))

  (edoc "The columns of a window left for text, after its scrollbar and line numbers."
        (w window "the window")
        (returns integer))
  (define (window-content-width w)
    (max 1 (- (window-width w)
              (if (window-scrollbar? w) 1 0)
              (window-line-number-width w))))

  (edoc "The text grid, (rows . columns), of the preferred window showing a buffer, the focused one first, or #f."
        (b buffer "the buffer")
        (returns (or pair #f)))
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

  (edoc "The screen column of a window's scrollbar, or #f."
        (w window "the window")
        (returns (or integer #f)))
  (define (window-scrollbar-column w)
    (case (window-scrollbar? w)
      [(left) (window-xoff w)]
      [(right) (+ (window-xoff w) (window-width w) -1)]
      [else #f]))

  (edoc "Rebuild every registered app shown in a window, reporting a failed refresh in its buffer.")
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
                                  (log:add! 'head:refresh-visible-views! text)))])
                    ((app-refresh! a))
                    (app-refresh-error-set! a #f))))
              (filter (lambda (a) (memq (app-buffer a) the-buffers))
                      (registered-apps))))

  (edoc "Install a prepared widget frame in its default outer window. Logical text, selection and scrolling remain in the widget; this buffer holds only the current frame."
        (b buffer "registered host") (w window "displaying window") (lines list "prepared display rows"))
  (define (replace-widget-frame! b w lines)
    (unless (and (app-of b) (not (buffer-store-id b)) (eq? b (window-buffer w)))
      (error 'replace-widget-frame! "expected the widget's window placement"))
    (let ([new (text:normalize lines)])
      (call-with-display-update
        (lambda ()
          (let ([changed? (not (equal? (buffer-text b) new))])
            (when changed? (adopt-local! b new #f))
            (apply-placements! b (list (cons w '(0 . 0)) (cons (cons 'top w) '(0 . 0))))
            (clamp-buffer-positions! b)
            (when changed? (request-repaint!)))))))

  (edoc "The buffer with a name, or #f."
        (name string "the name")
        (returns (or buffer #f)))
  (define (buffer-named name)
    (find (lambda (b) (string=? (buffer-name b) name)) the-buffers))

  (edoc "Point in a buffer: its selected or first window's, else its saved spot; (row . col)."
        (b buffer "the buffer")
        (returns position))
  (define (buffer-point b)
    ;; Reading a position must not switch a window or invoke callbacks.
    (let ([w (if (eq? (window-buffer the-current) b) the-current
                 (find (lambda (w) (eq? (window-buffer w) b)) the-windows))])
      (if w (cons (window-prow w) (window-pcol w))
          (cons (buffer-spot-row b) (buffer-spot-col b)))))

  (edoc "Show a buffer in a window, saving point in the old buffer and restoring where it last was in the new one."
        (w window "the window")
        (b buffer "the buffer"))
  (define (set-window-buffer! w b)
    ;; Display b in w, remembering where point was in the old buffer and
    ;; restoring where it last was in the new one. Redisplaying the same
    ;; buffer preserves the live window; saved spots belong to hidden ones.
    (set! b (placed-buffer! w b the-windows))
    (ensure-buffer-visible! b)
    (let ([old (window-buffer w)])
      (unless (eq? old b)
        (window-status-actions-set! w '())
        (unless (window-editor w)
          (buffer-spot-row-set! old (window-prow w))
          (buffer-spot-col-set! old (window-pcol w))
          (buffer-spot-top-set! old (window-top w)))
        (let* ([id (window-document-view w)] [d (and id (interaction:snapshot id))])
          (when d (interaction:release! ui-actor id (view:generation d))))
        (window-buffer-set! w b)
        (window-prow-raw-set! w (buffer-spot-row b))
        (window-pcol-raw-set! w (buffer-spot-col b))
        (window-top-raw-set! w (buffer-spot-top b))
        (ensure-window-document-view! w)
        (unless (window-editor w)
          (window-prow-set! w (buffer-spot-row b))
          (window-pcol-set! w (buffer-spot-col b))
          (window-top-set! w (buffer-spot-top b)))
        (when (eq? w the-popup)
          ;; a buffer sent to the pop-up, by a link say, shows it at its
          ;; default size; its own placeholder hides it again
          (cond [(eq? b popup-buffer) (set! the-popup-rows 0) (popup-hidden!)]
                [(= the-popup-rows 0) (set! the-popup-rows (popup-default-rows))])
          (request-repaint!))
        (window-topseg-set! w 0)
        (window-left-set! w 0)
        (clamp-buffer-positions! b))
      (refresh-buffer-rendition! b)
      (when window-mounter (window-mounter w))
      ;; Identity, geometry, and rendition agree before a callback can switch.
      (unless (eq? old b) (request-repaint!))))

  (edoc "Show a buffer in the current window and put it first in the recency list; a buffer whose recency fact is behind goes last instead."
        (b buffer "the buffer to show"))
  (define (show-buffer! b)
    (let ([b (edoc:type-value 'buffer b)])
      (add-buffer! b)
      ;; A picker is inventory, not a document visit: it stays behind the
      ;; documents in the recency list even when its own row is opened.
      (let ([rest (remq b the-buffers)])
        (set! the-buffers (if (eq? (buffer-fact b 'recency #f) 'behind) (append rest (list b)) (cons b rest))))
      (set-window-buffer! the-current b)))

  ;;; Scopes: another window or buffer current for the extent of a body

  ;; One form each, no procedure beside it: M-x offers one spelling. The
  ;; procedures below are the forms' bodies.

  (define (call-with-window w thunk)
    ;; w temporarily selected, without telling the apps; the selection
    ;; returns on exit and on escape
    (unless (memq w the-windows) (error 'with-window "not a live window" w))
    (let ([prev the-current])
      (dynamic-wind
        (lambda () (set! the-current w))
        thunk
        (lambda () (set! the-current prev)))))

  (edoc "Run body with a window temporarily selected, without telling the apps; the selection returns on exit and on escape: (with-window (window 2) (window:split-right!))."
        (w window "the window to select")
        (body (list-of any) "the forms to run"))
  (define-syntax with-window
    (syntax-rules ()
      [(_ w body ...) (call-with-window (edoc:type-value 'window w) (lambda () body ...))]))

  (define (call-with-buffer b thunk)
    ;; b temporarily current: in the window already showing it when there
    ;; is one -- point moves where the user sees it -- else invisibly in
    ;; the current window with the usual spot saving; the recency order is
    ;; untouched and no app hears a focus change
    (cond
      [(eq? b (window-buffer the-current)) (thunk)]
      [(find (lambda (w) (eq? (window-buffer w) b)) the-windows)
       => (lambda (w) (call-with-window w thunk))]
      [else
       (let ([old (window-buffer the-current)])
         (dynamic-wind
           (lambda () (set-window-buffer! the-current b))
           thunk
           (lambda () (set-window-buffer! the-current old))))]))

  (edoc "Run body with a buffer temporarily current: in the window already showing it, else invisibly in the current window; the recency order is untouched and no app hears a focus change: (with-buffer (buffer \"notes.md\") (search:replace! \"x\" \"y\"))."
        (b buffer "the buffer to make current")
        (body (list-of any) "the forms to run"))
  (define-syntax with-buffer
    (syntax-rules ()
      [(_ b body ...) (call-with-buffer (edoc:type-value 'buffer b) (lambda () body ...))]))

  (edoc "Retire this head's record of a buffer, moving windows off it and running the kill hooks; the store content stays."
        (b buffer "the buffer"))
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
          (let ([fallback (or (find (lambda (b) (and (buffer-visible? b) (not (hashtable-ref (buffer-local-facts b) 'internal #f)))) the-buffers)
                            (new-buffer! "*scratch*"))])
            (for-each (lambda (w)
                        (when (eq? (window-buffer w) b)
                          (set-window-buffer! w fallback)))
                      the-windows))
          (for-each
            (lambda (hook)
              (guard (ex [else
                          (log:add! 'head:forget-buffer!
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
  (edoc "A computation was interrupted by C-g.")
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

  (edoc "Run an interaction during which C-g is an ordinary key rather than an interrupt."
        (thunk thunk "the interaction")
        (returns any "what the thunk returns")
        (effects internal))
  (define (call-uninterrupted thunk)
    ;; run thunk as an interaction: C-g is a key while it lasts
    (let ([old isig-on?])
      (dynamic-wind
        (lambda () (set-isig! #f))
        thunk
        (lambda () (set-isig! old)))))

  (edoc "Run a computation that C-g interrupts, raising an interrupted condition."
        (thunk thunk "the computation")
        (returns any "what the thunk returns"))
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

  ;; Under E_TEST_EVAL another actor may drive this head through mail: an
  ;; (evaluate token text) payload evaluates text at the top level on the main
  ;; thread -- the environment M-x sees -- and answers the sender with
  ;; (evaluated token printed) or (evaluated token error text). Like a key,
  ;; the evaluation is followed by a frame and a checkpoint, so what it showed
  ;; is what a resumed screen restores. Tests use it instead of typing into
  ;; the prompt; it is off unless the variable is set.
  (define evaluation-mail? (and (getenv "E_TEST_EVAL") #t))
  (define (deliver-evaluation-mail! message)
    (when (and evaluation-mail? (list? message) (= (length message) 3) (eq? (car message) 'message))
      (let ([from (cadr message)] [payload (caddr message)])
        (when (and (list? payload) (= (length payload) 3) (eq? (car payload) 'evaluate)
                   (string? (caddr payload)))
          (run-on-main!
            (lambda ()
              (suspension:call! ui-actor
                (lambda () (run-on-main! (lambda () (suspension:drain!
                                                      (lambda (ex) (log:add! 'head:deliver-evaluation-mail! (kernel:condition-text ex)))))))
                (lambda ()
                  (let ([reply (guard (ex [else (list 'evaluated (cadr payload) 'error (kernel:condition-text ex))])
                                 (let ([value (kernel:evaluate! (read (open-input-string (caddr payload))) (interaction-environment))])
                                   (list 'evaluated (cadr payload) (format "~s" value))))])
                    (guard (ex [else (void)]) (frame!) (checkpoint!))
                    (actor:send! from reply))))))))))

  ;; another actor's message to this head wakes its loop; the question
  ;; is presented before the next frame
  (edoc "This head's actor identity in the store and the interaction protocol."
        (value head))
  (define ui-actor ;; Claim and subscribe together before creating buffers or marks. Both
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
                                    (lambda (message) (deliver-evaluation-mail! message) (wake-main!))
                                    'all)])
                    (let-values ([(token take!) (store:watch! wake-main!)])
                      (set! take-store-changes! take!))
                    ;; Surface events are wakeups. The head prepares current
                    ;; demanded rows on its pump, never on a publisher thread.
                    (surface:subscribe! #f (lambda (event) (wake-main!)))
                    identity)))))))))

  ;;; The seat's first state ---------------------------------------------------------

  (define interaction-started
    (begin
      (interaction:start! ui-actor wake-main!)
      (kernel:call-with-runtime-registrations
        (lambda () (add-publication-hook! (lambda (fence?) (if fence? (interaction:flush!) (interaction:publish!))))))))

  ;; Subscribe before taking the initial inventory: writes during the read
  ;; are in the inventory, the queue, or both. Deduplicate at first adoption.
  (define initial-store-ids (list-sort < (store:buffer-list)))

  ;; the seat begins as *scratch* in one window; views the modules above
  ;; register while loading join the list behind it
  (define seat-initialized
    (let ([b (or (let ([id (store:find-named "*scratch*")])
                   (and id (adopt-store-buffer! id)))
                 (new-buffer! "*scratch*"))])
      (set! the-buffers (cons b (remq b the-buffers)))
      ;; the pop-up is born first, so it is window 0; *scratch* is window 1
      (set! popup-buffer (new-local-buffer! "pop-up"))
      (set! the-popup (make-window popup-buffer 0 0 0 0 0 0 0 0 'default))
      (set! the-windows (list the-popup))
      (let ([w (make-window b 0 0 0 0 0 0 0 0 'default)])
        (set! the-current w)
        (set-layout-root! w))))
)
