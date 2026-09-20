;; edit.sls -- the command layer: the library (edit), the e editor's
;; default app.
;;
;; Everything a user does to text and to the seat that shows it: the
;; buffer commands (the window commands are (window)'s), visiting,
;; saving, merging with the disk,
;; editing with undo, the kill ring and the clipboard, indentation and
;; formatting through the modes' registered indenters, mouse actions,
;; the default key bindings, and the generic editing helpers (regions,
;; conflict resolution; search and replace are (search)'s).  It
;; composes the seams below --
;; store, head, paint, prompt, file, mode, keymap -- and is what M-x
;; sees bare: the loader imports (edit) into the top level.
;;
;; Hot-reloadable like any module: its registrations (bindings, hooks,
;; formatters, descriptions) are made in init!, owned by edit, so a
;; reload retracts and remakes them; the loop in (main) reaches the
;; layer through hooks it installs in (head).  Internals -- all
;; mutable state included -- are invisible outside the library; the
;; exports are the editor's public command API.
;;
;; The generic helpers act on the selected region, else on the whole
;; current buffer -- current-region; with-region and head:with-buffer
;; retarget them for the extent of a body.  A region is a slice of one
;; buffer between two (row . col) points, and prints as the expression
;; that rebuilds it, like buffers do:  (region (buffer "e") '(0 . 0) '(12 . 5)).

(import (only (edoc) elibrary))
(elibrary (edit)
  (export init!
          current-region region-text with-region
          next-conflict! keep-mine! keep-disk!
    ;; state, read-only
    buffer-text buffer-clean?

    ;; buffers, windows, files
    visit-file! save-file! save! prompt-file!
    kill-buffer!
    new-buffer! trash restore! empty-trash!
    ;; editing and movement
    insert-text! replace-region-text! rewrite-region! newline! delete-forward! backspace!
    kill-line! kill-region! copy-region! yank! undo! redo! undo-scope undo-actor!
    copy-to-kill-buffer! current-kill-ring
    forward-kill-ring-to-system-clipboard
    set-mark-command! beginning-of-line! end-of-line! keyboard-quit!
    redraw-command! open-line! page-up! page-down!
    page-window-fraction! set-point-without-scroll!

    previous-line! next-line! beginning-of-buffer! end-of-buffer!
    move-left! move-right! indent-tab!
    call-as-one-edit!
    indent-line! indent-region! indent-buffer! format-region! format-buffer!
    move-horizontal! move-vertical!
    quit!
    ;; extending the editor

    describe-key!

    register-indenter! register-formatter!

    indent-on-tab!



    set-message!
    mouse!
    answer!
    present-log-entry! present-log-entries!


    message-source message-progress



    app-event-position app-event-buffer-position app-event-button
  )
  (import (chezscheme)
          (literal)
          (prefix (sys) sys:)
          (prefix (store) store:)
          (prefix (text) text:)
          (prefix (datum) datum:)
          (prefix (property) property:)
          (prefix (kernel) kernel:)
          (prefix (actor) actor:)
          (prefix (log) log:)
          (prefix (style) style:)
          (prefix (keymap) keymap:)
          (prefix (tty) tty:)
          (prefix (echo) echo:)
          (prefix (head) head:)
          (prefix (window) window:)
          (prefix (dispatch) dispatch:)
          (prefix (paint) paint:)
          (prefix (string) string:)
          (prefix (render) render:)
          (prefix (glyph) glyph:)
          (prefix (table) table:)
          (prefix (mode) mode:)
          (prefix (file) file:)
          (prefix (prompt) prompt:)
          (prefix (doc) doc:))

  ;;; Buffers and windows ----------------------------------------------------

  ;; The seat's buffer record lives in (head) -- the client-side cache
  ;; of a store buffer plus per-seat presentation; the commands reach
  ;; the seat's lists and selection through the identifier-syntax
  ;; facades below (a facade sweep is on the tech-debt ledger).
  (define-syntax buffers
    (identifier-syntax [id (head:buffers)]
      [(set! id v) (head:set-buffers! v)]))

  ;; Buffer facts and the store client -- the bridge between this
  ;; seat's records and the (store) -- live in (head) now; the
  ;; mode registry in (mode).
  (define layout-split-first-weight-set!
    head:layout-split-first-weight-set!)
  (define layout-split-second-weight-set!
    head:layout-split-second-weight-set!)
  (define-syntax windows
    (identifier-syntax [id (head:windows)]
      [(set! id v) (head:set-windows! v)]))
  (define-syntax layout-root
    (identifier-syntax [id (head:root)]
      [(set! id v) (head:set-root! v)]))
  (define-syntax current-window
    (identifier-syntax [id (head:current-window)]
      [(set! id v) (head:set-current! v)]))
  ;; The (store) is the master copy of every buffer's text; this
  ;; seat is one of its clients.  A buffer record's lines field is a
  ;; cache of the store's immutable text vector, adopted after every
  ;; operation -- nothing here mutates a line vector in place.  Edits
  ;; enter the store transactionally, explicit baseline replacements
  ;; are resets, and foreign actors' edits flow back before each frame
  ;; (head:before-frame!).  A refusal or failure stops the command;
  ;; the head never overwrites shared text to force an edit through.


  (define-syntax define-state
    (syntax-rules ()
      [(_ name place get put)
       (define-syntax name
         (identifier-syntax
           [id (get place)]
           [(set! id v) (put place v)]))]))

  (define-state lines (head:window-buffer current-window)
    head:buffer-lines head:buffer-lines-set!)
  (define-state file-name (head:window-buffer current-window)
    head:buffer-file head:buffer-file-set!)
  (define-state trailing-newline? (head:window-buffer current-window)
    head:buffer-trailing head:buffer-trailing-set!)
  (define-state modified? (head:window-buffer current-window)
    head:buffer-modified head:buffer-modified-set!)
  (define-state history (head:window-buffer current-window)
    head:buffer-history head:buffer-history-set!)
  (define-state mark-row (head:window-buffer current-window)
    head:buffer-mark-row head:buffer-mark-row-set!)
  (define-state mark-col (head:window-buffer current-window)
    head:buffer-mark-col head:buffer-mark-col-set!)
  (define-state mark-active? (head:window-buffer current-window)
    head:buffer-marked head:buffer-marked-set!)
  (define-state point-row current-window head:window-prow head:window-prow-set!)
  (define-state point-col current-window head:window-pcol head:window-pcol-set!)
  (define-state top-row current-window head:window-top head:window-top-set!)
  (define-state left-col current-window head:window-left head:window-left-set!)

  ;;; Editor state ------------------------------------------------------------


  ;; The echo area's model lives in (echo) and its painting in (paint);
  ;; message and echo-pending are identifier-syntax facades for the
  ;; sites here that still write them.
  (define-syntax rows
    (identifier-syntax [id (paint:screen-rows)]
      [(set! id v) (paint:set-screen-rows! v)]))
  (define-syntax cols
    (identifier-syntax [id (paint:screen-cols)]
      [(set! id v) (paint:set-screen-cols! v)]))
  (define-syntax message
    (identifier-syntax [id (echo:text)] [(set! id v) (echo:set-text! v)]))
  (define-syntax echo-pending
    (identifier-syntax [id (echo:pending)] [(set! id v) (echo:set-pending! v)]))
  (define-syntax kill-ring
    (identifier-syntax [id (head:kill-ring)] [(set! id v) (head:set-kill-ring! v)]))
  (define suppress-history (make-parameter #f))
  ;; Desired anchors in the command's proposed result.  The head projects
  ;; them into the accepted revision before adopting any later changes.
  (define edit-point (make-parameter 'end))
  (define edit-mark (make-parameter #f))
  (define edit-source (make-parameter #f))
  (define (edit-basis-for b) (or (edit-source) (head:edit-basis b)))

  ;;; Small utilities -------------------------------------------------------

  (define (insert-before lst x y)
    ;; A copy of lst with y inserted right before x (or at the end).
    (cond [(null? lst) (list y)]
          [(eq? (car lst) x) (cons y lst)]
          [else (cons (car lst) (insert-before (cdr lst) x y))]))

  (define (insert-after lst x y)
    ;; A copy of lst with y inserted right after x (or at the end).
    (cond [(null? lst) (list y)]
          [(eq? (car lst) x) (cons x (cons y (cdr lst)))]
          [else (cons (car lst) (insert-after (cdr lst) x y))]))

  ;;; Buffer access and undo ------------------------------------------------

  (define (vlen) (vector-length lines))
  (define (line-at n) (vector-ref lines n))
  (define (current-line) (line-at point-row))
  ;; Navigation addresses the window presentation; editing addresses source.
  (define (current-display-line) (vector-ref (head:window-lines current-window) point-row))

  (define (snapshot-key snapshot)
    (and (> (length snapshot) 6) (list-ref snapshot 6)))

  (define (submit-edit! b span replacement . properties)
    ;; Head snapshots supply grouping and presentation only.  Shared
    ;; undo is the store's inverse journal, never these saved vectors.
    ;; The entry is staged: do not clear redo or add it to the group's
    ;; history until a mutation actually succeeds.
    (let* ([action (pending-edit)]
           [entry (and action (cadr action))]
           [key (and entry (snapshot-key (cdr entry)))])
      (unless (and action (eq? (car action) b))
        (error 'submit-edit! "edit has no pending action"))
      (head:store-edit! b span replacement
                        (append (list key (caddr action)) properties)
                        (cons (cons current-window (edit-point))
                              (if (edit-mark) (list (cons 'mark (edit-mark))) '()))
                        (edit-basis-for b))
      ((cadddr action))))

  (define (replace-buffer-lines! b target . properties)
    ;; Formatting, indentation, and merging are ordinary attributed
    ;; edits.  Only loading/rereading a baseline may reset the store.
    (let-values ([(span replacement) (text:difference (car (edit-basis-for b)) target)])
      (apply submit-edit! b span replacement properties)))

  (define (editor-snapshot . key)
    ;; the cache vectors are immutable now: snapshots share, never copy
    (list lines point-row point-col trailing-newline? modified?
          (head:buffer-store-rev (head:window-buffer current-window))
          (if (pair? key) (car key)
              (list 'head-edit head:ui-actor
                    (head:buffer-store-rev (head:window-buffer current-window))))))

  (define (restore-snapshot! snapshot)
    ;; This is the local-buffer path.  Shared text never restores a
    ;; snapshot; the store's inverse operation owns its history.
    (let-values ([(span replacement) (text:difference lines (car snapshot))])
      (head:store-edit! (head:window-buffer current-window) span replacement
                        (list #f #f (list (cons 'trailing (cadddr snapshot))))
                        (list (cons current-window (cons (cadr snapshot) (caddr snapshot))))))
    ;; The buffer may have been saved or merged since this snapshot was
    ;; taken, changing its current disk base.  For a file buffer, derive
    ;; modified state from that base instead of restoring a stale flag.
    (let* ([b (head:window-buffer current-window)]
           [base (head:buffer-base b)])
      (set! modified?
        (if base
            (not (string=? (buffer-text b) base))
            (list-ref snapshot 4))))
    (set! mark-active? #f)
    (head:clamp-buffer-positions! (head:window-buffer current-window))
    (paint:invalidate-screen-cache!))

  ;; Undo entries are labeled with the user-level action that made them
  ;; -- "insert \"hello\"", "(search:replace-all! \"xx\" \"yy\")" -- and undo
  ;; and redo report the label.  Inside a call-as-one-edit! group, the
  ;; box holds (label . buffer-entries): one entry per buffer the
  ;; group touches, labeled with the group's label (or, lacking one,
  ;; that buffer's first edit's).
  (define edit-group (make-parameter #f))
  (define pending-edit (make-parameter #f))

  (define (check-disk-before-edit!)
    ;; The start of an edit session -- one undo entry; chained typing
    ;; checks once: if the file changed on disk meanwhile, mark the
    ;; buffer stale -- a red !! in the status bar -- and let the edit
    ;; proceed; the save guard still compares contents.  The mtime
    ;; raises the suspicion cheaply; the content confirms it, so a
    ;; mere touch passes silently.
    (let ([b (head:window-buffer current-window)])
      (when (and file-name (head:buffer-base b))
        (let-values ([(text revision facts) (head:buffer-state b)])
          (let ([path (cond [(assq 'file facts) => cdr] [else #f])]
                [base (cond [(assq 'base facts) => cdr] [else #f])])
            (when (and path base)
              (let ([stamp (file:stamp path)])
                (unless (and stamp (equal? stamp (cond [(assq 'stamp facts) => cdr] [else #f])))
                  (let ([disk (guard (ex [else #f]) (read-disk path))])
                    (head:buffer-facts-set! b
                      (cons (cons 'stamp (and disk (cdr disk)))
                            (if (and disk (string=? (car disk) base)) '() '((stale . #t))))
                      (property:select facts '(file base stamp stale))))))))))))

  (define (check-editable!)
    ;; The same guard protects fresh edits and history restoration:
    ;; #t forbids all edits, and a procedure decides per edit.
    (let ([guard (head:buffer-read-only (head:window-buffer current-window))])
      (when (if (procedure? guard) (not (guard)) guard)
        (raise (condition (kernel:make-read-only-error)
                          (make-message-condition "buffer is read-only"))))))

  (define (call-with-recorded-edit! label thunk)
    (check-editable!)
    (let ([b (head:window-buffer current-window)]
          [pending (pending-edit)])
      (if (and pending (eq? (car pending) b))
          (thunk)
          (begin
            (unless (suppress-history) (check-disk-before-edit!))
            (let* ([h (head:buffer-history b)]
                   [group (edit-group)]
                   [group-hit (and group (assq b (cdr (unbox group))))]
                   [group-entry (and group-hit (memq (cdr group-hit) (vector-ref h 0))
                                     group-hit)]
                   [previous
                    (or (and group-entry (cdr group-entry))
                        (and (not group) (suppress-history)
                             (pair? (vector-ref h 0)) (car (vector-ref h 0))))]
                   [label (cond [group-entry (car previous)]
                                [group (or (car (unbox group)) label)]
                                [else label])]
                   [entry (or previous (cons label (editor-snapshot)))]
                   [committed? #f]
                   [commit!
                    (lambda ()
                      (unless committed?
                        (unless previous
                          (vector-set! h 0 (cons entry (vector-ref h 0))))
                        (vector-set! h 1 '())
                        (set-car! entry label)
                        (when (and group (not group-entry))
                          (set-box! group
                            (cons (car (unbox group))
                                  (cons (cons b entry) (remq group-hit (cdr (unbox group)))))))
                        (set! committed? #t)))])
              (parameterize ([pending-edit (list b entry label commit!)])
                (thunk)))))))

  (define-syntax with-recorded-edit
    (syntax-rules ()
      [(_ label body ...)
       (call-with-recorded-edit! label (lambda () body ...))]))

  (edoc "Bundle every edit the thunk makes into one labeled undo step per buffer it touches; nested groups defer to the outermost."
        (label (or string #f) "the undo label")
        (thunk thunk "the edits to group")
        (returns any "what the thunk returns"))
  (define (call-as-one-edit! label thunk)
    ;; Bundle every edit thunk makes into one labeled undo step per
    ;; buffer it touches -- and none for buffers it does not edit.
    ;; Nested groups defer to the outermost.
    (if (edit-group)
        (thunk)
        (parameterize ([edit-group (box (cons label '()))]) (thunk))))

  (define (check-undo-scope scope)
    (unless (memq scope '(mine all))
      (error 'undo-scope "expected mine or all" scope))
    scope)

  (edoc "The default scope of undo!: mine, this head's own latest live action, or all, any actor's."
        (value (one-of mine all)))
  (define undo-scope (make-parameter 'mine check-undo-scope))

  (define (no-history verb)
    (format "No further ~a information" (string-downcase verb)))

  (define (local-history-shift! from to verb scope)
    (cond
      [(and (pair? scope) (not (equal? (cadr scope) head:ui-actor)))
       "Local buffers have no other actors' changes"]
      [(null? (vector-ref history from)) (no-history verb)]
      [else
       (check-editable!)
       (let* ([entry (car (vector-ref history from))]
              [snapshot (cdr entry)]
              [before (editor-snapshot (snapshot-key snapshot))])
         (restore-snapshot! snapshot)
         (vector-set! history from (cdr (vector-ref history from)))
         (vector-set! history to (cons (cons (car entry) before) (vector-ref history to)))
         (string:elide (if (car entry) (format "~a ~a" verb (car entry)) verb) cols))]))

  (define (shared-history-shift! from to verb scope)
    (check-editable!)
    (let* ([b (head:window-buffer current-window)]
           [before (editor-snapshot #f)])
      (let-values ([(status detail)
                    (head:store-history! b (if (zero? from) 'undo 'redo) scope)])
        (case status
          [(nothing) (no-history verb)]
          [(applied)
           (let* ([author (caddr detail)]
                  [key (if (and (equal? author head:ui-actor) (list-ref detail 3))
                           (list-ref detail 3)
                           (list 'store-action author (cadr detail)))]
                  [entry (find (lambda (entry) (equal? (snapshot-key (cdr entry)) key))
                               (vector-ref history from))]
                  [label (or (and entry (car entry)) (list-ref detail 4) "edit")])
             (when entry
               (vector-set! history from (remq entry (vector-ref history from))))
             (vector-set! history to
               (cons (cons label (append (list-head before 6) (list key)))
                     (vector-ref history to)))
             (set! mark-active? #f)
             (head:window-goal-set! current-window #f)
             (head:clamp-buffer-positions! b)
             (paint:invalidate-screen-cache!)
             (string:elide (format "~a ~s: ~a" verb author label) cols))]
          [else
           (format "~a blocked: ~a" verb
                   (case detail
                     [(read-only) "the buffer is read-only"]
                     [(basis-too-old) "history is incomplete"]
                     [(overlap) "another edit overlaps this action"]
                     [(property-changed) "a text property changed after this action"]
                     [else "the store is unavailable"]))]))))

  (define (history-shift! from to verb scope)
    (set! message
      (if (head:buffer-store-id (head:window-buffer current-window))
          (shared-history-shift! from to verb scope)
          (local-history-shift! from to verb scope)))
    message)

  (edoc "Undo one action in the current buffer within the undo-scope: this head's latest under mine, any actor's under all."
        (returns string "the report shown in the echo area"))
  (define (undo!)
    (history-shift! 0 1 "Undo" (undo-scope)))

  (edoc "Reverse this head's latest undo."
        (returns string "the report shown in the echo area"))
  (define (redo!)
    (history-shift! 1 0 "Redo" 'mine))

  (edoc "Undo an actor's latest live action in the current shared buffer."
        (who actor "the actor's identity")
        (returns string "the report shown in the echo area"))
  (define (undo-actor! who)
    (history-shift! 0 1 "Undo" (list 'actor who)))

  ;;; Point, mark, and editing ----------------------------------------------

  (define (changed!)
    (unless (head:buffer-store-id (head:window-buffer current-window))
      (set! modified? #t))
    (set! message "") (set! mark-active? #f)
    (head:window-goal-set! current-window #f))

  (define (ordered-region) ; -> start-row start-col end-row end-col
    (if (or (< point-row mark-row)
            (and (= point-row mark-row) (< point-col mark-col)))
        (values point-row point-col mark-row mark-col)
        (values mark-row mark-col point-row point-col)))

  (define (clamp-point!)
    (set! point-row (max 0 (min point-row (- (vlen) 1))))
    (set! point-col (max 0 (min point-col (string-length (current-display-line))))))

  (edoc "Move point one character left, crossing to the end of the previous line.")
  (define (move-left!)
    (cond [(> point-col 0) (set! point-col (- point-col 1))]
          [(> point-row 0)
           (set! point-row (- point-row 1))
           (set! point-col (string-length (current-display-line)))]))

  (edoc "Move point one character right, crossing to the start of the next line.")
  (define (move-right!)
    (cond [(< point-col (string-length (current-display-line)))
           (set! point-col (+ point-col 1))]
          [(< point-row (- (vlen) 1))
           (set! point-row (+ point-row 1)) (set! point-col 0)]))

  (edoc "Move point a number of characters, negative to the left, crossing line ends as single steps do."
        (delta integer "how far, negative for left"))
  (define (move-horizontal! delta)
    ;; Move point delta characters, negative to the left, crossing line
    ;; ends the way repeated single steps do.
    (if (< delta 0)
        (do ([i 0 (- i 1)]) ((= i delta)) (move-left!))
        (do ([i 0 (+ i 1)]) ((= i delta)) (move-right!))))

  ;; Vertical moves aim for a goal column, so point comes back to it after
  ;; passing through shorter lines (as in Emacs).  The goal is the
  ;; window's, kept with the navigation context that set it, and survives
  ;; exactly as long as each command finds point where the previous
  ;; vertical move left it; anything else that moves point, or a change
  ;; of buffer, revision or wrapping, starts a fresh goal.

  (define (goal-position wrapped?)
    ;; Equal row/column numbers alone do not identify a navigation context.
    (let ([b (head:current-buffer)])
      (list current-window b (caddr (head:edit-basis b))
            (head:window-lines current-window)
            (and wrapped? (paint:wrap-width current-window))
            (render:header (head:window-rendition current-window)) point-row point-col)))

  (define (visual-column w row col)
    (let* ([frame (head:window-rendition w)]
           [breaks (and (paint:window-wrapped? w)
                        (paint:line-breaks w (vector-ref (head:window-lines w) row)))])
      (- (render:column frame row col)
         (if breaks
             (render:column frame row (paint:segment-start breaks (paint:segment-of breaks col))) 0))))

  (edoc "Move point a number of lines, negative for up, aiming for the goal column; visual rows in a wrapping window."
        (delta integer "how far, negative for up"))
  (define (move-vertical! delta)
    ;; By buffer lines -- or by visual rows in a soft-wrapping window,
    ;; where up and down walk a long line's segments (C-a and C-e
    ;; still treat it as one line). The goal column is always in cells.
    (define wrapped? (paint:window-wrapped? current-window))
    (define goal-col
      (let ([goal (head:window-goal current-window)])
        (if (and goal (equal? (cdr goal) (goal-position wrapped?)))
            (car goal)
            (visual-column current-window point-row point-col))))
    (define (land! breaks k)
      ;; the goal column within segment k, clamped into it
      (set! point-col (paint:column-at-cell current-window point-row breaks k goal-col)))
    (if wrapped?
        (let step ([n delta])
          (cond
            [(zero? n) (void)]
            [(negative? n)
             (let* ([breaks (paint:line-breaks current-window (current-display-line))]
                    [seg (paint:segment-of breaks point-col)])
               (cond
                 [(> seg 0)                ; up, within the same line
                  (land! breaks (- seg 1))]
                 [(> point-row 0)          ; onto the line above's last row
                  (set! point-row (- point-row 1))
                  (let ([breaks (paint:line-breaks current-window
                                                   (current-display-line))])
                    (land! breaks (- (vector-length breaks) 1)))]))
             (step (+ n 1))]
            [else
             (let* ([breaks (paint:line-breaks current-window (current-display-line))]
                    [seg (paint:segment-of breaks point-col)])
               (cond
                 [(< (+ seg 1) (vector-length breaks))
                  (land! breaks (+ seg 1))]  ; down, within the same line
                 [(< point-row (- (vlen) 1))
                  (set! point-row (+ point-row 1))
                  (land! (paint:line-breaks current-window (current-display-line)) 0)]))
             (step (- n 1))]))
        (begin
          (set! point-row (max 0 (min (+ point-row delta) (- (vlen) 1))))
          (set! point-col (paint:column-at-cell current-window point-row #f 0 goal-col))))
    (head:window-goal-set! current-window (cons goal-col (goal-position wrapped?))))

  (define (split-inserted-lines s)
    ;; Unlike split-lines, retain an empty final part: inserting "a\n"
    ;; creates a new empty row and leaves point on it.
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond [(= i n) (reverse (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
              [else (loop (+ i 1) start acc)]))))

  (edoc "Insert text at point as one undo entry; its newlines become line breaks."
        (s string "the text to insert"))
  (define (insert-text! s)
    (insert-text-as! s (format "insert ~s" s)))

  (define (insert-text-as! s label)
    ;; Buffer rows never contain newline characters.  Programmatic inserts
    ;; get the same structural treatment as a paste or repeated newline!.
    (unless (string=? s "")
      (let* ([b (head:window-buffer current-window)]
             [source (edit-basis-for b)]
             [row point-row] [col point-col]
             [parts (split-inserted-lines s)])
        (with-recorded-edit label
          (parameterize ([edit-source source])
            (submit-edit! b (text:make-span row col row col) parts))
          (changed!)))))

  (edoc "Insert a line break at point.")
  (define (newline!)
    (insert-text-as! "\n" "newline"))

  (edoc "Delete the character after point, or join the next line at a line end.")
  (define (delete-forward!)
    (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
           [row point-row] [col point-col] [line (current-line)])
      (cond [(< col (string-length line))
             (with-recorded-edit (format "delete ~s" (string (string-ref line col)))
               (parameterize ([edit-source source])
                 (submit-edit! b (text:make-span row col row (+ col 1)) '("")))
               (changed!))]
            [(< row (- (vector-length (car source)) 1))
             (with-recorded-edit "delete newline"
               (parameterize ([edit-source source])
                 (submit-edit! b (text:make-span row col (+ row 1) 0) '("")))
               (changed!))])))

  (edoc "Delete the character before point, or join with the previous line at a line start.")
  (define (backspace!)
    (when (or (> point-col 0) (> point-row 0))
      (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
             [end-row point-row] [end-col point-col]
             [row (if (> end-col 0) end-row (- end-row 1))]
             [col (if (> end-col 0) (- end-col 1) (string-length (line-at row)))])
        (with-recorded-edit
          (if (> end-col 0)
              (format "delete ~s" (string (string-ref (line-at row) col)))
              "delete newline")
          (parameterize ([edit-source source])
            (submit-edit! b (text:make-span row col end-row end-col) '("")))
          (changed!)))))

  ;;; Kill and yank ---------------------------------------------------------

  (edoc "Whether every kill also reaches the terminal's clipboard, through OSC 52."
        (value boolean))
  (define forward-kill-ring-to-system-clipboard (make-parameter
                                                  #f
                                                  (lambda (enabled?)
                                                    (unless (boolean? enabled?)
                                                      (error 'forward-kill-ring-to-system-clipboard
                                                        "expected a boolean" enabled?))
                                                    enabled?)))

  (define base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define (base64-encode bytes)
    (let ([length (bytevector-length bytes)])
      (let loop ([at 0] [parts '()])
        (if (= at length)
            (apply string-append (reverse parts))
            (let* ([remaining (- length at)]
                   [a (bytevector-u8-ref bytes at)]
                   [b (if (> remaining 1)
                          (bytevector-u8-ref bytes (+ at 1)) 0)]
                   [c (if (> remaining 2)
                          (bytevector-u8-ref bytes (+ at 2)) 0)]
                   [bits (+ (bitwise-arithmetic-shift-left a 16)
                            (bitwise-arithmetic-shift-left b 8) c)]
                   [digit (lambda (shift)
                            (string
                              (string-ref
                                base64-alphabet
                                (bitwise-and
                                  (bitwise-arithmetic-shift-right bits shift)
                                  63))))]
                   [chunk (string-append
                            (digit 18) (digit 12)
                            (if (> remaining 1) (digit 6) "=")
                            (if (> remaining 2) (digit 0) "="))])
              (loop (+ at (min 3 remaining)) (cons chunk parts)))))))

  (define (publish-system-clipboard! text)
    ;; OSC 52 lets the host terminal own the clipboard, which also works when
    ;; e is several SSH or multiplexer layers away from the desktop. The
    ;; payload is base64, so buffer contents cannot terminate the sequence.
    (when (and (forward-kill-ring-to-system-clipboard) (paint:screen-live?))
      (with-mutex paint:redraw-lock
        (paint:ansi! "\x1b;]52;c;" (base64-encode (string->utf8 text)) "\x1b;\\")
        (flush-output-port (sys:terminal-output-port)))))

  (define (killing?)
    ;; was the previous command a kill?  Consecutive kills accumulate
    ;; into a single kill-ring entry.
    (and (memq (head:last-command) (list kill-line! kill-region!)) #t))

  (define (kill! text)
    (set! kill-ring (if (killing?) (string-append kill-ring text) text))
    (publish-system-clipboard! kill-ring))

  (edoc "Replace the kill ring's text without changing a buffer or point; C-y yanks it."
        (text string "the new kill ring text"))
  (define (copy-to-kill-buffer! text)
    ;; Replace the text yanked by C-y without changing a buffer or point.
    (unless (string? text)
      (error 'copy-to-kill-buffer! "expected a string" text))
    (set! kill-ring text)
    (publish-system-clipboard! kill-ring)
    (void))

  (edoc "Kill from point to the end of the line, or the line break when point is at the end; consecutive kills accumulate.")
  (define (kill-line!)
    (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
           [row point-row] [col point-col] [s (current-line)] [n (string-length s)])
      (cond [(< col n)
             (let ([text (substring s col n)])
               (with-recorded-edit (format "kill ~s" text)
                 (parameterize ([edit-source source])
                   (submit-edit! b (text:make-span row col row n) '("")))
                 (kill! text)
                 (changed!)))]
            [(< point-row (- (vlen) 1))
             (delete-forward!)
             (kill! "\n")])))

  (edoc "The kill ring's text."
        (returns string))
  (define (current-kill-ring)
    ;; The kill ring's text, for consumers outside the buffer -- the
    ;; terminal's yank, a future clipboard bridge.
    kill-ring)

  (edoc "Insert the kill ring's text at point.")
  (define (yank!)
    ;; Kill-ring entries can span lines after consecutive C-k commands.  Insert
    ;; newlines as buffer structure rather than embedding them in a line string.
    (unless (string=? kill-ring "")
      (insert-text-as! kill-ring (format "yank ~s" kill-ring))))

  (define (text-between sr sc er ec)
    (if (= sr er)
        (substring (line-at sr) sc ec)
        (let loop ([row (- er 1)] [acc (list (substring (line-at er) 0 ec))])
          (if (< row sr)
              (apply string-append acc)
              (loop (- row 1)
                    (cons (if (= row sr)
                              (string:tail (line-at sr) sc)
                              (line-at row))
                          (cons "\n" acc)))))))

  (define (delete-region! sr sc er ec)
    (submit-edit! (head:window-buffer current-window)
                  (text:make-span sr sc er ec) '("")))

  (edoc "Replace the text between two ordered points with new text, in one structural edit."
        (start position "where the replaced text starts")
        (end position "where it ends")
        (text string "the replacement"))
  (define (replace-region-text! start end text)
    ;; Replace one ordered buffer range in a single structural operation.
    ;; Bulk editors use this instead of rebuilding a line once per match.
    (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
           [parts (split-inserted-lines text)])
      (with-recorded-edit "replace region"
        (parameterize ([edit-source source])
          (submit-edit! b (text:make-span (car start) (cdr start) (car end) (cdr end)) parts))
        (changed!))))

  (edoc "Replace the text between two ordered points with text computed against a basis, in one structural edit that leaves point where it was: the basis, head:edit-basis taken before the computation, lets the store project point and mark into the revision it accepts."
        (basis list "the edit basis the text was computed against")
        (start position "where the replaced text starts")
        (end position "where it ends")
        (text string "the replacement"))
  (define (rewrite-region! basis start end text)
    ;; the editing operation behind the bulk replacers: an explicit basis
    ;; and a kept point, with the undo grouping and mark handling of every
    ;; recorded edit
    (parameterize ([edit-source basis] [edit-point (head:point)])
      (replace-region-text! start end text)))

  (edoc "Copy the text between mark and point to the kill ring without deleting it; the mark deactivates.")
  (define (copy-region!)
    ;; Save the region to the kill ring without deleting it -- M-w, as
    ;; in Emacs.  The mark deactivates; C-y reinserts.
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let-values ([(sr sc er ec) (ordered-region)])
          (if (and (= sr er) (= sc ec))
              (set! message "Empty region")
              (begin
                (copy-to-kill-buffer! (text-between sr sc er ec))
                (set! mark-active? #f)
                (set! message "Copied"))))))

  (edoc "Kill the text between mark and point into the kill ring.")
  (define (kill-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let-values ([(sr sc er ec) (ordered-region)])
          (if (and (= sr er) (= sc ec))
              (set! message "Empty region")
              (let ([text (text-between sr sc er ec)]
                    [source (edit-basis-for (head:window-buffer current-window))])
                (with-recorded-edit (format "kill ~s" text)
                  (parameterize ([edit-source source]) (delete-region! sr sc er ec))
                  (kill! text)
                  (changed!)))))))

  ;;; Files -----------------------------------------------------------------

  (define (file-buffer path)
    ;; -> (values buffer created?). Consult shared identity before reading
    ;; disk; admission rechecks it under the writer if another visitor wins.
    (cond [(store:find-file path)
           => (lambda (id)
                (values (or (head:adopt-store-buffer! id)
                            (error 'visit-file! "buffer visiting this file is not visible" path)) #f))]
      [else
       (when (file-directory? path) (refuse-file! "Choose a file inside the directory"))
       (unless (file-directory? (file:directory-part path))
         (refuse-file! "Parent directory does not exist"))
       (let* ([disk (and (file-exists? path) (file:read-state path))]
              [lines (file:lines (if disk (car disk) ""))]
              [detected (mode:detect path (vector-ref lines 0))])
         (head:visit-file! (file:base-name path) lines
                           (append (list (cons 'file path) (cons 'mode (and detected (mode:name detected))))
                             (if disk
                                 (list (cons 'trailing (file:ends-in-newline? (car disk)))
                                       (cons 'base (car disk)) (cons 'stamp (cdr disk))) '()))))]))

  (define (prepare-file-visit path)
    ;; Acquire the file before replacing the prompt. The returned action
    ;; shows the admitted buffer once its window is restored; no second
    ;; initial read or second creation is needed to recover from an error.
    (let ([path (file:visit-path path)])
      (let-values ([(b created?)
                    (cond [(find (lambda (b) (and (not (head:buffer-store-id b))
                                                  (equal? (head:buffer-file b) path))) buffers)
                           => (lambda (b) (values b #f))]
                      [else (file-buffer path)])])
        (lambda ()
          (head:show-buffer! b)
          (log:add! 'visit-file!
            (cons (if created? (if (head:buffer-base b) "Loaded" "New file:") "Visited") path)
            created?)
          (unless created?
            (let-values ([(text revision facts) (head:buffer-state b)])
              (let ([base (cond [(assq 'base facts) => cdr] [else #f])])
                (when (and base (equal? path (cond [(assq 'file facts) => cdr] [else #f])))
                  ;; Reopening compares content even if a stamp is unchanged.
                  (let ([disk (guard (ex [else #f]) (read-disk path))])
                    (cond
                      [(and disk (string=? (car disk) base))
                       (head:buffer-facts-set! b
                         (list (cons 'stamp (cdr disk)) '(stale . #f))
                         (property:select facts '(file base stamp stale)))]
                      [disk (reopen-changed-file! b path disk (cons revision facts))]
                      [else
                       (parameterize ([message-source 'visit-file!])
                         (set-message! (format "Cannot reread ~a" path)))]))))))))))

  (edoc "Visit a file in the current window, creating or reusing its buffer; nothing is written to disk."
        (path file "the file to visit")
        (prompts))
  (define (visit-file! path)
    ;; Direct visits and the interactive picker share acquisition and the
    ;; buffer-only merge/reread/cancel flow. Visiting never writes to disk.
    (guard (ex [else
                (parameterize ([message-source 'visit-file!])
                  (set-message! (format "Cannot open ~a: ~a" path (kernel:condition-text ex))))])
      ((prepare-file-visit path))))

  (define (refuse! message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (refuse-file! message) (refuse! message))


  (define (read-disk path)
    ;; #f means genuinely absent.  An existing file that cannot be read
    ;; cannot be compared with the buffer's base, so fail closed instead
    ;; of treating it as a new destination and replacing it unchecked.
    (and (file-exists? path)
         (guard (ex [else
                     (refuse-file!
                       (format "Cannot verify ~a: ~a" path (kernel:condition-text ex)))])
           (file:read-state path))))

  (define (review-disk! path disk)
    (let ([now (read-disk path)])
      (unless (equal? (and disk (car disk)) (and now (car now)))
        (refuse-file! "Disk changed again; operation cancelled. Review the file again."))
      now))

  (define (file-review facts)
    (property:select facts '(file base)))

  (define (check-file-review! facts review)
    (unless (property:matches? review facts)
      (refuse-file! "Buffer's file or baseline changed; operation cancelled. Review the file again.")))

  (edoc "Save the current buffer to a file, guarded by content: when the disk no longer matches the buffer's base, ask whether to overwrite, merge three-way or cancel."
        (target file "where to write: the buffer's own file, or a new destination it visits from then on")
        (returns boolean "whether the file was written")
        (prompts))
  (define (save-file! target)
    ;; Saving is guarded by content, not clocks: the disk is read and
    ;; compared with the buffer's base (what it loaded or last saved).
    ;; A mismatch means somebody changed the file meanwhile -- the
    ;; save stops and asks: overwrite, merge three-way, or cancel.
    (define path (file:visit-path target))
    (define b (head:window-buffer current-window))
    (define adopted? #f)
    (define disk #f)
    (define (write! review)
      (define written? #f)
      (guard (ex [else (parameterize ([message-source 'save-file!])
                         (set-message!
                           (if written?
                               (format "Wrote ~a, but could not finish saving: ~a" path (kernel:condition-text ex))
                               (format "Save failed: ~a" (kernel:condition-text ex)))))
                       #f])
        ;; Capture one coherent state after pre-save hooks.  The recorded
        ;; baseline is exactly what was written, even if a store subscriber
        ;; edits before these facts return.  Its dirty state stays derived.
        (let-values ([(text revision facts) (head:buffer-state b)])
          (check-file-review! facts review)
          (review-disk! path disk)
          (let* ([trailing (cond [(assq 'trailing facts) => cdr] [else #t])]
                 [written (file:text text trailing)]
                 [detected (and adopted? (mode:detect path (vector-ref text 0)))])
            (file:write! path text trailing)
            (set! written? #t)
            ;; A stat after writing could belong to another disk writer.
            ;; Invalidate the hint; the next edit verifies content again.
            (unless (head:buffer-facts-set! b
                      (append (list (cons 'file path) (cons 'base written)
                                '(stamp . #f) '(stale . #f))
                        (if adopted?
                            `((read-only . #f) (disposable . #f)
                              (mode . ,(and detected (mode:name detected))) (mode-auto . #t)) '())
                        (if (head:buffer-store-id b) '()
                          (list (cons 'modified (not (string=? (buffer-text b) written))))))
                      (append review (if adopted? (property:select facts '(read-only disposable mode mode-auto)) '()))
                      (file:base-name path))
              (refuse-file! "Buffer's file state changed; saved baseline was not updated."))))
        ;; File facts, label and adopted mode commit together. No follow-up
        ;; write may overwrite a subscriber's newer choice. Re-save keeps mode.
        ;; a conflicted merge reports its details once resolved --
        ;; saved with no markers left; the resolution preceded the
        ;; write, so its record does too
        (let ([pending (assq b merge-reports)])
          (when (and pending (not (buffer-has-conflicts? b)))
            (set! merge-reports (remq pending merge-reports))
            (log:add! 'save-file!
              (format "Merge resolved -- details in ~a" (cdr pending)))))
        (log:add! 'save-file! (cons "Wrote" path))
        (file:run-post-save-hooks! path)
        #t))
    (file:run-pre-save-hooks! path)
    (let-values ([(text revision facts) (head:buffer-state b)])
      (let* ([review (file-review facts)] [base (cond [(assq 'base facts) => cdr] [else #f])]
             [modified (cond [(assq 'modified facts) => cdr] [else #f])])
        (set! adopted? (not (equal? path (cond [(assq 'file facts) => cdr] [else #f]))))
        (set! disk (read-disk path))
        (cond
          [(and disk (not adopted?) (not modified)
             base (string=? (car disk) base))
           ;; nothing to do, and the mtime stays untouched
           (set! message "No changes to save")
           #f]
          [(and disk (not adopted?)
             (not (and base (string=? (car disk) base))))
           (unless (head:buffer-facts-set! b '((stale . #t))
                     (property:select facts '(file base stamp stale)))
             (refuse-file! "Buffer's file state changed; save cancelled. Review the file again."))
           (stale-save! b path disk review write!)]
          [(and disk adopted?)
           ;; saving under a new name onto an existing file
           (let ask ()
             (let* ([k (prompt:key! (format "~a exists; overwrite? y)es or n)o"
                                      (file:base-name path))
                                    "yn")]
                    [n (and k (char->integer k))])
               (cond [(memv n '(121 89)) (write! review)]
                 [(or (not n) (memv n '(110 78 7 27)))
                  (set! message "Save cancelled") #f]
                 [else (ask)])))]
          [else (write! review)]))))

  (define (merge-report! b report-lines)
    ;; The merge's paper trail: a read-only <merge-buffer> holding
    ;; diff's unified-diff-style rendering -- built quietly, never
    ;; displayed; the echo names it.  -> the report buffer's name.
    (let* ([name (format "*merge-~a*" (head:buffer-name b))]
           [rb (head:fresh-buffer! name)])
      (when (pair? report-lines) (apply head:buffer-append! rb report-lines))
      (head:buffer-read-only-set! rb #t)
      (head:buffer-name rb)))

  (define (merge-from-disk! b path disk review)
    ;; Replace the buffer with the three-way merge of its base, its
    ;; text, and the disk; -> the conflict count and the report
    ;; buffer's name.  The buffer adopts the disk as its new base
    ;; either way -- the external change is incorporated, so the next
    ;; save writes cleanly.  One undo entry.
    (let-values ([(text revision facts) (head:buffer-state b)])
      (check-file-review! facts review)
      (check-file-review! facts (list (cons 'file path)))
      (unless (cond [(assq 'base facts) => cdr] [else #f])
        (refuse-file! "Cannot merge: this buffer has no saved disk baseline."))
      (let ([disk (review-disk! path disk)]
            [source (head:edit-basis b)] [wanted (head:point)])
        (let-values ([(merged merged-trailing conflicts report-lines)
                      (file:merge path (cdr (assq 'base facts))
                        (file:text (car source) (cond [(assq 'trailing facts) => cdr] [else #t])) (car disk))])
          (with-recorded-edit "merge from disk"
            (parameterize ([edit-source source] [edit-point wanted])
              (replace-buffer-lines! b merged (list (cons 'trailing merged-trailing))
                                     (list (cons 'base (car disk)) (cons 'stamp (cdr disk)) '(stale . #f))
                                     (property:select facts '(file base trailing))))
            (changed!)
            (values conflicts (merge-report! b report-lines)))))))

  (define (reread-from-disk! b path disk review)
    ;; Discard the buffer's copy and adopt the disk verbatim.  Rereading is a
    ;; new baseline, not an edit: it clears modification and undo state.
    (let* ([disk (review-disk! path disk)]
           [accepted
            (and (equal? path (cond [(assq 'file (cdr review)) => cdr] [else #f]))
                 (head:store-reset! b (file:lines (car disk))
                   (append (list (cons 'trailing (file:ends-in-newline? (car disk)))
                                 (cons 'base (car disk)) (cons 'stamp (cdr disk)) '(stale . #f))
                           (if (head:buffer-store-id b) '() '((modified . #f))))
                   review))])
      ;; A reset subscriber may already have adopted/edited a newer source.
      ;; Its history and selection belong to that work, not this reread.
      (when (and accepted (= accepted (caddr (head:edit-basis b))))
        (head:buffer-history-set! b (vector '() '()))
        (head:buffer-marked-set! b #f)
        (set! merge-reports (remp (lambda (p) (eq? (car p) b)) merge-reports)))
      (parameterize ([message-source 'visit-file!])
        (set-message! (if accepted (format "Reread ~a" path)
                        "Buffer changed; reread cancelled. Reopen the file to review it again.")))
      (and accepted #t)))

  (define (reopen-changed-file! b path disk review)
    (let ([facts (cdr review)])
      (let ask ()
        (let* ([k (prompt:key!
                    (format "~a changed on disk: m)erge, r)eread, c)ancel"
                            (file:base-name path))
                    "mrc")]
               [n (and k (char->integer k))])
          (cond
            [(memv n '(109 77))                                 ; m
             (let-values ([(conflicts report-name)
                           (merge-from-disk! b path disk (file-review facts))])
               ;; The merge incorporated this disk version into the buffer's
               ;; baseline.  It remains modified only when it differs from disk.
               (when (> conflicts 0)
                 (set! merge-reports
                   (cons (cons b report-name)
                     (remp (lambda (p) (eq? (car p) b)) merge-reports))))
               (parameterize ([message-source 'visit-file!])
                 (set-message!
                   (if (zero? conflicts)
                     (format "Merged from disk -- details in ~a" report-name)
                     (format "Merged with ~a conflict~a -- resolve (~a)"
                             conflicts (if (= conflicts 1) "" "s")
                             (keymap:command-hint
                               '(next-conflict! keep-mine! keep-disk!))))))
               #t)]
            [(memv n '(114 82)) (reread-from-disk! b path disk review)] ; r
            [(or (not n) (memv n '(99 67 7 27)))                ; c, C-g, ESC
             (keyboard-quit!)
             #f]
            [else (ask)])))))

  ;; Merge reports awaiting resolution -- (buffer . report-name): a
  ;; conflicted merge does not announce its report buffer up front;
  ;; the save that carries the resolved text does, separately.
  (define merge-reports '())

  (define (buffer-conflict-count b)
    ;; How many merge conflict markers are left in b.
    (file:conflict-count (head:buffer-lines b)))

  (define (buffer-has-conflicts? b)
    (> (buffer-conflict-count b) 0))

  (define (stale-save! b path disk review write!)
    (define merge?
      (exists (lambda (entry) (and (pair? entry) (eq? (car entry) 'base) (cdr entry))) review))
    (let ask ()
      (let* ([k (prompt:key!
                  (format "~a changed on disk: ~a" (file:base-name path)
                    (if merge? "o)verwrite, m)erge, c)ancel"
                        "no saved baseline; o)verwrite, c)ancel"))
                  (if merge? "omc" "oc"))]
             [n (and k (char->integer k))])
        (cond
          [(memv n '(111 79)) (write! review)]                ; o
          [(memv n '(109 77))                                 ; m
           (let-values ([(conflicts report-name)
                         (merge-from-disk! b path disk review)])
             (if (zero? conflicts)
                 (and
                   (write! (list (car review) (cons 'base (car disk))))
                   (parameterize ([message-source 'save-file!])
                     (set-message!
                       (format "Merged and saved -- details in ~a"
                               report-name)))
                   #t)
                 (begin
                   (set! merge-reports
                     (cons (cons b report-name)
                           (remp (lambda (p) (eq? (car p) b))
                                 merge-reports)))
                   (parameterize ([message-source 'save-file!])
                     (set-message!
                       (format "Merged with ~a conflict~a -- resolve (~a), then save"
                               conflicts (if (= conflicts 1) "" "s")
                               (keymap:command-hint
                                 '(next-conflict! keep-mine! keep-disk!)))))
                   #f)))]
          [(memv n '(99 67 7 27)) (set! message "Save cancelled") #f]
          [(not n) #f]
          [else (ask)]))))

  (edoc "A buffer's text as its file would hold it: the lines joined with newlines, ending in one when the buffer keeps a trailing newline."
        (b buffer "the buffer to read")
        (returns string))
  (define (buffer-text b)
    ;; b's text as its file would hold it
    (file:text (head:buffer-lines b) (head:buffer-trailing b)))

  (edoc "Whether a buffer can be discarded without losing work: unmodified, or marked disposable; #f when its state cannot be read."
        (b buffer "the buffer to judge")
        (returns boolean))
  (define (buffer-clean? b)
    ;; Discard decisions use one current snapshot, not an empty/stale
    ;; head cache.  Read-only protects editing, not the lifetime of work.
    ;; Generated tools explicitly opt into disposal; failed reads fail closed.
    (guard (ex [else #f])
      (let-values ([(text revision facts) (head:buffer-state b)])
        (file:state-clean? text facts))))

  ;;; Buffer commands ------------------------------------------------------------

  ;; Read-only views of the editor's state, for M-x and modules; mutation
  ;; goes through the command API.
  (edoc "Show a message in the echo area; with a message-source it is logged too, with #f it is a plain indicator."
        (s string "the message"))
  (define (set-message! s)
    ;; A stamped message is a log entry -- recorded and shown; with
    ;; (message-source #f) it is an indicator, shown and forgotten,
    ;; like a CapsLock light, and an empty message merely clears the
    ;; indicator.  Either way it presents the moment it is set,
    ;; mid-command included, and never before the screen is the
    ;; editor's.
    (let ([src (message-source)])
      (if (and src (> (string-length s) 0))
          (log:add! src s)
          (paint:show-message! s #f))))
  (edoc "The selected region while the mark is active, else the whole current buffer as a region."
        (returns region))
  (define (current-region)
    (let ([m (head:mark)])
      (if m
          (region (head:current-buffer) m (head:point))
          (whole-buffer (head:current-buffer)))))

  (define (call-with-region r thunk)
    ;; r selected: its buffer current, the mark at its start and point at
    ;; its end; the previous selection and point return on exit and on
    ;; escape. The body of with-region, the one form M-x offers.
    (head:with-buffer (region-buffer r)
      (let ([saved-point (head:point)] [saved-mark (cons mark-row mark-col)] [saved-active mark-active?])
        (define (select! start end active?)
          (set! mark-row (car start)) (set! mark-col (cdr start)) (set! mark-active? active?)
          (set! point-row (car end)) (set! point-col (cdr end)))
        (dynamic-wind
          (lambda () (select! (region-start r) (region-end r) #t))
          thunk
          (lambda () (select! saved-mark saved-point saved-active))))))

  (edoc "Run body with a region selected: its buffer current, the mark at its start and point at its end; the previous selection and point return on exit and on escape: (with-region (region (buffer \"a\") '(0 . 0) '(4 . 0)) (search:replace-all! \"x\" \"y\"))."
        (r region "the region to select")
        (body (list-of any) "the forms to run"))
  (define-syntax with-region
    (syntax-rules ()
      [(_ r body ...) (call-with-region r (lambda () body ...))]))

  ;;; Apps and views ------------------------------------------------------------

  ;; The app registry, the hook registries, the view helpers, and the
  ;; window geometry helpers live in (head); the commands over them are
  ;; here.


  ;;; The log -----------------------------------------------------------------

  ;; The editor's syslog lives in (log) -- the structured records and
  ;; the formatter registry are state, not UI.  How a logged message is
  ;; shown is the head's side, here: set-message! and the echo-area
  ;; presenter that init! installs on the log.

  (edoc "Who a message came from, for the log's attribution: components parameterize it around their messages; #f makes a message a plain indicator, shown and never logged."
        (value (or symbol #f)))
  (define message-source (make-parameter 'e))

  (define message-progress
    ;; When true, a logged message supersedes its component's newest
    ;; line in the echo area -- progress redrawn in place rather than
    ;; stacked -- never a line from another component.  The log
    ;; records every step regardless.
    log:progress)

  (edoc "Show an existing log record in the echo area without logging it again."
        (e datum "the log record"))
  (define (present-log-entry! e)
    ;; Present an existing record in the echo area without logging it again.
    (present-log-entries! (list e)))

  (edoc "Queue several existing log records for the echo area and repaint once, with a ghost text after the last one when given."
        (entries (list-of datum) "the log records")
        (tail string "a ghost text after the last one"))
  (define present-log-entries!
    (case-lambda
      [(entries) (present-log-entries-with! entries "")]
      [(entries tail) (present-log-entries-with! entries tail)]))
  (define (present-log-entries-with! entries tail)
    ;; Queue several existing records and repaint once, avoiding a full echo
    ;; geometry change and terminal redraw for every streamed line.
    (let loop ([left entries])
      (when (pair? left)
        (let* ([e (car left)]
               [text (log:format-entry e)]
               [styler (log:styler (log:component e))]
               [ghost (if (null? (cdr left)) tail "")])
          (paint:echo-queue! (log:component e) text styler #f ghost)
          (loop (cdr left)))))
    (when (pair? entries) (paint:present-echo!)))


  ;;; Types ---------------------------------------------------------------------

  ;; The command type: what a key or a binding names. The literal types,
  ;; buffer, window, region and position, live in (literal) with their
  ;; spellings.

  (edoc-type command "a command: a procedure callable with no arguments, by its name"
    (predicate (lambda (v) (and (procedure? v) (logbit? 0 (procedure-arity-mask v)))))
    (write keymap:action-text))

  (edoc "Create an empty shared buffer with a name, suffixed when the name is taken, and show it here."
        (name string "the buffer's name")
        (returns buffer))
  (define (new-buffer! name)
    (let ([b (head:new-buffer! name)])
      (head:show-buffer! b)
      b))

  (define (age-text seconds)
    ;; how long ago, in the coarsest unit that is not zero
    (cond [(< seconds 60) (format "~a s" seconds)]
          [(< seconds 3600) (format "~a min" (quotient seconds 60))]
          [(< seconds 86400) (format "~a h" (quotient seconds 3600))]
          [else (format "~a d" (quotient seconds 86400))]))

  (define (now-seconds) (time-second (current-time 'time-utc)))

  (define (trashed-ids)
    (filter (lambda (id) (store:property id 'trashed #f)) (store:buffer-list)))

  (edoc "Kill a buffer at once: a shared document goes to the trash, where restore! finds it under its name for store:trash-retention days; disposable output is deleted and a local buffer forgotten."
        (b buffer "the buffer to kill"))
  (define (kill-buffer! b)
    (let ([id (head:buffer-store-id b)] [name (head:buffer-name b)])
      (let-values ([(text revision facts) (head:buffer-state b)])
        (let ([unsaved? (not (file:state-clean? text facts))]
              [disposable? (cond [(assq 'disposable facts) => cdr] [else #f])])
          (cond [(not id) (void)]
                [disposable? (store:delete! head:ui-actor id)]
                [else (store:set-properties! head:ui-actor id (list (list 'trashed (now-seconds) head:ui-actor)))])
          (head:forget-buffer! b)
          (parameterize ([message-source 'kill-buffer!])
            (set-message!
              (cond [(or (not id) disposable?) (format "Killed ~a" name)]
                    [unsaved? (format "Killed ~a; its unsaved work is in the trash" name)]
                    [else (format "Killed ~a; it is in the trash" name)])))))))

  (edoc "The trashed buffers, newest first, as (name killed-at actor): killed-at in UTC seconds; each expires store:trash-retention days after it was killed."
        (returns (list-of list)))
  (define (trash)
    (list-sort (lambda (a b) (> (cadr a) (cadr b)))
      (map (lambda (id)
             (let ([t (store:property id 'trashed #f)])
               (list (store:buffer-name id) (car t) (cadr t))))
           (trashed-ids))))

  (edoc-type trashed "the name of a buffer in the trash"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete (lambda (partial)
                (let ([now (now-seconds)])
                  (map (lambda (entry) (cons (car entry) (format "killed ~a ago" (age-text (- now (cadr entry))))))
                       (trash)))))
    (write (lambda (v) (format "~s" v)))
    (within string))

  (edoc "Bring a buffer back from the trash, with its text and history, and show it in the current window."
        (name trashed "the buffer's name in the trash")
        (returns buffer))
  (define (restore! name)
    (let ([id (find (lambda (id) (string=? (store:buffer-name id) name)) (trashed-ids))])
      (unless id (error 'restore! "no such buffer in the trash" name))
      (store:set-properties! head:ui-actor id '((trashed . #f)))
      (let ([b (head:adopt-store-buffer! id)])
        (unless b (error 'restore! "the buffer did not come back" name))
        (head:show-buffer! b)
        (parameterize ([message-source 'restore!])
          (set-message! (format "Restored ~a" (head:buffer-name b))))
        b)))

  (edoc "Delete every trashed buffer for good; how many went."
        (returns integer))
  (define (empty-trash!)
    (let ([ids (trashed-ids)])
      (for-each (lambda (id) (store:delete! head:ui-actor id)) ids)
      (parameterize ([message-source 'empty-trash!])
        (set-message! (format "Emptied the trash: ~a buffer~a" (length ids) (if (= (length ids) 1) "" "s"))))
      (length ids)))

  ;;; Indentation and formatting ------------------------------------------------

  ;; Both are provided per mode by modules.  An indenter maps rows to
  ;; where their text should start: (proc buffer from to) -> one entry
  ;; per row of from..to -- #f leaving a row alone (a multi-line
  ;; string's interior, say), a column, or an ascending list of
  ;; columns when several indentations are valid (its stops) --
  ;; computed as if each row settles on the stop nearest its current
  ;; indentation, top to bottom.  The commands settle likewise; TAB
  ;; instead cycles: the nearest stop to the right, wrapping around.
  ;; A formatter rewrites rows wholesale: (proc buffer from to) -> the
  ;; replacement lines, or #f when the rows cannot be formatted.  TAB
  ;; indents the current line when the mode registered its indenter
  ;; with the tab flag on (the default).
  (define indenters (kernel:make-registry))   ; entries (mode proc tab?)
  (define formatters (kernel:make-registry))  ; entries (mode proc)

  (edoc "Register a mode's indenter: (proc buffer from to) gives each row's column, its list of stops, or #f to leave it; tab says whether TAB runs it, on when omitted."
        (name mode "the mode")
        (proc procedure "the indenter")
        (tab boolean "whether TAB indents"))
  (define register-indenter!
    (case-lambda
      [(name proc) (register-indenter! name proc #t)]
      [(name proc tab) (kernel:registry-add! indenters (list name proc tab))]))

  (edoc "Register a mode's formatter: (proc buffer from to) gives the replacement lines, or #f when the rows cannot be formatted."
        (name mode "the mode")
        (proc procedure "the formatter"))
  (define (register-formatter! name proc)
    (kernel:registry-add! formatters (list name proc)))

  (define (mode-entry registry)
    (let ([m (mode:name-of (head:window-buffer current-window))])
      (and m (kernel:registry-find registry (lambda (x) (string=? (car x) m))))))

  (define (leading-blanks s)
    (let loop ([i 0])
      (if (and (< i (string-length s))
               (memv (string-ref s i) '(#\space #\tab)))
          (loop (+ i 1))
          i)))

  (define (settle-stops col cur)
    ;; An indenter entry resolved for a line currently at cur: the
    ;; nearest stop (ties leftward); a bare column stands.
    (if (pair? col)
        (fold-left (lambda (best s)
                     (if (< (abs (- s cur)) (abs (- best cur))) s best))
                   (car col) col)
        col))

  (define (cycle-stops col cur)
    ;; TAB's resolution: the nearest stop right of cur, wrapping back
    ;; to the first past the last.
    (if (pair? col)
        (or (find (lambda (s) (> s cur)) col) (car col))
        col))

  (define (apply-indent! from cols pad?)
    ;; Rewrite the leading whitespace of rows from.. to the given
    ;; columns (#f leaves a row, as does a whitespace-only row --
    ;; except with pad?, which pads it out to the column: TAB on a
    ;; blank line).  One undo entry; point and mark follow their
    ;; line's text, landing on the indentation when they sat inside
    ;; the old one.  -> whether anything changed.
    (define b (head:window-buffer current-window))
    (define v (car (edit-basis-for b)))
    (define wanted-point (edit-point))
    (define wanted-mark (edit-mark))
    (define n (vector-length v))
    (define (retabbed s col)
      (let ([rest (string:tail s (leading-blanks s))])
        (if (string=? rest "")
            (if pad? (make-string col #\space) s)
            (string-append (make-string col #\space) rest))))
    (let ([changes
           (let loop ([r from] [cs cols] [acc '()])
             (if (or (null? cs) (>= r n))
                 (reverse acc)
                 (loop (+ r 1) (cdr cs)
                       (if (and (car cs)
                                (not (string=? (retabbed (vector-ref v r)
                                                         (car cs))
                                               (vector-ref v r))))
                           (cons (cons r (car cs)) acc)
                           acc))))])
      (when (pair? changes)
        (with-recorded-edit "indent"
          (let ([nv (let ([o (make-vector n)])
                      (do ([i 0 (+ i 1)]) ((= i n) o)
                        (vector-set! o i (vector-ref v i))))]
                [next-point-col (cdr wanted-point)]
                [next-mark-col (and wanted-mark (cdr wanted-mark))])
            (for-each
              (lambda (change)
                (let* ([row (car change)] [col (cdr change)]
                       [old (vector-ref v row)]
                       [lead (leading-blanks old)]
                       [follow (lambda (c)
                                 (if (<= c lead) col (+ c (- col lead))))])
                  (vector-set! nv row (retabbed old col))
                  (when (= row (car wanted-point))
                    (set! next-point-col (follow (cdr wanted-point))))
                  (when (and wanted-mark (= row (car wanted-mark)))
                    (set! next-mark-col (follow (cdr wanted-mark))))))
              changes)
            (parameterize ([edit-point (cons (car wanted-point) next-point-col)]
                           [edit-mark (and wanted-mark (cons (car wanted-mark) next-mark-col))])
              (replace-buffer-lines! b nv)))
          (changed!)))
      (pair? changes)))

  (define (indent-rows! from to)
    ;; Indent rows [from, to] by the mode's indenter, each settling on
    ;; the stop nearest its current indentation; -> #f without one.
    (let ([entry (mode-entry indenters)])
      (if (not entry)
          (begin (set! message "No indenter for this mode") #f)
          (let* ([b (head:window-buffer current-window)]
                 [source (head:edit-basis b)]
                 [v (car source)]
                 [wanted (head:point)] [selected (head:mark)]
                 [last (min to (- (vector-length v) 1))]
                 [cols (let settle ([r from]
                                    [cs ((cadr entry) b from last)]
                                    [acc '()])
                         (if (null? cs)
                             (reverse acc)
                             (settle (+ r 1) (cdr cs)
                                     (cons (settle-stops
                                             (car cs)
                                             (leading-blanks
                                               (vector-ref v r)))
                                           acc))))])
            (parameterize ([edit-source source] [edit-point wanted] [edit-mark selected])
              (apply-indent! from cols #f))
            #t))))

  (edoc "Indent the current line by the mode's indenter, cycling through its stops; point lands on the indentation.")
  (define (indent-line!)
    ;; TAB's work: indent the current line, cycling through its stops
    ;; -- the nearest stop right of the current indentation, wrapping
    ;; -- and land on the indentation (a blank line pads out to it);
    ;; point already past it stays with its text.
    (let ([entry (mode-entry indenters)])
      (if (not entry)
          (set! message "No indenter for this mode")
          (let* ([b (head:window-buffer current-window)]
                 [source (head:edit-basis b)]
                 [wanted (head:point)] [selected (head:mark)]
                 [row (car wanted)]
                 [lead (leading-blanks (vector-ref (car source) row))]
                 [cols ((cadr entry) b row row)]
                 [col (and (pair? cols)
                           (cycle-stops (car cols) lead))])
            (when col
              (unless (parameterize ([edit-source source] [edit-point wanted] [edit-mark selected])
                        (apply-indent! row (list col) #t))
                (when (and (eq? (car source) (head:buffer-lines b)) (< point-col col))
                  (set! point-col col)))))))
    (void))

  (edoc "What TAB does: indent the current line when the mode's indenter asked for it, else nothing.")
  (define (indent-tab!)
    ;; TAB: the mode indents when it asked to; otherwise nothing.
    (let ([entry (mode-entry indenters)])
      (when (and entry (caddr entry))
        (indent-line!))))

  (edoc "Set whether TAB indents in a mode, overriding the flag its indenter registered with."
        (name mode "the mode")
        (flag boolean "whether TAB indents"))
  (define (indent-on-tab! name flag)
    ;; Configuration: whether TAB auto-indents in the named mode,
    ;; overriding the flag its indenter registered with.
    (let ([entry (kernel:registry-find indenters
                                       (lambda (x) (string=? (car x) name)))])
      (unless entry (error 'indent-on-tab! "no indenter for mode" name))
      (kernel:registry-add! indenters (list name (cadr entry) flag))))

  (edoc "Indent the lines between mark and point by the mode's indenter, each settling on its nearest stop.")
  (define (indent-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (indent-rows! from to)
            (set! message (format "Indented ~a line~a" (+ (- to from) 1)
                                  (if (= from to) "" "s"))))))
    (void))

  (edoc "Indent every line of the current buffer by the mode's indenter.")
  (define (indent-buffer!)
    (let ([n (vector-length (head:buffer-lines (head:window-buffer current-window)))])
      (when (indent-rows! 0 (- n 1))
        (set! message (format "Indented ~a lines" n))))
    (void))

  (define (replace-rows! from to lines . properties)
    ;; Replace rows [from, to] of the current buffer with lines (a
    ;; list), one undo entry; point keeps its row when it can.
    (define b (head:window-buffer current-window))
    (define v (car (edit-basis-for b)))
    (define n (vector-length v))
    (let ([nv (list->vector
                (let loop ([r 0] [acc '()])
                  (cond [(= r from)
                         (append (reverse acc) lines
                                 (let tail ([r (+ to 1)] [acc '()])
                                   (if (>= r n)
                                       (reverse acc)
                                       (tail (+ r 1)
                                             (cons (vector-ref v r) acc)))))]
                        [else (loop (+ r 1)
                                    (cons (vector-ref v r) acc))])))])
      (with-recorded-edit "format"
        (apply replace-buffer-lines! b (if (zero? (vector-length nv)) (vector "") nv) properties)
        (changed!))))

  (define (format-rows! from to)
    ;; Format rows [from, to] by the mode's formatter; -> whether the
    ;; buffer changed.
    (let ([entry (mode-entry formatters)])
      (cond
        [(not entry) (set! message "No formatter for this mode") #f]
        [else
         (let* ([b (head:window-buffer current-window)]
                [source (head:edit-basis b)]
                [v (car source)]
                [wanted (head:point)]
                [last (min to (- (vector-length v) 1))]
                [lines ((cadr entry) b from last)])
           (cond
             [(not lines) (set! message "Cannot format these lines") #f]
             [(and (or (< last (- (vector-length v) 1)) trailing-newline?)
                   (let same ([r from] [ls lines])
                     (if (null? ls)
                         (> r last)
                         (and (<= r last)
                              (string=? (car ls) (vector-ref v r))
                              (same (+ r 1) (cdr ls))))))
              (set! message "Already formatted") #f]
             [else
              ;; Text and its final-newline fact form one undoable
              ;; transaction, including a change only to that fact.
              (parameterize ([edit-source source] [edit-point wanted])
                (replace-rows! from last lines
                               (if (= last (- (vector-length v) 1)) '((trailing . #t)) '())))
              #t]))])))

  (edoc "Rewrite the lines between mark and point with the mode's formatter.")
  (define (format-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (format-rows! from to)
            (set! message "Formatted region"))))
    (void))

  (edoc "Rewrite the whole current buffer with the mode's formatter.")
  (define (format-buffer!)
    (let ([n (vector-length (head:buffer-lines (head:window-buffer current-window)))])
      (when (format-rows! 0 (- n 1))
        (set! message (format "Formatted ~a lines" n))))
    (void))

  ;;; Viewport commands -------------------------------------------------------

  ;; Painting and the frame are the painter's (paint); these are the
  ;; commands over its viewport logic -- paging and point placement --
  ;; and the head's side of the interaction protocol.

  (define (page-window! direction fraction)
    ;; Pagination is a viewport operation. Shift its top by the requested
    ;; fraction of the body height in visual rows, clamp at either end, then
    ;; put point in the middle.
    ;; A second outward page at an already-clamped edge moves point to that
    ;; edge. Wrapped segments count as rows; the visual column is preserved.
    (let* ([w current-window]
           [v (head:window-lines w)]
           [n (vector-length v)]
           [sticky (min (head:buffer-sticky-lines (head:current-buffer)) (- n 1))]
           [height (paint:page-size)]
           [wrapped? (paint:window-wrapped? w)]
           [visual-col (visual-column w point-row point-col)])
      (define (offset-at target segment)
        (let loop ([row sticky] [offset 0])
          (if (>= row target)
              (+ offset segment)
              (loop (+ row 1)
                    (+ offset (paint:line-segments w (vector-ref v row)))))))
      (define (position-at offset)
        (let loop ([row sticky] [left offset])
          (let ([segments (paint:line-segments w (vector-ref v row))])
            (if (or (= row (- n 1)) (< left segments))
                (cons row (min left (- segments 1)))
                (loop (+ row 1) (- left segments))))))
      (define (column-at position)
        (let* ([row (car position)]
               [line (vector-ref v row)])
          (paint:column-at-cell w row (and wrapped? (paint:line-breaks w line)) (cdr position) visual-col)))
      (define (land! top-offset point-offset)
        (let ([top (position-at top-offset)]
              [point (position-at point-offset)])
          (head:goto! (cons (car point) (column-at point)))
          (head:window-top-set! w (car top))
          (head:window-topseg-set! w (cdr top))))
      (let* ([total (max 1 (offset-at n 0))]
             [last-top (max 0 (- total height))]
             [old-top (min last-top
                           (max 0 (offset-at (head:window-top w)
                                             (head:window-topseg w))))]
             [up? (negative? direction)]
             [step (max 1 (quotient height fraction))]
             [at-edge? (= old-top (if up? 0 last-top))]
             [top (cond [(<= total height) 0]
                        [up? (max 0 (- old-top step))]
                        [else (min last-top (+ old-top step))])]
             [middle (+ top (quotient (- height 1) 2))]
             [point (cond [(<= total height) (if up? 0 (- total 1))]
                          [at-edge? (if up? 0 (- total 1))]
                          [else middle])])
        (land! top point))))

  (edoc "Scroll the selected window by a fraction of its height and put point in the middle: negative direction up, positive down; fraction 1 is a page, 8 an eighth."
        (direction integer "negative for up, positive for down")
        (fraction integer "the divisor of the window height"))
  (define (page-window-fraction! direction fraction)
    (page-window! direction fraction))

  (edoc "Place point at a (row . col) position, clamped into the window's text, leaving the viewport where it is."
        (position position "where point goes"))
  (define (set-point-without-scroll! position)
    (let* ([v (head:window-lines current-window)]
           [row (max 0 (min (car position) (- (vector-length v) 1)))])
      (head:window-prow-set! current-window row)
      (head:window-pcol-set! current-window
                             (max 0 (min (cdr position)
                                      (string-length (vector-ref v row)))))))


  ;; The head's side of the interaction protocol: another actor's
  ;; question waits in the echo area as an unlogged indicator until
  ;; C-c a answers it -- nobody's keyboard is stolen mid-thought.
  (edoc-type answer "an answer to the oldest pending question, one of its choices"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete (lambda (partial)
                (let ([asks (actor:pending head:ui-actor)])
                  (if (null? asks) '()
                      (let ([ask (car asks)])
                        (map (lambda (choice) (cons choice (caddr ask))) (cadddr ask)))))))
    (write (lambda (v) (format "~s" v)))
    (within string))

  (edoc "Answer the oldest question another actor posed through the interaction protocol; its choices complete."
        (choice answer "the answer"))
  (define (answer! choice)
    (let ([asks (actor:pending head:ui-actor)])
      (cond [(null? asks) (set! message "Nothing to answer")]
            [(actor:answer! (car (car asks)) choice) (set! message "Answered")]
            [else (set! message "That question was withdrawn")])))

  ;;; File commands -----------------------------------------------------------

  ;; The prompt -- the modal loop, completions, single-key questions --
  ;; lives in (prompt); the commands that ask are here.

  (define (file-prompt-styler label . directory)
    ;; Existence shown in the face, component-wise: the typed path's
    ;; longest leading run of components that exists on disk stays
    ;; upright, the rest leans italic -- so a TAB that landed on a
    ;; mere common prefix (no such file yet) is telling at a glance,
    ;; without another TAB to ask.
    (define (exists? p)
      (guard (ex [else #f])
        (file-exists? (file:expand (if (pair? directory) (file:absolute p (car directory)) p)))))
    (paint:prompt-styler label
      (lambda (path)
        (let* ([v (make-vector (string-length path) 'plain)]
               [split                    ; length of the existing prefix
                (let loop ([k (string-length path)])
                  (cond [(= k 0) 0]
                        [(exists? (substring path 0 k)) k]
                        [else (loop (let prev ([i (- k 2)])
                                      (cond [(< i 0) 0]
                                            [(char=? (string-ref path i) #\/)
                                             (+ i 1)]
                                            [else (prev (- i 1))])))]))])
          (style:fill-range! v split (string-length path) 'italic)
          v))))

  (define (file-completion-label path)
    (if (string:suffix? "/" path)
        (string-append (file:base-name (substring path 0 (- (string-length path) 1))) "/")
        (file:base-name path)))

  (edoc "Save the current buffer to its file; a buffer without one refuses and names save-file!, which takes a path."
        (returns boolean "whether the file was written")
        (prompts))
  (define (save!)
    (if file-name
        (save-file! file-name)
        (refuse-file! "This buffer has no file: (edit:save-file! path) saves it under one")))

  (define find-file-drafts (make-weak-eq-hashtable))

  (edoc "Read a file path with completion, history and validation, then visit it; with a directory procedure, read a path to create instead: missing parents and an empty file are made on disk, or just directories for a trailing slash, existing targets are refused, and a created directory goes to the procedure. An initial path seeds the prompt."
        (directory-action (or procedure #f) "what to do with a created directory; #f to visit instead")
        (initial (or string #f) "the path to start from; #f for the default directory")
        (prompts))
  (define (prompt-file! directory-action initial)
    ;; Validate/acquire while the path is still editable; show it only
    ;; after the temporary view has returned the window. Focus loss keeps
    ;; a per-window draft, while acceptance and explicit cancellation end it.
    ;; A browser's Create prompt makes missing parents and an empty file
    ;; (or just directories for a trailing slash), refusing existing targets.
    (unless (and (or (not directory-action) (procedure? directory-action))
                 (or (not initial) (string? initial)))
      (error 'prompt-file! "expected a directory action and an initial path" directory-action initial))
    (let* ([owner current-window] [before (head:current-buffer)]
           [saved (and (not initial) (hashtable-ref find-file-drafts owner #f))]
           [directory (if saved (car saved) (head:default-directory))]
           [draft (if saved (cdr saved) (box #f))]
           [label (if directory-action "Create file: " "Find file: ")]
           [ready #f])
      (define (resolve s) (file:absolute s directory))
      (define (complete s) (file:complete s directory))
      (define (normalize s)
        (if (and (not directory-action) (> (string-length s) 0) (not (string:suffix? "/" s))
                 (guard (ex [else #f]) (file-directory? (file:expand (resolve s)))))
            (string-append s "/") s))
      (define (validate s)
        (set! ready #f)
        (guard (ex [(head:interrupted? ex) "Interrupted; edit the path or try again"]
                   [(and directory-action (i/o-file-already-exists-error? ex))
                    (prompt:transient
                      (if (string:suffix? "/" s) "directory already exists" "file already exists"))]
                   [(i/o-file-protection-error? ex) "Permission denied"]
                   [(kernel:refusal? ex) (condition-message ex)]
                   [else (kernel:condition-text ex)])
          (if (string=? s "") #f
              (head:call-with-interrupt
                (lambda ()
                  (let* ([path (file:canonical (file:expand (resolve s)))]
                         [directory? (string:suffix? "/" s)])
                    (when directory-action
                      (file:make-directories! (file:directory-part path))
                      (file:create! (resolve s)))
                    (cond [directory?
                           (if (not directory-action)
                             (if (file-directory? path) "Directory; Tab to list files" "Not an existing directory")
                             (begin (set! ready (lambda () (directory-action path))) #f))]
                          [else (set! ready (prepare-file-visit path)) #f])))))))
      (let ([s (parameterize ([prompt:completion-label file-completion-label]
                              [paint:echo-highlight (file-prompt-styler label directory)]
                              [prompt:in-window #t] [prompt:validate validate] [prompt:draft draft])
                 (prompt:read! label complete (or initial directory)
                   (box (fold-right (lambda (path recent) (cons path (remove path recent)))
                          '() (log:history 'visit-file! cdr head:ui-actor)))
                   #f normalize))])
        (if (and (not s) (memq owner windows) (not (eq? owner current-window))
                 (eq? (head:window-buffer owner) before))
            (hashtable-set! find-file-drafts owner (cons directory draft))
            (hashtable-delete! find-file-drafts owner))
        (when (and s ready) (ready)))))

  (define (view-quit-buffers!)
    (let ([b (head:find-tool-buffer "*buffers*")])
      (if b
          (let ([w (window:display! b)])
            (when w
              (window:focus! w)
              (head:dispatch-app-event! "FOCUS")
              (set! message "")))
          (set-message! "The <buffers> app is not available"))))

  (edoc "Quit this head at once: shared text stays in the base, the screen is checkpointed for the next attach, and the exit notice names every buffer with unsaved work.")
  (define (quit!)
    (head:depart!))

  ;;; Pasting and typed runs --------------------------------------------------

  (define (paste-into-buffer!)
    ;; A bracketed paste: the whole text becomes one labeled edit, its
    ;; newlines becoming real line breaks.
    (let ([text (head:read-paste)])
      (unless (string=? text "")
        (call-as-one-edit! (format "insert ~s" text)
          (lambda ()
            (insert-text! (string:join (tty:paste-lines text) "\n")))))))

  ;; Consecutive typed characters coalesce into one undo entry (up to
  ;; twenty, as in Emacs), so undo removes the run, not one character.
  ;; The chain is (buffer row col run-length text): where the next typed
  ;; character must land to continue the run.  Any other command breaks
  ;; it: the chain only continues when the last command was this one.
  (define insert-chain #f)

  (define (self-insert-command!)
    ;; the key that reached no binding inserts itself (bound as
    ;; SELF-INSERT; the dispatcher leaves the key in head:current-keys)
    (self-insert! (tty:key-event-character (car (head:current-keys)))
                  (and (eq? (head:last-command) self-insert-command!)
                       insert-chain)))

  (define (self-insert! ch chain)
    (let ([b (head:window-buffer current-window)]
          [s (string ch)])
      (if (and chain
               (eq? (car chain) b)
               (= (cadr chain) point-row)
               (= (caddr chain) point-col)
               (< (cadddr chain) 20))
          (let ([text (string-append (list-ref chain 4) s)])
            (parameterize ([suppress-history #t])
              (insert-text-as! s (format "insert ~s" text)))
            (set! insert-chain
              (list b point-row point-col (+ (cadddr chain) 1) text)))
          (begin
            (insert-text! s)
            (set! insert-chain (list b point-row point-col 1 s))))))

  ;;; Mouse -------------------------------------------------------------------

  ;; SGR mouse tracking: clicks focus the window under the pointer and
  ;; place point at the clicked cell, dragging selects as though the
  ;; mark were set at the press (C-Space) and point moved, and the
  ;; wheel scrolls the window under the pointer, wherever the focus is.
  ;; The cost is the terminal's native mouse selection -- hold Shift
  ;; for that -- so mouse! turns the whole thing on or off at run time.
  (edoc "Turn mouse tracking on or off; off restores the terminal's native selection."
        (on boolean "whether to track the mouse"))
  (define (mouse! on)
    ;; Turn mouse tracking on or off (off restores native selection).
    (tty:mouse-reporting! on)
    (head:set-mouse-position! #f)
    (set! message (format "Mouse ~a" (if on "on" "off")))
    (void))

  ;; Hit-testing over the remembered tiling, and the gesture state,
  ;; live in (head); the actions they trigger stay here.
  (define-syntax mouse-gesture
    (identifier-syntax [id (head:drag)] [(set! id v) (head:set-drag! v)]))

  (define (text-gesture? w)
    (and (pair? mouse-gesture) (eq? (car mouse-gesture) w)
         (eq? (cdr mouse-gesture) (head:window-buffer w))))

  (define (word-char? c)
    (not (or (char-whitespace? c)
             (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\.)))))

  (define (select-word!)
    ;; Select the word point is on (or just after): mark at its start,
    ;; point at its end.
    (let* ([s (current-line)]
           [n (string-length s)]
           [on? (lambda (i)
                  (and (>= i 0) (< i n) (word-char? (string-ref s i))))]
           [col (cond [(on? point-col) point-col]
                      [(on? (- point-col 1)) (- point-col 1)]
                      [else #f])])
      (when col
        (set! mark-row point-row)
        (set! mark-col (let back ([i col])
                         (if (on? (- i 1)) (back (- i 1)) i)))
        (set! point-col (let fwd ([i col])
                          (if (on? i) (fwd (+ i 1)) i)))
        (set! mark-active? #t))))

  ;; Preserve the command-layer API; the head owns this delivery context.
  (define app-event-position head:app-event-position)
  (define app-event-button head:app-event-button)
  (define app-event-buffer-position head:app-event-buffer-position)
  (define (call-with-app-mouse-event w start height x y button thunk)
    ;; One coordinate boundary for clicks, drags, releases, and wheel ticks.
    ;; Exclude chrome from viewport cells; retain raw character positions
    ;; beyond text so apps can distinguish blank space from the last glyph.
    (parameterize
      ([app-event-position
        (cons (max 1 (- x (head:window-xoff w)
                        (if (eq? (head:window-scrollbar? w) 'left) 1 0)
                        (head:window-line-number-width w)))
              (max 1 (- y start)))]
       [app-event-buffer-position (paint:window-position w start height x y)]
       [app-event-button button])
      (thunk)))

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
    (set! mouse-gesture #f)
    (let ([double? (head:double-click? x y (real-time))])
      (define (arm-text-selection!)
        (set! mark-row point-row)
        (set! mark-col point-col)
        (set! mark-active? #f)
        (when double? (select-word!)))
      (cond
        [(head:window-button-at (- x 1) (- y 1)) =>
         (lambda (button)
           (let ([action (car button)] [w (cdr button)])
             (window:focus! w)
             (if (procedure? action) (action)
               (case action
                 [(below) (window:split-below!)]
                 [(right) (window:split-right!)]
                 [(close) (window:delete!)])))
           "MOUSE-HANDLED")]
        [(head:divider-at (- x 1) (- y 1)) =>
         (lambda (divider)
           ;; a below divider doubles as the upper window's status bar:
           ;; pressing it focuses that window, as any status bar does,
           ;; and still arms the drag
           (when (eq? (car divider) 'below)
             (head:window-at (- x 1) (- y 1)
               (lambda (entry) (window:focus! (car entry)))))
           (set! mouse-gesture divider)
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
                  (let ([old current-window])
                    (unless (head:app-buffer? (head:window-buffer w))
                      (window:focus! w))
                    (set! current-window w)
                    (when (and (head:app-buffer? (head:window-buffer w))
                               (memq old windows))
                      (set! current-window old)))
                  "MOUSE-HANDLED"]
                 [(head:app-buffer? (head:window-buffer w))
                  (let ([old current-window])
                    (set! current-window w)
                    (let ([old-point (head:point)]
                          [clicked (paint:window-position w start height x y)])
                      (head:goto! clicked)
                      (set! mark-active? #f)
                      (set! mouse-gesture (cons w (head:window-buffer w)))
                      ;; Focusing the clicked window is the default. An app may
                      ;; act on the click and explicitly preserve the old
                      ;; focus by returning keep-focus for MOUSE-CLICK.
                      (let ([result
                             (parameterize ([head:app-event-focus old])
                               (call-with-app-mouse-event w start height x y button
                                 (lambda () (head:dispatch-app-event! "MOUSE-CLICK"))))])
                        (cond [(eq? result 'ignore-click)
                               (set! mouse-gesture #f)
                               (head:goto! old-point)
                               (when (memq old windows)
                                 (set! current-window old))]
                              [(and (eq? result 'keep-focus) (memq old windows))
                               (set! current-window old)]
                              [(not result)
                               ;; Views and unhandled app text select like
                               ;; ordinary read-only buffer text. Arm the mark at
                               ;; this press instead of reusing stale state.
                               (arm-text-selection!)])))
                    "MOUSE-HANDLED")]
                 [else                                ; a text row
                  (window:focus! w)
                  (head:goto! (paint:window-position w start height x y))
                  (arm-text-selection!)
                  (set! mouse-gesture (cons w (head:window-buffer w)))
                  ;; A mode may act on the click -- following a link,
                  ;; say -- through a MOUSE-CLICK binding in its keymap.
                  (let ([context (mode:key-context (head:current-buffer))])
                    (when context
                      (let ([action (keymap:event-binding context
                                                          "MOUSE-CLICK")])
                        (when (procedure? action)
                          (guard (ex [else
                                      (set! message (kernel:condition-text ex))])
                            (action))))))
                  "MOUSE-HANDLED"]))))])))

  (define (mouse-drag! x y button)
    ;; A split-divider drag resizes its two subtrees; otherwise extend
    ;; the selection armed by the press --
    ;; the mark activates and point follows the pointer within the
    ;; focused window's text area.
    (cond
      [(and (pair? mouse-gesture) (memq (car mouse-gesture) '(right below)))
       (let* ([orientation (car mouse-gesture)]
              [split (cadr mouse-gesture)]
              [old (if (eq? orientation 'right)
                       (caddr mouse-gesture)
                       (cadddr mouse-gesture))]
              [now (if (eq? orientation 'right) (- x 1) (- y 1))]
              [delta (- now old)])
         (unless (= delta 0)
           (head:transfer-split! split delta)
           (if (eq? orientation 'right)
               (set-car! (cddr mouse-gesture) now)
               (set-car! (cdddr mouse-gesture) now))))]
      [else
       (head:window-at (- x 1) (- y 1)
         (lambda (entry)
           (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
             (when (and (eq? w current-window) (text-gesture? w)
                        (< (- y 1) (+ start height)))
               (head:goto! (paint:window-position w start height x y))
               (if (head:app-buffer? (head:window-buffer w))
                   (unless (call-with-app-mouse-event w start height x y button
                             (lambda () (head:dispatch-app-event! "MOUSE-DRAG")))
                     (set! mark-active? #t))
                   (set! mark-active? #t))))))]))

  (define (mouse-release! x y button)
    (head:window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
          (when (and (eq? w current-window) (text-gesture? w)
                     (< (- y 1) (+ start height))
                     (head:app-buffer? (head:window-buffer w)))
            (head:goto! (paint:window-position w start height x y))
            (call-with-app-mouse-event w start height x y button
              (lambda () (head:dispatch-app-event! "MOUSE-RELEASE"))))))))

  (define (mouse-wheel! x y button dir meta? shift?)
    ;; Scroll the window under the pointer; the focused window stays focused.
    ;; Meta-wheel
    ;; applies the corresponding global buffer-switch binding to the hovered
    ;; window instead. Apps get an ordinary directional tick first so list
    ;; controls can choose their wheel step.
    (head:window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([old current-window]
              [w (car entry)])
          (set! current-window w)
          (head:follow-app! w #f)
          (if (and meta? (memv dir '(0 1)))
              (dispatch:global-key! (if (= dir 0) "M-S-UP" "M-S-DOWN"))
              (begin
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
                  ((wheel-mover dir)))))
          (when (memq old windows) (set! current-window old))
          "MOUSE-HANDLED"))))

  (define (wheel-mover dir)
    ;; Wheel direction (the low bits of a 64-flagged button): up, down,
    ;; left, right. Vertical ticks move the hovered viewport by one eighth
    ;; of its height; horizontal ones move point sideways within its line.
    (case dir
      [(0) (lambda () (page-window! -1 8))]
      [(1) (lambda () (page-window! 1 8))]
      [(2) (lambda () (head:goto! (cons point-row (- point-col 3))))]
      [(3) (lambda () (head:goto! (cons point-row (+ point-col 3))))]
      [else (lambda () (void))]))

  ;; Input decoding lives in (tty): the head's reader thread calls
  ;; (tty:read-event stdin); the main thread applies the parsed mouse
  ;; data below, through the handler init! installs on the pump.

  (define hover-window #f)   ; the window whose local app last heard MOUSE-MOVE

  (define (tell-app! w event entry x y)
    ;; Deliver a pointer event to w's local app as the selected window,
    ;; then restore the selection: pointing focuses nothing.  Shared apps
    ;; are not told; their capture is for the keys and clicks the wire
    ;; carries.
    (when (and (memq w windows) (head:app-of (head:window-buffer w)))
      (let ([old current-window])
        (set! current-window w)
        (parameterize ([head:app-event-focus old])
          (if entry
              (call-with-app-mouse-event w (cadr entry) (caddr entry) x y 35
                (lambda () (head:dispatch-app-event! event)))
              (head:dispatch-app-event! event)))
        (when (memq old windows) (set! current-window old)))))

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
           (set! mouse-gesture #f)
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

  ;;; Small commands and key description -------------------------------------

  (edoc "Set the mark at point and activate it.")
  (define (set-mark-command!)
    (when (head:buffer-selectable? (head:current-buffer))
      (set! mark-row point-row) (set! mark-col point-col)
      (set! mark-active? #t))
    (set! message (if mark-active? "Mark set" "")))
  (edoc "Move point to the start of its line.")
  (define (beginning-of-line!)
    (set! point-col 0))
  (edoc "Move point to the end of its line.")
  (define (end-of-line!)
    (set! point-col (string-length (current-display-line))))
  (edoc "Deactivate the mark and abandon what was pending.")
  (define (keyboard-quit!)
    (set! mark-active? #f) (set! message "Quit"))
  (edoc "Erase and repaint the screen, asking the terminal for its color scheme again.")
  (define (redraw-command!)
    (tty:query-color-scheme!)
    (paint:mark-size-dirty!) (paint:erase-screen!) (set! message "Screen redrawn"))
  (edoc "Insert a line break after point, leaving point where it is.")
  (define (open-line!)
    (parameterize ([edit-point 'start]) (newline!)))
  (edoc "Scroll the selected window up by a page and put point in the middle; at the top, move point to the first line.")
  (define (page-up!)
    (page-window! -1 1))
  (edoc "Scroll the selected window down by a page and put point in the middle; at the bottom, move point to the last line.")
  (define (page-down!)
    (page-window! 1 1))
  (edoc "Move point up one line, or one visual row in a wrapping window, keeping the goal column.")
  (define (previous-line!)
    (move-vertical! -1))
  (edoc "Move point down one line, or one visual row in a wrapping window, keeping the goal column.")
  (define (next-line!)
    (move-vertical! 1))
  (edoc "Move point to the start of the buffer.")
  (define (beginning-of-buffer!)
    (set! point-row 0) (set! point-col 0))
  (edoc "Move point to the end of the buffer.")
  (define (end-of-buffer!)
    (set! point-row (- (vlen) 1))
    (set! point-col (string-length (current-display-line))))

  (define (binding-origin owned)
    (let ([owner (car owned)] [kind (keymap:binding-kind (cdr owned))])
      (cond [(eq? owner 'config) "config.e (user override)"]
            [owner (format "module ~a (~a)" owner kind)]
            [(eq? kind 'default) "built-in default"]
            [else "current session (user override)"])))

  (define (read-described-sequence)
    (let loop ([sequence (list (head:read-key-event #f))])
      (if (keymap:binding-prefix? 'global sequence)
          (begin
            (set! message (format "Describe key: ~a-" (keymap:sequence-text sequence)))
            (paint:redraw!)
            (loop (append sequence (list (head:read-key-event #f)))))
          sequence)))

  (edoc "Read a key sequence and show in the help buffer what it runs, who bound it and what it shadows."
        (prompts))
  (define (describe-key!)
    (parameterize ([message-source #f])
      (set-message! "Describe key: "))
    (paint:redraw!)
    (let* ([sequence (read-described-sequence)]
           [all (keymap:sequence-bindings sequence)]
           [entries (filter
                      (lambda (owned)
                        (eq? (keymap:binding-context (cdr owned)) 'global))
                      all)]
           [resolved (keymap:choose-binding entries)]
           [b (head:fresh-buffer! "*help*")])
      (head:buffer-append! b
        (keymap:sequence-text sequence)
        ""
        (if resolved
            (format "Resolved to: ~a" (keymap:action-text (keymap:binding-action (cdr resolved))))
            "Resolved to: self-insert or undefined")
        "Keymap: global"
        (if resolved
            (format "Defined by: ~a" (binding-origin resolved))
            "Defined by: fallback"))
      (when (> (length entries) 1)
        (head:buffer-append! b "" "Shadowed bindings:")
        (for-each
          (lambda (owned)
            (unless (eq? owned resolved)
              (head:buffer-append! b
                (format "  ~a — ~a"
                        (keymap:action-text (keymap:binding-action (cdr owned)))
                        (binding-origin owned)))))
          entries))
      (let ([contexts
             (fold-left
               (lambda (acc owned)
                 (let ([context (keymap:binding-context (cdr owned))])
                   (if (or (eq? context 'global) (memq context acc))
                       acc
                       (append acc (list context)))))
               '() all)])
        (when (pair? contexts)
          (head:buffer-append! b "" "Contextual bindings:")
          (for-each
            (lambda (context)
              (let ([hit (keymap:resolved-binding context sequence)])
                (when hit
                  (head:buffer-append! b
                    (format "  ~a: ~a — ~a"
                            context
                            (keymap:action-text (keymap:binding-action (cdr hit)))
                            (binding-origin hit))))))
            contexts)))
      (head:buffer-read-only-set! b #t)
      (set! message "")
      (unless (window:pop-up-or-reuse! b)
        (set-message! "The <help> buffer could not be displayed"))))

  ;;; Regions and the generic helpers ------------------------------------------

  (define (whole-buffer b)
    (let ([last (- (head:buffer-line-count b) 1)])
      (region b '(0 . 0)
              (cons last (string-length (head:buffer-line b last))))))

  (edoc "The text inside a region, rows joined with newlines."
        (r region "the region to read")
        (returns string))
  (define (region-text r)
    ;; The text inside r, rows joined with newlines.
    (let* ([b (region-buffer r)]
           [start (region-start r)]
           [end (region-end r)]
           [last (min (car end) (- (head:buffer-line-count b) 1))])
      (string:join
        (let loop ([row (max 0 (car start))] [acc '()])
          (if (> row last)
              (reverse acc)
              (let* ([s (head:buffer-line b row)]
                     [n (string-length s)]
                     [from (if (= row (car start)) (min (cdr start) n) 0)]
                     [to (if (= row (car end)) (min (cdr end) n) n)])
                (loop (+ row 1) (cons (substring s from (max from to)) acc)))))
        "\n")))

  ;;; Conflict resolution ---------------------------------------------------------

  ;; A merge left <<<<<<< buffer / ======= / >>>>>>> disk markers:
  ;; next-conflict! hops to one, keep-mine! and keep-disk! resolve the
  ;; conflict at point, each as one undo step.

  (define (conflict-marker? b row prefix)
    (and (>= row 0) (< row (head:buffer-line-count b))
         (string:prefix? prefix (head:buffer-line b row))))

  (define (conflict-at row)
    ;; The (start mid end) marker rows of the conflict containing row,
    ;; or #f.
    (let ([b (head:current-buffer)])
      (let up ([r row])
        (cond
          [(< r 0) #f]
          [(and (< r row) (conflict-marker? b r ">>>>>>>")) #f]
          [(conflict-marker? b r "<<<<<<<")
           (let mid ([m (+ r 1)])
             (cond
               [(>= m (head:buffer-line-count b)) #f]
               [(conflict-marker? b m "=======")
                (let end ([e (+ m 1)])
                  (cond
                    [(>= e (head:buffer-line-count b)) #f]
                    [(conflict-marker? b e ">>>>>>>")
                     (and (>= e row) (list r m e))]
                    [else (end (+ e 1))]))]
               [else (mid (+ m 1))]))]
          [else (up (- r 1))]))))

  (define (delete-rows! r1 r2)
    ;; Remove rows r1..r2 inclusive, joining across their newlines.
    (head:goto! (cons r1 0))
    (let ([n (let loop ([r r1] [n 0])
               (if (> r r2)
                   n
                   (loop (+ r 1)
                         (+ n 1 (string-length
                                  (head:buffer-line (head:current-buffer) r))))))])
      (do ([i 0 (+ i 1)]) ((= i n)) (delete-forward!))))

  (edoc "Move point to the next merge conflict marker, wrapping around at the end of the buffer.")
  (define (next-conflict!)
    ;; Point to the next conflict's <<<<<<< line, wrapping around.
    (let* ([b (head:current-buffer)]
           [n (head:buffer-line-count b)]
           [from (car (head:point))]
           [hit (let scan ([r (+ from 1)] [left n])
                  (cond [(zero? left) #f]
                        [(>= r n) (scan 0 left)]
                        [(conflict-marker? b r "<<<<<<<") r]
                        [else (scan (+ r 1) (- left 1))]))])
      (if hit
          (head:goto! (cons hit 0))
          (set-message! "No conflicts"))
      (void)))

  (edoc "Resolve the merge conflict at point in the buffer's favor, as one undo step.")
  (define (keep-mine!)
    ;; Resolve the conflict at point in the buffer's favor.
    (let ([c (conflict-at (car (head:point)))])
      (if c
          (begin
            (call-as-one-edit! "keep mine"
              (lambda ()
                (delete-rows! (cadr c) (caddr c))
                (delete-rows! (car c) (car c))
                (head:goto! (cons (car c) 0))))
            (set-message! "Kept the buffer side"))
          (set-message! "Not in a conflict"))
      (void)))

  (edoc "Resolve the merge conflict at point in the disk's favor, as one undo step.")
  (define (keep-disk!)
    ;; Resolve the conflict at point in the disk's favor.
    (let ([c (conflict-at (car (head:point)))])
      (if c
          (begin
            (call-as-one-edit! "keep disk"
              (lambda ()
                (delete-rows! (caddr c) (caddr c))
                (delete-rows! (car c) (cadr c))
                (head:goto! (cons (car c) 0))))
            (set-message! "Kept the disk side"))
          (set-message! "Not in a conflict"))
      (void)))

  ;;; Registration ----------------------------------------------------------------

  ;; Everything the layer registers -- owned by edit, so a reload
  ;; retracts and remakes it; what the loop and the seams ask of the
  ;; commands is installed here too.
  (edoc "Install the command layer: log presentation, the file formatters, status hints, the mouse handler, the default key bindings, the loop's hooks and the buffers app.")
  (define (init!)
    ;; One module-owned subscriber per head. All records wake its shared
    ;; history view; echo presentation belongs to the originating head.
    ;; Presentation mode is captured with the record, not read on delivery.
    (log:subscribe!
      (lambda (e presentation)
        (head:wake-main!)
        (when (and presentation (equal? (log:actor e) head:ui-actor))
          (if (eq? presentation 'progress)
              (paint:echo-append! (log:component e) (log:format-entry e)
                                  (log:styler (log:component e)) #t)
              (present-log-entry! e)))))
    ;; The file commands' formatters: their entries are (verb . path),
    ;; formatted "verb path", their histories the paths (see
    ;; log:history).
    (let ([fmt (lambda (d)
                 (if (pair? d)
                     (format "~a ~a" (car d) (cdr d))
                     (format "~a" d)))])
      (log:register-formatter! 'visit-file! fmt)
      (log:register-formatter! 'save-file! fmt))
    ;; the status line shows a merge's conflicts as a hint the files code
    ;; owns -- painting knows nothing about merges
    (paint:add-buffer-status-hint!
      (lambda (b active?)
        (and (assq b merge-reports)
             (let ([n (buffer-conflict-count b)])
               (and (> n 0)
                    (list (cons (format "  ~a conflict~a" n (if (= n 1) "" "s"))
                                'red)))))))
    (style:set-changed-hook!
      (lambda () (paint:invalidate-screen-cache!)))
    (style:color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! style:color-scheme!)
    (head:add-shutdown-hook! (lambda () (head:flush-ui-audit! 'all)))
    ;; The pump lives in (head); its mouse report handler is the
    ;; commands' and is installed here.
    (head:set-mouse-handler! apply-mouse-event!)
    ;; The layer's default bindings are data, like every module's.
    (begin
      (for-each
        (lambda (entry) (keymap:bind-default! (car entry) (cadr entry)))
        `(("C-@" ,set-mark-command!) ("C-a" ,beginning-of-line!)
          ("C-b" ,move-left!) ("C-d" ,delete-forward!)
          ("C-e" ,end-of-line!) ("C-f" ,move-right!)
          ("C-g" ,keyboard-quit!) ("ESC" ,keyboard-quit!)
          ("BACKSPACE" ,backspace!)
          ("TAB" ,indent-tab!) ("RET" ,newline!) ("C-k" ,kill-line!)
          ("C-l" ,redraw-command!) ("C-n" ,next-line!)
          ("C-o" ,open-line!) ("C-p" ,previous-line!)
          ("C-v" ,page-down!) ("C-w" ,kill-region!) ("C-y" ,yank!)
          ("C-_" ,undo!) ("C-M-_" ,redo!) ("M-w" ,copy-region!)
          ("M-v" ,page-up!) ("M-<" ,beginning-of-buffer!)
          ("M->" ,end-of-buffer!) ("UP" ,previous-line!)
          ("DOWN" ,next-line!) ("LEFT" ,move-left!)
          ("RIGHT" ,move-right!) ("HOME" ,beginning-of-line!)
          ("END" ,end-of-line!) ("DELETE" ,delete-forward!)
          ("PAGEUP" ,page-up!) ("PAGEDOWN" ,page-down!)
          ("PASTE" ,paste-into-buffer!) ("SELF-INSERT" ,self-insert-command!)
          ("C-x C-g" ,keyboard-quit!) ("C-x C-s" ,save!)
          ("C-x C-w" ,(keymap:prefill save-file!)) ("C-x C-c" ,quit!)
          ("C-x k" ,(keymap:call kill-buffer! head:current-buffer))
          ("C-h k" ,describe-key!)
          ("C-c a" ,(keymap:prefill answer!))))
      (for-each
        (lambda (entry)
          (keymap:bind-default! 'prompt (car entry) (cadr entry)))
        '(("C-g" cancel) ("ESC" cancel) ("RET" accept)
          ("C-a" beginning) ("HOME" beginning)
          ("C-b" backward) ("LEFT" backward)
          ("C-e" end) ("END" end) ("C-f" forward) ("RIGHT" forward)
          ("UP" up) ("DOWN" down) ("C-d" delete-forward)
          ("DELETE" delete-forward) ("C-h" delete-backward)
          ("BACKSPACE" delete-backward) ("C-k" kill) ("C-y" yank)
          ("TAB" complete) ("S-TAB" alternate-complete)
          ("M-." inspect) ("M-RET" newline) ("PASTE" paste)))
      #t)
    ;; The loop's hooks live in (head): how to open the file
    ;; argument, how to quit (the modified-buffers check), and what runs
    ;; after every key
    (begin
      (head:set-file-opener! visit-file!)
      (head:set-quit-command! quit!)
      (head:set-review-viewer! view-quit-buffers!)
      (head:set-after-key! clamp-point!))

    (doc:register!
      '(((undo-scope) (("parameter" . "(undo-scope [scope])")) "symbol"
         ("(edit)") edit "Editing commands" #f
         "Choose the default scope of `undo!` and C-_. `mine` (the default) selects this head's latest live action; `all` selects the latest live action of any actor. The preference belongs to the head. Local buffers use their own history in either mode.")
        ((undo!) (("procedure" . "(undo!)")) "string"
         ("(edit)") edit "Editing commands" #f
         "Undo one action in the current buffer within `undo-scope`, `mine` or `all`. Shared changes use attributed inverse edits; an overlap, changed text property, or unavailable history refuses without changing any part of the action.")
        ((redo!) (("procedure" . "(redo!)")) "string"
         ("(edit)") edit "Editing commands" #f
         "Reverse this head's latest undo, including an undo of another actor's action. Redo uses the same overlap checks and is independent of `undo-scope`. A fresh edit by this head invalidates its redo.")
        ((undo-actor!) (("procedure" . "(undo-actor! actor)")) "string"
         ("(edit)") edit "Editing commands" #f
         "Undo the named actor's latest live action in the current shared buffer without changing `undo-scope`. Both the original author and this head's request are retained in the history and audit log.")
        ((next-conflict!) (("procedure" . "(next-conflict!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Move point to the next merge conflict marker in the current buffer, wrapping at the end. Report a message if the buffer has no conflicts.")
        ((keep-mine!) (("procedure" . "(keep-mine!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Resolve the merge conflict at point by keeping the buffer side. The complete resolution is one undo step.")
        ((keep-disk!) (("procedure" . "(keep-disk!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Resolve the merge conflict at point by keeping the disk side. The complete resolution is one undo step.")))
    (keymap:bind-default! "M-n" next-conflict!)
    (keymap:bind-default! "M-m" keep-mine!)
    (keymap:bind-default! "M-d" keep-disk!)
  )

) ;; library (edit)
