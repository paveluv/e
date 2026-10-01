;; edit.sls -- the canonical editing API. Current-window mutation commands
;; delegate to the same editor views that nested hosts use.
;;
;; Everything a user does to text and to the seat that shows it: the
;; buffer commands (the window commands are (window)'s), visiting,
;; saving, reloading from the disk,
;; editing with undo, the copy buffer and the clipboard, indentation and
;; formatting through the modes' registered indenters,
;; the default key bindings, and the generic editing helpers (regions;
;; search and replace are (search)'s, the log's views and conflicts (delta-log)'s).  It
;; composes the seams below --
;; store, head, paint, prompt, file, mode, keymap -- and is what M-x
;; sees as edit: calls: the loader imports this library with its prefix.
;;
;; Hot-reloadable like any module: its registrations (bindings, hooks,
;; formatters, descriptions) are made in init!, owned by edit, so a
;; reload retracts and remakes them; the loop in (main) reaches the
;; layer through hooks it installs in (head).  Internals -- all
;; mutable state included -- are invisible outside the library; the
;; exports are the editor's public command API.
;;
;; The generic helpers act on the selected region, else on the whole
;; current buffer -- current-region; with-region and head:with-buffer-mirror
;; retarget them for the extent of a body.  A region is a slice of one
;; buffer between two (row . col) points: '(region (buffer 7) (0 . 0) (12 . 5)).

(import (only (foundation edoc) elibrary))
(elibrary (head edit)
  (export answer! backspace! backups backward-expression! backward-kill-expression! basis beginning-of-buffer! beginning-of-form!
          beginning-of-line! buffer-clean? buffer-text
          call-as-one-edit! copy-region! copy-text copy-text! (rename (editor:create-view! create-view!)) current-batch current-region
          (rename (editor:delete! delete!)) delete-forward! delete-trashed! down-expression! empty-trash! end-of-buffer! end-of-form! end-of-line! format-buffer!
          format-region!
          forward-copy-buffer-to-system-clipboard forward-expression! indent-buffer! indent-expression! indent-line!
          indent-region!
          indent-tab! init! (rename (editor:insert! insert!)) insert-text! keyboard-quit! kill-buffer! kill-expression! kill-line! kill-region!
          mark-expression! mark-form!
          message-progress (rename (editor:move! move!)) move-horizontal! move-left! move-right! move-vertical!
          new-buffer! newline! next-line! next-list! open-line! page! page-down! page-up!
          (rename (paste-into-buffer! paste!)) present-log-entries! present-log-entry! previous-line!
          previous-list!
          quit! redo! redraw-command! region-text (rename (text-control:register-policy! register-policy!)) reload! replace-region-text! reread! restore!
          rewrite-regions! save! save-file! (rename (editor:scroll! scroll!) (editor:select! select!) (editor:set-mark! set-mark!)) set-mark-command! set-message!
          transpose-expressions! trash type! undo! undo-actor! (rename (text-control:undo-scope undo-scope)) up-expression!
          visit-file! with-region
          yank!)
  (import (chezscheme)
          (prefix (core handle) handle:)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (core region) region:)
          (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head catalogue-host) catalogue-host:)
          (prefix (head dispatch) dispatch:)
          (prefix (head echo) echo:)
          (prefix (head editor) editor:)
          (prefix (head expression) expression:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head render) render:)
          (prefix (head style) style:)
          (prefix (head table) table:)
          (prefix (head text-control) text-control:)
          (prefix (head text-layout) text-layout:)
          (prefix (head text-source) text-source:)
          (prefix (head window) window:)
          (prefix (service document) document:)
          (prefix (service file) file:)
          (prefix (service log) log:)
          (prefix (state actor) actor:)
          (prefix (state store) store:)
          (prefix (sys glyph) glyph:)
          (prefix (sys sys) sys:)
          (prefix (sys tty) tty:))

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

  (define-state file-name (head:window-buffer current-window)
    head:buffer-file head:buffer-file-set!)
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

  (edoc "Borrow immutable text with its document identity and revision for a later bulk rewrite."
        (id model "explicit editor view; omission uses the legacy current buffer") (returns list "(lines document-id revision)"))
  (define basis
    (case-lambda [() (head:edit-basis (head:current-buffer-mirror))] [(id) (editor:basis id)]))

  ;;; Small utilities -------------------------------------------------------





  ;;; Buffer access and undo ------------------------------------------------

  (define (vlen) (head:buffer-line-count (head:current-buffer-mirror)))
  (define (line-at n) (head:buffer-line (head:current-buffer-mirror) n))
  ;; Navigation addresses the window presentation; editing addresses source.
  (define (current-display-line) (render:line-ref (head:window-text current-window) point-row))


  (define (check-editable!)
    ;; The same guard protects fresh edits and undo: #t forbids all edits,
    ;; and a procedure decides per edit. A local buffer, a view or a tool
    ;; of this head's, is never edited: every text edited is the base's.
    (let* ([b (head:window-buffer current-window)] [guard (head:buffer-read-only b)])
      ;; the pop-up shows; what it shows is edited in a window of its own
      (when (head:popup? current-window)
        (raise (condition (kernel:make-read-only-error)
                          (make-message-condition "the pop-up is read-only"))))
      (unless (head:buffer-store-id b)
        (raise (condition (kernel:make-read-only-error)
                          (make-message-condition "local buffers are read-only"))))
      (when (if (procedure? guard) (not (guard)) guard)
        (raise (condition (kernel:make-read-only-error)
                          (make-message-condition "buffer is read-only"))))))

  (edoc "The batch label of the edits in the current one-edit group, the (actor token) pair they share in the delta log, unique across head reattachments, or #f outside a group."
        (returns (or list #f)) (public))
  (define (current-batch) (text-source:current-batch head:ui-actor))

  (edoc "Bundle every edit the thunk makes into one labeled undo step per buffer it touches; nested groups defer to the outermost."
        (label (or string #f) "the undo label")
        (thunk thunk "the edits to group")
        (returns any "what the thunk returns"))
  (define (call-as-one-edit! label thunk) (text-source:call-grouped! head:ui-actor label thunk))

  (define (no-history verb)
    (format "No further ~a information" (string-downcase verb)))

  (define (history-shift! direction verb scope)
    (let-values ([(status detail) (editor:history! (require-editor) direction scope)])
      (set! message
        (case status
          [(nothing) (no-history verb)]
          [(applied) (string:elide (format "~a ~s: ~a" verb (caddr detail) (or (list-ref detail 4) "edit")) cols)]
          [else (format "~a blocked: ~a" verb
                  (case detail
                    [(read-only) "the buffer is read-only"] [(basis-too-old) "history is incomplete"]
                    [(overlap) "another edit overlaps this action"] [(property-changed) "a text property changed after this action"]
                    [else "the store is unavailable"]))])) message))

  (edoc "Undo one action within undo-scope. An explicit editor view returns journal status and detail; omitting it uses the current window and echo report."
        (id model "editor view; omission addresses the current window")
        (edits))
  (define undo!
    (case-lambda
      [() (history-shift! 'undo "Undo" (text-control:undo-scope))]
      [(id) (editor:history! id 'undo (text-control:undo-scope))]))

  (edoc "Reverse this head's latest undo in an explicit editor view, returning journal status and detail. Omitting the view uses the current window and echo report."
        (id model "editor view; omission addresses the current window")
        (edits))
  (define redo!
    (case-lambda
      [() (history-shift! 'redo "Redo" 'mine)]
      [(id) (editor:history! id 'redo 'mine)]))

  (edoc "Undo an actor's latest live action in an explicit editor view, returning journal status and detail. Omitting the view uses the current window and echo report."
        (who actor "the actor's identity")
        (id model "editor view; omission addresses the current window")
        (edits) (public))
  (define undo-actor!
    (case-lambda
      [(who) (history-shift! 'undo "Undo" (list 'actor who))]
      [(who id) (editor:history! id 'undo (list 'actor who))]))

  ;;; Point, mark, and editing ----------------------------------------------

  (define (ordered-region) ; -> start-row start-col end-row end-col
    (if (or (< point-row mark-row)
            (and (= point-row mark-row) (< point-col mark-col)))
        (values point-row point-col mark-row mark-col)
        (values mark-row mark-col point-row point-col)))

  (define (clamp-point!)
    (set! point-row (max 0 (min point-row (- (vlen) 1))))
    (set! point-col (max 0 (min point-col (string-length (current-display-line))))))

  (edoc "Move point forward over one expression: the atom around point, else the next expression inside the enclosing one; the C-M-f of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define forward-expression!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (forward-expression! id)
           (let-values ([(start end) (expression:forward (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if end (head:goto! end) (set-message! "No expression after point")))))]
      [(id) (editor:expression! id 'forward)]))

  (edoc "Move point backward over one expression: the atom around point, else the last expression ending by it inside the enclosing one; the C-M-b of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define backward-expression!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (backward-expression! id)
           (let-values ([(start end) (expression:backward (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if start (head:goto! start) (set-message! "No expression before point")))))]
      [(id) (editor:expression! id 'backward)]))

  (define (position-before? a b)
    (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (edoc "Kill from point to the end of the next expression into the copy buffer; consecutive kills accumulate; the C-M-k of Emacs."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define kill-expression!
    (case-lambda
      [() (kill-expression! (require-editor))]
      [(id) (editor:transfer! id 'forward publish-view-kill!)]))

  (edoc "Kill from the start of the expression before point to point into the copy buffer, ahead of a preceding kill; the C-M-BACKSPACE of Emacs."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define backward-kill-expression!
    (case-lambda
      [() (backward-kill-expression! (require-editor))]
      [(id)
       (editor:transfer!
         id
         'backward
         (lambda (text accumulate?)
           (publish-view-kill! text accumulate? #t)))]))

  (edoc "Set the mark at the end of the next expression and activate it, point staying; with the mark active beyond point, extend it by one more expression; the C-M-SPC of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define mark-expression!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (mark-expression! id)
           (let* ([point (head:point)] [mark (cons mark-row mark-col)]
                  [from (if (and mark-active? (position-before? point mark)) mark point)])
             (let-values ([(start end) (expression:forward (head:buffer-lines (head:current-buffer-mirror)) from)])
               (cond [(not end) (set-message! "No expression after point")]
                 [(head:buffer-selectable? (head:current-buffer-mirror))
                  (set! mark-row (car end)) (set! mark-col (cdr end)) (set! mark-active? #t)
                  (set! message "Mark set")])))))]
      [(id) (editor:expression! id 'mark)]))

  (edoc "Mark the top-level form around point: point at its start, the mark at its end; the C-M-h of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define mark-form!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (mark-form! id)
           (let-values ([(start end) (expression:top-level (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (cond [(not start) (set-message! "No top-level form in the buffer")]
               [(head:buffer-selectable? (head:current-buffer-mirror))
                (head:goto! start)
                (set! mark-row (car end)) (set! mark-col (cdr end)) (set! mark-active? #t)
                (set! message "Mark set")]))))]
      [(id) (editor:expression! id 'form)]))

  (edoc "Move point up out of the enclosing list or vector, to its start; the C-M-u of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define up-expression!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (up-expression! id)
           (let-values ([(start end) (expression:container (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if start (head:goto! start) (set-message! "Not inside an expression")))))]
      [(id) (editor:expression! id 'up)]))

  (edoc "Move point down into the next list or vector, just past its opening delimiter; the C-M-d of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define down-expression!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (down-expression! id)
           (let ([inside (expression:down (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if inside (head:goto! inside) (set-message! "No list after point")))))]
      [(id) (editor:expression! id 'down)]))

  (edoc "Move point over the next list or vector, skipping atoms; the C-M-n of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define next-list!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (next-list! id)
           (let-values ([(start end) (expression:next-list (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if end (head:goto! end) (set-message! "No list after point")))))]
      [(id) (editor:expression! id 'next)]))

  (edoc "Move point back over the previous list or vector, skipping atoms; the C-M-p of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define previous-list!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (previous-list! id)
           (let-values ([(start end) (expression:previous-list (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if start (head:goto! start) (set-message! "No list before point")))))]
      [(id) (editor:expression! id 'previous)]))

  (edoc "Move point to the start of the last top-level form beginning before point, the enclosing one included; the C-M-a of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define beginning-of-form!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (beginning-of-form! id)
           (let ([start (expression:form-start (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if start (head:goto! start) (set-message! "No top-level form before point")))))]
      [(id) (editor:expression! id 'start)]))

  (edoc "Move point to the end of the first top-level form ending after point, the enclosing one included; the C-M-e of Emacs."
        (id model "editor view; omission addresses the current window"))
  (define end-of-form!
    (case-lambda
      [()
       (let ([id (current-editor)])
         (if id (end-of-form! id)
           (let ([end (expression:form-end (head:buffer-lines (head:current-buffer-mirror)) (head:point))])
             (if end (head:goto! end) (set-message! "No top-level form after point")))))]
      [(id) (editor:expression! id 'end)]))

  (edoc "Swap the expression before point with the one after it, point ending after both; the C-M-t of Emacs."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define transpose-expressions!
    (case-lambda
      [() (transpose-expressions! (require-editor))]
      [(id) (editor:expression! id 'transpose)]))

  (edoc "Indent the lines of the next expression after its first by the mode's indenter; the C-M-q of Emacs."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define indent-expression!
    (case-lambda
      [() (indent-expression! (require-editor))]
      [(id) (editor:format! id 'indent-expression)]))

  (define (require-editor)
    (check-editable!)
    (or (current-editor) (refuse! "The current window has no mounted editor")))

  (define (current-editor)
    (and (head:window-widget current-window) (head:window-editor current-window)))

  (edoc "Move point one character left, crossing to the end of the previous line.")
  (define (move-left!)
    (let ([id (current-editor)])
      (if id (editor:move! id 'left)
        (cond [(> point-col 0) (set! point-col (- point-col 1))]
          [(> point-row 0)
           (set! point-row (- point-row 1))
           (set! point-col (string-length (current-display-line)))]))))

  (edoc "Move point one character right, crossing to the start of the next line.")
  (define (move-right!)
    (let ([id (current-editor)])
      (if id (editor:move! id 'right)
        (cond [(< point-col (string-length (current-display-line)))
               (set! point-col (+ point-col 1))]
          [(< point-row (- (vlen) 1))
           (set! point-row (+ point-row 1)) (set! point-col 0)]))))

  (edoc "Move point a number of characters, negative to the left, crossing line ends as single steps do."
        (delta integer "how far, negative for left") (public))
  (define (move-horizontal! delta)
    ;; Move point delta characters, negative to the left, crossing line
    ;; ends the way repeated single steps do.
    (unless (and (integer? delta) (exact? delta)) (error 'move-horizontal! "expected an exact integer" delta))
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
    (let ([b (head:current-buffer-mirror)])
      (list current-window b (head:buffer-revision b)
            (head:window-text current-window)
            (and wrapped? (paint:wrap-width current-window))
            (render:header (head:window-rendition current-window)) point-row point-col)))

  (define (visual-column w row col)
    (let* ([frame (head:window-rendition w)]
           [breaks (and (paint:window-wrapped? w)
                        (paint:line-breaks w (render:line-ref (head:window-text w) row)))])
      (- (render:column frame row col)
         (if breaks
             (render:column frame row (vector-ref breaks (text-layout:segment breaks col))) 0))))

  (edoc "Move point a number of lines, negative for up, aiming for the goal column; visual rows in a wrapping window."
        (delta integer "how far, negative for up"))
  (define (move-vertical! delta)
    (unless (and (integer? delta) (exact? delta)) (error 'move-vertical! "expected an exact integer" delta))
    (let ([id (current-editor)])
      (if id (do ([i (abs delta) (- i 1)]) ((zero? i)) (editor:move! id (if (< delta 0) 'up 'down)))
        (let* ([w current-window] [wrapped? (paint:window-wrapped? w)] [goal (head:window-goal w)]
               [goal-col (if (and goal (equal? (cdr goal) (goal-position wrapped?))) (car goal)
                             (visual-column w point-row point-col))]
               [point (text-layout:move (head:window-text w) (head:window-rendition w)
                        (and wrapped? (paint:wrap-width w)) (cons point-row point-col) delta goal-col)])
          (set! point-row (car point)) (set! point-col (cdr point))
          (head:window-goal-set! w (cons goal-col (goal-position wrapped?)))))))

  (edoc "Insert text at point as one undo entry; its newlines become line breaks."
        (s string "the text to insert")
        (edits))
  (define (insert-text! s)
    (insert-text-as! s (format "insert ~s" s)))

  (define (insert-text-as! s label)
    (unless (string=? s "")
      (call-as-one-edit! label (lambda () (editor:insert-at! (require-editor) s #f)))))

  (edoc "Insert a line break at point."
        (edits))
  (define (newline!)
    (insert-text-as! "\n" "newline"))

  (edoc "Delete the character after point, or join the next line at a line end; a run of typing, backspaces and deletes is one undo step."
        (edits))
  (define (delete-forward!) (editor:delete! (require-editor) 'forward))

  (edoc "Delete the character before point, or join with the previous line at a line start; a run of typing, backspaces and deletes is one undo step."
        (edits))
  (define (backspace!) (editor:delete! (require-editor) 'backward))

  ;;; Kill and yank ---------------------------------------------------------

  (edoc "Whether every kill and copy, and every other change to *copy*, also reaches the terminal's clipboard, through OSC 52."
        (value boolean))
  (define forward-copy-buffer-to-system-clipboard (make-parameter
                                                    #f
                                                    (lambda (enabled?)
                                                      (unless (boolean? enabled?)
                                                        (error 'forward-copy-buffer-to-system-clipboard
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
    (when (and (forward-copy-buffer-to-system-clipboard) (paint:screen-live?))
      (with-mutex paint:redraw-lock
        (paint:ansi! "\x1b;]52;c;" (base64-encode (string->utf8 text)) "\x1b;\\")
        (flush-output-port (sys:terminal-output-port)))))

  ;; Whatever changes *copy* -- a hand edit there, undo, the prompt's C-k --
  ;; reaches the clipboard at the next frame, once per revision; the copy
  ;; commands publish at once and note their revision. A copy buffer seen
  ;; for the first time is only noted, so a fresh or resumed head never
  ;; writes the clipboard by itself.
  (define published-copy (cons #f #f)) ; (buffer . the revision published)

  (define (copy-revision b)
    (let-values ([(lines revision facts) (head:buffer-state b)]) revision))

  (define (note-copy-published! b)
    (set! published-copy (cons b (copy-revision b))))

  (define (publish-copy-changes!)
    (let ([b (head:copy-buffer #f)])
      (when b
        (let ([revision (copy-revision b)])
          (cond [(not (eq? b (car published-copy))) (set! published-copy (cons b revision))]
                [(eqv? revision (cdr published-copy)) (void)]
                [else
                 (set! published-copy (cons b revision))
                 (when (forward-copy-buffer-to-system-clipboard)
                   (publish-system-clipboard! (head:copy-text)))])))))

  (define (replace-copy-text! text label)
    (let* ([b (head:copy-buffer)] [basis (head:edit-basis b)]
           [key (list head:ui-actor (gensym->unique-string (gensym "copy")))])
      (let-values ([(lines trailing?) (text:from-string text)])
        (let-values ([(span replacement) (text:difference (car basis) lines)])
          (head:store-edit! b span replacement
            (list key (format "~a ~s" label (string:elide text 40))
              (list 'undo (cons 'trailing trailing?)) (list 'labels (cons 'batch key)))
            (map (lambda (w) (cons w 'end)) (filter (lambda (w) (eq? (head:window-buffer w) b)) (head:windows))) basis)))
      (note-copy-published! b) (publish-system-clipboard! text)))

  (define (killing?)
    ;; was the previous command a kill?  Consecutive kills accumulate
    ;; into a single copy-buffer entry.
    (let* ([action (head:last-command)] [procedure (if (keymap:call-action? action) (keymap:call-action-procedure action) action)])
      (and (memq procedure (list kill-line! kill-region! kill-expression! backward-kill-expression!)) #t)))

  (define (publish-view-kill! text accumulate? . prepend?)
    (replace-copy-text! (if (and accumulate? (killing?))
                          (if (and (pair? prepend?) (car prepend?)) (string-append text (copy-text)) (string-append (copy-text) text)) text) "kill"))

  (edoc "Copy text into the copy buffer without changing a buffer or point; C-y pastes it."
        (text string "the text to copy"))
  (define (copy-text! text)
    (unless (string? text)
      (error 'copy-text! "expected a string" text))
    (replace-copy-text! text "copy")
    (void))

  (edoc "Kill from point to the end of the line, or the line break when point is at the end; consecutive kills accumulate."
        (id model "editor view; omission addresses the current window")
        (edits))
  (define kill-line!
    (case-lambda
      [() (kill-line! (require-editor))]
      [(id) (editor:transfer! id 'line publish-view-kill!)]))

  (edoc "The copy buffer's text."
        (returns string))
  (define (copy-text)
    (head:copy-text))

  (edoc "Insert the copy buffer's text at point."
        (id model "editor view; omission addresses the current window")
        (edits))
  (define yank!
    (case-lambda
      [() (yank! (require-editor))]
      [(id)
       (let ([text (copy-text)])
         (unless (string=? text "") (editor:paste! id text)))]))

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

  (edoc "Replace the text between two ordered points with new text, in one structural edit."
        (id model "editor view; omission addresses the current window")
        (start position "where the replaced text starts")
        (end position "where it ends")
        (text string "the replacement")
        (edits) (public))
  (define replace-region-text!
    (case-lambda
      [(start end text)
       (replace-region-text! (require-editor) start end text)]
      [(id start end text)
       (editor:replace-region! id start end text)]))

  (edoc "Rewrite ordered disjoint ranges computed against edit:basis. An explicit editor preserves its selection and groups accepted replacements into one undo action, applying them from the end so earlier coordinates stay stable. Ranges changed concurrently are skipped; lost history or ownership refuses the remaining work. Omitting the view addresses the current window."
        (id model "explicit editor view; omission addresses the current window")
        (basis list "the edit basis the ranges were computed against")
        (regions (list-of list) "(start end text) each, in the text's order")
        (returns integer "how many ranges were replaced")
        (edits))
  (define rewrite-regions!
    (case-lambda
      [(id basis regions)
       (editor:rewrite-regions! id basis regions)]
      [(basis regions)
       (rewrite-regions! (require-editor) basis regions)]))

  (edoc "Copy the text between mark and point to the copy buffer without deleting it; the mark deactivates. An explicit view refuses if the selected text changed."
        (id model "editor view; omission addresses the current window"))
  (define copy-region!
    (case-lambda
      [()
       ;; Save the region to the copy buffer without deleting it -- M-w, as
       ;; in Emacs.  The mark deactivates; C-y reinserts.
       (if (not mark-active?)
         (set! message "The mark is not set now")
         (let-values ([(sr sc er ec) (ordered-region)])
           (if (and (= sr er) (= sc ec))
             (set! message "Empty region")
             (begin
               (copy-text! (text-between sr sc er ec))
               (set! mark-active? #f)
               (set! message "Copied")))))]
      [(id) (editor:transfer! id 'copy (lambda (text accumulate?) (copy-text! text)))]))

  (edoc "Kill the text between mark and point into the copy buffer."
        (id model "editor view; omission addresses the current window")
        (edits))
  (define kill-region!
    (case-lambda
      [() (kill-region! (require-editor))]
      [(id) (editor:transfer! id 'cut publish-view-kill!)]))

  ;;; Files -----------------------------------------------------------------

  (edoc "Visit a file through the base, creating it and its missing parents if needed; a trailing slash creates/navigates a directory. Reopening preserves shared edits and merges disk changes undoably. An explicit destination receives directory/path or buffer/adopted-buffer; otherwise use this head's current window."
        (path file "the path to visit")
        (destination procedure "optional placement callback")
        (proposal any "optional Finder creation witness")
        (returns boolean))
  (define visit-file!
    (case-lambda
      [(path)
       (visit-file! path (lambda (kind value)
                           (case kind [(directory) (head:open-directory! value)] [(buffer) (head:show-buffer-mirror! value)])))]
      [(path destination) (visit-file! path destination #f)]
      [(path destination proposal)
       (unless (procedure? destination) (error 'visit-file! "expected a destination procedure" destination))
       (guard (ex [else (log:add! 'edit:visit-file! (format "Cannot open ~a: ~a" path (kernel:condition-text ex))) #f])
         (let ([result (document:acquire! head:ui-actor (file:expand (file:absolute path)) proposal)])
           (case (car result)
             [(directory) (destination 'directory (cadr result))]
             [(buffer)
              (let ([b (or (head:adopt-store-buffer! (cadr result))
                         (error 'visit-file! "acquired buffer is no longer visible"))])
                (head:sync-foreign-edits! (cadr result))
                (destination 'buffer b)
                (log:add! 'edit:visit-file! (cons (if (caddr result) "Loaded" "Visited") (list-ref result 3)) (caddr result))
                (when (list-ref result 4) (log:add! 'edit:visit-file! (list-ref result 4))))]) #t))]))

  (define (refuse! message)
    (raise (condition (kernel:make-refusal) (make-message-condition message))))
  (define (refuse-file! message) (refuse! message))

  (edoc "Save the current shared document through the base's document service. External changes merge undoably before saving; conflicts refuse, and an unavailable merge rereads undoably instead. Overwritten bytes become a shared backup. App presentations cannot be saved as files. Pre/post hooks run in this head, with mode, file and name adopted atomically."
        (target file "destination to write and visit") (returns boolean "whether saving completed"))
  (define (save-file! target)
    (let* ([path (file:visit-path target)] [b (head:window-buffer current-window)]
           [id (head:buffer-store-id b)])
      (define (check-source!)
        (when (head:app-buffer? b)
          (refuse-file! (format "Cannot save ~a: this buffer belongs to an app" (head:buffer-name b))))
        (unless id (refuse-file! "Only shared documents can be saved; copy the text into a document first")))
      (check-source!)
      (when (head:buffer-conflicted b) (refuse-file! "Resolve the conflicts first"))
      (file:run-pre-save-hooks! path)
      (check-source!)
      (let-values ([(text revision facts) (head:buffer-state b)])
        (let* ([detected (mode:detect path (vector-ref text 0))]
               [adoption (list (vector-ref text 0) (and detected (mode:name detected)))]
               [result (document:save! head:ui-actor id path adoption)])
          ;; Merge/reread can change shared text even when saving refuses.
          (head:sync-foreign-edits! id) (head:flush-ui-audit! id)
          (case (car result)
            [(refused) (refuse-file! (cadr result))]
            [(unchanged) (set! message (cadr result)) #f]
            [(failed) #f]
            [(saved)
             (guard (ex [else
                         (log:add! 'edit:save-file!
                           (format "Wrote ~a, but could not finish saving: ~a" path (kernel:condition-text ex)))
                         #f])
               (file:run-post-save-hooks! path)
               #t)])))))

  (define (merge-failure detail)
    ;; why the store could not merge the disk's changes, for the echo
    (case detail
      [(no-base) "without a saved baseline to merge from"]
      [(basis-too-old) "past the log's reach to merge"]
      [(pending-edits) "resolve the pending conflicts first; further edits have been preserved"]
      [else (format "not merged (~a)" detail)]))

  (define (reload-document! replace?)
    (let* ([b (head:current-buffer-mirror)] [id (head:buffer-store-id b)])
      (unless id (refuse-file! "This buffer is not a shared document"))
      (let-values ([(status detail)
                    (guard (ex [(kernel:refusal? ex) (raise ex)]
                               [else (refuse-file! (kernel:condition-text ex))])
                      ((if replace? document:reread! document:reload!) head:ui-actor id))])
        ;; An adoption failure after commit is not a refused document action.
        (head:sync-foreign-edits! id)
        (head:flush-ui-audit! id)
        (unless (eq? status 'applied)
          (refuse-file! (format "~a could not be ~a: ~a" (head:buffer-name b)
                          (if replace? "reread" "reloaded") (merge-failure detail)))))))

  (edoc "Reread the current document's file in the base as one undoable replacement, settling pending conflicts. Concurrent edits or retargeting during the read refuse; earlier undo history remains."
        (edits))
  (define (reread!) (check-editable!) (reload-document! #t))

  (edoc "Reload the current document's file in the base as one undoable merge, preserving earlier undo history. Concurrent edits or retargeting during the read refuse. Undo restores the pre-reload text while remembering the observed disk version, so saving can overwrite it." (public))
  (define (reload!) (reload-document! #f))

  (edoc "A buffer's text as its file would hold it: the lines joined with newlines, ending in one when the buffer keeps a trailing newline."
        (b buffer "the shared document to read")
        (returns string))
  (define (buffer-text b)
    (let-values ([(lines revision facts) (store:snapshot-state b)])
      (file:text lines (cond [(assq 'trailing facts) => cdr] [else #f]))))

  (edoc "Whether a buffer can be discarded without losing work: unmodified, or marked disposable; #f when its state cannot be read."
        (b buffer "the shared document to judge")
        (returns boolean) (public))
  (define (buffer-clean? b)
    ;; Discard decisions use one current snapshot, not an empty/stale
    ;; head cache.  Read-only protects editing, not the lifetime of work.
    ;; Generated tools explicitly opt into disposal; failed reads fail closed.
    (guard (ex [else #f])
      (let-values ([(text revision facts) (store:snapshot-state b)])
        (file:state-clean? text facts))))

  ;;; Buffer commands ------------------------------------------------------------

  ;; Read-only views of the editor's state, for M-x and modules; mutation
  ;; goes through the command API.
  (edoc "Log and show a message under edit:set-message!; an empty string clears the echo indicator without logging. Use log:add! in a function to name its own source, or paint:show-message! for an unlogged indicator."
        (s string "the message"))
  (define (set-message! s)
    (if (> (string-length s) 0)
        (log:add! 'edit:set-message! s)
        (paint:show-message! s #f)))

  (edoc "The selected region while the mark is active, else the whole current text buffer, as portable data. A local app without a document refuses."
        (returns region))
  (define (current-region)
    (let ([m (head:mark)] [b (head:current-buffer-mirror)])
      (unless (head:buffer-store-id b) (refuse! "The current app has no text document"))
      (if m
          (region:make (head:buffer-store-id b) m (head:point))
          (whole-buffer b))))

  (define (call-with-region r thunk)
    ;; r selected: its buffer current, the mark at its start and point at
    ;; its end; the previous selection and point return on exit and on
    ;; escape. The body of with-region, the one form M-x offers.
    (let* ([r (datum:copy r)] [id (region:buffer r)] [start (region:start r)] [end (region:end r)]
           [b (head:adopt-store-buffer! id)])
      (unless b (refuse! "The region's document is unavailable"))
      (for-each
        (lambda (p)
          (unless (and (< (car p) (head:buffer-line-count b))
                    (<= (cdr p) (string-length (head:buffer-line b (car p)))))
            (error 'edit:with-region "position is outside the document" p)))
        (list start end))
      (head:with-buffer-mirror b
        (let ([saved-point (head:point)] [saved-mark (cons mark-row mark-col)] [saved-active mark-active?])
          (define (select! start end active?)
            (set! mark-row (car start)) (set! mark-col (cdr start)) (set! mark-active? active?)
            (set! point-row (car end)) (set! point-col (cdr end)))
          (dynamic-wind
            (lambda () (select! start end #t))
            thunk
            (lambda () (select! saved-mark saved-point saved-active)))))))

  (edoc "Run body with a region selected: its document current, mark at its start and point at its end. Unavailable documents or coordinates outside the text refuse; previous selection and point return on exit and escape."
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

  ;;; Buffers: creation and the trash ---------------------------------------------

  (edoc "Create an empty shared buffer with a name, suffixed when the name is taken, and show it here."
        (name string "the buffer's name")
        (returns buffer) (public))
  (define (new-buffer! name)
    (let ([b (head:new-buffer! name)])
      (head:show-buffer-mirror! b)
      (datum:copy (head:buffer-store-id b))))

  (define (age-text seconds)
    ;; how long ago, in the coarsest unit that is not zero
    (cond [(< seconds 60) (format "~a s" seconds)]
          [(< seconds 3600) (format "~a min" (quotient seconds 60))]
          [(< seconds 86400) (format "~a h" (quotient seconds 3600))]
          [else (format "~a d" (quotient seconds 86400))]))

  (define (now-seconds) (time-second (current-time 'time-utc)))

  (define (trashed-entries)
    ;; (id name killed-at actor backup version), the newest kill first; backup is
    ;; the backup fact, (path stamp checksum), of a version a save kept
    (list-sort (lambda (a b) (or (> (caddr a) (caddr b)) (and (= (caddr a) (caddr b)) (> (cadar a) (cadar b)))))
      (filter values
        (map (lambda (entry)
               (let* ([m (cadr entry)] [t (cdr (assq 'trashed m))])
                 (and t (actor:in-audience? head:ui-actor (cdr (assq 'audience m)))
                   (not (cdr (assq 'internal m)))
                   (list (car entry) (cdr (assq 'name m)) (car t) (cadr t)
                     (cdr (assq 'backup m)) (cdr (assq 'version m))))))
             (cadr (store:metadata))))))

  (define (trash-entries) (filter (lambda (entry) (not (list-ref entry 4))) (trashed-entries)))
  (define (backup-entries) (filter (lambda (entry) (list-ref entry 4)) (trashed-entries)))
  (define (archive-entry! entry action)
    (let-values ([(status metadata) (store:archive! head:ui-actor (car entry) (list-ref entry 5) action)])
      (unless (eq? status 'applied) (error 'archive-entry! "the entry changed; choose it again" (cadr entry)))))

  (edoc "Trash a shared document by reference, or close the current buffer when omitted. Disposable output is deleted; a current local widget host is forgotten. Every window showing the document chooses its normal fallback."
        (buffer* (list-of buffer) "at most one shared document; default current"))
  (define (kill-buffer! . buffer*)
    (unless (<= (length buffer*) 1) (error 'kill-buffer! "expected at most one buffer"))
    (when (and (pair? buffer*) (not (handle:buffer? (car buffer*))))
      (error 'kill-buffer! "expected a buffer reference" (car buffer*)))
    (let* ([id (if (null? buffer*) (head:current-buffer) (car buffer*))]
           [b (if (null? buffer*) (head:current-buffer-mirror) (head:buffer-of-store-id id))]
           [m (and id (cadar (cadr (store:metadata (list id)))))]
           [name (if id (and m (cdr (assq 'name m))) (head:buffer-name b))])
      (when (and id (not (store:visible? head:ui-actor id))) (error 'kill-buffer! "buffer is not visible" id))
      (let* (
             [unsaved? (and m (cdr (assq 'modified m)))]
             [disposable? (and m (cdr (assq 'disposable m)))])
        (when id
          (unless m (error 'kill-buffer! "the buffer no longer exists" name))
          (let-values ([(status current) (store:archive! head:ui-actor id (cdr (assq 'version m)) 'trash)])
            (unless (eq? status 'applied) (error 'kill-buffer! "the buffer changed; choose it again" name))))
        (when b (head:forget-buffer! b))
        (log:add! 'edit:kill-buffer!
          (cond [(or (not id) disposable?) (format "Killed ~a" name)]
                [unsaved? (format "Killed ~a; its unsaved work is in the trash" name)]
                [else (format "Killed ~a; it is in the trash" name)])))))

  (edoc "The trashed buffers, the backups aside, newest first, as (name killed-at actor): killed-at in UTC seconds; each expires store:trash-retention days after it was killed."
        (returns (list-of list)))
  (define (trash)
    (map (lambda (entry) (list-head (cdr entry) 3)) (trash-entries)))

  (edoc "The backups, the versions saves wrote over, newest first, as (name path observed stamp checksum actor): the file's path, when it was read in UTC seconds, its modification time then as (seconds . nanoseconds) or #f, the checksum of its text and who saved; each expires store:trash-retention days after it was read, and a file keeps store:backups-kept of them."
        (returns (list-of list)) (public))
  (define (backups)
    (map (lambda (entry)
           (let ([backup (list-ref entry 4)])
             (list (cadr entry) (car backup) (caddr entry) (cadr backup) (caddr backup) (cadddr entry))))
         (backup-entries)))

  (edoc-type trashed "the name of a buffer in the trash, a backup included"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete (lambda (partial)
                (let ([now (now-seconds)])
                  (map (lambda (entry)
                         (let ([backup (list-ref entry 4)] [ago (age-text (- now (caddr entry)))])
                           (list (cadr entry) #f
                                 (if backup
                                     (format "backup of ~a, ~a ago" (file:abbreviate (car backup)) ago)
                                     (format "killed ~a ago" ago)))))
                       (trashed-entries)))))
    (write (lambda (v) (format "~s" v)))
    (within string))

  (edoc "Bring a buffer back from the trash, a backup included, the newest of that name, with its text and history, and show it in the current window; it takes a unique name when another buffer holds its own."
        (name trashed "the buffer's name in the trash")
        (returns buffer) (public))
  (define (restore! name)
    (let ([entry (find (lambda (entry) (string=? (cadr entry) name)) (trashed-entries))])
      (unless entry (error 'restore! "no such buffer in the trash" name))
      (let ([id (car entry)])
        (archive-entry! entry 'restore)
        (let ([b (head:adopt-store-buffer! id)])
          (unless b (error 'restore! "the buffer did not come back" name))
          (head:show-buffer-mirror! b)
          (log:add! 'edit:restore! (format "Restored ~a" (head:buffer-name b)))
          id))))

  (edoc "Permanently delete one trashed buffer or backup by name, including its history; live buffers and changed entries are refused. The original file on disk is untouched."
        (name trashed "the buffer's name in Trash or Backups") (public))
  (define (delete-trashed! name)
    (let ([entry (find (lambda (entry) (string=? (cadr entry) name)) (trashed-entries))])
      (unless entry (error 'delete-trashed! "no such buffer in the trash" name))
      (archive-entry! entry 'delete)
      (log:add! 'edit:delete-trashed! (format "Permanently deleted ~a" name))))

  (edoc "Delete every trashed buffer for good, the backups kept; how many went."
        (returns integer) (public))
  (define (empty-trash!)
    (let ([count 0])
      (for-each (lambda (entry)
                  (let-values ([(status m) (store:archive! head:ui-actor (car entry) (list-ref entry 5) 'delete)])
                    (when (eq? status 'applied) (set! count (+ 1 count))))) (trash-entries))
      (log:add! 'edit:empty-trash! (format "Emptied the trash: ~a buffer~a" count (if (= count 1) "" "s")))
      count))

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



  (edoc "Indent the current line by the mode's indenter, cycling through its stops; point lands on the indentation." (edits)
        (id model "editor view; omission addresses the current window"))
  (define indent-line!
    (case-lambda
      [() (indent-line! (require-editor))]
      [(id) (editor:format! id 'indent-line)]))

  (edoc "What TAB does: indent the current line when the mode's indenter asked for it, else nothing."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define indent-tab!
    (case-lambda
      [() (indent-tab! (require-editor))]
      [(id) (editor:format! id 'tab)]))

  (edoc "Indent the lines between mark and point by the mode's indenter, each settling on its nearest stop."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define indent-region!
    (case-lambda
      [() (indent-region! (require-editor))]
      [(id) (editor:format! id 'indent-region)]))

  (edoc "Indent every line of the current buffer by the mode's indenter."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define indent-buffer!
    (case-lambda
      [() (indent-buffer! (require-editor))]
      [(id) (editor:format! id 'indent-buffer)]))



  (edoc "Rewrite the lines between mark and point with the mode's formatter."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define format-region!
    (case-lambda
      [() (format-region! (require-editor))]
      [(id) (editor:format! id 'format-region)]))

  (edoc "Rewrite the whole current buffer with the mode's formatter."
        (edits)
        (id model "editor view; omission addresses the current window"))
  (define format-buffer!
    (case-lambda
      [() (format-buffer! (require-editor))]
      [(id) (editor:format! id 'format-buffer)]))

  ;;; Viewport commands -------------------------------------------------------

  ;; Painting and the frame are the painter's (paint); these are the
  ;; commands over its viewport logic -- paging and point placement --
  ;; and the head's side of the interaction protocol.

  (edoc "Page an allocated editor by a fraction of its height and put the caret in the middle; at an already reached edge, move to that edge. Retains mark activity and the desired column. Without an explicit view, page the legacy current window."
        (id model "editor view; omission addresses the current window")
        (direction integer "-1 for up, 1 for down")
        (fraction integer "positive page divisor"))
  (define page!
    (case-lambda
      [(direction fraction)
       (let ([id (current-editor)])
         (if id (page! id direction fraction)
           (let* ([w current-window] [v (head:window-text w)]
                  [sticky (min (head:buffer-sticky-lines (head:current-buffer-mirror)) (- (render:line-count v) 1))])
             (let-values ([(top point)
                           (text-layout:page v (head:window-rendition w) (and (paint:window-wrapped? w) (paint:wrap-width w))
                             sticky (paint:page-size) (cons (head:window-top w) (head:window-topseg w))
                             (visual-column w point-row point-col) direction fraction)])
               (head:goto! point)
               (head:window-top-set! w (car top)) (head:window-topseg-set! w (cdr top))))))]
      [(id direction fraction) (editor:page! id direction fraction)]))

  ;; The head's side of the interaction protocol: another actor's
  ;; question waits in the echo area as an unlogged indicator until
  ;; C-c a answers it -- nobody's keyboard is stolen mid-thought.
  (edoc-type answer "an answer to the oldest pending question, one of its choices"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (complete (lambda (partial)
                (let ([asks (actor:pending head:ui-actor)])
                  (if (null? asks) '()
                      (let ([ask (car asks)])
                        (map (lambda (choice) (list choice #f (caddr ask))) (cadddr ask)))))))
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

  (edoc "Save the current buffer to its file; a buffer without one refuses and names save-file!, which takes a path."
        (returns boolean "whether the file was written"))
  (define (save!)
    (if file-name
        (save-file! file-name)
        (refuse-file! "This buffer has no file: (edit:save-file! path) saves it under one")))

  (define (view-quit-buffers!)
    (let ([b (head:find-tool-buffer "*buffet*")])
      (if b
          (let ([w (window:display! (catalogue-host:reference b))])
            (when w
              (window:focus! w)
              (head:dispatch-app-event! "FOCUS")
              (set! message "")))
          (set-message! "The <buffet> app is not available"))))

  (edoc "Quit this head at once: shared text stays in the base, the screen is checkpointed for the next attach, and the exit notice names every buffer with unsaved work.")
  (define (quit!)
    (head:depart!))

  ;;; Pasting and typed runs --------------------------------------------------

  (edoc "Paste text into an explicit editor as one undo action separate from typing. Without arguments, insert the legacy window's pending terminal paste, normalizing line endings."
        (id model "editor view") (text string "text to insert")
        (edits))
  (define paste-into-buffer!
    (case-lambda
      [()
       ;; A bracketed paste: the whole text becomes one labeled edit, its
       ;; newlines becoming real line breaks.
       (let ([text (head:read-paste)])
         (unless (string=? text "")
           (call-as-one-edit! (format "insert ~s" text)
             (lambda ()
               (insert-text! (string:join (tty:paste-lines text) "\n"))))))]
      [(id text) (editor:paste! id text)]))

  (edoc "Type text: inserted at point as typing does, continuing the run of typing, backspaces and deletes before it, so a run undoes as one step and shares one batch; SELF-INSERT, any character without a binding of its own, runs it with the character typed."
        (text string "the text to type")
        (edits))
  (define (type! text) (editor:insert! (require-editor) text))

  ;;; Small commands and key description -------------------------------------

  (edoc "Set the mark at point and activate it.")
  (define (set-mark-command!)
    (when (head:buffer-selectable? (head:current-buffer-mirror))
      (set! mark-row point-row) (set! mark-col point-col)
      (set! mark-active? #t))
    (set! message (if mark-active? "Mark set" "")))

  (edoc "Move point to the start of its line.")
  (define (beginning-of-line!)
    (let ([id (current-editor)])
      (if id (editor:move! id 'home)
        (set! point-col 0))))

  (edoc "Move point to the end of its line.")
  (define (end-of-line!)
    (let ([id (current-editor)])
      (if id (editor:move! id 'end)
        (set! point-col (string-length (current-display-line))))))

  (edoc "Deactivate the mark and abandon what was pending.")
  (define (keyboard-quit!)
    (set! mark-active? #f) (set! message "Quit"))

  (edoc "Erase and repaint the screen, asking the terminal for its color scheme again.")
  (define (redraw-command!)
    (tty:query-color-scheme!)
    (paint:mark-size-dirty!) (paint:erase-screen!) (set! message "Screen redrawn"))

  (edoc "Insert a line break after point, leaving point where it is."
        (edits))
  (define (open-line!) (editor:insert-at! (require-editor) "\n" #t))

  (edoc "Scroll the selected window up by a page and put point in the middle; at the top, move point to the first line.")
  (define (page-up!)
    (page! -1 1))

  (edoc "Scroll the selected window down by a page and put point in the middle; at the bottom, move point to the last line.")
  (define (page-down!)
    (page! 1 1))

  (edoc "Move point up one line, or one visual row in a wrapping window, keeping the goal column.")
  (define (previous-line!)
    (move-vertical! -1))

  (edoc "Move point down one line, or one visual row in a wrapping window, keeping the goal column.")
  (define (next-line!)
    (move-vertical! 1))

  (edoc "Move point to the start of the buffer.")
  (define (beginning-of-buffer!)
    (let ([id (current-editor)])
      (if id (editor:move! id 'start)
        (begin
          (set! point-row 0) (set! point-col 0)))))

  (edoc "Move point to the end of the buffer.")
  (define (end-of-buffer!)
    (let ([id (current-editor)])
      (if id (editor:move! id 'finish)
        (begin
          (set! point-row (- (vlen) 1))
          (set! point-col (string-length (current-display-line)))))))

  ;;; Regions and the generic helpers ------------------------------------------

  (define (whole-buffer b)
    (let ([last (- (head:buffer-line-count b) 1)])
      (region:make (head:buffer-store-id b) '(0 . 0)
                   (cons last (string-length (head:buffer-line b last))))))

  (edoc "Read a region from one document snapshot, rows joined with newlines; unavailable documents or coordinates outside the text refuse. No displayed buffer is required."
        (r region "the region to read")
        (returns string))
  (define (region-text r)
    (let ([id (region:buffer r)] [start (region:start r)] [end (region:end r)])
      (string:join
        (store:extract id (text:make-span (car start) (cdr start) (car end) (cdr end)))
        "\n")))

  ;;; Registration ----------------------------------------------------------------

  ;; Everything the layer registers -- owned by edit, so a reload
  ;; retracts and remakes it; what the loop and the seams ask of the
  ;; commands is installed here too.
  (edoc "Install the command layer: log presentation, the file formatters, status hints, the default key bindings, the loop's hooks and the buffet." (public))
  (define (init!)
    (editor:register! (list (cons 'undo undo!) (cons 'redo redo!) (cons 'page page!) (cons 'paste paste-into-buffer!)
                        (cons 'kill-line kill-line!) (cons 'kill-region kill-region!) (cons 'copy-region copy-region!) (cons 'yank yank!)
                        (cons 'forward-expression forward-expression!) (cons 'backward-expression backward-expression!)
                        (cons 'up-expression up-expression!) (cons 'down-expression down-expression!)
                        (cons 'next-list next-list!) (cons 'previous-list previous-list!) (cons 'beginning-of-form beginning-of-form!) (cons 'end-of-form end-of-form!)
                        (cons 'mark-expression mark-expression!) (cons 'mark-form mark-form!) (cons 'transpose-expressions transpose-expressions!)
                        (cons 'kill-expression kill-expression!) (cons 'backward-kill-expression backward-kill-expression!)
                        (cons 'indent-tab indent-tab!) (cons 'indent-expression indent-expression!) (cons 'indent-region indent-region!)
                        (cons 'indent-line indent-line!) (cons 'indent-buffer indent-buffer!) (cons 'format-region format-region!) (cons 'format-buffer format-buffer!)))
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
      (log:register-formatter! 'edit:visit-file! fmt)
      (log:register-formatter! 'edit:save-file! fmt))
    (style:set-changed-hook!
      (lambda () (paint:invalidate-screen-cache!)))
    (style:color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! style:color-scheme!)
    (head:add-shutdown-hook! (lambda () (head:flush-ui-audit! 'all)))
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
          ("C-M-f" ,forward-expression!) ("C-M-b" ,backward-expression!)
          ("C-M-n" ,next-list!) ("C-M-p" ,previous-list!)
          ("C-M-u" ,up-expression!) ("C-M-d" ,down-expression!)
          ("C-M-a" ,beginning-of-form!) ("C-M-e" ,end-of-form!)
          ("C-M-k" ,kill-expression!) ("C-M-BACKSPACE" ,backward-kill-expression!)
          ("C-M-@" ,mark-expression!) ("C-M-SPC" ,mark-expression!) ("C-M-h" ,mark-form!)
          ("C-M-t" ,transpose-expressions!) ("C-M-q" ,indent-expression!)
          ("M-v" ,page-up!) ("M-<" ,beginning-of-buffer!)
          ("M->" ,end-of-buffer!) ("UP" ,previous-line!)
          ("DOWN" ,next-line!) ("LEFT" ,move-left!)
          ("RIGHT" ,move-right!) ("HOME" ,beginning-of-line!)
          ("END" ,end-of-line!) ("DELETE" ,delete-forward!)
          ("PAGEUP" ,page-up!) ("PAGEDOWN" ,page-down!)
          ("PASTE" ,paste-into-buffer!) ("SELF-INSERT" ,(keymap:call type! head:typed-text))
          ("C-x C-g" ,keyboard-quit!) ("C-x C-r" ,reread!) ("C-x C-s" ,save!)
          ("C-x C-w" ,(keymap:prefill save-file!)) ("C-x C-c" ,quit!)
          ("C-x k" ,kill-buffer!)
          ("C-c a" ,(keymap:prefill answer!))))
      #t)
    ;; The loop's hooks live in (head): how to open the file
    ;; argument, how to quit (the modified-buffers check), and what runs
    ;; after every key
    (begin
      (head:set-file-opener! visit-file!)
      (head:set-quit-command! quit!)
      (head:set-review-viewer! view-quit-buffers!)
      (head:add-pre-redraw-hook! publish-copy-changes!)
      (head:set-after-key! clamp-point!))

  )

) ;; library (edit)
