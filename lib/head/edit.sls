;; edit.sls -- the command layer: the library (edit), the e editor's
;; default app.
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

(import (only (foundation edoc) elibrary))
(elibrary (head edit)
  (export answer! backspace! backups backward-expression! backward-kill-expression! beginning-of-buffer! beginning-of-form!
          beginning-of-line! buffer-clean? buffer-text
          call-as-one-edit! copy-region! copy-text copy-text! current-batch current-region
          delete-forward! down-expression! empty-trash! end-of-buffer! end-of-form! end-of-line! format-buffer!
          format-region!
          forward-copy-buffer-to-system-clipboard forward-expression! indent-buffer! indent-expression! indent-line!
          indent-region!
          indent-tab! init! insert-text! keyboard-quit! kill-buffer! kill-expression! kill-line! kill-region!
          mark-expression! mark-form!
          message-progress message-source move-horizontal! move-left! move-right! move-vertical!
          new-buffer! newline! next-line! next-list! open-line! page-down! page-up! page-window!
          page-window-fraction! (rename (paste-into-buffer! paste!)) present-log-entries! present-log-entry! previous-line!
          previous-list!
          prompt-file! quit! redo! redraw-command! region-text reload! replace-region-text! reread! restore!
          rewrite-region! rewrite-regions! save! save-file! set-mark-command! set-message!
          set-point-without-scroll! transpose-expressions! trash type! undo! undo-actor! undo-scope up-expression!
          visit-file! with-region
          yank!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (core property) property:)
          (prefix (foundation datum) datum:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head dispatch) dispatch:)
          (prefix (head echo) echo:)
          (prefix (head expression) expression:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (head literal)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head render) render:)
          (prefix (head style) style:)
          (prefix (head table) table:)
          (prefix (head window) window:)
          (prefix (service doc) doc:)
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
  ;; whether an edit joins the buffer's latest recorded one, its undo group
  ;; and batch, as the keys of a typing run do
  (define continuing-edit (make-parameter #f))
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

  (define (submit-edit! b span replacement . properties)
    ;; The pending action supplies the store's grouping key, the label and
    ;; the batch; the store owns the undo history. The entry is staged: it
    ;; becomes the buffer's latest only when a mutation actually succeeds.
    (let* ([action (pending-edit)]
           [entry (and action (cadr action))]
           [key (and entry (cadr entry))])
      (unless (and action (eq? (car action) b))
        (error 'submit-edit! "edit has no pending action"))
      (head:store-edit! b span replacement
                        (append (list key (caddr action)) properties
                                (list (cons 'labels (list (cons 'batch (list-ref action 4))))))
                        (cons (cons current-window (edit-point))
                              (if (edit-mark) (list (cons 'mark (edit-mark))) '()))
                        (edit-basis-for b))
      ((cadddr action))))

  (define (replace-buffer-lines! b target . properties)
    ;; Formatting, indentation, and merging are ordinary attributed
    ;; edits.  Only loading/rereading a baseline may reset the store.
    (let-values ([(span replacement) (text:difference (car (edit-basis-for b)) target)])
      (apply submit-edit! b span replacement properties)))

  ;; Undo entries are labeled with the user-level action that made them
  ;; -- "insert \"hello\"", "(search:replace! \"xx\" \"yy\")" -- and undo
  ;; and redo report the label.  An entry is (label key batch): the key
  ;; the store groups the buffer's edits under, one undo step, and the
  ;; batch their delta log entries carry; the store owns the history
  ;; itself.  Inside a call-as-one-edit! group, the box holds (label .
  ;; buffer-entries): one entry per buffer the group touches, labeled with
  ;; the group's label (or, lacking one, that buffer's first edit's).
  (define edit-group (make-parameter #f))
  (define pending-edit (make-parameter #f))

  ;; The batch label every edit carries: one per outermost group, so a
  ;; command's edits across buffers share it; one per fresh entry
  ;; otherwise, chained typing sharing its entry's.
  (define edit-batch (make-parameter #f))
  (define (mint-batch!)
    (list head:ui-actor (gensym->unique-string (gensym "batch"))))

  ;; The entry a buffer's latest recorded edit joined, for the next key of
  ;; a typing run to continue; an undo, a redo or a reread forgets it, so a
  ;; run never joins an entry the store has moved
  (define latest-edits (make-weak-eq-hashtable))

  (define (forget-latest! b) (hashtable-delete! latest-edits b))

  (define reload-due '()) ; files noticed while a command is editing

  (define (check-disk-before-edit!)
    ;; The start of an edit session -- one undo entry; chained typing
    ;; checks once: a file changed on disk meanwhile is noted, and once the
    ;; edit is made, against the text as the user saw it, the buffer
    ;; reloads through the store before the frame, the disk's changes
    ;; merged with the buffer's, the edit just made among them, so an
    ;; insertion where the disk inserted conflicts instead of landing
    ;; elsewhere.  The mtime raises the suspicion cheaply; the content
    ;; confirms it, so a mere touch passes silently.
    (let ([b (head:window-buffer current-window)])
      (when (and file-name (head:buffer-base b))
        (let-values ([(text revision facts) (head:buffer-state b)])
          (let ([path (cond [(assq 'file facts) => cdr] [else #f])]
                [base (cond [(assq 'base facts) => cdr] [else #f])])
            (when (and path base)
              (let ([stamp (file:stamp path)])
                (unless (and stamp (equal? stamp (cond [(assq 'stamp facts) => cdr] [else #f])))
                  (let ([disk (guard (ex [else #f]) (read-disk path))])
                    (cond
                      [(not disk) (void)]
                      [(string=? (car disk) base)
                       (head:buffer-facts-set! b (list (cons 'stamp (cdr disk))) (property:select facts '(file base stamp)))]
                      [else (unless (memq b reload-due) (set! reload-due (cons b reload-due)))]))))))))))

  (define (reload-if-due!)
    ;; before the frame: the buffer an edit found changed on disk reloads,
    ;; the disk read again now; a file the store cannot reload says so in
    ;; the echo, as reload! does, and the edit stands
    (let ([pending reload-due])
      (set! reload-due '())
      (for-each (lambda (b)
                  (when (and b (memq b (head:buffers)) (head:buffer-store-id b))
                    (let-values ([(text revision facts) (head:buffer-state b)])
                      (let ([path (cond [(assq 'file facts) => cdr] [else #f])]
                            [base (cond [(assq 'base facts) => cdr] [else #f])])
                        (when (and path base)
                          (let ([disk (guard (ex [else #f]) (read-disk path))])
                            (when (and disk (not (string=? (car disk) base)))
                              (guard (ex [(kernel:refusal? ex) (void)])
                                (let-values ([(status detail) (reload-from-disk! b path disk)])
                                  (unless (eq? status 'applied)
                                    (refuse-file! (format "~a could not be reloaded: ~a" (file:base-name path) (merge-failure detail))))))))))))) pending)))

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

  (define (call-with-recorded-edit! label thunk)
    (check-editable!)
    (let ([b (head:window-buffer current-window)]
          [pending (pending-edit)])
      (if (and pending (eq? (car pending) b))
          (thunk)
          (begin
            (unless (continuing-edit) (check-disk-before-edit!))
            (let* ([group (edit-group)]
                   [group-hit (and group (assq b (cdr (unbox group))))]
                   [previous (or (and group-hit (cdr group-hit))
                                 (and (not group) (continuing-edit) (hashtable-ref latest-edits b #f)))]
                   [label (cond [group-hit (car previous)]
                                [group (or (car (unbox group)) label)]
                                [else label])]
                   [entry (or previous
                              (list label (list 'head-edit head:ui-actor (head:buffer-store-rev b))
                                    (or (edit-batch) (mint-batch!))))]
                   [committed? #f]
                   [commit!
                    (lambda ()
                      (unless committed?
                        (set-car! entry label)
                        (hashtable-set! latest-edits b entry)
                        (when (and group (not group-hit))
                          (set-box! group (cons (car (unbox group)) (cons (cons b entry) (cdr (unbox group))))))
                        (set! committed? #t)))])
              (let ([result (parameterize ([pending-edit (list b entry label commit! (caddr entry))])
                              (thunk))])
                ;; the file the edit found changed on disk reloads now, the
                ;; edit among the entries the merge carries or conflicts
                (unless group (reload-if-due!))
                result))))))

  (define-syntax with-recorded-edit
    (syntax-rules ()
      [(_ label body ...)
       (call-with-recorded-edit! label (lambda () body ...))]))

  (edoc "The batch label of the edits in the current one-edit group, the (actor token) pair they share in the delta log, unique across head reattachments, or #f outside a group."
        (returns (or list #f)))
  (define (current-batch) (edit-batch))

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
        (dynamic-wind void
          (lambda () (parameterize ([edit-group (box (cons label '()))] [edit-batch (mint-batch!)]) (thunk)))
          reload-if-due!)))

  (define (check-undo-scope scope)
    (unless (memq scope '(mine all))
      (error 'undo-scope "expected mine or all" scope))
    scope)

  (edoc "The default scope of undo!: mine, this head's own latest live action, or all, any actor's."
        (value (one-of mine all)))
  (define undo-scope (make-parameter 'mine check-undo-scope))

  (define (no-history verb)
    (format "No further ~a information" (string-downcase verb)))

  (define (history-shift! direction verb scope)
    ;; undo or redo through the store, which owns the history; the report
    ;; names the actor and the label of the action moved
    (check-editable!)
    (let ([b (head:window-buffer current-window)])
      (let-values ([(status detail) (head:store-history! b direction scope)])
        (set! message
          (case status
            [(nothing) (no-history verb)]
            [(applied)
             (forget-latest! b)
             (set! mark-active? #f)
             (head:window-goal-set! current-window #f)
             (head:clamp-buffer-positions! b)
             (paint:invalidate-screen-cache!)
             (string:elide (format "~a ~s: ~a" verb (caddr detail) (or (list-ref detail 4) "edit")) cols)]
            [else
             (format "~a blocked: ~a" verb
                     (case detail
                       [(read-only) "the buffer is read-only"]
                       [(basis-too-old) "history is incomplete"]
                       [(overlap) "another edit overlaps this action"]
                       [(property-changed) "a text property changed after this action"]
                       [else "the store is unavailable"]))]))
        message)))

  (edoc "Undo one action in the current buffer within the undo-scope: this head's latest under mine, any actor's under all."
        (returns string "the report shown in the echo area")
        (edits))
  (define (undo!)
    (history-shift! 'undo "Undo" (undo-scope)))

  (edoc "Reverse this head's latest undo."
        (returns string "the report shown in the echo area")
        (edits))
  (define (redo!)
    (history-shift! 'redo "Redo" 'mine))

  (edoc "Undo an actor's latest live action in the current shared buffer."
        (who actor "the actor's identity")
        (returns string "the report shown in the echo area")
        (edits))
  (define (undo-actor! who)
    (history-shift! 'undo "Undo" (list 'actor who)))

  ;;; Point, mark, and editing ----------------------------------------------

  (define (changed!)
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

  (edoc "Move point forward over one expression: the atom around point, else the next expression inside the enclosing one; the C-M-f of Emacs.")
  (define (forward-expression!)
    (let-values ([(start end) (expression:forward (head:current-buffer) (head:point))])
      (if end (head:goto! end) (set-message! "No expression after point"))))

  (edoc "Move point backward over one expression: the atom around point, else the last expression ending by it inside the enclosing one; the C-M-b of Emacs.")
  (define (backward-expression!)
    (let-values ([(start end) (expression:backward (head:current-buffer) (head:point))])
      (if start (head:goto! start) (set-message! "No expression before point"))))

  (define (position-before? a b)
    (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (define (kill-between! from to prepend?)
    ;; from precedes to; the killed text joins the copy buffer as C-k's
    ;; does, ahead of the previous kill when killing backward
    (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
           [text (text-between (car from) (cdr from) (car to) (cdr to))])
      (with-recorded-edit (format "kill ~s" text)
        (parameterize ([edit-source source]) (delete-region! (car from) (cdr from) (car to) (cdr to)))
        (kill! text prepend?)
        (changed!))))

  (edoc "Kill from point to the end of the next expression into the copy buffer; consecutive kills accumulate; the C-M-k of Emacs."
        (edits))
  (define (kill-expression!)
    (let-values ([(start end) (expression:forward (head:current-buffer) (head:point))])
      (if end (kill-between! (head:point) end #f) (set-message! "No expression after point"))))

  (edoc "Kill from the start of the expression before point to point into the copy buffer, ahead of a preceding kill; the C-M-BACKSPACE of Emacs."
        (edits))
  (define (backward-kill-expression!)
    (let-values ([(start end) (expression:backward (head:current-buffer) (head:point))])
      (if start (kill-between! start (head:point) #t) (set-message! "No expression before point"))))

  (edoc "Set the mark at the end of the next expression and activate it, point staying; with the mark active beyond point, extend it by one more expression; the C-M-SPC of Emacs.")
  (define (mark-expression!)
    (let* ([point (head:point)] [mark (cons mark-row mark-col)]
           [from (if (and mark-active? (position-before? point mark)) mark point)])
      (let-values ([(start end) (expression:forward (head:current-buffer) from)])
        (cond [(not end) (set-message! "No expression after point")]
              [(head:buffer-selectable? (head:current-buffer))
               (set! mark-row (car end)) (set! mark-col (cdr end)) (set! mark-active? #t)
               (set! message "Mark set")]))))

  (edoc "Mark the top-level form around point: point at its start, the mark at its end; the C-M-h of Emacs.")
  (define (mark-form!)
    (let-values ([(start end) (expression:top-level (head:current-buffer) (head:point))])
      (cond [(not start) (set-message! "No top-level form in the buffer")]
            [(head:buffer-selectable? (head:current-buffer))
             (head:goto! start)
             (set! mark-row (car end)) (set! mark-col (cdr end)) (set! mark-active? #t)
             (set! message "Mark set")])))

  (edoc "Move point up out of the enclosing list or vector, to its start; the C-M-u of Emacs.")
  (define (up-expression!)
    (let-values ([(start end) (expression:container (head:current-buffer) (head:point))])
      (if start (head:goto! start) (set-message! "Not inside an expression"))))

  (edoc "Move point down into the next list or vector, just past its opening delimiter; the C-M-d of Emacs.")
  (define (down-expression!)
    (let ([inside (expression:down (head:current-buffer) (head:point))])
      (if inside (head:goto! inside) (set-message! "No list after point"))))

  (edoc "Move point over the next list or vector, skipping atoms; the C-M-n of Emacs.")
  (define (next-list!)
    (let-values ([(start end) (expression:next-list (head:current-buffer) (head:point))])
      (if end (head:goto! end) (set-message! "No list after point"))))

  (edoc "Move point back over the previous list or vector, skipping atoms; the C-M-p of Emacs.")
  (define (previous-list!)
    (let-values ([(start end) (expression:previous-list (head:current-buffer) (head:point))])
      (if start (head:goto! start) (set-message! "No list before point"))))

  (edoc "Move point to the start of the last top-level form beginning before point, the enclosing one included; the C-M-a of Emacs.")
  (define (beginning-of-form!)
    (let ([start (expression:form-start (head:current-buffer) (head:point))])
      (if start (head:goto! start) (set-message! "No top-level form before point"))))

  (edoc "Move point to the end of the first top-level form ending after point, the enclosing one included; the C-M-e of Emacs.")
  (define (end-of-form!)
    (let ([end (expression:form-end (head:current-buffer) (head:point))])
      (if end (head:goto! end) (set-message! "No top-level form after point"))))

  (edoc "Swap the expression before point with the one after it, point ending after both; the C-M-t of Emacs."
        (edits))
  (define (transpose-expressions!)
    (let ([b (head:current-buffer)] [point (head:point)])
      (let-values ([(as ae) (expression:backward b point)] [(bs be) (expression:forward b point)])
        (if (or (not as) (not bs) (equal? as bs))
            (set-message! "No two expressions around point")
            (let ([before (text-between (car as) (cdr as) (car ae) (cdr ae))]
                  [between (text-between (car ae) (cdr ae) (car bs) (cdr bs))]
                  [after (text-between (car bs) (cdr bs) (car be) (cdr be))])
              (replace-region-text! as be (string-append after between before))
              (head:goto! be))))))

  (edoc "Indent the lines of the next expression after its first by the mode's indenter; the C-M-q of Emacs."
        (edits))
  (define (indent-expression!)
    (let-values ([(start end) (expression:forward (head:current-buffer) (head:point))])
      (cond [(not end) (set-message! "No expression after point")]
            [(< (car start) (car end))
             (when (indent-rows! (+ (car start) 1) (car end))
               (set! message (format "Indented ~a line~a" (- (car end) (car start)) (if (= (- (car end) (car start)) 1) "" "s"))))]
            [else (set! message "Nothing to indent below the first line")])))

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
        (s string "the text to insert")
        (edits))
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

  (edoc "Insert a line break at point."
        (edits))
  (define (newline!)
    (insert-text-as! "\n" "newline"))

  ;;; The typing run ------------------------------------------------------------
  ;;
  ;; Typed characters, backspaces and forward deletes coalesce into one undo
  ;; entry and one batch of the delta log (up to twenty keys, as in Emacs),
  ;; so undo removes the run, a typo and its correction together, not one
  ;; key.  The chain is (buffer row col count left before after): where
  ;; point must stand for the next of these commands to continue the run,
  ;; how many keys it has, the run's own text standing before point, and
  ;; the older text it deleted before and after that.  Any other command
  ;; breaks the run: it only continues when the last command was one of the
  ;; three, type! itself or its call from the SELF-INSERT key, and point is
  ;; where that command left it.
  (define typing-chain #f)

  (define no-run '(#f 0 0 0 "" "" ""))

  (define (typing-run b)
    ;; the run the next key continues, or #f
    (and typing-chain
         (let ([last (head:last-command)]) (or (typing? last) (memq last (list backspace! delete-forward!))))
         (eq? (car typing-chain) b)
         (= (cadr typing-chain) point-row)
         (= (caddr typing-chain) point-col)
         (< (cadddr typing-chain) 20)
         typing-chain))

  (define (typing-label left before after)
    ;; the run's net effect as its undo label
    (let ([removed (string-append before after)])
      (cond [(string=? removed "") (format "insert ~s" left)]
            [(string=? left "") (format "delete ~s" removed)]
            [else (format "replace ~s with ~s" removed left)])))

  (define (typing-edit! b run left before after thunk)
    ;; one key of a run: the edit joins the run's undo entry and batch when
    ;; the run continues, and the chain remembers where point now stands
    (parameterize ([continuing-edit (and run #t)])
      (with-recorded-edit (typing-label left before after)
        (thunk)
        (changed!)))
    (set! typing-chain (list b point-row point-col (+ (cadddr (or run no-run)) 1) left before after)))

  (edoc "Delete the character after point, or join the next line at a line end; a run of typing, backspaces and deletes is one undo step."
        (edits))
  (define (delete-forward!)
    (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
           [row point-row] [col point-col] [line (current-line)]
           [span (cond [(< col (string-length line)) (text:make-span row col row (+ col 1))]
                       [(< row (- (vector-length (car source)) 1)) (text:make-span row col (+ row 1) 0)]
                       [else #f])])
      (when span
        (let* ([deleted (if (< col (string-length line)) (string (string-ref line col)) "\n")]
               [run (typing-run b)] [chain (or run no-run)])
          ;; the text after point is never the run's own: it goes with the older text deleted after
          (typing-edit! b run (list-ref chain 4) (list-ref chain 5) (string-append (list-ref chain 6) deleted)
            (lambda ()
              (parameterize ([edit-source source])
                (submit-edit! b span '("")))))))))

  (edoc "Delete the character before point, or join with the previous line at a line start; a run of typing, backspaces and deletes is one undo step."
        (edits))
  (define (backspace!)
    (when (or (> point-col 0) (> point-row 0))
      (let* ([b (head:window-buffer current-window)] [source (edit-basis-for b)]
             [end-row point-row] [end-col point-col]
             [row (if (> end-col 0) end-row (- end-row 1))]
             [col (if (> end-col 0) (- end-col 1) (string-length (line-at row)))]
             [deleted (if (> end-col 0) (string (string-ref (line-at row) col)) "\n")]
             [run (typing-run b)] [chain (or run no-run)]
             [left (list-ref chain 4)] [n (string-length left)])
        ;; a typo corrected takes the run's own last character back; past
        ;; the run's text, the character goes with the older text deleted before it
        (typing-edit! b run
          (if (> n 0) (substring left 0 (- n 1)) left)
          (if (> n 0) (list-ref chain 5) (string-append deleted (list-ref chain 5)))
          (list-ref chain 6)
          (lambda ()
            (parameterize ([edit-source source])
              (submit-edit! b (text:make-span row col end-row end-col) '(""))))))))

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
    ;; *copy* takes the text as one entry of its log -- C-_ there
    ;; brings the previous copy back -- with point at its end in every
    ;; window showing it; then the clipboard follows at once.
    (let ([b (head:copy-buffer)])
      (let-values ([(lines trailing?) (text:from-string text)])
        (head:with-buffer b
          (parameterize ([edit-source #f])
            (with-recorded-edit (format "~a ~s" label (string:elide text 40))
              (replace-buffer-lines! b lines (cons 'undo (list (cons 'trailing trailing?))))))))
      (note-copy-published! b)
      (publish-system-clipboard! text)))

  (define (killing?)
    ;; was the previous command a kill?  Consecutive kills accumulate
    ;; into a single copy-buffer entry.
    (and (memq (head:last-command) (list kill-line! kill-region! kill-expression! backward-kill-expression!)) #t))

  (define (kill! text . before?)
    ;; consecutive kills accumulate into the copy buffer, a backward kill
    ;; ahead of what is there
    (let ([old (head:copy-text)] [prepend? (and (pair? before?) (car before?))])
      (replace-copy-text! (if (killing?) (if prepend? (string-append text old) (string-append old text)) text) "kill")))

  (edoc "Copy text into the copy buffer without changing a buffer or point; C-y pastes it."
        (text string "the text to copy"))
  (define (copy-text! text)
    (unless (string? text)
      (error 'copy-text! "expected a string" text))
    (replace-copy-text! text "copy")
    (void))

  (edoc "Kill from point to the end of the line, or the line break when point is at the end; consecutive kills accumulate."
        (edits))
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

  (edoc "The copy buffer's text."
        (returns string))
  (define (copy-text)
    (head:copy-text))

  (edoc "Insert the copy buffer's text at point."
        (edits))
  (define (yank!)
    ;; The copy buffer can span lines after consecutive C-k commands.  Insert
    ;; newlines as buffer structure rather than embedding them in a line string.
    (let ([text (head:copy-text)])
      (unless (string=? text "")
        (insert-text-as! text (format "yank ~s" text)))))

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
        (text string "the replacement")
        (edits))
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
        (text string "the replacement")
        (edits))
  (define (rewrite-region! basis start end text)
    ;; the editing operation behind the bulk replacers: an explicit basis
    ;; and a kept point, with the undo grouping and mark handling of every
    ;; recorded edit
    (parameterize ([edit-source basis] [edit-point (head:point)])
      (replace-region-text! start end text)))

  (edoc "Replace several ordered ranges of the current buffer with texts computed against a basis, one structural edit each in the current undo group, point kept where it was: the ranges are in the basis's coordinates, disjoint and in the text's order, and each later one is carried across the changes the store reports after an edit, this head's own and other actors', a range whose text changed under it being skipped."
        (basis list "the edit basis the ranges were computed against")
        (regions (list-of list) "(start end text) each, in the text's order")
        (returns integer "how many ranges were replaced")
        (edits))
  (define (rewrite-regions! basis regions)
    (define (span-of region)
      (text:make-span (car (car region)) (cdr (car region)) (car (cadr region)) (cdr (cadr region))))
    (define (carry regions changes)
      ;; the ranges still to replace, mapped through the changes since the
      ;; last basis, those a change touched dropped
      (filter values
        (map (lambda (region)
               (let ([span (fold-left (lambda (span change) (and span (text:rebase-span span (caddr change))))
                                      (car region) changes)])
                 (and span (cons span (cdr region)))))
             regions)))
    (let ([b (head:window-buffer current-window)])
      (let loop ([regions (map (lambda (region) (cons (span-of region) (caddr region))) regions)] [basis basis] [n 0])
        (if (null? regions) n
            (let ([span (car (car regions))] [text (cdr (car regions))])
              (rewrite-region! basis (text:span-start span) (text:span-end span) text)
              (let-values ([(lines revision changes) (head:snapshot-since b (caddr basis))])
                (unless changes (error 'rewrite-regions! "the changes since the basis are no longer available"))
                (loop (carry (cdr regions) changes) (head:edit-basis b) (+ n 1))))))))

  (edoc "Copy the text between mark and point to the copy buffer without deleting it; the mark deactivates.")
  (define (copy-region!)
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
                (set! message "Copied"))))))

  (edoc "Kill the text between mark and point into the copy buffer."
        (edits))
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
                         (list (cons 'stamp (cdr disk)))
                         (property:select facts '(file base stamp)))]
                      [disk (reopen-changed-file! b path disk)]
                      [else
                       (parameterize ([message-source 'visit-file!])
                         (set-message! (format "Cannot reread ~a" path)))]))))))))))

  (edoc "Visit a file in the current window, creating or reusing its buffer; nothing is written to disk."
        (path file "the file to visit"))
  (define (visit-file! path)
    ;; Direct visits and the interactive picker share acquisition and the
    ;; buffer-only reload or reread flow. Visiting never writes to disk.
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

  (edoc "Save the current buffer to a file; active app buffers refuse because their app owns the text and mode. Guarded by content: when the disk no longer matches the buffer's base, reload it first and write when no conflict pends, else stop for their resolution; where the store cannot reload, reread the disk instead, undoably, and refuse. Saving onto an existing file first reads what it holds into a backup, a trashed buffer named after the file with .bak that backups lists and restore! brings back; nothing asks."
        (target file "where to write: the buffer's own file, or a new destination it visits from then on")
        (returns boolean "whether the file was written"))
  (define (save-file! target)
    ;; Saving is guarded by content, not clocks: the disk is read and
    ;; compared with the buffer's base (what it loaded or last saved).
    ;; A mismatch means somebody changed the file meanwhile -- the
    ;; save reloads first, writing when nothing conflicts.
    (define path (file:visit-path target))
    (define b (head:window-buffer current-window))
    (define adopted? #f)
    (define disk #f)
    (define kept #f)
    (define (check-source!)
      (when (head:app-buffer? b)
        (refuse-file! (format "Cannot save ~a: this buffer belongs to an app" (head:buffer-name b)))))
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
          (when (> (cond [(assq 'conflicts facts) => cdr] [else 0]) 0)
            (refuse-file! "Resolve the conflicts first"))
          (review-disk! path disk)
          (let* ([trailing (cond [(assq 'trailing facts) => cdr] [else #t])]
                 [written (file:text text trailing)]
                 [detected (and adopted? (mode:detect path (vector-ref text 0)))])
            ;; the version written over is kept first, as a backup
            (when (and disk (not (string=? (car disk) written)))
              (set! kept (back-up! path disk)))
            (file:write! path text trailing)
            (set! written? #t)
            ;; A stat after writing could belong to another disk writer.
            ;; Invalidate the hint; the next edit verifies content again.
            (unless (head:buffer-facts-set! b
                      (append (list (cons 'file path) (cons 'base written)
                                '(stamp . #f))
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
        (file:run-post-save-hooks! path)
        (if (and adopted? kept)
            (parameterize ([message-source 'save-file!])
              (set-message! (format "Wrote ~a; what it held is kept as ~a" path kept)))
            (log:add! 'save-file! (cons "Wrote" path)))
        #t))
    (check-source!)
    (when (head:buffer-conflicted b) (refuse-file! "Resolve the conflicts first"))
    (file:run-pre-save-hooks! path)
    (check-source!)
    (let-values ([(text revision facts) (head:buffer-state b)])
      (when (> (cond [(assq 'conflicts facts) => cdr] [else 0]) 0)
        (refuse-file! "Resolve the conflicts first"))
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
           (stale-save! b path disk review write!)]
          [else (write! review)]))))

  (define (back-up! path disk)
    ;; What a file holds before a save writes over it, kept as a backup: a
    ;; trashed buffer named after the file with .bak, its backup fact the
    ;; path, the file's stamp and a checksum of its text, for restore! to
    ;; bring back and the buffet to list; a version the backups already
    ;; hold is not kept twice. The backup's name.
    (let* ([text (car disk)] [sum (file:checksum text)]
           [same (find (lambda (entry)
                         (let ([backup (list-ref entry 4)])
                           (and (string=? (car backup) path) (string=? (caddr backup) sum))))
                       (backup-entries))])
      (if same
          (cadr same)
          (let* ([lines (file:lines text)]
                 [detected (mode:detect path (vector-ref lines 0))]
                 [id (store:create! head:ui-actor (string-append (file:base-name path) ".bak") lines
                       (list (cons 'trailing (file:ends-in-newline? text))
                             (cons 'mode (and detected (mode:name detected)))
                             (list 'trashed (now-seconds) head:ui-actor)
                             (list 'backup path (cdr disk) sum)))])
            (store:buffer-name id)))))

  (define (reload-from-disk! b path disk . source)
    ;; The buffer reloaded from its file through the store: the disk's text
    ;; the baseline again, the buffer's entries reapplied on top, an entry the
    ;; disk contradicts pending as a conflict with the disk's side shown;
    ;; -> (values status detail), applied with (revision conflicts), and the
    ;; echo told under the command that reloaded, reload! unless the caller
    ;; names its own. Nothing is written.
    (let ([disk (review-disk! path disk)])
      (let-values ([(status detail)
                    (head:store-reload! b (file:lines (car disk))
                      (list (cons 'trailing (file:ends-in-newline? (car disk)))
                            (cons 'base (car disk)) (cons 'stamp (cdr disk))))])
        (when (eq? status 'applied)
          (let ([n (length (cadr detail))])
            (parameterize ([message-source (if (pair? source) (car source) 'reload!)])
              (set-message!
                (if (zero? n)
                    (format "Reloaded ~a, the buffer's edits merged" path)
                    (format "Reloaded ~a with ~a conflict~a" path n (if (= n 1) "" "s")))))))
        (values status detail))))

  (define (merge-failure detail)
    ;; why the store could not merge the disk's changes, for the echo
    (case detail
      [(no-base) "without a saved baseline to merge from"]
      [(basis-too-old) "past the log's reach to merge"]
      [(pending-edits) "resolve the pending conflicts first; further edits have been preserved"]
      [else (format "not merged (~a)" detail)]))

  (define (reread-through-store! b path disk why)
    ;; the disk adopted as one undoable edit where its changes could not be
    ;; merged: the buffer's text stays in the log, and undo brings it back;
    ;; nothing asks
    (let-values ([(status detail)
                  (head:store-reread! b (file:lines (car disk))
                    (list (cons 'trailing (file:ends-in-newline? (car disk)))
                          (cons 'base (car disk)) (cons 'stamp (cdr disk))))])
      (when (eq? status 'applied) (head:clamp-buffer-positions! b))
      (parameterize ([message-source 'visit-file!])
        (set-message!
          (if (eq? status 'applied)
              (format "Reread ~a, its changes on disk ~a; undo brings the buffer's text back" path why)
              (format "~a changed on disk, ~a, and could not be reread: ~a" path why detail))))
      (eq? status 'applied)))

  (define (reopen-changed-file! b path disk)
    ;; The file changed on disk since the buffer's baseline: reload it, and
    ;; where the store cannot, a baseline the log no longer reaches say,
    ;; reread it instead, undoably
    (let-values ([(status detail) (reload-from-disk! b path disk 'visit-file!)])
      (cond [(eq? status 'applied) #t]
            [(eq? detail 'pending-edits) (set-message! (merge-failure detail)) #f]
            [else (reread-through-store! b path disk (merge-failure detail))])))

  (define (current-file-disk)
    ;; the current buffer, its file's path and the disk's state, for the
    ;; commands that take the disk; refused without a file or unreadable
    (let ([b (head:current-buffer)])
      (let-values ([(text revision facts) (head:buffer-state b)])
        (let ([path (cond [(assq 'file facts) => cdr] [else #f])])
          (unless path (refuse-file! "This buffer visits no file"))
          (let ([disk (guard (ex [else #f]) (read-disk path))])
            (unless disk (refuse-file! (format "Cannot read ~a" path)))
            (values b path disk revision facts))))))

  (edoc "Reread the current buffer's file: the disk's text replaces the buffer's as one undoable edit, settling the pending conflicts, so the red !! goes and undo brings the text and the conflicts back; nothing is written."
        (edits))
  (define (reread!)
    (check-editable!)
    (let-values ([(b path disk revision facts) (current-file-disk)])
      (let-values ([(status detail)
                    (head:store-reread! b (file:lines (car disk))
                      (list (cons 'trailing (file:ends-in-newline? (car disk)))
                            (cons 'base (car disk)) (cons 'stamp (cdr disk))))])
        (case status
          [(applied)
           (head:clamp-buffer-positions! b)
           (parameterize ([message-source 'visit-file!]) (set-message! (format "Reread ~a" path)))]
          [else (refuse-file! (format "~a could not be reread: ~a" (file:base-name path) detail))]))))

  (edoc "Reload the current buffer's file through the store: the disk's text becomes the baseline again and the buffer's edits are merged on top, a collision pending as a conflict, the red !!; where the store cannot reload, the echo says so and C-x C-r rereads. Reopening the file and editing it after a change on disk reload it the same way.")
  (define (reload!)
    (let-values ([(b path disk revision facts) (current-file-disk)])
      (let-values ([(status detail) (reload-from-disk! b path disk)])
        (unless (eq? status 'applied)
          (refuse-file! (format "~a could not be reloaded: ~a" (file:base-name path) (merge-failure detail)))))))

  (define (stale-save! b path disk review write!)
    ;; The file changed on disk since the baseline: reload first, then write
    ;; when no conflict pends, else leave the conflicts to the user; where
    ;; the store cannot reload, reread instead, undoably
    (let-values ([(status detail) (reload-from-disk! b path disk)])
      (cond
        [(and (eq? status 'applied) (null? (cadr detail)))
         (write! (list (car review) (cons 'base (car disk))))]
        [(eq? status 'applied) (refuse-file! "Resolve the conflicts first")]
        [(eq? detail 'pending-edits) (refuse-file! (merge-failure detail))]
        [else
         ;; the disk's changes cannot be merged: the disk is reread, undoably,
         ;; and the save waits; undo brings the buffer's text back to save
         (reread-through-store! b path disk (merge-failure detail))
         (refuse-file! (format "~a changed on disk and was reread instead of saved; undo brings your text back"
                               (file:base-name path)))])))

  (edoc "A buffer's text as its file would hold it: the lines joined with newlines, ending in one when the buffer keeps a trailing newline."
        (b buffer "the buffer to read")
        (returns string))
  (define (buffer-text b)
    ;; b's text as its file would hold it; b by name or as its literal
    (let ([b (edoc:type-value 'buffer b)])
      (file:text (head:buffer-lines b) (head:buffer-trailing b))))

  (edoc "Whether a buffer can be discarded without losing work: unmodified, or marked disposable; #f when its state cannot be read."
        (b buffer "the buffer to judge")
        (returns boolean))
  (define (buffer-clean? b)
    ;; Discard decisions use one current snapshot, not an empty/stale
    ;; head cache.  Read-only protects editing, not the lifetime of work.
    ;; Generated tools explicitly opt into disposal; failed reads fail closed.
    (guard (ex [else #f])
      (let-values ([(text revision facts) (head:buffer-state (edoc:type-value 'buffer b))])
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

  (edoc "Run body with a region selected: its buffer current, the mark at its start and point at its end; the previous selection and point return on exit and on escape: (with-region (region (buffer \"a\") '(0 . 0) '(4 . 0)) (search:replace! \"x\" \"y\"))."
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


  ;;; Buffers: creation and the trash ---------------------------------------------

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

  (define (trashed-entries)
    ;; (id name killed-at actor backup), the newest kill first; backup is
    ;; the backup fact, (path stamp checksum), of a version a save kept
    (list-sort (lambda (a b) (or (> (caddr a) (caddr b)) (and (= (caddr a) (caddr b)) (> (car a) (car b)))))
      (filter values
        (map (lambda (id)
               (let ([t (store:property id 'trashed #f)])
                 (and t (list id (store:buffer-name id) (car t) (cadr t) (store:property id 'backup #f)))))
             (store:buffer-list)))))

  (define (trash-entries) (filter (lambda (entry) (not (list-ref entry 4))) (trashed-entries)))
  (define (backup-entries) (filter (lambda (entry) (list-ref entry 4)) (trashed-entries)))
  (define (trashed-ids) (map car (trash-entries)))

  (edoc "Kill a buffer at once: a shared document goes to the trash, where restore! finds it under its name for store:trash-retention days; disposable output is deleted and a local buffer forgotten."
        (b buffer "the buffer to kill"))
  (define (kill-buffer! b)
    (let* ([b (edoc:type-value 'buffer b)] [id (head:buffer-store-id b)] [name (head:buffer-name b)])
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

  (edoc "The trashed buffers, the backups aside, newest first, as (name killed-at actor): killed-at in UTC seconds; each expires store:trash-retention days after it was killed."
        (returns (list-of list)))
  (define (trash)
    (map (lambda (entry) (list-head (cdr entry) 3)) (trash-entries)))

  (edoc "The backups, the versions saves wrote over, newest first, as (name path observed stamp checksum actor): the file's path, when it was read in UTC seconds, its modification time then as (seconds . nanoseconds) or #f, the checksum of its text and who saved; each expires store:trash-retention days after it was read, and a file keeps store:backups-kept of them."
        (returns (list-of list)))
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
                           (cons (cadr entry)
                                 (if backup
                                     (format "backup of ~a, ~a ago" (file:abbreviate (car backup)) ago)
                                     (format "killed ~a ago" ago)))))
                       (trashed-entries)))))
    (write (lambda (v) (format "~s" v)))
    (within string))

  (edoc "Bring a buffer back from the trash, a backup included, the newest of that name, with its text and history, and show it in the current window; it takes a unique name when another buffer holds its own."
        (name trashed "the buffer's name in the trash")
        (returns buffer))
  (define (restore! name)
    (let ([entry (find (lambda (entry) (string=? (cadr entry) name)) (trashed-entries))])
      (unless entry (error 'restore! "no such buffer in the trash" name))
      (let ([id (car entry)])
        (store:set-properties! head:ui-actor id '((trashed . #f) (backup . #f)))
        (let ([b (head:adopt-store-buffer! id)])
          (unless b (error 'restore! "the buffer did not come back" name))
          (head:show-buffer! b)
          (parameterize ([message-source 'restore!])
            (set-message! (format "Restored ~a" (head:buffer-name b))))
          b))))

  (edoc "Delete every trashed buffer for good, the backups kept; how many went."
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
  (define (mode-tool registered)
    ;; the current buffer's mode's registered indenter, formatter or flag, or #f
    (let ([m (mode:name-of (head:window-buffer current-window))])
      (and m (registered m))))

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
    (let ([indent (mode-tool mode:indenter)])
      (if (not indent)
          (begin (set! message "No indenter for this mode") #f)
          (let* ([b (head:window-buffer current-window)]
                 [source (head:edit-basis b)]
                 [v (car source)]
                 [wanted (head:point)] [selected (head:mark)]
                 [last (min to (- (vector-length v) 1))]
                 [cols (let settle ([r from]
                                    [cs (indent b from last)]
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

  (edoc "Indent the current line by the mode's indenter, cycling through its stops; point lands on the indentation."
        (edits))
  (define (indent-line!)
    ;; TAB's work: indent the current line, cycling through its stops
    ;; -- the nearest stop right of the current indentation, wrapping
    ;; -- and land on the indentation (a blank line pads out to it);
    ;; point already past it stays with its text.
    (let ([indent (mode-tool mode:indenter)])
      (if (not indent)
          (set! message "No indenter for this mode")
          (let* ([b (head:window-buffer current-window)]
                 [source (head:edit-basis b)]
                 [wanted (head:point)] [selected (head:mark)]
                 [row (car wanted)]
                 [lead (leading-blanks (vector-ref (car source) row))]
                 [cols (indent b row row)]
                 [col (and (pair? cols)
                           (cycle-stops (car cols) lead))])
            (when col
              (unless (parameterize ([edit-source source] [edit-point wanted] [edit-mark selected])
                        (apply-indent! row (list col) #t))
                (when (and (eq? (car source) (head:buffer-lines b)) (< point-col col))
                  (set! point-col col)))))))
    (void))

  (edoc "What TAB does: indent the current line when the mode's indenter asked for it, else nothing."
        (edits))
  (define (indent-tab!)
    ;; TAB: the mode indents when it asked to; otherwise nothing.
    (when (mode-tool mode:indent-on-tab?)
      (indent-line!)))

  (edoc "Indent the lines between mark and point by the mode's indenter, each settling on its nearest stop."
        (edits))
  (define (indent-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (indent-rows! from to)
            (set! message (format "Indented ~a line~a" (+ (- to from) 1)
                                  (if (= from to) "" "s"))))))
    (void))

  (edoc "Indent every line of the current buffer by the mode's indenter."
        (edits))
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
    (let ([format-lines (mode-tool mode:formatter)])
      (cond
        [(not format-lines) (set! message "No formatter for this mode") #f]
        [else
         (let* ([b (head:window-buffer current-window)]
                [source (head:edit-basis b)]
                [v (car source)]
                [wanted (head:point)]
                [last (min to (- (vector-length v) 1))]
                [lines (format-lines b from last)])
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
                               (cons 'undo (if (= last (- (vector-length v) 1)) '((trailing . #t)) '()))))
              #t]))])))

  (edoc "Rewrite the lines between mark and point with the mode's formatter."
        (edits))
  (define (format-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (format-rows! from to)
            (set! message "Formatted region"))))
    (void))

  (edoc "Rewrite the whole current buffer with the mode's formatter."
        (edits))
  (define (format-buffer!)
    (let ([n (vector-length (head:buffer-lines (head:window-buffer current-window)))])
      (when (format-rows! 0 (- n 1))
        (set! message (format "Formatted ~a lines" n))))
    (void))

  ;;; Viewport commands -------------------------------------------------------

  ;; Painting and the frame are the painter's (paint); these are the
  ;; commands over its viewport logic -- paging and point placement --
  ;; and the head's side of the interaction protocol.

  (edoc "Scroll the selected window by a fraction of its page, direction -1 for up and 1 for down, and put point in the middle; at an edge already reached, point moves to that edge."
        (direction integer "-1 for up, 1 for down")
        (fraction integer "the divisor of the page: 1 for a whole page, 8 for a wheel tick"))
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
        (returns boolean "whether the file was written"))
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
      (let ([s (parameterize ([prompt:completion-kind "file"] [prompt:completion-label file-completion-label]
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
    (let ([b (head:find-tool-buffer "*buffet*")])
      (if b
          (let ([w (window:display! b)])
            (when w
              (window:focus! w)
              (head:dispatch-app-event! "FOCUS")
              (set! message "")))
          (set-message! "The <buffet> app is not available"))))

  (edoc "Quit this head at once: shared text stays in the base, the screen is checkpointed for the next attach, and the exit notice names every buffer with unsaved work.")
  (define (quit!)
    (head:depart!))

  ;;; Pasting and typed runs --------------------------------------------------

  (edoc "Insert a bracketed paste, the PASTE key, as one edit, its newlines real line breaks."
        (edits))
  (define (paste-into-buffer!)
    ;; A bracketed paste: the whole text becomes one labeled edit, its
    ;; newlines becoming real line breaks.
    (let ([text (head:read-paste)])
      (unless (string=? text "")
        (call-as-one-edit! (format "insert ~s" text)
          (lambda ()
            (insert-text! (string:join (tty:paste-lines text) "\n")))))))

  (define (typing? action)
    ;; whether a key's action typed: type! itself, or its call from SELF-INSERT
    (or (eq? action type!)
        (and (keymap:call-action? action) (eq? (keymap:call-action-procedure action) type!))))

  (edoc "Type text: inserted at point as typing does, continuing the run of typing, backspaces and deletes before it, so a run undoes as one step and shares one batch; SELF-INSERT, any character without a binding of its own, runs it with the character typed."
        (text string "the text to type")
        (edits))
  (define (type! text)
    ;; one key of the typing run, above
    (unless (string=? text "")
      (let* ([b (head:window-buffer current-window)]
             [run (typing-run b)] [chain (or run no-run)]
             [source (edit-basis-for b)] [row point-row] [col point-col])
        (typing-edit! b run (string-append (list-ref chain 4) text) (list-ref chain 5) (list-ref chain 6)
          (lambda ()
            (parameterize ([edit-source source])
              (submit-edit! b (text:make-span row col row col) (split-inserted-lines text))))))))

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

  (edoc "Insert a line break after point, leaving point where it is."
        (edits))
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

  ;;; Registration ----------------------------------------------------------------

  ;; Everything the layer registers -- owned by edit, so a reload
  ;; retracts and remakes it; what the loop and the seams ask of the
  ;; commands is installed here too.
  (edoc "Install the command layer: log presentation, the file formatters, status hints, the default key bindings, the loop's hooks and the buffet.")
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
          ("C-x k" ,(keymap:call kill-buffer! head:current-buffer))
          ("C-c a" ,(keymap:prefill answer!))))
      (for-each
        (lambda (entry)
          (keymap:bind-default! 'prompt (car entry) (cadr entry)))
        `(("C-g" ,prompt:cancel!) ("ESC" ,prompt:cancel!) ("RET" ,prompt:accept!)
          ("C-a" ,prompt:beginning!) ("HOME" ,prompt:beginning!)
          ("C-b" ,prompt:backward!) ("LEFT" ,prompt:backward!)
          ("C-e" ,prompt:end!) ("END" ,prompt:end!) ("C-f" ,prompt:forward!) ("RIGHT" ,prompt:forward!)
          ("UP" ,prompt:up!) ("DOWN" ,prompt:down!) ("C-d" ,prompt:delete-forward!)
          ("DEL" ,prompt:delete-forward!) ("C-h" ,prompt:delete-backward!)
          ("BS" ,prompt:delete-backward!) ("C-k" ,prompt:kill!) ("C-y" ,prompt:yank!)
          ("TAB" ,prompt:complete!) ("S-TAB" ,prompt:alternate-complete!)
          ("M-." ,prompt:inspect!) ("M-RET" ,prompt:newline!) ("PASTE" ,prompt:paste!)
          ("SELF-INSERT" ,(keymap:call prompt:type! head:typed-text))))
      #t)
    ;; The loop's hooks live in (head): how to open the file
    ;; argument, how to quit (the modified-buffers check), and what runs
    ;; after every key
    (begin
      (head:set-file-opener! visit-file!)
      (head:set-quit-command! quit!)
      (head:set-review-viewer! view-quit-buffers!)
      (head:add-pre-redraw-hook! publish-copy-changes!)
      (head:add-pre-redraw-hook! reload-if-due!)
      (head:set-after-key! clamp-point!))

    (doc:register!
      '(((undo-scope) (("parameter" . "(undo-scope [scope])")) "symbol"
         ("(head edit)") edit "Editing commands" #f
         "Choose the default scope of `undo!` and C-_. `mine` (the default) selects this head's latest live action; `all` selects the latest live action of any actor. The preference belongs to the head. Local buffers use their own history in either mode.")
        ((undo!) (("procedure" . "(undo!)")) "string"
         ("(head edit)") edit "Editing commands" #f
         "Undo one action in the current buffer within `undo-scope`, `mine` or `all`. Shared changes use attributed inverse edits; an overlap, changed text property, or unavailable history refuses without changing any part of the action.")
        ((redo!) (("procedure" . "(redo!)")) "string"
         ("(head edit)") edit "Editing commands" #f
         "Reverse this head's latest undo, including an undo of another actor's action. Redo uses the same overlap checks and is independent of `undo-scope`. A fresh edit by this head invalidates its redo.")
        ((undo-actor!) (("procedure" . "(undo-actor! actor)")) "string"
         ("(head edit)") edit "Editing commands" #f
         "Undo the named actor's latest live action in the current shared buffer without changing `undo-scope`. Both the original author and this head's request are retained in the history and audit log.")))
  )

) ;; library (edit)
