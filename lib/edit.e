;; edit.e -- the command layer: the library (edit), the e editor's
;; default app.
;;
;; Everything a user does to text and to the seat that shows it: the
;; buffer and window commands, visiting, saving, merging with the disk,
;; editing with undo, the kill ring and the clipboard, indentation and
;; formatting through the modes' registered indenters, mouse actions,
;; the default key bindings, and the generic editing helpers (regions,
;; replace, conflict resolution).  It composes the seams below --
;; store, head, paint, prompt, file, mode, keymap -- and is what M-x
;; sees bare: the loader imports (edit) into the top level.
;;
;; Hot-reloadable like any module: its registrations (bindings, hooks,
;; formatters, descriptions) are made in init!, owned by edit, so a
;; reload retracts and remakes them; the loop in (main) reaches the
;; layer only through setters it installs there.  Internals -- all
;; mutable state included -- are invisible outside the library; the
;; exports are the editor's public command API.
;;
;; Every generic helper here takes an optional `where` argument saying
;; what to operate on, normalized by regions-of: omitted -- the
;; selected region, or the whole current buffer; a buffer, or its name
;; -- all of it; a region; a predicate -- the buffers it accepts; or a
;; list of any of these.  A region is a slice of one buffer between two
;; (row . col) points, and prints as the expression that rebuilds it,
;; like buffers do:  (region (buffer "e") '(0 . 0) '(12 . 5)).

(library (edit)
  (export init!
          region region? region-buffer region-start region-end
          regions-of region-text
          replace-all! count-matches replace!!
          next-conflict! keep-mine! keep-disk!
          list-buffers!
    ;; the command layer
    (rename (head:scrollbar scrollbar)
            (head:scrollbar-position scrollbar-position)
            (head:line-numbers line-numbers)
            (paint:wrap-lines wrap-lines)
            (paint:scroll-margin scroll-margin))
    ;; state, read-only
    current-buffer buffer-list point mark
    buffer-text buffer-clean?
    buffer-line  buffer-line-count

    editor-symbol?
    (rename (lookup-buffer buffer))   ; buffers print as (buffer "name")
    (rename (lookup-window window))   ; windows print as (window n)
    ;; buffers, windows, files
    visit-file! save-file! save!! save-as!! find-file!!
    show-buffer! kill-buffer! display-buffer! pop-up-or-reuse! buffer-append!
    fresh-buffer
    set-buffer-read-only! set-buffer-wrap! set-buffer-name!
    call-with-buffer
    switch-buffer!! kill-buffer!!
    split-window! split-window-right!
    delete-window! delete-other-windows! other-window!
    focus-window-up! focus-window-down! focus-window-left! focus-window-right!
    resize-window! wrap!
    line-numbers!
    ;; editing and movement
    insert-text! replace-region-text! newline! delete-forward! backspace!
    kill-line! kill-region! copy-region! yank! undo! redo!
    copy-to-kill-buffer! current-kill-ring
    forward-kill-ring-to-system-clipboard
    set-mark-command! beginning-of-line! end-of-line! keyboard-quit!
    redraw-command! open-line! page-up! page-down!
    page-window-fraction! set-point-without-scroll!

    previous-line! next-line! beginning-of-buffer! end-of-buffer!
    move-left! move-right! indent-tab!
    call-as-one-edit!
    indent-line! indent-region! indent-buffer! format-region! format-buffer!
    move-horizontal! move-vertical! goto-point!
    quit!!
    ;; extending the editor

    describe-key!!

    register-indenter! register-formatter!

    indent-on-tab!



    selected-window select-window!
    set-message!
    mouse!
    present-log-entry! present-log-entries!


    message-source message-progress



    app-event-position app-event-buffer-position app-event-button
  )
  ;; The system-specific layer -- libc, termios, signals -- comes
  ;; from (sys).
  (import (chezscheme) (prefix (sys) sys:)
          (prefix (store) store:) (prefix (text) text:)
          (prefix (kernel) kernel:) (prefix (actor) actor:)
          (prefix (log) log:) (prefix (style) style:)
          (prefix (keymap) keymap:) (prefix (tty) tty:)
          (prefix (echo) echo:) (prefix (head) head:)
          (prefix (paint) paint:) (prefix (string) string:)
          (prefix (mode) mode:) (prefix (file) file:)
          (prefix (prompt) prompt:) (prefix (main) main:)
          (prefix (doc) doc:))

  ;; The bindings Chez itself provides, so that the editor's public API
  ;; (and module definitions) can be told apart from builtins -- M-x
  ;; completion highlights them.
  (define baseline-bindings
    (let ([table (make-eq-hashtable)])
      (for-each (lambda (sym) (eq-hashtable-set! table sym #t))
                (environment-symbols (scheme-environment)))
      table))

  (define (editor-symbol? sym)
    (and (top-level-bound? sym)
         (not (eq-hashtable-ref baseline-bindings sym #f))))

  ;;; Buffers and windows ----------------------------------------------------

  ;; The seat's buffer record lives in (head) -- the client-side cache
  ;; of a store buffer plus per-seat presentation; the commands reach
  ;; the seat's lists and selection through the identifier-syntax
  ;; facades below (a facade sweep is on the tech-debt ledger).
  (define buffer-line-numbers-setting-set!
    head:buffer-line-numbers-setting-set!)
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
    (identifier-syntax [id (head:current)]
      [(set! id v) (head:set-current! v)]))
  ;; The (store) is the master copy of every buffer's text; this
  ;; seat is one of its clients.  A buffer record's lines field is a
  ;; cache of the store's immutable text vector, adopted after every
  ;; operation -- nothing here mutates a line vector in place.  Edits
  ;; enter the store transactionally (whole-line and line-splice
  ;; granularity), wholesale replacements are resets, and foreign
  ;; actors' edits flow back before each frame (head:before-frame!).
  ;; If the store refuses (a foreign edit overlapped mid-command) or
  ;; breaks, the seat keeps editing: content wins locally and the store
  ;; is reset to match.


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
  (define (set-line! n s)
    ;; the store is the master: the edit goes there, the cache adopts
    (let ([b (head:window-buffer current-window)])
      (head:store-edit! b (text:make-span n 0 n
                                          (string-length (vector-ref lines n)))
                        (list s))))

  (define (splice-lines! from to inserted)
    ;; Replace whole lines [from, to) of the current buffer as one
    ;; transactional store edit -- so other actors' marks and bases
    ;; survive ordinary typing.
    (let* ([b (head:window-buffer current-window)]
           [old lines]
           [count (vector-length old)])
      (head:store-edit!
        b
        (cond
          [(< to count) (text:make-span from 0 to 0)]
          [(> from 0)
           (text:make-span (- from 1)
                           (string-length (vector-ref old (- from 1)))
                           (- count 1)
                           (string-length (vector-ref old (- count 1))))]
          [else
           (text:make-span 0 0 (- count 1)
                           (string-length (vector-ref old (- count 1))))])
        (cond
          [(< to count) (append inserted '(""))]
          [(> from 0) (cons "" inserted)]
          [(null? inserted) '("")]
          [else inserted]))))

  (define (editor-snapshot)
    ;; the cache vectors are immutable now: snapshots share, never copy
    (list lines point-row point-col trailing-newline? modified?
          (head:buffer-store-rev (head:window-buffer current-window))))

  (define (restore-snapshot! snapshot)
    ;; The snapshot was just popped off a history stack, so nothing else
    ;; references its line vector and it can be adopted without copying.
    (set! lines (car snapshot))
    (set! point-row (cadr snapshot))
    (set! point-col (caddr snapshot))
    (set! trailing-newline? (cadddr snapshot))
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
    (paint:invalidate-screen-cache!))

  ;; Undo entries are labeled with the user-level action that made them
  ;; -- "insert \"hello\"", "(replace-all! \"xx\" \"yy\")" -- and undo
  ;; and redo report the label.  Inside a call-as-one-edit! group, the
  ;; box holds (label . buffers-snapshotted): one entry per buffer the
  ;; group touches, labeled with the group's label (or, lacking one,
  ;; that buffer's first edit's).
  (define edit-group (make-parameter #f))

  (define (push-undo! label)
    (vector-set! history 0 (cons (cons label (editor-snapshot))
                                 (vector-ref history 0)))
    (vector-set! history 1 '()))

  (define (check-disk-before-edit!)
    ;; The start of an edit session -- one undo entry; chained typing
    ;; checks once: if the file changed on disk meanwhile, mark the
    ;; buffer stale -- a red !! in the status bar -- and let the edit
    ;; proceed; the save guard still compares contents.  The mtime
    ;; raises the suspicion cheaply; the content confirms it, so a
    ;; mere touch passes silently.
    (let ([b (head:window-buffer current-window)])
      (when (and file-name (head:buffer-base b))
        (let ([stamp (file:stamp file-name)])
          (unless (equal? stamp (head:buffer-stamp b))
            (let ([disk (guard (ex [else #f])
                          (and (file-exists? file-name)
                               (file:read file-name)))])
              (unless (and disk (string=? disk (head:buffer-base b)))
                (head:buffer-stale-set! b #t))
              (head:buffer-stamp-set! b stamp)))))))

  (define (record-edit! label)
    ;; Every editing command passes through here before touching the
    ;; buffer, so this is also where read-only buffers are protected:
    ;; #t forbids all edits, and a procedure decides per edit.
    (let ([guard (head:buffer-read-only (head:window-buffer current-window))])
      (when (if (procedure? guard) (not (guard)) guard)
        (raise (condition (kernel:make-read-only-error)
                          (make-message-condition "buffer is read-only")))))
    (unless (suppress-history)
      (check-disk-before-edit!)
      (let ([group (edit-group)]
            [b (head:window-buffer current-window)])
        (cond [(not group) (push-undo! label)]
              [(memq b (cdr (unbox group))) (void)]
              [else (push-undo! (or (car (unbox group)) label))
                    (set-box! group (cons (car (unbox group))
                                          (cons b (cdr (unbox group)))))]))))

  (define (relabel-last-edit! label)
    (let ([h (vector-ref history 0)])
      (when (pair? h) (set-car! (car h) label))))

  (define (call-as-one-edit! label thunk)
    ;; Bundle every edit thunk makes into one labeled undo step per
    ;; buffer it touches -- and none for buffers it does not edit.
    ;; Nested groups defer to the outermost.
    (if (edit-group)
        (thunk)
        (parameterize ([edit-group (box (cons label '()))]) (thunk))))

  (define (foreign-edits-since? b rev)
    ;; did another actor edit this buffer's store copy after rev?
    (and (head:buffer-store-id b)
         (guard (ex [else #f])
           (exists (lambda (entry)
                     (and (> (car entry) rev)
                          (not (equal? (cadr entry) head:ui-actor))))
                   (store:history (head:buffer-store-id b) 256)))))

  (define (history-shift! from to verb)
    ;; The report -- what was undone or redone -- is also returned, so
    ;; M-x (undo!) shows it as its result.  Restoring a snapshot from
    ;; before another actor's edit would silently erase their work, so
    ;; that refuses instead, like store:undo! reports 'blocked.
    (set! message
      (cond
        [(null? (vector-ref history from))
         (format "No further ~a information" (string-downcase verb))]
        [(foreign-edits-since?
           (head:window-buffer current-window)
           (let ([snapshot (cdr (car (vector-ref history from)))])
             (if (> (length snapshot) 5) (list-ref snapshot 5) 0)))
         (format "~a blocked: another actor edited this buffer since"
                 verb)]
        [else
         (let ([entry (car (vector-ref history from))])
           (vector-set! history from (cdr (vector-ref history from)))
           (vector-set! history to
             (cons (cons (car entry) (editor-snapshot))
                   (vector-ref history to)))
           (restore-snapshot! (cdr entry))
           (string:elide (if (car entry) (format "~a ~a" verb (car entry)) verb)
             cols))]))
    message)

  (define (undo!) (history-shift! 0 1 "Undo"))

  (define (redo!) (history-shift! 1 0 "Redo"))


  ;;; Point, mark, and editing ----------------------------------------------

  (define (changed!)
    (set! modified? #t) (set! message "") (set! mark-active? #f)
    (set! goal-pos #f))

  (define (ordered-region) ; -> start-row start-col end-row end-col
    (if (or (< point-row mark-row)
            (and (= point-row mark-row) (< point-col mark-col)))
        (values point-row point-col mark-row mark-col)
        (values mark-row mark-col point-row point-col)))

  (define (clamp-point!)
    (set! point-row (max 0 (min point-row (- (vlen) 1))))
    (set! point-col (max 0 (min point-col (string-length (current-line))))))

  (define (move-left!)
    (cond [(> point-col 0) (set! point-col (- point-col 1))]
          [(> point-row 0)
           (set! point-row (- point-row 1))
           (set! point-col (string-length (current-line)))]))

  (define (move-right!)
    (cond [(< point-col (string-length (current-line)))
           (set! point-col (+ point-col 1))]
          [(< point-row (- (vlen) 1))
           (set! point-row (+ point-row 1)) (set! point-col 0)]))

  (define (move-horizontal! delta)
    ;; Move point delta characters, negative to the left, crossing line
    ;; ends the way repeated single steps do.
    (if (< delta 0)
        (do ([i 0 (- i 1)]) ((= i delta)) (move-left!))
        (do ([i 0 (+ i 1)]) ((= i delta)) (move-right!))))

  (define (goto-point! p)
    ;; Move point straight to (row . col), clamped into the buffer.
    (set! point-row (max 0 (min (car p) (- (vlen) 1))))
    (set! point-col (max 0 (min (cdr p) (string-length (current-line)))))
    ;; App interaction state belongs to the buffer: every window showing the
    ;; same app mirrors its cursor and selection row.
    (when (head:app-buffer? (current-buffer))
      (head:buffer-spot-row-set! (current-buffer) point-row)
      (head:buffer-spot-col-set! (current-buffer) point-col)
      (for-each
        (lambda (w)
          (when (eq? (head:window-buffer w) (current-buffer))
            (head:window-prow-set! w point-row)
            (head:window-pcol-set! w point-col)))
        windows)))

  ;; Vertical moves aim for a goal column, so point comes back to it after
  ;; passing through shorter lines (as in Emacs).  The goal survives exactly
  ;; as long as each command finds point where the previous vertical move
  ;; left it; anything else that moves point starts a fresh goal.
  (define goal-col 0)
  (define goal-pos #f)

  (define (move-vertical! delta)
    ;; By buffer lines -- or by visual rows in a soft-wrapping window,
    ;; where up and down walk a long line's segments (C-a and C-e
    ;; still treat it as one line).  The goal column is visual when
    ;; wrapped.
    (define wrapped? (paint:window-wrapped? current-window))
    (define (land! breaks k)
      ;; the goal column within segment k, clamped into it
      (set! point-col
        (min (+ (paint:segment-start breaks k) goal-col)
             (paint:segment-close breaks k (string-length (current-line))))))
    (unless (equal? goal-pos (cons point-row point-col))
      (set! goal-col
        (if wrapped?
            (let ([breaks (paint:line-breaks current-window (current-line))])
              (- point-col
                 (paint:segment-start breaks (paint:segment-of breaks point-col))))
            point-col)))
    (if wrapped?
        (let step ([n delta])
          (cond
            [(zero? n) (void)]
            [(negative? n)
             (let* ([breaks (paint:line-breaks current-window (current-line))]
                    [seg (paint:segment-of breaks point-col)])
               (cond
                 [(> seg 0)                ; up, within the same line
                  (land! breaks (- seg 1))]
                 [(> point-row 0)          ; onto the line above's last row
                  (set! point-row (- point-row 1))
                  (let ([breaks (paint:line-breaks current-window
                                                   (current-line))])
                    (land! breaks (- (vector-length breaks) 1)))]))
             (step (+ n 1))]
            [else
             (let* ([breaks (paint:line-breaks current-window (current-line))]
                    [seg (paint:segment-of breaks point-col)])
               (cond
                 [(< (+ seg 1) (vector-length breaks))
                  (land! breaks (+ seg 1))]  ; down, within the same line
                 [(< point-row (- (vlen) 1))
                  (set! point-row (+ point-row 1))
                  (land! (paint:line-breaks current-window (current-line)) 0)]))
             (step (- n 1))]))
        (begin
          (set! point-row (max 0 (min (+ point-row delta) (- (vlen) 1))))
          (set! point-col (min goal-col (string-length (current-line))))))
    (set! goal-pos (cons point-row point-col)))

  (define (split-inserted-lines s)
    ;; Unlike split-lines, retain an empty final part: inserting "a\n"
    ;; creates a new empty row and leaves point on it.
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond [(= i n) (reverse (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
              [else (loop (+ i 1) start acc)]))))

  (define (insert-text! s)
    ;; Buffer rows never contain newline characters.  Programmatic inserts
    ;; get the same structural treatment as a paste or repeated newline!.
    (unless (string=? s "")
      (record-edit! (format "insert ~s" s))
      (let* ([row point-row]
             [col point-col]
             [old (current-line)]
             [parts (split-inserted-lines s)])
        (if (null? (cdr parts))
            (begin
              (set-line! row (string:insert old col s))
              (set! point-col (+ col (string-length s))))
            (let* ([last (car (reverse parts))]
                   [replacement
                    (append
                      (list (string-append (substring old 0 col) (car parts)))
                      (reverse (cdr (reverse (cdr parts))))
                      (list (string-append last (string:tail old col))))])
              (splice-lines! row (+ row 1) replacement)
              (set! point-row (+ row (- (length parts) 1)))
              (set! point-col (string-length last))))
        (changed!))))

  (define (newline!)
    (record-edit! "newline")
    (let ([s (current-line)])
      (set-line! point-row (substring s 0 point-col))
      (splice-lines! (+ point-row 1) (+ point-row 1)
                     (list (string:tail s point-col)))
      (set! point-row (+ point-row 1)) (set! point-col 0)
      (changed!)))

  (define (delete-forward!)
    (cond [(< point-col (string-length (current-line)))
           (record-edit!
             (format "delete ~s"
                     (string (string-ref (current-line) point-col))))
           (set-line! point-row
             (string:delete (current-line) point-col (+ point-col 1)))
           (changed!)]
          [(< point-row (- (vlen) 1))
           (record-edit! "delete newline")
           (set-line! point-row
             (string-append (current-line) (line-at (+ point-row 1))))
           (splice-lines! (+ point-row 1) (+ point-row 2) '())
           (changed!)]))

  (define (backspace!)
    (when (or (> point-col 0) (> point-row 0))
      (record-edit!
        (if (> point-col 0)
            (format "delete ~s"
                    (string (string-ref (current-line) (- point-col 1))))
            "delete newline"))
      (parameterize ([suppress-history #t])
        (move-left!) (delete-forward!))))

  ;;; Kill and yank ---------------------------------------------------------

  (define forward-kill-ring-to-system-clipboard
    (make-parameter
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
        (paint:ansi "\x1b;]52;c;" (base64-encode (string->utf8 text)) "\x1b;\\")
        (flush-output-port (sys:terminal-output-port)))))

  (define (killing?)
    ;; was the previous command a kill?  Consecutive kills accumulate
    ;; into a single kill-ring entry.
    (and (memq (head:last-command) (list kill-line! kill-region!)) #t))

  (define (kill! text)
    (set! kill-ring (if (killing?) (string-append kill-ring text) text))
    (publish-system-clipboard! kill-ring))

  (define (copy-to-kill-buffer! text)
    ;; Replace the text yanked by C-y without changing a buffer or point.
    (unless (string? text)
      (error 'copy-to-kill-buffer! "expected a string" text))
    (set! kill-ring text)
    (publish-system-clipboard! kill-ring)
    (void))

  (define (kill-line!)
    (let* ([s (current-line)] [n (string-length s)])
      (cond [(< point-col n)
             (let ([text (substring s point-col n)])
               (record-edit! (format "kill ~s" text))
               (kill! text)
               (set-line! point-row (substring s 0 point-col))
               (changed!))]
            [(< point-row (- (vlen) 1))
             (kill! "\n")
             (delete-forward!)])))

  (define (current-kill-ring)
    ;; The kill ring's text, for consumers outside the buffer -- the
    ;; terminal's yank, a future clipboard bridge.
    kill-ring)

  (define (yank!)
    ;; Kill-ring entries can span lines after consecutive C-k commands.  Insert
    ;; newlines as buffer structure rather than embedding them in a line string.
    (unless (string=? kill-ring "")
      (record-edit! (format "yank ~s" kill-ring))
      (parameterize ([suppress-history #t])
        (let ([parts (string:lines kill-ring)])
          (insert-text! (car parts))
          (for-each (lambda (part) (newline!) (insert-text! part))
                    (cdr parts))))))

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
    (if (= sr er)
        (set-line! sr (string:delete (line-at sr) sc ec))
        (let ([joined (string-append (substring (line-at sr) 0 sc)
                                     (string:tail (line-at er) ec))])
          (splice-lines! sr (+ er 1) (list joined)))))

  (define (replace-region-text! start end text)
    ;; Replace one ordered buffer range in a single structural operation.
    ;; Bulk editors use this instead of rebuilding a line once per match.
    (let ([sr (car start)] [sc (cdr start)]
          [er (car end)] [ec (cdr end)])
      (record-edit! "replace region")
      (parameterize ([suppress-history #t])
        (delete-region! sr sc er ec)
        (set! point-row sr)
        (set! point-col sc)
        (insert-text! text))
      (changed!)))

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

  (define (kill-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let-values ([(sr sc er ec) (ordered-region)])
          (if (and (= sr er) (= sc ec))
              (set! message "Empty region")
              (let ([text (text-between sr sc er ec)])
                (record-edit! (format "kill ~s" text))
                (kill! text)
                (delete-region! sr sc er ec)
                (set! point-row sr) (set! point-col sc)
                (changed!))))))

  ;;; Files -----------------------------------------------------------------

  (define (default-directory)
    ;; The directory of the current buffer's file (or the working
    ;; directory), with a trailing slash, absolute -- a file visited
    ;; by a relative path has a relative directory-part, useless as a
    ;; prompt offer on its own -- and abbreviated for display.
    (file:abbreviate
      (file:absolute
        (or (and file-name (file:directory-part file-name))
            (string-append (current-directory) "/")))))

  (define (unique-name base self)
    ;; base, or base<2>, base<3>, ... -- whichever no other buffer uses.
    (let ([used (make-hashtable string-hash string=?)])
      (for-each (lambda (b)
                  (unless (eq? b self)
                    (hashtable-set! used (head:buffer-name b) #t)))
                buffers)
      (let loop ([k 1])
        (let ([name (if (= k 1) base (format "~a<~a>" base k))])
          (if (hashtable-ref used name #f)
              (loop (+ k 1))
              name)))))

  (define (set-buffer-name! b name)
    (unless (and (head:buffer? b) (string? name) (> (string-length name) 0))
      (error 'set-buffer-name! "expected a buffer and nonempty name" b name))
    (head:buffer-name-set! b (unique-name name b))
    (head:mirror-rename! b)
    b)

  (define (file-buffer path)
    ;; A fresh buffer visiting path; #f (with a message) when it cannot be read.
    (if (file-exists? path)
        (guard (ex [else (parameterize ([message-source 'visit-file!])
                           (set-message! (format "Cannot open ~a: ~a"
                                                 path (kernel:condition-text ex))))
                         #f])
          (let* ([content (file:read path)]
                 [b (head:new-buffer (unique-name (file:base-name path) #f))])
            (head:buffer-lines-set! b (file:lines content))
            (head:buffer-trailing-set! b (file:ends-in-newline? content))
            (head:buffer-file-set! b path)
            (head:buffer-base-set! b content)
            (head:buffer-stamp-set! b (file:stamp path))
            (mode:assign! b)
            (log:add! 'visit-file! (cons "Loaded" path))
            b))
        (let ([b (head:new-buffer (unique-name (file:base-name path) #f))])
          (head:buffer-file-set! b path)
          (mode:assign! b)
          (log:add! 'visit-file! (cons "New file:" path))
          b)))

  (define (visit-file! path)
    ;; Switch to the buffer visiting path, creating it if necessary.
    ;; Reopening a buffer whose file changed on disk meanwhile raises
    ;; a buffer-only dialog: merge, reread, cancel.  Reopening never writes.
    (let ([path (file:visit-path path)])
      (cond [(find (lambda (b) (equal? (head:buffer-file b) path)) buffers)
             => (lambda (b)
                  (show-buffer! b)
                  (when (head:buffer-base b)
                    ;; Reopening is explicit and uncommon, so compare content
                    ;; every time. This catches preserved timestamps and a
                    ;; stale buffer whose cached stamp was already refreshed.
                    (let ([disk (guard (ex [else #f])
                                  (and (file-exists? path)
                                       (file:read path)))])
                      (cond
                        [(and disk (string=? disk (head:buffer-base b)))
                         (head:buffer-stamp-set! b (file:stamp path))
                         (head:buffer-stale-set! b #f)]
                        [disk (reopen-changed-file! b path disk)]
                        [else
                         (parameterize ([message-source 'visit-file!])
                           (set-message!
                             (format "Cannot reread ~a" path)))]))))]
            [(file-buffer path) => show-buffer!])))

  (define (read-disk-for-save path)
    ;; #f means genuinely absent.  An existing file that cannot be read
    ;; cannot be compared with the buffer's base, so fail closed instead
    ;; of treating it as a new destination and replacing it unchecked.
    (and (file-exists? path)
         (guard (ex [else
                     (raise
                       (condition
                         (kernel:make-refusal)
                         (make-message-condition
                           (format "Cannot verify ~a before saving: ~a"
                                   path (kernel:condition-text ex)))))])
           (file:read path))))
  (define (save-file! path*)
    ;; Saving is guarded by content, not clocks: the disk is read and
    ;; compared with the buffer's base (what it loaded or last saved).
    ;; A mismatch means somebody changed the file meanwhile -- the
    ;; save stops and asks: overwrite, merge three-way, or cancel.
    (define path (file:visit-path path*))
    (define adopted? (not (equal? path file-name)))  ; saving under a new name
    (define b (head:window-buffer current-window))
    (define disk (read-disk-for-save path))
    (define (write!)
      (guard (ex [else (parameterize ([message-source 'save-file!])
                         (set-message!
                           (format "Save failed: ~a" (kernel:condition-text ex))))
                       #f])
        (file:write! path lines trailing-newline?)
        (set! file-name path) (set! modified? #f)
        (begin
          (head:buffer-name-set! b (unique-name (file:base-name path) b))
          (head:mirror-rename! b))
        ;; re-detect the mode only when the name changed: a plain
        ;; re-save must not clobber a mode chosen by hand; adoption
        ;; also lifts read-only -- the buffer visits an ordinary
        ;; file now, whatever protected its previous life
        (when adopted? (mode:assign! b) (head:buffer-read-only-set! b #f))
        (head:buffer-base-set! b (buffer-text b))
        (head:buffer-stamp-set! b (file:stamp path))
        (head:buffer-stale-set! b #f)
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
    (cond
      [(and disk (not adopted?) (not modified?)
            (head:buffer-base b) (string=? disk (head:buffer-base b)))
       ;; nothing to do, and the mtime stays untouched
       (set! message "No changes to save")
       #f]
      [(and disk (not adopted?)
            (not (and (head:buffer-base b) (string=? disk (head:buffer-base b)))))
       (stale-save! b path disk write!)]
      [(and disk adopted?)
       ;; saving under a new name onto an existing file
       (let ask ()
         (let* ([k (prompt:key! (format "~a exists; overwrite? y)es or n)o"
                                        (file:base-name path))
                                "yn")]
                [n (and k (char->integer k))])
           (cond [(memv n '(121 89)) (write!)]
                 [(or (not n) (memv n '(110 78 7 27)))
                  (set! message "Save cancelled") #f]
                 [else (ask)])))]
      [else (write!)]))

  (define (merge-report! b report-lines)
    ;; The merge's paper trail: a read-only *merge-<buffer>* holding
    ;; diff's unified-diff-style rendering -- built quietly, never
    ;; displayed; the echo names it.  -> the report buffer's name.
    (let* ([name (format "*merge-~a*" (head:buffer-name b))]
           [rb (fresh-buffer name)])
      (when (pair? report-lines) (apply buffer-append! rb report-lines))
      (head:buffer-read-only-set! rb #t)
      name))

  (define (merge-from-disk! b path disk)
    ;; Replace the buffer with the three-way merge of its base, its
    ;; text, and the disk; -> the conflict count and the report
    ;; buffer's name.  The buffer adopts the disk as its new base
    ;; either way -- the external change is incorporated, so the next
    ;; save writes cleanly.  One undo entry.
    (let-values ([(merged merged-trailing conflicts report-lines)
                  (file:merge path (head:buffer-base b) (buffer-text b) disk)])
      (head:buffer-base-set! b disk)
      (head:buffer-stamp-set! b (file:stamp path))
      (record-edit! "merge from disk")
      (head:buffer-lines-set! b merged)
      (head:buffer-trailing-set! b merged-trailing)
      (changed!)
      (values conflicts (merge-report! b report-lines))))

  (define (reread-from-disk! b path disk)
    ;; Discard the buffer's copy and adopt the disk verbatim.  Rereading is a
    ;; new baseline, not an edit: it clears modification and undo state.
    (let* ([lines (file:lines disk)]
           [last (- (vector-length lines) 1)])
      (head:buffer-lines-set! b lines)
      (head:buffer-trailing-set! b (file:ends-in-newline? disk))
      (head:buffer-base-set! b disk)
      (head:buffer-stamp-set! b (file:stamp path))
      (head:buffer-stale-set! b #f)
      (head:buffer-modified-set! b #f)
      (head:buffer-history-set! b (vector '() '()))
      (head:buffer-marked-set! b #f)
      (set! merge-reports (remp (lambda (p) (eq? (car p) b)) merge-reports))
      (for-each
        (lambda (w)
          (when (eq? (head:window-buffer w) b)
            (let ([row (min (head:window-prow w) last)])
              (head:window-prow-set! w row)
              (head:window-pcol-set! w
                (min (head:window-pcol w)
                     (string-length (vector-ref lines row))))
              (head:window-top-set! w (min (head:window-top w) last)))))
        windows)
      (parameterize ([message-source 'visit-file!])
        (set-message! (format "Reread ~a" path)))
      #t))

  (define (reopen-changed-file! b path disk)
    (let ask ()
      (let* ([k (prompt:key!
                  (format "~a changed on disk: m)erge, r)eread, c)ancel"
                          (file:base-name path))
                  "mrc")]
             [n (and k (char->integer k))])
        (cond
          [(memv n '(109 77))                                 ; m
           (let-values ([(conflicts report-name)
                         (merge-from-disk! b path disk)])
             ;; The merge incorporated this disk version into the buffer's
             ;; baseline.  It remains modified only when it differs from disk.
             (head:buffer-stale-set! b #f)
             (head:buffer-modified-set! b (not (string=? (buffer-text b) disk)))
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
          [(memv n '(114 82)) (reread-from-disk! b path disk)] ; r
          [(or (not n) (memv n '(99 67 7 27)))                ; c, C-g, ESC
           (keyboard-quit!)
           #f]
          [else (ask)]))))

  ;; Merge reports awaiting resolution -- (buffer . report-name): a
  ;; conflicted merge does not announce its report buffer up front;
  ;; the save that carries the resolved text does, separately.
  (define merge-reports '())

  (define (buffer-conflict-count b)
    ;; How many merge conflict markers are left in b.
    (file:conflict-count (head:buffer-lines b)))

  (define (buffer-has-conflicts? b)
    (> (buffer-conflict-count b) 0))

  (define (stale-save! b path disk write!)
    (head:buffer-stale-set! b #t)   ; worn until a write settles it
    (let ask ()
      (let* ([k (prompt:key!
                  (format "~a changed on disk: o)verwrite, m)erge, c)ancel"
                          (file:base-name path))
                  "omc")]
             [n (and k (char->integer k))])
        (cond
          [(memv n '(111 79)) (write!)]                       ; o
          [(memv n '(109 77))                                 ; m
           (let-values ([(conflicts report-name)
                         (merge-from-disk! b path disk)])
             (if (zero? conflicts)
                 (begin
                   (write!)
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

  (define (buffer-text b)
    ;; b's text as its file would hold it
    (file:text (head:buffer-lines b) (head:buffer-trailing b)))

  (define (buffer-clean? b)
    ;; Nothing is lost by discarding b: it was never modified, it is
    ;; read-only (a view, a report), its text is identical to what is
    ;; on disk again, or it is an empty file-less buffer.
    (or (not (head:buffer-modified b))
        (head:buffer-read-only b)
        (let ([path (head:buffer-file b)])
          (if path
              (and (file-exists? path)
                   (guard (ex [else #f])
                     (string=? (buffer-text b) (file:read path))))
              (let ([v (head:buffer-lines b)])
                (and (= (vector-length v) 1)
                     (string=? (vector-ref v 0) "")))))))

  ;;; Buffer and window commands ---------------------------------------------

  (define (show-buffer! b)
    (set! buffers (cons b (remq b buffers)))   ; most recently used first
    (head:set-window-buffer! current-window b))

  ;; Read-only views of the editor's state, for M-x and modules; mutation
  ;; goes through the command API.
  (define (current-buffer) (head:window-buffer current-window))
  (define (buffer-list) (list-copy buffers))
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
  (define (point) (cons point-row point-col))
  (define (mark) (and mark-active? (cons mark-row mark-col)))
  (define (buffer-line-count b) (vector-length (head:buffer-lines b)))
  (define (buffer-line b n) (vector-ref (head:buffer-lines b) n))

  (define (call-with-buffer b thunk)
    ;; Run thunk with b temporarily the current buffer: in the window
    ;; already showing it when there is one -- point moves where the
    ;; user sees them -- else invisibly in the current window with the
    ;; usual spot saving; the MRU order is untouched either way.
    (cond
      [(eq? b (head:window-buffer current-window)) (thunk)]
      [(find (lambda (w) (eq? (head:window-buffer w) b)) windows)
       => (lambda (w)
            (let ([prev current-window])
              (dynamic-wind
                (lambda () (set! current-window w))
                thunk
                (lambda () (set! current-window prev)))))]
      [else
       (let ([old (head:window-buffer current-window)])
         (dynamic-wind
           (lambda () (head:set-window-buffer! current-window b))
           thunk
           (lambda () (head:set-window-buffer! current-window old))))]))

  (define (fresh-buffer name)
    ;; A named snapshot-style tool buffer, emptied for rebuilding. Live tools
    ;; use head:register-view! instead. An existing buffer is reused: the
    ;; windows showing it keep showing it and display-buffer! finds it
    ;; on screen -- no kill, no second window, no duplication.
    (let ([b (or (head:buffer-named name) (head:new-buffer name))])
      (head:buffer-read-only-set! b #f)
      (head:buffer-lines-set! b (vector ""))
      (head:buffer-history-set! b (vector '() '()))
      (head:buffer-modified-set! b #f)
      (for-each (lambda (w)
                  (when (eq? (head:window-buffer w) b)
                    (head:window-top-set! w 0)
                    (head:window-topseg-set! w 0)
                    (head:window-prow-set! w 0)
                    (head:window-pcol-set! w 0)))
                windows)
      b))

  ;;; Apps and views ------------------------------------------------------------

  ;; The app registry, the hook registries, the view helpers, and the
  ;; window geometry helpers live in (head); the commands over them are
  ;; here.

  (define (line-numbers!)
    (let ([b (current-buffer)])
      (head:buffer-line-numbers-setting-set! b (not (head:buffer-line-numbers b)))
      (paint:invalidate-screen-cache!)
      (set-message!
        (format "Line numbers ~a" (if (head:buffer-line-numbers b) "on" "off")))))

  ;;; The log -----------------------------------------------------------------

  ;; The editor's syslog lives in (log) -- the structured records and
  ;; the formatter registry are state, not UI.  How a logged message is
  ;; shown is the head's side, here: set-message! and the echo-area
  ;; presenter that init! installs on the log.

  (define message-source
    ;; Who a message came from, for the log's attribution: components
    ;; parameterize it around their messages.  #f makes the message a
    ;; plain indicator -- shown, never logged.
    (make-parameter 'e))

  (define message-progress
    ;; When true, a logged message supersedes its component's newest
    ;; line in the echo area -- progress redrawn in place rather than
    ;; stacked -- never a line from another component.  The log
    ;; records every step regardless.
    (make-parameter #f))

  (define (present-log-entry! e)
    ;; Present an existing record in the echo area without logging it again.
    (present-log-entries! (list e)))

  (define (present-log-entries! entries . tail)
    ;; Queue several existing records and repaint once, avoiding a full echo
    ;; geometry change and terminal redraw for every streamed line.
    (let loop ([left entries])
      (when (pair? left)
        (let* ([e (car left)]
               [text (log:format-entry e)]
               [styler (log:styler (cadr e))]
               [ghost (if (and (null? (cdr left)) (pair? tail))
                          (car tail)
                          "")])
          (paint:echo-queue! (cadr e) text styler #f ghost)
          (loop (cdr left)))))
    (when (pair? entries) (paint:present-echo!)))


  ;; A buffer's printed form is the expression that looks it up again, so
  ;; results shown in *eval* can be pasted straight into the next
  ;; expression: (buffer-line-count (buffer "e")).  The lookup is by
  ;; name at evaluation time -- a killed buffer's form reports itself.
  (define (lookup-buffer name)
    (or (head:buffer-named name) (error 'buffer "no buffer named" name)))

  (define buffer-printing
    (record-writer (record-type-descriptor head:buffer)
      (lambda (r p wr)
        (display "(buffer " p)
        (wr (head:buffer-name r) p)
        (display ")" p))))

  ;; A window's printed form is likewise the expression that finds it
  ;; again: (window 1) is the window numbered 1 at the left of its
  ;; status line.  Numbers are reused, so the form names whatever
  ;; window holds the number when it is evaluated.
  (define (lookup-window n)
    (or (head:window-numbered n) (error 'window "no window numbered" n)))

  (define window-printing
    (record-writer (record-type-descriptor head:window)
      (lambda (r p wr)
        (display "(window " p)
        (wr (head:window-index r) p)
        (display ")" p))))

  (define (complete-buffer-name s)
    (sort string<? (filter (lambda (n) (string:prefix? s n))
                           (map head:buffer-name buffers))))

  (define (switch-buffer!!)
    (let* ([current (head:window-buffer current-window)]
           [default (find (lambda (b) (not (eq? b current))) buffers)]
           [s (prompt:read! (if default
                              (format "Switch to buffer (default ~a): "
                                (head:buffer-name default))
                              "Switch to buffer: ")
                            complete-buffer-name)])
      (when s
        (cond [(string=? s "") (when default (show-buffer! default))]
              [(head:buffer-named s) => show-buffer!]
              [else (show-buffer! (head:new-buffer s))
                    (set! message (format "New buffer ~a" s))]))))

  (define (kill-buffer! b)
    (when (head:buffer-store-id b)
      (guard (ex [else (void)])
        (store:delete! head:ui-actor (head:buffer-store-id b))
        (head:buffer-store-id-set! b #f)))
    (head:forget-buffer! b)
    (parameterize ([message-source 'kill-buffer!])
      (set-message! (format "Killed ~a" (head:buffer-name b)))))

  (define (kill-buffer!!)
    (let* ([current (head:window-buffer current-window)]
           [s (prompt:read! (format "Kill buffer (default ~a): "
                              (head:buffer-name current))
                            complete-buffer-name)])
      (when s
        (let ([b (if (string=? s "") current (head:buffer-named s))])
          (cond [(not b) (set! message (format "No buffer named ~a" s))]
                [(or (buffer-clean? b)
                     (prompt:confirm? (format "Buffer ~a modified; kill anyway?"
                                        (head:buffer-name b))))
                 (kill-buffer! b)])))))

  (define (next-window w)
    (let ([tail (cdr (memq w windows))])
      (if (pair? tail) (car tail) (car windows))))

  (define (focus-window! w)
    ;; All user-visible focus changes pass here: the apps being left
    ;; and entered hear BLUR and FOCUS.
    (when (and (memq w windows) (not (eq? w current-window)))
      (head:dispatch-app-event! "BLUR")
      (set! current-window w)
      (head:dispatch-app-event! "FOCUS"))
    current-window)

  (define (other-window!)
    (focus-window! (next-window current-window)))

  (define (focus-window-direction! direction)
    (let* ([layout (paint:window-layout)]
           [cursor (paint:window-screen-position current-window
                                                 point-row point-col)]
           [cx (- (cdr cursor) 1)]
           [cy (- (car cursor) 1)])
      ;; Cast a ray from point. This matters in asymmetric trees: from a tall
      ;; right-hand window, for example, the cursor row chooses which of two
      ;; stacked windows on the left receives focus.
      (define (distance entry)
        (let* ([w (car entry)]
               [x0 (head:window-xoff w)] [x1 (+ x0 (head:window-width w) -1)]
               [y0 (cadr entry)] [y1 (+ y0 (caddr entry))])
          (case direction
            [(left) (and (< x1 cx) (<= y0 cy y1) (- cx x1))]
            [(right) (and (> x0 cx) (<= y0 cy y1) (- x0 cx))]
            [(up) (and (< y1 cy) (<= x0 cx x1) (- cy y1))]
            [(down) (and (> y0 cy) (<= x0 cx x1) (- y0 cy))])))
      (let loop ([entries layout] [best #f] [best-distance #f])
        (if (null? entries)
            (when best (focus-window! (car best)))
            (let ([d (and (not (eq? (caar entries) current-window))
                          (distance (car entries)))])
              (if (and d (or (not best-distance) (< d best-distance)))
                  (loop (cdr entries) (car entries) d)
                  (loop (cdr entries) best best-distance)))))))

  (define (focus-window-up!) (focus-window-direction! 'up))
  (define (focus-window-down!) (focus-window-direction! 'down))
  (define (focus-window-left!) (focus-window-direction! 'left))
  (define (focus-window-right!) (focus-window-direction! 'right))

  (define (selected-window)
    ;; The current window, an opaque token: hold it, compare it, give
    ;; it back to select-window!.
    current-window)

  (define (select-window! w)
    ;; Make w current when it is still on screen; -> whether it was.
    (and (memq w windows) (begin (focus-window! w) #t)))

  (define (split-current-window! orientation b)
    (let* ([vertical? (eq? orientation 'below)]
           [extent (if vertical?
                       (+ (head:window-size current-window) 1)
                       (head:window-width current-window))]
           [minimum (if vertical? (+ (head:min-window-lines) 1) 20)]
           [usable (- extent (if vertical? 0 1))])
      (and (>= usable (* 2 minimum))
           (let* ([second (quotient usable 2)]
                  [first (- usable second)]
                  [w (head:make-window b top-row (head:window-topseg current-window)
                                       left-col point-row point-col
                                       (max 1 (- second 1))
                                       (max 1 (- second 1)) 0 0 second
                                       (head:window-wrap current-window))]
                  [node (head:make-layout-split orientation current-window w
                                                first second)])
             (head:replace-layout-window! current-window node)
             w))))

  (define (split-window!)
    ;; Split only the selected leaf, as in Emacs.
    (unless (split-current-window! 'below (head:window-buffer current-window))
      (set! message "Not enough room to split")))

  (define (split-window-right!)
    (unless (split-current-window! 'right (head:window-buffer current-window))
      (set! message "Not enough room to split"))
    (void))

  (define (wrap! . on)
    ;; Toggle (or set) soft-wrapping of long lines in the current window.
    (head:window-wrap-set! current-window
                           (if (pair? on) (car on)
                             (not (paint:window-wrapped? current-window))))
    (head:window-left-set! current-window 0)
    (set! goal-pos #f)              ; the goal column changes meaning
    (set! message (format "Wrap ~a"
                          (if (paint:window-wrapped? current-window) "on" "off")))
    (void))

  (define (resize-window! delta)
    ;; Resize at the nearest enclosing stacked split.
    (let loop ([child current-window])
      (let ([parent (head:layout-parent layout-root child)])
        (cond
          [(not parent) (set! message "No vertical split")]
          [(eq? (head:layout-split-orientation parent) 'below)
           (let ([signed (if (eq? child (head:layout-split-first parent))
                             delta (- delta))])
             (layout-split-first-weight-set!
               parent (max 1 (+ (head:layout-split-first-weight parent) signed)))
             (layout-split-second-weight-set!
               parent (max 1 (- (head:layout-split-second-weight parent) signed))))]
          [else (loop parent)]))))

  (define (delete-window!)
    (if (null? (cdr (head:layout-leaves layout-root)))
        (set! message "Only one window")
        (let* ([next (next-window current-window)]
               [parent (head:layout-parent layout-root current-window)]
               [sibling (if (eq? current-window (head:layout-split-first parent))
                            (head:layout-split-second parent)
                            (head:layout-split-first parent))])
          (head:replace-layout-window! parent sibling)
          (focus-window! next))))

  (define (delete-other-windows!)
    (head:set-layout-root! current-window))

  (define (display-buffer! b)
    ;; Show b without leaving the current window: in the window already
    ;; showing it, else the next window, else a fresh split.  The window,
    ;; or #f when the screen has no room for one.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (cond
      [(find (lambda (w) (eq? (head:window-buffer w) b)) windows)]
      [(pair? (cdr (head:layout-leaves layout-root)))
       (let ([w (next-window current-window)])
         (head:set-window-buffer! w b)
         w)]
      [(split-current-window! 'below b)]
      [else #f]))

  (define (pop-up-or-reuse! b)
    ;; Help-like buffers never appropriate another leaf: reuse an existing
    ;; window displaying b, otherwise create a new tile below the current one.
    ;; Focus stays where it was so the buffer remains a reference alongside
    ;; the command that requested it.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (or (find (lambda (w) (eq? (head:window-buffer w) b)) windows)
        (split-current-window! 'below b)))

  (define (buffer-append! b . new-lines)
    ;; Append lines to b, transcript style: a fresh buffer's single empty
    ;; line is replaced, and the display follows -- point moves to the
    ;; last line in every window showing b, and in ones that show it later.
    ;; A transcript belongs in the buffer list even before it is shown.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (let ([v (head:buffer-lines b)]
          [add (list->vector new-lines)])
      (head:buffer-lines-set! b
        (if (and (= (vector-length v) 1) (string=? (vector-ref v 0) ""))
            add
            (vector-append v add))))
    (let ([last (- (vector-length (head:buffer-lines b)) 1)])
      (head:buffer-spot-row-set! b last)
      (head:buffer-spot-col-set! b 0)
      (for-each (lambda (w)
                  (when (eq? (head:window-buffer w) b)
                    (head:window-prow-set! w last)
                    (head:window-pcol-set! w 0)))
                windows)))

  ;;; Buffer settings ---------------------------------------------------------

  ;; The mode registry -- records, detection, the memoized stylers --
  ;; lives in (mode); these two settings are commands' business.

  (define (set-buffer-read-only! b flag)
    (head:buffer-read-only-set! b flag))

  (define (set-buffer-wrap! b setting)
    ;; clean wraps like #t but draws no continuation marks and lets the
    ;; text use the full width -- for formatted read-only presentations;
    ;; (clean . n) additionally caps the wrapping width at n columns.
    (unless (or (memq setting '(default #t #f clean))
                (and (pair? setting) (eq? (car setting) 'clean)
                     (fixnum? (cdr setting)) (>= (cdr setting) 20)))
      (error 'set-buffer-wrap!
             "expected default, #t, #f, clean, or (clean . columns)"
             setting))
    (head:buffer-fact-set! b 'wrap setting)
    b)

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

  (define (register-indenter! name proc . tab)
    (kernel:registry-add! indenters (list name proc (or (null? tab) (car tab)))))

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
    (define v (head:buffer-lines b))
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
        (record-edit! "indent")
        (let ([nv (let ([o (make-vector n)])
                    (do ([i 0 (+ i 1)]) ((= i n) o)
                      (vector-set! o i (vector-ref v i))))])
          (for-each
            (lambda (change)
              (let* ([row (car change)] [col (cdr change)]
                     [old (vector-ref v row)]
                     [lead (leading-blanks old)]
                     [follow (lambda (c)
                               (if (<= c lead) col (+ c (- col lead))))])
                (vector-set! nv row (retabbed old col))
                (when (= row point-row)
                  (set! point-col (follow point-col)))
                (when (and mark-active? (= row mark-row))
                  (set! mark-col (follow mark-col)))))
            changes)
          (head:buffer-lines-set! b nv))
        (changed!))
      (pair? changes)))

  (define (indent-rows! from to)
    ;; Indent rows [from, to] by the mode's indenter, each settling on
    ;; the stop nearest its current indentation; -> #f without one.
    (let ([entry (mode-entry indenters)])
      (if (not entry)
          (begin (set! message "No indenter for this mode") #f)
          (let* ([b (head:window-buffer current-window)]
                 [v (head:buffer-lines b)]
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
            (apply-indent! from cols #f)
            #t))))

  (define (indent-line!)
    ;; TAB's work: indent the current line, cycling through its stops
    ;; -- the nearest stop right of the current indentation, wrapping
    ;; -- and land on the indentation (a blank line pads out to it);
    ;; point already past it stays with its text.
    (let ([entry (mode-entry indenters)])
      (if (not entry)
          (set! message "No indenter for this mode")
          (let* ([b (head:window-buffer current-window)]
                 [cols ((cadr entry) b point-row point-row)]
                 [col (and (pair? cols)
                           (cycle-stops (car cols)
                                        (leading-blanks
                                          (line-at point-row))))])
            (when col
              (apply-indent! point-row (list col) #t)
              (when (< point-col col) (set! point-col col))))))
    (void))

  (define (indent-tab!)
    ;; TAB: the mode indents when it asked to; otherwise nothing.
    (let ([entry (mode-entry indenters)])
      (when (and entry (caddr entry))
        (indent-line!))))

  (define (indent-on-tab! name flag)
    ;; Configuration: whether TAB auto-indents in the named mode,
    ;; overriding the flag its indenter registered with.
    (let ([entry (kernel:registry-find indenters
                                       (lambda (x) (string=? (car x) name)))])
      (unless entry (error 'indent-on-tab! "no indenter for mode" name))
      (kernel:registry-add! indenters (list name (cadr entry) flag))))

  (define (indent-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (indent-rows! from to)
            (set! message (format "Indented ~a line~a" (+ (- to from) 1)
                                  (if (= from to) "" "s"))))))
    (void))

  (define (indent-buffer!)
    (let ([n (vector-length (head:buffer-lines (head:window-buffer current-window)))])
      (when (indent-rows! 0 (- n 1))
        (set! message (format "Indented ~a lines" n))))
    (void))

  (define (replace-rows! from to lines)
    ;; Replace rows [from, to] of the current buffer with lines (a
    ;; list), one undo entry; point keeps its row when it can.
    (define b (head:window-buffer current-window))
    (define v (head:buffer-lines b))
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
      (record-edit! "format")
      (head:buffer-lines-set! b (if (zero? (vector-length nv)) (vector "") nv))
      (set! point-row (max 0 (min point-row
                                  (- (vector-length (head:buffer-lines b)) 1))))
      (changed!)))

  (define (format-rows! from to)
    ;; Format rows [from, to] by the mode's formatter; -> whether the
    ;; buffer changed.
    (let ([entry (mode-entry formatters)])
      (cond
        [(not entry) (set! message "No formatter for this mode") #f]
        [else
         (let* ([b (head:window-buffer current-window)]
                [v (head:buffer-lines b)]
                [last (min to (- (vector-length v) 1))]
                [lines ((cadr entry) b from last)])
           (cond
             [(not lines) (set! message "Cannot format these lines") #f]
             [(let same ([r from] [ls lines])
                (if (null? ls)
                    (> r last)
                    (and (<= r last)
                         (string=? (car ls) (vector-ref v r))
                         (same (+ r 1) (cdr ls)))))
              (set! message "Already formatted") #f]
             [else
              (replace-rows! from last lines)
              ;; formatted through the end: the file ends with exactly
              ;; one newline
              (when (= last (- (vector-length v) 1))
                (set! trailing-newline? #t))
              #t]))])))

  (define (format-region!)
    (if (not mark-active?)
        (set! message "The mark is not set now")
        (let ([from (min mark-row point-row)]
              [to (max mark-row point-row)])
          (when (format-rows! from to)
            (set! message "Formatted region"))))
    (void))

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
           [v (head:buffer-lines (current-buffer))]
           [n (vector-length v)]
           [sticky (min (head:buffer-sticky-lines (current-buffer)) (- n 1))]
           [height (paint:page-size)]
           [wrapped? (paint:window-wrapped? w)]
           [visual-col (if wrapped?
                           (let* ([line (vector-ref v point-row)]
                                  [breaks (paint:line-breaks w line)])
                             (- point-col
                                (paint:segment-start breaks
                                  (paint:segment-of breaks point-col))))
                           point-col)])
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
          (if wrapped?
              (let ([breaks (paint:line-breaks w line)] [seg (cdr position)])
                (min (+ (paint:segment-start breaks seg) visual-col)
                     (paint:segment-close breaks seg (string-length line))))
              (min visual-col (string-length line)))))
      (define (land! top-offset point-offset)
        (let ([top (position-at top-offset)]
              [point (position-at point-offset)])
          (goto-point! (cons (car point) (column-at point)))
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

  (define (page-window-fraction! direction fraction)
    (page-window! direction fraction))

  (define (set-point-without-scroll! position)
    (let* ([v (head:buffer-lines (current-buffer))]
           [row (max 0 (min (car position) (- (vector-length v) 1)))])
      (head:window-prow-set! current-window row)
      (head:window-pcol-set! current-window
                             (max 0 (min (cdr position)
                                      (string-length (vector-ref v row)))))))


  ;; The head's side of the interaction protocol: another actor's
  ;; question waits in the echo area as an unlogged indicator until
  ;; C-c a answers it -- nobody's keyboard is stolen mid-thought.
  (define (answer!!)
    ;; Answer the oldest question another actor posed (the interaction
    ;; protocol: actor.e).
    (let ([asks (actor:pending head:ui-actor)])
      (if (null? asks)
          (set! message "Nothing to answer")
          (let* ([ask (car asks)]
                 [choices (cadddr ask)]
                 [reply
                  (prompt:read! (format "~a [~a] "
                                  (caddr ask)
                                  (if (null? choices)
                                      "..."
                                      (string:join choices "/")))
                                (and (pair? choices)
                                  (lambda (s)
                                    (filter
                                      (lambda (choice)
                                        (string:prefix? s choice))
                                      choices))))])
            (when (and reply (> (string-length reply) 0))
              (if (actor:answer! (car ask) reply)
                  (set! message "Answered")
                  (set! message "That question was withdrawn")))))))

  ;;; File commands -----------------------------------------------------------

  ;; The prompt -- the modal loop, completions, single-key questions --
  ;; lives in (prompt); the commands that ask are here.

  (define (prompt-kill-buffer!)
    ;; kill-buffer!!'s prompt-safe stand-in: no nested prompt, and a
    ;; file-backed buffer with unsaved changes is refused with a note.
    (let ([b (current-buffer)])
      (if (and (head:buffer-file b) (not (buffer-clean? b)))
          (format "  ~a has unsaved changes" (head:buffer-name b))
          (guard (ex [else (string-append "  " (kernel:condition-text ex))])
            (kill-buffer! b)
            ""))))

  (define (file-prompt-styler label)
    ;; Existence shown in the face, component-wise: the typed path's
    ;; longest leading run of components that exists on disk stays
    ;; upright, the rest leans italic -- so a TAB that landed on a
    ;; mere common prefix (no such file yet) is telling at a glance,
    ;; without another TAB to ask.
    (define (exists? p)
      (guard (ex [else #f]) (file-exists? (file:expand p))))
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

  (define (save!!)
    (if file-name
        (save-file! file-name)
        (let ([s (parameterize ([paint:echo-highlight
                                 (file-prompt-styler "Write file: ")])
                   (prompt:read! "Write file: " file:complete
                                 (default-directory)))])
          (when (and s (> (string-length s) 0)) (save-file! s))))
    (void))

  (define (save-as!!)
    ;; Prompt for a path -- prefilled with the current file, ready to
    ;; edit -- and save the buffer there: the buffer visits the new
    ;; file from then on, its name and mode following.
    (let ([s (parameterize ([paint:echo-highlight (file-prompt-styler "Save as: ")])
               (prompt:read! "Save as: " file:complete
                             (if file-name
                               (file:abbreviate (file:absolute file-name))
                               (default-directory))
                             (box (log:history 'save-file! cdr))))])
      (when (and s (> (string-length s) 0)) (save-file! s)))
    (void))

  (define (find-file!!)
    ;; Visiting a file never loses the old buffer, so no confirmation
    ;; needed.  Up and down browse the paths visited before, off the
    ;; log.
    (let ([s (parameterize ([paint:echo-highlight
                             (file-prompt-styler "Find file: ")])
               (prompt:read! "Find file: " file:complete (default-directory)
                             (box (log:history 'visit-file! cdr))))])
      (when (and s (> (string-length s) 0)) (visit-file! s))))

  (define (quit!!)
    (if (for-all buffer-clean? buffers)
        (head:quit!)
        (let ([answer (prompt:key!
                        "Modified buffers exist; quit anyway? y)es, n)o, v)iew"
                        "ynv")])
          (case (and answer (char-downcase answer))
            [(#\y) (head:quit!)]
            [(#\v)
             (let ([b (head:buffer-named "*buffers*")])
               (if b
                   (let ([w (display-buffer! b)])
                     (when w
                       (select-window! w)
                       ;; A direct app entry still receives the same
                       ;; initialization opportunity as its ordinary command.
                       (head:dispatch-app-event! "FOCUS")
                       (set! message "")))
                   (set-message! "The *buffers* app is not available")))]
            [else (void)]))))

  ;;; Pasting and typed runs --------------------------------------------------

  (define (paste-into-buffer!)
    ;; A bracketed paste: the whole text becomes one labeled edit, its
    ;; newlines becoming real line breaks.
    (let ([text (head:read-paste)])
      (unless (string=? text "")
        (call-as-one-edit! (format "insert ~s" text)
          (lambda ()
            (let ([parts (tty:paste-lines text)])
              (insert-text! (car parts))
              (for-each (lambda (part) (newline!) (insert-text! part))
                        (cdr parts))))))))

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
            (parameterize ([suppress-history #t]) (insert-text! s))
            (relabel-last-edit! (format "insert ~s" text))
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
  (define (mouse! on)
    ;; Turn mouse tracking on or off (off restores native selection).
    (tty:mouse-reporting! on)
    (set! message (format "Mouse ~a" (if on "on" "off")))
    (void))

  ;; Hit-testing over the remembered tiling, and the gesture state,
  ;; live in (head); the actions they trigger stay here.
  (define-syntax drag-divider
    (identifier-syntax [id (head:drag)] [(set! id v) (head:set-drag! v)]))

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

  ;; 1-based cell coordinates within the app's text viewport while a mouse
  ;; event is dispatched, or #f for keyboard events.
  (define app-event-position (make-parameter #f))
  ;; Raw xterm button code (including modifier/motion bits) for an app mouse
  ;; event, or #f for keyboard events.
  (define app-event-button (make-parameter #f))
  ;; The unclamped (row . column) addressed by an app content click. This can
  ;; lie beyond the buffer and lets apps distinguish empty viewport space from
  ;; their last rendered line.
  (define app-event-buffer-position (make-parameter #f))
  (define (window-position w start height x y)
    ;; The buffer (row . col) at 1-based screen (x, y) inside w's text
    ;; band, wrap-aware: wrapped lines occupy successive screen rows,
    ;; so the band row is walked through the segment counts.
    (let* ([v (head:buffer-lines (head:window-buffer w))]
           [sticky (head:buffer-sticky-lines (head:window-buffer w))]
           [k (max 0 (- y 1 start))]
           [col (max 0 (- x 1 (head:window-xoff w)
                          (if (eq? (head:window-scrollbar? w) 'left) 1 0)
                          (head:window-line-number-width w)))])
      (cond
        [(< k sticky)
         (cons (min k (- (vector-length v) 1)) col)]
        [(paint:window-wrapped? w)
         (let loop ([i (max sticky (head:window-top w))]
                    [k (+ (- k sticky) (head:window-topseg w))])
           (if (>= i (vector-length v))
               ;; Preserve the addressed row outside the buffer. The point
               ;; setter clamps ordinary clicks; app handlers also receive
               ;; this raw position so blank viewport space stays distinct
               ;; from the final rendered line.
               (cons i col)
               (let* ([line (vector-ref v i)]
                      [breaks (paint:line-breaks w line)]
                      [segs (vector-length breaks)])
                 (if (< k segs)
                     (cons i (min (+ (paint:segment-start breaks k) col)
                                  (paint:segment-close breaks k
                                                       (string-length line))))
                     (loop (+ i 1) (- k segs))))))]
        [else
         (cons (+ (max sticky (head:window-top w)) (- k sticky))
               (+ (head:window-left w) col))])))

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
    (set! drag-divider #f)
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
             (cond
               [else
                (focus-window! w)
                (case action
                  [(below) (split-window!)]
                  [(right) (split-window-right!)]
                  [(close) (delete-window!)])]))
           "MOUSE-HANDLED")]
        [(head:divider-at (- x 1) (- y 1)) =>
         (lambda (divider)
           ;; a below divider doubles as the upper window's status bar:
           ;; pressing it focuses that window, as any status bar does,
           ;; and still arms the drag
           (when (eq? (car divider) 'below)
             (head:window-at (- x 1) (- y 1)
               (lambda (entry) (focus-window! (car entry)))))
           (set! drag-divider divider)
           "MOUSE-HANDLED")]
        [else
         (head:window-at (- x 1) (- y 1)
           (lambda (entry)
             (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
               (cond
                 [(= (- y 1) (+ start height))        ; the status bar
                  (focus-window! w)
                  "MOUSE-HANDLED"]
                 [(and (head:window-scrollbar-column w)
                       (= (- x 1) (head:window-scrollbar-column w)))
                  ;; App bars navigate like their wheel controls: they do not
                  ;; take focus and do not invoke the row's click action.
                  (let ([old current-window])
                    (unless (head:app-buffer? (head:window-buffer w))
                      (focus-window! w))
                    (set! current-window w)
                    (when (and (head:app-buffer? (head:window-buffer w))
                               (memq old windows))
                      (set! current-window old)))
                  "MOUSE-HANDLED"]
                 [(head:app-buffer? (head:window-buffer w))
                  (let ([old current-window])
                    (set! current-window w)
                    (let ([old-point (point)]
                          [clicked (window-position w start height x y)])
                      (goto-point! clicked)
                      (set! mark-active? #f)
                      ;; Focusing the clicked window is the default. An app may
                      ;; act on the click and explicitly preserve the old
                      ;; focus by returning keep-focus for MOUSE-CLICK.
                      (let ([result
                             (parameterize
                               ([app-event-buffer-position clicked]
                                [app-event-button button])
                               (head:dispatch-app-event! "MOUSE-CLICK"))])
                        (cond [(eq? result 'ignore-click)
                               (goto-point! old-point)
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
                  (focus-window! w)
                  (goto-point! (window-position w start height x y))
                  (arm-text-selection!)
                  ;; A mode may act on the click -- following a link,
                  ;; say -- through a MOUSE-CLICK binding in its keymap.
                  (let ([context (mode:key-context (current-buffer))])
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
      [drag-divider
       (let* ([orientation (car drag-divider)]
              [split (cadr drag-divider)]
              [old (if (eq? orientation 'right)
                       (caddr drag-divider)
                       (cadddr drag-divider))]
              [now (if (eq? orientation 'right) (- x 1) (- y 1))]
              [delta (- now old)])
         (unless (= delta 0)
           (head:transfer-split! split delta)
           (if (eq? orientation 'right)
               (set-car! (cddr drag-divider) now)
               (set-car! (cdddr drag-divider) now))))]
      [else
       (head:window-at (- x 1) (- y 1)
         (lambda (entry)
           (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
             (when (and (eq? w current-window)
                        (< (- y 1) (+ start height)))
               (goto-point! (window-position w start height x y))
               (if (head:app-buffer? (head:window-buffer w))
                   (unless (parameterize ([app-event-button button])
                             (head:dispatch-app-event! "MOUSE-DRAG"))
                     (set! mark-active? #t))
                   (set! mark-active? #t))))))]))

  (define (mouse-release! x y button)
    (head:window-at (- x 1) (- y 1)
      (lambda (entry)
        (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
          (when (and (eq? w current-window)
                     (< (- y 1) (+ start height))
                     (head:app-buffer? (head:window-buffer w)))
            (goto-point! (window-position w start height x y))
            (parameterize ([app-event-button button])
              (head:dispatch-app-event! "MOUSE-RELEASE")))))))

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
          (if (and meta? (memv dir '(0 1)))
              (run-global-key! (if (= dir 0) "M-S-UP" "M-S-DOWN"))
              (begin
                (unless (parameterize
                          ([app-event-position
                            (cons (max 1 (- x (head:window-xoff w)))
                                  (max 1 (- y (cadr entry))))]
                           [app-event-button button])
                          (head:dispatch-app-event!
                            (string-append
                              (if shift? "S-" "")
                              (case dir
                                [(0) "WHEEL-UP"]
                                [(1) "WHEEL-DOWN"]
                                [(2) "WHEEL-LEFT"]
                                [(3) "WHEEL-RIGHT"]
                                [else "WHEEL"]))))
                  ((wheel-mover dir)))))
          (when (memq old windows) (set! current-window old))
          "MOUSE-HANDLED"))))

  (define (run-global-key! key)
    ;; the global map's command for one key, run as a command
    (let ([hit (keymap:resolved-binding 'global (list key))])
      (when hit
        (let ([action (keymap:binding-action (cdr hit))])
          (when (procedure? action) (action))))))

  (define (wheel-mover dir)
    ;; Wheel direction (the low bits of a 64-flagged button): up, down,
    ;; left, right. Vertical ticks move the hovered viewport by one eighth
    ;; of its height; horizontal ones move point sideways within its line.
    (case dir
      [(0) (lambda () (page-window! -1 8))]
      [(1) (lambda () (page-window! 1 8))]
      [(2) (lambda () (goto-point! (cons point-row (- point-col 3))))]
      [(3) (lambda () (goto-point! (cons point-row (+ point-col 3))))]
      [else (lambda () (void))]))

  ;; Input decoding lives in (tty): the head's reader thread calls
  ;; (tty:read-event stdin); the main thread applies the parsed mouse
  ;; data below, through the handler init! installs on the pump.

  (define (apply-mouse-event! handle? c b x y)
    ;; Wheel is button 64/65; releases are ignored.  A context that
    ;; must not change editor focus passes handle? #f: the report is
    ;; consumed without being applied.
    (and handle?
         (cond [(char=? c #\m)                         ; release
                (mouse-release! x y b)
                (set! drag-divider #f)
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
               [else "MOUSE-HANDLED"])))

  ;;; Small commands and key description -------------------------------------

  (define (set-mark-command!)
    (set! mark-row point-row) (set! mark-col point-col)
    (set! mark-active? #t) (set! message "Mark set"))
  (define (beginning-of-line!) (set! point-col 0))
  (define (end-of-line!) (set! point-col (string-length (current-line))))
  (define (keyboard-quit!) (set! mark-active? #f) (set! message "Quit"))
  (define (redraw-command!)
    (paint:mark-size-dirty!) (paint:erase-screen!) (set! message "Screen redrawn"))
  (define (open-line!)
    (let ([row point-row] [col point-col])
      (newline!) (set! point-row row) (set! point-col col)))
  (define (page-up!) (page-window! -1 1))
  (define (page-down!) (page-window! 1 1))
  (define (previous-line!) (move-vertical! -1))
  (define (next-line!) (move-vertical! 1))
  (define (beginning-of-buffer!) (set! point-row 0) (set! point-col 0))
  (define (end-of-buffer!)
    (set! point-row (- (vlen) 1))
    (set! point-col (string-length (current-line))))

  (define (action-name action)
    (cond
      [(not action) "unbound"]
      [(symbol? action) (symbol->string action)]
      [else
       (let ([sym
              (find
                (lambda (s)
                  (and (top-level-bound? s)
                       (guard (ex [else #f])
                         (eq? (top-level-value s) action))))
                (environment-symbols (interaction-environment)))])
         (if sym (symbol->string sym) "anonymous command"))]))

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

  (define (describe-key!!)
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
           [b (fresh-buffer "*help*")])
      (buffer-append! b
        (keymap:sequence-text sequence)
        ""
        (if resolved
            (format "Resolved to: ~a" (action-name (keymap:binding-action (cdr resolved))))
            "Resolved to: self-insert or undefined")
        "Keymap: global"
        (if resolved
            (format "Defined by: ~a" (binding-origin resolved))
            "Defined by: fallback"))
      (when (> (length entries) 1)
        (buffer-append! b "" "Shadowed bindings:")
        (for-each
          (lambda (owned)
            (unless (eq? owned resolved)
              (buffer-append! b
                (format "  ~a — ~a"
                        (action-name (keymap:binding-action (cdr owned)))
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
          (buffer-append! b "" "Contextual bindings:")
          (for-each
            (lambda (context)
              (let ([hit (keymap:resolved-binding context sequence)])
                (when hit
                  (buffer-append! b
                    (format "  ~a: ~a — ~a"
                            context
                            (action-name (keymap:binding-action (cdr hit)))
                            (binding-origin hit))))))
            contexts)))
      (head:buffer-read-only-set! b #t)
      (set! message "")
      (unless (pop-up-or-reuse! b)
        (set-message! "The *help* buffer could not be displayed"))))

  ;;; Regions and the generic helpers ------------------------------------------

  (define-record-type (region-record make-region region?)
    (fields (immutable buffer region-buffer)
            (immutable start region-start)
            (immutable end region-end)))

  (define (point<? a b)
    (or (< (car a) (car b))
        (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (define (region b start end)
    ;; The slice of buffer b between two (row . col) points, either order.
    (if (point<? end start)
        (make-region b end start)
        (make-region b start end)))

  (define region-printing
    (record-writer (record-type-descriptor region-record)
      (lambda (r p wr)
        (display "(region " p)
        (wr (region-buffer r) p)
        (display " '" p) (wr (region-start r) p)
        (display " '" p) (wr (region-end r) p)
        (display ")" p))))

  (define (whole-buffer b)
    (let ([last (- (buffer-line-count b) 1)])
      (make-region b '(0 . 0)
                   (cons last (string-length (buffer-line b last))))))

  (define (regions-of where)
    ;; The regions a `where` argument denotes (see the header).
    (cond [(not where)
           (list (let ([m (mark)])
                   (if m
                       (region (current-buffer) m (point))
                       (whole-buffer (current-buffer)))))]
          [(region? where) (list where)]
          [(head:buffer? where) (list (whole-buffer where))]
          [(string? where) (list (whole-buffer (lookup-buffer where)))]
          [(procedure? where) (regions-of (filter where (buffer-list)))]
          [(list? where) (apply append (map regions-of where))]
          [else (error 'regions-of
                       "not a buffer, name, region, predicate, or list"
                       where)]))

  ;;; Matching ----------------------------------------------------------------

  (define (for-matches! r needle handle!)
    ;; Walk the matches of needle inside r in order, calling
    ;; (handle! row col) on each; it returns the width the match occupies
    ;; afterwards (an edit may have changed it).  The match count.
    ;; Needles are single-line: lines are searched one at a time.
    (when (= (string-length needle) 0)
      (error 'edit "empty search string"))
    (let* ([b (region-buffer r)]
           [m (string-length needle)]
           [start (region-start r)]
           [end (region-end r)]
           [count 0])
      (let row-loop ([row (max 0 (car start))])
        (when (<= row (min (car end) (- (buffer-line-count b) 1)))
          (let col-loop ([at (if (= row (car start)) (cdr start) 0)]
                         [shift 0])
            (let* ([s (buffer-line b row)]
                   [limit (if (= row (car end))
                              (min (+ (cdr end) shift) (string-length s))
                              (string-length s))]
                   [hit (string:search s needle at limit)])
              (if hit
                  (let ([w (handle! row hit)])
                    (set! count (+ count 1))
                    (col-loop (+ hit w) (+ shift (- w m))))
                  (row-loop (+ row 1)))))))
      count))

  (define (where-of rest)
    (and (pair? rest) (car rest)))

  ;;; Commands ----------------------------------------------------------------

  (define (count-matches needle . rest)
    ;; How many times needle occurs inside `where`.
    (fold-left (lambda (n r)
                 (+ n (for-matches! r needle
                        (lambda (row col) (string-length needle)))))
               0 (regions-of (where-of rest))))

  (define (replace-all! from to . rest)
    ;; Replace every occurrence of from with to inside `where`: one undo
    ;; step per buffer, point left where it was.  The replacement count.
    (define m (string-length from))
    (define (replace-line s)
      ;; Accumulate pieces and join once instead of copying the growing line
      ;; for every non-overlapping match.
      (let loop ([at 0] [pieces '()] [count 0])
        (let ([hit (string:search s from at (string-length s))])
          (if hit
              (loop (+ hit m)
                    (cons to (cons (substring s at hit) pieces))
                    (+ count 1))
              (values (apply string-append
                             (reverse (cons (string:tail s at) pieces)))
                      count)))))
    (define (rewritten-region r)
      ;; Preserve the single-line-needle contract by rewriting each selected
      ;; row independently, including only the selected edge fragments.
      (let* ([b (region-buffer r)]
             [start (region-start r)]
             [end (region-end r)]
             [last (min (car end) (- (buffer-line-count b) 1))])
        (let loop ([row (max 0 (car start))] [lines '()] [count 0])
          (if (> row last)
              (values (string:join (reverse lines) "\n") count)
              (let* ([s (buffer-line b row)]
                     [n (string-length s)]
                     [from-col (if (= row (car start)) (min (cdr start) n) 0)]
                     [to-col (if (= row (car end)) (min (cdr end) n) n)])
                (let-values ([(line found)
                              (replace-line
                                (substring s from-col (max from-col to-col)))])
                  (loop (+ row 1) (cons line lines) (+ count found))))))))
    (when (= m 0) (error 'edit "empty search string"))
    (fold-left
      (lambda (n r)
        (+ n (call-with-buffer (region-buffer r)
               (lambda ()
                 (let ([saved (point)])
                   (call-as-one-edit!
                     (format "(replace-all! ~s ~s)" from to)
                     (lambda ()
                       (let-values ([(text count) (rewritten-region r)])
                         (when (> count 0)
                           (replace-region-text! (region-start r)
                                                 (region-end r) text))
                         (goto-point! saved)
                         count))))))))
      0 (regions-of (where-of rest))))

  (define (region-text r)
    ;; The text inside r, rows joined with newlines.
    (let* ([b (region-buffer r)]
           [start (region-start r)]
           [end (region-end r)]
           [last (min (car end) (- (buffer-line-count b) 1))])
      (string:join
        (let loop ([row (max 0 (car start))] [acc '()])
          (if (> row last)
              (reverse acc)
              (let* ([s (buffer-line b row)]
                     [n (string-length s)]
                     [from (if (= row (car start)) (min (cdr start) n) 0)]
                     [to (if (= row (car end)) (min (cdr end) n) n)])
                (loop (+ row 1) (cons (substring s from (max from to)) acc)))))
        "\n")))

  ;;; Query replace -------------------------------------------------------------

  ;; The candidate being offered, drawn highlighted by the highlighter
  ;; init! registers; #f outside replace!!.
  (define query-match #f)

  (define (find-from b needle row col)
    ;; The first match of needle at or after (row . col): (row . start),
    ;; or #f.  Needles are single-line.
    (let loop ([row row] [col col])
      (and (< row (buffer-line-count b))
           (let* ([s (buffer-line b row)]
                  [hit (string:search s needle col (string-length s))])
             (if hit
                 (cons row hit)
                 (loop (+ row 1) 0))))))

  (define (replace!! . args)
    ;; Query-replace in the current buffer, from point to the end: each
    ;; occurrence of from is highlighted and offered -- y (or SPC)
    ;; replaces, n (or DEL) skips, q / RET / C-g / ESC stops.  Prompts
    ;; for whichever of from and to are not supplied.  The whole run is
    ;; one undo step; point follows, ending after the last replacement
    ;; (or at the start of the last skipped or stopped-at match).  The
    ;; report -- how many replaced and skipped -- is echoed.
    (let* ([from (if (pair? args) (car args) (prompt:read! "Replace: "))]
           [to (and from
                    (if (and (pair? args) (pair? (cdr args)))
                        (cadr args)
                        (prompt:read! (format "Replace ~s with: " from))))])
      (if (or (not from) (not to) (string=? from ""))
          (void)                      ; cancelled at a prompt
          (let ([b (current-buffer)]
                [m (string-length from)]
                [question (format "Replace ~s with ~s? (y, n, q)" from to)]
                [replaced 0]
                [skipped 0])
            (dynamic-wind
              void
              (lambda ()
                (call-as-one-edit! (format "(replace!! ~s ~s)" from to)
                  (lambda ()
                    (let loop ([row (car (point))] [col (cdr (point))])
                      (let ([hit (find-from b from row col)])
                        (when hit
                          (set! query-match
                            (list (car hit) (cdr hit) (+ (cdr hit) m)))
                          (goto-point! (cons (car hit) (+ (cdr hit) m)))
                          (parameterize ([message-source #f]) ; an indicator
                            (set-message! question))
                          (paint:redraw!)     ; the match highlight, not the message
                          (let* ([event (head:read-key-event #f)]
                                 [action (and (not (eof-object? event))
                                              (keymap:event-binding
                                                'query-replace event))])
                            (case action
                              [(replace)
                               (goto-point! hit)
                               (do ([i 0 (+ i 1)]) ((= i m)) (delete-forward!))
                               (insert-text! to)
                               (set! replaced (+ replaced 1))
                               (loop (car (point)) (cdr (point)))]
                              [(skip)
                               (set! skipped (+ skipped 1))
                               (goto-point! hit)
                               (loop (car hit) (+ (cdr hit) m))]
                              [(stop) (goto-point! hit)]
                              [(quit-prefix)
                               (let ([next (head:read-key-event #f)])
                                 (when (and (not (eof-object? next))
                                            (eq? (keymap:event-binding
                                                   'query-replace "C-x" next)
                                              'quit-editor))
                                   (quit!!))
                                 (goto-point! hit))]
                              [else
                               (if (eof-object? event)
                                   (goto-point! hit)
                                   (loop (car hit) (cdr hit)))]))))))))
              (lambda () (set! query-match #f)))
            (set-message! (format "Replaced ~a, skipped ~a" replaced skipped))
            (void)))))

  ;;; Conflict resolution ---------------------------------------------------------

  ;; A merge left <<<<<<< buffer / ======= / >>>>>>> disk markers:
  ;; next-conflict! hops to one, keep-mine! and keep-disk! resolve the
  ;; conflict at point, each as one undo step.

  (define (conflict-marker? b row prefix)
    (and (>= row 0) (< row (buffer-line-count b))
         (string:prefix? prefix (buffer-line b row))))

  (define (conflict-at row)
    ;; The (start mid end) marker rows of the conflict containing row,
    ;; or #f.
    (let ([b (current-buffer)])
      (let up ([r row])
        (cond
          [(< r 0) #f]
          [(and (< r row) (conflict-marker? b r ">>>>>>>")) #f]
          [(conflict-marker? b r "<<<<<<<")
           (let mid ([m (+ r 1)])
             (cond
               [(>= m (buffer-line-count b)) #f]
               [(conflict-marker? b m "=======")
                (let end ([e (+ m 1)])
                  (cond
                    [(>= e (buffer-line-count b)) #f]
                    [(conflict-marker? b e ">>>>>>>")
                     (and (>= e row) (list r m e))]
                    [else (end (+ e 1))]))]
               [else (mid (+ m 1))]))]
          [else (up (- r 1))]))))

  (define (delete-rows! r1 r2)
    ;; Remove rows r1..r2 inclusive, joining across their newlines.
    (goto-point! (cons r1 0))
    (let ([n (let loop ([r r1] [n 0])
               (if (> r r2)
                   n
                   (loop (+ r 1)
                         (+ n 1 (string-length
                                  (buffer-line (current-buffer) r))))))])
      (do ([i 0 (+ i 1)]) ((= i n)) (delete-forward!))))

  (define (next-conflict!)
    ;; Point to the next conflict's <<<<<<< line, wrapping around.
    (let* ([b (current-buffer)]
           [n (buffer-line-count b)]
           [from (car (point))]
           [hit (let scan ([r (+ from 1)] [left n])
                  (cond [(zero? left) #f]
                        [(>= r n) (scan 0 left)]
                        [(conflict-marker? b r "<<<<<<<") r]
                        [else (scan (+ r 1) (- left 1))]))])
      (if hit
          (goto-point! (cons hit 0))
          (set-message! "No conflicts"))
      (void)))

  (define (keep-mine!)
    ;; Resolve the conflict at point in the buffer's favor.
    (let ([c (conflict-at (car (point)))])
      (if c
          (begin
            (call-as-one-edit! "keep mine"
              (lambda ()
                (delete-rows! (cadr c) (caddr c))
                (delete-rows! (car c) (car c))
                (goto-point! (cons (car c) 0))))
            (set-message! "Kept the buffer side"))
          (set-message! "Not in a conflict"))
      (void)))

  (define (keep-disk!)
    ;; Resolve the conflict at point in the disk's favor.
    (let ([c (conflict-at (car (point)))])
      (if c
          (begin
            (call-as-one-edit! "keep disk"
              (lambda ()
                (delete-rows! (caddr c) (caddr c))
                (delete-rows! (car c) (cadr c))
                (goto-point! (cons (car c) 0))))
            (set-message! "Kept the disk side"))
          (set-message! "Not in a conflict"))
      (void)))

  ;;; The buffer list -----------------------------------------------------------

  (define (pad s width)
    (let ([n (string-length s)])
      (if (>= n width) s (string-append s (make-string (- width n) #\space)))))

  (define (pad-left s width)
    (let ([n (string-length s)])
      (if (>= n width) s (string-append (make-string (- width n) #\space) s))))

  (define (abbreviate-home path)
    (let ([home (getenv "HOME")])
      (if (and home (string:prefix? (string-append home "/") path))
          (string-append "~" (string:tail path (string-length home)))
          path)))

  (define buffers-view #f)
  (define buffer-rows '())

  (define (buffers-styles line)
    ;; Separate the headings from the data; in data rows the third status cell
    ;; is M, where a star makes the complete modified-buffer row italic.
    (make-vector (string-length line)
                 (cond [(string:prefix? "CRM  Buffer" line) 'bold]
                       [(and (> (string-length line) 2)
                             (char=? (string-ref line 2) #\*))
                        'italic]
                       [else 'plain])))

  (define (buffer-table)
    ;; The complete rendering and its source rows. Keeping this pure lets the
    ;; view refresh on every redraw without mutation hooks throughout the
    ;; editor.
    (let* ([current (current-buffer)]
           [listed (sort (lambda (a b)
                           (string-ci<? (head:buffer-name a) (head:buffer-name b)))
                         (buffer-list))]
           [view-lines (+ (length listed) 1)]
           [rows (map (lambda (b)
                        (list (string-append
                                (if (eq? b current) "." " ")
                                (if (head:buffer-read-only b) "%" " ")
                                (if (head:buffer-modified b) "*" " "))
                              (head:buffer-name b)
                              (number->string
                                (if (eq? b buffers-view)
                                    view-lines
                                    (buffer-line-count b)))
                              (or (mode:name-of b) "")
                              (let ([f (head:buffer-file b)])
                                (if f (abbreviate-home f) ""))))
                      listed)]
           [all (cons (list "CRM" "Buffer" "Lines" "Mode" "File") rows)]
           [width (lambda (i)
                    (fold-left (lambda (w r)
                                 (max w (string-length (list-ref r i))))
                               0 all))]
           [wname (width 1)] [wlines (width 2)] [wmode (width 3)]
           [render (lambda (r)
                     (format "~a  ~a  ~a  ~a  ~a"
                             (car r)
                             (pad (list-ref r 1) wname)
                             (pad-left (list-ref r 2) wlines)
                             (pad (list-ref r 3) wmode)
                             (list-ref r 4)))])
      (set! buffer-rows listed)
      (cons (map render all) rows)))

  (define (refresh-buffers-view!)
    (head:view-replace! buffers-view (car (buffer-table))))

  (define (buffer-row b)
    (let loop ([left buffer-rows] [row 1])
      (cond [(null? left) #f]
            [(eq? (car left) b) row]
            [else (loop (cdr left) (+ row 1))])))

  (define (select-buffer-row! b)
    (let ([row (buffer-row b)])
      (when row (goto-point! (cons row 0)))))

  (define (clamp-buffer-row!)
    (when (pair? buffer-rows)
      (goto-point! (cons (min (max 1 (car (point))) (length buffer-rows)) 0))))

  (define (move-buffer-row! delta)
    (clamp-buffer-row!)
    (goto-point! (cons (min (max 1 (+ (car (point)) delta))
                            (length buffer-rows))
                       0)))

  (define (activate-buffer-row!)
    (clamp-buffer-row!)
    (let* ([row (car (point))]
           [b (and (<= 1 row (length buffer-rows))
                   (list-ref buffer-rows (- row 1)))])
      (when b
        (show-buffer! b)
        ;; the view acts on the selected window -- its own, so activating
        ;; a row replaces the list with the buffer (unless it is the list)
        (when (eq? (current-buffer) buffers-view)
          (refresh-buffers-view!)
          (select-buffer-row! b)))))

  (define (previous-buffer)
    ;; the buffer the user was in before this view: next in MRU order
    (let ([bs (buffer-list)])
      (if (and (pair? bs) (pair? (cdr bs))) (cadr bs) (car bs))))

  (define (handle-buffers-event! event)
    (cond [(string=? event "FOCUS")
           (refresh-buffers-view!)
           (select-buffer-row! (previous-buffer))
           #t]
          [(member event '("UP" "C-p"))
           (move-buffer-row! -1) #t]
          [(member event '("DOWN" "C-n"))
           (move-buffer-row! 1) #t]
          [(string=? event "WHEEL-UP")
           (move-buffer-row! -1) (activate-buffer-row!) #t]
          [(string=? event "WHEEL-DOWN")
           (move-buffer-row! 1) (activate-buffer-row!) #t]
          [(string=? event "RET") (activate-buffer-row!) #t]
          [(string=? event "MOUSE-CLICK")
           (let ([clicked (app-event-buffer-position)])
             (if (and clicked
                      (<= 1 (car clicked) (length buffer-rows)))
                 (begin
                   (goto-point! (cons (car clicked) 0))
                   (activate-buffer-row!)
                   'keep-focus)
                 'ignore-click))]
          [else #f]))

  (define (switch-buffer-by-row! delta)
    ;; Global alphabetical traversal, matching the stable order in *buffers*.
    (let* ([current (current-buffer)]
           [listed (sort (lambda (a b)
                           (string-ci<? (head:buffer-name a) (head:buffer-name b)))
                         (buffer-list))]
           [tail (memq current listed)])
      (when (and tail (pair? (cdr listed)))
        (let ([next
               (cond [(positive? delta)
                      (if (pair? (cdr tail)) (cadr tail) (car listed))]
                     [(eq? current (car listed)) (car (reverse listed))]
                     [else
                      (let loop ([left listed])
                        (if (eq? (cadr left) current)
                            (car left)
                            (loop (cdr left))))])])
          (show-buffer! next)
          ;; Each window has its own point. If traversal enters the buffers app
          ;; in this window, its active row must describe what this window now
          ;; shows rather than inherit an unrelated row from another app window.
          (when (eq? next buffers-view)
            (refresh-buffers-view!)
            (select-buffer-row! buffers-view))))))

  (define (previous-buffer!) (switch-buffer-by-row! -1))
  (define (next-buffer!) (switch-buffer-by-row! 1))

  (define (buffers-view-buffer)
    ;; Created at startup, or recreated after the user kills the view.
    (or (and buffers-view (memq buffers-view (buffer-list)) buffers-view)
        (begin
          (set! buffers-view (head:register-app! "*buffers*"
                               refresh-buffers-view!
                               handle-buffers-event!))
          ;; Always show its position bar, using the globally selected side.
          (head:set-app-presentation! buffers-view 1 #t)
          (mode:choose! buffers-view "buffers")
          (refresh-buffers-view!)
          buffers-view)))

  (define (list-buffers!)
    ;; Use the live table as an interactive buffer switcher in this
    ;; window: Enter shows the chosen buffer here, completing the switch
    ;; in place.
    (let ([b (buffers-view-buffer)]
          [was (current-buffer)])
      (refresh-buffers-view!)
      (show-buffer! b)
      (refresh-buffers-view!)
      (select-buffer-row! was)
      (set-message! "")))


  ;;; Registration ----------------------------------------------------------------

  ;; Everything the layer registers -- owned by edit, so a reload
  ;; retracts and remakes it; what the loop and the seams ask of the
  ;; commands is installed here too.
  (define (init!)
    ;; The head's side of every log:add! -- present the fresh record
    ;; transiently in the echo area, styled by its component's styler.
    ;; Visible log views catch up at the next redraw.
    (log:set-presenter!
      (lambda (e show?)
        (when show?
          (if (message-progress)
              (paint:echo-append! (cadr e) (log:format-entry e)
                                  (log:styler (cadr e)) #t)
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
    (head:add-shutdown-hook! (lambda () (head:flush-ui-audit! 'all)))
    ;; the global commands a prompt may run without losing its input:
    ;; pure window management, and a prompt-safe stand-in for the one
    ;; that would nest a prompt
    (begin
      (for-each prompt:allow!
                (list focus-window-up! focus-window-down!
                      focus-window-left! focus-window-right!
                      other-window! split-window! split-window-right!
                      delete-window! delete-other-windows!))
      (prompt:allow! kill-buffer!! prompt-kill-buffer!))
    ;; The seat's loop and key dispatch live in (main); the mouse report
    ;; handler is the commands' and is installed here.
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
          ("M-UP" ,focus-window-up!) ("M-DOWN" ,focus-window-down!)
          ("M-LEFT" ,focus-window-left!) ("M-RIGHT" ,focus-window-right!)
          ("M->" ,end-of-buffer!) ("UP" ,previous-line!)
          ("DOWN" ,next-line!) ("LEFT" ,move-left!)
          ("RIGHT" ,move-right!) ("HOME" ,beginning-of-line!)
          ("END" ,end-of-line!) ("DELETE" ,delete-forward!)
          ("PAGEUP" ,page-up!) ("PAGEDOWN" ,page-down!)
          ("PASTE" ,paste-into-buffer!) ("SELF-INSERT" ,self-insert-command!)
          ("C-x C-g" ,keyboard-quit!) ("C-x C-s" ,save!!)
          ("C-x C-w" ,save-as!!) ("C-x C-c" ,quit!!)
          ("C-x C-f" ,find-file!!) ("C-x b" ,switch-buffer!!)
          ("C-x k" ,kill-buffer!!) ("C-x o" ,other-window!)
          ("C-x 0" ,delete-window!) ("C-x 1" ,delete-other-windows!)
          ("C-x 2" ,split-window!) ("C-x 3" ,split-window-right!)
          ("C-x l" ,line-numbers!) ("C-x t" ,wrap!)
          ("C-h k" ,describe-key!!)
          ("C-c a" ,answer!!)))
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
    ;; what the loop in (main) asks of the commands: how to open the file
    ;; argument, how to quit (the modified-buffers check), and what runs
    ;; after every key
    (begin
      (main:set-file-opener! visit-file!)
      (main:set-quit-command! quit!!)
      (main:set-after-key! clamp-point!))

    (mode:register! "buffers" '() '() buffers-styles)
    (doc:register!
      '(((replace-all!)
         (("procedure" . "(replace-all! from to [where])"))
         "integer" ("(edit)") edit "Editing commands" #f
         "Replace every occurrence of `from` with `to` in `where`. Each buffer is changed as one undo step and point is preserved. If `where` is omitted, use the selected region or the whole current buffer; it may also be a buffer, buffer name, region, buffer predicate, or list of these.")
        ((replace!!)
         (("procedure" . "(replace!! [from] [to] [where])"))
         "void" ("(edit)") edit "Editing commands" #f
         "Interactively visit occurrences of `from` in `where`, asking whether to replace each one with `to`. Any omitted arguments are prompted for. The query accepts `y` or Space to replace, `n` to skip, and `q` or Return to stop.")
        ((next-conflict!) (("procedure" . "(next-conflict!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Move point to the next merge conflict marker in the current buffer, wrapping at the end. Report a message if the buffer has no conflicts.")
        ((keep-mine!) (("procedure" . "(keep-mine!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Resolve the merge conflict at point by keeping the buffer side. The complete resolution is one undo step.")
        ((keep-disk!) (("procedure" . "(keep-disk!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Resolve the merge conflict at point by keeping the disk side. The complete resolution is one undo step.")
        ((list-buffers!) (("procedure" . "(list-buffers!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Show `*buffers*` in the current window and make that window its own target. Move through its alphabetical rows with Up, Down, or the wheel; press Enter to replace the app with the selected buffer, or click a row to switch immediately.")
        ((previous-buffer!) (("procedure" . "(previous-buffer!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Switch the current window to the previous buffer in alphabetical order, wrapping at the beginning.")
        ((next-buffer!) (("procedure" . "(next-buffer!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Switch the current window to the next buffer in alphabetical order, wrapping at the end.")))
    (buffers-view-buffer)
    (paint:add-highlighter!
      (lambda ()
        (cond
          [(and buffers-view (eq? (current-buffer) buffers-view))
           (if (> (car (point)) 0)
               (let ([row (car (point))])
                 (list
                   (list buffers-view row 0
                         (string-length (buffer-line buffers-view row))
                         'active-shadow)
                   (list (selected-window) row 0
                         (string-length (buffer-line buffers-view row))
                         'active)))
               '())]
          [(and buffers-view (memq buffers-view (buffer-list))
                (buffer-row (current-buffer)))
           => (lambda (row)
                (list (list buffers-view row 0
                            (string-length (buffer-line buffers-view row))
                            'active-shadow)))]
          [else '()])))
    (keymap:bind-default! "C-x C-b" list-buffers!)
    (keymap:bind-default! "M-S-UP" previous-buffer!)
    (keymap:bind-default! "M-S-DOWN" next-buffer!)
    (keymap:bind-default! "M-%" replace!!)
    (keymap:bind-default! "M-n" next-conflict!)
    (keymap:bind-default! "M-m" keep-mine!)
    (keymap:bind-default! "M-d" keep-disk!)
    (for-each
      (lambda (entry)
        (keymap:bind-default! 'query-replace (car entry) (cadr entry)))
      '(("y" replace) ("Y" replace) ("SPC" replace)
        ("n" skip) ("N" skip) ("BACKSPACE" skip)
        ("q" stop) ("RET" stop) ("C-g" stop) ("ESC" stop)
        ("C-x" quit-prefix) ("C-x C-c" quit-editor)))
    (paint:add-highlighter!
      (lambda () (if query-match (list query-match) '())))
  )

) ;; library (edit)
