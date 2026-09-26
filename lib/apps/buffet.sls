;; buffet.sls -- the <buffet> app, a spread of the buffers to pick from: the library (buffet).
;;
;; A live table of the head's buffers behind both switching keys, with a
;; name and path filter, ordered column sorts, modification clocks and a
;; candidate kept by buffer identity, each window with its own; below the
;; live rows the backups and the trash, dimmed, one key from restoring.  The app handler
;; receives the events the dispatcher sends, so the suite drives it
;; headless.

(import (only (foundation edoc) elibrary))
(elibrary (apps buffet)
  (export choose! (rename (chosen-entry chosen)) clear-filter! erase! extend-filter! filter! first-row! init! last-row! next! next-row!
          open! page-down! page-up! paste-filter! previous! previous-row! return! (rename (select-buffer! select!)) toggle-sort-column!)
  (import (chezscheme)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head dispatch) dispatch:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head style) style:)
          (prefix (head table) table:)
          (prefix (service file) file:)
          (prefix (state store) store:)
          (prefix (sys glyph) glyph:)
          (prefix (sys tty) tty:))

  ;;; The model -------------------------------------------------------------------

  (define view #f)
  (define rows '())                 ; the listed entries: buffers, then the backups and the trash under their headings
  (define buffer-filter "")
  (define buffer-sorts '())         ; (column . descending?) in priority order
  (define columns
    (table:make '#("Modified" "RO" "Buffer" "Lines" "Mode" "File")
      '#(10 5 9 8 7 10) 2 '(4 3 1 0 5) '#(text text text right text tail)))
  (define first-row 2)              ; sticky filter and column headings
  (define filter-label "Filter: ")
  (define trash-heading "Trash: Enter restores")
  (define backups-heading "Backups: Enter restores")
  (define-record-type choice (fields (mutable origin) (mutable selected) (mutable columns)))
  (define choices (make-weak-eq-hashtable))
  ;; A trashed buffer's row keeps its identity across refreshes, so a
  ;; selection or a hover on it survives the next rebuild; a backup's row
  ;; carries the path of the file whose version it holds.
  (define-record-type trashed (fields name killed-at path))
  (define trash-entries (make-hashtable string-hash string=?))
  ;; The pointer targets an entry or a column number in one window. A
  ;; hovered entry takes precedence over that window's keyboard candidate;
  ;; a heading only decorates its label. Neither moves point or the viewport.
  (define hover #f)

  (define (hover-of w)
    (and hover (eq? (car hover) w) (cdr hover)))

  (define (entry-at-row row)
    (and (<= first-row row (+ first-row (length rows) -1))
         (list-ref rows (- row first-row))))

  (define (row-of entry)
    (let loop ([left rows] [row first-row])
      (cond [(null? left) #f]
            [(eq? (car left) entry) row]
            [else (loop (cdr left) (+ row 1))])))

  (define (heading-of entry)
    ;; a section's heading text, or #f for a buffer's or a trashed row
    (case entry [(trash) trash-heading] [(backups) backups-heading] [else #f]))

  (define (selectable? entry)
    ;; a heading is a row, not a candidate
    (and entry (not (heading-of entry))))

  (define (column-at w at)
    ;; Sorting and hover share the column's padded hit area; gaps are inert.
    (and at (= (car at) (- first-row 1))
         (find (lambda (column) (<= (cadr column) (cdr at) (- (caddr column) 1)))
           (choice-columns (choice-for w)))))

  (define (other-buffer was)
    (find (lambda (b) (and (not (eq? b was)) (not (eq? b view)))) (head:buffers)))

  (define (choice-for w)
    (or (hashtable-ref choices w #f)
        (let ([choice (make-choice (other-buffer view)
                        (let ([e (entry-at-row (head:window-prow w))]) (and (selectable? e) e)) '())])
          (hashtable-set! choices w choice)
          choice)))

  (define (candidate w)
    (let ([e (if (memq (hover-of w) rows) (hover-of w) (choice-selected (choice-for w)))])
      (and (selectable? e) (memq e rows) e)))

  ;;; The table -------------------------------------------------------------------

  (define (styles b row line)
    ;; A row provider sees live metadata even if its printed text is equal.
    ;; A text-only style cache cannot notice modified -> saved transitions.
    (let ([styles
           (make-vector (string-length line)
             (let ([entry (entry-at-row row)])
               (cond [(zero? row) 'plain]
                     [(< row first-row) 'header]
                     [(heading-of entry) 'chrome]
                     [(trashed? entry) 'ghost]
                     [(not entry) 'chrome]
                     [(head:buffer-modified entry) 'italic]
                     [else 'plain])))])
      (when (zero? row)
        (style:fill-range! styles 0 (min (string-length filter-label) (vector-length styles)) 'chrome))
      ;; a conflicted buffer's !! in red, as on its status line
      (let ([entry (entry-at-row row)])
        (when (and (>= row first-row) (head:buffer? entry) (head:buffer-conflicted entry))
          (style:fill-range! styles 0 (min 2 (vector-length styles)) 'error)))
      styles))

  (define (buffer-data b)
    (vector (and (head:buffer-modified b) (head:buffer-modified-at b))
            (and (head:buffer-read-only b) #t) (head:buffer-name b) (head:buffer-line-count b)
            (or (mode:name-of b) "") (or (head:buffer-file b) "") (head:buffer-conflicted b)))

  (define (age-text seconds)
    ;; how long, in the coarsest unit that is not zero
    (cond [(< seconds 60) (format "~a s" seconds)]
          [(< seconds 3600) (format "~a min" (quotient seconds 60))]
          [(< seconds 86400) (format "~a h" (quotient seconds 3600))]
          [else (format "~a d" (quotient seconds 86400))]))

  (define (trash-data e now retention)
    ;; a trashed buffer's columns: how long ago it was killed, or a backup
    ;; read, its name, its kind, then a backup's file or how long a trashed
    ;; buffer stays before the base deletes it
    (let ([left (- (+ (trashed-killed-at e) (* retention 86400)) now)])
      (vector (string-append (age-text (max 0 (- now (trashed-killed-at e)))) " ago") "" (trashed-name e) ""
              (if (trashed-path e) "backup" "trash")
              (cond [(trashed-path e)] [(> left 0) (string-append (age-text left) " left")] [else "expiring"]))))

  (define (trash-rows)
    ;; the trash, then the backups, as rows, each newest first and each
    ;; name keeping its record
    (let ([fresh (make-hashtable string-hash string=?)])
      (let ([entries
             (map (lambda (t)
                    (let* ([name (car t)] [old (hashtable-ref trash-entries name #f)]
                           [e (if (and old (= (trashed-killed-at old) (cadr t)) (equal? (trashed-path old) (caddr t)))
                                  old
                                  (make-trashed name (cadr t) (caddr t)))])
                      (hashtable-set! fresh name e)
                      e))
                  (append (map (lambda (t) (list (car t) (cadr t) #f)) (edit:trash))
                          ;; a backup as (name path observed ...): its time is the read
                          (map (lambda (b) (list (car b) (caddr b) (cadr b))) (edit:backups))))])
        (set! trash-entries fresh)
        entries)))

  (define (cell data column)
    (let ([value (vector-ref data column)])
      (cond [(= column 0)
             (cond [(and (> (vector-length data) 6) (vector-ref data 6)) "!!"]
                   [(string? value) value]
                   [value
                    (let ([date (time-utc->date
                                  (make-time 'time-utc (mod value 1000000000) (div value 1000000000)))])
                      (format "~2,'0d:~2,'0d:~2,'0d" (date-hour date) (date-minute date) (date-second date)))]
                   [else ""])]
            [(boolean? value) (if value "%" "")]
            [(number? value) (number->string value)]
            [(= column 5) (file:abbreviate value)]
            [else value])))

  (define (entry<? a b)
    (table:less? buffer-sorts (lambda (entry column) (vector-ref (cdr entry) column))
      (lambda (a b)
        (let ([x (vector-ref (cdr a) 2)] [y (vector-ref (cdr b) 2)])
          (or (string-ci<? x y) (and (string-ci=? x y) (string<? x y))))) a b))

  (define (heading column)
    (table:heading columns buffer-sorts column))

  (define (cycle-sort! column)
    (set! buffer-sorts (table:cycle-sort buffer-sorts column))
    (set! hover #f)
    (refresh!))

  (define (matches? entry)
    ;; live and backup rows by name, path and shown path; trash rows by name
    (let ([data (cdr entry)])
      (exists (lambda (s) (string:search s buffer-filter 0 (string-length s) #t))
        (if (and (trashed? (car entry)) (not (trashed-path (car entry))))
            (list (vector-ref data 2))
            (list (vector-ref data 2) (vector-ref data 5) (cell data 5))))))

  (define (table-lines entries all width)
    ;; the fitted rows for one window: the filter line, the headings, then
    ;; the entries, the backups and the trash under their own headings
    (let-values ([(row cols) (table:layout columns buffer-sorts (map cdr all) cell width)])
      (values
        (cons* (let* ([label filter-label] [n (glyph:cells label)])
                 (string-append (glyph:fit label (min n width))
                   (glyph:fit buffer-filter (max 0 (- width n)) 'left)))
          (row #f)
          (if (null? entries) (list (glyph:fit "No matching buffers" width))
              (map (lambda (entry)
                     (cond [(heading-of (car entry)) => (lambda (text) (glyph:fit text width))]
                           [else (row (cdr entry))]))
                   entries)))
        cols)))

  (define (table-source entries)
    ;; Shared rows retain unelided field text, independent of window width.
    ;; Each window presentation has these same logical rows.
    (cons* (string-append filter-label buffer-filter)
      (string:join (map heading (iota 6)) "  ")
      (if (null? entries) '("No matching buffers")
          (map (lambda (entry)
                 (or (heading-of (car entry))
                     (let ([line (string:join (map (lambda (i) (cell (cdr entry) i)) (iota 6)) "  ")])
                       (glyph:fit line (glyph:cells line)))))
               entries))))

  (define (refresh!)
    (head:call-with-display-update
      (lambda ()
        (let* ([live (map (lambda (b) (cons b (buffer-data b))) (head:buffers))]
               [now (time-second (current-time 'time-utc))]
               [retention (store:trash-retention)]
               [kept (map (lambda (e) (cons e (trash-data e now retention))) (trash-rows))]
               [all (append live kept)]
               [matches (filter matches? live)]
               [backups (filter (lambda (entry) (and (trashed-path (car entry)) (matches? entry))) kept)]
               [trashed (filter (lambda (entry) (and (not (trashed-path (car entry))) (matches? entry))) kept)]
               ;; a section is its heading over its rows, or nothing
               [section (lambda (heading entries) (if (null? entries) '() (cons (cons heading #f) entries)))]
               [entries
                (begin
                  ;; The self row describes this publication, including its
                  ;; line count for sorting, without a second refresh.
                  (let ([self (assq view live)])
                    (when self
                      (vector-set! (cdr self) 3
                        (+ first-row (max 1 (+ (length matches) (length (section 'backups backups))
                                               (length (section 'trash trashed))))))))
                  (append (sort entry<? matches) (section 'backups backups) (section 'trash trashed)))]
               [saved (map (lambda (w)
                             (list w (choice-for w) (entry-at-row (head:window-top w))))
                        (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))])
          (set! rows (map car entries))
          (let* ([presentations
                  (map (lambda (entry)
                         (let ([w (car entry)])
                           (let-values ([(lines cols) (table-lines entries all (head:window-content-width w))])
                             (choice-columns-set! (cadr entry) cols)
                             (cons w lines)))) saved)]
                 [placements
                  (apply append
                    (map (lambda (entry)
                           (let* ([w (car entry)] [choice (cadr entry)]
                                  [selected (choice-selected choice)]
                                  [row (or (row-of selected) (and (pair? rows) first-row))])
                             (when row
                               (let ([e (entry-at-row row)])
                                 (choice-selected-set! choice (and (selectable? e) e))))
                             (list (cons w (cons (or row first-row) 0))
                                   (cons (cons 'top w) (cons (or (row-of (caddr entry)) first-row) 0))))) saved))])
            (head:view-replace! view (table-source entries) '() placements presentations))
          (when (and hover (not (or (memq (cdr hover) rows)
                                  (assv (cdr hover) (choice-columns (choice-for (car hover)))))))
            (set! hover #f))))))

  ;;; Navigation and switching ------------------------------------------------------------

  (define (select-row! e)
    (let ([row (row-of e)])
      (when row
        (choice-selected-set! (choice-for (head:current-window)) e)
        (head:goto! (cons row 0)))))

  (define (move-row! delta)
    (let* ([last (+ first-row (length rows) -1)]
           [from (or (row-of (candidate (head:current-window))) first-row)])
      (set! hover #f)
      (when (pair? rows)
        (let step ([row (min (max first-row (+ from delta)) last)])
          (let ([e (entry-at-row row)])
            (cond [(selectable? e) (select-row! e)]
                  ;; a heading is passed over in the direction of travel
                  [(and (>= delta 0) (< row last)) (step (+ row 1))]
                  [(> row first-row) (step (- row 1))]
                  [(< row last) (step (+ row 1))]
                  [else (void)]))))))

  (define (activate-row!)
    ;; Panel clicks change the focused window; keyboard use replaces the
    ;; list here. Both use the visible candidate: a buffer is shown, a
    ;; trashed one or a backup restored.
    (let* ([e (candidate (head:current-window))]
           [target (head:app-event-focus)])
      (when e
        (set! hover #f)
        (when (and target (memq target (head:windows))) (head:set-current! target))
        (if (trashed? e) (edit:restore! (trashed-name e)) (head:show-buffer! e)))))

  (edoc "Set the filter: buffers whose names, paths or shown paths contain the text stay listed."
        (text string "the filter text"))
  (define (filter! text)
    (set! hover #f)
    (set! buffer-filter text)
    (refresh!))

  (define (page) (max 1 (- (head:window-size (head:current-window)) first-row)))

  ;;; The app as an API: what M-x or an agent asks and does --------------------------

  (edoc "The chosen entry in the current window: a buffer, a trashed or backup buffer's name, or #f without one."
        (returns (or buffer string #f)))
  (define (chosen-entry)
    ;; read without making a window's choice record, as candidate would
    (let* ([w (head:current-window)] [choice (hashtable-ref choices w #f)]
           [e (if (memq (hover-of w) rows) (hover-of w) (and choice (choice-selected choice)))]
           [e (and (selectable? e) (memq e rows) e)])
      (cond [(not e) #f] [(trashed? e) (trashed-name e)] [else e])))

  (edoc "Make a listed buffer the choice in the current window; refused when it is not listed."
        (b buffer "the buffer"))
  (define (select-buffer! b)
    (let ([b (edoc:type-value 'buffer b)])
      (unless (memq b rows) (error 'select! "the buffer is not listed" (head:buffer-name b)))
      (select-row! b)))

  (edoc "Switch this window to the chosen buffer, or restore a chosen trashed one or backup.")
  (define (choose!) (activate-row!))

  (edoc "Move the choice to the next buffer.")
  (define (next-row!) (move-row! 1))

  (edoc "Move the choice to the previous buffer.")
  (define (previous-row!) (move-row! -1))

  (edoc "Move the choice a page of rows down.")
  (define (page-down!) (move-row! (page)))

  (edoc "Move the choice a page of rows up.")
  (define (page-up!) (move-row! (- (page))))

  (edoc "Move the choice to the first buffer.")
  (define (first-row!) (move-row! (- (length rows))))

  (edoc "Move the choice to the last buffer.")
  (define (last-row!) (move-row! (length rows)))

  (edoc "Erase the filter's last character.")
  (define (erase!)
    (unless (string=? buffer-filter "")
      (filter! (substring buffer-filter 0
                 (- (string-length buffer-filter) (car (car (reverse (glyph:clusters buffer-filter)))))))))

  (edoc "Clear the filter, every buffer listed again.")
  (define (clear-filter!) (filter! ""))

  (edoc "Add text to the filter, as typing does: SELF-INSERT, any character, runs it with the character typed."
        (text string "the text to add"))
  (define (extend-filter! text) (filter! (string-append buffer-filter text)))

  (edoc "Sort the buffers by a column, the same column again reversing the order: 1 modified, 2 read-only, 3 buffer, 4 lines, 5 mode, 6 file; F1 to F6 sort by the column of their number."
        (column integer "the column, 1 to 6"))
  (define (toggle-sort-column! column)
    (unless (and (integer? column) (exact? column) (<= 1 column 6))
      (error 'toggle-sort-column! "expected a column, 1 to 6" column))
    (cycle-sort! (- column 1)))

  (edoc "Return to the buffer the buffet replaced in this window.")
  (define (return!)
    (let* ([origin (choice-origin (choice-for (head:current-window)))]
           [b (if (memq origin (head:buffers)) origin (other-buffer view))])
      (set! hover #f)
      (when b (head:show-buffer! b))))

  (edoc "Add the pasted text to the filter, control characters dropped.")
  (define (paste-filter!)
    (filter! (string-append buffer-filter
               (list->string (filter (lambda (c) (>= (char->integer c) 32)) (string->list (head:read-paste)))))))

  ;; The keys of the buffet, bound in its mode's context to the
  ;; commands above, so the keys helper lists them and C-h k describes them
  (define buffet-keys
    `((("RET") ,choose!)
      (("DOWN" "C-n" "TAB") ,next-row!) (("UP" "C-p" "S-TAB") ,previous-row!)
      (("PGDN" "C-v") ,page-down!) (("PGUP" "M-v") ,page-up!)
      (("HOME" "C-a" "M-<") ,first-row!) (("END" "C-e" "M->") ,last-row!)
      (("BS" "C-h") ,erase!) (("C-u") ,clear-filter!)
      (("ESC" "C-g") ,return!) (("PASTE") ,paste-filter!)
      (("SELF-INSERT") ,(keymap:call extend-filter! head:typed-text))
      ;; the function keys sort by the column of their number
      ,@(map (lambda (n) (list (list (format "F~a" n)) (keymap:call toggle-sort-column! n))) '(1 2 3 4 5 6))))

  (define (handle! event)
    ;; what the buffet context leaves to the app: focus, the wheel and the
    ;; pointer; typing grows the filter through the context's SELF-INSERT
    (cond [(string=? event "FOCUS") (refresh!) #t]
          [(member event '("WHEEL-UP" "WHEEL-DOWN"))
           (let ([target (head:app-event-focus)] [up? (string=? event "WHEEL-UP")])
             (if (and target (not (eq? target (head:current-window))) (memq target (head:windows)))
                 (begin
                   (head:set-current! target)
                   (dispatch:global-key! (if up? "M-S-UP" "M-S-DOWN")))
                 (move-row! (if up? -1 1)))) #t]
          [(string=? event "MOUSE-MOVE")
           (let* ([at (head:app-event-buffer-position)]
                  [target (or (let ([e (and at (entry-at-row (car at)))]) (and (selectable? e) e))
                              (cond [(column-at (head:current-window) at) => car] [else #f]))])
             (set! hover (and target (cons (head:current-window) target))))
           #t]
          [(member event '("MOUSE-LEAVE" "BLUR")) (set! hover #f) #t]
          [(member event '("MOUSE-RELEASE" "MOUSE-DRAG"))
           (let ([e (choice-selected (choice-for (head:current-window)))])
             (when e (select-row! e))) #t]
          [(string=? event "MOUSE-CLICK")
           (let* ([at (head:app-event-buffer-position)]
                  [e (let ([e (and at (entry-at-row (car at)))]) (and (selectable? e) e))]
                  [column (column-at (head:current-window) at)])
             (cond [e (set! hover #f) (select-row! e) (activate-row!) 'keep-focus]
                   [column (cycle-sort! (car column))
                           (set! hover (cons (head:current-window) (car column))) 'keep-focus]
                   [else 'ignore-click]))]
          [else #f]))

  (define (switch-by-row! delta)
    ;; Global alphabetical traversal is independent of the table's filter/sort.
    (let* ([current (head:current-buffer)]
           [listed (sort (lambda (a b)
                           (string-ci<? (head:buffer-name a) (head:buffer-name b)))
                         (head:buffers))]
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
          (if (eq? next view) (open!) (head:show-buffer! next))))))

  (edoc "Switch the current window to the previous buffer in alphabetical order, wrapping at the beginning; the buffet's own turn opens the app.")
  (define (previous!) (switch-by-row! -1))

  (edoc "Switch the current window to the next buffer in alphabetical order, wrapping at the end; the buffet's own turn opens the app.")
  (define (next!) (switch-by-row! 1))

  (define (ensure!)
    ;; Created at startup, or recreated after the user kills the view.
    (or (and view (memq view (head:buffers)) view)
        (begin
          (set! view (head:register-app! "*buffet*" refresh! handle!))
          ;; inventory, not a visit: head:show-buffer! keeps it behind the documents
          (head:buffer-fact-set! view 'recency 'behind)
          ;; A position bar on the configured side, only while the rows
          ;; overflow the window.
          (head:set-app-presentation! view first-row 'auto #f)
          (head:set-app-cursor-visible! view #f)
          (head:set-app-selectable! view #f)
          (head:set-app-status-position! view (lambda (b) ""))   ; the name alone
          (mode:choose! "buffet" view)
          (refresh!)
          view)))

  (edoc "Show the buffet in the current window with the most recently used other buffer selected: type to filter, arrows choose, Enter switches to the row's buffer or restores a trashed one or a backup, Esc returns.")
  (define (open!)
    ;; Both switch shortcuts use one app. The app itself never displaces the
    ;; previous document as the default, even after repeated quick switches.
    (let ([b (ensure!)]
          [was (head:current-buffer)])
      (head:call-with-display-update
        (lambda ()
          (set! buffer-filter "")
          (set! hover #f)
          (hashtable-set! choices (head:current-window)
            (make-choice
              (if (eq? was b) (choice-origin (choice-for (head:current-window))) was)
              (or (other-buffer was) was) '()))
          (head:show-buffer! b)
          (refresh!)))
      (edit:set-message! "")))

  ;;; Registration -------------------------------------------------------------------

  (edoc "Install the buffet: its mode with its keys bound in the buffet context, its view, kill hook and highlighter, and the keys C-x b, C-x C-b, M-S-UP and M-S-DOWN.")
  (define (init!)
    (mode:register! "buffet" '() '() (lambda (line) #f) #f styles)
    (for-each (lambda (entry) (for-each (lambda (key) (keymap:bind-default! 'buffet key (cadr entry))) (car entry))) buffet-keys)
    (ensure!)
    (head:add-buffer-kill-hook!
      (lambda (b)
        ;; A hidden picker must not keep a killed document's text alive.
        (when (and hover (eq? (cdr hover) b)) (set! hover #f))
        (vector-for-each
          (lambda (choice)
            (when (eq? (choice-origin choice) b) (choice-origin-set! choice #f))
            (when (eq? (choice-selected choice) b) (choice-selected-set! choice #f)))
          (hashtable-values choices))
        (when (eq? b view)
          (set! view #f)
          (set! hover #f)
          (set! rows '())
          (hashtable-clear! choices))))
    (paint:add-highlighter!
      (lambda ()
        ;; Strong blue describes the focused document in other panes. Bold and
        ;; a subtle tint mark the hovered row or focused list's candidate.
        (if (and view (memq view (head:buffers)))
            (let ([active-row (row-of (head:current-buffer))]
                  [row-range
                   (lambda (w row face)
                     (list w row 0
                           (string-length (vector-ref (head:window-lines w) row)) face))])
              (apply append
                (map (lambda (w)
                       (let* ([over (and (head:mouse-position) (hover-of w))]
                              [column (assv over (choice-columns (choice-for w)))]
                              [row (row-of (candidate w))])
                         (append
                           (if (and active-row (not (eq? w (head:current-window))))
                               (list (row-range w active-row 'active)) '())
                           (if column
                               (list (list w (- first-row 1) (cadr column)
                                       (min (caddr column)
                                            (+ (cadr column) (string-length (heading (car column)))))
                                       'hover))
                               '())
                           (if (and row (or (eq? w (head:current-window)) (memq over rows)))
                               (list (row-range w row (if (memq over rows) 'candidate-hover 'candidate))) '()))))
                     (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))))
            '())))
    (keymap:bind-default! "C-x b" open!)
    (keymap:bind-default! "C-x C-b" open!)
    (keymap:bind-default! "M-S-UP" previous!)
    (keymap:bind-default! "M-S-DOWN" next!)))
