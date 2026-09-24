;; delta-log.sls -- the delta log at M-x: a buffer's entries as data, a
;; view with entries disabled shown live in a local buffer, committed as a
;; rewrite of the trunk for everyone or abandoned, the revision, batch and
;; conflict types completing from the current buffer's log and its pending
;; reload conflicts, a candidate previewing itself while a prompt has it,
;; and the <delta-log> browser over the entries or the conflicts.

(import (only (foundation edoc) elibrary))
(elibrary (apps delta-log)
  (export (rename (delta-log-cancel! cancel!)) (rename (delta-log-close! close!)) (rename (delta-log-commit! commit!))
          (rename (delta-log-conflicts conflicts)) (rename (delta-log-conflicts! conflicts!))
          (rename (delta-log-disabled disabled)) (rename (delta-log-filter! filter!)) (rename (delta-log-flip! flip!))
          (rename (delta-log-flip-row! flip-row!)) init! (rename (delta-log-keep-disk! keep-disk!))
          (rename (delta-log-keep-mine! keep-mine!)) (rename (delta-log-entries log)) (rename (delta-log-next! next!))
          (rename (delta-log-open! open!)) (rename (delta-log-previous! previous!)) (rename (delta-log-resolve! resolve!))
          (rename (delta-log-resolve-all! resolve-all!)) (rename (delta-log-revert! revert!)) (rename (delta-log-show! show!))
          (rename (delta-log-show-row! show-row!)) (rename (delta-log-toggle! toggle!)) (rename (delta-log-toggle-row! toggle-row!))
          (rename (delta-log-view view)))
  (import (rnrs)
          (only (chezscheme) format void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head window) window:)
          (prefix (state store) store:))

  ;;; The log -------------------------------------------------------------------

  ;; One live view per head: the trunk buffer, the local buffer showing the
  ;; view's text, the revisions disabled, and the conflicts the last rendering
  ;; found, (disabled . later) pairs naming a later entry that overlaps a
  ;; disabled one.
  (define-record-type view (fields trunk buffer (mutable disabled) (mutable conflicts)))
  (define the-view #f)

  (define (trunk-of b)
    ;; the shared buffer whose log b shows: b itself, or the trunk behind
    ;; the view, the browser or a flip
    (cond [(and the-view (eq? b (view-buffer the-view))) (view-trunk the-view)]
          [(and browser (eq? b browser) browser-trunk) browser-trunk]
          [(and flip (eq? b (car flip))) (caddr flip)]
          [else b]))

  (define (current-trunk who)
    (let ([b (trunk-of (head:current-buffer))])
      (unless (head:buffer-store-id b) (error who "not a shared buffer" (head:buffer-name b)))
      b))

  (define (trunk-id b) (head:buffer-store-id b))

  (define (spell v) (format "~s" v))

  (define (entry-hint row . context)
    ;; an entry in one line, its revision apart: the actor, where it wrote,
    ;; what it removed and inserted, its batch, the entry it reverts when
    ;; it is an inverse, and, disabled while a live inverse reverts it, what
    ;; that inverse did to it and its revision, undone by 4 say, when the
    ;; log's (revision inverse kind) triples are given
    (let* ([actor (cadr row)] [delta (cadddr row)] [state (list-ref row 5)]
           [span (car delta)]
           [removed (string:join (cadr delta) "\n")]
           [inserted (string:join (caddr delta) "\n")]
           [disabler (and (pair? context) (assv (car row) (car context)))])
      (string-append
        (format "~a  ~a:~a" (spell actor) (car span) (cadr span))
        (if (string=? removed "") "" (string-append "  -" (spell (string:elide removed 24))))
        (if (string=? inserted "") "" (string-append "  +" (spell (string:elide inserted 24))))
        (batch-label row)
        (origin-label row)
        (cond [(eq? state 'enabled) ""]
              [disabler (format "  ~a by ~a" (participle (caddr disabler)) (cadr disabler))]
              [else "  disabled"]))))

  (define (entry-text row . context) (format "~a  ~a" (car row) (apply entry-hint row context)))

  (define (batch-of row) (cond [(assq 'batch (caddr row)) => cdr] [else #f]))

  (define (batch-label row)
    ;; the entry's batch in brief: its counter alone when the batch is the
    ;; entry's actor's own, else with the actor that minted it
    (let ([id (batch-of row)])
      (cond [(not id) ""]
            [(and (list? id) (= (length id) 2) (equal? (car id) (cadr row))) (format "  batch ~a" (cadr id))]
            [(and (list? id) (= (length id) 2)) (format "  batch ~a of ~a" (cadr id) (spell (car id)))]
            [else (format "  batch ~s" id)])))

  (define (verb kind)
    ;; what an inverse of a kind does to its target
    (case kind [(undo) "undoes"] [(redo) "redoes"] [(rewrite) "reverts"] [(reload) "disables"] [else (format "~a" kind)]))

  (define (participle kind)
    ;; what its target has had done to it
    (case kind [(undo) "undone"] [(redo) "redone"] [(rewrite) "reverted"] [else "disabled"]))

  (define (origin-label row)
    ;; what an inverse reverts: the entry an undo undoes, a redo redoes, a
    ;; rewrite reverts or a reload disables
    (let ([origin (list-ref row 4)])
      (if (not origin) "" (format "  ~a ~a" (verb (car origin)) (list-ref origin 3)))))

  (define (disablers rows)
    ;; (revision inverse kind) for each entry a live inverse reverts, the
    ;; newest live inverse counting, as the store derives the state
    (let loop ([rows rows] [acc '()])
      (cond
        [(null? rows) acc]
        [(let ([origin (list-ref (car rows) 4)])
           (and origin (eq? (list-ref (car rows) 5) 'enabled) (not (assv (list-ref origin 3) acc)) origin))
         => (lambda (origin) (loop (cdr rows) (cons (list (list-ref origin 3) (car (car rows)) (car origin)) acc)))]
        [else (loop (cdr rows) acc)])))

  (define selector-keys '(count actor batch since until state))

  (define (selector-of v)
    ;; a selector as given, or the one selecting the batch a batch literal
    ;; or id names
    (if (or (null? v) (and (pair? v) (pair? (car v)) (memq (caar v) selector-keys)))
        v
        (list (cons 'batch (edoc:type-value 'batch v)))))

  ;;; Marking an entry --------------------------------------------------------------

  (define previewed #f) ; (buffer . span) while a prompt previews a revision
  (define browsed #f) ; (buffer . span) of the browser's current row, while it is on screen

  (define (span-of trunk revision)
    ;; an entry's span rebased into the trunk's current text, with the trunk, or #f
    (let* ([id (trunk-id trunk)]
           [hit (find (lambda (entry) (= (caddr entry) revision)) (store:blame id (length (store:log id))))])
      (and hit (cons trunk (car hit)))))

  (define (span-ranges marked)
    ;; a marked span, row by row, in the match face
    (let* ([b (car marked)] [span (cdr marked)]
           [start (text:span-start span)] [end (text:span-end span)])
      (let loop ([row (car start)] [out '()])
        (if (> row (car end)) (reverse out)
            (let* ([from (if (= row (car start)) (cdr start) 0)]
                   [to (if (= row (car end)) (cdr end) (string-length (head:buffer-line b row)))])
              (loop (+ row 1) (cons (list b row from (max to (+ from 1)) 'match) out)))))))

  (define (entry-highlights)
    ;; the previewed entry's span, the browsed one's and a flip's region
    (append (if previewed (span-ranges previewed) '()) (if browsed (span-ranges browsed) '())
            (if flip (span-ranges (cons (car flip) (cadr flip))) '())))

  (define (preview-revision! revision)
    ;; bring the entry's span, rebased into the current text, under point and
    ;; highlight it; the thunk returned puts point back and clears the mark
    (let* ([trunk (guard (ex [else #f]) (current-trunk 'revision))]
           [marked (and trunk (integer? revision) (span-of trunk revision))])
      (and marked
           (let ([point (head:buffer-point trunk)])
             (set! previewed marked)
             (head:with-buffer trunk (head:goto! (text:span-start (cdr marked))))
             (lambda ()
               (set! previewed #f)
               (head:with-buffer trunk (head:goto! point)))))))

  ;;; Types -----------------------------------------------------------------------

  (edoc-type revision "an entry of the current buffer's delta log, by its revision"
    (predicate (lambda (v) (and (integer? v) (exact? v) (>= v 0))))
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (let* ([rows (store:log (trunk-id (current-trunk 'revision)))] [context (disablers rows)])
                    (map (lambda (row) (cons (car row) (entry-hint row context))) rows)))))
    (write number->string)
    (preview preview-revision!))

  (edoc-type batch "the edits made together in the current buffer, by their batch label"
    (predicate pair?)
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (let ([rows (store:log (trunk-id (current-trunk 'batch)))])
                    (let loop ([rest rows] [seen '()] [out '()])
                      (cond
                        [(null? rest) (reverse out)]
                        [(let ([id (batch-of (car rest))]) (and id (not (member id seen)) id))
                         => (lambda (id)
                              (let ([n (length (filter (lambda (row) (equal? (batch-of row) id)) rows))])
                                (loop (cdr rest) (cons id seen)
                                      (cons (cons id (format "~a entr~a by ~a" n (if (= n 1) "y" "ies") (spell (cadr (car rest))))) out))))]
                        [else (loop (cdr rest) seen out)]))))))
    (write (lambda (v) (format "'~s" v))))

  ;;; Reload conflicts --------------------------------------------------------------

  ;; A pending conflict of the trunk, as the store lists them: (revision actor
  ;; labels region mine disk). The disk's side stands in the text; a flip
  ;; shows the entry's side in place, in a local buffer where the trunk was.
  (define flip #f) ; (buffer region trunk window) while a flip shows

  (define (conflict-at trunk revision)
    (or (find (lambda (c) (= (car c) revision)) (store:conflicts (trunk-id trunk)))
        (error 'delta-log "no pending conflict at that revision" revision)))

  (define (conflict-hint c . width)
    ;; a conflict in one line, its revision apart: the actor, where the
    ;; disk's side stands, and both sides, elided to a width when one is given
    (let* ([start (text:span-start (text:datum->span (cadddr c)))]
           [side (lambda (lines)
                   (let ([s (string:join lines "\n")])
                     (spell (if (pair? width) (string:elide s (car width)) s))))])
      (format "~a  ~a:~a  mine ~a · disk ~a" (spell (cadr c)) (car start) (cdr start)
              (side (list-ref c 4)) (side (list-ref c 5)))))

  (define (conflict-text c . width) (format "~a  ~a" (car c) (apply conflict-hint c width)))

  (define (unflip!)
    (when flip
      (let ([vb (car flip)] [trunk (caddr flip)] [w (cadddr flip)])
        (set! flip #f)
        (when (eq? (head:window-buffer w) vb) (head:set-window-buffer! w trunk))
        (head:forget-buffer! vb))))

  (define (show-flip! trunk c)
    ;; the entry's side of a conflict shown in place of the trunk, read-only
    ;; and highlighted, in the window that showed the trunk; the thunk
    ;; returned puts the trunk back
    (unflip!)
    (let-values ([(text delta) (text:apply-edit (head:buffer-lines trunk) (text:datum->span (cadddr c)) (list-ref c 4))])
      (let* ([region (let ([s (text:span-start (text:delta-span delta))] [e (text:delta-new-end delta)])
                       (text:make-span (car s) (cdr s) (car e) (cdr e)))]
             [vb (head:fresh-buffer! (string-append "<flip: " (head:buffer-name trunk) ">"))]
             [w (or (find (lambda (w) (eq? (head:window-buffer w) trunk)) (head:windows)) (head:current-window))])
        (head:buffer-fact-set! vb 'mode (head:buffer-fact trunk 'mode #f))
        (head:buffer-lines-set! vb text)
        (head:buffer-read-only-set! vb #t)
        (head:add-buffer! vb)
        (head:set-window-buffer! w vb)
        (head:with-buffer vb (head:goto! (text:span-start region)))
        (set! flip (list vb region trunk w))
        unflip!)))

  (define (preview-conflict! revision)
    ;; the entry's side flipped into place while a prompt has the conflict
    (let ([trunk (guard (ex [else #f]) (current-trunk 'conflict))])
      (and trunk (integer? revision)
           (let ([c (find (lambda (c) (= (car c) revision)) (store:conflicts (trunk-id trunk)))])
             (and c (show-flip! trunk c))))))

  (edoc-type conflict "a pending conflict of the current buffer's last reload, by the revision of the entry the disk contradicted"
    (predicate (lambda (v) (and (integer? v) (exact? v) (>= v 1))))
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (map (lambda (c) (cons (car c) (conflict-hint c 20))) (store:conflicts (trunk-id (current-trunk 'conflict)))))))
    (write number->string)
    (preview preview-conflict!))

  (edoc "The current buffer's pending reload conflicts as data, newest first, (revision actor labels region mine disk) each: the disabled entry's revision, actor and labels, the region the disk's side occupies, the entry's lines and the disk's."
        (returns list))
  (define (delta-log-conflicts)
    (store:conflicts (trunk-id (current-trunk 'delta-log:conflicts))))

  (edoc "Settle a reload conflict: disk keeps the disk's side and drops the pending mark, mine writes the entry's side over the disk's region, replacement lines write those; a write is one undoable edit."
        (conflict conflict "the conflict")
        (choice (or (one-of disk mine) (list-of string)) "disk, mine or the replacement lines")
        (returns symbol "applied, refused or nothing"))
  (define (delta-log-resolve! conflict choice)
    (let* ([trunk (current-trunk 'delta-log:resolve!)] [revision (edoc:type-value 'conflict conflict)])
      (unflip!)
      (let-values ([(status detail) (head:store-resolve! trunk revision choice)])
        (edit:set-message!
          (case status
            [(applied)
             (let ([left (length (store:conflicts (trunk-id trunk)))])
               (format "Conflict ~a settled, ~a; ~a pending" revision
                       (cond [(eq? choice 'disk) "the disk's side kept"] [(eq? choice 'mine) "your side written"] [else "your lines written"])
                       left))]
            [else (format "Conflict ~a: ~a ~s" revision status detail)]))
        (refresh-browser!)
        status)))

  (edoc "Settle every pending reload conflict of the current buffer the same way."
        (choice (one-of disk mine) "disk or mine")
        (returns integer "how many were settled"))
  (define (delta-log-resolve-all! choice)
    (let ([trunk (current-trunk 'delta-log:resolve-all!)])
      (unflip!)
      (let ([settled (fold-left (lambda (n c)
                                  (let-values ([(status detail) (head:store-resolve! trunk (car c) choice)])
                                    (if (eq? status 'applied) (+ n 1) n)))
                                0 (store:conflicts (trunk-id trunk)))])
        (edit:set-message! (format "~a conflict~a settled, the ~a side kept" settled (if (= settled 1) "" "s") choice))
        (refresh-browser!)
        settled)))

  (edoc "Show the other side of a reload conflict in place: the entry's lines over the disk's region, in a read-only local buffer where the buffer was; shown already, the buffer returns."
        (conflict conflict "the conflict"))
  (define (delta-log-flip! conflict)
    (let* ([trunk (current-trunk 'delta-log:flip!)] [revision (edoc:type-value 'conflict conflict)])
      (if flip (unflip!) (show-flip! trunk (conflict-at trunk revision)))))

  ;;; The view --------------------------------------------------------------------

  (define (start-view! trunk)
    ;; a fresh view of the trunk in a local tool buffer sharing its mode; a
    ;; view of another buffer ends first, this head having one at a time
    (when the-view (drop-view! the-view))
    (let ([vb (head:fresh-buffer! (string-append "<view: " (head:buffer-name trunk) ">"))])
      (head:buffer-fact-set! vb 'mode (head:buffer-fact trunk 'mode #f))
      (set! the-view (make-view trunk vb '() '()))
      the-view))

  (define (render-view! v)
    ;; the view's text in its buffer, shown where the trunk was, point
    ;; carried from the trunk through the view's mapping
    (let ([trunk (view-trunk v)] [vb (view-buffer v)])
      (let-values ([(text mapping conflicts) (store:view (trunk-id trunk) (view-disabled v))])
        (view-conflicts-set! v conflicts)
        (let ([point (fold-left (lambda (p d) (text:rebase-position p (text:datum->delta d)))
                                (head:buffer-point trunk) mapping)]
              [w (or (find (lambda (w) (memq (head:window-buffer w) (list vb trunk))) (head:windows))
                     (head:current-window))])
          (head:buffer-read-only-set! vb #f)
          (head:buffer-lines-set! vb text)
          (head:buffer-read-only-set! vb #t)
          (head:add-buffer! vb)
          (unless (eq? (head:window-buffer w) vb) (head:set-window-buffer! w vb))
          (head:with-buffer vb (head:goto! point))))))

  (define (drop-view! v)
    ;; the windows showing the view return to the trunk; the view buffer is retired
    (let ([trunk (view-trunk v)] [vb (view-buffer v)])
      (for-each (lambda (w) (when (eq? (head:window-buffer w) vb) (head:set-window-buffer! w trunk))) (head:windows))
      (head:forget-buffer! vb)
      (set! the-view #f)))

  (define (report! v)
    (let ([n (length (view-disabled v))] [k (length (view-conflicts v))])
      (edit:set-message!
        (string-append
          (format "View of ~a: ~a entr~a disabled" (head:buffer-name (view-trunk v)) n (if (= n 1) "y" "ies"))
          (if (= k 0) ""
              (format "; ~a conflict~a, a later entry over a disabled one: ~a" k (if (= k 1) "" "s")
                      (string:join (map (lambda (c) (format "~a over ~a" (cdr c) (car c))) (view-conflicts v)) ", ")))))))

  ;;; Commands --------------------------------------------------------------------

  (edoc "The current buffer's delta log as data, newest first, (revision actor labels delta origin state) each, the log behind its view when a view is current; a selector narrows it by count, actor, batch, since, until or state, and a batch given alone selects its entries."
        (selector (list-of (or list batch)) "the selector or a batch, at most one")
        (returns list))
  (define (delta-log-entries . selector)
    (apply store:log (trunk-id (current-trunk 'delta-log:log)) (map selector-of selector)))

  (edoc "Toggle entries in the view of the current buffer: a revision disabled in the view is enabled again, any other joins the disabled; the view's text, the rest rebased over their absence, shows in the window at once as a local buffer, and with nothing disabled the view ends."
        (revisions (list-of revision) "the entries to toggle")
        (returns list "the revisions disabled"))
  (define (delta-log-toggle! . revisions)
    (let* ([trunk (current-trunk 'delta-log:toggle!)]
           [v (if (and the-view (eq? (view-trunk the-view) trunk)) the-view (start-view! trunk))])
      (for-each
        (lambda (r)
          (let ([r (edoc:type-value 'revision r)])
            (view-disabled-set! v (if (memv r (view-disabled v)) (remv r (view-disabled v)) (cons r (view-disabled v))))))
        revisions)
      (cond
        [(null? (view-disabled v)) (drop-view! v) (edit:set-message! "View ended: nothing disabled") (refresh-browser!) '()]
        [else (render-view! v) (report! v) (refresh-browser!) (view-disabled v)])))

  (edoc "Commit the view: the trunk rewritten for everyone with the view's entries disabled, their inverses this head's own undoable action, and the window back on the trunk; blocked when a later entry overlaps a disabled one, the conflicts named."
        (returns symbol "applied, blocked, refused or nothing"))
  (define (delta-log-commit!)
    (let ([v (or the-view (error 'delta-log:commit! "no view to commit"))])
      (let-values ([(status detail) (head:store-rewrite! (view-trunk v) (view-disabled v))])
        (case status
          [(applied)
           (let ([n (length (view-disabled v))] [name (head:buffer-name (view-trunk v))])
             (drop-view! v)
             (edit:set-message! (format "Rewrote ~a: ~a entr~a disabled, now revision ~a" name n (if (= n 1) "y" "ies") detail))
             (refresh-browser!))]
          [(blocked) (edit:set-message! (format "Rewrite blocked, a later entry over a disabled one: ~s" detail))]
          [else (edit:set-message! (format "Rewrite ~a: ~s" status detail))])
        status)))

  (edoc "Abandon the view: the window shows the trunk again, nothing rewritten.")
  (define (delta-log-revert!)
    (unflip!)
    (when the-view
      (let ([name (head:buffer-name (view-trunk the-view))])
        (drop-view! the-view)
        (edit:set-message! (format "View of ~a abandoned" name))
        (refresh-browser!))))

  (edoc "The live view as data, (trunk-name disabled conflicts), or #f without one."
        (returns (or list #f)))
  (define (delta-log-view)
    (and the-view (list (head:buffer-name (view-trunk the-view)) (view-disabled the-view) (view-conflicts the-view))))

  (edoc "The revisions the live view disables, newest first; () without a view."
        (returns (list-of integer)))
  (define (delta-log-disabled)
    (if the-view (list-sort > (view-disabled the-view)) '()))

  (edoc "Describe an entry of the current buffer's log in the echo area: its actor, where it wrote, what it removed and inserted."
        (revision revision "the entry"))
  (define (delta-log-show! revision)
    (let* ([trunk (current-trunk 'delta-log:show!)] [rows (store:log (trunk-id trunk))]
           [revision (edoc:type-value 'revision revision)])
      (edit:set-message!
        (entry-text (or (find (lambda (row) (= (car row) revision)) rows) (error 'delta-log:show! "no entry at that revision" revision))
                    (disablers rows)))))

  ;;; The browser ------------------------------------------------------------------

  ;; The <delta-log> app: one row per entry of a trunk's log, newest first,
  ;; an entry disabled in the view marked, or one row per pending reload
  ;; conflict; the current row's span highlighted in the trunk's window
  ;; while the browser is on screen.
  (define browser #f) ; the app buffer, while live
  (define browser-trunk #f) ; the shared buffer whose log it shows
  (define browser-mode 'entries) ; entries, or conflicts while the pending conflicts are the rows
  (define browser-rows '()) ; the entries or conflicts listed, one per line
  (define browser-filter '()) ; the selector narrowing the entries
  (define browser-disablers '()) ; (revision inverse kind) triples of the whole log, for the rows
  (define browsed-key #f) ; (mode revision trunk-revision) the browsed span was computed for
  (define browsed-origin #f) ; (trunk . point) where point stood when the browser opened, for C-g

  (define (browser-live?) (and browser (memq browser (head:buffers)) #t))

  (define (row-text row)
    ;; a browser line: a conflict with both sides, or the mark of an entry
    ;; disabled in the view, then the entry
    (if (eq? browser-mode 'conflicts)
        (string-append "  " (conflict-text row 20))
        (string-append (if (and the-view (memv (car row) (view-disabled the-view))) "- " "  ") (entry-text row browser-disablers))))

  (define (current-row)
    (let ([i (car (head:buffer-point browser))])
      (and (< i (length browser-rows)) (list-ref browser-rows i))))

  (define (browser-rows-now)
    ;; the rows the mode lists, the conflicts pending or the entries the
    ;; filter selects; none where the trunk is gone
    (guard (ex [else '()])
      (let ([id (trunk-id browser-trunk)])
        (if (eq? browser-mode 'conflicts) (store:conflicts id)
            (let ([all (store:log id)])
              (set! browser-disablers (disablers all))
              (if (null? browser-filter) all (store:log id browser-filter)))))))

  (define (refresh-browser!)
    (when (browser-live?)
      (let ([rows (browser-rows-now)])
        (cond
          [(and (eq? browser-mode 'conflicts) (null? rows))
           ;; the last conflict settled, the rows return to the log
           (set! browser-mode 'entries)
           (head:with-buffer browser (head:goto! '(0 . 0)))
           (refresh-browser!)]
          [else
           (set! browser-rows rows)
           (head:view-replace! browser (if (null? rows) (list "no entries") (map row-text rows)))
           (sync-browsed!)]))))

  (define (browsed-span row)
    ;; the span a row marks in the trunk: an entry's span rebased into the
    ;; current text, or the region a conflict's disk side occupies
    (if (eq? browser-mode 'conflicts)
        (cons browser-trunk (text:datum->span (cadddr row)))
        (span-of browser-trunk (car row))))

  (define (sync-browsed!)
    ;; the current row's span for the highlighter, recomputed when the row,
    ;; the mode or the trunk's revision changes, none while the browser is
    ;; off screen
    (let* ([row (and (browser-live?) browser-trunk
                     (exists (lambda (w) (eq? (head:window-buffer w) browser)) (head:windows))
                     (current-row))]
           [key (and row (list browser-mode (car row) (head:buffer-store-rev browser-trunk)))])
      (unless (equal? key browsed-key)
        (set! browsed-key key)
        (set! browsed (and row (guard (ex [else #f]) (browsed-span row)))))))

  (define (follow-row!)
    ;; the trunk's point on the browsed span, so its window scrolls to
    ;; show the highlighted text as a search's does to its match
    (when (and browsed browser-trunk (memq browser-trunk (head:buffers)))
      (head:with-buffer browser-trunk (head:goto! (text:span-start (cdr browsed))))))

  (define (restore-origin!)
    ;; point back where it stood in the trunk when the browser opened
    (when (and browsed-origin (memq (car browsed-origin) (head:buffers)))
      (head:with-buffer (car browsed-origin) (head:goto! (cdr browsed-origin)))))

  (define (move-row! delta)
    (when (pair? browser-rows)
      (let* ([at (car (head:buffer-point browser))]
             [row (min (max 0 (+ at delta)) (- (length browser-rows) 1))])
        (unless (= row at)
          ;; a flip shown for the row left ends with it
          (when (eq? browser-mode 'conflicts) (unflip!))
          (head:with-buffer browser (head:goto! (cons row 0))))
        (sync-browsed!)
        (follow-row!))))

  (define (browser-status b)
    (let ([n (length browser-rows)] [i (+ 1 (car (head:buffer-point b)))])
      (if (eq? browser-mode 'conflicts)
          (format "delta log conflicts ~a of ~a" (min i n) n)
          (format "delta log ~a of ~a~a" (min i n) n (if (null? browser-filter) "" (format "  ~s" browser-filter))))))

  ;;; The browser's keys ---------------------------------------------------------
  ;;
  ;; Commands over the current row, bound in the browser's contexts: the
  ;; delta-log mode's while the browser is on screen, delta-log-entries
  ;; over the entries and delta-log-conflicts over the conflicts, so
  ;; C-x TAB lists what works there, and C-h k and M-x reach them.
  (define (browser-row who)
    (unless (browser-live?) (error who "the delta log browser is not open"))
    (or (current-row) (error who "no row under point")))

  (define (conflict-row who)
    (let ([row (browser-row who)])
      (unless (eq? browser-mode 'conflicts) (error who "the rows are entries; delta-log:conflicts! lists the conflicts"))
      row))

  (define (entries-browser? b) (and (browser-live?) (eq? b browser) (eq? browser-mode 'entries)))

  (define (conflicts-browser? b) (and (browser-live?) (eq? b browser) (eq? browser-mode 'conflicts)))

  (edoc "Describe the browser's current row in the echo area: the entry's actor, where it wrote, what it removed and inserted, or both sides of the conflict in full.")
  (define (delta-log-show-row!)
    (let ([row (browser-row 'delta-log:show-row!)])
      (edit:set-message! (if (eq? browser-mode 'conflicts) (conflict-text row) (entry-text row browser-disablers)))))

  (edoc "Close the browser and put point back where it stood in the buffer when the browser opened.")
  (define (delta-log-cancel!)
    (restore-origin!)
    (delta-log-close!))

  (edoc "Toggle the browser's current row's entry in the view, as delta-log:toggle! does with its revision.")
  (define (delta-log-toggle-row!)
    (let ([row (browser-row 'delta-log:toggle-row!)])
      (when (eq? browser-mode 'conflicts) (error 'delta-log:toggle-row! "the rows are conflicts; delta-log:open! lists the entries"))
      (delta-log-toggle! (car row))))

  (edoc "Show the other side of the browser's current row's conflict in place, or put the disk's side back, as delta-log:flip! does with its revision.")
  (define (delta-log-flip-row!)
    (delta-log-flip! (car (conflict-row 'delta-log:flip-row!))))

  (edoc "Settle the browser's current row's conflict keeping the disk's side, as delta-log:resolve! does with disk.")
  (define (delta-log-keep-disk!)
    (delta-log-resolve! (car (conflict-row 'delta-log:keep-disk!)) 'disk))

  (edoc "Settle the browser's current row's conflict writing the entry's side over the disk's region, as delta-log:resolve! does with mine.")
  (define (delta-log-keep-mine!)
    (delta-log-resolve! (car (conflict-row 'delta-log:keep-mine!)) 'mine))

  (define (ensure-browser!)
    (unless (browser-live?)
      (set! browser (head:register-app! "<delta-log>" refresh-browser!))
      (head:buffer-fact-set! browser 'recency 'behind)
      (head:set-app-presentation! browser 0 'auto #f)
      (head:set-app-selectable! browser #f)
      (head:set-app-status-position! browser browser-status)
      (mode:choose! "delta-log" browser)))

  (define (open-browser! trunk mode selector)
    ;; the browser over the trunk in a mode, shown beside it and selected,
    ;; its first row current
    (unless (and browsed-origin (eq? (car browsed-origin) trunk) (browser-live?))
      (set! browsed-origin (cons trunk (head:buffer-point trunk))))
    (set! browser-trunk trunk)
    (set! browser-mode mode)
    (set! browser-filter selector)
    (ensure-browser!)
    (refresh-browser!)
    (let ([w (or (window:companion! browser) (window:display! browser))])
      (when w (window:focus! w)))
    (head:with-buffer browser (head:goto! '(0 . 0)))
    (sync-browsed!)
    (follow-row!))

  (edoc "Open the delta log browser for the current buffer in the companion window below it, the window a split below made, else a fresh split, and select it: one row per entry, newest first, an entry disabled in the view marked, the current row's text highlighted in the buffer's window and point on it, so the window follows; M-n and M-p move, M-t toggles the row's entry in the view, RET describes it, M-RET commits the view, ESC closes the browser leaving point on the row's text, C-g closes it and puts point back. A selector narrows the rows as for delta-log:log, a batch given alone to its entries, a replacement's occurrences say."
        (selector (list-of (or list batch)) "the selector or a batch, at most one"))
  (define (delta-log-open! . selector)
    (open-browser! (current-trunk 'delta-log:open!) 'entries (if (pair? selector) (selector-of (car selector)) '())))

  (edoc "Show the current buffer's pending reload conflicts in the browser, in the companion window below the buffer, and select it: one row each, newest first, the entry's revision and actor, where the disk's side stands and both sides elided, the current row's region highlighted in the buffer's window and point on it, so the window follows; M-n and M-p move, M-/ shows the entry's side in place and back, M-d keeps the disk's side, M-m writes the entry's, RET describes the row in full, ESC closes the browser leaving point on the region, C-g closes it and puts point back. With the last conflict settled the rows return to the log; without any, the log shows and the echo says so."
        (returns integer "how many conflicts pend"))
  (define (delta-log-conflicts!)
    (let* ([trunk (current-trunk 'delta-log:conflicts!)] [n (length (store:conflicts (trunk-id trunk)))])
      (open-browser! trunk 'conflicts '())
      (when (zero? n) (edit:set-message! (format "No conflicts pending in ~a" (head:buffer-name trunk))))
      n))

  (edoc "Narrow the browser's rows to the entries a selector picks, as for delta-log:log, or to a batch's; #f shows every entry again."
        (selector (or list batch #f) "the selector, a batch or #f"))
  (define (delta-log-filter! selector)
    (set! browser-filter (if selector (selector-of selector) '()))
    (set! browser-mode 'entries)
    (refresh-browser!))

  (edoc "Move the browser to the next row, its text highlighted in the buffer's window and point on it.")
  (define (delta-log-next!) (when (browser-live?) (move-row! 1)))

  (edoc "Move the browser to the previous row, its text highlighted in the buffer's window and point on it.")
  (define (delta-log-previous!) (when (browser-live?) (move-row! -1)))

  (edoc "Close the delta log browser: its window shows another buffer, a flip it showed ends, the buffer's window is selected again, and point stays where the last row put it.")
  (define (delta-log-close!)
    (when (browser-live?)
      (unflip!)
      (set! browsed #f) (set! browsed-key #f) (set! browsed-origin #f)
      (let ([w (and (eq? (head:current-buffer) browser)
                    (find (lambda (w) (eq? (head:window-buffer w) browser-trunk)) (head:windows)))])
        (head:forget-buffer! browser)
        (set! browser #f)
        (when (and w (memq w (head:windows))) (window:focus! w)))))

  ;; the browser's keys: what both kinds of row allow in its mode's context,
  ;; the rest in the state context of the rows shown
  (define browser-keys
    `((("M-n" "DOWN" "C-n") ,delta-log-next!) (("M-p" "UP" "C-p") ,delta-log-previous!)
      (("RET") ,delta-log-show-row!) (("ESC") ,delta-log-close!) (("C-g") ,delta-log-cancel!)))

  (define entries-keys `((("M-t") ,delta-log-toggle-row!) (("M-RET") ,delta-log-commit!)))

  (define conflicts-keys `((("M-/") ,delta-log-flip-row!) (("M-d") ,delta-log-keep-disk!) (("M-m") ,delta-log-keep-mine!)))

  (define (bind-keys! context table)
    (for-each (lambda (entry) (for-each (lambda (key) (keymap:bind-default! context key (cadr entry))) (car entry))) table))

  (edoc "Install the delta log: the browser's mode with its keys bound in the delta-log context, the entries' and the conflicts' keys in their state contexts, the highlighter marking a previewed or browsed entry's span, a browsed conflict's region and a flip's, and the browser's highlight following its row before every frame; C-x TAB lists the keys.")
  (define (init!)
    (mode:register! "delta-log" '() '() (lambda (line) #f))
    (mode:add-context! 'delta-log-entries entries-browser?)
    (mode:add-context! 'delta-log-conflicts conflicts-browser?)
    (bind-keys! 'delta-log browser-keys)
    (bind-keys! 'delta-log-entries entries-keys)
    (bind-keys! 'delta-log-conflicts conflicts-keys)
    (paint:add-highlighter! entry-highlights)
    (head:add-pre-redraw-hook! sync-browsed!)))
