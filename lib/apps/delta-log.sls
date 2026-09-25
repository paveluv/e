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
          (rename (delta-log-open! open!)) (rename (delta-log-page-down! page-down!)) (rename (delta-log-page-up! page-up!))
          (rename (delta-log-previous! previous!)) (rename (delta-log-resolve! resolve!))
          (rename (delta-log-resolve-all! resolve-all!)) (rename (delta-log-revert! revert!)) (rename (delta-log-show! show!))
          (rename (delta-log-show-row! show-row!)) (rename (delta-log-toggle! toggle!)) (rename (delta-log-toggle-row! toggle-row!))
          (rename (delta-log-view view)))
  (import (rnrs)
          (only (chezscheme) format make-weak-eq-hashtable void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head table) table:)
          (prefix (head window) window:)
          (prefix (state store) store:)
          (prefix (sys glyph) glyph:))

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
          [(browser-of b) => (lambda (br)
                               ;; a browser stands for its current row's buffer, else the first it tracks
                               (let ([row (current-row br)] [tracked (tracked-buffers)])
                                 (cond [row (car row)] [(pair? tracked) (car tracked)] [else b])))]
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
        (follow!)
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
        (follow!)
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
        [(null? (view-disabled v)) (drop-view! v) (edit:set-message! "View ended: nothing disabled") (follow!) '()]
        [else (render-view! v) (report! v) (follow!) (view-disabled v)])))

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
             (follow!))]
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
        (follow!))))

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

  ;;; The browsers -----------------------------------------------------------------

  ;; Two apps over the shared buffers on screen, in the finder's manner: a
  ;; heading row, a tinted current row and no cursor.  <delta-log> lists one
  ;; row per entry of every shared buffer a window shows, newest first
  ;; within a buffer, an entry disabled in the view marked; <conflicts> one
  ;; row per pending reload conflict of them.  A Buffer column tells the
  ;; buffers apart, the current row's text is highlighted in its buffer's
  ;; window, which follows, and the rows follow the windows and the store
  ;; before every frame.  Each opens in the window of the caller's choosing
  ;; and returns the window to what it showed before when closed.
  (define-record-type browser
    (fields name mode table cell (mutable buffer) (mutable rows) (mutable over) (mutable cache)))

  (define log-browser
    (make-browser "<delta-log>" "delta-log" (table:make '#("Buffer" "Rev" "Entry") '#(8 4 24) 2 '(0) '#(text right text))
                  (lambda (row column)
                    (let ([b (car row)] [entry (cdr row)])
                      (case column
                        [(0) (head:buffer-name b)]
                        [(1) (number->string (car entry))]
                        [else (string-append (if (and the-view (eq? (view-trunk the-view) b) (memv (car entry) (view-disabled the-view))) "- " "  ")
                                             (entry-hint entry (disablers-of b)))])))
                  #f '() '() (make-weak-eq-hashtable)))

  (define conflicts-browser
    (make-browser "<conflicts>" "conflicts"
                  (table:make '#("Buffer" "Rev" "Actor" "At" "Mine" "Disk") '#(8 4 8 6 12 12) 1 '(2 3 0) '#(text right text text text text))
                  (lambda (row column)
                    (let ([b (car row)] [c (cdr row)])
                      (define (side lines) (spell (string:elide (string:join lines "\n") 24)))
                      (case column
                        [(0) (head:buffer-name b)]
                        [(1) (number->string (car c))]
                        [(2) (spell (cadr c))]
                        [(3) (let ([start (text:span-start (text:datum->span (cadddr c)))]) (format "~a:~a" (car start) (cdr start)))]
                        [(4) (side (list-ref c 4))]
                        [else (side (list-ref c 5))])))
                  #f '() '() (make-weak-eq-hashtable)))

  (define browsers (list log-browser conflicts-browser))
  (define browser-filter '()) ; the selector narrowing the log's rows

  (define (browser-live? br) (and (browser-buffer br) (memq (browser-buffer br) (head:buffers)) #t))

  (define (browser-of b) (find (lambda (br) (and (browser-buffer br) (eq? (browser-buffer br) b))) browsers))

  (define (browser-windows br)
    (filter (lambda (w) (eq? (head:window-buffer w) (browser-buffer br))) (head:windows)))

  (define (tracked-buffers)
    ;; the shared buffers the windows show, a view or a flip standing for
    ;; its trunk and a browser for the buffer it replaced, each once, in
    ;; window order
    (let loop ([ws (head:windows)] [acc '()])
      (if (null? ws) (reverse acc)
          (let* ([shown (head:window-buffer (car ws))]
                 [br (browser-of shown)]
                 [b (trunk-of (if br (cond [(assq (car ws) (browser-over br)) => cdr] [else shown]) shown))])
            (loop (cdr ws) (if (and (head:buffer-store-id b) (memq b (head:buffers)) (not (memq b acc))) (cons b acc) acc))))))

  (define (disablers-of b)
    ;; the (revision inverse kind) triples of a buffer's whole log, cached with its rows
    (let ([hit (hashtable-ref (browser-cache log-browser) b #f)])
      (if hit (caddr hit) '())))

  (define (log-rows-of b)
    ;; a buffer's log rows and disablers, read again only at a new revision or filter
    (let* ([key (cons (head:buffer-store-rev b) browser-filter)]
           [hit (hashtable-ref (browser-cache log-browser) b #f)])
      (if (and hit (equal? (car hit) key))
          (cadr hit)
          (let* ([all (guard (ex [else '()]) (store:log (trunk-id b)))]
                 [rows (if (null? browser-filter) all (guard (ex [else '()]) (store:log (trunk-id b) browser-filter)))])
            (hashtable-set! (browser-cache log-browser) b (list key rows (disablers all)))
            rows))))

  (define (rows-now br)
    ;; the rows the browser lists, (buffer . datum) each, in window order
    (apply append
      (map (lambda (b)
             (if (eq? br log-browser)
                 (map (lambda (row) (cons b row)) (log-rows-of b))
                 (if (head:buffer-conflicted b)
                     (map (lambda (c) (cons b c)) (guard (ex [else '()]) (store:conflicts (trunk-id b))))
                     '())))
           (tracked-buffers))))

  (define (row-key row) (cons (car row) (car (cdr row))))

  (define (browser-width br)
    ;; the narrowest window showing the browser, a cell short of its edge;
    ;; the screen's width while the windows are not tiled yet and report no
    ;; width to speak of
    (let* ([ws (browser-windows br)]
           [narrowest (if (null? ws) 0 (apply min (map head:window-content-width ws)))])
      (- (if (> narrowest 40) narrowest (paint:screen-cols)) 1)))

  (define (rendered-lines br rows)
    ;; the heading and the rows, or a word for none: the conflicts' columns
    ;; fitted to the browser's width, the log's Entry column left whole after
    ;; its Buffer and Rev columns, since the entry's tail names its batch and
    ;; what undid it
    (cond
      [(null? rows) (list (car (rendered-lines br (list #f))) (if (eq? br log-browser) "No entries" "No conflicts pending"))]
      [(eq? br log-browser)
       (let* ([cell (browser-cell br)]
              [names (map (lambda (row) (if row (cell row 0) "Buffer")) rows)]
              [revisions (map (lambda (row) (if row (cell row 1) "Rev")) rows)]
              [name-width (apply max 6 (map glyph:cells names))]
              [revision-width (apply max 3 (map string-length revisions))]
              [line (lambda (name revision entry)
                      (string-append (glyph:fit name name-width) "  "
                                     (make-string (- revision-width (string-length revision)) #\space) revision "  " entry))])
         (cons (line "Buffer" "Rev" "Entry")
               (map (lambda (row name revision) (if row (line name revision (cell row 2)) "")) rows names revisions)))]
      [else
       (let-values ([(line cols) (table:layout (browser-table br) '() (filter values rows) (browser-cell br) (max 20 (browser-width br)))])
         (cons (line #f) (map (lambda (row) (if row (line row) "")) rows)))]))

  (define (refresh-browser! br)
    ;; the rows from the windows and the store, rendered again when their
    ;; text changed, a toggle's mark say, the current row kept by its buffer
    ;; and revision when the rows shift, and the highlight synced
    (when (browser-live? br)
      (let* ([b (browser-buffer br)] [rows (rows-now br)] [old (browser-rows br)] [lines (rendered-lines br rows)])
        (unless (equal? lines (vector->list (head:buffer-lines b)))
          (let ([keys (map (lambda (w) (cons w (let ([row (current-row-in br w)]) (and row (row-key row))))) (browser-windows br))])
            (browser-rows-set! br rows)
            (head:view-replace! b lines)
            (for-each (lambda (entry)
                        (let ([at (and (cdr entry) (list-index (lambda (row) (equal? (row-key row) (cdr entry))) rows))])
                          (head:window-prow-set! (car entry) (if at (+ at 1) (min 1 (length rows))))
                          (head:window-pcol-set! (car entry) 0)))
                      keys)))
        (browser-rows-set! br rows)
        (sync-browsed!))))

  (define (list-index pred lst)
    (let loop ([lst lst] [i 0]) (cond [(null? lst) #f] [(pred (car lst)) i] [else (loop (cdr lst) (+ i 1))])))

  (define (current-row-in br w)
    ;; the row under the window's point, or #f on the heading or past the end
    (let ([i (- (head:window-prow w) 1)] [rows (browser-rows br)])
      (and (>= i 0) (< i (length rows)) (list-ref rows i))))

  (define (browser-window br)
    ;; the window the browser's current row is read from: the selected one
    ;; when it shows the browser, else the first showing it
    (let ([ws (browser-windows br)])
      (cond [(null? ws) #f] [(memq (head:current-window) ws) (head:current-window)] [else (car ws)])))

  (define (current-row br)
    (let ([w (browser-window br)]) (and w (current-row-in br w))))

  (define (current-browser who)
    (or (browser-of (head:current-buffer)) (error who "the current buffer is no delta log browser")))

  (define (browser-row who)
    (let ([br (current-browser who)])
      (or (current-row br) (error who "no row under point"))))

  (define (browsed-span row browser)
    ;; the span a row marks in its buffer: an entry's span rebased into the
    ;; text, or the region a conflict's disk side occupies
    (if (eq? browser conflicts-browser)
        (cons (car row) (text:datum->span (cadddr (cdr row))))
        (span-of (car row) (car (cdr row)))))

  (define browsed-key #f) ; (browser buffer revision trunk-revision) the browsed span was computed for

  (define (sync-browsed!)
    ;; the selected browser's current row highlighted in its buffer, recomputed
    ;; when the row or the buffer's revision changes; none while no browser shows
    (let* ([br (or (browser-of (head:current-buffer)) (find (lambda (br) (pair? (browser-windows br))) browsers))]
           [row (and br (browser-live? br) (current-row br))]
           [key (and row (list br (car row) (car (cdr row)) (head:buffer-store-rev (car row))))])
      (unless (equal? key browsed-key)
        (set! browsed-key key)
        (set! browsed (and row (guard (ex [else #f]) (browsed-span row br)))))))

  (define origins '()) ; ((buffer . point) ...) where point stood before a browser first moved it

  (define (follow-row!)
    ;; the row's buffer's point on the browsed span, so its window scrolls
    ;; to the highlighted text as a search's does to its match; where point
    ;; stood before the first move is kept for C-g
    (when (and browsed (memq (car browsed) (head:buffers)))
      (unless (assq (car browsed) origins)
        (set! origins (cons (cons (car browsed) (head:buffer-point (car browsed))) origins)))
      (head:with-buffer (car browsed) (head:goto! (text:span-start (cdr browsed))))))

  (define (follow!)
    ;; before every frame: the rows follow the windows and the store
    (for-each refresh-browser! browsers))

  (define (move-row! delta)
    (let* ([br (current-browser 'delta-log:next!)] [w (head:current-window)] [n (length (browser-rows br))])
      (when (> n 0)
        (let* ([at (head:window-prow w)]
               [row (min (max 1 (+ at delta)) n)])
          (unless (= row at)
            (when (eq? br conflicts-browser) (unflip!))
            (head:goto! (cons row 0)))
          (sync-browsed!)
          (follow-row!)))))

  (define (browser-status b)
    (let* ([br (browser-of b)] [n (length (browser-rows br))] [w (browser-window br)]
           [i (if w (head:window-prow w) 0)])
      (if (eq? br conflicts-browser)
          (format "conflicts  ~a of ~a" (min i n) n)
          (format "delta log  ~a of ~a~a" (min i n) n (if (null? browser-filter) "" (format "  ~s" browser-filter))))))

  (define (styles b row line)
    (make-vector (string-length line) (if (zero? row) 'header 'plain)))

  (define (ensure-browser! br)
    (unless (browser-live? br)
      (let ([b (head:register-app! (browser-name br) (lambda () (refresh-browser! br)))])
        (browser-buffer-set! br b)
        (browser-rows-set! br '())
        (browser-over-set! br '())
        (head:buffer-fact-set! b 'recency 'behind)
        (head:set-app-presentation! b 1 'auto #f)
        (head:set-app-cursor-visible! b #f)
        (head:set-app-selectable! b #f)
        (head:set-app-status-position! b browser-status)
        (mode:choose! (browser-mode br) b))))

  (define (target-window w*)
    (if (pair? w*) (edoc:type-value 'window (car w*)) (head:current-window)))

  (define (show-browser! br w)
    ;; the browser in a window, remembering what it showed, the pop-up
    ;; shown when it is the window, and selected; the first row current
    (ensure-browser! br)
    (let ([b (browser-buffer br)])
      (unless (eq? (head:window-buffer w) b)
        (browser-over-set! br (cons (cons w (head:window-buffer w)) (remp (lambda (e) (eq? (car e) w)) (browser-over br))))
        (head:set-window-buffer! w b))
      (when (and (head:popup? w) (= (head:popup-rows) 0)) (head:show-popup! (head:popup-default-rows)))
      (window:focus! w)
      (browser-rows-set! br '())
      (refresh-browser! br)
      (head:goto! (cons (min 1 (length (browser-rows br))) 0))
      (sync-browsed!)
      (follow-row!)))

  (define (close-browser! br)
    ;; the windows showing the browser show what they showed before, the
    ;; pop-up hiding again when it showed its placeholder, and the app buffer goes
    (when (browser-live? br)
      (let ([b (browser-buffer br)])
        (when (eq? br conflicts-browser) (unflip!))
        (for-each
          (lambda (w)
            (let ([back (cond [(assq w (browser-over br)) => cdr] [else #f])])
              (cond
                [(and (head:popup? w) (or (not back) (not (memq back (head:buffers))) (eq? back (head:window-buffer (head:popup)))))
                 (head:hide-popup!)]
                [(and back (memq back (head:buffers))) (head:set-window-buffer! w back)]
                [else (window:display! (or (find (lambda (o) (not (eq? o b))) (head:buffers)) b))])))
          (browser-windows br))
        (head:forget-buffer! b)
        (browser-buffer-set! br #f)
        (browser-rows-set! br '())
        (browser-over-set! br '())
        (set! origins '())
        (set! browsed #f) (set! browsed-key #f))))

  ;;; The browsers' commands ---------------------------------------------------------

  (edoc "Open the delta log browser, <delta-log>, in a window, the current one by default, and select it: one row per entry of every shared buffer a window shows, newest first within a buffer, its buffer, revision, actor, place, removed and inserted text, batch and what an inverse reverts, an entry disabled in the view marked with -, the current row's text highlighted in its buffer's window, which follows; delta-log:filter! narrows the rows. ESC closes it, the window showing what it showed before."
        (w* (list-of window) "the window to open it in, at most one; the current window by default"))
  (define (delta-log-open! . w*)
    (show-browser! log-browser (target-window w*)))

  (edoc "Open the conflicts browser, <conflicts>, in a window, the current one by default, and select it: one row per pending reload conflict of every shared buffer a window shows, its buffer, the entry's revision and actor, where the disk's side stands and both sides, the current row's region highlighted in its buffer's window, which follows; LEFT keeps mine, RIGHT keeps the disk's side, SPC shows the other side in place and back. ESC closes it, the window showing what it showed before."
        (w* (list-of window) "the window to open it in, at most one; the current window by default"))
  (define (delta-log-conflicts! . w*)
    (show-browser! conflicts-browser (target-window w*)))

  (edoc "Narrow the delta log browser's rows to the entries a selector picks, as for delta-log:log, or to a batch's; #f shows every entry again."
        (selector (or list batch #f) "the selector, a batch or #f"))
  (define (delta-log-filter! selector)
    (set! browser-filter (if selector (selector-of selector) '()))
    (hashtable-clear! (browser-cache log-browser))
    (refresh-browser! log-browser))

  (edoc "Move the browser to the next row, its text highlighted in its buffer's window and point on it.")
  (define (delta-log-next!) (move-row! 1))

  (edoc "Move the browser to the previous row, its text highlighted in its buffer's window and point on it.")
  (define (delta-log-previous!) (move-row! -1))

  (edoc "Move the browser a page of rows down.")
  (define (delta-log-page-down!) (move-row! (max 1 (- (head:window-size (head:current-window)) 1))))

  (edoc "Move the browser a page of rows up.")
  (define (delta-log-page-up!) (move-row! (- (max 1 (- (head:window-size (head:current-window)) 1)))))

  (edoc "Describe the browser's current row in the echo area: the entry's actor, where it wrote, what it removed and inserted, or both sides of the conflict in full.")
  (define (delta-log-show-row!)
    (let* ([br (current-browser 'delta-log:show-row!)] [row (browser-row 'delta-log:show-row!)])
      (edit:set-message!
        (string-append (head:buffer-name (car row)) "  "
                       (if (eq? br conflicts-browser) (conflict-text (cdr row)) (entry-text (cdr row) (disablers-of (car row))))))))

  (edoc "Close the delta log or conflicts browser the current buffer is: its windows show what they showed before, the pop-up hides when it showed nothing else, a flip it showed ends, and point stays where the last row put it.")
  (define (delta-log-close!)
    (close-browser! (current-browser 'delta-log:close!)))

  (edoc "Close the browser the current buffer is, putting point back where it stood in the row's buffer when the browser last moved it.")
  (define (delta-log-cancel!)
    (let ([br (current-browser 'delta-log:cancel!)])
      (for-each (lambda (origin)
                  (when (memq (car origin) (head:buffers))
                    (head:with-buffer (car origin) (head:goto! (cdr origin)))))
                origins)
      (close-browser! br)))

  (edoc "Toggle the browser's current row's entry in its buffer's view, as delta-log:toggle! does with its revision.")
  (define (delta-log-toggle-row!)
    (let ([row (browser-row 'delta-log:toggle-row!)])
      (unless (eq? (current-browser 'delta-log:toggle-row!) log-browser)
        (error 'delta-log:toggle-row! "the rows are conflicts; delta-log:open! lists the entries"))
      (head:with-buffer (car row) (delta-log-toggle! (car (cdr row))))))

  (define (conflict-row who)
    (let ([row (browser-row who)])
      (unless (eq? (current-browser who) conflicts-browser) (error who "the rows are entries; delta-log:conflicts! lists the conflicts"))
      row))

  (edoc "Show the other side of the browser's current row's conflict in place, or put the disk's side back, as delta-log:flip! does with its revision.")
  (define (delta-log-flip-row!)
    (let ([row (conflict-row 'delta-log:flip-row!)])
      (head:with-buffer (car row) (delta-log-flip! (car (cdr row))))))

  (edoc "Settle the browser's current row's conflict writing this side over the disk's region, as delta-log:resolve! does with mine; LEFT, the Mine column's side.")
  (define (delta-log-keep-mine!)
    (let ([row (conflict-row 'delta-log:keep-mine!)])
      (head:with-buffer (car row) (delta-log-resolve! (car (cdr row)) 'mine))))

  (edoc "Settle the browser's current row's conflict keeping the disk's side, as delta-log:resolve! does with disk; RIGHT, the Disk column's side.")
  (define (delta-log-keep-disk!)
    (let ([row (conflict-row 'delta-log:keep-disk!)])
      (head:with-buffer (car row) (delta-log-resolve! (car (cdr row)) 'disk))))

  ;; the browsers' keys: what both list in their modes' contexts, the log's
  ;; and the conflicts' own beside
  (define browser-keys
    `((("DOWN" "C-n" "M-n") ,delta-log-next!) (("UP" "C-p" "M-p") ,delta-log-previous!)
      (("PGDN" "C-v") ,delta-log-page-down!) (("PGUP" "M-v") ,delta-log-page-up!)
      (("RET") ,delta-log-show-row!) (("ESC") ,delta-log-close!) (("C-g") ,delta-log-cancel!)))

  (define log-keys `((("M-t") ,delta-log-toggle-row!) (("M-RET") ,delta-log-commit!)))

  (define conflicts-keys
    `((("LEFT" "M-m") ,delta-log-keep-mine!) (("RIGHT" "M-d") ,delta-log-keep-disk!) (("SPC" "M-/") ,delta-log-flip-row!)))

  (define (bind-keys! context table)
    (for-each (lambda (entry) (for-each (lambda (key) (keymap:bind-default! context key (cadr entry))) (car entry))) table))

  (edoc "Install the delta log: the two browsers' modes with their keys bound in the delta-log and conflicts contexts, C-x l and C-x ! opening them in the pop-up, the highlighter marking a previewed or browsed entry's span, a browsed conflict's region and a flip's, and the browsers following the windows and the store before every frame; C-x TAB lists the keys.")
  (define (init!)
    (mode:register! "delta-log" '() '() (lambda (line) #f) #f styles)
    (mode:register! "conflicts" '() '() (lambda (line) #f) #f styles)
    (bind-keys! 'delta-log browser-keys)
    (bind-keys! 'delta-log log-keys)
    (bind-keys! 'conflicts browser-keys)
    (bind-keys! 'conflicts conflicts-keys)
    (keymap:bind-default! "C-x l" (keymap:call delta-log-open! 0))
    (keymap:bind-default! "C-x !" (keymap:call delta-log-conflicts! 0))
    (paint:add-highlighter! entry-highlights)
    (paint:add-highlighter!
      (lambda ()
        ;; the tinted current row of each window showing a browser
        (apply append
          (map (lambda (br)
                 (if (browser-live? br)
                     (map (lambda (w)
                            (let ([row (head:window-prow w)])
                              (list w row 0 (string-length (vector-ref (head:window-lines w) row)) 'candidate)))
                          (filter (lambda (w) (> (head:window-prow w) 0)) (browser-windows br)))
                     '()))
               browsers))))
    (head:add-pre-redraw-hook! follow!))

) ;; library (delta-log)
