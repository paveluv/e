;; delta-log.sls -- the delta log at M-x: a buffer's entries as data, a
;; view with entries disabled shown live in a local buffer, committed as a
;; rewrite of the trunk for everyone or abandoned, the revision, batch and
;; conflict types completing from the current buffer's log and its pending
;; reload conflicts, a candidate previewing itself while a prompt has it,
;; and the <delta-log> browser over the entries or the conflicts.

(import (only (foundation edoc) elibrary))
(elibrary (apps delta-log)
  (export (rename (delta-log-cancel! cancel!)) (rename (delta-log-choose! choose!)) (rename (delta-log-close! close!))
          (rename (delta-log-commit! commit!)) (rename (delta-log-commit-picks! commit-picks!))
          (rename (delta-log-conflicts conflicts)) (rename (delta-log-conflicts! conflicts!))
          (rename (delta-log-disabled disabled)) (rename (delta-log-filter! filter!)) (rename (delta-log-flip! flip!))
          (rename (delta-log-flip-row! flip-row!)) init! (rename (delta-log-entries log)) (rename (delta-log-next! next!))
          (rename (delta-log-open! open!)) (rename (delta-log-page-down! page-down!)) (rename (delta-log-page-up! page-up!))
          (rename (delta-log-pick! pick!)) (rename (delta-log-pick-all! pick-all!))
          (rename (delta-log-pick-disk! pick-disk!)) (rename (delta-log-pick-mine! pick-mine!))
          (rename (delta-log-picks picks)) (rename (delta-log-previous! previous!)) (rename (delta-log-resolve! resolve!))
          (rename (delta-log-resolve-all! resolve-all!)) (rename (delta-log-revert! revert!))
          (rename (delta-log-rewrite-preview rewrite-preview))
          (rename (delta-log-save-row! save-row!)) (rename (delta-log-show! show!))
          (rename (delta-log-show-row! show-row!)) (rename (delta-log-toggle! toggle!)) (rename (delta-log-toggle-row! toggle-row!)))
  (import (rnrs)
          (only (chezscheme) hashtable-values parameterize format make-weak-eq-hashtable void)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head render) render:)
          (prefix (head table) table:)
          (prefix (head window) window:)
          (prefix (service conflict-review) conflict-review:)
          (prefix (service log) log:)
          (prefix (service rewrite) rewrite:)
          (prefix (state model) model:)
          (prefix (state store) store:)
          (prefix (sys glyph) glyph:))

  ;;; The log -------------------------------------------------------------------

  ;; One default rewrite preview per head: the trunk buffer, the local buffer showing the
  ;; view's text, the revisions disabled, and the conflicts the last rendering
  ;; found, (disabled . later) pairs naming a later entry that overlaps a
  ;; disabled one.
  (define-record-type rewrite-preview (fields trunk buffer draft token (mutable snapshot) (mutable conflicts) (mutable key)))
  (define the-rewrite #f)
  (define (field r key) (cdr (assq key r)))
  (define (rewrite-preview-disabled v) (field (field (rewrite-preview-snapshot v) 'value) 'disabled))
  (define (adopt-rewrite! v r)
    (when (and r (>= (field r 'revision) (field (rewrite-preview-snapshot v) 'revision))) (rewrite-preview-snapshot-set! v r)))

  (define (trunk-of b)
    ;; the shared buffer whose log b shows: b itself, or the trunk behind
    ;; the view, the browser or a flip
    (cond [(and the-rewrite (eq? b (rewrite-preview-buffer the-rewrite))) (rewrite-preview-trunk the-rewrite)]
          [(browser-of b) => (lambda (br)
                               ;; a browser stands for its current row's buffer, else the first it tracks
                               (let ([row (current-row br)] [tracked (tracked-buffers)])
                                 (cond [row (car row)] [(pair? tracked) (car tracked)] [else b])))]
          [(preview-trunk-of b) => values]
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

  (define (span-ranges marked . face)
    ;; a marked span, row by row, in the match face unless another is given
    (let* ([b (car marked)] [span (cdr marked)] [face (if (pair? face) (car face) 'match)]
           [start (text:span-start span)] [end (text:span-end span)])
      (let loop ([row (car start)] [out '()])
        (if (> row (car end)) (reverse out)
            (let* ([from (if (= row (car start)) (cdr start) 0)]
                   [to (if (= row (car end)) (cdr end) (string-length (head:buffer-line b row)))])
              (loop (+ row 1) (cons (list b row from (max to (+ from 1)) face) out)))))))

  (define browsed-face 'match) ; the browsed span's face, #f when the conflict regions paint it

  (define (entry-highlights)
    ;; the previewed entry's span, the browsed one's, and every pending
    ;; conflict's region in the face of the side it shows
    (append (if previewed (span-ranges previewed) '())
            (if (and browsed browsed-face) (span-ranges browsed browsed-face) '())
            (conflict-highlights)))

  (define (same-span? a b)
    (and (equal? (text:span-start a) (text:span-start b)) (equal? (text:span-end a) (text:span-end b))))

  (define (side-face side current?)
    (case side
      [(mine) (if current? 'conflict-mine-current 'conflict-mine)]
      [else (if current? 'conflict-disk-current 'conflict-disk)]))

  (define (conflict-highlights)
    ;; the regions of every tracked buffer's pending conflicts, in the buffer
    ;; standing for it on screen, each in its side's face, the browsed one
    ;; brighter
    (apply append
      (map (lambda (trunk)
             (let ([shown (shown-for trunk)])
               (apply append
                 (map (lambda (region)
                        (let ([current? (and browsed (eq? (car browsed) shown) (same-span? (cdr browsed) (caddr region)))])
                          (span-ranges (cons shown (caddr region)) (side-face (cadr region) current?))))
                      (regions-of trunk)))))
           (filter head:buffer-conflicted (tracked-buffers)))))

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
  ;; labels region mine disk). The disk's side stands in the text. A review
  ;; picks a side per conflict, mine or disk, disk unless picked otherwise,
  ;; and the picks show at once in a preview: a read-only local buffer where
  ;; the trunk was, with every mine pick's lines over its region. The picks
  ;; settle together when asked. Two mine picks whose regions share text
  ;; cannot both be written, so the later pick sends the other back to disk.
  ;; Only the default host adapter lives here; witnesses and choices are base data.
  (define review-id #f)
  (define review-snapshot #f)
  (define review-token #f)
  (define review-stamp #f)
  (define-record-type preview (fields trunk buffer (mutable regions) (mutable key)))
  (define previews (make-weak-eq-hashtable))
  (define (adopt-review! r)
    (when (and r (or (not review-snapshot) (>= (field r 'revision) (field review-snapshot 'revision))))
      (set! review-snapshot r)))
  (define (review-records) (if review-snapshot (field (field review-snapshot 'value) 'records) '()))
  (define (review-entry trunk)
    (find (lambda (e) (equal? (trunk-id trunk) (field e 'document))) (review-records)))
  (define (review-revision) (field review-snapshot 'revision))
  (define (review-trunks)
    (filter (lambda (b) (and (trunk-id b) (review-entry b))) (head:buffers)))
  (define (refresh-review! trunks)
    (let ([scope (map trunk-id trunks)])
      (cond [(not review-id)
             (set! review-id (conflict-review:create! head:ui-actor scope))
             (set! review-token (kernel:call-with-runtime-registrations
                                  (lambda () (model:subscribe! (list review-id)
                                               (lambda (notice) (adopt-review! (model:snapshot review-id)) (head:wake-main!))))))
             (adopt-review! (model:snapshot review-id))]
            [else (adopt-review! (conflict-review:refresh! head:ui-actor review-id (review-revision) scope))])))
  (define (ensure-review! trunk)
    (unless (review-entry trunk) (refresh-review! (cons trunk (review-trunks)))))
  (define (mine-picks trunk) (cond [(review-entry trunk) => (lambda (e) (field e 'mine))] [else '()]))
  (define (reviewed-conflicts trunk)
    (cond [(review-entry trunk) => (lambda (e) (field e 'alternatives))] [else (store:conflicts (trunk-id trunk))]))

  (define (conflict-at trunk revision)
    (or (find (lambda (c) (= (car c) revision)) (store:conflicts (trunk-id trunk)))
        (error 'delta-log "no pending conflict at that revision" revision)))

  (define (conflict-hint c . width)
    ;; a conflict in one line, its revision apart: the actor, where the
    ;; disk's side stands, and both sides, elided to a width when one is given
    (let* ([start (text:span-start (region-of c))]
           [side (lambda (lines)
                   (let ([s (string:join lines "\n")])
                     (spell (if (pair? width) (string:elide s (car width)) s))))])
      (format "~a  ~a:~a  mine ~a · disk ~a" (spell (cadr c)) (car start) (cdr start)
              (side (list-ref c 4)) (side (list-ref c 5)))))

  (define (conflict-text c . width) (format "~a  ~a" (car c) (apply conflict-hint c width)))

  (define (region-of c) (text:datum->span (cadddr c)))

  (define (sorted-conflicts trunk)
    ;; the trunk's pending conflicts in the order of their regions in the text
    (sort-conflicts (guard (ex [else '()]) (store:conflicts (trunk-id trunk)))))

  (define (sort-conflicts conflicts)
    (list-sort (lambda (a b) (text:position<? (text:span-start (region-of a)) (text:span-start (region-of b))))
      conflicts))

  (define (side-of trunk c)
    (if (and (memv (car c) (mine-picks trunk)) (member c (reviewed-conflicts trunk))) 'mine 'disk))

  (define (pick-conflict! trunk c side)
    (ensure-review! trunk)
    (let ([accepted? (guard (ex [else #f])
                       (adopt-review! (conflict-review:choose! head:ui-actor review-id (review-revision)
                                        (list (list (trunk-id trunk) c)) side)) #t)])
      (follow!)
      (log:add! 'delta-log:pick-conflict!
        (if accepted? (format "Conflict ~a shows ~a" (car c) side)
          "Conflict alternatives changed; review the refreshed choices"))
      (and accepted? side)))

  (define (preview-of trunk) (hashtable-ref previews trunk #f))

  (define (preview-trunk-of b)
    ;; the trunk a preview buffer stands for, or #f for any other buffer
    (let ([hit (find (lambda (pv) (eq? (preview-buffer pv) b)) (vector->list (hashtable-values previews)))])
      (and hit (preview-trunk hit))))

  (define (shown-for trunk)
    ;; the buffer standing for the trunk on screen: its preview while one shows, else itself
    (cond [(preview-of trunk) => preview-buffer] [else trunk]))

  (define (regions-of trunk)
    ;; the conflicts' regions as (revision side span) in the shown buffer's text
    (cond [(preview-of trunk) => preview-regions]
          [else
           (let-values ([(text revision conflicts) (store:conflict-state (trunk-id trunk))])
             ;; A frame will synchronize the head. Until then, do not paint
             ;; newer coordinates onto the older text still on screen.
             (if (= revision (head:buffer-store-rev trunk))
                 (map (lambda (c) (list (car c) 'disk (region-of c))) (sort-conflicts conflicts)) '()))]))

  (define (start-preview! trunk)
    (let ([vb (head:fresh-buffer! (string-append "<preview: " (head:buffer-name trunk) ">"))])
      (head:buffer-fact-set! vb 'mode (head:buffer-fact trunk 'mode #f))
      (head:buffer-read-only-set! vb #t)
      (head:add-buffer! vb)
      (let ([pv (make-preview trunk vb '() #f)])
        (hashtable-set! previews trunk pv)
        pv)))

  (define (drop-preview! pv)
    ;; the windows showing the preview show the trunk again; the preview buffer goes
    (let ([trunk (preview-trunk pv)] [vb (preview-buffer pv)])
      (for-each (lambda (w)
                  (when (eq? (head:window-buffer w) vb)
                    (head:set-window-buffer! w (if (memq trunk (head:buffers)) trunk
                                                   (find (lambda (o) (not (eq? o vb))) (head:buffers))))))
                (head:windows))
      (head:forget-buffer! vb)
      (hashtable-delete! previews trunk)))

  (define (render-previews!)
    ;; Temporary local presentation of a base draft; the widget composition
    ;; replaces this adapter and its outer-window placement policy together.
    (let ([trunks (let dedupe ([ts (append (if (browser-live? conflicts-browser) (tracked-buffers) '())
                                     (review-trunks) (map preview-trunk (vector->list (hashtable-values previews))))] [out '()])
                    (cond [(null? ts) (reverse out)]
                      [(or (memq (car ts) out) (not (memq (car ts) (head:buffers))) (not (trunk-id (car ts)))) (dedupe (cdr ts) out)]
                      [else (dedupe (cdr ts) (cons (car ts) out))]))])
      (let ([stamp (map (lambda (b) (list (trunk-id b) (head:buffer-store-rev b) (head:buffer-fact b 'conflicts 0))) trunks)])
        (when (and (or review-id (pair? trunks)) (not (equal? stamp review-stamp)))
          (refresh-review! trunks)
          (set! review-stamp stamp)))
      (for-each
        (lambda (trunk)
          (let* ([e (review-entry trunk)] [mine (field e 'mine)] [pv (preview-of trunk)])
            (when (pair? (field e 'invalidated)) (edit:set-message! "Conflict alternatives changed; review the refreshed choices"))
            (cond [(null? mine) (when pv (drop-preview! pv))]
              [else
               (let ([key (list (field e 'basis) (field e 'alternatives) mine)])
                 (unless (and pv (equal? (preview-key pv) key))
                   (let* ([data (conflict-review:preview review-id (trunk-id trunk))]
                          [pv (or pv (start-preview! trunk))])
                     (preview-key-set! pv key)
                     (preview-regions-set! pv (map (lambda (r) (list (car r) (cadr r) (text:datum->span (caddr r)))) (list-ref data 4)))
                     (head:buffer-read-only-set! (preview-buffer pv) #f)
                     (head:buffer-lines-set! (preview-buffer pv) (list-ref data 3))
                     (head:buffer-read-only-set! (preview-buffer pv) #t)
                     (for-each (lambda (w) (when (eq? (head:window-buffer w) trunk) (head:set-window-buffer! w (preview-buffer pv))))
                       (head:windows)))))]))) trunks)))

  (define (clear-picks!)
    (when review-id
      (refresh-review! (review-trunks))
      (adopt-review! (conflict-review:choose! head:ui-actor review-id (review-revision)
                       (map (lambda (trunk) (cons (trunk-id trunk) (reviewed-conflicts trunk))) (review-trunks)) 'disk)))
    (render-previews!))

  (define (preview-conflict! revision)
    ;; Legacy M-x preview restores only its own untouched draft revision.
    (let ([trunk (guard (ex [else #f]) (current-trunk 'conflict))])
      (and trunk (integer? revision)
        (let ([c (find (lambda (c) (= (car c) revision)) (sorted-conflicts trunk))])
          (and c
            (begin
              (ensure-review! trunk)
              (let ([was (mine-picks trunk)])
                (and (pick-conflict! trunk c 'mine)
                  (let ([basis (review-revision)])
                    (lambda ()
                      (when (= basis (review-revision))
                        (let* ([cs (reviewed-conflicts trunk)] [document (trunk-id trunk)])
                          (adopt-review! (conflict-review:choose! head:ui-actor review-id basis (list (cons document cs)) 'disk))
                          (adopt-review! (conflict-review:choose! head:ui-actor review-id (review-revision)
                                           (list (cons document (filter (lambda (c) (memv (car c) was)) cs))) 'mine)))
                        (render-previews!))))))))))))

  (edoc-type conflict "a pending conflict of the current buffer's last reload, by the revision of the entry the disk contradicted"
    (predicate (lambda (v) (and (integer? v) (exact? v) (>= v 1))))
    (complete (lambda (partial)
                (guard (ex [else '()])
                  (map (lambda (c) (cons (car c) (conflict-hint c 20))) (store:conflicts (trunk-id (current-trunk 'conflict)))))))
    (write number->string)
    (preview preview-conflict!))

  (edoc "The current buffer's pending reload conflicts as data, newest first, (revision actor labels region mine disk) each: the disabled entry's revision, actor and labels, the region the disk's side occupies, the entry's lines and the disk's."
        (returns list) (public))
  (define (delta-log-conflicts)
    (store:conflicts (trunk-id (current-trunk 'delta-log:conflicts))))

  (edoc "Settle a reload conflict: disk keeps the disk's side and drops the pending mark, mine writes the entry's side over the disk's region, replacement lines write those; a write is one undoable edit."
        (conflict conflict "the conflict")
        (choice (or (one-of disk mine) (list-of string)) "disk, mine or the replacement lines")
        (returns symbol "applied, refused or nothing") (public))
  (define (delta-log-resolve! conflict choice)
    (let* ([trunk (current-trunk 'delta-log:resolve!)] [revision (edoc:type-value 'conflict conflict)])
      (let-values ([(status detail) (store:resolve! head:ui-actor (trunk-id trunk) revision choice 'any)])
        (head:before-frame!)
        (log:add! 'delta-log:resolve!
          (case status
            [(applied)
             (let ([left (length (store:conflicts (trunk-id trunk)))])
               (format "Conflict ~a settled, ~a; ~a pending" revision
                       (cond [(eq? choice 'disk) "the disk's side kept"] [(eq? choice 'mine) "your side written"] [else "your lines written"])
                       left))]
            [else (format "Conflict ~a: ~a ~s" revision status detail)]))
        (follow!)
        status)))

  (define (resolve-conflicts! trunks one-way who)
    (for-each ensure-review! trunks)
    (when one-way
      (refresh-review! (review-trunks))
      (adopt-review! (conflict-review:choose! head:ui-actor review-id (review-revision)
                       (map (lambda (trunk) (cons (trunk-id trunk) (reviewed-conflicts trunk))) trunks) one-way)))
    (let* ([groups (map (lambda (trunk) (review-entry trunk)) trunks)]
           [results (if review-id (conflict-review:settle! head:ui-actor review-id (review-revision) (map trunk-id trunks)) '())]
           [settled (filter (lambda (e) (eq? (cadr (assv (field e 'document) results)) 'applied)) groups)]
           [mine (apply + (map (lambda (e) (length (field e 'mine))) settled))]
           [total (apply + (map (lambda (e) (length (field e 'alternatives))) settled))])
      (when review-id (model:snapshots (list review-id)) (adopt-review! (model:snapshot review-id)))
      (head:before-frame!)
      (follow!)
      (log:add! 'delta-log:resolve-conflicts!
        (format "~a conflicts settled: ~a mine, ~a disk~a" total mine (- total mine)
          (if (exists (lambda (r) (eq? (cadr r) 'refused)) results) "; refused documents remain pending" "")))
      total))

  (edoc "Settle every pending reload conflict of the current buffer, or the browser's current row's buffer: as picked by default, or all mine or disk when given. The target buffer is the same with or without a choice."
        (choice (list-of (one-of disk mine)) "disk or mine for them all, at most one")
        (returns integer "how many were settled") (public))
  (define (delta-log-resolve-all! . choice)
    (unless (or (null? choice) (and (null? (cdr choice)) (memq (car choice) '(disk mine))))
      (error 'delta-log:resolve-all! "expected at most one choice, mine or disk" choice))
    (resolve-conflicts! (list (current-trunk 'delta-log:resolve-all!)) (and (pair? choice) (car choice)) 'resolve-all!))

  (edoc "Commit the conflict conflict-review: settle every pending conflict of the buffers the browsers track as picked, mine or disk each. This is the conflicts browser's Settle action; unpicked conflicts keep disk."
        (returns integer "how many were settled"))
  (define (delta-log-commit-picks!)
    (resolve-conflicts! (filter head:buffer-conflicted (tracked-buffers)) #f 'commit-picks!))

  (edoc "Pick the side a reload conflict shows, mine or disk, in the preview where the buffer is, nothing settled yet; a mine pick over text another mine pick already covers sends that one back to disk."
        (conflict conflict "the conflict")
        (side (one-of mine disk) "the side to show")
        (returns (or symbol #f) "the side, or #f when the alternative changed meanwhile") (public))
  (define (delta-log-pick! conflict side)
    (let* ([trunk (current-trunk 'delta-log:pick!)] [revision (edoc:type-value 'conflict conflict)])
      (unless (memq side '(mine disk)) (error 'delta-log:pick! "expected mine or disk" side))
      (pick-conflict! trunk (conflict-at trunk revision) side)))

  (edoc "Preview the same side for every pending conflict, changing only picks: current selects the current buffer or browser row's buffer, the default; visible selects every buffer in the review, as the Mine (all) and Disk (all) headers do. Overlapping Mine regions refuse the whole choice."
        (side (one-of mine disk) "the side to preview")
        (scope (list-of (one-of current visible)) "current or visible, at most one")
        (returns integer "how many conflicts were picked"))
  (define (delta-log-pick-all! side . scope)
    (unless (memq side '(mine disk)) (error 'delta-log:pick-all! "expected mine or disk" side))
    (unless (or (null? scope) (and (null? (cdr scope)) (memq (car scope) '(current visible))))
      (error 'delta-log:pick-all! "expected at most one scope, current or visible" scope))
    (let* ([trunks (if (equal? scope '(visible)) (tracked-buffers) (list (current-trunk 'delta-log:pick-all!)))])
      (for-each ensure-review! trunks)
      (refresh-review! (review-trunks))
      (let* ([groups (map (lambda (trunk) (cons (trunk-id trunk) (reviewed-conflicts trunk))) trunks)]
             [n (apply + (map (lambda (g) (length (cdr g))) groups))])
        (adopt-review! (conflict-review:choose! head:ui-actor review-id (review-revision) groups side))
        (follow!)
        (log:add! 'delta-log:pick-all! (format "~a conflicts show ~a; nothing settled" n side)) n)))

  (edoc "Flip the side a reload conflict shows, mine for disk and back, as delta-log:pick! does."
        (conflict conflict "the conflict")
        (returns (or symbol #f) "the side shown now, or #f when the alternative changed meanwhile") (public))
  (define (delta-log-flip! conflict)
    (let* ([trunk (current-trunk 'delta-log:flip!)] [c (conflict-at trunk (edoc:type-value 'conflict conflict))])
      (pick-conflict! trunk c (if (eq? (side-of trunk c) 'mine) 'disk 'mine))))

  (edoc "The sides the current buffer's pending conflicts show, (revision . side) each in the order of their regions."
        (returns list) (public))
  (define (delta-log-picks)
    (let ([trunk (current-trunk 'delta-log:picks)])
      (map (lambda (c) (cons (car c) (side-of trunk c))) (sorted-conflicts trunk))))

  ;;; The view --------------------------------------------------------------------

  (define (start-rewrite! trunk)
    ;; a fresh view of the trunk in a local tool buffer sharing its mode; a
    ;; view of another buffer ends first, this head having one at a time
    (when the-rewrite (drop-rewrite! the-rewrite))
    (let* ([vb (head:fresh-buffer! (string-append "<rewrite: " (head:buffer-name trunk) ">"))]
           [draft (rewrite:create! head:ui-actor (trunk-id trunk))] [v #f]
           [token (kernel:call-with-runtime-registrations
                    (lambda () (model:subscribe! (list draft)
                                 (lambda (notice) (when v (adopt-rewrite! v (model:snapshot draft)) (head:wake-main!))))))])
      (head:buffer-fact-set! vb 'mode (head:buffer-fact trunk 'mode #f))
      (set! v (make-rewrite-preview trunk vb draft token (model:snapshot draft) '() #f))
      (set! the-rewrite v)
      the-rewrite))

  (define (render-rewrite! v)
    ;; the view's text in its buffer, shown where the trunk was, point
    ;; carried from the trunk through the view's mapping
    (let ([trunk (rewrite-preview-trunk v)] [vb (rewrite-preview-buffer v)])
      (let* ([snapshot (rewrite:preview (rewrite-preview-draft v))]
             [text (list-ref snapshot 3)] [mapping (list-ref snapshot 4)] [conflicts (list-ref snapshot 5)])
        (rewrite-preview-key-set! v (list (list-ref snapshot 6) (list-ref snapshot 2)))
        (rewrite-preview-conflicts-set! v conflicts)
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

  (define (drop-rewrite! v)
    ;; the windows showing the view return to the trunk; the view buffer is retired
    (let ([trunk (rewrite-preview-trunk v)] [vb (rewrite-preview-buffer v)])
      (when (memq trunk (head:buffers))
        (for-each (lambda (w) (when (eq? (head:window-buffer w) vb) (head:set-window-buffer! w trunk))) (head:windows)))
      (head:forget-buffer! vb)
      (set! the-rewrite #f)
      (model:snapshots (list (rewrite-preview-draft v)))
      (let ([r (model:snapshot (rewrite-preview-draft v))])
        (when r (rewrite:close! head:ui-actor (rewrite-preview-draft v) (field r 'revision))))
      (model:unsubscribe! (rewrite-preview-token v))))

  (define (report! v)
    (let ([n (length (rewrite-preview-disabled v))] [k (length (rewrite-preview-conflicts v))])
      (edit:set-message!
        (string-append
          (format "View of ~a: ~a entr~a disabled" (head:buffer-name (rewrite-preview-trunk v)) n (if (= n 1) "y" "ies"))
          (if (= k 0) ""
              (format "; ~a conflict~a, a later entry over a disabled one: ~a" k (if (= k 1) "" "s")
                      (string:join (map (lambda (c) (format "~a over ~a" (cdr c) (car c))) (rewrite-preview-conflicts v)) ", ")))))))

  ;;; Commands --------------------------------------------------------------------

  (edoc "The current buffer's delta log as data, newest first, (revision actor labels delta origin state) each, the log behind its view when a view is current; a selector narrows it by count, actor, batch, since, until or state, and a batch given alone selects its entries."
        (selector (list-of (or list batch)) "the selector or a batch, at most one")
        (returns list) (public))
  (define (delta-log-entries . selector)
    (apply store:log (trunk-id (current-trunk 'delta-log:log)) (map selector-of selector)))

  (edoc "Toggle entries in the view of the current buffer: a revision disabled in the view is enabled again, any other joins the disabled; the view's text, the rest rebased over their absence, shows in the window at once as a local buffer, and with nothing disabled the view ends."
        (revisions (list-of revision) "the entries to toggle")
        (returns list "the revisions disabled"))
  (define (delta-log-toggle! . revisions)
    (let* ([trunk (current-trunk 'delta-log:toggle!)]
           [v (if (and the-rewrite (eq? (rewrite-preview-trunk the-rewrite) trunk)) the-rewrite (start-rewrite! trunk))])
      (adopt-rewrite! v (rewrite:toggle! head:ui-actor (rewrite-preview-draft v) (field (rewrite-preview-snapshot v) 'revision)
                          (map (lambda (r) (edoc:type-value 'revision r)) revisions)))
      (cond
        [(null? (rewrite-preview-disabled v)) (drop-rewrite! v) (edit:set-message! "View ended: nothing disabled") (follow!) '()]
        [else (render-rewrite! v) (report! v) (follow!) (rewrite-preview-disabled v)])))

  (edoc "Commit the view: the trunk rewritten for everyone with the view's entries disabled, their inverses this head's own undoable action, and the window back on the trunk; blocked when a later entry overlaps a disabled one, the conflicts named."
        (returns symbol "applied, blocked, refused or nothing"))
  (define (delta-log-commit!)
    (let* ([v (or the-rewrite (error 'delta-log:commit! "no view to commit"))] [n (length (rewrite-preview-disabled v))])
      (let-values ([(status detail) (rewrite:settle! head:ui-actor (rewrite-preview-draft v) (field (rewrite-preview-snapshot v) 'revision))])
        (case status
          [(applied)
           (let ([name (head:buffer-name (rewrite-preview-trunk v))])
             (drop-rewrite! v)
             (head:before-frame!)
             (edit:set-message! (format "Rewrote ~a: ~a entr~a disabled, now revision ~a" name n (if (= n 1) "y" "ies") detail))
             (follow!))]
          [(blocked) (edit:set-message! (format "Rewrite blocked, a later entry over a disabled one: ~s" detail))]
          [else (edit:set-message! (format "Rewrite ~a: ~s" status detail))])
        status)))

  (edoc "Abandon the view: the window shows the trunk again, nothing rewritten." (public))
  (define (delta-log-revert!)
    (when the-rewrite
      (let ([name (head:buffer-name (rewrite-preview-trunk the-rewrite))])
        (drop-rewrite! the-rewrite)
        (edit:set-message! (format "View of ~a abandoned" name))
        (follow!))))

  (edoc "The active rewrite preview as data, (trunk-name disabled conflicts), or #f without one."
        (returns (or list #f)) (public))
  (define (delta-log-rewrite-preview)
    (and the-rewrite (list (head:buffer-name (rewrite-preview-trunk the-rewrite)) (rewrite-preview-disabled the-rewrite) (rewrite-preview-conflicts the-rewrite))))

  (edoc "The revisions the live view disables, newest first; () without a view."
        (returns (list-of integer)) (public))
  (define (delta-log-disabled)
    (if the-rewrite (list-sort > (rewrite-preview-disabled the-rewrite)) '()))

  (edoc "Describe an entry of the current buffer's log in the echo area: its actor, where it wrote, what it removed and inserted."
        (revision revision "the entry") (public))
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
                        [else (string-append (if (and the-rewrite (eq? (rewrite-preview-trunk the-rewrite) b) (memv (car entry) (rewrite-preview-disabled the-rewrite))) "- " "  ")
                                             (entry-hint entry (disablers-of b)))])))
                  #f '() '() (make-weak-eq-hashtable)))

  (define conflicts-browser
    (make-browser "<conflicts>" "conflicts"
                  (table:make '#("Buffer" "Rev" "Actor" "At" "Mine (all)" "Disk (all)") '#(8 4 8 6 12 12) 1 '(2 3 0) '#(text right text text text text))
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

  (define (under-browser w b)
    ;; Follow this window's saved buffers, never a browser's global row
    ;; lookup. Opening another browser carries this return target forward.
    (let under ([b b] [seen '()])
      (let ([br (browser-of b)])
        (if br
            (and (not (memq br seen))
                 (let ([back (assq w (browser-over br))])
                   (and back (under (cdr back) (cons br seen)))))
            b))))

  (define (tracked-buffers)
    ;; the shared buffers the windows show, a view or a flip standing for
    ;; its trunk and a browser for the buffer it replaced, each once, in
    ;; window order
    (let loop ([ws (head:windows)] [acc '()])
      (if (null? ws) (reverse acc)
          (let* ([shown (under-browser (car ws) (head:window-buffer (car ws)))] [b (and shown (trunk-of shown))])
            (loop (cdr ws) (if (and b (head:buffer-store-id b) (memq b (head:buffers)) (not (memq b acc))) (cons b acc) acc))))))

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
    ;; the rows the browser lists, (buffer . datum) each, in window order, a
    ;; buffer's conflicts in the order of their regions
    (apply append
      (map (lambda (b)
             (if (eq? br log-browser)
                 (map (lambda (row) (cons b row)) (log-rows-of b))
                 (if (head:buffer-conflicted b)
                     (map (lambda (c) (cons b c)) (sort-conflicts (reviewed-conflicts b)))
                     '())))
           (tracked-buffers))))

  (define conflict-columns '()) ; (index start end) of the conflicts table's columns as last laid out

  (define (settle-line rows)
    ;; the conflicts browser's last row, the one that settles every pick
    (let ([mine (length (filter (lambda (row) (eq? (side-of (car row) (cdr row)) 'mine)) rows))])
      (format "Settle all as picked: ~a mine, ~a disk" mine (- (length rows) mine))))

  (define (settle-row? br w)
    ;; whether the window's point is on the conflicts browser's settle row
    (and (eq? br conflicts-browser) (pair? (browser-rows br))
         (= (head:window-prow w) (+ 1 (length (browser-rows br))))))

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
         (set! conflict-columns cols)
         (let ([real (filter values rows)])
           (append (cons (line #f) (map (lambda (row) (if row (line row) "")) rows))
                   (if (null? real) '() (list (settle-line real))))))]))

  (define (refresh-browser! br)
    ;; the rows from the windows and the store, rendered again when their
    ;; text changed, a toggle's mark say, the current row kept by its buffer
    ;; and revision when the rows shift, and the highlight synced
    (when (browser-live? br)
      (let* ([b (browser-buffer br)] [rows (rows-now br)] [old (browser-rows br)] [lines (rendered-lines br rows)])
        (unless (equal? lines (vector->list (head:buffer-lines b)))
          ;; each window's place: its row's key, or the settle row it sits on
          (let ([keys (map (lambda (w)
                             (cons w (cond [(settle-row? br w) 'settle]
                                           [(current-row-in br w) => row-key]
                                           [else #f])))
                           (browser-windows br))])
            (browser-rows-set! br rows)
            (head:view-replace! b lines)
            (for-each (lambda (entry)
                        (let ([at (and (cdr entry) (not (eq? (cdr entry) 'settle))
                                       (list-index (lambda (row) (equal? (row-key row) (cdr entry))) rows))])
                          (head:window-prow-set! (car entry)
                            (cond [at (+ at 1)]
                                  [(and (eq? (cdr entry) 'settle) (pair? rows)) (+ 1 (length rows))]
                                  [else (min 1 (length rows))]))
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
    ;; the span a row marks: an entry's span rebased into its buffer's text,
    ;; or a conflict's region in the buffer standing for its own, the
    ;; preview's when one shows
    (if (eq? browser conflicts-browser)
        (let ([region (assv (car (cdr row)) (regions-of (car row)))])
          (and region (cons (shown-for (car row)) (caddr region))))
        (span-of (car row) (car (cdr row)))))

  (define browsed-key #f) ; (browser buffer revision trunk-revision) the browsed span was computed for

  (define (sync-browsed!)
    ;; the selected browser's current row highlighted in its buffer, recomputed
    ;; when the row or the buffer's revision changes; none while no browser shows
    (let* ([br (or (browser-of (head:current-buffer)) (find (lambda (br) (pair? (browser-windows br))) browsers))]
           [row (and br (browser-live? br) (current-row br))]
           [key (and row (list br (car row) (car (cdr row)) (head:buffer-store-rev (car row))
                           (and (eq? br conflicts-browser) (mine-picks (car row)))))])
      (unless (equal? key browsed-key)
        (set! browsed-key key)
        (set! browsed-face (if (eq? br conflicts-browser) #f 'match))
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
    ;; before every frame: the previews follow the store and the picks, and
    ;; the rows the windows and the store
    (render-previews!)
    (when the-rewrite
      (let* ([v the-rewrite] [trunk (rewrite-preview-trunk v)]
             [key (list (head:buffer-store-rev trunk) (rewrite-preview-disabled v))])
        (cond
          [(not (memq trunk (head:buffers))) (drop-rewrite! v)]
          [(not (equal? key (rewrite-preview-key v)))
           ;; A reload can retire the revisions selected in this view.
           ;; End that view visibly instead of leaving stale text to commit.
           (guard (ex [else (drop-rewrite! v) (edit:set-message! "View ended: its entries are no longer available")])
             (render-rewrite! v))])))
    (for-each refresh-browser! browsers))

  (define (move-row! delta)
    ;; the rows, and in the conflicts browser the settle row after them
    (let* ([br (current-browser 'delta-log:next!)] [w (head:current-window)] [n (length (browser-rows br))]
           [last (if (eq? br conflicts-browser) (+ n 1) n)])
      (when (> n 0)
        (let* ([at (head:window-prow w)]
               [row (min (max 1 (+ at delta)) last)])
          (unless (= row at) (head:goto! (cons row 0)))
          (sync-browsed!)
          (follow-row!)))))

  (define (browser-status b)
    ;; after the buffer's name, which the status line shows by itself: the
    ;; row of how many, and the log's filter when one is set
    (let* ([br (browser-of b)] [n (length (browser-rows br))] [w (browser-window br)]
           [i (if w (head:window-prow w) 0)])
      (cond [(and w (settle-row? br w)) "settle all as picked"]
            [else (format "~a of ~a~a" (min i n) n
                          (if (or (eq? br conflicts-browser) (null? browser-filter)) "" (format "  ~s" browser-filter)))])))

  (define (styles b row line)
    (make-vector (string-length line)
      (cond [(zero? row) 'header] [(string:prefix? "Settle all as picked" line) 'choice] [else 'plain])))

  (define (side-range w row side)
    ;; The table measures cells; mouse events and highlights use character
    ;; positions. Share the rendered row's projection, including wide glyphs.
    (let ([column (assv (if (eq? side 'mine) 4 5) conflict-columns)]
          [frame (head:window-rendition w)])
      (and column (list (render:character frame row (cadr column))
                        (render:character frame row (caddr column)) side))))

  (define (conflict-hit w row col)
    ;; Click and hover share the side cells, each header's (all), and Settle.
    (and (eq? (browser-of (head:window-buffer w)) conflicts-browser)
         (let* ([n (length (browser-rows conflicts-browser))] [lines (head:window-lines w)])
           (cond
             [(and (> n 0) (= row (+ n 1)) (< row (vector-length lines)))
              (let ([end (string-length (vector-ref lines row))])
                (and (<= 0 col) (< col end) (list 0 end 'settle)))]
             [(and (= row 0) (> n 0))
              (find (lambda (range) (and range (<= (car range) col) (< col (cadr range))))
                (map (lambda (side)
                       (let ([range (side-range w row side)])
                         (and range (list (+ (car range) 5) (+ (car range) 10) side)))) '(mine disk)))]
             [(<= 1 row n)
              (find (lambda (range) (and range (<= (car range) col) (< col (cadr range))))
                (map (lambda (side) (side-range w row side)) '(mine disk)))]
             [else #f]))))

  (define (browser-handler br)
    ;; The app's window is current while its handler runs, even when the
    ;; pointer came from an unfocused pane. A side click previews, the
    ;; settle button commits, and the rest of a data row only selects it.
    (lambda (event)
      (cond
        [(string=? event "MOUSE-CLICK")
         (let* ([at (head:app-event-buffer-position)] [w (head:current-window)]
                [row (and at (car at))] [n (length (browser-rows br))]
                [hit (and at (conflict-hit w row (cdr at)))])
           (cond
             [(not (and row w (memq w (browser-windows br)))) 'ignore-click]
             [(and hit (= row 0)) (delta-log-pick-all! (caddr hit) 'visible) 'keep-focus]
             [(and hit (eq? (caddr hit) 'settle)) (delta-log-commit-picks!) 'keep-focus]
             [(and (>= row 1) (<= row n))
              (head:goto! (cons row 0))
              (when hit
                (let ([entry (list-ref (browser-rows br) (- row 1))])
                  (pick-conflict! (car entry) (cdr entry) (caddr hit))))
              (sync-browsed!) (follow-row!) 'keep-focus]
             [else 'ignore-click]))]
        [(member event '("MOUSE-MOVE" "MOUSE-LEAVE")) #t]
        [else #f])))

  (define (ensure-browser! br)
    (unless (browser-live? br)
      (let ([b (head:register-app! (browser-name br) (lambda () (refresh-browser! br)) (browser-handler br))])
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
    ;; shown when it is the window, and selected; start at the caller's
    ;; buffer when it has rows, otherwise at the first available row
    (ensure-browser! br)
    (let ([b (browser-buffer br)] [source (trunk-of (head:current-buffer))])
      (unless (eq? (head:window-buffer w) b)
        (browser-over-set! br (cons (cons w (under-browser w (head:window-buffer w))) (remp (lambda (e) (eq? (car e) w)) (browser-over br))))
        (head:set-window-buffer! w b))
      (when (and (head:popup? w) (= (head:popup-rows) 0)) (head:show-popup! (head:popup-default-rows)))
      (window:focus! w)
      (browser-rows-set! br '())
      (refresh-browser! br)
      (let ([at (list-index (lambda (row) (eq? (car row) source)) (browser-rows br))])
        (head:goto! (cons (if at (+ at 1) (min 1 (length (browser-rows br)))) 0)))
      (sync-browsed!)
      (follow-row!)))

  (define (close-browser! br)
    ;; the windows showing the browser show what they showed before, the
    ;; pop-up hiding again when it showed its placeholder, and the app buffer goes
    (when (browser-live? br)
      (let ([b (browser-buffer br)])
        (when (eq? br conflicts-browser) (clear-picks!))
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

  (edoc "Open the conflicts browser, <conflicts>, in a window, the current one by default, and select it: one row per pending reload conflict of every buffer a window shows, with its region and both sides. Click Mine or Disk, or use LEFT and RIGHT, to preview that side; the headers' (all) controls and S-LEFT/S-RIGHT pick the side throughout the review. RET and SPC flip a row's pick, and on the last row commit every pick, as clicking Settle does. M-RET commits from any row. ESC closes the browser and abandons the previews."
        (w* (list-of window) "the window to open it in, at most one; the current window by default"))
  (define (delta-log-conflicts! . w*)
    (show-browser! conflicts-browser (target-window w*)))

  (edoc "Narrow the delta log browser's rows to the entries a selector picks, as for delta-log:log, or to a batch's; #f shows every entry again."
        (selector (or list batch #f) "the selector, a batch or #f") (public))
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

  (edoc "Describe the browser's current row in the echo area: an entry's actor and edit, both sides of a conflict in full, or the Settle row's counts. Inspection changes neither the text nor the picks.")
  (define (delta-log-show-row!)
    (let ([br (current-browser 'delta-log:show-row!)])
      (log:add! 'delta-log:show-row!
        (if (settle-row? br (head:current-window))
            (settle-line (browser-rows br))
            (let ([row (browser-row 'delta-log:show-row!)])
              (string-append (head:buffer-name (car row)) "  "
                             (if (eq? br conflicts-browser) (conflict-text (cdr row)) (entry-text (cdr row) (disablers-of (car row))))))))))

  (edoc "Activate the browser's selected row: describe a log entry, flip a conflict's preview side, or commit the review on the Settle row. RET in either browser, and SPC in conflicts; show-row! only describes, flip-row! only previews, and commit-picks! explicitly settles the review.")
  (define (delta-log-choose!)
    (let ([br (current-browser 'delta-log:choose!)])
      (cond [(settle-row? br (head:current-window)) (delta-log-commit-picks!)]
            [(eq? br conflicts-browser) (delta-log-flip-row!)]
            [else (delta-log-show-row!)])))

  (edoc "Close the delta log or conflicts browser the current buffer is: its windows show what they showed before, the pop-up hides when it showed nothing else, the conflicts browser's picks are abandoned and its preview ends, and point stays where the last row put it.")
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
    (let ([br (current-browser who)])
      (unless (eq? br conflicts-browser) (error who "the rows are entries; delta-log:conflicts! lists the conflicts"))
      (when (settle-row? br (head:current-window)) (error who "this row settles every conflict as picked"))
      (browser-row who)))

  (edoc "Flip the side the browser's current conflict row previews, as delta-log:flip! does with its revision. Nothing is settled; a conflict row must be selected.")
  (define (delta-log-flip-row!)
    (let ([row (conflict-row 'delta-log:flip-row!)])
      (pick-conflict! (car row) (cdr row) (if (eq? (side-of (car row) (cdr row)) 'mine) 'disk 'mine))))

  (edoc "Pick mine for the browser's current row's conflict, the entry's side shown in the preview, nothing settled yet, as delta-log:pick! does; LEFT, the Mine column's side.")
  (define (delta-log-pick-mine!)
    (let ([row (conflict-row 'delta-log:pick-mine!)])
      (pick-conflict! (car row) (cdr row) 'mine)))

  (edoc "Pick disk for the browser's current row's conflict, the disk's side shown, nothing settled yet, as delta-log:pick! does; RIGHT, the Disk column's side.")
  (define (delta-log-pick-disk!)
    (let ([row (conflict-row 'delta-log:pick-disk!)])
      (pick-conflict! (car row) (cdr row) 'disk)))

  (edoc "Save the browser's current row's buffer, or with no row, none pending say, the buffer of the window selected before the browser's, as C-x C-s does in that window: refused while its conflicts pend or it has no file.")
  (define (delta-log-save-row!)
    ;; the row's buffer saved in its window; a preview standing for it there
    ;; steps aside for the save and comes back
    (let* ([br (current-browser 'delta-log:save-row!)] [row (current-row br)]
           [here (head:current-window)] [previous (head:previous-window)]
           [trunk (cond [row (car row)] [(and previous (not (eq? previous here))) (trunk-of (head:window-buffer previous))] [else #f])]
           [w (and trunk (find (lambda (w) (eq? (head:window-buffer w) (shown-for trunk))) (head:windows)))])
      (unless w (error 'delta-log:save-row! "no row under point and no window to save from"))
      (let ([shown (head:window-buffer w)])
        (head:set-current! w)
        (unless (eq? shown trunk) (head:set-window-buffer! w trunk))
        (dynamic-wind void (lambda () (edit:save!))
          (lambda ()
            (when (and (not (eq? shown trunk)) (memq shown (head:buffers))) (head:set-window-buffer! w shown))
            (head:set-current! here))))))

  ;; the browsers' keys: what both list in their modes' contexts, the log's
  ;; and the conflicts' own beside; C-x C-s saves the row's buffer, since a
  ;; save is what a review ends in
  (define browser-keys
    `((("DOWN" "C-n" "M-n") ,delta-log-next!) (("UP" "C-p" "M-p") ,delta-log-previous!)
      (("PGDN" "C-v") ,delta-log-page-down!) (("PGUP" "M-v") ,delta-log-page-up!)
      (("RET") ,delta-log-choose!) (("ESC") ,delta-log-close!) (("C-g") ,delta-log-cancel!)
      (("C-x C-s") ,delta-log-save-row!)))

  (define log-keys `((("M-t") ,delta-log-toggle-row!) (("M-RET") ,delta-log-commit!)))

  (define conflicts-keys
    `((("LEFT" "M-m") ,delta-log-pick-mine!) (("RIGHT" "M-d") ,delta-log-pick-disk!)
      (("S-LEFT") ,(keymap:call delta-log-pick-all! 'mine 'visible))
      (("S-RIGHT") ,(keymap:call delta-log-pick-all! 'disk 'visible))
      (("SPC") ,delta-log-choose!) (("M-/") ,delta-log-flip-row!) (("M-RET") ,delta-log-commit-picks!)))

  (define (bind-keys! context table)
    (for-each (lambda (entry) (for-each (lambda (key) (keymap:bind-default! context key (cadr entry))) (car entry))) table))

  (edoc "Install the delta log: the two browsers' modes with their keys bound in the delta-log and conflicts contexts, C-x l and C-x ! opening them in the pop-up, the highlighter marking a previewed or browsed entry's span, a browsed conflict's region and a flip's, and the browsers following the windows and the store before every frame; C-x TAB lists the keys." (public))
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
    (paint:add-highlighter! (lambda () (paint:hover-ranges conflict-hit)))
    (paint:add-highlighter!
      (lambda ()
        ;; the tinted current row of each window showing a browser, and in the
        ;; conflicts browser the cell of the side each row shows, in that
        ;; side's face as its region is in the buffer, the current row's brighter
        (apply append
          (map (lambda (br)
                 (if (browser-live? br)
                     (apply append
                       (map (lambda (w)
                              (let ([current (head:window-prow w)] [lines (head:window-lines w)])
                                (append
                                  (if (> current 0) (list (list w current 0 (string-length (vector-ref lines current)) 'candidate)) '())
                                  (if (eq? br conflicts-browser) (side-cells w current lines) '()))))
                            (browser-windows br)))
                     '()))
               browsers))))
    (paint:set-conflicts-action! (lambda () (delta-log-conflicts! 0)))
    (head:add-pre-redraw-hook! follow!))

  (define (side-cells w current lines)
    ;; the Mine or Disk cell of each conflict row, whichever side it shows
    (let loop ([rows (browser-rows conflicts-browser)] [k 1] [out '()])
      (if (or (null? rows) (>= k (vector-length lines))) (reverse out)
          (let* ([row (car rows)] [side (side-of (car row) (cdr row))]
                 [range (side-range w k side)]
                 [len (string-length (vector-ref lines k))])
            (loop (cdr rows) (+ k 1)
                  (if (and range (< (car range) len))
                      (cons (list w k (car range) (min (cadr range) len) (side-face side (= k current))) out)
                      out))))))

) ;; library (delta-log)
