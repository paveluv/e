;; file-view.sls -- the local, filterable <files> app.
(import (only (foundation edoc) elibrary))
(elibrary (apps file-view)
  (export expansion-limit init! open! open-directory! refresh! show-hidden)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head style) style:)
          (prefix (head table) table:)
          (prefix (head window) window:)
          (prefix (service directory) directory:)
          (prefix (service doc) doc:)
          (prefix (service file) file:)
          (prefix (sys glyph) glyph:)
          (prefix (sys tty) tty:))

  (edoc "How many matching entries the files view expands a directory into while filtering."
        (value integer))
  (define expansion-limit (make-parameter 20
                            (lambda (n)
                              (unless (and (integer? n) (exact? n) (>= n 0))
                                (error 'expansion-limit "expected a nonnegative integer" n)) n)))

  (edoc "Whether the files view lists hidden entries, the dot files."
        (value boolean))
  (define show-hidden (make-parameter #f
                        (lambda (value)
                          (unless (boolean? value) (error 'show-hidden "expected a boolean" value)) value)))
  (define view #f)
  (define location #f)
  (define query "")
  (define path-part #f)             ; #f while browsing; literal completion prefix otherwise
  (define sorts '())
  (define inventory '())
  (define rows '())                 ; (full path . entry)
  (define complete? #f)
  (define failures 0)
  (define first-row 3)              ; filter, directory, column headings
  (define hover #f)                 ; (window . path/column)
  (define-record-type choice (fields (mutable origin) (mutable selected) (mutable columns) history))
  (define choices (make-weak-eq-hashtable))
  (define resumed-choices '())       ; plain window-number/path pairs from a checkpoint

  ;; One worker per module instance, replacing its pending request. Stale
  ;; results cannot mutate a new query, a killed app or a reloaded module.
  (define-record-type scan
    (fields buffer registration path query hidden? limit (mutable update) (mutable started?)))
  (define scan-lock (make-mutex))
  (define request #f)
  (define running? #f)
  (define (live-request? job)
    (and (with-mutex scan-lock (eq? request job))
         (scan-registration job)
         (eq? (scan-registration job) (head:app-of (scan-buffer job)))))

  (define (include-hidden?)
    (if path-part (string:prefix? "." path-part)
        (or (show-hidden) (string:prefix? "." query)
            (and (string:search query "/." 0 (string-length query)) #t))))

  (define (start-scan!)
    (set! complete? #f)
    (set! failures 0)
    (let ([needle (if path-part "" query)] [hidden? (include-hidden?)] [limit (expansion-limit)])
      (with-mutex scan-lock
        (set! inventory
          (if (and request (string=? (scan-path request) location))
              (directory:refilter inventory location (scan-query request) needle (scan-hidden? request) hidden? limit)
              '()))
        (set! request (make-scan view (head:app-of view) location needle hidden? limit #f #f))))
    (render!))

  (define (scan-work!)
    (let work ()
      (let ([job (with-mutex scan-lock request)])
        (define (publish entries skipped done? . failure)
          ;; Refresh consumes the latest snapshot, even while a modal prompt
          ;; owns input. Wakes carry no callbacks or old inventories.
          (when (with-mutex scan-lock
                  (and (eq? request job)
                       (begin (scan-update-set! job
                                (list entries skipped done? (and (pair? failure) (car failure)))) #t)))
            (head:wake-main!)))
        (when (and job (live-request? job))
          (with-mutex scan-lock (scan-started?-set! job #t))
          (guard (ex [else (publish '() 1 #t (kernel:condition-text ex))])
            (directory:scan (scan-path job) (scan-query job)
              (scan-hidden? job) (scan-limit job) (lambda () (not (live-request? job))) publish)))
        (when (with-mutex scan-lock
                (if (eq? request job) (begin (set! running? #f) #f) #t)) (work)))))

  (define (collect-scan!)
    (let ([job (with-mutex scan-lock request)])
      (when (and job (live-request? job))
        (let ([update (with-mutex scan-lock
                        (let ([update (scan-update job)]) (scan-update-set! job #f) update))])
          (when update
            (apply (lambda (entries skipped done? failure)
                     (set! inventory (directory:reconcile entries inventory (scan-limit job) done?))
                     (set! failures skipped) (set! complete? done?)
                     (when failure (edit:set-message! (string-append "File scan failed: " failure)))) update)))
        ;; Config/reload can render before publishing registration. Launch
        ;; only after the worker will see this same identity, never staged state.
        (when (and (kernel:call-with-runtime-registrations
                     (lambda () (eq? (scan-registration job) (head:app-of (scan-buffer job)))))
                   (with-mutex scan-lock
                     (and (not running?) (not (scan-started? job))
                          (begin (set! running? #t) #t))))
          (fork-thread scan-work!)))))

  (define (over w) (and hover (eq? (car hover) w) (cdr hover)))
  (define (at-row row)
    (and (<= first-row row (+ first-row (length rows) -1)) (list-ref rows (- row first-row))))
  (define (row-index path)
    (let loop ([left rows] [i first-row])
      (cond [(null? left) #f] [(equal? path (caar left)) i]
            [else (loop (cdr left) (+ i 1))])))
  (define (choice-for w)
    (or (hashtable-ref choices w #f)
        (let* ([saved (assv (head:window-index w) resumed-choices)]
               [state (make-choice #f (and saved (cdr saved)) '() (make-hashtable string-hash string=?))])
          (when saved (set! resumed-choices (remq saved resumed-choices)))
          (hashtable-set! choices w state) state)))
  (define (exact-row text)
    (define (find-path same?)
      (find (lambda (row)
              (let ([path (directory:relative-path (cdr row) location)])
                (or (same? path text)
                    (and (directory:directory? (cdr row)) (same? (string-append path "/") text))))) rows))
    (or (find-path string=?) (find-path string-ci=?)))
  (define (default-row)
    ;; Exact paths take precedence, including a typed directory path.
    ;; Otherwise select a filename ahead of its containing match group.
    (or (exact-row query)
        (and (not (string=? query ""))
             (find (lambda (row) (not (directory:directory? (cdr row)))) rows))
        (and (pair? rows) (car rows))))
  (define (keyboard-row w)
    (or (assoc (choice-selected (choice-for w)) rows) (default-row)))
  (define (candidate w)
    (or (and (string? (over w)) (assoc (over w) rows)) (keyboard-row w)))
  (define (column-at w at)
    (and at (= (car at) (- first-row 1))
         (find (lambda (column) (<= (cadr column) (cdr at) (- (caddr column) 1)))
           (choice-columns (choice-for w)))))

  (define (display-path path)
    ;; Control characters are legal in filenames, but not extra table rows
    ;; or terminal instructions. Keep the real pathname as the row identity.
    (apply string-append
      (map (lambda (c)
             (cond [(char=? c #\\) "\\\\"]
                   [(eq? (char-general-category c) 'Cc) (format "\\x~x;" (char->integer c))]
                   [else (string c)])) (string->list path))))
  (define (label row)
    (string-append
      (display-path (directory:relative-path (cdr row) location))
      (if (directory:entry-link? (cdr row)) "@" "")
      (if (directory:directory? (cdr row)) "/" "")))
  (define (raw row column)
    (let ([entry (cdr row)])
      (if (zero? column) (car row)
          (case column
            [(1) (and (not (directory:directory? entry)) (directory:entry-size entry))]
            [(2) (directory:entry-modified entry)]
            [(3) (directory:entry-created entry)]
            [(4) (directory:entry-mode entry)]
            [(5) (directory:entry-count entry)]))))
  (define (permissions entry)
    (let ([mode (directory:entry-mode entry)])
      (if (not mode) "?"
          (let ([text (string-copy "----------")])
            (string-set! text 0 (cond [(directory:entry-link? entry) #\l]
                                      [(directory:directory? entry) #\d]
                                      [(eq? (directory:entry-kind entry) 'file) #\-] [else #\?]))
            (do ([i 0 (+ i 1)]) ((= i 9))
              (unless (zero? (logand mode (expt 2 (- 8 i))))
                (string-set! text (+ i 1) (string-ref "rwxrwxrwx" i))))
            (for-each (lambda (bit index set unset)
                        (unless (zero? (logand mode bit))
                          (string-set! text index (if (char=? (string-ref text index) #\x) set unset))))
              '(#o4000 #o2000 #o1000) '(3 6 9) '(#\s #\s #\t) '(#\S #\S #\T)) text))))
  (define (cell row column)
    (let ([value (raw row column)] [entry (cdr row)])
      (cond [(zero? column) (label row)]
            [(= column 4) (permissions entry)]
            [(= column 5)
             (if (directory:directory? entry)
                 (if value (format "~a~a" value (if (directory:entry-complete? entry) "" "+")) "?") "")]
            [(not value) (if (and (= column 1) (directory:directory? entry)) "" "—")]
            [(memv column '(2 3))
             (let ([d (time-utc->date (make-time 'time-utc (mod value 1000000000) (div value 1000000000)))])
               (format "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" (date-year d) (date-month d)
                 (date-day d) (date-hour d) (date-minute d)))]
            [else
             (let scale ([n value] [units '("B" "KiB" "MiB" "GiB" "TiB")])
               (if (and (>= n 1024) (pair? (cdr units))) (scale (/ n 1024.0) (cdr units))
                   (if (string=? (car units) "B") (format "~a B" n)
                       (format "~,1f ~a" n (car units)))))])))
  (define (columns)
    (table:make (vector "Name" "Size" "Modified" "Created" "Permissions"
                  (if (string=? query "") "Entries" "Matches"))
      '#(14 8 16 16 13 9) 0 '(3 4 2 1 5) '#(text right text text text right)))
  (define (heading column) (table:heading (columns) sorts column))
  (define (entry<? a b)
    (table:less? sorts raw
      (lambda (a b)
        (or (string-ci<? (car a) (car b))
            (and (string-ci=? (car a) (car b)) (string<? (car a) (car b))))) a b))
  (define (listing)
    (let* ([filtered? (not (string=? query ""))]
           [inventory (if path-part
                          (filter (lambda (e)
                                    (and (string:prefix? path-part (file:base-name (directory:entry-path e)))
                                         (or (include-hidden?)
                                             (not (string:prefix? "." (file:base-name (directory:entry-path e))))))) inventory)
                          inventory)]
           [dirs (filter directory:directory? inventory)]
           [visible-dirs
            (filter (lambda (e)
                      (or (not filtered?) (directory:matches? e location query)
                          ;; Keep the route to an explicitly typed descendant,
                          ;; even when this directory is a non-traversed link.
                          (let ([prefix (string-append (directory:relative-path e location) "/")])
                            (and (<= (string-length prefix) (string-length query))
                                 (string-ci=? prefix (substring query 0 (string-length prefix)))))
                          (and (directory:entry-count e) (positive? (directory:entry-count e)))
                          ;; Keep unknown groups navigable, including failed
                          ;; searches, without resurfacing pending zeroes.
                          (and (not (directory:entry-link? e))
                               (or complete? (not (directory:entry-count e)))
                               (not (directory:entry-complete? e))))) dirs)]
           [files (append
                    (filter (lambda (e) (and (not (directory:directory? e)) (directory:matches? e location query))) inventory)
                    (if filtered? (apply append (map directory:entry-matches dirs)) '()))])
      (append (sort entry<? (map (lambda (e) (cons (directory:entry-path e) e)) visible-dirs))
        (sort entry<? (map (lambda (e) (cons (directory:entry-path e) e)) files)))))
  (define (directory-label)
    (string-append "Directory: " (display-path (file:abbreviate location)) (if (string=? location "/") "" "/")
      (if (and (not path-part) (show-hidden)) "  [hidden]" "")
      (if complete? "" "  Searching…")
      (if (positive? failures) (format "  ~a unreadable path~a" failures (if (= failures 1) "" "s")) "")))
  (define (directory-links line)
    ;; Clicks and hover share character ranges in the actual fitted line.
    ;; Expand displayed ancestors (including ~/) back to their real paths.
    ;; Left elision may hide ancestors, but its ellipsis is never a link.
    (let* ([path (file:abbreviate location)]
           [clipped? (and (positive? (string-length line)) (char=? (string-ref line 0) #\…))]
           [shift (if clipped?
                      (- (let trim ([end (string-length line)])
                           (if (and (positive? end) (char=? (string-ref line (- end 1)) #\space))
                               (trim (- end 1)) end))
                         (string-length (directory-label))) 0)])
      (let find ([from 0] [start (+ 11 shift)])
        (let ([slash (string:search path "/" from (string-length path))])
          (if (not slash) '()
              (let ([end (+ start (string-length (display-path (substring path from (+ slash 1)))))])
                (if (< (max (if clipped? 1 0) start) end)
                    (cons (list (max (if clipped? 1 0) start) end
                                (file:canonical (file:expand (substring path 0 (+ slash 1)))))
                          (find (+ slash 1) end))
                    (find (+ slash 1) end))))))))
  (define (breadcrumb-hit w row column)
    (and (eq? (head:window-buffer w) view) (= row 1) (not (string=? location "/"))
         (find (lambda (link) (<= (car link) column (- (cadr link) 1)))
           (directory-links (vector-ref (head:window-lines w) row)))))

  (define (render!)
    (when (and view location (head:app-buffer? view))
      (collect-scan!)
      (head:call-with-display-update
        (lambda ()
          (let ([saved
                 (map (lambda (w) (list w (choice-for w)
                                    (let ([row (at-row (head:window-top w))]) (and row (car row)))))
                   (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))])
            (set! rows (listing))
            (when (and hover (string? (cdr hover)) (not (assoc (cdr hover) rows))) (set! hover #f))
            (let* ([empty (if (pair? rows) '()
                              (list (cond [(not complete?) "Searching…"]
                                          [(positive? failures) "Cannot read directory; Left goes to its parent"]
                                          [(string=? query "") "Empty directory"] [else "No matching files"])))]
                   [all (map (lambda (e) (cons (directory:entry-path e) e)) inventory)]
                   [presentations
                    (map (lambda (saved)
                           (let* ([w (car saved)] [width (head:window-content-width w)])
                             (let-values ([(format-row bounds) (table:layout (columns) sorts (append all rows) cell width)])
                               (choice-columns-set! (cadr saved) bounds)
                               (cons w
                                 (cons* (if (< (head:window-size w) (+ first-row 1)) (glyph:fit "Enlarge pane" width)
                                            (string-append (glyph:fit "Filter: " (min 8 width))
                                              (glyph:fit (display-path query) (max 0 (- width 8)) 'left)))
                                   (glyph:fit (directory-label) width 'left) (format-row #f)
                                   (append (map format-row rows) (map (lambda (s) (glyph:fit s width)) empty))))))) saved)]
                   [placements
                    (apply append
                      (map (lambda (saved)
                             (let* ([w (car saved)] [chosen (keyboard-row w)]
                                    [row (and chosen (row-index (car chosen)))])
                               (when (and complete? chosen) (choice-selected-set! (cadr saved) (car chosen)))
                               (list (cons w (cons (or row first-row) 0))
                                     (cons (cons 'top w) (cons (or (row-index (caddr saved)) first-row) 0))))) saved))])
              (head:view-replace! view
                (cons* (string-append "Filter: " (display-path query)) (directory-label)
                  (string:join (map heading (iota 6)) "  ")
                  (append (map (lambda (row) (string:join (map (lambda (i) (cell row i)) (iota 6)) "  ")) rows) empty))
                (list (cons 'directory location) (cons 'file-filter query) (cons 'file-sorts sorts)
                      (cons 'file-hidden (show-hidden)))
                placements presentations)))))))

  (define (select! row)
    (when row
      (choice-selected-set! (choice-for (head:current-window)) (car row))
      (head:goto! (cons (row-index (car row)) 0))))
  (define (move! delta)
    (let* ([row (candidate (head:current-window))]
           [index (if row (row-index (car row)) (- first-row 1))])
      (set! hover #f)
      (when (pair? rows)
        (select! (at-row (min (+ first-row (length rows) -1) (max first-row (+ index delta))))))))
  (define (navigate! path keep-filter? selected)
    ;; Recall a directory's choice only for the same filter; a fresh query
    ;; must get its own default instead of an old unfiltered container.
    (let ([path (file:canonical (file:expand path))]
          [next-query (if keep-filter? query "")])
      (vector-for-each
        (lambda (choice)
          (when location (hashtable-set! (choice-history choice) location (cons query (choice-selected choice))))
          (choice-selected-set! choice
            (or selected (let ([saved (hashtable-ref (choice-history choice) path #f)])
                           (and saved (string=? (car saved) next-query) (cdr saved))))))
        (hashtable-values choices))
      (set! query next-query)
      (set! location path))
    (when (and selected (string:prefix? "." (file:base-name selected))) (show-hidden #t))
    (set! hover #f)
    (start-scan!))
  (define (up-to! path)
    ;; Remember every branch, including those skipped by a breadcrumb jump,
    ;; so entering it again retraces the route in each window.
    (let branch ([child location])
      (let ([parent (directory:parent child)])
        (vector-for-each (lambda (choice) (hashtable-set! (choice-history choice) parent (cons query child)))
          (hashtable-values choices))
        (if (string=? parent path) (navigate! path #t child) (branch parent)))))
  (define (parent!)
    (unless (string=? location "/")
      (up-to! (directory:parent location))))
  (define (activate! directories-only?)
    (let* ([choice (choice-for (head:current-window))]
           [row (candidate (head:current-window))] [entry (and row (cdr row))]
           ;; A remembered directory can be reentered before its fresh
           ;; inventory arrives. Rapid Right presses then retrace Left
           ;; presses without dropping input or reading disk on this thread.
           [returning (and (not complete?) (not row) (choice-selected choice)
                           (hashtable-contains? (choice-history choice) (choice-selected choice)))]
           [path (if row (car row) (and returning (choice-selected choice)))])
      (when path
        (set! hover #f)
        (cond [(or returning (directory:directory? entry)) (navigate! path #t #f)]
              [(not directories-only?)
               (if (not (eq? (directory:entry-kind entry) 'file))
                   (edit:set-message! "Not a readable regular file; refresh to check for changes")
                   (let* ([focus (head:app-event-focus)]
                          [source (if (and focus (memq focus (head:windows))) focus (head:current-window))]
                          [targets (window:linked 'target source)])
                     (cond
                       [(pair? targets)
                        ;; the pick opens in every target window; this one keeps
                        ;; the files view and the focus
                        (for-each (lambda (w) (head:with-window w (head:call-with-interrupt (lambda () (edit:visit-file! path)))))
                                  targets)]
                       [else
                        (head:set-current! source)
                        (head:call-with-interrupt (lambda () (edit:visit-file! path)))
                        ;; the view was a step to the document, not a stop: it
                        ;; goes behind in the recency list, so C-x b offers the
                        ;; document it replaced
                        (head:set-buffers! (append (remq view (head:buffers)) (list view)))])))]))))
  (define (filter! text)
    ;; A container visible only because descendants match must not keep
    ;; stealing Enter from the filename being typed. Preserve an existing
    ;; choice only when its path still matches and no exact path supersedes it.
    (let ([exact (exact-row text)])
      (vector-for-each
        (lambda (choice)
          (let ([row (assoc (choice-selected choice) rows)])
            (unless (and row (directory:matches? (cdr row) location text)
                         (or (not exact) (equal? (car row) (car exact))))
              (choice-selected-set! choice #f)))) (hashtable-values choices)))
    (set! query text)
    (set! hover #f)
    (start-scan!))
  (define (cycle! column)
    (set! sorts (table:cycle-sort sorts column)) (set! hover #f) (render!))
  (define (sort-event! event)
    (and (member event '("F1" "F2" "F3" "F4" "F5" "F6"))
         (begin (cycle! (- (char->integer (string-ref event 1)) 49)) #t)))
  (define (path-event! event)
    ;; Tab's filesystem lookup and the live metadata must see fresh entries.
    (if (member event '("TAB" "C-r")) (begin (refresh!) (string=? event "C-r")) (sort-event! event)))
  (define (follow-path! input base)
    (let* ([full (file:expand (file:absolute (if (string=? input "~") "~/" input) base))]
           [directory (file:canonical (file:directory-part full))])
      (set! path-part (file:base-name full))
      (unless (string=? directory location)
        (set! location directory) (set! hover #f)
        (vector-for-each (lambda (choice) (choice-selected-set! choice #f)) (hashtable-values choices)))
      (unless (and request (string=? (scan-path request) location)
                   (string=? (scan-query request) "") (eq? (scan-hidden? request) (include-hidden?)))
        (start-scan!))
      (collect-scan!)
      (set! rows (listing))))
  (define (path-lines input w available page base)
    (follow-path! input base)
    (head:buffer-facts-set! (head:window-buffer w)
      (list (cons 'directory location) (cons 'file-filter "") (cons 'file-sorts sorts)))
    (let* ([width (head:window-content-width w)] [size (max 1 (- available first-row))]
           [pages (max 1 (div (+ (length rows) size -1) size))] [page (mod page pages)]
           [from (* page size)] [shown (list-head (list-tail rows from) (min size (- (length rows) from)))])
      (define (path-input path directory?)
        (file:abbreviate (if directory? (file:absolute "" path) path)))
      (define (line row text links)
        (prompt:line text (styles view row text) links (if (< row first-row) 'hover 'candidate-hover)))
      (let-values ([(format-row bounds) (table:layout (columns) sorts rows cell width)])
        (values
          (if (< available (+ first-row 1))
              (if (zero? available) '() (list (line 0 (glyph:fit "Enlarge pane" width) '())))
              (cons* (line 0 (glyph:fit "Filter: " width) '())
                (let ([text (glyph:fit (directory-label) width 'left)])
                  (line 1 text (if (string=? location "/") '()
                                 (map (lambda (link) (list (car link) (cadr link) (path-input (caddr link) #t)))
                                   (directory-links text)))))
                (line 2 (format-row #f)
                  (map (lambda (bound)
                         (list (cadr bound) (caddr bound) (lambda () (cycle! (car bound)) #f))) bounds))
                (if (null? rows)
                    (list (line first-row (glyph:fit (cond [(not complete?) "Searching…"]
                                                       [(positive? failures) "Cannot read directory"]
                                                       [else "No matching files"]) width) '()))
                    (map (lambda (row)
                           (let ([text (format-row row)])
                             (line first-row text
                               (list (list 0 (string-length text) (path-input (car row) (directory:directory? (cdr row)))))))) shown))))
          pages))))
  (define (path!)
    ;; The prompt owns the literal path; the browsing filter is set aside
    ;; and comes back when path entry is cancelled or a file is created.
    ;; A created directory is entered fresh: the filter seeded its path.
    (let ([base location] [saved query] [entered? #f]
          [initial (file:abbreviate (file:absolute query location))])
      (dynamic-wind
        (lambda () (set! query "") (set! path-part "") (set! hover #f))
        (lambda ()
          (parameterize ([prompt:content (prompt:make-content (+ first-row 1)
                                           (lambda (input w height page) (path-lines input w height page base)) path-event!)])
            (edit:prompt-file! (lambda (path) (set! entered? #t) (navigate! path #f #f)) initial)))
        (lambda ()
          (set! path-part #f) (unless entered? (set! query saved)) (set! hover #f)
          (when (and view (memq view (head:buffers)) (head:app-buffer? view)) (start-scan!))))))

  (edoc "Rescan the directory the files view shows.")
  (define (refresh!)
    (when view (start-scan!)) (void))

  (define (handle! event)
    (cond [(string=? event "FOCUS") (render!) #t]
          [(sort-event! event) #t]
          [(member event '("UP" "C-p" "S-TAB" "WHEEL-UP")) (move! -1) #t]
          [(member event '("DOWN" "C-n" "TAB" "WHEEL-DOWN")) (move! 1) #t]
          [(member event '("HOME" "C-a" "M-<")) (move! (- (length rows))) #t]
          [(member event '("END" "C-e" "M->")) (move! (length rows)) #t]
          [(member event '("PAGEUP" "M-v" "PAGEDOWN" "C-v"))
           (move! (* (if (member event '("PAGEUP" "M-v")) -1 1)
                     (max 1 (- (head:window-size (head:current-window)) first-row)))) #t]
          [(string=? event "LEFT") (parent!) #t]
          [(string=? event "RIGHT") (activate! #t) #t]
          [(string=? event "RET") (activate! #f) #t]
          [(member event '("ESC" "C-g"))
           (let ([origin (choice-origin (choice-for (head:current-window)))])
             (set! hover #f)
             (let ([target (if (memq origin (head:buffers)) origin
                               (find (lambda (b) (not (eq? b view))) (head:buffers)))])
               (when target (head:show-buffer! target)))) #t]
          [(string=? event "C-u") (filter! "") #t]
          [(string=? event "C-r") (refresh!) #t]
          [(string=? event "M-c") (path!) #t]
          [(string=? event "M-.") (show-hidden (not (show-hidden))) (filter! query) #t]
          [(member event '("BACKSPACE" "C-h"))
           (if (string=? query "") (parent!)
               (filter! (substring query 0 (- (string-length query) (caar (reverse (glyph:clusters query))))))) #t]
          [(string=? event "PASTE")
           (filter! (string-append query
                      (list->string (filter (lambda (c) (not (eq? (char-general-category c) 'Cc)))
                                      (string->list (head:read-paste)))))) #t]
          [(tty:key-event-character event) => (lambda (c) (filter! (string-append query (string c))) #t)]
          [(string=? event "MOUSE-MOVE")
           (let* ([at (head:app-event-buffer-position)] [row (and at (at-row (car at)))]
                  [column (column-at (head:current-window) at)])
             (set! hover (cond [row (cons (head:current-window) (car row))]
                               [column (cons (head:current-window) (car column))] [else #f]))) #t]
          [(member event '("MOUSE-LEAVE" "BLUR")) (set! hover #f) #t]
          [(member event '("MOUSE-RELEASE" "MOUSE-DRAG")) (select! (keyboard-row (head:current-window))) #t]
          [(string=? event "MOUSE-CLICK")
           (let* ([at (head:app-event-buffer-position)] [row (and at (at-row (car at)))]
                  [breadcrumb (and at (breadcrumb-hit (head:current-window) (car at) (cdr at)))]
                  [column (column-at (head:current-window) at)]
                  ;; Navigation from another pane ends path entry through the
                  ;; prompt's normal focus-loss rule; its input must not undo it.
                  [navigation (if path-part #t 'keep-focus)])
             (cond [row (set! hover #f) (select! row) (activate! #f) navigation]
                   [breadcrumb (up-to! (caddr breadcrumb)) navigation]
                   [column (cycle! (car column)) (set! hover (cons (head:current-window) (car column))) 'keep-focus]
                   [else 'ignore-click]))]
          [else #f]))

  (define (styles b row line)
    (let* ([entry (at-row row)]
           [face (cond [(= row 2) 'header] [(< row 2) 'plain]
                       [(not entry) 'chrome] [else 'plain])]
           [out (make-vector (string-length line) face)])
      (when (or (zero? row) (and (= row 1) (string:prefix? "Directory: " line)))
        (style:fill-range! out 0 (min (vector-length out) (if (zero? row) 8 11)) 'chrome)) out))
  (define (hints)
    (and (eq? (head:current-buffer) view)
         (let ([room (- (head:window-width (head:current-window))
                        head:window-buttons-width
                        (glyph:cells (format "~a▏~a " (head:window-index (head:current-window)) (head:buffer-name view))))])
           (fold-left (lambda (text hint)
                        (if (<= (+ (glyph:cells text) 2 (glyph:cells hint)) room)
                            (string-append text "  " hint) text)) ""
             '("M-c create" "Left parent" "F1–F6 sort" "C-u clear" "M-. hidden" "C-r refresh")))))
  (define (ensure!)
    (unless (and view (memq view (head:buffers)) (head:app-buffer? view))
      (set! view (head:register-app! "*files*" render! handle!))
      (set! location (head:buffer-fact view 'directory (file:canonical (file:expand (head:default-directory)))))
      (set! query (head:buffer-fact view 'file-filter ""))
      (set! sorts (head:buffer-fact view 'file-sorts '()))
      (show-hidden (head:buffer-fact view 'file-hidden (show-hidden)))
      (head:set-app-presentation! view first-row 'auto #f)
      (head:set-app-cursor-visible! view #f)
      (head:set-app-selectable! view #f)
      (head:set-app-status-position! view head:buffer-name)
      (head:buffer-fact-set! view 'resume-kind 'file-view)
      ;; the keys helper lists these under C-x TAB, the app handling them itself
      (head:buffer-fact-set! view 'keys
        '(("RET" "open" "Open the chosen file here, or in the window's target windows; enter a chosen directory")
          ("RIGHT" "enter" "Enter the chosen directory")
          ("LEFT" "parent" "Go up to the parent directory")
          ("UP, DOWN" "move" "Move the choice; PAGEUP and PAGEDOWN by a page, HOME and END to the ends")
          ("text" "filter" "Filter the entries by name, or by path with a slash in the text")
          ("BACKSPACE" "erase" "Erase the filter's last character, or go up when it is empty")
          ("C-u" "clear" "Clear the filter")
          ("M-c" "create" "Create a file or a directory at a typed path")
          ("F1 to F6" "sort" "Sort by a column, again for the other direction")
          ("M-." "hidden" "Show or hide the dot entries")
          ("C-r" "refresh" "Scan the directory again")
          ("ESC, C-g" "return" "Return to the buffer the view replaced")))
      (mode:choose! "files" view))
    view)

  (edoc "Show the files view for the current file's directory, an app's working directory or the head's launch directory, with the current file selected.")
  (define (open!)
    (open-at! (head:default-directory) #f))

  (edoc "Show the files view for a directory, with the current file selected when it is inside."
        (directory directory "the directory to browse"))
  (define (open-directory! directory)
    (unless (string? directory) (error 'open-directory! "expected a directory path" directory))
    (open-at! directory #t))

  (define (open-at! dir explicit?)
    (let* ([was (head:current-buffer)] [selected (head:buffer-file was)])
      (ensure!)
      (unless (eq? was view)
        (hashtable-set! choices (head:current-window) (make-choice was selected '() (make-hashtable string-hash string=?))))
      (head:show-buffer! view)
      (if (and (eq? was view) (not explicit?)) (refresh!)
          (navigate! dir #f selected))) (void))

  (edoc "Install the files app: its mode, the C-x C-f binding, its status hints and buffer-kill hook.")
  (define (init!)
    (mode:register! "files" '() '() (lambda (line) #f) #f styles)
    (keymap:bind-default! "C-x C-f" open!)
    (paint:add-status-hint! hints)
    (head:add-buffer-kill-hook!
      (lambda (b)
        (vector-for-each (lambda (choice)
                           (when (eq? b (choice-origin choice)) (choice-origin-set! choice #f)))
          (hashtable-values choices))
        (when (eq? b view)
          (with-mutex scan-lock (set! request #f))
          (set! view #f) (set! hover #f) (set! inventory '()) (set! rows '())
          (hashtable-clear! choices) (set! resumed-choices '()))))
    (head:add-shutdown-hook! (lambda () (with-mutex scan-lock (set! request #f))))
    (paint:add-highlighter!
      (lambda ()
        (append (paint:hover-ranges breadcrumb-hit)
          (apply append
            (map (lambda (w)
                   (let* ([over (and (head:mouse-position) (over w))]
                          [column (assv over (choice-columns (choice-for w)))]
                          [chosen (candidate w)] [row (and chosen (row-index (car chosen)))])
                     (append
                       (if column (list (list w 2 (cadr column)
                                          (min (caddr column) (+ (cadr column) (string-length (heading (car column))))) 'hover)) '())
                       (if (and row (or (eq? w (head:current-window)) (string? over)))
                         (list (list w row 0 (string-length (vector-ref (head:window-lines w) row))
                                 (if (string? over) 'candidate-hover 'candidate))) '()))))
              (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))))))
    (when (head:find-tool-buffer "*files*") (ensure!) (start-scan!))
    (head:register-resume! 'file-view
      (lambda (b positions)
        (values (list location query sorts (show-hidden) (head:buffer-name b)
                  (fold-right
                    (lambda (w out)
                      (let ([row (keyboard-row w)]) (if row (cons (cons (head:window-index w) (car row)) out) out)))
                    '() (filter (lambda (w) (eq? (head:window-buffer w) b)) (head:windows)))) positions))
      (lambda (reference positions)
        (apply
          (lambda (path filter keys hidden? name selected)
            (unless (and (string? path) (string? filter) (string? name) (boolean? hidden?)
                         (list? keys) (for-all (lambda (key)
                                                 (and (pair? key) (memv (car key) '(0 1 2 3 4 5)) (boolean? (cdr key)))) keys)
                         (list? selected) (for-all (lambda (p)
                                                     (and (pair? p) (integer? (car p)) (string? (cdr p)))) selected))
              (error 'restore "invalid files view descriptor" reference))
            (ensure!)
            (head:buffer-name-set! view name)
            (set! query filter) (set! sorts keys) (show-hidden hidden?)
            (set! resumed-choices selected)
            (navigate! path #t #f)
            (values view positions)) reference)))
    (doc:register!
      '(((file-view:open!) (("procedure" . "(file-view:open!)")) "void"
         ("(apps file-view)") file-view "Files" #f
         "Open `<files>` in this window at the current file's directory; `(file-view:open-directory! path)` starts elsewhere. Type to filter names recursively, or relative paths when the filter contains a slash; Enter opens the selected file or directory. M-c sets the filter aside and opens `<create-file>` with its literal path below a live table of immediate prefix matches. Directory follows input, sorting remains available, and repeated Tab pages the table. Enter creates an empty file on disk or just a directory for a trailing slash, creating missing parents and logging each new path in order. Existing targets are refused. Esc returns to browsing the shown directory with the previous filter. Click ancestor path components to navigate. Browsing preserves the filter exactly: Left selects the directory just left when visible, and Right recalls its selection for the same filter. C-u clears, M-. toggles hidden entries and C-r refreshes. Click headings or use F1–F6 for ordered ascending/descending/off sorting. Small recursive match groups expand; larger groups show counts.")
        ((file-view:expansion-limit) (("parameter" . "(file-view:expansion-limit [count])")) "integer"
         ("(apps file-view)") file-view "Files" #f
         "Maximum descendant matches shown individually for each immediate subdirectory; default 20. Counting continues past this display threshold. Zero collapses all nonempty groups. Refresh after changing this option.")
        ((file-view:show-hidden) (("parameter" . "(file-view:show-hidden [boolean])")) "boolean"
         ("(apps file-view)") file-view "Files" #f
         "Whether files scanning includes dot entries and traverses dot directories; default false. A filter with a path component starting with a dot also includes them. M-. toggles this setting and refreshes the view.")
        ((file-view:refresh!) (("procedure" . "(file-view:refresh!)")) "void"
         ("(apps file-view)") file-view "Files" #f
         "Rescan the files app's current directory with its current filter and options, preserving candidate identities where possible."))))
)
