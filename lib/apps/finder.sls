;; finder.sls -- the local, filterable <finder> app, the directory browser.
(import (only (foundation edoc) elibrary))
(elibrary (apps finder)
  (export choose! (rename (chosen-path chosen)) clear-filter! complete! enter! (rename (listed-entries entries)) erase!
          extend-filter! filter! first-row! init! last-row! (rename (directory-shown location)) next-row! open!
          open-directory! page-down! page-up! parent! paste-filter! previous-row! refresh! return! (rename (select-path! select!))
          show-hidden (rename (sort-order sorts)) toggle-hidden! toggle-sort-column!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation path-filter) path-filter:)
          (prefix (foundation string) string:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head render) render:)
          (prefix (head style) style:)
          (prefix (head table) table:)
          (prefix (head window) window:)
          (prefix (service directory) directory:)
          (prefix (service doc) doc:)
          (prefix (service file) file:)
          (prefix (service file-query) file-query:)
          (prefix (sys glyph) glyph:)
          (prefix (sys sys) sys:)
          (prefix (sys tty) tty:))

  (edoc "Whether the finder lists hidden entries, the dot files."
        (value boolean))
  (define show-hidden (make-parameter #f
                        (lambda (value)
                          (unless (boolean? value) (error 'show-hidden "expected a boolean" value)) value)))

  (define view #f)
  (define location #f)
  (define query #f)
  (define sorts '())
  (define rows '())                 ; (full path . entry)
  (define row-data '#())
  (define row-positions (make-hashtable string-ci-hash string-ci=?))
  (define default-choice #f)
  (define source #f)
  (define shown-query #f)
  (define rendered #f)              ; (source key installed presentations)
  (define cell-cache (make-weak-eq-hashtable))
  (define details (make-hashtable string:hash string=?))
  (define pending-completion? #f)
  (define complete? #f)
  (define match-count 0)
  (define failures 0)
  (define missing-path #f)          ; displayed filter span, computed by the worker
  (define first-row 2)              ; filter and column headings
  (define hover #f)                 ; (window . path/column)
  (define-record-type choice (fields (mutable origin) (mutable selected) (mutable columns) history))
  (define choices (make-weak-eq-hashtable))
  (define resumed-choices '())       ; plain window-number/path pairs from a checkpoint

  ;; One worker per module instance, replacing its pending request. Stale
  ;; results cannot mutate a new query, a killed app or a reloaded module.
  (define-record-type scan
    (fields buffer registration (mutable path) query (mutable keys) hidden? show-hidden? sorts generation (mutable seed) (mutable update) (mutable started?)
            (mutable wanted) (mutable detailed) (mutable details) (mutable finished) (mutable completion) (mutable missing) (mutable proposed) (mutable plan)))
  (define-record-type result (fields inventory rows data positions choice source count index))
  (define scan-lock (make-mutex))
  (define scan-ready (make-condition))
  (define generation 0)
  (define request #f)
  (define running? #f)
  (define (live-request? job)
    (and (with-mutex scan-lock (eq? request job))
         (scan-registration job)
         (eq? (scan-registration job) (head:app-of (scan-buffer job)))))

  (define (directory-prefix path)
    (if (string=? path "/") "/" (string-append path "/")))
  (define (directory-filter path) (file-query:directory-filter path))
  (define (normalize-filter text)
    (cond [(string=? text "") text]
          [(char=? (string-ref text 0) #\/) text]
          [(char=? (string-ref text 0) #\~)
           (string-append (file:expand "~") (string:tail text 1))]
          [(char=? (string-ref text 0) #\")
           (if (and (> (string-length text) 1) (char=? (string-ref text 1) #\/)) text
               (string:insert text 1 "/"))]
          [else (string-append "/" text)]))
  (define (typed-keys text) (file-query:keys text (file:expand "~")))
  (define (key-root keys) (file-query:root keys))
  (define (search-root text) (key-root (typed-keys text)))
  (define (filter-keys text) (file-query:filter-keys (typed-keys text)))
  (define (saved-filter text directory)
    ;; Migrate saved views from the separate-directory model. New views
    ;; already have a rooted first key, or an empty filter rooted at /.
    (let ([keys (typed-keys text)])
      (if (or (and (null? keys) (string=? directory "/"))
              (and (pair? keys) (path-filter:anchored? (car keys)))) text
          (string-append (directory-filter directory) (if (string=? text "") "" (string-append " " text))))))
  (define (query-hidden? text) (or (show-hidden) (file-query:hidden-keys? (typed-keys text))))

  (define (start-scan!)
    (set! location (search-root query))
    (set! complete? #f)
    (set! match-count 0)
    (set! failures 0)
    (set! missing-path #f)
    (let* ([needle query] [keys (filter-keys needle)] [hidden? (query-hidden? query)])
      (with-mutex scan-lock
        ;; Retain the last owned presentation while the worker builds the
        ;; next one. Never refilter a large result under the input lock.
        (unless (and request (string=? (scan-path request) (search-root needle)))
          (clear-rows!) (set! source #f))
        (set! details (make-hashtable string:hash string=?))
        ;; Completing a lone directory enters its unscanned children. Only
        ;; filtered completions preserve the retained result tree.
        (let ([seed (and (pair? keys) request (= generation (scan-generation request))
                         (equal? needle (scan-completion request)) (scan-finished request)
                         (cons (scan-path request) (scan-finished request)))])
          (set! request (make-scan view (head:app-of view) (search-root needle) needle keys hidden? (show-hidden) sorts generation seed #f #f '() #f #f #f #f #f #f #f)))
        (condition-signal scan-ready)))
    (render!))

  (define (scan-work!)
    (let ([cache (directory:make-cache #f)] [seen-generation generation] [existence (make-hashtable equal-hash equal?)])
      (define (exists? path directory?)
        (let* ([key (cons path directory?)] [known (hashtable-ref existence key 'unknown)])
          (if (not (eq? known 'unknown)) known
              (let ([value (guard (ex [else #f]) (if directory? (file-directory? path) (file-exists? path #f)))])
                (hashtable-set! existence key value) value))))
      (define (inspect-filter! job)
        (let* ([text (scan-query job)]
               [plan (file-query:plan text (file:expand "~") (scan-show-hidden? job) exists?)]
               [missing (file-query:plan-missing plan)])
          (scan-plan-set! job plan)
          (scan-path-set! job (file-query:plan-root plan))
          (scan-keys-set! job (file-query:plan-keys plan))
          (scan-proposed-set! job (file-query:plan-proposed plan))
          (scan-missing-set! job (and missing
                                   (cons (string-length (filter-text (substring text 0 (car missing))))
                                     (string-length (filter-text (substring text 0 (cdr missing)))))))))
      (define (completion-inventory job)
        ;; Completion has proved the same matches. Re-root its retained tree
        ;; instead of walking millions of cached nonmatching entries again.
        (let* ([seed (scan-seed job)] [result (cdr seed)] [root (scan-path job)])
          (if (string=? root (car seed)) (result-inventory result)
              (let ([at (find (lambda (i) (string=? root (car (vector-ref (result-data result) (- i first-row)))))
                          (hashtable-ref (result-positions result) root '()))])
                (if at (directory:entry-matches (cdr (vector-ref (result-data result) (- at first-row))))
                    (error 'completion "missing directory in retained matches" root))))))
      (define (load-details! job force?)
        (let ([wanted (with-mutex scan-lock (scan-wanted job))])
          (unless (and (not force?) (eq? wanted (scan-detailed job)))
            (let loop ([rest wanted] [out '()])
              (when (live-request? job)
                (if (pair? rest) (loop (cdr rest) (cons (directory:read! cache (car rest)) out))
                    (begin
                      (with-mutex scan-lock
                        (scan-detailed-set! job wanted) (scan-details-set! job out))
                      (unless (null? out) (head:wake-main!)))))))))
      (define (complete-filter! job)
        (when (and (scan-finished job) (eq? (with-mutex scan-lock (scan-completion job)) 'requested))
          (let* ([plan (scan-plan job)]
                 [next (file-query:complete (result-index (scan-finished job)) plan (scan-query job)
                         (file:expand "~") (scan-show-hidden? job) (lambda () (not (live-request? job))))])
            (when (with-mutex scan-lock
                    (and (eq? request job) (begin (scan-completion-set! job next) #t))) (head:wake-main!)))))
      (dynamic-wind
        void
        (lambda ()
          (let work ([previous #f])
            (let ([job (with-mutex scan-lock request)])
              (define (publish entries skipped done? . failure)
                ;; Wakes carry no callbacks or old inventories. Only the
                ;; current request may publish, including from OS events.
                (load-details! job #f)
                (let ([prepared (prepare-result entries job done?)])
                  (when (and prepared (with-mutex scan-lock
                                        (and (eq? request job)
                                          (begin (when (and done? (zero? skipped)) (scan-finished-set! job prepared))
                                            (scan-update-set! job
                                              (list prepared skipped done? (and (pair? failure) (car failure)))) #t))))
                    (head:wake-main!))))
              (cond
                [(and job (live-request? job))
                 (unless (= seen-generation (scan-generation job))
                   (directory:clear! cache) (hashtable-clear! existence) (set! seen-generation (scan-generation job)))
                 (let ([changed? (directory:poll! cache)])
                   (load-details! job changed?)
                   (when (or changed? (not (eq? previous job)))
                     (with-mutex scan-lock (scan-started?-set! job #t))
                     (guard (ex [else (publish '() 1 #t (kernel:condition-text ex))])
                       (inspect-filter! job)
                       (if (scan-seed job) (begin (publish (completion-inventory job) 0 #t) (scan-seed-set! job #f))
                           (directory:scan! cache (scan-path job) (scan-keys job) (scan-hidden? job)
                             (and (exists (lambda (key) (memv (car key) '(1 2 3 4))) (scan-sorts job)) #t)
                             (lambda () (not (live-request? job))) publish)))))
                 (complete-filter! job)
                 ;; Check the nonblocking event queue while idle, without
                 ;; filesystem walks. New input wakes this wait immediately.
                 (with-mutex scan-lock
                   (when (eq? request job) (condition-wait scan-ready scan-lock (sys:duration 0.1))))
                 (work job)]
                [(with-mutex scan-lock (not (eq? request job))) (work previous)]))))
        (lambda ()
          (directory:close! cache)
          (with-mutex scan-lock (set! running? #f))
          (head:wake-main!)))))

  (define (collect-scan!)
    (let ([job (with-mutex scan-lock request)])
      (when (and job (live-request? job))
        (let ([fresh (with-mutex scan-lock
                       (let ([fresh (scan-details job)]) (scan-details-set! job #f) fresh))])
          (when (pair? fresh)
            (set! details (make-hashtable string:hash string=?))
            (for-each (lambda (entry) (hashtable-set! details (directory:entry-path entry) entry)) fresh)))
        (let ([update (with-mutex scan-lock
                        (let ([update (scan-update job)]) (scan-update-set! job #f) update))])
          (when update
            (apply (lambda (result skipped done? failure)
                     ;; The worker's initial notification is not an empty
                     ;; result. Keep the preview until there is new evidence.
                     (unless (and (not done?) (null? (result-inventory result)))
                       (set! rows (result-rows result)) (set! row-data (result-data result))
                       (set! row-positions (result-positions result))
                       (set! default-choice (result-choice result)) (set! source (result-source result))
                       (set! shown-query (scan-query job)))
                     (set! match-count (result-count result))
                     (set! location (scan-path job))
                     (set! missing-path (scan-missing job))
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
  (define (request-details! saved)
    (when request
      (let ([wanted (make-hashtable string:hash string=?)])
        (for-each
          (lambda (s)
            (let* ([w (car s)] [height (max 1 (head:window-size w))]
                   [chosen (keyboard-row w)] [point (if chosen (row-index (car chosen)) first-row)])
              (for-each (lambda (start)
                          (do ([i (max first-row start) (+ i 1)])
                              ((>= i (min (+ first-row (vector-length row-data)) (+ start height))))
                            (let ([entry (cdr (at-row i))])
                              (unless (or (directory:missing? entry) (directory:entry-mode entry))
                                (hashtable-set! wanted (directory:entry-path entry) entry)))))
                (list (head:window-top w) (max first-row (- point (quotient height 2))))))) saved)
        (demand-details! (vector->list (hashtable-values wanted))))))
  (define (demand-details! wanted)
    (when request
      (with-mutex scan-lock
        (unless (equal? (map directory:entry-path wanted) (map directory:entry-path (scan-wanted request)))
          (scan-wanted-set! request wanted) (condition-signal scan-ready)))))
  (define (at-row row)
    (and (<= first-row row (+ first-row (vector-length row-data) -1)) (vector-ref row-data (- row first-row))))
  (define (row-index path)
    (and path
         (find (lambda (i) (string=? path (car (at-row i))))
           (hashtable-ref row-positions path '()))))
  (define (path-row path)
    (let ([index (row-index path)]) (and index (at-row index))))
  (define (clear-rows!)
    (set! rows '()) (set! row-data '#())
    (set! row-positions (make-hashtable string-ci-hash string-ci=?))
    (set! default-choice #f))
  (define (choice-for w)
    (or (hashtable-ref choices w #f)
        (let* ([saved (assv (head:window-index w) resumed-choices)]
               [state (make-choice #f (and saved (cdr saved)) '() (make-hashtable string:hash string=?))])
          (when saved (set! resumed-choices (remq saved resumed-choices)))
          (hashtable-set! choices w state) state)))
  (define (exact-row text)
    (let* ([keys (typed-keys text)] [text (if (= (length keys) 1) (car keys) "")]
           [n (string-length text)] [dir? (and (positive? n) (char=? (string-ref text (- n 1)) #\/))]
           [path (if dir? (substring text 0 (- n 1)) text)]
           [indices (hashtable-ref row-positions path '())]
           [index (or (row-index path) (and (pair? indices) (car indices)))]
           [row (and index (at-row index))])
      (and row (or (not dir?) (directory:directory? (cdr row))) row)))
  (define (keyboard-row w)
    (or (path-row (choice-selected (choice-for w))) default-choice))
  (define (candidate w)
    (let ([row (or (and (string? (over w)) (path-row (over w))) (keyboard-row w))])
      ;; A previous result may remain as a preview while new input is
      ;; searched. Enter must never open a row excluded by that input.
      (and row (or (equal? shown-query query)
                   (and (not (directory:missing? (cdr row)))
                        (directory:matches? (cdr row) (filter-keys query)))) row)))
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
  (define (label row root)
    (string-append
      (make-string (length (filter (lambda (c) (char=? c #\/))
                             (string->list (directory:relative-path (cdr row) root)))) #\space)
      (display-path (file:base-name (car row)))
      (if (directory:entry-link? (cdr row)) "@" "")
      (if (directory:directory? (cdr row)) "/" "")
      (if (directory:missing? (cdr row)) " [create]" "")))
  (define (raw row column)
    (file-query:value (cdr row) (list-ref '(name size modified created permissions count) column)))
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
  (define (format-cell row column root)
    (let ([value (raw row column)] [entry (cdr row)])
      (cond [(zero? column) (label row root)]
            [(directory:missing? entry) ""]
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
  (define (cell row column)
    (cell-at row column location details))
  (define (cell-at row column root metadata)
    (let* ([entry (if (memv column '(1 2 3 4)) (hashtable-ref metadata (car row) (cdr row)) (cdr row))]
           [known (hashtable-ref cell-cache entry #f)]
           [cells (if (and known (equal? (car known) root)) (cdr known)
                      (let ([cells (make-vector 6 #f)])
                        (hashtable-set! cell-cache entry (cons root cells)) cells))])
      (or (vector-ref cells column)
          (let ([text (format-cell (cons (car row) entry) column root)]) (vector-set! cells column text) text))))
  (define (columns)
    (table:make (vector "Name" "Size" "Modified" "Created" "Permissions"
                  (if (pair? (filter-keys query)) "Matches" "Entries"))
      '#(14 8 16 16 13 9) 0 '(3 4 2 1 5) '#(text right text text text right)))
  (define (heading column) (table:heading (columns) sorts column))
  (define (prepare-result inventory job done?)
    (call/cc
      (lambda (cancel)
        (define steps 0)
        (define (check!)
          (set! steps (+ steps 1))
          (when (and (zero? (mod steps 256)) (not (live-request? job))) (cancel #f)))
        (let* ([root (scan-path job)]
               [plan (scan-plan job)]
               [index (file-query:prepare inventory plan (scan-query job)
                        (map (lambda (key) (list (list-ref '(name size modified created permissions count) (car key))
                                                 (if (cdr key) 'descending 'ascending))) (scan-sorts job)) done? check!)]
               [rows (map (lambda (entry) (cons (directory:entry-path entry) entry)) (vector->list (file-query:index-entries index)))]
               [data (list->vector rows)] [positions (make-hashtable string-ci-hash string-ci=?)])
          (do ([i 0 (+ i 1)]) ((= i (vector-length data)))
            (check!)
            (hashtable-update! positions (car (vector-ref data i))
              (lambda (indices) (append indices (list (+ i first-row)))) '()))
          (and (live-request? job)
            (make-result inventory rows data positions
              (let ([entry (file-query:index-choice index)]) (and entry (cons (directory:entry-path entry) entry)))
              (render:defer (+ first-row (max 1 (vector-length data)))
                (lambda (i)
                  (if (or (< i first-row) (zero? (vector-length data))) ""
                    (let* ([row (vector-ref data (- i first-row))] [entry (cdr row)])
                      (string-append (label row root)
                        (if (and (not (directory:missing? entry)) (directory:directory? entry))
                          (format "  ~a~a" (or (directory:entry-count entry) "?")
                            (if (directory:entry-complete? entry) "" "+")) ""))))))
              (file-query:index-count index) index))))))
  (define (match-ghost)
    (string-append
      (format " [~a~a match~a]" match-count (if (and complete? (zero? failures)) "" "+")
        (if (= match-count 1) "" "es"))
      (if (show-hidden) " [hidden]" "")
      (cond [(not complete?) " Searching…"] [pending-completion? " Completing…"] [else ""])
      (if (positive? failures) (format " [~a unreadable]" failures) "")))
  (define (filter-text text)
    (display-path
      (apply string-append (map (lambda (c) (if (char=? c #\space) " ∧ " (string c))) (string->list text)))))
  (define (filter-label width)
    (let* ([ghost (match-ghost)] [text (filter-text query)]
           [space (max 0 (- width 8 (glyph:cells ghost)))])
      (if (> (glyph:cells text) space)
          (glyph:fit (string-append "Filter: " (glyph:fit text space 'left) ghost) width)
          (glyph:fit (string-append "Filter: " text ghost) width))))
  (define (render!)
    (when (and view location (head:app-buffer? view))
      (let ([saved (map (lambda (w) (list w (choice-for w)
                                      (let ([row (at-row (head:window-top w))]) (and row (car row)))))
                     (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows)))])
        (collect-scan!)
        (request-details! saved)
        (let ([key (list location query sorts complete? failures match-count missing-path pending-completion? (show-hidden) details
                     (map (lambda (s) (let ([w (car s)])
                                        (list w (head:window-content-width w) (head:window-size w)))) saved))])
          ;; Switching buffers retires a window's presentation even when its
          ;; geometry is unchanged. Reuse only rows that are still installed.
          (unless (and rendered (eq? (car rendered) source) (equal? (cadr rendered) key)
                       (for-all (lambda (entry) (eq? (head:window-text (car entry)) (cdr entry)))
                         (caddr rendered)))
            (head:call-with-display-update
              (lambda ()
                (when (and hover (string? (cdr hover)) (not (path-row (cdr hover)))) (set! hover #f))
                (let* ([empty (cond [(not complete?) "Searching…"]
                                    [(positive? failures) "Cannot read directory; Left goes to its parent"]
                                    [(string=? query "") "Empty directory"] [else "No matching files"])]
                       [data row-data] [root location] [metadata details]
                       [front (list (string-append "Filter: " (filter-text query) (match-ghost))
                                (string:join (map heading (iota 6)) "  "))]
                       [text (if (pair? rows) (render:prefix source front) (append front (list empty)))]
                       [presentations
                        (map (lambda (s)
                               (let* ([w (car s)] [width (head:window-content-width w)])
                                 ;; Metadata widths are stable; the identity
                                 ;; column receives the remaining room. Layout
                                 ;; never measures offscreen cells.
                                 (let-values ([(format-row bounds)
                                               (table:layout (columns) sorts (vector width 10 16 16 13 9)
                                                 (lambda (row column) (cell-at row column root metadata)) width)])
                                   (define (format-entry row)
                                     (let ([text (cell-at row 0 root metadata)] [name-width (caddar bounds)])
                                       (if (and (directory:missing? (cdr row)) (> (glyph:cells text) name-width))
                                           ;; Metadata is blank; reserve the action
                                           ;; indicator when shortening the name.
                                           (glyph:fit (string-append
                                                        (glyph:fit (substring text 0 (- (string-length text) 9)) (max 0 (- name-width 9)))
                                                        " [create]") width)
                                           (format-row row))))
                                   (choice-columns-set! (cadr s) bounds)
                                   (let ([headers
                                          (vector (if (< (head:window-size w) (+ first-row 1)) (glyph:fit "Enlarge pane" width)
                                                      (filter-label width))
                                            (format-row #f))])
                                     (cons w (render:defer (+ first-row (max 1 (vector-length data)))
                                               (lambda (i)
                                                 (cond [(< i first-row) (vector-ref headers i)]
                                                       [(zero? (vector-length data)) (glyph:fit empty width)]
                                                       [else (format-entry (vector-ref data (- i first-row)))])))))))) saved)]
                       [placements
                        (apply append
                          (map (lambda (s)
                                 (let* ([w (car s)] [chosen (keyboard-row w)]
                                        [row (and chosen (row-index (car chosen)))])
                                   (when (and complete? chosen) (choice-selected-set! (cadr s) (car chosen)))
                                   (list (cons w (cons (or row first-row) 0))
                                     (cons (cons 'top w) (cons (or (row-index (caddr s)) first-row) 0))))) saved))])
                  (head:view-replace! view text
                    (list (cons 'directory location) (cons 'file-filter query) (cons 'file-sorts sorts)
                      (cons 'file-hidden (show-hidden)))
                    placements presentations)
                  (set! rendered (list source key presentations))))))
          (when pending-completion?
            (let ([answer (with-mutex scan-lock (and request (scan-completion request)))])
              (when (or (string? answer) (and complete? (positive? failures)))
                (set! pending-completion? #f)
                (if (and (string? answer) (not (string=? answer query))) (set-filter! answer) (render!)))))))))

  (define (select-row! row)
    (when row
      (choice-selected-set! (choice-for (head:current-window)) (car row))
      (head:goto! (cons (row-index (car row)) 0))))
  (define (move! delta)
    (let* ([row (candidate (head:current-window))]
           [index (if row (row-index (car row)) (- first-row 1))])
      (set! hover #f)
      (when (pair? rows)
        (select-row! (at-row (min (+ first-row (vector-length row-data) -1) (max first-row (+ index delta))))))))
  (define (navigate! path selected)
    (set! pending-completion? #f)
    ;; Recall a directory's choice only for the same filter; a fresh query
    ;; must get its own default instead of an old unfiltered container.
    (let* ([path (file:canonical (file:expand path))]
           [next-query (directory-filter path)])
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
        (if (string=? parent path) (navigate! path child) (branch parent)))))

  (edoc "Go up to the parent directory, the way back remembered for every window.")
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
        (cond [(or returning (directory:directory? entry))
               (if (and entry (directory:missing? entry))
                   (unless (head:call-with-interrupt (lambda () (edit:visit-file! (directory:filter-path entry)))) (refresh!))
                   (navigate! path #f))]
              [(not directories-only?)
               (if (not (eq? (directory:entry-kind entry) 'file))
                   (edit:set-message! "Not a readable regular file; refresh to check for changes")
                   (let* ([focus (head:app-event-focus)]
                          [source (if (and focus (memq focus (head:windows))) focus (head:current-window))]
                          [targets (window:linked 'target source)])
                     (cond
                       [(pair? targets)
                        ;; the pick opens in every target window; this one keeps
                        ;; the finder and the focus
                        (for-each (lambda (w) (head:with-window w (head:call-with-interrupt (lambda () (edit:visit-file! path)))))
                                  targets)]
                       [else
                        (head:set-current! source)
                        (head:call-with-interrupt (lambda () (edit:visit-file! path)))
                        ;; the view was a step to the document, not a stop: it
                        ;; goes behind in the recency list, so C-x b offers the
                        ;; document it replaced
                        (head:set-buffers! (append (remq view (head:buffers)) (list view)))])))
               (when (directory:missing? entry) (refresh!))]))))

  (edoc "Set the complete filter. The first token is an absolute leading path whose nearest existing parent roots the table; missing components become creation rows. Further keys match disjoint path fragments. Empty input lists root."
        (text string "the filter"))
  (define (filter! text)
    (unless (string? text) (error 'filter! "expected filter text" text))
    (set-filter! (normalize-filter text)))

  (define (set-filter! text)
    ;; A container visible only because descendants match must not keep
    ;; stealing Enter from the filename being typed. Preserve an existing
    ;; choice only when its path still matches and no exact path supersedes it.
    (let ([exact (exact-row text)])
      (vector-for-each
        (lambda (choice)
          (let ([row (path-row (choice-selected choice))])
            (unless (and row (not (directory:missing? (cdr row))) (directory:matches? (cdr row) (filter-keys text))
                         (or (not exact) (equal? (car row) (car exact))))
              (choice-selected-set! choice #f)))) (hashtable-values choices)))
    (set! query text)
    (set! pending-completion? #f)
    (set! hover #f)
    (start-scan!))
  (define (cycle! column)
    (set! sorts (table:cycle-sort sorts column)) (set! hover #f) (start-scan!))

  (edoc "Clear the finder's filesystem cache and rescan the current directory.")
  (define (refresh!)
    (set! generation (+ generation 1))
    (when view (start-scan!)) (void))

  (define (page) (max 1 (- (head:window-size (head:current-window)) first-row)))

  ;;; The app as an API: what M-x or an agent asks and does --------------------------

  (edoc "The directory the finder shows, or #f before it opens."
        (returns (or string #f)))
  (define (directory-shown) location)

  (edoc "The listed paths in display order, described as (directory path) or (file path), including missing paths offered for creation."
        (returns list))
  (define (listed-entries)
    (map (lambda (row) (list (if (directory:directory? (cdr row)) 'directory 'file) (car row))) rows))

  (edoc "The chosen entry's path in the current window, or #f without one."
        (returns (or string #f)))
  (define (chosen-path)
    ;; read without making a window's choice record, as candidate would
    (let* ([w (head:current-window)] [choice (hashtable-ref choices w #f)]
           [row (or (and (string? (over w)) (path-row (over w)))
                    (and choice (path-row (choice-selected choice)))
                    default-choice)])
      (and row (car row))))

  (edoc "Make a listed entry the choice in the current window, by its path, absolute or relative to the directory shown; refused when none is listed under it."
        (path string "the entry's path"))
  (define (select-path! path)
    (let ([row (or (path-row (file:canonical (file:expand (file:absolute path location)))) (exact-row path))])
      (unless row (error 'select! "no such entry is listed" path))
      (select-row! row)))

  (edoc "The sort order as data, the columns sorted by, first first, each (column . descending?), the columns 1 to 6."
        (returns list))
  (define (sort-order)
    (map (lambda (s) (cons (+ (car s) 1) (cdr s))) sorts))

  (edoc "Open the chosen file in this window, or its target windows; enter a chosen directory. Missing choices are created through edit:visit-file!, including their parents.")
  (define (choose!) (activate! #f))

  (edoc "Enter the chosen directory, creating it and its missing parents when needed; a chosen file stays where it is.")
  (define (enter!) (activate! #t))

  (edoc "Move the choice to the next entry.")
  (define (next-row!) (move! 1))

  (edoc "Move the choice to the previous entry.")
  (define (previous-row!) (move! -1))

  (edoc "Move the choice a page of entries down.")
  (define (page-down!) (move! (page)))

  (edoc "Move the choice a page of entries up.")
  (define (page-up!) (move! (- (page))))

  (edoc "Move the choice to the first entry.")
  (define (first-row!) (move! (- (vector-length row-data))))

  (edoc "Move the choice to the last entry.")
  (define (last-row!) (move! (vector-length row-data)))

  (edoc "Erase the filter's last character, updating the directory and matches as its path changes.")
  (define (erase!)
    (unless (string=? query "")
      (filter! (substring query 0 (- (string-length query) (caar (reverse (glyph:clusters query))))))))

  (edoc "Clear the entire filter, showing the root directory's immediate children.")
  (define (clear-filter!) (filter! ""))

  (edoc "Complete a lone path to its unique directory with a trailing slash, listing its children. Otherwise expand to a longest filter with the same matches and the fewest keys. The worker waits for a complete scan, and further typing cancels completion.")
  (define (complete!)
    (when request
      (if (and complete? (positive? failures))
          (edit:set-message! "Cannot complete an unreadable search; refresh after correcting it")
          (begin
            (set! pending-completion? #t)
            (with-mutex scan-lock (scan-completion-set! request 'requested) (condition-signal scan-ready))
            (render!)))))

  (edoc "Add text to the filter, as typing does: SELF-INSERT, any character, runs it with the character typed."
        (text string "the text to add"))
  (define (extend-filter! text) (filter! (string-append query text)))

  (edoc "Sort the entries by a column, the same column again reversing the order: 1 name, 2 size, 3 modified, 4 created, 5 permissions, 6 the entry or match count; F1 to F6 sort by the column of their number."
        (column integer "the column, 1 to 6"))
  (define (toggle-sort-column! column)
    (unless (and (integer? column) (exact? column) (<= 1 column 6))
      (error 'toggle-sort-column! "expected a column, 1 to 6" column))
    (cycle! (- column 1)))

  (edoc "Show the dot entries, or hide them again.")
  (define (toggle-hidden!)
    (show-hidden (not (show-hidden)))
    (filter! query))

  (edoc "Return to the buffer the finder replaced in this window.")
  (define (return!)
    (let ([origin (choice-origin (choice-for (head:current-window)))])
      (set! hover #f)
      (let ([target (if (memq origin (head:buffers)) origin
                        (find (lambda (b) (not (eq? b view))) (head:buffers)))])
        (when target (head:show-buffer! target)))))

  (edoc "Add the pasted text to the filter, control characters dropped.")
  (define (paste-filter!)
    (filter! (string-append query
               (list->string (filter (lambda (c) (not (eq? (char-general-category c) 'Cc)))
                               (string->list (head:read-paste)))))))

  ;; The keys of the finder, bound in its mode's context to the commands
  ;; above, so the keys helper lists them and C-h k describes them
  (define finder-keys
    `((("RET") ,choose!) (("RIGHT") ,enter!) (("LEFT") ,parent!)
      (("DOWN" "C-n") ,next-row!) (("UP" "C-p" "S-TAB") ,previous-row!) (("TAB") ,complete!)
      (("PGDN" "C-v") ,page-down!) (("PGUP" "M-v") ,page-up!)
      (("HOME" "C-a" "M-<") ,first-row!) (("END" "C-e" "M->") ,last-row!)
      (("BS" "C-h") ,erase!) (("C-u") ,clear-filter!)
      (("M-.") ,toggle-hidden!) (("C-r") ,refresh!) (("ESC" "C-g") ,return!) (("PASTE") ,paste-filter!)
      (("SELF-INSERT") ,(keymap:call extend-filter! head:typed-text))
      ;; the function keys sort by the column of their number
      ,@(map (lambda (n) (list (list (format "F~a" n)) (keymap:call toggle-sort-column! n))) '(1 2 3 4 5 6))))

  (define (handle! event)
    ;; what the finder context leaves to the app: focus, the wheel and the
    ;; pointer; typing grows the filter through the context's SELF-INSERT
    (cond [(string=? event "FOCUS") (render!) #t]
          [(member event '("WHEEL-UP" "WHEEL-DOWN" "S-WHEEL-UP" "S-WHEEL-DOWN"))
           (set! hover #f)
           (edit:page-window! (if (member event '("WHEEL-UP" "S-WHEEL-UP")) -1 1) 8)
           ;; Keep the app's choice at the scroller's landing point, so a
           ;; refresh does not restore the old choice and pull the view back.
           (select-row! (at-row (max first-row (car (head:point))))) #t]
          [(string=? event "MOUSE-MOVE")
           (let* ([at (head:app-event-buffer-position)] [row (and at (at-row (car at)))]
                  [column (column-at (head:current-window) at)])
             (set! hover (cond [row (cons (head:current-window) (car row))]
                               [column (cons (head:current-window) (car column))] [else #f]))) #t]
          [(member event '("MOUSE-LEAVE" "BLUR")) (set! hover #f) #t]
          [(member event '("MOUSE-RELEASE" "MOUSE-DRAG")) (select-row! (keyboard-row (head:current-window))) #t]
          [(string=? event "MOUSE-CLICK")
           (let* ([at (head:app-event-buffer-position)] [row (and at (at-row (car at)))]
                  [column (column-at (head:current-window) at)])
             (cond [row (set! hover #f) (select-row! row) (activate! #f) 'keep-focus]
                   [column (cycle! (car column)) (set! hover (cons (head:current-window) (car column))) 'keep-focus]
                   [else 'ignore-click]))]
          [else #f]))

  (define (styles b row line)
    (let* ([entry (at-row row)]
           [face (cond [(= row (- first-row 1)) 'header] [(< row first-row) 'plain]
                       [(not entry) 'chrome] [else 'plain])]
           [out (make-vector (string-length line) face)])
      (when entry
        ;; Match literal paths before mapping to escaped, indented labels.
        ;; Only their visible prefix is styled, never an elision or metadata.
        (let* ([keys (typed-keys query)] [item (cdr entry)]
               [full (directory:filter-path item)]
               [path full]
               [ranges (path-filter:ranges keys path (directory:directory? item))]
               [name (file:base-name (car entry))]
               [start (- (string-length path) (string-length name) (if (directory:directory? item) 1 0))]
               [label (label entry location)]
               [visible (string-length (string:common-prefix (list label line)))]
               [indent (- (string-length label) (string-length (display-path name))
                          (if (directory:missing? item) 9 0)
                          (if (directory:entry-link? item) 1 0) (if (directory:directory? item) 1 0))])
          (when (directory:missing? item)
            (style:fill-range! out indent (min (- (string-length label) 9) visible) '(plain italic))
            (let trim ([end (string-length line)])
              (cond [(and (positive? end) (char=? (string-ref line (- end 1)) #\space)) (trim (- end 1))]
                    [(and (>= end 9) (string=? (substring line (- end 9) end) " [create]"))
                     (style:fill-range! out (- end 9) end 'ghost)])))
          (let mark ([i 0] [at indent])
            (when (< i (+ (string-length name) (if (directory:directory? item) 1 0)))
              (let* ([slash? (= i (string-length name))]
                     [at (+ at (if (and slash? (directory:entry-link? item)) 1 0))]
                     [size (if slash? 1 (string-length (display-path (string (string-ref name i)))))]
                     [end (+ at size)])
                (when (and (<= end visible) (exists (lambda (range) (<= (car range) (+ start i) (- (cdr range) 1))) ranges))
                  (style:fill-range! out at end (if (directory:missing? item) '(plain italic mark) '(plain mark))))
                (mark (+ i 1) end))))))
      (when (zero? row)
        (style:fill-range! out 0 (min (vector-length out) 8) 'chrome))
      (when (zero? row)
        (let* ([ghost (match-ghost)]
               [end (let trim ([i (string-length line)])
                      (if (and (positive? i) (char=? (string-ref line (- i 1)) #\space)) (trim (- i 1)) i))]
               [start (- end (string-length ghost))])
          (when (and (>= start 8) (string=? (substring line start end) ghost))
            (style:fill-range! out start end 'ghost)
            (when missing-path
              (let* ([text (filter-text query)] [shown (substring line 8 start)]
                     [n (string-length text)] [size (string-length shown)])
                (define (mark shift first last)
                  (let ([from (max first (+ shift (car missing-path)))]
                        [to (min last (+ shift (cdr missing-path)))])
                    (when (< from to) (style:fill-range! out from to '(plain italic)))))
                (cond [(string=? text shown) (mark 8 8 start)]
                      [(and (positive? size) (char=? (string-ref shown 0) #\…))
                       ;; Left elision may pad a clipped wide glyph. Map only
                       ;; the retained suffix, excluding that pad and ellipsis.
                       (let suffix ([end size])
                         (when (positive? end)
                           (if (and (<= (- end 1) n)
                                    (string=? (substring shown 1 end) (string:tail text (- n (- end 1)))))
                               (mark (- 9 (- n (- end 1))) 9 (+ 8 end))
                               (suffix (- end 1)))))])))))) out))
  (define (ensure!)
    (unless (and view (memq view (head:buffers)) (head:app-buffer? view))
      (set! view (head:register-app! "*finder*" render! handle!))
      (set! location (head:buffer-fact view 'directory (file:canonical (file:expand (head:default-directory)))))
      (set! query (saved-filter (head:buffer-fact view 'file-filter (or query (directory-filter location))) location))
      (set! sorts (head:buffer-fact view 'file-sorts '()))
      (show-hidden (head:buffer-fact view 'file-hidden (show-hidden)))
      (head:set-app-presentation! view first-row 'auto #f)
      (head:set-app-cursor-visible! view #f)
      (head:set-app-selectable! view #f)
      (head:set-app-status-position! view (lambda (b) ""))   ; the name alone
      (head:buffer-fact-set! view 'resume-kind 'finder)
      (mode:choose! "finder" view))
    view)

  (edoc "Reopen the finder with its directory and filter intact; on first use, start at the current file's directory, an app's working directory or the head's launch directory.")
  (define (open!)
    (open-at! (head:default-directory) #f))

  (edoc "Reset the finder filter to a directory's full path, with the current file selected when it is inside."
        (directory directory "the directory to browse"))
  (define (open-directory! directory)
    (unless (string? directory) (error 'open-directory! "expected a directory path" directory))
    (open-at! directory #t))

  (define (open-at! dir explicit?)
    (let* ([was (head:current-buffer)] [selected (head:buffer-file was)]
           [existing? (and view (memq view (head:buffers)) (head:app-buffer? view))])
      (ensure!)
      (unless (eq? was view)
        (hashtable-set! choices (head:current-window) (make-choice was selected '() (make-hashtable string:hash string=?))))
      (head:show-buffer! view)
      (if (and existing? (not explicit?)) (render!)
          (navigate! dir selected))) (void))

  (edoc "Install the finder: its mode with its keys bound in the finder context, the C-x C-f binding and its buffer-kill hook; C-x TAB lists the keys.")
  (define (init!)
    (head:set-directory-opener!
      (lambda (path) (set! generation (+ generation 1)) (open-directory! path)))
    (mode:register! "finder" '() '() (lambda (line) #f) #f styles)
    (keymap:bind-default! "C-x C-f" open!)
    (for-each (lambda (entry) (for-each (lambda (key) (keymap:bind-default! 'finder key (cadr entry))) (car entry))) finder-keys)
    (head:add-buffer-kill-hook!
      (lambda (b)
        (vector-for-each (lambda (choice)
                           (when (eq? b (choice-origin choice)) (choice-origin-set! choice #f)))
          (hashtable-values choices))
        (when (eq? b view)
          (with-mutex scan-lock (set! request #f) (condition-signal scan-ready))
          (set! view #f) (set! hover #f) (clear-rows!) (set! source #f) (set! rendered #f)
          (hashtable-clear! choices) (set! resumed-choices '()))))
    (head:add-shutdown-hook! (lambda () (with-mutex scan-lock (set! request #f) (condition-signal scan-ready))))
    (paint:add-highlighter!
      (lambda ()
        (apply append
          (map (lambda (w)
                 (let* ([over (and (head:mouse-position) (over w))]
                        [column (assv over (choice-columns (choice-for w)))]
                        [chosen (candidate w)] [row (and chosen (row-index (car chosen)))])
                   (append
                     (if column (list (list w (- first-row 1) (cadr column)
                                        (min (caddr column) (+ (cadr column) (string-length (heading (car column))))) 'hover)) '())
                     (if (and row (or (eq? w (head:current-window)) (string? over)))
                         (list (list w row 0 (string-length (head:window-line w row))
                                 (if (string? over) 'candidate-hover 'candidate))) '()))))
               (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows))))))
    (when (head:find-tool-buffer "*finder*") (ensure!) (start-scan!))
    (head:register-resume! 'finder
      (lambda (b positions)
        (values (list location query sorts (show-hidden) (head:buffer-name b)
                  (fold-right
                    (lambda (w out)
                      (let ([row (keyboard-row w)]) (if row (cons (cons (head:window-index w) (car row)) out) out)))
                    '() (filter (lambda (w) (eq? (head:window-buffer w) b)) (head:windows)))
                ) positions))
      (lambda (reference positions)
        (apply
          (lambda (path filter keys hidden? name selected . saved-mode)
            (unless (and (string? path) (string? filter) (string? name) (boolean? hidden?)
                         (or (null? saved-mode) (and (= (length saved-mode) 1) (memq (car saved-mode) '(prefix fuzzy deep))))
                         (list? keys) (for-all (lambda (key)
                                                 (and (pair? key) (memv (car key) '(0 1 2 3 4 5)) (boolean? (cdr key)))) keys)
                         (list? selected) (for-all (lambda (p)
                                                     (and (pair? p) (integer? (car p)) (string? (cdr p)))) selected))
              (error 'restore "invalid finder descriptor" reference))
            (ensure!)
            (head:buffer-name-set! view name)
            (set! query (saved-filter filter path)) (set! sorts keys) (show-hidden hidden?)
            (set! resumed-choices selected)
            (start-scan!)
            (values view positions)) reference)))
    (doc:register!
      '(((finder:open!) (("procedure" . "(finder:open!)")) "void"
         ("(apps finder)") finder "Finder" #f
         "Reopen `<finder>` with its last filter, initially the current directory's full path. The first token is an absolute leading path; its directory portion determines the table's root. Add literal keys after spaces to search recursively in any order, without overlapping. Spaces appear as ∧, matching path fragments are underlined, and missing leading-path components use normal-color italics. Backspace edits the path; C-u clears everything and lists root. Enter on a directory replaces the filter with its full path ending in /; `(finder:open-directory! path)` does the same. Tab completes a lone path to its unique directory with a trailing slash; other completions preserve the match set while maximizing literal characters minus separating spaces. Scanning, path checks, sorting and completion run in the background. Cached listings are reused until C-r; filesystem watches are disabled. Missing components of the leading path appear as italic [create] rows under the nearest existing directory. Choose one to create it and its parents; a trailing slash requests a directory. M-. toggles hidden entries. Click headings or use F1–F6 for ordered ascending/descending/off sorting among siblings.")
        ((finder:show-hidden) (("parameter" . "(finder:show-hidden [boolean])")) "boolean"
         ("(apps finder)") finder "Finder" #f
         "Whether the finder's scan includes dot entries and traverses dot directories; default false. A filter with a path component starting with a dot also includes them. M-. toggles this setting and refreshes the view.")
        ((finder:refresh!) (("procedure" . "(finder:refresh!)")) "void"
         ("(apps finder)") finder "Finder" #f
         "Clear all cached directory listings and metadata, then rescan the current directory with its current filter and options, preserving candidate identities where possible."))))
)
