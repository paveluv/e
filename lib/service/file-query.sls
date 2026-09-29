;; Filesystem query semantics shared by base providers and the migrating Finder.
(import (only (foundation edoc) elibrary))
(elibrary (service file-query)
  (export complete directory-filter filter-keys index-choice index-count index-entries
          keys plan plan-hidden? plan-keys plan-missing plan-proposed plan-root prepare root value)
  (import (chezscheme) (prefix (core row) row:)
          (prefix (foundation path-filter) path-filter:) (prefix (foundation string) string:)
          (prefix (service directory) directory:) (prefix (service file) file:))

  (define (path-prefix path) (if (string=? path "/") "/" (string-append path "/")))

  (edoc "Spell an absolute directory as a single quoted-if-needed filter token ending in slash."
        (path string "canonical directory") (returns string))
  (define (directory-filter path) (path-filter:format-keys (list (path-prefix path))))

  (edoc "Parse path keys with an explicit home context and normalize rooted directory components."
        (text string "filter text") (home string "absolute home") (returns list))
  (define (keys text home)
    (map (lambda (key)
           (if (path-filter:anchored? key)
             (string-append (path-prefix (file:canonical (file:directory-part key))) (file:base-name key)) key))
      (path-filter:parse text home)))

  (edoc "The directory implied by the first rooted key, before checking existence."
        (keys list "expanded keys") (returns string))
  (define (root keys)
    (if (and (pair? keys) (path-filter:anchored? (car keys))) (file:canonical (file:directory-part (car keys))) "/"))

  (edoc "The matching keys; a lone directory token denotes an immediate-child overview."
        (keys list "expanded keys") (returns list))
  (define (filter-keys keys)
    (if (and (= (length keys) 1) (string=? (car keys) (path-prefix (root keys)))) '() keys))

  (edoc "A resolved filesystem query."
        (root string "nearest existing directory") (keys list "expanded matching keys")
        (hidden? boolean "effective visibility") (missing any "raw character span or false") (proposed any "creation tree or false"))
  (define-record-type (query-plan make-plan plan?)
    (fields (immutable root plan-root) (immutable keys plan-keys) (immutable hidden? plan-hidden?)
            (immutable missing plan-missing) (immutable proposed plan-proposed)))

  (edoc "Resolve a filter's existing root and optional creation hierarchy. Missing spans use raw filter character positions, independent of display. Existence reads belong to the caller's inventory worker."
        (text string "filter") (home string "absolute home") (hidden? boolean "explicit hidden option")
        (exists? procedure "path, directory? -> boolean") (returns any))
  (define (plan text home hidden? exists?)
    (let* ([parsed (path-filter:parse text home)] [expanded (keys text home)]
           [base (root expanded)] [matching (filter-keys expanded)] [missing #f] [proposed #f])
      (when (and (pair? parsed) (path-filter:anchored? (car parsed)))
        (let* ([path (car parsed)] [n (string-length path)])
          (define (raw-index at)
            (cond
              [(string:prefix? "~" text) (+ 1 (- at (string-length home)))]
              [(not (string:prefix? "\"" text)) at]
              [else
               (let loop ([i 1])
                 (if (= i (string-length text)) i
                   (let ([part (guard (ex [else #f]) (read (open-input-string (string-append (substring text 0 i) "\""))))])
                     (if (and (string? part) (string=? part (substring path 0 at))) i (loop (+ i 1))))))]))
          (let walk ([i 1] [start 1])
            (cond [(> i n) (void)]
              [(or (= i n) (char=? (string-ref path i) #\/))
               (if (exists? (substring path 0 i) (or (< i n) (string:suffix? "/" path)))
                 (walk (+ i 1) (+ i 1))
                 (let ([full (car expanded)])
                   (unless (exists? full (string:suffix? "/" full))
                     (set! base (let parent ([path base])
                                  (if (or (string=? path "/") (exists? path #t)) path (parent (directory:parent path)))))
                     (set! matching expanded)
                     (set! missing (cons (max 0 (raw-index start)) (raw-index n)))
                     (set! proposed
                       (let build ([from (string-length (path-prefix base))])
                         (and (< from (string-length full))
                           (let* ([slash (string:search full "/" from (string-length full))]
                                  [child (and slash (build (+ slash 1)))])
                             (directory:make-missing (substring full 0 (or slash (string-length full)))
                               (and slash #t) (if child (list child) '())))))))))]
              [else (walk (+ i 1) start)]))))
      (make-plan base matching hidden? missing proposed)))

  (edoc "Read a raw sortable entry fact; unavailable facts remain false."
        (entry any "directory entry") (column symbol "name, size, modified, created, permissions or count") (returns any))
  (define (value entry column)
    (case column
      [(name) (directory:entry-path entry)]
      [(size) (and (not (directory:directory? entry)) (directory:entry-size entry))]
      [(modified) (directory:entry-modified entry)] [(created) (directory:entry-created entry)]
      [(permissions) (directory:entry-mode entry)] [(count) (directory:entry-count entry)]))

  (edoc "A prepared filesystem ordering."
        (entries vector "hierarchical entry order") (choice any "suggested entry or false") (count integer "actual matches"))
  (define-record-type index (fields entries choice count))

  (edoc "Prepare a hierarchical sibling-sorted index with exact path identities, match count and default choice. No display geometry or formatted metadata is retained."
        (inventory list "immutable directory tree") (plan any "resolved query") (text string "filter text")
        (sort list "compound column/direction keys") (complete? boolean "scan finished") (check! thunk "cancellation/yield checkpoint")
        (returns any))
  (define (prepare inventory plan text sort complete? check!)
    (define match? (path-filter:matcher (plan-keys plan)))
    (define (less? a b)
      (check!)
      (let compare ([rest sort])
        (if (null? rest)
          (let ([a (directory:entry-path a)] [b (directory:entry-path b)])
            (or (string-ci<? a b) (and (string-ci=? a b) (string<? a b))))
          (let* ([column (caar rest)] [x (value a column)] [y (value b column)]
                 [x (and x (cons column x))] [y (and y (cons column y))])
            (cond [(row:less? x y) (eq? (cadar rest) 'ascending)]
              [(row:less? y x) (eq? (cadar rest) 'descending)] [else (compare (cdr rest))])))))
    (define (tree entries tail)
      (fold-right (lambda (entry rest) (check!) (cons entry (tree (directory:entry-matches entry) rest))) tail
        (append (list-sort less? (filter directory:directory? entries))
          (list-sort less? (filter (lambda (e) (not (directory:directory? e))) entries)))))
    (let* ([proposed (plan-proposed plan)]
           [inventory (if (and proposed (not (exists (lambda (e) (string=? (directory:entry-path e) (directory:entry-path proposed))) inventory)))
                        (cons proposed inventory) inventory)]
           [shown (filter (lambda (e)
                            (check!)
                            (or (directory:missing? e) (match? (directory:filter-path e))
                              (and (directory:directory? e)
                                (or (let ([relative (string-append (directory:relative-path e (plan-root plan)) "/")])
                                      (exists (lambda (key) (string:prefix? (string:fold-case relative) (string:fold-case key)))
                                        (plan-keys plan)))
                                  (and (directory:entry-count e) (positive? (directory:entry-count e)))
                                  (and (not (directory:entry-link? e)) (or complete? (not (directory:entry-count e)))
                                    (not (directory:entry-complete? e))))))) inventory)]
           [entries (list->vector (tree shown '()))] [count 0]
           [exact #f] [folded #f] [file #f] [first #f] [creation #f])
      (vector-for-each
        (lambda (entry)
          (check!)
          (let* ([path (directory:entry-path entry)] [relative (directory:relative-path entry (plan-root plan))]
                 [spellings (list path relative (directory:filter-path entry)
                              (if (directory:directory? entry) (string-append relative "/") relative))])
            (if (directory:missing? entry) (set! creation entry)
              (begin
                (unless first (set! first entry))
                (when (member text spellings) (set! exact entry))
                (when (and (not folded) (exists (lambda (s) (string-ci=? s text)) spellings)) (set! folded entry))
                (when (and (not file) (not (directory:directory? entry))) (set! file entry))
                (when (match? (directory:filter-path entry)) (set! count (+ count 1))))))) entries)
      (make-index entries (or exact folded (and (pair? (plan-keys plan)) file) first creation) count)))

  (edoc "Complete against a prepared readable match set while preserving scope, hidden policy and case-distinct paths. A unique lone directory may gain its trailing slash."
        (index any "prepared index") (plan any "resolved query") (text string "original filter")
        (home string "absolute home")
        (cancelled? thunk "cancellation/yield checkpoint") (returns string))
  (define (complete index plan text home cancelled?)
    (let* ([base (plan-root plan)] [prefix-size (if (string=? base "/") 0 (string-length base))]
           [full-keys (keys text home)]
           [keys (if (pair? full-keys) (cons (string:tail (car full-keys) prefix-size) (cdr full-keys)) '())]
           [entries (index-entries index)] [match? (path-filter:matcher (plan-keys plan))]
           [common #f] [example #f] [matching-directories (make-hashtable string-ci-hash string-ci=?)] [only #f])
      (define (spell parts)
        (if (and example (pair? parts) (path-filter:anchored? (car parts)))
          (cons (substring example 0 (+ prefix-size (string-length (car parts)))) (cdr parts))
          (if (pair? parts) (cons (string-append (substring base 0 prefix-size) (car parts)) (cdr parts)) parts)))
      (define (walk visit)
        (let loop ([i 0])
          (or (= i (vector-length entries))
            (and (or (not (zero? (mod i 256))) (not (cancelled?)))
              (let* ([entry (vector-ref entries i)] [path (directory:filter-path entry)])
                (and (or (directory:missing? entry) (not (match? path))
                       (begin
                         (set! only entry)
                         (when (directory:directory? entry) (hashtable-set! matching-directories (directory:entry-path entry) #t))
                         (unless example (set! example path))
                         (set! common (if common (string:common-prefix (list common path)) path))
                         (visit (string:tail path prefix-size))))
                  (loop (+ i 1))))))))
      (when (and (= (length full-keys) 1) (= (index-count index) 1)) (walk (lambda (path) #t)))
      (if (and (= (length full-keys) 1) (= (index-count index) 1) only (directory:directory? only))
        (directory-filter (directory:entry-path only))
        (path-filter:format-keys
          (spell (path-filter:complete keys walk
                   (lambda (parts)
                     (and (or (null? parts) (path-filter:anchored? (car parts)))
                       (or (not common) (string:prefix? (path-prefix (root (spell parts))) common))
                       (not (hashtable-contains? matching-directories (root (spell parts)))))) cancelled?)))))))
