;; directory.sls -- filesystem inventory and bounded recursive match groups.
;; No head state or threads: the caller owns cancellation and publication.
(import (only (foundation edoc) elibrary))
(elibrary (service directory)
  (export directory? entry-complete? entry-count entry-created entry-kind entry-link?
          entry-matches entry-mode entry-modified entry-path entry-size matches?
          (rename (parent-path parent)) reconcile refilter relative-path scan)
  (import (chezscheme)
          (prefix (foundation string) string:)
          (prefix (service file) file:)
          (prefix (sys sys) sys:))

  (edoc "A file system entry as the files view lists it."
        (path string "the absolute path")
        (kind symbol "file, directory or another kind")
        (link? boolean "whether it is a symbolic link")
        (mode (or integer #f) "the permission bits")
        (size (or integer #f) "the size in bytes")
        (modified (or number #f) "the modification time")
        (created (or number #f) "the creation time")
        (count (or integer #f) "how many descendants match, when known")
        (complete? boolean "whether the count is exact")
        (matches list "the retained child entries, including the directories leading to matches"))
  (define-record-type entry
    (fields path kind link? mode size modified created count complete? matches))

  (edoc "The canonical parent of a directory path."
        (path directory "the directory")
        (returns directory))
  (define (parent-path path)
    (file:canonical (string-append path "/..")))

  (edoc "Whether an entry is a directory."
        (entry (record entry) "the entry")
        (returns boolean))
  (define (directory? entry)
    (eq? (entry-kind entry) 'directory))

  (edoc "An entry's path relative to a root directory."
        (entry (record entry) "the entry")
        (root directory "the root")
        (returns string))
  (define (relative-path entry root)
    (string:tail (entry-path entry) (if (string=? root "/") 1 (+ 1 (string-length root)))))

  (define (path-query? query)
    (and (string:search query "/" 0 (string-length query)) #t))

  (define (matcher root query)
    ;; A filter with a slash matches the path relative to root; one without
    ;; matches only the entry's own name, so a matching ancestor does not
    ;; claim every descendant. Directories carry their trailing slash.
    (let ([path? (path-query? query)])
      (lambda (entry)
        (let ([name (string-append (if path? (relative-path entry root) (file:base-name (entry-path entry)))
                      (if (directory? entry) "/" ""))])
          (and (string:search name query 0 (string-length name) #t) #t)))))

  (edoc "Whether an entry matches a query under a root: by name, or by path when the query has a slash."
        (entry (record entry) "the entry")
        (root directory "the root")
        (query string "the filter")
        (returns boolean))
  (define (matches? entry root query)
    ((matcher root query) entry))

  (define (inspect-entry path)
    (let* ([info (sys:file-info path)]
           [link? (and info (eq? (vector-ref info 0) 'link))]
           [target (if link? (sys:file-info path #t) info)])
      (make-entry path (if target (vector-ref target 0) 'unavailable) link?
        (and info (vector-ref info 1)) (and info (vector-ref info 2))
        (and info (vector-ref info 3)) (and info (vector-ref info 4)) #f #f '())))

  (define (with-count entry count complete? matches)
    (make-entry (entry-path entry) (entry-kind entry) (entry-link? entry)
      (entry-mode entry) (entry-size entry) (entry-modified entry) (entry-created entry)
      count complete? matches))

  (define (visible? entry root hidden?)
    (or hidden?
        (let ([path (relative-path entry root)])
          (not (or (string:prefix? "." path)
                   (string:search path "/." 0 (string-length path)))))))

  (define (retained-count entries match?)
    (fold-left (lambda (n entry)
                 (+ n (if (match? entry) 1 0) (retained-count (entry-matches entry) match?)))
      0 entries))

  (define (branch? entry match?)
    (or (match? entry)
        (and (entry-count entry) (positive? (entry-count entry)))))

  (edoc "Filter known entries by a new query without rescanning: exact when the previous group kept every match, else a bounded subset until a fresh scan."
        (entries list "the entries")
        (root directory "the root")
        (previous string "the previous query")
        (query string "the new query")
        (was-hidden? boolean "whether hidden entries were included")
        (hidden? boolean "whether they are now")
        (limit integer "the expansion limit")
        (returns list))
  (define (refilter entries root previous query was-hidden? hidden? limit)
    ;; A narrower query is exact when the previous group retained every
    ;; match. Otherwise keep the known subset, with a lower bound or an
    ;; unknown count, until a fresh scan replaces it. Empty queries count
    ;; immediate children, so their counts cannot stand in for search counts.
    (let* ([match? (matcher root query)] [previous-match? (matcher root previous)]
           ;; Containment implies a subset only within one matching mode: a
           ;; name filter gaining a slash starts matching different text.
           [same-kind? (eq? (path-query? previous) (path-query? query))]
           [same? (and (string=? previous query) (eq? was-hidden? hidden?))]
           [narrower? (and same-kind? (not (string=? query ""))
                           (or (not hidden?) was-hidden?)
                           (string:search query previous 0 (string-length query) #t))]
           [wider? (and same-kind? (not (string=? previous "")) (not (string=? query ""))
                        (or hidden? (not was-hidden?))
                        (string:search previous query 0 (string-length previous) #t))])
      (define (project entry)
        (if (or (not (directory? entry)) (entry-link? entry)) entry
            (let* ([old (entry-count entry)]
                   [kept (retained-count (entry-matches entry) previous-match?)]
                   [matches (if (string=? query "") '()
                                (filter (lambda (e) (branch? e match?))
                                  (map project (filter (lambda (e) (visible? e root hidden?))
                                                 (entry-matches entry)))))]
                   [known (retained-count matches match?)]
                   [exact? (and (entry-complete? entry) (or same? (and narrower? old (= old kept))))]
                   [count (cond [same? old] [exact? known] [wider? (and old (max old known))]
                                [(positive? known) known]
                                [(or (string=? query "") (not old) (> old kept)) #f]
                                [else 0])])
              (with-count entry count exact?
                (if (and count (> count limit)) '() matches)))))
      (map project (filter (lambda (entry) (visible? entry root hidden?)) entries))))

  (edoc "Merge a partial publication with the previous entries so known matches survive, preferring fresh metadata; a complete one replaces them."
        (entries list "the new entries")
        (previous list "the previous entries")
        (root directory "the search root")
        (query string "the filter")
        (limit integer "the expansion limit")
        (done? boolean "whether the publication is complete")
        (returns list))
  (define (reconcile entries previous root query limit done?)
    ;; A shallow/partial publication must not erase matches the new query
    ;; already knows. Prefer fresh metadata and union bounded match sets;
    ;; completed groups (and the final snapshot, including errors/removals)
    ;; always replace the preview authoritatively.
    (let ([match? (matcher root query)])
      (define (merge entries previous keep-missing?)
        (let ([known (make-hashtable string-hash string=?)])
          (for-each (lambda (entry) (hashtable-set! known (entry-path entry) entry)) previous)
          (let ([merged
                 (map (lambda (entry)
                        (let ([old (hashtable-ref known (entry-path entry) #f)] [count (entry-count entry)])
                          (hashtable-delete! known (entry-path entry))
                          (cond [(or (not old) (not (directory? entry)) (entry-link? entry) (entry-complete? entry)) entry]
                            [(not count) (with-count entry (entry-count old) (entry-complete? old) (entry-matches old))]
                            [else
                             (let* ([matches (merge (entry-matches entry) (entry-matches old) #t)]
                                    [total (max count (or (entry-count old) 0) (retained-count matches match?))])
                               (with-count entry total #f
                                 (if (<= total limit) matches '())))]))) entries)])
            (if keep-missing?
                (append merged (filter (lambda (e) (hashtable-contains? known (entry-path e))) previous))
                merged))))
      (if done? entries (merge entries previous #f))))

  (edoc "Scan a directory for entries matching a query, publishing (entries unreadable-directories done?) at most ten times a second until the counts are exact; each directory keeps at most limit matches."
        (path directory "the directory")
        (query string "the filter")
        (hidden? boolean "whether to include dot names")
        (limit integer "the expansion limit")
        (cancelled? thunk "whether to stop")
        (publish! procedure "(publish! entries unreadable done?)"))
  (define (scan path query hidden? limit cancelled? publish!)
    ;; Publish (entries unreadable-directories done?), initially the shallow
    ;; inventory, then at most ten times/second, and finally exact counts.
    ;; Each directory retains at most limit descendant matches. Crossing the
    ;; threshold drops those rows, but counting continues without a result cap.
    ;; Entries/counts include dot names only when requested. Symbolic directory
    ;; links are listed and may be entered explicitly, never walked recursively.
    (unless (and (integer? limit) (exact? limit) (>= limit 0))
      (error 'scan "expected a nonnegative expansion limit" limit))
    (call/cc
      (lambda (cancel)
        (define failures 0)
        (define match? (matcher path query))
        (define next-update (current-time 'time-monotonic))
        (define entries '#())
        (define (check!) (when (cancelled?) (cancel (void))))
        (define (names path)
          (check!)
          (guard (ex [else (set! failures (+ failures 1)) '()])
            (filter (lambda (name) (or hidden? (not (string:prefix? "." name))))
              (directory-list path))))
        (define (child path name)
          (check!)
          (inspect-entry (string-append path (if (string=? path "/") "" "/") name)))
        (define (publish force? done?)
          (check!)
          (let ([now (current-time 'time-monotonic)])
            (when (or force? (time>=? now next-update))
              (set! next-update (add-duration now (make-time 'time-duration 100000000 0)))
              (publish! (vector->list entries) failures done?))))
        (define (search! index root)
          (let ([total 0])
            (define (weight entry)
              (+ (if (match? entry) 1 0) (or (entry-count entry) 0)))
            (define (update! entry)
              (vector-set! entries index entry)
              (publish #f #f))
            ;; Retain the route to each match, not a second flat list. The
            ;; limit counts actual matches, never connecting directories.
            ;; Once the root exceeds it, release the tree while counting on.
            (define (walk entry notify)
              (let ([before failures] [count 0] [found '()])
                (define (snapshot current done?)
                  (with-count entry (+ count (if current (weight current) 0))
                    (and done? (= before failures))
                    (if (> total limit) '()
                        (reverse (if (and current (branch? current match?)) (cons current found) found)))))
                (for-each
                  (lambda (name)
                    (let ([e (child (entry-path entry) name)])
                      (when (match? e) (set! total (+ total 1)))
                      (when (eq? (entry-kind e) 'unavailable) (set! failures (+ failures 1)))
                      (let ([e (if (and (directory? e) (not (entry-link? e)))
                                   (walk e (lambda (partial) (notify (snapshot partial #f)))) e)])
                        (set! count (+ count (weight e)))
                        (set! found (cond [(> total limit) '()] [(branch? e match?) (cons e found)] [else found]))
                        ;; Assemble ancestor snapshots only when publication
                        ;; is due; their unfinished counts stay lower bounds.
                        (when (time>=? (current-time 'time-monotonic) next-update)
                          (notify (snapshot #f #f))))))
                  (names (entry-path entry)))
                (snapshot #f #t)))
            (update! (walk root update!))))
        (set! entries (list->vector (map (lambda (name) (child path name)) (names path))))
        (publish #t #f)
        (do ([i 0 (+ i 1)]) ((= i (vector-length entries)))
          (check!)
          (let ([entry (vector-ref entries i)])
            (when (and (directory? entry) (not (entry-link? entry)))
              (if (string=? query "")
                  (let* ([before failures] [count (length (names (entry-path entry)))])
                    (vector-set! entries i (with-count entry count (= before failures) '()))
                    (publish #f #f))
                  (search! i entry)))))
        (publish #t #t))))
)
