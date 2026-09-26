;; directory.sls -- cached filesystem inventory and recursive match trees.
;; No head state or threads: the caller owns cancellation and publication.
(import (only (foundation edoc) elibrary))
(elibrary (service directory)
  (export clear! close! directory? entry-complete? entry-count entry-created entry-kind entry-link?
          entry-matches entry-mode entry-modified entry-path entry-size make-cache matches?
          (rename (parent-path parent)) poll! refilter relative-path scan!)
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
           ;; Resolve links to recognize navigable directories, but keep the
           ;; link's own kind if its target is absent or inaccessible. Only
           ;; failure to inspect the entry itself makes a scan incomplete.
           [target (if link? (or (sys:file-info path #t) info) info)])
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

  (edoc "Filter the previous result immediately while a new query projects the cached inventory."
        (entries list "the entries")
        (root directory "the root")
        (previous string "the previous query")
        (query string "the new query")
        (was-hidden? boolean "whether hidden entries were included")
        (hidden? boolean "whether they are now")
        (returns list))
  (define (refilter entries root previous query was-hidden? hidden?)
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
              (with-count entry count exact? matches))))
      (map project (filter (lambda (entry) (visible? entry root hidden?)) entries))))

  ;; One worker owns a cache, including its event stream. Neither filters
  ;; nor navigation invalidate it. Listings include hidden names, but their
  ;; metadata and subtrees are loaded only when a query needs them.
  (define-record-type (cache %make-cache cache?)
    (fields entries directories watch? (mutable watcher)))

  (edoc "Create a filesystem inventory cache, optionally subscribing to directory changes."
        (watch? boolean "whether to use OS notifications when available") (returns any))
  (define (make-cache watch?)
    (%make-cache (make-hashtable string-hash string=?) (make-hashtable string-hash string=?)
      watch? (and watch? (sys:open-directory-watch))))

  (edoc "Release the cache's filesystem subscriptions."
        (cache any "the inventory cache"))
  (define (close! cache)
    (sys:close-directory-watch! (cache-watcher cache))
    (cache-watcher-set! cache #f))

  (edoc "Forget the entire inventory and renew its filesystem subscriptions."
        (cache any "the inventory cache"))
  (define (clear! cache)
    (close! cache)
    (hashtable-clear! (cache-entries cache))
    (hashtable-clear! (cache-directories cache))
    (cache-watcher-set! cache (and (cache-watch? cache) (sys:open-directory-watch))))

  (define (invalidate! cache path replaced?)
    (let ([changed? #f])
      (define (drop! table key)
        (when (hashtable-contains? table key)
          (set! changed? #t) (hashtable-delete! table key)))
      (define (dirty! path)
        ;; Keep the directory identity until its subtree is discarded. An
        ;; earlier chmod/child event must not hide it from a later rename
        ;; in the same batch, leaving old descendants under a reused path.
        (when (hashtable-contains? (cache-directories cache) path)
          (set! changed? #t) (hashtable-set! (cache-directories cache) path 'stale)))
      (if replaced?
          (let ([prefix (if (string=? path "/") "/" (string-append path "/"))]
                [entry (hashtable-ref (cache-entries cache) path #f)])
            (if (or (hashtable-contains? (cache-directories cache) path) (and entry (directory? entry)))
              (begin (for-each
                       (lambda (table)
                         (vector-for-each
                           (lambda (p) (when (or (string=? path p) (string:prefix? prefix p)) (drop! table p)))
                           (hashtable-keys table)))
                       (list (cache-entries cache) (cache-directories cache)))
                (sys:unwatch-directory! (cache-watcher cache) path))
              (drop! (cache-entries cache) path))
            (let ([parent (parent-path path)])
              (dirty! parent)
              (drop! (cache-entries cache) parent)))
          (begin (drop! (cache-entries cache) path)
                 ;; A directory's own permission change may make a formerly
                 ;; failed listing readable, or a known listing inaccessible.
                 (dirty! path)))
      changed?))

  (edoc "Invalidate only inventory affected by pending filesystem events; return whether anything changed."
        (cache any "the inventory cache") (returns boolean))
  (define (poll! cache)
    (let ([events (sys:directory-changes! (cache-watcher cache))])
      (if (eq? events #t) (begin (clear! cache) #t)
          (fold-left (lambda (changed? event)
                       (or (invalidate! cache (car event) (eq? (cdr event) 'replaced)) changed?)) #f events))))

  ;; Loading and projecting use one traversal. A read-only projection of a
  ;; partially filled cache keeps everything already known; no separate
  ;; union of old and new query results is necessary.
  (define (inventory cache path query hidden? load? check! tick!)
    (let ([failures 0] [pending 0] [match? (matcher path query)])
      (define (names path)
        (check!)
        (when (eq? (hashtable-ref (cache-directories cache) path 'stale) 'stale)
          (when load?
            ;; Subscribe before listing, so mutations during discovery are
            ;; queued and invalidate the just-read inventory on the next pass.
            (sys:watch-directory! (cache-watcher cache) path)
            (hashtable-set! (cache-directories cache) path
              (guard (ex [else #f]) (directory-list path)))
            (tick!)))
        (cond [(eq? (hashtable-ref (cache-directories cache) path 'stale) 'stale)
               (set! pending (+ pending 1)) #f]
              [(hashtable-ref (cache-directories cache) path #f) =>
               (lambda (names) (filter (lambda (name) (or hidden? (not (string:prefix? "." name)))) names))]
              [else (set! failures (+ failures 1)) '()]))
      (define (child path name)
        (check!)
        (let ([path (string-append path (if (string=? path "/") "" "/") name)])
          (unless (hashtable-contains? (cache-entries cache) path)
            (when load? (hashtable-set! (cache-entries cache) path (inspect-entry path)) (tick!)))
          (let ([entry (hashtable-ref (cache-entries cache) path #f)])
            (cond [(not entry) (set! pending (+ pending 1))]
                  [(eq? (entry-kind entry) 'unavailable) (set! failures (+ failures 1))])
            entry)))
      (define (children path)
        (fold-right (lambda (name out)
                      (let ([entry (child path name)])
                        (if entry (cons (visit entry) out) out))) '() (or (names path) '())))
      (define (visit entry)
        (if (or (not (directory? entry)) (entry-link? entry)) entry
            (let* ([before failures] [waiting pending]
                   [children (if (string=? query "") (names (entry-path entry)) (children (entry-path entry)))]
                   [count (if (string=? query "") (and children (length children))
                              (fold-left (lambda (n e) (+ n (if (match? e) 1 0) (or (entry-count e) 0))) 0 children))])
              (with-count entry count (and (= before failures) (= waiting pending))
                (if (string=? query "") '() (filter (lambda (e) (branch? e match?)) children))))))
      (let ([entries (children path)])
        (values entries failures (zero? pending)))))

  (edoc "Project all recursive matches from the cache, loading missing inventory and publishing (entries failures done?) at most ten times a second."
        (cache any "the inventory cache")
        (path directory "the directory")
        (query string "the filter")
        (hidden? boolean "whether to include dot names")
        (cancelled? thunk "whether to stop")
        (publish! procedure "(publish! entries unreadable done?)"))
  (define (scan! cache path query hidden? cancelled? publish!)
    (call/cc
      (lambda (cancel)
        (define next-update (add-duration (current-time 'time-monotonic) (make-time 'time-duration 100000000 0)))
        (define (check!) (when (cancelled?) (cancel (void))))
        (define (publish entries failures done?)
          (check!) (publish! entries failures done?) done?)
        (define (tick!)
          (when (time>=? (current-time 'time-monotonic) next-update)
            (call-with-values (lambda () (inventory cache path query hidden? #f check! void)) publish)
            (set! next-update (add-duration (current-time 'time-monotonic) (make-time 'time-duration 100000000 0)))))
        (check!)
        ;; Watching the parent also detects replacement/recreation of the
        ;; browsing root after its own watch disappears.
        (sys:watch-directory! (cache-watcher cache) (parent-path path))
        (unless (call-with-values (lambda () (inventory cache path query hidden? #f check! void)) publish)
          (call-with-values (lambda () (inventory cache path query hidden? #t check! tick!)) publish)))))
)
