;; directory.sls -- cached filesystem inventory and recursive match trees.
;; No head state or threads: the caller owns cancellation and publication.
(import (only (foundation edoc) elibrary))
(elibrary (service directory)
  (export clear! close! directory? entry-complete? entry-count entry-created entry-kind entry-link?
          entry-matches entry-mode entry-modified entry-path entry-size filter-path make-cache make-missing matches? missing?
          (rename (parent-path parent)) poll! read! relative-path scan!)
  (import (chezscheme)
          (prefix (foundation path-filter) path-filter:)
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

  (edoc "An uncreated filesystem entry, distinct from entries observed on disk.")
  (define-record-type (missing %make-missing missing?) (parent entry) (fields))

  (edoc "An uncreated entry with optional uncreated children, for a filesystem view's creation choices."
        (path string "the absolute path") (directory? boolean "whether to create a directory")
        (children list "the missing child entries") (returns (record entry)))
  (define (make-missing path directory? children)
    (%make-missing path (if directory? 'directory 'file) #f #f #f #f #f #f #t children))

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
    (if (string=? (entry-path entry) root) ""
        (string:tail (entry-path entry) (if (string=? root "/") 1 (+ 1 (string-length root))))))

  (edoc "The absolute match path, ending in a slash for directories."
        (entry (record entry) "the entry") (returns string))
  (define (filter-path entry)
    (let ([path (entry-path entry)])
      (if (and (directory? entry) (not (string=? path "/"))) (string-append path "/") path)))

  (edoc "Whether an entry's full path matches non-overlapping literal keys."
        (entry (record entry) "the entry") (keys list "the expanded keys") (returns boolean))
  (define (matches? entry keys)
    ((path-filter:matcher keys) (filter-path entry)))

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

  (edoc "Load an entry's metadata from the worker-owned cache, retaining its search counts and children."
        (cache any "the inventory cache") (entry (record entry) "the entry") (returns (record entry)))
  (define (read! cache entry)
    (let* ([path (entry-path entry)]
           [known (or (hashtable-ref (cache-entries cache) path #f)
                      (let ([new (inspect-entry path)]) (hashtable-set! (cache-entries cache) path new) new))])
      (with-count known (entry-count entry) (entry-complete? entry) (entry-matches entry))))

  ;; One worker owns a cache, including its event stream. Neither filters
  ;; nor navigation invalidate it. Listings include hidden names, but their
  ;; metadata and subtrees are loaded only when a query needs them.
  (define-record-type (cache %make-cache cache?)
    (fields entries directories read watch? (mutable watcher)))

  (edoc "Create a filesystem inventory cache, optionally subscribing to directory changes."
        (watch? boolean "whether to use OS notifications when available") (returns any))
  (define (make-cache watch?)
    (%make-cache (make-hashtable string:hash string=?) (make-hashtable string:hash string=?)
      (sys:directory-reader) watch? (and watch? (sys:open-directory-watch))))

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

  (edoc "Search a cached inventory, loading missing listings and result metadata; publish (entries failures done?) at most ten times a second."
        (cache any "the inventory cache")
        (path directory "the directory")
        (keys list "expanded path keys, empty for an immediate directory overview")
        (hidden? boolean "whether to include dot names")
        (metadata? boolean "whether all matches need metadata, for example for sorting")
        (cancelled? thunk "whether to stop")
        (publish! procedure "(publish! entries unreadable done?)"))
  (define (scan! cache path keys hidden? metadata? cancelled? publish!)
    (call/cc
      (lambda (cancel)
        (define failures 0)
        (define frames '())
        (define steps 0)
        (define empty? (null? keys))
        (define deep? (not empty?))
        (define match-text? (path-filter:matcher keys))
        (define root-prefix (if (string=? path "/") "/" (string-append path "/")))
        (define next-update (sys:after 0.1))
        (define (check!) (when (cancelled?) (cancel (void))))
        (define (visible? name) (or hidden? (not (char=? (string-ref name 0) #\.))))
        (define (names path follow?)
          (check!)
          (let ([known (hashtable-ref (cache-directories cache) path 'stale)])
            (let ([entries (if (eq? known 'stale)
                             (begin
                               (sys:watch-directory! (cache-watcher cache) path)
                               (let ([entries ((cache-read cache) path follow?)])
                                 (hashtable-set! (cache-directories cache) path entries) entries))
                             known)])
              (unless entries (set! failures (+ failures 1)))
              (or entries '()))))
        (define (entry! path kind)
          (or (hashtable-ref (cache-entries cache) path #f)
              (if (or metadata? (memq kind '(link unavailable))) (begin
                                                                   (check!)
                                                                   (let ([entry (inspect-entry path)])
                                                                     (hashtable-set! (cache-entries cache) path entry) entry))
                  (make-entry path kind #f #f #f #f #f #f #f '()))))
        (define (publish entries done?)
          (check!) (publish! entries failures done?))
        (define (tick!)
          (check!)
          (when (time>=? (current-time 'time-monotonic) next-update)
            ;; Frames hold only retained results, not the visited inventory.
            ;; Seal the current branch around already completed siblings.
            (let ([started (current-time 'time-monotonic)]
                  [result (fold-left (lambda (child frame) (frame child #f)) #f frames)])
              (when result (publish (car result) #f))
              ;; Preparing a broad result can cost more than the scan that
              ;; discovered it. Keep that work a fraction of the search,
              ;; rather than repeatedly rebuilding an ever larger table.
              (let ([elapsed (time-difference (current-time 'time-monotonic) started)])
                (set! next-update (sys:after (max 0.1 (* 4 (+ (time-second elapsed) (/ (time-nanosecond elapsed) 1e9))))))))))
        (define (walk directory node own? keep?)
          (let ([out '()] [count 0] [before failures]
                [prefix (if (string=? directory "/") "/" (string-append directory "/"))])
            (define (snapshot child done?)
              (let* ([children (reverse (if (and child (car child)) (cons (car child) out) out))]
                     [total (+ count (if child (cdr child) 0))])
                (cons (if node
                          (and (or keep? own? (positive? total))
                               (let ([entry (entry! directory 'directory)])
                                 (when (and done? (eq? (entry-kind entry) 'unavailable))
                                   (set! failures (+ failures 1)))
                                 (with-count entry total (and done? (= failures before)) children)))
                          children)
                  (+ total (if own? 1 0)))))
            (set! frames (cons snapshot frames))
            (for-each
              (lambda (item)
                (let ([name (car item)] [kind (cdr item)])
                  (when (visible? name)
                    ;; Paths and metadata are materialized only for a match
                    ;; or a directory we visit. Most files need neither.
                    (let* ([full (string-append prefix name)]
                           [link (and (eq? kind 'link) (entry! full kind))]
                           [dir? (or (eq? kind 'directory) (and link (directory? link)))]
                           [text (if dir? (string-append full "/") full)]
                           [match? (or empty? (match-text? text))]
                           [result
                            (cond
                              [(and dir? (not link) deep? (not match?) (path-filter:possible? keys text))
                               (walk full #t #f (not node))]
                              [(or match? (and (not deep?) dir?) (and (not node) (eq? kind 'link)))
                               (let* ([entry (entry! (or full (string-append prefix name)) kind)]
                                      [entry (cond [(and deep? dir?) (with-count entry 0 #t '())]
                                               [(and (not deep?) dir? (not link))
                                                (let* ([before failures] [children (names full #f)])
                                                  (with-count entry (length (filter (lambda (p) (visible? (car p))) children))
                                                    (= failures before) '()))] [else entry])])
                                 (when (eq? (entry-kind entry) 'unavailable) (set! failures (+ failures 1)))
                                 (cons entry (if match? 1 0)))]
                              [else
                               (when (eq? kind 'unavailable) (set! failures (+ failures 1)))
                               (cons #f 0)])])
                      (when (car result) (set! out (cons (car result) out)))
                      (set! count (+ count (cdr result))))))
                (set! steps (+ steps 1))
                (when (zero? (mod steps 256)) (tick!)))
              (names directory (not node)))
            (set! frames (cdr frames))
            (snapshot #f #t)))
        (check!)
        ;; Watching the parent also detects replacement/recreation of the
        ;; browsing root after its own watch disappears.
        (sys:watch-directory! (cache-watcher cache) (parent-path path))
        (publish '() #f)
        (if (and deep? (match-text? root-prefix))
            (publish (list (with-count (entry! path 'directory) 0 #t '())) #t)
            (let ([result (walk path #f #f #t)]) (publish (car result) #t))))))
)
