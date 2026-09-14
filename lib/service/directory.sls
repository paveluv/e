;; directory.sls -- filesystem inventory and bounded recursive match groups.
;; No head state or threads: the caller owns cancellation and publication.
(library (directory)
  (export scan refilter reconcile entry-path entry-kind entry-link? entry-mode entry-size
          entry-modified entry-created entry-count entry-complete? entry-matches
          relative-path matches? directory? (rename (parent-path parent)))
  (import (chezscheme) (prefix (sys) sys:) (prefix (file) file:)
          (prefix (string) string:))

  (define-record-type entry
    (fields path kind link? mode size modified created count complete? matches))

  (define (parent-path path)
    (file:canonical (string-append path "/..")))

  (define (directory? entry) (eq? (entry-kind entry) 'directory))

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

  (define (matches? entry root query) ((matcher root query) entry))

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

  (define (refilter entries root previous query was-hidden? hidden? limit)
    ;; A narrower query is exact when the previous group retained every
    ;; match. Otherwise keep the known subset, with a lower bound or an
    ;; unknown count, until a fresh scan replaces it. Empty queries count
    ;; immediate children, so their counts cannot stand in for search counts.
    (let* ([match? (matcher root query)]
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
      (map (lambda (entry)
             (if (or (not (directory? entry)) (entry-link? entry)) entry
                 (let* ([old (entry-count entry)]
                        [matches (if (string=? query "") '()
                                     (filter (lambda (e) (and (visible? e root hidden?) (match? e)))
                                       (entry-matches entry)))]
                        [exact? (and (entry-complete? entry)
                                     (or same? (and narrower? old (= old (length (entry-matches entry))))))]
                        [count (cond [same? old] [exact? (length matches)] [wider? old]
                                     [(pair? matches) (length matches)]
                                     [(or (string=? query "") (not old)
                                          (> old (length (entry-matches entry)))) #f]
                                     [else 0])])
                   (with-count entry count exact?
                     (if (and count (> count limit)) '() matches)))))
        (filter (lambda (entry) (visible? entry root hidden?)) entries))))

  (define (reconcile entries previous limit done?)
    ;; A shallow/partial publication must not erase matches the new query
    ;; already knows. Prefer fresh metadata and union bounded match sets;
    ;; completed groups (and the final snapshot, including errors/removals)
    ;; always replace the preview authoritatively.
    (if done? entries
        (let ([known (make-hashtable string-hash string=?)])
          (for-each (lambda (entry) (hashtable-set! known (entry-path entry) entry)) previous)
          (map (lambda (entry)
                 (let ([old (hashtable-ref known (entry-path entry) #f)] [count (entry-count entry)])
                   (cond [(or (not old) (not (directory? entry)) (entry-link? entry) (entry-complete? entry)) entry]
                         [(not count) (with-count entry (entry-count old) (entry-complete? old) (entry-matches old))]
                         [else
                          (let* ([seen (make-hashtable string-hash string=?)]
                                 [matches
                                  (filter (lambda (e)
                                            (and (not (hashtable-contains? seen (entry-path e)))
                                                 (begin (hashtable-set! seen (entry-path e) #t) #t)))
                                    (append (entry-matches entry) (entry-matches old)))]
                                 [total (max count (or (entry-count old) 0) (length matches))])
                            (with-count entry total #f
                              (if (<= total limit) matches '())))]))) entries))))

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
          (let ([before failures] [count 0] [found '()])
            (define (update! done?)
              (vector-set! entries index
                (with-count root count (and done? (= before failures))
                  (if (<= count limit) (reverse found) '())))
              (publish #f #f))
            ;; The explicit work stack avoids recursive Scheme frames for
            ;; deep paths. Each pending directory is listed only on descent.
            (let walk ([pending (list (entry-path root))])
              (check!)
              (if (null? pending) (update! #t)
                  (let ([children (names (car pending))] [next (cdr pending)])
                    (for-each
                      (lambda (name)
                        (let ([entry (child (car pending) name)])
                          (when (match? entry)
                            (set! count (+ count 1))
                            (set! found (if (<= count limit) (cons entry found) '())))
                          (when (eq? (entry-kind entry) 'unavailable)
                            (set! failures (+ failures 1)))
                          (when (and (directory? entry) (not (entry-link? entry)))
                            (set! next (cons (entry-path entry) next)))
                          ;; Only build/publish a snapshot when the clock is
                          ;; due; a huge group still reports growing counts.
                          (when (time>=? (current-time 'time-monotonic) next-update)
                            (update! #f)))) children)
                    (walk next))))))
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
