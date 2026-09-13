;; directory.sls -- filesystem inventory and bounded recursive match groups.
;; No head state or threads: the caller owns cancellation and publication.
(library (directory)
  (export scan entry-path entry-kind entry-link? entry-mode entry-size
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

  (define (matches? entry root query)
    (let ([name (string-append (relative-path entry root) (if (directory? entry) "/" ""))])
      (and (string:search name query 0 (string-length name) #t) #t)))

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
                          (when (matches? entry path query)
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
