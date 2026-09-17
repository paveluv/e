;; Symbol completion by disjoint, boundary-starting segments. One matching
;; relation governs admission, ranking alignments and safe normalization.

(library (fuzzy)
  (export matches expansions rank
          (rename (match-name name) (match-score score) (match-fragments fragments)))
  (import (rnrs))

  (define-record-type match (fields name score fragments))
  ;; Parts are (start . end): each subword includes its trailing ':' or '-'.
  ;; Every part starts at a boundary, and every character stays in a segment.
  (define-record-type source (fields name parts counts))
  (define (separator? c) (memv c '(#\: #\-)))

  (define (character-counts s)
    (let ([counts (make-eqv-hashtable)])
      (string-for-each
        (lambda (c) (hashtable-set! counts c (+ 1 (hashtable-ref counts c 0)))) s)
      counts))

  (define (includes-counts? counts required)
    (for-all
      (lambda (c) (>= (hashtable-ref counts c 0) (hashtable-ref required c 0)))
      (vector->list (hashtable-keys required))))

  (define (source-of name)
    (let ([n (string-length name)])
      (let scan ([i 0] [start 0] [parts '()])
        (cond
          [(= i n)
           (make-source name
             (reverse (if (< start n) (cons (cons start n) parts) parts))
             (character-counts name))]
          [(separator? (string-ref name i))
           (scan (+ i 1) (+ i 1)
             (cons (cons start (+ i 1)) parts))]
          [else (scan (+ i 1) start parts)]))))

  (define (alignment query source)
    ;; Match longest leading segments first, then earliest candidate position.
    ;; All characters, including ':' and '-', stay inside these literal runs.
    ;; Backtracking keeps eligibility independent of a greedy choice; the bit
    ;; mask prevents reuse of any character occurrence.
    (let* ([name (source-name source)] [n (string-length name)] [m (string-length query)]
           [failed (make-eqv-hashtable)] [stride (bitwise-arithmetic-shift-left 1 n)])
      (and (<= m n) (includes-counts? (source-counts source) (character-counts query))
        (let solve ([at 0] [used 0])
          (let ([key (+ used (* at stride))])
            (cond
              [(= at m) '()]
              [(hashtable-ref failed key #f) #f]
              [else
               (let ([options
                      (list-sort
                        (lambda (a b) (if (= (cdr a) (cdr b)) (< (car a) (car b)) (> (cdr a) (cdr b))))
                        (fold-right
                          (lambda (part out)
                            (let* ([start (car part)]
                                   [size
                                    (let prefix ([size 0])
                                      (if (and (< (+ at size) m) (< (+ start size) n)
                                               (not (bitwise-bit-set? used (+ start size)))
                                               (char=? (string-ref query (+ at size)) (string-ref name (+ start size))))
                                          (prefix (+ size 1)) size))])
                              (if (zero? size) out (cons (cons start size) out))))
                          '() (source-parts source)))])
                 (or
                   (let candidates ([options options])
                     (and (pair? options)
                       (let ([start (caar options)])
                         (or
                           (let lengths ([size (cdar options)])
                             (and (> size 0)
                               (let* ([mask (bitwise-arithmetic-shift-left
                                              (- (bitwise-arithmetic-shift-left 1 size) 1) start)]
                                      [tail (solve (+ at size) (bitwise-ior used mask))])
                                 (if tail
                                     (cons (list at start size) tail)
                                     (lengths (- size 1))))))
                           (candidates (cdr options))))))
                   (begin (hashtable-set! failed key #t) #f)))]))))))

  (define (score fragments size name-size)
    (if (null? fragments) (list 0 0 0 0 name-size)
      (let* ([inversions
              (let pairs ([xs fragments])
                (if (null? xs) 0
                    (+ (length (filter (lambda (b) (> (cadar xs) (cadr b))) (cdr xs)))
                       (pairs (cdr xs)))))]
             [prefix (apply min (map cadr fragments))]
             [end (apply max (map (lambda (f) (+ (cadr f) (caddr f))) fragments))])
        (list (- (length fragments) 1) inversions prefix (- end prefix size) name-size))))

  (define (score<? a b)
    (cond [(null? a) #f] [(< (car a) (car b)) #t] [(> (car a) (car b)) #f]
          [else (score<? (cdr a) (cdr b))]))

  (define (rank query names)
    (if (string=? query "")
        (map (lambda (name) (make-match name (list 0 0 0 0 (string-length name)) '()))
          (list-sort string<? names))
      (list-sort
        (lambda (a b)
          (if (equal? (match-score a) (match-score b))
            (string<? (match-name a) (match-name b))
            (score<? (match-score a) (match-score b))))
        (fold-right
          (lambda (name out)
            (let ([fragments (alignment query (source-of name))])
              (if fragments
                (cons (make-match name (score fragments (string-length query) (string-length name)) fragments) out)
                out))) '() names))))

  (define (matches query names) (map match-name (rank query names)))

  (define (common-counts sources)
    (let ([counts (hashtable-copy (source-counts (car sources)) #t)])
      (for-each
        (lambda (source)
          (vector-for-each
            (lambda (c)
              (hashtable-set! counts c (min (hashtable-ref counts c 0) (hashtable-ref (source-counts source) c 0))))
            (hashtable-keys counts))) (cdr sources))
      counts))

  (define (walk-extensions query source ordered? counts limit minimum compatible? accept)
    ;; Every match is a permutation of prefixes of the boundary-starting parts.
    ;; Ordered walks produce readable projections for the Tab cycle. An
    ;; unrestricted walk establishes the maximum length, even when no such
    ;; projection can express it. Prefix rejection prunes every continuation.
    (define name (source-name source))
    (define (capacity parts left)
      ;; A missing character blocks the rest of its subword, even if those
      ;; later characters occur elsewhere. Counting that tail as available
      ;; makes the longest-extension proof needlessly enumerate permutations.
      (fold-left
        (lambda (n part)
          (+ n (let prefix ([at (car part)])
                 (if (and (< at (cdr part)) (> (hashtable-ref left (string-ref name at) 0) 0))
                     (prefix (+ at 1)) (- at (car part)))))) 0 parts))
    (define (viable? text parts)
      ;; Extra boundaries make the unused parts a superset of every possible
      ;; continuation. Reject prefixes that cannot retain the original query
      ;; even there, instead of proving this again for every permutation.
      (alignment query
        (source-of
          (fold-right
            (lambda (part tail) (string-append tail "-" (substring name (car part) (cdr part))))
            text parts))))
    (call-with-current-continuation
      (lambda (done)
        (let walk ([parts (source-parts source)] [text ""] [left counts])
          (let ([size (string-length text)])
            (when (and (>= size (minimum)) (accept text)) (done text))
            (when (and (< size limit)
                       (>= (+ size (capacity parts left)) (minimum))
                       (viable? text parts))
              (let choices ([rest parts] [seen '()])
                (when (pair? rest)
                  (let* ([part (car rest)] [start (car part)] [spelling (substring name start (cdr part))]
                         [remaining (hashtable-copy left #t)]
                         [end
                          (let take ([end start])
                            (if (and (< end (cdr part)) (< (+ size (- end start)) limit)
                                     (> (hashtable-ref remaining (string-ref name end) 0) 0))
                                (begin
                                  (hashtable-set! remaining (string-ref name end)
                                    (- (hashtable-ref remaining (string-ref name end) 0) 1))
                                  (take (+ end 1))) end))])
                    (unless (member spelling seen)
                      (let lengths ([end end])
                        (when (> end start)
                          (let ([next (string-append text (substring name start end))])
                            (when (compatible? next)
                              (walk (if ordered? (cdr rest) (remq part parts)) next remaining)))
                          (let ([c (string-ref name (- end 1))])
                            (hashtable-set! remaining c (+ (hashtable-ref remaining c 0) 1)))
                          (lengths (- end 1)))))
                    ;; Identical parts are interchangeable only in unrestricted
                    ;; permutations; ordered projections retain their positions.
                    (choices (cdr rest) (if ordered? seen (cons spelling seen)))))))))
        #f)))

  (define (expansions query names)
    ;; A safe extension E satisfies query <= E <= every current match under
    ;; this same relation. Boundary-starting alignments compose, so transitivity
    ;; guarantees that E cannot introduce a name the query did not match.
    (cond [(null? names) (list query)] [(null? (cdr names)) names]
      [else
       (let* ([sources (map source-of names)] [counts (common-counts sources)]
              [limit (fold-left (lambda (n c) (+ n (hashtable-ref counts c 0))) 0
                       (vector->list (hashtable-keys counts)))]
              [known (make-hashtable string-hash string=?)]
              [best query] [size (string-length query)])
         (define (compatible? text)
           (let ([hit (hashtable-ref known text #f)])
             (if hit (car hit)
               (let ([yes? (for-all (lambda (source) (and (alignment text source) #t)) sources)])
                 (hashtable-set! known text (list yes?)) yes?))))
         (define (refines? text) (alignment query (source-of text)))
         (when (> limit size)
           (walk-extensions
             query
             (car (list-sort (lambda (a b) (< (string-length (source-name a)) (string-length (source-name b)))) sources))
             #f counts limit (lambda () (+ size 1)) compatible?
             (lambda (text)
               (and (refines? text)
                 (begin (set! best text) (set! size (string-length text)) (= size limit))))))
         (let ([whole (filter (lambda (name) (and (= (string-length name) size) (compatible? name))) names)])
           (if (pair? whole) whole
             (let ([seen (make-hashtable string-hash string=?)])
               (let candidates ([sources sources] [out '()])
                 (if (null? sources) (if (null? out) (list best) (reverse out))
                   (let ([text (walk-extensions query (car sources) #t counts size (lambda () size) compatible? refines?)])
                     (if (or (not text) (hashtable-ref seen text #f)) (candidates (cdr sources) out)
                       (begin (hashtable-set! seen text #t) (candidates (cdr sources) (cons text out)))))))))))]))
)
