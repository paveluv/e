;; Symbol completion by disjoint, boundary-starting segments. One matching
;; relation governs admission, ranking alignments and safe normalization.

(import (only (edoc) elibrary))
(elibrary (fuzzy)
  (export matches expansions rank
          (rename (match-name name) (match-score score) (match-fragments fragments)))
  (import (rnrs) (only (chezscheme) make-mutex with-mutex vector-copy iota void))

  (edoc "A name a query matched, with its rank and where the query's characters landed."
        (name string "the matched name")
        (score list "the rank components, smaller first")
        (fragments list "(query-index name-index length) runs of matched characters"))
  (define-record-type match
    (fields name score fragments))
  ;; A source is a name prepared once for alignment: the indices where its
  ;; parts start (the beginning and the position after every separator),
  ;; its characters as sorted code points, and a presence mask over the
  ;; slots below. Every part starts at a boundary, and every character stays
  ;; in a segment.
  (define-record-type source (fields name starts codes mask))
  (define (separator? c)
    ;; The character before a word start: anything but a letter or a digit,
    ;; so that a query aligns with the parts of set-buffer-name!, of
    ;; (head:current-buffer) and of (buffer "*scratch*") alike.
    (not (or (char-alphabetic? c) (char-numeric? c))))

  (define (slot c)
    ;; One mask bit per common symbol character; everything else shares one.
    (let ([n (char->integer c)])
      (cond [(fx<=? 97 n 122) (fx- n 97)]
            [(fx<=? 48 n 57) (fx+ 26 (fx- n 48))]
            [else
             (case c
               [(#\-) 36] [(#\:) 37] [(#\!) 38] [(#\?) 39] [(#\*) 40] [(#\<) 41] [(#\>) 42]
               [(#\=) 43] [(#\/) 44] [(#\+) 45] [(#\.) 46] [(#\_) 47] [(#\$) 48] [(#\%) 49]
               [(#\&) 50] [(#\^) 51] [(#\~) 52] [(#\@) 53] [(#\#) 54] [else 55])])))

  (define (build-source text)
    (let* ([n (string-length text)] [codes (make-vector n 0)])
      (let scan ([i 0] [mask 0] [starts (if (fx>? n 0) '(0) '())])
        (if (fx=? i n)
            (begin
              (vector-sort! fx<? codes)
              (make-source text (list->vector (reverse starts)) codes mask))
            (let ([c (string-ref text i)])
              (vector-set! codes i (char->integer c))
              (scan (fx+ i 1)
                    (fxior mask (fxarithmetic-shift-left 1 (slot c)))
                    (if (and (separator? c) (fx<? (fx+ i 1) n)) (cons (fx+ i 1) starts) starts)))))))

  (define (part-end source p)
    (let ([starts (source-starts source)])
      (if (fx<? (fx+ p 1) (vector-length starts))
          (vector-ref starts (fx+ p 1))
          (string-length (source-name source)))))

  ;; Names recur across keystrokes: keep their sources, keyed by the symbol
  ;; or string a caller passes, within a bound. One lock serializes callers.
  (define lock (make-mutex))
  (define by-symbol (make-eq-hashtable))
  (define by-string (make-hashtable string-hash string=?))
  (define (text-of name) (if (symbol? name) (symbol->string name) name))
  (define (source-of name)
    (let ([table (if (symbol? name) by-symbol by-string)])
      (or (hashtable-ref table name #f)
          (let ([source (build-source (text-of name))])
            (when (fx>? (hashtable-size table) 16384) (hashtable-clear! table))
            (hashtable-set! table name source)
            source))))

  (define (codes-within? part whole)
    ;; Is the sorted multiset part contained in the sorted multiset whole?
    (let ([m (vector-length part)] [n (vector-length whole)])
      (let walk ([i 0] [j 0])
        (cond [(fx=? i m) #t]
              [(fx=? j n) #f]
              [(fx<? (vector-ref whole j) (vector-ref part i)) (walk i (fx+ j 1))]
              [(fx=? (vector-ref whole j) (vector-ref part i)) (walk (fx+ i 1) (fx+ j 1))]
              [else #f]))))

  (define (align query source)
    ;; The query arrives prepared like a source: its mask and sorted codes
    ;; reject most names here, before anything is allocated for a search.
    (and (fx<=? (string-length (source-name query)) (string-length (source-name source)))
         (fxzero? (fxand (source-mask query) (fxnot (source-mask source))))
         (codes-within? (source-codes query) (source-codes source))
         (search query source)))

  (define (search query source)
    ;; Match longest leading segments first, then earliest candidate position.
    ;; All characters, including ':' and '-', stay inside these literal runs.
    ;; Backtracking keeps eligibility independent of a greedy choice; the bit
    ;; mask prevents reuse of any character occurrence. The failure memo
    ;; exists only once a choice has to be undone.
    (let* ([text (source-name query)] [m (string-length text)]
           [name (source-name source)] [n (string-length name)]
           [starts (source-starts source)] [parts (vector-length starts)]
           [failed #f] [stride (bitwise-arithmetic-shift-left 1 n)])
      (let solve ([at 0] [used 0])
        (cond
          [(fx=? at m) '()]
          [(and failed (hashtable-ref failed (+ used (* at stride)) #f)) #f]
          [else
           (let ([count 0] [option-start (make-vector parts 0)] [option-size (make-vector parts 0)])
             ;; Every part whose run from its start matches, longest run
             ;; first and earliest start among equals, as the parts come.
             (do ([p 0 (fx+ p 1)]) ((fx=? p parts))
                 (let* ([start (vector-ref starts p)]
                        [size (let prefix ([size 0])
                                (if (and (fx<? (fx+ at size) m) (fx<? (fx+ start size) n)
                                         (not (bitwise-bit-set? used (fx+ start size)))
                                         (char=? (string-ref text (fx+ at size))
                                                 (string-ref name (fx+ start size))))
                                    (prefix (fx+ size 1)) size))])
                   (when (fx>? size 0)
                     (let insert ([i count])
                       (if (and (fx>? i 0) (fx<? (vector-ref option-size (fx- i 1)) size))
                           (begin
                             (vector-set! option-size i (vector-ref option-size (fx- i 1)))
                             (vector-set! option-start i (vector-ref option-start (fx- i 1)))
                             (insert (fx- i 1)))
                           (begin (vector-set! option-size i size) (vector-set! option-start i start))))
                     (set! count (fx+ count 1)))))
             (or (let candidates ([i 0])
                   (and (fx<? i count)
                        (let ([start (vector-ref option-start i)])
                          (or (let lengths ([size (vector-ref option-size i)])
                                (and (fx>? size 0)
                                     (let* ([mask (bitwise-arithmetic-shift-left
                                                    (- (bitwise-arithmetic-shift-left 1 size) 1) start)]
                                            [tail (solve (fx+ at size) (bitwise-ior used mask))])
                                       (if tail
                                           (cons (list at start size) tail)
                                           (lengths (fx- size 1))))))
                              (candidates (fx+ i 1))))))
                 (begin
                   (unless failed (set! failed (make-eqv-hashtable)))
                   (hashtable-set! failed (+ used (* at stride)) #t)
                   #f)))]))))

  (define (score fragments size name-size)
    ;; Fewer segments, fewer reordered pairs, an earlier first character and
    ;; a tighter span rank ahead; a shorter name breaks the remaining ties.
    (if (null? fragments) (list 0 0 0 0 name-size)
        (let loop ([xs fragments] [count 0] [inversions 0] [prefix name-size] [end 0])
          (if (null? xs)
              (list (fx- count 1) inversions prefix (fx- (fx- end prefix) size) name-size)
              (let* ([f (car xs)] [start (cadr f)])
                (loop (cdr xs) (fx+ count 1)
                      (fx+ inversions
                           (let later ([ys (cdr xs)] [k 0])
                             (if (null? ys) k
                                 (later (cdr ys) (if (fx>? start (cadr (car ys))) (fx+ k 1) k)))))
                      (fxmin prefix start) (fxmax end (fx+ start (caddr f)))))))))

  (define (score<? a b)
    (cond [(null? a) #f] [(< (car a) (car b)) #t] [(> (car a) (car b)) #f]
          [else (score<? (cdr a) (cdr b))]))

  (edoc "The names a query matches as subsequences, best first: fewer segments, fewer reorderings, an earlier first character, a tighter span and a shorter name rank ahead."
        (query string "the typed characters")
        (names (list-of (or symbol string)) "the candidates")
        (returns (list-of (record match))))
  (define (rank query names)
    ;; Names may be symbols or strings; every match names a string.
    (with-mutex lock
      (if (string=? query "")
          (map (lambda (name) (make-match name (list 0 0 0 0 (string-length name)) '()))
               (list-sort string<? (map text-of names)))
          (let ([prepared (build-source query)] [size (string-length query)])
            (list-sort
              (lambda (a b)
                (if (equal? (match-score a) (match-score b))
                    (string<? (match-name a) (match-name b))
                    (score<? (match-score a) (match-score b))))
              ;; Gathered in reverse: the sort below settles every order that
              ;; matters, and identical names are identical matches.
              (fold-left
                (lambda (out name)
                  (let* ([source (source-of name)] [fragments (align prepared source)])
                    (if fragments
                        (cons (make-match (source-name source)
                                          (score fragments size (string-length (source-name source)))
                                          fragments)
                              out)
                        out)))
                '() names))))))

  (edoc "The names a query matches, best first."
        (query string "the typed characters")
        (names (list-of (or symbol string)) "the candidates")
        (returns (list-of string)))
  (define (matches query names)
    (map match-name (rank query names)))

  (define (common-alphabet sources)
    ;; The distinct characters of the first source, sorted, each with the
    ;; count every source can spare: the letters any extension may use.
    (let* ([first (source-codes (car sources))] [n (vector-length first)])
      (let distinct ([i 0] [codes '()] [counts '()])
        (if (fx=? i n)
            (let ([codes (list->vector (reverse codes))] [counts (list->vector (reverse counts))])
              (for-each (lambda (source) (lower-counts! codes counts (source-codes source))) (cdr sources))
              (values codes counts))
            (let ([c (vector-ref first i)])
              (if (and (pair? codes) (fx=? (car codes) c))
                  (distinct (fx+ i 1) codes (cons (fx+ (car counts) 1) (cdr counts)))
                  (distinct (fx+ i 1) (cons c codes) (cons 1 counts))))))))

  (define (lower-counts! codes counts whole)
    ;; Cap each count by the occurrences of its code in the sorted vector whole.
    (let ([m (vector-length codes)] [n (vector-length whole)])
      (let walk ([i 0] [j 0] [seen 0])
        (cond
          [(fx=? i m) (void)]
          [(or (fx=? j n) (fx>? (vector-ref whole j) (vector-ref codes i)))
           (vector-set! counts i (fxmin (vector-ref counts i) seen))
           (walk (fx+ i 1) j 0)]
          [(fx=? (vector-ref whole j) (vector-ref codes i)) (walk i (fx+ j 1) (fx+ seen 1))]
          [else (walk i (fx+ j 1) seen)]))))

  (define (index-of codes c)
    (let ([code (char->integer c)] [n (vector-length codes)])
      (let find ([i 0])
        (cond [(fx=? i n) #f]
              [(fx=? (vector-ref codes i) code) i]
              [(fx>? (vector-ref codes i) code) #f]
              [else (find (fx+ i 1))]))))

  (define (count-of codes counts c)
    (let ([i (index-of codes c)]) (if i (vector-ref counts i) 0)))

  (define (walk-extensions query source ordered? codes counts limit minimum compatible? accept)
    ;; Every match is a permutation of prefixes of the boundary-starting parts.
    ;; Ordered walks produce readable projections for the Tab cycle. An
    ;; unrestricted walk establishes the maximum length, even when no such
    ;; projection can express it. Prefix rejection prunes every continuation.
    ;; Parts are indices into the source; the letters left are counted like
    ;; codes.
    (define name (source-name source))
    (define (start-of p) (vector-ref (source-starts source) p))
    (define (end-of p) (part-end source p))
    (define (capacity parts left)
      ;; A missing character blocks the rest of its subword, even if those
      ;; later characters occur elsewhere. Counting that tail as available
      ;; makes the longest-extension proof needlessly enumerate permutations.
      (fold-left
        (lambda (n p)
          (fx+ n (let prefix ([at (start-of p)])
                   (if (and (fx<? at (end-of p)) (fx>? (count-of codes left (string-ref name at)) 0))
                       (prefix (fx+ at 1)) (fx- at (start-of p))))))
        0 parts))
    (define (viable? text parts)
      ;; Extra boundaries make the unused parts a superset of every possible
      ;; continuation. Reject prefixes that cannot retain the original query
      ;; even there, instead of proving this again for every permutation.
      (align query
        (build-source
          (fold-right
            (lambda (p tail) (string-append tail "-" (substring name (start-of p) (end-of p))))
            text parts))))
    (call-with-current-continuation
      (lambda (done)
        (let walk ([parts (iota (vector-length (source-starts source)))] [text ""] [left counts])
          (let ([size (string-length text)])
            (when (and (fx>=? size (minimum)) (accept text)) (done text))
            (when (and (fx<? size limit)
                       (fx>=? (fx+ size (capacity parts left)) (minimum))
                       (viable? text parts))
              (let choices ([rest parts] [seen '()])
                (when (pair? rest)
                  (let* ([p (car rest)] [start (start-of p)] [stop (end-of p)]
                         [spelling (substring name start stop)]
                         [remaining (vector-copy left)]
                         [end
                          (let take ([end start])
                            (if (and (fx<? end stop) (fx<? (fx+ size (fx- end start)) limit)
                                     (fx>? (count-of codes remaining (string-ref name end)) 0))
                                (let ([i (index-of codes (string-ref name end))])
                                  (vector-set! remaining i (fx- (vector-ref remaining i) 1))
                                  (take (fx+ end 1)))
                                end))])
                    (unless (member spelling seen)
                      (let lengths ([end end])
                        (when (fx>? end start)
                          (let ([next (string-append text (substring name start end))])
                            (when (compatible? next)
                              (walk (if ordered? (cdr rest) (remq p parts)) next remaining)))
                          (let ([i (index-of codes (string-ref name (fx- end 1)))])
                            (vector-set! remaining i (fx+ (vector-ref remaining i) 1)))
                          (lengths (fx- end 1)))))
                    ;; Identical parts are interchangeable only in unrestricted
                    ;; permutations; ordered projections retain their positions.
                    (choices (cdr rest) (if ordered? seen (cons spelling seen)))))))))
        #f)))

  (edoc "The safe extensions of a query: the longer strings every current match still matches, the query itself included; a predicate over texts confines them to what the caller can insert."
        (query string "the typed characters")
        (names (list-of (or symbol string)) "the current matches")
        (acceptable? procedure "(acceptable? text) admitting an extension; omitted, every text is")
        (returns (list-of string)))
  (define expansions
    (case-lambda
      [(query names) (extensions query names (lambda (text) #t))]
      [(query names acceptable?) (extensions query names acceptable?)]))

  (define (extensions query names acceptable?)
    ;; A safe extension E satisfies query <= E <= every current match under
    ;; this same relation. Boundary-starting alignments compose, so transitivity
    ;; guarantees that E cannot introduce a name the query did not match.
    (with-mutex lock
      (cond [(null? names) (list query)] [(null? (cdr names)) (list (text-of (car names)))]
        [else
         (let*-values ([(sources) (map source-of names)]
                       [(codes counts) (common-alphabet sources)])
           (let* ([limit (let sum ([i 0] [n 0])
                           (if (fx=? i (vector-length counts)) n (sum (fx+ i 1) (fx+ n (vector-ref counts i)))))]
                  [prepared (build-source query)]
                  [known (make-hashtable string-hash string=?)]
                  [best query] [size (string-length query)])
             (define (compatible? text)
               (let ([hit (hashtable-ref known text #f)])
                 (if hit (car hit)
                     (let* ([candidate (build-source text)]
                            [yes? (and (acceptable? text)
                                       (for-all (lambda (source) (and (align candidate source) #t)) sources))])
                       (hashtable-set! known text (list yes?)) yes?))))
             (define (refines? text) (align prepared (build-source text)))
             (when (fx>? limit size)
               (walk-extensions
                 prepared
                 (let shortest ([rest (cdr sources)] [found (car sources)])
                   (cond [(null? rest) found]
                         [(fx<? (string-length (source-name (car rest))) (string-length (source-name found)))
                          (shortest (cdr rest) (car rest))]
                         [else (shortest (cdr rest) found)]))
                 #f codes counts limit (lambda () (fx+ size 1)) compatible?
                 (lambda (text)
                   (and (refines? text)
                        (begin (set! best text) (set! size (string-length text)) (fx=? size limit))))))
             (let ([whole (filter (lambda (source)
                                    (and (fx=? (string-length (source-name source)) size)
                                         (compatible? (source-name source))))
                                  sources)])
               (if (pair? whole) (map source-name whole)
                   (let ([seen (make-hashtable string-hash string=?)])
                     (let candidates ([sources sources] [out '()])
                       (if (null? sources) (if (null? out) (list best) (reverse out))
                           (let ([text (walk-extensions prepared (car sources) #t codes counts size
                                                        (lambda () size) compatible? refines?)])
                             (if (or (not text) (hashtable-ref seen text #f)) (candidates (cdr sources) out)
                                 (begin (hashtable-set! seen text #t)
                                        (candidates (cdr sources) (cons text out))))))))))))])))
)
