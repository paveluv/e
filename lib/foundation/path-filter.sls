;; Literal path keys: unordered, disjoint occurrences, with root anchoring.
(import (only (foundation edoc) elibrary))
(elibrary (foundation path-filter)
  (export anchored? complete format-keys matcher parse possible? ranges)
  (import (chezscheme) (prefix (foundation string) string:))

  (edoc "Whether a key starts at the filesystem root."
        (key string "the key") (returns boolean))
  (define (anchored? key) (string:prefix? "/" key))

  (edoc "Read whitespace-separated literal keys, accepting quoted Scheme strings for names containing spaces; a leading tilde expands to home. An unfinished quote remains editable."
        (text string "the filter") (home string "the home directory") (returns list))
  (define (parse text home)
    (let ([n (string-length text)])
      (define (expand key)
        (if (string:prefix? "~" key) (string-append home (string:tail key 1)) key))
      (let loop ([at 0] [out '()])
        (cond [(= at n) (reverse out)]
              [(char-whitespace? (string-ref text at)) (loop (+ at 1) out)]
              [(char=? (string-ref text at) #\")
               (let quote-end ([i (+ at 1)] [escaped? #f])
                 (cond [(= i n) (loop n (if (= i (+ at 1)) out (cons (substring text (+ at 1) n) out)))]
                       [(and (not escaped?) (char=? (string-ref text i) #\"))
                        (let ([key (guard (ex [else (substring text (+ at 1) i)])
                                     (read (open-input-string (substring text at (+ i 1)))))])
                          (loop (+ i 1) (if (string=? key "") out (cons key out))))]
                       [else (quote-end (+ i 1) (and (not escaped?) (char=? (string-ref text i) #\\)))]))]
              [else
               (let end ([i (+ at 1)])
                 (if (or (= i n) (char-whitespace? (string-ref text i)))
                     (loop i (cons (expand (substring text at i)) out)) (end (+ i 1))))]))))

  (edoc "Spell literal keys with one separating space, quoting keys when necessary."
        (keys list "the keys") (returns string))
  (define (format-keys keys)
    (string:join
      (map (lambda (key)
             (if (or (string:prefix? "\"" key) (string:prefix? "~" key)
                     (exists char-whitespace? (string->list key))) (format "~s" key) key)) keys) " "))

  (define (disjoint? range used)
    (for-all (lambda (other) (or (<= (cdr range) (car other)) (<= (cdr other) (car range)))) used))

  (edoc "Literal key occurrences as character ranges [start, end); a directory prefix also includes unfinished occurrences at its end. Rooted keys only start at zero."
        (keys list "expanded keys") (path string "the path")
        (prefix? boolean "whether the path can continue below this directory") (returns list))
  (define (ranges keys path prefix?)
    (let ([n (string-length path)])
      (apply append
        (map (lambda (key)
               (let ([size (string-length key)] [search (string:searcher key #t)])
                 (if (anchored? key)
                     (let ([end (min size n)])
                       (if (and (or prefix? (<= size n))
                                (string-ci=? (substring key 0 end) (substring path 0 end)))
                           (list (cons 0 end)) '()))
                     (let full ([from 0] [out '()])
                       (let ([at (and (positive? size) (search path from n))])
                         (if at (full (+ at 1) (cons (cons at (+ at size)) out))
                             (let partial ([size (if prefix? (min n (- size 1)) 0)] [out out])
                               (if (<= size 0) (reverse out)
                                   (partial (- size 1)
                                     (if (string-ci=? (substring key 0 size) (substring path (- n size) n))
                                         (cons (cons (- n size) n) out) out)))))))))) keys))))

  (edoc "Compile a case-insensitive predicate: every key occupies a separate literal range; leading slash keys are anchored at zero."
        (keys list "the expanded keys") (returns procedure))
  (define (matcher keys)
    (let* ([anchors (filter anchored? keys)]
           [anchor (and (pair? anchors) (car anchors))]
           [start (if anchor (string-length anchor) 0)]
           [keys (sort string-ci<? (filter (lambda (key) (not (anchored? key))) keys))]
           ;; Group equal keys: placing identical occurrences in increasing
           ;; order removes factorial permutations without changing matches.
           [groups (let group ([keys keys])
                     (if (null? keys) '()
                         (let count ([rest (cdr keys)] [n 1])
                           (if (and (pair? rest) (string-ci=? (car keys) (car rest))) (count (cdr rest) (+ n 1))
                               (cons (list (string:searcher (car keys) #t) (string-length (car keys)) n)
                                 (group rest))))))])
      (lambda (path)
        (and (<= (length anchors) 1)
             (or (not anchor) (and (<= start (string-length path)) (string-ci=? anchor (substring path 0 start))))
             (cond [(null? groups) #t]
                   [(null? (cdr groups))
                    ;; The usual one-key search needs no occurrence lists.
                    (let loop ([from start] [left (caddar groups)])
                      (or (zero? left)
                          (let ([at ((caar groups) path from (string-length path))])
                            (and at (loop (+ at (cadar groups)) (- left 1))))))]
                   [else
                    (let gather ([rest groups] [options '()])
                      (if (null? rest)
                          (let solve ([options (sort (lambda (a b) (< (length (cdr a)) (length (cdr b)))) options)] [used '()])
                            (or (null? options)
                                (let choose ([ranges (cdar options)] [left (caar options)] [used used])
                                  (if (zero? left) (solve (cdr options) used)
                                      (and (pair? ranges)
                                           (or (and (disjoint? (car ranges) used)
                                                    (choose (cdr ranges) (- left 1) (cons (car ranges) used)))
                                               (choose (cdr ranges) left used)))))))
                          (let* ([key (car rest)] [size (cadr key)] [needed (caddr key)])
                            (let occurrences ([from start] [out '()] [edge start] [count 0])
                              (let ([at ((car key) path from (string-length path))])
                                (if at
                                    (occurrences (+ at 1) (cons (cons at (+ at size)) out)
                                      (if (>= at edge) (+ at size) edge) (if (>= at edge) (+ count 1) count))
                                    (and (>= count needed) (gather (cdr rest) (cons (cons needed (reverse out)) options)))))))))])))))

  (edoc "Whether a directory prefix can still reach every root-anchored key."
        (keys list "the expanded keys") (prefix string "the directory, with its trailing slash") (returns boolean))
  (define (possible? keys prefix)
    (and (<= (length (filter anchored? keys)) 1)
      (for-all (lambda (key)
                 (or (not (anchored? key))
                   (let ([n (min (string-length key) (string-length prefix))])
                     (string-ci=? (substring key 0 n) (substring prefix 0 n))))) keys)))

  (edoc "Maximize literal characters minus separating spaces over equivalent filters; fewer spaces break ties. Cancellation returns the original keys."
        (keys list "the original expanded keys")
        (walk procedure "(walk visit) traverses matching paths, stopping when visit returns #f")
        (acceptable? procedure "whether the proposed keys preserve visibility and scope")
        (cancelled? thunk "whether to abandon computation") (returns list))
  (define (complete keys walk acceptable? cancelled?)
    ;; Solve against witnesses, then verify the proposed optimum against
    ;; every result. A counterexample tightens the next solution. Once a
    ;; witness optimum fits all results it is also the global optimum;
    ;; result count affects verification time, never storage or UI work.
    (call/cc
      (lambda (return)
        (define steps 0)
        (define (check!)
          (set! steps (+ steps 1))
          (when (and (zero? (mod steps 256)) (cancelled?)) (return keys)))
        (define base #f)
        (define last #f)
        (define samples '())
        (define seen 0)
        (define next-sample 1)
        (walk (lambda (path)
                (check!)
                (set! seen (+ seen 1)) (set! last path)
                ;; Spread the first constraints across the stream instead
                ;; of solving a hard optimum for two nearly identical files.
                ;; Sampling only seeds the proof; verification still visits
                ;; every result and adds any missing constraint.
                (when (= seen next-sample)
                  (set! samples (cons path samples)) (set! next-sample (* next-sample 2)))
                (when (or (not base) (< (string-length path) (string-length base))) (set! base path)) #t))
        (if (not base) keys
            (let refine ([witnesses (cons base (cons last samples))])
              (when (cancelled?) (return keys))
              (let* ([answer (complete-witnesses keys witnesses acceptable? cancelled?)]
                     [match? (matcher answer)] [counterexample #f])
                (walk (lambda (path)
                        (check!)
                        (or (match? path) (begin (set! counterexample path) #f))))
                (if counterexample (refine (cons counterexample witnesses)) answer)))))))

  (define (complete-witnesses keys paths acceptable? cancelled?)
    ;; Each key contributes its literal characters, and each separating
    ;; space costs one. Quoting does not affect the score. Every refinement
    ;; must imply the original filter, so it
    ;; cannot add unseen matches or move a match to an earlier directory.
    (if (null? paths) keys
        (call/cc
          (lambda (return)
            (define (check!) (when (cancelled?) (return keys)))
            (define (fold text) (list->string (map char-foldcase (string->list text))))
            (define names (sort (lambda (a b) (< (string-length a) (string-length b)))
                            (map fold paths)))
            (define base (car names))
            (define n (string-length base))
            (define original (matcher keys))
            (define (implies? parts)
              ;; NUL cannot occur in a filesystem name. It separates keys
              ;; without allowing an old literal to cross between them.
              (original (string:join parts (string #\nul))))
            (define alphabet (make-eqv-hashtable))
            (define size 0)
            (define (index c)
              (or (hashtable-ref alphabet c #f)
                  (let ([i size]) (set! size (+ size 1)) (hashtable-set! alphabet c i) i)))
            (define unused (string-for-each index base))
            (define (counts name)
              (let ([v (make-vector size 0)])
                (string-for-each (lambda (c)
                                   (let ([i (hashtable-ref alphabet c #f)])
                                     (when i (vector-set! v i (+ 1 (vector-ref v i)))))) name) v))
            (define common (counts base))
            (define best keys)
            (define best-size (- (apply + (map string-length keys)) (max 0 (- (length keys) 1))))
            (define best-count (length keys))
            (define ends (make-vector n 0))
            (define suffix-score (make-vector (+ n 1) 0))
            (define suffix-count (make-vector (+ n 1) 0))
            (define longest 0)
            (define limit #f)
            (define lower #f)
            (define ceiling #f)
            (define (fits? parts)
              (let ([match? (matcher parts)])
                (for-all (lambda (name) (check!) (match? name)) names)))
            (define (consider! parts score)
              (let* ([count (length parts)] [score (- score (max 0 (- count 1)))])
                (when (and (or (> score best-size) (and (= score best-size) (< count best-count)))
                           (implies? parts) (acceptable? parts))
                  (set! best parts) (set! best-size score) (set! best-count count)
                  (when (and ceiling (= score ceiling) (= count lower)) (return best)))))
            ;; Identical paths and a single result have a direct answer.
            (when (and (for-all (lambda (name) (string=? name base)) names)
                       (implies? (list base)) (acceptable? (list base))) (return (list base)))
            (for-each (lambda (name)
                        (check!)
                        (let ([v (counts name)])
                          (do ([i 0 (+ i 1)]) ((= i size))
                            (vector-set! common i (min (vector-ref common i) (vector-ref v i)))))) (cdr names))
            ;; At a fixed position, a missing common substring rules out
            ;; every extension of it. Store only its maximal endpoint.
            (do ([at 0 (+ at 1)]) ((= at n))
              (check!)
              (let end ([to (+ at 1)])
                (when (and (<= to n) (or (zero? at) (not (char=? (string-ref base at) #\/)))
                           (fits? (list (substring base at to))))
                  (vector-set! ends at to)
                  (set! longest (max longest (- to at))) (end (+ to 1)))))
            (set! limit (apply + (vector->list common)))
            (set! lower (if (zero? longest) (+ limit 1) (div (+ limit longest -1) longest)))
            (set! ceiling (- limit (max 0 (- lower 1))))
            ;; A shorter spelling with fewer keys can tie the score of full
            ;; coverage: ".sls" beats "o .sls". Bound key count by score too.
            (set! lower (if (<= longest 1) 1 (max 1 (div (+ ceiling longest -3) (- longest 1)))))
            ;; A character multiset is a loose bound for long paths: it
            ;; ignores both missing substrings and the spaces between keys.
            ;; Solve the independent interval problem once, from the end,
            ;; to bound every remaining suffix before attempting packing.
            (do ([at (- n 1) (- at 1)]) ((negative? at))
              (vector-set! suffix-score at (vector-ref suffix-score (+ at 1)))
              (vector-set! suffix-count at (vector-ref suffix-count (+ at 1)))
              (do ([to (+ at 1) (+ to 1)]) ((> to (vector-ref ends at)))
                (let ([score (+ (- to at 1) (vector-ref suffix-score to))]
                      [count (+ 1 (vector-ref suffix-count to))])
                  (when (or (> score (vector-ref suffix-score at))
                            (and (= score (vector-ref suffix-score at)) (< count (vector-ref suffix-count at))))
                    (vector-set! suffix-score at score) (vector-set! suffix-count at count)))))
            (let search ([at 0] [left common] [room limit] [parts '()] [score 0])
              (check!)
              (when (and (< at n)
                         (>= (- (+ score (min room (- n at))) (max 0 (- (length parts) 1))) best-size)
                         (let ([upper (+ (- score (length parts)) 1 (vector-ref suffix-score at))])
                           (or (> upper best-size)
                               (and (= upper best-size) (< (+ (length parts) (vector-ref suffix-count at)) best-count))))
                         (implies? (append parts (list (substring base at n)))))
                (let try ([to (vector-ref ends at)])
                  (when (> to at)
                    (let* ([key (substring base at to)] [used (counts key)]
                           [next (make-vector size)] [allowed? #t])
                      (do ([i 0 (+ i 1)]) ((= i size))
                        (let ([remain (- (vector-ref left i) (vector-ref used i))])
                          (when (negative? remain) (set! allowed? #f)) (vector-set! next i remain)))
                      (let ([parts (append parts (list key))] [score (+ score (- to at))])
                        (when (and allowed? (fits? parts))
                          (consider! parts score)
                          (when (or (< best-size ceiling) (< (length parts) best-count))
                            (search to next (- room (- to at)) parts score)))))
                    (try (- to 1))))
                (search (+ at 1) left room parts score)))
            best))))
)
