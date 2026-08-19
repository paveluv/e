;; diff.e -- line diffs and three-way merge, for the e editor.
;;
;; A pure library (no init!): a patience diff over line vectors and
;; the diff3 merge built on it.  The core imports it for the
;; stale-file guard; anything else may use it too.

(library (diff)
  (export diff-matches merge3)
  (import (chezscheme))

  ;;; Patience diff --------------------------------------------------------------

  (define (common-prefix a alo ahi b blo bhi)
    (let loop ([n 0])
      (if (and (< (+ alo n) ahi) (< (+ blo n) bhi)
               (string=? (vector-ref a (+ alo n)) (vector-ref b (+ blo n))))
          (loop (+ n 1))
          n)))

  (define (common-suffix a alo ahi b blo bhi)
    (let loop ([n 0])
      (if (and (< alo (- ahi n)) (< blo (- bhi n))
               (string=? (vector-ref a (- ahi n 1))
                         (vector-ref b (- bhi n 1))))
          (loop (+ n 1))
          n)))

  (define (unique-anchors a alo ahi b blo bhi)
    ;; Lines unique within both ranges, as ((ai . bi) ...) in a-order.
    (let ([table (make-hashtable string-hash string=?)])
      ;; count occurrences and remember positions: (a-count a-pos b-count b-pos)
      (do ([i alo (+ i 1)]) ((= i ahi))
        (let* ([line (vector-ref a i)]
               [e (hashtable-ref table line '(0 #f 0 #f))])
          (hashtable-set! table line
                          (list (+ (car e) 1) i (caddr e) (cadddr e)))))
      (do ([i blo (+ i 1)]) ((= i bhi))
        (let* ([line (vector-ref b i)]
               [e (hashtable-ref table line '(0 #f 0 #f))])
          (hashtable-set! table line
                          (list (car e) (cadr e) (+ (caddr e) 1) i))))
      (let loop ([i alo] [acc '()])
        (if (= i ahi)
            (reverse acc)
            (let ([e (hashtable-ref table (vector-ref a i) '(0 #f 0 #f))])
              (if (and (= (car e) 1) (= (caddr e) 1))
                  (loop (+ i 1) (cons (cons i (cadddr e)) acc))
                  (loop (+ i 1) acc)))))))

  (define (longest-increasing pairs)
    ;; The longest chain of anchors increasing on both sides; pairs
    ;; come a-sorted, so filter to a b-increasing subsequence
    ;; (patience sorting, O(n^2) -- honest prototype speed).
    (let* ([v (list->vector pairs)]
           [n (vector-length v)]
           [len (make-vector n 1)]
           [prev (make-vector n -1)])
      (if (= n 0)
          '()
          (begin
            (do ([i 1 (+ i 1)]) ((= i n))
              (do ([j 0 (+ j 1)]) ((= j i))
                (when (and (< (cdr (vector-ref v j)) (cdr (vector-ref v i)))
                           (>= (vector-ref len j) (vector-ref len i)))
                  (vector-set! len i (+ (vector-ref len j) 1))
                  (vector-set! prev i j))))
            (let ([best 0])
              (do ([i 1 (+ i 1)]) ((= i n))
                (when (> (vector-ref len i) (vector-ref len best))
                  (set! best i)))
              (let loop ([i best] [acc '()])
                (if (< i 0)
                    acc
                    (loop (vector-ref prev i)
                          (cons (vector-ref v i) acc)))))))))

  (define (diff-matches a b)
    ;; Matching line pairs ((ai . bi) ...) between vectors a and b,
    ;; increasing on both sides: common prefix and suffix, then
    ;; recursion between unique-line anchors (patience diff).
    (let walk ([alo 0] [ahi (vector-length a)]
               [blo 0] [bhi (vector-length b)])
      (let* ([pre (common-prefix a alo ahi b blo bhi)]
             [alo (+ alo pre)] [blo (+ blo pre)]
             [suf (common-suffix a alo ahi b blo bhi)]
             [ahi (- ahi suf)] [bhi (- bhi suf)]
             [head (let loop ([n 0] [acc '()])
                     (if (= n pre)
                         (reverse acc)
                         (loop (+ n 1)
                               (cons (cons (+ alo (- n pre))
                                           (+ blo (- n pre)))
                                     acc))))]
             [tail (let loop ([n 0] [acc '()])
                     (if (= n suf)
                         acc
                         (loop (+ n 1)
                               (cons (cons (+ ahi n) (+ bhi n)) acc))))]
             [anchors (longest-increasing
                        (unique-anchors a alo ahi b blo bhi))])
        (append
          head
          (if (null? anchors)
              '()
              (let loop ([alo alo] [blo blo] [anchors anchors] [acc '()])
                (if (null? anchors)
                    (apply append (reverse (cons (walk alo ahi blo bhi) acc)))
                    (let ([anchor (car anchors)])
                      (loop (+ (car anchor) 1) (+ (cdr anchor) 1)
                            (cdr anchors)
                            (cons (list anchor)
                                  (cons (walk alo (car anchor)
                                              blo (cdr anchor))
                                        acc)))))))
          tail))))

  ;;; Three-way merge -------------------------------------------------------------

  (define (merge3 base mine theirs)
    ;; Merge line vectors: three values -- the merged lines (a list),
    ;; the conflict count, and a report of the changed chunks, each
    ;; (kind base-lines mine-lines theirs-lines) with kind mine,
    ;; theirs, both, or conflict.  Regions only one side touched take
    ;; that side; regions both touched identically take either; the
    ;; rest conflict, marked <<<<<<< buffer / ======= / >>>>>>> disk.
    (let* ([n (vector-length base)]
           [m1 (make-vector (+ n 1) #f)]
           [m2 (make-vector (+ n 1) #f)])
      (define (slice v lo hi)
        (let loop ([i (- hi 1)] [acc '()])
          (if (< i lo) acc (loop (- i 1) (cons (vector-ref v i) acc)))))
      (for-each (lambda (p) (vector-set! m1 (car p) (cdr p)))
                (diff-matches base mine))
      (for-each (lambda (p) (vector-set! m2 (car p) (cdr p)))
                (diff-matches base theirs))
      (vector-set! m1 n (vector-length mine))
      (vector-set! m2 n (vector-length theirs))
      (let loop ([b 0] [i 0] [j 0] [out '()] [conflicts 0] [report '()])
        (cond
          [(and (>= b n) (>= i (vector-length mine)) (>= j (vector-length theirs)))
           (values (reverse out) conflicts (reverse report))]
          [(and (< b n)
                (eqv? (vector-ref m1 b) i)
                (eqv? (vector-ref m2 b) j))
           ;; all three aligned: the line is common ground
           (loop (+ b 1) (+ i 1) (+ j 1)
                 (cons (vector-ref base b) out) conflicts report)]
          [else
           ;; a changed chunk: up to the next base line aligned on
           ;; both sides (or the ends)
           (let find ([s b])
             (if (and (< s n)
                      (not (and (vector-ref m1 s) (vector-ref m2 s))))
                 (find (+ s 1))
                 (let* ([i2 (vector-ref m1 s)]
                        [j2 (vector-ref m2 s)]
                        [bs (slice base b s)]
                        [ms (slice mine i i2)]
                        [ts (slice theirs j j2)])
                   (cond
                     [(equal? ms bs)      ; only theirs changed
                      (loop s i2 j2 (append (reverse ts) out) conflicts
                            (cons (list 'theirs bs ms ts) report))]
                     [(equal? ts bs)      ; only mine changed
                      (loop s i2 j2 (append (reverse ms) out) conflicts
                            (cons (list 'mine bs ms ts) report))]
                     [(equal? ms ts)      ; both, identically
                      (loop s i2 j2 (append (reverse ms) out) conflicts
                            (cons (list 'both bs ms ts) report))]
                     [else
                      (loop s i2 j2
                            (append
                              (reverse
                                (append '("<<<<<<< buffer") ms
                                        '("=======") ts
                                        '(">>>>>>> disk")))
                              out)
                            (+ conflicts 1)
                            (cons (list 'conflict bs ms ts) report))]))))]))))
)
