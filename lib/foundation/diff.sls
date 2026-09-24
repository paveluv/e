;; diff.sls -- line diffs, for the e editor.
;;
;; A pure library (no init!): a patience diff over line vectors, the
;; matching line pairs of two texts.  The store computes the disk's changes
;; at a reload with it; anything else may use it too.

(import (only (foundation edoc) elibrary))
(elibrary (foundation diff)
  (export (rename (diff-matches matches)))
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
    ;; with patience sorting.  tails[k] is the index of the smallest
    ;; b-coordinate ending a chain of length k+1.
    (let* ([v (list->vector pairs)]
           [n (vector-length v)]
           [tails (make-vector n 0)]
           [prev (make-vector n -1)])
      (let loop ([i 0] [size 0])
        (if (= i n)
            (if (= size 0)
                '()
                (let build ([i (vector-ref tails (- size 1))] [acc '()])
                  (if (< i 0)
                      acc
                      (build (vector-ref prev i)
                             (cons (vector-ref v i) acc)))))
            (let* ([x (cdr (vector-ref v i))]
                   [place
                    (let search ([lo 0] [hi size])
                      (if (= lo hi)
                          lo
                          (let ([mid (div (+ lo hi) 2)])
                            (if (< (cdr (vector-ref v (vector-ref tails mid))) x)
                                (search (+ mid 1) hi)
                                (search lo mid)))))])
              (when (> place 0)
                (vector-set! prev i (vector-ref tails (- place 1))))
              (vector-set! tails place i)
              (loop (+ i 1) (if (= place size) (+ size 1) size)))))))

  (edoc "The matching line pairs ((ai . bi) ...) between two line vectors, increasing on both sides: a patience diff."
        (a vector "one text")
        (b vector "the other")
        (returns list))
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
             [tail (let loop ([n (- suf 1)] [acc '()])
                     (if (< n 0)
                         acc
                         (loop (- n 1)
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

)
