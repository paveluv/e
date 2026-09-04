;; strings.e -- the small pure string utilities every module shares:
;; the library (strings), v2 core dissolution (docs/DESIGN2.md).  The
;; one home of these helpers -- no module keeps a private copy.
;; Exported names drop the module stem, per the v2 import convention:
;; (strings:tail s 2), (strings:prefix? "C-" s), (strings:join parts
;; " "), (strings:lines text).

(library (strings)
  (export tail prefix? suffix? join search lines common-prefix
          insert delete elide)
  (import (rnrs))

  (define (tail s i) (substring s i (string-length s)))

  (define (prefix? prefix s)
    (let ([np (string-length prefix)])
      (and (>= (string-length s) np)
           (string=? (substring s 0 np) prefix))))

  (define (suffix? suffix s)
    (let ([ns (string-length suffix)] [n (string-length s)])
      (and (>= n ns)
           (string=? (substring s (- n ns) n) suffix))))

  (define (insert s at addition)
    (string-append (substring s 0 at) addition (tail s at)))

  (define (delete s from to)
    (string-append (substring s 0 from) (tail s to)))

  (define (elide s width)
    ;; s shortened to about width with an elided middle, for messages
    (if (<= (string-length s) width)
        s
        (let ([keep (max 4 (div (- width 5) 2))])
          (string-append (substring s 0 keep) " ... "
                         (tail s (- (string-length s) keep))))))

  (define (lines s)
    ;; s split at every newline: "" is one empty line, a trailing
    ;; newline yields an empty last line
    (let loop ([start 0] [i 0] [acc '()])
      (cond [(= i (string-length s))
             (reverse (cons (substring s start i) acc))]
            [(char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
            [else (loop start (+ i 1) acc)])))

  (define (common-prefix strs)
    ;; the longest prefix shared by every string in the non-empty list
    (fold-left (lambda (acc s)
                 (let loop ([i 0])
                   (if (and (< i (string-length acc)) (< i (string-length s))
                            (char=? (string-ref acc i) (string-ref s i)))
                       (loop (+ i 1))
                       (substring acc 0 i))))
               (car strs) (cdr strs)))

  (define (join xs sep)
    (if (null? xs)
        ""
        (fold-left (lambda (acc x) (string-append acc sep x))
                   (car xs) (cdr xs))))

  (define search
    ;; Index of the first occurrence of needle inside s[start, limit),
    ;; or #f.  Exact by default; the optional fold? matches case
    ;; insensitively (incremental search offers that -- lexers, mode
    ;; detection, and replace! must not).
    (case-lambda
      [(s needle start limit) (search s needle start limit #f)]
      [(s needle start limit fold?)
       (let ([eq? (if fold? char-ci=? char=?)]
             [len (string-length needle)])
         (if (= len 0)
             start
             (let ([failure (make-vector len 0)])
               ;; KMP prefix table: the longest proper prefix ending here.
               (let build ([i 1] [matched 0])
                 (when (< i len)
                   (cond
                     [(eq? (string-ref needle i) (string-ref needle matched))
                      (let ([matched (+ matched 1)])
                        (vector-set! failure i matched)
                        (build (+ i 1) matched))]
                     [(> matched 0)
                      (build i (vector-ref failure (- matched 1)))]
                     [else (build (+ i 1) 0)])))
               (let scan ([i start] [matched 0])
                 (cond
                   [(>= i limit) #f]
                   [(eq? (string-ref s i) (string-ref needle matched))
                    (let ([matched (+ matched 1)])
                      (if (= matched len)
                          (+ (- i len) 1)
                          (scan (+ i 1) matched)))]
                   [(> matched 0)
                    (scan i (vector-ref failure (- matched 1)))]
                   [else (scan (+ i 1) 0)])))))])))
