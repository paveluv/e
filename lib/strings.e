;; strings.e -- the small pure string utilities shared by the modules
;; that sit below core: the library (strings), v2 core dissolution
;; (docs/DESIGN2.md).  Exported names drop the module stem, per the
;; v2 import convention: (strings:tail s 2), (strings:prefix? "C-" s),
;; (strings:join parts " ").
;;
;; core.e still carries its own copies under the string- names; they
;; dissolve with it.

(library (strings)
  (export tail prefix? suffix? join search)
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
