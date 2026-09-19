;; string.sls -- the small pure string utilities every module shares:
;; the library (string).  The one home of these
;; helpers -- no module keeps a private copy.  Exported names drop the
;; module stem, per the import convention:
;; (string:tail s 2), (string:prefix? "C-" s), (string:join parts
;; " "), (string:lines text).

(library (string)
  (export tail prefix? suffix? join search lines common-prefix
          insert delete elide)
  (import (only (edoc) edefine edoc) (rnrs))

  (edefine (tail s i)
    (edoc "A string from an index on."
          (s string "the string")
          (i integer "the first index kept")
          (returns string))
    (substring s i (string-length s)))

  (edefine (prefix? prefix s)
    (edoc "Whether a string starts with a prefix."
          (prefix string "the prefix")
          (s string "the string")
          (returns boolean))
    (let ([np (string-length prefix)])
      (and (>= (string-length s) np)
           (string=? (substring s 0 np) prefix))))

  (edefine (suffix? suffix s)
    (edoc "Whether a string ends with a suffix."
          (suffix string "the suffix")
          (s string "the string")
          (returns boolean))
    (let ([ns (string-length suffix)] [n (string-length s)])
      (and (>= n ns)
           (string=? (substring s (- n ns) n) suffix))))

  (edefine (insert s at addition)
    (edoc "A string with an addition inserted at an index."
          (s string "the string")
          (at integer "where to insert")
          (addition string "the text inserted")
          (returns string))
    (string-append (substring s 0 at) addition (tail s at)))

  (edefine (delete s from to)
    (edoc "A string without its characters [from, to)."
          (s string "the string")
          (from integer "the first index removed")
          (to integer "the index after the last")
          (returns string))
    (string-append (substring s 0 from) (tail s to)))

  (edefine (elide s width)
    (edoc "A string shortened to about a width with its middle elided, for messages."
          (s string "the string")
          (width integer "the wanted width")
          (returns string))
    ;; s shortened to about width with an elided middle, for messages
    (if (<= (string-length s) width)
        s
        (let ([keep (max 4 (div (- width 5) 2))])
          (string-append (substring s 0 keep) " ... "
                         (tail s (- (string-length s) keep))))))

  (edefine (lines s)
    (edoc "A string split at every newline; a trailing newline yields an empty last line."
          (s string "the string")
          (returns (list-of string)))
    ;; s split at every newline: "" is one empty line, a trailing
    ;; newline yields an empty last line
    (let loop ([start 0] [i 0] [acc '()])
      (cond [(= i (string-length s))
             (reverse (cons (substring s start i) acc))]
            [(char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
            [else (loop start (+ i 1) acc)])))

  (edefine (common-prefix strs)
    (edoc "The longest prefix every string of a nonempty list shares."
          (strs (list-of string) "the strings")
          (returns string))
    ;; the longest prefix shared by every string in the non-empty list
    (fold-left (lambda (acc s)
                 (let loop ([i 0])
                   (if (and (< i (string-length acc)) (< i (string-length s))
                            (char=? (string-ref acc i) (string-ref s i)))
                       (loop (+ i 1))
                       (substring acc 0 i))))
               (car strs) (cdr strs)))

  (edefine (join xs sep)
    (edoc "Strings joined with a separator."
          (xs (list-of string) "the strings")
          (sep string "the separator")
          (returns string))
    (if (null? xs)
        ""
        (fold-left (lambda (acc x) (string-append acc sep x))
                   (car xs) (cdr xs))))

  (edefine search
    ;; Index of the first occurrence of needle inside s[start, limit),
    ;; or #f.  Exact by default; the optional fold? matches case
    ;; insensitively (incremental search offers that -- lexers, mode
    ;; detection, and replace! must not).
    (case-lambda
      [(s needle start limit)
       (edoc "The index of the first occurrence of a needle inside s[start, limit), or #f."
             (s string "the string")
             (needle string "what to find")
             (start integer "where to begin")
             (limit integer "where to stop")
             (returns (or integer #f)))
       (search s needle start limit #f)]
      [(s needle start limit fold?)
       (edoc "The index of the first occurrence of a needle inside s[start, limit), matching case-insensitively when fold?, or #f."
             (s string "the string")
             (needle string "what to find")
             (start integer "where to begin")
             (limit integer "where to stop")
             (fold? boolean "whether to ignore case")
             (returns (or integer #f)))
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
