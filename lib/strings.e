;; strings.e -- the small pure string utilities shared by the modules
;; that sit below core: the library (strings), v2 core dissolution
;; (docs/DESIGN2.md).  Exported names drop the module stem, per the
;; v2 import convention: (strings:tail s 2), (strings:prefix? "C-" s),
;; (strings:join parts " ").
;;
;; core.e still carries its own copies under the string- names; they
;; dissolve with it.

(library (strings)
  (export tail prefix? suffix? join)
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
                   (car xs) (cdr xs)))))
