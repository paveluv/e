;; path.sls -- shared textual path operations: the library (path).
;;
;; Normalize names without requiring components to exist or chasing
;; symbolic links. Tilde expansion and canonicalization are separate:
;; compose them as (path:canonical (path:expand name)).

(import (only (foundation edoc) elibrary))
(elibrary (sys path)
  (export canonical expand)
  (import (except (chezscheme) expand) (prefix (foundation string) string:))

  (edoc "A path with a leading ~ expanded to the home directory."
        (path string "the path")
        (returns string))
  (define (expand path)
    ;; Expand a leading ~ to the home directory.
    (let ([home (getenv "HOME")])
      (cond [(not home) path]
            [(string=? path "~") home]
            [(string:prefix? "~/" path) (string-append home (string:tail path 1))]
            [else path])))

  (edoc "A path made absolute with its dot, dot-dot and empty segments resolved textually; links are not chased."
        (path* string "the path")
        (returns string))
  (define (canonical path*)
    ;; path made absolute, with ".", "..", and empty segments resolved
    ;; textually (symbolic links are not chased) -- enough to recognize
    ;; the editor's own files whichever way they are named.
    (let* ([path (if (string:prefix? "/" path*)
                     path*
                     (string-append (current-directory) "/" path*))]
           [n (string-length path)])
      (let loop ([i 0] [start 0] [stack '()])
        (define (push seg)
          (cond [(or (string=? seg "") (string=? seg ".")) stack]
                [(string=? seg "..") (if (pair? stack) (cdr stack) stack)]
                [else (cons seg stack)]))
        (cond [(> i n) (string-append "/" (string:join (reverse stack) "/"))]
              [(or (= i n) (char=? (string-ref path i) #\/))
               (loop (+ i 1) (+ i 1) (push (substring path start i)))]
              [else (loop (+ i 1) start stack)]))))

) ;; library (path)
