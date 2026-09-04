;; c-mode.e -- C syntax highlighting for the e editor.
;;
;; An e extension module: the library (c-mode), loaded at startup by
;; the core, which calls init!.  Registers a mode tied to C file
;; extensions.  Lines are lexed individually, but whether a line
;; starts inside a /* */ block comment comes from a whole-buffer scan,
;; memoized per buffer and redone only when some line changed, so
;; block comments span lines correctly.  Braces and brackets styled
;; delimiter take part in bracket matching.

(library (c-mode)
  (export init!)
  (import (chezscheme) (except (edit) init!)
          (prefix (style) style:)
          (prefix (mode) mode:)
          (prefix (string) string:))

  (define c-keywords
    (let ([table (make-hashtable string-hash string=?)])
      (for-each (lambda (k) (hashtable-set! table k #t))
                '("auto" "break" "case" "char" "const" "continue"
                  "default" "do" "double" "else" "enum" "extern"
                  "float" "for" "goto" "if" "inline" "int" "long"
                  "register" "restrict" "return" "short" "signed"
                  "sizeof" "static" "struct" "switch" "typedef"
                  "union" "unsigned" "void" "volatile" "while"
                  "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex"
                  "_Generic" "_Imaginary" "_Noreturn" "_Static_assert"
                  "_Thread_local"))
      table))

  (define (ident-char? c)
    (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

  (define (c-line-styles s in-comment)
    ;; One line's styles, given whether it starts inside a block
    ;; comment.  -> (values styles still-inside-at-the-end).
    (define n (string-length s))
    (define styles (make-vector n 'plain))
    (define (quoted-loop i close)
      ;; A string or character constant from i, escapes honored;
      ;; -> the index just past it (unterminated: the end of line).
      (let loop ([j (+ i 1)] [escaped? #f])
        (cond [(= j n) n]
              [(and (char=? (string-ref s j) close) (not escaped?))
               (+ j 1)]
              [else (loop (+ j 1)
                          (and (char=? (string-ref s j) #\\)
                               (not escaped?)))])))
    (define (directive i)
      ;; # opening the line (whitespace aside): the directive word is a
      ;; literal, and an #include's <...> path a string.  -> the index
      ;; where ordinary lexing resumes.
      (let* ([w (let skip ([j (+ i 1)])
                  (if (and (< j n) (char-whitespace? (string-ref s j)))
                      (skip (+ j 1))
                      j))]
             [k (let word ([j w])
                  (if (and (< j n) (char-alphabetic? (string-ref s j)))
                      (word (+ j 1))
                      j))])
        (style:fill-range! styles i k 'literal)
        (if (and (string=? (substring s w k) "include")
                 (let ([lt (string:search s "<" k n)])
                   (and lt (string:search s ">" lt n))))
            (let* ([lt (string:search s "<" k n)]
                   [gt (string:search s ">" lt n)])
              (style:fill-range! styles lt (+ gt 1) 'string)
              (+ gt 1))
            k)))
    (let loop ([i 0] [in-c in-comment])
      (if (= i n)
          (values styles in-c)
          (let ([c (string-ref s i)])
            (cond
              [in-c
               (let ([end (string:search s "*/" i n)])
                 (if end
                     (begin (style:fill-range! styles i (+ end 2) 'comment)
                            (loop (+ end 2) #f))
                     (begin (style:fill-range! styles i n 'comment)
                            (values styles #t))))]
              [(and (char=? c #\/) (< (+ i 1) n)
                    (char=? (string-ref s (+ i 1)) #\/))
               (style:fill-range! styles i n 'comment)
               (values styles #f)]
              [(and (char=? c #\/) (< (+ i 1) n)
                    (char=? (string-ref s (+ i 1)) #\*))
               (style:fill-range! styles i (+ i 2) 'comment)
               (loop (+ i 2) #t)]
              [(char=? c #\")
               (let ([end (quoted-loop i #\")])
                 (style:fill-range! styles i end 'string)
                 (loop end #f))]
              [(char=? c #\')
               (let ([end (quoted-loop i #\')])
                 (style:fill-range! styles i end 'string)
                 (loop end #f))]
              [(and (char=? c #\#)
                    (let blank ([j 0])
                      (or (= j i)
                          (and (char-whitespace? (string-ref s j))
                               (blank (+ j 1))))))
               (loop (directive i) #f)]
              [(memv c '(#\( #\) #\[ #\] #\{ #\}))
               (vector-set! styles i 'delimiter)
               (loop (+ i 1) #f)]
              [(char-numeric? c)
               ;; digits with the tails of hex, floats, and suffixes --
               ;; crude, but 0x1F, 3.14 and 42UL all hold together
               (let num ([j (+ i 1)])
                 (if (and (< j n)
                          (or (ident-char? (string-ref s j))
                              (char=? (string-ref s j) #\.)))
                     (num (+ j 1))
                     (begin (style:fill-range! styles i j 'number)
                            (loop j #f))))]
              [(or (char-alphabetic? c) (char=? c #\_))
               (let word ([j (+ i 1)])
                 (if (and (< j n) (ident-char? (string-ref s j)))
                     (word (+ j 1))
                     (let ([token (substring s i j)])
                       (style:fill-range! styles i j
                         (cond [(hashtable-ref c-keywords token #f) 'keyword]
                               [(member token '("NULL" "true" "false"))
                                'literal]
                               [else 'plain]))
                       (loop j #f))))]
              [else (loop (+ i 1) #f)])))))

  (define (c-styles s)
    ;; The per-line fallback: lexed as if outside any block comment.
    (let-values ([(styles still) (c-line-styles s #f)])
      styles))

  (define (analyze v)
    ;; Every line's styles, the block-comment state threaded through.
    (let ([out (make-vector (vector-length v))])
      (let loop ([i 0] [in-c #f])
        (if (= i (vector-length v))
            out
            (let-values ([(styles still)
                          (c-line-styles (vector-ref v i) in-c)])
              (vector-set! out i styles)
              (loop (+ i 1) still))))))

  (define c-row (mode:memoize-analysis analyze))

  (define (c-row-styles b row line)
    (c-row b row))

  (define (init!)
    (mode:register! "c" '(".c" ".h") '("tcc")
                    c-styles #f c-row-styles)))
