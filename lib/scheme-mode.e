;; scheme-mode.e -- Scheme syntax highlighting for the e editor.
;;
;; An e extension module: the library (scheme-mode), loaded at startup by
;; the core, which calls init!.  Registers a mode tied to Scheme file
;; extensions and to #! interpreter lines naming a Scheme, so only Scheme
;; buffers are highlighted.  The style symbols it produces are rendered
;; by the editor's style-code, and brackets styled 'delimiter take part
;; in bracket matching (so brackets inside strings and comments don't
;; count).

(library (scheme-mode)
  (export init!)
  (import (chezscheme) (core) (scheme-format))

  (define scheme-keywords
    (let ([table (make-hashtable string-hash string=?)])
      (for-each (lambda (k) (hashtable-set! table k #t))
                '("and" "begin" "case" "case-lambda" "cond" "define"
                  "define-condition-type" "define-record-type"
                  "define-syntax" "define-values" "delay" "do"
                  "else" "export" "guard" "identifier-syntax" "if" "import"
                  "include" "lambda" "let" "let*"
                  "let-values" "letrec" "letrec*" "library" "or"
                  "parameterize" "quote" "quasiquote" "set!"
                  "syntax" "syntax-case" "syntax-rules" "unless" "unquote"
                  "unquote-splicing" "when" "with-syntax"))
      table))

  (define (maybe-number? token)
    ;; Only tokens that can begin a number are worth handing to the
    ;; reader: a digit, or a sign, dot, or # prefix (rare identifiers
    ;; like ->x cost one failed parse; the mass of alphabetic ones skip).
    (let ([c (string-ref token 0)])
      (or (char<=? #\0 c #\9) (memv c '(#\+ #\- #\. #\#)))))

  (define standard-symbol?
    ;; Membership in the standard language -- Chez's (chezscheme)
    ;; environment, built once at first use.  The negative-space check
    ;; is against the language, not the editor's session: what counts
    ;; as known does not drift as things get defined.
    (let ([table #f])
      (lambda (sym)
        (unless table
          (set! table (make-eq-hashtable))
          (for-each (lambda (s) (eq-hashtable-set! table s #t))
                    (environment-symbols (environment '(chezscheme)))))
        (eq-hashtable-ref table sym #f))))

  (define (scheme-token-style token)
    ;; Character literals never reach here -- the line scanner consumes
    ;; them whole -- so # tokens are booleans or radix-prefixed numbers.
    ;; A symbol the standard language does not know is the negative
    ;; space: a local, a program-defined name, or a typo.  Italic.
    (cond [(hashtable-ref scheme-keywords token #f) 'keyword]
          [(member token '("#t" "#f")) 'literal]
          [(and (maybe-number? token) (string->number token)) 'number]
          [(guard (ex [else #t])
             (standard-symbol? (string->symbol token)))
           'plain]
          [else 'italic]))

  (define (scheme-styles s)
    (let* ([n (string-length s)]
           [styles (make-vector n 'plain)])
      (let loop ([i 0])
        (when (< i n)
          (let ([c (string-ref s i)])
            (cond
              [(and (char=? c #\#) (< (+ i 1) n)
                    (char=? (string-ref s (+ i 1)) #\\))
               ;; A character literal: #\ followed by any character --
               ;; delimiters like ; " ( ) included -- then any trailing
               ;; name characters (#\space, #\x41).
               (let lit-loop ([j (min n (+ i 3))])
                 (if (and (< j n) (not (scheme-delimiter? (string-ref s j))))
                     (lit-loop (+ j 1))
                     (begin (vector-fill-range! styles i j 'literal)
                            (loop j))))]
              [(char=? c #\;)
               (vector-fill-range! styles i n 'comment)]
              [(char=? c #\")
               (let string-loop ([j (+ i 1)] [escaped? #f])
                 (cond [(= j n) (vector-fill-range! styles i n 'string)]
                       [(and (char=? (string-ref s j) #\") (not escaped?))
                        (vector-fill-range! styles i (+ j 1) 'string)
                        (loop (+ j 1))]
                       [else
                        (string-loop (+ j 1)
                                     (and (char=? (string-ref s j) #\\) (not escaped?)))]))]
              [(memv c '(#\( #\) #\[ #\] #\{ #\}))
               (vector-set! styles i 'delimiter) (loop (+ i 1))]
              [(memv c '(#\' #\` #\,))
               (vector-set! styles i 'quote) (loop (+ i 1))]
              [(char-whitespace? c) (loop (+ i 1))]
              [else
               (let token-loop ([j (+ i 1)])
                 (if (and (< j n) (not (scheme-delimiter? (string-ref s j))))
                     (token-loop (+ j 1))
                     (begin
                       (vector-fill-range! styles i j
                         (scheme-token-style (substring s i j)))
                       (loop j))))]))))
      styles))

  ;;; Indentation and formatting ------------------------------------------------

  ;; The engine is the pure (scheme-format) library; these adapters
  ;; feed it buffer lines.

  (define (buffer-vector b)
    (let* ([n (buffer-line-count b)]
           [v (make-vector n)])
      (do ([i 0 (+ i 1)]) ((= i n) v)
        (vector-set! v i (buffer-line b i)))))

  (define (scheme-indent b from to)
    (scheme-indent-lines (buffer-vector b) from to))

  (define (scheme-format b from to)
    (scheme-format-lines (buffer-vector b) from to))

  (define (init!)
    (register-mode! "scheme"
                    '(".scm" ".ss" ".sls" ".sps" ".sc" ".e")
                    '("scheme" "petite" "chez" "guile" "racket")
                    scheme-styles)
    (register-indenter! "scheme" scheme-indent)
    (register-formatter! "scheme" scheme-format)))
