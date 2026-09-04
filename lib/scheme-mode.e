;; scheme-mode.e -- Scheme syntax highlighting for the e editor.
;;
;; An e extension module: the library (scheme-mode), loaded at startup by
;; the core, which calls init!.  Registers a mode tied to Scheme file
;; extensions and to #! interpreter lines naming a Scheme, so only Scheme
;; buffers are highlighted.  Lines are lexed individually, but whether a
;; line starts inside a string constant or a #| |# block comment comes
;; from a whole-buffer scan, memoized per buffer and redone only when
;; some line changed (the c-mode pattern), so both span lines correctly.
;; The style symbols it produces are rendered by the editor's
;; style-code, and brackets styled 'delimiter take part in bracket
;; matching (so brackets inside strings and comments don't count).

(library (scheme-mode)
  (export init! scheme-format-on-save)
  (import (chezscheme) (core)
          (prefix (files) files:)
          (prefix (styles) styles:)
          (prefix (modes) modes:) (scheme-format)
          (only (describe) register-descriptions!))

  ;; Configuration: format Scheme buffers just before they are written
  ;; (a pre-save hook), so every save leaves the normal form on disk.
  ;; On by default; (scheme-format-on-save #f) in config.e turns it off.
  (define scheme-format-on-save (make-parameter #t))

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

  (define (scheme-line-styles s state)
    ;; One line's styles, given the state it starts in: #f (plain
    ;; code), 'string (inside a string constant), or the nesting
    ;; depth of an open #| |# block comment.  -> (values styles the
    ;; state at the end of the line).
    (define n (string-length s))
    (define styles (make-vector n 'plain))
    (define (string-body i)
      ;; a string's continuation from i (past its opening quote):
      ;; escapes honored -- a backslash may escape the newline itself,
      ;; carrying the string on; unterminated it spans the line
      (let body ([j i] [escaped? #f])
        (cond [(= j n) (styles:fill-range! styles i n 'string) 'string]
              [(and (char=? (string-ref s j) #\") (not escaped?))
               (styles:fill-range! styles i (+ j 1) 'string)
               (scan (+ j 1))]
              [else (body (+ j 1) (and (char=? (string-ref s j) #\\)
                                       (not escaped?)))])))
    (define (comment-body i depth)
      ;; inside #| |# from i, nesting honored
      (let body ([j i] [depth depth])
        (cond [(>= j n) (styles:fill-range! styles i n 'comment) depth]
              [(and (char=? (string-ref s j) #\|) (< (+ j 1) n)
                    (char=? (string-ref s (+ j 1)) #\#))
               (if (= depth 1)
                   (begin (styles:fill-range! styles i (+ j 2) 'comment)
                          (scan (+ j 2)))
                   (body (+ j 2) (- depth 1)))]
              [(and (char=? (string-ref s j) #\#) (< (+ j 1) n)
                    (char=? (string-ref s (+ j 1)) #\|))
               (body (+ j 2) (+ depth 1))]
              [else (body (+ j 1) depth)])))
    (define (scan i)
      (if (>= i n)
          #f
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
                     (begin (styles:fill-range! styles i j 'literal)
                            (scan j))))]
              [(and (char=? c #\#) (< (+ i 1) n)
                    (char=? (string-ref s (+ i 1)) #\|))
               (styles:fill-range! styles i (+ i 2) 'comment)
               (comment-body (+ i 2) 1)]
              [(char=? c #\;)
               (styles:fill-range! styles i n 'comment)
               #f]
              [(char=? c #\")
               (vector-set! styles i 'string)
               (string-body (+ i 1))]
              [(memv c '(#\( #\) #\[ #\] #\{ #\}))
               (vector-set! styles i 'delimiter) (scan (+ i 1))]
              [(memv c '(#\' #\` #\,))
               (vector-set! styles i 'quote) (scan (+ i 1))]
              [(char-whitespace? c) (scan (+ i 1))]
              [else
               (let token-loop ([j (+ i 1)])
                 (if (and (< j n) (not (scheme-delimiter? (string-ref s j))))
                     (token-loop (+ j 1))
                     (begin
                       (styles:fill-range! styles i j
                         (scheme-token-style (substring s i j)))
                       (scan j))))]))))
    (values styles
            (cond [(eq? state 'string) (string-body 0)]
                  [(number? state) (comment-body 0 state)]
                  [else (scan 0)])))

  (define (scheme-styles s)
    ;; The per-line fallback (md-mode borrows it for code inside
    ;; fences): lexed as if outside any string or block comment.
    (let-values ([(styles state) (scheme-line-styles s #f)])
      styles))

  (define (analyze v)
    ;; Every line's styles, the string/block-comment state threaded
    ;; through.
    (let ([out (make-vector (vector-length v))])
      (let loop ([i 0] [state #f])
        (if (= i (vector-length v))
            out
            (let-values ([(styles next)
                          (scheme-line-styles (vector-ref v i) state)])
              (vector-set! out i styles)
              (loop (+ i 1) next))))))

  (define scheme-row (modes:memoize-analysis analyze))

  (define (scheme-row-styles b row line)
    (scheme-row b row))

  ;;; Indentation and formatting ------------------------------------------------

  ;; The engine is the pure (scheme-format) library, shared with the
  ;; standalone scheme-format tool; these adapters feed it buffer lines.

  (define (buffer-vector b)
    (let* ([n (buffer-line-count b)]
           [v (make-vector n)])
      (do ([i 0 (+ i 1)]) ((= i n) v)
        (vector-set! v i (buffer-line b i)))))

  (define (scheme-indent b from to)
    (scheme-indent-lines (buffer-vector b) from to))

  (define (scheme-format b from to)
    (scheme-format-lines (buffer-vector b) from to))

  (define (format-on-save! path)
    (when (and (scheme-format-on-save)
               (equal? (modes:name-of (current-buffer)) "scheme"))
      (format-buffer!)))

  (define (init!)
    (modes:register! "scheme"
                     '(".scm" ".ss" ".sls" ".sps" ".sc" ".e")
                     '("scheme" "petite" "chez" "guile" "racket")
                     scheme-styles #f scheme-row-styles)
    (register-indenter! "scheme" scheme-indent)
    (register-formatter! "scheme" scheme-format)
    (register-descriptions!
      '(((scheme-format-intrusive)
         (("parameter" . "(scheme-format-intrusive [enabled?])")) "boolean"
         ("(scheme-format)") scheme-format "Scheme formatting" #f
         "Control intrusive Scheme formatting. It is off by default; when enabled, whole-buffer formatting collapses excess code spacing, joins fitting continuation lines, normalizes inline-comment gaps, and breaks code toward `scheme-format-width`.")
        ((scheme-format-width)
         (("parameter" . "(scheme-format-width [columns])")) "integer"
         ("(scheme-format)") scheme-format "Scheme formatting" #f
         "Get or set the target width used when `scheme-format-intrusive` is enabled. The default is 100 columns and the minimum is 20.")))
    (files:add-pre-save-hook! format-on-save!)))
