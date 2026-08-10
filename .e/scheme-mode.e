;; scheme-mode.e -- Scheme syntax highlighting for the e editor.
;;
;; An e extension module: dropped into ~/.e, it is loaded at startup into
;; the editor's top level.  It installs itself by replacing the
;; line-styles hook; the style symbols it produces are rendered by the
;; editor's style-code, and brackets styled 'delimiter take part in
;; bracket matching (so brackets inside strings and comments don't count).

(define scheme-keywords
  '("and" "begin" "case" "cond" "define" "define-syntax" "delay" "do"
    "else" "guard" "if" "import" "lambda" "let" "let*" "let-values"
    "letrec" "letrec*" "or" "parameterize" "quote" "quasiquote" "set!"
    "syntax" "syntax-case" "syntax-rules" "unless" "unquote"
    "unquote-splicing" "when"))

(define (scheme-delimiter? c)
  (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\'))))

(define (scheme-token-style token)
  (cond [(member token scheme-keywords) 'keyword]
        [(or (member token '("#t" "#f"))
             (and (>= (string-length token) 2)
                  (string=? (substring token 0 2) "#\\")))
         'literal]
        [(string->number token) 'number]
        [else 'plain]))

(define (scheme-styles s)
  (let* ([n (string-length s)]
         [styles (make-vector n 'plain)])
    (let loop ([i 0])
      (when (< i n)
        (let ([c (string-ref s i)])
          (cond
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

(set! line-styles scheme-styles)
