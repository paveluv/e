;; pretty-scheme.e -- per-construct unicode parens, for the e editor.
;;
;; An e extension module: the library (pretty-scheme), loaded at
;; startup by the core, which calls init!.  (pretty-scheme-clusters!) toggles
;; the current buffer
;; between scheme and pretty-scheme.  The mode draws each construct's
;; parentheses as the unicode pair assigned to its cluster -- (define
;; ...) wears double angles, conditionals white parens, and so on --
;; while the buffer and the file keep plain "(" and ")" (or "[" and
;; "]": the rules ignore the difference, and the status line hints the
;; source character under point).  Typing ")" or "]" closes the
;; innermost open construct with whatever character the source opened
;; it with, as the Scheme REPL does.  Presentation only: every glyph
;; is one character and one terminal cell, so editing, search, and the
;; file on disk are untouched.

(library (pretty-scheme)
  (export init! pretty-scheme-clusters! pretty-scheme-depth! pretty-scheme-rainbow!)
  (import (chezscheme) (core)
          (only (describe) register-descriptions!))

  ;;; Clusters ------------------------------------------------------------------


  (define clusters
    ;; construct name -> (name open-glyph close-glyph); coarse for now,
    ;; to be fine-tuned.  Single-width glyphs only.  Spare pairs, for
    ;; the copy-pasting -- check that your font draws them one cell
    ;; wide before adopting:
    ;;
    ;;   ⟪ ⟫   double angles          ⦇ ⦈   Z-notation images
    ;;   ⌈ ⌉   ceiling corners        ⌊ ⌋   floor corners
    ;;   ⦓ ⦔   arc-angle brackets     ⦕ ⦖   double arc brackets
    ;;   ⟬ ⟭   tortoise shells        ⟮ ⟯   flattened parens
    ;;   ⧚ ⧛   double wiggly fences   ⁅ ⁆   squares with quill
    ;;   ⦋ ⦌   squares, underbar      ⦍ ⦎   squares, ticked
    ;;   ⦏ ⦐   squares, reverse tick  ⦉ ⦊   Z-notation binding
    ;;   ⸢ ⸣   top half brackets      ⸤ ⸥   bottom half brackets
    ;;   ❨ ❩   ornament parens        ❪ ❫   flattened ornaments
    ;;   ❴ ❵   ornament braces        ❬ ❭   ornament angles
    ;;   ﴾ ﴿   ornate parens          ‹ ›   single guillemets
    ;;   ༼ ༽   Tibetan ang khang      ｢ ｣   halfwidth corners
    (let ()
      (define (cluster open close . names)
        (map (lambda (n) (list n open close)) names))
      (append
        (cluster #\｢ #\｣ ; #\⦃ #\⦄                  ; definitions
          "define" "define-values" "define-syntax" "define-record-type"
          "define-condition-type")
        (cluster #\⸦ #\⸧                  ; lambdas
          "lambda" "case-lambda")
        (cluster #\⟨ #\⟩                  ; binders
          "let" "let*" "letrec" "letrec*" "let-values" "let*-values"
          "let-syntax" "letrec-syntax" "parameterize" "fluid-let")
        (cluster #\⦅ #\⦆                  ; conditionals
          "if" "cond" "case" "when" "unless" "and" "or" "else")
        (cluster #\⟦ #\⟧                  ; control
          "begin" "guard" "dynamic-wind")
        (cluster #\⟅ #\⟆                  ; iteration
          "do" "map" "for-each" "fold-left" "fold-right" "filter"
          "exists" "for-all")
        (cluster #\⧼ #\⧽                  ; syntax
          "syntax-rules" "syntax-case" "with-syntax" "quasisyntax"
          "syntax" "identifier-syntax")
        (cluster #\⸨ #\⸩                  ; modules
          "library" "import" "export" "include")
        (cluster #\‹ #\›                  ; quoting
          "quote" "quasiquote" "unquote" "unquote-splicing")
        (cluster #\⧘ #\⧙                  ; mutation
          "set!")
        (cluster #\⌊ #\⌋                  ; laziness
          "delay"))))

  ;;; Scanning ------------------------------------------------------------------

  (define (walk v visit)
    ;; Call (visit row col char) for every character outside strings,
    ;; comments, and character literals.  Crude but serviceable: #| |#
    ;; nests, a string continues across lines, #\X consumes one
    ;; character (a named literal's tail reads as harmless letters).
    (let ([rows (vector-length v)])
      (let loop ([r 0] [c 0] [state 'code] [depth 0])
        (when (< r rows)
          (let* ([s (vector-ref v r)] [n (string-length s)])
            (if (>= c n)
                (loop (+ r 1) 0
                      (if (eq? state 'line-comment) 'code state) depth)
                (let ([ch (string-ref s c)])
                  (case state
                    [(code)
                     (cond
                       [(char=? ch #\;) (loop r n 'line-comment depth)]
                       [(char=? ch #\") (loop r (+ c 1) 'string depth)]
                       [(and (char=? ch #\#) (< (+ c 1) n)
                             (char=? (string-ref s (+ c 1)) #\|))
                        (loop r (+ c 2) 'block-comment 1)]
                       [(and (char=? ch #\#) (< (+ c 1) n)
                             (char=? (string-ref s (+ c 1)) #\\))
                        (loop r (min n (+ c 3)) 'code depth)]
                       [else (visit r c ch)
                             (loop r (+ c 1) 'code depth)])]
                    [(string)
                     (cond
                       [(char=? ch #\\) (loop r (+ c 2) 'string depth)]
                       [(char=? ch #\") (loop r (+ c 1) 'code depth)]
                       [else (loop r (+ c 1) 'string depth)])]
                    [(block-comment)
                     (cond
                       [(and (char=? ch #\|) (< (+ c 1) n)
                             (char=? (string-ref s (+ c 1)) #\#))
                        (if (= depth 1)
                            (loop r (+ c 2) 'code 0)
                            (loop r (+ c 2) 'block-comment (- depth 1)))]
                       [(and (char=? ch #\#) (< (+ c 1) n)
                             (char=? (string-ref s (+ c 1)) #\|))
                        (loop r (+ c 2) 'block-comment (+ depth 1))]
                       [else (loop r (+ c 1) 'block-comment depth)])]
                    [else (loop r (+ c 1) state depth)]))))))))

  (define (operator-after v r c)
    ;; The operator token following the opener at (r . c), across
    ;; lines; #f when the next thing is not a plain symbol.
    (let loop ([r r] [c (+ c 1)])
      (cond
        [(>= r (vector-length v)) #f]
        [(>= c (string-length (vector-ref v r))) (loop (+ r 1) 0)]
        [else
         (let* ([s (vector-ref v r)]
                [ch (string-ref s c)])
           (cond
             [(memv ch '(#\space #\tab)) (loop r (+ c 1))]
             [(memv ch '(#\( #\[ #\) #\] #\" #\; #\' #\` #\,)) #f]
             [else
              (let end ([j c])
                (if (or (>= j (string-length s))
                        (memv (string-ref s j)
                              '(#\space #\tab #\( #\) #\[ #\] #\" #\;)))
                    (substring s c j)
                    (end (+ j 1))))]))])))

  (define (analyze v)
    ;; The display lines: fresh copies with each construct's parens
    ;; drawn as its cluster's glyphs, closers matched to their openers.
    (let ([out (let ([o (make-vector (vector-length v))])
                 (do ([i 0 (+ i 1)]) ((= i (vector-length v)) o)
                   (vector-set! o i (string-copy (vector-ref v i)))))]
          [stack '()])
      (walk v
        (lambda (r c ch)
          (cond
            [(memv ch '(#\( #\[))
             (let* ([op (operator-after v r c)]
                    [hit (and op (assoc op clusters))])
               (when hit
                 (string-set! (vector-ref out r) c (cadr hit)))
               (set! stack (cons (and hit (caddr hit)) stack)))]
            [(memv ch '(#\) #\]))
             (when (pair? stack)
               (when (car stack)
                 (string-set! (vector-ref out r) c (car stack)))
               (set! stack (cdr stack)))])))
      out))

  ;;; Depth variants --------------------------------------------------------------

  (define depth-pairs
    ;; The rotation for pretty-scheme-depth: one pair per nesting level,
    ;; cycling when exhausted.
    '((#\｢ . #\｣) (#\⸦ . #\⸧) (#\⟨ . #\⟩) (#\⦅ . #\⦆)
      (#\⟦ . #\⟧) (#\⟅ . #\⟆) (#\⧼ . #\⧽) (#\⸨ . #\⸩)
      (#\⟪ . #\⟫) (#\⦇ . #\⦈) (#\⌈ . #\⌉) (#\⌊ . #\⌋)))

  (define rainbow
    ;; The rotation for pretty-scheme-rainbow: seven colors, rainbow order.
    '#(rainbow1 rainbow2 rainbow3 rainbow4 rainbow5 rainbow6 rainbow7))

  (define (analyze-depth v)
    ;; Display lines with every paren drawn as its nesting level's
    ;; pair: the top level wears the first, each level deeper the
    ;; next, cycling.
    (let ([out (let ([o (make-vector (vector-length v))])
                 (do ([i 0 (+ i 1)]) ((= i (vector-length v)) o)
                   (vector-set! o i (string-copy (vector-ref v i)))))]
          [stack '()])
      (walk v
        (lambda (r c ch)
          (cond
            [(memv ch '(#\( #\[))
             (let ([pair (list-ref depth-pairs
                                   (mod (length stack)
                                        (length depth-pairs)))])
               (string-set! (vector-ref out r) c (car pair))
               (set! stack (cons (cdr pair) stack)))]
            [(memv ch '(#\) #\]))
             (when (pair? stack)
               (string-set! (vector-ref out r) c (car stack))
               (set! stack (cdr stack)))])))
      out))

  (define (analyze-rainbow v)
    ;; Per-row paren coloring by nesting level: a vector of
    ;; ((col . style) ...) alists, the seven colors cycling.
    (let ([out (make-vector (vector-length v) '())]
          [depth 0])
      (walk v
        (lambda (r c ch)
          (cond
            [(memv ch '(#\( #\[))
             (vector-set! out r
               (cons (cons c (vector-ref rainbow (mod depth 7)))
                     (vector-ref out r)))
             (set! depth (+ depth 1))]
            [(memv ch '(#\) #\]))
             (set! depth (max 0 (- depth 1)))
             (vector-set! out r
               (cons (cons c (vector-ref rainbow (mod depth 7)))
                     (vector-ref out r)))])))
      out))

  ;;; The mode ------------------------------------------------------------------

  (define (buffer-vector b)
    ;; The buffer's lines as a vector of (shared) strings, through the
    ;; public accessors.
    (let* ([n (buffer-line-count b)]
           [v (make-vector n)])
      (do ([i 0 (+ i 1)]) ((= i n) v)
        (vector-set! v i (buffer-line b i)))))

  (define cluster-row (memoize-buffer-analysis analyze))
  (define depth-row (memoize-buffer-analysis analyze-depth))
  (define rainbow-row (memoize-buffer-analysis analyze-rainbow))

  (define (rendered b row line)
    (or (cluster-row b row) line))

  (define (depth-rendered b row line)
    (or (depth-row b row) line))

  (define (rainbow-styles b row line)
    ;; The scheme styles with the paren cells recolored by depth --
    ;; copied first: the base vector belongs to the style cache.
    (let ([styles (let ([s (scheme-styles line)])
                    (if s
                        (let ([copy (make-vector (vector-length s))])
                          (do ([i 0 (+ i 1)])
                              ((= i (vector-length s)) copy)
                            (vector-set! copy i (vector-ref s i))))
                        (make-vector (string-length line) 'plain)))])
      (for-each (lambda (o)
                  (when (< (car o) (vector-length styles))
                    (vector-set! styles (car o) (cdr o))))
                (or (rainbow-row b row) '()))
      styles))

  (define (scheme-styles s)
    (let ([m (find-mode "scheme")])
      (and m ((mode-styles m) s))))

  ;;; Editing -------------------------------------------------------------------

  (define (pretty-buffer?)
    ;; The modes whose display hides the source characters -- they get
    ;; the REPL-style closing and the source hint.
    (member (buffer-mode-name (current-buffer))
            '("pretty-scheme-clusters" "pretty-scheme-depth")))

  (define (innermost-opener)
    ;; The source character of the innermost construct still open at
    ;; point: #\( or #\[, or #f outside any.
    (let ([target (point)] [stack '()])
      (walk (buffer-vector (current-buffer))
        (lambda (r c ch)
          (when (or (< r (car target))
                    (and (= r (car target)) (< c (cdr target))))
            (cond
              [(memv ch '(#\( #\[)) (set! stack (cons ch stack))]
              [(memv ch '(#\) #\]))
               (when (pair? stack) (set! stack (cdr stack)))]))))
      (and (pair? stack) (car stack))))

  (define (close! typed)
    ;; ")" and "]" both close the innermost open construct with the
    ;; character the source opened it with, as the Scheme REPL does;
    ;; outside the mode they insert themselves.
    (if (pretty-buffer?)
        (insert-text! (string (if (eqv? (innermost-opener) #\[) #\] #\))))
        (insert-text! (string typed))))

  (define (toggle-mode! name)
    (set-buffer-mode! (current-buffer)
                      (if (equal? (buffer-mode-name (current-buffer)) name)
                          "scheme"
                          name))
    (void))

  (define (pretty-scheme-clusters!)
    ;; Toggle the current buffer between scheme and pretty-scheme-clusters:
    ;; construct-cluster parens.
    (toggle-mode! "pretty-scheme-clusters"))

  (define (pretty-scheme-depth!)
    ;; Toggle pretty-scheme-depth: parens by nesting level, the pair rotation
    ;; cycling as the tree deepens.
    (toggle-mode! "pretty-scheme-depth"))

  (define (pretty-scheme-rainbow!)
    ;; Toggle pretty-scheme-rainbow: plain characters, colored by nesting
    ;; level through the rainbow.
    (toggle-mode! "pretty-scheme-rainbow"))

  (define (init!)
    (register-descriptions!
      '(((pretty-scheme-clusters!)
         (("procedure" . "(pretty-scheme-clusters!)")) "void"
         ("(pretty-scheme)") pretty-scheme "Display commands" #f
         "Toggle the current Scheme buffer between its normal mode and a view that renders parentheses with glyph pairs chosen by syntactic construct.")
        ((pretty-scheme-depth!)
         (("procedure" . "(pretty-scheme-depth!)")) "void"
         ("(pretty-scheme)") pretty-scheme "Display commands" #f
         "Toggle the current Scheme buffer between its normal mode and a view whose parenthesis glyph pairs rotate with nesting depth.")
        ((pretty-scheme-rainbow!)
         (("procedure" . "(pretty-scheme-rainbow!)")) "void"
         ("(pretty-scheme)") pretty-scheme "Display commands" #f
         "Toggle the current Scheme buffer between its normal mode and a view that colors parentheses by nesting depth.")))
    (register-mode! "pretty-scheme-clusters" '() '() scheme-styles rendered)
    (register-mode! "pretty-scheme-depth" '() '() scheme-styles depth-rendered)
    (register-mode! "pretty-scheme-rainbow" '() '() scheme-styles #f
                    rainbow-styles)
    (bind-default-key! ")" (lambda () (close! #\))))
    (bind-default-key! "]" (lambda () (close! #\])))
    (add-status-hint!
      (lambda ()
        (and (pretty-buffer?)
             (let* ([p (point)]
                    [s (buffer-line (current-buffer) (car p))]
                    [c (and (< (cdr p) (string-length s))
                            (string-ref s (cdr p)))])
               (and c (memv c '(#\( #\) #\[ #\]))
                    (format "  src ~c" c))))))))
