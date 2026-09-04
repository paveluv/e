;; scheme-format.e -- Scheme indentation and formatting, for the e editor.
;;
;; A pure library (no init!): the engine behind scheme-mode's
;; indenter and formatter and behind the standalone scheme-format tool.
;; It operates on vectors of lines; nothing here touches the editor.
;;
;; Conservative formatting preserves spacing and line boundaries. The optional
;; intrusive pass may collapse code whitespace and continuation lines, place
;; inline comments consistently, and break code toward a target width. A line
;; can have
;; several valid indentations, its stops: an application whose first
;; argument shares the opener's line may indent two past the opener
;; or align under that argument, whichever reads better.
;; scheme-indent-lines maps rows [from, to] to their stops -- #f
;; (leave the line alone), a column, or an ascending list of columns
;; -- computed as if each line settles on the stop nearest its
;; current indentation, top to bottom.  scheme-format-lines rewrites
;; the rows: indentation to the nearest stop, tabs outside strings
;; widened to spaces (scheme-tab-width each, #f leaves them), the
;; ( ) vs [ ] conventions (scheme-format-brackets #f leaves them),
;; trailing whitespace trimmed, and -- when the range reaches the last
;; row -- trailing blank lines dropped, so a file ends with exactly
;; one newline.

(library (scheme-format)
  (export scheme-indent-lines scheme-format-lines scheme-delimiter?
          scheme-format-brackets scheme-tab-width
          scheme-format-intrusive scheme-format-width)
  (import (chezscheme) (prefix (string) string:))

  ;; Configuration: bracket convention, tab expansion, and the opt-in
  ;; width-aware layout pass.
  (define scheme-format-brackets (make-parameter #t))
  (define scheme-tab-width (make-parameter 2))
  (define scheme-format-intrusive (make-parameter #f))
  (define scheme-format-width
    (make-parameter 100
      (lambda (width)
        (unless (and (integer? width) (exact? width) (>= width 20))
          (error 'scheme-format-width "expected an integer >= 20" width))
        width)))

  (define (string-find s needle)
    (let ([n (string-length s)] [m (string-length needle)])
      (let loop ([i 0])
        (cond [(> (+ i m) n) #f]
              [(string=? (substring s i (+ i m)) needle) i]
              [else (loop (+ i 1))]))))

  (define (scheme-delimiter? c)
    (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\'))))

  ;; One scanner drives both: it walks lines left to right, skipping
  ;; strings, comments, and character literals, keeping a stack of
  ;; open brackets -- each a frame that remembers where its
  ;; continuation lines belong and what character convention writes it
  ;; with.  The indenter asks the stack where a line should start; the
  ;; formatter additionally normalizes ( ) against [ ] by convention.

  (define specials
    ;; Operators whose continuation lines indent two columns past the
    ;; opener (body forms), rather than aligning under the first
    ;; argument as applications do.
    (let ([t (make-hashtable string-hash string=?)])
      (for-each (lambda (k) (hashtable-set! t k #t))
                '("lambda" "case-lambda" "when" "unless"
                  "case" "do" "parameterize" "guard" "dynamic-wind"
                  "library" "module" "set!" "delay"
                  "syntax-case" "syntax-rules" "identifier-syntax"))
      t))

  (define special-prefixes
    '("define" "let" "with-" "call-with-" "call-as-"))

  (define (special-operator? tok)
    (or (hashtable-ref specials tok #f)
        (exists (lambda (p) (string:prefix? p tok)) special-prefixes)))

  (define binding-forms
    ;; Forms whose bindings list holds [bracketed] clauses.
    '("let" "let*" "letrec" "letrec*" "let-values" "let*-values"
      "let-syntax" "letrec-syntax" "fluid-let" "parameterize"
      "with-syntax" "do"))

  (define clause-forms
    ;; Forms taking [bracketed] clauses directly: (name . first-clause).
    '(("cond" . 1) ("case" . 2) ("case-lambda" . 1)
      ("syntax-rules" . 2) ("syntax-case" . 3)))

  ;; A frame per open bracket: col is the opener's column; norm the
  ;; character convention writes it with; stops the ascending columns
  ;; its continuation lines may indent to (#f until decided, fixed
  ;; once final); count the elements seen so far; op the operator -- a
  ;; token string, 'list when it is itself bracketed, #f before any;
  ;; sym1 whether element 1 was a bare token (a named let); idx this
  ;; frame's element index within its parent; inside whether it opened
  ;; inside the formatted region; orow the opener's row.
  (define-record-type frame
    (fields (immutable col) (immutable norm)
            (mutable stops) (mutable fixed) (mutable count)
            (mutable op) (mutable sym1)
            (immutable idx) (immutable inside) (immutable orow)))

  (define-record-type sstate
    (fields (mutable stack) (mutable str) (mutable blk)))

  (define (fresh-state) (make-sstate '() #f 0))

  (define (closing ch) (if (char=? ch #\[) #\] #\)))

  (define (conventional-open stack idx)
    ;; The character convention writes for an opener sitting at
    ;; element idx of the innermost frame: clause and binding
    ;; positions take brackets, everything else parens.
    (if (null? stack)
        #\(
        (let* ([f (car stack)]
               [op (frame-op f)]
               [g (and (pair? (cdr stack)) (cadr stack))]
               [gop (and g (frame-op g))]
               ;; a form is a paren whose operator is the keyword; a
               ;; bracket with the same first token is only a binding
               ;; or clause that happens to start with that name
               [form? (lambda (fr) (char=? (frame-norm fr) #\())])
          (cond
            [(and (string? op) (form? f) (assoc op clause-forms))
             => (lambda (hit) (if (>= idx (cdr hit)) #\[ #\())]
            ;; identifier-syntax comes in one- and two-clause shapes;
            ;; the writing is the author's call, kept as it stands
            [(and (string? op) (string=? op "identifier-syntax")) #f]
            ;; a binding: any element of the bindings list of a let-ish
            ;; form (the list's own operator is its first binding)
            [(and (not (string? op)) (string? gop) (form? g)
                  (member gop binding-forms)
                  (= (frame-idx f)
                     (if (and (string=? gop "let") (frame-sym1 g)) 2 1)))
             #\[]
            ;; guard's spec list (ex clause ...): the clauses
            [(and (string? gop) (form? g) (string=? gop "guard")
                  (= (frame-idx f) 1) (>= idx 1))
             #\[]
            [else #\(]))))

  (define (scan-line! st s row inside plan)
    ;; Advance st across line s.  With plan (a box), collect the
    ;; formatter's bracket rewrites -- (row col char), both ends of
    ;; every pair whose opener sits inside the formatted region; a
    ;; pair the region only half-covers is left alone.
    (define n (string-length s))
    (define (top)
      (let ([k (sstate-stack st)]) (and (pair? k) (car k))))
    (define (register! col kind tok)
      ;; An element begins at col in the innermost frame: kind is
      ;; 'open, 'sym (a bare token), or 'other (strings, numbers, #
      ;; syntax).  -> the element's index.
      (let ([f (top)])
        (if (not f)
            0
            (let ([k (frame-count f)])
              (frame-count-set! f (+ k 1))
              (when (= k 0)
                (frame-op-set! f (if (eq? kind 'open) 'list tok)))
              (when (= k 1)
                (frame-sym1-set! f (eq? kind 'sym)))
              (cond
                [(frame-fixed f)
                 ;; do already fixed at two-in: its termination clause
                 ;; may also align under the bindings
                 (when (and (= k 1) (equal? (frame-op f) "do"))
                   (frame-stops-set! f
                     (two-stops (+ (frame-col f) 2) col)))]
                [else
                 (case k
                   [(0) (if (and (eq? kind 'sym) (special-operator? tok))
                            (begin (frame-stops-set!
                                     f (list (+ (frame-col f) 2)))
                                   (frame-fixed-set! f #t))
                            (begin (frame-stops-set! f (list col))
                                   (frame-fixed-set! f
                                     (not (eq? kind 'sym)))))]
                   ;; an application: two past the opener and under the
                   ;; first argument are both fine -- the author picks
                   [(1) (frame-stops-set! f
                          (two-stops (+ (frame-col f) 2) col))
                    (frame-fixed-set! f #t)])])
              k))))
    (define (open! col qcol)
      ;; qcol: the introducing quote character's column, if any -- the
      ;; parent aligns elements at the quote, the frame itself keeps
      ;; the real bracket column.
      (let* ([idx (register! (or qcol col) 'open #f)]
             [norm (or (conventional-open (sstate-stack st) idx)
                       (string-ref s col))])   ; #f: kept as written
        (sstate-stack-set! st
                           (cons (make-frame col norm #f #f 0 #f #f idx inside row)
                                 (sstate-stack st)))))
    (define (close! col)
      (let ([f (top)])
        (when f
          (when (and plan (frame-inside f))
            (set-box! plan
                      (cons* (list (frame-orow f) (frame-col f) (frame-norm f))
                             (list row col (closing (frame-norm f)))
                             (unbox plan))))
          (sstate-stack-set! st (cdr (sstate-stack st))))))
    (let loop ([i 0] [q #f])
      (when (< i n)
        (cond
          [(sstate-str st)
           (let str ([j i] [esc #f])
             (cond [(= j n) (loop n #f)]
                   [esc (str (+ j 1) #f)]
                   [(char=? (string-ref s j) #\\) (str (+ j 1) #t)]
                   [(char=? (string-ref s j) #\")
                    (sstate-str-set! st #f) (loop (+ j 1) #f)]
                   [else (str (+ j 1) #f)]))]
          [(> (sstate-blk st) 0)
           (cond
             [(and (< (+ i 1) n)
                   (char=? (string-ref s i) #\|)
                   (char=? (string-ref s (+ i 1)) #\#))
              (sstate-blk-set! st (- (sstate-blk st) 1)) (loop (+ i 2) q)]
             [(and (< (+ i 1) n)
                   (char=? (string-ref s i) #\#)
                   (char=? (string-ref s (+ i 1)) #\|))
              (sstate-blk-set! st (+ (sstate-blk st) 1)) (loop (+ i 2) q)]
             [else (loop (+ i 1) q)])]
          [else
           (let ([c (string-ref s i)])
             (cond
               [(char=? c #\;) (void)]           ; comment to end of line
               [(char=? c #\")
                (register! (or q i) 'other #f)
                (sstate-str-set! st #t)
                (loop (+ i 1) #f)]
               [(and (char=? c #\#) (< (+ i 1) n)
                     (char=? (string-ref s (+ i 1)) #\\))
                (register! (or q i) 'other #f)   ; a character literal
                (let lit ([j (min n (+ i 3))])
                  (if (and (< j n)
                           (not (scheme-delimiter? (string-ref s j))))
                      (lit (+ j 1))
                      (loop j #f)))]
               [(and (char=? c #\#) (< (+ i 1) n)
                     (char=? (string-ref s (+ i 1)) #\|))
                (sstate-blk-set! st 1) (loop (+ i 2) q)]
               [(and (char=? c #\#) (< (+ i 1) n)
                     (char=? (string-ref s (+ i 1)) #\;))
                (loop (+ i 2) (or q i))]         ; the datum scans normally
               [(memv c '(#\( #\[))
                (open! i q) (loop (+ i 1) #f)]
               [(memv c '(#\) #\]))
                (close! i) (loop (+ i 1) #f)]
               [(memv c '(#\' #\` #\,))
                (loop (+ i (if (and (char=? c #\,) (< (+ i 1) n)
                                    (char=? (string-ref s (+ i 1)) #\@))
                               2 1))
                      (or q i))]                 ; elements align at the quote
               [(char-whitespace? c) (loop (+ i 1) q)]
               [else
                (let tok ([j (+ i 1)])
                  (if (and (< j n)
                           (not (scheme-delimiter? (string-ref s j))))
                      (tok (+ j 1))
                      (let ([symish (not (or (char<=? #\0 c #\9)
                                           (char=? c #\#)))])
                        (register! (or q i) (if symish 'sym 'other)
                                   (substring s i j))
                        (loop j #f))))]))])))
    ;; the line is over: frames it opened freeze.  An opener with no
    ;; elements yet indents its contents one column in (data lists);
    ;; a paren whose operator ends the line indents two in (a lone
    ;; cond's clauses, a lone append's arguments), while a bracket's
    ;; contents stay one in ([else and its kin).
    (for-each (lambda (f)
                (unless (frame-fixed f)
                  (cond
                    [(not (frame-stops f))
                     (frame-stops-set! f (list (+ (frame-col f) 1)))]
                    [(char=? (frame-norm f) #\()
                     (frame-stops-set! f (list (+ (frame-col f) 2)))])
                  (frame-fixed-set! f #t)))
              (sstate-stack st)))

  (define (two-stops a b)
    ;; a and b ascending, collapsed when equal.
    (cond [(= a b) (list a)]
          [(< a b) (list a b)]
          [else (list b a)]))

  (define (nearest-stop stops cur)
    ;; The stop closest to cur; on a tie, the leftmost.
    (fold-left (lambda (best s)
                 (if (< (abs (- s cur)) (abs (- best cur))) s best))
               (car stops) stops))

  (define (indent-of s)
    ;; The column the line's text starts at (its length when blank).
    (let loop ([i 0])
      (if (and (< i (string-length s))
               (memv (string-ref s i) '(#\space #\tab)))
          (loop (+ i 1))
          i)))

  (define (line-stops st s)
    ;; Where line s may indent to, given the state at its start: #f
    ;; to leave it alone (a string's or block comment's interior, a
    ;; margin comment -- their layout belongs to the author), or an
    ;; ascending list of stops.
    (cond
      [(sstate-str st) #f]
      [(> (sstate-blk st) 0) #f]
      [else
       (let* ([n (string-length s)]
              [i (indent-of s)]
              [f (let ([k (sstate-stack st)]) (and (pair? k) (car k)))])
         (cond
           ;; a margin comment -- one semicolon, nothing else on the
           ;; line -- keeps its hand-chosen column: it usually
           ;; continues the trailing comment of the line above
           [(and (< i n) (char=? (string-ref s i) #\;)
                 (not (and (< (+ i 1) n)
                           (char=? (string-ref s (+ i 1)) #\;))))
            #f]
           [(not f) (list 0)]
           [(and (< i n) (memv (string-ref s i) '(#\) #\])))
            (list (frame-col f))]                ; a closer under its opener
           [else (frame-stops f)]))]))

  (define (reindent s col)
    ;; s with its leading whitespace replaced by col spaces.
    (string-append (make-string col #\space)
                   (string:tail s (indent-of s))))

  (define (trim-right s)
    (let loop ([n (string-length s)])
      (if (and (> n 0) (memv (string-ref s (- n 1)) '(#\space #\tab)))
          (loop (- n 1))
          (substring s 0 n))))

  (define (detab s width)
    ;; s with each tab outside its string literals widened to width
    ;; spaces.  Only called for lines that start outside a string.
    (if (not (string-search-char s #\tab))
        s
        (let ([op (open-output-string)]
              [n (string-length s)])
          (let loop ([i 0] [str? #f] [lit #f])
            (if (= i n)
                (get-output-string op)
                (let ([c (string-ref s i)])
                  (cond
                    [lit (put-char op c) (loop (+ i 1) str? #f)]
                    [str?
                     (put-char op c)
                     (loop (+ i 1)
                           (cond [(eq? str? 'escaped) #t]
                                 [(char=? c #\\) 'escaped]
                                 [(char=? c #\") #f]
                                 [else #t])
                           #f)]
                    [(char=? c #\tab)
                     (put-string op (make-string width #\space))
                     (loop (+ i 1) #f #f)]
                    [(char=? c #\") (put-char op c) (loop (+ i 1) #t #f)]
                    [(and (char=? c #\#) (< (+ i 1) n)
                          (char=? (string-ref s (+ i 1)) #\\))
                     ;; #\X: the literal character passes verbatim
                     (put-char op c) (put-char op #\\)
                     (loop (+ i 2) #f #t)]
                    [else (put-char op c) (loop (+ i 1) #f #f)])))))))

  (define (string-search-char s ch)
    (let loop ([i 0])
      (cond [(= i (string-length s)) #f]
            [(char=? (string-ref s i) ch) i]
            [else (loop (+ i 1))])))

  (define (settle stops s)
    ;; The stop nearest the line's current indentation: on a stop it
    ;; stays, before the first it takes the first, past the last the
    ;; last, between two the closer.
    (nearest-stop stops (indent-of s)))

  (define (blank? s) (= (indent-of s) (string-length s)))

  (define (scheme-indent-lines v from to)
    ;; The indenter: stops for rows [from, to] -- #f, a column, or an
    ;; ascending list -- each line scanned as it will lie once settled
    ;; on the stop nearest its current indentation.
    (let ([st (fresh-state)])
      (do ([r 0 (+ r 1)]) ((= r from))
        (scan-line! st (vector-ref v r) r #f #f))
      (let loop ([r from] [acc '()])
        (if (> r to)
            (reverse acc)
            (let* ([s (vector-ref v r)]
                   [stops (line-stops st s)]
                   [laid (if (and stops (not (blank? s)))
                             (reindent s (settle stops s))
                             s)])
              (scan-line! st laid r #f #f)
              (loop (+ r 1)
                    (cons (cond [(not stops) #f]
                                [(null? (cdr stops)) (car stops)]
                                [else stops])
                          acc)))))))

  (define (scheme-basic-format-lines v from to)
    ;; The formatter: rows [from, to] indented to their nearest stops,
    ;; tabs outside strings widened, brackets normalized to convention
    ;; (only pairs the range wholly contains), trailing whitespace
    ;; trimmed; a range reaching the last row drops trailing blank
    ;; lines, so a file ends with exactly one newline.
    (let ([st (fresh-state)]
          [plan (and (scheme-format-brackets) (box '()))]
          [width (scheme-tab-width)])
      (do ([r 0 (+ r 1)]) ((= r from))
        (scan-line! st (vector-ref v r) r #f #f))
      (let loop ([r from] [acc '()] [ends-in-string '()])
        (if (> r to)
            (let ([lines (list->vector (reverse acc))]
                  [stringy (list->vector (reverse ends-in-string))])
              ;; commit the bracket rewrites, then trim
              (when plan
                (for-each (lambda (rw)
                            (string-set! (vector-ref lines (- (car rw) from))
                                         (cadr rw) (caddr rw)))
                          (unbox plan)))
              (let final ([i (- (vector-length lines) 1)] [out '()]
                          [tail (= to (- (vector-length v) 1))])
                (if (< i 0)
                    out
                    (let ([line (if (vector-ref stringy i)
                                    (vector-ref lines i)
                                    (trim-right (vector-ref lines i)))])
                      (if (and tail (string=? line "") (> i 0))
                          (final (- i 1) out #t)      ; a trailing blank
                          (final (- i 1) (cons line out) #f))))))
            (let* ([s (vector-ref v r)]
                   [stops (line-stops st s)]
                   [laid (string-copy
                           (cond
                             [(not stops) s]
                             [(blank? s) s]
                             [else
                              (let ([d (if width (detab s width) s)])
                                (reindent d (settle stops d)))]))])
              (scan-line! st laid r #t plan)
              (loop (+ r 1) (cons laid acc)
                    (cons (sstate-str st) ends-in-string)))))))

  (define (comment-at s)
    ;; The first line-comment semicolon outside strings and character
    ;; literals. Block-comment lines are left to the structural formatter.
    (let ([n (string-length s)])
      (let loop ([i 0] [string? #f] [escaped? #f])
        (cond [(= i n) #f]
              [string?
               (let ([c (string-ref s i)])
                 (cond [escaped? (loop (+ i 1) #t #f)]
                       [(char=? c #\\) (loop (+ i 1) #t #t)]
                       [(char=? c #\") (loop (+ i 1) #f #f)]
                       [else (loop (+ i 1) #t #f)]))]
              [(char=? (string-ref s i) #\")
               (loop (+ i 1) #t #f)]
              [(and (char=? (string-ref s i) #\#) (< (+ i 1) n)
                    (char=? (string-ref s (+ i 1)) #\\))
               (let literal ([j (+ i 2)])
                 (if (and (< j n)
                          (not (scheme-delimiter? (string-ref s j))))
                     (literal (+ j 1))
                     (loop j #f #f)))]
              [(and (char=? (string-ref s i) #\#) (< (+ i 1) n)
                    (char=? (string-ref s (+ i 1)) #\;))
               (loop (+ i 2) #f #f)]
              [(char=? (string-ref s i) #\;) i]
              [else (loop (+ i 1) #f #f)]))))

  (define (block-comment-line? s)
    (or (string-find s "#|") (string-find s "|#")))

  (define (collapse-code-space s)
    ;; One horizontal space per run, preserving leading indentation and every
    ;; byte inside strings and character literals.
    (let ([out (open-output-string)] [n (string-length s)])
      (let leading ([i 0])
        (if (and (< i n) (memv (string-ref s i) '(#\space #\tab)))
            (begin (put-char out #\space) (leading (+ i 1)))
            (let loop ([i i] [string? #f] [escaped? #f] [pending? #f])
              (cond
                [(= i n) (trim-right (get-output-string out))]
                [string?
                 (let ([c (string-ref s i)])
                   (put-char out c)
                   (cond [escaped? (loop (+ i 1) #t #f #f)]
                         [(char=? c #\\) (loop (+ i 1) #t #t #f)]
                         [(char=? c #\") (loop (+ i 1) #f #f #f)]
                         [else (loop (+ i 1) #t #f #f)]))]
                [(memv (string-ref s i) '(#\space #\tab))
                 (loop (+ i 1) #f #f #t)]
                [else
                 (when pending? (put-char out #\space))
                 (let ([c (string-ref s i)])
                   (put-char out c)
                   (cond
                     [(char=? c #\") (loop (+ i 1) #t #f #f)]
                     [(and (char=? c #\#) (< (+ i 1) n)
                           (char=? (string-ref s (+ i 1)) #\\))
                      (put-char out #\\)
                      (when (< (+ i 2) n)
                        (put-char out (string-ref s (+ i 2))))
                      (let literal ([j (min n (+ i 3))])
                        (if (and (< j n)
                                 (not (scheme-delimiter? (string-ref s j))))
                            (begin (put-char out (string-ref s j))
                                   (literal (+ j 1)))
                            (loop j #f #f #f)))]
                     [else (loop (+ i 1) #f #f #f)]))]))))))

  (define (normalize-layout-line s)
    (if (or (block-comment-line? s) (string-find s "#\\"))
        s
        (let ([comment (comment-at s)])
          (if comment
              (let ([code (trim-right (substring s 0 comment))]
                    [text (substring s comment (string-length s))])
                (if (blank? code)
                    (string-append
                      (make-string (indent-of s) #\space) text)
                    (string-append (collapse-code-space code) "  " text)))
              (collapse-code-space s)))))

  (define (layout-lines lines width)
    ;; Fold adjacent continuation lines when they fit. scan-line! supplies the
    ;; reader-aware depth and protects comments, block comments, and continued
    ;; strings from joining.
    (let ([st (fresh-state)])
      (let loop ([rest (map normalize-layout-line lines)]
                 [current #f] [depth 0] [protected? #f] [out '()])
        (if (null? rest)
            (reverse (if current (cons current out) out))
            (let* ([line (car rest)]
                   [before-string (sstate-str st)]
                   [before-block (sstate-blk st)]
                   [comment? (comment-at line)])
              (scan-line! st line 0 #f #f)
              (let* ([next-depth (length (sstate-stack st))]
                     [next-protected?
                      (or before-string (> before-block 0) comment?
                          (sstate-str st) (> (sstate-blk st) 0))]
                     [candidate
                      (and current
                           (string-append current " "
                             (string:tail line (indent-of line))))]
                     [join? (and current (> depth 0) (not protected?)
                                 (not next-protected?) (not (blank? line))
                                 (<= (string-length candidate) width))])
                (loop (cdr rest) (if join? candidate line) next-depth
                      next-protected?
                      (if (or (not current) join?) out
                          (cons current out)))))))))

  (define (break-at s limit)
    ;; Last whitespace at or before limit that is outside a string. Lines with
    ;; comments and block comments never reach this function.
    (let loop ([i 0] [string? #f] [escaped? #f] [last #f])
      (if (or (= i (string-length s)) (> i limit))
          last
          (let ([c (string-ref s i)])
            (cond
              [string?
               (cond [escaped? (loop (+ i 1) #t #f last)]
                     [(char=? c #\\) (loop (+ i 1) #t #t last)]
                     [(char=? c #\") (loop (+ i 1) #f #f last)]
                     [else (loop (+ i 1) #t #f last)])]
              [(char=? c #\") (loop (+ i 1) #t #f last)]
              [(char-whitespace? c) (loop (+ i 1) #f #f i)]
              [else (loop (+ i 1) #f #f last)])))))

  (define (break-layout-line s width)
    (if (or (<= (string-length s) width) (comment-at s)
            (block-comment-line? s))
        (list s)
        (let ([at (break-at s width)])
          (if (or (not at) (<= at (indent-of s)))
              (list s)
              (cons (trim-right (substring s 0 at))
                    (break-layout-line
                      (string-append
                        (make-string (+ (indent-of s) 2) #\space)
                        (string:tail s (+ at 1)))
                      width))))))

  (define (lines-text lines)
    (apply string-append
      (map (lambda (line) (string-append line "\n")) lines)))

  (define (read-all text)
    (guard (ex [else #f])
      (let ([port (open-string-input-port text)])
        (let loop ([data '()])
          (let ([datum (read port)])
            (if (eof-object? datum)
                (cons #t (reverse data))
                (loop (cons datum data))))))))

  (define (same-data? before after)
    (let ([a (read-all (lines-text before))]
          [b (read-all (lines-text after))])
      (and a b (equal? (cdr a) (cdr b)))))

  (define (scheme-format-lines v from to)
    ;; Intrusive, line-count-changing layout is opt-in and limited to a whole
    ;; buffer; region formatting retains the old structural guarantees.
    (let ([basic (scheme-basic-format-lines v from to)])
      (if (and (scheme-format-intrusive)
               (= from 0) (= to (- (vector-length v) 1)))
          (let* ([laid (apply append
                         (map (lambda (line)
                                (break-layout-line line
                                                   (scheme-format-width)))
                              (layout-lines basic (scheme-format-width))))]
                 [all (list->vector laid)])
            (let ([formatted
                   (if (zero? (vector-length all)) '()
                       (scheme-basic-format-lines
                         all 0 (- (vector-length all) 1)))])
              (if (same-data? (vector->list v) formatted) formatted basic)))
          basic)))

)
