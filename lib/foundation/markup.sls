;; Width-independent Markdown blocks: source rows, text, semantic roles and
;; links. Character ranges are portable; no backend geometry or callbacks.
(import (only (foundation edoc) elibrary))
(elibrary (foundation markup) (export parse)
  (import (chezscheme) (prefix (foundation string) string:))
  (define (render-inline text base)
    (define n (string-length text))
    (define out '())
    (define emitted 0)
    (define links '())
    (define (emit! c style)
      (set! out (cons (cons c style) out))
      (set! emitted (+ emitted 1)))
    (define (emit-run! from to style)
      (do ([i from (+ i 1)])
        ((>= i to))
        (emit! (string-ref text i) style)))
    (define (find-close needle from)
      (let ([m (string-length needle)])
        (let loop ([i from])
          (cond
            [(> (+ i m) n) #f]
            [(string=? (substring text i (+ i m)) needle) i]
            [else (loop (+ i 1))]))))
    (define (prefix-at? sub i)
      (let ([m (string-length sub)])
        (and (<= (+ i m) n)
             (string=? (substring text i (+ i m)) sub))))
    (let loop ([i 0])
      (when (< i n)
        (let ([c (string-ref text i)])
          (cond
            [(char=? c #\`)
             (let ([close (find-close "`" (+ i 1))])
               (if close
                 (begin
                   (emit-run! (+ i 1) close 'string)
                   (loop (+ close 1)))
                 (begin (emit! c base) (loop (+ i 1)))))]
            [(or (prefix-at? "**" i) (prefix-at? "__" i))
             (let* ([mark (substring text i (+ i 2))]
                    [close (find-close mark (+ i 2))])
               (if close
                 (begin
                   (emit-run! (+ i 2) close 'bold)
                   (loop (+ close 2)))
                 (begin (emit! c base) (loop (+ i 1)))))]
            [(char=? c #\*)
             (let ([close (find-close "*" (+ i 1))])
               (if (and close (> close (+ i 1)))
                 (begin
                   (emit-run! (+ i 1) close 'italic)
                   (loop (+ close 1)))
                 (begin (emit! c base) (loop (+ i 1)))))]
            [(char=? c #\[)
             (let ([close (find-close "]" (+ i 1))])
               (if (and close
                        (< (+ close 1) n)
                        (char=? (string-ref text (+ close 1)) #\())
                 (let ([pclose (find-close ")" (+ close 2))])
                   (if pclose
                       (let ([start emitted])
                         (emit-run! (+ i 1) close 'md-link)
                         (set! links
                           (cons
                             (list
                               start
                               emitted
                               (substring text (+ close 2) pclose))
                             links))
                         (loop (+ pclose 1)))
                       (begin (emit! c base) (loop (+ i 1)))))
                 (begin (emit! c base) (loop (+ i 1)))))]
            [else (emit! c base) (loop (+ i 1))]))))
    (let* ([pairs (reverse out)]
           [stripped (list->string (map car pairs))]
           [styles (list->vector (map cdr pairs))])
      (values stripped styles (reverse links))))
  (define (blank? s)
    (let loop ([i 0])
      (cond
        [(= i (string-length s)) #t]
        [(char=? (string-ref s i) #\space) (loop (+ i 1))]
        [else #f])))
  (define (indentation s)
    (let loop ([i 0])
      (if (and (< i (string-length s))
               (char=? (string-ref s i) #\space))
        (loop (+ i 1))
        i)))
  (define (fence? s)
    (let ([i (indentation s)])
      (and (<= (+ i 3) (string-length s))
           (string=? (substring s i (+ i 3)) "```"))))
  (define (heading-level s)
    (let ([i (indentation s)])
      (let count ([j i])
        (cond
          [(and (< j (string-length s)) (char=? (string-ref s j) #\#))
           (count (+ j 1))]
          [(and (> j i)
                (< j (string-length s))
                (char=? (string-ref s j) #\space))
           (- j i)]
          [else #f]))))
  (define (quote-line? s)
    (let ([i (indentation s)])
      (and (< i (string-length s))
           (char=? (string-ref s i) #\>))))
  (define (table-line? s)
    (let ([i (indentation s)])
      (and (< i (string-length s))
           (char=? (string-ref s i) #\|))))
  (define (table-separator? s)
    (and (table-line? s)
         (let loop ([i 0] [dash #f])
           (cond
             [(= i (string-length s)) dash]
             [(memv (string-ref s i) '(#\| #\: #\space))
              (loop (+ i 1) dash)]
             [(char=? (string-ref s i) #\-) (loop (+ i 1) #t)]
             [else #f]))))
  (define (rule? s)
    (let loop ([i (indentation s)] [marker #f] [count 0])
      (cond
        [(>= i (string-length s)) (>= count 3)]
        [(char=? (string-ref s i) #\space)
         (loop (+ i 1) marker count)]
        [(and (memv (string-ref s i) '(#\- #\* #\_))
              (or (not marker) (char=? (string-ref s i) marker)))
         (loop (+ i 1) (string-ref s i) (+ count 1))]
        [else #f])))
  (define (item-start s)
    (let ([i (indentation s)] [n (string-length s)])
      (cond
        [(and (< (+ i 1) n)
              (memv (string-ref s i) '(#\- #\* #\+))
              (char=? (string-ref s (+ i 1)) #\space))
         (cons (string-append (make-string i #\space) "• ") (+ i 2))]
        [(and (< i n) (char-numeric? (string-ref s i)))
         (let digits ([j i])
           (cond
             [(and (< j n) (char-numeric? (string-ref s j)))
              (digits (+ j 1))]
             [(and (< (+ j 1) n)
                   (memv (string-ref s j) '(#\. #\)))
                   (char=? (string-ref s (+ j 1)) #\space))
              (cons (string-append (substring s i (+ j 1)) " ") (+ j 2))]
             [else #f]))]
        [else #f])))
  (define (structural? s)
    (or (blank? s)
      (fence? s)
      (heading-level s)
      (quote-line? s)
      (table-line? s)
      (rule? s)
      (item-start s)))
  (define (strip-quote s)
    (let* ([i (indentation s)]
           [j (+ i 1)]
           [j (if (and (< j (string-length s))
                       (char=? (string-ref s j) #\space))
                (+ j 1)
                j)])
      (substring s j (string-length s))))
  (define (split-cells s)
    (let* ([i (indentation s)]
           [body (substring s i (string-length s))]
           [body (if (and (> (string-length body) 0)
                          (char=? (string-ref body 0) #\|))
                   (substring body 1 (string-length body))
                   body)]
           [body (if (and (> (string-length body) 0)
                          (char=?
                            (string-ref body (- (string-length body) 1))
                            #\|))
                   (substring body 0 (- (string-length body) 1))
                   body)])
      (map (lambda (cell) (string:trim-spaces cell #t))
           (split-parameter-cells body))))
  (define (split-parameter-cells body)
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(= i (string-length body))
         (reverse (cons (substring body start i) acc))]
        [(char=? (string-ref body i) #\|)
         (loop (+ i 1) (+ i 1) (cons (substring body start i) acc))]
        [else (loop (+ i 1) start acc)])))
  (define (inline text base prefix prefix-style)
    (let-values ([(text styles links)
                  (render-inline text base)])
      (let* ([lead (string-length prefix)]
             [full (string-append prefix text)]
             [n (string-length full)]
             [face (lambda (i)
                     (if (< i lead)
                       prefix-style
                       (vector-ref styles (- i lead))))])
        (list
          full
          (let loop ([i 0] [out '()])
            (if (= i n)
              (reverse out)
              (let* ([role (face i)]
                     [end (let scan ([j (+ i 1)])
                            (if (and (< j n) (eq? role (face j)))
                                (scan (+ j 1))
                                j))])
                (loop
                  end
                  (if (eq? role 'plain)
                      out
                      (cons (list i end role) out))))))
          (map (lambda (l)
                 (list (+ lead (car l)) (+ lead (cadr l)) (caddr l)))
               links)))))

  (edoc
    "Interpret Markdown as width-independent blocks with source rows, character-based role spans and link targets. Line blocks hold (text roles links), code blocks retain language and raw lines, and tables retain source rows and inline cells. No layout, display backend or mode callbacks run."
    (lines (list-of string) "Markdown source")
    (returns list "portable semantic blocks"))
  (define (parse lines)
    (define source (list->vector lines))
    (define count (vector-length source))
    (define out '())
    (define (line r) (vector-ref source r))
    (define (emit! block) (set! out (cons block out)))
    (define (emit-inline! text base row prefix prefix-style)
      (emit!
        (list 'line row (inline text base prefix prefix-style))))
    (define (hard-break? l)
      (let ([n (string-length l)])
        (and (>= n 2) (string=? (substring l (- n 2) n) "  "))))
    (define (prose! entries base prefix prefix-style)
      (let segment ([entries entries]
                    [parts '()]
                    [start #f]
                    [lead prefix])
        (if (null? entries)
          (when (pair? parts)
            (emit-inline! (string:join (reverse parts) " ") base start
              lead prefix-style))
          (let* ([entry (car entries)]
                 [parts (cons
                          (string:trim-spaces (caddr entry) #f)
                          parts)]
                 [start (or start (car entry))])
            (if (hard-break? (cadr entry))
                (begin
                  (emit-inline! (string:join (reverse parts) " ") base
                    start lead prefix-style)
                  (segment
                    (cdr entries)
                    '()
                    #f
                    (make-string (string-length prefix) #\space)))
                (segment (cdr entries) parts start lead))))))
    (define (gather r stop? strip)
      (let loop ([j r] [out '()])
        (if (or (= j count) (and (> j r) (stop? (line j))))
          (values (reverse out) j)
          (loop
            (+ j 1)
            (cons (list j (line j) (strip (line j) (= j r))) out)))))
    (let walk ([r 0])
      (when (< r count)
        (let ([s (line r)])
          (cond
            [(blank? s)
             (unless (and (pair? out) (equal? (cddar out) '(("" () ()))))
               (emit! (list 'line r '("" () ()))))
             (walk (+ r 1))]
            [(fence? s)
             (let ([tag (string:trim-spaces
                          (substring
                            s
                            (+ (indentation s) 3)
                            (string-length s))
                          #t)])
               (let scan ([j (+ r 1)] [body '()])
                 (if (or (= j count) (fence? (line j)))
                   (begin
                     (emit!
                       (list 'code r (min (- count 1) j) tag
                         (reverse body)))
                     (walk (if (= j count) j (+ j 1))))
                   (scan (+ j 1) (cons (line j) body)))))]
            [(heading-level s) =>
             (lambda (level)
               (let ([role (case level
                             [(1) 'md-h1]
                             [(2) 'md-h2]
                             [(3) 'md-h3]
                             [else 'md-h4])])
                 (emit-inline!
                   (substring
                     s
                     (+ (indentation s) level 1)
                     (string-length s))
                   role r "" role))
               (walk (+ r 1)))]
            [(rule? s) (emit! (list 'rule r)) (walk (+ r 1))]
            [(quote-line? s)
             (let-values ([(entries next)
                           (gather
                             r
                             (lambda (s) (not (quote-line? s)))
                             (lambda (s first?) (strip-quote s)))])
               (prose! entries 'md-quote "" 'md-quote)
               (walk next))]
            [(table-line? s)
             (let scan ([j r] [rows '()] [header? #f])
               (cond
                 [(or (= j count) (not (table-line? (line j))))
                  (emit! (list 'table r header? (reverse rows)))
                  (walk j)]
                 [(table-separator? (line j)) (scan (+ j 1) rows #t)]
                 [else
                  (scan
                    (+ j 1)
                    (cons
                      (cons
                        j
                        (map (lambda (cell) (inline cell 'plain "" 'plain))
                             (split-cells (line j))))
                      rows)
                    header?)]))]
            [else
             (let* ([item (item-start s)]
                    [prefix (if item (car item) "")])
               (let-values ([(entries next)
                             (gather
                               r
                               structural?
                               (lambda (s first?)
                                 (substring
                                   s
                                   (if (and first? item)
                                     (cdr item)
                                     (indentation s))
                                   (string-length s))))])
                 (prose! entries 'plain prefix 'delimiter)
                 (walk next)))]))))
    (when (and (pair? out)
               (equal? (cddar out) '(("" () ())))
               (eq? (caar out) 'line))
      (set! out (cdr out)))
    (reverse out)))
