;; markdown.sls -- a read-only Markdown viewer for the e editor.
;;
;; An e extension module: the library (markdown), loaded at startup by
;; the kernel, which calls init!.  Renders Markdown as formatted text:
;; emphasis markers are stripped and their text wears the face instead,
;; headings take level faces, soft line breaks inside a paragraph
;; disappear (the window's word wrap lays prose out), tables align
;; their columns, fenced code sits between two rules, and
;; [text](url) shows only the text -- the target lives in the buffer's
;; hyperlink layer, followed with RET or a mouse click.
;;
;; markdown:view! shows a local presentation of a source buffer;
;; markdown:edit! returns to that source without replacing its text.
;; Both try to keep the cursor on the matching content.  C-c v toggles
;; in either mode. Apps can request a companion without changing focus;
;; markdown:view-install! also renders literal input into local views.

(import (only (foundation edoc) elibrary))
(elibrary (apps markdown)
  (export (rename (markdown-browser browser))
          (rename (source-companion companion) (source-view! companion!))
          (rename (markdown-edit! edit!)) init! (rename (markdown-render render))
          (rename (markdown-view! view!)) (rename (markdown-view-install! view-install!))
          (rename (markdown-view-max-width view-max-width)))
  (import (chezscheme)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head echo) echo:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head style) style:)
          (prefix (service file) file:)
          (prefix (only (sys sys) terminal-character-width) sys:))

  ;;; Faces -------------------------------------------------------------

  (define (register-md-faces!)
    (style:set! 'md-h1 '(bold underline))
    (style:set! 'md-h2 '(bold))
    (style:set! 'md-h3 '(bold italic))
    (style:set! 'md-h4 '(italic))
    (style:set! 'md-quote '(italic (foreground bright-black)))
    (style:set! 'md-link '(underline (foreground 33)))
    (style:set! 'md-code '(reset)))

  (edoc "The reading width cap: a view in a wider window wraps at this many columns."
        (value integer))
  (define markdown-view-max-width ;; Reading width cap: a view in a wider window wraps at this many
    ;; columns instead of the full width.
    (make-parameter 80
      (lambda (columns)
        (unless (and (fixnum? columns) (>= columns 20))
          (error 'markdown-view-max-width
                 "must be an integer of at least 20" columns))
        columns)))

  (edoc "The command handed a web link's URL when a link is followed."
        (value string))
  (define markdown-browser ;; The command handed a web link's quoted URL.
    (make-parameter "xdg-open"
      (lambda (command)
        (unless (and (string? command) (> (string-length command) 0))
          (error 'markdown-browser "must be a nonempty command" command))
        command)))

  ;;; Inline rendering ---------------------------------------------------

  (define (display-width text)
    (fold-left (lambda (total c) (+ total (sys:terminal-character-width c)))
               0 (string->list text)))

  (define (render-inline text base)
    ;; Strip emphasis, code, and link markup from one line of prose.
    ;; Returns (values stripped styles links): styles is a vector over
    ;; the stripped text, links a list of (start end url).
    (define n (string-length text))
    (define out '())          ; reversed (char . style)
    (define links '())
    (define (emit! c style) (set! out (cons (cons c style) out)))
    (define (emit-run! from to style)
      (do ([i from (+ i 1)]) ((>= i to))
        (emit! (string-ref text i) style)))
    (define (find-close needle from)
      (let ([m (string-length needle)])
        (let loop ([i from])
          (cond [(> (+ i m) n) #f]
                [(string=? (substring text i (+ i m)) needle) i]
                [else (loop (+ i 1))]))))
    (define (prefix-at? sub i)
      (let ([m (string-length sub)])
        (and (<= (+ i m) n) (string=? (substring text i (+ i m)) sub))))
    (let loop ([i 0])
      (when (< i n)
        (let ([c (string-ref text i)])
          (cond
            [(char=? c #\`)
             (let ([close (find-close "`" (+ i 1))])
               (if close
                   (begin (emit-run! (+ i 1) close 'string)
                          (loop (+ close 1)))
                   (begin (emit! c base) (loop (+ i 1)))))]
            [(or (prefix-at? "**" i) (prefix-at? "__" i))
             (let* ([mark (substring text i (+ i 2))]
                    [close (find-close mark (+ i 2))])
               (if close
                   (begin (emit-run! (+ i 2) close 'bold)
                          (loop (+ close 2)))
                   (begin (emit! c base) (loop (+ i 1)))))]
            [(char=? c #\*)
             (let ([close (find-close "*" (+ i 1))])
               (if (and close (> close (+ i 1)))
                   (begin (emit-run! (+ i 1) close 'italic)
                          (loop (+ close 1)))
                   (begin (emit! c base) (loop (+ i 1)))))]
            [(char=? c #\[)
             (let ([close (find-close "]" (+ i 1))])
               (if (and close
                        (< (+ close 1) n)
                        (char=? (string-ref text (+ close 1)) #\())
                   (let ([pclose (find-close ")" (+ close 2))])
                     (if pclose
                         (let ([start (length out)])
                           (emit-run! (+ i 1) close 'md-link)
                           (set! links
                             (cons (list start (length out)
                                         (substring text (+ close 2)
                                                    pclose))
                                   links))
                           (loop (+ pclose 1)))
                         (begin (emit! c base) (loop (+ i 1)))))
                   (begin (emit! c base) (loop (+ i 1)))))]
            [else (emit! c base) (loop (+ i 1))]))))
    (let* ([pairs (reverse out)]
           [stripped (list->string (map car pairs))]
           [styles (list->vector (map cdr pairs))])
      (values stripped styles (reverse links))))

  ;;; Block rendering ----------------------------------------------------

  (define (blank? s)
    (let loop ([i 0])
      (cond [(= i (string-length s)) #t]
            [(char=? (string-ref s i) #\space) (loop (+ i 1))]
            [else #f])))

  (define (indentation s)
    (let loop ([i 0])
      (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
          (loop (+ i 1))
          i)))

  (define (fence? s)
    (let ([i (indentation s)])
      (and (<= (+ i 3) (string-length s))
           (string=? (substring s i (+ i 3)) "```"))))

  (define (heading-level s)
    (let ([i (indentation s)])
      (let count ([j i])
        (cond [(and (< j (string-length s))
                    (char=? (string-ref s j) #\#))
               (count (+ j 1))]
              [(and (> j i) (< j (string-length s))
                    (char=? (string-ref s j) #\space))
               (- j i)]
              [else #f]))))

  (define (quote-line? s)
    (let ([i (indentation s)])
      (and (< i (string-length s)) (char=? (string-ref s i) #\>))))

  (define (table-line? s)
    (let ([i (indentation s)])
      (and (< i (string-length s)) (char=? (string-ref s i) #\|))))

  (define (table-separator? s)
    (and (table-line? s)
         (let loop ([i 0] [dash #f])
           (cond [(= i (string-length s)) dash]
                 [(memv (string-ref s i) '(#\| #\: #\space)) (loop (+ i 1) dash)]
                 [(char=? (string-ref s i) #\-) (loop (+ i 1) #t)]
                 [else #f]))))

  (define (rule? s)
    (let loop ([i (indentation s)] [marker #f] [count 0])
      (cond [(>= i (string-length s)) (>= count 3)]
            [(char=? (string-ref s i) #\space) (loop (+ i 1) marker count)]
            [(and (memv (string-ref s i) '(#\- #\* #\_))
                  (or (not marker) (char=? (string-ref s i) marker)))
             (loop (+ i 1) (string-ref s i) (+ count 1))]
            [else #f])))

  (define (item-start s)
    ;; (marker . text-start) for a bullet or numbered item, else #f.
    (let ([i (indentation s)] [n (string-length s)])
      (cond
        [(and (< (+ i 1) n)
              (memv (string-ref s i) '(#\- #\* #\+))
              (char=? (string-ref s (+ i 1)) #\space))
         (cons (string-append (make-string i #\space) "\x2022; ") (+ i 2))]
        [(and (< i n) (char-numeric? (string-ref s i)))
         (let digits ([j i])
           (cond [(and (< j n) (char-numeric? (string-ref s j))) (digits (+ j 1))]
                 [(and (< (+ j 1) n)
                       (memv (string-ref s j) '(#\. #\)))
                       (char=? (string-ref s (+ j 1)) #\space))
                  (cons (string-append (substring s i (+ j 1)) " ") (+ j 2))]
                 [else #f]))]
        [else #f])))

  (define (structural? s)
    (or (blank? s) (fence? s) (heading-level s) (quote-line? s)
        (table-line? s) (rule? s) (item-start s)))

  (define (strip-quote s)
    (let* ([i (indentation s)]
           [j (+ i 1)]
           [j (if (and (< j (string-length s))
                       (char=? (string-ref s j) #\space))
                  (+ j 1) j)])
      (substring s j (string-length s))))

  (define (split-cells s)
    ;; The inner cells of a | row, trimmed.
    (let* ([i (indentation s)]
           [body (substring s i (string-length s))]
           [body (if (and (> (string-length body) 0)
                          (char=? (string-ref body 0) #\|))
                     (substring body 1 (string-length body)) body)]
           [body (if (and (> (string-length body) 0)
                          (char=? (string-ref body
                                              (- (string-length body) 1))
                                  #\|))
                     (substring body 0 (- (string-length body) 1)) body)])
      (map (lambda (cell)
             (let trim-front ([s cell])
               (cond [(and (> (string-length s) 0)
                           (char=? (string-ref s 0) #\space))
                      (trim-front (substring s 1 (string-length s)))]
                     [(and (> (string-length s) 0)
                           (char=? (string-ref s (- (string-length s) 1))
                                   #\space))
                      (trim-front (substring s 0 (- (string-length s) 1)))]
                     [else s])))
           (split-parameter-cells body))))

  (define (split-parameter-cells body)
    (let loop ([i 0] [start 0] [acc '()])
      (cond [(= i (string-length body))
             (reverse (cons (substring body start i) acc))]
            [(char=? (string-ref body i) #\|)
             (loop (+ i 1) (+ i 1) (cons (substring body start i) acc))]
            [else (loop (+ i 1) start acc)])))

  (edoc "Render Markdown source lines for display, as four parallel lists with one entry per rendered line: its text, style vector, (start end url) links and source row."
        (source-lines (list-of string) "the Markdown source")
        (width* (list-of integer) "the target width, at most one; 79 by default"))
  (define (markdown-render source-lines . width*)
    (define target-width (if (pair? width*) (car width*) 79))
    ;; (values lines styles links rows): parallel lists, one entry per
    ;; rendered line -- the text, its style vector, its (start end url)
    ;; links, and the source row it came from.
    (define out-lines '())
    (define out-styles '())
    (define out-links '())
    (define out-rows '())
    (define (emit! line styles links row)
      (set! out-lines (cons line out-lines))
      (set! out-styles (cons styles out-styles))
      (set! out-links (cons links out-links))
      (set! out-rows (cons row out-rows)))
    (define (emit-inline! text base row prefix prefix-style)
      (let-values ([(stripped styles links) (render-inline text base)])
        (let* ([lead (string-length prefix)]
               [full (string-append prefix stripped)]
               [vec (make-vector (string-length full) prefix-style)])
          (do ([i 0 (+ i 1)]) ((= i (vector-length styles)))
            (vector-set! vec (+ lead i) (vector-ref styles i)))
          (emit! full vec
                 (map (lambda (l)
                        (list (+ lead (car l)) (+ lead (cadr l)) (caddr l)))
                      links)
                 row))))
    (define (longest-word-width text)
      (let loop ([i 0] [word 0] [best 0])
        (cond [(= i (string-length text)) (max best word)]
              [(char=? (string-ref text i) #\space)
               (loop (+ i 1) 0 (max best word))]
              [else (loop (+ i 1)
                          (+ word (sys:terminal-character-width
                                    (string-ref text i)))
                          best)])))
    (define (allocate-widths naturals minimums avail)
      ;; HTML-like auto layout: natural widths when the table fits,
      ;; otherwise each column's longest word plus the leftover split
      ;; in proportion to the slack -- columns with more text to wrap
      ;; get more room, which keeps rows short.
      (let ([nat (fold-left + 0 naturals)]
            [floor-sum (fold-left + 0 minimums)])
        (cond
          [(<= nat avail) naturals]
          [(>= floor-sum avail)
           ;; even the longest words overflow: squeeze proportionally
           ;; and let the wrapper break words
           (map (lambda (m)
                  (max 1 (div (* m avail) floor-sum)))
                minimums)]
          [else
           (let* ([slack (map - naturals minimums)]
                  [total (fold-left + 0 slack)]
                  [extra (- avail floor-sum)]
                  [shares (map (lambda (s) (div (* s extra) total))
                               slack)]
                  [left (- extra (fold-left + 0 shares))]
                  [widest (fold-left max 0 slack)])
             ;; the rounding leftover goes to the slackest column
             (let give ([ws (map + minimums shares)] [ss slack]
                        [left left])
               (cond [(null? ws) '()]
                     [(and (> left 0) (= (car ss) widest))
                      (cons (+ (car ws) left)
                            (give (cdr ws) (cdr ss) 0))]
                     [else (cons (car ws)
                                 (give (cdr ws) (cdr ss) left))])))])))
    (define (improve-widths widths minimums naturals rendered)
      ;; Hill-climb on total table height: hand single characters from
      ;; a wide column to one cut short of its natural width while the
      ;; table gets shorter -- the character that stops a row from
      ;; wrapping is worth more than a wide column's margin.
      (define wv (list->vector widths))
      (define mv (list->vector minimums))
      (define nv (list->vector naturals))
      (define k (vector-length wv))
      (define (cell-height cell width)
        (length (wrap-cell (car cell) '#() '() width)))
      (define (height)
        (fold-left
          (lambda (total row)
            (+ total
               (let cells ([cs row] [i 0] [m 1])
                 (if (or (null? cs) (= i k))
                     m
                     (cells (cdr cs) (+ i 1)
                            (max m (cell-height (car cs)
                                                (vector-ref wv i))))))))
          0 rendered))
      (define (find-improvement h0)
        (let taker ([i 0])
          (and (< i k)
               (if (>= (vector-ref wv i) (vector-ref nv i))
                   (taker (+ i 1))
                   (let donor ([j 0])
                     (cond
                       [(= j k) (taker (+ i 1))]
                       [(or (= j i)
                            (<= (vector-ref wv j)
                                (max 1 (vector-ref mv j))))
                        (donor (+ j 1))]
                       [else
                        (vector-set! wv i (+ (vector-ref wv i) 1))
                        (vector-set! wv j (- (vector-ref wv j) 1))
                        (let ([h1 (height)])
                          (if (or (< h1 h0)
                                  ;; a tie completes the narrow column:
                                  ;; prose wraps well, a split key or
                                  ;; name column reads broken
                                  (and (= h1 h0)
                                       (< (vector-ref nv i)
                                          (vector-ref wv j))))
                              #t
                              (begin
                                (vector-set! wv i (- (vector-ref wv i) 1))
                                (vector-set! wv j (+ (vector-ref wv j) 1))
                                (donor (+ j 1)))))]))))))
      (let climb ([budget 32])
        (when (and (> budget 0) (find-improvement (height)))
          (climb (- budget 1))))
      (vector->list wv))
    (define (wrap-cell text styles links width)
      ;; Word-wrap one rendered cell into visual lines at most width
      ;; columns wide; each line carries its slice of the styles and
      ;; its reanchored links.  A word longer than the column breaks.
      (define n (string-length text))
      (define (cut-point from)
        ;; (values end next): break before end, resume at next
        (let scan ([i from] [used 0] [space #f])
          (if (= i n)
              (values n n)
              (let ([w (sys:terminal-character-width (string-ref text i))])
                (cond
                  [(<= (+ used w) width)
                   (scan (+ i 1) (+ used w)
                         (if (char=? (string-ref text i) #\space)
                             i space))]
                  [space (values space (+ space 1))]
                  [(= i from) (values (+ i 1) (+ i 1))]
                  [else (values i i)])))))
      (define (line-slice from end)
        (list (substring text from end)
              (let ([v (make-vector (- end from) 'plain)])
                (do ([p from (+ p 1)])
                    ((or (= p end) (>= p (vector-length styles))))
                  (vector-set! v (- p from) (vector-ref styles p)))
                v)
              (filter (lambda (l) l)
                      (map (lambda (l)
                             (let ([s (max (car l) from)]
                                   [e (min (cadr l) end)])
                               (and (< s e)
                                    (list (- s from) (- e from)
                                          (caddr l)))))
                           links))))
      (let build ([from 0] [acc '()])
        (if (>= from n)
            (if (null? acc) (list (list "" '#() '())) (reverse acc))
            (let-values ([(end next) (cut-point from)])
              (build (let skip ([j next])
                       (if (and (< j n)
                                (char=? (string-ref text j) #\space))
                           (skip (+ j 1))
                           j))
                     (cons (line-slice from end) acc))))))
    (define (pad-to text width)
      (let ([shortfall (- width (display-width text))])
        (if (> shortfall 0)
            (string-append text (make-string shortfall #\space))
            text)))
    (let* ([source (list->vector source-lines)]
           [count (vector-length source)])
      (define (line r) (vector-ref source r))
      (define (hard-break? l)
        (let ([n (string-length l)])
          (and (>= n 2) (string=? (substring l (- n 2) n) "  "))))
      (define (trim-right l)
        (let loop ([n (string-length l)])
          (if (and (> n 0) (char=? (string-ref l (- n 1)) #\space))
              (loop (- n 1))
              (substring l 0 n))))
      (define (gather-rows r stop? strip)
        ;; Rows r.. as (source-row raw-text stripped-text) until a
        ;; stopping line.
        (let loop ([j r] [acc '()])
          (if (or (>= j count) (and (> j r) (stop? (line j))))
              (values (reverse acc) j)
              (loop (+ j 1)
                    (cons (list j (line j) (strip (line j))) acc)))))
      (define (emit-prose! entries base prefix prefix-style)
        ;; Joined by single spaces, but a markdown hard break -- a line
        ;; ending in two spaces -- keeps its line ending.
        (let segment ([entries entries] [parts '()] [start #f] [lead prefix])
          (define (flush!)
            (when (pair? parts)
              (emit-inline! (string:join (reverse parts) " ")
                            base start lead prefix-style)))
          (if (null? entries)
              (flush!)
              (let* ([entry (car entries)]
                     [row (car entry)]
                     [raw (cadr entry)]
                     [text (trim-right (caddr entry))]
                     [parts (cons text parts)]
                     [start (or start row)])
                (if (hard-break? raw)
                    (begin
                      (emit-inline! (string:join (reverse parts) " ")
                                    base start lead prefix-style)
                      (segment (cdr entries) '() #f
                               (make-string (string-length prefix)
                                            #\space)))
                    (segment (cdr entries) parts start lead))))))
      (let walk ([r 0])
        (when (< r count)
          (let ([s (line r)])
            (cond
              [(blank? s)
               (unless (and (pair? out-lines) (string=? (car out-lines) ""))
                 (emit! "" '#() '() r))
               (walk (+ r 1))]
              [(fence? s)
               ;; a verbatim block between two rules: the fence's
               ;; language tag sits on the top one
               (let* ([i (indentation s)]
                      [tag (let trim ([t (substring s (+ i 3)
                                                    (string-length s))])
                             (cond [(and (> (string-length t) 0)
                                         (char=? (string-ref t 0) #\space))
                                    (trim (substring t 1
                                                     (string-length t)))]
                                   [(and (> (string-length t) 0)
                                         (char=? (string-ref
                                                   t (- (string-length t)
                                                        1))
                                                 #\space))
                                    (trim (substring t 0
                                                     (- (string-length t)
                                                        1)))]
                                   [else t]))])
                 (let scan ([j (+ r 1)] [rows '()])
                   (if (or (>= j count) (fence? (line j)))
                       (let* ([body (reverse rows)]
                              [label (if (string=? tag "")
                                         ""
                                         (string-append "\x2504; " tag " "))]
                              [width (max (fold-left
                                            (lambda (m l)
                                              (max m (display-width l)))
                                            0 body)
                                          (+ 1 (display-width label)))]
                              [rule (lambda (text k)
                                      (emit! text
                                             (make-vector
                                               (string-length text)
                                               'chrome)
                                             '() k))])
                         ;; dotted top rule carrying the language tag
                         (rule (string-append
                                 label
                                 (make-string
                                   (- width (display-width label))
                                   #\x2504))
                               r)
                         (for-each
                           (lambda (l k)
                             (let* ([vec (make-vector (string-length l)
                                                      'md-code)]
                                    [mode (and (not (string=? tag ""))
                                               (mode:find tag))]
                                    [syntax
                                     (and mode
                                          (guard (ex [else #f])
                                            ((mode:styles mode) l)))])
                               ;; the language's own faces color the code
                               (when (vector? syntax)
                                 (do ([p 0 (+ p 1)])
                                     ((or (= p (vector-length syntax))
                                          (= p (string-length l))))
                                   (let ([face (vector-ref syntax p)])
                                     (unless (eq? face 'plain)
                                       (vector-set! vec p face)))))
                               (emit! l vec '() k)))
                           body
                           (let index ([k (+ r 1)] [acc '()])
                             (if (= (length acc) (length body))
                                 (reverse acc)
                                 (index (+ k 1) (cons k acc)))))
                         (rule (make-string width #\x2504)
                               (min (max 0 (- count 1)) j))
                         (walk (if (>= j count) j (+ j 1))))
                       (scan (+ j 1) (cons (line j) rows)))))]
              [(heading-level s)
               => (lambda (level)
                    (let* ([i (indentation s)]
                           [text (substring s (+ i level 1)
                                            (string-length s))]
                           [face (case level
                                   [(1) 'md-h1] [(2) 'md-h2]
                                   [(3) 'md-h3] [else 'md-h4])])
                      (emit-inline! text face r "" face))
                    (walk (+ r 1)))]
              [(rule? s)
               (emit! (make-string 40 #\x2500)
                      (make-vector 40 'chrome) '() r)
               (walk (+ r 1))]
              [(quote-line? s)
               (let scan ([j r] [entries '()])
                 (if (and (< j count) (quote-line? (line j)))
                     (scan (+ j 1)
                           (cons (list j (line j) (strip-quote (line j)))
                                 entries))
                     (begin
                       (emit-prose! (reverse entries) 'md-quote
                                    "" 'md-quote)
                       (walk j))))]
              [(table-line? s)
               (let scan ([j r] [rows '()])
                 (if (and (< j count) (table-line? (line j)))
                     (scan (+ j 1) (cons (line j) rows))
                     (let* ([raw (reverse rows)]
                            [cells (map split-cells
                                        (filter (lambda (l)
                                                  (not (table-separator? l)))
                                                raw))]
                            [rendered
                             (map (lambda (row-cells)
                                    (map (lambda (cell)
                                           (let-values
                                             ([(text styles links)
                                               (render-inline cell 'plain)])
                                             (list text styles links)))
                                         row-cells))
                                  cells)]
                            [columns
                             (fold-left max 0 (map length rendered))]
                            [naturals
                             (let column ([k 0] [acc '()])
                               (if (= k columns)
                                   (reverse acc)
                                   (column
                                     (+ k 1)
                                     (cons (fold-left
                                             (lambda (m row-cells)
                                               (if (< k (length row-cells))
                                                   (max m (display-width
                                                            (car (list-ref
                                                                   row-cells
                                                                   k))))
                                                   m))
                                             0 rendered)
                                           acc))))]
                            [minimums
                             (let column ([k 0] [acc '()])
                               (if (= k columns)
                                   (reverse acc)
                                   (column
                                     (+ k 1)
                                     (cons (fold-left
                                             (lambda (m row-cells)
                                               (if (< k (length row-cells))
                                                   (max m
                                                        (longest-word-width
                                                          (car (list-ref
                                                                 row-cells
                                                                 k))))
                                                   m))
                                             1 rendered)
                                           acc))))]
                            [widths
                             (improve-widths
                               (allocate-widths
                                 naturals minimums
                                 (- target-width
                                    (* 2 (max 0 (- columns 1)))))
                               minimums naturals rendered)]
                            [header? (exists table-separator? raw)])
                       (let build ([rows rendered] [k r] [first #t])
                         (unless (null? rows)
                           (let* ([row-cells (car rows)]
                                  [wrapped
                                   ;; each cell becomes its visual
                                   ;; lines within the column width
                                   (let fill ([i 0] [cells row-cells]
                                              [acc '()])
                                     (if (= i columns)
                                         (reverse acc)
                                         (fill (+ i 1)
                                               (if (pair? cells)
                                                   (cdr cells) '())
                                               (cons
                                                 (if (pair? cells)
                                                     (apply wrap-cell
                                                       (append
                                                         (car cells)
                                                         (list
                                                           (list-ref
                                                             widths i))))
                                                     (list
                                                       (list "" '#() '())))
                                                 acc))))]
                                  [height (fold-left
                                            (lambda (m lines)
                                              (max m (length lines)))
                                            1 wrapped)])
                             ;; emit the row's visual lines: cells
                             ;; padded and joined, headers bold, links
                             ;; reanchored into the joined line
                             (do ([v 0 (+ v 1)]) ((= v height))
                               (let* ([segments
                                       (map (lambda (lines)
                                              (if (< v (length lines))
                                                  (list-ref lines v)
                                                  (list "" '#() '())))
                                            wrapped)]
                                      [parts (map (lambda (seg w)
                                                    (pad-to (car seg) w))
                                                  segments widths)]
                                      [joined
                                       (let trim ([text (string:join
                                                          parts "  ")])
                                         (let ([n (string-length text)])
                                           (if (and (> n 0)
                                                    (char=? (string-ref
                                                              text (- n 1))
                                                            #\space))
                                               (trim (substring
                                                       text 0 (- n 1)))
                                               text)))]
                                      [vec (make-vector
                                             (string-length joined)
                                             'plain)]
                                      [row-links '()])
                                 (let paint ([at 0] [segs segments]
                                             [parts parts])
                                   (when (pair? segs)
                                     (let* ([seg (car segs)]
                                            [text (car seg)]
                                            [styles (cadr seg)])
                                       (do ([p 0 (+ p 1)])
                                           ((or (= p (string-length text))
                                                (>= (+ at p)
                                                    (vector-length vec))))
                                         (let ([st (if (< p (vector-length
                                                              styles))
                                                       (vector-ref
                                                         styles p)
                                                       'plain)])
                                           (vector-set!
                                             vec (+ at p)
                                             (if (and first header?
                                                      (eq? st 'plain))
                                                 'bold st))))
                                       (for-each
                                         (lambda (l)
                                           (set! row-links
                                             (cons (list (+ at (car l))
                                                         (+ at (cadr l))
                                                         (caddr l))
                                                   row-links)))
                                         (caddr seg))
                                       (paint (+ at
                                                 (string-length
                                                   (car parts))
                                                 2)
                                              (cdr segs) (cdr parts)))))
                                 (emit! joined vec (reverse row-links) k)))
                             (when (and first header?)
                               (emit! (string:join
                                        (map (lambda (w)
                                               (make-string w #\x2500))
                                             widths)
                                        "  ")
                                      (make-vector
                                        (+ (fold-left + 0 widths)
                                           (* 2 (- columns 1)))
                                        'chrome)
                                      '() k))
                             (build (cdr rows) (+ k 1) #f))))
                       (walk j))))]
              [(item-start s)
               => (lambda (start)
                    (let-values ([(entries next)
                                  (gather-rows
                                    r structural?
                                    (let ([first #t])
                                      (lambda (l)
                                        (let ([i (if first (cdr start)
                                                     (indentation l))])
                                          (set! first #f)
                                          (substring l
                                                     (min i
                                                          (string-length l))
                                                     (string-length l))))))])
                      (emit-prose! entries 'plain (car start) 'delimiter)
                      (walk next)))]
              [else
               (let-values ([(entries next)
                             (gather-rows
                               r structural?
                               (lambda (l)
                                 (substring l (indentation l)
                                            (string-length l))))])
                 (emit-prose! entries 'plain "" 'plain)
                 (walk next))]))))
      ;; drop one trailing blank
      (when (and (pair? out-lines) (string=? (car out-lines) ""))
        (set! out-lines (cdr out-lines))
        (set! out-styles (cdr out-styles))
        (set! out-links (cdr out-links))
        (set! out-rows (cdr out-rows)))
      (values (reverse out-lines) (reverse out-styles)
              (reverse out-links) (reverse out-rows))))

  ;;; The mode and the toggle --------------------------------------------

  ;; Inputs and derived rendering are local buffer facts.  The cache
  ;; is plain data, so a new module instance can read its old row map
  ;; while rebuilding styles and text.  A fresh renderer token makes
  ;; implementation changes invalidate the cache like width or input.
  ;; #(styles links source-rows source-lines width measure renderer input revision)
  (define renderer-token (gensym "markdown-renderer"))
  (define (rendering-of b)
    (and (not (head:buffer-store-id b))
         (head:buffer-fact b 'markdown-rendering #f)))
  (define (rendering-styles r) (vector-ref r 0))
  (define (rendering-links r) (vector-ref r 1))
  (define (rendering-rows r) (vector-ref r 2))
  (define (rendering-lines r) (vector-ref r 3))
  (define (rendering-width r) (vector-ref r 4))
  (define (rendering-measure r) (vector-ref r 5))
  (define (rendering-renderer r) (vector-ref r 6))
  (define (rendering-input r) (vector-ref r 7))
  (define (rendering-revision r) (vector-ref r 8))

  (define (view-row-styles b row line)
    (let ([r (rendering-of b)])
      (and r (<= 0 row) (< row (vector-length (rendering-styles r)))
           (vector-ref (rendering-styles r) row))))

  (define (view-row-links b row line)
    (let ([r (rendering-of b)])
      (if (and r (<= 0 row) (< row (vector-length (rendering-links r))))
          (vector-ref (rendering-links r) row)
          '())))

  (define (render-width b)
    ;; Fit tables to the narrowest window showing the buffer -- one
    ;; rendering serves them all -- under the reading-width cap; the
    ;; fallback matches the renderer's own default.
    (let ([width (head:buffer-narrowest-width b)])
      (min (markdown-view-max-width)
           (if width (max 20 width) 79))))

  (define (source-row-at r row)
    (let ([rows (rendering-rows r)])
      (if (zero? (vector-length rows))
          0
          (vector-ref rows (max 0 (min row (- (vector-length rows) 1)))))))

  (define (view-row-showing r source-row)
    ;; Prefer the first rendered row for the closest source row.  A
    ;; wrapped table cell can produce several rows from the same input.
    (let ([rows (rendering-rows r)])
      (let find ([k 0] [best 0] [closest -1])
        (cond [(>= k (vector-length rows)) best]
              [(and (<= (vector-ref rows k) source-row)
                    (> (vector-ref rows k) closest))
               (find (+ k 1) k (vector-ref rows k))]
              [else (find (+ k 1) best closest)]))))

  (define (render-input b)
    (and (not (head:buffer-store-id b))
         (head:buffer-fact b 'markdown-input #f)))

  (define (refresh-render! b)
    (let ([input (render-input b)] [old (rendering-of b)])
      (when input
        (let* ([source (and (head:buffer? input) input)]
               [basis (and old (eq? source (rendering-input old)) (rendering-revision old))]
               [width (render-width b)] [measure (markdown-view-max-width)])
          (let-values ([(lines revision changes)
                        (if source (head:snapshot-since source basis)
                            (values (list->vector input) #f #f))])
            (unless (and old
                         (eq? renderer-token (rendering-renderer old))
                         (eq? source (rendering-input old))
                         (eqv? revision (rendering-revision old))
                         (equal? lines (rendering-lines old))
                         (= width (rendering-width old))
                         (= measure (rendering-measure old)))
              ;; The renderer maps rows, not source columns: markup and
              ;; joined paragraphs make those different coordinate spaces.
              ;; Follow each source row's start through the complete chain;
              ;; keep the view column separately and clamp it on adoption.
              (let* ([deltas (and changes (map caddr changes))]
                     [anchor
                      (lambda (row col)
                        (let* ([p (cons (source-row-at old row) 0)]
                               [p (if deltas (fold-left text:rebase-position p deltas) p)])
                          (cons (max 0 (min (car p) (- (vector-length lines) 1))) col)))]
                     [anchors
                      (if old
                          (map (lambda (entry) (cons (car entry) (anchor (cadr entry) (cddr entry))))
                            (head:buffer-placements b))
                          '())])
                (let-values ([(text styles links rows) (markdown-render (vector->list lines) width)])
                  (let ([r (vector (list->vector styles) (list->vector links) (list->vector rows)
                                   lines width measure renderer-token source revision)])
                    (head:view-replace! b text
                      (list (cons 'wrap (cons 'clean measure)) (cons 'markdown-rendering r))
                      (map (lambda (entry)
                             (cons (car entry) (cons (view-row-showing r (cadr entry)) (cddr entry))))
                           anchors)))))))))))

  (define (refit-views!)
    ;; Width, reading measure, and source text are inputs to the same
    ;; renderer.  The local facts also rediscover views after reload.
    (for-each
      (lambda (b)
        (when (and (render-input b) (head:buffer-window-size b))
          (refresh-render! b)))
      (head:buffers)))

  (edoc "Install Markdown lines as the input of a local view buffer: read-only, in markdown-view mode and rendered now."
        (b buffer "a local buffer")
        (lines (list-of string) "the Markdown lines")
        (returns buffer))
  (define (markdown-view-install! b lines)
    ;; Literal input belongs to an existing local view.
    ;; Rendering can never replace a shared buffer's source text.
    (unless (and (head:buffer? b) (not (head:buffer-store-id b)))
      (error 'markdown-view-install! "expected a local buffer" b))
    (unless (and (list? lines) (for-all string? lines))
      (error 'markdown-view-install! "expected markdown lines" lines))
    (head:buffer-fact-set! b 'markdown-input lines)
    (head:buffer-read-only-set! b #t)
    (mode:choose! "markdown-view" b)
    (refresh-render! b)
    b)

  (define (attach-source-view! b)
    (head:buffer-fact-set! b 'resume-kind 'markdown)
    (head:register-view! b (lambda () (refresh-render! b)))
    (mode:choose! "markdown-view" b)
    b)

  (edoc "The local view buffer rendering a Markdown source buffer, or #f."
        (source buffer "the source buffer")
        (returns (or buffer #f)))
  (define (source-companion source)
    (and (head:buffer? source)
         (find (lambda (b) (eq? (render-input b) source)) (head:buffers))))

  (edoc "The local companion view of a Markdown buffer, created when there is none, under the given name or *markdown NAME*."
        (source buffer "a buffer in markdown mode")
        (name string "a preferred buffer name for a new view")
        (returns buffer))
  (define source-view!
    ;; A source record is the identity, never its mutable label.  The
    ;; relationship belongs only to the local companion, not the store.
    ;; Apps can supply a preferred local label without selecting a window.
    (case-lambda
      [(source) (source-view-named! source #f)]
      [(source name)
       (unless (and (string? name) (> (string-length name) 0))
         (error 'companion! "expected a buffer name" name))
       (source-view-named! source name)]))
  (define (source-view-named! source name)
    (unless (equal? (mode:name-of source) "markdown")
      (error 'companion! "not a markdown buffer" source))
    (head:add-buffer! source)
    (let ([b (or (source-companion source)
                 (head:new-local-buffer!
                   (or name (format "*markdown ~a*" (head:buffer-name source)))))])
      (head:buffer-fact-set! b 'markdown-input source)
      (attach-source-view! b)
      (refresh-render! b)
      b))

  (define (capture-resume b positions)
    (let ([source (render-input b)] [r (rendering-of b)])
      (if (and (head:buffer? source) (head:buffer-store-id source) r)
          (values (list (head:buffer-store-id source) (rendering-revision r) (head:buffer-name b))
            (map (lambda (entry) (cons (car entry) (cons (source-row-at r (cadr entry)) (cddr entry)))) positions))
          (values #f positions))))

  (define (restore-resume reference positions)
    ;; Source row starts follow edits; view columns retain their separate
    ;; meaning. Rendering at this head's width never becomes shared text.
    (apply
      (lambda (id revision name)
        (let-values ([(source anchors)
                      (head:resume-source! id revision
                        (map (lambda (entry) (cons (car entry) (cons (cadr entry) 0))) positions))])
          (if (and source (equal? (mode:name-of source) "markdown"))
              (let* ([b (source-view! source name)] [r (rendering-of b)])
                (values b
                  (map (lambda (anchor position)
                         (cons (car anchor) (cons (view-row-showing r (cadr anchor)) (cddr position))))
                    anchors positions)))
              (values #f positions)))) reference))

  (edoc "Show the rendered view of a Markdown buffer, the current one by default, in this window at the corresponding row."
        (b* (list-of buffer) "the source buffer, at most one"))
  (define (markdown-view! . b*)
    ;; Show a local companion in this window; other windows can keep
    ;; editing the original source at the same time.
    (let ([source (if (pair? b*) (car b*) (head:current-buffer))])
      (head:call-with-display-update
        (lambda ()
          (let ([row (car (head:buffer-point source))]
                [b (source-view! source)])
            (head:show-buffer! b)
            (refresh-render! b)
            (head:goto! (cons (view-row-showing (rendering-of b) row) 0)))))
      (void)))

  (edoc "Return from a Markdown view to its live source buffer."
        (b* (list-of buffer) "the view buffer, at most one"))
  (define (markdown-edit! . b*)
    ;; Return to the live source, without restoring any old snapshot or
    ;; changing its mode, read-only state, file facts, or undo history.
    (let ([b (if (pair? b*) (car b*) (head:current-buffer))])
      (unless (equal? (mode:name-of b) "markdown-view")
        (error 'markdown-edit! "not a markdown view" b))
      (let ([source (render-input b)])
        (unless (and (head:buffer? source) (memq source (head:buffers)))
          (error 'markdown-edit! "no live markdown source" b))
        (head:call-with-display-update
          (lambda ()
            (refresh-render! b)
            (let ([row (source-row-at (rendering-of b) (car (head:buffer-point b)))])
              (head:show-buffer! source)
              (head:goto! (cons row 0))))))
      (void)))

  (define (forget-render! b)
    ;; Killing a source closes its dependent presentations; killing a
    ;; presentation leaves its source alone.  All companions are local.
    (for-each
      (lambda (view)
        (when (eq? (render-input view) b) (head:forget-buffer! view)))
      (head:buffers)))

  ;;; Following links ------------------------------------------------------

  (define (shell-quoted url)
    (string-append
      "'"
      (apply string-append
             (map (lambda (c)
                    (if (char=? c #\') "'\\''" (string c)))
                  (string->list url)))
      "'"))

  (define (link-at-point)
    (let* ([b (head:current-buffer)]
           [pt (head:point)]
           [links (view-row-links b (car pt) #f)])
      (find (lambda (l) (and (<= (car l) (cdr pt)) (< (cdr pt) (cadr l))))
            links)))

  (define (markdown-file? path)
    (or (string:suffix? ".md" path) (string:suffix? ".markdown" path)))

  (define (open-link! url)
    ;; Followed links log under the markdown source; web links go to
    ;; the configured browser command.
    (parameterize ([edit:message-source 'markdown])
      (cond
        [(or (string:prefix? "http://" url)
             (string:prefix? "https://" url))
         (system (format "~a ~a >/dev/null 2>&1 &"
                         (markdown-browser) (shell-quoted url)))
         (edit:set-message! (format "Opened ~a" url))]
        [(string:prefix? "#" url)
         (edit:set-message! "Anchor links are not followed yet")]
        [else
         (let* ([b (head:current-buffer)]
                [input (render-input b)]
                [base (head:buffer-file (if (head:buffer? input) input b))]
                [dir (if base (or (file:directory-part base) "") "")]
                [path (file:expand url)]
                [target (if (string:prefix? "/" path) path
                            (string-append dir path))])
           (edit:visit-file! target)
           ;; a linked markdown document arrives already formatted
           (when (and (markdown-file? url)
                      (equal? (mode:name-of (head:current-buffer))
                              "markdown"))
             (guard (ex [else (void)]) (markdown-view!)))
           (edit:set-message! (format "Followed ~a" url)))])))

  (define (follow-md-link!)
    (let ([link (link-at-point)])
      (if link
          (open-link! (caddr link))
          (edit:set-message! "No link at point"))))

  (define (click-md-link!)
    ;; The click already placed point; only an actual link acts.
    (let ([link (link-at-point)])
      (when link (open-link! (caddr link)))))

  ;; A transient, unlogged echo hint while point rests on a link,
  ;; worn like a prompt label.  Only cursor motion updates it, so
  ;; command feedback in the echo area stays until the user moves.
  (define hint-point #f)
  (define hint-shown #f)

  (define (link-hint)
    (unless (or (prompt:active?) (equal? hint-point (head:point)))
      (set! hint-point (head:point))
      (let ([link (and (equal? (mode:name-of (head:current-buffer))
                               "markdown-view")
                       (link-at-point))])
        (cond
          [link
           (let ([url (caddr link)])
             (unless (equal? hint-shown url)
               (set! hint-shown url)
               (paint:show-prompt-message! "hyperlink: " url #f)))]
          [hint-shown
           (when (equal? (echo:text)
                         (string-append "hyperlink: " hint-shown))
             (paint:show-message! "" #f))
           (set! hint-shown #f)])))
    '())

  (edoc "Install Markdown viewing: its faces, mode, links, highlighter, hooks and session resume, and its describe entries and bindings.")
  (define (init!)
    (register-md-faces!)
    (mode:register! "markdown-view" '() '() (lambda (line) #f)
                    #f view-row-styles)
    (paint:add-hyperlinker! view-row-links)
    (paint:add-highlighter! link-hint)
    (head:add-pre-redraw-hook! refit-views!)
    (head:add-buffer-kill-hook! forget-render!)
    (head:register-resume! 'markdown capture-resume restore-resume)
    ;; Reconstruct callbacks and derived data from local inputs even
    ;; when runtime-created registrations survived module retraction.
    (for-each
      (lambda (b)
        (let ([input (render-input b)])
          (when input
            (when (head:buffer? input) (attach-source-view! b))
            (refresh-render! b))))
      (head:buffers))
    (keymap:bind-default! 'markdown "C-c v" markdown-view!)
    (keymap:bind-default! 'markdown-view "C-c v" markdown-edit!)
    (keymap:bind-default! 'markdown-view "RET" follow-md-link!)
    (keymap:bind-default! 'markdown-view "MOUSE-CLICK" click-md-link!)))
