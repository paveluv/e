;; md-mode.e -- Markdown highlighting for the e editor.
;;
;; An e extension module: the library (md-mode), loaded at startup by the
;; kernel, which calls init!.  Markdown is line-oriented, which suits
;; the editor's per-line styles contract: headings, blockquotes, list
;; bullets, fences, and rules are recognized from the line start, then
;; code spans, bold, italics, and links inline.  Fenced code block
;; contents need cross-line state -- whether a row sits inside ``` ```
;; -- which a memoized whole-buffer scan provides (the c-mode
;; pattern); code in Scheme documents is Scheme, so fence interiors
;; delegate to the scheme mode, like indented blocks.

(library (md-mode)
  (export init!)
  (import (chezscheme) (except (edit) init!)
          (prefix (style) style:)
          (prefix (mode) mode:))

  (define (md-styles s)
    (define n (string-length s))
    (define styles (make-vector n 'plain))
    (define (mark! from to style) (style:fill-range! styles from to style))
    (define (prefix-at? sub i)
      (let ([m (string-length sub)])
        (and (<= (+ i m) n) (string=? (substring s i (+ i m)) sub))))
    (define (next-table match?)
      ;; Nearest matching position at or after every column.  Inline parsing
      ;; then finds delimiters in O(1), including repeated unmatched openers.
      (let ([out (make-vector (+ n 1) #f)])
        (let loop ([i (- n 1)] [next #f])
          (if (< i 0)
              out
              (let ([next (if (match? i) i next)])
                (vector-set! out i next)
                (loop (- i 1) next))))))
    (define (at c i)
      (and (< i n) (char=? (string-ref s i) c)))
    (define next-tick (next-table (lambda (i) (at #\` i))))
    (define next-star (next-table (lambda (i) (at #\* i))))
    (define next-bracket (next-table (lambda (i) (at #\] i))))
    (define next-paren (next-table (lambda (i) (at #\) i))))
    (define next-double-tick
      (next-table (lambda (i) (and (at #\` i) (at #\` (+ i 1))))))
    (define next-double-star
      (next-table (lambda (i) (and (at #\* i) (at #\* (+ i 1))))))
    (define next-double-underscore
      (next-table (lambda (i) (and (at #\_ i) (at #\_ (+ i 1))))))
    (define (next table from) (vector-ref table (min from n)))
    (define (skip-spaces i)
      (if (and (< i n) (char=? (string-ref s i) #\space)) (skip-spaces (+ i 1)) i))
    (define (rule? i)
      ;; Only -, * or _ (at least three, spaces allowed) up to the end.
      (let loop ([i i] [marker #f] [count 0])
        (cond [(>= i n) (>= count 3)]
              [(char=? (string-ref s i) #\space) (loop (+ i 1) marker count)]
              [(and (memv (string-ref s i) '(#\- #\* #\_))
                    (or (not marker) (char=? (string-ref s i) marker)))
               (loop (+ i 1) (string-ref s i) (+ count 1))]
              [else #f])))
    (define (inline i)
      (when (< i n)
        (let ([c (string-ref s i)])
          (cond
            [(char=? c #\`)             ; `code`, or `` code with ` ``
             (let* ([double? (and (< (+ i 1) n)
                                  (char=? (string-ref s (+ i 1)) #\`))]
                    [close (if double?
                               (next next-double-tick (+ i 2))
                               (next next-tick (+ i 1)))])
               (cond [close
                      (let ([end (+ close (if double? 2 1))])
                        (mark! i end 'string)
                        (inline end))]
                     [else (mark! i n 'string)]))]
            [(or (prefix-at? "**" i) (prefix-at? "__" i))    ; **bold**
             (let ([close (next (if (char=? c #\*)
                                    next-double-star
                                    next-double-underscore)
                                (+ i 2))])
               (cond [close (mark! i (+ close 2) 'bold) (inline (+ close 2))]
                     [else (inline (+ i 2))]))]
            [(char=? c #\*)                                  ; *italic*
             (let ([close (next next-star (+ i 1))])
               (cond [close (mark! i (+ close 1) 'italic) (inline (+ close 1))]
                     [else (inline (+ i 1))]))]
            [(char=? c #\[)                                  ; [text](url)
             (let ([close (next next-bracket (+ i 1))])
               (cond
                 [(not close) (inline (+ i 1))]
                 [else
                  (mark! i (+ close 1) 'quote)
                  (if (and (< (+ close 1) n)
                           (char=? (string-ref s (+ close 1)) #\())
                      (let ([pclose (next next-paren (+ close 2))])
                        (cond [pclose (mark! (+ close 1) (+ pclose 1) 'comment)
                                      (inline (+ pclose 1))]
                              [else (mark! (+ close 1) n 'comment)]))
                      (inline (+ close 1)))]))]
            [else (inline (+ i 1))]))))
    (let ([i0 (skip-spaces 0)])
      (cond
        [(>= i0 n) styles]
        [(>= i0 4)
         ;; An indented code block line -- markdown's other code syntax,
         ;; self-identifying per line.  Code in Scheme documents is
         ;; Scheme: delegate to the scheme mode when one is registered.
         (let ([scheme (mode:find "scheme")])
           (if scheme
               (or ((mode:styles scheme) s) styles)
               (begin (mark! 0 n 'string) styles)))]
        [(char=? (string-ref s i0) #\#) (mark! 0 n 'keyword) styles]   ; heading
        [(char=? (string-ref s i0) #\>) (mark! 0 n 'comment) styles]   ; quote
        [(prefix-at? "```" i0) (mark! 0 n 'delimiter) styles]          ; fence
        [(rule? i0) (mark! 0 n 'delimiter) styles]                     ; ---
        [else
         (inline
           (cond
             ;; - bullet
             [(and (memv (string-ref s i0) '(#\- #\* #\+))
                   (< (+ i0 1) n)
                   (char=? (string-ref s (+ i0 1)) #\space))
              (mark! i0 (+ i0 1) 'delimiter)
              (+ i0 2)]
             ;; 1. numbered item
             [(char-numeric? (string-ref s i0))
              (let digits ([i i0])
                (cond [(and (< i n) (char-numeric? (string-ref s i)))
                       (digits (+ i 1))]
                      [(and (< (+ i 1) n)
                            (memv (string-ref s i) '(#\. #\)))
                            (char=? (string-ref s (+ i 1)) #\space))
                       (mark! i0 (+ i 1) 'number)
                       (+ i 2)]
                      [else i0]))]
             [else i0]))
         styles])))

  (define (fence-line? s)
    (let skip ([i 0])
      (cond [(and (< i (string-length s))
                  (char=? (string-ref s i) #\space))
             (skip (+ i 1))]
            [else (and (<= (+ i 3) (string-length s))
                       (string=? (substring s i (+ i 3)) "```"))])))

  (define (analyze v)
    ;; Which rows sit inside a code fence.
    (let ([out (make-vector (vector-length v) #f)])
      (let loop ([i 0] [in #f])
        (cond [(= i (vector-length v)) out]
              [(fence-line? (vector-ref v i)) (loop (+ i 1) (not in))]
              [else (when in (vector-set! out i 'code))
                    (loop (+ i 1) in)]))))

  (define md-row (mode:memoize-analysis analyze))

  (define (md-row-styles b row line)
    ;; Inside a fence the line is Scheme; elsewhere #f falls back to
    ;; the cached per-line markdown styles.
    (and (eq? (md-row b row) 'code)
         (let ([scheme (mode:find "scheme")])
           (and scheme ((mode:styles scheme) line)))))

  (define (init!)
    (mode:register! "markdown" '(".md" ".markdown") '() md-styles
                    #f md-row-styles)))
