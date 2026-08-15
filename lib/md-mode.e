;; md-mode.e -- Markdown highlighting for the e editor.
;;
;; An e extension module: the library (md-mode), loaded at startup by the
;; core, which calls init!.  Markdown is line-oriented, which suits
;; the editor's per-line styles contract: headings, blockquotes, list
;; bullets, fences, and rules are recognized from the line start, then
;; code spans, bold, italics, and links inline.  Fenced code block
;; *contents* are not highlighted specially -- a line-local highlighter
;; cannot know it is inside a fence.

(library (md-mode)
  (export init!)
  (import (chezscheme) (core))

  (define (md-styles s)
    (define n (string-length s))
    (define styles (make-vector n 'plain))
    (define (mark! from to style) (vector-fill-range! styles from to style))
    (define (prefix-at? sub i)
      (let ([m (string-length sub)])
        (and (<= (+ i m) n) (string=? (substring s i (+ i m)) sub))))
    (define (find-char c from)
      (let loop ([i from])
        (cond [(>= i n) #f]
              [(char=? (string-ref s i) c) i]
              [else (loop (+ i 1))])))
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
            [(char=? c #\`)                                  ; `code`
             (let ([close (find-char #\` (+ i 1))])
               (cond [close (mark! i (+ close 1) 'string) (inline (+ close 1))]
                     [else (mark! i n 'string)]))]
            [(or (prefix-at? "**" i) (prefix-at? "__" i))    ; **bold**
             (let* ([marker (substring s i (+ i 2))]
                    [close (string-search s marker (+ i 2) n)])
               (cond [close (mark! i (+ close 2) 'bold) (inline (+ close 2))]
                     [else (inline (+ i 2))]))]
            [(char=? c #\*)                                  ; *italic*
             (let ([close (find-char #\* (+ i 1))])
               (cond [close (mark! i (+ close 1) 'italic) (inline (+ close 1))]
                     [else (inline (+ i 1))]))]
            [(char=? c #\[)                                  ; [text](url)
             (let ([close (find-char #\] (+ i 1))])
               (cond
                 [(not close) (inline (+ i 1))]
                 [else
                  (mark! i (+ close 1) 'quote)
                  (if (and (< (+ close 1) n)
                           (char=? (string-ref s (+ close 1)) #\())
                      (let ([pclose (find-char #\) (+ close 2))])
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
         (let ([scheme (find-mode "scheme")])
           (if scheme
               (or ((mode-styles scheme) s) styles)
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

  (define (init!)
    (register-mode! "markdown" '(".md" ".markdown") '() md-styles)))
