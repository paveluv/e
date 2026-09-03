;; paint.e -- the row painter: the library (paint), v2 core
;; dissolution (docs/DESIGN2.md).  Pure infrastructure with no init!.
;;
;; This is the data-in, ANSI-out half of painting: given a line, its
;; style vector, the marks/links/selection covering it, and the
;; column window to show, emit the minimal styled runs to the
;; terminal-output-port.  Everything is passed in -- no buffer,
;; window, or cache access -- so the painter tests headlessly against
;; a string port.  Frame composition (layout, scrolling, the screen
;; cache, redraw!) stays with the head until head.e.
;;
;; Also here: the pure helpers frame composition shares -- soft-wrap
;; break computation and plain-text hyperlink detection.

(library (paint)
  (export ansi goto fit
          display-editor-line emit-runs
          detect-hyperlinks valid-hyperlink? compute-breaks)
  (import (rnrs) (rnrs mutable-strings)
          (only (chezscheme) format void)
          (only (sys) terminal-output-port)
          (prefix (styles) styles:)
          (prefix (strings) strings:))

  ;;; Output primitives ---------------------------------------------------------

  (define (ansi . xs)
    (for-each (lambda (x) (display x (terminal-output-port))) xs))

  (define (goto r c)
    (ansi "\x1b;[" (number->string r) ";" (number->string c) "H"))

  (define (fit s width)
    (let ([n (string-length s)])
      (if (> n width)
          (substring s 0 width)
          (string-append s (make-string (- width n) #\space)))))

  ;;; The row painter ------------------------------------------------------------

  (define (display-editor-line s shown span marks links left styles edge
                               width bound)
    ;; edge: #f, or the continuation mark for the last column -- 'wrap
    ;; (the line goes on below) or 'trunc (past the right edge).
    ;; bound: the first column past this row's content (a word-wrapped
    ;; segment may end short of the width; the rest pads blank).
    (define n (min (string-length s) bound))
    (define limit (+ left width (if edge -1 0)))
    (define (style-at col)
      (if (and styles (< col n)) (vector-ref styles col) 'plain))
    (define (mark-style m)
      (if (pair? (cddr m)) (caddr m) 'mark))
    (define (covers? m col)
      (and (<= (car m) col) (< col (cadr m))))
    (define (bg-at col)
      ;; The strongest background among the marks covering col:
      ;; match-point and app selections over match, or #f.
      (fold-left (lambda (acc m)
                   (if (and (< col n) (covers? m col))
                       (case (mark-style m)
                         [(match-point) 'match-point]
                         [(active) 'active]
                         [(active-shadow) (or acc 'active-shadow)]
                         [(match) (or acc 'match)]
                         [else acc])
                       acc))
                 #f marks))
    (define (selected? col)
      (and (< col n) span (<= (car span) col) (< col (cdr span))))
    (define (link-at col)
      (find (lambda (link) (and (<= (car link) col) (< col (cadr link))))
            links))
    (define (safe-link-text text)
      (list->string
        (filter (lambda (character)
                  (let ([code (char->integer character)])
                    (and (>= code 32)
                         (not (<= 127 code 159)))))
                (string->list text))))
    (define (safe-link-id text)
      (list->string
        (filter (lambda (character)
                  (or (char-alphabetic? character)
                      (char-numeric? character)
                      (memv character '(#\- #\_ #\.))))
                (string->list text))))
    (define (open-link link)
      (let ([id (and (pair? (cdddr link)) (cadddr link))])
        (ansi "\x1b;]8;"
              (if (and id (string? id))
                  (string-append "id=" (safe-link-id id)) "")
              ";" (safe-link-text (caddr link)) "\x1b;\\")))
    (define (close-link) (ansi "\x1b;]8;;\x1b;\\"))
    (define (segment from to)
      ;; The characters of columns [from, to), off the shown text (the
      ;; mode's display transform, usually the line itself); control
      ;; characters (notably tabs) and columns past the end of the line
      ;; become spaces, so every column is exactly one cell wide.
      (if (vector? shown)
          (let loop ([i from] [parts '()])
            (if (= i to)
                (apply string-append (reverse parts))
                (loop (+ i 1)
                      (cons (if (and (< i (vector-length shown))
                                     (< i bound))
                                (vector-ref shown i) " ")
                            parts))))
          (let ([out (make-string (- to from) #\space)])
            (let loop ([i from])
              (when (and (< i to) (< i (min (string-length shown) bound)))
                (let ([ch (string-ref shown i)])
                  (unless (< (char->integer ch) 32)
                    (string-set! out (- i from) ch)))
                (loop (+ i 1))))
            out)))
    (define (overlay-at col)
      ;; The overlay style covering col: any mark style that is not one
      ;; of the background styles is emitted on top of the base style,
      ;; so highlighters can name their own faces (the bracket match
      ;; does).
      (and (< col n)
           (let ([m (find (lambda (m)
                            (and (covers? m col)
                                 (not (memq (mark-style m)
                                            '(match match-point active
                                                    active-shadow)))))
                          marks)])
             (and m (mark-style m)))))
    ;; Emit runs of identically-attributed columns as single writes.
    (let loop ([col left])
      (when (< col limit)
        (let* ([style (style-at col)]
               [bg (bg-at col)]
               [sel (selected? col)]
               [mk (overlay-at col)]
               [link (link-at col)]
               [end (let run ([j (+ col 1)])
                      (if (and (< j limit)
                               (eq? (style-at j) style)
                               (eq? (bg-at j) bg)
                               (eq? (selected? j) sel)
                               (eq? (overlay-at j) mk)
                               (equal? (link-at j) link))
                          (run (+ j 1))
                          j))])
          (ansi "\x1b;[0m" (styles:style-code style))
          (when sel (ansi (styles:style-code 'selection)))
          (case bg
            [(match-point) (ansi (styles:style-code 'match-point))]
            [(active) (ansi (styles:style-code 'active))]
            [(active-shadow) (ansi (styles:style-code 'active-shadow))]
            [(match) (ansi (styles:style-code 'match))]
            [else (void)])
          (when mk (ansi (styles:style-code mk)))
          (when link (open-link link))
          (ansi (segment col end))
          (when link (close-link))
          (loop end))))
    (when edge
      (ansi "\x1b;[0m" (styles:style-code 'chrome)
            (if (eq? edge 'wrap) "\\" "$")))
    (ansi "\x1b;[0m"))

  (define (emit-runs content styles start end)
    ;; content[start,end) in styled runs, each under its style's code;
    ;; positions past the styles vector paint plain.
    (let emit ([i start])
      (when (< i end)
        (let* ([at (lambda (k)
                     (if (< k (vector-length styles))
                         (vector-ref styles k)
                         'plain))]
               [st (at i)]
               [j (let run ([j (+ i 1)])
                    (if (and (< j end) (eq? (at j) st))
                        (run (+ j 1))
                        j))])
          (ansi "\x1b;[0m" (styles:style-code st) (substring content i j))
          (emit j)))))

  ;;; Soft wrap -------------------------------------------------------------------

  (define (compute-breaks s width)
    (let ([n (string-length s)])
      (let loop ([start 0] [acc '(0)])
        (if (<= (- n start) width)
            (list->vector (reverse acc))
            (let* ([limit (+ start width)]
                   [p (let find ([j limit])
                        (cond [(<= j start) limit]
                              [(char=? (string-ref s (- j 1)) #\space) j]
                              [else (find (- j 1))]))])
              (loop p (cons p acc)))))))

  ;;; Hyperlinks -------------------------------------------------------------------

  (define (url-end-character? character)
    (or (char-whitespace? character)
        (memv character '(#\< #\> #\" #\' #\`))))

  (define (url-trailing-character? character)
    (memv character '(#\. #\, #\; #\: #\! #\? #\) #\] #\})))

  (define (detect-hyperlinks text)
    ;; Return explicit ranges rather than styling URLs directly, so callers
    ;; can inspect the destination and modes can add non-URL labels later.
    (let ([length (string-length text)])
      (let loop ([from 0] [links '()])
        (let ([http (strings:search text "http://" from length)]
              [https (strings:search text "https://" from length)])
          (let ([start (cond [(and http https) (min http https)]
                             [http http]
                             [else https])])
            (if (not start)
                (reverse links)
                (let* ([raw-end
                        (let scan ([at start])
                          (if (or (= at length)
                                  (url-end-character? (string-ref text at)))
                              at
                              (scan (+ at 1))))]
                       [end
                        (let trim ([at raw-end])
                          (if (and (> at start)
                                   (url-trailing-character?
                                     (string-ref text (- at 1))))
                              (trim (- at 1))
                              at))])
                  (if (= end (+ start
                                (if (and https (= start https)) 8 7)))
                      (loop (max (+ start 1) raw-end) links)
                      (loop (max end (+ start 1))
                            (cons (list start end
                                        (substring text start end))
                                  links))))))))))

  (define (valid-hyperlink? link line-length)
    (and (list? link) (<= 3 (length link) 4)
         (integer? (car link)) (integer? (cadr link))
         (<= 0 (car link)) (< (car link) (cadr link))
         (<= (cadr link) line-length) (string? (caddr link)))))
