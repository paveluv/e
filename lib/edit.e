;; edit.e -- generic editing helpers for the e editor: the library (edit).
;;
;; An e extension module: the library (edit), loaded at startup by the
;; core.  Layer 1 of the editor: operations composed from the core's
;; public API, hot-reloadable like any module -- extend this file, save,
;; and the helpers update in place.
;;
;; Every command here takes an optional `where` argument saying what to
;; operate on, normalized by regions-of: omitted -- the selected region,
;; or the whole current buffer; a buffer, or its name -- all of it; a
;; region; a predicate -- the buffers it accepts; or a list of any of
;; these.  A region is a slice of one buffer between two (row . col)
;; points, and prints as the expression that rebuilds it, like buffers
;; do:  (region (buffer "e") '(0 . 0) '(12 . 5)).

(library (edit)
  (export init!
          region region? region-buffer region-start region-end
          regions-of region-text
          replace-all! count-matches replace!!
          list-buffers!)
  (import (chezscheme) (core))

  ;;; Regions -----------------------------------------------------------------

  (define-record-type (region-record make-region region?)
    (fields (immutable buffer region-buffer)
            (immutable start region-start)
            (immutable end region-end)))

  (define (point<? a b)
    (or (< (car a) (car b))
        (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

  (define (region b start end)
    ;; The slice of buffer b between two (row . col) points, either order.
    (if (point<? end start)
        (make-region b end start)
        (make-region b start end)))

  (define region-printing
    (record-writer (record-type-descriptor region-record)
      (lambda (r p wr)
        (display "(region " p)
        (wr (region-buffer r) p)
        (display " '" p) (wr (region-start r) p)
        (display " '" p) (wr (region-end r) p)
        (display ")" p))))

  (define (whole-buffer b)
    (let ([last (- (buffer-line-count b) 1)])
      (make-region b '(0 . 0)
                   (cons last (string-length (buffer-line b last))))))

  (define (regions-of where)
    ;; The regions a `where` argument denotes (see the header).
    (cond [(not where)
           (list (let ([m (mark)])
                   (if m
                       (region (current-buffer) m (point))
                       (whole-buffer (current-buffer)))))]
          [(region? where) (list where)]
          [(buffer? where) (list (whole-buffer where))]
          [(string? where) (list (whole-buffer (buffer where)))]
          [(procedure? where) (regions-of (filter where (buffer-list)))]
          [(list? where) (apply append (map regions-of where))]
          [else (error 'regions-of
                       "not a buffer, name, region, predicate, or list"
                       where)]))

  ;;; Matching ----------------------------------------------------------------

  (define (for-matches! r needle handle!)
    ;; Walk the matches of needle inside r in order, calling
    ;; (handle! row col) on each; it returns the width the match occupies
    ;; afterwards (an edit may have changed it).  The match count.
    ;; Needles are single-line: lines are searched one at a time.
    (when (= (string-length needle) 0)
      (error 'edit "empty search string"))
    (let* ([b (region-buffer r)]
           [m (string-length needle)]
           [start (region-start r)]
           [end (region-end r)]
           [count 0])
      (let row-loop ([row (max 0 (car start))])
        (when (<= row (min (car end) (- (buffer-line-count b) 1)))
          (let col-loop ([at (if (= row (car start)) (cdr start) 0)]
                         [shift 0])
            (let* ([s (buffer-line b row)]
                   [limit (if (= row (car end))
                              (min (+ (cdr end) shift) (string-length s))
                              (string-length s))]
                   [hit (string-search s needle at limit)])
              (if hit
                  (let ([w (handle! row hit)])
                    (set! count (+ count 1))
                    (col-loop (+ hit w) (+ shift (- w m))))
                  (row-loop (+ row 1)))))))
      count))

  (define (where-of rest)
    (and (pair? rest) (car rest)))

  ;;; Commands ----------------------------------------------------------------

  (define (count-matches needle . rest)
    ;; How many times needle occurs inside `where`.
    (fold-left (lambda (n r)
                 (+ n (for-matches! r needle
                        (lambda (row col) (string-length needle)))))
               0 (regions-of (where-of rest))))

  (define (replace-all! from to . rest)
    ;; Replace every occurrence of from with to inside `where`: one undo
    ;; step per buffer, point left where it was.  The replacement count.
    (fold-left
      (lambda (n r)
        (+ n (call-with-buffer (region-buffer r)
               (lambda ()
                 (let ([saved (point)])
                   (call-as-one-edit!
                     (format "(replace-all! ~s ~s)" from to)
                     (lambda ()
                       (let ([n (for-matches! r from
                                  (lambda (row col)
                                    (goto-point! (cons row col))
                                    (do ([i 0 (+ i 1)])
                                        ((= i (string-length from)))
                                      (delete-forward!))
                                    (insert-text! to)
                                    (string-length to)))])
                         (goto-point! saved)
                         n))))))))
      0 (regions-of (where-of rest))))

  (define (region-text r)
    ;; The text inside r, rows joined with newlines.
    (let* ([b (region-buffer r)]
           [start (region-start r)]
           [end (region-end r)]
           [last (min (car end) (- (buffer-line-count b) 1))])
      (string-join
        (let loop ([row (max 0 (car start))] [acc '()])
          (if (> row last)
              (reverse acc)
              (let* ([s (buffer-line b row)]
                     [n (string-length s)]
                     [from (if (= row (car start)) (min (cdr start) n) 0)]
                     [to (if (= row (car end)) (min (cdr end) n) n)])
                (loop (+ row 1) (cons (substring s from (max from to)) acc)))))
        "\n")))

  ;;; Query replace -------------------------------------------------------------

  ;; The candidate being offered, drawn highlighted by the highlighter
  ;; init! registers; #f outside replace!!.
  (define query-match #f)

  (define (find-from b needle row col)
    ;; The first match of needle at or after (row . col): (row . start),
    ;; or #f.  Needles are single-line.
    (let loop ([row row] [col col])
      (and (< row (buffer-line-count b))
           (let* ([s (buffer-line b row)]
                  [hit (string-search s needle col (string-length s))])
             (if hit
                 (cons row hit)
                 (loop (+ row 1) 0))))))

  (define (drain-escape!)
    ;; Swallow the tail of an escape sequence -- arrow keys, mouse
    ;; reports -- so it doesn't leak into the buffer as text.
    (when (pending-input?)
      (let ([a (read-key)])
        (when (and a (char=? a #\[))
          (let drain ([c (read-key)])
            (when (and c (or (char<=? #\0 c #\9)
                             (memv c '(#\; #\<))))
              (drain (read-key))))))))

  (define (replace!! . args)
    ;; Query-replace in the current buffer, from point to the end: each
    ;; occurrence of from is highlighted and offered -- y (or SPC)
    ;; replaces, n (or DEL) skips, q / RET / C-g / ESC stops.  Prompts
    ;; for whichever of from and to are not supplied.  The whole run is
    ;; one undo step; point follows, ending after the last replacement
    ;; (or at the start of the last skipped or stopped-at match).  The
    ;; report -- how many replaced and skipped -- is echoed.
    (let* ([from (if (pair? args) (car args) (prompt! "Replace: "))]
           [to (and from
                    (if (and (pair? args) (pair? (cdr args)))
                        (cadr args)
                        (prompt! (format "Replace ~s with: " from))))])
      (if (or (not from) (not to) (string=? from ""))
          (void)                      ; cancelled at a prompt
          (let ([b (current-buffer)]
                [m (string-length from)]
                [question (format "Replace ~s with ~s? (y, n, q)" from to)]
                [replaced 0]
                [skipped 0])
            (dynamic-wind
              void
              (lambda ()
                (call-as-one-edit! (format "(replace!! ~s ~s)" from to)
                  (lambda ()
                    (let loop ([row (car (point))] [col (cdr (point))])
                      (let ([hit (find-from b from row col)])
                        (when hit
                          (set! query-match
                            (list (car hit) (cdr hit) (+ (cdr hit) m)))
                          (goto-point! (cons (car hit) (+ (cdr hit) m)))
                          (set-message! question)
                          (redraw!)
                          (let* ([key (read-key)]
                                 [n (and key (char->integer key))])
                            (cond
                              [(memv n '(121 89 32))          ; y Y SPC
                               (goto-point! hit)
                               (do ([i 0 (+ i 1)]) ((= i m)) (delete-forward!))
                               (insert-text! to)
                               (set! replaced (+ replaced 1))
                               (loop (car (point)) (cdr (point)))]
                              [(memv n '(110 78 127))         ; n N DEL
                               (set! skipped (+ skipped 1))
                               (goto-point! hit)
                               (loop (car hit) (+ (cdr hit) m))]
                              [(not n) (goto-point! hit)]     ; end of input
                              [(= n 27)                       ; ESC stops
                               (drain-escape!) (goto-point! hit)]
                              [(memv n '(7 13 10 113))        ; C-g RET q
                               (goto-point! hit)]
                              [else (loop (car hit) (cdr hit))]))))))))
              (lambda () (set! query-match #f)))
            (set-message! (format "Replaced ~a, skipped ~a" replaced skipped))
            (void)))))

  ;;; The buffer list -----------------------------------------------------------

  (define (pad s width)
    (let ([n (string-length s)])
      (if (>= n width) s (string-append s (make-string (- width n) #\space)))))

  (define (pad-left s width)
    (let ([n (string-length s)])
      (if (>= n width) s (string-append (make-string (- width n) #\space) s))))

  (define (abbreviate-home path)
    (let ([home (getenv "HOME")])
      (if (and home (string-prefix? (string-append home "/") path))
          (string-append "~" (string-tail path (string-length home)))
          path)))

  (define (list-buffers!)
    ;; Pop up a *Buffer List*, Emacs-style: every buffer with its marks
    ;; (. current, % read-only, * modified), length in lines, mode, and
    ;; file, most recently used first.  Rebuilt on every invocation; on
    ;; a screen too small for a second window, a one-line summary.
    (let ([old (buffer-named "*Buffer List*")])
      (when old (kill-buffer! old)))
    (let* ([current (current-buffer)]
           [rows (map (lambda (b)
                        (list (string-append
                                (if (eq? b current) "." " ")
                                (if (buffer-read-only b) "%" " ")
                                (if (buffer-modified b) "*" " "))
                              (buffer-name b)
                              (number->string (buffer-line-count b))
                              (or (buffer-mode-name b) "")
                              (let ([f (buffer-file b)])
                                (if f (abbreviate-home f) ""))))
                      (buffer-list))]
           [all (cons (list "CRM" "Buffer" "Lines" "Mode" "File") rows)]
           [width (lambda (i)
                    (fold-left (lambda (w r) (max w (string-length (list-ref r i))))
                               0 all))]
           [wname (width 1)] [wlines (width 2)] [wmode (width 3)]
           [render (lambda (r)
                     (format "~a  ~a  ~a  ~a  ~a"
                             (car r)
                             (pad (list-ref r 1) wname)
                             (pad-left (list-ref r 2) wlines)
                             (pad (list-ref r 3) wmode)
                             (list-ref r 4)))]
           [b (new-buffer "*Buffer List*")])
      (apply buffer-append! b (map render all))
      (set-buffer-read-only! b #t)
      (call-with-buffer b (lambda () (goto-point! '(0 . 0))))
      (if (display-buffer! b)
          (set-message! "")
          (set-message!
            (fold-left (lambda (acc r)
                         (format "~a ~a~a" acc
                                 (if (char=? (string-ref (car r) 2) #\*) "*" "")
                                 (list-ref r 1)))
                       "Buffers:" rows)))))

  (define (init!)
    (bind-key! "C-x C-b" list-buffers!)
    (bind-key! "M-%" replace!!)
    (add-highlighter!
      (lambda () (if query-match (list query-match) '())))))
