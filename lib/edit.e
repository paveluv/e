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
          next-conflict! keep-mine! keep-disk!
          list-buffers!)
  (import (chezscheme) (core)
          (only (describe) register-descriptions!))

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
                       (let* ([m (string-length from)]
                              [start (region-start r)]
                              [end (region-end r)])
                         (when (= m 0) (error 'edit "empty search string"))
                         (let loop ([row (car start)]
                                    [col (cdr start)]
                                    [end-row (car end)]
                                    [end-col (cdr end)]
                                    [count 0])
                           (cond
                             [(> row (min end-row (- (buffer-line-count
                                                       (region-buffer r)) 1)))
                              (goto-point! saved)
                              count]
                             [else
                              (let* ([s (buffer-line (region-buffer r) row)]
                                     [limit (if (= row end-row)
                                                (min end-col (string-length s))
                                                (string-length s))]
                                     [hit (string-search s from col limit)])
                                (if (not hit)
                                    (loop (+ row 1) 0 end-row end-col count)
                                    (begin
                                      (goto-point! (cons row hit))
                                      (do ([i 0 (+ i 1)]) ((= i m))
                                        (delete-forward!))
                                      (insert-text! to)
                                      (let* ([next (point)]
                                             [rows-added (- (car next) row)])
                                        (if (< row end-row)
                                            (loop (car next) (cdr next)
                                                  (+ end-row rows-added) end-col
                                                  (+ count 1))
                                            (loop (car next) (cdr next)
                                                  (+ end-row rows-added)
                                                  (+ (cdr next)
                                                     (- end-col (+ hit m)))
                                                  (+ count 1)))))))]))))))))))
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
                          (parameterize ([message-source #f]) ; an indicator
                            (set-message! question))
                          (redraw!)     ; the match highlight, not the message
                          (let* ([event (read-key-event #f)]
                                 [action (and (not (eof-object? event))
                                              (key-event-binding
                                                'query-replace event))])
                            (case action
                              [(replace)
                               (goto-point! hit)
                               (do ([i 0 (+ i 1)]) ((= i m)) (delete-forward!))
                               (insert-text! to)
                               (set! replaced (+ replaced 1))
                               (loop (car (point)) (cdr (point)))]
                              [(skip)
                               (set! skipped (+ skipped 1))
                               (goto-point! hit)
                               (loop (car hit) (+ (cdr hit) m))]
                              [(stop) (goto-point! hit)]
                              [(quit-prefix)
                               (let ([next (read-key-event #f)])
                                 (when (and (not (eof-object? next))
                                            (eq? (key-event-binding
                                                   'query-replace "C-x" next)
                                              'quit-editor))
                                   (quit!!))
                                 (goto-point! hit))]
                              [else
                               (if (eof-object? event)
                                   (goto-point! hit)
                                   (loop (car hit) (cdr hit)))]))))))))
              (lambda () (set! query-match #f)))
            (set-message! (format "Replaced ~a, skipped ~a" replaced skipped))
            (void)))))

  ;;; Conflict resolution ---------------------------------------------------------

  ;; A merge left <<<<<<< buffer / ======= / >>>>>>> disk markers:
  ;; next-conflict! hops to one, keep-mine! and keep-disk! resolve the
  ;; conflict at point, each as one undo step.

  (define (conflict-marker? b row prefix)
    (and (>= row 0) (< row (buffer-line-count b))
         (string-prefix? prefix (buffer-line b row))))

  (define (conflict-at row)
    ;; The (start mid end) marker rows of the conflict containing row,
    ;; or #f.
    (let ([b (current-buffer)])
      (let up ([r row])
        (cond
          [(< r 0) #f]
          [(and (< r row) (conflict-marker? b r ">>>>>>>")) #f]
          [(conflict-marker? b r "<<<<<<<")
           (let mid ([m (+ r 1)])
             (cond
               [(>= m (buffer-line-count b)) #f]
               [(conflict-marker? b m "=======")
                (let end ([e (+ m 1)])
                  (cond
                    [(>= e (buffer-line-count b)) #f]
                    [(conflict-marker? b e ">>>>>>>")
                     (and (>= e row) (list r m e))]
                    [else (end (+ e 1))]))]
               [else (mid (+ m 1))]))]
          [else (up (- r 1))]))))

  (define (delete-rows! r1 r2)
    ;; Remove rows r1..r2 inclusive, joining across their newlines.
    (goto-point! (cons r1 0))
    (let ([n (let loop ([r r1] [n 0])
               (if (> r r2)
                   n
                   (loop (+ r 1)
                         (+ n 1 (string-length
                                  (buffer-line (current-buffer) r))))))])
      (do ([i 0 (+ i 1)]) ((= i n)) (delete-forward!))))

  (define (next-conflict!)
    ;; Point to the next conflict's <<<<<<< line, wrapping around.
    (let* ([b (current-buffer)]
           [n (buffer-line-count b)]
           [from (car (point))]
           [hit (let scan ([r (+ from 1)] [left n])
                  (cond [(zero? left) #f]
                        [(>= r n) (scan 0 left)]
                        [(conflict-marker? b r "<<<<<<<") r]
                        [else (scan (+ r 1) (- left 1))]))])
      (if hit
          (goto-point! (cons hit 0))
          (set-message! "No conflicts"))
      (void)))

  (define (keep-mine!)
    ;; Resolve the conflict at point in the buffer's favor.
    (let ([c (conflict-at (car (point)))])
      (if c
          (begin
            (call-as-one-edit! "keep mine"
              (lambda ()
                (delete-rows! (cadr c) (caddr c))
                (delete-rows! (car c) (car c))
                (goto-point! (cons (car c) 0))))
            (set-message! "Kept the buffer side"))
          (set-message! "Not in a conflict"))
      (void)))

  (define (keep-disk!)
    ;; Resolve the conflict at point in the disk's favor.
    (let ([c (conflict-at (car (point)))])
      (if c
          (begin
            (call-as-one-edit! "keep disk"
              (lambda ()
                (delete-rows! (caddr c) (caddr c))
                (delete-rows! (car c) (cadr c))
                (goto-point! (cons (car c) 0))))
            (set-message! "Kept the disk side"))
          (set-message! "Not in a conflict"))
      (void)))

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
           [b (fresh-buffer "*Buffer List*")])
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
    (register-descriptions!
      '(((replace-all!)
         (("procedure" . "(replace-all! from to [where])"))
         "integer" ("(edit)") edit "Editing commands" #f
         "Replace every occurrence of `from` with `to` in `where`. Each buffer is changed as one undo step and point is preserved. If `where` is omitted, use the selected region or the whole current buffer; it may also be a buffer, buffer name, region, buffer predicate, or list of these.")
        ((replace!!)
         (("procedure" . "(replace!! [from] [to] [where])"))
         "void" ("(edit)") edit "Editing commands" #f
         "Interactively visit occurrences of `from` in `where`, asking whether to replace each one with `to`. Any omitted arguments are prompted for. The query accepts `y` or Space to replace, `n` to skip, and `q` or Return to stop.")
        ((next-conflict!) (("procedure" . "(next-conflict!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Move point to the next merge conflict marker in the current buffer, wrapping at the end. Report a message if the buffer has no conflicts.")
        ((keep-mine!) (("procedure" . "(keep-mine!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Resolve the merge conflict at point by keeping the buffer side. The complete resolution is one undo step.")
        ((keep-disk!) (("procedure" . "(keep-disk!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Resolve the merge conflict at point by keeping the disk side. The complete resolution is one undo step.")
        ((list-buffers!) (("procedure" . "(list-buffers!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Display a freshly rebuilt `*Buffer List*` showing each buffer's current, read-only, and modified marks, line count, mode, and file.")))
    (bind-default-key! "C-x C-b" list-buffers!)
    (bind-default-key! "M-%" replace!!)
    (bind-default-key! "M-n" next-conflict!)
    (bind-default-key! "M-m" keep-mine!)
    (bind-default-key! "M-d" keep-disk!)
    (for-each
      (lambda (entry)
        (bind-default-key! 'query-replace (car entry) (cadr entry)))
      '(("y" replace) ("Y" replace) ("SPC" replace)
        ("n" skip) ("N" skip) ("BACKSPACE" skip)
        ("q" stop) ("RET" stop) ("C-g" stop) ("ESC" stop)
        ("C-x" quit-prefix) ("C-x C-c" quit-editor)))
    (add-highlighter!
      (lambda () (if query-match (list query-match) '())))))
