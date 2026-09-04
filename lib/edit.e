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
          (prefix (head) head:)
          (prefix (keymap) keymap:)
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
          [(head:buffer? where) (list (whole-buffer where))]
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
    (define m (string-length from))
    (define (replace-line s)
      ;; Accumulate pieces and join once instead of copying the growing line
      ;; for every non-overlapping match.
      (let loop ([at 0] [pieces '()] [count 0])
        (let ([hit (string-search s from at (string-length s))])
          (if hit
              (loop (+ hit m)
                    (cons to (cons (substring s at hit) pieces))
                    (+ count 1))
              (values (apply string-append
                             (reverse (cons (string-tail s at) pieces)))
                      count)))))
    (define (rewritten-region r)
      ;; Preserve the single-line-needle contract by rewriting each selected
      ;; row independently, including only the selected edge fragments.
      (let* ([b (region-buffer r)]
             [start (region-start r)]
             [end (region-end r)]
             [last (min (car end) (- (buffer-line-count b) 1))])
        (let loop ([row (max 0 (car start))] [lines '()] [count 0])
          (if (> row last)
              (values (string-join (reverse lines) "\n") count)
              (let* ([s (buffer-line b row)]
                     [n (string-length s)]
                     [from-col (if (= row (car start)) (min (cdr start) n) 0)]
                     [to-col (if (= row (car end)) (min (cdr end) n) n)])
                (let-values ([(line found)
                              (replace-line
                                (substring s from-col (max from-col to-col)))])
                  (loop (+ row 1) (cons line lines) (+ count found))))))))
    (when (= m 0) (error 'edit "empty search string"))
    (fold-left
      (lambda (n r)
        (+ n (call-with-buffer (region-buffer r)
               (lambda ()
                 (let ([saved (point)])
                   (call-as-one-edit!
                     (format "(replace-all! ~s ~s)" from to)
                     (lambda ()
                       (let-values ([(text count) (rewritten-region r)])
                         (when (> count 0)
                           (replace-region-text! (region-start r)
                                                 (region-end r) text))
                         (goto-point! saved)
                         count))))))))
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
                          (let* ([event (head:read-key-event #f)]
                                 [action (and (not (eof-object? event))
                                              (keymap:key-event-binding
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
                               (let ([next (head:read-key-event #f)])
                                 (when (and (not (eof-object? next))
                                            (eq? (keymap:key-event-binding
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

  (define buffers-view #f)
  (define buffer-rows '())

  (define (buffers-styles line)
    ;; Separate the headings from the data; in data rows the third status cell
    ;; is M, where a star makes the complete modified-buffer row italic.
    (make-vector (string-length line)
                 (cond [(string-prefix? "CRM  Buffer" line) 'bold]
                       [(and (> (string-length line) 2)
                             (char=? (string-ref line 2) #\*))
                        'italic]
                       [else 'plain])))

  (define (buffer-table)
    ;; The complete rendering and its source rows. Keeping this pure lets the
    ;; view refresh on every redraw without mutation hooks throughout core.
    (let* ([current (current-buffer)]
           [listed (sort (lambda (a b)
                           (string-ci<? (head:buffer-name a) (head:buffer-name b)))
                         (buffer-list))]
           [view-lines (+ (length listed) 1)]
           [rows (map (lambda (b)
                        (list (string-append
                                (if (eq? b current) "." " ")
                                (if (head:buffer-read-only b) "%" " ")
                                (if (head:buffer-modified b) "*" " "))
                              (head:buffer-name b)
                              (number->string
                                (if (eq? b buffers-view)
                                    view-lines
                                    (buffer-line-count b)))
                              (or (buffer-mode-name b) "")
                              (let ([f (head:buffer-file b)])
                                (if f (abbreviate-home f) ""))))
                      listed)]
           [all (cons (list "CRM" "Buffer" "Lines" "Mode" "File") rows)]
           [width (lambda (i)
                    (fold-left (lambda (w r)
                                 (max w (string-length (list-ref r i))))
                               0 all))]
           [wname (width 1)] [wlines (width 2)] [wmode (width 3)]
           [render (lambda (r)
                     (format "~a  ~a  ~a  ~a  ~a"
                             (car r)
                             (pad (list-ref r 1) wname)
                             (pad-left (list-ref r 2) wlines)
                             (pad (list-ref r 3) wmode)
                             (list-ref r 4)))])
      (set! buffer-rows listed)
      (cons (map render all) rows)))

  (define (refresh-buffers-view!)
    (view-replace! buffers-view (car (buffer-table))))

  (define (buffer-row b)
    (let loop ([left buffer-rows] [row 1])
      (cond [(null? left) #f]
            [(eq? (car left) b) row]
            [else (loop (cdr left) (+ row 1))])))

  (define (select-buffer-row! b)
    (let ([row (buffer-row b)])
      (when row (goto-point! (cons row 0)))))

  (define (clamp-buffer-row!)
    (when (pair? buffer-rows)
      (goto-point! (cons (min (max 1 (car (point))) (length buffer-rows)) 0))))

  (define (move-buffer-row! delta)
    (clamp-buffer-row!)
    (goto-point! (cons (min (max 1 (+ (car (point)) delta))
                            (length buffer-rows))
                       0)))

  (define (activate-buffer-row!)
    (clamp-buffer-row!)
    (let* ([row (car (point))]
           [b (and (<= 1 row (length buffer-rows))
                   (list-ref buffer-rows (- row 1)))])
      (when b
        (show-buffer! b)
        ;; the view acts on the selected window -- its own, so activating
        ;; a row replaces the list with the buffer (unless it is the list)
        (when (eq? (current-buffer) buffers-view)
          (refresh-buffers-view!)
          (select-buffer-row! b)))))

  (define (previous-buffer)
    ;; the buffer the user was in before this view: next in MRU order
    (let ([bs (buffer-list)])
      (if (and (pair? bs) (pair? (cdr bs))) (cadr bs) (car bs))))

  (define (handle-buffers-event! event)
    (cond [(string=? event "FOCUS")
           (refresh-buffers-view!)
           (select-buffer-row! (previous-buffer))
           #t]
          [(member event '("UP" "C-p"))
           (move-buffer-row! -1) #t]
          [(member event '("DOWN" "C-n"))
           (move-buffer-row! 1) #t]
          [(string=? event "WHEEL-UP")
           (move-buffer-row! -1) (activate-buffer-row!) #t]
          [(string=? event "WHEEL-DOWN")
           (move-buffer-row! 1) (activate-buffer-row!) #t]
          [(string=? event "RET") (activate-buffer-row!) #t]
          [(string=? event "MOUSE-CLICK")
           (let ([clicked (app-event-buffer-position)])
             (if (and clicked
                      (<= 1 (car clicked) (length buffer-rows)))
                 (begin
                   (goto-point! (cons (car clicked) 0))
                   (activate-buffer-row!)
                   'keep-focus)
                 'ignore-click))]
          [else #f]))

  (define (switch-buffer-by-row! delta)
    ;; Global alphabetical traversal, matching the stable order in *buffers*.
    (let* ([current (current-buffer)]
           [listed (sort (lambda (a b)
                           (string-ci<? (head:buffer-name a) (head:buffer-name b)))
                         (buffer-list))]
           [tail (memq current listed)])
      (when (and tail (pair? (cdr listed)))
        (let ([next
               (cond [(positive? delta)
                      (if (pair? (cdr tail)) (cadr tail) (car listed))]
                     [(eq? current (car listed)) (car (reverse listed))]
                     [else
                      (let loop ([left listed])
                        (if (eq? (cadr left) current)
                            (car left)
                            (loop (cdr left))))])])
          (show-buffer! next)
          ;; Each window has its own point. If traversal enters the buffers app
          ;; in this window, its active row must describe what this window now
          ;; shows rather than inherit an unrelated row from another app window.
          (when (eq? next buffers-view)
            (refresh-buffers-view!)
            (select-buffer-row! buffers-view))))))

  (define (previous-buffer!) (switch-buffer-by-row! -1))
  (define (next-buffer!) (switch-buffer-by-row! 1))

  (define (buffers-view-buffer)
    ;; Created at startup, or recreated after the user kills the view.
    (or (and buffers-view (memq buffers-view (buffer-list)) buffers-view)
        (begin
          (set! buffers-view (register-app! "*buffers*"
                               refresh-buffers-view!
                               handle-buffers-event!))
          ;; Always show its position bar, using the globally selected side.
          (set-app-presentation! buffers-view 1 #t)
          (set-buffer-mode! buffers-view "buffers")
          (refresh-buffers-view!)
          buffers-view)))

  (define (list-buffers!)
    ;; Use the live table as an interactive buffer switcher in this
    ;; window: Enter shows the chosen buffer here, completing the switch
    ;; in place.
    (let ([b (buffers-view-buffer)]
          [was (current-buffer)])
      (refresh-buffers-view!)
      (show-buffer! b)
      (refresh-buffers-view!)
      (select-buffer-row! was)
      (set-message! "")))

  (define (init!)
    (register-mode! "buffers" '() '() buffers-styles)
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
         "Show `*buffers*` in the current window and make that window its own target. Move through its alphabetical rows with Up, Down, or the wheel; press Enter to replace the app with the selected buffer, or click a row to switch immediately.")
        ((previous-buffer!) (("procedure" . "(previous-buffer!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Switch the current window to the previous buffer in alphabetical order, wrapping at the beginning.")
        ((next-buffer!) (("procedure" . "(next-buffer!)")) "void"
         ("(edit)") edit "Editing commands" #f
         "Switch the current window to the next buffer in alphabetical order, wrapping at the end.")))
    (buffers-view-buffer)
    (add-highlighter!
      (lambda ()
        (cond
          [(and buffers-view (eq? (current-buffer) buffers-view))
           (if (> (car (point)) 0)
               (let ([row (car (point))])
                 (list
                   (list buffers-view row 0
                         (string-length (buffer-line buffers-view row))
                         'active-shadow)
                   (list (selected-window) row 0
                         (string-length (buffer-line buffers-view row))
                         'active)))
               '())]
          [(and buffers-view (memq buffers-view (buffer-list))
                (buffer-row (current-buffer)))
           => (lambda (row)
                (list (list buffers-view row 0
                            (string-length (buffer-line buffers-view row))
                            'active-shadow)))]
          [else '()])))
    (keymap:bind-default-key! "C-x C-b" list-buffers!)
    (keymap:bind-default-key! "M-S-UP" previous-buffer!)
    (keymap:bind-default-key! "M-S-DOWN" next-buffer!)
    (keymap:bind-default-key! "M-%" replace!!)
    (keymap:bind-default-key! "M-n" next-conflict!)
    (keymap:bind-default-key! "M-m" keep-mine!)
    (keymap:bind-default-key! "M-d" keep-disk!)
    (for-each
      (lambda (entry)
        (keymap:bind-default-key! 'query-replace (car entry) (cadr entry)))
      '(("y" replace) ("Y" replace) ("SPC" replace)
        ("n" skip) ("N" skip) ("BACKSPACE" skip)
        ("q" stop) ("RET" stop) ("C-g" stop) ("ESC" stop)
        ("C-x" quit-prefix) ("C-x C-c" quit-editor)))
    (add-highlighter!
      (lambda () (if query-match (list query-match) '())))))
