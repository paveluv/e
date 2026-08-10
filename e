#!/usr/bin/scheme --script
;; e -- a tiny, single-file Emacs-like console editor for Chez Scheme.
;; Run:  ./e [file]   (or: scheme --script e [file])
;;
;; Extension modules (*.e files in ~/.e) are plain Scheme source loaded at
;; startup into the editor's top level, where they can use and replace any
;; of its definitions -- see the line-styles hook for syntax highlighting.

(import (chezscheme))

;;; Buffers and windows ----------------------------------------------------

(define-record-type buffer
  (fields (mutable name) (mutable lines) (mutable file) (mutable trailing)
          (mutable modified) (mutable history) (mutable hist-dir)
          (mutable hist-last) (mutable mark-row) (mutable mark-col)
          (mutable marked)
          ;; where point was when the buffer was last displayed
          (mutable spot-row) (mutable spot-col) (mutable spot-top)))

(define-record-type window
  (fields (mutable buffer) (mutable top) (mutable left)
          (mutable prow) (mutable pcol)))

(define (new-buffer name)
  (make-buffer name (vector "") #f #t #f (vector '() '()) 'undo #f
               0 0 #f 0 0 0))

(define buffers (list (new-buffer "*scratch*")))        ; most recent first
(define windows (list (make-window (car buffers) 0 0 0 0))) ; top to bottom
(define current-window (car windows))

;; The rest of the editor is written against simple state names: `lines`,
;; `point-row`, and so on.  Each name is an identifier macro reading and
;; writing through current-window, so every command transparently follows
;; buffer and window switches.
(define-syntax define-state
  (syntax-rules ()
    [(_ name place get put)
     (define-syntax name
       (identifier-syntax
         [id (get place)]
         [(set! id v) (put place v)]))]))

(define-state lines (window-buffer current-window)
  buffer-lines buffer-lines-set!)
(define-state file-name (window-buffer current-window)
  buffer-file buffer-file-set!)
(define-state trailing-newline? (window-buffer current-window)
  buffer-trailing buffer-trailing-set!)
(define-state modified? (window-buffer current-window)
  buffer-modified buffer-modified-set!)
(define-state history (window-buffer current-window)
  buffer-history buffer-history-set!)
(define-state history-direction (window-buffer current-window)
  buffer-hist-dir buffer-hist-dir-set!)
(define-state last-history-command (window-buffer current-window)
  buffer-hist-last buffer-hist-last-set!)
(define-state mark-row (window-buffer current-window)
  buffer-mark-row buffer-mark-row-set!)
(define-state mark-col (window-buffer current-window)
  buffer-mark-col buffer-mark-col-set!)
(define-state mark-active? (window-buffer current-window)
  buffer-marked buffer-marked-set!)
(define-state point-row current-window window-prow window-prow-set!)
(define-state point-col current-window window-pcol window-pcol-set!)
(define-state top-row current-window window-top window-top-set!)
(define-state left-col current-window window-left window-left-set!)

;;; Editor state ------------------------------------------------------------

(define editor-name "e")
(define message "C-s search  C-_ undo  C-x b switch  C-x C-s save  C-x C-c quit")
(define kill-ring "")
(define last-command #f)
(define suppress-history (make-parameter #f))
(define quit? #f)
(define key-prefix #f)
(define search-highlight "")
(define rows 24)
(define cols 80)
(define stdin (current-input-port))
(define stdout (current-output-port))

;;; Terminal size ---------------------------------------------------------

(define size-dirty? #t)
(define terminal-ioctl
  (guard (ex [else #f])
    (load-shared-object "libc.so.6")
    (foreign-procedure "ioctl" (int unsigned-long u8*) int)))

;; SIGWINCH is signal 28 on the Unix systems targeted by this little editor.
;; C-l also forces a size refresh in case a platform uses another number.
(guard (ex [else (void)])
  (register-signal-handler 28 (lambda args (set! size-dirty? #t))))

(define (env-number name fallback)
  (let* ([s (getenv name)]
         [n (and s (string->number s))])
    (if (and n (exact? n) (integer? n) (> n 0)) n fallback)))

(define (terminal-size!)
  (when size-dirty?
    (set! size-dirty? #f)
    (set! rows (max 4 (env-number "LINES" 24)))
    (set! cols (max 20 (env-number "COLUMNS" 80)))
    (when terminal-ioctl
      (guard (ex [else (void)])
        (let ([size (make-bytevector 8 0)])
          (when (= (terminal-ioctl
                     (port-file-descriptor (standard-output-port)) #x5413 size) 0)
            (let ([r (bytevector-u16-native-ref size 0)]
                  [c (bytevector-u16-native-ref size 2)])
              (when (> r 0) (set! rows (max 4 r)))
              (when (> c 0) (set! cols (max 20 c))))))))))

;;; Small utilities -------------------------------------------------------

(define (string-tail s i) (substring s i (string-length s)))

(define (string-insert s at addition)
  (string-append (substring s 0 at) addition (string-tail s at)))

(define (string-delete s from to)
  (string-append (substring s 0 from) (string-tail s to)))

(define (error-text ex)
  (if (condition? ex)
      (with-output-to-string (lambda () (display-condition ex)))
      (format "~a" ex)))

(define (string-suffix? suffix s)
  (let ([n (string-length s)] [m (string-length suffix)])
    (and (>= n m) (string=? (substring s (- n m) n) suffix))))

(define (string-search s needle start limit)
  ;; Index of the first occurrence of needle inside s[start, limit), or #f.
  (let ([len (string-length needle)])
    (let loop ([i start])
      (cond [(> (+ i len) limit) #f]
            [(let match ([j 0])
               (or (= j len)
                   (and (char=? (string-ref s (+ i j)) (string-ref needle j))
                        (match (+ j 1)))))
             i]
            [else (loop (+ i 1))]))))

(define (split-lines s)
  (let loop ([start 0] [i 0] [acc '()])
    (cond [(= i (string-length s))
           (reverse (cons (substring s start i) acc))]
          [(char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
          [else (loop start (+ i 1) acc)])))

(define (vector-splice v from to inserted)
  ;; A copy of v with elements [from, to) replaced by the list `inserted`.
  (vector-append (vector-copy v 0 from)
                 (list->vector inserted)
                 (vector-copy v to (- (vector-length v) to))))

(define (vector-fill-range! v from to x)
  (let loop ([i from])
    (when (< i to) (vector-set! v i x) (loop (+ i 1)))))

(define (insert-after lst x y)
  ;; A copy of lst with y inserted right after x (or at the end).
  (cond [(null? lst) (list y)]
        [(eq? (car lst) x) (cons x (cons y (cdr lst)))]
        [else (cons (car lst) (insert-after (cdr lst) x y))]))

;;; Buffer access and undo ------------------------------------------------

(define (vlen) (vector-length lines))
(define (line-at n) (vector-ref lines n))
(define (current-line) (line-at point-row))
(define (set-line! n s) (vector-set! lines n s))

(define (editor-snapshot)
  (list (vector-copy lines) point-row point-col modified?))

(define (restore-snapshot! snapshot)
  ;; The snapshot was just popped off a history stack, so nothing else
  ;; references its line vector and it can be adopted without copying.
  (set! lines (car snapshot))
  (set! point-row (cadr snapshot))
  (set! point-col (caddr snapshot))
  (set! modified? (cadddr snapshot))
  (set! mark-active? #f)
  (invalidate-screen-cache!))

(define (record-edit!)
  (unless (suppress-history)
    (vector-set! history 0 (cons (editor-snapshot) (vector-ref history 0)))
    (vector-set! history 1 '())
    (set! history-direction 'undo)))

(define (history-shift! from to label)
  (if (null? (vector-ref history from))
      (set! message (format "No further ~a information" (string-downcase label)))
      (let ([snapshot (car (vector-ref history from))])
        (vector-set! history from (cdr (vector-ref history from)))
        (vector-set! history to (cons (editor-snapshot) (vector-ref history to)))
        (restore-snapshot! snapshot)
        (set! message label))))

(define (undo-command!)
  ;; C-g after an undo flips the direction, giving a simple redo.
  (if (eq? history-direction 'redo)
      (history-shift! 1 0 "Redo")
      (history-shift! 0 1 "Undo"))
  (set! last-history-command 'undo))

;;; Point, mark, and editing ----------------------------------------------

(define (changed!)
  (set! modified? #t) (set! message "") (set! mark-active? #f)
  (set! goal-pos #f))

(define (ordered-region) ; -> start-row start-col end-row end-col
  (if (or (< point-row mark-row)
          (and (= point-row mark-row) (< point-col mark-col)))
      (values point-row point-col mark-row mark-col)
      (values mark-row mark-col point-row point-col)))

(define (clamp-point!)
  (set! point-row (max 0 (min point-row (- (vlen) 1))))
  (set! point-col (max 0 (min point-col (string-length (current-line))))))

(define (move-left!)
  (cond [(> point-col 0) (set! point-col (- point-col 1))]
        [(> point-row 0)
         (set! point-row (- point-row 1))
         (set! point-col (string-length (current-line)))]))

(define (move-right!)
  (cond [(< point-col (string-length (current-line)))
         (set! point-col (+ point-col 1))]
        [(< point-row (- (vlen) 1))
         (set! point-row (+ point-row 1)) (set! point-col 0)]))

;; Vertical moves aim for a goal column, so point comes back to it after
;; passing through shorter lines (as in Emacs).  The goal survives exactly
;; as long as each command finds point where the previous vertical move
;; left it; anything else that moves point starts a fresh goal.
(define goal-col 0)
(define goal-pos #f)

(define (move-vertical! delta)
  (unless (equal? goal-pos (cons point-row point-col))
    (set! goal-col point-col))
  (set! point-row (max 0 (min (+ point-row delta) (- (vlen) 1))))
  (set! point-col (min goal-col (string-length (current-line))))
  (set! goal-pos (cons point-row point-col)))

(define (insert-text! s)
  (record-edit!)
  (set-line! point-row (string-insert (current-line) point-col s))
  (set! point-col (+ point-col (string-length s)))
  (changed!))

(define (newline!)
  (record-edit!)
  (let ([s (current-line)])
    (set-line! point-row (substring s 0 point-col))
    (set! lines (vector-splice lines (+ point-row 1) (+ point-row 1)
                               (list (string-tail s point-col))))
    (set! point-row (+ point-row 1)) (set! point-col 0)
    (changed!)))

(define (delete-forward!)
  (cond [(< point-col (string-length (current-line)))
         (record-edit!)
         (set-line! point-row
           (string-delete (current-line) point-col (+ point-col 1)))
         (changed!)]
        [(< point-row (- (vlen) 1))
         (record-edit!)
         (set-line! point-row
           (string-append (current-line) (line-at (+ point-row 1))))
         (set! lines (vector-splice lines (+ point-row 1) (+ point-row 2) '()))
         (changed!)]))

(define (backspace!)
  (when (or (> point-col 0) (> point-row 0))
    (record-edit!)
    (parameterize ([suppress-history #t])
      (move-left!) (delete-forward!))))

;;; Kill and yank ---------------------------------------------------------

(define (kill! text)
  ;; Consecutive kill commands accumulate into a single kill-ring entry.
  (set! kill-ring
    (if (eq? last-command 'kill) (string-append kill-ring text) text))
  (set! last-command 'kill))

(define (kill-line!)
  (let* ([s (current-line)] [n (string-length s)])
    (cond [(< point-col n)
           (record-edit!)
           (kill! (substring s point-col n))
           (set-line! point-row (substring s 0 point-col))
           (changed!)]
          [(< point-row (- (vlen) 1))
           (kill! "\n")
           (delete-forward!)])))

(define (yank!)
  ;; Kill-ring entries can span lines after consecutive C-k commands.  Insert
  ;; newlines as buffer structure rather than embedding them in a line string.
  (unless (string=? kill-ring "")
    (record-edit!)
    (parameterize ([suppress-history #t])
      (let ([parts (split-lines kill-ring)])
        (insert-text! (car parts))
        (for-each (lambda (part) (newline!) (insert-text! part))
                  (cdr parts))))))

(define (region-text sr sc er ec)
  (if (= sr er)
      (substring (line-at sr) sc ec)
      (let loop ([row (- er 1)] [acc (list (substring (line-at er) 0 ec))])
        (if (< row sr)
            (apply string-append acc)
            (loop (- row 1)
                  (cons (if (= row sr)
                            (string-tail (line-at sr) sc)
                            (line-at row))
                        (cons "\n" acc)))))))

(define (delete-region! sr sc er ec)
  (if (= sr er)
      (set-line! sr (string-delete (line-at sr) sc ec))
      (let ([joined (string-append (substring (line-at sr) 0 sc)
                                   (string-tail (line-at er) ec))])
        (set! lines (vector-splice lines sr (+ er 1) (list joined))))))

(define (kill-region!)
  (if (not mark-active?)
      (set! message "The mark is not set now")
      (let-values ([(sr sc er ec) (ordered-region)])
        (if (and (= sr er) (= sc ec))
            (set! message "Empty region")
            (begin
              (record-edit!)
              (kill! (region-text sr sc er ec))
              (delete-region! sr sc er ec)
              (set! point-row sr) (set! point-col sc)
              (changed!)
              (set! last-command 'kill))))))

;;; Files -----------------------------------------------------------------

(define (read-file path)
  (call-with-input-file path
    (lambda (p)
      (let ([s (get-string-all p)])
        (if (eof-object? s) "" s)))))

(define (base-name path)
  (let loop ([i (- (string-length path) 1)])
    (cond [(< i 0) path]
          [(char=? (string-ref path i) #\/) (string-tail path (+ i 1))]
          [else (loop (- i 1))])))

(define (unique-name base self)
  ;; base, or base<2>, base<3>, ... -- whichever no other buffer uses.
  (let loop ([k 1])
    (let ([name (if (= k 1) base (format "~a<~a>" base k))])
      (if (find (lambda (b) (and (not (eq? b self))
                                 (string=? (buffer-name b) name)))
                buffers)
          (loop (+ k 1))
          name))))

(define (file-buffer path)
  ;; A fresh buffer visiting path; #f (with a message) when it cannot be read.
  (if (file-exists? path)
      (guard (ex [else (set! message (format "Cannot open ~a: ~a"
                                             path (error-text ex)))
                       #f])
        (let* ([content (read-file path)]
               [n (string-length content)]
               [ends? (and (> n 0)
                           (char=? (string-ref content (- n 1)) #\newline))]
               [body (if ends? (substring content 0 (- n 1)) content)]
               [b (new-buffer (unique-name (base-name path) #f))])
          (buffer-lines-set! b (list->vector (split-lines body)))
          (buffer-trailing-set! b ends?)
          (buffer-file-set! b path)
          (set! message (format "Loaded ~a" path))
          b))
      (let ([b (new-buffer (unique-name (base-name path) #f))])
        (buffer-file-set! b path)
        (set! message (format "New file: ~a" path))
        b)))

(define (visit-file! path)
  ;; Switch to the buffer visiting path, creating it if necessary.
  (cond [(find (lambda (b) (equal? (buffer-file b) path)) buffers)
         => show-buffer!]
        [(file-buffer path) => show-buffer!]))

(define (save-file! path)
  (guard (ex [else (set! message (format "Save failed: ~a" (error-text ex))) #f])
    (call-with-output-file path
      (lambda (p)
        (let loop ([i 0])
          (when (< i (vlen))
            (display (line-at i) p)
            (when (or (< i (- (vlen) 1)) trailing-newline?) (newline p))
            (loop (+ i 1)))))
      'replace)
    (set! file-name path) (set! modified? #f)
    (let ([b (window-buffer current-window)])
      (buffer-name-set! b (unique-name (base-name path) b)))
    (set! message (format "Wrote ~a" path)) #t))

(define (buffer-text b)
  (let* ([v (buffer-lines b)] [n (vector-length v)])
    (let loop ([i (- n 1)] [acc (if (buffer-trailing b) (list "\n") '())])
      (let ([acc (cons (vector-ref v i) acc)])
        (if (= i 0)
            (apply string-append acc)
            (loop (- i 1) (cons "\n" acc)))))))

(define (buffer-clean? b)
  ;; Nothing is lost by discarding b: it was never modified, its text is
  ;; identical to what is on disk again, or it is an empty file-less buffer.
  (or (not (buffer-modified b))
      (let ([path (buffer-file b)])
        (if path
            (and (file-exists? path)
                 (guard (ex [else #f])
                   (string=? (buffer-text b) (read-file path))))
            (let ([v (buffer-lines b)])
              (and (= (vector-length v) 1)
                   (string=? (vector-ref v 0) "")))))))

;;; Buffer and window commands ---------------------------------------------

(define (set-window-buffer! w b)
  ;; Display b in w, remembering where point was in the old buffer and
  ;; restoring where it last was in the new one.
  (let ([old (window-buffer w)])
    (unless (eq? old b)
      (buffer-spot-row-set! old (window-prow w))
      (buffer-spot-col-set! old (window-pcol w))
      (buffer-spot-top-set! old (window-top w))))
  (window-buffer-set! w b)
  (window-prow-set! w (buffer-spot-row b))
  (window-pcol-set! w (buffer-spot-col b))
  (window-top-set! w (buffer-spot-top b))
  (window-left-set! w 0))

(define (show-buffer! b)
  (set! buffers (cons b (remq b buffers)))   ; most recently used first
  (set-window-buffer! current-window b))

(define (buffer-named name)
  (find (lambda (b) (string=? (buffer-name b) name)) buffers))

(define (switch-buffer-command!)
  (let* ([current (window-buffer current-window)]
         [default (find (lambda (b) (not (eq? b current))) buffers)]
         [s (prompt! (if default
                         (format "Switch to buffer (default ~a): "
                                 (buffer-name default))
                         "Switch to buffer: "))])
    (when s
      (cond [(string=? s "") (when default (show-buffer! default))]
            [(buffer-named s) => show-buffer!]
            [else (show-buffer! (new-buffer s))
                  (set! message (format "New buffer ~a" s))]))))

(define (kill-buffer! b)
  (set! buffers (remq b buffers))
  (when (null? buffers) (set! buffers (list (new-buffer "*scratch*"))))
  (for-each (lambda (w)
              (when (eq? (window-buffer w) b)
                (set-window-buffer! w (car buffers))))
            windows)
  (set! message (format "Killed ~a" (buffer-name b))))

(define (kill-buffer-command!)
  (let* ([current (window-buffer current-window)]
         [s (prompt! (format "Kill buffer (default ~a): "
                             (buffer-name current)))])
    (when s
      (let ([b (if (string=? s "") current (buffer-named s))])
        (cond [(not b) (set! message (format "No buffer named ~a" s))]
              [(or (buffer-clean? b)
                   (confirm? (format "Buffer ~a modified; kill anyway? (yes or no) "
                                     (buffer-name b))))
               (kill-buffer! b)])))))

(define (list-buffers-command!)
  (set! message
    (fold-left (lambda (acc b)
                 (format "~a ~a~a" acc
                         (if (buffer-modified b) "*" "") (buffer-name b)))
               "Buffers:" buffers)))

(define (next-window w)
  (let ([tail (cdr (memq w windows))])
    (if (pair? tail) (car tail) (car windows))))

(define (other-window!)
  (set! current-window (next-window current-window)))

(define (split-window!)
  ;; Stack a new window under the current one, showing the same buffer.
  (let ([n (+ (length windows) 1)])
    (if (< (- rows 1 n) (* 2 n))
        (set! message "Not enough room to split")
        (let ([w (make-window (window-buffer current-window)
                              top-row left-col point-row point-col)])
          (set! windows (insert-after windows current-window w))))))

(define (delete-window!)
  (if (null? (cdr windows))
      (set! message "Only one window")
      (let ([next (next-window current-window)])
        (set! windows (remq current-window windows))
        (set! current-window next))))

(define (delete-other-windows!)
  (set! windows (list current-window)))

;;; Syntax highlighting ---------------------------------------------------

;; Syntax highlighting is provided by extension modules (see ~/.e), which
;; replace this hook.  It receives a line and returns either a vector of
;; per-column style symbols understood by style-code, or #f for an
;; unstyled line.  Brackets styled 'delimiter take part in bracket
;; matching; with no styles at all, every bracket counts.
(define line-styles (lambda (s) #f))

(define (style-code style)
  (case style
    [(comment) "\x1b;[90m"]
    [(string) "\x1b;[32m"]
    [(keyword) "\x1b;[1;36m"]
    [(number) "\x1b;[35m"]
    [(literal) "\x1b;[1;35m"]
    [(delimiter) "\x1b;[33m"]
    [(quote) "\x1b;[36m"]
    [else "\x1b;[0m"]))

;;; Rendering -------------------------------------------------------------

(define (ansi . xs) (for-each (lambda (x) (display x stdout)) xs))
(define (goto r c) (ansi "\x1b;[" (number->string r) ";" (number->string c) "H"))

(define (fit s width)
  (let ([n (string-length s)])
    (if (> n width)
        (substring s 0 width)
        (string-append s (make-string (- width n) #\space)))))

(define (matching-columns s needle)
  ;; Columns covered by a search match, as a boolean vector, or #f when
  ;; there is no active search.
  (and (> (string-length needle) 0)
       (let ([marked (make-vector (string-length s) #f)])
         (let loop ([start 0])
           (let ([found (string-search s needle start (string-length s))])
             (when found
               (vector-fill-range! marked found (+ found (string-length needle)) #t)
               ;; Advance one column so overlapping matches highlight too.
               (loop (+ found 1)))))
         marked)))

(define (region-span row line-length)
  ;; The columns of `row` inside the active region, as (start . end), or #f.
  (and mark-active?
       (let-values ([(sr sc er ec) (ordered-region)])
         (cond [(or (< row sr) (> row er)) #f]
               [(= sr er) (cons sc ec)]
               [(= row sr) (cons sc line-length)]
               [(= row er) (cons 0 ec)]
               [else (cons 0 line-length)]))))

(define (scan-paren start-row start-col dir)
  ;; Find the bracket balancing the one at (start-row, start-col), scanning
  ;; forward (dir 1) or backward (dir -1).  Brackets inside strings and
  ;; comments don't count, per the syntax styles.  The scan is bounded so
  ;; pathological buffers stay responsive; #f when nothing balances.
  (let walk ([row start-row] [col start-col]
             [styles (line-styles (line-at start-row))]
             [depth 0] [budget 50000])
    (and (> budget 0)
         (if (or (< col 0) (>= col (string-length (line-at row))))
             (let ([row (+ row dir)])
               (and (>= row 0) (< row (vlen))
                    (walk row
                          (if (> dir 0) 0 (- (string-length (line-at row)) 1))
                          (line-styles (line-at row))
                          depth (- budget 1))))
             (let* ([c (string-ref (line-at row) col)]
                    [delta (if (or (not styles)
                                   (eq? (vector-ref styles col) 'delimiter))
                               (cond [(memv c '(#\( #\[ #\{)) dir]
                                     [(memv c '(#\) #\] #\})) (- dir)]
                                     [else 0])
                               0)])
               (if (and (not (= delta 0)) (= (+ depth delta) 0))
                   (cons row col)
                   (walk row (+ col dir) styles (+ depth delta) (- budget 1))))))))

(define (paren-highlights)
  ;; The bracket at point and its partner, as a list of (row . col) pairs
  ;; to highlight: the opener point sits on, or the closer just before
  ;; point (as in Emacs's show-paren-mode).  Empty when neither applies.
  (let* ([line (current-line)]
         [styles (line-styles line)])
    (define (bracket-at col kinds)
      (and (>= col 0) (< col (string-length line))
           (memv (string-ref line col) kinds)
           (or (not styles) (eq? (vector-ref styles col) 'delimiter))
           col))
    (let* ([closer (bracket-at (- point-col 1) '(#\) #\] #\}))]
           [opener (and (not closer) (bracket-at point-col '(#\( #\[ #\{)))]
           [col (or closer opener)]
           [match (and col (scan-paren point-row col (if closer -1 1)))])
      (if match (list (cons point-row col) match) '()))))

(define (paren-cols parens row)
  (map cdr (filter (lambda (p) (= (car p) row)) parens)))

(define (display-editor-line s span brackets left)
  (define n (string-length s))
  (define limit (+ left cols))
  (define styles (line-styles s))
  (define marked (matching-columns s search-highlight))
  (define (style-at col)
    (if (and styles (< col n)) (vector-ref styles col) 'plain))
  (define (highlighted? col)
    (and (< col n)
         (or (and marked (vector-ref marked col))
             (and span (<= (car span) col) (< col (cdr span))))))
  (define (segment from to)
    ;; The characters of columns [from, to); control characters (notably
    ;; tabs) and columns past the end of the line become spaces, so every
    ;; column is exactly one cell wide.
    (let ([out (make-string (- to from) #\space)])
      (let loop ([i from])
        (when (and (< i to) (< i n))
          (let ([ch (string-ref s i)])
            (unless (< (char->integer ch) 32)
              (string-set! out (- i from) ch)))
          (loop (+ i 1))))
      out))
  (define (bracket? col) (and (memv col brackets) #t))
  ;; Emit runs of identically-attributed columns as single writes.
  (let loop ([col left])
    (when (< col limit)
      (let* ([style (style-at col)]
             [hi (highlighted? col)]
             [br (bracket? col)]
             [end (let run ([j (+ col 1)])
                    (if (and (< j limit)
                             (eq? (style-at j) style)
                             (eq? (highlighted? j) hi)
                             (eq? (bracket? j) br))
                        (run (+ j 1))
                        j))])
        (ansi "\x1b;[0m" (style-code style))
        (when hi (ansi "\x1b;[7m"))
        (when br (ansi "\x1b;[4m"))
        (ansi (segment col end))
        (loop end))))
  (ansi "\x1b;[0m"))

(define (window-layout)
  ;; Stack the windows top to bottom, each a band of text rows followed by
  ;; its own status line; the last screen row is the echo area.
  ;; -> list of (window start text-height), start 0-based.
  (let* ([n (length windows)]
         [text (- rows 1 n)]
         [base (quotient text n)]
         [extra (remainder text n)])
    (let loop ([ws windows] [start 0] [i 0] [acc '()])
      (if (null? ws)
          (reverse acc)
          (let ([h (+ base (if (< i extra) 1 0))])
            (loop (cdr ws) (+ start h 1) (+ i 1)
                  (cons (list (car ws) start h) acc)))))))

(define (scroll-window! w height)
  ;; Clamp w's point to its buffer (edits in another window may have moved
  ;; the ground under it) and scroll so point stays visible.
  (let* ([v (buffer-lines (window-buffer w))]
         [prow (max 0 (min (window-prow w) (- (vector-length v) 1)))]
         [pcol (max 0 (min (window-pcol w)
                           (string-length (vector-ref v prow))))])
    (window-prow-set! w prow)
    (window-pcol-set! w pcol)
    (when (< prow (window-top w)) (window-top-set! w prow))
    (when (>= prow (+ (window-top w) height))
      (window-top-set! w (- prow height -1)))
    (when (< pcol (window-left w)) (window-left-set! w pcol))
    (when (>= pcol (+ (window-left w) cols))
      (window-left-set! w (- pcol cols -1)))))

;; The cache holds, per screen row, the key describing what that row
;; currently shows; a row is repainted only when its key changes.  Any
;; change of view (size, search highlight, window arrangement) discards
;; the whole cache.
(define screen-cache '#())
(define cached-top-row #f)
(define cached-view #f)

(define (invalidate-screen-cache!) (set! cached-view #f))

(define (shift-screen-cache! delta height)
  ;; Mirror a native one-row terminal scroll in the text rows of the cache.
  (if (= delta 1)
      (begin (let loop ([i 0])
               (when (< i (- height 1))
                 (vector-set! screen-cache i (vector-ref screen-cache (+ i 1)))
                 (loop (+ i 1))))
             (vector-set! screen-cache (- height 1) #f))
      (begin (let loop ([i (- height 1)])
               (when (> i 0)
                 (vector-set! screen-cache i (vector-ref screen-cache (- i 1)))
                 (loop (- i 1))))
             (vector-set! screen-cache 0 #f))))

(define (paint! row key draw)
  ;; Repaint the 0-based screen row unless it already shows key.
  (unless (equal? key (vector-ref screen-cache row))
    (ansi "\x1b;[?25l") (goto (+ row 1) 1)
    (draw)
    (vector-set! screen-cache row key)))

(define (paint-window! w start height parens)
  (let* ([b (window-buffer w)]
         [v (buffer-lines b)]
         [n (vector-length v)]
         [top (window-top w)]
         [left (window-left w)]
         [current? (eq? w current-window)])
    (let loop ([k 0])
      (when (< k height)
        (let ([i (+ top k)] [row (+ start k)])
          (if (< i n)
              (let* ([line (vector-ref v i)]
                     [span (and current? (region-span i (string-length line)))]
                     [brackets (if current? (paren-cols parens i) '())])
                (paint! row (list i line span brackets left)
                        (lambda () (display-editor-line line span brackets left))))
              (paint! row '(empty)
                      (lambda () (ansi (fit "~" cols))))))
        (loop (+ k 1))))
    (let ([status (format " ~a~a  ~a  L~a C~a "
                          (if (buffer-modified b) "**" "--") editor-name
                          (buffer-name b)
                          (+ (window-prow w) 1) (+ (window-pcol w) 1))])
      (paint! (+ start height) (list 'status status current?)
              (lambda ()
                (ansi (if current? "\x1b;[7m" "\x1b;[7;2m")
                      (fit status cols) "\x1b;[0m"))))))

(define (redraw!)
  (terminal-size!)
  ;; A terminal too small for the splits collapses back to one window.
  (when (and (pair? (cdr windows))
             (< (- rows 1 (length windows)) (* 2 (length windows))))
    (set! windows (list current-window)))
  (let* ([layout (window-layout)]
         [single? (null? (cdr windows))]
         [view (list rows cols search-highlight (map cdr layout))]
         [top-delta (if (and single? cached-top-row)
                        (- top-row cached-top-row)
                        0)])
    (for-each (lambda (entry) (scroll-window! (car entry) (caddr entry)))
              layout)
    (cond [(not (equal? view cached-view))
           (set! screen-cache (make-vector rows #f))
           (set! cached-view view)]
          [(and single? (memv (- top-row cached-top-row) '(-1 1)))
           ;; Point scrolled by one row in the sole window: let the terminal
           ;; move the text and repaint only what the scroll uncovered.
           (let ([height (- rows 2)]
                 [delta (- top-row cached-top-row)])
             (ansi "\x1b;[?25l" "\x1b;[1;" (number->string height) "r"
                   (if (= delta 1) "\x1b;[1S" "\x1b;[1T") "\x1b;[r")
             (shift-screen-cache! delta height))])
    (set! cached-top-row (and single? top-row))
    (let ([parens (paren-highlights)])
      (for-each (lambda (entry)
                  (paint-window! (car entry) (cadr entry) (caddr entry) parens))
                layout))
    (paint! (- rows 1) (list 'message message)
            (lambda () (ansi (fit message cols))))
    (let ([entry (assq current-window layout)])
      (goto (+ (cadr entry) (- point-row top-row) 1)
            (+ (- point-col left-col) 1))))
  (ansi "\x1b;[?25h") (flush-output-port stdout))

;;; Prompts and commands --------------------------------------------------

(define (discard-escape-sequence!)
  ;; Consume the remaining bytes of an ESC-initiated key sequence (arrow
  ;; keys and the like) so they are not read back as ordinary characters.
  (let ([a (read-char stdin)])
    (when (and (char? a) (char=? a #\[))
      (let loop ()
        (let ([b (read-char stdin)])
          (when (and (char? b) (or (char<=? #\0 b #\9) (char=? b #\;)))
            (loop)))))))

(define (prompt! label)
  (let loop ([s ""])
    (set! message (string-append label s)) (redraw!)
    (let ([c (read-char stdin)])
      (if (eof-object? c)
          #f
          (case (char->integer c)
            [(27)
             ;; A bare ESC cancels; an escape sequence (arrow key etc.) is
             ;; discarded so its bytes do not leak into the prompt.
             (if (char-ready? stdin)
                 (begin (discard-escape-sequence!) (loop s))
                 (begin (set! message "Quit") #f))]
            [(7) (set! message "Quit") #f]
            [(10 13) s]
            [(8 127)
             (loop (if (string=? s "") s
                       (substring s 0 (- (string-length s) 1))))]
            [else (loop (if (>= (char->integer c) 32)
                            (string-append s (string c))
                            s))])))))

(define (confirm? label)
  (let ([s (prompt! label)]) (and s (string-ci=? s "yes"))))

(define (save-command!)
  (if file-name
      (save-file! file-name)
      (let ([s (prompt! "Write file: ")])
        (when (and s (> (string-length s) 0)) (save-file! s)))))

(define (find-file-command!)
  ;; Visiting a file never loses the old buffer, so no confirmation needed.
  (let ([s (prompt! "Find file: ")])
    (when (and s (> (string-length s) 0)) (visit-file! s))))

(define (quit-command!)
  (when (or (for-all buffer-clean? buffers)
            (confirm? "Modified buffers exist; quit anyway? (yes or no) "))
    (set! quit? #t)))

;;; Incremental search ----------------------------------------------------

(define (search-forward-from needle start-row start-col)
  ;; Search from the supplied position to the end of the buffer, then wrap
  ;; once.  The first pass covers the starting line from start-col onward,
  ;; so the wrap pass covers matches beginning before start-col --
  ;; including ones that straddle it.
  (let loop ([row start-row] [col start-col] [remaining (vlen)])
    (if (= remaining 0)
        (let* ([line (line-at start-row)]
               [found (string-search line needle 0
                                     (min (+ start-col (string-length needle) -1)
                                          (string-length line)))])
          (and found (cons start-row found)))
        (let* ([line (line-at row)]
               [found (string-search line needle col (string-length line))])
          (if found
              (cons row found)
              (loop (modulo (+ row 1) (vlen)) 0 (- remaining 1)))))))

(define (goto-match! match)
  (set! point-row (car match)) (set! point-col (cdr match)))

(define (search-command!)
  (let ([origin (cons point-row point-col)])
    (let loop ([needle ""] [match #f] [failed? #f])
      (set! search-highlight needle)
      (set! message (format "~aI-search: ~a" (if failed? "Failing " "") needle))
      (redraw!)
      (let ([c (read-char stdin)]
            [anchor (or match origin)])
        (if (eof-object? c)
            (set! quit? #t)
            (case (char->integer c)
              ;; RET or ESC accepts the current match.  An escape sequence
              ;; (arrow key etc.) also accepts, then moves point as usual.
              [(10 13 27)
               (set! search-highlight "")
               (set! message (if (string=? needle "") ""
                                 (format "Search: ~a" needle)))
               (when (and (= (char->integer c) 27) (char-ready? stdin))
                 (escape-sequence!))]
              ;; C-g cancels the search and restores point.
              [(7)
               (set! search-highlight "")
               (goto-match! origin)
               (set! message "Quit")]
              ;; C-s repeats the current search from just beyond this match.
              [(19)
               (if (string=? needle "")
                   (loop needle match failed?)
                   (let ([next (search-forward-from needle (car anchor)
                                                    (+ (cdr anchor) 1))])
                     (if next
                         (begin (goto-match! next) (loop needle next #f))
                         (loop needle match #t))))]
              ;; Backspace shortens the needle and searches again from the
              ;; original point.
              [(8 127)
               (if (string=? needle "")
                   (loop needle match failed?)
                   (let ([shorter (substring needle 0 (- (string-length needle) 1))])
                     (if (string=? shorter "")
                         (begin (goto-match! origin) (loop shorter #f #f))
                         (let ([next (search-forward-from shorter (car origin)
                                                          (cdr origin))])
                           (when next (goto-match! next))
                           (loop shorter next (not next))))))]
              [else
               (if (< (char->integer c) 32)
                   (loop needle match failed?)
                   ;; Extend the current match when possible; if it no longer
                   ;; matches, continue forward to the next candidate.
                   (let* ([longer (string-append needle (string c))]
                          [next (search-forward-from longer (car anchor)
                                                     (cdr anchor))])
                     (if next
                         (begin (goto-match! next) (loop longer next #f))
                         (loop longer match #t))))]))))))

;;; Key handling ----------------------------------------------------------

(define (escape-sequence!)
  (let ([a (read-char stdin)])
    (cond
      [(eof-object? a) (void)]
      [(char=? a #\[)
       (case (read-char stdin)
         [(#\A) (move-vertical! -1)] [(#\B) (move-vertical! 1)]
         [(#\C) (move-right!)] [(#\D) (move-left!)]
         [(#\H) (set! point-col 0)]
         [(#\F) (set! point-col (string-length (current-line)))]
         [(#\3) (read-char stdin) (delete-forward!)]
         [(#\5) (read-char stdin) (move-vertical! (- 3 rows))]
         [(#\6) (read-char stdin) (move-vertical! (- rows 3))]
         [else (void)])]
      ;; M-v: page up, M-< and M->: beginning/end of buffer.
      [(char=? a #\v) (move-vertical! (- 3 rows))]
      [(char=? a #\<) (set! point-row 0) (set! point-col 0)]
      [(char=? a #\>) (set! point-row (- (vlen) 1))
                      (set! point-col (string-length (current-line)))])))

(define (handle-prefix! c)
  (set! key-prefix #f)
  (if (eof-object? c)
      (set! quit? #t)
      (case (char->integer c)
        [(19) (save-command!)]                                ; C-x C-s
        [(3) (quit-command!)]                                 ; C-x C-c
        [(6) (find-file-command!)]                            ; C-x C-f
        [(2) (list-buffers-command!)]                         ; C-x C-b
        [(98) (switch-buffer-command!)]                       ; C-x b
        [(107) (kill-buffer-command!)]                        ; C-x k
        [(111) (other-window!)]                               ; C-x o
        [(48) (delete-window!)]                               ; C-x 0
        [(49) (delete-other-windows!)]                        ; C-x 1
        [(50) (split-window!)]                                ; C-x 2
        [else (set! message "C-x is undefined for that key")])))

(define (handle-key! c)
  (when (char? c)
    (let ([n (char->integer c)])
      (unless (= n 11) (set! last-command #f))                ; C-k chains kills
      (unless (memv n '(7 31))                                ; C-g/C-_ keep undo state
        (set! history-direction 'undo)
        (set! last-history-command #f))))
  (cond
    [key-prefix (handle-prefix! c)]
    [(eof-object? c) (set! quit? #t)]
    [else
     (case (char->integer c)
       [(0) (set! mark-row point-row) (set! mark-col point-col) ; C-@ set mark
            (set! mark-active? #t) (set! message "Mark set")]
       [(1) (set! point-col 0)]                                 ; C-a
       [(2) (move-left!)]                                       ; C-b
       [(4) (delete-forward!)]                                  ; C-d
       [(5) (set! point-col (string-length (current-line)))]    ; C-e
       [(6) (move-right!)]                                      ; C-f
       [(7) (set! mark-active? #f) (set! message "Quit")        ; C-g
            (when (eq? last-history-command 'undo)
              (set! history-direction 'redo))]
       [(8 127) (backspace!)]                                   ; C-h, DEL
       [(10 13) (newline!)]                                     ; RET
       [(11) (kill-line!)]                                      ; C-k
       [(12) (set! size-dirty? #t) (invalidate-screen-cache!)   ; C-l
             (set! message "Screen redrawn")]
       [(14) (move-vertical! 1)]                                ; C-n
       [(15) (let ([row point-row] [col point-col])             ; C-o open line
               (newline!)
               (set! point-row row) (set! point-col col))]
       [(16) (move-vertical! -1)]                               ; C-p
       [(19) (search-command!)]                                 ; C-s
       [(22) (move-vertical! (- rows 3))]                       ; C-v
       [(23) (kill-region!)]                                    ; C-w
       [(24) (set! key-prefix 'c-x) (set! message "C-x-")]      ; C-x
       [(25) (yank!)]                                           ; C-y
       [(27) (escape-sequence!)]                                ; ESC
       [(31) (undo-command!)]                                   ; C-_
       [else (when (>= (char->integer c) 32)
               (insert-text! (string c)))])]))

;;; Main ------------------------------------------------------------------

(define (usage)
  (display "Usage: e [file]\n")
  (display "A tiny Emacs-like terminal editor. Set LINES/COLUMNS if needed.\n")
  (display "Extension modules are loaded from ~/.e/*.e at startup.\n"))

(define (load-modules!)
  ;; Load extension modules -- every *.e file in ~/.e, in name order.  A
  ;; module that fails to load reports itself in the message line without
  ;; keeping the editor (or the other modules) from starting.
  (let ([dir (format "~a/.e" (or (getenv "HOME") "."))])
    (when (file-directory? dir)
      (for-each
        (lambda (f)
          (guard (ex [else (set! message (format "Error in ~a: ~a" f (error-text ex)))])
            (load (string-append dir "/" f))))
        (sort string<? (filter (lambda (f) (string-suffix? ".e" f))
                               (directory-list dir)))))))

(define (main)
  (let ([args (command-line-arguments)])
    (when (and (pair? args) (member (car args) '("-h" "--help"))) (usage) (exit 0))
    (when (pair? args) (visit-file! (car args))))
  (load-modules!)
  (unless (and (getenv "TERM") (not (string=? (getenv "TERM") "dumb")))
    (display "e: an interactive terminal is required\n" (current-error-port))
    (exit 1))
  (dynamic-wind
    (lambda () (system "stty raw -echo") (ansi "\x1b;[?1049h\x1b;[2J"))
    (lambda ()
      (let loop ()
        (unless quit?
          (redraw!) (handle-key! (read-char stdin)) (clamp-point!) (loop))))
    (lambda () (ansi "\x1b;[?25h\x1b;[?1049l\x1b;[0m") (flush-output-port stdout)
               (system "stty sane"))))

(main)
