#!/usr/bin/scheme --script
;; e -- a tiny, single-file Emacs-like console editor for Chez Scheme.
;; Run:  ./e [file]   (or: scheme --script e [file])
;;
;; Extension modules (*.e files in ~/.e) are plain Scheme source loaded at
;; startup into the editor's top level, where they can use and replace any
;; of its definitions -- see the line-styles hook for syntax highlighting.

(import (chezscheme))

;;; Editor state ----------------------------------------------------------

(define editor-name "e")
(define lines (vector ""))         ; buffer contents, one string per line
(define point-row 0)
(define point-col 0)
(define top-row 0)                 ; first buffer row shown on screen
(define left-col 0)                ; first buffer column shown on screen
(define file-name #f)
(define trailing-newline? #t)      ; did the file end with a newline?
(define modified? #f)
(define message "C-s search  C-_ undo  C-x C-s save  C-x C-c quit")
(define kill-ring "")
(define last-command #f)
(define mark-row 0)
(define mark-col 0)
(define mark-active? #f)
(define history (vector '() '())) ; undo and redo stacks of snapshots
(define history-direction 'undo)
(define last-history-command #f)
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

(define (adopt-buffer! path new-lines ends? msg)
  (set! lines new-lines)
  (set! trailing-newline? ends?)
  (set! file-name path)
  (set! point-row 0) (set! point-col 0) (set! top-row 0)
  (set! modified? #f) (set! mark-active? #f) (set! goal-pos #f)
  (set! history (vector '() '()))
  (set! history-direction 'undo) (set! last-history-command #f)
  (set! message msg))

(define (load-file! path)
  ;; A failed read leaves the buffer, file name, and undo state untouched.
  (if (file-exists? path)
      (guard (ex [else (set! message (format "Cannot open ~a: ~a" path (error-text ex)))])
        (let* ([content (read-file path)]
               [n (string-length content)]
               [ends? (and (> n 0)
                           (char=? (string-ref content (- n 1)) #\newline))]
               [body (if ends? (substring content 0 (- n 1)) content)])
          (adopt-buffer! path (list->vector (split-lines body)) ends?
                         (format "Loaded ~a" path))))
      (adopt-buffer! path (vector "") #t (format "New file: ~a" path))))

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
    (set! message (format "Wrote ~a" path)) #t))

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

(define (display-editor-line s span brackets)
  (define n (string-length s))
  (define limit (+ left-col cols))
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
  (let loop ([col left-col])
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

(define (scroll!)
  (let ([height (- rows 2)])
    (when (< point-row top-row) (set! top-row point-row))
    (when (>= point-row (+ top-row height)) (set! top-row (- point-row height -1)))
    (when (< point-col left-col) (set! left-col point-col))
    (when (>= point-col (+ left-col cols)) (set! left-col (- point-col cols -1)))))

;; The cache holds, per screen row (text rows, then status, then message),
;; the key describing what that row currently shows; a row is repainted
;; only when its key changes.  Any change of view (size, horizontal scroll,
;; or search highlight) discards the whole cache.
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

(define (redraw!)
  (terminal-size!) (scroll!)
  (let* ([height (- rows 2)]
         [view (list rows cols left-col search-highlight)]
         [top-delta (if cached-top-row (- top-row cached-top-row) 0)])
    (cond [(not (equal? view cached-view))
           (set! screen-cache (make-vector rows #f))
           (set! cached-view view)]
          [(memv top-delta '(-1 1))
           ;; Point scrolled by one row: let the terminal move the text and
           ;; repaint only what the scroll uncovered.
           (ansi "\x1b;[?25l" "\x1b;[1;" (number->string height) "r"
                 (if (= top-delta 1) "\x1b;[1S" "\x1b;[1T") "\x1b;[r")
           (shift-screen-cache! top-delta height)])
    (set! cached-top-row top-row)
    (let ([parens (paren-highlights)])
      (let loop ([screen 0])
        (when (< screen height)
          (let ([i (+ top-row screen)])
            (if (< i (vlen))
                (let* ([line (line-at i)]
                       [span (region-span i (string-length line))]
                       [brackets (paren-cols parens i)])
                  (paint! screen (list i line span brackets)
                          (lambda () (display-editor-line line span brackets))))
                (paint! screen '(empty)
                        (lambda () (ansi (fit "~" cols))))))
          (loop (+ screen 1)))))
    (let ([status (format " ~a~a  ~a  L~a C~a "
                          (if modified? "**" "--") editor-name
                          (or file-name "*scratch*")
                          (+ point-row 1) (+ point-col 1))])
      (paint! height (list 'status status)
              (lambda () (ansi "\x1b;[7m" (fit status cols) "\x1b;[0m"))))
    (paint! (+ height 1) (list 'message message)
            (lambda () (ansi (fit message cols)))))
  (goto (+ (- point-row top-row) 1) (+ (- point-col left-col) 1))
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
  (when (or (not modified?)
            (confirm? "Buffer modified; discard changes? (yes or no) "))
    (let ([s (prompt! "Find file: ")])
      (when (and s (> (string-length s) 0)) (load-file! s)))))

(define (buffer-contents)
  (string-append
    (region-text 0 0 (- (vlen) 1) (string-length (line-at (- (vlen) 1))))
    (if trailing-newline? "\n" "")))

(define (buffer-saved?)
  ;; True when quitting loses nothing: the buffer was never modified, or its
  ;; text is identical to what is on disk again (edits undone or reverted).
  (or (not modified?)
      (if file-name
          (and (file-exists? file-name)
               (guard (ex [else #f])
                 (string=? (buffer-contents) (read-file file-name))))
          (and (= (vlen) 1) (string=? (line-at 0) "")))))

(define (quit-command!)
  (when (or (buffer-saved?) (confirm? "Modified; quit anyway? (yes or no) "))
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
    (when (pair? args) (load-file! (car args))))
  (load-modules!)
  (unless (and (getenv "TERM") (not (string=? (getenv "TERM") "dumb")))
    (display "em: an interactive terminal is required\n" (current-error-port))
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
