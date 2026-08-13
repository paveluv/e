;; core.e -- the e editor's core: the library (core).
;;
;; Imported (and thereby compiled, when stale) by the `e` loader script
;; before the extension modules, which import it in turn.  Internals
;; (including all mutable state) are invisible outside the library and
;; open to compiler optimization; the exports -- the editor's public API,
;; which is all that extension modules and M-x can see -- are immutable.
;; Both guarantees are enforced by the language.

(library (core)
  (export
    ;; state, read-only
    current-buffer buffer-list
    buffer-name buffer-file buffer-text buffer-clean? buffer-modified
    new-buffer buffer-named editor-symbol?
    ;; buffers, windows, files
    visit-file! save-file! save-command! find-file-command!
    show-buffer! kill-buffer! display-buffer! buffer-append!
    set-buffer-mode! set-buffer-read-only!
    switch-buffer-command! kill-buffer-command! list-buffers-command!
    split-window! delete-window! delete-other-windows! other-window!
    ;; editing and movement
    insert-text! newline! delete-forward! backspace!
    kill-line! kill-region! yank! undo-command!
    move-left! move-right! move-vertical!
    search-command! quit-command!
    ;; extending the editor
    bind-key! register-mode! find-mode mode-styles
    prompt! confirm? prompt-ghost completion-highlight
    set-message! echo! redraw! error-text
    call-with-interrupt interrupted?
    vector-fill-range! string-search
    string-tail string-prefix? string-suffix? string-join
    ;; the editor itself
    main)
  ;; The editor defines a few names Chez also exports (the buffer record's
  ;; buffer-mode accessor vs the port option, ...); a library body may not
  ;; shadow an import, so those imports are excluded.  The system-specific
  ;; layer -- libc, termios, signals -- comes from (sys).
  (import (except (chezscheme) buffer-mode) (sys))

  ;; The bindings Chez itself provides, so that the editor's public API
  ;; (and module definitions) can be told apart from builtins -- M-x
  ;; completion highlights them.
  (define baseline-bindings
    (let ([table (make-eq-hashtable)])
      (for-each (lambda (sym) (eq-hashtable-set! table sym #t))
                (environment-symbols (scheme-environment)))
      table))

  (define (editor-symbol? sym)
    (and (top-level-bound? sym)
         (not (eq-hashtable-ref baseline-bindings sym #f))))

  ;;; Buffers and windows ----------------------------------------------------

  (define-record-type buffer
    (fields (mutable name) (mutable lines) (mutable file) (mutable trailing)
            (mutable modified) (mutable history) (mutable hist-dir)
            (mutable hist-last) (mutable mark-row) (mutable mark-col)
            (mutable marked)
            ;; where point was when the buffer was last displayed
            (mutable spot-row) (mutable spot-col) (mutable spot-top)
            (mutable mode) (mutable read-only)))

  (define-record-type window
    (fields (mutable buffer) (mutable top) (mutable left)
            (mutable prow) (mutable pcol)
            ;; the top row last drawn, for native scrolling
            (mutable shown-top)))

  (define (new-buffer name)
    (make-buffer name (vector "") #f #t #f (vector '() '()) 'undo #f
                 0 0 #f 0 0 0 #f #f))

  (define buffers (list (new-buffer "*scratch*")))        ; most recent first
  (define windows (list (make-window (car buffers) 0 0 0 0 #f))) ; top to bottom
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
  ;; The echo area is normally one line; during a prompt it grows with
  ;; the input, wrapping at the right edge Emacs-style with a trailing
  ;; backslash and continuation lines indented to the prompt text, up to
  ;; eight lines, after which it scrolls.  The windows above share what
  ;; remains of the screen.
  (define echo-cursor #f)   ; content index to park the cursor at, or #f
  (define echo-indent #f)   ; prompt continuation indent; #f = no prompt
  (define echo-height 1)
  (define echo-scroll 0)
  (define echo-spans '((0 . 0)))
  (define message-ghost "") ; grey suggestion drawn after the message text
  (define search-highlight "")
  (define rows 24)
  (define cols 80)
  (define stdin (current-input-port))
  (define stdout (current-output-port))

  ;;; Terminal size ---------------------------------------------------------

  ;; The system-specific work (termios, ioctl, SIGWINCH) lives in (sys);
  ;; here only the editor's idea of its size.  Without a terminal, sizes
  ;; fall back to LINES/COLUMNS.

  (define size-dirty? #t)

  ;; C-l also forces a size refresh in case resize events are unavailable.
  (define sigwinch-registered
    (watch-terminal-resize! (lambda () (set! size-dirty? #t))))

  (define (env-number name fallback)
    (let* ([s (getenv name)]
           [n (and s (string->number s))])
      (if (and n (exact? n) (integer? n) (> n 0)) n fallback)))

  (define (terminal-size!)
    (when size-dirty?
      (set! size-dirty? #f)
      (set! rows (max 4 (env-number "LINES" 24)))
      (set! cols (max 20 (env-number "COLUMNS" 80)))
      (let ([size (terminal-size)])
        (when size
          (set! rows (max 4 (car size)))
          (set! cols (max 20 (cdr size)))))))

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

  (define (string-prefix? prefix s)
    (let ([n (string-length s)] [m (string-length prefix)])
      (and (>= n m) (string=? (substring s 0 m) prefix))))

  (define (string-join xs sep)
    (if (null? xs) ""
        (fold-left (lambda (acc x) (string-append acc sep x)) (car xs) (cdr xs))))

  (define (common-prefix strs)
    ;; The longest prefix shared by every string in the non-empty list.
    (fold-left (lambda (acc s)
                 (let loop ([i 0])
                   (if (and (< i (string-length acc)) (< i (string-length s))
                            (char=? (string-ref acc i) (string-ref s i)))
                       (loop (+ i 1))
                       (substring acc 0 i))))
               (car strs) (cdr strs)))

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
    ;; Every editing command passes through here before touching the buffer,
    ;; so this is also where read-only buffers are protected.
    (when (buffer-read-only (window-buffer current-window))
      (error #f "buffer is read-only"))
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

  (define (directory-part path)
    ;; Everything up to and including the last slash, or #f without one.
    (let loop ([i (- (string-length path) 1)])
      (cond [(< i 0) #f]
            [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
            [else (loop (- i 1))])))

  (define (base-name path)
    (let ([dir (directory-part path)])
      (if dir (string-tail path (string-length dir)) path)))

  (define (expand-path path)
    ;; Expand a leading ~ to the home directory.
    (let ([home (getenv "HOME")])
      (cond [(not home) path]
            [(string=? path "~") home]
            [(string-prefix? "~/" path) (string-append home (string-tail path 1))]
            [else path])))

  (define (abbreviate-path path)
    ;; The inverse of expand-path, for display: home becomes ~.
    (let ([home (getenv "HOME")])
      (if (and home (string-prefix? (string-append home "/") path))
          (string-append "~" (string-tail path (string-length home)))
          path)))

  (define (default-directory)
    ;; The directory of the current buffer's file (or the working
    ;; directory), with a trailing slash, abbreviated for display.
    (abbreviate-path
      (or (and file-name (directory-part file-name))
          (string-append (current-directory) "/"))))

  (define (complete-file-name s)
    ;; Completion candidates for the partial path s: the entries of its
    ;; directory whose names extend its final component, as full paths, with
    ;; a trailing slash on directories so completion can descend into them.
    ;; A leading ~ is kept in the candidates but expanded for the lookups.
    ;; Dotfiles are offered only once the component starts with a dot.
    (guard (ex [else '()])
      (let* ([dir (or (directory-part s) "")]
             [part (string-tail s (string-length dir))]
             [listing (directory-list
                        (expand-path
                          (cond [(string=? dir "") "."]
                                [(string=? dir "/") "/"]
                                [else (substring dir 0 (- (string-length dir) 1))])))])
        (map (lambda (name)
               (let ([full (string-append dir name)])
                 (if (file-directory? (expand-path full))
                     (string-append full "/")
                     full)))
             (sort string<?
                   (filter (lambda (name)
                             (and (string-prefix? part name)
                                  (or (not (string=? part ""))
                                      (not (string-prefix? "." name)))))
                           listing))))))

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
            (assign-mode! b)
            (set! message (format "Loaded ~a" path))
            b))
        (let ([b (new-buffer (unique-name (base-name path) #f))])
          (buffer-file-set! b path)
          (assign-mode! b)
          (set! message (format "New file: ~a" path))
          b)))

  (define (visit-file! path)
    ;; Switch to the buffer visiting path, creating it if necessary.
    (let ([path (expand-path path)])
      (cond [(find (lambda (b) (equal? (buffer-file b) path)) buffers)
             => show-buffer!]
            [(file-buffer path) => show-buffer!])))

  (define (save-file! path*)
    (define path (expand-path path*))
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
        (buffer-name-set! b (unique-name (base-name path) b))
        (assign-mode! b))
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

  ;; Read-only views of the editor's state, for M-x and modules; mutation
  ;; goes through the command API.
  (define (current-buffer) (window-buffer current-window))
  (define (buffer-list) (list-copy buffers))
  (define (set-message! s) (set! message s))

  (define (buffer-named name)
    (find (lambda (b) (string=? (buffer-name b) name)) buffers))

  (define (complete-buffer-name s)
    (sort string<? (filter (lambda (n) (string-prefix? s n))
                           (map buffer-name buffers))))

  (define (switch-buffer-command!)
    (let* ([current (window-buffer current-window)]
           [default (find (lambda (b) (not (eq? b current))) buffers)]
           [s (prompt! (if default
                           (format "Switch to buffer (default ~a): "
                                   (buffer-name default))
                           "Switch to buffer: ")
                       complete-buffer-name)])
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
                               (buffer-name current))
                       complete-buffer-name)])
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
      (if (< (- rows echo-height n) (* 2 n))
          (set! message "Not enough room to split")
          (let ([w (make-window (window-buffer current-window)
                                top-row left-col point-row point-col #f)])
            (set! windows (insert-after windows current-window w))))))

  (define (delete-window!)
    (if (null? (cdr windows))
        (set! message "Only one window")
        (let ([next (next-window current-window)])
          (set! windows (remq current-window windows))
          (set! current-window next))))

  (define (delete-other-windows!)
    (set! windows (list current-window)))

  (define (display-buffer! b)
    ;; Show b without leaving the current window: in the window already
    ;; showing it, else the next window, else a fresh split.  The window,
    ;; or #f when the screen has no room for one.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
    (cond
      [(find (lambda (w) (eq? (window-buffer w) b)) windows)]
      [(pair? (cdr windows))
       (let ([w (next-window current-window)])
         (set-window-buffer! w b)
         w)]
      [(>= (- rows 3) 4)
       (let ([w (make-window b 0 0 0 0 #f)])
         (set! windows (insert-after windows current-window w))
         w)]
      [else #f]))

  (define (buffer-append! b . new-lines)
    ;; Append lines to b, transcript style: a fresh buffer's single empty
    ;; line is replaced, and the display follows -- point moves to the
    ;; last line in every window showing b, and in ones that show it later.
    (let ([v (buffer-lines b)]
          [add (list->vector new-lines)])
      (buffer-lines-set! b
        (if (and (= (vector-length v) 1) (string=? (vector-ref v 0) ""))
            add
            (vector-append v add))))
    (let ([last (- (vector-length (buffer-lines b)) 1)])
      (buffer-spot-row-set! b last)
      (buffer-spot-col-set! b 0)
      (for-each (lambda (w)
                  (when (eq? (window-buffer w) b)
                    (window-prow-set! w last)
                    (window-pcol-set! w 0)))
                windows)))

  ;;; Modes -------------------------------------------------------------------

  ;; A mode provides syntax highlighting for the buffers it matches.
  ;; Extension modules call register-mode! with the mode's name,
  ;; the file-name endings it claims, the interpreter names recognized in a
  ;; #! first line (for files without a matching extension), and a styles
  ;; function mapping a line to a vector of per-column style symbols
  ;; understood by style-code, or #f for an unstyled line.  Brackets styled
  ;; 'delimiter take part in bracket matching; in a buffer without a mode
  ;; every bracket counts.

  (define-record-type mode
    (fields name extensions interpreters styles))

  (define modes '())

  (define (register-mode! name extensions interpreters styles)
    (set! modes (cons (make-mode name extensions interpreters styles) modes)))

  (define (detect-mode path first-line)
    ;; The mode for a file: by extension, then by the #! interpreter line.
    (or (and path
             (find (lambda (m)
                     (exists (lambda (ext) (string-suffix? ext path))
                             (mode-extensions m)))
                   modes))
        (and (string-prefix? "#!" first-line)
             (find (lambda (m)
                     (exists (lambda (name)
                               (string-search first-line name 0
                                              (string-length first-line)))
                             (mode-interpreters m)))
                   modes))))

  (define (assign-mode! b)
    (buffer-mode-set! b
      (detect-mode (buffer-file b) (vector-ref (buffer-lines b) 0))))

  (define (find-mode name)
    (find (lambda (m) (string=? (mode-name m) name)) modes))

  (define (set-buffer-mode! b name)
    ;; Give b the registered mode called name (#f for none), regardless of
    ;; its file name -- how transcript buffers get their highlighting.
    (buffer-mode-set! b (and name (find-mode name))))

  (define (set-buffer-read-only! b flag)
    (buffer-read-only-set! b flag))

  (define (buffer-line-styles b)
    ;; The line-styles function of b's mode; unstyled without one.
    (let ([m (buffer-mode b)])
      (if m (mode-styles m) (lambda (s) #f))))

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

  (define (scan-paren start-row start-col dir styles-of)
    ;; Find the bracket balancing the one at (start-row, start-col), scanning
    ;; forward (dir 1) or backward (dir -1).  Brackets inside strings and
    ;; comments don't count, per the syntax styles.  The scan is bounded so
    ;; pathological buffers stay responsive; #f when nothing balances.
    (let walk ([row start-row] [col start-col]
               [styles (styles-of (line-at start-row))]
               [depth 0] [budget 50000])
      (and (> budget 0)
           (if (or (< col 0) (>= col (string-length (line-at row))))
               (let ([row (+ row dir)])
                 (and (>= row 0) (< row (vlen))
                      (walk row
                            (if (> dir 0) 0 (- (string-length (line-at row)) 1))
                            (styles-of (line-at row))
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
    (let* ([styles-of (buffer-line-styles (window-buffer current-window))]
           [line (current-line)]
           [styles (styles-of line)])
      (define (bracket-at col kinds)
        (and (>= col 0) (< col (string-length line))
             (memv (string-ref line col) kinds)
             (or (not styles) (eq? (vector-ref styles col) 'delimiter))
             col))
      (let* ([closer (bracket-at (- point-col 1) '(#\) #\] #\}))]
             [opener (and (not closer) (bracket-at point-col '(#\( #\[ #\{)))]
             [col (or closer opener)]
             [match (and col (scan-paren point-row col (if closer -1 1) styles-of))])
        (if match (list (cons point-row col) match) '()))))

  (define (paren-cols parens row)
    (map cdr (filter (lambda (p) (= (car p) row)) parens)))

  (define (display-editor-line s span brackets left styles)
    (define n (string-length s))
    (define limit (+ left cols))
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
    ;; its own status line; the last echo-height screen rows are the echo
    ;; area.  -> list of (window start text-height), start 0-based.
    (let* ([n (length windows)]
           [text (- rows echo-height n)]
           [base (quotient text n)]
           [extra (remainder text n)])
      (let loop ([ws windows] [start 0] [i 0] [acc '()])
        (if (null? ws)
            (reverse acc)
            (let ([h (+ base (if (< i extra) 1 0))])
              (loop (cdr ws) (+ start h 1) (+ i 1)
                    (cons (list (car ws) start h) acc)))))))

  (define (page-size)
    ;; One page of the current window: its text height less one overlap line.
    (max 1 (- (caddr (assq current-window (window-layout))) 1)))

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
  (define cached-view #f)

  (define (invalidate-screen-cache!) (set! cached-view #f))

  (define (shift-screen-cache! delta start height)
    ;; Mirror a native delta-row terminal scroll in cache rows
    ;; [start, start+height); rows the scroll uncovered become #f.
    (let ([end (+ start height)])
      (if (> delta 0)
          (let loop ([i start])
            (when (< i end)
              (vector-set! screen-cache i
                (and (< (+ i delta) end) (vector-ref screen-cache (+ i delta))))
              (loop (+ i 1))))
          (let loop ([i (- end 1)])
            (when (>= i start)
              (vector-set! screen-cache i
                (and (>= (+ i delta) start) (vector-ref screen-cache (+ i delta))))
              (loop (- i 1)))))))

  (define (native-scroll! w start height)
    ;; When w's top moved since it was last drawn, and mostly the same lines
    ;; remain visible, let the terminal shift them: restrict the scrolling
    ;; region to this window's text rows, scroll, and mirror it in the cache.
    ;; The rows the scroll uncovered then repaint through the usual path.
    (let* ([shown (window-shown-top w)]
           [delta (and shown (- (window-top w) shown))])
      (when (and delta (not (= delta 0)) (< (abs delta) height))
        (ansi "\x1b;[?25l"
              "\x1b;[" (number->string (+ start 1)) ";"
              (number->string (+ start height)) "r"
              (format "\x1b;[~a~a" (abs delta) (if (> delta 0) "S" "T"))
              "\x1b;[r")
        (shift-screen-cache! delta start height))))

  (define (paint! row key draw)
    ;; Repaint the 0-based screen row unless it already shows key.
    (unless (equal? key (vector-ref screen-cache row))
      (ansi "\x1b;[?25l") (goto (+ row 1) 1)
      (draw)
      (vector-set! screen-cache row key)))

  (define (echo-indent-now)
    ;; The continuation indent, kept small enough that lines progress.
    (min (or echo-indent 0) (max 0 (- cols 2))))

  (define (compute-echo-spans len)
    ;; Content index ranges of the echo area's visual lines: the first
    ;; line spans the full width, continuations start at the indent, and
    ;; every wrapped line gives its last column to the wrap mark.
    (let ([indent (echo-indent-now)])
      (let loop ([start 0] [first? #t] [acc '()])
        (let ([avail (if first? cols (- cols indent))])
          (if (<= (- len start) avail)
              (reverse (cons (cons start len) acc))
              (let ([take (- avail 1)])
                (loop (+ start take) #f
                      (cons (cons start (+ start take)) acc))))))))

  (define (echo-position k)
    ;; Visual (line . column) of content index k, per echo-spans.
    (let loop ([spans echo-spans] [line 0])
      (let ([span (car spans)])
        (if (or (null? (cdr spans)) (< k (cdr span)))
            (cons line (+ (if (= line 0) 0 (echo-indent-now))
                          (- k (car span))))
            (loop (cdr spans) (+ line 1))))))

  (define (paint-echo-area!)
    (if (not echo-indent)
        (paint! (- rows 1) (list 'message message message-ghost)
                (lambda ()
                  (let* ([mlen (min (string-length message) cols)]
                         [glen (min (string-length message-ghost)
                                    (- cols mlen))])
                    (ansi (substring message 0 mlen)
                          "\x1b;[90m" (substring message-ghost 0 glen)
                          "\x1b;[0m"
                          (make-string (- cols mlen glen) #\space)))))
        ;; A prompt: paint the visible wrapped lines.
        (let* ([content (string-append message message-ghost)]
               [ghost-at (string-length message)]
               [total (length echo-spans)]
               [indent (echo-indent-now)])
          (let loop ([line echo-scroll] [row (- rows echo-height)])
            (when (< row rows)
              (let* ([span (list-ref echo-spans line)]
                     [start (car span)]
                     [end (min (cdr span) (string-length content))]
                     [end (max end start)]
                     [lead (if (= line 0) 0 indent)]
                     [wrapped? (< line (- total 1))]
                     [cut (min (max (- ghost-at start) 0) (- end start))])
                (paint! row
                        (list 'echo line (substring content start end)
                              cut lead wrapped?)
                        (lambda ()
                          (ansi (make-string lead #\space)
                                (substring content start (+ start cut))
                                "\x1b;[90m"
                                (substring content (+ start cut) end)
                                "\x1b;[0m"
                                (make-string
                                  (max 0 (- cols lead (- end start)
                                            (if wrapped? 1 0)))
                                  #\space)
                                (if wrapped? "\\" "")))))
              (loop (+ line 1) (+ row 1)))))))

  (define (echo! text . ghost)
    ;; Set the echo area (with an optional grey suffix) and paint it right
    ;; away -- usable while the main loop is busy running a command.
    (set! message text)
    (set! message-ghost (if (pair? ghost) (car ghost) ""))
    (paint-echo-area!)
    (flush-output-port stdout))

  (define (paint-window! w start height parens)
    (let* ([b (window-buffer w)]
           [v (buffer-lines b)]
           [n (vector-length v)]
           [top (window-top w)]
           [left (window-left w)]
           [styles-of (buffer-line-styles b)]
           [mode-tag (let ([m (buffer-mode b)]) (and m (mode-name m)))]
           [current? (eq? w current-window)])
      (let loop ([k 0])
        (when (< k height)
          (let ([i (+ top k)] [row (+ start k)])
            (if (< i n)
                (let* ([line (vector-ref v i)]
                       [span (and current? (region-span i (string-length line)))]
                       [brackets (if current? (paren-cols parens i) '())])
                  (paint! row (list i line span brackets left mode-tag)
                          (lambda ()
                            (display-editor-line line span brackets left
                                                 (styles-of line)))))
                (paint! row '(empty)
                        (lambda () (ansi (fit "~" cols))))))
          (loop (+ k 1))))
      (let ([status (format " ~a~a  ~a  L~a C~a~a "
                            (cond [(buffer-read-only b) "%%"]
                                  [(buffer-modified b) "**"]
                                  [else "--"])
                            editor-name
                            (buffer-name b)
                            (+ (window-prow w) 1) (+ (window-pcol w) 1)
                            (if mode-tag (format "  (~a)" mode-tag) ""))])
        (paint! (+ start height) (list 'status status current?)
                (lambda ()
                  (ansi (if current? "\x1b;[7m" "\x1b;[7;2m")
                        (fit status cols) "\x1b;[0m"))))))

  (define (update-echo-geometry!)
    ;; The prompt area's height follows its wrapped text (the grey
    ;; suggestion included), up to eight lines, then scrolls keeping the
    ;; cursor's line visible; without a prompt it is one line.
    (if echo-indent
        (let* ([len (+ (string-length message) (string-length message-ghost))]
               [padded (max len (+ (or echo-cursor 0) 1))])
          (set! echo-spans (compute-echo-spans padded))
          (let* ([total (length echo-spans)]
                 [cap (max 1 (min 8 (- rows 3)))])
            (set! echo-height (min total cap))
            (when echo-cursor
              (let ([line (car (echo-position echo-cursor))])
                (when (< line echo-scroll) (set! echo-scroll line))
                (when (>= line (+ echo-scroll echo-height))
                  (set! echo-scroll (- line (- echo-height 1))))))
            (set! echo-scroll
              (max 0 (min echo-scroll (- total echo-height))))))
        (begin
          (set! echo-spans '((0 . 0)))
          (set! echo-height 1)
          (set! echo-scroll 0))))

  (define (redraw!)
    (terminal-size!)
    (update-echo-geometry!)
    ;; A terminal too small for the splits collapses back to one window.
    (when (and (pair? (cdr windows))
               (< (- rows echo-height (length windows))
                  (* 2 (length windows))))
      (set! windows (list current-window)))
    (let* ([layout (window-layout)]
           [view (list rows cols search-highlight (map cdr layout))])
      (for-each (lambda (entry) (scroll-window! (car entry) (caddr entry)))
                layout)
      (if (not (equal? view cached-view))
          (begin (set! screen-cache (make-vector rows #f))
                 (set! cached-view view))
          (for-each (lambda (entry)
                      (native-scroll! (car entry) (cadr entry) (caddr entry)))
                    layout))
      (let ([parens (paren-highlights)])
        (for-each (lambda (entry)
                    (paint-window! (car entry) (cadr entry) (caddr entry) parens)
                    (window-shown-top-set! (car entry) (window-top (car entry))))
                  layout))
      (paint-echo-area!)
      (if echo-cursor
          (if echo-indent
              (let ([p (echo-position echo-cursor)])
                (goto (+ (- rows echo-height) (- (car p) echo-scroll) 1)
                      (min (+ (cdr p) 1) cols)))
              (goto rows (min (+ echo-cursor 1) cols)))
          (let ([entry (assq current-window layout)])
            (goto (+ (cadr entry) (- point-row top-row) 1)
                  (+ (- point-col left-col) 1)))))
    (ansi "\x1b;[?25h") (flush-output-port stdout))

  ;;; Prompts and commands --------------------------------------------------

  (define (completion-label c)
    ;; A candidate as shown in the completions list: the part after the last
    ;; separator -- a path's last component (with the trailing slash kept on
    ;; directories), an expression's trailing symbol; plain names unchanged.
    (if (string-suffix? "/" c)
        (string-append (base-name (substring c 0 (- (string-length c) 1))) "/")
        (let loop ([i (- (string-length c) 1)])
          (cond [(< i 0) c]
                [(memv (string-ref c i) '(#\/ #\space #\( #\) #\[ #\]))
                 (string-tail c (+ i 1))]
                [else (loop (- i 1))]))))

  (define (format-columns labels width)
    ;; The labels laid out in columns across width, one string per line.
    (let* ([w (+ 2 (fold-left max 0 (map string-length labels)))]
           [ncols (max 1 (quotient width w))])
      (let loop ([xs labels] [acc '()])
        (if (null? xs)
            (reverse acc)
            (let row ([i 0] [xs xs] [line ""])
              (if (or (= i ncols) (null? xs))
                  (loop xs (cons line acc))
                  (row (+ i 1) (cdr xs) (string-append line (fit (car xs) w)))))))))

  ;; The *Completions* pop-up: shown on a TAB that cannot extend the input,
  ;; in the next window when there are several, in a temporary split when
  ;; there is one, and taken down again when the prompt finishes.
  (define completions-buffer #f)
  (define completions-restore #f)

  ;; Prompts may parameterize this to make candidates stand out in the
  ;; pop-up -- M-x highlights the symbols the editor itself defines.
  (define completion-highlight (make-parameter (lambda (label) #f)))

  (define (completions-mode)
    ;; A mode for the pop-up highlighting the labels the current
    ;; completion-highlight predicate selects.
    (let ([highlight? (completion-highlight)])
      (make-mode "completions" '() '()
        (lambda (s)
          (let ([styles (make-vector (string-length s) 'plain)]
                [n (string-length s)])
            (let loop ([i 0])
              (cond [(>= i n) styles]
                    [(char=? (string-ref s i) #\space) (loop (+ i 1))]
                    [else
                     (let ([j (let end ([j i])
                                (if (or (>= j n)
                                        (char=? (string-ref s j) #\space))
                                    j
                                    (end (+ j 1))))])
                       (when (highlight? (substring s i j))
                         (vector-fill-range! styles i j 'keyword))
                       (loop j))])))))))

  (define (show-completions! labels)
    ;; #f when the screen has no room; the caller falls back to a note.
    (let ([content (list->vector (format-columns labels cols))])
      (cond
        [completions-restore                       ; already up: refresh it
         (buffer-lines-set! completions-buffer content)
         (let ([w (find (lambda (w) (eq? (window-buffer w) completions-buffer))
                        windows)])
           (when w
             (window-top-set! w 0)
             (window-prow-set! w 0) (window-pcol-set! w 0)))
         #t]
        [(pair? (cdr windows))                     ; borrow the next window
         (set! completions-buffer (new-buffer "*Completions*"))
         (buffer-read-only-set! completions-buffer #t)
         (buffer-mode-set! completions-buffer (completions-mode))
         (buffer-lines-set! completions-buffer content)
         (let* ([w (next-window current-window)]
                [prev (window-buffer w)])
           (set-window-buffer! w completions-buffer)
           (set! completions-restore (lambda () (set-window-buffer! w prev))))
         #t]
        [(>= (- rows 3) 4)                         ; room for a second window?
         (set! completions-buffer (new-buffer "*Completions*"))
         (buffer-read-only-set! completions-buffer #t)
         (buffer-mode-set! completions-buffer (completions-mode))
         (buffer-lines-set! completions-buffer content)
         (let ([w (make-window completions-buffer 0 0 0 0 #f)])
           (set! windows (insert-after windows current-window w))
           (set! completions-restore (lambda () (set! windows (remq w windows)))))
         #t]
        [else #f])))

  (define (dismiss-completions!)
    (when completions-restore
      (completions-restore)
      (set! completions-restore #f)
      (set! completions-buffer #f)))

  ;; Prompts may parameterize this to suggest what could follow the input --
  ;; M-x uses it to show the pending parameters of the call being typed.
  ;; The suggestion is drawn in grey after the cursor; #f for none.
  (define prompt-ghost (make-parameter (lambda (s) #f)))

  (define (complete! s complete k)
    ;; TAB in a prompt, as in Emacs: extend s to the longest common prefix
    ;; of its completions; when it cannot be extended, pop up the candidate
    ;; list.  k continues the prompt loop as (k new-s note).
    (let ([cands (complete s)])
      (cond
        [(null? cands) (k s " [No match]")]
        [(null? (cdr cands))
         (if (string=? (car cands) s)
             (k s " [Sole completion]")
             (k (car cands) ""))]
        [else
         (let ([lcp (common-prefix cands)])
           (cond [(> (string-length lcp) (string-length s)) (k lcp "")]
                 [(show-completions! (map completion-label cands)) (k s "")]
                 [else (k s (format " {~a}"
                                    (string-join (map completion-label cands)
                                                 " ")))]))])))

  (define (prompt! label . rest)
    ;; Read a line in the echo area, with the cursor parked there.  Optional
    ;; arguments: a completer (string -> list of candidate strings) enabling
    ;; TAB completion, initial input (pre-filled, editable), a history box
    ;; (a list of previous inputs, newest first) navigated with the up and
    ;; down arrows -- accepting an input records it there -- and an
    ;; alternative completer bound to Shift-TAB.  Whichever way the prompt
    ;; ends, the completions pop-up is taken down.
    (define (optional n)
      (let loop ([r rest] [n n])
        (cond [(null? r) #f]
              [(= n 0) (car r)]
              [else (loop (cdr r) (- n 1))])))
    (define complete (optional 0))
    (define initial (or (optional 1) ""))
    (define history (optional 2))
    (define alt-complete (optional 3))
    (define hist-pos -1)   ; -1: editing; 0..: showing that history entry
    (define stash "")      ; the in-progress input while browsing history
    (define (record-history! s)
      (when (and history (> (string-length s) 0))
        (let ([h (unbox history)])
          (unless (and (pair? h) (string=? (car h) s))
            (set-box! history (cons s h))))))
    (define (read-csi)
      ;; The rest of an ESC [ sequence: (parameter-string . final-char).
      (let drain ([b (read-char stdin)] [params '()])
        (if (and (char? b) (or (char<=? #\0 b #\9) (char=? b #\;)))
            (drain (read-char stdin) (cons b params))
            (cons (list->string (reverse params)) b))))
    (define result
      (let loop ([s initial] [pos (string-length initial)] [note ""])
        (define len (string-length s))
        (define (edited new-s new-pos) ; an edit restarts history browsing
          (set! hist-pos -1)
          (loop new-s new-pos ""))
        (define (history-show entry)
          (loop entry (string-length entry) ""))
        (define (history-up)
          (let ([h (if history (unbox history) '())])
            (if (< (+ hist-pos 1) (length h))
                (begin
                  (when (= hist-pos -1) (set! stash s))
                  (set! hist-pos (+ hist-pos 1))
                  (history-show (list-ref h hist-pos)))
                (loop s pos note))))
        (define (history-down)
          (cond [(= hist-pos -1) (loop s pos note)]
                [(= hist-pos 0) (set! hist-pos -1) (history-show stash)]
                [else (set! hist-pos (- hist-pos 1))
                      (history-show (list-ref (unbox history) hist-pos))]))
        (define (vertical-move delta)
          ;; Move the cursor between the prompt's visual lines, keeping
          ;; the column, clamped into the editable input.
          (let* ([p (echo-position echo-cursor)]
                 [target (list-ref echo-spans (+ (car p) delta))]
                 [indent (echo-indent-now)]
                 [col (cdr p)]
                 [k (if (= (+ (car p) delta) 0)
                        (min col (cdr target))
                        (+ (car target) (max 0 (- col indent))))]
                 [k (min k (cdr target))]
                 [new-pos (min (max 0 (- k (string-length label)))
                               (string-length s))])
            (loop s new-pos note)))
        (define (cursor-on-top?)
          (= (car (echo-position echo-cursor)) 0))
        (define (cursor-on-bottom?)
          (= (car (echo-position echo-cursor)) (- (length echo-spans) 1)))
        (set! message (string-append label s note))
        (set! message-ghost
          (if (string=? note "") (or ((prompt-ghost) s) "") ""))
        (set! echo-indent (string-length label))
        (set! echo-cursor (+ (string-length label) pos))
        (redraw!)
        (let ([c (read-char stdin)])
          (if (eof-object? c)
              #f
              (case (char->integer c)
                [(27)
                 ;; A bare ESC cancels.  Escape sequences edit: up and down
                 ;; browse the history, the rest move and delete as usual.
                 (if (char-ready? stdin)
                     (let ([a (read-char stdin)])
                       (if (and (char? a) (char=? a #\[))
                           (let* ([csi (read-csi)]
                                  [params (car csi)]
                                  [final (cdr csi)])
                             (case final
                               ;; Up and down move within a wrapped
                               ;; prompt; history takes over at the edges.
                               [(#\A) (if (cursor-on-top?)
                                          (history-up)
                                          (vertical-move -1))]
                               [(#\B) (if (cursor-on-bottom?)
                                          (history-down)
                                          (vertical-move 1))]
                               [(#\C) (loop s (min len (+ pos 1)) "")]
                               [(#\D) (loop s (max 0 (- pos 1)) "")]
                               [(#\H) (loop s 0 "")]
                               [(#\F) (loop s len "")]
                               [(#\Z)                       ; Shift-TAB
                                (set! hist-pos -1)
                                (if alt-complete
                                    (complete! s alt-complete
                                               (lambda (new-s note)
                                                 (loop new-s
                                                       (string-length new-s)
                                                       note)))
                                    (loop s pos ""))]
                               [(#\~)
                                (cond [(and (string=? params "3") (< pos len))
                                       (edited (string-delete s pos (+ pos 1)) pos)]
                                      [(member params '("1" "7")) (loop s 0 "")]
                                      [(member params '("4" "8")) (loop s len "")]
                                      [else (loop s pos "")])]
                               [else (loop s pos "")]))
                           (loop s pos "")))
                     (begin (set! message "Quit") #f))]
                [(7) (set! message "Quit") #f]
                [(10 13) (record-history! s) (set! message "") s]
                [(1) (loop s 0 "")]                                   ; C-a
                [(2) (loop s (max 0 (- pos 1)) "")]                   ; C-b
                [(5) (loop s len "")]                                 ; C-e
                [(6) (loop s (min len (+ pos 1)) "")]                 ; C-f
                [(4) (if (< pos len)                                  ; C-d
                         (edited (string-delete s pos (+ pos 1)) pos)
                         (loop s pos ""))]
                [(11) (set! kill-ring (string-tail s pos))            ; C-k
                      (edited (substring s 0 pos) pos)]
                [(25) (edited (string-insert s pos kill-ring)         ; C-y
                              (+ pos (string-length kill-ring)))]
                [(9) (set! hist-pos -1)                               ; TAB
                     (if complete
                         (complete! s complete
                                    (lambda (new-s note)
                                      (loop new-s (string-length new-s) note)))
                         (loop s pos ""))]
                [(8 127)
                 (if (= pos 0)
                     (loop s pos "")
                     (edited (string-delete s (- pos 1) pos) (- pos 1)))]
                [else
                 (if (>= (char->integer c) 32)
                     (edited (string-insert s pos (string c)) (+ pos 1))
                     (loop s pos ""))])))))
    (set! echo-cursor #f)
    (set! echo-indent #f)
    (set! echo-scroll 0)
    (set! message-ghost "")
    (dismiss-completions!)
    result)

  (define (confirm? label)
    (let ([s (prompt! label)]) (and s (string-ci=? s "yes"))))

  (define (save-command!)
    (if file-name
        (save-file! file-name)
        (let ([s (prompt! "Write file: " complete-file-name (default-directory))])
          (when (and s (> (string-length s) 0)) (save-file! s)))))

  (define (find-file-command!)
    ;; Visiting a file never loses the old buffer, so no confirmation needed.
    (let ([s (prompt! "Find file: " complete-file-name (default-directory))])
      (when (and s (> (string-length s) 0)) (visit-file! s))))

  (define (quit-command!)
    (when (or (for-all buffer-clean? buffers)
              (confirm? "Modified buffers exist; quit anyway? (yes or no) "))
      (set! quit? #t)))

  ;;; Interruptible execution -------------------------------------------------

  ;; A runaway computation run on the user's behalf (an M-x expression, a
  ;; shell command, ...) would freeze the editor, so for its duration the
  ;; terminal is allowed to turn C-c into SIGINT (isig; outside it the
  ;; editor runs with signals off and C-c is an ordinary key), and SIGINT
  ;; becomes a raised condition, answering #t to interrupted?, that unwinds
  ;; the computation.  Limitation: only running Scheme can be interrupted
  ;; this way -- a blocking foreign call runs to completion.
  (define-condition-type &interrupted &serious make-interrupted interrupted?)

  (define interrupt-generation 0)

  (define (call-with-interrupt busy thunk)
    ;; Run thunk interruptibly.  Once it has run for a whole second, the
    ;; string busy is appended in grey to whatever the echo area shows (a
    ;; watcher thread, so it also fires during blocking foreign calls; the
    ;; generation counter keeps a watcher from outliving its computation).
    (let ([saved (keyboard-interrupt-handler)]
          [gen (+ interrupt-generation 1)])
      (set! interrupt-generation gen)
      (dynamic-wind
        (lambda ()
          (keyboard-interrupt-handler
            (lambda () (raise (make-interrupted))))
          (when (and busy (threaded?))
            (fork-thread
              (lambda ()
                (sleep (make-time 'time-duration 0 1))
                (when (= interrupt-generation gen)
                  (set! message-ghost busy)
                  (paint-echo-area!)
                  (flush-output-port stdout)))))
          (terminal-isig! #t))
        thunk
        (lambda ()
          (set! interrupt-generation (+ interrupt-generation 1))
          (terminal-isig! #f)
          (keyboard-interrupt-handler saved)))))

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

  ;; Keys bound by modules, consulted before the built-in bindings (so a
  ;; module may also rebind a built-in key): plain control keys, keys
  ;; after the C-x prefix, and keys after ESC (meta).
  (define user-keys (make-eqv-hashtable))
  (define user-cx-keys (make-eqv-hashtable))
  (define user-meta-keys (make-eqv-hashtable))

  (define (bind-key! spec command)
    ;; Bind a key to a zero-argument command.  spec is "C-t" for a control
    ;; key, "M-x" for a meta key, or "C-x <key>" where <key> is "C-t" or a
    ;; plain character, as in "C-x e".
    (define (code s)
      (cond [(and (= (string-length s) 3) (string-prefix? "C-" s))
             (bitwise-and (char->integer (string-ref s 2)) 31)]
            [(= (string-length s) 1) (char->integer (string-ref s 0))]
            [else (error 'bind-key! "unrecognized key" s)]))
    (cond
      [(string-prefix? "C-x " spec)
       (hashtable-set! user-cx-keys (code (string-tail spec 4)) command)]
      [(and (= (string-length spec) 3) (string-prefix? "M-" spec))
       (hashtable-set! user-meta-keys
                       (char->integer (string-ref spec 2)) command)]
      [else (hashtable-set! user-keys (code spec) command)]))

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
           [(#\5) (read-char stdin) (move-vertical! (- (page-size)))]
           [(#\6) (read-char stdin) (move-vertical! (page-size))]
           [else (void)])]
        [(hashtable-ref user-meta-keys (char->integer a) #f)
         => (lambda (command) (command))]
        ;; M-v: page up, M-< and M->: beginning/end of buffer.
        [(char=? a #\v) (move-vertical! (- (page-size)))]
        [(char=? a #\<) (set! point-row 0) (set! point-col 0)]
        [(char=? a #\>) (set! point-row (- (vlen) 1))
                        (set! point-col (string-length (current-line)))])))

  (define (handle-prefix! c)
    (set! key-prefix #f)
    (set! message "")               ; take down the C-x- hint
    (cond
      [(eof-object? c) (set! quit? #t)]
      [(hashtable-ref user-cx-keys (char->integer c) #f)
       => (lambda (command) (command))]
      [else
       (case (char->integer c)
          [(7) (set! message "Quit")]                           ; C-x C-g
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
          [else (set! message "C-x is undefined for that key")])]))

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
       (set! message "")            ; messages last until the next key
       (cond
         [(hashtable-ref user-keys (char->integer c) #f)
          => (lambda (command) (command))]
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
         [(22) (move-vertical! (page-size))]                      ; C-v
         [(23) (kill-region!)]                                    ; C-w
         [(24) (set! key-prefix 'c-x) (set! message "C-x-")]      ; C-x
         [(25) (yank!)]                                           ; C-y
         [(27) (escape-sequence!)]                                ; ESC
         [(31) (undo-command!)]                                   ; C-_
         [else (when (>= (char->integer c) 32)
                 (insert-text! (string c)))])])]))

  ;;; Main ------------------------------------------------------------------

  (define (usage)
    (display "Usage: e [file]\n")
    (display "A tiny Emacs-like terminal editor. Set LINES/COLUMNS if needed.\n")
    (display "Extension modules are loaded from the lib directory at startup.\n"))

  (define (main)
    ;; The loader script has compiled and loaded all extension modules by
    ;; the time this runs.
    (let ([args (command-line-arguments)])
      (when (and (pair? args) (member (car args) '("-h" "--help"))) (usage) (exit 0))
      (when (pair? args) (visit-file! (car args))))
    (unless (and (getenv "TERM") (not (string=? (getenv "TERM") "dumb")))
      (display "e: an interactive terminal is required\n" (current-error-port))
      (exit 1))
    ;; A stray SIGINT outside an evaluation must not drop into Chez's break
    ;; prompt underneath the editor's screen.
    (keyboard-interrupt-handler void)
    (dynamic-wind
      (lambda () (terminal-raw!) (ansi "\x1b;[?1049h\x1b;[2J"))
      (lambda ()
        (let loop ()
          (unless quit?
            (redraw!)
            ;; A command that raises (a read-only buffer, a bug in an
            ;; extension module) reports itself instead of killing the
            ;; editor.
            (guard (ex [else (set! message (error-text ex))])
              (handle-key! (read-char stdin)))
            (clamp-point!)
            (loop))))
      (lambda () (ansi "\x1b;[?25h\x1b;[?1049l\x1b;[0m") (flush-output-port stdout)
                 (terminal-restore!))))

  ) ;; library (core)
