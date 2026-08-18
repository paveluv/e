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
    current-buffer buffer-list point mark
    buffer? buffer-name buffer-file buffer-text buffer-clean? buffer-modified
    buffer-read-only buffer-mode-name
    buffer-line buffer-line-count buffer-line-styles
    new-buffer buffer-named editor-symbol?
    (rename (lookup-buffer buffer))   ; buffers print as (buffer "name")
    ;; buffers, windows, files
    visit-file! save-file! save!! find-file!!
    show-buffer! kill-buffer! display-buffer! buffer-append!
    fresh-buffer
    set-buffer-mode! set-buffer-read-only! call-with-buffer
    switch-buffer!! kill-buffer!!
    split-window! delete-window! delete-other-windows! other-window!
    resize-window!
    ;; editing and movement
    insert-text! newline! delete-forward! backspace!
    kill-line! kill-region! yank! undo! redo!
    call-as-one-edit!
    move-horizontal! move-vertical! goto-point!
    search!! quit!!
    ;; extending the editor
    bind-key! register-mode! find-mode mode-styles add-highlighter!
    load-module! reload-module! auto-reload
    prompt! confirm? prompt-ghost completion-highlight
    read-key pending-input? cursor-in-echo
    set-message! current-message echo! redraw! error-text mouse!
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
            (mutable modified) (mutable history)
            (mutable mark-row) (mutable mark-col)
            (mutable marked)
            ;; where point was when the buffer was last displayed
            (mutable spot-row) (mutable spot-col) (mutable spot-top)
            (mutable mode) (mutable read-only)))

  (define-record-type window
    (fields (mutable buffer) (mutable top) (mutable left)
            (mutable prow) (mutable pcol)
            ;; the top row last drawn, for native scrolling
            (mutable shown-top)
            ;; text height in screen lines; the layout keeps the sizes
            ;; tiling the text area, rescaling them when it changes
            (mutable size)))

  (define (new-buffer name)
    (make-buffer name (vector "") #f #t #f (vector '() '())
                 0 0 #f 0 0 0 #f #f))

  (define buffers (list (new-buffer "*scratch*")))        ; most recent first
  (define windows (list (make-window (car buffers) 0 0 0 0 #f 0))) ; top to bottom
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

  ;; Undo entries are labeled with the user-level action that made them
  ;; -- "insert \"hello\"", "(replace-all! \"xx\" \"yy\")" -- and undo
  ;; and redo report the label.  Inside a call-as-one-edit! group, the
  ;; box holds (label . buffers-snapshotted): one entry per buffer the
  ;; group touches, labeled with the group's label (or, lacking one,
  ;; that buffer's first edit's).
  (define edit-group (make-parameter #f))

  (define (push-undo! label)
    (vector-set! history 0 (cons (cons label (editor-snapshot))
                                 (vector-ref history 0)))
    (vector-set! history 1 '()))

  (define (record-edit! label)
    ;; Every editing command passes through here before touching the buffer,
    ;; so this is also where read-only buffers are protected.
    (when (buffer-read-only (window-buffer current-window))
      (error #f "buffer is read-only"))
    (unless (suppress-history)
      (let ([group (edit-group)]
            [b (window-buffer current-window)])
        (cond [(not group) (push-undo! label)]
              [(memq b (cdr (unbox group))) (void)]
              [else (push-undo! (or (car (unbox group)) label))
                    (set-box! group (cons (car (unbox group))
                                          (cons b (cdr (unbox group)))))]))))

  (define (relabel-last-edit! label)
    (let ([h (vector-ref history 0)])
      (when (pair? h) (set-car! (car h) label))))

  (define (call-as-one-edit! label thunk)
    ;; Bundle every edit thunk makes into one labeled undo step per
    ;; buffer it touches -- and none for buffers it does not edit.
    ;; Nested groups defer to the outermost.
    (if (edit-group)
        (thunk)
        (parameterize ([edit-group (box (cons label '()))]) (thunk))))

  (define (elide s width)
    ;; s shortened to about width with an elided middle, for messages.
    (if (<= (string-length s) width)
        s
        (let ([keep (max 4 (quotient (- width 5) 2))])
          (string-append (substring s 0 keep) " ... "
                         (string-tail s (- (string-length s) keep))))))

  (define (history-shift! from to verb)
    ;; The report -- what was undone or redone -- is also returned, so
    ;; M-x (undo!) shows it as its result.
    (set! message
      (if (null? (vector-ref history from))
          (format "No further ~a information" (string-downcase verb))
          (let ([entry (car (vector-ref history from))])
            (vector-set! history from (cdr (vector-ref history from)))
            (vector-set! history to
              (cons (cons (car entry) (editor-snapshot))
                    (vector-ref history to)))
            (restore-snapshot! (cdr entry))
            (elide (if (car entry) (format "~a ~a" verb (car entry)) verb)
                   cols))))
    message)

  (define (undo!) (history-shift! 0 1 "Undo"))

  (define (redo!) (history-shift! 1 0 "Redo"))


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

  (define (move-horizontal! delta)
    ;; Move point delta characters, negative to the left, crossing line
    ;; ends the way repeated single steps do.
    (if (< delta 0)
        (do ([i 0 (- i 1)]) ((= i delta)) (move-left!))
        (do ([i 0 (+ i 1)]) ((= i delta)) (move-right!))))

  (define (goto-point! p)
    ;; Move point straight to (row . col), clamped into the buffer.
    (set! point-row (max 0 (min (car p) (- (vlen) 1))))
    (set! point-col (max 0 (min (cdr p) (string-length (current-line))))))

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
    (record-edit! (format "insert ~s" s))
    (set-line! point-row (string-insert (current-line) point-col s))
    (set! point-col (+ point-col (string-length s)))
    (changed!))

  (define (newline!)
    (record-edit! "newline")
    (let ([s (current-line)])
      (set-line! point-row (substring s 0 point-col))
      (set! lines (vector-splice lines (+ point-row 1) (+ point-row 1)
                                 (list (string-tail s point-col))))
      (set! point-row (+ point-row 1)) (set! point-col 0)
      (changed!)))

  (define (delete-forward!)
    (cond [(< point-col (string-length (current-line)))
           (record-edit!
             (format "delete ~s"
                     (string (string-ref (current-line) point-col))))
           (set-line! point-row
             (string-delete (current-line) point-col (+ point-col 1)))
           (changed!)]
          [(< point-row (- (vlen) 1))
           (record-edit! "delete newline")
           (set-line! point-row
             (string-append (current-line) (line-at (+ point-row 1))))
           (set! lines (vector-splice lines (+ point-row 1) (+ point-row 2) '()))
           (changed!)]))

  (define (backspace!)
    (when (or (> point-col 0) (> point-row 0))
      (record-edit!
        (if (> point-col 0)
            (format "delete ~s"
                    (string (string-ref (current-line) (- point-col 1))))
            "delete newline"))
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
             (let ([text (substring s point-col n)])
               (record-edit! (format "kill ~s" text))
               (kill! text)
               (set-line! point-row (substring s 0 point-col))
               (changed!))]
            [(< point-row (- (vlen) 1))
             (kill! "\n")
             (delete-forward!)])))

  (define (yank!)
    ;; Kill-ring entries can span lines after consecutive C-k commands.  Insert
    ;; newlines as buffer structure rather than embedding them in a line string.
    (unless (string=? kill-ring "")
      (record-edit! (format "yank ~s" kill-ring))
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
              (let ([text (region-text sr sc er ec)])
                (record-edit! (format "kill ~s" text))
                (kill! text)
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
      (set! message (format "Wrote ~a" path))
      (reload-on-save! path)
      #t))

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
  (define (current-message) message)
  (define (point) (cons point-row point-col))
  (define (mark) (and mark-active? (cons mark-row mark-col)))
  (define (buffer-line-count b) (vector-length (buffer-lines b)))
  (define (buffer-line b n) (vector-ref (buffer-lines b) n))

  (define (call-with-buffer b thunk)
    ;; Run thunk with b temporarily the current buffer: in the window
    ;; already showing it when there is one -- point moves where the
    ;; user sees them -- else invisibly in the current window with the
    ;; usual spot saving; the MRU order is untouched either way.
    (cond
      [(eq? b (window-buffer current-window)) (thunk)]
      [(find (lambda (w) (eq? (window-buffer w) b)) windows)
       => (lambda (w)
            (let ([prev current-window])
              (dynamic-wind
                (lambda () (set! current-window w))
                thunk
                (lambda () (set! current-window prev)))))]
      [else
       (let ([old (window-buffer current-window)])
         (dynamic-wind
           (lambda () (set-window-buffer! current-window b))
           thunk
           (lambda () (set-window-buffer! current-window old))))]))

  (define (buffer-named name)
    (find (lambda (b) (string=? (buffer-name b) name)) buffers))

  (define (fresh-buffer name)
    ;; The named tool buffer, emptied for rebuilding -- *describe*,
    ;; *Buffer List*, and their kin.  An existing one is reused: the
    ;; windows showing it keep showing it and display-buffer! finds it
    ;; on screen -- no kill, no second window, no duplication.
    (let ([b (or (buffer-named name) (new-buffer name))])
      (buffer-read-only-set! b #f)
      (buffer-lines-set! b (vector ""))
      (buffer-history-set! b (vector '() '()))
      (buffer-modified-set! b #f)
      (for-each (lambda (w)
                  (when (eq? (window-buffer w) b)
                    (window-top-set! w 0)
                    (window-prow-set! w 0)
                    (window-pcol-set! w 0)))
                windows)
      b))

  ;; A buffer's printed form is the expression that looks it up again, so
  ;; results shown in *eval* can be pasted straight into the next
  ;; expression: (buffer-line-count (buffer "e")).  The lookup is by
  ;; name at evaluation time -- a killed buffer's form reports itself.
  (define (lookup-buffer name)
    (or (buffer-named name) (error 'buffer "no buffer named" name)))

  (define buffer-printing
    (record-writer (record-type-descriptor buffer)
      (lambda (r p wr)
        (display "(buffer " p)
        (wr (buffer-name r) p)
        (display ")" p))))

  (define (complete-buffer-name s)
    (sort string<? (filter (lambda (n) (string-prefix? s n))
                           (map buffer-name buffers))))

  (define (switch-buffer!!)
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

  (define (kill-buffer!!)
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

  (define (next-window w)
    (let ([tail (cdr (memq w windows))])
      (if (pair? tail) (car tail) (car windows))))

  (define (other-window!)
    (set! current-window (next-window current-window)))

  (define (halved-size!)
    ;; Take the lower half of the current window's band for a new
    ;; window (one line goes to its status); #f when too small.
    (let ([h (window-size current-window)])
      (and (>= (- h 1) 4)
           (let ([new (quotient (- h 1) 2)])
             (window-size-set! current-window (- h 1 new))
             new))))

  (define (split-window!)
    ;; Stack a new window under the current one, showing the same
    ;; buffer, in the lower half of its band.
    (cond
      [(halved-size!) =>
       (lambda (new)
         (set! windows
           (insert-after windows current-window
                         (make-window (window-buffer current-window)
                                      top-row left-col
                                      point-row point-col #f new))))]
      [else (set! message "Not enough room to split")]))

  (define (resize-window! delta)
    ;; Grow the current window by delta text lines (negative shrinks),
    ;; trading lines with the window below -- or above, for the lowest.
    (if (null? (cdr windows))
        (set! message "Only one window")
        (let ([tail (memq current-window windows)])
          (transfer-lines! current-window
                           (if (pair? (cdr tail))
                               (cadr tail)
                               (list-ref windows (- (length windows) 2)))
                           delta))))

  (define (transfer-lines! w partner delta)
    ;; Move up to delta text lines from partner to w, both keeping at
    ;; least two.
    (let* ([delta (min delta (- (window-size partner) 2))]
           [delta (max delta (- 2 (window-size w)))])
      (window-size-set! w (+ (window-size w) delta))
      (window-size-set! partner (- (window-size partner) delta))))

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
      [(halved-size!) =>
       (lambda (new)
         (let ([w (make-window b 0 0 0 0 #f new)])
           (set! windows (insert-after windows current-window w))
           w))]
      [else #f]))

  (define (buffer-append! b . new-lines)
    ;; Append lines to b, transcript style: a fresh buffer's single empty
    ;; line is replaced, and the display follows -- point moves to the
    ;; last line in every window showing b, and in ones that show it later.
    ;; A transcript belongs in the buffer list even before it is shown.
    (unless (memq b buffers) (set! buffers (append buffers (list b))))
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

  ;;; Module registries -------------------------------------------------------

  ;; Everything a module registers with the core -- key bindings, modes,
  ;; highlighters, whatever a future hook adds -- goes through a registry
  ;; and is tagged with the module whose init! is running.  Reloading a
  ;; module retracts its entries wholesale before running its init!
  ;; afresh, so registration is replace-by-module by construction: a new
  ;; hook gets it by using make-registry, with nothing to remember.
  ;; Entries registered outside any module (M-x, say) have owner #f and
  ;; survive reloads.  Lookups prefer newer entries.

  (define registering-module (make-parameter #f))
  (define registries '())

  (define (make-registry)
    (let ([r (box '())])            ; entries (owner . item), newest first
      (set! registries (cons r registries))
      r))

  (define (registry-add! r item)
    (set-box! r (cons (cons (registering-module) item) (unbox r))))

  (define (registry-items r) (map cdr (unbox r)))

  (define (registry-find r match?)
    (let loop ([entries (unbox r)])
      (cond [(null? entries) #f]
            [(match? (cdar entries)) (cdar entries)]
            [else (loop (cdr entries))])))

  (define (retract-module! owner)
    (for-each (lambda (r)
                (set-box! r (remp (lambda (e) (eq? (car e) owner))
                                  (unbox r))))
              registries))

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

  (define modes (make-registry))

  (define (register-mode! name extensions interpreters styles)
    (registry-add! modes (make-mode name extensions interpreters styles)))

  (define (detect-mode path first-line)
    ;; The mode for a file: by extension, then by the #! interpreter line.
    (or (and path
             (registry-find modes
               (lambda (m)
                 (exists (lambda (ext) (string-suffix? ext path))
                         (mode-extensions m)))))
        (and (string-prefix? "#!" first-line)
             (registry-find modes
               (lambda (m)
                 (exists (lambda (name)
                           (string-search first-line name 0
                                          (string-length first-line)))
                         (mode-interpreters m)))))))

  (define (assign-mode! b)
    (buffer-mode-set! b
      (detect-mode (buffer-file b) (vector-ref (buffer-lines b) 0))))

  (define (find-mode name)
    (registry-find modes (lambda (m) (string=? (mode-name m) name))))

  (define (set-buffer-mode! b name)
    ;; Give b the registered mode called name (#f for none), regardless of
    ;; its file name -- how transcript buffers get their highlighting.
    (buffer-mode-set! b (and name (find-mode name))))

  (define (set-buffer-read-only! b flag)
    (buffer-read-only-set! b flag))

  (define (buffer-mode-name b)
    ;; The name of b's mode, or #f without one.
    (let ([m (buffer-mode b)]) (and m (mode-name m))))

  (define (no-styles s) #f)

  ;; Computed styles, memoized per line string.  Edits replace line
  ;; strings (never mutate them), so string identity keys the cache and
  ;; can never go stale; weak keys keep it bounded by the live lines.
  ;; Each entry remembers its mode, in case an identical string is shared
  ;; between buffers of different modes.
  (define style-cache (make-weak-eq-hashtable))

  (define (buffer-line-styles b)
    ;; The line-styles function of b's mode; unstyled without one.
    (let ([m (buffer-mode b)])
      (if m
          (lambda (s)
            (let ([hit (eq-hashtable-ref style-cache s #f)])
              (if (and hit (eq? (car hit) m))
                  (cdr hit)
                  ;; a raising mode styles the line plain rather than
                  ;; taking the redraw (and the editor) down
                  (let ([styles (guard (ex [else #f])
                                  ((mode-styles m) s))])
                    (eq-hashtable-set! style-cache s (cons m styles))
                    styles))))
          no-styles)))

  (define (style-code style)
    (case style
      [(comment) "\x1b;[90m"]
      [(string) "\x1b;[32m"]
      [(keyword) "\x1b;[1;36m"]
      [(number) "\x1b;[35m"]
      [(literal) "\x1b;[1;35m"]
      [(delimiter) "\x1b;[33m"]
      [(quote) "\x1b;[36m"]
      [(bold) "\x1b;[1m"]      ; real face attributes: terminals without
      [(italic) "\x1b;[3m"]    ; them simply show plain text
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
    ;; there is no active search or no match on this line.
    (and (> (string-length needle) 0)
         (let ([first (string-search s needle 0 (string-length s))])
           (and first
                (let ([marked (make-vector (string-length s) #f)])
                  (let loop ([found first])
                    (when found
                      (vector-fill-range! marked found
                        (+ found (string-length needle)) #t)
                      ;; Advance one column so overlapping matches highlight too.
                      (loop (string-search s needle (+ found 1)
                                           (string-length s)))))
                  marked)))))

  (define (region-span row line-length)
    ;; The columns of `row` inside the active region, as (start . end), or #f.
    (and mark-active?
         (let-values ([(sr sc er ec) (ordered-region)])
           (cond [(or (< row sr) (> row er)) #f]
                 [(= sr er) (cons sc ec)]
                 [(= row sr) (cons sc line-length)]
                 [(= row er) (cons 0 ec)]
                 [else (cons 0 line-length)]))))

  ;; Context highlighting is provided by modules: a highlighter, registered
  ;; with add-highlighter!, is called at every redraw and returns ranges of
  ;; the current buffer to mark up -- a list of (row start end) triples,
  ;; drawn underlined on top of the syntax styles.  The paren module
  ;; matches brackets this way; a broken highlighter is ignored for that
  ;; redraw rather than taking the editor down.
  (define highlighters (make-registry))

  (define (add-highlighter! proc)
    (registry-add! highlighters proc))

  (define (highlight-ranges)
    (fold-left (lambda (acc h) (append (guard (ex [else '()]) (h)) acc))
               '() (registry-items highlighters)))

  (define (ranges-on-row ranges row)
    (fold-left (lambda (acc r)
                 (if (= (car r) row) (cons (cdr r) acc) acc))
               '() ranges))

  (define (display-editor-line s span marks left styles)
    (define n (string-length s))
    (define limit (+ left cols))
    (define marked (matching-columns s search-highlight))
    (define (style-at col)
      (if (and styles (< col n)) (vector-ref styles col) 'plain))
    (define (search-hit? col)
      (and (< col n) marked (vector-ref marked col)))
    (define (selected? col)
      (and (< col n) span (<= (car span) col) (< col (cdr span))))
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
    (define (marked? col)
      (exists (lambda (m) (and (<= (car m) col) (< col (cadr m)))) marks))
    ;; Emit runs of identically-attributed columns as single writes.
    (let loop ([col left])
      (when (< col limit)
        (let* ([style (style-at col)]
               [srch (search-hit? col)]
               [sel (selected? col)]
               [mk (marked? col)]
               [end (let run ([j (+ col 1)])
                      (if (and (< j limit)
                               (eq? (style-at j) style)
                               (eq? (search-hit? j) srch)
                               (eq? (selected? j) sel)
                               (eq? (marked? j) mk))
                          (run (+ j 1))
                          j))])
          (ansi "\x1b;[0m" (style-code style))
          (when sel (ansi "\x1b;[44m"))     ; the selection: blue backdrop
          (when srch (ansi "\x1b;[7m"))     ; search matches: reverse video
          (when mk (ansi "\x1b;[4m"))
          (ansi (segment col end))
          (loop end))))
    (ansi "\x1b;[0m"))

  (define (window-layout)
    ;; Stack the windows top to bottom, each a band of text rows followed by
    ;; its own status line; the last echo-height screen rows are the echo
    ;; area.  Each window's stored size is honored; when the sizes no
    ;; longer tile the text area (a terminal resize, prompt growth, a
    ;; window added or removed) they are rescaled proportionally --
    ;; evenly when there is nothing valid to scale.
    ;; -> list of (window start text-height), start 0-based.
    (let* ([n (length windows)]
           [text (- rows echo-height n)]
           [sum (fold-left + 0 (map window-size windows))])
      (unless (= sum text)
        (if (or (<= sum 0)
                (exists (lambda (w) (< (window-size w) 2)) windows))
            (let ([base (quotient text n)] [extra (remainder text n)])
              (let loop ([ws windows] [i 0])
                (unless (null? ws)
                  (window-size-set! (car ws) (+ base (if (< i extra) 1 0)))
                  (loop (cdr ws) (+ i 1)))))
            (let loop ([ws windows] [left text])
              (if (null? (cdr ws))
                  (window-size-set! (car ws) (max 2 left))
                  (let ([h (max 2 (quotient (* (window-size (car ws)) text)
                                            sum))])
                    (window-size-set! (car ws) h)
                    (loop (cdr ws) (- left h)))))))
      (let loop ([ws windows] [start 0] [acc '()])
        (if (null? ws)
            (reverse acc)
            (let ([h (window-size (car ws))])
              (loop (cdr ws) (+ start h 1)
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
  (define cursor-style-shown "\x1b;[0 q")   ; DECSCUSR last emitted

  (define (invalidate-screen-cache!) (set! cached-view #f))

  (define (erase-screen!)
    ;; Blank the terminal and schedule the full repaint -- an actual
    ;; erase, which also clears the terminal's own selection highlight
    ;; where an identical overwrite would not.
    (ansi "\x1b;[2J")
    (invalidate-screen-cache!))

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
    ;; The continuation indent, capped at half the width so a prompt
    ;; whose label alone overflows the screen still wraps usefully.
    (min (or echo-indent 0) (quotient cols 2)))

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

  ;; Parameterized on (by eval, around an evaluation), the cursor parks
  ;; at the end of the echo area's content -- and is drawn as a blinking
  ;; underline, so a running evaluation is visible at a glance.
  (define cursor-in-echo (make-parameter #f))

  (define (echo-cursor-now)
    (or echo-cursor
        (and (cursor-in-echo)
             (+ (string-length message) (string-length message-ghost)))))

  (define (paint-echo-area!)
    ;; Paint the visible (wrapped) echo lines.  Recompute the geometry
    ;; first: echo! and the busy-message watcher come here directly,
    ;; with the content just changed (from redraw! it is a no-op).
    (update-echo-geometry!)
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
          (loop (+ line 1) (+ row 1))))))

  (define (echo! text . ghost)
    ;; Set the echo area (with an optional grey suffix) and paint it right
    ;; away -- usable while the main loop is busy running a command.
    (set! message text)
    (set! message-ghost (if (pair? ghost) (car ghost) ""))
    (paint-echo-area!)
    (flush-output-port stdout))

  (define (paint-window! w start height ranges)
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
                       [marks (if current? (ranges-on-row ranges i) '())])
                  (paint! row (list i line span marks left mode-tag)
                          (lambda ()
                            (display-editor-line line span marks left
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
    ;; The echo area's height follows its wrapped content (the grey
    ;; suggestion included): prompt input wraps with continuations
    ;; indented to the prompt text, and a plain message that overflows
    ;; the width wraps the same way at indent zero, temporarily
    ;; borrowing rows.  Up to eight lines, after which it scrolls,
    ;; keeping the prompt cursor's line visible.
    (let* ([len (+ (string-length message) (string-length message-ghost))]
           [cursor (echo-cursor-now)]
           [padded (max len (if cursor (+ cursor 1) 1))])
      (set! echo-spans (compute-echo-spans padded))
      (let* ([total (length echo-spans)]
             [cap (max 1 (min 8 (- rows 3)))])
        (set! echo-height (min total cap))
        (when cursor
          (let ([line (car (echo-position cursor))])
            (when (< line echo-scroll) (set! echo-scroll line))
            (when (>= line (+ echo-scroll echo-height))
              (set! echo-scroll (- line (- echo-height 1))))))
        (set! echo-scroll
          (max 0 (min echo-scroll (- total echo-height)))))))

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
      (let ([ranges (highlight-ranges)])
        (for-each (lambda (entry)
                    (paint-window! (car entry) (cadr entry) (caddr entry) ranges)
                    (window-shown-top-set! (car entry) (window-top (car entry))))
                  layout))
      (paint-echo-area!))
    (place-cursor!))

  (define (place-cursor!)
    ;; Park the cursor in the echo area (a prompt, or a running
    ;; evaluation -- the latter drawn as a blinking underline), else
    ;; put it at point in the current window.  Also called on its own
    ;; when an interaction is about to wait for a key, so its cursor
    ;; rules take effect without a repaint.
    (let ([cursor (echo-cursor-now)])
      (if cursor
          (let ([p (echo-position cursor)])
            (goto (+ (- rows echo-height) (- (car p) echo-scroll) 1)
                  (min (+ (cdr p) 1) cols)))
          (let ([entry (assq current-window (window-layout))])
            (goto (+ (cadr entry) (- point-row top-row) 1)
                  (+ (- point-col left-col) 1)))))
    (let ([style (if (cursor-in-echo) "\x1b;[3 q" "\x1b;[0 q")])
      (unless (string=? style cursor-style-shown)
        (set! cursor-style-shown style)
        (ansi style)))
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
        [(halved-size!) =>                         ; room for a second window?
         (lambda (new)
           (set! completions-buffer (new-buffer "*Completions*"))
           (buffer-read-only-set! completions-buffer #t)
           (buffer-mode-set! completions-buffer (completions-mode))
           (buffer-lines-set! completions-buffer content)
           (let ([w (make-window completions-buffer 0 0 0 0 #f new)])
             (set! windows (insert-after windows current-window w))
             (set! completions-restore
               (lambda () (set! windows (remq w windows)))))
           #t)]
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
    (define (run-prompt)
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
                               [(#\<)
                                ;; a mouse report: swallow it
                                (let mouse ([c (read-char stdin)])
                                  (if (and (char? c)
                                           (not (memv c '(#\M #\m))))
                                      (mouse (read-char stdin))
                                      (loop s pos note)))]
                               [(#\~)
                                (cond [(and (string=? params "3") (< pos len))
                                       (edited (string-delete s pos (+ pos 1)) pos)]
                                      [(member params '("1" "7")) (loop s 0 "")]
                                      [(member params '("4" "8")) (loop s len "")]
                                      [(string=? params "200")
                                       ;; A paste, inserted whole; its line
                                       ;; breaks become spaces.
                                       (let ([text (string-join
                                                     (split-pasted-lines
                                                       (read-paste))
                                                     " ")])
                                         (edited (string-insert s pos text)
                                                 (+ pos (string-length text))))]
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
    ;; The prompt owns C-g while it runs, and its echo-area state is
    ;; restored however it exits -- an error unwinding through it
    ;; included.
    (call-uninterrupted
      (lambda ()
        (dynamic-wind
          void
          run-prompt
          (lambda ()
            (set! echo-cursor #f)
            (set! echo-indent #f)
            (set! echo-scroll 0)
            (set! message-ghost "")
            (dismiss-completions!))))))

  (define (confirm? label)
    (let ([s (prompt! label)]) (and s (string-ci=? s "yes"))))

  ;; Key-at-a-time input, for modules building interactive commands
  ;; (single-key queries, search-like loops): the next key as a
  ;; character (#f at end of input), and whether one is already waiting.
  (define (read-key)
    (call-uninterrupted
      (lambda ()
        ;; The caller's redraw may have parked the cursor for a running
        ;; evaluation; waiting for a key is interaction, so re-place it.
        (place-cursor!)
        (let ([c (read-char stdin)]) (and (char? c) c)))))

  (define (pending-input?) (char-ready? stdin))

  (define (save!!)
    (if file-name
        (save-file! file-name)
        (let ([s (prompt! "Write file: " complete-file-name (default-directory))])
          (when (and s (> (string-length s) 0)) (save-file! s))))
    (void))

  (define (find-file!!)
    ;; Visiting a file never loses the old buffer, so no confirmation needed.
    (let ([s (prompt! "Find file: " complete-file-name (default-directory))])
      (when (and s (> (string-length s) 0)) (visit-file! s))))

  (define (quit!!)
    (when (or (for-all buffer-clean? buffers)
              (confirm? "Modified buffers exist; quit anyway? (yes or no) "))
      (set! quit? #t)))

  ;;; Interruptible execution -------------------------------------------------

  ;; A runaway computation run on the user's behalf (an M-x expression, a
  ;; shell command, ...) would freeze the editor, so for its duration the
  ;; terminal turns C-g into SIGINT (outside it the editor runs with
  ;; signals off), and SIGINT becomes a raised condition, answering #t to
  ;; interrupted?, that unwinds the computation -- C-g aborts an
  ;; evaluation just as it cancels a prompt.  Limitation: only running
  ;; Scheme can be interrupted this way -- a blocking foreign call runs
  ;; to completion.
  (define-condition-type &interrupted &serious make-interrupted interrupted?)

  ;; Interaction owns C-g; interruption applies to computation.  While
  ;; the editor waits for the user -- a prompt, a key query, a search --
  ;; isig is off and C-g arrives as an ordinary key the interaction
  ;; handles, so a command cancels the same way however it was invoked;
  ;; between interactions an evaluation is interruptible.
  (define isig-on? #f)

  (define (set-isig! on)
    (unless (eq? on isig-on?)
      (set! isig-on? on)
      (terminal-isig! on)))

  (define (call-uninterrupted thunk)
    ;; Interaction also owns the cursor: while it runs, the cursor
    ;; follows the interaction's rules, not a parked evaluation's.
    (let ([old isig-on?])
      (dynamic-wind
        (lambda () (set-isig! #f))
        (lambda () (parameterize ([cursor-in-echo #f]) (thunk)))
        (lambda () (set-isig! old)))))

  (define (call-with-interrupt thunk)
    ;; Run thunk interruptibly by C-g.
    (let ([saved (keyboard-interrupt-handler)]
          [old isig-on?])
      (dynamic-wind
        (lambda ()
          (keyboard-interrupt-handler
            (lambda () (raise (make-interrupted))))
          (set-isig! #t))
        thunk
        (lambda ()
          (set-isig! old)
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

  (define (goto-match-end! match needle)
    ;; Point lands right after the match, so accepting the search
    ;; leaves it there -- a region set before searching then covers
    ;; the found text.
    (set! point-row (car match))
    (set! point-col (+ (cdr match) (string-length needle))))

  (define (search!!)
    ;; The search owns C-g while it runs; the match highlighting goes
    ;; away however it exits.
    (call-uninterrupted
      (lambda ()
        (dynamic-wind
          void
          run-search!
          (lambda () (set! search-highlight ""))))))

  (define (run-search!)
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
                ;; RET or ESC accepts the current match, silently.  An escape
                ;; sequence (arrow key etc.) also accepts, then moves point.
                [(10 13 27)
                 (set! search-highlight "")
                 (set! message "")
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
                           (begin (goto-match-end! next needle)
                                  (loop needle next #f))
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
                             (when next (goto-match-end! next shorter))
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
                           (begin (goto-match-end! next longer)
                                  (loop longer next #f))
                           (loop longer match #t))))]))))))

  ;;; Pasting and typed runs --------------------------------------------------

  (define (split-pasted-lines s)
    ;; Pasted text split at newlines, whichever convention the terminal
    ;; delivered: \n, \r\n, or bare \r.
    (let ([n (string-length s)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond [(= i n) (reverse (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\newline)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
              [(char=? (string-ref s i) #\return)
               (let ([next (if (and (< (+ i 1) n)
                                    (char=? (string-ref s (+ i 1)) #\newline))
                               (+ i 2) (+ i 1))])
                 (loop next next (cons (substring s start i) acc)))]
              [else (loop (+ i 1) start acc)]))))

  (define (read-paste)
    ;; The text of a bracketed paste: everything up to ESC [ 2 0 1 ~.
    (let loop ([acc '()])
      (let ([c (read-char stdin)])
        (cond [(eof-object? c) (list->string (reverse acc))]
              [(char=? c #\esc)
               (do ([i 0 (+ i 1)]) ((= i 5)) (read-char stdin))
               (list->string (reverse acc))]
              [else (loop (cons c acc))]))))

  (define (paste-into-buffer!)
    ;; A bracketed paste: the whole text becomes one labeled edit, its
    ;; newlines becoming real line breaks.
    (let ([text (read-paste)])
      (unless (string=? text "")
        (call-as-one-edit! (format "insert ~s" text)
          (lambda ()
            (let ([parts (split-pasted-lines text)])
              (insert-text! (car parts))
              (for-each (lambda (part) (newline!) (insert-text! part))
                        (cdr parts))))))))

  ;; Consecutive typed characters coalesce into one undo entry (up to
  ;; twenty, as in Emacs), so undo removes the run, not one character.
  ;; The chain is (buffer row col run-length text): where the next typed
  ;; character must land to continue the run.  Any other key breaks it.
  (define insert-chain #f)

  (define (self-insert! ch chain)
    (let ([b (window-buffer current-window)]
          [s (string ch)])
      (if (and chain
               (eq? (car chain) b)
               (= (cadr chain) point-row)
               (= (caddr chain) point-col)
               (< (cadddr chain) 20))
          (let ([text (string-append (list-ref chain 4) s)])
            (parameterize ([suppress-history #t]) (insert-text! s))
            (relabel-last-edit! (format "insert ~s" text))
            (set! insert-chain
              (list b point-row point-col (+ (cadddr chain) 1) text)))
          (begin
            (insert-text! s)
            (set! insert-chain (list b point-row point-col 1 s))))))

  ;;; Mouse -------------------------------------------------------------------

  ;; SGR mouse tracking: clicks focus the window under the pointer and
  ;; place point at the clicked cell, dragging selects as though the
  ;; mark were set at the press (C-Space) and point moved, and the
  ;; wheel scrolls the window under the pointer, wherever the focus is.
  ;; The cost is the terminal's native mouse selection -- hold Shift
  ;; for that -- so mouse! turns the whole thing on or off at run time.
  (define mouse-on? #f)

  (define (set-mouse! on)
    (set! mouse-on? on)
    (ansi (if on "\x1b;[?1002;1006h" "\x1b;[?1002;1006l"))
    (flush-output-port stdout))

  (define (mouse! on)
    ;; Turn mouse tracking on or off (off restores native selection).
    (set-mouse! on)
    (set! message (format "Mouse ~a" (if on "on" "off")))
    (void))

  (define (window-under-row r0 receiver)
    ;; Call receiver with the layout entry containing 0-based screen
    ;; row r0 (text rows or the status line); #f when r0 is echo area.
    (let loop ([entries (window-layout)])
      (cond [(null? entries) #f]
            [(<= (cadr (car entries)) r0
                 (+ (cadr (car entries)) (caddr (car entries))))
             (receiver (car entries))]
            [else (loop (cdr entries))])))

  (define last-press #f)   ; (x y ms) of the previous button press

  (define (word-char? c)
    (not (or (char-whitespace? c)
             (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\, #\.)))))

  (define (select-word!)
    ;; Select the word point is on (or just after): mark at its start,
    ;; point at its end.
    (let* ([s (current-line)]
           [n (string-length s)]
           [on? (lambda (i)
                  (and (>= i 0) (< i n) (word-char? (string-ref s i))))]
           [col (cond [(on? point-col) point-col]
                      [(on? (- point-col 1)) (- point-col 1)]
                      [else #f])])
      (when col
        (set! mark-row point-row)
        (set! mark-col (let back ([i col])
                         (if (on? (- i 1)) (back (- i 1)) i)))
        (set! point-col (let fwd ([i col])
                          (if (on? i) (fwd (+ i 1)) i)))
        (set! mark-active? #t))))

  ;; The window whose status bar is being dragged to resize it, or #f.
  (define drag-status #f)

  (define (mouse-press! x y)
    ;; Focus the window at 1-based screen position (x, y).  A press in
    ;; its text area also places point at the clicked cell and arms the
    ;; mark there -- dragging activates it, a motionless click does not;
    ;; a second press on the same cell within half a second is a double
    ;; click, selecting the word there.  A press on a status bar (other
    ;; than the lowest) arms a resize drag instead.
    ;; The terminal's own Shift-selection highlight is not touched here
    ;; (erasing on every press flickers); C-l clears it.
    (set! drag-status #f)
    (let ([prev last-press]
          [now (real-time)])
      (set! last-press (list x y now))
      (window-under-row (- y 1)
        (lambda (entry)
          (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
            (set! current-window w)
            (cond
              [(= (- y 1) (+ start height))        ; the status bar
               (when (pair? (cdr (memq w windows)))
                 (set! drag-status w))]
              [else                                ; a text row
               (goto-point! (cons (+ (window-top w) (- y 1 start))
                                  (+ (window-left w) (- x 1))))
               (set! mark-row point-row)
               (set! mark-col point-col)
               (set! mark-active? #f)
               (when (and prev
                          (= (car prev) x) (= (cadr prev) y)
                          (< (- now (caddr prev)) 450))
                 (select-word!))]))))))

  (define (mouse-drag! x y)
    ;; A status-bar drag resizes; otherwise extend the selection armed
    ;; by the press -- the mark activates and point follows the pointer
    ;; within the focused window's text area.
    (if drag-status
        (let ([entry (assq drag-status (window-layout))])
          (when entry
            (let ([delta (- (- y 1) (cadr entry) (caddr entry))])
              (unless (= delta 0)
                (transfer-lines! drag-status
                                 (cadr (memq drag-status windows))
                                 delta)))))
        (window-under-row (- y 1)
          (lambda (entry)
            (let ([w (car entry)] [start (cadr entry)] [height (caddr entry)])
              (when (and (eq? w current-window)
                         (< (- y 1) (+ start height)))
                (set! mark-active? #t)
                (goto-point! (cons (+ (window-top w) (- y 1 start))
                                   (+ (window-left w) (- x 1))))))))))

  (define (mouse-wheel! y mover)
    ;; Scroll the window under the pointer by moving its point (redraw
    ;; scrolls it to follow); the focused window stays focused.
    (window-under-row (- y 1)
      (lambda (entry)
        (let ([old current-window])
          (set! current-window (car entry))
          (mover)
          (set! current-window old)))))

  (define (wheel-mover dir)
    ;; Wheel direction (the low bits of a 64-flagged button): up, down,
    ;; left, right.  Vertical ticks move point by lines; horizontal ones
    ;; move it sideways within its line.
    (case dir
      [(0) (lambda () (move-vertical! -3))]
      [(1) (lambda () (move-vertical! 3))]
      [(2) (lambda () (goto-point! (cons point-row (- point-col 3))))]
      [(3) (lambda () (goto-point! (cons point-row (+ point-col 3))))]
      [else (lambda () (void))]))

  (define (mouse-event!)
    ;; The rest of an ESC [ < sequence: b ; x ; y then M (press) or
    ;; m (release).  Wheel is button 64/65; releases are ignored.
    (let drain ([c (read-char stdin)] [ps '()])
      (if (and (char? c) (or (char<=? #\0 c #\9) (char=? c #\;)))
          (drain (read-char stdin) (cons c ps))
          (let ([nums (let split ([chars (reverse ps)] [cur 0] [acc '()])
                        (cond [(null? chars) (reverse (cons cur acc))]
                              [(char=? (car chars) #\;)
                               (split (cdr chars) 0 (cons cur acc))]
                              [else (split (cdr chars)
                                           (+ (* cur 10)
                                              (- (char->integer (car chars)) 48))
                                           acc)]))])
            (when (and (char? c) (= (length nums) 3))
              (let ([b (car nums)] [x (cadr nums)] [y (caddr nums)])
                (cond [(char=? c #\m) (set! drag-status #f)]     ; release
                      [(= (bitwise-and b 64) 64)                 ; wheel
                       (mouse-wheel! y (wheel-mover (bitwise-and b 3)))]
                      [(= (bitwise-and b 32) 32)                 ; drag
                       (when (< (bitwise-and b 3) 3)
                         (mouse-drag! x y))]
                      [(< (bitwise-and b 3) 3)                   ; a press
                       (mouse-press! x y)])))))))

  ;;; Key handling ----------------------------------------------------------

  ;; Keys bound by modules, consulted before the built-in bindings (so a
  ;; module may also rebind a built-in key): plain control keys, keys
  ;; after the C-x prefix, and keys after ESC (meta).
  (define user-keys (make-registry))       ; entries: (key-code . command)
  (define user-cx-keys (make-registry))
  (define user-meta-keys (make-registry))

  (define (user-binding r code)
    (let ([hit (registry-find r (lambda (item) (= (car item) code)))])
      (and hit (cdr hit))))

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
       (registry-add! user-cx-keys (cons (code (string-tail spec 4)) command))]
      [(and (= (string-length spec) 3) (string-prefix? "M-" spec))
       (registry-add! user-meta-keys
                      (cons (char->integer (string-ref spec 2)) command))]
      [else (registry-add! user-keys (cons (code spec) command))]))

  (define (escape-sequence!)
    (let ([a (read-char stdin)])
      (cond
        [(eof-object? a) (void)]
        [(char=? a #\[)
         (let ([first (read-char stdin)])
           (if (and (char? first) (char=? first #\<))
               (mouse-event!)
               (let drain ([b first] [ps '()])
                 (if (and (char? b)
                          (or (char<=? #\0 b #\9) (char=? b #\;)))
                     (drain (read-char stdin) (cons b ps))
                     (let ([params (list->string (reverse ps))])
                       (case b
                         [(#\A) (move-vertical! -1)]
                         [(#\B) (move-vertical! 1)]
                         [(#\C) (move-right!)] [(#\D) (move-left!)]
                         [(#\H) (set! point-col 0)]
                         [(#\F) (set! point-col
                                  (string-length (current-line)))]
                         [(#\~)
                          (cond [(string=? params "3") (delete-forward!)]
                                [(string=? params "5")
                                 (move-vertical! (- (page-size)))]
                                [(string=? params "6")
                                 (move-vertical! (page-size))]
                                [(string=? params "200")
                                 (paste-into-buffer!)]
                                [else (void)])]
                         [else (void)]))))))]
        [(user-binding user-meta-keys (char->integer a))
         => (lambda (command) (command))]
        ;; C-M-_: redo, as in Emacs.
        [(char=? a #\x1f) (redo!)]
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
      [(user-binding user-cx-keys (char->integer c))
       => (lambda (command) (command))]
      [else
       (case (char->integer c)
          [(7) (set! message "Quit")]                           ; C-x C-g
          [(19) (save!!)]                                ; C-x C-s
          [(3) (quit!!)]                                 ; C-x C-c
          [(6) (find-file!!)]                            ; C-x C-f
          [(98) (switch-buffer!!)]                       ; C-x b
          [(107) (kill-buffer!!)]                        ; C-x k
          [(111) (other-window!)]                               ; C-x o
          [(48) (delete-window!)]                               ; C-x 0
          [(49) (delete-other-windows!)]                        ; C-x 1
          [(50) (split-window!)]                                ; C-x 2
          [else (set! message "C-x is undefined for that key")])]))

  (define (handle-key! c)
    (define chain insert-chain)     ; only an unbroken typed run keeps it
    (set! insert-chain #f)
    (when (char? c)
      (let ([n (char->integer c)])
        (unless (= n 11) (set! last-command #f))))              ; C-k chains kills
    (cond
      [key-prefix (handle-prefix! c)]
      [(eof-object? c) (set! quit? #t)]
      [else
       (set! message "")            ; messages last until the next key
       (cond
         [(user-binding user-keys (char->integer c))
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
         [(7) (set! mark-active? #f) (set! message "Quit")]      ; C-g
         [(8 127) (backspace!)]                                   ; C-h, DEL
         [(10 13) (newline!)]                                     ; RET
         [(11) (kill-line!)]                                      ; C-k
         [(12) (set! size-dirty? #t) (erase-screen!)              ; C-l
               (set! message "Screen redrawn")]
         [(14) (move-vertical! 1)]                                ; C-n
         [(15) (let ([row point-row] [col point-col])             ; C-o open line
                 (newline!)
                 (set! point-row row) (set! point-col col))]
         [(16) (move-vertical! -1)]                               ; C-p
         [(19) (search!!)]                                 ; C-s
         [(22) (move-vertical! (page-size))]                      ; C-v
         [(23) (kill-region!)]                                    ; C-w
         [(24) (set! key-prefix 'c-x) (set! message "C-x-")]      ; C-x
         [(25) (yank!)]                                           ; C-y
         [(27) (escape-sequence!)]                                ; ESC
         [(31) (undo!)]                                           ; C-_
         [else (when (>= (char->integer c) 32)
                 (self-insert! c chain))])])]))

  ;;; Modules -----------------------------------------------------------------

  ;; Extension modules are libraries in the lib directory, loaded through
  ;; here -- by the loader at startup, or later by hand -- so the core
  ;; knows which modules exist and owns their registrations (see the
  ;; module registries above).

  (define loaded-modules '())   ; module names, in load order

  (define (module-source name)
    (format "~a/~a.e" (caar (library-directories)) name))

  (define (init-module! name)
    ;; Import the module's library into the editor's top level (compiling
    ;; it when stale) and run its init!, if any, owning its registrations.
    (let ([lib (list (string->symbol name))])
      (eval `(import ,lib) (interaction-environment))
      (when (memq 'init! (library-exports lib))
        (parameterize ([registering-module (string->symbol name)])
          (eval `(let () (import (only ,lib init!)) (init!))
                (interaction-environment))))))

  (define (load-module! name)
    (unless (member name loaded-modules)
      (set! loaded-modules (append loaded-modules (list name))))
    (init-module! name))

  (define (load-modules!)
    ;; Load every module in the lib directory (the loader script has
    ;; already pointed library-directories there): each .e file but the
    ;; core itself, in name order.  A broken module reports itself
    ;; without keeping the editor (or the others) from starting.
    (for-each
      (lambda (file)
        (guard (ex [else
                    (let ([msg (format "Error in ~a: ~a"
                                       file (error-text ex))])
                      (display (format "e: ~a\n" msg) (current-error-port))
                      (set! message msg))])
          (load-module! (substring file 0 (- (string-length file) 2)))))
      (sort string<?
            (filter (lambda (file)
                      (and (string-suffix? ".e" file)
                           (not (string=? file "core.e"))))
                    (directory-list (caar (library-directories)))))))

  (define (module-requires? name target)
    ;; Does library (name) build on (target), directly or through others?
    (let ([t (string->symbol target)])
      (let walk ([lib (list (string->symbol name))])
        (exists (lambda (req) (or (eq? (car req) t) (walk req)))
                (guard (ex [else '()]) (library-requirements lib))))))

  (define (refresh-buffer-modes!)
    ;; Re-resolve every buffer's mode by name, so buffers pick up a
    ;; reloaded mode's new styles (or lose a mode that is gone).
    (for-each (lambda (b)
                (let ([m (buffer-mode b)])
                  (when m (buffer-mode-set! b (find-mode (mode-name m))))))
              buffers))

  (define (reload-module! name*)
    ;; Reload a module in place: redefine its library from the (edited)
    ;; source, likewise every loaded module built on it, then retract all
    ;; module registrations and run every init! afresh -- the effect is
    ;; exactly a clean startup, with the editor's state (buffers, windows,
    ;; this session's top level) untouched.  Closures already captured
    ;; keep running the old code; a module's own state starts over.  The
    ;; core itself cannot be reloaded: everything is compiled against it.
    (let* ([name (if (symbol? name*) (symbol->string name*) name*)]
           [source (module-source name)])
      (when (string=? name "core")
        (error 'reload-module! "the core cannot be reloaded in place"))
      (unless (file-exists? source)
        (error 'reload-module! "no module source" source))
      (load source)
      (unless (member name loaded-modules)
        (set! loaded-modules (append loaded-modules (list name))))
      (for-each (lambda (m)
                  (when (and (not (string=? m name))
                             (module-requires? m name))
                    (load (module-source m))))
                loaded-modules)
      (for-each (lambda (m) (retract-module! (string->symbol m)))
                loaded-modules)
      (for-each init-module! loaded-modules)
      (refresh-buffer-modes!)
      (invalidate-screen-cache!)
      (set! message (format "Reloaded ~a" name))))

  ;; Saving a module's source reloads it on the spot (a fresh .e file in
  ;; the lib directory is loaded for the first time), so editing the
  ;; editor from inside itself takes effect on save.  Off by default;
  ;; the loader script turns it on -- comment that line out for an
  ;; installation without it -- and M-x (auto-reload #f) turns it off
  ;; for a session.
  (define auto-reload (make-parameter #f))

  (define (canonical-path path*)
    ;; path made absolute, with ".", "..", and empty segments resolved
    ;; textually (symbolic links are not chased) -- enough to recognize
    ;; the editor's own files whichever way they are named.
    (let* ([path (if (string-prefix? "/" path*)
                     path*
                     (string-append (current-directory) "/" path*))]
           [n (string-length path)])
      (let loop ([i 0] [start 0] [stack '()])
        (define (push seg)
          (cond [(or (string=? seg "") (string=? seg ".")) stack]
                [(string=? seg "..") (if (pair? stack) (cdr stack) stack)]
                [else (cons seg stack)]))
        (cond [(> i n) (string-append "/" (string-join (reverse stack) "/"))]
              [(or (= i n) (char=? (string-ref path i) #\/))
               (loop (+ i 1) (+ i 1) (push (substring path start i)))]
              [else (loop (+ i 1) start stack)]))))

  (define (module-name-of-path path)
    ;; The module name a saved path denotes: a .e file directly in the
    ;; editor's lib directory; #f for anything else -- the core included,
    ;; which cannot be reloaded.
    (let ([full (canonical-path path)]
          [lib (string-append (canonical-path (caar (library-directories)))
                              "/")])
      (and (string-prefix? lib full)
           (string-suffix? ".e" full)
           (let ([base (string-tail full (string-length lib))])
             (and (not (string-search base "/" 0 (string-length base)))
                  (not (string=? base "core.e"))
                  (substring base 0 (- (string-length base) 2)))))))

  (define (reload-on-save! path)
    ;; The save-file! hook.  A reload that fails (a module saved mid-edit,
    ;; say) reports itself without disturbing the save -- or the editor,
    ;; which keeps running the module's old version.
    (let ([name (and (auto-reload) (module-name-of-path path))])
      (when name
        (guard (ex [else (set! message
                           (format "Wrote ~a; reload failed: ~a"
                                   path (error-text ex)))])
          (reload-module! name)
          (set! message (format "Wrote ~a (reloaded ~a)" path name))))))

  ;;; Main ------------------------------------------------------------------

  (define (usage)
    (display "Usage: e [file]\n")
    (display "A tiny Emacs-like terminal editor. Set LINES/COLUMNS if needed.\n")
    (display "Extension modules are loaded from the lib directory at startup.\n"))

  (define (main)
    ;; The loader script is pure bootstrap; the extension modules are
    ;; loaded here, before the file argument needs their modes.
    (let ([args (command-line-arguments)])
      (when (and (pair? args) (member (car args) '("-h" "--help"))) (usage) (exit 0))
      (load-modules!)
      (when (pair? args) (visit-file! (car args))))
    (unless (and (getenv "TERM") (not (string=? (getenv "TERM") "dumb")))
      (display "e: an interactive terminal is required\n" (current-error-port))
      (exit 1))
    ;; A stray SIGINT outside an evaluation must not drop into Chez's break
    ;; prompt underneath the editor's screen.
    (keyboard-interrupt-handler void)
    ;; A stray (exit) or (abort) evaluated at the prompt must not kill
    ;; the process past the modified-buffers check: they run the
    ;; editor's quit and unwind the evaluation instead.
    (let ([safe-quit (lambda args
                       (quit!!)
                       (raise (make-interrupted)))])
      (exit-handler safe-quit)
      (abort-handler safe-quit)
      (reset-handler safe-quit))
    (dynamic-wind
      ;; The alternate screen, plus bracketed paste: terminals that
      ;; support it (virtually all) wrap pastes in ESC[200~ / ESC[201~,
      ;; making a paste one identifiable edit; others ignore the mode.
      ;; Mouse tracking likewise (see mouse!).
      (lambda () (terminal-raw!) (ansi "\x1b;[?1049h\x1b;[2J\x1b;[?2004h")
                 (set-mouse! #t))
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
      (lambda () (unless (string=? cursor-style-shown "\x1b;[0 q")
                   (ansi "\x1b;[0 q"))
                 (ansi "\x1b;[?1002;1006l\x1b;[?2004l\x1b;[?25h\x1b;[?1049l\x1b;[0m")
                 (flush-output-port stdout)
                 (terminal-restore!))))

  ) ;; library (core)
