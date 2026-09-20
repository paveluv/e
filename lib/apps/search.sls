;; search.sls -- incremental search for the e editor.
;;
;; An e extension module: the library (search), loaded at startup by
;; the kernel, which calls init!.  C-s starts the search; typing extends
;; the needle, C-s repeats, backspace retracts, RET or ESC accepts
;; where it stands, C-g cancels back to the origin.  The needle's
;; matches in the current buffer paint cyan and the current match
;; yellow, through the painter's styled highlighter ranges.  Other
;; control keys (C-x o, C-x b and friends) run through the ordinary
;; dispatch with the search carrying on, so windows and buffers can be
;; switched mid-search.

(import (only (edoc) elibrary))
(elibrary (search)
  (export init! (rename (search! incremental!)) (rename (search-fold-case fold-case))
          count replace-all! replace!)
  (import (chezscheme)
          (prefix (edit) edit:)
          (literal)
          (prefix (dispatch) dispatch:)
          (prefix (style) style:)
          (prefix (prompt) prompt:)
          (prefix (string) string:)
          (prefix (paint) paint:)
          (prefix (tty) tty:)
          (prefix (keymap) keymap:)
          (prefix (head) head:)
          (prefix (window) window:)
          (prefix (doc) doc:))

  ;; Configuration: whether the incremental search folds case the
  ;; smart way, as Emacs does -- matching ignores case only while the
  ;; needle is all lowercase; one typed capital makes it exact.
  ;; (search:fold-case #f) in config.e makes C-s always exact.
  ;; M-c inside a search toggles the current search either way.
  (edoc "Whether incremental search folds case the smart way: matching ignores case only while the needle is all lowercase."
        (value boolean))
  (define search-fold-case (make-parameter #t))

  ;; The most recently entered nonempty needle.  It survives accepting
  ;; or cancelling a search, so C-s at an empty I-search can repeat it.
  (define last-needle "")

  ;; The running search's M-c override: 'fold or 'exact beats the
  ;; smart default for this search alone.
  (define fold-override #f)

  (define (fold-for needle)
    ;; Whether this needle matches case-insensitively right now.
    (case fold-override
      [(fold) #t]
      [(exact) #f]
      [else (and (search-fold-case)
                 (let all-lower ([i 0])
                   (or (= i (string-length needle))
                       (and (not (char-upper-case? (string-ref needle i)))
                            (all-lower (+ i 1))))))]))

  ;; The live search, feeding the registered highlighter: the needle
  ;; whose matches paint cyan, and the current match -- (buffer row
  ;; start end) -- painted yellow while that buffer is current.
  (define needle-now "")
  (define current-match #f)

  (define (search-highlights)
    ;; The needle's matches in the current buffer, overlaps included,
    ;; with the current match on top.
    (if (string=? needle-now "")
        '()
        (let* ([b (head:current-buffer)]
               [len (string-length needle-now)]
               [rows (head:buffer-line-count b)])
          (let loop ([row 0]
                     [acc (if (and current-match
                                   (eq? (car current-match) b))
                              (list (list (cadr current-match)
                                          (caddr current-match)
                                          (cadddr current-match)
                                          'match-point))
                              '())])
            (if (= row rows)
                acc
                (let ([line (head:buffer-line b row)])
                  (let scan ([from 0] [acc acc])
                    (let ([hit (string:search line needle-now from
                                              (string-length line)
                                              (fold-for needle-now))])
                      (if hit
                          (scan (+ hit 1)   ; overlapping matches too
                                (cons (list row hit (+ hit len) 'match)
                                      acc))
                          (loop (+ row 1) acc))))))))))

  (define (search-forward-from needle start-row start-col)
    ;; Search from the supplied position to the end of the buffer, then
    ;; wrap once.  The first pass covers the starting line from
    ;; start-col onward, so the wrap pass covers matches beginning
    ;; before start-col -- including ones that straddle it.
    (let* ([b (head:current-buffer)]
           [rows (head:buffer-line-count b)])
      (let loop ([row start-row] [col start-col] [remaining rows])
        (if (= remaining 0)
            (let* ([line (head:buffer-line b start-row)]
                   [found (string:search line needle 0
                            (min (+ start-col (string-length needle) -1)
                                 (string-length line))
                            (fold-for needle))])
              (and found (cons start-row found)))
            (let* ([line (head:buffer-line b row)]
                   [found (string:search line needle col
                                         (string-length line)
                                         (fold-for needle))])
              (if found
                  (cons row found)
                  (loop (modulo (+ row 1) rows) 0 (- remaining 1))))))))

  (define (goto-match! match)
    (head:goto! (cons (car match) (cdr match))))

  (define (goto-match-end! match needle)
    ;; Point lands right after the match, so accepting the search
    ;; leaves it there -- a region set before searching then covers
    ;; the found text.
    (head:goto! (cons (car match)
                      (+ (cdr match) (string-length needle)))))

  (define (indicate! s)
    ;; The search's status line, its label greyed like any prompt's.
    ;; The search is no prompt -- it reads keys, not a line -- so the
    ;; label grey comes through the message's own styler rather than
    ;; the prompt machinery.
    (if (string=? s "")
        (parameterize ([edit:message-source #f]) (edit:set-message! s))
        (paint:show-message! s
          (cons s (lambda (text)
                    (let* ([n (string-length text)]
                           [v (make-vector n 'plain)]
                           [colon (string:search text ": " 0 n)])
                      (when colon
                        (style:fill-range! v 0 (+ colon 2) 'chrome))
                      v))))))

  (define (run-search!)
    (define origin-window (head:current-window))
    (define origin (head:point))
    (define (match-here? match)
      (and match (eq? (car match) (head:current-buffer))))
    (define (anchor match)
      ;; Where the next search starts: the current match when it is in
      ;; this buffer, else point.
      (if (match-here? match)
          (cons (cadr match) (caddr match))
          (head:point)))
    (define (found hit needle)
      (list (head:current-buffer) (car hit) (cdr hit) (string-length needle)))
    (define (dispatch! event)
      ;; Keys the search does not use run through the ordinary
      ;; dispatch, so windows and buffers can be switched without
      ;; leaving the search; it then continues from point in the new
      ;; buffer.  The global dispatcher reads a complete chord.
      (dispatch:key! event))
    ;; A match records where it was found -- (buffer row col len) --
    ;; so the highlight and the anchors survive an excursion to
    ;; another window or buffer.
    (set! fold-override #f)
    (let loop ([needle ""] [match #f] [failed? #f])
      (unless (string=? needle "") (set! last-needle needle))
      (set! needle-now needle)
      (set! current-match
        (and match (list (car match) (cadr match) (caddr match)
                         (+ (caddr match) (cadddr match)))))
      (indicate!
        (format "~aI-search~a: ~a" (if failed? "Failing " "")
                (if (fold-for needle) "" " (exact)") needle))
      (paint:redraw!)
      (let* ([event (head:read-key-event)]
             [action (and (not (eof-object? event))
                          (keymap:event-binding 'isearch event))])
        (cond
          [(eof-object? event) (dispatch:key! event)]
          [(eq? action 'accept)
           (set! needle-now "")
           (set! current-match #f)
           (indicate! "")]
          [(eq? action 'accept-dispatch)
           (set! needle-now "")
           (set! current-match #f)
           (indicate! "")
           (dispatch:key! event)]
          [(eq? action 'toggle-case)
           (set! fold-override (if (fold-for needle) 'exact 'fold))
           (let ([home (if (eq? (head:current-window) origin-window)
                           origin
                           (head:point))])
             (if (string=? needle "")
                 (loop needle match failed?)
                 (let ([next (search-forward-from needle
                                                  (car home) (cdr home))])
                   (when next (goto-match-end! next needle))
                   (loop needle (and next (found next needle)) (not next)))))]
          [(eq? action 'cancel)
           (set! needle-now "")
           (set! current-match #f)
           (when (window:focus! origin-window) (head:goto! origin))
           (indicate! "Quit")]
          [(eq? action 'repeat)
           (if (string=? needle "")
               (if (string=? last-needle "")
                   (loop needle match failed?)
                   (let* ([home (head:point)]
                          [next (search-forward-from last-needle
                                                     (car home) (cdr home))])
                     (when next (goto-match-end! next last-needle))
                     (loop last-needle
                           (and next (found next last-needle))
                           (not next))))
               (let* ([a (anchor match)]
                      [skip (if (match-here? match) 1 0)]
                      [next (search-forward-from needle (car a)
                                                 (+ (cdr a) skip))])
                 (if next
                     (begin (goto-match-end! next needle)
                            (loop needle (found next needle) #f))
                     (loop needle match #t))))]
          [(eq? action 'delete-character)
           (if (string=? needle "")
               (loop needle match failed?)
               (let ([shorter (substring needle 0
                                (- (string-length needle) 1))]
                     [home (if (eq? (head:current-window) origin-window)
                               origin
                               (head:point))])
                 (if (string=? shorter "")
                     (begin (goto-match! home) (loop shorter #f #f))
                     (let ([next (search-forward-from shorter (car home)
                                                      (cdr home))])
                       (when next (goto-match-end! next shorter))
                       (loop shorter (and next (found next shorter))
                             (not next))))))]
          [(tty:key-event-character event)
           => (lambda (c)
                (let* ([longer (string-append needle (string c))]
                       [a (anchor match)]
                       [next (search-forward-from longer (car a) (cdr a))])
                  (if next
                      (begin (goto-match-end! next longer)
                             (loop longer (found next longer) #f))
                      (loop longer match #t))))]
          [else
           (dispatch! event)
           (unless (head:quitting?) (loop needle match failed?))]))))

  (edoc "Start an incremental search in the current buffer: typing extends it, C-s repeats, M-c toggles case folding, Return accepts and C-g cancels."
        (prompts))
  (define (search!)
    ;; The search owns C-g while it runs; the match highlighting goes
    ;; away however it exits.
    (prompt:interaction
      (lambda ()
        (dynamic-wind
          void
          run-search!
          (lambda ()
            (set! needle-now "")
            (set! current-match #f))))))

  ;;; Matching -----------------------------------------------------------------------

  (define (for-matches! r needle handle!)
    ;; Walk the matches of needle inside r in order, calling
    ;; (handle! row col) on each; it returns the width the match occupies
    ;; afterwards (an edit may have changed it).  The match count.
    ;; Needles are single-line: lines are searched one at a time.
    (when (= (string-length needle) 0)
      (error 'search "empty search string"))
    (let* ([b (region-buffer r)]
           [m (string-length needle)]
           [start (region-start r)]
           [end (region-end r)]
           [count 0])
      (let row-loop ([row (max 0 (car start))])
        (when (<= row (min (car end) (- (head:buffer-line-count b) 1)))
          (let col-loop ([at (if (= row (car start)) (cdr start) 0)]
                         [shift 0])
            (let* ([s (head:buffer-line b row)]
                   [limit (if (= row (car end))
                              (min (+ (cdr end) shift) (string-length s))
                              (string-length s))]
                   [hit (string:search s needle at limit)])
              (if hit
                  (let ([w (handle! row hit)])
                    (set! count (+ count 1))
                    (col-loop (+ hit w) (+ shift (- w m))))
                  (row-loop (+ row 1)))))))
      count))

  (edoc "How many times needle occurs in the selected region, else in the whole current buffer."
        (needle string "the text to count, within one line")
        (returns integer))
  (define (count needle)
    (for-matches! (edit:current-region) needle (lambda (row col) (string-length needle))))

  (edoc "Replace every occurrence of from with to in the selected region, else in the whole current buffer: one undo step, point left where it was."
        (from string "the text to find, within one line")
        (to string "its replacement")
        (returns integer "how many occurrences were replaced"))
  (define (replace-all! from to)
    (define m (string-length from))
    (define (replace-line s)
      ;; Accumulate pieces and join once instead of copying the growing line
      ;; for every non-overlapping match.
      (let loop ([at 0] [pieces '()] [count 0])
        (let ([hit (string:search s from at (string-length s))])
          (if hit
              (loop (+ hit m)
                    (cons to (cons (substring s at hit) pieces))
                    (+ count 1))
              (values (apply string-append
                             (reverse (cons (string:tail s at) pieces)))
                      count)))))
    (define (rewritten-region r)
      ;; Preserve the single-line-needle contract by rewriting each selected
      ;; row independently, including only the selected edge fragments.
      (let* ([b (region-buffer r)]
             [start (region-start r)]
             [end (region-end r)]
             [last (min (car end) (- (head:buffer-line-count b) 1))])
        (let loop ([row (max 0 (car start))] [lines '()] [count 0])
          (if (> row last)
              (values (string:join (reverse lines) "\n") count)
              (let* ([s (head:buffer-line b row)]
                     [n (string-length s)]
                     [from-col (if (= row (car start)) (min (cdr start) n) 0)]
                     [to-col (if (= row (car end)) (min (cdr end) n) n)])
                (let-values ([(line found)
                              (replace-line
                                (substring s from-col (max from-col to-col)))])
                  (loop (+ row 1) (cons line lines) (+ count found))))))))
    (when (= m 0) (error 'replace-all! "empty search string"))
    (let ([r (edit:current-region)] [basis (head:edit-basis (head:current-buffer))])
      (edit:call-as-one-edit!
        (format "(search:replace-all! ~s ~s)" from to)
        (lambda ()
          (let-values ([(text count) (rewritten-region r)])
            (when (> count 0)
              (edit:rewrite-region! basis (region-start r) (region-end r) text))
            count)))))

  ;;; Query replace --------------------------------------------------------------------

  ;; The candidate being offered, drawn highlighted by the highlighter
  ;; init! registers; #f outside replace!.
  (define query-match #f)

  (define (find-from b needle row col)
    ;; The first match of needle at or after (row . col): (row . start),
    ;; or #f.  Needles are single-line.
    (let loop ([row row] [col col])
      (and (< row (head:buffer-line-count b))
           (let* ([s (head:buffer-line b row)]
                  [hit (string:search s needle col (string-length s))])
             (if hit
                 (cons row hit)
                 (loop (+ row 1) 0))))))

  (edoc "Query-replace in the current buffer from point to the end: each occurrence of from is highlighted and offered, y or SPC replaces, n or DEL skips, q stops; one undo step, point following."
        (from string "the text to find, within one line")
        (to string "its replacement")
        (prompts))
  (define (replace! from to)
    ;; Each occurrence is highlighted and offered -- y (or SPC) replaces,
    ;; n (or DEL) skips, q / RET / C-g / ESC stops.  The whole run is one
    ;; undo step; point follows, ending after the last replacement (or at
    ;; the start of the last skipped or stopped-at match).  The report --
    ;; how many replaced and skipped -- is echoed.
    (if (string=? from "")
        (void)
        (let ([b (head:current-buffer)]
              [m (string-length from)]
              [question (format "Replace ~s with ~s? (y, n, q)" from to)]
              [replaced 0]
              [skipped 0])
          (dynamic-wind
            void
            (lambda ()
              (edit:call-as-one-edit! (format "(search:replace! ~s ~s)" from to)
                (lambda ()
                  (let loop ([row (car (head:point))] [col (cdr (head:point))])
                    (let ([hit (find-from b from row col)])
                      (when hit
                        (set! query-match
                          (list (car hit) (cdr hit) (+ (cdr hit) m)))
                        (head:goto! (cons (car hit) (+ (cdr hit) m)))
                        (parameterize ([edit:message-source #f]) ; an indicator
                          (edit:set-message! question))
                        (paint:redraw!)     ; the match highlight, not the message
                        (let* ([event (head:read-key-event #f)]
                               [action (and (not (eof-object? event))
                                            (keymap:event-binding
                                              'query-replace event))])
                          (case action
                            [(replace)
                             (edit:replace-region-text! hit (cons (car hit) (+ (cdr hit) m)) to)
                             (set! replaced (+ replaced 1))
                             (loop (car (head:point)) (cdr (head:point)))]
                            [(skip)
                             (set! skipped (+ skipped 1))
                             (head:goto! hit)
                             (loop (car hit) (+ (cdr hit) m))]
                            [(stop) (head:goto! hit)]
                            [(quit-prefix)
                             (let ([next (head:read-key-event #f)])
                               (when (and (not (eof-object? next))
                                          (eq? (keymap:event-binding
                                                 'query-replace "C-x" next)
                                               'quit-editor))
                                 (edit:quit!))
                               (head:goto! hit))]
                            [else
                             (if (eof-object? event)
                                 (head:goto! hit)
                                 (loop (car hit) (cdr hit)))]))))))))
            (lambda () (set! query-match #f)))
          (edit:set-message! (format "Replaced ~a, skipped ~a" replaced skipped))
          (void))))

  (edoc "Install search: its describe entry, the match highlighters, C-s with the search keymap, and M-% with the query-replace keymap.")
  (define (init!)
    (doc:register!
      '(((search:incremental!) (("procedure" . "(search:incremental!)")) "void"
         ("(search)") search "Search commands" #f
         "Start incremental search in the current buffer. Typing extends the search, `C-s` repeats it, `M-c` toggles case sensitivity, Return accepts, and `C-g` cancels.")))
    (paint:add-highlighter! search-highlights)
    (paint:add-highlighter! (lambda () (if query-match (list query-match) '())))
    (keymap:bind-default! "C-s" search!)
    (keymap:bind-default! "M-%" (keymap:prefill replace!))
    (for-each
      (lambda (entry)
        (keymap:bind-default! 'query-replace (car entry) (cadr entry)))
      '(("y" replace) ("Y" replace) ("SPC" replace)
        ("n" skip) ("N" skip) ("BACKSPACE" skip)
        ("q" stop) ("RET" stop) ("C-g" stop) ("ESC" stop)
        ("C-x" quit-prefix) ("C-x C-c" quit-editor)))
    (for-each
      (lambda (entry)
        (keymap:bind-default! 'isearch (car entry) (cadr entry)))
      '(("C-s" repeat) ("C-g" cancel) ("RET" accept) ("ESC" accept)
        ("M-c" toggle-case) ("C-h" delete-character)
        ("BACKSPACE" delete-character)
        ("UP" accept-dispatch) ("DOWN" accept-dispatch)
        ("LEFT" accept-dispatch) ("RIGHT" accept-dispatch)
        ("HOME" accept-dispatch) ("END" accept-dispatch)
        ("PAGEUP" accept-dispatch) ("PAGEDOWN" accept-dispatch)))))
