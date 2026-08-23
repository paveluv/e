;; search.e -- incremental search for the e editor.
;;
;; An e extension module: the library (search), loaded at startup by
;; the core, which calls init!.  C-s starts the search; typing extends
;; the needle, C-s repeats, backspace retracts, RET or ESC accepts
;; where it stands, C-g cancels back to the origin.  The needle's
;; matches in the current buffer paint cyan and the current match
;; yellow, through the core's styled highlighter ranges.  Other
;; control keys (C-x o, C-x b and friends) run through the ordinary
;; dispatch with the search carrying on, so windows and buffers can be
;; switched mid-search.

(library (search)
  (export init! search!! search-fold-case)
  (import (chezscheme) (core))

  ;; Configuration: whether the incremental search folds case the
  ;; smart way, as Emacs does -- matching ignores case only while the
  ;; needle is all lowercase; one typed capital makes it exact.
  ;; (search-fold-case #f) in config.e makes C-s always exact.
  ;; M-c inside a search toggles the current search either way.
  (define search-fold-case (make-parameter #t))

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
        (let* ([b (current-buffer)]
               [len (string-length needle-now)]
               [rows (buffer-line-count b)])
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
                (let ([line (buffer-line b row)])
                  (let scan ([from 0] [acc acc])
                    (let ([hit (string-search line needle-now from
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
    (let* ([b (current-buffer)]
           [rows (buffer-line-count b)])
      (let loop ([row start-row] [col start-col] [remaining rows])
        (if (= remaining 0)
            (let* ([line (buffer-line b start-row)]
                   [found (string-search line needle 0
                            (min (+ start-col (string-length needle) -1)
                                 (string-length line))
                            (fold-for needle))])
              (and found (cons start-row found)))
            (let* ([line (buffer-line b row)]
                   [found (string-search line needle col
                                         (string-length line)
                                         (fold-for needle))])
              (if found
                  (cons row found)
                  (loop (modulo (+ row 1) rows) 0 (- remaining 1))))))))

  (define (goto-match! match)
    (goto-point! (cons (car match) (cdr match))))

  (define (goto-match-end! match needle)
    ;; Point lands right after the match, so accepting the search
    ;; leaves it there -- a region set before searching then covers
    ;; the found text.
    (goto-point! (cons (car match)
                       (+ (cdr match) (string-length needle)))))

  (define (indicate! s)
    ;; The search's status line, its label greyed like any prompt's.
    ;; The search is no prompt! -- it reads keys, not a line -- so the
    ;; label grey comes through the message's own styler rather than
    ;; the prompt machinery.
    (if (string=? s "")
        (parameterize ([message-source #f]) (set-message! s))
        (show-message! s
          (cons s (lambda (text)
                    (let* ([n (string-length text)]
                           [v (make-vector n 'plain)]
                           [colon (string-search text ": " 0 n)])
                      (when colon
                        (vector-fill-range! v 0 (+ colon 2) 'comment))
                      v))))))

  (define (run-search!)
    (define origin-window (selected-window))
    (define origin (point))
    (define (match-here? match)
      (and match (eq? (car match) (current-buffer))))
    (define (anchor match)
      ;; Where the next search starts: the current match when it is in
      ;; this buffer, else point.
      (if (match-here? match)
          (cons (cadr match) (caddr match))
          (point)))
    (define (found hit needle)
      (list (current-buffer) (car hit) (cdr hit) (string-length needle)))
    (define (dispatch! c)
      ;; Keys the search does not use run through the ordinary
      ;; dispatch, so windows and buffers can be switched without
      ;; leaving the search; it then continues from point in the new
      ;; buffer.  A C-x owns the following key: the chord completes
      ;; here, its hint visible, before the search prompt repaints.
      (dispatch-key! c)
      (when (and (= (char->integer c) 24) (not (quitting?)))
        (redraw!)
        (let ([k (read-key)])
          (dispatch-key! (or k (eof-object))))))
    ;; A match records where it was found -- (buffer row col len) --
    ;; so the highlight and the anchors survive an excursion to
    ;; another window or buffer.
    (set! fold-override #f)
    (let loop ([needle ""] [match #f] [failed? #f])
      (set! needle-now needle)
      (set! current-match
        (and match (list (car match) (cadr match) (caddr match)
                         (+ (caddr match) (cadddr match)))))
      (indicate!
        (format "~aI-search~a: ~a" (if failed? "Failing " "")
                (if (fold-for needle) "" " (exact)") needle))
      (redraw!)
      (let ([c (read-key)])
        (if (not c)
            (dispatch-key! (eof-object))     ; end of input: quit
            (case (char->integer c)
              ;; RET or ESC accepts the current match, silently.  An
              ;; escape sequence (arrow key etc.) also accepts, then
              ;; moves point.
              [(10 13 27)
               (let ([k (and (= (char->integer c) 27) (pending-input?)
                             (peek-key))])
                 (if (and k (memv k '(#\c #\C)))
                     ;; M-c: flip case sensitivity for this search and
                     ;; look again from where it started
                     (begin
                       (read-key)
                       (set! fold-override
                         (if (fold-for needle) 'exact 'fold))
                       (let ([home (if (eq? (selected-window)
                                            origin-window)
                                       origin
                                       (point))])
                         (if (string=? needle "")
                             (loop needle match failed?)
                             (let ([next (search-forward-from
                                           needle (car home) (cdr home))])
                               (when next (goto-match-end! next needle))
                               (loop needle
                                     (and next (found next needle))
                                     (not next))))))
                     (begin
                       (set! needle-now "")
                       (set! current-match #f)
                       (indicate! "")
                       (when k (dispatch-key! c)))))]
              ;; C-g cancels the search and restores point -- back in
              ;; the window it started in.
              [(7)
               (set! needle-now "")
               (set! current-match #f)
               (when (select-window! origin-window)
                 (goto-point! origin))
               (indicate! "Quit")]
              ;; C-s repeats the current search from just beyond this
              ;; match -- or from point, after a move to another buffer.
              [(19)
               (if (string=? needle "")
                   (loop needle match failed?)
                   (let* ([a (anchor match)]
                          [skip (if (match-here? match) 1 0)]
                          [next (search-forward-from needle (car a)
                                                     (+ (cdr a) skip))])
                     (if next
                         (begin (goto-match-end! next needle)
                                (loop needle (found next needle) #f))
                         (loop needle match #t))))]
              ;; Backspace shortens the needle and searches again from
              ;; the original point (or from point, away from the
              ;; origin window).
              [(8 127)
               (if (string=? needle "")
                   (loop needle match failed?)
                   (let ([shorter (substring needle 0
                                    (- (string-length needle) 1))]
                         [home (if (eq? (selected-window) origin-window)
                                   origin
                                   (point))])
                     (if (string=? shorter "")
                         (begin (goto-match! home) (loop shorter #f #f))
                         (let ([next (search-forward-from shorter (car home)
                                                          (cdr home))])
                           (when next (goto-match-end! next shorter))
                           (loop shorter (and next (found next shorter))
                                 (not next))))))]
              [else
               (if (< (char->integer c) 32)
                   ;; Any other control key runs as usual -- C-x o,
                   ;; C-x b and friends -- and the search carries on.
                   (begin (dispatch! c)
                          (unless (quitting?)
                            (loop needle match failed?)))
                   ;; Extend the current match when possible; if it no
                   ;; longer matches, continue forward to the next
                   ;; candidate.
                   (let* ([longer (string-append needle (string c))]
                          [a (anchor match)]
                          [next (search-forward-from longer (car a)
                                                     (cdr a))])
                     (if next
                         (begin (goto-match-end! next longer)
                                (loop longer (found next longer) #f))
                         (loop longer match #t))))])))))

  (define (search!!)
    ;; The search owns C-g while it runs; the match highlighting goes
    ;; away however it exits.
    (call-uninterrupted
      (lambda ()
        (dynamic-wind
          void
          run-search!
          (lambda ()
            (set! needle-now "")
            (set! current-match #f))))))

  (define (init!)
    (add-highlighter! search-highlights)
    (bind-key! "C-s" search!!)))
