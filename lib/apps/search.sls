;; Search commands and the temporary M-x needle preview adapter.

(import (only (foundation edoc) elibrary))
(elibrary (apps search)
  (export count (rename (search-fold-case fold-case)) (rename (search! incremental!)) init!
          replace!)
  (import (chezscheme)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head completion) completion:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (head literal)
          (prefix (head paint) paint:)
          (prefix (head search-host) search-host:))

  ;; Configuration: whether the incremental search folds case the
  ;; smart way, as Emacs does -- matching ignores case only while the
  ;; needle is all lowercase; one typed capital makes it exact.
  ;; (search:fold-case #f) in config.e makes C-s always exact.
  ;; M-c inside a search toggles the current search either way.
  (edoc "Whether incremental search folds case the smart way: matching ignores case only while the needle is all lowercase."
        (value boolean))
  (define search-fold-case (make-parameter #t))

  ;; M-x's typed needle preview moves to the type-selected widget path in W6f.
  (define preview-highlights #f)
  (define (search-highlights) (if preview-highlights (preview-highlights) '()))
  (define (goto-match! match) (head:goto! match))

  (define (matches-of b needle)
    ;; every match of needle in b as (row . col), in order and without
    ;; overlaps, exactly as the replace commands find them
    (let ([m (string-length needle)])
      (let rows ([row 0] [acc '()])
        (if (= row (head:buffer-line-count b))
            (reverse acc)
            (let ([s (head:buffer-line b row)])
              (let cols ([from 0] [acc acc])
                (let ([hit (string:search s needle from (string-length s))])
                  (if hit
                      (cols (+ hit m) (cons (cons row hit) acc))
                      (rows (+ row 1) acc)))))))))

  (define (make-searcher)
    ;; The needle type's live search for the prompt: the needle's matches
    ;; highlight through the search highlighter, the current one on top,
    ;; point previews a match without changing the command's input region.
    ;; Anchors and cached matches follow the adopted source's revision.
    (let* ([window (head:current-window)] [b (head:current-buffer)]
           [origin (head:point)] [basis (caddr (head:edit-basis b))]
           [needle #f] [hits '()] [at #f])
      (define (first-at position)
        (let loop ([rest hits] [i 0])
          (cond [(null? rest) (and (pair? hits) 0)]
                [(not (text:position<? (car rest) position)) i]
                [else (loop (cdr rest) (+ i 1))])))
      (define (refresh! s)
        (let-values ([(lines revision changes) (head:snapshot-since b basis)])
          (when (or (not (equal? s needle)) (not (= revision basis)))
            (let ([chosen (and at (list-ref hits at))]
                  [deltas (if changes (map caddr changes) '())])
              (set! origin (fold-left text:rebase-position origin deltas))
              (set! hits (if (or (not s) (string=? s "")) '() (matches-of b s)))
              (set! at (first-at (if (and chosen (equal? s needle))
                                   (fold-left text:rebase-position chosen deltas) origin)))
              (set! needle s)
              (set! basis revision)))))
      (define (show!)
        (when (and at (memq window (head:windows)) (eq? (head:window-buffer window) b))
          (head:with-window window (goto-match! (list-ref hits at))))
        (cons (and at (+ at 1)) (length hits)))
      (define (move step)
        (refresh! needle)
        (when (pair? hits) (set! at (mod (+ at step) (length hits))))
        (show!))
      (set! preview-highlights
        (lambda ()
          (refresh! needle)
          (if (not (eq? b (head:current-buffer))) '()
              (let ([n (if needle (string-length needle) 0)])
                (append (if at (let ([p (list-ref hits at)]) (list (list (car p) (cdr p) (+ (cdr p) n) 'match-point))) '())
                        (map (lambda (p) (list (car p) (cdr p) (+ (cdr p) n) 'match)) hits))))))
      (completion:make-searcher
        (lambda (s)
          (refresh! s)
          (show!))
        (lambda () (move 1))
        (lambda () (move -1))
        (lambda (accepted?)
          (refresh! needle)
          (set! preview-highlights #f)
          (when (and (memq window (head:windows)) (eq? (head:window-buffer window) b))
            (head:with-window window (head:goto! origin)))))))

  ;; The needle type: a string argument that searches while it is typed.
  ;; At M-x the prompt highlights the needle's matches in the current
  ;; buffer as a search would and Tab visits them in turn, completing
  ;; nothing. Ending the preview restores the command's original point.
  (edoc-type needle "text to find in the current buffer, within one line; typed at M-x, its matches highlight and Tab visits them in turn"
    (predicate (lambda (v) (and (string? v) (> (string-length v) 0))))
    (search make-searcher)
    (within string))

  (edoc "Start incremental search in the current editor through the ordinary event pump. C-s repeats, M-c toggles case, Return accepts and C-g returns to the safely rebased origin.")
  (define (search!) (search-host:open! (if (search-fold-case) 'smart 'exact)))

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
        (needle needle "the text to count, within one line")
        (returns integer) (public))
  (define (count needle)
    (for-matches! (edit:current-region) needle (lambda (row col) (string-length needle))))

  (edoc "Replace every occurrence of from with to in the selected region, else in the whole current buffer: one entry of the delta log per occurrence under one batch, one undo step, point left where it was."
        (from needle "the text to find, within one line")
        (to string "its replacement")
        (returns integer "how many occurrences were replaced")
        (public) (edits))
  (define (replace! from to)
    (define m (string-length from))
    (define (occurrences r)
      ;; every occurrence within the region, an edit each in the text's
      ;; order; the needle is within one line, so each selected row is
      ;; searched between its selected edges
      (let* ([b (region-buffer r)]
             [start (region-start r)]
             [end (region-end r)]
             [last (min (car end) (- (head:buffer-line-count b) 1))])
        (let rows ([row (max 0 (car start))] [out '()])
          (if (> row last)
              (reverse out)
              (let* ([s (head:buffer-line b row)]
                     [n (string-length s)]
                     [from-col (if (= row (car start)) (min (cdr start) n) 0)]
                     [to-col (if (= row (car end)) (min (cdr end) n) n)])
                (let hits ([at from-col] [out out])
                  (let ([hit (and (< at to-col) (string:search s from at to-col))])
                    (if hit
                        (hits (+ hit m) (cons (list (cons row hit) (cons row (+ hit m)) to) out))
                        (rows (+ row 1) out)))))))))
    (when (= m 0) (error 'replace! "empty search string"))
    (let ([r (edit:current-region)] [basis (edit:basis)])
      (edit:call-as-one-edit!
        (format "(search:replace! ~s ~s)" from to)
        (lambda () (edit:rewrite-regions! basis (occurrences r))))))

  (edoc "Install the search composition, the temporary typed-needle preview and default C-s/M-% bindings." (public))
  (define (init!)
    (search-host:init!)
    (paint:add-highlighter! search-highlights)
    (keymap:bind-default! "C-s" search!)
    (keymap:bind-default! "M-%" (keymap:prefill replace!))))
