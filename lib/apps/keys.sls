;; keys.sls -- the key bindings helper: C-x TAB shows in the pop-up the
;; keys that work in the active window's buffer, its mode contexts'
;; bindings first, an app's own keys among them, then the global ones; keys running one command share a row,
;; with the command and its description beside them, the description
;; wrapped in its column.  The listing is a local read-only buffer,
;; <keys>, browsed like any other; C-x TAB pages it down from anywhere,
;; and back to the top past the end, and it follows the active window.

(import (only (foundation edoc) elibrary))
(elibrary (apps keys)
  (export (rename (keys-hide! hide!)) init! (rename (keys-open! open!)) (rename (keys-page-up! page-up!))
          (rename (keys-show! show!)))
  (import (rnrs)
          (only (chezscheme) format iota list-head quotient void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (sys glyph) glyph:))

  (define view #f) ; the <keys> buffer while it is shown
  (define listed #f) ; (buffer contexts read-only?) the listing describes
  (define swept? #f) ; whether the listings an older checkpoint restored have been dropped

  (define (stale-listing? b)
    ;; a <keys> or <keys 2> local buffer that is not the view: one an older
    ;; checkpoint brought back as text
    (let ([name (head:buffer-name b)])
      (and (not (eq? b view)) (not (head:buffer-store-id b))
           (string:prefix? "<keys" name) (string:suffix? ">" name))))

  (define (sweep!)
    ;; the listings an older checkpoint restored go; the view is made fresh
    (for-each (lambda (b) (when (stale-listing? b) (head:forget-buffer! b))) (head:buffers))
    (set! swept? #t))

  ;;; The rows ------------------------------------------------------------------------

  (define (action-procedure action)
    ;; the procedure a key action runs, or #f
    (cond [(procedure? action) action]
          [(keymap:call-action? action) (keymap:call-action-procedure action)]
          [(keymap:prefill-action? action) (keymap:prefill-action-procedure action)]
          [else #f]))

  (define (edits? action)
    ;; whether the command declares (edits): refused where the text is read-only
    (let* ([proc (action-procedure action)] [sigs (and proc (edoc:edoc-of proc))])
      (and (pair? sigs)
           (exists (lambda (f) (and (pair? f) (eq? (car f) 'edits))) (edoc:signature-flags (car sigs))))))

  (define (summary-of action)
    ;; what the procedure a key action runs does, from its documentation
    (let* ([proc (action-procedure action)] [sigs (and proc (edoc:edoc-of proc))])
      (if (pair? sigs) (edoc:signature-summary (car sigs)) "")))

  (define (shadowed? sequence nearer)
    ;; whether a nearer context binds the sequence, or a prefix of it, so
    ;; the key never reaches this binding
    (exists (lambda (context)
              (exists (lambda (n) (keymap:resolved-binding context (list-head sequence n)))
                      (map (lambda (i) (+ i 1)) (iota (length sequence)))))
            nearer))

  (define (context-groups context nearer read-only? . keep)
    ;; keep, when given, admits a binding's action: the commands allowed in a
    ;; prompt for the global section while one is open
    ;; (keys command description) for a context's bindings that work here:
    ;; not shadowed by a nearer context, and not editing where the text is
    ;; read-only; the keys running one command together, groups by their
    ;; first key; a lambda shows as the anonymous command it is, a name
    ;; being owed
    (define (add key command description groups)
      (let ([hit (find (lambda (g) (string=? (cadr g) command)) groups)])
        (if hit
            (map (lambda (g) (if (eq? g hit) (cons (cons key (car g)) (cdr g)) g)) groups)
            (cons (list (list key) command description) groups))))
    (let loop ([owned (keymap:context-bindings context)] [groups '()])
      (if (null? owned)
          (list-sort (lambda (a b) (string<? (car (car a)) (car (car b))))
                     (map (lambda (g) (cons (list-sort string<? (car g)) (cdr g))) groups))
          (let* ([b (cdr (car owned))] [action (keymap:binding-action b)]
                 [command (and action (keymap:action-text action))])
            (loop (cdr owned)
                  (if (and command
                           (not (shadowed? (keymap:binding-sequence b) nearer))
                           (not (and read-only? (edits? action)))
                           (or (null? keep) ((car keep) action)))
                      (add (keymap:sequence-text (keymap:binding-sequence b)) command (summary-of action) groups)
                      groups))))))

  ;;; The text -------------------------------------------------------------------------

  (define (cells s) (glyph:cells s))

  (define (pad s width)
    (if (>= (cells s) width) s (string-append s (make-string (- width (cells s)) #\space))))

  (define (wrap text width)
    ;; the text as lines of at most width cells, broken at spaces, a word
    ;; wider than the column broken where the column ends
    (define (chop word)
      ;; a word as pieces the column holds
      (let loop ([word word] [out '()])
        (if (<= (cells word) width)
            (reverse (cons word out))
            (let cut ([n (string-length word)])
              (if (or (<= n 1) (<= (cells (substring word 0 n)) width))
                  (loop (substring word n (string-length word)) (cons (substring word 0 n) out))
                  (cut (- n 1)))))))
    (let loop ([words (apply append (map chop (filter (lambda (w) (> (string-length w) 0)) (split-words text))))]
               [line ""] [out '()])
      (cond
        [(null? words) (reverse (if (string=? line "") out (cons line out)))]
        [(string=? line "") (loop (cdr words) (car words) out)]
        [(<= (+ (cells line) 1 (cells (car words))) width)
         (loop (cdr words) (string-append line " " (car words)) out)]
        [else (loop words "" (cons line out))])))

  (define (split-words s)
    (let loop ([i 0] [start 0] [out '()])
      (cond
        [(= i (string-length s)) (reverse (cons (substring s start i) out))]
        [(char=? (string-ref s i) #\space) (loop (+ i 1) (+ i 1) (cons (substring s start i) out))]
        [else (loop (+ i 1) start out)])))

  (define (bracket keys i)
    ;; the margin of a group's row i: a line down the keys sharing the
    ;; command, from the middle of the first key's row to the middle of the
    ;; last's; nothing beside a lone key or a description running on
    (let ([last (- (length keys) 1)])
      (cond [(< last 1) "  "]
            [(= i 0) " ╷"]
            [(< i last) " │"]
            [(= i last) " ╵"]
            [else "  "])))

  (define (section title groups width)
    ;; a heading, then each group's keys down the first column beside its
    ;; command, a long call wrapped at its spaces, and its description
    ;; wrapped in the last column, the keys of a group joined by a line in
    ;; the margin
    (if (null? groups)
        '()
        (let* ([key-width (apply max (map (lambda (g) (apply max (map cells (car g)))) groups))]
               ;; the key column takes what its widest key needs; of what is
               ;; left, the margin and separators apart, the command column
               ;; takes what its widest command needs while the description
               ;; keeps twenty-four cells, and shrinks to eight cells before
               ;; the description shrinks below that
               [room (- width key-width 6)]
               [command-width (min (apply max (map (lambda (g) (cells (cadr g))) groups)) (max 8 (- room 24)))]
               [text-width (max 8 (- room command-width))])
          (cons title
                (apply append
                  (map (lambda (g)
                         (let* ([keys (car g)] [command (wrap (cadr g) command-width)]
                                [text (wrap (caddr g) text-width)]
                                [height (max (length keys) (length command) (length text) 1)])
                           (let loop ([i 0] [out '()])
                             (if (= i height) (reverse out)
                                 (loop (+ i 1)
                                       (cons (string-append
                                               (bracket keys i) (pad (if (< i (length keys)) (list-ref keys i) "") key-width) "  "
                                               (pad (if (< i (length command)) (list-ref command i) "") command-width) "  "
                                               (if (< i (length text)) (list-ref text i) ""))
                                             out))))))
                       groups))))))

  (define (capture-note context)
    ;; a capturing context's other keys go to the app: one row saying so,
    ;; with the toggle and the keys the editor keeps
    (let ([capture (keymap:context-capture context)])
      (if (not capture) '()
          (list (list (list "other keys") "to the app"
                      (format "Every other key goes to the app; ~a keep~a to the editor unless ~a toggles full capture"
                              (string:join (map (lambda (k) (keymap:sequence-text (list k))) (cddr capture)) " and ")
                              (if (= (length (cddr capture)) 1) "s" "")
                              (keymap:sequence-text (list (car capture)))))))))

  (define (read-only-text? b)
    ;; whether an editing command is refused in the buffer: an app's, or one
    ;; read-only outright; a guard deciding per edit does not count
    (or (head:app-buffer? b)
        (let ([guard (head:buffer-read-only b)]) (and guard (not (procedure? guard))))))

  (define (prompt-context)
    ;; the context of an open prompt's content view, or #f
    (let ([body (prompt:content)]) (and body (prompt:content-context body))))

  (define (listing b width)
    ;; the keys that work now: with a prompt open, its content view's
    ;; context, the prompt's keys and the global commands allowed in a
    ;; prompt; else the buffer's mode contexts' bindings, an app's own keys
    ;; among them, then the global ones; each context's keys less those a
    ;; nearer context takes, and less the editing commands where the text
    ;; is read-only, in the width given
    (let ([width (max 40 width)] [read-only? (read-only-text? b)] [prompting? (prompt:active?)])
      (let loop ([contexts (if prompting?
                               (append (if (prompt-context) (list (prompt-context)) '()) '(prompt global))
                               (append (mode:key-contexts b) '(global)))]
                 [nearer '()] [out '()])
        (if (null? contexts)
            (apply append (reverse out))
            (let ([context (car contexts)])
              (loop (cdr contexts) (cons context nearer)
                    (cons (section (if (eq? context 'global) "Global keys" (format "~a keys" context))
                                   (append (if (and prompting? (eq? context 'global))
                                               (context-groups context nearer #f prompt:allowed?)
                                               (context-groups context nearer (and (not prompting?) read-only?)))
                                           (capture-note context))
                                   width)
                          out)))))))

  (define (heading? line)
    ;; a section title: a line that is not a row, rows starting with two spaces
    (and (> (string-length line) 0) (not (char=? (string-ref line 0) #\space))))

  (define (styles line)
    ;; the section titles in bold, the grouping line in the margin faint
    (let ([v (make-vector (string-length line) (if (heading? line) 'bold 'plain))])
      (when (and (> (string-length line) 1) (memv (string-ref line 1) '(#\╷ #\│ #\╵)))
        (vector-set! v 1 'chrome))
      v))

  ;;; The buffer in the pop-up ------------------------------------------------------------

  (define (view-windows)
    ;; the windows showing the listing, the pop-up first when it does
    (let ([ws (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows))])
      (if (memq (head:popup) ws) (cons (head:popup) (remq (head:popup) ws)) ws)))

  (define (showing?) (and view (memq view (head:buffers)) (pair? (view-windows)) #t))

  (define (describable? w)
    ;; a window the listing can describe: one showing something other than
    ;; the listing, the pop-up only while it shows
    (and w (not (eq? (head:window-buffer w) view))
         (not (and (head:popup? w) (= (head:popup-rows) 0)))))

  (define (subject)
    ;; the window whose keys the listing describes: the current one unless
    ;; it shows the listing, then the one selected before it, else none
    (let ([w (head:current-window)] [p (head:previous-window)])
      (cond [(describable? w) w]
            [(describable? p) p]
            [else #f])))

  (define (listing-width)
    ;; the narrowest window showing the listing, a cell short of its edge so
    ;; no line wraps; the screen's width before any shows it, or while the
    ;; windows are not yet tiled and report no width to speak of
    (let* ([ws (view-windows)]
           [narrowest (if (null? ws) 0 (apply min (map head:window-content-width ws)))])
      ;; the screen's width less the listing's scrollbar column
      (- (if (> narrowest 40) narrowest (- (paint:screen-cols) 1)) 1)))

  (define (situation b)
    ;; what the listing depends on: the buffer, its contexts, its text being
    ;; read-only, an open prompt with its content's context, and the width
    ;; it is laid out for, which a resize of the terminal changes; the width
    ;; comes last, so the rest compares on its own
    (list b (mode:key-contexts b) (read-only-text? b) (prompt:active?) (prompt-context) (listing-width)))

  (define (fill! b)
    ;; the listing for a buffer into the view: from the top for a new
    ;; subject, in place when only the width changed, a resize say
    (let* ([now (situation b)]
           [same? (and listed (equal? (list-head listed 5) (list-head now 5)))]
           [lines (let ([lines (listing b (listing-width))]) (if (null? lines) (list "no keys") lines))])
      (set! listed now)
      (head:buffer-read-only-set! view #f)
      (head:buffer-lines-set! view (list->vector lines))
      (head:buffer-read-only-set! view #t)
      (for-each (lambda (w)
                  (let ([top (if same? (min (head:window-top w) (- (length lines) 1)) 0)])
                    (head:window-top-set! w top) (head:window-prow-set! w top) (head:window-pcol-set! w 0)))
                (view-windows))))

  (define (ensure-view!)
    ;; the <keys> buffer, made fresh when none is live
    (unless (and view (memq view (head:buffers)))
      (sweep!)
      (set! view (head:new-local-buffer! "keys"))
      ;; transient: a checkpoint keeps no listing, so a restart brings none back
      (head:buffer-fact-set! view 'resume-kind 'keys)
      ;; long, and read by position: a scrollbar on the configured side
      (head:buffer-fact-set! view 'scrollbar #t)
      (head:add-buffer! view)
      (mode:choose! "keys" view)))

  (define (drop-view!)
    (when (and view (memq view (head:buffers))) (head:forget-buffer! view))
    (set! view #f)
    (set! listed #f))

  (define (follow!)
    ;; the listing keeps to the active window's buffer and its mode; the view
    ;; goes once no window shows it, the pop-up cleared by its ↓ say
    (unless swept? (sweep!))
    (when (and view (memq view (head:buffers)))
      (if (null? (view-windows))
          (drop-view!)
          (let* ([w (subject)] [b (and w (head:window-buffer w))])
            (when (and b (not (eq? b view)) (not (equal? listed (situation b))))
              (fill! b))))))

  (define (page! direction)
    ;; the listing a page further where it shows, the pop-up first: down,
    ;; from the top again past the end; up, from the last page again past
    ;; the top
    (let* ([w (car (view-windows))] [n (vector-length (head:buffer-lines view))]
           [size (max 1 (if (head:popup? w) (head:popup-rows) (head:window-size w)))]
           [top (+ (head:window-top w) (* direction size))]
           [top (cond [(>= top n) 0]
                      [(< top 0) (* size (quotient (max 0 (- n 1)) size))]
                      [else top])])
      (head:window-top-set! w top)
      (head:window-prow-set! w top)
      (head:window-pcol-set! w 0)))

  (define (page-down!) (page! 1))

  (edoc "Show the keys that work in the active window's buffer in the pop-up, window 0, as the read-only buffer <keys>: its mode contexts' bindings, an app's own keys among them, then the global ones, keys running one command sharing a row with the command and what it does; shown already, in the pop-up or a window, page it down there, and from the top again past the end. The listing follows the active window.")
  (define (keys-show!)
    (cond
      [(showing?)
       ;; the situation changed under the listing, a prompt opened say: it
       ;; refills; unchanged, or with nothing else to describe, it pages
       (let* ([w (subject)] [b (and w (head:window-buffer w))])
         (if (and b (not (eq? b view)) (not (equal? listed (situation b))))
             (fill! b)
             (page-down!)))]
      [else
       (let ([b (head:window-buffer (or (subject) (head:current-window)))])
         (ensure-view!)
         (head:set-window-buffer! (head:popup) view)
         (fill! b)
         (head:show-popup! (head:popup-default-rows)))]))

  (edoc "Page the keys listing up where it shows, from the last page again past the top; not shown, show it as C-x TAB does.")
  (define (keys-page-up!)
    (if (showing?) (page! -1) (keys-show!)))

  (edoc "Show the keys listing in the current window as the read-only buffer <keys>, for the buffer the window shows now; the listing follows the active window from then on, and C-x TAB and C-x S-TAB page it there.")
  (define (keys-open!)
    (let ([b (head:current-buffer)])
      (ensure-view!)
      (head:show-buffer! view)
      (unless (eq? b view) (fill! b))))

  (edoc "Put the key listing away: the pop-up shows its placeholder again and hides, and a window showing the listing shows another buffer.")
  (define (keys-hide!)
    (when (and view (eq? (head:window-buffer (head:popup)) view)) (head:hide-popup!))
    (drop-view!))

  (define (hint b active?)
    ;; the listing's paging keys on its bar, in every window showing it,
    ;; selected or not
    (and view (eq? b view) "C-x TAB page down, C-x S-TAB page up"))

  (edoc "Install the keys helper: its mode, C-x TAB and C-x S-TAB showing or paging the listing, its status hint, the listing following the active window before every frame, and its exclusion from checkpoints.")
  (define (init!)
    (mode:register! "keys" '() '() styles #f #f)
    (head:register-resume! 'keys (lambda (b positions) (values #f positions)) (lambda args #f))
    (keymap:bind-default! "C-x TAB" keys-show!)
    (keymap:bind-default! "C-x S-TAB" keys-page-up!)
    ;; both work everywhere, inside a prompt too, where the listing is the prompt's keys
    (prompt:allow! keys-show!)
    (prompt:allow! keys-page-up!)
    (paint:add-buffer-status-hint! hint)
    (head:add-pre-redraw-hook! follow!)))
