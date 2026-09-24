;; keys.sls -- the key bindings helper: C-x TAB shows in the pop-up the
;; keys that work in the active window's buffer, its mode contexts'
;; bindings first, an app's own keys among them, then the global ones; keys running one command share a row,
;; with the command and its description beside them, the description
;; wrapped in its column.  The listing is a local read-only buffer,
;; <keys>, browsed like any other; C-x TAB pages it down from anywhere,
;; and back to the top past the end, and it follows the active window.

(import (only (foundation edoc) elibrary))
(elibrary (apps keys)
  (export (rename (keys-hide! hide!)) init! (rename (keys-open! open!)) (rename (keys-show! show!)))
  (import (rnrs)
          (only (chezscheme) format void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (sys glyph) glyph:))

  (define view #f) ; the <keys> buffer while it is shown
  (define listed #f) ; (buffer . contexts) the listing describes
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

  (define (summary-of action)
    ;; what the procedure a key action runs does, from its documentation
    (let* ([proc (cond [(procedure? action) action]
                       [(keymap:call-action? action) (keymap:call-action-procedure action)]
                       [(keymap:prefill-action? action) (keymap:prefill-action-procedure action)]
                       [else #f])]
           [sigs (and proc (edoc:edoc-of proc))])
      (if (pair? sigs) (edoc:signature-summary (car sigs)) "")))

  (define (named? command)
    ;; a command the listing can say anything about: one the top level
    ;; names; an anonymous procedure has neither a name nor a description
    (not (or (string=? command "anonymous command") (string:prefix? "(anonymous command" command))))

  (define (context-groups context)
    ;; (keys command description) for a context's live bindings to named
    ;; commands, the keys running one command together, groups by their
    ;; first key
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
                  (if (and command (named? command))
                      (add (keymap:sequence-text (keymap:binding-sequence b)) command (summary-of action) groups)
                      groups))))))

  ;;; The text -------------------------------------------------------------------------

  (define (cells s) (glyph:cells s))

  (define (pad s width)
    (if (>= (cells s) width) s (string-append s (make-string (- width (cells s)) #\space))))

  (define (wrap text width)
    ;; the text as lines of at most width cells, broken at spaces
    (let loop ([words (filter (lambda (w) (> (string-length w) 0)) (split-words text))] [line ""] [out '()])
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

  (define (section title groups width)
    ;; a heading, then each group's keys down the first column beside its
    ;; command and its description wrapped in the last column
    (if (null? groups)
        '()
        (let* ([key-width (apply max (map (lambda (g) (apply max (map cells (car g)))) groups))]
               [command-width (min 40 (apply max (map (lambda (g) (cells (cadr g))) groups)))]
               [text-width (max 10 (- width key-width command-width 6))])
          (cons title
                (apply append
                  (map (lambda (g)
                         (let* ([keys (car g)] [command (string:elide (cadr g) command-width)]
                                [text (wrap (caddr g) text-width)]
                                [height (max (length keys) (length text) 1)])
                           (let loop ([i 0] [out '()])
                             (if (= i height) (reverse out)
                                 (loop (+ i 1)
                                       (cons (string-append
                                               "  " (pad (if (< i (length keys)) (list-ref keys i) "") key-width) "  "
                                               (pad (if (= i 0) command "") command-width) "  "
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

  (define (listing b width)
    ;; the buffer's mode contexts' bindings, an app's own keys among them,
    ;; then the global ones, in the width given
    (let ([width (max 40 width)])
      (append (apply append
                (map (lambda (context)
                       (section (format "~a keys" context) (append (context-groups context) (capture-note context)) width))
                     (mode:key-contexts b)))
              (section "Global keys" (context-groups 'global) width))))

  (define (heading? line)
    ;; a section title: a line that is not a row, rows starting with two spaces
    (and (> (string-length line) 0) (not (char=? (string-ref line 0) #\space))))

  (define (styles line)
    ;; the section titles in bold
    (make-vector (string-length line) (if (heading? line) 'bold 'plain)))

  ;;; The buffer in the pop-up ------------------------------------------------------------

  (define (view-windows)
    ;; the windows showing the listing, the pop-up first when it does
    (let ([ws (filter (lambda (w) (eq? (head:window-buffer w) view)) (head:windows))])
      (if (memq (head:popup) ws) (cons (head:popup) (remq (head:popup) ws)) ws)))

  (define (showing?) (and view (memq view (head:buffers)) (pair? (view-windows)) #t))

  (define (subject)
    ;; the window whose keys the listing describes: the current one unless
    ;; it shows the listing, then the one selected before it, else none
    (let ([w (head:current-window)] [p (head:previous-window)])
      (cond [(not (eq? (head:window-buffer w) view)) w]
            [(and p (not (eq? (head:window-buffer p) view))) p]
            [else #f])))

  (define (listing-width)
    ;; the narrowest window showing the listing, a cell short of its edge so
    ;; no line wraps; the screen's width before any shows it, or while the
    ;; windows are not yet tiled and report no width to speak of
    (let* ([ws (view-windows)]
           [narrowest (if (null? ws) 0 (apply min (map head:window-content-width ws)))])
      (- (if (> narrowest 40) narrowest (paint:screen-cols)) 1)))

  (define (fill! b)
    ;; the listing for a buffer into the view, shown from the top wherever it is
    (set! listed (cons b (mode:key-contexts b)))
    (head:buffer-read-only-set! view #f)
    (head:buffer-lines-set! view
      (list->vector (let ([lines (listing b (listing-width))]) (if (null? lines) (list "no keys") lines))))
    (head:buffer-read-only-set! view #t)
    (for-each (lambda (w) (head:window-top-set! w 0) (head:window-prow-set! w 0) (head:window-pcol-set! w 0))
              (view-windows)))

  (define (ensure-view!)
    ;; the <keys> buffer, made fresh when none is live
    (unless (and view (memq view (head:buffers)))
      (sweep!)
      (set! view (head:new-local-buffer! "keys"))
      ;; transient: a checkpoint keeps no listing, so a restart brings none back
      (head:buffer-fact-set! view 'resume-kind 'keys)
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
            (when (and b (not (eq? b view))
                       (not (and listed (eq? (car listed) b) (equal? (cdr listed) (mode:key-contexts b)))))
              (fill! b))))))

  (define (page-down!)
    ;; the listing a page further where it shows, the pop-up first, from the
    ;; top again past the end
    (let* ([w (car (view-windows))] [n (vector-length (head:buffer-lines view))]
           [size (max 1 (if (head:popup? w) (head:popup-rows) (head:window-size w)))]
           [top (+ (head:window-top w) size)]
           [top (if (>= top n) 0 top)])
      (head:window-top-set! w top)
      (head:window-prow-set! w top)
      (head:window-pcol-set! w 0)))

  (edoc "Show the keys that work in the active window's buffer in the pop-up, window 0, as the read-only buffer <keys>: its mode contexts' bindings, an app's own keys among them, then the global ones, keys running one command sharing a row with the command and what it does; shown already, in the pop-up or a window, page it down there, and from the top again past the end. The listing follows the active window.")
  (define (keys-show!)
    (cond
      [(showing?) (page-down!)]
      [else
       (let ([b (head:window-buffer (or (subject) (head:current-window)))])
         (ensure-view!)
         (head:set-window-buffer! (head:popup) view)
         (fill! b)
         (head:show-popup! (head:popup-default-rows)))]))

  (edoc "Show the keys listing in the current window as the read-only buffer <keys>, for the buffer the window shows now; the listing follows the active window from then on, and C-x TAB pages it there.")
  (define (keys-open!)
    (let ([b (head:current-buffer)])
      (ensure-view!)
      (head:show-buffer! view)
      (unless (eq? b view) (fill! b))))

  (edoc "Put the key listing away: the pop-up shows its placeholder again and hides, and a window showing the listing shows another buffer.")
  (define (keys-hide!)
    (when (and view (eq? (head:window-buffer (head:popup)) view)) (head:hide-popup!))
    (drop-view!))

  (define (hint)
    (and view (eq? (head:current-buffer) view) "C-x TAB page down"))

  (edoc "Install the keys helper: its mode, C-x TAB showing or paging the listing, its status hint, the listing following the active window before every frame, and its exclusion from checkpoints.")
  (define (init!)
    (mode:register! "keys" '() '() styles #f #f)
    (head:register-resume! 'keys (lambda (b positions) (values #f positions)) (lambda args #f))
    (keymap:bind-default! "C-x TAB" keys-show!)
    (paint:add-status-hint! hint)
    (head:add-pre-redraw-hook! follow!)))
