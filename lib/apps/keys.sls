;; keys.sls -- the key bindings helper: C-x TAB shows in the pop-up the
;; keys that work in the active window's buffer, its mode contexts'
;; bindings first, an app's own keys among them, then the global ones; keys running one command share a row,
;; with the command and its description beside them, the description
;; wrapped in its column.  The listing is a local read-only buffer,
;; <keys>, browsed like any other; C-x TAB pages it down from anywhere,
;; and back to the top past the end, and it follows the active window.

(import (only (foundation edoc) elibrary))
(elibrary (apps keys)
  (export (rename (keys-hide! hide!)) init! (rename (keys-show! show!)))
  (import (rnrs)
          (only (chezscheme) format void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:))

  (define view #f) ; the <keys> buffer while it is shown
  (define listed #f) ; (buffer . contexts) the listing describes

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

  (define (pad s width)
    (if (>= (string-length s) width) s (string-append s (make-string (- width (string-length s)) #\space))))

  (define (wrap text width)
    ;; the text as lines of at most width characters, broken at spaces
    (let loop ([words (filter (lambda (w) (> (string-length w) 0)) (split-words text))] [line ""] [out '()])
      (cond
        [(null? words) (reverse (if (string=? line "") out (cons line out)))]
        [(string=? line "") (loop (cdr words) (car words) out)]
        [(<= (+ (string-length line) 1 (string-length (car words))) width)
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
        (let* ([key-width (apply max (map (lambda (g) (apply max (map string-length (car g)))) groups))]
               [command-width (min 40 (apply max (map (lambda (g) (string-length (cadr g))) groups)))]
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

  (define (listing b)
    ;; the buffer's mode contexts' bindings, an app's own keys among them,
    ;; then the global ones
    (let ([width (max 40 (paint:screen-cols))])
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

  (define (subject)
    ;; the window whose keys the listing describes: the current one, or the
    ;; one selected before the pop-up when the pop-up itself is selected
    (let ([w (head:current-window)])
      (if (head:popup? w) (or (head:previous-window) w) w)))

  (define (showing?) (and view (memq view (head:buffers)) (eq? (head:window-buffer (head:popup)) view) #t))

  (define (fill! b)
    ;; the listing for a buffer into the view, from the top
    (set! listed (cons b (mode:key-contexts b)))
    (head:buffer-read-only-set! view #f)
    (head:buffer-lines-set! view (list->vector (let ([lines (listing b)]) (if (null? lines) (list "no keys") lines))))
    (head:buffer-read-only-set! view #t)
    (head:window-top-set! (head:popup) 0)
    (head:window-prow-set! (head:popup) 0)
    (head:window-pcol-set! (head:popup) 0))

  (define (drop-view!)
    (when (and view (memq view (head:buffers))) (head:forget-buffer! view))
    (set! view #f)
    (set! listed #f))

  (define (follow!)
    ;; the listing keeps to the active window's buffer and its mode; the view
    ;; goes once the pop-up shows something else, cleared by its ↓ say
    (when (and view (memq view (head:buffers)))
      (if (not (eq? (head:window-buffer (head:popup)) view))
          (drop-view!)
          (let ([b (head:window-buffer (subject))])
            (unless (or (eq? b view) (and listed (eq? (car listed) b) (equal? (cdr listed) (mode:key-contexts b))))
              (fill! b))))))

  (define (page-down!)
    ;; the pop-up's view a page further, from the top again past the end
    (let* ([w (head:popup)] [n (vector-length (head:buffer-lines view))]
           [size (max 1 (head:popup-rows))]
           [top (+ (head:window-top w) size)]
           [top (if (>= top n) 0 top)])
      (head:window-top-set! w top)
      (head:window-prow-set! w top)
      (head:window-pcol-set! w 0)))

  (edoc "Show the keys that work in the active window's buffer in the pop-up, window 0, as the read-only buffer <keys>: its mode contexts' bindings, an app's own keys among them, then the global ones, keys running one command sharing a row with the command and what it does; shown already, page it down, and from the top again past the end. The listing follows the active window.")
  (define (keys-show!)
    (cond
      [(showing?) (page-down!)]
      [else
       (let ([b (head:window-buffer (subject))])
         (unless (and view (memq view (head:buffers)))
           (set! view (head:new-local-buffer! "keys"))
           (head:add-buffer! view)
           (mode:choose! "keys" view))
         (head:set-window-buffer! (head:popup) view)
         (fill! b)
         (head:show-popup! (head:popup-default-rows)))]))

  (edoc "Put the key listing away: the pop-up shows its placeholder again and hides.")
  (define (keys-hide!)
    (when (showing?) (head:hide-popup!))
    (drop-view!))

  (define (hint)
    (and view (eq? (head:current-buffer) view) "C-x TAB page down"))

  (edoc "Install the keys helper: its mode, C-x TAB showing or paging the listing, its status hint, and the listing following the active window before every frame.")
  (define (init!)
    (mode:register! "keys" '() '() styles #f #f)
    (keymap:bind-default! "C-x TAB" keys-show!)
    (paint:add-status-hint! hint)
    (head:add-pre-redraw-hook! follow!)))
