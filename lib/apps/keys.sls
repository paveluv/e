;; keys.sls -- the key bindings helper: C-x TAB lists in the pop-up the
;; keys that work in the current buffer, the app's own keys first as it
;; declares them under its keys fact, then its mode contexts' bindings,
;; then the global ones, each with the command it runs and what that does;
;; pressed again it turns the page, and past the last page it puts the
;; pop-up away.

(import (only (foundation edoc) elibrary))
(elibrary (apps keys)
  (export (rename (keys-hide! hide!)) init! (rename (keys-show! show!)))
  (import (rnrs)
          (only (chezscheme) format list-head quotient void)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:))

  (define view #f) ; the <keys> view while it is shown
  (define lines '()) ; every line of the listing
  (define page 0) ; the page the pop-up shows

  ;;; The listing -------------------------------------------------------------------

  (define (summary-of action)
    ;; what the procedure a key action runs does, from its documentation
    (let* ([proc (cond [(procedure? action) action]
                       [(keymap:call-action? action) (keymap:call-action-procedure action)]
                       [(keymap:prefill-action? action) (keymap:prefill-action-procedure action)]
                       [else #f])]
           [sigs (and proc (edoc:edoc-of proc))])
      (if (pair? sigs) (edoc:signature-summary (car sigs)) "")))

  (define (context-rows context)
    ;; (key command description) for a context's live bindings, by key
    (list-sort (lambda (a b) (string<? (car a) (car b)))
      (fold-right
        (lambda (owned out)
          (let* ([b (cdr owned)] [action (keymap:binding-action b)])
            (if action
                (cons (list (keymap:sequence-text (keymap:binding-sequence b)) (keymap:action-text action) (summary-of action))
                      out)
                out)))
        '() (keymap:context-bindings context))))

  (define (pad s width)
    (if (>= (string-length s) width) s (string-append s (make-string (- width (string-length s)) #\space))))

  (define (cut s width)
    ;; a line cut at the right to the width, its end marked
    (if (> (string-length s) width) (string-append (substring s 0 (max 0 (- width 1))) "…") s))

  (define (section title rows)
    ;; a heading, then the rows in three aligned columns, each line cut at
    ;; the right to the screen's width
    (if (null? rows)
        '()
        (let ([key-width (apply max (map (lambda (r) (string-length (car r))) rows))]
              [command-width (min 40 (apply max (map (lambda (r) (string-length (cadr r))) rows)))]
              [width (max 20 (paint:screen-cols))])
          (cons title
                (map (lambda (r)
                       (cut (string-append "  " (pad (car r) key-width) "  "
                                           (pad (string:elide (cadr r) command-width) command-width) "  " (caddr r))
                            width))
                     rows)))))

  (define (listing b)
    ;; the buffer's own keys, its mode contexts' bindings, then the global ones
    (append (section (format "~a keys" (head:buffer-name b)) (head:buffer-fact b 'keys '()))
            (apply append (map (lambda (context) (section (format "~a keys" context) (context-rows context)))
                               (mode:key-contexts b)))
            (section "Global keys" (context-rows 'global))))

  ;;; The pop-up ----------------------------------------------------------------------

  (define (page-rows) (max 3 (quotient (paint:screen-rows) 2)))

  (define (pages) (max 1 (div (+ (length lines) (page-rows) -1) (page-rows))))

  (define (showing?) (and view (memq view (head:buffers)) (eq? (head:window-buffer (head:popup)) view) #t))

  (define (render!)
    ;; the current page into the view
    (when view
      (let* ([n (page-rows)] [start (min (* page n) (length lines))]
             [shown (list-head (list-tail lines start) (min n (- (length lines) start)))])
        (head:view-replace! view (if (null? shown) (list "no keys") shown)))))

  (define (drop-view!)
    (when (and view (memq view (head:buffers))) (head:forget-buffer! view))
    (set! view #f)
    (set! page 0))

  (define (settle-view!)
    ;; the view is gone once the pop-up shows something else, cleared by its ↓ say
    (when (and view (memq view (head:buffers)) (not (eq? (head:window-buffer (head:popup)) view)))
      (drop-view!)))

  (edoc "List the keys that work in the current buffer in the pop-up, window 0: the buffer's own keys as its app declares them, then its mode contexts' bindings, then the global ones, each with the command it runs and what that does; pressed again it shows the next page, and past the last page it puts the pop-up away.")
  (define (keys-show!)
    (cond
      [(and (showing?) (< (+ page 1) (pages))) (set! page (+ page 1)) (render!)]
      [(showing?) (keys-hide!)]
      [else
       (set! lines (listing (head:current-buffer)))
       (set! page 0)
       (unless (and view (memq view (head:buffers)))
         (set! view (head:register-view! (head:new-local-buffer! "keys") render!))
         (head:set-app-status-position! view
           (lambda (b) (format "keys  page ~a of ~a  C-x TAB turns it, ↓ puts it away" (+ page 1) (pages))))
         (head:set-app-selectable! view #f))
       (render!)
       (head:set-window-buffer! (head:popup) view)
       (head:show-popup! (max 1 (min (length lines) (page-rows))))]))

  (edoc "Put the key listing away: the pop-up shows its placeholder again and hides.")
  (define (keys-hide!)
    (when (showing?) (head:hide-popup!))
    (drop-view!))

  (edoc "Install the keys helper: C-x TAB lists the keys, and the view goes when the pop-up shows something else.")
  (define (init!)
    (keymap:bind-default! "C-x TAB" keys-show!)
    (head:add-pre-redraw-hook! settle-view!)))
