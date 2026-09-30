;; markdown.sls -- a read-only Markdown viewer for the e editor.
;;
;; An e extension module: the library (markdown), loaded at startup by
;; the kernel, which calls init!.  Renders Markdown as formatted text:
;; emphasis markers are stripped and their text wears the face instead,
;; headings take level faces, soft line breaks inside a paragraph
;; disappear (the window's word wrap lays prose out), tables align
;; their columns, fenced code sits between two rules, and
;; [text](url) shows only the text -- the target lives in the buffer's
;; hyperlink layer, followed with RET or a mouse click.
;;
;; markdown:view! shows a local presentation of a source buffer;
;; markdown:edit! returns to that source without replacing its text.
;; Both try to keep the cursor on the matching content.  C-c v toggles
;; in either mode. Apps can request a companion without changing focus;
;; markdown:view-install! also renders literal input into local views.

(import (only (foundation edoc) elibrary))
(elibrary (apps markdown)
  (export (rename (markdown-browser browser))
          (rename (source-companion companion) (source-view! companion!))
          (rename (control:copy! copy!) (control:create-view! create-view!))
          (rename (markdown-edit! edit!)) init! (rename (control:move! move!) (markdown-render render)
                                                  (control:scroll! scroll!) (control:select! select!) (control:set-mark! set-mark!))
          (rename (markdown-view! view!)) (rename (markdown-view-install! view-install!))
          (rename (markdown-view-max-width view-max-width)))
  (import (chezscheme)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation markup) markup:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head echo) echo:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head markdown-control) control:)
          (prefix (head markdown-layout) markdown-layout:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head style) style:)
          (prefix (service file) file:)
          (prefix (service log) log:))

  ;;; Faces -------------------------------------------------------------

  (define (register-md-faces!)
    (style:set! 'md-h1 '(bold underline))
    (style:set! 'md-h2 '(bold))
    (style:set! 'md-h3 '(bold italic))
    (style:set! 'md-h4 '(italic))
    (style:set! 'md-quote '(italic (foreground bright-black)))
    (style:set! 'md-link '(underline (foreground 33)))
    (style:set! 'md-code '(reset)))

  (edoc "The reading width cap: a view in a wider window wraps at this many columns."
        (value integer))
  (define markdown-view-max-width ;; Reading width cap: a view in a wider window wraps at this many
    ;; columns instead of the full width.
    (make-parameter 80
      (lambda (columns)
        (unless (and (fixnum? columns) (>= columns 20))
          (error 'markdown-view-max-width
                 "must be an integer of at least 20" columns))
        columns)))

  (edoc "The command handed a web link's URL when a link is followed."
        (value string))
  (define markdown-browser ;; The command handed a web link's quoted URL.
    (make-parameter "xdg-open"
      (lambda (command)
        (unless (and (string? command) (> (string-length command) 0))
          (error 'markdown-browser "must be a nonempty command" command))
        command)))

  (edoc
    "Render Markdown source as parallel text, style, link and source-row lists. Parsing is width independent; fitting and syntax faces belong to the head."
    (source-lines (list-of string) "Markdown source")
    (width*
      (list-of integer)
      "at most one width, 79 by default"))
  (define (markdown-render source-lines . width*)
    (let-values ([(text styles links rows anchors)
                  (markdown-layout:render (markup:parse source-lines) (if (pair? width*) (car width*) 79))])
      (values text styles links rows)))
  ;;; The mode and the toggle --------------------------------------------

  ;; Inputs and derived rendering are local buffer facts.  The cache
  ;; is plain data, so a new module instance can read its old row map
  ;; while rebuilding styles and text.  A fresh renderer token makes
  ;; implementation changes invalidate the cache like width or input.
  ;; #(styles links source-rows source-lines width measure renderer input revision)
  (define renderer-token (gensym "markdown-renderer"))
  (define (rendering-of b)
    (and (not (head:buffer-store-id b))
         (head:buffer-fact b 'markdown-rendering #f)))
  (define (rendering-styles r) (vector-ref r 0))
  (define (rendering-links r) (vector-ref r 1))
  (define (rendering-rows r) (vector-ref r 2))
  (define (rendering-lines r) (vector-ref r 3))
  (define (rendering-width r) (vector-ref r 4))
  (define (rendering-measure r) (vector-ref r 5))
  (define (rendering-renderer r) (vector-ref r 6))
  (define (rendering-input r) (vector-ref r 7))
  (define (rendering-revision r) (vector-ref r 8))

  (define (view-row-styles source row line)
    (let ([r (mode:source-fact source 'markdown-rendering #f)])
      (and r (<= 0 row) (< row (vector-length (rendering-styles r)))
           (vector-ref (rendering-styles r) row))))

  (define (view-row-links b row line)
    (let ([r (rendering-of b)])
      (if (and r (<= 0 row) (< row (vector-length (rendering-links r))))
          (vector-ref (rendering-links r) row)
          '())))

  (define (render-width b)
    ;; Fit tables to the narrowest window showing the buffer -- one
    ;; rendering serves them all -- under the reading-width cap; the
    ;; fallback matches the renderer's own default.
    (let ([width (head:buffer-narrowest-width b)])
      (min (markdown-view-max-width)
           (if width (max 20 width) 79))))

  (define (source-row-at r row)
    (let ([rows (rendering-rows r)])
      (if (zero? (vector-length rows))
          0
          (vector-ref rows (max 0 (min row (- (vector-length rows) 1)))))))

  (define (view-row-showing r source-row)
    ;; Prefer the first rendered row for the closest source row.  A
    ;; wrapped table cell can produce several rows from the same input.
    (let ([rows (rendering-rows r)])
      (let find ([k 0] [best 0] [closest -1])
        (cond [(>= k (vector-length rows)) best]
              [(and (<= (vector-ref rows k) source-row)
                    (> (vector-ref rows k) closest))
               (find (+ k 1) k (vector-ref rows k))]
              [else (find (+ k 1) best closest)]))))

  (define (render-input b)
    (and (not (head:buffer-store-id b))
         (head:buffer-fact b 'markdown-input #f)))

  (define (refresh-render! b)
    (let ([input (render-input b)] [old (rendering-of b)])
      (when input
        (let* ([source (and (head:buffer? input) input)]
               [basis (and old (eq? source (rendering-input old)) (rendering-revision old))]
               [width (render-width b)] [measure (markdown-view-max-width)])
          (let-values ([(lines revision changes)
                        (if source (head:snapshot-since source basis)
                            (values (list->vector input) #f #f))])
            (unless (and old
                         (eq? renderer-token (rendering-renderer old))
                         (eq? source (rendering-input old))
                         (eqv? revision (rendering-revision old))
                         (equal? lines (rendering-lines old))
                         (= width (rendering-width old))
                         (= measure (rendering-measure old)))
              ;; The renderer maps rows, not source columns: markup and
              ;; joined paragraphs make those different coordinate spaces.
              ;; Follow each source row's start through the complete chain;
              ;; keep the view column separately and clamp it on adoption.
              (let* ([deltas (and changes (map caddr changes))]
                     [anchor
                      (lambda (row col)
                        (let* ([p (cons (source-row-at old row) 0)]
                               [p (if deltas (fold-left text:rebase-position p deltas) p)])
                          (cons (max 0 (min (car p) (- (vector-length lines) 1))) col)))]
                     [anchors
                      (if old
                          (map (lambda (entry) (cons (car entry) (anchor (cadr entry) (cddr entry))))
                            (head:buffer-placements b))
                          '())])
                (let-values ([(text styles links rows) (markdown-render (vector->list lines) width)])
                  (let ([r (vector (list->vector styles) (list->vector links) (list->vector rows)
                                   lines width measure renderer-token source revision)])
                    (head:view-replace! b text
                      (list (cons 'wrap (cons 'clean measure)) (cons 'markdown-rendering r))
                      (map (lambda (entry)
                             (cons (car entry) (cons (view-row-showing r (cadr entry)) (cddr entry))))
                           anchors)))))))))))

  (define (refit-views!)
    ;; Width, reading measure, and source text are inputs to the same
    ;; renderer.  The local facts also rediscover views after reload.
    (for-each
      (lambda (b)
        (when (and (render-input b) (head:buffer-window-size b))
          (refresh-render! b)))
      (head:buffers)))

  (edoc "Install Markdown lines as the input of a local view buffer: read-only, in markdown-view mode and rendered now."
        (b buffer "a local buffer")
        (lines (list-of string) "the Markdown lines")
        (returns buffer) (public))
  (define (markdown-view-install! b lines)
    ;; Literal input belongs to an existing local view.
    ;; Rendering can never replace a shared buffer's source text.
    (unless (and (head:buffer? b) (not (head:buffer-store-id b)))
      (error 'markdown-view-install! "expected a local buffer" b))
    (unless (and (list? lines) (for-all string? lines))
      (error 'markdown-view-install! "expected markdown lines" lines))
    (head:buffer-fact-set! b 'markdown-input lines)
    (head:buffer-read-only-set! b #t)
    (mode:choose! "markdown-view" b)
    (refresh-render! b)
    b)

  (define (attach-source-view! b)
    (head:buffer-fact-set! b 'resume-kind 'markdown)
    (head:register-view! b (lambda () (refresh-render! b)))
    (mode:choose! "markdown-view" b)
    b)

  (edoc "The local view buffer rendering a Markdown source buffer, or #f."
        (source buffer "the source buffer")
        (returns (or buffer #f)))
  (define (source-companion source)
    (and (head:buffer? source)
         (find (lambda (b) (eq? (render-input b) source)) (head:buffers))))

  (edoc "The local companion view of a Markdown buffer, created when there is none, under the given name or *markdown NAME*."
        (source buffer "a buffer in markdown mode")
        (name string "a preferred buffer name for a new view")
        (returns buffer))
  (define source-view!
    ;; A source record is the identity, never its mutable label.  The
    ;; relationship belongs only to the local companion, not the store.
    ;; Apps can supply a preferred local label without selecting a window.
    (case-lambda
      [(source) (source-view-named! source #f)]
      [(source name)
       (unless (and (string? name) (> (string-length name) 0))
         (error 'companion! "expected a buffer name" name))
       (source-view-named! source name)]))
  (define (source-view-named! source name)
    (unless (equal? (mode:name-of source) "markdown")
      (error 'companion! "not a markdown buffer" source))
    (head:add-buffer! source)
    (let ([b (or (source-companion source)
                 (head:new-local-buffer!
                   (or name (format "*markdown ~a*" (head:buffer-name source)))))])
      (head:buffer-fact-set! b 'markdown-input source)
      (attach-source-view! b)
      (refresh-render! b)
      b))

  (define (capture-resume b positions)
    (let ([source (render-input b)] [r (rendering-of b)])
      (if (and (head:buffer? source) (head:buffer-store-id source) r)
          (values (list (head:buffer-store-id source) (rendering-revision r) (head:buffer-name b))
            (map (lambda (entry) (cons (car entry) (cons (source-row-at r (cadr entry)) (cddr entry)))) positions))
          (values #f positions))))

  (define (restore-resume reference positions)
    ;; Source row starts follow edits; view columns retain their separate
    ;; meaning. Rendering at this head's width never becomes shared text.
    (apply
      (lambda (id revision name)
        (let-values ([(source anchors)
                      (head:resume-source! id revision
                        (map (lambda (entry) (cons (car entry) (cons (cadr entry) 0))) positions))])
          (if (and source (equal? (mode:name-of source) "markdown"))
              (let* ([b (source-view! source name)] [r (rendering-of b)])
                (values b
                  (map (lambda (anchor position)
                         (cons (car anchor) (cons (view-row-showing r (cadr anchor)) (cddr position))))
                    anchors positions)))
              (values #f positions)))) reference))

  (edoc "Show the rendered view of a Markdown buffer, the current one by default, in this window at the corresponding row."
        (b* (list-of buffer) "the source buffer, at most one"))
  (define (markdown-view! . b*)
    ;; Show a local companion in this window; other windows can keep
    ;; editing the original source at the same time.
    (let ([source (if (pair? b*) (edoc:type-value 'buffer (car b*)) (head:current-buffer))])
      (head:call-with-display-update
        (lambda ()
          (let ([row (car (head:buffer-point source))]
                [b (source-view! source)])
            (head:show-buffer! b)
            (refresh-render! b)
            (head:goto! (cons (view-row-showing (rendering-of b) row) 0)))))
      (void)))

  (edoc "Return from a Markdown view to its live source buffer."
        (b* (list-of buffer) "the view buffer, at most one"))
  (define (markdown-edit! . b*)
    ;; Return to the live source, without restoring any old snapshot or
    ;; changing its mode, read-only state, file facts, or undo history.
    (let ([b (if (pair? b*) (edoc:type-value 'buffer (car b*)) (head:current-buffer))])
      (unless (equal? (mode:name-of b) "markdown-view")
        (error 'markdown-edit! "not a markdown view" b))
      (let ([source (render-input b)])
        (unless (and (head:buffer? source) (memq source (head:buffers)))
          (error 'markdown-edit! "no live markdown source" b))
        (head:call-with-display-update
          (lambda ()
            (refresh-render! b)
            (let* ([r (rendering-of b)] [row (source-row-at r (car (head:buffer-point b)))])
              (head:show-buffer! source)
              ;; Mounting the source can acquire newer text. Carry the
              ;; rendered row through that adoption before placing point.
              (let-values ([(lines revision changes) (head:snapshot-since source (vector-ref r 8))])
                (head:goto! (fold-left (lambda (p change) (text:rebase-position p (caddr change)))
                              (cons row 0) (or changes '()))))))))
      (void)))

  (define (forget-render! b)
    ;; Killing a source closes its dependent presentations; killing a
    ;; presentation leaves its source alone.  All companions are local.
    (for-each
      (lambda (view)
        (when (eq? (render-input view) b) (head:forget-buffer! view)))
      (head:buffers)))

  ;;; Following links ------------------------------------------------------

  (define (shell-quoted url)
    (string-append
      "'"
      (apply string-append
             (map (lambda (c)
                    (if (char=? c #\') "'\\''" (string c)))
                  (string->list url)))
      "'"))

  (define (link-at-point)
    (let* ([b (head:current-buffer)]
           [pt (head:point)]
           [links (view-row-links b (car pt) #f)])
      (find (lambda (l) (and (<= (car l) (cdr pt)) (< (cdr pt) (cadr l))))
            links)))

  (define (markdown-file? path)
    (or (string:suffix? ".md" path) (string:suffix? ".markdown" path)))

  (define (open-link! url)
    ;; Followed links log under this function; web links go to
    ;; the configured browser command.
    (cond
      [(or (string:prefix? "http://" url)
           (string:prefix? "https://" url))
       (system (format "~a ~a >/dev/null 2>&1 &"
                       (markdown-browser) (shell-quoted url)))
       (log:add! 'markdown:open-link! (format "Opened ~a" url))]
      [(string:prefix? "#" url)
       (log:add! 'markdown:open-link! "Anchor links are not followed yet")]
      [else
       (let* ([b (head:current-buffer)]
              [input (render-input b)]
              [base (head:buffer-file (if (head:buffer? input) input b))]
              [dir (if base (or (file:directory-part base) "") "")]
              [path (file:expand url)]
              [target (if (string:prefix? "/" path) path
                          (string-append dir path))])
         (edit:visit-file! target)
         ;; a linked markdown document arrives already formatted
         (when (and (markdown-file? url)
                    (equal? (mode:name-of (head:current-buffer))
                            "markdown"))
           (guard (ex [else (void)]) (markdown-view!)))
         (log:add! 'markdown:open-link! (format "Followed ~a" url)))]))

  (define (follow-md-link!)
    (let ([link (link-at-point)])
      (if link
          (open-link! (caddr link))
          (edit:set-message! "No link at point"))))

  (define (click-md-link!)
    ;; The click already placed point; only an actual link acts.
    (let ([link (link-at-point)])
      (when link (open-link! (caddr link)))))

  ;; A transient, unlogged echo hint while point rests on a link,
  ;; worn like a prompt label.  Only cursor motion updates it, so
  ;; command feedback in the echo area stays until the user moves.
  (define hint-point #f)
  (define hint-shown #f)

  (define (link-hint)
    (unless (or (prompt:active?) (equal? hint-point (head:point)))
      (set! hint-point (head:point))
      (let ([link (and (equal? (mode:name-of (head:current-buffer))
                               "markdown-view")
                       (link-at-point))])
        (cond
          [link
           (let ([url (caddr link)])
             (unless (equal? hint-shown url)
               (set! hint-shown url)
               (paint:show-prompt-message! "hyperlink: " url #f)))]
          [hint-shown
           (when (equal? (echo:text)
                         (string-append "hyperlink: " hint-shown))
             (paint:show-message! "" #f))
           (set! hint-shown #f)])))
    '())

  (edoc "Install Markdown viewing: its faces, mode, links, highlighter, hooks and session resume, and its describe entries and bindings." (public))
  (define (init!)
    (control:register! edit:copy-text!)
    (register-md-faces!)
    (mode:register! "markdown-view" '() '() (lambda (line) #f)
                    #f view-row-styles '(markdown-rendering))
    (paint:add-hyperlinker! view-row-links)
    (paint:add-highlighter! link-hint)
    (head:add-pre-redraw-hook! refit-views!)
    (head:add-buffer-kill-hook! forget-render!)
    (head:register-resume! 'markdown capture-resume restore-resume)
    ;; Reconstruct callbacks and derived data from local inputs even
    ;; when runtime-created registrations survived module retraction.
    (for-each
      (lambda (b)
        (let ([input (render-input b)])
          (when input
            (when (head:buffer? input) (attach-source-view! b))
            (refresh-render! b))))
      (head:buffers))
    (keymap:bind-default! 'markdown "C-c v" markdown-view!)
    (keymap:bind-default! 'markdown-view "C-c v" markdown-edit!)
    (keymap:bind-default! 'markdown-view "RET" follow-md-link!)
    (keymap:bind-default! 'markdown-view "MOUSE-CLICK" click-md-link!)))
