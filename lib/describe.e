;; describe.e -- the head's reference browser and documentation commands.
;;
;; Corpus queries and fetching are aliases of the base's reference: API.
;; This facade adds completion, local key annotations, and Markdown display.

(library (describe)
  (export init! (rename (describe this) (describe! show!) (describe!! show!!)
                        (describe-at-point! at-point!)
                        (reference:fetch! fetch-data!) (reference:lookup lookup)
                        (reference:entries entries) (reference:browser-url browser-url)))
  (import (chezscheme) (except (edit) init!)
          (prefix (doc) doc:) (prefix (reference) reference:)
          (prefix (prompt) prompt:) (prefix (mode) mode:)
          (prefix (string) string:) (prefix (paint) paint:)
          (prefix (head) head:) (prefix (style) style:)
          (prefix (keymap) keymap:) (prefix (only (markdown) companion companion!) markdown:))

  ;;; Display -------------------------------------------------------------------

  (define (refresh-describe!)
    (let* ([page (reference:page head:ui-actor)]
           [source (and page (head:buffer-of-store-id (car page)))]
           [view (and source (markdown:companion source))])
      (when (and source (or (head:buffer-window-size source)
                            (and view (head:buffer-window-size view))))
        (let ([id (reference:page! head:ui-actor (caddr page)
                    (keymap:command-keys (caddr page)) (cons (car page) (cadr page)))])
          (when id (head:sync-foreign-edits! id))))))

  (define (describe! name)
    (let* ([name (if (string? name) (string->symbol name) name)]
           [id (reference:page! head:ui-actor name (keymap:command-keys name))])
      (if (not id)
          (set-message! (format "No documentation for ~a" name))
          (head:call-with-display-update
            (lambda ()
              (head:sync-foreign-edits! id)
              (let ([source (head:adopt-store-buffer! id)])
                (when source
                  (let ([b (markdown:companion! source "*describe*")])
                    (call-with-buffer b (lambda () (goto-point! '(0 . 0))))
                    (if (pop-up-or-reuse! b)
                        (set-message! "")
                        (set-message! (format "~a: see ~a" name (head:buffer-name b)))))))))))
    (void))

  (define-syntax describe
    (syntax-rules ()
      [(_ name) (describe! 'name)]))

  (define (complete-described-name part)
    ;; Complete against the names that actually have a describe page.
    (let ([seen (make-eq-hashtable)])
      (sort string<?
            (fold-left
              (lambda (names entry)
                (fold-left
                  (lambda (names name)
                    (let ([text (symbol->string name)])
                      (if (or (eq-hashtable-ref seen name #f)
                              (not (string:prefix? part text)))
                          names
                          (begin
                            (eq-hashtable-set! seen name #t)
                            (cons text names)))))
                  names (doc:names entry)))
              '() (reference:entries)))))

  (define (describe!!)
    ;; Prompt for a documented name and display its live describe page.
    (define label "Describe function: ")
    (define (editor-name? text)
      (and (> (string-length text) 0)
           (editor-symbol? (string->symbol text))))
    (define (described-name? text)
      (and (> (string-length text) 0)
           (pair? (reference:lookup (string->symbol text)))))
    (let ([name (parameterize ([prompt:completion-highlight editor-name?]
                               [paint:echo-highlight
                                (paint:prompt-styler
                                  label
                                  (paint:completion-styler described-name?
                                                           editor-name?))])
                  (prompt:read! label complete-described-name))])
      (when (and name (> (string-length name) 0))
        (describe! (string->symbol name))))
    (void))

  ;;; The symbol at point ---------------------------------------------------------

  (define (scheme-delimiter? c)
    (or (char-whitespace? c)
        (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,))))

  (define (symbol-at-point)
    ;; The symbol the cursor is on -- or just after, as at the end of a
    ;; word -- in the current buffer; #f when point is not at one.
    (let* ([b (current-buffer)]
           [p (point)]
           [s (buffer-line b (car p))]
           [n (string-length s)]
           [on? (lambda (i)
                  (and (>= i 0) (< i n)
                       (not (scheme-delimiter? (string-ref s i)))))]
           [col (cond [(on? (cdr p)) (cdr p)]
                      [(on? (- (cdr p) 1)) (- (cdr p) 1)]
                      [else #f])])
      (and col
           (let ([start (let back ([i col]) (if (on? (- i 1)) (back (- i 1)) i))]
                 [end (let fwd ([i col]) (if (on? i) (fwd (+ i 1)) i))])
             (string->symbol (substring s start end))))))

  (define (scheme-buffer?)
    ;; Scheme under any dress: the scheme mode itself and the
    ;; pretty-scheme-* renderings, which draw the same buffer text.
    (let ([m (mode:name-of (current-buffer))])
      (and m (or (string=? m "scheme")
                 (string:prefix? "pretty-scheme" m)))))

  (define (describe-at-point!)
    ;; Describe the symbol the cursor is on -- M-., in Scheme buffers.
    (cond [(not (scheme-buffer?))
           (set-message! "Not a Scheme buffer")]
          [(symbol-at-point) => describe!]
          [else (set-message! "No symbol at point")])
    (void))

  (define (describe-input! text pos)
    ;; M-. at a prompt: describe the symbol at the cursor, or the one
    ;; just before it, trailing spaces skipped -- "vector-sort " M-.
    ;; pops the page for vector-sort while the prompt stays open.
    (define (delim? c)
      (or (char-whitespace? c)
          (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,))))
    (let* ([n (string-length text)]
           [i (min pos n)]
           [end (if (and (< i n) (not (delim? (string-ref text i))))
                    (let fwd ([j i])
                      (if (and (< j n) (not (delim? (string-ref text j))))
                          (fwd (+ j 1))
                          j))
                    (let back ([j i])
                      (if (and (> j 0)
                               (memv (string-ref text (- j 1))
                                     '(#\space #\tab)))
                          (back (- j 1))
                          j)))]
           [start (let back ([j end])
                    (if (and (> j 0)
                             (not (delim? (string-ref text (- j 1)))))
                        (back (- j 1))
                        j))])
      (when (> end start)
        (describe! (string->symbol (substring text start end))))))

  (define (init!)
    ;; Rebind a head callback; selection itself belongs to the store page.
    (head:add-pre-redraw-hook! refresh-describe!)
    (doc:register!
      '(((describe:show!) (("procedure" . "(describe:show! name)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Display every documentation entry for `name` in a read-only Markdown `<describe>` buffer.")
        ((describe:at-point!)
         (("procedure" . "(describe:at-point!)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Display documentation for the symbol at point in the current Scheme buffer.")
        ((describe:show!!) (("procedure" . "(describe:show!!)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Prompt for a documented function name with completion, then display its live describe page.")
        ((style:compile) (("procedure" . "(style:compile expression)")) "string"
         ("(edit)") core "Style customization" #f
         "Compile a style expression to terminal SGR parameters. The expression is a list containing attributes (`reset`, `bold`, `dim`, `italic`, `underline`, `blink`, `reverse`, `hidden`, or `strike`) and color clauses `(foreground color)` or `(background color)`; `fg` and `bg` are aliases. A color is a basic name from `black` through `white`, a `bright-` variant, an integer from 0 through 255, or `(rgb red green blue)`.")
        ((style:set!) (("procedure" . "(style:set! face style)")) "void"
         ("(edit)") core "Style customization" #f
         "Override an editor face using a style expression accepted by `compile-style`, a 256-color foreground number, or a raw SGR parameter string. Configuration-owned overrides disappear when their line is removed and config.e is reloaded.")
        ((markdown:view!) (("procedure" . "(markdown:view! [buffer])")) "void"
         ("(md-view)") md-view "Markdown viewing" #f
         "Show a local, read-only companion of a markdown source buffer. Markup strips into faces, paragraphs join, tables align, and fenced code frames. Source text and history stay intact; `C-c v` switches this window between source and view.")
        ((markdown:edit!) (("procedure" . "(markdown:edit! [buffer])")) "void"
         ("(md-view)") md-view "Markdown viewing" #f
         "Return from a markdown companion to its live source at the matching row, preserving the source's text, mode, read-only state, and undo history.")
        ((markdown:view-max-width)
         (("parameter" . "(markdown:view-max-width [columns])"))
         "integer" ("(md-view)") md-view "Markdown viewing" #f
         "Get or set the reading-width cap of markdown views: in a window wider than this many columns, prose and tables wrap at the cap instead of the full width. The default is 80 and the minimum is 20.")
        ((markdown:browser)
         (("parameter" . "(markdown:browser [command])")) "string"
         ("(md-view)") md-view "Markdown viewing" #f
         "Get or set the command that opens a markdown view's web links; it receives the quoted URL as its argument. The default is `xdg-open`.")
        ((answer!!) (("procedure" . "(answer!!)")) "void"
         ("(edit)") core "Interaction" #f
         "Answer the oldest question another actor posed through the interaction protocol (`actor:ask!`): a prompt shows the question with its choices completing on Tab, and the answer routes back to the asker. Bound to `C-c a`; pending questions wait as an echo-area indicator.")
        ((line-numbers!) (("procedure" . "(line-numbers!)")) "void"
         ("(edit)") core "Buffer display" #f
         "Toggle the non-editable line-number gutter for the current buffer. Every window showing that buffer shares the setting. The initial state follows the `line-numbers` configuration parameter.")
        ((focus-window-up!) (("procedure" . "(focus-window-up!)")) "void"
         ("(edit)") core "Window commands" #f
         "Cast a ray upward from point and focus the first window it crosses.")
        ((focus-window-down!) (("procedure" . "(focus-window-down!)")) "void"
         ("(edit)") core "Window commands" #f
         "Cast a ray downward from point and focus the first window it crosses.")
        ((focus-window-left!) (("procedure" . "(focus-window-left!)")) "void"
         ("(edit)") core "Window commands" #f
         "Cast a ray leftward from point and focus the first window it crosses.")
        ((focus-window-right!) (("procedure" . "(focus-window-right!)")) "void"
         ("(edit)") core "Window commands" #f
         "Cast a ray rightward from point and focus the first window it crosses.")
        ((mode:add-extension!)
         (("procedure" . "(mode:add-extension! mode extension)")) "void"
         ("(edit)") core "Mode customization" #f
         "Associate an additional filename extension such as `.foo` with an existing mode such as `scheme`, without replacing that mode's implementation. Configuration-owned associations are reapplied dynamically and disappear when removed from config.e.")
        ((head:register-app!)
         (("procedure" . "(head:register-app! key-or-buffer refresh! [handle-event!])"))
         "buffer" ("(edit)") core "App buffers" #f
         "Create or update a local, read-only head app by stable string key, or attach it to an existing local buffer. Labels are suffixed on collision; renaming keeps the tool identity. The refresh procedure renders current state; an optional event handler receives canonical key, click, and wheel events and returns true when it consumes one. From `MOUSE-CLICK`, `keep-focus` preserves the previously focused window, while `ignore-click` also restores the app's previous point. A view is an app without a handler.")
        ((app-event-buffer-position)
         (("parameter" . "(app-event-buffer-position)")) "pair or #f"
         ("(edit)") core "App buffers" #f
         "During a mouse-click app event, return the unclamped zero-based buffer position addressed by the pointer. The row may be beyond the buffer, allowing apps to distinguish empty viewport space from their last line. Return false outside such an event.")
        ((head:set-app-cursor-visible!)
         (("procedure" . "(head:set-app-cursor-visible! buffer visibility)")) "buffer"
         ("(edit)") core "App buffers" #f
         "Set app cursor visibility to a boolean or a procedure receiving the window token. This supports per-window cursor hiding while an app viewport is detached from its live cursor.")
        ((head:detach-app!)
         (("procedure" . "(head:detach-app! buffer)")) "buffer"
         ("(edit)") core "App buffers" #f
         "Turn an app into an ordinary read-only buffer, preserving its current contents while removing refresh, its event handler, and app presentation.")
        ((set-buffer-wrap!)
         (("procedure" . "(set-buffer-wrap! buffer setting)")) "buffer"
         ("(edit)") core "Buffers" #f
         "Set a buffer-wide wrapping override to #t or #f, or use `default` to follow each window and the global wrapping preference.")
        ((set-buffer-name!)
         (("procedure" . "(set-buffer-name! buffer name)")) "buffer"
         ("(edit)") core "Buffers" #f
         "Rename a buffer, adding the usual numeric suffix when another buffer already uses the requested name.")
        ((head:set-app-presentation!)
         (("procedure" . "(head:set-app-presentation! buffer sticky-lines head:scrollbar [wrap cursor-style])"))
         "buffer" ("(edit)") core "App buffers" #f
         "Configure presentation shared by every window showing an app. `sticky-lines` is a nonnegative count of leading rows fixed above the scrollable body; `scrollbar` is #f, #t, `left`, or `right`; optional `wrap` is #t, #f, or `default`; optional `cursor-style` is `block`, `underline`, `bar`, a `blinking-` variant of those, or `default`.")
        ((head:buffer-window-size)
         (("procedure" . "(head:buffer-window-size buffer)")) "pair or #f"
         ("(edit)") core "App buffers" #f
         "Return `(rows . columns)` for the preferred window displaying `buffer`, choosing the focused window when it displays the buffer, or #f when it is not visible.")
        ((head:add-buffer-kill-hook!)
         (("procedure" . "(head:add-buffer-kill-hook! procedure)")) "unspecified"
         ("(edit)") core "Buffer lifecycle" #f
         "Register a module-owned cleanup procedure called with a buffer immediately before it is killed. Errors are recorded in the log without preventing the kill.")
        ((head:add-shutdown-hook!)
         (("procedure" . "(head:add-shutdown-hook! procedure)")) "unspecified"
         ("(edit)") core "Editor lifecycle" #f
         "Register a module-owned cleanup procedure invoked while e unwinds, before it restores the host terminal. Cleanup errors do not prevent other hooks from running.")
        ((paint:add-buffer-status-hint!)
         (("procedure" . "(paint:add-buffer-status-hint! procedure)")) "unspecified"
         ("(edit)") core "Buffer lifecycle" #f
         "Register a module-owned status hint procedure called as `(procedure buffer active?)` for every window. It may return a string, a `(string . style)` pair, or #f.")
        ((describe:fetch-data!)
         (("procedure" . "(describe:fetch-data!)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Download the TSPL4 and Chez Scheme User's Guide reference pages, rebuild the reference database, and load it. Fetch progress is recorded in the log.")))
    (prompt:inspector describe-input!)
    (keymap:bind-default! "C-h f" describe!!)
    (keymap:bind-default! "M-." describe-at-point!)))
