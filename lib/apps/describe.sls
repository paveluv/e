;; describe.sls -- the head's reference browser and documentation commands.
;;
;; Corpus queries belong to reference:. This facade adds completion, local
;; key annotations, Markdown display, and the interactive fetch command.

(import (only (edoc) elibrary))
(elibrary (describe)
  (export init! (rename (describe this) (describe! show!)
                        (describe-at-point! at-point!)
                        (describe-key! key!)
                        (reference:fetch! fetch-data!)))
  (import (chezscheme)
          (prefix (edit) edit:)
          (prefix (kernel) kernel:)
          (prefix (doc) doc:)
          (prefix (reference) reference:)
          (prefix (prompt) prompt:)
          (prefix (mode) mode:)
          (prefix (string) string:)
          (prefix (paint) paint:)
          (prefix (head) head:)
          (prefix (window) window:)
          (prefix (style) style:)
          (prefix (keymap) keymap:)
          (prefix (only (markdown) companion companion!) markdown:))

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

  (define (top-level-name value)
    ;; the symbol the top level binds to a value, so a procedure written
    ;; literally at M-x, (describe:show! edit:undo!), names itself
    (find (lambda (sym) (and (top-level-bound? sym) (eq? (top-level-value sym) value)))
          (environment-symbols (interaction-environment))))

  (edoc "Show every documentation entry for a name in the read-only Markdown describe buffer, or say there is none."
        (name (or symbol string procedure) "the documented name"))
  (define (describe! name)
    (let* ([name (cond [(string? name) (string->symbol name)]
                       [(symbol? name) name]
                       [else (or (top-level-name name) name)])]
           [id (reference:page! head:ui-actor name (keymap:command-keys name))])
      (if (not id)
          (edit:set-message! (format "No documentation for ~a" name))
          (head:call-with-display-update
            (lambda ()
              (head:sync-foreign-edits! id)
              (let ([source (head:adopt-store-buffer! id)])
                (when source
                  (let ([b (markdown:companion! source "*describe*")])
                    (head:with-buffer b (head:goto! '(0 . 0)))
                    (if (window:pop-up-or-reuse! b)
                        (edit:set-message! "")
                        (edit:set-message! (format "~a: see ~a" name (head:buffer-name b)))))))))))
    (void))

  (edoc "Show the describe page of a name written literally: (describe edit:visit-file!)."
        (name symbol "the name, unquoted"))
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


  ;;; The symbol at point ---------------------------------------------------------

  (define (scheme-delimiter? c)
    (or (char-whitespace? c)
        (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,))))

  (define (symbol-at-point)
    ;; The symbol the cursor is on -- or just after, as at the end of a
    ;; word -- in the current buffer; #f when point is not at one.
    (let* ([b (head:current-buffer)]
           [p (head:point)]
           [s (head:buffer-line b (car p))]
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
    (let ([m (mode:name-of (head:current-buffer))])
      (and m (or (string=? m "scheme")
                 (string:prefix? "pretty-scheme" m)))))

  (edoc "Show the describe page of the symbol under the cursor in a Scheme buffer.")
  (define (describe-at-point!)
    ;; Describe the symbol the cursor is on -- M-., in Scheme buffers.
    (cond [(not (scheme-buffer?))
           (edit:set-message! "Not a Scheme buffer")]
          [(symbol-at-point) => describe!]
          [else (edit:set-message! "No symbol at point")])
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

  ;;; Describing a key ---------------------------------------------------------------

  (define (binding-origin owned)
    (let ([owner (car owned)] [kind (keymap:binding-kind (cdr owned))])
      (cond [(eq? owner 'config) "config.e (user override)"]
            [owner (format "module ~a (~a)" owner kind)]
            [(eq? kind 'default) "built-in default"]
            [else "current session (user override)"])))

  (define (read-described-sequence)
    (let loop ([sequence (list (head:read-key-event #f))])
      (if (keymap:binding-prefix? 'global sequence)
          (begin
            (paint:show-message! (format "Describe key: ~a-" (keymap:sequence-text sequence)) #f)
            (paint:redraw!)
            (loop (append sequence (list (head:read-key-event #f)))))
          sequence)))

  (edoc "Read a key sequence and show in the help buffer what it runs, who bound it and what it shadows."
        (prompts))
  (define (describe-key!)
    (paint:show-message! "Describe key: " #f)
    (paint:redraw!)
    (let* ([sequence (read-described-sequence)]
           [all (keymap:sequence-bindings sequence)]
           [entries (filter
                      (lambda (owned)
                        (eq? (keymap:binding-context (cdr owned)) 'global))
                      all)]
           [resolved (keymap:choose-binding entries)]
           [b (head:fresh-buffer! "*help*")])
      (head:buffer-append! b
        (keymap:sequence-text sequence)
        ""
        (if resolved
            (format "Resolved to: ~a" (keymap:action-text (keymap:binding-action (cdr resolved))))
            "Resolved to: self-insert or undefined")
        "Keymap: global"
        (if resolved
            (format "Defined by: ~a" (binding-origin resolved))
            "Defined by: fallback"))
      (when (> (length entries) 1)
        (head:buffer-append! b "" "Shadowed bindings:")
        (for-each
          (lambda (owned)
            (unless (eq? owned resolved)
              (head:buffer-append! b
                (format "  ~a — ~a"
                        (keymap:action-text (keymap:binding-action (cdr owned)))
                        (binding-origin owned)))))
          entries))
      (let ([contexts
             (fold-left
               (lambda (acc owned)
                 (let ([context (keymap:binding-context (cdr owned))])
                   (if (or (eq? context 'global) (memq context acc))
                       acc
                       (append acc (list context)))))
               '() all)])
        (when (pair? contexts)
          (head:buffer-append! b "" "Contextual bindings:")
          (for-each
            (lambda (context)
              (let ([hit (keymap:resolved-binding context sequence)])
                (when hit
                  (head:buffer-append! b
                    (format "  ~a: ~a — ~a"
                            context
                            (keymap:action-text (keymap:binding-action (cdr hit)))
                            (binding-origin hit))))))
            contexts)))
      (head:buffer-read-only-set! b #t)
      (paint:show-message! "" #f)
      (unless (window:pop-up-or-reuse! b)
        (edit:set-message! "The <help> buffer could not be displayed"))))

  (edoc "Install the describe commands: the page refresh hook, the describe entries of the extension API and the C-h f and C-h k bindings.")
  (define (init!)
    ;; Rebind a head callback; selection itself belongs to the store page.
    (head:add-pre-redraw-hook! refresh-describe!)
    (keymap:bind-default! "C-h k" describe-key!)
    (doc:register!
      '(((describe:show!) (("procedure" . "(describe:show! name)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Display every documentation entry for `name` in a read-only Markdown `<describe>` buffer.")
        ((describe:at-point!)
         (("procedure" . "(describe:at-point!)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Display documentation for the symbol at point in the current Scheme buffer.")
        ((style:compile) (("procedure" . "(style:compile expression)")) "string"
         ("(style)") style "Style customization" #f
         "Compile a style expression to terminal SGR parameters. The expression is a list containing attributes (`reset`, `bold`, `dim`, `italic`, `underline`, `blink`, `reverse`, `hidden`, or `strike`) and color clauses `(foreground color)` or `(background color)`; `fg` and `bg` are aliases. A color is a basic name from `black` through `white`, a `bright-` variant, an integer from 0 through 255, or `(rgb red green blue)`.")
        ((style:set!) (("procedure" . "(style:set! face style)")) "void"
         ("(style)") style "Style customization" #f
         "Override an editor face using a style expression accepted by `style:compile`, a 256-color foreground number, or a raw SGR parameter string. Configuration-owned overrides disappear when their line is removed and config.e is reloaded.")
        ((markdown:view!) (("procedure" . "(markdown:view! [buffer])")) "void"
         ("(markdown)") markdown "Markdown viewing" #f
         "Show a local, read-only companion of a markdown source buffer. Markup strips into faces, paragraphs join, tables align, and fenced code frames. Source text and history stay intact; `C-c v` switches this window between source and view.")
        ((markdown:edit!) (("procedure" . "(markdown:edit! [buffer])")) "void"
         ("(markdown)") markdown "Markdown viewing" #f
         "Return from a markdown companion to its live source at the matching row, preserving the source's text, mode, read-only state, and undo history.")
        ((markdown:view-max-width)
         (("parameter" . "(markdown:view-max-width [columns])"))
         "integer" ("(markdown)") markdown "Markdown viewing" #f
         "Get or set the reading-width cap of markdown views: in a window wider than this many columns, prose and tables wrap at the cap instead of the full width. The default is 80 and the minimum is 20.")
        ((markdown:browser)
         (("parameter" . "(markdown:browser [command])")) "string"
         ("(markdown)") markdown "Markdown viewing" #f
         "Get or set the command that opens a markdown view's web links; it receives the quoted URL as its argument. The default is `xdg-open`.")
        ((mode:add-extension!)
         (("procedure" . "(mode:add-extension! mode extension)")) "void"
         ("(mode)") mode "Mode customization" #f
         "Associate an additional filename extension such as `.foo` with an existing mode such as `scheme`, without replacing that mode's implementation. Configuration-owned associations are reapplied dynamically and disappear when removed from config.e.")
        ((head:register-app!)
         (("procedure" . "(head:register-app! key-or-buffer refresh! [handle-event!])"))
         "buffer" ("(head)") head "App buffers" #f
         "Create or update a local, read-only head app by stable string key, or attach it to an existing local buffer. Labels are suffixed on collision; renaming keeps the tool identity. The refresh procedure renders current state; an optional event handler receives canonical key, click, and wheel events and returns true when it consumes one. From `MOUSE-CLICK`, `keep-focus` preserves the previously focused window, while `ignore-click` also restores the app's previous point. A view is an app without a handler.")
        ((head:set-app-cursor-visible!)
         (("procedure" . "(head:set-app-cursor-visible! buffer visibility)")) "buffer"
         ("(head)") head "App buffers" #f
         "Set app cursor visibility to a boolean or a procedure receiving the window token. This supports per-window cursor hiding while an app viewport is detached from its live cursor.")
        ((head:detach-app!)
         (("procedure" . "(head:detach-app! buffer)")) "buffer"
         ("(head)") head "App buffers" #f
         "Turn an app into an ordinary read-only buffer, preserving its current contents while removing refresh, its event handler, and app presentation.")
        ((head:set-app-presentation!)
         (("procedure" . "(head:set-app-presentation! buffer sticky-lines head:scrollbar [wrap cursor-style])"))
         "buffer" ("(head)") head "App buffers" #f
         "Configure presentation shared by every window showing an app. `sticky-lines` is a nonnegative count of leading rows fixed above the scrollable body; `scrollbar` is #f, #t, `left`, or `right`; optional `wrap` is #t, #f, or `default`; optional `cursor-style` is `block`, `underline`, `bar`, a `blinking-` variant of those, or `default`.")
        ((head:buffer-window-size)
         (("procedure" . "(head:buffer-window-size buffer)")) "pair or #f"
         ("(head)") head "App buffers" #f
         "Return `(rows . columns)` for the preferred window displaying `buffer`, choosing the focused window when it displays the buffer, or #f when it is not visible.")
        ((head:add-buffer-kill-hook!)
         (("procedure" . "(head:add-buffer-kill-hook! procedure)")) "unspecified"
         ("(head)") head "Buffer lifecycle" #f
         "Register a module-owned cleanup procedure called with a buffer immediately before it is killed. Errors are recorded in the log without preventing the kill.")
        ((head:add-shutdown-hook!)
         (("procedure" . "(head:add-shutdown-hook! procedure)")) "unspecified"
         ("(head)") head "Editor lifecycle" #f
         "Register a module-owned cleanup procedure invoked while e unwinds, before it restores the host terminal. Cleanup errors do not prevent other hooks from running.")
        ((main:shutdown!)
         (("procedure" . "(main:shutdown!)")) "does not return after acceptance"
         ("(main)") main "Editor lifecycle" #f
         "Save shared text and named views through the same path as SIGTERM, then stop the base and every head. Requires an all-buffer head. Ask about local drafts, other heads, terminals, agent sessions and pending interactions; shared unsaved text is saved without a question. No, View, Esc and C-g cancel; View opens the buffers app. New transient work receives a fresh review. A failed pause or save resumes service. The next base restores the snapshot; processes, undo history and local drafts do not survive the stop.")
        ((main:shutdown-on-exit)
         (("thread parameter" . "main:shutdown-on-exit")) "boolean"
         ("(main)") main "Editor lifecycle" #f
         "Default #f: quitting detaches this screen. Set #t to review shutting down the base when this is the last participating head. The base decides atomically; cancelling keeps the last head open. Restricted heads always detach normally.")
        ((paint:add-buffer-status-hint!)
         (("procedure" . "(paint:add-buffer-status-hint! procedure)")) "unspecified"
         ("(paint)") paint "Buffer lifecycle" #f
         "Register a module-owned status hint procedure called as `(procedure buffer active?)` for every window. It may return a string, a `(string . style)` pair, or #f.")
        ((describe:fetch-data!)
         (("procedure" . "(describe:fetch-data!)")) "void"
         ("(describe)") describe "Documentation commands" #f
         "Download the TSPL4 and Chez Scheme User's Guide reference pages, rebuild the reference database, and load it. Fetch progress is recorded in the log.")))
    (prompt:inspector describe-input!)
    (keymap:bind-default! "C-h f" (keymap:prefill describe!))
    (keymap:bind-default! "M-." describe-at-point!)))
