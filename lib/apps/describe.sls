;; describe.sls -- the head's reference browser and documentation commands.
;;
;; Corpus queries belong to reference:. This facade adds completion, local
;; key annotations, Markdown display, and the interactive fetch command.

(import (only (foundation edoc) elibrary))
(elibrary (apps describe)
  (export (rename (describe-at-point! at-point!)) fetch-data! init! (rename (describe-input! input!))
          (rename (describe! show!)) (rename (describe this)))
  (import (chezscheme)
          (prefix (apps markdown) markdown:)
          (prefix (core kernel) kernel:)
          (prefix (foundation string) string:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head style) style:)
          (prefix (head text-source) text-source:)
          (prefix (head widget) widget:)
          (prefix (head window-control) window-control:)
          (prefix (service doc) doc:)
          (prefix (service log) log:)
          (prefix (service reference) reference:)
          (prefix (state store) store:)
          (prefix (state view) view:))

  ;;; Fetching --------------------------------------------------------------------

  (edoc "Ask the base to download the reference corpus, TSPL and CSUG, and rebuild the describe database: the command announces the fetch and returns at once, each page's progress replaces the announcement in the echo area as it comes, and the completion or a failure is announced there; one fetch at a time." (public))
  (define (fetch-data!)
    ;; the base downloads in a worker on this head's behalf: its records are
    ;; this head's, presented as progress while the editor stays free. The
    ;; announcement is the command's own word, spoken before M-x reports a
    ;; result, so it reports none; logging it also delivers the base's first
    ;; records, and every later one replaces the line in place
    (reference:fetch!)
    (parameterize ([log:progress #t])
      (log:add! 'describe:fetch-data! "Fetching the reference corpus..." #t))
    (void))

  ;;; Display -------------------------------------------------------------------

  (define refreshed (make-hashtable equal-hash equal?))
  (define definition-generation 0)
  (define (invalidate-pages!) (set! definition-generation (+ definition-generation 1)))
  (define (refresh-page! id frame)
    (when frame
      (let-values ([(source d inputs) (widget:context id 'current)])
        (let ([context (list (keymap:generation) (doc:entries) definition-generation)])
          (unless (equal? context (hashtable-ref refreshed id #f))
            (hashtable-set! refreshed id context)
            (let ([page (reference:page head:ui-actor (view:source d))])
              (when page (reference:select! head:ui-actor (car page) (cadr page) (caddr page) (keymap:command-keys (caddr page))))))))))
  (define (create-page! owner document commands)
    (let* ([page (view:create! head:ui-actor document 'describe 1 '() '() owner)]
           [body (markdown:create! head:ui-actor page document commands)])
      (view:arrange! head:ui-actor (list (list page 0 (list (list 'body body '(grow 1))) '())) '()) page))

  (define (top-level-name value)
    ;; the symbol the top level binds to a value, so a procedure written
    ;; literally at M-x, (describe:show! edit:undo!), names itself
    (find (lambda (sym) (and (top-level-bound? sym) (eq? (top-level-value sym) value)))
          (environment-symbols (interaction-environment))))

  (define (describe-composed! name host page)
    (let* ([host (or (widget:command-owner host 'auxiliary) (error 'describe "no auxiliary host"))]
           [name (cond [(string? name) (string->symbol name)] [(symbol? name) name] [else (or (top-level-name name) name)])]
           [id (if (null? page) (reference:create! head:ui-actor name (keymap:command-keys name))
                 (let ([old (reference:page head:ui-actor (car page))])
                   (and old (reference:select! head:ui-actor (car old) (cadr old) name (keymap:command-keys name)))))])
      (if (not id) (head:report! (format "No documentation for ~a" name))
        (let* ([root (let climb ([id host]) (let ([parent (view:parent (interaction:snapshot id))]) (if parent (climb parent) id)))]
               [focus (widget:focused host)] [window (widget:invoke! host 'auxiliary)]
               [open (assq 'open-document (widget:commands host))])
          (window-control:open-app! window "describe"
            (lambda (owner commands)
              (create-page! owner id
                (if open (cons (cons 'open (cdr open)) (remp (lambda (c) (eq? (car c) 'open)) commands)) commands)))
            (format "describe:~s" id))
          (when focus (interaction:focus! root focus)))) id))

  (edoc "Show documentation through an explicit composition's auxiliary command, preserving focus, and return its source document. Omitted host uses the focused view's declared host. An optional page updates that existing reference source only."
    (name (or symbol string procedure) "documented name")
    (destination (list-of (or model buffer)) "optional composition, then existing page document") (returns (or buffer #f)))
  (define (describe! name . destination)
    (unless (<= (length destination) 2) (error 'describe! "expected host and optional page"))
    (let ([host (if (pair? destination) (car destination)
                  (let ([focus (widget:focused)]) (and focus (widget:command-owner focus 'auxiliary))))])
      (unless host (error 'describe! "no auxiliary host"))
      (describe-composed! name host (if (pair? destination) (cdr destination) '()))))

  (edoc "Show the describe page of a name written literally: (describe edit:visit-file!)."
        (name symbol "the name, unquoted"))
  (define-syntax describe
    (syntax-rules ()
      [(_ name) (describe! 'name)]))


  (edoc "Describe the symbol at an explicit Scheme editor's caret through its containing composition. The text and point come from acquired editor state."
    (receiver id (view editor)) (id model "editor view"))
  (define (describe-at-point! id)
    (let-values ([(source d inputs) (widget:context id 'current)])
      (let* ([document (view:source d)]
             [name (cond [(assq 'mode (view:options d)) => cdr] [else (store:property document 'mode #f)])]
             [text (text-source:lookup document)] [point (car (view:state d))])
        (if (and name (or (equal? name "scheme") (string:prefix? "pretty-scheme" name)))
            (describe-input! (vector-ref (text-source:lines text) (car point)) (cdr point) id)
            (head:report! "Not a Scheme buffer")))))

  (edoc "Describe the Scheme name at an explicit input caret, without reading a buffer's point."
        (text string "Scheme input") (pos integer "character offset") (host (list-of model) "optional containing composition"))
  (define (describe-input! text pos . host)
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
        (apply describe! (string->symbol (substring text start end)) host))))

  (edoc "Register Describe's visible-page service, documentation and C-h f command." (public))
  (define (init!)
    ;; Rebind a head callback; selection itself belongs to the store page.
    (kernel:add-after-reload-hook! (lambda (name) (invalidate-pages!)))
    (log:subscribe! (lambda (entry presentation)
                      (when (memq (log:component entry) '(reference:run-fetch! reference:begin-fetch!))
                        (invalidate-pages!))))
    (widget:register! 'describe 1
      (append (layout:container 'y)
        (list (cons 'service refresh-page!) (cons 'release (lambda (id) (hashtable-delete! refreshed id))))))
    (doc:register!
      '(((describe:show!) (("procedure" . "(describe:show! name [composition [page]])")) "buffer reference or #f"
         ("(apps describe)") describe "Documentation commands" #f
         "Open an independent Markdown page for `name` through the composition's auxiliary host, preserving focus, and return its source document. Omit the composition to use the focused view's declared host. Supply an existing page document to update that receiver.")
        ((describe:at-point!)
         (("procedure" . "(describe:at-point! editor)")) "void"
         ("(apps describe)") describe "Documentation commands" #f
         "Display documentation for the symbol at an explicit Scheme editor's caret.")
        ((style:compile) (("procedure" . "(style:compile expression)")) "string"
         ("(head style)") style "Style customization" #f
         "Compile a style expression to terminal SGR parameters. The expression is a list containing attributes (`reset`, `bold`, `dim`, `italic`, `underline`, `blink`, `reverse`, `hidden`, or `strike`) and color clauses `(foreground color)` or `(background color)`; `fg` and `bg` are aliases. A color is a basic name from `black` through `white`, a `bright-` variant, an integer from 0 through 255, or `(rgb red green blue)`.")
        ((style:set!) (("procedure" . "(style:set! face style)")) "void"
         ("(head style)") style "Style customization" #f
         "Override an editor face using a style expression accepted by `style:compile`, a 256-color foreground number, or a raw SGR parameter string. Configuration-owned overrides disappear when their line is removed and config.e is reloaded.")
        ((markdown:view!) (("procedure" . "(markdown:view! window [buffer])")) "model"
         ("(apps markdown)") markdown "Markdown viewing" #f
         "Show an independently fitted widget over a Markdown source document. Markup strips into faces, paragraphs join, tables align, and fenced code frames. Source text and history stay intact; `C-c v` switches this window between source and view.")
        ((markdown:edit!) (("procedure" . "(markdown:edit! view)")) "void"
         ("(apps markdown)") markdown "Markdown viewing" #f
         "Ask a Markdown widget's explicit host to return to its source at the matching row, preserving source text and undo history.")
        ((markdown:view-max-width)
         (("parameter" . "(markdown:view-max-width [columns])"))
         "integer" ("(apps markdown)") markdown "Markdown viewing" #f
         "Get or set the reading-width cap of markdown views: in a window wider than this many columns, prose and tables wrap at the cap instead of the full width. The default is 80 and the minimum is 20.")
        ((markdown:browser)
         (("parameter" . "(markdown:browser [command])")) "string"
         ("(apps markdown)") markdown "Markdown viewing" #f
         "Get or set the command that opens a markdown view's web links; it receives the quoted URL as its argument. The default is `xdg-open`.")
        ((mode:add-extension!)
         (("procedure" . "(mode:add-extension! mode extension)")) "void"
         ("(head mode)") mode "Mode customization" #f
         "Associate an additional filename extension such as `.foo` with an existing mode such as `scheme`, without replacing that mode's implementation. Configuration-owned associations are reapplied dynamically and disappear when removed from config.e.")
        ((head:add-shutdown-hook!)
         (("procedure" . "(head:add-shutdown-hook! procedure)")) "unspecified"
         ("(head head)") head "Editor lifecycle" #f
         "Register a module-owned cleanup procedure invoked while e unwinds, before it restores the host terminal. Cleanup errors do not prevent other hooks from running.")
        ((lifecycle:shutdown!)
         (("procedure" . "(lifecycle:shutdown!)")) "does not return after acceptance"
         ("(head lifecycle)") lifecycle "Editor lifecycle" #f
         "Save documents and named compositions through the same path as SIGTERM, then stop the base and every head. Requires an all-buffer head. Review other heads, terminals, agents and pending interactions; unsaved text needs no confirmation. No, View, Esc and C-g cancel; the default screen's View opens Buffet. New transient work receives a fresh review. A failed pause or save resumes service. The next base restores text, retained undo history and persistent views; processes end.")
        ((lifecycle:shutdown-on-exit)
         (("thread parameter" . "lifecycle:shutdown-on-exit")) "boolean"
         ("(head lifecycle)") lifecycle "Editor lifecycle" #f
         "Default #f: quitting detaches this screen. Set #t to review shutting down the base when this is the last participating head. The base decides atomically; cancelling keeps the last head open. Restricted heads always detach normally.")
        ((describe:fetch-data!)
         (("procedure" . "(describe:fetch-data!)")) "void"
         ("(apps describe)") describe "Documentation commands" #f
         "Download the TSPL4 and Chez Scheme User's Guide reference pages, rebuild the reference database, and load it. Fetch progress is recorded in the log.")))
    (keymap:bind-default! 'screen "C-h f" (keymap:prefill describe!))
    (keymap:bind-default! 'widget-editor "M-." (keymap:call describe-at-point! widget:target))
  ))
