;; Markdown is a source presentation. Only these entry points choose windows;
;; the reusable composition opens documents through explicit host commands.
(import (only (foundation edoc) elibrary))
(elibrary (apps markdown)
  (export browser (rename (control:copy! copy!)) create! (rename (control:create-view! create-view!)
                                                           (control:source! edit!) (control:follow! follow!)) init! (rename (control:locate! locate!) (control:move! move!))
          open-link! open-source! render (rename (control:scroll! scroll!) (control:select! select!) (control:set-mark! set-mark!))
          view! (rename (control:width-limit view-max-width)))
  (import (chezscheme) (prefix (foundation markup) markup:) (prefix (foundation string) string:)
          (prefix (head catalogue-host) catalogue-host:) (prefix (head edit) edit:)
          (prefix (head head) head:) (prefix (head keymap) keymap:) (prefix (head layout) layout:)
          (prefix (head markdown-control) control:) (prefix (head markdown-layout) markdown-layout:)
          (prefix (head style) style:) (prefix (head text-source) text-source:)
          (prefix (head widget) widget:) (prefix (head window) window:)
          (prefix (service file) file:) (prefix (service log) log:) (prefix (state store) store:)
          (prefix (state view) view:))
  (define (get r k fallback) (cond [(assq k r) => cdr] [else fallback]))

  (edoc "The command handed a quoted web URL when a Markdown link is followed." (value string))
  (define browser (make-parameter "xdg-open" (lambda (command)
                                               (unless (and (string? command) (> (string-length command) 0))
                                                 (error 'browser "expected a nonempty command")) command)))

  (edoc "Fit Markdown source to a terminal width. Return text, character styles, links and source rows."
        (lines (list-of string) "Markdown source") (width (list-of integer) "optional width, default 79") (public))
  (define (render lines . width)
    (let-values ([(text styles links rows anchors)
                  (markdown-layout:render (markup:parse lines) (if (pair? width) (car width) 79))])
      (values text styles links rows)))

  (edoc "Compose a Markdown document with source/link commands. The host's open command receives a document reference and optional point/presentation preferences. Child text has its own selection and viewport."
        (actor actor "creator") (document buffer "borrowed source") (commands list "explicit host commands")
        (origin (list-of integer) "optional source row") (returns model) (public))
  (define (create! actor document commands . origin)
    (let* ([text (apply control:create-view! actor document origin)]
           [root (view:create! actor #f 'markdown-page 1 (list (cons 'commands commands)) '())])
      (view:arrange! actor
        (list (list root 0 (list (list 'text text '(grow 1))) (list (cons 'commands commands)))
          (list text 0 '() (list (list 'commands (list 'open-uri root 'open-link '()) (list 'open-source root 'open-source '()))))) '()) root))
  (define (check-source! id document)
    (let-values ([(source d inputs) (widget:context (widget:descendant id 'text) 'current)])
      (unless (equal? document (get (get (get source 'value '()) 'details '()) 'document #f))
        (error 'markdown "the presentation's source changed"))))

  (edoc "Open this presentation's source at a reviewed logical position. Rebase through retained edits before asking the explicit host; unavailable history refuses."
        (id model "Markdown page") (document buffer "source identity") (revision integer "shown basis")
        (position pair "source row and character"))
  (define (open-source! id document revision position)
    (check-source! id document)
    (let* ([source (text-source:open! head:ui-actor document revision)]
           [points (and source (text-source:rebase (list position) (text-source:changes source revision (text-source:revision source))))])
      (unless points (error 'open-source! "source position is unavailable"))
      (widget:invoke! id 'open document (list (cons 'point (car points))))))

  (define (shell-quoted url)
    (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list url))) "'"))

  (edoc "Follow a URI relative to the explicit source. Web links use the configured browser; files are visited through the base and sent to this page's host."
        (id model "Markdown page") (document buffer "source identity") (uri string "shown link target"))
  (define (open-link! id document uri)
    (check-source! id document)
    (cond [(or (string:prefix? "http://" uri) (string:prefix? "https://" uri))
           (system (format "~a ~a >/dev/null 2>&1 &" (browser) (shell-quoted uri)))
           (log:add! 'markdown:open-link! (format "Opened ~a" uri))]
      [(string:prefix? "#" uri) (log:add! 'markdown:open-link! "Anchor links are not followed yet")]
      [else
       (let* ([base (store:property document 'file #f)] [dir (if base (file:directory-part base) "")]
              [path (file:expand uri)] [target (if (string:prefix? "/" path) path (string-append dir path))])
         (edit:visit-file! target
           (lambda (kind value)
             (unless (eq? kind 'buffer) (error 'open-link! "link names a directory" target))
             (widget:invoke! id 'open (catalogue-host:reference value)
               (if (or (string:suffix? ".md" target) (string:suffix? ".markdown" target)) '((presentation . markdown)) '())))))]))

  (edoc "Show a Markdown source in this window using an independently fitted widget. Source text, file state and undo history are unchanged. C-c v returns to its source."
        (source (list-of buffer) "optional source reference, default current") (returns model))
  (define (view! . source)
    (unless (<= (length source) 1) (error 'view! "expected at most one source"))
    (let* ([document (or (if (pair? source) (car source) (head:current-buffer))
                       (error 'view! "Markdown needs a base document"))]
           [row (if (equal? document (head:current-buffer)) (car (head:point)) 0)]
           [root (window:tool! (string-append "markdown " (store:buffer-name document))
                   (lambda (commands) (create! head:ui-actor document commands row)) (format "markdown:~s" document))])
      (let* ([host (window:show-widget! (head:current-window) root)]
             [actual (head:buffer-fact host 'widget-id #f)])
        (control:locate! (widget:descendant actual 'app 'text) row) actual)))

  (edoc "Register Markdown composition, presentation, faces and source/view commands." (public))
  (define (init!)
    (control:register! edit:copy-text!)
    (widget:register! 'markdown-page 1
      (append (layout:container 'y) (list (cons 'actions (list (cons 'open-link open-link!) (cons 'open-source open-source!))))))
    (window:register-presentation! 'markdown (lambda (document commands position) (create! head:ui-actor document commands (car position))))
    (for-each (lambda (face) (style:set! (car face) (cdr face)))
      '((md-h1 bold underline) (md-h2 bold) (md-h3 bold italic) (md-h4 italic)
        (md-quote italic (foreground bright-black)) (md-link underline (foreground 33)) (md-code reset)))
    (keymap:bind-default! 'markdown "C-c v" view!)))
