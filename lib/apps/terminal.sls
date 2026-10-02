;; terminal.sls -- head commands and presentation for base-owned VT apps.

(import (only (foundation edoc) elibrary))
(elibrary (apps terminal)
  (export (rename (terminal-close! close!)) (rename (terminal-color-scheme! color-scheme!))
          (rename (control:create-view! create-view!))
          (rename (control:follow! follow!))
          (rename (control:forward-clipboard-to-copy-buffer forward-clipboard-to-copy-buffer))
          init! (rename (terminal! open!) (control:page! page!))
          (rename (control:paste! paste!) (control:pointer! pointer!) (control:press! press!)
            (vt:scrollback scrollback) (control:send! send!) (control:set-capture! set-capture!) (vt:shell shell))
          (rename (control:toggle-capture! toggle-capture!)))
  (import (chezscheme)
          (prefix (core handle) handle:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head terminal-control) control:)
          (prefix (head widget) widget:)
          (prefix (head window-control) window-control:)
          (prefix (service doc) doc:)
          (prefix (service file) file:)
          (prefix (service vt) vt:)
          (prefix (service window) window:)
          (prefix (state store) store:))

  (edoc "Close a terminal buffer, ending its process."
        (id buffer "terminal document") (public))
  (define (terminal-close! id)
    (let ([owner (store:property id 'app #f)])
      (when (and owner (eq? (cadr owner) 'terminal)) (vt:close! id)))
    (void))

  (edoc "Tell the terminals the host's color scheme, so their default colors follow it."
        (scheme symbol "light or dark"))
  (define (terminal-color-scheme! scheme)
    (vt:color-scheme! scheme head:ui-actor))

  (edoc "Start a terminal in this window, running the shell or an explicit command in its file's directory. The base owns the process and document; another presentation shares them. Failed placement closes only the newly started process and document."
        (receiver window (view window)) (window model "destination window")
        (command (list-of string) "optional shell command") (returns buffer))
  (define (terminal! window . command)
    (unless (and (<= (length command) 1) (for-all string? command)) (error 'terminal! "expected at most one command"))
    (let* ([manager (window-control:manager window)] [document (window:document manager window)]
           [path (and (handle:buffer? document) (store:property document 'file #f))]
           [f (widget:prepared window)] [rect (and f (widget:frame-rect f))]
           [id (vt:open! head:ui-actor (and (pair? command) (car command))
                 (if path (file:directory-part path) (current-directory))
                 (if rect (max 1 (- (cadddr rect) 1)) 1) (if rect (max 1 (caddr rect)) 1)
                 (head:host-color-scheme))])
      (guard (ex [else (vt:close! id) (when (store:exists? id) (store:delete! head:ui-actor id)) (raise ex)])
        (window:open-document! manager window id) id)))

  (edoc "Install the terminal app: its mode with the keys of its context, color scheme and clipboard capabilities, the C-c t binding and its describe entries." (public))
  (define (init!)
    (control:register! edit:copy-text!)
    (mode:register! "terminal" '() '() (lambda (line) #f))
    (terminal-color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! terminal-color-scheme!)
    (keymap:bind-default! 'composed-window "C-c t" (keymap:call terminal! widget:target))
    (doc:register!
      '(((terminal:open!)
         (("procedure" . "(terminal:open! window [command])")) "buffer"
         ("(apps terminal)") terminal "Terminal" #f
         "Open a new PTY-backed terminal buffer using the shell configured by `terminal:shell`, or interpret `command` with that shell when supplied. Partial capture is the default: C-x and M-x run e commands; other input reaches the child. C-] or the clickable status indicator toggles full capture for this window. Shift-PageUp/Down scroll in either mode.")
        ((terminal:close!)
         (("procedure" . "(terminal:close! buffer)")) "void"
         ("(apps terminal)") terminal "Terminal" #f
         "Terminate and detach the process owned by a terminal buffer.")
        ((terminal:shell)
         (("parameter" . "(terminal:shell [path])")) "string"
         ("(apps terminal)") terminal "Terminal" #f
         "Get or set the shell used by terminal:open!. It defaults to $SHELL, then /bin/sh.")
        ((terminal:color-scheme!)
         (("procedure" . "(terminal:color-scheme! scheme)")) "void"
         ("(apps terminal)") terminal "Terminal" #f
         "Record the host's color scheme (dark, light, or #f for unknown) and report the change to terminal children subscribed with private mode 2031. Wired to the host's own reports at startup."))))

) ;; library (terminal)
