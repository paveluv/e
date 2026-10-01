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
          (prefix (foundation edoc) edoc:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head terminal-control) control:)
          (prefix (service doc) doc:)
          (prefix (service file) file:)
          (prefix (service vt) vt:)
          (prefix (state store) store:))

  (edoc "Close the terminal of a buffer, the current one by default, ending its process."
        (buffer* (list-of buffer) "the terminal buffer reference, at most one") (public))
  (define (terminal-close! . buffer*)
    (unless (<= (length buffer*) 1) (error 'terminal-close! "expected at most one buffer"))
    (let* ([id (if (pair? buffer*) (edoc:type-value 'buffer (car buffer*)) (head:current-buffer))]
           [owner (and id (store:property id 'app #f))])
      (when (and owner (eq? (cadr owner) 'terminal)) (vt:close! id)))
    (void))

  (edoc "Tell the terminals the host's color scheme, so their default colors follow it."
        (scheme symbol "light or dark"))
  (define (terminal-color-scheme! scheme)
    (vt:color-scheme! scheme head:ui-actor))

  (edoc "Open a terminal in a new buffer, running a command or the shell, in the current file's directory."
        (command* (list-of string) "the command line to run, at most one; the shell by default"))
  (define (terminal! . command*)
    (let* ([prior (head:current-buffer-mirror)] [path (head:buffer-file prior)] [id #f] [buffer #f])
      (guard (ex [else
                  (when id
                    (vt:close! id)
                    (when (store:exists? id) (store:delete! head:ui-actor id)))
                  (when (eq? (head:current-buffer-mirror) buffer) (head:show-buffer-mirror! prior))
                  (raise ex)])
        (paint:window-layout)
        (let ([w (head:current-window)])
          (set! id (vt:open! head:ui-actor (and (pair? command*) (car command*))
                     (if path (file:directory-part path) (current-directory))
                     (max 1 (head:window-size w)) (head:window-content-width w)
                     (head:host-color-scheme))))
        (set! buffer (head:adopt-store-buffer! id))
        (head:show-buffer-mirror! buffer)
        (void))))

  (edoc "Install the terminal app: its mode with the keys of its context, color scheme and clipboard capabilities, the C-c t binding and its describe entries." (public))
  (define (init!)
    (control:register! edit:copy-text!)
    (mode:register! "terminal" '() '() (lambda (line) #f))
    (terminal-color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! terminal-color-scheme!)
    (keymap:bind-default! "C-c t" terminal!)
    (doc:register!
      '(((terminal:open!)
         (("procedure" . "(terminal:open! [command])")) "void"
         ("(apps terminal)") terminal "Terminal" #f
         "Open a new PTY-backed terminal buffer using the shell configured by `terminal:shell`, or interpret `command` with that shell when supplied. Partial capture is the default: C-x and M-x run e commands; other input reaches the child. C-] or the clickable status indicator toggles full capture for this window. Shift-PageUp/Down scroll in either mode.")
        ((terminal:close!)
         (("procedure" . "(terminal:close! [buffer])")) "void"
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
