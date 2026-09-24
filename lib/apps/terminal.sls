;; terminal.sls -- head commands and presentation for base-owned VT apps.

(import (only (foundation edoc) elibrary))
(elibrary (apps terminal)
  (export (rename (terminal-close! close!)) (rename (terminal-color-scheme! color-scheme!))
          (rename (terminal-yank! edit:yank!))
          (rename (terminal-forward-clipboard-to-copy-buffer forward-clipboard-to-copy-buffer))
          init! (rename (terminal! open!)) page-down! page-up! (rename (vt:scrollback scrollback))
          (rename (terminal-send! send!)) (rename (vt:shell shell))
          (rename (terminal-toggle-capture! toggle-capture!)))
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (head edit) edit:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (service doc) doc:)
          (prefix (service file) file:)
          (prefix (service log) log:)
          (prefix (service vt) vt:)
          (prefix (state store) store:))

  (edoc "Whether text a terminal program puts on the clipboard through OSC 52 also becomes the copy buffer's."
        (value boolean))
  (define terminal-forward-clipboard-to-copy-buffer (make-parameter #t
                                                      (lambda (enabled?)
                                                        (unless (boolean? enabled?)
                                                          (error 'forward-clipboard-to-copy-buffer "expected a boolean" enabled?))
                                                        enabled?)))

  (define (terminal-facts buffer)
    (let* ([facts (head:app-facts buffer)] [owner (and facts (cdr (assq 'app facts)))])
      (and owner (equal? (cadr owner) 'terminal) facts)))

  (define (terminal-id buffer)
    (and (terminal-facts buffer) (head:buffer-store-id buffer)))

  (define (send-input! text paste?)
    (let* ([w (head:current-window)] [id (terminal-id (head:current-buffer))])
      (unless id (error 'terminal "current buffer is not a terminal"))
      (head:follow-app! w #t)
      (vt:send! head:ui-actor id text
        (list (max 1 (head:window-size w)) (head:window-content-width w)) paste?
        (head:host-color-scheme))))

  (edoc "Send text to the terminal in the current buffer as typed input."
        (text string "what to type"))
  (define (terminal-send! text)
    (send-input! text #f) (void))

  (edoc "Send the copy buffer's text to the terminal in the current buffer as pasted input.")
  (define (terminal-yank!)
    (send-input! (edit:copy-text) #t) (void))

  (edoc "Toggle whether the current terminal window captures every key, C-x and M-x included.")
  (define (terminal-toggle-capture!)
    (unless (and (terminal-id (head:current-buffer)) (head:app-buffer? (head:current-buffer)))
      (error 'toggle-capture! "current buffer is not a live terminal"))
    (let ([w (head:current-window)]) (head:set-full-capture! w (not (head:full-capture? w)))))

  (edoc "Close the terminal of a buffer, the current one by default, ending its process."
        (buffer* (list-of buffer) "the terminal buffer, at most one"))
  (define (terminal-close! . buffer*)
    (cond [(terminal-id (if (pair? buffer*) (edoc:type-value 'buffer (car buffer*)) (head:current-buffer))) => vt:close!])
    (void))

  (edoc "Tell the terminals the host's color scheme, so their default colors follow it."
        (scheme symbol "light or dark"))
  (define (terminal-color-scheme! scheme)
    (vt:color-scheme! scheme head:ui-actor))

  (edoc "Open a terminal in a new buffer, running a command or the shell, in the current file's directory."
        (command* (list-of string) "the command line to run, at most one; the shell by default"))
  (define (terminal! . command*)
    (let* ([prior (head:current-buffer)] [path (head:buffer-file prior)] [id #f] [buffer #f])
      (guard (ex [else
                  (when id
                    (vt:close! id)
                    (when (store:exists? id) (store:delete! head:ui-actor id)))
                  (when (eq? (head:current-buffer) buffer) (head:show-buffer! prior))
                  (raise ex)])
        (paint:window-layout)
        (let ([w (head:current-window)])
          (set! id (vt:open! head:ui-actor (and (pair? command*) (car command*))
                     (if path (file:directory-part path) (current-directory))
                     (max 1 (head:window-size w)) (head:window-content-width w)
                     (head:host-color-scheme))))
        (set! buffer (head:adopt-store-buffer! id))
        (head:show-buffer! buffer)
        (head:set-full-capture! (head:current-window) #f)
        (void))))

  ;; UI effects consume published data on the head pump. Claims precede
  ;; callouts, so repaint/reload cannot duplicate clipboard or log delivery.
  (define presented
    (unbox (kernel:persistent-cell 'terminal-presented (lambda () (make-weak-eq-hashtable)))))
  (define (present-notices!)
    (for-each
      (lambda (buffer)
        (let ([facts (terminal-facts buffer)])
          (when facts
            (let* ([clipboard (cond [(assq 'clipboard facts) => cdr] [else #f])]
                   [diagnostics (cond [(assq 'diagnostics facts) => cdr] [else '()])]
                   [old (hashtable-ref presented buffer '(0))]
                   [sequence (if clipboard (car clipboard) (car old))])
              (hashtable-set! presented buffer (cons sequence diagnostics))
              (when (and clipboard (> sequence (car old))
                         (equal? (cadr clipboard) head:ui-actor)
                         (terminal-forward-clipboard-to-copy-buffer))
                (edit:copy-text! (caddr clipboard))
                (log:add! 'terminal (format "Copied clipboard text from ~a"
                                            (head:buffer-name buffer))))
              (for-each
                (lambda (message)
                  (unless (member message (cdr old))
                    (log:add! 'terminal (format "~a: ~a" (head:buffer-name buffer) message)))) diagnostics)))))
      (head:buffers)))

  (edoc "Scroll the terminal window a page up into its scrollback.")
  (define (page-up!) (edit:page-window-fraction! -1 1))

  (edoc "Scroll the terminal window a page down toward the live screen.")
  (define (page-down!) (edit:page-window-fraction! 1 1))

  (edoc "Install the terminal app: its mode with the keys of its context, color scheme hooks, notices, the C-c t binding, the capture toggle and its describe entries.")
  (define (init!)
    (mode:register! "terminal" '() '() (lambda (line) #f))
    (terminal-color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! terminal-color-scheme!)
    (head:add-pre-redraw-hook! present-notices!)
    (keymap:bind-default! "C-c t" terminal!)
    (keymap:set-context-capture! 'terminal "C-]" terminal-toggle-capture! '("C-x" "M-x"))
    (keymap:bind-default! 'terminal "S-PGUP" page-up!)
    (keymap:bind-default! 'terminal "S-PGDN" page-down!)
    (doc:register!
      '(((terminal:open!)
         (("procedure" . "(terminal:open! [command])")) "void"
         ("(apps terminal)") terminal "Terminal" #f
         "Open a new PTY-backed terminal buffer using the shell configured by `terminal:shell`, or interpret `command` with that shell when supplied. Partial capture is the default: C-x and M-x run e commands; other input reaches the child. C-] or the clickable status indicator toggles full capture for this window. Shift-PageUp/Down scroll in either mode.")
        ((terminal:toggle-capture!)
         (("procedure" . "(terminal:toggle-capture!)")) "void"
         ("(apps terminal)") terminal "Terminal" #f
         "Toggle capture in the selected live terminal window without changing cursor following or other windows. Partial capture (◐) leaves C-x and M-x to e; full capture (●) forwards them to the child. Capture controls are unavailable after the process exits.")
        ((terminal:send!)
         (("procedure" . "(terminal:send! text)")) "void"
         ("(apps terminal)") terminal "Terminal" #f
         "Send text to the process in the current terminal buffer.")
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
