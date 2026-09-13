;; terminal.sls -- head commands and presentation for base-owned VT apps.

(library (terminal)
  (export init! (rename (terminal!! open!!) (terminal-send! send!)
                        (terminal-yank! yank!) (terminal-close! close!)
                        (terminal-toggle-capture! toggle-capture!)
                        (terminal-color-scheme! color-scheme!)
                        (vt:scrollback scrollback) (vt:shell shell)
                        (terminal-forward-clipboard-to-kill-ring forward-clipboard-to-kill-ring)))
  (import (chezscheme) (except (edit) init!)
          (prefix (vt) vt:) (prefix (head) head:) (prefix (paint) paint:)
          (prefix (mode) mode:) (prefix (keymap) keymap:) (prefix (kernel) kernel:)
          (prefix (file) file:) (prefix (store) store:) (prefix (log) log:) (prefix (doc) doc:))

  (define terminal-forward-clipboard-to-kill-ring
    (make-parameter #t
      (lambda (enabled?)
        (unless (boolean? enabled?)
          (error 'forward-clipboard-to-kill-ring "expected a boolean" enabled?))
        enabled?)))

  (define (terminal-facts buffer)
    (let* ([facts (head:app-facts buffer)] [owner (and facts (cdr (assq 'app facts)))])
      (and owner (equal? (cadr owner) 'terminal) facts)))

  (define (terminal-id buffer)
    (and (terminal-facts buffer) (head:buffer-store-id buffer)))

  (define (send-input! text paste?)
    (let* ([w (selected-window)] [id (terminal-id (current-buffer))])
      (unless id (error 'terminal "current buffer is not a terminal"))
      (head:follow-app! w #t)
      (vt:send! head:ui-actor id text
        (list (max 1 (head:window-size w)) (head:window-content-width w)) paste?
        (head:host-color-scheme))))

  (define (terminal-send! text) (send-input! text #f) (void))
  (define (terminal-yank!) (send-input! (current-kill-ring) #t) (void))
  (define (terminal-toggle-capture!)
    (unless (and (terminal-id (current-buffer)) (head:app-buffer? (current-buffer)))
      (error 'toggle-capture! "current buffer is not a live terminal"))
    (let ([w (selected-window)]) (head:set-full-capture! w (not (head:full-capture? w)))))
  (define (terminal-close! . buffer*)
    (cond [(terminal-id (if (pair? buffer*) (car buffer*) (current-buffer))) => vt:close!])
    (void))
  (define (terminal-color-scheme! scheme) (vt:color-scheme! scheme head:ui-actor))

  (define (terminal!! . command*)
    (let* ([prior (current-buffer)] [path (head:buffer-file prior)] [id #f] [buffer #f])
      (guard (ex [else
                  (when id
                    (vt:close! id)
                    (when (store:exists? id) (store:delete! head:ui-actor id)))
                  (when (eq? (current-buffer) buffer) (show-buffer! prior))
                  (raise ex)])
        (paint:window-layout)
        (let ([w (selected-window)])
          (set! id (vt:open! head:ui-actor (and (pair? command*) (car command*))
                     (if path (file:directory-part path) (current-directory))
                     (max 1 (head:window-size w)) (head:window-content-width w)
                     (head:host-color-scheme))))
        (set! buffer (head:adopt-store-buffer! id))
        (head:buffer-line-numbers-setting-set! buffer #f)
        (show-buffer! buffer)
        (head:set-full-capture! (selected-window) #f)
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
                         (terminal-forward-clipboard-to-kill-ring))
                (copy-to-kill-buffer! (caddr clipboard))
                (log:add! 'terminal (format "Received clipboard from ~a, stored in kill ring"
                                            (head:buffer-name buffer))))
              (for-each
                (lambda (message)
                  (unless (member message (cdr old))
                    (log:add! 'terminal (format "~a: ~a" (head:buffer-name buffer) message)))) diagnostics)))))
      (head:buffers)))

  (define (init!)
    (mode:register! "terminal" '() '() (lambda (line) #f))
    (terminal-color-scheme! (head:host-color-scheme))
    (head:add-color-scheme-hook! terminal-color-scheme!)
    (head:add-pre-redraw-hook! present-notices!)
    (keymap:bind-default! "C-c t" terminal!!)
    (keymap:set-context-capture! 'terminal "C-]" terminal-toggle-capture! '("C-x" "M-x"))
    (keymap:bind-default! 'terminal "S-PAGEUP" (lambda () (page-window-fraction! -1 1)))
    (keymap:bind-default! 'terminal "S-PAGEDOWN" (lambda () (page-window-fraction! 1 1)))
    (doc:register!
      '(((terminal:open!!)
         (("procedure" . "(terminal:open!! [command])")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Open a new PTY-backed terminal buffer using the shell configured by `terminal:shell`, or interpret `command` with that shell when supplied. Partial capture is the default: C-x and M-x run e commands; other input reaches the child. C-] or the clickable status indicator toggles full capture for this window. Shift-PageUp/Down scroll in either mode.")
        ((terminal:toggle-capture!)
         (("procedure" . "(terminal:toggle-capture!)")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Toggle capture in the selected live terminal window without changing cursor following or other windows. Partial capture (◐) leaves C-x and M-x to e; full capture (●) forwards them to the child. Capture controls are unavailable after the process exits.")
        ((terminal:send!)
         (("procedure" . "(terminal:send! text)")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Send text to the process in the current terminal buffer.")
        ((terminal:close!)
         (("procedure" . "(terminal:close! [buffer])")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Terminate and detach the process owned by a terminal buffer.")
        ((terminal:shell)
         (("parameter" . "(terminal:shell [path])")) "string"
         ("(terminal)") terminal "Terminal" #f
         "Get or set the shell used by terminal:open!!. It defaults to $SHELL, then /bin/sh.")
        ((terminal:color-scheme!)
         (("procedure" . "(terminal:color-scheme! scheme)")) "void"
         ("(terminal)") terminal "Terminal" #f
         "Record the host's color scheme (dark, light, or #f for unknown) and report the change to terminal children subscribed with private mode 2031. Wired to the host's own reports at startup."))))

) ;; library (terminal)
