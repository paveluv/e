;; main.e -- the editor: the library (main), what the loader runs.
;;
;; Startup (the modules, config.e, the file argument or a welcome
;; page), the seat's loop -- a frame, a key, a command -- and
;; shutdown, plus key dispatch: a key sequence resolves through the
;; current mode's context, then the global map, and an unbound
;; character through the SELF-INSERT binding; the current buffer's app
;; handler has first refusal.  The dispatcher records the command it
;; ran (head:last-command) and the keys that ran it
;; (head:current-keys), so commands that chain -- kills, typed runs --
;; ask the head instead of the dispatcher keeping flags for them.
;; Another actor's pending question is presented before each frame.
;;
;; Like the kernel, main is never reloaded: it is what everything else
;; runs under.  (main:run) is the whole program.

(library (main)
  (export run set-startup-page! (rename (handle-key! dispatch-key!))
          load-config! modules-reload-on-save config-reload-on-save
          set-file-opener! set-quit-command! set-after-key!)
  (import (chezscheme) (prefix (sys) sys:)
          (prefix (file) file:)
          (prefix (kernel) kernel:)
          (prefix (head) head:)
          (prefix (paint) paint:)
          (prefix (echo) echo:)
          (prefix (prompt) prompt:)
          (prefix (keymap) keymap:)
          (prefix (tty) tty:)
          (prefix (mode) mode:)
          (prefix (actor) actor:)
          (prefix (log) log:)
          (prefix (string) string:))

  ;;; Key dispatch ---------------------------------------------------------------------

  (define (run-key-action! action)
    ;; Run a resolved binding's action and remember it as the last
    ;; command (an error still counts); an unbound key, or a context
    ;; action leaking into the global map, is reported and remembered
    ;; as no command at all.
    (cond [(procedure? action)
           (dynamic-wind void action
             (lambda () (head:set-last-command! action)))]
          [(not action)
           (head:set-last-command! #f)
           (echo:set-text! "Key is unbound")]
          [else
           (head:set-last-command! #f)
           (error 'dispatch-key! "context action used globally" action)]))

  (define (dispatch-sequence! first)
    ;; Resolve a key sequence: the buffer's mode context first, then the
    ;; global map.  A context may name an escape prefix
    ;; (keymap:set-context-escape!): a sequence it starts and the
    ;; context does not bind resolves, minus the prefix, in the global
    ;; map -- how a captured app's user runs one complete global
    ;; command.
    (let* ([mode-context (mode:key-context (head:window-buffer (head:current)))]
           [escape (and mode-context (keymap:context-escape mode-context))])
      (let loop ([sequence (list first)])
        (let* ([in-context (and mode-context
                                (keymap:resolved-binding mode-context sequence))]
               [context-prefix? (and mode-context
                                     (keymap:binding-prefix? mode-context sequence))]
               [escaped (and escape (not in-context) (not context-prefix?)
                             (pair? (cdr sequence))
                             (string=? (car sequence) escape)
                             (cdr sequence))]
               [global (or escaped sequence)]
               [hit (or in-context (keymap:resolved-binding 'global global))]
               [prefix? (or context-prefix? (keymap:binding-prefix? 'global global))])
          (cond
            [prefix?
             (echo:set-text! (string-append (keymap:sequence-text sequence) "-"))
             (echo:set-pending! '())
             (paint:redraw!)
             (let ([next (head:read-key-event)])
               (if (eof-object? next)
                   (head:quit!)
                   (loop (append sequence (list next)))))]
            [hit
             ;; A prefix is only a waiting indicator. Once its complete binding
             ;; is known, remove it before the command runs; commands that have
             ;; something useful to report will publish their own message.
             (when (> (length sequence) 1) (echo:settle!))
             (head:set-current-keys! sequence)
             (run-key-action! (keymap:binding-action (cdr hit)))]
            [(and (= (length sequence) 1)
                  (tty:key-event-character first)
                  (keymap:resolved-binding 'global '("SELF-INSERT")))
             ;; an unbound character inserts itself: the command bound to
             ;; SELF-INSERT reads the key from head:current-keys
             => (lambda (hit)
                  (head:set-current-keys! sequence)
                  (run-key-action! (keymap:binding-action (cdr hit))))]
            [else
             (head:set-last-command! #f)
             (echo:set-text!
               (format "~a is undefined" (keymap:sequence-text sequence)))])))))

  (define (handle-key! input)
    ;; One key from the pump: a character or an event string, eof
    ;; when the terminal is gone.  The current buffer's app has first
    ;; refusal; what it declines goes through the keymaps.
    (let ([event (cond [(eof-object? input) input]
                       [(char? input) (tty:character-event input)]
                       [else input])])
      (cond
        [(eof-object? event) (head:quit!)]
        [(string=? event "MOUSE-HANDLED")
         (echo:settle!)
         (void)]
        [else
         (echo:settle!)
         (if (head:dispatch-app-event! event)
             (head:set-last-command! #f)
             (dispatch-sequence! event))])))

  ;;; Another actor's question --------------------------------------------------------

  ;; The head's side of the interaction protocol: another actor's
  ;; question waits in the echo area as an unlogged indicator until
  ;; C-c a answers it -- nobody's keyboard is stolen mid-thought.
  (define (present-pending-ask!)
    (let ([asks (actor:pending head:ui-actor)])
      (when (and (pair? asks)
                 (not (prompt:active?))
                 (string=? (echo:text) ""))
        (let ([ask (car asks)])
          (echo:set-text!
            (string:elide (format "~a asks: ~a -- C-c a answers~a"
                            (cadr ask) (caddr ask)
                            (if (> (length asks) 1)
                              (format " (~a waiting)" (length asks))
                              ""))
              (paint:screen-cols)))))))


  ;;; Configuration and reloads -----------------------------------------------------

  (define (load-config!)
    ;; The kernel loads config.e (kernel:load-config!); the head repaints
    ;; around it -- a recolor must repaint rows cached under the old
    ;; codes -- re-resolves buffer modes, and reports an error.  ->
    ;; whether it loaded cleanly.
    (paint:invalidate-screen-cache!)
    (let ([result (kernel:load-config!)])
      (cond [(eq? result #t)
             (mode:refresh!)
             (paint:invalidate-screen-cache!)
             #t]
            [(eq? result 'absent) #f]
            [else
             (log:add! 'config (format "Error in config.e: ~a"
                                       (kernel:condition-text result)))
             #f])))

  (define reload-tail-hooked
    (kernel:add-after-reload-hook!
      (lambda (name)
        (load-config!)              ; the settings reapply on top
        (mode:refresh!)
        (paint:invalidate-screen-cache!)
        (echo:set-text! (format "Reloaded ~a" name)))))

  ;; Saving a module's source reloads it on the spot (a fresh .e file
  ;; in the lib directory is loaded for the first time), and saving
  ;; config.e applies it, so editing the editor from inside itself
  ;; takes effect on save.  Both on by default; (main:modules-reload-on-save
  ;; #f) or (main:config-reload-on-save #f) -- in config.e for an
  ;; installation, at M-x for a session -- turns either off.
  (define modules-reload-on-save (make-parameter #t))
  (define config-reload-on-save (make-parameter #t))

  (define (module-name-of-path path)
    ;; The module name a saved path denotes: a .e file directly in the
    ;; editor's lib directory; #f for anything else -- main included,
    ;; which cannot be reloaded.
    (let ([full (file:canonical path)]
          [lib (string-append (file:canonical (caar (library-directories)))
                              "/")])
      (and (string:prefix? lib full)
           (string:suffix? ".e" full)
           (let ([base (string:tail full (string-length lib))])
             (and (not (string:search base "/" 0 (string-length base)))
                  (not (member base '("kernel.e" "main.e")))
                  (substring base 0 (- (string-length base) 2)))))))

  (define (reload-on-save! path)
    ;; The post-save hook.  A reload that fails (a module saved mid-edit,
    ;; say) reports itself without disturbing the save -- or the editor,
    ;; which keeps running the module's old version.  A saved config.e
    ;; applies on the spot the same way.
    (let ([name (and (modules-reload-on-save) (module-name-of-path path))])
      (cond
        [name
         (guard (ex [else (log:add! 'reload-module!
                            (format "Reload of ~a failed: ~a"
                                    name (kernel:condition-text ex)))])
           (kernel:reload-module! name)
           (log:add! 'reload-module! (format "Reloaded ~a" name)))]
        [(and (config-reload-on-save)
              (string=? (file:canonical path) (file:canonical (kernel:config-file))))
         (when (load-config!)
           (log:add! 'config "Applied config.e"))])))

  ;; the reload is a post-save hook like any module's
  (define reload-hooked (file:add-post-save-hook! reload-on-save!))

  ;;; Startup, the loop, shutdown --------------------------------------------------------

  ;; a frame is the painter's: the pump asks for one through this hook
  (define frame-hooked (head:set-frame-hook! (lambda () (paint:redraw!))))

  (define ask-presented (head:add-pre-redraw-hook! present-pending-ask!))

  ;; What the loop asks of the commands, installed by them: how to open
  ;; the file argument, how to quit (the modified-buffers check), and
  ;; what runs after every key.  The command layer reloads; the loop
  ;; does not, so it holds no direct reference into it.
  (define open-file! (lambda (path) (void)))
  (define quit-command (lambda () (head:quit!)))
  (define after-key! void)

  (define (set-file-opener! proc) (set! open-file! proc))
  (define (set-quit-command! proc) (set! quit-command proc))
  (define (set-after-key! proc) (set! after-key! proc))

  (define (startup-greeting)
    (let* ([date (current-date)]
           [hour (date-hour date)]
           [hour12 (let ([h (mod hour 12)]) (if (= h 0) 12 h))]
           [weekdays '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
                        "Friday" "Saturday")]
           [months '#("January" "February" "March" "April" "May" "June"
                      "July" "August" "September" "October" "November"
                      "December")]
           [salutation (cond [(< hour 12) "Good morning!"]
                             [(< hour 18) "Good afternoon!"]
                             [else "Good evening!"])])
      (format "Today is ~a, ~a ~a, ~a. It's ~2,'0d:~2,'0d ~a. ~a"
              (vector-ref weekdays (date-week-day date))
              (vector-ref months (- (date-month date) 1))
              (date-day date)
              (date-year date)
              hour12
              (date-minute date)
              (if (< hour 12) "AM" "PM")
              salutation)))

  (define echo-greeting-shown (echo:set-text! (startup-greeting)))

  (define (usage)
    (display "Usage: e [file]\n")
    (display "A tiny Emacs-like terminal editor. Set LINES/COLUMNS if needed.\n")
    (display "Extension modules are loaded from the lib directory at startup.\n"))

  (define startup-page #f)

  (define (set-startup-page! proc)
    ;; A module (or config.e) may present a welcome page when e starts
    ;; without a file argument; #f restores the plain scratch buffer.
    (unless (or (not proc) (procedure? proc))
      (error 'set-startup-page! "expected a procedure or #f" proc))
    (set! startup-page proc))

  (define (run)
    ;; The loader script is pure bootstrap; the extension modules are
    ;; loaded here, before the file argument needs their modes.
    (let ([args (command-line-arguments)])
      (when (and (pair? args) (member (car args) '("-h" "--help"))) (usage) (exit 0))
      ;; the log-view module lists *log* from startup
      (for-each
        (lambda (failure)
          (let ([msg (format "Error in ~a: ~a"
                             (car failure) (kernel:condition-text (cdr failure)))])
            (display (format "e: ~a\n" msg) (current-error-port))
            (echo:set-text! msg)))
        (reverse (kernel:load-modules!)))
      (load-config!)
      (if (pair? args)
          (open-file! (car args))
          (when startup-page
            (guard (ex [else (void)]) (startup-page))
            ;; the greeting outlives the page's own load chatter
            (echo:set-text! (startup-greeting)))))
    (unless (and (getenv "TERM") (not (string=? (getenv "TERM") "dumb")))
      (display "e: an interactive terminal is required\n" (current-error-port))
      (exit 1))
    ;; A stray SIGINT outside an evaluation must not drop into Chez's break
    ;; prompt underneath the editor's screen.
    (keyboard-interrupt-handler void)
    ;; A stray (exit) or (abort) evaluated at the prompt must not kill
    ;; the process past the modified-buffers check: they run the
    ;; editor's quit and unwind the evaluation instead.
    (let ([safe-quit (lambda args
                       (quit-command)
                       (raise (head:make-interrupted)))])
      (exit-handler safe-quit)
      (abort-handler safe-quit)
      (reset-handler safe-quit))
    (dynamic-wind
      ;; The alternate screen, plus bracketed paste: terminals that
      ;; support it (virtually all) wrap pastes in ESC[200~ / ESC[201~,
      ;; making a paste one identifiable edit; others ignore the mode.
      ;; Mouse tracking likewise (see mouse!).
      ;; Mode 2031 subscribes to the host's color-scheme change reports
      ;; and DSR 996 asks for the current one; hosts without the feature
      ;; ignore both.
      (lambda () (sys:terminal-raw!)
        (paint:ansi "\x1b;[?1049h\x1b;[2J\x1b;[?2004h\x1b;[?2031h\x1b;[?996n")
        (tty:mouse-reporting! #t)
        (paint:set-screen-live! #t)
        (head:start-input-reader!))
      (lambda ()
        (let loop ()
          (unless (head:quitting?)
            (head:run-deferred!)
            (paint:window-layout)
            (head:before-frame!)
            (paint:redraw!)
            ;; A command that raises (a read-only buffer, a bug in an
            ;; extension module) reports itself instead of killing the
            ;; editor.
            (guard (ex [(kernel:read-only-error? ex)
                        (echo:set-text! "Buffer is read-only")]
                       [(kernel:refusal? ex)
                        (echo:set-text! (condition-message ex))]
                       [else (log:add! 'error (kernel:condition-text ex))])
              (handle-key! (parameterize ([head:in-main-pump #t])
                             (head:read-key-event))))
            (after-key!)
            (loop))))
      (lambda ()
        (head:run-shutdown-hooks!)
        (paint:set-screen-live! #f)
        (paint:reset-cursor-style!)
        (paint:ansi "\x1b;[?1002;1006l\x1b;[?2031l\x1b;[?2004l\x1b;[?25h\x1b;[?1049l\x1b;[0m")
        (flush-output-port (sys:terminal-output-port))
        (sys:terminal-restore!))))

) ;; library (main)
