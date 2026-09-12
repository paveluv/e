;; main.sls -- the editor: the library (main), what the loader runs.
;;
;; Startup (the modules, config.e, the file argument or a welcome
;; page), the seat's loop -- a frame, a key, a command -- and
;; shutdown. Key dispatch lives in (dispatch); the loop reaches the
;; reloadable commands through hooks held by (head).
;; Another actor's pending question is presented before each frame.
;;
;; Like the kernel, main is never reloaded: it is what everything else
;; runs under.  (main:run) is the whole program.

(library (main)
  (export run set-startup-page! load-config!
          modules-reload-on-save config-reload-on-save)
  (import (chezscheme) (prefix (sys) sys:)
          (prefix (file) file:)
          (prefix (kernel) kernel:)
          (prefix (startup) startup:)
          (prefix (head) head:)
          (prefix (dispatch) dispatch:)
          (prefix (paint) paint:)
          (prefix (echo) echo:)
          (prefix (prompt) prompt:)
          (prefix (tty) tty:)
          (prefix (mode) mode:)
          (prefix (actor) actor:)
          (prefix (log) log:)
          (prefix (string) string:))

  ;;; Another actor's question --------------------------------------------------------

  ;; The head's side of the interaction protocol: another actor's
  ;; question waits in the echo area as an unlogged indicator until
  ;; C-c a answers it -- nobody's keyboard is stolen mid-thought.
  (define (present-pending-ask!)
    (when (and (not (prompt:active?))
               (or (eq? (echo:text-owner) 'ask) (string=? (echo:text) "")))
      (let ([asks (actor:pending head:ui-actor)])
        (if (null? asks)
          (echo:set-text! "")
          (let ([ask (car asks)])
            (echo:set-text!
              (string:elide (format "~a asks: ~a -- C-c a answers~a"
                              (cadr ask) (caddr ask)
                              (if (> (length asks) 1)
                                (format " (~a waiting)" (length asks))
                                ""))
                (paint:screen-cols))
              'ask))))))


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

  ;; Saving a module's source reloads it on the spot (a fresh .sls file
  ;; in an active library root is loaded for the first time), and saving
  ;; config.e applies it, so editing the editor from inside itself
  ;; takes effect on save.  Both on by default; (main:modules-reload-on-save
  ;; #f) or (main:config-reload-on-save #f) -- in config.e for an
  ;; installation, at M-x for a session -- turns either off.
  (define modules-reload-on-save (make-parameter #t))
  (define config-reload-on-save (make-parameter #t))

  (define (module-name-of-path path)
    ;; The module name a saved path denotes in the selected source roots;
    ;; #f for other paths, including kernel/main, which cannot be reloaded.
    (let* ([full (file:canonical path)] [base (file:base-name full)])
      (and (string:suffix? ".sls" base)
           (not (member base '("kernel.sls" "main.sls")))
           (let ([name (substring base 0 (- (string-length base) 4))])
             (and (string=? full (file:canonical (kernel:module-source name))) name)))))

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

  (define startup-page #f)

  (define (set-startup-page! proc)
    ;; A module (or config.e) may present a welcome page when e starts
    ;; without a file argument; #f restores the plain scratch buffer.
    (unless (or (not proc) (procedure? proc))
      (error 'set-startup-page! "expected a procedure or #f" proc))
    (set! startup-page proc))

  (define (run)
    (kernel:pin-modules! '("main"))
    (actor:call-as head:ui-actor run-head))

  (define (run-head)
    ;; The loader script is pure bootstrap; the extension modules are
    ;; loaded here, before the file argument needs their modes.
    (let ([file (startup:file)])
      ;; the log-view module lists *log* from startup
      (for-each
        (lambda (failure)
          (let ([msg (format "Error in ~a: ~a"
                             (car failure) (kernel:condition-text (cdr failure)))])
            (display (format "e: ~a\n" msg) (current-error-port))
            (echo:set-text! msg)))
        (reverse
          (kernel:load-modules!
            '("blame" "c-mode" "describe" "dispatch" "echo" "edit" "eval" "file-view" "git-view"
              "glyph" "head" "keymap" "log-view" "markdown" "md-mode" "mode"
              "paint" "paren" "pretty-scheme" "prompt" "render" "scheme-format"
              "scheme-mode" "search" "style" "terminal" "tty"))))
      (load-config!)
      ;; Config loads the local view providers before resolving their plain
      ;; descriptors. An explicit file still opens in the restored selection.
      (let ([resumed? (and (eq? (startup:mode) 'attach) (head:resume!))])
        (if file
            (head:open-file! file)
            (when (and (not resumed?) startup-page)
              (guard (ex [else (void)]) (startup-page))
              ;; the greeting outlives the page's own load chatter
              (echo:set-text! (startup-greeting))))))
    ;; A stray SIGINT outside an evaluation must not drop into Chez's break
    ;; prompt underneath the editor's screen.
    (keyboard-interrupt-handler void)
    ;; A stray (exit) or (abort) evaluated at the prompt must not kill
    ;; the process past the modified-buffers check: they run the
    ;; editor's quit and unwind the evaluation instead.
    (let ([safe-quit (lambda args
                       (head:quit-command!)
                       (raise (head:make-interrupted)))])
      (exit-handler safe-quit)
      (abort-handler safe-quit)
      (reset-handler safe-quit))
    (dynamic-wind
      ;; The alternate screen, plus bracketed paste: terminals that
      ;; support it (virtually all) wrap pastes in ESC[200~ / ESC[201~,
      ;; making a paste one identifiable edit; others ignore the mode.
      ;; Mouse tracking likewise (see mouse!).
      ;; Mode 2031 subscribes to theme changes. Query both the scheme and
      ;; the background color so older hosts can supply a fallback.
      (lambda () (sys:terminal-raw!)
        (paint:ansi "\x1b;[?1049h\x1b;[2J\x1b;[?2004h\x1b;[?2031h")
        (tty:query-color-scheme!)
        (tty:mouse-reporting! #t)
        (paint:set-screen-live! #t)
        (head:start-input-reader!))
      (lambda ()
        (let loop ()
          (unless (head:quitting?)
            (head:run-deferred!)
            (paint:redraw!)
            ;; This head's own key may have changed the screen: publish
            ;; at once. Wake frames (foreign edits) checkpoint at most once
            ;; a second, from the frame hook.
            (when (eq? (startup:mode) 'attach) (head:checkpoint!))
            ;; A command that raises (a read-only buffer, a bug in an
            ;; extension module) reports itself instead of killing the
            ;; editor.
            (guard (ex [(kernel:read-only-error? ex)
                        (echo:set-text! "Buffer is read-only")]
                       [(kernel:refusal? ex)
                        (echo:set-text! (condition-message ex))]
                       [else (log:add! 'error (kernel:condition-text ex))])
              (dispatch:key! (parameterize ([head:in-main-pump #t])
                               (head:read-key-event))))
            (head:after-key!)
            (loop))))
      (lambda ()
        (when (eq? (startup:mode) 'attach)
          (guard (ex [else (void)]) (head:checkpoint!)))
        (head:run-shutdown-hooks!)
        (paint:set-screen-live! #f)
        (paint:reset-cursor-style!)
        (paint:ansi "\x1b;[?1002;1006l\x1b;[?2031l\x1b;[?2004l\x1b;[?25h\x1b;[?1049l\x1b;[0m")
        (flush-output-port (sys:terminal-output-port))
        (sys:terminal-restore!))))

) ;; library (main)
