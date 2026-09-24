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

(import (only (foundation edoc) elibrary))
(elibrary (run main)
  (export config-reload-on-save load-config! modules-reload-on-save run! set-startup-page!
          shutdown! shutdown-on-exit)
  (import (chezscheme)
          (prefix (core client) client:)
          (prefix (core kernel) kernel:)
          (prefix (core startup) startup:)
          (prefix (foundation string) string:)
          (prefix (head dispatch) dispatch:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (service file) file:)
          (prefix (service log) log:)
          (prefix (state actor) actor:)
          (prefix (sys sys) sys:)
          (prefix (sys tty) tty:))

  (edoc "Whether the base stops when the last head leaves."
        (value boolean))
  (define shutdown-on-exit (make-parameter #f
                             (lambda (value)
                               (unless (boolean? value) (error 'shutdown-on-exit "expected a boolean")) value)))

  (edoc "Stop the base and every head after reviewing modified buffers: yes, no, or view them."
        (prompts))
  (define (shutdown!)
    (let ([token #f])
      (dynamic-wind void
        (lambda ()
          (let review ([remote (client:request 'prepare-close)] [changed? #f])
            (set! token (cadr remote))
            (let-values ([(local valid?) (head:prepare-quit)])
              (let* ([status (cadddr remote)]
                     [count (lambda (key) (cdr (assq key status)))]
                     [risks
                      (filter values
                        (map (lambda (n noun)
                               (and (> n 0) (format "~a ~a~a" n noun (if (= n 1) "" "s"))))
                          (list local (count 'terminals)
                            (max 0 (- (count 'heads) 1)) (count 'agents) (count 'pending))
                          '("local draft" "terminal" "other head" "agent session" "pending interaction")))]
                     [answer (if (null? risks) #\y
                                 (prompt:key!
                                   (format "~aStop the base? ~a. Shared text and views are saved. y)es, n)o, v)iew"
                                     (if changed? "Work changed; " "") (string:join risks ", ")) "ynv"))])
                (case (and answer (char-downcase answer))
                  [(#\y)
                   (if (not (head:call-uninterrupted valid?))
                       (review (client:request 'prepare-close) #t)
                       (begin
                         (head:checkpoint!)
                         ;; The base rechecks transient work, then uses the
                         ;; same save/stop path as a system stop or restart.
                         (let ([next (client:request 'shutdown token)])
                           (review next #t))))]
                  [(#\v)
                   (client:request 'cancel-review token)
                   (set! token #f)
                   (head:view-review!)]
                  [else (void)])))))
        (lambda ()
          (when token
            (guard (ex [else (void)]) (client:request 'cancel-review token)))))))

  (define departure-hooked
    (head:set-departure!
      (lambda ()
        (if (not (shutdown-on-exit))
            ;; Ordinary detach commits after shutdown hooks and the final
            ;; checkpoint, once main has restored the terminal.
            (head:quit!)
            (begin
              (head:flush-ui-audit! 'all)
              (head:checkpoint!)
              (let ([result (client:leave! #t)])
                (if (and (pair? result) (eq? (car result) 'last))
                    (shutdown!)
                    (head:quit!))))))))

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

  (edoc "Load config.e into the editor top level, repainting and re-resolving buffer modes; whether it loaded cleanly."
        (returns boolean))
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
  (edoc "Whether saving a module's source reloads it on the spot."
        (value boolean))
  (define modules-reload-on-save (make-parameter #t))

  (edoc "Whether saving config.e applies it on the spot."
        (value boolean))
  (define config-reload-on-save (make-parameter #t))

  (define (module-name-of-path path)
    ;; The module name a saved path denotes in the selected source roots;
    ;; #f for other paths, including kernel/main, which cannot be reloaded.
    (let* ([full (or (sys:canonical-file-path path) (file:canonical path))]
           [library (kernel:source-library full)])
      (and library
           (not (member library '((core kernel) (run main))))
           (or (find (lambda (name) (equal? (kernel:module-library name) library))
                     (kernel:loaded-modules))
               ;; Saving an independent new module still publishes its API.
               ;; Imported helpers retain their full library identity and
               ;; do not acquire another public prefix merely by being saved.
               (and (not (exists (lambda (name) (kernel:module-requires? name library))
                           (kernel:loaded-modules)))
                    (let ([name (symbol->string (car (reverse library)))])
                      (and (equal? (kernel:module-library name) library) name)))
               library))))

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

  (edoc "Install the welcome page shown when e starts without a file; #f restores the scratch buffer."
        (proc (or procedure #f) "the page"))
  (define (set-startup-page! proc)
    ;; A module (or config.e) may present a welcome page when e starts
    ;; without a file argument; #f restores the plain scratch buffer.
    (unless (or (not proc) (procedure? proc))
      (error 'set-startup-page! "expected a procedure or #f" proc))
    (set! startup-page proc))

  (edoc "Run the head: the main loop against the base, as this head's actor."
        (returns integer "the exit status"))
  (define (run!)
    (kernel:pin-modules! '("main"))
    (parameterize ([exit-handler (exit-handler)] [abort-handler (abort-handler)] [reset-handler (reset-handler)])
      (actor:call-as head:ui-actor run-head)))

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
            '("blame" "buffer-view" "c-mode" "describe" "dispatch" "echo" "edit" "eval" "extension" "file-view" "git-view"
              "glyph" "head" "keymap" "keys" "literal" "log-view" "markdown" "md-mode" "merge" "mode" "mouse"
              "paint" "paren" "pretty-scheme" "prompt" "render" "scheme-format"
              "scheme-mode" "search" "style" "terminal" "tty" "window"))))
      (load-config!)
      ;; Config loads the local view providers before resolving their plain
      ;; descriptors. An explicit file still opens in the restored selection,
      ;; but only once the terminal is live and keys arrive, below: visiting
      ;; may ask about a file changed on disk, and a question asked before
      ;; the input reader runs waits forever.
      (let ([resumed? (head:resume!)])
        (when (and (not file) (not resumed?) startup-page)
          (guard (ex [else (void)]) (startup-page))
          ;; the greeting outlives the page's own load chatter
          (echo:set-text! (startup-greeting)))))
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
      ;; Mouse tracking likewise (see mouse:track!).
      ;; Mode 2031 subscribes to theme changes. Query both the scheme and
      ;; the background color so older hosts can supply a fallback.
      (lambda () (sys:terminal-raw!)
        (paint:ansi! "\x1b;[?1049h\x1b;[2J\x1b;[?2004h\x1b;[?2031h")
        (tty:query-color-scheme!)
        (tty:mouse-reporting! #t)
        (paint:set-screen-live! #t)
        (head:start-input-reader!))
      (lambda ()
        (let ([file (startup:file)])
          (when file (head:open-file! file)))
        (let loop ()
          (unless (head:quitting?)
            (head:run-deferred!)
            (paint:redraw!)
            ;; This head's own key may have changed the screen: publish
            ;; at once. Wake frames (foreign edits) checkpoint at most once
            ;; a second, from the frame hook.
            (head:checkpoint!)
            ;; A command that raises (a read-only buffer, a bug in an
            ;; extension module) reports itself instead of killing the
            ;; editor.
            (guard (ex [(client:ended? ex) (raise ex)]
                       [(kernel:read-only-error? ex)
                        (echo:set-text! "Buffer is read-only")]
                       [(kernel:refusal? ex)
                        (echo:set-text! (condition-message ex))]
                       [else (log:add! 'error (kernel:condition-text ex))])
              (dispatch:key! (parameterize ([head:in-main-pump #t])
                               (head:read-key-event))))
            (head:after-key!)
            (loop))))
      (lambda ()
        ;; A dead connection or a failing shutdown hook must not prevent
        ;; restoration of the shell's terminal modes.
        (dynamic-wind void
          head:run-shutdown-hooks!
          (lambda ()
            (guard (ex [else (void)]) (head:checkpoint!))
            (paint:set-screen-live! #f)
            (paint:reset-cursor-style!)
            (tty:mouse-reporting! #f)
            (paint:ansi! "\x1b;[?2031l\x1b;[?2004l\x1b;[?25h\x1b;[?1049l\x1b;[0m")
            (flush-output-port (sys:terminal-output-port))
            (sys:terminal-restore!)
            (report-unsaved-work!))))))

  (define (report-unsaved-work!)
    ;; After the screen is given back, in bold red on a terminal: the
    ;; buffers whose work is unsaved, the shared ones kept in the base and
    ;; the local ones that went with this head. Quitting asks nothing.
    (let ([unsaved (filter (lambda (b)
                             (and (head:buffer-modified b) (not (head:buffer-fact b 'disposable #f))))
                           (head:buffers))])
      (unless (null? unsaved)
        (let* ([port (current-error-port)]
               [term (getenv "TERM")]
               [color? (and term (not (string=? term "dumb")))]
               [names (lambda (bs) (string:join (map head:buffer-name bs) ", "))]
               [shared (filter head:buffer-store-id unsaved)]
               [local (filter (lambda (b) (not (head:buffer-store-id b))) unsaved)])
          (define (say text)
            (display (if color? (string-append "\x1b;[1;31m" text "\x1b;[0m\n") (string-append text "\n")) port))
          (unless (null? shared)
            (say (format "e: unsaved work stays in the base: ~a" (names shared))))
          (unless (null? local)
            (say (format "e: unsaved local work went with this head: ~a" (names local))))
          (flush-output-port port)))))

) ;; library (main)
