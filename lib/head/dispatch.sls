;; dispatch.sls -- key dispatch for the head: the library (dispatch).
;;
;; A key sequence resolves through the current mode's context, then
;; the global map, and an unbound character through SELF-INSERT. The
;; current buffer's app handler has first refusal of the keys its
;; mode context leaves unbound. Commands that chain -- kills, typed
;; runs -- read head:last-command and head:current-keys, which the
;; dispatcher records for them. The main loop and modal readers use
;; the same entrypoint.

(import (only (foundation edoc) elibrary))
(elibrary (head dispatch)
  (export global-key! (rename (handle-key! key!)) set-prompt-opener!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (sys tty) tty:))

  ;;; Key dispatch ---------------------------------------------------------------------

  (define prompt-opener
    (lambda (name arguments) (error 'dispatch "no M-x prompt is installed to pre-fill" name)))

  (edoc "Install the procedure that opens M-x with a call begun: (open name arguments), the command's top-level name and the arguments already given; a key bound with keymap:prefill calls it."
        (open procedure "(open name arguments)"))
  (define (set-prompt-opener! open)
    (set! prompt-opener open))

  (define (run-key-action! action capture)
    ;; Run a resolved binding's action and remember it as the last
    ;; command (an error still counts); an unbound key, or a context
    ;; action leaking into the global map, is reported and remembered
    ;; as no command at all.  A call's producers run at the press, its
    ;; other arguments stand as given.
    (cond [(procedure? action)
           (unless (and capture (eq? action (cadr capture)))
             (head:follow-app! (head:current-window) #f))
           (dynamic-wind void action
             (lambda () (head:set-last-command! action)))]
          [(keymap:call-action? action)
           ;; a call built with keymap:call: the producers run at the press
           (head:follow-app! (head:current-window) #f)
           (dynamic-wind void
             (lambda ()
               (apply (keymap:call-action-procedure action)
                      (map (lambda (produce) (if (procedure? produce) (produce) produce))
                           (keymap:call-action-arguments action))))
             (lambda () (head:set-last-command! action)))]
          [(keymap:prefill-action? action)
           ;; a pre-filled M-x built with keymap:prefill
           (head:follow-app! (head:current-window) #f)
           (dynamic-wind void
             (lambda ()
               (let ([name (keymap:prefill-name action)])
                 (unless name (error 'dispatch "the pre-filled command has no top-level name" action))
                 (apply prompt-opener name (keymap:prefill-action-arguments action))))
             (lambda () (head:set-last-command! action)))]
          [(not action)
           (head:set-last-command! #f)
           (echo:set-text! "Key is unbound")]
          [else
           (head:set-last-command! #f)
           (error 'dispatch-key! "context action used globally" action)]))

  (edoc "Run the global map's command for one key as a command, the app handler bypassed: for a control standing in for a key, a wheel over an unfocused pane say. Whether the key was bound."
        (key string "the key event")
        (returns boolean))
  (define (global-key! key)
    (let ([hit (keymap:resolved-binding 'global (list key))])
      (and hit
           (begin
             (head:set-current-keys! (list key))
             (run-key-action! (keymap:binding-action (cdr hit)) #f)
             #t))))

  (define (dispatch-sequence! first)
    ;; Resolve a key sequence: the buffer's mode contexts first, nearest first, then the
    ;; global map. Once a prefix reaches e, the whole command stays here,
    ;; including synchronous prompts; an app cannot consume its suffix.
    (let* ([buffer (head:window-buffer (head:current-window))]
           [contexts (mode:key-contexts buffer)]
           [capture (exists keymap:context-capture contexts)])
      (let loop ([sequence (list first)])
        (let* ([in-context (exists (lambda (context) (keymap:resolved-binding context sequence)) contexts)]
               [context-prefix? (exists (lambda (context) (keymap:binding-prefix? context sequence)) contexts)]
               [hit (or in-context (keymap:resolved-binding 'global sequence))]
               [prefix? (or context-prefix?
                            (keymap:binding-prefix? 'global sequence))])
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
             (run-key-action! (keymap:binding-action (cdr hit)) capture)]
            [(and (= (length sequence) 1)
                  (tty:key-event-character first)
                  (or (exists (lambda (context) (keymap:resolved-binding context '("SELF-INSERT"))) contexts)
                      (keymap:resolved-binding 'global '("SELF-INSERT"))))
             ;; an unbound character goes to SELF-INSERT, the mode's context
             ;; first: its command receives the key through head:typed-text
             => (lambda (hit)
                  (head:set-current-keys! sequence)
                  (run-key-action! (keymap:binding-action (cdr hit)) capture))]
            [else
             (head:set-last-command! #f)
             (echo:set-text!
               (format "~a is undefined" (keymap:sequence-text sequence)))])))))

  (define (context-claims? event)
    ;; Whether the current buffer's mode context binds event, starts a
    ;; binding with it, or leaves it to e in partial capture. Such a key belongs
    ;; to the keymaps even inside a capturing app: the app's handler
    ;; sees only the keys its context leaves unbound, so a terminal
    ;; cannot swallow its capture control or a reserved editor prefix.
    (let ([contexts (mode:key-contexts (head:window-buffer (head:current-window)))])
      (and (pair? contexts)
           (let ([sequence (list event)] [capture (exists keymap:context-capture contexts)])
             (or (exists (lambda (context) (keymap:resolved-binding context sequence)) contexts)
                 (exists (lambda (context) (keymap:binding-prefix? context sequence)) contexts)
                 ;; a context binding SELF-INSERT claims every character
                 (and (tty:key-event-character event)
                      (exists (lambda (context) (keymap:resolved-binding context '("SELF-INSERT"))) contexts))
                 (and capture (not (head:full-capture? (head:current-window)))
                      (member event (cddr capture)))))
           #t)))

  (edoc "Dispatch one key from the pump: the current buffer's app has first refusal of keys its mode context leaves unbound, the rest go through the keymaps; eof quits."
        (input (or char string any) "a character, an event string, or eof")
        (prompts))
  (define (handle-key! input)
    ;; One key from the pump: a character or an event string, eof
    ;; when the terminal is gone.  The current buffer's app has first
    ;; refusal of every key its mode context leaves unbound; what it
    ;; declines, and what the context claims, goes through the keymaps.
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
         (if (and (not (context-claims? event))
                  (head:dispatch-app-event! event))
             (head:set-last-command! #f)
             (dispatch-sequence! event))])))

) ;; library (dispatch)
