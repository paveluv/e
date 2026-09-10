;; dispatch.e -- key dispatch for the head: the library (dispatch).
;;
;; A key sequence resolves through the current mode's context, then
;; the global map, and an unbound character through SELF-INSERT. The
;; current buffer's app handler has first refusal of the keys its
;; mode context leaves unbound. Commands that chain -- kills, typed
;; runs -- read head:last-command and head:current-keys, which the
;; dispatcher records for them. The main loop and modal readers use
;; the same entrypoint.

(library (dispatch)
  (export (rename (handle-key! key!)))
  (import (chezscheme)
          (prefix (head) head:)
          (prefix (paint) paint:)
          (prefix (echo) echo:)
          (prefix (keymap) keymap:)
          (prefix (tty) tty:)
          (prefix (mode) mode:))

  ;;; Key dispatch ---------------------------------------------------------------------

  (define (run-key-action! action)
    ;; Run a resolved binding's action and remember it as the last
    ;; command (an error still counts); an unbound key, or a context
    ;; action leaking into the global map, is reported and remembered
    ;; as no command at all.
    (cond [(procedure? action)
           (head:follow-app! (head:current) #f)
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
    (let* ([buffer (head:window-buffer (head:current))]
           [mode-context (mode:key-context buffer)]
           [escape (and mode-context (keymap:context-escape mode-context))])
      (define (resolve!)
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
                 ;; Declaring an escape makes it a prefix on its own; an app
                 ;; need not bind a dummy escaped command to keep it open.
                 [prefix? (or context-prefix?
                              (and escape (not in-context) (null? (cdr sequence))
                                   (string=? first escape))
                              (keymap:binding-prefix? 'global global))])
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
                 (format "~a is undefined" (keymap:sequence-text sequence)))]))))
      (if (and escape (string=? first escape))
          ;; the escape suspends the app's capture until the command it
          ;; introduces has run -- prefixes and synchronous prompts
          ;; included -- and the seat shows the buffer as escaped
          ;; throughout, for its status hint and cursor
          (let ([outer (head:escaped-buffer)])
            (dynamic-wind
              (lambda () (head:set-escaped-buffer! buffer))
              resolve!
              (lambda () (head:set-escaped-buffer! outer))))
          (resolve!))))

  (define (context-claims? event)
    ;; Whether the current buffer's mode context binds event, starts a
    ;; binding with it, or names it as the escape.  Such a key belongs
    ;; to the keymaps even inside a capturing app: the app's handler
    ;; sees only the keys its context leaves unbound, so a terminal
    ;; cannot swallow the C-] that is meant to get out of it.
    (let ([context (mode:key-context (head:window-buffer (head:current)))])
      (and context
           (let ([sequence (list event)])
             (or (keymap:resolved-binding context sequence)
                 (keymap:binding-prefix? context sequence)
                 (equal? event (keymap:context-escape context))))
           #t)))

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
