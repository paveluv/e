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
  (export cancel! global-key! input! (rename (handle-key! key!)) pending? resolve! set-prompt-opener!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head widget) widget:)
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
             (unless (widget:target) (head:follow-app! (head:current-window) #f)))
           (dynamic-wind void action
             (lambda () (head:set-last-command! action)))]
          [(keymap:call-action? action)
           ;; a call built with keymap:call: the producers run at the press
           (unless (widget:target) (head:follow-app! (head:current-window) #f))
           (dynamic-wind void
             (lambda () (keymap:run! action))
             (lambda () (head:set-last-command! action)))]
          [(keymap:prefill-action? action)
           ;; a pre-filled M-x built with keymap:prefill
           (unless (widget:target) (head:follow-app! (head:current-window) #f))
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
    (keymap:call-with-command! (lambda ()
                                 (let ([hit (keymap:resolved-binding 'global (list key))])
                                   (and hit
                                     (begin
                                       (head:set-current-keys! (list key))
                                       (run-key-action! (keymap:binding-action (cdr hit)) #f)
                                       #t))))))

  ;; One chord for this pump, shared by ordinary dispatch and prompt readers.
  ;; Its receiver owns every suffix; stale/invalid suffixes are never replayed.
  (define chord #f)

  (edoc "Discard a pending key chord when an interaction begins, ends or loses its input device.")
  (define (cancel!) (set! chord #f))

  (edoc "Whether a key prefix is waiting for one more event, without reading input." (returns boolean))
  (define (pending?) (and chord #t))

  (edoc "Advance one key through ordered receiver contexts. Return (status receiver action sequence); no input is read here."
        (owner any "stable routing basis; changes cancel the pending chord")
        (scopes list "(receiver contexts stop-on-unhandled?) entries") (key string "canonical key token") (returns list))
  (define (resolve! owner scopes key)
    (let* ([old chord] [sequence (append (if old (list-ref old 2) '()) (list key))]
           [stale? (and old (or (not (equal? owner (car old))) (not (= (keymap:generation) (cadr old)))))] )
      (set! chord #f)
      (if stale? (list 'cancelled #f #f sequence)
        ;; A prefix reserves the sequence, not every suffix in its first
        ;; receiver. Keep the original routing path so a local C-x D can
        ;; coexist with the host's C-x b; focus/reload still fence the chord.
        (let loop ([rest (if old (list-ref old 3) scopes)])
          (if (null? rest) (list (if old 'invalid 'unhandled) #f #f sequence)
            (let* ([scope (car rest)] [receiver (car scope)] [contexts (cadr scope)]
                   [match (exists (lambda (context)
                                    (let ([hit (keymap:resolved-binding context sequence)] [prefix? (keymap:binding-prefix? context sequence)])
                                      (and (or hit prefix?) (list hit prefix?)))) contexts)])
              (cond
                [(and match (cadr match))
                 (set! chord (list owner (keymap:generation) sequence (if old (list-ref old 3) scopes)))
                 (list 'prefix receiver #f sequence)]
                [match (list 'command receiver (keymap:binding-action (cdar match)) sequence)]
                [(caddr scope) (list (if old 'invalid 'blocked) receiver #f sequence)]
                [else (loop (cdr rest))])))))))

  (define (dispatch-sequence! first)
    (let* ([w (head:current-window)] [buffer (head:window-buffer w)] [contexts (mode:key-contexts buffer)]
           [capture (exists keymap:context-capture contexts)]
           [result (resolve! (list w buffer contexts) (list (list 'editor (append contexts '(global)) #f)) first)]
           [status (car result)] [sequence (list-ref result 3)])
      (case status
        [(prefix) (echo:set-text! (string-append (keymap:sequence-text sequence) "-")) (echo:set-pending! '())]
        [(command)
         (head:set-current-keys! sequence) (run-key-action! (caddr result) capture)]
        [(unhandled)
         (let ([hit (and (tty:key-event-character first)
                      (or (exists (lambda (context) (keymap:resolved-binding context '("SELF-INSERT"))) contexts)
                        (keymap:resolved-binding 'global '("SELF-INSERT"))))])
           (if hit (begin (head:set-current-keys! sequence) (run-key-action! (keymap:binding-action (cdr hit)) capture))
             (begin (head:set-last-command! #f) (echo:set-text! (format "~a is undefined" (keymap:sequence-text sequence))))))]
        [else (head:set-last-command! #f) (echo:set-text! "Key sequence cancelled")])) )

  (edoc "Dispatch normalized key or committed text to an explicit widget root. Optional contexts belong to its outer host."
        (root list "active root") (event list "(key token text-fallback) or (text string source)")
        (contexts (list-of symbol) "outer host keymaps") (returns boolean))
  (define (input! root event . contexts)
    (keymap:call-with-command! (lambda ()
                                 (widget:cancel! root 'keyboard)
                                 (case (car event)
                                   [(text) (set! chord #f) (widget:input! root event)]
                                   [(key)
                                    (let* ([key (cadr event)] [routing (widget:key-scopes! root key)]
                                           [reply (resolve! (car routing) (append (cadr routing) (if (null? contexts) '() (list (list 'editor contexts #f)))) key)]
                                           [receiver (cadr reply)] [sequence (list-ref reply 3)])
                                      (case (car reply)
                                        [(prefix) (echo:set-text! (string-append (keymap:sequence-text sequence) "-")) #t]
                                        [(command)
                                         (head:set-current-keys! sequence)
                                         (if (eq? receiver 'editor) (run-key-action! (caddr reply) #f)
                                           (parameterize ([widget:target receiver])
                                             (if (symbol? (caddr reply)) (widget:act! receiver (caddr reply)) (run-key-action! (caddr reply) #f)))) #t]
                                        [(cancelled invalid) (echo:set-text! "Key sequence cancelled") #t]
                                        [else
                                         (or (widget:input! root event) (eq? (car reply) 'blocked))]))]
                                   [else (error 'input! "expected key or text input" event)]))))

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
  )
  (define (handle-key! input)
    (keymap:call-with-command! (lambda ()
                                 ;; One key from the pump: a character or an event string, eof
                                 ;; when the terminal is gone.  The current buffer's app has first
                                 ;; refusal of every key its mode context leaves unbound; what it
                                 ;; declines, and what the context claims, goes through the keymaps.
                                 (let ([event (cond [(eof-object? input) input]
                                                [(char? input) (tty:character-event input)]
                                                [else input])])
                                   (cond
                                     [(eof-object? event) (cancel!) (head:quit!)]
                                     [(string=? event "MOUSE-HANDLED")
                                      (cancel!)
                                      (echo:settle!)
                                      (void)]
                                     [(head:window-widget (head:current-window))
                                      => (lambda (root)
                                           (echo:settle!)
                                           (input! root (if (string=? event "PASTE") (list 'text (head:read-paste) 'paste)
                                                          (list 'key event (let ([c (tty:key-event-character event)]) (and c (string c))))) 'global))]
                                     [else
                                      (echo:settle!)
                                      (if (and (not (pending?)) (not (context-claims? event))
                                            (head:dispatch-app-event! event))
                                        (head:set-last-command! #f)
                                        (dispatch-sequence! event))])))))

) ;; library (dispatch)
