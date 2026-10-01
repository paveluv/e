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
  (export input-root (rename (handle-key! key!)) register-input-root!)
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (head echo) echo:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head routing) routing:)
          (prefix (head seat) seat:)
          (prefix (sys tty) tty:))

  ;;; Key dispatch ---------------------------------------------------------------------

  (define input-roots (kernel:make-registry))

  (edoc "Register an outer host's temporary keyboard receiver. Called at input boundaries with the ordinary root and normalized event; return a mounted replacement root or false. The receiver uses ordinary widget routing, and unclaimed keys retain global host bindings."
        (resolve procedure "(ordinary-root event) -> root or false")
        (inspect procedure "pure ordinary-root -> root or false, for binding inspection"))
  (define (register-input-root! resolve inspect)
    (unless (and (procedure? resolve) (procedure? inspect)) (error 'register-input-root! "expected routing and inspection procedures"))
    (kernel:registry-add! input-roots (cons resolve inspect)))
  (define (input-root! event)
    (let ([ordinary (seat:window-widget (seat:current-window))])
      (or (exists (lambda (entry) ((car entry) ordinary event)) (kernel:registry-items input-roots)) ordinary)))

  (edoc "Inspect the active temporary keyboard root without dispatching an event, accepting an interaction or retargeting it."
        (returns (or model #f)) (effects internal))
  (define (input-root)
    (let ([ordinary (seat:window-widget (seat:current-window))])
      (or (exists (lambda (entry) ((cdr entry) ordinary)) (kernel:registry-items input-roots)) ordinary)))

  (define (dispatch-sequence! first)
    (let* ([w (seat:current-window)] [buffer (seat:window-buffer w)] [contexts (mode:key-contexts buffer)]
           [result (routing:resolve! (list w buffer contexts) (list (list 'editor (append contexts '(global)) #f)) first)]
           [status (car result)] [sequence (list-ref result 3)])
      (case status
        [(prefix) (echo:set-text! (string-append (keymap:sequence-text sequence) "-")) (echo:set-pending! '())]
        [(command)
         (head:set-current-keys! sequence) (routing:run! (caddr result))]
        [(unhandled)
         (let ([hit (and (tty:key-event-character first)
                      (or (exists (lambda (context) (keymap:resolved-binding context '("SELF-INSERT"))) contexts)
                        (keymap:resolved-binding 'global '("SELF-INSERT"))))])
           (if hit (begin (head:set-current-keys! sequence) (routing:run! (keymap:binding-action (cdr hit))))
             (begin (head:set-last-command! #f) (echo:set-text! (format "~a is undefined" (keymap:sequence-text sequence))))))]
        [else (head:set-last-command! #f) (echo:set-text! "Key sequence cancelled")])) )

  (define (context-claims? event)
    ;; Whether the current buffer's mode context binds event, starts a
    ;; binding with it. Legacy local app handlers see only keys their mode
    ;; contexts leave unbound. Widget capture uses its recursive route.
    (let ([contexts (mode:key-contexts (seat:window-buffer (seat:current-window)))])
      (and (pair? contexts)
           (let ([sequence (list event)])
             (or (exists (lambda (context) (keymap:resolved-binding context sequence)) contexts)
                 (exists (lambda (context) (keymap:binding-prefix? context sequence)) contexts)
                 ;; a context binding SELF-INSERT claims every character
                 (and (tty:key-event-character event)
                      (exists (lambda (context) (keymap:resolved-binding context '("SELF-INSERT"))) contexts))))
           #t)))

  (edoc "Dispatch one key from the pump: the current buffer's app has first refusal of keys its mode context leaves unbound, the rest go through the keymaps; eof quits."
        (input (or char string any) "a character, an event string, or eof")
  )
  (define (handle-key! input)
    (parameterize ([routing:feedback echo:set-text!])
      (keymap:call-with-command! (lambda ()
                                   ;; One key from the pump: a character or an event string, eof
                                   ;; when the terminal is gone.  The current buffer's app has first
                                   ;; refusal of every key its mode context leaves unbound; what it
                                   ;; declines, and what the context claims, goes through the keymaps.
                                   (let ([event (cond [(eof-object? input) input]
                                                  [(char? input) (tty:character-event input)]
                                                  [else input])])
                                     (cond
                                       [(eof-object? event) (routing:cancel!) (head:quit!)]
                                       [(string=? event "MOUSE-HANDLED")
                                        (routing:cancel!)
                                        (echo:settle!)
                                        (void)]
                                       [(input-root! event)
                                        => (lambda (root)
                                             (echo:settle!)
                                             (routing:input! root (if (string=? event "PASTE") (list 'text (head:read-paste) 'paste)
                                                                    (list 'key event (let ([c (tty:key-event-character event)]) (and c (string c))))) 'global))]
                                       [else
                                        (echo:settle!)
                                        (if (and (not (routing:pending?)) (not (context-claims? event))
                                              (seat:dispatch-app-event! event))
                                          (head:set-last-command! #f)
                                          (dispatch-sequence! event))])))))

  )
) ;; library (dispatch)
