;; Widget key routing and chords, independent of the default window host.
(import (only (foundation edoc) elibrary))
(elibrary (head routing)
  (export cancel! feedback global-key! input! pending? resolve! (rename (run-key-action! run!)) set-prompt-opener!)
  (import (chezscheme) (prefix (head head) head:) (prefix (head keymap) keymap:)
    (prefix (head widget) widget:))

  (edoc "Optional transient key feedback supplied by the composition; a blank root has no message display." (value procedure))
  (define feedback (make-parameter (lambda (text) (void))))
  (define prompt-opener
    (lambda (name arguments) (error 'routing "no M-x prompt is installed to pre-fill" name)))

  (edoc "Install the procedure that opens M-x with a call begun: (open name arguments), the command's top-level name and the arguments already given; a key bound with keymap:prefill calls it."
        (open procedure "(open name arguments)"))
  (define (set-prompt-opener! open)
    (set! prompt-opener open))

  (edoc "Run a resolved key action and record the last command, including prefilled expressions through the installed prompt opener."
        (action any "resolved keymap action"))
  (define (run-key-action! action)
    ;; Run a resolved binding's action and remember it as the last
    ;; command (an error still counts); an unbound key, or a context
    ;; action leaking into the global map, is reported and remembered
    ;; as no command at all.  A call's producers run at the press, its
    ;; other arguments stand as given.
    (cond [(procedure? action)
           (dynamic-wind void action
             (lambda () (head:set-last-command! action)))]
          [(keymap:call-action? action)
           ;; a call built with keymap:call: the producers run at the press
           (dynamic-wind void
             (lambda () (keymap:run! action))
             (lambda () (head:set-last-command! action)))]
          [(keymap:prefill-action? action)
           ;; a pre-filled M-x built with keymap:prefill
           (dynamic-wind void
             (lambda ()
               (let ([name (keymap:prefill-name action)])
                 (unless name (error 'run! "the pre-filled command has no top-level name" action))
                 (apply prompt-opener name
                   (keymap:run! (keymap:call (apply list (keymap:prefill-action-arguments action)))))))
             (lambda () (head:set-last-command! action)))]
          [(not action)
           (head:set-last-command! #f)
           ((feedback) "Key is unbound")]
          [else
           (head:set-last-command! #f)
           (error 'run! "context action used globally" action)]))

  (edoc "Run the global map's command for one key as a command, the app handler bypassed: for a control standing in for a key, a wheel over an unfocused pane say. Whether the key was bound."
        (key string "the key event")
        (returns boolean))
  (define (global-key! key)
    (keymap:call-with-command! (lambda ()
                                 (let ([hit (keymap:resolved-binding 'global (list key))])
                                   (and hit
                                     (begin
                                       (head:set-current-keys! (list key))
                                       (run-key-action! (keymap:binding-action (cdr hit)))
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

  (edoc "Dispatch normalized key or committed text to an explicit widget root. Optional contexts belong to its outer host."
        (root model "active root") (event list "(key token text-fallback) or (text string source)")
        (contexts (list-of symbol) "outer host keymaps") (returns boolean))
  (define (input! root event . contexts)
    (keymap:call-with-command! (lambda ()
                                 (widget:cancel! root 'keyboard)
                                 (case (car event)
                                   [(text) (set! chord #f) (widget:input! root event)]
                                   [(key)
                                    (let* ([key (cadr event)] [routing (widget:key-scopes! root (if chord (car (list-ref chord 2)) key))]
                                           [reply (resolve! (car routing) (append (cadr routing) (if (null? contexts) '() (list (list 'editor contexts #f)))) key)]
                                           [receiver (cadr reply)] [sequence (list-ref reply 3)])
                                      (case (car reply)
                                        [(prefix) ((feedback) (string-append (keymap:sequence-text sequence) "-")) #t]
                                        [(command)
                                         (head:set-current-keys! sequence)
                                         (if (eq? receiver 'editor) (run-key-action! (caddr reply))
                                           (parameterize ([widget:target receiver])
                                             (if (symbol? (caddr reply)) (widget:act! receiver (caddr reply)) (run-key-action! (caddr reply))))) #t]
                                        [(cancelled invalid) ((feedback) "Key sequence cancelled") #t]
                                        [else
                                         (or (widget:input! root event) (eq? (car reply) 'blocked))]))]
                                   [else (error 'input! "expected key or text input" event)]))))

)
