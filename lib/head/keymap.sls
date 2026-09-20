;; keymap.sls -- key syntax and the binding tables: the library
;; (keymap).  Pure infrastructure with no init!;
;; dispatch lives in (dispatch).
;;
;; Every keyboard binding, including the command layer's defaults,
;; lives in one kernel registry.  An item is (context sequence action
;; kind spelling), where kind is user or default.  The registry
;; supplies its owner: config, a module (edit included), or #f for
;; live M-x customizations.  User entries always beat defaults;
;; within a layer the newest wins.  Contexts are symbols: 'global,
;; or a buffer mode's name for mode-local maps, or a synthetic scope
;; like 'isearch.

(import (only (edoc) elibrary))
(elibrary (keymap)
  (export (rename (key-spec spec)) sequence-text
          (rename (bind-key! bind!)) (rename (bind-default-key! bind-default!)) (rename (unbind-key! unbind!))
          (rename (key-binding binding)) (rename (key-event-binding event-binding)) binding-prefix?
          command-keys command-key command-hint
          sequence-bindings resolved-binding choose-binding
          binding-context binding-sequence binding-action
          binding-kind binding-spec same-sequence?
          set-context-capture! context-capture)
  (import (rnrs)
          (only (chezscheme)
                cons* format iota top-level-bound? top-level-value)
          (prefix (kernel) kernel:)
          (prefix (string) string:))

  ;;; Key syntax --------------------------------------------------------------

  (define special-key-names
    (append
      '("UP" "DOWN" "LEFT" "RIGHT" "HOME" "END" "BEGIN" "INSERT"
        "DELETE" "PAGEUP" "PAGEDOWN" "TAB" "RET" "ESC" "BACKSPACE"
        ;; the pseudo-keys the dispatcher resolves: a bracketed paste,
        ;; a mode's click action, the unbound character's self-insert
        "PASTE" "MOUSE-CLICK" "SELF-INSERT")
      (map (lambda (number) (format "F~a" (+ number 1))) (iota 63))
      '("KP-0" "KP-1" "KP-2" "KP-3" "KP-4" "KP-5" "KP-6"
        "KP-7" "KP-8" "KP-9" "KP-DECIMAL" "KP-DIVIDE" "KP-MULTIPLY"
        "KP-SUBTRACT" "KP-ADD" "KP-COMMA" "KP-EQUAL" "KP-ENTER")))

  (define special-key-prefixes
    '("C-M-S-" "C-M-" "C-S-" "M-S-" "C-" "M-" "S-"))

  (define (special-key-name? name)
    (or (member name special-key-names)
        (exists
          (lambda (prefix)
            (and (string:prefix? prefix name)
                 (member (string:tail name (string-length prefix))
                         special-key-names)))
          special-key-prefixes)))

  (define (key-token s)
    (cond
      [(string=? s "SPC") " "]
      [(string=? s "TAB") "TAB"]
      [(string=? s "RET") "RET"]
      [(string=? s "ESC") "ESC"]
      [(string=? s "DEL") "DELETE"]
      [(string=? s "BACKSPACE") "BACKSPACE"]
      [(and (= (string-length s) 3) (string:prefix? "C-" s))
       (format "C-~c" (char-downcase (string-ref s 2)))]
      [(and (= (string-length s) 3) (string:prefix? "M-" s))
       (format "M-~c" (string-ref s 2))]
      [(and (> (string-length s) 3) (string:prefix? "M-" s))
       (let ([base (key-token (string:tail s 2))])
         (string-append "M-" (if (string=? base " ") "SPC" base)))]
      [(and (= (string-length s) 5) (string:prefix? "C-M-" s))
       (format "C-M-~c" (char-downcase (string-ref s 4)))]
      [(= (string-length s) 1) s]
      [(special-key-name? s) s]
      [else (error 'bind-key! "unrecognized key" s)]))

  (edoc "A human key spelling, C-x C-f say, as its canonical event tokens."
        (spec key "the spelling")
        (returns (list-of string)))
  (define (key-spec spec)
    ;; a human spelling -- "C-x C-f" -- into canonical event tokens
    (unless (and (string? spec) (> (string-length spec) 0))
      (error 'bind-key! "key specification must be a nonempty string" spec))
    (let ([n (string-length spec)])
      (let loop ([i 0] [start 0] [parts '()])
        (cond
          [(= i n)
           (reverse (cons (key-token (substring spec start i)) parts))]
          [(char=? (string-ref spec i) #\space)
           (when (= i start) (error 'bind-key! "empty key in sequence" spec))
           (loop (+ i 1) (+ i 1)
                 (cons (key-token (substring spec start i)) parts))]
          [else (loop (+ i 1) start parts)]))))

  (edoc "Event tokens spelled as one key sequence, space-separated."
        (sequence (list-of string) "the tokens")
        (returns string))
  (define (sequence-text sequence)
    (string:join sequence " "))

  ;; A key spelling as an edoc type: completion offers the spellings bound
  ;; in the global map now.
  (edoc-type key "a key spelling, C-x C-f say"
    (predicate (lambda (v) (and (string? v) (guard (ex [else #f]) (key-spec v) #t))))
    (complete (lambda (partial)
                (map (lambda (owned) (cons (binding-spec (cdr owned)) #f)) (effective-bindings 'global))))
    (write (lambda (v) (call-with-string-output-port (lambda (p) (write v p))))))


  ;;; The binding table ---------------------------------------------------------

  (define key-bindings (kernel:make-registry))

  (define (binding-item context sequence action kind spec)
    (list context sequence action kind spec))
  (edoc "The keymap context of a binding."
        (b list "the binding")
        (returns symbol))
  (define (binding-context b)
    (car b))

  (edoc "The event tokens of a binding."
        (b list "the binding")
        (returns (list-of string)))
  (define (binding-sequence b)
    (cadr b))

  (edoc "What a binding runs: a procedure, a symbol for a keymap action, or #f when it unbinds."
        (b list "the binding")
        (returns (or procedure symbol #f)))
  (define (binding-action b)
    (caddr b))

  (edoc "Whether a binding is a user or default one."
        (b list "the binding")
        (returns (one-of user default)))
  (define (binding-kind b)
    (cadddr b))

  (edoc "The spelling a binding was made with."
        (b list "the binding")
        (returns string))
  (define (binding-spec b)
    (car (cddddr b)))

  (edoc "Whether two key sequences spell the same events."
        (a (list-of string) "one sequence")
        (b (list-of string) "the other")
        (returns boolean))
  (define (same-sequence? a b)
    (and (= (length a) (length b))
         (for-all string=? a b)))

  (define (sequence-prefix? prefix whole)
    (and (<= (length prefix) (length whole))
         (let loop ([a prefix] [b whole])
           (or (null? a)
               (and (string=? (car a) (car b))
                    (loop (cdr a) (cdr b)))))))

  (define (matching-bindings context sequence exact?)
    (filter
      (lambda (owned)
        (let ([b (cdr owned)])
          (and (eq? (binding-context b) context)
               ((if exact? same-sequence? sequence-prefix?)
                sequence (binding-sequence b)))))
      (kernel:registry-entries key-bindings)))

  (edoc "The binding that wins among owned entries: a user binding, else a default."
        (entries list "(owner . binding) entries")
        (returns (or pair #f)))
  (define (choose-binding entries)
    (or (find (lambda (owned) (eq? (binding-kind (cdr owned)) 'user))
              entries)
        (find (lambda (owned) (eq? (binding-kind (cdr owned)) 'default))
              entries)))

  (edoc "The winning owned binding of a key sequence in a context, or #f."
        (context symbol "the keymap context")
        (sequence (list-of string) "the event tokens")
        (returns (or pair #f)))
  (define (resolved-binding context sequence)
    (choose-binding (matching-bindings context sequence #t)))

  (edoc "Every owned binding whose spelling matches a sequence exactly, in any context: describe-key's raw material."
        (sequence (list-of string) "the event tokens")
        (returns list))
  (define (sequence-bindings sequence)
    ;; every owned entry whose spelling matches the sequence exactly,
    ;; any context: ((owner . item) ...) -- describe-key's raw material
    (filter
      (lambda (owned)
        (same-sequence? sequence (binding-sequence (cdr owned))))
      (kernel:registry-entries key-bindings)))

  (define (effective-bindings context)
    ;; One chosen entry per sequence.  Registry order handles newest-first;
    ;; a user entry replaces a previously seen default regardless of age.
    (let ([chosen (make-hashtable equal-hash equal?)]
          [entries (kernel:registry-entries key-bindings)])
      (for-each
        (lambda (owned)
          (let ([b (cdr owned)])
            (when (eq? (binding-context b) context)
              (let* ([sequence (binding-sequence b)]
                     [old (hashtable-ref chosen sequence #f)])
                (when (or (not old)
                          (and (eq? (binding-kind (cdr old)) 'default)
                               (eq? (binding-kind b) 'user)))
                  (hashtable-set! chosen sequence owned))))))
        entries)
      ;; Keep registry order (newest first), using wrappers from this one
      ;; snapshot. Public registry reads return fresh ownership wrappers.
      (filter (lambda (owned)
                (eq? owned (hashtable-ref chosen (binding-sequence (cdr owned)) #f)))
              entries)))

  (edoc "The action bound to a key spelling in a context, or #f."
        (context symbol "the keymap context")
        (spec key "the spelling")
        (returns (or procedure symbol #f)))
  (define key-binding
    (case-lambda
      [(spec)
       (key-binding 'global spec)]
      [(context spec)
       (let ([hit (resolved-binding context (key-spec spec))])
         (and hit (binding-action (cdr hit))))]))

  (edoc "The action bound to runtime events in a context, or #f; events are canonical tokens, not spellings."
        (context symbol "the keymap context")
        (events (list-of string) "the event tokens")
        (returns (or procedure symbol #f)))
  (define (key-event-binding context . events)
    ;; Runtime events are already canonical tokens.  Do not feed them
    ;; back through the human key-spec parser: its spaces are separators,
    ;; while a typed space is itself the literal " " event.
    (let ([hit (resolved-binding context events)])
      (and hit (binding-action (cdr hit)))))

  (edoc "Whether a sequence begins a longer binding in a context, so more keys should be read."
        (context symbol "the keymap context")
        (sequence (list-of string) "the event tokens")
        (returns boolean))
  (define (binding-prefix? context sequence)
    (let ([exact (resolved-binding context sequence)])
      (exists
        (lambda (owned)
          (let* ([candidate (cdr owned)]
                 [longer (binding-sequence candidate)])
            (and (> (length longer) (length sequence))
                 (sequence-prefix? sequence longer)
                 (binding-action candidate)
                 ;; An exact user binding deliberately reclaims a key
                 ;; that used to be only a default prefix.
                 (or (not exact)
                     (eq? (binding-kind (cdr exact)) 'default)
                     (eq? (binding-kind candidate) 'user)))))
        (effective-bindings context))))

  (define (add-key-binding! context spec action kind)
    (unless (symbol? context)
      (error 'bind-key! "context must be a symbol" context))
    (unless (or (procedure? action) (symbol? action) (string? action) (not action))
      (error 'bind-key! "action must be a procedure, symbol, or #f" action))
    (kernel:registry-add! key-bindings
                          (binding-item context (key-spec spec) action
                                        kind spec)))

  (edoc "Bind a key spelling as a user binding, which wins over defaults: in a context, or in the global map when none is given."
        (context symbol "the keymap context")
        (spec key "the spelling")
        (action (or procedure symbol string) "the command, a keymap action, or an expression to evaluate at the top level, (edit:kill-buffer! (head:current-buffer)) say"))
  (define bind-key!
    (case-lambda
      [(spec action)
       (add-key-binding! 'global spec action 'user)]
      [(context spec action)
       (add-key-binding! context spec action 'user)]))

  (edoc "Bind a key spelling as a module's default, which user bindings override: in a context, or in the global map when none is given."
        (context symbol "the keymap context")
        (spec key "the spelling")
        (action (or procedure symbol string) "the command, a keymap action, or an expression to evaluate at the top level, (edit:kill-buffer! (head:current-buffer)) say"))
  (define bind-default-key!
    (case-lambda
      [(spec action)
       (add-key-binding! 'global spec action 'default)]
      [(context spec action)
       (add-key-binding! context spec action 'default)]))

  (edoc "Unbind a key spelling as a user override, in a context or in the global map when none is given."
        (context symbol "the keymap context")
        (spec key "the spelling"))
  (define unbind-key!
    (case-lambda
      [(spec)
       (add-key-binding! 'global spec #f 'user)]
      [(context spec)
       (add-key-binding! context spec #f 'user)]))

  ;;; Capture controls -----------------------------------------------------------

  ;; Capture belongs to the app; full/partial capture is a window preference.
  ;; A context declares the control and keys left to e in partial capture.
  ;; The toggle is an ordinary binding, but does not pause following the app's cursor.
  ;; Both declarations retract with their owning module on reload.
  (define context-captures (kernel:make-registry))

  (edoc "Declare a context's capture control: the key that toggles full capture, and the keys left to the editor in partial capture."
        (context symbol "the keymap context")
        (spec string "the toggle key")
        (toggle procedure "the toggle command")
        (keys (list-of string) "the keys the editor keeps"))
  (define (set-context-capture! context spec toggle keys)
    (define (single-key spec)
      (let ([tokens (key-spec spec)])
        (unless (null? (cdr tokens))
          (error 'set-context-capture! "expected a single key" spec))
        (car tokens)))
    (unless (and (symbol? context) (procedure? toggle) (list? keys))
      (error 'set-context-capture! "expected a context, toggle procedure, and key list" context toggle keys))
    (let ([key (single-key spec)] [keys (map single-key keys)])
      (bind-default-key! context spec toggle)
      (kernel:registry-add! context-captures (cons* context key toggle keys))))

  (edoc "A context's capture policy, (toggle-key toggle-procedure editor-key ...), or #f."
        (context symbol "the keymap context")
        (returns (or list #f)))
  (define (context-capture context)
    ;; (toggle-key toggle-procedure editor-key ...) or #f. Return owned
    ;; key strings so a caller cannot change a registered capture policy.
    (cond [(kernel:registry-find context-captures (lambda (entry) (eq? (car entry) context)))
           => (lambda (entry) (cons* (string-copy (cadr entry)) (caddr entry)
                                (map string-copy (cdddr entry))))]
          [else #f]))

  ;;; Reverse lookup -------------------------------------------------------------

  (edoc "Every global key spelling currently bound to the top-level command named sym, read live."
        (sym symbol "the command's name")
        (returns (list-of string)))
  (define (command-keys sym)
    ;; Every global key spec currently resolved to the top-level command
    ;; named sym. Bindings are read live, so overrides and module reloads are
    ;; reflected immediately.
    (guard (ex [else '()])
      (let ([proc (and (top-level-bound? sym) (top-level-value sym))])
        (if (procedure? proc)
            (map (lambda (owned) (binding-spec (cdr owned)))
                 (filter (lambda (owned) (eq? (binding-action (cdr owned)) proc))
                         (effective-bindings 'global)))
            '()))))

  (edoc "The most recently registered key bound to the command named sym, or #f."
        (sym symbol "the command's name")
        (returns (or string #f)))
  (define (command-key sym)
    ;; The most recently registered key currently bound to sym, or #f.
    (let ([keys (command-keys sym)])
      (and (pair? keys) (car keys))))

  (edoc "Command names with their current keys, M-n next-conflict! say, comma-separated; bare when unbound."
        (syms (list-of symbol) "the command names")
        (returns string))
  (define (command-hint syms)
    ;; "M-n next-conflict!, M-m keep-mine!" for a list of command
    ;; names: each with its current key, or bare when unbound.
    (string:join
      (map (lambda (s)
             (let ([k (command-key s)])
               (if k (format "~a ~a" k s) (format "~a" s))))
           syms)
      ", ")))
