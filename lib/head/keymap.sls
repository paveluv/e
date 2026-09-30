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

(import (only (foundation edoc) elibrary))
(elibrary (head keymap)
  (export action-text action-trace (rename (bind-key! bind!))
    (rename (bind-default-key! bind-default!))
    (rename (key-binding binding)) binding-action
    binding-context binding-kind binding-prefix?
    binding-sequence binding-spec call call-action-arguments
    call-action-procedure call-action? call-with-command! choose-binding
    command-hint command-key command-keys command-state
    (rename (effective-bindings context-bindings))
    (rename (key-event-binding event-binding))
    generation prefill prefill-action-arguments
    prefill-action-procedure prefill-action? prefill-name
    prefill-text resolved-binding run! same-sequence?
    sequence-bindings sequence-text
    (rename (key-spec spec)) (rename (unbind-key! unbind!)))
  (import (rnrs)
          (only (chezscheme)
                cons* format iota top-level-bound? top-level-value environment-symbols interaction-environment
                procedure-arity-mask logbit? make-parameter parameterize)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation string) string:))

  (define command-turn 0)
  (define in-command? (make-parameter #f))

  (edoc "Read this head's input-command generation and whether it is currently executing, as a pair. Controls can end local typing continuity without inspecting bindings or adding view traffic."
        (returns pair))
  (define (command-state) (cons command-turn (in-command?)))

  (edoc "Execute one input command boundary. Nested routing shares the boundary; an unbound or no-op input still separates editing runs."
        (thunk thunk "input dispatch") (returns any))
  (define (call-with-command! thunk)
    (if (in-command?) (thunk)
      (begin
        (set! command-turn (+ command-turn 1))
        (parameterize ([in-command? #t]) (thunk)))))

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

  ;; The short spellings of the long-named keys, accepted under any
  ;; modifiers: S-PGUP binds the shifted PAGEUP.
  (define key-aliases '(("BS" . "BACKSPACE") ("DEL" . "DELETE") ("PGUP" . "PAGEUP") ("PGDN" . "PAGEDOWN")))

  (define (expand-alias s)
    ;; a spelling with its short key name written long, modifiers kept
    (let loop ([prefix ""] [rest s])
      (cond
        [(find (lambda (m) (and (string:prefix? m rest) (> (string-length rest) (string-length m)))) '("C-" "M-" "S-"))
         => (lambda (m) (loop (string-append prefix m) (string:tail rest (string-length m))))]
        [(assoc rest key-aliases) => (lambda (hit) (string-append prefix (cdr hit)))]
        [else s])))

  (define (key-token spelled)
    (define s (expand-alias spelled))
    (cond
      [(string=? s "SPC") " "]
      [(string=? s "TAB") "TAB"]
      [(string=? s "RET") "RET"]
      [(string=? s "ESC") "ESC"]
      [(string=? s "BACKSPACE") "BACKSPACE"]
      [(and (= (string-length s) 3) (string:prefix? "C-" s))
       (format "C-~c" (char-downcase (string-ref s 2)))]
      [(and (= (string-length s) 3) (string:prefix? "M-" s))
       (format "M-~c" (string-ref s 2))]
      [(and (> (string-length s) 3) (string:prefix? "M-" s))
       (let ([base (key-token (string:tail s 2))])
         (string-append "M-" (if (string=? base " ") "SPC" base)))]
      [(and (> (string-length s) 5) (string:prefix? "C-M-" s))
       (let ([base (key-token (string:tail s 4))])
         (string-append "C-M-" (if (string=? base " ") "SPC" base)))]
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

  ;; The short spellings the long-named keys show under, SPC for the
  ;; space; either spelling binds.
  (define short-names
    '(("BACKSPACE" . "BS") ("DELETE" . "DEL") ("PAGEUP" . "PGUP") ("PAGEDOWN" . "PGDN") (" " . "SPC")
      ("SELF-INSERT" . "any character")))

  (define (token-text token)
    ;; a token as shown: its modifiers, then the key's short name
    (let loop ([prefix ""] [rest token])
      (cond
        [(find (lambda (m) (and (string:prefix? m rest) (> (string-length rest) (string-length m)))) '("C-" "M-" "S-"))
         => (lambda (m) (loop (string-append prefix m) (string:tail rest (string-length m))))]
        [(assoc rest short-names) => (lambda (hit) (string-append prefix (cdr hit)))]
        [else token])))

  (edoc "Event tokens spelled as one key sequence, space-separated, the long-named keys short: BS, DEL, PGUP, PGDN and SPC; SELF-INSERT reads any character."
        (sequence (list-of string) "the tokens")
        (returns string))
  (define (sequence-text sequence)
    (string:join (map token-text sequence) " "))

  ;; A key spelling as an edoc type: completion offers the spellings bound
  ;; in the global map now.
  (edoc-type key "a key spelling, C-x C-f say"
    (predicate (lambda (v) (and (string? v) (guard (ex [else #f]) (key-spec v) #t))))
    (complete (lambda (partial)
                (map (lambda (owned) (cons (binding-spec (cdr owned)) #f)) (effective-bindings 'global))))
    (write (lambda (v) (call-with-string-output-port (lambda (p) (write v p))))))


  ;;; The binding table ---------------------------------------------------------

  (define key-bindings (kernel:make-registry))
  (define binding-generation 0)
  (define binding-observer
    (kernel:call-with-runtime-registrations
      (lambda () (kernel:registry-observe! key-bindings
                   (lambda (removed added) (set! binding-generation (+ binding-generation 1)))))))

  (edoc "The current binding generation; pending chords use it to reject suffixes after a reload." (returns integer))
  (define (generation) binding-generation)

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

  (edoc "What a binding runs: a procedure, a structured call or prefill, a symbol for a keymap action, or #f when it unbinds."
        (b list "the binding")
        (returns (or procedure (record call-action) (record prefill-action) symbol #f)))
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

  (edoc "A context's live bindings, one owned entry per key sequence, a user binding winning over a default: the keys helper's raw material."
        (context symbol "the keymap context, global or a mode's")
        (returns list))
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

  ;;; Structured actions ------------------------------------------------------

  ;; Besides a procedure, a key may be bound to a call whose arguments are
  ;; produced when it is pressed, or to a pre-filled M-x. Both are built from
  ;; the procedures themselves, never from spelled names, and describe
  ;; themselves by the names the top level gives those procedures, so a
  ;; rename follows and C-h k shows the call as it runs.
  (edoc "A key action calling a procedure with constants and the results of producers or nested calls when the key is pressed."
        (procedure procedure "the command to call")
        (arguments (list-of any) "constants, producer procedures or nested calls"))
  (define-record-type (call-action make-call-action call-action?)
    (fields (immutable procedure call-action-procedure) (immutable arguments call-action-arguments)))

  (edoc "A key action that opens M-x with a call typed up to its next argument, so completion does the asking."
        (procedure procedure "the command the call names")
        (arguments (list-of datum) "the arguments already given, spelled into the text"))
  (define-record-type (prefill-action make-prefill-action prefill-action?)
    (fields (immutable procedure prefill-action-procedure) (immutable arguments prefill-action-arguments)))

  (edoc "Bind a key to a call: apply the command at the key press to the results of procedure producers or nested keymap:call expressions, and other arguments as given."
        (procedure procedure "the command to call")
        (producers (list-of any) "arguments: procedures and nested calls are evaluated at the press; other values stand as they are")
        (returns (record call-action)))
  (define-syntax call
    (syntax-rules (apply)
      [(_ (apply procedure arguments)) (apply make-call (edoc:forward-callee procedure) arguments)]
      [(_ procedure producer ...) (make-call (edoc:forward-callee procedure) producer ...)]))

  (define (make-call procedure . producers)
    (unless (procedure? procedure)
      (error 'call "expected a procedure" procedure))
    (make-call-action procedure producers))

  (edoc "Execute a structured key call, evaluating procedure producers and nested calls only now. Describing a call never evaluates it."
        (action (record call-action) "call to execute") (returns any))
  (define (run! action)
    (apply (call-action-procedure action)
      (map (lambda (p) (cond [(call-action? p) (run! p)] [(procedure? p) (p)] [else p]))
        (call-action-arguments action))))

  (edoc "Bind a key to a pre-filled M-x: the command's call typed up to its next argument, (keymap:prefill edit:answer!) say, the given arguments spelled first."
        (procedure procedure "the command the call names")
        (arguments (list-of datum) "the arguments already given")
        (returns (record prefill-action)))
  (define (prefill procedure . arguments)
    (unless (procedure? procedure) (error 'prefill "expected a procedure" procedure))
    (make-prefill-action procedure arguments))

  (define (top-level-name procedure)
    ;; the symbol the editor's top level binds to a procedure, or #f
    (let ([sym (or (edoc:forwarding-name procedure)
                 (find (lambda (s) (and (top-level-bound? s) (eq? (top-level-value s) procedure)))
                   (environment-symbols (interaction-environment))))])
      (and sym (symbol->string sym))))

  (define (spell procedure index value)
    (edoc:type-spelling (edoc:call-argument-type (edoc:edoc-of procedure) index) value))

  (edoc "The top-level name of the command a pre-filled M-x calls, or #f while it has none."
        (action (record prefill-action) "the pre-fill")
        (returns (or symbol #f)))
  (define (prefill-name action)
    (let ([name (top-level-name (prefill-action-procedure action))])
      (and name (string->symbol name))))

  (edoc "The text a pre-filled M-x starts with: the call up to its next argument, (edit:answer!  with the trailing space."
        (action (record prefill-action) "the pre-fill")
        (returns string))
  (define (prefill-text action)
    (string-append "(" (action-text (prefill-action-procedure action))
                   (apply string-append (map (lambda (v i) (string-append " " (spell (prefill-action-procedure action) i v)))
                                          (prefill-action-arguments action) (iota (length (prefill-action-arguments action)))))
                   " "))

  (edoc "How a key action reads: a procedure by its top-level name, a call as the expression it runs, a pre-filled M-x as M-x and its text, a keymap action by name; unbound and anonymous say so."
        (action any "the action")
        (bindings (list-of list) "optional alist of producer procedures to known values; substitutes without invoking them")
        (returns string))
  (define (action-text action . bindings)
    (define substitutions (if (null? bindings) '() (car bindings)))
    (define (describe action)
      (cond [(not action) "unbound"]
        [(symbol? action) (symbol->string action)]
        [(call-action? action)
         (string-append "(" (describe (call-action-procedure action))
                        (apply string-append
                          (map (lambda (p i)
                                 (string-append " "
                                   (cond [(call-action? p) (describe p)]
                                     [(procedure? p) (cond [(assq p substitutions) => (lambda (v) (spell (call-action-procedure action) i (cdr v)))]
                                                       [else (string-append "(" (describe p) ")")])]
                                     [else (spell (call-action-procedure action) i p)])))
                               (call-action-arguments action) (iota (length (call-action-arguments action)))))
                        ")")]
        ;; the M-x prompt's label, as eval draws it, then the text it opens with
        [(prefill-action? action) (string-append "λ " (prefill-text action))]
        [(procedure? action) (or (top-level-name action) "anonymous command")]
        [else (format "~s" action)]))
    (unless (<= (length bindings) 1) (error 'action-text "expected optional producer values"))
    (describe action))

  (edoc "Describe a structured binding and its registered forwarding chain without running argument producers or commands. Rows are (depth text procedure-or-false note-or-false unresolved-spans), with half-open (start . end) character spans for unresolved arguments. Alternatives and cycles stay explicit; only declared local inspection queries may reduce arguments."
        (action any "binding action") (bindings (list-of list) "optional producer-to-value substitutions") (returns list))
  (define (action-trace action . bindings)
    (define substitutions (if (null? bindings) '() (car bindings)))
    (define (node argument)
      (cond [(call-action? argument)
             (let ([result (edoc:inspection-value (call-action-procedure argument) (map node (call-action-arguments argument)))])
               (if (eq? (car result) 'value) result (list 'unknown (action-text argument substitutions))))]
        [(procedure? argument)
         (cond [(assq argument substitutions) => (lambda (p) (list 'value (cdr p)))]
           [else (list 'unknown (string-append "(" (action-text argument) ")"))])]
        [else (list 'value argument)]))
    (define (node-text procedure index n)
      (if (eq? (car n) 'value) (spell procedure index (cadr n)) (format "~a" (cadr n))))
    (define (call-text procedure arguments tail)
      (let ([prefix (string-append "(" (action-text procedure))])
        (let loop ([nodes (append arguments (if tail (list tail) '()))] [i 0]
                   [offset (string-length prefix)] [parts (list prefix)] [spans '()])
          (if (null? nodes) (cons (apply string-append (reverse (cons ")" parts))) (reverse spans))
            (let* ([separator (if (= i (length arguments)) " . " " ")]
                   [text (node-text procedure i (car nodes))]
                   [start (+ offset (string-length separator))] [end (+ start (string-length text))])
              (loop (cdr nodes) (+ i 1) end (cons text (cons separator parts))
                (if (eq? (caar nodes) 'unknown) (cons (cons start end) spans) spans)))))))
    (define left 64)
    (define (follow procedure arguments tail depth seen note)
      (let* ([identity (list procedure arguments tail)] [cycle? (member identity seen)]
             [stop? (or cycle? (>= depth 16) (<= left 1) tail note)]
             [note (or note (and cycle? "cycle") (and (or (>= depth 16) (<= left 1)) "trace limit"))]
             [text (call-text procedure arguments tail)]
             [row (list depth (car text) procedure note (cdr text))])
        (set! left (- left 1))
        (cons row
          (if stop? '()
            (let ([steps (edoc:forwarding-steps procedure arguments)])
              (let walk ([rest steps])
                (if (or (null? rest) (<= left 0)) '()
                  (let* ([step (car rest)]
                         [rows (follow (car step) (cadr step) (caddr step) (+ depth 1) (cons identity seen) (cadddr step))])
                    (append
                      (if (> (length steps) 1)
                        (cons (list (caar rows) (cadar rows) (caddar rows) (or (cadddr (car rows)) "possible")
                                (list-ref (car rows) 4)) (cdr rows)) rows)
                      (walk (cdr rest)))))))))))
    (unless (<= (length bindings) 1) (error 'action-trace "expected optional producer values"))
    (let ([procedure (cond [(call-action? action) (call-action-procedure action)] [(procedure? action) action] [else #f])])
      (if (not procedure) (list (list 0 (action-text action substitutions) #f #f '()))
        (let* ([arguments (if (call-action? action) (map node (call-action-arguments action)) '())]
               [original (action-text action substitutions)] [chain (follow procedure arguments #f 0 '() #f)])
          (if (string=? original (cadar chain)) chain
            (cons (list 0 original procedure #f '())
              (if (call-action? action)
                (map (lambda (row) (cons (+ 1 (car row)) (cdr row))) chain)
                (cdr chain))))))))

  ;; The command type: what a key or a binding names, spelled as the
  ;; call it makes.
  (edoc-type command "a command: a procedure callable with no arguments, by its name"
    (predicate (lambda (v) (and (procedure? v) (logbit? 0 (procedure-arity-mask v)))))
    (write (lambda (v) (action-text v))))

  (define (add-key-binding! context spec action kind)
    (unless (symbol? context)
      (error 'bind-key! "context must be a symbol" context))
    (unless (or (procedure? action) (symbol? action) (call-action? action) (prefill-action? action) (not action))
      (error 'bind-key! "action must be a procedure, a keymap:call, a keymap:prefill, a symbol, or #f" action))
    (kernel:registry-add! key-bindings
                          (binding-item context (key-spec spec) action
                                        kind spec)))

  (edoc "Bind a key spelling as a user binding, which wins over defaults: in a context, or in the global map when none is given."
        (context symbol "the keymap context")
        (spec key "the spelling")
        (action (or procedure symbol (record call-action) (record prefill-action)) "the command; a call built with keymap:call; a pre-filled M-x built with keymap:prefill; or a keymap action"))
  (define bind-key!
    (case-lambda
      [(spec action)
       (add-key-binding! 'global spec action 'user)]
      [(context spec action)
       (add-key-binding! context spec action 'user)]))

  (edoc "Bind a key spelling as a module's default, which user bindings override: in a context, or in the global map when none is given."
        (context symbol "the keymap context")
        (spec key "the spelling")
        (action (or procedure symbol (record call-action) (record prefill-action)) "the command; a call built with keymap:call; a pre-filled M-x built with keymap:prefill; or a keymap action"))
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
