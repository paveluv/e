;; keymap.e -- key syntax and the binding tables: the library
;; (keymap), v2 core dissolution (docs/DESIGN2.md).  Pure
;; infrastructure with no init!; dispatch stays with the head.
;;
;; Every keyboard binding, including the core's defaults, lives in
;; one kernel registry.  An item is (context sequence action kind
;; spelling), where kind is user or default.  The registry supplies
;; its owner: config, an extension module, or #f for the core and
;; live M-x customizations.  User entries always beat defaults;
;; within a layer the newest wins.  Contexts are symbols: 'global,
;; or a buffer mode's name for mode-local maps, or a synthetic scope
;; like 'isearch.

(library (keymap)
  (export key-spec sequence-text
          bind-key! bind-default-key! unbind-key!
          key-binding key-event-binding binding-prefix?
          command-keys command-key command-hint
          sequence-bindings resolved-binding choose-binding
          binding-context binding-sequence binding-action
          binding-kind binding-spec same-sequence?)
  (import (rnrs)
          (only (chezscheme)
                format iota top-level-bound? top-level-value
                hashtable-values)
          (prefix (kernel) kernel:))

  ;; local string utilities (core has its own copies; this library
  ;; sits below core -- see the tech debt ledger)
  (define (string-tail s i) (substring s i (string-length s)))

  (define (string-prefix? prefix s)
    (let ([np (string-length prefix)])
      (and (>= (string-length s) np)
           (string=? (substring s 0 np) prefix))))

  (define (string-join xs sep)
    (if (null? xs)
        ""
        (fold-left (lambda (acc x) (string-append acc sep x))
                   (car xs) (cdr xs))))

  ;;; Key syntax --------------------------------------------------------------

  (define special-key-names
    (append
      '("UP" "DOWN" "LEFT" "RIGHT" "HOME" "END" "BEGIN" "INSERT"
        "DELETE" "PAGEUP" "PAGEDOWN" "TAB" "RET" "ESC" "BACKSPACE"
        "PASTE" "MOUSE" "MOUSE-CLICK")
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
            (and (string-prefix? prefix name)
                 (member (string-tail name (string-length prefix))
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
      [(and (= (string-length s) 3) (string-prefix? "C-" s))
       (format "C-~c" (char-downcase (string-ref s 2)))]
      [(and (= (string-length s) 3) (string-prefix? "M-" s))
       (format "M-~c" (string-ref s 2))]
      [(and (> (string-length s) 3) (string-prefix? "M-" s))
       (let ([base (key-token (string-tail s 2))])
         (string-append "M-" (if (string=? base " ") "SPC" base)))]
      [(and (= (string-length s) 5) (string-prefix? "C-M-" s))
       (format "C-M-~c" (char-downcase (string-ref s 4)))]
      [(= (string-length s) 1) s]
      [(special-key-name? s) s]
      [else (error 'bind-key! "unrecognized key" s)]))

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

  (define (sequence-text sequence) (string-join sequence " "))

  ;;; The binding table ---------------------------------------------------------

  (define key-bindings (kernel:make-registry))

  (define (binding-item context sequence action kind spec)
    (list context sequence action kind spec))
  (define binding-context car)
  (define binding-sequence cadr)
  (define binding-action caddr)
  (define binding-kind cadddr)
  (define (binding-spec b) (car (cddddr b)))

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

  (define (choose-binding entries)
    (or (find (lambda (owned) (eq? (binding-kind (cdr owned)) 'user))
              entries)
        (find (lambda (owned) (eq? (binding-kind (cdr owned)) 'default))
              entries)))

  (define (resolved-binding context sequence)
    (choose-binding (matching-bindings context sequence #t)))

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
    (let ([chosen (make-hashtable equal-hash equal?)])
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
        (kernel:registry-entries key-bindings))
      (vector->list (hashtable-values chosen))))

  (define key-binding
    (case-lambda
      [(spec) (key-binding 'global spec)]
      [(context spec)
       (let ([hit (resolved-binding context (key-spec spec))])
         (and hit (binding-action (cdr hit))))]))

  (define (key-event-binding context . events)
    ;; Runtime events are already canonical tokens.  Do not feed them
    ;; back through the human key-spec parser: its spaces are separators,
    ;; while a typed space is itself the literal " " event.
    (let ([hit (resolved-binding context events)])
      (and hit (binding-action (cdr hit)))))

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
    (unless (or (procedure? action) (symbol? action) (not action))
      (error 'bind-key! "action must be a procedure, symbol, or #f" action))
    (kernel:registry-add! key-bindings
                          (binding-item context (key-spec spec) action
                                        kind spec)))

  (define bind-key!
    (case-lambda
      [(spec action) (add-key-binding! 'global spec action 'user)]
      [(context spec action) (add-key-binding! context spec action 'user)]))

  (define bind-default-key!
    (case-lambda
      [(spec action) (add-key-binding! 'global spec action 'default)]
      [(context spec action)
       (add-key-binding! context spec action 'default)]))

  (define unbind-key!
    (case-lambda
      [(spec) (add-key-binding! 'global spec #f 'user)]
      [(context spec) (add-key-binding! context spec #f 'user)]))

  ;;; Reverse lookup -------------------------------------------------------------

  (define (command-keys sym)
    ;; Every global key spec currently resolved to the top-level command
    ;; named sym. Bindings are read live, so overrides and module reloads are
    ;; reflected immediately.
    (guard (ex [else '()])
      (if (top-level-bound? sym)
          (let ([proc (top-level-value sym)])
            (map (lambda (owned) (binding-spec (cdr owned)))
                 (filter
                   (lambda (owned)
                     (let ([b (cdr owned)])
                       (and (eq? (binding-context b) 'global)
                            (eq? (binding-action b) proc)
                            (eq? owned
                                 (resolved-binding 'global
                                                   (binding-sequence b))))))
                   (kernel:registry-entries key-bindings))))
          '())))

  (define (command-key sym)
    ;; The most recently registered key currently bound to sym, or #f.
    (let ([keys (command-keys sym)])
      (and (pair? keys) (car keys))))

  (define (command-hint syms)
    ;; "M-n next-conflict!, M-m keep-mine!" for a list of command
    ;; names: each with its current key, or bare when unbound.
    (string-join
      (map (lambda (s)
             (let ([k (command-key s)])
               (if k (format "~a ~a" k s) (format "~a" s))))
           syms)
      ", ")))
