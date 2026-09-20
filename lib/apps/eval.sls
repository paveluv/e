;; eval.sls -- M-x: evaluate Scheme expressions, for the e editor.
;;
;; An e extension module: the library (eval), loaded at startup by the
;; kernel, which calls init!.  M-x prompts for an expression (the opening
;; parenthesis is pretyped and deletable, so a bare symbol works too;
;; missing closing parentheses are forgiven), evaluates it in the
;; editor's top level, logs the expression (component eval, which also
;; carries the history), and shows the result in the echo area,
;; transiently like any message -- the log keeps what flashed by.  The
;; expression styles as Scheme while typed.  TAB completes symbols
;; (Shift-TAB: only editor-defined ones,
;; which are also highlighted in the completions pop-up), the parameters
;; still to be supplied appear as a grey suggestion while typing, up and
;; down arrows browse the history, and C-g interrupts a runaway
;; evaluation. Parameter suggestions query the base's reference corpus
;; and live module entries, with source and arity as fallbacks.
;; C-x C-e runs eval:run! over the whole current buffer, or an explicit
;; region/buffer target, in that same top level.

(import (only (edoc) elibrary))
(elibrary (eval)
  (export init! settle-completion completion-candidates completion-extensions
          (rename (eval! run!)) (rename (eval!! run!!)) (rename (eval-copy-result copy-result)))
  (import (chezscheme)
          (except (edit) init!)
          (prefix (prompt) prompt:)
          (prefix (head) head:)
          (prefix (mode) mode:)
          (prefix (kernel) kernel:)
          (prefix (string) string:)
          (prefix (fuzzy) fuzzy:)
          (prefix (edoc) edoc:)
          (prefix (style) style:)
          (prefix (paint) paint:)
          (prefix (log) log:)
          (prefix (keymap) keymap:)
          (prefix (only (reference) lookup) reference:)
          (prefix (doc) doc:)
          (only (edit) regions-of region-text)
          (prefix (only (scheme-format) indent-lines delimiter?) scheme-format:)
          (prefix (only (sys) call-with-streamed-output duplicate-standard-output-port terminal-output-port) sys:))

  ;;; Symbol completion -------------------------------------------------------

  (define (symbol-range s pos)
    ;; The query itself need not read as Scheme (2foo can find foo-2). Replace
    ;; its raw token with an identifier, then let Scheme's lexer decide whether
    ;; that slot is code rather than a string/comment. No expression is evaluated.
    (define (delimiter? c)
      (or (scheme-format:delimiter? c) (memv c '(#\` #\,))))
    (guard (ex [else #f])
      (let* ([start (let back ([i pos])
                      (if (and (> i 0) (not (delimiter? (string-ref s (- i 1)))))
                          (back (- i 1)) i))]
             [end (let forward ([i pos])
                    (if (and (< i (string-length s)) (not (delimiter? (string-ref s i))))
                        (forward (+ i 1)) i))]
             [in (open-input-string
                   (string-append (substring s 0 start) "x" (string:tail s end)))])
        (and (not (string:prefix? "#\\" (substring s start end)))
          (let scan ()
            (let-values ([(kind value from to) (read-token in)])
              (cond [(or (eq? kind 'eof) (> from start)) #f]
                [(> to start)
                 (and (eq? kind 'atomic) (symbol? value) (= from start) (cons start end))]
                [else (scan)])))))))

  ;;; Completing an argument by its type -----------------------------------------

  ;; At an argument position of a documented operator, Tab offers what the
  ;; argument's edoc type accepts: the type's own values, spelled as
  ;; expressions; the documented procedures and parameters that produce one;
  ;; and the top-level variables holding one. Inside a string literal, the
  ;; type's string values complete the literal. The token matches a
  ;; candidate's spelling the way it matches a symbol, and Tab extends it
  ;; the same way: to the longest text every current match still matches.

  (define (open-string-start s pos)
    ;; the index of the quote opening a string still open at pos, or #f
    (let loop ([i 0] [open #f])
      (cond [(>= i pos) open]
            [(and open (char=? (string-ref s i) #\\)) (loop (+ i 2) open)]
            [(char=? (string-ref s i) #\") (loop (+ i 1) (if open #f i))]
            [else (loop (+ i 1) open)])))

  (define (formal-at formals index)
    ;; (name . rest?) of the formal taking a zero-based argument index, or #f
    (let loop ([f formals] [i index])
      (cond [(pair? f) (if (= i 0) (cons (car f) #f) (loop (cdr f) (- i 1)))]
            [(symbol? f) (cons f #t)]
            [else #f])))

  (define (callable-formals sig)
    ;; the lambda list of a documented callable: a procedure's own, a record
    ;; procedure's from its argument names, in order; #f for the rest
    (case (edoc:signature-kind sig)
      [(procedure) (edoc:signature-formals sig)]
      [(constructor accessor mutator predicate) (map edoc:argument-name (edoc:signature-arguments sig))]
      [else #f]))

  (define (argument-type sym index)
    ;; the type documented for argument index of the callable bound to sym,
    ;; the union of its lambda lists' answers, or #f
    (let* ([value (and (top-level-bound? sym) (top-level-value sym))]
           [signatures (and (procedure? value) (edoc:edoc-of value))])
      (and signatures
           (let ([types
                  (fold-left
                    (lambda (types sig)
                      (let* ([formals (callable-formals sig)]
                             [formal (and formals (formal-at formals index))]
                             [argument (and formal (find (lambda (a) (eq? (edoc:argument-name a) (car formal)))
                                                         (edoc:signature-arguments sig)))])
                        (if (not argument) types
                            (let* ([type (edoc:argument-type argument)]
                                   [type (if (and (cdr formal) (pair? type) (eq? (car type) 'list-of)) (cadr type) type)])
                              (if (member type types) types (cons type types))))))
                    '() signatures)])
             (cond [(null? types) #f]
                   [(null? (cdr types)) (car types)]
                   [else (cons 'or (reverse types))])))))

  (define (argument-context s pos)
    ;; (type start end token string?) for the cursor at a documented
    ;; argument position: the argument's type, the range and text of the
    ;; token being completed, and whether it sits inside a string literal.
    ;; At the operator position of a nested form, (show-buffer! (bu, the
    ;; token is the form's opening and the type is the enclosing argument's:
    ;; whatever the form produces has to serve it. #f under a quote, or
    ;; without a type.
    (define (typed frame start end in-string?)
      (let ([type (argument-type (string->symbol (frame-operator frame)) (frame-arguments frame))])
        (and type (list type start end (substring s start end) in-string?))))
    (define (plain? frame) (and (not (frame-quoted? frame)) (string? (frame-operator frame))))
    (let* ([quote-at (open-string-start s pos)]
           [range (and (not quote-at) (symbol-range s pos))]
           [start (cond [quote-at (+ quote-at 1)] [range (car range)] [else pos])]
           [end (cond [quote-at pos] [range (cdr range)] [else pos])]
           [frames (call-frames (substring s 0 (if quote-at quote-at start)))])
      (and (pair? frames)
           (let ([frame (car frames)])
             (cond
               [(plain? frame) (typed frame start end (and quote-at #t))]
               [(and (eq? (frame-operator frame) 'pending) (not (frame-quoted? frame)) (not quote-at)
                     (pair? (cdr frames)) (plain? (cadr frames))
                     (> start 0) (char=? (string-ref s (- start 1)) (frame-opener frame)))
                (typed (cadr frames) (- start 1) end #f)]
               [else #f])))))

  (define (type-fits? wanted produced)
    ;; whether a produced type serves a wanted one: the same, one refining
    ;; it, a named type and the record type it denotes either way round, a
    ;; member of a wanted union, or a union with a serving member
    (define (refines? produced fuel)
      (let ([record (and (symbol? produced) (> fuel 0) (edoc:type-named produced))])
        (and record (edoc:type-within record)
             (or (type-fits? wanted (edoc:type-within record)) (refines? (edoc:type-within record) (- fuel 1))))))
    (define (record-of? t) (and (pair? t) (eq? (car t) 'record)))
    (or (equal? wanted produced)
        (refines? produced 8)
        (and (record-of? wanted) (symbol? produced) (edoc:type-denotes-record? produced (cadr wanted)))
        (and (record-of? produced) (symbol? wanted) (edoc:type-denotes-record? wanted (cadr produced)))
        (and (pair? wanted) (eq? (car wanted) 'or) (exists (lambda (m) (type-fits? m produced)) (cdr wanted)))
        (and (pair? produced) (eq? (car produced) 'or) (exists (lambda (m) (type-fits? wanted m)) (cdr produced)))))

  (define (module-type? type)
    ;; a type with values worth scanning the top level for: one a library
    ;; defined, or a record; the language's types would list everything
    (cond [(symbol? type)
           (let ([record (edoc:type-named type)])
             (and record (not (equal? (edoc:type-owner record) "(edoc)")) (edoc:type-owner record) #t))]
          [(and (pair? type) (eq? (car type) 'record)) #t]
          [(and (pair? type) (eq? (car type) 'or)) (exists module-type? (cdr type))]
          [else #f]))

  ;; A typed candidate: the text the token matches against, what Tab inserts
  ;; for it alone, the label the row shows, the grey hint beside it, and the
  ;; label's face. The text is the candidate's opening: its spelling without
  ;; the closers the settle step supplies, so an extension never closes the
  ;; form the token is still inside.
  (define-record-type option (fields text insert label hint face))

  (define (opening spelling)
    (let loop ([end (string-length spelling)])
      (if (and (> end 1) (memv (string-ref spelling (- end 1)) '(#\) #\] #\")))
          (loop (- end 1))
          (substring spelling 0 end))))

  (define (value-options type token in-string?)
    (fold-right
      (lambda (pair out)
        (let ([value (car pair)] [hint (or (cdr pair) "")])
          (cond
            [in-string?
             (if (string? value)
                 ;; the literal stays open for a directory, to descend into
                 (cons (make-option value (if (string:suffix? "/" value) value (string-append value "\"")) value hint 'plain) out)
                 out)]
            [else (let ([text (edoc:type-spelling type value)])
                    (cons (make-option (opening text) text text hint 'plain) out))])))
      '() (edoc:type-completions type token)))

  (define (documented-symbols)
    ;; the editor's top-level names bound to documented values, with their signatures
    (fold-right
      (lambda (sym out)
        (let ([signatures (and (kernel:editor-symbol? sym) (edoc:edoc-of (top-level-value sym)))])
          (if signatures (cons (cons sym signatures) out) out)))
      '() (environment-symbols (interaction-environment))))

  (define (option<? a b)
    ;; the command layer's bare names before a module's prefixed ones, fewer
    ;; arguments first, then alphabetically
    (define (key o)
      (let ([label (option-label o)])
        (list (if (string:search label ":" 0 (string-length label)) 1 0)
              (let count ([i 0] [n 0])
                (if (= i (string-length label)) n (count (+ i 1) (if (char=? (string-ref label i) #\space) (+ n 1) n))))
              label)))
    (let ([ka (key a)] [kb (key b)])
      (or (< (car ka) (car kb))
          (and (= (car ka) (car kb))
               (or (< (cadr ka) (cadr kb))
                   (and (= (cadr ka) (cadr kb)) (string<? (caddr ka) (caddr kb))))))))

  (define (producer-options type)
    ;; the documented procedures and parameters whose result serves the type;
    ;; for the language's types, integer or boolean say, that would be half
    ;; the editor, so only a library's own types and records get them
    (if (not (module-type? type)) '()
      (list-sort option<?
        (fold-right
          (lambda (entry out)
            (let* ([sym (car entry)] [name (symbol->string sym)]
                   [producing
                    (filter (lambda (sig)
                              (case (edoc:signature-kind sig)
                                [(procedure constructor accessor)
                                 (let ([returns (edoc:signature-returns sig)])
                                   (and returns (type-fits? type (edoc:argument-type returns))))]
                                [(parameter) (let ([value (find (lambda (a) (eq? (edoc:argument-name a) 'value)) (edoc:signature-arguments sig))])
                                               (and value (type-fits? type (edoc:argument-type value))))]
                                [else #f]))
                      (cdr entry))])
              (if (null? producing) out
                (let* ([sig (car producing)]
                       [formals (or (callable-formals sig) '())]
                       [label (edoc:edoc-template sym formals)]
                       [text (string-append "(" name)]
                       [insert (if (null? formals) label text)])
                  (cons (make-option text insert label (edoc:signature-summary sig) 'editor) out)))))
          '() (documented-symbols)))))

  (define (variable-options type)
    ;; the top-level names whose current values the type accepts, alphabetically
    (if (not (module-type? type)) '()
        (list-sort option<?
          (fold-right
            (lambda (sym out)
              (let ([value (and (kernel:editor-symbol? sym) (top-level-value sym))])
                (if (and value (guard (ex [else #f]) (edoc:type-accepts? type value)))
                  (let ([name (symbol->string sym)])
                    (cons (make-option name name name
                            (if (procedure? value) (completion-hint sym) (edoc:type-spelling type value)) 'editor)
                          out))
                  out)))
            '() (environment-symbols (interaction-environment))))))

  (define (quality<? a b)
    ;; The matcher's rank components that judge an alignment: segments,
    ;; reorderings, first character and span, without the name length that
    ;; breaks its own ties. Among equals the sources keep their order, so
    ;; the values lead the producers when a token fits every candidate.
    (let loop ([a a] [b b] [n 4])
      (cond [(= n 0) #f]
            [(< (car a) (car b)) #t]
            [(> (car a) (car b)) #f]
            [else (loop (cdr a) (cdr b) (- n 1))])))

  (define (typed-options context)
    ;; ((option . fragments) ...) for an argument context, best first, or #f
    ;; when the type offers nothing the token matches: the token aligns with
    ;; a candidate's text as it would with a symbol, so (bu matches both
    ;; (buffer "a.txt") and (head:current-buffer), and name matches no formal
    (let* ([type (car context)] [token (cadddr context)] [in-string? (car (cddddr context))]
           [all (append (value-options type token in-string?)
                        (if in-string? '() (producer-options type))
                        (if in-string? '() (variable-options type)))])
      (cond
        [(null? all) #f]
        [(string=? token "") (map (lambda (o) (cons o '())) all)]
        [else
         (let ([by-text (make-hashtable string-hash string=?)])
           (for-each (lambda (o) (hashtable-set! by-text (option-text o) #t)) all)
           (for-each (lambda (m) (hashtable-set! by-text (fuzzy:name m) m))
                     (fuzzy:rank token (vector->list (hashtable-keys by-text))))
           (let ([matched (fold-right
                            (lambda (o out)
                              (let ([m (hashtable-ref by-text (option-text o) #t)])
                                (if (eq? m #t) out (cons (cons o m) out))))
                            '() all)])
             (and (pair? matched)
                  (map (lambda (entry) (cons (car entry) (fuzzy:fragments (cdr entry))))
                       (list-sort (lambda (a b) (quality<? (fuzzy:score (cdr a)) (fuzzy:score (cdr b)))) matched)))))])))

  (define (typed-inserts s context options)
    ;; what Tab puts in place of the token: a sole candidate whole, else the
    ;; safe extensions of the token over the candidates' texts, the longest
    ;; that every current match still matches, as for symbols, and that
    ;; leaves the cursor at a typed argument: an extension opening a string
    ;; after an operator nobody documents, (hea" say, would strand it
    (let ([start (cadr context)] [end (caddr context)] [token (cadddr context)])
      (define (typed-still? text)
        (and (argument-context (string-append (substring s 0 start) text (substring s end (string-length s)))
                               (+ start (string-length text)))
             #t))
      (if (null? (cdr options))
          (list (option-insert (car (car options))))
          (let ([seen (make-hashtable string-hash string=?)])
            (fuzzy:expansions token
              (fold-right (lambda (entry out)
                            (let ([text (option-text (car entry))])
                              (if (hashtable-ref seen text #f) out
                                  (begin (hashtable-set! seen text #t) (cons text out)))))
                          '() options)
              typed-still?)))))

  (define (typed-candidate entry)
    ;; a prompt candidate from (option . fragments): the label with its
    ;; matched characters underlined, the hint in grey, the insertion apart
    (let* ([option (car entry)] [fragments (cdr entry)]
           [label (option-label option)] [hint (option-hint option)]
           [text (if (string=? hint "") label (string-append label "  " hint))]
           [styles (make-vector (string-length text) 'chrome)]
           [face (option-face option)])
      (style:fill-range! styles 0 (string-length label) face)
      (for-each
        (lambda (fragment)
          (style:fill-range! styles (cadr fragment) (+ (cadr fragment) (caddr fragment)) (list face 'mark)))
        fragments)
      (prompt:make-candidate (option-insert option) text styles)))

  (edoc "The typed completions M-x offers at the cursor: for an argument position whose operator documents the argument's type, the labels of the type's values, of the procedures producing one and of the variables holding one; #f where symbols complete instead."
        (text string "the prompt input")
        (pos integer "the cursor position")
        (returns (or (list-of string) #f)))
  (define (completion-candidates text pos)
    (let* ([context (argument-context text pos)] [options (and context (typed-options context))])
      (and options (map (lambda (entry) (option-label (car entry))) options))))

  (edoc "The texts Tab puts in place of the token at a typed argument position: a sole candidate whole, else the longest extensions of the token that every current candidate still matches, the token itself when nothing longer does; #f where symbols complete instead."
        (text string "the prompt input")
        (pos integer "the cursor position")
        (returns (or (list-of string) #f)))
  (define (completion-extensions text pos)
    (let* ([context (argument-context text pos)] [options (and context (typed-options context))])
      (and options (typed-inserts text context options))))

  (define hint-cache (make-weak-eq-hashtable))

  (define (completion-hint sym)
    ;; The grey text beside a candidate: what it takes, then its edoc summary;
    ;; "" when nothing local is known. A procedure shows its arguments, a
    ;; parameter [value], a value its type, a keyword its parts. The list
    ;; wraps it as needed. Cached per procedure -- stripping a source datum
    ;; is costly and the answer never changes for the same procedure -- and
    ;; per name for the rest.
    (let* ([bound? (top-level-bound? sym)]
           [value (and bound? (top-level-value sym))]
           [key (if (procedure? value) value sym)])
      (or (eq-hashtable-ref hint-cache key #f)
          (let* ([signatures (or (and bound? (edoc:edoc-of value)) (edoc:edoc-named sym))]
                 [sig (and signatures (car signatures))]
                 [arguments
                  (cond
                    [(not sig)
                     (let ([tokens (and (procedure? value) (guard (ex [else #f]) (local-params value)))])
                       (if tokens (string-append "(" (string:join tokens " ") ")") ""))]
                    [else
                     (case (edoc:signature-kind sig)
                       [(procedure)
                        (string:join (map (lambda (s) (format "~s" (edoc:signature-formals s))) signatures) " ")]
                       [(parameter) "[value]"]
                       [(value)
                        (let ([type (find (lambda (a) (eq? (edoc:argument-name a) 'value)) (edoc:signature-arguments sig))])
                          (if type (string-append "<" (edoc:type-text (edoc:argument-type type)) ">") ""))]
                       [(syntax) (format "~s" (map edoc:argument-name (edoc:signature-arguments sig)))]
                       [else (format "~s" (map edoc:argument-name (edoc:signature-arguments sig)))])])]
                 [summary (if sig (edoc:signature-summary sig) "")]
                 [hint (cond [(string=? summary "") arguments]
                             [(string=? arguments "") summary]
                             [else (string-append arguments "  " summary)])])
            (eq-hashtable-set! hint-cache key hint)
            hint))))

  (define (completion-candidate match)
    ;; The label: the name with its matched characters underlined, then in
    ;; grey what is known about it, kept apart from the inserted value.
    (let* ([name (fuzzy:name match)] [fragments (fuzzy:fragments match)]
           [hint (completion-hint (string->symbol name))]
           [label (if (string=? hint "") name (string-append name "  " hint))]
           [styles (make-vector (string-length label) 'chrome)]
           [face (if (kernel:editor-symbol? (string->symbol name)) 'editor 'plain)]
           [matched (list face 'mark)])
      (style:fill-range! styles 0 (string-length name) face)
      (for-each
        (lambda (fragment)
          (style:fill-range! styles (cadr fragment) (+ (cadr fragment) (caddr fragment)) matched))
        fragments)
      (prompt:make-candidate name label styles)))

  (define (symbol-completer keep? typed?)
    (prompt:make-completer
      (lambda (s pos)
        (define (symbols)
          (let ([range (symbol-range s pos)])
            (if (not range) (values #f #f '() '())
                (let* ([part (substring s (car range) (cdr range))]
                       ;; Symbols go in as they are: the matcher keeps each one
                       ;; prepared across keystrokes.
                       [ranked (fuzzy:rank part (filter keep? (environment-symbols (interaction-environment))))]
                       [names (map fuzzy:name ranked)])
                  (values (car range) (cdr range) (lambda () (fuzzy:expansions part names))
                    (map completion-candidate ranked))))))
        ;; an argument with a documented type offers its own candidates; a
        ;; sole one is what Tab inserts, else Tab extends the token as far as
        ;; every candidate allows and lists them
        (let* ([context (and typed? (argument-context s pos))]
               [options (and context (typed-options context))])
          (if (not options) (symbols)
              (values (cadr context) (caddr context)
                (lambda () (typed-inserts s context options))
                (map typed-candidate options)))))
      ;; a closure: the completers are built while the module loads, before
      ;; the settling procedures below are defined
      (lambda (text pos) (settle-completion text pos))))

  (define complete-symbol (symbol-completer (lambda (sym) #t) #t))
  (define complete-editor-symbol (symbol-completer kernel:editor-symbol? #f))

  ;;; Signatures ----------------------------------------------------------------

  (define (arity-params mask)
    ;; Generic parameter names from procedure-arity-mask: arg1 ... for the
    ;; smallest accepted count, [argN] for the optional ones beyond it, and
    ;; ... when any further count is accepted (a negative mask).
    (if (= mask 0)
        '()
        (let* ([rest? (< mask 0)]
               [lo (let loop ([n 0]) (if (logbit? n mask) n (loop (+ n 1))))]
               [hi (if rest?
                       lo
                       (let loop ([n 0] [hi 0])
                         (cond [(> (expt 2 n) mask) hi]
                               [(logbit? n mask) (loop (+ n 1) n)]
                               [else (loop (+ n 1) hi)])))])
          (append
            (let loop ([i 1])
              (if (> i lo) '() (cons (format "arg~a" i) (loop (+ i 1)))))
            (let loop ([i (+ lo 1)])
              (if (> i hi) '() (cons (format "[arg~a]" i) (loop (+ i 1)))))
            (if rest? '("...") '())))))

  (define (signature-arity sig)
    (let loop ([p (cdr sig)] [n 0])
      (if (pair? p) (loop (cdr p) (+ n 1)) n)))

  (define (signature-tokens sig)
    ;; A documented call shape becomes display tokens. A parenthesized
    ;; parameter is optional and a dotted tail is a rest parameter.
    (let loop ([p (cdr sig)])
      (cond [(null? p) '()]
            [(symbol? p) (list (format ". ~a" p))]
            [(pair? (car p))
             (cons (format "[~a]"
                           (string:join (map (lambda (x) (format "~a" x))
                                             (car p))
                                        " "))
                   (loop (cdr p)))]
            [else (cons (format "~a" (car p)) (loop (cdr p)))])))

  (define (described-params sym)
    ;; Pick the longest documented procedure form for this name.
    (guard (ex [else #f])
      (let ([best #f])
        (for-each
          (lambda (entry)
            (for-each
              (lambda (form)
                (when (equal? (car form) "procedure")
                  (let ([sig (guard (ex [else #f])
                               (with-input-from-string (cdr form) read))])
                    (when (and (pair? sig) (eq? (car sig) sym)
                               (or (not best)
                                   (> (signature-arity sig)
                                      (signature-arity best))))
                      (set! best sig)))))
              (doc:forms entry)))
          (reference:lookup sym))
        (and best (signature-tokens best)))))

  (define (local-params v)
    ;; The parameters of procedure v as display tokens, from its source, else
    ;; its arity in brackets.
    (let ([src (((inspect/object v) 'code) 'source)])
      (cond
        [(and src (pair? (src 'value)) (eq? (car (src 'value)) 'lambda))
         (let loop ([p (cadr (src 'value))])
           (cond [(null? p) '()]
                 [(symbol? p) (list (format ". ~a" p))]
                 [else (cons (format "~a" (car p)) (loop (cdr p)))]))]
        [else (arity-params (procedure-arity-mask v))])))

  (define (symbol-params sym)
    ;; The parameters of the procedure sym names, as a list of display
    ;; tokens: from its live describe entry when available, else its source,
    ;; else its arity in brackets.  #f for anything else.
    (and (top-level-bound? sym)
         (let ([v (top-level-value sym)])
           (and (procedure? v)
                (or (described-params sym) (local-params v))))))

  (define (drop-params tokens n)
    ;; The parameter tokens left after n arguments: one is consumed per
    ;; argument, but a rest marker (... or a dotted tail) absorbs any count.
    (cond [(or (null? tokens) (= n 0)) tokens]
          [(string=? (car tokens) "...") tokens]
          [(string:prefix? ". " (car tokens)) tokens]
          [(and (pair? (cdr tokens)) (string=? (cadr tokens) "...")) tokens]
          [else (drop-params (cdr tokens) (- n 1))]))

  (define (open-call-frames text)
    ;; The unclosed calls in text, innermost first, each as
    ;; (operator . arguments-so-far) -- operator is its token string, #f
    ;; when it is not a plain symbol, or 'pending when not yet typed.  A
    ;; trailing partial atom or string counts as an argument in progress.
    (define n (string-length text))
    (define (atom-end i)
      (if (or (>= i n)
              (memv (string-ref text i)
                    '(#\space #\tab #\newline #\( #\) #\[ #\] #\")))
          i
          (atom-end (+ i 1))))
    (define (string-end j)
      (cond [(>= j n) n]
            [(char=? (string-ref text j) #\\) (string-end (+ j 2))]
            [(char=? (string-ref text j) #\") (+ j 1)]
            [else (string-end (+ j 1))]))
    (define (datum stack tok)
      ;; A completed datum: the pending operator slot, or one more argument.
      (if (null? stack)
          stack
          (let ([frame (car stack)])
            (cons (if (eq? (car frame) 'pending)
                      (cons (or tok #f) 0)
                      (cons (car frame) (+ (cdr frame) 1)))
                  (cdr stack)))))
    (let loop ([i 0] [stack '()])
      (if (>= i n)
          stack
          (let ([c (string-ref text i)])
            (cond
              [(memv c '(#\space #\tab #\newline #\' #\` #\,))
               (loop (+ i 1) stack)]
              [(memv c '(#\( #\[)) (loop (+ i 1) (cons (cons 'pending 0) stack))]
              [(memv c '(#\) #\]))
               (loop (+ i 1) (if (pair? stack) (datum (cdr stack) #f) stack))]
              [(char=? c #\") (loop (string-end (+ i 1)) (datum stack #f))]
              [else (let ([j (atom-end (+ i 1))])
                      (loop j (datum stack (substring text i j))))])))))

  (define (signature-ghost s)
    ;; The grey suggestion for the M-x input: the parameters of the
    ;; innermost open call's operator that have not been supplied yet.
    (guard (ex [else #f])
      (let ([stack (open-call-frames s)])
        (and (pair? stack)
             (string? (caar stack))
             (let ([tokens (symbol-params (string->symbol (caar stack)))])
               (and tokens
                    (let ([left (drop-params tokens (cdar stack))])
                      (and (pair? left)
                           (string-append
                             (if (string:suffix? " " s) "" " ")
                             (string:join left " "))))))))))

  ;;; Settling a sole completion --------------------------------------------------

  (define (fixed-arity sym)
    ;; The number of arguments sym's procedure takes when that is one number:
    ;; #f for optional or rest parameters, for syntax, and for anything unbound.
    (let ([tokens (guard (ex [else #f]) (symbol-params sym))])
      (and tokens
           (for-all (lambda (token)
                      (not (or (string=? token "...")
                               (string:prefix? ". " token)
                               (string:prefix? "[" token))))
                    tokens)
           (length tokens))))

  ;; An unclosed form: its opening bracket, its operator (the token string,
  ;; #f for a datum that is not a symbol, pending before any), the completed
  ;; data after the operator, and whether a quote or quasiquote covers it.
  (define-record-type frame (fields opener operator arguments quoted?))
  (define closers '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))

  (define (call-frames text)
    ;; The unclosed forms in text, innermost first. A trailing partial atom
    ;; counts as a datum; a quoted atom is a datum without a symbol.
    (define n (string-length text))
    (define (atom-end i)
      (if (or (>= i n)
              (memv (string-ref text i)
                    '(#\space #\tab #\newline #\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\,)))
          i
          (atom-end (+ i 1))))
    (define (string-end j)
      (cond [(>= j n) n]
            [(char=? (string-ref text j) #\\) (string-end (+ j 2))]
            [(char=? (string-ref text j) #\") (+ j 1)]
            [else (string-end (+ j 1))]))
    (let loop ([i 0] [stack '()] [quote-next? #f])
      (if (>= i n)
          stack
          (let ([c (string-ref text i)])
            (cond
              [(memv c '(#\space #\tab #\newline)) (loop (+ i 1) stack quote-next?)]
              [(memv c '(#\' #\`)) (loop (+ i 1) stack #t)]
              [(char=? c #\,)
               (loop (+ i (if (and (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\@)) 2 1)) stack quote-next?)]
              [(assv c closers)
               (loop (+ i 1)
                     (cons (make-frame c 'pending 0
                             (or quote-next? (and (pair? stack) (frame-quoted? (car stack)))))
                           stack)
                     #f)]
              [(memv c '(#\) #\] #\}))
               (loop (+ i 1) (if (pair? stack) (count-datum (cdr stack) #f) stack) #f)]
              [(char=? c #\") (loop (string-end (+ i 1)) (count-datum stack #f) #f)]
              [else
               (let ([j (atom-end (+ i 1))])
                 (loop j (count-datum stack (and (not quote-next?) (substring text i j))) #f))])))))

  (define (count-datum frames token)
    ;; A completed datum fills the innermost form's operator slot, else is one
    ;; more argument of it.
    (if (null? frames)
        frames
        (let ([f (car frames)])
          (cons (if (eq? (frame-operator f) 'pending)
                    (make-frame (frame-opener f) token 0 (frame-quoted? f))
                    (make-frame (frame-opener f) (frame-operator f) (+ (frame-arguments f) 1) (frame-quoted? f)))
                (cdr frames)))))

  (edoc "The input to continue with after a sole completion ends at pos: a form whose operator has a known arity closes when complete and settles again in its parent, or steps to its next argument; an unknown arity, a quoted form or text after pos leaves the cursor at the symbol."
        (text string "the prompt input")
        (pos integer "where the completed symbol ends")
        (returns pair "the new input and cursor position, (text . pos)"))
  (define (settle-completion text pos)
    ;; The input to continue with after a sole completion ends at pos: while
    ;; the enclosing operator has a fixed arity, a complete form closes with
    ;; its matching bracket and settles again as an argument of its parent,
    ;; and an incomplete one steps to its next argument. An unknown arity, a
    ;; quoted form, or text after pos leaves the cursor at the symbol.
    (define (blank? from)
      (let loop ([i from])
        (or (>= i (string-length text))
            (and (memv (string-ref text i) '(#\space #\tab #\newline)) (loop (+ i 1))))))
    (define (settle frames out at)
      (if (or (null? frames) (frame-quoted? (car frames)))
          (cons out at)
          (let* ([frame (car frames)]
                 [operator (frame-operator frame)]
                 [arity (and (string? operator) (fixed-arity (string->symbol operator)))])
            (cond
              [(not arity) (cons out at)]
              [(< (frame-arguments frame) arity) (cons (string-append out " ") (+ at 1))]
              [(= (frame-arguments frame) arity)
               (settle (count-datum (cdr frames) #f)
                 (string-append out (string (cdr (assv (frame-opener frame) closers))))
                 (+ at 1))]
              [else (cons out at)]))))
    (if (not (blank? pos))
        (cons text pos)
        (let* ([head (substring text 0 pos)] [tail (substring text pos (string-length text))]
               [settled (settle (call-frames head) head pos)])
          (cons (string-append (car settled) tail) (cdr settled)))))

  ;;; Evaluation ----------------------------------------------------------------

  (edoc "Whether a non-void evaluation result is also placed in the kill ring."
        (value boolean))
  (define eval-copy-result (make-parameter #t))

  (define (close-expression text)
    ;; text completed with the parentheses it is missing (up to a few), so
    ;; it reads as one datum; #f when that is not enough to make it read.
    (let loop ([extra 0])
      (and (<= extra 8)
           (let ([t (string-append text (make-string extra #\)))])
             (if (guard (ex [else #f])
                   (with-input-from-string t read)
                   #t)
                 t
                 (loop (+ extra 1)))))))

  (define (format-exchange d)
    ;; The eval entry: (query . result) formatted "query => result";
    ;; a bare string (an old-style record) as itself.
    (if (pair? d)
        (string-append (car d) " => " (cdr d))
        (format "~a" d)))

  (define (editorize! text styles)
    ;; Overlay for eval contexts, which run in the editor's top level:
    ;; symbols the editor itself defines take the editor style, on top
    ;; of cells the base styling left plain or italic.
    (let ([n (string-length text)])
      (define (boundary? c)
        (memv c '(#\space #\tab #\newline #\( #\) #\[ #\] #\" #\' #\` #\,
                  #\;)))
      (let loop ([i 0])
        (when (< i n)
          (if (boundary? (string-ref text i))
              (loop (+ i 1))
              (let end ([j (+ i 1)])
                (if (and (< j n) (not (boundary? (string-ref text j))))
                    (end (+ j 1))
                    (begin
                      (when (and (guard (ex [else #f])
                                   (kernel:editor-symbol?
                                     (string->symbol (substring text i j))))
                                 (memq (vector-ref styles i) '(plain italic)))
                        (let fill ([k i])
                          (when (< k j)
                            (vector-set! styles k 'editor)
                            (fill (+ k 1)))))
                      (loop j)))))))
      styles))

  (define (style-exchange text)
    ;; Scheme highlighting over the whole exchange, echo and *log*
    ;; alike -- the editor's own names in the editor style: eval runs
    ;; in the editor's environment, whatever a random file does.
    (let ([scheme (mode:find "scheme")])
      (and scheme (editorize! text ((mode:styles scheme) text)))))

  (define mx-echo-styles
    ;; Scheme highlighting for the M-x prompt: the label stays grey,
    ;; the expression styles as Scheme with the editor's own names in
    ;; the editor style.
    (paint:prompt-styler "M-x "
      (lambda (input)
        (guard (ex [else #f])
          (let ([scheme (mode:find "scheme")])
            (and scheme
                 (editorize! input ((mode:styles scheme) input))))))))

  (define (trim-right s)
    ;; s without trailing blanks, so auto-closed parentheses attach
    ;; directly to the input (completion appends a space, for one).
    (let loop ([n (string-length s)])
      (if (and (> n 0) (memv (string-ref s (- n 1)) '(#\space #\tab)))
          (loop (- n 1))
          (substring s 0 n))))

  (define (normalize-input s)
    ;; The prompt's normalizer: trailing blanks go and the forgiven
    ;; closing parentheses are appended, so the history and the kept
    ;; echo carry the completed expression.  A lone "(" -- the
    ;; untouched pretyped prompt -- passes through for the
    ;; cancellation check.
    (let ([t (trim-right s)])
      (if (or (string=? t "") (string=? t "("))
          t
          (or (close-expression t) t))))

  (define (leading-blanks text)
    (let loop ([i 0])
      (if (and (< i (string-length text))
               (memv (string-ref text i) '(#\space #\tab)))
          (loop (+ i 1))
          i)))

  (define (nearest-stop stops current)
    (if (pair? stops)
        (fold-left (lambda (best stop)
                     (if (< (abs (- stop current))
                            (abs (- best current)))
                         stop
                         best))
                   (car stops) stops)
        stops))

  (define (reindent-scheme-input text pos)
    ;; Reindent every logical line and keep the cursor attached to the same
    ;; text even when an earlier edit shifts this line left or right.
    (let* ([lines (string:lines text)]
           [v (list->vector lines)]
           [stops (scheme-format:indent-lines v 0 (- (vector-length v) 1))]
           [before (string:lines (substring text 0 pos))]
           [point-row (- (length before) 1)]
           [point-col (string-length (car (reverse before)))])
      (let loop ([rows lines] [cols stops] [row 0]
                 [built '()] [offset 0] [cursor #f])
        (if (null? rows)
            (cons (string:join (reverse built) "\n") cursor)
            (let* ([line (car rows)]
                   [old (leading-blanks line)]
                   [target (and (car cols) (nearest-stop (car cols) old))]
                   [laid (if target
                             (string-append (make-string target #\space)
                                            (string:tail line old))
                             line)]
                   [cursor (if (= row point-row)
                               (+ offset
                                  (if target
                                      (max target (+ point-col (- target old)))
                                      point-col))
                               cursor)])
              (loop (cdr rows) (cdr cols) (+ row 1)
                    (cons laid built) (+ offset (string-length laid) 1)
                    cursor))))))

  (define (indent-scheme-insertion text pos inserted)
    ;; Preserve multiline insertion as entered; the prompt's central edit path
    ;; immediately runs reindent-scheme-input over the complete result.
    (cons (string-append (substring text 0 pos) inserted
                         (string:tail text pos))
          (+ pos (string-length inserted))))

  (define (mx-edge-motion action text pos second?)
    ;; First C-a/C-e addresses the logical line; a consecutive second press
    ;; addresses the whole M-x input even when the first did not move point.
    (if second?
        (if (eq? action 'beginning) 0 (string-length text))
        (let* ([start (let back ([i pos])
                        (if (and (> i 0)
                                 (not (char=? (string-ref text (- i 1))
                                              #\newline)))
                            (back (- i 1))
                            i))]
               [end (let forward ([i pos])
                      (if (and (< i (string-length text))
                               (not (char=? (string-ref text i) #\newline)))
                          (forward (+ i 1))
                          i))])
          (if (eq? action 'end)
              end
              (let skip ([i start])
                (if (and (< i end)
                         (memv (string-ref text i) '(#\space #\tab)))
                    (skip (+ i 1))
                    i))))))

  (define (evaluate-text text)
    ;; Evaluate every datum in text in the M-x interaction environment and
    ;; return the values of the last one. An empty input returns no values.
    (let ([in (open-input-string text)])
      (let loop ([last '()])
        (let ([form (read in)])
          (if (eof-object? form)
              (apply values last)
              (loop (call-with-values
                      (lambda () (eval form (interaction-environment)))
                      list)))))))

  (define (evaluation-outcome label text)
    ;; Evaluation is one undo step in every buffer it edits, and C-g can
    ;; interrupt it whether it came from M-x or eval:run!.
    (define (run)
      (guard (ex [(head:interrupted? ex) "interrupted"]
                 [else (format "error: ~a" (kernel:condition-text ex))])
        (head:call-with-interrupt
          (lambda ()
            (call-as-one-edit! label
              (lambda ()
                (let-values ([vals (evaluate-text text)]) vals)))))))
    (let ([lock (make-mutex)]
          [terminal (sys:duplicate-standard-output-port)])
      (define (record! component line)
        (parameterize ([sys:terminal-output-port terminal])
          (with-mutex lock (log:add! component line))))
      (dynamic-wind
        void
        (lambda ()
          (values
            (parameterize ([sys:terminal-output-port terminal])
              (sys:call-with-streamed-output
                (lambda (line) (record! 'stdout line))
                (lambda (line) (record! 'stderr line))
                run))
            '()))
        (lambda () (close-port terminal)))))

  (define (report-evaluation! query outcome output-records)
    (let* ([failed? (string? outcome)]
           [void? (and (not failed?)
                       (or (null? outcome)
                           (and (null? (cdr outcome))
                                (eq? (car outcome) (void)))))]
           [result (if failed?
                       outcome
                       (string:join (map (lambda (v) (format "~s" v)) outcome)
                                    ", "))])
      (let* ([copied? (and (eval-copy-result) (not failed?) (not void?))]
             [result-record
              (log:add! 'eval (cons query (if void? "#<void>" result)) #f)])
        (when copied? (copy-to-kill-buffer! result))
        (present-log-entries!
          (append output-records (list result-record))
          (if copied? " [stored in kill ring]" "")))))

  (edoc "Evaluate the Scheme text in where, the whole current buffer by default, in the M-x interaction environment and show the last result in the echo area."
        (where* (list-of (or buffer string region procedure list)) "what to evaluate, at most one: a buffer, its name, a region, a predicate on buffers or a list of these"))
  (define (eval! . where*)
    (let* ([where (if (pair? where*) (car where*) (head:current-buffer))]
           [query (if (pair? where*) (format "(eval! ~s)" where) "(eval!)")]
           [text (string:join (map region-text (regions-of where)) "\n")])
      (let-values ([(outcome output-records)
                    (evaluation-outcome query text)])
        (report-evaluation! query outcome output-records))
      (void)))

  (edoc "Read an expression at the M-x prompt, with completion and hints, evaluate it in the editor top level and log the exchange; the result shows in the echo area.")
  (define (eval!!)
    ;; Read an expression -- the prompt pretypes "(", deletable, so a
    ;; bare symbol evaluates too -- and evaluate it in the editor's
    ;; own top level.  The expression is logged (component eval, which
    ;; also carries the history); the result shows in the echo area,
    ;; transiently like any message, and lands in the log with it.
    (let ([s (parameterize ([prompt:ghost signature-ghost]
                            [prompt:multiline indent-scheme-insertion]
                            [prompt:edge-motion mx-edge-motion]
                            [prompt:reindent reindent-scheme-input]
                            [paint:echo-highlight mx-echo-styles])
               (prompt:read! "M-x " complete-symbol "("
                             (box (log:history 'eval car))
                             complete-editor-symbol normalize-input))])
      (when (and s (> (string-length s) 0) (not (string=? s "(")))
        ;; Keep the prompt on screen while its expression evaluates --
        ;; forgiven parentheses included -- with the cursor parked at
        ;; its end, drawn as the evaluation-in-progress underline.
        ;; An indicator, not a record: the expression is already
        ;; logged under eval.
        (paint:show-prompt-message! "M-x " s mx-echo-styles)
        (let-values ([(outcome output-records)
                      (parameterize ([paint:cursor-in-echo #t])
                        (paint:redraw!)
                        (evaluation-outcome s s))])
          ;; One structured record per exchange: history reads the query,
          ;; while the view and echo show the formatted pair.
          (report-evaluation! s outcome output-records)))))

  (edoc "Install the evaluation commands: their describe entries, the log formatter and the C-x C-e and M-x bindings.")
  (define (init!)
    (doc:register!
      '(((eval:run!) (("procedure" . "(eval:run! [where])")) "void"
         ("(eval)") eval "Evaluation commands" #f
         "Evaluate every Scheme datum in `where` in the same interaction environment as M-x and show the last datum's result in the echo area. Non-void results are stored in the kill ring when `eval-copy-result` is true. Standard output and error are logged per line under `stdout` and `stderr`, including child-process output. By default, evaluate the whole current buffer; `where` accepts the same buffer, name, region, predicate, and list forms as the editing commands.")
        ((eval:run!!) (("procedure" . "(eval:run!!)")) "void"
         ("(eval)") eval "Evaluation commands" #f
         "Prompt for a Scheme expression, evaluate it in the editor's interaction environment, and record the expression and result in the log. Non-void results are stored in the kill ring when `eval-copy-result` is true. Standard output and error are logged per line under `stdout` and `stderr`, including child-process output.")))
    (log:register-formatter! 'eval format-exchange style-exchange)
    (keymap:bind-default! "C-x C-e" eval!)
    (keymap:bind-default! "M-x" eval!!)))
