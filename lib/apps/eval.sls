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
;; C-x C-e evaluates the expression before point and C-M-x the top-level
;; form around it, as in Emacs, in that same top level; eval:run! takes
;; the selected region or the whole buffer at M-x.

(import (only (foundation edoc) elibrary))
(elibrary (apps eval)
  (export call-with-evaluation! completion-candidates completion-extensions completion-span
          (rename (evaluation-condition condition)) (rename (eval-copy-result copy-result))
          init! input-closers input-diagnostic (rename (eval-last-expression! last-expression!))
          (rename (eval-prompt! prompt!))
          (rename (eval-prompt-with! prompt-with!)) report! (rename (eval! run!)) settle-completion
          (rename (evaluation-status status)) (rename (eval-top-level-form! top-level-form!)) type-fits?
          (rename (evaluation-values values)))
  (import (chezscheme)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation fuzzy) fuzzy:)
          (prefix (only (foundation scheme-format) indent-lines delimiter?) scheme-format:)
          (prefix (foundation string) string:)
          (prefix (head dispatch) dispatch:)
          (prefix (head echo) echo:)
          (prefix (head edit) edit:)
          (prefix (head expression) expression:)
          (prefix (head head) head:)
          (prefix (head keymap) keymap:)
          (prefix (head mode) mode:)
          (prefix (head paint) paint:)
          (prefix (head prompt) prompt:)
          (prefix (head style) style:)
          (prefix (service doc) doc:)
          (prefix (service log) log:)
          (prefix (only (service reference) lookup) reference:)
          (prefix (only (sys sys) call-with-streamed-output duplicate-standard-output-port terminal-output-port) sys:))

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

  (define (string-content raw)
    ;; what a string literal's text so far denotes, its escapes read: quo\"te
    ;; is quo"te; the text itself when it does not read, an escape left open say
    (guard (ex [else raw])
      (let ([v (read (open-input-string (string-append "\"" raw "\"")))])
        (if (string? v) v raw))))

  (define (string-escaped value)
    ;; a value as a string literal holds it, without the quotes: quo"te is quo\"te
    (let ([w (call-with-string-output-port (lambda (p) (write value p)))])
      (substring w 1 (- (string-length w) 1))))

  (define (open-position? s pos)
    ;; whether an empty token at pos stands where a datum is due: at the
    ;; start or after a separator; glued to a closed string or form it does
    ;; not, that datum being final, for Tab to settle around
    (or (= pos 0) (and (memv (string-ref s (- pos 1)) '(#\space #\tab #\newline #\( #\[ #\{)) #t)))

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
    ;; procedure's or a keyword's from its argument names, in order; #f for
    ;; the rest
    (case (edoc:signature-kind sig)
      [(procedure) (edoc:signature-formals sig)]
      [(constructor accessor mutator predicate syntax) (map edoc:argument-name (edoc:signature-arguments sig))]
      [else #f]))

  (define (named-signatures sym)
    ;; the signatures recorded under a name, a keyword's or a type's: as
    ;; spelled, else, for a module-prefixed name, under the library's own
    ;; spelling when the library is the module
    (define (some sigs) (and (pair? sigs) sigs))
    (define (of-module? sig prefix)
      ;; whether the signature's library, "(kind leaf)" or "(leaf)", has the
      ;; prefix's module as its leaf: the prefix its names carry at M-x
      (let* ([library (edoc:signature-library sig)] [n (string-length library)] [k (string-length prefix)])
        (and (> n (+ k 1))
             (memv (string-ref library (- n k 2)) '(#\( #\space))
             (string=? (substring library (- n k 1) (- n 1)) prefix)
             #t)))
    (or (some (edoc:edoc-named sym))
        (let* ([text (symbol->string sym)] [n (string-length text)])
          (let loop ([i 0])
            (cond
              [(= i n) #f]
              [(char=? (string-ref text i) #\:)
               (let ([prefix (substring text 0 i)]
                     [sigs (edoc:edoc-named (string->symbol (substring text (+ i 1) n)))])
                 (some (filter (lambda (sig) (of-module? sig prefix)) (or sigs '()))))]
              [else (loop (+ i 1))])))))

  (define (operator-signatures sym)
    ;; the signatures of the callable bound to sym, else those recorded under its name
    (let ([value (and (top-level-bound? sym) (top-level-value sym))])
      (if (procedure? value) (edoc:edoc-of value) (named-signatures sym))))

  (define (argument-type sym index)
    ;; the type documented for argument index of the callable bound to sym,
    ;; the union of its lambda lists' answers, or #f
    (let ([signatures (operator-signatures sym)])
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
    ;; (type start end token where) for the cursor at a documented argument
    ;; position: the argument's type, the range and text of the token being
    ;; completed, and where it sits: #t inside a string literal, literal in
    ;; a string at an argument whose type spells its values as literals,
    ;; which expands into the literal from its quote, inside for a bare
    ;; token within a literal constructor, else #f. At the operator position
    ;; of a nested form, (head:show-buffer! (bu, the token is the form's
    ;; opening and the type is the enclosing argument's: whatever the form
    ;; produces has to serve it. #f under a quote, or without a type.
    (define (typed frame start end token where)
      (let ([type (argument-type (string->symbol (frame-operator frame)) (frame-arguments frame))])
        (and type (list type start end token where))))
    (define (plain? frame) (and (not (frame-quoted? frame)) (string? (frame-operator frame))))
    (define (element-type type)
      ;; the element type of a (list-of t) argument, or of such a member of an or
      (cond [(and (pair? type) (eq? (car type) 'list-of) (pair? (cdr type))) (cadr type)]
            [(and (pair? type) (eq? (car type) 'or)) (exists element-type (cdr type))]
            [else #f]))
    (define (literal-typed? type)
      ;; whether the type, or a member of a union, spells its values as literals
      (cond [(symbol? type) (and (memq type (edoc:type-literals)) #t)]
            [(and (pair? type) (eq? (car type) 'or)) (exists literal-typed? (cdr type))]
            [else #f]))
    (define (literal-operator? frame)
      ;; whether the frame's operator denotes a value: a type's derived
      ;; literal, or a constructor (head literal) writes by hand, (agent
      ;; "name") say, which are the procedures that library documents
      (let ([sym (string->symbol (frame-operator frame))])
        (or (and (memq sym (edoc:type-literals)) #t)
            (let ([sigs (operator-signatures sym)])
              (and sigs
                   (exists (lambda (sig) (and (eq? (edoc:signature-kind sig) 'procedure) (equal? (edoc:signature-library sig) "(head literal)"))) sigs)
                   #t)))))
    (let* ([quote-at (open-string-start s pos)]
           [range (and (not quote-at) (symbol-range s pos))]
           [start (cond [quote-at (+ quote-at 1)] [range (car range)] [else pos])]
           [end (cond [quote-at pos] [range (cdr range)] [else pos])]
           [token (let ([raw (substring s start end)]) (if quote-at (string-content raw) raw))]
           [frames (call-frames (substring s 0 (if quote-at quote-at start)))])
      (and (or quote-at (and range (< (car range) (cdr range))) (open-position? s pos)) (pair? frames)
           (let ([frame (car frames)])
             (cond
               [(plain? frame)
                (let ([context (typed frame start end token (and quote-at #t))])
                  (cond [(not context) #f]
                        ;; inside (file "~ the values spell bare, never a literal within a literal
                        [(literal-operator? frame) (if quote-at context (list (car context) start end token 'inside))]
                        ;; a string at an argument spelling its values as
                        ;; literals expands into the literal, from its quote
                        [(and quote-at (literal-typed? (car context))) (list (car context) quote-at end token 'literal)]
                        [else context]))]
               [(and (eq? (frame-operator frame) 'pending) (not (frame-quoted? frame)) (not quote-at)
                     (pair? (cdr frames)) (plain? (cadr frames))
                     (> start 0) (char=? (string-ref s (- start 1)) (frame-opener frame)))
                (typed (cadr frames) (- start 1) end (substring s (- start 1) end) #f)]
               ;; an element of a quoted list at an argument typed (list-of t)
               ;; takes t: '("../sch completes directories under a roots argument
               [(and (frame-quoted? frame) (pair? (cdr frames)) (plain? (cadr frames)))
                (let ([type (element-type (argument-type (string->symbol (frame-operator (cadr frames)))
                                                         (frame-arguments (cadr frames))))])
                  (and type (list type start end token (and quote-at #t))))]
               [else #f])))))

  (edoc "Whether a produced type serves a wanted one: the same, one refining it, a named type and the record type it denotes either way round, or a member serving a member across unions; #f, the absence a union allows, serves nothing."
        (wanted datum "the argument's documented type")
        (produced datum "the documented result type")
        (returns boolean))
  (define (type-fits? wanted produced)
    ;; A procedure that may return #f is no producer of the other member:
    ;; (or integer #f) does not serve (or mode #f).
    (define (refines? wanted produced fuel)
      (let ([record (and (symbol? produced) (> fuel 0) (edoc:type-named produced))])
        (and record (edoc:type-within record)
             (or (member-fits? wanted (edoc:type-within record)) (refines? wanted (edoc:type-within record) (- fuel 1))))))
    (define (record-of? t) (and (pair? t) (eq? (car t) 'record)))
    (define (members t)
      (cond [(and (pair? t) (eq? (car t) 'or)) (apply append (map members (cdr t)))]
            [(eq? t #f) '()]
            [else (list t)]))
    (define (member-fits? wanted produced)
      (or (equal? wanted produced)
          (refines? wanted produced 8)
          (and (record-of? wanted) (symbol? produced) (edoc:type-denotes-record? produced (cadr wanted)))
          (and (record-of? produced) (symbol? wanted) (edoc:type-denotes-record? wanted (cadr produced)))))
    (and (exists (lambda (w) (exists (lambda (p) (member-fits? w p)) (members produced))) (members wanted)) #t))

  ;; the language's types, integer or boolean say, belong to the edoc library
  (define language-owner (edoc:type-owner (edoc:type-named 'boolean)))

  (define (module-type? type)
    ;; a type with values worth scanning the top level for: one a library
    ;; defined, or a record; the language's types would list everything
    (cond [(symbol? type)
           (let ([record (edoc:type-named type)])
             (and record (not (equal? (edoc:type-owner record) language-owner)) (edoc:type-owner record) #t))]
          [(and (pair? type) (eq? (car type) 'record)) #t]
          [(and (pair? type) (eq? (car type) 'or)) (exists module-type? (cdr type))]
          [else #f]))

  ;; A typed candidate: the text the token matches against, what Tab inserts
  ;; for it alone, the label the row shows, the grey hint beside it, and the
  ;; label's face. The text is the candidate's opening: its spelling without
  ;; the closers the settle step supplies, so an extension never closes the
  ;; form the token is still inside.
  (define-record-type option (fields text insert label hint face value))

  (define (opening spelling)
    (let loop ([end (string-length spelling)])
      (if (and (> end 1) (memv (string-ref spelling (- end 1)) '(#\) #\] #\")))
          (loop (- end 1))
          (substring spelling 0 end))))

  (define (value-options type token in-string?)
    ;; the type's values as options, with the value itself: inside a string
    ;; the bare string values, completing the literal in place; inside a
    ;; literal constructor, (file "~ say, each value's own spelling; elsewhere,
    ;; and for a string expanding into a literal, the literal denoting it
    (fold-right
      (lambda (pair out)
        (let ([value (car pair)] [hint (or (cdr pair) "")])
          (cond
            [(eq? in-string? #t)
             ;; the literal stays open: the settle step closes it at the
             ;; session's dead end, once nothing more completes from the value
             (if (string? value) (cons (make-option value value value hint 'plain value) out) out)]
            [else (let ([text (if (eq? in-string? 'inside)
                                  (edoc:type-spelling type value)
                                  (edoc:type-literal-spelling type value))])
                    (cons (make-option (opening text) text text hint 'plain value) out))])))
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
                  (cons (make-option text insert label (edoc:signature-summary sig) 'editor #f) out)))))
          '() (let ([literals (edoc:type-literals)])
                ;; a type's derived literal produces its values by definition
                ;; and adds nothing to them; a constructor written by hand,
                ;; (buffer name) or (head name . more), is a producer like any
                (filter (lambda (entry)
                          (not (and (memq (car entry) literals) (eq? (top-level-value (car entry)) (edoc:type-literal (car entry))))))
                        (documented-symbols)))))))

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
                            (if (procedure? value) (completion-hint sym) (edoc:type-spelling type value)) 'editor #f)
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

  (define (quoted-part text)
    ;; the text after its first quote, a literal opening's value part, ~/ in
    ;; (file "~/ say; #f without one
    (let find ([i 0])
      (cond [(>= i (string-length text)) #f]
            [(char=? (string-ref text i) #\") (substring text (+ i 1) (string-length text))]
            [else (find (+ i 1))])))

  (define (value-part text)
    ;; the value in a candidate's text: after the opening quote of a literal
    ;; or of a string spelling, (file "~/ or "~/; a bare value is all of it,
    ;; a quote within a name notwithstanding
    (if (and (> (string-length text) 0) (memv (string-ref text 0) '(#\( #\")))
        (or (quoted-part text) text)
        text))

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
           (let* ([matched (fold-right
                             (lambda (o out)
                               (let ([m (hashtable-ref by-text (option-text o) #t)])
                                 (if (eq? m #t) out (cons (cons o m) out))))
                             '() all)]
                  ;; a token the matcher has no segment for, ~ or / say, still
                  ;; leads the values it begins: the home or the root directory
                  [matched (if (pair? matched) matched
                               (fold-right (lambda (o out)
                                             (let ([text (option-text o)])
                                               (if (string:prefix? token (value-part text)) (cons (cons o #f) out) out)))
                                           '() all))])
             (and (pair? matched)
                  (map (lambda (entry) (cons (car entry) (if (cdr entry) (fuzzy:fragments (cdr entry)) '())))
                       (list-sort (lambda (a b) (and (cdr a) (cdr b) (quality<? (fuzzy:score (cdr a)) (fuzzy:score (cdr b))) #t))
                                  matched)))))])))

  (define (dead-end? type value)
    ;; whether completing from a string value offers nothing but the value
    ;; itself: the completion session's end, where its literal closes. A
    ;; directory offers its entries; a file, or a directory without
    ;; subdirectories to a directory argument, offers itself alone.
    (let ([options (typed-options (list type 0 0 value #t))])
      (or (not options)
          (for-all (lambda (entry) (string=? (option-text (car entry)) value)) options))))

  (define (typed-inserts s context options)
    ;; what Tab puts in place of the token: a sole candidate whole; inside a
    ;; string the candidates' longest common prefix when it extends the
    ;; token, as a shell does, since the values are literal (a projection of
    ;; what they share, manual/md, would be no path, and the projection walk
    ;; grows with a path's parts); else the safe extensions of the token over
    ;; the candidates' texts, the longest that every current match still
    ;; matches, as for symbols, and that leaves the cursor at a typed
    ;; argument: an extension opening a string after an operator nobody
    ;; documents, (hea" say, would strand it
    (let ([type (car context)] [start (cadr context)] [end (caddr context)] [token (cadddr context)] [in-string? (car (cddddr context))])
      (define (typed-still? text)
        (and (argument-context (string-append (substring s 0 start) text (substring s end (string-length s)))
                               (+ start (string-length text)))
             #t))
      (define (sole-insert option)
        ;; a sole value whole: a string value still completing on, a
        ;; directory say, open to continue into, else closed at its dead end
        (let ([value (option-value option)])
          (if (and (string? value) (not (eq? in-string? #t)) (not (dead-end? type value)))
              (option-text option)
              (option-insert option))))
      (define (literal-common)
        ;; the openings' common prefix when its value part extends the
        ;; token, (file "/ over the root's entries say, else #f
        (let* ([common (string:common-prefix (map (lambda (entry) (option-text (car entry))) options))]
               [value-part (quoted-part common)])
          (and value-part (string:prefix? token value-part) common)))
      (define (bare-inserts)
        ;; the inserts as the candidates' texts spell them
        (cond
          [(null? (cdr options)) (list (sole-insert (car (car options))))]
          ;; a string or bare token becoming a value's spelling: the openings'
          ;; common prefix when its value extends the token, as inside a string
          [(memq in-string? '(literal inside)) (list (or (literal-common) (substring s start end)))]
          [in-string?
           (let ([common (string:common-prefix (map (lambda (entry) (option-text (car entry))) options))])
             (list (if (and (> (string-length common) (string-length token)) (string:prefix? token common)) common token)))]
          ;; a bare token the values alone match, / over the root's entries
          ;; say, opens their literal as a string would; with a symbol among
          ;; the matches it extends as symbols do
          [(and (for-all (lambda (entry) (option-value (car entry))) options) (literal-common)) => list]
          [else
           (let ([seen (make-hashtable string-hash string=?)])
             (fuzzy:expansions token
                               (fold-right (lambda (entry out)
                                             (let ([text (option-text (car entry))])
                                               (if (hashtable-ref seen text #f) out
                                                 (begin (hashtable-set! seen text #t) (cons text out)))))
                                 '() options)
                               typed-still?))]))
      ;; inside a string the inserts are what the literal holds, a quote or a
      ;; backslash in a name escaped, as the token was read
      (let ([inserts (bare-inserts)])
        (if (eq? in-string? #t) (map string-escaped inserts) inserts))))

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

  (edoc "The span Tab replaces at a typed argument position, (start . end) character offsets, or #f: the token, or the whole string when it expands into a literal."
        (text string "the prompt input")
        (pos integer "the cursor position")
        (returns (or pair #f)))
  (define (completion-span text pos)
    (let* ([context (argument-context text pos)] [options (and context (typed-options context))])
      (and options (cons (cadr context) (caddr context)))))

  (define hint-cache (make-weak-eq-hashtable))

  (edoc "The grey text beside a candidate: what it takes, then its edoc summary; \"\" when nothing local is known."
        (sym symbol "the candidate")
        (returns string)
        (effects internal))
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
          (let* ([signatures (or (and bound? (edoc:edoc-of value)) (named-signatures sym))]
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
            (if (or (not range) (and (= (car range) (cdr range)) (not (open-position? s pos)))) (values #f #f '() '())
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
      (lambda (text pos) (settle-completion text pos))
      ;; what the list holds, for its status line: the argument's type at a
      ;; typed position, else the symbols offered
      (lambda (s pos)
        (let ([context (and typed? (argument-context s pos))])
          (if (and context (typed-options context))
              (type-text (car context))
              (if typed? "symbol" "editor symbol"))))))

  (define (type-text type)
    ;; a type as the status line names it: a name as itself, a record type
    ;; by its record, a compound as written
    (cond [(symbol? type) (symbol->string type)]
          [(and (pair? type) (eq? (car type) 'record) (pair? (cdr type))) (symbol->string (cadr type))]
          [else (format "~s" type)]))

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

  (edoc "The input to continue with after a sole completion ends at pos: inside a string, a typed value at its dead end closes the literal and settles on, one that completes further stays open; a form whose operator has a known arity closes when complete and settles again in its parent, or steps to its next argument; an unknown arity, a quoted form, text after pos or an input that does not read leaves the cursor where it is."
        (text string "the prompt input")
        (pos integer "where the completed symbol ends")
        (returns pair "the new input and cursor position, (text . pos)"))
  (define (settle-completion text pos)
    ;; The input to continue with after a sole completion ends at pos. Inside
    ;; a string the typed session is judged: a value that still completes on
    ;; stays open, one at its dead end closes the literal and settles on.
    ;; Then, while the enclosing operator has a fixed arity, a complete form
    ;; closes with its matching bracket and settles again as an argument of
    ;; its parent, and an incomplete one steps to its next argument. An
    ;; unknown arity, a quoted form, text after pos or an input that does
    ;; not read leaves the cursor where it is; a settled input always reads.
    (define (blank? from)
      (let loop ([i from])
        (or (>= i (string-length text))
            (and (memv (string-ref text i) '(#\space #\tab #\newline)) (loop (+ i 1))))))
    (define (trim-right s)
      (let loop ([n (string-length s)])
        (if (and (> n 0) (memv (string-ref s (- n 1)) '(#\space #\tab #\newline))) (loop (- n 1)) (substring s 0 n))))
    (define (settle frames out at)
      (if (or (null? frames) (frame-quoted? (car frames)))
          (cons out at)
          (let* ([frame (car frames)]
                 [operator (frame-operator frame)]
                 [arity (and (string? operator) (fixed-arity (string->symbol operator)))])
            (cond
              [(not arity) (cons out at)]
              ;; one space on to the next argument, unless a separator is there already
              [(< (frame-arguments frame) arity)
               (if (< (string-length (trim-right out)) (string-length out)) (cons out at) (cons (string-append out " ") (+ at 1)))]
              ;; a complete form closes flush, never as (f )
              [(= (frame-arguments frame) arity)
               (let ([out (string-append (trim-right out) (string (cdr (assv (frame-opener frame) closers))))])
                 (settle (count-datum (cdr frames) #f) out (string-length out)))]
              [else (cons out at)]))))
    (define (settled head tail at)
      (let* ([result (settle (call-frames head) head at)]
             [out (cons (string-append (car result) tail) (cdr result))])
        (if (input-closers (car out)) out (cons text pos))))
    (cond
      [(not (blank? pos)) (cons text pos)]
      [(open-string-start text pos)
       (let ([context (argument-context text pos)])
         (if (and context (car (cddddr context)) (dead-end? (car context) (cadddr context)))
             (settled (string-append (substring text 0 pos) "\"") (substring text pos (string-length text)) (+ pos 1))
             (cons text pos)))]
      [(not (input-closers text)) (cons text pos)]
      [else (settled (substring text 0 pos) (substring text pos (string-length text)) pos)]))

  ;;; Reading the input -----------------------------------------------------------

  (define (scan-openers text)
    ;; (values #t closers) for an input whose open string and forms can be
    ;; closed, the closers innermost first; (values #f complaint) at a closer
    ;; matching nothing: an extra one, or one of the wrong kind. Strings,
    ;; character literals and comments are skipped as the reader would.
    (define n (string-length text))
    (define (closers-of stack) (apply string-append (map (lambda (opener) (string (cdr (assv opener closers)))) stack)))
    (let loop ([i 0] [stack '()])
      (if (>= i n)
          (values #t (closers-of stack))
          (let ([c (string-ref text i)])
            (cond
              [(char=? c #\")
               (let string ([j (+ i 1)])
                 (cond [(>= j n) (values #t (string-append "\"" (closers-of stack)))]
                       [(char=? (string-ref text j) #\\) (string (+ j 2))]
                       [(char=? (string-ref text j) #\") (loop (+ j 1) stack)]
                       [else (string (+ j 1))]))]
              [(char=? c #\;)
               ;; a comment reaching the end hides what follows it: the
               ;; closers go on a line of their own
               (let comment ([j i])
                 (cond [(>= j n) (values #t (string-append "\n" (closers-of stack)))]
                       [(char=? (string-ref text j) #\newline) (loop j stack)]
                       [else (comment (+ j 1))]))]
              [(and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\\))
               ;; a character literal: #\x, #\(, or a named one like #\space
               (let name ([j (+ i 3)])
                 (if (and (< j n) (< (+ i 2) n) (char-alphabetic? (string-ref text (+ i 2))) (char-alphabetic? (string-ref text j)))
                     (name (+ j 1))
                     (loop (min j n) stack)))]
              [(and (char=? c #\#) (< (+ i 1) n) (char=? (string-ref text (+ i 1)) #\|))
               (let block ([j (+ i 2)] [depth 1])
                 (cond [(>= (+ j 1) n) (loop n stack)]
                       [(and (char=? (string-ref text j) #\|) (char=? (string-ref text (+ j 1)) #\#))
                        (if (= depth 1) (loop (+ j 2) stack) (block (+ j 2) (- depth 1)))]
                       [(and (char=? (string-ref text j) #\#) (char=? (string-ref text (+ j 1)) #\|))
                        (block (+ j 2) (+ depth 1))]
                       [else (block (+ j 1) depth)]))]
              [(assv c closers) (loop (+ i 1) (cons c stack))]
              [(memv c '(#\) #\] #\}))
               (cond [(null? stack) (values #f (format "unexpected ~a" c))]
                     [(char=? c (cdr (assv (car stack) closers))) (loop (+ i 1) (cdr stack))]
                     [else (values #f (format "~a closes ~a" c (car stack)))])]
              [else (loop (+ i 1) stack)])))))

  (define (read-all text)
    ;; every datum of text read, or the reader's complaint raised
    (let ([port (open-string-input-port text)])
      (let loop () (unless (eof-object? (read port)) (loop)))))

  (define (reader-complaint ex)
    ;; the reader's message alone, without the position in its port
    (let ([text (if (and (message-condition? ex) (irritants-condition? ex))
                    (let ([message (condition-message ex)] [irritants (condition-irritants ex)])
                      (if (and (string:prefix? "~?" message) (pair? irritants) (pair? (cdr irritants)) (string? (car irritants)))
                          (guard (ex [else message]) (apply format (car irritants) (cadr irritants)))
                          (guard (ex [else message]) (format "~?" message irritants))))
                    (kernel:condition-text ex))])
      (let cut ([i 0])
        (cond [(> (+ i 9) (string-length text)) text]
              [(string=? (substring text i (+ i 9)) " at char ") (substring text 0 i)]
              [else (cut (+ i 1))]))))

  (edoc "The closers that complete the M-x input as data: a quote for a string left open, then the bracket of every unclosed form, innermost first; \"\" when it reads as it is, #f when no closing makes it read."
        (text string "the prompt input")
        (returns (or string #f)))
  (define (input-closers text)
    (let-values ([(ok? tail) (scan-openers text)])
      (and ok? (guard (ex [else #f]) (read-all (string-append text tail)) tail))))

  (edoc "Why the M-x input does not read as data even with its open string and forms closed, in a few words for the ghost; #f when it reads."
        (text string "the prompt input")
        (returns (or string #f)))
  (define (input-diagnostic text)
    (let-values ([(ok? tail) (scan-openers text)])
      (if (not ok?)
          tail
          (guard (ex [else (reader-complaint ex)])
            (read-all (string-append text tail))
            #f))))

  (define (input-ghost s)
    ;; the M-x ghost: what keeps an input from reading, bracketed, else the
    ;; parameters the innermost open call still expects
    (let ([complaint (input-diagnostic s)])
      (if complaint (string-append " [" complaint "]") (signature-ghost s))))

  ;;; Evaluation ----------------------------------------------------------------

  (edoc "Whether a non-void evaluation result is also placed in the copy buffer."
        (value boolean))
  (define eval-copy-result (make-parameter #t))

  (define (close-expression text)
    ;; text completed with the closers it is missing -- a quote for an open
    ;; string, then its open brackets -- so it reads as data; #f when that
    ;; is not enough to make it read
    (let ([tail (input-closers text)])
      (and tail (string-append text tail))))

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

  ;; the M-x prompt's label: a lambda, the mark of an expression to evaluate
  ;; (keymap's action-text spells it too, describing a pre-filled key)
  (define mx-label "λ ")

  (define mx-echo-styles
    ;; Scheme highlighting for the M-x prompt: the label stays grey,
    ;; the expression styles as Scheme with the editor's own names in
    ;; the editor style.
    (paint:prompt-styler mx-label
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

  (edoc "An evaluation's outcome, before reporting or copying it."
        (status symbol "ok, error or interrupted")
        (values list "the returned values, empty on failure")
        (condition (or condition #f) "the original condition, or #f on success")
        (spoken any "private echo observation before execution"))
  (define-record-type evaluation (fields status values condition spoken))

  (define evaluating? (make-thread-parameter #f))

  (edoc "Run a thunk with C-g interruption, streamed stdout/stderr logging and one undo group. Return its values or original condition in an evaluation result, without reporting it. Nested calls share the outer capture and interruption scope. Runs on the head's main thread."
        (label string "the undo label")
        (thunk thunk "the computation, returning ordinary Scheme values")
        (returns (record evaluation)))
  (define (call-with-evaluation! label thunk)
    (define spoken (echo:text))
    (define (run)
      (guard (ex [else (make-evaluation (if (head:interrupted? ex) 'interrupted 'error) '() ex spoken)])
        (call-with-values
          (lambda () (edit:call-as-one-edit! label thunk))
          (lambda vals (make-evaluation 'ok vals #f spoken)))))
    (if (evaluating?) (run)
      (let ([lock (make-mutex)]
            [terminal (sys:duplicate-standard-output-port)])
        (define (record! component line . show)
          (parameterize ([sys:terminal-output-port terminal])
            (with-mutex lock (apply log:add! component line show))))
        (define compile-default (compile-library-handler))
        (define (compile-quietly source object)
          ;; A library compiled on import, once an extension enabled lazy
          ;; compilation, is bookkeeping: a compile record naming its source
          ;; for the log, nothing in the echo area, and Chez's own line
          ;; withheld. A compilation that fails raises into the result.
          (record! 'compile source #f)
          (parameterize ([compile-file-message #f]) (compile-default source object)))
        (dynamic-wind
          void
          (lambda ()
            (parameterize ([sys:terminal-output-port terminal] [evaluating? #t]
                           [compile-library-handler compile-quietly])
              (sys:call-with-streamed-output
                (lambda (line) (record! 'stdout line))
                (lambda (line) (record! 'stderr line))
                (lambda () (head:call-with-interrupt run)))))
          (lambda () (close-port terminal))))))

  (edoc "Report an evaluation in the echo area, copying non-void values when copy-result is enabled and preserving a message a void command spoke. The record goes to the log under the caller's component, an extension's own; given the M-x input as a string instead, it is recorded as an eval exchange, with its history."
        (outcome (record evaluation) "the execution result")
        (destination (or symbol string) "the log component for the record, or the M-x input to record as an exchange"))
  (define (report! outcome destination)
    (unless (or (symbol? destination) (string? destination))
      (error 'eval:report! "expected a log component or the M-x input" destination))
    (let ([query (and (string? destination) destination)] [component (if (string? destination) 'eval destination)])
      ;; A command run at M-x that spoke in the echo area, (edit:answer! ...)
      ;; say, keeps its message: a void result is logged but not shown over
      ;; it. Spoken is the echo text before the evaluation, when known.
      (let* ([failed? (not (eq? (evaluation-status outcome) 'ok))]
             [vals (evaluation-values outcome)]
             [void? (and (not failed?)
                      (or (null? vals)
                          (and (null? (cdr vals))
                               (eq? (car vals) (void)))))]
             [result (if failed?
                       (if (eq? (evaluation-status outcome) 'interrupted) "interrupted"
                         (format "error: ~a" (kernel:condition-text (evaluation-condition outcome))))
                       (string:join (map (lambda (v) (format "~s" v)) vals)
                                    ", "))]
             [spoke? (and void?
                       (let ([now (echo:text)])
                         (and (string? now) (> (string-length now) 0) (not (equal? now (evaluation-spoken outcome))))))])
        (let* ([copied? (and (eval-copy-result) (not failed?) (not void?))]
               [result-record
                (log:add! component
                  (let ([text (if void? "#<void>" result)]) (if query (cons query text) text)) #f)])
          (when copied? (edit:copy-text! result))
          (unless spoke?
            (edit:present-log-entries!
              (list result-record)
              (if copied? " [copied]" "")))))))

  (edoc "Evaluate the Scheme text of the selected region, else of the whole current buffer, in the M-x interaction environment and show the last result in the echo area.")
  (define (eval!)
    (report! (call-with-evaluation! "(eval!)"
               (lambda () (evaluate-text (edit:region-text (edit:current-region))))) "(eval!)")
    (void))

  (define (evaluate-span! start end label)
    ;; the buffer text between two positions, evaluated and reported as
    ;; the exchange it is: the expression, then its result
    (let ([text (expression:text (head:current-buffer) start end)])
      (report! (call-with-evaluation! label (lambda () (evaluate-text text))) text)
      (void)))

  (edoc "Evaluate the expression before point, the one C-M-b would cross, in the M-x interaction environment and show its result; the C-x C-e of Emacs.")
  (define (eval-last-expression!)
    (let-values ([(start end) (expression:backward (head:current-buffer) (head:point))])
      (unless start (error 'eval:last-expression! "no expression before point"))
      (evaluate-span! start end "(eval:last-expression!)")))

  (edoc "Evaluate the top-level form around point, else the next one after it, in the M-x interaction environment and show its result; the C-M-x of Emacs.")
  (define (eval-top-level-form!)
    (let-values ([(start end) (expression:top-level (head:current-buffer) (head:point))])
      (unless start (error 'eval:top-level-form! "no top-level form in the buffer"))
      (evaluate-span! start end "(eval:top-level-form!)")))

  (define (spell value)
    ;; a pre-filled argument as the expression denoting it
    (if (symbol? value) (format "'~s" value) (format "~s" value)))

  (edoc "Open the M-x prompt with a call begun, the command's name and any arguments already given typed, so completion asks for the next: (eval:prompt-with! 'edit:answer!) reads (edit:answer! and a choice."
        (name symbol "the command's name at the top level")
        (arguments (list-of datum) "the arguments already given, spelled first")
        (prompts))
  (define (eval-prompt-with! name . arguments)
    (read-and-run! (string-append "(" (symbol->string name)
                                  (apply string-append (map (lambda (v) (string-append " " (spell v))) arguments))
                                  " ")))

  (define (read-and-run! initial)
    ;; Read an expression -- the prompt pretypes "(", deletable, so a
    ;; bare symbol evaluates too -- and evaluate it in the editor's
    ;; own top level.  The expression is logged (component eval, which
    ;; also carries the history); the result shows in the echo area,
    ;; transiently like any message, and lands in the log with it.
    (let ([s (parameterize ([prompt:ghost input-ghost]
                            [prompt:multiline indent-scheme-insertion]
                            [prompt:edge-motion mx-edge-motion]
                            [prompt:reindent reindent-scheme-input]
                            [paint:echo-highlight mx-echo-styles])
               (prompt:read! mx-label complete-symbol initial
                             (box (log:history 'eval car))
                             complete-editor-symbol normalize-input))])
      (when (and s (> (string-length s) 0) (not (string=? s "(")) (not (string=? s initial)))
        ;; Keep the prompt on screen while its expression evaluates --
        ;; forgiven parentheses included -- with the cursor parked at
        ;; its end, drawn as the evaluation-in-progress underline.
        ;; An indicator, not a record: the expression is already
        ;; logged under eval.
        (paint:show-prompt-message! mx-label s mx-echo-styles)
        (let ([outcome
               (parameterize ([paint:cursor-in-echo #t])
                 (paint:redraw!)
                 (call-with-evaluation! s (lambda () (evaluate-text s))))])
          ;; One structured record per exchange: history reads the query,
          ;; while the view and echo show the formatted pair.
          (report! outcome s)))))

  (edoc "Read an expression at the M-x prompt, with completion and hints, evaluate it in the editor top level and log the exchange; the result shows in the echo area."
        (prompts))
  (define (eval-prompt!)
    ;; the prompt pretypes "(", deletable, so a bare symbol evaluates too
    (read-and-run! "("))

  (edoc "Install the evaluation commands: their describe entries, the log formatter and the C-x C-e, C-M-x and M-x bindings.")
  (define (init!)
    (doc:register!
      '(((eval:run!) (("procedure" . "(eval:run!)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Evaluate every Scheme datum in `where` in the same interaction environment as M-x and show the last datum's result in the echo area. Non-void results are stored in the copy buffer when `eval-copy-result` is true. Standard output and error are logged per line under `stdout` and `stderr`, including child-process output. By default, evaluate the whole current buffer; `where` accepts the same buffer, name, region, predicate, and list forms as the editing commands.")
        ((eval:last-expression!) (("procedure" . "(eval:last-expression!)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Evaluate the expression before point, the one C-M-b would cross, in the M-x interaction environment and show its result in the echo area; C-x C-e.")
        ((eval:top-level-form!) (("procedure" . "(eval:top-level-form!)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Evaluate the top-level form around point, else the next one after it, in the M-x interaction environment and show its result in the echo area; C-M-x.")
        ((eval:prompt!) (("procedure" . "(eval:prompt!)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Prompt for a Scheme expression, evaluate it in the editor's interaction environment, and record the expression and result in the log. Non-void results are stored in the copy buffer when `eval-copy-result` is true. Standard output and error are logged per line under `stdout` and `stderr`, including child-process output.")))
    (log:register-formatter! 'eval format-exchange style-exchange)
    (keymap:bind-default! "C-x C-e" eval-last-expression!)
    (keymap:bind-default! "C-M-x" eval-top-level-form!)
    (keymap:bind-default! "M-x" eval-prompt!)
    ;; keys bound with keymap:prefill open this prompt with their text
    (dispatch:set-prompt-opener! eval-prompt-with!)))
