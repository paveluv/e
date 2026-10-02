;; eval.sls -- M-x: evaluate Scheme expressions, for the e editor.
;;
;; An e extension module: the library (eval), loaded at startup by the
;; kernel, which calls init!.  M-x prompts for an expression (the opening
;; parenthesis is pretyped and deletable, so a bare symbol works too;
;; missing closing parentheses are forgiven), evaluates it in the
;; editor's top level, logs the expression (eval:report!, which also
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
  (export call-with-evaluation! completion-candidates completion-extensions completion-hint completion-span
          (rename (evaluation:condition condition)) (rename (eval-copy-result copy-result))
          create-model-prompt! create-result-view!
          init! input-closers input-diagnostic (rename (eval-last-expression! last-expression!))
          (rename (eval-prompt! prompt!))
          (rename (eval-prompt-with! prompt-with!)) report! (rename (eval! run!)) settle-completion
          (rename (evaluation:status status)) (rename (eval-top-level-form! top-level-form!)) type-fits?
          (rename (evaluation:values values)))
  (import (chezscheme)
          (prefix (apps describe) describe:)
          (prefix (core evaluation) evaluation:)
          (prefix (core kernel) kernel:)
          (prefix (foundation edoc) edoc:)
          (prefix (foundation fuzzy) fuzzy:)
          (prefix (only (foundation scheme-format) indent-lines delimiter?) scheme-format:)
          (prefix (foundation string) string:)
          (prefix (foundation text) text:)
          (prefix (head completion) completion:)

          (prefix (head edit) edit:)
          (prefix (head expression) expression:)
          (prefix (head head) head:)
          (prefix (head interaction) interaction:)
          (prefix (head keymap) keymap:)
          (prefix (head layout) layout:)
          (prefix (head mode) mode:)
          (prefix (head namespace) namespace:)
          (prefix (head prompt) prompt:)
          (prefix (head routing) routing:)
          (prefix (head style) style:)
          (prefix (head text-control) text-control:)
          (prefix (head text-source) text-source:)
          (prefix (head widget) widget:)
          (prefix (service doc) doc:)
          (prefix (service log) log:)
          (prefix (service prompt-request) prompt-request:)
          (prefix (only (service reference) signatures) reference:)
          (prefix (state construction) construction:)
          (prefix (state model) model:)
          (prefix (state view) view:)
          (prefix (sys glyph) glyph:))

  (define (model-value id kind)
    (let ([r (model:snapshot id)])
      (and r (eq? (cdr (assq 'kind r)) kind) (cdr (assq 'value r)))))
  (define (model-completer id)
    (let ([reader (namespace:acquire! id)])
      (completion:make-source
        (lambda (text caret)
          (let ([range (symbol-range text caret)] [packet (namespace:snapshot reader)])
            (if (or (not range) (not (eq? (caddr packet) 'ready))) (values #f #f '() '())
              (let* ([part (substring text (car range) (cdr range))]
                     [matches (fuzzy:rank part (cadr packet))] [names (map fuzzy:name matches)])
                (values (car range) (cdr range) (lambda () (fuzzy:expansions part names)) names)))))
        #f "environment symbol"
        (lambda () (let ([p (namespace:snapshot reader)]) (list (car p) (caddr p))))
        (lambda () (namespace:release! reader)))))

  (edoc "Create an embedded Scheme prompt using an environment's actual symbol catalogue. The prompt borrows an authored draft; accepted/cancelled commands belong to its explicit host. Acceptance carries the captured environment and generation in its origin; submit with environment:evaluate!. No current window or head API namespace is used."
    (environment model "base environment") (generation integer "expected namespace generation")
    (draft buffer "borrowed authored Scheme text") (commands list "accepted/cancelled widget command targets")
    (returns model "unmounted prompt view") (public))
  (define (create-model-prompt! environment generation draft commands)
    (let* ([origin (list (cons 'environment environment) (cons 'generation generation))]
           [request (prompt-request:create! head:ui-actor #f draft "" origin (list 'environment 1 environment))])
      (guard (ex [else (prompt-request:close! head:ui-actor request) (raise ex)])
        (prompt:create! request
          '((label . "Scheme:") (multiline? . #t) (profile model-scheme 1 ())
            (editing-policy scheme-input 1) (mode . "scheme")) commands))))

  (define (result-text id source inputs)
    (let* ([v (cdr (assq 'value source))] [status (cdr (assq 'status v))]
           [value (cdr (assq 'result v))] [diagnostic (cdr (assq 'diagnostic v))])
      (cond [value (string-append (if (eq? (car value) 'expired) "[Expired result] " "=> ")
                     (car (reverse value)))]
        [diagnostic (cdr (assq 'message diagnostic))]
        [else (format "[~a]" status)])))
  (define (render-result text d width height range)
    (let* ([lines (string:lines text)] [start (min (car range) (length lines))])
      (map (lambda (line) (glyph:fit line width)) (list-head (list-tail lines start) (min (cdr range) (- (length lines) start))))))

  (edoc "Compose a job's shared output editor and bounded result/diagnostic summary. Views borrow the job; splitting, unmounting and resizing never execute code, allocate workers or retain another result handle. Release the job explicitly through environment:release!."
    (actor actor "view creator") (owner (or model #f) "lifetime owner, false for a session root") (job model "base evaluation job") (returns model "unmounted result composition") (public))
  (define (create-result-view! actor owner job)
    (construction:call! actor
      (lambda (remember!)
        (let* ([r (caddar (cadr (model:snapshots (list job))))]
               [v (and r (eq? (cdr (assq 'kind r)) 'evaluation-job) (cdr (assq 'value r)))])
          (unless v (error 'create-result-view! "evaluation job is unavailable" job))
          (let* ([root (remember! (view:create! actor job 'evaluation-result 1 '() '() owner))]
                 [output (edit:create-view! actor (cdr (assq 'output v)) '((read-only . #t)) root)]
                 [summary (remember! (view:create! actor job 'evaluation-summary 1 '() '() root))])
            (view:arrange! actor (list (list root 0 (list (list 'output output '(grow 1)) (list 'result summary 'fit)) '())) '()) root)))))

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
                [(and (eq? kind 'quote) (eq? value 'datum-comment)) (read in) (scan)]
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
    ;; Let the lexer distinguish strings from quotes in comments/characters.
    (guard (ex [else #f])
      (let ([in (open-input-string (string-append (substring s 0 pos) "\""))])
        (let loop ()
          (let-values ([(kind value from to) (read-token in)])
            (cond [(eq? kind 'eof) #f]
              [(and (eq? kind 'quote) (eq? value 'datum-comment)) (read in) (loop)]
              [(and (eq? kind 'atomic) (string? value) (= to (+ pos 1))) from]
              [else (loop)]))))))

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
    (edoc:call-argument-type (operator-signatures sym) index))

  (define (element-type type)
    (cond [(and (pair? type) (eq? (car type) 'list-of)) (cadr type)]
      [(and (pair? type) (eq? (car type) 'or)) (exists element-type (cdr type))]
      [else #f]))

  (define (argument-context s pos)
    ;; (type start end token where) for the cursor at a documented argument
    ;; position: the argument's type, the range and text of the token being
    ;; completed, and where it sits: #t inside a string literal, else #f.
    ;; At the operator position
    ;; of a nested form, (store:buffer-name (bu, the token is the form's
    ;; opening and the type is the enclosing argument's: whatever the form
    ;; produces has to serve it. Quoted data offers only values, recursively
    ;; using list element types; quoted replaces the value's own outer quote,
    ;; data inserts inside a surrounding quotation. #f without a type.
    (define (typed frame start end token where)
      (let ([type (argument-type (string->symbol (frame-operator frame)) (frame-arguments frame))])
        (and type (list type start end token where))))
    (define (plain? frame) (and (not (frame-quoted? frame)) (string? (frame-operator frame))))
    (let* ([quote-at (open-string-start s pos)]
           [range (and (not quote-at) (symbol-range s pos))]
           [start (cond [quote-at (+ quote-at 1)] [range (car range)] [else pos])]
           [end (cond [quote-at pos] [range (cdr range)] [else pos])]
           [token (let ([raw (substring s start end)]) (if quote-at (string-content raw) raw))]
           [frames (call-frames (substring s 0 (if quote-at quote-at start)))])
      (and (or quote-at (and range (< (car range) (cdr range))) (open-position? s pos)) (pair? frames)
           (let* ([frame (car frames)]
                  ;; A partially inserted reference is still one value, not
                  ;; an application of its tag. Include its quote/container
                  ;; when replacing it, even after normalization added spaces.
                  [whole (and (data-position? frames) (find (lambda (f)
                                                              (and (frame-expected f) (not (element-type (frame-expected f)))
                                                                (or (frame-quoted? f) (quotation-frame? f))))
                                                        (let loop ([rest frames] [out '()])
                                                          (if (or (null? rest)
                                                                (and (quotation-frame? (car rest)) (eqv? (next-mode rest) 0))) out
                                                            (loop (cdr rest) (cons (car rest) out))))))])
             (cond
               [whole
                (let* ([from (frame-start whole)]
                       [through (guard (ex [else end])
                                  (let ([in (open-input-string (substring s from (string-length s)))])
                                    (read in) (max end (+ from (file-position in)))))])
                  (list (frame-expected whole) from through (substring s from end)
                    (if (eqv? (frame-mode whole) 0) 'quoted 'data)))]
               [(data-position? frames)
                (let ([type (next-type frames)])
                  (and type (list type start end token (if quote-at #t 'data))))]
               [(quotation-frame? frame)
                (let ([type (next-type frames)])
                  (and type (list type start end token (and quote-at #t))))]
               [(plain? frame) (typed frame start end token (and quote-at #t))]
               [(and (eq? (frame-operator frame) 'pending) (not (frame-quoted? frame)) (not quote-at)
                     (pair? (cdr frames)) (plain? (cadr frames))
                     (> start 0) (char=? (string-ref s (- start 1)) (frame-opener frame)))
                (typed (cadr frames) (- start 1) end (substring s (- start 1) end) #f)]
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
  (define-record-type option (fields text insert label hint face value named?))

  (define (opening spelling)
    (let loop ([end (string-length spelling)])
      (if (and (> end 1) (memv (string-ref spelling (- end 1)) '(#\) #\] #\")))
          (loop (- end 1))
          (substring spelling 0 end))))

  (define (unquoted text)
    ;; Source punctuation is not a search key. Explicit quote and reader
    ;; abbreviation normalize to the same datum prefix.
    (cond [(string:prefix? "'" text) (substring text 1 (string-length text))]
      [(string:prefix? "(quote " text) (substring text 7 (string-length text))]
      [else text]))

  (define (value-options type token in-string?)
    ;; Inside a string, complete its contents. Other positions use the
    ;; value's ordinary Scheme spelling, with quotation only in expressions.
    (fold-right
      (lambda (entry out)
        (let* ([value (car entry)] [label (cadr entry)] [hint (or (caddr entry) "")]
               [spelling (edoc:type-spelling type value)]
               [named? (and label (not (eq? in-string? #t))
                            (not (string:prefix? "(" token)))])
          (cond
            [(and (eq? in-string? 'quoted) (not (edoc:value-expression value))) out]
            [named?
             ;; Search readable labels; insertion remains the value's ordinary
             ;; Scheme expression. Explicit datum prefixes search that spelling.
             (let ([insert (if (eq? in-string? 'data) (format "~s" value) spelling)])
               (cons (make-option label insert label
                       (string-append spelling (if (string=? hint "") "" (string-append "  " hint))) 'plain value #t) out))]
            [(eq? in-string? #t)
             ;; the literal stays open: the settle step closes it at the
             ;; session's dead end, once nothing more completes from the value
             (if (string? value) (cons (make-option value value value hint 'plain value #f) out) out)]
            [(eq? in-string? 'data)
             (if (edoc:value-expression value)
               (let ([text (format "~s" value)])
                 (cons (make-option (opening text) text text hint 'plain value #f) out)) out)]
            [else (let ([text (edoc:type-spelling type value)])
                    (cons (make-option (opening (unquoted text)) text text hint 'plain value #f) out))])))
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
                  (cons (make-option text insert label (edoc:signature-summary sig) 'editor #f #f) out)))))
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
                            (if (procedure? value) (completion-hint sym) (edoc:type-spelling type value)) 'editor #f #f)
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
    ;; The string contents after its opening quote.
    (let find ([i 0])
      (cond [(>= i (string-length text)) #f]
            [(char=? (string-ref text i) #\") (substring text (+ i 1) (string-length text))]
            [else (find (+ i 1))])))

  (define (value-part text)
    ;; The value after the opening quote of a string
    ;; spelling; a bare value is all of it,
    ;; a quote within a name notwithstanding
    (if (and (> (string-length text) 0) (memv (string-ref text 0) '(#\( #\")))
        (or (quoted-part text) text)
        text))

  (define (typed-options context)
    ;; ((option . fragments) ...) for an argument context, best first, or #f
    ;; when the type offers nothing the token matches: the token aligns with
    ;; a candidate's text as it would with a symbol, so (bu matches both
    ;; quoted buffer references and their producers; formals are not keys.
    (let* ([type (car context)] [in-string? (car (cddddr context))]
           [token (if (eq? in-string? #t) (cadddr context) (unquoted (cadddr context)))]
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
    (let* ([type (car context)] [start (cadr context)] [end (caddr context)] [in-string? (car (cddddr context))]
           [token (if (eq? in-string? #t) (cadddr context) (unquoted (cadddr context)))])
      (define quote-query?
        (or (eq? in-string? 'quoted)
          (and (not (eq? in-string? 'data))
            (for-all (lambda (entry) (string:prefix? "'" (option-insert (car entry)))) options))))
      (define (query-insert text) (if quote-query? (string-append "'" text) text))
      (define (typed-still? text)
        (let* ([text (query-insert text)]
               [next (argument-context (string-append (substring s 0 start) text (substring s end (string-length s)))
                       (+ start (string-length text)))])
          ;; Normalization must not step into a constructor's argument and
          ;; change a portable value's candidate domain without user input.
          (and next (or (not (edoc:type-portable? type)) (equal? type (car next)))
            ;; A label resembling Scheme data must not normalize into the
            ;; other search domain. The user can enter a datum prefix explicitly.
            (not (and (exists (lambda (entry) (option-named? (car entry))) options)
                   (string:prefix? "(" (unquoted text)))))))
      (define (sole-insert option)
        ;; a sole value whole: a string value still completing on, a
        ;; directory say, open to continue into, else closed at its dead end
        (let ([value (option-value option)])
          (if (and (string? value) (not (eq? in-string? #t)) (not (dead-end? type value)))
              (option-text option)
              (option-insert option))))
      (define (string-common)
        ;; the openings' common prefix when its value part extends the
        ;; token, "/ over the root's entries say, else #f
        (let* ([common (string:common-prefix (map (lambda (entry) (option-text (car entry))) options))]
               [value-part (quoted-part common)])
          (and value-part (string:prefix? token value-part) common)))
      (define (bare-inserts)
        ;; the inserts as the candidates' texts spell them
        (cond
          [(null? (cdr options)) (list (sole-insert (car (car options))))]
          [(eq? in-string? #t)
           (let ([common (string:common-prefix (map (lambda (entry) (option-text (car entry))) options))])
             (list (if (and (> (string-length common) (string-length token)) (string:prefix? token common)) common token)))]
          ;; a bare token the values alone match, / over the root's entries
          ;; say, opens a string; with a symbol among
          ;; the matches it extends as symbols do
          [(and (for-all (lambda (entry) (option-value (car entry))) options) (string-common)) => list]
          [else
           (let ([seen (make-hashtable string-hash string=?)])
             (map query-insert
               (fuzzy:expansions token
                                 (fold-right (lambda (entry out)
                                               (let ([text (option-text (car entry))])
                                                 (if (hashtable-ref seen text #f) out
                                                   (begin (hashtable-set! seen text #t) (cons text out)))))
                                   '() options)
                                 typed-still?)))]))
      ;; inside a string the inserts are what the literal holds, a quote or a
      ;; backslash in a name escaped, as the token was read
      (let ([inserts (bare-inserts)])
        (if (eq? in-string? #t) (map string-escaped inserts) inserts))))

  (define (typed-candidate type entry preview)
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
          (let ([offset (if (and (not (option-named? option)) (string:prefix? "'" label)) 1 0)])
            (style:fill-range! styles (+ offset (cadr fragment)) (+ offset (cadr fragment) (caddr fragment)) (list face 'mark))))
        fragments)
      (completion:make-candidate (option-insert option) text styles (vector label hint)
        (and (option-value option)
          (append (list (cons 'type type) (cons 'value (option-value option)) '(literal? . #t)) preview)))))

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

  (edoc "The span Tab replaces at a typed argument position, (start . end) character offsets, or #f: the token or the contents of an open string."
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
    (refresh-signatures!)
    (let* ([bound? (top-level-bound? sym)]
           [value (and bound? (top-level-value sym))]
           [key (if (procedure? value) value sym)])
      (or (eq-hashtable-ref hint-cache key #f)
          (let* ([signatures (or (and bound? (edoc:edoc-of value)) (named-signatures sym))]
                 [sig (and signatures (car signatures))]
                 [arguments
                  (cond
                    [(not sig)
                     ;; a procedure without an edoc: its describe entry's
                     ;; parameters, the corpus's or a module's, else its own
                     (let ([tokens (or (and (procedure? value) (described-params sym))
                                       (and (procedure? value) (guard (ex [else #f]) (local-params value))))])
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
      (completion:make-candidate name label styles)))

  (define (receiver-matches declaration receivers)
    (filter (lambda (r) (and (eq? (caadr declaration) (if (eq? (car (list-ref r 3)) 'source) 'model 'view))
                          (memq (cadr r) (cdadr declaration)) (widget:receiver-live? r))) receivers))

  (define (receiver-at sym index)
    (exists
      (lambda (sig)
        (let ([r (edoc:signature-receiver sig)])
          (and r (let loop ([args (edoc:signature-formals sig)] [i index])
                   (and (pair? args) (if (zero? i) (and (eq? (car r) (car args)) r)
                                         (loop (cdr args) (- i 1))))))))
      (receiver-signatures sym)))

  (define receiver-cache (make-weak-eq-hashtable))
  (define (receiver-signatures sym)
    (let ([proc (and (top-level-bound? sym) (top-level-value sym))])
      (if (not (procedure? proc)) '()
        (or (hashtable-ref receiver-cache proc #f)
          (let ([sigs (filter (lambda (s) (edoc:signature-receiver s)) (or (edoc:edoc-of proc) '()))])
            (hashtable-set! receiver-cache proc sigs) sigs)))))

  (define (empty-receiver text pos receivers)
    ;; Only whitespace at a fresh argument slot qualifies. An existing token,
    ;; nested expression, literal or variable keeps ordinary typed completion.
    (let ([frames (call-frames (substring text 0 pos))])
      (and (> pos 0) (char-whitespace? (string-ref text (- pos 1))) (pair? frames)
        (not (frame-quoted? (car frames))) (string? (frame-operator (car frames)))
        (or (= pos (string-length text)) (memv (string-ref text pos) '(#\) #\] #\})))
        (let ([r (receiver-at (string->symbol (frame-operator (car frames))) (frame-arguments (car frames)))])
          (and r (receiver-matches r receivers))))))

  (define (current-receivers)
    (let ([focus (widget:focused)]) (if focus (widget:receivers focus) '())))

  (define (validate-receivers! text receivers)
    (define (walk form)
      (when (and (pair? form) (list? form) (not (memq (car form) '(quote quasiquote))))
        (when (symbol? (car form))
          (for-each
            (lambda (arg i)
              (when (receiver-at (car form) i)
                (let* ([value (and (list? arg) (= (length arg) 2) (eq? (car arg) 'quote) (cadr arg))]
                       [r (and value (assoc value receivers))])
                  (when (and r (not (widget:receiver-live? r)))
                    (raise (condition (kernel:make-refusal) (make-message-condition "The command's captured receiver is no longer available")))))))
            (cdr form) (iota (length (cdr form)))))
        (for-each walk form)))
    (let ([p (open-input-string (string-append text (or (input-closers text) "")))])
      (let loop ([form (read p)]) (unless (eof-object? form) (walk form) (loop (read p))))))

  (define (symbol-completer keep? typed? origin)
    ;; The status line describes the last lookup. It must not query a type's
    ;; live directory again while painting (some directories live at the base).
    (define kind (if typed? "symbol" "editor symbol"))
    (define preview (origin-preview origin))
    (define receivers (cond [(assq 'receivers origin) => cdr] [else '()]))
    (define (eligible? sym)
      (and (keep? sym)
        (let ([declared (map edoc:signature-receiver (receiver-signatures sym))])
          (or (null? declared) (exists (lambda (d) (pair? (receiver-matches d receivers))) declared)))))
    (completion-at-editor (completion:make-source
                            (lambda (s pos)
                              (define (symbols)
                                (let ([range (symbol-range s pos)])
                                  (if (or (not range) (data-position? (call-frames (substring s 0 (car range))))
                                          (and (= (car range) (cdr range)) (not (open-position? s pos)))) (values #f #f '() '())
                                      (let* ([part (substring s (car range) (cdr range))]
                                             ;; Symbols go in as they are: the matcher keeps each one
                                             ;; prepared across keystrokes.
                                             [ranked (fuzzy:rank part (filter eligible? (environment-symbols (interaction-environment))))]
                                             [names (map fuzzy:name ranked)])
                                        (values (car range) (cdr range) (lambda () (fuzzy:expansions part names))
                                                (map completion-candidate ranked))))))
                              ;; an argument with a documented type offers its own candidates; a
                              ;; sole one is what Tab inserts, else Tab extends the token as far as
                              ;; every candidate allows and lists them
                              (let* ([targets (and typed? (empty-receiver s pos receivers))]
                                     [context (and typed? (argument-context s pos))]
                                     [options (and (not (pair? targets)) context (typed-options context))])
                                (set! kind (if options (type-text (car context)) (if typed? "symbol" "editor symbol")))
                                (cond [(pair? targets)
                                       (set! kind "receiver")
                                       (let ([literals (map (lambda (r) (edoc:value-expression (car r))) targets)])
                                         (values pos pos (if (null? (cdr literals)) literals '(""))
                                                 (map (lambda (r text)
                                                        (completion:make-candidate text (format "~a  ~a" (list-ref r 5) text) #f)) targets literals)))]
                                      [(not options) (symbols)]
                                      [else
                                       (values (cadr context) (caddr context)
                                               (lambda () (typed-inserts s context options))
                                               (map (lambda (o) (typed-candidate (car context) o preview)) options))])))
                            settle-completion
                            ;; what the list holds, for its status line: the argument's type at a
                            ;; typed position, else the symbols offered
                            (lambda (s pos) kind)
                            (lambda () #f) (lambda () (values))
                            (lambda (s pos)
                              (let ([context (and typed? (argument-context s pos))])
                                (if (not context) '()
                                    (append (list (cons 'type (car context)) (cons 'token (cadddr context))
                                                  (cons 'literal? (car (cddddr context)))
                                                  ;; Numbers outside strings denote themselves;
                                                  ;; a preview never evaluates an expression.
                                                  (cons 'value (and (not (eq? (car (cddddr context)) #t))
                                                                    (string->number (unquoted (cadddr context)))))) preview)))))
      (cond [(assq 'editor preview) => cdr] [else #f])))

  (define (completion-at-editor source editor)
    ;; Typed providers see the same captured receiver as their preview, even
    ;; while focus is inside the prompt. This scopes queries, never execution.
    (define (scope proc)
      (if (not (procedure? proc)) proc
        (lambda args (parameterize ([widget:target editor]) (apply proc args)))))
    (completion:make-source (scope (completion:source-lookup source))
      (scope (completion:source-settle source)) (completion:source-kind source)
      (completion:source-basis source) (completion:source-release source)
      (completion:source-context source)))

  (define (origin-preview origin)
    (let find ([id (cond [(assq 'view origin) => cdr] [else #f])])
      (let ([d (and id (interaction:snapshot id))])
        (cond [(and d (eq? (view:kind d) 'editor))
               (list (cons 'editor id) (cons 'document (view:source d)))]
          [(and d (view:parent d)) (find (view:parent d))]
          [else '()]))))

  (define (type-text type)
    ;; a type as the status line names it: a name as itself, a record type
    ;; by its record, a compound as written
    (cond [(symbol? type) (symbol->string type)]
          [(and (pair? type) (eq? (car type) 'record) (pair? (cdr type))) (symbol->string (cadr type))]
          [else (format "~s" type)]))

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

  (define signature-table #f) ; (source . index): the base's signatures last seen, indexed by name

  (define (refresh-signatures!)
    ;; the base's signatures as the hints know them: a new version, after a
    ;; fetch say, is indexed anew and every hint computed against the old
    ;; one is forgotten, so the cache is checked only after this
    (let ([source (guard (ex [else '()]) (reference:signatures))])
      (unless (and signature-table (eq? (car signature-table) source))
        (let ([index (make-eq-hashtable)])
          (for-each (lambda (entry) (eq-hashtable-set! index (car entry) (cdr entry))) source)
          (set! signature-table (cons source index))
          (hashtable-clear! hint-cache)))))

  (edoc "The documented procedure forms recorded for a name, as text: the base's corpus and registered modules come in one piece, indexed once per version."
        (sym symbol "the name")
        (returns (list-of string))
        (effects internal))
  (define (described-forms sym)
    (refresh-signatures!)
    (eq-hashtable-ref (cdr signature-table) sym '()))

  (define (described-params sym)
    ;; Pick the longest documented procedure form for this name.
    (guard (ex [else #f])
      (let ([best #f])
        (for-each
          (lambda (text)
            (let ([sig (guard (ex [else #f]) (with-input-from-string text read))])
              (when (and (pair? sig) (eq? (car sig) sym)
                         (or (not best) (> (signature-arity sig) (signature-arity best))))
                (set! best sig))))
          (described-forms sym))
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

  ;; The Scheme lexer handles comments, escapes and vector openers. Synthetic
  ;; frames for reader abbreviations disappear after their single datum.
  ;; Mode is a quasiquote depth, or literal under an ordinary quote.
  (define-record-type frame (fields opener operator arguments mode start expected))
  (define closers '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))

  (define (frame-quoted? f)
    (or (not (frame-opener f)) (not (eqv? (frame-mode f) 0))))

  (define (quotation-frame? f)
    (and (not (eq? (frame-mode f) 'literal)) (= (frame-arguments f) 0)
      (member (frame-operator f) '("quote" "quasiquote" "unquote" "unquote-splicing"))))

  (define (next-mode stack)
    (if (null? stack) 0
      (let* ([f (car stack)] [mode (frame-mode f)])
        (cond [(eq? mode 'literal) mode]
          [(not (quotation-frame? f)) mode]
          [(equal? (frame-operator f) "quasiquote") (+ mode 1)]
          [(member (frame-operator f) '("unquote" "unquote-splicing")) (max 0 (- mode 1))]
          [(= mode 0) 'literal]
          [else mode]))))

  (define (data-position? stack) (not (eqv? (next-mode stack) 0)))

  (define (next-type stack)
    (and (pair? stack)
      (let ([f (car stack)])
        (cond [(quotation-frame? f) (frame-expected f)]
          [(data-position? stack) (element-type (frame-expected f))]
          [(string? (frame-operator f))
           (argument-type (string->symbol (frame-operator f)) (frame-arguments f))]
          [else #f]))))

  (define (call-frames text)
    (let ([in (open-input-string text)])
      (let loop ([stack '()])
        ;; Incomplete strings/escaped identifiers leave the preceding context.
        (guard (ex [else stack])
          (let-values ([(kind value from to) (read-token in)])
            (case kind
              [(eof) stack]
              [(quote)
               (if (eq? value 'datum-comment)
                 (begin (read in) (loop stack))
                 (loop (cons (make-frame #f (symbol->string value) 0 (next-mode stack) from (next-type stack)) stack)))]
              [(lparen lbrack vparen vu8paren)
               (loop (cons (make-frame (if (eq? kind 'lbrack) #\[ #\() 'pending 0
                             (if (and (memq kind '(vparen vu8paren)) (eqv? (next-mode stack) 0)) 'literal (next-mode stack))
                             from (next-type stack)) stack))]
              [(rparen rbrack)
               (loop (if (pair? stack) (count-datum (cdr stack) #f) stack))]
              [else
               (cond [(and (= (- to from) 1) (char=? (string-ref text from) #\{))
                      (loop (cons (make-frame #\{ 'pending 0 (next-mode stack) from (next-type stack)) stack))]
                 [(and (= (- to from) 1) (char=? (string-ref text from) #\}))
                  (loop (if (pair? stack) (count-datum (cdr stack) #f) stack))]
                 [else (loop (count-datum stack (and (eq? kind 'atomic) (symbol? value) (symbol->string value))))])]))))))

  (define (count-datum frames token)
    ;; A completed datum fills the innermost form's operator slot, else is one
    ;; more argument of it.
    (if (null? frames)
        frames
        (let ([f (car frames)])
          (if (not (frame-opener f)) (count-datum (cdr frames) #f)
            (cons (make-frame (frame-opener f)
                    (if (eq? (frame-operator f) 'pending) token (frame-operator f))
                    (if (eq? (frame-operator f) 'pending) 0 (+ (frame-arguments f) 1))
                    (frame-mode f) (frame-start f) (frame-expected f)) (cdr frames))))))

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
         (if (and context (car (cddddr context))
                  (dead-end? (car context) (cadddr context)))
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
    ;; Strings are M-x input; symbols label an extension's evaluation.
    (format "~a => ~a" (car d) (cdr d)))

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
    ;; Scheme highlighting over the exchange, echo and *log* alike -- the
    ;; editor's own names in the editor style: eval runs in the editor's
    ;; environment, whatever a random file does.  A failed evaluation's
    ;; message is prose, not Scheme: plain red after the arrow.
    (let* ([scheme (mode:find "scheme")]
           [styles (if scheme
                       (editorize! text ((mode:styles scheme) text))
                       (make-vector (string-length text) 'plain))]
           [failed (string:search text " => error: " 0 (string-length text))])
      (when failed
        (style:fill-range! styles (+ failed 4) (vector-length styles) 'error))
      styles))

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
                      (lambda () (kernel:evaluate! form (interaction-environment)))
                      list)))))))

  (edoc "Run a thunk with C-g interruption, streamed output logging and one undo group per uninterrupted command segment. Prompt suspension releases capture and closes the segment; resumption starts a fresh one. Return values or the original condition without reporting. Nested calls share capture and grouping. Runs on the head's main thread."
        (label string "the undo label")
        (thunk thunk "the computation, returning ordinary Scheme values")
        (returns (record evaluation)))
  (define (call-with-evaluation! label thunk)
    (evaluation:call!
      (lambda () (text-source:call-segmented! head:ui-actor label thunk))
      (lambda (channel line)
        (log:add! 'eval:call-with-evaluation! (cons channel line) (not (eq? channel 'compile))))
      head:call-with-interrupt head:interrupted?))

  (edoc "Report an evaluation under eval:report!, copying non-void values when copy-result is enabled and preserving command feedback. The datum is (destination . result): a symbol labels an extension's result; a string records M-x input and participates in its history."
        (outcome (record evaluation) "the execution result")
        (destination (or symbol string) "an extension label, or the M-x input to record as an exchange"))
  (define (report! outcome destination)
    (unless (or (symbol? destination) (string? destination))
      (error 'eval:report! "expected an extension label or the M-x input" destination))
    (let* ([failed? (not (eq? (evaluation:status outcome) 'ok))]
           [vals (evaluation:values outcome)]
           [void? (and (not failed?)
                       (or (null? vals)
                         (and (null? (cdr vals))
                              (eq? (car vals) (void)))))]
           [expressions (and (not failed?) (map edoc:value-expression vals))]
           [expression (and expressions (for-all string? expressions)
                         (if (= (length expressions) 1) (car expressions)
                           (string-append "(values" (apply string-append (map (lambda (s) (string-append " " s)) expressions)) ")")))]
           [result (if failed?
                       (if (eq? (evaluation:status outcome) 'interrupted) "interrupted"
                         (format "error: ~a" (kernel:condition-text (evaluation:condition outcome))))
                       (or expression (string:join (map (lambda (v) (format "~s" v)) vals) ", ")))]
          )
      (let* ([copied? (and (eval-copy-result) expression (not void?))]
             [result-record
              (log:add! 'eval:report! (cons destination (if void? "#<void>" result)) #f)])
        (when copied? (edit:copy-text! result))
        (edit:present-log-entries!
          (list result-record)
          (if copied? " [copied]" "")))))

  (define (evaluate-editor! id selector label)
    (let-values ([(source d) (text-control:context id 'editor)])
      ;; Resolve text and positions against the same retained revision before
      ;; evaluation can edit it, change focus or suspend in another prompt.
      (let* ([lines (text-control:basis-text source d)] [state (view:state d)])
        (let-values ([(start end) (selector lines state)])
          (unless start (error 'eval "no expression at the editor's point" id))
          (parameterize ([widget:target id])
            (evaluate-text! (expression:text lines start end) label))))))

  (define (evaluate-text! text label)
    (report! (call-with-evaluation! label (lambda () (evaluate-text text))) text)
    (void))

  (edoc "Evaluate the selected region, or the whole explicit editor when no mark is active, in the head's M-x interaction environment. Read the text at the selection's revision and report through its composition."
        (id model "source editor") (receiver id (view editor)) (public))
  (define (eval! id)
    (evaluate-editor! id
      (lambda (lines state)
        (if (list-ref state 3)
            (if (text:position<=? (car state) (cadr state))
                (values (car state) (cadr state))
                (values (cadr state) (car state)))
            (let ([row (- (vector-length lines) 1)])
              (values '(0 . 0) (cons row (string-length (vector-ref lines row)))))))
      "(eval:run!)"))

  (edoc "Evaluate the expression before an explicit editor's point, the one C-M-b would cross, in the head's M-x interaction environment and report its result; the C-x C-e of Emacs."
        (id model "source editor") (receiver id (view editor)))
  (define (eval-last-expression! id)
    (evaluate-editor! id (lambda (lines state) (expression:backward lines (car state))) "(eval:last-expression!)"))

  (edoc "Evaluate the top-level form around an explicit editor's point, or the next one after it, in the head's M-x interaction environment and report its result; the C-M-x of Emacs."
        (id model "source editor") (receiver id (view editor)))
  (define (eval-top-level-form! id)
    (evaluate-editor! id (lambda (lines state) (expression:top-level lines (car state))) "(eval:top-level-form!)"))

  (edoc "Open the M-x prompt with a call begun, the command's name and any arguments already given typed, so completion asks for the next: (eval:prompt-with! 'edit:answer!) reads (edit:answer! and a choice."
        (name symbol "the command's name at the top level")
        (arguments (list-of datum) "the arguments already given, spelled first")
        (prompts))
  (define (eval-prompt-with! name . arguments)
    (read-and-run! (string-append "(" (symbol->string name)
                                  (apply string-append (map (lambda (v i) (string-append " " (edoc:type-spelling (argument-type name i) v)))
                                                         arguments (iota (length arguments))))
                                  " ")))

  (define (read-and-run! initial)
    ;; Read an expression -- the prompt pretypes "(", deletable, so a
    ;; bare symbol evaluates too -- and evaluate it in the editor's
    ;; own top level.  The expression is logged (eval:report!, which
    ;; also carries the history); the result shows in the echo area,
    ;; transiently like any message, and lands in the log with it.
    (let* ([focus (widget:focused)] [receivers (current-receivers)]
           [origin (and focus (find (lambda (r) (equal? (car r) focus)) receivers))]
           [s (prompt:read! "λ" initial '(scheme 1 ())
                '((multiline? . #t) (profile scheme 1 ()) (editing-policy scheme-input 1) (mode . "scheme-prompt")))])
      (unless s ((routing:feedback) "Quit"))
      (when (and s (> (string-length s) 0) (not (string=? s "(")) (not (string=? s initial)))
        (unless (and origin (widget:receiver-live? origin))
          (raise (condition (kernel:make-refusal) (make-message-condition "The command's origin is no longer displayed"))))
        (validate-receivers! s receivers)
        ((routing:feedback) (string-append "λ " s))
        (head:redraw!)
        (parameterize ([widget:target focus])
          ;; One structured record per exchange: history reads the query,
          ;; while the view and echo show the formatted pair.
          (report! (call-with-evaluation! s (lambda () (evaluate-text s))) s)))))

  (edoc "Read an expression at the M-x prompt, with completion and hints, evaluate it in the editor top level and log the exchange; the result shows in the echo area."
        (prompts))
  (define (eval-prompt!)
    ;; the prompt pretypes "(", deletable, so a bare symbol evaluates too
    (read-and-run! "("))

  (define (input-offset text p)
    (+ (cdr p) (fold-left + 0 (map (lambda (line) (+ 1 (string-length line))) (list-head (string:lines text) (car p))))))
  (define (input-position text offset)
    (let ([lines (string:lines (substring text 0 offset))]) (cons (- (length lines) 1) (string-length (car (reverse lines))))))

  (edoc "Install the evaluation commands: their describe entries, the log formatter and the C-x C-e, C-M-x and M-x bindings." (public))
  (define (init!)
    (completion:register! 'environment 1 (lambda (id origin) (model-completer id)))
    (prompt:register-profile! 'model-scheme 1
      (lambda (configuration origin)
        (let ([id (cdr (assq 'environment origin))] [generation (cdr (assq 'generation origin))])
          (list (cons 'normalize normalize-input) (cons 'transform reindent-scheme-input) (cons 'edge mx-edge-motion)
            (cons 'ghost (lambda (text caret) (input-diagnostic (substring text 0 caret))))
            (cons 'validate
              (lambda (text)
                (let ([v (model-value id 'environment)])
                  (and (not (and v (= generation (cdr (assq 'generation v)))))
                    "Environment changed; open a new prompt"))))))))
    (widget:register! 'evaluation-summary 1
      (list (cons 'prepare result-text) (cons 'render render-result)
        (cons 'measure (lambda (text d axis cross child) (if (eq? axis 'y) (let ([n (length (string:lines text))]) (list n n)) '(0 0))))))
    (widget:register! 'evaluation-result 1 (append (layout:container 'y) '((source-receiver . job))))
    (mode:register! "scheme-prompt" '() '()
      (lambda (text) (let ([scheme (mode:find "scheme")]) (and scheme (editorize! text ((mode:styles scheme) text))))) #f)
    (edit:register-policy! 'scheme-input 1
      (lambda (lines positions)
        (let* ([text (string:join (vector->list lines) "\n")]
               [results (map (lambda (p) (reindent-scheme-input text (input-offset text p))) positions)]
               [text (caar results)])
          (values (list->vector (string:lines text)) (map (lambda (r) (input-position text (cdr r))) results)))))
    (completion:register! 'scheme 1
      (lambda (configuration origin) (symbol-completer (lambda (sym) #t) #t origin)))
    (prompt:register-profile! 'scheme 1
      (lambda (configuration origin)
        (list (cons 'history (log:history 'eval:report! car)) (cons 'normalize normalize-input)
          (cons 'ghost (lambda (text caret) (input-ghost (substring text 0 caret))))
          (cons 'transform reindent-scheme-input) (cons 'edge mx-edge-motion) (cons 'inspect describe:input!)
          (cons 'alternate (symbol-completer kernel:editor-symbol? #f origin)))))
    (doc:register!
      '(((eval:run!) (("procedure" . "(eval:run! editor)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Evaluate the selected region of the explicit editor, or its whole document when no mark is active, in the head's M-x interaction environment. Non-void results are stored in the copy buffer when `eval:copy-result` is true. Output is logged per line under `eval:call-with-evaluation!`, including child-process output. Text is captured at the selection's revision before execution.")
        ((eval:last-expression!) (("procedure" . "(eval:last-expression! editor)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Evaluate the expression before point, the one C-M-b would cross, in the M-x interaction environment and show its result in the echo area; C-x C-e.")
        ((eval:top-level-form!) (("procedure" . "(eval:top-level-form! editor)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Evaluate the top-level form around point, else the next one after it, in the M-x interaction environment and show its result in the echo area; C-M-x.")
        ((eval:prompt!) (("procedure" . "(eval:prompt!)")) "void"
         ("(apps eval)") eval "Evaluation commands" #f
         "Prompt for a Scheme expression, evaluate it in the editor's interaction environment, and record the expression and result under `eval:report!`. Non-void results are stored in the copy buffer when `eval-copy-result` is true. Output is logged per line under `eval:call-with-evaluation!`, with stdout/stderr channels, including child-process output.")))
    (log:register-formatter! 'eval:report! format-exchange style-exchange)
    (log:register-formatter! 'eval:call-with-evaluation!
      (lambda (d) (format "[~a] ~a" (car d) (cdr d))))
    (keymap:bind-default! 'widget-editor "C-x C-e" (keymap:call eval-last-expression! widget:target))
    (keymap:bind-default! 'widget-editor "C-M-x" (keymap:call eval-top-level-form! widget:target))
    (keymap:bind-default! "M-x" eval-prompt!)
    ;; keys bound with keymap:prefill open this prompt with their text
    (routing:set-prompt-opener! eval-prompt-with!)))
