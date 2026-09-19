;; edoc.sls -- documented definitions: the library (edoc).
;;
;; (edefine (name . formals) (edoc summary clause ...) body ...) binds name
;; to a procedure whose body opens with the edoc form as a quoted datum: a
;; constant the body discards, so it costs nothing to run, while the
;; procedure's recorded source keeps it for introspection. edefine also
;; takes a case-lambda whose clauses each open with an edoc. The form is
;; checked while the module expands: the summary is a string, every formal
;; has exactly one typed clause and nothing else is named, a rest parameter
;; is a list-of, every type is in the vocabulary, and returns appears at
;; most once. edoc outside these forms is a syntax error.
;;
;; Definitions without a lambda body carry their edoc differently. (edefine
;; name (edoc ...) expression) attaches the datum to the value when it is
;; defined -- a parameter, a procedure built by an expression, or any other
;; value; a value without identity, such as a number, is recorded under its
;; name. (edefine-record-type spec (edoc summary (field type note ...) ...)
;; clause ...) documents a record's constructor, predicate and field
;; procedures from one edoc. (edefine-syntax name (edoc ...) transformer)
;; records a keyword's edoc under its name. edoc-of reads an object's
;; signatures back, edoc-named those recorded under a name, and edoc-entry
;; shapes signatures as a documentation entry in the describe corpus's
;; eight-field format.

(library (edoc)
  (export edefine edefine-record-type edefine-syntax edoc
          edoc-of edoc-named signature? signature-kind signature-formals signature-summary
          signature-arguments signature-returns signature-library
          argument? argument-name argument-type argument-notes
          edoc-types edoc-type? type-text edoc-entry edoc-template first-sentence)
  (import (rnrs)
          (only (chezscheme) meta void inspect/object make-weak-eq-hashtable make-eq-hashtable
                eq-hashtable-ref eq-hashtable-set! format path-last make-parameter make-thread-parameter syntax->list
                syntax->annotation annotation-source source-object-sfd source-file-descriptor-path))

  ;;; The vocabulary, for the expander and for run time ---------------------------

  (define-syntax vocabulary
    ;; The editor's notions first, then the language's. A record instance is
    ;; (record name).
    (syntax-rules ()
      [(_) '(file directory buffer window command symbol key mode style
              string char integer number boolean list pair vector bytevector hashtable
              port procedure thunk condition datum any)]))

  (define-syntax type-checker
    ;; (checker types t): is t a type over the vocabulary types? A symbol of
    ;; the vocabulary, (one-of literal ...), (or type type ...), (list-of type)
    ;; or (record name).
    (syntax-rules ()
      [(_)
       (lambda (types t)
         (define (literal? x) (or (symbol? x) (string? x) (number? x) (boolean? x) (char? x)))
         (let ok? ([t t])
           (cond
             [(symbol? t) (and (memq t types) #t)]
             [(and (pair? t) (list? t) (pair? (cdr t)))
              (case (car t)
                [(one-of) (for-all literal? (cdr t))]
                [(or) (and (pair? (cddr t)) (for-all ok? (cdr t)))]
                [(list-of) (and (null? (cddr t)) (ok? (cadr t)))]
                [(record) (and (null? (cddr t)) (symbol? (cadr t)))]
                [else #f])]
             [else #f])))]))

  (meta define known-types (vocabulary))
  (meta define meta-type-ok? (type-checker))
  (define edoc-types (vocabulary))
  (define type-ok? (type-checker))
  (define (edoc-type? t) (type-ok? edoc-types t))

  ;;; Checking, shared by the forms -------------------------------------------------

  (define-syntax edoc
    (lambda (x)
      (syntax-violation 'edoc "edoc belongs at the head of an edefine, edefine-syntax or edefine-record-type" x)))

  (meta define (source-library x)
    ;; the library the form x appears in, from its source file, or #f when
    ;; it was not read from a file
    (let ([annotation (syntax->annotation x)])
      (and annotation
           (let* ([path (source-file-descriptor-path (source-object-sfd (annotation-source annotation)))]
                  [file (path-last path)]
                  [n (string-length file)])
             (and (> n 4) (string=? (substring file (- n 4) n) ".sls")
                  (string-append "(" (substring file 0 (- n 4)) ")"))))))

  (meta define (kept-spec x doc extra)
    ;; the datum kept for introspection: the edoc clauses, then clauses no
    ;; user form can write, since their heads are strings -- extra, and the
    ;; defining library
    (datum->syntax #'edefine
      (append (syntax->datum doc)
        (let ([library (source-library x)])
          (append extra (if library (list (list "library" library)) '()))))))

  (meta define (check-summary! who x doc)
    (syntax-case doc ()
      [(summary clause ...)
       (unless (string? (syntax->datum #'summary))
         (syntax-violation who "the edoc summary must be a string" x #'summary))]
      [_ (syntax-violation who "expected (edoc summary clause ...)" x doc)]))

  (meta define (check-clause-shape! who x clause)
    ;; (name type note ...) with a known type and string notes
    (syntax-case clause ()
      [(head type . notes)
       (identifier? #'head)
       (let ([t (syntax->datum #'type)] [notes (syntax->datum #'notes)])
         (unless (and (list? notes) (for-all string? notes))
           (syntax-violation who "edoc notes must be strings" x clause))
         (unless (meta-type-ok? known-types t)
           (syntax-violation who "unknown edoc type" x #'type)))]
      [_ (syntax-violation who "expected an edoc clause (name type note ...)" x clause)]))

  (meta define (clause-head clause) (syntax-case clause () [(head . _) (syntax->datum #'head)]))

  (meta define (check-free-clauses! who x doc)
    ;; clauses not bound to formals: well shaped, distinct heads, returns at
    ;; most once; the heads other than returns, in order
    (check-summary! who x doc)
    (syntax-case doc ()
      [(summary clause ...)
       (let loop ([clauses #'(clause ...)] [seen '()] [returns? #f])
         (syntax-case clauses ()
           [() (reverse seen)]
           [(clause . rest)
            (begin
              (check-clause-shape! who x #'clause)
              (let ([head (clause-head #'clause)])
                (cond
                  [(eq? head 'returns)
                   (when returns? (syntax-violation who "one returns clause at most" x #'clause))
                   (loop #'rest seen #t)]
                  [else
                   (when (memq head seen) (syntax-violation who "one edoc clause per name" x #'clause))
                   (loop #'rest (cons head seen) returns?)])))]))]))

  ;;; The forms ---------------------------------------------------------------------

  (define-syntax edefine
    (lambda (x)
      (define (formal-names formals)
        ;; ((identifier . rest?) ...) for a lambda list
        (let loop ([f formals] [out '()])
          (syntax-case f ()
            [() (reverse out)]
            [(id . more) (identifier? #'id) (loop #'more (cons (cons #'id #f) out))]
            [id (identifier? #'id) (reverse (cons (cons #'id #t) out))]
            [_ (syntax-violation 'edefine "expected a lambda list" x formals)])))
      (define (spec doc) (kept-spec x doc '()))
      (define (check-value! doc)
        ;; a value's edoc: one (value type note ...) clause, or the argument
        ;; clauses of a procedure the expression builds; returns at most once
        (let ([heads (check-free-clauses! 'edefine x doc)])
          (when (and (memq 'value heads) (pair? (cdr heads)))
            (syntax-violation 'edefine "a value clause stands alone" x doc))))
      (define (check! formals doc)
        (let ([names (formal-names formals)])
          (syntax-case doc ()
            [(summary clause ...)
             (begin
               (unless (string? (syntax->datum #'summary))
                 (syntax-violation 'edefine "the edoc summary must be a string" x #'summary))
               (let loop ([clauses #'(clause ...)] [seen '()] [returns? #f])
                 (syntax-case clauses ()
                   [()
                    (for-each
                      (lambda (entry)
                        (unless (memp (lambda (s) (bound-identifier=? s (car entry))) seen)
                          (syntax-violation 'edefine "every formal needs an edoc clause" x (car entry))))
                      names)]
                   [((head type . notes) . rest)
                    (identifier? #'head)
                    (let ([t (syntax->datum #'type)] [notes (syntax->datum #'notes)])
                      (unless (and (list? notes) (for-all string? notes))
                        (syntax-violation 'edefine "edoc notes must be strings" x #'(head type . notes)))
                      (unless (meta-type-ok? known-types t)
                        (syntax-violation 'edefine "unknown edoc type" x #'type))
                      (cond
                        [(eq? (syntax->datum #'head) 'returns)
                         (when returns?
                           (syntax-violation 'edefine "one returns clause at most" x #'(head type . notes)))
                         (loop #'rest seen #t)]
                        [else
                         (let ([entry (find (lambda (n) (bound-identifier=? (car n) #'head)) names)])
                           (unless entry
                             (syntax-violation 'edefine "an edoc clause names a formal" x #'head))
                           (when (memp (lambda (s) (bound-identifier=? s #'head)) seen)
                             (syntax-violation 'edefine "one edoc clause per formal" x #'head))
                           (when (and (cdr entry) (not (and (pair? t) (eq? (car t) 'list-of))))
                             (syntax-violation 'edefine "a rest parameter is a list-of" x #'type))
                           (loop #'rest (cons #'head seen) returns?))]))]
                   [(other . rest)
                    (syntax-violation 'edefine "expected an edoc clause (name type note ...)" x #'other)])))]
            [_ (syntax-violation 'edefine "expected (edoc summary clause ...)" x doc)])))
      (syntax-case x (edoc case-lambda)
        [(_ (name . formals) (edoc . doc) body0 body ...)
         (identifier? #'name)
         (begin
           (check! #'formals #'doc)
           (with-syntax ([kept (spec #'doc)])
             #'(define name (lambda formals '(edoc . kept) body0 body ...))))]
        [(_ name (case-lambda [formals (edoc . doc) body0 body ...] ...))
         (identifier? #'name)
         (begin
           (for-each check! #'(formals ...) #'(doc ...))
           (with-syntax ([(kept ...) (map spec #'(doc ...))])
             #'(define name (case-lambda [formals '(edoc . kept) body0 body ...] ...))))]
        [(_ name (edoc . doc) expression)
         (identifier? #'name)
         ;; a value: the datum is attached to the object when it is defined
         (begin
           (check-value! #'doc)
           (with-syntax ([kept (kept-spec x #'doc
                                 (list (list "kind"
                                         (syntax-case #'expression [make-parameter make-thread-parameter]
                                           [(make-parameter . _) 'parameter]
                                           [(make-thread-parameter . _) 'parameter]
                                           [_ 'value]))))])
             #'(define name (attach! 'name expression '(edoc . kept)))))]
        [_ (syntax-violation 'edefine
             "expected (edefine (name . formals) (edoc summary clause ...) body ...), a case-lambda whose clauses open with edoc, or (edefine name (edoc summary clause ...) expression)"
             x)])))

  (define-syntax edefine-syntax
    ;; (edefine-syntax name (edoc summary clause ...) transformer): the edoc
    ;; is recorded under the name, since a keyword has no object. Clauses
    ;; describe the form's parts.
    (lambda (x)
      (syntax-case x (edoc)
        [(_ name (edoc . doc) transformer)
         (identifier? #'name)
         (begin
           (check-free-clauses! 'edefine-syntax x #'doc)
           (with-syntax ([kept (kept-spec x #'doc '(("kind" syntax)))]
                         [(tmp) (generate-temporaries '(edoc))])
             #'(begin
                 (define-syntax name transformer)
                 (define tmp (attach-name! 'name '(edoc . kept))))))]
        [_ (syntax-violation 'edefine-syntax
             "expected (edefine-syntax name (edoc summary clause ...) transformer)" x)])))

  (define-syntax edefine-record-type
    ;; (edefine-record-type spec (edoc summary (field type note ...) ...)
    ;; clause ...): a define-record-type whose constructor, predicate and
    ;; field procedures carry edocs derived from the record's. Every field
    ;; has exactly one clause, and nothing else is named. A record with a
    ;; protocol or a parent documents no constructor, since its arguments
    ;; are not the fields.
    (lambda (x)
      (define who 'edefine-record-type)
      (define (field-name field)
        (syntax-case field (mutable immutable)
          [(mutable name . _) #'name]
          [(immutable name . _) #'name]
          [name (identifier? #'name) #'name]
          [_ (syntax-violation who "expected a field spec" x field)]))
      (define (named type text) (datum->syntax type (string->symbol text)))
      (define (type-string type) (symbol->string (syntax->datum type)))
      (define (field-procedures type field)
        ;; (accessor . mutator-or-#f) for one field spec
        (let ([base (string-append (type-string type) "-" (symbol->string (syntax->datum (field-name field))))])
          (syntax-case field (mutable immutable)
            [(mutable name accessor mutator) (cons #'accessor #'mutator)]
            [(mutable name) (cons (named type base) (named type (string-append base "-set!")))]
            [(immutable name accessor) (cons #'accessor #f)]
            [_ (cons (named type base) #f)])))
      (define (clause-named? head)
        (lambda (b) (syntax-case b () [(h . _) (and (identifier? #'h) (eq? (syntax->datum #'h) head))] [_ #f])))
      (syntax-case x (edoc)
        [(_ spec (edoc summary clause ...) body ...)
         (let* ([type (syntax-case #'spec [] [(type . _) #'type] [type #'type])]
                [constructor (syntax-case #'spec []
                               [(type ctor pred) (and (identifier? #'ctor) #'ctor)]
                               [_ (named type (string-append "make-" (type-string type)))])]
                [predicate (syntax-case #'spec []
                             [(type ctor pred) (and (identifier? #'pred) #'pred)]
                             [_ (named type (string-append (type-string type) "?"))])]
                [fields (let ([f (find (clause-named? 'fields) #'(body ...))])
                          (if f (syntax-case f () [(_ . specs) (syntax->list #'specs)]) '()))]
                [plain-constructor?
                 (and constructor
                      (not (exists (clause-named? 'protocol) #'(body ...)))
                      (not (exists (clause-named? 'parent) #'(body ...)))
                      (not (exists (clause-named? 'parent-rtd) #'(body ...))))]
                [type-name (syntax->datum type)]
                [library (source-library x)])
           (check-summary! who x #'(summary clause ...))
           (for-each (lambda (clause) (check-clause-shape! who x clause)) #'(clause ...))
           ;; every field has exactly one clause, and nothing else is named
           (let ([field-names (map (lambda (f) (syntax->datum (field-name f))) fields)]
                 [clause-names (map clause-head #'(clause ...))])
             (for-each
               (lambda (clause)
                 (unless (memq (clause-head clause) field-names)
                   (syntax-violation who "an edoc clause names a field" x clause)))
               #'(clause ...))
             (for-each
               (lambda (f)
                 (unless (memq (syntax->datum (field-name f)) clause-names)
                   (syntax-violation who "every field needs an edoc clause" x (field-name f))))
               fields)
             (let loop ([names clause-names])
               (unless (null? names)
                 (when (memq (car names) (cdr names))
                   (syntax-violation who "one edoc clause per field" x (car names)))
                 (loop (cdr names)))))
           (let* ([clauses (syntax->datum #'(clause ...))]
                  [tail (if library (list (list "library" library)) '())]
                  [instance (list type-name (list 'record type-name))]
                  [attachment
                   (lambda (object summary clauses kind)
                     (list object (append (list 'edoc summary) clauses (list (list "kind" kind)) tail)))]
                  [attachments
                   (append
                     (if plain-constructor?
                         (list (attachment constructor (syntax->datum #'summary) clauses 'constructor))
                         '())
                     (if predicate
                         (list (attachment predicate (format "Whether a value is a ~a." type-name)
                                 (list (list 'value 'any) (list 'returns 'boolean)) 'predicate))
                         '())
                     (apply append
                       (map (lambda (field)
                              (let* ([clause (assq (syntax->datum (field-name field)) clauses)]
                                     [name (car clause)] [field-type (cadr clause)] [notes (cddr clause)]
                                     [procedures (field-procedures type field)])
                                (append
                                  (list (attachment (car procedures)
                                          (format "The ~a of a ~a~a" name type-name
                                            (if (pair? notes) (string-append ": " (car notes)) "."))
                                          (list instance (list 'returns field-type)) 'accessor))
                                  (if (cdr procedures)
                                      (list (attachment (cdr procedures)
                                              (format "Set the ~a of a ~a." name type-name)
                                              (list instance (list 'value field-type)) 'mutator))
                                      '()))))
                            fields)))])
             (with-syntax ([((object datum) ...)
                            (map (lambda (a) (list (car a) (datum->syntax #'edefine-record-type (cadr a)))) attachments)]
                           [(tmp) (generate-temporaries '(edoc))])
               #'(begin
                   (define-record-type spec body ...)
                   (define tmp (begin (attach! 'object object 'datum) ... (void)))))))]
        [_ (syntax-violation who
             "expected (edefine-record-type spec (edoc summary (field type note ...) ...) clause ...)" x)])))

  ;;; Reading it back -------------------------------------------------------------

  ;; A signature: the kind of definition -- procedure, parameter, value,
  ;; syntax, constructor, predicate, accessor or mutator -- its lambda list
  ;; when it has one, the summary, the typed arguments, the return and the
  ;; defining library.
  (define-record-type signature (fields kind formals summary arguments returns library))
  (define-record-type argument (fields name type notes))

  ;; Attached edocs: the forms that document an object rather than a lambda
  ;; body record their datum here when the definition runs, and edoc-of
  ;; consults it before a procedure's source. Objects without identity --
  ;; numbers, characters, booleans, symbols and the like -- and syntax
  ;; keywords are recorded by name instead.
  (define attached (make-weak-eq-hashtable))
  (define named (make-eq-hashtable))

  (define (identity? object)
    (not (or (number? object) (char? object) (boolean? object) (symbol? object)
             (null? object) (eof-object? object) (eq? object (void)))))

  (define (attach! name object spec)
    (if (identity? object)
        (eq-hashtable-set! attached object spec)
        (eq-hashtable-set! named name spec))
    object)

  (define (attach-name! name spec) (eq-hashtable-set! named name spec) name)

  (define (spec-signature kind formals spec library)
    ;; The signature of one (edoc summary clause ...) datum, or #f.
    (and (pair? spec) (eq? (car spec) 'edoc) (pair? (cdr spec)) (string? (cadr spec))
         (let loop ([clauses (cddr spec)] [arguments '()] [returns #f] [library library] [kind kind])
           (cond
             [(null? clauses) (make-signature kind formals (cadr spec) (reverse arguments) returns library)]
             [(and (pair? (car clauses)) (pair? (cdar clauses)))
              (let ([c (car clauses)])
                (cond
                  [(equal? (car c) "library") (loop (cdr clauses) arguments returns (cadr c) kind)]
                  [(equal? (car c) "kind") (loop (cdr clauses) arguments returns library (cadr c))]
                  [(eq? (car c) 'returns)
                   (loop (cdr clauses) arguments (make-argument 'returns (cadr c) (cddr c)) library kind)]
                  [else
                   (loop (cdr clauses) (cons (make-argument (car c) (cadr c) (cddr c)) arguments)
                         returns library kind)]))]
             [else #f]))))

  (define (attached-signatures spec procedure?)
    ;; the signatures of an attached datum; a procedure documented as a
    ;; value is a procedure whose formals are its argument names
    (let ([sig (spec-signature 'value '() spec #f)])
      (and sig
           (list (if (and procedure? (eq? (signature-kind sig) 'value))
                     (make-signature 'procedure (map argument-name (signature-arguments sig))
                       (signature-summary sig) (signature-arguments sig) (signature-returns sig)
                       (signature-library sig))
                     sig)))))

  (define (clause-signature formals body library)
    ;; The signature of one clause whose body opens with a quoted edoc datum.
    (and (pair? body)
         (let ([first (car body)])
           (and (pair? first) (eq? (car first) 'quote) (pair? (cdr first))
                (spec-signature 'procedure formals (cadr first) library)))))

  (define (read-signatures proc)
    (let* ([code (guard (ex [else #f]) ((inspect/object proc) 'code))]
           [source (and code (code 'source))]
           [datum (and source (source 'value))]
           [library
             (and code
                  (call-with-values (lambda () (code 'source-path))
                    (lambda args
                      (and (pair? args) (string? (car args))
                           (let* ([file (path-last (car args))] [n (string-length file)])
                             (and (> n 4) (string=? (substring file (- n 4) n) ".sls")
                                  (string-append "(" (substring file 0 (- n 4)) ")")))))))])
      (and (pair? datum)
           (case (car datum)
             [(lambda)
              (and (pair? (cdr datum))
                   (let ([sig (clause-signature (cadr datum) (cddr datum) library)])
                     (and sig (list sig))))]
             [(case-lambda)
              (let ([sigs (map (lambda (clause)
                                 (and (pair? clause) (clause-signature (car clause) (cdr clause) library)))
                               (cdr datum))])
                (and (pair? sigs) (for-all values sigs) sigs))]
             [else #f]))))

  (define cache (make-weak-eq-hashtable))

  (define (edoc-of object)
    ;; The signatures object was defined with -- an attached edoc, else the
    ;; clauses of a procedure's source -- or #f.
    (cond
      [(eq-hashtable-ref attached object #f)
       => (lambda (spec) (attached-signatures spec (procedure? object)))]
      [(procedure? object)
       (let ([hit (eq-hashtable-ref cache object #f)])
         (cond [(eq? hit 'none) #f]
               [hit hit]
               [else (let ([found (read-signatures object)])
                       (eq-hashtable-set! cache object (or found 'none))
                       found)]))]
      [else #f]))

  (define (edoc-named name)
    ;; The signatures recorded under a name -- a syntax keyword's, or a
    ;; value's without identity -- or #f.
    (let ([spec (eq-hashtable-ref named name #f)])
      (and spec (attached-signatures spec #f))))

  ;;; Presenting ------------------------------------------------------------------

  (define (type-text t)
    ;; a type as prose: file, one of utf-8 or latin-1, string or #f, list of
    ;; buffer, frame record
    (cond
      [(symbol? t) (symbol->string t)]
      [(and (pair? t) (eq? (car t) 'one-of))
       (string-append "one of " (join (map (lambda (x) (format "~s" x)) (cdr t)) " or "))]
      [(and (pair? t) (eq? (car t) 'or)) (join (map type-text (cdr t)) " or ")]
      [(and (pair? t) (eq? (car t) 'list-of)) (string-append "list of " (type-text (cadr t)))]
      [(and (pair? t) (eq? (car t) 'record)) (format "~a record" (cadr t))]
      [else (format "~s" t)]))

  (define (join parts separator)
    (if (null? parts) ""
        (fold-left (lambda (out part) (string-append out separator part)) (car parts) (cdr parts))))

  (define (first-sentence text)
    ;; text up to and including its first sentence end
    (let ([n (string-length text)])
      (let loop ([i 0])
        (cond [(>= i n) text]
              [(and (char=? (string-ref text i) #\.)
                    (or (= (+ i 1) n) (char-whitespace? (string-ref text (+ i 1)))))
               (substring text 0 (+ i 1))]
              [else (loop (+ i 1))]))))

  (define (edoc-template name formals)
    ;; the call template in the corpus's form: (name a b . rest)
    (format "~s" (cons name formals)))

  (define (value-kind? sig) (and (memq (signature-kind sig) '(value parameter)) #t))

  (define (value-argument sig)
    ;; the (value type note ...) clause of a value or parameter, or #f
    (and (value-kind? sig) (find (lambda (a) (eq? (argument-name a) 'value)) (signature-arguments sig))))

  (define (form-of name sig)
    ;; the describe form for one signature: its kind and call template
    (let ([names (map argument-name (signature-arguments sig))])
      (case (signature-kind sig)
        [(parameter) (cons "parameter" (format "(~a [value])" name))]
        [(value) (cons "variable" (format "~a" name))]
        [(syntax) (cons "syntax" (if (null? names) (format "(~a ...)" name) (edoc-template name names)))]
        [(procedure) (cons "procedure" (edoc-template name (signature-formals sig)))]
        [else (cons "procedure" (edoc-template name names))])))

  (define (edoc-entry name sigs)
    ;; A describe entry -- (names forms returns libraries source chapter url
    ;; description) -- for the signatures of the definition bound to name,
    ;; or #f without signatures. A value's type stands where a return does.
    (and (pair? sigs)
         (let* ([returns (find values (map (lambda (sig) (or (signature-returns sig) (value-argument sig))) sigs))]
                [library (signature-library (car sigs))]
                [summaries (let dedupe ([sigs sigs] [out '()])
                             (cond [(null? sigs) (reverse out)]
                                   [(member (signature-summary (car sigs)) out) (dedupe (cdr sigs) out)]
                                   [else (dedupe (cdr sigs) (cons (signature-summary (car sigs)) out))]))]
                [argument-lines
                 (let collect ([sigs sigs] [seen '()] [out '()])
                   (if (null? sigs) (reverse out)
                       (let inner ([arguments (signature-arguments (car sigs))] [seen seen] [out out])
                         (if (null? arguments) (collect (cdr sigs) seen out)
                             (let ([a (car arguments)])
                               (if (or (memq (argument-name a) seen) (eq? a (value-argument (car sigs))))
                                   (inner (cdr arguments) seen out)
                                   (inner (cdr arguments) (cons (argument-name a) seen)
                                     (cons (format "- `~a` (~a)~a" (argument-name a) (type-text (argument-type a))
                                             (if (pair? (argument-notes a))
                                                 (string-append ": " (join (argument-notes a) " ")) ""))
                                           out))))))))])
           (list (list name)
                 (map (lambda (sig) (form-of name sig)) sigs)
                 (and returns
                      (string-append (type-text (argument-type returns))
                                     (if (pair? (argument-notes returns))
                                         (string-append ": " (join (argument-notes returns) " ")) "")))
                 (if library (list library) '())
                 'edoc "Documented definitions" #f
                 (join (append summaries
                               (if (pair? argument-lines) (cons "" argument-lines) '()))
                       "\n")))))
)
