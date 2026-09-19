;; edoc.sls -- documented definitions: the library (edoc).
;;
;; (elibrary (name) (export ...) (import ...) body ...) is a library whose
;; exports are documented. In its body an (edoc summary clause ...) form
;; annotates the definition that follows it -- a define, define-syntax,
;; define-record-type or define-condition-type, all left exactly as they
;; are -- and (edoc name summary clause ...) documents a name defined by
;; some other form. The annotations are checked while the library expands:
;; the summary is a string, every formal, or every field, has exactly one
;; typed clause and nothing else is named, a rest parameter is a list-of,
;; every type is in the vocabulary, and returns appears at most once. Every
;; export the body defines must be annotated, or expansion fails naming
;; the export; re-exported imports and aliases, (define x other), take
;; their documentation from their origin. The library records the edocs
;; when it is initialized: attached to the objects they document, or under
;; the name for a keyword, a record type or a value without identity. A
;; library file starts with (import (only (edoc) elibrary)) and the
;; elibrary form.
;;
;; The older forms remain while libraries migrate: (edefine (name .
;; formals) (edoc ...) body ...) keeps the edoc as a quoted datum at the
;; head of a lambda body, read back through the inspector; (edefine name
;; (edoc ...) expression) attaches it to a value; edefine-record-type,
;; edefine-condition-type and edefine-syntax document their definitions
;; from one edoc. edoc-of reads an object's signatures back, edoc-named
;; those recorded under a name, and edoc-entry shapes signatures as a
;; documentation entry in the describe corpus's eight-field format.

(library (edoc)
  (export elibrary edefine edefine-record-type edefine-condition-type edefine-syntax edoc
          edoc-of edoc-named signature? signature-kind signature-formals signature-summary
          signature-arguments signature-returns signature-library
          argument? argument-name argument-type argument-notes
          edoc-types edoc-type? type-text edoc-entry edoc-template first-sentence)
  (import (rnrs)
          (only (chezscheme) library meta void inspect/object make-weak-eq-hashtable make-eq-hashtable
                eq-hashtable-ref eq-hashtable-set! format path-last make-parameter make-thread-parameter syntax->list
                syntax->annotation annotation-source source-object-sfd source-file-descriptor-path))

  ;;; The vocabulary, for the expander and for run time ---------------------------

  (define-syntax vocabulary
    ;; The editor's notions first -- a position is a (row . col) pair, a
    ;; region a slice of a buffer between two -- then the language's. A
    ;; record instance is (record name); #f stands for itself, for unions
    ;; such as (or string #f).
    (syntax-rules ()
      [(_) '(file directory buffer window region position command symbol key mode style
              string char integer number boolean list pair vector bytevector hashtable
              port procedure thunk condition datum any)]))

  (define-syntax type-checker
    ;; (checker types t): is t a type over the vocabulary types? A symbol of
    ;; the vocabulary, #f, (one-of literal ...), (or type type ...), (list-of
    ;; type) or (record name).
    (syntax-rules ()
      [(_)
       (lambda (types t)
         (define (literal? x) (or (symbol? x) (string? x) (number? x) (boolean? x) (char? x)))
         (let ok? ([t t])
           (cond
             [(symbol? t) (and (memq t types) #t)]
             [(eq? t #f) #t]
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
  (define type-ok? (type-checker))

  ;;; Attachments --------------------------------------------------------------------

  ;; Attached edocs: the forms that document an object rather than a lambda
  ;; body record their datum here when the definition runs, and edoc-of
  ;; consults it before a procedure's source. Objects without identity --
  ;; numbers, characters, booleans, symbols and the like -- keywords and
  ;; record types are recorded by name instead.
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

  ;;; Checking, shared by the forms -------------------------------------------------

  (define-syntax edoc
    (lambda (x)
      (syntax-violation 'edoc "edoc annotates a definition inside an elibrary, or heads an edefine form" x)))

  ;; The forms that cannot document themselves record their edocs by hand;
  ;; the coverage tool reads these attach-name! definitions as documentation.
  (define edoc-documentation
    (attach-name! 'edoc
      '(edoc "The documentation form: a summary, then typed clauses. Inside an elibrary it annotates the definition that follows it, or names the definition it documents; it also heads an edefine form."
         (summary string "the description, its first sentence the short one")
         (clause list "(name type note ...) for a formal or field, or (returns type note ...)")
         ("kind" syntax) ("library" "(edoc)"))))

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

  (meta define (kept-datum doc extra library)
    ;; the datum recorded for introspection: (edoc summary clause ...), then
    ;; clauses no user form can write, since their heads are strings --
    ;; extra, and the defining library
    (append (list 'edoc) (syntax->datum doc) extra (if library (list (list "library" library)) '())))

  (meta define (kept-spec x doc extra)
    ;; kept-datum without its head, as syntax, for the edefine forms
    (datum->syntax #'edefine (cdr (kept-datum doc extra (source-library x)))))

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

  (meta define (check-value-doc! who x doc)
    ;; a value's edoc: one (value type note ...) clause, or the argument
    ;; clauses of a procedure the expression builds; returns at most once
    (let ([heads (check-free-clauses! who x doc)])
      (when (and (memq 'value heads) (pair? (cdr heads)))
        (syntax-violation who "a value clause stands alone" x doc))))

  (meta define (formal-names who x formals)
    ;; ((symbol . rest?) ...) for a lambda list, keeping the identifiers for
    ;; the violations
    (let loop ([f formals] [out '()])
      (syntax-case f ()
        [() (reverse out)]
        [(id . more) (identifier? #'id) (loop #'more (cons (cons #'id #f) out))]
        [id (identifier? #'id) (reverse (cons (cons #'id #t) out))]
        [_ (syntax-violation who "expected a lambda list" x formals)])))

  (meta define (check-formals! who x formals-list doc)
    ;; The edoc of a procedure with one lambda list, or a case-lambda's
    ;; several: every formal has exactly one clause, nothing else is named,
    ;; a rest parameter is a list-of, returns appears at most once.
    (let* ([entries (apply append (map (lambda (formals) (formal-names who x formals)) formals-list))]
           [names (map (lambda (e) (cons (syntax->datum (car e)) (cdr e))) entries)]
           [rest? (lambda (name) (exists (lambda (n) (and (eq? (car n) name) (cdr n))) names))])
      (syntax-case doc ()
        [(summary clause ...)
         (begin
           (unless (string? (syntax->datum #'summary))
             (syntax-violation who "the edoc summary must be a string" x #'summary))
           (let loop ([clauses #'(clause ...)] [seen '()] [returns? #f])
             (syntax-case clauses ()
               [()
                (for-each
                  (lambda (entry)
                    (unless (memq (syntax->datum (car entry)) seen)
                      (syntax-violation who "every formal needs an edoc clause" x (car entry))))
                  entries)]
               [((head type . notes) . rest)
                (identifier? #'head)
                (let ([t (syntax->datum #'type)] [notes (syntax->datum #'notes)] [name (syntax->datum #'head)])
                  (unless (and (list? notes) (for-all string? notes))
                    (syntax-violation who "edoc notes must be strings" x #'(head type . notes)))
                  (unless (meta-type-ok? known-types t)
                    (syntax-violation who "unknown edoc type" x #'type))
                  (cond
                    [(eq? name 'returns)
                     (when returns?
                       (syntax-violation who "one returns clause at most" x #'(head type . notes)))
                     (loop #'rest seen #t)]
                    [else
                     (unless (assq name names)
                       (syntax-violation who "an edoc clause names a formal" x #'head))
                     (when (memq name seen)
                       (syntax-violation who "one edoc clause per formal" x #'head))
                     (when (and (rest? name) (not (and (pair? t) (eq? (car t) 'list-of))))
                       (syntax-violation who "a rest parameter is a list-of" x #'type))
                     (loop #'rest (cons name seen) returns?)]))]
               [(other . rest)
                (syntax-violation who "expected an edoc clause (name type note ...)" x #'other)])))]
        [_ (syntax-violation who "expected (edoc summary clause ...)" x doc)])))

  (meta define (head-is? form sym)
    ;; whether a form's head is the identifier spelled sym: the shapes the
    ;; documenting forms recognize are matched by name, so a body need not
    ;; import anything for them
    (syntax-case form ()
      [(head . _) (and (identifier? #'head) (eq? (syntax->datum #'head) sym))]
      [_ #f]))

  ;; Records: the parts of a define-record-type and the attachments its edoc
  ;; derives -- (object-identifier . datum) for the constructor, the predicate
  ;; and every field procedure -- shared by edefine-record-type and elibrary.

  (meta define (record-parts who x spec bodies)
    ;; (type constructor-or-#f predicate-or-#f fields plain-constructor?)
    (define (named type text) (datum->syntax type (string->symbol text)))
    (define (type-string type) (symbol->string (syntax->datum type)))
    (define (clause-named? head) (lambda (b) (head-is? b head)))
    (let* ([type (syntax-case spec () [(type . _) #'type] [type #'type])]
           [constructor (syntax-case spec ()
                          [(type ctor pred) (and (identifier? #'ctor) #'ctor)]
                          [_ (named type (string-append "make-" (type-string type)))])]
           [predicate (syntax-case spec ()
                        [(type ctor pred) (and (identifier? #'pred) #'pred)]
                        [_ (named type (string-append (type-string type) "?"))])]
           [fields (let ([f (find (clause-named? 'fields) bodies)])
                     (if f (syntax-case f () [(_ . specs) (syntax->list #'specs)]) '()))]
           [plain-constructor?
            (and constructor
                 (not (exists (clause-named? 'protocol) bodies))
                 (not (exists (clause-named? 'parent) bodies))
                 (not (exists (clause-named? 'parent-rtd) bodies)))])
      (list type constructor predicate fields plain-constructor?)))

  (meta define (record-field-name who x field)
    (syntax-case field ()
      [(m name . _) (or (head-is? field 'mutable) (head-is? field 'immutable)) #'name]
      [name (identifier? #'name) #'name]
      [_ (syntax-violation who "expected a field spec" x field)]))

  (meta define (record-field-procedures who x type field)
    ;; (accessor . mutator-or-#f) for one field spec
    (let* ([name (symbol->string (syntax->datum (record-field-name who x field)))]
           [base (string-append (symbol->string (syntax->datum type)) "-" name)]
           [named (lambda (text) (datum->syntax type (string->symbol text)))])
      (syntax-case field ()
        [(m name accessor mutator) (head-is? field 'mutable) (cons #'accessor #'mutator)]
        [(m name) (head-is? field 'mutable) (cons (named base) (named (string-append base "-set!")))]
        [(i name accessor) (head-is? field 'immutable) (cons #'accessor #f)]
        [_ (cons (named base) #f)])))

  (meta define (record-names who x spec bodies)
    ;; every identifier a define-record-type binds: the type, the
    ;; constructor, the predicate, the field procedures
    (let* ([parts (record-parts who x spec bodies)]
           [type (car parts)] [constructor (cadr parts)] [predicate (caddr parts)] [fields (cadddr parts)])
      (append (list type) (if constructor (list constructor) '()) (if predicate (list predicate) '())
              (apply append
                (map (lambda (field)
                       (let ([procedures (record-field-procedures who x type field)])
                         (if (cdr procedures) (list (car procedures) (cdr procedures)) (list (car procedures)))))
                     fields)))))

  (meta define (record-attachments who x spec doc bodies library)
    ;; The edoc of a record -- (summary (field type note ...) ... [(constructor
    ;; field ...)]) -- checked against its fields, as attachments: one per
    ;; procedure, and the record's own under the type name.
    (define (clause-named? head) (lambda (b) (head-is? b head)))
    (let* ([parts (record-parts who x spec bodies)]
           [type (car parts)] [constructor (cadr parts)] [predicate (caddr parts)]
           [fields (cadddr parts)] [plain-constructor? (car (cddddr parts))]
           [clauses-syntax (syntax-case doc () [(summary clause ...) #'(clause ...)])]
           [constructor-clause (find (clause-named? 'constructor) clauses-syntax)]
           [field-clauses (filter (lambda (c) (not ((clause-named? 'constructor) c))) clauses-syntax)]
           [type-name (syntax->datum type)]
           [noun (let* ([s (symbol->string type-name)] [n (string-length s)])
                   ;; a type named to avoid clashing with its constructor
                   ;; procedure, region-record say, is a region in prose
                   (if (and (> n 7) (string=? (substring s (- n 7) n) "-record")) (substring s 0 (- n 7)) s))]
           [summary (syntax-case doc () [(summary . _) (syntax->datum #'summary)])])
      (check-summary! who x doc)
      (for-each (lambda (clause) (check-clause-shape! who x clause)) field-clauses)
      ;; every field has exactly one clause, and nothing else is named
      (let ([field-names (map (lambda (f) (syntax->datum (record-field-name who x f))) fields)]
            [clause-names (map clause-head field-clauses)])
        (for-each
          (lambda (clause)
            (unless (memq (clause-head clause) field-names)
              (syntax-violation who "an edoc clause names a field" x clause)))
          field-clauses)
        (when constructor-clause
          (when plain-constructor?
            (syntax-violation who "a constructor clause belongs to a record with a protocol or parent" x constructor-clause))
          (for-each
            (lambda (f)
              (unless (and (identifier? f) (memq (syntax->datum f) field-names))
                (syntax-violation who "a constructor clause names fields" x f)))
            (syntax->list (syntax-case constructor-clause () [(_ . fs) #'fs]))))
        (for-each
          (lambda (f)
            (unless (memq (syntax->datum (record-field-name who x f)) clause-names)
              (syntax-violation who "every field needs an edoc clause" x (record-field-name who x f))))
          fields)
        (let loop ([names clause-names])
          (unless (null? names)
            (when (memq (car names) (cdr names))
              (syntax-violation who "one edoc clause per field" x (car names)))
            (loop (cdr names)))))
      (let* ([clauses (syntax->datum field-clauses)]
             [tail (if library (list (list "library" library)) '())]
             [instance (list type-name (list 'record type-name))]
             [attachment
              (lambda (object summary clauses kind)
                (cons object (append (list 'edoc summary) clauses (list (list "kind" kind)) tail)))])
        (append
          (list (attachment type summary clauses 'record))
          (cond
            [plain-constructor? (list (attachment constructor summary clauses 'constructor))]
            [(and constructor constructor-clause)
             (list (attachment constructor summary
                     (map (lambda (f) (assq f clauses)) (cdr (syntax->datum constructor-clause)))
                     'constructor))]
            [else '()])
          (if predicate
              (list (attachment predicate (format "Whether a value is a ~a." noun)
                      (list (list 'value 'any) (list 'returns 'boolean)) 'predicate))
              '())
          (apply append
            (map (lambda (field)
                   (let* ([clause (assq (syntax->datum (record-field-name who x field)) clauses)]
                          [name (car clause)] [field-type (cadr clause)] [notes (cddr clause)]
                          [procedures (record-field-procedures who x type field)])
                     (append
                       (list (attachment (car procedures)
                               (format "The ~a of a ~a~a" name noun
                                 (if (pair? notes) (string-append ": " (car notes)) "."))
                               (list instance (list 'returns field-type)) 'accessor))
                       (if (cdr procedures)
                           (list (attachment (cdr procedures)
                                   (format "Set the ~a of a ~a." name noun)
                                   (list instance (list 'value field-type)) 'mutator))
                           '()))))
                 fields))))))

  (meta define (condition-attachments who x name constructor predicate doc field-specs library)
    ;; The edoc of a condition type -- (summary (field type note ...) ...)
    ;; -- as attachments for its constructor, predicate and accessors, and
    ;; the type's own under its name; field-specs are the (field accessor)
    ;; forms.
    (let* ([noun (let ([s (symbol->string (syntax->datum name))])
                   (if (and (> (string-length s) 1) (char=? (string-ref s 0) #\&)) (substring s 1 (string-length s)) s))]
           [article (if (memv (string-ref noun 0) '(#\a #\e #\i #\o #\u)) "an" "a")]
           [fields (map (lambda (spec) (syntax-case spec () [(field accessor) #'field])) field-specs)]
           [accessors (map (lambda (spec) (syntax-case spec () [(field accessor) #'accessor])) field-specs)]
           [field-names (map syntax->datum fields)]
           [clauses-syntax (syntax-case doc () [(summary clause ...) #'(clause ...)])]
           [clause-names (map clause-head clauses-syntax)]
           [summary (syntax-case doc () [(summary . _) (syntax->datum #'summary)])])
      (check-summary! who x doc)
      (for-each (lambda (clause) (check-clause-shape! who x clause)) clauses-syntax)
      (for-each
        (lambda (clause)
          (unless (memq (clause-head clause) field-names)
            (syntax-violation who "an edoc clause names a field" x clause)))
        clauses-syntax)
      (for-each
        (lambda (f)
          (unless (memq (syntax->datum f) clause-names)
            (syntax-violation who "every field needs an edoc clause" x f)))
        fields)
      (let loop ([names clause-names])
        (unless (null? names)
          (when (memq (car names) (cdr names))
            (syntax-violation who "one edoc clause per field" x (car names)))
          (loop (cdr names))))
      (let* ([clauses (syntax->datum clauses-syntax)]
             [tail (if library (list (list "library" library)) '())]
             [attachment
              (lambda (object summary clauses kind)
                (cons object (append (list 'edoc summary) clauses (list (list "kind" kind)) tail)))])
        (append
          (list (attachment name summary clauses 'condition)
                (attachment constructor summary clauses 'constructor)
                (attachment predicate (format "Whether a condition is ~a ~a." article noun)
                  (list (list 'value 'any) (list 'returns 'boolean)) 'predicate))
          (map (lambda (accessor field)
                 (let ([clause (assq (syntax->datum field) clauses)])
                   (attachment accessor (format "The ~a of ~a ~a~a" (car clause) article noun
                                          (if (pair? (cddr clause)) (string-append ": " (caddr clause)) "."))
                     (list (list 'condition 'condition) (list 'returns (cadr clause))) 'accessor)))
               accessors fields)))))

  (meta define (attachment-forms attachments by-name)
    ;; the expressions recording attachments at initialization: by object,
    ;; or by name for the identifiers in by-name -- keywords and types,
    ;; which have no object to attach to
    (map (lambda (a)
           (with-syntax ([object (car a)] [datum (datum->syntax #'edoc (cdr a))])
             (if (memp (lambda (id) (bound-identifier=? id (car a))) by-name)
                 #'(attach-name! 'object 'datum)
                 #'(attach! 'object object 'datum))))
         attachments))

  ;;; The documented library ------------------------------------------------------

  (define-syntax elibrary
    (lambda (x)
      (define who 'elibrary)
      (define (edoc-form? form) (head-is? form 'edoc))
      (define (export-identifiers exports)
        ;; the internal identifiers the export clause names
        (syntax-case exports ()
          [(_ spec ...)
           (apply append
             (map (lambda (spec)
                    (syntax-case spec ()
                      [id (identifier? #'id) (list #'id)]
                      [(r (internal external) ...) (head-is? spec 'rename) (syntax->list #'(internal ...))]
                      [_ (syntax-violation who "expected an export spec" x spec)]))
                  (syntax->list #'(spec ...))))]))
      (define (definition-info form)
        ;; (kind names . details) for a definition form, or #f
        (define (define? f) (head-is? f 'define))
        (syntax-case form ()
          [(d (name . formals) body ...) (and (define? form) (identifier? #'name))
           (list 'procedure (list #'name) (list #'formals))]
          [(d name expression) (and (define? form) (identifier? #'name))
           (syntax-case #'expression []
             [(l formals body ...) (head-is? #'expression 'lambda)
              (list 'procedure (list #'name) (list #'formals))]
             [(cl (formals body ...) ...) (head-is? #'expression 'case-lambda)
              (list 'procedure (list #'name) (syntax->list #'(formals ...)))]
             [(mp . _) (or (head-is? #'expression 'make-parameter) (head-is? #'expression 'make-thread-parameter))
              (list 'parameter (list #'name))]
             [origin (identifier? #'origin) (list 'alias (list #'name))]
             [_ (list 'value (list #'name))])]
          [(d name) (and (define? form) (identifier? #'name)) (list 'value (list #'name))]
          [(ds name . _) (and (head-is? form 'define-syntax) (identifier? #'name)) (list 'syntax (list #'name))]
          [(drt spec clause ...) (head-is? form 'define-record-type)
           (list 'record (record-names who x #'spec (syntax->list #'(clause ...))) #'spec (syntax->list #'(clause ...)))]
          [(dct name parent constructor predicate (field accessor) ...) (head-is? form 'define-condition-type)
           (list 'condition (append (list #'name #'constructor #'predicate) (syntax->list #'(accessor ...)))
                 #'name #'constructor #'predicate (syntax->list #'((field accessor) ...)))]
          [_ #f]))
      (define (same-name? a b) (eq? (syntax->datum a) (syntax->datum b)))
      (define (attachments-of info doc library-name)
        ;; the attachments an edoc gives a definition; doc is (summary clause ...)
        (let ([kind (car info)] [name (car (cadr info))])
          (define (own extra) (list (cons name (kept-datum doc extra library-name))))
          (case kind
            [(procedure)
             (check-formals! who x (caddr info) doc)
             (own (list (list "kind" 'procedure) (cons "formals" (map syntax->datum (caddr info)))))]
            [(parameter) (check-value-doc! who x doc) (own (list (list "kind" 'parameter)))]
            [(value) (check-value-doc! who x doc) (own (list (list "kind" 'value)))]
            [(syntax) (check-free-clauses! who x doc) (own (list (list "kind" 'syntax)))]
            [(record) (record-attachments who x (caddr info) doc (cadddr info) library-name)]
            [(condition)
             (let ([details (cddr info)])
               (condition-attachments who x (car details) (cadr details) (caddr details) doc (cadddr details) library-name))]
            [(alias) (syntax-violation who "an alias takes its documentation from its origin" doc)])))
      (syntax-case x ()
        [(_ name exports imports body ...)
         (and (head-is? #'exports 'export) (head-is? #'imports 'import))
         (let ([library-name (format "~s" (syntax->datum #'name))]
               [exported (export-identifiers #'exports)])
           ;; pair every annotation with the definition that follows it
           (let walk ([forms (syntax->list #'(body ...))] [pending #f] [kept '()] [entries '()] [named '()])
             (cond
               [(null? forms)
                (when pending (syntax-violation who "an edoc annotates the definition that follows it" pending))
                (let* ([entries (reverse entries)]
                       [named (reverse named)]
                       [documented
                        ;; (info doc-or-#f) with named edocs claimed by their definitions
                        (map (lambda (entry)
                               (let* ([info (car entry)] [doc (cadr entry)]
                                      [hit (find (lambda (n) (exists (lambda (id) (same-name? id (car n))) (cadr info))) named)])
                                 (when (and doc hit)
                                   (syntax-violation who "two edocs document one definition" (cdr hit)))
                                 (list info (or doc (and hit (cdr hit))))))
                             entries)]
                       [claimed (filter (lambda (n) (exists (lambda (entry) (exists (lambda (id) (same-name? id (car n))) (cadr (car entry)))) entries)) named)]
                       [free (filter (lambda (n) (not (memq n claimed))) named)])
                  ;; every export the body defines is documented
                  (for-each
                    (lambda (id)
                      (let ([entry (find (lambda (entry) (exists (lambda (n) (same-name? n id)) (cadr (car entry)))) documented)])
                        (when (and entry (not (eq? (car (car entry)) 'alias)) (not (cadr entry)))
                          (syntax-violation who "export has no edoc" id))))
                    exported)
                  (let* ([attachments
                          (append
                            (apply append
                              (map (lambda (entry)
                                     (if (cadr entry)
                                         (attachments-of (car entry) (syntax-case (cadr entry) () [(_ . doc) #'doc]) library-name)
                                         '()))
                                   documented))
                            ;; names defined by forms this walk cannot see
                            (map (lambda (n)
                                   (let ([doc (syntax-case (cdr n) () [(_ id . doc) #'doc])])
                                     (check-value-doc! who x doc)
                                     (cons (car n) (kept-datum doc (list (list "kind" 'value)) library-name))))
                                 free))]
                         [by-name
                          (apply append
                            (map (lambda (entry)
                                   (let ([info (car entry)])
                                     (case (car info)
                                       [(syntax) (list (car (cadr info)))]
                                       [(record condition) (list (car (cadr info)))]
                                       [else '()])))
                                 documented))])
                    (with-syntax ([(form ...) (reverse kept)]
                                  [(attachment ...) (attachment-forms attachments by-name)]
                                  [(tmp) (generate-temporaries '(edocs))])
                      #'(library name exports imports
                          form ...
                          (define tmp (begin attachment ... (void)))))))]
               [(edoc-form? (car forms))
                (syntax-case (car forms) ()
                  [(_ summary clause ...) (string? (syntax->datum #'summary))
                   (begin
                     (when pending (syntax-violation who "two edocs annotate one definition" (car forms)))
                     (walk (cdr forms) (car forms) kept entries named))]
                  [(_ id summary clause ...) (and (identifier? #'id) (string? (syntax->datum #'summary)))
                   (walk (cdr forms) pending kept entries (cons (cons #'id (car forms)) named))]
                  [_ (syntax-violation who "expected (edoc summary clause ...) or (edoc name summary clause ...)" (car forms))])]
               [else
                (let ([info (definition-info (car forms))])
                  (cond
                    [info (walk (cdr forms) #f (cons (car forms) kept) (cons (list info pending) entries) named)]
                    [pending (syntax-violation who "an edoc annotates the definition that follows it" pending)]
                    [else (walk (cdr forms) #f (cons (car forms) kept) entries named)]))])))]
        [_ (syntax-violation who "expected (elibrary (name) (export ...) (import ...) body ...)" x)])))

  (define elibrary-documentation
    (attach-name! 'elibrary
      '(edoc "A library whose exports are documented: an edoc annotates the definition that follows it, and every export the body defines must have one."
         (name list "the library name")
         (exports list "the export clause")
         (imports list "the import clause")
         (body any "the definitions, annotated")
         ("kind" syntax) ("library" "(edoc)"))))

  ;;; The older forms ----------------------------------------------------------------

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

  (define edefine-syntax-documentation
    (attach-name! 'edefine-syntax
      '(edoc "Define a keyword whose edoc is recorded under its name, the clauses describing the form's parts."
         (name symbol "the keyword")
         (transformer any "the transformer, as define-syntax takes it")
         ("kind" syntax) ("library" "(edoc)"))))

  (edefine-syntax edefine
    (edoc "Define a documented procedure, (edefine (name . formals) (edoc ...) body ...) or a case-lambda whose clauses open with an edoc, or a documented value, (edefine name (edoc ...) expression)."
          (name symbol "the name defined")
          (formals list "the lambda list, each formal with an edoc clause")
          (body any "the body, which may open with definitions"))
    (lambda (x)
      (define (spec doc) (kept-spec x doc '()))
      (syntax-case x (edoc case-lambda)
        [(_ (name . formals) (edoc . doc) body0 body ...)
         (identifier? #'name)
         (begin
           (check-formals! 'edefine x (list #'formals) #'doc)
           (with-syntax ([kept (spec #'doc)])
             ;; the body in its own scope, so its internal definitions may
             ;; follow the datum
             #'(define name (lambda formals '(edoc . kept) (let () body0 body ...)))))]
        [(_ name (case-lambda [formals (edoc . doc) body0 body ...] ...))
         (identifier? #'name)
         (begin
           (for-each (lambda (formals doc) (check-formals! 'edefine x (list formals) doc)) #'(formals ...) #'(doc ...))
           (with-syntax ([(kept ...) (map spec #'(doc ...))])
             #'(define name (case-lambda [formals '(edoc . kept) (let () body0 body ...)] ...))))]
        [(_ name (edoc . doc) expression)
         (identifier? #'name)
         ;; a value: the datum is attached to the object when it is defined
         (begin
           (check-value-doc! 'edefine x #'doc)
           (with-syntax ([kept (kept-spec x #'doc
                                 (list (list "kind"
                                         (if (or (head-is? #'expression 'make-parameter) (head-is? #'expression 'make-thread-parameter))
                                             'parameter 'value))))])
             #'(define name (attach! 'name expression '(edoc . kept)))))]
        [_ (syntax-violation 'edefine
             "expected (edefine (name . formals) (edoc summary clause ...) body ...), a case-lambda whose clauses open with edoc, or (edefine name (edoc summary clause ...) expression)"
             x)])))

  (edefine-syntax edefine-record-type
    (edoc "A define-record-type whose constructor, predicate and field procedures carry edocs derived from one edoc naming every field; a (constructor field ...) clause documents a protocol's constructor."
          (spec any "the record name, or (name constructor predicate)")
          (clause list "the define-record-type clauses: fields, protocol and the rest"))
    (lambda (x)
      (syntax-case x (edoc)
        [(_ spec (edoc . doc) body ...)
         (let* ([attachments (record-attachments 'edefine-record-type x #'spec #'doc (syntax->list #'(body ...)) (source-library x))]
                [type (car (record-parts 'edefine-record-type x #'spec (syntax->list #'(body ...))))])
           (with-syntax ([(attachment ...) (attachment-forms attachments (list type))]
                         [(tmp) (generate-temporaries '(edoc))])
             #'(begin
                 (define-record-type spec body ...)
                 (define tmp (begin attachment ... (void))))))]
        [_ (syntax-violation 'edefine-record-type
             "expected (edefine-record-type spec (edoc summary (field type note ...) ...) clause ...)" x)])))

  (edefine-syntax edefine-condition-type
    (edoc "A define-condition-type whose constructor, predicate and accessors carry edocs derived from one edoc naming every field."
          (name symbol "the condition type, &name")
          (parent symbol "its parent condition type")
          (constructor symbol "the constructor's name")
          (predicate symbol "the predicate's name")
          (field list "(field accessor) per field"))
    (lambda (x)
      (syntax-case x (edoc)
        [(_ name parent constructor predicate (edoc . doc) (field accessor) ...)
         (let ([attachments (condition-attachments 'edefine-condition-type x #'name #'constructor #'predicate #'doc
                              (syntax->list #'((field accessor) ...)) (source-library x))])
           (with-syntax ([(attachment ...) (attachment-forms attachments (list #'name))]
                         [(tmp) (generate-temporaries '(edoc))])
             #'(begin
                 (define-condition-type name parent constructor predicate (field accessor) ...)
                 (define tmp (begin attachment ... (void))))))]
        [_ (syntax-violation 'edefine-condition-type
             "expected (edefine-condition-type &name &parent constructor predicate (edoc summary (field type note ...) ...) (field accessor) ...)" x)])))

  ;;; Reading it back -------------------------------------------------------------

  (edefine edoc-types
    (edoc "The type vocabulary: the editor's notions, then the language's; compounds are (one-of literal ...), (or type ...), (list-of type) and (record name)."
          (value (list-of symbol)))
    (vocabulary))

  (edefine (edoc-type? t)
    (edoc "Whether a value is an edoc type over the vocabulary." (t datum "the value") (returns boolean))
    (type-ok? edoc-types t))

  ;; A signature: the kind of definition -- procedure, parameter, value,
  ;; syntax, record, condition, constructor, predicate, accessor or mutator
  ;; -- its lambda list when it has one, the summary, the typed arguments,
  ;; the return and the defining library.
  (edefine-record-type signature
    (edoc "What one definition, or one lambda list of a case-lambda, was documented with."
          (kind symbol "procedure, parameter, value, syntax, record, condition, constructor, predicate, accessor or mutator")
          (formals list "the lambda list, for a procedure")
          (summary string "the description")
          (arguments (list-of (record argument)) "the typed arguments")
          (returns (or (record argument) #f) "the return, as an argument named returns")
          (library (or string #f) "the defining library, (edit) say"))
    (fields kind formals summary arguments returns library))
  (edefine-record-type argument
    (edoc "One typed clause of an edoc."
          (name symbol "the formal or field")
          (type datum "its edoc type")
          (notes (list-of string) "its notes"))
    (fields name type notes))

  (define (formal-symbols formals)
    ;; the names in a lambda list, proper or not
    (let loop ([f formals] [out '()])
      (cond [(pair? f) (loop (cdr f) (cons (car f) out))]
            [(symbol? f) (reverse (cons f out))]
            [else (reverse out)])))

  (define (spec-signatures kind formals spec library)
    ;; The signatures of one (edoc summary clause ...) datum, or #f: one per
    ;; lambda list of its "formals" clause, each keeping the arguments its
    ;; formals name, else one with the formals given.
    (and (pair? spec) (eq? (car spec) 'edoc) (pair? (cdr spec)) (string? (cadr spec))
         (let loop ([clauses (cddr spec)] [arguments '()] [returns #f] [library library] [kind kind] [lambda-lists #f])
           (cond
             [(null? clauses)
              (let ([arguments (reverse arguments)] [summary (cadr spec)])
                (if lambda-lists
                    (map (lambda (f)
                           (let ([names (formal-symbols f)])
                             (make-signature kind f summary
                               (filter (lambda (a) (memq (argument-name a) names)) arguments) returns library)))
                         lambda-lists)
                    (list (make-signature kind formals summary arguments returns library))))]
             [(and (pair? (car clauses)) (pair? (cdar clauses)))
              (let ([c (car clauses)])
                (cond
                  [(equal? (car c) "library") (loop (cdr clauses) arguments returns (cadr c) kind lambda-lists)]
                  [(equal? (car c) "kind") (loop (cdr clauses) arguments returns library (cadr c) lambda-lists)]
                  [(equal? (car c) "formals") (loop (cdr clauses) arguments returns library kind (cdr c))]
                  [(eq? (car c) 'returns)
                   (loop (cdr clauses) arguments (make-argument 'returns (cadr c) (cddr c)) library kind lambda-lists)]
                  [else
                   (loop (cdr clauses) (cons (make-argument (car c) (cadr c) (cddr c)) arguments)
                         returns library kind lambda-lists)]))]
             [else #f]))))

  (define (attached-signatures spec procedure?)
    ;; the signatures of an attached datum; a procedure documented as a
    ;; value is a procedure whose formals are its argument names
    (let ([sigs (spec-signatures 'value '() spec #f)])
      (and sigs
           (map (lambda (sig)
                  (if (and procedure? (eq? (signature-kind sig) 'value))
                      (make-signature 'procedure (map argument-name (signature-arguments sig))
                        (signature-summary sig) (signature-arguments sig) (signature-returns sig)
                        (signature-library sig))
                      sig))
                sigs))))

  (define (clause-signature formals body library)
    ;; The signature of one clause whose body opens with a quoted edoc datum.
    (and (pair? body)
         (let ([first (car body)])
           (and (pair? first) (eq? (car first) 'quote) (pair? (cdr first))
                (let ([sigs (spec-signatures 'procedure formals (cadr first) library)])
                  (and sigs (car sigs)))))))

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

  (edefine (edoc-of object)
    (edoc "The signatures an object was defined with, from its attached edoc or a procedure's source, or #f."
          (object any "the procedure, parameter or value")
          (returns (or (list-of (record signature)) #f)))
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

  (edefine (edoc-named name)
    (edoc "The signatures recorded under a name: a keyword's, a record or condition type's, or a value's without identity; or #f."
          (name symbol "the name")
          (returns (or (list-of (record signature)) #f)))
    (let ([spec (eq-hashtable-ref named name #f)])
      (and spec (attached-signatures spec #f))))

  ;;; Presenting ------------------------------------------------------------------

  (edefine (type-text t)
    (edoc "An edoc type as prose: file, one of utf-8 or latin-1, string or #f, list of buffer, frame record."
          (t datum "the type")
          (returns string))
    (cond
      [(symbol? t) (symbol->string t)]
      [(and (pair? t) (eq? (car t) 'one-of))
       (string-append "one of " (join (map (lambda (x) (format "~s" x)) (cdr t)) " or "))]
      [(and (pair? t) (eq? (car t) 'or)) (join (map type-text (cdr t)) " or ")]
      [(and (pair? t) (eq? (car t) 'list-of)) (string-append "list of " (type-text (cadr t)))]
      [(and (pair? t) (eq? (car t) 'record)) (format "~a record" (cadr t))]
      [(eq? t #f) "#f"]
      [else (format "~s" t)]))

  (define (join parts separator)
    (if (null? parts) ""
        (fold-left (lambda (out part) (string-append out separator part)) (car parts) (cdr parts))))

  (edefine (first-sentence text)
    (edoc "A text up to and including its first sentence end." (text string "the text") (returns string))
    (let ([n (string-length text)])
      (let loop ([i 0])
        (cond [(>= i n) text]
              [(and (char=? (string-ref text i) #\.)
                    (or (= (+ i 1) n) (char-whitespace? (string-ref text (+ i 1)))))
               (substring text 0 (+ i 1))]
              [else (loop (+ i 1))]))))

  (edefine (edoc-template name formals)
    (edoc "A call template in the describe corpus's form: (name a b . rest)."
          (name symbol "the procedure")
          (formals list "its lambda list")
          (returns string))
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
        [(record) (cons "record" (format "~a" name))]
        [(condition) (cons "condition" (format "~a" name))]
        [(procedure) (cons "procedure" (edoc-template name (signature-formals sig)))]
        [else (cons "procedure" (edoc-template name names))])))

  (edefine (edoc-entry name sigs)
    (edoc "A describe entry, (names forms returns libraries source chapter url description), for the signatures of the definition bound to a name, or #f without signatures; a value's type stands where a return does."
          (name symbol "the name")
          (sigs (or (list-of (record signature)) #f) "its signatures")
          (returns (or list #f)))
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
