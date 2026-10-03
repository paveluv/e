;; edoc.sls -- documented libraries: the library (edoc).
;;
;; (elibrary (name) (export ...) (import ...) body ...) is a library whose
;; exports are documented. In its body an (edoc summary clause ...) form
;; annotates the definition that follows it -- a define, define-syntax,
;; define-record-type or define-condition-type, left exactly as written --
;; and (edoc name summary clause ...) documents a name some other form
;; defines. The annotations are checked while the library expands: the
;; summary is a string, every formal, or every field, has exactly one typed
;; clause and nothing else is named, a rest parameter is a list-of, every
;; type is in the vocabulary, and returns appears at most once. Every export
;; the body defines must be annotated, or expansion fails naming the
;; export; re-exported imports and aliases, (define x other), take their
;; documentation from their origin. When the library is initialized the
;; edocs are attached to the objects they document, or recorded under the
;; name for a keyword, a record type or a value without identity. A library
;; file starts with (import (only (foundation edoc) elibrary)) and the elibrary form.
;;
;; Types are data. A name in a clause resolves, when the library
;; initializes, to a type record with prose, a predicate and optionally a
;; completer, a reader and a writer; the language's types come predefined
;; here, and a library defines the types for the notions it owns with an
;; (edoc-type name prose (predicate p) ...) form in its body, registered
;; when it initializes. type-accepts?, type-completions and type-spelling
;; work over the compound forms too. A record documented in an elibrary
;; registers its predicate for (record name).
;;
;; edoc-of reads an object's signatures back, edoc-named those recorded
;; under a name, and edoc-entry shapes signatures as a documentation entry
;; in the describe corpus's eight-field format. This library documents
;; itself with the same helpers, through forms of its own.

(library (foundation edoc)
  (export argument-name argument-notes argument-type argument? call-argument-type edoc edoc-entry edoc-named edoc-of
          edoc-template edoc-type edoc-type? edoc-types elibrary expression forward-callee forwarding-name forwarding-steps inspection-value
          install-type-registry! observe-types! restore-types!
          signature-arguments signature-flags signature-formals signature-kind signature-library
          signature-receiver signature-returns signature-summary signature? type-accepts? type-compatible? type-completions
          type-denotes-record? type-named type-owner type-portable? type-prose
          type-spelling
          type-text type-value type-within value-expression)
  (import (rnrs)
          (only (chezscheme) library import meta void make-weak-eq-hashtable make-eq-hashtable
                eq-hashtable-ref eq-hashtable-set! eq-hashtable-contains? format syntax->list
                procedure-arity-mask logbit? make-compile-time-value fluid-let-syntax define-property iota list-tail delay force))

  ;;; The vocabulary, for the expander and for run time ---------------------------

  (define-syntax vocabulary
    ;; The editor's notions first -- a position is a (row . col) pair, a
    ;; region a slice of a buffer between two -- then the language's. A
    ;; record instance is (record name); #f stands for itself, for unions
    ;; such as (or string #f).
    (syntax-rules ()
      [(_) '(file directory buffer window region position command symbol key mode style actor head
              string char integer number boolean list pair vector bytevector hashtable
              port procedure thunk condition datum any)]))

  (define-syntax type-checker
    ;; (checker known? t): is t a type? A symbol known? accepts, #f, (one-of
    ;; literal ...), (or type type ...), (list-of type) or (record name).
    ;; While a library expands every symbol may name a type another library
    ;; defines, so the expander accepts them all and initialization checks.
    (syntax-rules ()
      [(_)
       (lambda (known? t)
         (define (literal? x) (or (symbol? x) (string? x) (number? x) (boolean? x) (char? x)))
         (let ok? ([t t])
           (cond
             [(symbol? t) (and (known? t) #t)]
             [(eq? t #f) #t]
             [(and (pair? t) (list? t) (pair? (cdr t)))
              (case (car t)
                [(one-of) (for-all literal? (cdr t))]
                [(or) (and (pair? (cddr t)) (for-all ok? (cdr t)))]
                [(list-of) (and (null? (cddr t)) (ok? (cadr t)))]
                [(record) (and (null? (cddr t)) (symbol? (cadr t)))]
                [else #f])]
             [else #f])))]))

  (meta define known-types (lambda (t) #t))
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
    (check-spec-types! spec name)
    (when (and (procedure? object) (member '(inspect) (cddr spec)))
      (eq-hashtable-set! inspection-queries object #t))
    (if (identity? object)
        (eq-hashtable-set! attached object spec)
        (eq-hashtable-set! named name spec))
    object)

  (define (attach-name! name spec) (check-spec-types! spec name) (eq-hashtable-set! named name spec) name)

  ;; Forwarding is compiler metadata. Only the public syntax can enter the
  ;; private dispatcher; each admitted call site belongs to a described body.
  (define forward-property #f)
  (define inspection-property #f)
  (define-syntax forward-context (make-compile-time-value #f))
  (define forwards (make-weak-eq-hashtable))
  (define forward-names (make-weak-eq-hashtable))
  (define forward-inspectors (make-weak-eq-hashtable))
  (define inspection-queries (make-weak-eq-hashtable))

  (define (attach-forwards! procedure descriptions)
    (eq-hashtable-set! forwards procedure descriptions)
    procedure)

  (define (install-forward! procedure name inspect)
    (eq-hashtable-set! forward-names procedure name)
    (eq-hashtable-set! forward-inspectors procedure inspect))

  (define-syntax admitted-forward
    (syntax-rules ()
      [(_ expression)
       (fluid-let-syntax ((forward-context (make-compile-time-value #t))) expression)]))

  (define-syntax forward-callee
    (lambda (x)
      (lambda (lookup)
        (syntax-case x ()
          [(_ name)
           (let ([p (and (identifier? #'name) (lookup #'name #'forward-property))])
             (if p (car p) #'name))]))))

  (define-syntax attach-named-definition!
    (lambda (x)
      (lambda (lookup)
        (syntax-case x ()
          [(_ name spec)
           (let ([p (lookup #'name #'forward-property)])
             (if p #`(begin (attach-name! 'name spec) (attach! 'name #,(car p) spec))
               #'(attach-name! 'name spec)))]))))

  (define-syntax install-named-forward!
    (lambda (x)
      (lambda (lookup)
        (syntax-case x ()
          [(_ name)
           (let ([p (lookup #'name #'forward-property)])
             #`(install-forward! #,(car p) #,(cadr p) #,(caddr p)))]))))

  (define-syntax forwarding-definition
    (lambda (x)
      (syntax-case x ()
        [(_ name public-name procedure inspect)
         #'(begin
             (define-syntax name
               (lambda (call)
                 (lambda (lookup)
                   (unless (lookup #'forward-context)
                     (syntax-violation 'name "forwarding requires an elibrary procedure or edoc:expression" call))
                   (syntax-case call (apply)
                     [(_ (apply argument (... ...) tail))
                      #'(fluid-let-syntax ((forward-context (make-compile-time-value #f)))
                          (apply procedure argument (... ...) tail))]
                     [(_ argument (... ...))
                      #'(fluid-let-syntax ((forward-context (make-compile-time-value #f)))
                          (procedure argument (... ...)))]
                     [_ (syntax-violation 'name "forwarding syntax is not a procedure; use keymap:call for a binding" call)]))))
             (define-property name forward-property (list #'procedure #'public-name #'inspect)))])))

  ;; This pass handles lexical Scheme forms, not arbitrary macro expansion.
  ;; A macro that hides a dispatch site must introduce edoc:expression itself;
  ;; otherwise the dispatch syntax rejects it, rather than losing metadata.
  (define-syntax described-procedure
    (lambda (x)
      (lambda (lookup)
        (define (unknown e)
          (with-syntax ([datum (datum->syntax #'described-procedure (list 'unknown (syntax->datum e)))]) #' 'datum))
        (define (property id)
          (guard (ex [else #f]) (lookup id #'forward-property)))
        (define (query? id)
          (and (identifier? id)
            (or (exists (lambda (p) (free-identifier=? id p))
                  (syntax->list #'(car cdr cadr caddr cadddr null? pair? list cons append not equal? eq?)))
                (guard (ex [else #f]) (lookup id #'inspection-property)))))
        (define (assigned form)
          (syntax-case form (quote syntax quasiquote quasisyntax set!)
            [(quote . _) '()] [(syntax . _) '()] [(quasiquote . _) '()] [(quasisyntax . _) '()]
            [(set! id value) (cons #'id (assigned #'value))]
            [(a . b) (append (assigned #'a) (assigned #'b))] [_ '()]))
        (define written (assigned x))
        (define (mutable? id) (exists (lambda (name) (bound-identifier=? id name)) written))
        (define (binding id env)
          (and (identifier? id) (find (lambda (p) (bound-identifier=? id (car p))) env)))
        (define (shadow ids env)
          (append (map (lambda (id) (cons id (unknown id))) ids) env))
        (define (ids formals)
          (syntax-case formals ()
            [() '()] [(a . b) (cons #'a (ids #'b))] [a (list #'a)]))
        (define (parameters formals)
          (let loop ([f formals] [i 0])
            (syntax-case f ()
              [() '()]
              [(a . b) (cons (cons #'a #`'(argument #,i a)) (loop #'b (+ i 1)))]
              [a (list (cons #'a #`'(rest #,i a)))])))
        (define (template e env)
          (syntax-case e (quote if)
            [(quote datum) #'(list 'value 'datum)]
            [(if test yes no) #`(list 'if #,(template #'test env) #,(template #'yes env) #,(template #'no env))]
            [id (identifier? #'id) (if (mutable? #'id) (unknown #'id) (cond [(binding #'id env) => cdr] [else (unknown #'id)]))]
            [(proc argument ...)
             (and (query? #'proc) (not (binding #'proc env)))
             #`(list 'query proc (list #,@(map (lambda (arg) (template arg env)) (syntax->list #'(argument ...)))))]
            [datum (not (pair? (syntax->datum #'datum))) #'(list 'value 'datum)]
            [_ (unknown e)]))
        (define collected '())
        (define (walk e env)
          (syntax-case e (quote syntax quasiquote quasisyntax expression lambda case-lambda let let* letrec letrec* let-values let*-values let-syntax letrec-syntax do define define-syntax set!)
            [(quote . _) e] [(syntax . _) e]
            ;; Explicit metadata boundaries perform their own traversal. Walking
            ;; their generated procedures again can expand indefinitely when a
            ;; startup form is already wrapped by kernel:evaluate!.
            [(expression . _) e]
            ;; Quoted templates have their own evaluation rules. A hidden
            ;; forwarding site in an unquote must use edoc:expression.
            [(quasiquote . _) e] [(quasisyntax . _) e]
            [(lambda formals body ...) #`(described-procedure (lambda formals body ...))]
            [(case-lambda . _) #`(described-procedure #,e)]
            [(define (name . formals) body ...)
             #'(define name (described-procedure (lambda formals body ...)))]
            [(define name value) #`(define name #,(walk #'value env))]
            [(define-syntax . _) e]
            [(kind bindings body ...)
             (memq (syntax->datum #'kind) '(let-syntax letrec-syntax))
             #`(kind bindings #,@(walk-body #'(body ...) env))]
            [(do ([id initial step ...] ...) (test result ...) body ...)
             (let ([next (shadow (syntax->list #'(id ...)) env)])
               #`(do (#,@(map (lambda (id initial step)
                                #`(#,id #,(walk initial env) #,@(map (lambda (e) (walk e next)) (syntax->list step))))
                           (syntax->list #'(id ...)) (syntax->list #'(initial ...)) (syntax->list #'((step ...) ...))))
                   (#,(walk #'test next) #,@(map (lambda (e) (walk e next)) (syntax->list #'(result ...))))
                   #,@(walk-body #'(body ...) next)))]
            [(let name ([id value] ...) body ...)
             (identifier? #'name)
             #`(let name (#,@(map (lambda (id value) #`(#,id #,(walk value env)))
                               (syntax->list #'(id ...)) (syntax->list #'(value ...))))
                 #,@(walk-body #'(body ...) (shadow (cons #'name (syntax->list #'(id ...))) env)))]
            [(kind ((id value) ...) body ...)
             (memq (syntax->datum #'kind) '(let let* letrec letrec*))
             (let* ([names (syntax->list #'(id ...))] [values (syntax->list #'(value ...))]
                    [recursive? (memq (syntax->datum #'kind) '(letrec letrec*))]
                    [sequential? (eq? (syntax->datum #'kind) 'let*)]
                    [next (if recursive? (shadow names env) env)]
                    [bindings
                     (map (lambda (id value)
                            (let* ([scope (if (or recursive? sequential?) next env)]
                                   [code (walk value scope)] [t (if recursive? (unknown id) (template value scope))])
                              (set! next (cons (cons id #`(list 'bound '#,id #,t)) next)) #`(#,id #,code))) names values)])
               #`(kind (#,@bindings) #,@(walk-body #'(body ...) next)))]
            [(kind ((formals value) ...) body ...)
             (memq (syntax->datum #'kind) '(let-values let*-values))
             (let ([next env])
               (with-syntax ([(entry ...)
                              (map (lambda (f v)
                                     (let ([code (walk v (if (eq? (syntax->datum #'kind) 'let*-values) next env))])
                                       (set! next (shadow (ids f) next)) #`(#,f #,code)))
                                (syntax->list #'(formals ...)) (syntax->list #'(value ...)))]
                             [(rest ...) (walk-body #'(body ...) next)])
                 #'(kind (entry ...) rest ...)))]
            [(operator . rest)
             (let ([p (and (identifier? #'operator) (not (binding #'operator env))
                        (property #'operator))])
               (if p
                 (let-values ([(args tail)
                               (syntax-case #'rest [apply]
                                 [((apply a ... tail)) (values (syntax->list #'(a ...)) #'tail)]
                                 [(a ...) (values (syntax->list #'(a ...)) #f)]
                                 [_ (syntax-violation 'forwarding "invalid forwarding call" e)])])
                   (set! collected
                     (cons #`(list #,(car p) (list #,@(map (lambda (a) (template a env)) args))
                               #,(if tail (template tail env) #'#f)) collected))
                   #`(admitted-forward
                       (operator #,@(if tail
                                      (list #`(apply #,@(map (lambda (a) (walk a env)) args) #,(walk tail env)))
                                      (map (lambda (a) (walk a env)) args)))))
                 (syntax-case #'rest []
                   [(argument ...) #`(#,(walk #'operator env) #,@(map (lambda (a) (walk a env)) (syntax->list #'(argument ...))))]
                   [_ e])))]
            [_ e]))
        (define (walk-body forms env)
          (define (flatten forms)
            (apply append
              (map (lambda (form)
                     (syntax-case form (begin)
                       [(begin body ...) (flatten (syntax->list #'(body ...)))]
                       [_ (list form)])) forms)))
          (let* ([forms (flatten (syntax->list forms))]
                 [names (filter values
                          (map (lambda (form)
                                 (syntax-case form (define)
                                   [(define (name . _) . _) #'name]
                                   [(define name _) #'name] [_ #f])) forms))])
            (map (lambda (form) (walk form (shadow names env))) forms)))
        (define (clause formals forms)
          (set! collected '())
          (let* ([code (walk-body forms (parameters formals))] [descriptions (reverse collected)])
            (values #`(#,formals #,@code) #`(cons '#,formals (list #,@descriptions)))))
        (syntax-case x (lambda case-lambda)
          [(_ (lambda formals expression ...))
           (let-values ([(code description) (clause #'formals #'(expression ...))])
             (if (null? collected) #`(lambda . #,code)
               #`(attach-forwards! (lambda . #,code) (delay (list #,description)))))]
          [(_ (case-lambda [formals expression ...] ...))
           (let ([descriptions '()] [forwarding? #f])
             (let ([clauses (map (lambda (f b)
                                   (let-values ([(code description) (clause f b)])
                                     (when (pair? collected) (set! forwarding? #t))
                                     (set! descriptions (cons description descriptions)) code))
                              (syntax->list #'(formals ...)) (syntax->list #'((expression ...) ...)))])
               (if forwarding?
                 #`(attach-forwards! (case-lambda #,@clauses) (delay (list #,@(reverse descriptions))))
                 #`(case-lambda #,@clauses))))]
          ;; At top level there is no owning procedure to attach to. Admit
          ;; direct calls and describe nested procedures without introducing
          ;; a lambda scope around definitions or other declaration macros.
          [(_ form) (walk #'form '())]))))

  (define-syntax expression
    (syntax-rules (begin define define-syntax import library elibrary lambda case-lambda)
      [(_ (begin form ...)) (begin (expression form) ...)]
      [(_ (import spec ...)) (import spec ...)]
      [(_ (library . body)) (library . body)]
      [(_ (elibrary . body)) (elibrary . body)]
      [(_ (define-syntax . body)) (define-syntax . body)]
      [(_ form) (described-procedure form)]))

  ;;; Types ------------------------------------------------------------------------

  ;; A type: what a clause's symbol resolves to when its library initializes.
  ;; The language's types are predefined below; a library defines its own
  ;; with edoc-type. The editor's notions start as placeholders, prose alone
  ;; without an owner, so libraries that use them may initialize before the
  ;; one that defines them; the owner's definition replaces the placeholder.
  (define-record-type (type make-type type?)
    (fields (immutable name type-name-of) (immutable prose type-prose-of) (immutable predicate type-predicate-of)
            (immutable complete type-complete-of) (immutable read type-read-of) (immutable write type-write-of)
            (immutable owner type-owner-of) (immutable within type-within-of)
            (immutable portable? type-portable-of)))
  (define types (make-eq-hashtable))
  (define record-predicates (make-eq-hashtable))
  ;; The catalogue lets an already initialized library be retried after a
  ;; failed module load. Once the kernel starts, its transactional registry
  ;; is the authority; the catalogue is not an active-definition fallback.
  (define type-publish #f)
  (define type-lookup #f)
  (define type-observe #f)
  (define (lookup-type name)
    (if type-lookup (type-lookup name) (eq-hashtable-ref types name #f)))

  (define (register-type! name prose predicate complete read write owner within . portable)
    (let ([existing (lookup-type name)])
      (when (and existing (type-owner-of existing) (not (equal? (type-owner-of existing) owner)))
        (error 'edoc-type (format "type ~a is defined by ~a" name (type-owner-of existing)) owner))
      (let ([type (make-type name prose predicate complete read write owner
                    (if (eq? name 'integer) 'number within)
                    (or (and (pair? portable) (car portable))
                        (and (equal? owner "(foundation edoc)")
                          (memq name '(boolean string char integer number list pair vector bytevector symbol datum any)) #t)))])
        (when type-publish (type-publish name owner type))
        (eq-hashtable-set! types name type))))

  (define (register-record-type! name predicate)
    (eq-hashtable-set! record-predicates name predicate))

  (define (check-spec-types! spec name)
    ;; every type a recorded datum names is known by now
    (define (known? t)
      (cond [(symbol? t) (and (lookup-type t) #t)]
            [(eq? t #f) #t]
            [(and (pair? t) (list? t))
             (case (car t)
               [(one-of record) #t]
               [(or list-of values) (for-all known? (cdr t))]
               [else #t])]
            [else #t]))
    (when (pair? spec)
      (for-each
        (lambda (clause)
          ;; a flag, (prompts), (edits) or (effects kind), names no type
          (when (and (pair? clause) (symbol? (car clause)) (pair? (cdr clause))
                     (not (or (memq (car clause) '(prompts effects edits inspect public))
                              (and (eq? (car clause) 'receiver) (pair? (cddr clause)) (pair? (caddr clause)))))
                     (not (known? (cadr clause))))
            (error 'edoc (format "unknown edoc type ~s in the edoc of ~a" (cadr clause) name))))
        (cddr spec))))

  (define (plain-datum? value)
    ;; Memoize compound values so shared structure is linear and cycles are
    ;; rejected. An opaque leaf must never acquire a fake readable spelling.
    (let ([seen #f])
      (let walk ([v value])
        (if (or (pair? v) (vector? v))
          (begin
            (unless seen (set! seen (make-eq-hashtable)))
            (case (hashtable-ref seen v #f)
              [(done) #t] [(active) #f]
              [else
               (hashtable-set! seen v 'active)
               (and (if (pair? v) (and (walk (car v)) (walk (cdr v)))
                      (let loop ([i 0]) (or (= i (vector-length v)) (and (walk (vector-ref v i)) (loop (+ i 1))))))
                 (begin (hashtable-set! seen v 'done) #t))]))
          (or (null? v) (symbol? v) (string? v) (number? v) (boolean? v) (char? v) (bytevector? v))))))

  (define (always v) #t)

  (define base-types
    (begin
      (register-type! 'boolean "a boolean" boolean? (lambda (partial) '((#t #f #f) (#f #f #f))) #f #f "(foundation edoc)" #f)
      (for-each
        (lambda (entry) (register-type! (car entry) (cadr entry) (caddr entry) #f #f #f "(foundation edoc)" #f))
        (list (list 'string "a string" string?)
              (list 'char "a character" char?)
              (list 'integer "an exact integer" (lambda (v) (and (integer? v) (exact? v))))
              (list 'number "a number" number?)
              (list 'list "a proper list" list?)
              (list 'pair "a pair" pair?)
              (list 'vector "a vector" vector?)
              (list 'bytevector "a bytevector" bytevector?)
              (list 'hashtable "a hashtable" hashtable?)
              (list 'port "a port" port?)
              (list 'procedure "a procedure" procedure?)
              (list 'thunk "a procedure taking no arguments"
                    (lambda (v) (and (procedure? v) (logbit? 0 (procedure-arity-mask v)))))
              (list 'condition "a condition" condition?)
              (list 'symbol "a symbol" symbol?)
              (list 'datum "plain data: pairs, vectors, strings and atoms" plain-datum?)
              (list 'any "anything" always)))
      (for-each
        (lambda (entry) (register-type! (car entry) (cadr entry) always #f #f #f #f #f))
        '((file "a file, by its path") (directory "a directory, by its path") (buffer "a buffer")
          (window "a window") (region "a region of a buffer") (position "a (row . col) position")
          (command "a command") (key "a key spelling") (mode "a mode") (style "a face")
          (actor "an actor's identity") (head "a head's identity")))
      'registered))

  ;;; Checking, shared by the forms -------------------------------------------------

  (define-syntax edoc
    (lambda (x)
      (syntax-violation 'edoc "edoc annotates a definition inside an elibrary" x)))

  (define-syntax edoc-type
    (lambda (x)
      (syntax-violation 'edoc-type "edoc-type defines a type inside an elibrary" x)))

  ;; The forms that cannot document themselves record their edocs by hand;
  ;; the coverage tool reads these attach-name! definitions as documentation.
  (define edoc-type-documentation
    (attach-name! 'edoc-type
      '(edoc "Define a type for edoc clauses inside an elibrary: (edoc-type name prose (predicate p) (complete c) (read r) (write w) (within t) (portable #t)), all but the predicate optional; registered when the library initializes."
         (name symbol "the type's name")
         (prose string "what values of the type are")
         (field list "(predicate p), (complete c) giving (value label hint) entries; label and hint may be false for a partial text, (read r) text to value, (write w) value to expression text, (within t) the type this one refines, (portable #t) a pure bounded predicate for portable contracts")
         ("kind" syntax) ("library" "(foundation edoc)"))))

  (define edoc-documentation
    (attach-name! 'edoc
      '(edoc "The documentation form: a summary, then typed clauses. Inside an elibrary it annotates the definition that follows it, or names the definition it documents."
         (summary string "the description, its first sentence the short one")
         (clause list "(name type note ...) for a formal or field, (returns type note ...), (prompts) for a command that waits for input, (edits) for a buffer edit refused when read-only, (effects internal) for a query that fills a cache, (effects remote) for a transport whose effect is the message's, (public) for an intentional user or extension API even without repository callers, (inspect) for a bounded local query safe to evaluate during binding inspection")
         ("kind" syntax) ("library" "(foundation edoc)"))))

  (define expression-documentation
    (attach-name! 'expression
      '(edoc "Register forwarding in a Scheme expression or definition. Elibrary handles procedure bodies automatically; editor evaluation uses this form for interactive Scheme. A macro introducing forwarding must introduce this context too."
         (form any "expression, definition, import, library or begin")
         ("kind" syntax) ("library" "(foundation edoc)"))))

  (define forward-callee-documentation
    (attach-name! 'forward-callee
      '(edoc "Adapt a forwarding syntax identifier to its registered dispatcher for a structured call; ordinary procedure expressions pass through. Keymap:call uses this compiler adapter and keeps the dispatcher identity available to inspection."
         (name any "forwarding identifier or procedure expression")
         ("kind" syntax) ("library" "(foundation edoc)"))))

  (meta define (kept-datum doc extra library)
    ;; the datum recorded for introspection: (edoc summary clause ...), then
    ;; clauses no user form can write, since their heads are strings --
    ;; extra, and the defining library
    (append (list 'edoc) (syntax->datum doc) extra (if library (list (list "library" library)) '())))

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
         (unless (if (and (eq? (syntax->datum #'head) 'returns)
                          (list? t) (pair? t) (eq? (car t) 'values))
                     (for-all (lambda (t) (meta-type-ok? known-types t)) (cdr t))
                     (meta-type-ok? known-types t))
           (syntax-violation who "unknown edoc type" x #'type)))]
      [_ (syntax-violation who "expected an edoc clause (name type note ...)" x clause)]))

  (meta define (clause-head clause) (syntax-case clause () [(head . _) (syntax->datum #'head)]))
  (meta define (receiver-clause? clause)
    (let ([d (syntax->datum clause)])
      (and (pair? d) (eq? (car d) 'receiver) (pair? (cdr d)) (pair? (cddr d)) (pair? (caddr d)))))

  (meta define (flag-clause? clause)
    ;; (prompts), (effects internal) or (effects remote): what a procedure
    ;; does beyond its bang, named for the effects check; none names a formal
    (syntax-case clause ()
      [(head) (and (identifier? #'head) (memq (syntax->datum #'head) '(prompts edits inspect public)))]
      [(head kind) (and (identifier? #'head) (identifier? #'kind)
                        (eq? (syntax->datum #'head) 'effects) (memq (syntax->datum #'kind) '(internal remote)) #t)]
      [(head formal (category kind ...))
       (and (eq? (syntax->datum #'head) 'receiver) (identifier? #'formal)
         (pair? (syntax->list #'(kind ...))) (for-all identifier? (syntax->list #'(kind ...)))
         (memq (syntax->datum #'category) '(view model)) #t)]
      [_ #f]))

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
            (when (receiver-clause? #'clause)
              (syntax-violation who "a receiver requires procedure formals" x #'clause))
            (if (flag-clause? #'clause)
                (loop #'rest seen returns?)
                (begin
                  (check-clause-shape! who x #'clause)
                  (let ([head (clause-head #'clause)])
                    (cond
                      [(eq? head 'returns)
                       (when returns? (syntax-violation who "one returns clause at most" x #'clause))
                       (loop #'rest seen #t)]
                      [else
                       (when (memq head seen) (syntax-violation who "one edoc clause per name" x #'clause))
                       (loop #'rest (cons head seen) returns?)]))))]))]))

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
           (let ([receivers (filter receiver-clause? (syntax->list #'(clause ...)))])
             (unless (<= (length receivers) 1) (syntax-violation who "one receiver clause at most" x doc))
             (for-each
               (lambda (c)
                 (unless (flag-clause? c) (syntax-violation who "expected (receiver formal (view-or-model kind))" x c))
                 (let* ([name (cadr (syntax->datum c))] [entry (assq name names)]
                        [argument (find (lambda (a) (and (not (receiver-clause? a)) (eq? (clause-head a) name))) (syntax->list #'(clause ...)))])
                   (unless (and entry (not (rest? name)))
                     (syntax-violation who "a receiver names a non-rest formal" x c))
                   (unless (and argument (eq? (cadr (syntax->datum argument)) 'model))
                     (syntax-violation who "a receiver formal has type model" x c)))) receivers))
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
               [(clause . rest)
                (flag-clause? #'clause)
                (loop #'rest seen returns?)]
               [((head type . notes) . rest)
                (identifier? #'head)
                (let ([t (syntax->datum #'type)] [name (syntax->datum #'head)])
                  (check-clause-shape! who x #'(head type . notes))
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
                   ;; procedure drops its -record suffix in prose
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
             [instance (list (string->symbol noun) (list 'record type-name))]
             [made (list 'returns (list 'record type-name))]
             [attachment
              (lambda (object summary clauses kind)
                (cons object (append (list 'edoc summary) clauses (list (list "kind" kind)) tail)))])
        (append
          (list (attachment type summary clauses 'record))
          (cond
            [plain-constructor? (list (attachment constructor summary (append clauses (list made)) 'constructor))]
            [(and constructor constructor-clause)
             (list (attachment constructor summary
                     (append (map (lambda (f) (assq f clauses)) (cdr (syntax->datum constructor-clause))) (list made))
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
                 #'(attach-named-definition! object 'datum)
                 #'(attach! 'object object 'datum))))
         attachments))

  ;;; The documented library ------------------------------------------------------

  (define-syntax elibrary
    (lambda (x)
      (define who 'elibrary)
      (define (edoc-form? form) (head-is? form 'edoc))
      (define (type-form? form) (head-is? form 'edoc-type))
      (define (type-registration form library-name)
        ;; (edoc-type name prose field ...) as the expression registering it
        (syntax-case form ()
          [(_ name prose field ...)
           (and (identifier? #'name) (string? (syntax->datum #'prose)))
           (let ([fields (syntax->list #'(field ...))])
             (define (field-of key)
               (let ([hit (find (lambda (f) (head-is? f key)) fields)])
                 (and hit (syntax-case hit () [(_ e) #'e] [_ (syntax-violation who "expected (field expression)" form hit)]))))
             (for-each
               (lambda (f)
                 (unless (exists (lambda (key) (head-is? f key)) '(predicate complete read write within portable))
                   (syntax-violation who "expected a predicate, complete, read, write, within or portable field" form f)))
               fields)
             (unless (field-of 'predicate) (syntax-violation who "a type needs a predicate" form))
             (with-syntax ([predicate (field-of 'predicate)]
                           [complete (or (field-of 'complete) #'#f)]
                           [portable (let ([p (field-of 'portable)])
                                       (cond [(not p) #'#f]
                                             [(boolean? (syntax->datum p)) p]
                                             [else (syntax-violation who "portable must be a boolean" form p)]))]
                           [read (or (field-of 'read) #'#f)]
                           [write (or (field-of 'write) #'#f)]
                           [within (let ([w (field-of 'within)])
                                     (cond [(not w) #'#f]
                                           [(identifier? w) (list #'quote w)]
                                           [else (syntax-violation who "within names a type" form w)]))]
                           [library library-name])
               #'(register-type! 'name prose predicate complete read write library within portable)))]
          [_ (syntax-violation who "expected (edoc-type name prose (predicate p) field ...)" form)]))
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
          [(d (name . formals) body ...) (and (or (define? form) (head-is? form 'define-operation)) (identifier? #'name))
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
          [(df (name . formals) implementation inspector) (head-is? form 'define-forwarding)
           (list 'forwarding (list #'name) (list #'formals))]
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
            [(procedure forwarding)
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
         (let ([library-id #'name] [library-name (format "~s" (syntax->datum #'name))]
               [exported (export-identifiers #'exports)])
           ;; pair every annotation with the definition that follows it
           (let walk ([forms (syntax->list #'(body ...))] [pending #f] [kept '()] [entries '()] [named '()] [registrations '()])
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
                                       [(syntax forwarding) (list (car (cadr info)))]
                                       [(record condition) (list (car (cadr info)))]
                                       [else '()])))
                                 documented))]
                         [operation-id (car (generate-temporaries '(operation-implementation)))])
                    (with-syntax ([operation-implementation operation-id]
                                  [(query-registration ...)
                                   (apply append
                                     (map (lambda (entry)
                                            (let ([doc (cadr entry)] [info (car entry)])
                                              (if (and doc (eq? (car info) 'procedure)
                                                    (member '(inspect) (syntax->datum doc)))
                                                (list (with-syntax ([id (car (cadr info))])
                                                        #'(define-property id inspection-property #t))) '()))) documented))]
                                  [(form ...)
                                   (map (lambda (form)
                                          (syntax-case form ()
                                            [(d (name . formals) body ...) (head-is? form 'define-operation)
                                             (begin
                                               (unless (syntax->list #'formals)
                                                 (syntax-violation who "base operations require fixed positional arguments" form))
                                               (with-syntax ([op operation-id])
                                                 #'(define name (op name formals (described-procedure (lambda formals body ...))))))]
                                            [(d (name . formals) body ...) (head-is? form 'define)
                                             #'(define name (described-procedure (lambda formals body ...)))]
                                            [(d name value) (and (head-is? form 'define)
                                                                 (or (head-is? #'value 'lambda) (head-is? #'value 'case-lambda)))
                                             #'(define name (described-procedure value))]
                                            [(d (name . formals) implementation inspector) (head-is? form 'define-forwarding)
                                             (with-syntax ([public-name (datum->syntax #'name
                                                                          (string->symbol (format "~a:~a" (car (reverse (syntax->datum library-id))) (syntax->datum #'name))))])
                                               #'(forwarding-definition name 'public-name implementation inspector))]
                                            [_ form])) (reverse kept))]
                                  [(registration ...) (reverse registrations)]
                                  [(forward-registration ...)
                                   (apply append
                                     (map (lambda (entry)
                                            (let ([info (car entry)])
                                              (if (eq? (car info) 'forwarding)
                                                (list (with-syntax ([id (car (cadr info))]) #'(install-named-forward! id))) '()))) documented))]
                                  [(record-registration ...)
                                   ;; a documented record's predicate stands for (record name)
                                   (apply append
                                     (map (lambda (entry)
                                            (let ([info (car entry)])
                                              (if (and (eq? (car info) 'record) (cadr entry))
                                                  (let* ([parts (record-parts who x (caddr info) (cadddr info))]
                                                         [type (car parts)] [predicate (caddr parts)])
                                                    (if predicate
                                                        (list (with-syntax ([t type] [p predicate]) #'(register-record-type! 't p)))
                                                        '()))
                                                  '())))
                                          documented))]
                                  [(attachment ...) (attachment-forms attachments by-name)]
                                  [(tmp) (generate-temporaries '(edocs))])
                      (with-syntax ([imports (if (exists (lambda (form) (head-is? form 'define-operation)) kept)
                                               (syntax-case #'imports []
                                                 [(i spec ...) #'(i spec ... (rename (only (core operation) implementation) (implementation operation-implementation)))])
                                               #'imports)])
                        #'(library name exports imports
                            form ...
                            query-registration ...
                            (define tmp (begin registration ... record-registration ... forward-registration ... attachment ... (void))))))))]
               [(edoc-form? (car forms))
                (syntax-case (car forms) ()
                  [(_ summary clause ...) (string? (syntax->datum #'summary))
                   (begin
                     (when pending (syntax-violation who "two edocs annotate one definition" (car forms)))
                     (walk (cdr forms) (car forms) kept entries named registrations))]
                  [(_ id summary clause ...) (and (identifier? #'id) (string? (syntax->datum #'summary)))
                   (walk (cdr forms) pending kept entries (cons (cons #'id (car forms)) named) registrations)]
                  [_ (syntax-violation who "expected (edoc summary clause ...) or (edoc name summary clause ...)" (car forms))])]
               [(type-form? (car forms))
                (when pending (syntax-violation who "an edoc annotates the definition that follows it" pending))
                (walk (cdr forms) #f kept entries named (cons (type-registration (car forms) library-name) registrations))]
               [else
                (let ([info (definition-info (car forms))])
                  (cond
                    [info (walk (cdr forms) #f (cons (car forms) kept) (cons (list info pending) entries) named registrations)]
                    [pending (syntax-violation who "an edoc annotates the definition that follows it" pending)]
                    [else (walk (cdr forms) #f (cons (car forms) kept) entries named registrations)]))])))]
        [_ (syntax-violation who "expected (elibrary (name) (export ...) (import ...) body ...)" x)])))

  (define elibrary-documentation
    (attach-name! 'elibrary
      '(edoc "A library whose exports are documented: an edoc annotates the definition that follows it, and every export the body defines must have one."
         (name list "the library name")
         (exports list "the export clause")
         (imports list "the import clause")
         (body any "the definitions, annotated")
         ("kind" syntax) ("library" "(foundation edoc)"))))

  ;;; This library's own definitions --------------------------------------------------

  ;; (edoc) cannot be an elibrary, since it defines the form; these two
  ;; forms document its procedures and records the same way, with the same
  ;; checks, attaching at initialization.

  (define-syntax edefine
    (lambda (x)
      (syntax-case x (edoc)
        [(_ (name . formals) (edoc . doc) body ...)
         (begin
           (check-formals! 'edefine x (list #'formals) #'doc)
           (with-syntax ([datum (datum->syntax #'edoc
                                  (kept-datum #'doc (list (list "kind" 'procedure) (list "formals" (syntax->datum #'formals))) "(foundation edoc)"))]
                         [(tmp) (generate-temporaries '(edoc))])
             #'(begin
                 (define (name . formals) body ...)
                 (define tmp (attach! 'name name 'datum)))))]
        [(_ name (edoc . doc) expression)
         (begin
           (check-value-doc! 'edefine x #'doc)
           (with-syntax ([datum (datum->syntax #'edoc (kept-datum #'doc (list (list "kind" 'value)) "(foundation edoc)"))])
             #'(define name (attach! 'name expression 'datum))))])))

  (define-syntax edefine-record-type
    (lambda (x)
      (syntax-case x (edoc)
        [(_ spec (edoc . doc) body ...)
         (let ([attachments (record-attachments 'edefine-record-type x #'spec #'doc (syntax->list #'(body ...)) "(foundation edoc)")]
               [type (car (record-parts 'edefine-record-type x #'spec (syntax->list #'(body ...))))])
           (with-syntax ([(attachment ...) (attachment-forms attachments (list type))]
                         [(tmp) (generate-temporaries '(edoc))])
             #'(begin
                 (define-record-type spec body ...)
                 (define tmp (begin attachment ... (void))))))])))

  ;;; Reading it back -------------------------------------------------------------

  (edefine (forwarding-name procedure)
    (edoc "The public syntax name of a registered forwarding dispatcher, or false."
          (procedure procedure "dispatcher") (returns (or symbol #f)))
    (eq-hashtable-ref forward-names procedure #f))

  (edefine (inspection-value procedure arguments)
    (edoc "Reduce a call only when it is an explicitly inspectable query or a small structural primitive, with all arguments known. Nodes are (value datum) or (unknown expression); exceptions stay symbolic. Inspection never runs an ordinary command."
          (procedure procedure "query") (arguments list "symbolic argument nodes") (returns list))
    (if (and (or (eq-hashtable-ref inspection-queries procedure #f)
                 (memq procedure (list car cdr cadr caddr cadddr null? pair? list cons append not equal? eq?)))
             (for-all (lambda (a) (eq? (car a) 'value)) arguments))
      (guard (ex [else '(unknown unavailable)])
        (list 'value (apply procedure (map cadr arguments))))
      '(unknown computed)))

  (edefine (forwarding-steps procedure arguments)
    (edoc "Inspect one forwarding step using compile-time call templates and local dispatch inspectors. Each result is (procedure argument-nodes rest-node-or-false reason-or-false). Multiple sites are alternatives, not an execution trace."
          (procedure procedure "command") (arguments list "known or symbolic argument nodes") (returns list))
    (define (reduce node)
      (case (car node)
        [(value unknown) node]
        [(argument) (if (< (cadr node) (length arguments)) (list-ref arguments (cadr node)) (list 'unknown (caddr node)))]
        [(rest) (let ([tail (list-tail arguments (min (cadr node) (length arguments)))])
                  (if (for-all (lambda (a) (eq? (car a) 'value)) tail)
                    (list 'value (map cadr tail)) (list 'unknown (caddr node))))]
        [(bound) (let ([v (reduce (caddr node))]) (if (eq? (car v) 'value) v (list 'unknown (cadr node))))]
        [(if) (let ([test (reduce (cadr node))])
                (if (eq? (car test) 'value) (reduce (if (cadr test) (caddr node) (cadddr node))) '(unknown conditional)))]
        [(query) (inspection-value (cadr node) (map reduce (caddr node)))]
        [else '(unknown computed)]))
    (define (accepts? formals count)
      (cond [(null? formals) (= count 0)] [(symbol? formals) #t]
        [(> count 0) (accepts? (cdr formals) (- count 1))] [else #f]))
    (let ([inspect (eq-hashtable-ref forward-inspectors procedure #f)])
      (if inspect (inspect arguments)
        (let* ([saved (eq-hashtable-ref forwards procedure #f)]
               [description (find (lambda (d) (accepts? (car d) (length arguments))) (if saved (force saved) '()))])
          (if (not description) '()
            (map (lambda (site)
                   (let* ([args (map reduce (cadr site))] [tail (and (caddr site) (reduce (caddr site)))])
                     (if (and tail (eq? (car tail) 'value) (list? (cadr tail)))
                       (list (car site) (append args (map (lambda (v) (list 'value v)) (cadr tail))) #f #f)
                       (list (car site) args tail #f)))) (cdr description)))))))

  (edefine edoc-types
    (edoc "The base type vocabulary: the editor's notions, then the language's; libraries add their own with edoc-type, and compounds are (one-of literal ...), (or type ...), (list-of type) and (record name)."
          (value (list-of symbol)))
    (vocabulary))

  (edefine (edoc-type? t)
    (edoc "Whether a value is an edoc type: a registered name, #f, or a compound over them." (t datum "the value") (returns boolean))
    (type-ok? (lambda (name) (and (lookup-type name) #t)) t))

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
          (library (or string #f) "the defining library, (edit) say")
          (flags (list-of list) "the declarations beyond the bang: (prompts), (edits), (effects internal), (effects remote), (inspect), (public)"))
    (fields kind formals summary arguments returns library flags))
  (edefine (signature-receiver sig)
    (edoc "The explicit contextual receiver declaration (formal (view-or-model kind)), or false. It describes discovery, without changing Scheme invocation."
          (sig (record signature) "documented signature") (returns (or list #f)))
    (cond [(assq 'receiver (signature-flags sig)) => cdr] [else #f]))
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
         (let loop ([clauses (cddr spec)] [arguments '()] [returns #f] [library library] [kind kind] [lambda-lists #f] [flags '()])
           (cond
             [(null? clauses)
              (let ([arguments (reverse arguments)] [summary (cadr spec)] [flags (reverse flags)])
                (if lambda-lists
                    (map (lambda (f)
                           (let ([names (formal-symbols f)])
                             (make-signature kind f summary
                               (filter (lambda (a) (memq (argument-name a) names)) arguments) returns library
                               (filter (lambda (f) (or (not (eq? (car f) 'receiver)) (memq (cadr f) names))) flags))))
                         lambda-lists)
                    (list (make-signature kind formals summary arguments returns library flags))))]
             [(and (pair? (car clauses))
                   (or (memq (caar clauses) '(prompts effects edits inspect public))
                       (and (eq? (caar clauses) 'receiver) (pair? (cdar clauses))
                         (pair? (cddar clauses)) (pair? (caddar clauses)))))
              (loop (cdr clauses) arguments returns library kind lambda-lists (cons (car clauses) flags))]
             [(and (pair? (car clauses)) (pair? (cdar clauses)))
              (let ([c (car clauses)])
                (cond
                  [(equal? (car c) "library") (loop (cdr clauses) arguments returns (cadr c) kind lambda-lists flags)]
                  [(equal? (car c) "kind") (loop (cdr clauses) arguments returns library (cadr c) lambda-lists flags)]
                  [(equal? (car c) "formals") (loop (cdr clauses) arguments returns library kind (cdr c) flags)]
                  [(eq? (car c) 'returns)
                   (loop (cdr clauses) arguments (make-argument 'returns (cadr c) (cddr c)) library kind lambda-lists flags)]
                  [else
                   (loop (cdr clauses) (cons (make-argument (car c) (cadr c) (cddr c)) arguments)
                         returns library kind lambda-lists flags)]))]
             [else #f]))))

  (define (attached-signatures spec procedure?)
    ;; the signatures of a recorded datum; a procedure documented as a value
    ;; is a procedure whose formals are its argument names
    (let ([sigs (spec-signatures 'value '() spec #f)])
      (and sigs
           (map (lambda (sig)
                  (if (and procedure? (eq? (signature-kind sig) 'value))
                      (make-signature 'procedure (map argument-name (signature-arguments sig))
                        (signature-summary sig) (signature-arguments sig) (signature-returns sig)
                        (signature-library sig) (signature-flags sig))
                      sig))
                sigs))))

  (edefine (edoc-of object)
    (edoc "The signatures an object was documented with, or #f."
          (object any "the procedure, parameter or value")
          (returns (or (list-of (record signature)) #f)))
    (let ([spec (eq-hashtable-ref attached object #f)])
      (and spec (attached-signatures spec (procedure? object)))))

  (edefine (edoc-named name)
    (edoc "The signatures recorded under a name: a keyword's, a record or condition type's, or a value's without identity; or #f."
          (name symbol "the name")
          (returns (or (list-of (record signature)) #f)))
    (let ([spec (eq-hashtable-ref named name #f)])
      (and spec (attached-signatures spec #f))))

  (edefine (type-named name)
    (edoc "The type record registered under a name, or #f." (name symbol "the type's name") (returns (or (record type) #f)))
    (lookup-type name))

  (edefine (install-type-registry! publish lookup observe)
    (edoc "Install the kernel's transactional type registry once during bootstrap, seeding library declarations already initialized."
          (publish procedure "(name library type)") (lookup procedure "name to active type")
          (observe procedure "register a module-owned change observer"))
    (when type-publish (error 'install-type-registry! "type registry already installed"))
    (set! type-publish publish) (set! type-lookup lookup) (set! type-observe observe)
    (vector-for-each (lambda (name) (let ([type (eq-hashtable-ref types name #f)])
                                      (publish name (type-owner-of type) type))) (hashtable-keys types)))

  (edefine (restore-types! library)
    (edoc "Republish an imported library's declarations after module retraction or a failed initialization."
          (library string "qualified library spelling"))
    (when type-publish
      (vector-for-each (lambda (name)
                         (let ([type (eq-hashtable-ref types name #f)])
                           (when (and (equal? library (type-owner-of type)) (not (eq? type (lookup-type name))))
                             (type-publish name library type)))) (hashtable-keys types))))

  (edefine (observe-types! proc)
    (edoc "Observe committed type-definition changes through the kernel's module-owned registry."
          (proc procedure "(removed added) definition entries") (returns any))
    (unless type-observe (error 'observe-types! "kernel type registry is not installed"))
    (type-observe proc))

  (edefine (type-portable? t)
    (edoc "Whether a concrete type declares a pure contract over portable data; values still need independent portable-data validation."
          (t datum "type expression") (returns boolean))
    (and (edoc-type? t)
      (let walk ([t t] [seen '()])
        (cond [(eq? t #f) #t]
              [(symbol? t) (let ([type (lookup-type t)])
                             (and type (type-owner-of type) (type-portable-of type)
                               (not (memq t seen))
                               (or (not (type-within-of type)) (walk (type-within-of type) (cons t seen))) #t))]
              [(pair? t) (case (car t)
                           [(one-of) #t]
                           [(or list-of) (for-all (lambda (part) (walk part seen)) (cdr t))]
                           [else #f])]
              [else #f]))))

  (edefine (type-compatible? from to)
    (edoc "Whether a portable producer type conservatively fits a consumer: equality, declared refinements, finite literals, unions and covariant lists."
          (from datum "producer type") (to datum "consumer type") (returns boolean))
    (and (type-portable? from) (type-portable? to)
      (let fits ([a from] [b to])
        (cond [(equal? a b) #t]
              [(memq b '(datum any)) #t]
              [(and (pair? a) (eq? (car a) 'or)) (for-all (lambda (part) (fits part b)) (cdr a))]
              [(and (pair? a) (eq? (car a) 'one-of)) (for-all (lambda (v) (type-accepts? b v)) (cdr a))]
              [(eq? a #f) (type-accepts? b #f)]
              [(and (pair? b) (eq? (car b) 'or)) (exists (lambda (part) (fits a part)) (cdr b))]
              [(and (pair? a) (eq? (car a) 'list-of))
               (or (eq? b 'list) (and (pair? b) (eq? (car b) 'list-of) (fits (cadr a) (cadr b))))]
              [(symbol? a) (let* ([type (lookup-type a)] [parent (and type (type-within-of type))])
                             (and parent (fits parent b)))]
              [else #f]))))

  (edefine (type-owner type)
    (edoc "The library that defined a type, (literal) say, or #f for a placeholder." (type (record type) "the type record") (returns (or string #f)))
    (type-owner-of type))

  (edefine (type-within type)
    (edoc "The type this one refines, head within actor say, or #f." (type (record type) "the type record") (returns (or symbol #f)))
    (type-within-of type))

  (edefine (type-denotes-record? t name)
    (edoc "Whether a named type denotes a documented record: its predicate is the record's."
          (t symbol "the type's name") (name symbol "the record type's name") (returns boolean))
    (let ([type (type-named t)] [predicate (eq-hashtable-ref record-predicates name #f)])
      (and type predicate (eq? (type-predicate-of type) predicate))))

  (edefine (type-value name v)
    (edoc "Validate a typed value; the legacy window reader temporarily resolves its selector. No constructors are generated."
      (name symbol "type name") (v any "value or legacy selector") (returns any) (effects internal))
    (if (type-accepts? name v) v
      (let* ([type (type-named name)] [reader (and type (type-read-of type))]
             [value (and reader (reader v))])
        (unless (and reader (type-accepts? name value))
          (error 'type-value "value does not satisfy the type" name v))
        value)))

  (edefine (type-accepts? t value)
    (edoc "Whether a value satisfies a type: its predicate for a name, membership for a one-of, any member for an or, every element for a list-of, the record's predicate for (record name); an unknown name accepts anything."
          (t datum "the type") (value any "the value") (returns boolean))
    (cond
      [(symbol? t)
       (let ([type (type-named t)])
         (if type (and (guard (ex [else #f]) ((type-predicate-of type) value)) #t) #t))]
      [(eq? t #f) (eq? value #f)]
      [(and (pair? t) (list? t))
       (case (car t)
         [(one-of) (and (member value (cdr t)) #t)]
         [(or) (exists (lambda (m) (type-accepts? m value)) (cdr t))]
         [(list-of) (and (list? value) (for-all (lambda (x) (type-accepts? (cadr t) x)) value))]
         [(record) (let ([p (eq-hashtable-ref record-predicates (cadr t) #f)]) (if p (and (p value) #t) #t))]
         [else #t])]
      [else #t]))

  (edefine (call-argument-type signatures index)
    (edoc "The documented type at a zero-based call argument, including rest elements and the union of overloaded signatures; false when undocumented."
          (signatures (or list #f) "edoc signatures") (index integer "argument position") (returns datum))
    (define (formal-at formals index)
      (cond [(pair? formals) (if (= index 0) (cons (car formals) #f) (formal-at (cdr formals) (- index 1)))]
        [(symbol? formals) (cons formals #t)] [else #f]))
    (unless (and (integer? index) (exact? index) (>= index 0)) (error 'call-argument-type "expected a nonnegative argument position" index))
    (let ([types
           (fold-left
             (lambda (types sig)
               (let* ([formals (case (signature-kind sig)
                                 [(procedure) (signature-formals sig)]
                                 [(constructor accessor mutator predicate syntax) (map argument-name (signature-arguments sig))]
                                 [else #f])]
                      [formal (and formals (formal-at formals index))]
                      [argument (and formal (find (lambda (a) (eq? (argument-name a) (car formal))) (signature-arguments sig)))])
                 (if (not argument) types
                   (let* ([type (argument-type argument)]
                          [type (if (and (cdr formal) (pair? type) (eq? (car type) 'list-of)) (cadr type) type)])
                     (if (member type types) types (cons type types)))))) '() (or signatures '()))])
      (cond [(null? types) #f] [(null? (cdr types)) (car types)] [else (cons 'or (reverse types))])))

  (edefine (type-completions t partial)
    (edoc "The values a type offers for a partial text, as (value label hint) entries; label and hint may be false: a completer's for a name, the literals of a one-of, every member's for an or, #f for #f."
          (t datum "the type") (partial string "the text typed so far") (returns list))
    (cond
      [(symbol? t)
       (let ([type (type-named t)])
         (if (and type (type-complete-of type))
             (guard (ex [else '()]) ((type-complete-of type) partial))
             '()))]
      [(eq? t #f) (list (list #f #f #f))]
      [(and (pair? t) (list? t))
       (case (car t)
         [(one-of) (map (lambda (literal) (list literal #f #f)) (cdr t))]
         [(or) (apply append (map (lambda (m) (type-completions m partial)) (cdr t)))]
         [else '()])]
      [else '()]))

  (edefine (type-spelling t value)
    (edoc "A value as a Scheme expression, independent of its type for plain data. Opaque values retain a legacy type writer or a diagnostic spelling."
          (t datum "the type") (value any "the value") (returns string))
    (define (default v) (format "~s" v))
    (cond
      [(value-expression value) => values]
      [(symbol? t)
       (let ([type (type-named t)])
         (if (and type (type-write-of type))
             (guard (ex [else (default value)]) ((type-write-of type) value))
             (default value)))]
      [(and (pair? t) (eq? (car t) 'or))
       (let ([m (find (lambda (m) (type-accepts? m value)) (cdr t))])
         (if m (type-spelling m value) (default value)))]
      [else (default value)]))

  (edefine (value-expression value)
    (edoc "A reconstructible Scheme expression for finite plain data, or false for cyclic or opaque runtime values. Quote compound data and symbols once; metadata and resource availability never affect spelling."
      (value any "value to spell") (returns (or string #f)))
    (and (plain-datum? value)
      (if (or (symbol? value) (pair? value) (null? value) (vector? value))
        (format "'~s" value) (format "~s" value))))

  (edefine (type-prose t)
    (edoc "A type as prose: a name's own description, else type-text." (t datum "the type") (returns string) (public))
    (let ([type (and (symbol? t) (type-named t))])
      (if type (type-prose-of type) (type-text t))))

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
