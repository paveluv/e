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
;; most once. edoc outside edefine is a syntax error.
;;
;; edoc-of reads a procedure's signatures back, and edoc-entry shapes them
;; as a documentation entry in the describe corpus's eight-field format.

(library (edoc)
  (export edefine edoc
          edoc-of signature? signature-formals signature-summary signature-arguments
          signature-returns signature-library
          argument? argument-name argument-type argument-notes
          edoc-types edoc-type? type-text edoc-entry edoc-template first-sentence)
  (import (rnrs)
          (only (chezscheme) meta inspect/object make-weak-eq-hashtable
                eq-hashtable-ref eq-hashtable-set! format path-last
                syntax->annotation annotation-source source-object-sfd source-file-descriptor-path))

  ;;; The vocabulary, for the expander and for run time ---------------------------

  (define-syntax vocabulary
    (syntax-rules ()
      [(_) '(file directory buffer window command symbol key mode style
              string integer boolean procedure any)]))

  (define-syntax type-checker
    ;; (checker types t): is t a type over the vocabulary types? A symbol of
    ;; the vocabulary, (one-of literal ...), (or type type ...) or (list-of type).
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
                [else #f])]
             [else #f])))]))

  (meta define known-types (vocabulary))
  (meta define meta-type-ok? (type-checker))
  (define edoc-types (vocabulary))
  (define type-ok? (type-checker))
  (define (edoc-type? t) (type-ok? edoc-types t))

  ;;; The forms ---------------------------------------------------------------------

  (define-syntax edoc
    (lambda (x)
      (syntax-violation 'edoc "edoc belongs at the head of an edefine body" x)))

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
      (define (source-library)
        ;; the library this edefine appears in, from the form's source file,
        ;; or #f when it was not read from a file
        (let ([annotation (syntax->annotation x)])
          (and annotation
               (let* ([path (source-file-descriptor-path (source-object-sfd (annotation-source annotation)))]
                      [file (path-last path)]
                      [n (string-length file)])
                 (and (> n 4) (string=? (substring file (- n 4) n) ".sls")
                      (string-append "(" (substring file 0 (- n 4)) ")"))))))
      (define (spec doc)
        ;; the datum kept in the body: the edoc clauses, then the library as
        ;; a clause no user form can write, since its head is a string
        (let ([library (source-library)])
          (datum->syntax #'edefine
            (append (syntax->datum doc) (if library (list (list "library" library)) '())))))
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
        [_ (syntax-violation 'edefine
             "expected (edefine (name . formals) (edoc summary clause ...) body ...) or a case-lambda whose clauses open with edoc"
             x)])))

  ;;; Reading it back -------------------------------------------------------------

  (define-record-type signature (fields formals summary arguments returns library))
  (define-record-type argument (fields name type notes))

  (define (clause-signature formals body library)
    ;; The signature of one clause whose body opens with a quoted edoc datum.
    (and (pair? body)
         (let ([first (car body)])
           (and (pair? first) (eq? (car first) 'quote) (pair? (cdr first))
                (let ([spec (cadr first)])
                  (and (pair? spec) (eq? (car spec) 'edoc) (pair? (cdr spec)) (string? (cadr spec))
                       (let loop ([clauses (cddr spec)] [arguments '()] [returns #f] [library library])
                         (cond
                           [(null? clauses)
                            (make-signature formals (cadr spec) (reverse arguments) returns library)]
                           [(and (pair? (car clauses)) (pair? (cdar clauses)))
                            (let ([c (car clauses)])
                              (cond
                                [(equal? (car c) "library") (loop (cdr clauses) arguments returns (cadr c))]
                                [(eq? (car c) 'returns)
                                 (loop (cdr clauses) arguments (make-argument 'returns (cadr c) (cddr c)) library)]
                                [else
                                 (loop (cdr clauses) (cons (make-argument (car c) (cadr c) (cddr c)) arguments)
                                       returns library)]))]
                           [else #f]))))))))

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

  (define (edoc-of proc)
    ;; The signatures proc was edefined with, one per clause, or #f.
    (and (procedure? proc)
         (let ([hit (eq-hashtable-ref cache proc #f)])
           (cond [(eq? hit 'none) #f]
                 [hit hit]
                 [else (let ([found (read-signatures proc)])
                         (eq-hashtable-set! cache proc (or found 'none))
                         found)]))))

  ;;; Presenting ------------------------------------------------------------------

  (define (type-text t)
    ;; a type as prose: file, one of utf-8 or latin-1, string or #f, list of buffer
    (cond
      [(symbol? t) (symbol->string t)]
      [(and (pair? t) (eq? (car t) 'one-of))
       (string-append "one of " (join (map (lambda (x) (format "~s" x)) (cdr t)) " or "))]
      [(and (pair? t) (eq? (car t) 'or)) (join (map type-text (cdr t)) " or ")]
      [(and (pair? t) (eq? (car t) 'list-of)) (string-append "list of " (type-text (cadr t)))]
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

  (define (edoc-entry name proc)
    ;; A describe entry -- (names forms returns libraries source chapter url
    ;; description) -- for an edefined procedure bound to name, or #f.
    (let ([sigs (edoc-of proc)])
      (and sigs
           (let* ([returns (find values (map signature-returns sigs))]
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
                                 (if (memq (argument-name a) seen) (inner (cdr arguments) seen out)
                                     (inner (cdr arguments) (cons (argument-name a) seen)
                                       (cons (format "- `~a` (~a)~a" (argument-name a) (type-text (argument-type a))
                                               (if (pair? (argument-notes a))
                                                   (string-append ": " (join (argument-notes a) " ")) ""))
                                             out))))))))])
             (list (list name)
                   (map (lambda (sig) (cons "procedure" (edoc-template name (signature-formals sig)))) sigs)
                   (and returns
                        (string-append (type-text (argument-type returns))
                                       (if (pair? (argument-notes returns))
                                           (string-append ": " (join (argument-notes returns) " ")) "")))
                   (if library (list library) '())
                   'edoc "Documented definitions" #f
                   (join (append summaries
                                 (if (pair? argument-lines) (cons "" argument-lines) '()))
                         "\n"))))))
)
