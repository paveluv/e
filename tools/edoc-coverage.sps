#!/usr/bin/env scheme-script
;; edoc-coverage.sps -- which exported names carry an edoc, library by
;; library, read from the sources:
;;
;;   tools/edoc-coverage.sps [--list] [library ...]
;;
;; Every export is classified by its definition: documented (an edoc
;; annotation in an elibrary), procedure (define with a lambda list, or a
;; lambda/case-lambda value), parameter, syntax, record (constructor,
;; predicate or field procedure of a define-record-type), value (any other
;; define), standard (a name of (rnrs) or (chezscheme) passed on), or
;; elsewhere (a name this scan cannot place). A re-exported import, or an
;; alias of one, is classified where it is defined, through the import
;; specs.
;; --list prints the undocumented names of each library; library
;; arguments restrict the report to those stems. The types every edoc
;; clause names are checked against the base vocabulary and the edoc-type
;; forms of the whole tree; unknown ones are reported last.
;;
;;   tools/edoc-coverage.sps --effects [--internal] [library ...]
;;
;; checks the bang instead: a name ending in ! changes state, one without
;; is a query. Every definition's body is walked for the effects it
;; reaches, directly or through the tree's own definitions, and three
;; disagreements are reported: a ! name reaching no effect, a bangless
;; name reaching one without an (effects internal) clause, and a name
;; reaching a prompt without a (prompts) clause. Documented definitions
;; are checked; --internal adds the rest. The exit status is the number
;; of disagreements, so the suite can run it.
(import (chezscheme))

(define (read-forms path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([out '()])
        (let ([form (read port)])
          (if (eof-object? form) (reverse out) (loop (cons form out))))))))

(define (sls-files directory)
  (let loop ([names (directory-list directory)] [acc '()])
    (cond [(null? names) acc]
          [(file-directory? (string-append directory "/" (car names)))
           (loop (cdr names) (append (sls-files (string-append directory "/" (car names))) acc))]
          [(let* ([name (car names)] [n (string-length name)])
             (and (> n 4) (string=? (substring name (- n 4) n) ".sls")))
           (loop (cdr names) (cons (string-append directory "/" (car names)) acc))]
          [else (loop (cdr names) acc)])))

(define (exports-of library)
  ;; ((external . internal) ...) from the export clause
  (let ([clause (assq 'export (cddr library))])
    (apply append
      (map (lambda (spec)
             (cond [(symbol? spec) (list (cons spec spec))]
                   [(and (pair? spec) (eq? (car spec) 'rename))
                    (map (lambda (r) (cons (cadr r) (car r))) (cdr spec))]
                   [else '()]))
           (if clause (cdr clause) '())))))

(define (import-specs library)
  (let ([clause (assq 'import (cddr library))])
    (if clause (cdr clause) '())))

(define (record-names form)
  ;; the names a define-record-type binds
  (let* ([spec (cadr form)]
         [type (if (pair? spec) (car spec) spec)]
         [constructor (if (pair? spec) (cadr spec) (string->symbol (string-append "make-" (symbol->string type))))]
         [predicate (if (pair? spec) (caddr spec) (string->symbol (string-append (symbol->string type) "?")))]
         [fields (let ([f (assq 'fields (cddr form))]) (if f (cdr f) '()))])
    (append
      (list type constructor predicate)
      (apply append
        (map (lambda (field)
               (let* ([name (if (pair? field) (if (memq (car field) '(mutable immutable)) (cadr field) (car field)) field)]
                      [explicit (and (pair? field) (memq (car field) '(mutable immutable)) (cddr field))])
                 (if (and explicit (pair? explicit))
                     explicit
                     (list (string->symbol (string-append (symbol->string type) "-" (symbol->string name)))
                           (string->symbol (string-append (symbol->string type) "-" (symbol->string name) "-set!"))))))
             fields)))))

(define (edoc-annotation? form)
  ;; (edoc "summary" ...) annotates the next form; (edoc name "summary" ...) names its definition
  (and (pair? form) (eq? (car form) 'edoc) (pair? (cdr form))))

;;; Types ----------------------------------------------------------------------------

(define base-types
  '(string char integer number boolean list pair vector bytevector hashtable port procedure thunk condition
     symbol datum any file directory buffer window region position command key mode style actor head))

(define defined-types (make-eq-hashtable))
(define used-types '())   ; ((type library name) ...)

(define (note-types! library form)
  ;; the types an edoc annotation's clauses name, and the types an edoc-type form defines
  (cond
    [(and (pair? form) (eq? (car form) 'edoc-type) (pair? (cdr form)) (symbol? (cadr form)))
     (eq-hashtable-set! defined-types (cadr form) library)]
    [(edoc-annotation? form)
     (let* ([named? (symbol? (cadr form))]
            [clauses (if named? (cdddr form) (cddr form))]
            [name (if named? (cadr form) 'annotation)])
       (for-each
         (lambda (clause)
           (when (and (pair? clause) (symbol? (car clause)) (pair? (cdr clause)) (not (eq? (car clause) 'constructor)))
             (set! used-types (cons (list (cadr clause) library name) used-types))))
         clauses))]))

(define (unknown-type? t)
  (cond [(symbol? t) (not (or (memq t base-types) (eq-hashtable-contains? defined-types t)))]
        [(eq? t #f) #f]
        [(and (pair? t) (list? t))
         (case (car t)
           [(one-of record) #f]
           [(or list-of) (exists unknown-type? (cdr t))]
           [else #t])]
        [else #t]))

(define (definitions library)
  ;; (name . kind) for every top-level definition of the library body; a
  ;; definition an edoc annotates, or names, is documented
  (let loop ([forms (cdddr library)] [pending? #f] [out '()])
    (cond
      [(null? forms) (apply append (reverse out))]
      [(edoc-annotation? (car forms))
       (if (string? (cadr (car forms)))
           (loop (cdr forms) #t out)
           (loop (cdr forms) pending? (cons (list (cons (cadr (car forms)) 'documented)) out)))]
      [else
       (let ([defs (form-definitions (car forms))])
         (loop (cdr forms) #f
               (cons (if pending? (map (lambda (d) (cons (car d) 'documented)) defs) defs) out)))])))

(define (form-definitions form)
  ;; (name . kind) for one top-level form of a library body
  (apply append
    (map (lambda (form)
           (if (not (pair? form)) '()
               (case (car form)
                 ;; the edoc library documents its own definitions with two
                 ;; forms of its own, checked and recorded like annotations
                 [(edefine) (list (cons (if (pair? (cadr form)) (car (cadr form)) (cadr form)) 'documented))]
                 [(edefine-record-type)
                  (map (lambda (n) (cons n 'documented)) (record-names (cons 'define-record-type (cons (cadr form) (cdddr form)))))]
                 [(define)
                  (let ([target (cadr form)])
                    (cond [(pair? target) (list (cons (car target) 'procedure))]
                          [(and (pair? (cddr form)) (pair? (caddr form)) (eq? (car (caddr form)) 'attach-name!)
                                (pair? (cdr (caddr form))) (pair? (cadr (caddr form))) (eq? (car (cadr (caddr form))) 'quote))
                           ;; (define x (attach-name! 'name '(edoc ...))): the edoc library's own keywords
                           (list (cons (cadr (cadr (caddr form))) 'documented))]
                          [(and (pair? (cddr form)) (symbol? (caddr form)))
                           (list (cons target (list 'alias (caddr form))))]
                          [(and (pair? (cddr form)) (pair? (caddr form)))
                           (let ([head (car (caddr form))])
                             (list (cons target
                                     (cond [(memq head '(lambda case-lambda)) 'procedure]
                                           [(memq head '(make-parameter make-thread-parameter)) 'parameter]
                                           [else 'value]))))]
                          [else (list (cons target 'value))]))]
                 [(define-syntax) (list (cons (cadr form) 'syntax))]
                 [(define-record-type) (map (lambda (n) (cons n 'record)) (record-names form))]
                 [(define-condition-type)
                  ;; (define-condition-type &name &parent make-name name? (field accessor) ...)
                  (map (lambda (n) (cons n 'record))
                    (append (list (cadr form) (cadddr form) (car (cddddr form)))
                            (map cadr (cdr (cddddr form)))))]
                 [else '()])))
         (list form))))

;;; The libraries, indexed by name ------------------------------------------------

(define libraries
  ;; (name library) for every library form under lib
  (apply append
    (map (lambda (path)
           (let ([library (find (lambda (f) (and (pair? f) (memq (car f) '(library elibrary)))) (read-forms path))])
             (if library (list (list (cadr library) library path)) '())))
         (list-sort string<? (sls-files "lib")))))

(define (library-named name) (let ([hit (assoc name libraries)]) (and hit (cadr hit))))

(define definition-table (make-hashtable equal-hash equal?))
(define (definitions-of name)
  (or (hashtable-ref definition-table name #f)
      (let ([defs (let ([library (library-named name)]) (if library (definitions library) '()))])
        (hashtable-set! definition-table name defs)
        defs)))

(define (strip-prefix prefix name)
  ;; name without prefix, or #f
  (let ([p (symbol->string prefix)] [n (symbol->string name)])
    (and (> (string-length n) (string-length p))
         (string=? p (substring n 0 (string-length p)))
         (string->symbol (substring n (string-length p) (string-length n))))))

(define standard-libraries '((rnrs) (chezscheme) (scheme)))

(define standard-names
  ;; every name Chez's environment binds, which the standard libraries claim
  (let ([t (make-eq-hashtable)])
    (for-each (lambda (s) (eq-hashtable-set! t s #t)) (environment-symbols (environment '(chezscheme))))
    t))

(define (standard-library? spec)
  (or (member spec standard-libraries) (and (pair? spec) (eq? (car spec) 'rnrs))))

(define (resolve-import spec local)
  ;; (library-name . external-name) when spec brings local in, or #f; a
  ;; standard library claims the names Chez binds
  (cond
    [(not (pair? spec)) #f]
    [(standard-library? spec) (and (eq-hashtable-ref standard-names local #f) (cons '(rnrs) local))]
    [(eq? (car spec) 'prefix)
     (let ([inner (strip-prefix (caddr spec) local)])
       (and inner (resolve-import (cadr spec) inner)))]
    [(eq? (car spec) 'only)
     (and (memq local (cddr spec)) (resolve-import (cadr spec) local))]
    [(eq? (car spec) 'except)
     (and (not (memq local (cddr spec))) (resolve-import (cadr spec) local))]
    [(eq? (car spec) 'rename)
     (let ([renamed (find (lambda (r) (eq? (cadr r) local)) (cddr spec))])
       (cond [renamed (resolve-import (cadr spec) (car renamed))]
             [(exists (lambda (r) (eq? (car r) local)) (cddr spec)) #f]
             [else (resolve-import (cadr spec) local)]))]
    [(eq? (car spec) 'for) (resolve-import (cadr spec) local)]
    [else
     (let ([library (library-named spec)])
       (and library (assq local (exports-of library)) (cons spec local)))]))

(define (classify name external depth)
  ;; the kind of the export external of the library name: its local
  ;; definition, or the definition behind the import it re-exports or
  ;; aliases
  (define (imported internal)
    (let loop ([specs (import-specs (library-named name))])
      (if (null? specs) 'elsewhere
          (let ([origin (resolve-import (car specs) internal)])
            (if origin (classify (car origin) (cdr origin) (+ depth 1)) (loop (cdr specs)))))))
  (let* ([library (library-named name)]
         [internal (let ([e (and library (assq external (exports-of library)))]) (if e (cdr e) external))]
         [defs (definitions-of name)]
         ;; a keyword documented by hand has two entries; the documented one wins
         [def (or (find (lambda (d) (and (eq? (car d) internal) (eq? (cdr d) 'documented))) defs) (assq internal defs))])
    (cond
      [(equal? name '(rnrs)) 'standard]
      [(or (not library) (> depth 8)) 'elsewhere]
      [(not def) (imported internal)]
      [(and (pair? (cdr def)) (eq? (cadr def) 'alias))
       (let ([kind (if (assq (caddr def) (definitions-of name)) (classify name (caddr def) (+ depth 1)) (imported (caddr def)))])
         (if (eq? kind 'elsewhere) 'value kind))]
      [else (cdr def)])))

;;; The report -----------------------------------------------------------------------

(define arguments (command-line-arguments))
(define listing? (and (member "--list" arguments) #t))
(define effects? (and (member "--effects" arguments) #t))
(define internal? (and (member "--internal" arguments) #t))
(define selected (filter (lambda (a) (not (member a '("--list" "--effects" "--internal")))) arguments))

(define (stem-of path)
  (let ([file (path-last path)]) (substring file 0 (- (string-length file) 4))))
(define (selected? path) (or (null? selected) (member (stem-of path) selected)))

;;; Effects ------------------------------------------------------------------------

;; What a body reaches. A standard mutator is known by its bang or by these
;; lists and judged on its subject: something the body made or bound is
;; scratch, one of the definition's own arguments is a change the caller
;; sees, anything else is a change. Calls stop at three seams and count as
;; opaque there, satisfying a bang without indicting a query: a foreign
;; procedure, a hook installed at run time, and an argument the body calls.

(define bangless-mutators
  ;; standard names whose call is a change whatever their arguments
  '(printf delete-file rename-file mkdir chmod putenv copy-file truncate-file
     call-with-output-file with-output-to-file open-output-file open-file-output-port
     fork-thread condition-signal condition-broadcast mutex-acquire mutex-release
     register-signal-handler putprop remprop define-top-level-value system exit abort))
(define opaque-standard
  ;; standard names whose effect is whatever they are handed
  '(eval load compile-library compile-file))
(define mutator-subjects
  ;; (name . index of the argument it changes)
  '((set! . 0) (set-car! . 0) (set-cdr! . 0) (set-box! . 0)
    (vector-set! . 0) (vector-fill! . 0) (vector-sort! . 1) (list-sort! . 1) (sort! . 1)
    (hashtable-set! . 0) (hashtable-update! . 0) (hashtable-delete! . 0) (hashtable-clear! . 0)
    (eq-hashtable-set! . 0) (eq-hashtable-update! . 0) (eq-hashtable-delete! . 0)
    (eqv-hashtable-set! . 0) (eqv-hashtable-update! . 0) (eqv-hashtable-delete! . 0)
    (symbol-hashtable-set! . 0) (symbol-hashtable-update! . 0) (symbol-hashtable-delete! . 0)
    (string-set! . 0) (string-fill! . 0) (string-copy! . 2)
    (bytevector-u8-set! . 0) (bytevector-s8-set! . 0) (bytevector-u16-set! . 0) (bytevector-s16-set! . 0)
    (bytevector-u32-set! . 0) (bytevector-s32-set! . 0) (bytevector-u64-set! . 0) (bytevector-s64-set! . 0)
    (bytevector-u16-native-set! . 0) (bytevector-u32-native-set! . 0) (bytevector-u64-native-set! . 0)
    (bytevector-ieee-double-set! . 0) (bytevector-fill! . 0) (bytevector-copy! . 2)
    (foreign-set! . 1) (get-bytevector-n! . 1) (get-bytevector-some! . 1) (get-string-n! . 1)
    (close-port . 0) (close-input-port . 0) (close-output-port . 0)
    (set-port-nonblocking! . 0) (set-port-position! . 0) (set-port-eol-style! . 0) (set-port-length! . 0)))
(define part-accessors
  ;; a subject reached through these is judged by what they take apart
  '(car cdr caar cadr cdar cddr caddr cdddr cadddr list-ref list-tail vector-ref string-ref
     hashtable-ref eq-hashtable-ref eqv-hashtable-ref symbol-hashtable-ref unbox))
(define fresh-makers
  ;; a subject made on the spot is the body's own
  '(make-vector make-string make-bytevector make-list list vector string bytevector cons box
     make-eq-hashtable make-eqv-hashtable make-hashtable make-weak-eq-hashtable make-weak-eqv-hashtable
     vector-copy string-copy bytevector-copy list-copy list->vector vector->list list->string string->list
     string-append substring format number->string symbol->string reverse append map filter vector-map iota
     open-output-string open-string-output-port open-bytevector-output-port foreign-alloc
     open-input-file open-file-input-port open-string-input-port open-bytevector-input-port
     open-fd-input-port open-fd-output-port open-fd-input/output-port))
(define prompting-procedures
  ;; (library-name . names) whose call waits for a key
  '(((head head) read-key-event)))

(define editing-procedures
  ;; (library-name . names) whose call edits the current buffer's text: what
  ;; a command declaring (edits) must reach
  '(((head edit) check-editable!)))

(define macro-calls
  ;; (macro . procedure) within one library: a syntax form standing for a
  ;; call of the procedure its template makes, followed as a callee
  '((with-recorded-edit . call-with-recorded-edit!)))

(define higher-order
  ;; (form . positions): arguments that are procedures the form calls
  '((apply 0) (for-each 0) (map 0) (vector-for-each 0) (vector-map 0) (string-for-each 0)
    (call-with-values 0 1) (dynamic-wind 0 1 2) (call-with-current-continuation 0) (call/cc 0)
    (call-with-port 1) (call-with-string-output-port 0) (call-with-bytevector-output-port 0)
    (with-exception-handler 0 1) (hashtable-update! 2) (eq-hashtable-update! 2) (fork-thread 0)
    (fold-left 0) (fold-right 0) (filter 0) (find 0) (exists 0) (for-all 0) (partition 0) (assp 0) (memp 0)
    (list-sort 0) (vector-sort 0) (vector-sort! 0) (remp 0)))

(define port-writers
  ;; (writer . where its port argument is); without one they write to the current output
  '((display . last) (write . last) (write-char . last) (newline . last) (fresh-line . last) (pretty-print . last)
    (put-string . first) (put-char . first) (put-bytevector . first) (put-u8 . first) (put-datum . first)
    (flush-output-port . first) (fprintf . first)))
(define (writer-port op args)
  ;; the port form a writer call writes to, or #f for the current output
  (and (list? args)
       (case (cdr (assq op port-writers))
         [(first) (and (pair? args) (car args))]
         [(last) (and (>= (length args) (if (memq op '(newline fresh-line flush-output-port)) 1 2))
                      (list-ref args (- (length args) 1)))]
         [else #f])))

(define assigned-table (make-hashtable equal-hash equal?))
(define (assigned-variables path)
  ;; the top-level variables a library assigns anywhere: hooks and state
  (or (hashtable-ref assigned-table path #f)
      (let ([names '()])
        (let scan ([form (cons 'begin (let ([e (entry-at path)]) (if e (cdddr (cadr e)) '())))])
          (when (and (pair? form) (not (memq (car form) '(quote quasiquote))))
            (when (and (eq? (car form) 'set!) (pair? (cdr form)) (symbol? (cadr form)))
              (set! names (cons (cadr form) names)))
            (when (list? form) (for-each scan form))))
        (hashtable-set! assigned-table path names)
        names)))

(define (bang-name? sym)
  (let ([s (symbol->string sym)]) (and (> (string-length s) 1) (char=? (string-ref s (- (string-length s) 1)) #\!))))

(define (formal-list f)
  ;; the names a formals spec binds, a rest name last
  (cond [(pair? f) (cons (car f) (formal-list (cdr f)))] [(symbol? f) (list f)] [else '()]))

(define (index-in sym names)
  (let loop ([names names] [i 0])
    (cond [(null? names) #f] [(eq? (car names) sym) i] [else (loop (cdr names) (+ i 1))])))

;; a body's context: 0 formals 1 locals 2 inner formals 3 inits ((name . form) ...)
;; 4 path 5 helpers ((name . tail form) ...) for the procedures it defines
(define (kind-of path op)
  (let ([target (and (symbol? op) (resolve-name path op))])
    (and (pair? target) (vector-ref (hashtable-ref all-definitions target #f) 0))))

(define (fresh? ctx form)
  ;; whether a form makes what it returns: a literal, quoted data, a
  ;; maker's or a record constructor's result, or a call to a procedure
  ;; whose every return is fresh
  (cond
    [(symbol? form) #f]
    [(not (pair? form)) #t]
    [(memq (car form) '(quote quasiquote)) #t]
    [(memq (car form) fresh-makers) #t]
    [(memq (car form) '(and or if let let* letrec letrec* begin cond))
     (let ([tails (if (memq (car form) '(and or)) (if (eq? (car form) 'and) (list (list-ref form (- (length form) 1))) (cdr form)) (tails-of form))])
       (and (list? form) (pair? (cdr form)) (for-all (lambda (tail) (or (boolean? tail) (fresh? ctx tail))) tails)))]
    [(and (pair? (car form)) (eq? (caar form) 'if) (list? (car form)) (= (length (car form)) 4))
     (for-all (lambda (maker) (and (symbol? maker) (fresh? ctx (list maker)))) (cddar form))]
    [(not (symbol? (car form))) #f]
    [(eq? (kind-of (vector-ref ctx 4) (car form)) 'constructor) #t]
    [(eq? (kind-of (vector-ref ctx 4) (car form)) 'procedure) (fresh-result? (resolve-name (vector-ref ctx 4) (car form)))]
    [else #f]))

(define (tails-of form)
  ;; the forms a form's value comes from
  (cond
    [(not (pair? form)) (list form)]
    [(and (memq (car form) '(let let* letrec letrec* begin)) (list? form) (pair? (cdr form)))
     (tails-of (list-ref form (- (length form) 1)))]
    [(and (eq? (car form) 'if) (list? form) (>= (length form) 3)) (apply append (map tails-of (cddr form)))]
    [(and (eq? (car form) 'cond) (list? form))
     (apply append (map (lambda (clause) (if (and (list? clause) (pair? clause) (not (memq '=> clause))) (tails-of (list-ref clause (- (length clause) 1))) (list clause)))
                        (cdr form)))]
    [else (list form)]))

(define (fresh-result? key)
  ;; whether every return of a procedure is fresh; a recursive return
  ;; is taken as fresh while its own verdict is pending
  (let ([v (hashtable-ref all-definitions key #f)])
    (cond
      [(not (eq? (vector-ref v 13) 'unknown)) (vector-ref v 13)]
      [(not (eq? (vector-ref v 0) 'procedure)) #f]
      [else
       (vector-set! v 13 #t)
       (let ([verdict
              (for-all (lambda (body)
                         (let ([forms (cdr body)])
                           (and (pair? forms)
                                (let ([ctx (vector (formal-list (car body)) '() '() '() (car key) '())])
                                  (for-all (lambda (tail) (fresh? ctx tail)) (tails-of (list-ref forms (- (length forms) 1))))))))
                       (vector-ref v 12))])
         (vector-set! v 13 verdict)
         verdict)])))

(define (subject-root ctx form)
  ;; the variable a subject form takes apart, through the standard part
  ;; accessors, the tree's record accessors, and, let, begin and the
  ;; body's own helpers, or #f
  (let ([path (vector-ref ctx 4)] [helpers (vector-ref ctx 5)])
    (cond [(symbol? form) form]
          [(not (and (pair? form) (symbol? (car form)) (list? form))) #f]
          [(and (pair? (cdr form)) (or (memq (car form) part-accessors) (eq? (kind-of path (car form)) 'accessor)))
           (subject-root ctx (cadr form))]
          [(and (memq (car form) '(and begin let let* letrec letrec*)) (pair? (cdr form)))
           (subject-root ctx (list-ref form (- (length form) 1)))]
          [(assq (car form) helpers) => (lambda (helper) (subject-root (vector 0 0 0 0 path (remq helper helpers)) (cdr helper)))]
          [else #f])))

(define (judge-form form ctx)
  ;; what changing form amounts to in a body: scratch when the body made
  ;; it, the index of the argument it came from, else change
  (let ([formals (vector-ref ctx 0)] [locals (vector-ref ctx 1)] [inner (vector-ref ctx 2)] [inits (vector-ref ctx 3)])
    (let judge ([form form] [seen '()])
      (let ([root (subject-root ctx form)])
        (cond
          [(and (not root) (fresh? ctx form)) 'scratch]
          [(not root) 'change]
          [(index-in root formals) => values]
          [(memq root seen) 'scratch]
          [(assq root inits)
           (let ([init (cdr (assq root inits))])
             (if (fresh? ctx init) 'scratch (judge init (cons root seen))))]
          [(or (memq root inner) (memq root locals)) 'scratch]
          [else 'change])))))

(define (tail-lambdas forms)
  ;; the lambda forms a body returns rather than runs: deferred, their
  ;; effects belong to whoever calls the result
  (define (tails form)
    (cond
      [(not (pair? form)) '()]
      [(memq (car form) '(lambda case-lambda)) (list form)]
      [(and (memq (car form) '(let let* letrec letrec* begin when unless parameterize fluid-let)) (list? form) (pair? (cdr form)))
       (tails (list-ref form (- (length form) 1)))]
      [(and (eq? (car form) 'if) (list? form)) (apply append (map tails (cddr form)))]
      [(and (memq (car form) '(cond case)) (list? form))
       (apply append
         (map (lambda (clause)
                (if (and (list? clause) (pair? clause) (not (memq '=> clause))) (tails (list-ref clause (- (length clause) 1))) '()))
              (if (eq? (car form) 'case) (if (pair? (cdr form)) (cddr form) '()) (cdr form))))]
      [else '()]))
  (if (pair? forms) (tails (list-ref forms (- (length forms) 1))) '()))

(define (walk-body forms)
  ;; (values locals inner inits helpers calls): the names the body binds,
  ;; those among them that are formals of inner lambdas, what the plain
  ;; variables start as, the tail form of each procedure the body
  ;; defines, and every (op . args) in operator position with
  ;; its argument forms, or the symbol higher when apply, for-each and the
  ;; like hand op its arguments; quoted data, syntax, clause heads and the
  ;; lambdas the body returns are not calls
  (let ([locals '()] [inner '()] [calls '()] [inits '()] [helpers '()] [deferred (tail-lambdas forms)])
    (define (bind! names) (set! locals (append names locals)))
    (define (init! bindings)
      ;; ([name init ...] ...): what each plain variable starts as
      (when (list? bindings)
        (for-each (lambda (b) (when (and (list? b) (>= (length b) 2) (symbol? (car b))) (set! inits (cons (cons (car b) (cadr b)) inits))))
                  bindings)))
    (define (bind-formals! f) (let ([names (formal-list f)]) (bind! names) (set! inner (append names inner))))
    (define (binding-names bindings)
      (if (list? bindings)
          (apply append (map (lambda (b) (cond [(pair? b) (formal-list (if (pair? (car b)) (car b) (list (car b))))] [(symbol? b) (list b)] [else '()])) bindings))
          '()))
    (define (walk-all forms) (when (list? forms) (for-each walk forms)))
    (define (walk-clauses clauses)
      ;; cond and guard clauses: tests and bodies are expressions, else and => are not
      (when (list? clauses)
        (for-each (lambda (clause) (when (list? clause) (for-each (lambda (x) (unless (memq x '(else =>)) (walk x))) clause)))
                  clauses)))
    (define (walk form)
      (cond
        [(or (not (pair? form)) (memq form deferred)) (void)]
        [(memq (car form) '(quote quasiquote syntax quasisyntax syntax-case syntax-rules define-syntax let-syntax letrec-syntax define-record-type)) (void)]
        [(not (symbol? (car form))) (when (list? form) (for-each walk form))]
        [else
         (let ([op (car form)] [args (if (list? (cdr form)) (cdr form) '())])
           (set! calls (cons (cons op (if (list? (cdr form)) (cdr form) 'improper)) calls))
           ;; a procedure handed to apply, for-each and the like is called too
           (let ([positions (assq op higher-order)])
             (when positions
               (for-each (lambda (i) (when (and (< i (length args)) (symbol? (list-ref args i)))
                                       (set! calls (cons (cons (list-ref args i) 'higher) calls))))
                         (cdr positions))))
           (case op
             [(lambda) (when (pair? args) (bind-formals! (car args)) (walk-all (cdr args)))]
             [(case-lambda) (for-each (lambda (clause) (when (pair? clause) (bind-formals! (car clause)) (walk-all (cdr clause)))) args)]
             [(let let* letrec letrec* let-values let*-values do)
              (when (pair? args)
                (cond [(symbol? (car args)) (bind! (list (car args))) (when (pair? (cdr args)) (bind! (binding-names (cadr args))) (init! (cadr args)))]
                      [else (bind! (binding-names (car args))) (init! (car args))])
                (walk-all args))]
             [(define)
              (when (pair? args)
                (cond [(pair? (car args))
                       (bind! (list (caar args))) (bind-formals! (cdar args))
                       (when (pair? (cdr args)) (set! helpers (cons (cons (caar args) (list-ref args (- (length args) 1))) helpers)))]
                      [else (bind! (list (car args))) (init! (list args))])
                (walk-all (cdr args)))]
             [(cond) (walk-clauses args)]
             [(case) (when (pair? args) (walk (car args)) (walk-clauses (map (lambda (c) (if (and (list? c) (pair? c)) (cdr c) '())) (cdr args))))]
             [(guard)
              (when (pair? args)
                (when (pair? (car args)) (bind! (list (caar args))) (walk-clauses (cdar args)))
                (walk-all (cdr args)))]
             [(parameterize fluid-let)
              (when (pair? args)
                (walk-all (map (lambda (b) (if (and (list? b) (= (length b) 2)) (cadr b) b)) (if (list? (car args)) (car args) '())))
                (walk-all (cdr args)))]
             [else (walk-all args)]))]))
    (for-each walk forms)
    (values locals inner inits helpers (reverse calls))))

(define (record-procedures form)
  ;; ((name . kind) ...) for the constructor, accessors and mutators a
  ;; define-record-type form defines
  (let* ([spec (cadr form)]
         [type (if (pair? spec) (car spec) spec)]
         [clause (and (list? (cddr form)) (find (lambda (c) (and (pair? c) (eq? (car c) 'fields))) (cddr form)))]
         [fields (if clause (cdr clause) '())])
    (cons
      (cons (if (and (list? spec) (>= (length spec) 2)) (cadr spec) (string->symbol (format "make-~a" type))) 'constructor)
      (apply append
        (map (lambda (f)
               (let* ([f (if (symbol? f) (list 'immutable f) f)]
                      [field (and (pair? f) (pair? (cdr f)) (cadr f))])
                 (if (not field) '()
                   (cons (cons (if (and (list? f) (>= (length f) 3)) (caddr f) (string->symbol (format "~a-~a" type field))) 'accessor)
                         (if (eq? (car f) 'mutable)
                             (list (cons (if (and (list? f) (>= (length f) 4)) (cadddr f) (string->symbol (format "~a-~a-set!" type field))) 'mutator))
                             '())))))
          fields)))))

(define (flag-clauses clauses)
  (filter (lambda (c) (and (pair? c) (memq (car c) '(prompts effects edits)))) clauses))

(define (definition-bodies library)
  ;; (name kind bodies documented? flags) for every top-level definition
  ;; with something to walk: procedure, parameter, foreign, value, alias,
  ;; and a record's constructor, accessors and mutators
  (let loop ([forms (cdddr library)] [pending #f] [out '()])
    (cond
      [(null? forms) (reverse out)]
      [(and (edoc-annotation? (car forms)) (string? (cadr (car forms))))
       (loop (cdr forms) (cddr (car forms)) out)]
      [(edoc-annotation? (car forms)) (loop (cdr forms) #f out)]
      [else
       (let* ([form (car forms)]
              [flags (if pending (flag-clauses pending) '())]
              [entries
               (cond
                 [(not (pair? form)) '()]
                 [(memq (car form) '(define edefine))
                  (let* ([target (cadr form)]
                         [rest (if (eq? (car form) 'edefine)
                                   (filter (lambda (f) (not (edoc-annotation? f))) (cddr form))
                                   (cddr form))]
                         [inner (and (eq? (car form) 'edefine) (find edoc-annotation? (cddr form)))]
                         [flags (if inner (flag-clauses (cddr inner)) flags)]
                         [documented? (or (and pending #t) (eq? (car form) 'edefine))])
                    (list
                      (cond
                        [(pair? target) (list (car target) 'procedure (list (cons (cdr target) rest)) documented? flags)]
                        [(and (pair? rest) (pair? (car rest)) (eq? (caar rest) 'lambda))
                         (list target 'procedure (list (cons (cadar rest) (cddar rest))) documented? flags)]
                        [(and (pair? rest) (pair? (car rest)) (eq? (caar rest) 'case-lambda))
                         (list target 'procedure (map (lambda (c) (cons (car c) (cdr c))) (cdar rest)) documented? flags)]
                        [(and (pair? rest) (pair? (car rest)) (memq (caar rest) '(make-parameter make-thread-parameter)))
                         (list target 'parameter '() documented? flags)]
                        [(and (pair? rest) (let has ([f (car rest)]) (and (pair? f) (or (eq? (car f) 'foreign-procedure) (has (car f)) (has (cdr f))))))
                         (list target 'foreign '() documented? flags)]
                        [(and (pair? rest) (symbol? (car rest))) (list target (list 'alias (car rest)) '() documented? flags)]
                        [else (list target 'value '() documented? flags)])))]
                 [(eq? (car form) 'define-record-type)
                  (map (lambda (p) (list (car p) (cdr p) '() #f '())) (record-procedures form))]
                 [else '()])])
         (loop (cdr forms) #f (append (reverse entries) out)))])))

(define (runtime-of path)
  (cond [(and (>= (string-length path) 9) (string=? (substring path 0 9) "lib/base/")) 'base]
        [(and (>= (string-length path) 11) (string=? (substring path 0 11) "lib/client/")) 'client]
        [else 'common]))

(define (entry-for name from-path)
  ;; the library entry a name denotes for an importer at from-path: the same
  ;; runtime's when both runtimes implement it, else the client's, the
  ;; head's side of every seam
  (let ([candidates (filter (lambda (e) (equal? (car e) name)) libraries)])
    (cond [(null? candidates) #f]
          [(null? (cdr candidates)) (car candidates)]
          [else (let ([rt (runtime-of from-path)])
                  (or (find (lambda (e) (eq? (runtime-of (caddr e)) rt)) candidates)
                      (find (lambda (e) (eq? (runtime-of (caddr e)) 'client)) candidates)
                      (car candidates)))])))

(define (entry-at path) (find (lambda (e) (string=? (caddr e) path)) libraries))
(define (name-at path) (let ([e (entry-at path)]) (and e (car e))))

(define (resolve-import-from from-path spec local)
  ;; (path . external-name) when spec brings local in, or standard, or #f
  (cond
    [(not (pair? spec)) #f]
    [(standard-library? spec) (and (eq-hashtable-ref standard-names local #f) 'standard)]
    [(eq? (car spec) 'prefix)
     (let ([inner (strip-prefix (caddr spec) local)])
       (and inner (resolve-import-from from-path (cadr spec) inner)))]
    [(eq? (car spec) 'only) (and (memq local (cddr spec)) (resolve-import-from from-path (cadr spec) local))]
    [(eq? (car spec) 'except) (and (not (memq local (cddr spec))) (resolve-import-from from-path (cadr spec) local))]
    [(eq? (car spec) 'rename)
     (let ([renamed (find (lambda (r) (eq? (cadr r) local)) (cddr spec))])
       (cond [renamed (resolve-import-from from-path (cadr spec) (car renamed))]
             [(exists (lambda (r) (eq? (car r) local)) (cddr spec)) #f]
             [else (resolve-import-from from-path (cadr spec) local)]))]
    [(eq? (car spec) 'for) (resolve-import-from from-path (cadr spec) local)]
    [else
     (let ([entry (entry-for spec from-path)])
       (and entry (assq local (exports-of (cadr entry))) (cons (caddr entry) local)))]))

(define all-definitions (make-hashtable equal-hash equal?))   ; (path . name) -> vector
(define definition-order '())
;; fields: 0 kind 1 documented? 2 flags 3 witnesses ((kind . text) ...) 4 callees ((key args ctx) ...)
;;         5 prompts-direct? 6 effect (#f, opaque or #t) 7 prompts? 8 changed-argument indices
;;         9 library 10 exported name 11 bang? 12 bodies 13 fresh result (unknown, #t or #f)
;;         14 edits-direct? 15 edits?

(define (resolve-name path sym)
  ;; the definition key a symbol denotes in the library at path, or
  ;; standard, or unknown; an alias resolves to what it names
  (let ([own (cons path sym)])
    (cond
      [(and (hashtable-contains? all-definitions own)
            (pair? (vector-ref (hashtable-ref all-definitions own #f) 0)))
       (let ([other (cadr (vector-ref (hashtable-ref all-definitions own #f) 0))])
         (if (eq? other sym) 'unknown (resolve-name path other)))]
      [(hashtable-contains? all-definitions own) own]
      [else
       (let loop ([specs (import-specs (cadr (entry-at path)))])
         (if (null? specs) 'unknown
             (let ([origin (resolve-import-from path (car specs) sym)])
               (cond [(not origin) (loop (cdr specs))]
                     [(eq? origin 'standard) 'standard]
                     [else
                      (let* ([exports (exports-of (cadr (entry-at (car origin))))]
                             [internal (let ([e (assq (cdr origin) exports)]) (if e (cdr e) (cdr origin)))]
                             [key (cons (car origin) internal)])
                        (if (hashtable-contains? all-definitions key) (resolve-name (car origin) internal) 'unknown))]))))])))

(define (prompting-key? key)
  (and (pair? key)
       (let ([entry (assoc (name-at (car key)) prompting-procedures)])
         (and entry (memq (cdr key) (cdr entry)) #t))))

(define (editing-key? key)
  (and (pair? key)
       (let ([entry (assoc (name-at (car key)) editing-procedures)])
         (and entry (memq (cdr key) (cdr entry)) #t))))

(define (analyze-library! entry)
  ;; every definition of a library, an assigned one a hook whatever it
  ;; held; the bang judged is the name importers see
  (let* ([path (caddr entry)] [assigned (assigned-variables path)] [exports (exports-of (cadr entry))])
    (for-each
      (lambda (def)
        (let* ([name (car def)]
               [kind (if (and (memq name assigned) (not (memq (cadr def) '(mutator accessor constructor)))) 'hook (cadr def))]
               [key (cons path name)]
               [external (let ([e (find (lambda (e) (eq? (cdr e) name)) exports)]) (if e (car e) name))])
          (hashtable-set! all-definitions key
            (vector kind (cadddr def) (car (cddddr def)) '() '() #f #f #f '() (car entry) external (bang-name? external) (caddr def) 'unknown
                    #f #f))
          (set! definition-order (cons key definition-order))))
      (definition-bodies (cadr entry)))))

(define (resolve-bodies! key)
  ;; direct witnesses, callees and prompting for one definition
  (let* ([v (hashtable-ref all-definitions key #f)] [path (car key)] [bodies (vector-ref v 12)])
    (when (eq? (vector-ref v 0) 'procedure)
      (let ([witnesses '()] [callees '()] [prompts? #f] [edits? #f])
        (define (witness! kind text) (set! witnesses (cons (cons kind text) witnesses)))
        (for-each
          (lambda (body)
            (let ([formals (formal-list (car body))])
              (let-values ([(locals inner inits helpers calls) (walk-body (cdr body))])
                (define ctx (vector formals locals inner inits path helpers))
                (define (judge-subject! op subject)
                  ;; op changes subject: nothing when the body made it, the
                  ;; argument's index when the caller handed it over, else a
                  ;; witness; set! rebinds a variable rather than changing
                  ;; what it holds
                  (let ([verdict (if (and (eq? op 'set!) (symbol? subject) (or (memq subject locals) (memq subject formals)))
                                     'scratch
                                     (judge-form subject ctx))])
                    (cond
                      [(eq? verdict 'scratch) (void)]
                      [(integer? verdict) (witness! verdict (format "~a on its argument ~a" op (list-ref formals verdict)))]
                      [else (witness! 'definite (format "~a on ~a" op (or (subject-root ctx subject) "an expression")))])))
                (for-each
                  (lambda (call)
                    (let* ([op (car call)] [args (cdr call)] [arglist (if (list? args) args '())])
                      (cond
                        [(assq op mutator-subjects)
                         (let ([i (cdr (assq op mutator-subjects))])
                           (when (< i (length arglist)) (judge-subject! op (list-ref arglist i))))]
                        [(or (memq op formals) (memq op inner)) (witness! 'opaque (format "~a, an argument it calls" op))]
                        [(memq op locals) (void)]   ; a helper the body binds: its calls are here already
                        [(assq op macro-calls)
                         ;; a macro of the library standing for a call: its procedure is a callee
                         => (lambda (implied)
                              (let ([t (resolve-name path (cdr implied))])
                                (when (pair? t) (set! callees (cons (list t '() ctx) callees)))))]
                        [else
                         (let ([target (resolve-name path op)])
                           (cond
                             [(pair? target)
                              (let ([tv (hashtable-ref all-definitions target #f)])
                                (when (prompting-key? target) (set! prompts? #t))
                                (when (editing-key? target) (set! edits? #t))
                                (case (vector-ref tv 0)
                                  [(foreign) (witness! 'opaque (format "~a, a foreign procedure" op))]
                                  [(hook) (witness! 'opaque (format "~a, a hook installed at run time" op))]
                                  [(parameter) (when (pair? arglist) (witness! 'definite (format "(~a ...) sets the parameter" op)))]
                                  [(mutator) (when (pair? arglist) (judge-subject! op (car arglist)))]
                                  [(procedure) (set! callees (cons (list target arglist ctx) callees))]
                                  [else (void)]))]
                             [(assq op port-writers)
                              (let ([port (writer-port op args)])
                                (if port (judge-subject! op port) (witness! 'definite (format "~a to the current output" op))))]
                             [(memq op opaque-standard) (witness! 'opaque (symbol->string op))]
                             [(memq op bangless-mutators) (witness! 'definite (symbol->string op))]
                             [(bang-name? op) (witness! 'definite (symbol->string op))]
                             [else (void)]))])))
                  calls))))
          bodies)
        (vector-set! v 3 (reverse witnesses))
        (vector-set! v 4 (reverse callees))
        (vector-set! v 5 prompts?)
        (vector-set! v 14 edits?)))))

(define (flagged? v kind)
  (exists (lambda (f) (and (eq? (car f) 'effects) (pair? (cdr f)) (eq? (cadr f) kind))) (vector-ref v 2)))
(define (internal-effects? v) (flagged? v 'internal))
(define (remote-effects? v) (flagged? v 'remote))
(define (declares-prompts? v) (exists (lambda (f) (eq? (car f) 'prompts)) (vector-ref v 2)))
(define (declares-edits? v) (exists (lambda (f) (eq? (car f) 'edits)) (vector-ref v 2)))

(define (scoped-name? sym)
  ;; call-with-x and with-x run a thunk inside a setting: their effects are
  ;; the thunk's, and the setting's own are the scope, not a change
  (let ([s (symbol->string sym)])
    (or (string=? s "call-with")
        (and (>= (string-length s) 10) (string=? (substring s 0 10) "call-with-"))
        (and (>= (string-length s) 5) (string=? (substring s 0 5) "with-")))))

(define (join a b)
  (cond [(or (eq? a #t) (eq? b #t)) #t] [(or (eq? a 'opaque) (eq? b 'opaque)) 'opaque] [else #f]))

(define (callee-change callee)
  ;; what a call contributes: #f, opaque, #t, or the indices of the
  ;; caller's own arguments the callee changes through the forms handed
  ;; over; a form the caller made is scratch, anything else a change
  (let* ([ckey (car callee)] [args (cadr callee)] [ctx (caddr callee)]
         [cv (hashtable-ref all-definitions ckey #f)])
    (cond
      [(internal-effects? cv) #f]
      [(or (remote-effects? cv) (scoped-name? (cdr ckey))) 'opaque]
      [(eq? (vector-ref cv 6) #t) #t]
      [else
       (let loop ([indices (vector-ref cv 8)] [result (vector-ref cv 6)])
         (if (null? indices) result
             (let ([form (and (< (car indices) (length args)) (list-ref args (car indices)))])
               (if (not form) (loop (cdr indices) result)
                   (let ([verdict (judge-form form ctx)])
                     (cond
                       [(eq? verdict 'scratch) (loop (cdr indices) result)]
                       [(integer? verdict) (loop (cdr indices) (cons verdict (if (pair? result) result '())))]
                       [else #t]))))))])))

(define (settle-effects!)
  ;; the least fixed point: a definition's effect joins its witnesses' and
  ;; its callees'; an argument a callee changes is the caller's argument
  ;; in turn, its scratch, or a change of its own
  (let loop ()
    (let ([changed #f])
      (for-each
        (lambda (key)
          (let* ([v (hashtable-ref all-definitions key #f)]
                 [effect (fold-left (lambda (acc w) (join acc (case (car w) [(definite) #t] [(opaque) 'opaque] [else #f])))
                                    (vector-ref v 6) (vector-ref v 3))]
                 [indices (append (filter integer? (map car (vector-ref v 3))) (vector-ref v 8))]
                 [prompts? (or (vector-ref v 7) (vector-ref v 5))]
                 [edits? (or (vector-ref v 15) (vector-ref v 14))])
            (for-each
              (lambda (callee)
                (let ([change (callee-change callee)])
                  (when (vector-ref (hashtable-ref all-definitions (car callee) #f) 7) (set! prompts? #t))
                  (when (vector-ref (hashtable-ref all-definitions (car callee) #f) 15) (set! edits? #t))
                  (cond [(pair? change) (set! indices (append change indices))]
                        [else (set! effect (join effect change))])))
              (vector-ref v 4))
            (let ([indices (let dedupe ([l indices] [out '()]) (cond [(null? l) out] [(memv (car l) out) (dedupe (cdr l) out)] [else (dedupe (cdr l) (cons (car l) out))]))])
              (unless (and (eq? effect (vector-ref v 6)) (= (length indices) (length (vector-ref v 8))) (eq? prompts? (vector-ref v 7))
                           (eq? edits? (vector-ref v 15)))
                (vector-set! v 6 effect) (vector-set! v 8 indices) (vector-set! v 7 prompts?) (vector-set! v 15 edits?)
                (set! changed #t)))))
        definition-order)
      (when changed (loop)))))

(define (changes? v)
  ;; a definite change of its own or of an argument
  (or (eq? (vector-ref v 6) #t) (pair? (vector-ref v 8))))

(define (effect-chain key depth)
  ;; how a definition reaches a change: its first witness, else the first
  ;; callee that carries one and its chain
  (let ([v (hashtable-ref all-definitions key #f)])
    (cond
      [(find (lambda (w) (not (eq? (car w) 'opaque))) (vector-ref v 3)) => cdr]
      [(> depth 6) "..."]
      [else
       (let ([callee (find (lambda (c) (and (not (equal? (car c) key)) (let ([change (callee-change c)]) (or (eq? change #t) (pair? change))))) (vector-ref v 4))])
         (if callee
             (let ([cv (hashtable-ref all-definitions (car callee) #f)])
               (if (eq? (vector-ref cv 6) #t)
                   (format "~a -> ~a" (cdar callee) (effect-chain (car callee) (+ depth 1)))
                   (format "~a, changing ~a" (cdar callee)
                           (let ([form (list-ref (cadr callee) (car (vector-ref cv 8)))]) (if (symbol? form) form "an expression")))))
             "?"))])))

(define (prompt-chain key depth)
  (let ([v (hashtable-ref all-definitions key #f)])
    (cond
      [(vector-ref v 5) "a prompt of its own"]
      [(> depth 6) "..."]
      [else
       (let ([callee (find (lambda (c) (and (not (equal? (car c) key)) (vector-ref (hashtable-ref all-definitions (car c) #f) 7))) (vector-ref v 4))])
         (if callee (format "~a -> ~a" (cdar callee) (prompt-chain (car callee) (+ depth 1))) "?"))])))

(define (effects-report!)
  (for-each analyze-library! libraries)
  (set! definition-order (reverse definition-order))
  (for-each resolve-bodies! definition-order)
  (settle-effects!)
  (let ([disagreements 0] [checked 0])
    (for-each
      (lambda (entry)
        (let* ([name (car entry)] [path (caddr entry)])
          (when (selected? path)
            (let ([lines '()])
              (for-each
                (lambda (key)
                  (when (equal? (car key) path)
                    (let ([v (hashtable-ref all-definitions key #f)])
                      (when (and (eq? (vector-ref v 0) 'procedure) (or internal? (vector-ref v 1)))
                        (set! checked (+ checked 1))
                        (let ([declared? (or (internal-effects? v) (remote-effects? v))])
                          (define (say text)
                            (set! lines (cons (format "    ~a~a: ~a" (cdr key) (if (eq? (vector-ref v 10) (cdr key)) "" (format " (exported as ~a)" (vector-ref v 10))) text) lines))
                            (set! disagreements (+ disagreements 1)))
                          (cond
                            [(and (vector-ref v 11) (not (vector-ref v 6)) (not (changes? v)) (not declared?)) (say "reaches no effect")]
                            [(and (not (vector-ref v 11)) (changes? v) (not declared?) (not (scoped-name? (cdr key))))
                             (say (format "reaches ~a" (effect-chain key 0)))]
                            [(and (vector-ref v 11) (internal-effects? v)) (say "declares (effects internal) yet is named as a command")]
                            [else (void)])
                          (cond
                            [(and (vector-ref v 7) (not (declares-prompts? v))) (say (format "prompts, through ~a, without (prompts)" (prompt-chain key 0)))]
                            [(and (declares-prompts? v) (not (vector-ref v 7))) (say "declares (prompts) yet reaches none")]
                            [else (void)])
                          ;; a command whose purpose is editing the text declares (edits), for
                          ;; a listing to leave it out where the buffer is read-only; the
                          ;; declaration must reach an edit, while reaching one without it is
                          ;; allowed, a visit merging or a copy writing <copy> say
                          (when (and (declares-edits? v) (not (vector-ref v 15)))
                            (say "declares (edits) yet edits nothing")))))))
                definition-order)
              (unless (null? lines)
                (printf "~s  ~a\n" name path)
                (for-each (lambda (l) (printf "~a\n" l)) (reverse lines)))))))
      libraries)
    (printf "\neffects: ~a definitions checked, ~a disagreements\n" checked disagreements)
    (exit (min disagreements 100))))

(define totals (make-eq-hashtable))
(define (count! kind) (hashtable-update! totals kind (lambda (n) (+ n 1)) 0))
(define kinds '(procedure parameter syntax record value standard elsewhere))

(define (coverage-report!)
  (for-each
    (lambda (entry)
      (let* ([name (car entry)] [library (cadr entry)] [path (caddr entry)]
             [stem (let ([file (path-last path)]) (substring file 0 (- (string-length file) 4)))])
        (when (or (null? selected) (member stem selected))
          (let* ([rows (map (lambda (export) (cons (car export) (classify name (car export) 0))) (exports-of library))]
                 [documented (filter (lambda (r) (eq? (cdr r) 'documented)) rows)])
            (for-each (lambda (r) (count! (cdr r))) rows)
            (for-each (lambda (form) (note-types! name form)) (cdddr library))
            (printf "~24a ~3a of ~3a documented" (format "~s" name) (length documented) (length rows))
            (let ([present (filter (lambda (k) (exists (lambda (r) (eq? (cdr r) k)) rows)) kinds)])
              (printf "  ~a\n"
                (apply string-append
                  (map (lambda (k) (format " ~a ~a" (length (filter (lambda (r) (eq? (cdr r) k)) rows)) k)) present))))
            (when listing?
              (for-each (lambda (k)
                          (let ([names (map car (filter (lambda (r) (eq? (cdr r) k)) rows))])
                            (when (pair? names) (printf "    ~a: ~a\n" k names))))
                kinds))))))
    libraries)

  (printf "\ntotal:")
  (for-each (lambda (k) (printf " ~a ~a" (hashtable-ref totals k 0) k)) (cons 'documented kinds))
  (newline)
  (let ([unknown (filter (lambda (use) (unknown-type? (car use))) used-types)])
    (printf "types: ~a defined by libraries, ~a unknown\n" (hashtable-size defined-types) (length unknown))
    (for-each (lambda (use) (printf "    ~s in ~s, ~a\n" (car use) (cadr use) (caddr use))) (reverse unknown))))

(if effects? (effects-report!) (coverage-report!))
