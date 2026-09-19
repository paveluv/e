#!/usr/bin/env scheme-script
;; edoc-coverage.sps -- which exported names carry an edoc, library by
;; library, read from the sources:
;;
;;   tools/edoc-coverage.sps [--list] [library ...]
;;
;; Every export is classified by its definition: edefine (documented, by
;; any of the edefine forms), procedure (define with a lambda list, or a
;; lambda/case-lambda value), parameter, syntax, record (constructor,
;; predicate or field procedure of a define-record-type), value (any other
;; define), standard (a name of (rnrs) or (chezscheme) passed on), or
;; elsewhere (a name this scan cannot place). A re-exported import, or an
;; alias of one, is classified where it is defined, through the import
;; specs.
;; --list prints the undocumented names of each library; library
;; arguments restrict the report to those stems.
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

(define (definitions library)
  ;; (name . kind) for every top-level definition of the library body
  (apply append
    (map (lambda (form)
           (if (not (pair? form)) '()
               (case (car form)
                 [(edefine) (list (cons (if (pair? (cadr form)) (car (cadr form)) (cadr form)) 'edefine))]
                 [(edefine-syntax) (list (cons (cadr form) 'edefine))]
                 [(edefine-record-type) (map (lambda (n) (cons n 'edefine)) (record-names (cons 'define-record-type (cons (cadr form) (cdddr form)))))]
                 [(define)
                  (let ([target (cadr form)])
                    (cond [(pair? target) (list (cons (car target) 'procedure))]
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
         (cdddr library))))

;;; The libraries, indexed by name ------------------------------------------------

(define libraries
  ;; (name library) for every library form under lib
  (apply append
    (map (lambda (path)
           (let ([library (find (lambda (f) (and (pair? f) (eq? (car f) 'library))) (read-forms path))])
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
         [def (assq internal (definitions-of name))])
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
(define list? (and (member "--list" arguments) #t))
(define selected (filter (lambda (a) (not (string=? a "--list"))) arguments))

(define totals (make-eq-hashtable))
(define (count! kind) (hashtable-update! totals kind (lambda (n) (+ n 1)) 0))
(define kinds '(procedure parameter syntax record value standard elsewhere))

(for-each
  (lambda (entry)
    (let* ([name (car entry)] [library (cadr entry)] [path (caddr entry)]
           [stem (let ([file (path-last path)]) (substring file 0 (- (string-length file) 4)))])
      (when (or (null? selected) (member stem selected))
        (let* ([rows (map (lambda (export) (cons (car export) (classify name (car export) 0))) (exports-of library))]
               [documented (filter (lambda (r) (eq? (cdr r) 'edefine)) rows)])
          (for-each (lambda (r) (count! (cdr r))) rows)
          (printf "~24a ~3a of ~3a documented" (format "~s" name) (length documented) (length rows))
          (let ([present (filter (lambda (k) (exists (lambda (r) (eq? (cdr r) k)) rows)) kinds)])
            (printf "  ~a\n"
              (apply string-append
                (map (lambda (k) (format " ~a ~a" (length (filter (lambda (r) (eq? (cdr r) k)) rows)) k)) present))))
          (when list?
            (for-each (lambda (k)
                        (let ([names (map car (filter (lambda (r) (eq? (cdr r) k)) rows))])
                          (when (pair? names) (printf "    ~a: ~a\n" k names))))
              kinds))))))
  libraries)

(printf "\ntotal:")
(for-each (lambda (k) (printf " ~a ~a" (hashtable-ref totals k 0) k)) (cons 'edefine kinds))
(newline)
