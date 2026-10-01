;; Source attribution is lexical: a library-level definition owns its local
;; helpers and callbacks. Export renames give public procedures their M-x name;
;; private definitions use the same module prefix without becoming exports.
(define (check-log-sources! form report!)
  (define (datum x) (if (annotation? x) (annotation-stripped x) x))
  (define (parts x) (if (annotation? x) (annotation-expression x) x))
  (define (imports spec)
    (cond
      [(equal? spec '(service log)) '(add!)]
      [(not (pair? spec)) '()]
      [else
       (case (car spec)
         [(prefix) (map (lambda (s) (string->symbol (string-append (symbol->string (caddr spec)) (symbol->string s)))) (imports (cadr spec)))]
         [(only) (filter (lambda (s) (memq s (cddr spec))) (imports (cadr spec)))]
         [(except) (filter (lambda (s) (not (memq s (cddr spec)))) (imports (cadr spec)))]
         [(rename) (map (lambda (s) (cond [(assq s (cddr spec)) => cadr] [else s])) (imports (cadr spec)))]
         [(for library) (imports (cadr spec))]
         [else '()])]))
  (let* ([body (parts form)] [library (datum (cadr body))]
         [prefix (string-append (symbol->string (car (reverse library))) ":")]
         [exports (cdr (datum (caddr body)))]
         [names (apply append (map imports (cdr (datum (cadddr body)))))])
    (define (qualified name)
      (let ([external
             (or (and (memq name exports) name)
                 (exists (lambda (spec)
                           (and (pair? spec) (eq? (car spec) 'rename)
                                (cond [(assq name (cdr spec)) => cadr] [else #f]))) exports)
                 name)])
        (string->symbol (string-append prefix (symbol->string external)))))
    (define (mentions-logger? x)
      (let ([p (parts x)])
        (or (memq (datum x) names)
            (and (pair? p) (or (mentions-logger? (car p)) (mentions-logger? (cdr p)))))))
    (define (template x owner depth opener escapes)
      (let ([p (parts x)])
        (cond
          [(vector? p) (for-each (lambda (x) (template x owner depth opener escapes)) (vector->list p))]
          [(pair? p)
           (cond
             [(memq (datum (car p)) escapes)
              (if (= depth 1) (walk (cadr p) owner) (template (cadr p) owner (- depth 1) opener escapes))]
             [(eq? (datum (car p)) opener) (template (cadr p) owner (+ depth 1) opener escapes)]
             [else
              (let loop ([xs p])
                (cond [(pair? xs) (template (car xs) owner depth opener escapes) (loop (cdr xs))]
                      [(not (null? xs)) (template xs owner depth opener escapes)]))])])))
    (define (walk x owner)
      (let ([p (parts x)])
        (cond
          [(memq (datum x) names)
           (report! x owner "log:add! must be called directly with a literal source")]
          [(pair? p)
           (let ([op (datum (car p))])
             (cond
               [(memq op '(quote syntax)) (void)]
               [(eq? op 'define-syntax)
                (when (mentions-logger? x)
                  (report! x owner "logging macros must delegate to a named function"))]
               [(eq? op 'quasiquote) (template (cadr p) owner 1 op '(unquote unquote-splicing))]
               [(eq? op 'quasisyntax) (template (cadr p) owner 1 op '(unsyntax unsyntax-splicing))]
               [(memq op names)
                (unless (and owner (pair? (cdr p)) (equal? (datum (cadr p)) (list 'quote owner)))
                  (report! x owner "log source must be the enclosing definition's qualified name"))
                (when (pair? (cdr p)) (for-each (lambda (a) (walk a owner)) (cddr p)))]
               [else
                (let loop ([xs p])
                  (cond [(pair? xs) (walk (car xs) owner) (loop (cdr xs))]
                        [(not (null? xs)) (walk xs owner)]))]))])))
    (define (top x)
      (let ([p (parts x)])
        (cond
          [(and (pair? p) (eq? (datum (car p)) 'begin)) (for-each top (cdr p))]
          [(and (pair? p) (memq (datum (car p)) '(define define-operation)))
           (let* ([target (datum (cadr p))] [name (if (pair? target) (car target) target)])
             (for-each (lambda (x) (walk x (qualified name))) (cddr p)))]
          [else (walk x #f)])))
    (for-each top (cddddr body))))
