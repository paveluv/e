;; Conservative liveness, without loading product libraries. Exports are
;; roots: an absent repository caller does not prove an external API dead.
;; Unknown syntax and quoted code retain every identifier they mention.

(define (source-symbols x)
  (let ([seen (make-eq-hashtable)])
    (let walk ([x x])
      (cond [(symbol? x) (eq-hashtable-set! seen x #t)]
            [(pair? x) (walk (car x)) (walk (cdr x))]
            [(vector? x) (vector-for-each walk x)]))
    (vector->list (hashtable-keys seen))))

(define (source-formals x)
  (cond [(null? x) '()] [(pair? x) (cons (car x) (source-formals (cdr x)))]
        [(symbol? x) (list x)] [else '()]))

(define (source-definition x)
  ;; (name . initializer), making procedure shorthand explicit.
  (and (pair? x) (memq (car x) '(define edefine))
    (pair? (cdr x))
    (let* ([target (cadr x)]
           [body (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'edoc)))) (cddr x))])
      (if (pair? target)
          (cons (car target) (cons 'lambda (cons (cdr target) body)))
          (cons target (if (null? body) '(void) (car body)))))))

(define (source-body forms)
  (apply append
    (map (lambda (form)
           (if (and (pair? (parts form)) (eq? (stripped (car (parts form))) 'begin))
               (source-body (cdr (parts form))) (list form))) forms)))

(define (source-normalize expression reference)
  ;; Return an alpha-normalized expression; reference maps free identifiers
  ;; to binding identities and also lets liveness collect dependency edges.
  ;; Only understood binders are normalized. Opaque syntax stays literal.
  (define serial 0)
  (define (bind names env)
    (append (map (lambda (name)
                   (set! serial (+ serial 1)) (cons name (vector 'local serial))) names) env))
  (define (atom x env)
    (cond [(assq x env) => cdr] [else (vector 'free (reference x))]))
  (define (opaque x)
    (for-each reference (source-symbols x))
    (vector 'opaque x))
  (define (formals x env)
    (cond [(pair? x) (cons (formals (car x) env) (formals (cdr x) env))]
          [(symbol? x) (atom x env)] [else x]))
  (define (body forms env)
    (let* ([forms (map stripped (source-body forms))]
           [defs (filter values (map source-definition forms))]
           [env (bind (map car defs) env)])
      (map (lambda (x)
             (cond [(source-definition x) =>
                    (lambda (d) (list 'define (atom (car d) env) (walk (cdr d) env)))]
                   [else (walk x env)])) forms)))
  (define (walk x env)
    (cond
      [(symbol? x) (atom x env)]
      [(not (pair? x)) x]
      [(or (not (list? x)) (assq (car x) env))
       (if (list? x) (map (lambda (v) (walk v env)) x) (opaque x))]
      [else
       (case (car x)
         ;; Quote can contain delayed commands or macro templates. Retain
         ;; such references conservatively, but never rename quoted data.
         [(quote syntax quasiquote quasisyntax) (opaque x)]
         [(lambda)
          (let ([env (bind (source-formals (cadr x)) env)])
            (cons 'lambda (cons (formals (cadr x) env) (body (cddr x) env))))]
         [(case-lambda)
          (cons 'case-lambda
            (map (lambda (clause)
                   (let ([env (bind (source-formals (car clause)) env)])
                     (cons (formals (car clause) env) (body (cdr clause) env)))) (cdr x)))]
         [(let*)
          (let loop ([bindings (cadr x)] [env env] [out '()])
            (if (null? bindings) (cons 'let* (cons (reverse out) (body (cddr x) env)))
                (let* ([b (car bindings)] [v (walk (cadr b) env)] [next (bind (list (car b)) env)])
                  (loop (cdr bindings) next (cons (list (cdar next) v) out)))))]
         [(let letrec letrec*)
          (let* ([named? (symbol? (cadr x))]
                 [bindings (if named? (caddr x) (cadr x))]
                 [outer env]
                 [env (if named? (bind (list (cadr x)) env) env)]
                 [all (bind (map car bindings) env)]
                 [recursive? (memq (car x) '(letrec letrec*))]
                 [normalized
                  (map (lambda (b)
                         (let ([v (walk (cadr b) (cond [recursive? all] [named? outer] [else env]))]
                               [slot (cdr (assq (car b) all))])
                           (list slot v))) bindings)])
            (append (list (car x)) (if named? (list (atom (cadr x) all)) '())
              (list normalized) (body (if named? (cdddr x) (cddr x)) all)))]
         [(let*-values)
          (let loop ([bindings (cadr x)] [env env] [out '()])
            (if (null? bindings) (cons 'let*-values (cons (reverse out) (body (cddr x) env)))
                (let* ([b (car bindings)] [v (walk (cadr b) env)] [next (bind (source-formals (car b)) env)])
                  (loop (cdr bindings) next (cons (list (formals (car b) next) v) out)))))]
         [(let-values)
          (let* ([bindings (cadr x)] [all (bind (apply append (map (lambda (b) (source-formals (car b))) bindings)) env)]
                 [normalized
                  (map (lambda (b)
                         (let ([v (walk (cadr b) env)])
                           (list (formals (car b) all) v))) bindings)])
            (cons (car x) (cons normalized (body (cddr x) all))))]
         [(define-syntax let-syntax letrec-syntax syntax-case syntax-rules
            define-record-type define-condition-type meta include)
          (opaque x)]
         [(edoc) '(edoc)]
         ;; These forms do not introduce bindings. Unknown forms might be
         ;; macros with different binding rules: scanning them literally
         ;; keeps dependencies rather than accidentally dropping one.
         [(if begin and or when unless cond case set! parameterize
            fluid-let dynamic-wind)
          (cons (car x) (map (lambda (v) (walk v env)) (cdr x)))]
         [else
          ;; Procedure applications cannot bind arguments. But without
          ;; expansion an operator could be a macro: retain raw names too.
          (let ([result (map (lambda (v) (walk v env)) x)])
            (for-each (lambda (name) (unless (assq name env) (reference name))) (source-symbols x))
            result)])]))
  (walk expression '()))

(define (health-library form report!)
  ;; Return definition entries for the advisory API/clone report, while
  ;; enforcing unreachability only for safely removable private definitions.
  ;; Entry: #(name initializer source public? normalized).
  (let* ([library (stripped form)] [exports (map cdr (exports-of library))]
         [definitions (make-eq-hashtable)] [edges (make-eq-hashtable)]
         [roots exports] [entries '()] [initializers '()])
    (let loop ([forms (source-body (cddddr (parts form)))] [doc #f])
      (unless (null? forms)
        (let* ([at (car forms)] [x (stripped at)] [d (source-definition x)])
          (cond
            [(and (pair? x) (eq? (car x) 'edoc))
             (if (string? (cadr x)) (loop (cdr forms) x)
                 (begin (set! roots (cons (cadr x) roots)) (loop (cdr forms) #f)))]
            [d
             (let* ([doc (or doc (find (lambda (f) (and (pair? f) (eq? (car f) 'edoc))) (cddr x)))]
                    [public? (and doc (member '(public) (cddr doc)) #t)]
                    [entry (vector (car d) (cdr d) at public? #f)]
                    [value (cdr d)])
               (eq-hashtable-set! definitions (car d) entry)
               (set! entries (cons entry entries))
               (when public? (set! roots (cons (car d) roots)))
               (when (and public? (not (memq (car d) exports)))
                 (report! at "a public declaration requires an exported definition"))
               ;; Unknown initializers may have effects. Only a literal,
               ;; alias or closure can be discarded without running it.
               (unless (or (not (pair? value)) (memq (car value) '(lambda case-lambda quote)))
                 (set! roots (cons (car d) roots))))
             (loop (cdr forms) #f)]
            [else (set! initializers (cons x initializers)) (loop (cdr forms) #f)]))))
    (for-each
      (lambda (entry)
        (let ([refs (make-eq-hashtable)])
          (vector-set! entry 4
            (source-normalize (vector-ref entry 1)
              (lambda (name) (eq-hashtable-set! refs name #t) name)))
          (eq-hashtable-set! edges (vector-ref entry 0) (vector->list (hashtable-keys refs))))) entries)
    (for-each (lambda (x) (set! roots (append (source-symbols x) roots))) initializers)
    ;; include or generated identifiers can hide dependencies from source
    ;; inspection. Do not issue deletion errors for that library.
    (when (exists (lambda (x) (exists (lambda (s) (memq s '(include datum->syntax))) (source-symbols x))) initializers)
      (set! roots (append (map (lambda (e) (vector-ref e 0)) entries) roots)))
    (let ([live (make-eq-hashtable)])
      (let visit ([todo roots])
        (unless (null? todo)
          (let ([name (car todo)])
            (if (eq-hashtable-contains? live name) (visit (cdr todo))
                (begin
                  (eq-hashtable-set! live name #t)
                  (visit (append (eq-hashtable-ref edges name '()) (cdr todo))))))))
      (for-each
        (lambda (entry)
          (unless (eq-hashtable-contains? live (vector-ref entry 0))
            (report! (vector-ref entry 2)
              (format "unreachable private definition ~a" (vector-ref entry 0)))))
        (reverse entries)))
    (reverse entries)))
