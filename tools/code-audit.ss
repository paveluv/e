;; Advisory analysis. Never interpret absence of a repository reference as
;; permission to remove an exported API, or a pattern as a safe rewrite.

(define (source-resolver sources)
  (let ([cache (make-hashtable equal-hash equal?)] [by-name (make-hashtable equal-hash equal?)]
        [standard (make-eq-hashtable)])
    (define (resolve source name seen)
      (let* ([path (car source)] [key (cons path name)])
        (if (member key seen) key
          (or (hashtable-ref cache key #f)
            (let* ([form (stripped (caddr source))]
                   [own? (exists (lambda (e) (eq? name (vector-ref e 0))) (cadddr source))]
                   [answer
                    (or (and own? key)
                      (exists
                        (lambda (spec)
                          (source-import spec name
                            (lambda (library external)
                              (let* ([choices (filter (lambda (s) (assq external (exports-of (stripped (caddr s)))))
                                                (hashtable-ref by-name library '()))]
                                     [target (or (find (lambda (s) (equal? (path-parent (path-parent (car s)))
                                                                     (path-parent (path-parent path)))) choices)
                                                 (and (pair? choices) (car choices)))])
                                (cond
                                  [target (let ([export (assq external (exports-of (stripped (caddr target))))])
                                            (and export (resolve target (cdr export) (cons key seen))))]
                                  [(and (memq (car library) '(chezscheme scheme rnrs)) (eq-hashtable-contains? standard external))
                                   (cons 'scheme external)]
                                  [else #f])))))
                        (reverse (import-specs form)))
                      key)])
              (hashtable-set! cache key answer) answer)))))
    (for-each (lambda (name) (eq-hashtable-set! standard name #t)) (environment-symbols (environment '(chezscheme))))
    (for-each (lambda (s) (hashtable-update! by-name (cadr (stripped (caddr s))) (lambda (xs) (cons s xs)) '())) sources)
    (lambda (source name) (resolve source name '()))))

(define (audit-apis sources evidence)
  ;; Return #(path external-name source evidence) for exports without a
  ;; code reference or public declaration. Evidence is review guidance.
  (let ([resolve (source-resolver sources)] [used (make-hashtable equal-hash equal?)] [result '()])
    (for-each
      (lambda (s)
        (let ([form (stripped (caddr s))])
          (define (note! name owner)
            (let ([key (resolve s name)])
              (unless (or (equal? key (cons (car s) owner)) (hashtable-contains? used key))
                (hashtable-set! used key #t)
                ;; Common libraries run in both runtime graphs. An imported
                ;; seam API may resolve to either implementation; retaining
                ;; both avoids mistaking the base half for an unused export.
                (let ([origin (assoc (car key) sources)])
                  (when origin
                    (let* ([form (stripped (caddr origin))]
                           [export (find (lambda (e) (eq? (cdr key) (cdr e))) (exports-of form))])
                      (when export
                        (for-each
                          (lambda (other)
                            (when (equal? (cadr (stripped (caddr other))) (cadr form))
                              (cond [(assq (car export) (exports-of (stripped (caddr other)))) =>
                                     (lambda (e) (hashtable-set! used (cons (car other) (cdr e)) #t))]))) sources))))))))
          (for-each
            (lambda (x)
              (cond [(source-definition x) =>
                     (lambda (d) (source-normalize (cdr d) (lambda (name) (note! name (car d)) name)))]
                    [(and (pair? x) (eq? (car x) 'edoc)) (void)]
                    [else (for-each (lambda (name) (note! name #f)) (source-symbols x))]))
            (map stripped (source-body (cddddr form))))))
      sources)
    (for-each
      (lambda (s)
        (let* ([form (stripped (caddr s))] [prefix (string-append (symbol->string (car (reverse (cadr form)))) ":")])
          (for-each
            (lambda (export)
              (let* ([name (cdr export)] [entry (find (lambda (e) (eq? name (vector-ref e 0))) (cadddr s))]
                     [spelling (string->symbol (string-append prefix (symbol->string (car export))))])
                ;; Aliases inherit documentation from their origin and
                ;; cannot carry a separate edoc/public declaration.
                (when (and entry (not (symbol? (vector-ref entry 1))) (not (vector-ref entry 3))
                        (not (hashtable-ref used (cons (car s) name) #f)))
                  (set! result
                    (cons (vector (car s) (car export) (vector-ref entry 2)
                            (cond [(find (lambda (e) (memq spelling (cdr e))) evidence) => car]
                                  [else 'unreferenced])) result)))))
            (exports-of form)))) sources)
    (reverse result)))

(define (source-words text)
  ;; Names in prose/code fences are evidence, not executable references.
  (define (constituent? c)
    (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\: #\- #\! #\? #\* #\/ #\+ #\< #\> #\= #\_ #\%))))
  (let loop ([i 0] [begin #f] [out '()])
    (cond [(= i (string-length text))
           (if begin (cons (string->symbol (substring text begin i)) out) out)]
          [(constituent? (string-ref text i)) (loop (+ i 1) (or begin i) out)]
          [else (loop (+ i 1) #f (if begin (cons (string->symbol (substring text begin i)) out) out))])))

(define (pattern-size x)
  ;; Count syntax nodes, not source lines. Normalized identifiers are one
  ;; node; opaque data is not a candidate for extracting executable code.
  (cond [(list? x) (+ 1 (fold-left (lambda (n x) (+ n (pattern-size x))) 0 x))]
        [(pair? x) (+ 1 (pattern-size (car x)) (pattern-size (cdr x)))] [else 1]))

(define (common-pattern a b)
  ;; Anti-unification: repeated pairs of differing subtrees share a hole.
  ;; Return the template and substitutions, which make both inputs again.
  (let ([holes '()])
    (define (walk a b)
      (cond [(equal? a b) a]
            [(and (pair? a) (pair? b) (list? a) (list? b) (= (length a) (length b)))
             (map walk a b)]
            [else
             (let* ([values (list a b)] [known (assoc values holes)]
                    [n (if known (cdr known) (length holes))])
               (unless known (set! holes (cons (cons values n) holes)))
               (vector 'hole n))]))
    (let ([pattern (walk a b)]) (values pattern (reverse holes)))))

(define (pattern-saving a b pattern holes)
  ;; Charge a helper definition, its parameters, both calls and all actual
  ;; substitutions. Nontrivial substitutions also need deferred evaluation;
  ;; charge callback wrappers rather than assuming eager extraction is safe.
  (- (+ (pattern-size a) (pattern-size b))
     (+ (pattern-size pattern) 6 (* 3 (length holes))
        (fold-left
          (lambda (n h)
            (+ n (fold-left (lambda (n x) (+ n (pattern-size x) (if (pair? x) 4 0))) 0 (car h))))
          0 holes))))

(define (audit-patterns sources)
  ;; Buckets bound comparisons to 12 recent representatives. These are
  ;; suggestions, not an exhaustive clone search. Largest nonoverlapping
  ;; savings win, so a function and all its children do not flood the report.
  ;; Candidate: #(source owner expression first-node last-node).
  (let ([resolve (source-resolver sources)] [buckets (make-hashtable equal-hash equal?)] [matches '()] [serial 0])
    (define (tag x)
      (cond [(and (vector? x) (eq? (vector-ref x 0) 'free)) (cdr (vector-ref x 1))]
            [(pair? x) (tag (car x))] [else x]))
    (define (consider s owner x first last)
      (let* ([candidate (vector s owner x first last)]
             [key (list (tag x) (length x))]
             [peers (hashtable-ref buckets key '())])
        (for-each
          (lambda (peer)
            (unless (and (eq? s (vector-ref peer 0))
                      (<= first (vector-ref peer 4)) (<= (vector-ref peer 3) last))
              (let-values ([(pattern holes) (common-pattern (vector-ref peer 2) x)])
                (let ([saving (pattern-saving (vector-ref peer 2) x pattern holes)])
                  (when (and (<= (length holes) 4) (>= saving 24))
                    (set! matches (cons (vector saving peer candidate pattern holes) matches))))))) peers)
        (hashtable-set! buckets key (cons candidate (if (>= (length peers) 12) (list-head peers 11) peers)))))
    (define (walk s owner x)
      (set! serial (+ serial 1))
      (let* ([first serial]
             [size (cond [(list? x) (+ 1 (fold-left (lambda (n x) (+ n (walk s owner x))) 0 x))]
                         [(pair? x) (+ 1 (walk s owner (car x)) (walk s owner (cdr x)))] [else 1])])
        (when (pair? x)
          (when (and (list? x) (>= size 48)
                  (or (memq (car x) '(lambda case-lambda let let* letrec letrec* if begin when unless))
                      (and (vector? (car x)) (eq? (vector-ref (car x) 0) 'free))))
            (consider s owner x first serial)))
        size))
    (for-each
      (lambda (s)
        (for-each
          (lambda (entry)
            (let ([expr (vector-ref entry 1)])
              (when (and (pair? expr) (memq (car expr) '(lambda case-lambda)))
                (walk s entry (source-normalize expr (lambda (name) (resolve s name)))))))
          (cadddr s))) sources)
    (let ([claimed '()])
      (filter
        (lambda (match)
          (let ([pair (list (vector-ref match 1) (vector-ref match 2))])
            (and (not (exists (lambda (a)
                                (exists (lambda (b) (and (eq? (vector-ref a 0) (vector-ref b 0))
                                                         (<= (vector-ref a 3) (vector-ref b 4))
                                                         (<= (vector-ref b 3) (vector-ref a 4)))) claimed)) pair))
              (begin (set! claimed (append pair claimed)) #t))))
        (list-sort (lambda (a b) (> (vector-ref a 0) (vector-ref b 0))) matches)))))
