;; Portable endpoint contracts, shared by base resolution and head mirrors.
(import (only (foundation edoc) elibrary))
(elibrary (core port)
  (export catalogue dependencies describe key observe! project register! resolve unobserve!)
  (import (chezscheme) (prefix (core kernel) kernel:)
          (prefix (foundation datum) datum:) (prefix (foundation edoc) edoc:))
  (define definitions (kernel:make-registry car))
  (define (natural? n) (and (integer? n) (exact? n) (>= n 0)))
  (define (key? k)
    (and (list? k) (= (length k) 3) (memq (car k) '(model view))
      (symbol? (cadr k)) (natural? (caddr k)) (> (caddr k) 0)))
  (define (get value name)
    (and (list? value) (for-all pair? value)
      (let ([matches (filter (lambda (p) (eq? (car p) name)) value)])
        (and (= (length matches) 1) (car matches)))))
  (define (selector? kind direction s)
    (and (list? s) (pair? s)
      (case (car s)
        [(value) (and (eq? kind 'model) (for-all (lambda (x) (or (symbol? x) (natural? x))) (cdr s)))]
        [(state options) (and (eq? kind 'view) (for-all (lambda (x) (or (symbol? x) (natural? x))) (cdr s)))]
        [(source) (and (eq? kind 'view) (null? (cdr s)))]
        [(source-text) (and (eq? kind 'view) (eq? direction 'output) (null? (cdr s)))]
        [(id) (and (eq? direction 'output) (null? (cdr s)))]
        [else #f])))
  (define (declarations? k ds)
    (and (list? ds)
      (let walk ([rest ds] [seen '()])
        (or (null? rest)
          (let ([d (car rest)])
            (and (list? d) (= (length d) 4) (memq (car d) '(input output))
              (symbol? (cadr d)) (not (memq (cadr d) seen))
              (edoc:type-portable? (caddr d)) (selector? (car k) (car d) (cadddr d))
              (walk (cdr rest) (cons (cadr d) seen))))))))

  (edoc "Register data-only (direction name type selector) ports for (model|view kind schema), owned by the current module."
        (contract list "versioned endpoint kind") (declarations list "concrete portable ports"))
  (define (register! contract declarations)
    (let ([k (datum:copy contract)] [ds (datum:copy declarations)])
      (unless (and (key? k) (declarations? k ds)) (error 'register! "invalid port contract" contract declarations))
      (kernel:registry-add! definitions (cons k ds))))

  (edoc "Read a current endpoint contract, or #f when its definition/types are unavailable."
        (contract list "(model|view kind schema)") (returns any))
  (define (describe contract)
    (let ([d (kernel:registry-find definitions (lambda (d) (equal? (car d) contract)))])
      (and d (declarations? (car d) (cdr d)) (datum:copy (cdr d)))))

  (edoc "Read the portable contract catalogue used to fence base/head declaration differences." (returns list))
  (define (catalogue)
    (list-sort (lambda (a b) (string<? (format "~s" (car a)) (format "~s" (car b))))
      (filter values (map (lambda (d) (let ([ds (describe (car d))]) (and ds (cons (car d) ds)))) (kernel:registry-items definitions)))))

  (edoc "The port-contract key of a model envelope, interpreting a widget-view's declared kind."
        (envelope any "model envelope") (returns any))
  (define (key envelope)
    (let ([k (get envelope 'kind)] [s (get envelope 'schema)] [v (get envelope 'value)])
      (and k s
        (if (eq? (cdr k) 'widget-view)
          (let ([k (and v (get (cdr v) 'kind))] [s (and v (get (cdr v) 'schema))])
            (and k s (list 'view (cdr k) (cdr s))))
          (list 'model (cdr k) (cdr s))))))

  (edoc "Project a declared port without I/O: (ready value) or (unavailable reason). Source is an optional text envelope."
        (envelope any "owned endpoint snapshot") (name symbol "input or output name")
        (source any "text source with revision and vector value, or #f") (returns list))
  (define (project envelope name source)
    (guard (ex [else '(unavailable invalid-value)])
      (call/cc
        (lambda (fail)
          (define (missing) (fail '(unavailable missing-value)))
          (define (field value key) (let ([p (get value key)]) (if p (cdr p) (missing))))
          (let* ([k (key envelope)] [ds (and k (describe k))]
                 [d (and ds (find (lambda (d) (eq? (cadr d) name)) ds))])
            (unless d (fail '(unavailable contract)))
            (let* ([s (cadddr d)]
                   [value (case (car s)
                            [(id) (field envelope 'id)]
                            [(value) (field envelope 'value)]
                            [(state options source) (field (field envelope 'value) (car s))]
                            [(source-text)
                             (let ([lines (field source 'value)])
                               (unless (and (vector? lines) (= (vector-length lines) 1)) (missing))
                               (vector-ref lines 0))])]
                   [value (fold-left
                            (lambda (v part)
                              (cond [(symbol? part) (field v part)]
                                [(vector? v) (if (< part (vector-length v)) (vector-ref v part) (missing))]
                                [(list? v) (if (< part (length v)) (list-ref v part) (missing))]
                                [else (missing)])) value (cdr s))]
                   [copy (datum:copy value)])
              (if (edoc:type-accepts? (caddr d) copy) (list 'ready copy) '(unavailable type))))))))

  (edoc "Observe endpoint or type-definition changes; callbacks run after registry publication."
        (proc procedure "zero-argument invalidation") (returns any))
  (define (observe! proc)
    (list (kernel:registry-observe! definitions (lambda (removed added) (proc)))
      (edoc:observe-types! (lambda (removed added) (proc)))))

  (edoc "Release both owned contract observers, including queued callbacks." (token list "observation handle"))
  (define (unobserve! token) (for-each kernel:registry-unobserve! token))

  (edoc "Collect effective model/text dependencies of endpoints from owned binding rows. Read must return locally captured envelopes; no provider callbacks are invoked."
        (ids list "endpoint IDs") (edges list "(owner consumer input producer) rows")
        (read procedure "captured model lookup") (returns list))
  (define (dependencies ids edges read)
    (let ([seen '()] [ids-read '()] [texts '()])
      (define (visit id name)
        (unless (member (list id name) seen)
          (set! seen (cons (list id name) seen))
          (unless (member id ids-read) (set! ids-read (cons id ids-read)))
          (let* ([r (read id)] [k (and r (key r))] [ds (and k (describe k))]
                 [ds (and ds (if name (filter (lambda (d) (eq? name (cadr d))) ds) ds))])
            (when ds
              (for-each
                (lambda (d)
                  (let ([edge (find (lambda (e) (and (equal? id (cadr e)) (eq? (cadr d) (caddr e)))) edges)])
                    (cond [(and (eq? (car d) 'input) edge) (visit (car (cadddr edge)) (cadr (cadddr edge)))]
                      [(or (equal? (cadddr d) '(source-text)) (equal? (cadddr d) '(source))
                         (and (eq? (car k) 'view) (eq? (car (cadddr d)) 'state)))
                       (let ([source (get (cdr (get r 'value)) 'source)])
                         (when (and source (cdr source))
                           (if (eq? (cadr source) 'model) (visit (cdr source) #f)
                             (unless (member (cdr source) texts) (set! texts (cons (cdr source) texts))))))]
                      [else (void)]))) ds)))))
      (for-each (lambda (id) (visit id #f)) ids) (list (reverse ids-read) (reverse texts))))

  (edoc "Resolve one port against captured model/text lookups. Return (ready value basis), (pending reason basis), or (unavailable reason basis); false is a value."
        (id list "endpoint") (name symbol "port") (edges list "owned binding rows")
        (read procedure "captured available model lookup") (text procedure "captured text lookup") (returns list))
  (define (resolve id name edges read text)
    (let ([basis '()] [seen '()])
      (define (remember r)
        (when r
          (let ([id (cdr (get r 'id))])
            (unless (assoc id basis)
              (set! basis (cons (list id (cdr (get r 'revision))
                                  (and (eq? (cdr (get r 'kind)) 'widget-view) (cdr (get r 'value)))) basis))))))
      (define (lookup id name)
        (let* ([r (read id)] [k (and r (key r))] [ds (and k (describe k))]
               [d (and ds (find (lambda (d) (eq? name (cadr d))) ds))])
          (remember r)
          (cond [(or (not r) (not d)) '(unavailable contract)]
            [(member (list id name) seen) '(unavailable cycle)]
            [else
             (set! seen (cons (list id name) seen))
             (let ([edge (and (eq? (car d) 'input)
                              (find (lambda (e) (and (equal? id (cadr e)) (eq? name (caddr e)))) edges))])
               (if edge
                   (let* ([p (cadddr edge)] [producer (read (car p))]
                          [contracts (and producer (describe (key producer)))]
                          [out (and contracts (find (lambda (d) (eq? (cadr p) (cadr d))) contracts))])
                     (if (and out (eq? (car out) 'output) (edoc:type-compatible? (caddr out) (caddr d)))
                       (lookup (car p) (cadr p)) '(unavailable incompatible-contract)))
                   (let* ([v (cdr (get r 'value))] [source (and (eq? (car k) 'view) (get v 'source))]
                          [source (and source (cdr source))]
                          [s (and source (if (eq? (car source) 'buffer) (text source) (read source)))])
                     (when s
                       (if (eq? (car source) 'buffer)
                         (set! basis (cons (list source (cdr (get s 'revision))) basis)) (remember s)))
                     (if (and (eq? (car k) 'view) (eq? (car (cadddr d)) 'state)
                           (get v 'basis) (cdr (get v 'basis))
                           (or (not s) (not (equal? (cdr (get v 'basis)) (cdr (get s 'revision))))))
                       '(pending source-basis) (project r name s)))))])))
      (let ([result (lookup id name)]) (append result (list (reverse basis))))))

  (define builtin
    (kernel:call-with-runtime-registrations
      (lambda ()
        (register! '(view entry 1) '((output text string (source-text))))
        (register! '(view filter 1) '((output text string (source-text))))
        (register! '(view label 1) '((input text string (options text))))
        (register! '(view action-text 1) '((input text string (options text)) (input enabled boolean (options enabled))))))))
