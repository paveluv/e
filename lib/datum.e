;; datum.e -- owned snapshots of finite, plain protocol data.
(library (datum)
  (export copy)
  (import (rnrs))

  (define copy
    (case-lambda
      [(value) (copy value (lambda (leaf) (error 'datum:copy "expected plain protocol data" leaf)))]
      [(value copy-leaf)
       ;; Sharing is allowed, cycles and runtime objects are not. Readers own
       ;; every mutable part; no retained data changes without a seam operation.
       ;; An explicit leaf copier can preserve a domain's opaque values, e.g.
       ;; immutable text deltas or identity-bearing in-process grouping tokens.
       (let walk ([value value] [path '()])
         (when (memq value path) (error 'datum:copy "cyclic protocol data"))
         (cond
           [(pair? value)
            (let ([path (cons value path)])
              (cons (walk (car value) path) (walk (cdr value) path)))]
           [(vector? value)
            (list->vector (map (lambda (item) (walk item (cons value path))) (vector->list value)))]
           [(string? value) (string-copy value)]
           [(bytevector? value) (bytevector-copy value)]
           [(or (null? value) (symbol? value) (number? value) (boolean? value) (char? value)) value]
           [else (copy-leaf value)]))])))
