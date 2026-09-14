;; datum.sls -- owned snapshots of finite, plain protocol data.
(library (datum)
  (export copy invalid?)
  (import (rnrs))

  (define-condition-type &invalid &error make-invalid invalid?)
  (define (invalid! message value)
    (raise (condition (make-invalid) (make-who-condition 'datum:copy)
             (make-message-condition message) (make-irritants-condition (list value)))))

  (define copy
    (case-lambda
      [(value) (copy value (lambda (leaf) (invalid! "expected plain protocol data" leaf)))]
      [(value copy-leaf)
       ;; Sharing is allowed, cycles and runtime objects are not. Readers own
       ;; every mutable part; no retained data changes without a seam operation.
       ;; An explicit leaf copier can preserve a domain's opaque values, e.g.
       ;; immutable text deltas or identity-bearing in-process grouping tokens.
       ;; The pairs and vectors on the current descent are the cycle check;
       ;; a list spine is walked once, so long lists cost one step per pair.
       (let ([active (make-eq-hashtable)])
         (define (enter! node)
           (when (hashtable-contains? active node) (invalid! "cyclic protocol data" node))
           (hashtable-set! active node #t))
         (define (leave! node) (hashtable-delete! active node))
         (let walk ([value value])
           (cond
             [(pair? value)
              ;; Every pair of the spine stays active while any car is copied,
              ;; so a car leading back into the spine is a cycle; a car
              ;; pointing further down the spine is ordinary sharing.
              (let spine ([rest value] [pairs '()])
                (if (pair? rest)
                    (begin (enter! rest) (spine (cdr rest) (cons rest pairs)))
                    (let build ([pairs pairs] [out (walk rest)])
                      (if (null? pairs) out
                          (let ([copied (cons (walk (car (car pairs))) out)])
                            (leave! (car pairs))
                            (build (cdr pairs) copied))))))]
             [(vector? value)
              (enter! value)
              (let ([copied (vector-map walk value)])
                (leave! value)
                copied)]
             [(string? value) (string-copy value)]
             [(bytevector? value) (bytevector-copy value)]
             [(or (null? value) (symbol? value) (number? value) (boolean? value) (char? value)) value]
             [else (copy-leaf value)])))])))
