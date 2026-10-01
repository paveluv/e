;; Portable resource identities. Shape checks never resolve a resource.
(import (only (foundation edoc) elibrary))
(elibrary (core handle)
  (export buffer? model?)
  (import (rnrs))

  (define (tagged? value tag)
    (and (list? value) (= (length value) 2) (eq? (car value) tag)
      (integer? (cadr value)) (exact? (cadr value)) (> (cadr value) 0)))

  (edoc "Whether a value is a (model positive-integer) reference; no availability check."
    (value any "candidate") (returns boolean))
  (define (model? value) (tagged? value 'model))

  (edoc "Whether a value is a (buffer positive-integer) reference; no availability check."
    (value any "candidate") (returns boolean))
  (define (buffer? value) (tagged? value 'buffer)))
