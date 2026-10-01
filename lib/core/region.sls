;; A region is portable data, independent of a head or a live document.
(import (only (foundation edoc) elibrary))
(elibrary (core region)
  (export buffer end make start valid?)
  (import (rnrs)
          (prefix (core handle) handle:)
          (prefix (foundation datum) datum:)
          (prefix (foundation text) text:))

  (edoc-type region "a (region (buffer id) start end) datum with ordered character positions; operations check availability and bounds"
    (predicate valid?) (portable #t) (within list))

  (edoc "Whether a value is a region with a buffer reference and ordered nonnegative positions; no document lookup."
    (value any "candidate") (returns boolean))
  (define (valid? value)
    (and (list? value) (= (length value) 4) (eq? (car value) 'region)
      (handle:buffer? (cadr value)) (text:position? (caddr value)) (text:position? (cadddr value))
      (text:position<=? (caddr value) (cadddr value))))

  (edoc "Construct an owned region datum, validating the reference and positions and putting its endpoints in order. This performs no document lookup; coordinates do not rebase after edits."
    (buffer buffer "document reference") (start position "one endpoint") (end position "the other endpoint")
    (returns region) (public))
  (define (make buffer start end)
    (unless (and (handle:buffer? buffer) (text:position? start) (text:position? end))
      (error 'region:make "expected a buffer reference and nonnegative character positions" buffer start end))
    (datum:copy
      (if (text:position<? end start)
        (list 'region buffer end start)
        (list 'region buffer start end))))

  (define (check value)
    (unless (valid? value) (error 'region "expected an ordered region datum" value)))

  (edoc "A region's buffer reference, without resolving the document."
    (r region "region datum") (returns buffer))
  (define (buffer r) (check r) (cadr r))

  (edoc "A region's inclusive start position."
    (r region "region datum") (returns position))
  (define (start r) (check r) (caddr r))

  (edoc "A region's exclusive end position."
    (r region "region datum") (returns position))
  (define (end r) (check r) (cadddr r)))
