(import (only (foundation edoc) elibrary))
(elibrary (core review-contract)
  (export ports)
  (import (chezscheme) (prefix (core row) row:))

  (edoc "Portable selection input and revisioned annotation output shared by preview requests in base and head runtimes.")
  (define ports
    '((input selection (or row-selection #f) (value selection))
      (output annotations list (value annotations)))))
