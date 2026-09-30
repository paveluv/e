(import (only (foundation edoc) elibrary))
(elibrary (service conflict-source)
  (export choose! choose-all! settle!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Choose Mine or Disk for an exact displayed conflict row, fenced by query generation, basis and draft revision."
        (actor actor "connection attribution") (selection row-selection "shown conflict") (basis datum "result basis")
        (side (one-of mine disk flip) "choice, or flip the reviewed choice"))
  (define (choose! actor selection basis side) (client:request 'conflict-source-choose selection basis side))

  (edoc "Choose a side for all documents in the exact shown review, without settling."
        (actor actor "connection attribution") (query row-source "review rows") (generation integer "shown generation")
        (basis datum "result basis") (side (one-of mine disk) "choice"))
  (define (choose-all! actor query generation basis side) (client:request 'conflict-source-choose-all query generation basis side))

  (edoc "Settle the complete shown review and return per-document results; stale listings refuse."
        (actor actor "connection attribution") (query row-source "review rows") (generation integer "shown generation")
        (basis datum "result basis") (returns list))
  (define (settle! actor query generation basis) (client:request 'conflict-source-settle query generation basis)))
