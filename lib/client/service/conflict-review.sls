(import (only (foundation edoc) elibrary))
(elibrary (service conflict-review)
  (export choose! close! create! preview refresh! settle!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create an independent persistent conflict review over borrowed documents."
        (actor actor "connection attribution") (scope (list-of buffer) "document IDs") (returns model))
  (define (create! actor scope) (client:request 'conflict-review-create scope))

  (edoc "Refresh scope and exact alternatives, rebasing proven choices; return the updated envelope."
        (actor actor "connection attribution") (id model "review") (revision integer "expected draft revision")
        (scope (list-of buffer) "document IDs") (returns list))
  (define (refresh! actor id revision scope) (client:request 'conflict-review-refresh id revision scope))

  (edoc "Choose a side for groups of (document alternative ...), validating all before changing the draft. Return its envelope."
        (actor actor "connection attribution") (id model "review") (revision integer "expected draft revision")
        (groups list "exact displayed alternatives") (side (one-of mine disk) "choice") (returns list))
  (define (choose! actor id revision groups side) (client:request 'conflict-review-choose id revision groups side))

  (edoc "Derive (draft-revision document source-revision text regions) without editing source text."
        (id model "review") (document buffer "scoped source") (returns list))
  (define (preview id document) (client:request 'conflict-review-preview id document))

  (edoc "Settle explicit scoped documents against their complete displayed alternatives. Return per-document status and detail."
        (actor actor "connection attribution") (id model "review") (revision integer "expected draft revision")
        (documents (list-of buffer) "scoped document IDs") (returns list))
  (define (settle! actor id revision documents) (client:request 'conflict-review-settle id revision documents))

  (edoc "Retire the review and its scoped views, preserving borrowed sources."
        (actor actor "connection attribution") (id model "review") (revision integer "expected draft revision"))
  (define (close! actor id revision) (client:request 'conflict-review-close id revision)))
