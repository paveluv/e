(import (only (foundation edoc) elibrary))
(elibrary (service rewrite-source)
  (export settle! toggle!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Toggle the exact displayed history entry, fenced by source, query and draft revisions."
        (actor actor "connection attribution") (selection row-selection "shown entry") (basis datum "result basis"))
  (define (toggle! actor selection basis) (client:request 'rewrite-source-toggle selection basis))

  (edoc "Settle the exact displayed rewrite draft and return (status detail)."
        (actor actor "connection attribution") (query row-source "history query") (generation integer "shown generation")
        (basis datum "result basis") (returns list))
  (define (settle! actor query generation basis) (client:request 'rewrite-source-settle query generation basis)))
