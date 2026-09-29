(import (only (foundation edoc) elibrary))
(elibrary (service filesystem)
  (export complete! configure! create-query! create-source! refresh!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a base filesystem source sharing cached inventory, with explicit home and hidden-entry policy."
        (actor actor "connection supplies attribution") (home string "absolute home") (hidden? boolean "include dot entries")
        (persistence (one-of transient persistent) "restart policy") (returns row-source))
  (define (create-source! actor home hidden? persistence) (client:request 'filesystem-source home hidden? persistence))

  (edoc "Create a filesystem query owning its supplied persistent source and internal editable filter."
        (actor actor "connection supplies attribution") (source row-source "unshared persistent filesystem source")
        (text string "initial rooted filter") (returns list "(query filter-reference)"))
  (define (create-query! actor source text) (client:request 'filesystem-query source text))

  (edoc "Change a filesystem source's hidden-entry policy against its model revision."
        (actor actor "connection supplies attribution") (source row-source "filesystem source")
        (revision integer "expected model revision") (hidden? boolean "include dot entries"))
  (define (configure! actor source revision hidden?) (apply values (client:request 'filesystem-configure source revision hidden?)))

  (edoc "Invalidate the shared inventory and restart its filesystem queries."
        (actor actor "connection supplies attribution"))
  (define (refresh! actor) (client:request 'filesystem-refresh))

  (edoc "Queue completion of a complete readable generation at the base, returning an intent number or false. Match that intent and the collection basis in details.completion; apply text only to the unchanged requesting filter revision."
        (actor actor "connection supplies attribution") (query row-source "filesystem query")
        (generation integer "shown generation") (returns (or integer #f)))
  (define (complete! actor query generation) (client:request 'filesystem-complete query generation)))
