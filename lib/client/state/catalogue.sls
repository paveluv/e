(import (only (foundation edoc) elibrary))
(elibrary (state catalogue)
  (export create-query! create-source! neighbor)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a base-owned buffer catalogue source for this head."
        (actor actor "connection supplies attribution") (home string "absolute home for filtering")
        (persistence (one-of transient persistent) "restart policy") (returns row-source))
  (define (create-source! actor home persistence) (client:request 'catalogue-source home persistence))

  (edoc "Create a catalogue query owning its supplied source and internal editable filter."
        (actor actor "connection supplies attribution") (source row-source "unshared persistent catalogue source") (returns list "(query filter-reference)"))
  (define (create-query! actor source) (client:request 'catalogue-query source))

  (edoc "Return the adjacent live key in the query's cached unfiltered order, wrapping at the ends. False uses default name order without allocating a query."
        (actor actor "connection supplies attribution") (query (or row-source #f) "catalogue query or default order") (key datum "current key")
        (direction (one-of next previous) "ring direction") (returns any))
  (define (neighbor actor query key direction) (client:request 'catalogue-neighbor query key direction)))
