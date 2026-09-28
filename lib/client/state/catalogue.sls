(import (only (foundation edoc) elibrary))
(elibrary (state catalogue)
  (export attach! contribute! create-query! create-source! neighbor)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Open this head connection's temporary local-buffer contribution; detach releases it."
        (actor actor "connection supplies attribution") (returns integer "attachment token"))
  (define (attach! actor) (client:request 'catalogue-attach))

  (edoc "Publish a bounded batch of local metadata changes; false refuses an obsolete attachment."
        (actor actor "connection supplies attribution") (token integer "attachment token") (changes list "at most 256 upserts/removals within 64 KiB") (returns boolean))
  (define (contribute! actor token changes) (client:request 'catalogue-contribute token changes))

  (edoc "Create a base-owned buffer catalogue source for this head."
        (actor actor "connection supplies attribution") (home string "absolute home for filtering")
        (persistence (one-of transient persistent) "restart policy") (returns row-source))
  (define (create-source! actor home persistence) (client:request 'catalogue-source home persistence))

  (edoc "Create a catalogue query owning its supplied source and internal editable filter."
        (actor actor "connection supplies attribution") (source row-source "unshared persistent catalogue source") (returns list "(query filter-reference)"))
  (define (create-query! actor source) (client:request 'catalogue-query source))

  (edoc "Return the adjacent live key in the query's cached unfiltered order, wrapping at the ends."
        (actor actor "connection supplies attribution") (query row-source "catalogue query") (key datum "current key")
        (direction (one-of next previous) "ring direction") (returns any))
  (define (neighbor actor query key direction) (client:request 'catalogue-neighbor query key direction)))
