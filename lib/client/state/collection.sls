;; Indexed collection operations; head/range owns local viewport demand.
(import (only (foundation edoc) elibrary))
(elibrary (state collection)
  (export configure! create! create-source! fetch range rank summary)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a canonical vector-backed row source at the base."
        (actor actor "connection supplies attribution") (columns list "raw column contracts")
        (rows vector "portable rows") (persistence (one-of transient persistent) "restart policy") (returns list))
  (define (create-source! actor columns rows persistence)
    (client:request 'collection-source columns rows persistence))

  (edoc "Create a shared filter/sort query over an indexed source."
        (actor actor "connection supplies attribution") (source row-source "row provider") (filter string "substring")
        (sort list "compound keys") (persistence (one-of transient persistent) "restart policy") (returns list))
  (define (create! actor source filter sort persistence)
    (client:request 'collection-create source filter sort persistence))

  (edoc "Change a guarded query recipe at the base."
        (actor actor "connection supplies attribution") (id row-source "query") (revision integer "model revision")
        (changes list "filter/sort fields"))
  (define (configure! actor id revision changes)
    (apply values (client:request 'collection-configure id revision changes)))

  (edoc "Read authoritative compact query metadata. Rendering uses range:summary's acquired mirror."
        (id row-source "query") (returns any) (effects remote))
  (define (summary id) (client:request 'collection-summary id))

  (edoc "Read a bounded prepared range from the base at an explicit generation. Rendering uses range:read's cache."
        (id row-source "query") (generation integer "result generation") (start integer "first ordinal")
        (count integer "row limit") (columns list "requested column IDs") (returns list) (effects remote))
  (define (range id generation start count columns) (client:request 'collection-range id generation start count columns))

  (edoc "Locate a stable key in an explicit result generation at the base."
        (id row-source "query") (generation integer "result generation") (key datum "stable row key") (returns list) (effects remote))
  (define (rank id generation key) (client:request 'collection-rank id generation key))

  (edoc "Read a bounded batch of prepared range/rank requests."
        (requests list "at most four requests") (returns list) (effects remote))
  (define (fetch requests) (client:request 'collection-fetch requests)))
