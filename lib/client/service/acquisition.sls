(import (only (foundation edoc) elibrary))
(elibrary (service acquisition)
  (export acquire!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Acquire a file or directory in the base without choosing its destination. Returns (directory path) or (buffer id admitted? path diagnostic); disk contents stay in the base."
        (actor actor "connection supplies attribution") (path string "absolute path")
        (proposal (list-of any) "optional creation witness") (returns list))
  (define (acquire! actor path . proposal)
    (unless (equal? actor (client:identity)) (error 'acquire! "actor differs from connection identity"))
    (apply client:request 'acquire path proposal)))
