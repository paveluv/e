(import (only (foundation edoc) elibrary))
(elibrary (service search-request)
  (export close! configure! create!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a base search request with an explicit editor/document, revision, interaction sequence, needle, case policy, origin and visible logical span."
        (actor actor "connection supplies attribution") (request list "ordered target/document/basis/sequence/start/needle/fold?/visible alist") (returns model))
  (define (create! actor request) (client:request 'search-create request))

  (edoc "Supersede a base search request against its request generation, retaining its cancellation origin. Return the new generation or false."
        (actor actor "connection supplies attribution") (id model "search request") (generation integer "expected generation")
        (request list "same fields as create!") (returns (or integer #f)))
  (define (configure! actor id generation request) (client:request 'search-configure id generation request))

  (edoc "Close a search request; its borrowed editor and document survive."
        (actor actor "connection supplies attribution") (id model "search request"))
  (define (close! actor id) (client:request 'search-close id)))
