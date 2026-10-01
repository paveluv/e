(import (only (foundation edoc) elibrary))
(elibrary (service history)
  (export append! create!)
  (import (except (chezscheme) append!) (prefix (core client) client:))
  (define (request actor operation . args)
    (unless (equal? actor (client:identity)) (error 'history "actor differs from connection identity"))
    (apply client:request operation args))

  (edoc "Create persistent or transient ordered history. Items borrow existing sources and jobs; no text or native values are copied into this model."
    (actor actor "creator") (persistence (one-of persistent transient) "recovery policy") (returns model) (public))
  (define (create! actor persistence) (request actor 'history-create persistence))

  (edoc "Append an explicit (kind schema data) presentation at the expected history revision. Projection is a borrowed buffer reference for copy/export, or false. References name borrowed sources and jobs. Return an item ID or false for stale history."
    (actor actor "caller") (id model "history") (revision integer "expected revision")
    (recipe list "portable presentation recipe") (projection datum "buffer reference or false")
    (references list "borrowed model/buffer references") (returns (or model #f)) (receiver id (model history)) (public))
  (define (append! actor id revision recipe projection references)
    (request actor 'history-append id revision recipe projection references)))
