;; Canonical view operations over the existing base connection. Head-local
;; provisional state and publication are provided by head/interaction.
(import (only (foundation edoc) elibrary))
(elibrary (state view)
  (export claim! create! publish! release! set-state! snapshot)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a persistent view over a model using this connection's identity."
        (actor actor "connection attribution") (model list "model id") (renderer symbol "widget kind")
        (schema integer "renderer schema") (state datum "initial interaction") (returns list))
  (define (create! actor model renderer schema state)
    (client:request 'view-create model renderer schema state))

  (edoc "Read a canonical view descriptor from the base. Rendering uses interaction:snapshot instead."
        (id list "view id") (returns (or list #f)) (effects remote))
  (define (snapshot id) (client:request 'view-read id))

  (edoc "Claim an unowned view; return status and descriptor."
        (actor actor "connection attribution") (id list "view id"))
  (define (claim! actor id) (apply values (client:request 'view-claim id)))

  (edoc "Commit a batch of owner generations, sequences and interaction states at the base."
        (actor actor "connection attribution") (updates list "(id generation sequence basis state) entries"))
  (define (publish! actor updates) (apply values (client:request 'view-publish updates)))

  (edoc "Change saved state of an unowned view; active views must be operated through their owner."
        (actor actor "connection attribution") (id list "view id") (basis (or integer #f) "model revision") (state datum "interaction"))
  (define (set-state! actor id basis state) (apply values (client:request 'view-set id basis state)))

  (edoc "Release the matching owner generation."
        (actor actor "connection attribution") (id list "view id") (generation integer "ownership generation"))
  (define (release! actor id generation) (apply values (client:request 'view-release id generation)))
)
