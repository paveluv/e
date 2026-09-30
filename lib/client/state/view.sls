;; Canonical view operations over the existing base connection. Head-local
;; provisional state and publication are provided by head/interaction.
(import (only (foundation edoc) elibrary))
(elibrary (state view)
  (export arrange! (rename (descriptor:basis basis))
    (rename (descriptor:children children)) claim! create!
    (rename (descriptor:focus focus)) fork!
    (rename (descriptor:generation generation))
    (rename (descriptor:kind kind))
    (rename (descriptor:options options))
    (rename (descriptor:owner owner))
    (rename (descriptor:parent parent)) publish! release! retire!
    (rename (descriptor:schema schema))
    (rename (descriptor:sequence sequence)) set-state! snapshot
    (rename (descriptor:source source))
    (rename (descriptor:state state)) tree)
  (import (chezscheme) (prefix (core client) client:) (prefix (core descriptor) descriptor:)
          (prefix (state model) model:))

  (edoc "Create a view using this connection's identity. An optional resource owner supplies its scope and persistence; otherwise it is session-persistent. Source is a model/buffer reference or false for a container."
        (actor actor "connection attribution") (source datum "source reference") (kind symbol "widget kind")
        (schema integer "contract version") (options list "logical options") (state datum "initial interaction")
        (scope (list-of model) "optional resource owner") (returns model))
  (define (create! actor source kind schema options state . scope)
    (apply client:request 'view-create source kind schema options state scope))

  (edoc "Retire an unmounted view against its revision, atomically detaching it and releasing its child subtrees. Sources and command targets survive. Return status and current target envelope."
        (actor actor "connection attribution") (id model "view") (revision integer "expected model revision"))
  (define (retire! actor id revision) (apply values (client:request 'view-retire id revision)))

  (edoc "Read a coherent canonical subtree." (id model "view") (returns list) (effects remote))
  (define (tree id) (client:request 'view-tree id))

  (edoc "Arrange parents under expected revisions and owned root leases."
        (actor actor "connection identity") (changes list "(id revision children options)") (leases list "(root generation)"))
  (define (arrange! actor changes leases) (apply values (client:request 'view-arrange changes leases)))

  (edoc "Fork view descriptors while sharing domain sources." (actor actor "connection identity") (id model "view") (returns model))
  (define (fork! actor id) (client:request 'view-fork id))

  (edoc "Read a canonical view descriptor from the base. Rendering uses interaction:snapshot instead."
        (id model "view id") (returns (or list #f)) (effects remote))
  (define (snapshot id)
    (unless (model:reference? id) (error 'snapshot "expected a model reference" id))
    (client:request 'view-read id))

  (edoc "Claim an unowned tree; return status and (id . descriptor) entries."
        (actor actor "connection attribution") (id model "view id"))
  (define (claim! actor id) (apply values (client:request 'view-claim id)))

  (edoc "Publish the newest still-owned interaction snapshots atomically. Retired views, former ownership generations and acknowledged sequences are ignored; focus outside a surviving root clears."
        (actor actor "connection attribution") (updates list "(id generation sequence basis state focus) entries"))
  (define (publish! actor updates) (apply values (client:request 'view-publish updates)))

  (edoc "Change saved state of an unowned view; active views must be operated through their owner."
        (actor actor "connection attribution") (id model "view id") (basis (or integer #f) "model revision") (state datum "interaction"))
  (define (set-state! actor id basis state) (apply values (client:request 'view-set id basis state)))

  (edoc "Release the matching owner generation."
        (actor actor "connection attribution") (id model "view id") (generation integer "ownership generation"))
  (define (release! actor id generation) (apply values (client:request 'view-release id generation)))
)
