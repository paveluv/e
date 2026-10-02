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
    (rename (descriptor:owned owned)) (rename (descriptor:owner owner))
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
        (scope (list-of (or model #f)) "optional resource owner, false for session lifetime") (returns model))
  (define (create! actor source kind schema options state . scope)
    (apply client:request 'view-create source kind schema options state scope))

  (edoc "Retire a view against its revision, unlinking its parent and releasing borrowed children. Under a view lifetime, atomically retire its owned/scoped graph and keep resumable output cleanup on that owner; standalone retirement releases resources synchronously. Borrowed sources and command targets survive. Another head's mount refuses. Return status and current target envelope; pending preserves an unfinished cleanup intent."
        (actor actor "connection attribution") (id model "view") (revision integer "expected model revision"))
  (define (retire! actor id revision) (apply values (client:request 'view-retire id revision)))

  (edoc "Read a coherent canonical subtree." (id model "view") (returns list) (effects remote))
  (define (tree id) (client:request 'view-tree id))

  (edoc "Arrange parents under expected revisions and owned root leases."
        (actor actor "connection identity") (changes list "(id revision children options)") (leases list "(root generation)"))
  (define (arrange! actor changes leases) (apply values (client:request 'view-arrange changes leases)))

  (edoc "Fork a subtree and its owned resources, sharing borrowed sources. Options may supply an owner view and receivers, a list of (old new) external command receivers. Only command targets are rebound; sources and fixed arguments stay borrowed. The retainer atomically owns the unmounted copy. Internal scopes follow the copy; other descendants belong to its root. Restart policies are preserved."
        (actor actor "connection identity") (id model "view") (options (list-of list) "optional alist: owner model, receivers ((old new) ...)") (returns model))
  (define (fork! actor id . options) (apply client:request 'view-fork id options))

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
