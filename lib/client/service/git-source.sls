(import (only (foundation edoc) elibrary))
(elibrary (service git-source)
  (export create! create-patch! expand! refresh! select-patch!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a lazy Git history query for a path." (actor actor "connection attribution") (path file "repository path") (returns row-source))
  (define (create! actor path) (client:request 'git-source path))

  (edoc "Expand or collapse an exact displayed commit." (actor actor "connection attribution") (selection row-selection "shown row") (basis datum "shown result"))
  (define (expand! actor selection basis) (client:request 'git-expand selection basis))

  (edoc "Create an independent patch query and document; return (query document)." (actor actor "connection attribution")
    (owner (list-of model) "optional owning view") (returns list))
  (define (create-patch! actor . owner) (apply client:request 'git-patch owner))

  (edoc "Select a displayed file into an explicit patch query." (actor actor "connection attribution") (query row-source "patch query") (selection row-selection "shown row") (basis datum "shown result"))
  (define (select-patch! actor query selection basis) (client:request 'git-select-patch query selection basis))

  (edoc "Refresh a Git query, superseding older work." (actor actor "connection attribution") (query row-source "Git query"))
  (define (refresh! actor query) (client:request 'git-refresh query)))
