(import (only (foundation edoc) elibrary))
(elibrary (service journal-source)
  (export create!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a bounded journal collection, optionally filtered by component."
        (actor actor "connection supplies attribution") (component (or symbol #f) "component or all") (returns row-source))
  (define (create! actor component) (client:request 'journal-source component)))
