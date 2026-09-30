(import (only (foundation edoc) elibrary))
(elibrary (service review-preview)
  (export close! create!)
  (import (chezscheme) (prefix (core client) client:) (prefix (core port) port:)
          (prefix (core review-contract) review-contract:))
  (define ports (port:register! '(model review-preview 1) review-contract:ports))

  (edoc "Create a demand-owned preview request and read-only document over a borrowed review draft; return (request document). Connect a table's selection output to the request's selection input."
        (actor actor "connection attribution") (id model "conflict or rewrite draft") (returns list))
  (define (create! actor id) (client:request 'review-preview-create id))

  (edoc "Retire a preview request, scoped views and its owned output; preserve the borrowed draft and source."
        (actor actor "connection attribution") (id model "preview request"))
  (define (close! actor id) (client:request 'review-preview-close id)))
