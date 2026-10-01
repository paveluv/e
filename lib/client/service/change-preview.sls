(import (only (foundation edoc) elibrary))
(elibrary (service change-preview)
  (export create!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a demand-scoped revision/conflict annotation query over a borrowed document. Connect its selection input to false or (revision entry)/(conflict entry); the owning view declares the returned query in owned. No text is copied."
        (actor actor "connection attribution") (document buffer "borrowed document") (owner model "owning view") (returns model))
  (define (create! actor document owner) (client:request 'change-preview-create document owner)))
