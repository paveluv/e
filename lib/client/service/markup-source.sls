(import (only (foundation edoc) elibrary))
(elibrary (service markup-source)
  (export create!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a base collection of portable Markdown blocks over a borrowed source document."
        (actor actor "connection supplies attribution") (document buffer "source document") (returns row-source))
  (define (create! actor document) (client:request 'markup-source document)))
