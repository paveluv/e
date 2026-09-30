(import (only (foundation edoc) elibrary))
(elibrary (service rewrite)
  (export close! create! preview settle! toggle!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create an independent rewrite draft over a borrowed document." (actor actor "connection attribution") (document integer "source") (returns model))
  (define (create! actor document) (client:request 'rewrite-create document))

  (edoc "Toggle entries against a draft revision; return its updated envelope." (actor actor "connection attribution") (id model "draft")
        (revision integer "expected draft revision") (revisions (list-of integer) "entries") (returns list))
  (define (toggle! actor id revision revisions) (client:request 'rewrite-toggle id revision revisions))

  (edoc "Read (draft-revision document disabled text mapping conflicts source-revision)." (id model "rewrite draft") (returns list))
  (define (preview id) (client:request 'rewrite-preview-draft id))

  (edoc "Settle an explicit rewrite draft; return status and detail." (actor actor "connection attribution") (id model "draft") (revision integer "expected draft revision"))
  (define (settle! actor id revision) (apply values (client:request 'rewrite-settle id revision)))

  (edoc "Retire the draft and its scoped views, preserving the source." (actor actor "connection attribution") (id model "draft") (revision integer "expected draft revision"))
  (define (close! actor id revision) (client:request 'rewrite-close id revision)))
