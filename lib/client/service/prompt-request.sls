(import (only (foundation edoc) elibrary))
(elibrary (service prompt-request)
  (export accept! cancel! close! create!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a transient input request with captured origin and provider recipe. False draft creates owned disposable text; an explicit buffer borrows existing text. Read the resulting model for its draft and terminal outcome."
        (actor actor "connection attribution") (parent (or model #f) "owning request")
        (draft (or buffer #f) "borrowed draft") (text string "initial owned text")
        (origin datum "captured context") (provider datum "completion recipe") (returns (or model #f)))
  (define (create! actor parent draft text origin provider)
    (client:request 'prompt-create parent draft text origin provider))

  (edoc "Accept the exact reviewed request and draft once. Return applied, stale, closed or unavailable; the request model contains the immutable accepted text and origin."
        (actor actor "connection attribution") (id model "request") (revision integer "request revision")
        (draft-revision integer "reviewed text revision") (returns symbol))
  (define (accept! actor id revision draft-revision)
    (client:request 'prompt-accept id revision draft-revision))

  (edoc "Cancel an input request and its descendants without answering independent actor questions. Return whether this request changed."
        (actor actor "connection attribution") (id model "request") (returns boolean))
  (define (cancel! actor id) (client:request 'prompt-cancel id))

  (edoc "Close a request tree and release owned transient drafts; borrowed authored text survives."
        (actor actor "connection attribution") (id model "request"))
  (define (close! actor id) (client:request 'prompt-close id)))
