(import (only (foundation edoc) elibrary))
(elibrary (service inspection)
  (export close! create! publish!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a bounded, transient inspection snapshot belonging to this attachment."
        (actor actor "connection attribution") (subject datum "initial explicit subject")
        (parts (list-of symbol) "independent section names") (returns list "inspection ID and named section references"))
  (define (create! actor subject parts) (client:request 'inspection-create subject parts))

  (edoc "Publish portable rows against the last inspection revision, only while demanded. Return applied, unchanged, stale, hidden or unavailable."
        (actor actor "connection attribution") (id model "inspection") (revision integer "expected revision")
        (subject datum "explicit inspected subject") (definitions integer "head definition generation")
        (parts list "changed (section-name . rows) entries") (truncated? boolean "explicit budget limit") (returns symbol))
  (define (publish! actor id revision subject definitions parts truncated?)
    (client:request 'inspection-publish id revision subject definitions parts truncated?))

  (edoc "Retire an owned inspection and its scoped presentations." (actor actor "connection attribution") (id model "inspection"))
  (define (close! actor id) (client:request 'inspection-close id)))
