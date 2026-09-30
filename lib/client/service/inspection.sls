(import (only (foundation edoc) elibrary))
(elibrary (service inspection)
  (export close! create! publish!)
  (import (chezscheme) (prefix (core client) client:))

  (edoc "Create a bounded, transient inspection snapshot belonging to this attachment."
        (actor actor "connection attribution") (returns model))
  (define (create! actor) (client:request 'inspection-create))

  (edoc "Publish portable rows against the last inspection revision, only while demanded. Return applied, unchanged, stale, hidden or unavailable."
        (actor actor "connection attribution") (id model "inspection") (revision integer "expected revision")
        (subject datum "explicit inspected subject") (definitions integer "head definition generation")
        (rows list "(key heading-or-false keys command description italic-spans)") (truncated? boolean "explicit budget limit") (returns symbol))
  (define (publish! actor id revision subject definitions rows truncated?)
    (client:request 'inspection-publish id revision subject definitions rows truncated?))

  (edoc "Retire an owned inspection and its scoped presentations." (actor actor "connection attribution") (id model "inspection"))
  (define (close! actor id) (client:request 'inspection-close id)))
