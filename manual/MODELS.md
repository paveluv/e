# Canonical model state

`(state model)` stores portable non-text state in the base, alongside the
text buffer store. Base modules and `base-config.e` define kinds; the client
implementation provides shared head mirrors over the existing connection.

Use models for interaction state and derived values whose updates need no
undo journal. Authored text remains in `store:`. An authored-data domain
must provide its own undo/trash operations before exposing destructive
controls; the low-level model API is not a replacement for them.

## Kinds and ownership

Register a globally unique kind symbol, positive schema version and pure
payload predicate. The predicate can use the existing edoc types:

```scheme
(import (prefix (foundation edoc) edoc:))
(model:register-kind! 'selection 1
  (lambda (value) (edoc:type-accepts? '(list-of string) value)))
(define selection
  (model:create! '(base example) 'selection 1
    '(head "desk") 'persistent '() '("notes.txt")))
```

An extension imports `(state model)` as `model:` and `(foundation edoc)` as
`edoc:`. Definition registrations are module-owned and participate in kernel
registration transactions. Reloading or retracting a definition leaves its
model records intact. Each kind/version has one definition; register a new
version when the payload contract changes.

Model IDs are tagged lists such as `(model 7)`. A buffer numbered 7 is a
different resource, referenced as `(buffer 7)`. Scopes are `session`, a named
head `(head "desk")`, or an owning model ID, including a future view
descriptor. Scope declares composition ownership, not an access grant or
automatic deletion policy. References are lists of tagged model/buffer IDs.
Missing targets remain explicit; retiring a model never cascades into its
referenced resources.

Persistence is independent of scope and undo policy. `transient` records
survive head detach with the base; `persistent` records also survive a saved
restart. IDs are never recycled within the saved allocation sequence,
including IDs consumed by retired and transient records.

## Reads and changes

`model:ids` lists live IDs in allocation order. `model:snapshot` returns an
owned alist, or `#f` for an absent ID, with these fields in order:

`id`, `kind`, `schema`, `scope`, `persistence`, `revision`, `actor`,
`references`, `value`.

Readers own every mutable part. Input mutation, snapshot mutation and even
a predicate mutating its argument cannot alter stored state. Portable
values include lists, vectors, strings and bytevectors; cycles and runtime
objects such as procedures, ports and environments are rejected.

Commit a batch of `(model-id expected-revision references value)` changes:

```scheme
(model:commit! '(head "desk")
  (list (list selection 0 '() '("notes.txt" "draft.txt"))))
```

The result is two values: `applied`, `stale` or `unavailable`, and owned
current envelopes in request order. All changes install together or none
do. A stale batch returns the current basis, including `#f` for missing
records. Unknown or unaccepted schemas return `unavailable`. Invalid
arguments or replacement payloads raise an error without changing records.
Equal changes retain the revision and previous actor. Changed records
advance their revision once and record the supplied actor.

Validation runs outside the mutation lock. Publication checks both the
record basis and definition identity again. Predicates must be pure and
bounded; they receive disposable copies and must not depend on mutable
external state. Mutations participate in the base's existing activity
barrier, so restart waits for admitted work.

`(model:retire! actor id revision)` removes non-authored state. It returns
`applied` and `#f`, or `stale`/`unavailable` and the current envelope. It does
not delete files, buffers or other models.

## Subscriptions and head mirrors

In a head, subscribe before reading `snapshot` or `available?`:

```scheme
(define reader
  (model:subscribe! (list selection)
    (lambda (notice) (model:snapshot selection))))
(model:snapshot selection) ; owned local copy, no request
(model:unsubscribe! reader)
```

The callback receives `(generation ids)`, after adopting refreshed values on
the client pump thread. Use these APIs on that thread. Subscriptions belong
to their registering module. The first reader seeds the mirror; additional
readers reuse it, and removing the last reader releases it. Registrations
inside a staged module update acquire mirrors only when the update publishes.
Read them after publication, not inside the staged initializer.

The base subscribes before taking the initial snapshot. Monotone watermarks
prevent late replies from replacing newer mirrors. Invalidation batches are
bounded; overflow becomes a rescan. One background read batch and one pending
invalidation set serve the head. Rendering never waits for refresh. Definition
changes invalidate availability even when a record's revision stays unchanged.

At the base, `subscribe!` also accepts `#f` for all IDs; notices have the form
`(generation ids-or-#f)`, where `#f` requests a rescan. `snapshots` returns
`(generation ((id available? envelope-or-#f) ...))`. Callbacks run outside
the model writer and queued callbacks check that their owner remains live.

Client mutations use the authenticated connection's actor, regardless of
the supplied actor argument. Currently they require an all-buffer head
connection; model-specific agent grants are deferred.

## Recovery

Persistent models share the atomic session file with buffers and head
checkpoints. Format 2 adds model envelopes and their allocation counter;
format 1 sessions still load with an empty model store.

Restore validates portable envelopes and any already-known payload schema
before installing state. Unknown kinds and versions are retained intact.
`model:available?` reports whether the current definition accepts a model's
payload; later registration can make an opaque record usable without
changing its ID or revision. Rejected late adoption leaves it unavailable.
Saving again preserves unavailable records and never calls kind code while
the base is paused. Malformed envelopes follow the existing incompatible
session archive policy.

`model:export`, `model:valid-import?` and `model:import!` form the session
representation boundary. Import requires a fresh empty model store; these
are recovery operations, not interactive reset commands.
