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

At M-x, write a model reference as `'(model 7)`. Tab at a documented `model`
argument offers live models with their kind as a hint. Retired models leave
the choices, but their references can still be written for inspection.
Operations check existence, kind and ownership when used. Views are models too.

The portable value is the tagged list `(model 7)`; the apostrophe is ordinary
Scheme source syntax. Inside a quoted collection, no additional quote is
needed: `'((model 7) (model 8))`. Bindings, completion and copied evaluation
results use the same spelling regardless of argument metadata.
`(model:metadata)` returns compact `(reference kind)` rows for all live
models in allocation order, including models whose kind is unavailable.
It neither copies payloads nor runs kind validators. Each completion lookup
makes one metadata request to the base; printing a reference or repainting
the completion list makes none.

A buffer numbered 7 is a different resource, referenced as `'(buffer 7)`.
The text-store API accepts that same value: `(store:buffer-name '(buffer 7))`.
Use `(store:find-named "notes.txt")` to resolve a name explicitly. Bare
numeric IDs and model references are not accepted as buffer references.
Editing, scoped search and placement take the same tagged buffer references.
Head presentation records are internal adapters and are not buffer values.

Scopes are `session`, a named head `(head "desk")`, or an owning model ID,
including a view descriptor. Scope declares composition ownership, not an access grant or
automatic deletion policy. References are lists of tagged model/buffer IDs.
Missing targets remain explicit; retiring a model never cascades into its
referenced resources.

Persistence is independent of scope and undo policy. `transient` records
survive head detach with the base; `persistent` records also survive a saved
restart. IDs are never recycled within the saved allocation sequence,
including IDs consumed by retired and transient records.

## Reads and changes

`model:ids` lists live IDs in allocation order. An optional kind, for example
`(model:ids 'widget-view)`, filters without reading payloads. `model:snapshot` returns an
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

## Views and provisional interaction

`view:create!` takes actor, source, widget kind, schema, options and initial
interaction. The source is a tagged model/buffer ID, or `#f` for a container.
Its persistent descriptor has named source, kind/schema, parent, children,
options, generation, owner, sequence, basis, state and focus fields. Use the
corresponding `view:` accessors on a descriptor; they never perform a lookup.
Geometry is never saved. Basis identifies the source revision of an anchor.

`view:arrange!` takes an actor, `(parent-id expected-revision children options)`
changes and `(root-id generation)` leases for owned roots. Children are
`(slot-symbol child-id sizing)` entries; sizing is `fit` or `(grow weight)`.
Reparenting includes both parents. Backlinks, owner transfers and generations
change atomically; cycles, duplicate children and foreign owners are refused.
Removal retains the child subtree and its sources. An active owner fences
publication and uses `interaction:arrange!` to adopt the returned descriptors.

`view:tree` returns a coherent list of `(id . descriptor)` entries.
`view:fork!` copies that subtree's descriptors, remapping children and focus,
while sharing sources and resetting ownership. Unavailable schemas refuse
forking before allocation. These are explicit base requests in a head.

`view:claim!` takes an actor and root ID and returns status plus descriptor
entries for the whole supported tree. A nested child cannot be claimed alone.
An already owned tree returns `owned`, even to the same head. A successful
claim increments generations and resets sequences to zero.
`view:publish!` takes an actor and a batch of
`(view-id generation sequence basis state focus)` snapshots. The newest
snapshots with matching owners and generations apply atomically. Retired
views, former owners and acknowledged sequences are ignored, allowing live
views to keep publishing when a temporary sibling disappears. Focus outside
the surviving root clears. This is interaction publication; authored text
continues to use the store's guarded edit commands.
`view:release!` checks the root generation and releases its supported tree,
retaining acknowledged state. An owned arrangement renews generations to
prevent an old publication from overwriting corrected focus or removed state.
Disconnect releases the head's views; restart clears recovered mount owners.

`view:set-state!` changes saved state only while unmounted. Commands for an
active view must be sent to its owning head. Low-level `model:` mutations are
trusted infrastructure; use `view:` for view state so these contracts apply.

The head's `interaction:` module claims views and holds their provisional
descriptors. `interaction:set-state!` updates them immediately without a
request. `interaction:snapshot` reads that local state. Activation must pass
the actual selected target and its model basis to the domain operation; it
must not reread a potentially older base selection.

Call `interaction:publish!` after presentation or dispatch. The shared
publisher sends only changed views, keeping one batch in flight and one
latest pending replacement. Replies never replace provisional state.
`interaction:flush!` fences delivery, and `interaction:release!` fences before
releasing an owner. Mount adapters must fence before detach. The head's
interaction owner and its one worker live for the process, not per mount.

Canonical `view:snapshot` in a head is an explicit remote inspection operation;
it is unsuitable for rendering. `interaction:snapshot` is the rendering read.
Both expose owned copies.

The named descriptor uses schema 2. Recovery converts known schema-1 leaf
descriptors once, while keeping unknown schemas opaque and inspectable.

## Portable ports

`port:register!` declares `(model kind schema)`, `(view kind schema)` or
`(buffer kind schema)`
contracts as `(input|output name type selector)` entries. For example:

```scheme
(port:register! '(model filenames 1)
  '((output files (list-of file) (value files))))
```

Selectors read `value`, view `state`/`options`, `id`, or a view's `source`.
Trailing symbols select alist fields and integers select sequence elements.
The built-in entry and `(buffer text 1)` contracts expose `text` through
`(source-text)` and become unavailable for a multiline source. Bind directly
to `((buffer id) text)` when a consumer should outlive a particular entry.
Dependency bundles carry buffer contract headers and text IDs; mounted hosts
provide their existing text mirrors. `port:project` returns `(ready value)` or
`(unavailable reason)`; `#f` and an empty list can be ordinary ready values.
Declarations and returned values are owned copies. Definitions follow module
registration/retraction, while saved data stays intact.

Types must be concrete portable contracts. An `edoc-type` opts in with
`(portable #t)`, promising a pure, bounded predicate; `within` declares its
refinement. `edoc:type-compatible?` conservatively checks equality,
refinements, finite literals, unions and covariant lists. Unknown types,
placeholders and runtime records cannot authorize connections. Actual values
also undergo portable-data validation. Filename producers use absolute paths;
the filename type does not assert that a path exists.

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
