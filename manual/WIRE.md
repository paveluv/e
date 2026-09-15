# Local wire protocol

The base exposes a Unix socket at `socket` inside its working directory
(`.base` inside the installation by default). Connections require the same
OS user. This protocol supports attached heads and scripted agents; it does
not provide remote transport or multi-user authentication.

See [Multiple heads and agents](MULTIHEAD.md) for session permissions,
editing semantics, named screens and recovery. The request dispatcher is
in [base.sls](../lib/base/run/base.sls), and framing is in
[wire.sls](../lib/foundation/wire.sls).

## Framing and admission

Each frame has a four-byte unsigned big-endian byte count followed by one
UTF-8 Scheme datum. The count must be 1 through 16,777,216. Finite pairs,
vectors, strings, symbols, numbers, booleans, characters and bytevectors
are accepted; cycles and runtime objects are refused. EOF between frames
disconnects; partial or malformed frames close the connection.

The current normal protocol version is **5**. Send
`(hello 5 (head "name") fingerprint)` or
`(hello 5 (agent "name") fingerprint)`, using the installation's
`kernel:fingerprint` string. The version and source fingerprint must match
the running base. Older normal protocols are not negotiated. The fingerprint
identifies compatible sources and grants no permissions.

A successful hello returns `(hello 5 actor capabilities)`. The base chooses
the policy: heads default to all-buffer writes, agents to read-only sessions.
Capabilities are `(read)` or `(read edit undo redo)`; the hello cannot grant
itself access. Admission failures return `(error #f reason)` and close.
Reasons include `name-in-use`, `(stale-base status)`, `(busy phase)` and diagnostic text.

Use `sys:connect-local`, `sys:connection-input`/`sys:connection-output`,
`wire:send!`/`wire:receive` and `sys:close-connection!` for Scheme clients.
Serialize complete frames through one writer per direction. Replies and
events can interleave, so one reader must dispatch both.

## Requests and notifications

Square brackets below denote optional arguments; they are not literal
wire syntax. Request IDs are nonnegative exact integers. Replies use
`(reply id ok result)` or `(reply id error text)`. A valid request that fails
does not by itself close the connection. There is no implicit retry or replay
protection: inspect current state after a lost connection before retrying an
edit with an unknown outcome.

| Message | Meaning |
| --- | --- |
| `(request id buffers)` | Return the shared store ids. |
| `(request id name buffer-id)` | Return the store label. |
| `(request id snapshot buffer-id [basis])` | Return `(text-vector revision facts)`; with a nonnegative exact basis, append the matching change chain as a fourth field. One `store:snapshot-state` read supplies all fields. |
| `(request id watch)` | Subscribe to store invalidations before returning the current shared ids. Repeated calls reuse this connection's watch. |
| `(request id watch-head)` | A head also subscribes to surface, presence and log notices. Idempotent; all registrations belong to this connection. |
| `(request id state buffer-id basis [delta?])` | Return `#f` if absent, otherwise `(name text revision facts [changes])`; `basis` is `#f` or a revision, with changes returned only for a revision. Existence, name, text, revision, facts and changes come from one `store:state` read, including across rename/deletion. Name/liveness changes also invalidate the cache. `delta?` is `#f`, `#t` or `facts`. `#t` says the client holds text at the basis: the text slot is then `#f` whenever the complete chain since the basis is included, and the client advances its own copy through that chain. `facts` says it also holds the stored facts: such a delta reply carries only the owner-maintained `modified` and `modified-at` facts, since stored facts change through the events the client already receives. |
| `(request id find-file canonical-path)` | Return the id whose current `file` fact matches, or `#f`. File commands query shared identity before disk I/O or creation; cached head lists do not decide shared file identity. |
| `(request id actors)` | Return the existing actor directory records. |
| `(request id eval expression)` | Evaluate source text or a datum in this session's granted read-only environment. Return `(status . text)`; see evaluation outcomes below. |
| `(request id sessions)` | An all-buffer head reads the existing `((actor owner) ...)` session inventory; no handles cross the connection. |
| `(request id revoke actor)` | An all-buffer head revokes the selected agent's live sessions. Return their selection count, normally 1 or 0. Head targets and agent/read-only-head callers are refused. |
| `(request id edit buffer-id basis span lines [context [delta?]])` | Return `(applied receipt)`, `(stale reason)` or `(refused reason)` under the connection's session. Context is `#f` or `(key label [undo-facts [commit-facts [expected]]])`. Expected facts are exact-value pairs or absent symbol keys; mismatch returns `(stale property-changed)`. The receipt is `(revision text changes edit-facts)` with the chain from the basis through the accepted edit and the `modified`/`modified-at` fact pairs captured at that commit. A true `delta?` replaces only the text with `#f`, for a client holding text and facts at the basis. |
| `(request id undo buffer-id [scope])` | Scope defaults to `mine`; `all` and `(actor who)` also work. Return `(applied revision)`, `(blocked reason)`, `(nothing #f)` or `(refused reason)`. |
| `(request id redo buffer-id)` | Redo this requester's latest undo, including an undo of another actor's action. Same result forms as undo; no scope argument. |
| `(request id history-step buffer-id direction scope)` | Rich existing history status/detail for head cursor history and author/label feedback, through the same guarded session transaction. |
| `(request id undo-authors buffer-id)`, `history buffer-id [count]`, `blame buffer-id [count]` | Existing history queries. Blame spans use the plain span codec. |
| `(request id create name lines [facts])`, `reset buffer-id lines [facts [review]]`, `rename buffer-id name`, `delete buffer-id`, `properties buffer-id updates [expected [name]]` | Head lifecycle/fact operations. Require an all-buffer head selected by base policy; producer access is not granted to ordinary agent edit sessions. Reset's optional `(revision fact ...)` review must match the full state; it returns its accepted revision or `#f` on mismatch/deletion. Properties return `#t` on acceptance or `#f` on expected-fact mismatch/deletion; the optional nonempty name commits with the facts through the ordinary name allocator. Omission/`#f` retains unconditional mutation. |
| `(request id visit name lines facts)` | Under the same head lifecycle authority, return `(buffer-id created?)` for a canonical `file` fact. Recheck lookup and creation under one writer; reuse preserves all existing buffer state and emits no create/reset. Ordinary `create` still creates an independent buffer. |
| `(request id discard buffer-id revision facts)` | Delete only if the reviewed revision and whole fact set still match; return `#f` to review again, `#t` on success/already absent. Same head authority as lifecycle calls. |
| `(request id marks buffer-id basis updates drops)` | Publish only this head's marks at the reviewed revision. Position values are pairs; spans are tagged `(span span-datum)`. |
| `(request id read-marks buffer-id)` | Read this head's owned mark names/coordinates using the same span encoding; resume seeds its publication diff from these names. |
| `(request id checkpoint [datum])` | Read or replace this active named head's latest opaque screen checkpoint. No target identity argument; agents cannot access this operation. A screen checkpoint whose third element is the symbol `kept` keeps the retained checkpoint's kill text, so a head sends that text only when it changes. |
| `(request id surface buffer-id)`, `rows buffer-id generation from to` | Read a surface header or a demanded row range guarded by generation. |
| `(request id send to datum)` | Trusted raw application delivery; requires an all-buffer head. |
| `(request id mail to datum)` | Send `(message actor datum)` under the connection's session. Return `#t` for delivery, `#f` for a revoked session or failed delivery. The payload cannot choose the envelope's sender. |
| `(request id owner)` | Return this session's configured question recipient, or `#f`. |
| `(request id ask [to] question choices)` | Ask the explicit recipient, or the configured owner. Return a ticket or `#f` when unavailable; known named heads retain questions while absent. Question is text; choices is a list of strings. |
| `(request id pending)` | Return this actor's questions, oldest first, as `(ticket from question choices)` entries. |
| `(request id answer ticket value)` | Consume a ticket addressed to this session's actor. Return `#f` if revoked, stale or addressed elsewhere. |
| `(request id cancel ticket)` | Withdraw a ticket created by this exact session. Return `#f` if revoked, stale or owned by another session. |
| `(request id log-snapshot start [count [component [actor]]])` | Return `(records end first)`: owned newest-first matching records, absolute end bookmark, and global oldest retained index. Count is a nonnegative exact integer or `#f`; component is a symbol or `#f`; actor is an identity or `#f`. Filters apply before the count limit, so another head's activity cannot crowd out this head's file history. Expired starts clamp to `first`; future/invalid starts refuse. |
| `(request id log-add component datum presentation)` | Append as the connection actor; presentation is `#f`, `append` or `progress`. |
| `(request id log-retention [count])` | Get or set the base journal's retention, default 1,000,000 records. A setter requires an all-buffer head and a positive exact vector-size count. Return the resulting limit; resizing preserves absolute bookmarks. |
| `(request id vt-open command directory rows cols scheme)`, `vt-send buffer-id text size paste? scheme`, `vt-close buffer-id`, `vt-color scheme`, `vt-option option [value]` | Actual terminal command consumers; require an all-buffer head. Options are `shell` and `scrollback`. PTYs remain owned by the base. |
| `(request id reference-fetch)`, `reference-page`, `reference-page! name keys basis-list docs`, `reference-lookup name docs`, `reference-entries docs`, `reference-url entry` | Base corpus and private page service. Fetch/page publication require an all-buffer head; a head addresses its own page. `docs` contains owned module-entry datums scoped to this query. |
| `(reply id ok result)` or `(reply id error text)` | A nonnegative exact request id correlates a reply; a valid request error keeps the connection open. |
| `(event datum)` | Actor mail: `(message from payload)`, question wakeup `(ask ticket from question choices)`, pending-question invalidation `(pending)`, or asynchronous `(answer ask-request-id value)`. Trusted app traffic also uses this envelope. |
| `(changed pending)` | An opted-in store watcher receives `(id . metadata?)` pairs, or `#f` for a full inventory rescan. |
| `(surface pending)`, `(presence)` | Head invalidations: `(id . event)` surface entries, or re-read the actor directory. Surface hints invalidate all rows so coalescing generations cannot lose a row change. |
| `(logged entry presentation)` | One base log record with its original actor and presentation intent. |

The connection owns its session and subscriptions. Disconnect or revocation
ends that session; reconnect creates a new one using current base policy.
Audience is presentation data, not a read-access restriction. Edit, undo and
redo check the current buffer name, read-only fact and session permission
inside the store transaction. Buffer lifecycle and terminal operations
require an all-buffer head. See [session configuration](MULTIHEAD.md#agents-and-sessions).

Changes in snapshots and edit receipts are oldest-first
`(revision actor delta-datum)` entries ending at the returned revision.
An empty chain means current; `#f` means a reset, expired history or a future
basis. Rebase positions only through a complete chain. An edit receipt's text,
revision, change chain and modification facts describe that same accepted
commit. Undo/redo return their accepted revision; a later snapshot can include
subsequent edits from other actors.

A non-false edit-context key groups that actor's action in a buffer, including
across intervening edits by others. Use distinct keys for distinct actions.
The key is not a request ID or replay token. Undo facts reverse with the text;
commit facts describe external state and survive undo/redo.

Evaluation uses the session's granted read-only environment, never a head's
unrestricted M-x environment. Its result is `(status . text)`, with status
`ok`, `unbound`, `error`, `fuel` or `refused`, inside an ordinary successful
reply envelope. Strings are source text; other datums are expressions.
Literal `#f` is valid. Evaluation and result formatting share the configured
engine allowance, and output is clipped to the configured preview length.

An asynchronous answer event uses the original **ask request ID**, while
answering and cancelling use the returned **ticket**. The answer can arrive
before the ask's ticket reply. Keep that request ID unique until the question
is answered or cancelled. A `(pending)` event invalidates the question list:
read it again instead of treating notices as ordered ticket changes.

Store and surface notices are also invalidations. Coalescing can replace
individual store IDs with `#f`, requiring a full inventory rescan. Do not
interpret notifications as an edit history. Slow connections can be closed
when their bounded output queue fills.

## Status, departure and restart

Normal head connections also support these lifecycle requests. The base
serializes reviews with admission and departure; a stale review token is
refused. `prepare-close` and `shutdown` require an all-buffer head.

| Request | Result or effect |
| --- | --- |
| `(request id status)` | An association list containing phase, instance, wire version, fingerprint, buffer/modified/head/terminal/agent counts, attached or detached head names, pending questions and saved-session information. |
| `(request id startup-notice)` | A head takes the one-time recovery notice, or `#f` when there is none. |
| `(request id leaving shutdown-on-exit?)` | Commit departure, or claim the last-head shutdown review when requested; return status, or `(last status)` for that review. |
| `(request id prepare-close)` | Prepare the shutdown review; return `(review token () status)`. |
| `(request id shutdown token)` | Accept the current review and gracefully save and stop the base. |
| `(request id cancel-review token)` | Release this connection's current review; return `#t`. |
| `(closing reason)` | An asynchronous base stop notice preceding disconnection. |

Restart and help use a separate maintenance handshake:
`(maintenance 1 (head "name"))` returns `(maintenance 1 status)`. Maintenance
requires all-buffer head authority but does not claim or attach that named
screen. It accepts source and normal-protocol mismatches, allowing a launcher
to inspect and restart an older base.

Maintenance accepts only `status`, `prepare-restart`, `restart token` and
`cancel-review token`, in the same request/reply envelopes. `prepare-restart`
returns `(review token () status)`; `restart` saves and stops the reviewed
instance. The launcher starts the replacement. See
[restart and recovery](MULTIHEAD.md#restart-and-recovery) for confirmation,
snapshot durability and interrupted-restart behavior.
