# Base, heads and agents

e is one editor state with any number of screens on it. The **base** owns
what is shared: the buffer store with its text, undo history and marks,
terminal processes, the log, describe's documentation sources and the
permission policy. A **head** is one user's screen: windows, prompts, local
buffers such as `<buffers>` and `<log>`, the kill ring and `config.e`. Plain
`e` starts or attaches to the installation's base; `e --base` runs the base
alone for a supervisor. Scripted clients -- agents -- connect
to the same base under their own permissions.

This iteration supplies the common protocol and Scheme APIs for agents;
bundled agent/provider integrations are deferred. It serves one OS user on
one host, including screens attached after logging in over SSH. Multi-user
access and direct remote connections with mutual certificate authentication
remain future work.

Everyone who acts on shared state is an **actor** with an identity such as
`(head "desk")`, `(agent "helper")`, `(app terminal)` or `(base e)`. Edits,
log records, questions and undo history carry the actor that made them.

## Running a daemon and attaching

Run e on the host where the files live. This command starts the base when
necessary, then attaches a screen; repeat it after each SSH login:

```sh
./e --name desk
```

The base owns `.base/` beside the loader: its `socket`, lifetime `lock`,
process identity record `pid`, and daily diagnostics in `log/YYYY-MM-DD.log`.
The directory is private (mode 700), and its runtime files are mode 600.
Logs older than fourteen days are removed at daily rotation. Use
`--base-working-dir DIR` on each invocation to select an independent base;
an existing directory must be private and owned by you. Directory aliases
resolve to the same base. The head keeps your shell's working directory;
the base runs in its own directory.

`./e [--base-working-dir DIR] [--name NAME] [--] [file]` shares edits and undo
while windows, prompts and local buffers belong to that screen. Terminal
processes and describe sources belong to the base. Head names and the generated default are
described under [startup](CONFIGURATION.md#startup-and-head-names).

For a supervisor, `./e --base [--base-working-dir DIR]` runs the same base
without a screen. It redirects its standard input to `/dev/null` and output
to the daily log. Exit status 3 means a base already owns the directory.
Concurrent ordinary starts attach to the winner. Startup is bounded to two
minutes, with a message after two seconds; a silent hello times out after ten
seconds. Permission failures and foreign files are reported without removing
them. A timeout does not kill or replace the existing base.

SIGHUP leaves the base running. SIGTERM or SIGINT stops it and its terminal
processes. State currently lasts only for the base's lifetime: stopping it
does not save a session to disk. Session saving and `--restart` are being
implemented in later slices. The current wire protocol
is version 3; save your work and stop the old base before changing builds.

`C-x C-c` detaches this head. Shared unsaved text, terminals and other heads
stay alive; local unsaved text still requires confirmation. After restoring
the shell, e reports the base's counts and prints a shell-quoted command to
resume this screen. A new attachment
with the same `--name` restores its split layout, selected window and buffers,
points, viewports, selection, window preferences and kill text. Use distinct
names for independent screens. An explicit file argument opens in the restored
selected window. Without a saved screen, normal startup configuration applies.

`M-x (main:shutdown!!)` stops the base and every attached screen. It reviews
unsaved shared and local text, running terminals, agent sessions and other
heads before asking for consent. `n`, `v`, Esc or C-g cancels; `v` opens
the buffers app. If relevant work changes while the question is open, e
asks again. A modified file whose current text matches disk is clean;
read-only text still needs review when unsaved. New attachments receive a
temporary busy refusal while a review is open and can retry after it ends.
Existing heads and terminals keep working during the question.

When you confirm, the base briefly pauses mutations and publication and
rechecks the review before accepting shutdown. It durably removes any saved
session before ending processes. A failed pause or disk operation resumes service. If deletion
succeeded but directory sync failed, the error reports that durability is
uncertain; it does not claim the old session file remains.

To use this review when the last head quits, put
`(main:shutdown-on-exit #t)` in `config.e`. The base decides which head is
last atomically, even when two quit together. Cancelling keeps the last
head open. The default is `#f`. Restricted heads can always detach, but
only an all-buffer head may prepare or accept shutdown.

The daemon retains the latest completed screen checkpoint, including after an
abrupt SSH disconnect. Shared edits made while absent move the saved positions;
after a reset or expired history, positions clamp to the current text. Markdown
and describe companions rebuild from their shared sources at the new width.
The files app rebuilds its directory/filter/sort state and selected paths
from a small descriptor, then rescans at the new window widths.
Existing registered tools reopen by identity. A missing or hidden source, or a
local view without a restore provider, uses the startup buffer in that window.
Arbitrary local buffer text and query settings of tools without a restore
provider are not saved. Very
small terminals use the editor's usual layout fitting. Checkpoints last only
while the daemon runs. Questions first asked while a known named head is
offline wait for its next attachment; press `C-c a` to answer. An agent's
disconnect withdraws its own unanswered questions.

Only peers running as the same OS user connect. The base holds an exclusive
lock for its whole lifetime; only that owner removes a stale socket or process
record. A clean stop removes both, and the next start cleans up after a crash.
The lock file stays in place. An announced base stop restores the terminal and
exits successfully; an unexpected disconnect reports `e: the base is gone`
and exits nonzero. A local protocol/inbox error retains its specific diagnostic.

The base reads `base-config.e` and a head reads `config.e`; the daemon never
evaluates head configuration and an attached head never evaluates base
configuration. See [the configuration file](CONFIGURATION.md#configuration-file).

## What is shared and what is local

| Shared, owned by the base | Local, owned by each head |
|---|---|
| Buffer text, file facts, undo and redo history, marks | Windows, points, viewports and the selection |
| Terminal processes and their screens | Prompts, `<completions>`, `<buffers>` |
| The log's records | `<log>` renderings and the echo area |
| Describe's `*describe*` source | Its rendered `<describe>` companion and Markdown views |
| Questions waiting for a named head | The kill ring, checkpointed under the head's name |
| `base-config.e`, permissions and sessions | `config.e`, key bindings, styles |

Shared buffers keep file names or names such as `*scratch*`; local buffers
wear angle brackets. [Buffers](BUFFERS.md) covers naming, conflicts, undo
scopes, attribution and the store API; [Terminal buffers](TERMINAL.md) the
shared terminals; [Echo area and log](LOG.md) the shared log; and
[App buffers](APPS.md#publishing-shared-rendition) apps that publish through
the store.

## Editing together

Edits from different actors combine when their ranges do not overlap. If
another edit consumes the text being changed, e reports `Edit not applied`
and refreshes the buffer; shared text and history stay intact
([details](BUFFERS.md#switching-creating-and-killing)). `C-_` undoes this
head's latest action by default; `(undo-scope 'all)` includes every actor and
`M-x undo-actor!!` picks one ([undo](BUFFERS.md#undo-selections-and-the-kill-buffer)).
Another actor's fresh text is tinted briefly in its own color, and
`M-x blame:at-point!` reports authorship from the retained edit log
([attribution](BUFFERS.md#recent-edit-attribution)). Saving captures the
current shared text, whoever typed it. Killing a shared buffer reviews its
current unsaved work. Quitting reviews only this head's local buffers because
shared work stays in the base.

## Questions between actors

An agent or another actor can leave a question for you. The echo area shows
the oldest pending question when no other message or prompt occupies it, and
`C-c a` (`answer!!`) answers it, with Tab offering any supplied choices; see
[prompts](PROMPTS.md#questions-from-other-actors) for the interaction.

Extensions in the base use
`actor:ask! from to question choices reply!` to send a
question. It returns a ticket, or `#f` for an unknown/unspecified recipient or
an unavailable agent. A known named head retains the ticket even if its wakeup
cannot be delivered. `actor:pending to`
lists pending questions in ticket order as `(ticket from question choices)`.
`actor:answer! ticket answer` and `actor:cancel! ticket` return `#t` for
the call that consumes the ticket and `#f` thereafter. Cancellation does
not invoke the reply procedure.

Recipients receive `(ask ticket from question choices)` when a question is
created, and `(pending)` when questions are removed by an answer, cancellation
or session revocation. Treat these as notifications to re-read `actor:pending`;
the table is authoritative even if notifications from concurrent changes
arrive out of order. Revoking several questions sends one notification per
recipient, after removing the whole batch.

The protocol copies and validates the question's actors, text, and list of
string choices before admitting a ticket; an empty list allows free-form answers.
Delivered questions and `pending` reads are independent snapshots; changing
them cannot redirect a ticket or alter another reader's question. All
`actor:send!` messages and `actor:answer!` payloads also copy mutable plain
data at delivery. Cycles and runtime values such as procedures are refused.
An invalid answer raises before consuming its ticket, so the question stays
pending. Sender and receiver may change their own copies independently.

Concurrent questions retain distinct tickets. The protocol releases its lock
before calling delivery or reply procedures, which may ask or answer another
question.
A reply runs on the answering thread with the asker's `actor:current`
identity. A delivery through `actor:send!` runs with the recipient's
identity. Use `head:run-on-main!` for changes to the head from a worker.
A failing reply still consumes its ticket.
Delivery and replies use published registrations, independently of a module
reload that triggered them. Actors registered during initialization become
reachable when that registration update commits; see [module registration](MODULES.md#registries-and-persistent-state).

## The actor directory

Actors have a directory as well as a mailbox. `actor:register! who deliver!
[capabilities]` claims an identity `(kind name ...)`; names may be strings or
symbols. Duplicate identities raise a registration-conflict condition,
including races between staged updates. Replace an endpoint by detaching and
registering it in one `kernel:call-with-registration-update` scope. Directory
metadata and delivery follow the same module ownership and rollback rules.

`actor:attached` returns copied `(actor kind name attached-at capabilities)`
entries, oldest registration first; `actor:describe who` returns one or `#f`.
The display name is a string, and the timestamp is the UTC second when the
registration was created. Optional capabilities are descriptive plain data,
defaulting to `#f`; they do not grant permissions. `actor:registered? who`
queries presence.

`actor:subscribe! proc` returns a token for `actor:unsubscribe!`. The callback
receives one batch of `(detached actor)` and `(attached actor)` entries per
committed change. Replacement reports both together. Callbacks run in commit
order on a draining writer's thread, outside state locks; post head work with
`head:run-on-main!`. Subscribers hear future commits, and revocation skips
queued deliveries. As with store notifications, a callback must not wait for
a later event. Directory and presence data are independent snapshots.

`actor:detach! who` removes the captured endpoint; repeating it is harmless.
An already selected delivery may finish. A named head's identity is retained
with its optional screen checkpoint only after registration commits. Questions
remain available across head detach and head-app reload; all this state ends
when the base stops. Explicit answer or cancellation consumes each ticket once.

`actor:current` returns a copy of the executing actor's identity, or `#f`
outside actor work. `actor:call-as who thunk` scopes that identity to the
thunk and restores it on return, error, or escape. Scopes are local to each
thread; a newly forked worker inherits its parent's context. This attributes
work; it grants no permissions. The editor's startup, configuration, and
command loop run as `head:ui-actor`.

## Agents and sessions

Scheme clients can read, evaluate granted read-only expressions, edit, undo and
redo according to their session's permissions, exchange attributed mail and ask
other actors questions. The messages and primitives an agent uses over the
socket are specified in the
[wire contract](../dev/MULTIHEAD.md#implemented-local-protocol).

The daemon uses `base:connection-policy`, a procedure parameter, to choose a
policy from each connecting actor identity. Heads default to all-buffer write
access; agents default to read-only sessions. For example, in `base-config.e`:

```scheme
(define default-connection-policy (base:connection-policy))
(base:connection-policy
  (lambda (actor)
    (if (equal? actor '(agent "helper"))
        (policy:make '(+ buffer-text-line read-buffer) 100000 '("notes") 8000)
        (default-connection-policy actor))))
```

This grants that named agent the listed read-only evaluation bindings and
edits, undo and redo in `notes`, subject to the buffer's current name and
read-only flag. Buffer write permissions do not restrict which buffers it can
read. The resolver receives
an owned identity; the hello carries no permissions. Each connection gets a
new session and disconnect revokes it. The `eval` request uses that session's
grants, engine fuel and result preview cap. Fuel covers evaluation and result
formatting; the cap clips displayed text and does not bound memory allocation.

`base:connection-owner` independently chooses the actor an agent asks when it
omits a recipient. Heads default to themselves; agents default to `#f` (no
owner). To route the helper's questions to your named screen:

```scheme
(define default-connection-owner (base:connection-owner))
(base:connection-owner
  (lambda (actor)
    (if (equal? actor '(agent "helper"))
        '(head "desk")
        (default-connection-owner actor))))
```

Attach `--name desk` once so the daemon knows that head. Later questions can
wait while it is disconnected. Owner selection supplies no permissions, and
an explicit question recipient does not change the configured owner. Each
connection keeps the owner selected at admission.

From an attached head, use `M-x` to inspect sessions and revoke an agent:

```scheme
(client:request 'sessions)                 ; ((actor owner) ...)
(client:request 'revoke '(agent "helper"))  ; normally 1, or 0 if absent
```

These controls require a head with all-buffer permission. Any such head can
revoke an agent, regardless of where the agent routes its questions. The result
counts sessions selected from the current inventory; private handles stay in
the base. Revocation closes their connections and withdraws their unanswered
questions. Already admitted operations may finish, including an edit whose
reply is lost on close; inspect a fresh snapshot before continuing.

Revocation applies to the selected sessions. A later connection receives a
fresh session from base configuration, and cannot revive the old questions.
It does not change future admission rules or undo completed edits; the usual
undo scopes let you undo an agent's work.

Session events go quietly to the shared log under component `policy`;
use `log:entries`/`log:datum` to query them and `log:subscribe!` to observe them.

## Policy API

`policy:mint! actor policy [owner [close!]]` defaults the escalation owner
to `actor:current`. Base callers can supply an owner explicitly or
use `actor:call-as`; without either, the owner is `#f` and questions have
no implicit recipient. Session work and its audit run as the session actor and
restore the caller's context, including when evaluation runs out of fuel.
Mint/revoke audits describe the controlling caller's actions.

`policy:make grants fuel buffers cap` specifies the sandbox bindings,
engine ticks per evaluation, writable buffer names (`'any` or a list), and
result/output character cap. `policy:reader` grants the read-only sandbox
with an empty writable-buffer list. Sessions have no edit-count limit;
`policy:revoke!` ends their permission. `policy:sessions` lists `(actor owner)`
pairs. Policies and session metadata own their inputs and returned values.
Concurrent session admission/removal uses one inventory; repeated revocation
audits once. Operations admitted before revocation may finish, while later
calls refuse. Revocation also withdraws that session's unanswered questions;
another session using the same actor identity retains its own tickets.
An optional zero-argument `close!` ties a connection to this lifetime. Revocation
takes and clears that procedure with admission/inventory under one lock, then
calls it outside the lock. It runs once; a failure is logged as `revoke-error`
and does not stop the remaining cleanup. Trusted base callers can use
`policy:revoke-actor! actor` to revoke the matching live sessions without their
handles. It returns the number selected from one inventory snapshot; a
concurrent or reentrant same-name replacement is outside that selection.

`policy:session-eval! session expression` accepts source text or a Scheme datum.
It returns `(status . text)`: `ok` for values and captured output, `unbound`
for an unavailable binding, `error` for unreadable/empty input or an evaluation
error, `fuel` for exhausted engine fuel, and `refused` for a revoked session.
Literal `#f` is valid, including source `"#f"`. Multiple source forms use `begin`,
which must be in the grant. Evaluation and result/condition formatting share
the fuel allowance; cyclic results use Scheme graph notation. Formatted values,
output and condition text share the preview cap, with `" ..."` appended when
clipped. Fixed status explanations are not clipped. This is not a total-memory
limit. The common connection's `eval` request uses this same evaluator.

The base's `policy:session-send! session to datum` delivers `(message actor datum)`, with
the sender supplied by the session. It returns `#f` if the session is revoked
or delivery fails. `policy:session-ask! session [to] question choices reply!`
asks the explicit recipient, or the session's configured owner when omitted.
The reply procedure receives the answer as plain data; answering does not
change the session's permissions. `policy:session-answer! session ticket value`
answers only questions addressed to that actor. `policy:session-cancel! session
ticket` withdraws only questions created by that exact session. Both return
`#f` for a revoked session, a stale ticket or a different recipient/session.

`policy:session-edit! session buffer-id basis span lines [context]` returns
`(values 'applied (revision text-vector changes edit-facts))` on success.
The receipt is owned plain data captured at the transaction, with each change
represented as `(revision actor delta-datum)`; use `text:datum->delta` to
reconstruct a delta. `edit-facts` contains the `modified` and `modified-at`
pairs from that same commit. Stale/refused outcomes keep their existing meanings.
Context is `#f` or `(key label [undo-facts [commit-facts [expected]]])`. Reuse a key for
the parts of one action in one buffer; keys belong to the session actor.
Undo reverses text and undo facts together. Commit facts describe external
state and survive undo/redo. Expected facts are pairs for exact present values
or symbols for absent keys; mismatch returns `(stale property-changed)` before
mutation. They do not weaken the session's write permissions or enter history.
The context contains plain data only. The session
owns the submitted span, replacement lines and context as well as its receipt.
`policy:session-undo! session buffer-id [scope]` defaults to `mine`, and accepts
`all` or `(actor who)` under the same buffer grant. It returns an applied
revision or the store's blocked/nothing outcome, or a permission refusal.
`policy:session-redo! session buffer-id` redoes that requester's latest undo,
including an undo of another actor's action, with the same result forms.
All three mutations check the current buffer name and shared `read-only` flag
in the store transaction. A refusal returns `buffer`, `read-only` or `revoked`;
an edit cannot clear an existing read-only flag through its own context.
