# Base and heads: the multi-head design

The next stage after the core dissolution: one **base** that owns
everything that exists whether or not anyone is looking, and any
number of **heads** that attach to it, each owning what its user
sees.  `e --daemon` starts a base; `e --attach` starts a head on it;
a second `e --attach` starts another.  Plain `e` keeps working as it
does today: one process that is base and head at once.

This document fixes the vocabulary, the rules, and the contracts, then
lays out milestones that can each be built and committed on their own.
It supersedes the "Stage 5 -- the wire" sketch in `V2_TASKS.md`; the
items there fold into the milestones below.

## 1. Vocabulary

- **Base.** The process that hosts the store, the log, the actor
  directory, and the base apps.  It has no screen.  "Server" describes
  its role on the wire; "base" is what it is.
- **Head.** One seat: a terminal, its windows and layout, keymaps,
  prompts, echo area, kill ring, painting.  A head is a client of the
  base.  Today's `head.e`, `paint.e`, `prompt.e`, `echo.e`, `keymap.e`,
  `main.e`, and the command layer `edit.e` are the head.
- **Actor.** An identity every operation carries: a head, an agent, a
  base app.  Plain data, as today: `(head "pavel-laptop")`,
  `(agent "claude" 3)`, `(app terminal 2)`.
- **Store buffer.** A buffer whose text and facts live in the base's
  store.  Shared truth: every head sees the same text, the same
  facts, the same marks.
- **Local buffer.** A buffer that exists in one head only, with no
  store twin.  Its text and facts live on the head's record.
- **Head app.** An app whose logic runs in the head: it renders local
  and remote data into a local buffer and handles input locally.
  `*buffers*` is the type specimen.
- **Base app.** An app whose logic runs in the base: it owns a process
  or long-lived state, writes its buffer's text into the store,
  publishes facts and a surface, and receives input as messages.  The
  terminal is the type specimen; agents and describe are base apps.
- **Surface.** What a base app publishes beyond text: per-row
  rendition (styles, hyperlinks, row attributes), cursor, size, and
  the facts a head needs to present it.  Heads render surfaces; the
  base never paints.

## 2. The rules

1. **The base owns what exists when nobody is looking.**  A terminal
   process, an agent, a file buffer and its disk facts, the audit
   trail, the documentation corpus.  If every head detached, these
   must survive.
2. **A head owns how things look, and nothing else.**  Windows, the
   layout tree, viewports, points, selections, the kill ring, the echo
   area, the current window and therefore "the current buffer",
   capture and the escape, the styles chosen for painting.
3. **Rendering always happens in a head, from structured data.**  The
   store carries text; surfaces carry rendition; heads paint.  No
   rendered presentation is ever written into the store.
4. **The store holds text, facts, and marks -- shared truth only.**
   Nothing per-seat goes there.  Base apps write their text into it
   so agents and other heads can read it; the terminal's transcript
   is text from the first byte.
5. **First refusal is declarative.**  What an app consumes is data a
   head can read (keymap context bindings, the escape, a capture
   fact), so no keystroke needs a round trip to decide who gets it.
6. **Every actor sees every other actor.**  The base keeps a directory
   of attached actors, queryable by all; any actor can message any
   other through the interaction protocol.
7. **Everything that crosses a seam is plain data.**  The store API
   already obeys this; surfaces, events, suggestions, and the actor
   directory obey it too.  The wire then serializes what already
   exists rather than adding a second model.

A sentence to test any placement against: *the base owns what is
true, the head owns what is shown.*

## 3. Actors and identity

**Head identity** is an arbitrary string, chosen with `--name` or
generated (`user@host:tty` with a numeric suffix on collision).  The
actor is `(head "name")`.  Today's single `(head main)` becomes the
in-process head's name, `main` by default.

**The actor directory** lives in the base as a kernel persistent cell
behind a small `actor:` extension:

- `(actor:attached)` -- every attached actor as plain data:
  `((actor kind name attached-at capabilities) ...)`.
- `(actor:describe who)` -- one actor's entry, or #f.
- Presence events on the store's subscription stream:
  `(attached actor)` and `(detached actor)`, so a head's `*buffers*`
  or a future `*actors*` view updates live.
- Heads register on attach and are removed on disconnect (the wire's
  job) or on process exit (in-process).

**Messaging** is the existing interaction protocol: `actor:send!` for
a one-way message, `actor:ask!`/`actor:answer!` for a question with a
reply.  Delivery is by registration, as now: a head's registration
shows the message in its echo area or as a pending ask; an agent's
posts to its mailbox.  Over the wire, delivery is a message to the
actor's connection.

**Marks.**  Each head publishes its points and regions under its own
actor, so `(store:mark '(head "x") id 'point)` answers where head x is
looking, and a window's serial is unique within that head.

**Capabilities.**  Heads are full-power actors on their own store.
Agents are minted policy sessions (`policy:mint!`), first in-process
on the base, later per connection.  The directory records each
actor's kind and policy so a head can show "agent claude (read-only)".

## 4. The store in the base

The store's API does not change.  Four behaviors do.

- **Names are the store's.**  `store:create!` and `store:rename!`
  enforce unique labels, suffixing `<2>` as the head does today.  The
  head stops computing uniqueness for store buffers and keeps doing
  it for local ones.  Local names must not shadow store names: on
  adoption a head renames a local buffer that collides.
- **Conflicts rebase, never reset.**  Today a head whose edit arrives
  stale wins by resetting the store.  With two heads that destroys
  the other's typing.  The head's edits are small; on a stale refusal
  the head re-reads the twin, rebases its span through the deltas it
  missed, and resubmits.  Only an unresolvable rebase (the edited
  text is gone) becomes a conflict message.  This is a prerequisite
  for two heads and is independent of apps.
- **Audience.**  A new store property, `audience`: `all` (the default)
  or a list of actors.  A head adopts a buffer only if it is in the
  audience.  It replaces the `ephemeral` fact (a private buffer has
  audience `(head "x")`) and gives base apps per-actor pages (a
  describe page requested by one head).
- **The log lives in the base.**  `log:add!` from a head forwards the
  record under the head's actor; the base's log is the one `*log*`
  view renders.  In-process nothing changes but the actor on the
  record.

## 5. Buffers: store and local

A head's buffer list is the union of the store buffers it has
adopted (audience permitting) and its local buffers.  `the-buffers`
already is this list; the local kind is what is missing.

**Local buffers** are head records with `store-id` #f.

- Text: the record's `lines` cache is the master.  View replacement
  writes it directly: no reset, no copy, no notification.
- Facts: a `local-facts` field on the record.  `buffer-fact` reads it
  when there is no store id; `buffer-fact-set!` writes it.  This also
  repairs the store-outage path, where facts today silently vanish.
- Marks: never published (already skipped for id-less buffers).
- Names: unique within the head; a local name that collides with a
  store name being adopted is suffixed.
- Kill: no `store:delete!`.  Rename: no `store:rename!` (already
  guarded).

**Which buffers are local:** every head app's buffer -- `*buffers*`,
`*completions*`, the git views, the `*log*` view (a rendering of the
base's log), markdown renderings of a store buffer.  Everything a
base app makes is a store buffer.

## 6. Apps

### 6.1 Head apps

Unchanged in shape from today: `head:register-app!` with a refresh
and a handler, on a local buffer.  The refresh renders from data --
this head's windows and lists plus whatever the store reports.  The
handler runs locally and has first refusal of the keys the buffer's
mode context leaves unbound.  `*buffers*` renders the store's buffer
list (names, facts, line counts via `store:`) and this head's
current-window marker and recency order; head 2 sees its own marker.

### 6.2 Base apps

A base app is a module that runs in the base.  Its contract:

- **It owns store buffers.**  It creates them under its own actor
  (`(app terminal 2)`), writes their text with `store:edit!` (never
  reset while alive: appended scrollback is an insert, a redrawn
  screen row is a replace), sets their facts, and sets the audience.
- **It publishes facts a head needs to present the buffer:**
  `alive` (#t while the process runs), `capture` (`all` while alive,
  #f after: the declarative first refusal), `cursor-style`,
  `sticky-lines`, `scrollbar`, `wrap`, `status` (a short hint string,
  e.g. the bell glyph), and app-specific ones (`size`, `title`).
- **It publishes a surface** through `surface:` -- a new seam module
  in the base: `(surface:publish! id revision rows)` where rows map
  buffer row numbers to `(styles hyperlinks attributes)` for the rows
  whose rendition changed at that store revision, plus
  `(surface:cursor! id row col visible?)` and `(surface:size! id rows
  cols)`.  Heads subscribe: `(surface:subscribe! id proc)` delivers
  `(surface id revision changed-rows cursor size)` events, coalesced
  per frame.  A head asks `(surface:rows id from to)` for rendition it
  has not cached (scrollback it scrolled into).  Rendition is plain
  data: style vectors of symbols or SGR strings as the emulator emits
  them today.
- **It receives input as messages.**  A head that forwards a key,
  paste, or mouse event posts `(input actor id event data)` to the
  app's mailbox: `actor:send!` to `(app terminal 2)`.  Data carries
  what the handler reads today through parameters: the event
  position within the window and the button.  No reply is expected;
  the head has already decided, from `capture` and the keymap
  context, that the app gets this key.
- **It answers requests** the same way: `(request actor id what
  args)`, e.g. `(resize rows cols)`, `(scroll-hint ...)`, replied
  through `actor:ask!` when an answer matters.
- **It dies cleanly.**  On process exit it sets `alive` #f and
  `capture` #f, withdraws its surface, and leaves the store buffer as
  plain text: the transcript, readable by everyone, read-only.

**The head side of a base app** is one generic module, `remote-app`
(name to settle): for every adopted store buffer with a surface it
installs a mode whose row styler draws the surface's rendition,
follows the surface's cursor for windows that follow (today's
unfollowed-window logic stays per head), shows `status` and the
capture hint, and forwards input while `capture` is on.  The escape,
the escaped state, and the status hint stay in the head exactly as
they are now.  A base app never knows about windows.

### 6.3 The terminal

The emulator already exists headless (`terminal:make-emulator`,
`emulator-feed!`, `emulator-screen`, `emulator-styles`,
`emulator-hyperlinks`, `emulator-state`, `emulator-input`).  The base
app wraps it:

- **Text.**  The store buffer is scrollback followed by the live
  screen, one row per line.  A scrollback push is an insert before
  the screen rows; a screen update replaces the changed rows; a
  resize replaces the screen block.  Updates are coalesced per frame
  as the wake mechanism does today (synchronized-output mode
  included), so a chatty child costs one transaction per frame, not
  one per chunk.  The alternate screen replaces the screen block;
  scrollback is unchanged behind it.  Decorated rows (DECDWL) enter
  the text in their displayed form, as the transcript does today.
- **Surface.**  Per-row styles, hyperlinks, and row attributes for the
  rows that changed; the cursor; the size.  Scrollback rendition is
  served on demand from the emulator's history.
- **Input.**  Keys, pastes, and mouse events arrive as messages and go
  through the existing encoders (`event-bytes`, `send-paste!`,
  `send-mouse!`).  The reader thread stays in the base; there is no
  main-thread seat to marshal to, only the store's lock.
- **Size with two heads.**  The emulator has one size.  Policy: the
  window whose head last sent input sets it (`resize` request on
  focus and on window resize); other heads clip.  Recorded as the
  `size` fact so every head paints honestly.  The tmux "smallest
  attached" alternative is a one-line change if we prefer it later.
- **Death.**  `alive` and `capture` go #f, the surface is withdrawn,
  the last screen is already in the text.  Every head's buffer
  becomes an ordinary read-only store buffer with the read-only
  cursor; the `■` status glyph comes from the `status` fact.
- **Agents** read a live terminal with the sandbox's `read-buffer` by
  name, because the text is in the store.

### 6.4 Describe

Describe is a base app because the corpus is on the base's disk and
fetching is the base's job.

- The corpus (`data/describe/describe.sdata`) and the documentation
  registry (`doc:`) live in the base.  Head modules that document
  themselves (the command layer, head apps) publish their entries to
  the base's registry at init; in-process that is the same call.
- `describe:show!` from a head is a request to the base app; the base
  renders the page as markdown text into a store buffer named
  `*describe*` with audience `(head "x")`, one per requesting head.
  The head's markdown viewer renders it, so the page is shared text
  and local presentation, as the rules require.
- `describe:fetch-data!` runs on the base and reports progress
  through the log.

### 6.5 Agents

- **Phase one:** agents run in the base under minted policy sessions
  (`policy:`, `sandbox:`), as today.  They read buffers by name, edit
  through `session-edit!`, ask through `session-ask!`.
- **Phase two:** agents attach like heads.  A connection is an actor
  with a minted session; the same wire carries their store calls,
  their mailbox, and their suggestions.
- **Suggestions,** the agent-to-head channel from `DESIGN2.md`, become
  concrete: messages a head honors or ignores by its user's policy.
  `(suggest actor (highlight buffer span face seconds))`, `(suggest
  actor (reveal buffer position))`, `(suggest actor (message
  text))`, `(suggest actor (open buffer))`.  The head shows a
  highlight through the existing highlighter registry (blame's tint
  is the precedent), reveals by moving a viewport, and puts messages
  in the echo area under the agent's component.  A head's policy is a
  parameter: `honor-suggestions` as `all`, a list of actors, or #f.

## 7. The wire

The wire serializes what exists.  Design constraints:

- **Transport.**  Unix domain sockets first (`sys:` gains listen,
  accept, connect over a path); TCP with TLS later, reusing the
  libssl binding whose client half exists in `https.e`.
- **Framing.**  Length-prefixed datums written with `write` and read
  with `read`, restricted to the plain-data subset: lists, vectors,
  strings, symbols, numbers, booleans.  Spans and deltas need a
  canonical list form; `text.e` gains `span->datum`/`datum->span` and
  the same for deltas.
- **Sessions.**  A connection is an actor.  `wire.e` in the base
  accepts a connection, reads a hello `(hello kind name capabilities)`,
  registers the actor in the directory, mints its policy session
  (heads full, agents constrained), and then dispatches
  request/response messages with ids to `store:`, `actor:`,
  `surface:`, `log:`, `policy:`, and base apps, and streams events
  (store subscriptions, surface events, presence, mailbox deliveries)
  back.
- **The head's side.**  `remote-store.e` implements the `store:`
  signatures over a connection so `head.e` links against it
  unchanged: the wire is a transport swap.  Edits are applied
  optimistically to the local cache and reconciled on the reply, with
  the rebase rule from section 4.  Likewise `remote-surface`,
  `remote-log`, and the actor registration.
- **Coalescing and resync.**  Events are coalesced per connection per
  frame, as the head's pump coalesces wakes today.  On reconnect a
  head resyncs by snapshot plus revision for every adopted buffer.
- **What runs where.**  In-process (`e`): everything in one image,
  the head calling the store directly.  Split (`--daemon`/`--attach`):
  the base runs kernel, store, log, actor directory, surface, policy,
  sandbox, file I/O, and base apps (terminal, describe, agents,
  git queries); the head runs kernel, head, paint, echo, prompt,
  keymap, tty, mode, style, main, the command layer, head apps, and
  the remote client modules.  Both processes load their own module
  set from `lib/`; a module declares its side (`base`, `head`, or
  `both`) in its header for the loader to filter.

## 8. Processes and startup

- `e [file]` -- today's behavior: one process, base and head in one
  image, the head named `main`.
- `e --daemon [--socket PATH]` -- start a base, no screen; the default
  socket is `$XDG_RUNTIME_DIR/e/base` or `~/.e/base`.
- `e --attach [--socket PATH] [--name NAME] [file]` -- start a head on
  a running base; without a base, report it rather than start one
  silently.
- Promoting a live in-process session into a daemon is out of scope
  for this stage; the in-process head can be told to `--daemon` at
  startup instead.
- `config.e` loads in both processes; head-only forms (styles, keys)
  are no-ops in the base and base-only forms (agent policies) in the
  head, by the same side declaration modules carry.

## 9. Milestones

Each milestone is a set of commits that leaves the editor working
end to end with all suites green, adds tests for what it introduces,
and updates the manual where behavior is user-visible.  In-process
`e` is the product throughout; the socket arrives last.

The v2 review in [section 11](#11-review-findings-and-prerequisites)
records open defects in the current seams.  The milestone assignments
below are completion gates; these fixes can land earlier on their own.

### M0 -- Local buffers

- `head.e`: buffers without a twin; `local-facts` on the record;
  `buffer-fact`/`buffer-fact-set!` consult it when `store-id` is #f;
  `register-app!` and `register-view!` create local buffers; kill and
  rename skip the store for them.
- Move `*buffers*`, `*completions*`, the git views, and the markdown
  renderings onto local buffers.  `ephemeral` disappears from
  `prompt.e`.
- Tests: `tests/mode.ss` (headless facts on local buffers), a new
  `tests/local.ss` (no store twin, facts round-trip, kill without
  delete), `wiring.ss` (the store's buffer list holds no views).
- Exit: `store:buffer-list` in a running editor shows only file and
  scratch buffers; `*buffers*` refresh performs no store transaction.

### M1 -- Head identity, the directory, the conflict rule

- `(head "name")` actors; `--name`; generated default.  The actor
  directory in `actor.e` with presence events.  Marks published under
  the head's actor.
- The `audience` property, honored by adoption.
- Stale edits rebase through missed deltas instead of resetting; the
  conflict message is reserved for unresolvable rebases.
- Close R1--R5: undo preserves foreign work, scratch edits trigger
  unsaved-work protection, questions survive concurrent callers,
  store events stay in revision order, and both selection endpoints
  follow edits.
- Tests: `tests/actor.ss` (directory, presence), `tests/store.ss`
  (audience), `wiring.ss` (a foreign edit racing a keystroke leaves
  both edits in the text).
- Exit: two in-process heads are not yet possible, but every actor
  is named and the destructive reset path is gone.

### M2 -- Base apps in-process: surface, input, the terminal

- `surface.e` in the base: publish, subscribe, rows on demand,
  cursor, size, coalesced delivery.
- `remote-app` in the head: the generic renderer for surfaced
  buffers, cursor following, capture forwarding, status hints, all
  from facts.
- The terminal becomes a base app: transcript text in the store,
  rendition on the surface, input by message, `alive`/`capture`/
  `status`/`size` facts, clean death.  The head keeps escape,
  escaped state, and unfollowed windows.  The size policy "latest
  typist".
- Tests: `tests/terminal.ss` unchanged (the emulator), a new
  `tests/surface.ss` (publish, subscribe, coalescing, rows on
  demand), `interactive.ss` extended (the transcript is readable via
  `store:` while the shell runs; the dead buffer is plain text).
- Exit: `sandbox:read-buffer "*terminal*"` returns the live screen;
  no head code touches the emulator.

### M3 -- Describe as a base app

- Corpus and `doc:` registry on the base; head modules publish their
  entries at init.  Pages rendered to store buffers with a per-head
  audience.  `fetch-data!` on the base.
- Tests: `tests/markdown.ss` unchanged; a describe check in
  `interactive.ss` (a page appears, is a store buffer, and is
  private to the requesting head).

### M4 -- The log in the base

- `log:add!` records carry the actor; the base's log is the one;
  the `*log*` view is a head app on a local buffer rendering it.
- Close A2: concurrent appends preserve every record, including
  during vector growth, before relying on the log for actor audits.
- Tests: `tests/log.ss` (actor on records), `wiring.ss` (a head
  message appears in the base log under the head's actor).

### M5 -- Suggestions

- The suggestion messages and the head's `honor-suggestions` policy;
  highlight through the highlighter registry with a deadline, reveal
  by viewport, message through the echo area.
- `sandbox:` gains `suggest!` under the session's policy.
- Close R6 and R7 before extending capabilities: quotas hold under
  overlapping calls, and reload revokes retained sessions and entry
  points.
- Tests: `tests/sandbox.ss` and `tests/policy.ss` (the call exists
  and is budgeted), `wiring.ss` (an agent highlight paints and
  fades; a refused one does not).

### M6 -- The wire

- Serialization of spans and deltas; unix sockets in `sys.e`;
  `wire.e` sessions with hello, directory registration, minted
  sessions, request/response, event streaming, coalescing, resync.
- `remote-store.e`, `remote-surface`, `remote-log`, remote actor
  delivery on the head; optimistic apply with reconciliation.
- `e --daemon`, `e --attach`, the module side declaration and
  loader filtering, `config.e` in both processes.
- Resolve A1's reload contract for each process and document which
  modules require restart.  Preserve R4's revision ordering through
  event coalescing and resync.
- Tests: `tests/wire.ss` (a base and a head in two processes over a
  socket: attach, adopt, edit, see the other's edit, detach,
  reattach and resync), `interactive.ss` run once in-process and once
  attached.
- Exit: two `--attach` heads on one `--daemon` see each other in the
  directory, edit one buffer, and share a live terminal.

### M7 -- Agents attach

- A connection with agent capabilities is a minted session; the
  claude module ports onto it, as a client of the base.
- Tests: `tests/wire.ss` extended (an agent connection reads a
  buffer, edits within its quota, suggests a highlight a head shows).
- Carry R6 and R7's regression checks over the connection: parallel
  requests cannot overspend, and revoked sessions cannot resume work.

### Later, not scheduled

- TCP with TLS, using the libssl binding's server half.
- Seat records: a base-kept layout per head name, restored on
  reattach.
- Two heads typing into one terminal at once; input arbitration
  beyond "latest typist sets the size".
- Remote disk: file commands from a head going through the base's
  `file:` so a remote head edits the base's files.

## 10. Open questions

- **Terminal text granularity.**  One transaction per coalesced frame
  is the plan.  If the store's delta log (256 entries) proves too
  chatty for blame with a busy terminal, base apps may write with a
  flag that skips the delta log for their own buffers.
- **Row attributes in text.**  Double-width rows enter the transcript
  expanded, as today.  Whether an agent reading the transcript wants
  the expanded or the logical row is undecided; the surface carries
  the attribute either way.
- **Keymap contexts for base apps.**  The terminal's `C-] C-]` and
  `C-] C-y` bindings run head-side commands that send to the app.
  Base apps that want head-side chords declare them as data in their
  facts, or ship a head-side companion module; the terminal will
  show which is cleaner.
- **Head-local config.**  Whether `config.e` is one file with side
  filtering or two files, `config.e` and `head.e`-style per-head
  settings, decided when M6 makes the difference visible.
- **Naming the head-side renderer.**  `remote-app` is a placeholder.

## 11. Review findings and prerequisites

Review baseline (2026-09-04): `v2@8b28a92` against
`main@a9a7b42`, covering 98 commits and 90 changed files.  The branch
separates the former core into `kernel`, `store`/`text`,
`head`/`paint`/`prompt`, and the command layer `edit`.  It still runs
one head in one process; the base/head split and wire above are
planned work.

All seven correctness findings below are **open at this baseline**.
Separate Scheme probes reproduced them.  All 19 existing automated
suites passed (708 checks), including live PTY tests; optional
`vttest` was not run.  The regression checks below are required new
coverage, not claims about what those passing suites already test.
Concurrent stress counts describe individual runs and vary with
scheduling.

### R1: Undo can discard another actor's work (P1; M1)

`foreign-edits-since?` and `history-shift!` in
[edit.e](../lib/edit.e) rely on the store's bounded delta history to
decide whether restoring a snapshot is safe.  A human edit, a foreign
edit, another human edit, and two undos reproduce data loss: the
first undo resets the store and clears that history; the second sees
no foreign edit and restores a snapshot from before it.

Undo must preserve foreign work or refuse.  Its safety check needs
provenance that survives resets and history truncation, or undo must
be expressed as a safely rebased edit.  Missing history cannot count
as proof that no foreign edit occurred.  Add a `tests/wiring.ss`
regression for this sequence, plus cleared and truncated history;
the foreign text must survive every permitted undo and redo.

### R2: Foreign scratch edits do not become unsaved work (P1; M1)

`sync-foreign-edits!` in [head.e](../lib/head.e) marks adopted text
modified only when the buffer visits a file.  Editing an empty
scratch buffer through the store leaves `buffer-clean?` true, so
kill and quit can discard agent-written text without a warning.

Dirty state for editable store buffers must be shared truth and
cover scratch buffers as well as files.  Generated app output needs
an explicit policy separate from unsaved user or agent work.  Add
store and wiring checks that foreign edits make scratch buffers
dirty and trigger the normal kill/quit protection; include the
same check after another head adopts the buffer when M6 lands.

### R3: Concurrent questions disappear (P2; M1)

`ask!` and `take-ticket!` in [actor.e](../lib/actor.e) update the
ticket counter and pending-question list without synchronization.
Eight threads issuing 300 questions each returned 2,400 tickets but
left only 414 pending in one run.  Lost questions cannot be answered;
answer and cancel also race to consume a ticket.

Allocate unique tickets and update or consume pending entries
atomically.  Delivery and reply callbacks must run outside that
lock.  Extend `tests/actor.ss` with concurrent asks and racing
answer/cancel calls: every delivered, unanswered question remains
pending, and each ticket can invoke its reply at most once.

### R4: Store notifications can arrive out of order (P2; M1, M6)

Mutations in [store.e](../lib/store.e) release the store lock before
calling `notify!`.  A later commit can therefore notify first, while
the head rebases positions in arrival order.  A controlled probe
delivered revisions 3 then 2, moved a cursor to column 2 instead of
1, and published that incorrect position back to the store.

Enqueue events in commit order under the mutation lock and deliver
them in that order, keeping subscriber callbacks outside the lock.
Consumers must detect revision gaps or stale events and resync
instead of applying deltas in arbitrary arrival order.  Add a
barrier-controlled `tests/store.ss` case and a wiring check of the
resulting cursor and published marks.  M6 must preserve the same
contract when events are coalesced or a connection resyncs.

### R5: Foreign edits change the selected text (P2; M1)

`sync-foreign-edits!` in [head.e](../lib/head.e) rebases point and
saved positions but leaves the selection mark unchanged.  Inserting
a line above a selection of `pick` changed the selected text to
`zero\npick`.  Region commands then act on unintended text, and
publishing the head's marks can overwrite the store's correctly
rebased region with the stale endpoint.

Rebase both selection endpoints through the same ordered deltas,
with a defined clamp/resync rule when a reset removes their text.
Add `tests/wiring.ss` checks for forward and backward selections,
insertions and deletions before or across them, and agreement
between the visible selection and the published region.

### R6: Concurrent edits exceed a session's quota (P2; M5)

`session-edit!` in [policy.e](../lib/policy.e) checks the budget
before entering the store and spends it after the store call and
its callbacks return.  Holding the first call in a subscriber let a
second call succeed too, despite a quota of one.

Reserve quota atomically before applying an edit and refund it when
the store refuses the edit.  All operations sharing the budget must
use the same accounting.  Add a controlled overlapping-call test
to `tests/policy.ss`: quota one permits exactly one accepted edit,
and a refused edit does not consume the remaining allowance.  Repeat
this through parallel agent requests in M7.

### R7: Reload leaves old capabilities usable (P2; M5)

[policy.e](../lib/policy.e) promises that reloading revokes existing
sessions, but replacing `live-sessions` does not mark old session
objects revoked.  Retaining a session and the old `session-eval!`
procedure allowed evaluation after an actual module reload.

Revocation must reach retained capabilities, including callers that
cached an old entry point.  Use persistent revocation or generation
state, or revoke all prior sessions before replacing the module.
Add a reload regression that retains both a session and old entry
points: evaluation, edits, and questions must refuse afterward;
freshly minted sessions must still work.  Carry this check into M7's
connection lifecycle.

### A1: The documented reload boundary exceeds the implementation (M6)

`reload-module!` in [kernel.e](../lib/kernel.e) rejects any library
that `main` links against, to prevent the running loop from retaining
an old instance while new code uses another.  Besides `kernel` and
`main`, 16 libraries are pinned at this baseline: `actor`, `diff`,
`echo`, `file`, `head`, `keymap`, `log`, `mode`, `paint`, `prompt`,
`store`, `string`, `style`, `sys`, `text`, and `tty`.
[README.md](../README.md) and [the module manual](../manual/MODULES.md)
instead say everything except `kernel` and `main` can reload.

Keep the guard unless the linkage and state-ownership problem is
solved, and correct the documented boundary.  M6's module-side
declaration alone does not solve reload: each process needs an
explicit restart boundary.  Validate allowed and refused reloads
against the running process's dependency graph.

### A2: Concurrent logging loses audit records (M4)

`append-record!` in [log.e](../lib/log.e) updates its vector and
count without synchronization.  A stress run retained only 1,417
of 8,000 appended records.  This race is inherited from the old
logger, rather than introduced by the core split, but makes the
shared actor audit trail unreliable.

Synchronize append, vector growth, and publication of the count;
readers need a coherent view.  Invoke presentation callbacks outside
the lock.  Extend `tests/log.ss` with concurrent writers across
growth boundaries and verify that every submitted record appears
exactly once.  Actor attribution in M4 is complete only when records
are also preserved under concurrent load.
