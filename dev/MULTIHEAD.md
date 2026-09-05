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
- Tests: `tests/log.ss` (actor on records), `wiring.ss` (a head
  message appears in the base log under the head's actor).

### M5 -- Suggestions

- The suggestion messages and the head's `honor-suggestions` policy;
  highlight through the highlighter registry with a deadline, reveal
  by viewport, message through the echo area.
- `sandbox:` gains `suggest!` under the session's policy.
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
