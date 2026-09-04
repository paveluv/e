# e v2: a multi-actor design

Status: draft for iteration. v0.1 is the stable prototype this
redesign departs from.

## Why

e v0.1 is an Emacs-like editor built on assumptions inherited from
last-century Unix editors: one human, one keyboard, one screen, one
cursor, one synchronous command loop that owns everything. The Claude
assistant experiment stress-tested every one of those assumptions and
each gave way only through workarounds: worker threads repainting
through duplicated display ports, approval hand-offs smuggled through
condition variables, a capability sandbox bolted beside the real API,
edits raced between keystrokes.

v2 is designed for multiple actors from the ground up: one or more
humans, each with their own UI head, and any number of AI agents
working through the API -- concurrently, in one editing session. The
Emacs interaction vocabulary (buffers, windows, keys, M-x) survives
where it earns its place; the single-user implementation underneath
it does not.

## The layers

```
  human ─ keyboard/mouse ─→ UI head ──┐
  human ─ keyboard/mouse ─→ UI head ──┤        ┌─→ apps
                                      ├─→ buffer state
  agent ───────── API/protocol ───────┤        └─→ kernel
  agent ───────── API/protocol ───────┘
```

### Kernel

Owns exactly three things:

- **registries** -- the generic ownership machinery (v0.1's module
  registration and retraction, kept);
- **module lifecycle** -- load, hot reload, init/retract;
- **the scheduling substrate** -- actor mailboxes and the event
  queues everything above runs on.

The kernel knows nothing of keyboards, screens, or buffers. The
third item is the new one, and it is the deepest fix: v0.1's main
loop -- one thread blocked in a keyboard read, owning all mutation --
forced every concurrent feature into a private workaround (the
terminal reader's display port, the assistant's approval
condition-variable, "workers run between keystrokes"). The kernel
provides the one true answer to "who runs when", and the layers
above stop improvising.

### Buffer state

A multi-actor-safe module owning the state of all buffers, behind a
message-shaped API. It knows nothing of UI or keyboards. Kernel +
buffer state run headless: that pair is the whole system an agent
needs, and later the server that remote UIs connect to.

The v0.1 lesson forcing real redesign here: buffers today are not
self-contained. Point and mark live in windows, the kill ring is a
global, "current buffer" is a UI notion, undo is tangled with
commands. Extraction therefore changes the model, not just the
module boundary:

- **Marks are first-class; cursors are marks.** A mark is a named,
  actor-owned position anchored to content, rebased automatically
  when other actors edit. "Point" is nothing but an actor's cursor
  mark; multiple cursors exist by construction, one per actor (or
  more). Regions are mark pairs. There is no global point anywhere.

- **Edits are transactions.** An edit is data:

  ```scheme
  (edit (actor  (agent claude 3))
        (buffer buf-17)
        (basis  revision-241)        ; what the actor last saw
        (span   (mark-a . mark-b))   ; or an anchored range
        (text   "..."))
  ```

  Buffer state applies it -- rebasing every other actor's marks --
  or rejects it as stale when the basis revision no longer permits a
  clean application. Compare-and-swap on revisions covers local
  multi-actor; operational transforms and CRDTs are deliberately out
  of scope until networked collaboration is real.

- **Single-writer concurrency.** All mutation flows through one
  serialized queue; reads come from cheap versioned snapshots.
  No buffer mutex zoo, no torn reads, and the keystroke path stays
  fast because applying one edit is microseconds. This choice is
  what makes "headless server" true rather than aspirational.

- **Subscriptions, not polling.** Actors subscribe to what they care
  about: a UI to the buffers its windows show, an agent to *log* or
  to a file's buffer. Change events carry the edit data, so a
  subscriber can update incrementally. v0.1's repaint-everything-
  per-keystroke is the degenerate single-subscriber case.

- **Undo: shared and attributed.** Per buffer, one linear undo
  history in which every entry names its actor. "Undo my last edit"
  is best-effort selective undo (apply the inverse if it still
  rebases cleanly). Fully general per-actor undo is research-grade
  and explicitly not promised.

### UI heads

A UI head is one actor's screen: the window tree, keymaps, echo/
notification area, kill ring, histories, and rendering -- everything
v0.1 wrongly held globally that is really per-user. A head talks to
buffer state like any other client: it subscribes to the buffers it
shows and submits edits attributed to its human.

Multiple heads may exist (two terminals into one session; later, two
machines). Each head owns its user's view state exclusively --
including scroll positions, which today's terminal module already
had to invent per-window "unfollowed" tracking for.

Agents do not own UI state. They may send **suggestions** to a
specific actor's head -- `(reveal buffer span)`, `(highlight span
face)`, `(open-view name)` -- which the head honors or ignores by
its user's policy. That is how "the assistant walks you to the bug"
works without an agent ever holding a window.

### Apps

Apps (file editing included -- it is just the default app) talk to
buffer state for content and to UI heads for presentation requests.
An app declares what it owns through kernel registries exactly as in
v0.1. The difference: app operations take explicit actor and buffer
arguments -- nothing reads an ambient "current buffer", because
there is no such thing; there is only "actor X's cursor's buffer".

### Actors

An actor is an identity plus a session:

```scheme
(actor (kind human) (name pavel) (head tty-1))
(actor (kind agent) (name claude) (instance 3))
```

Every operation entering buffer state carries its actor. Identity is
the foundation for four features at once: permissions, attribution
(in-session blame: who wrote this line), the audit stream (v0.1's
`(log-view 'claude)` generalized to every actor), and per-actor
budgets. It must live in the seam from the first commit; it cannot
be retrofitted cheaply.

**The interaction protocol.** Asking is asymmetric today: apps can
prompt only through the one keyboard. v2 defines one generic
mechanism: any actor (usually an app or agent) may pose a question
to another actor -- `(ask actor question choices)` -- delivered
through that actor's head (for humans: the echo area / a prompt) or
mailbox (for agents), answered asynchronously, reply routed back.
The v0.1 assistant's `C-c y` approval flow was a hand-built instance
of this; in v2 it is the library case.

## The seam is a protocol

The buffer-state API is message-shaped: operations in, results and
events out, all plain data -- no closures, no shared mutable
structures across the boundary. One definition buys four things:

1. **Network transparency later** -- serialize the same messages and
   remote heads and agents fall out.
2. **Audit and replay** -- the operation stream is the log; a session
   can be replayed, and "what did agent 3 do" is a filter.
3. **AI-native surface** -- tool calls are already this shape; the
   v0.1 claude module's JSON loop was an accidental prototype of the
   v2 protocol.
4. **The security boundary** -- see below.

To be precise about what this does and does not change: modules keep
importing libraries and calling procedures directly -- hot reload,
registries, and M-x work as in v0.1, and in-process nothing is
dispatched through a bus. "Message-shaped" constrains the signature,
not the transport: across a seam, arguments and results are plain
data -- records, strings, spans, actor ids -- never closures, never
shared mutable structures. The procedure call is the message; the
litmus test is "could this call and its result be serialized without
loss?". Only the actor-facing seams carry the discipline (buffer
state's API, the interaction protocol, UI suggestions, the audit
stream); within a layer, ordinary Scheme -- closures in registries,
per-line styling calls during redraw -- remains ordinary. Nor do
messages imply asynchrony: mutation is serialized through the
single-writer entry point, but when the caller is the UI thread
applying a keystroke, enqueue-and-drain collapses to a synchronous
call. Mailboxes earn their keep across threads and, later, across
the wire.

## Permissions

Scheme gives us two real mechanisms, both proven in v0.1, and one
honest limit.

**Environments are permission sets.** `(claude-safe)` demonstrated
symbol-level security: evaluate in an environment holding only the
granted bindings, and everything else is not forbidden but
*unreachable* -- enforcement, not detection. v2 keeps this for the
expression-evaluation tier.

**Capabilities are closures.** Environments give tiers, not
per-actor policy ("agent A may edit *scratch* but only read lib/").
The object-capability pattern covers that: a session mints, per
actor, a record of procedures curried with the actor's identity that
check policy, attribute, and bound their results internally. Holding
the record is the permission; revocation is dropping it. No new
mechanism -- discipline at the minting point, with one iron rule
learned from claude-safe: nothing crossing a permission boundary may
expose a mutable structure the editor holds; readers return data,
mutators go through edit transactions.

**Budgets are policy too.** Fuel for evaluation, result-size caps,
edit quotas, token spend -- per-actor, enforced at the seam, not
inside individual tools.

**The honest limit.** In-process, all of this constrains a
misbehaving model, not hostile code: one approved full-power eval
owns the image. The confirmation gate ("would I type this at M-x?")
remains the in-process trust boundary. True isolation is the process
boundary -- an untrusted agent runs in its own process speaking the
protocol, and the server enforces policy per connection. The
collaboration seam and the security boundary are the same feature;
building the first delivers the second.

## Module layout

`lib/` stays flat: library `(name)` in `name.e`, hot-reloadable
unless noted, loaded by the same loader. Layer by layer:

**Kernel** (the only layer that is not hot-reloadable):

| File | Owns |
|---|---|
| `kernel.e` | persistent cells, registries, module load/reload, init!/retract, config.e loading, mailboxes, the text of a caught condition (`condition-text`), the two refusal conditions (read-only, refused) |
| `doc.e` | the documentation entry: one record per documented name, the eight-field validation, and the registry modules publish into at init! (`doc:register!`) -- below every module that documents itself |
| `actor.e` | actor identity and sessions, mailboxes, the ask/reply interaction protocol |

**State**:

| File | Owns |
|---|---|
| `text.e` | pure text and span algebra: lines, anchored spans, edit rebasing -- no state, fully unit-testable |
| `state.e` | the buffer store: buffers, revisions, the single-writer queue, marks, subscriptions, attributed undo, and buffer properties -- the buffer-level facts every head shares (visited file, mode name, read-only, disk base), so a second or remote head reads the same truth; per-seat state (cursors, selections, viewports) stays with heads |
| `file.e` | the disk: path algebra, reading, stamps, permission-preserving writes, the line/trailing-newline algebra, the three-way merge over text, path completion, the pre/post-save hooks -- no buffers, no dialogs; the server's side of a save |
| `log.e` | the structured log and audit stream (state, not UI) |
| `policy.e` | permissions: capability minting per actor, budgets |
| `sandbox.e` | the read-only capability environment for expression eval -- v0.1's `claude-safe`, generalized to any constrained actor |

`state.e` keeps its store in a kernel-registered cell, so hot
reloading the state module preserves every buffer -- the same trick
that lets v0.1 reload modules under a running editor.

**UI** (one set of modules, many head instances):

| File | Owns |
|---|---|
| `tty.e` | the terminal backend: raw mode, key/mouse/paste decoding, byte output |
| `style.e` | faces and the style DSL |
| `mode.e` | the mode registry: records, detection by extension and #! interpreter, the memoized line and whole-buffer stylers -- the mode NAME is a store property, the record is this head's |
| `paint.e` | the screen model: damage, cache, painting, synchronized updates |
| `keymap.e` | key syntax, binding contexts, per-head and per-mode dispatch |
| `echo.e` | the notification area's model: message, ghost, transient log queue, prompt geometry |
| `prompt.e` | the prompt: the modal line editor in the echo area (history, completion with the *completions* view, M-x's multiline variants), single-key questions, the commands a prompt may run (allow!), the interaction guard (C-g as a key, the cursor) -- the human frontend of ask/reply |
| `head.e` | a UI head: window tree and layout (with the fit-to-screen collapse), per-user state (kill ring, paste text, scroll, the last command, the keys being dispatched, the quit flag, the host's color scheme), the store client, the scheduling pump (which services its own side effects; two hooks reach up: the frame, the mouse), C-g interruption, the app event dispatch |
| `main.e` | the editor: startup, the seat's loop, shutdown, key dispatch (mode context, global map, SELF-INSERT), the pending-ask presenter, config.e loading and the reload-on-save policy; what it asks of the command layer arrives through setters, so the layer reloads under it -- main itself never does; the loader runs `(main:run)` |

**Platform** (infrastructure, layerless):

| File | Owns |
|---|---|
| `sys.e` | FFI: processes, PTYs, termios, fd plumbing (input decoding moves out, to `tty.e`) |
| `string.e` | the pure string helpers every layer shares (tail, prefix?/suffix?, join, KMP search, lines, common-prefix, insert/delete, elide) -- the one copy; no module keeps its own |
| `json.e` | JSON (unchanged) |
| `https.e` | HTTP(S) with the channel/connector seams (unchanged) |

**Apps** -- ported onto the seams, filenames as in v0.1: `edit.e`
(the command layer -- buffers, windows, files, editing, undo, the kill
ring, indentation, the mouse, the default bindings, the generic
editing helpers: file editing is just the default app, and what M-x
sees bare; every other app imports it as `(except (edit) init!)`),
`eval.e`, `search.e`,
`paren.e`, `describe.e`, `terminal.e`, `md-mode.e`, `md-view.e`,
`log-view.e`, `git.e`, `git-view.e`, `diff.e`, `scheme-mode.e`,
`c-mode.e`, `scheme-format.e`, `pretty-scheme.e`.

**Agents**:

| File | Owns |
|---|---|
| `claude.e` | the assistant: a session minted by `policy.e`, evaluating through `sandbox.e` |

## Import convention

Every cross-module import is prefixed with the module's own name, so
a call site names its layer without looking at the import list:

```scheme
(import (prefix (state) state:)
        (prefix (actor) actor:)
        (prefix (text) text:))

(state:apply-edit!
  (text:edit actor buffer basis span replacement))
```

Two naming rules follow from the prefix.  Seam modules are named in
the singular -- `style`, `file`, `mode`, `string`, `actor`, `doc` --
and an exported name never repeats the module's stem, because the
prefix already says it once: `style:set!`, `log:add!`,
`mode:register!`, `file:read`, `policy:make`, never `styles:set-style!`
or `log:log-entries`.  Inside the module the definition may keep a
longer name; the export list renames it (`(rename (set-style! set!))`).

`(rnrs)` and `(chezscheme)` stay unprefixed. Two consequences,
adopted as rules: exported names drop their module stem
(`state:apply-edit!`, never `state:state-apply-edit!`), and a
module's public vocabulary is designed to read well behind its
prefix -- the prefix is part of the name.

## What stays

- The interaction vocabulary: buffers, windows, key chords, M-x,
  the echo area, describe. Fingers are an API too.
- Hot reload, registries, and the module conventions -- they move
  into the kernel largely intact.
- The terminal emulator, markdown view, describe corpus, https/json
  -- apps and infrastructure that port onto the new seams.
- The test discipline: every extraction lands with its suite green.

## Migration: the strangler path

No big bang. Each stage keeps the editor working and the suites
green; hot reload makes the extractions unusually safe to iterate.

1. **Extract buffer state** behind the new API; the existing UI
   becomes its first (privileged) client. Marks become first-class;
   window point becomes a mark owned by the sole human actor.
2. **Introduce actor identity** in every state operation; the audit
   stream generalizes from `claude` to all actors; edits gain
   attribution.
3. **Split the main loop** into kernel scheduling: the keyboard
   becomes one event producer among several; prompts become the
   interaction protocol; the display-port and between-keystrokes
   workarounds retire.
4. **Mint capabilities** per actor; the assistant moves from the
   bolt-on sandbox to a minted session; budgets move to policy.
5. **Lift the seam onto a wire** (optional, later): serialize the
   protocol, allow remote heads and out-of-process agents; this is
   also the moment real isolation exists.

Stages 1-2 are the bulk of the value and can be validated entirely
by the existing suites plus new state-layer tests. Stage 3 is the
most delicate (it touches everything interactive) and should land
behind the old loop as a facade first.

## Settled questions

- **Edit rebasing is line-based** -- spans anchor to line and column,
  rebasing shifts whole lines; it matches the buffer representation
  and covers local multi-actor. Character-precise rebasing within a
  concurrently edited line rejects as stale instead of guessing.
- **Undo across actors refuses politely**: "undo my edit" applies the
  inverse only when it still rebases cleanly; otherwise it explains
  which later edits overlap rather than guessing.
- **Keymaps layer per head**: a head's own bindings overlay the
  buffer-mode maps, which overlay the defaults -- two humans, two
  keymaps, one buffer.
- **The state layer coalesces notifications**: subscription delivery
  batches change events, so an agent editing in a tight loop costs
  its subscribers one update per batch, not per edit.
- **Vocabulary is settled**: *actor* (an identity acting on the
  session), *head* (one actor's UI), *session* (the running system
  plus its actors), *seam* (a data-disciplined boundary) -- and the
  module names above.
