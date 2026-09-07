# Interactive prompts

Prompts share one editing and presentation engine. File selection, buffer
selection, `M-x`, describe, and command-specific questions therefore use the
same movement, history, completion, wrapping, and styling behavior.

## Editing

Prompt input supports the familiar bindings:

| Key | Action |
|---|---|
| `C-a`, `C-e`, Home, End | Move to an input or visual-line boundary |
| `C-b`, `C-f`, Left, Right | Move by one character |
| Up, Down | Move through visual lines, then history |
| `C-k`, `C-y` | Use the shared kill ring |
| Tab | Complete |
| `C-g`, Escape | Cancel |
| Return | Accept |

Prompt input wider than the screen wraps onto continuation rows marked with
`\`. Continuations align beneath the prompt text. The echo area grows by
shrinking windows to their configured minimum; after eight prompt rows, the
prompt scrolls while keeping its cursor visible.

## Window management during a prompt

A prompt does not lock the window layout. The window-management commands
-- focusing (Meta-arrows, `C-x o`), splitting (`C-x 2`, `C-x 3`),
closing (`C-x 0`, `C-x 1`), and killing the focused buffer (`C-x k`,
refused with a note when it has unsaved changes) -- keep working while a
prompt runs, under whatever keys they are bound to: events resolve
through the live global keymap, so rebound or newly bound chords work in
every prompt as well. Only self-inserting characters always stay with
the input. The mouse works too: clicks focus windows and the status-bar
controls split and close as usual. None of this cancels the prompt, and
the window focused when the prompt is accepted is the command's target
-- the file opens there, the evaluation runs against that buffer. Chords
that resolve to any other command are consumed without effect so their
tail keys cannot leak into the input.

## Multiline input

Prompts that enable multiline input accept Meta+Return to insert a newline.
Lines are automatically indented and the complete input is reindented after
each edit, so a structural change can update following lines immediately.
Pasted multiline input uses the same indentation pass.

At line boundaries, the first `C-a` or `C-e` moves within the current visual
line; a repeated command moves to the beginning or end of the complete input.
The repetition is command-based rather than inferred from the cursor position.

## Completion

Tab extends input to the longest common prefix. If nothing can be added, a
second Tab shows `<completions>` in the current window -- the one the prompt
was invoked from. Repeated Tab cycles through pages when the
list is taller than the window. When the prompt finishes the window gets its
buffer back, point and viewport intact; the split tree never changes.

Completion candidates use a shared semantic style:

- an incomplete or unknown value is italic;
- an exact ordinary match is upright;
- a distinguished editor-defined value uses the editor face.

File prompts apply the same mechanism component by component: the existing
path prefix is upright and the nonexistent remainder is italic.

## Suggestions and inspection

Prompts may display a grey ghost tail after the cursor. `M-x` derives its tail
from structured describe data, so module-published procedures receive the same
parameter hints as built-in entries.

`M-.` may inspect the value at the prompt cursor. In `M-x` it opens the live
describe page for the Scheme symbol under or immediately before point.

## Questions from other actors

An agent or another actor can leave a question for you. The echo area shows
the oldest pending question when no other message or prompt occupies it.
Press `C-c a` (`answer!!`) to answer; Tab offers any supplied choices.
Cancelling the prompt leaves the question pending so you can return to it.
If it was withdrawn while you were typing, the editor says so when you submit.

Extensions use `actor:ask! from to question choices reply!` to send a
question. It returns a ticket, or `#f` if delivery fails. `actor:pending to`
lists pending questions in ticket order as `(ticket from question choices)`.
`actor:answer! ticket answer` and `actor:cancel! ticket` return `#t` for
the call that consumes the ticket and `#f` thereafter. Cancellation does
not invoke the reply procedure.

The protocol copies the question's actors, text, and choices on admission.
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

Actors have a directory as well as a mailbox. `actor:register! who deliver!
[capabilities]` claims an identity `(kind name ...)`; names may be strings or
legacy symbols. Duplicate identities raise a registration-conflict condition,
including races between staged updates. Replace an endpoint by detaching and
registering it in one `kernel:call-with-registration-update` scope. Directory
metadata and delivery follow the same module ownership and rollback rules.

`actor:attached` returns copied `(actor kind name attached-at capabilities)`
entries, oldest registration first; `actor:describe who` returns one or `#f`.
The display name is a string, and the timestamp is the UTC second when the
registration was created. Optional capabilities are descriptive plain data,
defaulting to `#f`; they do not grant permissions. `actor:registered? who`
queries presence, replacing the old, misleading `unregister?` name.

`actor:subscribe! proc` returns a token for `actor:unsubscribe!`. The callback
receives one batch of `(detached actor)` and `(attached actor)` entries per
committed change. Replacement reports both together. Callbacks run in commit
order on a draining writer's thread, outside state locks; post head work with
`head:run-on-main!`. Subscribers hear future commits, and revocation skips
queued deliveries. As with store notifications, a callback must not wait for
a later event. Directory and presence data are independent snapshots.

`actor:detach! who` removes the captured endpoint; repeating it is harmless.
An already selected delivery may finish. Existing questions remain pending
across detach/reload and are available on reattachment. Explicit answer or
cancellation still consumes each ticket once.

`actor:current` returns a copy of the executing actor's identity, or `#f`
outside actor work. `actor:call-as who thunk` scopes that identity to the
thunk and restores it on return, error, or escape. Scopes are local to each
thread; a newly forked worker inherits its parent's context. This attributes
work; it grants no permissions. The editor's startup, configuration, and
command loop run as `head:ui-actor`.

`policy:mint! actor policy [owner]` defaults the escalation owner
to `actor:current`. Standalone callers can supply an owner explicitly or
use `actor:call-as`; without either, the owner is `#f` and questions have
no implicit recipient. Session evaluation runs as the session actor and
restores its caller's context, including when evaluation runs out of fuel.

`policy:make grants fuel buffers cap` specifies the sandbox bindings,
engine ticks per evaluation, writable buffer names (`'any` or a list), and
result/output character cap. `policy:reader` grants the read-only sandbox
with an empty writable-buffer list. Sessions have no edit-count limit;
`policy:revoke!` ends their permission. `policy:sessions` lists `(actor owner)`
pairs. Session events go quietly to the shared log under component `policy`;
use `log:entries`/`log:datum` to query them and `log:subscribe!` to observe them.

## Prompt API

`prompt:read!` accepts completion, initial input, and history. Presentation can be
customized with `paint:prompt-styler`, `paint:completion-styler`, `prompt:completion-highlight`,
`prompt:ghost`, `prompt:inspector`, `prompt:multiline`, `prompt:edge-motion`,
and `prompt:reindent`.

Use `paint:show-prompt-message!` when a non-`prompt:read!` interaction should retain the
same styled label and wrapped layout.
