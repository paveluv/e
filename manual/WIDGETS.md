# Widgets and views

The first widget adapter mounts a base-owned view in an ordinary editor
window. Models hold data; views hold independent interaction state; the head
owns rendering and geometry. Multiple views can share one model and its
head mirror. Recursive layout and input routing will build on this boundary;
existing apps continue to work.

For a small experiment, register a derived text kind in `base-config.e`:

```scheme
(model:register-kind! 'example-text 1 string?)
```

After restarting the base, evaluate in a head:

```scheme
(define data
  (model:create! (actor:current) 'example-text 1
    'session 'persistent '() "first\nsecond\nthird"))
(define first (view:create! (actor:current) data 'text 1 '() '(0 0)))
(define second (view:create! (actor:current) data 'text 1 '() '(0 0)))
(window:show-widget! (head:current-window) first)
(window:show-widget! (window:split-right!) second)
```

Each view selects a row with Up/Down or a click. The wheel scrolls its contents
without changing selection. Enter shows the selected value in the echo area.
Resize either window freely. A `model:commit!` from another head updates both
views. This example is derived model state, with no authored-data undo journal;
use the text store for ordinary editing.

`widget:actions` lists public actions. `widget:act!` invokes them explicitly:

```scheme
(widget:act! first 'move 1 10) ; offset and visible height
(widget:act! first 'choose)   ; (model-id revision row text)
```

The built-in `text` renderer accepts any model: strings become lines and other
values are printed as Scheme data. Its interaction state is `(selection top)`.
`move`, `select`, `scroll`, `choose` and `input` are its public actions. Actions
receive the current mirrored model and provisional descriptor, so activation
does not depend on an older acknowledged selection. An authored domain must
validate the actual target and revision before committing an effect.

## Definitions and lifetime

`widget:register!` takes a kind, schema version and a definition alist.
The definition's `render` field is a procedure, `actions` is an alist of
named procedures, `contexts` lists keymap contexts, `focus` is a boolean,
and `capture` is `full` or `partial`. Optional `measure`, `layout` and
`event` fields accept procedures. Unknown or duplicate fields are rejected.
A renderer receives
`(model-envelope interaction-state width height)` and returns a list of text
lines. The host clips rows and terminal-cell widths, preserving whole glyph
clusters. Rendering reads owned head snapshots and must be bounded and free
of remote requests or mutations.

An action receives `(view-id model-envelope provisional-descriptor . args)`.
An optional `input` action receives an event string, a zero-based pointer
position or `#f`, and `(width height)`. Keyboard and mouse handlers should call
the same public actions used by programmatic hosts. No separate command API
is needed. Renderer definitions are module-owned; runtime mounts belong to
the head and survive definition reloads.

`widget:mount!` takes a root ID and an opaque host slot, claims the entire
tree and returns a head-local runtime handle. Repeating that attachment is
idempotent; attaching the same root to another slot is refused. Children have
no adapter buffers and share batched source subscriptions.

`window:show-widget!` supplies the existing-window adapter. Showing a root
in a second window, including an ordinary window split, forks its descriptors
while sharing sources. Reopening a hidden root reuses its adapter and state.
`widget:arrange!` stages source demand before committing an owned topology
change. Reordering preserves child identities; unlinking releases their
mounts without deleting their descriptors or data.

`widget:unmount!`, or killing the adapter buffer, fences publication, releases
subscriptions and relinquishes the owner generation. It keeps the underlying
model and descriptor. Detach checkpoints retain widget IDs, not generated
text. Reattach claims those views and restores acknowledged state. A missing
renderer or unavailable model produces a placeholder with actions disabled;
installing the definition makes the existing mount usable.

An unsupported view descriptor itself remains an inert adapter with its
original ID and no claimed interaction owner. Its complete envelope can be
inspected with `model:snapshot`; recovery and subsequent saves preserve it.

The [model API](MODELS.md) describes canonical envelopes, ownership, recovery
and the distinction between remote `view:` reads and local `interaction:`
reads. The head automatically queues interaction after presentation and
fences it before lifecycle checkpoints. Geometry never crosses that seam.
