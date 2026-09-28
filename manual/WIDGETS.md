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
(define first (view:create! (actor:current) data 'text 2 '() 0))
(define second (view:create! (actor:current) data 'text 2 '() 0))
(window:show-widget! (head:current-window) first)
(window:show-widget! (window:split-right!) second)
```

Each view selects a row with Up/Down or a click. To scroll a long value,
place it inside a scroll container; the wheel changes that viewport without
changing the text selection. Enter shows the selected value in the echo area.
Resize either window freely. A `model:commit!` from another head updates both
views. This example is derived model state, with no authored-data undo journal;
use the text store for ordinary editing.

`widget:actions` lists public actions. `widget:act!` invokes them explicitly:

```scheme
(widget:act! first 'move 1)   ; selection offset
(widget:act! first 'choose)   ; (model-id revision row text)
```

The built-in `text` renderer accepts any model: strings become lines and other
values are printed as Scheme data. Its schema-2 interaction state is the
selected row number. `move`, `select` and `choose` are its public actions.
The scroll container owns the viewport; the text leaf has no second offset. Actions
receive the current mirrored model and provisional descriptor, so activation
does not depend on an older acknowledged selection. An authored domain must
validate the actual target and revision before committing an effect.

## Definitions and lifetime

`widget:register!` takes a kind, schema version and a definition alist.
The definition's `render` field is a procedure, `actions` is an alist of
named procedures, `contexts` lists keymap contexts, `focus` is a boolean,
and `capture` is `full` or `partial`. Optional `prepare`, `measure`,
`layout`, `anchor`, `locate` and `event` fields accept procedures.
Unknown or duplicate fields are rejected.

`prepare` derives an index once per source/definition change; its result is
borrowed immutable input to measurement and rendering. Without it, that input
is the source envelope. A renderer receives
`(data interaction-state width height visible-range)`; the range is
`(first-row . row-count)`. It returns only those text lines. The host clips
rows and cell widths without splitting grapheme clusters. These callbacks
must be bounded and free of remote requests or domain mutations.

`measure` receives `(data descriptor axis cross-extent measure-child)` and
returns `(minimum preferred)`. `layout` receives
`(descriptor width height measure-child locate-anchor)` and returns
`(child-id (x y width height))` placements. Rectangles are half-open and may
have zero extent. `anchor` maps `(data position width)` to a logical anchor;
`locate` maps `(data anchor width)` back to a backend position.

An action receives `(view-id model-envelope provisional-descriptor . args)`.
An optional `event` callback receives `(view-id source descriptor event)`
and returns whether it handled the event. Events are normalized key, text,
pointer, focus/blur or cancel data. Keyboard and mouse handlers call the same
public actions used by programmatic hosts. No separate command API
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
change. Fence interaction before reading expected parent revisions; a stale
revision refuses the batch. Reordering preserves child identities; unlinking releases their
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

## Recursive layout and presentation

The `row` and `column` kinds allocate children using their descriptor sizing,
`fit` or `(grow weight)`. Their `spacing` option is `none`, `normal` or
`wide`. Tiny allocations collapse gaps before compressing minima. The
`overlay` kind paints children in their descriptor order, back to front.
`scroll` hosts one child and stores its logical anchor; its `scroll` action
returns any movement left over at the edge.

`widget:prepare!` returns a head-local frame for a root and its allocation.
The existing-window adapter supplies that allocation automatically.
Preparation is separate from presentation: the painter adopts the exact
frames included in successfully flushed output. Partial echo updates retain
the previously shown geometry. Failed terminal output disables widget hits
until a full repaint succeeds. No layout or frame data is sent to the base.

## Focus and input

`widget:focus!` selects a visible accepting descendant. The root remembers
its focus in the base; inactive roots retain it. Modal overlays confine focus
and consume input even when their contents are empty.

Definitions list ordinary `contexts` and optional `capture-contexts`.
Captures are checked from outermost ancestor first; ordinary bindings bubble
from the focused leaf. A `full` capture stops unhandled input; a `partial`
capture can list first-key tokens in `yield`. Bind named actions through
`keymap:call` and use `widget:target` to obtain the explicit receiver.

`dispatch:input!` accepts a root and normalized `(key token text-fallback)`
or `(text string source)` input. Optional trailing contexts belong to the
outer host. Paste uses the text path alone. Chords advance one event at a
time, and focus, definition or binding changes invalidate their pending
suffix. Prompt readers use the same resolver.

Pointer callbacks receive `(pointer phase button modifiers x y)` in their
allocation's coordinates. `widget:event-frame` supplies the shown source
basis; `widget:capture!` keeps motion and release on that target outside its
rectangle. Blur, removal and failed output cancel capture. The TUI decodes
device button codes before routing. Wheel movement changes scroll anchors,
preserves focus and selection, and bubbles only its unconsumed remainder.
