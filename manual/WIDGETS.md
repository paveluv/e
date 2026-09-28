# Widgets and views

The widget adapter mounts a base-owned view tree in an ordinary editor
window. Models hold data; views hold independent interaction state; the head
owns rendering and geometry. Multiple views can share one model and its
head mirror. Layout and input routing follow the same recursive tree;
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
read the current mirrored model and provisional descriptor, so activation
does not depend on an older acknowledged selection. An authored domain must
validate the actual target and revision before committing an effect.

## Definitions and lifetime

`widget:register!` takes a kind, schema version and a definition alist.
The definition's `render` field is a procedure, `actions` is an alist of
named procedures, `contexts` lists keymap contexts, `focus` is a boolean,
and `capture` is `full` or `partial`. Optional `prepare`, `measure`,
`layout`, `anchor`, `locate`, `decorate`, `caret` and `event` fields accept procedures.
Unknown or duplicate fields are rejected.

`prepare` receives `(source inputs)` and derives display data once per source,
input or definition change; its result is
borrowed immutable input to measurement and rendering. Without it, that input
is the source envelope. A renderer receives
`(data descriptor width height visible-range)`; the range is
`(first-row . row-count)`. It returns only those text lines. The host clips
rows and cell widths without splitting grapheme clusters. These callbacks
must be bounded and free of remote requests or domain mutations.

`measure` receives `(data descriptor axis cross-extent measure-child)` and
returns `(minimum preferred)`. `layout` receives
`(descriptor width height measure-child locate-anchor)` and returns
`(child-id (x y width height))` placements. Rectangles are half-open and may
have zero extent. `anchor` maps `(data position width)` to a logical anchor;
`locate` maps `(data anchor width)` back to a backend position.

An action receives `(view-id . args)`. Register the public operation itself;
it obtains three values, source envelope, provisional descriptor and resolved inputs, from
`(widget:context view-id)`. This is a local read. During an action the source
basis is pinned; pointer actions use the source and inputs that were actually displayed.
An optional `event` callback receives `(view-id source descriptor event)`
and returns whether it handled the event. Events are normalized key, text,
pointer, focus/blur or cancel data. Keyboard and mouse handlers call the same
public actions used by programmatic hosts. No separate command API
is needed. Renderer definitions are module-owned; runtime mounts belong to
the head and survive definition reloads.

`decorate` receives the same arguments as `render` and returns
`((rectangle face-symbol) ...)` in local backend coordinates. `caret` receives
`(data descriptor width height)` and returns a local `(x . y)` or `#f`.
The host clips and composes both with the text. Only the active root's focused
descendant supplies the displayed caret. These are head presentation callbacks;
cell coordinates never enter a base model or view state.

## Connected state

Declare portable input and output ports with `port:register!` in a shared
module loaded by both the base and heads. A host connects them atomically:

```scheme
(connection:bind! (actor:current) owner
  (list (list consumer 'files #f (list producer 'files))))
```

Each change names the consumer, input, expected producer and replacement.
Use `#f` to disconnect. The owner must contain the consumer. Types, direction,
ownership and the complete dependency graph are validated; a cycle or stale
replacement leaves the batch unchanged. Retiring an endpoint removes its
bindings, while losing a definition keeps them inert for later reload.
Forking a composition remaps its internal bindings and shares borrowed data.
For an actively mounted composition use `interaction:bind!` with the same
arguments; it fences pending state and supplies the required ownership guards.

The `inputs` alist maps port names to `(ready value basis)`,
`(pending reason basis)` or `(unavailable reason basis)`. False and empty
values are distinct from unavailable inputs. Connected inputs never silently
use a fallback when their producer is unavailable. Same-head interaction is
immediate; another head sees published state. A notification never invokes
an action or edits a consumer's saved fallback.

Outside a mounted widget, use `connection:subscribe!` to acquire dependencies
before local `connection:read` calls, and `connection:unsubscribe!` to release
them. All consumers share the model mirror reader. Mounts acquire demand
before preparation, whose reads never start wire requests.

## Editable children

An `entry` view (schema 1) references an existing `(buffer n)` text source.
Its state is `(caret anchor)`, each a `(row . character-index)` source position;
the descriptor's basis identifies their revision. For an initial empty
selection use `'((0 . 0) (0 . 0))`. Multiple entries share text and undo history
while keeping independent selection. `entry:insert!`, `delete!`, `move!`,
`select!`, `undo!` and `redo!` all take an explicit view ID. They are also the
registered actions, reached by normal keys, committed paste and click/drag.
Tab and Shift-Tab cycle visible accepting children inside the current modal
scope; `(widget:focus-next! view-id [backward?])` is the same host operation.
Undo follows `edit:undo-scope`, or an explicit scope supplied to `entry:undo!`.

The field accepts one line. Multiline paste is refused whole; an external
multiline edit displays an explanatory ghost without changing the source or
its read-only flag. Undo is still available. Selections retain their actual
edit basis: a concurrent disjoint edit rebases, and overlap refuses rather
than overwriting unseen text. No operation switches the current editor buffer.
If the selection's history has expired, Home or End establishes a new caret
at the corresponding endpoint; typing cannot silently reuse an unknown range.

This example runs in a head without any base configuration. It builds a row
inside a column, inside an overlay and a scroll viewport. Make the host narrow
or short to exercise clipping; click either entry to edit their common source.

```scheme
(define who (actor:current))
(define source (list 'buffer (store:create! who "widget example" '("edit me"))))
(define left (view:create! who source 'entry 1 '() '((0 . 0) (0 . 0))))
(define right (view:create! who source 'entry 1 '() '((0 . 0) (0 . 0))))
(define row (view:create! who #f 'row 1 '((spacing . normal)) '()))
(define column (view:create! who #f 'column 1 '() '()))
(define overlay (view:create! who #f 'overlay 1 '() '()))
(define scroll (view:create! who #f 'scroll 1 '() #f))
(view:arrange! who
  (list (list row 0 (list (list 'left left '(grow 1))
                        (list 'right right '(grow 1))) '((spacing . normal)))
        (list column 0 (list (list 'fields row 'fit)) '())
        (list overlay 0 (list (list 'content column '(grow 1))) '())
        (list scroll 0 (list (list 'content overlay '(grow 1))) '()))
  '())
(window:show-widget! (head:current-window) scroll)
```

The caller creates the source. Mounting, splitting or closing views never
creates, copies or deletes that authored text. A GUI head can present the same
logical source and selection with its own metrics and input adapter.

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
Resume acquires retained text history for saved selections before painting,
including edits made while the head was detached. Repeated preparation, hover
and resizing read local snapshots. Measurement and rendering share immutable
descriptor reads within each preparation pass.

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
