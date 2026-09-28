# Widgets and views

`control:create-filter!` composes a label, an `entry` over an existing text
buffer, and an italic status label. Its root exposes the `text` output port;
the entry retains the normal editing, undo and stale-edit protection.

An `action-text` view takes `text` and `enabled` input defaults in its options,
plus explicit command bindings such as
`(commands (activate target-view-id insert ("replacement")))`.
`control:activate!` and Return/Space or a valid mouse press/release invoke the
target's registered action. Targets must be mounted in the same head.
Forks remap internal targets and retain external references. Changes to state
connections never execute commands.

`widget:context` returns source, descriptor and resolved inputs. Its optional
`'current` argument checks current availability during a shown-frame action.
`widget:repaint!` invalidates a control's local presentation without publishing
hover or cached geometry to the base. Shared row/column allocation is exposed
as `layout:container` for compound controls.

`table:create!` takes an actor, collection and ordered column symbols. Its
optional fourth argument is an options alist. Use `((kind . list))` for one
column without a heading; use `((identity . name))` to keep `name` when fitting
a narrow pane, independently of its position among the columns. The default
identity is the first column. `set-columns!` retains that identity.
The table composes sticky headings and a normal scroll view;
it does not allocate a view for each row. `table:select!`, `move!`, `activate!`,
`sort-by!`, `toggle-sort!` and `set-columns!` are the same operations used by
keyboard and mouse. F1–F12 use `table:toggle-visible-sort!` to address visible
headings by zero-based position. Wheel movement scrolls
without changing selection. Sorting is shared through the collection;
selection and visible columns belong to each view.

Section rows remain scrollable but cannot be selected or activated. Up/Down,
Home/End and Page Up/Down use the provider's selectable index; a large run of
sections never makes the head walk the result. Page movement uses the shown
viewport height. A result's suggested default is used for the initial choice;
subsequent results retain a surviving stable key.

Rows use the same theme-aware `candidate` and `candidate-hover` faces as
Buffet. A hovered row takes precedence over the keyboard choice without
moving focus, scrolling or publishing selection. Up/Down continues from that
row; Enter adopts and activates it. Leaving the rows restores the keyboard
choice, emphasized only while the table or its filter has focus. Heading
hover adds bold and dotted underline while retaining the heading background.

The optional `activate` command binding receives a `(collection generation
key)` row reference and its result basis after the binding's fixed arguments.
Pending navigation cannot activate the previous row. A domain action should
validate the reference and basis before changing data. Cells retain raw
types until the head formats them; unavailable rows have an explicit ghost.

While a query or its next row page is pending, the table retains one bounded
viewport, replacing it when the new rows arrive. Updates lasting more than
200 ms show a single-cell spinner at the table's top-left corner, including
its filter when present. It starts rotating after one second. Retained rows preserve
their presentation but cannot activate an obsolete result. Initial loading
and unavailable sources still have explicit placeholders. Viewport and
selection lookups share a batch rather than waiting for each other.

Columns measure formatted cells from the retained viewport. Each view remembers
observed widths so filtering does not repeatedly shrink its columns; the last
column takes spare room. No full collection scan or provider width data is needed.

`table:register-presentation!` registers a head-local name, schema version
and column rules `(column minimum alignment dependencies formatter)`. Alignment is `text`,
`tail` or `right`. Select it with a table option such as
`(presentation file-labels 1)`. Dependencies list additional raw columns needed
to present this cell; only the selected columns and their dependencies are
requested. The formatter receives the cell state (`(ready value)`, `(absent)`,
`(pending)` or `(unavailable reason)`), the requested row cells and row attributes.
It returns `(text (start end roles) ...)`, where spans
use character offsets in that formatted text. Map raw match spans through
escaping or abbreviation in this function. Table fitting handles grapheme
clipping and padding, keeping matches off ellipses and neighboring columns.
Rules format only visible cells, and registration replacement invalidates
local presentation without changing the query. A missing named presentation
produces an explicit unavailable view.

An explicit `table:select!` synchronizes the compact query before choosing a
key. Immediate normal activation can resolve that exact key with one bounded
`collection:lookup` while its page is still arriving. Pending movement,
destructive commands and still-preparing queries refuse; nothing is retried
or queued for later execution. `lookup` also lets domain actions validate a
row and its result basis without separate rank and range requests.

`table:emphasize!` supplies a host's current document key without changing
selection or sending interaction updates. `table:activate!` invokes the
composition's `activate` command (or an explicitly supplied command name).
For concrete domain bindings, `table:target` reads the cached hovered or
selected `(selection basis row)` without fetching or adopting it, and returns
false when unavailable. `table:accept!` validates and adopts that captured
target at execution. Domain commands must also validate authoritative object
versions; accepting a selection is not a transaction with its later mutation.

Without a rule, string cells retain their raw text and match spans, and other
values print as Scheme data. Logical depth indents the identity cell in the
TUI. Creation rows show italic names and an italic `[create]` suffix;
pending cells show `[Pending]`. Semantic row roles compose with the normal
choice/hover styles. Providers supply facts, never terminal widths or ANSI.

`window:tool!` retains a named composition for this head. Its builder receives
explicit `open` and `return` command bindings and returns an unmounted app
view. Show the returned host with `window:show-widget!`; simultaneous placements
fork views over shared sources. The host owns origin, MRU and inactive-panel
click routing. An optional app `current` binding receives the focused document
key (or false while the tool has focus), for local emphasis.

Embedded compositions supply their own command bindings. They do not use an
implicit current window. `widget:host` returns the opaque mounting slot;
`widget:keep-host-focus!` lets a pointer action retain the outer host's focus
when it opens a document elsewhere.

`C-x TAB` lists the focused widget path's keys, including app capture contexts
and unshadowed entry, table and global bindings. Its widget calls show the
actual receiver ID, so they can be issued from eval or scripts outside key
dispatch. Shared operations retain their `table:`, `entry:` or `widget:`
names. `widget:descendant` follows named children, for example
`(widget:descendant app-id 'table 'filter 'entry)` in Buffet. Nested
`keymap:call` expressions compose these public operations in bindings.
The listing follows internal focus changes. `widget:key-scopes` exposes that
routing without moving focus or touching a chord; dispatch uses `key-scopes!`
to reconcile focus first.

When a key operates on the selected domain object, use `keymap:derive` to
resolve the canonical command and concrete arguments from the receiving
view. Buffet's kill/delete keys use the ordinary `edit:` commands this way;
no forwarding app API or extra symbolic dispatch is needed. Derivation is
pure and head-local, with no requests or selection publication. Keys and
dispatch use the same resolver, including unavailable reasons. Use
`keymap:checked` for execution-only selection validation and adoption; the
concrete command still carries any required version guard to the base.
See [derived key bindings](KEY_BINDING.md) for the full contract.

The first Keys section follows the mouse independently of keyboard focus.
Widget definitions can provide `pointer-bindings`: a procedure receiving a
shown frame and local x/y coordinates and returning `(gesture action)` pairs.
Use `(click primary ())`, `(click secondary ())`, `(click primary (shift))`
or `(drag primary ())` for gestures, and `keymap:call` with public commands
and explicit targets for actions. The callback must only inspect local,
bounded presentation state: no input dispatch, RPC, focus changes or model
updates. Reuse this same binding lookup in the widget's gesture handler.
Press/release ownership, cancellation and dragging remain input behavior;
reading a binding never starts a gesture.

`widget:pointer-bindings` queries zero-based screen coordinates through the
same shown-frame hit testing as pointer dispatch. Child gestures shadow the
same gestures on ancestors; clipped and modal content cannot leak bindings.
Scroll containers contribute their normal wheel route. The shared table,
entry and action-text controls expose their public commands through this
contract. `table:choose!` takes a table and a `(collection generation key)`
reference, selects that displayed row and activates it when the host has
provided an activation command; an obsolete result refuses.

## Buffer catalogue

Create a head's source with `(document:create-source! 'transient)`, then use
`collection:create!` and `table:create!` as for other collections. Shared
documents, Backups and Trash come from one subscribed base inventory.
Case-insensitive filters search names and file paths, including `~/` spelling.
Compound sorts apply to live rows; archives follow in separate newest-first
sections. The sortable columns are `modified`, `flags`, `name`, `lines`,
`mode` and `file`. Timestamps are raw nanoseconds, flags are `buffer-flag`
enumerations, and paths keep their absolute identity. Format them in the head.
Generated apps and widgets have no Lines value.

For an editable shared filter, use a persistent source and
`(catalogue:create-query! actor source)`, which returns `(query filter-reference)`.
The query owns the source and internal filter buffer; views borrow both.
Retiring the query releases those resources, while unmounting a view leaves
other borrowers intact. More generally, the optional final argument to
`collection:create!` declares owned resource references. Ownership requires a
persistent query so restart cannot leave saved resources without their owner.
`catalogue:neighbor` uses the same cached, unfiltered live ordering for buffer
switching, without fetching archive rows or maintaining a head-side comparator.

Rows have stable keys: `(buffer id)` for shared documents, a base `(model id)`
for widget hosts, and `(local actor attachment token)` for remaining local
buffers. `document:reference` obtains a listed buffer's key;
`document:resolve!` resolves it in the owning head, adopting shared text as
needed. Foreign or retired local tokens return false. A retained widget view
can be mounted with `window:show-widget!`. Local metadata is sent in bounded,
coalesced batches; repaint, hover and generated rows are never contributions.
Disconnect removes the attachment's contribution. Persistent source recipes
rebuild their inventory, not opaque local objects, after restart.

`store:metadata` reads a coherent `(epoch ((id metadata-or-false) ...))`
without copying text or history; an optional list restricts it to those IDs.
Each row includes a `version` witness for content, facts and lifetime.
`store:archive!` takes actor, ID, reviewed version and `trash`, `restore` or
`delete`, returning status and current metadata. It refuses stale versions
and incompatible states atomically. Unrelated buffer changes do not invalidate
the witness. Restoration retains history and uses the usual unique-name
policy; permanent deletion requires an archive and never deletes a disk file.
Validate the query basis at dispatch as well. A refusal refreshes the view
without retrying the action. The host still retires displayed buffers through
`head:forget-buffer!`, which moves windows to surviving buffers.

## Prepared collections

`collection:create!` creates a query over a source model, filter and compound
sort. A vector source uses case-insensitive substring filtering and typed
scalar sorting. Other providers own their domain filtering and ordering.
The compact summary includes `status`, `generation`, `basis`, raw `columns`,
display-row `count`, `complete`, `default`, `details` and `sortable`. `default`
is an empty or single-key list, so a false key is unambiguous. `details` holds
domain facts such as a match count distinct from the number of display rows.
Supported sorts are validated when configuring a prepared query.

Register a provider in the base with `collection:register!`:

```scheme
(collection:register! 'my-source 1
  (lambda (source query cancelled? publish!)
    ;; Queue work in the domain service; return promptly.
    ...))
```

`source` is a borrowed immutable model envelope. `query` contains `id`,
`filter` and `sort`. The provider owns its queue and cancellation checkpoints;
the vector provider uses its own computation worker. Preparation must never
block the collection dispatcher. Use `(publish! result #f)` for an immutable
snapshot or `(publish! #f diagnostic-string)` for failure. Several snapshots
may be published in order; a partial readable result has `complete` false.
Publish completion promptly and coalesce intermediate updates. A repeated
publication of the same result object is a no-op. The callback returns false
after its request is superseded, including an input change away and back.

`collection:make-result` takes columns, count, `row-at`, `locate`, `seek` and
an options alist with `complete`, `default`, `details` and `sortable`.
`row-at` reads `(key cells attributes)` at an ordinal. `locate` maps a key to
an ordinal or false. `seek` takes `(ordinal forward|backward offset)` and
returns a selectable ordinal or false for an entirely ineligible result.
It starts inclusively, skips sections and clamps at selectable ends. Offset
zero finds the eligible row at or beyond the origin in the chosen direction.
Callbacks read prepared indexes only: no scanning, waiting, filesystem I/O
or formatting. Results must remain immutable after publication.

Raw row attributes are a validated alist: `selectable` (boolean), `depth`
(nonnegative logical level), `roles` (semantic symbols), `matches`
(`(column start end)` spans in raw strings), `creation` (`file` or `directory`),
and `pending` (column symbols). Attributes and spans belong to the range's
generation and basis. Missing cells are absent; pending and unavailable cells
are distinct from zero, false and an empty string. Custom non-scalar sorting
belongs to the provider and is declared through `sortable`.

`collection:range`, `rank` and `seek` are guarded by the result generation;
`fetch` batches at most four requests. Rows, keys, attributes and diagnostics
share the reply budget. A range contains at most 256 rows and 512 KiB; cells
over 64 KiB become unavailable, while an oversized key/attribute row makes
the range unavailable. Heads use the shared `range:` cache, queuing misses
on the pump. Painting, hover and cached navigation perform no remote work.

Connect a shared query's filter to its text buffer, not to a particular entry
view. For example, with `filter-source` a `(buffer id)` reference:

```scheme
(connection:bind! (actor:current) query
  (list (list query 'filter #f (list filter-source 'text))))
```

The query owns this connection. Closing the original entry leaves other
views and the query connected to the same authored text. A multiline source
is unavailable; deleting the source removes the edge and restores the input
default. Source edits use the text store's normal revision and undo behavior.

Paged controls use a `service` callback `(id latest-frame)` on the head pump
and a `release` callback `(id)` on unmount or definition replacement. They
request ranges there, outside painting. Their pure `viewport` callback
`(data descriptor width height visible-range)` derives the bounded visible
data used by rendering, decoration and `frame-data` for exact shown-row hits.
Measurement continues to use the compact prepared summary. `anchor` and
`locate` may return `#f` while a row/rank is unavailable: scrolling retains
the last saved stable anchor and a temporary head-local destination.

The widget adapter mounts a base-owned view tree in an ordinary editor
window. Models hold data; views hold independent interaction state; the head
owns rendering and geometry. Multiple views can share one model and its
head mirror. Layout and input routing follow the same recursive tree;
existing apps continue to work.

The executable [composition example](../examples/widgets.e) combines a
connected filter, filename table, editable answer and Undo action:

```scheme
(load (string-append (kernel:installation-directory) "/examples/widgets.e"))
(widget-example:open! '("alpha.sls" "beta.ss" "gamma.e"))
```

Tab traverses the controls. The filter is inside the table's keyboard scope:
type to filter, use Up/Down and Return to choose a row, or click it. Undo the
inserted filename using the button or the entry's normal undo key. The example
validates the selected query basis
before editing. Its data and views survive detach; load its action definition
again in head configuration when using it across head restarts.

Base and head port declarations must agree. A differing or absent declaration
makes its endpoint unavailable; restoring the matching declaration reacquires
its dependencies. Change the contract schema when changing a nominal type's
meaning, and load its implementation in both runtimes. Persistent collection
recipes rebuild their indexes after a base restart.

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
`layout`, `anchor`, `locate`, `decorate`, `caret`, `busy?` and `event` fields accept procedures.
Unknown or duplicate fields are rejected.

`busy?` receives `(data descriptor)` on visible frame preparation and returns
whether this widget is awaiting work. Read already mirrored state only. The
head supplies the delayed activity indicator over the composited top-left
cell, preserving layout and the underlying content. Clipped or hidden corners
schedule no animation; completion restores the original cell. The timer,
animation and frame deadlines stay in the head and publish no model state.

`prepare` receives `(id source inputs)` and derives display data once per source,
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
`((rectangle face) ...)` in local backend coordinates. A face is a semantic
symbol or a nonempty list of symbols layered in order, such as `(header hover)`.
Later rectangles replace earlier ones where they overlap. `caret` receives
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

## Indexed rows

Use a collection for data larger than a small model value. The base owns
the row source, filter/sort recipe and prepared indexes. Rows have the form
`(stable-key ((column . raw-value) ...) attributes)`; columns are
`(column-id label type)`. Omit a cell to represent missing data; `#f` is a
present boolean value.

```scheme
(define source
  (collection:create-source! (actor:current)
    '((name "Name" string) (size "Size" integer))
    '#((first ((name . "alpha") (size . 20)) ())
       (second ((name . "beta") (size . 10)) ()))
    'persistent))
(define rows
  (collection:create! (actor:current) source ""
    '((size ascending) (name ascending)) 'persistent))
```

`collection:summary` reads compact authoritative metadata. Once its status
is `ready`, `collection:range` and `collection:rank` accept that result's
generation. An old generation returns `stale`. `collection:configure!`
changes filter/sort fields against the query model revision; filters use
case-insensitive literal substring matching. Connect an entry's `text`
output to the query's `filter` input to reuse ordinary text editing and undo.
Rows from another collection can themselves be an indexed source.

Base providers register an immutable capture containing column contracts,
count, row-at-ordinal and key-to-ordinal procedures. The supplied vector
provider indexes a source revision once across queries. Filtering and sorting
run in cancellable base jobs. Only metadata and requested ranges reach heads;
queries do not embed another copy of their dataset in recovery snapshots.

Head controls use `range:acquire!`, `request!` and `release!` to own bounded
viewport demand. `range:summary`, `read` and `locate` read local state;
`range:pump!` adopts replies and fetches missing demand outside rendering.
One scheduler serves all views, with up to four operations per batch and a
cache bounded by both entries and bytes. Row replies distinguish absent,
ready and unavailable cells, including an explicit oversized-cell diagnostic.
If visible demand itself exceeds the cache budget, the affected query reports
`(unavailable cache-budget)` until its demand changes. It does not repeatedly
fetch and evict the same pages; reducing the requested range permits a retry.

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
