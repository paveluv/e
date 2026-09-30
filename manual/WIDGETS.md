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

At M-x, refer to a view with the `(model N)` literal; Tab at a documented
model argument offers live models. `C-x TAB` includes **Widget commands**
for the current composition, showing named connections and their target
API calls. `(widget:command-bindings root)` provides the same discovery as
data: `(view child-path kind bindings)` rows, with each binding spelled
`(name target action fixed-arguments procedure-or-false available?)`.
It includes unavailable connections; availability describes the target
connection, while the control and domain action still validate their input.
`widget:commands` returns only usable bindings for one view. Neither query
invokes a command. Use `widget:invoke!` to follow a named connection with
control-supplied arguments after its fixed arguments.

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
it does not allocate a view for each row. `table:select!`, `move!`, `invoke!`,
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
selection or sending interaction updates. `table:invoke!` invokes the
composition's named command, defaulting to `activate` when the name is omitted.
The control validates and adopts its hovered or selected row before supplying
the selection and result basis to the connected action. Domain actions must
also validate authoritative object versions; selection validation is not a
transaction with a later mutation.

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

Buffet's kill/delete keys activate the table's `trash` and `delete` command
connections, targeting `buffet:kill!` and `buffet:delete!`. The keyboard
section follows the full forwarding chain from the control operation to the
app action, with each step's documentation. **Widget commands** also lists
the connections independently. The same explicit connection serves keys,
mouse actions and programmatic activation.

The first Bindings section follows the mouse independently of keyboard focus.
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

## Forwarding and inspection

`widget:invoke!` and `widget:act!` are exported syntax, with private runtime
dispatchers. An `elibrary` registers their call sites while compiling its
procedure definitions. Ordinary app and control commands remain procedures:

```scheme
(edoc "Activate the chosen row." (id model) (selection any) (basis any))
(define (choose! id selection basis)
  (widget:invoke! id 'activate selection basis))
```

One call supplies both execution and inspection; no separate forwarding
annotation can drift away from it. Constants, arguments, lexical bindings and
simple structural operations provide the symbolic call template. Unknown
runtime work remains named rather than being evaluated. Import prefixes and
renames retain the dispatcher's identity.

Outside `elibrary`, wrap a definition or expression in `edoc:expression`.
M-x and the head's evaluation channel do this automatically. A macro that
introduces forwarding must introduce this context around its generated code
too. An unregistered call is a Scheme compilation error, including one hidden
by a macro; the linter is not involved. Quoted code remains data.

For a runtime list of arguments use the explicit spread form:

```scheme
(widget:invoke! (apply id 'activate arguments))
```

The syntax identifiers cannot be passed to ordinary `apply` or aliased as
procedure values. Use `keymap:call` for structured bindings. Its compiler
adapter retains the registered dispatcher identity so the same route is
inspectable. This is an API contract, not an isolation boundary for arbitrary
Scheme code.

An extension implementing its own dispatch protocol can declare it inside
`elibrary` with:

```scheme
(edoc "Dispatch an action." (id model) (action symbol) (args (list-of any)))
(define-forwarding (dispatch! id action . args)
  dispatch-command! inspect-command)
```

Keep `dispatch-command!` private and dedicated to this entry point. The
inspector receives argument nodes `(value datum)` or `(unknown expression)`
and returns a list of steps, each
`(procedure argument-nodes rest-node-or-false reason-or-false)`. It reads
existing local connection metadata; it must not invoke commands, perform I/O
or request remote data. Private forwarding declarations are registered too.

A query annotated `(inspect)` in its edoc may also reduce arguments during
inspection when all inputs are known. Use this only for bounded local reads
such as resolving a mounted descendant, never for producers or model fetches.
Exceptions leave the argument symbolic. Inspection follows only registered
forwarding, reports alternative sites conservatively and bounds cyclic or
large chains. The compiler enforces registration; the local-query contract
remains the extension author's responsibility.

## Buffer catalogue

Create a head's source with `(catalogue-host:create-source! 'transient)`, then use
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
buffers. `catalogue-host:reference` obtains a listed buffer's key;
`catalogue-host:resolve!` resolves it in the owning head, adopting shared text as
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
Tables retain selection by default. The `selection-policy` option `suggest`
adopts a provider's default after filter changes, unless the view has newer
explicit navigation pending; Finder uses this to select a nested file match.
The summary's `input-filter` is the resolved text driving the current job;
`filter` remains the configured fallback used when its input is disconnected.
A changed input gets a fresh `basis` while pending, before rows are published.

Prepared work follows explicit subscriptions. A mounted table retains its
query through the existing model/range subscriptions; several views of one
query share one preparation. An API consumer without a visible table can
retain it with `(model:subscribe! (list query) callback)` and release the
returned token with `model:unsubscribe!`. A global `#f` invalidation observer
does not retain every query. Subscribe before waiting for a ready summary.

Releasing the last reader cancels preparation, completion and enrichment and
discards the query's runtime index. It preserves the recipe, filter, selection
and shared filesystem inventory. Reacquiring demand rebuilds from those
retained resources. Creation and recovery alone do not start providers.
Derived queries retain their upstream models while they are demanded.

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

An optional final `demand` procedure receives `(ordinals columns)` for the
bounded rows actually returned by a range read. It only queues background
enrichment and returns promptly; it performs no filesystem I/O or waiting.
Publish enriched cells as a new immutable generation sharing the existing
ordering. Queries over prepared queries forward demand to the original index.

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

### Filesystem sources

`filesystem:create-source!` takes an actor, absolute home directory, hidden-entry
boolean and persistence. Sources share a cached filesystem inventory in the
base; their collection queries keep independent filters and compound sorts.
`filesystem:create-query!` takes actor, an unshared persistent source and initial
filter text. It returns `(query filter-buffer-reference)` and gives the query
ownership of that source and internal filter buffer. Views borrow these resources.

Finder and ordinary file visits acquire files through the base's
`document:acquire!` service. Creation invalidates affected inventory and
parent listings; unrelated cached subtrees remain available. A creation row's
`proposal` cell carries its kind and observed parent identity. Finder checks
the shown selection/basis, then passes this witness to `edit:visit-file!` with
an explicit destination callback. A changed parent or an already created
target refuses the stale proposal. Acquired file references go to the host;
directory results stay in the Finder query. No filesystem work runs while
painting rows or inspecting their cells.

The filter uses Finder's rooted, non-overlapping literal path keys. Prepared
rows retain hierarchy, exact path identities, raw metadata and match spans.
Keys distinguish observed `(path absolute-path kind)` from uncreated
`(proposal absolute-path kind)` entries. Summary `details` includes `root`, a
raw-character `missing` span or false, `matches`, `unreadable`, `hidden` and
`completion`. Creation rows and intermediate ancestors are display rows;
they do not inflate the match count. Names-only searches avoid file metadata
reads; requesting metadata columns queues enrichment. Metadata sorts acquire
the required facts before publishing their order.

`filesystem:configure!` changes the hidden option against a source revision.
`filesystem:refresh!` invalidates the shared inventory and restarts its queries.
Filesystem watches are disabled, so external changes require refresh. Scanning,
sorting and completion share a separate cooperative queue, allowing other
filesystem queries and in-memory collections to progress.

`filesystem:complete!` takes actor, query and shown generation, and queues
completion only for a complete readable match set, returning an intent number
or false. The same query basis publishes `completion` as `(pending intent)`,
`(ready intent text)` or `(unavailable intent diagnostic)`. The host must match
the intent and query basis and retain the requesting filter revision, applying
the proposed text as one guarded edit only while all remain current.
Reading a result never edits the filter or executes an
activation. Typing, refresh, newer requests and source retirement supersede
obsolete work. The head receives a bounded proposal, never the full match set.

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
recipes rebuild their indexes on demand after a base restart.

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
`select!`, `set-text!`, `undo!` and `redo!` all take an explicit view ID. They are also the
registered actions, reached by normal keys, committed paste and click/drag.
Tab and Shift-Tab cycle visible accepting children inside the current modal
scope; `(widget:focus-next! view-id [backward?])` is the same host operation.
Undo follows `edit:undo-scope`, or an explicit scope supplied to `entry:undo!`.
`(entry:set-text! entry text [revision])` replaces the whole field as one
undoable edit. With a revision it refuses any intervening source edit,
including endpoint insertions. This is the safe application boundary for an
asynchronous completion proposal.

An entry's `(presentation name schema)` option selects a pure formatter
registered with `entry:register-presentation!`. The formatter receives raw
text and its `context` input and returns one `(display roles)` pair per source
grapheme. Rendering, caret, selection and pointer hits use the same mapping.
Finder uses it for conjunction separators and italic missing path components;
the source still contains ordinary spaces. Its `context` input is connected
to the collection's `summary` output, without polling or copying result rows.
An independent `(policy name schema)` option selects a logical text-edit
normalizer registered with `entry:register-policy!`: `(text caret) → (text caret)`.
This handles typed leading paths without encoding terminal coordinates.
Programmatic whole-field replacements already supply their intended text.

The field accepts one line. Multiline paste is refused whole; an external
multiline edit displays an explanatory ghost without changing the source or
its read-only flag. Undo is still available. Selections retain their actual
edit basis: a concurrent disjoint edit rebases, and overlap refuses rather
than overwriting unseen text. No operation switches the current editor buffer.
If the selection's history has expired, Home or End establishes a new caret
at the corresponding endpoint; typing cannot silently reuse an unknown range.

Entry and the ordinary editor use one `text-source:` mirror and edit path.
Mounting an entry does not create a legacy buffer or borrow a window.
Closing or reclaiming its mount fences delayed selection updates: a text
edit that already committed survives, but its caret cannot overwrite a new
mount's state.

For text consumers, `(text-source:open! actor document-id [basis])` acquires
the shared mirror outside painting, optionally retaining a saved selection's
history. `lookup`, `lines`, `revision` and `snapshot` read adopted state
without I/O. `snapshot` returns text, revision and the exact delta chain;
missing history is `#f`. `changes`, `rebase` and `basis-text` handle logical
endpoints and retained edit intent, independent of terminal cells or widgets.
The mirror keeps a bounded history and shares immutable text across views.

`(text-source:edit! actor basis span replacement context positions)` admits
a document edit without a mounted view. `basis` is `(old-text document-id
revision)`, `context` is the store's edit context, and desired result positions
are `start`, `end` or `(row . column)` pairs. It returns text, revision, changes,
projected positions and the committed revision. The presentation adopts that
receipt with `text-source:adopt!`; only a still-current view receives the
projected selection. `text-source:history!` steps the document's existing
attributed undo/redo journal. File I/O remains in `document:`. Entry commands
add their single-line policy and explicit view selection to this common path.

TUI text consumers can import `(head text-layout)` for the editor's shared
wrapping, vertical motion, paging, scroll margins and hit-coordinate mapping.
These helpers take explicit source lines, a `render:` frame and resolved
geometry; they never select a window or perform terminal I/O. `locate` and
`hit` share whole-grapheme geometry. `scroll` and `page` return a proposed
viewport and caret, leaving interaction publication to the caller. Their
`(line . wrapped-segment)` addresses belong to the head; use `anchor` to
convert a viewport address to a logical text position before persisting it.
Changing width recomputes segments from that logical anchor. Ordinary paging
and distant-caret scrolling inspect a viewport-sized part of the source.

Mode renderers also accept explicit sources. Construct `mode:source` from the
presentation text and the facts listed by `mode:required-facts`. Render and
row-style callbacks receive that snapshot, a row and its line, without a
buffer or window. Text-only `mode:memoize-analysis` providers share work
between presentations of the same text. See [mode presentation](MODULES.md#modes).

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
The window adapter unmounts hidden roots before the next frame, after the
invoking action has returned. A restored hidden adapter stays unmounted until
shown. Embedded hosts manage their mounts explicitly with `widget:mount!`
and `widget:unmount!`; window visibility never releases an embedded host.
`widget:arrange!` stages source demand before committing an owned topology
change. Fence interaction before reading expected parent revisions; a stale
revision refuses the batch. Reordering preserves child identities; unlinking releases their
mounts without deleting their descriptors or data.

`widget:unmount!`, or killing the adapter buffer, fences publication, releases
subscriptions and relinquishes the owner generation. It keeps the underlying
model and descriptor. Detach checkpoints retain widget IDs, not generated
text. Disconnect also releases connection-owned subscriptions, including
explicit API demand. Reattach claims visible views and restores acknowledged state. A missing
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
`contexts` may also be a read-only `(id descriptor)` procedure returning
context symbols from already acquired state. Routing and binding inspection
use the same provider; it must not perform I/O. A context change cancels an
unfinished chord.
Captures are checked from outermost ancestor first; ordinary bindings bubble
from the focused leaf. A `full` capture stops unhandled input; a `partial`
capture can list first-key tokens in `yield`. Bind named actions through
`keymap:call` and use `widget:target` to obtain the explicit receiver.

`dispatch:input!` accepts a root and normalized `(key token text-fallback)`
or `(text string source)` input. Optional trailing contexts belong to the
outer host. Paste uses the text path alone. Chords advance one event at a
time, and focus, definition or binding changes invalidate their pending
suffix. Prompt readers use the same resolver.

Pointer callbacks receive `(pointer phase button modifiers x y [click-count])`
in their allocation's coordinates. Backends can append a click count;
omission means one press. The editor exposes word selection as a
`double-click` binding. `widget:event-frame` supplies the shown source
basis; `widget:capture!` keeps motion and release on that target outside its
rectangle. Blur, removal and failed output cancel capture. The TUI decodes
device button codes before routing. Wheel movement changes scroll anchors,
preserves focus and selection, and bubbles only its unconsumed remainder.

## Multiline editor views

`(edit:create-view! actor document-id options)` creates an unmounted `editor`
view over an existing store document. Mount it directly, compose it with
other views, or pass its root to `window:show-widget!`. With `()` or
`((wrap . default))`, wrapping follows the document's `wrap` fact, then
`paint:wrap-lines`. Use `((wrap . #t))` or `((wrap . #f))` to override it.
`((read-only . #t))` prevents edits and undo through this view without making
the shared document read-only. Selection and copying remain available.
Caret movement uses the same `paint:scroll-margin` as ordinary windows.
The `text` output port exposes
single-line sources, like Entry; multiline sources do not satisfy that
string-field contract. [examples/editor.e](../examples/editor.e) places wrapped and
unwrapped editors side by side over one document.

Each view owns `(caret anchor top marked?)`, with all three positions expressed
as zero-based `(row . character)` pairs at the descriptor's text basis.
Widths, wrapped segments and desired display columns remain in the head.
Ordinary document windows also retain a separate editor view for each
document they visit. Switching away and back restores that window's selection;
splitting creates an independent selection over the same text. The outer
checkpoint retains view identities, so resume reuses their saved state.
Ordinary windows route keyboard input, mouse selection and body painting
through these same editor widgets, while keeping the actual document as
their buffer. Gutters, scrollbars and status bars belong to the outer host.
Mode metadata is acquired outside painting; warm navigation and
resizing use the shared mirror without requesting text or publishing geometry.

The canonical commands take an explicit view, including `(model N)` at M-x:

- `edit:select! id caret anchor` establishes a selection, or clears it when
  the endpoints agree. It also recovers from unavailable selection history.
- `edit:move! id direction [extend]` accepts `left`, `right`, `up`, `down`,
  `home`, `end`, `start` and `finish`. Up/Down follow displayed rows and require
  an allocation. Omitted `extend` follows mark activity.
- `edit:set-mark! id active` starts selection at the caret or collapses it.
- `edit:insert! id text` and `edit:delete! id direction` use the source journal;
  deletion directions are `backward` and `forward`. Consecutive insertions and
  corrections share a labeled undo group, bounded to twenty edits; movement
  or a source change ends the run.
- `edit:paste! id text` inserts multiline text as one undo action, separate
  from surrounding typing. Terminal paste also normalizes CR/LF line endings.
- `edit:copy-region! id`, `edit:kill-region! id`, `edit:kill-line! id` and
  `edit:yank! id` use the ordinary copy buffer and optional system clipboard.
  Copying works on read-only documents. Copy and cut refuse a changed selection;
  a refused cut leaves the copy buffer unchanged. Consecutive keyboard kills
  accumulate only while the same view's interaction and source remain current.
- `edit:undo! id`, `edit:redo! id` and `edit:undo-actor! actor id` preserve
  actor attribution and the existing overlap protection. `undo-scope` still
  defaults to `mine`; explicit view calls return journal status and detail.
- `edit:scroll! id rows` changes the logical top without changing selection
  and returns any unconsumed scroll distance for an enclosing viewport.
- `edit:page! id direction fraction` pages by displayed rows, retaining the
  desired column and mark. Direction is negative up or positive down; fraction
  is a positive divisor of the allocated height. A page lands the caret in
  the middle; paging outward at an already reached edge selects that edge.
  This replaces `page-window!` and `page-window-fraction!`. The temporary
  two-argument form operates on the legacy current window.
- Expression motion, marking, killing and transposition take the same explicit
  view: for example, `(edit:forward-expression! id)`, `(edit:mark-form! id)` and
  `(edit:transpose-expressions! id)`. Their ordinary Control-Meta bindings work
  inside nested editors. All views share expression analysis by immutable text
  snapshot. The `expression:` query API now accepts line vectors, not buffers.
- Indentation and formatting accept a view too: `edit:indent-line!`,
  `edit:indent-region!`, `edit:indent-buffer!`, `edit:indent-expression!`,
  `edit:format-region!` and `edit:format-buffer!`. Tab uses the mode's existing
  opt-in and cycles indentation stops. Transformations preserve the logical
  selection; formatting the last line records the final-newline flag in the
  same undo action. Providers compute against a captured `mode:source`.
- `edit:replace-region-text! id start end text` replaces an explicit range at
  the view's declared text basis, with the caret following the accepted edit.
- `edit:basis id` captures `(immutable-lines document-id revision)` for
  computing a bulk change. `edit:rewrite-regions! id basis ranges` accepts
  ordered, disjoint `(start end replacement-string)` entries in that basis.
  It validates the whole proposal before editing, preserves selection and
  groups accepted edits into one undo action. Replacements run from the end,
  avoiding repeated coordinate shifts. Concurrently changed ranges are
  skipped; missing history or changed ownership refuses the remaining work.
  Already accepted edits remain undoable. Use a one-element range list for
  one rewrite; the redundant `edit:rewrite-region!` export is removed.

Arrow keys, Home/End, Control-Home/End and their ordinary Emacs motion keys
use those commands. Shift-arrows extend selection; Control-Space sets the
mark and C-g clears it. Return inserts a newline, Backspace/Delete remove
whole graphemes, and C-_/C-M-_ undo/redo. Mouse presses and drags select using
the shown frame; the wheel scrolls even when the host is inactive. PageUp/M-v
and PageDown/C-v page the editor. M-w copies the region, C-w cuts it, C-k kills
to the line end (or kills the newline there), and C-y yanks the copy buffer.

Entry and editor share guarded mutation and history settlement. A committed
edit survives a callback that closes its view, but cannot overwrite a newly
claimed view or a newer selection. Missing history and overlapping edits
refuse instead of clamping an edit to different text. Read-only sources remain
navigable. Empty insertion and deletion at a document boundary are inert.

The head starts interaction publication with
`interaction:start! actor wake-on-failure`. The publisher itself has no window
or screen dependency. The default head wires publication to frame boundaries
and lifecycle fences; extensions do not need a separate initializer.

Mode-specific editing bindings precede the editor's defaults and include
inherited mode contexts. Pretty Scheme's bracket-closing commands accept an
explicit view and use its source and caret. Mode contexts are cached outside
key routing, so discovering or dispatching a binding performs no remote reads.

The `annotations` input accepts `()` or
`(document-id revision ((span face) ...))`, where each span is
`(start-row start-character end-row end-character)` and each face is a semantic
style symbol, for example `match` or `conflict-disk`. Connect a model output
through the ordinary port protocol, or supply a fixed `annotations` option.
One batch can decorate several views of the same document at different widths.
The editor rebases ranges through retained changes, withholding overlapping
ranges, another document's annotations, or annotations whose history is gone.
Painting uses an index of visible ranges; navigation reuses it. Selection
overrides annotations, which override syntax styles. Annotation data contains
no display geometry.

`mode:add-highlighter!` registers a pure `(source mode caret)` callback for
cheap context-sensitive highlighting. It returns the same `(span face)` pairs,
at the supplied source revision, and runs only for the focused editor.
Callbacks use explicit `mode:source` text; they must avoid I/O and keep work
bounded. The matching-bracket extension uses this interface, including in
nested editors. Tool results acquired asynchronously use `annotations` instead.

Nested editors currently provide these core commands. Ordinary editor windows
still use their existing host; search/conflict producers and window chrome have not
yet moved to the nested editor.
