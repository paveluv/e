# Widgets and views

`markdown:create-view!` creates an unmounted Markdown presentation over a
borrowed source document. Forked views share the base interpretation while
keeping their selection and scrolling independent. Fitted text and hit maps
belong to each head. Selection anchors identify a source block, source row,
table field and character, rather than a wrapped display row.
Arrow keys and Page Up/Down move through the presentation; `M-<` and `M->`
acquire the beginning and end. Mouse dragging selects text. Return or a link
click invokes the view's `open-uri` command with the source document ID and
URI, letting the host choose where to open it. Link hover never fetches data.

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

Use `((cell-commands (mine . mine) (disk . disk)))` to make specific columns
invoke named table commands. Each command receives the same exact row
selection and result basis as Enter. Other cells use `activate` when bound.
`table:choose!` accepts an optional command name for the same operation in
scripts. Stale rows or changed column geometry cannot activate a different
cell; an explicit missing command refuses before changing selection.

`review-preview:create!` takes an actor and a conflict or rewrite draft,
returning `(request document)`. The document is disposable, read-only output;
the original text remains editable. Connect a table's `selection` output to
the request's `selection` input to follow rows without an extra head command.
The request's `annotations` output connects to an editor's `annotations`
input. Both text and highlights carry their publication revision. A preview
retains its upstream selection dependencies only while demanded.
Moving among rows of one unchanged document reuses the derived text. A rewrite
preview marks its selected revision and reports `blocked` when later edits
prevent inversion. `store:revision-span` locates one retained edit with its
current source revision without deriving provenance for the rest of the log.
Highlight batches contain at most 512 other conflict regions plus the
selected region; `truncated?` in the request reports omitted highlights.
`review-preview:close!` takes the actor and request, retiring its scoped views
and owned output while preserving the draft and source documents.

`delta-log:create!` composes these services with a table, Mine/Disk cell
commands, explicit bulk/settle controls and an editor. Supply host commands,
`conflicts` or `rewrite`, and an ordered list of document IDs (one for a
rewrite). Each constructor call has its own draft and query. Row navigation
publishes ordinary view interaction; base derivation follows the connection.
The query owns its draft. Each root view owns its preview request and output:
forking the view shares choices but gives each table an independent preview.
Hiding the composition releases demand while keeping choices; retiring its
query removes its scoped views and their resources. Source documents are
borrowed and remain intact.

A view declares private model resources in its `owned` option. Their model
scope names that view. `view:fork!` copies them and remaps sources and internal
connections; `view:retire!` releases them through their base kind's registered
lifecycle. Borrowed sources remain shared. Base services register copy and
release procedures with `view:register-resource-kind!`; copy preparation must
provide rollback for output allocated before the guarded model transaction.

For a private collection query, pass its owning view after the owned resource
list to `collection:create!`. Its base provider registers source copying with
`collection:register-copy!`; ordinary borrowed queries remain shared. Git uses
this path so a fork has its own patch request and output while sharing history.
Retiring that fork releases only its private preview.

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

`widget:inspect` takes a mounted root and a traversal limit (at most 256).
It returns portable containment, source, command and port declarations plus
acquired connections, with an explicit truncation flag. It reads local
metadata without executing actions or fetching source payloads.

`inspection:create! actor subject section-names` creates a base-owned,
attachment-specific listing and returns its ID and named section sources.
`inspection:publish!` accepts changed sections against the header revision
only while demanded, with limits of 2048 rows and 256 KiB across all sections.
Omitted or equal sections produce no notification. Inspect the subject,
definition basis and section references through `model:snapshot`. A departing
producer leaves the snapshot unavailable; reattaching cannot silently adopt it.

`bindings:create! commands root` creates an unmounted inspector with explicit
host commands and an inspected mounted root (or false for global keys).
`bindings:inspect!` changes that subject. The composition lists mouse and
keyboard bindings, full forwarding chains, widget commands, containment,
sources, ports and connections. Its ordinary scroll viewport retains logical
row anchors through reflow. `bindings:page!` pages up or down; pointer selection
and `bindings:copy!` use stable row/field/character anchors at the shown basis.
Selection belongs to each listing view. `bindings:show!` and `bindings:open!`
provide the default window placement and active-window following.

`bindings:capture-key! inspector` inserts a temporary modal child to collect
a chord through the normal event pump. Its prefix is ordinary view state;
completion removes and retires the child, then publishes the key's resolution,
forwarding chain, origin and shadowed/contextual meanings. Escape and C-g
cancel. `bindings:press! reader key` is the same explicit operation used by
input dispatch; it never executes the inspected command. `bindings:key!`
provides default placement for C-h k. The former `describe:key!` is removed.

Live facts are acquired only while an inspector is mounted. Mouse changes
publish only the mouse section, reusing cached keyboard traces. Hovering over
the inspector itself freezes that section, so reading and scrolling do not
replace the inspected subject. Layout reads acquired snapshots and performs
no inspection RPC. Listings explicitly report unavailable or truncated data.

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

Create a head's source with `(catalogue:create-source! actor home 'transient)`, then use
`collection:create!` and `table:create!` as for other collections. Shared
documents, Backups and Trash come from one subscribed base inventory.
Case-insensitive filters search names and file paths, including `~/` spelling.
Compound sorts apply to live rows; archives follow in separate newest-first
sections. The sortable columns are `modified`, `flags`, `name`, `lines`,
`mode` and `file`. Timestamps are raw nanoseconds, flags are `buffer-flag`
enumerations, and paths keep their absolute identity. Format them in the head.
Widgets have no Lines value. Named root views are listed directly from base
state; nested children are excluded. A root's `name` option supplies its label
and its optional `audience` option restricts visibility, as for documents.
The default window host names its roots and limits tools to their head.

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
for named root views. Hidden views retain their identity across detach and
restart. Retiring a view removes its entry while preserving borrowed sources.
`catalogue-host:reference` and `catalogue-host:resolve!` are default window
placement adapters; a retained view without a placement can be mounted with
`window:show-widget!`. There is no head contribution stream or local-token
database. Model notifications update only affected catalogue metadata;
selection, repaint and generated text never republish rows.

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

Views are session-persistent by default. Pass an optional resource-owner model
after the initial state in `view:create!` to give a view that model's scope and
persistence. This is separate from containment and from the head's mount lease.
Forking preserves resource lifetimes and remaps internal owners along with the
copied tree. An allocation or fork refuses if its resource owner disappears.

After unmounting, `view:retire!` takes actor, view and expected model revision.
It atomically removes the view from its parent, clears affected host focus and
releases borrowed child subtrees as unowned roots. Views scoped to the retired
view's lifetime are retired too, including their scoped descendants. Sources and command targets are
borrowed and survive. Use this operation for views; `model:retire!` handles
ordinary model state. A resource-owning service may retire its scoped views
when its request or session ends.

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

For sources with separately published presentation data, optional `snapshot`
receives `(id latest-source)` and returns an already acquired coherent source
envelope, or false while none is ready. It retains the source identity and
never advances beyond the mirror. Commands and shown frames use that exact
basis. The editor uses this to pair VT text with its rendition, retaining the
previous pair across a publication gap. Acquisition belongs to `service`;
`snapshot`, painting and navigation do no remote reads.

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

The head captures a coherent dependency bundle when its mount's subscriptions
change. Frame preparation reuses that bundle, resolving current provisional
view state and mirrored text locally. Input callbacks retain the exact bundle
of the displayed frame; a later rewire does not change what an earlier click
saw. Treat the supplied source, descriptor and input data as immutable.

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
normalizer registered with `edit:register-policy!`. It receives proposed line
strings and logical result positions, returning both normalized values.
Entries and editors apply the same policy before one guarded journal edit.
This handles typed leading paths or Scheme indentation without terminal
coordinates. Programmatic whole-field replacements already supply their
intended text and bypass this policy.

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
`contexts`, `capture-contexts`, `capture` and `yield` may also be read-only
`(id descriptor)` procedures returning their respective values from already
acquired state. Routing and binding inspection use the same providers;
they must not perform I/O. A context change cancels an unfinished chord.
Captures are checked from outermost ancestor first; ordinary bindings bubble
from the focused leaf. A `full` capture stops unhandled input. Either policy
can list first-key exceptions in `yield`; every suffix of a yielded chord
keeps that first key's route. Bind named actions through
`keymap:call` and use `widget:target` to obtain the explicit receiver.

Public operations can declare a contextual receiver in their edoc:

```scheme
(edoc "Toggle hidden entries." (id model "Finder view")
      (receiver id (view finder)))
(define (toggle-hidden! id) ...)
```

The compiler checks that `id` is a non-rest formal with type `model`.
Widget action registration checks that its kind matches the annotation.
An operation shared by multiple kinds can declare `(view table list)`.
This metadata affects discovery and argument assistance, not evaluation.
M-x inserts an explicit model literal; scripts still supply ordinary values.

M-x captures the focused view and its ancestors. A widget can additionally
expose finite child paths with a definition field such as
`(receivers (table table))`: the first symbol labels the receiver, and the
remaining symbols name its child path. Unlisted siblings and private children
are not searched. `widget:receivers` reads this structure from local mirrors;
`widget:receiver-live?` checks a captured identity and ownership generation.
Custom prompt hosts pass these rows in the origin's `receivers` field.

`dispatch:input!` accepts a root and normalized `(key token text-fallback)`
or `(text string source)` input. Optional trailing contexts belong to the
outer host. Paste uses the text path alone. Chords advance one event at a
time, and focus, definition or binding changes invalidate their pending
suffix. Prompt readers use the same resolver.

An optional `capture-event` procedure uses the ordinary event signature but
runs from the outermost ancestor before child handlers. Returning true
consumes the input. It receives key/text events, pointer events with local
coordinates, and `(scroll dx dy units x y)`. Yielded keys skip that ancestor's
capture handler. Modal scope also bounds this phase. This lets a process
control intercept input while alive and release its child text viewport after
exit. `capture-pointer-bindings` describes its clickable commands with the
same signature as `pointer-bindings`; inspection lists capture commands first.

Pointer callbacks receive `(pointer phase button modifiers x y [click-count])`
in their allocation's coordinates. Backends can append a click count;
omission means one press. The editor exposes word selection as a
`double-click` binding. `widget:event-frame` supplies the shown source
basis; `widget:capture!` keeps motion and release on that target outside its
rectangle. Blur, removal and failed output cancel capture. The TUI decodes
device button codes before routing. Wheel movement changes scroll anchors,
preserves focus and selection, and bubbles only its unconsumed remainder.

## Multiline editor views

The editor's optional `follow` boolean input follows a service-provided
cursor. This is derived from base output and does not publish a new selection
for every frame. `editor:frame-state` captures the prepared logical selection
and top anchor when a composition leaves follow mode.

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
Current-window editing, formatting and undo commands use that window's editor
view. `edit:call-as-one-edit!` groups commands across ordinary and nested views:
one undo step per document, with a common batch and the outermost scope's label.
Mode metadata is acquired outside painting; warm navigation and
resizing use the shared mirror without requesting text or publishing geometry.

The canonical commands take an explicit view, including `(model N)` at M-x:

- `edit:select! id caret anchor` establishes a selection, or clears it when
  the endpoints agree. It also recovers from unavailable selection history.
- `edit:move! id direction [extend]` accepts `left`, `right`, `up`, `down`,
  `home`, `end`, `start`, `finish` or an absolute `(row . character)` position.
  Absolute movement clamps and reveals the new caret. Up/Down follow displayed rows and require
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
  two-argument form operates on the current window.
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
- `edit:replace-region-text! id start end text` replaces an explicit range of
  the current mirrored text, with the caret following the accepted edit.
  Use a captured basis for ranges computed before other commands.
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

Ordinary editor windows mount this same editor. Their outer placement and
chrome remain host responsibilities; search and conflict tools retain their
adapters until those applications migrate.

## Terminal views

Ordinary terminal windows mount this same composition. Splitting forks the
view's capture and scroll state; switching buffers and resuming a named head
retain the view identity. The process and its output remain shared.

A definition may provide `status`, a pure `(id descriptor active?)` procedure
returning text spans. Each span is `(text . style-or-action)`; a
`keymap:call` makes it a clickable, inspectable control. Hosts decide where to
present these spans. Reading status must use already acquired state.

`(terminal:create-view! actor document-id)` creates an unmounted terminal
composition over an existing base-owned process document. Its `text` child
is the read-only editor. Multiple views share the process, output and grid,
with independent capture, following and selection. View creation, forks and
unmounting never spawn or terminate a process.

Use `terminal:set-capture!` with `partial` or `full`, or
`terminal:toggle-capture!`, with an explicit view. Partial capture yields
`C-x` and `M-x`; `C-]` toggles capture. `terminal:send!` types text,
`terminal:paste!` honors bracketed paste, and `terminal:press!` sends a
normalized key. `terminal:pointer!` accepts a displayed frame address; the
base rejects stale output and view generations.

Accepted process input includes the view's latest grid size and claims
resize control. Subsequent size offers are coalesced per view; observers,
focus reports, painting and scrollback do not take control. Unrecognized keys
that produce no process input cannot claim control. Releasing a
controller invalidates its lease and keeps the last grid.

`terminal:page!` and ordinary wheel scrolling retain the shown position and
leave cursor following. Explicit process mouse capture can consume the wheel
instead. `terminal:follow!` resumes following; process-directed input also
resumes it. The `following` output is connected to the editor's `follow`
input, so repeated process output causes no interaction publication.
After process exit, capture bindings disappear and the same child remains
available for ordinary selection, copying and navigation.

The editor acquires service renditions on its service path, including styles,
grapheme geometry and hyperlinks. It retains a coherent text/rendition pair
across separate publications; the document mirror can advance independently.
Surface-only changes wake the same widget pump. Painting and navigation use
the prepared packet and do not request another rendition.

Terminal clipboard requests and diagnostics are delivered on the service path,
once per shared output source even with multiple views. Clipboard delivery uses
the installed host capability and remains addressed to the controlling head.
The base publishes process state and bell activity; head adapters choose glyphs.

## Model evaluation environments

`environment:create!` creates a base model from a portable recipe and an explicit
`transient` or `persistent` recovery policy. It starts no process. For example:

```scheme
(environment:create! head:ui-actor
  '((directory . "/home/me/project")
    (roots "/home/me/src")
    (imports (chezscheme) (prefix (service resource) resource:))
    (values (seed . 10))
    (resources (document buffer 42)))
  'persistent)
```

The recipe declares imports, absolute library roots and working directory,
copied initial values, and named borrowed buffer/model references. Ordinary
Scheme import modifiers resolve conflicts. Editor implementation libraries
are excluded from recipes except the resource bridge. This separates namespace
and heap lifetime; it is not a security sandbox for hostile Scheme.

Read the environment with `model:snapshots`. Its value holds the recipe,
generation, status, catalogue revision/count and any reset notice.
`environment:evaluate! actor environment generation source` returns a job model
immediately. Jobs in one environment execute in submission order; different
environments execute independently. Explicitly sharing the environment ID
shares definitions and ordering. Opening or splitting a view starts no worker.

`environment:for-document! actor document-id recipe` obtains one persistent
environment associated with a document, shared across heads. A changed recipe
resets its generation and native bindings while retaining completed jobs.
Other documents have independent environments.

An optional fifth argument to `environment:evaluate!` is a Scheme expression
evaluating to a procedure that receives the final values list inside the worker.
Its returned values become the job result. For example, a worksheet can pass
`'render-result-comment` to its imported formatter. The source runs once, at
top level; formatting happens afterward, so even native objects can be reduced
to portable comment text. False preserves the ordinary result.

The job contains its source, generation, status, structured diagnostic and
result. `output` references ordinary read-only base text. `channels` records
reverse-chronological `(channel row character)` run starts; channels are
`stdout`, `stderr` and `compile`, in capture arrival order. Output is streamed in
bounded chunks with pipe backpressure, without embedding a growing transcript
in each job update. A result is `(value values preview)` or
`(handle generation number preview)`; previews are bounded. A reset changes a
retained handle to `(expired preview)`. A general live-object browser is not
provided yet.

`history:create!` and `history:append!` retain ordered references to text, jobs
and explicit `(kind schema data)` presentation recipes. `history-view:create!`
hosts a bounded page of independent child views; its second argument is the
number of visible items (1–16). Previous/Next and M-p/M-n change its logical
item anchor. Text and result presentations are built in; an extension can
register another recipe with `history-view:register!`. Missing definitions
show inert markers until registered. Recipes are never evaluated as Scheme.

The current page's child interaction survives hiding or recovery. Moving an
item off the page retires its disposable presentation; showing it again creates
a new view over the same retained source. It never releases a borrowed job or
environment. `history-view:projection` returns an item's explicitly supplied
text for copy/export, or an unavailable marker; widget data never enters file
bytes or text undo. `examples/history.e` constructs text, an evaluation result
and a connected table/preview entirely at runtime, with no inner window.

Within a worker, `resource:read` returns a declared resource's `(revision value)`.
`resource:edit!` edits declared text using an exact revision and a logical span;
`resource:commit!` updates a declared data model using its revision. Effects
are admitted in the base under the submitting actor and namespace generation.
Read-only documents and service-owned models retain their ordinary protections.

`environment:cancel!` removes a queued job without changing definitions.
Cancelling a running job kills and reaps that environment's worker and resets
its generation; queued jobs are marked reset too. Committed external effects
remain. `environment:reset!` explicitly does the same namespace reset.
`environment:release!` drops a completed job, its output and retained result.
`environment:close!` releases an environment and all its owned jobs/output;
declared borrowed resources survive.

Detaching a head leaves accepted jobs running in the base. Restart restores
persistent recipes, output and portable results, reports that bindings were
reset, and lazily starts fresh workers. History is never replayed. Completion
uses the last completed symbol catalogue: `environment:completion` returns
pages of at most 256 names at an explicit generation/catalogue basis. Fetch
pages outside painting and filter cached names locally while typing.

`eval:create-model-prompt! environment generation draft commands` composes
multiline Scheme entry, completion and help over a borrowed draft. Its accepted
outcome is `(draft-revision lines origin)`, with explicit environment/generation
in `origin`; the host submits those forms through `environment:evaluate!`.
Resetting the environment invalidates the old prompt. This is ordinary Scheme:
variables and nested calls are unrestricted, and head commands do not leak into
the model namespace's completions. The catalogue becomes available after the
first evaluation initializes the worker; submitting empty source also initializes
it. Additional prompts share cached pages, and typing performs no catalogue RPC.

`eval:create-result-view! actor job` composes a shared output editor with a
bounded result or diagnostic summary. It borrows the job and its output. Load
[the environment example](../examples/environments.e) and run
`(environment-example:open!)` for two panels sharing a namespace and a third
independent panel. This demonstrates the pieces rather than a full worksheet
history application. Its prompts are transient; reopening a composition after
restart creates new prompts against the restored environment generation.

A widget definition can explicitly expose its model source to M-x with
`(source-receiver . label)`. Commands annotate it with
`(receiver id (model environment))`, for example. Capture retains the source
identity and hosting view lease, plus the model's `generation` when present.
No arbitrary model traversal or implicit current-model alias is involved.
Environment controls and result controls use this same receiver mechanism.

Asynchronous completion sources may supply a local `basis` procedure and an
idempotent `release` procedure as the fourth/fifth arguments to
`completion:make-source`. A changed basis refreshes visible choices and fences
old selections even when the draft has not changed. Cleanup releases shared
catalogue demand; painting reads only prepared completion data.

## Prompt completion presentations

`prompt:register-presentation! type factory` maps an exact edoc argument type
to a module-owned head factory. Compound type data is matched exactly too;
there is no inheritance or ranking between factories. Conflicting owners
refuse registration. Unregistered types use the generic presentation, and
reload/removal replaces the affected child through the normal widget lifetime.

The factory receives `(request context commands)` and returns an unmounted
view or composition. `context` contains the captured origin, prepared argument
context and completion snapshot. The `select` target accepts the displayed
generation and insertion text; `cancel` cancels the prompt. For example:

```scheme
(prompt:register-presentation! '(one-of red green blue)
  (lambda (request context commands)
    (prompt:create-choices! request commands 'table request)))
```

`prompt:create-choices!` supports `columns` and `table` using the same provider
candidates. Its final argument is its lifetime owner: use the request for a
standalone choice view or a containing view for a composed child. File,
buffer and terminal-capture arguments have table presentations. Empty hint
columns are omitted. Selecting a file inserts its literal; it does not open it.

`prompt:completion-context request` reads prepared head data without querying
the provider. A completion source may provide a sixth `context` callback to
`completion:make-source`; it receives text and caret and returns an alist with
an exact `type` and any explicit argument context its presentation needs.
Factories do not perform matching. Normalization, generation fencing and
literal insertion remain with the existing completion session. Typing within
one type reuses its composition; painting makes no completion requests.

A presentation may register a `complete` action taking `backwards?`. Returning
true handles Tab; false allows ordinary normalization. The needle presentation
uses this to navigate a private read-only editor through `search-control:`.
Its query has no redundant input draft; the typed needle input carries text.
First-hit annotations arrive before the optional cancellable count. Closing
the prompt releases its views and search demand without restoring or modifying
the original editor.

Revision and conflict arguments use the same choice control beside a read-only
editor. `change-preview:create!` creates a base query owned by the containing
view; that view declares it in `owned`. Its `selection` input is false,
`(revision number)` or `(conflict number)`, and its `annotations` output is a
revision-bound batch containing at most one span. It borrows the document
instead of copying text or creating a review draft. Selection and document
version fence worker publication, including settlement that changes conflict
state without changing text. Retiring the owner removes its query and views.

Completion candidates may carry typed value context as their fifth argument.
The session retains a selected literal after automatic closing parentheses,
until input, caret or provider basis changes. A different current argument
type takes precedence. This is data for presentation; it never evaluates a
variable, nested call or candidate callback. Legacy edoc `search`/`preview`
clauses and completion preview thunks are removed.
