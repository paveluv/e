# Apps

Apps are widget compositions: base models own their data and logical state,
and heads render and route input through explicit views. The default window
host presents an app under a name such as `<finder>` or `<buffet>`. Splitting
a window forks its views while sharing the source; selection, scrolling and
geometry stay independent. Widgets can also be nested without a window.

Use the [widget API](WIDGETS.md) to build extensions. Define named actions,
bind keys and pointer targets to those actions, and connect children through
typed ports and explicit command targets. `C-x TAB` opens Bindings to inspect
the active keymaps, pointer bindings and forwarding chains. Those same public
commands can be called from M-x or scripts with an explicit view receiver.

`window-control:open-app!` places a named composition in an explicit window.
Its builder receives the window lifetime owner and `open`/`return` command
bindings, and returns a fresh unmounted app. Reopening reuses its retained
state; splitting forks views while sharing sources. Custom compositions
provide their own hosts. Widgets render without replacing a local buffer:
source documents use `store:`, bounded rows use `collection:`, and editor
views provide text presentation and selection.

Examples in `examples/widgets.e`, `examples/environments.e` and
`examples/history.e` demonstrate connected controls, isolated evaluation,
and dynamically created interactive content. Load an example in a head and
call its documented entry point; no private event loop is required.

## Interaction and built-in apps

Input follows the focused widget's recursive keymap and capture chain.
Pointer events target the hit widget within its clipped geometry. Wheel
input scrolls the viewport under the pointer while preserving keyboard focus.
Clickable text uses bold, muted dotted underlining on hover. A table's active
keyboard choice uses bold text and a subtle blue background; pointer hover
takes precedence without emphasizing an unfocused keyboard choice.

- [Finder](FINDER.md) provides recursive path filtering, navigation, creation
  and sortable metadata. Filesystem work belongs to base services.
- [Buffet](BUFFERS.md#the-buffet-app) provides a shared name/path filter and
  compound sorting over documents and explicitly listed views, including itself.
  Each placement keeps its own selection and viewport. Buffer switching
  follows the current sort; wheel input simply scrolls.
- [Terminal](TERMINAL.md) combines a process source with an editor and capture
  parent. C-] and the clickable capture indicator switch full/partial capture;
  process exit leaves a read-only transcript with no capture.
- Markdown, Describe, logs, Git, Bindings and conflict/rewrite review use the
  same source/view protocol. Opening or settling a selection is an explicit
  action, never a side effect of painting or hovering.

## Publishing terminal rendition

The following low-level surface API represents terminal grid rendition.
Ordinary app layout belongs in head widgets, not shared cell vectors.

`surface:` attaches presentation data to a store buffer. The head renders
visible surfaced buffers automatically, keeping their ordinary mode.
The terminal uses this surface API. Describe publishes ordinary
Markdown source and uses independently fitted widget presentations.
The terminal emulator provides an owned
[`emulator-frame`](TERMINAL.md#scheme-api) containing text and complete
surface rows for publishers that need terminal output.

```scheme
(surface:publish! id basis revision changes cursor size)
(surface:snapshot id)
(surface:rows id generation from to)
(surface:withdraw! id basis)
```

Publish row rendition, cursor, and size together. `basis` is the previous
surface generation, or `#f` for no surface; `revision` is the store text
revision. The two return values are `applied` and a generation, or `stale`
and `frame-changed`/`text-changed`. Malformed data raises without changing
the frame. Identical publication keeps the generation and does not notify.

`changes` contains unique `(row styles hyperlinks attributes)` entries.
Styles and hyperlinks are equal-length cell vectors: styles contain
symbols, SGR strings, or `#f`; links contain `(uri id)` or `#f`, with a
nonempty URI string and a string or `#f` id. Attributes are plain acyclic
data. `(row . #f)` drops rendition. Omitted rows retain their metadata at
the same row number: remap them when text moves, and drop rows past the
new text's end. Cell widths may differ from character counts and between
rows. `cursor` is `#f` or `(buffer-row cell-column visible?)`; `size` is
positive `(rows cols)`. Coordinates start at zero.

For nontrivial glyph geometry, supply a `clusters` row attribute:

```scheme
;; Source "界éZ": two cells, a combining cluster, then one cell.
'((clusters (1 . 2) (2 . 1) (1 . 1)))
```

Each positive exact pair is `(character-count . cell-count)`; their totals
must match the source string and cell vectors. Omission declares one
character per cell. Supply geometry from the app's layout. A glyph uses its
leading cell's style/link. The head maps selection, cursor geometry, links,
and mouse hits through these clusters while store positions stay in character
coordinates. Surfaced grids do not soft-wrap in the head; publishers own
reflow. Withdrawal restores the buffer/window wrap preference.

`snapshot` returns `(generation text-revision cursor size)` or `#f`.
`rows` returns entries for `[from,to)`, including `(row . #f)` for plain
rows, or `#f` if the generation is no longer current. Match the text
revision exactly and use one generation for all row ranges; retry after
refusal. Store text and surface updates are separate, so a surface can
lag. Inputs, returned metadata, and events have independent ownership.

The head caches only rows needed around visible viewports and points, plus
sticky rows. Missing or mismatched metadata, unsupported cluster maps, and
unavailable reads fall back to ordinary text for optional decoration. A live
app with `manages-viewport` requires its surface: after initial construction,
the head retains its previous complete text, display and positions until a
matching frame is available. Publish valid rendition to advance that view;
the store's latest text remains readable independently. Clear `alive` or
`manages-viewport` when returning to ordinary text. Surface-only updates wake
the head to retry deferred adoption without forcing a full-screen repaint.
For a custom surface consumer, `render:prepare` acquires a coherent frame
from an explicit document, immutable text, text revision and bounded row
ranges. Editor views do this on their preparation path. `render:header` and
`render:row` return owned header and `(cell-strings styles cell-link-ranges)`
data; plain or unrequested rows return `#f`. Retain a complete prior frame
while the source and rendition revisions disagree.

`(surface:subscribe! id proc)` subscribes to one buffer, or all buffers
when `id` is `#f`, and returns a token for `surface:unsubscribe!`.
Subscriptions follow module registration lifetime. Events are
`(surface id generation revision changed-rows cursor size)`; changed rows
are sorted, `all` means invalidate every cached row, and `()` means only
cursor/size changed. Pending notices merge per subscriber/buffer and keep
the latest header. Callbacks can reenter and run outside state locks;
a publication's receipt may already have been superseded when it returns.
Batch app output into frames before publishing; the seam has no timer.

Withdrawal uses the same generation guard, returns `applied` with a new
generation, and emits `(surface id generation #f all #f #f)`. Repeated
withdrawal returns `applied #f`. It leaves the store text intact. Deleting
the store buffer also retires its surface. Raw reads do not enforce
audience permissions; consumers must apply the store's visibility rules.
