# App buffers

An app buffer is a dynamic, read-only buffer that may handle user input. A
view is an app without an input handler: it renders changing state but leaves
keys to the ordinary editor.

Apps look like buffers, participate in the buffer list, may appear in any
window, and carry `[]` in their status line. Local apps refresh while visible;
shared apps publish text, facts, and rendition through the store and surface.

Head apps are local buffers: their generated text, modes, and presentation
facts stay in this head.  They have no store id and do not appear in the
store's buffer list or publish cursor marks.  This includes completions,
buffer and git views, log renderings, and describe's rendered companion.
Describe's private Markdown source and terminal buffers belong to the base:
their text is readable through `store:`.

## Registering an app

```scheme
(head:register-app! key-or-buffer refresh! handle-event!)
```

A string is a stable tool key and its preferred initial buffer label.  If
that label is already used, the local buffer receives a suffix such as
`<2>`.  An ordinary buffer with the same name is preserved.  Renaming
the app changes its label; registering the same key again reuses the
same local buffer and replaces its refresh and input handler.
You can also pass an existing local buffer to attach the app directly
to that identity, as a source's companion view does. Shared buffers
are refused by this head-app API.
`refresh!` takes no arguments and updates the registered buffer with
`head:view-replace!` or `head:view-append!`. `handle-event!` receives one canonical
event string, such as `"UP"`, `"RET"`, `"MOUSE-CLICK"`, or `"WHEEL-UP"`.
For keyboard events it returns true when the app consumed the event; false
lets the normal global key dispatcher handle it. A local app also hears
`"MOUSE-MOVE"` while the pointer moves over its text, with the same position
parameters as a click and the hovered window selected for the call, and
`"MOUSE-LEAVE"` when the pointer moves off it; neither changes focus or
settles the echo area, and shared apps do not receive them. For `"MOUSE-CLICK"`, returning
the symbol `keep-focus` consumes the click but restores keyboard focus to the
previously focused window. Any other result follows the normal rule that
clicking app content focuses the app and places its cursor at the clicked
position. Returning `ignore-click` consumes the click and restores both the
previous focus and the app's previous point. If the handler returns false—or the buffer is a view with no
handler—the press also starts an ordinary text selection, so dragging selects
from the clicked cell even though the buffer is read-only.
Drag and release belong to the window and buffer that accepted the press.
An ignored click arms neither, and an action that opens another buffer cannot
move that new buffer's point when the mouse button is released.
During clicks, drags, releases, and wheel events, `(app-event-buffer-position)`
returns the unclamped zero-based `(row . character-column)` addressed by the
pointer, using that window's presentation when it has one. It may lie beyond
the buffer's last line, allowing an app to ignore
empty viewport space. `(app-event-position)` is a one-based `(x . y)` cell
position within the text viewport, excluding the scrollbar and line-number
gutter. `(app-event-button)` is the raw xterm button code, including motion and
modifier bits. These thread-local parameters are also exported by `head:`;
the command-layer names reference the same context. They are `#f` outside
pointer delivery. `(head:app-event-focus)` is the window that had keyboard
focus when the pointer event began: the app's own window is selected while
its handler runs, so an app acting as a control panel for another window
addresses this one instead. It is `#f` for keyboard events.
Registrations belong to their module and disappear transactionally on unload
or reload like modes, key bindings, and hooks.
The local buffer and its facts remain, ready for the module to register
the same tool key again.  Killing the buffer ends that tool instance;
its next registration creates a new buffer.

`(head:view-replace! buffer lines [facts [placements [presentations]]])` installs a local
rendering and its related state before repaint callbacks can run. `lines`
is a line list or vector; `facts` is an optional alist. `placements` is an
alist whose keys are windows showing this buffer, `mark`, `spot`,
`(top . window)`, or `spot-top`, and whose values are `(row . column)`
positions in the new rendering. Top placements use only the row and reset
wrapped top segments. All positions are clamped into the new text; omitted
ones keep their coordinates. Invalid input changes nothing. Changed facts
invalidate painting even when the text is equal, as for a style change.
When a window has a separate presentation, its point columns are clamped to
that presentation; saved buffer positions and marks address the shared text.
Pass computed positions and presentation facts in the replacement call:
assigning old coordinates after it returns can overwrite a newer refresh
performed by a callback. Workers schedule head changes with
`head:run-on-main!`.

For a local app with text selection disabled, `presentations` can supply an
alist of `(window . lines)` entries. Each window must be live, show this app,
and occur only once. Its lines must have the same count and logical row order
as the shared text, but may fit columns and shorten labels for its own width.
The shared text, window presentations and positions are installed together.
Windows omitted from the list use the shared text; omitting the argument
clears earlier presentations. Detachment, re-registration, switching buffers
and replacing the source also retire old presentations.

`(head:window-lines window)` reads the text displayed in that window, and
`(head:window-rendition window)` supplies its glyph geometry. Use them for
window overlays and hit testing. Ordinary buffers use their existing text
and rendition through the same accessors. Mode stylers receive the displayed
row text. Buffer text queries continue to
read the common rows; changing only a window's formatting does not create
a content revision. The buffers app uses this facility to share filtering
and sorting while fitting each pane independently.

`(head:view-append! buffer lines [drop])` appends a line list and optionally
drops that many old rows from the start. `drop` defaults to zero and must be
an exact integer within the old row count. Windows whose point was at the
old end follow the new end. Other points, marks and saved viewports move with
surviving text; positions in dropped rows move to the start. Appending to an
empty view replaces its placeholder row. Like replacement, this operation
validates first and installs the complete local rendering before repaint.

Use `(head:call-with-display-update thunk)` when a display operation also
switches windows or places the cursor. Nested scopes defer repaint
notification until the complete operation is installed. A repaint callback
may then start a new display operation without having its state overwritten
by the outer one. This batches only repaint notification: it does not defer
arbitrary callbacks, lock the head, or roll back changes on an exception.

## Publishing shared rendition

`surface:` attaches presentation data to a store buffer. The head renders
visible surfaced buffers automatically, keeping their ordinary mode.
Shared apps can also declare input capture and cursor following as described
below. The terminal uses this surface API. Describe publishes ordinary
Markdown source and uses a local companion for its presentation.
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
`(head:buffer-rendition buffer)` returns its opaque prepared frame;
`(head:read-rendition buffer ranges)` reads explicit `[from,to)` ranges
given as `(from . to)` pairs without filling that cache. Both enforce current
visibility and return `#f` when unavailable. `render:header` and `render:row`
return owned header and `(cell-strings styles cell-link-ranges)` data from a
frame; plain/unrequested rows return `#f`.

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

## Input capture and propagation

Shared apps register an `app` actor endpoint and publish its identity in the
buffer's `app` fact. They do not register a local head app. Set related facts
in one `store:set-properties!` batch:

| Fact | Meaning |
| --- | --- |
| `app` | An actor identity, such as `(app example)`. |
| `alive` | Boolean; enables app input and cursor following while true. |
| `capture` | `#f` or `()` for none, `all`, a list of event strings, or `(except "EVENT" ...)`. |
| `status` | A short string or `#f`; remains visible after the app stops. |
| `cursor-style` | `default`, `text`, `block`, `underline`, `bar`, their `blinking-` variants, or `#f`. |
| `sticky-lines` | Nonnegative count of leading rows kept visible. |
| `scrollbar`, `wrap` | The same presentation preferences described below. |
| `manages-viewport` | Boolean; declares a live grid at the transcript's tail. |

The head checks current audience, liveness, capture, and keymap context before
forwarding input through `actor:send!`. The receiver gets owned plain data:

```scheme
(input (head "name") buffer-id "MOUSE-CLICK"
  ((point 8 . 1) (cell 8 . 2) (viewport 3 . 1) (button . 0)
   (size 20 80) (color-scheme . dark) (revision . 12) (generation . 34)))
```

`point` is a zero-based buffer character position; `cell` projects that
position through the displayed surface. `viewport` and `button` are the
pointer parameters above, or `#f` for keys. `size` is the addressed window's
content grid `(rows cols)`, excluding chrome. `color-scheme` is the head's
`dark`, `light`, or unknown `#f` theme. `revision` and `generation`
identify the adopted text and rendition; generation is `#f` without a frame.
A `"PASTE"` event additionally contains `(paste . "text")`. The producer
decides how to handle an input based on an older frame. No reply is needed
to decide capture; an unreachable or failing endpoint declines delivery.

Each window initially follows the shared surface cursor. Editor commands
and mouse navigation pause following; captured input resumes it. Focus and
blur reports preserve the current preference. Escape suppresses following
and gives the cursor back to the editor. Extensions can set the preference
with `(head:follow-app! window boolean)`; `(head:app-following? window)` reports
whether it is active. Following uses the same prepared surface generation
as painting, even when the cursor moves offscreen.

With `manages-viewport` true, the final `rows` text lines of surface size
`(rows cols)` form the live grid, and the published cursor must lie there.
Following windows anchor at that grid and clip around the cursor when smaller. Other apps use ordinary
viewport scrolling. Inspection uses the editor's cursor visibility and shape;
following uses the published ones. The selected app's status also shows
`capturing input` or `escaped` when capture is enabled.

After layout, the focused head sends `(request actor buffer-id resize (rows
cols))` when its size offer changes or focus/presence is refreshed. Repeated
offers coalesce, and every input also carries its window size. The producer
chooses which head controls its one grid; latest-typist ownership belongs in
the producer. Set `alive` and `capture` false together and withdraw the surface
on exit; the text remains an ordinary read-only transcript. Store facts and
surface frames are separate publications, so fact changes do not identify a
surface generation. Routine app operations are recorded in the store audit
log without filling the echo area; explicit app messages still use the usual
presentation path.

App input is layered: an active prompt first, then the focused app, then e's
global bindings, then the ordinary buffer fallback such as self-insertion.
Most apps are partial: their handler consumes only their own controls and
returns false for everything else. Thus `<buffers>` owns navigation and row
activation while `M-x`, window commands, and other global bindings pass
through naturally.

The handler has first refusal on every key the buffer's mode context leaves
unbound: a true result consumes the event, a false one lets it continue
through the keymaps -- the mode context, then the global map.  A key the
context binds, starts a binding with, or names as its escape goes straight
to the keymaps; the handler never sees it.  An app that embeds a complete
interactive environment simply consumes everything it is offered while it
is alive; a shared terminal declares that capture through its store facts.

The way out of such an app is keymap data, not a mode of dispatch.  The
app's mode context names an escape prefix and may bind app-specific sequences:

```scheme
(keymap:set-context-escape! 'terminal "C-]")
(keymap:bind-default! 'terminal "C-] C-]" terminal-literal-escape!)
(keymap:bind-default! 'terminal "C-] C-y" terminal:yank!)
```

Declaring the escape alone makes it wait for the next key; an app need not
add a binding under that prefix.

A sequence starting with the escape that the context does not bind resolves,
minus the prefix, in the global map: `C-] C-x C-f` runs `find-file!!` from
inside a captured terminal.  Multi-key bindings wait for their remaining
keys, commands keep control through their synchronous prompts, and when the
command returns the next key goes to the app's handler again.  The handler
need not know about the escape: the dispatcher consults the context first.
While the escape is in progress `head:escaped-buffer` names the buffer, so a
status hint can say so -- the terminal shows `▶ escaped` -- and the cursor
takes the editor's shape rather than the app's.

The handler is optional. Thus these are equivalent:

```scheme
(head:register-view! "*example*" refresh!)
(head:register-app! "*example*" refresh!)
```

Both display a local `<example>` buffer. The string is its stable tool
key; changing the displayed label keeps that identity. Local renames
retain angle brackets, and duplicate labels become `<example 2>`.

Apps act on the selected window -- their own, when it is selected.  Use
`show-buffer!` to show an app here, or `display-buffer!` to show it without
leaving the current window.

Table-like apps can request shared presentation chrome:

```scheme
(head:set-app-presentation! app-buffer 1 #t 'default 'default)
```

The second argument is the number of sticky leading rows. The third is either
`#t` for a one-column vertical scrollbar on the configured side, `'left` or
`'right` for a fixed side, `'auto` for a bar on the configured side only while
the app's rows overflow the window, or `#f`. The optional fourth argument overrides
soft wrapping with `#t` or `#f`; `default` (and omission) follows the ordinary
window and global setting. A fifth argument selects `block`, `underline`,
`bar`, or the normal `default` cursor; these explicit shapes are steady.
`text` asks for the editor's own shape for editable text, for an app whose
rows are typed into even though the buffer is read-only; a `default` app
shows the read-only bar.

`(head:set-app-cursor-visible! app-buffer #f)` hides the text cursor while
retaining normal keyboard navigation and viewport following. It also accepts
a predicate receiving the window. Viewport ownership is separate:
`head:set-app-manages-viewport!` disables the editor's automatic following
when the app positions its own viewport.

`(head:set-app-selectable! app-buffer #f)` disables text selection and clears
an existing mark. Keyboard commands and mouse gestures cannot activate a mark
while it is disabled. Selection defaults to enabled and is independent of
cursor visibility. Detaching the app restores ordinary text selection.

Sticky rows, scrollbar geometry, cursor placement,
mouse hit-testing, and scrolling are handled together by the head and the
painter and apply to every window showing the app. The scrollbar is a position indicator:
it is painted, not dragged -- the wheel, the keyboard, and clicks in the
text scroll.

`head:set-app-status-position!` accepts a callback receiving the buffer. A
returned zero-based `(row . column)` projects the status position onto source
text. A returned string replaces the usual buffer details with operation
text, while retaining the window number and controls. Temporary prompts use
this for key hints and completion page counts; the buffers app shows its
name and adds keyboard hints only in the focused window. Status text fits
terminal cells, including wide characters, so window controls keep their
positions. `#f` restores the default.

The same bar is off for ordinary buffers by default; `(scrollbar #t)`
enables it there. `(scrollbar-position 'left)` and `(scrollbar-position 'right)`
select the global side, which defaults to the right. An app's explicit side
overrides that position. `<buffers>` uses `auto`: its bar appears on the
global side when the list is taller than its window and disappears when
everything fits.

## Windows

There is no notion of an app's "target window": an app acts on the
selected window, its own included.  A command that shows another buffer
(`show-buffer!`) replaces the app in the window the user is in; one that
wants the app to stay visible shows the buffer elsewhere
(`display-buffer!`).  The window tree is the only source of windows, and
the user's window commands and mouse gestures move between them as usual
while an app is focused.
Each window keeps its own point and viewport, including multiple
windows showing the same app.

## The buffers app

`<buffers>` is the shared implementation of `C-x b` and `C-x C-b`: a live
table with a name/path filter, ordered column sort keys, modification times,
read-only flags, and a candidate preserved by buffer identity. Click headings
or use F1–F6 to cycle sorting. Each window keeps its own candidate and point;
the filter and sort belong to the local app. Its rows fit each window
independently, with sticky filter and
heading rows, elided paths, an automatic scrollbar, a hidden cursor and
disabled text selection. See [Using the buffers app](BUFFERS.md#the-buffers-app)
for the complete keyboard, mouse and cancellation behavior. Wheel input
in an unfocused pane runs the global `M-Shift-Up` / `M-Shift-Down` binding
in the focused window, preserving focus. Its default alphabetical traversal
and wraparound are independent of the table's filter, sorting and hovered
row. Wheel input in the focused app browses rows without opening a buffer.

Status-bar clicks always focus their window; app handlers cannot override
them. `<buffers>` returns `keep-focus` for content clicks because the click's
purpose is to switch a buffer, not to enter the app.

The `active` face marks the document in the focused window. The `candidate`
face marks a keyboard choice in the focused buffers pane; unfocused panes
retain their choices without making those rows bold. Mouse-hovered clickable
text uses the shared `hover` face, bold with a muted gray dotted underline
suited to the theme; in the buffers app it takes precedence over the keyboard candidate in that
window. Headings, completion labels, Git file rows and refresh, hyperlinks,
and status-bar window controls use the same face. These faces can be
customized like any other:

```scheme
(style:set! 'active '((background 31) (foreground white)))
(style:set! 'candidate '(bold (foreground 208)))
(style:set! 'hover '(bold dotted-underline (underline-color 242)))
```

For a clickable app, register a highlighter that calls
`(paint:hover-ranges hit)`. `hit` receives `(window row character-column)`
and returns `(start end ...)` for the clickable label or `#f` for inert text.
Use the same hit test as the click handler. The helper supplies the shared
face and window scope, excludes gutters and status bars, and uses the
current viewport, including wrapping and wide characters. It does not move
point or invoke the action. Keyboard input clears the pointer emphasis until
another mouse report. Hyperlinks receive this feedback automatically.

Refresh failures are logged under the `app` component and shown in the echo
area. An unchanged failure is reported once instead of once per redraw; a
successful refresh clears it so a later failure is reported again.
