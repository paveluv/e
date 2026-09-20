# Buffers

A buffer is e's in-memory text object. It may visit a file, exist only for the
editing session, or be generated dynamically by an app. Windows display
buffers but do not own them: one buffer may appear in several windows, and
changing its text updates every window that shows it.

The focused buffer is advertised to the containing terminal as
`e: <buffer-name>`, allowing terminal emulators such as GNOME Terminal to show
it in their tab or window title.

The initial `*scratch*` buffer is an ordinary shared, unvisited buffer.
Local buffers belong to this head and have names in angle brackets:
`<buffers>`, `<log>`, `<describe>`, `<completions>`, and merge reports.
Shared buffers retain file names or names such as `*scratch*`. Describe's
private `*describe*` source belongs to the base, while its rendered
`<describe>` companion belongs to the head. Shared names
are unique across the store, including buffers hidden from this head;
collisions receive `<2>`, `<3>`, and so on, as in `notes<2>`. Local names
keep their brackets when renamed; duplicate labels become `<name 2>`,
`<name 3>`, and so on. Names do not determine behavior: registration makes
a buffer an app or view, and its owner may make it read-only.

## Buffer and window state

Text, the optional file name, mode, modified state, read-only state, mark, and
undo/redo history belong to the buffer. Point and scrolling belong to a window,
so two ordinary windows showing the same buffer may be at different places.
Returning to a buffer restores the position remembered by that window.

App windows also keep independent points and viewports; see
[App buffers](APPS.md).

Every window has a status line. It begins with the window's number and a thin
vertical line, `1▏`, then the state marker:

| Marker | Meaning |
|---|---|
| `--` | ordinary, unmodified buffer |
| `**` | modified buffer |
| `%%` | read-only buffer |
| `!!` | the visited file changed on disk |
| `[]` | dynamic app or view buffer |

The status line also shows the buffer name, one-based line and column, detected
mode, remaining merge-conflict count, and applicable command hints. Temporary
interactions such as find-file use that space for operation hints and page
counts instead of generated-buffer coordinates.

Window 0 is the pop-up window. It has no rows and no status line until a
completion list or another echo-area pop-up needs it, when it appears above
the echo area for that interaction; it is never split, deleted or focused.
Ordinary windows are numbered from 1. A new window takes the smallest number no
window holds, so a closed window's number goes to the next window created and
the numbers on screen stay small. `(window 1)` names the window numbered 1 in
M-x and in module code, the way `(buffer "name")` names a buffer, and windows
print in that form.

## Switching, creating, and killing

| Key | Action |
|---|---|
| `C-x b` / `C-x C-b` | Open the buffers app. Type to filter; Enter selects the most recently used other buffer or the chosen match. |
| `M-x (edit:new-buffer!!)` | Ask for a name and create an empty unvisited buffer. |
| `M-Up` / `M-Down` / `M-Left` / `M-Right` | Move focus to the neighboring window in that screen direction. |
| `M-Shift-Up` / `M-Shift-Down` | Switch the current window through all buffers alphabetically, wrapping at either end. |
| `C-x k` | Prompt for a buffer to kill, defaulting to the current buffer. |

`C-x C-f` opens the [files app](FILES.md) for directory navigation and recursive
filename filtering, with the same column-sorting controls as buffers.
`M-x (edit:find-file!!)` reads a path [in the window](PROMPTS.md#prompts-in-the-window).
Both buffer-switch shortcuts use the [live table](#the-buffers-app) below.
An unmatched filter stays in the table; it never creates a buffer. Use
`edit:new-buffer!!` for creation. Empty input cancels creation; an existing name
receives a unique suffix.

The kill prompt completes buffer names with Tab. Killing a modified buffer
requires confirmation. If its shared text or facts change while the
question is open, e reviews it again before deleting. A failed deletion reports
the error and keeps the buffer. Killing a buffer removes its app registration,
if any, and every window showing it changes to another live buffer. If the last
buffer is killed, e creates a new `*scratch*` buffer.

Scratch text written by another actor is unsaved work too. Making a buffer
read-only still leaves its unsaved text protected by kill and quit prompts.
Generated tools and views declare that their output can be discarded.

Read-only buffers reject editing commands without creating an undo entry. The
error is reported in the echo area rather than corrupting generated content.

Edits from different actors combine automatically when their ranges do not
overlap. If another edit consumes the text being changed, or the required
revision history is no longer available, e reports `Edit not applied` and
refreshes the buffer. The rejected edit leaves shared text and undo/redo
history intact. Store failures are reported to the caller; the head does not
keep an offline copy that later overwrites other actors' work.
Point and selection endpoints follow accepted edits, including edits that
arrive while a command runs. Each window keeps its own point. If a reset or
missing history prevents tracking a position, e keeps it within the new text.

The alphabetical traversal is stable: merely visiting a buffer does not move it
in that order. `M`-mousewheel performs the same previous/next operation on the
window under the pointer without moving keyboard focus.

`C-x C-c` reviews this head's local unsaved work. It detaches if all are clean; otherwise it
offers `yes`, `no`, and `view`; `view` opens this head's `<buffers>` list and
moves focus there.
If protected work changes during confirmation, e reviews it again before exiting.
Disposable generated output can keep updating without requiring new confirmation.
Shared unsaved text and terminal processes stay in the running daemon.
`M-x (main:shutdown!!)` saves shared text and named views, reviewing only
local drafts, other heads and live work that will end. Shared unsaved text
needs no confirmation. With `(main:shutdown-on-exit #t)`, quitting the last
head uses this shutdown; cancelling keeps the head open.
`e --restart` uses the same save path and also starts a replacement base. IDs,
revisions and modification times survive, while undo history and local draft
text do not; see [restart and recovery](MULTIHEAD.md#restart-and-recovery).

## File buffers

`C-x C-f` opens the files app, `C-x C-s` saves, and `C-x C-w` saves under a
new path. The direct path prompt, `M-x (edit:find-file!!)`, offers the current
file's directory, a terminal's launch directory, or the head's working
directory for other buffers. Clearing the offered path
and typing a relative name still uses that starting directory. Absolute paths
and `~/` select their own location; Tab on `~` extends it to `~/`. A terminal's
default does not track subsequent shell `cd` commands.

Enter on a directory keeps the path open with a trailing `/`; Tab completes
its contents. A read error or missing parent directory leaves the input and
cursor available for correction. A new filename in an existing directory
creates an empty buffer; the file is written only when saved. Up and Down
browse this head's successful visits, most recent first, including revisits
to existing buffers. Each path appears once. That history comes from the
daemon's retained journal and remains available to the same named head when
it reattaches.

An unnamed buffer asks for a path when first saved. Saving as makes the buffer
visit the chosen file and updates its mode from the new name. File facts,
the buffer label and its detected mode publish together. A callback's later
rename, mode choice or file retarget survives the save returning. An ordinary
re-save preserves a manually chosen mode.

Visited paths are canonicalized. Relative paths, `.` and `..`, and symbolic-link
aliases of one existing file resolve to the same buffer. Visiting an already
open file switches to that buffer rather than making a duplicate, after checking
whether its disk contents changed.

Files round-trip byte-for-byte, including whether they end in a newline. A file
buffer is considered clean when its text is identical to its disk baseline;
timestamps alone do not make it modified.
Saving captures the current shared text, including edits that have not yet
appeared in a window. If another edit arrives during the save, that newer
work stays marked unsaved. Undoing back to the saved contents makes the
shared buffer clean again, regardless of which actor requested undo.
If another actor changes the buffer's file or baseline during the write,
or changes its mode while saving under a new path,
e preserves those newer facts. It reports that the destination was written
but saving could not finish, and returns failure; it does not undo the disk
write or run post-save hooks. Review the buffer and destination before retrying.

Writes preserve existing file permissions, including after interruption.
Permission restoration is best-effort; a failed or interrupted write can
still leave partial contents on disk.

### External changes and rereading

Each file buffer remembers the last disk contents it accepted. e checks at the
start of an edit group and before saving. A mere `touch` is ignored because the
contents are compared. A changed file marks the status line with red `!!`.

Reopening an already visited, externally changed file presents:

- `merge`: combine the loaded baseline, buffer text, and current disk text;
- `reread`: replace the buffer from disk, clear modified state and history, and
  adopt the disk copy as the new baseline;
- `cancel`: leave the buffer untouched, like `C-g` or Escape.

The prompt keeps focus until one of its valid keys is pressed. Invalid keyboard
or mouse input flashes only the echo area, without sound.
If the buffer's text or facts change while reread is being reviewed, e cancels
that reread and preserves the newer work and history. Reopen the file to
review again. This applies in every head.

Saving an externally changed file offers `overwrite`, `merge`, or `cancel`.
If the file did not exist when visited and appeared before the first save,
there is no saved merge baseline: the prompt offers `overwrite` or `cancel`.
An explicitly unset baseline follows the same rule; a loaded empty file has
a valid empty baseline and can be merged.
The disk comparison runs after pre-save hooks, so a hook's write is included
in that decision. e rechecks the disk contents after a file prompt. Changes
to those contents or the buffer's file/baseline cancel the choice; run the
command again to review the new state. A timestamp change with identical
contents is accepted. These checks also apply when saving over an existing
file under a new name. A merged buffer is reported as saved only after its
write succeeds; a failed write leaves the merged text available for review.

Merge uses a three-way patience diff. Independent changes combine silently;
collisions become `<<<<<<< buffer`, `=======`, and `>>>>>>> disk` regions.
`M-n` moves to the next conflict, while `M-m` and `M-d` keep the buffer or disk
side. Each resolution is one undo step, and saving waits until all conflicts are
resolved. A read-only `<merge-name>` buffer records the merge report.

## Undo, selections, and the kill buffer

Undo and redo history are per buffer. One typed run, pasted block, formatting
operation, replacement, or grouped API edit normally forms one undo entry.
`C-_` and `(edit:undo!)` undo this head's latest action by default, preserving
other actors' disjoint changes. To make ordinary undo include every actor,
put this in `config.e` or evaluate it with `M-x`:

```scheme
(edit:undo-scope 'all)              ; default: 'mine
```

`(edit:undo! 'mine)` and `(edit:undo! 'all)` override the setting for one call.
`M-x edit:undo-actor!!` opens an actor picker; `(edit:undo-actor! actor)` selects an
actor directly. Each selects that actor's latest live action without
changing the preference. `C-M-_` or `(edit:redo!)` reverses this head's latest
undo, including one that undid another actor's work. A new edit by this
head clears its redo; changing `edit:undo-scope` does not.

Shared undo applies an attributed inverse and retains both the original
author and the requesting head in history. An overlapping edit, a changed
text property, or unavailable history refuses the whole action. Formatting
and disk merges include their final-newline setting in the same transaction;
undo never restores shared text or facts from a head's old snapshot.
Read-only protection applies to every scope. Local buffers keep their own
snapshot history and behave the same under `mine` and `all`.

The mark belongs to the buffer, while point belongs to each window.

The kill buffer is global: text killed or copied in one buffer can be yanked in
another. Consecutive kill commands accumulate, so repeated `C-k` followed by
`C-y` reconstructs the complete block.

Kill-ring updates may also be sent to the host terminal with OSC 52. Thus,
`M-w`, `C-w`, repeated `C-k`, and Scheme calls to `edit:copy-to-kill-buffer!` can
place the exact UTF-8 text in the desktop clipboard without selecting padded
terminal cells. The host terminal retains final control over whether clipboard
writes are permitted. Enable forwarding in `config.e` when desired:

```scheme
(edit:forward-kill-ring-to-system-clipboard #t) ; default is #f
```

OSC 52 can work over SSH: a supporting terminal on the local desktop decodes
the base64 payload and owns the clipboard; e never invokes a graphical
clipboard command on the remote host. Not every terminal supports clipboard
writes; in particular, GNOME Terminal currently ignores them.

## Recent edit attribution

Another head's or agent's new text is briefly tinted in an actor-specific
color. Your own edits and app output, including terminal screens, create no
tints. An edit inside or overlapping a tinted span removes that whole tint;
other spans move with their original text. Typing at either edge keeps your
new text outside the tint. Up to eight recent spans per buffer remain
highlighted; each expires even while idle or answering a prompt. The color is
temporary presentation: edits take effect immediately and attribution/history
does not disappear with the tint.

`(blame:tint-seconds 8)` sets the lifetime in seconds; fractional values work.
Zero prevents new tints, while existing ones keep their deadlines.
`M-x blame:at-point!` reports recent authorship from the retained edit log.

## Line numbers

`C-x l` toggles line numbers for the current buffer. The setting belongs to the
buffer, so every window displaying it agrees. New and otherwise untoggled
buffers follow the configurable default:

```scheme
(head:line-numbers #t) ; default is #f
```

The gutter is left of the text (and right of a left-side scrollbar). It expands
to the decimal width of the buffer's largest line number plus one separating
space: a 1,000-line buffer therefore uses four digits and one space. Wrapped
continuation rows leave the number blank. The gutter is display chrome: it is
not part of buffer text, point cannot enter it, and selections cannot include
it. Clicking or dragging there addresses column zero of the corresponding text
line.

## The `<buffers>` app

`C-x b` and `C-x C-b` show the same app in the current window. The initial
candidate is the most recently used other buffer, so either shortcut followed
by Enter switches back immediately. Repeated quick switches alternate between
the documents; the switcher itself never becomes the default. The table
includes `<buffers>` so every buffer reachable through global switching
also has a row when the filter is clear. Its own row can be filtered,
sorted and opened like the others.

Type a substring to filter by buffer name or file path, ignoring case. The
whole path is searchable, including directories hidden by elision. Pasted
text also filters. The first line shows `Filter: ` followed by the query;
a long query keeps its most recently typed characters visible. Backspace
removes the last character cluster, and C-u clears the filter. The selected
buffer stays selected while it matches;
otherwise the first match becomes the candidate. Empty results show
`No matching buffers`, and Enter leaves the filter available for correction.

Enter opens the candidate. Esc or C-g returns to the document from which
the app was opened, preserving its text and point. Changing window focus
keeps the list and filter available; invoking either shortcut clears the
filter. The filter and sort belong to this head's app. Two windows showing
it share the filter and row order, but each fits its own columns and retains
its own candidate and viewport.

The live, read-only table has these columns:

| Column | Sort key | Meaning |
|---|---|---|
| `Modified` | F1 | Time of the latest content change, in local `HH:MM:SS`, when the buffer has unsaved changes; blank otherwise. |
| `RO` | F2 | Read-only: `%` when ordinary text editing is disabled; blank otherwise. |
| `Buffer` | F3 | Buffer name. |
| `Lines` | F4 | Current line count. |
| `Mode` | F5 | Detected or assigned mode. |
| `File` | F6 | Visited path, with the home directory abbreviated as `~`. |

Click a heading or press its function key to cycle through ascending,
descending, then off. Function keys keep this full left-to-right mapping
even when a narrow pane hides columns. Several columns can be enabled:
the first enabled column is the primary key, followed by the others in
activation order. Superscript priorities follow the column name
directly, before the arrow: `Modified¹↓`, `Lines²↑`. These numbers show sort
priority, independently of the function keys. Changing direction keeps
that priority. Turning a key off removes it and renumbers the others;
enabling it again appends it after them.

Names, modes and full paths sort alphabetically without case distinctions;
line counts sort numerically. Modified sorts by the full timestamp, including
date and nanoseconds, even when the displayed times are identical. Ascending
puts clean buffers first; descending puts unsaved buffers first, newest change
first. RO sorts blank first in ascending order, flagged first in descending
order. Names break remaining ties and provide the default order when every
key is off. The sort keys survive reopening;
C-u clears the filter without changing them. Selection follows buffer identity
across sorting, renaming and live updates.

Content edits, undo, redo and final-newline changes update the modification
time. Saving clears the displayed time when the buffer becomes clean; the
recorded time is retained. Unchanged edits and other metadata changes preserve
it. Every attached head sees the time recorded by the buffer's owner.

The filter and column headings stay visible while rows scroll.
The `header` face uses plain text on a neutral band: light gray text on
medium gray for dark terminals, dark text on pale gray for light terminals.
The band distinguishes headings from the muted gray filter label and the
bold candidate, including when only one match remains. Hovering over a column
makes its label bold with a muted dotted underline where supported, including
any sort indicators. Padding shares the column's click target; gaps between
columns are inactive. Heading hover does not move the buffer selection or
take focus from another window. Theme changes update
automatically when reported by the terminal; use C-l to refresh older
terminals after changing their theme.
Rows fit each window independently and never wrap. Resizing one pane does
not shorten the rows in another pane whose width stays the same. Long paths
keep their tail, with `…` marking
the omitted beginning. Names elide at the end. Narrow panes omit metadata
columns before names and paths, preserving sorted columns in priority order.
Widening a pane restores the columns and fuller labels. Elision never changes
which buffer a row opens.

The blue `active` face marks the document in the focused window, only in
unfocused buffers panes. A focused buffers pane does not mark its own
`<buffers>` row as active; another pane showing the same list still can. The
`candidate` face marks the keyboard choice with bold text and a soft
blue background, pale in light themes and dark navy in dark themes.
Unfocused panes show no candidate emphasis unless the pointer hovers over a row.
A mouse-hovered row uses `candidate-hover`: the same bold text and tint with a
muted dotted underline, taking precedence over the keyboard candidate there.
Enter accepts that row, and arrows continue from it with bold text and tint.
Moving the pointer away restores the focused list's keyboard emphasis. Hover neither
scrolls nor takes focus. Modified rows are italic. These faces are
configurable through [Styles](STYLES.md).

There is no text cursor or text selection; C-Space does not set a mark.
The keyboard candidate still scrolls into view. The status bar shows
`<buffers>` and, only in the focused window, hints for
F1–F6 and C-u as space permits. Window numbers and controls remain available
in every window. Creation, deletion, edits, saves, renames and mode/file
changes appear on redraw.

### Keyboard and mouse controls

- Up / `C-p` / Shift-Tab, Down / `C-n` / Tab: move the candidate row.
- Home / `C-a` / `M-<`, End / `C-e` / `M->`: select the first or last match.
- Page Up / `M-v`, Page Down / `C-v`: move by a page of rows.
- Enter: show the candidate row's buffer in this window, completing the
  switch in place.
- Esc / C-g: return to the invoking document; C-u: clear the filter.
- F1–F6: cycle sorting for Modified, RO, Buffer, Lines, Mode and File.
- Move the pointer over a row: emphasize that candidate without taking focus.
- Click a row: show its buffer in the selected window; focus stays there.
- Click a heading: cycle its sort key ascending, descending, then off.
- Wheel over the focused app: browse rows until Enter or a click opens a buffer.
- Wheel over an unfocused app: run `M-Shift-Up` / `M-Shift-Down` in the focused
  window. By default this switches buffers alphabetically with wraparound,
  independently of the panel's filter, sorting and hovered row. Focus stays put.
- Click the app's status line: focus `<buffers>`.

Kept in another window, the same app is a control panel: a click or wheel tick
switches the focused window without taking focus. Click the panel's status
line to focus it and type a filter. Global `M-Shift-Up` / `M-Shift-Down` and
Meta-wheel keep their alphabetical traversal independently of the table's
filter and sort.
The public app API is documented in [App buffers](APPS.md).

## Scrollbars

Ordinary buffers show no head:scrollbar by default; `(head:scrollbar #t)` in config.e
enables a one-column vertical bar for them, and `<buffers>` shows one while
its rows overflow the window. The thin `│` is the track and the centered heavy
`┃` is the visible extent. Thumb size reflects the proportion of the buffer
visible in the window, and its position reflects the scrollable range. Sticky
app headers do not count as part of that range.

The scrollbar is a position indicator: it is painted, not dragged. The
wheel, the keyboard, and clicks in the text scroll. Clicking the bar of an
ordinary buffer focuses its window; in an app buffer it does nothing, so the
previous focus is preserved and the app's row-click action is not invoked.
Mouse clicks and wheel events also settle the echo area.

Configure scrollbars in `config.e`:

```scheme
(head:scrollbar #f)                 ; default; #t shows ordinary-buffer scrollbars
(head:scrollbar-position 'right)    ; default; the alternative is 'left
```

An app may force a scrollbar, a side, or an automatic bar through
`head:set-app-presentation!`. `<buffers>` uses the automatic bar: it appears
on the `head:scrollbar-position` side only when the list is taller than the
window.

Every frame is a cached repaint -- rows are painted only when their content
changed -- framed in a synchronized update, so fixed chrome never shifts and
scrolling does not flicker. e does not use the terminal's native scrolling.

## Scrolling, wrapping, and windows

`(paint:scroll-margin 8)` keeps point that many rows away from the top and bottom when
the buffer has room. PageUp and PageDown operate on the viewport rather than
point: in the middle they shift its top by exactly one full window body and put
point in the middle of the result. A partial page clamps at the first or last
viewport and still centers point; pressing outward again moves point to the
first or last line. If the whole buffer fits, its viewport stays at the top and
PageUp/PageDown put point at the first/last line. Wrapped screen rows count
individually, while sticky app headers are excluded from page height.

Each vertical mousewheel tick moves the hovered viewport by one eighth of its
height without focusing it. It otherwise behaves like `PageUp` or `PageDown`:
point is centered and movement clamps at the buffer boundaries. Horizontal
wheel ticks move point sideways.

Long lines soft-wrap by default. A continuation row ends in `\`. With wrapping
off, truncated lines end in `$` and the window scrolls horizontally to follow
point. `(paint:wrap-lines #f)` changes the default, and `C-x t` toggles wrapping for
one window.

Wrapping, Up/Down, and paging measure screen cells, keeping the visual column
across wide characters and combining sequences. Selections highlight whole
displayed glyphs; clicking either cell of a wide glyph selects its start.
Clipped glyph fragments display as blanks at pane edges. Tabs and other
control characters occupy one blank cell. Buffer positions and the status
column still count characters.

Each split has independent point, scrolling, wrapping, and status. Splits form
a tree, so either half may be split again in either direction: `C-x 2`
(`edit:split-window-below!`) divides only the current window into a stacked pair,
and `C-x 3` (`edit:split-window-right!`) divides only it into a side-by-side pair;
`edit:split-window-above!` and `edit:split-window-left!` make the same splits with the
new window first, and have no default keys. Deleting a window with `C-x 0`
promotes its complete sibling subtree; the `×` button at the right edge of
every status line performs the
same operation with the mouse. Beside it, `↕` performs the stacked `C-x 2`
split and `↔` performs the side-by-side `C-x 3` split. `C-x 1` retains only
the current window. `C-x o` moves focus. Status lines and column dividers can
be dragged to resize their local split.
The buttons appear as `│↕│↔│×│`. Hovering a symbol makes it bold with a
dotted underline without changing keyboard focus; the separators stay plain.

Divider intersections expose the split hierarchy even when two layouts have
the same four rectangles. A thin vertical stroke through the crossing means
the vertical divider spans the complete layout and drags as one. A thin `┴`
junction—a horizontal stroke connected to the divider above—means the
horizontal divider spans the complete layout and owns that crossing; dragging
it moves the whole horizontal boundary. The shorter perpendicular dividers
resize only their own subtrees. The same `┴` caps a vertical divider where it
meets a status line directly above the echo area; there it is only a visual
termination, and dragging still resizes the vertical split.
`<completions>` appears in the pop-up window, window 0, above the echo area
for the prompt's duration and hides again afterwards; the other windows keep
their buffers, points and viewports. The split tree is the only source of
windows, the pop-up included: it is the root split's second leaf.
`M-Up`, `M-Down`,
`M-Left`, and `M-Right` cast an imaginary ray from the cursor in that direction
and focus the first window it crosses. Thus the cursor's row chooses between
stacked windows beside a tall window, and its column chooses between adjacent
windows above or below it. The destination keeps its own point position.

Window edges are resized by dragging them with the mouse, respecting the
split tree's ownership and minimum sizes.

## Buffer API

The public Scheme API exposes read-only inspection through `head:current-buffer`,
`head:buffers`, `head:buffer?`, `head:buffer-name`, `head:buffer-file`, `edit:buffer-text`,
`edit:buffer-clean?`, `head:buffer-modified`, `head:buffer-modified-at`,
`head:buffer-read-only`, `mode:name-of`,
`head:buffer-line`, `head:buffer-line-count`, and `mode:line-styles`.

`(head:buffer-modified-at b)` returns the last content-change time as an exact
integer of UTC nanoseconds, or `#f` before a change has been recorded.
`(store:property id 'modified-at)` exposes the same owner-maintained fact
for shared buffers. Like `modified`, it cannot be overwritten through the
shared property API. It survives saves; use `head:buffer-modified` to check
whether there are unsaved changes.

`(buffer "name")` looks up a live buffer; buffers print in that reusable form.
`(window n)` looks up the window numbered n, and windows print as `(window n)`.
`head:new-buffer`, `head:new-local-buffer`, `edit:fresh-buffer`, `edit:show-buffer!`,
`edit:display-buffer!`,
`edit:pop-up-or-reuse!`, `edit:kill-buffer!`,
`edit:buffer-append!`, `mode:choose!`, and `edit:set-buffer-read-only!` provide
controlled mutation and display. `edit:call-with-buffer` temporarily makes another
buffer current, and `edit:call-as-one-edit!` groups mutations into coherent undo
entries. `edit:focus-window-up!`, `edit:focus-window-down!`, `edit:focus-window-left!`, and
`edit:focus-window-right!` expose directional focus to Scheme. App authors should
use `head:view-replace!` and `head:view-append!` for generated content. Run
`M-x (describe:show!!)` for live signatures and registered command documentation.

`(head:new-buffer name)` creates a shared buffer with one empty line and
adopts its canonical record into this head's buffer list. Use
`(head:new-buffer name lines facts)` to publish initial text and a fact alist
together, before create subscribers run. Missing `trailing`, `mode-auto`
and `wrap` facts default to `#t`, `#t` and `default`; explicit `#f` values
are preserved. As with other text inputs, the line container is copied and
its strings must be treated as immutable. Fact values are copied.
`(head:new-local-buffer name)` creates a buffer belonging only to this
head, with one empty line and no store id; its caller decides when to add
it to the list. Use `edit:show-buffer!` or `edit:display-buffer!`
to display the result in a window. The same text, mode, and fact accessors
work on either kind. A local buffer's facts and generated text stay in the head and
produce no store notifications; local points and selections are not
published to other actors.

Opening a file publishes its loaded text, disk baseline, path and detected
mode together at revision 0. A create subscriber's later edits, undo history
and mode choices survive opening. A missing file starts empty with its path
and detected mode, and acquires a disk baseline on its first successful save.
Overlapping visits, including a callback reopening the same path, reuse the
same shared buffer and preserve edits made before the first visit returns.

For extension code, `(store:find-file canonical-path)` looks up the shared
buffer's id or returns `#f`. `(store:visit! actor name lines facts)` requires
a canonical `file` fact and returns two values: id and whether it was created.
Lookup and creation happen together; a reused buffer keeps all its existing
state. Prepare disk text and initial facts before calling it.
`(head:visit-file! name lines facts)` uses the same head defaults as
`head:new-buffer` and returns the adopted buffer plus that creation flag.
The ordinary `edit:visit-file!` command also performs the usual disk-change review
when it reuses a buffer. Local file buffers remain local to their head.

`(store:create! actor name lines [facts])` returns a store id. The optional
fact alist publishes atomically with the content, before the create event.
Use `audience` to control which heads adopt it: `all` is the default,
`((head "desk"))` selects one head, and `()` hides it from every head.
For example, an agent can create content for the requesting head with:

```scheme
(store:create! '(agent helper) "*review*" '("Review notes")
  (list (cons 'audience (list head:ui-actor))))
```

Shared names must be nonempty strings. Creation and
`(store:rename! actor id name)` claim the first free name atomically,
including under concurrent calls. Renaming excludes the buffer's own name;
deletion releases it. `store:buffer-name` returns the current name and
`store:find-named` returns its id or `#f`. Returned strings are copies.
`rename!` returns the name accepted at that commit; a subscriber can rename
or delete the buffer before the call returns. For a head record, use
`(edit:set-buffer-name! b name)` or `(head:buffer-name-set! b name)` to commit and
adopt its current name. A failed rename preserves the cached label and
reports the error.

An audience change takes effect before the head's next frame, including
changes made by that head. Hiding moves its windows to visible buffers,
closes dependent local views, and withdraws its managed marks; shared text,
history, and other actors' marks survive. Dropping `audience` restores the
default. `(store:visible? actor id)` tests existence and audience; raw store
reads remain available under their own policy. Audience is routing, not an
access-control boundary. `(head:adopt-store-buffer! id)` returns the current
head record, or `#f` when invisible. After readmission use that record;
retained hidden or superseded records cannot be added or displayed again.

`(head:buffer-point b)` reads point in the selected window when it shows
`b`, otherwise in another window showing it, otherwise from its saved
position. It does not switch windows or run repaint callbacks.
`edit:show-buffer!` on the already displayed buffer preserves the live cursor
and viewport. To compose window changes and cursor placement before repaint
callbacks run, use `(head:call-with-display-update thunk)`; nested calls
produce one notification after all changes. The thunk's return values are
preserved. Exceptions and escapes still notify completed changes; this scope
does not roll them back.

`head:buffer-lines-set!` and `head:store-reset!` accept a line list or vector.
An empty input becomes one empty line. They validate the complete input and
own a new vector; callers must treat the shared line strings as immutable.
Reset is for an explicit baseline or generated view, and clears shared undo.
Ordinary edits use `head:store-edit!`.

Shared head and policy edits, undo and redo check `read-only` inside the store
transaction as well as at the command prompt. Trusted producers can still
update their read-only output. The underlying `store:edit!` and
`store:edit-with-snapshot!` accept optional write access after the edit context;
`store:history-step!` accepts it after scope. Pass `'any` or a list of permitted
buffer names for a client mutation. Omitted access (`#f`) is the trusted
producer path; it is not exposed through session or wire requests.

Local labels share the head's buffer namespace.  A collision receives
`<name 2>`, `<name 3>`, and so on; when a visible shared buffer arrives or is renamed,
the local buffer yields the conflicting label.  `head:add-buffer!`
adds a buffer to the list without displaying it and claims its label.

`(head:tool-buffer key)` returns or creates a local tool buffer under a
stable string key; `(head:find-tool-buffer key)` only looks it up.
Renaming the displayed buffer does not change its tool key.  App
registration and `edit:fresh-buffer` use this same lookup, so a snapshot
tool rebuilds its own buffer and preserves ordinary buffers with a
matching label.  Killing a tool buffer removes that instance.
Names supplied as `name`, `<name>`, or `*name*` get the local label
`<name>`; tool keys retain the exact supplied string, so the key `"*log*"`
identifies the tool displayed as `<log>`.  Use the displayed label with
`buffer` and the key with `head:find-tool-buffer`.

`head:buffer-fact` uses its fallback only for an absent fact; an explicit
`#f` remains `#f`, and store failures propagate. `head:buffer-facts-set!`
accepts an alist and validates the whole batch before either owner changes
any fact. The corresponding store call is `(store:set-properties! actor id
facts [expected [name]])`; the head call takes the same optional arguments
after its fact alist. Both calls return `#t` on acceptance. Optional `expected`
facts are checked by the same owner before publication: `(key . value)`
requires that exact value, while a bare symbol requires that key to be absent.
For example, `'((base . "old\n") stamp)` requires the old baseline and no stamp
fact; an explicit `(stamp . #f)` would not match. Build this list from a coherent
snapshot with `(property:select facts '(file base stamp))`. A mismatch or deleted
buffer returns `#f` without mutation or notifications. Omission or `#f` is
unguarded; an empty list still requires a live buffer. The predicate compares
values, independently of text revisions and undo's property-version checks.
An optional nonempty `name` commits with the facts, using the ordinary name
allocator and local `<name>` convention. Pass `#f` for `expected` to combine
an unconditional fact update and rename. A stale review refuses both; all
accepted fields are installed before any notification. The head reconciles
the current name after subscribers return, preserving their newer choices.
For detection without mutation, `(mode:detect path first-line)` returns a
registered mode record or `#f`. Its `mode:name` can join a larger fact batch;
save uses this to detect outside the store's mutation lock.
`base` is a string or `#f`; `trailing` and `disposable` are booleans.
Shared `modified` is derived and cannot be set or dropped. Generated output
can set `disposable` to `#t`; registered apps and tool buffers do so already.
The local modified flag remains available for private command history.
Shared fact admission and reads copy finite data: pairs, vectors, strings,
bytevectors, and scalar Scheme data. Mutating a supplied value or a read
result cannot change store state; use a fact transaction. Cycles and runtime
objects such as procedures or records are rejected before mutation. Local
facts can hold runtime objects, including a dynamic `read-only` guard;
that procedure returns true to allow an edit. Use ordinary data flags for
shared read-only state.

`(head:buffer-state b)` and `(store:snapshot-state id)` return
`(values text revision facts)` from one current read. The shared form can be
newer than the window's cached text. `(head:store-reset! b lines facts)` and
`(store:reset! actor id lines facts)` install a baseline and related facts
together; facts are optional. An invalid input changes neither text nor facts.
An optional final `(revision fact ...)`, built with `(cons revision facts)`
from that state read, requires the complete reviewed state to still match.
Both reset forms return the accepted revision, or `#f` if the review is stale
or the source has disappeared. A refused reset does not change history,
marks or the head cache. Omitting the review (or passing `#f`) keeps explicit
unconditional replacement. A returned revision describes that reset's commit;
callbacks can already have advanced the head/store beyond it.

For shared text, `(store:snapshot id)` returns immutable lines and their
revision.  `(store:snapshot-since id basis)` also returns a complete list
of `(revision actor delta)` changes since that basis, oldest first, read
atomically with the text.  An empty list means the basis is current;
`#f` means reset or history truncation removed it, or the basis is in the
future.  Rebase positions only through a complete chain.  When the head
must resync without one, it clamps positions into the new text and logs
the lost history.

`(store:snapshot-state id basis)` returns four values: text, revision,
facts, and that same change chain, all from one read. Use this form when a
client needs to adopt both metadata and positions. Omitting the basis (or
passing `#f` in process) keeps the original three-value form.

In the base, `(store:state id basis)` returns `#f` for an absent buffer, or
`(name text revision facts [changes])` from one read. Pass `#f` for no chain,
or a revision to include it. Names and facts are owned copies; text remains
an immutable snapshot. This supplies the daemon's existing `state` request,
so concurrent rename/deletion cannot split the name from the snapshot.

For derived views, `(head:snapshot-since b basis)` returns the same three
values from this head's adopted text, for either a local or shared buffer.
It does not pull a newer store snapshot. Pass the previous content revision,
or `#f` when there is no previous basis. The head retains up to 256 adopted
deltas; reset or expired history returns `#f` for the chain. Local content
revisions in this API, `head:edit-basis`, and `head:buffer-state` are distinct
from `head:buffer-revision`, which counts repaint changes. Run head reads and
mutations on the main pump; workers schedule work with `head:run-on-main!`.

Local views over shared sources can participate in named-screen restoration.
Set their local `resume-kind` fact to a symbol and register
`(head:register-resume! kind capture restore)` in the owning module's `init!`.
`capture` receives the buffer and an alist of placements, returning two values:
a plain descriptor list (or `#f` when unavailable) and projected placements.
`restore` receives that descriptor and the saved placements, returning the
rebuilt buffer (or `#f`) and its current placements. Placement keys are `spot`,
`spot-top`, `mark`, a window number, or `(top . window-number)`; coordinates are
`(row . column)`. Keep keys unchanged and serialize source/query intent only.
`(head:resume-source id revision placements)` returns the current shared buffer
and rebased/clamped source placements. Markdown uses this path for its source
rows while retaining rendered columns separately. The head restores the layout
and applies placements; providers do not select windows or publish shared text.

`(store:set-marks! actor id basis updates drops)` publishes named positions
and regions in one batch. Updates are an alist of names to `(row . column)`
positions or text spans; drops is a list of names. A numeric basis must be
the current text revision. The result is `(values 'applied revision)` or
`(values 'stale current-revision)`; staleness changes no marks. Invalid shapes,
out-of-bounds coordinates, or repeated names are errors before mutation.
Actors, mark names, and position/span values are copied on admission;
`store:mark`/`store:marks` return owned names and coordinates. Names are
finite plain data, commonly symbols or lists identifying windows.
`store:set-mark!` and `store:drop-mark!` keep their immediate current-text
behavior through the same validated boundary. Use the batch with a captured
basis when publishing positions computed from a snapshot. Heads already do
so and retry stale or failed publication without losing pending removals.
A `#f` batch basis explicitly addresses the current text, as the single-mark
helpers do.

`(store:edit! actor id basis span replacement [context])` applies an
attributed edit or returns a stale refusal.  The optional context is
`(group-key label [undo-facts [commit-facts [expected]]])`: the same non-false key groups that
actor's transactions in this buffer into one undo action.  Without a key,
each call is an action.  Properties such as `((trailing . #t))` commit with
the text and are included in its inverse.  Property versions are checked
on undo and redo, so a later write blocks restoration even if it returns
to the same value.  Public property queries omit deleted properties.
Optional commit facts are installed in the same transaction but survive
undo, as a merge's disk baseline should. A key cannot appear in both lists.
Optional expected facts use the same predicate as `set-properties!`. A mismatch
or deleted source returns `(values 'stale 'property-changed)` before text,
facts or history change; ordinary text rebasing still applies. The predicate
does not become part of undo history. Shared inputs must be finite plain data.
Labels and plain key structure are copied at admission and readback.
Grouping retains `equal?` matching. Opaque runtime leaves in an in-process
key retain their original identity; keep their equality stable while the
action is retained. Use plain keys for transportable work.

`store:edit-with-snapshot!` takes the same arguments but returns
`(values 'applied (revision text changes edit-facts))`. This acknowledgement
describes exactly the accepted transaction, even if a subscriber immediately
edits again. `edit-facts` contains the `modified` and `modified-at` pairs
captured under the same store lock. Its complete `(revision actor delta)`
chain starts after the supplied basis and includes this edit; a commit that
trims the oldest retained log entry still returns that entry in its
acknowledgement. Refusals are the same as for `store:edit!`.

Head extensions can capture `(head:edit-basis b)` before computing a
proposal. It contains the immutable source lines, store id (or `#f`), and
revision. Pass it to `(head:store-edit! b span replacement context placements
source)` so a callback advancing the head cannot change the proposal's
basis. The context is the store edit context above. Placements are an alist
whose keys are windows, `mark`, `spot`, `(top . window)`, or `spot-top`, and
whose values are `start`, `end`, or positions in the proposed result. A top
placement uses the row and resets the window's wrapped top segment. The head
projects them into the accepted
revision and follows subsequent edits. Context, placements, and source are
optional, in that order; defaults are `#f`, `()`, and the current head basis.
Use placements to express a command's point movement instead of assigning
coordinates saved before submission after the call returns. Local edits
use the same geometry and refuse a proposal whose source text changed.

`(store:undo! requester id [scope])` defaults to `mine`.  Use `all` to select
the latest live action of any actor, or `(actor who)` to select that actor.
`(store:redo! requester id)` reverses this requester's latest undo, including
one that undid somebody else's work.  Both return `(values 'applied revision)`,
`(values 'blocked reason)`, or `(values 'nothing #f)`.  Reasons include
`overlap`, `property-changed`, and `basis-too-old`.  A group commits entirely or refuses entirely;
undo and redo preserve the revision log.  The retained delta chain and each
group's parts are bounded at 256; unavailable history never permits a
partial group undo.  A new edit invalidates that requester's redo.

`(store:history-step! requester id direction scope)` uses the same transaction
but returns an applied receipt `(revision action-id original-author group-key
label)` for a head's presentation.  Direction is `undo` or `redo`; redo's scope
must be `mine`, meaning that requester's undo history.  `store:undo-authors`
lists actors with retained live actions; the transaction rechecks eligibility.

`store:history` returns newest-first `(revision actor start end new-end)`
rows.  Undo/redo rows append `(direction original-author action-id reversed-revision)`.
Their edit events likewise append this origin to the ordinary
`(edit id revision requester delta)` shape.  This distinguishes the original
author from the requester of the inverse.  `snapshot-since` continues to
return three-field change entries.

Store operations validate and copy `(kind name ...)` actor identities
before mutation. The kind is a symbol; the name is a nonempty string or a
symbol, and any additional metadata is finite plain data. Registration is
independent of attribution. History, blame, incremental changes, author
lists, and receipts own their returned actors and metadata; history/blame
coordinates are copies too. Text vectors, line strings, and delta records
retain the text algebra's immutable sharing contract: do not mutate them
or the coordinates and replacement lists reachable through a delta.

`(store:subscribe! id callback)` observes one shared buffer; use `#f` for
all buffers and `(store:unsubscribe! token)` to revoke its returned token.
Each callback owns its event envelope and metadata; changing those cannot
affect the store or another subscriber. Edit deltas remain immutable shared
values.
Callbacks receive events in commit order outside the store lock, and can
read or edit the store.  A write normally drains notifications before
returning, but a concurrent or nested write returns after committing
while another delivery is active.  Its notification follows later, so a
callback must not wait for a later event's delivery.  New subscriptions
observe future commits; revocation skips queued callbacks, while an
already running callback may finish.  Exceptions do not stop delivery.
Escaping a callback releases delivery ownership and drains queued work;
resuming a continuation into completed delivery is an error.

For readers that catch up from snapshots, `(store:watch! wake)` returns two
values: an ordinary subscription token and a zero-argument `take!` procedure.
`wake` runs outside the locks when pending work first appears. `take!` clears
and returns the pending `(buffer-id . metadata?)` pairs; true means facts or
lifecycle changed, false means only text changed. Taking one reader's batch
does not affect another reader. Subscribe before reading the initial inventory.

Repeated updates to an id coalesce. More than 256 pending ids collapse to
`#f`, requesting a full inventory rescan, including previously adopted ids
that may now be deleted or hidden. An empty list means no pending work.
These are invalidations, not edit history: obtain the complete revision
chain from a snapshot before moving positions. The head uses this path on
its normal pump. Revoke with `store:unsubscribe!` or the registration owner's
usual cleanup.
