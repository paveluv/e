# Buffers

A buffer is e's in-memory text object. It may visit a file, exist only for the
editing session, or be generated dynamically by an app. Windows display
buffers but do not own them: one buffer may appear in several windows, and
changing its text updates every window that shows it.

The focused buffer is advertised to the containing terminal as
`e: <buffer-name>`, allowing terminal emulators such as GNOME Terminal to show
it in their tab or window title.

The initial `*scratch*` buffer is an ordinary unvisited buffer. Names wrapped in
asterisks conventionally identify generated tools: `*buffers*`, `*log*`,
`*describe*`, `*completions*`, and merge reports. The convention itself does not
determine behavior; registration makes a buffer an app or view, and its owner
may make it read-only.

## Buffer and window state

Text, the optional file name, mode, modified state, read-only state, mark, and
undo/redo history belong to the buffer. Point and scrolling belong to a window,
so two ordinary windows showing the same buffer may be at different places.
Returning to a buffer restores the position remembered by that window.

App cursors are deliberately shared buffer state. Multiple windows showing the
same app therefore mirror its active row; see [App buffers](APPS.md).

Every window has a status line. Its leading state marker is:

| Marker | Meaning |
|---|---|
| `--` | ordinary, unmodified buffer |
| `**` | modified buffer |
| `%%` | read-only buffer |
| `!!` | the visited file changed on disk |
| `[]` | dynamic app or view buffer |

The status line also shows the buffer name, one-based line and column, detected
mode, remaining merge-conflict count, and applicable command hints. When an app
has focus, `>` marks the window it will act on.

## Switching, creating, and killing

| Key | Action |
|---|---|
| `C-x b` | Prompt for a buffer name. Empty input selects the most recently used other buffer; an unknown name creates an unvisited buffer. |
| `C-x C-b` | Focus the nearest visible `*buffers*` window, or open it in a new full-width window at the bottom. |
| `M-Up` / `M-Down` / `M-Left` / `M-Right` | Move focus to the neighboring window in that screen direction. |
| `M-Shift-Up` / `M-Shift-Down` | Switch the current window through all buffers alphabetically, wrapping at either end. |
| `C-x k` | Prompt for a buffer to kill, defaulting to the current buffer. |

Buffer-name completion is available with Tab in the prompts. Killing a modified
buffer requires confirmation. Killing a buffer removes its app registration,
if any, and every window showing it changes to another live buffer. If the last
buffer is killed, e creates a new `*scratch*` buffer.

Read-only buffers reject editing commands without creating an undo entry. The
error is reported in the echo area rather than corrupting generated content.

The alphabetical traversal is stable: merely visiting a buffer does not move it
in that order. `M`-mousewheel performs the same previous/next operation on the
window under the pointer without moving keyboard focus.

`C-x C-c` exits immediately when every buffer is clean. Otherwise its focused
question offers `yes`, `no`, and `view`; `view` opens `*buffers*` and moves focus
there so modified rows can be inspected before deciding.

## File buffers

`C-x C-f` visits a path, `C-x C-s` saves, and `C-x C-w` saves under a new path.
An unnamed buffer asks for a path when first saved. Saving as makes the buffer
visit the chosen file and updates its mode from the new name.

Visited paths are canonicalized. Relative paths, `.` and `..`, and symbolic-link
aliases of one existing file resolve to the same buffer. Visiting an already
open file switches to that buffer rather than making a duplicate, after checking
whether its disk contents changed.

Files round-trip byte-for-byte, including whether they end in a newline. A file
buffer is considered clean when its text is identical to its disk baseline;
timestamps alone do not make it modified.

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

Saving an externally changed file offers `overwrite`, `merge`, or `cancel`.
Merge uses a three-way patience diff. Independent changes combine silently;
collisions become `<<<<<<< buffer`, `=======`, and `>>>>>>> disk` regions.
`M-n` moves to the next conflict, while `M-m` and `M-d` keep the buffer or disk
side. Each resolution is one undo step, and saving waits until all conflicts are
resolved. A read-only `*merge-<name>*` buffer records the merge report.

## Undo, selections, and the kill buffer

Undo and redo history are per buffer. One typed run, pasted block, formatting
operation, replacement, or grouped API edit normally forms one undo entry.
The mark also belongs to the buffer, while point belongs to each ordinary
window.

The kill buffer is global: text killed or copied in one buffer can be yanked in
another. Consecutive kill commands accumulate, so repeated `C-k` followed by
`C-y` reconstructs the complete block.

## Line numbers

`C-x l` toggles line numbers for the current buffer. The setting belongs to the
buffer, so every window displaying it agrees. New and otherwise untoggled
buffers follow the configurable default:

```scheme
(line-numbers #t) ; default is #f
```

The gutter is left of the text (and right of a left-side scrollbar). It expands
to the decimal width of the buffer's largest line number plus one separating
space: a 1,000-line buffer therefore uses four digits and one space. Wrapped
continuation rows leave the number blank. The gutter is display chrome: it is
not part of buffer text, point cannot enter it, and selections cannot include
it. Clicking or dragging there addresses column zero of the corresponding text
line.

## The `*buffers*` app

`C-x C-b` reuses the nearest window already showing the app. If none is
visible, it adds a full-width window at the bottom of the frame, directly above
the echo area, without disturbing the split tree above it. In either case the
command moves focus into `*buffers*` and retains the originating window as its
target.

`*buffers*` is a live, read-only table with these columns:

| Column | Meaning |
|---|---|
| `C` | `.` marks the buffer displayed by the current window. |
| `R` | `%` marks a read-only buffer. |
| `M` | `*` marks a modified buffer. |
| `Buffer` | Buffer name. |
| `Lines` | Current line count. |
| `Mode` | Detected or assigned mode. |
| `File` | Visited path, with the home directory abbreviated as `~`. |

Rows remain alphabetical and update whenever buffers are created, killed,
modified, reread, renamed, or shown, or when their mode, file, read-only state,
or line count changes. The bold header is sticky; modified rows are italic.

The active row uses the `active` face. When another window has focus, the row
for that window's buffer follows it with the lighter `active-shadow` face.
Multiple windows showing `*buffers*` mirror the same active row.
Both faces are configurable through the style DSL described in
[Styles](STYLES.md).

### Keyboard and mouse controls

- Up or `C-p`, Down or `C-n`: move the active row without changing the target.
- Enter: show the active row's buffer in the target window.
- Click a row: show it immediately in the target, without focusing `*buffers*`.
- Wheel over the app: move one row and show it immediately, without changing
  focus.
- Click the app's status line: focus `*buffers*`; the previously focused window
  becomes its target.
- Click or drag its scrollbar: reposition the list and keep its active row in
  the middle of the visible body without activating a target buffer.

Ordinary content clicks focus their window, but `*buffers*` consumes row clicks
to preserve target focus. Status-line clicks always focus their window and
cannot be overridden by an app.

### Target windows

The target is the last window that was current before entering an app. Selecting
a row changes that window, not necessarily the window displaying `*buffers*`.
Its status line begins with `>` while the app has focus.

If a target window is deleted, the target becomes ephemeral: the app remembers
the target buffer, and the next selection creates a new target window. Two
`*buffers*` windows may target one another, which makes it possible to swap
which window contains the app. Full target semantics and the public app API are
documented in [App buffers](APPS.md).

## Scrollbars

Every ordinary buffer shows a one-column vertical scrollbar by default;
`*buffers*` always shows one. The thin `│` is the track and the centered heavy
`┃` is the visible extent. Thumb size reflects the proportion of the buffer
visible in the window, and its position reflects the scrollable range. Sticky
app headers do not count as part of that range.

The scrollbar supports:

- a track click, which centers the thumb on the clicked cell;
- a thumb click, which grabs it without first jumping;
- dragging from the grabbed position, including outside the scrollbar column;
- cursor placement in the middle of the resulting viewport.

In ordinary buffers, clicking the scrollbar focuses its window. In app buffers,
scrollbar interaction preserves the previous focus and does not invoke the
app's row-click action. Mouse clicks and wheel events also settle the echo area.

Configure scrollbars in `config.e`:

```scheme
(scrollbar #t)                 ; default; #f hides ordinary-buffer scrollbars
(scrollbar-position 'right)    ; default; the alternative is 'left
```

An app may force a scrollbar or a side through `set-app-presentation!`.
`*buffers*` forces it on but follows `scrollbar-position`.

Windows with sticky rows or scrollbars use cached repainting rather than native
terminal scrolling so fixed chrome cannot be shifted or lost. Side-by-side
windows can otherwise opt into terminal-native column scrolling with
`(column-native-scroll #t)` after checking support with `(probe-terminal!)`.

## Scrolling, wrapping, and windows

`(scroll-margin 8)` keeps point that many rows away from the top and bottom when
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
point. `(wrap-lines #f)` changes the default, and `C-x t` toggles wrapping for
one window.

Each split has independent point, scrolling, wrapping, and status. Splits form
a tree, so either half may be split again in either direction: `C-x 2` divides
only the current window into a stacked pair, and `C-x 3` divides only it into a
side-by-side pair. Deleting a window with `C-x 0` promotes its complete sibling
subtree; the `[×]` button at the right edge of every status line performs the
same operation with the mouse. Beside it, `[↕]` performs the stacked `C-x 2`
split and `[↔]` performs the side-by-side `C-x 3` split. `C-x 1` retains only
the current window. `C-x o` moves focus. Status lines and column dividers can
be dragged to resize their local split.
`*completions*` is a temporary full-width pop-up above the echo area, not a
member of the split tree, so opening and closing it leaves the layout intact.
`M-Up`, `M-Down`,
`M-Left`, and `M-Right` cast an imaginary ray from the cursor in that direction
and focus the first window it crosses. Thus the cursor's row chooses between
stacked windows beside a tall window, and its column chooses between adjacent
windows above or below it. The destination keeps its own point position.

## Buffer API

The public Scheme API exposes read-only inspection through `current-buffer`,
`buffer-list`, `buffer?`, `buffer-name`, `buffer-file`, `buffer-text`,
`buffer-clean?`, `buffer-modified`, `buffer-read-only`, `buffer-mode-name`,
`buffer-line`, `buffer-line-count`, and `buffer-line-styles`.

`(buffer "name")` looks up a live buffer; buffers print in that reusable form.
`new-buffer`, `fresh-buffer`, `show-buffer!`, `display-buffer!`, `kill-buffer!`,
`buffer-append!`, `set-buffer-mode!`, and `set-buffer-read-only!` provide
controlled mutation and display. `call-with-buffer` temporarily makes another
buffer current, and `call-as-one-edit!` groups mutations into coherent undo
entries. `focus-window-up!`, `focus-window-down!`, `focus-window-left!`, and
`focus-window-right!` expose directional focus to Scheme. App authors should
use `view-replace!` and `view-append!` for generated content. Run
`M-x (describe!!)` for live signatures and registered command documentation.
