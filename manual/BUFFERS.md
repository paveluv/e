# Buffers

A buffer is e's in-memory text object. It may visit a file, exist only for the
editing session, or be generated dynamically by an app. Windows display
buffers but do not own them: one buffer may appear in several windows, and
changing its text updates every window that shows it.

The focused buffer is advertised to the containing terminal as
`e: <buffer-name>`, allowing terminal emulators such as GNOME Terminal to show
it in their tab or window title.

The default composition creates `*scratch*`, an ordinary shared, unvisited
Scheme document. App views use angle-bracket names such as `<buffet>`,
`<finder>` and `<describe>`. These are base-owned views scoped to their
composition, not local text buffers. Shared text may be restricted to a
head's audience without becoming head-owned. Store names are unique, with
`<2>`, `<3>` suffixes for collisions; names never determine behavior or
identity. A custom startup composition need not create scratch or windows.

## Buffer and window state

Text, file metadata, modified/read-only facts and attributed history belong
to the base document. Each editor view owns a logical caret, selection and
viewport anchor, so windows over the same document can remain independent.
Returning to a document restores that window's retained presentation.
App controls own their interaction state and refuse ordinary file Save As.

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

The status line shows the document/app name and, for an editor, its line,
column, mode and conflict count. `C-x TAB` opens Bindings for the applicable
commands. Ordinary windows are numbered from 1, using the smallest free
number. A number is a selector within one manager; `window:numbered` resolves
it to a stable `'(model id)` reference. The auxiliary manager has its own
windows above the message area and is revealed by completion or Describe.

## Switching, creating, and killing

| Key | Action |
|---|---|
| `C-x b` / `C-x C-b` | Open the buffet, the buffers laid out to pick from. Type to filter; Enter selects the most recently used other buffer or the chosen match. |
| `store:create!` followed by `window-control:open-document!` | Create and place an unvisited document programmatically. |
| `M-Up` / `M-Down` / `M-Left` / `M-Right` | Move focus to the neighboring window in that screen direction. |
| `M-Shift-Up` / `M-Shift-Down` | Switch the current window through live buffers in Buffet's compound sort order, ignoring its filter and wrapping at either end. |
| `C-x k` | Kill the current buffer at once; a document goes to the trash. |

`C-x C-f` opens the [finder](FINDER.md) for directory navigation and recursive
filename filtering, with the same column-sorting controls as buffers.
`M-x (screen:open-file! ` completes an explicit screen receiver and path. Both buffer-switch shortcuts use
the [live table](#the-buffet-app) below. An unmatched filter stays in the
table; it never creates a buffer. Use `store:create!` for creation; an
existing name receives a unique suffix.

Killing asks nothing. `M-x (edit:kill-buffer! ` completes the live buffers,
and `C-x k` kills the current one. A shared document goes to the trash
rather than being deleted: it disappears from every head, its text, facts
and undo history stay in the base, and the echo area says whether the work
in it was unsaved. Visiting the file again reads the disk into a fresh buffer
under the plain name; the trashed one stays in the trash, renamed with a
suffix, since every document keeps a distinct name. `M-x (edit:restore! `
completes the trashed names, newest first with how long ago each was killed,
and returns its reference to place explicitly, renamed again with a suffix
while another buffer holds its name, since several buffers may visit one
file. `(edit:trash)` lists them as `(name killed-at actor)`, and
`(edit:empty-trash!)` deletes them for good, the backups below kept.
`(edit:delete-trashed! name)` permanently deletes one named Trash or Backups
entry and its history, leaving the file on disk untouched. It refuses live
buffers and entries changed during deletion. The trash survives a base
restart and empties itself by age, thirty days by
default through `(store:trash-retention days)` in `base-config.e`.
Disposable output is deleted; app views release their owned resources.
Every showing window applies the manager's fallback policy. Borrowed source
documents survive retirement of a presentation.

A save keeps the version it writes over. Before writing onto an existing
file whose text differs, e reads the file into a backup: a trashed buffer
named after the file with `.bak`, carrying the file's path, its
modification time and a checksum of its text, the habit of copying a file
to `file.bak` before changing it made e's own. The buffet lists the
backups in their own section, `M-x (edit:restore! ` completes their names
beside the trash's and brings one back as a buffer to read, copy from or
save over the file, and `(edit:backups)` lists them as `(name path observed
stamp checksum actor)`, newest first. A version the backups already hold is
not kept twice; a file keeps ten versions, the oldest dropped as a new one
arrives, `(store:backups-kept n)` in `base-config.e` sets how many, and
backups expire with the trash's retention.

Scratch text written by another actor is unsaved work too, and goes to the
trash with its buffer. Generated tools and views declare that their output
can be discarded.

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

With no explicit sort, traversal is alphabetical: merely visiting a buffer does
not move it in that order. `M`-mousewheel performs the same previous/next operation on the
window under the pointer without moving keyboard focus.

`C-x C-c` detaches this head: documents and its named composition stay in
the base. The console prints a single status line after restoring the terminal.
`M-x (lifecycle:shutdown!)` saves documents and persistent views, reviewing
other heads and live work that will end. Unsaved text
needs no confirmation. With `(lifecycle:shutdown-on-exit #t)`, quitting the last
head uses this shutdown; cancelling keeps the head open.
`e --restart` uses the same save path and also starts a replacement base. IDs,
revisions, modification times, retained undo history and reload conflicts
survive, including private documents; see [restart and recovery](MULTIHEAD.md#restart-and-recovery).

## File buffers

`C-x C-f` opens [Finder](FINDER.md), `C-x C-s` saves, and `C-x C-w` saves
under a new path. Finder offers missing path components as italic `[create]`
rows. Choose one to create and open it, including missing parent directories.

The same command is available directly as `(edit:visit-file! path)` in M-x,
with path completion. A new file is created on disk immediately, before its
empty buffer opens. A trailing `/` creates directories only and opens Finder
there; an existing directory also opens Finder. Every newly created path is
logged. Existing files and shared buffers are reused without overwriting
their contents; a concurrent creation is opened as an existing file.

An unnamed buffer needs an explicit destination, supplied with `C-x C-w` or
`(edit:save-file! editor path)`. Saving as makes the buffer
visit the chosen file and updates its mode from the new name. File facts,
the buffer label and its detected mode publish together. A callback's later
rename, mode choice or file retarget survives the save returning. An ordinary
re-save preserves a manually chosen mode.
Active app buffers, such as Finder, refuse saving: their text and mode belong
to the app. Copy any text you want to save into an ordinary buffer first.

`(edit:save! editor)`, `(edit:save-file! editor path)`,
`(edit:reload! editor)` and `(edit:reread! editor)` address an explicit editor
view. They work while another pane is focused. Save hooks receive that same
view; changing or closing it before the write refuses the operation.

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

### External changes and reloading

Each file buffer remembers the last disk contents it accepted, its baseline.
Nothing asks about a file changed on disk; e **reloads** it as one undoable
action, preserving your earlier undo history. A reload happens when
you reopen the file, when you first edit a buffer whose file changed
meanwhile, when you save one, and on `(edit:reload! editor)`. A mere `touch` is ignored
because the contents are compared.

Undo restores the text, final newline and pending conflicts from before the
reload; keep undoing to reach your earlier edits. Redo reapplies the reload.
The observed disk baseline stays remembered, so you can undo the reload
and save your restored version over it, provided the file has not changed
again. Reload belongs to the head that requested it, including automatic
reload, and follows the usual undo scope.

A reload makes the disk's text the baseline again and reapplies the
buffer's own entries since the old baseline on top, carried across the
disk's changes with the geometry that carries concurrent editors' edits
across each other. The disk's changes are inferred by a diff of lines, each
changed stretch then refined token by token, a word, a run of blanks, a
punctuation character or a line break being the unit, so a reindent, a
formatter's line split or a rename on the rest of a line combines with your
edits there, and a word both actors changed conflicts as a word. Changes
that touch different tokens combine silently, and the same change made by
both stands once. Where the two texts meet at one point, what would fuse
into one word conflicts instead, as does a line opened where the disk
joined two lines, and text appended to a line or a word the disk deleted;
typing at the end of the line above an added or deleted line stays on its
line. A reload that merges everything leaves a line in the log and nothing
else. The first edit after an external change applies to the text as you
saw it, and then the buffer reloads and merges, your edit one side and the
disk's the other, so an insertion where the disk inserted conflicts rather
than landing elsewhere.
Every head's point and marks cross the reload on their text: the disk's
changes, carried over the buffer's, take them from where they were to
where that text now stands, so a line added above the cursor moves it
down rather than leaving it on the wrong line. A reread, its undo and a
reset carry them the same way, on a line diff of the two texts; only a
position inside a line that changed whole moves to the end of its
replacement.

An entry the disk's change overlaps is disabled and pends as a
**conflict**: the disk's side stands in the text, which stays consistent at
every moment, and the entry's side is kept. The entries of its batch whose
text adjoins it join the conflict, so a replacement typed as a backspace and
a character conflicts whole, while the occurrences of one `replace!` pend
one by one. Both sides are the two images of one region of the text before
either change, so keeping either gives a clean text, and a conflict whose
sides agree settles itself. While conflicts pend the buffer's status line and
its `<buffet>` row show red `!!`, the buffer stays editable, and a save
refuses: resolve the conflicts first.

You can keep editing while conflicts are pending. Disk then means the text
currently in that region, including your further edits. Undo restores the
conflict alternatives as well as the text. If another disk change would
overwrite further edits to an unresolved region, reload leaves the buffer
and its history intact and asks you to resolve the pending conflicts first.

`C-x !` opens the **conflict review**, `<conflicts>`, in the invoking window;
`(delta-log:conflicts! window)` takes an explicit window model. Clicking
the red `!!` opens it too. Its initial scope captures the visible shared
documents, with the invoking document first. Each row names the document,
revision, actor, position and both alternatives. Navigating other windows
does not silently change the scope of an existing review.

`LEFT` picks Mine and `RIGHT` picks Disk; clicking those cells does the same.
`RET` and `SPC` flip a row's choice. `S-LEFT` and `S-RIGHT`, or the
`Mine (all)` and `Disk (all)` controls, choose throughout the review.
Overlapping Mine alternatives cannot all be selected together; a bulk choice
refuses atomically, while an individual Mine choice displaces overlapping
choices. The connected read-only preview below the table follows the selected
document and highlights its reviewed alternatives. It never replaces another
window's source buffer. Wheel scrolling uses the ordinary widget viewport.

Click **Settle** or press `M-RET` to apply the choices. Selection and preview
commands never settle. Each document settles atomically and is undoable;
changed alternatives or read-only sources refuse without losing other drafts.
Unrelated source edits survive. A stale table cannot silently apply unseen
alternatives. Changes elsewhere refresh the demanded review asynchronously.
`ESC` returns to the host's previous document; the retained review keeps its
choices for reopening. New reviews have independent drafts.

At M-x, `(store:conflicts document)` lists a document's alternatives and
`(delta-log:resolve! document n 'mine)` settles one explicitly; replacement
lines may be supplied instead. For scripted or embedded reviews, use
`delta-log:create!` with an explicit document scope and host commands.
`conflict-review:create!`, `choose!`, `preview` and `settle!` expose the base
draft independently of any head. `delta-log:choose!`, `choose-all!` and
`settle!` operate the widget through the same guarded paths as its controls.
`C-x TAB` shows their forwarding chains.

`C-x C-r` **rereads** the file instead: the disk's text replaces the
buffer's as one undoable edit, settling every pending conflict, so the red
`!!` goes at once; undo brings the text and the conflicts back. A conflict
settled by keeping mine comes back with its undo the same way. Where the
store cannot merge the disk's changes at all, a buffer without a saved
baseline or one the log no longer reaches, e rereads the disk by itself and
says so in the echo; undo brings the buffer's text back. A save that finds
such a file rereads it too and waits: undo, then save writes your text.

Saving an externally changed file reloads it first and writes when no
conflict pends. The disk comparison runs after pre-save hooks, so a hook's
write is included in that decision, and a timestamp change with identical
contents is accepted. Saving under a new name onto an existing file asks
nothing either: what the file held is backed up first, as a trashed buffer
named after the file with `.bak`, the echo names it, and `restore!` brings
it back.

## Undo, selections, and the copy buffer

Undo and redo history are per buffer. One typed run, pasted block, formatting
operation, replacement, or grouped API edit normally forms one undo entry. A
run is up to twenty keys of typing, backspaces and forward deletes without
moving point, so a typo and its correction undo together and share one batch
in the delta log.
`C-_` and `(edit:undo! editor)` undo this head's latest action by default, preserving
other actors' disjoint changes. To make ordinary undo include every actor,
put this in `config.e` or evaluate it with `M-x`:

```scheme
(edit:undo-scope 'all)              ; default: 'mine
```

`(parameterize ([edit:undo-scope 'all]) (edit:undo! editor))` overrides the setting for one call.
`(edit:undo-actor! actor editor)` selects that actor's latest live action
in an explicit editor without changing the preference. Tab offers the actor
and the applicable editor receiver. `C-M-_` or `(edit:redo! editor)` reverses this head's latest
undo, including one that undid another actor's work. A new edit by this
head clears its redo; changing `edit:undo-scope` does not.

Shared undo applies an attributed inverse and retains both the original
author and the requesting head in history. An overlapping edit, a changed
text property, or unavailable history refuses the whole action. Formatting
and disk merges include their final-newline setting in the same transaction;
undo never restores shared text or facts from a head's old snapshot.
Read-only protection applies to every scope. Local tool buffers are read-only
projections; undo belongs to their shared source buffers.

The mark belongs to the buffer, while point belongs to each window.

The copy buffer is a buffer of its own, `*copy*`, a shared buffer of the base
in this head's audience alone, so every head has its own and shows it as
`[copy]`, created by the first kill or copy. Text killed or copied in one
buffer can be pasted with `C-y` in another. Consecutive kill commands
accumulate, so repeated `C-k` followed by `C-y` reconstructs the complete
block. Show `[copy]` in a window
to watch copies arrive, edit it before pasting, or undo in it: every copy is
one entry of its delta log, and `C-_` brings the previous one back, as far as
the log's retention reaches. It is disposable: killing it asks nothing, it
does not outlive the base, and the next copy recreates it. A second head on
the same base gets its own, shown as `[copy]` there as well.
`edit:copy-text` returns its text. `(clipboard:open! #f)` returns this actor's
document reference, or `#f` before the first copy; pass `#t` to create it.
Copies use the ordinary text journal without a head-local buffer mirror.

Copy buffer updates may also be sent to the host terminal with OSC 52. Thus,
`M-w`, `C-w`, repeated `C-k`, Scheme calls to `edit:copy-text!`, and any other
change to `[copy]`, such as editing it by hand, can place the exact UTF-8 text
in the desktop clipboard without selecting padded terminal cells. The host
terminal retains final control over whether clipboard writes are permitted.
Forwarding processes coalesced document changes between commands; it does not
poll the copy buffer while painting. Restoring a head does not export its
saved copy until that text changes.
Enable forwarding in `config.e` when desired:

```scheme
(edit:forward-copy-buffer-to-system-clipboard #t) ; default is #f
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
Zero clears existing tints and prevents new ones. Tint preparation reads only
the editor's acquired source; painting and expiry make no wire requests.
`M-x (blame:at-point! ` selects an editor receiver and reports recent
authorship from the retained edit log. `(blame:describe document position)`
returns the same text for an extension's own label or pop-up.

## The delta log

The retained delta log records edits, actors, grouping labels and inverse
relationships. `(delta-log:log document)` returns a document's entries,
newest first, as `(revision actor labels delta origin state)` records.
An optional selector narrows them by `count`, `actor`, `batch`, `since`,
`until` or `state`, for example `(delta-log:log document '((count . 10)))`.
A quoted batch value selects that batch. `(delta-log:show! editor n)`
describes one retained entry in the journal. Revision and batch arguments
complete from the editor captured when the prompt opened; prompt focus does
not change that source. M-x offers the captured editor as the receiver.

`C-x C-l` opens `<delta-log>` in the invoking window; `(delta-log:open! window)`
chooses an explicit window model. The browser shows one document's retained history,
newest first, above an independent read-only rewrite preview. `RET` or `SPC`
toggles whether the selected revision is kept in the preview. The Preview
column marks `keep` or `omit`. Later edits are rebased over omitted entries;
overlapping later edits block settlement. `Settle` or `M-RET` commits the
rewrite as an undoable action. `ESC` returns to the previous document,
retaining the draft. The original remains editable throughout review.

`(delta-log:filter! review selector)` narrows an explicit rewrite browser by
the same selector or a batch literal; `#f` shows all history again. Filtering
does not discard choices. Navigation, scrolling, columns and pointer handling
use the ordinary table widget. Sources and row generations fence each action.

`(rewrite:create! actor document)` creates an independent base draft.
`rewrite:toggle!` and `rewrite:settle!` take its ID and expected revision;
`rewrite:preview` returns choices, derived text, mapping, conflicts and source
revision. `rewrite:close!` abandons it without changing the source. For a
composition, `(delta-log:create! owner commands 'rewrite (list document))` creates
an unmounted table/preview over a fresh draft, usable without an editor window.

## Line numbers

`C-x l` (`window-control:toggle-display! window 'line-numbers`) toggles line numbers in the current
window, and `window:set-display!` sets an explicit manager/window preference to `#t`,
`#f` or `default`. The setting belongs to the window and applies while it shows an
edit buffer; an app's buffer shows itself as the app decides. Untoggled
windows follow the configurable default:

```scheme
(window-control:line-numbers #t) ; default is #f
```

The gutter is left of the text (and right of a left-side scrollbar). It expands
to the decimal width of the buffer's largest line number plus one separating
space: a 1,000-line buffer therefore uses four digits and one space. Wrapped
continuation rows leave the number blank. The gutter is display chrome: it is
not part of buffer text, point cannot enter it, and selections cannot include
it. Clicking or dragging there addresses column zero of the corresponding text
line.

## The `<buffet>` app

`C-x b` and `C-x C-b` show the same app in the current window. The initial
candidate is the most recently used other buffer, so either shortcut followed
by Enter switches back immediately. Repeated quick switches alternate between
the documents; the switcher itself never becomes the default. The table
includes `<buffet>` so every buffer reachable through global switching
also has a row when the filter is clear. Its own row can be filtered,
sorted and opened like the others. `M-x (buffet:open! window)` is the same
command.

Below the live rows, while saves have kept anything, a `Backups` section
lists the versions they wrote over, dimmed, with how long ago each was read
and the file it came from; the filter applies to their names and paths.
Below that, while the trash holds anything, a `Trash` section lists the
killed shared buffers, with how long ago each was killed and how long it
stays before the base deletes it; the filter applies to their names. Enter
on a row of either restores it, as `(edit:restore! name)` does.

`C-k` kills the chosen live buffer using the ordinary buffer-kill behavior:
shared documents move to Trash, disposable output is deleted, and local apps
close. Windows showing it switch to another buffer. The next row becomes the
candidate, or the preceding row at the end. `C-x D` (Control-X, then Shift-D)
permanently deletes the chosen Trash or Backups entry; it refuses live rows.

Type a substring to filter by buffer name or file path, ignoring case. The
whole path is searchable, including directories hidden by elision. Pasted
text also filters. The first line shows `Filter: ` followed by an editable
single-line entry; long text follows its caret. Left/Right move within it,
Backspace removes a character cluster, and C-u clears it. The selected
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
| `Flags` | F2 | `!!` for unsettled reload conflicts, `%` when ordinary text editing is guarded; both can appear together. |
| `Buffer` | F3 | Buffer name. |
| `Lines` | F4 | Current document line count; blank for widget apps, whose generated rows are not document text. |
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
first. Flags sorts lexicographically by the complete marker text: blank,
`!!`, `!! %`, then `%` in ascending order. Names break remaining ties and
provide the default order when every key is off. The sort keys survive reopening;
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
`<buffet>` row as active; another pane showing the same list still can. The
`candidate` face marks the keyboard choice with bold text and a soft
blue background, pale in light themes and dark navy in dark themes.
Unfocused panes show no candidate emphasis unless the pointer hovers over a row.
A mouse-hovered row uses `candidate-hover`: the same bold text and tint with a
muted dotted underline, taking precedence over the keyboard candidate there.
Enter accepts that row, and arrows continue from it with bold text and tint.
Moving the pointer away restores the focused list's keyboard emphasis. Hover neither
scrolls nor takes focus. Modified rows are italic. These faces are
configurable through [Styles](STYLES.md).

Table rows have no text cursor or mark. The filter is an ordinary entry with
a caret, text selection and undo. The keyboard candidate scrolls into view.
The status bar keeps the name `<buffet>`; `C-x TAB` lists the keys. Window numbers and
controls remain available in every window. Creation, deletion, edits, saves, renames and mode/file
changes appear on redraw.

### Keyboard and mouse controls

Buffet composes the shared [entry, table and scroll widgets](WIDGETS.md).
`(buffet:open! window)` returns its app view. Its named `table` child provides
`table:select!`, `table:move!`, `table:sort-by!`, `table:toggle-sort!` and
`table:set-columns!`. That table's `filter` child contains an `entry` child
for `entry:insert!`, `entry:delete!` and the other normal entry operations.
All these operations take explicit view IDs. The table's logical state holds
its `(collection generation key)` selection and result basis.

`table:invoke!` invokes the selected row's `activate` command, connected to
`buffet:choose!` for opening or restoring the document through its host.
`C-k` invokes the table's `trash` command, connected to `buffet:kill!`;
`C-x D` invokes `delete`, connected to `buffet:delete!`. Each receives the
Buffet view, selection and result basis. `C-x TAB` shows the table activation
expression in the keyboard section and its target API in **Widget commands**.
The table adopts the hovered or selected row; the app validates the result
basis and passes the document's metadata version to the archive operation.
Pending and stale targets refuse. Flags use
the same `buffer-flag` enumeration as `property:flags`: `conflicted` and
`read-only`. `(edit:delete-trashed! name)` is the direct archive command;
`(edit:kill-buffer! buffer)` kills a buffer directly.

`(buffet:create! owner commands)` creates an independent, unmounted composition.
An optional catalogue query shares its filter and ordering with another view.
Bind `open` and `return` explicitly to host actions; an embedded Buffet never
chooses a window implicitly. The default `window-control:open-app!` host retains the named
app, origin and reopening policy. Split panes fork view state over the query,
so selection, scroll and geometry remain independent.

- Up / `C-p` / Shift-Tab, Down / `C-n` / Tab: move the candidate row.
- Home / `C-a` / `M-<`, End / `C-e` / `M->`: select the first or last match.
- Page Up / `M-v`, Page Down / `C-v`: move by a page of rows.
- Enter: show the candidate row's buffer in this window, completing the
  switch in place; on a backup or trash row, restore that buffer.
- Esc / C-g: return to the invoking document; C-u: clear the filter.
- C-k: kill the chosen live buffer.
- C-x D: permanently delete the chosen Trash or Backups entry.
- F1–F6: cycle sorting for Modified, Flags, Buffer, Lines, Mode and File.
- Move the pointer over a row: emphasize that candidate without taking focus.
- Click a row: show its buffer in the selected window; focus stays there.
- Click a heading: cycle its sort key ascending, descending, then off.
- Wheel over the app: scroll that window normally, including Backups and Trash.
  The focused window stays focused and no buffer is opened.
- Click the app's status line: focus `<buffet>`.

Kept in another window, the same app is a control panel: a click
switches the focused window without taking focus. Click the panel's status
line to focus it and type a filter. Global `M-Shift-Up` / `M-Shift-Down` and
Meta-wheel traverse live buffers in the table's current sort order, with
wraparound, independently of its filter. With no sort columns enabled they
use buffer names. Backups and Trash are not switching destinations.
The public app API is documented in [App buffers](APPS.md).

## Scrollbars

Ordinary document windows show no scrollbar by default. Configure
`(window-control:scrollbar 'right)` or `'left` for a one-column position bar,
`'auto` for overflowing text, or `#f` to hide it. A window's own preference
overrides this default. Widgets such as Buffet use their own scroll container.
The thin `│` is the track and the heavy `┃` shows the visible extent.
Wheel input scrolls the viewport under the pointer without changing focus.

Every frame is a cached repaint -- rows are painted only when their content
changed -- framed in a synchronized update, so fixed chrome never shifts and
scrolling does not flicker. e does not use the terminal's native scrolling.

## Scrolling, wrapping, and windows

`(text-layout:scroll-margin 8)` keeps point that many rows away from the top and bottom when
the buffer has room. PageUp and PageDown operate on the viewport rather than
point: in the middle they shift its top by exactly one full window body and put
point in the middle of the result. A partial page clamps at the first or last
viewport and still centers point; pressing outward again moves point to the
first or last line. If the whole buffer fits, its viewport stays at the top and
PageUp/PageDown put point at the first/last line. Wrapped screen rows count
individually, while sticky app headers are excluded from page height.

Mousewheel input scrolls the hovered viewport without focusing it or
changing its selection. Movement clamps at the source boundaries; an
enclosing viewport may consume remaining scroll distance.

Long lines soft-wrap by default. A continuation row ends in `\`. With wrapping
off, truncated lines end in `$` and the window scrolls horizontally to follow
point. `(text-layout:wrap-lines #f)` changes the default. `C-x t` calls
`window-control:toggle-display!` with `wrap` for the active window;
`window:set-display!` sets an explicit policy to `#t`, `#f` or `default`.
Ordinary retained editors follow that policy. App editors and terminal grids
keep their own wrapping contract; a terminal surface never soft-wraps.

Wrapping, Up/Down, and paging measure screen cells, keeping the visual column
across wide characters and combining sequences. Selections highlight whole
displayed glyphs; clicking either cell of a wide glyph selects its start.
Clipped glyph fragments display as blanks at pane edges. Tabs and other
control characters occupy one blank cell. Buffer positions and the status
column still count characters.

Each split has independent point, scrolling, wrapping, and status. Splits form
a tree, so either half may be split again in either direction: `C-x 2`
(`window-control:split! window 'below`) divides only the current window into a stacked pair,
and `C-x 3` (`window-control:split! window 'right`) divides only it into a side-by-side pair.
The same operation accepts `above` and `left` for a new window first;
these have no default keys. Deleting a window with `C-x 0`
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
Completions and Describe use the screen's auxiliary manager above the
message area. Showing or hiding it preserves the main windows' documents
and selections. `screen:auxiliary!` reveals it; `screen:hide-auxiliary!`
hides it and restores the surviving focus origin. It uses the same split
container and geometry adaptation as other windows.
`M-Up`, `M-Down`,
`M-Left`, and `M-Right` cast an imaginary ray from the cursor in that direction
and focus the first window it crosses. Thus the cursor's row chooses between
stacked windows beside a tall window, and its column chooses between adjacent
windows above or below it. The destination keeps its own point position.

Window edges are resized by dragging them with the mouse, respecting the
split tree's ownership and minimum sizes.

### Window links

A window can link to others, directed and tagged. For an explicit manager,
`(window:link! manager from to 'target)` adds a target and
`(window:unlink! manager from to 'target)` removes it. `window:links` reads
`(from to tag)` triples. References are window models, not displayed numbers;
closing a window removes its links. Finder opens a chosen file in its target
windows while preserving its own view and focus. Without targets, the normal
keyboard or inactive-panel host placement applies. Tags are ordinary symbols.

## Buffer API

Buffers are stable references such as `'(buffer 17)`. Resolve a name once
with `store:find-named`; renaming never changes the reference and deletion
never reuses it. Shape checks do not imply availability. Read text with
`edit:buffer-text`, `store:line`, `store:line-count` or `store:snapshot-state`;
read names/facts with `store:buffer-name`, `store:property` and `store:metadata`.
These queries do not need a window. `mode:of` and `mode:name-of` take the same
explicit document. `modified-at` is the last content-change time in UTC
nanoseconds, or `#f` before one; it survives saves, while `modified` denotes
unsaved changes.

`store:create!` creates content without placing it.
`window-control:open-document!` places a document in an explicit window;
`window-control:discard!` applies the window's disposal and fallback policy.
Shared documents go to Trash, disposable output is deleted, and apps retire
their owned resources. `edit:restore!` returns an archived document reference
for explicit placement. `edit:call-as-one-edit!` groups attributed edits.

Editors are view references, not buffer aliases. `edit:basis` captures
`(immutable-lines document revision)` from an acquired editor.
`edit:replace-region-text!` changes an explicit editor range;
`edit:rewrite-regions!` applies guarded ranges computed against a captured
basis, preserving the logical selection. Use explicit view APIs for deferred
work rather than changing a global current buffer. See [Widgets](WIDGETS.md).

Opening a file reads and admits its text, disk baseline and canonical path
in the base. A missing file and its missing parent directories are created
immediately; each actual creation is logged once. The adopting head detects
the mode, preserving any explicit mode chosen before adoption. A create
subscriber's later edits and undo history survive opening.
Overlapping visits, including a callback reopening the same path, reuse the
same shared buffer and preserve edits made before the first visit returns.

For extension code, `(document:acquire! actor absolute-path)` acquires a
file without selecting a window: `(directory path)` for a directory, or
`(buffer id admitted? path diagnostic)` for a file. The optional diagnostic
describes a disk review that could not be applied; shared work is retained.
`edit:visit-file!` requires a destination
`(lambda (kind value) ...)` receiving a directory path or buffer reference.
`screen:open-file!` supplies the default screen placement policy. File reads and creation run in the base's connection worker,
outside its lifecycle dispatcher and store locks. External disk changes
merge undoably; concurrent text or file-fact changes refuse a stale review.

`(document:reload! actor id)` rereads a shared document's file in the base
and merges it as one undoable action. `(document:reread! actor id)` replaces
the text and settles pending conflicts, also undoably. Both return status
and detail: `applied` with `(revision conflicts)` for reload, or the revision
for reread. A concurrent edit or change to the reviewed file facts returns
`refused` with `stale-review`; unreadable files raise. Neither operation
requires an editor window or discards earlier undo history.

`(document:check! actor id)` checks for changed disk content using the saved
stamp as a hint. Equal content updates only the reviewed stamp. Its boolean
result can schedule a reload after an editing action, but does not authorize
reusing an old disk observation. An unreadable or unvisited file returns
`#f`. These operations share the acquisition service's base worker boundary;
heads receive results and ordinary text deltas, not a disk-text round trip.

`(document:save! actor id canonical-path '(first-line mode-name))` saves
a shared document without requiring a window. The last argument carries a
reviewed first line and the head's detected mode name (or `#f`) for Save As;
an inconsistent first line refuses adoption. The base owns file reads,
undoable merge/reread decisions, backups, writing and atomic publication of
the file, baseline, name and mode. Text edited during writing remains dirty;
concurrent changes to the reviewed file facts refuse baseline publication.
The result is `(saved message)`, `(unchanged message)`, `(refused message)`
or `(failed message)`. A failure after writing says so explicitly.

`edit:save-file!` supplies this mode choice and runs this head's pre-save
hooks before the request and post-save hooks after successful adoption.
Saving requires a shared document. App presentations
cannot acquire file identity through Save As; copy wanted text into a shared
document first. Read-only output stored in a shared document can be saved
after its producing app has stopped.
Restored backups detect their mode from their original path and first line.

`(store:find-file canonical-path)` looks up the shared
buffer's id or returns `#f`. `(store:visit! actor name lines facts)` requires
a canonical `file` fact and returns two values: id and whether it was created.
Lookup and creation happen together; a reused buffer keeps all its existing
state. Prepare disk text and initial facts before calling it.
Use `document:acquire!` for files; `store:visit!` is the lower-level admission
primitive for already prepared content. Visiting always uses the shared file
identity; manually constructed local buffers are not file-identity owners.

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
or delete the buffer before the call returns. Views observe the accepted name through ordinary metadata notifications.

An audience change takes effect before the head's next frame, including
changes made by that head. Hiding moves its windows to visible buffers,
closes dependent local views, and withdraws its managed marks; shared text,
history, and other actors' marks survive. Dropping `audience` restores the
default. `(store:visible? actor id)` tests existence and audience; raw store
reads remain available under their own policy. Audience is routing, not an
access-control boundary. Presentation demand applies visibility at its
admission boundary; raw store APIs have their own authority rules.

Shared head and policy edits, undo and redo check `read-only` inside the store
transaction as well as at the command prompt. Trusted producers can still
update their read-only output. The underlying `store:edit!` and
`store:edit-with-snapshot!` accept optional write access after the edit context;
`store:history-step!` accepts it after scope. Pass `'any` or a list of permitted
buffer names for a client mutation. Omitted access (`#f`) is the trusted
producer path; it is not exposed through session or wire requests.

`(store:set-properties! actor id facts [expected [name]])` validates and
installs a complete fact batch atomically, returning `#t` on acceptance.
`expected` checks reviewed facts: a `(key . value)` requires that value; a
bare symbol requires absence. A mismatch or deleted source returns `#f`
without changing facts, name or notifications.
For example, `'((base . "old\n") stamp)` requires the old baseline and no stamp
fact; an explicit `(stamp . #f)` would not match. Build this list from a coherent
snapshot with `(property:select facts '(file base stamp))`. A mismatch or deleted
buffer returns `#f` without mutation or notifications. Omission or `#f` is
unguarded; an empty list still requires a live buffer. The predicate compares
values, independently of text revisions and undo's property-version checks.
An optional nonempty `name` commits with the facts, using the ordinary unique-name allocator. Pass `#f` for `expected` to combine
an unconditional fact update and rename. A stale review refuses both; all
accepted fields are installed before any notification. Subscribers may advance the accepted state before the caller resumes.
For detection without mutation, `(mode:detect path first-line)` returns a
registered mode record or `#f`. Its `mode:name` can join a larger fact batch;
save uses this to detect outside the store's mutation lock.
`base` is a string or `#f`; `trailing` and `disposable` are booleans.
Shared `modified` is derived and cannot be set or dropped. Generated output
can set `disposable` to `#t`; registered apps and tool buffers do so already.
Shared fact admission and reads copy finite data: pairs, vectors, strings,
bytevectors, and scalar Scheme data. Mutating a supplied value or a read
result cannot change store state; use a fact transaction. Cycles and runtime
objects such as procedures or records are rejected before mutation. Use ordinary data flags for shared read-only state.

`(store:snapshot-state id)` return
`(values text revision facts)` from one current read. This can be
newer than a view's acquired text. `(store:reset! actor id lines facts)` install a baseline and related facts
together; facts are optional. An invalid input changes neither text nor facts.
An optional final `(revision fact ...)`, built with `(cons revision facts)`
from that state read, requires the complete reviewed state to still match.
The reset returns the accepted revision, or `#f` if the review is stale
or the source has disappeared. A refused reset does not change history,
marks or the head cache. Omitting the review (or passing `#f`) keeps explicit
unconditional replacement. A returned revision describes that reset's commit;
callbacks can already have advanced the store beyond it.

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

`(store:conflict-state id)` returns `(values text revision conflicts)`
from one read. Use it to build conflict previews: the regions and Disk
alternatives describe exactly the returned text, which can be newer than
the head's cached buffer. Each conflict is
`(revision actor labels region mine disk)`; Disk always shows the current
text that choosing Disk keeps, including changes since the first reload.
Settled conflict records remain only while their resolution can be undone.

For independent review drafts, use `(conflict-review:create! actor documents)`.
The persistent model records an explicit scope, exact reviewed alternatives
and Mine choices. `choose!` takes the draft ID, expected model revision,
groups of `(document alternative ...)`, and `mine` or `disk`. It only changes
choices. `refresh!` takes the draft and expected revision plus its new scope;
it retains choices through unrelated edits when the retained history proves
their identity and records invalidated choices otherwise.
`settle!` takes the draft, expected revision and scoped document IDs, returning
`(document status detail)` per document. It validates the complete reviewed
set without silently refreshing before writing. Each document settles
atomically and undoably; refused documents keep their choices. `preview`
returns `(draft-revision document source-revision text regions)` for an
explicit document. `close!` retires the draft and scoped views while keeping
the borrowed documents. Mutating operations take the actor first.

Load `conflict-source` to present a review through a collection:
`(collection:create! actor review "" '() 'persistent)`. Demand prepares
bounded row pages in base workers; hidden queries stop work. Rows use stable
`(document revision)` keys and keep scope order. `conflict-source:choose!`
takes a displayed row selection, result basis and side. Its `choose-all!`
and `settle!` take the query, displayed generation and basis (plus the side
for choosing). These operations take the actor first and reject stale
listings without sending alternative text back through the command channel.

In the base, `(store:state id basis)` returns `#f` for an absent buffer, or
`(name text revision facts [changes])` from one read. Pass `#f` for no chain,
or a revision to include it. Names and facts are owned copies; text remains
an immutable snapshot. This supplies the base's `state` request,
so concurrent rename/deletion cannot split the name from the snapshot.

Named-head recovery retains the base view graph and its logical selection
anchors. The head reacquires sources and projects those anchors through
retained changes, without restoring geometry or a second screen checkpoint.
Views and environments survive according to their declared lifetimes.

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
`(group-key label . options)`: the same non-false key groups that
actor's transactions in this buffer into one undo action.  Without a key,
each call is an action. The options are an alist: `(undo . facts)`,
`(commit . facts)`, `(expected . review)` and `(labels . alist)`, the labels
riding on the log entry with `(batch . id)` naming the edits made together;
every head edit carries one, per action or per grouped command.
`(store:log id [selector])` lists the retained entries newest first as
`(revision actor labels delta origin state)`, narrowed by a selector alist
among `count`, `actor`, `batch`, `since`, `until` and `state`; an entry is
`disabled` while a live undo or rewrite reverts it. `(store:rewrite-preview id
revisions)` gives `(values text mapping conflicts revision)`, the text with those
entries disabled and the rest rebased over their absence, the deltas
taking the current text there, and `(revision . cause)` pairs for the
entries a later entry overlaps, without changing anything;
`(store:rewrite! actor id revisions)` disables them for everyone, the
inverses the actor's own undoable action, blocked with the same conflicts
when any remain. Undo and redo plan through the same inversion. A buffer
retains `(store:log-retention)` entries, 4096 by default, and the
retained log is saved and restored with the session.  Properties such as `((trailing . #t))` commit with
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

Head extensions capture `edit:basis` for computations and submit through
explicit editor commands. `edit:rewrite-regions!` fences the receiver and
rebases unchanged ranges from that basis. A stale or unavailable history
refuses instead of assigning coordinates saved before another edit.

`(store:undo! requester id [scope])` defaults to `mine`.  Use `all` to select
the latest live action of any actor, or `(actor who)` to select that actor.
`(store:redo! requester id)` reverses this requester's latest undo, including
one that undid somebody else's work.  Both return `(values 'applied revision)`,
`(values 'blocked reason)`, or `(values 'nothing #f)`.  Reasons include
`overlap`, `property-changed`, and `basis-too-old`.  A group commits entirely or refuses entirely;
undo and redo preserve the revision log.  The retained delta chain and each
group's parts are bounded by `(store:log-retention)`, 4096 by default;
unavailable history never permits a partial group undo. A selective rewrite
temporarily disables members of their original actions; undoing it makes
those members eligible again. A new edit invalidates that requester's redo.

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
