# Finder

`C-x C-f` opens the `<finder>` app in the current window, retaining its last
filter. On first use, the filter is prefilled with the current file's directory,
an app's working directory, or the head's launch directory, as a full path
starting and ending with `/`. `M-x (finder:open! window "/some/directory")`
resets the filter to that directory's full path.
`M-x (edit:visit-file! path)` also opens or creates a path, with path completion.
`C-x f` has no default binding.

The first line is the filter, followed by an italic `[N matches]` count.
Separating spaces in the filter appear as ` ∧ ` (logical AND), so `sls m19` is shown as `sls ∧ m19`. Spaces inside quoted paths stay literal.
One Backspace removes the whole ` ∧ ` separator.
This is only a display convention; typing, matching and completion use spaces.
Connecting directories and creation suggestions do not inflate that count. A trailing `+` means the
scan is still running or some paths could not be read.
There is no separate Directory field: the first token is the leading path.
Its existing directory portion determines where the table starts. `/home/me/git/`
shows that directory's children; `/home/me/gi` shows matching entries under
`/home/me/`, without displaying the tree from root. Backspace edits this path
one character at a time and the table follows immediately. A leading `~`
expands to home; otherwise a missing initial `/` is supplied automatically.
Directory lookup follows the filesystem's spelling; filtering names ignores
case. Missing path components, and the path after them, appear in italics in
the normal text color. Existence checks run in the worker and are cached until
`C-r`. Narrow panes scroll the entry horizontally to keep its caret visible.

The list contains child directories first, then files. Enter or click a
directory to enter it; Right does the same for a selected directory. Left
goes up one level and selects the directory just left, enabling hidden entries
if needed to show it. Each window remembers its selection in visited directories,
so Left–Left–Left followed by Right–Right–Right retraces the route. Entering a
directory, by keyboard or mouse, replaces the whole filter with its absolute
path ending in `/`; extra tokens are cleared. Left does the same for the parent.
C-u clears the entire filter and shows root's immediate children. Backspace
on an empty filter does nothing.

Type to filter, ignoring case. Up/Down, C-p/C-n, Shift-Tab, Home/End
and PageUp/PageDown choose a row; Enter opens it. An exact path takes priority.
Otherwise a unique nested file match is selected directly. Enter on an empty result stays in the current directory.
Esc or C-g returns to the document from which this window opened the app.

| Key | Action |
|---|---|
| `C-u` | Clear the whole filter; show root's immediate children. |
| `Tab` | Complete a unique directory path with `/`; otherwise expand the filter without changing its matches. |
| `Left` / `Right` | Go to the parent / enter the selected directory. |
| `C-b` / `C-f` | Move the filter caret by one grapheme. |
| `F1`–`F6` | Cycle sorting on the corresponding column. |
| `M-.` | Toggle hidden entries; `[showing hidden]` appears while enabled. |
| `C-r` | Rescan the directory with the current filter and settings. |

After the leading path, Space starts another literal key. For example,
`/home/me/git/ sls m19` is displayed as `/home/me/git/ ∧ sls ∧ m19`.
Every key must occur in the path, ignoring case, and occurrences must not
overlap. Additional keys may appear in any order and include slashes;
repeating a key requires another occurrence. Matching token text is underlined
in the table, including fragments across directory boundaries. Quote a token
as a Scheme string to include spaces, for example `/home/me/ "my notes" txt`.
A quoted leading tilde remains literal. An unquoted leading tilde in a key
expands to home; a key starting with `/` is anchored at root. As all keys need
separate occurrences, a second rooted key cannot overlap the first one.

Matching stops at the first file or directory that contains all keys.
Below `/work/`, keys `A B` find `A/B/`, `B/C` finds `A/B/C/`, and
`A txt` finds `A/B/C/file.txt`. A matching directory's descendants are omitted:
matching a directory does not flood the list with everything inside it.
A leading directory path alone shows its immediate children.

When the filter contains only a path matching a single directory, Tab appends
`/` and lists that directory's children. For example, `/home/me` completes to
`/home/me/`. An unfinished directory name completes the same way when unique.
Otherwise Tab extends the filter without changing its matches. It maximizes literal
characters minus separating spaces; fewer keys break a tie. For example,
`file` scores 4 and `f i l e` scores 1. A single file expands to its whole
path, quoted if necessary. A multi-key search completing to a directory keeps
that match; Enter opens it.
Multiple results may produce several shared path fragments. Completion
preserves hidden-entry visibility, directory spelling and the match set.
Tab waits for a complete scan, then computes in the background; `Completing…`
marks that work. Keep typing to cancel it and refine the search. An unreadable
subtree prevents completion from assuming the visible results are exhaustive.
Creation suggestions do not constrain completion or contribute to match counts.

## Creating files and directories

The missing portion of the leading path also appears as a hierarchy of virtual
table entries. Each missing component is italic and has an italic `[create]`
ghost. The table starts at the nearest existing directory: if `/a/b/` exists
but `/a/b/c/d/file.txt` does not, it shows:

```text
c/ [create]
 d/ [create]
  file.txt [create]
```

These entries remain available with additional filter keys, even when those
keys do not match the proposed name. They sort alongside existing entries,
using the same sibling ordering and directories-first rule. Their metadata
is blank. Existing matches keep the default selection; when there are none,
the proposed leaf is selected. To create `notes` when `notes.txt` matches,
select the `notes [create]` row and press Enter or click it.

Choosing a proposed file calls `edit:visit-file!`: missing parents and an
empty file are created on disk, then its buffer opens. Choosing a proposed
directory creates only that directory and its parents, then enters it. A
trailing `/` requests directories only. Selecting `c/` in the example creates
only `c/`; selecting the file creates the entire path. Right enters or creates
a directory and does nothing on a file. Creation refreshes the cached listing.

The same API works directly: `(edit:visit-file! "/a/b/c/file.txt")` creates
and opens a new file; `(edit:visit-file! "/a/b/c/")` creates directories and
opens Finder there. Existing files and shared buffers are reused. If another
process creates the target first, its contents are opened without replacement.
Files or dangling links blocking a parent directory cause an error.

Each new directory is logged as `Created directory /path/`, from parent to
child, followed by `Created file /path/name` for a file. Paths created before
a later error remain available and are logged. Typing or selecting a row
with arrows does not create anything; Enter, Right on a directory, or a click
performs the action. There is no separate Create app or M-c binding.

## Recursive filtering

A filter with additional keys searches below the directory derived from
its leading path. Directory paths include their trailing `/` for matching.
Dot-prefixed filter keys, such as `.sls`, do not change hidden-entry visibility.
Every matching path is expanded as a tree, with one space of indentation per
level and no limit on the number of matches or the depth of expansion.
Intermediate directories show their own descendant counts and can be entered
with Enter or a click, just like immediate directories. For example, a match
at `A/B/C/foo.txt` appears as:

```text
A/             1
 B/            1
  C/           1
   foo.txt
```

A count includes matching files and directories below that row. Connecting
directories do not count as matches themselves. A directory that satisfies
the filter has a descendant count of zero, because search stops there.

Entering a listed directory starts a fresh overview there, clearing extra
keys. There is no filter pruning or hidden matching base.

Scanning, sorting and completion run in the background. Typing or navigating
replaces pending work. The previous table remains as a preview until a new
result arrives; Enter cannot open a preview row excluded by the new filter.
`Searching…` marks work in progress, `+` marks a lower bound, and `?` means
unknown. An unreadable or vanished subtree leaves its count incomplete.

Listings and metadata are cached for the lifetime of the Finder app. Filters
and navigation reuse this inventory, including work from cancelled searches.
On Linux the scan reads entry types directly from directory listings.
Metadata and formatted table cells are loaded only for visible rows, except
that sorting by metadata needs those values for all results. Large result
tables remain scrollable while background work continues.

Filesystem subscriptions are disabled: Finder consumes no directory watches.
Use `C-r` to discard the cache and pick up external changes. The inventory is
not an atomic filesystem snapshot; cancellation takes effect between filesystem
operations.

Dotfiles and dot directories are excluded by default. `M-.` toggles them;
`(finder:show-hidden #t)` enables them for newly created queries in configuration.
You can navigate directly into a hidden directory by typing its full path ending
in `/`; its children follow the same hidden-entry setting. Directory symlinks
are marked `@/` and can be entered explicitly. Recursive searches do not
follow them, so links cannot create loops or duplicate entire subtrees. Files
inside a link can be reached by entering that directory; a typed path through
it keeps the directory available as a navigation row. Links with missing or
inaccessible targets can match their own names without making counts incomplete.
Files and directories with control characters in their names have escaped labels;
opening still uses the exact original path. Devices and FIFOs cannot be opened
as text files.

## Columns and sorting

| Column | Value |
|---|---|
| Name | Entry name, indented beneath its parent in recursive results; `/` marks a directory and `@` a symbolic link. |
| Size | File size in bytes or binary units (KiB, MiB, …). Directories have no byte-size value. |
| Modified | Last data modification time, displayed in local time with the date. |
| Created | Birth time, when available. |
| Permissions | Unix type and permission flags, including setuid, setgid and sticky bits. |
| Entries / Matches | Visible immediate entry count in the directory overview; descendant match count during a search. |

Click a heading or press its F-key to cycle ascending → descending → off.
Several columns may be active. Click order determines priority; changing
direction retains it, and disabling then reenabling a key moves it to the end.
For example, `Size¹↓` and `Name²↑` mean largest first, then name. Sorting applies
among siblings, with directories before files. Each directory stays together
with its descendants.

Sorting uses numeric sizes, permissions, counts and full timestamps including
nanoseconds. It never compares formatted size or time labels. Unknown values
come first in ascending order and last in descending order. Equal keys fall
back to path order. Metadata is obtained without reading file contents.
Linux uses `statx`; pending and unavailable fields stay blank. On other supported systems,
the current fallback supplies type, permissions and modification time, with
size and creation time unknown. Inode-change time is never labeled Created.

## The app as an API

Finder composes the shared entry, table and scroll widgets. `finder:open!`
returns its app model; `widget:descendant` finds the controls:

```scheme
(define browser (finder:open! window))
(define paths (widget:descendant browser 'table))
(define input (widget:descendant browser 'table 'filter 'entry))
(entry:set-text! input "/work/ sls")
(table:sort-by! paths '((modified descending) (name ascending)))
(table:move! paths 'next)
(table:invoke! paths 'activate)
```

`finder:navigate!`, `parent!`, `enter!`, `complete!` and `toggle-hidden!`
take the app model. `filesystem:refresh!` takes the actor and clears the
shared inventory; other queries using that inventory also refresh.
`C-x TAB` opens Bindings, showing the public calls and their forwarding chains.
`C-h k` describes an individual binding. Filter editing supports the entry's
ordinary selection and undo APIs; completion is one undoable, revision-guarded
replacement and cannot overwrite newer typing.

For embedding, `finder:create!` takes explicit host commands and an initial
directory. An optional existing filesystem query shares the filter and sort.
It never chooses a destination window: the host's `open` command receives a
document reference. Directory navigation stays within the app. File creation
and visiting use `edit:visit-file!`, whose two-argument form accepts a
`(kind value)` destination handler (`directory` with a path, or `buffer` with
the admitted buffer). Its ordinary one-argument form still opens in the
current window.

## Windows and heads

Like `<buffet>`, `<finder>` shares its filter and sort order between
windows in one head. Each window fits its own columns and retains its own
keyboard choice and viewport. Narrow panes hide lower-priority metadata;
names stay visible and long labels are shortened without wrapping.

The focused pane's keyboard choice is bold with a soft blue background:
pale in a light theme, dark navy in a dark theme. A hovered row
gets the same tint and a muted dotted underline, taking precedence over the
keyboard choice. Headings keep their own background and use bold text with
the same dotted underline.
Hovering does not move the keyboard choice or scroll the list. Normal browsing
shows a caret only in the editable filter; table rows are not editable text.
The status bar shows the pane's
name only; `C-x TAB` lists its keys.

A chosen file, by Enter or by a click, opens in the focused window, and the
files view steps behind in the recency list, so `C-x b` offers the document
the view replaced and Enter returns to it. When the window has target links,
`(window:link! manager finder-window target-window 'target)` say, the file opens in every target window
instead and the finder keeps its view and the focus. A directory click
navigates the app while keeping keyboard focus where it was. The mouse wheel
scrolls the pointed pane by the usual fraction of its height, without opening
files or changing keyboard focus. Named-head
reattachment restores the filter, sorts, hidden-entry setting and
selected paths, then rescans. Other heads have independent finders.
