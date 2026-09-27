# Finder

`C-x C-f` opens the `<finder>` app in the current window. It starts in the
current file's directory, an app's working directory, or the head's launch
directory. `M-x (finder:open-directory! "/some/directory")` starts elsewhere.
The original path-entry prompt remains available as `M-x (edit:visit-file!)`.
`C-x f` has no default binding.

The first line is the filter, followed by an italic `[N matches]` count.
Connecting directories do not inflate that count. A trailing `+` means the
scan is still running or some paths could not be read.
The Directory line shows the
directory with a trailing slash and scan status. Below your home directory,
its prefix becomes `~/`, as in `~/git/e/`. Each ancestor component and its
following slash is clickable: `git/` goes to `~/git/`, and `~/` goes home.
At home itself the full path appears, such as `/home/paveluv/`, with its
ancestors clickable. To reach root from a home descendant, click `~/`, then
the first `/`. Paths outside home also appear in full.
Hover highlights just that component with bold text and a muted dotted
underline. The final component, `e/` here, is the current directory and
stays plain. Narrow panes elide the start of the line; the ellipsis is not
clickable, and visible components still lead to their full paths.

The list contains child directories first, then files. Enter or click a
directory to enter it; Right does the same for a selected directory. Left
goes up one level and selects the directory just left when it matches the
filter, enabling hidden entries if needed to show it. Each window remembers
its selection in visited directories for the same filter, so Left–Left–Left followed by
Right–Right–Right retraces the route. A breadcrumb jump also selects the
branch leading back to the previous location. Backspace goes up when the
filter is empty. Left, Right, Enter and directory/breadcrumb clicks preserve
the filter exactly as typed; C-u explicitly clears it.

Type to filter, ignoring case. Up/Down, C-p/C-n, Shift-Tab, Home/End
and PageUp/PageDown choose a row; Enter opens it. An exact path takes priority.
Otherwise a unique nested file match is selected directly. Enter on an empty result stays in the current directory.
Esc or C-g returns to the document from which this window opened the app.

| Key | Action |
|---|---|
| `C-u` | Clear the filter. |
| `Tab` | Expand the filter without changing its matches. |
| `Left` / `Right` | Go to the parent / enter the selected directory. |
| `M-c` | Enter Create mode, with a path prompt below the live table. |
| `F1`–`F6` | Cycle sorting on the corresponding column. |
| `M-.` | Toggle hidden entries. A filter with a component beginning with `.` also includes them. |
| `C-r` | Rescan the directory with the current filter and settings. |

The filter is a list of literal keys separated by spaces. Every key must
occur in the path, ignoring case, and their occurrences must not overlap.
Keys may appear in any order and include slashes. Repeating a key requires
another occurrence. Quote a key as a Scheme string to include spaces, for
example `"my notes" txt`. A quoted leading tilde remains literal.

A key starting with `/` matches from the filesystem root. An unquoted leading
`~` expands to your home directory, so `~/src txt` searches there regardless
of the directory shown. Without a rooted key, the current directory's full
path, ending in `/`, is an implicit key. It consumes that prefix, leaving
your explicit keys to match below the current directory.

Matching stops at the first file or directory that contains all keys.
For `A/B/C/file.txt`, `A B` finds `A/B/`, `B/C` finds `A/B/C/`, and
`A txt` finds the file. A matching directory's descendants are omitted:
matching a directory does not flood the list with everything inside it.
An empty filter shows the current directory's immediate children.

Tab extends the filter without changing its matches. It maximizes literal
characters minus separating spaces; fewer keys break a tie. For example,
`file` scores 4 and `f i l e` scores 1. A single result expands to its whole
path, quoted if necessary. Multiple results may produce several shared path
fragments. Completion preserves hidden-entry visibility and directory scope.
Tab waits for a complete scan, then computes in the background; `Completing…`
marks that work. Keep typing to cancel it and refine the search. An unreadable
subtree prevents completion from assuming the visible results are exhaustive.
Use M-c for the separate Create prompt and literal path completion.

## Create mode

M-c sets the Filter aside and opens a temporary `<create-file>` view with an
editable `Create file:` prompt at the bottom of the window, seeded from the
filter's literal path. Directory follows the path being edited. The table
shows only immediate children whose names start with its final component,
using the same case-sensitive
matching as find-file. For example, `src/re` shows `re…` entries inside
`src/`; it does not search below those entries. A trailing `/` shows the
directory's children. Dot entries appear when the final component starts
with `.`. Normal browsing's hidden-entry preference is retained for later.

Tab completes a component or extends a common prefix, adding `/` for a
directory. Candidates are already visible; repeated Tab pages through them
only when they do not fit. PageUp/PageDown, Shift-Tab and the mouse wheel
also page the table. Sorting by headings or F1–F6 still works and uses the
whole matching list, before paging. Clicking a directory or breadcrumb
updates the path and its table. Clicking a file fills the prompt so you can
edit its name. Enter refuses an existing file and shows `[file already exists]`
as an italic ghost immediately after the input. It disappears after two seconds
or when you continue editing; it is not repeated in the echo area.
Up/Down browse file history; Left/Right edit the path.
Tab also refreshes the directory's metadata; C-r rescans without completing
input, so external file creations and removals can be picked up in this mode. These keys, with F1–F6, are the `finder-create` context's, listed by `C-x TAB` while the mode is open.

The input keeps find-file's editing, cursor, wrapping and error recovery.
Esc or C-g removes the prompt and returns to normal files mode at the
directory currently shown, with the filter you had before M-c. If that
directory does not exist or cannot be read, Left still goes to its parent. Creation starts
only when you press Enter.

## Creating files and directories

Creation is explicit and works regardless of what the filter matches. For
example, type `notes`, press M-c, then Enter to create a new file called `notes`,
even if the list contains `notes.txt`. The prompt uses the literal path rather
than the selected search result. Open existing files from the finder's browsing mode.

For a new file, Enter creates missing parent directories and an empty file on
disk, then opens its buffer. No save is needed to create it. Creation refuses
an existing name, including a symbolic link, without changing it. For example,
`drafts/idea.txt` creates `drafts/` if needed. End the path with `/` to create
directories only and enter the last one: `drafts/research/` creates both
levels if necessary. The new directory opens with an empty filter. An existing directory with a trailing slash is refused
with the same transient inline ghost, `[directory already exists]`. Without
a trailing slash the request is for a file, so any existing name is refused
with `[file already exists]`. Use the finder's browsing mode to enter existing directories.

Each newly created directory is logged as `Created directory /path/`, from
parent to child, followed by `Created file /path/name` for a file. Existing
directories and refused names do not produce creation entries.

Cancelling before Enter leaves the filesystem alone. Errors, including a
file where a directory is required, keep the prompt editable. Paths created
before a later error remain available and are logged; existing files are
never replaced by directory creation.

## Recursive filtering

A nonempty filter searches below the current directory, or from root when
a key is rooted. Directory paths include their trailing `/` for matching.
Typing a dot component, such as `lib/.git/`, includes hidden entries.
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

The implicit current-directory key changes when you navigate. Entering
`lib/` with `lib foo` still in Filter may therefore produce no matches.
Edit or clear the filter to search for `foo` there, or keep a rooted key to
search independently of the current directory.

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

Dotfiles and dot directories are excluded by default. `M-.` includes them;
`(finder:show-hidden #t)` enables them in configuration. Directory symlinks
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
Linux uses `statx`; unavailable fields show `—`. On other supported systems,
the current fallback supplies type, permissions and modification time, with
size and creation time unknown. Inode-change time is never labeled Created.

## The app as an API

The finder pane is driven entirely through `finder:` commands, so M-x or
an agent can do everything a key does. Every key of the pane is bound in
the `finder` context to one of them: `finder:choose!` for Enter, `enter!`,
`parent!`, `next-row!`, `previous-row!`, `page-down!`, `page-up!`,
`first-row!`, `last-row!`, `erase!`, `clear-filter!`, `create!`,
`(toggle-sort-column! n)` for `F1` to `F6`, `toggle-hidden!`, `refresh!`,
`complete!`, `return!` and `paste-filter!`, and typing itself is the context's
`SELF-INSERT` binding, `(extend-filter! text)` with the character typed, so
`C-x TAB` lists them all, typing as `any character`, and `C-h k` describes
one. Beside the keys, `(filter! text)` sets the filter that typing grows, `(select! path)` makes a listed entry the
choice, `(chosen)` is the choice's path, `(entries)` lists what is shown as
literals, `(file "path")` and `(directory "path")`, so each reads back as a
value, `(location)` is the directory shown, and `(sorts)` the
sort order as `(column . descending?)` pairs; `open-directory!` opens the
pane on a directory.

## Windows and heads

Like `<buffet>`, `<finder>` shares its directory, filter and sort order between
windows in one head. Each window fits its own columns and retains its own
keyboard choice and viewport. Narrow panes hide lower-priority metadata;
names stay visible and long labels are shortened without wrapping.
While Create owns one pane, sorting can still be changed from another
finder pane. Navigating from that other pane ends path entry and keeps the
chosen destination, using the prompt's usual focus-loss behavior.

The focused pane's keyboard choice is bold with a soft blue background:
pale in a light theme, dark navy in a dark theme. A hovered row
gets the same tint and a muted dotted underline, taking precedence over the
keyboard choice. Headings and breadcrumbs keep their own background and use
bold text with the same dotted underline.
Hovering does not move the keyboard choice or scroll the list. Normal browsing
hides the cursor and disables text selection. The status bar shows the pane's
name only; `C-x TAB` lists its keys.

A chosen file, by Enter or by a click, opens in the focused window, and the
files view steps behind in the recency list, so `C-x b` offers the document
the view replaced and Enter returns to it. When the window has target links,
`(window:link-target! (window 2))` say, the file opens in every target window
instead and the finder keeps its view and the focus. A directory click
navigates the app while keeping keyboard focus where it was. The mouse wheel
scrolls the pointed pane by the usual fraction of its height, without opening
files or changing keyboard focus. Named-head
reattachment restores the directory, filter, matching mode, sorts, hidden-entry setting and
selected paths, then rescans. Other heads have independent finders.
