# Files

`C-x C-f` opens the `<files>` app in the current window. It starts in the
current file's directory, an app's working directory, or the head's launch
directory. `M-x (file-view:open! "/some/directory")` starts elsewhere.
The original path-entry prompt remains available as `M-x (edit:visit-file!)`.
`C-x f` has no default binding.

The first line is the relative-path filter. The Directory line shows the
directory with a trailing slash, plus scan status. Below your home directory,
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

Type to filter by name, ignoring case; a filter containing `/` matches
relative paths instead. Up/Down, C-p/C-n, Shift-Tab/Tab, Home/End
and PageUp/PageDown choose a row; Enter opens it. An exact path takes priority.
Otherwise a unique nested file match is selected directly. Enter on an empty result stays in the current directory.
Esc or C-g returns to the document from which this window opened the app.

| Key | Action |
|---|---|
| `C-u` | Clear the filter. |
| `Left` / `Right` | Go to the parent / enter the selected directory. |
| `M-c` | Enter Create mode, with a path prompt below the live table. |
| `F1`–`F6` | Cycle sorting on the corresponding column. |
| `M-.` | Toggle hidden entries. A filter with a component beginning with `.` also includes them. |
| `C-r` | Rescan the directory with the current filter and settings. |

Filtering is a case-insensitive substring search, and the filter is never
completed: Tab moves the row like Down. Use M-c to enter a literal path,
including absolute, home or parent paths; its prompt completes components.

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
input, so external file creations and removals can be picked up in this mode.

The input keeps find-file's editing, cursor, wrapping and error recovery.
Esc or C-g removes the prompt and returns to normal files mode at the
directory currently shown, with the filter you had before M-c. If that
directory does not exist or cannot be read, Left still goes to its parent. Creation starts
only when you press Enter.

## Creating files and directories

Creation is explicit and works regardless of what the filter matches. For
example, type `notes`, press M-c, then Enter to create a new file called `notes`,
even if the list contains `notes.txt`. The prompt uses the literal path rather
than the selected search result. Open existing files from normal Files mode.

For a new file, Enter creates missing parent directories and an empty file on
disk, then opens its buffer. No save is needed to create it. Creation refuses
an existing name, including a symbolic link, without changing it. For example,
`drafts/idea.txt` creates `drafts/` if needed. End the path with `/` to create
directories only and enter the last one: `drafts/research/` creates both
levels if necessary. The new directory opens with an empty filter. An existing directory with a trailing slash is refused
with the same transient inline ghost, `[directory already exists]`. Without
a trailing slash the request is for a file, so any existing name is refused
with `[file already exists]`. Use normal Files mode to enter existing directories.

Each newly created directory is logged as `Created directory /path/`, from
parent to child, followed by `Created file /path/name` for a file. Existing
directories and refused names do not produce creation entries.

Cancelling before Enter leaves the filesystem alone. Errors, including a
file where a directory is required, keep the prompt editable. Paths created
before a later error remain available and are logged; existing files are
never replaced by directory creation.

## Recursive filtering

A nonempty filter searches the whole tree below the directory. Without a
slash it matches file and directory names only: `lib` finds every entry
whose name contains `lib`, and a matching directory does not claim its
contents. A filter containing `/` matches full relative paths instead, and
slashes are literal: `lib/` matches that part of a path, including descendants
whose own names do not contain `lib`. Directories include their trailing `/`
for matching. Typing a dot component, such as `lib/.git/`, includes hidden entries.
Each immediate subdirectory shows its descendant match count. Up to 20 matches
in a group are listed individually with relative paths. Above that threshold,
the directory and its count remain; enter it to search a smaller subtree with
the same filter. Relative paths are measured from the new directory: entering
`lib/` with `lib/foo` still in Filter may produce no matches. Edit or clear the
filter to search for `foo` there, or use M-c to work with a literal path.
The threshold does not limit counting or hide direct files inside the current
directory.

`(file-view:expansion-limit 10)` changes the per-directory expansion threshold;
zero keeps all nonempty groups collapsed. Use `C-r` after changing it through
M-x. A count includes matching directories as well as files. A directory's own
name may match independently of its descendant count.

Scanning runs in the background. Typing or navigating replaces the pending
search; results from an older search cannot replace the new view. Known paths
that still match stay visible while the search catches up, including when
you erase part of the filter. Newly excluded paths disappear immediately.
`Searching…`
marks work in progress, `+` marks a lower bound, and `?` means unknown. An
unreadable or vanished subtree is reported and leaves its count incomplete.
This is a live filesystem inventory, not an atomic filesystem snapshot; use
`C-r` to pick up later external changes. Cancellation takes effect between
filesystem operations.

Dotfiles and dot directories are excluded by default. `M-.` includes them;
`(file-view:show-hidden #t)` enables them in configuration. Directory symlinks
are marked `@/` and can be entered explicitly. Recursive searches do not
follow them, so links cannot create loops or duplicate entire subtrees. Files
inside a link can be reached by entering that directory; a typed path through
it keeps the directory available as a navigation row. Files and directories
with control characters in their names have escaped labels;
opening still uses the exact original path. Devices and FIFOs cannot be opened
as text files.

## Columns and sorting

| Column | Value |
|---|---|
| Name | Filename or relative match path; `/` marks a directory and `@` a symbolic link. |
| Size | File size in bytes or binary units (KiB, MiB, …). Directories have no byte-size value. |
| Modified | Last data modification time, displayed in local time with the date. |
| Created | Birth time, when available. |
| Permissions | Unix type and permission flags, including setuid, setgid and sticky bits. |
| Entries / Matches | Visible immediate entry count without a filter; descendant match count with one. |

Click a heading or press its F-key to cycle ascending → descending → off.
Several columns may be active. Click order determines priority; changing
direction retains it, and disabling then reenabling a key moves it to the end.
For example, `Size¹↓` and `Name²↑` mean largest first, then name. Directory/file
groups stay in place.

Sorting uses numeric sizes, permissions, counts and full timestamps including
nanoseconds. It never compares formatted size or time labels. Unknown values
come first in ascending order and last in descending order. Equal keys fall
back to path order. Metadata is obtained without reading file contents.
Linux uses `statx`; unavailable fields show `—`. On other supported systems,
the current fallback supplies type, permissions and modification time, with
size and creation time unknown. Inode-change time is never labeled Created.

## Windows and heads

Like `<buffers>`, `<files>` shares its directory, filter and sort order between
windows in one head. Each window fits its own columns and retains its own
keyboard choice and viewport. Narrow panes hide lower-priority metadata;
names stay visible and long labels are shortened without wrapping.
While Create owns one pane, sorting can still be changed from another
files pane. Navigating from that other pane ends path entry and keeps the
chosen destination, using the prompt's usual focus-loss behavior.

The focused pane's keyboard choice is bold with a soft blue background:
pale in a light theme, dark navy in a dark theme. A hovered row
gets the same tint and a muted dotted underline, taking precedence over the
keyboard choice. Headings and breadcrumbs keep their own background and use
bold text with the same dotted underline.
Hovering does not move the keyboard choice or scroll the list. Normal browsing
hides the cursor and disables text selection. Status hints appear only while
the app is focused.

A file clicked in a side panel opens in the focused window. A directory click
navigates the app while keeping keyboard focus where it was. The mouse wheel
browses rows in the pointed files pane without opening files. Named-head
reattachment restores the directory, filter, sorts, hidden-entry setting and
selected paths, then rescans. Other heads have independent files apps.
