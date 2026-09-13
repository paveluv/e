# Files

`C-x f` opens the `<files>` app in the current window. It starts in the
current file's directory, an app's working directory, or the head's launch
directory. `M-x (file-view:open! "/some/directory")` starts elsewhere.
`C-x C-f` still opens the existing path-entry prompt. To use the app for
that shortcut, put `(keymap:bind! "C-x C-f" file-view:open!)` in `config.e`.

The first line is the filename filter; the second shows the directory and
scan status. Ancestor shortcuts appear first (nearest first), followed by
child directories, then files. Enter or click a directory to enter it. Every
ancestor has a row, so a click can jump several levels. Left goes up one
level and selects the directory just left. Backspace also goes up when the
filter is empty.

Type to filter by filename, ignoring case. Up/Down, Tab/Shift-Tab, Home/End
and PageUp/PageDown choose a row; Enter opens it. A unique nested file match
is selected directly. An empty result never opens an ancestor automatically.
Esc or C-g returns to the document from which this window opened the app.

| Key | Action |
|---|---|
| `C-u` | Clear the filter. |
| `Left` / `Right` | Go to the parent / enter the selected directory. |
| `C-l` | Enter a path, with the usual completion and file history. Accept a directory to browse it, or a file to open it. |
| `F1`–`F6` | Cycle sorting on the corresponding column. |
| `M-.` | Toggle hidden entries. A filter beginning with `.` also includes them. |
| `C-r` | Rescan the directory with the current filter and settings. |

Path entry can create an empty visiting buffer for a new file in an existing
directory; nothing is written until save. Failed path validation leaves the
prompt editable. Opening a file uses the usual file identity and disk-conflict
handling, including reuse of unsaved buffers and remembered points.

## Recursive filtering

A nonempty filter searches descendant filenames as well as immediate entries.
Each immediate subdirectory shows its descendant match count. Up to 20 matches
in a group are listed individually with relative paths. Above that threshold,
the directory and its count remain; enter it to search a smaller subtree with
the same filter. The threshold does not limit counting or hide direct files
inside the current directory.

`(file-view:expansion-limit 10)` changes the per-directory expansion threshold;
zero keeps all nonempty groups collapsed. Use `C-r` after changing it through
M-x. A count includes matching directories as well as files. A directory's own
name may match independently of its descendant count.

Scanning runs in the background. Typing or navigating replaces the pending
search; results from an older search cannot replace the new view. `Searching…`
marks work in progress, `+` marks a lower bound, and `?` means unknown. An
unreadable or vanished subtree is reported and leaves its count incomplete.
This is a live filesystem inventory, not an atomic filesystem snapshot; use
`C-r` to pick up later external changes. Cancellation takes effect between
filesystem operations.

Dotfiles and dot directories are excluded by default. `M-.` includes them;
`(file-view:show-hidden #t)` enables them in configuration. Directory symlinks
are marked `@/` and can be entered explicitly. Recursive searches do not
follow them, so links cannot create loops or duplicate entire subtrees. Files
and directories with control characters in their names have escaped labels;
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
groups stay in place and ancestors always remain nearest first.

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

Only the focused pane shows its keyboard choice in bold. Hovered rows and
headings use the common bold, muted dotted underline. Hovering does not move
the keyboard choice or scroll the list. The cursor and text selection are
disabled. Status hints appear only while the app is focused.

A file clicked in a side panel opens in the focused window. A directory click
navigates the app while keeping keyboard focus where it was. The mouse wheel
browses rows in the pointed files pane without opening files. Named-head
reattachment restores the directory, filter, sorts, hidden-entry setting and
selected paths, then rescans. Other heads have independent files apps.
