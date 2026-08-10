# e

A tiny, single-file Emacs-like console editor written in Chez Scheme.

The whole editor lives in one script, `e`, with no dependencies beyond
[Chez Scheme](https://cisco.github.io/ChezScheme/) and a Unix-like terminal.

## Running

```sh
./e file.txt          # or: scheme --script e file.txt
```

The terminal size is detected via `ioctl` (and tracked across window
resizes); if that is unavailable, set the `LINES` and `COLUMNS` environment
variables.

## Extension modules

At startup `e` loads every `*.e` file from `~/.e` (in name order). Modules
are plain Scheme source evaluated in the editor's top level, so they can
use and replace any of its definitions. To install the modules shipped in
this repository:

```sh
ln -s "$(pwd)/.e" ~/.e
```

Syntax highlighting lives in a module: `.e/scheme-mode.e` provides the
Scheme highlighter by replacing the editor's `line-styles` hook — a
function from a line to a vector of per-column style symbols (or `#f` for
plain text). Without it the editor runs unstyled, and bracket matching
falls back to counting every bracket. A module that fails to load reports
itself in the message line without preventing startup.

## Key bindings

### Files and exiting

| Key       | Action                                                |
|-----------|-------------------------------------------------------|
| `C-x C-s` | Save (prompts for a name in an unnamed buffer)        |
| `C-x C-f` | Visit a file in its own buffer (creating if needed)   |
| `C-x C-c` | Quit (asks only if some buffer differs from its file) |

### Buffers and windows

| Key       | Action                                                  |
|-----------|---------------------------------------------------------|
| `C-x b`   | Switch buffer (default: most recent other; new name creates a buffer) |
| `C-x C-b` | List buffers in the echo area (`*` marks modified)      |
| `C-x k`   | Kill a buffer (default: current; asks if modified)      |
| `C-x 2`   | Split the current window in two (stacked)               |
| `C-x 1`   | Delete all other windows                                |
| `C-x 0`   | Delete the current window                               |
| `C-x o`   | Move to the next window                                 |

Each window has its own status line (the active one is bright, the others
dim) and its own point and scroll position; the same buffer can be shown
in several windows at once.

Prompts complete with TAB, as in Emacs: file names in `C-x C-f` and the
write-file prompt (a unique directory completes with a `/` and descends),
buffer names in `C-x b` and `C-x k`. TAB extends to the longest common
prefix; when nothing extends, a second TAB pops up a `*Completions*`
window listing the candidates in columns (borrowing the other window, or
splitting a temporary one), which disappears when the prompt finishes.

### Movement

| Key                   | Action                        |
|-----------------------|-------------------------------|
| `C-b` / `C-f` / arrows| Left / right one character    |
| `C-p` / `C-n` / arrows| Up / down one line            |
| `C-a` / `C-e`, Home/End | Beginning / end of line     |
| `C-v` / `M-v`, PgDn/PgUp | Page down / up             |
| `M-<` / `M->`         | Beginning / end of buffer     |

### Editing

| Key             | Action                                        |
|-----------------|-----------------------------------------------|
| `C-d`, Delete   | Delete the character after point              |
| `C-h`, Backspace| Delete the character before point             |
| `C-o`           | Open a line below, leaving point in place     |
| `C-k`           | Kill to end of line (repeats accumulate)      |
| `C-@` (`C-Space`) | Set the mark                                |
| `C-w`           | Kill the region between mark and point        |
| `C-y`           | Yank the last kill                            |
| `C-_`           | Undo; `C-g` then `C-_` redoes                 |
| `C-l`           | Repaint the screen and re-read its size       |
| `C-g`           | Cancel (prompt, search, mark)                 |

### Search

`C-s` starts an incremental search: type to extend the needle, `C-s` again
jumps to the next match (wrapping around), Backspace shortens the needle,
`RET`/`ESC` accepts, `C-g` cancels and returns point to where the search
began. All matches on screen are highlighted while searching.

## Features

- Multiple buffers and stacked windows, Emacs-style: per-buffer undo
  history and mark, per-window point and scroll, a global kill ring (kill
  in one buffer, yank in another), and buffers that remember where you
  were when you come back to them.
- Scheme-aware syntax highlighting (keywords, strings, comments, numbers,
  character literals, delimiters, quotes).
- Matching-bracket highlighting: the opener under point, or the closer just
  before it, is shown together with its partner (brackets inside strings
  and comments are ignored).
- Unlimited undo with redo (`C-g` after an undo flips direction, Emacs-style).
- A kill ring entry that accumulates across consecutive kill commands, so
  `C-k C-k C-k … C-y` reassembles whole blocks.
- Incremental screen updates: only rows whose content changed are repainted,
  and single-row scrolls use the terminal's native scroll operation.
- Files round-trip byte-for-byte, including the presence or absence of a
  trailing newline.
- Quitting never asks about "modified" buffers whose text is identical to
  the file on disk (e.g. after undoing everything).

## Limitations

Kept deliberately small: windows split horizontally only (no side-by-side),
and there is no configuration beyond extension modules. Tabs and
other control characters are displayed as a single space cell. Input is read
as UTF-8, but all characters are assumed to be one column wide.

## Licence

[MIT](LICENCE)
