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

## Key bindings

### Files and exiting

| Key       | Action                                              |
|-----------|-----------------------------------------------------|
| `C-x C-s` | Save (prompts for a name in an unnamed buffer)      |
| `C-x C-f` | Open a file (asks before discarding unsaved edits)  |
| `C-x C-c` | Quit (asks only if the buffer differs from disk)    |

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

Kept deliberately small: one buffer, one window, no configuration. Tabs and
other control characters are displayed as a single space cell. Input is read
as UTF-8, but all characters are assumed to be one column wide.

## Licence

[MIT](LICENCE)
