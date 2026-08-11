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

Syntax highlighting is provided by *modes*, registered by modules:

```scheme
(register-mode! "scheme"
  '(".scm" ".ss" ".sls" ".sps" ".sc" ".e")   ; file-name extensions
  '("scheme" "petite" "chez" "guile")        ; #! interpreters, for
  scheme-styles)                             ;   files without an extension
```

A buffer gets the first mode whose extension matches its file name, or —
failing that — whose interpreter name appears in the file's `#!` first
line. The styles function maps a line to a vector of per-column style
symbols (or `#f` for plain text); brackets styled `delimiter` take part
in bracket matching. The mode's name is shown in the buffer's status
line. Buffers without a mode render unstyled, and bracket matching falls
back to counting every bracket. Two modes ship in `.e/`: `scheme-mode.e`
(Scheme, by extension or `#!` interpreter) and `md-mode.e` (Markdown:
headings, blockquotes, lists, fences, rules, inline code, bold, italics,
links). A module that fails to load reports itself in the message line
without preventing startup.

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

Prompt input is line-editable with the usual bindings: `C-a`/`C-e`,
`C-b`/`C-f` and the left/right arrows, Home/End, `C-d`/Delete,
Backspace, `C-k` (kills into the shared kill ring) and `C-y` (yanks from
it — including text killed in a buffer). Up and down arrows are reserved
for history navigation where a prompt keeps one (M-x).

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

### Evaluating Scheme

`M-x` prompts for a Scheme expression — the opening parenthesis is
supplied (and cannot be deleted), and missing closing parentheses are
forgiven. The expression is evaluated in the editor's own top level, so
it can call any editor function or inspect its state:

    M-x (buffer-name (window-buffer current-window))

Each exchange is appended to a read-only `*eval*` buffer, which pops up
in another window (without stealing focus) if not already visible:

    [1]> (+ 1 2)
    3
    [2]> (buffer-name (window-buffer current-window))
    "e"

Entries are numbered from 1 (restarting if the buffer is killed), the
expression shows any parentheses that were auto-closed for you and is
syntax-highlighted, and results — errors included — appear beneath it,
unprefixed, in a distinct color. The up and down arrows browse
previously evaluated expressions, newest first; going back down past the
newest restores whatever you had been typing.

TAB completes the symbol being typed against everything bound in the
editor's top level — Chez itself, the editor's own functions, and any
loaded modules — with the same longest-common-prefix and `*Completions*`
pop-up behavior as the file and buffer prompts. While you type, the
parameters still to be supplied to the innermost open call appear after
the cursor as a grey suggestion, shrinking as you enter arguments:
`M-x (vector-sort` suggests `predicate vector`; after typing the
predicate only `vector` remains. Parameter names come from the source
for procedures loaded from it, from the `chez-sigs.e` module's
documented signatures for common builtins, or fall back to generic names
derived from the arity (`arg1 arg2`, with `[arg3]` for optional ones and
`...` for a rest). Rest parameters (`num ...`) persist, and the
suggestion follows nested calls — inside `(car (cdr` it shows `pair`
for the `cdr`.

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
