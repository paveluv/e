# e

A tiny console text editor written in
[Chez Scheme](https://cisco.github.io/ChezScheme/). Emacs-like in
spirit (buffers, windows, a kill ring, incremental search, a Scheme
top level), but not an Emacs clone and not trying to become one: it
borrows the ideas that stay small and stops there.

- The editor is a Scheme system. Everything is an R6RS library: a
  minimal core with a published, immutable API, and extension modules
  built on it (the syntax modes, bracket matching, the editing
  helpers, M-x itself). The core enforces its boundaries with the
  language rather than with discipline: internals are invisible,
  exports cannot be reassigned.
- M-x evaluates Scheme against the editor's live top level, with
  symbol completion, parameter hints as you type, history, and a
  transcript buffer.
- Modules hot-reload. Saving a module's source inside the editor
  reloads it into the running session; sources edited elsewhere are
  picked up with `reload-module!`. Registrations are replaced,
  dependent modules recompile, buffers stay put. e is developed from
  inside itself.
- No dependencies beyond Chez Scheme and a Unix-like terminal; no
  build, no installation. A checkout runs in place: the first start
  compiles the libraries, later starts take about 100 ms.
- Undo reports what it undoes: `Undo insert "hello"`,
  `Undo (replace-all! "xx" "yy")`. Typed characters coalesce into
  runs, a paste is a single step, an M-x command is one labeled step.

## Quick start

As your everyday editor, cloned as `~/.e`:

```sh
git clone https://github.com/paveluv/e ~/.e
~/.e/e file.txt        # add ~/.e to PATH for plain `e`
```

Or vendored inside a project, so anyone who clones the project gets a
working editor with it:

```sh
git clone https://github.com/paveluv/e ~/git/your_project/.e
rm -rf ~/git/your_project/.e/.git    # make it ordinary files of your repo
~/git/your_project/.e/e file.txt
```

Every installation is self-contained: the loader uses strictly the
`lib/` next to the script itself, compiled objects stay local in `eo/`,
and sources recompile automatically when they (or anything they import)
change. Project-specific modules dropped into a vendored checkout's
`lib/` stay local to that project.

The terminal size is detected via `ioctl` and tracked across resizes;
if that is unavailable, set `LINES` and `COLUMNS`. On FreeBSD, whose
Chez port renames the `scheme-script` interpreter, change the shebang
to `#!/usr/bin/env -S chez-scheme --script` or run
`chez-scheme --script e` (details in the loader's header comment).

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
| `C-x C-b` | Pop up `*Buffer List*`: marks (`.` current, `%` read-only, `*` modified), lines, mode, file |
| `C-x k`   | Kill a buffer (default: current; asks if modified)      |
| `C-x 2`   | Split the current window in two (stacked)               |
| `C-x 1`   | Delete all other windows                                |
| `C-x 0`   | Delete the current window                               |
| `C-x o`   | Move to the next window                                 |

Each window has its own status line, point, and scroll position; the
same buffer can be shown in several windows at once. Windows resize by
dragging a status bar with the mouse, or with
`M-x (resize-window! n)`, which grows the current window by n text
lines (negative shrinks), trading lines with its neighbor; splits halve
the current window, and sizes rescale proportionally when the screen
or the prompt area changes.

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
| `C-_`           | Undo                                          |
| `C-M-_`         | Redo                                          |
| `M-%`           | Query replace from point: `y`/`SPC` replaces, `n`/`DEL` skips, `q`/`RET`/`C-g` stops |
| `M-.`           | Describe the symbol at point (Scheme buffers; see below) |
| `C-l`           | Repaint the screen and re-read its size       |
| `C-g`           | Cancel (prompt, search, mark, running evaluation) |

### Search

`C-s` starts an incremental search: type to extend the needle, `C-s`
again jumps to the next match (wrapping around), Backspace shortens the
needle, `RET`/`ESC` accepts silently, `C-g` cancels and returns point
to where the search began. Point rides just past the current match, so
a mark set before searching leaves the found text inside the region.
All matches on screen are highlighted while searching.

### Prompts

Prompt input is line-editable with the usual bindings (`C-a`/`C-e`,
arrows, Home/End, `C-k`/`C-y` through the shared kill ring), and
completes with TAB: file names in the file prompts, buffer names in the
buffer prompts. TAB extends to the longest common prefix; when nothing
extends, a second TAB pops up a `*Completions*` window, which
disappears when the prompt finishes. Input longer than the screen is
wide wraps onto continuation lines, marked with a trailing `\` and
indented under the prompt text, the windows above shrinking to make
room; at eight lines the prompt area scrolls instead. Long echo-area
messages wrap the same way. In a wrapped prompt the arrows move between
visual lines, turning into history navigation at the top and bottom
edges.

## Evaluating Scheme

`M-x` prompts for an expression (the opening parenthesis is pretyped
and deletable, so a bare symbol works too; missing closing parentheses
are forgiven) and evaluates it in the editor's own top level, where the
whole published API lives.  The expression styles as Scheme while you
type, and the result shows in the echo area, transiently like any
message:

    M-x (buffer-name (current-buffer))
    (buffer-name (current-buffer)) => "sa.txt"

Nothing is lost when it fades: every message that passes through the
echo area lands in the log -- the editor's syslog, structured records
of date, component, and text.  `show-log!` pops up the `*log*` view
(rendered from the records on the fly), `log-entries` returns the
records themselves for slicing from M-x, and `log!` adds your own.
The M-x history is read off the log, so the up arrow recalls past
expressions -- forgiven parentheses included -- across the session.

TAB completes the symbol being typed against everything bound in the
top level: Chez itself, the editor, loaded modules. A unique completion
appends a space so the next argument can start at once. Shift-TAB
completes against only the editor's own definitions, which the
`*Completions*` pop-up also highlights. While you type, the parameters
still to be supplied to the innermost open call appear after the cursor
as a grey suggestion, shrinking as you enter arguments:
`M-x (vector-sort` suggests `predicate vector`. Parameter names come
from procedure sources when available, from documented signatures in
`scheme-sigs.e` for common builtins, or from the arity (`arg1 arg2`).
Up and down arrows browse the history, newest first.

Buffers print as the expression that looks them up: `(current-buffer)`
shows `(buffer "e")`, not the record's contents, so a printed result
pastes straight into the next expression:
`(buffer-line-count (buffer "e"))`. Regions print the same way.

While an expression runs, its full closed form stays in the echo area
with the cursor parked at its end as a blinking underline, so a running
evaluation is visible at a glance. `C-g` interrupts it, even out of one
of its own prompts, logging `interrupted` as the result. An expression
that returns a value pops the transcript up; one that returns nothing
(the interactive commands) is recorded without disturbing whatever it
displayed.

Generic editing helpers live in the `(edit)` module and take an
optional `where` argument. Omitted, it means the selected region or the
whole current buffer; other options are a buffer or its name, a
predicate over buffers, a region, or a list of any of these:

    M-x (replace-all! "xx" "yy")                    ; current buffer
    M-x (replace-all! "xx" "yy" buffer-file)        ; every file buffer
    M-x (count-matches "xx" "notes.md")             ; count, don't edit

Each targeted buffer gets one undo step labeled with the command
itself, and point stays where it was. The interactive `replace!!`
(`M-%`) queries occurrence by occurrence instead, highlighting each,
and prompts for whichever arguments it is not given.

The Scheme manual is available inside the editor:

    M-x (describe eq-hashtable-ref)

pops up a `*describe*` buffer with the entry from the reference
documentation: forms, what it returns, libraries, source, and the full
prose. The corpus covers R6RS (from TSPL4) and the Chez extensions
(from the Chez Scheme User's Guide); run `M-x (fetch-describe-data!)`
once to download and extract it (about 1400 entries, kept in
`describe-data/` next to `lib/`, out of git; the byte transfer uses curl,
everything else is the `describe.e` module itself). The database is structured and queryable from M-x:
`doc-lookup` returns the entries for a name, `doc-entries` all of them
(optionally filtered by a predicate), and the `doc-*` accessors take
entries apart, so the whole corpus can be sliced by source, chapter,
library, or anything else. In a Scheme buffer, `M-.` describes the
symbol the cursor is on.

## Architecture and extension modules

Everything in `lib/` is a library with the `.e` extension, named after
its file: `core.e` is `(core)`, `eval.e` is `(eval)`. The core is a
kernel (buffers, windows, editing, rendering, prompts) resting on
`sys.e`, the system-specific layer (libc, termios, ioctl, signals), the
only library containing foreign procedures. The core's exports are the
editor's published API: internals, all mutable state included, are
invisible outside it and open to compiler optimization, and the exports
are immutable; both guarantees enforced by the language (`set!` on an
exported name raises an exception). The API comprises the command
procedures, read-only state accessors (`current-buffer`, `point`,
`buffer-line`, ...), the editing primitives modules compose commands
from (`goto-point!`; `call-with-buffer`, which runs code with another
buffer temporarily current; `call-as-one-edit!`, which bundles edits
into one undo step), and the extension hooks (`bind-key!`,
`register-mode!`, `add-highlighter!`, `prompt!`, `read-key`,
`reload-module!`, ...). `M-x (` followed by Shift-TAB lists the entire
catalog.

Names follow a contract. A procedure ending in `!!` waits for input
from the user: a prompt, a confirmation, a key query. It takes no
required arguments, reports through the echo area, and returns nothing;
on success it is void, so evaluating one from M-x leaves its display
and message in place, and failures raise, reported in the echo area
(and the log) as errors. A single-`!` verb acts immediately with what it is given
(explicit arguments, a useful return value), even when it displays
something: `list-buffers!` pops up the buffer list but asks nothing, so
one bang. Two bangs ask you things. The `!!` commands are thin wrappers
over primitives (`find-file!!` prompts and calls `visit-file!`), and
keys bind to whichever fits.

An extension module is a library that imports `(core)` and exports an
`init!` performing its registrations:

```scheme
;; lib/my-mode.e
(library (my-mode)
  (export init!)
  (import (chezscheme) (core))
  (define (my-styles s) ...)
  (define (init!) (register-mode! "my" '(".my") '() my-styles)))
```

At startup the core loads every `lib/*.e` library and calls its
`init!`; the loader script is pure bootstrap. Features are built this
way on top of the kernel: M-x itself lives in `eval.e`, the editing
helpers in `edit.e`, bracket matching in `paren.e`. Modules use one
another by ordinary import, and the library system orders
initialization and recompilation accordingly. A module that fails
reports itself in the message line without preventing startup.

Modules reload without restarting. Saving a module source in the
editor (any `.e` file in the running editor's `lib/`, including a new
one) reloads it on the spot; `M-x (auto-reload #f)` turns that off for
a session, commenting the `(auto-reload #t)` line out of the loader
disables it for an installation, and a save whose reload fails reports
the error and leaves the old version running. For sources edited
outside e:

    M-x (reload-module! "paren")

The core redefines the module's library from source, recompiles every
loaded module built on it, and re-runs the `init!`s. Everything a
module registers is tagged with the module that registered it, and a
reload retracts the old registrations first, so re-registration
replaces rather than accumulates, for any present or future hook, with
nothing for module authors to remember. Buffers, windows, and the M-x
top level are untouched; the core itself cannot be reloaded.

Syntax highlighting is provided by modes, registered by modules:

```scheme
(register-mode! "scheme"
  '(".scm" ".ss" ".sls" ".sps" ".sc" ".e")   ; file-name extensions
  '("scheme" "petite" "chez" "guile")        ; #! interpreters, for
  scheme-styles)                             ;   files without an extension
```

A buffer gets the first mode whose extension matches its file name, or
whose interpreter name appears in the file's `#!` line. The styles
function maps a line to a vector of per-column style symbols; brackets
styled `delimiter` take part in bracket matching. Two modes ship in
`lib/`: `scheme-mode.e` and `md-mode.e` (Markdown; indented code
blocks are styled as Scheme, modes composing per line). Context
highlighting, markup that depends on where point is, is provided by
highlighters: `add-highlighter!` registers a function consulted at
every redraw that returns `(row start end)` ranges of the current
buffer to underline. Bracket matching is such a module, `paren.e`;
local-variable or other semantic highlighting could be added the same
way.

## Details worth knowing

- Per-buffer undo history and mark; per-window point and scroll; a
  global kill ring (kill in one buffer, yank in another); buffers
  remember where you were when you come back.
- Bracketed paste: the editor enables it itself, so a paste from
  anywhere is one edit, and one undo step, with no pasted control
  characters running commands; in a prompt, the paste lands whole.
- Mouse: a click focuses the window under the pointer and places point
  at the clicked cell; dragging selects, exactly as though the mark
  were set at the press and point moved (so `C-w`, `C-y`, and friends
  apply); a double click selects the word under the pointer; dragging
  a status bar resizes the windows it separates; the wheel
  scrolls the window under the pointer, wherever the focus is, and
  horizontal wheel ticks move point sideways within its line. The
  terminal's own mouse selection needs Shift held while tracking is
  on; `M-x (mouse! #f)` turns tracking off (and native selection back
  on) for the session; a lingering Shift-selection highlight clears
  with `C-l`.
- Kill ring entries accumulate across consecutive kill commands, so
  `C-k C-k C-k ... C-y` reassembles whole blocks.
- Incremental screen updates: only rows whose content changed repaint,
  single-row scrolls use the terminal's native scroll operation, and
  line styling is memoized per line.
- Files round-trip byte-for-byte, including the presence or absence of
  a trailing newline, and quitting never asks about "modified" buffers
  whose text is identical to the file on disk.

## Limitations

Kept deliberately small: windows split horizontally only (no
side-by-side), and there is no configuration beyond extension modules.
Tabs and other control characters are displayed as a single space
cell. Input is read as UTF-8, but all characters are assumed to be one
column wide.

## Licence

[MIT](LICENCE)
