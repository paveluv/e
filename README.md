# e

A tiny Emacs-like console editor written in Chez Scheme.

The editor is a set of R6RS libraries in `lib/` — a deliberately minimal
core plus extension modules — started by the small loader script `e`,
which finds them next to itself. No dependencies beyond
[Chez Scheme](https://cisco.github.io/ChezScheme/) and a Unix-like
terminal, and no installation steps: a checkout runs in place.

Repository: <https://github.com/paveluv/e>

## Installation

Two recommended ways:

1. **As your home editor** — clone the repository directly as `~/.e` and
   use it for all your projects:

   ```sh
   git clone https://github.com/paveluv/e ~/.e
   ~/.e/e file.txt        # add ~/.e to PATH for plain `e`
   ```

2. **Inside a project** — include it in the project's root and use it
   from there:

   ```sh
   git clone https://github.com/paveluv/e ~/git/your_project/.e
   rm -rf ~/git/your_project/.e/.git   # vendor: make it part of your project
   ~/git/your_project/.e/e file.txt
   ```

   Copying is the point: with `.e/.git` removed, `git add .e` makes the
   editor ordinary files of *your* repository, so anyone cloning your
   project gets a working editor with it — decentralized: projects come
   with their own editor. Project-specific extension modules dropped
   into that checkout's `lib/` stay local to the project, and the
   shipped `.gitignore` keeps the compiled `eo/` objects out of your
   repository too. (Without removing `.e/.git`, git records the
   directory as an empty submodule reference — fine if you *don't* want
   the editor in your repo; add `.e/` to your `.gitignore` and `git
   pull` inside it for updates.)

(The loader uses the `lib/` next to the script itself, falling back to
`~/.e/lib` — so a copied or symlinked `e` script alone still finds your
home installation.)

## Running

```sh
./e file.txt
```

The first run compiles the libraries into `eo/` next to `lib/` (a few
hundred milliseconds); afterwards the editor starts from the compiled
`*.eo` objects in ~115 ms, recompiling automatically whenever a source
file — or a library it imports — changes. Compiled objects are always
local to their installation. This is Chez's own library machinery
(`compile-imported-libraries`); the loader contains no build logic.

The terminal size is detected via `ioctl` (and tracked across window
resizes); if that is unavailable, set the `LINES` and `COLUMNS` environment
variables.

## Architecture and extension modules

Everything in `lib/` is a library with the `.e` extension, named after
its file: `core.e` is `(core)`, `eval.e` is `(eval)`. The core is a
kernel — buffers, windows, editing, rendering, prompts — whose exports
are the editor's *published API*: internals (including all mutable
state) are invisible outside it, open to compiler optimization, and the
exports are immutable; both guarantees enforced by the language (`set!`
on an exported name raises an exception). The API comprises the command
procedures (everything key-bound: `visit-file!`, `show-buffer!`,
`split-window!`, …), read-only state accessors (`current-buffer`,
`buffer-list`, `buffer-name`, `buffer-text`, …), and the extension hooks
(`bind-key!`, `register-mode!`, `prompt!`, `confirm?`, `set-message!`,
`echo!`, `buffer-append!`, `call-with-interrupt`, plus a few
string/vector utilities). `M-x (` followed by Shift-TAB lists the entire
catalog.

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

The loader imports every `lib/*.e` library and calls its `init!`.
Features are built this way on top of the kernel: M-x itself lives in
`eval.e`, which binds its key with `bind-key!`, keeps its transcript with
`buffer-append!`, and interrupts runaway evaluations through the core's
generic `call-with-interrupt` (which a future shell module can reuse).
Modules use one another by ordinary import — `scheme-sigs.e` does
`(import (only (eval) register-signatures!))` — and the library system
orders initialization and recompilation accordingly (the `only` matters:
a library body may not shadow an imported name, and every module exports
`init!`). A module that fails reports itself in the message line without
preventing startup.

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
back to counting every bracket. Two modes ship in `lib/`: `scheme-mode.e`
(Scheme, by extension or `#!` interpreter) and `md-mode.e` (Markdown:
headings, blockquotes, lists, fences, rules, inline code, bold, italics,
links).

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

    M-x (buffer-name (current-buffer))

Each exchange is appended to a read-only `*eval*` buffer, which pops up
in another window (without stealing focus) if not already visible:

    [1]> (+ 1 2)
    3
    [2]> (buffer-name (current-buffer))
    "e"

Entries are numbered from 1 (restarting if the buffer is killed), the
expression shows any parentheses that were auto-closed for you and is
syntax-highlighted, and results — errors included — appear beneath it,
unprefixed, in a distinct color.

A runaway expression (an infinite loop, say) can be interrupted with
`C-c`: during evaluation the terminal is allowed to deliver SIGINT,
which unwinds the evaluation and logs `interrupted` as its result; the
editor carries on. (A blocking foreign call — `system`, a blocked read —
cannot be interrupted this way.) Outside evaluation `C-c` is an ordinary
key, so `C-x C-c` still quits. The up and down arrows browse
previously evaluated expressions, newest first; going back down past the
newest restores whatever you had been typing.

TAB completes the symbol being typed against everything bound in the
editor's top level — Chez itself, the editor's own functions, and any
loaded modules — with the same longest-common-prefix and `*Completions*`
pop-up behavior as the file and buffer prompts. In the pop-up, symbols
the editor itself defines are highlighted, making its internals easy to
spot; Shift-TAB completes against *only* those symbols (on an empty
input it lists everything the editor defines). While you type, the
parameters still to be supplied to the innermost open call appear after
the cursor as a grey suggestion, shrinking as you enter arguments:
`M-x (vector-sort` suggests `predicate vector`; after typing the
predicate only `vector` remains. Parameter names come from the source
for procedures loaded from it, from the `scheme-sigs.e` module's
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
