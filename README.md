# e

A tiny console text editor written in
[Chez Scheme](https://cisco.github.io/ChezScheme/). It is Emacs-like in
spirit—buffers, windows, a kill ring, incremental search, and a Scheme
top level—but it is not an Emacs clone. It borrows the ideas that stay
small and stops there.

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
- No dependencies beyond Chez Scheme and a Unix-like terminal. There
  is no build or installation step: a checkout runs in place. The
  first start compiles the libraries; later starts take about 100 ms.
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
| `C-x C-w` | Save as: prompt for a path, the buffer visits it      |
| `C-x C-f` | Visit a file in its own buffer (creating if needed)   |
| `C-x C-c` | Quit (asks only if some buffer differs from its file) |

### Buffers and windows

| Key       | Action                                                  |
|-----------|---------------------------------------------------------|
| `C-x b`   | Switch buffer (default: most recent other; new name creates a buffer) |
| `C-x C-b` | Pop up `*Buffer List*`: marks (`.` current, `%` read-only, `*` modified), lines, mode, file |
| `C-x k`   | Kill a buffer (default: current; asks if modified)      |
| `C-x 2`   | Split the current window in two (stacked)               |
| `C-x 3`   | Split the current window in two (side by side)          |
| `C-x 1`   | Delete all other windows                                |
| `C-x 0`   | Delete the current window                               |
| `C-x o`   | Move to the next window                                 |
| `C-x t`   | Toggle soft-wrapping of long lines in this window       |

Each window has its own status line, point, and scroll position; the
same buffer can be shown in several windows at once. `C-x 3` puts
windows side by side—columns sharing a band of rows, with a grey divider
between them, each wrapping at its own width; `C-x 2` under such a
band splits below the whole band. Columns repaint when they scroll;
on a terminal with VT420 left/right margins (xterm, iTerm2, WezTerm,
foot, ...), `(column-native-scroll #t)` in config.e lets them scroll
natively like full-width windows. `M-x (probe-terminal!)` detects
the support cooperatively (asking the terminal, and you, when its
protocol is silent) and offers to record the setting in `config.e`.
Windows resize by dragging a status bar or column divider with the
mouse, or with `M-x (resize-window! n)`, which grows the current window
by `n` text lines (a negative `n` shrinks it), trading lines with its
neighbor. Splits halve
the current window, and sizes rescale proportionally when the screen
or the prompt area changes.

`(scroll-margin 8)` (the default) keeps at least eight rows between the
cursor and the window's top and bottom edges: the
view scrolls that early, and the cursor enters the zone only where
the buffer's ends leave nothing to scroll.  PageUp and PageDown, like
all vertical movement, shift the view through the terminal's native
scrolling.

Long lines soft-wrap onto continuation rows, Emacs-style: a wrapped
row ends in a grey `\`, and with wrapping off a line running past the
right edge ends in a grey `$` (the window then scrolls horizontally to
follow point).  Wrapping is on by default -- `(wrap-lines #f)` in
config.e flips the default -- and `C-x t` (`wrap!`) toggles it per
window.

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
| `TAB`           | Indent the line by its mode's rules           |
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
The needle's matches in the current window are highlighted while
searching. Matching folds case the smart way, as in Emacs: it
ignores case only while the needle is all lowercase, and one typed
capital makes it exact (the prompt then reads `I-search (exact):`).
`M-c` toggles the current search either way, and `(search-fold-case
#f)` in `config.e` makes every search exact. Everything else—including
`M-%` query replace—always matches exactly.

### Indentation and formatting

Indentation is the one thing enforced—spacing within a line is the
author's (tables, alignment, and hand layout communicate structure),
and lines are never joined or split.  The Scheme rules are
Emacs-like: body forms (`define`, `lambda`, the `let` family, `when`,
...) indent their bodies two columns past the opener, a lone closer
sits under its opener, and an application's continuation lines have
two *stops*: two past the opener, or aligned under the first
argument when it shares the opener's line. The author picks:

    (very-long-function-name param1 param2
      param3)
    (very-long-function-name param1 param2
                             param3)

`TAB` indents the current line, cycling through its stops -- the
nearest stop to the right, wrapping around -- and puts point on the
indentation (a blank line pads out to it); modes opt in when they
register an indenter, and scheme-mode does.  `indent-region!` and
`indent-buffer!` indent line by line from the top as one undo step,
each line settling on the stop nearest its current indentation: on a
stop it stays, before the first it takes the first, past the last the
last.  `format-region!` and `format-buffer!` additionally widen tabs
outside strings to spaces (`(scheme-tab-width 2)`; `#f` keeps tabs),
trim trailing whitespace, drop trailing blank lines so the file ends
with exactly one newline, and apply the bracket conventions -- `[ ]`
for the bindings of the `let` family, `do`, `parameterize`, and
`with-syntax`, and for the clauses of `cond`, `case`, `case-lambda`,
`guard`, `syntax-rules`, and `syntax-case`; `( )` everywhere else --
the `scheme-format-brackets` parameter turns that off.  A region
format only rewrites bracket pairs it wholly contains, so it can
never unbalance the buffer; string interiors and margin comments are
never touched.  Modules provide the rules per mode with
`register-indenter!` and `register-formatter!`. The Scheme engine is
the pure `(scheme-format)` library, which also drives
`tools/scheme-format`: the same treatment from the shell with
`scheme-format [-i] [file ...]`, or stdin to stdout without arguments.
With `(scheme-format-on-save #t)`
(on by default) every save of a Scheme buffer formats it
first through a pre-save hook; modules add their own with
`add-pre-save-hook!` and `add-post-save-hook!` (the module reload is
a post-save hook).

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
    eval: (buffer-name (current-buffer)) => "sa.txt"

The echo area is a transient tail of the log: each message a command
logs stacks on its own line, prefixed with its component in grey, so a
save that reloads a module and then reports a merge shows all three
lines at once.  The area grows as messages arrive, the windows above
shrinking down to their minimum, past which the oldest lines give way;
the next keystroke settles it back to a single line.  A message logged
under `(parameterize ([message-progress #t]) ...)` is progress: it
supersedes its component's newest line instead of stacking -- never
another component's -- so a download reports fourteen steps as one
line redrawn in place, while `*log*` still records every step.

Nothing is lost when it settles: every message that passes through the
echo area lands in the log -- the editor's syslog, structured records
of time (nanosecond precision), component, and text.  The `*log*`
buffer is a *view*: rendered from the records on the fly, always in
the buffer list (`C-x b *log*`), read-only, refreshed while visible,
wearing `[]` in its status bar where ordinary buffers show `--`.
With the cursor at the end (`M->`) it tails the log; anywhere else
the viewport holds still while records arrive.  `(log-view 'eval)`
makes a filtered view -- `*log eval*` -- on the fly.  Entries are
structured data with registered presentation: a module registers a
formatter (and optionally a styler) for its component, used
identically in the echo area and the view -- eval logs the pair
`(query . result)`, formatted `query => result` with Scheme
highlighting, while its history reads just the queries.  Messages
set with a `message-source` of `#f` are indicators -- shown like a
CapsLock light, never logged.  `log-entries` returns the records
themselves for slicing from M-x, `log-history` derives a command
history from any component (find-file and save-as browse their past
paths this way), and `log!` adds your own.

TAB completes the symbol being typed against everything bound in the
top level: Chez itself, the editor, loaded modules. A unique completion
appends a space so the next argument can start at once. Shift-TAB
completes against only the editor's own definitions, which the
`*Completions*` pop-up also highlights. While you type, the parameters
still to be supplied to the innermost open call appear after the cursor
as a grey suggestion, shrinking as you enter arguments:
`M-x (vector-sort` suggests `predicate vector`. Parameter names come
from procedure sources when available, from documented signatures in
the describe corpus for the documented builtins (generated into
`data/describe/signatures.sdata`), or from the arity (`arg1 arg2`).
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
prose. The corpus covers R6RS (from TSPL4) and Chez extensions
(from the Chez Scheme User's Guide). Run `M-x (fetch-describe-data!)`
once to download and extract it: about 1,400 entries, kept out of Git in
`data/describe/` next to `lib/`. The download uses curl; `describe.e`
does the rest. The database is structured and queryable from M-x:
`doc-lookup` returns the entries for a name, `doc-entries` all of them
(optionally filtered by a predicate), and the `doc-*` accessors take
entries apart, so the whole corpus can be sliced by source, chapter,
library, or anything else. In a Scheme buffer, `M-.` describes the
symbol the cursor is on.

## Pretty parens

`M-x (pretty-scheme-clusters!)` toggles the current buffer into a mode
in which each construct's parentheses appear as the Unicode pair
assigned to its cluster, while the buffer and file retain plain ASCII:

    ｢define (twice x)
      ⟨let ([y (* x 2)])
        ⦅if (odd? y) y ⟦begin x⟧⦆⟩｣

Definitions wear `｢ ｣`, lambdas `⸦ ⸧`, the `let` family `⟨ ⟩`,
conditionals `⦅ ⦆`, control `⟦ ⟧`, iteration `⟅ ⟆`, syntax `⧼ ⧽`,
modules `⸨ ⸩`, quoting `‹ ›`, `set!` `⧘ ⧙`, `delay` `⌊ ⌋`.
Applications and clause brackets stay plain, so the glyphs mark
structure without drowning it. The display follows the code as you
type: the glyph appears when the operator completes and flips if you
edit `define` into `cond`.  Typing `)` or `]` closes the innermost
open construct with whatever character the source opened it with, as
the REPL does, and the status line shows the source character under
the cursor.  The cluster table lives at the top of `pretty-scheme.e`
as literal glyphs next to a palette of spare pairs; with auto-reload
on, editing it re-skins every pretty buffer on save.

Two sibling variants trade the cluster rule for nesting depth:
`M-x (pretty-scheme-depth!)` rotates the glyph pairs as the tree deepens
(cycling when the twelve run out), and `M-x (pretty-scheme-rainbow!)` keeps
the plain characters but colors each depth through a seven-color
rainbow, closers mirroring their openers.

Two style rules ride along in every Scheme buffer: symbols the
standard language does not know render italic (a local, a
program-defined name, or a typo), and in eval contexts, which run in
the editor's own top level, the editor's names take their own style
-- so at the M-x prompt, plain is standard Scheme, purple is the
editor, italic is nobody's.

## Integrity

The editor never quietly overwrites work -- yours or anybody's.
Every file buffer remembers the disk state it last agreed with: at
the start of each edit session (one undo entry -- an unbroken typed
run checks once), a detected external change marks the buffer with a
red `!!` in its status bar -- no interruption, just the warning --
and a mere `touch` passes silently because content, not clocks, is
what counts. Saving re-reads the file and compares contents; if
somebody changed it meanwhile, the save stops and asks: `o)verwrite,
m)erge, c)ancel`.  Merge is a three-way merge (the in-house `diff.e`,
a patience diff) of what you loaded, what you have, and what the disk
says: changes on one side apply silently, and colliding ones become
`<<<<<<< buffer / ======= / >>>>>>> disk` markers in the buffer --
the save waits until you resolve, the status bar counting the
remaining conflicts in red. `M-n` (`next-conflict!`) hops
between them; `M-m` (`keep-mine!`) and `M-d` (`keep-disk!`) settle
the one at point, each a single undo step.  A clean merge saves both
sides in one go.  Every merge writes a unified-diff-style report to a
read-only `*merge-<name>*` buffer, named in the echo once the merge is
done -- immediately for a clean one, after the resolving save for a
conflicted one.

## Configuration

`config.e` next to the loader script is plain Scheme—no library,
no shebang: every expression evaluates in the editor's top level, the
M-x environment, with the whole public API in scope. It loads at
startup once the modules are up, and loads again after every module
reload so its settings reapply on top of fresh registrations -- write
it to tolerate being loaded any number of times.  Saving it inside
the editor applies it on the spot, as does M-x `(load-config!)`; an
error reports in the echo area and leaves the editor running.

`config.e` itself is not in version control -- git ignores it, so
personal settings stay inside the directory without ever showing up
in a diff.  What ships is `config.template.e`: every option as a
commented line showing its default (the editor behaves identically
with or without it).  Copy it to get started:

```sh
cp config.template.e config.e
```

then uncomment a line and change its value to disagree with a default
-- for example, `(indent-on-tab! "scheme" #f)` turns TAB's
auto-indent off for Scheme buffers.

## Architecture and extension modules

Everything in `lib/` is a library with the `.e` extension, named after
its file: `core.e` is `(core)`, `eval.e` is `(eval)`. The core is a
kernel (buffers, windows, editing, rendering, prompts) resting on
`sys.e`, the system-specific layer (libc, termios, ioctl, signals), the
only library containing foreign procedures. The core's exports are the
editor's published API: internals, including all mutable state, are
invisible outside it and open to compiler optimization, while exports
are immutable. The language enforces both guarantees (`set!` on an
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
on success it returns void, so evaluating one from M-x leaves its display
and message in place, and failures raise, reported in the echo area
(and the log) as errors. A single-`!` verb acts immediately on what it is given
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
one) reloads it on the spot. `M-x (modules-reload-on-save #f)` turns that
off for a session; `(modules-reload-on-save #f)` in `config.e` disables
it for an installation. `config-reload-on-save` controls `config.e`'s
own on-save reload in the same way. If a reload fails, the editor reports
the error and keeps the old version running.
For sources edited outside e:

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
  '("scheme" "petite" "chez" "guile" "racket") ; #! interpreters for
  scheme-styles)                                ; files without an extension
```

A buffer gets the first mode whose extension matches its file name, or
whose interpreter name appears in the file's `#!` line. The styles
function maps a line to a vector of per-column style symbols; brackets
styled `delimiter` take part in bracket matching. Three file-detected
syntax modes ship in
`lib/`: `scheme-mode.e`, `c-mode.e` (block comments span lines through
a memoized whole-buffer scan), and `md-mode.e` (Markdown; indented
code blocks are styled as Scheme, modes composing per line). Context
highlighting, markup that depends on where point is, is provided by
highlighters: `add-highlighter!` registers a function consulted at
every redraw that returns `(row start end)` or `(row start end style)`
ranges of the current buffer to mark up -- `mark` (the default)
underlines, `match` and `match-point` are the search's cyan and yellow
backgrounds. Bracket matching is such a module, `paren.e`, and the
incremental search itself is another, `search.e`; local-variable or
other semantic highlighting could be added the same way.

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

The window layout is deliberately simple: horizontal bands may contain
side-by-side columns, but it is not a general-purpose tiling system.
Tabs and other control characters are displayed as a single space cell.
Input is read as UTF-8, but every character is assumed to occupy one
terminal column.

## Licence

[MIT](LICENCE)
