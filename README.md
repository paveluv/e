# e

e is a tiny, fully customizable, self-aware, Emacs-like console editor written
in [Chez Scheme](https://cisco.github.io/ChezScheme/). It feels like Emacs in
many ways (buffers, recursive window splits, incremental search, built-in
Scheme), but it's not trying to be an Emacs clone.

It's "self-aware" because it knows its own internals (like most Lisp systems).
It's "fully customizable" because its code is just one big configuration
(code is data :)

The editor is itself a Scheme system. It's a set of R6RS libraries, all of
which, except the kernel and the main loop, are hot-reloadable. Editing and saving a module from
within e applies the changes immediately.

Highlights:

- `M-x` evaluates Scheme in the editor's context, with structural multiline
  input, semantic completion, parameter hints, history, and captured output
  (evaluation results, stdout, and stderr are captured separately).
- Besides normal editing buffers, there are app buffers. They update their
  presentation from internal structures and can optionally
  interact with the user (`*log*`, `*git-log*`, `*buffers*`, `*terminal*`).
- Windows form a recursive tiling layout that is easy to reshape: split in
  either direction (`C-x 2`, `C-x 3`) and drag edges with a mouse.
- `C-c t` turns the current buffer into a PTY-backed terminal able to run
  shells, full-screen programs, or another editor such as
  [legmacs](https://github.com/nooga/legmacs).
- Undo and redo describe meaningful edits: typed runs, pastes, replacements,
  and formatter passes.
- There are no dependencies beyond Chez Scheme, a Unix-like terminal, and the
  system `libc.so`. The first start automatically compiles its libraries; later
  starts are typically around 100 ms.

## Start

### Prerequisites

The only dependency is [Chez Scheme](https://cisco.github.io/ChezScheme/)
(a threaded build, which is what every package below ships):

```sh
# Linux (Debian/Ubuntu)
$ sudo apt install chezscheme

# FreeBSD
$ pkg install chez-scheme

# macOS
$ brew install chezscheme
```

### Install

e is installed from source in your home directory. There are no prebuilt
binaries or packages; stable Git tags mark the releases.

Install as a personal editor:

```sh
$ git clone --branch v0.1 https://github.com/paveluv/e ~/.e
$ ~/.e/e file.txt
```

Or vendor it inside a project so the editor and project-specific extensions
travel with the source:

```sh
$ git clone --branch v0.1 https://github.com/paveluv/e ~/git/project/.e
$ rm -rf ~/git/project/.e/.git
$ ~/git/project/.e/e file.txt
```

Each installation is self-contained. It uses the `lib/`, `config.e`, `data/`,
and compiled `eo/` beside its own loader.

On FreeBSD, where Chez installs a differently named script interpreter, run
`chez-scheme --script e` or change the shebang as explained in the loader.

## Essential keys

| Key | Action |
|---|---|
| `C-x C-f` | Find a file |
| `C-x C-s` | Save |
| `C-x C-w` | Save as |
| `C-x C-c` | Quit safely |
| `C-x b` | Switch buffers by name |
| `C-x C-b` | Switch through the interactive `*buffers*` table |
| `C-x 2`, `C-x 3` | Split the current window below or right |
| `C-x 0`, `C-x 1` | Delete this window or every other window |
| `M-Arrows` | Move between windows along the cursor's screen ray |
| `C-s` | Incremental search |
| `M-%` | Query replace |
| `C-_`, `C-M-_` | Undo, redo |
| `C-@`, `C-w`, `M-w`, `C-y` | Mark, kill, copy, yank |
| `M-x` | Evaluate Scheme interactively |
| `C-x C-e` | Evaluate the current buffer as Scheme |
| `C-h f`, `M-.`, `C-h k` | Describe a name, symbol at point, or key |
| `C-x g` | Browse Git history and patches |
| `C-c t` | Open a terminal buffer |
| `C-g`, Escape | Cancel the current interaction |

## Scheme at the center

The live top-level environment exposes the published editor API and loaded
modules. You can call it via `M-x`:

```scheme
M-x (buffer-name (current-buffer))
M-x (replace-all! "old" "new" buffer-file)
M-x (log-view 'eval)
M-x (terminal!!)
M-x (describe terminal!!)
```

(The double-bang suffix `!!` means that the command is interactive.)

Configuration is Scheme too. Copy `config.template.e` to `config.e`, uncomment
the settings worth changing, and save it; the running editor applies it
immediately.

## Documentation

- [Buffers and windows](manual/BUFFERS.md): files, splits, scrolling, line
  numbers, scrollbars, mouse behavior, `*buffers*`, and the buffer API.
- [Evaluation](manual/EVAL.md): M-x, `eval!`, multiline commands, output capture,
  interruption, history, and result copying.
- [Terminal buffers](manual/TERMINAL.md): capture, escape, emulation, scrollback,
  titles, process lifetime, and the terminal API.
- [Search and replacement](manual/SEARCH.md): incremental search, smart case,
  query replace, and structured replacement targets.
- [Indentation and formatting](manual/FORMATTING.md): Scheme layout, conservative
  and intrusive formatting, save hooks, and the CLI formatter.
- [Interactive prompts](manual/PROMPTS.md): editing, multiline input, completion,
  styling, suggestions, and prompt APIs.
- [Echo area and log](manual/LOG.md): structured messages, progress, histories,
  formatters, and dynamic log views.
- [Describe](manual/DESCRIBE.md): live reference pages, key discovery, corpus
  installation, structured queries, and module-published documentation.
- [Configuration](manual/CONFIGURATION.md): reload semantics, precedence, and
  common settings.
- [Key bindings](manual/KEY_BINDING.md): key syntax, contextual maps, overrides,
  unbinding, and inspection.
- [Styles](manual/STYLES.md): the style DSL, faces, colors, terminal behavior, and
  configuration lifecycle.
- [App buffers](manual/APPS.md): dynamic views, interaction, the escape
  prefix, mouse events, and the `*buffers*` switcher.
- [Git](manual/GIT.md): structured repository queries and the history browser.
- [Pretty Scheme](manual/PRETTY_SCHEME.md): structural delimiter glyphs, depth
  and rainbow variants, and semantic symbol styling.
- [Modules and architecture](manual/MODULES.md): library boundaries, hot reload,
  registrations, modes, highlighters, and extension conventions.

Development notes live in `dev/`, apart from the manual: the v2 design
(`dev/DESIGN2.md`), the migration tracker with its tech-debt ledger
(`dev/V2_TASKS.md`), the dead-code and debugging-lessons ledgers, and the
terminal test notes.

## Limits

Tabs and other control characters display as one space cell. Input is UTF-8,
but e currently assumes every character occupies one terminal column.

## Version history

- **v0.1** (2026-09-01) -- the first tagged release. The core editor:
  buffers, recursive tiling windows, incremental search and query
  replace, meaningful undo, mouse support, styles. `M-x` with semantic
  completion and captured output; hot-reloadable extension modules;
  app buffers; a VT-conformant terminal emulator (`C-c t`); the
  describe reference corpus; a read-only markdown view; the Git
  history browser; Scheme indentation and formatting; an FFI HTTPS
  client with certificate verification and a curl fallback. Runs on
  Linux, FreeBSD, and macOS.

## How to contribute

Prototype a feature (AI agents are welcome) and submit a pull request. Don't
worry about whether it's well designed or tested. Most likely, I'll merge it
and then rewrite it.

Or just open an issue on GitHub.

## Licence

[MIT](LICENCE)
