# e

e is a tiny, fully customizable, self-aware, Emacs-like console editor written
in [Chez Scheme](https://cisco.github.io/ChezScheme/). It feels like Emacs in
many ways (buffers, recursive window splits, incremental search, built-in
Scheme), but it's not trying to be an Emacs clone.

It's "self-aware" because it knows its own internals (like most Lisp systems).
It's "fully customizable" because its code is just one big configuration
(code is data :)

The editor is itself a Scheme system built from R6RS libraries. Editing and
saving a reloadable extension from within e applies its changes immediately.
The kernel, base services, main loop and their linked libraries require a
restart; see [the reload boundary](manual/MODULES.md#hot-reload).

Highlights:

- `M-x` evaluates Scheme in the editor's context, with structural multiline
  input, semantic completion, parameter hints, history, and captured output
  (evaluation results, stdout, and stderr are captured separately).
- Besides normal editing buffers, there are app buffers. They update their
  presentation from internal structures and can optionally
  interact with the user (`<log>`, `<git-log>`, `<buffers>`, `*terminal*`).
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
$ git clone https://github.com/paveluv/e ~/.e
$ ~/.e/e file.txt
```

Or vendor it inside a project so the editor and project-specific extensions
travel with the source:

```sh
$ git clone https://github.com/paveluv/e ~/git/project/.e
$ rm -rf ~/git/project/.e/.git
$ ~/git/project/.e/e file.txt
```

Each installation is self-contained. It uses the `lib/`, `config.e`, `data/`,
optional `base-config.e`, and compiled `eo/` beside its own loader.
On this branch, `lib/` groups flat-named `.sls` libraries by responsibility.
Base and attached implementations use separate `eo/base/` and `eo/client/`
caches of `.so` objects. Configurations retain the `.e` extension; tools use
`.sps`. See [the module layout](manual/MODULES.md#library-architecture).

This branch supports [daemon attachment](manual/MULTIHEAD.md#running-a-daemon-and-attaching):
`e --daemon` keeps shared buffers and terminals alive; `e --attach` opens a
head on it, and `C-x C-c` detaches that head. Several screens can edit together.
Reattaching with the same `--name` restores its layout, positions and kill text,
including after an abrupt disconnect. Scripted clients can cooperate while
heads are absent and leave questions for a named head to answer on return.
They share the granted read-only evaluator; an attached human can inspect
sessions and revoke an agent's connection.
Actual agent provider integrations are deferred.

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
| `C-x C-b` | Switch through the interactive `<buffers>` table |
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
| `C-c a` | Answer a question another actor left for you |
| `C-g`, Escape | Cancel the current interaction |

## Scheme at the center

The live top-level environment exposes the published editor API and loaded
modules. You can call it via `M-x`:

```scheme
M-x (head:buffer-name (current-buffer))
M-x (replace-all! "old" "new" head:buffer-file)
M-x (log-view:buffer 'eval)
M-x (terminal:open!!)
M-x (describe:this terminal:open!!)
```

(The double-bang suffix `!!` means that the command is interactive. Every
library but the command layer is seen under its prefix -- `head:`, `log-view:`,
`terminal:` -- and the command layer's names are bare.)

Configuration is Scheme too. Copy `config.template.e` to `config.e`, uncomment
the settings worth changing, and save it; the running editor applies it
immediately.

## Documentation

- [Buffers and windows](manual/BUFFERS.md): files, splits, scrolling, line
  numbers, scrollbars, mouse behavior, `<buffers>`, and the buffer API.
- [Base, heads and agents](manual/MULTIHEAD.md): the daemon, attaching and
  reattaching named screens, what is shared and what is local, questions
  between actors, agent sessions and permissions.
- [Evaluation](manual/EVAL.md): M-x, `eval:run!`, multiline commands, output
  capture, interruption, history, and result copying.
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
- [Markdown viewing](manual/MARKDOWN.md): the rendered companion of a
  Markdown buffer, links, tables, and the viewer API.
- [Configuration](manual/CONFIGURATION.md): startup options and head names,
  `config.e` and `base-config.e`, reload semantics, precedence, and common
  settings.
- [Key bindings](manual/KEY_BINDING.md): key syntax, contextual maps, overrides,
  unbinding, and inspection.
- [Styles](manual/STYLES.md): the style DSL, faces, colors, terminal behavior, and
  configuration lifecycle.
- [App buffers](manual/APPS.md): dynamic views, interaction, the escape
  prefix, mouse events, and the `<buffers>` switcher.
- [Git](manual/GIT.md): structured repository queries and the history browser.
- [Pretty Scheme](manual/PRETTY_SCHEME.md): structural delimiter glyphs, depth
  and rainbow variants, and semantic symbol styling.
- [HTTPS](manual/HTTPS.md): the HTTP(S) client, its TLS transport, the curl
  backend, and portability notes.
- [Modules and architecture](manual/MODULES.md): the library layout, hot
  reload, registrations, modes, highlighters, and extension conventions.

Development notes live in `dev/`, apart from the manual: design notes, the
task tracker with its tech-debt ledger, the dead-code and debugging-lessons
ledgers, and the terminal test notes.

## Limits

Tabs and other control characters display as one space cell. Input is UTF-8,
and buffer cursor, wrapping, selection, and mouse geometry account for wide
characters and combining/emoji clusters. Widths use the host locale and the
same glyph rules as the terminal emulator.

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
