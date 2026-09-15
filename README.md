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

- Local installation. All code and state are kept in the installation
  directory (where you cloned `e`), including modules, configuration files,
  and the base's session state and logs. To uninstall `e`, simply remove
  the installation directory.
- Multi-head and persistent sessions. On its first invocation, `e` starts a
  daemon base to which multiple heads can connect (local connections only
  for now). Heads detach on exit and reattach on the next invocation.
  Reattaching with the same head name restores its layout. The base owns
  the buffers, terminals, and some app state, so multiple heads can access
  them and edit simultaneously. Work can resume after an SSH disconnect,
  much like with tmux.
- Scheme evaluation. `M-x` evaluates Scheme in the editor's context, with
  structural multiline input, semantic completion, parameter hints, history,
  and captured output (evaluation results, stdout, and stderr are captured
  separately).
- Besides normal editing buffers, there are app buffers. They update their
  presentation from internal structures and can optionally
  interact with the user: logs, Git history, live reference pages and rendered
  Markdown, as well as the file and buffer pickers.
- Easy filesystem navigation. The `files` and `buffers` apps have live,
  filterable tables with multi-column sorting and keyboard or mouse
  navigation. Files supports recursive path matching, clickable directory
  breadcrumbs and explicit file/directory creation.
- Windows form a recursive tiling layout that is easy to reshape: split in
  either direction (`C-x 2`, `C-x 3`) and drag edges with a mouse.
- Terminals. `C-c t` opens a new PTY-backed terminal buffer able to run
  shells, full-screen programs, or another editor such as
  [legmacs](https://github.com/nooga/legmacs). `C-x` and `M-x` reach e by default;
  `C-]` or the clickable `●` / `◐` indicator toggles full / partial capture.
- Undo and redo describe meaningful edits: typed runs, pastes, replacements,
  and formatter passes. Undo targets your own changes by default; choose
  another actor's changes or opt into undoing everyone's work.
- The same shared-state APIs are available to scripted clients for attributed
  edits, change notifications, scoped undo and communication between actors.
  The editor is agent-ready; bundled agent integrations remain deferred.

## Start

### Prerequisites

The core editor needs a threaded build of
[Chez Scheme](https://cisco.github.io/ChezScheme/) 10 or newer, a Unix-like system and a
terminal. Package installation examples:

```sh
# Linux (Debian/Ubuntu)
$ sudo apt install chezscheme

# FreeBSD
$ pkg install chez-scheme

# macOS
$ brew install chezscheme
```

Cloning the repository and the Git browser use the installed `git` command.
HTTPS features, including reference downloads, need OpenSSL's `libssl` or
the optional `curl` backend and trusted CA certificates; see
[HTTPS](manual/HTTPS.md). Opening web links
uses the configured [browser command](manual/MARKDOWN.md#scheme-api).

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

Each installation keeps its libraries, configuration, downloaded data and
compiled objects beside its loader: `lib/`, `config.e`, `data/`, optional
`base-config.e` and `eo/`. The daemon owns `.base/` there, including its
socket, recovery snapshot and daily diagnostic logs, so checkouts have independent settings,
caches and daemons.
`lib/` groups flat-named `.sls` libraries by responsibility.
Base and attached implementations use separate `eo/base/` and `eo/client/`
caches of `.so` objects. The first start compiles the required libraries;
later starts reuse them. Configurations retain the `.e` extension; tools
use `.sps`. See [the module layout](manual/MODULES.md#library-architecture).

On FreeBSD, where Chez installs a differently named script interpreter,
change the shebang as explained in the loader. Automatic base startup
executes that same loader.

### Keep a session across SSH logins

Run this on the host where the files live, each time you log in:

```sh
$ ~/.e/e --name work
```

`C-x C-c` detaches that screen. Attach with the same name to restore its
layout, positions and kill ring; use a different name for an independent
screen. Shared edits and terminal processes continue while no screen is
attached. `M-x (main:shutdown!!)` saves shared text and named views, then
stops the base and all its screens. It asks about local drafts, live processes
and other screens. Set `(main:shutdown-on-exit #t)` in `config.e` to use
this shutdown when the last screen quits.
`e --restart --name work` also starts the replacement base and reattaches.
All graceful stops, including `kill -TERM PID`, use the same save path;
the next start restores the snapshot. Terminal processes, undo history and
local drafts do not survive a stop; terminal text returns as read-only
transcripts.
New heads check library sources and protocol compatibility against the running
base and ask for `e --restart` when they differ.
`e --help` shows the base's status, version, buffer and process counts,
attached and detached screens, and commands to resume them. Starting a new
base prints its details and stop options before the screen opens; detaching
prints only the session summary.
Connections currently use a local Unix socket under
the same OS user. See [the base and its heads](manual/MULTIHEAD.md#the-base-and-its-heads)
for configuration, lifecycle and scripted clients.

## Essential keys

| Key | Action |
|---|---|
| `C-x C-f` | Browse and recursively filter files in the `<files>` app |
| `C-x C-s` | Save |
| `C-x C-w` | Save as |
| `C-x C-c` | Detach this screen; keep shared buffers and terminals running |
| `C-x b`, `C-x C-b` | Filter and switch buffers; Enter initially selects the previous buffer |
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

In Files, type to filter, use Left/Right to navigate directories and Enter
to open a row. `M-c` enters Create mode: Enter creates the typed file, or
directories when the path ends in `/`. Existing names are refused. In both
Files and Buffers, click column headings or use `F1`–`F6` to cycle ascending,
descending and off, with multiple sort keys in the order you add them.
The original path-entry command remains available as `M-x (find-file!!)`.

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
- [Files app](manual/FILES.md): directory navigation, recursive path filtering,
  completion, explicit creation, match counts and sortable filesystem metadata.
- [Base, heads and agents](manual/MULTIHEAD.md): the daemon, attaching and
  reattaching named screens, what is shared and what is local, questions
  between actors, and the agent-ready session and permission APIs.
- [Evaluation](manual/EVAL.md): M-x, `eval:run!`, multiline commands, output
  capture, interruption, history, and result copying.
- [Terminal buffers](manual/TERMINAL.md): full/partial capture, emulation, scrollback,
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

Development notes live in `dev/`. Start with the
[task tracker and tech-debt ledger](dev/V2_TASKS.md) for current status and
links to the design, implementation records and deferred work.

## Limits

Tabs and other control characters display as one space cell. Input is UTF-8,
and buffer cursor, wrapping, selection, and mouse geometry account for wide
characters and combining/emoji clusters. Widths use the host locale and the
same glyph rules as the terminal emulator.

## Version history

- **Current development version (unreleased)** -- a persistent daemon with named screens,
  saved-session recovery and reviewed restart; shared terminals and attributed undo;
  agent-ready APIs; interactive Files
  and Buffers apps with filtering, compound sorting and per-window column
  widths. The repository uses the settled R6RS library layout.
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
