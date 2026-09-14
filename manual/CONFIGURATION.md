# Configuration

## Startup and head names

Run `./e [--restart [--force]] [--name NAME] [--base-working-dir DIR] [--] [file]`. A name is a nonempty string; quote it
in the shell if it contains spaces. `--name=NAME` also works. `--` allows a
file whose name begins with `-`. Help (`-h` or `--help`) and argument errors
are handled before loading the editor or `config.e`.
Compatibility with a running base is also checked before head imports and
`config.e`; a library-source or wire mismatch prints a restart command and
exits. Configuration files themselves do not participate in the fingerprint.

Without `--name`, the head uses `user@host:tty`, with `pid-N` in place of
the terminal path when there is no terminal. Generated names gain ` 2`,
` 3`, and so on if occupied; an explicitly requested name must be free.
The identity is `(head "name")`, exposed as `head:ui-actor`. It is chosen
before shared buffers or cursor marks are created. Every invocation starts or
attaches to the installation's base. Use a stable explicit name to restore the
same screen after an SSH login whose terminal name has changed.
`--restart` reviews and saves the existing base before restarting and claiming
the requested name. `--force` requires `--restart`; neither combines with
`--base`. See [restart and recovery](MULTIHEAD.md#restart-and-recovery).

## Daemon and attachment

`./e --base [--base-working-dir DIR]` runs the base alone; ordinary `./e`
starts it automatically when necessary. `--base-working-dir` selects a private
directory for an independent base. Running the daemon, attaching and detaching, named screens and their
checkpoints, what is shared between heads, and agent sessions are described
in [Base, heads and agents](MULTIHEAD.md). The daemon reads `base-config.e`
only; a head reads `config.e` only.

Quitting normally detaches the screen and keeps the base running.
Set `(main:shutdown-on-exit #t)` in `config.e` to review shutting down the
base when this is the last attached head. Cancelling the review keeps that
head open. The default is `#f`; the setting accepts only booleans.
`M-x (main:shutdown!!)` requests the same review explicitly, regardless of
the setting. Shutdown requires an all-buffer head connection.

## Configuration file

`config.e`, beside the loader script, is plain Scheme rather than an R6RS
library. Every expression evaluates in the same live top level as `M-x`, with
the complete published editor API available.

The file is intentionally ignored by Git. The repository ships
`config.template.e`, with commented examples of common settings. Most show
defaults; others demonstrate overrides, such as browser commands, styles and
key bindings. The template does not enumerate the complete API:

```sh
cp config.template.e config.e
```

Uncomment only settings that should differ from defaults.

`base-config.e`, also beside the loader and ignored by Git, configures the
base services. It runs once when the base starts, after restoring a saved
session and before accepting any head. Initialization that creates a named
shared buffer can therefore find and reuse a restored one.
It sees base APIs such as `store:`, `policy:`, `vt:` and
`reference:`. Keep key bindings, painting and other head settings in `config.e`.
An attached head evaluates `config.e` using client implementations of its
service APIs; it does not evaluate `base-config.e`. The daemon does not evaluate
`config.e`; forms are never assigned a side by
guessing what they call. Base configuration errors stop startup.

For example, the base retains one million log records by default. Configure
that shared limit in `base-config.e`:

```scheme
(log:retention 1000000)
```

An authorized attached head can change it at runtime with the same call in
M-x. Shrinking expires old records immediately; growing preserves the retained
records and their append bookmarks. Local log views show a recent window
independently of this limit; see [the log manual](LOG.md).

`kernel:config-file` and `kernel:load-config!` accept an optional `base` or
`head` symbol; the default remains `head`. Supported head-app reload reapplies
head configuration. Changes to the base runtime require a restart.

Agent permissions and question routing are base configuration too:
`base:connection-policy` chooses each connecting actor's session policy and
`base:connection-owner` the actor its questions go to by default. Both are
procedure parameters set in `base-config.e`; examples, the session controls
an attached head can run, and the policy API are in
[Agents and sessions](MULTIHEAD.md#agents-and-sessions).

## Loading and reloading

Configuration loads after extension modules at startup, then reapplies after a
module reload so personal choices remain above fresh module defaults. Write it
so evaluating it repeatedly is safe.

Saving `config.e` inside e reloads it immediately. It can also be applied with:

```scheme
(main:load-config!)
```

`main:config-reload-on-save` controls automatic reload. A configuration error is
reported in the echo area and structured log without terminating the editor.

Configuration-owned registrations publish together after the file finishes.
Removing a key binding, style override, or extra mode extension from the file
removes it on the next successful reload. A failed load keeps the previous
registered settings and preserves concurrent runtime registrations. Parameter
assignments and other Scheme effects that ran before the error remain applied.

## Common examples

```scheme
(keymap:bind! "C-c s" save!!)
(keymap:unbind! "C-v")
(mode:add-extension! "scheme" ".foo")
(indent-on-tab! "scheme" #f)
(wrap-lines #f)
(scrollbar #t)
(scrollbar-position 'right)
(line-numbers #f)
(undo-scope 'all) ; include other actors' changes; default is 'mine
(scheme-format:width 100)
(style:set! 'editor '((foreground 135) bold))
```

See [Key binding configuration](KEY_BINDING.md), [Styles](STYLES.md),
[Formatting](FORMATTING.md), and [Buffers](BUFFERS.md) for the relevant APIs
and precedence rules.

## Self-contained installations

Each checkout keeps `config.e`, `base-config.e`, `lib/`, `data/` and compiled
`eo/` beside its own loader. The default base directory is `.base/`, containing
`socket`, `lock`, `pid`, `session` and `log/`; `--base-working-dir` selects another
directory. A project can therefore
vendor a customized e checkout without affecting a personal installation
elsewhere.
