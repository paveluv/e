# Configuration

## Startup and head names

Run `./e [--name NAME] [--] [file]`. A name is a nonempty string; quote it
in the shell if it contains spaces. `--name=NAME` also works. `--` allows a
file whose name begins with `-`. Help (`-h` or `--help`) and argument errors
are handled before loading the editor or `config.e`.

Without `--name`, the head uses `user@host:tty`, with `pid-N` in place of
the terminal path when there is no terminal. Generated names gain ` 2`,
` 3`, and so on if occupied; an explicitly requested name must be free.
The identity is `(head "name")`, exposed as `head:ui-actor`. It is chosen
before shared buffers or cursor marks are created. The interactive editor
still runs one head in one process; interactive `--attach` is not implemented.

## Daemon foundation

`./e --daemon [--socket PATH]` starts a foreground base without a screen.
The socket defaults to `$XDG_RUNTIME_DIR/e/base`, or `~/.e/base` when that
environment variable is absent. `--socket=PATH` also works. The daemon does
not take a head name or file argument. Run it under a supervisor or as a shell
background job with its output redirected; SIGHUP leaves it running.
SIGTERM or Ctrl-C stops it and its terminal processes. State is in memory
for the life of the daemon; stopping it does not save a session to disk.

Local clients can read, edit and undo according to their session's buffer
permissions, and receive actor mailbox delivery. Interactive attachment and
named screen restoration are still being built. The current protocol and Scheme
client primitives are described in the development
[wire contract](../dev/MULTIHEAD.md#implemented-local-protocol).

Only peers running as the same OS user connect. An existing socket path is
never removed at startup, and a clean stop releases its path. After a crash,
remove a stale socket explicitly after checking that no daemon still uses it.

## Configuration file

`config.e`, beside the loader script, is plain Scheme rather than an R6RS
library. Every expression evaluates in the same live top level as `M-x`, with
the complete published editor API available.

The file is intentionally ignored by Git. The repository ships
`config.template.e`, containing every supported option as a commented example
with its default value:

```sh
cp config.template.e config.e
```

Uncomment only settings that should differ from defaults.

`base-config.e`, also beside the loader and ignored by Git, configures the
base services. It runs once at startup in both plain and daemon modes, before
any head is imported. It sees base APIs such as `store:`, `policy:`, `vt:` and
`reference:`. Keep key bindings, painting and other head settings in `config.e`.
The daemon does not evaluate `config.e`; forms are never assigned a side by
guessing what they call. Base configuration errors stop startup.

`kernel:config-file` and `kernel:load-config!` accept an optional `base` or
`head` symbol; the default remains `head`. Supported head-app reload reapplies
head configuration. Changes to the base runtime require a restart.

The daemon uses `base:connection-policy`, a procedure parameter, to choose a
policy from each connecting actor identity. Heads default to all-buffer write
access; agents default to read-only sessions. For example, in `base-config.e`:

```scheme
(define default-connection-policy (base:connection-policy))
(base:connection-policy
  (lambda (actor)
    (if (equal? actor '(agent "helper"))
        (policy:make '() 10000 '("notes") 8000)
        (default-connection-policy actor))))
```

This grants that named agent edits and undo in `notes`. The resolver receives
an owned identity; the hello carries no permissions. Each connection gets a
new session and disconnect revokes it. The grants/fuel/cap fields concern
session evaluation, which is not yet exposed through the wire.

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

Each checkout reads only the `config.e`, `base-config.e`, `lib/`, `data/`, and compiled `eo/`
beside its own loader. A project can therefore vendor a customized e checkout
without affecting a personal installation elsewhere.
