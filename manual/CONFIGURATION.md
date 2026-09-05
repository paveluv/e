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
before shared buffers or cursor marks are created. This build still runs
one head in one process; daemon and attach modes are not implemented.

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

Each checkout reads only the `config.e`, `lib/`, `data/`, and compiled `eo/`
beside its own loader. A project can therefore vendor a customized e checkout
without affecting a personal installation elsewhere.
