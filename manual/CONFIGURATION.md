# Configuration

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

Configuration-owned registrations are transactional where applicable. For
example, removing a key binding, style override, or extra mode extension from
the file removes it on the next reload rather than leaving stale state behind.

## Common examples

```scheme
(keymap:bind-key! "C-c s" save!!)
(keymap:unbind-key! "C-v")
(mode:add-extension! "scheme" ".foo")
(indent-on-tab! "scheme" #f)
(wrap-lines #f)
(scrollbar #t)
(scrollbar-position 'right)
(line-numbers #f)
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

