# Architecture and extension modules

## Library architecture

Everything in `lib/` is an R6RS library using the `.e` extension. `core.e` is
`(core)`, `eval.e` is `(eval)`, and so on. The loader is only bootstrap: it
locates the adjacent libraries and compiled-object directory, configures Chez,
imports core, and starts the editor.

Core is a generic kernel for buffers, windows, editing, rendering, prompts,
apps, and registration. `sys.e` owns libc, termios, ioctl, signals, PTYs, and
other foreign procedures. Feature modules compose the published core API and,
when necessary, narrowly scoped system facilities. Feature policy does not
belong in core.

R6RS enforces the boundary. Core internals are invisible to modules and its
exports are immutable. Extension code cannot accidentally reassign editor
state that was not intentionally published.

## Module shape

An extension exports `init!`, which performs its registrations:

```scheme
(library (my-mode)
  (export init!)
  (import (chezscheme) (core))

  (define (my-styles line) ...)

  (define (init!)
    (register-mode! "my" '(".my") '() my-styles)))
```

At startup core discovers `lib/*.e`, loads each library, and calls its `init!`.
Dependencies are ordinary R6RS imports, so Chez determines compilation order.
A failing module reports an error but does not prevent unrelated modules or the
editor from starting.

Bundled and third-party modules should use `bind-default-key!`. `bind-key!` is
for deliberate user or session overrides, ensuring a module reload cannot
displace configuration choices.

## Hot reload

Saving any module source from the active installation reloads it in place.
Modules that import it recompile and reinitialize in dependency order. Editing
outside e can be picked up explicitly:

```scheme
(reload-module! "paren")
```

`modules-reload-on-save` controls automatic source reload. The core itself is
not hot-reloadable.

Registrations are tagged with their owning module. Reload first retracts the
old modes, keys, hooks, app callbacks, descriptions, and other registrations,
then installs the new set. Authors do not need to unregister old values.
Buffers, windows, and the live evaluation top level remain in place. If reload
fails, the old module remains usable where possible and the failure is logged.

## Public API conventions

The published API contains commands, read-only state, editing primitives, and
extension registries. `M-x (` followed by Shift+Tab lists the current top-level
catalog; `C-h f` describes documented values.

Naming distinguishes interaction:

- a procedure ending in `!!` waits for user input and normally returns void;
- a single-`!` procedure acts immediately on explicit state and may return a
  useful value;
- predicates end in `?` and parameters are ordinary callable Scheme values.

Interactive wrappers should be thin. For example, `find-file!!` prompts and
then calls `visit-file!`.

`call-with-buffer` temporarily evaluates against another buffer.
`call-as-one-edit!` groups mutations into a labeled undo step. Errors should be
raised normally; the command loop reports unexpected conditions in the echo
area and log.

## Modes

Modes are registered by name, filename extensions, optional shebang
interpreters, and a line styler:

```scheme
(register-mode! "scheme"
  '(".scm" ".ss" ".sls" ".sps" ".sc" ".e")
  '("scheme" "petite" "chez" "guile" "racket")
  scheme-styles)
```

The first matching extension or interpreter selects the mode. Bundled modes
cover Scheme, C, and Markdown. Additional personal extensions should be added
without replacing the mode:

```scheme
(add-mode-extension! "scheme" ".foo")
```

Stateful syntax analysis uses `memoize-buffer-analysis`. The analyzer receives
a snapshot vector of lines and returns per-row results, recomputed once per
buffer revision.

## Highlighting and formatting

`add-highlighter!` registers redraw-time ranges shaped as `(row start end)` or
`(row start end face)`. Search, bracket matching, selections, and app cursors
use this mechanism.

Language layout remains modular through `register-indenter!` and
`register-formatter!`. See [Formatting](FORMATTING.md).

## Apps and views

An app buffer owns dynamic rendered state and may handle input. A view is an
app without interaction. Apps can capture all input, handle only a small local
keymap, act on a target window, publish status hints, control cursor display,
and consume mouse events without taking focus.

See [App buffers](APPS.md) for registration and event propagation, and
[Buffers](BUFFERS.md) for the `*buffers*` interface and target-window model.

## Describe and log integration

Modules can publish structured documentation with `register-descriptions!`
and component-specific log presentation with `register-log-formatter!`. Both
registries participate in transactional reload. See [Describe](DESCRIBE.md)
and [Logging](LOG.md).

