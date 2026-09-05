# Architecture and extension modules

## Library architecture

Everything in `lib/` is an R6RS library using the `.e` extension. `edit.e` is
`(edit)`, `eval.e` is `(eval)`, and so on. The loader is only bootstrap: it
locates the adjacent libraries and compiled-object directory, configures Chez,
imports the command layer (`edit`, bare -- the names M-x sees), `main` and
`kernel` (prefixed), and runs `(main:run)`.

The editor is layered seam modules -- `kernel`, `store`, `file`, `head`,
`paint`, `prompt`, `mode`, `keymap`, ... -- with `main.e` running the loop on
top, `edit.e`, the command layer, as the default app, and the other apps
(`terminal`, `git`, `describe`, `eval`, ...) beside it. `sys.e` owns libc,
termios, ioctl, signals, PTYs, and other foreign procedures. Feature modules
compose the command API and the seams and, when necessary, narrowly scoped
system facilities.

Every library but `edit` is imported with its own prefix, and that is also how
M-x sees it: `store:`, `keymap:`, `terminal:`, `git:`, `sys:`. Only `edit`'s
names are bare. Modules are named in the singular (`style`, `file`, `mode`,
`string`, `actor`, `doc`), and their exported names drop the module's stem: the
prefix says it once -- `style:set!`, not `styles:set-style!`; `keymap:bind!`,
`log:add!`, `mode:register!`, `git:branches`, `terminal:send!`.

R6RS enforces the boundary. A library's internals are invisible to modules and
its exports are immutable. Extension code cannot accidentally reassign editor
state that was not intentionally published.

## Module shape

An extension exports `init!`, which performs its registrations:

```scheme
(library (my-mode)
  (export init!)
  (import (chezscheme) (except (edit) init!)   ; the command layer, bare
          (prefix (mode) mode:))             ; seams, prefixed

  (define (my-styles line) ...)

  (define (init!)
    (mode:register! "my" '(".my") '() my-styles)))
```

At startup the kernel discovers `lib/*.e`, loads each library, and calls its
`init!`.  Dependencies are ordinary R6RS imports, so Chez determines
compilation order.  A failing module reports an error but does not prevent
unrelated modules or the editor from starting.

Bundled and third-party modules should use `keymap:bind-default!`.
`keymap:bind!` is for deliberate user or session overrides, ensuring a
module reload cannot displace configuration choices.

## Hot reload

Saving a reloadable extension's source from the active installation reloads it in place.
Modules that import it recompile and reinitialize in dependency order. Editing
outside e can be picked up explicitly:

```scheme
(kernel:reload-module! "paren")
```

`main:modules-reload-on-save` controls automatic source reload. The kernel,
`main` (the loop), and libraries that `main` imports directly or transitively
require a restart. The editor refuses to reload those libraries to keep every
caller using the same instance.

Registrations are tagged with their owning module. Reload stages replacement
modes, keys, hooks, app callbacks, and descriptions, then publishes them
together after initialization and reload hooks succeed. Authors do not need
to unregister old values. Other threads retain the published registrations
while initialization runs. A failed or abandoned reload discards its staged
changes, preserving concurrent registrations and revocations.

Buffers, windows, and the live evaluation top level remain in place. Registry
rollback does not undo arbitrary Scheme effects, resources, or library
redefinition: old registered callbacks may remain usable even when new library
exports already exist. The failure is logged.

## Registries and persistent state

`kernel:make-registry` returns an opaque handle. `kernel:registry-add!` tags an
item with `kernel:registering-module`, or `#f` for a runtime registration.
`kernel:registry-items`, `kernel:registry-entries`, and `kernel:registry-find`
read newest entries first; the ownership read returns copied `(owner . item)`
wrappers. Registered items remain the registering code's responsibility.

`kernel:make-registry key-of` additionally requires unique keys. The key
procedure runs once on admission, outside the kernel lock, and must return a
stable value. The complete proposed state is checked before publication;
`kernel:registration-conflict?` identifies a duplicate-key condition. A
conflict discards the whole registration update, including changes to other
registries. Individual mutations use this same publication rule.

`kernel:registry-remove!` selects entries from one snapshot, calling its
predicate outside the kernel lock, then removes those entry identities from
the latest state. `kernel:retract-module!` removes an owner's entries across
registries. Both preserve unrelated concurrent changes.

`kernel:call-with-registration-update` stages changes on the calling thread,
which reads its own additions and removals. Nested success joins the parent;
only the outer scope publishes. Exceptions and continuation escapes discard
the scope's changes, and a closed scope cannot be resumed. Return values are
preserved. Use this scope instead of the old snapshot/restore helpers, and
publish new registration handles to runtime consumers only after it commits.
Reads can observe later committed work; this publication scope does not make
a read-then-add check atomic. Use a keyed registry to enforce uniqueness at
commit. Staging and `kernel:registering-module` are thread parameters, so
overlapping updates cannot replace each other's context.
Module loading, reload, and configuration already use it; load and reload
must run on the main pump.

Store events and actor messages/replies use
`kernel:call-with-runtime-registrations`. They resolve published callbacks and
clear the triggering initializer's staging and module owner. Registrations
made by those callbacks are independent runtime changes unless the callback
sets an explicit owner. A worker also operates independently of a staging
scope on the thread that created it.

`kernel:registry-observe! registry proc` returns a revocation token. After a
commit, `proc` receives two lists: removed items and added items, newest first.
Observers are owned registrations; `kernel:registry-unobserve! token` also
revokes one explicitly. A failed update or a net empty change emits nothing.
Recipients are captured after the whole batch commits, so an observer added
in that batch hears its changes, and an observer removed in it does not.
Revocation also skips queued callbacks; a callback already selected may finish.

The kernel and store share `kernel:make-delivery-queue`,
`kernel:enqueue-delivery!`, and `kernel:drain-deliveries!`.
Queue callbacks under the owning state lock and drain
after releasing it. Delivery is ordered, outside initializer context, and
survives a callback failure or escape. A completed delivery continuation
cannot be resumed. Reentrant or concurrent writers may return before delivery;
callbacks must not wait for a later notification. Mutating an observed
registry can invoke its callbacks, so release application locks first.

`kernel:persistent-cell key make-initial` initializes one box per key.
Concurrent callers wait for the initializer, which runs outside the table
lock and may request other keys. Failure or escape releases the key for
retry; recursive initialization of the same key raises an error. Callers
must synchronize subsequent mutations of the box's contents themselves.

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
(mode:register! "scheme"
  '(".scm" ".ss" ".sls" ".sps" ".sc" ".e")
  '("scheme" "petite" "chez" "guile" "racket")
  scheme-styles)
```

The first matching extension or interpreter selects the mode. Bundled modes
cover Scheme, C, and Markdown. Additional personal extensions should be added
without replacing the mode:

```scheme
(mode:add-extension! "scheme" ".foo")
```

Stateful syntax analysis uses `mode:memoize-analysis`. The analyzer receives a
snapshot vector of lines and returns per-row results, recomputed once per
buffer revision.

## Highlighting and formatting

`paint:add-highlighter!` registers redraw-time ranges shaped as `(row start
end)` or `(row start end face)`. Search, bracket matching, selections, and app
cursors use this mechanism.

Language layout remains modular through `register-indenter!` and
`register-formatter!`. See [Formatting](FORMATTING.md).

## Apps and views

An app buffer owns dynamic rendered state and may handle input. A view is an
app without interaction. An app's handler has first refusal on every key its
mode context leaves unbound and passes the rest through to the keymaps; an app
that consumes everything (the terminal) names an escape prefix in its mode's
keymap context, and the dispatcher hands that key to the keymaps before the
handler sees it. Apps act on the
selected window, publish status hints, control cursor display, and consume
mouse events without taking focus.

See [App buffers](APPS.md) for registration and event propagation, and
[Buffers](BUFFERS.md) for the `<buffers>` interface.

## Describe and log integration

Modules can publish structured documentation with `doc:register!` and
component-specific log presentation with `log:register-formatter!`. Both
registries participate in transactional reload. See [Describe](DESCRIBE.md)
and [Logging](LOG.md).
