# Architecture and extension modules

## Library architecture

Libraries under `lib/` use the `.sls` extension and qualified names:
`lib/head/edit.sls` is `(head edit)`, `lib/apps/eval.sls` is `(apps eval)`, and so on.
Directories group responsibility in dependency order:
`foundation`, `sys`, `core`, `state`, `service`, `head`, `apps`, `modes`, `run`.
Imports may point down or sideways; no library imports a runtime entrypoint.
The loader locates the adjacent libraries and object caches and configures
Chez. It admits options through `startup`, then selects the base or client runtime.
Plain `e` starts or connects to the base, checks source and wire compatibility,
and claims the connection before importing the modules, each under its
prefix, the literals (bare: `(buffer "name")`, `(window n)`, `(mode "scheme")` and one per completing type,
`(region ...)` and `(head "desk")` read back as they print) and `main`, and
runs `(main:run!)`.
`--base` acquires the directory's lifetime lock and runs the base without
importing a head. This ordering chooses a head's identity before it creates
shared state. The base owns terminal processes through shutdown; a client
disconnect owns only that connection and its actor registration.
`kernel:fingerprint` reads every `.sls` below this installation's `lib/`,
independent of runtime roots. A new head must match the base's startup
fingerprint; see [attachment](MULTIHEAD.md#the-base-and-its-heads).
`--restart` performs the maintenance review before importing head libraries.
The daemon entry point lives in `lib/base/run/base.sls`, outside client roots.
In the base, `session` restores store IDs, revisions and opaque named
checkpoints before module initialization and `base-config.e`; the listener
binds last. Session serialization uses the store and VT representation
boundaries and does not import a head.

A library is named by its kind directory and its file: `lib/core/kernel.sls`
declares `(core kernel)`, `lib/head/edit.sls` declares `(head edit)`, and
code imports them so, `(prefix (core kernel) kernel:)`; the prefix is the
last component. Plain `e` selects the `lib/client` implementation tree;
`e --base` selects `lib/base`. Each runtime searches two roots, its own
tree in front of `lib/`, so `(state store)` resolves to the runtime's
`state/store.sls` and every other name to `lib/`'s. Attached heads import
the same `edit` and `main` consumers. Chez's `.so` objects live in
`eo/client` or `eo/base`, mirroring the kind directories, one cache per
runtime. An installation-wide `eo/lock` serializes expansion, dependency
checks and automatic compilation across processes. The small `cache` library
loads directly from source before the first cached import; it holds no lock
while editor commands run. Source lookup and reload follow the active implementation. The loader sets
`kernel:installation-directory` to its own directory; `config.e`,
`base-config.e` and `data/` are located there independently of source roots.
Direct library users can set this parameter before starting their runtime.
It defaults to the working directory at kernel initialization and normalizes
each assigned path to an absolute textual path, so a later directory change
does not redirect configuration or data. Client libraries expose the operations
used by head commands, not the base's producer and session-control APIs.

The editor is layered seam modules -- `kernel`, `store`, `file`, `head`,
`paint`, `prompt`, `mode`, `keymap`, ... -- with `main.sls` running the loop on
top, `edit.sls`, the command layer, as the default app, and the other apps
(`terminal`, `git-view`, `file-view`, `describe`, `eval`, ...) beside it.
`sys.sls` owns libc, termios, ioctl, signals, PTYs, and other foreign procedures.
Feature modules compose the command API and the seams and, when necessary,
narrowly scoped system facilities.

`dispatch:key!` handles a key through the current app and keymaps; modal
readers such as incremental search use the same dispatcher as the main loop.
The command layer installs the loop's file opener, quit command and after-key
hook through `head:set-file-opener!`, `head:set-quit-command!` and
`head:set-after-key!`. Commands and apps can use these head libraries without
importing the runtime entrypoint `(main)`.

`path:expand` expands leading `~` and `~/`; `path:canonical` makes a name
absolute and resolves dot components and repeated separators textually.
Combine them as `(path:canonical (path:expand name))`. These operations do
not require the path to exist or chase symbolic links. The `file:expand`
and `file:canonical` names remain aliases of the shared procedures;
`file:visit-path` additionally resolves filesystem identity for visited files.

For file codecs, `(file:call-with-port path output? use)` opens a text port
and passes it to `use`, preserving its return values. `#f` reads; `#t` replaces
the file and preserves existing permissions, best-effort. The scope closes
the port on return, exception or engine expiry. An expired scope cannot be
resumed; start a fresh operation after reviewing any partial output. Both
ordinary text I/O and the reference corpus use this scope.

For time-dependent presentation, a `head:add-pre-redraw-hook!` callback can
call `(head:request-frame-at! deadline)` with a Chez monotonic time. The head
copies the earliest request and wakes its input wait at that time, including
inside a prompt. Each frame clears the request before running the hooks:
renew only deadlines still needed by live state. Blame uses this for tint
expiry, so removing an overlay or reloading its module leaves no timer worker.
This is a main-thread presentation API; background changes still notify the
head through `head:wake-main!` or `head:run-on-main!`.

`paint:redraw!` refreshes terminal size and window tiling before running that
head preparation, including direct redraws during a prompt. Hooks can read
`paint:screen-cols` and window widths for the current frame. A hook that
presents a message may reenter redraw; publish its state before calling out.
Preparation finishes before the frame's synchronized terminal update begins.
An alternative `head:set-frame-hook!` callback owns the complete frame,
including `head:before-frame!` after establishing geometry.

Documentation data lives in `reference` and the module-entry registry in `doc`.
Use `reference:lookup` for queries that need no browser; `describe` adds the
head's prompts, key annotations, and Markdown display. `reference:page!`
publishes a private Markdown source per requesting head; the head uses the
ordinary local Markdown companion to display it.

Every library is imported with its own prefix, and that is also how M-x
sees it: `edit:`, `store:`, `keymap:`, `terminal:`, `git:`, `sys:`. Only
`literal`'s names are bare, the constructors that read a printed value
back, `(buffer "name")`, `(window n)`, `(region b start end)` and the
identities `(head "desk")`, `(agent "claude")` and `(base 'e)`, with the
region's predicate and accessors. Modules
are named in the singular (`style`, `file`, `mode`,
`string`, `actor`, `doc`), and their exported names drop the module's stem: the
prefix says it once -- `style:set!`, not `styles:set-style!`; `keymap:bind!`,
`log:add!`, `mode:register!`, `git:branches`, `terminal:send!`.

R6RS enforces the boundary. A library's internals are invisible to modules and
its exports are immutable. Extension code cannot accidentally reassign editor
state that was not intentionally published.

## Module shape

An extension exports `init!`, which performs its registrations:

```scheme
(library (modes my-mode)
  (export init!)
  (import (chezscheme)
          (prefix (head edit) edit:)              ; the command layer, a seam like any
          (prefix (head mode) mode:))             ; seams, prefixed

  (define (my-styles line) ...)

  (define (init!)
    (mode:register! "my" '(".my") '() my-styles)))
```

Place this example in `lib/modes/my-mode.sls`; its library name and prefix
remain `my-mode` regardless of the containing kind directory.

`base`, `client` and `main` select their bundled modules explicitly. The kernel loads
that list and calls each `init!`; dependencies remain ordinary R6RS imports.
Load additional extensions with `(kernel:load-module! "my-mode")` from the
appropriate configuration file. A failed base initializer stops startup;
failed head extensions report their errors while the editor continues.

### External checkouts

Keep an extension in its own repository with sources under `lib/`. Enable
its entry module with one expression in `config.e`:

```scheme
(extension:load! "~/git/my-extension" "my-mode")
```

The entry can be `lib/my-mode.sls`, declaring `(my-mode)`, or
`lib/modes/my-mode.sls`, declaring `(modes my-mode)`. It exports `init!`;
the kernel publishes its exports as `my-mode:` and owns its registrations
just like a bundled module. Helpers are ordinary imported libraries.
Relative checkout paths start at e's installation, not the current buffer.
An optional third argument adds R6RS source roots, one directory or a list
of them, relative to the checkout or absolute, with `~` supported; at M-x a
root completes as a directory, inside the string and inside each element of
the quoted list:

```scheme
(extension:load! "~/git/my-extension" "my-mode" '("vendor" "~/scheme"))
```

The loader manages compilation under e's runtime cache. The checkout can
be read-only; it needs no cache setup, generated files or machine-specific
paths in its libraries. It never downloads dependencies. A library present
under two roots is not checked: Chez searches the roots in the order given,
e's own first, and the first match wins. Libraries may be added under a root
after loading, so resolving an overlap rests with the extension's user. A
missing import names the
library and the third argument that supplies its root.
Repeated loading is harmless. If initialization or a surrounding config
fails, module membership and registrations roll back, allowing a corrected
retry. Imported libraries and their search roots stay for the process's
lifetime, as do arbitrary initializer effects. Load these head extensions
from `config.e`, not `base-config.e`.

For example, put this in a separate checkout's `lib/greeting.sls`:

```scheme
(import (only (foundation edoc) elibrary))
(elibrary (greeting)
  (export hello! init!)
  (import (chezscheme)
          (prefix (head edit) edit:)
          (prefix (head keymap) keymap:))

  (edoc "Show a greeting in the echo area.")
  (define (hello!) (edit:set-message! "Hello from an extension"))

  (define (init!) (keymap:bind-default! "C-c h" hello!)))
```

Load it with `(extension:load! "~/git/greeting" "greeting")`. Its command
appears as `greeting:hello!` in completion and describe, and saving its source
in e reloads its binding. Plain R6RS libraries are also supported; `elibrary`
and `edoc` supply documentation beside the definitions without a second API.

Test an entry in a headless process using:

```sh
scheme --script /path/to/e/tools/test-extension.sps /path/to/greeting greeting tests/smoke.ss
```

The runner initializes editing, evaluation and Scheme mode, then loads the
extension and the test file. Tests run from the extension checkout; optional
trailing arguments supply dependency roots just like `extension:load!`.
They can import editor libraries and use `kernel:load-module!` for additional
apps. The runner reads no personal configuration, contacts no base and needs
no terminal. It shares e's compiler cache but editor state lasts only for that
process. For a minimal `tests/smoke.ss`, call `(greeting:hello!)`; ordinary
Scheme assertions and test libraries work as usual.

Bundled and third-party modules should use `keymap:bind-default!`.
`keymap:bind!` is for deliberate user or session overrides, ensuring a
module reload cannot displace configuration choices.

To give a mode Scheme's editing behavior with its own file endings, derive it:

```scheme
(mode:derive! "worksheet" "scheme" '(".ws" ".mpl"))
```

A derived mode, a submode, has a parent. Whatever it does not define itself,
line styles, rendering, row styles, indentation, formatting and the Tab
policy, follows the parent's current registration, including after a reload,
and keys are looked up in its own context first, then the parent's, then the
global map. Optional arguments after the endings give the submode its own
line styles, render transform and row styles, as `mode:register!` takes them:
Pretty Scheme's displays are submodes of Scheme that override only their
presentation. Local indenter and formatter registrations override inherited
ones. The new mode keeps its own suffix and interpreter detection. The parent
may be registered later, since resolution is by name at each use; a cycle is
refused. If the parent is removed, inherited behavior becomes unavailable
while local behavior and the child's identity remain.

## Hot reload

Saving a reloadable extension's source from the active installation reloads it in place.
The source must be a `.sls` file in an active library root; its stem remains
the module name even when its directory changes. A saved file in the other
runtime's implementation tree does not reload the active implementation.
Modules that import it recompile and reinitialize in dependency order. Editing
outside e can be picked up explicitly:

```scheme
(kernel:reload-module! "paren")
;; A helper's full library name reloads its importers without publishing it:
(kernel:reload-module! '(my-extension helper))
```

`main:modules-reload-on-save` controls automatic source reload. The kernel,
`main` (the loop), the base services (including policy, terminal runtime and
reference corpus), and their transitive imports require a restart. Use
`e --restart` to preserve shared text and named views;
see [the recovery limits](MULTIHEAD.md#restart-and-recovery). Bootstrap
declares these process roots with `kernel:pin-modules!`, including the attached
connection and client seams; refusal happens before
redefinition. A reload reinitializes only the affected module and its loaded
importers. Unrelated owners keep their registrations and active work.

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
preserved. Publish new registration handles to runtime consumers only after
it commits.
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

The process head's actor registration and store subscription publish together
as runtime registrations. They outlive an extension that happens to import
the head first, even if that extension's initialization fails. Other
extension-owned registrations retain the normal staged lifetime.

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
extension registries. At the initial `λ (` prompt, press Shift-Tab twice to
list the current editor-defined symbols; `C-h f` describes documented values.

Naming distinguishes effects:

- a procedure ending in `!` changes state, and may return a useful value;
- a procedure without it is a query;
- predicates end in `?` and parameters are ordinary callable Scheme values.

The bang is checked, not trusted. `tools/edoc-coverage.sps --effects` walks
every documented definition's body, following calls through the tree's
own libraries, and reports a `!` name that reaches no change, a name
without one that reaches a change, and a name that reaches a key read
without declaring it. A change is a mutating primitive on something the
body did not make itself (a module variable, a table it was handed, an
argument), a parameter called with a value, a port written, or a call
that resolves to such a definition; something the body made, a fresh
vector or a record it constructed, is scratch. The walk stops at three
seams and takes them as opaque, satisfying a bang without indicting a
query: a foreign procedure, a hook installed at run time, and an argument
the body calls. Three edoc clauses declare the exceptions: `(prompts)` for
a procedure that waits for a key, `(effects internal)` for a query whose
only changes are its own caches, and `(effects remote)` for a transport
such as `client:request`, whose effect is the message's and whose caller
keeps the bang. Its verdict is on the name importers see, so an export
renamed with `rename` is judged by its exported spelling.

The check is one of the linter's, `tools/elinter.sps`, beside two layout
conventions: a blank line precedes every `edoc` form, comments allowed
between, and the definition an `edoc` annotates starts on the very next
line; export lists are sorted by exported name, and import specs by
library name with `(rnrs)` and `(chezscheme)` first. The linter prints
each finding as `path:line: message` and exits with their count. The
suite runs it as `tests/lint.ss`, and the versioned hook
`tools/hooks/pre-commit` runs it over the tree being committed, once a
clone has set `git config core.hooksPath tools/hooks`.

Prompting is the exception, not a naming matter: a command that must wait
for input, `describe:key!` reading a key or `search:replace!` asking per
occurrence, declares `(prompts)` in its edoc. Otherwise the M-x API with typed
completion does the asking, and a key that used to prompt opens M-x with the
call typed up to its argument: `C-c a` gives `λ (edit:answer! `. Such keys
are bound structurally, from the procedures themselves rather than spelled
names: `(keymap:bind! "C-c a" (keymap:prefill edit:answer!))` opens M-x
pre-filled, and `(keymap:bind! "C-x k" (keymap:call edit:kill-buffer!
head:current-buffer))` calls the command on what the producers return when
the key is pressed. `C-h k` shows both as the call they make, by the names
the top level gives the procedures, so a rename follows.

A command acts on the current window, buffer or region, or takes its
target as a required argument, never both; the scope forms retarget it for
the extent of a body: `head:with-buffer`, `head:with-window` and
`edit:with-region`, dynamic and invisible to the apps. Each is one form
with no procedure beside it, so M-x offers one spelling.
`edit:call-as-one-edit!` groups mutations into a labeled undo step. Errors should be
raised normally; the command loop reports unexpected conditions in the echo
area and log.

Foreground commands use one `sys` owner. `sys:open-process` takes a nonempty
list of argument strings, executes them directly and searches `PATH` for a
program without a slash; `sys:write-process!` sends one bytevector, or `#f`
for no input, and closes stdin. Read the blocking binary `sys:process-input`
port to EOF, then call `sys:process-result` for two values: exit status and
stderr text. A terminating signal is returned as its negative number.
The caller must close the command on every exit from its scope. Closing the
input port or calling `sys:close-process!` closes its pipes, terminates a still
running command and reaps its PID; repeated close is harmless. The owner drains
stderr while transferring stdout/input, so large transfers need no separate
reader threads. Keep each command under one caller's ownership.

`sys:call-with-streamed-output stdout! stderr! thunk` captures Scheme and
process stdout/stderr, delivering complete lines and the final partial line
to its two reader callbacks. It returns all the thunk's values. Both readers
finish and descriptors are restored before it returns or an escape reaches
the caller, so callbacks may borrow resources from the caller's surrounding
scope. A failed callback stops receiving lines, but its stream is drained;
the failure is raised after normal body completion. An escaping body error
takes precedence. An ended capture cannot be resumed through a continuation.
Keep captures serialized because process descriptors are shared, and send
callback output to a separate port so it does not feed back into capture.

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

A buffer takes the mode detection finds when it opens, and follows detection
until a mode is chosen for it by hand:

```scheme
(mode:choose! "scheme")
(mode:choose! "markdown" (buffer "notes.md"))
```

Registering a mode, deriving one or adding an ending gives the mode to the
open buffers that have none yet, so a file opened before its extension loads
takes the mode when the extension registers it. A buffer that already has a
mode, detected or chosen, keeps it and picks up only a reloaded record of the
same name; `(mode:assign!)` re-detects the current buffer on request, and either
command takes another buffer as a last argument or under `head:with-buffer`.

Every type a library defines with a completer also spells its values as a
literal at the top level, named after the type and derived from it: `(mode
"scheme")`, `(file "notes.txt")`, `(directory "lib")`, `(style 'ghost)`,
`(key "C-x C-f")`. The literal reads the spelling with the type's reader when
it has one, `(buffer "name")` giving the live buffer, and otherwise returns
the spelling itself once the type accepts it, so `(mode "scheme")` is the
name `"scheme"` checked against the registered modes. Commands take the bare
value and the literal alike; a command's author gets that with one call,
`(edoc:type-value 'mode name)`, which returns a value the type accepts or
reads it from its spelling. So the commands that take a buffer take its name
too, and those that take a window its index: `(kill-buffer! "notes.txt")`,
`(window:focus! 2)`, `(head:with-buffer "*scratch*" ...)`, `(mode:of
"notes.txt")`; the accessors under `head:` keep taking the values
themselves. A type whose literal returns its spelling, `file` or `mode`,
needs nothing, since `(file "notes.txt")` is `"notes.txt"`. Completion
spells an argument's options as literals, a reminder of the type at that
position, and writes the whole literal on Tab.

Stateful syntax analysis uses `mode:memoize-analysis`. The analyzer receives a
snapshot vector of lines and returns per-row results, recomputed once per
buffer revision.

Styler vectors use source character positions. Every cell of a glyph uses its
leading character's style. An optional display transform must preserve the
source's character-to-cell geometry: return a same-length string, or a
same-length vector of strings whose concatenation meets that contract.
Incompatible substitutions fall back to source text, keeping cursor, mouse,
selection, and wrapping consistent with what is displayed.

## Highlighting and formatting

`paint:add-highlighter!` registers redraw-time ranges shaped as `(row start
end)` or `(row start end face)` in the selected window, or `(buffer row start
end face)` / `(window row start end face)` with explicit scope. Search, bracket
matching, selections, and app candidates use this mechanism.
`paint:hover-ranges` builds a window-scoped `hover` range from a clickable
text hit test; see [App buffers](APPS.md). It shares click and navigation
geometry and keeps mouse emphasis separate from keyboard selection.

Language layout remains modular through `mode:register-indenter!` and
`mode:register-formatter!`. See [Formatting](FORMATTING.md).

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
registries participate in transactional reload. A library written with
`elibrary` documents its exports where it defines them, each definition
annotated with a typed `edoc` form checked against it when the module
expands. See [Describe](DESCRIBE.md) and [Logging](LOG.md).
