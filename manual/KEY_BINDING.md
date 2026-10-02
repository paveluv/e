# Key binding configuration

Every keyboard command in e is resolved through a keymap. Built-in and
extension-module bindings are defaults; bindings from `config.e` are user
overrides and take priority regardless of registration or module reload order.

Press `C-h k`, then a key or complete chord, to inspect it in `<bindings>`.
The report follows the captured app's input route and shows its resolved
command, forwarding trace, binding origin, shadowed definitions and other
contextual meanings. Capture never executes the command; Escape or `C-g`
cancels. `(bindings:key! root)` opens this same capture from M-x; `root` is the composition providing the auxiliary host.

## Inspecting bindings

`C-x TAB` opens the read-only `<bindings>` inspector above the echo area.
It shows mouse bindings first, followed by the active window's keyboard
bindings and its widget command connections. Each row shows the public API
and its documentation. Keyboard bindings are grouped by app and mode context,
then by global bindings. The listing fits its window, with a scrollbar and
independent scrolling in each view. `(bindings:show! root)` opens the same inspector
from M-x or a script.

It lists what works here: a key a nearer context takes, `RET`
in `<finder>` say, is left out of the global section, and where the text is
read-only, an app's buffer or one made read-only, the editing commands are
left out, those whose edoc declares `(edits)`. Keys that run one
command share a row beside
the command, `edit:kill-line!` say, and what it does, from its documentation,
wrapped in its column. The
listing follows the active window: switch to another buffer or app and it
lists that one's keys. Every binding is listed; a key bound to a lambda,
in `config.e` say, shows as `anonymous command` with nothing to say about
it, so bind a named command instead, and the editor's own libraries never
bind a lambda, the linter refusing one.
`C-x TAB` works everywhere: inside a prompt it lists the prompt's keys, each
a `prompt:` command such as `prompt:accept!` for Enter, the keys of the
prompt's own view where it has one, and the
global commands allowed while a prompt is open.
`C-x TAB` again pages the listing down from wherever
you are, and back to the top past the end, and `C-x S-TAB` pages it up; `C-x o` or `M-Down` select the
pop-up to browse it, select text with the mouse and copy with `M-w`. The `↓` on its status
line puts it away, as `(bindings:hide! root)` does. `(bindings:open! window root)` shows the listing
in the current window instead, for the buffer that window shows, and
`C-x TAB` pages it there. Apps bind keys in their contexts to public commands.
In `<finder>`, Enter invokes the table's `activate` connection and the listing
traces it to `finder:choose!`. These commands run from M-x and are described
by `C-h k`; an app that captures keys, the terminal, lists its toggle and a
row saying what it takes. In the pop-up itself, an app's there, the conflicts browser's say, it lists that app's keys and keeps to them while the pop-up stays current.

## Global bindings

`keymap:bind!` takes an optional context, a key specification and a
zero-argument command. Bind explicit receivers through `keymap:call`:

```scheme
(keymap:bind! 'composed-window "M-l" (keymap:call log-view:open! widget:target))
(keymap:bind! 'widget-editor "C-c s" (keymap:call edit:save! widget:target))
(keymap:bind! 'widget-editor "M-g" (keymap:call edit:select! widget:target '(0 . 0) '(0 . 0)))
```

Global specifications may contain any number of space-separated events.
When a prefix is entered, e waits for the rest and displays the chord.
A context target comes from recursive routing, so the same command works
in an embedded editor without assuming a current window. A lambda works
too, but appears as an anonymous command in Bindings.

Two structural actions describe themselves where a lambda shows as an
anonymous command. `keymap:call` applies a command to what producer procedures
or nested `keymap:call` expressions return when the key is pressed, and to
any other argument as given. `keymap:run!` executes that same structured call;
`keymap:prefill` opens M-x with the command's call typed up to its next
argument, so completion asks for it. Its arguments follow the same rule:
producers and nested calls run at the key press, and their resulting values
are inserted into the prompt. Inspecting either action never runs them:

```scheme
(keymap:bind! 'composed-window "C-x k" (keymap:call window-control:discard! widget:target))
(keymap:bind! 'finder "F2"
  (keymap:call table:toggle-sort!
    (keymap:call widget:descendant widget:target 'table) 'size))
(keymap:bind! "C-c a" (keymap:prefill edit:answer!))
```

Bindings shows the first with its explicit window receiver and
the third as `λ (edit:answer! `, by the names the top level gives the
procedures, so a rename follows.

Nested calls let a composition address a named child through public APIs:

```scheme
(keymap:bind! 'my-app "C-u"
  (keymap:call entry:delete!
    (keymap:call widget:descendant widget:target 'filter 'entry)
    'all))
```

For widget bindings, `C-x TAB` substitutes the receiving view's ID for
`widget:target`, so the shown Scheme expression also runs in eval or scripts.
The ID addresses that view while it remains mounted. Describing a binding
never invokes its argument producers.

For a table action, bind the key to `table:invoke!` with a named command.
Keep the command name explicit in bindings, including Enter's `activate`, so
the listing identifies which connection is followed:

```scheme
(table:invoke! '(model 110) 'activate) ; default action, opening a row in Buffet
(table:invoke! '(model 110) 'trash)    ; invoke the table's trash connection
(table:invoke! '(model 110) 'delete)   ; invoke the table's delete connection
```

Buffet's `C-k`, for example, is displayed as:

```scheme
(table:invoke! (widget:descendant '(model 109) 'table) 'trash)
```

The table supplies its hovered or selected row and result basis to the
connected action. Bindings shows the full registered forwarding chain below
the key, with each operation's own documentation:

```scheme
(table:invoke! (widget:descendant '(model 109) 'table) 'trash)
  → (table:invoke! '(model 110) 'trash)
    → (widget:invoke! '(model 110) 'trash selection basis)
      → (widget:act! '(model 109) 'kill selection basis)
        → (buffet:kill! '(model 109) selection basis)
```

The model IDs are those of the current composition. `selection` and `basis`
remain symbolic and appear in italics: inspection does not select a row,
run argument producers or execute commands. Only explicitly declared, bounded
local queries can resolve arguments. Multiple forwarding sites are marked as possible routes;
cycles and unavailable targets stop the chain with a note. This is a map of
registered forwarding, not a prediction that runtime validation will succeed.
The **Widget commands** section still lists the connections independently.
Selection changes do not rewrite the binding or its help. Copy the initial
expression to eval to use the same activation and validation path as the key.

`keymap:call` is syntax so it can also accept forwarding syntax such as
`widget:act!`. To construct a call from a list of arguments, use
`(keymap:call (apply command arguments))`. Ordinary command procedures remain
first-class values. [Widgets](WIDGETS.md#forwarding-and-inspection) describes
how extensions register their forwarding at compilation.

When the pointer has a target, `C-x TAB` starts with **Mouse bindings**.
This section follows the pointer, including over an unfocused window; the
keyboard sections continue to follow keyboard focus. A table heading shows
its sort command, a row shows its explicit choice, and an entry shows caret
placement and selection. Unavailable widget actions are omitted.
While the pointer is over `<bindings>` itself, the mouse section keeps the last
inspected target so you can read and scroll it. Moving elsewhere resumes
inspection without resetting your reading position. A different keyboard
context starts the listing at the top.

For a widget window, **Widget commands** follows the keyboard sections.
It lists named command connections from the whole current composition,
including children outside the keyboard focus path. Each group names its
child path, widget kind and `(model N)` reference. A row such as `activate`
shows its target's public call and documentation. Fixed arguments are
spelled as literals; remaining argument names, such as `selection basis`,
are supplied by the invoking control, so these rows are call templates.
Unavailable targets stay listed with an explanation. Rewiring updates the
listing without resetting its reading position.

This discovery reads mounted descriptors and cached sources locally. It
does not execute actions or query the base, and does no work while Bindings is
hidden. Argument spelling in Bindings and prefilled M-x expressions uses the
same Scheme spelling as completion: a model argument is `'(model N)`, and
compound values are quoted once regardless of their argument type.

`(mouse:bindings)` returns the current `(gesture action)` pairs as data;
an optional `(column . row)` selects another screen cell, using one-based
coordinates. `(mouse:position)` reports the physical pointer's last known
cell even after keyboard input clears hover emphasis, or `#f` if unknown.
Gestures include `(click primary ())`, `(click secondary ())`,
`(click primary (shift))`, `(drag primary ())`, and `(wheel down ())`.
`mouse:gesture-text` spells them for help. Actions use `keymap:call`, just
like keyboard bindings. Scripts can use `widget:pointer!` with a normalized
pointer or scroll event and zero-based screen coordinates. The TUI adapter
`mouse:input!` decodes terminal reports into that same route.

Printable characters can also be bound. An explicit binding takes precedence
over ordinary self-insertion:

```scheme
(keymap:bind! 'widget-editor ";" (keymap:call edit:insert! widget:target " — "))
```

## Key names

Ordinary printable keys are written literally: `"a"`, `"%"`, `")"`. Use
`SPC` for a space inside a specification.

Modifiers use the familiar prefixes:

- `C-a` through `C-z`, plus forms such as `C-@` and `C-_`
- `M-a`, `M-%`, `M-<`, and other Meta characters
- `C-M-_` for a combined Control-Meta character

Named terminal keys are:

- `RET`, `TAB`, `ESC`, `BS`, `DEL`, and `S-TAB`; `BACKSPACE` and `DELETE`
  are accepted for the two, which show as `BS` and `DEL`
- `UP`, `DOWN`, `LEFT`, `RIGHT`, `HOME`, and `END`
- `PGUP`, `PGDN`, `INSERT`, and `BEGIN`; `PAGEUP` and `PAGEDOWN` are
  accepted too and show short
- `F1` through `F12`; higher names through `F63` are also accepted
- Numeric-keypad names such as `KP-0`, `KP-ADD`, and `KP-ENTER`

Named keys accept `C-`, `M-` and `S-` modifiers, such as `M-S-UP` and
`C-LEFT`. The terminal must send a distinguishable sequence, and its own
shortcuts can intercept a key before e receives it. Finder and Buffet use
`F1`–`F6` for column sorting while their app is focused; these are app controls,
so a global binding lookup can still report the key as unbound.

Three pseudo-keys are bindable like any other. `PASTE` is the event a
bracketed paste produces, bound to `edit:paste!`. `SELF-INSERT` is what a
printable character without a binding of its own resolves to, in the mode's
context first, then the global map, and its command receives the character
through `head:typed-text`. Editor contexts bind it to an explicit insertion
call; Entry handles typed-text events for editable filters and prompts.
Bindings shows the pseudo-key as `any character`. Pointer bindings are
separate widget targets, inspected through the same forwarding machinery.
Mouse reports
themselves are handled before key dispatch: clicks, drags, releases, and
wheel events act directly and settle the transient echo area like keyboard
input. Pointer motion is not a key event: it updates hover feedback on
clickable text and controls and leaves the echo area alone. Hover uses bold
text with a dotted underline where supported, without moving keyboard focus.

Examples:

```scheme
(keymap:bind! 'widget-editor "C-c SPC" (keymap:call edit:set-mark! widget:target #t))
(keymap:bind! 'widget-editor "PGUP" (keymap:call edit:move! widget:target 'start))
(keymap:bind! 'widget-editor "C-c LEFT" (keymap:call edit:move! widget:target 'home))
```

Terminal protocols cannot distinguish every physical key combination. In
particular, some terminals configure the Backspace key to send `C-h`. Such a
terminal cannot distinguish physical Backspace from e's `C-h` help prefix;
configure it to send DEL if necessary.

## Removing and replacing bindings

`keymap:unbind!` creates a user-level unbinding, so a lower-priority default does
not become active again:

```scheme
(keymap:unbind! 'widget-editor "C-v")
(keymap:unbind! 'widget-editor "M-w")
```

Binding the same specification again replaces its effective meaning. An exact
user binding can also reclaim a key used as a default prefix:

```scheme
(keymap:bind! 'widget-editor "C-h" (keymap:call edit:delete! widget:target 'backward))
```

Here `C-h` deletes backward in the editor immediately instead of waiting for the default
`C-h k` chord. A user-defined longer chord still makes its initial keys act as
a prefix.

Bindings evaluated with `M-x` last for the current session. Put them in the
installation's `config.e` to apply them at startup and whenever configuration
is reloaded. Removing a line from `config.e` removes that override on the next
reload; configuration-owned registrations do not accumulate.

## Contextual keymaps

Widget contexts route through the focused tree. A view declares its
contexts, capture contexts and named actions; Bindings lists those along
the acquired route. The three-argument keymap form binds an explicit context
to a command, using `widget:target` for its receiver. For example:

```scheme
(keymap:bind! 'widget-search "M-i"
  (keymap:call widget:act! widget:target 'toggle-case))
(keymap:unbind! 'widget-search "M-c")
(keymap:bind! 'widget-prompt "M-p"
  (keymap:call prompt:history! widget:target 'previous))
```

Named widget actions use the ordinary declared forwarding chain. Entry
children handle text, caret motion and deletion; their host handles acceptance,
cancellation and history. Incremental search is an ordinary search/entry
composition, with `repeat`, `accept`, `cancel` and `toggle-case` actions.
`C-s` repeats, `M-c` toggles case, Enter/Escape accepts, and `C-g` restores
the starting selection. `search:replace!` performs a guarded replacement in
an explicit editor scope; there is no separate symbolic query-replace keymap.

Mode contexts and widget contexts both support complete chords. Terminal
views yield C-x and M-x in partial capture; C-] and the clickable ● / ◐
control toggle that view's policy. Full capture forwards those prefixes to
the child. After process exit its capture context disappears and the editor
child handles ordinary input. See [Terminal](TERMINAL.md) and [Widgets](WIDGETS.md).

## Inspecting bindings from Scheme

`keymap:binding` returns the effective command or action, or `#f` when the key is
unbound or has no explicit binding:

```scheme
(keymap:binding 'widget-editor "C-s")
(keymap:binding 'widget-search "M-c")
```

Code that has already read canonical events with `head:read-key-event` should use
`keymap:event-binding` instead. It accepts events directly, without reparsing the
space-separated configuration syntax:

```scheme
(let ([event (head:read-key-event)])
  (keymap:event-binding 'widget-search event))
```

`(head:read-key-event #f)` consumes mouse reports without applying them, which is
appropriate for modal interactions that must not let a click change the active
buffer while their state refers to the old one.

`keymap:command-key` performs the reverse lookup for a top-level command symbol and
returns one effective global key specification:

```scheme
(keymap:command-key 'eval:prompt!)
```

`keymap:command-keys` returns every effective global binding for the command. This is
the live lookup used by describe pages, so adding, replacing, or removing a
binding is reflected the next time the view redraws:

```scheme
(keymap:command-keys 'eval:prompt!)
```

`keymap:command-hint` formats a list of command symbols with their current keys. It is
primarily useful to extension modules when constructing status or help text.

## Defaults in extension modules

Modules should register suggested bindings with `keymap:bind-default!`, normally
inside `init!`:

```scheme
(define (init!)
  (keymap:bind-default! 'widget-editor "M-j" (keymap:call describe:at-point! widget:target)))
```

Context defaults use the corresponding three-argument form:

```scheme
(keymap:bind-default! 'widget-search "M-i" (keymap:call widget:act! widget:target 'toggle-case))
```

Defaults remain replaceable by `keymap:bind!` and `keymap:unbind!`. Registrations are
owned by their module, so reloading it retracts the old defaults before running
its new `init!`; user choices remain effective.

Use `keymap:bind!` in a module only when the module deliberately installs an
override rather than offering a default. For normal extension behavior,
`keymap:bind-default!` is the cooperative choice.
