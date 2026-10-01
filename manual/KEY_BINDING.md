# Key binding configuration

Every keyboard command in e is resolved through a keymap. Built-in and
extension-module bindings are defaults; bindings from `config.e` are user
overrides and take priority regardless of registration or module reload order.

Press `C-h k`, then a key or complete chord, to open `<help>`. The report shows
the resolved global command, where it was defined, shadowed definitions, and
any meanings the key has inside prompts, incremental search, or query-replace.

## Inspecting bindings

`C-x TAB` opens the read-only `<bindings>` inspector above the echo area.
It shows mouse bindings first, followed by the active window's keyboard
bindings and its widget command connections. Each row shows the public API
and its documentation. Keyboard bindings are grouped by app and mode context,
then by global bindings. The listing fits its window, with a scrollbar and
independent scrolling in each view. `(bindings:show!)` opens the same inspector
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
line puts it away, as `(bindings:hide!)` does. `(bindings:open!)` shows the listing
in the current window instead, for the buffer that window shows, and
`C-x TAB` pages it there. Apps bind keys in their contexts to public commands.
In `<finder>`, Enter invokes the table's `activate` connection and the listing
traces it to `finder:choose!`. These commands run from M-x and are described
by `C-h k`; an app that captures keys, the terminal, lists its toggle and a
row saying what it takes. In the pop-up itself, an app's there, the conflicts browser's say, it lists that app's keys and keeps to them while the pop-up stays current.

## Global bindings

`keymap:bind!` takes a key specification and a zero-argument command:

```scheme
(keymap:bind! "M-l" log-view:show!)
(keymap:bind! "C-c s" edit:save!)
(keymap:bind! "C-c C-f" edit:visit-file!)
```

Global specifications may contain any number of space-separated key events.
This permits arbitrary prefixes; they are not limited to `C-x`. When a prefix
is entered, e waits for the rest of the chord and displays the partial sequence
in the echo area.

A command must be a procedure callable with no arguments. Existing commands
such as `edit:save!`, `edit:undo!`, `edit:beginning-of-buffer!`, and `window:focus-next!` can be
used directly. `keymap:call` adapts a command that needs arguments, and the
binding then reads as the call it makes in the bindings listing and under `C-h k`;
a lambda works too, but shows as an anonymous command:

```scheme
(keymap:bind! "M-g" (keymap:call head:goto! '(0 . 0)))
(keymap:bind! "C-c n" (keymap:call edit:move-vertical! 10))
```

Two structural actions describe themselves where a lambda shows as an
anonymous command. `keymap:call` applies a command to what producer procedures
or nested `keymap:call` expressions return when the key is pressed, and to
any other argument as given. `keymap:run!` executes that same structured call;
`keymap:prefill` opens M-x with the command's call typed up to its next
argument, so completion asks for it:

```scheme
(keymap:bind! "C-x k" (keymap:call edit:kill-buffer! head:current-buffer))
(keymap:bind! 'finder "F2"
  (keymap:call table:toggle-sort!
    (keymap:call widget:descendant widget:target 'table) 'size))
(keymap:bind! "C-c a" (keymap:prefill edit:answer!))
```

`C-h k` shows the first as `(edit:kill-buffer! (head:current-buffer))` and
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
(table:invoke! (model 110) 'activate) ; default action, opening a row in Buffet
(table:invoke! (model 110) 'trash)    ; invoke the table's trash connection
(table:invoke! (model 110) 'delete)   ; invoke the table's delete connection
```

Buffet's `C-k`, for example, is displayed as:

```scheme
(table:invoke! (widget:descendant (model 109) 'table) 'trash)
```

The table supplies its hovered or selected row and result basis to the
connected action. Bindings shows the full registered forwarding chain below
the key, with each operation's own documentation:

```scheme
(table:invoke! (widget:descendant (model 109) 'table) 'trash)
  → (table:invoke! (model 110) 'trash)
    → (widget:invoke! (model 110) 'trash selection basis)
      → (widget:act! (model 109) 'kill selection basis)
        → (buffet:kill! (model 109) selection basis)
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
same edoc types as completion; a model argument is `(model N)`, while an
ordinary list argument stays quoted.

`(mouse:bindings)` returns the current `(gesture action)` pairs as data;
an optional `(column . row)` selects another screen cell, using one-based
coordinates. `(mouse:position)` reports the physical pointer's last known
cell even after keyboard input clears hover emphasis, or `#f` if unknown.
Gestures include `(click primary ())`, `(click secondary ())`,
`(click primary (shift))`, `(drag primary ())`, and `(wheel down ())`.
`mouse:gesture-text` spells them for help. Actions use `keymap:call`, just
like keyboard bindings. Legacy buffer apps expose `mouse:click!` and
`mouse:scroll!`, which deliver input through their normal routes at the
given screen coordinates.

Printable characters can also be bound. An explicit binding takes precedence
over ordinary self-insertion:

```scheme
(keymap:bind! ";" (keymap:call edit:type! " — "))
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
through `head:typed-text`: globally `(keymap:call edit:type! head:typed-text)`
inserts it, while in `<finder>` and `<buffet>` the context binds it to
`extend-filter!`, so typing grows the filter. The bindings listing shows the
pseudo-key as `any character`. `MOUSE-CLICK`
fires in a mode's context after a text click has placed point, so a mode can
act on the click (the markdown viewer follows links with it). Mouse reports
themselves are handled before key dispatch: clicks, drags, releases, and
wheel events act directly and settle the transient echo area like keyboard
input. Pointer motion is not a key event: it updates hover feedback on
clickable text and controls and leaves the echo area alone. Hover uses bold
text with a dotted underline where supported, without moving keyboard focus.

Examples:

```scheme
(keymap:bind! "C-c SPC" edit:set-mark-command!)
(keymap:bind! "PGUP" edit:beginning-of-buffer!)
(keymap:bind! "C-c LEFT" edit:beginning-of-line!)
```

Terminal protocols cannot distinguish every physical key combination. In
particular, some terminals configure the Backspace key to send `C-h`. Such a
terminal cannot distinguish physical Backspace from e's `C-h` help prefix;
configure it to send DEL if necessary.

## Removing and replacing bindings

`keymap:unbind!` creates a user-level unbinding, so a lower-priority default does
not become active again:

```scheme
(keymap:unbind! "C-v")
(keymap:unbind! "M-w")
```

Binding the same specification again replaces its effective meaning. An exact
user binding can also reclaim a key used as a default prefix:

```scheme
(keymap:bind! "C-h" edit:backspace!)
```

Here `C-h` runs `edit:backspace!` immediately instead of waiting for the default
`C-h k` chord. A user-defined longer chord still makes its initial keys act as
a prefix.

Bindings evaluated with `M-x` last for the current session. Put them in the
installation's `config.e` to apply them at startup and whenever configuration
is reloaded. Removing a line from `config.e` removes that override on the next
reload; configuration-owned registrations do not accumulate.

## Contextual keymaps

Some interactions interpret keys using local state. Their bindings use a
three-argument form consisting of the context, key, and semantic action:

```scheme
(keymap:bind! 'isearch "M-i" 'toggle-case)
(keymap:unbind! 'isearch "M-c")
(keymap:bind! 'prompt "C-u" prompt:kill!)
(keymap:bind! 'query-replace "SPC" 'skip)
```

The search and query-replace contexts use action symbols because the
operation acts on the currently running search; the prompt context binds the
`prompt:` commands, which ask the open prompt for their action. Their keys are
individual decoded key events; global keymaps provide arbitrary multi-key
chords.

A context may also come from a buffer's state rather than its mode. An app
registers it with a predicate, `(mode:add-context! 'conflicted conflicted?)`
say, and every buffer the predicate holds of has the context, before its
mode's, so keys bound in it work only while the state holds, keep their
other meanings elsewhere, and the bindings listing shows them only where they
work.

Buffer-mode contexts bind command procedures and complete chords. Widget
contexts route recursively through the focused tree. Terminal views yield C-x
and M-x in partial capture; C-] and the clickable ● / ◐ status control toggle
that view's policy. Full capture forwards those prefixes to the child.
Shift-PageUp/Down remain viewport commands in either state. After process exit,
its capture context disappears and the editor child handles ordinary input.
See [Terminal buffers](TERMINAL.md) and [Widgets](WIDGETS.md).

### `isearch`

Available actions are:

- `repeat`: find the next match, or recall the previous needle when empty
- `cancel`: restore the point where the search began
- `accept`: keep the current match and leave search
- `accept-dispatch`: accept, then run the key's global binding
- `toggle-case`: switch this search between folded and exact matching
- `delete-character`: remove the last character from the needle

Example:

```scheme
(keymap:bind! 'isearch "M-i" 'toggle-case)
(keymap:unbind! 'isearch "M-c")
```

Printable keys without contextual actions extend the search. Other unhandled
keys fall through to the global map while search remains active; movement keys
use `accept-dispatch` by default.

### `prompt`

Available actions are:

- `accept` and `cancel`
- `beginning`, `end`, `backward`, and `forward`
- `up` and `down`, which move through wrapped input or prompt history
- `delete-forward` and `delete-backward`
- `kill` and `yank`
- `complete` and `alternate-complete`
- `inspect`, used by the Scheme prompt's symbol inspector
- `newline`, used by M-x for an indented logical newline
- `paste`

Example:

```scheme
(keymap:bind! 'prompt "C-u" 'kill)
(keymap:bind! 'prompt "M-p" 'up)
(keymap:bind! 'prompt "M-n" 'down)
(keymap:bind! 'prompt "M-RET" 'newline)
```

Printable keys without prompt actions insert themselves. Other unhandled keys
are ignored by the prompt.

### `query-replace`

The ordinary configurable actions are:

- `replace`: replace the highlighted match
- `skip`: leave it unchanged and continue
- `stop`: finish query-replace at this match

Example:

```scheme
(keymap:bind! 'query-replace "r" 'replace)
(keymap:bind! 'query-replace "s" 'skip)
(keymap:bind! 'query-replace "q" 'stop)
```

## Inspecting bindings from Scheme

`keymap:binding` returns the effective command or action, or `#f` when the key is
unbound or has no explicit binding:

```scheme
(keymap:binding "C-s")
(keymap:binding 'isearch "M-c")
```

Code that has already read canonical events with `head:read-key-event` should use
`keymap:event-binding` instead. It accepts events directly, without reparsing the
space-separated configuration syntax:

```scheme
(let ([event (head:read-key-event)])
  (keymap:event-binding 'isearch event))
```

`(head:read-key-event #f)` consumes mouse reports without applying them, which is
appropriate for modal interactions that must not let a click change the active
buffer while their state refers to the old one.

`keymap:command-key` performs the reverse lookup for a top-level command symbol and
returns one effective global key specification:

```scheme
(keymap:command-key 'save!)
```

`keymap:command-keys` returns every effective global binding for the command. This is
the live lookup used by describe pages, so adding, replacing, or removing a
binding is reflected the next time the view redraws:

```scheme
(keymap:command-keys 'eval:run!)
```

`keymap:command-hint` formats a list of command symbols with their current keys. It is
primarily useful to extension modules when constructing status or help text.

## Defaults in extension modules

Modules should register suggested bindings with `keymap:bind-default!`, normally
inside `init!`:

```scheme
(define (init!)
  (keymap:bind-default! "M-j" describe:at-point!))
```

Context defaults use the corresponding three-argument form:

```scheme
(keymap:bind-default! 'isearch "M-i" 'toggle-case)
```

Defaults remain replaceable by `keymap:bind!` and `keymap:unbind!`. Registrations are
owned by their module, so reloading it retracts the old defaults before running
its new `init!`; user choices remain effective.

Use `keymap:bind!` in a module only when the module deliberately installs an
override rather than offering a default. For normal extension behavior,
`keymap:bind-default!` is the cooperative choice.
