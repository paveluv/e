# Key binding configuration

Every keyboard command in e is resolved through a keymap. Built-in and
extension-module bindings are defaults; bindings from `config.e` are user
overrides and take priority regardless of registration or module reload order.

Press `C-h k`, then a key or complete chord, to open `*help*`. The report shows
the resolved global command, where it was defined, shadowed definitions, and
any meanings the key has inside prompts, incremental search, or query-replace.

## Global bindings

`keymap:bind-key!` takes a key specification and a zero-argument command:

```scheme
(keymap:bind-key! "M-l" log-view:show!)
(keymap:bind-key! "C-c s" save!!)
(keymap:bind-key! "C-c C-f" find-file!!)
```

Global specifications may contain any number of space-separated key events.
This permits arbitrary prefixes; they are not limited to `C-x`. When a prefix
is entered, e waits for the rest of the chord and displays the partial sequence
in the echo area.

A command must be a procedure callable with no arguments. Existing commands
such as `save!!`, `undo!`, `beginning-of-buffer!`, and `other-window!` can be
used directly. A lambda can adapt a command that needs arguments:

```scheme
(keymap:bind-key! "M-g" (lambda () (goto-point! '(0 . 0))))
(keymap:bind-key! "C-c n" (lambda () (move-vertical! 10)))
```

Printable characters can also be bound. An explicit binding takes precedence
over ordinary self-insertion:

```scheme
(keymap:bind-key! ";" (lambda () (insert-text! " — ")))
```

## Key names

Ordinary printable keys are written literally: `"a"`, `"%"`, `")"`. Use
`SPC` for a space inside a specification.

Modifiers use the familiar prefixes:

- `C-a` through `C-z`, plus forms such as `C-@` and `C-_`
- `M-a`, `M-%`, `M-<`, and other Meta characters
- `C-M-_` for a combined Control-Meta character

Named terminal keys are:

- `RET`, `TAB`, `ESC`, `BACKSPACE`, `DELETE`, and `S-TAB`
- `UP`, `DOWN`, `LEFT`, `RIGHT`, `HOME`, and `END`
- `PAGEUP` and `PAGEDOWN`

Three pseudo-keys are bindable like any other. `PASTE` is the event a
bracketed paste produces. `SELF-INSERT` is what an unbound printable
character resolves to; its binding is the self-inserting command. `MOUSE-CLICK`
fires in a mode's context after a text click has placed point, so a mode can
act on the click (the markdown viewer follows links with it). Mouse reports
themselves are handled before key dispatch: clicks, drags, releases, and
wheel events act directly and settle the transient echo area like keyboard
input.

Examples:

```scheme
(keymap:bind-key! "C-c SPC" set-mark-command!)
(keymap:bind-key! "PAGEUP" beginning-of-buffer!)
(keymap:bind-key! "C-c LEFT" beginning-of-line!)
```

Terminal protocols cannot distinguish every physical key combination. In
particular, some terminals configure the Backspace key to send `C-h`. Such a
terminal cannot distinguish physical Backspace from e's `C-h` help prefix;
configure it to send DEL if necessary.

## Removing and replacing bindings

`keymap:unbind-key!` creates a user-level unbinding, so a lower-priority default does
not become active again:

```scheme
(keymap:unbind-key! "C-v")
(keymap:unbind-key! "M-w")
```

Binding the same specification again replaces its effective meaning. An exact
user binding can also reclaim a key used as a default prefix:

```scheme
(keymap:bind-key! "C-h" backspace!)
```

Here `C-h` runs `backspace!` immediately instead of waiting for the default
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
(keymap:bind-key! 'isearch "M-i" 'toggle-case)
(keymap:unbind-key! 'isearch "M-c")
(keymap:bind-key! 'prompt "C-u" 'kill)
(keymap:bind-key! 'query-replace "SPC" 'skip)
```

Context bindings use action symbols rather than command procedures because the
operation acts on the currently running prompt or search. Context keys are
individual decoded key events; global keymaps provide arbitrary multi-key
chords.

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
(keymap:bind-key! 'isearch "M-i" 'toggle-case)
(keymap:unbind-key! 'isearch "M-c")
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
(keymap:bind-key! 'prompt "C-u" 'kill)
(keymap:bind-key! 'prompt "M-p" 'up)
(keymap:bind-key! 'prompt "M-n" 'down)
(keymap:bind-key! 'prompt "M-RET" 'newline)
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
(keymap:bind-key! 'query-replace "r" 'replace)
(keymap:bind-key! 'query-replace "s" 'skip)
(keymap:bind-key! 'query-replace "q" 'stop)
```

## Inspecting bindings from Scheme

`keymap:key-binding` returns the effective command or action, or `#f` when the key is
unbound or has no explicit binding:

```scheme
(keymap:key-binding "C-s")
(keymap:key-binding 'isearch "M-c")
```

Code that has already read canonical events with `head:read-key-event` should use
`keymap:key-event-binding` instead. It accepts events directly, without reparsing the
space-separated configuration syntax:

```scheme
(let ([event (head:read-key-event)])
  (keymap:key-event-binding 'isearch event))
```

`(head:read-key-event #f)` consumes mouse reports without applying them, which is
appropriate for modal interactions that must not let a click change the active
buffer while their state refers to the old one.

`keymap:command-key` performs the reverse lookup for a top-level command symbol and
returns one effective global key specification:

```scheme
(keymap:command-key 'save!!)
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

Modules should register suggested bindings with `keymap:bind-default-key!`, normally
inside `init!`:

```scheme
(define (init!)
  (keymap:bind-default-key! "M-j" describe:at-point!))
```

Context defaults use the corresponding three-argument form:

```scheme
(keymap:bind-default-key! 'isearch "M-i" 'toggle-case)
```

Defaults remain replaceable by `keymap:bind-key!` and `keymap:unbind-key!`. Registrations are
owned by their module, so reloading it retracts the old defaults before running
its new `init!`; user choices remain effective.

Use `keymap:bind-key!` in a module only when the module deliberately installs an
override rather than offering a default. For normal extension behavior,
`keymap:bind-default-key!` is the cooperative choice.
