# App buffers

An app buffer is a dynamic, read-only buffer that may handle user input. A
view is an app without an input handler: it renders changing state but leaves
keys to the ordinary editor.

Apps look like buffers, participate in the buffer list, may appear in any
window, and carry `[]` in their status line. Their content is refreshed while
visible on every redraw.

## Registering an app

```scheme
(register-app! name refresh! handle-event!)
```

`name` is the buffer name and must not already belong to an ordinary buffer.
`refresh!` takes no arguments and updates the registered buffer with
`view-replace!` or `view-append!`. `handle-event!` receives one canonical
event string, such as `"UP"`, `"RET"`, `"MOUSE-CLICK"`, or `"WHEEL-UP"`.
For keyboard events it returns true when the app consumed the event; false
lets the normal global key dispatcher handle it. For `"MOUSE-CLICK"`, returning
the symbol `keep-focus` consumes the click but restores keyboard focus to the
previously focused window. Any other result follows the normal rule that
clicking app content focuses the app and places its cursor at the clicked
position. If the handler returns false—or the buffer is a view with no
handler—the press also starts an ordinary text selection, so dragging selects
from the clicked cell even though the buffer is read-only.
Registrations belong to their module and disappear transactionally on unload
or reload like modes, key bindings, and hooks.

## Input capture and propagation

App input is layered: an active prompt first, then the focused app, then e's
global bindings, then the ordinary buffer fallback such as self-insertion.
Most apps are partial: their handler consumes only their own controls and
returns false for everything else. Thus `*buffers*` owns navigation and row
activation while `M-x`, window commands, and other global bindings pass
through naturally.

An app that embeds a complete interactive environment can capture every event:

```scheme
(set-app-capture! app-buffer #t)
```

Its handler still receives events normally, but a false result is consumed
instead of propagating. This applies uniformly to keyboard, paste, mouse
presses, drags, releases, and wheel events.

A capturing app can grant e one complete global command:

```scheme
(escape-app-capture! "C-]" send-literal-control-bracket!)
```

If the next event is `C-]`, the supplied procedure sends the literal event.
Any other key begins ordinary global dispatch. Multi-key bindings remain in e
until complete, and commands retain control through their synchronous prompts.
When the command finishes, capture is recomputed from the focused buffer: it
resumes if the capturing app is active again, or the newly focused app/buffer
takes over. An unbound printable key follows the global self-insertion fallback
and therefore reports read-only in a read-only app; it is never silently sent
back to the embedded application.

The handler is optional. Thus these are equivalent:

```scheme
(register-view! "*example*" refresh!)
(register-app! "*example*" refresh!)
```

Use `display-app!` to display and focus an app. Unlike `display-buffer!`, it
records the window that was current before entry as the app's target.

Table-like apps can request shared presentation chrome:

```scheme
(set-app-presentation! app-buffer 1 #t 'default 'default)
```

The second argument is the number of sticky leading rows. The third is either
`#t` for a one-column vertical scrollbar on the configured side, `'left` or
`'right` for a fixed side, or `#f`. The optional fourth argument overrides
soft wrapping with `#t` or `#f`; `default` (and omission) follows the ordinary
window and global setting. A fifth argument selects `block`, `underline`,
`bar`, or the normal `default` cursor; these explicit shapes are steady.
Sticky rows, scrollbar geometry, cursor placement,
mouse hit-testing, and scrolling are handled together by the core and apply
to every window showing the app.

The same bar is enabled for ordinary buffers by default; `(scrollbar #f)`
hides it there. `(scrollbar-position 'left)` and `(scrollbar-position 'right)`
select the global side, which defaults to the right. An app's explicit side
overrides that position. `*buffers*` always enables its bar and follows the
global side.

## Current and target windows

While an app is active, the current window is the window displaying the app.
The target window is the window the user was working in immediately before
entering it. Entering by keyboard, switching buffers, or clicking an already
visible app all establish the target through the same mechanism.

The target may be another window showing the same app. This makes it easy to
swap the app to a different window: show the app in the intended target, focus
the other app window, then make a selection.

When the target window is deleted, the target becomes *ephemeral*. The app
remembers its buffer but has no window. Its next target action creates a fresh
window rather than replacing the app or appropriating an unrelated window. If
the terminal is too small to create one, the action leaves the app and target
intact and reports the failure.

The public accessors are:

```scheme
(target-window) ; opaque window token, or #f for an ephemeral app target
(target-buffer) ; buffer currently associated with that target
```

Outside an app they return the current window and current buffer.

An app normally performs its primary action with:

```scheme
(show-buffer-in-target! buffer)
```

If the target is another window, the app remains focused and that window
changes buffers. If the target is the app's own window, the selected buffer
replaces the app.

## The buffers app

`*buffers*` is the first interactive app. It renders live buffer status and
supports these controls:

Its heading is sticky at the top of every window. The remaining rows scroll
under it, with the configured edge showing the visible body's position and
extent.

- Up or `C-p`: move the active row up without changing the target.
- Down or `C-n`: move the active row down without changing the target.
- Enter: show the active row's buffer in the target window.
- Mouse click: select the clicked row and show it immediately without moving
  keyboard focus from the target window.
- Mouse wheel: while hovering over the app, move exactly one row per tick and
  show it immediately without moving keyboard focus.
- Status-bar click: focus the app window and make the previously focused window
  its target.

Status-bar clicks always focus their window; app handlers cannot override
them. `*buffers*` returns `keep-focus` for content clicks because the click's
purpose is to change the target rather than enter the app.

Outside the app, `M-Shift-Up` and `M-Shift-Down` switch the current window through the
same alphabetical buffer list.

The active row uses the `active` face. It can be customized like any other
face:

```scheme
(set-style! 'active '((background 24) (foreground white)))
```

Rows are always alphabetical, so visiting a buffer does not move it. The table
header is bold, and modified-buffer rows are italic. The table remains a live rendering: buffer creation,
removal, focus, modified state, read-only state, line count, mode, and file
changes appear on redraw.

While `*buffers*` has focus, its active row uses the `active` face. When focus
moves to another window, the row for that window's buffer continues to follow
it dynamically using the subtler `active-shadow` face.

When an app has keyboard focus, the target window's status bar begins with
`>`. This marker is deliberately part of the status bar rather than app
content, so it remains visible while navigating and works for every app. An
ephemeral target has no marker until its next action creates a window.

App cursors are buffer state: multiple windows showing one app mirror the same
active row. The focused app window paints it with `active`; other windows use
`active-shadow`.

Refresh failures are logged under the `app` component and shown in the echo
area. An unchanged failure is reported once instead of once per redraw; a
successful refresh clears it so a later failure is reported again.
