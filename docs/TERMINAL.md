# Terminal buffers

e can host a real pseudo-terminal inside an app buffer. Start one with
`C-c t` or:

```scheme
(terminal!!)
```

The command opens `*terminal*` in the current window and starts `$SHELL` as an
interactive shell in the current file's directory (or e's current working
directory for a file-less buffer). A second session is named
`*terminal*<2>`, and so on.

Pass a shell command to run it instead:

```scheme
(terminal!! "top")
(terminal!! "python3")
(terminal!! "e README.md")
(terminal!! "legmacs README.md")
```

This is a PTY, not redirected pipes. The child sees `TERM=xterm-256color`, a
controlling terminal, and the terminal buffer's actual row and column count.
Window changes propagate through `TIOCSWINSZ`/`SIGWINCH`, so interactive
shells, job control, screen-addressed programs such as `top`, and another
instance of e work normally.

## Input and leaving the terminal

Printable keys, control and Meta keys, arrows, Home/End, PageUp/PageDown,
Delete, F1–F12, and bracketed paste are sent to the child. Applications may
enable xterm mouse reporting; clicks and wheel reports are then forwarded
through the PTY, including through nested terminal emulators. Without mouse
reporting, wheel ticks scroll the local terminal history by one eighth of the
window. `Shift-PageUp` and `Shift-PageDown` move by a full window;
`Shift-wheel` explicitly selects local history even while the child reports
mouse input. Scrolling is per window when several windows mirror one terminal.
The terminal cursor disappears while that window is browsing history; the
next keyboard or paste input returns it to the live cursor before sending the
input. Ordinary mouse selection remains available when the child is not
tracking the mouse. A blinking block cursor marks the terminal's live input
position by default. Programs can change its shape and blinking behavior with
the standard `DECSCUSR` terminal sequence.

The italic status text distinguishes a focused terminal that is capturing
input (`running; capturing input, C-] to escape`), its temporarily escaped
state (`running; escaped`), and a terminal running in a passive window
(`running`). After the process exits, every window shows `exited`; capture is
disabled and the retained terminal buffer remains a read-only transcript with
the normal vertical read-only cursor. It is then an ordinary text buffer:
keyboard and mouse navigation, selection, and `M-w` copying work normally.
Killing this buffer terminates a process that is still running; deleting one
of several windows displaying it does not. Exiting e terminates every live
terminal process, including terminals whose buffers are not currently shown.

`C-]` temporarily suspends terminal capture for one complete global e command:

| Sequence | Action |
|----------|--------|
| `C-] C-]` | Send a literal `C-]` to the child |
| `C-] M-x` | Open M-x; capture resumes when its command finishes |
| `C-] C-x 2` | Run the complete global split-window binding |
| `C-] C-x o` | Focus the next e window |
| `C-] C-x k` | Run the ordinary kill-buffer command |

Any other key receives its normal global meaning. For example, `C-] a` attempts
e's ordinary self-insertion, which reports that the terminal buffer is
read-only; it does not send `a` to the child. After the command or prompt ends,
the focused buffer determines capture again. If the terminal is active, it
immediately resumes capturing. Status-bar clicks always remain editor-owned.

## Display model

The emulator maintains a fixed, non-wrapping cell grid and interprets the
common ECMA-48/VT and xterm sequences used for cursor motion, erasing,
insertion/deletion, scrolling regions, saved cursor position, line-drawing
characters, SGR attributes and 256/RGB colors, OSC metadata, and private
modes. The alternate screen is kept separate: entering `top`, less, or nested
e preserves the shell screen and restores it when the application exits.

The main screen retains scrollback; alternate-screen frames are never added
to it. Configure the maximum retained line count in `config.e`:

```scheme
(terminal-scrollback 10000) ; default
(terminal-scrollback 0)     ; disable scrollback
```

The terminal buffer is read-only from the editor's perspective. Its contents
come only from the PTY, but ordinary selection and copying still work whenever
the child has not requested mouse input.

## Scheme API

```scheme
(terminal!! [command])
(terminal-send! text)
(terminal-close! [buffer])
(terminal-scrollback [lines])
```

`terminal-send!` writes UTF-8 text to the current terminal's PTY. It is useful
for macros and automation; it does not append text directly to the buffer.
`terminal-close!` sends termination to the whole terminal process group,
closes the PTY, and reaps its session leader. Killing a terminal buffer calls
it automatically. A naturally exited process leaves its final screen visible
and marks the status line `process exited`. It also stops capturing input, so
ordinary editor chords such as `C-x b`, `C-x k`, and `C-x o` work immediately;
kill the buffer normally when it is no longer needed.

The OS-specific PTY creation, resize, cleanup, and process-group operations
live in `sys.e`. Escape parsing, screen state, scrollback, input translation,
and the app lifecycle live entirely in `terminal.e`; the core contains only
the generic app sizing, wrapping override, kill hook, key decoding, and
thread-safe redraw mechanisms used by this and other apps.
