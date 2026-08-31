# Terminal buffers

e can host a real pseudo-terminal inside an app buffer. Start one with
`C-c t` or:

```scheme
(terminal!!)
```

The command opens `*terminal*` in the current window and starts
`terminal-shell` as an interactive shell in the current file's directory (or
e's current working directory for a file-less buffer). It defaults to
`$SHELL`, falling back to `/bin/sh`. A second session is named
`*terminal*<2>`, and so on.

Set the shell in `config.e` when desired:

```scheme
(terminal-shell "/bin/bash")
```

Pass a shell command to run it instead:

```scheme
(terminal!! "top")
(terminal!! "python3")
(terminal!! "e README.md")
(terminal!! "legmacs README.md")
```

Explicit command strings are interpreted by the configured shell with `-c`,
so quoting, pipelines, redirection, and compound shell commands work. A bare
`(terminal!!)` executes the configured shell directly.

This is a PTY, not redirected pipes. The child sees `TERM=xterm-256color`, a
controlling terminal, and the terminal buffer's actual row and column count.
Window changes propagate through `TIOCSWINSZ`/`SIGWINCH`, so interactive
shells, job control, screen-addressed programs such as `top`, and another
instance of e work normally. The ioctl and signal are issued only when the
PTY grid dimensions actually change; ordinary output redraws never interrupt
the child with spurious resize notifications.

## Input and leaving the terminal

Printable keys, control and Meta keys, arrows, Home/End, Insert/Delete,
PageUp/PageDown, F1–F63, application-keypad keys, their xterm modifier
combinations, and bracketed paste are sent to the child. Applications may
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

Mouse reports support the original X10 coordinates, UTF-8 extended
coordinates (`1005`), SGR coordinates (`1006`), and urxvt coordinates
(`1015`); SGR takes precedence when a child enables several encodings. Focus
reporting mode (`1004`) sends `CSI I` and `CSI O` as editor focus enters and
leaves a terminal window. Mirrored windows share one terminal mode state,
while each real focus transition produces only one report.

The `xterm-256color` Meta mode (`1034`) is honored dynamically. Meta keys use
the ordinary ESC prefix by default; while the mode is enabled, single-byte
Meta and Control-Meta characters are encoded by setting their high bit.

The advertised memory-lock controls are also stateful: `ESC l` preserves rows
above the cursor while scrolling continues from the cursor row downward, and
`ESC m` restores the active vertical region's normal scrolling behavior.
Partial-width margins compose with the locked scroll rectangle.

The terminfo media-copy controls use a virtual printer sink. Printing the
screen records its cell rows, and printer-controller mode diverts subsequent
output until its termination sequence instead of echoing it to the grid. The
headless emulator exposes both the controller state and accumulated output for
tests; e never invokes a host printer or command implicitly.

The italic status hint distinguishes a focused terminal that is capturing
input (`▶ capturing input, C-] to escape`), its temporarily escaped state
(`▶ escaped`), and a terminal running in a passive window (`▶`). After the
process exits, every window shows `■`; capture is
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
modes. Both ESC-prefixed and 8-bit C1 forms are accepted. Device attributes,
status, and cursor-position probes receive terminal replies. The alternate
screen is kept separate: entering `top`, less, or nested e preserves the shell
screen and restores it when the application exits.
DEC screen-reverse mode is applied non-destructively, including the brief
reverse-video transition used by terminfo's visual bell capability.
BEL never produces sound or changes e's global echo area. It briefly replaces
the terminal buffer's `▶` status marker with a red `♪`; both occupy the same cell
after the single mode/status spacer. Mirrored windows show the same
buffer-owned indication without shifting their status text. Its asynchronous expiry cannot
delay diagnostic text or later PTY input.

DEC left/right margin mode (`DECSLRM`, enabled by `DECLRMM`) composes with the
vertical scrolling region. Cursor addressing, wrapping, character editing,
line insertion/deletion, and scrolling honor the resulting rectangle; a
partial-width scroll does not leak its untouched columns into scrollback.

The grid stores Unicode grapheme strings in terminal cells. Combining marks,
emoji presentation selectors, regional-indicator flags, and ZWJ emoji remain
one grapheme; CJK and other wide characters occupy two cells without being
split by editing or reflow. Resizing reflows primary-screen scrollback by its
recorded soft-wrap boundaries while preserving explicit newlines, styles, and
the logical cursor position. The primary screen also reflows while an
alternate-screen application is active; the alternate screen itself remains a
fixed application grid.
OSC 0, 1, and 2 titles rename the buffer dynamically to the title wrapped in
asterisks, such as `*bash*`; duplicate names receive a numeric suffix.
OSC 4 changes and queries the 256-color palette, including multiple indexed
colors in one command; OSC 104 restores selected entries or the complete
xterm palette. OSC 10 and 11 change or query the default foreground and
background, while OSC 110 and 111 restore them. Colors accept `rgb:` notation
with one to four hexadecimal digits per component and `#rrggbb`. Palette and
default changes are state, not paint commands: they immediately restyle cells
already on screen as well as subsequent output.

DCS queries are parsed separately from ignored string-control metadata.
`DECRQSS` reports the effective SGR attributes and active vertical or
horizontal margins, with an explicit failure reply for unsupported requests.
`XTGETTCAP` decodes hexadecimal capability names and reports the terminal name,
color count, RGB support, and other advertised color limits; unknown names
receive the protocol's negative reply.

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
(terminal-shell [path])
```

`terminal-send!` writes UTF-8 text to the current terminal's PTY. It is useful
for macros and automation; it does not append text directly to the buffer.
`terminal-shell` gets or sets the shell executable used by future terminal
buffers; changing it does not affect processes that are already running.
`terminal-close!` sends `SIGTERM` to the whole terminal process group and gives
it a short, bounded cleanup period before using `SIGKILL`. It closes the PTY
and reaps its session leader without allowing a stubborn child to hold up the
editor. Killing a terminal buffer calls it automatically. A naturally exited
process leaves its final screen visible and marks the status line `process
exited`. It also stops capturing input, so ordinary editor chords such as
`C-x b`, `C-x k`, and `C-x o` work immediately; kill the buffer normally when
it is no longer needed.

The same state machine can run without a PTY or editor buffer. This is useful
for tests, protocol experiments, and tools that need structured terminal
output:

```scheme
(define vt (make-terminal-emulator 24 80))
(terminal-emulator-feed! vt "\x1b;[2J\x1b;[10;20Hhello")
(terminal-emulator-resize! vt 40 100)
(terminal-emulator-screen vt)       ; copied vector of cell rows
(terminal-emulator-styles vt)       ; copied vector of cell-style rows
(terminal-emulator-state vt)        ; dimensions, cursor, and active modes
(terminal-emulator-input vt "UP")  ; mode-aware input bytevector
(terminal-emulator-replies vt)      ; DSR/DA and other protocol replies
```

`terminal-emulator-feed!` currently accepts decoded Scheme text. The live PTY
reader performs UTF-8 decoding before feeding the same state machine.

The OS-specific PTY creation, resize, cleanup, and process-group operations
live in `sys.e`. The PTY session leader directly executes the configured shell,
adding `-c command` only when a command is supplied; there is no intermediate
`system()` process, and setup failures are written to the child terminal before
it exits. Escape parsing, screen state, scrollback, input translation, and the
app lifecycle live entirely in `terminal.e`; the core contains only the generic
app sizing, wrapping override, kill hook, key decoding, and thread-safe redraw
mechanisms used by this and other apps.
