# Terminal buffers

e can host a real pseudo-terminal inside an app buffer. Start one with
`C-c t` or:

```scheme
(terminal:open!)
```

The command opens `*terminal*` in the current window and starts
`terminal:shell` as an interactive shell in the current file's directory (or
e's current working directory for a file-less buffer). It defaults to
`$SHELL`, falling back to `/bin/sh`. A second session is named
`*terminal 2*`, and so on. These are shared buffers: their actual live text
and scrollback are readable through `store:` while the process runs.
Output keeps the program's colors without temporary edit-author highlights,
including when several heads view the same terminal session.

Set the shell in `config.e` when desired:

```scheme
(terminal:shell "/bin/bash")
```

Pass a shell command to run it instead:

```scheme
(terminal:open! "top")
(terminal:open! "python3")
(terminal:open! "e README.md")
(terminal:open! "legmacs README.md")
```

Explicit command strings are interpreted by the configured shell with `-c`,
so quoting, pipelines, redirection, and compound shell commands work. A bare
`(terminal:open!)` executes the configured shell directly.

This is a PTY, not redirected pipes. The child sees `TERM=xterm-256color`, a
controlling terminal, and the terminal buffer's actual row and column count.
Window changes propagate through `TIOCSWINSZ`/`SIGWINCH`, so interactive
shells, job control, screen-addressed programs such as `top`, and another
instance of e work normally. The ioctl and signal are issued only when the
PTY grid dimensions actually change; ordinary output redraws never interrupt
the child with spurious resize notifications. The actor that last sent input
controls the shared grid's size. Other views clip it to their own windows;
focus reports alone do not take control of its size or color scheme.

## Input and leaving the terminal

Live terminals start with **partial capture** (`◐`). `C-x` and `M-x` reach e, so
`C-x 2`, `C-x k`, and M-x work directly. Press `C-]` or click the capture
indicator in the status bar to enable **full capture** (`●`), forwarding
those keys to the child too. Use full capture for another editor such as Emacs.
`C-]` always toggles immediately; `Shift-PageUp/Down` remain available for local
scrollback in either mode. Hovering over the indicator makes it bold with a
muted dotted underline.

Printable keys, control and Meta keys, arrows, Home/End, Insert/Delete,
PageUp/PageDown, F1–F63, application-keypad keys, their xterm modifier
combinations, and bracketed paste are sent to the child, except for the
editor controls above. Applications may
enable xterm mouse reporting; clicks and wheel reports are then forwarded
through the PTY, including through nested terminal emulators. Without mouse
reporting, wheel ticks scroll the local terminal history by one eighth of the
window. `Shift-PageUp` and `Shift-PageDown` move by a full window;
`Shift-wheel` explicitly selects local history even while the child reports
mouse input. Scrolling is per window when several windows mirror one terminal.
The editor's cursor replaces the terminal cursor while that window is browsing
history; the next input sent to the child returns it to the live cursor.
Toggling capture preserves the cursor and scrollback position.
Ordinary mouse selection remains available when the child is not
tracking the mouse. A blinking block cursor marks the terminal's live input
position by default. Programs can change its shape and blinking behavior with
the standard `DECSCUSR` terminal sequence.

Mouse reports support the original X10 coordinates, UTF-8 extended
coordinates (`1005`), SGR coordinates (`1006`), and urxvt coordinates
(`1015`); SGR takes precedence when a child enables several encodings. Focus
reporting mode (`1004`) sends `CSI I` and `CSI O` as editor focus enters and
leaves a terminal window. Mirrored windows share the child's terminal protocol
modes; the capture preference remains local to each e window. Each real focus
transition produces only one report.

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
headless emulator exposes both the controller state and accumulated output;
e never invokes a host printer or command implicitly.

Every live terminal window shows its process indicator and capture preference:
`▶ ◐` or `▶ ●`. A focused window also shows `C-] toggle capture`.
The preference belongs to the window; other windows and attached heads keep
their own choice. A split copies the
current choice into the new window, after which each is independent. Switching
buffers preserves the window preference; opening a new terminal resets it to
partial capture. Named-head reattachment restores the saved choice.

After the process exits, `■` replaces the running indicator and the capture
symbol and hint disappear. The terminal keymap becomes inactive: `C-]` is
unbound unless you gave it a global binding. The retained terminal buffer is
a read-only transcript with the normal vertical read-only cursor.
It is then an ordinary text buffer:
keyboard and mouse navigation, selection, and `M-w` copying work normally.
Killing this buffer terminates a process that is still running; deleting one
of several windows displaying it does not. Stopping
the daemon terminates every live terminal process, including terminals whose
buffers are not currently shown. Quitting an attached head only detaches that
screen; the daemon's terminals keep running.
On a successful restart or SIGTERM/SIGINT stop, the last published terminal
text is saved as an ordinary read-only buffer. Processes, emulator state and
terminal colors are not restored. Output arriving during the save or process
termination may be absent. A failed save leaves the same terminal process
running and publication resumes.

With partial capture:

| Sequence | Action |
|----------|--------|
| `M-x` | Run an e command or Scheme expression |
| `C-x 2` / `C-x 3` | Split the e window |
| `C-x o` | Focus the next e window |
| `C-x k` | Open the ordinary kill-buffer prompt |
| `C-]` or click `◐` | Enable full capture for this window |

Complete editor chords and their prompts stay in e. Subsequent input follows
the newly focused buffer. Status-bar clicks always remain editor-owned; clicking
an unfocused window's capture indicator focuses it and toggles only its capture
preference.

Run `terminal:yank!` through M-x to paste the kill ring into the child.
`C-]` is reserved for the toggle; to send its literal byte, evaluate
`(terminal:send! "\x1d;")` through M-x. `C-] C-]` now toggles twice.

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
the terminal buffer's `▶` status marker with `♪`; both occupy the same cell
after the single mode/status spacer. Mirrored windows show the same
buffer-owned indication without shifting their status text. Its asynchronous expiry cannot
delay diagnostic text or later PTY input.

DEC left/right margin mode (`DECSLRM`, enabled by `DECLRMM`) composes with the
vertical scrolling region. Cursor addressing, wrapping, character editing,
line insertion/deletion, and scrolling honor the resulting rectangle; a
partial-width scroll does not leak its untouched columns into scrollback.

DEC double-size lines (`DECDWL`, `DECDHL`, `DECSWL`) are per-row display
attributes over unchanged buffer content: a decorated row addresses half
the columns and presents its left half one character per two cells, using
Unicode fullwidth forms for ASCII, while `DECSWL` reveals the retained
full-width content again. A cell grid cannot stretch glyphs vertically, so
the two `DECDHL` halves each render as double-width. Decorated rows enter
scrollback in their displayed form and revert to single width when a
resize reflows the primary screen.

Color-scheme change notifications (private mode 2031 with DSR 996/997)
pass through: e subscribes to its host at startup, remembers the reported
scheme, answers children's `CSI ?996n` queries from it, and forwards each
host report to children that subscribed. A subscribing child also hears
the current scheme immediately when it is known. Under a host without the
feature the queries stay unanswered, exactly as they would against that
host directly.

Character-set designations cover ASCII, DEC special graphics with its
control pictures, and the DEC national replacement sets (British, Dutch,
Finnish, French, French Canadian, German, Italian, Norwegian/Danish,
Spanish, Swedish, and Swiss), invoked through G0/G1 with SI/SO. The NRC
enable mode (`?42`) is tracked and reported through DECRQM; the sets
themselves are always available, as in xterm.

The grid stores Unicode grapheme strings in terminal cells. Combining marks,
emoji presentation selectors, regional-indicator flags, and ZWJ emoji remain
one grapheme; CJK and other wide characters occupy two cells without being
split by editing or reflow. Resizing reflows primary-screen scrollback by its
recorded soft-wrap boundaries while preserving explicit newlines, styles, and
the logical cursor position. The primary screen also reflows while an
alternate-screen application is active; the alternate screen itself remains a
fixed application grid.

After the child exits, its last text remains an ordinary read-only buffer.
Wide and combining text retains the same cursor, selection, wrapping, and
mouse geometry as file buffers, including in side-by-side panes.

OSC 0, 1, and 2 title changes rename the shared buffer to the title wrapped in
stars, such as `*bash*`; store name collisions receive a suffix such as
`*bash*<2>`. An unchanged title does not overwrite a later user rename.
OSC 8 hyperlinks remain attached to their cells through editing, scrolling,
scrollback reflow, and alternate-screen rendering. They enter e's generic
buffer hyperlink layer, which emits OSC 8 to the host terminal around the
corresponding visible cells. Thus, links produced by an application inside an
e terminal remain available to the outer terminal even when their labels are
not URLs. Hover applies e's shared bold, dotted underline to the label in
that head's window, without moving the child's cursor or sending it an input
event.
OSC 52 clipboard writes from terminal children are decoded into exact UTF-8
text and, by default, stored in the last input actor's head kill ring. The
receiving head's echo area and `<log>` report
the terminal buffer that supplied the clipboard. Disable this independently
of outbound clipboard forwarding in `config.e`:

```scheme
(terminal:forward-clipboard-to-kill-ring #f) ; default is #t
```

OSC 52 clipboard queries are ignored: a child may offer text to its containing
editor, but it cannot read unrelated contents from e's kill ring. When
`edit:forward-kill-ring-to-system-clipboard` is enabled, imported text follows the
same outbound path as `M-w` and `C-k`, allowing it to continue through another
multiplexer or supporting host terminal.
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

The main screen retains scrollback behind the alternate grid; alternate-screen
frames are never added to that history. Configure the maximum retained line count in `config.e`:

```scheme
(terminal:scrollback 10000) ; default
(terminal:scrollback 0)     ; disable scrollback
```

The terminal buffer is read-only from the editor's perspective. Its contents
come only from the PTY, but ordinary selection and copying still work whenever
the child has not requested mouse input.

## Unsupported features

When a child sends a terminal sequence that e does not implement, e reports
`*buffer-name*: Unsupported <feature>` in the echo area and
`<log>`. Each distinct feature is reported only once per terminal buffer, so a
full-screen program cannot flood the log by emitting it on every redraw.
Diagnostics include the identifying CSI parameters or protocol selector but
omit arbitrary OSC and DCS payloads, which may contain private application
data. Unknown control strings and character controls are reported as well.
Headless emulators record the same signatures, readable through
`vt:emulator-unsupported`.

## Scheme API

```scheme
(terminal:open! [command])
(terminal:send! text)
(terminal:close! [buffer])
(terminal:scrollback [lines])
(terminal:shell [path])
```

`terminal:send!` writes UTF-8 text to the current terminal's PTY. It is useful
for macros and automation; it does not append text directly to the buffer.
`terminal:shell` gets or sets the shell executable used by future terminal
buffers; changing it does not affect processes that are already running.
`terminal:close!` sends `SIGTERM` to the whole terminal process group and gives
it a short, bounded cleanup period before using `SIGKILL`. It closes the PTY
and reaps its session leader without allowing a stubborn child to hold up the
editor. Killing a terminal buffer calls it automatically. A naturally exited
process leaves its final screen visible and marks the status line `■`.
It also stops capturing input, so ordinary editor chords such as
`C-x b`, `C-x k`, and `C-x o` work immediately; kill the buffer normally when
it is no longer needed.

The base's `vt` library also runs without a PTY or editor buffer. This is useful
for protocol experiments and tools that need structured terminal
output. The emulator is the base implementation, `lib/base/service/vt.sls`;
an attached head's `vt` client exposes terminal service calls and does not
contain the emulator:

```scheme
(import (prefix (vt) vt:))
(define vt (vt:make-emulator 24 80))
(vt:emulator-feed! vt "\x1b;[2J\x1b;[10;20Hhello")
(vt:emulator-resize! vt 40 100)
(vt:emulator-frame vt)        ; owned coherent frame, or #f during mode 2026
(vt:emulator-screen vt)       ; copied vector of cell rows
(vt:emulator-styles vt)       ; copied vector of cell-style rows
(vt:emulator-hyperlinks vt)   ; copied vector of cell link metadata
(vt:emulator-state vt)        ; dimensions, cursor, and active modes
(vt:emulator-input vt "UP")   ; mode-aware key bytevector
(vt:emulator-mouse-input vt 0 20 8 #f) ; mode-aware mouse bytevector
(vt:emulator-replies vt)      ; DSR/DA and other protocol replies
(vt:emulator-unsupported vt)  ; reported unsupported-feature signatures
```

Mouse coordinates are one-based. The numeric code uses the xterm button and
modifier bits, and the final argument distinguishes a release from a press or
motion event. The procedure returns `#f` while mouse tracking is disabled.
Hyperlink cells are either `#f` or `(URI id)`, where `id` may itself be `#f`.
Style cells are `plain` or complete SGR parameter strings such as `"0;31"`;
the leading reset prevents inheritance from the preceding cell. They do not
allocate named faces. `emulator-state` includes the `cursor-style` symbol.

`vt:emulator-frame` reads text and presentation together:

```scheme
;; (text rows cursor size facts)
'(#("界éZ ")
  ((0 #(plain plain plain plain plain) #(#f #f #f #f #f)
      ((clusters (1 . 2) (2 . 1) (1 . 1) (1 . 1)))))
  (0 4 #t) (1 5) ((cursor-style . blinking-block)))
```

Text contains displayed strings, including main-screen scrollback. While the
alternate screen is active it contains only that screen; main history stays
in the emulator and reappears on leaving alternate mode. Rows are complete
`(row styles hyperlinks attributes)` entries suitable for
[surface publication](APPS.md#publishing-shared-rendition). Cluster pairs
give character and cell counts. The zero-based cursor is
`(buffer-row cell-column visible?)`; `(rows cols)` describes the live grid,
so historical row widths may differ. Facts currently contain `cursor-style`.
This API returns a full frame; a publisher chooses which text and row changes
to commit. The live base app uses the same capture with main history retained
before the alternate grid, then publishes attributed text edits and surface
patches independently of head redraws. Mode 2026 delays those publications
for up to one second; process exit flushes a held final frame immediately.

Public feed, resize, and read operations serialize through the emulator's
lock. Every read returns independently owned mutable data. Use `emulator-frame`
when several layers must describe the same instant; separate read calls may
observe different updates. It returns `#f` during a child's mode 2026 hold,
bounded to one second. Older inspection APIs continue reading parsed state
during that hold. Reading a frame does not consume pending output.

`vt:emulator-feed!` currently accepts decoded Scheme text. The live PTY
reader performs UTF-8 decoding before feeding the same state machine.

The OS-specific PTY creation, resize, cleanup, and process-group operations
live in `sys.sls`. The PTY session leader directly executes the configured shell,
adding `-c command` only when a command is supplied; there is no intermediate
`system()` process, and setup failures are written to the child terminal before
it exits. Escape parsing, screen state, scrollback, input translation, and the
app lifecycle live in `vt.sls`, with no head or painter dependency. `terminal.sls`
provides commands, escape/paging bindings, and local clipboard/diagnostic
presentation. The shared app adapter owns each window's following and input
projection. Killing the store buffer closes its process even when no head is
looking at it. Quitting e detaches the head; the terminal continues in the
daemon and can be displayed from another head or the next attachment.

Code that runs without a head can open and address the producer directly:

```scheme
(vt:open! actor command-or-#f directory rows cols [color-scheme]) ; store id
(vt:send! actor id text (list rows cols) paste? [color-scheme])
(vt:close! id)
```

The base library also exports the same emulator and shell/scrollback APIs
as `terminal:`. Normal commands remain under `terminal:`.
