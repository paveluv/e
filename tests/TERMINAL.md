# Terminal testing

e claims the behavior advertised by `xterm-256color`, plus the extensions
listed in [TERMINAL.md](../docs/TERMINAL.md). It does not claim every private feature
implemented by xterm.

## Test layers

The automated suite is wholly owned by e and has no runtime or build-time
dependency on `vttest`, xterm, GNOME Terminal, terminfo, or network resources.

- `tests/terminal.ss` feeds literal control sequences to the headless emulator
  and checks cells, styles, modes, replies, reflow, and encoded input.
- `tests/terminal-process.ss` checks the PTY boundary: controlling-terminal
  setup, initial and changed geometry, process groups, and bounded shutdown.
- Manual integration checks cover properties that require a real renderer or
  human input, such as redraw atomicity, keyboard layouts, mouse capture,
  selection, dragging, and nested full-screen applications.

`vttest` is an optional development oracle for the manual layer. It is used to
find discrepancies, never as the assertion engine. Every fixed discrepancy
must have a minimal literal-sequence or PTY regression in the owned suite.

## Automated gate

Run:

```sh
scheme --script tests/terminal.ss
scheme --script tests/terminal-process.ss
```

Tests should be deterministic, should not inspect the host terminfo database,
and should name the behavior rather than the external program that exposed it.
Prefer the smallest sequence that reproduces a failure. A parser failure test
should also feed the sequence in fragments when the bug could depend on PTY
read boundaries.

## Optional vttest pass

Pass the terminal app's actual drawable geometry explicitly. `vttest` otherwise
defaults to a 24-by-80 canvas even when e's status bar, echo area, and scrollbar
leave a smaller PTY grid. In the terminal shell, run:

```sh
set -- $(stty size)
vttest -u "${1}x${2}.${2}"
```

Repeat with `-8` and `-s` inserted after `-u`. Re-run `stty size` after any
split or resize; a stale geometry makes cursor tests scroll and produces
plausible-looking but invalid one-row or one-column offsets.

Use at least 24 drawable rows for menu 2. Its final save/restore screen writes
three explanatory lines beginning at row 21; on a shorter terminal vttest
itself scrolls the expected 5-by-4 `A` rectangle upward, despite continuing to
state that all four rows should be visible.

Run the same case in a known-good terminal and in an e terminal. Keep both at
the same geometry and locale. The `-u` pass exercises UTF-8 without vttest
switching the terminal to ISO-8859-1; `-8` repeats parsing with 8-bit C1
controls; `-s` makes scrolling transitions visible.

Cover these menus in order:

1. Cursor movements, screen features, character sets, and VT102 insertion and
   deletion.
2. Terminal reports and reset behavior.
3. VT220 through VT520, ISO-6429, color, and xterm-specific menus, limited to
   behavior represented by `xterm-256color` or documented in TERMINAL.md.
4. Keyboard tests for cursor, editing, keypad, function, modified function,
   and Meta keys.
5. Mouse tests for press, release, motion, wheel, modifiers, focus reporting,
   and restoration of ordinary e selection after mouse reporting is disabled.
6. Repeat the full-screen and scrolling cases with `-8`, then the scrolling
   cases with `-s`.

Do not treat VT52, double-height/double-width lines, downloadable character
sets, sixel/ReGIS graphics, Tektronix mode, or other unadvertised xterm
extensions as failures.

## Integration matrix

After the menu pass, exercise real programs because their timing and mixed
protocol use expose different bugs:

- shell editing, completion, resize, job control, and process exit;
- `top` entry, live updates, resize, and exit;
- Emacs startup, Info navigation, visual bell, resize, and exit;
- nested e, including its terminal, mouse capture, scrollback, and resize;
- a true-color application, alternate screen, bracketed paste, and mouse mode;
- scrollback navigation followed by typing, which must return to the live
  cursor without corrupting the child screen.

Repeat resize-sensitive cases in one window, mirrored windows, and after a
split changes the available rows or columns.

## Recording a discrepancy

Record:

- vttest version, menu path, and test name;
- e commit, geometry, locale, and 7-bit or 8-bit mode;
- expected reference result and actual e result;
- whether the failure is display, reply, input encoding, timing, or resize;
- a captured byte sequence when available.

Reduce the capture before changing the emulator. Add the owned regression,
verify that it fails for the intended reason, implement the fix, and run both
automated suites. Re-run only the relevant vttest section before the next full
manual pass.
