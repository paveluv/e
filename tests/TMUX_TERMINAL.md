# tmux terminal compatibility audit

This audit covers only tmux's terminal boundary. It excludes tmux commands,
key tables, prefixes, windows, panes, status UI, and control mode.

## Reference

- Repository: `https://github.com/tmux/tmux.git`
- Commit: `b28244e2fe4493a71d54d0a17d87aa9106d8e665`
- Date: 2026-08-31
- Local, ignored checkout: `.reference/tmux`

The application-facing contract is defined primarily by `input.c`: controls
which tmux accepts from a program in a pane and the replies it sends back. The
host-facing contract is defined by `tty.c`, `tty-term.c`, `tty-features.c`, and
the built-in `tmux-256color` terminfo entry. DCS passthrough is recorded
separately from terminal emulation; accepting a tmux passthrough envelope does
not imply implementing its opaque payload.

## Procedure

For each row below:

1. Reduce the tmux implementation to literal input and expected terminal state.
2. Add the equivalent assertion to `terminal.ss`; use `terminal-process.ss`
   only when the PTY boundary matters.
3. Confirm that the test fails when the behavior is absent or incorrect.
4. Implement the behavior and run both owned suites.
5. Compare the reduced case in tmux and e at identical geometry.
6. Mark the row complete only after the owned test and comparison both pass.

Parser recovery, cancellation, fragmented input, parameter defaults, clipping,
and wide-cell boundary behavior are part of each applicable row rather than a
separate optional pass.

## Application-facing inventory

| Area | tmux behavior | e status | Next action |
|---|---|---|---|
| C0 and UTF-8 | BEL, BS, HT, LF/VT/FF, CR, SI/SO; Unicode cells | covered | differential review |
| ESC | IND, NEL, HTS, RI, RIS, save/restore, keypad, DECALN, G0/G1 | covered | compare edge cases |
| Cursor CSI | CUU/CUD/CUF/CUB, CNL/CPL, CUP/HVP, HPA/VPA, save/restore | covered | compare defaults and clipping |
| Editing CSI | ICH/DCH, IL/DL, ECH, ED/EL, SU/SD, CBT, TBC, REP | covered | compare wide-cell boundaries |
| Modes | insert, origin, autowrap, cursor visibility, mouse, focus, bracketed paste | partial | enumerate exact tmux mode set |
| SGR | base attributes, 16/256/RGB colors | partial | add underline variants/color, overline, strike parity |
| Reports | DA1/DA2/XDA, DSR, DECRQM, window operations | partial | match tmux replies intentionally |
| Extended keys | modifyOtherKeys enable/disable and encoded input | partial | differential key matrix |
| OSC 0/1/2 | title operations | covered | compare termination and empty title |
| OSC 4/10/11/104/110/111 | palette and default colors | covered | compare malformed/query cases |
| OSC 8 | hyperlinks | covered | generic ranges and terminal cell metadata have owned tests |
| OSC 9;4 | progress indication | missing | decide buffer-level presentation and implement state |
| OSC 12/112 | cursor color set/query/reset | missing | implement cursor-color state |
| OSC 52 | clipboard set/query | covered | writes import; reads are denied by policy |
| OSC 133 | shell integration marks | missing | preserve semantic marks without tmux UI behavior |
| DCS DECRQSS | effective-state queries | partial | compare every query tmux answers |
| DCS tmux passthrough | unwrap `tmux;` payload when enabled | not applicable yet | retain as future multiplexer-boundary work |
| Sixel | optional compiled tmux image support | deferred | audit separately only if e advertises graphics |

## Host-facing inventory

These are sequences tmux may send to e when tmux itself runs in an e terminal.

| Feature | Principal protocol | e status |
|---|---|---|
| 256 and RGB colors | SGR 38/48 | covered |
| Bracketed paste | DECSET 2004 | covered |
| Mouse | DECSET 1000/1002/1003 and 1006 | covered |
| Focus | DECSET 1004 | covered |
| Titles | OSC 0 | covered |
| Cursor style | DECSCUSR | covered |
| Cursor color | OSC 12/112 | missing |
| Clipboard | OSC 52 | covered, configurable |
| Hyperlinks | OSC 8 | covered |
| Strikethrough and overline | SGR 9/53 | partial |
| Underline styles and color | SGR 4 subparameters, 58/59 | missing |
| Synchronized updates | DECSET 2026 | missing |
| Extended keys | modifyOtherKeys | partial |
| Left/right margins | DECLRMM and DECSLRM | covered |
| Rectangle fill | DECFRA | missing |
| Progress indication | OSC 9;4 | missing |
| Terminal probes | DA/DA2/XDA, DECRQM, OSC colors, size reports | partial |

## Milestones

1. Freeze the exact input tables, OSC dispatch, DCS dispatch, modes, and
   replies from the pinned source into this inventory.
2. Close parser, cursor, editing, scrolling, and mode gaps.
3. Close style, color, hyperlink, and synchronized-update gaps.
4. Close input encoding, mouse, focus, paste, and report gaps.
5. Add clipboard support with an explicit trust/configuration policy.
6. Run tmux inside e, then modern applications inside that tmux, including
   resize, alternate screen, mouse, paste, detach/attach, and nested tmux.
7. Re-audit against a newer tmux by diffing the four reference source files
   and updating the pinned commit above.

Completion means every non-deferred row has an owned regression and an
identical reduced result in tmux and e. Optional graphics support does not
silently expand the advertised terminal profile.
