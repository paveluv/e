# Dead code ledger

Findings from dead-code sweeps: suspects, verdicts, and deletions.
Sweep method: single-occurrence scan (a name appearing only at its
definition across lib/, tests/, config.template.e, and the loader)
plus call-site spot checks of exports. New findings go at the top
with a date; deletions note the commit.

## Known idioms that look dead but are not

- **define-for-effect**: `(define name (side-effecting-expression))`
  runs initialization at library load; the name is intentionally
  never referenced. Confirmed instances: `buffer-printing`,
  `core-keys-bound`, `log-formatters-init`, `reload-hooked`,
  `sigwinch-registered` (core), `region-printing` (edit),
  `libc-character-locale`, `libutil-loaded?` (sys),
  `state-subscription`, `foreign-sync-hooked`, `reload-tail-hooked`,
  `ui-actor-registered`, `log-presenter-installed`,
  `styles-hook-installed`, `ui-audit-flushed-at-exit`,
  `echo-greeting-shown`, `head-seat-initialized`, `buffers-initialized`,
  `pump-handlers-installed`
  (core, the v2 wiring). Skip these in future sweeps.

## Deleted

- 2026-09-03 the completions pop-up window (core): a window outside the
  layout tree, special-cased in the layout, set-layout-root!, the
  echo cap, the status line, and four mouse sites.  Completions now
  borrow the prompt's target window; the tree is the only source of
  windows (harmony rule).  Also fixed on the way: dismissed
  *completions* buffers leaked their store twins, one per prompt.

- 2026-09-03 `widen-window!!`, `divider-in-direction`, `resize-highlight`
  (core): the transient `C-x w` keyboard resize mode and its divider
  highlight -- a product decision under the harmony rule (a modal
  mini-loop plus a paint special case, spanning three seams);
  removed for later redesign, keyboard resizing to return as plain
  commands.
- 2026-09-03 `scrollbar-set-top!`, `scrollbar-move!`, `scrollbar-thumb-at?`,
  `scrollbar-thumb-position`, `scrollbar-drag-to!`, `drag-scrollbar`
  (core): scrollbar thumb dragging -- the scrollbar is a display-only
  position indicator now (same rule: paint, mouse, and viewport were
  coupled only to drag it).

- 2026-09-01 `read-key`, `peek-key` (core): exported raw stdin readers
  with no callers anywhere; retired with the scheduling substrate,
  which they could not have survived (the reader thread owns stdin).

- 2026-09-01 `drag-status` (core): defined and assigned, never read
  -- its own comment admitted "retained for clearing older mouse
  state". Removed with the state.e work.

## Suspects awaiting a look

(none)
