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
  `styles-hook-installed`, `ui-audit-flushed-at-exit`
  (core, the v2 wiring). Skip these in future sweeps.

## Deleted

- 2026-09-01 `read-key`, `peek-key` (core): exported raw stdin readers
  with no callers anywhere; retired with the scheduling substrate,
  which they could not have survived (the reader thread owns stdin).

- 2026-09-01 `drag-status` (core): defined and assigned, never read
  -- its own comment admitted "retained for clearing older mouse
  state". Removed with the state.e work.

## Suspects awaiting a look

(none)
