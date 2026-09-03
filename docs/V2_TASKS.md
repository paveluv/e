# v2 task tracker

The living checklist for the docs/DESIGN2.md migration. Updated as
work lands; the design doc holds the reasoning, this file holds the
state.

## Stage 1 -- buffer state

- [x] `text.e`: the pure span/edit/rebase algebra (tests/text.ss)
- [x] `state.e`: transactions, marks, subscriptions, attributed undo,
      reset/rename (tests/state.ss)
- [x] `kernel.e` born: persistent cells; excluded from reload
- [x] core wired as the store's first privileged client: mirrored
      creation, line edits, splices, resets, deletion, renames;
      foreign edits synced back per frame (tests/wiring.ss)
- [x] the human's point published as the ui actor's 'point mark
- [x] flip mastery: the store is the master text; the core's lines
      field is an adopted cache of the store's immutable vector,
      never mutated in place (the eq? proof is in tests/wiring.ss)
- [x] rebase window points, viewport tops, and buffer spots through
      foreign deltas; clamping remains only as the safety net
- [ ] publish the v0.1 region (mark..point) as a state span

## Stage 2 -- actor identity

- [x] every state operation carries its actor
- [x] attributed edit history: `state:history` (revision, actor,
      positions), cleared by resets
- [x] the audit stream: foreign operations logged under the `state`
      component -- `(log-view 'state)` is the record
- [ ] in-UI blame: show an edit's actor at point
- [x] core undo refuses to time-travel over foreign edits (snapshots
      carry the store revision; history-shift! checks state:history)
- [x] the store's undo history is bounded like the delta log
- [ ] coalesced (optional) auditing of ui edits

## Stage 3 -- kernel scheduling

- [x] registries move from core to kernel.e (core keeps facade
      aliases until its call sites migrate to kernel: prefixes)
- [x] module lifecycle (load/reload/retract) moves to kernel.e;
      core keeps load-module!/reload-module! aliases and hangs its
      after-reload work (config, buffer modes, repaint) on the
      kernel's hook
- [x] mailboxes and the event queue: kernel mailboxes; a dedicated
      reader thread owns terminal input over a private dup'd port and
      posts parsed data events; read-key-event is the main-thread
      mailbox pump (side effects -- mouse, paste, host reports --
      apply at consumption); run-on-main! and wake-main! exist, and
      foreign edits paint without a keypress
- [x] the ask/reply interaction protocol: actors.e (registration,
      ask!/answer!/cancel!, tickets, pending); the head shows waiting
      questions as an echo indicator and answers via C-c a -- nobody's
      keyboard is stolen.  The head's own modal prompts (find-file,
      M-x) stay direct: they are the head asking its own user.
- [ ] retire the display-port and between-keystrokes workarounds
      (run-on-main!/wake-main! now exist; the terminal reader still
      repaints via its display port -- migrate it; the claude module
      follows only after v2 is finished, when it is ported from its
      branch)
- [ ] subscription delivery batches through mailboxes (the settled
      coalescing)

## Stage 4 -- capabilities

- [ ] `policy.e`: per-actor capability minting and budgets
- [ ] `sandbox.e`: claude-safe generalized to any constrained actor
- [ ] port the claude module onto a minted session -- deliberately
      last: it stays on the `claude` branch until v2 is finished

## Stage 5 -- the wire (optional, later)

- [ ] serialize the protocol; remote heads and out-of-process agents

## Core dissolution (crosses stages)

Extraction order for the remaining core.e content, each leaving a
re-export facade until its importers migrate:

- [ ] `styles.e` (faces, the style DSL)
- [ ] `keymap.e` (key syntax, contexts, dispatch)
- [ ] `log.e` (the log store; log-view stays an app)
- [ ] `tty.e` (input decoding, raw terminal)
- [ ] `paint.e` (screen model, cache, painting)
- [ ] `echo.e` (notification area, prompts)
- [ ] `head.e` (window tree, per-user state, routing)
- [ ] `edit.e` absorbs the command layer; core.e deleted

## Tech debt ledger

A priority queue (100 = fix first). Anything left expediently during
execution, or discovered and worth fixing, lands here with a
priority; items graduate into stage tasks when picked up.

| P | Debt | Notes |
|---|---|---|
| 50 | Conflict override only reaches the log, not the losing actor | `lib/core.e` `state-edit!` stale branch now logs "conflict: ui overrode ACTOR in BUFFER" under `state`, naming the newest foreign editor from `state:history` -- but the losing actor itself only sees a generic reset event. Fix: a `conflict` event through `lib/state.e` `notify!`, or the stage 3 ask/reply protocol. |
| 45 | View buffers mirror wholesale on every refresh | `lib/core.e` `view-replace!` -> `buffer-lines-set!` -> `state:reset!` runs per app refresh; a busy terminal pays O(rows) store copies for content nothing subscribes to. Fix: an opt-out flag on app buffers (skip `mirror-create!`), or make `state:reset!` diff against the current text and no-op when equal. Measure with the terminal fixture in tests/interactive.ss. |
| 35 | `splice-lines!` span math is only integration-tested | `lib/core.e` `splice-lines!` translates whole-line splices into spans with three cases (interior, end-of-buffer, whole-buffer). tests/wiring.ss covers them live; add unit checks in tests/state.ss driving the same spans and comparing against `vector-splice` results. |
| 35 | Store outage silently forks the text | `lib/core.e` `state-edit!`/`state-reset!` guard clauses fall back to `adopt-local!` when the store errors: the editor keeps working but the store copy silently diverges until the next successful reset. Fix: log the outage once under `state`, and re-reset on recovery. |
| 30 | Reloading `state.e` leaves two library instances | `reload-module!` (`lib/core.e`) re-evaluates state, but core keeps the instance it compiled against; both share the store via `(kernel)` `persistent-cell`, so behavior stays coherent while stale code lingers. Fix: have reload-module! refuse seam modules the core links against, or restart-advice message. |
| 25 | `text:apply-edit` copies the whole line vector per edit | `lib/text.e` `apply-edit` allocates a fresh vector of all lines per edit: O(lines) per keystroke, fine to ~100k lines. Eventual fix: a rope or line-tree text in `text.e` behind the same API. |
| 20 | Only the selected window's cursor is published | `lib/core.e` `publish-point!` publishes one 'point mark for `current-window`. Fix: publish per window (mark name `(point . window-index)`) and the v0.1 region (buffer mark .. point) as a span mark. |
| 15 | Store marks and subscribers are assoc lists | `lib/state.e` `buffer-marks` and `store-subscribers` scan linearly per edit/notify. Fix when profiles say so: hashtables keyed by (actor . name) and token. |
