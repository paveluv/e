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
- [ ] flip mastery: core reads text through state snapshots and drops
      its own line cache (today: core is master, state mirrors)
- [ ] rebase window points through foreign deltas instead of clamping
- [ ] publish the v0.1 region (mark..point) as a state span

## Stage 2 -- actor identity

- [x] every state operation carries its actor
- [x] attributed edit history: `state:history` (revision, actor,
      positions), cleared by resets
- [x] the audit stream: foreign operations logged under the `state`
      component -- `(log-view 'state)` is the record
- [ ] in-UI blame: show an edit's actor at point
- [ ] coalesced (optional) auditing of ui edits

## Stage 3 -- kernel scheduling

- [ ] registries move from core to kernel.e
- [ ] module lifecycle (load/reload/retract) moves to kernel.e
- [ ] mailboxes and the event queue; the keyboard becomes one
      producer among several
- [ ] prompts become the ask/reply interaction protocol
- [ ] retire the display-port and between-keystrokes workarounds
- [ ] subscription delivery batches through mailboxes (the settled
      coalescing)

## Stage 4 -- capabilities

- [ ] `policy.e`: per-actor capability minting and budgets
- [ ] `sandbox.e`: claude-safe generalized to any constrained actor
- [ ] port the claude module (lives on the `claude` branch) onto a
      minted session

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
| 70 | Mirror conflicts silently drop the foreign edit | When the core's mirror transaction goes stale (an agent edited the same line mid-command), the fallback reset makes core content win; the agent's change vanishes with only an audit log line. Should refuse or notify the losing actor. |
| 65 | Foreign edits wait for a keypress to appear | A worker-thread `state:edit!` is adopted by the pre-redraw hook, which runs only when the main loop wakes. Needs a way to nudge the blocked keyboard read (self-pipe / kernel mailboxes, stage 3). |
| 60 | Core undo time-travels over foreign edits | `restore-snapshot!` resets the store to the pre-undo core lines, silently clobbering any agent edit that landed since the snapshot. Undo should refuse or rebase, like `state:undo!` does. |
| 45 | View buffers mirror wholesale on every refresh | A busy terminal resets its state twin each frame -- O(rows) copies for content no actor subscribes to yet. Consider opting app buffers out of mirroring, or dirty-diffing resets. |
| 40 | `state.e` undo history is unbounded | The delta log is bounded (256); `buffer-undo` grows forever in long sessions. |
| 35 | `splice-lines!` span math is only integration-tested | The end-of-buffer special cases (append, whole-buffer) mirror correctly per tests/wiring.ss, but deserve direct unit tests against `text:extract` round-trips. |
| 30 | Reloading `state.e` leaves two library instances | Core keeps calling the old instance; both share the persistent store cell so behavior stays coherent, but stale code lingers until restart. |
| 25 | `text:apply-edit` copies the whole line vector per edit | O(lines) per keystroke; fine to ~100k lines, a rope or gap structure eventually. |
| 20 | Only the selected window's cursor is published | Other windows' points (same human) and the v0.1 region are invisible to agents. |
| 15 | Store marks and subscribers are assoc lists | Linear scans per edit; fine at current scale. |
