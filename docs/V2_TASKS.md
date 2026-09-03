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
