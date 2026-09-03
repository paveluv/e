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
- [x] publish the v0.1 region (mark..point) as a state span -- marks
      may now hold (text) spans (rebased strictly, degrading to
      endpoint rebasing when an edit overlaps: a selection survives a
      race, never goes stale); the head publishes the selected
      window's region as the ui's 'region mark per frame and drops it
      on deactivation

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
- [x] retire the display-port workaround: the terminal's duplicated
      display port is gone -- the reader thread wakes the main loop
      (repaints), and reader/feed-side editor work (log entries, the
      OSC 52 kill-ring store, reader-failure messages) marshals via
      run-on-main!, which the idle main pump now executes inline
      (nested pumps -- prompts, i-search -- still defer, so foreign
      thunks never run inside a modal read); the claude module follows
      only after v2 is finished, when it is ported from its branch
- [x] delivery coalescing: wake-main! dedupes -- a burst of foreign
      edits collapses into one frame per pump instead of one repaint
      per event, with the claim taken before painting so a mid-paint
      wake queues the next frame rather than being lost (racing-burst
      check in tests/wiring.ss).  Agents' own subscriptions choose
      their delivery mailboxes at registration when agent sessions
      arrive (stage 4)

## Stage 4 -- capabilities

- [x] `sandbox.e`: claude-safe generalized to any constrained actor
      -- the read-only tier as a curated export list, granted whole
      via (environment '(sandbox)) or narrowed with (only (sandbox)
      names...).  v2 hardening over v0.1: editor readers are keyed by
      buffer NAME over the state store and return only plain data (no
      buffer records cross the boundary), and every lock-taking
      reader runs with interrupts off so an engine's fuel expiry can
      never strand the store's mutex
- [x] `policy.e`: per-actor capability minting and budgets --
      policies are data (grants, fuel, edit quota, buffer allowlist,
      result cap); mint! curries a session with the actor's identity;
      session-eval! (engine-fueled, output-captured), session-edit!/
      session-undo! (attributed through state:, quota- and
      allowlist-checked), session-ask! (escalation to the owner via
      actors:); revoke! plus revocation-by-reload (sessions
      deliberately do not ride a persistent cell -- reloading
      policy.e revokes everything outstanding); the audit trail is
      state and does persist (policy:audit-log).  Both are seam
      modules: policy:/sandbox: at M-x and in module inits
- [ ] port the claude module onto a minted session -- deliberately
      last: it stays on the `claude` branch until v2 is finished

## Stage 5 -- the wire (optional, later)

- [ ] serialize the protocol; remote heads and out-of-process agents

## Core dissolution (crosses stages)

Extraction order for the remaining core.e content, each leaving a
re-export facade until its importers migrate:

- [ ] `styles.e` (faces, the style DSL)
- [ ] `keymap.e` (key syntax, contexts, dispatch)
- [x] `log.e` (the log store; log-view stays an app) -- the records
      (persistent cell: reloads keep the log), the formatter
      registry, log!/entries/history moved; the core keeps echo
      presentation, installed as the log's presenter hook, plus
      facade aliases for every old name; sandbox reads log: directly,
      and policy's audit trail streams quietly into (log-view
      'policy)
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
| 35 | `splice-lines!` span math is only integration-tested | `lib/core.e` `splice-lines!` translates whole-line splices into spans with three cases (interior, end-of-buffer, whole-buffer). tests/wiring.ss covers them live; add unit checks in tests/state.ss driving the same spans and comparing against `vector-splice` results. |
| 30 | Reloading a core-linked seam module leaves two library instances | `reload-module!` (`lib/kernel.e`) re-evaluates `state.e`/`log.e`/`text.e`/`actors.e`, but core keeps the instances it compiled against; both share stores via `(kernel)` `persistent-cell` (state store, log records, log presenter, pending asks), so behavior stays coherent while stale code lingers -- except registries, which fork: a reloaded log.e gets an empty formatter registry while core's aliases (and every module init! going through them) keep the old one. Fix: have reload-module! refuse seam modules core links against (list them next to the core/kernel refusal), or restart-advice message. Repro: reload log -- log-view (core aliases) still styles, but sandbox's log-tail (prefixed log: import, recompiled against the new instance) falls back to unformatted text for formatter-owning components. |
| 25 | `text:apply-edit` copies the whole line vector per edit | `lib/text.e` `apply-edit` allocates a fresh vector of all lines per edit: O(lines) per keystroke, fine to ~100k lines. Eventual fix: a rope or line-tree text in `text.e` behind the same API. |
| 20 | Only the selected window's cursor and region are published | `lib/core.e` `publish-point!`/`publish-region!` publish one 'point mark and one 'region span for `current-window`. Fix: publish per window (mark names `(point . window-index)`, `(region . window-index)`), dropping a window's marks when it closes. |
| 15 | Store marks and subscribers are assoc lists | `lib/state.e` `buffer-marks` and `store-subscribers` scan linearly per edit/notify. Fix when profiles say so: hashtables keyed by (actor . name) and token. |
| 15 | eval.e still paints through a dup'd stdout port | `lib/eval.e` `evaluate!` (the `terminal (duplicate-standard-output-port)` let) streams stdout/stderr of evaluated code live while the main thread is busy inside the eval, so it cannot marshal via `run-on-main!` (the pump is not running).  The last display-port workaround.  Fix arrives with stage 4 agent sessions: agent evals run off-main and their output posts to mailboxes; a main-thread M-x eval can then simply defer its log lines. |
