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
- [x] in-UI blame: `state:blame` (the attributed delta log with spans
      rebased to the current text) plus the new `blame.e` extension
      module -- another actor's fresh edit is tinted in that actor's
      color (a stable hash into six muted faces, fading after
      `blame-tint-seconds`), and `(blame-at-point!)` names who
      recently wrote the text at point.  Zero core growth: overlays
      ride the store subscription (marshaled to the main thread) and
      paint through `add-highlighter!`
- [x] core undo refuses to time-travel over foreign edits (snapshots
      carry the store revision; history-shift! checks state:history)
- [x] the store's undo history is bounded like the delta log
- [x] coalesced auditing of ui edits: consecutive ui edits to a
      buffer batch into one quiet audit entry ("ui: N edits in BUF
      (revisions A-B)"), flushed before a foreign actor's operation
      on that buffer so (log-view 'state) reads in true order, on a
      3-second idle, and at shutdown -- stage 2 complete

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

- [x] `styles.e` (faces, the style DSL) -- compile-style/style-escape,
      the override registry, the built-in faces, and style-code moved;
      the core keeps facade aliases and installs the repaint trigger
      as the styles-changed hook (painted rows are cached by content,
      not face definitions)
- [x] `keymap.e` (key syntax, contexts, resolution) -- the key-spec
      parser, the one binding registry (user beats default, newest
      wins, owners retract with their module), lookup/prefix
      resolution, and the command-keys reverse index moved; the core
      keeps dispatch (dispatch-sequence!, run-key-action!,
      mode-key-context, describe-key's presentation) plus facade
      aliases
- [x] `log.e` (the log store; log-view stays an app) -- the records
      (persistent cell: reloads keep the log), the formatter
      registry, log!/entries/history moved; the core keeps echo
      presentation, installed as the log's presenter hook, plus
      facade aliases for every old name; sandbox reads log: directly,
      and policy's audit trail streams quietly into (log-view
      'policy)
- [x] `tty.e` (input decoding) -- the whole byte->event decoder
      moved (key naming, CSI/SS3 sequences, SGR mouse, bracketed
      paste, host reports), parameterized over its input port and so
      unit-testable for the first time (tests/tty.ss, 32 checks --
      which immediately caught the latent M-DELETE decode bug, see
      tests/DEBUGGING_LESSONS.md); the core keeps the reader thread,
      the mailbox pump, and mouse-event application.  Raw-mode
      termios control already lives in (sys).  `strings.e` was born
      alongside: the shared pure string helpers (tail, prefix?,
      suffix?, join) now have one home below the seams, and styles.e/
      keymap.e dropped their local copies
- [~] `paint.e` begun: the row painter moved -- display-editor-line
      (styled runs, marks, links, selection, wrap/truncation edges),
      emit-runs, ansi/goto/fit, soft-wrap break computation, and
      hyperlink detection, all pure given their inputs and headlessly
      tested for the first time (tests/paint.ss); strings.e gained the
      KMP `search`.  Remaining for the head.e era: frame composition
      -- layout, scrolling, the screen cache, paint-window!,
      paint-echo-area!, redraw! -- which reads window records
      directly and moves when they do
- [~] `echo.e` begun: the notification area's model moved -- the
      live message/ghost/styler, the transient-log queue, prompt
      indent bookkeeping, and the wrap-geometry math (spans, log-row
      folding), width always passed in.  The core reads and writes it
      through identifier-syntax facades, so its ~54 (set! message
      ...) sites landed there unchanged.  Remaining for the head.e
      era: presentation (present-echo!, paint-echo-area!, geometry
      driver) and the prompts, which read keys and own the modal loop
- [~] `head.e` begun: the window tree moved -- the window and
      layout-split records, the seat state (windows, layout root,
      selected window, divider output) behind identifier-syntax
      facades, and the pure tiling geometry (leaves, replace, parent,
      minima, weighted splits, layout-node!).  Remaining: routing
      (apps, capture, set-layout-root!), wrap policy, per-buffer state
      swapping (define-state), the main loop -- plus the presentation
      halves of paint.e and echo.e that come loose with them
- [ ] `edit.e` absorbs the command layer; core.e deleted

## Tech debt ledger

A priority queue (100 = fix first). Anything left expediently during
execution, or discovered and worth fixing, lands here with a
priority; items graduate into stage tasks when picked up.

| P | Debt | Notes |
|---|---|---|
| 25 | `text:apply-edit` copies the whole line vector per edit | `lib/text.e` `apply-edit` allocates a fresh vector of all lines per edit: O(lines) per keystroke, fine to ~100k lines. Eventual fix: a rope or line-tree text in `text.e` behind the same API. |
| 15 | Store marks and subscribers are assoc lists | `lib/state.e` `buffer-marks` and `store-subscribers` scan linearly per edit/notify. Fix when profiles say so: hashtables keyed by (actor . name) and token. |
| 15 | The delta/undo log bounds entries, not bytes | `lib/state.e` `delta-log-limit` (256) trims by count, but each delta pins its removed lines for invert/rebase: 256 large kills retain megabytes while 256 typed characters retain almost nothing. Fix: a secondary byte budget -- track retained removed-content size and trim the tail past N cells (keep the count cap too); adjust the `basis-too-old` comment in `edit!` and the undo-depth expectation in tests/state.ss. Repro/measure: kill a 5000-line region 256 times, watch resident size. |
| 15 | eval.e still paints through a dup'd stdout port | `lib/eval.e` `evaluate!` (the `terminal (duplicate-standard-output-port)` let) streams stdout/stderr of evaluated code live while the main thread is busy inside the eval, so it cannot marshal via `run-on-main!` (the pump is not running).  The last display-port workaround.  Fix arrives with stage 4 agent sessions: agent evals run off-main and their output posts to mailboxes; a main-thread M-x eval can then simply defer its log lines. |
