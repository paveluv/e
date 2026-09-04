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
  `region-printing` (edit -- the command layer's other load-time
  effects moved into its init! when core dissolved); `reload-hooked`,
  `reload-tail-hooked` (main); `libc-character-locale`,
  `libutil-loaded?` (sys); `store-subscription`, `ui-actor-registered`,
  `seat-initialized` (head); `adopt-hooked` (modes); `repaint-hooked`
  (paint);
  `completions-mode-registered`, `completions-status-hinted` (prompt);
  `frame-hooked`, `ask-presented`, `echo-greeting-shown` (main).
  Skip these in future sweeps.

## Deleted

- 2026-09-04 `kernel:seam-modules`: the list of libraries the kernel
  imported prefixed into the M-x top level -- every library but edit is
  prefixed now, so the rule needs no list.

- 2026-09-04 exports `paint:paint-window!`, `paint-dividers!`,
  `paint-echo-area!`, `paint-visual-bell!`, `head:publish-head-marks!`:
  the frame calls them inside their own modules; nothing outside did.

- 2026-09-03 `lib/core.e` itself: its last 2,419 lines became the
  command layer in edit.e; the define-for-effect registrations became
  init! statements (`core-keys-bound`, `log-formatters-init`,
  `log-presenter-installed`, `styles-hook-installed`,
  `ui-audit-flushed-at-exit`, `conflict-status-hinted`,
  `prompt-commands-allowed`, `mouse-hooked`, `main-hooked` are no
  longer names); `buffers-initialized`/`head-seat-initialized` became
  head's `seat-initialized`.  describe.e's `register-descriptions!`,
  `published-descriptions`, `entry-datum->doc-entry` (the registry half)
  moved to docs.e.

- 2026-09-03 `mouse-on?`, `set-mouse!` (core): a flag written and never
  read, and its setter; `mouse!` and startup call `tty:mouse-reporting!`
  directly.  The `("MOUSE" ,void)` default binding and the "MOUSE" key
  name (keymap): the pump never produced that key -- a report becomes
  "MOUSE-HANDLED" or is consumed.  `run-posted-thunk!`,
  `state-frame-sync!`, `settle-echo!`, the five-handler
  `set-pump-handlers!` (core/head): the seat services its own side
  effects; `run-key-action!`'s `'kill` flag and the `insert-chain` reset
  in the dispatcher: commands ask `head:last-command` instead.

- 2026-09-03 `prompt-window-commands` (core): the hard-coded list of
  commands a prompt may run, now the `prompt:allow!` registry;
  `current-message` (core, one caller, which reads `echo:text`);
  `split-pasted-lines`/`read-paste`/`pending-paste`, `string-insert`/
  `string-delete`, `vector-fill-range!`, `call-with-interrupt`,
  `set-isig!`, `&interrupted` moved out of core rather than dying
  (tty:paste-lines, head:read-paste, strings:insert/delete,
  styles:fill-range!, head:call-with-interrupt).

- 2026-09-03 `paint:set-redraw-hook!`, `paint-redraw-hooked` (core),
  `paint:visual-bell-active?`/`set-visual-bell-active!`: the frame and
  the bell live in paint.e, so nothing outside needs to ask for a frame
  through a hook or flip the bell's flag.  The per-prompt creation and
  deletion of the *completions* buffer (its store twin with it) went
  too: the buffer is a registered view now.

- 2026-09-03 `read-file` (describe): a private copy of core's; both are
  `files:read` now.  Core's own disk helpers (`read-file`, `disk-stamp`,
  `string-lines`, `ends-in-newline?`, `merge-trailing-newline`, the path
  functions, `complete-file-name`, `data-directory`, `canonical-path`)
  moved to files.e rather than dying; the inner `write!` of save-file!
  lost its permission juggling to `files:write!`.

- 2026-09-03 `paint:set-mode-hook!`, `head:set-client-hooks!`,
  `paint-mode-hooked`, `head-client-hooked`, `completions-mode` (core):
  with the mode registry in modes.e, paint imports it directly and
  modes installs head's adopt hook itself, so the core stopped
  brokering both; the completions mode is registered once instead of
  minted per prompt (`modes:make` was exported for that and is not
  any more).

- 2026-09-03 the string helpers' copies (core, scheme-format, https):
  `string-tail`, `string-prefix?`, `string-suffix?`, `string-join`,
  `string-search`, `split-lines`, `common-prefix` in core and the
  private `string-prefix?`/`string-tail` of scheme-format.e and
  https.e -- strings.e is their one home (`strings:lines` and
  `strings:common-prefix` joined it).  The condition formatter had
  three copies too -- core's `error-text`, head's and policy's
  `condition-text` -- now `kernel:condition-text`.  diff.e's
  six-argument `common-prefix` is a different function and stays.

- 2026-09-03 native scrolling (core): `native-scroll!`, `shift-screen-cache!`,
  and the window record's `shown-top` field -- a bytes-on-the-wire
  optimization coupling paint's cache, the window record, and terminal
  scroll regions (harmony rule).  If measurements ever want it back it
  belongs inside paint.e, keyed by paint's own shadow of viewport tops.

- 2026-09-03 `vector-splice` (core): its last user, view-append!, moved to
  head.e, which splices through the pure `text:splice`; the stale
  "created lazily" registry comment and the store client's forget hook
  (forgetting is head's own now) went in the same move.

- 2026-09-03 `known-apps`, `known-app-of`, `ensure-app-registry!`, the
  lazy hook registries (core): the second app bookkeeping list existed
  to carry presentation across a module's re-registration -- those
  facts are store properties now and persist by themselves; the
  registries were lazy only because the kernel once came after the
  core.  The app record's sticky-lines/scrollbar/wrap/cursor-style/
  manages-viewport fields and the seat record's wrap field went with
  them: one wrap fact per buffer, shared by every head.

- 2026-09-03 app target windows (core): `set-app-target!`,
  `target-window`, `target-buffer`, `show-buffer-in-target!`,
  `display-app!`, `display-app-here!`, `create-ephemeral-target-window!`,
  the app record's target fields, and the status-line `>` marker --
  apps act on the selected window now (harmony rule: eight functions of
  routing state for a per-app 'where you came from').
- 2026-09-03 input capture (core): `set-app-capture!`,
  `app-capture-escaped?`, `escape-app-capture!`, `clear-capture-bypass!`,
  the three bypass globals, and the capture flag -- a live app's handler
  consumes what it wants, and the escape is keymap data:
  `keymap:set-context-escape!` names a context's escape prefix, and
  dispatch resolves the rest of such a sequence globally.

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
  state". Removed with the store.e work.

## Suspects awaiting a look

(none)
