# Debugging lessons

A ledger of investigations that were expensive once so they need not be
expensive twice. Each entry records the symptom as first reported, the
theories that failed, the step that actually cracked it, the root cause,
and what generalizes. Add new entries at the top.

## The reader thread detached the app under a frame in progress (2026-09-04)

**Symptom.** Exiting the shell inside a terminal buffer (C-d) logged
`app: App *terminal* refresh failed: Exception in view-invalidate!: not
an app or view buffer` -- once per exit, with the transcript otherwise
intact.

**Root cause.** The PTY reader thread's end-of-input handler did the
buffer's afterlife itself: wake the main thread to paint, then turn the
rendered cells into a plain transcript, then `head:detach-app!`.  The
wake made the main thread paint at once, and a refresh of the still-dirty
terminal was under way when the reader removed the app from the
registry; the refresh reached `paint:view-invalidate!`, which requires
an app buffer, and failed.  The same handler also replaced the buffer's
lines from the reader thread -- a seat mutation off the main thread that
happened not to be the one that blew up.

**Fix.** The handler flips its own `alive` flag and posts the rest
through `head:run-on-main!`: render the final screen, then materialize
the transcript, then detach -- in that order, between frames.  Only the
platform wait for the session leader stays on the reader thread.  The
feed path's other seat touches -- the OSC title rename and the
cursor-shape fact that RIS and DECSCUSR set -- marshal the same way
now, each tolerating a terminal that died before its turn came.  The
interactive suite checks that no "refresh failed" message follows the
shell's exit.

**What generalizes.**

- Reader and feed threads may flip their own flags; every touch of seat
  state (buffers, apps, names, facts) goes through `run-on-main!`.  The
  compiler will not catch a violation here either.
- "It only logs an error" is still a data race.  The visible failure was
  the harmless half; the buffer-lines rewrite from the wrong thread was
  the dangerous one, and both had the same fix.
- A wake is a request to paint *now*; anything the waker does afterwards
  runs concurrently with that paint.

## The escape the keymap declared and the app still swallowed (2026-09-04)

**Symptom.** Inside a live terminal buffer, `C-]` did nothing visible:
the status line still promised "C-] to escape", but the key went to the
shell as a literal 0x1d and no global command could be run from inside
the capture.

**Root cause.** When capture became keymap data (the `'terminal`
context names `C-]` as its escape and binds `C-] C-]` and `C-] C-y`),
the explicit "decline C-]" clause left the terminal's event handler --
the escape was now the dispatcher's business.  But the dispatcher kept
giving the app handler first refusal of every key, and the handler
encodes any `C-<char>` for the child, `C-]` included.  The keymap
declared the escape; nothing consulted it before the app did.  The
manual papered over the gap with "the handler must decline the escape
key itself", which no handler did.  No suite drove `C-]`: the
interactive suite left its nested shell with `exit`.

**Fix.** `main:handle-key!` asks the buffer's mode context first: a key
the context binds, starts a binding with, or names as its escape goes to
the keymaps, and the app's handler sees only the keys its context leaves
unbound.  The redundant plain `C-y` binding in the terminal context went
with it (it would have hijacked the child's `C-y`).  The escaped state's
presentation had gone in the same refactor -- the `▶ escaped` status hint
the manual still described, and the editor cursor while escaped -- and
came back as seat state (`head:escaped-buffer`) the dispatcher sets
around the escaped sequence.  The interactive suite now runs `C-] M-x`
and `C-] C-]` inside the nested shell and watches the status hint.

**What generalizes.**

- When a behavior turns into declarative data, find every consumer of
  the old imperative path and make the dispatcher read the data; a
  declaration nobody consults is a comment with syntax.
- A manual rule of the form "X must remember to do Y" for the design to
  work marks a seam in the wrong place.  Move Y to the one place that
  already knows about it.
- The suite drove the feature next to the bug (open a terminal, type
  into it) but never the bug's own key.  Every escape hatch needs a
  test that uses it.

## A library that exports init! collides with every importer's (2026-09-03)

**Symptom.** The moment core.e's body joined edit.e, every app failed to
compile with "multiple definitions for init! in body": each imports the
command layer bare, and edit.e -- a module, so it exports `init!` for
the kernel's lifecycle -- now handed them a second `init!` next to
their own.  Core never had one.

**Root cause.** R6RS forbids a library body from defining a name it
imports.  A bare import of a module that follows the init! protocol is
therefore only safe from libraries that do not follow it themselves --
which no app is.  The importers say `(except (edit) init!)` now.

**What generalizes.** Two conventions met: "the command API is
imported bare" and "every module exports init!".  Each was fine alone;
the merge that made one library obey both surfaced the conflict in
every importer at once, which at least made it impossible to miss.  A
second, quieter surprise from the same merge: core created the seat's
*scratch* buffer and first window at library load, so headless suites
that never ran the main loop still had a window; moving that setup
into `main:run` broke them with "#f is not of type window" far from
the cause.  The seat's initial state belongs to the seat (head.e), where
every importer gets it.

## The M-x environment is whatever the loader imports (2026-09-03)

**Symptom.** After the loop moved from core.e to main.e and the loader
switched from `(import (core)) (main)` to `(import (prefix (main)
main:)) (main:run)`, every suite compiled and the terminal app worked,
yet M-x completion answered `split-w [No match]` and the wiring test's
typed expression failed on `current-buffer`.  A first round chased the
key dispatcher (a headless probe of the keymap registry came back empty
-- but so did the same probe at the last good commit: the probe, not
the registry, was wrong).

**Root cause.** M-x, `describe`, and every expression a test types at
the prompt evaluate in the interaction environment, whose bare names are
exactly what the loader's `import` put there.  Dropping `(core)` from
that import removed every command name from M-x, silently: completion
simply had nothing to offer, and the compiler had no say because the
names are looked up at run time.

**What generalizes.** The loader's import line is part of the editor's
user-facing API -- the M-x namespace.  Moving code between libraries
changes nothing for compiled callers but changes what a user can type;
the interactive suite (a real PTY, real M-x) is the only test that sees
it.  Two smaller lessons from the same slice: a script that rewrites a
file with `'replace` creates a new file and drops the executable bit
(the harness's `exec ./e` then fails as "no such file" one layer up),
and a headless probe that imports a library is not the running editor
-- compare it against a known-good commit before believing it.

## The Delete key was M-DELETE all along (2026-09-03)

**Symptom.** None reported — that is the lesson.  The first unit test
ever written against the input decoder (tests/tty.ss, created when the
parser moved from core.e to tty.e) expected `ESC [ 3 ~` to decode as
`DELETE` and got `M-DELETE`.

**Root cause.** The CSI modifier heuristic accepted a lone first
parameter of 2/3/4 as a legacy modifier spelling.  That reading is only
valid for letter finals; for a `~` final the first parameter *is* the
keycode, so Delete (3) decoded as meta, Insert (2) as shift.  Present on
main since the terminal Unicode/keys commit; unnoticed because the
affected keys are also reachable other ways (BACKSPACE deletes, paging
works via C-v) and an unbound `M-DELETE` fails silently as "Key is
unbound".

**What generalizes.** Code that is only integration-tested is only
tested on the paths the integration happens to walk: no interactive test
pressed Delete, so the decoder's most common special key was wrong for
months.  Extracting a pure seam (bytes in, data out) made the first
direct test trivial — and it failed immediately.  When a dissolution
slice makes something newly unit-testable, write the obvious table of
cases even if the code "has been working": that table is where the
latent bugs are.

## The console ports share one lock (2026-08-31)

**Symptom.** Running `vi` inside an e terminal for the first time: press
Enter, the cursor moves, nothing shows. A second Enter displays vim, but
without its intro page. Every stall ends the moment a key is pressed.

**Failed theories, and why they were plausible.** Synchronized output
(mode 2026) had just been implemented and gates frame presentation — but
a raw byte capture showed vim never sends it. A byte-level trigger in
vim's startup burst — but bisecting the burst gave different verdicts on
identical prefixes across runs, which is the signature of a race, not of
a byte. A GC rendezvous blocked by the main thread's `read()` — but a
forced `(collect)` on the reader thread completed instantly. A
frame-behind cache bug — but the render cache and dirty flags checked
out. One earlier "reproduction" of the shifted-frame variant turned out
to be an artifact of grepping blank rows out of the screen dumps:
verify the dump before verifying the theory.

**What cracked it.** Three instruments, in order:

1. Timestamped markers appended to a side file — never to the screen,
   which was the suspect channel — from both the reader's feed loop and
   the refresh path, with phase markers from the test driver. They
   proved the emulator grid held vim's complete frame at t=5027 while
   e's host output first carried it at t=8102, right after the next
   keystroke: the data was fine, the presentation was stuck.
2. `/proc/<pid>/task/*/syscall` during the stall: the main thread was in
   `read(0, ...)` on the keyboard, the reader thread in `futex` — a
   lock, not I/O, and a lock the keyboard read would release.
3. A ten-line standalone probe: in a script-mode Chez process, a second
   thread's `display` to `(current-output-port)` blocked for exactly as
   long as the main thread sat in `get-char` on `(current-input-port)`
   (2772 ms measured, ending precisely at the keystroke). The first
   version of this probe ran `scheme -q` interactively and showed no
   blocking at all — the interactive REPL uses the expression editor's
   ports, not the script-mode console ports. An oracle that does not
   match the production environment refutes nothing.

**Root cause.** Chez's script-mode console input and output ports share
one lock, and the main thread holds it across its blocking keyboard
read. The PTY reader thread logged an unsupported-sequence diagnostic
(vim's `CSI >4;2m`), and painting the echo area writes through the
console output port — so the reader stalled until the next keystroke,
while holding the terminal state lock, with vim's fully parsed frame
unpresentable behind it. A second-order effect hid the intro page: once
diagnostics could paint, the growing echo area resized the windows, and
vim dismisses its intro on any resize — solved separately by handling
modifyOtherKeys instead of logging it.

**Fix.** `call-with-display-output` in the terminal module routes every
reader-thread diagnostic (`log!`, `set-message!`, clipboard notices)
through the terminal's duplicated display port — a separate port with a
separate lock over the same tty — exactly as `display-redraw!` already
did for frames. The interactive suite pins it: an unsupported sequence
followed by output and then silence must appear within two seconds.

**Lessons.**

- A stall that ends exactly when the user provides input means someone
  is waiting on a resource the main thread only releases between reads.
  Look for locks held across blocking reads before looking at the data.
- A bisection whose verdict changes on identical input is diagnosing a
  race; stop bisecting content and start instrumenting time.
- `/proc/<pid>/task/*/syscall` answers "what is every thread blocked
  on" in one read, and would have shortcut hours of theorizing.
- Instrumentation must not travel through the channel under suspicion;
  a side file with timestamps is cheap and honest.
- Never write to the console output port from a thread other than the
  main input loop; use the duplicated display port. The compiler will
  not catch a violation — only this ledger and the regression test do.
