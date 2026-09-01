# Debugging lessons

A ledger of investigations that were expensive once so they need not be
expensive twice. Each entry records the symptom as first reported, the
theories that failed, the step that actually cracked it, the root cause,
and what generalizes. Add new entries at the top.

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
