# Repository rules

- Before every commit, run `tools/scheme-format -i` on all `*.e` files.
- Temporary scripts (file surgery, probes, one-off drivers) are
  written in Scheme, not Python or other languages. Committed test
  fixtures may shell out where a fixture genuinely needs another
  runtime, but reach for Scheme first.
- Every library but the command layer `edit` is imported with its own
  prefix -- seams and apps alike (`store:`, `terminal:`, `git:`,
  `sys:`) -- and that is how M-x sees them; only `edit`'s names are
  bare, and apps import it as `(except (edit) init!)`.  Modules are
  named in the singular (`style`, `file`, `mode`, `string`, `actor`,
  `doc`), and exported names never repeat the module's stem:
  `style:set!`, `log:add!`, `git:branches`, `terminal:send!` -- never
  `styles:set-style!` or `git:git-branches`.  Rename in the export list
  (`(rename (internal external))`) if the definition keeps a longer
  name; a command that was the bare stem gets a verb (`terminal:open!!`,
  `eval:run!`, `describe:show!`).
- `manual/` is the user's manual (nothing else goes there); development notes live in `dev/`: the
  design (DESIGN2.md), the tracker and tech-debt ledger (V2_TASKS.md),
  the dead-code ledger (DEADCODE.md), the debugging-lessons ledger
  (DEBUGGING_LESSONS.md), and the terminal test notes.
