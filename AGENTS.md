# Repository rules

## Commit attribution

Always include a `Co-Authored-By` trailer for the AI model contributing to a commit.
Use the exact runtime model identifier from the active session, including its version or
snapshot suffix when exposed; never substitute a generic name such as Codex or GPT, infer
the model from the configured default, or invent a version.

## Formatting

Before every commit, run `tools/scheme-format.sps -i` on all `*.sls`,
`*.ss`, `*.sps`, and `*.e` files, plus the extensionless loader `e`.

## Naming conventions

Every library but the command layer `edit` is imported with its own
prefix -- seams and apps alike (`store:`, `terminal:`, `git:`,
`sys:`) -- and that is how M-x sees them; only `edit`'s names are
bare, and apps import it as `(except (edit) init!)`.  Modules are
named in the singular (`style`, `file`, `mode`, `string`, `actor`,
`doc`), and exported names never repeat the module's stem:
`style:set!`, `log:add!`, `git:branches`, `terminal:send!` -- never
`styles:set-style!` or `git:git-branches`.  Rename in the export list
(`(rename (internal external))`) if the definition keeps a longer
name; a command that was the bare stem gets a verb (`terminal:open!!`,
`eval:run!`, `describe:show!`). Commands that interact with the user
(can block on input from the user) have double-bang suffix "!!". Commands
without a bang, or with a single bang "!" are supposed to finish without
the user's intervention.

Log sources are the qualified name of the enclosing library-level function,
using its exported spelling when renamed: `(log:add! 'file:create! datum)`.
Local helpers and callbacks belong to their containing function. Private
functions use the module prefix too, without adding exports. Use a literal
source at a direct `log:add!` call; `tools/elinter.sps` checks it statically.
Logging macros must delegate to a named function.
Keep categories and extension labels in the datum, not in the source. The
journal transport preserves the source already assigned by the producer.

## Contents and commit messages

This repository contains the editor's code, user documentation and tests.
`manual/` contains only the user's manual. Commit messages describe product
behavior, code decisions and relevant validation.
