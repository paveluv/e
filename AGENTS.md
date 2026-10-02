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

Every module API, including the command layer `edit`, is imported with its own
prefix -- seams and apps alike (`edit:`, `store:`, `terminal:`, `git:`,
`sys:`) -- and that is how M-x sees them. Portable values use ordinary Scheme
data, with quoting in expressions: `'(model 7)` and `'(app describe)`, or
`'((model 7) (model 8))` in collections. Completing types do not change their
spelling. Scalar refinements stay strings, symbols or numbers; completing a
type never publishes a constructor. Windows and compositions are model
references too. Do not introduce constructor aliases.
Modules are named in the singular (`style`, `file`, `mode`, `string`, `actor`,
`doc`), and exported names never repeat the module's stem:
`style:set!`, `log:add!`, `git:branches`, `terminal:send!` -- never
`styles:set-style!` or `git:git-branches`.  Rename in the export list
(`(rename (internal external))`) if the definition keeps a longer
name; a command that was the bare stem gets a verb (`terminal:open!`,
`eval:run!`, `describe:show!`). Effectful procedures use a single `!`;
queries have no bang and predicates end in `?`. A procedure that waits for
input declares `(prompts)` in its edoc; it does not get a separate suffix.
The effect checker validates these conventions and documented exceptions.

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
