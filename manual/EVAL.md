# Evaluation

e is a live Scheme environment. Both evaluation commands run code in the
editor's interaction environment: the same top level used by `config.e` and
the module loader, with Chez Scheme, every loaded module's exports under its
prefix (`edit:`, `store:`, `head:`, `keymap:`, ...) and the literals of
`(head literal)` bare, `(buffer "name")` and `(window n)`, in scope. Definitions
persist for the rest of the session and are immediately available to later
evaluations.

## Commands

### `eval:prompt!` — interactive M-x evaluation

`M-x` is bound to `eval:prompt!`. It prompts for a Scheme expression, under
the label `λ`, and evaluates it:

```scheme
λ (head:buffer-name (head:current-buffer))
λ (define answer 42)
λ answer
```

The prompt begins with an editable `(`. It may be deleted when evaluating a
bare symbol. An input is valid when it reads as Scheme data once its open
string and its open forms are closed; Enter closes them, a quote for the
string and then each form's own bracket, and the closed expression is what
runs and enters the history and log. Completion never produces an invalid
input: settling closes only what the structure allows.

While the prompt is active:

- `TAB` fuzzily completes the symbol at the cursor from the interaction environment,
  including symbols in nested expressions. The first press normalizes the input;
  the second shows matches and cycles any alternative normalizations.
- `Shift-TAB` completes only symbols published by e and its modules.
- e-specific completion candidates use the editor highlight.
- Unknown or partial symbols are italic, standard Scheme symbols are plain,
  and e-specific symbols use the editor highlight.
- A grey, italic ghost shows the documented parameters still expected by the
  innermost open call. Signatures come live from structured describe entries,
  including entries registered by modules; source parameters and procedure
  arity are fallbacks. While the input cannot read even with its string and
  forms closed, an extra `)` or a bracket of the wrong kind say, the ghost
  shows the complaint instead, bracketed: ` [unexpected )]`.
- Up and Down browse evaluation history, newest first.
- `M-Enter` inserts a real newline and indents the new line according to
  Scheme structure. Ordinary Enter accepts and runs the input.
- The first `C-a` moves to the current logical line's first non-space
  character; a consecutive second `C-a` moves to the beginning of the whole
  input. The first `C-e` moves to the current line's end; a consecutive second
  `C-e` moves to the end of the whole input. The second press is recognized as
  a repeated command even when the first press did not move the cursor.
- `M-.` describes the symbol at or immediately before the prompt cursor
  without closing the prompt.
- `C-g` cancels the prompt. During evaluation it interrupts running code.

Symbol completion matches contiguous segments beginning at the start of a
symbol or immediately after `-` or `:`. Segments may appear in a different
order: `splitright` and `rightsplit` both find `window:split-right!`. Each
character occurrence can be used only once, so `xx` requires two `x` characters.
Matching is case-sensitive; other punctuation, including `_`, does not create
a boundary. Longer intact segments, fewer reorderings, and matches nearer the
beginning rank first.

Typed `-` and `:` stay inside literal segments, just like letters, and a
segment may lead with one, anchored to the same separator in the name: `:sp`
finds `head:split-window` but not `head:window-split`, `.sls` finds the
`.sls` files and not `sls-mode`, and `s-b` abbreviates `window:split-below!`
as `s` + `-b`, while `b-s` does not, since no `-s` follows a `b` there. A
separator alone is no segment, so `ker:` still cannot abbreviate `kernel:`;
the one exception is a name's first character, so `*` finds `*scratch*`.
You can omit separators when typing prefixes: `spwir` finds
`window:split-right!` as `sp` + `wi` + `r`. Reordering still works with
punctuation when the literal pieces exist: `rightwindow:` can match
`window:split-right!` as `right` + `window:`.

Tab chooses a longest extension that the original query can match and that
still matches every candidate. This preserves exactly the same match set,
including its boundary constraints. For example, `splitwindow` and
`windowsplit` normalize to `window:split-` while all four split commands
remain. Adding `r` would lose the other three, so it is not inserted yet.
Typing `r` and pressing Tab then produces `window:split-right!`, including
the `!`.
The same rule applies to separators: `ker:` cannot abbreviate the literal
prefix `kernel:`. Tab cannot add a colon after `ker` merely because all
matches contain one.

When complete names are longest extensions, Tab cycles those names in their
original ranking. Otherwise, it offers at most one longest spelling in each
candidate's character order, removing duplicates. If no such spelling exists,
it keeps one valid extension rather than cycling arbitrary permutations.
Typing or moving the cursor starts a new completion cycle.

The result can be a complete symbol even when longer candidates remain.
Enter evaluates the input as usual; to
refine it instead, keep typing and press Tab again.

A Tab with exactly one match inserts that symbol, closes the list, and
settles the form around it while every enclosing operator has a fixed arity:
a procedure of no arguments closes its form with the matching `)`, `]` or
`}`, one expecting more arguments leaves the cursor one space on, at the next
argument, and a completed last argument closes the form. A closed form is then
settled as an argument of its parent, so `(head:window-index (head:curr` Tab
yields `(head:window-index (head:current-window))`. Optional and rest parameters,
syntax, unbound names, quoted or quasiquoted forms, and text after the
cursor all leave the cursor at the end of the symbol without appending a
space or changing the arguments. Strings, comments and character literals
are left alone. A fuzzy query need not itself be valid Scheme: `2foo` can
find `foo-2`.

Once the list is open, typing and deleting refresh it immediately. Tab normalizes
the edited symbol; another Tab resumes cycling. When there is just one
normalization, repeated Tab pages through the list. PageUp/PageDown and the
mouse wheel also page, including when Tab is cycling alternatives. Clicking a
candidate fills that symbol and closes the list. Leaving the symbol or cancelling M-x also hides
the pop-up. No match leaves your input intact.

The completion list shows one candidate per row: the name, with the
character occurrences the matcher used underlined, then in grey what it takes
and its `edoc` summary when it has one: a procedure its arguments, a
parameter `[value]`, a value its type in angle brackets, a keyword the parts
of its form. A procedure without an `edoc` shows the parameters of its
source, or its arity. A long hint wraps onto further rows, indented under
itself. The hint is display-only: clicking anywhere in a candidate's rows
inserts just the symbol. The list's status line shows its buffer's name,
`<completions>`, then counts the matches and names what they are, `12
matches of file` or `4 matches of symbol`, adding `; page 2 of 3` when they
take several pages.

At an argument position of a documented procedure, Tab completes by the
argument's type instead of by symbol. `(head:show-buffer! ` offers every live
buffer as the expression that denotes it, `(buffer "edit.sls")`, with the
buffer's file, mode and state as its hint; then the documented procedures
and parameters that produce a buffer, `(head:current-buffer)` and
`(head:fresh-buffer! name)` say, which insert their opening and settle to their
first argument; then the top-level variables holding one, so a buffer you
bound with `define` at M-x is offered by name. The token matches a
candidate's own spelling the way it matches a symbol, by parts starting at
any punctuation, so `scr` finds `(buffer "*scratch*")` and `cur` finds
`(head:current-buffer)`, while the formals shown in a producer's label take no
part. Tab extends the token to the longest text every current candidate
still matches: `bu` becomes `(buffer` when everything offered is a form,
and stays bare while a variable such as `myb` is among the matches. The
operator position of a nested form takes the enclosing argument's type,
so `(head:show-buffer! (cu` completes to `(head:current-buffer)` rather than to every
symbol; a form under a quote, or under an undocumented operator, completes
symbols as before, except an element of a quoted list at an argument typed
`(list-of T)`, which completes as a `T`: `(extension:load! "x" "y" '("../sch`
lists directories. A `one-of` type offers its literals, a boolean `#t` and
`#f`, and inside a string literal the type's string values complete the
literal: `(edit:visit-file! "man` lists paths under `manual/`, `(buffer "`
lists buffer names. A string value completes as a session: with several
matches Tab extends the literal to their longest common prefix, as a shell
does; a sole match is inserted bare, and the literal closes only at a dead
end, where completing from the value would offer nothing but the value itself. So `(edit:visit-file!
"man` Tab gives `"manual/` with the manual's entries listed at once, and
`"manual/EVAL.m` Tab gives `"manual/EVAL.md")`, closed and settled; a
directory argument closes at a directory without subdirectories. How paths
are offered is the `file:completion` parameter: `fuzzy`, the default, lists
every entry of the partial path's directory for the matcher's segments, so
`"lib/apps/evsl` finds `eval.sls`; `prefix` lists only the entries that
extend its last component, as a shell does; `deep` lists the entries below
the directory as well, a few thousand at most, so `"lib/evsl` finds
`lib/apps/eval.sls`. Switch at M-x with `(file:completion 'prefix)`. An actor argument lists the registered actors as
`(head "desk")` or `(agent "claude")`, the identities' own spelling. The language's own types, `string` or `integer`, offer
no producers, and an argument whose type offers nothing the token matches
falls back to symbol completion. `S-Tab` always completes symbols.

Bracketed multiline paste keeps its line breaks and runs the same Scheme
indenter over the resulting expression. This makes copied definitions and
multi-form snippets line up as they would in a Scheme buffer. Other prompts
remain single-line and continue to fold pasted line breaks into spaces.

After every edit in M-x—including typing, deletion, completion, yank,
newline, and paste—the complete input is reindented. If a structural edit
changes the indentation of later lines, those lines are redrawn immediately.

The complete expression remains visible while it is running, with the cursor
at its end changed to a blinking underline. Its Scheme and e-specific symbol
highlighting, logical-line indentation, and continuation layout remain exactly
as they appeared in the editable prompt. Explicit input newlines occupy real
echo-area rows; long individual lines still soft-wrap at the terminal edge.

After completion, the transient echo record retains the command's explicit
line breaks, matching its multiline representation in `<log>`.

### `eval:run!` — evaluate buffer or region text

`C-x C-e` is bound to `eval:run!`.

```scheme
(eval:run!)
```

`eval:run!` evaluates the selected region while the mark is active, else the
whole current buffer. Another buffer or region is evaluated under a scope
form:

```scheme
(head:with-buffer (buffer "scratch.scm") (eval:run!))
(edit:with-region (region (head:current-buffer) '(10 . 0) '(18 . 0)) (eval:run!))
```

Every datum in the text is evaluated. The values of the last datum become
the command result; definitions and effects from earlier datums remain in
place.

## Results and the copy buffer

Evaluation results are printed with Scheme's write representation. Multiple
values are separated by `, `. A result is shown in the echo area and stored
as an `eval` log record:

```text
eval: (+ 20 22) => 42
```

By default, a non-void result is also copied, ready to
insert with `C-y`. The echo result gains a grey, italic ghost tail:

```text
eval: (+ 20 22) => 42 [copied]
```

The ghost is presentation only and is not part of the result or log record.
The copied text is exactly the displayed result representation. Void results,
zero-value results, errors, and interruptions do not replace the copy buffer.

Disable automatic copying in `config.e`:

```scheme
(eval:copy-result #f)
```

The default is `#t`.

## Standard output and standard error

Both commands capture Scheme output and process-level output inherited by
child programs. Complete lines are emitted as they arrive:

```scheme
(display "starting\n")
(system "sleep 1")
(display "finished\n")
(system "printf 'child output\n'")
```

`starting` appears immediately, `finished` about one second later, and the
child's line after it. Output is separated into structured log components:

- the current output port and process stdout become `stdout` records;
- the current error port and process stderr become `stderr` records.

Each completed line receives its timestamp when it arrives, so `<log>`
preserves the timing of long-running commands. A final unterminated line is
emitted when evaluation closes the stream. stdout, stderr, and the evaluation
result remain separate records.

Captured output finishes draining before the command returns, including after
an evaluation error or `C-g`. Explicitly closing the captured output or error
port does not prevent the editor from restoring its output normally.

Examples:

```scheme
(display "hello\n")
(display "warning\n" (current-error-port))
(system "echo out; echo error >&2")
```

## Errors, interruption, and edits

Reader and evaluation failures are reported as `error: ...` eval results.
They are logged and shown in the echo area but are not copied to the kill
buffer. `C-g` interrupts running evaluation and records `interrupted`.

An evaluation is wrapped in `edit:call-as-one-edit!`. Any editor buffer changes
made by the evaluated code form one undo step per affected buffer, labeled by
the M-x expression or the corresponding `eval:run!` invocation. This grouping
does not roll back Scheme definitions or external effects when later code
fails; it controls editor undo history only.

## Logging and history

Every completed evaluation creates an `eval` record whose datum is the query
and formatted result. M-x history is derived from these records. Captured
output creates independent `stdout` and `stderr` records and does not enter
the M-x expression history.

Open the live log view through the buffer list or with:

```scheme
(log-view:show!)
(log-view:buffer! 'eval)
(log-view:buffer! 'stdout)
(log-view:buffer! 'stderr)
```

The result is posted after both output streams close, so it remains the final
entry for that evaluation.

## Describe integration

Both commands publish structured describe entries. Use any of:

```scheme
(describe:this eval:run!)
(describe:show! 'eval:prompt!)
```

or press `C-h f` and complete the command name. The live describe page shows
the commands' current key bindings, including user rebinding from `config.e`.

## Configuration summary

```scheme
;; Copy non-void eval:run!/M-x results for C-y (default: #t).
(eval:copy-result #t)

;; Optional key rebinding examples.
(keymap:bind! "C-c e" eval:run!)
(keymap:bind! "M-X" eval:prompt!)
```

`eval:copy-result` is a parameter and may also be changed temporarily with
`parameterize` around programmatic evaluation.

## Reusing evaluation in an extension

An evaluator with its own Scheme environment can reuse e's interruption,
output capture, undo grouping and reporting without using M-x's environment:

```scheme
(eval:report!
  (eval:call-with-evaluation! "worksheet evaluation"
    (lambda () (eval form worksheet-environment)))
  'worksheet)
```

The thunk returns ordinary Scheme values. `eval:status` is `ok`, `error` or
`interrupted`; `eval:values` returns the list of values and `eval:condition`
the original condition on failure. Execution alone neither copies nor
reports the result. `eval:report!` applies `eval:copy-result` and preserves a
message spoken by a void command. Its second argument says where the record
goes: an extension passes its own log component, `'worksheet`, and the
result appears under it in `<log>`; M-x passes the actual input as a string,
which records an `eval` exchange with its history.

Run on the head's main thread. Nested calls share the outer capture and
interruption scope and one undo group. A continuation escape cleans up and
escapes normally; grouping is not a rollback of edits or arbitrary Scheme
effects. Parse inside the thunk so read errors use the same reporting path.
For result insertion, edit inside the thunk and return the original values
with `apply values`, keeping the computation and insertion in one undo step.
