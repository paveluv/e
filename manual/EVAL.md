# Evaluation

e is a live Scheme environment. Both evaluation commands run code in the
editor's interaction environment: the same top level used by `config.e` and
the module loader, with Chez Scheme, the command layer `(edit)`, every loaded
module's exports, and the seam modules under their prefixes (`state:`,
`head:`, `keymap:`, ...) in scope. Definitions persist for the rest of the session and are
immediately available to later evaluations.

## Commands

### `eval!!` — interactive M-x evaluation

`M-x` is bound to `eval!!`. It prompts for a Scheme expression and evaluates
it:

```scheme
M-x (buffer-name (current-buffer))
M-x (define answer 42)
M-x answer
```

The prompt begins with an editable `(`. It may be deleted when evaluating a
bare symbol. Missing closing parentheses are added when the input can be
completed unambiguously; the normalized, closed expression is what enters
the history and log.

While the prompt is active:

- `TAB` completes symbols from the interaction environment.
- `Shift-TAB` completes only symbols published by e and its modules.
- e-specific completion candidates use the editor highlight.
- Unknown or partial symbols are italic, standard Scheme symbols are plain,
  and e-specific symbols use the editor highlight.
- A grey ghost shows the documented parameters still expected by the
  innermost open call. Signatures come live from structured describe entries,
  including entries registered by modules; source parameters and procedure
  arity are fallbacks.
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
line breaks, matching its multiline representation in `*log*`.

### `eval!` — evaluate buffer or region text

`C-x C-e` is bound to `eval!`.

```scheme
(eval!)
(eval! where)
```

With no argument, `eval!` evaluates the whole current buffer. It does not use
the active selection implicitly. An explicit `where` accepts the same target
forms as the editing helpers:

- a buffer;
- a buffer name;
- a `region`;
- a predicate selecting buffers;
- a list containing any of these.

For multiple targets, their region texts are joined with newlines and
evaluated in order. Every datum in the resulting text is evaluated. The
values of the last datum become the command result; definitions and effects
from earlier datums remain in place.

Examples:

```scheme
(eval!)
(eval! (buffer "scratch.scm"))
(eval! "helpers.scm")
(eval! (region (current-buffer) '(10 . 0) '(18 . 0)))
(eval! (lambda (b) (string=? (buffer-mode-name b) "scheme")))
```

## Results and the kill buffer

Evaluation results are printed with Scheme's write representation. Multiple
values are separated by `, `. A result is shown in the echo area and stored
as an `eval` log record:

```text
eval: (+ 20 22) => 42
```

By default, a non-void result is also copied to the kill buffer, ready to
insert with `C-y`. The echo result gains a grey ghost tail:

```text
eval: (+ 20 22) => 42 [stored in kill ring]
```

The ghost is presentation only and is not part of the result or log record.
The copied text is exactly the displayed result representation. Void results,
zero-value results, errors, and interruptions do not replace the kill buffer.

Disable automatic copying in `config.e`:

```scheme
(eval-copy-result #f)
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

Each completed line receives its timestamp when it arrives, so `*log*`
preserves the timing of long-running commands. A final unterminated line is
emitted when evaluation closes the stream. stdout, stderr, and the evaluation
result remain separate records.

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

An evaluation is wrapped in `call-as-one-edit!`. Any editor buffer changes
made by the evaluated code form one undo step per affected buffer, labeled by
the M-x expression or the corresponding `eval!` invocation. This grouping
does not roll back Scheme definitions or external effects when later code
fails; it controls editor undo history only.

## Logging and history

Every completed evaluation creates an `eval` record whose datum is the query
and formatted result. M-x history is derived from these records. Captured
output creates independent `stdout` and `stderr` records and does not enter
the M-x expression history.

Open the live log view through the buffer list or with:

```scheme
(show-log!)
(log-view 'eval)
(log-view 'stdout)
(log-view 'stderr)
```

The result is posted after both output streams close, so it remains the final
entry for that evaluation.

## Describe integration

Both commands publish structured describe entries. Use any of:

```scheme
(describe eval!)
(describe eval!!)
```

or press `C-h f` and complete the command name. The live describe page shows
the commands' current key bindings, including user rebinding from `config.e`.

## Configuration summary

```scheme
;; Copy non-void eval!/M-x results to the C-y kill buffer (default: #t).
(eval-copy-result #t)

;; Optional key rebinding examples.
(bind-key! "C-c e" eval!)
(bind-key! "M-X" eval!!)
```

`eval-copy-result` is a parameter and may also be changed temporarily with
`parameterize` around programmatic evaluation.
