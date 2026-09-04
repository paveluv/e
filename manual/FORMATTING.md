# Indentation and formatting

## Indentation

Modes opt into indentation with `register-indenter!`. Scheme indentation is
Emacs-like: body forms such as `define`, `lambda`, the `let` family, and `when`
indent their bodies two columns beyond the opener. A lone closing delimiter
aligns with its opener.

Application continuations offer two stops: two columns beyond the opener, or
aligned beneath the first argument when it shares the opener's line:

```scheme
(very-long-function-name param1 param2
  param3)
(very-long-function-name param1 param2
                         param3)
```

Tab cycles through the available stops, beginning with the nearest stop to the
right and wrapping around. On a blank line it inserts enough spaces to reach
the chosen stop.

`indent-region!` and `indent-buffer!` process lines from top to bottom as one
undo step. A line already on a valid stop remains there; otherwise the nearest
appropriate stop is chosen.

Per-mode automatic Tab indentation can be changed with:

```scheme
(indent-on-tab! "scheme" #f)
```

## Conservative formatting

`format-region!` and `format-buffer!` perform indentation and also:

- expand tabs outside strings according to `scheme-tab-width`;
- trim trailing whitespace;
- remove trailing blank lines and leave exactly one final newline;
- normalize structural Scheme delimiters when `scheme-format-brackets` is on.

Bindings of the `let` family, `do`, `parameterize`, and `with-syntax`, plus
clauses of `cond`, `case`, `case-lambda`, `guard`, `syntax-rules`, and
`syntax-case`, use brackets. Other structural pairs use parentheses. Region
formatting changes a pair only when the region contains the complete pair, so
it cannot unbalance the surrounding buffer. Strings and margin comments are
preserved.

Scheme buffers format before saving by default:

```scheme
(scheme-format-on-save #t)
```

Modules may register additional work with `add-pre-save-hook!` and
`add-post-save-hook!`.

## Intrusive formatting

Intrusive formatting is optional and disabled by default:

```scheme
(scheme-format-intrusive #t)
(scheme-format-width 100)
```

It collapses redundant spacing outside literals, joins continuation lines that
fit, places inline comments after two spaces, and breaks long expressions
toward the target width. Partial-region formatting remains conservative. The
formatter refuses any result that does not read back as the same sequence of
Scheme data.

## Command-line formatter

The same pure `(scheme-format)` library drives `tools/scheme-format`:

```sh
tools/scheme-format file.e
tools/scheme-format -i file.e
tools/scheme-format --intrusive --width 100 -i file.e
```

Without files it reads standard input and writes standard output. Supplying
`--width` also enables intrusive mode.

## Formatter API

Modes register indentation and formatting independently:

```scheme
(register-indenter! "mode-name" indenter)
(register-formatter! "mode-name" formatter)
```

This keeps the core generic while allowing language modules to own their
layout policy.

