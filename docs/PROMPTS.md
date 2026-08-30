# Interactive prompts

Prompts share one editing and presentation engine. File selection, buffer
selection, `M-x`, describe, and command-specific questions therefore use the
same movement, history, completion, wrapping, and styling behavior.

## Editing

Prompt input supports the familiar bindings:

| Key | Action |
|---|---|
| `C-a`, `C-e`, Home, End | Move to an input or visual-line boundary |
| `C-b`, `C-f`, Left, Right | Move by one character |
| Up, Down | Move through visual lines, then history |
| `C-k`, `C-y` | Use the shared kill ring |
| Tab | Complete |
| `C-g`, Escape | Cancel |
| Return | Accept |

Prompt input wider than the screen wraps onto continuation rows marked with
`\`. Continuations align beneath the prompt text. The echo area grows by
shrinking windows to their configured minimum; after eight prompt rows, the
prompt scrolls while keeping its cursor visible.

## Multiline input

Prompts that enable multiline input accept Meta+Return to insert a newline.
Lines are automatically indented and the complete input is reindented after
each edit, so a structural change can update following lines immediately.
Pasted multiline input uses the same indentation pass.

At line boundaries, the first `C-a` or `C-e` moves within the current visual
line; a repeated command moves to the beginning or end of the complete input.
The repetition is command-based rather than inferred from the cursor position.

## Completion

Tab extends input to the longest common prefix. If nothing can be added, a
second Tab opens the full-width `*completions*` popup directly above the echo
area. Repeated Tab cycles through pages when necessary. The popup disappears
when the prompt finishes and never changes the persistent window split tree.

Completion candidates use a shared semantic style:

- an incomplete or unknown value is italic;
- an exact ordinary match is upright;
- a distinguished editor-defined value uses the editor face.

File prompts apply the same mechanism component by component: the existing
path prefix is upright and the nonexistent remainder is italic.

## Suggestions and inspection

Prompts may display a grey ghost tail after the cursor. `M-x` derives its tail
from structured describe data, so module-published procedures receive the same
parameter hints as built-in entries.

`M-.` may inspect the value at the prompt cursor. In `M-x` it opens the live
describe page for the Scheme symbol under or immediately before point.

## Prompt API

`prompt!` accepts completion, initial input, and history. Presentation can be
customized with `prompt-styler`, `completion-styler`, `completion-highlight`,
`prompt-ghost`, `prompt-inspector`, `prompt-multiline`, `prompt-edge-motion`,
and `prompt-reindent`.

Use `show-prompt-message!` when a non-`prompt!` interaction should retain the
same styled label and wrapped layout. The transient `C-x w` widen-window mode
uses this path.

