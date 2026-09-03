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

## Window management during a prompt

A prompt does not lock the window layout. The window-management commands
-- focusing (Meta-arrows, `C-x o`), splitting (`C-x 2`, `C-x 3`),
closing (`C-x 0`, `C-x 1`), and killing the focused buffer (`C-x k`,
refused with a note when it has unsaved changes) -- keep working while a
prompt runs, under whatever keys they are bound to: events resolve
through the live global keymap, so rebound or newly bound chords work in
every prompt as well. Only self-inserting characters always stay with
the input. The mouse works too: clicks focus windows and the status-bar
controls split and close as usual. None of this cancels the prompt, and
the window focused when the prompt is accepted is the command's target
-- the file opens there, the evaluation runs against that buffer. Chords
that resolve to any other command are consumed without effect so their
tail keys cannot leak into the input.

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
second Tab shows `*completions*` in the prompt's target window -- the window
whose buffer the prompt is about. Repeated Tab cycles through pages when the
list is taller than the window. When the prompt finishes the window gets its
buffer back, point and viewport intact; the split tree never changes.

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
same styled label and wrapped layout.

