# Interactive prompts

Prompts share one editing and presentation engine. Create mode, the original
find-file command, `M-x`, describe, and command-specific text questions use
the same movement, history, completion, wrapping, and styling behavior.
The default [Files](FILES.md) and [Buffers](BUFFERS.md#the-buffers-app) pickers
are table apps with their own navigation and filtering controls.

## Editing

Prompt input supports the familiar bindings:

| Key | Action |
|---|---|
| `C-a`, `C-e`, Home, End | Move to an input or visual-line boundary |
| `C-b`, `C-f`, Left, Right | Move by one character |
| Up, Down | Browse history in window prompts; move through visual lines, then history in the echo area |
| `C-k`, `C-y` | Use this head's kill ring |
| Tab | Complete |
| `C-g`, Escape | Cancel |
| Return | Accept |

Single-key questions, such as yes/no choices, briefly flash the echo area
when a key is not one of the allowed answers. The question returns after
about 50 ms without another keypress; repeated invalid keys extend the flash.
The question stays active until you answer or cancel it.

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
controls split and close as usual. An echo-area prompt keeps running, and
the window focused when it is accepted is the command's target: an
evaluation runs against that buffer. Chords
that resolve to any other command are consumed without effect so their
tail keys cannot leak into the input. A [prompt in the
window](#prompts-in-the-window) differs in one respect: it belongs to its
window, so focusing another window cancels it.

Resizing your terminal refreshes the layout without another keypress, even
while a prompt is open. Your input stays intact. Each attached head uses its
own terminal dimensions.

## Multiline input

Prompts that enable multiline input accept Meta+Return to insert a newline.
Lines are automatically indented and the complete input is reindented after
each edit, so a structural change can update following lines immediately.
Pasted multiline input uses the same indentation pass.

At line boundaries, the first `C-a` or `C-e` moves within the current visual
line; a repeated command moves to the beginning or end of the complete input.
The repetition is command-based rather than inferred from the cursor position.

## Prompts in the window

`M-x (find-file!!)` reads its input in the current window instead of the
echo area. Each invocation creates a temporary local `<find-file>` view.
The input sits at the bottom of the window, with the same editing keys,
styles, suggestions and text
cursor as an echo-area prompt. Clicking the input moves its insertion point.

`C-x b` and `C-x C-b` use the [filterable buffers app](BUFFERS.md#the-buffers-app).
The [files app's M-c mode](FILES.md#create-mode) uses the same input editor
below a live directory table, with sortable columns and paged matches.

Long input wraps above the bottom row. Tab lists candidates above the input;
repeated Tab pages through them. Every input change, including history
recall, clears the old candidates. Up and Down browse history. Clicking a
candidate fills the input with its complete value; Enter accepts it. The
status line shows key hints or a page count as space permits. In small panes,
the input clips around the cursor to leave a candidate row visible. A pane
with only one text row asks you to enlarge it to see matches.

Acceptance shows the result in the invoking window. `C-g` or Escape restores
its previous buffer and position. Focusing another window, closing the
prompt's window, or killing its temporary buffer also ends the interaction.
An explicit buffer choice in a side `<buffers>` panel takes effect and ends
the prompt. Any split copies of the temporary view are restored too; the
temporary buffer disappears when the interaction ends.

For find-file, changing focus keeps the unfinished path and cursor for the
next invocation in that window. Explicit cancellation discards this draft.
Drafts last only in the current head process; reconnecting restores the
last editing screen, without a pending prompt. File-open errors keep the
path editable; see [File buffers](BUFFERS.md#file-buffers). The echo area
keeps showing messages while a window prompt is active.

Any prompt can use the window: `(prompt:in-window #t)` in `config.e` makes
every `prompt:read!` take the current window. Find-file always uses the window.
A nested prompt uses the echo area. Its own completion list takes the pop-up
window temporarily, then returns to the outer prompt and its input.

## Completion

Ordinary prompts use prefix completion. Tab extends input to the longest
common prefix. When an ambiguous prefix cannot be extended, Tab shows
`<completions>` in the pop-up window (window 0), which appears above the echo
area, or, for a window prompt, candidates above its input. Plain candidates
fill columns; labelled ones, as M-x's, take a row each, a long label wrapping
under its hint. Repeated Tab cycles
through pages when the list is taller than the available space. Clicking a
candidate fills the input without opening it or moving focus away from the
prompt. Hover makes the candidate label bold with a dotted underline without
changing the input; column padding remains clickable without being underlined.
Finishing or dismissing the list hides the pop-up again; the other windows
keep their buffers, points and viewports, and completion does not change the
split layout.

[M-x](EVAL.md) uses fuzzy symbol completion: the first Tab normalizes the token
while preserving its matches, and the second opens the list. Further typing
keeps that list up to date. Further Tab presses cycle distinct normalizations,
or page when there is only one. PageUp/PageDown and the mouse wheel also page.

Completion candidates use a shared semantic style:

- an incomplete or unknown value is italic;
- an exact ordinary match is upright;
- a distinguished editor-defined value uses the editor face.

File prompts apply the same mechanism component by component: the existing
path prefix is upright and the nonexistent remainder is italic. File labels
show literal basenames, including spaces and punctuation; directories end
in `/`. Labels too wide for the pane end in `…`; clicking them still fills
the complete path. Dotfiles appear when the final component starts with `.`.

## Suggestions and inspection

Prompts may display a grey, italic ghost tail after the input. All ghosts use
the shared `ghost` face, including inline notices and echo-area result tails.
`M-x` derives its tail from structured describe data, so module-published
procedures receive the same parameter hints as built-in entries.

`M-.` may inspect the value at the prompt cursor. In `M-x` it opens the live
describe page for the Scheme symbol under or immediately before point.

## Questions from other actors

An agent or another actor can leave a question for you. The echo area shows
the oldest pending question when no other message or prompt occupies it.
The indicator updates while idle as questions arrive or are withdrawn,
advancing to the next question or clearing when none remain. It also refits
immediately when you resize the terminal. Other messages and anything you are
typing into a prompt stay intact.
Press `C-c a` (`answer!!`) to answer; Tab offers any supplied choices.
Cancelling the prompt leaves the question pending so you can return to it.
If it was withdrawn while you were typing, the editor says so when you submit.
Once a named head has attached, questions can also arrive while it is absent.
They wait in the running daemon for that name to return. Disconnecting the
asking agent cancels its unanswered questions; disconnecting your head does
not stop the agent or discard its questions.

The protocol behind the question -- `actor:ask!`, tickets, the actor
directory and agent sessions -- is documented in
[Base, heads and agents](MULTIHEAD.md#questions-between-actors).

## Prompt API

`prompt:read!` accepts a label followed by optional completion, initial input,
history box, alternate completion and input normalization procedures.
Presentation can be customized with `paint:prompt-styler`, `paint:completion-styler`,
`prompt:completion-label`, `prompt:completion-highlight`,
`prompt:ghost`, `prompt:inspector`, `prompt:multiline`, `prompt:edge-motion`,
and `prompt:reindent`. The echo area is a bordered box of at most `paint:echo-box-width` columns (100 by default), centered on the screen; a narrower screen is the whole box. Messages and prompts wrap inside its borders, row by row, with their text at the left border. `paint:echo-box-border` sets the one-cell glyph drawn on both sides (`┊` by default).

`prompt:completion-label` maps a full candidate to its displayed label;
the default preserves the value. A normal completion procedure receives the
input and returns a list of full replacement strings, using prefix completion.
For normalization and live filtering, pass `(prompt:make-completer lookup)`
as the completion or alternate-completion argument. `lookup` receives the input
and cursor index and returns four values: the start and exclusive end of the
token, its proposed expansions, and a list of candidate replacement strings.
Expansions are a nonempty list or a zero-argument procedure returning that list.
The procedure runs only when Tab starts a new normalization; live filtering
and subsequent cycling do not call it. All expansions must preserve the same
match set. Their order and the candidate order stay fixed while Tab cycles; editing starts a
new cycle. Duplicate expansions should be removed by the source.
Return `#f` as the start when there is no completable token. String candidates
are displayed as supplied and styled with `prompt:completion-highlight`.
For richer presentation, return `(prompt:make-candidate value label styles)`
in place of a string: `value` is the replacement string, `label` is the displayed
text, and `styles` is a vector with one face per label character. Labels clip
at whole glyphs; generated ellipses and padding stay plain. Clicking any part
of a candidate inserts only its value. Candidates replace only the token
interval; `prompt:completion-label` applies to ordinary completion procedures.
The caller owns matching and expansion; the prompt owns
normalization and cycling, live refresh within the same token, pagination,
and placing the cursor after a replacement. Lookup must have no command
effects, because editing may call it repeatedly. An optional second argument, `(settle text position)`, receives the input after a Tab with exactly one match has inserted that match and closed the list, and returns the `(text . position)` to continue with; M-x uses it to close forms and step to the next argument.

`prompt:validate` is `#f` or a procedure
called on normalized input when Enter is pressed. It returns `#f` to accept
or a short explanation to keep editing. Returning `(prompt:transient "message")`
instead shows only an inline `[message]` ghost, cleared after two seconds or
on editing, in both window and echo-area prompts. `prompt:draft` is `#f` or a box
containing `#f` or `(input . cursor)`; the prompt starts from that draft and
updates it as input changes. The caller decides when to retain it. Validation
and draft ownership are local to each invocation; nested reads do not inherit
those two options.

An in-window prompt can replace the ordinary candidate grid with live content:
parameterize `prompt:content` to `(prompt:make-content minimum-height render handle)`.
The renderer receives `(input window available-height page)` on every refresh
and returns two values: a list of `prompt:line` values and the page count.
It must fit within the supplied height, wrapping the requested zero-based
page into its current count. The minimum height reserves room above wrapped
input; very small panes can still provide less. Keep filesystem work outside
this renderer; publish background results and wake the head to refresh.

`(prompt:line text styles choices [hover-face])` describes one displayed row. Styles is
a vector indexed by character; choices is a list of `(start end value)`
intervals. Clicking a string value fills the input. An action value is called
and may return new input, or `#f` to keep editing unchanged, as with a sort
heading. Hover defaults to the standard `hover` face; files rows use
`candidate-hover` to include the subtle row tint. `handle` is `#f` or a
key handler returning true for consumed events; editing and prompt/window
commands otherwise retain their usual meaning. The content's table pages
with repeated Tab, PageUp/PageDown, Shift-Tab or wheel input. Content ownership,
like validation and drafts, is scoped to one invocation and is not inherited
by nested prompts.

Use `paint:show-prompt-message!` when a non-`prompt:read!` interaction should retain the
same styled label and wrapped layout.
