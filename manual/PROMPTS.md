# Interactive prompts

Prompts are compositions of ordinary text, completion and help widgets.
M-x and command questions use a temporary pop-up above the echo area. A
custom host can place the same controls inside another widget tree.

## Editing

| Key | Action |
|---|---|
| C-a, C-e, Home, End | Move to a line boundary |
| C-b, C-f, Left, Right | Move by one character |
| Up, Down | Move through multiline input; browse history at its edges |
| C-k, C-y | Kill and yank using this head's copy buffer |
| Tab | Normalize completion, then cycle alternatives or pages |
| Shift-Tab | Use the alternate completion source, if provided |
| M-Return | Insert a newline in multiline input |
| C-g, Escape | Cancel |
| Return | Accept |

M-x uses Scheme highlighting and indentation. Pasted line breaks remain in
multiline input; single-line inputs fold them into spaces. A consecutive
second C-a or C-e in M-x addresses the whole expression. Unknown symbols
and ghost hints are italic. Help and validation appear above the input.

Single-key questions accept only their listed characters, case-insensitively.
Other text and navigation keys do not answer; Escape and C-g cancel.

## Placement and focus

The default prompt host temporarily owns the pop-up. Nested questions share
one composition; the inner question receives keyboard input until it ends.
Accepting or cancelling restores the surviving origin and previous pop-up
content. Replacing the prompt host or removing its registration cancels the
waiting command. Transient prompts do not resume after detaching.

The global window commands remain available through ordinary widget routing.
You can focus another window while the prompt remains in its pop-up. Closing
or clearing that pop-up cancels the interaction. C-x Tab opens the bindings
reference beside a focused pop-up and shows the prompt's named commands and
its text control's commands. There is no separate allowlist of prompt-safe keys.

Completion captures its original document. Needle previews use their own
read-only editor and do not move or restyle that document's other editors.
They do not retarget to a newly focused window. Resizing uses the host's
current geometry without discarding input.

## Completion

[M-x](EVAL.md) retains fuzzy symbol completion in any Scheme application
position. The first Tab extends the token while preserving its matches; the
next opens the candidate list. Typing updates the visible list. Further Tab
presses cycle equivalent normalizations or completion pages. A prepared
status names the candidate type and count.

Plain candidates fill columns; labelled candidates include their documentation.
Clicking a candidate inserts its value without accepting the prompt. Hover
uses the normal bold, dotted underline. Stale displayed choices refuse after
the draft or provider changes. Rendering and pointer discovery use prepared
data and never run the provider.

Typed arguments offer the existing value constructors, names, file paths,
procedures and variables described by edoc. Search arguments retain their
live match count and Tab navigation in a scoped editor child.
M-. describes the Scheme name at the current input caret.

## Questions from other actors

An actor's pending question appears in the echo area. C-c a opens an answer
through M-x. Cancelling that local prompt leaves the underlying question
pending. See [Base, heads and agents](MULTIHEAD.md#questions-between-actors)
for actor tickets and disconnect behavior.

## Prompt API

`(prompt:read! label initial provider options)` is the linear interface for
commands documented with `(prompts)`. `provider` is false or a portable
`(namespace schema configuration)` recipe registered with
`completion:register!`. Options include `multiline?`, `help`, `profile`,
`editing-policy` and `mode`. The function returns accepted text or false on
cancellation. It parks its continuation on the ordinary command pump; it
does not read keyboard input recursively. An explicit edit group cannot
span a prompt. Automatic evaluation undo groups end at suspension and start
fresh on resumption.

For embedded applications, create a base request with `prompt-request:create!`
and a control tree with `prompt:create!`. Supply named accepted and cancelled
targets, then mount the tree in your host. Acceptance captures an exact draft
revision. The request owns its transient controls and any draft it creates;
a borrowed authored buffer keeps its own lifetime. A fork can display the
same draft without duplicating the controller's delivery.

`prompt:register-profile!` installs a versioned factory receiving configuration
and the captured origin. Its alist can contain history strings, an alternate
completion source, pure normalization/validation/ghost/transform/edge callbacks,
and an inspection command. Validation returns false or an explanation;
invalid text stays editable. Replacing the provider or profile cancels its
mounted interaction. `edit:register-policy!` supplies shared proposed-text and
logical-position normalization for Entry and multiline editor controls.

`completion:make-source` takes a lookup procedure receiving text and caret.
It returns replacement start and end, valid extensions, and candidates.
Extensions may be deferred until Tab requests normalization; every extension
must preserve the candidate set. Return false as the start when no token can
complete. Optional settle, kind and argument-context callbacks preserve typed
completion behavior. Rich candidates use `completion:make-candidate` with an
insertion string, display label, character styles and optional reversible
preview. The provider owns matching; the prompt owns input, selection and
lifetime.

`prompt:register-host!` supplies placement for linear callers. Its preparation
procedure returns parent request, portable captured origin, and an attachment
procedure. Attachment receives the request and receiver root and returns a
cleanup thunk. Hosts choose geometry and focus; controls contain no window
identity or keyboard reader. See [Widgets](WIDGETS.md) for the shared model,
view, binding and composition protocols.
