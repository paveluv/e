# Markdown viewing

Markdown buffers edit in the `markdown` mode ([syntax
highlighting](STYLES.md) only). The `markdown-view` mode presents the
document in a separate local, read-only buffer, initially named
`<markdown filename>`. `C-c v` switches the selected window between
source and view, keeping the cursor on the matching source row.
Other windows keep their own buffer and cursor, so source and view
can be displayed side by side. The describe browser renders its pages
through the same viewer.

Viewing leaves the source text, file state, mode, and undo history
intact. Source edits appear in the view on the next redraw. Killing a
view keeps its source; killing the source closes its dependent views.
Relative file links resolve from the source file's directory.

Each view window's cursor and viewport, the selection mark, and the saved
position follow source edits, including another actor's edits and undo.
Positions track source rows; rendered columns are kept where possible and
clamped when a line becomes shorter. A source reload or expired edit history
clamps positions into the new document. Width changes and renderer reloads
use the same row mapping.

## The presentation

- Emphasis markers disappear and their text wears the face instead:
  `**bold**` shows as bold, `*italic*` as italic, `` `code` `` in the
  code face.
- Headings drop their `#` markers and take level faces `md-h1` through
  `md-h4`.
- Soft line breaks inside a paragraph disappear -- prose becomes one
  logical line and the window's word wrap lays it out at any width. A
  markdown hard break (a line ending in two spaces) keeps its line.
- Blockquotes drop their `>` markers and wear `md-quote`.
- `[text](url)` shows only the text, underlined in the link face; the
  target lives in the buffer's hyperlink layer. While point rests on a
  link the echo area shows a transient, unlogged `hyperlink:` hint
  with the target. RET or a mouse click follows it (and that is
  logged): web links open through the `markdown:browser` command
  (default `xdg-open`), relative links visit the file -- a linked
  markdown document arrives already in the view. Bare URLs link as in
  any buffer.
- Tables align their columns, header row bold over a rule, and lay
  out like HTML tables: when the natural widths overflow the window,
  each column gets at least its longest word and the rest of the
  width in proportion to how much it has to wrap, and cell text wraps
  inside its column. A view re-renders itself whenever the width of
  its narrowest window changes -- splitting, resizing, or closing
  windows re-fits the tables.
- In a really wide window the view keeps a readable measure: prose
  and tables wrap at `markdown:view-max-width` columns (default 80)
  instead of the full width.
- Fenced code blocks sit between two dotted rules, the fence's
  language tag on the top one; a registered mode of that name colors
  the code.
- List bullets render as `\x2022;`, and an item's continuation lines
  join into the item.
- Horizontal rules draw as a line.
- Long prose still wraps at the window edge, but without the `\`
  wrap marks of an editing buffer.

Every face (`md-h1..4`, `md-quote`, `md-link`, `md-code`) can be
restyled with `style:set!` in config.e.

## Scheme API

```scheme
(markdown:view! [source])          ; show its local, read-only companion
(markdown:edit! [view])            ; return to its live source
(markdown:companion source)        ; existing companion, or #f
(markdown:companion! source [name]) ; prepare a companion, keeping focus
(markdown:view-install! buffer lines) ; render lines into an app view
(markdown:render lines [width])    ; => lines styles links source-rows
```

`markdown:view!` reuses a companion by source identity, including after
either buffer is renamed. `markdown:edit!` returns to the live source
without restoring an old snapshot or changing its read-only state.
Apps such as describe use `markdown:companion!` to prepare a source's view
without selecting a window. Its optional name is a preferred local label;
later calls preserve the same companion, including a renamed one.
Named daemon attachments rebuild companions from their shared source identity
and revision, preserving the local label and following saved source-row anchors
through edits and width changes. Literal views and companions of local-only
sources have no daemon source to restore.
`markdown:render` is the pure renderer
(the automated suite pins it; `width` bounds tables, default 79), and
`markdown:view-install!` renders literal lines into a local buffer;
it refuses a shared buffer. These literal views have no source to
return to with `markdown:edit!`. The `markdown:browser` parameter holds the web-link command,
and `markdown:view-max-width` the reading-width cap:

```scheme
(markdown:browser "firefox")
(markdown:view-max-width 120)
```

## Mode key bindings

The toggle and the link keys are ordinary [key bindings](KEY_BINDING.md)
in per-mode contexts, consulted before the global map while a buffer of
that mode is current:

```scheme
;; as markdown.e's init! registers them
(keymap:bind-default! 'markdown "C-c v" markdown:view!)
(keymap:bind-default! 'markdown-view "C-c v" markdown:edit!)
(keymap:bind-default! 'markdown-view "RET" follow-link)
(keymap:bind-default! 'markdown-view "MOUSE-CLICK" follow-link-quietly)
```

(`follow-link` and `follow-link-quietly` are the viewer's own procedures;
a user override in config.e binds its own command with `keymap:bind!`.)

Any mode can carry such a context: the name is the mode's name as a
symbol, and `MOUSE-CLICK` is a bindable pseudo-key that fires after a
text click places point.
