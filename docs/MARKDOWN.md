# Markdown viewing

Markdown buffers edit in the `markdown` mode ([syntax
highlighting](STYLES.md) only). The `markdown-view` mode presents the
same document formatted and read-only; `C-c v` toggles between them in
either direction, keeping the cursor on the matching content. The
describe browser renders its pages through the same viewer.

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
  logged): web links open through the `markdown-browser` command
  (default `xdg-open`), relative links visit the file -- a linked
  markdown document arrives already in the view. Bare URLs link as in
  any buffer.
- Tables align their columns, header row bold over a rule, and lay
  out like HTML tables: when the natural widths overflow the window,
  each column gets at least its longest word and the rest of the
  width in proportion to how much it has to wrap, and cell text wraps
  inside its column.
- Fenced code blocks sit between two dotted rules, the fence's
  language tag on the top one; a registered mode of that name colors
  the code.
- List bullets render as `\x2022;`, and an item's continuation lines
  join into the item.
- Horizontal rules draw as a line.
- Long prose still wraps at the window edge, but without the `\`
  wrap marks of an editing buffer.

Every face (`md-h1..4`, `md-quote`, `md-link`, `md-code`) can be
restyled with `set-style!` in config.e.

## Scheme API

```scheme
(markdown-view! [buffer])          ; present formatted, read-only
(markdown-edit! [buffer])          ; restore the markdown source
(markdown-view-install! buffer lines) ; render lines into an app view
(markdown-render lines [width])    ; => lines styles links source-rows
```

`markdown-view!` stashes the source and the buffer's read-only state;
`markdown-edit!` restores both. `markdown-render` is the pure renderer
(the automated suite pins it; `width` bounds tables, default 79), and
`markdown-view-install!` is how the describe browser presents its
pages. The `markdown-browser` parameter holds the web-link command:

```scheme
(markdown-browser "firefox")
```

## Mode key bindings

The toggle and the link keys are ordinary [key bindings](KEY_BINDING.md)
in per-mode contexts, consulted before the global map while a buffer of
that mode is current:

```scheme
(bind-key! 'markdown "C-c v" markdown-view!)
(bind-key! 'markdown-view "C-c v" markdown-edit!)
(bind-key! 'markdown-view "RET" follow-link)
(bind-key! 'markdown-view "MOUSE-CLICK" follow-link-quietly)
```

Any mode can carry such a context: the name is the mode's name as a
symbol, and `MOUSE-CLICK` is a bindable pseudo-key that fires after a
text click places point.
