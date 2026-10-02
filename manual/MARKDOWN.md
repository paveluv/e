# Markdown viewing

Markdown source buffers use the `markdown` syntax mode. `C-c v` opens a
read-only widget presentation in the selected window; `C-c v` there returns
to the source at the corresponding row. Viewing leaves source text, file
state and undo history unchanged. Source and presentation can remain side
by side, with independent widths, selections and scrolling.

The base interprets source once per revision. Heads fit the demanded blocks
to their own geometry. Selection uses source rows, table fields and character
anchors, so resizing does not move it to another cell. Retained edit history
rebases anchors across source changes. An unavailable anchor is reported;
`M-<` or `M->` can establish a new position instead of guessing through lost
history. Removing the source leaves an unavailable presentation and never
recreates the document.

## Interaction

Arrow keys move the caret; Page Up/Down scroll by a page. `M-<` and `M->`
acquire the beginning and end. Shift-arrows extend selection, `C-Space`
sets the mark and `C-g` clears it. Mouse dragging also selects text. `M-w`
copies the displayed text, including presentation spacing. An off-page
selection is copied in bounded batches; changing its selection or source
cancels pending copying. Saving operates on source documents, never on
width-specific rendered text.

Return or a click follows a link. Hover uses the shared bold, dotted
underline style. Web links use `markdown:browser` (default `xdg-open`);
relative file links resolve against the source file's directory. Linked
Markdown files request a Markdown presentation from the host. Anchor-only
links are not followed yet. Link hover and painting perform no filesystem
or documentation lookup.

## Presentation

- Emphasis and heading markers become faces; paragraphs join soft line
  breaks and retain explicit hard breaks. Blockquotes and list bullets keep
  their usual presentation.
- Tables align and wrap their cells independently in each view.
- Fenced code keeps its language's syntax colors between dotted rules.
- Prose and tables respect `markdown:view-max-width` (80 by default,
  minimum 20) in wide windows. Tiny hosts still clip safely.
- Blocks exceeding the collection's transfer budget display an unavailable
  marker; `C-c v` opens their source.

Customize `md-h1` through `md-h4`, `md-quote`, `md-link` and `md-code` with
`style:set!`. Rendering caches and terminal-cell geometry remain in heads.

## Scheme API

```scheme
(markdown:view! [source-buffer]) ; default-window entry point; returns root view
(markdown:create! actor document commands [source-row]) ; reusable composition
(markdown:create-view! actor owner document [source-row])     ; presentation leaf
(markdown:edit! view)            ; ask its host to open the source
(markdown:locate! view source-row)
(markdown:move! view 'down)
(markdown:select! view caret fixed)
(markdown:copy! view)
(markdown:render lines [width])  ; four values: text, styles, links, source rows
```

Leaf anchors are `(block-source-row source-row field character)`: field zero
for prose/code, positive for table cells. The full composition contains a
`text` child, found with `(widget:descendant page 'text)`. Its `open` host
command receives a semantic document reference and optional preferences
such as `((point . (2 . 0)))` or `((presentation . markdown))`.
The leaf's `open-uri` command receives document ID and URI; `open-source`
receives document ID, reviewed revision and source position. Custom hosts
can choose placement without creating an inner window.

Forking views shares interpretation but keeps logical interaction separate.
Named attachments restore these views and rebuild presentation at the new
width. Runtime definitions can reload without replacing their saved state.

```scheme
(markdown:browser "firefox")
(markdown:view-max-width 120)
```
