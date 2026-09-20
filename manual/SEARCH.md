# Search and replacement

## Incremental search

`C-s` starts an incremental search. Typing extends the needle and moves point
just beyond the current match. Pressing `C-s` again advances to the next match,
wrapping at the end of the buffer. Backspace shortens the needle.

Return or Escape accepts the current match without an extra message. `C-g`
cancels the search and restores point to its position before the search began.
Starting with an empty needle and pressing `C-s` recalls the previous search.

Point remains just after the match. Consequently, setting the mark before a
search leaves the found text inside the resulting region.

All matches visible in the current window are highlighted, with the current
match distinguished from the others.

## Case sensitivity

Search uses smart case folding by default. An all-lowercase needle ignores
case; entering an uppercase character makes the search exact and changes the
prompt to `I-search (exact):`.

`M-c` toggles case folding for the current search. To make every search exact:

```scheme
(search:fold-case #f)
```

This setting affects incremental search only. Other matching operations,
including query replacement, remain exact.

## Query replacement

`M-%` opens M-x with `(edit:replace! ` typed; give the text to find and its
replacement as strings. From point to the end of the current buffer it
highlights each occurrence before asking:

| Key | Action |
|---|---|
| `y` or Space | Replace this occurrence |
| `n` or Backspace | Skip this occurrence |
| `q`, Return, `C-g`, or Escape | Stop |

The complete run is one undo step. Point follows the operation and finishes at
the last replaced, skipped, or pending occurrence.

For noninteractive replacement, `edit:replace-all!` works on the selected
region, else on the whole current buffer; the scope forms retarget it:

```scheme
(edit:replace-all! "old" "new")
(head:with-buffer (buffer "notes.md") (edit:replace-all! "old" "new"))
(edit:with-region (region (buffer "notes.md") '(0 . 0) '(4 . 0))
  (edit:replace-all! "old" "new"))
(for-each (lambda (b) (when (head:buffer-file b) (head:with-buffer b (edit:replace-all! "old" "new"))))
          (head:buffers))
```

Each call is one undo step in its buffer and retains its point.
`edit:count-matches` counts the same way.

