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

`M-%` runs `replace!!` from point to the end of the current buffer. It prompts
for any omitted arguments and highlights each occurrence before asking:

| Key | Action |
|---|---|
| `y` or Space | Replace this occurrence |
| `n` or Backspace | Skip this occurrence |
| `q`, Return, `C-g`, or Escape | Stop |

The complete run is one undo step. Point follows the operation and finishes at
the last replaced, skipped, or pending occurrence.

For noninteractive replacement, `replace-all!` accepts an optional target:

```scheme
(replace-all! "old" "new")
(replace-all! "old" "new" (buffer "notes.md"))
(replace-all! "old" "new" head:buffer-file)
```

Targets may be buffers, buffer names, regions, predicates over buffers, or
lists of those values. Each affected buffer receives one undo step and retains
its point.

