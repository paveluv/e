# Search and replacement

## Incremental search

`C-s` starts an incremental search. Typing extends the needle and moves point
just beyond the current match. Pressing `C-s` again advances to the next match,
wrapping at the end of the buffer. Backspace shortens the needle.

Return or Escape accepts the latest needle when its search finishes. Arrow
keys accept the currently displayed position and resume normal navigation.
`C-g` cancels the search and restores its original point, carried across any
intervening edits; it never undoes those edits. Switching to another text
window retargets the search, while cancellation still restores the original
window and point.
Starting with an empty needle and pressing `C-s` recalls the previous search.

Point remains just after the match. Consequently, setting the mark before a
search leaves the found text inside the resulting region.

Visible matches are highlighted, with the current match distinguished from
the others. Large searches run cooperatively in the base while the head
continues handling input; annotation batches are bounded.

Extensions can embed the same interaction using `search-control:create!`
with an explicit mounted editor view and a `finished` host command. Its entry,
request and annotations are scoped to that search and released on closure.

## Case sensitivity

Search uses smart case folding by default. An all-lowercase needle ignores
case; entering an uppercase character makes the search exact and changes the
prompt to `I-search (exact):`.

`M-c` toggles case folding for the current search. To make every search exact:

```scheme
(search:fold-case #f)
```

This setting affects incremental search only. Other matching operations,
including replacement, remain exact.

## Replacement

`M-%` opens M-x with `(search:replace! ` typed; give the text to find and
its replacement as strings. The command replaces every occurrence in the
selected region, else in the whole current buffer, and leaves point where it
was. Every occurrence is an entry of the buffer's delta log, all under one
batch, and the whole replacement is one undo step. Reviewing the occurrences
happens in the delta log rather than one question at a time: the buffer shows
the result at once, and `C-x l` opens the delta log browser in the pop-up,
where `(delta-log:filter! ` followed by Tab offers the buffer's batches
newest first; narrowed to the replacement's, one row per occurrence, a
replacement that should not have happened is toggled out of a view and the
view committed, or the whole step undone.

The text to find is a `needle`: while you type it at M-x, its matches
highlight in the current buffer as a search would, the prompt notes `[1 of
3]`, and Tab visits the next occurrence, Shift-Tab the previous, inserting
nothing. Point previews the occurrence's start; leaving the argument or
accepting or cancelling the prompt restores the original point and selection,
carried across any intervening edits. Matches refresh when the buffer changes.
`search:count` takes a needle too. Matching and highlighting are exact.

`search:replace!` is scoped by the selection or the scope forms:

```scheme
(search:replace! "old" "new")
(head:with-buffer (buffer "notes.md") (search:replace! "old" "new"))
(edit:with-region (region (buffer "notes.md") '(0 . 0) '(4 . 0))
  (search:replace! "old" "new"))
(for-each (lambda (b) (when (head:buffer-file b) (head:with-buffer b (search:replace! "old" "new"))))
          (head:buffers))
```

Each call is one undo step in its buffer and retains its point, through
`edit:rewrite-regions!`, the editing operation that takes the basis the
occurrences were found against and replaces each in its own edit, carrying
the ones still to come across the changes the store reports meanwhile, other
actors' included. `search:count` counts the same way.
