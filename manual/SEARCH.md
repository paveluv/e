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
where `(delta-log:filter! review '(...))` supplies a batch value to narrow an explicit review
to the replacement's entries. An unwanted occurrence can be omitted from the
rewrite preview and the draft settled, or the whole replacement undone.

The text to find is a `needle`: while you type it at M-x, its matches
highlight in a separate read-only editor above the prompt. Its status shows
`[1 of 3]`; Tab visits the next occurrence and Shift-Tab the previous, inserting
nothing. The original editor's point and selection stay unchanged. Leaving
the argument or closing the prompt releases the preview. Matches refresh
when the document changes; counting runs in cancellable base work after the
first hit is available.
`search:count` takes a needle too. Matching and highlighting are exact.

`search:replace!` is scoped by the selection or the scope forms:

```scheme
(search:replace! "old" "new")
(head:with-buffer (store:find-named "notes.md") (search:replace! "old" "new"))
(edit:with-region (region:make (store:find-named "notes.md") '(0 . 0) '(4 . 0))
  (search:replace! "old" "new"))
(for-each (lambda (b) (when (head:buffer-file b) (head:with-buffer b (search:replace! "old" "new"))))
          (head:buffers))
```

Regions are ordinary data: `'(region (buffer 17) (0 . 0) (4 . 0))`.
`region:make` validates the reference and positions and orders the endpoints;
`edit:with-region` checks the document and bounds before selecting it. A saved
region keeps its document identity through renames, but its coordinates do
not follow later edits. Use `edit:region-text` to read it without displaying it.

Each call is one undo step in its buffer and retains its point, through
`edit:rewrite-regions!`, the editing operation that takes the basis the
occurrences were found against and replaces each in its own edit, carrying
the ones still to come across the changes the store reports meanwhile, other
actors' included. `search:count` counts the same way.
