# Git API

The `(git)` module is a small, read-only wrapper around common Git queries. It
invokes the installed `git` executable using plumbing-friendly formats and
parses all output at the module boundary. Callers receive Scheme records rather
than command output or display-oriented text.

This design avoids a runtime dependency on `libgit2` and follows the user's Git
configuration and repository semantics. A direct `libgit2` binding would
require a substantial foreign API layer for opaque object lifetimes, callbacks,
errors, and ABI compatibility; it can be added later without changing these
public records.

## Opening a repository

```scheme
(define repo (git-open))
(define repo (git-open "/src/e"))
(git-repository-path repo)
```

`git-open` resolves the worktree root. Other procedures require the returned
repository object, preventing dependence on the editor process's working
directory.

## Status and diffs

```scheme
(git-status repo)
(git-diff repo)       ; unstaged changes
(git-diff repo #t)    ; staged changes
```

Status records expose `git-status-path`, `git-status-original-path`,
`git-status-index`, and `git-status-worktree`. States are symbols such as
`modified`, `added`, `deleted`, `renamed`, `copied`, `unmerged`,
`type-changed`, or `untracked`; #f means unchanged on that side.

Diff records expose `git-diff-status`, `git-diff-path`, and
`git-diff-original-path`. Rename and copy records preserve both paths.
NUL-delimited Git output keeps whitespace and newlines in paths unambiguous.

## Branches and history

```scheme
(git-current-branch repo) ; #f at detached HEAD
(git-branches repo)
(git-log repo)            ; latest 50 commits
(git-log repo 10)
```

Branch records expose name, current state, object hash, upstream, and numeric
ahead/behind counts through the `git-branch-*` accessors. Commit records expose
the hash, parent hashes, author name and email, Unix timestamp, subject, and
body through the `git-commit-*` accessors.

## Errors

A failed Git invocation raises `git-error?`. `git-error-code`,
`git-error-command`, and `git-error-stderr` retain the exit status, argument
list, and diagnostic text as structured condition fields. Arguments are shell
quoted internally; callers never construct command strings.

The initial API is intentionally query-only. Future mutation commands such as
stage, commit, or switch can reuse the same runner while keeping their effects
explicit.

## History browser

`C-x g` or M-x `(git-log!!)` opens the `*git-log*` app for the repository
containing the current file. Pass a path explicitly to browse another one:

```scheme
(git-log!! "/src/e")
```

The app shows the latest 20 commits followed by each commit's changed files.
The repository heading stays fixed while the body scrolls. Click `[refresh]`
beside the repository name, or press `r`, to reload it; the control changes
color briefly while pressed. Use Up/Down or the wheel
to move one row at a time. Enter on a file opens that file's patch in a
read-only `*git-diff*` view in the app's target window. Clicking a file performs
the same action immediately and preserves focus in the target window, following
the normal app mouse convention.

The patch view classifies and styles metadata, hunk headers, additions, and
deletions. It is a special view rather than a file buffer: its text cannot be
edited or saved, and selecting another file replaces it dynamically.
