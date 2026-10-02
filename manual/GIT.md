# Git API

The `(git)` module is a small, read-only wrapper around common Git queries. It
invokes the installed `git` executable using plumbing-friendly formats and
parses all output at the module boundary. Callers receive Scheme records rather
than command output or display-oriented text.

This design avoids a runtime dependency on `libgit2` and follows the user's Git
configuration and repository semantics.

## Opening a repository

```scheme
(define repo (git:open))
(define repo (git:open "/src/e"))
(git:repository-path repo)
```

`git:open` resolves the worktree root. Other procedures require the returned
repository object, preventing dependence on the editor process's working
directory.

## Status and diffs

```scheme
(git:status repo)
(git:diff repo)       ; unstaged changes
(git:diff repo #t)    ; staged changes
```

Status records expose `git:status-path`, `git:status-original-path`,
`git:status-index`, and `git:status-worktree`. States are symbols such as
`modified`, `added`, `deleted`, `renamed`, `copied`, `unmerged`,
`type-changed`, or `untracked`; #f means unchanged on that side.

Diff records expose `git:diff-status`, `git:diff-path`, and
`git:diff-original-path`. Rename and copy records preserve both paths.
NUL-delimited Git output keeps whitespace and newlines in paths unambiguous.

## Branches and history

```scheme
(git:current-branch repo) ; #f at detached HEAD
(git:branches repo)
(git:log repo)            ; latest 50 commits
(git:log repo 10)
```

Branch records expose name, current state, object hash, upstream, and numeric
ahead/behind counts through the `git:branch-*` accessors. Commit records expose
the hash, parent hashes, author name and email, Unix timestamp, subject, and
body through the `git:commit-*` accessors.

## Errors

A failed Git invocation raises `git:error?`. `git:error-code`,
`git:error-command`, and `git:error-stderr` retain the exit status, argument
list, and diagnostic text as structured condition fields. Git runs from an
argument list, without a shell; callers never construct command strings. The status comes
from the child process; a terminating signal is represented by its negative
number. Both successful and failed calls close their pipes and reap the child.

The API is query-only: e never runs a Git command that changes the repository.

## History browser

`C-x g` opens a Git browser for the repository containing the window's file.
`git-view:open!` takes a window model and an optional repository path. M-x
receiver completion supplies the window; scripts can pass it explicitly:

```scheme
(git-view:open! window "/src/e")
```

The table lists the latest 20 commits. Enter or click a commit to expand its
changed files; selecting a file shows its patch below the table. Up/Down moves
the table selection; the wheel scrolls the pointed table or patch without
changing selection. Tab moves focus between controls. Each browser keeps its
own expanded commit and patch request.

Click `[refresh]`, or press `r` or `C-r`, to refresh this browser. Git commands
run asynchronously in the base and only while the query is demanded. Changing
selection supersedes earlier patch requests. There is no artificial refresh
delay; long operations use the ordinary widget busy indicator.

The patch is a read-only editor over generated base text, with normal selection
and copy. Metadata, hunk headers, additions and deletions have distinct styles.
It has no visited file; selecting a file never edits the working tree.

`(git-view:create! owner path)` creates the same table/patch composition without a
window, for embedding in other hosts. `git-view:refresh!` takes this explicit
view. The base `git-source:` API exposes lazy history and patch queries for
other presentations.
